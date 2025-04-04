(in-package :opentelemetry)

(defun create-resource (resource-attributes)
  (otel.resource:make-resource :attributes (plist-to-attributes resource-attributes)))

(defclass tracer ()
  ((channel :reader tracer-channel
            :initarg :channel ; Add initarg
            ;; Remove initform
            :documentation "The channel used to queue spans data for export.")
   (exporter-thread :accessor exporter-thread
                    :initform nil
                    :documentation "The thread running the exporter loop.")
   (otlp-endpoint :reader otlp-endpoint
                  :initarg :otlp-endpoint
                  :documentation "The OTLP HTTP endpoint URL for traces.")
   (max-spans-per-batch :initarg :max-spans-per-batch
                        :reader tracer-max-spans-per-batch
                        :initform 100
                        :documentation "Maximum number of spans to include in a single batch export.")
   (export-timeout-ms :initarg :export-timeout-ms
                      :reader tracer-export-timeout-ms
                      :initform 5000
                      :documentation "Maximum time (in milliseconds) to wait before exporting a batch of spans."))
  (:documentation "Manages the asynchronous export of trace spans."))

(defun make-tracer (otlp-endpoint &key (channel-buffer-size 100) (max-spans-per-batch 100) (export-timeout-ms 5000))
  "Creates and returns a new TRACER instance.

  Args:
    otlp-endpoint: The URL of the OTLP HTTP endpoint for traces.
    channel-buffer-size: The buffer size for the internal trace channel. Defaults to 100.
    max-spans-per-batch: Maximum number of spans to include in a single batch export. Defaults to 100.
    export-timeout-ms: Maximum time (in milliseconds) to wait before exporting a batch of spans. Defaults to 5000."
  (let ((buf (if (zerop channel-buffer-size)
                 calispel:+null-queue+
                 (make-instance 'jpl-queues:bounded-fifo-queue :capacity channel-buffer-size))))
    (make-instance 'tracer
                   :otlp-endpoint otlp-endpoint
                   :channel (make-instance 'calispel:channel :buffer buf :name "Trace Export Channel")
                   :max-spans-per-batch max-spans-per-batch
                   :export-timeout-ms export-timeout-ms)))

(defmethod run-exporter ((tracer tracer) &key (background t))
  "Starts the exporter loop for the given TRACER instance.
  If BACKGROUND is true (the default), runs the exporter in a new thread.
  Otherwise, runs in the current thread (blocking)."
  (flet ((exporter-loop ()
           (do-run-exporter tracer)))
    (if background
        (progn
          (when (and (exporter-thread tracer) (bt2:thread-alive-p (exporter-thread tracer)))
            (error "Exporter thread is already running for this tracer."))
          (setf (exporter-thread tracer)
                (bt2:make-thread (serapeum:dynamic-closure '(*resource* *scope*) #'exporter-loop) :name "OTLP Exporter Thread")))
        (exporter-loop)))) ; Run in current thread if background is nil

(defmethod do-run-exporter ((tracer tracer))
  "Consumes serialized trace data from the specified CHANNEL and sends it
  via HTTP/protobuf to the OTLP-ENDPOINT. Batches spans before sending based on max spans per batch or timeout.

  This function runs in a loop and is intended to be run in a separate thread
  (usually managed by the 'tracer' class instance).
  It blocks until data is available on the channel."
  ;; use an array instead of a list for the span buffer so we don't have to keep creating a list ai!
  (let ((span-buffer (make-array 0 :adjustable t :fill-pointer t))
        (span-count 0)
        (last-export-time (timestamp)))
    (loop
      do (progn
           (unless (tracer-channel tracer)
             (error "Trace channel is not provided or initialized."))
           (unless *resource*
             (error "resource is nil"))
           (unless *scope*
             (error "scope is nil"))
           (alexandria:when-let ((span (calispel:? (tracer-channel tracer) 0.1)))
             (vector-push-extend span span-buffer)
             (incf span-count))
           (when (or (>= span-count (tracer-max-spans-per-batch tracer))
                     (>= (- (timestamp) last-export-time) (* (tracer-export-timeout-ms tracer) 1000000))) ; Convert ms to ns
             (when (> (length span-buffer) 0)
               (handler-case
                   (progn
                     (let ((resource-spans (otel.trace:make-resource-spans :resource *resource* :scope-spans (list (otel.trace:make-scope-spans :scope *scope* :spans (coerce span-buffer 'list))))))
                       (dexador:post (otlp-endpoint tracer)
                                     :headers '((:content-type . "application/x-protobuf"))
                                     :content (cl-protobufs:serialize-to-bytes (otel.service.trace:make-export-trace-service-request :resource-spans (list resource-spans))))))
                 (error (c)
                   (let ((err (format nil "Error exporting spans: ~a" c)))
                     (log:error "exporting spans" err))))
               (setf (fill-pointer span-buffer) 0)
               (setf span-count 0)
               (setf last-export-time (timestamp))))))))

(defmacro with-resource ((service-name &rest attributes) &body body)
  "Sets up the resource for tracing with a given service name within the lexical scope.

  This macro establishes a resource with the 'service.name' attribute.
  Spans created within this scope will be associated with this resource.

  Args:
      service-name (string): The name of the service to be associated with the resource."
  (make-random-state)
  `(let ((*resource* (create-resource (list :service.name ,service-name ,@attributes))))
     ,@body))
