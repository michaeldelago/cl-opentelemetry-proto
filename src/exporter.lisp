(defpackage opentelemetry.exporter
  (:use :cl)
  (:local-nicknames (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1)
                    (#:otel.resource #:cl-protobufs.opentelemetry.proto.resource.v1)
                    (#:bt2 #:bordeaux-threads)
                    (#:otel.common #:cl-protobufs.opentelemetry.proto.common.v1))
  (:export #:*current-span-id*
           #:*trace-id*
           #:create-resource
           #:*resource*
           #:with-span
           #:tracer
           #:make-tracer
           #:*tracer*
           #:tracer-channel
           #:otlp-endpoint
           #:run-exporter
           #:with-resource))


(in-package :opentelemetry.exporter)

(defvar *current-span-id* nil)
(defvar *trace-id* nil)
(defvar *tracer* nil "The currently active tracer instance.")

(defparameter *resource* nil)

(serapeum:-> timestamp () fixnum)
(serapeum:defsubst timestamp ()
  "Returns the current time as nanoseconds since the Unix epoch"
  (let ((now (local-time:now)))
    (+ (* (local-time:timestamp-to-unix now) 1000000000)
       (local-time:nsec-of now))))


(defun generate-span-id ()
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8))))
    (dotimes (i 8)
      (setf (aref bytes i) (random 256)))
    bytes))

(defun generate-trace-id ()
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref bytes i) (random 256)))
    bytes))

(defun symbol-to-string (sym)
  (typecase sym
    (symbol (string-downcase (format nil "~a" sym)))
    (t sym)))

(defun get-otel-value (value)
  (typecase value
    (keyword (otel.common:make-any-value :string-value (symbol-to-string value)))
    (string (otel.common:make-any-value :string-value value))
    (integer (otel.common:make-any-value :int-value value))
    (float (otel.common:make-any-value :double-value (float value 1.0d0)))
    (hash-table (otel.common:make-any-value :kvlist-value (otel.common:make-key-value-list :values (serapeum:maphash-return (serapeum:op (otel.common:make-key-value :key (symbol-to-string _) :value (get-otel-value _))) value))))
    (null (otel.common:make-any-value :bool-value value))
    ((or list array) (otel.common:make-any-value :array-value (otel.common:make-array-value :values (map 'list #'get-otel-value value))))
    (t (otel.common:make-any-value :bool-value (when (eq t value)
                                                 value)
                                   :string-value (unless (eq t value)
                                                   (symbol-to-string value))))))

(defun plist-to-resource-attributes (attrs)
  (loop for (key value) on attrs by #'cddr
        collect (otel.common:make-key-value :key (symbol-to-string key) :value (get-otel-value value))))


(defun create-resource (resource-attributes)
  (otel.resource:make-resource :attributes (plist-to-resource-attributes resource-attributes)))

(defmacro with-span ((span-name &key (span-kind :SPAN-KIND-INTERNAL)) &body body)
  `(let* ((*trace-id* (or opentelemetry.exporter:*trace-id* (generate-trace-id)))
          (parent-span-id opentelemetry.exporter:*current-span-id*)
          (*current-span-id* (generate-span-id))
          (start-time (timestamp))
          (end-time 0)                  ; Initialize end-time
          (span (otel.trace:make-span
                 :trace-id *trace-id*
                 :span-id *current-span-id*
                 :parent-span-id parent-span-id
                 :name ,span-name
                 :kind ,span-kind
                 :start-time-unix-nano start-time))
          ;; Note: resource-spans is created *before* the body runs and end-time is set.
          ;; It will need to be re-serialized later if end-time is needed for export.
          (resource-spans (otel.trace:make-resource-spans
                           :resource *resource*
                           :scope-spans (list (otel.trace:make-scope-spans :spans (list span))))))
     (unwind-protect
          (prog1
              ,@body
            ;; Capture the end time after the body executes
            (setf end-time (timestamp)))
       ;; Cleanup form: Set end time and send to channel
       (setf (otel.trace:end-time-unix-nano span) end-time)
       ;; Re-encode the resource-spans *after* setting the end-time
       (let ((final-encoded-resource (cl-protobufs:serialize-to-bytes resource-spans)))
         ;; Use the channel from the active *tracer* instance
         (when (and *tracer* (tracer-channel *tracer*))
           (calispel:! (tracer-channel *tracer*) final-encoded-resource))))))


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
                  :documentation "The OTLP HTTP endpoint URL for traces."))
  (:documentation "Manages the asynchronous export of trace spans."))

(defun make-tracer (otlp-endpoint &key (channel-buffer-size 100))
  "Creates and returns a new TRACER instance.

  Args:
    otlp-endpoint: The URL of the OTLP HTTP endpoint for traces.
    channel-buffer-size: The buffer size for the internal trace channel. Defaults to 100."
  (let ((buf (if (zerop channel-buffer-size)
                 calispel:+null-queue+
                 (make-instance 'jpl-queues:bounded-fifo-queue :capacity channel-buffer-size))))
    (make-instance 'tracer
                   :otlp-endpoint otlp-endpoint
                   :channel (make-instance 'calispel:channel :buffer buf
                                                             :name "Trace Export Channel"))))

(defmethod run-exporter ((tracer tracer) &key (background t))
  "Starts the exporter loop for the given TRACER instance.
  If BACKGROUND is true (the default), runs the exporter in a new thread.
  Otherwise, runs in the current thread (blocking)."
  (let ((endpoint (otlp-endpoint tracer))
        (channel (tracer-channel tracer)))
    (flet ((exporter-loop ()
             (do-run-exporter endpoint :channel channel)))
      (if background
          (progn
            (when (and (exporter-thread tracer) (bt2:thread-alive-p (exporter-thread tracer)))
              (error "Exporter thread is already running for this tracer."))
            (setf (exporter-thread tracer)
                  (bt2:make-thread #'exporter-loop :name "OTLP Exporter Thread")))
          (exporter-loop))))) ; Run in current thread if background is nil

(defun do-run-exporter (otlp-endpoint &key channel)
  "Consumes serialized trace data from the specified CHANNEL and sends it
  via HTTP/protobuf to the OTLP-ENDPOINT.

  This function runs in a loop and is intended to be run in a separate thread
  (usually managed by the 'tracer' class instance).
  It blocks until data is available on the channel."
  (loop do (progn
             (unless channel
               (error "Trace channel is not provided or initialized."))
             (let ((serialized-spans (calispel:? channel))) ; Blocks until data is available
               (when serialized-spans
                 (dexador:post otlp-endpoint
                               :headers '((:content-type . "application/x-protobuf"))
                               :content serialized-spans)))))) ; Use raw-content for byte array
;; TODO: Implement more robust error handling and logging

(defmacro with-resource ((service-name &rest attributes) &body body)
  "Sets up the resource for tracing with a given service name within the lexical scope.

  This macro establishes a resource with the 'service.name' attribute.
  Spans created within this scope will be associated with this resource.

  Args:
      service-name (string): The name of the service to be associated with the resource."
  `(let ((*resource* (create-resource (list :service.name ,service-name ,@attributes))))
     ,@body))
