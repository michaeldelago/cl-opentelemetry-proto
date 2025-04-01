(defpackage opentelemetry.exporter
  (:use :cl)
  (:local-nicknames (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1)
                    (#:bt #:bordeaux-threads)) ; Added bordeaux-threads
  (:export #:*current-span-id*
           #:*trace-id*
           #:create-tracer ; Keep this? Might need adjustment for the class
           #:*resource*
           #:with-span
           #:tracer        ; Export the class name
           #:make-tracer   ; Export the constructor
           #:*tracer*      ; Export the active tracer instance var
           #:tracer-channel ; Export the channel reader
           #:otlp-endpoint ; Export the endpoint reader
           #:run-exporter)) ; Export the generic function/method name


(in-package :opentelemetry.exporter)

(defvar *current-span-id* nil)
(defvar *trace-id* nil)
;; Removed global *trace-channel*
;; (defvar *trace-channel* nil
;;   "A calispel channel used to send trace data for asynchronous export.")

(defvar *tracer* nil "The currently active tracer instance.")

(defparameter *resource* nil)

(serapeum:defsubst timestamp ()
  "Returns the current time as Unix time in nanoseconds."
  (let* ((now (local-time:now))
         (unix-seconds (local-time:timestamp-to-unix now))
         (nanos (local-time:nsec-of now)))
    (+ (* unix-seconds 1000000000) nanos)))

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


(defun create-tracer (resource-attributes)
  (make-instance 'otel.trace:resource-spans
                 :resource (make-instance 'otel.trace:resource
                                          :attributes (coerce resource-attributes 'vector))))


(defmacro with-span ((span-name &key (span-kind :SPAN-KIND-INTERNAL)) &body body)
  `(let* ((*trace-id* (or opentelemetry.exporter:*trace-id* (generate-trace-id)))
          (parent-span-id opentelemetry.exporter:*current-span-id*)
          (*current-span-id* (generate-span-id))
          (start-time (timestamp))
          (end-time 0)                  ; Initialize end-time
          (span (make-instance 'otel.trace:span
                               :trace-id *trace-id*
                               :span-id *current-span-id*
                               :parent-span-id parent-span-id
                               :name ,span-name
                               :kind ,span-kind
                               :start-time-unix-nano start-time))
          ;; Note: resource-spans is created *before* the body runs and end-time is set.
          ;; It will need to be re-serialized later if end-time is needed for export.
          (resource-spans (make-instance 'otel.trace:resource-spans
                                         :resource *resource*
                                         :scope-spans (vector (make-instance 'otel.trace:scope-spans
                                                                             :spans (vector span))))))
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
  (make-instance 'tracer
                 :otlp-endpoint otlp-endpoint
                 :channel (calispel:make-channel :buffer channel-buffer-size
                                                 :name "Trace Export Channel")))

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
            (when (and (exporter-thread tracer) (bt:thread-alive-p (exporter-thread tracer)))
              (error "Exporter thread is already running for this tracer."))
            (setf (exporter-thread tracer)
                  (bt:make-thread #'exporter-loop :name "OTLP Exporter Thread")))
          (exporter-loop))))) ; Run in current thread if background is nil

(defun do-run-exporter (otlp-endpoint &key channel)
  "Consumes serialized trace data from the specified CHANNEL and sends it
  via HTTP/protobuf to the OTLP-ENDPOINT.

  This function runs in a loop and is intended to be run in a separate thread
  (usually managed by the 'tracer' class instance).
  It blocks until data is available on the channel."
  (unless channel
    (error "Trace channel is not provided or initialized."))
  (loop
    (let ((serialized-spans (calispel:? channel))) ; Blocks until data is available
      (when serialized-spans
        (handler-case
            (dexador:post otlp-endpoint
                          :headers '(:content-type "application/x-protobuf")
                          :content serialized-spans)
          ;; TODO: Implement more robust error handling and logging
          (error (c)
            (format *error-output* "~&Error exporting trace: ~A~%" c)))))))
