(defpackage opentelemetry.exporter
  (:use :cl)
  (:local-nicknames (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1))
  (:export #:*current-span-id*
           #:*trace-id*
           #:create-tracer

           #:*resource*
           #:with-span
           #:*trace-channel*)) ; Export the new channel variable

(in-package :opentelemetry.exporter)

(defvar *current-span-id* nil)
(defvar *trace-id* nil)
(defvar *trace-channel* nil
  "A calispel channel used to send trace data for asynchronous export.")

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
         (when *trace-channel*
           (calispel:! *trace-channel* final-encoded-resource))))))

;; # write a function to consume *trace-channel* and send the traces over http/protobuf to the otlp endpoint ai!
