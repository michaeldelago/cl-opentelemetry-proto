(defpackage opentelemetry.exporter
  (:use :cl)
  (:local-nicknames (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1))
  (:export #:*current-span-id*
           #:*trace-id*
           #:create-tracer

           #:*resource*
           #:with-span))

(in-package :opentelemetry.exporter)

(defvar *current-span-id* nil)
(defvar *trace-id* nil)

(defparameter *resource* nil)

;; # this function needs to return a unix-epoch timestamp in nanoseconds ai!
(serapeum:defsubst timestamp ()
  (local-time:format-timestring nil (local-time:now)))

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
          (resource-spans (make-instance 'otel.trace:resource-spans
                                         :resource *resource*
                                         :scope-spans (vector (make-instance 'otel.trace:scope-spans
                                                                             :spans (vector span)))))
          (encoded-resource (cl-protobufs:serialize-to-bytes resource-spans)))
     (unwind-protect
          (prog1
              ,@body
            (setf end-time (timestamp)))
       (setf (otel.trace:end-time-unix-nano span) end-time)
       (let ((collector-url "http://otel:4318/v1/traces"))
         (ignore-errors
          (dexador:post collector-url
                        :headers '(:content-type "application/x-protobuf")
                        :content encoded-resource))))))
