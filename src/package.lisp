(defpackage opentelemetry
  (:use :cl)
  (:local-nicknames (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1)
                    (#:otel.resource #:cl-protobufs.opentelemetry.proto.resource.v1)
                    (#:otel.service.trace #:cl-protobufs.opentelemetry.proto.collector.trace.v1)
                    (#:otel.common #:cl-protobufs.opentelemetry.proto.common.v1))
  (:export #:*current-span-id*
           #:*resource*
           #:*scope*
           #:*span*
           #:*trace-id*
           #:*tracer*
           #:call-with-span
           #:create-resource
           #:make-tracer
           #:new-span-event
           #:otlp-endpoint
           #:run-exporter
           #:set-span-attribute
           #:set-span-status
           #:set-span-status-error
           #:set-span-status-ok
           #:tracer
           #:tracer-channel
           #:with-resource
           #:with-span))

(in-package :opentelemetry)

(defvar *current-span-id* nil)
(defvar *span* nil)
(defvar *trace-id* nil)
(defvar *tracer* nil "The currently active tracer instance.")
(defvar *scope* (otel.common:make-instrumentation-scope :name "opentelemetry-cl" :version (slot-value (asdf:find-system 'cl-opentelemetry) 'asdf:version)))
(defvar *resource* nil)


(setf bt2:*default-special-bindings*
      (append `((*current-span-id* . *current-span-id*)
                (*span* . *span*)
                (*trace-id* . *trace-id*)
                (*tracer* . *tracer*)
                (*scope* . *scope*)
                (*resource* . *resource*))
              bt2:*default-special-bindings*))

(setf bordeaux-threads:*default-special-bindings*
      (append `((*current-span-id* . *current-span-id*)
                (*span* . *span*)
                (*trace-id* . *trace-id*)
                (*tracer* . *tracer*)
                (*scope* . *scope*)
                (*resource* . *resource*))
              bordeaux-threads:*default-special-bindings*))
