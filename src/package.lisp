(defpackage opentelemetry
  (:use :cl)
  (:local-nicknames (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1)
                    (#:otel.resource #:cl-protobufs.opentelemetry.proto.resource.v1)
                    (#:otel.service.trace #:cl-protobufs.opentelemetry.proto.collector.trace.v1)
                    (#:otel.common #:cl-protobufs.opentelemetry.proto.common.v1))
  (:export #:*current-span-id*
           #:*trace-id*
           #:create-resource
           #:*resource*
           #:with-span
           #:tracer
           #:make-tracer
           #:*tracer*
           #:*span*
           #:tracer-channel
           #:otlp-endpoint
           #:run-exporter
           #:with-resource
           #:set-span-attribute
           #:set-span-status
           #:set-span-status-ok
           #:set-span-status-error
           #:call-with-span
           #:new-span-event))

(in-package :opentelemetry)

(defvar *current-span-id* nil)
(defvar *span* nil)
(defvar *trace-id* nil)
(defvar *tracer* nil "The currently active tracer instance.")
(defvar *scope* (otel.common:make-instrumentation-scope :name "opentelemetry-cl" :version (slot-value (asdf:find-system 'cl-otel) 'asdf:version)))
(defvar *resource* nil)

(make-random-state)
