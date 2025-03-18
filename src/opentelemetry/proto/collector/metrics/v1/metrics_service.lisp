;;; opentelemetry/proto/collector/metrics/v1/metrics_service.proto.lisp
;;;
;;; Generated by the protocol buffer compiler. DO NOT EDIT!

(cl:in-package #:common-lisp-user)

#+sbcl
(cl:progn
 (cl:eval-when (:compile-toplevel) (sb-ext:restrict-compiler-policy 'cl:debug 0 1))
 (cl:declaim (cl:optimize (sb-c:store-coverage-data 0))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (cl:find-package "CL-PROTOBUFS.OPENTELEMETRY.PROTO.COLLECTOR.METRICS.V1")
    (cl:defpackage "CL-PROTOBUFS.OPENTELEMETRY.PROTO.COLLECTOR.METRICS.V1" (:use)
                   (:local-nicknames (#:pi #:cl-protobufs.implementation)))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (cl:find-package "CL-PROTOBUFS.OPENTELEMETRY.PROTO.COLLECTOR.METRICS.V1-RPC")
    (cl:defpackage "CL-PROTOBUFS.OPENTELEMETRY.PROTO.COLLECTOR.METRICS.V1-RPC" (:use)
                   (:local-nicknames (#:pi #:cl-protobufs.implementation)))))

(cl:in-package "CL-PROTOBUFS.OPENTELEMETRY.PROTO.COLLECTOR.METRICS.V1")

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:define-schema 'metrics_service
    :syntax :proto3

    :package "opentelemetry.proto.collector.metrics.v1"
    :import '("opentelemetry/proto/metrics/v1/metrics.proto"))
)


;;; Top-Level messages

(pi:define-message export-metrics-service-request
    ()
  ;; Fields
  (resource-metrics
   :index 1 :type cl-protobufs.opentelemetry.proto.metrics.v1::resource-metrics :kind :message :label (:repeated :list) :field-presence :implicit :json-name "resourceMetrics"))

(pi:define-message export-metrics-service-response
    ()
  ;; Fields
  (partial-success
   :index 1 :type export-metrics-partial-success :kind :message :label (:optional) :field-presence :explicit :json-name "partialSuccess"))

(pi:define-message export-metrics-partial-success
    ()
  ;; Fields
  (rejected-data-points
   :index 1 :type cl-protobufs:int64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "rejectedDataPoints")
  (error-message
   :index 2 :type cl:string :kind :scalar :label (:optional) :field-presence :implicit :json-name "errorMessage"))

;;; Services
(pi:define-service metrics-service
    (:source-location #P"opentelemetry/proto/collector/metrics/v1/metrics_service.proto")
  (export (
    export-metrics-service-request =>
    export-metrics-service-response)))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:add-file-descriptor #P"opentelemetry/proto/collector/metrics/v1/metrics_service.proto" 'metrics_service)
)

(cl:export '(error-message
export-metrics-partial-success
export-metrics-service-request
export-metrics-service-response
metrics-service
metrics_service
partial-success
rejected-data-points
resource-metrics))

(cl:in-package "CL-PROTOBUFS.OPENTELEMETRY.PROTO.COLLECTOR.METRICS.V1-RPC")

(cl:export '(call-export
export))
