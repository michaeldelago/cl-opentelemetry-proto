;;; opentelemetry/proto/metrics/v1/metrics.proto.lisp
;;;
;;; Generated by the protocol buffer compiler. DO NOT EDIT!

(cl:in-package #:common-lisp-user)

#+sbcl
(cl:progn
 (cl:eval-when (:compile-toplevel) (sb-ext:restrict-compiler-policy 'cl:debug 0 1))
 (cl:declaim (cl:optimize (sb-c:store-coverage-data 0))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (cl:find-package "CL-PROTOBUFS.OPENTELEMETRY.PROTO.METRICS.V1")
    (cl:defpackage "CL-PROTOBUFS.OPENTELEMETRY.PROTO.METRICS.V1" (:use)
                   (:local-nicknames (#:pi #:cl-protobufs.implementation)))))

(cl:in-package "CL-PROTOBUFS.OPENTELEMETRY.PROTO.METRICS.V1")

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:define-schema 'metrics
    :syntax :proto3

    :package "opentelemetry.proto.metrics.v1"
    :import '("opentelemetry/proto/common/v1/common.proto"
    "opentelemetry/proto/resource/v1/resource.proto"))
)


;;; Top-Level enums

(pi:define-enum aggregation-temporality
    ()
  (:aggregation-temporality-unspecified :index 0)
  (:aggregation-temporality-delta :index 1)
  (:aggregation-temporality-cumulative :index 2))

(pi:define-enum data-point-flags
    ()
  (:data-point-flags-do-not-use :index 0)
  (:data-point-flags-no-recorded-value-mask :index 1))

;;; Top-Level messages

(pi:define-message metrics-data
    ()
  ;; Fields
  (resource-metrics
   :index 1 :type resource-metrics :kind :message :label (:repeated :list) :field-presence :implicit :json-name "resourceMetrics"))

(pi:define-message resource-metrics
    ()
  ;; Fields
  (resource
   :index 1 :type cl-protobufs.opentelemetry.proto.resource.v1::resource :kind :message :label (:optional) :field-presence :explicit :json-name "resource")
  (scope-metrics
   :index 2 :type scope-metrics :kind :message :label (:repeated :list) :field-presence :implicit :json-name "scopeMetrics")
  (schema-url
   :index 3 :type cl:string :kind :scalar :label (:optional) :field-presence :implicit :json-name "schemaUrl"))

(pi:define-message scope-metrics
    ()
  ;; Fields
  (scope
   :index 1 :type cl-protobufs.opentelemetry.proto.common.v1::instrumentation-scope :kind :message :label (:optional) :field-presence :explicit :json-name "scope")
  (metrics
   :index 2 :type metric :kind :message :label (:repeated :list) :field-presence :implicit :json-name "metrics")
  (schema-url
   :index 3 :type cl:string :kind :scalar :label (:optional) :field-presence :implicit :json-name "schemaUrl"))

(pi:define-message metric
    ()
  ;; Fields
  (pi:define-oneof data ()
    (gauge
     :index 5 :type gauge :kind :message :label (:optional) :field-presence :explicit :json-name "gauge")
    (sum
     :index 7 :type sum :kind :message :label (:optional) :field-presence :explicit :json-name "sum")
    (histogram
     :index 9 :type histogram :kind :message :label (:optional) :field-presence :explicit :json-name "histogram")
    (exponential-histogram
     :index 10 :type exponential-histogram :kind :message :label (:optional) :field-presence :explicit :json-name "exponentialHistogram")
    (summary
     :index 11 :type summary :kind :message :label (:optional) :field-presence :explicit :json-name "summary"))
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :field-presence :implicit :json-name "name")
  (description
   :index 2 :type cl:string :kind :scalar :label (:optional) :field-presence :implicit :json-name "description")
  (unit
   :index 3 :type cl:string :kind :scalar :label (:optional) :field-presence :implicit :json-name "unit"))

(pi:define-message gauge
    ()
  ;; Fields
  (data-points
   :index 1 :type number-data-point :kind :message :label (:repeated :list) :field-presence :implicit :json-name "dataPoints"))

(pi:define-message sum
    ()
  ;; Fields
  (data-points
   :index 1 :type number-data-point :kind :message :label (:repeated :list) :field-presence :implicit :json-name "dataPoints")
  (aggregation-temporality
   :index 2 :type aggregation-temporality :kind :enum :label (:optional) :field-presence :implicit :json-name "aggregationTemporality" :default :aggregation-temporality-unspecified)
  (is-monotonic
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :field-presence :implicit :json-name "isMonotonic"))

(pi:define-message histogram
    ()
  ;; Fields
  (data-points
   :index 1 :type histogram-data-point :kind :message :label (:repeated :list) :field-presence :implicit :json-name "dataPoints")
  (aggregation-temporality
   :index 2 :type aggregation-temporality :kind :enum :label (:optional) :field-presence :implicit :json-name "aggregationTemporality" :default :aggregation-temporality-unspecified))

(pi:define-message exponential-histogram
    ()
  ;; Fields
  (data-points
   :index 1 :type exponential-histogram-data-point :kind :message :label (:repeated :list) :field-presence :implicit :json-name "dataPoints")
  (aggregation-temporality
   :index 2 :type aggregation-temporality :kind :enum :label (:optional) :field-presence :implicit :json-name "aggregationTemporality" :default :aggregation-temporality-unspecified))

(pi:define-message summary
    ()
  ;; Fields
  (data-points
   :index 1 :type summary-data-point :kind :message :label (:repeated :list) :field-presence :implicit :json-name "dataPoints"))

(pi:define-message number-data-point
    ()
  ;; Fields
  (pi:define-oneof value ()
    (as-double
     :index 4 :type cl:double-float :kind :scalar :label (:optional) :field-presence :explicit :json-name "asDouble")
    (as-int
     :index 6 :type cl-protobufs:sfixed64 :kind :scalar :label (:optional) :field-presence :explicit :json-name "asInt"))
  (attributes
   :index 7 :type cl-protobufs.opentelemetry.proto.common.v1::key-value :kind :message :label (:repeated :list) :field-presence :implicit :json-name "attributes")
  (start-time-unix-nano
   :index 2 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "startTimeUnixNano")
  (time-unix-nano
   :index 3 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "timeUnixNano")
  (exemplars
   :index 5 :type exemplar :kind :message :label (:repeated :list) :field-presence :implicit :json-name "exemplars")
  (flags
   :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :field-presence :implicit :json-name "flags"))

(pi:define-message histogram-data-point
    ()
  ;; Fields
  (pi:define-oneof -sum (:synthetic-p t)
    (sum
     :index 5 :type cl:double-float :kind :scalar :label (:optional) :field-presence :explicit :json-name "sum"))
  (pi:define-oneof -min (:synthetic-p t)
    (min
     :index 11 :type cl:double-float :kind :scalar :label (:optional) :field-presence :explicit :json-name "min"))
  (pi:define-oneof -max (:synthetic-p t)
    (max
     :index 12 :type cl:double-float :kind :scalar :label (:optional) :field-presence :explicit :json-name "max"))
  (attributes
   :index 9 :type cl-protobufs.opentelemetry.proto.common.v1::key-value :kind :message :label (:repeated :list) :field-presence :implicit :json-name "attributes")
  (start-time-unix-nano
   :index 2 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "startTimeUnixNano")
  (time-unix-nano
   :index 3 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "timeUnixNano")
  (count
   :index 4 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "count")
  (bucket-counts
   :index 6 :type cl-protobufs:fixed64 :kind :scalar :label (:repeated :list) :field-presence :implicit :json-name "bucketCounts" :packed cl:t)
  (explicit-bounds
   :index 7 :type cl:double-float :kind :scalar :label (:repeated :list) :field-presence :implicit :json-name "explicitBounds" :packed cl:t)
  (exemplars
   :index 8 :type exemplar :kind :message :label (:repeated :list) :field-presence :implicit :json-name "exemplars")
  (flags
   :index 10 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :field-presence :implicit :json-name "flags"))

(pi:define-message exponential-histogram-data-point
    ()
  ;; Nested messages

  (pi:define-message exponential-histogram-data-point.buckets
      ()
    ;; Fields
    (offset
     :index 1 :type cl-protobufs:sint32 :kind :scalar :label (:optional) :field-presence :implicit :json-name "offset")
    (bucket-counts
     :index 2 :type cl-protobufs:uint64 :kind :scalar :label (:repeated :list) :field-presence :implicit :json-name "bucketCounts" :packed cl:t))
  ;; Fields
  (pi:define-oneof -sum (:synthetic-p t)
    (sum
     :index 5 :type cl:double-float :kind :scalar :label (:optional) :field-presence :explicit :json-name "sum"))
  (pi:define-oneof -min (:synthetic-p t)
    (min
     :index 12 :type cl:double-float :kind :scalar :label (:optional) :field-presence :explicit :json-name "min"))
  (pi:define-oneof -max (:synthetic-p t)
    (max
     :index 13 :type cl:double-float :kind :scalar :label (:optional) :field-presence :explicit :json-name "max"))
  (attributes
   :index 1 :type cl-protobufs.opentelemetry.proto.common.v1::key-value :kind :message :label (:repeated :list) :field-presence :implicit :json-name "attributes")
  (start-time-unix-nano
   :index 2 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "startTimeUnixNano")
  (time-unix-nano
   :index 3 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "timeUnixNano")
  (count
   :index 4 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "count")
  (scale
   :index 6 :type cl-protobufs:sint32 :kind :scalar :label (:optional) :field-presence :implicit :json-name "scale")
  (zero-count
   :index 7 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "zeroCount")
  (positive
   :index 8 :type exponential-histogram-data-point.buckets :kind :message :label (:optional) :field-presence :explicit :json-name "positive")
  (negative
   :index 9 :type exponential-histogram-data-point.buckets :kind :message :label (:optional) :field-presence :explicit :json-name "negative")
  (flags
   :index 10 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :field-presence :implicit :json-name "flags")
  (exemplars
   :index 11 :type exemplar :kind :message :label (:repeated :list) :field-presence :implicit :json-name "exemplars")
  (zero-threshold
   :index 14 :type cl:double-float :kind :scalar :label (:optional) :field-presence :implicit :json-name "zeroThreshold"))

(pi:define-message summary-data-point
    ()
  ;; Nested messages

  (pi:define-message summary-data-point.value-at-quantile
      ()
    ;; Fields
    (quantile
     :index 1 :type cl:double-float :kind :scalar :label (:optional) :field-presence :implicit :json-name "quantile")
    (value
     :index 2 :type cl:double-float :kind :scalar :label (:optional) :field-presence :implicit :json-name "value"))
  ;; Fields
  (attributes
   :index 7 :type cl-protobufs.opentelemetry.proto.common.v1::key-value :kind :message :label (:repeated :list) :field-presence :implicit :json-name "attributes")
  (start-time-unix-nano
   :index 2 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "startTimeUnixNano")
  (time-unix-nano
   :index 3 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "timeUnixNano")
  (count
   :index 4 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "count")
  (sum
   :index 5 :type cl:double-float :kind :scalar :label (:optional) :field-presence :implicit :json-name "sum")
  (quantile-values
   :index 6 :type summary-data-point.value-at-quantile :kind :message :label (:repeated :list) :field-presence :implicit :json-name "quantileValues")
  (flags
   :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :field-presence :implicit :json-name "flags"))

(pi:define-message exemplar
    ()
  ;; Fields
  (pi:define-oneof value ()
    (as-double
     :index 3 :type cl:double-float :kind :scalar :label (:optional) :field-presence :explicit :json-name "asDouble")
    (as-int
     :index 6 :type cl-protobufs:sfixed64 :kind :scalar :label (:optional) :field-presence :explicit :json-name "asInt"))
  (filtered-attributes
   :index 7 :type cl-protobufs.opentelemetry.proto.common.v1::key-value :kind :message :label (:repeated :list) :field-presence :implicit :json-name "filteredAttributes")
  (time-unix-nano
   :index 2 :type cl-protobufs:fixed64 :kind :scalar :label (:optional) :field-presence :implicit :json-name "timeUnixNano")
  (span-id
   :index 4 :type cl-protobufs:byte-vector :kind :scalar :label (:optional) :field-presence :implicit :json-name "spanId")
  (trace-id
   :index 5 :type cl-protobufs:byte-vector :kind :scalar :label (:optional) :field-presence :implicit :json-name "traceId"))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:add-file-descriptor #P"opentelemetry/proto/metrics/v1/metrics.proto" 'metrics)
)

(cl:export '(aggregation-temporality
aggregation-temporality-int-to-keyword
aggregation-temporality-keyword-to-int
as-double
as-int
attributes
bucket-counts
count
data-point-flags
data-point-flags-int-to-keyword
data-point-flags-keyword-to-int
data-points
description
exemplar
exemplars
explicit-bounds
exponential-histogram
exponential-histogram-data-point
exponential-histogram-data-point.buckets
filtered-attributes
flags
gauge
histogram
histogram-data-point
is-monotonic
max
metric
metrics
metrics-data
min
name
negative
number-data-point
offset
positive
quantile
quantile-values
resource
resource-metrics
scale
schema-url
scope
scope-metrics
span-id
start-time-unix-nano
sum
summary
summary-data-point
summary-data-point.value-at-quantile
time-unix-nano
trace-id
unit
value
zero-count
zero-threshold))
