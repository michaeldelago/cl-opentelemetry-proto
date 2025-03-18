(defsystem "cl-otel-proto"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on ()
  :defsystem-depends-on (:cl-protobufs)
  :serial t
  :components ((:module "opentelemetry-proto"
                :serial t
                :components ((:protobuf-source-file "opentelemetry/proto/common/v1/common")
                             (:protobuf-source-file "opentelemetry/proto/resource/v1/resource")
                             (:protobuf-source-file "opentelemetry/proto/logs/v1/logs")
                             (:protobuf-source-file "opentelemetry/proto/metrics/v1/metrics")
                             (:protobuf-source-file "opentelemetry/proto/trace/v1/trace")
                             (:protobuf-source-file "opentelemetry/proto/collector/logs/v1/logs_service")
                             (:protobuf-source-file "opentelemetry/proto/collector/metrics/v1/metrics_service")
                             (:protobuf-source-file "opentelemetry/proto/collector/trace/v1/trace_service"))))
  :description "")
