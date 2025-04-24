(defsystem "cl-opentelemetry"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on (:bordeaux-threads
               :calispel
               :dexador
               :log4cl
               :serapeum
               :cl-protobufs)
  :defsystem-depends-on (:cl-protobufs.asdf)
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
                             (:protobuf-source-file "opentelemetry/proto/collector/trace/v1/trace_service")))
               (:file "src/package")
               (:file "src/util")
               (:file "src/otel")
               (:file "src/span"))
  :description ""
  :in-order-to ((test-op (test-op "cl-opentelemetry/tests"))))


(defsystem "cl-opentelemetry/tests"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on (:cl-opentelemetry
               :parachute
               :hunchentoot)
  :components ((:file "tests/otel")
               (:file "tests/resource-attributes"))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :opentelemetry/tests)))
