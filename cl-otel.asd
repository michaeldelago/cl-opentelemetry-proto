(defsystem "cl-otel"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on (:bordeaux-threads
               :calispel
               :dexador
               :log4cl
               :serapeum
               :cl-protobufs)
  :defsystem-depends-on ()
  :serial t
  :components ((:file "src/opentelemetry/proto/common/v1/common")
               (:file "src/opentelemetry/proto/resource/v1/resource")
               (:file "src/opentelemetry/proto/logs/v1/logs")
               (:file "src/opentelemetry/proto/metrics/v1/metrics")
               (:file "src/opentelemetry/proto/trace/v1/trace")
               (:file "src/opentelemetry/proto/collector/logs/v1/logs_service")
               (:file "src/opentelemetry/proto/collector/metrics/v1/metrics_service")
               (:file "src/opentelemetry/proto/collector/trace/v1/trace_service")
               (:file "src/exporter"))
  :description ""
  :in-order-to ((test-op (test-op "cl-otel/tests"))))


(defsystem "cl-otel/tests"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on (:cl-otel
               :parachute
               :hunchentoot)
  :components ((:file "tests/exporter")
               (:file "tests/resource-attributes"))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :opentelemetry.exporter/tests)))
