(defsystem "cl-otel"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on (:cl-protobufs
               :serapeum
               :calispel
               :bordeaux-threads-2)
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
  :test-op (test-op :depends-on (:cl-otel-tests)
                   :perform (,(find-class 'asdf:load-op)   ; Load cl-otel-tests system
                             (o c)
                             (asdf:load-system :cl-otel-tests))
                   :perform (,(find-class 'asdf:test-op)   ; Run tests of cl-otel-tests system
                             (o c)
                             (asdf:test-system :cl-otel-tests))))


(defsystem "cl-otel-tests"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on (:cl-otel
               :parachute)
  :components ((:file "tests/exporter")))
