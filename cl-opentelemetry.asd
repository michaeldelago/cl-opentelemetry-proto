;; Build the service using cached/vendored outputs from the cl-protobufs compiler. This is _probably_ what you want so you don't need to ship the protobuf compiler around.
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
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:module "opentelemetry"
                              :serial t
                              :components ((:file "common/v1/common")
                                           (:file "resource/v1/resource")
                                           (:file "logs/v1/logs")
                                           (:file "metrics/v1/metrics")
                                           (:file "trace/v1/trace")
                                           (:file "collector/logs/v1/logs_service")
                                           (:file "collector/metrics/v1/metrics_service")
                                           (:file "collector/trace/v1/trace_service")))
                             (:file "package")
                             (:file "util")
                             (:file "otel")
                             (:file "span"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-opentelemetry/tests"))))

;; This build uses the cl-protobufs compiler to generate the common lisp code. Use this if you're having issues with the above code.
(defsystem "cl-opentelemetry/proto"
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
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "otel")
                             (:file "span"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-opentelemetry/proto/tests"))))

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

(defsystem "cl-opentelemetry/proto/tests"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on (:cl-opentelemetry/proto
               :parachute
               :hunchentoot)
  :components ((:file "tests/otel")
               (:file "tests/resource-attributes"))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :opentelemetry/tests)))
