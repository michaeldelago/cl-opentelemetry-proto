(defpackage opentelemetry.integrations.clack
  (:use :cl)
  (:local-nicknames
   (#:otel #:opentelemetry)))

(in-package :opentelemetry.integrations.clack)

;; write a clack middleware that wraps each request in a span ai!
