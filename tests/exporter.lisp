(defpackage opentelemetry.exporter/tests
  (:use :cl :parachute)
  (:local-nicknames (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1)
                    (#:otel.common #:cl-protobufs.opentelemetry.proto.common.v1)
                    (#:otel.resource #:cl-protobufs.opentelemetry.proto.resource.v1)
                    (#:re #:cl-ppcre)
                    (#:ht #:hunchentoot))
  (:import-from :opentelemetry.exporter
                #:tracer
                #:make-tracer
                #:otlp-endpoint
                #:generate-span-id
                #:generate-trace-id
                #:with-span
                #:tracer-channel
                #:timestamp
                #:run-exporter
                #:with-resource
                #:*resource*))
(in-package :opentelemetry.exporter/tests)

(define-test test-generate-span-id
  (let ((span-id (generate-span-id)))
    (true (vectorp span-id))
    (is equal (length span-id) 8)
    (true (every #'integerp span-id))))

(define-test test-generate-trace-id
  (let ((trace-id (generate-trace-id)))
    (true (vectorp trace-id))
    (is equal (length trace-id) 16)
    (true (every #'integerp trace-id))))

(define-test test-with-span
  (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces" :channel-buffer-size 10))
         (opentelemetry.exporter:*tracer* test-tracer))
    (with-span ("test-span")
      (sleep 0.1)) ; Give a little time for the span to be processed
    (let ((received-data (calispel:? (tracer-channel test-tracer) 1))) ; Check for data with a timeout
      (true received-data "Channel should receive data after with-span"))))

(define-test test-timestamp
  (let ((ts (timestamp)))
    (true (integerp ts))))

(define-test test-tracer
  "Test for the tracer with a mock HTTP server"
  (let* ((mock-server (ht:start (make-instance 'ht:easy-acceptor :port 8080)))
         (mock-url (format nil "http://localhost:~a/v1/traces" (ht:acceptor-port mock-server)))
         (test-tracer (make-tracer mock-url :channel-buffer-size 10))
         (opentelemetry.exporter:*tracer* test-tracer)
         (payload-chan (make-instance 'calispel:channel)))
    (unwind-protect
         (progn
           (ht:define-easy-handler (mock-trace-handler :uri "/v1/traces") ()
             (setf (ht:content-type*) "application/x-protobuf")
             (calispel:! payload-chan (ht:raw-post-data :request ht:*request*))
             "")
           (run-exporter test-tracer)
           (with-span ("test-span")
             (identity "hello"))
           (let ((received-data (calispel:? payload-chan 1)))
             (true received-data "Channel should receive data after with-span")
             (let ((resource-spans (cl-protobufs:deserialize-from-bytes 'otel.trace:resource-spans received-data)))
               (true resource-spans "resource-spans should not be nil")
               (let ((scope-spans (first (otel.trace:resource-spans.scope-spans resource-spans))))
                 (true scope-spans "scope-spans should not be nil")
                 (let ((span (first (otel.trace:scope-spans.spans scope-spans))))
                   (true span "span should not be nil")
                   (is equal (otel.trace:span.name span) "test-span"))))))
      (ht:stop mock-server))))

;; (define-test test-tracer-integration
;;   "Test for the tracer with a mock HTTP server"
;;   (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces" :channel-buffer-size 10))
;;          (opentelemetry.exporter:*tracer* test-tracer))
;;     (progn
;;       (run-exporter test-tracer)
;;       (with-span ("test-span")
;;         (identity "hello"))))
;;   (true t))

(define-test test-with-resource
  "Test for the with-resource macro to set service.name"
  (let ((test-service-name "test-service"))
    ;; write a test for a "with-resource" macro. it should set the service.name
    ;; Assuming with-resource macro is defined and sets *resource*
    (with-resource (test-service-name)
      (let ((resource *resource*))
        (true resource "resource should not be nil")
        (let ((attributes (otel.resource:resource.attributes resource)))
          (true attributes "resource attributes should not be nil")
          (let ((service-name-attribute (find-if (lambda (attr)
                                                   (string= (otel.common:key attr) "service.name"))
                                                 attributes)))
            (true service-name-attribute "service.name attribute should be present")
            (is equal (otel.common:any-value.string-value (otel.common:make-any-value :string-value service-name-attribute)) test-service-name)))))))
