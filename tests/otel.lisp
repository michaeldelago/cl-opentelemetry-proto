(defpackage opentelemetry/tests
  (:use :cl :parachute)
  (:local-nicknames (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1)
                    (#:otel.common #:cl-protobufs.opentelemetry.proto.common.v1)
                    (#:otel.resource #:cl-protobufs.opentelemetry.proto.resource.v1)
                    (#:re #:cl-ppcre)
                    (#:ht #:hunchentoot))
  (:import-from :opentelemetry
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
                #:*resource*
                #:*span*))
(in-package :opentelemetry/tests)

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
         (opentelemetry:*tracer* test-tracer))
    (with-span ("test-span")
      (sleep 0.1))
    (let ((received-data (calispel:? (tracer-channel test-tracer) 1)))
      (true received-data "Channel should receive data after with-span"))))

(define-test test-timestamp
  (let ((ts (timestamp)))
    (true (integerp ts))))

;; (define-test test-tracer
;;   "Test for the tracer with a mock HTTP server"
;;   (let* ((mock-server (ht:start (make-instance 'ht:easy-acceptor :port 8080)))
;;          (mock-url (format nil "http://localhost:~a/v1/traces" (ht:acceptor-port mock-server)))
;;          (test-tracer (make-tracer mock-url :channel-buffer-size 10))
;;          (opentelemetry:*tracer* test-tracer)
;;          (payload-chan (make-instance 'calispel:channel)))
;;     (unwind-protect
;;          (progn
;;            (ht:define-easy-handler (mock-trace-handler :uri "/v1/traces") ()
;;              (setf (ht:content-type*) "application/x-protobuf")
;;              (calispel:! payload-chan (ht:raw-post-data :request ht:*request*))
;;              "")
;;            (run-exporter test-tracer)
;;            (with-span ("test-span")
;;              (identity "hello"))
;;            (let ((received-data (calispel:? payload-chan 1)))
;;              (true received-data "Channel should receive data after with-span")
;;              (let ((resource-spans (cl-protobufs:deserialize-from-bytes 'otel.trace:resource-spans received-data)))
;;                (true resource-spans "resource-spans should not be nil")
;;                (let ((scope-spans (first (otel.trace:resource-spans.scope-spans resource-spans))))
;;                  (true scope-spans "scope-spans should not be nil")
;;                  (let ((span (first (otel.trace:scope-spans.spans scope-spans))))
;;                    (true span "span should not be nil")
;;                    (is equal (otel.trace:span.name span) "test-span"))))))
;;       (ht:stop mock-server))))

;; (define-test test-tracer-integration
;;   "Test for the tracer with a mock HTTP server"
;;   (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces" :channel-buffer-size 10))
;;          (opentelemetry:*tracer* test-tracer))
;;     (run-exporter test-tracer)
;;     (progn
;;       (with-resource ("otel-cl-test")
;;         (with-span ("test-span")
;;           (identity "hello")))))
;;   ;; (let ((span (calispel:? (tracer-channel test-tracer))))
;;   ;;   (true span)
;;   ;;   (print (cl-protobufs:deserialize-from-bytes 'otel.trace:resource-spans span)))
;;   (true t))

(define-test test-nested-span-integration
  "Test for the tracer with a mock HTTP server"
  (with-resource ("otel-cl-test")
    (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces"  :channel-buffer-size 10 :export-timeout-ms 100 :max-spans-per-batch 1))
           (opentelemetry:*tracer* test-tracer))
      (run-exporter test-tracer)
      (with-span ("parent-span")
        (sleep 0.1)
        (let ((parent-span-id (otel.trace:span.span-id *span*)))
          (with-span ("child-span")
            (true parent-span-id)
            (is equal parent-span-id (otel.trace:span.parent-span-id *span*))
            (sleep 1))))
      (sleep 1))
    ;; (let ((span (calispel:? (tracer-channel test-tracer))))
    ;;   (true span)
    ;;   (print (cl-protobufs:deserialize-from-bytes 'otel.trace:resource-spans span)))
    (true t)))

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
            (is equal (otel.common:any-value.string-value (otel.common:key-value.value service-name-attribute)) test-service-name)))))))

(define-test test-with-resource-attributes
  "Test for the with-resource macro to set additional attributes"
  (let ((test-service-name "test-service")
        (test-service-version "1.2.3")
        (test-environment "staging"))
    (with-resource (test-service-name :service.version test-service-version :environment test-environment)
      (let ((resource *resource*))
        (true resource "resource should not be nil")
        (let ((attributes (otel.resource:resource.attributes resource)))
          (true attributes "resource attributes should not be nil")
          (let ((service-name-attribute (find-if (lambda (attr)
                                                   (string= (otel.common:key attr) "service.name"))
                                                 attributes)))
            (true service-name-attribute "service.name attribute should be present")
            (is equal (otel.common:any-value.string-value (otel.common:key-value.value service-name-attribute)) test-service-name))
          (let ((service-version-attribute (find-if (lambda (attr)
                                                      (string= (otel.common:key attr) "service.version"))
                                                    attributes)))
            (true service-version-attribute "service.version attribute should be present")
            (is equal (otel.common:any-value.string-value (otel.common:key-value.value service-version-attribute)) test-service-version))
          (let ((environment-attribute (find-if (lambda (attr)
                                                  (string= (otel.common:key attr) "environment"))
                                                attributes)))
            (true environment-attribute "environment attribute should be present")
            (is equal (otel.common:any-value.string-value (otel.common:key-value.value environment-attribute)) test-environment)))))))

(define-test test-set-span-attribute
  "Test setting an attribute on the current span"
  (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces" :channel-buffer-size 10 :max-spans-per-batch 1 :export-timeout-ms 0.5))
         (opentelemetry:*tracer* test-tracer)
         (attribute-key "test-attribute")
         (attribute-value "test-value"))
    (with-resource ("test-service")
      (with-span ("test-span")
        (opentelemetry:set-span-attribute attribute-key attribute-value)
        (sleep 0.1))
      (let* ((span (calispel:? (tracer-channel test-tracer) 10))
             (attributes (otel.trace:span.attributes span)))
        (true attributes "Span attributes should not be nil")
        (let ((test-attribute (find-if (lambda (attr)
                                         (string= (otel.common:key attr) attribute-key))
                                       attributes)))
          (true test-attribute "Test attribute should be present")
          (is equal (otel.common:any-value.string-value (otel.common:key-value.value test-attribute)) attribute-value))))))

;; (define-test test-span-kind-internal
;;   "Test creating a span with SPAN-KIND-INTERNAL"
;;   (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces" :channel-buffer-size 10))
;;          (opentelemetry:*tracer* test-tracer))
;;     (with-span ("internal-span" :span-kind :span-kind-internal)
;;       (sleep 0.1))
;;     (let ((received-data (calispel:? (tracer-channel test-tracer) 1)))
;;       (true received-data "Channel should receive data after with-span")
;;       (let ((resource-spans (cl-protobufs:deserialize-from-bytes 'otel.trace:resource-spans received-data)))
;;         (let ((scope-spans (first (otel.trace:resource-spans.scope-spans resource-spans))))
;;           (let ((span (first (otel.trace:scope-spans.spans scope-spans))))
;;             (is equal (otel.trace:span.kind span) :span-kind-internal)))))))

;; (define-test test-span-kind-server
;;   "Test creating a span with SPAN-KIND-SERVER"
;;   (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces" :channel-buffer-size 10))
;;          (opentelemetry:*tracer* test-tracer))
;;     (with-span ("server-span" :span-kind :span-kind-server)
;;       (sleep 0.1))
;;     (let ((received-data (calispel:? (tracer-channel test-tracer) 1)))
;;       (true received-data "Channel should receive data after with-span")
;;       (let ((resource-spans (cl-protobufs:deserialize-from-bytes 'otel.trace:resource-spans received-data)))
;;         (let ((scope-spans (first (otel.trace:resource-spans.scope-spans resource-spans))))
;;           (let ((span (first (otel.trace:scope-spans.spans scope-spans))))
;;             (is equal (otel.trace:span.kind span) :span-kind-server)))))))

;; (define-test test-span-kind-client
;;   "Test creating a span with SPAN-KIND-CLIENT"
;;   (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces" :channel-buffer-size 10))
;;          (opentelemetry:*tracer* test-tracer))
;;     (with-span ("client-span" :span-kind :span-kind-client)
;;       (sleep 0.1))
;;     (let ((received-data (calispel:? (tracer-channel test-tracer) 1)))
;;       (true received-data "Channel should receive data after with-span")
;;       (let ((resource-spans (cl-protobufs:deserialize-from-bytes 'otel.trace:resource-spans received-data)))
;;         (let ((scope-spans (first (otel.trace:resource-spans.scope-spans resource-spans))))
;;           (let ((span (first (otel.trace:scope-spans.spans scope-spans))))
;;             (is equal (otel.trace:span.kind span) :span-kind-client)))))))

;; (define-test test-span-kind-producer
;;   "Test creating a span with SPAN-KIND-PRODUCER"
;;   (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces" :channel-buffer-size 10))
;;          (opentelemetry:*tracer* test-tracer))
;;     (with-span ("producer-span" :span-kind :span-kind-producer)
;;       (sleep 0.1))
;;     (let ((received-data (calispel:? (tracer-channel test-tracer) 1)))
;;       (true received-data "Channel should receive data after with-span")
;;       (let ((resource-spans (cl-protobufs:deserialize-from-bytes 'otel.trace:resource-spans received-data)))
;;         (let ((scope-spans (first (otel.trace:resource-spans.scope-spans resource-spans))))
;;           (let ((span (first (otel.trace:scope-spans.spans scope-spans))))
;;             (is equal (otel.trace:span.kind span) :span-kind-producer)))))))

;; (define-test test-span-kind-consumer
;;   "Test creating a span with SPAN-KIND-CONSUMER"
;;   (let* ((test-tracer (make-tracer "http://localhost:4318/v1/traces" :channel-buffer-size 10))
;;          (opentelemetry:*tracer* test-tracer))
;;     (with-span ("consumer-span" :span-kind :span-kind-consumer)
;;       (sleep 0.1))
;;     (let ((received-data (calispel:? (tracer-channel test-tracer) 1)))
;;       (true received-data "Channel should receive data after with-span")
;;       (let ((resource-spans (cl-protobufs:deserialize-from-bytes 'otel.trace:resource-spans received-data)))
;;         (let ((scope-spans (first (otel.trace:resource-spans.scope-spans resource-spans))))
;;           (let ((span (first (otel.trace:scope-spans.spans scope-spans))))
;;             (is equal (otel.trace:span.kind span) :span-kind-consumer)))))))

;; TODO: Test setting attributes with different data types (string, integer, float, boolean, array, map)
;; TODO: Test setting events on spans
;; TODO: Test setting span status (OK, ERROR, UNSET)
;; TODO: Test resource attributes more thoroughly, including different attribute types and combinations
;; TODO: Test different configurations of the tracer (e.g., different buffer sizes, export timeouts, batch sizes)
;; TODO: Test error handling in the exporter
;; TODO: Test sampling
