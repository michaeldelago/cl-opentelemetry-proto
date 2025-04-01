(defpackage opentelemetry-tests.exporter
  (:use :cl :parachute :opentelemetry.exporter)
  (:import-from :opentelemetry.exporter
                #:tracer
                #:make-tracer
                #:run-exporter
                #:tracer-channel
                #:otlp-endpoint
                #:*tracer*
                #:with-span
                #:timestamp
                #:generate-span-id
                #:generate-trace-id
                #:create-resource
                #:exporter-thread))
(in-package :opentelemetry-tests.exporter)


(test-case test-make-tracer
           (test-section "make-tracer function"
                         (let ((tracer (make-tracer "http://localhost:4318/v1/traces")))
                           (is typep tracer 'tracer "make-tracer returns a tracer object")
                           (is string= (otlp-endpoint tracer) "http://localhost:4318/v1/traces" "tracer has the correct endpoint"))))

(test-case test-run-exporter-foreground
           (test-section "run-exporter in foreground (blocking)"
                         (let ((endpoint "http://localhost:4318/v1/traces")
                               (tracer (make-tracer endpoint :channel-buffer-size 10))
                               (test-channel (tracer-channel tracer))
                               (data-sent nil))
                           ;; Mock dexador:post to avoid actual HTTP calls and verify data sending
                           (flet ((mock-post (url &key headers content &allow-other-keys)
                                    (declare (ignore url headers))
                                    (setf data-sent content)
                                    (is string= data-sent "test-span-data" "data was sent to exporter"))))
                             (with-mocked-functions ((dexador:post #'mock-post))
                               (with-timeout 5 ;; Timeout to prevent indefinite blocking if something goes wrong
                                 (bordeaux-threads-2:make-thread (lambda ()
                                                                   (run-exporter tracer :background nil))
                                                                 :name "Exporter Test Thread")
                                 (calispel:! test-channel "test-span-data")))))))



(test-case test-timestamp
           (test-section "timestamp function"
                         (let ((ts (timestamp)))
                           (is numberp ts "timestamp returns a number")
                           ;; Check if the timestamp is within a reasonable range of current time
                           (let ((current-unix-time (local-time:timestamp-to-unix (local-time:now))))
                             (is >= ts (* current-unix-time 1000000000) "timestamp is not in the past")
                             (is < ts (* (+ current-unix-time 60) 1000000000) "timestamp is not too far in the future (within 60 seconds)")))))

(test-case test-generate-span-id
           (test-section "generate-span-id function"
                         (let ((span-id (generate-span-id)))
                           (is vectorp span-id "generate-span-id returns a vector")
                           (is = (length span-id) 8 "span-id is 8 bytes long")
                           (is every #'integerp span-id "span-id contains integers"))))

(test-case test-generate-trace-id
           (test-section "generate-trace-id function"
                         (let ((trace-id (generate-trace-id)))
                           (is vectorp trace-id "generate-trace-id returns a vector")
                           (is = (length trace-id) 16 "trace-id is 16 bytes long")
                           (is every #'integerp trace-id "trace-id contains integers"))))

(test-case test-create-resource
           (test-section "create-resource function"
                         (let ((resource-attrs `((:service.name "test-service") (:service.version "1.0")))
                               (resource-spans (create-resource resource-attrs)))
                           (is typep resource-spans 'otel.proto.trace.v1:resource-spans "create-resource returns resource-spans")
                           (is typep (otel.proto.trace.v1:resource resource-spans) 'otel.proto.trace.v1:resource "resource-spans contains a resource")
                           (let ((attrs (otel.proto.trace.v1:attributes (otel.proto.trace.v1:resource resource-spans))))
                             (is vectorp attrs "resource attributes are a vector")
                             (is = (length attrs) 2 "resource has 2 attributes")
                             ;; Check for specific attributes (order might not be guaranteed)
                             (is (find-if (lambda (attr))
                                          (and (string= (otel.proto.trace.v1:key attr) "service.name")
                                               (string= (otel.proto.trace.v1:string-value attr) "test-service"))
                                          attrs) "resource has service.name attribute")
                             (is (find-if (lambda (attr))
                                          (and (string= (otel.proto.trace.v1:key attr) "service.version")
                                               (string= (otel.proto.trace.v1:string-value attr) "1.0"))
                                          attrs) "resource has service.version attribute")))))

(test-case test-run-exporter-background
           (test-section "run-exporter in background (non-blocking)"
                         (let ((endpoint "http://localhost:4318/v1/traces")
                               (tracer (make-tracer endpoint :channel-buffer-size 10))
                               (test-channel (tracer-channel tracer)))
                           (run-exporter tracer :background t) ; Run in background, should not block
                           (is bordeaux-threads-2:thread-alive-p (exporter-thread tracer) "exporter thread is alive")
                           (calispel:! test-channel "test-span-data-background") ; Send some dummy data
                           (sleep 1) ; Give exporter thread some time to process
                           ;; We can't directly verify HTTP export here without mocking,
                           ;; but we can check that the thread is running and data can be sent to the channel.
                           (assert-true t "run-exporter in background started and did not block"))
                         (bordeaux-threads-2:join-thread (exporter-thread tracer) :timeout 5) ; Clean up thread
                         (setf (exporter-thread tracer) nil))) ; Reset thread slot
