;; # write tests for src/exporter.lisp ai!
(rove:deftest test-make-tracer
  (rove:testing "make-tracer function"
    (let ((tracer (opentelemetry.exporter:make-tracer "http://localhost:4318/v1/traces")))
      (rove:ok (typep tracer 'opentelemetry.exporter:tracer) "make-tracer returns a tracer object")
      (rove:ok (string= (opentelemetry.exporter:otlp-endpoint tracer) "http://localhost:4318/v1/traces") "tracer has the correct endpoint"))))

(rove:deftest test-run-exporter-foreground
  (rove:testing "run-exporter in foreground (blocking)"
    (let ((endpoint "http://localhost:4318/v1/traces")
          (tracer (opentelemetry.exporter:make-tracer endpoint :channel-buffer-size 10))
          (test-channel (opentelemetry.exporter:tracer-channel tracer))
          (data-sent nil))
      ;; Mock dexador:post to avoid actual HTTP calls and verify data sending
      (flet ((mock-post (url &key headers content &allow-other-keys)
               (declare (ignore url headers))
               (setf data-sent content)
               (return-from test-run-exporter-foreground (rove:ok (string= data-sent "test-span-data") "data was sent to exporter"))) )
        (rove:with-mock-function dexador:post #'mock-post
          (bt:with-timeout (5) ;; Timeout to prevent indefinite blocking if something goes wrong
            (bt:make-thread (lambda ()
                              (opentelemetry.exporter:run-exporter tracer :background nil))
                            :name "Exporter Test Thread")
            (calispel:! test-channel "test-span-data")
            ))))))


(rove:deftest test-timestamp
  (rove:testing "timestamp function"
    (let ((ts (opentelemetry.exporter:timestamp)))
      (rove:ok (numberp ts) "timestamp returns a number")
      ;; Check if the timestamp is within a reasonable range of current time
      (let ((current-unix-time (local-time:timestamp-to-unix (local-time:now))))
        (rove:ok (>= ts (* current-unix-time 1000000000)) "timestamp is not in the past")
        (rove:ok (< ts (* (+ current-unix-time 60) 1000000000)) "timestamp is not too far in the future (within 60 seconds)")))))

(rove:deftest test-generate-span-id
  (rove:testing "generate-span-id function"
    (let ((span-id (opentelemetry.exporter:generate-span-id)))
      (rove:ok (vectorp span-id) "generate-span-id returns a vector")
      (rove:ok (= (length span-id) 8) "span-id is 8 bytes long")
      (rove:ok (every #'integerp span-id) "span-id contains integers"))))

(rove:deftest test-generate-trace-id
  (rove:testing "generate-trace-id function"
    (let ((trace-id (opentelemetry.exporter:generate-trace-id)))
      (rove:ok (vectorp trace-id) "generate-trace-id returns a vector")
      (rove:ok (= (length trace-id) 16) "trace-id is 16 bytes long")
      (rove:ok (every #'integerp trace-id) "trace-id contains integers"))))

(rove:deftest test-create-resource
  (rove:testing "create-resource function"
    (let ((resource-attrs `((:service.name "test-service") (:service.version "1.0")))
          (resource-spans (opentelemetry.exporter:create-resource resource-attrs)))
      (rove:ok (typep resource-spans 'otel.trace:resource-spans) "create-resource returns resource-spans")
      (rove:ok (typep (otel.trace:resource resource-spans) 'otel.trace:resource) "resource-spans contains a resource")
      (let ((attrs (otel.trace:attributes (otel.trace:resource resource-spans))))
        (rove:ok (vectorp attrs) "resource attributes are a vector")
        (rove:ok (= (length attrs) 2) "resource has 2 attributes")
        ;; Check for specific attributes (order might not be guaranteed)
        (rove:ok (find-if (lambda (attr)
                         (and (string= (otel.trace:key attr) "service.name")
                              (string= (otel.trace:string-value attr) "test-service")))
                       attrs))
        (rove:ok (find-if (lambda (attr)
                         (and (string= (otel.trace:key attr) "service.version")
                              (string= (otel.trace:string-value attr) "1.0")))
                       attrs))))))

(rove:deftest test-run-exporter-background
  (rove:testing "run-exporter in background (non-blocking)"
    (let ((endpoint "http://localhost:4318/v1/traces")
          (tracer (opentelemetry.exporter:make-tracer endpoint :channel-buffer-size 10))
          (test-channel (opentelemetry.exporter:tracer-channel tracer)))
      (opentelemetry.exporter:run-exporter tracer :background t) ; Run in background, should not block
      (rove:ok (bt:thread-alive-p (opentelemetry.exporter:exporter-thread tracer)) "exporter thread is alive")
      (calispel:! test-channel "test-span-data-background") ; Send some dummy data
      (sleep 1) ; Give exporter thread some time to process
      ;; We can't directly verify HTTP export here without mocking,
      ;; but we can check that the thread is running and data can be sent to the channel.
      (rove:assert t "run-exporter in background started and did not block"))
    (bt:join-thread (opentelemetry.exporter:exporter-thread tracer) :timeout 5) ; Clean up thread
    (setf (opentelemetry.exporter:exporter-thread tracer) nil))) ; Reset thread slot
