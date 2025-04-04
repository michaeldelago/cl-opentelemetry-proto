# Opentelemetry-Common-Lisp

**This is experimental, use at your own risk**

## How to use

To use the opentelemetry in your Common Lisp application, follow these steps:

1. **Load the library**: Ensure that the `cl-otel` library is loaded in your Lisp environment. You can use Quicklisp to load it:
   ```lisp
   (ql:quickload :cl-otel)
   ```

2. **Create a Tracer**: Instantiate a tracer, providing the OTLP endpoint where you want to send your telemetry data. For example, to send traces to a local OTLP collector at `http://localhost:4318/v1/traces`:
   ```lisp
   (defvar my-tracer (opentelemetry:make-tracer "http://localhost:4318/v1/traces"))
   (setf opentelemetry:*tracer* my-tracer)
   ```
   You can configure the tracer with options like `channel-buffer-size`, `max-spans-per-batch`, and `export-timeout-ms` if needed.

3. **Set up a Resource**: Define a resource that represents your service or application. This resource will be associated with all spans created within its scope. Use the `with-resource` macro, providing a service name and any other resource attributes:
   ```lisp
   (opentelemetry:with-resource ("my-service" :service.version "1.0.0")
     ;; Your code that creates spans goes here
     )
   ```

4. **Create Spans**: Use the `with-span` macro to create spans around operations you want to trace. Provide a span name and the code you want to execute within the span:
   ```lisp
   (opentelemetry:with-resource ("my-service")
     (opentelemetry:run-exporter my-tracer) ; Start the exporter within the resource scope
     (opentelemetry:with-span ("my-operation")
       (sleep 0.5) ; Simulate some work
       (opentelemetry:with-span ("my-sub-operation")
         (identity "hello"))))
   ```
   **Important**: Ensure `run-exporter` is called within the `with-resource` scope to properly associate the resource with the exporter thread.

5. **Run the Exporter**: Start the exporter to begin sending collected spans to the OTLP endpoint.  It's recommended to start the exporter within the `with-resource` scope to ensure the resource context is properly set for the exporter thread:
   ```lisp
   (opentelemetry:with-resource ("my-service")
     (let ((my-tracer (opentelemetry:make-tracer "http://localhost:4318/v1/traces")))
       (setf opentelemetry:*tracer* my-tracer)
       (opentelemetry:run-exporter my-tracer)
       (opentelemetry:with-span ("my-operation")
         (sleep 0.5))))
   ```

This basic guide should help you get started with using the opentelemetry-common-lisp library and exporting traces. Remember to consult the code and tests for more advanced usage and configuration options.
