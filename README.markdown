# Opentelemetry-Common-Lisp

**This is experimental, use at your own risk**

OpenTelemetry SDK for Common Lisp

## Gathering dependencies

This project depends on [qitab/cl-protobufs](https://github.com/qitab/cl-protobufs) for working with the OpenTelemetry data model and formats. 

In the code for this project, the generated lisp source for the OpenTelemetry data types is included, but the `qitab/cl-protobufs` project itself is not included. 

As cl-protobufs is not in quicklisp, you'll need to make sure it's available to your local lisp environment:

```bash
$ git clone https://github.com/qitab/cl-protobufs ~/common-lisp/qitab-cl-protobufs
```

## Usage

To use the opentelemetry exporter in your Common Lisp application, follow these steps:

1. **Load the library**: Ensure that the `cl-otel` library is loaded in your Lisp environment. You can use Quicklisp to load it:
   ```lisp
   (ql:quickload :cl-otel)
   ```

2. **Create a Tracer**: Instantiate a tracer, providing the OTLP endpoint where you want to send your telemetry data. For example, to send traces to a local OTLP collector at `http://localhost:4318/v1/traces`:
   ```lisp
   (let ((opentelemetry:*tracer* (opentelemetry:make-tracer "http://localhost:4318/v1/traces")))
     (print "do something"))
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

### Setting Span Status

You can set the status of a span to indicate the outcome of the operation it represents.  The status can be set to OK or ERROR, and optionally include a description.

Use `opentelemetry:set-span-status-ok` to set the span status to OK:

```lisp
(opentelemetry:with-span ("my-operation")
  ;; Perform operation
  (opentelemetry:set-span-status-ok "Operation completed successfully"))
```

Use `opentelemetry:set-span-status-error` to set the span status to ERROR:

```lisp
(opentelemetry:with-span ("another-operation")
  ;; Perform operation that resulted in an error
  (opentelemetry:set-span-status-error "Something went wrong"))
```

### Adding Span Attributes

Attributes provide additional context and information about a span. You can add attributes to a span using the `:attributes` key in the `with-span` macro, or by using `opentelemetry:set-span-attribute` within a span's scope.

**Setting attributes when creating a span:**

```lisp
(opentelemetry:with-span ("http-request" :attributes '(:http.method "GET" :http.url "/resource"))
  ;; Perform HTTP request
  (print "Making request"))
```

**Setting attributes within a span:**

```lisp
(opentelemetry:with-span ("process-data")
  (opentelemetry:set-span-attribute :data.item-count 100)
  ;; Process data
  (print "Processing data"))
```

Attributes are key-value pairs where keys are strings or symbols and values can be strings, numbers, or booleans.

### Adding Span Events

Events are specific points in time during a Span's execution, and can be used to attach textual messages or attribute information to the span.

Use `opentelemetry:new-span-event` to add an event to the current span:

```lisp
(opentelemetry:with-span ("sort-data")
  (opentelemetry:new-span-event "start-sort" '(:algorithm "quick-sort"))
  ;; Perform sort operation
  (opentelemetry:new-span-event "end-sort" '(:item-count 1000))
  (print "Sorting data"))
```

Events have a name and optional attributes, similar to spans. They can be useful for logging significant occurrences during the span's operation.
