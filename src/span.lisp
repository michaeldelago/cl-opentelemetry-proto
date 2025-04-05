(in-package :opentelemetry)

(defmacro with-span ((span-name &key attributes (span-kind :SPAN-KIND-INTERNAL)) &body body)
  "Create and execute a span.

  This macro simplifies the creation and management of spans for tracing code execution. It automatically handles span lifecycle, including starting and ending the span, and recording it.

  The span is automatically associated with the current trace and becomes the active span within the dynamic scope of the body.

  - SPAN-NAME: A string or symbol representing the name of the span.
  - SPAN-KIND (keyword, optional): Specifies the kind of span. Defaults to `:SPAN-KIND-INTERNAL`.
    Possible values are:
      - `:SPAN-KIND-INTERNAL`:  Span represents internal application logic.
      - `:SPAN-KIND-SERVER`:    Span represents a server-side operation.
      - `:SPAN-KIND-CLIENT`:    Span represents a client-side operation.
      - `:SPAN-KIND-PRODUCER`:  Span represents a producer of messages.
      - `:SPAN-KIND-CONSUMER`:  Span represents a consumer of messages.
  - ATTRIBUTES (plist, optional): A property list of attributes to associate with the span. Keys should be symbols or strings, and values should be valid attribute types (strings, numbers, booleans, or lists of these).

  Example:

    (with-span (\"my-operation\" :attributes '(:http.method \"GET\" :http.url \"/resource\"))
      ;; Code to be traced within the span
      (do-something))
  "
  (serapeum:with-thunk (body)
    `(call-with-span ,span-name ,body :span-kind ,span-kind :attributes ,attributes)))

(defun call-with-span (span-name thunk &key (span-kind :SPAN-KIND-INTERNAL) attributes)
  "Create and execute a span with explicit control over the thunk.

  This function is the underlying implementation for `with-span` macro, providing more direct control by accepting a thunk (a function with no arguments) to represent the code to be executed within the span.

  It is generally recommended to use the `with-span` macro for simpler span management. This function is useful for cases where you need to programmatically create and manage spans outside of the macro's convenience.

  - SPAN-NAME: A string or symbol representing the name of the span.
  - THUNK: A function with no arguments (a thunk) that encapsulates the code to be executed within the span.
  - SPAN-KIND (keyword, optional): Specifies the kind of span, same as in `with-span`. Defaults to `:SPAN-KIND-INTERNAL`.
  - ATTRIBUTES (plist, optional): A property list of attributes to associate with the span, same as in `with-span`.

  Returns:
    The result of calling the THUNK.

  Side Effects:
    Creates and records a span. Sends the span to the configured tracer.

  Example:

    (call-with-span \"my-async-operation\" (lambda () (do-async-work)) :span-kind :SPAN-KIND-CLIENT)
  "
  (let* ((*trace-id* (or opentelemetry:*trace-id* (generate-trace-id)))
         (parent-span-id opentelemetry:*current-span-id*)
         (*current-span-id* (generate-span-id))
         (start-time (timestamp))
         (end-time 0)                  ; Initialize end-time
         (*span* (otel.trace:make-span
                  :trace-id *trace-id*
                  :span-id *current-span-id*
                  :parent-span-id parent-span-id
                  :name span-name
                  :kind span-kind
                  :start-time-unix-nano start-time
                  :attributes (plist-to-attributes attributes))))
    (unwind-protect
         (prog1
             (funcall thunk)
           ;; Capture the end time after the body executes
           (setf end-time (timestamp)))
      ;; Cleanup form: Set end time and send to channel
      (setf (otel.trace:end-time-unix-nano *span*) end-time)
      ;; Use the channel from the active *tracer* instance
      (when (and *tracer* (tracer-channel *tracer*))
        (calispel:! (tracer-channel *tracer*) *span*)))))

(defun set-span-attribute (key value)
  "Set an attribute on the current span."
  (when *span*
    (let ((current-attributes (or (otel.trace:span.attributes *span*) (list))))
      (setf (otel.trace:span.attributes *span*)
            (append current-attributes
                    (list (otel.common:make-key-value :key (symbol-to-string key) :value (get-otel-value value))))))))


(defun set-span-status-ok (&optional description)
  (set-span-status :status-code-ok description))

(defun set-span-status-error (&optional description)
  (set-span-status :status-code-error description))

(defun set-span-status (status &optional description)
  "Set the status of the current span."
  (when *span*
    (setf (otel.trace:span.status *span*)
          (otel.trace:make-status
           :code status
           :message description))))
