(in-package :opentelemetry)

(defmacro with-span ((span-name &key (span-kind :SPAN-KIND-INTERNAL)) &body body)
  `(let* ((*trace-id* (or opentelemetry:*trace-id* (generate-trace-id)))
          (parent-span-id opentelemetry:*current-span-id*)
          (*current-span-id* (generate-span-id))
          (start-time (timestamp))
          (end-time 0)                  ; Initialize end-time
          (*span* (otel.trace:make-span
                   :trace-id *trace-id*
                   :span-id *current-span-id*
                   :parent-span-id parent-span-id
                   :name ,span-name
                   :kind ,span-kind
                   :start-time-unix-nano start-time)))
     (unwind-protect
          (prog1
              ,@body
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
