(in-package :opentelemetry)

(serapeum:-> timestamp () fixnum)
(serapeum:defsubst timestamp ()
  "Returns the current time as nanoseconds since the Unix epoch"
  (let ((now (local-time:now)))
    (+ (* (local-time:timestamp-to-unix now) 1000000000)
       (local-time:nsec-of now))))

(serapeum:-> random-byte-array (fixnum) (vector (unsigned-byte 8)))
(serapeum:defsubst random-byte-array (size)
  (map '(vector (unsigned-byte 8)) (lambda (x) (declare (ignorable x)) (random 256)) (cl-protobufs:make-byte-vector size :adjustable nil)))

(defun generate-span-id ()
  (random-byte-array 8))

(defun generate-trace-id ()
  (random-byte-array 16))

(defun symbol-to-string (sym)
  (typecase sym
    (symbol (string-downcase (format nil "~a" sym)))
    (t sym)))

(defun get-otel-value (value)
  (typecase value
    (keyword (otel.common:make-any-value :string-value (symbol-to-string value)))
    (string (otel.common:make-any-value :string-value value))
    (integer (otel.common:make-any-value :int-value value))
    (float (otel.common:make-any-value :double-value (float value 1.0d0)))
    (hash-table (otel.common:make-any-value :kvlist-value (otel.common:make-key-value-list :values (serapeum:maphash-return (serapeum:op (otel.common:make-key-value :key (symbol-to-string _) :value (get-otel-value _))) value))))
    (null (otel.common:make-any-value :bool-value value))
    ((or list array) (otel.common:make-any-value :array-value (otel.common:make-array-value :values (map 'list #'get-otel-value value))))
    (t (otel.common:make-any-value :bool-value (when (eq t value)
                                                 value)
                                   :string-value (unless (eq t value)
                                                   (symbol-to-string value))))))

(defun plist-to-attributes (attrs)
  (loop for (key value) on attrs by #'cddr
        collect (otel.common:make-key-value :key (symbol-to-string key) :value (get-otel-value value))))
