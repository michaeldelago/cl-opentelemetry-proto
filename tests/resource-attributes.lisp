(in-package :opentelemetry/tests)

(define-test test-plist-to-attributes-empty
  "Test plist-to-attributes with an empty plist"
  (is = 0 (length (opentelemetry::plist-to-attributes nil))))

(define-test test-plist-to-attributes-string
  "Test plist-to-attributes with a string value"
  (let ((attrs (opentelemetry::plist-to-attributes '(:key1 "string-value"))))
    (is = 1 (length attrs))
    (let ((attr (first attrs)))
      (is equal "key1" (otel.common:key-value.key attr))
      (is equal "string-value" (otel.common:any-value.string-value (otel.common:key-value.value attr))))))

(define-test test-plist-to-attributes-integer
  "Test plist-to-attributes with an integer value"
  (let ((attrs (opentelemetry::plist-to-attributes '(:key2 123))))
    (is = 1 (length attrs))
    (let ((attr (first attrs)))
      (is equal "key2" (otel.common:key-value.key attr))
      (is = 123 (otel.common:any-value.int-value (otel.common:key-value.value attr))))))

(define-test test-plist-to-attributes-float
  "Test plist-to-attributes with a float value"
  (let ((attrs (opentelemetry::plist-to-attributes '(:key3 3.14))))
    (is = 1 (length attrs))
    (let ((attr (first attrs)))
      (is equal "key3" (otel.common:key-value.key attr))
      (is = 3.14 (otel.common:any-value.double-value (otel.common:key-value.value attr))))))

(define-test test-plist-to-attributes-boolean-true
  "Test plist-to-attributes with a boolean true value"
  (let ((attrs (opentelemetry::plist-to-attributes '(:key4 t))))
    (is = 1 (length attrs))
    (let ((attr (first attrs)))
      (is equal "key4" (otel.common:key-value.key attr))
      (is eq t (otel.common:any-value.bool-value (otel.common:key-value.value attr))))))

(define-test test-plist-to-attributes-boolean-false
  "Test plist-to-attributes with a boolean false value"
  (let ((attrs (opentelemetry::plist-to-attributes '(:key5 nil))))
    (is = 1 (length attrs))
    (let ((attr (first attrs)))
      (is equal "key5" (otel.common:key-value.key attr))
      (is eq nil (otel.common:any-value.bool-value (otel.common:key-value.value attr))))))

(define-test test-plist-to-attributes-list
  "Test plist-to-attributes with a list value"
  (let ((attrs (opentelemetry::plist-to-attributes `(:key6 ,(list 1 "a" 3.14)))))
    (is = 1 (length attrs))
    (let ((attr (first attrs)))
      (is equal "key6" (otel.common:key-value.key attr))
      (let ((array-value (otel.common:any-value.array-value (otel.common:key-value.value attr))))
        (is = 3 (length (otel.common:array-value.values array-value)))
        (is = 1 (otel.common:any-value.int-value (elt (otel.common:array-value.values array-value) 0)))
        (is equal "a" (otel.common:any-value.string-value (elt (otel.common:array-value.values array-value) 1)))
        (is = 3.14 (otel.common:any-value.double-value (elt (otel.common:array-value.values array-value) 2)))))))

(define-test test-plist-to-attributes-hash-table
  "Test plist-to-attributes with a hash-table value"
  (let ((attrs (opentelemetry::plist-to-attributes `(:key7 ,(serapeum:dict :nested-key1 "nested-value1"
                                                                           :nested-key2 100)))))
    (is = 1 (length attrs))
    (let ((attr (first attrs)))
      (is equal "key7" (otel.common:key-value.key attr))
      (let ((kvlist-value (otel.common:any-value.kvlist-value (otel.common:key-value.value attr))))
        (is = 2 (length (otel.common:key-value-list.values kvlist-value)))
        (let ((kv1 (find-if (lambda (kv) (equal "nested-key1" (otel.common:key-value.key kv))) (otel.common:key-value-list.values kvlist-value)))
              (kv2 (find-if (lambda (kv) (equal "nested-key2" (otel.common:key-value.key kv))) (otel.common:key-value-list.values kvlist-value))))
          (true kv1)
          (true kv2)
          (is equal "nested-value1" (otel.common:any-value.string-value (otel.common:key-value.value kv1)))
          (is = 100 (otel.common:any-value.int-value (otel.common:key-value.value kv2))))))))

(define-test test-plist-to-attributes-mixed
  "Test plist-to-attributes with mixed value types"
  (let ((attrs (opentelemetry::plist-to-attributes '(:key1 "string" :key2 123 :key3 3.14))))
    (is = 3 (length attrs))
    (is equal "key1" (otel.common:key-value.key (elt attrs 0)))
    (is equal "string" (otel.common:any-value.string-value (otel.common:key-value.value (elt attrs 0))))
    (is equal "key2" (otel.common:key-value.key (elt attrs 1)))
    (is = 123 (otel.common:any-value.int-value (otel.common:key-value.value (elt attrs 1))))
    (is equal "key3" (otel.common:key-value.key (elt attrs 2)))
    (is = 3.14 (otel.common:any-value.double-value (otel.common:key-value.value (elt attrs 2))))))
