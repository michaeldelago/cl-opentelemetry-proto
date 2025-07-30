(defpackage opentelemetry.integrations.tiny-routes
  (:use :cl)
  (:local-nicknames (#:otel #:opentelemetry)
                    (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1)
                    (#:otel.clack #:opentelemetry.integrations.clack))
  (:export #:tracing-middleware))

(in-package :opentelemetry.integrations.tiny-routes)

;; the request is a plist like this:
;;
;; (:PATH-PARAMETERS (:ACCOUNT-ID "22") :REQUEST-METHOD :GET :SCRIPT-NAME ""
;;  :PATH-INFO "/accounts/22" :SERVER-NAME "localhost" :SERVER-PORT 8080
;;  :SERVER-PROTOCOL :HTTP/1.1 :REQUEST-URI "/accounts/22" :URL-SCHEME "http"
;;  :REMOTE-ADDR "127.0.0.1" :REMOTE-PORT 52524 :QUERY-STRING NIL
;;  :RAW-BODY #<FLEXI-STREAMS:FLEXI-IO-STREAM {1000CE8733}> :CONTENT-LENGTH NIL
;;  :CONTENT-TYPE NIL
;;  :CLACK.STREAMING T
;;  :CLACK.IO #<CLACK.HANDLER.HUNCHENTOOT::CLIENT {1000D57973}>
;;  :HEADERS #<HASH-TABLE :TEST EQUAL :COUNT 14 {1000D98203}>)

(defun untemplate-path-info (req)
  "Un-templates path parameters in a tiny-routes request's path-info.
  Replaces actual parameter values (e.g., '22') with their keys (e.g., ':account-id').

  (untemplate-path-info
    '(:PATH-PARAMETERS (:ACCOUNT-ID \"22\" :rule \"13\")
      :PATH-INFO \"/accounts/22/13\"))
  => \"/accounts/:account-id/:rule\""
  (let ((path-info (uiop:split-string (getf req :path-info) :separator '(#\slash)))
        (val-to-key (loop for (param arg) on (getf req :path-parameters) by #'cddr collect (cons arg param))))
    (format nil "~{~A~^/~}"
            (mapcar (lambda (arg)
                      (let ((key (cdr (serapeum:pop-assoc arg val-to-key :test #'equal))))
                        (if (not key)
                            arg
                            (string-downcase (prin1-to-string key)))))
                    path-info))))



(defun tracing-middleware-int (handler)
  (serapeum:dynamic-closure otel:*special-bindings*
                            (lambda (request)
                              (let ((span-name (format nil "~a ~a" (getf request :request-method) (untemplate-path-info request))))
                                (if otel:*span*
                                    (progn
                                      (setf (otel.trace:name otel:*span*) span-name)
                                      (funcall handler request))
                                    (let* ((attributes (otel.clack:extract-attributes-from-env request)))
                                      (otel:with-span (span-name :span-kind :span-kind-server :attributes attributes)
                                        (handler-case
                                            (let ((res (funcall handler request)))

                                              ;; Set span status based on HTTP status code
                                              (let ((status-code (first res)))
                                                (if (>= (the fixnum status-code) 500)
                                                    (otel:set-span-status-error (format nil "HTTP status code: ~a" status-code))
                                                    (otel:set-span-status-ok)))
                                              res)
                                          (error (c)
                                            ;; TODO Record exception and set status to error
                                            (otel:set-span-status-error (format nil "Unhandled error: ~a" c))
                                            ;; Re-signal the error to allow higher-level handlers to catch it
                                            (error c))))))))))

(defun tracing-middleware (handler)
  "tiny-routes middleware to wrap each request in an OpenTelemetry span."
  (tiny-routes:wrap-post-match-middleware handler #'tracing-middleware-int))
