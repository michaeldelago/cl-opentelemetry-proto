(defpackage opentelemetry.integrations.tiny-routes
  (:use :cl)
  (:local-nicknames (#:otel #:opentelemetry))
  (:export #:*tiny-routes-middleware-opentelemetry*))

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

(defun untemplate-path-info (env)
  "Un-templates path parameters in a tiny-routes request's path-info.
  Replaces actual parameter values (e.g., '22') with their keys (e.g., ':account-id')."
  (let ((path-info (getf env :path-info))
        (path-params (getf env :path-parameters)))
    (loop for (key value) on path-params by #'cddr
          do (setf path-info (cl-ppcre:regex-replace-all (format nil "/~a(?=/|$)" (cl-ppcre:quote-meta-chars value))
                                                          path-info
                                                          (format nil "/~a" (string-downcase (symbol-name key))))))
    path-info))
