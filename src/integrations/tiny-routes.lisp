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
;;
;; Write a function that un-templates path-parameters in tiny-routes, so that in :request-uri, "22" is replaced with ":account-id" ai!
