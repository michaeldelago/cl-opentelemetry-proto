(defpackage opentelemetry.integrations.clack
  (:use :cl)
  (:local-nicknames (#:otel #:opentelemetry)
                    (#:otel.trace #:cl-protobufs.opentelemetry.proto.trace.v1))
  (:export #:extract-attributes-from-env
           #:*lack-middleware-opentelemetry*))

(in-package :opentelemetry.integrations.clack)

(defun extract-attributes-from-env (env)
  "Extracts OpenTelemetry-compatible attributes from the Clack env plist."
  (let* ((headers (getf env :headers))
         (attrs (list :http.request.method (getf env :request-method)
                      :url.scheme (getf env :url-scheme)
                      :url.path (getf env :path-info)
                      :url.query (getf env :query-string)
                      ;; :url.full - Could construct this, but often redundant with path/query/scheme/host
                      :network.protocol.version (getf env :server-protocol)
                      :server.address (getf env :server-name)
                      :server.port (getf env :server-port)
                      :client.address (getf env :remote-addr)
                      :client.port (getf env :remote-port)
                      :user_agent.original (gethash "user-agent" headers)))
         out)
    (alexandria:doplist (k v attrs out)
      (when (and k v)
        (push v out)
        (push k out)))))


(defun extract-attributes-from-res (res)
  "Extracts OpenTelemetry-compatible attributes from the Clack response list."
  (list :http.response.status_code (first res)))

(defmacro set-span-route (method target)
  `(prog1 (setf (otel.trace:name otel:*span*) (format nil "~a ~a" ,method ,target))
     ,target))

(defparameter *lack-middleware-opentelemetry*
  (lambda (app)
    (serapeum:dynamic-closure otel:*special-bindings*
                              (lambda (env)
                                (let* ((span-name (format nil "~a" (getf env :request-method)))
                                       (attributes (extract-attributes-from-env env)))
                                  (otel:with-span (span-name :span-kind :span-kind-server :attributes attributes)
                                    (handler-case
                                        (let ((res (funcall app env)))
                                          ;; Set response attributes

                                          ;; Set span status based on HTTP status code
                                          (let ((status-code (first res)))
                                            (if (>= (the fixnum status-code) 500)
                                                (otel:set-span-status-error (format nil "HTTP status code: ~a" status-code))
                                                (otel:set-span-status-ok)))
                                          res)
                                      (error (c)
                                        ;; Record exception and set status to error
                                        ;; TODO: Add otel:record-exception function
                                        (otel:set-span-status-error (format nil "Unhandled error: ~a" c))
                                        ;; Re-signal the error to allow higher-level handlers to catch it
                                        (error c))))))))
  "Clack middleware to wrap each request in an OpenTelemetry span.")
