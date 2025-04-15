(defpackage opentelemetry.integrations.clack
  (:use :cl)
  (:local-nicknames
   (#:otel #:opentelemetry))
  (:import-from #:lack/util
                #:funcall-with-cb)
  (:export #:*lack-middleware-opentelemetry*))

(in-package :opentelemetry.integrations.clack)

(defun extract-attributes-from-env (env)
  "Extracts OpenTelemetry-compatible attributes from the Clack env plist."
  (let ((headers (getf env :headers)))
    (remove nil
            (list :http.request.method (getf env :request-method)
                  :url.scheme (getf env :url-scheme)
                  :url.path (getf env :path-info)
                  :url.query (getf env :query-string)
                  ;; :url.full - Could construct this, but often redundant with path/query/scheme/host
                  :network.protocol.version (getf env :server-protocol)
                  :server.address (getf env :server-name)
                  :server.port (getf env :server-port)
                  :client.address (getf env :remote-addr)
                  :client.port (getf env :remote-port)
                  :user_agent.original (gethash "user-agent" headers))
            :key #'second)))

(defun extract-attributes-from-res (res)
  "Extracts OpenTelemetry-compatible attributes from the Clack response list."
  (list :http.response.status_code (first res)))

(defparameter *lack-middleware-opentelemetry*
  (lambda (app)
    (lambda (env)
      (let* ((span-name (format nil "HTTP ~a" (getf env :request-method)))
             (attributes (extract-attributes-from-env env)))
        (otel:with-span (span-name :span-kind :server :attributes attributes)
          (handler-case
              (let ((res (funcall app env)))
                ;; Set response attributes
                (loop for (key value) on (extract-attributes-from-res res) by #'cddr
                      do (otel:set-span-attribute key value))

                ;; Set span status based on HTTP status code
                (let ((status-code (first res)))
                  (if (and (integerp status-code) (>= status-code 500))
                      (otel:set-span-status-error (format nil "HTTP status code: ~a" status-code))
                      (otel:set-span-status-ok)))
                res)
            (error (c)
              ;; Record exception and set status to error
              ;; TODO: Add otel:record-exception function
              (otel:set-span-status-error (format nil "Unhandled error: ~a" c))
              ;; Re-signal the error to allow higher-level handlers to catch it
              (error c)))))))
  "Clack middleware to wrap each request in an OpenTelemetry span.")
