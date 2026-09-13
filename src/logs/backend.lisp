(defpackage #:telemetry-backend-otlp/logs
  (:use #:cl #:telemetry-protocol #:telemetry-backend-otlp)
  (:export #:otlp-log-backend
           #:make-otlp-log-backend
           #:encode-otlp-logs))

(in-package #:telemetry-backend-otlp/logs)

(defclass otlp-log-backend (log-protocol:log-backend)
  ((endpoint :initarg :endpoint :accessor otlp-log-endpoint
             :initform +default-otlp-endpoint+)
   (headers :initarg :headers :accessor otlp-log-headers :initform nil)
   (service-name :initarg :service-name :accessor otlp-log-service-name
                 :initform "unknown_service")
   (request-fn :initarg :request-fn :accessor otlp-log-request-fn :initform nil)))

(defun make-otlp-log-backend (&key endpoint headers service-name request-fn)
  (make-instance 'otlp-log-backend
                 :endpoint (telemetry-backend-otlp::%logs-url
                            (or endpoint
                                (telemetry-backend-otlp::%env
                                 "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT")
                                (telemetry-backend-otlp::%env
                                 "OTEL_EXPORTER_OTLP_ENDPOINT")
                                +default-otlp-endpoint+))
                 :headers (or headers
                              (telemetry-backend-otlp::%parse-headers
                               (telemetry-backend-otlp::%env
                                "OTEL_EXPORTER_OTLP_HEADERS")))
                 :service-name (or service-name
                                   (telemetry-backend-otlp::%env "OTEL_SERVICE_NAME")
                                   "unknown_service")
                 :request-fn request-fn))

(defun %plist-get (plist key)
  (or (getf plist key)
      (loop for (k v) on plist by #'cddr
            when (and k (string-equal (string k) (string key)))
              return v)))

(defun %context-id (fields key)
  (or (%plist-get fields key)
      (%plist-get log-protocol:*log-context* key)))

(defun %severity-number (level)
  (ecase level
    (:trace 1)
    (:debug 5)
    (:info 9)
    (:warn 13)
    (:error 17)
    (:fatal 21)))

(defun %log-attributes (logger-name fields)
  (telemetry-backend-otlp::%attrs
   (append (list "logger" logger-name)
           (loop for (k v) on fields by #'cddr
                 unless (and k (member (string-downcase (string k))
                                       '("trace-id" "span-id" "trace_id" "span_id")
                                       :test #'string=))
                   collect k and collect v))))

(defun %encode-log-record (level logger-name message fields)
  (let ((trace-id (%context-id fields :trace-id))
        (span-id (%context-id fields :span-id)))
    (telemetry-backend-otlp::%ht
     "timeUnixNano" (telemetry-backend-otlp::%ns-string (telemetry-unix-nano))
     "severityNumber" (%severity-number level)
     "severityText" (string-upcase (symbol-name level))
     "body" (telemetry-backend-otlp::%ht "stringValue" (or message ""))
     "attributes" (%log-attributes logger-name fields)
     "traceId" (or trace-id :omit)
     "spanId" (or span-id :omit))))

(defun %logs-payload (backend level logger-name message fields)
  (telemetry-backend-otlp::%ht
   "resourceLogs"
   (vector
    (telemetry-backend-otlp::%ht
     "resource" (telemetry-backend-otlp::%resource-ht (otlp-log-service-name backend))
     "scopeLogs"
     (vector
      (telemetry-backend-otlp::%ht
       "scope" (telemetry-backend-otlp::%scope-ht)
       "logRecords" (vector (%encode-log-record level logger-name message fields))))))))

(defun encode-otlp-logs (backend level logger-name message &key fields)
  "Hash-table OTLP/HTTP JSON logs payload (resourceLogs / scopeLogs)."
  (%logs-payload backend level logger-name message (or fields '())))

(defmethod log-protocol:backend-log ((backend otlp-log-backend) level logger-name
                                     message &key fields layout format)
  (declare (ignore layout format))
  (let* ((fn (or (otlp-log-request-fn backend)
                 #'telemetry-backend-otlp::%http-request))
         (payload (%logs-payload backend level logger-name message (or fields '())))
         (content (stack-json:encode payload))
         (headers (append '(("content-type" . "application/json")
                            ("accept" . "application/json"))
                          (otlp-log-headers backend))))
    (multiple-value-bind (status body)
        (funcall fn :post (otlp-log-endpoint backend)
                 :headers headers
                 :content content)
      (unless (<= 200 status 299)
        (error 'telemetry-error
               :message (format nil "OTLP logs export HTTP ~a~@[: ~a~]"
                                status
                                (and body (plusp (length body))
                                     (subseq body 0 (min 200 (length body)))))))))
  backend)
