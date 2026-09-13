(in-package #:telemetry-backend-otlp/tests)

(deftest otlp-logs-include-trace-id
  (let* ((box (list nil))
         (lb (telemetry-backend-otlp/logs:make-otlp-log-backend
              :endpoint "http://collector.invalid"
              :service-name "unit-test"
              :request-fn (%capture-fn box)))
         (trace-id "0123456789abcdef0123456789abcdef")
         (span-id "0123456789abcdef"))
    (let ((log-protocol:*log-context* (list :trace-id trace-id :span-id span-id)))
      (log-protocol:backend-log lb :info "app" "hello" :fields '()))
    (ok (car box))
    (destructuring-bind (method url headers body) (car box)
      (ok (eq :post method))
      (ok (equal "http://collector.invalid/v1/logs" url))
      (ok (equal "application/json" (cdr (assoc "content-type" headers :test #'equal))))
      (let* ((rl (elt (%ht-get body "resourceLogs") 0))
             (rec (elt (%ht-get (elt (%ht-get rl "scopeLogs") 0) "logRecords") 0)))
        (ok (equal "hello" (%ht-get rec "body" "stringValue")))
        (ok (equal "INFO" (gethash "severityText" rec)))
        (ok (= 9 (gethash "severityNumber" rec)))
        (ok (equal trace-id (gethash "traceId" rec)))
        (ok (equal span-id (gethash "spanId" rec)))))))
