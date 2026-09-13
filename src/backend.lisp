(in-package #:telemetry-backend-otlp)

(defparameter +default-otlp-endpoint+ "http://127.0.0.1:4318"
  "Collector base. Traces POST to {endpoint}/v1/traces.")

(defparameter +scope-name+ "telemetry-backend-otlp")
(defparameter +scope-version+ "0.2.0")

(defun %env (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defun %parse-headers (s)
  (when (and s (plusp (length s)))
    (loop for part in (uiop:split-string s :separator ",")
          for trimmed = (string-trim '(#\Space #\Tab) part)
          for eq = (position #\= trimmed)
          when (and eq (plusp eq))
            collect (cons (subseq trimmed 0 eq)
                          (subseq trimmed (1+ eq))))))

(defun %v1-url (endpoint signal)
  "ENDPOINT may be a collector base or a /v1/{traces|metrics|logs} URL."
  (let* ((base (string-right-trim "/" (or endpoint +default-otlp-endpoint+)))
         (cut (loop for marker in '("/v1/traces" "/v1/metrics" "/v1/logs")
                    for pos = (search marker base)
                    when pos return pos)))
    (format nil "~a/v1/~a" (if cut (subseq base 0 cut) base) signal)))

(defun %traces-url (endpoint)
  (%v1-url endpoint "traces"))

(defun %metrics-url (endpoint)
  (%v1-url endpoint "metrics"))

(defun %logs-url (endpoint)
  (%v1-url endpoint "logs"))

(defclass otlp-telemetry-backend (telemetry-backend)
  ((endpoint :initarg :endpoint :accessor otlp-endpoint
             :initform +default-otlp-endpoint+)
   (headers :initarg :headers :accessor otlp-headers :initform nil)
   (service-name :initarg :service-name :accessor otlp-service-name
                 :initform "unknown_service")
   (request-fn :initarg :request-fn :accessor otlp-request-fn :initform nil)
   (max-batch :initarg :max-batch :accessor otlp-max-batch :initform 64)
   (pending :initform nil :accessor otlp-pending-spans)
   (pending-metrics :initform nil :accessor otlp-pending-metrics)))

(defun make-otlp-telemetry-backend (&key endpoint headers service-name
                                      request-fn max-batch)
  (make-instance 'otlp-telemetry-backend
                 :endpoint (%traces-url
                            (or endpoint
                                (%env "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT")
                                (%env "OTEL_EXPORTER_OTLP_ENDPOINT")
                                +default-otlp-endpoint+))
                 :headers (or headers (%parse-headers
                                       (%env "OTEL_EXPORTER_OTLP_HEADERS")))
                 :service-name (or service-name (%env "OTEL_SERVICE_NAME")
                                   "unknown_service")
                 :request-fn request-fn
                 :max-batch (or max-batch 64)))

(defun use-otlp-telemetry (&rest args &key &allow-other-keys)
  (setf *telemetry-backend* (apply #'make-otlp-telemetry-backend args)
        *tracer-provider* nil
        *current-span* nil
        *current-trace-id* nil)
  *telemetry-backend*)

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit))
            do (setf (gethash k h) v))
    h))

(defun %any-value (v)
  (cond
    ((eq v t) (%ht "boolValue" t))
    ((null v) (%ht "boolValue" nil))
    ((stringp v) (%ht "stringValue" v))
    ((integerp v) (%ht "intValue" (princ-to-string v)))
    ((floatp v) (%ht "doubleValue" v))
    (t (%ht "stringValue" (princ-to-string v)))))

(defun %attrs (plist)
  (coerce
   (loop for (k v) on plist by #'cddr
         collect (%ht "key" (if (stringp k) k (string-downcase (string k)))
                      "value" (%any-value v)))
   'vector))

(defun %kind (k)
  (ecase k
    ((:internal nil) 1)
    (:server 2)
    (:client 3)))

(defun %status (s)
  (%ht "code" (ecase (or s :unset)
                (:unset 0)
                (:ok 1)
                (:error 2))))

(defun %ns-string (n)
  (princ-to-string (or n 0)))

(defun %encode-event (ev)
  (%ht "timeUnixNano" (%ns-string (telemetry-event-time-unix-ns ev))
       "name" (telemetry-event-name ev)
       "attributes" (%attrs (telemetry-event-attributes ev))))

(defun %encode-span (span)
  (let ((parent (telemetry-span-parent-id span))
        (events (reverse (telemetry-span-events span))))
    (%ht "traceId" (telemetry-span-trace-id span)
         "spanId" (telemetry-span-id span)
         "parentSpanId" (or parent :omit)
         "name" (telemetry-span-name span)
         "kind" (%kind (telemetry-span-kind span))
         "startTimeUnixNano" (%ns-string (telemetry-span-start-unix-ns span))
         "endTimeUnixNano" (%ns-string (telemetry-span-end-unix-ns span))
         "attributes" (%attrs (telemetry-span-attributes span))
         "events" (coerce (mapcar #'%encode-event events) 'vector)
         "status" (%status (telemetry-span-status span)))))

(defun %resource-ht (service-name)
  (%ht "attributes"
       (vector (%ht "key" "service.name"
                    "value" (%ht "stringValue" service-name)))))

(defun %scope-ht ()
  (%ht "name" +scope-name+ "version" +scope-version+))

(defun %traces-payload (backend spans)
  (%ht "resourceSpans"
       (vector
        (%ht "resource" (%resource-ht (otlp-service-name backend))
             "scopeSpans"
             (vector
              (%ht "scope" (%scope-ht)
                   "spans" (coerce (mapcar #'%encode-span spans) 'vector)))))))

(defun %number-dp (metric)
  (let ((v (telemetry-metric-value metric)))
    (%ht "attributes" (%attrs (telemetry-metric-attributes metric))
         "timeUnixNano" (%ns-string (telemetry-unix-nano))
         (if (integerp v) "asInt" "asDouble")
         (if (integerp v) (princ-to-string v) (float v 1.0d0)))))

(defun %histogram-index (value bounds)
  (or (position-if (lambda (b) (<= value b)) bounds)
      (length bounds)))

(defun %histogram-dp (metric)
  (let* ((value (telemetry-metric-value metric))
         (bounds (or (telemetry-metric-boundaries metric) '()))
         (counts (make-array (1+ (length bounds)) :initial-element 0)))
    (incf (aref counts (%histogram-index value bounds)))
    (%ht "attributes" (%attrs (telemetry-metric-attributes metric))
         "timeUnixNano" (%ns-string (telemetry-unix-nano))
         "count" "1"
         "sum" (float value 1.0d0)
         "bucketCounts" (map 'vector #'princ-to-string counts)
         "explicitBounds" (coerce bounds 'vector))))

(defun %encode-metric (metric)
  (let* ((kind (or (telemetry-metric-kind metric) :counter))
         (name (telemetry-metric-name metric))
         (unit (or (telemetry-metric-unit metric) ""))
         (dp (%number-dp metric)))
    (ecase kind
      (:counter
       (%ht "name" name "unit" unit
            "sum" (%ht "aggregationTemporality" 1
                       "isMonotonic" t
                       "dataPoints" (vector dp))))
      (:up-down-counter
       (%ht "name" name "unit" unit
            "sum" (%ht "aggregationTemporality" 1
                       "isMonotonic" nil
                       "dataPoints" (vector dp))))
      (:gauge
       (%ht "name" name "unit" unit
            "gauge" (%ht "dataPoints" (vector dp))))
      (:histogram
       (%ht "name" name "unit" unit
            "histogram" (%ht "aggregationTemporality" 1
                             "dataPoints" (vector (%histogram-dp metric))))))))

(defun %metrics-payload (backend metrics)
  (%ht "resourceMetrics"
       (vector
        (%ht "resource" (%resource-ht (otlp-service-name backend))
             "scopeMetrics"
             (vector
              (%ht "scope" (%scope-ht)
                   "metrics" (coerce (mapcar #'%encode-metric metrics)
                                     'vector)))))))

(defun encode-otlp-metrics (backend metrics)
  "Hash-table OTLP/HTTP JSON metrics payload (resourceMetrics / scopeMetrics)."
  (%metrics-payload backend metrics))

(defun %headers (backend)
  (append '(("content-type" . "application/json")
            ("accept" . "application/json"))
          (otlp-headers backend)))

(defun %body-string (response)
  (let ((b (http-protocol:response-body response)))
    (cond
      ((stringp b) b)
      ((and (vectorp b) (not (stringp b)))
       (babel:octets-to-string b :encoding :utf-8))
      (t ""))))

(defun %http-request (method url &key headers content)
  (unless http-protocol:*http-backend*
    (error 'telemetry-error
           :message "*http-backend* is nil — bind an http-protocol backend"))
  (let ((res (apply #'http:request method url
                    :headers headers
                    :timeout 10
                    (and content (list :content content)))))
    (values (http-protocol:response-status res) (%body-string res))))

(defun %export-json (backend url payload)
  (let* ((fn (or (otlp-request-fn backend) #'%http-request))
         (content (stack-json:encode payload)))
    (multiple-value-bind (status body)
        (funcall fn :post url
                 :headers (%headers backend)
                 :content content)
      (unless (<= 200 status 299)
        (error 'telemetry-error
               :message (format nil "OTLP export HTTP ~a~@[: ~a~]"
                                status
                                (and body (plusp (length body))
                                     (subseq body 0 (min 200 (length body))))))))))

(defun %export-traces (backend spans)
  (%export-json backend (otlp-endpoint backend) (%traces-payload backend spans)))

(defun %export-metrics (backend metrics)
  (%export-json backend (%metrics-url (otlp-endpoint backend))
                (%metrics-payload backend metrics)))

(defmethod end-span :after ((backend otlp-telemetry-backend) span
                            &key status attributes)
  (declare (ignore status attributes))
  (when (and span (telemetry-span-ended-p span))
    (push span (otlp-pending-spans backend))
    (when (>= (length (otlp-pending-spans backend)) (otlp-max-batch backend))
      (flush-telemetry backend))))

(defmethod record-instrument ((backend otlp-telemetry-backend) instrument value
                              &key attributes)
  (let ((m (make-telemetry-metric
            :name (telemetry-instrument-name instrument)
            :value value
            :unit (telemetry-instrument-unit instrument)
            :attributes attributes
            :kind (telemetry-instrument-kind instrument)
            :boundaries (and (histogram-p instrument)
                             (histogram-boundaries instrument)))))
    (push m (otlp-pending-metrics backend))
    (when (>= (length (otlp-pending-metrics backend)) (otlp-max-batch backend))
      (flush-telemetry backend))
    m))

(defmethod flush-telemetry ((backend otlp-telemetry-backend) &key)
  (let ((spans (nreverse (otlp-pending-spans backend)))
        (metrics (nreverse (otlp-pending-metrics backend))))
    (setf (otlp-pending-spans backend) nil
          (otlp-pending-metrics backend) nil)
    (when spans
      (%export-traces backend spans))
    (when metrics
      (%export-metrics backend metrics)))
  backend)
