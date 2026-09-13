(in-package #:telemetry-backend-otlp/tests)

(defun %ht-get (obj &rest keys)
  (let ((cur obj))
    (dolist (k keys cur)
      (setf cur (and (hash-table-p cur) (gethash k cur))))))

(defun %capture-fn (box)
  (lambda (method url &key headers content)
    (setf (car box)
          (list method url headers (stack-json:decode content)))
    (values 200 "{}")))

(deftest otlp-flush-posts-resource-spans
  (let* ((box (list nil))
         (b (telemetry-backend-otlp:make-otlp-telemetry-backend
             :endpoint "http://collector.invalid"
             :service-name "unit-test"
             :request-fn (%capture-fn box))))
    (let ((stack-telemetry:*telemetry-backend* b))
      (stack-telemetry:with-span ("invoke_agent"
                                  :attributes (list stack-telemetry:+gen-ai-agent-name+
                                                    "researcher"))
        (stack-telemetry:with-span ("chat" :kind :client
                                    :attributes (list stack-telemetry:+gen-ai-request-model+
                                                      "mock"))
          (stack-telemetry:set-span-attribute
           b stack-telemetry:*current-span*
           stack-telemetry:+gen-ai-usage-output-tokens+ 32)
          (stack-telemetry:add-span-event b stack-telemetry:*current-span*
                                          "first-token"))))
    (ok (null (car box)))
    (stack-telemetry:flush-telemetry b)
    (destructuring-bind (method url headers body) (car box)
      (ok (eq :post method))
      (ok (equal "http://collector.invalid/v1/traces" url))
      (ok (equal "application/json" (cdr (assoc "content-type" headers :test #'equal))))
      (let* ((rs (elt (%ht-get body "resourceSpans") 0))
             (spans (%ht-get (elt (%ht-get rs "scopeSpans") 0) "spans"))
             (chat (find "chat" spans :key (lambda (s) (gethash "name" s))
                         :test #'equal))
             (agent (find "invoke_agent" spans :key (lambda (s) (gethash "name" s))
                          :test #'equal)))
        (ok (equal "unit-test"
                   (%ht-get (elt (%ht-get rs "resource" "attributes") 0)
                            "value" "stringValue")))
        (ok chat)
        (ok agent)
        (ok (= 3 (gethash "kind" chat)))
        (ok (equal (gethash "traceId" chat) (gethash "traceId" agent)))
        (ok (equal (gethash "spanId" agent) (gethash "parentSpanId" chat)))
        (ok (= 32 (length (gethash "traceId" chat))))
        (ok (= 16 (length (gethash "spanId" chat))))
        (ok (stringp (gethash "startTimeUnixNano" chat)))
        (ok (stringp (gethash "endTimeUnixNano" chat)))
        (ok (= 1 (%ht-get chat "status" "code")))
        (let ((tok (find "gen_ai.usage.output_tokens"
                         (gethash "attributes" chat)
                         :key (lambda (a) (gethash "key" a))
                         :test #'equal)))
          (ok (equal "32" (%ht-get tok "value" "intValue"))))
        (ok (equal "first-token"
                   (gethash "name" (elt (gethash "events" chat) 0))))))))

(deftest otlp-max-batch-auto-flush
  (let* ((box (list nil))
         (b (telemetry-backend-otlp:make-otlp-telemetry-backend
             :endpoint "http://collector.invalid/v1/traces"
             :max-batch 1
             :request-fn (%capture-fn box))))
    (let ((stack-telemetry:*telemetry-backend* b))
      (stack-telemetry:with-span ("solo") nil))
    (ok (car box))
    (ok (equal "http://collector.invalid/v1/traces" (second (car box))))
    (ok (null (telemetry-backend-otlp::otlp-pending-spans b)))))

(deftest otlp-flush-empty-does-not-post
  (let* ((called nil)
         (b (telemetry-backend-otlp:make-otlp-telemetry-backend
             :request-fn (lambda (&rest _)
                           (declare (ignore _))
                           (setf called t)
                           (values 200 "{}")))))
    (stack-telemetry:flush-telemetry b)
    (ok (not called))))

(deftest otlp-export-error-on-http
  (let ((b (telemetry-backend-otlp:make-otlp-telemetry-backend
            :max-batch 1
            :request-fn (lambda (&rest _)
                          (declare (ignore _))
                          (values 503 "nope")))))
    (ok (signals
            (let ((stack-telemetry:*telemetry-backend* b))
              (stack-telemetry:with-span ("x") nil))
          'stack-telemetry:telemetry-error))))

(deftest otlp-extra-headers
  (let* ((box (list nil))
         (b (telemetry-backend-otlp:make-otlp-telemetry-backend
             :headers '(("authorization" . "Bearer z"))
             :max-batch 1
             :request-fn (%capture-fn box))))
    (let ((stack-telemetry:*telemetry-backend* b))
      (stack-telemetry:with-span ("h") nil))
    (ok (equal "Bearer z"
               (cdr (assoc "authorization" (third (car box)) :test #'equal))))))

(deftest otlp-metrics-payload-shape
  (let* ((b (telemetry-backend-otlp:make-otlp-telemetry-backend
             :service-name "unit-test"))
         (counter (stack-telemetry:make-telemetry-metric
                   :name "hits" :value 3 :unit "1" :kind :counter
                   :attributes (list "route" "/")))
         (hist (stack-telemetry:make-telemetry-metric
                :name "latency" :value 0.7 :unit "s" :kind :histogram
                :boundaries '(0.5 1.0 5.0)))
         (payload (telemetry-backend-otlp:encode-otlp-metrics
                   b (list counter hist)))
         (rm (elt (%ht-get payload "resourceMetrics") 0))
         (sm (elt (%ht-get rm "scopeMetrics") 0))
         (metrics (%ht-get sm "metrics"))
         (hits (find "hits" metrics :key (lambda (m) (gethash "name" m))
                     :test #'equal))
         (lat (find "latency" metrics :key (lambda (m) (gethash "name" m))
                    :test #'equal)))
    (ok (hash-table-p payload))
    (ok rm)
    (ok sm)
    (ok hits)
    (ok lat)
    (ok (equal "1" (gethash "unit" hits)))
    (ok (gethash "sum" hits))
    (ok (eq t (%ht-get hits "sum" "isMonotonic")))
    (ok (equal "3" (%ht-get (elt (%ht-get hits "sum" "dataPoints") 0)
                            "asInt")))
    (ok (gethash "histogram" lat))
    (ok (equalp #(0.5 1.0 5.0)
                (%ht-get (elt (%ht-get lat "histogram" "dataPoints") 0)
                         "explicitBounds")))))

(deftest otlp-metrics-flush-posts-resource-metrics
  (let* ((box (list nil))
         (b (telemetry-backend-otlp:make-otlp-telemetry-backend
             :endpoint "http://collector.invalid"
             :service-name "unit-test"
             :request-fn (%capture-fn box))))
    (stack-telemetry:record-metric b "hits" 3 :unit "1")
    (ok (null (car box)))
    (stack-telemetry:flush-telemetry b)
    (destructuring-bind (method url headers body) (car box)
      (ok (eq :post method))
      (ok (equal "http://collector.invalid/v1/metrics" url))
      (ok (equal "application/json" (cdr (assoc "content-type" headers :test #'equal))))
      (let* ((rm (elt (%ht-get body "resourceMetrics") 0))
             (metrics (%ht-get (elt (%ht-get rm "scopeMetrics") 0) "metrics"))
             (hits (find "hits" metrics :key (lambda (m) (gethash "name" m))
                         :test #'equal)))
        (ok (equal "unit-test"
                   (%ht-get (elt (%ht-get rm "resource" "attributes") 0)
                            "value" "stringValue")))
        (ok hits)
        (ok (gethash "sum" hits))))))
