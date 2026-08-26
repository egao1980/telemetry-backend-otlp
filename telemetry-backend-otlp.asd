(defsystem "telemetry-backend-otlp"
  :version "0.1.0"
  :description "OTLP/HTTP JSON exporter for telemetry-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("telemetry-protocol" "http-protocol" "json-protocol" "json-backend-jzon" "babel")
  :properties
  (:cl-repo
   (:ci (:sources (("rove" :ql)))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "telemetry-backend-otlp/tests"))))

(defsystem "telemetry-backend-otlp/tests"
  :depends-on ("telemetry-backend-otlp" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
