(defsystem "telemetry-backend-otlp"
  :version "0.2.0"
  :description "OTLP/HTTP JSON exporter for telemetry-protocol (traces + metrics)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("telemetry-protocol" "http-protocol" "json-protocol" "json-backend-jzon" "babel")
  :properties (:cl-repo (:ci (:with ("dissect" "log-protocol"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "telemetry-backend-otlp/tests"))))

(defsystem "telemetry-backend-otlp/logs"
  :version "0.2.0"
  :description "OTLP/HTTP JSON log-protocol backend"
  :author "egao1980"
  :license "MIT"
  :depends-on ("telemetry-backend-otlp" "log-protocol")
  :pathname "src/logs"
  :components ((:file "backend")))

(defsystem "telemetry-backend-otlp/tests"
  :depends-on ("telemetry-backend-otlp" "telemetry-backend-otlp/logs" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test")
               (:file "logs-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
