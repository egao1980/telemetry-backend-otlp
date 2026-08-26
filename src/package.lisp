(defpackage #:telemetry-backend-otlp
  (:use #:cl #:telemetry-protocol)
  (:nicknames #:stack-telemetry-otlp)
  (:export
   #:otlp-telemetry-backend
   #:make-otlp-telemetry-backend
   #:use-otlp-telemetry
   #:otlp-endpoint
   #:otlp-service-name
   #:+default-otlp-endpoint+))

(in-package #:telemetry-backend-otlp)
