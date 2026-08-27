# telemetry-backend-otlp

[OTLP/HTTP](https://opentelemetry.io/docs/specs/otlp/#otlphttp) JSON exporter for [`telemetry-protocol`](https://github.com/egao1980/telemetry-protocol). Not the protocol — product backends stay in their own repos.

`POST {endpoint}/v1/traces` (`application/json`). Default endpoint `http://127.0.0.1:4318`. gRPC/protobuf is a later backend if ever.

Transport is `http-protocol` — bind [`http-backend-async`](https://github.com/egao1980/http-backend-async) × [`event-backend-libuv`](https://github.com/egao1980/event-backend-libuv). Dexador is maintenance.

```lisp
(asdf:load-system "telemetry-backend-otlp")
(asdf:load-system "event-backend-libuv")
(asdf:load-system "http-backend-async")

(setf http-backend-async:*event-backend-maker*
      #'event-backend-libuv:make-libuv-backend)
(setf http-protocol:*http-backend*
      (http-backend-async:make-async-backend))

(stack-telemetry-otlp:use-otlp-telemetry :service-name "demo")
(stack-telemetry:with-span ("chat" :kind :client) nil)
(stack-telemetry:flush-telemetry stack-telemetry:*telemetry-backend*)
```

Env (when initargs omitted): `OTEL_EXPORTER_OTLP_TRACES_ENDPOINT` / `OTEL_EXPORTER_OTLP_ENDPOINT` / `OTEL_SERVICE_NAME` / `OTEL_EXPORTER_OTLP_HEADERS` (`k=v,k2=v2`).

Ended spans buffer until `flush-telemetry` or `max-batch` (default 64). Inject `request-fn` in tests.

CI needs published `telemetry-protocol` on GHCR.

Part of [cl-stack](https://github.com/egao1980/cl-stack).

## License

MIT — see [LICENSE](LICENSE).
