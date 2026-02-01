# Observability & Alerting (Figma MCP)

## Endpoints

- `GET /metrics` (Prometheus text format)
- `GET /stats` (JSON summary)

## Metrics Summary

### HTTP/SSE (`/metrics`)

- `mcp_http_requests_total{class="2xx|3xx|4xx|5xx"}`
- `mcp_http_inflight`
- `mcp_http_errors_total`
- `mcp_http_bytes_out_total`
- `mcp_http_rps_1m`, `mcp_http_rps_5m`
- `mcp_http_latency_ms{quantile="0.50|0.95|0.99"}`
- `mcp_http_latency_ms{stat="avg|min|max"}`
- `mcp_sse_open`, `mcp_sse_total`

### Agent Queue (`/stats`)

- `agent_queue.pending`, `agent_queue.claimed`, `agent_queue.completed`
- `agent_queue.failed`, `agent_queue.drifted`
- `agent_queue.oldest_pending_sec`, `agent_queue.oldest_claimed_sec`
- `agent_queue.claim_ttl_sec`, `agent_queue.heartbeat_ttl_sec`
- `agent_queue.max_age_sec`, `agent_queue.max_attempts`

## Alert Thresholds (Initial Proposal)

> 아래 기준은 **초기 제안**입니다. 실제 운영 값은 7일 이상 baseline 수집 후 조정합니다.

### HTTP Errors

- **Warn**: `5xx` 비율 > **1%** (5m window)
- **Critical**: `5xx` 비율 > **5%** (5m window)

- **Warn**: `mcp_http_errors_total` 증가율 > **1/min** (5m)
- **Critical**: 증가율 > **5/min** (5m)

### Latency

- **Warn**: `mcp_http_latency_ms{quantile="0.95"}` > **10,000 ms** (5m)
- **Critical**: `p95` > **30,000 ms** (5m)

### Inflight / SSE

- **Warn**: `mcp_http_inflight` > **50** for 5m
- **Critical**: `mcp_http_inflight` > **200** for 5m

- **Warn**: `mcp_sse_open` > **expected_clients + 5**
- **Critical**: `mcp_sse_open` > **expected_clients + 20**

### Agent Queue Health

- **Warn**: `oldest_pending_sec` > `claim_ttl_sec`
- **Critical**: `oldest_pending_sec` > `2 * claim_ttl_sec`

- **Warn**: `oldest_claimed_sec` > `2 * heartbeat_ttl_sec`
- **Critical**: `oldest_claimed_sec` > `3 * heartbeat_ttl_sec`

- **Warn**: `pending` > **10** for 5m
- **Critical**: `pending` > **50** for 5m

- **Warn**: `drifted` > **0**
- **Critical**: `drifted` >= **5**

## Tuning Rules

- low traffic 환경에서는 **rate 기반** 대신 **절대값 기준**을 우선 적용.
- `claim_ttl_sec`, `heartbeat_ttl_sec` 변경 시 관련 임계치도 동일 배수로 조정.
- 7일 이상 정상 트래픽 baseline 후:
  - latency: `p95`의 **2x**를 Warn, **4x**를 Critical로 설정
  - errors: `5xx` 비율의 **3x**를 Warn, **6x**를 Critical로 설정

