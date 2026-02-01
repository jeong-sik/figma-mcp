#!/bin/bash
# MCP HTTP smoke test (Figma-MCP)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

LOG_DIR="${LOG_DIR:-$ROOT_DIR/logs}"
mkdir -p "$LOG_DIR"

TS="$(date +%Y%m%d_%H%M%S)"
SMOKE_LOG="$LOG_DIR/figma-mcp-smoke-$TS.log"
SERVER_LOG="$LOG_DIR/figma-mcp-smoke-server-$TS.log"

PORT="${PORT:-8940}"
URL="http://127.0.0.1:${PORT}/mcp"
HEALTH_URL="http://127.0.0.1:${PORT}/health"

if lsof -nP -iTCP:"$PORT" -sTCP:LISTEN >/dev/null 2>&1; then
  echo "Port $PORT is already in use; aborting." | tee -a "$SMOKE_LOG" >&2
  exit 1
fi

cleanup() {
  if [ -n "${PID:-}" ] && kill -0 "$PID" >/dev/null 2>&1; then
    kill "$PID" >/dev/null 2>&1 || true
    sleep 0.5
    kill -9 "$PID" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

echo "[figma-mcp] starting on :$PORT" >> "$SMOKE_LOG"
"$ROOT_DIR/start-figma-mcp-http.sh" --port "$PORT" >"$SERVER_LOG" 2>&1 &
PID=$!

ok=0
for _ in {1..30}; do
  if curl -fsS "$HEALTH_URL" >/dev/null 2>&1; then
    ok=1
    break
  fi
  sleep 0.2
done
if [ "$ok" -ne 1 ]; then
  echo "Health check failed." | tee -a "$SMOKE_LOG" >&2
  tail -n 20 "$SERVER_LOG" >> "$SMOKE_LOG" 2>/dev/null || true
  exit 1
fi

fetch_headers() {
  curl -sS -m 3 -D - -o /dev/null "$@" 2>/dev/null | tr -d '\r'
}

ct_of() {
  echo "$1" | awk -F': ' 'BEGIN{IGNORECASE=1} tolower($1)=="content-type"{print $2}' | head -n 1
}

status_of() {
  echo "$1" | head -n 1 | awk '{print $2}'
}

echo "[figma-mcp] POST /mcp Accept */* -> JSON" >> "$SMOKE_LOG"
HDR_JSON=$(fetch_headers -X POST "$URL" \
  -H 'Content-Type: application/json' \
  -H 'Accept: */*' \
  --data '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}')
STATUS_JSON="$(status_of "$HDR_JSON")"
CT_JSON="$(ct_of "$HDR_JSON")"
if [ "$STATUS_JSON" != "200" ] || ! echo "$CT_JSON" | grep -qi "application/json"; then
  echo "JSON response check failed (status=$STATUS_JSON, ct=$CT_JSON)." | tee -a "$SMOKE_LOG" >&2
  exit 1
fi

echo "[figma-mcp] POST /mcp Accept text/event-stream -> SSE" >> "$SMOKE_LOG"
HDR_SSE=$(fetch_headers -X POST "$URL" \
  -H 'Content-Type: application/json' \
  -H 'Accept: text/event-stream' \
  --data '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}')
STATUS_SSE="$(status_of "$HDR_SSE")"
CT_SSE="$(ct_of "$HDR_SSE")"
if [ "$STATUS_SSE" != "200" ] || ! echo "$CT_SSE" | grep -qi "text/event-stream"; then
  echo "SSE response check failed (status=$STATUS_SSE, ct=$CT_SSE)." | tee -a "$SMOKE_LOG" >&2
  exit 1
fi

echo "[figma-mcp] OK" >> "$SMOKE_LOG"
