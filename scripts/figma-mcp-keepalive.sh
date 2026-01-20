#!/bin/bash
# Keep figma-mcp running with automatic restart and logs.

set -euo pipefail

ME_ROOT="${ME_ROOT:-$HOME/me}"
PORT="${FIGMA_MCP_PORT:-8940}"
GRPC_PORT="${FIGMA_MCP_GRPC_PORT:-50052}"
RESTART_DELAY="${FIGMA_MCP_RESTART_DELAY:-2}"
LOG_DIR="${FIGMA_MCP_LOG_DIR:-$ME_ROOT/logs}"
LOG_FILE="${FIGMA_MCP_LOG_FILE:-$LOG_DIR/figma-mcp-${PORT}.log}"
PID_FILE="${FIGMA_MCP_PID_FILE:-/tmp/figma-mcp-${PORT}.pid}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
START_SCRIPT="$SCRIPT_DIR/../start-figma-mcp-http.sh"

if [ -f "$PID_FILE" ]; then
  existing_pid="$(cat "$PID_FILE" 2>/dev/null || true)"
  if [ -n "$existing_pid" ] && kill -0 "$existing_pid" 2>/dev/null; then
    echo "figma-mcp already running (pid=$existing_pid)" >&2
    exit 1
  fi
fi

mkdir -p "$LOG_DIR"
echo $$ > "$PID_FILE"

child_pid=""
cleanup() {
  if [ -n "$child_pid" ]; then
    kill -TERM "$child_pid" 2>/dev/null || true
  fi
  rm -f "$PID_FILE"
}
trap cleanup INT TERM EXIT

while true; do
  echo "[$(date -u +"%Y-%m-%dT%H:%M:%SZ")] starting figma-mcp..." >> "$LOG_FILE"
  args=(--port "$PORT")
  if [ -n "$GRPC_PORT" ]; then
    args+=(--grpc-port "$GRPC_PORT")
  fi
  "$START_SCRIPT" "${args[@]}" >> "$LOG_FILE" 2>&1 &
  child_pid=$!
  if wait "$child_pid"; then
    exit_code=0
  else
    exit_code=$?
  fi
  echo "[$(date -u +"%Y-%m-%dT%H:%M:%SZ")] exited (code=$exit_code)" >> "$LOG_FILE"
  sleep "$RESTART_DELAY"
done
