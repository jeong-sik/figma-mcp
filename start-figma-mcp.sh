#!/bin/bash
# Figma MCP Server (HTTP) - Start Script
# Usage: ./start-figma-mcp-http.sh [--port PORT] [--grpc-port PORT]

set -e

# Try to raise the file descriptor limit to avoid EMFILE accept crashes.
# This must never be fatal under launchd.
ULIMIT_NOFILE="${FIGMA_ULIMIT_NOFILE:-65536}"
ulimit -n "$ULIMIT_NOFILE" >/dev/null 2>&1 || true

# Load FIGMA_TOKEN from Keychain if not already set
if [ -z "$FIGMA_TOKEN" ]; then
  FIGMA_TOKEN=$(security find-generic-password -s "figma-mcp" -a "FIGMA_TOKEN" -w 2>/dev/null || true)
  if [ -n "$FIGMA_TOKEN" ]; then
    export FIGMA_TOKEN
  fi
fi

# Ensure OCaml environment
if command -v opam >/dev/null 2>&1; then
  eval "$(opam env 2>/dev/null)" >/dev/null 2>/dev/null || true
elif [ -f "/opt/homebrew/bin/opam" ]; then
  eval "$(/opt/homebrew/bin/opam env 2>/dev/null)" >/dev/null 2>/dev/null || true
elif [ -f "$HOME/.opam/opam-init/init.sh" ]; then
  . "$HOME/.opam/opam-init/init.sh" >/dev/null 2>/dev/null || true
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

PORT="${FIGMA_MCP_PORT:-8940}"
GRPC_PORT="${FIGMA_MCP_GRPC_PORT:-}"

EXPECTED_VERSION=""
if [ -f "$SCRIPT_DIR/dune-project" ]; then
  EXPECTED_VERSION=$(awk '/^[[:space:]]*\\(version[[:space:]]+/ { gsub(/[()]/, "", $2); print $2; exit }' "$SCRIPT_DIR/dune-project")
fi

get_binary_version() {
  local bin="$1"
  if [ -x "$bin" ]; then
    "$bin" --version 2>/dev/null | awk '{print $NF}' | tr -d '\r'
  fi
}

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --port)
      PORT="$2"
      shift 2
      ;;
    --grpc-port)
      GRPC_PORT="$2"
      shift 2
      ;;
    --grpc-port=*)
      GRPC_PORT="${1#*=}"
      shift 1
      ;;
    -h|--help)
      echo "Usage: $0 [--port PORT] [--grpc-port PORT]"
      echo "  --port PORT  Server port (default: 8940)"
      echo "  --grpc-port PORT  gRPC port (optional, enables streaming)"
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      exit 1
      ;;
  esac
done

# Resolve executable path (prefer workspace build dir)
WORKSPACE_EXE="$SCRIPT_DIR/../_build/default/figma-mcp/bin/main.exe"
LOCAL_EXE="$SCRIPT_DIR/_build/default/bin/main.exe"
INSTALLED_EXE="$(command -v figma-mcp || true)"
FIGMA_EXE=""

if [ -n "${FIGMA_MCP_EXE:-}" ] && [ -x "$FIGMA_MCP_EXE" ]; then
  FIGMA_EXE="$FIGMA_MCP_EXE"
elif [ -x "$WORKSPACE_EXE" ]; then
  FIGMA_EXE="$WORKSPACE_EXE"
elif [ -x "$LOCAL_EXE" ]; then
  FIGMA_EXE="$LOCAL_EXE"
elif [ -n "$INSTALLED_EXE" ]; then
  FIGMA_EXE="$INSTALLED_EXE"
fi

if [ -n "$FIGMA_EXE" ] && [ -n "$EXPECTED_VERSION" ]; then
  BIN_VERSION="$(get_binary_version "$FIGMA_EXE")"
  if [ -n "$BIN_VERSION" ] && [ "$BIN_VERSION" != "$EXPECTED_VERSION" ]; then
    if command -v dune >/dev/null 2>&1; then
      echo "Binary version $BIN_VERSION != expected $EXPECTED_VERSION. Rebuilding..." >&2
      FIGMA_EXE=""
    else
      echo "Warning: binary version $BIN_VERSION != expected $EXPECTED_VERSION (dune not found; using existing binary)" >&2
    fi
  fi
fi

# Build if needed (requires dune on PATH)
if [ -z "$FIGMA_EXE" ]; then
  echo "Building figma-mcp (bin/main.exe only)..." >&2
  if ! command -v dune >/dev/null 2>&1; then
    echo "Error: dune not found. Install dune or build figma-mcp binary first." >&2
    exit 1
  fi
  dune build ./bin/main.exe >&2

  if [ -x "$WORKSPACE_EXE" ]; then
    FIGMA_EXE="$WORKSPACE_EXE"
  elif [ -x "$LOCAL_EXE" ]; then
    FIGMA_EXE="$LOCAL_EXE"
  elif [ -n "$INSTALLED_EXE" ]; then
    FIGMA_EXE="$INSTALLED_EXE"
  else
    echo "Error: build succeeded but figma-mcp executable not found." >&2
    exit 1
  fi
fi

if [ -n "$GRPC_PORT" ]; then
  echo "Using figma-mcp binary: $FIGMA_EXE" >&2
  exec "$FIGMA_EXE" --port "$PORT" --grpc-port "$GRPC_PORT"
else
  echo "Using figma-mcp binary: $FIGMA_EXE" >&2
  exec "$FIGMA_EXE" --port "$PORT"
fi
