#!/bin/bash
# Figma MCP Server
# Usage:
#   ./start-figma-mcp.sh              # stdio mode (for MCP clients)
#   ./start-figma-mcp.sh --port 8940  # HTTP mode (for web clients)

set -e

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

# Detect HTTP mode (--port or --http argument)
HTTP_MODE=false
for arg in "$@"; do
  if [[ "$arg" == "--port" ]] || [[ "$arg" == "--http" ]] || [[ "$arg" =~ ^--port= ]]; then
    HTTP_MODE=true
    break
  fi
done

# Select binary based on mode
if [ "$HTTP_MODE" = true ]; then
  # HTTP mode: use main.exe (supports --port)
  BINARY_NAME="main.exe"
  WORKSPACE_EXE="$SCRIPT_DIR/../_build/default/figma-mcp/bin/main.exe"
  LOCAL_EXE="$SCRIPT_DIR/_build/default/bin/main.exe"
else
  # stdio mode: use mcp_main.exe
  BINARY_NAME="mcp_main.exe"
  WORKSPACE_EXE="$SCRIPT_DIR/../_build/default/figma-mcp/bin/mcp_main.exe"
  LOCAL_EXE="$SCRIPT_DIR/_build/default/bin/mcp_main.exe"
fi

INSTALLED_EXE="$(command -v figma-mcp-server || true)"
FIGMA_EXE=""

if [ -x "$WORKSPACE_EXE" ]; then
  FIGMA_EXE="$WORKSPACE_EXE"
elif [ -x "$LOCAL_EXE" ]; then
  FIGMA_EXE="$LOCAL_EXE"
elif [ -n "$INSTALLED_EXE" ] && [ "$HTTP_MODE" = false ]; then
  FIGMA_EXE="$INSTALLED_EXE"
fi

# Build if needed (requires dune on PATH)
if [ -z "$FIGMA_EXE" ]; then
  echo "Building figma-mcp (bin/$BINARY_NAME)..." >&2
  if ! command -v dune >/dev/null 2>&1; then
    echo "Error: dune not found. Install dune or build figma-mcp binary first." >&2
    exit 1
  fi
  dune build "./bin/$BINARY_NAME" >&2

  if [ -x "$WORKSPACE_EXE" ]; then
    FIGMA_EXE="$WORKSPACE_EXE"
  elif [ -x "$LOCAL_EXE" ]; then
    FIGMA_EXE="$LOCAL_EXE"
  else
    echo "Error: build succeeded but $BINARY_NAME not found." >&2
    exit 1
  fi
fi

exec "$FIGMA_EXE" "$@"
