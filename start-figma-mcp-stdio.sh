#!/bin/bash
# Figma MCP Server
# Usage:
#   ./start-figma-mcp.sh              # stdio mode (for MCP clients)
#   ./start-figma-mcp.sh --port 8940  # HTTP mode (for web clients)

set -e

# Ensure system CA bundle is available for TLS (macOS ca-certs can be empty)
if [ -z "${SSL_CERT_FILE:-}" ] && [ -f "/etc/ssl/cert.pem" ]; then
  export SSL_CERT_FILE="/etc/ssl/cert.pem"
fi

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

# Resolve executable path
# Priority: 1. Release binary  2. Workspace build  3. Local build  4. Installed  5. Auto-download
RELEASE_BINARY="$SCRIPT_DIR/figma-mcp-macos-arm64"

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

# 1. Pre-downloaded release binary (HTTP mode only - release is HTTP server)
if [ "$HTTP_MODE" = true ] && [ -x "$RELEASE_BINARY" ]; then
  FIGMA_EXE="$RELEASE_BINARY"
# 2. Workspace build
elif [ -x "$WORKSPACE_EXE" ]; then
  FIGMA_EXE="$WORKSPACE_EXE"
# 3. Local build
elif [ -x "$LOCAL_EXE" ]; then
  FIGMA_EXE="$LOCAL_EXE"
# 4. System-installed (stdio only)
elif [ -n "$INSTALLED_EXE" ] && [ "$HTTP_MODE" = false ]; then
  FIGMA_EXE="$INSTALLED_EXE"
fi

# 5. Auto-download from GitHub releases if nothing found (HTTP mode only)
if [ -z "$FIGMA_EXE" ] && [ "$HTTP_MODE" = true ]; then
  echo "No binary found. Downloading from GitHub releases..." >&2
  RELEASE_URL="https://github.com/jeong-sik/figma-mcp/releases/latest/download/figma-mcp-macos-arm64"
  if curl -fsSL -o "$RELEASE_BINARY" "$RELEASE_URL" 2>/dev/null; then
    chmod +x "$RELEASE_BINARY"
    FIGMA_EXE="$RELEASE_BINARY"
    echo "Downloaded: $RELEASE_BINARY" >&2
  fi
fi

# Fallback: build from source
if [ -z "$FIGMA_EXE" ]; then
  echo "Building figma-mcp (bin/$BINARY_NAME)..." >&2
  if ! command -v dune >/dev/null 2>&1; then
    echo "Error: dune not found. Install dune or download binary manually." >&2
    exit 1
  fi
  dune build "./bin/$BINARY_NAME" >&2

  if [ -x "$WORKSPACE_EXE" ]; then
    FIGMA_EXE="$WORKSPACE_EXE"
  elif [ -x "$LOCAL_EXE" ]; then
    FIGMA_EXE="$LOCAL_EXE"
  else
    echo "Error: build failed." >&2
    exit 1
  fi
fi

exec "$FIGMA_EXE" "$@"
