#!/usr/bin/env bash
set -eu

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
  echo "usage: $0 <template.plist> [output.plist]" >&2
  exit 1
fi

template="$1"
output="${2:-${template%.plist}.generated.plist}"
repo_root="$(cd "$(dirname "$0")/.." && pwd)"
python3_bin="${PYTHON3_BIN:-$(command -v python3)}"

if [ ! -f "$template" ]; then
  echo "template not found: $template" >&2
  exit 1
fi

sed \
  -e "s|__FIGMA_MCP_ROOT__|$repo_root|g" \
  -e "s|__PYTHON3__|$python3_bin|g" \
  "$template" > "$output"

echo "rendered: $output"
