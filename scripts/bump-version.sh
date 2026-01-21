#!/bin/bash
# Usage: ./scripts/bump-version.sh 0.3.4

set -e

NEW_VERSION="${1:-}"
if [ -z "$NEW_VERSION" ]; then
  echo "Usage: $0 <new-version>"
  echo "Example: $0 0.3.4"
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

echo "🔄 Bumping version to $NEW_VERSION"

# 1. Update dune-project
sed -i '' "s/(version [^)]*)/(version $NEW_VERSION)/" "$ROOT_DIR/dune-project"
echo "✅ dune-project"

# 2. Update lib/version.ml
cat > "$ROOT_DIR/lib/version.ml" << VEOF
(** Version from dune-project *)
let version = "$NEW_VERSION"
VEOF
echo "✅ lib/version.ml"

# 3. Update opam file if exists
if [ -f "$ROOT_DIR/figma_mcp.opam" ]; then
  sed -i '' "s/^version: .*/version: \"$NEW_VERSION\"/" "$ROOT_DIR/figma_mcp.opam"
  echo "✅ figma_mcp.opam"
fi

echo ""
echo "📦 Version bumped to $NEW_VERSION"
echo "   Run: dune build && git add -A && git commit -m 'chore: bump to $NEW_VERSION'"
