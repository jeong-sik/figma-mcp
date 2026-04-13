#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"
export DUNE_ROOT="$REPO_ROOT"

echo "=== Visual Strict Gate ==="
echo "Mode: Fail-Closed (any failure = gate REJECT)"

PASS=0
FAIL=0
RESULTS=()

has_named_test() {
  rg -q "\\(name[[:space:]]+$1\\)" test . 2>/dev/null
}

run_gate() {
  local name="$1"
  shift
  echo ""
  echo "--- Gate: $name ---"
  if "$@" 2>&1; then
    echo "  PASS $name PASSED"
    PASS=$((PASS + 1))
    RESULTS+=("PASS: $name")
  else
    echo "  FAIL $name FAILED"
    FAIL=$((FAIL + 1))
    RESULTS+=("FAIL: $name")
  fi
}

# Gate 1: Build succeeds
run_gate "Build" opam exec -- dune build

# Gate 2: CIEDE2000 tests pass
if has_named_test "test_ciede2000"; then
  run_gate "CIEDE2000 Tests" opam exec -- dune exec test/test_ciede2000.exe
fi

# Gate 3: SSIM tests pass (if exists)
if has_named_test "test_ssim"; then
  run_gate "SSIM Tests" opam exec -- dune exec test/test_ssim.exe
fi

# Gate 4: Color convert tests pass (if exists)
if has_named_test "test_color_convert"; then
  run_gate "Color Convert Tests" opam exec -- dune exec test/test_color_convert.exe
fi

# Gate 5: Full test suite
run_gate "Full Test Suite" opam exec -- dune test

echo ""
echo "=== Gate Summary ==="
for r in "${RESULTS[@]}"; do
  echo "  $r"
done
echo ""
echo "Total: $PASS passed, $FAIL failed"

if [ "$FAIL" -gt 0 ]; then
  echo ""
  echo "GATE DECISION: REJECT (${FAIL} failure(s))"
  exit 1
fi

echo ""
echo "GATE DECISION: PASS"
exit 0
