#!/bin/bash
# Integration tests for Figma MCP endpoints
set -e

BASE_URL="${FIGMA_MCP_URL:-http://127.0.0.1:8940}"
PASS=0
FAIL=0

test_endpoint() {
    local name="$1"
    local endpoint="$2"
    local data="$3"
    local expected="$4"

    response=$(curl -s -X POST "$BASE_URL$endpoint" \
        -H "Content-Type: application/json" \
        -d "$data" 2>&1)

    if echo "$response" | grep -q "$expected"; then
        echo "✓ $name"
        ((PASS++))
    else
        echo "✗ $name"
        echo "  Expected: $expected"
        echo "  Got: ${response:0:200}..."
        ((FAIL++))
    fi
}

echo "=== Figma MCP Integration Tests ==="
echo "Server: $BASE_URL"
echo ""

# Health check
echo "--- Health Check ---"
if curl -s "$BASE_URL/health" | grep -q '"status":"ok"'; then
    echo "✓ Health check"
    ((PASS++))
else
    echo "✗ Health check - server not running?"
    exit 1
fi

# Animation Tests
echo ""
echo "--- Animation Extraction Tests ---"

# Test DISSOLVE animation
test_endpoint "Animation: DISSOLVE type" \
    "/plugin/extract-animations" \
    '{"node":{"name":"Fade","type":"FRAME","opacity":0.5},"outputType":"css"}' \
    "DISSOLVE"

# Test PUSH animation
test_endpoint "Animation: PUSH transition" \
    "/plugin/extract-animations" \
    '{"node":{"name":"Modal","type":"FRAME","reactions":[{"trigger":{"type":"ON_CLICK"},"action":{"type":"NODE","transition":{"type":"PUSH","duration":0.3}}}]},"outputType":"css"}' \
    "scale(0.8)"

# Test SwiftUI output
test_endpoint "Animation: SwiftUI output" \
    "/plugin/extract-animations" \
    '{"node":{"name":"Button","type":"FRAME"},"outputType":"both"}' \
    ".animation"

# Accessibility Tests
echo ""
echo "--- Accessibility Tests ---"

test_endpoint "A11y: Button role" \
    "/plugin/accessibility" \
    '{"node":{"name":"Submit Button","type":"FRAME"}}' \
    "button"

test_endpoint "A11y: Image alt text" \
    "/plugin/accessibility" \
    '{"node":{"name":"Profile Photo","type":"FRAME"}}' \
    "img"

# Variant Tests
echo ""
echo "--- Variant Extraction Tests ---"

test_endpoint "Variants: COMPONENT_SET" \
    "/plugin/extract-variants" \
    '{"node":{"name":"Button","type":"COMPONENT_SET","children":[{"name":"State=Default, Size=Small"},{"name":"State=Hover, Size=Large"}]}}' \
    "State"

# Responsive Tests
echo ""
echo "--- Responsive Breakpoints Tests ---"

test_endpoint "Responsive: Mobile breakpoint" \
    "/plugin/responsive-breakpoints" \
    '{"node":{"name":"Card","width":320,"height":200}}' \
    "mobile"

# Webhook Tests
echo ""
echo "--- Webhook Tests ---"

test_endpoint "Webhook: FILE_UPDATE" \
    "/webhook/figma" \
    '{"event_type":"FILE_UPDATE","file_key":"abc123","file_name":"Design"}' \
    "received"

# Summary
echo ""
echo "=== Test Summary ==="
echo "Passed: $PASS"
echo "Failed: $FAIL"
echo ""

if [ $FAIL -gt 0 ]; then
    echo "Some tests failed!"
    exit 1
else
    echo "All tests passed!"
fi
