# Figma MCP Server

[![Version](https://img.shields.io/badge/version-0.9.0-blue.svg)](https://github.com/jeong-sik/figma-mcp)
[![Coverage](https://img.shields.io/badge/coverage-85.96%25-brightgreen.svg)]()
[![OCaml](https://img.shields.io/badge/OCaml-5.x-orange.svg)](https://ocaml.org/)
[![MCP](https://img.shields.io/badge/MCP-2025--11--25-blue.svg)](https://spec.modelcontextprotocol.io/)
[![Status](https://img.shields.io/badge/status-Personal%20Project-lightgrey.svg)]()

OCaml 5.x native MCP server for Figma design-to-code conversion. Fidelity DSL output preserves layout, paint, border, and typography information.

Personal project.

## Quickstart

```bash
eval $(opam env)
opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git -y
opam install . --deps-only
dune build
export FIGMA_TOKEN="YOUR_TOKEN"
./start-figma-mcp-http.sh --port 8940
```

```json
{
  "mcpServers": {
    "figma": {
      "type": "http",
      "url": "http://127.0.0.1:8940/mcp"
    }
  }
}
```

## Features

- **MCP 2025-11-25 spec** -- JSON-RPC 2.0 over stdio, HTTP+SSE, gRPC
- **Fidelity DSL** -- structured JSON preserving layout/paint/border/typography
- **Type-safe parsing** -- OCaml variant/ADT-based Figma JSON parser
- **Native binary** -- single executable, no runtime dependency
- **Agent-first planning** -- `figma_get_planning_context` + `figma_validate_agent_plan` for structured task decomposition
- **Eval harness** -- CIEDE2000, SSIM, pass@k trajectory for automated quality measurement
- **Visual strict CI gate** -- pass/fail thresholds for design fidelity in CI pipelines
- **Category router** -- 6 domain categories (core/visual/export/components/code/team) with `mode=list|call|describe`
- **Request deduplication** -- Eio.Promise-based in-flight coalescing
- **Concurrency limiting** -- Eio.Semaphore-based Figma API rate limiting
- **Plugin bridge** -- bidirectional real-time sync with Figma Desktop plugin
- **gRPC streaming** -- 7 RPC methods for large file traversal
- **Multi-metric similarity** -- CIEDE2000 color + IoU layout + SSIM visual comparison
- **Visual verification** -- SSIM-based rendered HTML vs Figma comparison

## Capabilities

```
Capabilities: tools / resources / prompts
```

| Capability | Description |
|------------|-------------|
| **tools** | 61 internal tools. Exposed as 6 category routers + 15 featured = 21 items in `tools/list` |
| **resources** | `figma://docs/*` guides, `figma://tokens/{file_key}` dynamic tokens |
| **prompts** | `figma_fidelity_review` prompt |

### Resources

```
figma://docs/fidelity      # Fidelity DSL key reference
figma://docs/usage         # Recommended call patterns
figma://docs/tokens        # Design token/variable guide
figma://tokens/{file_key}  # Per-file design token (dynamic)
```

## Tool Overview

61 tools registered in `all_detailed_tools`. The `tools/list` endpoint exposes 21 items: 6 category routers + 15 featured tools.

### Category Routers

Each category router supports `mode=list|call|describe`:

| Category | Tools | Description |
|----------|-------|-------------|
| `figma_core` | 15 | File/node read, search, parse, tree |
| `figma_visual` | 5 | Verify, compare, fidelity loop |
| `figma_team` | 8 | Project/file listing, team crawl |
| `figma_export` | 4 | Image export, tokens, image fills |
| `figma_components` | 8 | Components, styles, variables, code connect |
| `figma_code` | 2 | Codegen, Code Connect |

### Featured Tools (direct access)

`figma_codegen`, `figma_doctor`, `figma_stats`, `figma_cache_stats`, `figma_cache_invalidate`, `figma_read_large_result`, `figma_code_connect`, `figma_post_comment`, `figma_get_file_comments`, `figma_plugin`, `figma_plugin_edit_node`, `figma_plugin_create_node`, `figma_plugin_delete_nodes`, `figma_plugin_batch`, `figma_plugin_subscribe_events`

`figma_doctor` now also checks Figma access state:
- no args: local runtime/dependency + token validity
- `file_key=...`: token validity + whether that file is actually accessible with the current token

### Frequently Used

- `figma_get_node_bundle` -- DSL + render + metadata + variables in one call
- `figma_fidelity_loop` -- iterative fidelity improvement loop
- `figma_verify_visual` -- HTML render/compare/adjust loop
- `figma_parse_url` -- extract file_key/node_id from Figma URL
- `figma_export_tokens` -- design token export (CSS/Tailwind/JSON)
- `figma_compare` -- multi-metric similarity measurement

### node_id Format

Figma URL uses `node-id=123-456`, API uses `123:456`. Use `figma_parse_url` to handle conversion.

## Installation

Requirements: OCaml >= 5.1, dune >= 3.15.

```bash
eval $(opam env)

# Pin external dependency (not in opam repository)
opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git -y

# Install dependencies
opam install . --deps-only

# Build
dune build
```

See `docs/INSTALL-CHECKLIST.md` for post-install verification.

### Token Setup

Startup scripts read `FIGMA_TOKEN` from macOS Keychain:

```bash
# Option 1: Environment variable
export FIGMA_TOKEN="YOUR_TOKEN"

# Option 2: Keychain (persistent)
security add-generic-password -s "figma-mcp" -a "FIGMA_TOKEN" -w "YOUR_TOKEN"
```

## Running

Three transport modes:

```bash
# stdio (Claude Code / MCP clients)
./start-figma-mcp.sh

# HTTP+SSE
./start-figma-mcp-http.sh --port 8940

# HTTP + gRPC (large file streaming)
./figma-mcp --port 8940 --grpc-port 50052

# gRPC only
./figma-mcp --grpc-port 50052
```

### Claude Code Configuration

`~/.mcp.json` or project `.mcp.json`:

```json
{
  "mcpServers": {
    "figma": {
      "command": "/path/to/figma-mcp/start-figma-mcp.sh",
      "args": []
    }
  }
}
```

### Request Size Limits

- Max request body: 50MB (default). Adjust with `FIGMA_MCP_MAX_BODY_BYTES`.
- Exceeding limit returns HTTP 413.

### TLS Troubleshooting

If `ca-certs: empty trust anchors` occurs when running the binary directly:

```bash
# macOS
export SSL_CERT_FILE="/etc/ssl/cert.pem"
# Linux
export SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"
```

Startup scripts (`start-figma-mcp*.sh`) auto-detect CA bundles. See `docs/INSTALL-MANUAL.md`.

## Plugin Bridge

Real-time bidirectional communication with Figma Desktop plugin.

```bash
# 1. Start HTTP server
./start-figma-mcp-http.sh --port 8940

# 2. Import plugin in Figma Desktop
#    Plugins -> Development -> Import plugin from manifest
#    Select: plugin/manifest.json

# 3. Connect in plugin UI -> copy Channel ID

# 4. Use channel via MCP
figma_plugin action=connect
figma_plugin_use_channel channel_id="ch-..."
```

Plugin adds REST/layout/style data that the Figma API alone does not provide.

HTTP endpoints: `POST /plugin/connect`, `POST /plugin/poll`, `POST /plugin/result`, `GET /plugin/status`.

For detailed setup, troubleshooting, and configuration: `docs/plugin-workflow.md`.

Supported actions for `figma_plugin` (authoritative list) are documented in `docs/figma_plugin_actions.md`.

## gRPC Streaming

Server streaming for large Figma files. Use when responses exceed typical HTTP payload sizes or when recursive traversal is needed.

| Method | Type | Description |
|--------|------|-------------|
| `GetNodeStream` | server streaming | Node data with optional recursive expansion |
| `FidelityLoop` | server streaming | Iterative fidelity improvement |
| `GetSplitStream` | server streaming | Split large node trees |
| `GetFileMeta` | unary | File metadata |
| `PlanTasks` | server streaming | Divide-and-conquer task planning |
| `GetPlanningContext` | unary | Agent-first planning context |
| `ValidateAgentPlan` | unary | Plan structure validation |

```bash
grpcurl -plaintext -import-path proto -proto figma.proto \
  -d '{"file_key":"...","node_id":"...","token":"..."}' \
  localhost:50052 figma.v1.FigmaService/GetNodeStream
```

Recursive options:
- `recursive_max_depth` (default 20, env: `FIGMA_RECURSIVE_MAX_DEPTH`)
- `recursive_max_nodes` (default 5000, env: `FIGMA_RECURSIVE_MAX_NODES`)
- `recursive_depth_per_call` (default 1, env: `FIGMA_RECURSIVE_DEPTH_PER_CALL`)

Protocol definition: `proto/figma.proto`.

## Multi-Metric Similarity

`figma_compare` measures design-to-code fidelity:

| Metric | Method | Source |
|--------|--------|--------|
| **Color** | CIEDE2000 (ΔE\*₀₀) | CIE standard |
| **Layout** | IoU / GIoU / DIoU | Rezatofighi 2019, Zheng 2020 |
| **Structure** | Tree Edit Distance | Zhang-Shasha 1989 |
| **Visual** | SSIM | Wang et al. 2004 |

Color comparison uses CIEDE2000 with JND threshold (ΔE\*₀₀ < 2.3 = indistinguishable). Layout comparison uses IoU, GIoU (non-overlap penalty), and DIoU (center distance penalty). SSIM compares Figma renders against HTML renders via ImageMagick. Experiment log in `docs/DISCOVERIES.md`.

### References

- [CIEDE2000](https://en.wikipedia.org/wiki/Color_difference#CIEDE2000) -- CIE standard
- [GIoU](https://arxiv.org/abs/1902.09630) -- CVPR 2019
- [DIoU](https://arxiv.org/abs/1911.08287) -- AAAI 2020
- [SSIM](https://ieeexplore.ieee.org/document/1284395) -- IEEE TIP 2004

## Fidelity DSL

`format: fidelity` produces structured JSON:

```json
{
  "meta": {"id": "1:2", "name": "Card", "type": "FRAME"},
  "geometry": {"absoluteBoundingBox": {"x": 0, "y": 0, "width": 320, "height": 200}},
  "layout": {"layoutMode": "VERTICAL", "paddingTop": 16, "itemSpacing": 12},
  "paint": {"fills": [...], "strokes": [...], "strokeWeight": 1},
  "text": {"characters": null, "style": null},
  "children": [...],
  "layout_missing": ["layoutWrap", "layoutAlign"]
}
```

### Image Download

`figma_export_image` and `figma_get_node_bundle` support `download: true` with optional `save_dir`. Default path fallback: `FIGMA_MCP_ASSET_DIR` env var, then `$ME_ROOT/workspace/yousleepwhen/figma-mcp/assets`, then `$HOME/.figma-mcp/assets`, then `/tmp/figma-mcp/assets`.

## Testing

Coverage: 85.96% (bisect_ppx, v0.9.0).

```bash
# Unit tests
dune runtest

# Codegen tests
dune exec ./test/test_codegen_p0.exe

# Benchmark
dune exec ./test/bench_p0.exe

# stdio smoke test
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | ./start-figma-mcp.sh
```

105 test files in `test/`.

## Environment Variables

Key variables (see source for full list of 42+ `FIGMA_MCP_*` vars):

| Variable | Default | Description |
|----------|---------|-------------|
| `FIGMA_TOKEN` | -- | Figma API token |
| `FIGMA_MCP_CACHE_TTL_HOURS` | 8 | Node cache TTL |
| `FIGMA_MCP_MAX_BODY_BYTES` | 50MB | Max request body |
| `FIGMA_MCP_FIGMA_API_TIMEOUT_SECONDS` | 30 | Figma API timeout |
| `FIGMA_MCP_CORS_PROFILE` | compat | `compat` or `strict` |
| `FIGMA_MCP_CORS_ALLOWED_ORIGINS` | -- | Comma-separated allowed origins |
| `FIGMA_MCP_MAX_INLINE_RESPONSE` | 50000 | Max inline response bytes |
| `FIGMA_MCP_LARGE_RESPONSE_DIR` | /tmp/figma_responses | Large response file path |
| `FIGMA_MCP_PLUGIN_POLL_MAX_MS` | 30000 | Plugin long-poll max wait |
| `MCP_API_KEY` | -- | HTTP auth key |

## Dependencies

```
ocaml (>= 5.1), dune (>= 3.15), grpc-direct (>= 0.1.0),
yojson (>= 2.0), uri (>= 4.2), cmdliner (>= 1.1),
ppx_deriving_yojson (>= 3.6), eio (>= 1.0), eio_main (>= 1.0),
httpun-eio (>= 0.1), gluten-eio (>= 0.4), cohttp-eio (>= 6.0),
tls-eio (>= 1.0), ca-certs (>= 0.2), bigstringaf (>= 0.9),
mirage-crypto-rng (>= 1.0), ipaddr (>= 5.0)
```

Test: `alcotest (>= 1.8.0)`, `bisect_ppx (>= 2.8)`.

## Project Structure

```
lib/           73 .ml + .mli files (core logic)
test/          105 .ml test files
plugin/        Figma Desktop plugin (code.js, manifest.json, ui.html)
proto/         gRPC service definition (figma.proto)
scripts/       Automation (render, SSIM compare, smoke test)
docs/          13 documentation files
```

## Documentation

| Document | Content |
|----------|---------|
| `docs/RECIPES.md` | End-to-end usage patterns |
| `docs/SETUP.md` | Install/run/integration summary |
| `docs/INSTALL-CHECKLIST.md` | Post-install verification |
| `docs/INSTALL-MANUAL.md` | Detailed installation guide |
| `docs/MCP-TEMPLATE.md` | ~/.mcp.json templates |
| `docs/CODE-CONNECT.md` | Component mapping spec |
| `docs/DESIGN-PRINCIPLES.md` | Architecture and design philosophy |
| `docs/OBSERVABILITY.md` | /metrics, /stats, alerting |
| `docs/PROTOCOL-2025-11-25.md` | MCP protocol compliance |
| `docs/LARGE-RESPONSE-ARCHITECTURE.md` | Large response handling |
| `docs/plugin-workflow.md` | Plugin setup and workflow |
| `docs/SSIM-HEARTBEAT.md` | SSIM test heartbeat mechanism |
| `docs/DISCOVERIES.md` | Experimental findings |

## Changelog

See `CHANGELOG.md`.

## License

MIT
