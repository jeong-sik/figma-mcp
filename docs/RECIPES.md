# Figma MCP Recipes

Practical end-to-end flows for common tasks. Each recipe assumes:
- `FIGMA_TOKEN` is available (env or Keychain via start scripts)
- HTTP MCP is running (default `http://localhost:8940/mcp`)
- A plugin channel is optional unless stated

## 0) Plugin Bridge Setup (Once)

1) Start the HTTP server.
```
./start-figma-mcp-http.sh --port 8940
```
Optional (enable gRPC streaming too):
```
./start-figma-mcp-http.sh --port 8940 --grpc-port 50052
```
Quick check:
```
curl http://localhost:8940/health
```

2) Import the development plugin and connect.
- Figma → Plugins → Development → Import plugin from manifest
- Set Server URL to `http://localhost:8940`
- Click Connect and keep the plugin window open

3) Copy the channel ID (`ch-...`).

## 1) Quickstart: Single Node Snapshot

1) Parse the URL to extract `file_key` and `node_id`.
```
figma_parse_url
  url: "https://www.figma.com/file/KEY/NAME?node-id=123-456"
```

2) Fetch a single node with image (low friction).
```
figma_get_node_with_image
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  format: "fidelity"
  image_format: "png"
  scale: 2
```

If you get `status: large_result`, use the progressive recipes below.

## 1.5) Smart Selection (Layout-first)

Score and select meaningful parent nodes before pulling heavy DSL or plugin data.
```
figma_select_nodes
  url: "https://www.figma.com/file/KEY/NAME?node-id=123-456"
  token: "$FIGMA_TOKEN"
  summary_depth: 1
  layout_only: true
  text_mode: "exclude"
  score_threshold: 2.0
  max_parents: 8
```

Optional filters:
```
figma_select_nodes
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  exclude_patterns: ["guide", "spec", "annotation", "순서도", "주석"]
  note_patterns: ["note", "memo", "설명", "참고"]
  notes_limit: 50
```

## 2) High-Fidelity Bundle (Recommended)

Use when you need layout + variables + image fills in one shot.
```
figma_get_node_bundle
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  include_meta: true
  include_variables: true
  include_image_fills: true
```

If a plugin channel is available:
```
figma_get_node_bundle
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  include_plugin: true
  include_plugin_variables: true
  plugin_channel_id: "ch-..."
  plugin_depth: 0
  plugin_include_geometry: false
  plugin_timeout_ms: 20000
```

### 2.1) Plugin Precision (Text Segments & Line Bounds)

1) Select the exact frame/layer in Figma.
```
figma_plugin_read_selection
  channel_id: "ch-..."
  depth: 1
  timeout_ms: 20000
```

2) For targeted nodes (small scope), request a light snapshot.
```
figma_plugin_get_node
  channel_id: "ch-..."
  node_id: "123:456"
  depth: 0
  include_geometry: false
  timeout_ms: 20000
```

If you need vector geometry, flip `include_geometry: true` for that single node.

## 3) Large Nodes: Progressive Loading

1) Get structure only (fast).
```
figma_get_node_summary
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
```

2) Load depth ranges as needed.
```
figma_get_node_chunk
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  depth_start: 0
  depth_end: 2
```

3) Repeat for deeper ranges.

### 3.1) gRPC Recursive Stream (Full Subtree)

Use when you want the full subtree without precomputing depth ranges.
```
grpcurl -plaintext -import-path proto -proto figma.proto \
  -d '{"file_key":"KEY","node_id":"123-456","token":"$FIGMA_TOKEN","recursive":true}' \
  localhost:50052 figma.v1.FigmaService/GetNodeStream
```

Safety knobs:
- `recursive_max_depth` (default 20)
- `recursive_max_nodes` (default 5000)
- `recursive_depth_per_call` (default 1)

### 3.2) gRPC PlanTasks (Divide & Conquer)

Generate a prioritized task list + requirements summary from a recursive tree.
```
grpcurl -plaintext -import-path proto -proto figma.proto \
  -d '{"file_key":"KEY","node_id":"123-456","token":"$FIGMA_TOKEN","recursive":true}' \
  localhost:50052 figma.v1.FigmaService/PlanTasks
```

Response extras:
- `summary`: priority/tokens overview
- `requirements_json`: node type counts + auto-layout/image fills + top-level names

### 3.3) Chunk Index + Selection (heuristic/LLM)

청킹된 인덱스를 먼저 받고, 필요한 청크만 선별합니다.

1) Chunk index (heuristic).
```
figma_chunk_index
  url: "https://www.figma.com/design/...?...node-id=123-456"
  token: "$FIGMA_TOKEN"
  chunk_size: 60
  selection_mode: "heuristic"
  selection_limit: 4
```

2) Chunk index (LLM selection, local 포함).
```
figma_chunk_index
  url: "https://www.figma.com/design/...?...node-id=123-456"
  token: "$FIGMA_TOKEN"
  chunk_size: 60
  selection_mode: "llm"
  selection_task: "Generate HTML/CSS matching the main layout."
  selection_llm_tool: "ollama"
  selection_llm_args:
    model: "qwen3-coder:30b"
```

3) Fetch a specific chunk.
```
figma_chunk_get
  file_path: "/tmp/figma-mcp/chunk_index_123_456_1700000000_1234.json"
  chunk_index: 2
```

## 4) Visual Verification Loop

1) Generate HTML and verify against Figma render.
```
figma_verify_visual
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  target_ssim: 0.95
  max_iterations: 3
```

2) Inspect the evolution report.
```
figma_evolution_report
  run_dir: "/tmp/figma-evolution/run_1705123456789"
  generate_image: true
```

## 5) Design Tokens Export

```
figma_export_tokens
  file_key: "KEY"
  token: "$FIGMA_TOKEN"
  format: "css"  # or tailwind/json
```

## 6) Compare Web vs Mobile

```
figma_compare
  file_key: "KEY"
  token: "$FIGMA_TOKEN"
  node_a_id: "111:222"
  node_b_id: "333:444"
```

## 7) Real-World: Figma → Near-Production Output

Goal: keep the generated output visually close to Figma while preserving text and variables.

1) Base bundle (layout + tokens + fills).
```
figma_get_node_bundle
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  include_meta: true
  include_variables: true
  include_image_fills: true
  include_plugin: true
  plugin_channel_id: "ch-..."
  plugin_depth: 0
  plugin_include_geometry: false
```

2) Precision pass for text.
- Select target frames in Figma
- Run `figma_plugin_read_selection` and merge segment bounds into your renderer
  (Plugin exposes text segments/line bounds that REST does not provide.)

3) Visual regression check (optional).
```
figma_verify_visual
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  target_ssim: 0.95
```

## 8) LLM-Assisted Output (Plugin + LLM)

Goal: use plugin-only details (text segments/line bounds) with DSL in one prompt.

1) Start MCP endpoint (llm-mcp is the default driver).
```
cd ~/me/workspace/yousleepwhen/llm-mcp
./start-llm-mcp.sh --port 8932
export MCP_SLOT_URL="http://127.0.0.1:8932/mcp"
```

Optional overrides:
- `MCP_SLOT_TIMEOUT_MS=120000`
- Per-call: `mcp_url: "http://..."`, `tool_name: "codex"`

2) Run LLM task with plugin channel.
```
figma_llm_task
  task: "Generate HTML/CSS matching the design. Output only code."
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  include_plugin: true
  plugin_channel_id: "ch-..."
  plugin_depth: 0
  quality: "best"
  llm_tool: "codex"
  llm_args:
    model: "gpt-5.2"
```

3) Preset quick start (overridden by explicit options).
```
figma_llm_task
  task: "Generate HTML/CSS matching the design. Output only code."
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  include_plugin: true
  plugin_channel_id: "ch-..."
  preset: "balanced"  # draft|balanced|fidelity|text|icon
```

4) Large context fallback (chunked + retry).
```
figma_llm_task
  task: "Generate HTML/CSS matching the design. Output only code."
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  include_plugin: true
  plugin_channel_id: "ch-..."
  context_strategy: "chunked"
  max_context_chars: 1000000
  retry_on_llm_error: true
  max_retries: 1
  min_context_chars: 600000
  retry_context_scale: 0.5
```

5) Chunk selection (heuristic/LLM).
```
figma_llm_task
  task: "Generate HTML/CSS matching the design. Output only code."
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  include_plugin: true
  plugin_channel_id: "ch-..."
  context_strategy: "chunked"
  chunk_select_mode: "llm"
  chunk_select_limit: 4
  chunk_select_llm_tool: "ollama"
  chunk_select_llm_args:
    model: "qwen3-coder:30b"
```

6) Plugin summary only (context save).
```
figma_llm_task
  task: "Generate HTML/CSS matching the design. Output only code."
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  include_plugin: true
  plugin_channel_id: "ch-..."
  plugin_context_mode: "summary"
  plugin_summary_sample_size: 5
```

Notes:
- `quality: fast` sets `budget_mode` and uses compact responses.
- If plugin isn't available, set `include_plugin: false` and rely on DSL/variables.

## 9) Real-World Flow (URL → Split → LLM)

Goal: make large files manageable and still hit high fidelity.

1) Parse URL → `file_key`/`node_id`.
```
figma_parse_url
  url: "https://www.figma.com/design/...?...node-id=123-456"
```

2) Use gRPC recursive stream for full subtree (recommended for huge nodes).
```
grpcurl -plaintext -import-path proto -proto figma.proto \
  -d '{"file_key":"KEY","node_id":"123:456","token":"'$FIGMA_TOKEN'","recursive":true}' \
  localhost:50052 figma.FigmaService/GetNodeStream
```

3) Build chunk index for selective LLM context.
```
figma_chunk_index
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  chunk_size: 50
  sample_size: 6
```

4) Run LLM task with preset + plugin summary (fast iteration).
```
figma_llm_task
  task: "Generate HTML/CSS matching the design. Output only code."
  file_key: "KEY"
  node_id: "123:456"
  token: "$FIGMA_TOKEN"
  include_plugin: true
  plugin_channel_id: "ch-..."
  preset: "fidelity"
```

## 10) Real-World Use Cases (Quick List)

1) **Full-page export with minimal loss**
- gRPC `GetNodeStream` (`recursive=true`) → chunk index → `figma_llm_task` preset `fidelity`
- Use plugin summary for fast iterations; switch to `plugin_context_mode: "both"` on final pass.

2) **Typography precision (line breaks, text bounds)**
- Render from DSL → pick suspect frames → `figma_plugin_read_selection`
- Merge text segment bounds into renderer; verify with `figma_verify_visual`.

3) **Design tokens & theme sync**
- `figma_get_variables` or `figma_export_tokens` → generate CSS/Tailwind map
- Keep token updates in CI and re-run `figma_verify_visual` for regressions.

4) **Icon/vector fidelity**
- `figma_plugin_get_node` with `include_geometry: true`
- Use `preset: "icon"` to bias chunk selection for vector-heavy areas.

## Troubleshooting

- `status: large_result` means the payload was saved to a file. Use the file path
  and load only the chunks you need (`figma_read_large_result`).
- If plugin commands hang, confirm the channel is connected and polling.
- `/plugin/poll` accepts `wait_ms` (long-poll) if you build a custom UI.
- For large nodes, use `plugin_depth: 0` and rely on `figma_plugin_read_selection`
  for the specific text ranges you need.
- `plugin_context_mode: summary` + `plugin_depth: 0` is a fast pass; use
  `plugin_context_mode: both` + `plugin_depth: 1` for final accuracy.
- `net::ERR_CONNECTION_REFUSED` usually means the HTTP server is not running or the
  plugin URL/port mismatch. Confirm `curl http://localhost:8940/health`.
- If Figma reports `Invalid value for devAllowedDomains`, use `localhost` only.
  IPs like `127.0.0.1` can be rejected; switch back to `plugin/manifest.json`.
- If LLM tools fail, check `MCP_SLOT_URL` and verify the MCP endpoint supports `tools/call`.
- If `grpcurl` reports missing reflection, pass `-import-path proto -proto figma.proto`.

---

## 11) Visual Verification: 95% SSIM 달성 전략

### 문제: 72%에서 막힘

전체 이미지 비교 시 SSIM이 72%에서 정체되는 경우가 많습니다.

| 시도 | SSIM | 결과 |
|------|------|------|
| 초기 구현 | 65% | 많이 부족 |
| DSL 정확도 ↑ | 72% | 개선 |
| 추가 수정 | 72% | **정체** |

### 해결: 레이아웃/텍스트 분리 측정

**핵심 질문**: "폰트와 아이콘을 제외하고 큰 레이아웃 덩어리부터 잡는다면?"

```bash
# 1. 텍스트/아이콘 영역 마스킹 (ImageMagick)
magick figma.png -fill gray -draw "rectangle 100,50 300,80" figma_masked.png
magick html.png -fill gray -draw "rectangle 100,50 300,80" html_masked.png

# 2. 마스킹된 이미지로 레이아웃만 비교
magick compare -metric SSIM figma_masked.png html_masked.png null: 2>&1
```

**결과**:
```
전체 비교:     SSIM 72.5%  (막힘)
텍스트 마스킹: SSIM 95.7%  ✅
```

### 해석 및 다음 단계

| 레이아웃 SSIM | 의미 | 액션 |
|---------------|------|------|
| > 95% | 레이아웃 완벽 | 텍스트/아이콘만 집중 수정 |
| 80-95% | 미세 조정 필요 | DSL padding/radius 재확인 |
| < 80% | 구조 문제 | DSL 다시 분석 |

### 흔한 함정들

1. **높이 계산 오류**: 콘텐츠가 뷰포트를 초과하면 하단 요소가 잘림
   ```css
   /* DSL 높이값 정확히 적용 */
   .header  { height: 44px; }
   .content { height: 276px; }  /* 고정 height 누락 주의! */
   ```

2. **DSL 값 오류**: radius, padding 미세 차이
   ```
   radius: 10px  (4px로 잘못 설정 → 차이 발생)
   padding: 12/16/12/16  (TRBL 순서 주의!)
   ```

3. **현실적 기대치**:
   - 자동 변환: **70-80%** SSIM
   - 수동 미세조정: **80-85%** SSIM
   - 레이아웃만 측정: **95%+** 가능!

### MCP 도구 활용

```bash
# 1. DSL 추출
figma_get_node_bundle
  file_key: "KEY"
  node_id: "123:456"

# 2. Visual 검증 (텍스트 체크 포함)
figma_verify_visual
  file_key: "KEY"
  node_id: "123:456"
  html: "<html>...</html>"
  target_ssim: 0.95
  html_screenshot: "/path/to/rendered.png"  # Chrome MCP로 렌더링

# 3. 영역별 상세 비교
figma_compare_regions
  image1: "/path/to/figma.png"
  image2: "/path/to/html.png"
```

### 핵심 원칙

> **막혔을 때 "전체"를 보지 말고 "분리해서" 측정하라.
> 어디가 OK이고 어디가 문제인지 명확해진다.**
