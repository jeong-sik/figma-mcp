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

Notes:
- `quality: fast` sets `budget_mode` and uses compact responses.
- If plugin isn't available, set `include_plugin: false` and rely on DSL/variables.

## Troubleshooting

- `status: large_result` means the payload was saved to a file. Use the file path
  and load only the chunks you need (`figma_read_large_result`).
- If plugin commands hang, confirm the channel is connected and polling.
- `/plugin/poll` accepts `wait_ms` (long-poll) if you build a custom UI.
- For large nodes, use `plugin_depth: 0` and rely on `figma_plugin_read_selection`
  for the specific text ranges you need.
- If LLM tools fail, check `MCP_SLOT_URL` and verify the MCP endpoint supports `tools/call`.
- If `grpcurl` reports missing reflection, pass `-import-path proto -proto figma.proto`.
