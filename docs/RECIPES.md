# Figma MCP Recipes

Authoritative for v0.10.1 and later.

This server no longer exposes agent-planning or gRPC transport flows. Use the v2 MCP tools below.

## 1) Quickstart

1. Start the server:

```bash
./start-figma-mcp-http.sh --port 8940
```

2. Inspect large selections safely from a Figma URL:

```text
figma_get_metadata
  url: "https://www.figma.com/file/KEY/NAME?node-id=123-456"
  depth: 2
  max_children: 100
```

3. Fetch full implementation context:

```text
figma_get_design_context
  url: "https://www.figma.com/file/KEY/NAME?node-id=123-456"
  include_variables: true
```

## 2) Plugin-Enriched Context

If a plugin channel is connected, pass `plugin_channel_id`. The server will enable plugin enrichment automatically.

```text
figma_get_design_context
  file_key: "KEY"
  node_id: "123:456"
  plugin_channel_id: "ch-..."
```

## 3) Tokens And Code Connect

Variable definitions:

```text
figma_get_variable_defs
  file_key: "KEY"
  format: "resolved"
```

Code Connect map lookup:

```text
figma_get_code_connect_map
  mode: "match"
  node_id: "123:456"
```

## 4) Screenshots And Verification

Screenshot:

```text
figma_get_screenshot
  file_key: "KEY"
  node_id: "123:456"
  format: "png"
  scale: 2
```

Semantic verification:

```text
figma_verify_semantic
  file_key: "KEY"
  node_id: "123:456"
  html: "<html>...</html>"
```

Visual verification:

```text
figma_verify_visual
  file_key: "KEY"
  node_id: "123:456"
  html: "<html>...</html>"
  target_ssim: 0.95
```
