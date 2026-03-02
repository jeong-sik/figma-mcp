# figma-mcp Plugin Workflow & Call Flow

## Architecture Overview

```
┌──────────────┐    MCP (JSON-RPC)    ┌───────────────┐    HTTP Poll     ┌─────────────┐
│  Claude Code │ ──────────────────── │  figma-mcp    │ ─────────────── │  Figma      │
│  (MCP Client)│                      │  Server       │                  │  Plugin     │
│              │ ◀─────────────────── │  (OCaml/Eio)  │ ◀────────────── │  (code.js)  │
└──────────────┘    SSE / Response    └───────────────┘    Result POST   └─────────────┘
                                           │
                                      HTTP :8940
                                      /mcp (MCP)
                                      /plugin/* (Plugin Bridge)
```

## Connection Flow

```
1. Server start:   main.exe --port 8940
2. Plugin connect:  Figma Plugin → POST /plugin/connect → channel_id
3. Plugin polls:    Figma Plugin → GET /plugin/poll?channel={id}&max=5&wait_ms=15000
4. MCP ready:       Claude Code → POST /mcp (JSON-RPC initialize)
```

**Reconnect after server restart**: Bridge now attempts one forced channel refresh.
If refresh fails, reopen the plugin in Figma to establish a new channel.

## Command Dispatch Chain

```
MCP Client                    figma-mcp Server                      Figma Plugin
    │                              │                                     │
    │  tool_call: figma_plugin     │                                     │
    │  action: "set_fill"          │                                     │
    │  node_id: "6:7"             │                                     │
    │  ─────────────────────────► │                                     │
    │                              │  dispatch: action → handler         │
    │                              │  "set_fill" → handle_plugin()      │
    │                              │                                     │
    │                              │  enqueue_command(                   │
    │                              │    channel_id, name, payload)       │
    │                              │  ─────────────────────────────────► │
    │                              │                                     │  poll returns cmd
    │                              │                                     │  execute: handlers["set_fill"]
    │                              │                                     │  → H.node((node, p) => ...)
    │                              │  ◀───────────────────────────────── │
    │                              │    POST /plugin/result              │
    │                              │    { command_id, ok, payload }      │
    │                              │                                     │
    │  ◀──────────────────────────│  plugin_wait returns                │
    │    MCP response (JSON)       │                                     │
```

## Key Naming Mapping

| MCP action parameter | Server handler function | Server command name | Plugin handler key |
|----------------------|------------------------|--------------------|--------------------|
| `connect` | (direct HTTP) | - | `connect()` |
| `get_doc_info` | `handle_plugin_get_doc_info` | `get_doc_info` | `get_doc_info` |
| `set_fill` | via `handle_plugin()` | `set_fill` | `set_fill` |
| `export_image` | `handle_plugin_export_node_image` | `export_image` | `export_image` |
| `export_selection` | via `plugin_simple` | `export_selection` | `export_selection` |
| `export_viewport` | via `plugin_simple` | `export_viewport` | `export_viewport` |

**Bug fixed (0.7.0)**: Server was sending `export_node_image` but plugin expected `export_image`.

## Dedicated Write Tools (figma_plugin_*)

These have their own MCP tool definitions separate from the generic `figma_plugin` action:

| MCP Tool | Handler | Plugin Command |
|----------|---------|---------------|
| `figma_plugin_create_node` | `handle_plugin_create_node` | `create_{type}` |
| `figma_plugin_edit_node` | `handle_plugin_edit_node` | multiple (set_fill, set_text, etc.) |
| `figma_plugin_delete_nodes` | `handle_plugin_delete_nodes` | `delete_node` |
| `figma_plugin_batch` | `handle_plugin_batch` | (batched ops) |

## Export Methods Comparison

| Method | Source | Token Cost | File Output | Requirements |
|--------|--------|-----------|-------------|-------------|
| `export_image` (plugin) | Plugin `exportAsync` | File path only (base64 saved to /tmp) | `/tmp/figma-export-{node_id}.png` | Plugin connected |
| `export_selection` (plugin) | Plugin `exportAsync` on selection | Full base64 in response | No file (context only) | Selection set + Plugin |
| `export_viewport` (plugin) | Plugin renders visible area | Full base64 in response | No file (context only) | Plugin connected |
| `export_image` (REST API) | Figma cloud render | URL string or `download=true` | Configurable `save_dir` | file_key (cloud saved) |
| `export_smart` (REST API) | Figma cloud, auto-scale | URL or download | Configurable `save_dir` | file_key |

**Recommendation**: Use plugin `export_image` for local files (saves PNG to /tmp, no token waste).
Use REST API `export_image` with `download=true` for cloud-saved files.

## Component Reuse Flow

```
1. List components:     figma_plugin action=list_components
2. Create instance:     figma_plugin action=create_instance component_key=<key> parent_id=<frame_id>
3. Edit instance:       figma_plugin_edit_node node_id=<instance_id> properties={...}
4. Swap component:      figma_plugin action=swap_component node_id=<instance_id> component_id=<new_comp_id>
```

## Full Cycle Test Flow (Create → Export → Edit → Re-export)

```
Step 1: Create complex screen
  figma_plugin_create_node (frame + children)

Step 2: Export v1 snapshot
  figma_plugin action=set_selection node_ids=[frame_id]
  figma_plugin action=export_image node_id=<frame_id>
  → /tmp/figma-export-{node_id}.png (v1)

Step 3: Edit multiple elements
  figma_plugin_edit_node (color, text, opacity, etc.)

Step 4: Re-export v2 snapshot
  figma_plugin action=export_image node_id=<frame_id>
  → /tmp/figma-export-{node_id}.png (v2, overwrite)

Step 5: Visual diff
  timg /tmp/figma-export-*.png  (terminal display)
  or figma_verify_visual for SSIM comparison
```

## Known Issues & Gotchas

1. **Server restart kills plugin connection** — Plugin must be reopened in Figma after server restart
2. **btoa unavailable in Figma sandbox** — Custom `uint8ToBase64()` used instead (cf11850)
3. **export_selection returns full base64** — Token expensive, prefer plugin export_image for file output
4. **Untitled files have no file_key** — REST API export_image won't work, use plugin methods
5. **Plugin poll timeout** — Default 15s, long exports (25+ nodes) may need extended timeout
6. **Channel stale after restart** — Bridge performs one automatic channel refresh; reopen plugin if retry also fails
