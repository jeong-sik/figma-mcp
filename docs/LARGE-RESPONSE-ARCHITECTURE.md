# Figma MCP Large Response Architecture

## Problem Statement

When fetching complex Figma nodes (e.g., entire pages with deep hierarchies), the response can exceed 7MB, causing:

1. **MCP stdio buffer overflow**: Large JSON blocks the communication channel
2. **Claude Code UI freeze**: Attempting to load 7MB into context window
3. **Memory pressure**: Both server and client hold full response in memory

## Solution Architecture

### Phase 1: File-Based Large Response Handler

**Threshold**: 500KB (configurable via `FIGMA_MAX_INLINE_RESPONSE`)

```
┌─────────────────────┐
│  Figma API Response │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│   Size Check        │
│   > 500KB?          │
└──────────┬──────────┘
           │
    ┌──────┴──────┐
    │             │
    ▼             ▼
  Small         Large
    │             │
    ▼             ▼
┌─────────┐ ┌───────────────┐
│ Inline  │ │ Save to File  │
│ Return  │ │ Return Path   │
└─────────┘ └───────────────┘
```

**Response Format** (large):
```json
{
  "status": "large_result",
  "file_path": "/tmp/figma-mcp/node_2089_10737.json",
  "size_bytes": 7764220,
  "size_human": "7.4MB",
  "format": "fidelity",
  "hint": "Use read_file with offset/limit to read chunks, or figma_get_node_chunk for structured access"
}
```

### Phase 2: Progressive Loading Tools

New MCP tools for granular access:

#### `figma_get_node_summary`
Returns lightweight structure without full content:
```json
{
  "node_id": "2089:10737",
  "name": "HomePage",
  "type": "FRAME",
  "children_count": 47,
  "children": [
    { "id": "2089:10738", "name": "Header", "type": "FRAME", "children_count": 12 },
    { "id": "2089:10750", "name": "Hero", "type": "FRAME", "children_count": 8 },
    ...
  ],
  "estimated_full_size": "7.4MB"
}
```

#### `figma_get_node_chunk`
Fetch specific depth range:
```
figma_get_node_chunk(
  file_key: "...",
  node_id: "2089:10737",
  depth_start: 0,    # Start from root
  depth_end: 2,      # Only 2 levels deep
  include_styles: false  # Skip style definitions
)
```

#### `figma_get_component`
Fetch single component by name/id:
```
figma_get_component(
  file_key: "...",
  component_name: "Button/Primary"
)
```

### Phase 3: DSL Compression

**Current Fidelity DSL** (verbose):
```
FRAME "Card" {
  width: 320
  height: 200
  fill: #FFFFFF
  cornerRadius: 8

  FRAME "CardHeader" {
    width: 320
    height: 60
    fill: #FFFFFF
    cornerRadius: 8
  }
}
```

**Compressed DSL v2** (reference-based):
```
@styles {
  card_base: { fill: #FFFFFF, cornerRadius: 8 }
}

FRAME "Card" @card_base w:320 h:200 {
  FRAME "CardHeader" @card_base w:320 h:60 { }
}
```

**Compression strategies**:
1. **Style deduplication**: Extract repeated fill/stroke/effect patterns
2. **Dimension inference**: Omit width/height when matches parent
3. **Default omission**: Skip values matching Figma defaults
4. **Symbol references**: `@ComponentName` instead of inline definition

### Phase 4: Streaming (HTTP Mode Only)

For HTTP MCP mode, support Server-Sent Events:

```
GET /mcp/stream?tool=figma_get_node_bundle&...

data: {"type": "progress", "percent": 10, "message": "Fetching node..."}
data: {"type": "progress", "percent": 30, "message": "Parsing children..."}
data: {"type": "chunk", "index": 0, "data": "{\"type\":\"FRAME\"...}"}
data: {"type": "chunk", "index": 1, "data": "{\"children\":[...]}"}
data: {"type": "complete", "total_chunks": 15}
```

## Implementation Plan

### Phase 1 (Immediate)
- [x] Design document
- [ ] Add `large_response_handler.ml` module
- [ ] Modify `handle_get_node_bundle` to use handler
- [ ] Add file cleanup mechanism (TTL: 1 hour)

### Phase 2 (Short-term)
- [ ] Implement `figma_get_node_summary`
- [ ] Implement `figma_get_node_chunk`
- [ ] Add `max_response_size` parameter to existing tools

### Phase 3 (Medium-term)
- [ ] Create `figma_dsl_v2.ml` with compression logic
- [ ] Add style extraction and deduplication
- [ ] Benchmark compression ratios

### Phase 4 (Long-term)
- [x] SSE streaming for HTTP mode
- [ ] WebSocket support consideration
- [ ] Client-side progressive rendering

## Configuration

Environment variables:
```bash
# Maximum inline response size (default: 500KB)
FIGMA_MAX_INLINE_RESPONSE=524288

# Large response storage directory
FIGMA_LARGE_RESPONSE_DIR=/tmp/figma-mcp

# Response file TTL in seconds (default: 3600)
FIGMA_RESPONSE_TTL=3600
```

## Metrics

Track and log:
- Response sizes (histogram)
- File storage usage
- Compression ratios (Phase 3)
- Stream completion times (Phase 4)

## References

- MCP Specification: https://modelcontextprotocol.io/
- Claude Code large response handling
- Figma API rate limits and best practices
