# figma-mcp SDK Migration Roadmap

## Summary

This document fixes the current `codex-refactor-wave1` worktree as the Wave 1 baseline for `figma-mcp`, then defines a long-term migration path toward:

- `mcp-protocol-sdk` for pure MCP transport
- repo-local domain handlers for Figma/tool behavior
- a future split of plugin bridge and agent queue into separate extension services

`agent_sdk` is considered only for future runtime-style loops. It is not the target for the MCP transport layer.

## Wave 1 Baseline

Current baseline in this worktree:

- `lib/mcp_tools.ml` is a public facade
- `lib/mcp_tool_defs.ml` owns tool schemas and public tool definitions
- `lib/mcp_tool_handlers.ml` owns utility handlers and thin orchestration around handler functions
- `lib/mcp_tool_registry.ml` owns handler registration, category/public tool assembly, resources, prompts, and server creation

Transport split baseline:

- `lib/mcp_protocol_eio.ml` is a public facade
- `lib/mcp_protocol_request.ml` owns request classification and MCP request processing helpers
- `lib/mcp_protocol_plugin_http.ml` owns plugin bridge and agent queue HTTP handlers
- `lib/mcp_protocol_router.ml` owns route selection
- `lib/mcp_protocol_server.ml` owns httpun server lifecycle and stdio startup

Public behavior intentionally preserved:

- MCP tool/resource/prompt surface
- plugin bridge routes
- agent queue routes
- stdio and HTTP entrypoints

## Seam Inventory

### 1. MCP transport seam

Current modules:

- `mcp_protocol_request`
- `mcp_protocol_router`
- `mcp_protocol_server`
- `figma_mcp_protocol`

Natural target:

- `mcp-protocol-sdk/http/http_server`
- `mcp-protocol-sdk/eio/handler`

Status:

- request classification and MCP response processing are now isolated enough to compare directly with SDK APIs
- transport is still mixed with non-MCP routes through the router

Migration blockers:

- plugin bridge and agent queue live in the same router and process
- server lifecycle still knows about custom SSE client state and custom shutdown broadcasting
- `figma_mcp_protocol` remains a repo-local protocol layer around MCP primitives

### 2. Tool surface seam

Current modules:

- `mcp_tool_defs`
- `mcp_tool_handlers`
- `mcp_tool_registry`
- domain handlers under `mcp_api_handlers`, `mcp_visual_handlers`, `mcp_plugin_handlers`

Natural target:

- keep tool/domain logic repo-local
- adapt only registration and MCP exposure through `mcp-protocol-sdk`

Status:

- good fit for transport migration
- poor fit for generic runtime migration, because most value is domain-specific Figma logic rather than turn-based agent loops

### 3. Extension seam

Current HTTP extension groups:

- Plugin bridge:
  - `/plugin/connect`
  - `/plugin/poll`
  - `/plugin/result`
  - `/plugin/event`
  - `/plugin/status`
  - `/plugin/codegen`
  - `/plugin/analyze`
- Agent queue:
  - `/agent/request`
  - `/agent/pending`
  - `/agent/claim`
  - `/agent/heartbeat`
  - `/agent/abandon`
  - `/agent/result`
  - `/agent/status/:id`
  - `/agent/queue`

Long-term target:

- move both groups out of MCP transport into separate extension services or sidecars

Reason:

- these APIs are not MCP primitives
- keeping them inside MCP transport blocks a clean `mcp-protocol-sdk` swap
- their scaling, auth, and lifecycle concerns differ from MCP request handling

## Compatibility Matrix

| Area | Current owner | SDK target | Fit | Notes |
| --- | --- | --- | --- | --- |
| MCP request parsing | `mcp_protocol_request` | `mcp-protocol-sdk/eio/handler` | High | Method classification and process flow are now isolated. |
| MCP HTTP transport | `mcp_protocol_server` + `mcp_protocol_router` | `mcp-protocol-sdk/http/http_server` | Medium | Works after extension routes are separated. |
| Tool/resource/prompt registration | `mcp_tool_registry` | SDK handler registration | High | Current facade already centralizes these lists. |
| Custom SSE handling | `mcp_sse_transport` | SDK broadcaster/session flow | Medium | Needs adapter or replacement once custom routes are gone. |
| Plugin bridge HTTP | `mcp_protocol_plugin_http` | none | Low | Should become an extension service, not an SDK concern. |
| Agent queue HTTP | `mcp_protocol_plugin_http` + `mcp_agent_queue` | none | Low | Same as above. |
| Fidelity/visual/runtime loops | `mcp_visual_handlers`, `figma_*` domain modules | `agent_sdk` | Low/Medium | Only selected loop-shaped flows may fit. Core Figma APIs should stay repo-local. |

## Target Architecture

### Layer 1: MCP transport

- implemented via `mcp-protocol-sdk`
- exposes only MCP routes and MCP lifecycle
- owns MCP session and notification plumbing

### Layer 2: Domain handlers

- current Figma tool handlers remain repo-local
- tool registration is assembled by an adapter over `mcp_tool_registry`
- no attempt to genericize DSL generation, visual verification, or Figma API traversal

### Layer 3: Extension services

- plugin bridge service
- agent queue service

These may stay in-process initially but should have their own routing and deployment boundary in the architecture documents and implementation plan.

## Migration Phases

### Phase A

- Keep current Wave 1 split as the baseline.
- Treat `mcp_tools` and `mcp_protocol_eio` facades as stable public entrypoints.

### Phase B

- Write a transport adapter plan from `mcp_tool_registry` to `mcp-protocol-sdk` handler registration.
- Document exact mappings:
  - tools
  - resources
  - resource templates
  - prompts

### Phase C

- Move plugin bridge and agent queue to an explicit extension boundary.
- First step can still be same-process modules, but no longer inside the MCP transport callback path.

### Phase D

- Replace MCP transport routing with `mcp-protocol-sdk` callback/handler wiring.
- Keep existing domain handlers intact.

### Phase E

- Evaluate `agent_sdk` only for narrow loop-style flows, such as long-running verification or tool-guided refinement helpers.
- Do not use it for raw Figma API serving or MCP transport.

## Extension-Service Boundary Spec

### Plugin bridge service

Responsibilities:

- channel registration and waiter lifecycle
- plugin polling/result submission
- plugin-side codegen/analyze helpers

Contract:

- preserve current payload shapes during first extraction
- auth policy can remain identical initially
- MCP transport should call this service through an internal client if needed, not own its state directly

### Agent queue service

Responsibilities:

- request creation
- claim/heartbeat/result lifecycle
- queue inspection and status lookups

Contract:

- preserve current endpoint semantics during first extraction
- queue state and secret validation must move with the service, not stay mirrored in MCP transport

## Acceptance Criteria For The Next Refactor PR

- MCP-only route handling is separable from plugin/agent HTTP routes without changing tool behavior
- a documented adapter exists from `mcp_tool_registry` to `mcp-protocol-sdk`
- extension routes are listed as explicit migration targets, not hidden inside generic router logic
- no tool schema or public MCP tool name changes are bundled with transport migration

## Non-Goals

- rewriting Figma domain handlers into generic SDK tools
- moving visual verification into `agent_sdk` by default
- changing plugin or agent queue payload shapes during seam creation
- replacing `figma_mcp_protocol` and extension routes in the same PR
