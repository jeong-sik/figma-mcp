# figma-mcp

Fidelity-first Figma MCP server for design context extraction and verification.

This branch resets the public MCP surface around the core job:

- extract design context from Figma
- inspect large selections safely
- fetch variable definitions and screenshots
- resolve Code Connect mappings
- verify semantic and visual parity

It does not expose agent queue, planning, category routers, or gRPC transport from the MCP surface.

## Status

- `dune build --root .` passes
- `dune build --root . @runtest` passes
- startup scripts are worktree-safe and use `--root "$SCRIPT_DIR"` when building

## Public MCP Tools

The server exposes exactly 8 tools:

- `figma_get_design_context`
- `figma_get_metadata`
- `figma_get_variable_defs`
- `figma_get_screenshot`
- `figma_get_code_connect_map`
- `figma_whoami`
- `figma_verify_semantic`
- `figma_verify_visual`

## Architecture

### In scope

- Figma REST-backed context extraction
- optional desktop plugin bridge backend at `/plugin/connect`, `/plugin/poll`, `/plugin/result`, `/plugin/event`, `/plugin/status`
- semantic verification
- visual verification
- Code Connect lookup through local mapping files

### Out of scope

- in-process agent queue
- planning/orchestration tools
- gRPC transport
- plugin-hosted code generation and mutation endpoints

If agent orchestration is needed, it should live outside this server and consume these MCP tools or the underlying OCaml libraries.

## Quickstart

```bash
eval "$(opam env)"
opam install . --deps-only
dune build --root .

export FIGMA_TOKEN="YOUR_TOKEN"
export FIGMA_MCP_API_KEY="dev-only-key"

./start-figma-mcp.sh
./start-figma-mcp-http.sh --port 8940
```

Example MCP client config:

```json
{
  "mcpServers": {
    "figma": {
      "type": "http",
      "url": "http://127.0.0.1:8940/mcp",
      "headers": {
        "x-api-key": "dev-only-key"
      }
    }
  }
}
```

## Development

Build:

```bash
dune build --root .
```

Run tests:

```bash
dune build --root . @runtest
```

The current environment may return `Error: RPC server not running.` for `dune runtest --root .`. Use the `@runtest` alias through `dune build` instead.

## Notes

- `FIGMA_TOKEN` still takes precedence over request-supplied `token` values.
- HTTP mode requires `FIGMA_MCP_API_KEY` or `MCP_API_KEY`, unless `--allow-no-auth` is set.
- The `docs/` directory still contains historical material from pre-v2 branches. Treat `README.md` and the `figma://docs/*` MCP resources as the authoritative surface for this branch.
