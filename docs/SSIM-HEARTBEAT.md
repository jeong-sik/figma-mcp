# SSIM Heartbeat (Periodic Visual Checks)

`figma-ssim-heartbeat.py` runs visual similarity / regression checks periodically by calling `figma-mcp` tools over MCP JSON-RPC HTTP (`tools/call`).

This is intentionally a separate process (daemon) instead of embedding a long-lived loop inside the MCP server:
- The MCP server stays stateless and short-lived per request.
- Failures are isolated (restartable by launchd/systemd).
- Monitoring/logging becomes straightforward (JSONL).

## Quick Run

Example config lives at `scripts/ssim-heartbeat.example.json`.

```bash
python3 scripts/figma-ssim-heartbeat.py --config scripts/ssim-heartbeat.example.json --once
```

Exit code:
- `0`: all jobs succeeded and (when applicable) passed thresholds
- `1`: at least one job errored or failed a threshold

## Config Format

Top-level:
- `jobs`: array (required)

Each job:
- `type`: `image_similarity` | `verify_visual` | `fidelity_loop`
- `name`: string (optional; default `job_N`)
- `enabled`: bool (optional; default `true`)
- `timeout_s`: number (optional; default `180`)

Required fields by type:
- `image_similarity`: `file_key`, `node_a_id`, `node_b_id`
- `verify_visual`: `file_key`, `node_id`
- `fidelity_loop`: `file_key`, `node_id`

Notes:
- If your `figma-mcp` instance enforces an API key, set `FIGMA_MCP_API_KEY` (or `MCP_API_KEY`), or pass `--mcp-api-key`.
- If tools require a Figma PAT, use `FIGMA_TOKEN` env var (recommended). You can also set `token` per job, but the heartbeat log will redact it.

## Logging

Writes JSON Lines (one record per job run):
- Default: `~/me/logs/figma-ssim-heartbeat.jsonl`
- Override: `--log` or `FIGMA_SSIM_HEARTBEAT_LOG`

Sensitive fields are redacted:
- job args: `html`, `token`, `plugin_data`
- parsed results: `final_html`

## Running As a Daemon (launchd, macOS)

Template: `scripts/com.yousleepwhen.figma-ssim-heartbeat.plist`

Typical flow:
- set a real config path (not the example)
- set env vars: `FIGMA_MCP_URL`, `FIGMA_MCP_API_KEY` (if needed), `FIGMA_TOKEN` (if needed)
- load via `launchctl`

