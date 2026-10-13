# Figma Studio

A standalone local service and Figma Plugin. Any authenticated HTTP client can capture and edit a native document or inspect a retained snapshot. The service knows no agent harness, model provider or client repository. Python 3.11+ on macOS or Linux is its only service dependency. The existing repository MCP server is separate and is not required by Studio.

## Install and run

```sh
python3 studio/build.py --output figma-studio.zip
# Unpack in an installation directory; no source checkout is needed.
python3 studio/service.py init --config /path/to/private/studio.toml
python3 studio/service.py serve --config /path/to/private/studio.toml
```

`init` creates private, distinct client and Plugin credentials. The state directory resolves relative to that configuration. The optional `figma.token` is a PAT used only for REST capture; it stays in the service.

State is private to the current OS user. A lifetime writer lease rejects a second service using the same directory before opening or recovering the database, including when the second process uses another port. Use separate state directories for separate services.

Create a development Plugin in Figma, then preserve its assigned identity:

```sh
python3 studio/prepare.py --from-manifest /path/to/figma/manifest.json --output /path/to/new/plugin
python3 studio/service.py plugin-token --config /path/to/private/studio.toml
```

Import the manifest and enter the Plugin token. The development manifest permits `http://127.0.0.1:8941`. A different endpoint needs both the Plugin manifest permission and UI URL changed. [Figma manifest contract](https://developers.figma.com/docs/plugins/manifest/).

## Public API

POST JSON with `Authorization: Bearer <client_token>`. Plugin credentials cannot call client operations; client credentials cannot submit Plugin results. `/v1/info` returns `protocol: "figma-studio/1"`, `service_id`, capabilities and the envelope limit. Validate it before dispatch. An unsupported major version must fail before writes. Additive response fields and capabilities are allowed within v1.

| Endpoint | Body and result |
| --- | --- |
| `/v1/info` | `{}` → protocol, service identity, capabilities, envelope |
| `/v1/status` | `{}` → reported Plugin sessions and latest snapshot |
| `/v1/operations` | `service_id`, `operation_id`, `session_id`, `kind`, `root_id`; apply also needs `expected_tree_json` and `ops` |
| `/v1/operation` | `operation_id` → retained state and result without replay |
| `/v1/readback` | `operation_id`, byte `offset`, optional byte `limit` → exact JSON result bytes in bounded base64 chunks |
| `/v1/snapshot` | `snapshot_id` → native JSON and PNG with digest |
| `/v1/validate` | `snapshot_id`, `assertions` → structural verdict; no visual verdict |
| `/v1/rest-capture` | `file_key`, `root_id`, optional `version` → pinned server snapshot |

`operation_id` identifies exact input. Different input reusing an ID is refused. Capture uses `kind: "capture"`. Apply uses `kind: "apply"` and an exact native export as its baseline; the Plugin checks it before editing. Submission is a receipt, not completion.

Operation polling returns compact result metadata, including `snapshot_id` and `readback_available`. Readback chunks carry `encoding: "json-utf8"`, `offset`, `next_offset`, `total_bytes`, and `sha256`. Read until `next_offset` is null and verify the assembled digest. This preserves even an oversized or malformed Plugin result without making completion status unreadable. Chunking does not retry a document operation.

The Plugin uses its separate token at `/v1/plugin/session`, `/v1/plugin/poll` and `/v1/plugin/result`. Pairing explicitly identifies the protocol version. Dispatch is durable before a command is returned. Dispatched writes are never automatically replayed. Restart rejects undispatched work and marks dispatched work uncertain. Stale service identities cannot submit or acknowledge another incarnation's work.

## Validation and distribution

```sh
npm run test:studio
python3 -m unittest discover -s test/studio -p 'test_*.py' -v
```

Tests run the HTTP service and actual Plugin JavaScript against a controlled native document. They require no other repository, agent, model or external credential. Real Figma execution and visual fidelity remain separate evidence.

Consumers install the ZIP and speak the v1 API. They do not import this checkout or share implementation modules. Plugin, API, and service are released as one independently installable Figma capability.
