# Figma Studio: deferred design idea

Status: deferred by operator decision on 2026-10-13. The experimental `studio/`
relay, `plugin/studio/` Plugin and their test suite have been withdrawn. They
required MASC tools, credentials and worker protocols in this repository.
The existing eight public MCP tools and the general `plugin/` bridge remain.

A future Figma service must be independently installable and usable from a
non-MASC client. It owns Plugin sessions, Figma authentication, native document
operations and retained snapshots through a versioned API. It does not import
MASC code, receive MASC credentials or call MASC tools. A MASC Lane Addon, if
built later, consumes that API from the MASC repository.

The intended product still includes native design creation/revision and
implementation of a pinned design in an application. Before implementation,
review capability coverage against the existing MCP tools, full snapshot and
asset fidelity, session close/cancellation, uncertain effects and recovery.
Both real Figma editing and a running implementation need evidence; current
fixtures do not establish either journey or visual fidelity.

The unshipped independent-service experiment is retained for reference in
[#194](https://github.com/jeong-sik/figma-mcp/pull/194). It has unresolved Plugin
session lifecycle findings and is not ready for installation. The broader idea
is tracked in [MASC #42735](https://github.com/jeong-sik/masc/issues/42735).
There is no promised Studio release version.

## Native dispatcher repair

The existing `handle_plugin_apply_ops` sends a command named `apply_ops` with an
`ops` array. The shipped Plugin previously handled only the UI's `execute_dsl`
creation format. An always-successful server test responder did not execute the
Plugin and therefore did not detect that mismatch.

The Plugin now accepts this explicit envelope:

```json
{
  "ops": [
    {"op": "create_frame", "params": {"name": "Sign in", "width": 360, "height": 640}}
  ]
}
```

Each entry contains exactly `op` and object-valued `params`. The dispatcher
validates all operation names and envelopes before running any handler. Nested
`apply_ops` and `execute_dsl` are rejected. Parameters still follow the existing native handler
contracts; this is not the later typed design-plan validator. Bridge commands
execute serially, including when a native handler awaits fonts or node lookup.
The network-only `llm_proxy` route remains independent of that document queue.

Success returns `status: applied` and ordered `completed` handler results. This
means those native handlers reported success, not that an independent verifier
approved the design. A malformed envelope returns `rejected` without executing
its operations. A failure after dispatch returns `outcome_unknown`, the completed
results, failing index/action and failure payload; later operations are not run.
A failing native handler may already have edited the file. There is no automatic
rollback or duplicate-delivery guarantee in this first unit.

Native `payload.error` results are failures even when produced by a simple
handler. A result too large for the existing UI envelope, or one that cannot be
serialized, is returned with `ok: false` and `outcome_unknown`. The existing size
bound is measured in UTF-8 bytes. Oversized batch results retain compact completed
operation identities and explicit counts for omitted details/identities. A missing receipt
does not establish that its document edits failed or permit automatic replay.

The existing UI `execute_dsl` route remains supported. This change does not expose
write tools on the public eight-tool surface. Explicit session targeting,
idempotent operation storage, safe reconnect, full snapshot capture and an
optional editing surface remain deferred design topics.

## Verification

Run `npm run test:plugin` (Node.js test runner, no npm dependencies needed).
The fixture loads the actual `plugin/code.js` entrypoint, sends commands through
its nonce-protected UI boundary and reads resulting node state. It covers native
creation/text/auto layout, readback, invalid/unknown operations, partial effects,
queue continuity and the existing UI DSL. Its Figma document is a fake; this does
not prove behavior inside the real Figma app.
