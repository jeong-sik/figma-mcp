# Code Connect (Component Mapping) – Minimal Spec

This document proposes a minimal, repo-local "Code Connect" style component mapping system for figma-mcp. It defines:

- Mapping file format (JSON)
- Tool API (MCP tools)
- Matching algorithm (deterministic with optional LLM assist)
- Test plan

The design is intentionally small and deterministic. LLM usage is optional and never required for correctness.

## Goals

- Map Figma components/variants to code components in a local codebase.
- Provide deterministic matching first, with transparent scores.
- Support incremental adoption: start with a few explicit mappings and expand.
- Stay repo-local: no external network assumptions.

Non-goals:

- Auto-generating code.
- Cloud-hosted or shared registries.
- Perfect matching without explicit hints.

## Mapping File

### File name and location

Default search order:

1. `./figma-code-connect.json` (repo root of the codebase using figma-mcp)
2. `./.figma/code-connect.json`

The tool can also accept an explicit path or inline JSON.

### JSON schema (minimal)

```json
{
  "version": "1.0",
  "project": {
    "name": "my-app",
    "repo": "local",
    "design_system": "acme-ui"
  },
  "components": [
    {
      "id": "button.primary",
      "figma": {
        "node_id": "123:456",
        "component_key": "abcd1234",
        "name": "Button / Primary",
        "variant": {
          "size": "md",
          "state": "default"
        }
      },
      "code": {
        "package": "@acme/ui",
        "file": "src/components/button.tsx",
        "export": "Button",
        "props": {
          "variant": "primary",
          "size": "md"
        }
      },
      "aliases": ["PrimaryButton", "BtnPrimary"],
      "tags": ["core", "cta"]
    }
  ]
}
```

### Field definitions

- `version` (string, required): Mapping format version. Only `"1.0"` in this spec.
- `project` (object, optional)
  - `name` (string, optional)
  - `repo` (string, optional) – informational
  - `design_system` (string, optional)
- `components` (array, required)
  - `id` (string, required): unique identifier in mapping file.
  - `figma` (object, required)
    - `node_id` (string, optional): Figma node id, preferred when known.
    - `component_key` (string, optional): Figma component key.
    - `name` (string, required): Figma component name.
    - `variant` (object, optional): key/value variant properties.
  - `code` (object, required)
    - `package` (string, optional): NPM package name or internal module label.
    - `file` (string, optional): repo-relative path.
    - `export` (string, required): export name or component identifier.
    - `props` (object, optional): fixed props used for mapping.
  - `aliases` (array of string, optional)
  - `tags` (array of string, optional)

Constraints:

- `components[*].id` must be unique.
- At least one of `figma.node_id` or `figma.component_key` or `figma.name` must be present. `figma.name` is required in this minimal spec.
- `code.export` is required. `code.file` is recommended but not required.

## MCP Tool API

All tools are read-only with respect to the codebase. They parse mapping files and compute matches.

Implementation note (figma-mcp): this spec is exposed as a single MCP tool:

- `figma_code_connect` with `mode=validate|index|match|list`

The mode names map 1:1 to the conceptual tools below.

### `figma_code_connect_index`

Parse mapping file, validate schema, and return an index summary.

Request params:

- `path` (string, optional): file path. If omitted, search default locations.
- `json` (string, optional): inline JSON content (takes precedence over `path`).
- `cache_key` (string, optional): caller-provided cache key. If omitted, hash of content.

Response:

- `ok` (bool)
- `index_id` (string)
- `component_count` (int)
- `warnings` (array)

### `figma_code_connect_validate`

Validate mapping file with detailed diagnostics.

Request params:

- `path` (string, optional)
- `json` (string, optional)

Response:

- `ok` (bool)
- `errors` (array of `{message, path}`)
- `warnings` (array of `{message, path}`)

### `figma_code_connect_match`

Match a Figma component or node to code components.

Request params:

- `index_id` (string, optional): from `figma_code_connect_index`. If omitted, default mapping is loaded.
- `node_id` (string, optional)
- `component_key` (string, optional)
- `name` (string, optional)
- `variant` (object, optional)
- `limit` (int, optional, default 3)

Response:

- `matches`: array of
  - `mapping_id`
  - `score` (0.0-1.0)
  - `reason` (string)
  - `code` (same shape as mapping)

### `figma_code_connect_list`

List all mapped components (for inspection/UI).

Request params:

- `index_id` (string, optional)

Response:

- `components`: array of `{id, figma, code, tags}`

### Optional: `figma_code_connect_suggest`

LLM-assisted suggestion from a Figma node bundle. Must be opt-in via env flag.

Request params:

- `node_bundle` (object) – output of `figma_get_node_bundle` (or subset)
- `limit` (int, optional)

Response:

- `suggestions` with rationale and estimated confidence.

## Matching Algorithm (Deterministic)

Input:

- Figma descriptor: `node_id`, `component_key`, `name`, `variant`.
- Mapping index: list of entries.

Algorithm (score 0.0-1.0, higher is better):

1. **Exact node_id** match: score 1.0, return immediately unless `limit > 1`.
2. **Exact component_key** match: score 0.95.
3. **Normalized name exact** (`lower`, remove spaces/slashes/dashes): score 0.85.
4. **Name token overlap** (Jaccard): base score 0.60-0.80 depending on overlap.
5. **Variant property match**: +0.02 per matching key/value (cap +0.10).
6. **Alias match**: +0.05 if any alias equals name.

Tie-breaking:

- Prefer entries with `code.file` present.
- Prefer entries with more matching variant keys.
- Deterministic sort by `id` as final tie-breaker.

Return top `limit` results with score and reason string.

## LLM-Optional Matching

If `FIGMA_MCP_CODE_CONNECT_LLM=1`, allow `figma_code_connect_suggest` to use an LLM for:

- inferring code component name from a Figma node bundle
- mapping variant property names to code props

LLM outputs must be post-validated against the mapping schema and never override deterministic matches.

## Test Plan (Minimal)

Add `test/test_code_connect.ml` with unit tests for:

1. **Load/validate**
   - Valid mapping file loads successfully.
   - Duplicate `components[*].id` yields error.
   - Missing `code.export` yields error.
2. **Exact match**
   - `node_id` match returns score 1.0 and correct mapping.
3. **Component key match**
   - `component_key` match returns score 0.95.
4. **Name normalization**
   - `"Button / Primary"` matches `"button-primary"`.
5. **Variant scoring**
   - Matching variant keys increases score predictably (cap enforced).
6. **Tie-breaks**
   - Two entries with equal score sort by `code.file` presence, then `id`.
7. **Limit behavior**
   - `limit` respected and deterministic ordering preserved.

Fixtures:

- Add minimal mapping fixtures under `test/fixtures/code_connect/` with small JSON examples.

## Integration Hook (Optional)

Expose `figma_code_connect_match` as a post-processing step in `figma_get_node_bundle` output to provide `code_candidates` without modifying the core DSL structure.
