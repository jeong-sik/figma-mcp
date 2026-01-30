# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.5.2] - 2026-01-30

### Changed
- `hints_to_json` → `correction_hints_to_json` (더 명확한 네이밍)
- Docstring + 사용 예시 추가

## [0.5.1] - 2026-01-30

### Changed
- LLM 오버엔지니어링 제거 (-70줄)
- JSON 힌트 반환에 집중 (MCP 철학: Tool은 데이터, Agent는 지능)

### Removed
- `hints_to_enhanced_summary` (불필요한 LLM 통합)
- `Figma_config.Llm` 확장 설정들

## [0.5.0] - 2026-01-30

### Added
- **피드백 루프 시스템**
  - SSIM 로그: `log_verification`, `log_improvement`, `log_hint_application`
  - 토큰 중복 경고: Delta-E (CIEDE2000) 기반 색상 유사도 체크
  - 자연어 힌트: `hint_to_description`, `hints_to_summary`

- **Category Tool 시스템** (60 → 15 visible tools)
  - `figma_core`, `figma_visual`, `figma_plugin`, `figma_team`
  - `figma_export`, `figma_components` + 9 featured tools
  - 다이브 패턴: category tool 호출 → 개별 tool 선택

### Changed
- MCP Protocol: deprecated 필드 자동 감지 (`[DEPRECATED]` prefix)

## [0.3.16] - 2026-01-29

### Changed
- STRAP pattern: Consolidate 8 plugin tools → 1 unified `figma_plugin` tool
- Tool count: 51 → 44 (14% reduction, ~14% context savings)

### Plugin Actions
- `connect`, `use_channel`, `status`, `read_selection`
- `get_node`, `export_image`, `get_variables`, `apply_ops`

### Sources
- [STRAP Pattern](https://almatuck.com/articles/reduced-mcp-tools-96-to-10-strap-pattern)
- [MCP Spec 2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25)

## [0.3.15] - 2026-01-29

### Improved
- Add consistent category markers to all 56 tools (Tool Budget optimization)
- Categories: 🎯 CORE, 📦 BATCH, ✅ VERIFY, 📋 QUICK, 🔌 PLUGIN, 🖼️ ASSET, 📊 REPORT, 🔧 UTIL, [Advanced]
- Compress tool descriptions (avg 30% reduction)

### Sources
- "Less is More" MCP design patterns (Klavis)
- Docker MCP best practices
- Figma official MCP server docs

## [0.3.14] - 2026-01-29

### Improved
- Add `get_int_or`, `get_int_positive`, `get_int_nonneg` helper functions
- Replace 14 instances of verbose match patterns with concise helpers
- Remove unused `required` parameter workaround from `string_prop`

## [0.3.13] - 2026-01-29

### Improved
- Tool descriptions with emoji markers (🎯 CORE, 📦 RECOMMENDED, ✅ VERIFY)
- MCP Instructions: Parse, Don't Validate principle (always start with `figma_parse_url`)
- MCP Instructions: Error prevention checklist before API calls
- MCP Instructions: Tool selection guide table
- MCP Instructions: Common errors and solutions table
- MCP Instructions: Simplified 3-step error recovery workflow

### Added
- New prompt `figma_error_troubleshoot` for systematic error diagnosis
- Core principles summary section (Best Programmer Principles)
- `figma_verify_visual` detailed usage guide with code example

## [0.3.12] - 2026-01-29

### Improved
- Data-driven error suggestions with `first_match` pattern
- `body_contains`, `body_contains_any` helper functions
- Cleaner separation: `suggestion_for_400/403/404`

## [0.3.11] - 2026-01-29

### Fixed
- Include Figma API response body in error messages for better debugging (max 200 chars)
- Raise ulimit in startup scripts to prevent EMFILE accept crashes under high load

## [0.3.10] - 2026-01-28

### Changed
- Drop external `mcp_protocol` dependency in favor of local protocol module to avoid pin conflicts.

### Fixed
- Close gRPC server streams after writing payloads to prevent client hangs.
- Record `Result.Error` outcomes as circuit failures in LLM MCP retry flow.
- Clamp `figma_select_nodes` summary depth/preview scale and emit warnings on clamp.
- Run Lwt-based Figma API calls in a separate Eio domain in the example tool executor.

### Fixed
- Ensure cache directory creation works for nested paths (mkdir_p).
- Sanitize file_key when building asset paths to avoid unsafe characters.

## [0.3.5] - 2026-01-27

### Fixed
- Release workflow: remove unused `compact-protocol` and `mcp_protocol` pins to avoid GitHub Actions auth failures on tag builds.

## [0.3.4] - 2026-01-27

### Fixed
- Normalize node IDs (`:` vs `-`) and resolve nodes robustly across API responses.
- Fix `figma_get_node_summary` to avoid empty results and parse children safely.
- Fix `figma_get_node_bundle` to use the resolved node key when looking up documents and images.
- Add `summary_only` and `max_inline_bytes` handling in `figma_fidelity_loop`, and persist oversized full results to disk while returning a compact summary.

## [0.1.0] - 2026-01-18

### Added
- Initial release
- 43 Figma API tools
- Fidelity DSL output format
- Plugin channel support for real-time sync
- Visual verification tools (SSIM comparison)
- Evolution/iteration workflow for design refinement
- Resources: fidelity docs, usage guide
- Prompts: fidelity review

### Technical
- OCaml 5.x native implementation
- MCP 2025-11-25 spec compliance
- Fidelity-first design principle
- Type-safe Figma JSON parsing
