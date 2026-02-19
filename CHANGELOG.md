# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.8.1] - 2026-02-19

### Added
- B1: CIEDE2000 tests with CIE official formula verification (#117)
- B2: IoU integration tests with academic references (#118)
- README updated with academic foundation, vs official MCP, and B1/B2 progress (#119)

## [0.8.0] - 2026-02-19

### Changed
- Decompose `mcp_protocol_eio.ml` into 5 modules: `mcp_cors.ml`, `mcp_http_helpers.ml`, `mcp_agent_queue.ml`, `mcp_sse_transport.ml`, `mcp_figma_tool_handlers.ml` — main module reduced from 4,007 to 1,074 LOC (#114)
- Replace 6 runtime `failwith` calls with typed `Result.Error` values (#110)

### Added
- Phase A5 coverage push: 261 new tests across 5 files (#115)
- bisect_ppx campaign: coverage from 51.1% to 87.99% (#113)
- Coverage wave 3: 5 test files, 5,186 lines, 165 new tests (#111)
- Coverage wave 4: 5 test files, 4,613 lines, 276 new tests (#112)
- README updated to reflect v0.7.3 state (#242c03d)

### Fixed
- Flaky test fixes and resilience/early_stop coverage improvements (#109)

## [0.7.3] - 2026-02-14

### Added
- In-flight request deduplication via Eio.Promise (#106)
- Eio.Semaphore concurrency limiter for Figma API calls (#104)
- Breadcrumb/ancestor path in search response (#107)
- 13 test files (7,516 lines) pushing bisect_ppx coverage from 29.6% to 51.1% (#108)

### Fixed
- Compact JSON to single line before SSE framing — fixes multi-line data bug (#105)
- MCP tool descriptions and array_prop schema (#103)

### Changed
- Default node cache TTL reduced from 24h to 8h (#102)

## [0.7.2] - 2026-02-10

### Changed
- Replace Unix.sleepf with Eio.Time.sleep, fix metrics lock docs (#97)

## [0.7.1] - 2026-02-09

### Added
- `.mli` interface files for module encapsulation (#96)

### Changed
- Modularize mcp_tools.ml: 9,013 → 3,090 lines (#95)

### Fixed
- Plugin lazy-init documentchange watcher with loadAllPagesAsync

## [0.7.0] - 2026-02-07

### Added
- Plugin feedback loop: viewport capture, selection export, change watcher
- Plugin create_instance handler + enriched channel status
- Plugin mutation tools, event subscription
- Figma search hardening: tokenize + score + JSON (#88)

### Fixed
- Plugin sandbox messaging and poll loop restart
- Plugin parent_id + fill support for all create_* handlers
- Expose plugin tools as monolithic instead of broken category
- Log realpath fallback in mcp_protocol_eio
- Harden remaining silent failures, add diagnostic logging

## [0.6.0] - 2026-02-05

### Added
- File cache for search + restore depth=4 (#92)
- Category tools describe mode + strict routing (#91)
- Semantic-first verifier tool (design IR vs DOM metrics) (#90)
- Variables tokens via MCP resources (#85)
- Code Connect mapping + match tool (#73)
- Strict/compat CORS profile defaults (#72)
- Security hardening + tokens export formats (#71)
- CI test workflow (#74)
- bisect_ppx code coverage instrumentation (#69)

### Fixed
- Restrict env token resolution, simplify args, enable domain_mgr
- Clamp max_commands to prevent unbounded plugin poll requests
- Avoid verify_visual tmp collisions (#84)
- SSE client_id within int range (#83)
- Harden SSE session id + redact URL queries (#75)
- Disable Domain_manager.run for MCP requests (#77)
- Search depth=4 in handle_search (#76)
- Forward flat arguments in hub category dispatch (#70)
- Prevent zombie SSE client accumulation on broadcast failure
- Plugin API key UI and CORS defaults (#67)
- Accept MCP_API_KEY for HTTP auth (#66)

## [0.5.4] - 2026-02-01

### Fixed
- Avoid polymorphic compare on `Httpun.Reqd.t` in server metrics tracking (prevents `/metrics` 500)

## [0.5.3] - 2026-02-01

### Added
- Agent queue hardening: claim/heartbeat TTL, priority, drift detection, max attempts
- Queue stats + server metrics endpoints (`/stats`, `/metrics`)
- SSE/HTTP response metrics aggregation
- `scripts/mcp-smoke.sh`

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
