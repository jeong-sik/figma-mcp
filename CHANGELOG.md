# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- Drop external `mcp_protocol` dependency in favor of local protocol module to avoid pin conflicts.

### Fixed
- Close gRPC server streams after writing payloads to prevent client hangs.
- Record `Result.Error` outcomes as circuit failures in LLM MCP retry flow.
- Clamp `figma_select_nodes` summary depth/preview scale and emit warnings on clamp.
- Run Lwt-based Figma API calls in a separate Eio domain in the example tool executor.

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
