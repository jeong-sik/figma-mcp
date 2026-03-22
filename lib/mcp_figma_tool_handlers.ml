(** Figma tool HTTP handlers -- thin facade.

    Core helpers and shared state live in Mcp_figma_handlers_common.
    Handler implementations are split into domain modules:
    - Mcp_figma_handlers_codegen  (template, plugin codegen/analyze, multi-platform)
    - Mcp_figma_handlers_extract  (design tokens, variants, animations)
    - Mcp_figma_handlers_design   (storybook, responsive, a11y, asset export)
    - Mcp_figma_handlers_convert  (webhook, code-to-figma, vision compare)

    This facade re-exports everything for backward compatibility:
    callers that open/include Mcp_figma_tool_handlers get every symbol. *)

include Mcp_figma_handlers_common
include Mcp_figma_handlers_codegen
include Mcp_figma_handlers_extract
include Mcp_figma_handlers_design
include Mcp_figma_handlers_convert
