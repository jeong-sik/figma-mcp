(** Public facade for Figma MCP tools.

    Tool definitions, handlers, and registry assembly are split into
    dedicated modules to keep this entrypoint stable and small. *)

include Mcp_tool_defs
include Mcp_tool_handlers
include Mcp_tool_registry
