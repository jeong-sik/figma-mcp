(** Public facade for the Eio MCP transport.

    Request parsing, plugin/agent HTTP handlers, routing, and server lifecycle
    now live in dedicated modules. *)

include Mcp_agent_queue
include Mcp_figma_tool_handlers

include Mcp_protocol_request
include Mcp_protocol_plugin_http
include Mcp_protocol_router
include Mcp_protocol_server
