(** Figma MCP Tools (aggregated) *)

include Mcp_tools_defs
include Mcp_tools_handlers

(** ============== 서버 생성 ============== *)

let create_figma_server () =
  Mcp_protocol.create_server ~handlers_sync:all_handlers_sync all_tools resources prompts read_resource
