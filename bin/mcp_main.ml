(** Figma MCP Server - stdio 진입점 *)

let () =
  let server = Figma_mcp.Tools.create_figma_server () in
  Mcp_protocol_server.start_stdio_server server
