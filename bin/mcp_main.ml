(** Figma MCP Server - stdio 진입점 *)

let () =
  let server = Figma_mcp.Tools.create_figma_server () in
  Figma_mcp.Protocol.run_stdio_server server
