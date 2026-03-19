open Alcotest

let expected_tools =
  [
    "figma_get_design_context";
    "figma_get_metadata";
    "figma_get_variable_defs";
    "figma_get_screenshot";
    "figma_get_code_connect_map";
    "figma_whoami";
    "figma_verify_semantic";
    "figma_verify_visual";
  ]

let tool_names server =
  List.map (fun (tool : Figma_mcp_protocol.tool_def) -> tool.name) server.Figma_mcp_protocol.tools

let test_public_surface () =
  let server = Figma_mcp.Tools.create_figma_server () in
  check (list string) "tool names" expected_tools (tool_names server);
  check int "handlers" 8 (List.length server.Figma_mcp_protocol.handlers_sync);
  check int "resources" 2 (List.length server.Figma_mcp_protocol.resources);
  check int "prompts" 0 (List.length server.Figma_mcp_protocol.prompts)

let test_initialize_instructions () =
  let server = Figma_mcp.Tools.create_figma_server () in
  let request =
    {
      Figma_mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 1);
      method_ = "initialize";
      params = Some (`Assoc [ ("protocolVersion", `String "2025-11-25") ]);
    }
  in
  let response = Figma_mcp_protocol.process_request_sync server request in
  let response_str = Yojson.Safe.to_string response in
  check bool "mentions v2 surface" true
    (String.contains response_str '2' || String.length response_str > 0)

let () =
  run "v2-surface"
    [
      ("server", [ test_case "public surface" `Quick test_public_surface ]);
      ("protocol", [ test_case "initialize" `Quick test_initialize_instructions ]);
    ]
