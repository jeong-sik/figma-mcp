open Alcotest

let contains ~needle haystack =
  let re = Str.regexp_string needle in
  try
    ignore (Str.search_forward re haystack 0);
    true
  with Not_found -> false

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
  let open Yojson.Safe.Util in
  let instructions =
    response |> member "result" |> member "instructions" |> to_string
  in
  check bool "mentions design context" true
    (contains ~needle:"figma_get_design_context" instructions);
  check bool "mentions metadata" true
    (contains ~needle:"figma_get_metadata" instructions);
  check bool "omits removed parse tool" false
    (contains ~needle:"figma_parse_url" instructions);
  check bool "omits removed summary tool" false
    (contains ~needle:"figma_get_node_summary" instructions)

let () =
  run "v2-surface"
    [
      ("server", [ test_case "public surface" `Quick test_public_surface ]);
      ("protocol", [ test_case "initialize" `Quick test_initialize_instructions ]);
    ]
