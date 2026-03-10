open Alcotest

module MT = Mcp_protocol.Mcp_types

let text_of_tool_result (result : MT.tool_result) =
  match result.content with
  | MT.TextContent { text; _ } :: _ -> text
  | _ -> ""

let test_public_tool_count () =
  let snapshot = Mcp_sdk_adapter_figma.make_snapshot () in
  check int "public tool count"
    (List.length Mcp_tool_defs.public_tools)
    (List.length snapshot.tools)

let test_resource_template_count () =
  let snapshot = Mcp_sdk_adapter_figma.make_snapshot () in
  check int "resource template count"
    (List.length Mcp_tool_registry.resource_templates)
    (List.length snapshot.resource_templates)

let test_prompt_count () =
  let snapshot = Mcp_sdk_adapter_figma.make_snapshot () in
  check int "prompt count"
    (List.length Mcp_tool_registry.prompts)
    (List.length snapshot.prompts)

let test_doctor_handler () =
  let snapshot = Mcp_sdk_adapter_figma.make_snapshot () in
  match List.find_opt
          (fun (binding : Mcp_sdk_adapter_figma.registered_tool) ->
            binding.tool.name = "figma_doctor")
          snapshot.tools with
  | None -> fail "missing figma_doctor adapter binding"
  | Some binding ->
      (match
         binding.handler Mcp_sdk_adapter_figma.noop_context binding.tool.name
           (Some (`Assoc []))
       with
      | Ok result ->
          let text = text_of_tool_result result in
          check bool "doctor response present" true (String.length text > 0)
      | Error msg -> fail msg)

let test_tokens_resource_handler () =
  let snapshot = Mcp_sdk_adapter_figma.make_snapshot () in
  match List.find_opt
          (fun (binding : Mcp_sdk_adapter_figma.registered_resource) ->
            binding.resource.uri = "figma://docs/tokens")
          snapshot.resources with
  | None -> fail "missing figma://docs/tokens resource binding"
  | Some binding ->
      (match binding.handler Mcp_sdk_adapter_figma.noop_context binding.resource.uri with
      | Ok [{ MT.uri; mime_type = Some mime_type; text = Some body; _ }] ->
          check string "resource uri" "figma://docs/tokens" uri;
          check string "mime" "text/markdown" mime_type;
          check bool "has body" true (String.length body > 0)
      | Ok _ -> fail "unexpected resource content shape"
      | Error msg -> fail msg)

let () =
  run "mcp_sdk_adapter_figma"
    [
      ("snapshot", [
        test_case "public tool count" `Quick test_public_tool_count;
        test_case "resource template count" `Quick test_resource_template_count;
        test_case "prompt count" `Quick test_prompt_count;
        test_case "doctor handler" `Quick test_doctor_handler;
        test_case "tokens resource handler" `Quick test_tokens_resource_handler;
      ]);
    ]
