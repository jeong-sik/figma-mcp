open Figma_mcp_protocol
open Mcp_helpers
open Mcp_tool_handlers

let all_handlers_sync : (string * tool_handler_sync) list =
  [
    ("figma_get_design_context", wrap_sync_pure Mcp_v2_handlers.handle_get_design_context);
    ("figma_get_metadata", wrap_sync_pure Mcp_v2_handlers.handle_get_metadata);
    ("figma_get_variable_defs", wrap_sync_pure Mcp_v2_handlers.handle_get_variable_defs);
    ("figma_get_screenshot", wrap_sync_pure Mcp_v2_handlers.handle_get_screenshot);
    ("figma_get_code_connect_map", wrap_sync_pure Mcp_v2_handlers.handle_get_code_connect_map);
    ("figma_whoami", wrap_sync_pure Mcp_v2_handlers.handle_whoami);
    ("figma_verify_semantic", wrap_sync_pure Mcp_v2_handlers.handle_verify_semantic);
    ("figma_verify_visual", wrap_sync_pure Mcp_v2_handlers.handle_verify_visual);
  ]

let () =
  List.iter (fun (name, sync_handler) -> register_handler name sync_handler) all_handlers_sync

let resources : mcp_resource list =
  [
    {
      uri = "figma://docs/v2-surface";
      name = "V2 Surface";
      description = "v2 MCP surface and behavioral boundaries.";
      mime_type = "text/markdown";
    };
    {
      uri = "figma://docs/verification";
      name = "Verification";
      description = "Semantic and visual verification workflow.";
      mime_type = "text/markdown";
    };
  ]

let resource_templates : mcp_resource_template list = []
let prompts : mcp_prompt list = []

let read_resource uri =
  match uri with
  | "figma://docs/v2-surface" ->
      Ok
        ( "text/markdown",
          String.concat "\n"
            [
              "# figma-mcp v2";
              "";
              "- Server scope: design context extraction and verification only.";
              "- Agent orchestration is out of process.";
              "- Public tools: figma_get_design_context, figma_get_metadata, figma_get_variable_defs, figma_get_screenshot, figma_get_code_connect_map, figma_whoami, figma_verify_semantic, figma_verify_visual.";
            ] )
  | "figma://docs/verification" ->
      Ok
        ( "text/markdown",
          String.concat "\n"
            [
              "# Verification";
              "";
              "1. Use figma_get_design_context or figma_get_metadata to scope the node.";
              "2. Implement or render HTML.";
              "3. Run figma_verify_semantic first.";
              "4. Run figma_verify_visual when semantic checks pass or visual parity matters.";
            ] )
  | _ -> Error "Resource not found"

let public_tools = Mcp_v2_tool_defs.public_tools

let create_figma_server () =
  Figma_mcp_protocol.create_server ~handlers_sync:all_handlers_sync ~resource_templates
    public_tools resources prompts read_resource
