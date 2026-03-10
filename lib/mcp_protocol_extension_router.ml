open Mcp_protocol_plugin_http
open Mcp_figma_tool_handlers

let handle_extension_route ~clock ~sw ~eio_ctx request reqd =
  match (Request.method_ request, Request.path request) with
  | `GET, "/plugin/status" ->
      plugin_status_handler request reqd;
      true
  | `POST, "/plugin/connect" ->
      plugin_connect_handler request reqd;
      true
  | `POST, "/plugin/poll" ->
      plugin_poll_handler ~clock request reqd;
      true
  | `POST, "/plugin/result" ->
      plugin_result_handler request reqd;
      true
  | `POST, "/plugin/event" ->
      plugin_event_handler request reqd;
      true
  | `POST, "/plugin/codegen" ->
      plugin_codegen_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/template" ->
      template_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/code-to-figma" ->
      code_to_figma_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/vision-compare" ->
      vision_compare_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/analyze" ->
      plugin_analyze_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/extract-tokens" ->
      extract_tokens_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/generate-story" ->
      generate_story_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/codegen-multi" ->
      codegen_multi_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/extract-variants" ->
      extract_variants_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/responsive-breakpoints" ->
      responsive_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/accessibility" ->
      accessibility_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/export-assets" ->
      export_assets_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/plugin/extract-animations" ->
      extract_animations_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/webhook/figma" ->
      webhook_handler ~sw ~eio_ctx request reqd;
      true
  | `POST, "/agent/request" ->
      agent_request_handler request reqd;
      true
  | `POST, "/agent/claim" ->
      agent_claim_handler request reqd;
      true
  | `POST, "/agent/heartbeat" ->
      agent_heartbeat_handler request reqd;
      true
  | `POST, "/agent/abandon" ->
      agent_abandon_handler request reqd;
      true
  | `GET, "/agent/pending" ->
      agent_pending_handler request reqd;
      true
  | `POST, "/agent/result" ->
      agent_result_handler request reqd;
      true
  | `GET, path
    when String.length path > 14 && String.sub path 0 14 = "/agent/status/" ->
      agent_status_handler request reqd;
      true
  | `GET, "/agent/queue" ->
      agent_queue_handler request reqd;
      true
  | _ -> false
