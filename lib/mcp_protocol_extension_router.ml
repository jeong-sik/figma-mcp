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
  | _ -> false
