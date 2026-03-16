open Printf
open Mcp_agent_queue
open Mcp_protocol_request
(* ============== Router ============== *)

let is_public_path meth path =
  match (meth, path) with
  | (`OPTIONS, _) -> true
  | (`GET, "/health") -> true
  | (`GET, "/metrics") -> true
  | _ -> false

let normalize_env value =
  match value with
  | None -> None
  | Some v ->
      let trimmed = String.trim v in
      if trimmed = "" then None else Some trimmed

let api_key_env_name () =
  match normalize_env (Sys.getenv_opt "FIGMA_MCP_API_KEY") with
  | Some _ -> "FIGMA_MCP_API_KEY"
  | None ->
      (match normalize_env (Sys.getenv_opt "MCP_API_KEY") with
       | Some _ -> "MCP_API_KEY"
       | None -> "FIGMA_MCP_API_KEY")

let check_api_key request =
  let env_name = api_key_env_name () in
  match Mcp_http_auth.check_api_key
          ~env_name
          ~allow_no_auth:!(Mcp_figma_tool_handlers.allow_no_auth)
          request.Httpun.Request.headers with
  | Ok () -> Ok ()
  | Error Mcp_http_auth.Missing -> Error "API key required"
  | Error Mcp_http_auth.Invalid -> Error "Invalid API key"

[@@@coverage off]
let route_request ~clock ~domain_mgr ~sw ~eio_ctx server request reqd =
  let path = Request.path request in
  let meth = Request.method_ request in
  let public_path = is_public_path meth path in

  if not (Cors.is_allowed reqd) then
    Response.text ~status:`Forbidden "Forbidden" reqd
  else
    let route () =
      match (meth, path) with
      | `OPTIONS, _ ->
          Response.cors_preflight reqd

      | `GET, "/health" ->
          health_handler request reqd

      | `GET, "/metrics" ->
          Response.text (Server_metrics.to_prometheus_text ()) reqd

      | `GET, "/stats" ->
          let result = `Assoc [
            ("server_metrics", Server_metrics.to_json ());
            ("agent_queue", agent_queue_stats_json ());
          ] in
          Response.json (Yojson.Safe.to_string result) reqd

      | `GET, "/" ->
          Response.text (sprintf "🎨 %s MCP Server (Eio)" Figma_mcp_protocol.server_name) reqd

      | `GET, "/mcp" ->
          (* SSE stream for MCP streamable-http protocol *)
          mcp_sse_handler ~clock request reqd

      | `POST, "/" | `POST, "/mcp" ->
          mcp_post_handler ~sw ~domain_mgr ~eio_ctx server request reqd

      | _ ->
          if
            Mcp_protocol_extension_router.handle_extension_route ~clock ~sw
              ~eio_ctx request reqd
          then ()
          else Response.not_found reqd
    in
    if public_path then
      route ()
    else
      match check_api_key request with
      | Ok () -> route ()
      | Error msg -> Response.api_key_error msg reqd

(** ============== httpun-eio Server ============== *)

let make_request_handler ~clock ~domain_mgr ~sw ~eio_ctx server =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    Server_metrics.register_reqd reqd request;
    try
      route_request ~clock ~domain_mgr ~sw ~eio_ctx server request reqd
    with exn ->
      eprintf "[http] request handler exception: %s\n%!" (Printexc.to_string exn);
      Response.text ~status:`Internal_server_error "Internal Server Error" reqd

let error_handler _client_addr ?request error start_response =
  let status =
    match error with
    | `Bad_request -> `Bad_request
    | `Bad_gateway -> `Bad_gateway
    | `Internal_server_error -> `Internal_server_error
    | `Exn _ -> `Internal_server_error
  in
  let msg =
    match error with
    | `Exn exn ->
        if Mcp_tools.is_network_error exn then
          eprintf "[http] client disconnected: %s\n%!" (Printexc.to_string exn)
        else
          eprintf "[http] error handler exception: %s\n%!" (Printexc.to_string exn);
        "Internal Server Error"
    | `Bad_request -> "Bad Request"
    | `Bad_gateway -> "Bad Gateway"
    | `Internal_server_error -> "Internal Server Error"
  in
  let origin_opt =
    match request with
    | None -> None
    | Some req -> Httpun.Headers.get req.Httpun.Request.headers "origin"
  in
  let cors_headers =
    Cors.headers_for_origin_opt origin_opt ~include_methods:true ~include_headers:true
  in
  let headers = Httpun.Headers.of_list ([
    ("content-type", "text/plain; charset=utf-8");
    ("content-length", string_of_int (String.length msg));
    ("connection", "close");
  ] @ cors_headers) in
  let response_body = start_response headers in
  Httpun.Body.Writer.write_string response_body msg;
  Httpun.Body.Writer.close response_body;
  Server_metrics.record_untracked_response ~bytes:(String.length msg) status
