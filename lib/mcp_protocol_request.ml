open Printf

module Sse_transport = Mcp_sse_transport
(* ============== Request/Response Helpers ============== *)
(* Re-exported from extracted modules — see mcp_cors.ml, mcp_http_helpers.ml *)
module Cors = Mcp_cors
module Response = Mcp_http_helpers.Response
module Request = Mcp_http_helpers.Request

(** ============== MCP Request Processing ============== *)

(** Process MCP request synchronously (Eio-native, no Lwt).
    Canonical MCP method semantics are delegated to the shared SDK adapter. *)
let process_mcp_request_sync (_server : Figma_mcp_protocol.mcp_server) body_str =
  match Mcp_sdk_adapter_figma.process_jsonrpc body_str with
  | Some response -> response
  | None -> ""

type mcp_message_kind =
  [ `Request | `Notification | `Response | `Unknown ]

let classify_message body_str =
  match Yojson.Safe.from_string body_str with
  | exception (Yojson.Json_error _) -> `Unknown
  | `Assoc fields ->
      let has_method = List.mem_assoc "method" fields in
      let id = List.assoc_opt "id" fields in
      let has_result = List.mem_assoc "result" fields in
      let has_error = List.mem_assoc "error" fields in
      (match has_method, id with
       | true, None
       | true, Some `Null -> `Notification
       | true, Some _ -> `Request
       | false, Some _ when has_result || has_error -> `Response
       | _ -> `Unknown)
  | _ -> `Unknown

(* SSE transport types, client registry, and helpers — see mcp_sse_transport.ml *)
include Mcp_sse_transport

(** ============== HTTP Handlers ============== *)

let health_handler _request reqd =
  let json = sprintf {|{"status":"ok","server":"%s","version":"%s","protocol":"%s"}|}
    Figma_mcp_protocol.server_name
    Figma_mcp_protocol.server_version
    Figma_mcp_protocol.protocol_version
  in
  Response.json json reqd

(** MCP POST handler - async body reading with callback-based response *)
let run_mcp_request ~domain_mgr ~eio_ctx server body_str =
  let run () =
    Mcp_helpers.install_eio_context eio_ctx;
    process_mcp_request_sync server body_str
  in
  match domain_mgr with
  | None -> run ()
  | Some mgr -> Eio.Domain_manager.run mgr run

let mcp_post_handler ~sw ~domain_mgr ~eio_ctx server request reqd =
  let { Httpun.Request.headers; target = request_target; _ } = request in
  let header_first keys =
    let rec loop = function
      | [] -> None
      | key :: rest ->
          (match Httpun.Headers.get headers key with
           | Some value -> Some value
           | None -> loop rest)
    in
    loop keys
  in
  let query_first keys =
    let uri = Uri.of_string request_target in
    let rec loop = function
      | [] -> None
      | key :: rest ->
          (match Uri.get_query_param uri key with
           | Some value -> Some value
           | None -> loop rest)
    in
    loop keys
  in
  let client_id =
    let raw =
      match header_first [
        "mcp-client-id";
        "x-mcp-client-id";
        "mcp-session";
        "mcp-session-id";
      ] with
      | Some value -> Some value
      | None ->
          query_first [
            "client_id";
            "clientId";
            "session";
            "session_id";
            "mcp_session";
          ]
    in
    match raw with
    | Some value -> int_of_string_opt value
    | None -> None
  in
  Request.read_body_async reqd (fun body_str ->
    match classify_message body_str with
    | `Notification ->
        Eio.Fiber.fork ~sw (fun () ->
          try
            ignore (run_mcp_request ~domain_mgr ~eio_ctx server body_str)
          with exn ->
            eprintf "[MCP] notification failed: %s\n%!" (Printexc.to_string exn));
        Response.accepted reqd
    | `Response ->
        Response.accepted reqd
    | `Request | `Unknown ->
        (match Sse_transport.find_sse_client client_id with
         | Some (id, client) ->
             Response.accepted reqd;
             Eio.Fiber.fork ~sw (fun () ->
               try
                 let response_str = run_mcp_request ~domain_mgr ~eio_ctx server body_str in
                 Sse_transport.send_sse_event client ~event:"message"
                   ~data:response_str
               with exn ->
                 eprintf "[MCP] SSE request failed (client=%d): %s\n%!" id (Printexc.to_string exn);
                 Sse_transport.unregister_sse_client id)
         | None ->
             (* Check Accept header for SSE support (MCP Streamable HTTP) *)
             let wants_sse = Request.accepts_sse request in
             (try
               let response_str = run_mcp_request ~domain_mgr ~eio_ctx server body_str in
               if wants_sse then
                 Response.sse_message response_str reqd
               else
                 Response.json response_str reqd
             with exn ->
               eprintf "[MCP] request failed: %s\n%!" (Printexc.to_string exn);
                let err = Figma_mcp_protocol.make_error_response `Null
                  Figma_mcp_protocol.internal_error "Internal server error" None in
                if wants_sse then
                  Response.sse_message (Yojson.Safe.to_string err) reqd
                else
                  Response.json ~status:`Internal_server_error
                    (Yojson.Safe.to_string err) reqd)))
