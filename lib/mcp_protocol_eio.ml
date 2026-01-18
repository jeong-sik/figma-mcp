(** MCP Protocol Eio - httpun-eio 기반 HTTP 서버

    Eio-native HTTP server for MCP protocol.
    Uses lwt_eio bridge for Figma API calls (cohttp-lwt-unix).

    Architecture:
    - Server: httpun-eio (Eio native, Effect-based)
    - Client: cohttp-lwt-unix via lwt_eio bridge
    - JSON-RPC: Reuses types from mcp_protocol.ml
*)

open Printf

(** ============== Server Configuration ============== *)

type config = {
  port: int;
  host: string;
  max_connections: int;
}

let default_config = {
  port = 8933;
  host = "127.0.0.1";
  max_connections = 64;
}

(** ============== Request/Response Helpers ============== *)

module Response = struct
  let text ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("access-control-allow-origin", "*");
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let json ?(status = `OK) ?(extra_headers = []) body reqd =
    let headers = Httpun.Headers.of_list (
      [
        ("content-type", "application/json; charset=utf-8");
        ("content-length", string_of_int (String.length body));
        ("access-control-allow-origin", "*");
        ("access-control-allow-methods", "GET, POST, OPTIONS");
        ("access-control-allow-headers", "Content-Type, Accept, MCP-Session-Id, MCP-Protocol-Version");
      ] @ extra_headers
    ) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let sse ?(status = `OK) ?(extra_headers = []) reqd =
    let headers = Httpun.Headers.of_list (
      [
        ("content-type", "text/event-stream; charset=utf-8");
        ("cache-control", "no-cache, no-store, must-revalidate");
        ("connection", "keep-alive");
        ("x-accel-buffering", "no");
        ("access-control-allow-origin", "*");
        ("access-control-allow-methods", "GET, POST, OPTIONS");
        ("access-control-allow-headers", "Content-Type, Accept, MCP-Session-Id, MCP-Protocol-Version");
      ] @ extra_headers
    ) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_streaming reqd response

  let not_found reqd =
    text ~status:`Not_found "404 Not Found" reqd

  let cors_preflight reqd =
    let headers = Httpun.Headers.of_list [
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, MCP-Session-Id, MCP-Protocol-Version");
      ("content-length", "0");
    ] in
    let response = Httpun.Response.create ~headers `No_content in
    Httpun.Reqd.respond_with_string reqd response ""
end

module Sse = struct
  let format_event ?event_type ?event_id ?retry_ms data =
    let buf = Buffer.create 256 in
    (match event_type with
     | Some t -> Buffer.add_string buf (Printf.sprintf "event: %s\n" t)
     | None -> ());
    (match event_id with
     | Some id -> Buffer.add_string buf (Printf.sprintf "id: %s\n" id)
     | None -> ());
    (match retry_ms with
     | Some ms -> Buffer.add_string buf (Printf.sprintf "retry: %d\n" ms)
     | None -> ());
    let lines = String.split_on_char '\n' data in
    List.iter (fun line ->
      Buffer.add_string buf (Printf.sprintf "data: %s\n" line)
    ) lines;
    Buffer.add_string buf "\n";
    Buffer.contents buf

  let prime_event ?(retry_ms = 5000) () =
    Printf.sprintf "retry: %d\n\n" retry_ms
end

let contains_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop idx =
    if idx + needle_len > haystack_len then false
    else if String.sub haystack idx needle_len = needle then true
    else loop (idx + 1)
  in
  if needle_len = 0 then true else loop 0

let header_value (request : Httpun.Request.t) name =
  Httpun.Headers.get request.headers name

let accepts_mime request mime =
  match header_value request "accept" with
  | None -> false
  | Some value ->
      let value = String.lowercase_ascii value in
      let mime = String.lowercase_ascii mime in
      contains_substring value mime

let accepts_streamable_mcp request =
  accepts_mime request "application/json" && accepts_mime request "text/event-stream"

let generate_session_id () =
  let ts = int_of_float (Unix.gettimeofday () *. 1000.) mod 1000000 in
  Printf.sprintf "s%d-%d" ts (Random.int 10000)

let get_session_id request =
  match header_value request "mcp-session-id" with
  | Some id -> Some id
  | None -> header_value request "x-mcp-session-id"

let get_protocol_header request =
  header_value request "mcp-protocol-version"

let mcp_headers ~session_id ~protocol_version =
  [
    ("mcp-session-id", session_id);
    ("mcp-protocol-version", protocol_version);
  ]

module Request = struct
  (** Read request body - accumulates chunks until EOF.
      Uses callback pattern - the response MUST be sent from within the callback. *)
  let read_body_async reqd callback =
    let body = Httpun.Reqd.request_body reqd in
    let chunks = ref [] in
    let rec read_loop () =
      Httpun.Body.Reader.schedule_read body
        ~on_eof:(fun () ->
          callback (String.concat "" (List.rev !chunks)))
        ~on_read:(fun buffer ~off ~len ->
          let chunk = Bigstringaf.substring buffer ~off ~len in
          chunks := chunk :: !chunks;
          read_loop ())
    in
    read_loop ()

  let path (request : Httpun.Request.t) =
    request.target |> String.split_on_char '?' |> List.hd

  let method_ (request : Httpun.Request.t) =
    request.meth
end

(** ============== MCP Request Processing ============== *)

(** Process MCP request synchronously.
    Uses Lwt_main.run for Figma API calls (safe in httpun callback). *)
type mcp_http_response = {
  body: string;
  protocol_version: string;
}

let process_mcp_request_sync (server : Mcp_protocol.mcp_server) body_str =
  (* HTTP 모드 설정 - wrap_sync에서 nested Lwt_main.run 방지 *)
  Mcp_tools.is_http_mode := true;
  match Mcp_protocol.parse_request body_str with
  | Ok req ->
      let negotiated_version =
        Mcp_protocol.protocol_version_from_params req.params
        |> Mcp_protocol.normalize_protocol_version
      in
      (* HTTP 모드에서는 wrap_sync가 Effect 없이 직접 실행하므로
         여기서 한 번만 Lwt_main.run 호출 *)
      let response_json = Figma_effects.run_with_real_api (fun () ->
        Lwt_main.run (Mcp_protocol.process_request server req)
      ) in
      {
        body = Yojson.Safe.to_string response_json;
        protocol_version = negotiated_version;
      }
  | Error msg ->
      let err_response = Mcp_protocol.make_error_response
        `Null Mcp_protocol.parse_error msg None in
      {
        body = Yojson.Safe.to_string err_response;
        protocol_version = Mcp_protocol.default_protocol_version;
      }

(** ============== HTTP Handlers ============== *)

let health_handler _request reqd =
  let json = sprintf {|{"status":"ok","server":"%s","version":"%s","protocol":"%s"}|}
    Mcp_protocol.server_name
    Mcp_protocol.server_version
    Mcp_protocol.protocol_version
  in
  Response.json json reqd

(** MCP POST handler - async body reading with callback-based response *)
let mcp_post_handler server request reqd =
  let session_id =
    match get_session_id request with
    | Some id -> id
    | None -> generate_session_id ()
  in
  Request.read_body_async reqd (fun body_str ->
    let response = process_mcp_request_sync server body_str in
    let extra_headers = mcp_headers ~session_id ~protocol_version:response.protocol_version in
    if accepts_streamable_mcp request then
      let writer = Response.sse ~extra_headers reqd in
      let prime = Sse.prime_event () in
      let event = Sse.format_event response.body in
      Httpun.Body.Writer.write_string writer prime;
      Httpun.Body.Writer.write_string writer event;
      Httpun.Body.Writer.close writer
    else
      Response.json ~extra_headers response.body reqd
  )

let mcp_sse_handler _server request reqd =
  let session_id =
    match get_session_id request with
    | Some id -> id
    | None -> generate_session_id ()
  in
  let protocol_version =
    match get_protocol_header request with
    | Some v -> Mcp_protocol.normalize_protocol_version v
    | None -> Mcp_protocol.default_protocol_version
  in
  let extra_headers = mcp_headers ~session_id ~protocol_version in
  let writer = Response.sse ~extra_headers reqd in
  let connected_data =
    Printf.sprintf {|{"session_id":"%s","protocol_version":"%s"}|}
      session_id protocol_version
  in
  Httpun.Body.Writer.write_string writer (Sse.prime_event ());
  Httpun.Body.Writer.write_string writer (Sse.format_event ~event_type:"connected" connected_data);
  Httpun.Body.Writer.flush writer (fun () -> ())

(** ============== Plugin Bridge Handlers ============== *)

let json_error ?(status=`Bad_request) msg reqd =
  let body = Yojson.Safe.to_string (`Assoc [("error", `String msg)]) in
  Response.json ~status body reqd

let parse_json body_str =
  if String.trim body_str = "" then Ok `Null
  else
    try Ok (Yojson.Safe.from_string body_str)
    with Yojson.Json_error msg -> Error msg

let get_string_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) -> Some s
       | _ -> None)
  | _ -> None

let get_int_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Int i) -> Some i
       | Some (`Float f) -> Some (int_of_float f)
       | _ -> None)
  | _ -> None

let get_bool_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Bool b) -> Some b
       | _ -> None)
  | _ -> None

let get_payload_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some json -> Some json
       | _ -> None)
  | _ -> None

let plugin_connect_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let channel_id = get_string_field "channel_id" json in
        let channel_id = Figma_plugin_bridge.register_channel ?channel_id () in
        let body = `Assoc [
          ("status", `String "ok");
          ("channel_id", `String channel_id);
        ] in
        Response.json (Yojson.Safe.to_string body) reqd
  )

let plugin_poll_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        (match get_string_field "channel_id" json with
         | None -> json_error "Missing channel_id" reqd
         | Some channel_id ->
             let max_commands = get_int_field "max_commands" json |> Option.value ~default:1 in
             let commands : Figma_plugin_bridge.command list =
               Figma_plugin_bridge.poll_commands ~channel_id ~max:max_commands
             in
             let commands_json =
               `List (List.map (fun (cmd : Figma_plugin_bridge.command) ->
                 `Assoc [
                   ("id", `String cmd.id);
                   ("name", `String cmd.name);
                   ("payload", cmd.payload);
                   ("created_at", `Float cmd.created_at);
                 ]) commands)
             in
             let body = `Assoc [
               ("channel_id", `String channel_id);
               ("commands", commands_json);
             ] in
             Response.json (Yojson.Safe.to_string body) reqd))

let plugin_result_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let channel_id = get_string_field "channel_id" json in
        let command_id = get_string_field "command_id" json in
        let ok = get_bool_field "ok" json |> Option.value ~default:true in
        let payload = get_payload_field "payload" json |> Option.value ~default:`Null in
        (match (channel_id, command_id) with
         | (Some channel_id, Some command_id) ->
             Figma_plugin_bridge.store_result ~channel_id ~command_id ~ok ~payload;
             let body = `Assoc [("status", `String "ok")] in
             Response.json (Yojson.Safe.to_string body) reqd
         | _ ->
             json_error "Missing channel_id or command_id" reqd))

let plugin_status_handler _request reqd =
  let channels = Figma_plugin_bridge.list_channels () in
  let default_channel = Figma_plugin_bridge.get_default_channel () in
  let body = `Assoc [
    ("channels", `List (List.map (fun id -> `String id) channels));
    ("default_channel", match default_channel with Some id -> `String id | None -> `Null);
  ] in
  Response.json (Yojson.Safe.to_string body) reqd

(** ============== Router ============== *)

let route_request server request reqd =
  let path = Request.path request in
  let meth = Request.method_ request in

  match (meth, path) with
  | `OPTIONS, _ ->
      Response.cors_preflight reqd

  | `GET, "/health" ->
      health_handler request reqd

  | `GET, "/" ->
      Response.text (sprintf "🎨 %s MCP Server (Eio)" Mcp_protocol.server_name) reqd

  | `GET, "/mcp" ->
      mcp_sse_handler server request reqd

  | `GET, "/plugin/status" ->
      plugin_status_handler request reqd

  | `POST, "/" | `POST, "/mcp" ->
      mcp_post_handler server request reqd

  | `POST, "/plugin/connect" ->
      plugin_connect_handler request reqd

  | `POST, "/plugin/poll" ->
      plugin_poll_handler request reqd

  | `POST, "/plugin/result" ->
      plugin_result_handler request reqd

  | _ ->
      Response.not_found reqd

(** ============== httpun-eio Server ============== *)

let make_request_handler server =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    route_request server request reqd

let error_handler _client_addr ?request:_ error start_response =
  let response_body = start_response Httpun.Headers.empty in
  let msg = match error with
    | `Exn exn -> Printexc.to_string exn
    | `Bad_request -> "Bad Request"
    | `Bad_gateway -> "Bad Gateway"
    | `Internal_server_error -> "Internal Server Error"
  in
  Httpun.Body.Writer.write_string response_body msg;
  Httpun.Body.Writer.close response_body

(** Run HTTP server with Eio *)
let run ~sw ~net config server =
  let request_handler = make_request_handler server in
  let ip = match Ipaddr.of_string config.host with
    | Ok addr -> Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets addr)
    | Error _ -> Eio.Net.Ipaddr.V4.loopback
  in
  let addr = `Tcp (ip, config.port) in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:config.max_connections addr in

  eprintf "🎨 %s MCP Server (Eio)\n" Mcp_protocol.server_name;
  eprintf "   Protocol: %s\n" Mcp_protocol.protocol_version;
  eprintf "   HTTP:     http://%s:%d\n" config.host config.port;
  eprintf "   MCP:      http://%s:%d/mcp\n%!" config.host config.port;

  let rec accept_loop () =
    let flow, client_addr = Eio.Net.accept ~sw socket in
    Eio.Fiber.fork ~sw (fun () ->
      try
        Httpun_eio.Server.create_connection_handler
          ~sw
          ~request_handler
          ~error_handler
          client_addr
          flow
      with exn ->
        eprintf "[%s] Connection error: %s\n%!"
          Mcp_protocol.server_name
          (Printexc.to_string exn)
    );
    accept_loop ()
  in
  accept_loop ()

(** Start the server - entry point for main.ml *)
let start_server ?(config = default_config) server =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  run ~sw ~net config server
