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

  let json ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept");
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let not_found reqd =
    text ~status:`Not_found "404 Not Found" reqd

  let cors_preflight reqd =
    let headers = Httpun.Headers.of_list [
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept");
      ("content-length", "0");
    ] in
    let response = Httpun.Response.create ~headers `No_content in
    Httpun.Reqd.respond_with_string reqd response ""

  (** SSE streaming response for MCP streamable-http protocol *)
  let sse_stream reqd ~on_write =
    let headers = Httpun.Headers.of_list [
      ("content-type", "text/event-stream");
      ("cache-control", "no-cache");
      ("connection", "keep-alive");
      ("access-control-allow-origin", "*");
    ] in
    let response = Httpun.Response.create ~headers `OK in
    let body = Httpun.Reqd.respond_with_streaming reqd response in
    on_write body
end

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
let process_mcp_request_sync (server : Mcp_protocol.mcp_server) body_str =
  (* HTTP 모드 설정 - wrap_sync에서 nested Lwt_main.run 방지 *)
  Mcp_tools.is_http_mode := true;
  match Mcp_protocol.parse_request body_str with
  | Ok req ->
      (* HTTP 모드에서는 wrap_sync가 Effect 없이 직접 실행하므로
         여기서 한 번만 Lwt_main.run 호출 *)
      let response_json = Figma_effects.run_with_real_api (fun () ->
        Lwt_main.run (Mcp_protocol.process_request server req)
      ) in
      Yojson.Safe.to_string response_json
  | Error msg ->
      let err_response = Mcp_protocol.make_error_response
        `Null Mcp_protocol.parse_error msg None in
      Yojson.Safe.to_string err_response

(** ============== SSE Helpers ============== *)

(** Send SSE event and flush immediately *)
let send_sse_event body ~event ~data =
  let msg = sprintf "event: %s\ndata: %s\n\n" event data in
  Httpun.Body.Writer.write_string body msg;
  Httpun.Body.Writer.flush body ignore

(** ============== HTTP Handlers ============== *)

let health_handler _request reqd =
  let json = sprintf {|{"status":"ok","server":"%s","version":"%s","protocol":"%s"}|}
    Mcp_protocol.server_name
    Mcp_protocol.server_version
    Mcp_protocol.protocol_version
  in
  Response.json json reqd

(** MCP POST handler - async body reading with callback-based response *)
let mcp_post_handler server _request reqd =
  Request.read_body_async reqd (fun body_str ->
    let response_str = process_mcp_request_sync server body_str in
    Response.json response_str reqd
  )

(** MCP SSE handler for streamable-http protocol (GET /mcp) *)
let mcp_sse_handler ~clock _request reqd =
  Response.sse_stream reqd ~on_write:(fun body ->
    (* Send initial endpoint event (MCP protocol requirement) *)
    send_sse_event body ~event:"endpoint" ~data:"/mcp";

    (* Keep connection alive with periodic pings *)
    let rec ping_loop () =
      try
        Eio.Time.sleep clock 15.0;
        let timestamp = string_of_float (Unix.gettimeofday ()) in
        send_sse_event body ~event:"ping" ~data:timestamp;
        ping_loop ()
      with _ ->
        (* Client disconnected or error - close the body *)
        Httpun.Body.Writer.close body
    in
    ping_loop ()
  )

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

let route_request ~clock server request reqd =
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
      (* SSE stream for MCP streamable-http protocol *)
      mcp_sse_handler ~clock request reqd

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

let make_request_handler ~clock server =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    route_request ~clock server request reqd

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
let run ~sw ~net ~clock config server =
  let request_handler = make_request_handler ~clock server in
  let ip = match Ipaddr.of_string config.host with
    | Ok addr -> Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets addr)
    | Error _ -> Eio.Net.Ipaddr.V4.loopback
  in
  let addr = `Tcp (ip, config.port) in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:config.max_connections addr in

  eprintf "🎨 %s MCP Server (Eio)\n" Mcp_protocol.server_name;
  eprintf "   Protocol: %s\n" Mcp_protocol.protocol_version;
  eprintf "   HTTP:     http://%s:%d\n" config.host config.port;
  eprintf "   MCP:      GET  /mcp -> SSE stream (streamable-http)\n";
  eprintf "             POST /mcp -> JSON-RPC requests\n%!";

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
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  run ~sw ~net ~clock config server
