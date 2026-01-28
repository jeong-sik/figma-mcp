(** MCP Protocol Eio - Pure Eio HTTP/stdio 서버

    Pure Eio-native server for MCP protocol.
    No Lwt dependencies - uses cohttp-eio for all HTTP operations.

    Architecture:
    - HTTP Server: httpun-eio (Eio native, Effect-based)
    - HTTP Client: cohttp-eio (Pure Eio)
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
  host = "localhost";
  max_connections = 64;
}

(** ============== Request/Response Helpers ============== *)

module Response = struct
  let text ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("access-control-allow-origin", "*");
      ("access-control-allow-private-network", "true");
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let json ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list [
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, Access-Control-Request-Private-Network");
      ("access-control-allow-private-network", "true");
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let accepted reqd =
    let headers = Httpun.Headers.of_list [
      ("content-length", "0");
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, Access-Control-Request-Private-Network");
      ("access-control-allow-private-network", "true");
    ] in
    let response = Httpun.Response.create ~headers `Accepted in
    Httpun.Reqd.respond_with_string reqd response ""

  let not_found reqd =
    text ~status:`Not_found "404 Not Found" reqd

  let cors_preflight reqd =
    let headers = Httpun.Headers.of_list [
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, Access-Control-Request-Private-Network");
      ("access-control-allow-private-network", "true");
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
      ("access-control-allow-private-network", "true");
    ] in
    let response = Httpun.Response.create ~headers `OK in
    let body = Httpun.Reqd.respond_with_streaming reqd response in
    on_write body

  (** SSE single message response for POST→SSE (MCP Streamable HTTP) *)
  let sse_message ?(session_id="") json_str reqd =
    let event_id = Printf.sprintf "s%d-%d" (Unix.getpid ()) (Random.int 10000) in
    let prime = Printf.sprintf "retry: 5000\nid: %s:2\n\n" event_id in
    let message = Printf.sprintf "id: %s:1\ndata: %s\n\n" event_id json_str in
    let body = prime ^ message in
    let session_headers = if session_id = "" then [] else [("mcp-session-id", session_id)] in
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/event-stream");
      ("content-length", string_of_int (String.length body));
      ("cache-control", "no-cache");
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, Access-Control-Request-Private-Network");
      ("access-control-allow-private-network", "true");
    ] @ session_headers) in
    let response = Httpun.Response.create ~headers `OK in
    Httpun.Reqd.respond_with_string reqd response body
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

  (** Check if client accepts SSE (MCP Streamable HTTP) *)
  let accepts_sse (request : Httpun.Request.t) =
    match Httpun.Headers.get request.headers "accept" with
    | Some accept ->
        let accept_lower = String.lowercase_ascii accept in
        (try
          let _ = Str.search_forward (Str.regexp_string "text/event-stream") accept_lower 0 in
          true
        with Not_found -> false)
    | None -> false
end

(** ============== MCP Request Processing ============== *)

(** Process MCP request synchronously (Eio-native, no Lwt).
    Uses process_request_sync which calls handlers_sync directly. *)
let process_mcp_request_sync (server : Mcp_protocol.mcp_server) body_str =
  match Mcp_protocol.parse_request body_str with
  | Ok req ->
      (* process_request_sync: Lwt 없이 직접 실행 *)
      let response_json = Mcp_protocol.process_request_sync server req in
      Yojson.Safe.to_string response_json
  | Error msg ->
      let err_response = Mcp_protocol.make_error_response
        `Null Mcp_protocol.parse_error msg None in
      Yojson.Safe.to_string err_response

type mcp_message_kind =
  [ `Request | `Notification | `Response | `Unknown ]

let classify_message body_str =
  match Yojson.Safe.from_string body_str with
  | exception _ -> `Unknown
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

(** ============== SSE Helpers ============== *)

(** SSE client registry for shutdown notification *)
type sse_client = {
  body: Httpun.Body.Writer.t;
  mutex: Eio.Mutex.t;
  mutable connected: bool;
}
let sse_clients : (int, sse_client) Hashtbl.t = Hashtbl.create 16
let sse_client_counter = ref 0

let format_sse_data data =
  if data = "" then
    "data: "
  else
    data
    |> String.split_on_char '\n'
    |> List.map (fun line -> "data: " ^ line)
    |> String.concat "\n"

let register_sse_client body =
  incr sse_client_counter;
  let id = !sse_client_counter in
  let client = { body; mutex = Eio.Mutex.create (); connected = true } in
  Hashtbl.add sse_clients id client;
  (id, client)

let unregister_sse_client id =
  (match Hashtbl.find_opt sse_clients id with
   | Some c -> c.connected <- false
   | None -> ());
  Hashtbl.remove sse_clients id

(** Send SSE event and flush immediately *)
let send_sse_event client ~event ~data =
  let data_lines = format_sse_data data in
  let msg = sprintf "event: %s\n%s\n\n" event data_lines in
  Eio.Mutex.use_rw ~protect:true client.mutex (fun () ->
    Httpun.Body.Writer.write_string client.body msg;
    Httpun.Body.Writer.flush client.body ignore
  )

let broadcast_sse_shutdown reason =
  let data = sprintf
    {|{"jsonrpc":"2.0","method":"notifications/shutdown","params":{"reason":"%s","message":"Server is shutting down, please reconnect"}}|}
    reason
  in
  Hashtbl.iter (fun _ client ->
    if client.connected then
      try
        send_sse_event client ~event:"notification" ~data
      with _ -> ()
  ) sse_clients

let find_sse_client client_id =
  match client_id with
  | None -> None
  | Some id ->
      (match Hashtbl.find_opt sse_clients id with
       | Some client when client.connected -> Some (id, client)
       | _ -> None)

(** ============== Progress Notifications ============== *)

(** Generic SSE broadcast - sends data to all connected clients *)
let broadcast_sse_data data =
  Hashtbl.iter (fun _ client ->
    if client.connected then
      try send_sse_event client ~event:"notification" ~data with _ -> ()
  ) sse_clients

(** Initialize Mcp_progress with broadcast function *)
let () = Mcp_progress.set_broadcast_fn broadcast_sse_data

(** ============== HTTP Handlers ============== *)

let health_handler _request reqd =
  let json = sprintf {|{"status":"ok","server":"%s","version":"%s","protocol":"%s"}|}
    Mcp_protocol.server_name
    Mcp_protocol.server_version
    Mcp_protocol.protocol_version
  in
  Response.json json reqd

(** MCP POST handler - async body reading with callback-based response *)
let run_mcp_request ~domain_mgr ~eio_ctx server body_str =
  let run () =
    Mcp_tools.install_eio_context eio_ctx;
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
        (match find_sse_client client_id with
         | Some (id, client) ->
             Response.accepted reqd;
             Eio.Fiber.fork ~sw (fun () ->
               try
                 let response_str = run_mcp_request ~domain_mgr ~eio_ctx server body_str in
                 send_sse_event client ~event:"message" ~data:response_str
               with exn ->
                 eprintf "[MCP] SSE request failed (client=%d): %s\n%!" id (Printexc.to_string exn);
                 unregister_sse_client id)
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
               let err = Mcp_protocol.make_error_response `Null
                 Mcp_protocol.internal_error (Printexc.to_string exn) None in
               if wants_sse then
                 Response.sse_message (Yojson.Safe.to_string err) reqd
               else
                 Response.json ~status:`Internal_server_error (Yojson.Safe.to_string err) reqd)))

(** MCP SSE handler for streamable-http protocol (GET /mcp) *)
let mcp_sse_handler ~clock _request reqd =
  Response.sse_stream reqd ~on_write:(fun body ->
    (* Register client for shutdown broadcast *)
    let client_id, client = register_sse_client body in

    (* Send initial endpoint event (MCP protocol requirement) *)
    let endpoint = sprintf "/mcp?client_id=%d" client_id in
    send_sse_event client ~event:"endpoint" ~data:endpoint;

    (* Keep connection alive with periodic pings *)
    let rec ping_loop () =
      try
        Eio.Time.sleep clock 15.0;
        let timestamp = string_of_float (Unix.gettimeofday ()) in
        send_sse_event client ~event:"ping" ~data:timestamp;
        ping_loop ()
      with _ ->
        (* Client disconnected or error - unregister and close *)
        unregister_sse_client client_id;
        Httpun.Body.Writer.close body
    in
    ping_loop ()
  )

(** ============== Plugin Bridge Handlers ============== *)

(* Plugin configuration from centralized Figma_config *)
let plugin_ttl_seconds = Figma_config.Plugin.ttl_seconds
let plugin_poll_max_ms = Figma_config.Plugin.poll_max_ms

let plugin_cleanup () =
  Figma_plugin_bridge.cleanup_inactive ~ttl_seconds:plugin_ttl_seconds

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

let clamp_poll_ms value =
  let value = max 0 value in
  if value > plugin_poll_max_ms then plugin_poll_max_ms else value

let wait_for_commands ~clock ~channel_id ~max ~timeout_ms =
  let commands = Figma_plugin_bridge.poll_commands ~channel_id ~max in
  if commands <> [] || timeout_ms <= 0 then
    commands
  else begin
    let promise, resolver = Eio.Promise.create () in
    let waiter_id =
      Figma_plugin_bridge.register_waiter ~channel_id ~notify:(fun () ->
        try Eio.Promise.resolve resolver () with _ -> ())
    in
    let commands_after = Figma_plugin_bridge.poll_commands ~channel_id ~max in
    if commands_after <> [] then begin
      Figma_plugin_bridge.unregister_waiter ~channel_id ~waiter_id;
      commands_after
    end else begin
      let wait_s = float_of_int timeout_ms /. 1000.0 in
      let result =
        match Eio.Time.with_timeout clock wait_s (fun () ->
          Eio.Promise.await promise;
          Ok `Woke) with
        | Ok `Woke -> `Woke
        | Error `Timeout -> `Timeout
      in
      Figma_plugin_bridge.unregister_waiter ~channel_id ~waiter_id;
      match result with
      | `Woke -> Figma_plugin_bridge.poll_commands ~channel_id ~max
      | `Timeout -> []
    end
  end

let plugin_connect_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    plugin_cleanup ();
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let channel_id = get_string_field "channel_id" json in
        let channel_id = Figma_plugin_bridge.register_channel ?channel_id () in
        eprintf "[Plugin] connect channel=%s\n%!" channel_id;
        let body = `Assoc [
          ("status", `String "ok");
          ("channel_id", `String channel_id);
        ] in
        Response.json (Yojson.Safe.to_string body) reqd
  )

let plugin_poll_handler ~clock _request reqd =
  Request.read_body_async reqd (fun body_str ->
    plugin_cleanup ();
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        (match get_string_field "channel_id" json with
         | None -> json_error "Missing channel_id" reqd
         | Some channel_id ->
             let max_commands = get_int_field "max_commands" json |> Option.value ~default:1 in
             let wait_ms =
               match get_int_field "wait_ms" json with
               | Some value -> clamp_poll_ms value
               | None ->
                   (match get_int_field "timeout_ms" json with
                    | Some value -> clamp_poll_ms value
                    | None -> 0)
             in
             let commands : Figma_plugin_bridge.command list =
               if wait_ms > 0 then
                 wait_for_commands ~clock ~channel_id ~max:max_commands ~timeout_ms:wait_ms
               else
                 Figma_plugin_bridge.poll_commands ~channel_id ~max:max_commands
             in
             if commands <> [] then
               eprintf "[Plugin] poll channel=%s max=%d wait_ms=%d -> %d commands\n%!"
                 channel_id max_commands wait_ms (List.length commands);
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
    plugin_cleanup ();
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let channel_id = get_string_field "channel_id" json in
        let command_id = get_string_field "command_id" json in
        let ok = get_bool_field "ok" json |> Option.value ~default:true in
        let payload =
          match get_payload_field "payload" json with
          | Some (`String s) -> (
              try Yojson.Safe.from_string s
              with _ -> `Assoc [
                ("error", `String "Failed to parse payload string");
                ("raw", `String s);
              ])
          | Some payload -> payload
          | None -> `Null
        in
        (match (channel_id, command_id) with
         | (Some channel_id, Some command_id) ->
             Figma_plugin_bridge.store_result ~channel_id ~command_id ~ok ~payload;
             eprintf "[Plugin] result channel=%s cmd=%s ok=%b\n%!"
               channel_id command_id ok;
             let body = `Assoc [("status", `String "ok")] in
             Response.json (Yojson.Safe.to_string body) reqd
         | _ ->
             json_error "Missing channel_id or command_id" reqd))

let plugin_status_handler _request reqd =
  plugin_cleanup ();
  let channels = Figma_plugin_bridge.list_channels () in
  let default_channel = Figma_plugin_bridge.get_default_channel () in
  let body = `Assoc [
    ("channels", `List (List.map (fun id -> `String id) channels));
    ("default_channel", match default_channel with Some id -> `String id | None -> `Null);
  ] in
  Response.json (Yojson.Safe.to_string body) reqd

(** ============== Router ============== *)

let route_request ~clock ~domain_mgr ~sw ~eio_ctx server request reqd =
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
      mcp_post_handler ~sw ~domain_mgr ~eio_ctx server request reqd

  | `POST, "/plugin/connect" ->
      plugin_connect_handler request reqd

  | `POST, "/plugin/poll" ->
      plugin_poll_handler ~clock request reqd

  | `POST, "/plugin/result" ->
      plugin_result_handler request reqd

  | _ ->
      Response.not_found reqd

(** ============== httpun-eio Server ============== *)

let make_request_handler ~clock ~domain_mgr ~sw ~eio_ctx server =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    route_request ~clock ~domain_mgr ~sw ~eio_ctx server request reqd

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
let run ~sw ~net ~clock ~domain_mgr config server =
  (* Set Eio context for pure Eio handlers (Lwt-free path) *)
  let eio_client = Figma_api_eio.make_client net in
  let eio_ctx = Mcp_tools.set_eio_context ~sw ~net ~clock ~client:eio_client in
  let request_handler = make_request_handler ~clock ~domain_mgr ~sw ~eio_ctx server in
  let resolve_listen_ips host =
    match String.lowercase_ascii host with
    | "localhost" ->
        [Eio.Net.Ipaddr.V4.loopback; Eio.Net.Ipaddr.V6.loopback]
    | _ ->
        (match Ipaddr.of_string host with
         | Ok addr -> [Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets addr)]
         | Error _ -> [Eio.Net.Ipaddr.V4.loopback])
  in
  let listen_socket ip =
    let addr = `Tcp (ip, config.port) in
    try Some (Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:config.max_connections addr)
    with exn ->
      let ip_str = Format.asprintf "%a" Eio.Net.Ipaddr.pp ip in
      eprintf "[%s] Failed to listen on %s:%d (%s)\n%!"
        Mcp_protocol.server_name
        ip_str
        config.port
        (Printexc.to_string exn);
      None
  in
  let sockets =
    resolve_listen_ips config.host
    |> List.filter_map listen_socket
  in
  let is_cancelled exn =
    match exn with
    | Eio.Cancel.Cancelled _ -> true
    | _ -> false
  in
  let initial_backoff_s = 0.05 in
  let max_backoff_s = 1.0 in
  let make_accept_loop socket =
    let backoff_s = ref initial_backoff_s in
    let reset_backoff () = backoff_s := initial_backoff_s in
    let bump_backoff () = backoff_s := min max_backoff_s (!backoff_s *. 2.0) in
    let rec accept_loop () =
      try
        (try
           let flow, client_addr = Eio.Net.accept ~sw socket in
           reset_backoff ();
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
           )
         with exn ->
           if is_cancelled exn then raise exn;
           let delay = !backoff_s in
           eprintf "[%s] Accept error: %s (backoff %.2fs)\n%!"
             Mcp_protocol.server_name
             (Printexc.to_string exn)
             delay;
           Eio.Time.sleep clock delay;
           bump_backoff ());
        accept_loop ()
      with exn ->
        if is_cancelled exn then ()
        else
          let delay = !backoff_s in
          eprintf "[%s] Accept loop error: %s (backoff %.2fs)\n%!"
            Mcp_protocol.server_name
            (Printexc.to_string exn)
            delay;
          Eio.Time.sleep clock delay;
          bump_backoff ();
          accept_loop ()
    in
    accept_loop
  in
  let first_socket =
    match sockets with
    | [] -> failwith "No listening sockets available"
    | socket :: rest ->
        List.iter
          (fun extra ->
            Eio.Fiber.fork ~sw (fun () ->
              make_accept_loop extra ()))
          rest;
        socket
  in

  eprintf "🎨 %s MCP Server (Eio)\n" Mcp_protocol.server_name;
  eprintf "   Protocol: %s\n" Mcp_protocol.protocol_version;
  eprintf "   HTTP:     http://%s:%d\n" config.host config.port;
  eprintf "   MCP:      GET  /mcp -> SSE stream (streamable-http)\n";
  eprintf "             POST /mcp -> JSON-RPC requests\n";
  eprintf "   Graceful shutdown: SIGTERM/SIGINT supported\n%!";

  (* Periodic cleanup fiber for idle plugin channels - prevents memory leaks *)
  Eio.Fiber.fork ~sw (fun () ->
    let is_cancelled exn =
      match exn with
      | Eio.Cancel.Cancelled _ -> true
      | _ -> false
    in
    let rec cleanup_loop () =
      (try
         Eio.Time.sleep clock 60.0 (* Clean up every 1 minute *)
       with exn ->
         if is_cancelled exn then raise exn;
         eprintf "[Plugin] cleanup sleep error: %s\n%!" (Printexc.to_string exn));
      (try
         Figma_plugin_bridge.cleanup_inactive ~ttl_seconds:300.0 (* 5 min TTL *)
       with exn ->
         if is_cancelled exn then raise exn;
         eprintf "[Plugin] cleanup loop error: %s\n%!" (Printexc.to_string exn));
      cleanup_loop ()
    in
    try cleanup_loop () with exn ->
      if is_cancelled exn then ()
      else eprintf "[Plugin] cleanup fatal error: %s\n%!" (Printexc.to_string exn)
  );

  let accept_loop = make_accept_loop first_socket in
  accept_loop ()

(** Graceful shutdown exception *)
exception Shutdown

(** Start the server - entry point for main.ml (Pure Eio, no Lwt) *)
let start_server ?(config = default_config) server =
  (* Initialize crypto RNG for HTTPS/TLS *)
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let domain_mgr = Some (Eio.Stdenv.domain_mgr env) in

  (* Graceful shutdown setup *)
  let switch_ref = ref None in
  let shutdown_initiated = ref false in
  let initiate_shutdown signal_name =
    if not !shutdown_initiated then begin
      shutdown_initiated := true;
      eprintf "\n🎨 %s: Received %s, shutting down gracefully...\n%!" Mcp_protocol.server_name signal_name;

      (* Broadcast shutdown notification to all SSE clients *)
      broadcast_sse_shutdown signal_name;
      eprintf "🎨 %s: Sent shutdown notification to %d SSE clients\n%!" Mcp_protocol.server_name (Hashtbl.length sse_clients);

      (* Give clients 500ms to receive the notification *)
      Unix.sleepf 0.5;

      match !switch_ref with
      | Some sw -> Eio.Switch.fail sw Shutdown
      | None -> ()
    end
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGTERM"));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGINT"));

  (try
    Eio.Switch.run @@ fun sw ->
    switch_ref := Some sw;
    run ~sw ~net ~clock ~domain_mgr config server
  with
  | Shutdown ->
      eprintf "🎨 %s: Shutdown complete.\n%!" Mcp_protocol.server_name
  | Eio.Cancel.Cancelled _ ->
      eprintf "🎨 %s: Shutdown complete.\n%!" Mcp_protocol.server_name)

(** ============== stdio Server (Pure Eio) ============== *)

(** Run stdio server with Eio - blocking loop reading from stdin *)
let run_stdio ~sw ~env ~net ~clock server =
  (* Set Eio context for pure Eio handlers *)
  let eio_client = Figma_api_eio.make_client net in
  ignore (Mcp_tools.set_eio_context ~sw ~net ~clock ~client:eio_client);

  eprintf "[%s] MCP Server started (protocol: %s, mode: stdio/Eio)\n%!"
    Mcp_protocol.server_name Mcp_protocol.protocol_version;

  (* Create buffered reader for stdin *)
  let stdin_flow = Eio.Stdenv.stdin env in
  let buf_read = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdin_flow in

  let rec read_loop () =
    match Eio.Buf_read.line buf_read with
    | line ->
        if String.trim line <> "" then begin
          match Mcp_protocol.parse_request line with
          | Ok req ->
              if Mcp_protocol.is_notification req then
                (* Notification: no response on stdout per JSON-RPC *)
                ignore (Mcp_protocol.process_request_sync server req)
              else begin
                (* Process request using sync handler (runs in Eio context) *)
                let response = Mcp_protocol.process_request_sync server req in
                let response_str = Yojson.Safe.to_string response in
                print_endline response_str;
                flush stdout
              end
          | Error msg ->
              let err_response = Mcp_protocol.make_error_response `Null Mcp_protocol.parse_error msg None in
              print_endline (Yojson.Safe.to_string err_response);
              flush stdout
        end;
        read_loop ()
    | exception End_of_file ->
        eprintf "[%s] Connection closed (EOF)\n%!" Mcp_protocol.server_name
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
        eprintf "[%s] Error: Input line too long\n%!" Mcp_protocol.server_name
    | exception exn ->
        eprintf "[%s] Error: %s\n%!" Mcp_protocol.server_name (Printexc.to_string exn)
  in
  read_loop ()

(** Start stdio server - entry point that sets up Eio runtime *)
let start_stdio_server server =
  (* Initialize crypto RNG for HTTPS/TLS *)
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in

  (* Graceful shutdown setup *)
  let switch_ref = ref None in
  let shutdown_initiated = ref false in
  let initiate_shutdown signal_name =
    if not !shutdown_initiated then begin
      shutdown_initiated := true;
      eprintf "\n[%s] Received %s, shutting down...\n%!" Mcp_protocol.server_name signal_name;
      match !switch_ref with
      | Some sw -> Eio.Switch.fail sw Shutdown
      | None -> ()
    end
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGTERM"));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGINT"));

  (try
    Eio.Switch.run @@ fun sw ->
    switch_ref := Some sw;
    run_stdio ~sw ~env ~net ~clock server
  with
  | Shutdown ->
      eprintf "[%s] Shutdown complete.\n%!" Mcp_protocol.server_name
  | Eio.Cancel.Cancelled _ ->
      eprintf "[%s] Shutdown complete.\n%!" Mcp_protocol.server_name)
