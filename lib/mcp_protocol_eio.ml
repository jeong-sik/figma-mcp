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

(** ============== Agent Queue for MCP-style async codegen ============== *)

type agent_request = {
  id: string;
  node: Yojson.Safe.t;
  platform: string;
  prompt: string;
  created_at: float;
  mutable result: string option;
  mutable completed: bool;
}

let agent_queue : (string, agent_request) Hashtbl.t = Hashtbl.create 16
let agent_queue_mutex = Mutex.create ()

let agent_add_request node platform prompt =
  let id = Printf.sprintf "req-%d-%d" (int_of_float (Unix.gettimeofday () *. 1000.0)) (Random.int 10000) in
  let req = { id; node; platform; prompt; created_at = Unix.gettimeofday (); result = None; completed = false } in
  Mutex.lock agent_queue_mutex;
  Hashtbl.add agent_queue id req;
  Mutex.unlock agent_queue_mutex;
  id

let agent_get_pending () =
  Mutex.lock agent_queue_mutex;
  let pending = Hashtbl.fold (fun _ req acc ->
    if not req.completed then req :: acc else acc
  ) agent_queue [] in
  Mutex.unlock agent_queue_mutex;
  List.sort (fun a b -> compare a.created_at b.created_at) pending

let agent_submit_result id code =
  Mutex.lock agent_queue_mutex;
  (match Hashtbl.find_opt agent_queue id with
   | Some req -> req.result <- Some code; req.completed <- true
   | None -> ());
  Mutex.unlock agent_queue_mutex

let agent_get_result id =
  Mutex.lock agent_queue_mutex;
  let result = match Hashtbl.find_opt agent_queue id with
    | Some req when req.completed -> req.result
    | _ -> None
  in
  Mutex.unlock agent_queue_mutex;
  result

let agent_cleanup_old () =
  let now = Unix.gettimeofday () in
  Mutex.lock agent_queue_mutex;
  let old_ids = Hashtbl.fold (fun id req acc ->
    if now -. req.created_at > 300.0 then id :: acc else acc  (* 5분 후 삭제 *)
  ) agent_queue [] in
  List.iter (Hashtbl.remove agent_queue) old_ids;
  Mutex.unlock agent_queue_mutex

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
  let default_max_body_bytes = 50 * 1024 * 1024

  let parse_positive_int value =
    try
      let v = int_of_string value in
      if v > 0 then Some v else None
    with _ -> None

  let max_body_bytes =
    let from_env name =
      match Sys.getenv_opt name with
      | Some v -> parse_positive_int v
      | None -> None
    in
    match from_env "FIGMA_MCP_MAX_BODY_BYTES" with
    | Some v -> v
    | None ->
        (match from_env "MCP_MAX_BODY_BYTES" with
         | Some v -> v
         | None -> default_max_body_bytes)

  let respond_error reqd status body =
    let headers = Httpun.Headers.of_list [
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("access-control-allow-origin", "*");
      ("access-control-allow-methods", "GET, POST, OPTIONS");
      ("access-control-allow-headers", "Content-Type, Accept, Access-Control-Request-Private-Network");
      ("access-control-allow-private-network", "true");
      ("connection", "close");
    ] in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body

  let respond_too_large reqd max_bytes =
    let body = Printf.sprintf
      "413 Request Entity Too Large (max %d bytes)" max_bytes
    in
    respond_error reqd `Payload_too_large body

  let respond_internal_error reqd exn =
    let body = Printf.sprintf
      "500 Internal Server Error: %s" (Printexc.to_string exn)
    in
    respond_error reqd `Internal_server_error body

  let read_body_async reqd callback =
    let request = Httpun.Reqd.request reqd in
    let content_length =
      match Httpun.Headers.get request.headers "content-length" with
      | Some v -> parse_positive_int v
      | None -> None
    in
    let body = Httpun.Reqd.request_body reqd in
    let stopped = ref false in
    let stop () =
      if not !stopped then begin
        stopped := true;
        (try Httpun.Body.Reader.close body with _ -> ())
      end
    in
    match content_length with
    | Some len when len > max_body_bytes ->
        stop ();
        respond_too_large reqd max_body_bytes
    | _ ->
        let initial_capacity =
          match content_length with
          | Some len when len > 0 && len < max_body_bytes -> len
          | _ -> 1024
        in
        let buf = Buffer.create initial_capacity in
        let seen_bytes = ref 0 in
        let rec read_loop () =
          Httpun.Body.Reader.schedule_read body
            ~on_eof:(fun () ->
              let body_str = Buffer.contents buf in
              try callback body_str with exn ->
                respond_internal_error reqd exn)
            ~on_read:(fun buffer ~off ~len ->
              if !stopped then ()
              else
                let next_bytes = !seen_bytes + len in
                if next_bytes > max_body_bytes then begin
                  stop ();
                  respond_too_large reqd max_body_bytes
                end else begin
                  seen_bytes := next_bytes;
                  let chunk = Bigstringaf.substring buffer ~off ~len in
                  Buffer.add_string buf chunk;
                  read_loop ()
                end)
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

(** Close all SSE connections gracefully - for shutdown *)
let close_all_sse_connections () =
  let client_ids = Hashtbl.fold (fun k _ acc -> k :: acc) sse_clients [] in
  List.iter (fun id ->
    (match Hashtbl.find_opt sse_clients id with
     | Some client ->
         client.connected <- false;
         (try Httpun.Body.Writer.close client.body with _ -> ())
     | None -> ());
    Hashtbl.remove sse_clients id
  ) client_ids;
  eprintf "🎨 Figma MCP: Closed %d SSE connections\n%!" (List.length client_ids)

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

(** Semantic analyzer - extracts structured info with exact measurements *)
let analyze_node_semantic node =
  let open Yojson.Safe.Util in
  let buf = Buffer.create 2048 in
  let add s = Buffer.add_string buf s; Buffer.add_char buf '\n' in

  let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in
  let to_int json = int_of_float (to_num json) in

  (* Extract design tokens (colors) *)
  let colors = Hashtbl.create 16 in
  let rec collect_colors n =
    (match member "fills" n with
     | `List fills -> List.iter (fun fill ->
         match member "color" fill with
         | `String c -> Hashtbl.replace colors c (member "name" n |> to_string_option |> Option.value ~default:"")
         | _ -> ()
       ) fills
     | _ -> ());
    (match member "children" n with
     | `List kids -> List.iter collect_colors kids
     | _ -> ())
  in
  collect_colors node;

  (* Root info *)
  let name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
  let w = member "width" node |> to_int in
  let h = member "height" node |> to_int in
  let bg = match member "fills" node with
    | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> c | _ -> "transparent")
    | _ -> "transparent"
  in

  add (sprintf "## Layout: %s (%dx%d, background: %s)" name w h bg);
  add "";

  (* Analyze children with exact measurements *)
  let rec analyze_child depth n =
    let indent = String.make (depth * 2) ' ' in
    let cname = member "name" n |> to_string_option |> Option.value ~default:"Layer" in
    let ctype = member "type" n |> to_string_option |> Option.value ~default:"" in
    let cx = member "x" node |> to_int in
    let cy = member "y" node |> to_int in
    let cw = member "width" n |> to_int in
    let ch = member "height" n |> to_int in
    let radius = member "cornerRadius" n |> to_num in

    (* Component pattern detection *)
    let pattern =
      let ln = String.lowercase_ascii cname in
      if String.length ln >= 4 && String.sub ln 0 4 = "side" then "Sidebar"
      else if String.length ln >= 6 && String.sub ln 0 6 = "header" then "Header"
      else if String.length ln >= 4 && String.sub ln 0 4 = "card" then "Card"
      else if String.length ln >= 4 && String.sub ln 0 4 = "stat" then "StatCard"
      else if String.length ln >= 3 && String.sub ln 0 3 = "nav" then "NavItem"
      else if String.length ln >= 6 && String.sub ln 0 6 = "button" then "Button"
      else if String.length ln >= 6 && String.sub ln 0 6 = "search" then "SearchInput"
      else if String.length ln >= 4 && String.sub ln 0 4 = "logo" then "Logo"
      else if String.length ln >= 6 && String.sub ln 0 6 = "avatar" then "Avatar"
      else ""
    in

    let pattern_hint = if pattern <> "" then sprintf " [%s]" pattern else "" in

    (match ctype with
     | "TEXT" ->
         let text = member "text" n in
         let chars = member "characters" text |> to_string_option |> Option.value ~default:"" in
         let fontSize = member "fontSize" text |> to_num in
         let fill = match member "fills" n with
           | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> c | _ -> "")
           | _ -> ""
         in
         add (sprintf "%s- TEXT \"%s\": %dpx, color %s%s" indent chars (int_of_float fontSize) fill pattern_hint)
     | "FRAME" | "RECTANGLE" | "GROUP" ->
         let fill = match member "fills" n with
           | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> ", bg " ^ c | _ -> "")
           | _ -> ""
         in
         let rad = if radius > 0.0 then sprintf ", radius %.0fpx" radius else "" in
         add (sprintf "%s- %s \"%s\": %dx%d at (%d,%d)%s%s%s" indent ctype cname cw ch cx cy fill rad pattern_hint);
         (match member "children" n with
          | `List kids when depth < 3 -> List.iter (analyze_child (depth + 1)) kids
          | `List kids -> add (sprintf "%s  ... %d more children" indent (List.length kids))
          | _ -> ())
     | "ELLIPSE" ->
         let fill = match member "fills" n with
           | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> ", fill " ^ c | _ -> "")
           | _ -> ""
         in
         add (sprintf "%s- ELLIPSE \"%s\": %dx%d%s%s" indent cname cw ch fill pattern_hint)
     | _ ->
         add (sprintf "%s- %s \"%s\": %dx%d%s" indent ctype cname cw ch pattern_hint));
  in

  (match member "children" node with
   | `List kids ->
       add "## Structure:";
       List.iter (analyze_child 0) kids
   | _ -> ());

  (* Design tokens *)
  add "";
  add "## Design Tokens:";
  Hashtbl.iter (fun color ctx ->
    add (sprintf "- %s (used in: %s)" color (if ctx = "" then "fill" else ctx))
  ) colors;

  Buffer.contents buf

(** Generate code template for a Figma node - with recursive children generation *)
let rec generate_code_template ?(depth=0) node platform =
  let open Yojson.Safe.Util in
  let max_depth = 3 in (* Limit recursion depth *)
  let name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
  let safe_name = String.map (fun c -> if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_') name in
  let safe_name = if String.length safe_name > 0 && safe_name.[0] >= '0' && safe_name.[0] <= '9' then "_" ^ safe_name else safe_name in
  let to_number json = match json with
    | `Float f -> f | `Int i -> float_of_int i | _ -> 100.0
  in
  let w = member "width" node |> to_number |> int_of_float in
  let h = member "height" node |> to_number |> int_of_float in
  let node_type = member "type" node |> to_string_option |> Option.value ~default:"FRAME" in

  (* Extract background color from fills *)
  let bg_color = match member "fills" node with
    | `List ((`Assoc fields) :: _) ->
        (match List.assoc_opt "color" fields with
         | Some (`String c) -> Some c
         | _ -> None)
    | _ -> None
  in

  (* Extract children and generate code recursively *)
  let children = match member "children" node with
    | `List kids -> kids
    | _ -> []
  in
  let child_count = List.length children in

  (* Generate child components recursively (up to max_depth) *)
  let generate_child_code child =
    let cname = member "name" child |> to_string_option |> Option.value ~default:"Layer" in
    let ctype = member "type" child |> to_string_option |> Option.value ~default:"FRAME" in
    let cw = member "width" child |> to_number |> int_of_float in
    let ch = member "height" child |> to_number |> int_of_float in
    let safe_cname = String.map (fun c -> if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_') cname in

    if depth >= max_depth then
      (* At max depth, just generate placeholder *)
      match platform with
      | "react" -> sprintf "      <div style={{width: %d, height: %d}}>{/* %s */}</div>" cw ch cname
      | "swiftui" -> sprintf "            Rectangle().frame(width: %d, height: %d) // %s" cw ch cname
      | "compose" -> sprintf "        Box(Modifier.size(%d.dp, %d.dp)) // %s" cw ch cname
      | "flutter" -> sprintf "        SizedBox(width: %d, height: %d), // %s" cw ch cname
      | _ -> sprintf "  <div style=\"width: %dpx; height: %dpx;\"><!-- %s --></div>" cw ch cname
    else if ctype = "TEXT" then
      (* TEXT nodes become text elements *)
      let text_content = member "characters" child |> to_string_option |> Option.value ~default:cname in
      match platform with
      | "react" -> sprintf "      <span>%s</span>" text_content
      | "swiftui" -> sprintf "            Text(\"%s\")" text_content
      | "compose" -> sprintf "        Text(\"%s\")" text_content
      | "flutter" -> sprintf "        Text('%s')," text_content
      | _ -> sprintf "  <span>%s</span>" text_content
    else
      (* Recursively generate for FRAME/GROUP/COMPONENT *)
      match platform with
      | "react" -> sprintf "      <%s />" safe_cname
      | "swiftui" -> sprintf "            %s()" safe_cname
      | "compose" -> sprintf "        %s()" safe_cname
      | "flutter" -> sprintf "        %s()," safe_cname
      | _ -> sprintf "  <%s></%s>" (String.lowercase_ascii safe_cname) (String.lowercase_ascii safe_cname)
  in

  let children_code =
    if child_count = 0 then ""
    else
      let limited_children = if child_count > 10 then List.filteri (fun i _ -> i < 10) children else children in
      String.concat "\n" (List.map generate_child_code limited_children) ^
      (if child_count > 10 then sprintf "\n      {/* ... and %d more children */}" (child_count - 10) else "")
  in

  let children_comment =
    if child_count = 0 || depth > 0 then ""
    else
      let child_lines = List.mapi (fun _i child ->
        let cname = member "name" child |> to_string_option |> Option.value ~default:"Layer" in
        let ctype = member "type" child |> to_string_option |> Option.value ~default:"" in
        sprintf "//   - %s (%s)" cname ctype
      ) (if child_count > 8 then List.filteri (fun i _ -> i < 8) children else children) in
      let truncated = if child_count > 8 then sprintf "\n//   ... and %d more" (child_count - 8) else "" in
      sprintf "\n// Children (%d):\n%s%s\n" child_count (String.concat "\n" child_lines) truncated
  in

  (* Generate sub-component definitions for deeper children *)
  let sub_components =
    if depth >= max_depth || child_count = 0 then ""
    else
      let sub_defs = List.filter_map (fun child ->
        let ctype = member "type" child |> to_string_option |> Option.value ~default:"" in
        if ctype = "TEXT" then None
        else Some (generate_code_template ~depth:(depth + 1) child platform)
      ) (if child_count > 5 then List.filteri (fun i _ -> i < 5) children else children) in
      if List.length sub_defs = 0 then "" else "\n" ^ String.concat "\n" sub_defs
  in

  let _ = node_type in (* suppress unused warning *)

  (* Extract auto-layout info *)
  let layout_mode = match member "autoLayout" node with
    | `Assoc fields ->
        (match List.assoc_opt "mode" fields with
         | Some (`String m) -> Some m
         | _ -> None)
    | _ -> None
  in
  let layout_spacing = match member "autoLayout" node with
    | `Assoc fields ->
        (match List.assoc_opt "spacing" fields with
         | Some (`Int s) -> Some s
         | Some (`Float s) -> Some (int_of_float s)
         | _ -> None)
    | _ -> None
  in

  (* Build child content or empty placeholder *)
  let empty_children = if child_count = 0 then "{/* Empty */}" else "" in
  let react_children = if children_code = "" then empty_children else "\n" ^ children_code ^ "\n    " in
  let swift_children = if children_code = "" then "EmptyView()" else "\n" ^ children_code ^ "\n        " in
  let compose_children = if children_code = "" then "// Empty" else "\n" ^ children_code ^ "\n    " in
  let flutter_children = if children_code = "" then "// Empty" else "\n" ^ children_code ^ "\n      " in
  let html_children = if children_code = "" then "<!-- Empty -->" else "\n" ^ children_code ^ "\n" in

  match platform with
  | "react" ->
      let bg_style = match bg_color with Some c -> sprintf "backgroundColor: '%s',\n        " c | None -> "" in
      let flex_style = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "display: 'flex', flexDirection: 'row', gap: %d,\n        " sp
        | Some "VERTICAL", Some sp -> sprintf "display: 'flex', flexDirection: 'column', gap: %d,\n        " sp
        | _ -> ""
      in
      sprintf "import React from 'react';\n%s\nexport const %s: React.FC = () => (\n  <div style={{\n        width: %d, height: %d,\n        %s%s}}>%s</div>\n);\n%s"
        children_comment safe_name w h bg_style flex_style react_children sub_components

  | "swiftui" ->
      let bg_mod = match bg_color with Some c -> sprintf "\n        .background(Color(hex: \"%s\"))" c | None -> "" in
      let stack = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "HStack(spacing: %d)" sp
        | Some "VERTICAL", Some sp -> sprintf "VStack(spacing: %d)" sp
        | _ -> "ZStack"
      in
      sprintf "import SwiftUI\n%s\nstruct %s: View {\n    var body: some View {\n        %s {%s}\n        .frame(width: %d, height: %d)%s\n    }\n}\n%s"
        children_comment safe_name stack swift_children w h bg_mod sub_components

  | "compose" ->
      let bg_mod = match bg_color with Some c -> sprintf ".background(Color(0xFF%s))\n            " (String.sub c 1 (String.length c - 1)) | None -> "" in
      let layout = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "Row(horizontalArrangement = Arrangement.spacedBy(%d.dp))" sp
        | Some "VERTICAL", Some sp -> sprintf "Column(verticalArrangement = Arrangement.spacedBy(%d.dp))" sp
        | _ -> "Box"
      in
      sprintf "@Composable\nfun %s(modifier: Modifier = Modifier) {%s\n    %s(modifier.size(%d.dp, %d.dp)%s) {%s}\n}\n%s"
        safe_name children_comment layout w h bg_mod compose_children sub_components

  | "flutter" ->
      let bg_prop = match bg_color with Some c -> sprintf "color: Color(0xFF%s),\n      " (String.sub c 1 (String.length c - 1)) | None -> "" in
      let layout = match layout_mode with Some "HORIZONTAL" -> "Row" | Some "VERTICAL" -> "Column" | _ -> "Stack" in
      sprintf "import 'package:flutter/material.dart';\n%s\nclass %s extends StatelessWidget {\n  const %s({super.key});\n\n  @override\n  Widget build(BuildContext context) {\n    return Container(\n      width: %d, height: %d,\n      %schild: %s(children: [%s]),\n    );\n  }\n}\n%s"
        children_comment safe_name safe_name w h bg_prop layout flutter_children sub_components

  | _ ->
      let bg_css = match bg_color with Some c -> sprintf "background-color: %s;\n  " c | None -> "" in
      let flex_css = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "display: flex; flex-direction: row; gap: %dpx;\n  " sp
        | Some "VERTICAL", Some sp -> sprintf "display: flex; flex-direction: column; gap: %dpx;\n  " sp
        | _ -> ""
      in
      sprintf "<!-- %s -->%s\n<div class=\"%s\">%s</div>\n\n<style>\n.%s {\n  width: %dpx; height: %dpx;\n  %s%s}\n</style>\n%s"
        name children_comment (String.lowercase_ascii safe_name) html_children (String.lowercase_ascii safe_name) w h bg_css flex_css sub_components

(** POST /plugin/template - Direct template generation (for testing) *)
let template_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platform = member "platform" json |> to_string_option |> Option.value ~default:"react" in
        let code = generate_code_template node platform in
        let result = `Assoc [
          ("code", `String code);
          ("platform", `String platform);
          ("source", `String "template");
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** Plugin codegen handler - calls llm-mcp for code generation *)
let plugin_codegen_handler ~sw ~eio_ctx _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platform = member "platform" json |> to_string_option |> Option.value ~default:"react" in
        let prompt = member "prompt" json |> to_string_option |> Option.value ~default:"" in

        (* Build LLM request with semantic analysis *)
        let semantic_info = analyze_node_semantic node in
        let platform_instruction = match platform with
          | "react" -> "Generate production-ready React/TypeScript code. Use functional components with proper typing. Include all exact pixel values for width, height, padding, margin, fontSize, borderRadius."
          | "swiftui" -> "Generate production-ready SwiftUI code. Use proper View modifiers with exact pixel values for frame, padding, cornerRadius, fontSize."
          | "flutter" -> "Generate production-ready Flutter/Dart code. Use exact pixel values in SizedBox, Container, EdgeInsets, BorderRadius."
          | "compose" -> "Generate production-ready Jetpack Compose/Kotlin code. Use exact Dp values for size, padding, corner radius."
          | _ -> "Generate production-ready code with exact pixel measurements."
        in
        let full_prompt = if prompt = "" then
          sprintf "Convert this Figma design to %s code.\n\n%s\n\n%s\n\nIMPORTANT: Use EXACT pixel values from the design. Do not approximate." platform semantic_info platform_instruction
        else
          prompt
        in

        (* Fallback to template *)
        let send_template () =
          let template = generate_code_template node platform in
          let result = `Assoc [("code", `String template); ("platform", `String platform); ("fallback", `Bool true)] in
          Response.json (Yojson.Safe.to_string result) reqd
        in

        (* Try Ollama *)
        let try_ollama () =
          let ollama_url = "http://127.0.0.1:11434/api/generate" in
          let ollama_body = `Assoc [
            ("model", `String "qwen3-coder:30b");
            ("prompt", `String full_prompt);
            ("stream", `Bool false);
          ] in
          let cohttp = Figma_api_eio.get_cohttp_client eio_ctx.Mcp_tools.client in
          let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
          let req_body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string ollama_body) in
          let uri = Uri.of_string ollama_url in
          let resp, resp_body = Cohttp_eio.Client.post cohttp ~sw ~headers ~body:req_body uri in
          let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          if status_code < 200 || status_code >= 300 then failwith "Ollama HTTP error";
          let ollama_resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:(10 * 1024 * 1024) in
          let ollama_resp = Yojson.Safe.from_string ollama_resp_str in
          let gen_code = member "response" ollama_resp |> to_string_option |> Option.value ~default:"" in
          if String.length gen_code > 10 then
            let result_json = `Assoc [("code", `String gen_code); ("platform", `String platform); ("source", `String "ollama")] in
            Response.json (Yojson.Safe.to_string result_json) reqd
          else
            send_template ()
        in

        (* Try Claude API first if key available *)
        let anthropic_key = Sys.getenv_opt "ANTHROPIC_API_KEY" in
        (match anthropic_key with
        | Some key when String.length key > 10 ->
            printf "[Codegen] Trying Claude API (key: %d chars)...\n%!" (String.length key);
            (try
              let cohttp = Figma_api_eio.get_cohttp_client eio_ctx.Mcp_tools.client in
              let claude_body = `Assoc [
                ("model", `String "claude-sonnet-4-20250514");  (* Claude 4 Sonnet *)
                ("max_tokens", `Int 4096);
                ("messages", `List [
                  `Assoc [
                    ("role", `String "user");
                    ("content", `String full_prompt)
                  ]
                ]);
              ] in
              let headers = Cohttp.Header.of_list [
                ("Content-Type", "application/json");
                ("x-api-key", key);
                ("anthropic-version", "2023-06-01");
              ] in
              let req_body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string claude_body) in
              let uri = Uri.of_string "https://api.anthropic.com/v1/messages" in
              let resp, resp_body = Cohttp_eio.Client.post cohttp ~sw ~headers ~body:req_body uri in
              let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
              if status_code < 200 || status_code >= 300 then begin
                let err_body = try Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:4096 with _ -> "" in
                printf "[Codegen] Claude HTTP %d: %s\n%!" status_code err_body;
                failwith (sprintf "Claude HTTP %d" status_code)
              end;
              let claude_resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:(10 * 1024 * 1024) in
              let claude_resp = Yojson.Safe.from_string claude_resp_str in
              let content_blocks = member "content" claude_resp |> to_list in
              let gen_code = List.fold_left (fun acc block ->
                let text = member "text" block |> to_string_option |> Option.value ~default:"" in
                acc ^ text
              ) "" content_blocks in
              if String.length gen_code > 10 then
                let result_json = `Assoc [("code", `String gen_code); ("platform", `String platform); ("source", `String "claude")] in
                Response.json (Yojson.Safe.to_string result_json) reqd
              else begin
                printf "[Codegen] Claude returned empty, fallback to Ollama\n%!";
                try_ollama ()
              end
            with exn ->
              printf "[Codegen] Claude error: %s, fallback to Ollama\n%!" (Printexc.to_string exn);
              try_ollama ())
        | _ ->
            (* No Claude key, use Ollama *)
            (try try_ollama () with _ -> send_template ()))
  )

(** Plugin analyze handler - analyzes node structure with LLM insights *)
let plugin_analyze_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let prompt = member "prompt" json |> to_string_option |> Option.value ~default:"" in

        (* Build analysis from node info *)
        let node_info = Yojson.Safe.to_string node in
        let full_prompt = if prompt = "" then
          sprintf "Analyze this Figma node and provide insights:\n%s\n\nProvide: 1) Structure overview, 2) Design patterns used, 3) Accessibility considerations, 4) Implementation recommendations." node_info
        else
          prompt
        in

        (* Try llm-mcp, fallback to local analysis *)
        (* Local analysis - fast and reliable, no LLM dependency *)
        let _ = full_prompt in (* suppress unused warning *)
        let to_number json = match json with
          | `Float f -> f | `Int i -> float_of_int i | _ -> 0.0
        in
        let name = member "name" node |> to_string_option |> Option.value ~default:"Unnamed" in
        let node_type = member "type" node |> to_string_option |> Option.value ~default:"UNKNOWN" in
        let w = member "width" node |> to_number |> int_of_float in
        let h = member "height" node |> to_number |> int_of_float in
        let children = match member "children" node with `List kids -> List.length kids | _ -> 0 in
        let has_autolayout = match member "autoLayout" node with `Null -> false | _ -> true in
        let fills_count = match member "fills" node with `List f -> List.length f | _ -> 0 in

        (* Build children list *)
        let children_detail = match member "children" node with
          | `List kids ->
              let child_items = List.mapi (fun i c ->
                let cname = member "name" c |> to_string_option |> Option.value ~default:(sprintf "Layer %d" i) in
                let ctype = member "type" c |> to_string_option |> Option.value ~default:"UNKNOWN" in
                sprintf "  - %s (%s)" cname ctype
              ) (List.filteri (fun i _ -> i < 10) kids) in
              if List.length kids > 10 then
                String.concat "\n" child_items ^ sprintf "\n  - ... and %d more" (List.length kids - 10)
              else
                String.concat "\n" child_items
          | _ -> "  (none)"
        in

        let analysis = sprintf "## Analysis: %s\n\n**Type**: %s\n**Dimensions**: %d × %d px\n**Children**: %d layer(s)\n%s\n\n**Auto-layout**: %s\n**Fills**: %d\n\n### Recommendations\n- %s\n- Consider adding semantic naming for accessibility\n- %s"
          name node_type w h children children_detail
          (if has_autolayout then "Yes (responsive)" else "No (fixed)")
          fills_count
          (if children > 5 then "Consider grouping related layers" else "Structure looks manageable")
          (if w > 1200 then "Large width - ensure responsive breakpoints" else "Width suitable for most viewports")
        in
        let result_json = `Assoc [("analysis", `String analysis); ("source", `String "local")] in
        Response.json (Yojson.Safe.to_string result_json) reqd
  )

(** ============== Agent Queue Handlers ============== *)

(** POST /agent/request - Plugin submits a codegen request to queue *)
let agent_request_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platform = member "platform" json |> to_string_option |> Option.value ~default:"react" in
        let prompt = member "prompt" json |> to_string_option |> Option.value ~default:"" in
        let node_info = Yojson.Safe.to_string node in
        let full_prompt = if prompt = "" then
          sprintf "Convert this Figma node to %s code:\n%s\n\nGenerate clean, production-ready code." platform node_info
        else prompt in
        let req_id = agent_add_request node platform full_prompt in
        let result = `Assoc [("request_id", `String req_id); ("status", `String "queued")] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** GET /agent/pending - Agent polls for pending requests *)
let agent_pending_handler _request reqd =
  agent_cleanup_old ();
  let pending = agent_get_pending () in
  let requests = List.map (fun req ->
    `Assoc [
      ("id", `String req.id);
      ("platform", `String req.platform);
      ("prompt", `String req.prompt);
      ("node", req.node);
      ("age_sec", `Float (Unix.gettimeofday () -. req.created_at));
    ]
  ) pending in
  let result = `Assoc [("pending", `List requests); ("count", `Int (List.length pending))] in
  Response.json (Yojson.Safe.to_string result) reqd

(** POST /agent/result - Agent submits generated code *)
let agent_result_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let req_id = member "request_id" json |> to_string in
        let code = member "code" json |> to_string in
        agent_submit_result req_id code;
        let result = `Assoc [("status", `String "submitted"); ("request_id", `String req_id)] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** GET /agent/status/:id - Check request status *)
let agent_status_handler request reqd =
  let path = Request.path request in
  let req_id = String.sub path 14 (String.length path - 14) in (* /agent/status/ = 14 chars *)
  match agent_get_result req_id with
  | Some code ->
      let result = `Assoc [("status", `String "completed"); ("code", `String code)] in
      Response.json (Yojson.Safe.to_string result) reqd
  | None ->
      let result = `Assoc [("status", `String "pending")] in
      Response.json (Yojson.Safe.to_string result) reqd

(** POST /plugin/extract-tokens - Extract design tokens from Figma node *)
let extract_tokens_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let format = member "format" json |> to_string_option |> Option.value ~default:"tokens-studio" in

        (* Token collection *)
        let colors = Hashtbl.create 32 in
        let typography = Hashtbl.create 16 in
        let spacing = Hashtbl.create 16 in
        let radii = Hashtbl.create 16 in
        let effects = Hashtbl.create 16 in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* RGBA color to hex with alpha *)
        let rgba_to_hex r g b a =
          let ri = int_of_float (r *. 255.0) in
          let gi = int_of_float (g *. 255.0) in
          let bi = int_of_float (b *. 255.0) in
          if a < 1.0 then
            sprintf "rgba(%d, %d, %d, %.2f)" ri gi bi a
          else
            sprintf "#%02x%02x%02x" ri gi bi
        in

        (* Extract color from fill/stroke *)
        let extract_color prefix fill =
          match member "color" fill with
          | `Assoc color_obj ->
              let r = List.assoc_opt "r" color_obj |> Option.map to_num |> Option.value ~default:0.0 in
              let g = List.assoc_opt "g" color_obj |> Option.map to_num |> Option.value ~default:0.0 in
              let b = List.assoc_opt "b" color_obj |> Option.map to_num |> Option.value ~default:0.0 in
              let a = List.assoc_opt "a" color_obj |> Option.map to_num |> Option.value ~default:1.0 in
              let hex = rgba_to_hex r g b a in
              Hashtbl.replace colors (prefix ^ "-" ^ hex) hex
          | _ -> ()
        in

        (* Recursive token extraction *)
        let rec extract_from_node n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in

          (* Colors from fills *)
          (match member "fills" n with
           | `List fills -> List.iteri (fun i fill ->
               let visible = member "visible" fill |> to_bool_option |> Option.value ~default:true in
               if visible then extract_color (sprintf "fill-%s-%d" name i) fill
             ) fills
           | _ -> ());

          (* Colors from strokes *)
          (match member "strokes" n with
           | `List strokes -> List.iteri (fun i stroke ->
               extract_color (sprintf "stroke-%s-%d" name i) stroke
             ) strokes
           | _ -> ());

          (* Typography from TEXT nodes *)
          if node_type = "TEXT" then begin
            let style = member "style" n in
            let font_family = member "fontFamily" style |> to_string_option |> Option.value ~default:"" in
            let font_size = member "fontSize" style |> to_num in
            let font_weight = member "fontWeight" style |> to_num in
            let line_height = member "lineHeightPx" style |> to_num in
            let letter_spacing = member "letterSpacing" style |> to_num in
            if font_family <> "" then
              Hashtbl.replace typography name (`Assoc [
                ("fontFamily", `String font_family);
                ("fontSize", `Float font_size);
                ("fontWeight", `Int (int_of_float font_weight));
                ("lineHeight", `Float line_height);
                ("letterSpacing", `Float letter_spacing);
              ])
          end;

          (* Spacing from auto-layout *)
          let item_spacing = member "itemSpacing" n |> to_num in
          let padding_top = member "paddingTop" n |> to_num in
          let padding_right = member "paddingRight" n |> to_num in
          let padding_bottom = member "paddingBottom" n |> to_num in
          let padding_left = member "paddingLeft" n |> to_num in

          if item_spacing > 0.0 then
            Hashtbl.replace spacing (sprintf "gap-%s" name) (`Float item_spacing);
          if padding_top > 0.0 || padding_right > 0.0 || padding_bottom > 0.0 || padding_left > 0.0 then
            Hashtbl.replace spacing (sprintf "padding-%s" name) (`Assoc [
              ("top", `Float padding_top);
              ("right", `Float padding_right);
              ("bottom", `Float padding_bottom);
              ("left", `Float padding_left);
            ]);

          (* Border radius *)
          let corner_radius = member "cornerRadius" n |> to_num in
          let top_left = member "topLeftRadius" n |> to_num in
          let top_right = member "topRightRadius" n |> to_num in
          let bottom_right = member "bottomRightRadius" n |> to_num in
          let bottom_left = member "bottomLeftRadius" n |> to_num in

          if corner_radius > 0.0 then
            Hashtbl.replace radii (sprintf "radius-%s" name) (`Float corner_radius)
          else if top_left > 0.0 || top_right > 0.0 || bottom_right > 0.0 || bottom_left > 0.0 then
            Hashtbl.replace radii (sprintf "radius-%s" name) (`Assoc [
              ("topLeft", `Float top_left);
              ("topRight", `Float top_right);
              ("bottomRight", `Float bottom_right);
              ("bottomLeft", `Float bottom_left);
            ]);

          (* Effects (shadows, blurs) *)
          (match member "effects" n with
           | `List effs -> List.iteri (fun i eff ->
               let eff_type = member "type" eff |> to_string_option |> Option.value ~default:"" in
               let visible = member "visible" eff |> to_bool_option |> Option.value ~default:true in
               if visible then begin
                 match eff_type with
                 | "DROP_SHADOW" | "INNER_SHADOW" ->
                     let offset_x = member "offset" eff |> member "x" |> to_num in
                     let offset_y = member "offset" eff |> member "y" |> to_num in
                     let blur = member "radius" eff |> to_num in
                     let spread = member "spread" eff |> to_num in
                     let color = member "color" eff in
                     let r = member "r" color |> to_num in
                     let g = member "g" color |> to_num in
                     let b = member "b" color |> to_num in
                     let a = member "a" color |> to_num in
                     Hashtbl.replace effects (sprintf "shadow-%s-%d" name i) (`Assoc [
                       ("type", `String eff_type);
                       ("offsetX", `Float offset_x);
                       ("offsetY", `Float offset_y);
                       ("blur", `Float blur);
                       ("spread", `Float spread);
                       ("color", `String (rgba_to_hex r g b a));
                     ])
                 | "LAYER_BLUR" | "BACKGROUND_BLUR" ->
                     let blur = member "radius" eff |> to_num in
                     Hashtbl.replace effects (sprintf "blur-%s-%d" name i) (`Assoc [
                       ("type", `String eff_type);
                       ("radius", `Float blur);
                     ])
                 | _ -> ()
               end
             ) effs
           | _ -> ());

          (* Recurse into children *)
          (match member "children" n with
           | `List kids -> List.iter extract_from_node kids
           | _ -> ())
        in

        extract_from_node node;

        (* Build output based on format *)
        let color_list = Hashtbl.fold (fun k v acc -> (k, `String v) :: acc) colors [] in
        let typo_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) typography [] in
        let spacing_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) spacing [] in
        let radii_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) radii [] in
        let effects_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) effects [] in

        let result = match format with
          | "css-variables" ->
              (* CSS Custom Properties format *)
              let css_vars = Buffer.create 1024 in
              Buffer.add_string css_vars ":root {\n";
              List.iter (fun (k, v) ->
                match v with `String s -> Buffer.add_string css_vars (sprintf "  --%s: %s;\n" k s) | _ -> ()
              ) color_list;
              List.iter (fun (k, v) ->
                match v with `Float f -> Buffer.add_string css_vars (sprintf "  --%s: %.0fpx;\n" k f) | _ -> ()
              ) spacing_list;
              List.iter (fun (k, v) ->
                match v with `Float f -> Buffer.add_string css_vars (sprintf "  --%s: %.0fpx;\n" k f) | _ -> ()
              ) radii_list;
              Buffer.add_string css_vars "}\n";
              `Assoc [
                ("format", `String "css-variables");
                ("css", `String (Buffer.contents css_vars));
                ("tokenCount", `Int (List.length color_list + List.length typo_list + List.length spacing_list));
              ]
          | _ -> (* tokens-studio format *)
              `Assoc [
                ("format", `String "tokens-studio");
                ("tokens", `Assoc [
                  ("colors", `Assoc color_list);
                  ("typography", `Assoc typo_list);
                  ("spacing", `Assoc spacing_list);
                  ("borderRadius", `Assoc radii_list);
                  ("effects", `Assoc effects_list);
                ]);
                ("stats", `Assoc [
                  ("colors", `Int (List.length color_list));
                  ("typography", `Int (List.length typo_list));
                  ("spacing", `Int (List.length spacing_list));
                  ("borderRadius", `Int (List.length radii_list));
                  ("effects", `Int (List.length effects_list));
                  ("total", `Int (List.length color_list + List.length typo_list +
                                  List.length spacing_list + List.length radii_list + List.length effects_list));
                ]);
              ]
        in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/generate-story - Generate Storybook story from Figma node *)
let generate_story_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let figma_url = member "figmaUrl" json |> to_string_option |> Option.value ~default:"" in
        let framework = member "framework" json |> to_string_option |> Option.value ~default:"react" in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* Extract component info *)
        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in
        let safe_name = if String.length safe_name > 0 && safe_name.[0] >= '0' && safe_name.[0] <= '9'
          then "C" ^ safe_name else safe_name in

        (* Extract design tokens for controls *)
        let colors = ref [] in
        let spacings = ref [] in
        let radii = ref [] in

        let rec extract_tokens n =
          let _name = member "name" n |> to_string_option |> Option.value ~default:"" in

          (* Colors *)
          (match member "fills" n with
           | `List fills -> List.iter (fun fill ->
               match member "color" fill with
               | `Assoc c ->
                   let r = List.assoc_opt "r" c |> Option.map to_num |> Option.value ~default:0.0 in
                   let g = List.assoc_opt "g" c |> Option.map to_num |> Option.value ~default:0.0 in
                   let b = List.assoc_opt "b" c |> Option.map to_num |> Option.value ~default:0.0 in
                   let hex = sprintf "#%02x%02x%02x" (int_of_float (r *. 255.0)) (int_of_float (g *. 255.0)) (int_of_float (b *. 255.0)) in
                   if not (List.mem hex !colors) then colors := hex :: !colors
               | _ -> ()
             ) fills
           | _ -> ());

          (* Spacing *)
          let padding = member "paddingTop" n |> to_num in
          let gap = member "itemSpacing" n |> to_num in
          if padding > 0.0 && not (List.mem (int_of_float padding) !spacings) then
            spacings := (int_of_float padding) :: !spacings;
          if gap > 0.0 && not (List.mem (int_of_float gap) !spacings) then
            spacings := (int_of_float gap) :: !spacings;

          (* Border radius *)
          let radius = member "cornerRadius" n |> to_num in
          if radius > 0.0 && not (List.mem (int_of_float radius) !radii) then
            radii := (int_of_float radius) :: !radii;

          (* Recurse *)
          (match member "children" n with
           | `List kids -> List.iter extract_tokens kids
           | _ -> ())
        in
        extract_tokens node;

        (* Generate story based on framework *)
        let story_code = match framework with
          | "react" ->
              let color_options = String.concat ", " (List.map (sprintf "'%s'") !colors) in
              let spacing_options = String.concat ", " (List.map string_of_int !spacings) in
              let radius_options = String.concat ", " (List.map string_of_int !radii) in

              sprintf {|import type { Meta, StoryObj } from '@storybook/react';
import { %s } from './%s';

/**
 * %s Component
 *
 * Generated from Figma design.
 * @see %s
 */
const meta: Meta<typeof %s> = {
  title: 'Components/%s',
  component: %s,
  parameters: {
    layout: 'centered',
    design: {
      type: 'figma',
      url: '%s',
    },
  },
  tags: ['autodocs'],
  argTypes: {%s%s%s
  },
};

export default meta;
type Story = StoryObj<typeof meta>;

/**
 * Default state from Figma design
 */
export const Default: Story = {
  args: {},
};

/**
 * Interactive playground with all controls
 */
export const Playground: Story = {
  args: {},
  parameters: {
    docs: {
      canvas: { sourceState: 'shown' },
    },
  },
};
|}
                safe_name safe_name
                component_name
                (if figma_url = "" then "Figma" else figma_url)
                safe_name component_name safe_name
                figma_url
                (if !colors = [] then "" else sprintf "\n    backgroundColor: {\n      control: 'select',\n      options: [%s],\n    }," color_options)
                (if !spacings = [] then "" else sprintf "\n    padding: {\n      control: 'select',\n      options: [%s],\n    }," spacing_options)
                (if !radii = [] then "" else sprintf "\n    borderRadius: {\n      control: 'select',\n      options: [%s],\n    }," radius_options)

          | "vue" ->
              sprintf {|import type { Meta, StoryObj } from '@storybook/vue3';
import %s from './%s.vue';

const meta: Meta<typeof %s> = {
  title: 'Components/%s',
  component: %s,
  parameters: {
    design: {
      type: 'figma',
      url: '%s',
    },
  },
  tags: ['autodocs'],
};

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {
  args: {},
};
|}
                safe_name safe_name
                safe_name component_name safe_name
                figma_url

          | _ -> (* generic *)
              sprintf {|// Storybook story for %s
// Figma: %s

export default {
  title: 'Components/%s',
  parameters: {
    design: {
      type: 'figma',
      url: '%s',
    },
  },
};

export const Default = {};
|}
                component_name figma_url component_name figma_url
        in

        (* Generate component code if not provided *)
        let component_code = if code = "" then
          sprintf {|import React from 'react';

interface %sProps {
  backgroundColor?: string;
  padding?: number;
  borderRadius?: number;
  children?: React.ReactNode;
}

export const %s: React.FC<%sProps> = ({
  backgroundColor = '%s',
  padding = %d,
  borderRadius = %d,
  children,
}) => {
  return (
    <div
      style={{
        backgroundColor,
        padding,
        borderRadius,
      }}
    >
      {children}
    </div>
  );
};
|}
            safe_name safe_name safe_name
            (if !colors = [] then "#ffffff" else List.hd !colors)
            (if !spacings = [] then 16 else List.hd !spacings)
            (if !radii = [] then 8 else List.hd !radii)
        else
          code
        in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("storyFile", `String (sprintf "%s.stories.tsx" safe_name));
          ("componentFile", `String (sprintf "%s.tsx" safe_name));
          ("storyCode", `String story_code);
          ("componentCode", `String component_code);
          ("figmaUrl", `String figma_url);
          ("designTokens", `Assoc [
            ("colors", `List (List.map (fun c -> `String c) !colors));
            ("spacings", `List (List.map (fun s -> `Int s) !spacings));
            ("borderRadii", `List (List.map (fun r -> `Int r) !radii));
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/codegen-multi - Generate code for multiple platforms simultaneously *)
let codegen_multi_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platforms = match member "platforms" json with
          | `List ps -> List.filter_map (fun p -> to_string_option p) ps
          | _ -> ["react"; "swiftui"; "compose"]  (* default all 3 *)
        in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in
        let to_int_safe json = int_of_float (to_num json) in

        (* Extract component info *)
        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in
        let safe_name = if String.length safe_name > 0 && safe_name.[0] >= '0' && safe_name.[0] <= '9'
          then "C" ^ safe_name else safe_name in

        let w = member "width" node |> to_int_safe in
        let h = member "height" node |> to_int_safe in
        let radius = member "cornerRadius" node |> to_num in
        let padding_t = member "paddingTop" node |> to_int_safe in
        let padding_r = member "paddingRight" node |> to_int_safe in
        let padding_b = member "paddingBottom" node |> to_int_safe in
        let padding_l = member "paddingLeft" node |> to_int_safe in
        let gap = member "itemSpacing" node |> to_int_safe in

        (* Extract primary colors *)
        let bg_color = match member "fills" node with
          | `List (`Assoc fill :: _) ->
              (match List.assoc_opt "color" fill with
               | Some (`Assoc c) ->
                   let r = List.assoc_opt "r" c |> Option.map to_num |> Option.value ~default:1.0 in
                   let g = List.assoc_opt "g" c |> Option.map to_num |> Option.value ~default:1.0 in
                   let b = List.assoc_opt "b" c |> Option.map to_num |> Option.value ~default:1.0 in
                   (r, g, b)
               | _ -> (1.0, 1.0, 1.0))
          | _ -> (1.0, 1.0, 1.0)
        in
        let (bg_r, bg_g, bg_b) = bg_color in
        let bg_hex = sprintf "#%02x%02x%02x" (int_of_float (bg_r *. 255.0)) (int_of_float (bg_g *. 255.0)) (int_of_float (bg_b *. 255.0)) in

        (* Check layout mode *)
        let layout_mode = member "layoutMode" node |> to_string_option |> Option.value ~default:"NONE" in
        let is_vertical = layout_mode = "VERTICAL" in
        let is_horizontal = layout_mode = "HORIZONTAL" in

        (* Generate code for each platform *)
        let generate_for_platform platform =
          let code = match platform with
            | "react" ->
                let flex_dir = if is_vertical then "column" else if is_horizontal then "row" else "column" in
                sprintf {|import React from 'react';

interface %sProps {
  children?: React.ReactNode;
}

export const %s: React.FC<%sProps> = ({ children }) => {
  return (
    <div
      style={{
        width: %d,
        height: %d,
        backgroundColor: '%s',
        borderRadius: %.0f,
        padding: '%dpx %dpx %dpx %dpx',
        display: 'flex',
        flexDirection: '%s',
        gap: %d,
      }}
    >
      {children}
    </div>
  );
};
|}
                  safe_name safe_name safe_name
                  w h bg_hex radius
                  padding_t padding_r padding_b padding_l
                  flex_dir gap

            | "swiftui" ->
                let stack_type = if is_horizontal then "HStack" else "VStack" in
                sprintf {|import SwiftUI

struct %s: View {
    var body: some View {
        %s(spacing: %d) {
            // Children go here
        }
        .frame(width: %d, height: %d)
        .padding(.top, %d)
        .padding(.trailing, %d)
        .padding(.bottom, %d)
        .padding(.leading, %d)
        .background(Color(red: %.3f, green: %.3f, blue: %.3f))
        .cornerRadius(%.0f)
    }
}

#Preview {
    %s()
}
|}
                  safe_name
                  stack_type gap
                  w h
                  padding_t padding_r padding_b padding_l
                  bg_r bg_g bg_b
                  radius
                  safe_name

            | "compose" ->
                let arrangement = if is_horizontal then "Arrangement.spacedBy" else "Arrangement.spacedBy" in
                let container = if is_horizontal then "Row" else "Column" in
                sprintf {|import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp

@Composable
fun %s(
    modifier: Modifier = Modifier,
    content: @Composable %sScope.() -> Unit = {}
) {
    %s(
        modifier = modifier
            .size(width = %d.dp, height = %d.dp)
            .clip(RoundedCornerShape(%.0f.dp))
            .background(Color(0xFF%s))
            .padding(
                top = %d.dp,
                end = %d.dp,
                bottom = %d.dp,
                start = %d.dp
            ),
        %s(%d.dp),
        content = content
    )
}

@Preview
@Composable
private fun %sPreview() {
    %s()
}
|}
                  safe_name container
                  container
                  w h radius
                  (String.sub bg_hex 1 6)  (* remove # *)
                  padding_t padding_r padding_b padding_l
                  arrangement gap
                  safe_name safe_name

            | "flutter" ->
                let container = if is_horizontal then "Row" else "Column" in
                sprintf {|import 'package:flutter/material.dart';

class %s extends StatelessWidget {
  final List<Widget> children;

  const %s({
    super.key,
    this.children = const [],
  });

  @override
  Widget build(BuildContext context) {
    return Container(
      width: %d,
      height: %d,
      padding: const EdgeInsets.fromLTRB(%d, %d, %d, %d),
      decoration: BoxDecoration(
        color: const Color(0xFF%s),
        borderRadius: BorderRadius.circular(%.0f),
      ),
      child: %s(
        mainAxisSize: MainAxisSize.min,
        spacing: %d,
        children: children,
      ),
    );
  }
}
|}
                  safe_name safe_name
                  w h
                  padding_l padding_t padding_r padding_b
                  (String.sub bg_hex 1 6)
                  radius
                  container gap

            | _ -> sprintf "// Unsupported platform: %s" platform
          in
          (platform, code)
        in

        let results = List.map generate_for_platform platforms in
        let code_assoc = List.map (fun (p, c) -> (p, `String c)) results in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("platforms", `List (List.map (fun p -> `String p) platforms));
          ("code", `Assoc code_assoc);
          ("sharedTokens", `Assoc [
            ("width", `Int w);
            ("height", `Int h);
            ("backgroundColor", `String bg_hex);
            ("borderRadius", `Float radius);
            ("padding", `Assoc [
              ("top", `Int padding_t);
              ("right", `Int padding_r);
              ("bottom", `Int padding_b);
              ("left", `Int padding_l);
            ]);
            ("gap", `Int gap);
            ("layoutMode", `String layout_mode);
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/extract-variants - Extract component variants from Figma component set *)
let extract_variants_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* Extract variant properties from component set or variants *)
        let variants = ref [] in
        let props = Hashtbl.create 16 in

        let rec extract_variants_from ?(parent_is_set=false) n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in

          (* If parent is COMPONENT_SET and child has no type, treat as COMPONENT *)
          let effective_type = if node_type = "" && parent_is_set then "COMPONENT" else node_type in

          (* Parse variant name like "State=Default, Size=Medium" *)
          if effective_type = "COMPONENT" then begin
            let parts = String.split_on_char ',' name in
            let variant_props = List.filter_map (fun part ->
              match String.split_on_char '=' (String.trim part) with
              | [key; value] ->
                  let k = String.trim key in
                  let v = String.trim value in
                  (* Collect all values for each property *)
                  let existing = try Hashtbl.find props k with Not_found -> [] in
                  if not (List.mem v existing) then Hashtbl.replace props k (v :: existing);
                  Some (k, v)
              | _ -> None
            ) parts in
            if variant_props <> [] then
              variants := (name, variant_props, n) :: !variants
          end;

          (* Recurse into children (for component sets) *)
          let is_set = effective_type = "COMPONENT_SET" in
          (match member "children" n with
           | `List kids -> List.iter (extract_variants_from ~parent_is_set:is_set) kids
           | _ -> ())
        in
        extract_variants_from ~parent_is_set:false node;

        (* Generate TypeScript types from variants *)
        let prop_types = Hashtbl.fold (fun key values acc ->
          let union = String.concat " | " (List.map (sprintf "'%s'") (List.rev values)) in
          sprintf "  %s: %s;" key union :: acc
        ) props [] in

        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in

        let ts_types = sprintf {|interface %sProps {
%s
}

type %sVariant = {
  name: string;
  props: Partial<%sProps>;
};
|} safe_name (String.concat "\n" prop_types) safe_name safe_name in

        (* Build variant mapping *)
        let variant_list = List.map (fun (name, vprops, _n) ->
          let props_json = List.map (fun (k, v) -> (k, `String v)) vprops in
          `Assoc [("name", `String name); ("props", `Assoc props_json)]
        ) !variants in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("variantCount", `Int (List.length !variants));
          ("properties", `Assoc (Hashtbl.fold (fun k v acc ->
            (k, `List (List.map (fun x -> `String x) (List.rev v))) :: acc
          ) props []));
          ("variants", `List variant_list);
          ("typescript", `String ts_types);
        ] in
        let _ = to_num in (* suppress warning *)
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/responsive-breakpoints - Generate responsive code with breakpoints *)
let responsive_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let breakpoints = match member "breakpoints" json with
          | `List bps -> List.filter_map (fun bp ->
              match (member "name" bp |> to_string_option, member "width" bp |> to_int_option) with
              | (Some n, Some w) -> Some (n, w)
              | _ -> None
            ) bps
          | _ -> [("mobile", 375); ("tablet", 768); ("desktop", 1440)]
        in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in
        let to_int_safe json = int_of_float (to_num json) in

        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in

        let base_w = member "width" node |> to_int_safe in
        let base_h = member "height" node |> to_int_safe in
        let radius = member "cornerRadius" node |> to_num in
        let padding = member "paddingTop" node |> to_int_safe in

        (* Generate CSS with media queries *)
        let css_code = sprintf {|.%s {
  width: %dpx;
  height: %dpx;
  border-radius: %.0fpx;
  padding: %dpx;
}

%s
|}
          safe_name base_w base_h radius padding
          (String.concat "\n\n" (List.map (fun (bp_name, bp_width) ->
            let scale = float_of_int bp_width /. float_of_int base_w in
            sprintf {|/* %s (%dpx) */
@media (max-width: %dpx) {
  .%s {
    width: %dpx;
    height: %dpx;
    border-radius: %.0fpx;
    padding: %dpx;
  }
}|}
              bp_name bp_width bp_width
              safe_name
              bp_width
              (int_of_float (float_of_int base_h *. scale))
              (radius *. scale)
              (int_of_float (float_of_int padding *. scale))
          ) breakpoints))
        in

        (* Generate Tailwind classes *)
        let tailwind = sprintf "%s w-[%dpx] h-[%dpx] rounded-[%.0fpx] p-[%dpx] %s"
          safe_name base_w base_h radius padding
          (String.concat " " (List.map (fun (bp_name, bp_width) ->
            let scale = float_of_int bp_width /. float_of_int base_w in
            sprintf "%s:w-[%dpx] %s:h-[%dpx]"
              bp_name bp_width
              bp_name (int_of_float (float_of_int base_h *. scale))
          ) breakpoints))
        in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("baseWidth", `Int base_w);
          ("breakpoints", `List (List.map (fun (n, w) -> `Assoc [("name", `String n); ("width", `Int w)]) breakpoints));
          ("css", `String css_code);
          ("tailwind", `String tailwind);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/accessibility - Generate accessibility attributes *)
let accessibility_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in

        let suggestions = ref [] in
        let aria_attrs = ref [] in

        let rec analyze_accessibility n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in
          let ln = String.lowercase_ascii name in

          (* Helper to check if name contains keyword (anywhere, not just prefix) *)
          let contains_word s word =
            let re = Str.regexp_string word in
            try ignore (Str.search_forward re s 0); true
            with Not_found -> false
          in

          (* Detect interactive elements - check anywhere in name *)
          let is_button = contains_word ln "button" || contains_word ln "btn" || contains_word ln "cta" ||
                          contains_word ln "submit" || contains_word ln "cancel" || contains_word ln "action" in
          let is_link = contains_word ln "link" || contains_word ln "anchor" in
          let is_input = contains_word ln "input" || contains_word ln "search" || contains_word ln "field" ||
                         contains_word ln "text" || contains_word ln "email" || contains_word ln "password" in
          let is_image = contains_word ln "image" || contains_word ln "icon" || contains_word ln "avatar" ||
                         contains_word ln "photo" || contains_word ln "picture" || contains_word ln "img" in
          let is_nav = contains_word ln "nav" || contains_word ln "menu" || contains_word ln "sidebar" ||
                       contains_word ln "header" || contains_word ln "footer" in
          let is_modal = contains_word ln "modal" || contains_word ln "dialog" || contains_word ln "popup" ||
                         contains_word ln "overlay" || contains_word ln "drawer" in

          if is_button then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "button");
              ("aria-label", `String name);
              ("tabIndex", `Int 0);
            ] :: !aria_attrs;
            suggestions := sprintf "Button '%s': Add aria-label and ensure keyboard accessibility" name :: !suggestions
          end;

          if is_link then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "link");
              ("href", `String "#");
            ] :: !aria_attrs
          end;

          if is_input then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "textbox");
              ("aria-label", `String name);
              ("aria-required", `Bool false);
            ] :: !aria_attrs;
            suggestions := sprintf "Input '%s': Add label element or aria-label" name :: !suggestions
          end;

          if is_image then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "img");
              ("alt", `String name);
            ] :: !aria_attrs;
            suggestions := sprintf "Image '%s': Ensure alt text is descriptive" name :: !suggestions
          end;

          if is_nav then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "navigation");
              ("aria-label", `String name);
            ] :: !aria_attrs
          end;

          if is_modal then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "dialog");
              ("aria-modal", `Bool true);
              ("aria-labelledby", `String (name ^ "-title"));
            ] :: !aria_attrs;
            suggestions := sprintf "Modal '%s': Implement focus trap and escape key handling" name :: !suggestions
          end;

          (* Check for text contrast (simplified) *)
          if node_type = "TEXT" then begin
            let chars = member "text" n |> member "characters" |> to_string_option |> Option.value ~default:"" in
            if String.length chars > 0 then
              suggestions := sprintf "Text '%s': Verify color contrast meets WCAG AA (4.5:1)" name :: !suggestions
          end;

          (* Recurse *)
          (match member "children" n with
           | `List kids -> List.iter analyze_accessibility kids
           | _ -> ())
        in
        analyze_accessibility node;

        let result = `Assoc [
          ("ariaAttributes", `List (List.rev !aria_attrs));
          ("suggestions", `List (List.map (fun s -> `String s) (List.rev !suggestions)));
          ("summary", `Assoc [
            ("interactiveElements", `Int (List.length !aria_attrs));
            ("suggestions", `Int (List.length !suggestions));
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/export-assets - Export SVG/PNG assets from node *)
let export_assets_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let formats = match member "formats" json with
          | `List fs -> List.filter_map to_string_option fs
          | _ -> ["svg"; "png@1x"; "png@2x"]
        in

        (* Collect exportable assets (icons, images, logos) *)
        let assets = ref [] in

        let rec find_assets n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in
          let node_id = member "id" n |> to_string_option |> Option.value ~default:"" in
          let ln = String.lowercase_ascii name in

          let is_icon = String.length ln >= 4 && String.sub ln 0 4 = "icon" in
          let is_logo = String.length ln >= 4 && String.sub ln 0 4 = "logo" in
          let is_image = node_type = "VECTOR" || node_type = "ELLIPSE" ||
                         (node_type = "FRAME" && (is_icon || is_logo)) in

          if is_image || is_icon || is_logo then begin
            let export_settings = List.map (fun fmt ->
              let (format, scale) = match String.split_on_char '@' fmt with
                | [f; s] -> (f, (try float_of_string (String.sub s 0 (String.length s - 1)) with _ -> 1.0))
                | _ -> (fmt, 1.0)
              in
              `Assoc [
                ("format", `String format);
                ("scale", `Float scale);
                ("filename", `String (sprintf "%s%s.%s" name (if scale > 1.0 then sprintf "@%.0fx" scale else "") format));
              ]
            ) formats in
            assets := `Assoc [
              ("name", `String name);
              ("nodeId", `String node_id);
              ("type", `String node_type);
              ("exports", `List export_settings);
            ] :: !assets
          end;

          (match member "children" n with
           | `List kids -> List.iter find_assets kids
           | _ -> ())
        in
        find_assets node;

        let result = `Assoc [
          ("assets", `List (List.rev !assets));
          ("formats", `List (List.map (fun f -> `String f) formats));
          ("totalAssets", `Int (List.length !assets));
          ("note", `String "Use Figma API /images endpoint with nodeId to download actual files");
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/extract-animations - Extract animations and generate CSS/SwiftUI *)
let extract_animations_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* Extract transition/animation properties from prototype interactions *)
        let animations = ref [] in

        let rec extract_anims n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in

          (* Safe member access for nested JSON *)
          let safe_member_type obj =
            match obj with
            | `Null -> ""
            | _ -> member "type" obj |> to_string_option |> Option.value ~default:""
          in

          (* Check for Figma prototype interactions *)
          (match member "reactions" n with
           | `List reactions -> List.iter (fun reaction ->
               let trigger_obj = member "trigger" reaction in
               let trigger = safe_member_type trigger_obj in
               let action = member "action" reaction in
               let anim_type = safe_member_type action in

               if anim_type = "NODE" then begin
                 let transition = member "transition" action in
                 let trans_type = match transition with
                   | `Null -> "DISSOLVE"
                   | _ -> member "type" transition |> to_string_option |> Option.value ~default:"DISSOLVE"
                 in
                 let duration = match transition with
                   | `Null -> 0.3
                   | _ -> member "duration" transition |> to_num
                 in
                 let easing_obj = match transition with
                   | `Null -> `Null
                   | _ -> member "easing" transition
                 in
                 let easing = safe_member_type easing_obj in
                 let easing = if easing = "" then "EASE_OUT" else easing in

                 animations := `Assoc [
                   ("element", `String name);
                   ("trigger", `String trigger);
                   ("type", `String trans_type);
                   ("duration", `Float duration);
                   ("easing", `String easing);
                 ] :: !animations
               end
             ) reactions
           | _ -> ());

          (* Also check for smart animate properties *)
          let opacity = member "opacity" n |> to_num in
          let rotation = member "rotation" n |> to_num in
          if opacity < 1.0 || rotation <> 0.0 then begin
            animations := `Assoc [
              ("element", `String name);
              ("trigger", `String "state_change");
              ("properties", `Assoc [
                ("opacity", `Float opacity);
                ("rotation", `Float rotation);
              ]);
            ] :: !animations
          end;

          (match member "children" n with
           | `List kids -> List.iter extract_anims kids
           | _ -> ())
        in
        extract_anims node;

        (* Generate CSS keyframes based on actual transition type *)
        let safe_num key obj = match obj with `Null -> 0.0 | _ -> member key obj |> to_num in
        let generate_css_keyframes trans_type props =
          let opacity_from = safe_num "opacity" props in
          let opacity_to = if opacity_from < 1.0 then 1.0 else opacity_from in
          let rotation = safe_num "rotation" props in
          match trans_type with
          | "DISSOLVE" ->
              sprintf "from { opacity: %.2f; }\n  to { opacity: %.2f; }" opacity_from opacity_to
          | "MOVE_IN" | "SLIDE_IN" ->
              sprintf "from { opacity: 0; transform: translateX(-100%%); }\n  to { opacity: 1; transform: translateX(0); }"
          | "MOVE_OUT" | "SLIDE_OUT" ->
              sprintf "from { opacity: 1; transform: translateX(0); }\n  to { opacity: 0; transform: translateX(100%%); }"
          | "PUSH" ->
              sprintf "from { transform: scale(0.8); opacity: 0; }\n  to { transform: scale(1); opacity: 1; }"
          | "SMART_ANIMATE" ->
              let rot_css = if rotation <> 0.0 then sprintf " rotate(%.1fdeg)" rotation else "" in
              sprintf "from { opacity: %.2f; transform: translateY(20px)%s; }\n  to { opacity: 1; transform: translateY(0)%s; }"
                opacity_from rot_css rot_css
          | _ -> (* Default fade-in *)
              sprintf "from { opacity: %.2f; }\n  to { opacity: 1; }" (min opacity_from 0.0)
        in

        let css_animations = String.concat "\n\n" (List.mapi (fun i anim ->
          let name = member "element" anim |> to_string_option |> Option.value ~default:"element" in
          let duration = member "duration" anim |> to_num in
          let dur = if duration > 0.0 then duration else 0.3 in
          let easing = member "easing" anim |> to_string_option |> Option.value ~default:"EASE_OUT" in
          let trans_type = member "type" anim |> to_string_option |> Option.value ~default:"DISSOLVE" in
          let props = member "properties" anim in
          let css_easing = match easing with
            | "EASE_IN" -> "ease-in"
            | "EASE_OUT" -> "ease-out"
            | "EASE_IN_AND_OUT" -> "ease-in-out"
            | "LINEAR" -> "linear"
            | "CUSTOM_BEZIER" -> "cubic-bezier(0.4, 0, 0.2, 1)"
            | _ -> "ease-out"
          in
          let keyframes = generate_css_keyframes trans_type props in
          sprintf {|/* Animation %d: %s (type: %s) */
@keyframes %s_anim {
  %s
}

.%s {
  animation: %s_anim %.2fs %s forwards;
}|} i name trans_type name keyframes name name dur css_easing
        ) !animations) in

        (* Generate SwiftUI animations based on actual transition type *)
        let generate_swift_modifiers trans_type props =
          let opacity_from = safe_num "opacity" props in
          let rotation = safe_num "rotation" props in
          match trans_type with
          | "DISSOLVE" ->
              sprintf ".opacity(isAnimating ? 1 : %.2f)" opacity_from
          | "MOVE_IN" | "SLIDE_IN" ->
              ".opacity(isAnimating ? 1 : 0)\n.offset(x: isAnimating ? 0 : -UIScreen.main.bounds.width)"
          | "MOVE_OUT" | "SLIDE_OUT" ->
              ".opacity(isAnimating ? 0 : 1)\n.offset(x: isAnimating ? UIScreen.main.bounds.width : 0)"
          | "PUSH" ->
              ".scaleEffect(isAnimating ? 1 : 0.8)\n.opacity(isAnimating ? 1 : 0)"
          | "SMART_ANIMATE" ->
              let rot_mod = if rotation <> 0.0 then sprintf "\n.rotationEffect(.degrees(isAnimating ? 0 : %.1f))" rotation else "" in
              sprintf ".opacity(isAnimating ? 1 : %.2f)\n.offset(y: isAnimating ? 0 : 20)%s" opacity_from rot_mod
          | _ ->
              sprintf ".opacity(isAnimating ? 1 : %.2f)" (min opacity_from 0.0)
        in

        let swiftui_animations = String.concat "\n\n" (List.mapi (fun i anim ->
          let name = member "element" anim |> to_string_option |> Option.value ~default:"element" in
          let duration = member "duration" anim |> to_num in
          let dur = if duration > 0.0 then duration else 0.3 in
          let easing = member "easing" anim |> to_string_option |> Option.value ~default:"EASE_OUT" in
          let trans_type = member "type" anim |> to_string_option |> Option.value ~default:"DISSOLVE" in
          let props = member "properties" anim in
          let swift_easing = match easing with
            | "EASE_IN" -> ".easeIn"
            | "EASE_OUT" -> ".easeOut"
            | "EASE_IN_AND_OUT" -> ".easeInOut"
            | "LINEAR" -> ".linear"
            | "CUSTOM_BEZIER" -> ".timingCurve(0.4, 0, 0.2, 1)"
            | _ -> ".easeOut"
          in
          let modifiers = generate_swift_modifiers trans_type props in
          sprintf {|// Animation %d: %s (type: %s)
%s
.animation(%s(duration: %.2f), value: isAnimating)|} i name trans_type modifiers swift_easing dur
        ) !animations) in

        let result = `Assoc [
          ("animations", `List (List.rev !animations));
          ("count", `Int (List.length !animations));
          ("css", `String css_animations);
          ("swiftui", `String swiftui_animations);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /webhook/figma - Figma webhook handler for design changes *)
let webhook_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in

        (* Parse Figma webhook payload *)
        let event_type = member "event_type" json |> to_string_option |> Option.value ~default:"" in
        let file_key = member "file_key" json |> to_string_option |> Option.value ~default:"" in
        let file_name = member "file_name" json |> to_string_option |> Option.value ~default:"" in
        let timestamp = member "timestamp" json |> to_string_option |> Option.value ~default:"" in
        let passcode = member "passcode" json |> to_string_option |> Option.value ~default:"" in

        (* Verify webhook (in production, check passcode against stored secret) *)
        let _ = passcode in

        (* Log the webhook event *)
        printf "[Webhook] %s: file=%s (%s) at %s\n%!" event_type file_key file_name timestamp;

        (* Determine action based on event type *)
        let action = match event_type with
          | "FILE_UPDATE" -> "regenerate_code"
          | "FILE_VERSION_UPDATE" -> "sync_version"
          | "FILE_DELETE" -> "archive_code"
          | "LIBRARY_PUBLISH" -> "update_tokens"
          | _ -> "unknown"
        in

        (* Build response with recommended action *)
        let result = `Assoc [
          ("status", `String "received");
          ("event_type", `String event_type);
          ("file_key", `String file_key);
          ("file_name", `String file_name);
          ("timestamp", `String timestamp);
          ("recommended_action", `String action);
          ("webhook_endpoints", `Assoc [
            ("codegen", `String "/plugin/codegen");
            ("tokens", `String "/plugin/extract-tokens");
            ("variants", `String "/plugin/extract-variants");
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/code-to-figma - Convert code to Figma DSL *)
let code_to_figma_handler ~sw ~eio_ctx _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let target_name = member "name" json |> to_string_option |> Option.value ~default:"GeneratedComponent" in

        if String.length code < 10 then begin
          json_error "Code too short" reqd
        end else begin
          (* Build prompt for code → Figma DSL conversion *)
          let prompt = sprintf {|Analyze this React component and generate Figma node creation instructions.

CODE:
%s

Generate a JSON array of Figma node creation operations. Each operation should have:
- "action": one of "create_frame", "create_rectangle", "create_text", "create_ellipse"
- "name": node name
- "width", "height": dimensions in pixels
- "x", "y": position relative to parent
- "fills": array of {type: "SOLID", color: "#hexcolor"}
- "cornerRadius": optional, for rounded corners
- "children": nested operations array
- For text: "text", "fontSize", "fontWeight"

Output ONLY valid JSON array, no explanation. Example:
[
  {"action": "create_frame", "name": "Card", "width": 270, "height": 120, "fills": [{"type": "SOLID", "color": "#ffffff"}], "cornerRadius": 16, "children": [
    {"action": "create_text", "name": "Title", "x": 24, "y": 20, "text": "Hello", "fontSize": 16, "fills": [{"type": "SOLID", "color": "#1a1a26"}]}
  ]}
]|} code
          in

          (* Try LLM - Ollama for now *)
          let send_error msg =
            let result = `Assoc [("error", `String msg)] in
            Response.json (Yojson.Safe.to_string result) reqd
          in

          (try
            let cohttp = Figma_api_eio.get_cohttp_client eio_ctx.Mcp_tools.client in
            let ollama_url = "http://127.0.0.1:11434/api/generate" in
            let ollama_body = `Assoc [
              ("model", `String "qwen3-coder:30b");
              ("prompt", `String prompt);
              ("stream", `Bool false);
            ] in
            let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
            let req_body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string ollama_body) in
            let uri = Uri.of_string ollama_url in
            let resp, resp_body = Cohttp_eio.Client.post cohttp ~sw ~headers ~body:req_body uri in
            let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
            if status_code < 200 || status_code >= 300 then
              send_error "LLM request failed"
            else begin
              let ollama_resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:(10 * 1024 * 1024) in
              let ollama_resp = Yojson.Safe.from_string ollama_resp_str in
              let llm_output = member "response" ollama_resp |> to_string_option |> Option.value ~default:"[]" in
              (* Extract JSON from response (might have markdown code blocks) *)
              let json_str =
                if String.length llm_output > 0 && llm_output.[0] = '[' then llm_output
                else
                  let re = Str.regexp {|\[\(.*\)\]|} in
                  if Str.string_match re llm_output 0 then Str.matched_string llm_output
                  else "[]"
              in
              let result = `Assoc [
                ("operations", Yojson.Safe.from_string json_str);
                ("name", `String target_name);
                ("source", `String "ollama")
              ] in
              Response.json (Yojson.Safe.to_string result) reqd
            end
          with exn ->
            send_error (sprintf "Error: %s" (Printexc.to_string exn)))
        end
  )

(** POST /plugin/vision-compare - Compare Figma export with rendered code *)
let vision_compare_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let reference_path = member "reference" json |> to_string_option |> Option.value ~default:"" in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let width = member "width" json |> to_int_option |> Option.value ~default:375 in
        let height = member "height" json |> to_int_option |> Option.value ~default:812 in
        let threshold = member "threshold" json |> to_float_option |> Option.value ~default:0.95 in

        if reference_path = "" then begin
          json_error "reference path required" reqd
        end else if code = "" then begin
          json_error "code required" reqd
        end else begin
          (* Generate temp HTML file from code *)
          let html_path = sprintf "/tmp/figma-vision-%d.html" (int_of_float (Unix.gettimeofday () *. 1000.0)) in
          let rendered_path = sprintf "/tmp/figma-vision-%d-rendered.png" (int_of_float (Unix.gettimeofday () *. 1000.0)) in

          (* Wrap code in HTML boilerplate *)
          let html_content = sprintf {|<!DOCTYPE html>
<html><head>
<meta charset="UTF-8">
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: 'Inter', -apple-system, sans-serif; }
</style>
</head><body>
<div id="root">%s</div>
</body></html>|} code
          in

          (* Write HTML file *)
          let oc = open_out html_path in
          output_string oc html_content;
          close_out oc;

          (* Run render script *)
          let scripts_dir = Sys.getcwd () ^ "/scripts" in
          let render_cmd = sprintf "cd %s && node render-html.js %s %s %d %d 2>&1" scripts_dir html_path rendered_path width height in
          let render_result = Unix.open_process_in render_cmd in
          let _ = try input_line render_result with End_of_file -> "" in
          let _ = Unix.close_process_in render_result in

          (* Run SSIM comparison - using shell script (no Python deps) *)
          let ssim_cmd = sprintf "%s/ssim_compare.sh %s %s 2>&1" scripts_dir reference_path rendered_path in
          let ssim_result = Unix.open_process_in ssim_cmd in
          let ssim_output = try input_line ssim_result with End_of_file -> "{}" in
          let _ = Unix.close_process_in ssim_result in

          (* Parse SSIM result *)
          (try
            let ssim_json = Yojson.Safe.from_string ssim_output in
            let ssim_score = member "ssim" ssim_json |> to_float_option |> Option.value ~default:0.0 in
            let passed = ssim_score >= threshold in

            (* Cleanup temp files *)
            (try Sys.remove html_path with _ -> ());
            if passed then (try Sys.remove rendered_path with _ -> ());

            let result = `Assoc [
              ("ssim", `Float ssim_score);
              ("threshold", `Float threshold);
              ("pass", `Bool passed);
              ("reference", `String reference_path);
              ("rendered", `String (if passed then "(cleaned up)" else rendered_path));
            ] in
            Response.json (Yojson.Safe.to_string result) reqd
          with _ ->
            let result = `Assoc [("error", `String "SSIM comparison failed"); ("raw", `String ssim_output)] in
            Response.json (Yojson.Safe.to_string result) reqd)
        end
  )

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

  | `POST, "/plugin/codegen" ->
      plugin_codegen_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/template" ->
      template_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/code-to-figma" ->
      code_to_figma_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/vision-compare" ->
      vision_compare_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/analyze" ->
      plugin_analyze_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/extract-tokens" ->
      extract_tokens_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/generate-story" ->
      generate_story_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/codegen-multi" ->
      codegen_multi_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/extract-variants" ->
      extract_variants_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/responsive-breakpoints" ->
      responsive_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/accessibility" ->
      accessibility_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/export-assets" ->
      export_assets_handler ~sw ~eio_ctx request reqd

  | `POST, "/plugin/extract-animations" ->
      extract_animations_handler ~sw ~eio_ctx request reqd

  | `POST, "/webhook/figma" ->
      webhook_handler ~sw ~eio_ctx request reqd

  (* Agent Queue endpoints *)
  | `POST, "/agent/request" ->
      agent_request_handler request reqd

  | `GET, "/agent/pending" ->
      agent_pending_handler request reqd

  | `POST, "/agent/result" ->
      agent_result_handler request reqd

  | `GET, path when String.length path > 14 && String.sub path 0 14 = "/agent/status/" ->
      agent_status_handler request reqd

  | _ ->
      Response.not_found reqd

(** ============== httpun-eio Server ============== *)

let make_request_handler ~clock ~domain_mgr ~sw ~eio_ctx server =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    try
      route_request ~clock ~domain_mgr ~sw ~eio_ctx server request reqd
    with exn ->
      let msg = Printf.sprintf "Internal Server Error: %s" (Printexc.to_string exn) in
      Response.text ~status:`Internal_server_error msg reqd

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

      (* Give clients 200ms to receive the notification *)
      Unix.sleepf 0.2;

      (* Gracefully close all SSE connections before Switch.fail *)
      close_all_sse_connections ();

      (* Give connections 200ms to complete close handshake *)
      Unix.sleepf 0.2;

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
