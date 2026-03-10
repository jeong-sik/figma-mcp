(** MCP Protocol Eio - Pure Eio HTTP/stdio 서버

    Pure Eio-native server for MCP protocol.
    No Lwt dependencies - uses cohttp-eio for all HTTP operations.

    Architecture:
    - HTTP Server: httpun-eio (Eio native, Effect-based)
    - HTTP Client: cohttp-eio (Pure Eio)
    - JSON-RPC: Reuses types from figma_mcp_protocol.ml
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

(* Agent queue types, state machine, and utilities — see mcp_agent_queue.ml *)
include Mcp_agent_queue

(* Figma tool HTTP handlers — see mcp_figma_tool_handlers.ml *)
include Mcp_figma_tool_handlers


(* ============== Request/Response Helpers ============== *)
(* Re-exported from extracted modules — see mcp_cors.ml, mcp_http_helpers.ml *)
module Cors = Mcp_cors
module Response = Mcp_http_helpers.Response
module Request = Mcp_http_helpers.Request

(** ============== MCP Request Processing ============== *)

(** Process MCP request synchronously (Eio-native, no Lwt).
    Uses process_request_sync which calls handlers_sync directly. *)
let process_mcp_request_sync (server : Figma_mcp_protocol.mcp_server) body_str =
  match Figma_mcp_protocol.parse_request body_str with
  | Ok req ->
      (* process_request_sync: Lwt 없이 직접 실행 *)
      let response_json = Figma_mcp_protocol.process_request_sync server req in
      Yojson.Safe.to_string response_json
  | Error msg ->
      let err_response = Figma_mcp_protocol.make_error_response
        `Null Figma_mcp_protocol.parse_error msg None in
      Yojson.Safe.to_string err_response

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
                let err = Figma_mcp_protocol.make_error_response `Null
                  Figma_mcp_protocol.internal_error "Internal server error" None in
                if wants_sse then
                  Response.sse_message (Yojson.Safe.to_string err) reqd
                else
                  Response.json ~status:`Internal_server_error (Yojson.Safe.to_string err) reqd)))

(* ============== Plugin Bridge Handlers ============== *)

(* Plugin configuration from centralized Figma_config *)
let plugin_ttl_seconds = Figma_config.Plugin.ttl_seconds
let plugin_poll_max_ms = Figma_config.Plugin.poll_max_ms

let plugin_cleanup () =
  Figma_plugin_bridge.cleanup_inactive ~ttl_seconds:plugin_ttl_seconds

(* Re-exported from Mcp_http_helpers — shared JSON utilities *)
let json_error = Mcp_http_helpers.json_error
let parse_json = Mcp_http_helpers.parse_json
let get_string_field = Mcp_http_helpers.get_string_field
let get_int_field = Mcp_http_helpers.get_int_field
let get_bool_field = Mcp_http_helpers.get_bool_field
let get_payload_field = Mcp_http_helpers.get_payload_field

let clamp_poll_ms value =
  let value = max 0 value in
  if value > plugin_poll_max_ms then plugin_poll_max_ms else value

let clamp_max_commands value =
  let value = max 1 value in
  if value > Figma_config.Plugin.max_commands then
    Figma_config.Plugin.max_commands
  else
    value

[@@@coverage off]
let wait_for_commands ~clock ~channel_id ~max ~timeout_ms =
  let commands = Figma_plugin_bridge.poll_commands ~channel_id ~max in
  if commands <> [] || timeout_ms <= 0 then
    commands
  else begin
    let promise, resolver = Eio.Promise.create () in
    let waiter_id =
      Figma_plugin_bridge.register_waiter ~channel_id ~notify:(fun () ->
        try Eio.Promise.resolve resolver ()
        with exn -> eprintf "[mcp_protocol] Warning: promise double-resolve: %s\n%!" (Printexc.to_string exn))
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
             let max_commands =
               get_int_field "max_commands" json
               |> Option.value ~default:1
               |> clamp_max_commands
             in
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
              with Yojson.Json_error _ -> `Assoc [
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

let plugin_event_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    plugin_cleanup ();
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let channel_id = get_string_field "channel_id" json in
        let event_type = get_string_field "event_type" json in
        let payload =
          match get_payload_field "payload" json with
          | Some p -> p
          | None -> `Null
        in
        (match (channel_id, event_type) with
         | (Some channel_id, Some event_type) ->
             Figma_plugin_bridge.publish_event ~channel_id ~event_type ~payload;
             eprintf "[Plugin] event channel=%s type=%s\n%!"
               channel_id event_type;
             let body = `Assoc [("status", `String "ok")] in
             Response.json (Yojson.Safe.to_string body) reqd
         | _ ->
             json_error "Missing channel_id or event_type" reqd))

let plugin_status_handler _request reqd =
  plugin_cleanup ();
  let stats = Figma_plugin_bridge.list_channel_stats () in
  let default_channel = Figma_plugin_bridge.get_default_channel () in
  let stats_json =
    `List (List.map (fun (s : Figma_plugin_bridge.channel_stats) ->
      `Assoc [
        ("id", `String s.id);
        ("last_seen", `Float s.last_seen);
        ("commands", `Int s.commands);
        ("results", `Int s.results);
        ("waiters", `Int s.waiters);
      ]) stats)
  in
  let limits = `Assoc [
    ("max_commands", `Int Figma_config.Plugin.max_commands);
    ("max_results", `Int Figma_config.Plugin.max_results);
    ("max_waiters", `Int Figma_config.Plugin.max_waiters);
    ("result_ttl_seconds", `Float Figma_config.Plugin.result_ttl_seconds);
    ("cleanup_interval_seconds", `Float Figma_config.Plugin.cleanup_interval_seconds);
    ("poll_max_ms", `Int Figma_config.Plugin.poll_max_ms);
  ] in
  let channels_json =
    `List (List.map (fun (s : Figma_plugin_bridge.channel_stats) ->
      let age = Unix.gettimeofday () -. s.last_seen in
      `Assoc [
        ("channel_id", `String s.id);
        ("active", `Bool (age < 120.0));
        ("age_seconds", `Float (Float.round age));
        ("commands", `Int s.commands);
        ("results", `Int s.results);
        ("waiters", `Int s.waiters);
      ]) stats)
  in
  let body = `Assoc [
    ("channels", channels_json);
    ("stats", stats_json);
    ("limits", limits);
    ("default_channel", match default_channel with Some id -> `String id | None -> `Null);
  ] in
  Response.json (Yojson.Safe.to_string body) reqd


[@@@coverage on]
(* ============== Agent Queue Handlers ============== *)

[@@@coverage off]
(* POST /agent/request - Plugin submits a codegen request to queue *)
let agent_request_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platform = member "platform" json |> to_string_option |> Option.value ~default:"react" in
        let prompt = member "prompt" json |> to_string_option |> Option.value ~default:"" in
        let priority = member "priority" json |> to_int_option |> Option.value ~default:0 in
        let ctx_digest = member "context_digest" json |> to_string_option |> Option.value ~default:"" in
        let node_info = Yojson.Safe.to_string node in
        let full_prompt = if prompt = "" then
          sprintf "Convert this Figma node to %s code:\n%s\n\nGenerate clean, production-ready code." platform node_info
        else prompt in
        let context_digest =
          if ctx_digest <> "" then ctx_digest
          else Digest.to_hex (Digest.string (full_prompt ^ "\n" ^ node_info))
        in
        let (req_id, request_secret) =
          agent_add_request ~priority ~context_digest node platform full_prompt
        in
        let result = `Assoc [
          ("request_id", `String req_id);
          ("request_secret", `String request_secret);
          ("status", `String "queued");
          ("priority", `Int priority);
          ("context_digest", `String context_digest);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** GET /agent/pending - Agent polls for pending requests *)
let agent_pending_handler _request reqd =
  agent_cleanup_old ();
  let pending = agent_get_pending () in
  let requests = List.map (fun req ->
    agent_request_json ~include_node:false ~include_prompt:false req
  ) pending in
  let result = `Assoc [("pending", `List requests); ("count", `Int (List.length pending))] in
  Response.json (Yojson.Safe.to_string result) reqd

(** POST /agent/claim - Agent claims a pending request *)
let agent_claim_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let worker_id = member "worker_id" json |> to_string_option |> Option.value ~default:"" in
        if worker_id = "" then json_error "worker_id required" reqd
        else begin
          agent_cleanup_old ();
          match agent_claim ~worker_id with
          | None ->
              let result = `Assoc [("status", `String "empty")] in
              Response.json (Yojson.Safe.to_string result) reqd
          | Some req ->
              let result = `Assoc [
                ("status", `String "claimed");
                ("request", agent_request_json ~include_claim_token:true ~include_node:true ~include_prompt:true req);
              ] in
              Response.json (Yojson.Safe.to_string result) reqd
        end
  )

(** POST /agent/heartbeat - Agent keeps claim alive *)
let agent_heartbeat_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let worker_id = member "worker_id" json |> to_string_option |> Option.value ~default:"" in
        let req_id = member "request_id" json |> to_string_option |> Option.value ~default:"" in
        let claim_token = member "claim_token" json |> to_string_option |> Option.value ~default:"" in
        if worker_id = "" || req_id = "" || claim_token = "" then
          json_error "worker_id, request_id and claim_token required" reqd
        else
          match agent_heartbeat ~worker_id ~claim_token req_id with
          | Ok () ->
              let result = `Assoc [("status", `String "ok"); ("request_id", `String req_id)] in
              Response.json (Yojson.Safe.to_string result) reqd
          | Error msg -> json_error msg reqd
  )

(** POST /agent/abandon - Agent releases claim *)
let agent_abandon_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let worker_id = member "worker_id" json |> to_string_option |> Option.value ~default:"" in
        let req_id = member "request_id" json |> to_string_option |> Option.value ~default:"" in
        let reason = member "reason" json |> to_string_option |> Option.value ~default:"abandoned" in
        let claim_token = member "claim_token" json |> to_string_option |> Option.value ~default:"" in
        if worker_id = "" || req_id = "" || claim_token = "" then
          json_error "worker_id, request_id and claim_token required" reqd
        else
          match agent_abandon ~worker_id ~claim_token ~reason req_id with
          | Ok () ->
              let result = `Assoc [("status", `String "ok"); ("request_id", `String req_id)] in
              Response.json (Yojson.Safe.to_string result) reqd
          | Error msg -> json_error msg reqd
  )

(** POST /agent/result - Agent submits generated code *)
let agent_result_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let req_id = member "request_id" json |> to_string_option |> Option.value ~default:"" in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let claim_token = member "claim_token" json |> to_string_option |> Option.value ~default:"" in
        let worker_id = member "worker_id" json |> to_string_option in
        let context_digest = member "context_digest" json |> to_string_option in
        if req_id = "" || code = "" || claim_token = "" then
          json_error "request_id, code and claim_token required" reqd
        else
          match agent_submit_result ?worker_id ?context_digest ~claim_token req_id code with
          | Ok () ->
              let result = `Assoc [("status", `String "submitted"); ("request_id", `String req_id)] in
              Response.json (Yojson.Safe.to_string result) reqd
          | Error msg -> json_error msg reqd
  )

(** GET /agent/status/:id - Check request status *)
let agent_status_handler request reqd =
  let path = Request.path request in
  let req_id = String.sub path 14 (String.length path - 14) in (* /agent/status/ = 14 chars *)
  let request_secret =
    match Httpun.Headers.get request.Httpun.Request.headers "x-mcp-request-secret" with
    | Some v ->
        let v = String.trim v in
        if v = "" then None else Some v
    | None -> None
  in
  match agent_get_result req_id with
  | Some req ->
      let authorized =
        match request_secret with
        | Some s -> s = req.request_secret
        | None -> false
      in
      let base = [
        ("status", `String (agent_status_to_string req.status));
        ("request_id", `String req.id);
        ("priority", `Int req.priority);
        ("context_digest", `String req.context_digest);
        ("claimed_by", (match req.claimed_by with Some v -> `String v | None -> `Null));
        ("claimed_at", (match req.claimed_at with Some v -> `Float v | None -> `Null));
        ("last_heartbeat", (match req.last_heartbeat with Some v -> `Float v | None -> `Null));
        ("attempts", `Int req.attempts);
        ("drifted", `Bool req.drifted);
        ("error", (match req.error with Some v -> `String v | None -> `Null));
        ("age_sec", `Float (Unix.gettimeofday () -. req.created_at));
        ("code_available", `Bool (req.status = Completed && req.result <> None));
        ("authorized", `Bool authorized);
      ] in
      let result =
        match req.status, req.result with
        | Completed, Some code when authorized -> `Assoc (("code", `String code) :: base)
        | _ -> `Assoc base
      in
      Response.json (Yojson.Safe.to_string result) reqd
  | None ->
      let result = `Assoc [("status", `String "not_found")] in
      Response.json (Yojson.Safe.to_string result) reqd

(** GET /agent/queue - Queue monitoring *)
let agent_queue_handler _request reqd =
  agent_cleanup_old ();
  let items =
    Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
      Hashtbl.fold (fun _ req acc -> req :: acc) agent_queue [])
  in
  let payload = List.map (fun req ->
    agent_request_json ~include_node:false ~include_prompt:false req
  ) items in
  let result = `Assoc [
    ("stats", agent_queue_stats_json ());
    ("items", `List payload);
    ("count", `Int (List.length items));
  ] in
  Response.json (Yojson.Safe.to_string result) reqd

[@@@coverage on]
(* ============== Router ============== *)

let is_public_path meth path =
  match (meth, path) with
  | (`OPTIONS, _) -> true
  | (`GET, "/health") -> true
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
          ~allow_no_auth:!allow_no_auth
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

      | `POST, "/plugin/event" ->
          plugin_event_handler request reqd

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

      | `POST, "/agent/claim" ->
          agent_claim_handler request reqd

      | `POST, "/agent/heartbeat" ->
          agent_heartbeat_handler request reqd

      | `POST, "/agent/abandon" ->
          agent_abandon_handler request reqd

      | `GET, "/agent/pending" ->
          agent_pending_handler request reqd

      | `POST, "/agent/result" ->
          agent_result_handler request reqd

      | `GET, path when String.length path > 14 && String.sub path 0 14 = "/agent/status/" ->
          agent_status_handler request reqd

      | `GET, "/agent/queue" ->
          agent_queue_handler request reqd

      | _ ->
          Response.not_found reqd
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

(** Run HTTP server with Eio *)
let run ~sw ~net ~clock ~domain_mgr config server =
  (* Set Eio context for pure Eio handlers (Lwt-free path) *)
  let eio_client = Figma_api_eio.make_client net in
  let eio_ctx = Mcp_helpers.set_eio_context ~sw ~net ~clock ~client:eio_client in
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
        Figma_mcp_protocol.server_name
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
             Eio.Switch.run (fun conn_sw ->
               (* httpun_eio calls shutdown but not close; we close on release.
                  Switch.on_release handles normal exit, exceptions, and cancellation. *)
               Eio.Switch.on_release conn_sw (fun () ->
                 try Eio.Flow.close flow
                 with exn ->
                   eprintf "[%s] Flow close error: %s\n%!"
                     Figma_mcp_protocol.server_name
                     (Printexc.to_string exn));
               Httpun_eio.Server.create_connection_handler
                 ~sw:conn_sw
                 ~request_handler
                 ~error_handler
                 client_addr
                 flow)
           )
         with exn ->
           if is_cancelled exn then raise exn;
           let delay = !backoff_s in
           eprintf "[%s] Accept error: %s (backoff %.2fs)\n%!"
             Figma_mcp_protocol.server_name
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
            Figma_mcp_protocol.server_name
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

  eprintf "🎨 %s MCP Server %s (Eio)\n" Figma_mcp_protocol.server_name Figma_mcp_protocol.server_version;
  eprintf "   Protocol: %s\n" Figma_mcp_protocol.protocol_version;
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
         eprintf "[Plugin] cleanup loop error: %s\n%!" (Printexc.to_string exn);
         Eio.Time.sleep clock 5.0);  (* backoff before retry *)
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
      eprintf "\n🎨 %s: Received %s, shutting down gracefully...\n%!" Figma_mcp_protocol.server_name signal_name;

      (* Broadcast shutdown notification to all SSE clients *)
      broadcast_sse_shutdown signal_name;
      eprintf "🎨 %s: Sent shutdown notification to %d SSE clients\n%!" Figma_mcp_protocol.server_name (Hashtbl.length sse_clients);

      (* Give clients 200ms to receive the notification.
         NOTE: Unix.sleepf is intentional here. This closure runs as a POSIX signal
         handler (Sys.set_signal) which executes outside Eio fiber context, so
         Eio.Time.sleep is not available. Blocking the OS thread is acceptable
         during shutdown. *)
      Unix.sleepf 0.2;

      (* Gracefully close all SSE connections before Switch.fail *)
      close_all_sse_connections ();

      (* Give connections 200ms to complete close handshake (same signal handler
         context constraint as above) *)
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
      eprintf "🎨 %s: Shutdown complete.\n%!" Figma_mcp_protocol.server_name
  | Eio.Cancel.Cancelled _ ->
      eprintf "🎨 %s: Shutdown complete.\n%!" Figma_mcp_protocol.server_name)

(** ============== stdio Server (Pure Eio) ============== *)

(** Run stdio server with Eio - blocking loop reading from stdin *)
let run_stdio ~sw ~env ~net ~clock server =
  (* Set Eio context for pure Eio handlers *)
  let eio_client = Figma_api_eio.make_client net in
  ignore (Mcp_helpers.set_eio_context ~sw ~net ~clock ~client:eio_client);

  eprintf "[%s] MCP Server started (protocol: %s, mode: stdio/Eio)\n%!"
    Figma_mcp_protocol.server_name Figma_mcp_protocol.protocol_version;

  (* Create buffered reader for stdin *)
  let stdin_flow = Eio.Stdenv.stdin env in
  let buf_read = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdin_flow in

  let rec read_loop () =
    match Eio.Buf_read.line buf_read with
    | line ->
        if String.trim line <> "" then begin
          match Figma_mcp_protocol.parse_request line with
          | Ok req ->
              if Figma_mcp_protocol.is_notification req then
                (* Notification: no response on stdout per JSON-RPC *)
                ignore (Figma_mcp_protocol.process_request_sync server req)
              else begin
                (* Process request using sync handler (runs in Eio context) *)
                let response = Figma_mcp_protocol.process_request_sync server req in
                let response_str = Yojson.Safe.to_string response in
                print_endline response_str;
                flush stdout
              end
          | Error msg ->
              let err_response = Figma_mcp_protocol.make_error_response `Null Figma_mcp_protocol.parse_error msg None in
              print_endline (Yojson.Safe.to_string err_response);
              flush stdout
        end;
        read_loop ()
    | exception End_of_file ->
        eprintf "[%s] Connection closed (EOF)\n%!" Figma_mcp_protocol.server_name
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
        eprintf "[%s] Error: Input line too long\n%!" Figma_mcp_protocol.server_name
    | exception exn ->
        eprintf "[%s] Error: %s\n%!" Figma_mcp_protocol.server_name (Printexc.to_string exn)
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
      eprintf "\n[%s] Received %s, shutting down...\n%!" Figma_mcp_protocol.server_name signal_name;
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
      eprintf "[%s] Shutdown complete.\n%!" Figma_mcp_protocol.server_name
  | Eio.Cancel.Cancelled _ ->
      eprintf "[%s] Shutdown complete.\n%!" Figma_mcp_protocol.server_name)
