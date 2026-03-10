open Printf
open Mcp_agent_queue
module Request = Mcp_protocol_request.Request
module Response = Mcp_protocol_request.Response
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
