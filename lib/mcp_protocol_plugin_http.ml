open Printf

module Request = Mcp_protocol_request.Request
module Response = Mcp_protocol_request.Response

let plugin_ttl_seconds = Figma_config.Plugin.ttl_seconds
let plugin_poll_max_ms = Figma_config.Plugin.poll_max_ms

let plugin_cleanup () =
  Figma_plugin_bridge.cleanup_inactive ~ttl_seconds:plugin_ttl_seconds

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
          with exn ->
            eprintf "[mcp_protocol] Warning: promise double-resolve: %s\n%!"
              (Printexc.to_string exn))
    in
    let commands_after = Figma_plugin_bridge.poll_commands ~channel_id ~max in
    if commands_after <> [] then begin
      Figma_plugin_bridge.unregister_waiter ~channel_id ~waiter_id;
      commands_after
    end else begin
      let wait_s = float_of_int timeout_ms /. 1000.0 in
      let result =
        match
          Eio.Time.with_timeout clock wait_s (fun () ->
              Eio.Promise.await promise;
              Ok `Woke)
        with
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
          let body =
            `Assoc [ ("status", `String "ok"); ("channel_id", `String channel_id) ]
          in
          Response.json (Yojson.Safe.to_string body) reqd)

let plugin_poll_handler ~clock _request reqd =
  Request.read_body_async reqd (fun body_str ->
      plugin_cleanup ();
      match parse_json body_str with
      | Error msg -> json_error msg reqd
      | Ok json -> (
          match get_string_field "channel_id" json with
          | None -> json_error "Missing channel_id" reqd
          | Some channel_id ->
              let max_commands =
                get_int_field "max_commands" json
                |> Option.value ~default:1 |> clamp_max_commands
              in
              let wait_ms =
                match get_int_field "wait_ms" json with
                | Some value -> clamp_poll_ms value
                | None -> (
                    match get_int_field "timeout_ms" json with
                    | Some value -> clamp_poll_ms value
                    | None -> 0)
              in
              let commands : Figma_plugin_bridge.command list =
                if wait_ms > 0 then
                  wait_for_commands ~clock ~channel_id ~max:max_commands
                    ~timeout_ms:wait_ms
                else Figma_plugin_bridge.poll_commands ~channel_id ~max:max_commands
              in
              if commands <> [] then
                eprintf
                  "[Plugin] poll channel=%s max=%d wait_ms=%d -> %d commands\n%!"
                  channel_id max_commands wait_ms (List.length commands);
              let commands_json =
                `List
                  (List.map
                     (fun (cmd : Figma_plugin_bridge.command) ->
                       `Assoc
                         [
                           ("id", `String cmd.id);
                           ("name", `String cmd.name);
                           ("payload", cmd.payload);
                           ("created_at", `Float cmd.created_at);
                         ])
                     commands)
              in
              let body =
                `Assoc
                  [
                    ("channel_id", `String channel_id);
                    ("commands", commands_json);
                  ]
              in
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
                with Yojson.Json_error _ ->
                  `Assoc
                    [ ("error", `String "Failed to parse payload string"); ("raw", `String s) ])
            | Some payload -> payload
            | None -> `Null
          in
          match (channel_id, command_id) with
          | Some channel_id, Some command_id ->
              Figma_plugin_bridge.store_result ~channel_id ~command_id ~ok ~payload;
              eprintf "[Plugin] result channel=%s cmd=%s ok=%b\n%!" channel_id
                command_id ok;
              Response.json {|{"status":"ok"}|} reqd
          | _ -> json_error "Missing channel_id or command_id" reqd)

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
          match (channel_id, event_type) with
          | Some channel_id, Some event_type ->
              Figma_plugin_bridge.publish_event ~channel_id ~event_type ~payload;
              eprintf "[Plugin] event channel=%s type=%s\n%!" channel_id event_type;
              Response.json {|{"status":"ok"}|} reqd
          | _ -> json_error "Missing channel_id or event_type" reqd)

let plugin_status_handler _request reqd =
  plugin_cleanup ();
  let stats = Figma_plugin_bridge.list_channel_stats () in
  let default_channel = Figma_plugin_bridge.get_default_channel () in
  let stats_json =
    `List
      (List.map
         (fun (s : Figma_plugin_bridge.channel_stats) ->
           `Assoc
             [
               ("id", `String s.id);
               ("last_seen", `Float s.last_seen);
               ("commands", `Int s.commands);
               ("results", `Int s.results);
               ("waiters", `Int s.waiters);
             ])
         stats)
  in
  let limits =
    `Assoc
      [
        ("max_commands", `Int Figma_config.Plugin.max_commands);
        ("max_results", `Int Figma_config.Plugin.max_results);
        ("max_waiters", `Int Figma_config.Plugin.max_waiters);
        ("result_ttl_seconds", `Float Figma_config.Plugin.result_ttl_seconds);
        ( "cleanup_interval_seconds",
          `Float Figma_config.Plugin.cleanup_interval_seconds );
        ("poll_max_ms", `Int Figma_config.Plugin.poll_max_ms);
      ]
  in
  let channels_json =
    `List
      (List.map
         (fun (s : Figma_plugin_bridge.channel_stats) ->
           let age = Unix.gettimeofday () -. s.last_seen in
           `Assoc
             [
               ("channel_id", `String s.id);
               ("active", `Bool (age < 120.0));
               ("age_seconds", `Float (Float.round age));
               ("commands", `Int s.commands);
               ("results", `Int s.results);
               ("waiters", `Int s.waiters);
             ])
         stats)
  in
  let body =
    `Assoc
      [
        ("channels", channels_json);
        ("stats", stats_json);
        ("limits", limits);
        ( "default_channel",
          match default_channel with Some id -> `String id | None -> `Null );
      ]
  in
  Response.json (Yojson.Safe.to_string body) reqd

[@@@coverage on]
