(** Plugin handler infrastructure and all handle_plugin_* handlers.
    Extracted from mcp_tools.ml for maintainability. *)

open Mcp_helpers
open Printf

(** ============== Plugin Infrastructure ============== *)

let resolve_channel_id args =
  match get_string "channel_id" args with
  | Some id -> Ok id
  | None ->
      (match Figma_plugin_bridge.get_default_channel () with
       | Some id -> Ok id
       | None -> Error "Missing channel_id. Run figma_plugin_connect or figma_plugin_use_channel.")

let plugin_wait ~channel_id ~command_id ~timeout_ms =
  (* Pre-check: verify channel exists and plugin is connected *)
  let channel_exists =
    List.exists (fun (s : Figma_plugin_bridge.channel_stats) -> s.id = channel_id)
      (Figma_plugin_bridge.list_channel_stats ())
  in
  if not channel_exists then
    Error (Printf.sprintf "Channel %s not found. Plugin may be disconnected — reopen it in Figma." channel_id)
  else
    let wait_fn =
      match get_eio_context () with
      | Some ctx ->
          let (Clock clock) = ctx.clock in
          Figma_plugin_bridge.wait_for_result_with_sleep
            ~sleep:(Eio.Time.sleep clock)
      | None ->
          Figma_plugin_bridge.wait_for_result
    in
    match wait_fn ~channel_id ~command_id ~timeout_ms with
    | Some result -> Ok result
    | None ->
        Error (Printf.sprintf
          "Plugin timeout after %dms (channel: %s, cmd: %s). Check: 1) Plugin window open in Figma 2) Server URL correct 3) Increase timeout_ms"
          timeout_ms channel_id command_id)

let truncate_string ?(max_len=2000) s =
  if String.length s <= max_len then s
  else (String.sub s 0 max_len) ^ "…"

let plugin_error_message payload =
  let list_hd_opt = function
    | [] -> None
    | x :: _ -> Some x
  in
  let first_string_in_list = function
    | `List items ->
        items
        |> List.filter_map (function `String s -> Some s | _ -> None)
        |> list_hd_opt
    | _ -> None
  in
  let assoc_string key fields =
    match List.assoc_opt key fields with
    | Some (`String s) -> Some s
    | Some v -> (
        match first_string_in_list v with
        | Some s -> Some s
        | None -> None)
    | None -> None
  in
  let raw =
    match payload with
    | `String s -> s
    | `Assoc fields -> (
        match assoc_string "error" fields with
        | Some s -> s
        | None -> (
            match assoc_string "message" fields with
            | Some s -> s
            | None -> Yojson.Safe.to_string payload))
    | _ -> Yojson.Safe.to_string payload
  in
  truncate_string raw

(** Execute plugin command and return result *)
let plugin_exec ~channel_id ~name ~payload ~timeout_ms =
  let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name ~payload in
  match plugin_wait ~channel_id ~command_id ~timeout_ms with
  | Error err -> Error err
  | Ok result ->
      if result.ok then
        Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload))
      else
        Error (Printf.sprintf "Plugin error: %s" (plugin_error_message result.payload))

(** Simple handler - just needs channel_id *)
let plugin_simple ~name ?(default_timeout=10000) ~build_payload args =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:default_timeout in
      let payload = build_payload args in
      plugin_exec ~channel_id ~name ~payload ~timeout_ms

(** Node handler - needs node_id + channel_id *)
let plugin_node ~name ?(default_timeout=10000) ~build_payload args =
  match (resolve_node_id args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id (or url)"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:default_timeout in
      let payload = build_payload node_id args in
      plugin_exec ~channel_id ~name ~payload ~timeout_ms

(** Nodes handler - needs node_ids array + channel_id *)
let plugin_nodes ~name ?(default_timeout=10000) ~build_payload args =
  match (get_string_list "node_ids" args, resolve_channel_id args) with
  | (None, _) | (Some [], _) -> Error "Missing required parameter: node_ids"
  | (_, Error msg) -> Error msg
  | (Some ids, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:default_timeout in
      let payload = build_payload ids args in
      plugin_exec ~channel_id ~name ~payload ~timeout_ms

(** Custom handler - for special cases requiring custom validation *)
let plugin_custom ~name ?(default_timeout=10000) ~validate ~build_payload args =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      (match validate args with
       | Error msg -> Error msg
       | Ok validated ->
           let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:default_timeout in
           let payload = build_payload validated args in
           plugin_exec ~channel_id ~name ~payload ~timeout_ms)

(** ============== Plugin Handlers ============== *)

(** figma_plugin_connect 핸들러 *)
let handle_plugin_connect args : (Yojson.Safe.t, string) result =
  let channel_id = get_string "channel_id" args in
  let channel_id = Figma_plugin_bridge.register_channel ?channel_id () in
  let result = `Assoc [
    ("status", `String "ok");
    ("channel_id", `String channel_id);
  ] in
  Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** figma_plugin_use_channel 핸들러 *)
let handle_plugin_use_channel args : (Yojson.Safe.t, string) result =
  match get_string "channel_id" args with
  | None -> Error "Missing required parameter: channel_id"
  | Some channel_id ->
      Figma_plugin_bridge.set_default_channel channel_id;
      let result = `Assoc [
        ("status", `String "ok");
        ("channel_id", `String channel_id);
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** figma_plugin_status 핸들러 *)
let handle_plugin_status _args : (Yojson.Safe.t, string) result =
  let channels = Figma_plugin_bridge.list_channels () in
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
  let result = `Assoc [
    ("channels", `List (List.map (fun id -> `String id) channels));
    ("stats", stats_json);
    ("limits", limits);
    ("default_channel", match default_channel with Some id -> `String id | None -> `Null);
  ] in
  Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** figma_plugin_read_selection 핸들러 *)
let handle_plugin_read_selection args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let depth = get_int "depth" args |> Option.value ~default:6 in
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let payload = `Assoc [("depth", `Int depth)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"read_selection" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("channel_id", `String channel_id);
             ("command_id", `String command_id);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** figma_plugin_get_node 핸들러 *)
let handle_plugin_get_node args : (Yojson.Safe.t, string) result =
  match (resolve_node_id args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id (or url)"
  | (Some _, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let depth = get_int "depth" args |> Option.value ~default:6 in
      let include_geometry = get_bool_or "include_geometry" true args in
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let payload = `Assoc [
        ("node_id", `String node_id);
        ("depth", `Int depth);
        ("include_geometry", `Bool include_geometry);
      ] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_node" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("channel_id", `String channel_id);
             ("command_id", `String command_id);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** figma_plugin_export_node_image 핸들러 *)
let handle_plugin_export_node_image args : (Yojson.Safe.t, string) result =
  match (resolve_node_id args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id (or url)"
  | (Some _, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let format = get_string_or "format" "png" args in
      let scale = get_float_or "scale" 1.0 args in
      let payload = `Assoc [
        ("node_id", `String node_id);
        ("format", `String format);
        ("scale", `Float scale);
      ] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"export_image" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           (* Save base64 PNG to file to avoid token waste in MCP response *)
           let save_path =
             if result.ok then
               (try
                  (match Yojson.Safe.Util.member "base64" result.payload with
                   | `String b64 ->
                       let tmp_dir = Filename.get_temp_dir_name () in
                       let filename = Printf.sprintf "figma-export-%s.png" (String.map (fun c -> if c = ':' then '-' else c) node_id) in
                       let path = Filename.concat tmp_dir filename in
                       let decoded = Base64.decode_exn b64 in
                       let oc = open_out_bin path in
                       output_string oc decoded;
                       close_out oc;
                       Some path
                   | _ -> None)
                with _ -> None)
             else None
           in
           let response = `Assoc (
             [
               ("channel_id", `String channel_id);
               ("command_id", `String command_id);
               ("ok", `Bool result.ok);
             ] @
             (match save_path with
              | Some path ->
                  [("saved_to", `String path);
                   ("message", `String "PNG saved to file. Use timg or open to view.")]
              | None ->
                  [("payload", result.payload)])
           ) in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** figma_plugin_get_variables 핸들러 *)
let handle_plugin_get_variables args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let payload = `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_variables" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("channel_id", `String channel_id);
             ("command_id", `String command_id);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** figma_plugin_apply_ops 핸들러 *)
let handle_plugin_apply_ops args : (Yojson.Safe.t, string) result =
  match (get_json "ops" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: ops"
  | (_, Error msg) -> Error msg
  | (Some ops, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let payload = `Assoc [("ops", ops)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"apply_ops" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("channel_id", `String channel_id);
             ("command_id", `String command_id);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
            ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** Edit node properties - dispatches to appropriate plugin commands *)
let handle_plugin_edit_node args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_json "properties" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: properties"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some props, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let results = ref [] in
      let errors = ref [] in
      let dispatch name payload =
        let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name ~payload in
        match plugin_wait ~channel_id ~command_id ~timeout_ms with
        | Error err -> errors := (name, err) :: !errors
        | Ok result ->
            if result.ok then results := (name, result.payload) :: !results
            else errors := (name, Yojson.Safe.to_string result.payload) :: !errors
      in
      let assoc = match props with `Assoc a -> a | _ -> [] in
      List.iter (fun (key, value) ->
        match key with
        | "fill" ->
            let color = match value with `String s -> s | _ -> Yojson.Safe.to_string value in
            dispatch "set_fill" (`Assoc [("node_id", `String node_id); ("color", `String color)])
        | "stroke" ->
            let color = match value with `String s -> s | _ -> Yojson.Safe.to_string value in
            dispatch "set_stroke" (`Assoc [("node_id", `String node_id); ("color", `String color)])
        | "stroke_weight" ->
            dispatch "set_stroke_weight" (`Assoc [("node_id", `String node_id); ("weight", value)])
        | "opacity" ->
            dispatch "set_opacity" (`Assoc [("node_id", `String node_id); ("opacity", value)])
        | "corner_radius" ->
            dispatch "set_corner_radius" (`Assoc [("node_id", `String node_id); ("radius", value)])
        | "effects" ->
            dispatch "set_effects" (`Assoc [("node_id", `String node_id); ("effects", value)])
        | "blend_mode" ->
            let mode = match value with `String s -> s | _ -> "NORMAL" in
            dispatch "set_blend_mode" (`Assoc [("node_id", `String node_id); ("blendMode", `String mode)])
        | "visible" ->
            dispatch "set_visible" (`Assoc [("node_id", `String node_id); ("visible", value)])
        | "locked" ->
            dispatch "set_locked" (`Assoc [("node_id", `String node_id); ("locked", value)])
        | "name" ->
            let n = match value with `String s -> s | _ -> Yojson.Safe.to_string value in
            dispatch "rename" (`Assoc [("node_id", `String node_id); ("name", `String n)])
        | "text" ->
            let t = match value with `String s -> s | _ -> Yojson.Safe.to_string value in
            dispatch "set_text" (`Assoc [("node_id", `String node_id); ("characters", `String t)])
        | "font_size" ->
            dispatch "set_range_font_size" (`Assoc [("node_id", `String node_id); ("fontSize", value)])
        | "text_case" ->
            let tc = match value with `String s -> s | _ -> "ORIGINAL" in
            dispatch "set_text_case" (`Assoc [("node_id", `String node_id); ("textCase", `String tc)])
        | "auto_layout" ->
            (match value with
             | `String "NONE" ->
                 dispatch "remove_auto_layout" (`Assoc [("node_id", `String node_id)])
             | _ ->
                 let mode = match value with `String s -> s | _ -> "VERTICAL" in
                 dispatch "set_auto_layout" (`Assoc [("node_id", `String node_id); ("mode", `String mode)]))
        | "padding" ->
            dispatch "set_auto_layout" (`Assoc [("node_id", `String node_id); ("padding", value)])
        | "spacing" ->
            dispatch "set_auto_layout" (`Assoc [("node_id", `String node_id); ("itemSpacing", value)])
        | "constraints" ->
            dispatch "set_constraints" (`Assoc [("node_id", `String node_id); ("constraints", value)])
        | _ ->
            errors := (key, sprintf "Unknown property: %s" key) :: !errors
      ) assoc;
      let response = `Assoc [
        ("node_id", `String node_id);
        ("applied", `Int (List.length !results));
        ("errors", `List (List.map (fun (k, e) -> `Assoc [("property", `String k); ("error", `String e)]) !errors));
        ("results", `Assoc (List.rev !results));
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string response))

(** Create a new node *)
let handle_plugin_create_node args : (Yojson.Safe.t, string) result =
  match (get_string "type" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: type"
  | (_, Error msg) -> Error msg
  | (Some node_type, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let command_name = "create_" ^ node_type in
      let payload_fields = ref [("type", `String node_type)] in
      let add_opt key extract =
        match extract key args with
        | Some v -> payload_fields := (key, v) :: !payload_fields
        | None -> ()
      in
      add_opt "parent_id" (fun k a -> get_string k a |> Option.map (fun s -> `String s));
      add_opt "x" (fun k a -> get_json k a);
      add_opt "y" (fun k a -> get_json k a);
      add_opt "width" (fun k a -> get_json k a);
      add_opt "height" (fun k a -> get_json k a);
      add_opt "name" (fun k a -> get_string k a |> Option.map (fun s -> `String s));
      add_opt "fill" (fun k a -> get_string k a |> Option.map (fun s -> `String s));
      add_opt "text" (fun k a -> get_string k a |> Option.map (fun s -> `String s));
      add_opt "font_size" (fun k a -> get_json k a);
      let payload = `Assoc (List.rev !payload_fields) in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:command_name ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("command", `String command_name);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** Delete one or more nodes *)
let handle_plugin_delete_nodes args : (Yojson.Safe.t, string) result =
  match (get_json "node_ids" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_ids"
  | (_, Error msg) -> Error msg
  | (Some node_ids_json, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let ids = match node_ids_json with
        | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []
      in
      if ids = [] then Error "node_ids must be a non-empty array of strings"
      else begin
        let results = ref [] in
        List.iter (fun node_id ->
          let payload = `Assoc [("node_id", `String node_id)] in
          let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"delete_node" ~payload in
          match plugin_wait ~channel_id ~command_id ~timeout_ms with
          | Error err -> results := `Assoc [("node_id", `String node_id); ("ok", `Bool false); ("error", `String err)] :: !results
          | Ok result -> results := `Assoc [("node_id", `String node_id); ("ok", `Bool result.ok); ("payload", result.payload)] :: !results
        ) ids;
        let response = `Assoc [
          ("deleted", `Int (List.length ids));
          ("results", `List (List.rev !results));
        ] in
        Ok (make_text_content (Yojson.Safe.pretty_to_string response))
      end

(** Batch execute multiple plugin actions sequentially *)
let handle_plugin_batch args : (Yojson.Safe.t, string) result =
  match (get_json "actions" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: actions"
  | (_, Error msg) -> Error msg
  | (Some actions_json, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:30000 in
      let stop_on_error = get_bool "stop_on_error" args |> Option.value ~default:true in
      let actions = match actions_json with `List items -> items | _ -> [] in
      if actions = [] then Error "actions must be a non-empty array"
      else begin
        let results = ref [] in
        let stopped = ref false in
        List.iteri (fun i action_json ->
          if !stopped then ()
          else begin
            let action_name = match action_json with
              | `Assoc assoc -> (match List.assoc_opt "action" assoc with Some (`String s) -> Some s | _ -> None)
              | _ -> None
            in
            match action_name with
            | None ->
                results := `Assoc [("index", `Int i); ("ok", `Bool false); ("error", `String "Missing action field")] :: !results;
                if stop_on_error then stopped := true
            | Some name ->
                let payload = action_json in
                let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name ~payload in
                (match plugin_wait ~channel_id ~command_id ~timeout_ms with
                 | Error err ->
                     results := `Assoc [("index", `Int i); ("action", `String name); ("ok", `Bool false); ("error", `String err)] :: !results;
                     if stop_on_error then stopped := true
                 | Ok result ->
                     results := `Assoc [("index", `Int i); ("action", `String name); ("ok", `Bool result.ok); ("payload", result.payload)] :: !results;
                     if (not result.ok) && stop_on_error then stopped := true)
          end
        ) actions;
        let response = `Assoc [
          ("total", `Int (List.length actions));
          ("executed", `Int (List.length !results));
          ("stopped_early", `Bool !stopped);
          ("results", `List (List.rev !results));
        ] in
        Ok (make_text_content (Yojson.Safe.pretty_to_string response))
      end

(** Subscribe to plugin events via long-poll *)
let handle_plugin_subscribe_events args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:30000 in
      let max_events = get_int "max_events" args |> Option.value ~default:50 in
      let event_types = match get_json "event_types" args with
        | Some (`List items) -> List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []
      in
      let filter_events evts =
        if event_types = [] then evts
        else List.filter (fun (e : Figma_plugin_bridge.event) -> List.mem e.event_type event_types) evts
      in
      let events_to_json evts =
        let json_events = List.map (fun (e : Figma_plugin_bridge.event) ->
          `Assoc [
            ("event_type", `String e.event_type);
            ("channel_id", `String e.channel_id);
            ("payload", e.payload);
            ("timestamp", `Float e.timestamp);
          ]
        ) evts in
        make_text_content (Yojson.Safe.pretty_to_string (`Assoc [
          ("events", `List json_events);
          ("count", `Int (List.length json_events));
        ]))
      in
      (* First check for buffered events *)
      let events = Figma_plugin_bridge.poll_events ~channel_id ~max:max_events in
      let filtered = filter_events events in
      if filtered <> [] then
        Ok (events_to_json filtered)
      else if timeout_ms <= 0 then
        Ok (events_to_json [])
      else begin
        (* Long-poll: wait for events *)
        match get_eio_context () with
        | None ->
            (* No Eio context, return empty immediately *)
            Ok (events_to_json [])
        | Some ctx ->
            let (Clock clock) = ctx.clock in
            let promise, resolver = Eio.Promise.create () in
            let waiter_id =
              Figma_plugin_bridge.register_event_waiter ~channel_id ~notify:(fun () ->
                try Eio.Promise.resolve resolver ()
                with exn ->
                  Printf.eprintf "[mcp_tools] Warning: event waiter promise resolve failed: %s\n%!" (Printexc.to_string exn))
            in
            let wait_s = float_of_int timeout_ms /. 1000.0 in
            let _result =
              match Eio.Time.with_timeout clock wait_s (fun () ->
                Eio.Promise.await promise;
                Ok `Woke) with
              | Ok `Woke -> `Woke
              | Error `Timeout -> `Timeout
            in
            Figma_plugin_bridge.unregister_event_waiter ~channel_id ~waiter_id;
            let events_after = Figma_plugin_bridge.poll_events ~channel_id ~max:max_events in
            let filtered_after = filter_events events_after in
            Ok (events_to_json filtered_after)
      end

(* list_pages 핸들러 *)
let handle_plugin_list_pages args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"list_pages" ~build_payload:(fun _ -> `Null) args

(* switch_page 핸들러 *)
let handle_plugin_switch_page args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"switch_page"
    ~validate:(fun args -> match get_string "page_id" args with
      | None -> Error "Missing required parameter: page_id"
      | Some id -> Ok id)
    ~build_payload:(fun page_id _ -> `Assoc [("page_id", `String page_id)])
    args

(* list_components 핸들러 *)
let handle_plugin_list_components args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"list_components" ~default_timeout:20000 ~build_payload:(fun _ -> `Null) args

(* clone 핸들러 *)
let handle_plugin_clone args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let offset_x = get_int "offset_x" args |> Option.value ~default:20 in
      let offset_y = get_int "offset_y" args |> Option.value ~default:20 in
      let name = get_string "name" args in
      let payload_fields = [
        ("node_id", `String node_id);
        ("offset_x", `Int offset_x);
        ("offset_y", `Int offset_y);
      ] @ (match name with Some n -> [("name", `String n)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"clone" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* group 핸들러 *)
let handle_plugin_group args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let name = get_string "name" args in
      let payload_fields =
        (match node_ids with Some ids -> [("node_ids", `List (List.map (fun s -> `String s) ids))] | None -> []) @
        (match name with Some n -> [("name", `String n)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"group" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* ungroup 핸들러 *)
let handle_plugin_ungroup args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"ungroup"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id"
      | Some id -> Ok id)
    ~build_payload:(fun node_id _ -> `Assoc [("node_id", `String node_id)])
    args

(* set_selection 핸들러 *)
let handle_plugin_set_selection args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"set_selection" ~build_payload:(fun args ->
    match get_string_list "node_ids" args with
    | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
    | None -> `Assoc [("node_ids", `List [])]) args

(* zoom_to 핸들러 *)
let handle_plugin_zoom_to args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"zoom_to" ~build_payload:(fun args ->
    let node_ids = get_string_list "node_ids" args in
    let node_id = get_string "node_id" args in
    `Assoc (
      (match node_ids with Some ids -> [("node_ids", `List (List.map (fun s -> `String s) ids))] | None -> []) @
      (match node_id with Some id -> [("node_id", `String id)] | None -> []))) args

(* reorder 핸들러 *)
let handle_plugin_reorder args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"reorder"
    ~validate:(fun args ->
      match (get_string "node_id" args, get_string "direction" args) with
      | (None, _) -> Error "Missing required parameter: node_id"
      | (_, None) -> Error "Missing required parameter: direction"
      | (Some n, Some d) -> Ok (n, d))
    ~build_payload:(fun (node_id, direction) _ ->
      `Assoc [("node_id", `String node_id); ("direction", `String direction)])
    args

(* set_locked 핸들러 *)
let handle_plugin_set_locked args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_locked"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      `Assoc [("node_id", `String node_id); ("locked", `Bool (get_bool "locked" args |> Option.value ~default:true))])
    args

(* set_visible 핸들러 *)
let handle_plugin_set_visible args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_visible"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      `Assoc [("node_id", `String node_id); ("visible", `Bool (get_bool "visible" args |> Option.value ~default:true))])
    args

(* flatten 핸들러 *)
let handle_plugin_flatten args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"flatten"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id _ -> `Assoc [("node_id", `String node_id)])
    args

(* set_auto_layout 핸들러 *)
let handle_plugin_set_auto_layout args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let layout_mode = get_string "layout_mode" args in
      let item_spacing = get_int "item_spacing" args in
      let padding = get_int "padding" args in
      let primary_alignment = get_string "primary_alignment" args in
      let counter_alignment = get_string "counter_alignment" args in
      let payload_fields = [("node_id", `String node_id)] @
        (match layout_mode with Some m -> [("layout_mode", `String m)] | None -> []) @
        (match item_spacing with Some s -> [("item_spacing", `Int s)] | None -> []) @
        (match padding with Some p -> [("padding", `Int p)] | None -> []) @
        (match primary_alignment with Some a -> [("primary_alignment", `String a)] | None -> []) @
        (match counter_alignment with Some a -> [("counter_alignment", `String a)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_auto_layout" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_viewport 핸들러 *)
let handle_plugin_get_viewport args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"get_viewport" ~build_payload:(fun _ -> `Null) args

(* set_viewport 핸들러 *)
let handle_plugin_set_viewport args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"set_viewport" ~build_payload:(fun args ->
    `Assoc (
      (match get_float "center_x" args with Some x -> [("center_x", `Float x)] | None -> []) @
      (match get_float "center_y" args with Some y -> [("center_y", `Float y)] | None -> []) @
      (match get_float "zoom" args with Some z -> [("zoom", `Float z)] | None -> []))) args

(* rename 핸들러 *)
let handle_plugin_rename args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"rename"
    ~validate:(fun args ->
      match (get_string "node_id" args, get_string "name" args) with
      | (None, _) -> Error "Missing required parameter: node_id"
      | (_, None) -> Error "Missing required parameter: name"
      | (Some n, Some name) -> Ok (n, name))
    ~build_payload:(fun (node_id, name) _ -> `Assoc [("node_id", `String node_id); ("name", `String name)])
    args

(* resize 핸들러 *)
let handle_plugin_resize args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"resize"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      `Assoc ([("node_id", `String node_id)] @
        (match get_float "width" args with Some w -> [("width", `Float w)] | None -> []) @
        (match get_float "height" args with Some h -> [("height", `Float h)] | None -> [])))
    args

(* move 핸들러 *)
let handle_plugin_move args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"move"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      `Assoc ([("node_id", `String node_id)] @
        (match get_float "x" args with Some v -> [("x", `Float v)] | None -> []) @
        (match get_float "y" args with Some v -> [("y", `Float v)] | None -> [])))
    args

(* set_opacity 핸들러 *)
let handle_plugin_set_opacity args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_opacity"
    ~validate:(fun args ->
      match (get_string "node_id" args, get_float "opacity" args) with
      | (None, _) -> Error "Missing required parameter: node_id"
      | (_, None) -> Error "Missing required parameter: opacity"
      | (Some n, Some o) -> Ok (n, o))
    ~build_payload:(fun (node_id, opacity) _ ->
      `Assoc [("node_id", `String node_id); ("opacity", `Float opacity)])
    args

(* set_corner_radius 핸들러 *)
let handle_plugin_set_corner_radius args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let radius = get_float "radius" args in
      let top_left = get_float "top_left" args in
      let top_right = get_float "top_right" args in
      let bottom_left = get_float "bottom_left" args in
      let bottom_right = get_float "bottom_right" args in
      let payload_fields = [("node_id", `String node_id)] @
        (match radius with Some r -> [("radius", `Float r)] | None -> []) @
        (match top_left with Some r -> [("topLeft", `Float r)] | None -> []) @
        (match top_right with Some r -> [("topRight", `Float r)] | None -> []) @
        (match bottom_left with Some r -> [("bottomLeft", `Float r)] | None -> []) @
        (match bottom_right with Some r -> [("bottomRight", `Float r)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_corner_radius" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_fill 핸들러 *)
let handle_plugin_set_fill args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let r = get_float "r" args |> Option.value ~default:0.0 in
      let g = get_float "g" args |> Option.value ~default:0.0 in
      let b = get_float "b" args |> Option.value ~default:0.0 in
      let a = get_float "a" args in
      let color_fields = [("r", `Float r); ("g", `Float g); ("b", `Float b)] @
        (match a with Some v -> [("a", `Float v)] | None -> []) in
      let payload = `Assoc [("node_id", `String node_id); ("color", `Assoc color_fields)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_fill" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_stroke 핸들러 *)
let handle_plugin_set_stroke args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let r = get_float "stroke_r" args in
      let g = get_float "stroke_g" args in
      let b = get_float "stroke_b" args in
      let weight = get_float "stroke_weight" args in
      let color_opt = match (r, g, b) with
        | (Some rv, Some gv, Some bv) -> Some (`Assoc [("r", `Float rv); ("g", `Float gv); ("b", `Float bv)])
        | _ -> None in
      let payload_fields = [("node_id", `String node_id)] @
        (match color_opt with Some c -> [("color", c)] | None -> []) @
        (match weight with Some w -> [("weight", `Float w)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_stroke" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_effects 핸들러 *)
let handle_plugin_set_effects args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_effects"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      let effects = match args with
        | `Assoc fields -> (match List.assoc_opt "effects" fields with Some e -> e | None -> `List [])
        | _ -> `List [] in
      `Assoc [("node_id", `String node_id); ("effects", effects)])
    args

(* create_component 핸들러 *)
let handle_plugin_create_component args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"create_component"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id _ -> `Assoc [("node_id", `String node_id)])
    args

(* detach_instance 핸들러 *)
let handle_plugin_detach_instance args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"detach_instance"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id _ -> `Assoc [("node_id", `String node_id)])
    args

(* set_text 핸들러 *)
let handle_plugin_set_text args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_text"
    ~validate:(fun args ->
      match (get_string "node_id" args, get_string "text" args) with
      | (None, _) -> Error "Missing required parameter: node_id"
      | (_, None) -> Error "Missing required parameter: text"
      | (Some n, Some t) -> Ok (n, t))
    ~build_payload:(fun (node_id, text) _ ->
      `Assoc [("node_id", `String node_id); ("text", `String text)])
    args

(* find_all 핸들러 *)
let handle_plugin_find_all args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"find_all" ~default_timeout:20000 ~build_payload:(fun args ->
    `Assoc (
      (match get_string "type" args with Some t -> [("type", `String t)] | None -> []) @
      (match get_string "find_name" args with Some n -> [("name", `String n)] | None -> []) @
      (match get_string "name_contains" args with Some n -> [("name_contains", `String n)] | None -> [])))
  args

(* notify 핸들러 *)
let handle_plugin_notify args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"notify"
    ~validate:(fun args -> match get_string "message" args with
      | None -> Error "Missing required parameter: message" | Some m -> Ok m)
    ~build_payload:(fun message args ->
      `Assoc [("message", `String message);
              ("timeout", `Int (get_int "notify_timeout" args |> Option.value ~default:3000))])
    args

(* create_frame 핸들러 *)
let handle_plugin_create_frame args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"create_frame" ~build_payload:(fun args ->
    `Assoc (
      (match get_string "name" args with Some n -> [("name", `String n)] | None -> []) @
      (match get_float "x" args with Some v -> [("x", `Float v)] | None -> []) @
      (match get_float "y" args with Some v -> [("y", `Float v)] | None -> []) @
      (match get_float "width" args with Some v -> [("width", `Float v)] | None -> []) @
      (match get_float "height" args with Some v -> [("height", `Float v)] | None -> [])))
  args

(* create_rectangle 핸들러 *)
let handle_plugin_create_rectangle args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"create_rectangle" ~build_payload:(fun args ->
    `Assoc (
      (match get_string "name" args with Some n -> [("name", `String n)] | None -> []) @
      (match get_float "x" args with Some v -> [("x", `Float v)] | None -> []) @
      (match get_float "y" args with Some v -> [("y", `Float v)] | None -> []) @
      (match get_float "width" args with Some v -> [("width", `Float v)] | None -> []) @
      (match get_float "height" args with Some v -> [("height", `Float v)] | None -> []) @
      (match get_float "radius" args with Some v -> [("cornerRadius", `Float v)] | None -> [])))
  args

(* create_ellipse 핸들러 *)
let handle_plugin_create_ellipse args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"create_ellipse" ~build_payload:(fun args ->
    `Assoc (
      (match get_string "name" args with Some n -> [("name", `String n)] | None -> []) @
      (match get_float "x" args with Some v -> [("x", `Float v)] | None -> []) @
      (match get_float "y" args with Some v -> [("y", `Float v)] | None -> []) @
      (match get_float "width" args with Some v -> [("width", `Float v)] | None -> []) @
      (match get_float "height" args with Some v -> [("height", `Float v)] | None -> [])))
  args

(* create_text 핸들러 *)
let handle_plugin_create_text args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let text = get_string "text" args in
      let font_size = get_int "font_size" args in
      let payload_fields =
        (match name with Some n -> [("name", `String n)] | None -> []) @
        (match x with Some v -> [("x", `Float v)] | None -> []) @
        (match y with Some v -> [("y", `Float v)] | None -> []) @
        (match text with Some t -> [("text", `String t)] | None -> []) @
        (match font_size with Some s -> [("fontSize", `Int s)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_text" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_line 핸들러 *)
let handle_plugin_create_line args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let length = get_float "length" args in
      let rotation = get_float "rotation" args in
      let stroke_weight = get_float "stroke_weight" args in
      let payload_fields =
        (match name with Some n -> [("name", `String n)] | None -> []) @
        (match x with Some v -> [("x", `Float v)] | None -> []) @
        (match y with Some v -> [("y", `Float v)] | None -> []) @
        (match length with Some v -> [("length", `Float v)] | None -> []) @
        (match rotation with Some v -> [("rotation", `Float v)] | None -> []) @
        (match stroke_weight with Some v -> [("stroke_weight", `Float v)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_line" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_polygon 핸들러 *)
let handle_plugin_create_polygon args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let width = get_float "width" args in
      let height = get_float "height" args in
      let point_count = get_int "point_count" args in
      let payload_fields =
        (match name with Some n -> [("name", `String n)] | None -> []) @
        (match x with Some v -> [("x", `Float v)] | None -> []) @
        (match y with Some v -> [("y", `Float v)] | None -> []) @
        (match width with Some v -> [("width", `Float v)] | None -> []) @
        (match height with Some v -> [("height", `Float v)] | None -> []) @
        (match point_count with Some c -> [("pointCount", `Int c)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_polygon" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_star 핸들러 *)
let handle_plugin_create_star args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let width = get_float "width" args in
      let height = get_float "height" args in
      let point_count = get_int "point_count" args in
      let inner_radius = get_float "inner_radius" args in
      let payload_fields =
        (match name with Some n -> [("name", `String n)] | None -> []) @
        (match x with Some v -> [("x", `Float v)] | None -> []) @
        (match y with Some v -> [("y", `Float v)] | None -> []) @
        (match width with Some v -> [("width", `Float v)] | None -> []) @
        (match height with Some v -> [("height", `Float v)] | None -> []) @
        (match point_count with Some c -> [("pointCount", `Int c)] | None -> []) @
        (match inner_radius with Some r -> [("innerRadius", `Float r)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_star" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* delete_node 핸들러 *)
let handle_plugin_delete_node args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"delete_node" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* duplicate 핸들러 *)
let handle_plugin_duplicate args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let offset_x = get_float "offset_x" args in
      let offset_y = get_float "offset_y" args in
      let name = get_string "name" args in
      let payload_fields = [("node_id", `String node_id)] @
        (match offset_x with Some v -> [("offset_x", `Float v)] | None -> []) @
        (match offset_y with Some v -> [("offset_y", `Float v)] | None -> []) @
        (match name with Some n -> [("name", `String n)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"duplicate" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* align 핸들러 *)
let handle_plugin_align args : (Yojson.Safe.t, string) result =
  match (get_string "align_direction" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: align_direction (left/center/right/top/middle/bottom)"
  | (_, Error msg) -> Error msg
  | (Some direction, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload_fields = [("direction", `String direction)] @
        (match node_ids with Some ids -> [("node_ids", `List (List.map (fun s -> `String s) ids))] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"align" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* distribute 핸들러 *)
let handle_plugin_distribute args : (Yojson.Safe.t, string) result =
  match (get_string "distribute_direction" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: distribute_direction (horizontal/vertical)"
  | (_, Error msg) -> Error msg
  | (Some direction, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload_fields = [("direction", `String direction)] @
        (match node_ids with Some ids -> [("node_ids", `List (List.map (fun s -> `String s) ids))] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"distribute" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* boolean_union 핸들러 *)
let handle_plugin_boolean_union args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload = match node_ids with
        | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"boolean_union" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* boolean_subtract 핸들러 *)
let handle_plugin_boolean_subtract args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload = match node_ids with
        | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"boolean_subtract" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* boolean_intersect 핸들러 *)
let handle_plugin_boolean_intersect args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload = match node_ids with
        | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"boolean_intersect" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* boolean_exclude 핸들러 *)
let handle_plugin_boolean_exclude args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload = match node_ids with
        | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"boolean_exclude" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_local_styles 핸들러 *)
let handle_plugin_get_local_styles args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_local_styles" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_constraints 핸들러 *)
let handle_plugin_set_constraints args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let horizontal = get_string "constraint_horizontal" args in
      let vertical = get_string "constraint_vertical" args in
      let payload_fields = [("node_id", `String node_id)] @
        (match horizontal with Some h -> [("horizontal", `String h)] | None -> []) @
        (match vertical with Some v -> [("vertical", `String v)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_constraints" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_page 핸들러 *)
let handle_plugin_create_page args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let payload = match name with
        | Some n -> `Assoc [("name", `String n)]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_page" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* delete_page 핸들러 *)
let handle_plugin_delete_page args : (Yojson.Safe.t, string) result =
  match (get_string "page_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: page_id"
  | (_, Error msg) -> Error msg
  | (Some page_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("page_id", `String page_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"delete_page" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* rotate 핸들러 *)
let handle_plugin_rotate args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_float "angle" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: angle"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some angle, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id); ("angle", `Float angle)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"rotate" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* flip 핸들러 *)
let handle_plugin_flip args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_string "flip_direction" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: flip_direction (horizontal/vertical)"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some direction, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id); ("direction", `String direction)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"flip" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* outline_stroke 핸들러 *)
let handle_plugin_outline_stroke args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"outline_stroke" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_blend_mode 핸들러 *)
let handle_plugin_set_blend_mode args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_string "blend_mode" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: blend_mode"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some blend_mode, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id); ("blend_mode", `String blend_mode)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_blend_mode" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_selection_colors 핸들러 *)
let handle_plugin_get_selection_colors args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_selection_colors" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* swap_fill_stroke 핸들러 *)
let handle_plugin_swap_fill_stroke args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"swap_fill_stroke" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* copy_style 핸들러 *)
let handle_plugin_copy_style args : (Yojson.Safe.t, string) result =
  match (get_string "source_id" args, get_string "target_id" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: source_id"
  | (_, None, _) -> Error "Missing required parameter: target_id"
  | (_, _, Error msg) -> Error msg
  | (Some source_id, Some target_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("source_id", `String source_id); ("target_id", `String target_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"copy_style" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_fonts 핸들러 *)
let handle_plugin_get_fonts args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_fonts" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_parent 핸들러 *)
let handle_plugin_set_parent args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_string "parent_id" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: parent_id"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some parent_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id); ("parent_id", `String parent_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_parent" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_vector 핸들러 *)
let handle_plugin_create_vector args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let width = get_float "width" args in
      let height = get_float "height" args in
      let fields = [] in
      let fields = match name with Some v -> ("name", `String v) :: fields | None -> fields in
      let fields = match x with Some v -> ("x", `Float v) :: fields | None -> fields in
      let fields = match y with Some v -> ("y", `Float v) :: fields | None -> fields in
      let fields = match width with Some v -> ("width", `Float v) :: fields | None -> fields in
      let fields = match height with Some v -> ("height", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_vector" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_image_fill 핸들러 *)
let handle_plugin_set_image_fill args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let node_id = get_string "node_id" args in
      let image_hash = get_string "image_hash" args in
      let base64 = get_string "base64" args in
      let scale_mode = get_string "scale_mode" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match image_hash with Some v -> ("image_hash", `String v) :: fields | None -> fields in
      let fields = match base64 with Some v -> ("base64", `String v) :: fields | None -> fields in
      let fields = match scale_mode with Some v -> ("scale_mode", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_image_fill" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_plugin_data 핸들러 *)
let handle_plugin_get_plugin_data args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let key = get_string "data_key" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match key with Some v -> ("key", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_plugin_data" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_plugin_data 핸들러 *)
let handle_plugin_set_plugin_data args : (Yojson.Safe.t, string) result =
  match (get_string "data_key" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: data_key"
  | (_, Error msg) -> Error msg
  | (Some key, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let value = get_string "data_value" args |> Option.value ~default:"" in
      let fields = [("key", `String key); ("value", `String value)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_plugin_data" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_doc_info 핸들러 *)
let handle_plugin_get_doc_info args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_doc_info" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_absolute_bounds 핸들러 *)
let handle_plugin_get_absolute_bounds args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_absolute_bounds" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_component_set 핸들러 *)
let handle_plugin_create_component_set args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let component_ids = get_string_list "component_ids" args |> Option.value ~default:[] in
      let name = get_string "name" args in
      let fields = [("component_ids", `List (List.map (fun s -> `String s) component_ids))] in
      let fields = match name with Some v -> ("name", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_component_set" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* remove_auto_layout 핸들러 *)
let handle_plugin_remove_auto_layout args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"remove_auto_layout" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_slice 핸들러 *)
let handle_plugin_create_slice args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let width = get_float "width" args in
      let height = get_float "height" args in
      let fields = [] in
      let fields = match name with Some v -> ("name", `String v) :: fields | None -> fields in
      let fields = match x with Some v -> ("x", `Float v) :: fields | None -> fields in
      let fields = match y with Some v -> ("y", `Float v) :: fields | None -> fields in
      let fields = match width with Some v -> ("width", `Float v) :: fields | None -> fields in
      let fields = match height with Some v -> ("height", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_slice" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_export_settings 핸들러 *)
let handle_plugin_set_export_settings args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let format = get_string "export_format" args in
      let scale = get_float "scale" args in
      let suffix = get_string "suffix" args in
      let append = get_bool "append" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match format with Some v -> ("format", `String v) :: fields | None -> fields in
      let fields = match scale with Some v -> ("scale", `Float v) :: fields | None -> fields in
      let fields = match suffix with Some v -> ("suffix", `String v) :: fields | None -> fields in
      let fields = match append with Some v -> ("append", `Bool v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_export_settings" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_reactions 핸들러 *)
let handle_plugin_get_reactions args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_reactions" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_reactions 핸들러 *)
let handle_plugin_set_reactions args : (Yojson.Safe.t, string) result =
  match (get_string "target_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: target_id"
  | (_, Error msg) -> Error msg
  | (Some target_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let trigger = get_string "trigger" args |> Option.value ~default:"ON_CLICK" in
      let navigation = get_string "navigation" args in
      let preserve_scroll = get_bool "preserve_scroll" args in
      let fields = [("target_id", `String target_id); ("trigger", `String trigger)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match navigation with Some v -> ("navigation", `String v) :: fields | None -> fields in
      let fields = match preserve_scroll with Some v -> ("preserve_scroll", `Bool v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_reactions" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* rasterize 핸들러 *)
let handle_plugin_rasterize args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:30000 in
      let node_id = get_string "node_id" args in
      let format = get_string "format" args in
      let scale = get_float "scale" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match format with Some v -> ("format", `String v) :: fields | None -> fields in
      let fields = match scale with Some v -> ("scale", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"rasterize" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_shared_plugin_data 핸들러 *)
let handle_plugin_get_shared_plugin_data args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let namespace = get_string "namespace" args |> Option.value ~default:"shared" in
      let key = get_string "data_key" args in
      let fields = [("namespace", `String namespace)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match key with Some v -> ("key", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_shared_plugin_data" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_shared_plugin_data 핸들러 *)
let handle_plugin_set_shared_plugin_data args : (Yojson.Safe.t, string) result =
  match (get_string "data_key" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: data_key"
  | (_, Error msg) -> Error msg
  | (Some key, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let namespace = get_string "namespace" args |> Option.value ~default:"shared" in
      let value = get_string "data_value" args |> Option.value ~default:"" in
      let fields = [("namespace", `String namespace); ("key", `String key); ("value", `String value)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_shared_plugin_data" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* swap_component 핸들러 *)
let handle_plugin_swap_component args : (Yojson.Safe.t, string) result =
  match (get_string "component_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: component_id"
  | (_, Error msg) -> Error msg
  | (Some component_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = [("component_id", `String component_id)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"swap_component" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* resize_to_fit 핸들러 *)
let handle_plugin_resize_to_fit args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let axis = get_string "axis" args in
      let padding = get_float "padding" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match axis with Some v -> ("axis", `String v) :: fields | None -> fields in
      let fields = match padding with Some v -> ("padding", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"resize_to_fit" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_characters 핸들러 *)
let handle_plugin_get_characters args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_characters" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_range_fills 핸들러 *)
let handle_plugin_set_range_fills args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let start_idx = get_int "start" args in
      let end_idx = get_int "end" args in
      let r = get_float "r" args in
      let g = get_float "g" args in
      let b = get_float "b" args in
      let a = get_float "a" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match start_idx with Some v -> ("start", `Int v) :: fields | None -> fields in
      let fields = match end_idx with Some v -> ("end", `Int v) :: fields | None -> fields in
      let color_fields = [] in
      let color_fields = match r with Some v -> ("r", `Float v) :: color_fields | None -> color_fields in
      let color_fields = match g with Some v -> ("g", `Float v) :: color_fields | None -> color_fields in
      let color_fields = match b with Some v -> ("b", `Float v) :: color_fields | None -> color_fields in
      let color_fields = match a with Some v -> ("a", `Float v) :: color_fields | None -> color_fields in
      let fields = if color_fields <> [] then ("color", `Assoc color_fields) :: fields else fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_range_fills" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_range_font_size 핸들러 *)
let handle_plugin_set_range_font_size args : (Yojson.Safe.t, string) result =
  match (get_float "font_size" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: font_size"
  | (_, Error msg) -> Error msg
  | (Some font_size, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let start_idx = get_int "start" args in
      let end_idx = get_int "end" args in
      let fields = [("font_size", `Float font_size)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match start_idx with Some v -> ("start", `Int v) :: fields | None -> fields in
      let fields = match end_idx with Some v -> ("end", `Int v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_range_font_size" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* insert_child 핸들러 *)
let handle_plugin_insert_child args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_string "parent_id" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: parent_id"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some parent_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let index = get_int "index" args |> Option.value ~default:0 in
      let payload = `Assoc [("node_id", `String node_id); ("parent_id", `String parent_id); ("index", `Int index)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"insert_child" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_all_local_variables 핸들러 *)
let handle_plugin_get_all_local_variables args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_all_local_variables" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_styles_by_type 핸들러 *)
let handle_plugin_get_styles_by_type args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let style_type = get_string "style_type" args |> Option.value ~default:"FILL" in
      let payload = `Assoc [("style_type", `String style_type)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_styles_by_type" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* apply_style 핸들러 *)
let handle_plugin_apply_style args : (Yojson.Safe.t, string) result =
  match (get_string "style_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: style_id"
  | (_, Error msg) -> Error msg
  | (Some style_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let style_type = get_string "style_type" args |> Option.value ~default:"FILL" in
      let fields = [("style_id", `String style_id); ("style_type", `String style_type)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"apply_style" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_overrides 핸들러 *)
let handle_plugin_get_overrides args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_overrides" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* reset_overrides 핸들러 *)
let handle_plugin_reset_overrides args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"reset_overrides" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* bring_to_front 핸들러 *)
let handle_plugin_bring_to_front args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"bring_to_front" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* send_to_back 핸들러 *)
let handle_plugin_send_to_back args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"send_to_back" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_grid 핸들러 *)
let handle_plugin_set_grid args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let pattern = get_string "pattern" args in
      let count = get_int "count" args in
      let gutter = get_int "gutter" args in
      let offset = get_int "offset" args in
      let alignment = get_string "alignment" args in
      let size = get_int "size" args in
      let visible = get_bool "visible" args in
      let append = get_bool "append" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match pattern with Some v -> ("pattern", `String v) :: fields | None -> fields in
      let fields = match count with Some v -> ("count", `Int v) :: fields | None -> fields in
      let fields = match gutter with Some v -> ("gutter", `Int v) :: fields | None -> fields in
      let fields = match offset with Some v -> ("offset", `Int v) :: fields | None -> fields in
      let fields = match alignment with Some v -> ("alignment", `String v) :: fields | None -> fields in
      let fields = match size with Some v -> ("size", `Int v) :: fields | None -> fields in
      let fields = match visible with Some v -> ("visible", `Bool v) :: fields | None -> fields in
      let fields = match append with Some v -> ("append", `Bool v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_grid" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_layer_list 핸들러 *)
let handle_plugin_get_layer_list args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_layer_list" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* scroll_and_zoom 핸들러 *)
let handle_plugin_scroll_and_zoom args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let zoom = get_float "zoom" args in
      let fields = [] in
      let fields = match x with Some v -> ("x", `Float v) :: fields | None -> fields in
      let fields = match y with Some v -> ("y", `Float v) :: fields | None -> fields in
      let fields = match zoom with Some v -> ("zoom", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"scroll_and_zoom" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_paint_styles 핸들러 *)
let handle_plugin_get_paint_styles args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_paint_styles" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_text_case 핸들러 *)
let handle_plugin_set_text_case args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let text_case = get_string "text_case" args |> Option.value ~default:"ORIGINAL" in
      let fields = [("text_case", `String text_case)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_text_case" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_stroke_details 핸들러 *)
let handle_plugin_get_stroke_details args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_stroke_details" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_stroke_weight 핸들러 *)
let handle_plugin_set_stroke_weight args : (Yojson.Safe.t, string) result =
  match (get_float "weight" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: weight"
  | (_, Error msg) -> Error msg
  | (Some weight, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = [("weight", `Float weight)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_stroke_weight" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* collapse_layer 핸들러 *)
let handle_plugin_collapse_layer args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let expand = get_bool "expand" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match expand with Some v -> ("expand", `Bool v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"collapse_layer" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* Known plugin actions for typo suggestion *)
let known_plugin_actions = [
  "connect"; "use_channel"; "status"; "read_selection"; "get_node";
  "export_image"; "get_variables"; "apply_ops"; "list_pages"; "switch_page";
  "list_components"; "clone"; "group"; "ungroup"; "set_selection"; "zoom_to";
  "reorder"; "set_locked"; "set_visible"; "flatten"; "set_auto_layout";
  "get_viewport"; "set_viewport"; "rename"; "resize"; "move"; "set_opacity";
  "set_corner_radius"; "set_fill"; "set_stroke"; "set_effects";
  "create_component"; "create_instance"; "detach_instance"; "set_text";
  "find_all"; "notify"; "create_frame"; "create_rectangle"; "create_ellipse";
  "create_text"; "create_line"; "create_polygon"; "create_star"; "delete_node";
  "duplicate"; "align"; "distribute"; "boolean_union"; "boolean_subtract";
  "boolean_intersect"; "boolean_exclude"; "get_local_styles"; "set_constraints";
  "create_page"; "delete_page"; "rotate"; "flip"; "outline_stroke";
  "set_blend_mode"; "get_selection_colors"; "swap_fill_stroke"; "copy_style";
  "get_fonts"; "set_parent"; "create_vector"; "set_image_fill";
  "get_plugin_data"; "set_plugin_data"; "get_doc_info"; "get_absolute_bounds";
  "create_component_set"; "remove_auto_layout"; "create_slice";
  "set_export_settings"; "get_reactions"; "set_reactions"; "rasterize";
  "get_shared_plugin_data"; "set_shared_plugin_data"; "swap_component";
  "resize_to_fit"; "get_characters"; "set_range_fills"; "set_range_font_size";
  "insert_child"; "get_all_local_variables"; "get_styles_by_type"; "apply_style";
  "get_overrides"; "reset_overrides"; "bring_to_front"; "send_to_back";
  "set_grid"; "get_layer_list"; "scroll_and_zoom"; "get_paint_styles";
  "set_text_case"; "get_stroke_details"; "set_stroke_weight"; "collapse_layer";
  "execute_dsl"; "export_tokens"; "export_viewport"; "export_selection";
  "watch_start"; "watch_stop"; "get_changes";
]

let suggest_action unknown =
  let edit_distance a b =
    let la = String.length a and lb = String.length b in
    if la = 0 then lb else if lb = 0 then la
    else
      let prev = Array.init (lb + 1) (fun i -> i) in
      let curr = Array.make (lb + 1) 0 in
      for i = 1 to la do
        curr.(0) <- i;
        for j = 1 to lb do
          let cost = if Char.equal (String.get a (i-1)) (String.get b (j-1)) then 0 else 1 in
          curr.(j) <- min (min (prev.(j) + 1) (curr.(j-1) + 1)) (prev.(j-1) + cost)
        done;
        Array.blit curr 0 prev 0 (lb + 1)
      done;
      prev.(lb)
  in
  let scored = List.filter_map (fun action ->
    let d = edit_distance unknown action in
    if d <= 3 then Some (d, action) else None
  ) known_plugin_actions in
  let sorted = List.sort (fun (d1, _) (d2, _) -> compare d1 d2) scored in
  match sorted with
  | (_, suggestion) :: _ -> Printf.sprintf " Did you mean '%s'?" suggestion
  | [] -> ""

(* STRAP 통합 핸들러: action으로 라우팅, 기존 핸들러 재사용 *)
let handle_figma_plugin args : (Yojson.Safe.t, string) result =
  match get_string "action" args with
  | None -> Error "Missing required parameter: action. Use figma_plugin with action='connect' to start."
  | Some action ->
      match action with
      | "connect" -> handle_plugin_connect args
      | "use_channel" -> handle_plugin_use_channel args
      | "status" -> handle_plugin_status args
      | "read_selection" -> handle_plugin_read_selection args
      | "get_node" -> handle_plugin_get_node args
      | "export_image" -> handle_plugin_export_node_image args
      | "get_variables" -> handle_plugin_get_variables args
      | "apply_ops" -> handle_plugin_apply_ops args
      | "list_pages" -> handle_plugin_list_pages args
      | "switch_page" -> handle_plugin_switch_page args
      | "list_components" -> handle_plugin_list_components args
      | "clone" -> handle_plugin_clone args
      | "group" -> handle_plugin_group args
      | "ungroup" -> handle_plugin_ungroup args
      | "set_selection" -> handle_plugin_set_selection args
      | "zoom_to" -> handle_plugin_zoom_to args
      | "reorder" -> handle_plugin_reorder args
      | "set_locked" -> handle_plugin_set_locked args
      | "set_visible" -> handle_plugin_set_visible args
      | "flatten" -> handle_plugin_flatten args
      | "set_auto_layout" -> handle_plugin_set_auto_layout args
      | "get_viewport" -> handle_plugin_get_viewport args
      | "set_viewport" -> handle_plugin_set_viewport args
      | "rename" -> handle_plugin_rename args
      | "resize" -> handle_plugin_resize args
      | "move" -> handle_plugin_move args
      | "set_opacity" -> handle_plugin_set_opacity args
      | "set_corner_radius" -> handle_plugin_set_corner_radius args
      | "set_fill" -> handle_plugin_set_fill args
      | "set_stroke" -> handle_plugin_set_stroke args
      | "set_effects" -> handle_plugin_set_effects args
      | "create_component" -> handle_plugin_create_component args
      | "detach_instance" -> handle_plugin_detach_instance args
      | "set_text" -> handle_plugin_set_text args
      | "find_all" -> handle_plugin_find_all args
      | "notify" -> handle_plugin_notify args
      | "create_frame" -> handle_plugin_create_frame args
      | "create_rectangle" -> handle_plugin_create_rectangle args
      | "create_ellipse" -> handle_plugin_create_ellipse args
      | "create_text" -> handle_plugin_create_text args
      | "create_line" -> handle_plugin_create_line args
      | "create_polygon" -> handle_plugin_create_polygon args
      | "create_star" -> handle_plugin_create_star args
      | "delete_node" -> handle_plugin_delete_node args
      | "duplicate" -> handle_plugin_duplicate args
      | "align" -> handle_plugin_align args
      | "distribute" -> handle_plugin_distribute args
      | "boolean_union" -> handle_plugin_boolean_union args
      | "boolean_subtract" -> handle_plugin_boolean_subtract args
      | "boolean_intersect" -> handle_plugin_boolean_intersect args
      | "boolean_exclude" -> handle_plugin_boolean_exclude args
      | "get_local_styles" -> handle_plugin_get_local_styles args
      | "set_constraints" -> handle_plugin_set_constraints args
      | "create_page" -> handle_plugin_create_page args
      | "delete_page" -> handle_plugin_delete_page args
      | "rotate" -> handle_plugin_rotate args
      | "flip" -> handle_plugin_flip args
      | "outline_stroke" -> handle_plugin_outline_stroke args
      | "set_blend_mode" -> handle_plugin_set_blend_mode args
      | "get_selection_colors" -> handle_plugin_get_selection_colors args
      | "swap_fill_stroke" -> handle_plugin_swap_fill_stroke args
      | "copy_style" -> handle_plugin_copy_style args
      | "get_fonts" -> handle_plugin_get_fonts args
      | "set_parent" -> handle_plugin_set_parent args
      | "create_vector" -> handle_plugin_create_vector args
      | "set_image_fill" -> handle_plugin_set_image_fill args
      | "get_plugin_data" -> handle_plugin_get_plugin_data args
      | "set_plugin_data" -> handle_plugin_set_plugin_data args
      | "get_doc_info" -> handle_plugin_get_doc_info args
      | "get_absolute_bounds" -> handle_plugin_get_absolute_bounds args
      | "create_component_set" -> handle_plugin_create_component_set args
      | "remove_auto_layout" -> handle_plugin_remove_auto_layout args
      | "create_slice" -> handle_plugin_create_slice args
      | "set_export_settings" -> handle_plugin_set_export_settings args
      | "get_reactions" -> handle_plugin_get_reactions args
      | "set_reactions" -> handle_plugin_set_reactions args
      | "rasterize" -> handle_plugin_rasterize args
      | "get_shared_plugin_data" -> handle_plugin_get_shared_plugin_data args
      | "set_shared_plugin_data" -> handle_plugin_set_shared_plugin_data args
      | "swap_component" -> handle_plugin_swap_component args
      | "resize_to_fit" -> handle_plugin_resize_to_fit args
      | "get_characters" -> handle_plugin_get_characters args
      | "set_range_fills" -> handle_plugin_set_range_fills args
      | "set_range_font_size" -> handle_plugin_set_range_font_size args
      | "insert_child" -> handle_plugin_insert_child args
      | "get_all_local_variables" -> handle_plugin_get_all_local_variables args
      | "get_styles_by_type" -> handle_plugin_get_styles_by_type args
      | "apply_style" -> handle_plugin_apply_style args
      | "get_overrides" -> handle_plugin_get_overrides args
      | "reset_overrides" -> handle_plugin_reset_overrides args
      | "bring_to_front" -> handle_plugin_bring_to_front args
      | "send_to_back" -> handle_plugin_send_to_back args
      | "set_grid" -> handle_plugin_set_grid args
      | "get_layer_list" -> handle_plugin_get_layer_list args
      | "scroll_and_zoom" -> handle_plugin_scroll_and_zoom args
      | "get_paint_styles" -> handle_plugin_get_paint_styles args
      | "set_text_case" -> handle_plugin_set_text_case args
      | "get_stroke_details" -> handle_plugin_get_stroke_details args
      | "set_stroke_weight" -> handle_plugin_set_stroke_weight args
      | "collapse_layer" -> handle_plugin_collapse_layer args
      | "execute_dsl" -> handle_plugin_apply_ops args
      | "export_tokens" ->
          (match resolve_channel_id args with
           | Error msg -> Error msg
           | Ok channel_id ->
               let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
               let payload = `Assoc [] in
               let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"export_tokens" ~payload in
               (match plugin_wait ~channel_id ~command_id ~timeout_ms with
                | Error err -> Error err
                | Ok result ->
                    Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload))))
      (* Feedback Loop actions *)
      | "export_viewport" ->
          plugin_simple ~name:"export_viewport" ~default_timeout:20000
            ~build_payload:(fun a ->
              let max_nodes = get_int "max_nodes" a |> Option.value ~default:5 in
              `Assoc [("max_nodes", `Int max_nodes)]) args
      | "export_selection" ->
          plugin_simple ~name:"export_selection" ~default_timeout:20000
            ~build_payload:(fun _a -> `Assoc []) args
      | "watch_start" ->
          plugin_simple ~name:"watch_start" ~default_timeout:30000
            ~build_payload:(fun _a -> `Assoc []) args
      | "watch_stop" ->
          plugin_simple ~name:"watch_stop" ~default_timeout:10000
            ~build_payload:(fun _a -> `Assoc []) args
      | "get_changes" ->
          plugin_simple ~name:"get_changes" ~default_timeout:10000
            ~build_payload:(fun a ->
              let pairs = ref [] in
              (match get_int "since" a with Some v -> pairs := ("since", `Int v) :: !pairs | None -> ());
              (match get_int "limit" a with Some v -> pairs := ("limit", `Int v) :: !pairs | None -> ());
              (match get_bool "clear" a with Some v -> pairs := ("clear", `Bool v) :: !pairs | None -> ());
              `Assoc !pairs) args
      | "create_instance" ->
          plugin_simple ~name:"create_instance" ~default_timeout:15000
            ~build_payload:(fun a ->
              let pairs = ref [] in
              (match get_string "component_key" a with Some v -> pairs := ("component_key", `String v) :: !pairs | None -> ());
              (match get_string "name" a with Some v -> pairs := ("name", `String v) :: !pairs | None -> ());
              (match get_int "x" a with Some v -> pairs := ("x", `Int v) :: !pairs | None -> ());
              (match get_int "y" a with Some v -> pairs := ("y", `Int v) :: !pairs | None -> ());
              `Assoc !pairs) args
      | _ ->
          let suggestion = suggest_action action in
          Error (sprintf "Unknown action: '%s'.%s %d actions available." action suggestion (List.length known_plugin_actions))

