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

(** figma_annotate 핸들러 - 캔버스에 AI 노트 생성 *)
let handle_annotate args : (Yojson.Safe.t, string) result =
  let node_id = get_string "node_id" args in
  let message = get_string_or "message" "" args in
  let color = get_string_or "color" "yellow" args in

  let fill_color = match color with
    | "blue" -> "#E1F5FE"
    | "green" -> "#E8F5E9"
    | "red" -> "#FFEBEE"
    | _ -> "#FFF9C4" (* yellow *)
  in

  (* AI Annotation을 위한 복합 액션 구성 *)
  let ops = `List [
    `Assoc [
      ("action", `String "create_rectangle");
      ("name", `String "AI Annotation");
      ("width", `Int 200);
      ("height", `Int 100);
      ("fill", `String fill_color);
      ("node_id", (match node_id with Some id -> `String id | None -> `Null));
    ];
    `Assoc [
      ("action", `String "create_text");
      ("name", `String "AI Message");
      ("text", `String message);
      ("font_size", `Int 12);
      ("parent_last", `Bool true);
    ]
  ] in

  let batch_args = `Assoc (
    ("actions", ops) ::
    (List.filter (fun (k, _) -> k <> "actions") (match args with `Assoc kv -> kv | _ -> []))
  ) in
  
  handle_plugin_batch batch_args

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
                with Invalid_argument _ | Sys_error _ -> None)
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

(** Edit node properties *)
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

(** figma_export_tokens_plugin 핸들러 - Plugin Bridge 통해 디자인 토큰 추출 *)
let handle_export_tokens_plugin args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"export_design_tokens" ~default_timeout:20000
    ~build_payload:(fun args ->
      let format = get_string "format" args |> Option.value ~default:"json" in
      `Assoc [("format", `String format)])
    args

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
      let events = Figma_plugin_bridge.poll_events ~channel_id ~max:max_events in
      let filtered = filter_events events in
      if filtered <> [] then
        Ok (events_to_json filtered)
      else if timeout_ms <= 0 then
        Ok (events_to_json [])
      else begin
        match get_eio_context () with
        | None -> Ok (events_to_json [])
        | Some ctx ->
            let (Clock clock) = ctx.clock in
            let promise, resolver = Eio.Promise.create () in
            let waiter_id =
              Figma_plugin_bridge.register_event_waiter ~channel_id ~notify:(fun () ->
                try Eio.Promise.resolve resolver ()
                with Invalid_argument _ -> ())
            in
            let wait_s = float_of_int timeout_ms /. 1000.0 in
            let _ = Eio.Time.with_timeout clock wait_s (fun () -> Eio.Promise.await promise; Ok `Woke) in
            Figma_plugin_bridge.unregister_event_waiter ~channel_id ~waiter_id;
            let events_after = Figma_plugin_bridge.poll_events ~channel_id ~max:max_events in
            Ok (events_to_json (filter_events events_after))
      end

(** ============== Monolithic Router ============== *)

let known_plugin_actions =
  Mcp_plugin_actions.known_plugin_actions

let dispatch_unknown_plugin_action action args =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let payload =
        match args with
        | `Assoc fields ->
            `Assoc (List.filter (fun (key, _) -> key <> "action" && key <> "channel_id") fields)
        | _ -> `Assoc []
      in
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      plugin_exec ~channel_id ~name:action ~payload ~timeout_ms

let handle_figma_plugin args : (Yojson.Safe.t, string) result =
  match get_string "action" args with
  | None -> Error "Missing action"
  | Some action ->
      let action = String.lowercase_ascii (String.trim action) in
      match action with
      | "connect" -> handle_plugin_connect args
      | "use_channel" -> handle_plugin_use_channel args
      | "status" -> handle_plugin_status args
      | "read_selection" -> handle_plugin_read_selection args
      | "get_node" -> handle_plugin_get_node args
      | "export_image" -> handle_plugin_export_node_image args
      | "get_variables" -> handle_plugin_get_variables args
      | "apply_ops" -> handle_plugin_apply_ops args
      | "batch" -> handle_plugin_batch args
      | "annotate" -> handle_annotate args
      | "subscribe_events" -> handle_plugin_subscribe_events args
      | "export_tokens" -> handle_export_tokens_plugin args
      | action when List.mem action known_plugin_actions -> dispatch_unknown_plugin_action action args
      | _ -> Error (Printf.sprintf "Unknown plugin action '%s'." action)
