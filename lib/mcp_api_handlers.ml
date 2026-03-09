(** Figma MCP API handlers — core Figma API handler functions *)

open Mcp_helpers
open Mcp_plugin_handlers
open Printf

(** ============== Node selection helpers ============== *)

type selection_config = {
  layout_only: bool;
  auto_layout_only: bool;
  text_mode: string;
  score_threshold: float;
  max_parents: int;
  summary_depth: int;
  exclude_patterns: string list;
  note_patterns: string list;
  notes_limit: int;
  excluded_limit: int;
}

let default_exclude_patterns = [
  "guide"; "spec"; "measure"; "annotation"; "grid"; "flow"; "diagram"; "wip";
  "archive"; "note"; "memo"; "draft"; "unused"; "redline";
  "가이드"; "스펙"; "측정"; "주석"; "그리드"; "순서도"; "플로우"; "다이어그램";
  "메모"; "참고"; "설명"; "임시"; "미사용"; "가이드라인";
]

let default_note_patterns = [
  "note"; "memo"; "annotation"; "guide"; "spec"; "measure"; "as-is"; "as is";
  "to-be"; "to be";
  "주석"; "메모"; "참고"; "설명"; "가이드"; "스펙"; "측정"; "as-is"; "to-be";
]

let normalize_patterns patterns =
  patterns
  |> List.map String.trim
  |> List.filter (fun p -> p <> "")

let string_contains = Mcp_helpers.string_contains
let matches_any = Mcp_helpers.matches_any
let find_matching_pattern = Mcp_helpers.find_matching_pattern

let node_text_blob node =
  match node.Figma_types.characters with
  | Some txt -> String.concat " " [node.Figma_types.name; txt]
  | None -> node.Figma_types.name

let node_is_text node =
  match node.Figma_types.node_type with
  | Figma_types.Text -> true
  | _ -> false

let node_is_container node =
  match node.Figma_types.node_type with
  | Figma_types.Document
  | Figma_types.Canvas
  | Figma_types.Frame
  | Figma_types.Group
  | Figma_types.Section
  | Figma_types.Component
  | Figma_types.ComponentSet
  | Figma_types.Instance -> true
  | _ -> false

let node_is_component node =
  match node.Figma_types.node_type with
  | Figma_types.Component
  | Figma_types.ComponentSet
  | Figma_types.Instance -> true
  | _ -> false

let node_has_image_fill node =
  List.exists
    (fun (paint : Figma_types.paint) ->
       paint.visible && paint.opacity > 0.01 && paint.paint_type = Figma_types.Image)
    node.Figma_types.fills

let node_area node =
  match node.Figma_types.bbox with
  | Some b -> max 0. (b.width *. b.height)
  | None -> 0.

let node_area_score area =
  Float.log10 (area +. 1.)

let node_has_auto_layout node =
  match node.Figma_types.layout_mode with
  | Figma_types.None' -> false
  | _ -> true

let node_has_mask_hint node =
  let text = node_text_blob node in
  matches_any ["mask"; "clip"] text

let node_duplicate_key node =
  let type_str = Figma_query.node_type_to_string node.Figma_types.node_type in
  let name = String.lowercase_ascii (String.trim node.Figma_types.name) in
  let size =
    match node.Figma_types.bbox with
    | Some b -> Printf.sprintf "%.0fx%.0f" b.width b.height
    | None -> "?"
  in
  String.concat "|" [type_str; name; size]

(** Eio context: types and functions moved to mcp_helpers.ml *)
(** Plugin infrastructure (resolve_channel_id, plugin_wait, plugin_exec, etc.)
    moved to mcp_plugin_handlers.ml *)

(** Shared helpers (assoc_or_empty .. resolve_plugin_variables, fidelity scoring,
    sanitize_*, etc.): moved to mcp_helpers.ml *)

let download_image_fill save_dir file_key (id, url) =
  match url with
  | `String url when is_http_url url ->
      let ext = file_ext_from_url url in
      let path = Printf.sprintf "%s/%s/%s%s"
        save_dir (sanitize_file_key file_key) (sanitize_node_id id) ext in
      (match Figma_effects.Perform.download_url ~url ~path with
       | Ok saved ->
           `Assoc [
             ("image_ref", `String id);
             ("url", `String url);
             ("saved", `String saved);
           ]
       | Error err ->
           `Assoc [
             ("image_ref", `String id);
             ("url", `String url);
             ("error", `String err);
           ])
  | `String url ->
      `Assoc [
        ("image_ref", `String id);
        ("url", `String url);
        ("error", `String "Download skipped: invalid URL");
      ]
  | _ ->
      `Assoc [
        ("image_ref", `String id);
        ("error", `String "Invalid image URL payload");
      ]


(** ============== Core Figma API handlers ============== *)

let handle_get_file args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let format = get_string_or "format" "fidelity" args in
  let depth = get_int "depth" args in
  let geometry = get_string "geometry" args in
  let plugin_data = get_string "plugin_data" args in
  let version = get_string "version" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file ~token ~file_key ?depth ?geometry ?plugin_data ?version () with
       | Ok json ->
           (* document 추출 *)
           let doc = Figma_api.extract_document json in
           let doc_str = match doc with
             | Some d -> Yojson.Safe.to_string d
             | None -> Yojson.Safe.to_string json
           in
           (match process_json_string ~format doc_str with
            | Ok result -> Ok (make_text_content result)
            | Error msg -> Error msg)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_file_meta 핸들러 *)
let handle_get_file_meta args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let version = get_string "version" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_meta ~token ~file_key ?version () with
       | Ok json ->
           let meta = build_file_meta json in
           Ok (make_text_content (Yojson.Safe.pretty_to_string meta))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_list_screens 핸들러 *)
let handle_list_screens args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           let screens = Figma_api.get_all_screens json in
           let screen_list = List.map (fun (id, name) ->
             sprintf "- %s (%s)" name id
           ) screens in
           let result = sprintf "Found %d screens:\n%s"
             (List.length screens)
             (String.concat "\n" screen_list)
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_node 핸들러 *)
let handle_get_node args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let format = get_string_or "format" "fidelity" args in
  let depth = get_int "depth" args in
  let geometry = get_string "geometry" args in
  let plugin_data = get_string "plugin_data" args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ?depth ?geometry ?plugin_data ?version () with
       | Ok json ->
           let nodes = member "nodes" json in
           let node_data = match nodes with
             | Some (`Assoc nodes_map) ->
                 (match List.assoc_opt node_id nodes_map with
                  | Some n -> member "document" n
                  | None -> None)
             | _ -> None
           in
           (match node_data with
           | Some node ->
                let node_str = Yojson.Safe.to_string node in
                let node_count = Large_response.count_nodes_json node in
                let prefix = Printf.sprintf "node_%s" (sanitize_node_id node_id) in
                (match process_json_string ~format node_str with
                 | Ok result ->
                     Ok (Large_response.wrap_dsl_with_warning ~prefix ~format ~node_count result)
                 | Error msg -> Error msg)
            | None -> Error (sprintf "Node not found: %s" node_id))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_get_node_bundle 핸들러 *)
let handle_get_node_bundle args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let format = get_string_or "format" "fidelity" args in
  let image_format = get_string_or "image_format" "png" args in
  let scale = get_float_or "scale" 1.0 args in
  let use_absolute_bounds = get_bool "use_absolute_bounds" args in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in
  let include_raw = get_bool_or "include_raw" true args in
  let include_meta = get_bool_or "include_meta" true args in
  let include_variables = get_bool_or "include_variables" true args in
  let include_image_fills = get_bool_or "include_image_fills" true args in
  let auto_plugin =
    match get_bool "auto_plugin" args with
    | Some b -> b
    | None -> Option.is_some (get_string "url" args)
  in
  let include_plugin =
    match get_bool "include_plugin" args with
    | Some b -> b
    | None -> auto_plugin
  in
  let include_plugin_variables = get_bool_or "include_plugin_variables" false args in
  let include_plugin_image = get_bool_or "include_plugin_image" false args in
  let plugin_include_geometry = get_bool_or "plugin_include_geometry" false args in
  let depth = get_int "depth" args in
  let plugin_depth =
    match get_int "plugin_depth" args with
    | Some d when d >= 0 -> d
    | _ -> Option.value ~default:6 depth
  in
  let plugin_image_format = get_string_or "plugin_image_format" "png" args in
  let plugin_image_scale = get_float_or "plugin_image_scale" 1.0 args in
  let plugin_channel_id = get_string "plugin_channel_id" args in
  let plugin_timeout_ms = get_int "plugin_timeout_ms" args |> Option.value ~default:20000 in
  let geometry = get_string "geometry" args in
  let plugin_data = get_string "plugin_data" args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let node_id = normalize_node_id node_id in
      (* 캐시 옵션 생성 *)
      let cache_options =
        List.filter_map Fun.id [
          Option.map (sprintf "depth:%d") depth;
          Option.map (sprintf "geometry:%s") geometry;
          Option.map (sprintf "plugin_data:%s") plugin_data;
          Option.map (sprintf "version:%s") version;
        ]
      in
      (* 캐시에서 먼저 조회 *)
      let cached_json = Figma_cache.get ~file_key ~node_id ~options:cache_options () in
      let json_result = match cached_json with
        | Some json ->
            Printf.eprintf "[Cache] HIT for node %s\n%!" node_id;
            Ok json
        | None ->
            Printf.eprintf "[Cache] MISS for node %s → fetching from API\n%!" node_id;
            match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ?depth ?geometry ?plugin_data ?version () with
            | Error err -> Error err
            | Ok json ->
                (* 성공 시 캐시에 저장 *)
                Figma_cache.set ~file_key ~node_id ~options:cache_options json;
                Ok json
      in
      (match json_result with
       | Error err -> Error err
       | Ok json ->
           let node_lookup =
             match member "nodes" json with
             | Some (`Assoc nodes_map) ->
                 (match find_node_entry nodes_map ~node_id with
                  | Some (node_key, node_entry) ->
                      (match member "document" node_entry with
                       | Some doc -> Ok (node_key, doc)
                       | None -> Error (sprintf "Node %s found but document is null" node_id))
                  | None -> 
                      let keys = List.map fst nodes_map in
                      let keys_str = if keys = [] then "none" else String.concat ", " keys in
                      Error (sprintf "Node %s not found. Available: [%s]" node_id keys_str))
             | Some _ -> Error "API returned nodes in unexpected format"
             | None -> Error "API response missing 'nodes' field"
           in
           (match node_lookup with
            | Error msg -> Error msg
            | Ok (node_key, node) ->
                let node_str = Yojson.Safe.to_string node in
                let dsl_str = match process_json_string ~format node_str with
                  | Ok s -> s
                  | Error msg -> msg
                in
                let dsl_json =
                  try Yojson.Safe.from_string dsl_str
                  with exn ->
                    Printf.eprintf "[mcp_tools] Warning: DSL JSON parse failed for node %s: %s\n%!" node_id (Printexc.to_string exn);
                    `Null
                in
                let (image_url, image_download) =
                  match Figma_effects.Perform.get_images
                          ~token ~file_key ~node_ids:[node_id]
                          ~format:image_format ~scale
                          ?use_absolute_bounds ?version () with
                  | Ok img_json ->
                      let url =
                        match member "images" img_json with
                        | Some (`Assoc img_map) ->
                            (match List.assoc_opt node_key img_map with
                             | Some (`String u) -> u
                             | _ -> "No image URL returned")
                        | _ -> "No images returned"
                      in
                      if download then
                        if is_http_url url then
                          let path = Printf.sprintf "%s/%s/%s.%s"
                            save_dir (sanitize_file_key file_key) (sanitize_node_id node_id) image_format in
                          (match Figma_effects.Perform.download_url ~url ~path with
                           | Ok saved -> (url, `String saved)
                           | Error err -> (url, `String ("Download error: " ^ err)))
                        else
                          (url, `String "Download skipped: no image URL")
                      else
                        (url, `Null)
                  | Error err -> ("Image error: " ^ err, `Null)
                in
                let file_meta =
                  if include_meta then
                    match Figma_effects.Perform.get_file_meta ~token ~file_key ?version () with
                    | Ok meta_json -> build_file_meta meta_json
                    | Error err -> `Assoc [("error", `String err)]
                  else
                    `Null
                in
                let resolve_plugin_channel () =
                  match plugin_channel_id with
                  | Some id -> Ok id
                  | None -> resolve_channel_id args
                in
                let want_plugin_variables =
                  include_plugin_variables || (include_plugin && include_variables)
                in
                let plugin_variables =
                  if want_plugin_variables then
                    match resolve_plugin_channel () with
                    | Error msg -> `Assoc [("error", `String msg)]
                    | Ok channel_id ->
                        let payload = `Assoc [] in
                        let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_variables" ~payload in
                        (match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                         | Error err -> `Assoc [("error", `String err)]
                         | Ok result ->
                             `Assoc [
                               ("channel_id", `String channel_id);
                               ("command_id", `String command_id);
                               ("ok", `Bool result.ok);
                               ("payload", result.payload);
                             ])
                  else
                    `Null
                in
                let (variables, variables_source) =
                  if include_variables then
                    match fetch_variables_cached ~file_key ~token with
                    | Ok (vars_json, source) -> (resolve_variables vars_json, source)
                    | Error err ->
                        (match plugin_payload_if_ok plugin_variables with
                         | Some payload -> (resolve_plugin_variables payload, `String "plugin")
                         | None -> (`Assoc [("error", `String err)], `String "error"))
                  else
                    (`Null, `Null)
                in
                let image_fills =
                  if include_image_fills then
                    match Figma_effects.Perform.get_file_images ~token ~file_key ?version () with
                    | Ok img_json ->
                        let images =
                          match member "images" img_json with
                          | Some (`Assoc _ as m) -> m
                          | _ -> `Null
                        in
                        let downloads =
                          if download then
                            match images with
                            | `Assoc items ->
                                `List (List.map (download_image_fill save_dir file_key) items)
                            | _ -> `List []
                          else
                            `List []
                        in
                        `Assoc [("images", images); ("downloads", downloads)]
                    | Error err -> `Assoc [("error", `String err)]
                  else
                    `Null
                in
                let plugin_snapshot =
                  if include_plugin then
                    (match resolve_plugin_channel () with
                     | Error msg -> `Assoc [("error", `String msg)]
                     | Ok channel_id ->
                         let run_snapshot depth_used =
                           let payload = `Assoc [
                             ("node_id", `String node_id);
                             ("depth", `Int depth_used);
                             ("include_geometry", `Bool plugin_include_geometry);
                           ] in
                           let command_id =
                             Figma_plugin_bridge.enqueue_command
                               ~channel_id
                               ~name:"get_node"
                               ~payload
                           in
                           match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                           | Error err -> Error err
                           | Ok result ->
                               Ok (`Assoc [
                                 ("channel_id", `String channel_id);
                                 ("command_id", `String command_id);
                                 ("ok", `Bool result.ok);
                                 ("payload", result.payload);
                                 ("plugin_depth", `Int depth_used);
                               ])
                         in
                         match run_snapshot plugin_depth with
                         | Ok snapshot -> snapshot
                         | Error err ->
                             if plugin_depth > 0 then
                               (match run_snapshot 0 with
                                | Ok snapshot ->
                                    (match snapshot with
                                     | `Assoc fields ->
                                         `Assoc (("note", `String "plugin snapshot fallback to depth=0")
                                                 :: ("fallback_error", `String err)
                                                 :: fields)
                                     | _ -> snapshot)
                                | Error err2 -> `Assoc [("error", `String err2)])
                             else
                               `Assoc [("error", `String err)])
                  else
                    `Null
                in
                let plugin_image =
                  if include_plugin_image then
                    (match resolve_plugin_channel () with
                     | Error msg -> `Assoc [("error", `String msg)]
                     | Ok channel_id ->
                         let payload = `Assoc [
                           ("node_id", `String node_id);
                           ("format", `String plugin_image_format);
                           ("scale", `Float plugin_image_scale);
                         ] in
                         let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"export_node_image" ~payload in
                         (match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                          | Error err -> `Assoc [("error", `String err)]
                          | Ok result ->
                              let response =
                                `Assoc [
                                  ("channel_id", `String channel_id);
                                  ("command_id", `String command_id);
                                  ("ok", `Bool result.ok);
                                  ("payload", result.payload);
                                ]
                              in
                              if download then
                                `Assoc [
                                  ("note", `String "download=true is ignored for plugin_image (base64 only)");
                                  ("response", response);
                                ]
                              else
                                response))
                  else
                    `Null
                in
                let fidelity =
                  match dsl_json with
                  | `Assoc _ as json ->
                      let (overall, missing_total, sections) =
                        fidelity_score_of_bundle
                          ~dsl_json:json
                          ~variables
                          ~image_fills
                          ~plugin_snapshot
                          ~include_variables
                          ~include_image_fills
                          ~include_plugin
                      in
                      `Assoc [
                        ("overall", `Float overall);
                        ("missing_total", `Int missing_total);
                        ("sections", sections);
                      ]
                  | _ -> `Null
                in
                let result =
                  `Assoc [
                    ("file_key", `String file_key);
                    ("node_id", `String node_id);
                    ("dsl", `String dsl_str);
                    ("dsl_json", dsl_json);
                    ("node_raw", if include_raw then node else `Null);
                    ("image", `Assoc [
                      ("url", `String image_url);
                      ("download", image_download);
                    ]);
                    ("file_meta", file_meta);
                    ("variables", variables);
                    ("variables_source", variables_source);
                    ("plugin_variables", plugin_variables);
                    ("image_fills", image_fills);
                    ("plugin_snapshot", plugin_snapshot);
                    ("plugin_image", plugin_image);
                    ("fidelity", fidelity);
                  ]
                in
                (* Large Response Handler: 500KB 초과 시 파일로 저장 *)
                let result_str = Yojson.Safe.pretty_to_string result in
                let prefix = Printf.sprintf "node_%s" (sanitize_node_id node_id) in
                Ok (Large_response.wrap_string_result ~prefix ~format result_str)))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_get_node_summary 핸들러 - 경량 구조 요약 *)
let handle_get_node_summary args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let max_children = get_int_positive "max_children" 50 args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let node_id = normalize_node_id node_id in
      (* 최소 depth=1로 자식만 가져옴 *)
      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ~depth:1 ?version () with
       | Error err -> Error (Printf.sprintf "Figma API error: %s" err)
       | Ok nodes_json ->
           let module U = Yojson.Safe.Util in
           let nodes_map =
             match U.member "nodes" nodes_json with
             | `Assoc map -> map
             | _ -> []
           in
           let node_data =
             match find_node_entry nodes_map ~node_id with
             | Some (_key, node_entry) ->
                 (match U.member "document" node_entry with
                  | `Null -> None
                  | doc -> Some doc)
             | None -> None
           in
           (match node_data with
            | None ->
                (* Better error message with available keys *)
                let available_keys = List.map fst nodes_map in
                let keys_str = if available_keys = [] then "none" 
                  else String.concat ", " available_keys in
                Error (Printf.sprintf "Node %s not found in file %s. Available keys: [%s]" 
                  node_id file_key keys_str)
            | Some node_data ->
                let children =
                  match U.member "children" node_data with
                  | `List xs -> xs
                  | _ -> []
                in
                let children_count = List.length children in
                let children_summary =
                  children
                  |> List.mapi (fun i child ->
                      if i >= max_children then None
                      else
                        let id =
                          match U.member "id" child with
                          | `String s -> s
                          | _ -> ""
                        in
                        let name =
                          match U.member "name" child with
                          | `String s -> s
                          | _ -> ""
                        in
                        let typ =
                          match U.member "type" child with
                          | `String s -> s
                          | _ -> "UNKNOWN"
                        in
                        let sub_children =
                          match U.member "children" child with
                          | `List xs -> List.length xs
                          | _ -> 0
                        in
                        Some
                          (`Assoc
                            [
                              ("id", `String id);
                              ("name", `String name);
                              ("type", `String typ);
                              ("children_count", `Int sub_children);
                            ]))
                  |> List.filter_map Fun.id
                in
                let node_name =
                  match U.member "name" node_data with
                  | `String s -> s
                  | _ -> ""
                in
                let node_type =
                  match U.member "type" node_data with
                  | `String s -> s
                  | _ -> "UNKNOWN"
                in
                Ok
                  (`Assoc
                    [
                      ("node_id", `String node_id);
                      ("name", `String node_name);
                      ("type", `String node_type);
                      ("children_count", `Int children_count);
                      ("children", `List children_summary);
                      ("truncated", `Bool (children_count > max_children));
                      ( "hint",
                        `String
                          "Use figma_get_node_chunk for progressive loading of specific depth ranges" );
                    ])))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_select_nodes 핸들러 - 점수 기반 후보 선별 *)
let handle_select_nodes args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let max_summary_depth = 6 in
  let raw_summary_depth = match get_int "summary_depth" args with Some d -> d | _ -> 1 in
  let summary_depth =
    if raw_summary_depth < 0 then 1 else min raw_summary_depth max_summary_depth
  in
  let preview = get_bool_or "preview" true args in
  let preview_format = get_string_or "preview_format" "png" args in
  let raw_preview_scale = get_float_or "preview_scale" 1.0 args in
  let preview_scale =
    if raw_preview_scale < 0.01 then 0.01
    else if raw_preview_scale > 4.0 then 4.0
    else raw_preview_scale
  in
  let layout_only = get_bool_or "layout_only" false args in
  let auto_layout_only = get_bool_or "auto_layout_only" false args in
  let raw_text_mode = get_string_or "text_mode" "include" args in
  let text_mode =
    match raw_text_mode with
    | "include" | "exclude" | "only" -> raw_text_mode
    | _ -> "include"
  in
  let score_threshold = get_float_or "score_threshold" 2.0 args in
  let max_parents = get_int_positive "max_parents" 8 args in
  let notes_limit = get_int_positive "notes_limit" 50 args in
  let excluded_limit = get_int_positive "excluded_limit" 50 args in
  let version = get_string "version" args in
  let exclude_patterns =
    get_string_list "exclude_patterns" args
    |> Option.value ~default:default_exclude_patterns
    |> normalize_patterns
  in
  let note_patterns =
    get_string_list "note_patterns" args
    |> Option.value ~default:default_note_patterns
    |> normalize_patterns
  in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let node_id = Figma_api.normalize_node_id node_id in
      let config = {
        layout_only;
        auto_layout_only;
        text_mode;
        score_threshold;
        max_parents;
        summary_depth;
        exclude_patterns;
        note_patterns;
        notes_limit;
        excluded_limit;
      } in

      let warnings = ref [] in
      if raw_text_mode <> text_mode then
        warnings := "Invalid text_mode, fallback to include" :: !warnings;
      if raw_summary_depth <> summary_depth then
        warnings := Printf.sprintf "summary_depth clamped to %d" summary_depth :: !warnings;
      if raw_preview_scale <> preview_scale then
        warnings := "preview_scale clamped to 0.01-4.0" :: !warnings;

      let preview_json =
        if not preview then `Null
        else
          match Figma_effects.Perform.get_images
                  ~token ~file_key ~node_ids:[node_id]
                  ~format:preview_format ~scale:preview_scale ?version () with
          | Error err ->
              `Assoc [
                ("status", `String "error");
                ("error", `String err);
              ]
          | Ok json ->
              let open Yojson.Safe.Util in
              let images = json |> member "images" in
              let url =
                match images with
                | `Assoc map ->
                    (match List.assoc_opt node_id map with
                     | Some (`String url) -> Some url
                     | _ -> None)
                | _ -> None
              in
              (match url with
               | Some url ->
                   `Assoc [
                     ("status", `String "ok");
                     ("url", `String url);
                     ("format", `String preview_format);
                     ("scale", `Float preview_scale);
                   ]
               | None ->
                   `Assoc [
                     ("status", `String "missing");
                     ("format", `String preview_format);
                     ("scale", `Float preview_scale);
                   ])
      in

      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ~depth:summary_depth ?version () with
       | Error err -> Error (Printf.sprintf "Figma API error: %s" err)
       | Ok nodes_json ->
           let open Yojson.Safe.Util in
           let nodes = nodes_json |> member "nodes" in
           let node_entry = nodes |> member node_id in
           (match node_entry with
            | `Null -> Error (Printf.sprintf "Node %s not found in file %s" node_id file_key)
            | _ ->
                let node_data = node_entry |> member "document" in
                (match node_data with
                 | `Null -> Error (Printf.sprintf "Document not found for node %s" node_id)
                 | _ ->
                     (match Figma_parser.parse_node ~max_depth:summary_depth node_data with
                      | None -> Error "Failed to parse node JSON"
                      | Some root ->
                          let all_nodes = Figma_query.collect_nodes ~max_depth:(Some summary_depth) root in
                          let notes =
                            all_nodes
                            |> List.filter node_is_text
                            |> List.filter (fun node ->
                                matches_any config.note_patterns (node_text_blob node))
                            |> (fun nodes ->
                                let rec take acc count = function
                                  | [] -> List.rev acc
                                  | _ when count >= config.notes_limit -> List.rev acc
                                  | n :: rest -> take (n :: acc) (count + 1) rest
                                in
                                take [] 0 nodes)
                            |> List.map (fun node ->
                                let text = Option.value ~default:"" node.Figma_types.characters in
                                let pattern =
                                  find_matching_pattern config.note_patterns (node_text_blob node)
                                  |> Option.value ~default:""
                                in
                                `Assoc [
                                  ("id", `String node.Figma_types.id);
                                  ("name", `String node.Figma_types.name);
                                  ("type", `String (Figma_query.node_type_to_string node.Figma_types.node_type));
                                  ("text", `String text);
                                  ("pattern", `String pattern);
                                ])
                          in

                          let candidates =
                            if root.Figma_types.children = [] then [root] else root.Figma_types.children
                          in
                          let duplicates = Hashtbl.create 32 in
                          let next_duplicate_index node =
                            let key = node_duplicate_key node in
                            let count = Option.value ~default:0 (Hashtbl.find_opt duplicates key) in
                            Hashtbl.replace duplicates key (count + 1);
                            count
                          in

                          let scored = ref [] in
                          let excluded = ref [] in
                          List.iter (fun node ->
                              let name_blob = node_text_blob node in
                              let duplicate_index = next_duplicate_index node in
                              let exclusion_reason =
                                if (not node.Figma_types.visible) || node.Figma_types.opacity <= 0.01 then
                                  Some "invisible"
                                else if config.auto_layout_only && not (node_has_auto_layout node) then
                                  Some "auto_layout_only"
                                else if config.layout_only && not (node_is_container node) then
                                  Some "layout_only"
                                else if config.text_mode = "exclude" && node_is_text node then
                                  Some "text_mode_exclude"
                                else if config.text_mode = "only" && not (node_is_text node) then
                                  Some "text_mode_only"
                                else if matches_any config.exclude_patterns name_blob then
                                  Some "excluded_pattern"
                                else if node_is_text node && matches_any config.note_patterns name_blob then
                                  Some "note_text"
                                else
                                  None
                              in
                              match exclusion_reason with
                              | Some reason ->
                                  let pattern =
                                    if reason = "excluded_pattern" then
                                      find_matching_pattern config.exclude_patterns name_blob
                                    else if reason = "note_text" then
                                      find_matching_pattern config.note_patterns name_blob
                                    else
                                      None
                                  in
                                  let reason =
                                    match pattern with
                                    | Some p -> Printf.sprintf "%s:%s" reason p
                                    | None -> reason
                                  in
                                  excluded := (node, reason) :: !excluded
                              | None ->
                                  let score = ref 0.0 in
                                  let reasons = ref [] in
                                  let add amount label =
                                    if amount <> 0.0 then begin
                                      score := !score +. amount;
                                      reasons := label :: !reasons
                                    end
                                  in
                                  if node_is_text node then add 2.0 "text:+2";
                                  if node_has_image_fill node then add 2.0 "image_fill:+2";
                                  if node_has_auto_layout node then add 1.5 "auto_layout:+1.5";
                                  if node_is_component node then add 1.0 "component:+1";
                                  if node_has_mask_hint node then add 1.0 "mask_or_clip:+1";
                                  let area = node_area node in
                                  let area_score = node_area_score area in
                                  if area_score > 0.0 then
                                    add area_score (Printf.sprintf "area:+%.2f" area_score);
                                  let small_penalty =
                                    if area < 64.0 then 2.0
                                    else if area < 256.0 then 1.0
                                    else 0.0
                                  in
                                  if small_penalty > 0.0 then
                                    add (-. small_penalty)
                                      (Printf.sprintf "small_area:-%.1f" small_penalty);
                                  if duplicate_index > 0 then
                                    add (-. 1.0) "duplicate:-1";
                                  if matches_any ["as-is"; "as is"; "asis"] name_blob then
                                    add (-. 0.5) "as_is:-0.5";
                                  scored := (node, !score, List.rev !reasons, area) :: !scored
                            ) candidates;

                          let scored_sorted =
                            List.sort (fun (_, a, _, _) (_, b, _, _) -> Float.compare b a) !scored
                          in
                          let scored_selected =
                            scored_sorted
                            |> List.filter (fun (_, score, _, _) -> score >= config.score_threshold)
                            |> (fun nodes ->
                                let rec take acc count = function
                                  | [] -> List.rev acc
                                  | _ when count >= config.max_parents -> List.rev acc
                                  | n :: rest -> take (n :: acc) (count + 1) rest
                                in
                                take [] 0 nodes)
                          in
                          let selected, selection_mode =
                            if scored_selected = [] && scored_sorted <> [] then
                              let rec take acc count = function
                                | [] -> List.rev acc
                                | _ when count >= config.max_parents -> List.rev acc
                                | n :: rest -> take (n :: acc) (count + 1) rest
                              in
                              (take [] 0 scored_sorted, "fallback_top_scores")
                            else
                              (scored_selected, "threshold")
                          in

                          let selected_json =
                            selected
                            |> List.map (fun (node, score, reasons, area) ->
                                let (width, height) =
                                  match node.Figma_types.bbox with
                                  | Some b -> (b.width, b.height)
                                  | None -> (0., 0.)
                                in
                                `Assoc [
                                  ("id", `String node.Figma_types.id);
                                  ("name", `String node.Figma_types.name);
                                  ("type", `String (Figma_query.node_type_to_string node.Figma_types.node_type));
                                  ("score", `Float score);
                                  ("area", `Float area);
                                  ("width", `Float width);
                                  ("height", `Float height);
                                  ("reasons", `List (List.map (fun r -> `String r) reasons));
                                ])
                          in

                          let excluded_json =
                            !excluded
                            |> (fun nodes ->
                                let rec take acc count = function
                                  | [] -> List.rev acc
                                  | _ when count >= config.excluded_limit -> List.rev acc
                                  | n :: rest -> take (n :: acc) (count + 1) rest
                                in
                                take [] 0 nodes)
                            |> List.map (fun (node, reason) ->
                                `Assoc [
                                  ("id", `String node.Figma_types.id);
                                  ("name", `String node.Figma_types.name);
                                  ("type", `String (Figma_query.node_type_to_string node.Figma_types.node_type));
                                  ("reason", `String reason);
                                ])
                          in

                          let root_summary =
                            `Assoc [
                              ("id", `String root.Figma_types.id);
                              ("name", `String root.Figma_types.name);
                              ("type", `String (Figma_query.node_type_to_string root.Figma_types.node_type));
                              ("children_count", `Int (List.length root.Figma_types.children));
                            ]
                          in

                          let result =
                            `Assoc [
                              ("file_key", `String file_key);
                              ("node_id", `String node_id);
                              ("summary_depth", `Int summary_depth);
                              ("preview", preview_json);
                              ("root", root_summary);
                              ("selection_mode", `String selection_mode);
                              ("score_threshold", `Float config.score_threshold);
                              ("max_parents", `Int config.max_parents);
                              ("layout_only", `Bool config.layout_only);
                              ("auto_layout_only", `Bool config.auto_layout_only);
                              ("text_mode", `String config.text_mode);
                              ("selected", `List selected_json);
                              ("selected_count", `Int (List.length selected_json));
                              ("excluded", `List excluded_json);
                              ("excluded_count", `Int (List.length !excluded));
                              ("notes", `List notes);
                              ("notes_count", `Int (List.length notes));
                              ("warnings", `List (List.map (fun w -> `String w) (List.rev !warnings)));
                            ]
                          in
                          Ok (make_text_content (Yojson.Safe.pretty_to_string result))
                     )))
           )
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_get_node_chunk 핸들러 - 깊이 범위별 청크 로드 *)
let handle_get_node_chunk args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let depth_start = get_int_nonneg "depth_start" 0 args in
  let depth_end = get_int_nonneg "depth_end" 2 args in
  let format = get_string_or "format" "fidelity" args in
  let max_children = get_int "max_children" args in
  let warn_large = get_bool_or "warn_large" true args in
  let warn_threshold = get_int "warn_threshold" args |> Option.value ~default:500 in
  let error_on_large = get_bool_or "error_on_large" false args in
  let auto_trim_children = get_bool_or "auto_trim_children" false args in
  let auto_trim_limit = get_int "auto_trim_limit" args |> Option.value ~default:200 in
  let include_styles = get_bool_or "include_styles" false args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let node_id = Figma_api.normalize_node_id node_id in
      if depth_end < depth_start then
        Error "depth_end must be >= depth_start"
      else
        (* depth_end까지만 가져옴 *)
        let api_depth = depth_end + 1 in
        (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ~depth:api_depth ?version () with
         | Error err -> Error (Printf.sprintf "Figma API error: %s" err)
         | Ok nodes_json ->
             let open Yojson.Safe.Util in
             let nodes = nodes_json |> member "nodes" in
             let node_entry = nodes |> member node_id in
             (match node_entry with
              | `Null -> Error (Printf.sprintf "Node %s not found in file %s" node_id file_key)
              | _ ->
                  let node_data = node_entry |> member "document" in
                  (match node_data with
                   | `Null -> Error (Printf.sprintf "Document not found for node %s" node_id)
                   | _ ->

             let root_children_count =
               try (node_data |> member "children" |> to_list |> List.length)
               with Yojson.Safe.Util.Type_error _ -> 0
             in

             let effective_max_children =
               match max_children, auto_trim_children with
               | Some limit, _ -> Some limit
               | None, true -> Some (max 0 auto_trim_limit)
               | None, false -> None
             in

             let warnings = ref [] in
             let add_warning msg = warnings := msg :: !warnings in
             let is_large =
               warn_large && effective_max_children = None && root_children_count > warn_threshold
             in
             let large_error =
               if error_on_large && is_large then
                 Some (Printf.sprintf
                   "Large node %s: %d children at root (warn_threshold=%d). Set max_children/auto_trim_children or use figma_get_node_chunk + figma_read_large_result."
                   node_id root_children_count warn_threshold)
               else
                 None
             in
             (match effective_max_children, auto_trim_children with
              | Some limit, true when max_children = None ->
                  add_warning (Printf.sprintf "auto_trim_children applied: max_children=%d" limit)
              | _ -> ());
             (match warn_large, root_children_count, effective_max_children with
              | true, count, None when count > warn_threshold ->
                  add_warning (Printf.sprintf
                    "Large node %s: %d children at root (warn_threshold=%d). Consider max_children/auto_trim_children or figma_get_node_chunk + figma_read_large_result."
                    node_id count warn_threshold)
              | _ -> ());

             let take_n n lst =
               let rec loop acc i = function
                 | [] -> List.rev acc
                 | _ when i >= n -> List.rev acc
                 | x :: xs -> loop (x :: acc) (i + 1) xs
               in
               loop [] 0 lst
             in

             let trim_children children =
               match effective_max_children with
               | Some limit when limit >= 0 ->
                   let total = List.length children in
                   if total > limit then
                     (take_n limit children, Some (total - limit))
                   else
                     (children, None)
               | _ -> (children, None)
             in

             let append_truncated assoc truncated =
               match truncated with
               | Some n -> assoc @ [("_truncated_children", `Int n)]
               | None -> assoc
             in

             (* null-safe children 추출 *)
             let get_children_safe json =
               match json |> member "children" with
               | `Null -> []
               | `List lst -> lst
               | _ -> []
             in

             (* 깊이 범위에 따라 필터링하는 재귀 함수 *)
             let rec filter_by_depth current_depth json =
               if current_depth < depth_start then
                 (* 시작 깊이 미만: 자식만 재귀 처리 *)
                 let children = get_children_safe json in
                 let children, truncated = trim_children children in
                 let filtered_children = List.filter_map (fun c ->
                     let result = filter_by_depth (current_depth + 1) c in
                     if result = `Null then None else Some result
                   ) children
                 in
                 if filtered_children = [] then `Null
                 else
                   let assoc = to_assoc json in
                   let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                   let assoc = without_children @ [("children", `List filtered_children)] in
                   `Assoc (append_truncated assoc truncated)
               else if current_depth > depth_end then
                 (* 종료 깊이 초과: 자식 제거 *)
                 let assoc = to_assoc json in
                 let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                 let children_count = get_children_safe json |> List.length in
                 `Assoc (without_children @ [("_truncated_children", `Int children_count)])
               else
                 (* 범위 내: 자식 재귀 처리 *)
                 let children = get_children_safe json in
                 let children, truncated = trim_children children in
                 let filtered_children = List.map (fun c -> filter_by_depth (current_depth + 1) c) children in
                 let assoc = to_assoc json in
                 let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                 let assoc = without_children @ [("children", `List filtered_children)] in
                 `Assoc (append_truncated assoc truncated)
             in

             match large_error with
             | Some msg -> Error msg
             | None ->
                 let filtered = filter_by_depth 0 node_data in
                 let base =
                   let styles =
                     if include_styles then
                       match Figma_effects.Perform.get_file_styles ~token ~file_key with
                       | Ok json -> json
                       | Error err -> `Assoc [("error", `String err)]
                     else
                       `Null
                   in
                   let filtered_str = Yojson.Safe.to_string filtered in
                   match process_json_string ~format filtered_str with
                   | Ok dsl ->
                       `Assoc [
                         ("type", `String "text");
                         ("text", `String dsl);
                         ("depth_range", `String (Printf.sprintf "%d-%d" depth_start depth_end));
                         ("format", `String format);
                         ("styles", styles);
                       ]
                   | Error msg ->
                       `Assoc [
                         ("error", `String msg);
                         ("node", filtered);
                         ("depth_range", `String (Printf.sprintf "%d-%d" depth_start depth_end));
                         ("styles", styles);
                       ]
                 in
                 let result =
                   let warning =
                     match !warnings with
                     | [] -> None
                     | msgs -> Some (String.concat " | " (List.rev msgs))
                   in
                   match warning with
                   | Some msg ->
                       (match base with
                        | `Assoc fields -> `Assoc (fields @ [("warning", `String msg)])
                        | _ -> base)
                   | None -> base
                 in

                 (* Large Response Handler 적용 *)
                 let result_str = Yojson.Safe.pretty_to_string result in
                 let prefix = Printf.sprintf "chunk_%s_%d_%d" (sanitize_node_id node_id) depth_start depth_end in
                 Ok (Large_response.wrap_string_result ~prefix ~format result_str))))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_export_image 핸들러 - Streaming Progress 지원 *)
let handle_export_image args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_ids_str = get_string "node_ids" args in
  let token = resolve_token args in
  let format = get_string_or "format" "png" args in
  let scale = get_float_or "scale" 1.0 args in
  let use_absolute_bounds = get_bool "use_absolute_bounds" args in
  let version = get_string "version" args in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in

  match (file_key, node_ids_str, token) with
  | (Some file_key, Some node_ids_str, Some token) ->
      let node_ids =
        node_ids_str
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
        |> List.map normalize_node_id
      in
      (match Figma_effects.Perform.get_images ~token ~file_key ~node_ids ~format ~scale
               ?use_absolute_bounds ?version () with
       | Ok json ->
           let images = member "images" json in
           let result = match images with
             | Some (`Assoc img_map) ->
                 let total = List.length img_map in
                 (* 3개 이상 이미지 다운로드 시 Progress 알림 활성화 *)
                 let progress_token =
                   if download && total >= 3 then
                     Some (Mcp_progress.make_progress_token ())
                   else None
                 in
                 let _ = match progress_token with
                   | Some pt ->
                       Mcp_progress.update_progress ~token:pt ~current:0 ~total
                         ~message:(sprintf "Starting export of %d images..." total) ()
                   | None -> ()
                 in
                 let results = List.mapi (fun idx (id, url) ->
                   let result_str = match url with
                     | `String url ->
                         if download then
                           if is_http_url url then
                             let path = Printf.sprintf "%s/%s/%s.%s"
                               save_dir (sanitize_file_key file_key) (sanitize_node_id id) format in
                             (match Figma_effects.Perform.download_url ~url ~path with
                              | Ok saved -> sprintf "%s: %s -> %s" id url saved
                              | Error err -> sprintf "%s: %s (download error: %s)" id url err)
                           else
                             sprintf "%s: %s (download skipped: no URL)" id url
                         else
                           sprintf "%s: %s" id url
                     | _ -> sprintf "%s: (error)" id
                   in
                   (* Progress 업데이트 *)
                   let _ = match progress_token with
                     | Some pt ->
                         Mcp_progress.update_progress ~token:pt ~current:(idx + 1) ~total
                           ~message:(sprintf "Downloaded %d/%d: %s" (idx + 1) total id) ()
                     | None -> ()
                   in
                   result_str
                 ) img_map in
                 (* 완료 알림 *)
                 let _ = match progress_token with
                   | Some pt ->
                       Mcp_progress.update_progress ~token:pt ~current:total ~total
                         ~message:(sprintf "Export complete: %d images" total) ()
                   | None -> ()
                 in
                 String.concat "\n" results
             | _ -> "No images returned"
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, node_ids, token"

(** figma_export_smart 핸들러 - 대형 노드 자동 scale 조정 및 재귀 분할 *)
let handle_export_smart args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  let token = resolve_token args in
  let format = get_string_or "format" "png" args in
  let max_pixels = get_float_or "max_pixels" 16777216.0 args in  (* 4096x4096 default *)
  let split_children = get_bool_or "split_children" false args in
  let max_depth = Option.value ~default:1 (get_int "max_depth" args) in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in
  let include_debug = get_bool_or "debug" false args in

  (* Calculate optimal scale to fit within max_pixels *)
  let auto_scale ~width ~height =
    let actual = float_of_int (width * height) in
    if actual <= max_pixels then 1.0
    else
      let ratio = sqrt (max_pixels /. actual) in
      max 0.01 (min 4.0 ratio)  (* Figma API: scale must be 0.01-4.0 *)
  in

  (* Get node dimensions from absoluteBoundingBox *)
  let get_node_dims json =
    match member "absoluteBoundingBox" json with
    | Some box ->
        let w = match member "width" box with Some (`Float f) -> int_of_float f | Some (`Int i) -> i | _ -> 0 in
        let h = match member "height" box with Some (`Float f) -> int_of_float f | Some (`Int i) -> i | _ -> 0 in
        (w, h)
    | None -> (0, 0)
  in

  (* Get child node IDs *)
  let get_child_ids json =
    match member "children" json with
    | Some (`List children) ->
        List.filter_map (fun child ->
          match member "id" child with
          | Some (`String id) -> Some id
          | _ -> None
        ) children
    | _ -> []
  in

  (* Export a single node with calculated scale *)
  let export_node ~node_id ~scale =
    match Figma_effects.Perform.get_images ~token:(Option.get token)
            ~file_key:(Option.get file_key) ~node_ids:[node_id] ~format
            ~scale () with
    | Ok json ->
        (match member "images" json with
         | Some (`Assoc img_map) ->
             List.filter_map (fun (id, url) ->
               match url with
               | `String url_str ->
                   let final_path =
                     if download && is_http_url url_str then
                      let path = Printf.sprintf "%s/%s/%s.%s"
                        save_dir (sanitize_file_key (Option.get file_key)) (sanitize_node_id id) format in
                       match Figma_effects.Perform.download_url ~url:url_str ~path with
                       | Ok saved -> Some saved
                       | Error _ -> Some url_str
                     else Some url_str
                   in
                   Option.map (fun p -> `Assoc [
                     ("node_id", `String id);
                     ("url", `String url_str);
                     ("scale", `Float scale);
                     ("path", `String p);
                   ]) final_path
               | _ -> None
             ) img_map
         | _ -> [])
    | Error _ -> []
  in

  (* Debug info accumulator *)
  let debug_info = ref [] in

  (* Recursive export for split_children *)
  let rec export_recursive ~node_id ~depth results =
    if depth > max_depth then results
    else
      (* Get node info via get_nodes API *)
      match Figma_effects.Perform.get_nodes ~token:(Option.get token)
              ~file_key:(Option.get file_key) ~node_ids:[node_id] ~depth:1 () with
      | Ok json ->
          (* Extract node from "nodes" -> node_id -> "document" *)
          let nodes_opt = member "nodes" json in
          let node_json = match nodes_opt with
            | Some (`Assoc nodes) ->
                debug_info := !debug_info @ [Printf.sprintf "Found nodes with %d entries, looking for '%s'" (List.length nodes) node_id];
                debug_info := !debug_info @ [Printf.sprintf "Available keys: %s" (String.concat ", " (List.map fst nodes))];
                (match List.assoc_opt node_id nodes with
                 | Some node_data ->
                     let node_data_str = Yojson.Safe.to_string node_data in
                     let truncated = if String.length node_data_str > 200 then String.sub node_data_str 0 200 ^ "..." else node_data_str in
                     debug_info := !debug_info @ [Printf.sprintf "Found node_data: %s" truncated];
                     let doc_opt = member "document" node_data in
                     (match doc_opt with
                      | Some doc -> debug_info := !debug_info @ ["document found!"]; Some doc
                      | None ->
                          let keys = match node_data with `Assoc lst -> List.map fst lst | _ -> [] in
                          debug_info := !debug_info @ [Printf.sprintf "document NOT found. node_data keys: %s" (String.concat ", " keys)];
                          None)
                 | None ->
                     debug_info := !debug_info @ ["Node ID not found in nodes"];
                     None)
            | Some other ->
                let str = Yojson.Safe.to_string other in
                let truncated = if String.length str > 100 then String.sub str 0 100 ^ "..." else str in
                debug_info := !debug_info @ [Printf.sprintf "nodes is not Assoc: %s" truncated];
                None
            | None ->
                debug_info := !debug_info @ ["No 'nodes' key in response"];
                None
          in
          let (w, h) = match node_json with
            | Some n ->
                let dims = get_node_dims n in
                debug_info := !debug_info @ [Printf.sprintf "Got dimensions: %dx%d" (fst dims) (snd dims)];
                dims
            | None ->
                debug_info := !debug_info @ ["node_json is None"];
                (0, 0)
          in
          let actual_pixels = w * h in
          if actual_pixels = 0 then (debug_info := !debug_info @ ["actual_pixels=0, returning empty"]; results)
          else if float_of_int actual_pixels <= max_pixels then
            (* Node fits, export directly *)
            let scale = auto_scale ~width:w ~height:h in
            let exported = export_node ~node_id ~scale in
            results @ exported
          else if split_children && depth < max_depth then
            (* Too big, try children *)
            let child_ids = match node_json with Some n -> get_child_ids n | None -> [] in
            if child_ids = [] then
              (* No children, force scale down *)
              let scale = auto_scale ~width:w ~height:h in
              let exported = export_node ~node_id ~scale in
              results @ exported
            else
              (* Recurse into children *)
              List.fold_left (fun acc child_id ->
                export_recursive ~node_id:child_id ~depth:(depth + 1) acc
              ) results child_ids
          else
            (* Not splitting, just scale down *)
            let scale = auto_scale ~width:w ~height:h in
            let exported = export_node ~node_id ~scale in
            results @ exported
      | Error err ->
          debug_info := !debug_info @ [Printf.sprintf "get_nodes returned Error: %s" err];
          results
  in

  match (file_key, node_id, token) with
  | (Some _file_key, Some node_id, Some _token) ->
      let normalized = normalize_node_id node_id in
      debug_info := !debug_info @ [Printf.sprintf "Starting with node_id='%s', normalized='%s'" node_id normalized];
      let results = export_recursive ~node_id:normalized ~depth:0 [] in
      let base_fields = [
        ("total_exports", `Int (List.length results));
        ("max_pixels", `Float max_pixels);
        ("split_children", `Bool split_children);
        ("exports", `List results);
      ] in
      let summary = `Assoc (
        if include_debug then
          base_fields @ [("debug", `List (List.map (fun s -> `String s) !debug_info))]
        else
          base_fields
      ) in
      Ok (make_text_content (Yojson.Safe.pretty_to_string summary))
  | _ -> Error "Missing required parameters: file_key, node_id, token"

(** figma_get_image_fills 핸들러 *)
let handle_get_image_fills args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let version = get_string "version" args in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_images ~token ~file_key ?version () with
       | Ok json ->
           let images =
             match member "images" json with
             | Some (`Assoc _ as m) -> m
             | _ -> `Null
           in
           let downloads =
             if download then
               match images with
               | `Assoc items ->
                   `List (List.map (download_image_fill save_dir file_key) items)
               | _ -> `List []
             else
               `List []
           in
           let result = `Assoc [
             ("images", images);
             ("downloads", downloads);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string result))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_nodes 핸들러 *)
let handle_get_nodes args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_ids_str = get_string "node_ids" args in
  let token = resolve_token args in
  let format = get_string_or "format" "raw" args in
  let depth = get_int "depth" args in
  let geometry = get_string "geometry" args in
  let plugin_data = get_string "plugin_data" args in
  let version = get_string "version" args in

  match (file_key, node_ids_str, token) with
  | (Some file_key, Some node_ids_str, Some token) ->
      let node_ids =
        node_ids_str
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
        |> List.map normalize_node_id
      in
      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids ?depth ?geometry ?plugin_data ?version () with
       | Error err -> Error err
       | Ok json ->
           if format = "raw" then
             Ok (make_text_content (Yojson.Safe.pretty_to_string json))
           else
             let nodes = match member "nodes" json with
               | Some (`Assoc nodes_map) -> nodes_map
               | _ -> []
             in
             let converted =
               List.map (fun (id, node_json) ->
                 let doc = match member "document" node_json with
                   | Some d -> d
                   | None -> `Null
                 in
                 let dsl =
                   match process_json_string ~format (Yojson.Safe.to_string doc) with
                   | Ok s -> s
                   | Error msg -> "Error: " ^ msg
                 in
                 `Assoc [
                   ("node_id", `String id);
                   ("dsl", `String dsl);
                   ("node_raw", doc);
                 ]) nodes
             in
             let result = `Assoc [("nodes", `List converted)] in
             Ok (make_text_content (Yojson.Safe.pretty_to_string result)))
  | _ -> Error "Missing required parameters: file_key, node_ids, token"

(** figma_get_file_versions 핸들러 *)
let handle_get_file_versions args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_versions ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_file_comments 핸들러 *)
let handle_get_file_comments args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_comments ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_post_comment 핸들러 *)
let handle_post_comment args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  let message = get_string "message" args in
  let x = get_float "x" args in
  let y = get_float "y" args in
  let node_id = get_string "node_id" args in

  match (file_key, token, message, x, y) with
  | (Some file_key, Some token, Some message, Some x, Some y) ->
      let client_meta =
        `Assoc (
          ("x", `Float x) ::
          ("y", `Float y) ::
          (match node_id with Some id -> [("node_id", `String id)] | None -> [])
        )
      in
      (match Figma_effects.Perform.post_file_comment ~token ~file_key ~message ~client_meta with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token, message, x, y"

(** figma_get_file_components 핸들러 *)
let handle_get_file_components args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_components ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_team_components 핸들러 *)
let handle_get_team_components args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = resolve_token args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_components ~token ~team_id with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_get_file_component_sets 핸들러 *)
let handle_get_file_component_sets args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_component_sets ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_team_component_sets 핸들러 *)
let handle_get_team_component_sets args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = resolve_token args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_component_sets ~token ~team_id with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_get_file_styles 핸들러 *)
let handle_get_file_styles args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_styles ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_team_styles 핸들러 *)
let handle_get_team_styles args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = resolve_token args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_styles ~token ~team_id with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_get_component 핸들러 *)
let handle_get_component args : (Yojson.Safe.t, string) result =
  let component_key = get_string "component_key" args in
  let token = resolve_token args in

  match (component_key, token) with
  | (Some component_key, Some token) ->
      (match Figma_effects.Perform.get_component ~token ~component_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: component_key, token"

(** figma_get_component_set 핸들러 *)
let handle_get_component_set args : (Yojson.Safe.t, string) result =
  let component_set_key = get_string "component_set_key" args in
  let token = resolve_token args in

  match (component_set_key, token) with
  | (Some component_set_key, Some token) ->
      (match Figma_effects.Perform.get_component_set ~token ~component_set_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: component_set_key, token"

(** figma_get_dev_resources 핸들러 — planned for v0.8 *)
let handle_get_dev_resources args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  let token = resolve_token args in
  match (file_key, node_id, token) with
  | (Some _file_key, Some _node_id, Some _token) ->
      Error "dev_resources: planned for v0.8 (requires Figma REST API v2 dev_resources endpoint)"
  | _ -> Error "Missing required parameters: file_key, node_id, token"
[@@coverage off]

(** figma_add_dev_resource 핸들러 — planned for v0.8 *)
let handle_add_dev_resource args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  let name = get_string "name" args in
  let url = get_string "url" args in
  let token = resolve_token args in
  match (file_key, node_id, name, url, token) with
  | (Some _file_key, Some _node_id, Some _name, Some _url, Some _token) ->
      Error "add_dev_resource: planned for v0.8 (requires Figma REST API v2 dev_resources endpoint)"
  | _ -> Error "Missing required parameters: file_key, node_id, name, url, token"
[@@coverage off]

(** figma_setup_webhook 핸들러 — planned for v0.8 *)
let handle_setup_webhook args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let file_key = get_string "file_key" args in
  let endpoint = get_string_or "endpoint" "https://mcp.your-domain.com/webhook/figma" args in
  let passcode = get_string_or "passcode" "secret-passcode" args in
  let token = resolve_token args in
  match (team_id, file_key, token) with
  | (Some _team_id, Some _file_key, Some _token) ->
      ignore (endpoint, passcode);
      Error "setup_webhook: planned for v0.8 (requires Figma Webhooks v2 API)"
  | _ -> Error "Missing required parameters: team_id, file_key, token"
[@@coverage off]

