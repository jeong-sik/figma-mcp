(** Bundle/progressive loading handlers for Figma REST API nodes. *)

open Mcp_helpers
open Mcp_plugin_handlers
open Mcp_api_handler_support
open Printf

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
      let cache_options =
        List.filter_map Fun.id [
          Option.map (sprintf "depth:%d") depth;
          Option.map (sprintf "geometry:%s") geometry;
          Option.map (sprintf "plugin_data:%s") plugin_data;
          Option.map (sprintf "version:%s") version;
        ]
      in
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
                    ("image", `Assoc [("url", `String image_url); ("download", image_download)]);
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
                let result_str = Yojson.Safe.pretty_to_string result in
                let prefix = Printf.sprintf "node_%s" (sanitize_node_id node_id) in
                Ok (Large_response.wrap_string_result ~prefix ~format result_str)))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

let handle_get_node_summary args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let max_children = get_int_positive "max_children" 50 args in
  let version = get_string "version" args in
  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let node_id = normalize_node_id node_id in
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
                        Some (`Assoc [
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
                Ok (`Assoc [
                  ("node_id", `String node_id);
                  ("name", `String node_name);
                  ("type", `String node_type);
                  ("children_count", `Int children_count);
                  ("children", `List children_summary);
                  ("truncated", `Bool (children_count > max_children));
                  ("hint", `String "Use figma_get_node_chunk for progressive loading of specific depth ranges");
                ])))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

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
                         with _ -> 0
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
                       let get_children_safe json =
                         match json |> member "children" with
                         | `Null -> []
                         | `List lst -> lst
                         | _ -> []
                       in
                       let rec filter_by_depth current_depth json =
                         if current_depth < depth_start then
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
                           let assoc = to_assoc json in
                           let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                           let children_count = get_children_safe json |> List.length in
                           `Assoc (without_children @ [("_truncated_children", `Int children_count)])
                         else
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
                           let result_str = Yojson.Safe.pretty_to_string result in
                           let prefix = Printf.sprintf "chunk_%s_%d_%d" (sanitize_node_id node_id) depth_start depth_end in
                           Ok (Large_response.wrap_string_result ~prefix ~format result_str))))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"
