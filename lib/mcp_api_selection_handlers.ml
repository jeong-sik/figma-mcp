(** Selection/scoring handlers for Figma REST API nodes. *)

open Mcp_helpers
open Mcp_api_handler_support

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
              `Assoc [("status", `String "error"); ("error", `String err)]
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
