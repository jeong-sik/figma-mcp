open Mcp_helpers

let max_summary_depth = 6

let clamp_summary_depth raw default =
  let value = match raw with Some depth -> depth | None -> default in
  if value < 0 then 1 else min value max_summary_depth

let clamp_preview_scale scale =
  if scale < 0.01 then 0.01
  else if scale > 4.0 then 4.0
  else scale

let fetch_preview_json ~token ~file_key ~node_id ~preview_format ~preview_scale ?version () =
  match Figma_effects.Perform.get_images
          ~token ~file_key ~node_ids:[node_id]
          ~format:preview_format ~scale:preview_scale ?version () with
  | Error err ->
      `Assoc [("status", `String "error"); ("error", `String err)]
  | Ok json ->
      (match member "images" json with
       | Some (`Assoc image_map) ->
           (match List.assoc_opt node_id image_map with
            | Some (`String url) ->
                `Assoc [
                  ("status", `String "ok");
                  ("url", `String url);
                  ("format", `String preview_format);
                  ("scale", `Float preview_scale);
                ]
            | _ ->
                `Assoc [
                  ("status", `String "missing");
                  ("format", `String preview_format);
                  ("scale", `Float preview_scale);
                ])
       | _ ->
           `Assoc [
             ("status", `String "missing");
             ("format", `String preview_format);
             ("scale", `Float preview_scale);
           ])

let fetch_root_node ~token ~file_key ~node_id ~summary_depth ?version () =
  match
    Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id]
      ~depth:summary_depth ?version ()
  with
  | Error err ->
      Error (Printf.sprintf "Figma API error: %s" err)
  | Ok nodes_json ->
      (match member "nodes" nodes_json with
       | Some (`Assoc node_map) ->
           (match find_node_entry node_map ~node_id with
            | Some (_, (`Assoc fields as node_entry)) ->
                (match member "document" node_entry with
                 | Some node_data ->
                     (match Figma_parser.parse_node ~max_depth:summary_depth node_data with
                      | Some root -> Ok root
                      | None -> Error "Failed to parse node JSON")
                 | None ->
                     Error (Printf.sprintf "Document not found for node %s" node_id))
            | Some (_, _other) ->
                Error (Printf.sprintf "Document not found for node %s" node_id)
            | None ->
                Error (Printf.sprintf "Node %s not found in file %s" node_id file_key))
       | _ -> Error "Invalid Figma API response: missing nodes map")

let handle_get_planning_context args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let summary_depth = clamp_summary_depth (get_int "summary_depth" args) 2 in
  let preview = get_bool_or "preview" true args in
  let preview_format = get_string_or "preview_format" "png" args in
  let preview_scale = clamp_preview_scale (get_float_or "preview_scale" 1.0 args) in
  let note_patterns =
    get_string_list "note_patterns" args |> Option.value ~default:[]
  in
  let exclude_patterns =
    get_string_list "exclude_patterns" args |> Option.value ~default:[]
  in
  let version = get_string "version" args in
  match (file_key, node_id, token) with
  | Some file_key, Some node_id, Some token ->
      let node_id = normalize_node_id node_id in
      fetch_root_node ~token ~file_key ~node_id ~summary_depth ?version ()
      >>= fun root ->
      let preview_json =
        if preview then
          fetch_preview_json ~token ~file_key ~node_id ~preview_format ~preview_scale
            ?version ()
        else
          `Null
      in
      let context_json =
        Figma_planning_context.build_context_json
          ~note_patterns
          ~exclude_patterns
          ~file_key
          ~node_id
          ~summary_depth
          ~preview_json
          root
      in
      Ok (make_text_content (Yojson.Safe.pretty_to_string context_json))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

let resolve_plan args =
  match get_json "plan" args with
  | Some plan -> Ok plan
  | None -> Error "Missing required parameter: plan"

let handle_validate_agent_plan args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let summary_depth = clamp_summary_depth (get_int "summary_depth" args) max_summary_depth in
  let version = get_string "version" args in
  match (file_key, node_id, token) with
  | Some file_key, Some node_id, Some token ->
      let node_id = normalize_node_id node_id in
      resolve_plan args >>= fun plan ->
      fetch_root_node ~token ~file_key ~node_id ~summary_depth ?version ()
      >>= fun root ->
      let validation_json =
        Figma_agent_plan_validator.validate_json ~root_node_id:node_id root plan
      in
      Ok (make_text_content (Yojson.Safe.pretty_to_string validation_json))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"
