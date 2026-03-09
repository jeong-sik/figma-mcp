(** Figma MCP API handlers — facade over core and domain-specific handler groups. *)

open Mcp_helpers
open Printf

type selection_config = Mcp_api_handler_support.selection_config = {
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

let default_exclude_patterns = Mcp_api_handler_support.default_exclude_patterns
let default_note_patterns = Mcp_api_handler_support.default_note_patterns
let normalize_patterns = Mcp_api_handler_support.normalize_patterns
let string_contains = Mcp_api_handler_support.string_contains
let matches_any = Mcp_api_handler_support.matches_any
let find_matching_pattern = Mcp_api_handler_support.find_matching_pattern
let node_text_blob = Mcp_api_handler_support.node_text_blob
let node_is_text = Mcp_api_handler_support.node_is_text
let node_is_container = Mcp_api_handler_support.node_is_container
let node_is_component = Mcp_api_handler_support.node_is_component
let node_has_image_fill = Mcp_api_handler_support.node_has_image_fill
let node_area = Mcp_api_handler_support.node_area
let node_area_score = Mcp_api_handler_support.node_area_score
let node_has_auto_layout = Mcp_api_handler_support.node_has_auto_layout
let node_has_mask_hint = Mcp_api_handler_support.node_has_mask_hint
let node_duplicate_key = Mcp_api_handler_support.node_duplicate_key

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

let handle_list_screens args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           let screens = Figma_api.get_all_screens json in
           let screen_list = List.map (fun (id, name) -> sprintf "- %s (%s)" name id) screens in
           let result = sprintf "Found %d screens:\n%s"
             (List.length screens)
             (String.concat "\n" screen_list)
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

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

let handle_get_node_bundle = Mcp_api_bundle_handlers.handle_get_node_bundle
let handle_get_node_summary = Mcp_api_bundle_handlers.handle_get_node_summary
let handle_select_nodes = Mcp_api_selection_handlers.handle_select_nodes
let handle_get_node_chunk = Mcp_api_bundle_handlers.handle_get_node_chunk
let handle_export_image = Mcp_api_export_handlers.handle_export_image
let handle_export_smart = Mcp_api_export_handlers.handle_export_smart
let handle_get_image_fills = Mcp_api_export_handlers.handle_get_image_fills

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

let handle_get_file_versions = Mcp_api_resource_handlers.handle_get_file_versions
let handle_get_file_comments = Mcp_api_resource_handlers.handle_get_file_comments
let handle_post_comment = Mcp_api_resource_handlers.handle_post_comment
let handle_get_file_components = Mcp_api_resource_handlers.handle_get_file_components
let handle_get_team_components = Mcp_api_resource_handlers.handle_get_team_components
let handle_get_file_component_sets = Mcp_api_resource_handlers.handle_get_file_component_sets
let handle_get_team_component_sets = Mcp_api_resource_handlers.handle_get_team_component_sets
let handle_get_file_styles = Mcp_api_resource_handlers.handle_get_file_styles
let handle_get_team_styles = Mcp_api_resource_handlers.handle_get_team_styles
let handle_get_component = Mcp_api_resource_handlers.handle_get_component
let handle_get_component_set = Mcp_api_resource_handlers.handle_get_component_set
let handle_get_dev_resources = Mcp_api_resource_handlers.handle_get_dev_resources
let handle_add_dev_resource = Mcp_api_resource_handlers.handle_add_dev_resource
let handle_setup_webhook = Mcp_api_resource_handlers.handle_setup_webhook
