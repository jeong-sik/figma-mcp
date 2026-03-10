open Figma_types
open Mcp_api_handler_support

type flat_node = {
  node: ui_node;
  parent_node_id: string option;
  depth: int;
}

let layout_mode_to_string = function
  | None' -> "none"
  | Horizontal -> "horizontal"
  | Vertical -> "vertical"

let bbox_to_json = function
  | None ->
      `Null
  | Some bbox ->
      `Assoc [
        ("x", `Float bbox.x);
        ("y", `Float bbox.y);
        ("width", `Float bbox.width);
        ("height", `Float bbox.height);
      ]

let text_excerpt node =
  match node.characters with
  | None -> None
  | Some text ->
      let trimmed = String.trim text in
      if trimmed = "" then None
      else
        let limit = 120 in
        if String.length trimmed > limit then
          Some (String.sub trimmed 0 limit ^ "...")
        else
          Some trimmed

let rec flatten_tree ?(parent_node_id = None) ?(depth = 0) (node : ui_node) =
  let current = { node; parent_node_id; depth } in
  let children =
    node.children
    |> List.map (flatten_tree ~parent_node_id:(Some node.id) ~depth:(depth + 1))
    |> List.flatten
  in
  current :: children

let feature_flags_json (node : ui_node) =
  `Assoc [
    ("text_present", `Bool (node.characters <> None));
    ("component_like", `Bool (node_is_component node));
    ("mask_like", `Bool (node_has_mask_hint node));
    ("auto_layout", `Bool (node_has_auto_layout node));
    ("image_fill", `Bool (node_has_image_fill node));
  ]

let candidate_json (flat : flat_node) =
  let node = flat.node in
  let fields =
    [
      ("id", `String node.id);
      ("name", `String node.name);
      ("type", `String (Figma_query.node_type_to_string node.node_type));
      ("parent_node_id",
       match flat.parent_node_id with
       | Some id -> `String id
       | None -> `Null);
      ("depth", `Int flat.depth);
      ("child_count", `Int (List.length node.children));
      ("visible", `Bool node.visible);
      ("opacity", `Float node.opacity);
      ("bbox", bbox_to_json node.bbox);
      ("layout_mode", `String (layout_mode_to_string node.layout_mode));
      ("fills_count", `Int (List.length node.fills));
      ("effects_count", `Int (List.length node.effects));
      ("feature_flags", feature_flags_json node);
    ]
  in
  let fields =
    match text_excerpt node with
    | None -> fields
    | Some excerpt -> ("text_excerpt", `String excerpt) :: fields
  in
  `Assoc (List.rev fields)

let edge_json (flat : flat_node) =
  match flat.parent_node_id with
  | None -> None
  | Some parent_node_id ->
      Some
        (`Assoc [
          ("parent_node_id", `String parent_node_id);
          ("child_node_id", `String flat.node.id);
          ("depth", `Int flat.depth);
        ])

let note_json ~pattern (flat : flat_node) =
  let node = flat.node in
  `Assoc [
    ("id", `String node.id);
    ("name", `String node.name);
    ("type", `String (Figma_query.node_type_to_string node.node_type));
    ("pattern", `String pattern);
    ("depth", `Int flat.depth);
    ("parent_node_id",
     match flat.parent_node_id with
     | Some id -> `String id
     | None -> `Null);
    ("text", `String (Option.value ~default:"" node.characters));
  ]

let excluded_json ~reason (flat : flat_node) =
  let node = flat.node in
  `Assoc [
    ("id", `String node.id);
    ("name", `String node.name);
    ("type", `String (Figma_query.node_type_to_string node.node_type));
    ("reason", `String reason);
    ("depth", `Int flat.depth);
  ]

let aggregate_type_counts flats =
  let counts = Hashtbl.create 16 in
  List.iter
    (fun (flat : flat_node) ->
      let key = Figma_query.node_type_to_string flat.node.node_type in
      let next = Option.value ~default:0 (Hashtbl.find_opt counts key) + 1 in
      Hashtbl.replace counts key next)
    flats;
  counts
  |> Hashtbl.to_seq
  |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.map (fun (name, count) -> (name, `Int count))

let feature_summary_json ~all_nodes ~candidate_count ~notes_count ~excluded_count =
  let text_nodes = ref 0 in
  let component_nodes = ref 0 in
  let auto_layout_nodes = ref 0 in
  let image_fill_nodes = ref 0 in
  let invisible_nodes = ref 0 in
  let max_depth = ref 0 in
  List.iter
    (fun (flat : flat_node) ->
      if flat.depth > !max_depth then max_depth := flat.depth;
      if node_is_text flat.node then incr text_nodes;
      if node_is_component flat.node then incr component_nodes;
      if node_has_auto_layout flat.node then incr auto_layout_nodes;
      if node_has_image_fill flat.node then incr image_fill_nodes;
      if (not flat.node.visible) || flat.node.opacity <= 0.01 then incr invisible_nodes)
    all_nodes;
  `Assoc [
    ("total_nodes", `Int (List.length all_nodes));
    ("candidate_count", `Int candidate_count);
    ("notes_count", `Int notes_count);
    ("excluded_count", `Int excluded_count);
    ("top_level_count",
     `Int (List.filter (fun (flat : flat_node) -> flat.depth = 1) all_nodes |> List.length));
    ("max_depth", `Int !max_depth);
    ("text_nodes", `Int !text_nodes);
    ("component_nodes", `Int !component_nodes);
    ("auto_layout_nodes", `Int !auto_layout_nodes);
    ("image_fill_nodes", `Int !image_fill_nodes);
    ("invisible_nodes", `Int !invisible_nodes);
    ("type_counts", `Assoc (aggregate_type_counts all_nodes));
  ]

let root_summary_json (root : ui_node) =
  `Assoc [
    ("id", `String root.id);
    ("name", `String root.name);
    ("type", `String (Figma_query.node_type_to_string root.node_type));
    ("bbox", bbox_to_json root.bbox);
    ("layout_mode", `String (layout_mode_to_string root.layout_mode));
    ("child_count", `Int (List.length root.children));
  ]

let bundle_refs_json ~file_key ~node_id ~summary_depth =
  let shared_args =
    `Assoc [
      ("file_key", `String file_key);
      ("node_id", `String node_id);
    ]
  in
  let chunk_args =
    `Assoc [
      ("file_key", `String file_key);
      ("node_id", `String node_id);
      ("depth_start", `Int 0);
      ("depth_end", `Int summary_depth);
    ]
  in
  `List [
    `Assoc [
      ("kind", `String "node_bundle");
      ("tool", `String "figma_get_node_bundle");
      ("args", shared_args);
    ];
    `Assoc [
      ("kind", `String "node_chunk");
      ("tool", `String "figma_get_node_chunk");
      ("args", chunk_args);
    ];
  ]

let build_context_json
    ?(note_patterns = [])
    ?(exclude_patterns = [])
    ~file_key
    ~node_id
    ~summary_depth
    ~preview_json
    (root : ui_node) =
  let flattened = flatten_tree root in
  let descendants = List.filter (fun (flat : flat_node) -> flat.depth > 0) flattened in
  let notes = ref [] in
  let excluded = ref [] in
  let candidates =
    descendants
    |> List.filter_map (fun (flat : flat_node) ->
         let name_blob = node_text_blob flat.node in
         let note_pattern =
           if note_patterns = [] || not (node_is_text flat.node) then None
           else find_matching_pattern note_patterns name_blob
         in
         let exclude_pattern =
           if exclude_patterns = [] then None
           else find_matching_pattern exclude_patterns name_blob
         in
         (match note_pattern with
          | Some pattern -> notes := note_json ~pattern flat :: !notes
          | None -> ());
         (match exclude_pattern with
          | Some pattern ->
              excluded := excluded_json ~reason:("pattern:" ^ pattern) flat :: !excluded;
              None
          | None ->
              if note_pattern <> None then None else Some (candidate_json flat)))
  in
  `Assoc [
    ("file_key", `String file_key);
    ("node_id", `String node_id);
    ("summary_depth", `Int summary_depth);
    ("root_summary", root_summary_json root);
    ("candidates", `List candidates);
    ("candidate_count", `Int (List.length candidates));
    ("tree_edges", `List (List.filter_map edge_json descendants));
    ("notes", `List (List.rev !notes));
    ("excluded", `List (List.rev !excluded));
    ("preview", preview_json);
    ("bundle_refs", bundle_refs_json ~file_key ~node_id ~summary_depth);
    ("feature_summary",
     feature_summary_json
       ~all_nodes:flattened
       ~candidate_count:(List.length candidates)
       ~notes_count:(List.length !notes)
       ~excluded_count:(List.length !excluded));
  ]
