(** Figma REST API handlers — core file, node, and component operations.

    Leaf module — all functions consume tool args JSON and return result JSON. *)

(** {1 Types} *)

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

(** {1 Pure helpers (exposed for testing)} *)

val default_exclude_patterns : string list
val default_note_patterns : string list
val normalize_patterns : string list -> string list
val string_contains : haystack:string -> needle:string -> bool
val matches_any : string list -> string -> bool
val find_matching_pattern : string list -> string -> string option
val node_text_blob : Figma_types.ui_node -> string
val node_is_text : Figma_types.ui_node -> bool
val node_is_container : Figma_types.ui_node -> bool
val node_is_component : Figma_types.ui_node -> bool
val node_has_image_fill : Figma_types.ui_node -> bool
val node_area : Figma_types.ui_node -> float
val node_area_score : float -> float
val node_has_auto_layout : Figma_types.ui_node -> bool
val node_has_mask_hint : Figma_types.ui_node -> bool
val node_duplicate_key : Figma_types.ui_node -> string

(** {1 Handlers} *)

val handle_get_file : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_file_meta : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_list_screens : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_node : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_node_bundle : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_node_summary : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_select_nodes : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_node_chunk : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_export_image : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_export_smart : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_image_fills : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_nodes : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_file_versions : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_file_comments : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_post_comment : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_file_components : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_team_components : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_file_component_sets : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_team_component_sets : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_file_styles : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_team_styles : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_component : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_component_set : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_get_dev_resources : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_add_dev_resource : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_setup_webhook : Yojson.Safe.t -> (Yojson.Safe.t, string) result
