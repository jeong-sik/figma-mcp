(** Plugin handler infrastructure and all handle_plugin_* handlers.

    Provides plugin communication primitives and handler combinator functions
    used by mcp_visual_handlers and mcp_api_handlers. *)

(** {1 Pure helpers (exposed for testing)} *)

val truncate_string : ?max_len:int -> string -> string
val plugin_error_message : Yojson.Safe.t -> string

(** {1 Plugin infrastructure} *)

val resolve_channel_id : Yojson.Safe.t -> (string, string) result

val plugin_wait :
  channel_id:string ->
  command_id:string ->
  timeout_ms:int ->
  (Figma_plugin_bridge.result, string) result

val plugin_exec :
  channel_id:string ->
  name:string ->
  payload:Yojson.Safe.t ->
  timeout_ms:int ->
  (Yojson.Safe.t, string) result

(** {1 Handler combinators} *)

val plugin_simple :
  name:string ->
  ?default_timeout:int ->
  build_payload:(Yojson.Safe.t -> Yojson.Safe.t) ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, string) result

val plugin_node :
  name:string ->
  ?default_timeout:int ->
  build_payload:(string -> Yojson.Safe.t -> Yojson.Safe.t) ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, string) result

val plugin_nodes :
  name:string ->
  ?default_timeout:int ->
  build_payload:(string list -> Yojson.Safe.t -> Yojson.Safe.t) ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, string) result

val plugin_custom :
  name:string ->
  ?default_timeout:int ->
  validate:(Yojson.Safe.t -> ('a, string) result) ->
  build_payload:('a -> Yojson.Safe.t -> Yojson.Safe.t) ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, string) result

(** {1 Plugin handlers} *)

val handle_plugin_connect : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_use_channel : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_status : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_read_selection : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_get_node : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_export_node_image : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_annotate : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_get_variables : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_apply_ops : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_edit_node : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_create_node : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_delete_nodes : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_batch : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_plugin_subscribe_events : Yojson.Safe.t -> (Yojson.Safe.t, string) result

(** {1 Plugin Enhancement Tools (Phase A4)} *)

val handle_export_tokens_plugin : Yojson.Safe.t -> (Yojson.Safe.t, string) result

(** {1 Unified plugin dispatch} *)

val known_plugin_actions : string list
val suggest_action : string -> string
val handle_figma_plugin : Yojson.Safe.t -> (Yojson.Safe.t, string) result
