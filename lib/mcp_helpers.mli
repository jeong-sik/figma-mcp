(** Figma MCP shared helpers — JSON, Schema, Cache, Error, Parameter extraction.

    Foundation module used by all other mcp_* modules. *)

(** {1 JSON → DSL conversion} *)

val process_json_string : format:string -> string -> (string, string) result

(** {1 JSON Schema helpers} *)

val string_prop : string -> Yojson.Safe.t
val number_prop : string -> Yojson.Safe.t
val bool_prop : string -> Yojson.Safe.t
val enum_prop : string list -> string -> Yojson.Safe.t
val array_prop : ?items_type:string -> string -> Yojson.Safe.t
val object_prop : string -> Yojson.Safe.t
val object_schema : (string * Yojson.Safe.t) list -> string list -> Yojson.Safe.t

(** {1 Cache helpers} *)

val fetch_variables_cached :
  file_key:string ->
  token:string ->
  (Yojson.Safe.t * [> `String of string ], string) result
val fetch_file_for_search_cached :
  file_key:string ->
  token:string ->
  (Yojson.Safe.t * [> `String of string ], string) result

(** {1 Error types and handling} *)

type api_error =
  | NetworkError of string
  | AuthError of string
  | NotFound of string
  | RateLimited of float
  | ServerError of string
  | ParseError of string
  | TimeoutError of float
  | UnknownError of string

val error_to_string : api_error -> string
val classify_http_error : status_code:int -> body:string -> api_error

(** Result monadic operators *)

val ( >>= ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val ( >>| ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

(** {1 Temporary files and error JSON} *)

val with_temp_file : prefix:string -> suffix:string -> (string -> 'a) -> 'a
val make_error_json :
  operation:string ->
  error:api_error ->
  ?debug_info:(string * Yojson.Safe.t) list ->
  unit ->
  Yojson.Safe.t

(** {1 Handler registry} *)

val handler_registry :
  (string, Yojson.Safe.t -> (Yojson.Safe.t, string) result) Hashtbl.t
val register_handler :
  string -> (Yojson.Safe.t -> (Yojson.Safe.t, string) result) -> unit
val call_handler : string -> Yojson.Safe.t -> (Yojson.Safe.t, string) result

(** {1 Parameter extraction} *)

val get_string : string -> Yojson.Safe.t -> string option
val get_string_list : string -> Yojson.Safe.t -> string list option
val get_json : string -> Yojson.Safe.t -> Yojson.Safe.t option
val get_bool : string -> Yojson.Safe.t -> bool option
val get_int : string -> Yojson.Safe.t -> int option
val get_float : string -> Yojson.Safe.t -> float option

val get_string_or : string -> string -> Yojson.Safe.t -> string
val get_int_or : string -> int -> Yojson.Safe.t -> int
val get_int_positive : ?min:int -> string -> int -> Yojson.Safe.t -> int
val get_int_nonneg : ?min:int -> string -> int -> Yojson.Safe.t -> int
val get_float_or : string -> float -> Yojson.Safe.t -> float
val get_bool_or : string -> bool -> Yojson.Safe.t -> bool

(** {1 Option helpers} *)

val prefer_some : 'a option -> 'a option -> 'a option

(** {1 Token and URL resolution} *)

val resolve_token : Yojson.Safe.t -> string option
val resolve_url_info : Yojson.Safe.t -> Figma_api.figma_url_info option
val resolve_file_key_node_id :
  Yojson.Safe.t -> string option * string option
val resolve_node_id : Yojson.Safe.t -> string option

(** {1 JSON access helpers} *)

val member : string -> Yojson.Safe.t -> Yojson.Safe.t option
val normalize_node_id : string -> string
val normalize_node_id_key : string -> string -> string
val find_node_entry :
  (string * Yojson.Safe.t) list ->
  node_id:string ->
  (string * Yojson.Safe.t) option

(** {1 Content builders} *)

val make_text_content : string -> Yojson.Safe.t
val make_error_content : string -> Yojson.Safe.t
val build_file_meta : Yojson.Safe.t -> Yojson.Safe.t

(** {1 Eio context} *)

type any_clock = Clock : _ Eio.Time.clock -> any_clock
type any_net = Net : _ Eio.Net.t -> any_net

type eio_context = {
  sw: Eio.Switch.t;
  net: any_net;
  clock: any_clock;
  client: Figma_api_eio.client;
  domain: Domain.id;
}

val get_eio_context : unit -> eio_context option
val install_eio_context : eio_context -> unit
val set_eio_context :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  client:Figma_api_eio.client ->
  eio_context

(** {1 Fidelity scoring} *)

val fidelity_sections : (string * string * float) list
val fidelity_score_of_dsl :
  Yojson.Safe.t -> float * int * Yojson.Safe.t
val fidelity_score_with_overrides :
  Yojson.Safe.t ->
  (string * (float * int * int * int)) list ->
  float * int * Yojson.Safe.t
val fidelity_score_of_bundle :
  dsl_json:Yojson.Safe.t ->
  variables:Yojson.Safe.t ->
  image_fills:Yojson.Safe.t ->
  plugin_snapshot:Yojson.Safe.t ->
  include_variables:bool ->
  include_image_fills:bool ->
  include_plugin:bool ->
  float * int * Yojson.Safe.t

val override_section :
  ?score:float ->
  present:int ->
  missing:int ->
  total:int ->
  unit ->
  float * int * int * int

(** {1 Filesystem and sanitization} *)

val default_asset_dir : unit -> string
val default_compare_dir : unit -> string
val sanitize_node_id : string -> string
val sanitize_file_key : string -> string
val is_http_url : string -> bool
val strip_query : string -> string
val file_ext_from_url : string -> string
val mkdir_p : string -> unit
val hyphenate_node_id : string -> string

(** {1 DSL analysis} *)

val count_text_segments : Yojson.Safe.t -> int

(** {1 Variables resolution} *)

val resolve_variables : Yojson.Safe.t -> Yojson.Safe.t
val resolve_plugin_variables : Yojson.Safe.t -> Yojson.Safe.t
val plugin_payload_if_ok : Yojson.Safe.t -> Yojson.Safe.t option
