(** Figma MCP shared helpers — JSON, Schema, Cache, Error, Parameter extraction *)

(** ============== String search ============== *)

let string_contains ~haystack ~needle =
  let needle = String.lowercase_ascii (String.trim needle) in
  if needle = "" then false
  else
    let haystack = String.lowercase_ascii haystack in
    try
      ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
      true
    with Not_found -> false

let matches_any patterns text =
  List.exists (fun p -> string_contains ~needle:p ~haystack:text) patterns

let find_matching_pattern patterns text =
  List.find_opt (fun p -> string_contains ~needle:p ~haystack:text) patterns

(** ============== JSON → DSL 변환 (Figma_mcp 순환 의존 방지) ============== *)
let process_json_string ~format json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    Ok (match format with
        | "fidelity" | "pixel" | "accuracy" -> Figma_codegen.generate_fidelity json
        | "raw" -> Yojson.Safe.pretty_to_string json
        | "html" -> (
            let node_json =
              match Figma_api.extract_document json with
              | Some d -> d
              | None -> json
            in
            match Figma_parser.parse_json node_json with
            | None -> "Failed to parse JSON for HTML output"
            | Some node -> Figma_codegen.generate_html node
          )
        | _ -> "Unknown format (use fidelity, raw, or html)")
  with
  | Yojson.Json_error _ -> Error "Failed to parse JSON"

(** ============== JSON Schema 헬퍼 ============== *)

(** Note: required parameter is not used - JSON Schema required is at object level *)
let string_prop desc : Yojson.Safe.t =
  `Assoc [("type", `String "string"); ("description", `String desc)]

let number_prop desc : Yojson.Safe.t =
  `Assoc [("type", `String "number"); ("description", `String desc)]

let bool_prop desc : Yojson.Safe.t =
  `Assoc [("type", `String "boolean"); ("description", `String desc)]

let enum_prop options desc : Yojson.Safe.t =
  `Assoc [
    ("type", `String "string");
    ("enum", `List (List.map (fun s -> `String s) options));
    ("description", `String desc);
  ]

let array_prop ?(items_type="string") desc : Yojson.Safe.t =
  `Assoc [
    ("type", `String "array");
    ("items", `Assoc [("type", `String items_type)]);
    ("description", `String desc);
  ]

let object_prop desc : Yojson.Safe.t =
  `Assoc [
    ("type", `String "object");
    ("description", `String desc);
  ]

let object_schema props required : Yojson.Safe.t =
  `Assoc [
    ("type", `String "object");
    ("properties", `Assoc props);
    ("required", `List (List.map (fun s -> `String s) required));
  ]

(** ============== 캐시 헬퍼 ============== *)

let variables_cache_node_id = "__variables__"
let search_cache_node_id_prefix = "__search__"

let fetch_variables_cached ~file_key ~token =
  let cached_json =
    Figma_cache.get ~file_key ~node_id:variables_cache_node_id
      ~ttl_hours:Figma_cache.Config.ttl_variables_hours ()
  in
  match cached_json with
  | Some json -> Ok (json, `String "cache")
  | None ->
      (match Figma_effects.Perform.get_variables ~token ~file_key with
       | Ok json ->
           Figma_cache.set ~file_key ~node_id:variables_cache_node_id json;
           Ok (json, `String "rest")
       | Error err -> Error err)

let fetch_file_for_search_cached ~file_key ~token =
  let node_id = search_cache_node_id_prefix in
  let cached_json =
    Figma_cache.get ~file_key ~node_id
      ~ttl_hours:Figma_cache.Config.ttl_search_hours ()
  in
  match cached_json with
  | Some json -> Ok (json, `String "cache")
  | None ->
      (match Figma_effects.Perform.get_file ~token ~file_key ~depth:4 () with
       | Ok json ->
           Figma_cache.set ~file_key ~node_id json;
           Ok (json, `String "rest")
       | Error err -> Error err)

(** ============== Tool 핸들러 구현 ============== *)

let member key json =
  match json with
  | `Assoc lst -> List.assoc_opt key lst
  | _ -> None

let normalize_node_id value =
  Figma_api.normalize_node_id value

let normalize_node_id_key key value =
  match key with
  | "node_id" | "node_a_id" | "node_b_id" -> normalize_node_id value
  | _ -> value

let hyphenate_node_id value =
  String.map (fun c -> if c = ':' then '-' else c) value

(* Figma API and clients sometimes disagree on ":" vs "-" node IDs.
   Resolve by normalizing both the requested ID and map keys. *)
let find_node_entry (nodes_map : (string * Yojson.Safe.t) list) ~(node_id : string)
  : (string * Yojson.Safe.t) option =
  let target = normalize_node_id node_id in
  let direct_keys = [node_id; target; hyphenate_node_id target] in
  let direct_hit =
    List.find_map (fun key ->
      match List.assoc_opt key nodes_map with
      | Some v -> Some (key, v)
      | None -> None
    ) direct_keys
  in
  match direct_hit with
  | Some hit -> Some hit
  | None ->
      List.find_map (fun (key, value) ->
        if normalize_node_id key = target then Some (key, value) else None
      ) nodes_map

(** ============== 에러 처리 강화 + 모나딕 바인딩 ============== *)

(** 상세 에러 타입 *)
type api_error =
  | NetworkError of string
  | AuthError of string       (* 401/403 *)
  | NotFound of string        (* 404 *)
  | RateLimited of float      (* 429 + retry_after *)
  | ServerError of string     (* 5xx *)
  | ParseError of string
  | TimeoutError of float     (* timeout in seconds *)
  | UnknownError of string

(** 에러를 사용자 친화적 메시지로 변환 *)
let error_to_string = function
  | NetworkError msg -> Printf.sprintf "🌐 Network error: %s" msg
  | AuthError msg -> Printf.sprintf "🔐 Auth error: %s (check FIGMA_TOKEN)" msg
  | NotFound msg -> Printf.sprintf "🔍 Not found: %s" msg
  | RateLimited secs -> Printf.sprintf "⏳ Rate limited - retry after %.0fs" secs
  | ServerError msg -> Printf.sprintf "🔥 Figma server error: %s" msg
  | ParseError msg -> Printf.sprintf "📄 Parse error: %s" msg
  | TimeoutError secs -> Printf.sprintf "⏱️ Timeout after %.0fs" secs
  | UnknownError msg -> Printf.sprintf "❓ Unknown error: %s" msg

(** HTTP 상태 코드에서 에러 분류 *)
let classify_http_error ~status_code ~body =
  match status_code with
  | 401 | 403 -> AuthError body
  | 404 -> NotFound body
  | 429 ->
      (* Rate limit - retry_after 파싱 시도 *)
      let retry_after = try
        Scanf.sscanf body "retry after %f" (fun f -> f)
      with Scanf.Scan_failure _ | Failure _ -> 60.0 in
      RateLimited retry_after
  | n when n >= 500 -> ServerError (Printf.sprintf "HTTP %d: %s" n body)
  | _ -> UnknownError (Printf.sprintf "HTTP %d: %s" status_code body)

(** Result 모나딕 바인딩 (let* 대용) *)
let ( >>= ) result f = match result with
  | Ok v -> f v
  | Error e -> Error e

let ( >>| ) result f = match result with
  | Ok v -> Ok (f v)
  | Error e -> Error e

(** 안전한 임시 파일 사용 (Common.protect 패턴) *)
let with_temp_file ~prefix ~suffix f =
  let path = Printf.sprintf "/tmp/figma-visual/%s_%d_%d%s"
    prefix
    (int_of_float (Unix.gettimeofday () *. 1000.0))
    (Random.int 100000)
    suffix
  in
  Common.protect
    ~module_name:"mcp_helpers"
    ~finally_label:"Unix.unlink"
    ~finally:(fun () -> Unix.unlink path)
    (fun () -> f path)

(** 디버그 정보가 포함된 에러 JSON 생성 *)
let make_error_json ~operation ~error ?(debug_info=[]) () =
  let timestamp = Unix.gettimeofday () in
  let base = [
    ("error", `Bool true);
    ("operation", `String operation);
    ("message", `String (error_to_string error));
    ("timestamp", `Float timestamp);
    ("timestamp_iso", `String (
      let tm = Unix.localtime timestamp in
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
        (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec));
  ] in
  let debug = if debug_info = [] then [] else [("debug", `Assoc debug_info)] in
  `Assoc (base @ debug)

(** ============== 핸들러 런타임 조회 (상호 참조 해결) ============== *)

(** 핸들러 레지스트리 - lazy initialization으로 forward reference 해결 *)
let handler_registry : (string, Yojson.Safe.t -> (Yojson.Safe.t, string) result) Hashtbl.t =
  Hashtbl.create 64

(** 핸들러 등록 (파일 끝에서 호출) *)
let register_handler name handler =
  Hashtbl.replace handler_registry name handler

(** 런타임에 다른 핸들러 호출 (forward reference 가능) *)
let call_handler name args =
  match Hashtbl.find_opt handler_registry name with
  | Some handler -> handler args
  | None -> Error (Printf.sprintf "Handler not found: %s" name)

(** ============== 기존 헬퍼 함수 ============== *)

let get_string key json =
  match member key json with
  | Some (`String s) -> Some (normalize_node_id_key key s)
  | _ -> None

let get_string_list key json =
  let trim = String.trim in
  match member key json with
  | Some (`List items) ->
      let values =
        items
        |> List.filter_map (function `String s -> Some (trim s) | _ -> None)
        |> List.filter (fun s -> s <> "")
      in
      if values = [] then None else Some values
  | Some (`String s) ->
      let values =
        s
        |> String.split_on_char ','
        |> List.map trim
        |> List.filter (fun v -> v <> "")
      in
      if values = [] then None else Some values
  | _ -> None

let prefer_some primary fallback =
  match primary with
  | Some _ -> primary
  | None -> fallback

let resolve_url_info args =
  match get_string "url" args with
  | Some url -> Some (Figma_api.parse_figma_url url)
  | None -> None

let resolve_file_key_node_id args =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  match resolve_url_info args with
  | None -> (file_key, node_id)
  | Some info ->
      let file_key = prefer_some file_key info.file_key in
      let node_id = prefer_some node_id info.node_id in
      (file_key, node_id)

let resolve_node_id args =
  match get_string "node_id" args with
  | Some _ as node_id -> node_id
  | None ->
      (match resolve_url_info args with
       | Some info -> info.node_id
       | None -> None)

let get_json key json =
  member key json

let get_bool key json =
  match member key json with
  | Some (`Bool b) -> Some b
  | _ -> None

let get_string_or key default json =
  match member key json with
  | Some (`String s) -> s
  | _ -> default

let get_float key json =
  match member key json with
  | Some (`Float f) -> Some f
  | Some (`Int i) -> Some (float_of_int i)
  | _ -> None

let get_int key json =
  match member key json with
  | Some (`Int i) -> Some i
  | Some (`Float f) -> Some (int_of_float f)
  | _ -> None

let get_list key json =
  match member key json with
  | Some (`List l) -> l
  | _ -> []

(** Get int with default value *)
let get_int_or key default json =
  match get_int key json with
  | Some i -> i
  | None -> default

(** Get int with default, requiring value > min (for positive constraints) *)
let get_int_positive ?(min=0) key default json =
  match get_int key json with
  | Some i when i > min -> i
  | _ -> default

(** Get int with default, requiring value >= min (for non-negative constraints) *)
let get_int_nonneg ?(min=0) key default json =
  match get_int key json with
  | Some i when i >= min -> i
  | _ -> default

let get_float_or key default json =
  match get_float key json with
  | Some f -> f
  | None -> default

let get_bool_or key default json =
  match get_bool key json with
  | Some b -> b
  | None -> default

(** Token resolution with environment variable expansion.
    Supports: "env:FIGMA_TOKEN" syntax to read from environment.
    Priority: 1) explicit "token" (with env: expansion) 2) FIGMA_TOKEN env var *)
let resolve_token args =
  (* Security hardening: if FIGMA_TOKEN is set on the server, do NOT allow
     request-provided tokens to override it. This avoids accidental/abusive
     secret injection via tool args (e.g., MCP clients or logs). *)
  match Sys.getenv_opt "FIGMA_TOKEN" with
  | Some t when String.length t > 0 -> Some t
  | _ ->
    match get_string "token" args with
    | Some t when String.length t > 0 ->
      (* Handle "env:VAR_NAME" syntax *)
      if String.length t > 4 && String.sub t 0 4 = "env:" then
        let var_name = String.sub t 4 (String.length t - 4) |> String.trim in
        (* Do NOT allow arbitrary env var reads from tool args.
           Only support the documented "env:FIGMA_TOKEN". *)
        if var_name = "FIGMA_TOKEN" then
          Sys.getenv_opt "FIGMA_TOKEN"
        else
          None
      else
        Some t
    | _ -> None

(** ============== Content creation helpers ============== *)

let build_file_meta json =
  let meta_root =
    match Yojson.Safe.Util.member "meta" json with
    | `Null -> json
    | m -> m
  in
  let pick key = Yojson.Safe.Util.member key meta_root in
  `Assoc [
    ("components", pick "components");
    ("componentSets", pick "componentSets");
    ("styles", pick "styles");
  ]

let make_text_content text : Yojson.Safe.t =
  `Assoc [
    ("content", `List [
      `Assoc [("type", `String "text"); ("text", `String text)]
    ])
  ]

let make_error_content msg : Yojson.Safe.t =
  `Assoc [
    ("content", `List [
      `Assoc [("type", `String "text"); ("text", `String msg); ("isError", `Bool true)]
    ])
  ]

(** ============== Eio Context ============== *)

(** Existential wrapper for clock to hide the type parameter *)
type any_clock = Clock : _ Eio.Time.clock -> any_clock

(** Existential wrapper for net to hide the type parameter *)
type any_net = Net : _ Eio.Net.t -> any_net

type eio_context = {
  sw: Eio.Switch.t;
  net: any_net;
  clock: any_clock;
  client: Figma_api_eio.client;
  domain: Domain.id;
}

let eio_context_key : eio_context option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

let get_eio_context () = Domain.DLS.get eio_context_key

let install_eio_context ctx = Domain.DLS.set eio_context_key (Some ctx)

(** Set Eio context from server startup *)
let set_eio_context ~sw ~net ~clock ~client =
  let ctx = {
    sw;
    net = Net net;
    clock = Clock clock;
    client;
    domain = Domain.self ();
  } in
  install_eio_context ctx;
  ctx

(** ============== Shared utilities (used by mcp_tools, mcp_visual_handlers, etc.) ============== *)

let assoc_or_empty json =
  match json with
  | `Assoc lst -> lst
  | _ -> []

let list_member key json =
  match member key json with
  | Some (`List lst) -> Some lst
  | _ -> None

let count_assoc_fields json =
  match json with
  | `Assoc lst -> List.length lst
  | _ -> 0

let count_list_items json =
  match json with
  | `List lst -> List.length lst
  | _ -> 0

let coverage_for_section json section_key missing_key weight =
  let present = member section_key json |> Option.value ~default:`Null |> count_assoc_fields in
  let missing = member missing_key json |> Option.value ~default:(`List []) |> count_list_items in
  let total = present + missing in
  let score = if total = 0 then 1.0 else (float_of_int present /. float_of_int total) in
  (score, present, missing, total, weight)

let fidelity_sections = [
  ("meta", "meta_missing", 0.4);
  ("structure", "structure_missing", 1.2);
  ("geometry", "geometry_missing", 1.2);
  ("vector", "vector_missing", 1.0);
  ("layout", "layout_missing", 2.0);
  ("paint", "paint_missing", 2.0);
  ("effects", "effects_missing", 1.0);
  ("text", "text_missing", 1.2);
  ("text_segments", "text_segments_missing", 1.0);
  ("instance", "instance_missing", 0.8);
  ("variables", "variables_missing", 0.6);
  ("variables_resolved", "variables_resolved_missing", 0.6);
  ("assets", "assets_missing", 0.8);
  ("plugin", "plugin_missing", 0.8);
]

let override_section ?score ~present ~missing ~total () =
  let score =
    match score with
    | Some s -> s
    | None -> if total = 0 then 1.0 else (float_of_int present /. float_of_int total)
  in
  (score, present, missing, total)

let fidelity_score_with_overrides json overrides =
  let override_for section = List.assoc_opt section overrides in
  let fold (score_sum, weight_sum, details, missing_total) (section, missing_key, weight) =
    let (score, present, missing, total) =
      match override_for section with
      | Some override -> override
      | None ->
          let (score, present, missing, total, _) =
            coverage_for_section json section missing_key weight
          in
          (score, present, missing, total)
    in
    let detail =
      `Assoc [
        ("score", `Float score);
        ("present", `Int present);
        ("missing", `Int missing);
        ("total", `Int total);
        ("weight", `Float weight);
      ]
    in
    (score_sum +. (score *. weight),
     weight_sum +. weight,
     (section, detail) :: details,
     missing_total + missing)
  in
  let (score_sum, weight_sum, details, missing_total) =
    List.fold_left fold (0.0, 0.0, [], 0) fidelity_sections
  in
  let overall = if weight_sum = 0.0 then 1.0 else score_sum /. weight_sum in
  let detail_json = `Assoc (List.rev details) in
  (overall, missing_total, detail_json)

let fidelity_score_of_dsl json =
  fidelity_score_with_overrides json []

let string_list_of_json json =
  match json with
  | `List items ->
      items
      |> List.filter_map (function `String s -> Some s | _ -> None)
  | _ -> []

let image_refs_of_dsl json =
  match member "assets" json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "image_refs" fields with
       | Some v -> string_list_of_json v
       | None -> [])
  | _ -> []

let image_fill_map image_fills =
  match image_fills with
  | `Assoc fields -> (
      match List.assoc_opt "images" fields with
      | Some (`Assoc items) -> items
      | _ -> [])
  | _ -> []

let variables_counts variables =
  let assoc_len json =
    match json with
    | `Assoc items -> List.length items
    | _ -> 0
  in
  match variables with
  | `Assoc fields when List.assoc_opt "error" fields <> None ->
      (1, 0)
  | `Assoc fields ->
      let raw_vars =
        match List.assoc_opt "variables" fields with
        | Some v -> assoc_len v
        | None -> 0
      in
      let resolved =
        match List.assoc_opt "resolved" fields with
        | Some v -> assoc_len v
        | None -> 0
      in
      (raw_vars, resolved)
  | _ -> (0, 0)

let plugin_ok plugin_snapshot =
  match plugin_snapshot with
  | `Assoc fields -> (
      match List.assoc_opt "ok" fields with
      | Some (`Bool b) -> b
      | _ -> false)
  | _ -> false

let rec count_text_segments json =
  match json with
  | `Assoc fields ->
      let self_count =
        match List.assoc_opt "text" fields with
        | Some (`Assoc text_fields) -> (
            match List.assoc_opt "segments" text_fields with
            | Some (`List segments) -> List.length segments
            | _ -> 0)
        | _ -> 0
      in
      let child_count =
        match List.assoc_opt "children" fields with
        | Some (`List children) ->
            List.fold_left (fun acc child -> acc + count_text_segments child) 0 children
        | _ -> 0
      in
      self_count + child_count
  | `List items ->
      List.fold_left (fun acc item -> acc + count_text_segments item) 0 items
  | _ -> 0

let rec count_text_nodes_dsl json =
  match json with
  | `Assoc fields ->
      let self_count =
        match List.assoc_opt "meta" fields with
        | Some (`Assoc meta_fields) -> (
            match List.assoc_opt "type" meta_fields with
            | Some (`String "TEXT") -> 1
            | _ -> 0)
        | _ -> 0
      in
      let child_count =
        match List.assoc_opt "children" fields with
        | Some (`List children) ->
            List.fold_left (fun acc child -> acc + count_text_nodes_dsl child) 0 children
        | _ -> 0
      in
      self_count + child_count
  | `List items ->
      List.fold_left (fun acc item -> acc + count_text_nodes_dsl item) 0 items
  | _ -> 0

let rec count_text_nodes_with_segments json =
  match json with
  | `Assoc fields ->
      let self_count =
        match List.assoc_opt "text" fields with
        | Some (`Assoc text_fields) -> (
            match List.assoc_opt "segments" text_fields with
            | Some (`List _) -> 1
            | _ -> 0)
        | _ -> 0
      in
      let child_count =
        match List.assoc_opt "children" fields with
        | Some (`List children) ->
            List.fold_left (fun acc child -> acc + count_text_nodes_with_segments child) 0 children
        | _ -> 0
      in
      self_count + child_count
  | `List items ->
      List.fold_left (fun acc item -> acc + count_text_nodes_with_segments item) 0 items
  | _ -> 0

let plugin_text_nodes_with_segments plugin_snapshot =
  match plugin_snapshot with
  | `Assoc fields -> (
      match List.assoc_opt "payload" fields with
      | Some payload -> count_text_nodes_with_segments payload
      | _ -> 0)
  | _ -> 0

let fidelity_score_of_bundle ~dsl_json ~variables ~image_fills ~plugin_snapshot ~include_variables ~include_image_fills ~include_plugin =
  let overrides = [] in
  let overrides =
    if include_image_fills then
      let refs = image_refs_of_dsl dsl_json in
      if refs = [] then
        overrides
      else
        let fill_map = image_fill_map image_fills in
        let present =
          List.fold_left (fun acc ref ->
            match List.assoc_opt ref fill_map with
            | Some (`String _) -> acc + 1
            | _ -> acc
          ) 0 refs
        in
        let total = List.length refs in
        let missing = max 0 (total - present) in
        ("assets", override_section ~present ~missing ~total ()) :: overrides
    else overrides
  in
  let overrides =
    if include_plugin && plugin_ok plugin_snapshot then
      let total_text_nodes = count_text_nodes_dsl dsl_json in
      if total_text_nodes = 0 then
        overrides
      else
        let present = plugin_text_nodes_with_segments plugin_snapshot in
        let present = min present total_text_nodes in
        ("text_segments", override_section ~present ~missing:(max 0 (total_text_nodes - present)) ~total:total_text_nodes ()) :: overrides
    else overrides
  in
  let overrides =
    if include_variables then
      let (total, present) = variables_counts variables in
      if total = 0 then
        overrides
      else
        ("variables_resolved", override_section ~present ~missing:(max 0 (total - present)) ~total ()) :: overrides
    else overrides
  in
  let overrides =
    if include_plugin then
      let present = if plugin_ok plugin_snapshot then 1 else 0 in
      ("plugin", override_section ~present ~missing:(1 - present) ~total:1 ()) :: overrides
    else overrides
  in
  fidelity_score_with_overrides dsl_json overrides

let default_asset_dir () = Figma_config.Asset.dir

let default_compare_dir () =
  default_asset_dir () ^ "/compare"

let sanitize_node_id id =
  let buf = Bytes.of_string id in
  Bytes.iteri (fun i c ->
    if c = ':' then Bytes.set buf i '_'
  ) buf;
  Bytes.to_string buf

let sanitize_file_key key =
  let buf = Buffer.create (String.length key) in
  String.iter (fun c ->
    let is_safe =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c = '-' || c = '_'
    in
    Buffer.add_char buf (if is_safe then c else '-')
  ) key;
  let sanitized = String.trim (Buffer.contents buf) in
  if sanitized = "" then "unknown" else sanitized

let strip_query url =
  match String.index_opt url '?' with
  | Some i -> String.sub url 0 i
  | None -> url

let file_ext_from_url url =
  let base = strip_query url |> Filename.basename in
  let ext = Filename.extension base in
  if ext = "" then ".img" else ext

let is_http_url s =
  match Uri.scheme (Uri.of_string s) with
  | Some ("http" | "https") -> true
  | _ -> false

let resolve_variables json =
  let member_opt key json =
    match json with
    | `Assoc lst -> List.assoc_opt key lst
    | _ -> None
  in
  let string_opt key json =
    match member_opt key json with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let meta = match member_opt "meta" json with
    | Some m -> m
    | None -> `Null
  in
  let collections = match member_opt "variableCollections" meta with
    | Some v -> v
    | None -> `Null
  in
  let variables = match member_opt "variables" meta with
    | Some v -> v
    | None -> `Null
  in
  let collection_map = assoc_or_empty collections in
  let default_mode_id_for collection_id =
    match List.assoc_opt collection_id collection_map with
    | Some col -> string_opt "defaultModeId" col
    | None -> None
  in
  let default_mode_name_for collection_id default_mode_id =
    match List.assoc_opt collection_id collection_map with
    | Some col ->
        (match list_member "modes" col with
         | Some modes ->
             let find_name = function
               | `Assoc fields -> (
                   match List.assoc_opt "modeId" fields, List.assoc_opt "name" fields with
                   | Some (`String mid), Some (`String name) when mid = default_mode_id -> Some name
                   | _ -> None)
               | _ -> None
             in
             List.find_map find_name modes
         | None -> None)
    | None -> None
  in
  let resolved =
    match variables with
    | `Assoc vars ->
        `Assoc (List.map (fun (var_id, var_json) ->
          let collection_id = string_opt "variableCollectionId" var_json in
          let default_mode_id = Option.bind collection_id default_mode_id_for in
          let default_mode_name =
            match (collection_id, default_mode_id) with
            | Some cid, Some mid -> default_mode_name_for cid mid
            | _ -> None
          in
          let values_by_mode = match member_opt "valuesByMode" var_json with
            | Some v -> v
            | None -> `Null
          in
          let default_value =
            match (default_mode_id, values_by_mode) with
            | Some mode_id, `Assoc values ->
                (match List.assoc_opt mode_id values with
                 | Some v -> v
                 | None -> `Null)
            | None, `Assoc values ->
                (match values with
                 | (_, v) :: _ -> v
                 | [] -> `Null)
            | _ -> `Null
          in
          let resolved_json =
            `Assoc [
              ("name", (match string_opt "name" var_json with Some s -> `String s | None -> `Null));
              ("resolvedType", (match string_opt "resolvedType" var_json with Some s -> `String s | None -> `Null));
              ("collectionId", (match collection_id with Some s -> `String s | None -> `Null));
              ("defaultModeId", (match default_mode_id with Some s -> `String s | None -> `Null));
              ("defaultModeName", (match default_mode_name with Some s -> `String s | None -> `Null));
              ("defaultValue", default_value);
              ("valuesByMode", values_by_mode);
            ]
          in
          (var_id, resolved_json)
        ) vars)
    | _ -> `Null
  in
  `Assoc [
    ("collections", collections);
    ("variables", variables);
    ("resolved", resolved);
  ]

let plugin_payload_if_ok plugin_result =
  match plugin_result with
  | `Assoc fields -> (
      match List.assoc_opt "ok" fields, List.assoc_opt "payload" fields with
      | Some (`Bool true), Some payload -> Some payload
      | _ -> None)
  | _ -> None

let resolve_plugin_variables payload =
  match payload with
  | `Assoc fields -> (
      match List.assoc_opt "collections" fields, List.assoc_opt "variables" fields with
      | Some collections, Some variables ->
          resolve_variables (`Assoc [
            ("meta", `Assoc [
              ("variableCollections", collections);
              ("variables", variables);
            ])
          ])
      | _ -> `Assoc [("error", `String "Missing plugin variables payload")])
  | _ -> `Assoc [("error", `String "Invalid plugin variables payload")]

let mkdir_p path =
  let rec loop p =
    if p = Filename.dirname p then ()
    else if Sys.file_exists p then ()
    else begin
      loop (Filename.dirname p);
      try Unix.mkdir p 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  loop path
