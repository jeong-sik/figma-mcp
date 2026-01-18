(** Figma 노드 쿼리/필터링 엔진 *)

open Figma_types

(** ============== 필터 타입 ============== *)

type comparison = Eq | Gte | Lte | Gt | Lt

type filter =
  | TypeFilter of string list                 (** 노드 타입 (FRAME, TEXT, etc.) *)
  | WidthFilter of comparison * float         (** 너비 조건 *)
  | HeightFilter of comparison * float        (** 높이 조건 *)
  | ColorFilter of string                     (** 색상 (hex) *)
  | NameFilter of string                      (** 이름 패턴 (substring) *)
  | VisibleFilter of bool                     (** visible 속성 *)
  | HasChildrenFilter of bool                 (** 자식 유무 *)

type query = {
  filters: filter list;
  depth: int option;          (** None = 무제한, Some n = n단계까지 *)
  limit: int option;          (** 결과 개수 제한 *)
}

(** ============== 기본 쿼리 ============== *)

let empty_query = {
  filters = [];
  depth = None;
  limit = None;
}

(** ============== 필터 적용 ============== *)

let compare_value comp actual expected =
  match comp with
  | Eq -> actual = expected
  | Gte -> actual >= expected
  | Lte -> actual <= expected
  | Gt -> actual > expected
  | Lt -> actual < expected

let node_type_to_string = function
  | Document -> "DOCUMENT"
  | Canvas -> "CANVAS"
  | Frame -> "FRAME"
  | Group -> "GROUP"
  | Vector -> "VECTOR"
  | BooleanOperation -> "BOOLEAN_OPERATION"
  | Star -> "STAR"
  | Line -> "LINE"
  | Ellipse -> "ELLIPSE"
  | RegularPolygon -> "REGULAR_POLYGON"
  | Rectangle -> "RECTANGLE"
  | Text -> "TEXT"
  | Slice -> "SLICE"
  | Component -> "COMPONENT"
  | ComponentSet -> "COMPONENT_SET"
  | Instance -> "INSTANCE"
  | Sticky -> "STICKY"
  | Section -> "SECTION"
  | Unknown _ -> "UNKNOWN"

let get_node_width node =
  match node.bbox with
  | Some b -> Some b.width
  | None -> None

let get_node_height node =
  match node.bbox with
  | Some b -> Some b.height
  | None -> None

(** paint에서 hex 색상 추출 *)
let paint_to_hex (paint: paint) =
  match paint.paint_type, paint.color with
  | Solid, Some color ->
      let r = int_of_float (color.r *. 255.) in
      let g = int_of_float (color.g *. 255.) in
      let b = int_of_float (color.b *. 255.) in
      Some (Printf.sprintf "#%02X%02X%02X" r g b)
  | _ -> None

(** 노드가 특정 색상을 포함하는지 *)
let node_has_color node hex =
  let hex_upper = String.uppercase_ascii hex in
  List.exists (fun p ->
    match paint_to_hex p with
    | Some c -> String.uppercase_ascii c = hex_upper
    | None -> false
  ) node.fills

(** 단일 필터 적용 *)
let apply_filter filter node =
  match filter with
  | TypeFilter types ->
      let node_type_str = node_type_to_string node.node_type in
      List.exists (fun t -> String.uppercase_ascii t = node_type_str) types
  | WidthFilter (comp, value) ->
      (match get_node_width node with
       | Some w -> compare_value comp w value
       | None -> false)
  | HeightFilter (comp, value) ->
      (match get_node_height node with
       | Some h -> compare_value comp h value
       | None -> false)
  | ColorFilter hex ->
      node_has_color node hex
  | NameFilter pattern ->
      let pattern_lower = String.lowercase_ascii pattern in
      let name_lower = String.lowercase_ascii node.name in
      (* substring match *)
      (try
        let _ = Str.search_forward (Str.regexp_string pattern_lower) name_lower 0 in
        true
      with Not_found -> false)
  | VisibleFilter vis ->
      node.visible = vis
  | HasChildrenFilter has ->
      (List.length node.children > 0) = has

(** 모든 필터 적용 (AND 조건) *)
let matches_filters filters node =
  List.for_all (fun f -> apply_filter f node) filters

(** ============== 트리 탐색 ============== *)

(** 깊이 제한 탐색으로 모든 노드 수집 *)
let rec collect_nodes ?(current_depth=0) ~max_depth node =
  let should_continue = match max_depth with
    | None -> true
    | Some d -> current_depth < d
  in
  let self = [node] in
  if should_continue then
    let children = List.concat_map
      (collect_nodes ~current_depth:(current_depth + 1) ~max_depth)
      node.children
    in
    self @ children
  else
    self

(** ============== 쿼리 실행 ============== *)

(** 쿼리 실행: 노드 트리에서 조건에 맞는 노드들 반환 *)
let execute_query query root =
  (* 1. 깊이 제한으로 노드 수집 *)
  let all_nodes = collect_nodes ~max_depth:query.depth root in

  (* 2. 필터 적용 *)
  let filtered = List.filter (matches_filters query.filters) all_nodes in

  (* 3. 개수 제한 *)
  match query.limit with
  | None -> filtered
  | Some n -> List.filteri (fun i _ -> i < n) filtered

(** ============== 쿼리 빌더 헬퍼 ============== *)

let with_type types query =
  { query with filters = TypeFilter types :: query.filters }

let with_width_min min_width query =
  { query with filters = WidthFilter (Gte, min_width) :: query.filters }

let with_width_max max_width query =
  { query with filters = WidthFilter (Lte, max_width) :: query.filters }

let with_height_min min_height query =
  { query with filters = HeightFilter (Gte, min_height) :: query.filters }

let with_height_max max_height query =
  { query with filters = HeightFilter (Lte, max_height) :: query.filters }

let with_color hex query =
  { query with filters = ColorFilter hex :: query.filters }

let with_name pattern query =
  { query with filters = NameFilter pattern :: query.filters }

let with_depth d query =
  { query with depth = Some d }

let with_limit n query =
  { query with limit = Some n }

(** ============== 결과 포맷팅 ============== *)

(** 노드 요약 문자열 *)
let node_summary node =
  let type_str = node_type_to_string node.node_type in
  let size_str = match node.bbox with
    | Some b -> Printf.sprintf "%.0fx%.0f" b.width b.height
    | None -> "?"
  in
  Printf.sprintf "%s \"%s\" [%s] (id: %s)" type_str node.name size_str node.id

(** 쿼리 결과를 문자열로 *)
let results_to_string nodes =
  let count = List.length nodes in
  let summaries = List.map node_summary nodes in
  Printf.sprintf "Found %d nodes:\n%s" count (String.concat "\n" summaries)
