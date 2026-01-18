(** Figma 트리 구조 시각화 *)

open Figma_types

(** ============== 트리 출력 스타일 ============== *)

type tree_style =
  | Ascii      (** ├── └── 스타일 *)
  | Indent     (** 들여쓰기만 *)
  | Compact    (** 한 줄 압축 *)

(** ============== 노드 정보 포맷 ============== *)

let type_emoji = function
  | Document -> "📄"
  | Canvas -> "🎨"
  | Frame -> "🖼"
  | Group -> "📁"
  | Component -> "🧩"
  | ComponentSet -> "📦"
  | Instance -> "🔗"
  | Text -> "📝"
  | Rectangle -> "⬜"
  | Ellipse -> "⭕"
  | Vector -> "✏️"
  | Line -> "➖"
  | Star -> "⭐"
  | RegularPolygon -> "🔷"
  | BooleanOperation -> "🔀"
  | Slice -> "✂️"
  | Sticky -> "📌"
  | Section -> "📑"
  | Unknown _ -> "❓"

let node_info ?(show_size=true) ?(show_id=false) node =
  let type_str = Figma_query.node_type_to_string node.node_type in
  let emoji = type_emoji node.node_type in
  let size_str =
    if show_size then
      match node.bbox with
      | Some b -> Printf.sprintf " [%.0fx%.0f]" b.width b.height
      | None -> ""
    else ""
  in
  let id_str = if show_id then Printf.sprintf " (%s)" node.id else "" in
  Printf.sprintf "%s %s \"%s\"%s%s" emoji type_str node.name size_str id_str

(** ============== ASCII 트리 렌더링 ============== *)

let rec render_ascii ?(prefix="") ?(is_last=true) ?(depth=0) ~max_depth ~show_size ~show_id node =
  let connector = if depth = 0 then "" else if is_last then "└── " else "├── " in
  let line = prefix ^ connector ^ node_info ~show_size ~show_id node in

  let continue = match max_depth with
    | None -> true
    | Some d -> depth < d
  in

  if continue && List.length node.children > 0 then
    let new_prefix = if depth = 0 then "" else prefix ^ (if is_last then "    " else "│   ") in
    let children_count = List.length node.children in
    let children_lines = List.mapi (fun i child ->
      let is_child_last = (i = children_count - 1) in
      render_ascii ~prefix:new_prefix ~is_last:is_child_last
        ~depth:(depth + 1) ~max_depth ~show_size ~show_id child
    ) node.children in
    line :: List.concat children_lines
  else
    [line]

(** ============== 들여쓰기 트리 렌더링 ============== *)

let rec render_indent ?(depth=0) ~max_depth ~show_size ~show_id node =
  let indent = String.make (depth * 2) ' ' in
  let line = indent ^ node_info ~show_size ~show_id node in

  let continue = match max_depth with
    | None -> true
    | Some d -> depth < d
  in

  if continue then
    let children_lines = List.concat_map
      (render_indent ~depth:(depth + 1) ~max_depth ~show_size ~show_id)
      node.children
    in
    line :: children_lines
  else
    [line]

(** ============== 압축 트리 (한 줄) ============== *)

let rec render_compact ?(depth=0) ~max_depth node =
  let continue = match max_depth with
    | None -> true
    | Some d -> depth < d
  in

  let type_char = match node.node_type with
    | Frame -> "F"
    | Component -> "C"
    | Instance -> "I"
    | Text -> "T"
    | Group -> "G"
    | Rectangle -> "R"
    | _ -> "?"
  in

  if continue && List.length node.children > 0 then
    let children = String.concat "" (List.map (render_compact ~depth:(depth + 1) ~max_depth) node.children) in
    Printf.sprintf "%s[%s]" type_char children
  else
    type_char

(** ============== 통계 수집 ============== *)

type tree_stats = {
  total_nodes: int;
  max_depth: int;
  by_type: (string * int) list;
  leaf_count: int;
}

let rec collect_stats ?(depth=0) node =
  let type_str = Figma_query.node_type_to_string node.node_type in
  let is_leaf = List.length node.children = 0 in

  let children_stats = List.map (collect_stats ~depth:(depth + 1)) node.children in

  let merge_stats s1 s2 = {
    total_nodes = s1.total_nodes + s2.total_nodes;
    max_depth = max s1.max_depth s2.max_depth;
    by_type = (
      let merged = s1.by_type @ s2.by_type in
      let types = List.sort_uniq compare (List.map fst merged) in
      List.map (fun t ->
        let count = List.fold_left (fun acc (t', c) ->
          if t = t' then acc + c else acc
        ) 0 merged in
        (t, count)
      ) types);
    leaf_count = s1.leaf_count + s2.leaf_count;
  } in

  let self_stats = {
    total_nodes = 1;
    max_depth = depth;
    by_type = [(type_str, 1)];
    leaf_count = if is_leaf then 1 else 0;
  } in

  List.fold_left merge_stats self_stats children_stats

let stats_to_string stats =
  let type_breakdown = List.map (fun (t, c) ->
    Printf.sprintf "  %s: %d" t c
  ) (List.sort (fun (_, a) (_, b) -> compare b a) stats.by_type) in

  Printf.sprintf "=== 트리 통계 ===\n총 노드: %d\n최대 깊이: %d\n리프 노드: %d\n\n타입별:\n%s"
    stats.total_nodes stats.max_depth stats.leaf_count
    (String.concat "\n" type_breakdown)

(** ============== 메인 렌더 함수 ============== *)

let render ?(style=Ascii) ?(max_depth=None) ?(show_size=true) ?(show_id=false) ?(show_stats=false) node =
  let tree_str = match style with
    | Ascii -> String.concat "\n" (render_ascii ~max_depth ~show_size ~show_id node)
    | Indent -> String.concat "\n" (render_indent ~max_depth ~show_size ~show_id node)
    | Compact -> render_compact ~max_depth node
  in

  if show_stats then
    let stats = collect_stats node in
    tree_str ^ "\n\n" ^ stats_to_string stats
  else
    tree_str

(** ============== 경로 찾기 ============== *)

(** 특정 노드까지의 경로 찾기 *)
let rec find_path ~target_id node =
  if node.id = target_id then
    Some [node]
  else
    let found = List.find_map (find_path ~target_id) node.children in
    match found with
    | Some path -> Some (node :: path)
    | None -> None

let path_to_string path =
  let names = List.map (fun n -> n.name) path in
  String.concat " > " names
