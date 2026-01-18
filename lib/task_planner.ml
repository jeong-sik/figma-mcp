(** Task Planner - Figma DSL → Implementation Task Generator

    Hybrid Architecture (gRPC + Agent):
    - gRPC side (이 모듈): 빠른 파싱, ROI Tier 분류, Task Skeleton 생성
    - Agent side: 컨텍스트 인식, 의존성 분석, TodoWrite/MASC 생성

    ROI-Based Priority (UIFormer 연구 기반):
    - P1 (Layout): row/col, size, gap, padding - SSIM 80%+ 기여
    - P2 (Style): bg, radius, shadow, border - SSIM +10%
    - P3 (Text): font-size, color, weight - SSIM +5%
    - P4 (Specialist): vectors, animations, components
*)

open Figma_types

(** ============== Task Priority ============== *)

type priority =
  | P1_Layout    (** High ROI: 구조 결정 (80% SSIM) *)
  | P2_Style     (** Medium ROI: 시각적 완성도 (+10% SSIM) *)
  | P3_Text      (** Low ROI: 타이포그래피 (+5% SSIM) *)
  | P4_Specialist (** Domain-specific: 벡터, 애니메이션 *)

let priority_to_string = function
  | P1_Layout -> "P1_Layout"
  | P2_Style -> "P2_Style"
  | P3_Text -> "P3_Text"
  | P4_Specialist -> "P4_Specialist"

let priority_to_int = function
  | P1_Layout -> 1
  | P2_Style -> 2
  | P3_Text -> 3
  | P4_Specialist -> 4

(** ============== Task Definition ============== *)

type task = {
  id: string;                 (** Task ID (node_id 기반) *)
  node_id: string;           (** 원본 Figma 노드 ID *)
  node_name: string;         (** 노드 이름 (디버깅용) *)
  node_type: string;         (** Frame, Text, Vector 등 *)
  priority: priority;         (** 우선순위 *)
  dependencies: string list;  (** 선행 태스크 ID 목록 *)
  estimated_tokens: int;      (** 예상 토큰 소비량 *)
  semantic_dsl: string;       (** Semantic DSL 요약 *)
  hints: string list;         (** Agent에게 전달할 힌트 *)
}

(** ============== Node Type Classification ============== *)

(** 노드 타입에 따른 기본 우선순위 결정 *)
let classify_node_type = function
  | Frame | Group | Section -> P1_Layout
  | Text -> P3_Text
  | Vector | Star | Line | Ellipse | RegularPolygon -> P4_Specialist
  | Rectangle -> P2_Style  (* 단순 사각형은 스타일 *)
  | Component | ComponentSet | Instance -> P2_Style
  | BooleanOperation -> P4_Specialist
  | Document | Canvas | Slice | Sticky -> P4_Specialist
  | Unknown _ -> P4_Specialist

(** 노드 타입을 문자열로 변환 *)
let node_type_to_string = function
  | Document -> "Document"
  | Canvas -> "Canvas"
  | Frame -> "Frame"
  | Group -> "Group"
  | Vector -> "Vector"
  | BooleanOperation -> "BooleanOperation"
  | Star -> "Star"
  | Line -> "Line"
  | Ellipse -> "Ellipse"
  | RegularPolygon -> "RegularPolygon"
  | Rectangle -> "Rectangle"
  | Text -> "Text"
  | Slice -> "Slice"
  | Component -> "Component"
  | ComponentSet -> "ComponentSet"
  | Instance -> "Instance"
  | Sticky -> "Sticky"
  | Section -> "Section"
  | Unknown s -> "Unknown(" ^ s ^ ")"

(** ============== Priority Adjustment ============== *)

(** 노드 속성에 따른 우선순위 조정 *)
let adjust_priority (node : ui_node) base_priority =
  (* Layout 속성이 있으면 P1으로 상향 *)
  if node.layout_mode <> None' then P1_Layout
  (* 복잡한 스타일이 있으면 P2로 *)
  else if List.length node.effects > 0 || node.border_radius > 0. then
    min base_priority P2_Style
  (* 텍스트 노드는 P3 유지 *)
  else if node.typography <> None then P3_Text
  else base_priority

(** ============== Hint Generation ============== *)

(** 노드 특성에 따른 힌트 생성 *)
let generate_hints (node : ui_node) =
  let hints = ref [] in

  (* Layout 힌트 *)
  if node.layout_mode = Horizontal then
    hints := "row layout - implement with flex-row" :: !hints
  else if node.layout_mode = Vertical then
    hints := "column layout - implement with flex-col" :: !hints;

  (* Gap/Padding 힌트 *)
  if node.gap > 0. then
    hints := Printf.sprintf "gap: %.0fpx between children" node.gap :: !hints;

  let (pt, pr, pb, pl) = node.padding in
  if pt > 0. || pr > 0. || pb > 0. || pl > 0. then
    hints := Printf.sprintf "padding: %.0f %.0f %.0f %.0f" pt pr pb pl :: !hints;

  (* Style 힌트 *)
  if node.border_radius > 0. then
    hints := Printf.sprintf "rounded corners: %.0fpx" node.border_radius :: !hints;

  if List.length node.effects > 0 then
    hints := Printf.sprintf "%d effects (shadow/blur)" (List.length node.effects) :: !hints;

  (* Children 힌트 *)
  let child_count = List.length node.children in
  if child_count > 0 then
    hints := Printf.sprintf "%d children to implement" child_count :: !hints;

  List.rev !hints

(** ============== Token Estimation ============== *)

(** 예상 토큰 소비량 계산 *)
let estimate_tokens (node : ui_node) =
  let base = 50 in  (* 기본 토큰 *)
  let layout_cost = if node.layout_mode <> None' then 30 else 0 in
  let style_cost = (List.length node.fills * 10) + (List.length node.effects * 20) in
  let text_cost = match node.typography with Some _ -> 40 | None -> 0 in
  let child_cost = List.length node.children * 20 in
  base + layout_cost + style_cost + text_cost + child_cost

(** ============== DSL Summary ============== *)

(** 노드의 Semantic DSL 요약 생성 *)
let summarize_dsl (node : ui_node) =
  let parts = ref [] in

  (* Size *)
  (match node.bbox with
   | Some bbox ->
     parts := Printf.sprintf "%.0f×%.0f" bbox.width bbox.height :: !parts
   | None -> ());

  (* Layout *)
  (match node.layout_mode with
   | Horizontal -> parts := "row" :: !parts
   | Vertical -> parts := "col" :: !parts
   | None' -> ());

  (* Gap *)
  if node.gap > 0. then
    parts := Printf.sprintf "g:%.0f" node.gap :: !parts;

  (* Background (first fill) *)
  (match node.fills with
   | fill :: _ when fill.visible ->
     (match fill.color with
      | Some c -> parts := Printf.sprintf "bg:%s" (rgba_to_hex c) :: !parts
      | None -> ())
   | _ -> ());

  (* Radius *)
  if node.border_radius > 0. then
    parts := Printf.sprintf "r:%.0f" node.border_radius :: !parts;

  String.concat " " (List.rev !parts)

(** ============== Task Generation ============== *)

(** 단일 노드에서 태스크 생성 *)
let task_of_node ?(parent_id = None) (node : ui_node) =
  let base_priority = classify_node_type node.node_type in
  let priority = adjust_priority node base_priority in
  {
    id = "task_" ^ node.id;
    node_id = node.id;
    node_name = node.name;
    node_type = node_type_to_string node.node_type;
    priority;
    dependencies = (match parent_id with Some pid -> [pid] | None -> []);
    estimated_tokens = estimate_tokens node;
    semantic_dsl = summarize_dsl node;
    hints = generate_hints node;
  }

(** 노드 트리에서 모든 태스크 생성 (Outside-In 순서) *)
let rec collect_tasks ?(parent_id = None) (node : ui_node) =
  let task = task_of_node ~parent_id node in
  let child_tasks =
    node.children
    |> List.map (collect_tasks ~parent_id:(Some task.id))
    |> List.flatten
  in
  task :: child_tasks

(** ============== Task Sorting (by Priority) ============== *)

(** 우선순위 기반 정렬 *)
let sort_by_priority tasks =
  List.sort (fun a b -> compare (priority_to_int a.priority) (priority_to_int b.priority)) tasks

(** ============== JSON Output ============== *)

(** Task를 JSON 문자열로 변환 *)
let task_to_json task =
  let hints_json =
    task.hints
    |> List.map (Printf.sprintf "\"%s\"")
    |> String.concat ", "
  in
  let deps_json =
    task.dependencies
    |> List.map (Printf.sprintf "\"%s\"")
    |> String.concat ", "
  in
  Printf.sprintf {|{
  "id": "%s",
  "node_id": "%s",
  "node_name": "%s",
  "node_type": "%s",
  "priority": "%s",
  "dependencies": [%s],
  "estimated_tokens": %d,
  "semantic_dsl": "%s",
  "hints": [%s]
}|}
    task.id
    task.node_id
    (String.escaped task.node_name)
    task.node_type
    (priority_to_string task.priority)
    deps_json
    task.estimated_tokens
    (String.escaped task.semantic_dsl)
    hints_json

(** Task 목록을 JSON 배열로 변환 *)
let tasks_to_json tasks =
  let json_tasks = List.map task_to_json tasks in
  "[\n" ^ String.concat ",\n" json_tasks ^ "\n]"

(** ============== Main Entry Point ============== *)

(** ui_node 트리를 분석하여 정렬된 Task JSON 반환 *)
let plan_tasks (root : ui_node) =
  let tasks = collect_tasks root in
  let sorted = sort_by_priority tasks in
  tasks_to_json sorted

(** Task 통계 요약 *)
let summarize_plan tasks =
  let p1 = List.filter (fun t -> t.priority = P1_Layout) tasks |> List.length in
  let p2 = List.filter (fun t -> t.priority = P2_Style) tasks |> List.length in
  let p3 = List.filter (fun t -> t.priority = P3_Text) tasks |> List.length in
  let p4 = List.filter (fun t -> t.priority = P4_Specialist) tasks |> List.length in
  let total_tokens = List.fold_left (fun acc t -> acc + t.estimated_tokens) 0 tasks in
  Printf.sprintf "Tasks: %d total (P1:%d, P2:%d, P3:%d, P4:%d) | Est. tokens: %d"
    (List.length tasks) p1 p2 p3 p4 total_tokens
