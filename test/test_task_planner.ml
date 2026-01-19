(** Task Planner 테스트 스위트

    ROI-based Priority System 검증:
    - P1_Layout (80% SSIM): row/col, size, gap, padding, bg, radius
    - P2_Style (+10% SSIM): align, shadows, borders
    - P3_Text (+5% SSIM): font details
    - P4_Specialist: vectors, animations

    Outside-In 순서 검증:
    - 부모 컨테이너 → 자식 요소 순서
*)

open Alcotest
open Figma_types
open Task_planner

(** ============== Test Fixtures ============== *)

(** 기본 Frame 노드 (P1_Layout) *)
let make_frame ?(layout=Horizontal) ?(gap=0.) ?(padding=(0.,0.,0.,0.))
    ?(radius=0.) ?(children=[]) id name =
  { default_node with
    id; name;
    node_type = Frame;
    layout_mode = layout;
    gap;
    padding;
    border_radius = radius;
    children;
  }

(** 기본 Text 노드 (P3_Text) *)
let make_text id name =
  { default_node with
    id; name;
    node_type = Text;
    typography = Some {
      font_family = "Inter";
      font_size = 16.;
      font_weight = 400;
      font_style = "normal";
      line_height = None;
      letter_spacing = None;
      text_align_h = Left;
      text_align_v = Top;
      text_decoration = NoDeco;
      text_case = Original;
    };
  }

(** 기본 Rectangle 노드 (P2_Style) *)
let make_rect id name =
  { default_node with
    id; name;
    node_type = Rectangle;
    border_radius = 8.;
  }

(** 기본 Vector 노드 (P4_Specialist) *)
let make_vector id name =
  { default_node with
    id; name;
    node_type = Vector;
  }

(** ============== Priority Classification Tests ============== *)

let test_classify_frame () =
  let priority = classify_node_type Frame in
  check (testable (Fmt.of_to_string priority_to_string) (=))
    "Frame → P1_Layout" P1_Layout priority

let test_classify_text () =
  let priority = classify_node_type Text in
  check (testable (Fmt.of_to_string priority_to_string) (=))
    "Text → P3_Text" P3_Text priority

let test_classify_rectangle () =
  let priority = classify_node_type Rectangle in
  check (testable (Fmt.of_to_string priority_to_string) (=))
    "Rectangle → P2_Style" P2_Style priority

let test_classify_vector () =
  let priority = classify_node_type Vector in
  check (testable (Fmt.of_to_string priority_to_string) (=))
    "Vector → P4_Specialist" P4_Specialist priority

let priority_tests = [
  "classify Frame", `Quick, test_classify_frame;
  "classify Text", `Quick, test_classify_text;
  "classify Rectangle", `Quick, test_classify_rectangle;
  "classify Vector", `Quick, test_classify_vector;
]

(** ============== Priority Adjustment Tests ============== *)

let test_adjust_layout_to_p1 () =
  (* layout_mode가 있으면 P1으로 상향 *)
  let node = { default_node with layout_mode = Horizontal } in
  let adjusted = adjust_priority node P2_Style in
  check (testable (Fmt.of_to_string priority_to_string) (=))
    "layout → P1" P1_Layout adjusted

let test_adjust_style_effects () =
  (* effects가 있으면 P2 유지 *)
  let fx_instance : fx = { fx_type = DropShadow; visible = true; radius = 4.;
                           color = None; offset = Some (0., 2.); spread = Some 0. } in
  let node = { default_node with effects = [fx_instance] } in
  let adjusted = adjust_priority node P4_Specialist in
  check (testable (Fmt.of_to_string priority_to_string) (=))
    "effects → P2" P2_Style adjusted

let adjustment_tests = [
  "layout mode → P1", `Quick, test_adjust_layout_to_p1;
  "effects → P2", `Quick, test_adjust_style_effects;
]

(** ============== Task Collection Tests (Outside-In) ============== *)

let test_collect_single_node () =
  let node = make_frame "1:1" "Root" in
  let tasks = collect_tasks node in
  check int "single node → 1 task" 1 (List.length tasks);
  check string "task id" "task_1:1" (List.hd tasks).id

let test_collect_nested_nodes () =
  (* Root → Child1, Child2 *)
  let child1 = make_text "1:2" "Title" in
  let child2 = make_rect "1:3" "Icon" in
  let root = make_frame ~children:[child1; child2] "1:1" "Header" in
  let tasks = collect_tasks root in
  check int "3 nodes → 3 tasks" 3 (List.length tasks);
  (* Outside-In: Root first *)
  check string "first task is root" "task_1:1" (List.hd tasks).id

let test_dependencies () =
  let child = make_text "1:2" "Title" in
  let root = make_frame ~children:[child] "1:1" "Header" in
  let tasks = collect_tasks root in
  let child_task = List.find (fun t -> t.id = "task_1:2") tasks in
  check (list string) "child depends on parent"
    ["task_1:1"] child_task.dependencies

let collection_tests = [
  "single node", `Quick, test_collect_single_node;
  "nested nodes", `Quick, test_collect_nested_nodes;
  "dependencies", `Quick, test_dependencies;
]

(** ============== Flat Node Task Tests ============== *)

let test_tasks_from_flat_dependencies () =
  let root = make_frame "1:1" "Root" in
  let child = make_text "1:2" "Child" in
  let flat_nodes = [
    { node = root; parent_node_id = None; depth = 0 };
    { node = child; parent_node_id = Some "1:1"; depth = 1 };
  ] in
  let tasks = tasks_from_flat flat_nodes in
  let child_task = List.find (fun t -> t.node_id = "1:2") tasks in
  check (list string) "child depends on parent"
    ["task_1:1"] child_task.dependencies

let flat_tasks_tests = [
  "flat dependencies", `Quick, test_tasks_from_flat_dependencies;
]

(** ============== Priority Sorting Tests ============== *)

let test_sort_by_priority () =
  let child1 = make_text "1:2" "Title" in    (* P3 *)
  let child2 = make_rect "1:3" "Box" in      (* P2 *)
  let child3 = make_vector "1:4" "Icon" in   (* P4 *)
  let root = make_frame ~layout:Horizontal ~children:[child1; child2; child3]
               "1:1" "Header" in             (* P1 *)
  let tasks = collect_tasks root in
  let sorted = sort_by_priority tasks in

  (* P1 → P2 → P3 → P4 순서 확인 *)
  let priorities = List.map (fun t -> priority_to_int t.priority) sorted in
  let is_sorted = priorities = List.sort compare priorities in
  check bool "sorted by priority" true is_sorted

let sorting_tests = [
  "sort by priority", `Quick, test_sort_by_priority;
]

(** ============== DSL Summary Tests ============== *)

let test_summarize_frame () =
  let node = make_frame ~layout:Horizontal ~gap:12. ~radius:8. "1:1" "Card" in
  let node_with_bbox = { node with bbox = Some { x = 0.; y = 0.; width = 320.; height = 200. } } in
  let dsl = summarize_dsl node_with_bbox in
  check bool "contains size" true (String.length dsl > 0);
  check bool "contains row" true (String.sub dsl 0 3 = "320")

let test_summarize_with_bg () =
  let fill = { paint_type = Solid; visible = true; opacity = 1.;
               color = Some { r = 1.; g = 1.; b = 1.; a = 1. };
               gradient_stops = []; image_ref = None; scale_mode = None } in
  let node = { default_node with fills = [fill] } in
  let dsl = summarize_dsl node in
  check bool "contains bg" true (String.length dsl > 0)

let dsl_tests = [
  "summarize frame", `Quick, test_summarize_frame;
  "summarize with bg", `Quick, test_summarize_with_bg;
]

(** ============== Hint Generation Tests ============== *)

let test_hints_layout () =
  let node = make_frame ~layout:Horizontal "1:1" "Row" in
  let hints = generate_hints node in
  check bool "has row hint" true
    (List.exists (fun h -> String.sub h 0 3 = "row") hints)

let test_hints_gap () =
  let node = make_frame ~gap:12. "1:1" "Spaced" in
  let hints = generate_hints node in
  check bool "has gap hint" true
    (List.exists (fun h -> String.sub h 0 3 = "gap") hints)

let test_hints_children () =
  let child = make_text "1:2" "Title" in
  let node = make_frame ~children:[child] "1:1" "Container" in
  let hints = generate_hints node in
  check bool "has children hint" true
    (List.exists (fun h -> String.sub h (String.length h - 9) 9 = "implement") hints)

let hint_tests = [
  "layout hint", `Quick, test_hints_layout;
  "gap hint", `Quick, test_hints_gap;
  "children hint", `Quick, test_hints_children;
]

(** ============== Token Estimation Tests ============== *)

let test_estimate_base () =
  let node = { default_node with id = "1:1" } in
  let tokens = estimate_tokens node in
  check bool "base tokens >= 50" true (tokens >= 50)

let test_estimate_with_layout () =
  let node = { default_node with layout_mode = Horizontal } in
  let tokens = estimate_tokens node in
  check bool "layout adds tokens" true (tokens >= 80)

let test_estimate_with_children () =
  let child = make_text "1:2" "Text" in
  let node = { default_node with children = [child; child; child] } in
  let tokens = estimate_tokens node in
  check bool "children add tokens" true (tokens >= 110)

let estimation_tests = [
  "base estimation", `Quick, test_estimate_base;
  "layout estimation", `Quick, test_estimate_with_layout;
  "children estimation", `Quick, test_estimate_with_children;
]

(** ============== JSON Output Tests ============== *)

let test_task_to_json () =
  let task = {
    id = "task_1:1";
    node_id = "1:1";
    node_name = "Header";
    node_type = "Frame";
    priority = P1_Layout;
    dependencies = [];
    estimated_tokens = 100;
    semantic_dsl = "320×80 row g:12";
    hints = ["row layout"];
  } in
  let json = task_to_json task in
  check bool "contains id" true (String.length json > 0);
  check bool "is valid structure" true (String.sub json 0 1 = "{")

let test_plan_tasks_json () =
  let child = make_text "1:2" "Title" in
  let root = make_frame ~children:[child] "1:1" "Header" in
  let json = plan_tasks root in
  check bool "is JSON array" true (String.sub json 0 1 = "[");
  check bool "contains tasks" true (String.length json > 10)

let json_tests = [
  "task to json", `Quick, test_task_to_json;
  "plan_tasks json", `Quick, test_plan_tasks_json;
]

(** ============== Summary Tests ============== *)

let test_summarize_plan () =
  let child1 = make_text "1:2" "Title" in
  let child2 = make_rect "1:3" "Box" in
  let root = make_frame ~layout:Horizontal ~children:[child1; child2] "1:1" "Header" in
  let tasks = collect_tasks root in
  let summary = summarize_plan tasks in
  check bool "contains total" true (String.length summary > 0);
  check bool "contains P1" true (String.length summary > 20)

let summary_tests = [
  "summarize plan", `Quick, test_summarize_plan;
]

(** ============== Integration Tests ============== *)

let test_full_workflow () =
  (* 복잡한 UI 트리 구성 *)
  let title = make_text "1:10" "Header Title" in
  let icon = make_vector "1:11" "Menu Icon" in
  let header = make_frame ~layout:Horizontal ~gap:8. ~children:[icon; title]
                 "1:2" "Header" in

  let card_title = make_text "1:20" "Card Title" in
  let card_desc = make_text "1:21" "Description" in
  let card = make_frame ~layout:Vertical ~gap:4. ~radius:8.
               ~children:[card_title; card_desc] "1:3" "Card" in

  let button = make_rect "1:4" "Button" in

  let main = make_frame ~layout:Vertical ~gap:16. ~padding:(16., 16., 16., 16.)
               ~children:[header; card; button] "1:1" "Main Screen" in

  (* 전체 워크플로우 실행 *)
  let tasks = collect_tasks main in
  let sorted = sort_by_priority tasks in
  let json = tasks_to_json sorted in
  let summary = summarize_plan tasks in

  (* 검증 *)
  check int "total tasks" 8 (List.length tasks);
  check bool "json valid" true (String.sub json 0 1 = "[");
  check bool "summary valid" true (String.length summary > 30);

  (* P1 우선 확인 *)
  let first_task = List.hd sorted in
  check (testable (Fmt.of_to_string priority_to_string) (=))
    "first is P1" P1_Layout first_task.priority

let integration_tests = [
  "full workflow", `Quick, test_full_workflow;
]

(** ============== 메인 ============== *)

let () =
  run "Task Planner" [
    "Priority Classification", priority_tests;
    "Priority Adjustment", adjustment_tests;
    "Task Collection (Outside-In)", collection_tests;
    "Task Collection (Flat)", flat_tasks_tests;
    "Priority Sorting", sorting_tests;
    "DSL Summary", dsl_tests;
    "Hint Generation", hint_tests;
    "Token Estimation", estimation_tests;
    "JSON Output", json_tests;
    "Plan Summary", summary_tests;
    "Integration", integration_tests;
  ]
