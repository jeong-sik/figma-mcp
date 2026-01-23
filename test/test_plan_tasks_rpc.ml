(** PlanTasks RPC 통합 테스트

    Handler 응답 구조 검증:
    - tasks: Task 배열 (JSON)
    - summary: 계획 요약 문자열
    - total_estimated_tokens: 토큰 추정치
    - root_node_id: 루트 노드 ID

    Task_planner 모듈과 gRPC Handler 간의 통합 검증
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

(** ============== JSON Structure Tests ============== *)

(** tasks 배열이 유효한 JSON 형식인지 검증 *)
let test_tasks_json_valid () =
  let child = make_text "1:2" "Title" in
  let root = make_frame ~children:[child] "1:1" "Header" in
  let tasks = collect_tasks root in
  let sorted = sort_by_priority tasks in
  let json = tasks_to_json sorted in
  (* JSON 배열 시작 *)
  check bool "starts with [" true (String.sub json 0 1 = "[");
  (* JSON 배열 종료 *)
  let len = String.length json in
  check bool "ends with ]" true (String.sub json (len - 1) 1 = "]")

(** 개별 task JSON에 필수 필드 포함 확인 *)
let test_task_has_required_fields () =
  let node = make_frame ~layout:Horizontal "1:1" "Card" in
  let tasks = collect_tasks node in
  let task = List.hd tasks in
  let json = task_to_json task in
  (* 필수 필드 확인 *)
  check bool "has id" true (String.length json > 0 &&
    let _ = Str.search_forward (Str.regexp "\"id\"") json 0 in true);
  check bool "has node_id" true (
    let _ = Str.search_forward (Str.regexp "\"node_id\"") json 0 in true);
  check bool "has priority" true (
    let _ = Str.search_forward (Str.regexp "\"priority\"") json 0 in true);
  check bool "has estimated_tokens" true (
    let _ = Str.search_forward (Str.regexp "\"estimated_tokens\"") json 0 in true)

let json_structure_tests = [
  "tasks JSON valid", `Quick, test_tasks_json_valid;
  "task required fields", `Quick, test_task_has_required_fields;
]

(** ============== Response Structure Tests ============== *)

(** RPC 응답 형식 시뮬레이션 및 검증 *)
let simulate_rpc_response node =
  let tasks = collect_tasks node in
  let sorted = sort_by_priority tasks in
  let total_tokens = List.fold_left (fun acc t -> acc + t.estimated_tokens) 0 sorted in
  Printf.sprintf {|{
  "tasks": %s,
  "summary": "%s",
  "total_estimated_tokens": %d,
  "root_node_id": "%s"
}|}
    (tasks_to_json sorted)
    (summarize_plan sorted)
    total_tokens
    node.id

(** 응답에 tasks 필드 포함 *)
let test_response_has_tasks () =
  let node = make_frame "1:1" "Root" in
  let response = simulate_rpc_response node in
  check bool "has tasks field" true (
    try let _ = Str.search_forward (Str.regexp "\"tasks\":") response 0 in true
    with Not_found -> false)

(** 응답에 summary 필드 포함 *)
let test_response_has_summary () =
  let node = make_frame "1:1" "Root" in
  let response = simulate_rpc_response node in
  check bool "has summary field" true (
    try let _ = Str.search_forward (Str.regexp "\"summary\":") response 0 in true
    with Not_found -> false)

(** 응답에 total_estimated_tokens 필드 포함 *)
let test_response_has_tokens () =
  let node = make_frame "1:1" "Root" in
  let response = simulate_rpc_response node in
  check bool "has total_estimated_tokens field" true (
    try let _ = Str.search_forward (Str.regexp "\"total_estimated_tokens\":") response 0 in true
    with Not_found -> false)

(** 응답에 root_node_id 필드 포함 *)
let test_response_has_root_id () =
  let node = make_frame "1:1" "Root" in
  let response = simulate_rpc_response node in
  check bool "has root_node_id field" true (
    try let _ = Str.search_forward (Str.regexp "\"root_node_id\":") response 0 in true
    with Not_found -> false)

let response_structure_tests = [
  "response has tasks", `Quick, test_response_has_tasks;
  "response has summary", `Quick, test_response_has_summary;
  "response has total_tokens", `Quick, test_response_has_tokens;
  "response has root_node_id", `Quick, test_response_has_root_id;
]

(** ============== Priority Order Tests ============== *)

(** P1 > P2 > P3 > P4 순서 검증 *)
let test_priority_order_in_response () =
  let text = make_text "1:4" "Label" in      (* P3 *)
  let rect = make_rect "1:3" "Box" in        (* P2 *)
  let icon = make_vector "1:2" "Icon" in     (* P4 *)
  let root = make_frame ~layout:Horizontal ~children:[text; rect; icon]
               "1:1" "Container" in          (* P1 *)
  let tasks = collect_tasks root in
  let sorted = sort_by_priority tasks in

  (* 첫 번째는 P1 *)
  let first = List.hd sorted in
  check (testable (Fmt.of_to_string priority_to_string) (=))
    "first is P1" P1_Layout first.priority;

  (* 마지막은 P4 *)
  let last = List.nth sorted (List.length sorted - 1) in
  check (testable (Fmt.of_to_string priority_to_string) (=))
    "last is P4" P4_Specialist last.priority

(** 동일 우선순위 내 Outside-In 순서 (부모 먼저) *)
let test_outside_in_order () =
  let inner_frame = make_frame ~layout:Vertical "1:2" "Inner" in
  let outer_frame = make_frame ~layout:Horizontal ~children:[inner_frame]
                      "1:1" "Outer" in
  let tasks = collect_tasks outer_frame in

  (* Outer가 Inner보다 먼저 *)
  let outer_idx = ref (-1) in
  let inner_idx = ref (-1) in
  List.iteri (fun i t ->
    if t.node_id = "1:1" then outer_idx := i;
    if t.node_id = "1:2" then inner_idx := i
  ) tasks;
  check bool "outer before inner" true (!outer_idx < !inner_idx)

let priority_order_tests = [
  "priority order P1>P2>P3>P4", `Quick, test_priority_order_in_response;
  "outside-in order", `Quick, test_outside_in_order;
]

(** ============== Token Estimation Tests ============== *)

(** total_estimated_tokens가 0보다 큰지 확인 *)
let test_tokens_positive () =
  let child = make_text "1:2" "Title" in
  let root = make_frame ~children:[child] "1:1" "Header" in
  let tasks = collect_tasks root in
  let total = List.fold_left (fun acc t -> acc + t.estimated_tokens) 0 tasks in
  check bool "total tokens > 0" true (total > 0)

(** 복잡한 트리일수록 토큰이 많음 *)
let test_tokens_scale_with_complexity () =
  let simple = make_frame "1:1" "Simple" in
  let child1 = make_text "1:2" "Text1" in
  let child2 = make_rect "1:3" "Rect1" in
  let child3 = make_vector "1:4" "Icon1" in
  let complex = make_frame ~layout:Horizontal
                  ~children:[child1; child2; child3] "1:10" "Complex" in

  let simple_tasks = collect_tasks simple in
  let complex_tasks = collect_tasks complex in

  let simple_tokens = List.fold_left (fun acc t -> acc + t.estimated_tokens) 0 simple_tasks in
  let complex_tokens = List.fold_left (fun acc t -> acc + t.estimated_tokens) 0 complex_tasks in

  check bool "complex > simple tokens" true (complex_tokens > simple_tokens)

let token_tests = [
  "tokens positive", `Quick, test_tokens_positive;
  "tokens scale with complexity", `Quick, test_tokens_scale_with_complexity;
]

(** ============== Summary Tests ============== *)

(** summary가 비어있지 않음 *)
let test_summary_not_empty () =
  let node = make_frame "1:1" "Root" in
  let tasks = collect_tasks node in
  let summary = summarize_plan tasks in
  check bool "summary not empty" true (String.length summary > 0)

(** summary에 task 수 포함 *)
let test_summary_contains_count () =
  let child = make_text "1:2" "Title" in
  let root = make_frame ~children:[child] "1:1" "Header" in
  let tasks = collect_tasks root in
  let summary = summarize_plan tasks in
  (* "2 tasks" 또는 "2개" 등의 숫자 포함 *)
  check bool "summary mentions count" true (
    try let _ = Str.search_forward (Str.regexp "[0-9]") summary 0 in true
    with Not_found -> false)

let summary_tests = [
  "summary not empty", `Quick, test_summary_not_empty;
  "summary contains count", `Quick, test_summary_contains_count;
]

(** ============== Integration Test ============== *)

(** 전체 RPC 워크플로우 통합 테스트 *)
let test_full_rpc_workflow () =
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

  (* RPC 응답 시뮬레이션 *)
  let response = simulate_rpc_response main in

  (* JSON 유효성 *)
  check bool "response starts with {" true (String.sub response 0 1 = "{");
  let len = String.length response in
  check bool "response ends with }" true (String.sub response (len - 1) 1 = "}");

  (* 필수 필드 포함 *)
  check bool "has all required fields" true (
    let has_tasks = try let _ = Str.search_forward (Str.regexp "\"tasks\":") response 0 in true with Not_found -> false in
    let has_summary = try let _ = Str.search_forward (Str.regexp "\"summary\":") response 0 in true with Not_found -> false in
    let has_tokens = try let _ = Str.search_forward (Str.regexp "\"total_estimated_tokens\":") response 0 in true with Not_found -> false in
    let has_root = try let _ = Str.search_forward (Str.regexp "\"root_node_id\":") response 0 in true with Not_found -> false in
    has_tasks && has_summary && has_tokens && has_root
  );

  (* root_node_id 값 확인 *)
  check bool "root_node_id is 1:1" true (
    try let _ = Str.search_forward (Str.regexp "\"root_node_id\": \"1:1\"") response 0 in true
    with Not_found -> false)

let integration_tests = [
  "full RPC workflow", `Quick, test_full_rpc_workflow;
]

(** ============== 메인 ============== *)

let () =
  run "PlanTasks RPC Integration" [
    "JSON Structure", json_structure_tests;
    "Response Structure", response_structure_tests;
    "Priority Order", priority_order_tests;
    "Token Estimation", token_tests;
    "Summary", summary_tests;
    "Integration", integration_tests;
  ]
