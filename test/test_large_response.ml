(** Large Response Handler 테스트 *)

open Alcotest

(** ============== Large Response Handler 테스트 ============== *)

let test_small_response_inline () =
  (* 500KB 미만은 인라인 반환 *)
  let small_content = String.make 1000 'x' in (* 1KB *)
  let result = Large_response.wrap_string_result ~prefix:"test" ~format:"fidelity" small_content in
  let open Yojson.Safe.Util in
  let content = result |> member "content" |> to_list in
  let first = List.hd content in
  let result_type = first |> member "type" |> to_string_option in
  check (option string) "small response has type" (Some "text") result_type;
  let text = first |> member "text" |> to_string_option in
  check (option string) "text content preserved" (Some small_content) text

let test_large_response_to_file () =
  (* 500KB 초과는 파일로 저장 *)
  let large_content = String.make 600_000 'y' in (* 600KB *)
  let result = Large_response.wrap_string_result ~prefix:"test_large" ~format:"fidelity" large_content in
  let open Yojson.Safe.Util in
  let status = result |> member "status" |> to_string_option in
  check (option string) "large response status" (Some "large_result") status;
  let file_path = result |> member "file_path" |> to_string_option in
  check bool "file_path exists" true (Option.is_some file_path);
  (* 파일이 실제로 생성되었는지 확인 *)
  let path = Option.get file_path in
  check bool "file exists" true (Sys.file_exists path);
  (* 정리 *)
  Unix.unlink path

let test_json_response_handler () =
  (* JSON 결과도 동일하게 처리 *)
  let json_result = `Assoc [("key", `String "value")] in
  let result = Large_response.wrap_json_result ~prefix:"json_test" ~format:"raw" json_result in
  let open Yojson.Safe.Util in
  (* 작은 JSON은 인라인 *)
  let key = result |> member "key" |> to_string_option in
  check (option string) "json key preserved" (Some "value") key

let test_human_size_formatting () =
  (* 사람이 읽기 쉬운 크기 문자열 *)
  check string "bytes" "500 B" (Large_response.human_size 500);
  check string "kilobytes" "1.0 KB" (Large_response.human_size 1024);
  check string "megabytes" "1.0 MB" (Large_response.human_size (1024 * 1024))

let test_cleanup_old_files () =
  (* TTL 기반 정리 테스트 - 오래된 파일 정리 *)
  Large_response.cleanup_old_files ();
  (* 실제 정리 로직은 TTL 기반이므로 단순히 에러 없이 실행되는지 확인 *)
  check bool "cleanup runs without error" true true

let large_response_tests = [
  "small response inline", `Quick, test_small_response_inline;
  "large response to file", `Quick, test_large_response_to_file;
  "json response handler", `Quick, test_json_response_handler;
  "human size formatting", `Quick, test_human_size_formatting;
  "cleanup old files", `Quick, test_cleanup_old_files;
]

(** ============== Progressive Loading 테스트 ============== *)

(* 노드 요약 구조 테스트를 위한 mock 데이터 *)
let mock_node_json = {|
{
  "nodes": {
    "123_456": {
      "document": {
        "id": "123:456",
        "name": "TestFrame",
        "type": "FRAME",
        "children": [
          {"id": "123:457", "name": "Child1", "type": "TEXT", "children": []},
          {"id": "123:458", "name": "Child2", "type": "RECTANGLE", "children": [
            {"id": "123:459", "name": "GrandChild", "type": "ELLIPSE", "children": []}
          ]}
        ]
      }
    }
  }
}
|}

let test_depth_filter_basic () =
  (* 깊이 필터링 기본 테스트 *)
  let json = Yojson.Safe.from_string mock_node_json in
  let open Yojson.Safe.Util in
  let node_data = json |> member "nodes" |> member "123_456" |> member "document" in

  (* depth 0만 가져오면 자식이 truncated 되어야 함 *)
  let rec filter_depth current max_depth node =
    if current >= max_depth then
      let assoc = to_assoc node in
      let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
      let children_count = node |> member "children" |> to_list |> List.length in
      `Assoc (without_children @ [("_truncated_children", `Int children_count)])
    else
      let children = node |> member "children" |> to_list in
      let filtered = List.map (fun c -> filter_depth (current + 1) max_depth c) children in
      let assoc = to_assoc node in
      let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
      `Assoc (without_children @ [("children", `List filtered)])
  in

  let filtered = filter_depth 0 1 node_data in
  let truncated = filtered |> member "children" |> to_list |> List.hd |> member "_truncated_children" in
  check bool "children truncated at depth 1" true (truncated <> `Null)

let test_summary_structure () =
  (* Summary 응답 구조 테스트 *)
  let summary = `Assoc [
    ("node_id", `String "123:456");
    ("name", `String "TestFrame");
    ("type", `String "FRAME");
    ("children_count", `Int 2);
    ("children", `List [
      `Assoc [("id", `String "123:457"); ("name", `String "Child1"); ("type", `String "TEXT"); ("children_count", `Int 0)];
      `Assoc [("id", `String "123:458"); ("name", `String "Child2"); ("type", `String "RECTANGLE"); ("children_count", `Int 1)];
    ]);
    ("truncated", `Bool false);
  ] in
  let open Yojson.Safe.Util in
  check string "node_id" "123:456" (summary |> member "node_id" |> to_string);
  check int "children_count" 2 (summary |> member "children_count" |> to_int);
  check int "children list length" 2 (summary |> member "children" |> to_list |> List.length)

let progressive_loading_tests = [
  "depth filter basic", `Quick, test_depth_filter_basic;
  "summary structure", `Quick, test_summary_structure;
]

(** ============== Memoization 벤치마크 테스트 ============== *)

(** 반복 색상이 많은 대형 노드 트리 생성 - default_node 확장 *)
let make_large_node_tree () : Figma_types.ui_node =
  let open Figma_types in
  let blue_fill = {
    paint_type = Solid; visible = true; opacity = 1.;
    color = Some { r = 0.; g = 0.478; b = 1.; a = 1. };  (* #007AFF - 반복 *)
    gradient_stops = []; image_ref = None; scale_mode = None;
  } in
  let white_fill = {
    paint_type = Solid; visible = true; opacity = 1.;
    color = Some { r = 1.; g = 1.; b = 1.; a = 1. };
    gradient_stops = []; image_ref = None; scale_mode = None;
  } in
  let typo = {
    font_family = "Inter"; font_size = 16.; font_weight = 500;
    font_style = "normal"; line_height = Some 24.; letter_spacing = Some 0.;
    text_align_h = Left; text_align_v = Top;
    text_decoration = NoDeco; text_case = Original;
  } in
  let make_child i : ui_node = { default_node with
    id = Printf.sprintf "child_%d" i;
    name = Printf.sprintf "Item %d" i;
    typography = Some typo;
    fills = [blue_fill];
    bbox = Some { x = 0.; y = float_of_int (i * 50); width = 320.; height = 48. };
    layout_mode = Horizontal;
    layout_sizing_h = Fill;
    gap = 12.;
    padding = (16., 16., 16., 16.);
    primary_axis_align = Center;
    counter_axis_align = Center;
    border_radius = 8.;
  } in
  { default_node with
    id = "root";
    name = "Container";
    fills = [white_fill];
    bbox = Some { x = 0.; y = 0.; width = 375.; height = 812. };
    gap = 8.;
    padding = (16., 16., 16., 16.);
    children = List.init 30 make_child;  (* 30개의 동일한 스타일 자식 *)
  }

let test_memoization_token_savings () =
  let node = make_large_node_tree () in

  (* 기존 방식 *)
  let dsl_normal = Figma_codegen.generate_compact node in
  let len_normal = String.length dsl_normal in

  (* Memoization 방식 *)
  let dsl_memo = Figma_codegen.generate_compact_memoized ~threshold:2 node in
  let len_memo = String.length dsl_memo in

  (* 결과 출력 *)
  Printf.printf "\n=== Memoization 벤치마크 ===\n";
  Printf.printf "노드 수: 31 (root + 30 children)\n";
  Printf.printf "Normal DSL: %d chars\n" len_normal;
  Printf.printf "Memoized DSL: %d chars\n" len_memo;
  Printf.printf "절약: %d chars (%.1f%%)\n" (len_normal - len_memo)
    (float_of_int (len_normal - len_memo) /. float_of_int len_normal *. 100.);

  (* @vars 블록이 생성되었는지 확인 *)
  let has_vars = String.length dsl_memo >= 5 && String.sub dsl_memo 0 5 = "@vars" in
  check bool "memoized has @vars block" true has_vars;

  (* Memoized가 같거나 짧아야 함 (변수 정의 오버헤드로 조금 늘어날 수 있음) *)
  check bool "memoization reduces or similar size" true (len_memo <= len_normal + 100)

let test_smart_compact_threshold () =
  (* 작은 트리는 memoization 없이 - default_node 확장 *)
  let small_node = { Figma_types.default_node with
    id = "small"; name = "Small";
    bbox = Some { x = 0.; y = 0.; width = 100.; height = 100. };
  } in
  let dsl_smart = Figma_codegen.generate_compact_smart small_node in
  (* 작은 노드는 @vars 없이 *)
  let has_no_vars = String.length dsl_smart < 5 || String.sub dsl_smart 0 5 <> "@vars" in
  check bool "small tree has no @vars" true has_no_vars

let memoization_tests = [
  "memoization token savings", `Quick, test_memoization_token_savings;
  "smart compact threshold", `Quick, test_smart_compact_threshold;
]

(** ============== Warning System 테스트 ============== *)

let test_warning_level_info () =
  (* 50개 이하는 Info *)
  let analysis = Large_response.analyze_node_count 30 in
  check int "small tree nodes" 30 analysis.total_nodes;
  check int "estimated tokens" 1500 analysis.estimated_tokens;
  check bool "warning level is info" true (analysis.level = Large_response.Info);
  check int "no recommendations for small tree" 0 (List.length analysis.recommendations)

let test_warning_level_caution () =
  (* 51~200개는 Caution *)
  let analysis = Large_response.analyze_node_count 100 in
  check int "medium tree nodes" 100 analysis.total_nodes;
  check bool "warning level is caution" true (analysis.level = Large_response.Caution);
  check bool "has memoization recommendation" true (List.length analysis.recommendations >= 1)

let test_warning_level_warning () =
  (* 201~500개는 Warning *)
  let analysis = Large_response.analyze_node_count 300 in
  check bool "warning level is warning" true (analysis.level = Large_response.Warning);
  check bool "has progressive loading recommendation" true (List.length analysis.recommendations >= 2)

let test_warning_level_critical () =
  (* 501개 이상은 Critical *)
  let analysis = Large_response.analyze_node_count 1000 in
  check bool "warning level is critical" true (analysis.level = Large_response.Critical);
  check bool "has chunking recommendation" true (List.length analysis.recommendations >= 2)

let test_count_nodes_json () =
  (* JSON에서 노드 수 세기 *)
  let json = Yojson.Safe.from_string {|
    {
      "id": "root",
      "children": [
        {"id": "child1", "children": []},
        {"id": "child2", "children": [
          {"id": "grandchild", "children": []}
        ]}
      ]
    }
  |} in
  let count = Large_response.count_nodes_json json in
  check int "count nodes in tree" 4 count  (* root + child1 + child2 + grandchild *)

let test_analysis_to_json () =
  (* 분석 결과 JSON 변환 *)
  let analysis = Large_response.analyze_node_count 600 in
  let json = Large_response.analysis_to_json analysis in
  let open Yojson.Safe.Util in
  check int "json total_nodes" 600 (json |> member "total_nodes" |> to_int);
  check string "json warning_level" "critical" (json |> member "warning_level" |> to_string);
  check bool "json has recommendations" true (json |> member "recommendations" |> to_list |> List.length > 0)

let test_add_warning_to_response () =
  (* 응답에 경고 추가 *)
  let response = `Assoc [("data", `String "test")] in
  let with_warning = Large_response.add_warning_to_response ~node_count:300 response in
  let open Yojson.Safe.Util in
  let has_warning = with_warning |> member "_warning" <> `Null in
  check bool "warning added for large response" true has_warning;
  let data = with_warning |> member "data" |> to_string in
  check string "original data preserved" "test" data

let test_no_warning_for_small () =
  (* 작은 응답에는 경고 없음 *)
  let response = `Assoc [("data", `String "small")] in
  let with_warning = Large_response.add_warning_to_response ~node_count:30 response in
  let open Yojson.Safe.Util in
  let has_warning = with_warning |> member "_warning" <> `Null in
  check bool "no warning for small response" false has_warning

let warning_system_tests = [
  "warning level info", `Quick, test_warning_level_info;
  "warning level caution", `Quick, test_warning_level_caution;
  "warning level warning", `Quick, test_warning_level_warning;
  "warning level critical", `Quick, test_warning_level_critical;
  "count nodes json", `Quick, test_count_nodes_json;
  "analysis to json", `Quick, test_analysis_to_json;
  "add warning to response", `Quick, test_add_warning_to_response;
  "no warning for small", `Quick, test_no_warning_for_small;
]

(** ============== 메인 실행 ============== *)

let () =
  run "Large Response & Progressive Loading" [
    "Large Response Handler", large_response_tests;
    "Progressive Loading", progressive_loading_tests;
    "Memoization", memoization_tests;
    "Warning System", warning_system_tests;
  ]
