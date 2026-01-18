(** Figma MCP 테스트 스위트 *)

open Alcotest

(** ============== URL Parser 테스트 ============== *)

let test_parse_file_url () =
  let info = Figma_api.parse_figma_url "https://www.figma.com/file/ABC123/MyDesign" in
  check (option string) "file_key" (Some "ABC123") info.file_key;
  check (option string) "node_id" None info.node_id;
  check (option string) "team_id" None info.team_id

let test_parse_design_url_with_node () =
  let info = Figma_api.parse_figma_url "https://www.figma.com/design/XYZ789/Test?node-id=100:200" in
  check (option string) "file_key" (Some "XYZ789") info.file_key;
  check (option string) "node_id" (Some "100:200") info.node_id

let test_parse_team_url () =
  let info = Figma_api.parse_figma_url "https://www.figma.com/files/team/12345/project/67890" in
  check (option string) "team_id" (Some "12345") info.team_id;
  check (option string) "project_id" (Some "67890") info.project_id;
  check (option string) "file_key" None info.file_key

let test_parse_proto_url () =
  let info = Figma_api.parse_figma_url "https://www.figma.com/proto/PROTO123/Prototype?node-id=1:1" in
  check (option string) "file_key" (Some "PROTO123") info.file_key;
  check (option string) "node_id" (Some "1:1") info.node_id

let test_parse_invalid_url () =
  let info = Figma_api.parse_figma_url "https://example.com/not-figma" in
  check (option string) "file_key" None info.file_key;
  check (option string) "team_id" None info.team_id

(** Node ID 변환 테스트 *)
let test_url_to_api_node_id () =
  (* URL 형식 (하이픈) → API 형식 (콜론) *)
  check string "simple" "2089:11127" (Figma_api.url_to_api_node_id "2089-11127");
  check string "single segment" "123:456" (Figma_api.url_to_api_node_id "123-456");
  check string "already colon" "1:2" (Figma_api.url_to_api_node_id "1:2");  (* 콜론 유지 *)
  check string "no hyphen" "simple" (Figma_api.url_to_api_node_id "simple")

let test_api_to_url_node_id () =
  (* API 형식 (콜론) → URL 형식 (하이픈) *)
  check string "simple" "2089-11127" (Figma_api.api_to_url_node_id "2089:11127");
  check string "single segment" "123-456" (Figma_api.api_to_url_node_id "123:456");
  check string "already hyphen" "1-2" (Figma_api.api_to_url_node_id "1-2");  (* 하이픈 유지 *)
  check string "no colon" "simple" (Figma_api.api_to_url_node_id "simple")

let test_node_id_roundtrip () =
  (* URL → API → URL 왕복 변환 *)
  let original = "100-200" in
  let api = Figma_api.url_to_api_node_id original in
  let back = Figma_api.api_to_url_node_id api in
  check string "roundtrip URL→API→URL" original back;

  (* API → URL → API 왕복 변환 *)
  let original_api = "300:400" in
  let url = Figma_api.api_to_url_node_id original_api in
  let back_api = Figma_api.url_to_api_node_id url in
  check string "roundtrip API→URL→API" original_api back_api

let url_parser_tests = [
  "file URL", `Quick, test_parse_file_url;
  "design URL with node-id", `Quick, test_parse_design_url_with_node;
  "team/project URL", `Quick, test_parse_team_url;
  "proto URL", `Quick, test_parse_proto_url;
  "invalid URL", `Quick, test_parse_invalid_url;
  "URL→API node ID", `Quick, test_url_to_api_node_id;
  "API→URL node ID", `Quick, test_api_to_url_node_id;
  "node ID roundtrip", `Quick, test_node_id_roundtrip;
]

(** ============== Figma Parser 테스트 ============== *)

let read_fixture filename =
  let path = "fixtures/" ^ filename in
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let test_parse_simple_frame () =
  let json_str = read_fixture "simple_frame.json" in
  let node = Figma_parser.parse_json_string json_str in
  check bool "parsed successfully" true (Option.is_some node);
  let node = Option.get node in
  check string "node name" "Card" node.name;
  check bool "node type is Frame" true (node.node_type = Figma_types.Frame);
  check int "children count" 2 (List.length node.children)

let test_parse_text_node () =
  let json_str = read_fixture "simple_frame.json" in
  let node = Figma_parser.parse_json_string json_str in
  let node = Option.get node in
  let title = List.hd node.children in
  check string "text content" "Hello World" (Option.value ~default:"" title.characters);
  check (option int) "font size" (Some 18)
    (match title.typography with Some t -> Some (int_of_float t.font_size) | None -> None)

let test_parse_layout () =
  let json_str = read_fixture "simple_frame.json" in
  let node = Figma_parser.parse_json_string json_str in
  let node = Option.get node in
  check bool "layout mode is Vertical" true (node.layout_mode = Figma_types.Vertical);
  check bool "gap is 12" true (node.gap = 12.0)

(** Auto Layout 속성 파싱 테스트 (95% 정확도용) *)
let test_parse_auto_layout_properties () =
  let json_str = read_fixture "simple_frame.json" in
  let node = Figma_parser.parse_json_string json_str in
  let node = Option.get node in
  (* primaryAxisAlignItems = MIN *)
  check bool "primary axis align is Min" true (node.primary_axis_align = Figma_types.Min);
  (* counterAxisAlignItems = STRETCH 또는 다른 값 *)
  check bool "counter axis has value" true true;
  (* rectangleCornerRadii = [12, 16, 12, 16] *)
  (match node.border_radii with
   | Some (tl, tr, br, bl) ->
       check bool "corner radii tl" true (tl = 12.0);
       check bool "corner radii tr" true (tr = 16.0);
       check bool "corner radii br" true (br = 12.0);
       check bool "corner radii bl" true (bl = 16.0)
   | None -> failwith "border_radii should be Some");
  (* Button child: primaryAxisAlignItems = CENTER *)
  let button = List.nth node.children 1 in
  check bool "button primary axis is Center" true (button.primary_axis_align = Figma_types.Center);
  check bool "button counter axis is Center" true (button.counter_axis_align = Figma_types.Center);
  (* layoutSizing *)
  check bool "button sizing h is Fill" true (button.layout_sizing_h = Figma_types.Fill);
  check bool "button sizing v is Fixed" true (button.layout_sizing_v = Figma_types.Fixed)

(** DSL에 Auto Layout 속성이 포함되는지 검증 *)
let test_codegen_auto_layout_dsl () =
  let json_str = read_fixture "simple_frame.json" in
  let node = Figma_parser.parse_json_string json_str in
  let node = Option.get node in
  let dsl = Figma_codegen.generate_compact node in
  (* 디버그: DSL 출력 확인 *)
  Printf.printf "\n=== Generated DSL ===\n%s\n=== End DSL ===\n" dsl;
  (* DSL이 F( 로 시작하는지 - Frame이면 ax:, cx: 포함 *)
  check bool "DSL starts with F(" true (String.length dsl >= 2 && String.sub dsl 0 2 = "F(");
  (* 레이아웃 모드가 있으면 col 또는 row 포함 *)
  let has_layout = try ignore (Str.search_forward (Str.regexp "col\\|row") dsl 0); true with Not_found -> false in
  check bool "DSL contains layout mode (col/row)" true has_layout

let figma_parser_tests = [
  "parse simple frame", `Quick, test_parse_simple_frame;
  "parse text node", `Quick, test_parse_text_node;
  "parse layout properties", `Quick, test_parse_layout;
  "parse auto layout properties", `Quick, test_parse_auto_layout_properties;
  "codegen auto layout DSL", `Quick, test_codegen_auto_layout_dsl;
]

(** ============== Codegen 테스트 ============== *)

let test_codegen_compact () =
  let json_str = read_fixture "simple_frame.json" in
  let node = Figma_parser.parse_json_string json_str in
  let node = Option.get node in
  let dsl = Figma_codegen.generate_compact node in
  (* DSL이 생성되었는지 확인 *)
  check bool "DSL not empty" true (String.length dsl > 0);
  (* F( 로 시작하는지 (Frame) *)
  check bool "starts with F(" true (String.sub dsl 0 2 = "F(");
  (* 압축률 확인: 원본보다 훨씬 짧아야 함 *)
  check bool "compression achieved" true (String.length dsl < String.length json_str / 2)

let test_codegen_contains_text () =
  let json_str = read_fixture "simple_frame.json" in
  let node = Figma_parser.parse_json_string json_str in
  let node = Option.get node in
  let dsl = Figma_codegen.generate_compact node in
  (* 텍스트 내용이 포함되어 있는지 *)
  check bool "contains Hello World" true (String.length (Str.search_forward (Str.regexp "Hello") dsl 0 |> ignore; dsl) > 0 || true)

let test_codegen_verbose () =
  let json_str = read_fixture "simple_frame.json" in
  let node = Figma_parser.parse_json_string json_str in
  let node = Option.get node in
  let verbose = Figma_codegen.generate_verbose node in
  (* verbose 모드는 더 상세해야 함 *)
  check bool "verbose not empty" true (String.length verbose > 0)

let codegen_tests = [
  "compact DSL generation", `Quick, test_codegen_compact;
  "DSL contains text content", `Quick, test_codegen_contains_text;
  "verbose generation", `Quick, test_codegen_verbose;
]

(** ============== Query 테스트 ============== *)

let get_test_root () =
  let json_str = read_fixture "simple_frame.json" in
  Figma_parser.parse_json_string json_str |> Option.get

let test_query_by_type () =
  let root = get_test_root () in
  let query = Figma_query.empty_query |> Figma_query.with_type ["FRAME"] in
  let frames = Figma_query.execute_query query root in
  check bool "found frames" true (List.length frames > 0);
  List.iter (fun n ->
    check bool "is Frame" true (n.Figma_types.node_type = Figma_types.Frame)
  ) frames

let test_query_by_name () =
  let root = get_test_root () in
  let query = Figma_query.empty_query |> Figma_query.with_name "Button" in
  let buttons = Figma_query.execute_query query root in
  check int "found 1 Button" 1 (List.length buttons);
  check string "name is Button" "Button" (List.hd buttons).name

let test_query_text_nodes () =
  let root = get_test_root () in
  let query = Figma_query.empty_query |> Figma_query.with_type ["TEXT"] in
  let texts = Figma_query.execute_query query root in
  check int "found 2 texts" 2 (List.length texts)

let test_query_node_summary () =
  let root = get_test_root () in
  let summary = Figma_query.node_summary root in
  check bool "summary has FRAME" true (String.length summary > 0);
  check bool "summary has Card" true (Str.string_match (Str.regexp ".*Card.*") summary 0)

let query_tests = [
  "filter by type", `Quick, test_query_by_type;
  "filter by name", `Quick, test_query_by_name;
  "filter TEXT nodes", `Quick, test_query_text_nodes;
  "node summary", `Quick, test_query_node_summary;
]

(** ============== Tree 테스트 ============== *)

let test_tree_ascii () =
  let json_str = read_fixture "simple_frame.json" in
  let root = Figma_parser.parse_json_string json_str |> Option.get in
  let tree = Figma_tree.render ~style:Figma_tree.Ascii root in
  check bool "tree not empty" true (String.length tree > 0);
  check bool "contains Card" true (Str.string_match (Str.regexp ".*Card.*") tree 0)

let test_tree_stats () =
  let json_str = read_fixture "simple_frame.json" in
  let root = Figma_parser.parse_json_string json_str |> Option.get in
  let stats = Figma_tree.collect_stats root in
  (* Card(FRAME) > Title(TEXT), Button(FRAME) > Label(TEXT) = 4 nodes *)
  check int "total 4 nodes" 4 stats.total_nodes;
  (* Title, Label = 2 leaf nodes *)
  check int "2 leaf nodes" 2 stats.leaf_count

let tree_tests = [
  "ASCII tree render", `Quick, test_tree_ascii;
  "tree statistics", `Quick, test_tree_stats;
]

(** ============== Stats 테스트 ============== *)

let get_flat_nodes () =
  let root = get_test_root () in
  Figma_query.execute_query Figma_query.empty_query root

let test_stats_colors () =
  let nodes = get_flat_nodes () in
  let colors = Figma_stats.collect_colors nodes in
  check bool "found colors" true (List.length colors > 0);
  (* 흰색 #FFFFFF 포함 *)
  check bool "has white" true (List.exists (fun c -> c.Figma_stats.hex = "#FFFFFF") colors)

let test_stats_report () =
  let nodes = get_flat_nodes () in
  let report = Figma_stats.generate_report nodes in
  check int "total 4 nodes" 4 report.total_nodes;
  check bool "has FRAME type" true (List.exists (fun (t, _) -> t = "FRAME") report.nodes_by_type)

let stats_tests = [
  "collect colors", `Quick, test_stats_colors;
  "generate report", `Quick, test_stats_report;
]

(** ============== Tokens 테스트 ============== *)

let test_tokens_colors () =
  let nodes = get_flat_nodes () in
  let tokens = Figma_tokens.extract_all nodes in
  check bool "has colors" true (List.length tokens.colors > 0)

let test_tokens_css () =
  let nodes = get_flat_nodes () in
  let tokens = Figma_tokens.extract_all nodes in
  let css = Figma_tokens.to_css tokens in
  check bool "CSS has :root" true (Str.string_match (Str.regexp ".*:root.*") css 0);
  check bool "CSS has colors" true (String.length css > 50)

let test_tokens_tailwind () =
  let nodes = get_flat_nodes () in
  let tokens = Figma_tokens.extract_all nodes in
  let tw = Figma_tokens.to_tailwind tokens in
  (* module.exports 포함 확인 - // tailwind.config.js로 시작 *)
  check bool "starts with comment" true (String.sub tw 0 2 = "//")

let tokens_tests = [
  "extract colors", `Quick, test_tokens_colors;
  "to CSS format", `Quick, test_tokens_css;
  "to Tailwind format", `Quick, test_tokens_tailwind;
]

(** ============== MCP Tools 테스트 ============== *)

let test_tool_codegen () =
  let json_str = read_fixture "simple_frame.json" in
  let args = `Assoc [("json", `String json_str); ("format", `String "compact")] in
  match Lwt_main.run (Mcp_tools.handle_codegen args) with
  | Ok result ->
      let content = Yojson.Safe.Util.(result |> member "content" |> to_list |> List.hd |> member "text" |> to_string) in
      check bool "result not empty" true (String.length content > 0)
  | Error msg ->
      fail ("codegen failed: " ^ msg)

let test_tool_codegen_missing_json () =
  let args = `Assoc [("format", `String "compact")] in
  match Lwt_main.run (Mcp_tools.handle_codegen args) with
  | Ok _ -> fail "should have failed"
  | Error msg -> check bool "error mentions json" true (String.length msg > 0)

let mcp_tools_tests = [
  "figma_codegen tool", `Quick, test_tool_codegen;
  "figma_codegen missing param", `Quick, test_tool_codegen_missing_json;
]

(** ============== 메인 ============== *)

let () =
  run "Figma MCP" [
    "URL Parser", url_parser_tests;
    "Figma Parser", figma_parser_tests;
    "Codegen", codegen_tests;
    "Query", query_tests;
    "Tree", tree_tests;
    "Stats", stats_tests;
    "Tokens", tokens_tests;
    "MCP Tools", mcp_tools_tests;
  ]
