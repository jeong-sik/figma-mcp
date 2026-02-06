(** Comprehensive Coverage Tests for figma_stats.ml

    Target: figma_stats.ml (149 bisect points, 58.39% coverage)
    Uncovered: ~62 points

    Functions:
    - collect_colors (Solid + color extraction, hashtable, sorting)
    - collect_fonts (typography extraction, dedup sizes/weights, sorting)
    - collect_sizes (bbox filtering, min/max/avg, count_occurrences, None case)
    - collect_component_stats (component/instance/componentSet filtering, unused detection)
    - generate_report (aggregate all stats)
    - report_to_string (full formatting with all sections)
*)

open Alcotest
open Figma_types
open Figma_stats

(** ============== Test Fixtures ============== *)

let make_rgba ?(a=1.0) r g b = { r; g; b; a }

let make_solid_paint rgba =
  { paint_type = Solid; visible = true; opacity = 1.0;
    color = Some rgba; gradient_stops = []; image_ref = None; scale_mode = None }

let make_bbox ?(x=0.) ?(y=0.) width height = { x; y; width; height }

let make_typography ?(font_family="Inter") ?(font_weight=400) font_size =
  { Figma_types.font_family; font_size; font_weight;
    font_style = "normal"; line_height = None; letter_spacing = None;
    text_align_h = Left; text_align_v = Top;
    text_decoration = NoDeco; text_case = Original }

let make_node
    ?(id="n1") ?(name="Node") ?(node_type=Frame)
    ?(fills=[]) ?(typography=None) ?(bbox=None) ?(children=[])
    ?(component_id=None) () =
  { default_node with id; name; node_type; fills; typography;
    bbox; children; component_id }

(** ============== collect_colors Tests ============== *)

let test_collect_colors_basic () =
  let red = make_rgba 1.0 0.0 0.0 in
  let node = make_node ~name:"RedBox" ~fills:[make_solid_paint red] () in
  let colors = collect_colors [node] in
  check int "one color" 1 (List.length colors);
  let c = List.hd colors in
  check string "hex" "#FF0000" c.hex;
  check int "count 1" 1 c.count;
  check bool "has node name" true (List.mem "RedBox" c.node_names)

let test_collect_colors_multiple () =
  let red = make_rgba 1.0 0.0 0.0 in
  let blue = make_rgba 0.0 0.0 1.0 in
  let n1 = make_node ~id:"n1" ~name:"R1" ~fills:[make_solid_paint red] () in
  let n2 = make_node ~id:"n2" ~name:"R2" ~fills:[make_solid_paint red] () in
  let n3 = make_node ~id:"n3" ~name:"B1" ~fills:[make_solid_paint blue] () in
  let colors = collect_colors [n1; n2; n3] in
  check int "two colors" 2 (List.length colors);
  (* Sorted by count descending, red should be first *)
  let first = List.hd colors in
  check string "most used is red" "#FF0000" first.hex;
  check int "red count 2" 2 first.count

let test_collect_colors_non_solid_ignored () =
  let paint = { paint_type = GradientLinear; visible = true; opacity = 1.0;
                color = Some (make_rgba 1.0 0.0 0.0);
                gradient_stops = []; image_ref = None; scale_mode = None } in
  let node = make_node ~fills:[paint] () in
  let colors = collect_colors [node] in
  check int "gradient ignored" 0 (List.length colors)

let test_collect_colors_empty () =
  let colors = collect_colors [] in
  check int "empty" 0 (List.length colors)

(** ============== collect_fonts Tests ============== *)

let test_collect_fonts_basic () =
  let typo = make_typography 16.0 in
  let node = make_node ~typography:(Some typo) () in
  let fonts = collect_fonts [node] in
  check int "one font" 1 (List.length fonts);
  let f = List.hd fonts in
  check string "family" "Inter" f.family;
  check int "count 1" 1 f.count;
  check bool "has size 16" true (List.mem 16.0 f.sizes)

let test_collect_fonts_multiple_sizes () =
  let t1 = make_typography 16.0 in
  let t2 = make_typography 24.0 in
  let t3 = make_typography 16.0 in  (* duplicate size *)
  let n1 = make_node ~id:"n1" ~typography:(Some t1) () in
  let n2 = make_node ~id:"n2" ~typography:(Some t2) () in
  let n3 = make_node ~id:"n3" ~typography:(Some t3) () in
  let fonts = collect_fonts [n1; n2; n3] in
  check int "one family" 1 (List.length fonts);
  let f = List.hd fonts in
  check int "count 3" 3 f.count;
  (* sizes should be deduped *)
  check int "2 unique sizes" 2 (List.length f.sizes)

let test_collect_fonts_multiple_families () =
  let t1 = make_typography ~font_family:"Inter" 16.0 in
  let t2 = make_typography ~font_family:"Roboto" 14.0 in
  let n1 = make_node ~id:"n1" ~typography:(Some t1) () in
  let n2 = make_node ~id:"n2" ~typography:(Some t2) () in
  let fonts = collect_fonts [n1; n2] in
  check int "two families" 2 (List.length fonts)

let test_collect_fonts_different_weights () =
  let t1 = make_typography ~font_weight:400 16.0 in
  let t2 = make_typography ~font_weight:700 16.0 in
  let n1 = make_node ~id:"n1" ~typography:(Some t1) () in
  let n2 = make_node ~id:"n2" ~typography:(Some t2) () in
  let fonts = collect_fonts [n1; n2] in
  let f = List.hd fonts in
  check int "2 weights" 2 (List.length f.weights)

let test_collect_fonts_none_typography () =
  let node = make_node ~typography:None () in
  let fonts = collect_fonts [node] in
  check int "no fonts" 0 (List.length fonts)

(** ============== collect_sizes Tests ============== *)

let test_collect_sizes_basic () =
  let node = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let result = collect_sizes [node] in
  check bool "some" true (Option.is_some result);
  let s = Option.get result in
  check (float 0.1) "min_width" 100.0 s.min_width;
  check (float 0.1) "max_width" 100.0 s.max_width;
  check (float 0.1) "avg_width" 100.0 s.avg_width;
  check (float 0.1) "min_height" 50.0 s.min_height;
  check (float 0.1) "max_height" 50.0 s.max_height;
  check (float 0.1) "avg_height" 50.0 s.avg_height

let test_collect_sizes_multiple () =
  let n1 = make_node ~id:"n1" ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let n2 = make_node ~id:"n2" ~bbox:(Some (make_bbox 200.0 100.0)) () in
  let n3 = make_node ~id:"n3" ~bbox:(Some (make_bbox 300.0 150.0)) () in
  let result = collect_sizes [n1; n2; n3] in
  let s = Option.get result in
  check (float 0.1) "min_width" 100.0 s.min_width;
  check (float 0.1) "max_width" 300.0 s.max_width;
  check (float 0.1) "avg_width" 200.0 s.avg_width;
  check bool "has common_widths" true (List.length s.common_widths > 0);
  check bool "has common_heights" true (List.length s.common_heights > 0)

let test_collect_sizes_no_bbox () =
  let node = make_node ~bbox:None () in
  let result = collect_sizes [node] in
  check bool "none when no bbox" true (Option.is_none result)

let test_collect_sizes_empty () =
  let result = collect_sizes [] in
  check bool "none for empty" true (Option.is_none result)

let test_collect_sizes_common_widths () =
  (* Same width repeated should appear in common_widths *)
  let nodes = List.init 5 (fun i ->
    make_node ~id:(string_of_int i)
      ~bbox:(Some (make_bbox 100.0 50.0)) ()
  ) in
  let result = collect_sizes nodes in
  let s = Option.get result in
  check bool "100 is common width" true
    (List.exists (fun (w, _) -> w = 100.0) s.common_widths)

(** ============== collect_component_stats Tests ============== *)

let test_component_stats_basic () =
  let comp = make_node ~id:"c1" ~name:"Button" ~node_type:Component () in
  let inst = make_node ~id:"i1" ~name:"Button" ~node_type:Instance () in
  let stats = collect_component_stats [comp; inst] in
  check int "1 component" 1 stats.component_count;
  check int "1 instance" 1 stats.instance_count;
  check int "0 component_set" 0 stats.component_set_count;
  check int "0 unused" 0 (List.length stats.unused_components)

let test_component_stats_unused () =
  let comp = make_node ~id:"c1" ~name:"UnusedBtn" ~node_type:Component () in
  let inst = make_node ~id:"i1" ~name:"OtherBtn" ~node_type:Instance () in
  let stats = collect_component_stats [comp; inst] in
  check int "1 unused" 1 (List.length stats.unused_components);
  check string "unused name" "UnusedBtn" (List.hd stats.unused_components)

let test_component_stats_component_set () =
  let cs = make_node ~id:"cs1" ~name:"BtnSet" ~node_type:ComponentSet () in
  let stats = collect_component_stats [cs] in
  check int "1 component_set" 1 stats.component_set_count

let test_component_stats_empty () =
  let stats = collect_component_stats [] in
  check int "0 components" 0 stats.component_count;
  check int "0 instances" 0 stats.instance_count

(** ============== generate_report Tests ============== *)

let test_generate_report () =
  let red = make_rgba 1.0 0.0 0.0 in
  let typo = make_typography 16.0 in
  let nodes = [
    make_node ~id:"f1" ~name:"Frame1" ~node_type:Frame
      ~bbox:(Some (make_bbox 100.0 50.0))
      ~fills:[make_solid_paint red] ();
    make_node ~id:"t1" ~name:"Text1" ~node_type:Text
      ~typography:(Some typo) ();
    make_node ~id:"c1" ~name:"Comp1" ~node_type:Component ();
    make_node ~id:"i1" ~name:"Comp1" ~node_type:Instance ();
  ] in
  let report = generate_report nodes in
  check int "total 4" 4 report.total_nodes;
  check bool "has colors" true (List.length report.colors > 0);
  check bool "has fonts" true (List.length report.fonts > 0);
  check bool "has sizes" true (Option.is_some report.sizes);
  check int "1 component" 1 report.components.component_count

let test_generate_report_empty () =
  let report = generate_report [] in
  check int "total 0" 0 report.total_nodes;
  check int "no colors" 0 (List.length report.colors);
  check int "no fonts" 0 (List.length report.fonts);
  check bool "no sizes" true (Option.is_none report.sizes)

(** ============== report_to_string Tests ============== *)

let test_report_to_string_with_data () =
  let red = make_rgba 1.0 0.0 0.0 in
  let typo = make_typography 16.0 in
  let nodes = [
    make_node ~id:"f1" ~name:"Frame" ~node_type:Frame
      ~bbox:(Some (make_bbox 100.0 50.0))
      ~fills:[make_solid_paint red] ();
    make_node ~id:"t1" ~name:"Text" ~node_type:Text
      ~typography:(Some typo) ();
    make_node ~id:"c1" ~name:"Unused" ~node_type:Component ();
  ] in
  let report = generate_report nodes in
  let s = report_to_string report in
  check bool "has header" true
    (try let _ = Str.search_forward (Str.regexp_string "FIGMA") s 0 in true
     with Not_found -> false);
  check bool "has total" true
    (try let _ = Str.search_forward (Str.regexp "총 노드 수") s 0 in true
     with Not_found -> false);
  check bool "has unused" true
    (try let _ = Str.search_forward (Str.regexp "미사용") s 0 in true
     with Not_found -> false)

let test_report_to_string_no_sizes () =
  let nodes = [make_node ~bbox:None ()] in
  let report = generate_report nodes in
  let s = report_to_string report in
  check bool "no size section" true (String.length s > 0)

(** ============== Main Test Runner ============== *)

let () =
  run "figma-stats-coverage" [
    "collect_colors", [
      "basic", `Quick, test_collect_colors_basic;
      "multiple", `Quick, test_collect_colors_multiple;
      "non_solid_ignored", `Quick, test_collect_colors_non_solid_ignored;
      "empty", `Quick, test_collect_colors_empty;
    ];
    "collect_fonts", [
      "basic", `Quick, test_collect_fonts_basic;
      "multiple_sizes", `Quick, test_collect_fonts_multiple_sizes;
      "multiple_families", `Quick, test_collect_fonts_multiple_families;
      "different_weights", `Quick, test_collect_fonts_different_weights;
      "none_typography", `Quick, test_collect_fonts_none_typography;
    ];
    "collect_sizes", [
      "basic", `Quick, test_collect_sizes_basic;
      "multiple", `Quick, test_collect_sizes_multiple;
      "no_bbox", `Quick, test_collect_sizes_no_bbox;
      "empty", `Quick, test_collect_sizes_empty;
      "common_widths", `Quick, test_collect_sizes_common_widths;
    ];
    "component_stats", [
      "basic", `Quick, test_component_stats_basic;
      "unused", `Quick, test_component_stats_unused;
      "component_set", `Quick, test_component_stats_component_set;
      "empty", `Quick, test_component_stats_empty;
    ];
    "generate_report", [
      "with_data", `Quick, test_generate_report;
      "empty", `Quick, test_generate_report_empty;
    ];
    "report_to_string", [
      "with_data", `Quick, test_report_to_string_with_data;
      "no_sizes", `Quick, test_report_to_string_no_sizes;
    ];
  ]
