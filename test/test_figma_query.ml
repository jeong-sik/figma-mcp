(** Comprehensive Coverage Tests for figma_query.ml

    Target: figma_query.ml (99 bisect points, 31.31% coverage)
    Uncovered: 68 points

    Functions to cover:
    - compare_value (Eq, Gte, Lte, Gt, Lt)
    - node_type_to_string (all 18 variants + Unknown)
    - get_node_width / get_node_height (Some/None bbox)
    - paint_to_hex (Solid+Some / non-Solid / None)
    - node_has_color (match, no match)
    - apply_filter (TypeFilter, WidthFilter, HeightFilter, ColorFilter,
                    NameFilter, VisibleFilter, HasChildrenFilter)
    - matches_filters (AND logic)
    - collect_nodes (with/without depth limit)
    - execute_query (filter + limit)
    - Query builder helpers: with_type, with_width_min, with_width_max,
      with_height_min, with_height_max, with_color, with_name, with_depth, with_limit
    - node_summary / results_to_string
*)

open Alcotest
open Figma_types
open Figma_query

(** ============== Test Fixtures ============== *)

let make_rgba ?(a=1.0) r g b = { r; g; b; a }

let make_solid_paint rgba =
  { paint_type = Solid; visible = true; opacity = 1.0;
    color = Some rgba; gradient_stops = []; image_ref = None; scale_mode = None }

let make_bbox ?(x=0.) ?(y=0.) width height = { x; y; width; height }

let make_node
    ?(id="n1") ?(name="Node") ?(node_type=Frame) ?(visible=true)
    ?(bbox=None) ?(fills=[]) ?(children=[]) () =
  { default_node with id; name; node_type; visible; bbox; fills; children }

(** ============== compare_value Tests ============== *)

let test_compare_value () =
  check bool "Eq true" true (compare_value Eq 5.0 5.0);
  check bool "Eq false" false (compare_value Eq 5.0 6.0);
  check bool "Gte equal" true (compare_value Gte 5.0 5.0);
  check bool "Gte greater" true (compare_value Gte 6.0 5.0);
  check bool "Gte less" false (compare_value Gte 4.0 5.0);
  check bool "Lte equal" true (compare_value Lte 5.0 5.0);
  check bool "Lte less" true (compare_value Lte 4.0 5.0);
  check bool "Lte greater" false (compare_value Lte 6.0 5.0);
  check bool "Gt true" true (compare_value Gt 6.0 5.0);
  check bool "Gt false" false (compare_value Gt 5.0 5.0);
  check bool "Lt true" true (compare_value Lt 4.0 5.0);
  check bool "Lt false" false (compare_value Lt 5.0 5.0)

(** ============== node_type_to_string Tests ============== *)

let test_node_type_to_string () =
  check string "Document" "DOCUMENT" (node_type_to_string Document);
  check string "Canvas" "CANVAS" (node_type_to_string Canvas);
  check string "Frame" "FRAME" (node_type_to_string Frame);
  check string "Group" "GROUP" (node_type_to_string Group);
  check string "Vector" "VECTOR" (node_type_to_string Vector);
  check string "BooleanOperation" "BOOLEAN_OPERATION" (node_type_to_string BooleanOperation);
  check string "Star" "STAR" (node_type_to_string Star);
  check string "Line" "LINE" (node_type_to_string Line);
  check string "Ellipse" "ELLIPSE" (node_type_to_string Ellipse);
  check string "RegularPolygon" "REGULAR_POLYGON" (node_type_to_string RegularPolygon);
  check string "Rectangle" "RECTANGLE" (node_type_to_string Rectangle);
  check string "Text" "TEXT" (node_type_to_string Text);
  check string "Slice" "SLICE" (node_type_to_string Slice);
  check string "Component" "COMPONENT" (node_type_to_string Component);
  check string "ComponentSet" "COMPONENT_SET" (node_type_to_string ComponentSet);
  check string "Instance" "INSTANCE" (node_type_to_string Instance);
  check string "Sticky" "STICKY" (node_type_to_string Sticky);
  check string "Section" "SECTION" (node_type_to_string Section);
  check string "Unknown" "UNKNOWN" (node_type_to_string (Unknown "foo"))

(** ============== get_node_width / get_node_height Tests ============== *)

let test_get_node_width_some () =
  let node = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  check (option (float 0.01)) "width" (Some 100.0) (get_node_width node)

let test_get_node_width_none () =
  let node = make_node ~bbox:None () in
  check (option (float 0.01)) "no width" None (get_node_width node)

let test_get_node_height_some () =
  let node = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  check (option (float 0.01)) "height" (Some 50.0) (get_node_height node)

let test_get_node_height_none () =
  let node = make_node ~bbox:None () in
  check (option (float 0.01)) "no height" None (get_node_height node)

(** ============== paint_to_hex Tests ============== *)

let test_paint_to_hex_solid () =
  let paint = make_solid_paint (make_rgba 1.0 0.0 0.0) in
  check (option string) "red hex" (Some "#FF0000") (paint_to_hex paint)

let test_paint_to_hex_non_solid () =
  let paint = { paint_type = GradientLinear; visible = true; opacity = 1.0;
                color = Some (make_rgba 1.0 0.0 0.0);
                gradient_stops = []; image_ref = None; scale_mode = None } in
  check (option string) "non-solid returns None" None (paint_to_hex paint)

let test_paint_to_hex_no_color () =
  let paint = { paint_type = Solid; visible = true; opacity = 1.0;
                color = None; gradient_stops = []; image_ref = None; scale_mode = None } in
  check (option string) "no color returns None" None (paint_to_hex paint)

(** ============== node_has_color Tests ============== *)

let test_node_has_color_match () =
  let red = make_rgba 1.0 0.0 0.0 in
  let node = make_node ~fills:[make_solid_paint red] () in
  check bool "has red" true (node_has_color node "#FF0000")

let test_node_has_color_case_insensitive () =
  let red = make_rgba 1.0 0.0 0.0 in
  let node = make_node ~fills:[make_solid_paint red] () in
  check bool "has red lowercase" true (node_has_color node "#ff0000")

let test_node_has_color_no_match () =
  let red = make_rgba 1.0 0.0 0.0 in
  let node = make_node ~fills:[make_solid_paint red] () in
  check bool "no blue" false (node_has_color node "#0000FF")

let test_node_has_color_no_fills () =
  let node = make_node ~fills:[] () in
  check bool "no fills" false (node_has_color node "#FF0000")

(** ============== apply_filter Tests ============== *)

let test_filter_type () =
  let node = make_node ~node_type:Frame () in
  check bool "matches FRAME" true (apply_filter (TypeFilter ["FRAME"]) node);
  check bool "no match TEXT" false (apply_filter (TypeFilter ["TEXT"]) node);
  (* Case insensitive *)
  check bool "matches frame lowercase" true (apply_filter (TypeFilter ["frame"]) node)

let test_filter_width () =
  let node = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  check bool "width >= 100" true (apply_filter (WidthFilter (Gte, 100.0)) node);
  check bool "width >= 200" false (apply_filter (WidthFilter (Gte, 200.0)) node);
  check bool "width = 100" true (apply_filter (WidthFilter (Eq, 100.0)) node)

let test_filter_width_no_bbox () =
  let node = make_node ~bbox:None () in
  check bool "no bbox => false" false (apply_filter (WidthFilter (Gte, 10.0)) node)

let test_filter_height () =
  let node = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  check bool "height <= 50" true (apply_filter (HeightFilter (Lte, 50.0)) node);
  check bool "height <= 30" false (apply_filter (HeightFilter (Lte, 30.0)) node)

let test_filter_height_no_bbox () =
  let node = make_node ~bbox:None () in
  check bool "no bbox => false" false (apply_filter (HeightFilter (Lte, 50.0)) node)

let test_filter_color () =
  let red = make_rgba 1.0 0.0 0.0 in
  let node = make_node ~fills:[make_solid_paint red] () in
  check bool "has red" true (apply_filter (ColorFilter "#FF0000") node);
  check bool "no blue" false (apply_filter (ColorFilter "#0000FF") node)

let test_filter_name () =
  let node = make_node ~name:"My Button Component" () in
  check bool "substring match" true (apply_filter (NameFilter "button") node);
  check bool "no match" false (apply_filter (NameFilter "card") node)

let test_filter_visible () =
  let visible_node = make_node ~visible:true () in
  let hidden_node = make_node ~visible:false () in
  check bool "visible true" true (apply_filter (VisibleFilter true) visible_node);
  check bool "visible false" false (apply_filter (VisibleFilter true) hidden_node);
  check bool "hidden true" true (apply_filter (VisibleFilter false) hidden_node)

let test_filter_has_children () =
  let child = make_node ~id:"c1" () in
  let parent = make_node ~children:[child] () in
  let leaf = make_node ~children:[] () in
  check bool "has children" true (apply_filter (HasChildrenFilter true) parent);
  check bool "no children" false (apply_filter (HasChildrenFilter true) leaf);
  check bool "leaf check" true (apply_filter (HasChildrenFilter false) leaf)

(** ============== matches_filters Tests ============== *)

let test_matches_filters_all () =
  let node = make_node ~node_type:Frame ~visible:true
    ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let filters = [TypeFilter ["FRAME"]; VisibleFilter true; WidthFilter (Gte, 50.0)] in
  check bool "all match" true (matches_filters filters node)

let test_matches_filters_one_fails () =
  let node = make_node ~node_type:Frame ~visible:true
    ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let filters = [TypeFilter ["FRAME"]; VisibleFilter false] in
  check bool "one fails" false (matches_filters filters node)

let test_matches_filters_empty () =
  let node = make_node () in
  check bool "empty filters = true" true (matches_filters [] node)

(** ============== collect_nodes Tests ============== *)

let test_collect_nodes_flat () =
  let node = make_node ~children:[] () in
  let nodes = collect_nodes ~max_depth:None node in
  check int "single node" 1 (List.length nodes)

let test_collect_nodes_with_children () =
  let c1 = make_node ~id:"c1" ~children:[] () in
  let c2 = make_node ~id:"c2" ~children:[] () in
  let root = make_node ~id:"root" ~children:[c1; c2] () in
  let nodes = collect_nodes ~max_depth:None root in
  check int "root + 2 children" 3 (List.length nodes)

let test_collect_nodes_depth_limited () =
  let leaf = make_node ~id:"leaf" ~children:[] () in
  let mid = make_node ~id:"mid" ~children:[leaf] () in
  let root = make_node ~id:"root" ~children:[mid] () in
  let nodes = collect_nodes ~max_depth:(Some 1) root in
  (* depth 0: root, depth 1: mid (stops here, doesn't go to leaf) *)
  check int "depth 1 = 2 nodes" 2 (List.length nodes)

let test_collect_nodes_depth_zero () =
  let child = make_node ~id:"c1" ~children:[] () in
  let root = make_node ~id:"root" ~children:[child] () in
  let nodes = collect_nodes ~max_depth:(Some 0) root in
  check int "depth 0 = root only" 1 (List.length nodes)

(** ============== execute_query Tests ============== *)

let test_execute_query_no_filters () =
  let c1 = make_node ~id:"c1" () in
  let root = make_node ~id:"root" ~children:[c1] () in
  let results = execute_query empty_query root in
  check int "all nodes" 2 (List.length results)

let test_execute_query_with_type_filter () =
  let text_node = make_node ~id:"t1" ~node_type:Text () in
  let frame_node = make_node ~id:"f1" ~node_type:Frame ~children:[text_node] () in
  let query = empty_query |> with_type ["TEXT"] in
  let results = execute_query query frame_node in
  check int "only text nodes" 1 (List.length results)

let test_execute_query_with_limit () =
  let c1 = make_node ~id:"c1" () in
  let c2 = make_node ~id:"c2" () in
  let c3 = make_node ~id:"c3" () in
  let root = make_node ~id:"root" ~children:[c1; c2; c3] () in
  let query = empty_query |> with_limit 2 in
  let results = execute_query query root in
  check int "limited to 2" 2 (List.length results)

let test_execute_query_with_depth () =
  let leaf = make_node ~id:"leaf" () in
  let mid = make_node ~id:"mid" ~children:[leaf] () in
  let root = make_node ~id:"root" ~children:[mid] () in
  let query = empty_query |> with_depth 1 in
  let results = execute_query query root in
  check int "depth limited" 2 (List.length results)

(** ============== Query Builder Tests ============== *)

let test_query_builders () =
  let q = empty_query
    |> with_type ["FRAME"]
    |> with_width_min 100.0
    |> with_width_max 500.0
    |> with_height_min 50.0
    |> with_height_max 200.0
    |> with_color "#FF0000"
    |> with_name "Button"
    |> with_depth 3
    |> with_limit 10
  in
  check int "7 filters" 7 (List.length q.filters);
  check (option int) "depth 3" (Some 3) q.depth;
  check (option int) "limit 10" (Some 10) q.limit

(** ============== node_summary / results_to_string Tests ============== *)

let test_node_summary_with_bbox () =
  let node = make_node ~id:"n1" ~name:"Button" ~node_type:Frame
    ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let s = node_summary node in
  check bool "contains FRAME" true
    (try let _ = Str.search_forward (Str.regexp_string "FRAME") s 0 in true
     with Not_found -> false);
  check bool "contains Button" true
    (try let _ = Str.search_forward (Str.regexp_string "Button") s 0 in true
     with Not_found -> false);
  check bool "contains size" true
    (try let _ = Str.search_forward (Str.regexp_string "100x50") s 0 in true
     with Not_found -> false)

let test_node_summary_no_bbox () =
  let node = make_node ~id:"n1" ~name:"Node" ~bbox:None () in
  let s = node_summary node in
  check bool "contains ?" true
    (try let _ = Str.search_forward (Str.regexp_string "?") s 0 in true
     with Not_found -> false)

let test_results_to_string () =
  let n1 = make_node ~id:"n1" ~name:"A" () in
  let n2 = make_node ~id:"n2" ~name:"B" () in
  let s = results_to_string [n1; n2] in
  check bool "contains count" true
    (try let _ = Str.search_forward (Str.regexp_string "Found 2") s 0 in true
     with Not_found -> false)

let test_results_to_string_empty () =
  let s = results_to_string [] in
  check bool "contains 0" true
    (try let _ = Str.search_forward (Str.regexp_string "Found 0") s 0 in true
     with Not_found -> false)

(** ============== Main Test Runner ============== *)

let () =
  run "figma-query-coverage" [
    "compare_value", [
      "all_comparisons", `Quick, test_compare_value;
    ];
    "node_type_to_string", [
      "all_types", `Quick, test_node_type_to_string;
    ];
    "get_dimensions", [
      "width_some", `Quick, test_get_node_width_some;
      "width_none", `Quick, test_get_node_width_none;
      "height_some", `Quick, test_get_node_height_some;
      "height_none", `Quick, test_get_node_height_none;
    ];
    "paint_to_hex", [
      "solid", `Quick, test_paint_to_hex_solid;
      "non_solid", `Quick, test_paint_to_hex_non_solid;
      "no_color", `Quick, test_paint_to_hex_no_color;
    ];
    "node_has_color", [
      "match", `Quick, test_node_has_color_match;
      "case_insensitive", `Quick, test_node_has_color_case_insensitive;
      "no_match", `Quick, test_node_has_color_no_match;
      "no_fills", `Quick, test_node_has_color_no_fills;
    ];
    "apply_filter", [
      "type", `Quick, test_filter_type;
      "width", `Quick, test_filter_width;
      "width_no_bbox", `Quick, test_filter_width_no_bbox;
      "height", `Quick, test_filter_height;
      "height_no_bbox", `Quick, test_filter_height_no_bbox;
      "color", `Quick, test_filter_color;
      "name", `Quick, test_filter_name;
      "visible", `Quick, test_filter_visible;
      "has_children", `Quick, test_filter_has_children;
    ];
    "matches_filters", [
      "all_match", `Quick, test_matches_filters_all;
      "one_fails", `Quick, test_matches_filters_one_fails;
      "empty_filters", `Quick, test_matches_filters_empty;
    ];
    "collect_nodes", [
      "flat", `Quick, test_collect_nodes_flat;
      "with_children", `Quick, test_collect_nodes_with_children;
      "depth_limited", `Quick, test_collect_nodes_depth_limited;
      "depth_zero", `Quick, test_collect_nodes_depth_zero;
    ];
    "execute_query", [
      "no_filters", `Quick, test_execute_query_no_filters;
      "type_filter", `Quick, test_execute_query_with_type_filter;
      "with_limit", `Quick, test_execute_query_with_limit;
      "with_depth", `Quick, test_execute_query_with_depth;
    ];
    "query_builders", [
      "all_builders", `Quick, test_query_builders;
    ];
    "formatting", [
      "summary_with_bbox", `Quick, test_node_summary_with_bbox;
      "summary_no_bbox", `Quick, test_node_summary_no_bbox;
      "results_to_string", `Quick, test_results_to_string;
      "results_empty", `Quick, test_results_to_string_empty;
    ];
  ]
