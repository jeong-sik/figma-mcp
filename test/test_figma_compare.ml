(** Comprehensive Coverage Tests for figma_compare.ml

    Target: figma_compare.ml (227 bisect points, 0% coverage)

    Existing tests in test_utils_coverage.ml are NOT wired in dune,
    so this file provides complete coverage for all functions and branches.

    Functions to cover:
    - severity_to_string (4 variants)
    - severity_weight (4 variants)
    - float_eq (default/custom tolerance)
    - percent_diff (normal, zero base, both zero)
    - compare_size (same, width diff, height diff, aspect ratio, no bbox, severity levels)
    - rgba_to_hex (basic, mixed)
    - color_distance (same, different)
    - compare_colors (same, different Major/Minor, None-Some, Some-None, None-None)
    - compare_typography (same, size diff Major/Minor, family, weight, Some-None, None-Some, None-None)
    - layout_mode_to_string (3 variants)
    - compare_layout (same, mode, gap, padding)
    - compare_structure (same, diff Minor, diff Major, node_type Critical)
    - compare_nodes (identical, with diffs, similarity score)
    - result_to_string (no diffs, with diffs grouped)
    - normalize_name (all prefix variants, no prefix)
    - find_matching_pairs (match, no match, multiple)
    - compare_web_mobile (with/without matches, empty)
*)

open Alcotest
open Figma_types
open Figma_compare

(** ============== Test Fixtures ============== *)

let make_rgba ?(a=1.0) r g b = { r; g; b; a }

let make_paint ?(visible=true) ?(opacity=1.0) paint_type color =
  { paint_type; visible; opacity; color; gradient_stops = []; image_ref = None; scale_mode = None }

let make_solid_paint rgba = make_paint Solid (Some rgba)

let make_typography
    ?(font_family="Inter")
    ?(font_weight=400)
    ?(line_height=None)
    ?(letter_spacing=None)
    font_size =
  { Figma_types.font_family; font_size; font_weight;
    font_style = "normal"; line_height; letter_spacing;
    text_align_h = Left; text_align_v = Top;
    text_decoration = NoDeco; text_case = Original }

let make_bbox ?(x=0.) ?(y=0.) width height = { x; y; width; height }

let make_node
    ?(id="test-id")
    ?(name="Test Node")
    ?(node_type=Frame)
    ?(visible=true)
    ?(fills=[])
    ?(typography=None)
    ?(layout_mode=None')
    ?(padding=(0., 0., 0., 0.))
    ?(gap=0.)
    ?(bbox=None)
    ?(children=[])
    () =
  { Figma_types.default_node with
    id; name; node_type; visible; fills; typography;
    layout_mode; padding; gap; bbox; children }

(** ============== severity_to_string Tests ============== *)

let test_severity_to_string () =
  check string "critical" "CRITICAL" (severity_to_string Critical);
  check string "major" "MAJOR" (severity_to_string Major);
  check string "minor" "MINOR" (severity_to_string Minor);
  check string "info" "INFO" (severity_to_string Info)

(** ============== severity_weight Tests ============== *)

let test_severity_weight () =
  check (float 0.01) "critical = 1.0" 1.0 (severity_weight Critical);
  check (float 0.01) "major = 0.5" 0.5 (severity_weight Major);
  check (float 0.01) "minor = 0.2" 0.2 (severity_weight Minor);
  check (float 0.01) "info = 0.0" 0.0 (severity_weight Info)

(** ============== float_eq Tests ============== *)

let test_float_eq_within () =
  check bool "0.0 ~= 0.3" true (float_eq 0.0 0.3);
  check bool "10.0 ~= 10.4" true (float_eq 10.0 10.4);
  check bool "boundary 0.5" true (float_eq 0.0 0.5)

let test_float_eq_outside () =
  check bool "0.0 != 1.0" false (float_eq 0.0 1.0);
  check bool "10.0 != 11.0" false (float_eq 10.0 11.0)

let test_float_eq_custom_tolerance () =
  check bool "with tolerance 2.0" true (float_eq ~tolerance:2.0 0.0 1.5);
  check bool "with tolerance 0.1" false (float_eq ~tolerance:0.1 0.0 0.3)

(** ============== percent_diff Tests ============== *)

let test_percent_diff_basic () =
  check (float 0.1) "100 to 110 = 10%" 10.0 (percent_diff 100.0 110.0);
  check (float 0.1) "100 to 90 = 10%" 10.0 (percent_diff 100.0 90.0)

let test_percent_diff_zero_base () =
  check (float 0.1) "0 to 100 = 100%" 100.0 (percent_diff 0.0 100.0)

let test_percent_diff_both_zero () =
  check (float 0.1) "0 to 0 = 0%" 0.0 (percent_diff 0.0 0.0)

(** ============== compare_size Tests ============== *)

let test_compare_size_same () =
  let bbox = Some (make_bbox 100.0 50.0) in
  let node_a = make_node ~bbox () in
  let node_b = make_node ~bbox () in
  let diffs = compare_size node_a node_b in
  check int "no size diff" 0 (List.length diffs)

let test_compare_size_different_width_major () =
  (* width_diff > 20% => Major *)
  let node_a = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let node_b = make_node ~bbox:(Some (make_bbox 150.0 50.0)) () in
  let diffs = compare_size node_a node_b in
  check bool "has width diff" true (List.exists (fun d -> d.property = "width") diffs);
  let d = List.find (fun d -> d.property = "width") diffs in
  check bool "major severity" true (d.severity = Major)

let test_compare_size_different_width_minor () =
  (* width_diff > 5% but <= 20% => Minor *)
  let node_a = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let node_b = make_node ~bbox:(Some (make_bbox 112.0 50.0)) () in
  let diffs = compare_size node_a node_b in
  check bool "has width diff" true (List.exists (fun d -> d.property = "width") diffs);
  let d = List.find (fun d -> d.property = "width") diffs in
  check bool "minor severity" true (d.severity = Minor)

let test_compare_size_different_width_info () =
  (* width outside tolerance but diff <= 5% => Info *)
  let node_a = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let node_b = make_node ~bbox:(Some (make_bbox 103.0 50.0)) () in
  let diffs = compare_size node_a node_b in
  check bool "has width diff" true (List.exists (fun d -> d.property = "width") diffs);
  let d = List.find (fun d -> d.property = "width") diffs in
  check bool "info severity" true (d.severity = Info)

let test_compare_size_different_height_major () =
  let node_a = make_node ~bbox:(Some (make_bbox 100.0 100.0)) () in
  let node_b = make_node ~bbox:(Some (make_bbox 100.0 150.0)) () in
  let diffs = compare_size node_a node_b in
  check bool "has height diff" true (List.exists (fun d -> d.property = "height") diffs);
  let d = List.find (fun d -> d.property = "height") diffs in
  check bool "major severity for height" true (d.severity = Major)

let test_compare_size_different_height_minor () =
  let node_a = make_node ~bbox:(Some (make_bbox 100.0 100.0)) () in
  let node_b = make_node ~bbox:(Some (make_bbox 100.0 115.0)) () in
  let diffs = compare_size node_a node_b in
  check bool "has height diff" true (List.exists (fun d -> d.property = "height") diffs);
  let d = List.find (fun d -> d.property = "height") diffs in
  check bool "minor severity for height" true (d.severity = Minor)

let test_compare_size_different_height_info () =
  let node_a = make_node ~bbox:(Some (make_bbox 100.0 100.0)) () in
  let node_b = make_node ~bbox:(Some (make_bbox 100.0 103.0)) () in
  let diffs = compare_size node_a node_b in
  check bool "has height diff" true (List.exists (fun d -> d.property = "height") diffs);
  let d = List.find (fun d -> d.property = "height") diffs in
  check bool "info severity for height" true (d.severity = Info)

let test_compare_size_aspect_ratio () =
  (* 100x50 = ratio 2.0, 100x70 = ratio ~1.43, difference > 0.1 tolerance *)
  let node_a = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let node_b = make_node ~bbox:(Some (make_bbox 100.0 70.0)) () in
  let diffs = compare_size node_a node_b in
  check bool "has aspect_ratio diff" true
    (List.exists (fun d -> d.property = "aspect_ratio") diffs)

let test_compare_size_no_bbox () =
  let node_a = make_node ~bbox:None () in
  let node_b = make_node ~bbox:None () in
  let diffs = compare_size node_a node_b in
  check int "no diff when no bbox" 0 (List.length diffs)

let test_compare_size_one_bbox () =
  let node_a = make_node ~bbox:(Some (make_bbox 100.0 50.0)) () in
  let node_b = make_node ~bbox:None () in
  let diffs = compare_size node_a node_b in
  check int "no diff when one bbox" 0 (List.length diffs)

(** ============== rgba_to_hex / color_distance Tests ============== *)

let test_rgba_to_hex () =
  let red = make_rgba 1.0 0.0 0.0 in
  check string "red" "#FF0000" (rgba_to_hex red);
  let green = make_rgba 0.0 1.0 0.0 in
  check string "green" "#00FF00" (rgba_to_hex green);
  let blue = make_rgba 0.0 0.0 1.0 in
  check string "blue" "#0000FF" (rgba_to_hex blue);
  let black = make_rgba 0.0 0.0 0.0 in
  check string "black" "#000000" (rgba_to_hex black);
  let white = make_rgba 1.0 1.0 1.0 in
  check string "white" "#FFFFFF" (rgba_to_hex white)

let test_color_distance_same () =
  let red = make_rgba 1.0 0.0 0.0 in
  check (float 0.01) "same color = 0" 0.0 (color_distance red red)

let test_color_distance_different () =
  let black = make_rgba 0.0 0.0 0.0 in
  let white = make_rgba 1.0 1.0 1.0 in
  let dist = color_distance black white in
  check bool "large distance" true (dist > 400.0)

let test_color_distance_similar () =
  let c1 = make_rgba 0.5 0.5 0.5 in
  let c2 = make_rgba 0.51 0.51 0.51 in
  let dist = color_distance c1 c2 in
  check bool "small distance" true (dist < 10.0)

(** ============== compare_colors Tests ============== *)

let test_compare_colors_same () =
  let red = make_rgba 1.0 0.0 0.0 in
  let node_a = make_node ~fills:[make_solid_paint red] () in
  let node_b = make_node ~fills:[make_solid_paint red] () in
  let diffs = compare_colors node_a node_b in
  check int "no color diff" 0 (List.length diffs)

let test_compare_colors_different_major () =
  (* color_distance > 50 => Major *)
  let red = make_rgba 1.0 0.0 0.0 in
  let blue = make_rgba 0.0 0.0 1.0 in
  let node_a = make_node ~fills:[make_solid_paint red] () in
  let node_b = make_node ~fills:[make_solid_paint blue] () in
  let diffs = compare_colors node_a node_b in
  check bool "has diff" true (List.length diffs > 0);
  let d = List.hd diffs in
  check bool "major for large diff" true (d.severity = Major)

let test_compare_colors_different_minor () =
  (* color_distance > 10 but <= 50 => Minor *)
  let c1 = make_rgba 0.5 0.5 0.5 in
  let c2 = make_rgba 0.6 0.5 0.5 in
  let node_a = make_node ~fills:[make_solid_paint c1] () in
  let node_b = make_node ~fills:[make_solid_paint c2] () in
  let diffs = compare_colors node_a node_b in
  check bool "has diff" true (List.length diffs > 0);
  let d = List.hd diffs in
  check bool "minor for small diff" true (d.severity = Minor)

let test_compare_colors_small_diff () =
  (* color_distance <= 10 => no diff reported *)
  let c1 = make_rgba 0.5 0.5 0.5 in
  let c2 = make_rgba 0.51 0.51 0.51 in
  let node_a = make_node ~fills:[make_solid_paint c1] () in
  let node_b = make_node ~fills:[make_solid_paint c2] () in
  let diffs = compare_colors node_a node_b in
  check int "no diff for small color change" 0 (List.length diffs)

let test_compare_colors_none_some () =
  let blue = make_rgba 0.0 0.0 1.0 in
  let node_a = make_node ~fills:[] () in
  let node_b = make_node ~fills:[make_solid_paint blue] () in
  let diffs = compare_colors node_a node_b in
  check bool "has diff" true (List.length diffs > 0);
  let d = List.hd diffs in
  check string "a has none" "없음" d.value_a

let test_compare_colors_some_none () =
  let red = make_rgba 1.0 0.0 0.0 in
  let node_a = make_node ~fills:[make_solid_paint red] () in
  let node_b = make_node ~fills:[] () in
  let diffs = compare_colors node_a node_b in
  check bool "has diff" true (List.length diffs > 0);
  let d = List.hd diffs in
  check string "b has none" "없음" d.value_b

let test_compare_colors_none_none () =
  let node_a = make_node ~fills:[] () in
  let node_b = make_node ~fills:[] () in
  let diffs = compare_colors node_a node_b in
  check int "no diff" 0 (List.length diffs)

(** ============== compare_typography Tests ============== *)

let test_compare_typography_same () =
  let typo = make_typography 16.0 in
  let node_a = make_node ~typography:(Some typo) () in
  let node_b = make_node ~typography:(Some typo) () in
  let diffs = compare_typography node_a node_b in
  check int "no typography diff" 0 (List.length diffs)

let test_compare_typography_size_major () =
  (* size_diff > 20% => Major *)
  let typo_a = make_typography 16.0 in
  let typo_b = make_typography 24.0 in
  let node_a = make_node ~typography:(Some typo_a) () in
  let node_b = make_node ~typography:(Some typo_b) () in
  let diffs = compare_typography node_a node_b in
  check bool "has font_size diff" true
    (List.exists (fun d -> d.property = "font_size") diffs);
  let d = List.find (fun d -> d.property = "font_size") diffs in
  check bool "major for big size diff" true (d.severity = Major)

let test_compare_typography_size_minor () =
  (* size_diff > 5% but <= 20% => Minor *)
  let typo_a = make_typography 16.0 in
  let typo_b = make_typography 18.0 in
  let node_a = make_node ~typography:(Some typo_a) () in
  let node_b = make_node ~typography:(Some typo_b) () in
  let diffs = compare_typography node_a node_b in
  check bool "has font_size diff" true
    (List.exists (fun d -> d.property = "font_size") diffs);
  let d = List.find (fun d -> d.property = "font_size") diffs in
  check bool "minor for small size diff" true (d.severity = Minor)

let test_compare_typography_size_within () =
  (* size_diff <= 5% => no diff *)
  let typo_a = make_typography 100.0 in
  let typo_b = make_typography 104.0 in
  let node_a = make_node ~typography:(Some typo_a) () in
  let node_b = make_node ~typography:(Some typo_b) () in
  let diffs = compare_typography node_a node_b in
  check bool "no font_size diff" false
    (List.exists (fun d -> d.property = "font_size") diffs)

let test_compare_typography_family () =
  let typo_a = make_typography ~font_family:"Inter" 16.0 in
  let typo_b = make_typography ~font_family:"Roboto" 16.0 in
  let node_a = make_node ~typography:(Some typo_a) () in
  let node_b = make_node ~typography:(Some typo_b) () in
  let diffs = compare_typography node_a node_b in
  check bool "has font_family diff" true
    (List.exists (fun d -> d.property = "font_family") diffs);
  let d = List.find (fun d -> d.property = "font_family") diffs in
  check bool "minor severity" true (d.severity = Minor)

let test_compare_typography_weight () =
  let typo_a = make_typography ~font_weight:400 16.0 in
  let typo_b = make_typography ~font_weight:700 16.0 in
  let node_a = make_node ~typography:(Some typo_a) () in
  let node_b = make_node ~typography:(Some typo_b) () in
  let diffs = compare_typography node_a node_b in
  check bool "has font_weight diff" true
    (List.exists (fun d -> d.property = "font_weight") diffs);
  let d = List.find (fun d -> d.property = "font_weight") diffs in
  check bool "info severity" true (d.severity = Info)

let test_compare_typography_some_none () =
  let typo = make_typography 16.0 in
  let node_a = make_node ~typography:(Some typo) () in
  let node_b = make_node ~typography:None () in
  let diffs = compare_typography node_a node_b in
  check bool "has presence diff" true
    (List.exists (fun d -> d.property = "presence") diffs);
  let d = List.find (fun d -> d.property = "presence") diffs in
  check bool "major severity" true (d.severity = Major);
  check string "a has" "있음" d.value_a;
  check string "b missing" "없음" d.value_b

let test_compare_typography_none_some () =
  let typo = make_typography 16.0 in
  let node_a = make_node ~typography:None () in
  let node_b = make_node ~typography:(Some typo) () in
  let diffs = compare_typography node_a node_b in
  check bool "has presence diff" true
    (List.exists (fun d -> d.property = "presence") diffs);
  let d = List.find (fun d -> d.property = "presence") diffs in
  check string "a missing" "없음" d.value_a;
  check string "b has" "있음" d.value_b

let test_compare_typography_none_none () =
  let node_a = make_node ~typography:None () in
  let node_b = make_node ~typography:None () in
  let diffs = compare_typography node_a node_b in
  check int "no diffs" 0 (List.length diffs)

(** ============== layout_mode_to_string Tests ============== *)

let test_layout_mode_to_string () =
  check string "horizontal" "Horizontal" (layout_mode_to_string Horizontal);
  check string "vertical" "Vertical" (layout_mode_to_string Vertical);
  check string "none" "None" (layout_mode_to_string None')

(** ============== compare_layout Tests ============== *)

let test_compare_layout_same () =
  let node_a = make_node ~layout_mode:Horizontal ~gap:8.0 ~padding:(16., 16., 16., 16.) () in
  let node_b = make_node ~layout_mode:Horizontal ~gap:8.0 ~padding:(16., 16., 16., 16.) () in
  let diffs = compare_layout node_a node_b in
  check int "no layout diff" 0 (List.length diffs)

let test_compare_layout_mode () =
  let node_a = make_node ~layout_mode:Horizontal () in
  let node_b = make_node ~layout_mode:Vertical () in
  let diffs = compare_layout node_a node_b in
  check bool "has layout_mode diff" true
    (List.exists (fun d -> d.property = "layout_mode") diffs);
  let d = List.find (fun d -> d.property = "layout_mode") diffs in
  check bool "major severity" true (d.severity = Major)

let test_compare_layout_gap () =
  let node_a = make_node ~gap:8.0 () in
  let node_b = make_node ~gap:16.0 () in
  let diffs = compare_layout node_a node_b in
  check bool "has gap diff" true
    (List.exists (fun d -> d.property = "gap") diffs)

let test_compare_layout_padding () =
  let node_a = make_node ~padding:(8., 8., 8., 8.) () in
  let node_b = make_node ~padding:(16., 16., 16., 16.) () in
  let diffs = compare_layout node_a node_b in
  check bool "has padding diff" true
    (List.exists (fun d -> d.property = "padding") diffs)

let test_compare_layout_padding_partial () =
  (* Only one side different *)
  let node_a = make_node ~padding:(8., 8., 8., 8.) () in
  let node_b = make_node ~padding:(16., 8., 8., 8.) () in
  let diffs = compare_layout node_a node_b in
  check bool "has padding diff for one side" true
    (List.exists (fun d -> d.property = "padding") diffs)

(** ============== compare_structure Tests ============== *)

let test_compare_structure_same_children () =
  let child = make_node ~id:"c1" () in
  let node_a = make_node ~children:[child] () in
  let node_b = make_node ~children:[child] () in
  let diffs = compare_structure node_a node_b in
  check bool "no children_count diff" false
    (List.exists (fun d -> d.property = "children_count") diffs)

let test_compare_structure_different_minor () =
  (* abs diff <= 3 => Minor *)
  let child = make_node ~id:"c1" () in
  let node_a = make_node ~children:[child; child] () in
  let node_b = make_node ~children:[child] () in
  let diffs = compare_structure node_a node_b in
  check bool "has children_count diff" true
    (List.exists (fun d -> d.property = "children_count") diffs);
  let d = List.find (fun d -> d.property = "children_count") diffs in
  check bool "minor severity" true (d.severity = Minor)

let test_compare_structure_different_major () =
  (* abs diff > 3 => Major *)
  let child = make_node ~id:"c1" () in
  let node_a = make_node ~children:[child; child; child; child; child] () in
  let node_b = make_node ~children:[child] () in
  let diffs = compare_structure node_a node_b in
  check bool "has children_count diff" true
    (List.exists (fun d -> d.property = "children_count") diffs);
  let d = List.find (fun d -> d.property = "children_count") diffs in
  check bool "major severity" true (d.severity = Major)

let test_compare_structure_different_type () =
  let node_a = make_node ~node_type:Frame () in
  let node_b = make_node ~node_type:Text () in
  let diffs = compare_structure node_a node_b in
  check bool "has node_type diff" true
    (List.exists (fun d -> d.property = "node_type") diffs);
  let d = List.find (fun d -> d.property = "node_type") diffs in
  check bool "critical severity" true (d.severity = Critical)

let test_compare_structure_same_type () =
  let node_a = make_node ~node_type:Frame () in
  let node_b = make_node ~node_type:Frame () in
  let diffs = compare_structure node_a node_b in
  check bool "no node_type diff" false
    (List.exists (fun d -> d.property = "node_type") diffs)

(** ============== compare_nodes Tests ============== *)

let test_compare_nodes_identical () =
  let node = make_node () in
  let result = compare_nodes node node in
  check (float 0.01) "similarity 1.0" 1.0 result.similarity_score;
  check int "no differences" 0 (List.length result.differences);
  check string "node_a_name" "Test Node" result.node_a_name;
  check string "node_b_name" "Test Node" result.node_b_name

let test_compare_nodes_with_diffs () =
  let node_a = make_node ~name:"A" ~layout_mode:Horizontal () in
  let node_b = make_node ~name:"B" ~layout_mode:Vertical () in
  let result = compare_nodes node_a node_b in
  check bool "has differences" true (List.length result.differences > 0);
  check bool "similarity < 1" true (result.similarity_score < 1.0);
  check string "node_a_name" "A" result.node_a_name;
  check string "node_b_name" "B" result.node_b_name

let test_compare_nodes_many_diffs () =
  (* Multiple categories of diffs: structure, size, color, typography, layout *)
  let red = make_rgba 1.0 0.0 0.0 in
  let blue = make_rgba 0.0 0.0 1.0 in
  let node_a = make_node ~name:"A" ~node_type:Frame
    ~bbox:(Some (make_bbox 100.0 100.0))
    ~fills:[make_solid_paint red]
    ~typography:(Some (make_typography 16.0))
    ~layout_mode:Horizontal () in
  let node_b = make_node ~name:"B" ~node_type:Text
    ~bbox:(Some (make_bbox 200.0 200.0))
    ~fills:[make_solid_paint blue]
    ~typography:(Some (make_typography 32.0))
    ~layout_mode:Vertical () in
  let result = compare_nodes node_a node_b in
  check bool "many diffs" true (List.length result.differences > 3);
  (* Similarity < 1.0 when there are diffs *)
  check bool "reduced similarity" true (result.similarity_score < 1.0)

let test_compare_nodes_similarity_floor () =
  (* Similarity should floor at 0.0 for extreme differences *)
  let child = make_node ~id:"c1" () in
  let many_children = List.init 20 (fun _ -> child) in
  let red = make_rgba 1.0 0.0 0.0 in
  let blue = make_rgba 0.0 0.0 1.0 in
  let node_a = make_node ~name:"A" ~node_type:Frame
    ~bbox:(Some (make_bbox 10.0 10.0))
    ~fills:[make_solid_paint red]
    ~typography:(Some (make_typography 12.0))
    ~layout_mode:Horizontal ~gap:4.0 ~padding:(4., 4., 4., 4.)
    ~children:many_children () in
  let node_b = make_node ~name:"B" ~node_type:Text
    ~bbox:(Some (make_bbox 500.0 500.0))
    ~fills:[make_solid_paint blue]
    ~typography:(Some (make_typography 48.0))
    ~layout_mode:Vertical ~gap:32.0 ~padding:(32., 32., 32., 32.) () in
  let result = compare_nodes node_a node_b in
  check bool "similarity >= 0" true (result.similarity_score >= 0.0)

(** ============== result_to_string Tests ============== *)

let test_result_to_string_no_diff () =
  let node = make_node ~name:"TestNode" () in
  let result = compare_nodes node node in
  let s = result_to_string result in
  check bool "contains node names" true
    (try let _ = Str.search_forward (Str.regexp_string "TestNode") s 0 in true
     with Not_found -> false);
  check bool "contains 100%" true
    (try let _ = Str.search_forward (Str.regexp_string "100%") s 0 in true
     with Not_found -> false)

let test_result_to_string_with_diffs () =
  let node_a = make_node ~name:"A" ~layout_mode:Horizontal
    ~bbox:(Some (make_bbox 100.0 100.0)) () in
  let node_b = make_node ~name:"B" ~layout_mode:Vertical
    ~bbox:(Some (make_bbox 200.0 100.0)) () in
  let result = compare_nodes node_a node_b in
  let s = result_to_string result in
  (* Check grouped categories appear *)
  check bool "contains LAYOUT" true
    (try let _ = Str.search_forward (Str.regexp_string "LAYOUT") s 0 in true
     with Not_found -> false);
  check bool "contains SIZE" true
    (try let _ = Str.search_forward (Str.regexp_string "SIZE") s 0 in true
     with Not_found -> false);
  (* Check severity counts *)
  check bool "contains diff count" true
    (try let _ = Str.search_forward (Str.regexp "차이점:") s 0 in true
     with Not_found -> false)

let test_result_to_string_all_categories () =
  (* Generate diffs in structure, size, color, typography, layout *)
  let red = make_rgba 1.0 0.0 0.0 in
  let blue = make_rgba 0.0 0.0 1.0 in
  let child = make_node ~id:"c1" () in
  let node_a = make_node ~name:"X" ~node_type:Frame
    ~bbox:(Some (make_bbox 100.0 100.0))
    ~fills:[make_solid_paint red]
    ~typography:(Some (make_typography 16.0))
    ~layout_mode:Horizontal
    ~children:[child] () in
  let node_b = make_node ~name:"Y" ~node_type:Frame
    ~bbox:(Some (make_bbox 200.0 200.0))
    ~fills:[make_solid_paint blue]
    ~typography:(Some (make_typography ~font_family:"Roboto" 32.0))
    ~layout_mode:Vertical () in
  let result = compare_nodes node_a node_b in
  let s = result_to_string result in
  check bool "contains STRUCTURE" true
    (try let _ = Str.search_forward (Str.regexp_string "STRUCTURE") s 0 in true
     with Not_found -> false);
  check bool "contains COLOR" true
    (try let _ = Str.search_forward (Str.regexp_string "COLOR") s 0 in true
     with Not_found -> false);
  check bool "contains TYPOGRAPHY" true
    (try let _ = Str.search_forward (Str.regexp_string "TYPOGRAPHY") s 0 in true
     with Not_found -> false)

(** ============== normalize_name Tests ============== *)

let test_normalize_name_web () =
  check string "web/" "button" (normalize_name "Web/Button");
  check string "web_" "button" (normalize_name "web_button")

let test_normalize_name_mobile () =
  check string "mobile/" "button" (normalize_name "Mobile/Button");
  check string "mobile_" "button" (normalize_name "mobile_button")

let test_normalize_name_desktop () =
  check string "desktop/" "button" (normalize_name "Desktop/Button");
  check string "desktop_" "button" (normalize_name "Desktop_Button")

let test_normalize_name_ios () =
  check string "ios/" "card" (normalize_name "iOS/Card");
  check string "ios_" "card" (normalize_name "ios_card")

let test_normalize_name_android () =
  check string "android/" "list" (normalize_name "Android/List");
  check string "android_" "list" (normalize_name "Android_List")

let test_normalize_name_preserved () =
  check string "no prefix" "button" (normalize_name "Button")

let test_normalize_name_exact_prefix () =
  (* Edge case: name is exactly the prefix length - should NOT strip *)
  check string "short name" "web/" (normalize_name "Web/")

(** ============== find_matching_pairs Tests ============== *)

let test_find_matching_pairs_basic () =
  let web_btn = make_node ~name:"Web/Button" () in
  let mobile_btn = make_node ~name:"Mobile/Button" () in
  let pairs = find_matching_pairs [web_btn] [mobile_btn] in
  check int "one pair found" 1 (List.length pairs)

let test_find_matching_pairs_no_match () =
  let web_btn = make_node ~name:"Web/Button" () in
  let mobile_card = make_node ~name:"Mobile/Card" () in
  let pairs = find_matching_pairs [web_btn] [mobile_card] in
  check int "no pairs" 0 (List.length pairs)

let test_find_matching_pairs_multiple () =
  let web_btn = make_node ~name:"Web/Button" () in
  let web_card = make_node ~name:"Web/Card" () in
  let mobile_btn = make_node ~name:"Mobile/Button" () in
  let mobile_card = make_node ~name:"Mobile/Card" () in
  let pairs = find_matching_pairs [web_btn; web_card] [mobile_btn; mobile_card] in
  check int "two pairs" 2 (List.length pairs)

let test_find_matching_pairs_empty () =
  let pairs = find_matching_pairs [] [] in
  check int "no pairs from empty" 0 (List.length pairs)

(** ============== compare_web_mobile Tests ============== *)

let test_compare_web_mobile_match () =
  let web_btn = make_node ~name:"Web/Button" () in
  let mobile_btn = make_node ~name:"Mobile/Button" () in
  let (results, total, avg_sim, critical, major) =
    compare_web_mobile ~web_nodes:[web_btn] ~mobile_nodes:[mobile_btn] in
  check int "1 result" 1 total;
  check int "1 in list" 1 (List.length results);
  check (float 0.01) "100% avg" 1.0 avg_sim;
  check int "0 critical" 0 critical;
  check int "0 major" 0 major

let test_compare_web_mobile_with_diffs () =
  let web_btn = make_node ~name:"Web/Button" ~layout_mode:Horizontal () in
  let mobile_btn = make_node ~name:"Mobile/Button" ~layout_mode:Vertical () in
  let (results, total, avg_sim, _critical, major) =
    compare_web_mobile ~web_nodes:[web_btn] ~mobile_nodes:[mobile_btn] in
  check int "1 result" 1 total;
  check int "1 in list" 1 (List.length results);
  check bool "avg_sim < 1" true (avg_sim < 1.0);
  check bool "has major" true (major > 0)

let test_compare_web_mobile_empty () =
  let (_results, total, avg_sim, critical, major) =
    compare_web_mobile ~web_nodes:[] ~mobile_nodes:[] in
  check int "0 total" 0 total;
  check (float 0.01) "0 avg for empty" 0.0 avg_sim;
  check int "0 critical" 0 critical;
  check int "0 major" 0 major

let test_compare_web_mobile_no_matches () =
  let web_btn = make_node ~name:"Web/Button" () in
  let mobile_card = make_node ~name:"Mobile/Card" () in
  let (_results, total, _avg_sim, critical, major) =
    compare_web_mobile ~web_nodes:[web_btn] ~mobile_nodes:[mobile_card] in
  check int "0 total" 0 total;
  check int "0 critical" 0 critical;
  check int "0 major" 0 major

(** ============== Main Test Runner ============== *)

let () =
  run "figma-compare-coverage" [
    "severity", [
      "to_string", `Quick, test_severity_to_string;
      "weight", `Quick, test_severity_weight;
    ];
    "float_eq", [
      "within", `Quick, test_float_eq_within;
      "outside", `Quick, test_float_eq_outside;
      "custom_tolerance", `Quick, test_float_eq_custom_tolerance;
    ];
    "percent_diff", [
      "basic", `Quick, test_percent_diff_basic;
      "zero_base", `Quick, test_percent_diff_zero_base;
      "both_zero", `Quick, test_percent_diff_both_zero;
    ];
    "compare_size", [
      "same", `Quick, test_compare_size_same;
      "width_major", `Quick, test_compare_size_different_width_major;
      "width_minor", `Quick, test_compare_size_different_width_minor;
      "width_info", `Quick, test_compare_size_different_width_info;
      "height_major", `Quick, test_compare_size_different_height_major;
      "height_minor", `Quick, test_compare_size_different_height_minor;
      "height_info", `Quick, test_compare_size_different_height_info;
      "aspect_ratio", `Quick, test_compare_size_aspect_ratio;
      "no_bbox", `Quick, test_compare_size_no_bbox;
      "one_bbox", `Quick, test_compare_size_one_bbox;
    ];
    "colors", [
      "rgba_to_hex", `Quick, test_rgba_to_hex;
      "distance_same", `Quick, test_color_distance_same;
      "distance_different", `Quick, test_color_distance_different;
      "distance_similar", `Quick, test_color_distance_similar;
      "compare_same", `Quick, test_compare_colors_same;
      "compare_major", `Quick, test_compare_colors_different_major;
      "compare_minor", `Quick, test_compare_colors_different_minor;
      "compare_small", `Quick, test_compare_colors_small_diff;
      "compare_none_some", `Quick, test_compare_colors_none_some;
      "compare_some_none", `Quick, test_compare_colors_some_none;
      "compare_none_none", `Quick, test_compare_colors_none_none;
    ];
    "typography", [
      "same", `Quick, test_compare_typography_same;
      "size_major", `Quick, test_compare_typography_size_major;
      "size_minor", `Quick, test_compare_typography_size_minor;
      "size_within", `Quick, test_compare_typography_size_within;
      "family", `Quick, test_compare_typography_family;
      "weight", `Quick, test_compare_typography_weight;
      "some_none", `Quick, test_compare_typography_some_none;
      "none_some", `Quick, test_compare_typography_none_some;
      "none_none", `Quick, test_compare_typography_none_none;
    ];
    "layout", [
      "mode_to_string", `Quick, test_layout_mode_to_string;
      "same", `Quick, test_compare_layout_same;
      "different_mode", `Quick, test_compare_layout_mode;
      "different_gap", `Quick, test_compare_layout_gap;
      "different_padding", `Quick, test_compare_layout_padding;
      "partial_padding", `Quick, test_compare_layout_padding_partial;
    ];
    "structure", [
      "same_children", `Quick, test_compare_structure_same_children;
      "different_minor", `Quick, test_compare_structure_different_minor;
      "different_major", `Quick, test_compare_structure_different_major;
      "different_type", `Quick, test_compare_structure_different_type;
      "same_type", `Quick, test_compare_structure_same_type;
    ];
    "compare_nodes", [
      "identical", `Quick, test_compare_nodes_identical;
      "with_diffs", `Quick, test_compare_nodes_with_diffs;
      "many_diffs", `Quick, test_compare_nodes_many_diffs;
      "similarity_floor", `Quick, test_compare_nodes_similarity_floor;
    ];
    "result_to_string", [
      "no_diff", `Quick, test_result_to_string_no_diff;
      "with_diffs", `Quick, test_result_to_string_with_diffs;
      "all_categories", `Quick, test_result_to_string_all_categories;
    ];
    "normalize_name", [
      "web", `Quick, test_normalize_name_web;
      "mobile", `Quick, test_normalize_name_mobile;
      "desktop", `Quick, test_normalize_name_desktop;
      "ios", `Quick, test_normalize_name_ios;
      "android", `Quick, test_normalize_name_android;
      "preserved", `Quick, test_normalize_name_preserved;
      "exact_prefix", `Quick, test_normalize_name_exact_prefix;
    ];
    "find_matching_pairs", [
      "basic", `Quick, test_find_matching_pairs_basic;
      "no_match", `Quick, test_find_matching_pairs_no_match;
      "multiple", `Quick, test_find_matching_pairs_multiple;
      "empty", `Quick, test_find_matching_pairs_empty;
    ];
    "compare_web_mobile", [
      "match", `Quick, test_compare_web_mobile_match;
      "with_diffs", `Quick, test_compare_web_mobile_with_diffs;
      "empty", `Quick, test_compare_web_mobile_empty;
      "no_matches", `Quick, test_compare_web_mobile_no_matches;
    ];
  ]
