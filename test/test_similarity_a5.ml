(** Coverage A5 tests for figma_similarity.ml — precise numeric assertions
    on all pure color math, IoU/GIoU/DIoU, TED, and formatting functions.

    Complements existing test_similarity_effects_coverage.ml (loose range checks)
    with exact reference values and untouched branches.
*)

open Alcotest

(* ============== Test Helpers ============== *)

let float_eq ?(eps=0.01) a b = Float.abs (a -. b) < eps

let f eps =
  testable (fun ppf v -> Fmt.pf ppf "%.6f" v) (float_eq ~eps)

(* ============== linearize_rgb ============== *)

let test_linearize_rgb_zero () =
  let r = Figma_similarity.linearize_rgb 0.0 in
  check (f 0.0001) "0.0 stays 0.0" 0.0 r

let test_linearize_rgb_one () =
  let r = Figma_similarity.linearize_rgb 1.0 in
  check (f 0.0001) "1.0 stays 1.0" 1.0 r

let test_linearize_rgb_below_threshold () =
  (* 0.04045 is the threshold; 0.04 is below *)
  let r = Figma_similarity.linearize_rgb 0.04 in
  (* linear region: 0.04 / 12.92 = 0.003096 *)
  check (f 0.0001) "linear region" 0.003096 r

let test_linearize_rgb_above_threshold () =
  (* 0.5: ((0.5 + 0.055) / 1.055) ^ 2.4 = 0.214041 *)
  let r = Figma_similarity.linearize_rgb 0.5 in
  check (f 0.001) "gamma region 0.5" 0.2140 r

let test_linearize_rgb_at_threshold () =
  (* Exactly at threshold 0.04045: linear path *)
  let r = Figma_similarity.linearize_rgb 0.04045 in
  check (f 0.0001) "at threshold" (0.04045 /. 12.92) r

(* ============== rgb_to_xyz ============== *)

let test_rgb_to_xyz_black () =
  let (x, y, z) = Figma_similarity.rgb_to_xyz (0.0, 0.0, 0.0) in
  check (f 0.0001) "black X" 0.0 x;
  check (f 0.0001) "black Y" 0.0 y;
  check (f 0.0001) "black Z" 0.0 z

let test_rgb_to_xyz_white () =
  let (x, y, z) = Figma_similarity.rgb_to_xyz (1.0, 1.0, 1.0) in
  (* D65 white point: X=0.95047, Y=1.0, Z=1.08883 *)
  check (f 0.005) "white X" 0.9505 x;
  check (f 0.005) "white Y" 1.0 y;
  check (f 0.005) "white Z" 1.0888 z

let test_rgb_to_xyz_red () =
  let (x, y, z) = Figma_similarity.rgb_to_xyz (1.0, 0.0, 0.0) in
  (* sRGB red: X=0.4124, Y=0.2127, Z=0.0193 *)
  check (f 0.005) "red X" 0.4125 x;
  check (f 0.005) "red Y" 0.2127 y;
  check (f 0.005) "red Z" 0.0193 z

(* ============== xyz_to_lab ============== *)

let test_xyz_to_lab_d65_white () =
  (* D65 white point -> Lab(100, 0, 0) *)
  let (l, a, b) = Figma_similarity.xyz_to_lab (0.95047, 1.0, 1.08883) in
  check (f 0.1) "white L" 100.0 l;
  check (f 0.1) "white a" 0.0 a;
  check (f 0.1) "white b" 0.0 b

let test_xyz_to_lab_black () =
  let (l, _a, _b) = Figma_similarity.xyz_to_lab (0.0, 0.0, 0.0) in
  check (f 0.1) "black L" 0.0 l

let test_xyz_to_lab_below_threshold () =
  (* t <= 0.008856 triggers linear branch: (903.3 * t + 16) / 116 *)
  let (l, _a, _b) = Figma_similarity.xyz_to_lab (0.001, 0.001, 0.001) in
  check bool "very dark L < 1" true (l < 1.0 && l >= 0.0)

(* ============== rgb_to_lab ============== *)

let test_rgb_to_lab_black () =
  let (l, _a, _b) = Figma_similarity.rgb_to_lab (0.0, 0.0, 0.0) in
  check (f 0.1) "black L" 0.0 l

let test_rgb_to_lab_white () =
  let (l, a, b) = Figma_similarity.rgb_to_lab (1.0, 1.0, 1.0) in
  check (f 0.5) "white L" 100.0 l;
  check (f 0.5) "white a" 0.0 a;
  check (f 0.5) "white b" 0.0 b

let test_rgb_to_lab_red () =
  (* sRGB red -> Lab ~(53.23, 80.11, 67.22) *)
  let (l, a, b) = Figma_similarity.rgb_to_lab (1.0, 0.0, 0.0) in
  check (f 1.0) "red L" 53.23 l;
  check (f 1.0) "red a" 80.11 a;
  check (f 1.0) "red b" 67.22 b

(* ============== linear_rgb_to_oklab ============== *)

let test_linear_rgb_to_oklab_black () =
  let (l, a, b) = Figma_similarity.linear_rgb_to_oklab (0.0, 0.0, 0.0) in
  check (f 0.001) "black L" 0.0 l;
  check (f 0.001) "black a" 0.0 a;
  check (f 0.001) "black b" 0.0 b

let test_linear_rgb_to_oklab_white () =
  (* linear white (1,1,1) -> OKLab (1, 0, 0) *)
  let (l, a, b) = Figma_similarity.linear_rgb_to_oklab (1.0, 1.0, 1.0) in
  check (f 0.001) "white L" 1.0 l;
  check (f 0.001) "white a" 0.0 a;
  check (f 0.001) "white b" 0.0 b

(* ============== rgb_to_oklab ============== *)

let test_rgb_to_oklab_red () =
  (* sRGB red -> OKLab ~(0.628, 0.225, 0.126) *)
  let (l, a, b) = Figma_similarity.rgb_to_oklab (1.0, 0.0, 0.0) in
  check (f 0.005) "red L" 0.6280 l;
  check (f 0.005) "red a" 0.2249 a;
  check (f 0.005) "red b" 0.1260 b

let test_rgb_to_oklab_blue () =
  (* sRGB blue -> OKLab ~(0.452, -0.032, -0.312) *)
  let (l, a, b) = Figma_similarity.rgb_to_oklab (0.0, 0.0, 1.0) in
  check (f 0.005) "blue L" 0.4520 l;
  check (f 0.005) "blue a" (-0.0324) a;
  check (f 0.005) "blue b" (-0.3115) b

let test_rgb_to_oklab_50_gray () =
  let (l, a, b) = Figma_similarity.rgb_to_oklab (0.5, 0.5, 0.5) in
  check (f 0.005) "gray L" 0.5982 l;
  check (f 0.001) "gray a" 0.0 a;
  check (f 0.001) "gray b" 0.0 b

(* ============== oklab_distance ============== *)

let test_oklab_distance_same () =
  let d = Figma_similarity.oklab_distance (0.5, 0.1, -0.1) (0.5, 0.1, -0.1) in
  check (f 0.0001) "same color" 0.0 d

let test_oklab_distance_L_only () =
  (* Only L differs: sqrt((1.0-0.0)^2) = 1.0 *)
  let d = Figma_similarity.oklab_distance (0.0, 0.0, 0.0) (1.0, 0.0, 0.0) in
  check (f 0.0001) "L axis only" 1.0 d

let test_oklab_distance_a_only () =
  (* sqrt(0.5^2) = 0.5 *)
  let d = Figma_similarity.oklab_distance (0.5, 0.0, 0.0) (0.5, 0.5, 0.0) in
  check (f 0.0001) "a axis only" 0.5 d

(* ============== color_distance_oklab ============== *)

let test_color_distance_oklab_same () =
  let d = Figma_similarity.color_distance_oklab (1.0, 0.0, 0.0) (1.0, 0.0, 0.0) in
  check (f 0.0001) "same color" 0.0 d

let test_color_distance_oklab_bw () =
  let d = Figma_similarity.color_distance_oklab (0.0, 0.0, 0.0) (1.0, 1.0, 1.0) in
  check (f 0.05) "black-white" 1.0 d

(* ============== rgba_distance_oklab ============== *)

let test_rgba_distance_oklab_same () =
  let open Figma_types in
  let c : rgba = { r = 0.5; g = 0.3; b = 0.8; a = 1.0 } in
  let d = Figma_similarity.rgba_distance_oklab c c in
  check (f 0.0001) "same rgba" 0.0 d

let test_rgba_distance_oklab_different () =
  let open Figma_types in
  let c1 : rgba = { r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
  let c2 : rgba = { r = 0.0; g = 0.0; b = 1.0; a = 1.0 } in
  let d = Figma_similarity.rgba_distance_oklab c1 c2 in
  check bool "red-blue positive" true (d > 0.0)

(* ============== oklab_to_similarity ============== *)

let test_oklab_to_similarity_zero () =
  let s = Figma_similarity.oklab_to_similarity 0.0 in
  check (f 0.01) "0 dist -> 100%" 100.0 s

let test_oklab_to_similarity_jnd () =
  (* 0.02 JND: 100 * exp(-0.02 * 10) = 100 * exp(-0.2) = 81.87 *)
  let s = Figma_similarity.oklab_to_similarity 0.02 in
  check (f 0.5) "JND distance" 81.87 s

let test_oklab_to_similarity_large () =
  let s = Figma_similarity.oklab_to_similarity 1.0 in
  (* 100 * exp(-10) = 0.00454 *)
  check bool "large dist near 0" true (s < 0.01)

(* ============== ciede2000 ============== *)

let test_ciede2000_same_color () =
  let de = Figma_similarity.ciede2000 (50.0, 25.0, -10.0) (50.0, 25.0, -10.0) in
  check (f 0.0001) "same color" 0.0 de

let test_ciede2000_sharma_pair_1 () =
  (* Sharma et al. 2005, test pair 1 *)
  let de = Figma_similarity.ciede2000
    (50.0, 2.6772, -79.7751) (50.0, 0.0, -82.7485) in
  check (f 0.001) "Sharma pair 1" 2.0425 de

let test_ciede2000_sharma_pair_4 () =
  (* Sharma pair 4: expected 1.0 *)
  let de = Figma_similarity.ciede2000
    (50.0, -1.3802, -84.2814) (50.0, 0.0, -82.7485) in
  check (f 0.001) "Sharma pair 4" 1.0 de

let test_ciede2000_sharma_pair_7 () =
  (* Achromatic case: both near zero chroma *)
  let de = Figma_similarity.ciede2000
    (50.0, 0.0, 0.0) (50.0, -1.0, 2.0) in
  check (f 0.001) "Sharma pair 7" 2.3669 de

let test_ciede2000_sharma_pair_9 () =
  (* Near-achromatic hue angle wrap *)
  let de = Figma_similarity.ciede2000
    (50.0, 2.49, -0.001) (50.0, -2.49, 0.0009) in
  check (f 0.001) "Sharma pair 9" 7.1792 de

let test_ciede2000_sharma_pair_17 () =
  (* Large L difference *)
  let de = Figma_similarity.ciede2000
    (50.0, 2.5, 0.0) (73.0, 25.0, -18.0) in
  check (f 0.001) "Sharma pair 17" 27.1492 de

let test_ciede2000_sharma_pair_25 () =
  let de = Figma_similarity.ciede2000
    (60.2574, -34.0099, 36.2677) (60.4626, -34.1751, 39.4387) in
  check (f 0.001) "Sharma pair 25" 1.2644 de

let test_ciede2000_achromatic_both () =
  (* Both a=0, b=0: achromatic, h' forced to 0, delta_h'=0 *)
  let de = Figma_similarity.ciede2000 (50.0, 0.0, 0.0) (60.0, 0.0, 0.0) in
  check bool "achromatic L-only diff" true (de > 0.0)

let test_ciede2000_hue_wrap_gt180 () =
  (* h2' - h1' > 180: triggers dh - 360 branch *)
  let de = Figma_similarity.ciede2000
    (50.0, 2.5, 0.0) (50.0, 0.0, -2.5) in
  check (f 0.001) "hue wrap >180" 4.3065 de

let test_ciede2000_weighted () =
  let de_default = Figma_similarity.ciede2000
    (50.0, 10.0, 0.0) (60.0, 10.0, 0.0) in
  let de_kl2 = Figma_similarity.ciede2000 ~kl:2.0
    (50.0, 10.0, 0.0) (60.0, 10.0, 0.0) in
  check bool "kL=2 reduces L impact" true (de_kl2 < de_default)

let test_ciede2000_kc_weight () =
  let de_default = Figma_similarity.ciede2000
    (50.0, 30.0, 0.0) (50.0, 10.0, 0.0) in
  let de_kc2 = Figma_similarity.ciede2000 ~kc:2.0
    (50.0, 30.0, 0.0) (50.0, 10.0, 0.0) in
  check bool "kC=2 reduces C impact" true (de_kc2 < de_default)

(* ============== color_distance_ciede2000 ============== *)

let test_color_distance_ciede2000_same () =
  let de = Figma_similarity.color_distance_ciede2000 (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) in
  check (f 0.0001) "same gray" 0.0 de

let test_color_distance_ciede2000_rg () =
  let de = Figma_similarity.color_distance_ciede2000 (1.0, 0.0, 0.0) (0.0, 1.0, 0.0) in
  check bool "red-green large delta" true (de > 50.0)

(* ============== rgba_distance_ciede2000 ============== *)

let test_rgba_distance_ciede2000_same () =
  let open Figma_types in
  let c : rgba = { r = 0.5; g = 0.5; b = 0.5; a = 1.0 } in
  let de = Figma_similarity.rgba_distance_ciede2000 c c in
  check (f 0.0001) "same rgba" 0.0 de

(* ============== delta_e_to_similarity ============== *)

let test_delta_e_to_similarity_zero () =
  let s = Figma_similarity.delta_e_to_similarity 0.0 in
  check (f 0.01) "0 -> 100%" 100.0 s

let test_delta_e_to_similarity_50 () =
  (* 100 * exp(-50/50) = 100 * exp(-1) = 36.79 *)
  let s = Figma_similarity.delta_e_to_similarity 50.0 in
  check (f 0.5) "50 -> ~36.8%" 36.79 s

let test_delta_e_to_similarity_100 () =
  (* 100 * exp(-100/50) = 100 * exp(-2) = 13.53 *)
  let s = Figma_similarity.delta_e_to_similarity 100.0 in
  check (f 0.5) "100 -> ~13.5%" 13.53 s

(* ============== IoU ============== *)

let test_iou_identical () =
  let v = Figma_similarity.iou (10.0, 20.0, 50.0, 60.0) (10.0, 20.0, 50.0, 60.0) in
  check (f 0.0001) "identical" 1.0 v

let test_iou_no_overlap () =
  let v = Figma_similarity.iou (0.0, 0.0, 10.0, 10.0) (100.0, 100.0, 10.0, 10.0) in
  check (f 0.0001) "no overlap" 0.0 v

let test_iou_partial () =
  (* (0,0,100,100) and (50,0,100,100): intersection 50x100=5000, union 15000 *)
  let v = Figma_similarity.iou (0.0, 0.0, 100.0, 100.0) (50.0, 0.0, 100.0, 100.0) in
  check (f 0.001) "partial overlap" (1.0 /. 3.0) v

let test_iou_zero_area () =
  let v = Figma_similarity.iou (0.0, 0.0, 0.0, 0.0) (0.0, 0.0, 0.0, 0.0) in
  check (f 0.0001) "zero area" 0.0 v

let test_iou_contained () =
  (* (25,25,50,50) fully inside (0,0,100,100) *)
  (* intersection=2500, union=10000+2500-2500=10000 -> 0.25 *)
  let v = Figma_similarity.iou (0.0, 0.0, 100.0, 100.0) (25.0, 25.0, 50.0, 50.0) in
  check (f 0.001) "contained" 0.25 v

(* ============== GIoU ============== *)

let test_giou_identical () =
  let v = Figma_similarity.giou (0.0, 0.0, 100.0, 100.0) (0.0, 0.0, 100.0, 100.0) in
  check (f 0.0001) "identical" 1.0 v

let test_giou_no_overlap_negative () =
  let v = Figma_similarity.giou (0.0, 0.0, 50.0, 50.0) (100.0, 100.0, 50.0, 50.0) in
  check bool "non-overlapping negative" true (v < 0.0)

let test_giou_zero_area () =
  let v = Figma_similarity.giou (0.0, 0.0, 0.0, 0.0) (0.0, 0.0, 0.0, 0.0) in
  check (f 0.0001) "zero area" 0.0 v

let test_giou_to_similarity_max () =
  check (f 0.01) "1.0 -> 100%" 100.0 (Figma_similarity.giou_to_similarity 1.0)

let test_giou_to_similarity_min () =
  check (f 0.01) "-1.0 -> 0%" 0.0 (Figma_similarity.giou_to_similarity (-1.0))

let test_giou_to_similarity_mid () =
  check (f 0.01) "0.0 -> 50%" 50.0 (Figma_similarity.giou_to_similarity 0.0)

(* ============== DIoU ============== *)

let test_diou_identical () =
  let v = Figma_similarity.diou (0.0, 0.0, 100.0, 100.0) (0.0, 0.0, 100.0, 100.0) in
  check (f 0.0001) "identical" 1.0 v

let test_diou_offset () =
  let v = Figma_similarity.diou (0.0, 0.0, 100.0, 100.0) (50.0, 50.0, 100.0, 100.0) in
  check bool "offset < 1" true (v < 1.0 && v > -1.0)

let test_diou_zero_diagonal () =
  (* Both zero-area boxes at same point -> c2=0, returns iou_val *)
  let v = Figma_similarity.diou (5.0, 5.0, 0.0, 0.0) (5.0, 5.0, 0.0, 0.0) in
  check (f 0.0001) "zero diagonal" 0.0 v

let test_diou_to_similarity_max () =
  check (f 0.01) "1.0 -> 100%" 100.0 (Figma_similarity.diou_to_similarity 1.0)

let test_diou_to_similarity_min () =
  check (f 0.01) "-1.0 -> 0%" 0.0 (Figma_similarity.diou_to_similarity (-1.0))

(* ============== node_iou / node_giou / node_diou with None bbox ============== *)

let test_node_giou_no_bbox () =
  let open Figma_types in
  let n1 = { default_node with bbox = None } in
  let n2 = { default_node with bbox = Some { x = 0.0; y = 0.0; width = 10.0; height = 10.0 } } in
  let v = Figma_similarity.node_giou n1 n2 in
  check (f 0.0001) "one None" 0.0 v

let test_node_diou_no_bbox () =
  let open Figma_types in
  let n = { default_node with bbox = None } in
  let v = Figma_similarity.node_diou n n in
  check (f 0.0001) "both None" 0.0 v

(* ============== Tree Edit Distance ============== *)

let test_ted_leaf_same_type () =
  let open Figma_types in
  let n = { default_node with node_type = Text; children = [] } in
  check int "same leaf" 0 (Figma_similarity.tree_edit_distance n n)

let test_ted_leaf_different_type () =
  let open Figma_types in
  let n1 = { default_node with node_type = Frame; children = [] } in
  let n2 = { default_node with node_type = Text; children = [] } in
  check int "different leaf" 1 (Figma_similarity.tree_edit_distance n1 n2)

let test_ted_one_has_children () =
  let open Figma_types in
  let child1 = { default_node with node_type = Text; children = [] } in
  let child2 = { default_node with node_type = Rectangle; children = [] } in
  let n1 = { default_node with node_type = Frame; children = [child1; child2] } in
  let n2 = { default_node with node_type = Frame; children = [] } in
  (* label_cost=0, n2=0 children -> cost = 0 + 2 *)
  check int "n2 has no children" 2 (Figma_similarity.tree_edit_distance n1 n2)

let test_ted_other_has_children () =
  let open Figma_types in
  let child = { default_node with node_type = Text; children = [] } in
  let n1 = { default_node with node_type = Frame; children = [] } in
  let n2 = { default_node with node_type = Frame; children = [child; child; child] } in
  (* label_cost=0, n1=0 children -> cost = 0 + 3 *)
  check int "n1 has no children" 3 (Figma_similarity.tree_edit_distance n1 n2)

let test_ted_dp_matching () =
  let open Figma_types in
  let text = { default_node with node_type = Text; children = [] } in
  let rect = { default_node with node_type = Rectangle; children = [] } in
  let n1 = { default_node with node_type = Frame; children = [text; rect] } in
  let n2 = { default_node with node_type = Frame; children = [rect; text] } in
  (* Swap: each child needs 1 replace -> DP picks min 2 *)
  check int "swapped children" 2 (Figma_similarity.tree_edit_distance n1 n2)

let test_ted_to_similarity_zero () =
  check (f 0.01) "0/10" 100.0 (Figma_similarity.ted_to_similarity 0 10)

let test_ted_to_similarity_full () =
  check (f 0.01) "10/10" 0.0 (Figma_similarity.ted_to_similarity 10 10)

let test_ted_to_similarity_zero_max () =
  check (f 0.01) "0/0" 100.0 (Figma_similarity.ted_to_similarity 0 0)

(* ============== metrics_to_string formatting branches ============== *)

let test_metrics_to_string_jnd_below () =
  let m : Figma_similarity.similarity_metrics = {
    color_delta_e = 1.5;  (* < 2.3 JND *)
    color_similarity = 97.0;
    layout_iou = 0.9; layout_similarity = 90.0;
    structure_ted = 0; structure_similarity = 100.0;
    overall_similarity = 95.0;
  } in
  let s = Figma_similarity.metrics_to_string m in
  check bool "contains JND annotation" true (String.length s > 0)

let test_metrics_to_string_slight () =
  let m : Figma_similarity.similarity_metrics = {
    color_delta_e = 3.0;  (* 2.3..5.0 *)
    color_similarity = 94.0;
    layout_iou = 0.8; layout_similarity = 80.0;
    structure_ted = 1; structure_similarity = 90.0;
    overall_similarity = 88.0;
  } in
  let s = Figma_similarity.metrics_to_string m in
  check bool "non-empty output" true (String.length s > 100)

let test_metrics_to_string_large_de () =
  let m : Figma_similarity.similarity_metrics = {
    color_delta_e = 50.0;  (* >= 5.0, no annotation *)
    color_similarity = 37.0;
    layout_iou = 0.1; layout_similarity = 10.0;
    structure_ted = 5; structure_similarity = 50.0;
    overall_similarity = 30.0;
  } in
  let s = Figma_similarity.metrics_to_string m in
  check bool "no annotation" true (String.length s > 100)

(* ============== extended_color_to_string branches ============== *)

let test_extended_color_to_string_identical () =
  let m = Figma_similarity.compute_extended_color_metrics
    (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) in
  let s = Figma_similarity.extended_color_to_string m in
  check bool "has table" true (String.length s > 50)

let test_extended_color_to_string_different () =
  let m = Figma_similarity.compute_extended_color_metrics
    (1.0, 0.0, 0.0) (0.0, 0.0, 1.0) in
  let s = Figma_similarity.extended_color_to_string m in
  check bool "has table" true (String.length s > 50)

(* ============== extended_box_to_string IoU annotation branches ============== *)

let test_extended_box_to_string_perfect () =
  let m = Figma_similarity.compute_extended_box_metrics
    (0.0, 0.0, 100.0, 100.0) (0.0, 0.0, 100.0, 100.0) in
  let s = Figma_similarity.extended_box_to_string m in
  check bool "iou >= 0.9 branch" true (String.length s > 50)

let test_extended_box_to_string_partial () =
  let m = Figma_similarity.compute_extended_box_metrics
    (0.0, 0.0, 100.0, 100.0) (25.0, 25.0, 100.0, 100.0) in
  let s = Figma_similarity.extended_box_to_string m in
  check bool "partial overlap" true (String.length s > 50)

let test_extended_box_to_string_none () =
  let m = Figma_similarity.compute_extended_box_metrics
    (0.0, 0.0, 10.0, 10.0) (100.0, 100.0, 10.0, 10.0) in
  let s = Figma_similarity.extended_box_to_string m in
  check bool "no overlap" true (String.length s > 50)

(* ============== rgba_to_lab ============== *)

let test_rgba_to_lab_black () =
  let open Figma_types in
  let c : rgba = { r = 0.0; g = 0.0; b = 0.0; a = 1.0 } in
  let (l, _a, _b) = Figma_similarity.rgba_to_lab c in
  check (f 0.1) "black L" 0.0 l

(* ============== compute_extended_color_metrics ============== *)

let test_extended_color_metrics_rgb_euclidean () =
  let m = Figma_similarity.compute_extended_color_metrics
    (1.0, 0.0, 0.0) (0.0, 1.0, 0.0) in
  (* sqrt(1+1) = 1.414 *)
  check (f 0.01) "rg euclidean" 1.414 m.rgb_euclidean

(* ============== compute_extended_box_metrics ============== *)

let test_extended_box_metrics_center_distance () =
  let m = Figma_similarity.compute_extended_box_metrics
    (0.0, 0.0, 100.0, 100.0) (100.0, 0.0, 100.0, 100.0) in
  (* centers: (50,50) and (150,50), distance=100 *)
  check (f 0.01) "center dist" 100.0 m.center_distance

(* ============== Test Runner ============== *)

let () =
  run "Similarity A5" [
    ("linearize_rgb", [
      test_case "zero" `Quick test_linearize_rgb_zero;
      test_case "one" `Quick test_linearize_rgb_one;
      test_case "below threshold" `Quick test_linearize_rgb_below_threshold;
      test_case "above threshold" `Quick test_linearize_rgb_above_threshold;
      test_case "at threshold" `Quick test_linearize_rgb_at_threshold;
    ]);
    ("rgb_to_xyz", [
      test_case "black" `Quick test_rgb_to_xyz_black;
      test_case "white" `Quick test_rgb_to_xyz_white;
      test_case "red" `Quick test_rgb_to_xyz_red;
    ]);
    ("xyz_to_lab", [
      test_case "D65 white" `Quick test_xyz_to_lab_d65_white;
      test_case "black" `Quick test_xyz_to_lab_black;
      test_case "below threshold" `Quick test_xyz_to_lab_below_threshold;
    ]);
    ("rgb_to_lab", [
      test_case "black" `Quick test_rgb_to_lab_black;
      test_case "white" `Quick test_rgb_to_lab_white;
      test_case "red" `Quick test_rgb_to_lab_red;
    ]);
    ("linear_rgb_to_oklab", [
      test_case "black" `Quick test_linear_rgb_to_oklab_black;
      test_case "white" `Quick test_linear_rgb_to_oklab_white;
    ]);
    ("rgb_to_oklab", [
      test_case "red" `Quick test_rgb_to_oklab_red;
      test_case "blue" `Quick test_rgb_to_oklab_blue;
      test_case "50% gray" `Quick test_rgb_to_oklab_50_gray;
    ]);
    ("oklab_distance", [
      test_case "same" `Quick test_oklab_distance_same;
      test_case "L only" `Quick test_oklab_distance_L_only;
      test_case "a only" `Quick test_oklab_distance_a_only;
    ]);
    ("color_distance_oklab", [
      test_case "same" `Quick test_color_distance_oklab_same;
      test_case "black-white" `Quick test_color_distance_oklab_bw;
    ]);
    ("rgba_distance_oklab", [
      test_case "same" `Quick test_rgba_distance_oklab_same;
      test_case "different" `Quick test_rgba_distance_oklab_different;
    ]);
    ("oklab_to_similarity", [
      test_case "zero" `Quick test_oklab_to_similarity_zero;
      test_case "JND" `Quick test_oklab_to_similarity_jnd;
      test_case "large" `Quick test_oklab_to_similarity_large;
    ]);
    ("ciede2000", [
      test_case "same color" `Quick test_ciede2000_same_color;
      test_case "Sharma pair 1" `Quick test_ciede2000_sharma_pair_1;
      test_case "Sharma pair 4" `Quick test_ciede2000_sharma_pair_4;
      test_case "Sharma pair 7" `Quick test_ciede2000_sharma_pair_7;
      test_case "Sharma pair 9" `Quick test_ciede2000_sharma_pair_9;
      test_case "Sharma pair 17" `Quick test_ciede2000_sharma_pair_17;
      test_case "Sharma pair 25" `Quick test_ciede2000_sharma_pair_25;
      test_case "achromatic both" `Quick test_ciede2000_achromatic_both;
      test_case "hue wrap >180" `Quick test_ciede2000_hue_wrap_gt180;
      test_case "kL weighted" `Quick test_ciede2000_weighted;
      test_case "kC weighted" `Quick test_ciede2000_kc_weight;
    ]);
    ("color_distance_ciede2000", [
      test_case "same" `Quick test_color_distance_ciede2000_same;
      test_case "red-green" `Quick test_color_distance_ciede2000_rg;
    ]);
    ("rgba_distance_ciede2000", [
      test_case "same" `Quick test_rgba_distance_ciede2000_same;
    ]);
    ("delta_e_to_similarity", [
      test_case "zero" `Quick test_delta_e_to_similarity_zero;
      test_case "50" `Quick test_delta_e_to_similarity_50;
      test_case "100" `Quick test_delta_e_to_similarity_100;
    ]);
    ("iou", [
      test_case "identical" `Quick test_iou_identical;
      test_case "no overlap" `Quick test_iou_no_overlap;
      test_case "partial" `Quick test_iou_partial;
      test_case "zero area" `Quick test_iou_zero_area;
      test_case "contained" `Quick test_iou_contained;
    ]);
    ("giou", [
      test_case "identical" `Quick test_giou_identical;
      test_case "negative" `Quick test_giou_no_overlap_negative;
      test_case "zero area" `Quick test_giou_zero_area;
      test_case "sim max" `Quick test_giou_to_similarity_max;
      test_case "sim min" `Quick test_giou_to_similarity_min;
      test_case "sim mid" `Quick test_giou_to_similarity_mid;
    ]);
    ("diou", [
      test_case "identical" `Quick test_diou_identical;
      test_case "offset" `Quick test_diou_offset;
      test_case "zero diagonal" `Quick test_diou_zero_diagonal;
      test_case "sim max" `Quick test_diou_to_similarity_max;
      test_case "sim min" `Quick test_diou_to_similarity_min;
    ]);
    ("node bbox None", [
      test_case "node_giou no bbox" `Quick test_node_giou_no_bbox;
      test_case "node_diou no bbox" `Quick test_node_diou_no_bbox;
    ]);
    ("tree_edit_distance", [
      test_case "same leaf" `Quick test_ted_leaf_same_type;
      test_case "different leaf" `Quick test_ted_leaf_different_type;
      test_case "n2 empty" `Quick test_ted_one_has_children;
      test_case "n1 empty" `Quick test_ted_other_has_children;
      test_case "DP matching" `Quick test_ted_dp_matching;
      test_case "sim 0/10" `Quick test_ted_to_similarity_zero;
      test_case "sim 10/10" `Quick test_ted_to_similarity_full;
      test_case "sim 0/0" `Quick test_ted_to_similarity_zero_max;
    ]);
    ("metrics_to_string", [
      test_case "JND below" `Quick test_metrics_to_string_jnd_below;
      test_case "slight diff" `Quick test_metrics_to_string_slight;
      test_case "large delta" `Quick test_metrics_to_string_large_de;
    ]);
    ("extended_color_to_string", [
      test_case "identical" `Quick test_extended_color_to_string_identical;
      test_case "different" `Quick test_extended_color_to_string_different;
    ]);
    ("extended_box_to_string", [
      test_case "perfect overlap" `Quick test_extended_box_to_string_perfect;
      test_case "partial overlap" `Quick test_extended_box_to_string_partial;
      test_case "no overlap" `Quick test_extended_box_to_string_none;
    ]);
    ("rgba_to_lab", [
      test_case "black" `Quick test_rgba_to_lab_black;
    ]);
    ("extended_color_metrics", [
      test_case "rgb euclidean" `Quick test_extended_color_metrics_rgb_euclidean;
    ]);
    ("extended_box_metrics", [
      test_case "center distance" `Quick test_extended_box_metrics_center_distance;
    ]);
  ]
