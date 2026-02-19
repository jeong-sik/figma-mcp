(** B2 IoU Integration Tests — Academic Reference Values

    Comprehensive tests for IoU, GIoU, and DIoU metrics with reference
    values from academic literature.

    References:
    - IoU: Jaccard Index (Jaccard, 1912) — standard bounding box overlap
    - GIoU: Rezatofighi et al., "Generalized Intersection over Union" (CVPR 2019)
      https://giou.stanford.edu/
    - DIoU: Zheng et al., "Distance-IoU Loss: Faster and Better Learning for
      Bounding Box Regression" (AAAI 2020)
      https://arxiv.org/abs/1911.08287
*)

open Alcotest

(* ============== Test Helpers ============== *)

let float_eq ?(eps=0.0001) a b = Float.abs (a -. b) < eps

let f eps =
  testable (fun ppf v -> Fmt.pf ppf "%.6f" v) (float_eq ~eps)

(* ============== IoU (Intersection over Union) Tests ============== *)
(* Reference: Jaccard Index (1912) — standard bounding box overlap metric *)

(** Test 1: Identical boxes should yield IoU = 1.0 *)
let test_iou_identical_boxes () =
  let box = (10.0, 20.0, 100.0, 200.0) in
  let iou = Figma_similarity.iou box box in
  check (f 0.0001) "identical boxes -> IoU=1.0" 1.0 iou

(** Test 2: Non-overlapping boxes should yield IoU = 0.0 *)
let test_iou_non_overlapping_horizontal () =
  (* Box A: [0, 100] x [0, 100] *)
  (* Box B: [200, 300] x [0, 100] — completely to the right *)
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (200.0, 0.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.0001) "non-overlapping horizontal -> IoU=0.0" 0.0 iou

let test_iou_non_overlapping_vertical () =
  (* Box A: [0, 100] x [0, 100] *)
  (* Box B: [0, 100] x [200, 300] — completely below *)
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (0.0, 200.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.0001) "non-overlapping vertical -> IoU=0.0" 0.0 iou

let test_iou_non_overlapping_diagonal () =
  (* Box A: [0, 50] x [0, 50] *)
  (* Box B: [100, 150] x [100, 150] — diagonal separation *)
  let box_a = (0.0, 0.0, 50.0, 50.0) in
  let box_b = (100.0, 100.0, 50.0, 50.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.0001) "non-overlapping diagonal -> IoU=0.0" 0.0 iou

(** Test 3: Partial overlap with known IoU values *)

(* 50% horizontal overlap:
   Box A: [0, 100] x [0, 100], area = 10000
   Box B: [50, 150] x [0, 100], area = 10000
   Intersection: [50, 100] x [0, 100], area = 50 * 100 = 5000
   Union: 10000 + 10000 - 5000 = 15000
   IoU = 5000 / 15000 = 1/3 ≈ 0.3333
*)
let test_iou_half_horizontal_overlap () =
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (50.0, 0.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.001) "50% horizontal overlap -> IoU=1/3" (1.0 /. 3.0) iou

(* 50% vertical overlap:
   Box A: [0, 100] x [0, 100], area = 10000
   Box B: [0, 100] x [50, 150], area = 10000
   Intersection: [0, 100] x [50, 100], area = 100 * 50 = 5000
   Union: 15000
   IoU = 5000 / 15000 = 1/3
*)
let test_iou_half_vertical_overlap () =
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (0.0, 50.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.001) "50% vertical overlap -> IoU=1/3" (1.0 /. 3.0) iou

(* Corner overlap:
   Box A: [0, 100] x [0, 100], area = 10000
   Box B: [50, 150] x [50, 150], area = 10000
   Intersection: [50, 100] x [50, 100], area = 50 * 50 = 2500
   Union: 17500
   IoU = 2500 / 17500 = 1/7 ≈ 0.1429
*)
let test_iou_corner_overlap () =
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (50.0, 50.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.001) "corner overlap -> IoU=1/7" (1.0 /. 7.0) iou

(* 25% overlap (small intersection):
   Box A: [0, 200] x [0, 200], area = 40000
   Box B: [100, 200] x [100, 200], area = 10000
   Intersection: [100, 200] x [100, 200], area = 10000
   Union: 40000 + 10000 - 10000 = 40000
   IoU = 10000 / 40000 = 0.25
*)
let test_iou_quarter_overlap () =
  let box_a = (0.0, 0.0, 200.0, 200.0) in
  let box_b = (100.0, 100.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.001) "25% overlap -> IoU=0.25" 0.25 iou

(** Test 4: One box fully contained in another *)
let test_iou_contained_box () =
  (* Box A: [0, 100] x [0, 100], area = 10000 *)
  (* Box B: [25, 75] x [25, 75], area = 2500 *)
  (* Intersection = Box B = 2500 *)
  (* Union = Box A = 10000 *)
  (* IoU = 2500 / 10000 = 0.25 *)
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (25.0, 25.0, 50.0, 50.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.001) "contained box -> IoU=0.25" 0.25 iou

(** Test 5: Edge case - touching boxes (zero intersection area) *)
let test_iou_touching_edge () =
  (* Box A: [0, 100] x [0, 100] *)
  (* Box B: [100, 200] x [0, 100] — touches at x=100 *)
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (100.0, 0.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.0001) "touching edge -> IoU=0.0" 0.0 iou

(** Test 6: Zero-area boxes *)
let test_iou_zero_area_both () =
  let box_a = (0.0, 0.0, 0.0, 0.0) in
  let box_b = (0.0, 0.0, 0.0, 0.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.0001) "zero area both -> IoU=0.0" 0.0 iou

let test_iou_zero_area_one () =
  let box_a = (0.0, 0.0, 0.0, 0.0) in
  let box_b = (0.0, 0.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  check (f 0.0001) "zero area one -> IoU=0.0" 0.0 iou

(* ============== GIoU (Generalized IoU) Tests ============== *)
(* Reference: Rezatofighi et al., CVPR 2019
   GIoU = IoU - (C - U) / C
   where C = area of smallest enclosing box, U = union area
   Range: [-1, 1], where -1 = worst, 0 = no overlap, 1 = perfect match
*)

(** GIoU = 1.0 for identical boxes *)
let test_giou_identical_boxes () =
  let box = (10.0, 20.0, 100.0, 200.0) in
  let giou = Figma_similarity.giou box box in
  check (f 0.0001) "identical boxes -> GIoU=1.0" 1.0 giou

(** GIoU < 0 for non-overlapping boxes (key property from CVPR 2019 paper) *)
let test_giou_non_overlapping_negative () =
  let box_a = (0.0, 0.0, 50.0, 50.0) in
  let box_b = (100.0, 100.0, 50.0, 50.0) in
  let giou = Figma_similarity.giou box_a box_b in
  check bool "non-overlapping -> GIoU < 0" true (giou < 0.0)

(* Specific GIoU calculation for non-overlapping:
   Box A: [0, 50] x [0, 50], area = 2500
   Box B: [100, 150] x [100, 150], area = 2500
   IoU = 0
   Enclosing box C: [0, 150] x [0, 150], area = 22500
   Union = 5000
   GIoU = 0 - (22500 - 5000) / 22500 = -17500/22500 = -0.7778
*)
let test_giou_non_overlapping_known_value () =
  let box_a = (0.0, 0.0, 50.0, 50.0) in
  let box_b = (100.0, 100.0, 50.0, 50.0) in
  let giou = Figma_similarity.giou box_a box_b in
  check (f 0.01) "non-overlapping -> GIoU≈-0.78" (-0.7778) giou

(** GIoU = IoU for perfect overlap (since C = U) *)
let test_giou_partial_overlap () =
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (50.0, 0.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  let giou = Figma_similarity.giou box_a box_b in
  (* GIoU should be positive and close to IoU for partial overlap *)
  check bool "partial overlap -> GIoU > 0" true (giou > 0.0);
  check bool "partial overlap -> GIoU <= IoU" true (giou <= iou +. 0.0001)

(** GIoU for contained box *)
let test_giou_contained_box () =
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (25.0, 25.0, 50.0, 50.0) in
  let iou = Figma_similarity.iou box_a box_b in
  let giou = Figma_similarity.giou box_a box_b in
  (* When one box contains another, enclosing = larger box, so GIoU = IoU *)
  check (f 0.001) "contained -> GIoU = IoU" iou giou

(** GIoU to similarity conversion *)
let test_giou_to_similarity_range () =
  let sim_max = Figma_similarity.giou_to_similarity 1.0 in
  let sim_mid = Figma_similarity.giou_to_similarity 0.0 in
  let sim_min = Figma_similarity.giou_to_similarity (-1.0) in
  check (f 0.01) "GIoU=1 -> 100%" 100.0 sim_max;
  check (f 0.01) "GIoU=0 -> 50%" 50.0 sim_mid;
  check (f 0.01) "GIoU=-1 -> 0%" 0.0 sim_min

(* ============== DIoU (Distance IoU) Tests ============== *)
(* Reference: Zheng et al., AAAI 2020
   DIoU = IoU - (rho^2 / c^2)
   where rho = center distance, c = diagonal of enclosing box
   Range: [-1, 1], incorporates both overlap AND distance
*)

(** DIoU = 1.0 for identical boxes (rho = 0) *)
let test_diou_identical_boxes () =
  let box = (10.0, 20.0, 100.0, 200.0) in
  let diou = Figma_similarity.diou box box in
  check (f 0.0001) "identical boxes -> DIoU=1.0" 1.0 diou

(** DIoU < IoU for offset boxes (center distance penalty) *)
let test_diou_offset_penalty () =
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (50.0, 50.0, 100.0, 100.0) in
  let iou = Figma_similarity.iou box_a box_b in
  let diou = Figma_similarity.diou box_a box_b in
  (* DIoU should be less than IoU due to center distance penalty *)
  check bool "offset -> DIoU < IoU" true (diou < iou)

(* Specific DIoU calculation:
   Box A: [0, 100] x [0, 100], center = (50, 50)
   Box B: [50, 150] x [50, 150], center = (100, 100)
   IoU = 1/7 ≈ 0.1429
   rho^2 = (100-50)^2 + (100-50)^2 = 5000
   Enclosing: [0, 150] x [0, 150], c^2 = 150^2 + 150^2 = 45000
   DIoU = 0.1429 - 5000/45000 = 0.1429 - 0.1111 = 0.0318
*)
let test_diou_corner_overlap_known_value () =
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (50.0, 50.0, 100.0, 100.0) in
  let diou = Figma_similarity.diou box_a box_b in
  check (f 0.01) "corner overlap -> DIoU≈0.03" 0.03 diou

(** DIoU < 0 for non-overlapping boxes with distance penalty *)
let test_diou_non_overlapping () =
  let box_a = (0.0, 0.0, 50.0, 50.0) in
  let box_b = (100.0, 100.0, 50.0, 50.0) in
  let diou = Figma_similarity.diou box_a box_b in
  check bool "non-overlapping -> DIoU < 0" true (diou < 0.0)

(** DIoU for touching boxes (same center distance as partial overlap case) *)
let test_diou_touching () =
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (100.0, 0.0, 100.0, 100.0) in
  let diou = Figma_similarity.diou box_a box_b in
  (* IoU = 0, but DIoU should still be negative due to distance *)
  check bool "touching -> DIoU <= 0" true (diou <= 0.0)

(** DIoU to similarity conversion *)
let test_diou_to_similarity_range () =
  let sim_max = Figma_similarity.diou_to_similarity 1.0 in
  let sim_mid = Figma_similarity.diou_to_similarity 0.0 in
  let sim_min = Figma_similarity.diou_to_similarity (-1.0) in
  check (f 0.01) "DIoU=1 -> 100%" 100.0 sim_max;
  check (f 0.01) "DIoU=0 -> 50%" 50.0 sim_mid;
  check (f 0.01) "DIoU=-1 -> 0%" 0.0 sim_min

(** DIoU vs GIoU comparison (DIoU penalizes center distance directly) *)
let test_diou_vs_giou_distance_awareness () =
  (* Same boxes, same overlap but different positions:
     DIoU penalizes center-to-center distance, GIoU penalizes enclosing area *)

  (* Same size boxes at different distances *)
  let box_a = (0.0, 0.0, 50.0, 50.0) in

  (* Closer non-overlapping *)
  let box_close = (60.0, 0.0, 50.0, 50.0) in
  let diou_close = Figma_similarity.diou box_a box_close in
  let giou_close = Figma_similarity.giou box_a box_close in

  (* Farther non-overlapping (same relative position, scaled distance) *)
  let box_far = (200.0, 0.0, 50.0, 50.0) in
  let diou_far = Figma_similarity.diou box_a box_far in
  let giou_far = Figma_similarity.giou box_a box_far in

  (* Both DIoU and GIoU decrease with distance *)
  check bool "DIoU decreases with distance" true (diou_close > diou_far);
  check bool "GIoU decreases with distance" true (giou_close > giou_far);

  (* DIoU incorporates center distance penalty, so far box should be more negative *)
  check bool "DIoU far < GIoU far (center penalty)" true (diou_far < giou_far)

(** Zero diagonal edge case (both boxes are points at same location) *)
let test_diou_zero_diagonal () =
  let box_a = (5.0, 5.0, 0.0, 0.0) in
  let box_b = (5.0, 5.0, 0.0, 0.0) in
  let diou = Figma_similarity.diou box_a box_b in
  (* c^2 = 0, so function returns IoU (which is also 0 for zero-area boxes) *)
  check (f 0.0001) "zero diagonal -> DIoU=0" 0.0 diou

(* ============== Extended Box Metrics Integration Tests ============== *)

let test_extended_box_metrics_complete () =
  let box_a = (0.0, 0.0, 100.0, 100.0) in
  let box_b = (50.0, 50.0, 100.0, 100.0) in
  let metrics = Figma_similarity.compute_extended_box_metrics box_a box_b in

  (* Verify all metrics are computed *)
  check bool "iou_value set" true (metrics.iou_value >= 0.0);
  check bool "giou_value set" true (metrics.giou_value >= -1.0);
  check bool "diou_value set" true (metrics.diou_value >= -1.0);
  check bool "center_distance > 0" true (metrics.center_distance > 0.0);

  (* Verify known center distance: centers at (50,50) and (100,100) *)
  (* sqrt((100-50)^2 + (100-50)^2) = sqrt(5000) ≈ 70.71 *)
  check (f 0.1) "center distance ≈70.71" 70.71 metrics.center_distance

let test_extended_box_metrics_identical () =
  let box = (10.0, 20.0, 100.0, 200.0) in
  let metrics = Figma_similarity.compute_extended_box_metrics box box in

  check (f 0.0001) "identical IoU=1" 1.0 metrics.iou_value;
  check (f 0.0001) "identical GIoU=1" 1.0 metrics.giou_value;
  check (f 0.0001) "identical DIoU=1" 1.0 metrics.diou_value;
  check (f 0.0001) "identical center_dist=0" 0.0 metrics.center_distance

(* ============== Test Runner ============== *)

let () =
  run "B2 IoU Integration" [
    ("IoU Basic", [
      test_case "identical boxes" `Quick test_iou_identical_boxes;
      test_case "non-overlapping horizontal" `Quick test_iou_non_overlapping_horizontal;
      test_case "non-overlapping vertical" `Quick test_iou_non_overlapping_vertical;
      test_case "non-overlapping diagonal" `Quick test_iou_non_overlapping_diagonal;
    ]);
    ("IoU Partial Overlap", [
      test_case "50% horizontal overlap" `Quick test_iou_half_horizontal_overlap;
      test_case "50% vertical overlap" `Quick test_iou_half_vertical_overlap;
      test_case "corner overlap" `Quick test_iou_corner_overlap;
      test_case "25% overlap" `Quick test_iou_quarter_overlap;
    ]);
    ("IoU Edge Cases", [
      test_case "contained box" `Quick test_iou_contained_box;
      test_case "touching edge" `Quick test_iou_touching_edge;
      test_case "zero area both" `Quick test_iou_zero_area_both;
      test_case "zero area one" `Quick test_iou_zero_area_one;
    ]);
    ("GIoU CVPR 2019", [
      test_case "identical boxes" `Quick test_giou_identical_boxes;
      test_case "non-overlapping negative" `Quick test_giou_non_overlapping_negative;
      test_case "non-overlapping known value" `Quick test_giou_non_overlapping_known_value;
      test_case "partial overlap" `Quick test_giou_partial_overlap;
      test_case "contained box" `Quick test_giou_contained_box;
      test_case "to similarity range" `Quick test_giou_to_similarity_range;
    ]);
    ("DIoU AAAI 2020", [
      test_case "identical boxes" `Quick test_diou_identical_boxes;
      test_case "offset penalty" `Quick test_diou_offset_penalty;
      test_case "corner overlap known" `Quick test_diou_corner_overlap_known_value;
      test_case "non-overlapping" `Quick test_diou_non_overlapping;
      test_case "touching" `Quick test_diou_touching;
      test_case "to similarity range" `Quick test_diou_to_similarity_range;
      test_case "vs GIoU distance" `Quick test_diou_vs_giou_distance_awareness;
      test_case "zero diagonal" `Quick test_diou_zero_diagonal;
    ]);
    ("Extended Metrics", [
      test_case "complete computation" `Quick test_extended_box_metrics_complete;
      test_case "identical boxes" `Quick test_extended_box_metrics_identical;
    ]);
  ]
