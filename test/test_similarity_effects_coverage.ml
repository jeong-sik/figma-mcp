(** Comprehensive Coverage Tests for Figma Similarity and Effects Modules

    Modules tested:
    - figma_similarity.ml (653 lines) - Color, IoU, Tree Edit Distance algorithms
    - figma_effects.ml (605 lines) - Effect system and mock handlers
    - figma_image_similarity.ml (350 lines) - Image comparison metrics

    Target: 30+ tests with real assertions
*)

open Alcotest

(* ============== Test Helpers ============== *)

let float_eq ?(eps=0.001) a b = Float.abs (a -. b) < eps

let float_approx_testable eps =
  testable (fun ppf f -> Fmt.pf ppf "%.6f" f) (float_eq ~eps)

(* ============== Figma_similarity Tests ============== *)

(** Test RGB to XYZ conversion with known values *)
let test_rgb_to_xyz () =
  (* Black: RGB(0,0,0) -> XYZ(0,0,0) *)
  let (x, y, z) = Figma_similarity.rgb_to_xyz (0.0, 0.0, 0.0) in
  check (float_approx_testable 0.0001) "black X" 0.0 x;
  check (float_approx_testable 0.0001) "black Y" 0.0 y;
  check (float_approx_testable 0.0001) "black Z" 0.0 z;

  (* White: RGB(1,1,1) -> XYZ approximately (0.95, 1.0, 1.089) *)
  let (x, y, z) = Figma_similarity.rgb_to_xyz (1.0, 1.0, 1.0) in
  check bool "white X in range" true (x > 0.9 && x < 1.0);
  check bool "white Y in range" true (y > 0.9 && y < 1.1);
  check bool "white Z in range" true (z > 1.0 && z < 1.15)

(** Test linearize_rgb gamma correction *)
let test_linearize_rgb () =
  (* Below threshold: linear scaling *)
  let result = Figma_similarity.linearize_rgb 0.01 in
  check bool "low value linear" true (result < 0.001);

  (* Above threshold: gamma correction *)
  let result = Figma_similarity.linearize_rgb 0.5 in
  check bool "mid value corrected" true (result > 0.2 && result < 0.25)

(** Test XYZ to Lab conversion *)
let test_xyz_to_lab () =
  (* D65 white point: XYZ(0.95047, 1.0, 1.08883) -> Lab(100, 0, 0) *)
  let (l, a, b) = Figma_similarity.xyz_to_lab (0.95047, 1.0, 1.08883) in
  check (float_approx_testable 0.5) "white L" 100.0 l;
  check (float_approx_testable 0.5) "white a" 0.0 a;
  check (float_approx_testable 0.5) "white b" 0.0 b

(** Test RGB to Lab conversion *)
let test_rgb_to_lab () =
  (* Black -> Lab(0, 0, 0) *)
  let (l, _a, _b) = Figma_similarity.rgb_to_lab (0.0, 0.0, 0.0) in
  check (float_approx_testable 1.0) "black L" 0.0 l;

  (* White -> Lab(100, 0, 0) approximately *)
  let (l, a, b) = Figma_similarity.rgb_to_lab (1.0, 1.0, 1.0) in
  check bool "white L near 100" true (l > 99.0 && l < 101.0);
  check bool "white a near 0" true (Float.abs a < 1.0);
  check bool "white b near 0" true (Float.abs b < 1.0)

(** Test OKLab color space conversion *)
let test_rgb_to_oklab () =
  (* Black -> OKLab(0, 0, 0) *)
  let (l, a, b) = Figma_similarity.rgb_to_oklab (0.0, 0.0, 0.0) in
  check (float_approx_testable 0.001) "black L" 0.0 l;
  check (float_approx_testable 0.001) "black a" 0.0 a;
  check (float_approx_testable 0.001) "black b" 0.0 b;

  (* White -> OKLab(1, 0, 0) *)
  let (l, a, b) = Figma_similarity.rgb_to_oklab (1.0, 1.0, 1.0) in
  check (float_approx_testable 0.001) "white L" 1.0 l;
  check (float_approx_testable 0.001) "white a" 0.0 a;
  check (float_approx_testable 0.001) "white b" 0.0 b

(** Test OKLab distance calculation *)
let test_oklab_distance () =
  (* Same color -> 0 distance *)
  let d = Figma_similarity.color_distance_oklab (1.0, 0.0, 0.0) (1.0, 0.0, 0.0) in
  check (float_approx_testable 0.0001) "same color" 0.0 d;

  (* Black to white -> distance ~1.0 *)
  let d = Figma_similarity.color_distance_oklab (0.0, 0.0, 0.0) (1.0, 1.0, 1.0) in
  check bool "black-white distance" true (d > 0.9 && d < 1.1)

(** Test oklab_to_similarity conversion *)
let test_oklab_to_similarity () =
  (* Distance 0 -> 100% similarity *)
  let sim = Figma_similarity.oklab_to_similarity 0.0 in
  check (float_approx_testable 0.01) "zero distance" 100.0 sim;

  (* Large distance -> low similarity *)
  let sim = Figma_similarity.oklab_to_similarity 1.0 in
  check bool "large distance low sim" true (sim < 10.0)

(** Test CIEDE2000 color difference *)
let test_ciede2000 () =
  (* Same color -> 0 difference *)
  let de = Figma_similarity.ciede2000 (50.0, 0.0, 0.0) (50.0, 0.0, 0.0) in
  check (float_approx_testable 0.0001) "same color" 0.0 de;

  (* Different colors -> positive difference *)
  let de = Figma_similarity.ciede2000 (50.0, 10.0, 0.0) (50.0, -10.0, 0.0) in
  check bool "different colors" true (de > 0.0)

(** Test CIEDE2000 with custom weights *)
let test_ciede2000_weighted () =
  let de1 = Figma_similarity.ciede2000 ~kl:1.0 ~kc:1.0 ~kh:1.0
    (50.0, 10.0, 0.0) (60.0, 10.0, 0.0) in
  let de2 = Figma_similarity.ciede2000 ~kl:2.0 ~kc:1.0 ~kh:1.0
    (50.0, 10.0, 0.0) (60.0, 10.0, 0.0) in
  (* Higher kL weight should reduce the effect of L difference *)
  check bool "weighted vs unweighted" true (de2 < de1)

(** Test delta_e_to_similarity conversion *)
let test_delta_e_to_similarity () =
  (* Delta E = 0 -> 100% similarity *)
  let sim = Figma_similarity.delta_e_to_similarity 0.0 in
  check (float_approx_testable 0.01) "zero delta" 100.0 sim;

  (* Delta E ~50 -> ~37% similarity (e^-1) *)
  let sim = Figma_similarity.delta_e_to_similarity 50.0 in
  check bool "mid delta" true (sim > 30.0 && sim < 45.0)

(** Test IoU calculation with overlapping boxes *)
let test_iou_overlap () =
  (* Identical boxes -> IoU = 1.0 *)
  let iou = Figma_similarity.iou (0.0, 0.0, 100.0, 100.0) (0.0, 0.0, 100.0, 100.0) in
  check (float_approx_testable 0.0001) "identical boxes" 1.0 iou;

  (* 50% overlap horizontally *)
  let iou = Figma_similarity.iou (0.0, 0.0, 100.0, 100.0) (50.0, 0.0, 100.0, 100.0) in
  (* Intersection = 50*100 = 5000, Union = 100*100 + 100*100 - 5000 = 15000 *)
  check (float_approx_testable 0.01) "50% horizontal overlap" (5000.0 /. 15000.0) iou

(** Test IoU with non-overlapping boxes *)
let test_iou_no_overlap () =
  let iou = Figma_similarity.iou (0.0, 0.0, 50.0, 50.0) (100.0, 100.0, 50.0, 50.0) in
  check (float_approx_testable 0.0001) "no overlap" 0.0 iou

(** Test IoU with zero-area boxes *)
let test_iou_zero_area () =
  let iou = Figma_similarity.iou (0.0, 0.0, 0.0, 0.0) (0.0, 0.0, 0.0, 0.0) in
  check (float_approx_testable 0.0001) "zero area" 0.0 iou

(** Test GIoU calculation *)
let test_giou () =
  (* Identical boxes -> GIoU = 1.0 *)
  let giou = Figma_similarity.giou (0.0, 0.0, 100.0, 100.0) (0.0, 0.0, 100.0, 100.0) in
  check (float_approx_testable 0.0001) "identical" 1.0 giou;

  (* Non-overlapping boxes -> GIoU < 0 *)
  let giou = Figma_similarity.giou (0.0, 0.0, 50.0, 50.0) (100.0, 100.0, 50.0, 50.0) in
  check bool "non-overlapping negative" true (giou < 0.0)

(** Test GIoU to similarity conversion *)
let test_giou_to_similarity () =
  (* GIoU = 1.0 -> 100% *)
  let sim = Figma_similarity.giou_to_similarity 1.0 in
  check (float_approx_testable 0.01) "max giou" 100.0 sim;

  (* GIoU = -1.0 -> 0% *)
  let sim = Figma_similarity.giou_to_similarity (-1.0) in
  check (float_approx_testable 0.01) "min giou" 0.0 sim;

  (* GIoU = 0.0 -> 50% *)
  let sim = Figma_similarity.giou_to_similarity 0.0 in
  check (float_approx_testable 0.01) "zero giou" 50.0 sim

(** Test DIoU calculation *)
let test_diou () =
  (* Identical boxes -> DIoU = 1.0 *)
  let diou = Figma_similarity.diou (0.0, 0.0, 100.0, 100.0) (0.0, 0.0, 100.0, 100.0) in
  check (float_approx_testable 0.0001) "identical" 1.0 diou;

  (* Offset boxes -> lower DIoU due to center distance *)
  let diou = Figma_similarity.diou (0.0, 0.0, 100.0, 100.0) (50.0, 50.0, 100.0, 100.0) in
  check bool "offset boxes" true (diou < 1.0 && diou > 0.0)

(** Test DIoU to similarity conversion *)
let test_diou_to_similarity () =
  let sim = Figma_similarity.diou_to_similarity 1.0 in
  check (float_approx_testable 0.01) "max diou" 100.0 sim;

  let sim = Figma_similarity.diou_to_similarity (-1.0) in
  check (float_approx_testable 0.01) "min diou" 0.0 sim

(** Test Tree Edit Distance with identical trees *)
let test_ted_identical () =
  let open Figma_types in
  let node = { default_node with node_type = Frame; children = [] } in
  let ted = Figma_similarity.tree_edit_distance node node in
  check int "identical trees" 0 ted

(** Test Tree Edit Distance with different root types *)
let test_ted_different_root () =
  let open Figma_types in
  let node1 = { default_node with node_type = Frame; children = [] } in
  let node2 = { default_node with node_type = Text; children = [] } in
  let ted = Figma_similarity.tree_edit_distance node1 node2 in
  check int "different root types" 1 ted

(** Test Tree Edit Distance with children *)
let test_ted_with_children () =
  let open Figma_types in
  let child = { default_node with node_type = Text; children = [] } in
  let node1 = { default_node with node_type = Frame; children = [child] } in
  let node2 = { default_node with node_type = Frame; children = [] } in
  let ted = Figma_similarity.tree_edit_distance node1 node2 in
  (* Delete one child *)
  check int "one child difference" 1 ted

(** Test TED to similarity conversion *)
let test_ted_to_similarity () =
  let sim = Figma_similarity.ted_to_similarity 0 10 in
  check (float_approx_testable 0.01) "zero distance" 100.0 sim;

  let sim = Figma_similarity.ted_to_similarity 5 10 in
  check (float_approx_testable 0.01) "half distance" 50.0 sim;

  let sim = Figma_similarity.ted_to_similarity 0 0 in
  check (float_approx_testable 0.01) "zero max" 100.0 sim

(** Test node IoU calculation *)
let test_node_iou () =
  let open Figma_types in
  let bbox1 = { x = 0.0; y = 0.0; width = 100.0; height = 100.0 } in
  let bbox2 = { x = 0.0; y = 0.0; width = 100.0; height = 100.0 } in
  let node1 = { default_node with bbox = Some bbox1 } in
  let node2 = { default_node with bbox = Some bbox2 } in
  let iou = Figma_similarity.node_iou node1 node2 in
  check (float_approx_testable 0.0001) "identical bbox" 1.0 iou

(** Test node IoU with missing bbox *)
let test_node_iou_no_bbox () =
  let open Figma_types in
  let node1 = { default_node with bbox = None } in
  let node2 = { default_node with bbox = None } in
  let iou = Figma_similarity.node_iou node1 node2 in
  check (float_approx_testable 0.0001) "no bbox" 0.0 iou

(** Test node GIoU calculation *)
let test_node_giou () =
  let open Figma_types in
  let bbox1 = { x = 0.0; y = 0.0; width = 100.0; height = 100.0 } in
  let bbox2 = { x = 50.0; y = 0.0; width = 100.0; height = 100.0 } in
  let node1 = { default_node with bbox = Some bbox1 } in
  let node2 = { default_node with bbox = Some bbox2 } in
  let giou = Figma_similarity.node_giou node1 node2 in
  check bool "partial overlap" true (giou > 0.0 && giou < 1.0)

(** Test node DIoU calculation *)
let test_node_diou () =
  let open Figma_types in
  let bbox = { x = 0.0; y = 0.0; width = 100.0; height = 100.0 } in
  let node1 = { default_node with bbox = Some bbox } in
  let node2 = { default_node with bbox = Some bbox } in
  let diou = Figma_similarity.node_diou node1 node2 in
  check (float_approx_testable 0.0001) "identical" 1.0 diou

(** Test rgba_to_lab conversion *)
let test_rgba_to_lab () =
  let open Figma_types in
  let white: rgba = { r = 1.0; g = 1.0; b = 1.0; a = 1.0 } in
  let (l, a, b) = Figma_similarity.rgba_to_lab white in
  check bool "white L near 100" true (l > 99.0);
  check bool "white a near 0" true (Float.abs a < 1.0);
  check bool "white b near 0" true (Float.abs b < 1.0)

(** Test rgba_distance_ciede2000 *)
let test_rgba_distance_ciede2000 () =
  let open Figma_types in
  let c1: rgba = { r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
  let c2: rgba = { r = 0.0; g = 1.0; b = 0.0; a = 1.0 } in
  let de = Figma_similarity.rgba_distance_ciede2000 c1 c2 in
  (* Red vs Green should have large difference *)
  check bool "red-green difference" true (de > 50.0)

(** Test rgba_distance_oklab *)
let test_rgba_distance_oklab () =
  let open Figma_types in
  let c1: rgba = { r = 0.0; g = 0.0; b = 0.0; a = 1.0 } in
  let c2: rgba = { r = 1.0; g = 1.0; b = 1.0; a = 1.0 } in
  let d = Figma_similarity.rgba_distance_oklab c1 c2 in
  check bool "black-white distance" true (d > 0.9 && d < 1.1)

(** Test compute_similarity with basic nodes *)
let test_compute_similarity () =
  let open Figma_types in
  let bbox = { x = 0.0; y = 0.0; width = 100.0; height = 100.0 } in
  let fill_color: rgba = { r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
  let fill = {
    paint_type = Solid; visible = true; opacity = 1.0;
    color = Some fill_color; gradient_stops = [];
    image_ref = None; scale_mode = None
  } in
  let node1 = { default_node with bbox = Some bbox; fills = [fill] } in
  let node2 = { default_node with bbox = Some bbox; fills = [fill] } in
  let metrics = Figma_similarity.compute_similarity node1 node2 in
  check bool "overall high similarity" true (metrics.overall_similarity > 90.0)

(** Test compute_similarity without fills *)
let test_compute_similarity_no_fills () =
  let open Figma_types in
  let bbox = { x = 0.0; y = 0.0; width = 100.0; height = 100.0 } in
  let node1 = { default_node with bbox = Some bbox; fills = [] } in
  let node2 = { default_node with bbox = Some bbox; fills = [] } in
  let metrics = Figma_similarity.compute_similarity node1 node2 in
  (* Color similarity should be 100% when no fills *)
  check (float_approx_testable 0.01) "no fills color sim" 100.0 metrics.color_similarity

(** Test extended color metrics *)
let test_extended_color_metrics () =
  let metrics = Figma_similarity.compute_extended_color_metrics
    (1.0, 0.0, 0.0) (1.0, 0.0, 0.0) in
  check (float_approx_testable 0.0001) "same color oklab dist" 0.0 metrics.oklab_distance;
  check (float_approx_testable 0.0001) "same color ciede dist" 0.0 metrics.ciede2000_distance;
  check (float_approx_testable 0.01) "same color oklab sim" 100.0 metrics.oklab_similarity

(** Test extended box metrics *)
let test_extended_box_metrics () =
  let box1 = (0.0, 0.0, 100.0, 100.0) in
  let box2 = (0.0, 0.0, 100.0, 100.0) in
  let metrics = Figma_similarity.compute_extended_box_metrics box1 box2 in
  check (float_approx_testable 0.0001) "iou" 1.0 metrics.iou_value;
  check (float_approx_testable 0.0001) "giou" 1.0 metrics.giou_value;
  check (float_approx_testable 0.0001) "diou" 1.0 metrics.diou_value;
  check (float_approx_testable 0.01) "center dist" 0.0 metrics.center_distance

(** Test metrics_to_string formatting *)
let test_metrics_to_string () =
  let metrics: Figma_similarity.similarity_metrics = {
    color_delta_e = 1.0;
    color_similarity = 99.0;
    layout_iou = 0.9;
    layout_similarity = 90.0;
    structure_ted = 1;
    structure_similarity = 90.0;
    overall_similarity = 92.0;
  } in
  let s = Figma_similarity.metrics_to_string metrics in
  check bool "contains table chars" true (String.length s > 100)

(** Test extended_color_to_string formatting *)
let test_extended_color_to_string () =
  let metrics = Figma_similarity.compute_extended_color_metrics
    (1.0, 0.0, 0.0) (0.0, 1.0, 0.0) in
  let s = Figma_similarity.extended_color_to_string metrics in
  check bool "contains table" true (String.length s > 50)

(** Test extended_box_to_string formatting *)
let test_extended_box_to_string () =
  let metrics = Figma_similarity.compute_extended_box_metrics
    (0.0, 0.0, 100.0, 100.0) (50.0, 50.0, 100.0, 100.0) in
  let s = Figma_similarity.extended_box_to_string metrics in
  check bool "contains table" true (String.length s > 50)

(* ============== Figma_effects Tests ============== *)

(** Test mock store creation *)
let test_create_mock_store () =
  let store = Figma_effects.create_mock_store () in
  check bool "files table created" true (Hashtbl.length store.files = 0);
  check bool "nodes table created" true (Hashtbl.length store.nodes = 0)

(** Test run_with_mock for file lookup - found *)
let test_mock_file_found () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.add store.files "test-key" (`Assoc [("name", `String "Test")]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_file ~token:"tok" ~file_key:"test-key" ()
  ) in
  match result with
  | Ok json ->
      let name = Yojson.Safe.Util.(json |> member "name" |> to_string) in
      check string "file found" "Test" name
  | Error _ -> fail "should find file"

(** Test run_with_mock for file lookup - not found *)
let test_mock_file_not_found () =
  let store = Figma_effects.create_mock_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_file ~token:"tok" ~file_key:"missing" ()
  ) in
  match result with
  | Ok _ -> fail "should not find file"
  | Error msg -> check bool "error message" true (String.length msg > 0)

(** Test run_with_mock for nodes lookup *)
let test_mock_nodes () =
  let store = Figma_effects.create_mock_store () in
  let key = "file1:node1,node2" in
  Hashtbl.add store.nodes key (`Assoc [("nodes", `List [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_nodes ~token:"tok" ~file_key:"file1" ~node_ids:["node1"; "node2"] ()
  ) in
  match result with
  | Ok _ -> check pass "nodes found" () ()
  | Error _ -> fail "should find nodes"

(** Test run_with_mock for get_me *)
let test_mock_get_me () =
  let store = Figma_effects.create_mock_store () in
  store.me := Some (`Assoc [("id", `String "user123")]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_me ~token:"tok"
  ) in
  match result with
  | Ok json ->
      let id = Yojson.Safe.Util.(json |> member "id" |> to_string) in
      check string "user id" "user123" id
  | Error _ -> fail "should find user"

(** Test run_with_mock for get_me not set *)
let test_mock_get_me_not_set () =
  let store = Figma_effects.create_mock_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_me ~token:"tok"
  ) in
  match result with
  | Ok _ -> fail "should not find user"
  | Error _ -> check pass "user not set" () ()

(** Test run_with_mock for team projects *)
let test_mock_team_projects () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.add store.projects "team1" (`Assoc [("projects", `List [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_team_projects ~token:"tok" ~team_id:"team1"
  ) in
  match result with
  | Ok _ -> check pass "projects found" () ()
  | Error _ -> fail "should find projects"

(** Test run_with_mock for project files *)
let test_mock_project_files () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.add store.project_files "proj1" (`Assoc [("files", `List [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_project_files ~token:"tok" ~project_id:"proj1"
  ) in
  match result with
  | Ok _ -> check pass "files found" () ()
  | Error _ -> fail "should find files"

(** Test run_with_mock for variables *)
let test_mock_variables () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.add store.variables "file1" (`Assoc [("variables", `List [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_variables ~token:"tok" ~file_key:"file1"
  ) in
  match result with
  | Ok _ -> check pass "variables found" () ()
  | Error _ -> fail "should find variables"

(** Test run_with_mock for images *)
let test_mock_images () =
  let store = Figma_effects.create_mock_store () in
  let key = "file1:node1" in
  Hashtbl.add store.images key (`Assoc [("images", `Assoc [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_images ~token:"tok" ~file_key:"file1"
      ~node_ids:["node1"] ~format:"png" ~scale:1.0 ()
  ) in
  match result with
  | Ok _ -> check pass "images found" () ()
  | Error _ -> fail "should find images"

(** Test run_with_mock for file_meta *)
let test_mock_file_meta () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.add store.file_meta "file1" (`Assoc [("components", `List [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_file_meta ~token:"tok" ~file_key:"file1" ()
  ) in
  match result with
  | Ok _ -> check pass "meta found" () ()
  | Error _ -> fail "should find meta"

(** Test run_with_mock for file_images (fills) *)
let test_mock_file_images () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.add store.file_images "file1" (`Assoc [("meta", `Assoc [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_file_images ~token:"tok" ~file_key:"file1" ()
  ) in
  match result with
  | Ok _ -> check pass "file images found" () ()
  | Error _ -> fail "should find file images"

(** Test run_with_mock logging (silent in tests) *)
let test_mock_logging () =
  let store = Figma_effects.create_mock_store () in
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.log_debug "debug message";
    Figma_effects.Perform.log_info "info message";
    Figma_effects.Perform.log_error "error message";
    "completed"
  ) in
  check string "logging completed" "completed" result

(* ============== Figma_image_similarity Tests ============== *)

(** Test find_in_path with common binary *)
let test_find_in_path () =
  (* sh should exist on any Unix system *)
  match Figma_image_similarity.find_in_path "sh" with
  | Some path -> check bool "sh found" true (String.length path > 0)
  | None -> fail "sh should be found"

(** Test find_in_path with nonexistent binary *)
let test_find_in_path_missing () =
  match Figma_image_similarity.find_in_path "nonexistent_binary_xyz123" with
  | Some _ -> fail "should not find nonexistent binary"
  | None -> check pass "not found" () ()

(** Test run_cmd success *)
let test_run_cmd_success () =
  match Figma_image_similarity.run_cmd "true" with
  | Ok () -> check pass "success" () ()
  | Error _ -> fail "true should succeed"

(** Test run_cmd failure *)
let test_run_cmd_failure () =
  match Figma_image_similarity.run_cmd "false" with
  | Ok () -> fail "false should fail"
  | Error msg -> check bool "error message" true (String.length msg > 0)

(** Test is_space helper *)
let test_is_space () =
  check bool "space" true (Figma_image_similarity.is_space ' ');
  check bool "tab" true (Figma_image_similarity.is_space '\t');
  check bool "newline" true (Figma_image_similarity.is_space '\n');
  check bool "return" true (Figma_image_similarity.is_space '\r');
  check bool "letter" false (Figma_image_similarity.is_space 'a')

(** Test load_ppm with invalid data *)
let test_load_ppm_invalid () =
  (* Create temp file with invalid PPM *)
  let tmp = Filename.temp_file "test" ".ppm" in
  let oc = open_out tmp in
  output_string oc "P3\n2 2 255\n";  (* P3 instead of P6 *)
  close_out oc;
  let result = Figma_image_similarity.load_ppm tmp in
  Sys.remove tmp;
  match result with
  | Ok _ -> fail "should fail for P3 format"
  | Error msg -> check bool "error about format" true (String.length msg > 0)

(** Test ensure_ppm with already PPM file *)
let test_ensure_ppm_already_ppm () =
  match Figma_image_similarity.ensure_ppm "/some/file.ppm" with
  | Ok path -> check string "same path" "/some/file.ppm" path
  | Error _ -> fail "should return same path"

(* ============== Test Suites ============== *)

let similarity_tests = [
  "rgb_to_xyz", `Quick, test_rgb_to_xyz;
  "linearize_rgb", `Quick, test_linearize_rgb;
  "xyz_to_lab", `Quick, test_xyz_to_lab;
  "rgb_to_lab", `Quick, test_rgb_to_lab;
  "rgb_to_oklab", `Quick, test_rgb_to_oklab;
  "oklab_distance", `Quick, test_oklab_distance;
  "oklab_to_similarity", `Quick, test_oklab_to_similarity;
  "ciede2000", `Quick, test_ciede2000;
  "ciede2000_weighted", `Quick, test_ciede2000_weighted;
  "delta_e_to_similarity", `Quick, test_delta_e_to_similarity;
  "iou_overlap", `Quick, test_iou_overlap;
  "iou_no_overlap", `Quick, test_iou_no_overlap;
  "iou_zero_area", `Quick, test_iou_zero_area;
  "giou", `Quick, test_giou;
  "giou_to_similarity", `Quick, test_giou_to_similarity;
  "diou", `Quick, test_diou;
  "diou_to_similarity", `Quick, test_diou_to_similarity;
  "ted_identical", `Quick, test_ted_identical;
  "ted_different_root", `Quick, test_ted_different_root;
  "ted_with_children", `Quick, test_ted_with_children;
  "ted_to_similarity", `Quick, test_ted_to_similarity;
  "node_iou", `Quick, test_node_iou;
  "node_iou_no_bbox", `Quick, test_node_iou_no_bbox;
  "node_giou", `Quick, test_node_giou;
  "node_diou", `Quick, test_node_diou;
  "rgba_to_lab", `Quick, test_rgba_to_lab;
  "rgba_distance_ciede2000", `Quick, test_rgba_distance_ciede2000;
  "rgba_distance_oklab", `Quick, test_rgba_distance_oklab;
  "compute_similarity", `Quick, test_compute_similarity;
  "compute_similarity_no_fills", `Quick, test_compute_similarity_no_fills;
  "extended_color_metrics", `Quick, test_extended_color_metrics;
  "extended_box_metrics", `Quick, test_extended_box_metrics;
  "metrics_to_string", `Quick, test_metrics_to_string;
  "extended_color_to_string", `Quick, test_extended_color_to_string;
  "extended_box_to_string", `Quick, test_extended_box_to_string;
]

let effects_tests = [
  "create_mock_store", `Quick, test_create_mock_store;
  "mock_file_found", `Quick, test_mock_file_found;
  "mock_file_not_found", `Quick, test_mock_file_not_found;
  "mock_nodes", `Quick, test_mock_nodes;
  "mock_get_me", `Quick, test_mock_get_me;
  "mock_get_me_not_set", `Quick, test_mock_get_me_not_set;
  "mock_team_projects", `Quick, test_mock_team_projects;
  "mock_project_files", `Quick, test_mock_project_files;
  "mock_variables", `Quick, test_mock_variables;
  "mock_images", `Quick, test_mock_images;
  "mock_file_meta", `Quick, test_mock_file_meta;
  "mock_file_images", `Quick, test_mock_file_images;
  "mock_logging", `Quick, test_mock_logging;
]

let image_similarity_tests = [
  "find_in_path", `Quick, test_find_in_path;
  "find_in_path_missing", `Quick, test_find_in_path_missing;
  "run_cmd_success", `Quick, test_run_cmd_success;
  "run_cmd_failure", `Quick, test_run_cmd_failure;
  "is_space", `Quick, test_is_space;
  "load_ppm_invalid", `Quick, test_load_ppm_invalid;
  "ensure_ppm_already_ppm", `Quick, test_ensure_ppm_already_ppm;
]

let () =
  run "Similarity & Effects Coverage" [
    "figma_similarity", similarity_tests;
    "figma_effects", effects_tests;
    "figma_image_similarity", image_similarity_tests;
  ]
