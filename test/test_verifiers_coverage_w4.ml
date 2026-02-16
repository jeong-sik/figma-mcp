(** Coverage Wave 4: visual_verifier.ml + semantic_verifier.ml + figma_image_similarity.ml
    Targets uncovered pure functions across all three verifier modules.
    Goal: 85%+ coverage per module. *)

let () =
  Unix.putenv "FIGMA_TOKEN" "test-token-12345";
  let open Alcotest in
  let open Figma_types in

  (* ====== Helper constructors (shared) ====== *)
  let mk_bbox x y w h : bbox = { x; y; width = w; height = h } in
  let mk_rgba r g b a : rgba = { r; g; b; a } in
  let solid (c: rgba) : paint =
    { paint_type = Solid; visible = true; opacity = 1.0;
      color = Some c; gradient_stops = []; image_ref = None; scale_mode = None }
  in
  let invisible_solid (c: rgba) : paint =
    { paint_type = Solid; visible = false; opacity = 1.0;
      color = Some c; gradient_stops = []; image_ref = None; scale_mode = None }
  in
  let gradient_paint : paint =
    { paint_type = GradientLinear; visible = true; opacity = 1.0;
      color = None; gradient_stops = []; image_ref = None; scale_mode = None }
  in

  (* ====== SEMANTIC_VERIFIER tests ====== *)

  (* -- normalize_text -- *)
  let test_sv_normalize_text_simple () =
    check string "trim" "hello world" (Semantic_verifier.normalize_text "  hello   world  ")
  in
  let test_sv_normalize_text_newlines () =
    check string "newlines" "a b c" (Semantic_verifier.normalize_text "a\n\tb\r c")
  in
  let test_sv_normalize_text_empty () =
    check string "empty" "" (Semantic_verifier.normalize_text "   ")
  in

  (* -- rgba_dist_rgb -- *)
  let test_sv_rgba_dist_same () =
    let c = mk_rgba 0.5 0.5 0.5 1.0 in
    check (float 0.001) "same" 0.0 (Semantic_verifier.rgba_dist_rgb c c)
  in
  let test_sv_rgba_dist_different () =
    let a = mk_rgba 1.0 0.0 0.0 1.0 in
    let b = mk_rgba 0.0 1.0 0.0 1.0 in
    let d = Semantic_verifier.rgba_dist_rgb a b in
    check bool "positive" true (d > 0.0);
    check (float 0.01) "sqrt2" (sqrt 2.0) d
  in
  let test_sv_rgba_dist_black_white () =
    let a = mk_rgba 0.0 0.0 0.0 1.0 in
    let b = mk_rgba 1.0 1.0 1.0 1.0 in
    let d = Semantic_verifier.rgba_dist_rgb a b in
    check (float 0.01) "sqrt3" (sqrt 3.0) d
  in

  (* -- bbox_area -- *)
  let test_sv_bbox_area_normal () =
    check (float 0.001) "area" 600.0 (Semantic_verifier.bbox_area (mk_bbox 0. 0. 30. 20.))
  in
  let test_sv_bbox_area_zero () =
    check (float 0.001) "zero" 0.0 (Semantic_verifier.bbox_area (mk_bbox 10. 20. 0. 0.))
  in
  let test_sv_bbox_area_single_dim_zero () =
    check (float 0.001) "single zero" 0.0 (Semantic_verifier.bbox_area (mk_bbox 0. 0. 100. 0.))
  in

  (* -- bbox_center -- *)
  let test_sv_bbox_center () =
    let cx, cy = Semantic_verifier.bbox_center (mk_bbox 10. 20. 100. 50.) in
    check (float 0.001) "cx" 60.0 cx;
    check (float 0.001) "cy" 45.0 cy
  in
  let test_sv_bbox_center_origin () =
    let cx, cy = Semantic_verifier.bbox_center (mk_bbox 0. 0. 10. 10.) in
    check (float 0.001) "cx" 5.0 cx;
    check (float 0.001) "cy" 5.0 cy
  in

  (* -- bbox_delta -- *)
  let test_sv_bbox_delta_identical () =
    let b = mk_bbox 10. 20. 100. 50. in
    let dx, dy, dw, dh = Semantic_verifier.bbox_delta ~expected:b ~actual:b in
    check (float 0.001) "dx" 0.0 dx;
    check (float 0.001) "dy" 0.0 dy;
    check (float 0.001) "dw" 0.0 dw;
    check (float 0.001) "dh" 0.0 dh
  in
  let test_sv_bbox_delta_shifted () =
    let e = mk_bbox 10. 20. 100. 50. in
    let a = mk_bbox 15. 25. 110. 55. in
    let dx, dy, dw, dh = Semantic_verifier.bbox_delta ~expected:e ~actual:a in
    check (float 0.001) "dx" 5.0 dx;
    check (float 0.001) "dy" 5.0 dy;
    check (float 0.001) "dw" 10.0 dw;
    check (float 0.001) "dh" 5.0 dh
  in

  (* -- bbox_within -- *)
  let test_sv_bbox_within_exact () =
    let b = mk_bbox 10. 20. 100. 50. in
    check bool "exact" true (Semantic_verifier.bbox_within ~tol_px:0.0 ~expected:b ~actual:b)
  in
  let test_sv_bbox_within_inside_tol () =
    let e = mk_bbox 10. 20. 100. 50. in
    let a = mk_bbox 12. 22. 100. 50. in
    check bool "inside" true (Semantic_verifier.bbox_within ~tol_px:3.0 ~expected:e ~actual:a)
  in
  let test_sv_bbox_within_outside_tol () =
    let e = mk_bbox 10. 20. 100. 50. in
    let a = mk_bbox 20. 20. 100. 50. in
    check bool "outside" false (Semantic_verifier.bbox_within ~tol_px:5.0 ~expected:e ~actual:a)
  in

  (* -- normalize_bbox -- *)
  let test_sv_normalize_bbox () =
    let origin = mk_bbox 100. 200. 0. 0. in
    let b = mk_bbox 120. 240. 50. 30. in
    let r = Semantic_verifier.normalize_bbox ~origin b in
    check (float 0.001) "x" 20.0 r.x;
    check (float 0.001) "y" 40.0 r.y;
    check (float 0.001) "w" 50.0 r.width;
    check (float 0.001) "h" 30.0 r.height
  in

  (* -- pick_root_bbox -- *)
  let test_sv_pick_root_with_root () =
    let metrics : Html_metrics.t = {
      viewport_width = 300; viewport_height = 200;
      root = Some { bbox = mk_bbox 5. 10. 290. 180.; background = None; border_radius_px = None };
      bg_nodes = []; text_nodes = [];
    } in
    let b = Semantic_verifier.pick_root_bbox metrics in
    check (float 0.001) "x" 5.0 b.x;
    check (float 0.001) "w" 290.0 b.width
  in
  let test_sv_pick_root_no_root () =
    let metrics : Html_metrics.t = {
      viewport_width = 300; viewport_height = 200;
      root = None; bg_nodes = []; text_nodes = [];
    } in
    let b = Semantic_verifier.pick_root_bbox metrics in
    check (float 0.001) "x" 0.0 b.x;
    check (float 0.001) "w" 300.0 b.width;
    check (float 0.001) "h" 200.0 b.height
  in

  (* -- solid_color_of_paints -- *)
  let test_sv_solid_color_found () =
    let c = mk_rgba 1.0 0.0 0.0 1.0 in
    match Semantic_verifier.solid_color_of_paints [solid c] with
    | Some r -> check (float 0.001) "r" 1.0 r.r
    | None -> fail "expected Some"
  in
  let test_sv_solid_color_empty () =
    check bool "none" true (Semantic_verifier.solid_color_of_paints [] = None)
  in
  let test_sv_solid_color_invisible () =
    let c = mk_rgba 1.0 0.0 0.0 1.0 in
    check bool "invisible" true (Semantic_verifier.solid_color_of_paints [invisible_solid c] = None)
  in
  let test_sv_solid_color_gradient_then_solid () =
    let c = mk_rgba 0.0 1.0 0.0 1.0 in
    match Semantic_verifier.solid_color_of_paints [gradient_paint; solid c] with
    | Some r -> check (float 0.001) "g" 1.0 r.g
    | None -> fail "expected Some from second paint"
  in

  (* -- extract_expected_texts -- *)
  let test_sv_extract_texts_basic () =
    let typo = { default_typography with font_size = 16.0; font_weight = 400 } in
    let root_bbox = mk_bbox 100. 200. 300. 200. in
    let text_node : ui_node = { default_node with
      id = "t1"; name = "T"; node_type = Text;
      bbox = Some (mk_bbox 120. 240. 80. 20.);
      characters = Some "Hello";
      typography = Some typo;
      fills = [solid (mk_rgba 0.0 0.0 0.0 1.0)];
      children = [];
    } in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some root_bbox; children = [text_node];
    } in
    let texts = Semantic_verifier.extract_expected_texts ~root_bbox root in
    check int "count" 1 (List.length texts);
    let t = List.hd texts in
    check string "text" "Hello" t.text;
    check (float 0.001) "rel_x" 20.0 t.bbox.x;
    check (float 0.001) "rel_y" 40.0 t.bbox.y
  in
  let test_sv_extract_texts_empty_chars () =
    let root_bbox = mk_bbox 0. 0. 100. 100. in
    let text_node : ui_node = { default_node with
      id = "t2"; name = "T"; node_type = Text;
      bbox = Some (mk_bbox 10. 10. 50. 20.);
      characters = Some "   ";
      children = [];
    } in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some root_bbox; children = [text_node];
    } in
    let texts = Semantic_verifier.extract_expected_texts ~root_bbox root in
    check int "empty chars skipped" 0 (List.length texts)
  in
  let test_sv_extract_texts_no_bbox () =
    let root_bbox = mk_bbox 0. 0. 100. 100. in
    let text_node : ui_node = { default_node with
      id = "t3"; name = "T"; node_type = Text;
      bbox = None; characters = Some "No bbox";
      children = [];
    } in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some root_bbox; children = [text_node];
    } in
    let texts = Semantic_verifier.extract_expected_texts ~root_bbox root in
    check int "no bbox skipped" 0 (List.length texts)
  in

  (* -- choose_best_text_candidate -- *)
  let test_sv_choose_exact_match () =
    let expected : Semantic_verifier.expected_text = {
      text = "Hello"; bbox = mk_bbox 20. 40. 80. 20.;
      font_size_px = Some 16.0; font_weight = Some 400;
      color = Some (mk_rgba 0.0 0.0 0.0 1.0);
    } in
    let cand : Html_metrics.text_node = {
      text = "Hello"; bbox = mk_bbox 22. 42. 80. 20.;
      font_size_px = Some 16.0; font_weight = Some 400;
      font_family = None; color = Some (mk_rgba 0.0 0.0 0.0 1.0);
    } in
    let origin = mk_bbox 0. 0. 300. 200. in
    match Semantic_verifier.choose_best_text_candidate ~expected ~origin [cand] with
    | Some c -> check string "matched" "Hello" c.text
    | None -> fail "expected match"
  in
  let test_sv_choose_no_match () =
    let expected : Semantic_verifier.expected_text = {
      text = "Hello"; bbox = mk_bbox 20. 40. 80. 20.;
      font_size_px = None; font_weight = None; color = None;
    } in
    let cand : Html_metrics.text_node = {
      text = "Goodbye"; bbox = mk_bbox 20. 40. 80. 20.;
      font_size_px = None; font_weight = None;
      font_family = None; color = None;
    } in
    let origin = mk_bbox 0. 0. 300. 200. in
    check bool "none" true
      (Semantic_verifier.choose_best_text_candidate ~expected ~origin [cand] = None)
  in
  let test_sv_choose_substring_match () =
    let expected : Semantic_verifier.expected_text = {
      text = "Hello World"; bbox = mk_bbox 20. 40. 80. 20.;
      font_size_px = None; font_weight = None; color = None;
    } in
    let cand : Html_metrics.text_node = {
      text = "Say Hello World Please"; bbox = mk_bbox 20. 40. 80. 20.;
      font_size_px = None; font_weight = None;
      font_family = None; color = None;
    } in
    let origin = mk_bbox 0. 0. 300. 200. in
    match Semantic_verifier.choose_best_text_candidate ~expected ~origin [cand] with
    | Some c -> check string "substring" "Say Hello World Please" c.text
    | None -> fail "expected substring match"
  in
  let test_sv_choose_short_text_no_substring () =
    (* text length <= 2 should not attempt substring match *)
    let expected : Semantic_verifier.expected_text = {
      text = "Hi"; bbox = mk_bbox 20. 40. 80. 20.;
      font_size_px = None; font_weight = None; color = None;
    } in
    let cand : Html_metrics.text_node = {
      text = "This Hi is here"; bbox = mk_bbox 20. 40. 80. 20.;
      font_size_px = None; font_weight = None;
      font_family = None; color = None;
    } in
    let origin = mk_bbox 0. 0. 300. 200. in
    check bool "short no substring" true
      (Semantic_verifier.choose_best_text_candidate ~expected ~origin [cand] = None)
  in
  let test_sv_choose_best_by_distance () =
    let expected : Semantic_verifier.expected_text = {
      text = "Hello"; bbox = mk_bbox 20. 40. 80. 20.;
      font_size_px = None; font_weight = None; color = None;
    } in
    let far : Html_metrics.text_node = {
      text = "Hello"; bbox = mk_bbox 200. 200. 80. 20.;
      font_size_px = None; font_weight = None;
      font_family = None; color = None;
    } in
    let near : Html_metrics.text_node = {
      text = "Hello"; bbox = mk_bbox 22. 42. 80. 20.;
      font_size_px = None; font_weight = None;
      font_family = None; color = None;
    } in
    let origin = mk_bbox 0. 0. 300. 200. in
    match Semantic_verifier.choose_best_text_candidate ~expected ~origin [far; near] with
    | Some c -> check (float 0.001) "near x" 22.0 c.bbox.x
    | None -> fail "expected near match"
  in

  (* -- verify edge cases -- *)
  let test_sv_verify_no_bbox () =
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = None; children = [];
    } in
    let metrics : Html_metrics.t = {
      viewport_width = 300; viewport_height = 200;
      root = None; bg_nodes = []; text_nodes = [];
    } in
    match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
    | Error e -> check bool "has error" true (String.length e > 0)
    | Ok _ -> fail "expected error for missing bbox"
  in
  let test_sv_verify_root_size_mismatch () =
    let bg = mk_rgba 1.0 1.0 1.0 1.0 in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some (mk_bbox 0. 0. 300. 200.);
      fills = [solid bg]; children = [];
    } in
    let metrics : Html_metrics.t = {
      viewport_width = 500; viewport_height = 400;
      root = Some { bbox = mk_bbox 0. 0. 500. 400.; background = Some bg; border_radius_px = Some 0. };
      bg_nodes = []; text_nodes = [];
    } in
    match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
    | Error e -> fail e
    | Ok r ->
        check bool "has root size mismatch" true
          (List.exists (function Semantic_verifier.RootSizeMismatch _ -> true | _ -> false) r.mismatches)
  in
  let test_sv_verify_bg_mismatch () =
    let e_bg = mk_rgba 1.0 0.0 0.0 1.0 in
    let a_bg = mk_rgba 0.0 0.0 1.0 1.0 in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some (mk_bbox 0. 0. 300. 200.);
      fills = [solid e_bg]; children = [];
    } in
    let metrics : Html_metrics.t = {
      viewport_width = 300; viewport_height = 200;
      root = Some { bbox = mk_bbox 0. 0. 300. 200.; background = Some a_bg; border_radius_px = None };
      bg_nodes = []; text_nodes = [];
    } in
    match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
    | Error e -> fail e
    | Ok r ->
        check bool "bg mismatch" true
          (List.exists (function Semantic_verifier.RootBackgroundMismatch _ -> true | _ -> false) r.mismatches)
  in
  let test_sv_verify_border_radius_mismatch () =
    let bg = mk_rgba 1.0 1.0 1.0 1.0 in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some (mk_bbox 0. 0. 300. 200.);
      fills = [solid bg]; border_radius = 20.0; children = [];
    } in
    let metrics : Html_metrics.t = {
      viewport_width = 300; viewport_height = 200;
      root = Some { bbox = mk_bbox 0. 0. 300. 200.; background = Some bg; border_radius_px = Some 2.0 };
      bg_nodes = []; text_nodes = [];
    } in
    match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
    | Error e -> fail e
    | Ok r ->
        check bool "radius mismatch" true
          (List.exists (function Semantic_verifier.RootBorderRadiusMismatch _ -> true | _ -> false) r.mismatches)
  in
  let test_sv_verify_font_size_mismatch () =
    let bg = mk_rgba 1.0 1.0 1.0 1.0 in
    let text_color = mk_rgba 0.0 0.0 0.0 1.0 in
    let root_abs = mk_bbox 0. 0. 300. 200. in
    let text_abs = mk_bbox 20. 40. 100. 20. in
    let typo = { default_typography with font_size = 16.0; font_weight = 400 } in
    let text_node : ui_node = { default_node with
      id = "t"; name = "T"; node_type = Text;
      bbox = Some text_abs; characters = Some "Hello";
      typography = Some typo;
      fills = [solid text_color]; children = [];
    } in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some root_abs; fills = [solid bg]; children = [text_node];
    } in
    let metrics : Html_metrics.t = {
      viewport_width = 300; viewport_height = 200;
      root = Some { bbox = mk_bbox 0. 0. 300. 200.; background = Some bg; border_radius_px = None };
      bg_nodes = [];
      text_nodes = [{ text = "Hello"; bbox = mk_bbox 20. 40. 100. 20.;
                      font_size_px = Some 24.0; font_weight = Some 400;
                      font_family = None; color = Some text_color }];
    } in
    match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
    | Error e -> fail e
    | Ok r ->
        check bool "font size" true
          (List.exists (function Semantic_verifier.TextFontSizeMismatch _ -> true | _ -> false) r.mismatches)
  in
  let test_sv_verify_font_weight_mismatch () =
    let bg = mk_rgba 1.0 1.0 1.0 1.0 in
    let text_color = mk_rgba 0.0 0.0 0.0 1.0 in
    let typo = { default_typography with font_size = 16.0; font_weight = 400 } in
    let text_node : ui_node = { default_node with
      id = "t"; name = "T"; node_type = Text;
      bbox = Some (mk_bbox 20. 40. 100. 20.); characters = Some "Hello";
      typography = Some typo;
      fills = [solid text_color]; children = [];
    } in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some (mk_bbox 0. 0. 300. 200.);
      fills = [solid bg]; children = [text_node];
    } in
    let metrics : Html_metrics.t = {
      viewport_width = 300; viewport_height = 200;
      root = Some { bbox = mk_bbox 0. 0. 300. 200.; background = Some bg; border_radius_px = None };
      bg_nodes = [];
      text_nodes = [{ text = "Hello"; bbox = mk_bbox 20. 40. 100. 20.;
                      font_size_px = Some 16.0; font_weight = Some 800;
                      font_family = None; color = Some text_color }];
    } in
    match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
    | Error e -> fail e
    | Ok r ->
        check bool "font weight" true
          (List.exists (function Semantic_verifier.TextFontWeightMismatch _ -> true | _ -> false) r.mismatches)
  in
  let test_sv_verify_text_color_mismatch () =
    let bg = mk_rgba 1.0 1.0 1.0 1.0 in
    let e_color = mk_rgba 1.0 0.0 0.0 1.0 in
    let a_color = mk_rgba 0.0 0.0 1.0 1.0 in
    let typo = { default_typography with font_size = 16.0; font_weight = 400 } in
    let text_node : ui_node = { default_node with
      id = "t"; name = "T"; node_type = Text;
      bbox = Some (mk_bbox 20. 40. 100. 20.); characters = Some "Hello";
      typography = Some typo;
      fills = [solid e_color]; children = [];
    } in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some (mk_bbox 0. 0. 300. 200.);
      fills = [solid bg]; children = [text_node];
    } in
    let metrics : Html_metrics.t = {
      viewport_width = 300; viewport_height = 200;
      root = Some { bbox = mk_bbox 0. 0. 300. 200.; background = Some bg; border_radius_px = None };
      bg_nodes = [];
      text_nodes = [{ text = "Hello"; bbox = mk_bbox 20. 40. 100. 20.;
                      font_size_px = Some 16.0; font_weight = Some 400;
                      font_family = None; color = Some a_color }];
    } in
    match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
    | Error e -> fail e
    | Ok r ->
        check bool "text color" true
          (List.exists (function Semantic_verifier.TextColorMismatch _ -> true | _ -> false) r.mismatches)
  in
  let test_sv_verify_score_calculation () =
    (* 1 MissingText => score = 1.0 - 0.20 = 0.80 *)
    let bg = mk_rgba 1.0 1.0 1.0 1.0 in
    let typo = { default_typography with font_size = 16.0; font_weight = 400 } in
    let text_node : ui_node = { default_node with
      id = "t"; name = "T"; node_type = Text;
      bbox = Some (mk_bbox 20. 40. 100. 20.); characters = Some "Missing";
      typography = Some typo;
      fills = [solid (mk_rgba 0. 0. 0. 1.)]; children = [];
    } in
    let root : ui_node = { default_node with
      id = "root"; name = "Root"; node_type = Frame;
      bbox = Some (mk_bbox 0. 0. 300. 200.);
      fills = [solid bg]; children = [text_node];
    } in
    let metrics : Html_metrics.t = {
      viewport_width = 300; viewport_height = 200;
      root = Some { bbox = mk_bbox 0. 0. 300. 200.; background = Some bg; border_radius_px = None };
      bg_nodes = []; text_nodes = [];
    } in
    match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
    | Error e -> fail e
    | Ok r ->
        check (float 0.01) "score" 0.80 r.score;
        check bool "not passed" false r.passed
  in

  (* -- mismatch_to_json (all 8 variants) -- *)
  let test_sv_mismatch_json_root_size () =
    let j = Semantic_verifier.mismatch_to_json
      (RootSizeMismatch { expected_w=300.; expected_h=200.; actual_w=310.; actual_h=210.; dw=10.; dh=10. }) in
    let t = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "root_size" t
  in
  let test_sv_mismatch_json_root_bg () =
    let j = Semantic_verifier.mismatch_to_json
      (RootBackgroundMismatch { expected = mk_rgba 1. 0. 0. 1.; actual = mk_rgba 0. 1. 0. 1.; dist = 1.41 }) in
    let t = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "root_background" t
  in
  let test_sv_mismatch_json_root_radius () =
    let j = Semantic_verifier.mismatch_to_json
      (RootBorderRadiusMismatch { expected_px = 8.0; actual_px = 12.0; delta_px = 4.0 }) in
    let t = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "root_border_radius" t
  in
  let test_sv_mismatch_json_missing_text () =
    let j = Semantic_verifier.mismatch_to_json (MissingText { text = "Hello" }) in
    let t = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "missing_text" t
  in
  let test_sv_mismatch_json_text_bbox () =
    let j = Semantic_verifier.mismatch_to_json
      (TextBBoxMismatch { text = "Hi"; expected = mk_bbox 0. 0. 50. 20.; actual = mk_bbox 10. 10. 50. 20.;
                          dx = 10.; dy = 10.; dw = 0.; dh = 0. }) in
    let t = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "text_bbox" t
  in
  let test_sv_mismatch_json_font_size () =
    let j = Semantic_verifier.mismatch_to_json
      (TextFontSizeMismatch { text = "Hi"; expected_px = 16.0; actual_px = 24.0; delta_px = 8.0 }) in
    let t = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "text_font_size" t
  in
  let test_sv_mismatch_json_font_weight () =
    let j = Semantic_verifier.mismatch_to_json
      (TextFontWeightMismatch { text = "Hi"; expected = 400; actual = 700; delta = 300 }) in
    let t = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "text_font_weight" t
  in
  let test_sv_mismatch_json_text_color () =
    let j = Semantic_verifier.mismatch_to_json
      (TextColorMismatch { text = "Hi"; expected = mk_rgba 1. 0. 0. 1.; actual = mk_rgba 0. 1. 0. 1.; dist = 1.41 }) in
    let t = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "text_color" t
  in

  (* -- bbox_to_json, rgba_to_json -- *)
  let test_sv_bbox_to_json () =
    let j = Semantic_verifier.bbox_to_json (mk_bbox 10. 20. 30. 40.) in
    let x = Yojson.Safe.Util.(j |> member "x" |> to_float) in
    check (float 0.001) "x" 10.0 x
  in
  let test_sv_rgba_to_json () =
    let j = Semantic_verifier.rgba_to_json (mk_rgba 0.5 0.6 0.7 0.8) in
    let r = Yojson.Safe.Util.(j |> member "r" |> to_float) in
    check (float 0.001) "r" 0.5 r
  in

  (* -- result_to_json -- *)
  let test_sv_result_to_json () =
    let r : Semantic_verifier.t = {
      passed = true; score = 0.95;
      root_expected = mk_bbox 0. 0. 300. 200.;
      root_actual = mk_bbox 0. 0. 300. 200.;
      total_texts = 1; matched_texts = 1; mismatches = [];
    } in
    let j = Semantic_verifier.result_to_json r in
    let p = Yojson.Safe.Util.(j |> member "passed" |> to_bool) in
    check bool "passed" true p;
    let s = Yojson.Safe.Util.(j |> member "score" |> to_float) in
    check (float 0.01) "score" 0.95 s
  in

  (* ====== FIGMA_IMAGE_SIMILARITY tests ====== *)

  (* -- is_space -- *)
  let test_is_space_true () =
    check bool "space" true (Figma_image_similarity.is_space ' ');
    check bool "tab" true (Figma_image_similarity.is_space '\t');
    check bool "newline" true (Figma_image_similarity.is_space '\n');
    check bool "cr" true (Figma_image_similarity.is_space '\r')
  in
  let test_is_space_false () =
    check bool "a" false (Figma_image_similarity.is_space 'a');
    check bool "0" false (Figma_image_similarity.is_space '0');
    check bool "#" false (Figma_image_similarity.is_space '#')
  in

  (* -- parse_ppm_header -- *)
  let test_ppm_header_valid () =
    let data = "P6\n10 20\n255\n" in
    let (magic, w, h, maxv, _off) = Figma_image_similarity.parse_ppm_header data in
    check string "magic" "P6" magic;
    check int "w" 10 w;
    check int "h" 20 h;
    check int "maxv" 255 maxv
  in
  let test_ppm_header_with_comment () =
    let data = "P6\n# this is a comment\n4 3\n255\n" in
    let (magic, w, h, maxv, _off) = Figma_image_similarity.parse_ppm_header data in
    check string "magic" "P6" magic;
    check int "w" 4 w;
    check int "h" 3 h;
    check int "maxv" 255 maxv
  in
  let test_ppm_header_empty () =
    let (magic, w, h, _, _) = Figma_image_similarity.parse_ppm_header "" in
    check string "magic" "" magic;
    check int "w" 0 w;
    check int "h" 0 h
  in
  let test_ppm_header_incomplete () =
    (* Only 2 tokens (P6, 10) — needs 4, so match falls to default *)
    let (magic, w, _, _, _) = Figma_image_similarity.parse_ppm_header "P6 10" in
    check string "magic" "" magic;
    check int "w" 0 w
  in

  (* -- mse_psnr with synthetic images -- *)
  let mk_image w h luma_val =
    let n = w * h in
    let luma = Array.make n luma_val in
    let lab = Array.make n { Ciede2000.l = 50.0; a = 0.0; b = 0.0 } in
    ({ Figma_image_similarity.width = w; height = h; lab; luma } : Figma_image_similarity.image)
  in
  let test_mse_psnr_identical () =
    let img = mk_image 10 10 0.5 in
    let (mse, psnr, w, h) = Figma_image_similarity.mse_psnr img img in
    check (float 0.001) "mse" 0.0 mse;
    check bool "psnr inf" true (Float.is_infinite psnr);
    check int "w" 10 w;
    check int "h" 10 h
  in
  let test_mse_psnr_different () =
    let a = mk_image 10 10 0.0 in
    let b = mk_image 10 10 1.0 in
    let (mse, psnr, _, _) = Figma_image_similarity.mse_psnr a b in
    check (float 0.001) "mse" 1.0 mse;
    check bool "finite psnr" true (Float.is_finite psnr)
  in
  let test_mse_psnr_zero_area () =
    let a = mk_image 0 0 0.5 in
    let b = mk_image 0 0 0.5 in
    let (mse, psnr, w, h) = Figma_image_similarity.mse_psnr a b in
    check (float 0.001) "mse" 0.0 mse;
    check bool "psnr inf" true (Float.is_infinite psnr);
    check int "w" 0 w;
    check int "h" 0 h
  in
  let test_mse_psnr_different_sizes () =
    let a = mk_image 20 10 0.5 in
    let b = mk_image 10 20 0.5 in
    let (_, _, w, h) = Figma_image_similarity.mse_psnr a b in
    check int "overlap_w" 10 w;
    check int "overlap_h" 10 h
  in

  (* -- ssim_window -- *)
  let test_ssim_window_identical () =
    let img = mk_image 8 8 0.5 in
    let s = Figma_image_similarity.ssim_window ~a:img ~b:img ~x0:0 ~y0:0 ~w:8 ~h:8 in
    check (float 0.001) "perfect" 1.0 s
  in
  let test_ssim_window_zero () =
    let s = Figma_image_similarity.ssim_window
      ~a:(mk_image 8 8 0.5) ~b:(mk_image 8 8 0.5) ~x0:0 ~y0:0 ~w:0 ~h:0 in
    check (float 0.001) "zero window" 0.0 s
  in
  let test_ssim_window_different () =
    let a = mk_image 8 8 0.0 in
    let b = mk_image 8 8 1.0 in
    let s = Figma_image_similarity.ssim_window ~a ~b ~x0:0 ~y0:0 ~w:8 ~h:8 in
    check bool "less than 1" true (s < 1.0)
  in

  (* -- ssim (full image) -- *)
  let test_ssim_identical () =
    let img = mk_image 16 16 0.5 in
    let s = Figma_image_similarity.ssim img img ~window:8 in
    check (float 0.001) "perfect" 1.0 s
  in
  let test_ssim_zero_size () =
    let a = mk_image 0 0 0.5 in
    let b = mk_image 0 0 0.5 in
    let s = Figma_image_similarity.ssim a b ~window:8 in
    check (float 0.001) "zero" 0.0 s
  in
  let test_ssim_negative_window () =
    let img = mk_image 16 16 0.5 in
    let s = Figma_image_similarity.ssim img img ~window:(-1) in
    check (float 0.001) "uses default" 1.0 s
  in
  let test_ssim_too_small_for_window () =
    let img = mk_image 4 4 0.5 in
    let s = Figma_image_similarity.ssim img img ~window:8 in
    check (float 0.001) "too small" 0.0 s
  in

  (* -- calculate_avg_delta_e -- *)
  let test_delta_e_identical () =
    let img = mk_image 10 10 0.5 in
    let d = Figma_image_similarity.calculate_avg_delta_e img img in
    check (float 0.001) "zero" 0.0 d
  in
  let test_delta_e_zero_area () =
    let a = mk_image 0 0 0.5 in
    let d = Figma_image_similarity.calculate_avg_delta_e a a in
    check (float 0.001) "zero area" 0.0 d
  in
  let test_delta_e_different () =
    let n = 4 * 4 in
    let lab_a = Array.make n { Ciede2000.l = 50.0; a = 0.0; b = 0.0 } in
    let lab_b = Array.make n { Ciede2000.l = 80.0; a = 10.0; b = (-10.0) } in
    let a : Figma_image_similarity.image = { width = 4; height = 4; lab = lab_a; luma = Array.make n 0.5 } in
    let b : Figma_image_similarity.image = { width = 4; height = 4; lab = lab_b; luma = Array.make n 0.7 } in
    let d = Figma_image_similarity.calculate_avg_delta_e a b in
    check bool "positive" true (d > 0.0)
  in

  (* ====== VISUAL_VERIFIER tests (uncovered functions) ====== *)

  (* -- step_to_json -- *)
  let test_vv_step_to_json () =
    let step : Visual_verifier.evolution_step = {
      step_num = 1;
      step_ssim = 0.85;
      step_delta_e = 5.0;
      step_human_ssim = 0.80;
      step_html_path = "/tmp/test.html";
      step_png_path = "/tmp/test.png";
      corrections_this_step = [Visual_verifier.AdjustGap 2.0];
    } in
    let j = Visual_verifier.step_to_json step in
    let s = Yojson.Safe.Util.(j |> member "step" |> to_int) in
    check int "step" 1 s;
    let ssim = Yojson.Safe.Util.(j |> member "ssim" |> to_float) in
    check (float 0.001) "ssim" 0.85 ssim;
    let corr = Yojson.Safe.Util.(j |> member "corrections" |> to_list) in
    check int "corrections count" 1 (List.length corr)
  in

  (* -- apply_color_adjustment -- *)
  let test_vv_apply_color_no_selector () =
    let rgba = { Figma_types.r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
    let html = "div { color: #000000; }" in
    let result = Visual_verifier.apply_color_adjustment "" rgba html in
    check bool "contains red" true (
      try ignore (Str.search_forward (Str.regexp_string "#ff0000") result 0); true
      with Not_found -> false)
  in
  let test_vv_apply_color_with_selector () =
    let rgba = { Figma_types.r = 0.0; g = 1.0; b = 0.0; a = 1.0 } in
    let html = ".btn { background-color: #000000; padding: 10px; }" in
    let result = Visual_verifier.apply_color_adjustment ".btn" rgba html in
    check bool "contains green" true (
      try ignore (Str.search_forward (Str.regexp_string "#00ff00") result 0); true
      with Not_found -> false)
  in
  let test_vv_apply_color_selector_not_found () =
    let rgba = { Figma_types.r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
    let html = ".other { color: #000000; }" in
    let result = Visual_verifier.apply_color_adjustment ".missing" rgba html in
    check string "unchanged" html result
  in

  (* -- apply_single_hint -- *)
  let test_vv_apply_single_hint_color () =
    let rgba = { Figma_types.r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
    let html = "div { color: #000000; }" in
    let result = Visual_verifier.apply_single_hint (AdjustColor (".sel", rgba)) html in
    check bool "processed" true (String.length result > 0)
  in

  (* -- apply_corrections (chain) -- *)
  let test_vv_apply_corrections_empty () =
    let html = "<div>hello</div>" in
    let result = Visual_verifier.apply_corrections [] html in
    check string "unchanged" html result
  in
  let test_vv_apply_corrections_chain () =
    let html = "div { padding: 10px; gap: 5px; }" in
    let hints = [Visual_verifier.AdjustPadding (2., 0., 0., 0.); Visual_verifier.AdjustGap 1.0] in
    let result = Visual_verifier.apply_corrections hints html in
    check bool "modified" true (result <> html)
  in

  (* -- mkdir_p -- *)
  let test_vv_mkdir_p () =
    let dir = Filename.concat (Filename.get_temp_dir_name ()) "figma_test_mkdir_p/a/b/c" in
    Visual_verifier.mkdir_p dir;
    check bool "exists" true (Sys.file_exists dir);
    (* cleanup *)
    let _ = Sys.command (Printf.sprintf "rm -rf %s" (Filename.concat (Filename.get_temp_dir_name ()) "figma_test_mkdir_p")) in
    ()
  in
  let test_vv_mkdir_p_existing () =
    let dir = Filename.get_temp_dir_name () in
    Visual_verifier.mkdir_p dir;
    check bool "still exists" true (Sys.file_exists dir)
  in

  (* -- copy_file -- *)
  let test_vv_copy_file () =
    let src = Filename.temp_file "figma_copy_src" ".txt" in
    let dst = Filename.temp_file "figma_copy_dst" ".txt" in
    let data = "Hello, World. This is test data for copy_file." in
    Out_channel.with_open_bin src (fun oc -> output_string oc data);
    Visual_verifier.copy_file ~src ~dst;
    let result = In_channel.with_open_bin dst (fun ic ->
      really_input_string ic (in_channel_length ic)
    ) in
    check string "content" data result;
    Sys.remove src;
    Sys.remove dst
  in

  (* -- generate_temp_filename -- *)
  let test_vv_generate_temp_filename () =
    let f = Visual_verifier.generate_temp_filename ~prefix:"test" ~ext:"png" in
    check bool "has prefix" true (String.length f > 4);
    check bool "has ext" true (Filename.check_suffix f ".png")
  in

  (* -- log_verification, log_improvement, log_hint_application -- *)
  let test_vv_log_verification () =
    (* This writes to .figma-ssim.log, should not crash *)
    Visual_verifier.log_verification ~node_id:"test-node-1" ~ssim:0.95 ();
    check bool "no crash" true true
  in
  let test_vv_log_verification_with_notes () =
    Visual_verifier.log_verification ~node_id:"test-node-2" ~ssim:0.90 ~notes:"test notes" ();
    check bool "no crash" true true
  in
  let test_vv_log_improvement () =
    let improvement = Visual_verifier.log_improvement
      ~node_id:"test-node-3" ~before_ssim:0.80 ~after_ssim:0.90
      ~change_description:"adjusted padding" in
    check (float 0.1) "improvement" 10.0 improvement
  in
  let test_vv_log_hint_application () =
    Visual_verifier.log_hint_application
      ~node_id:"test-node-4" ~before_ssim:0.85 ~after_ssim:0.90
      ~hints:[Visual_verifier.AdjustPadding (1., 1., 1., 1.); Visual_verifier.AdjustGap 2.0];
    check bool "no crash" true true
  in
  let test_vv_log_hint_application_negative () =
    Visual_verifier.log_hint_application
      ~node_id:"test-node-5" ~before_ssim:0.90 ~after_ssim:0.85
      ~hints:[Visual_verifier.AdjustColor (".x", mk_rgba 1. 0. 0. 1.)];
    check bool "no crash" true true
  in
  let test_vv_log_hint_all_types () =
    Visual_verifier.log_hint_application
      ~node_id:"test-node-6" ~before_ssim:0.80 ~after_ssim:0.90
      ~hints:[
        Visual_verifier.AdjustPadding (1., 2., 3., 4.);
        Visual_verifier.AdjustGap 5.;
        Visual_verifier.AdjustColor (".c", mk_rgba 0. 0. 0. 1.);
        Visual_verifier.AdjustSize (1., 2.);
        Visual_verifier.AdjustFontSize 3.;
        Visual_verifier.AdjustBorderRadius 4.;
      ];
    check bool "no crash" true true
  in

  (* -- get_recent_logs, get_ssim_trend -- *)
  let test_vv_get_recent_logs () =
    (* After writing logs above, should be able to read them *)
    let logs = Visual_verifier.get_recent_logs ~count:5 () in
    check bool "has logs" true (List.length logs > 0)
  in
  let test_vv_get_ssim_trend () =
    let trend = Visual_verifier.get_ssim_trend ~node_id:"test-node-1" in
    check bool "has trend" true (List.length trend > 0)
  in
  let test_vv_get_ssim_trend_unknown () =
    let trend = Visual_verifier.get_ssim_trend ~node_id:"nonexistent-node-xyz" in
    check int "empty trend" 0 (List.length trend)
  in

  (* ====== RUN ALL ====== *)
  run "verifiers-w4" [
    ("semantic_verifier/normalize_text", [
      test_case "simple" `Quick test_sv_normalize_text_simple;
      test_case "newlines" `Quick test_sv_normalize_text_newlines;
      test_case "empty" `Quick test_sv_normalize_text_empty;
    ]);
    ("semantic_verifier/rgba_dist_rgb", [
      test_case "same" `Quick test_sv_rgba_dist_same;
      test_case "different" `Quick test_sv_rgba_dist_different;
      test_case "black_white" `Quick test_sv_rgba_dist_black_white;
    ]);
    ("semantic_verifier/bbox_area", [
      test_case "normal" `Quick test_sv_bbox_area_normal;
      test_case "zero" `Quick test_sv_bbox_area_zero;
      test_case "single_dim" `Quick test_sv_bbox_area_single_dim_zero;
    ]);
    ("semantic_verifier/bbox_center", [
      test_case "offset" `Quick test_sv_bbox_center;
      test_case "origin" `Quick test_sv_bbox_center_origin;
    ]);
    ("semantic_verifier/bbox_delta", [
      test_case "identical" `Quick test_sv_bbox_delta_identical;
      test_case "shifted" `Quick test_sv_bbox_delta_shifted;
    ]);
    ("semantic_verifier/bbox_within", [
      test_case "exact" `Quick test_sv_bbox_within_exact;
      test_case "inside" `Quick test_sv_bbox_within_inside_tol;
      test_case "outside" `Quick test_sv_bbox_within_outside_tol;
    ]);
    ("semantic_verifier/normalize_bbox", [
      test_case "basic" `Quick test_sv_normalize_bbox;
    ]);
    ("semantic_verifier/pick_root_bbox", [
      test_case "with_root" `Quick test_sv_pick_root_with_root;
      test_case "no_root" `Quick test_sv_pick_root_no_root;
    ]);
    ("semantic_verifier/solid_color_of_paints", [
      test_case "found" `Quick test_sv_solid_color_found;
      test_case "empty" `Quick test_sv_solid_color_empty;
      test_case "invisible" `Quick test_sv_solid_color_invisible;
      test_case "gradient_then_solid" `Quick test_sv_solid_color_gradient_then_solid;
    ]);
    ("semantic_verifier/extract_expected_texts", [
      test_case "basic" `Quick test_sv_extract_texts_basic;
      test_case "empty_chars" `Quick test_sv_extract_texts_empty_chars;
      test_case "no_bbox" `Quick test_sv_extract_texts_no_bbox;
    ]);
    ("semantic_verifier/choose_best_text_candidate", [
      test_case "exact" `Quick test_sv_choose_exact_match;
      test_case "no_match" `Quick test_sv_choose_no_match;
      test_case "substring" `Quick test_sv_choose_substring_match;
      test_case "short_no_substring" `Quick test_sv_choose_short_text_no_substring;
      test_case "best_by_distance" `Quick test_sv_choose_best_by_distance;
    ]);
    ("semantic_verifier/verify", [
      test_case "no_bbox" `Quick test_sv_verify_no_bbox;
      test_case "root_size_mismatch" `Quick test_sv_verify_root_size_mismatch;
      test_case "bg_mismatch" `Quick test_sv_verify_bg_mismatch;
      test_case "border_radius" `Quick test_sv_verify_border_radius_mismatch;
      test_case "font_size" `Quick test_sv_verify_font_size_mismatch;
      test_case "font_weight" `Quick test_sv_verify_font_weight_mismatch;
      test_case "text_color" `Quick test_sv_verify_text_color_mismatch;
      test_case "score_calculation" `Quick test_sv_verify_score_calculation;
    ]);
    ("semantic_verifier/mismatch_to_json", [
      test_case "root_size" `Quick test_sv_mismatch_json_root_size;
      test_case "root_bg" `Quick test_sv_mismatch_json_root_bg;
      test_case "root_radius" `Quick test_sv_mismatch_json_root_radius;
      test_case "missing_text" `Quick test_sv_mismatch_json_missing_text;
      test_case "text_bbox" `Quick test_sv_mismatch_json_text_bbox;
      test_case "font_size" `Quick test_sv_mismatch_json_font_size;
      test_case "font_weight" `Quick test_sv_mismatch_json_font_weight;
      test_case "text_color" `Quick test_sv_mismatch_json_text_color;
    ]);
    ("semantic_verifier/json_helpers", [
      test_case "bbox_to_json" `Quick test_sv_bbox_to_json;
      test_case "rgba_to_json" `Quick test_sv_rgba_to_json;
      test_case "result_to_json" `Quick test_sv_result_to_json;
    ]);
    ("figma_image_similarity/is_space", [
      test_case "true" `Quick test_is_space_true;
      test_case "false" `Quick test_is_space_false;
    ]);
    ("figma_image_similarity/parse_ppm_header", [
      test_case "valid" `Quick test_ppm_header_valid;
      test_case "comment" `Quick test_ppm_header_with_comment;
      test_case "empty" `Quick test_ppm_header_empty;
      test_case "incomplete" `Quick test_ppm_header_incomplete;
    ]);
    ("figma_image_similarity/mse_psnr", [
      test_case "identical" `Quick test_mse_psnr_identical;
      test_case "different" `Quick test_mse_psnr_different;
      test_case "zero_area" `Quick test_mse_psnr_zero_area;
      test_case "different_sizes" `Quick test_mse_psnr_different_sizes;
    ]);
    ("figma_image_similarity/ssim_window", [
      test_case "identical" `Quick test_ssim_window_identical;
      test_case "zero" `Quick test_ssim_window_zero;
      test_case "different" `Quick test_ssim_window_different;
    ]);
    ("figma_image_similarity/ssim", [
      test_case "identical" `Quick test_ssim_identical;
      test_case "zero_size" `Quick test_ssim_zero_size;
      test_case "negative_window" `Quick test_ssim_negative_window;
      test_case "too_small" `Quick test_ssim_too_small_for_window;
    ]);
    ("figma_image_similarity/calculate_avg_delta_e", [
      test_case "identical" `Quick test_delta_e_identical;
      test_case "zero_area" `Quick test_delta_e_zero_area;
      test_case "different" `Quick test_delta_e_different;
    ]);
    ("visual_verifier/step_to_json", [
      test_case "basic" `Quick test_vv_step_to_json;
    ]);
    ("visual_verifier/apply_color_adjustment", [
      test_case "no_selector" `Quick test_vv_apply_color_no_selector;
      test_case "with_selector" `Quick test_vv_apply_color_with_selector;
      test_case "selector_not_found" `Quick test_vv_apply_color_selector_not_found;
    ]);
    ("visual_verifier/apply_single_hint", [
      test_case "color" `Quick test_vv_apply_single_hint_color;
    ]);
    ("visual_verifier/apply_corrections", [
      test_case "empty" `Quick test_vv_apply_corrections_empty;
      test_case "chain" `Quick test_vv_apply_corrections_chain;
    ]);
    ("visual_verifier/mkdir_p", [
      test_case "create" `Quick test_vv_mkdir_p;
      test_case "existing" `Quick test_vv_mkdir_p_existing;
    ]);
    ("visual_verifier/copy_file", [
      test_case "basic" `Quick test_vv_copy_file;
    ]);
    ("visual_verifier/generate_temp_filename", [
      test_case "basic" `Quick test_vv_generate_temp_filename;
    ]);
    ("visual_verifier/logging", [
      test_case "log_verification" `Quick test_vv_log_verification;
      test_case "log_with_notes" `Quick test_vv_log_verification_with_notes;
      test_case "log_improvement" `Quick test_vv_log_improvement;
      test_case "log_hint_application" `Quick test_vv_log_hint_application;
      test_case "log_hint_negative" `Quick test_vv_log_hint_application_negative;
      test_case "log_hint_all_types" `Quick test_vv_log_hint_all_types;
    ]);
    ("visual_verifier/get_logs", [
      test_case "recent" `Quick test_vv_get_recent_logs;
      test_case "trend" `Quick test_vv_get_ssim_trend;
      test_case "trend_unknown" `Quick test_vv_get_ssim_trend_unknown;
    ]);
  ]
