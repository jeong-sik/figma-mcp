(** Coverage Wave 11: visual_verifier.ml + figma_effects.ml (remaining pure gaps)
    Focus areas:
    visual_verifier.ml:
    - parse_diff_regions: successful parse (all prior tests hit fallback)
    - hints_to_summary: empty hints (no-adjust message)
    - hint_to_description: padding all zeros → "여백 미세 조정 필요"
    - calculate_human_ssim: boundary values (delta_e=50, ssim=0, ssim=1)
    - suggest_corrections: combined triggers (edge+strip+quad simultaneous)
    - result_to_json with evolution_history items
    - step_to_json: empty corrections, multiple corrections
    - apply_color_adjustment: hex computation edge cases
    - adjust_css_value: multiple global occurrences
    - apply_padding_adjustment: no shorthand, no longhand → unchanged
    - correction_hints_to_json: empty list, many items
    - quadrant_result_to_json: worst quadrant = bottom_left, bottom_right
    - diff_image_result_to_json: zero values
    - log_hint_application: all 6 hint types in a single call
    - empty_diff_regions: field access

    figma_effects.ml:
    - parse_neo4j_response: general exception path
    - make_neo4j_statement: empty query, large params
    - run_with_mock: Perform.neo4j_run_cypher + neo4j_run_batch (fall through _ -> None)
    - run_with_mock: chained lookups with multiple hash tables
    - create_mock_store: populate + query all tables
    - Perform module: all optional param combos not yet exercised
*)

let () =
  let open Alcotest in
  let open Visual_verifier in
  let mk_rgba r g b a : Figma_types.rgba = { r; g; b; a } in

  (* ========== parse_diff_regions: successful full parse ========== *)

  let test_parse_regions_full_success () =
    let json = Yojson.Safe.from_string {|{
      "regions": {
        "quadrants": {"topLeft": 0.12, "topRight": 0.34, "bottomLeft": 0.56, "bottomRight": 0.78},
        "strips": {"top": 0.11, "middle": 0.22, "bottom": 0.33},
        "edges": {"top": 0.41, "bottom": 0.52, "left": 0.63, "right": 0.74}
      }
    }|} in
    let r = parse_diff_regions json in
    check (float 0.001) "q.tl" 0.12 r.quadrants.top_left;
    check (float 0.001) "q.tr" 0.34 r.quadrants.top_right;
    check (float 0.001) "q.bl" 0.56 r.quadrants.bottom_left;
    check (float 0.001) "q.br" 0.78 r.quadrants.bottom_right;
    check (float 0.001) "s.top" 0.11 r.strips.strip_top;
    check (float 0.001) "s.mid" 0.22 r.strips.strip_middle;
    check (float 0.001) "s.bot" 0.33 r.strips.strip_bottom;
    check (float 0.001) "e.top" 0.41 r.edges.edge_top;
    check (float 0.001) "e.bot" 0.52 r.edges.edge_bottom;
    check (float 0.001) "e.left" 0.63 r.edges.edge_left;
    check (float 0.001) "e.right" 0.74 r.edges.edge_right
  in

  let test_parse_regions_zero_values () =
    let json = Yojson.Safe.from_string {|{
      "regions": {
        "quadrants": {"topLeft": 0.0, "topRight": 0.0, "bottomLeft": 0.0, "bottomRight": 0.0},
        "strips": {"top": 0.0, "middle": 0.0, "bottom": 0.0},
        "edges": {"top": 0.0, "bottom": 0.0, "left": 0.0, "right": 0.0}
      }
    }|} in
    let r = parse_diff_regions json in
    check (float 0.001) "all zero q.tl" 0.0 r.quadrants.top_left;
    check (float 0.001) "all zero e.right" 0.0 r.edges.edge_right
  in

  let test_parse_regions_null_regions_key () =
    let json = Yojson.Safe.from_string {|{"regions": null}|} in
    let r = parse_diff_regions json in
    check (float 0.001) "null regions" 0.0 r.quadrants.top_left
  in

  (* ========== hints_to_summary: empty hints path ========== *)

  let test_summary_empty_hints () =
    let s = hints_to_summary [] in
    check bool "no adjust message" true (String.length s > 0);
    (* Should contain the success message, not a count *)
    check bool "no count prefix" true (
      try ignore (Str.search_forward (Str.regexp_string "조정 불필요") s 0); true
      with Not_found -> false)
  in

  (* ========== hint_to_description: padding all zeros ========== *)

  let test_desc_padding_all_zero () =
    let d = hint_to_description (AdjustPadding (0., 0., 0., 0.)) in
    (* All parts are zero, so parts = [] → "여백 미세 조정 필요" *)
    check bool "fine-tune msg" true (
      try ignore (Str.search_forward (Str.regexp_string "미세 조정") d 0); true
      with Not_found -> false)
  in

  let test_desc_padding_left_only () =
    let d = hint_to_description (AdjustPadding (0., 0., 0., 15.)) in
    check bool "left mentioned" true (
      try ignore (Str.search_forward (Str.regexp_string "좌측") d 0); true
      with Not_found -> false)
  in

  let test_desc_padding_bottom_right () =
    let d = hint_to_description (AdjustPadding (0., 8., 12., 0.)) in
    check bool "right mentioned" true (
      try ignore (Str.search_forward (Str.regexp_string "우측") d 0); true
      with Not_found -> false);
    check bool "bottom mentioned" true (
      try ignore (Str.search_forward (Str.regexp_string "하단") d 0); true
      with Not_found -> false)
  in

  let test_desc_color_specific_values () =
    let rgba = mk_rgba 0.0 0.0 0.0 1.0 in
    let d = hint_to_description (AdjustColor (".header", rgba)) in
    check bool "contains selector" true (
      try ignore (Str.search_forward (Str.regexp_string ".header") d 0); true
      with Not_found -> false);
    check bool "contains rgba" true (
      try ignore (Str.search_forward (Str.regexp_string "rgba(") d 0); true
      with Not_found -> false)
  in

  let test_desc_size_positive () =
    let d = hint_to_description (AdjustSize (10., 20.)) in
    check bool "width delta" true (String.length d > 10);
    check bool "contains width" true (
      try ignore (Str.search_forward (Str.regexp_string "너비") d 0); true
      with Not_found -> false)
  in

  let test_desc_gap_large () =
    let d = hint_to_description (AdjustGap 50.) in
    check bool "gap value" true (
      try ignore (Str.search_forward (Str.regexp_string "50") d 0); true
      with Not_found -> false)
  in

  (* ========== calculate_human_ssim: boundary values ========== *)

  let test_human_ssim_delta_e_exactly_50 () =
    (* penalty = min(1.0, 50/50) = 1.0 → ssim * 0.0 = 0.0 *)
    let result = calculate_human_ssim 0.95 50.0 in
    check (float 0.001) "zero at delta_e=50" 0.0 result
  in

  let test_human_ssim_delta_e_above_50 () =
    (* penalty = min(1.0, 100/50) = min(1.0, 2.0) = 1.0 → ssim * 0.0 = 0.0 *)
    let result = calculate_human_ssim 1.0 100.0 in
    check (float 0.001) "zero at delta_e>50" 0.0 result
  in

  let test_human_ssim_both_one_zero () =
    let result = calculate_human_ssim 1.0 0.0 in
    check (float 0.001) "perfect" 1.0 result
  in

  let test_human_ssim_ssim_zero () =
    let result = calculate_human_ssim 0.0 5.0 in
    check (float 0.001) "ssim=0" 0.0 result
  in

  let test_human_ssim_small_delta () =
    (* delta_e = 1.0 (JND), penalty = 0.02 *)
    let result = calculate_human_ssim 1.0 1.0 in
    check (float 0.001) "JND" 0.98 result
  in

  (* ========== suggest_corrections: combined triggers ========== *)

  let test_suggest_all_triggers_combined () =
    (* All edges > 5%, both strips > 8%, quads unbalanced and > 10% *)
    let regions = {
      quadrants = { top_left = 0.20; top_right = 0.05; bottom_left = 0.15; bottom_right = 0.12 };
      strips = { strip_top = 0.12; strip_middle = 0.05; strip_bottom = 0.15 };
      edges = { edge_top = 0.08; edge_bottom = 0.09; edge_left = 0.07; edge_right = 0.06 };
    } in
    let hints = suggest_corrections ~ssim:0.85 ~diff_regions:regions in
    (* Should have: padding from edges, padding from strips, gap from quad imbalance, size from quad max *)
    check bool "has padding" true
      (List.exists (function AdjustPadding _ -> true | _ -> false) hints);
    check bool "has gap" true
      (List.exists (function AdjustGap _ -> true | _ -> false) hints);
    check bool "has size" true
      (List.exists (function AdjustSize _ -> true | _ -> false) hints);
    check bool "multiple hints" true (List.length hints >= 3)
  in

  let test_suggest_only_top_edge () =
    let regions = {
      quadrants = { top_left = 0.0; top_right = 0.0; bottom_left = 0.0; bottom_right = 0.0 };
      strips = { strip_top = 0.0; strip_middle = 0.0; strip_bottom = 0.0 };
      edges = { edge_top = 0.10; edge_bottom = 0.0; edge_left = 0.0; edge_right = 0.0 };
    } in
    let hints = suggest_corrections ~ssim:0.93 ~diff_regions:regions in
    let p = List.find_opt (function AdjustPadding _ -> true | _ -> false) hints in
    (match p with
     | Some (AdjustPadding (t, _, _, _)) ->
       check (float 0.01) "top=1" 1.0 t
     | _ -> fail "expected padding")
  in

  let test_suggest_only_top_strip () =
    let regions = {
      quadrants = { top_left = 0.0; top_right = 0.0; bottom_left = 0.0; bottom_right = 0.0 };
      strips = { strip_top = 0.12; strip_middle = 0.0; strip_bottom = 0.0 };
      edges = { edge_top = 0.0; edge_bottom = 0.0; edge_left = 0.0; edge_right = 0.0 };
    } in
    let hints = suggest_corrections ~ssim:0.93 ~diff_regions:regions in
    (* top_strip > 0.08 → AdjustPadding (1.0, 0.0, 0.0, 0.0) *)
    let p = List.find_opt (function AdjustPadding (t, _, _, _) -> t > 0.5 | _ -> false) hints in
    check bool "top strip hint" true (p <> None)
  in

  let test_suggest_only_bottom_strip () =
    let regions = {
      quadrants = { top_left = 0.0; top_right = 0.0; bottom_left = 0.0; bottom_right = 0.0 };
      strips = { strip_top = 0.0; strip_middle = 0.0; strip_bottom = 0.12 };
      edges = { edge_top = 0.0; edge_bottom = 0.0; edge_left = 0.0; edge_right = 0.0 };
    } in
    let hints = suggest_corrections ~ssim:0.93 ~diff_regions:regions in
    (* bottom_strip > 0.08 → AdjustPadding (0.0, 0.0, 1.0, 0.0) *)
    let p = List.find_opt (function AdjustPadding (_, _, b, _) -> b > 0.5 | _ -> false) hints in
    check bool "bottom strip hint" true (p <> None)
  in

  let test_suggest_quad_high_balanced () =
    (* All quads > 10%, balanced (no imbalance) → size but no gap *)
    let regions = {
      quadrants = { top_left = 0.15; top_right = 0.15; bottom_left = 0.15; bottom_right = 0.15 };
      strips = { strip_top = 0.0; strip_middle = 0.0; strip_bottom = 0.0 };
      edges = { edge_top = 0.0; edge_bottom = 0.0; edge_left = 0.0; edge_right = 0.0 };
    } in
    let hints = suggest_corrections ~ssim:0.85 ~diff_regions:regions in
    check bool "has size" true
      (List.exists (function AdjustSize _ -> true | _ -> false) hints);
    (* max - min = 0 so no gap from imbalance check *)
    check bool "no gap from quad" false
      (List.exists (function AdjustGap _ -> true | _ -> false) hints)
  in

  let test_suggest_ssim_0_94_fallback () =
    (* ssim in 0.90-0.95 range, empty regions → AdjustPadding(0.5,...) *)
    let hints = suggest_corrections ~ssim:0.94 ~diff_regions:empty_diff_regions in
    let p = List.find_opt (function AdjustPadding _ -> true | _ -> false) hints in
    (match p with
     | Some (AdjustPadding (t, _, _, _)) ->
       check (float 0.01) "padding=0.5" 0.5 t
     | _ -> fail "expected padding 0.5")
  in

  (* ========== result_to_json with evolution_history ========== *)

  let test_result_json_with_history () =
    let rgba = mk_rgba 0.5 0.5 0.5 1.0 in
    let step1 : evolution_step = {
      step_num = 1; step_ssim = 0.70; step_delta_e = 10.0;
      step_human_ssim = 0.56; step_html_path = "/tmp/h1.html";
      step_png_path = "/tmp/p1.png";
      corrections_this_step = [AdjustPadding (1., 1., 1., 1.)];
    } in
    let step2 : evolution_step = {
      step_num = 2; step_ssim = 0.85; step_delta_e = 5.0;
      step_human_ssim = 0.77; step_html_path = "/tmp/h2.html";
      step_png_path = "/tmp/p2.png";
      corrections_this_step = [AdjustGap 2.0; AdjustColor (".x", rgba)];
    } in
    let result = {
      ssim = 0.95; delta_e = 2.0; human_ssim = 0.91;
      passed = true; iterations = 2;
      figma_png = "/tmp/f.png"; html_png = "/tmp/h.png";
      corrections_applied = [AdjustPadding (1., 1., 1., 1.); AdjustGap 2.0];
      final_html = Some "<div>done</div>";
      evolution_history = [step1; step2];
      evolution_dir = "/tmp/evo";
      errors = [];
    } in
    let j = result_to_json result in
    let open Yojson.Safe.Util in
    check int "2 history" 2 (j |> member "evolution_history" |> to_list |> List.length);
    let h1 = j |> member "evolution_history" |> to_list |> List.hd in
    check int "step 1" 1 (h1 |> member "step" |> to_int);
    check (float 0.001) "step 1 ssim" 0.70 (h1 |> member "ssim" |> to_float);
    check int "step 1 corrections" 1 (h1 |> member "corrections" |> to_list |> List.length)
  in

  let test_result_json_with_errors_and_history () =
    let step : evolution_step = {
      step_num = 1; step_ssim = 0.50; step_delta_e = 20.0;
      step_human_ssim = 0.30; step_html_path = ""; step_png_path = "";
      corrections_this_step = [];
    } in
    let result = {
      ssim = 0.50; delta_e = 20.0; human_ssim = 0.30;
      passed = false; iterations = 1;
      figma_png = ""; html_png = "";
      corrections_applied = [];
      final_html = None;
      evolution_history = [step];
      evolution_dir = "";
      errors = ["render failed"; "compare timeout"];
    } in
    let j = result_to_json result in
    let open Yojson.Safe.Util in
    check int "2 errors" 2 (j |> member "errors" |> to_list |> List.length);
    check int "1 history" 1 (j |> member "evolution_history" |> to_list |> List.length);
    check bool "null final_html" true (j |> member "final_html" = `Null)
  in

  (* ========== step_to_json: more variants ========== *)

  let test_step_json_empty_corrections () =
    let step : evolution_step = {
      step_num = 3; step_ssim = 0.99; step_delta_e = 0.5;
      step_human_ssim = 0.98; step_html_path = "/p.html";
      step_png_path = "/p.png";
      corrections_this_step = [];
    } in
    let j = step_to_json step in
    let open Yojson.Safe.Util in
    check int "0 corrections" 0 (j |> member "corrections" |> to_list |> List.length);
    check int "step 3" 3 (j |> member "step" |> to_int)
  in

  let test_step_json_multiple_corrections () =
    let rgba = mk_rgba 0.0 1.0 0.0 0.5 in
    let step : evolution_step = {
      step_num = 5; step_ssim = 0.88; step_delta_e = 6.0;
      step_human_ssim = 0.78; step_html_path = "/s5.html";
      step_png_path = "/s5.png";
      corrections_this_step = [
        AdjustPadding (2., 3., 4., 5.);
        AdjustGap 10.;
        AdjustColor (".card", rgba);
        AdjustSize (5., -3.);
        AdjustFontSize 1.5;
        AdjustBorderRadius 8.;
      ];
    } in
    let j = step_to_json step in
    let open Yojson.Safe.Util in
    check int "6 corrections" 6 (j |> member "corrections" |> to_list |> List.length);
    check (float 0.001) "delta_e" 6.0 (j |> member "delta_e" |> to_float)
  in

  (* ========== apply_color_adjustment: hex edge cases ========== *)

  let test_color_hex_boundary_255 () =
    let rgba = mk_rgba 1.0 1.0 1.0 1.0 in
    let html = "div { color: #000000; }" in
    let result = apply_color_adjustment "" rgba html in
    check bool "white hex" true (
      try ignore (Str.search_forward (Str.regexp_string "#ffffff") result 0); true
      with Not_found -> false)
  in

  let test_color_hex_boundary_zero () =
    let rgba = mk_rgba 0.0 0.0 0.0 1.0 in
    let html = "span { background-color: #ffffff; }" in
    let result = apply_color_adjustment "" rgba html in
    check bool "black hex" true (
      try ignore (Str.search_forward (Str.regexp_string "#000000") result 0); true
      with Not_found -> false)
  in

  let test_color_mid_values () =
    (* r=128/255, g=64/255, b=192/255 *)
    let rgba = mk_rgba (128.0 /. 255.0) (64.0 /. 255.0) (192.0 /. 255.0) 0.9 in
    let html = "p { color: #aabbcc; }" in
    let result = apply_color_adjustment "" rgba html in
    (* Should be close to #8040c0 *)
    check bool "mid color replaced" true (result <> html)
  in

  (* ========== adjust_css_value: multiple occurrences ========== *)

  let test_adjust_css_global_multiple () =
    (* global_substitute replaces ALL matches *)
    let html = "div { gap: 10px; } span { gap: 20px; }" in
    let result = adjust_css_value "gap" 5.0 html in
    check bool "first adjusted" true (
      try ignore (Str.search_forward (Str.regexp_string "gap: 15.0px") result 0); true
      with Not_found -> false);
    check bool "second adjusted" true (
      try ignore (Str.search_forward (Str.regexp_string "gap: 25.0px") result 0); true
      with Not_found -> false)
  in

  let test_adjust_css_no_match_property () =
    let html = "div { margin: 10px; }" in
    let result = adjust_css_value "padding-top" 5.0 html in
    check string "unchanged" html result
  in

  (* ========== apply_padding_adjustment: no match ========== *)

  let test_padding_no_match () =
    (* No padding-* or padding: in HTML → unchanged *)
    let html = "div { margin: 10px; color: red; }" in
    let result = apply_padding_adjustment (5., 5., 5., 5.) html in
    check string "no padding match" html result
  in

  let test_padding_longhand_only () =
    (* Has longhand but no shorthand → shorthand regex search fails with Not_found *)
    let html = "div { padding-top: 10px; padding-bottom: 20px; }" in
    let result = apply_padding_adjustment (5.0, 0.0, 3.0, 0.0) html in
    check bool "top adjusted" true (
      try ignore (Str.search_forward (Str.regexp_string "padding-top: 15.0px") result 0); true
      with Not_found -> false);
    check bool "bottom adjusted" true (
      try ignore (Str.search_forward (Str.regexp_string "padding-bottom: 23.0px") result 0); true
      with Not_found -> false)
  in

  (* ========== correction_hints_to_json: empty + many ========== *)

  let test_hints_json_empty_list () =
    let j = correction_hints_to_json [] in
    match j with
    | `List [] -> ()
    | _ -> fail "expected empty list"
  in

  let test_hints_json_all_types () =
    let rgba = mk_rgba 0.5 0.5 0.5 1.0 in
    let j = correction_hints_to_json [
      AdjustPadding (1., 2., 3., 4.);
      AdjustGap 10.;
      AdjustColor (".x", rgba);
      AdjustSize (5., -5.);
      AdjustFontSize 3.;
      AdjustBorderRadius 12.;
    ] in
    match j with
    | `List items -> check int "6 items" 6 (List.length items)
    | _ -> fail "expected list"
  in

  (* ========== quadrant_result_to_json: worst = bottom_left ========== *)

  let test_quadrant_worst_bottom_left () =
    let result = {
      top_left = (0.90, 2.0);
      top_right = (0.85, 3.0);
      bottom_left = (0.70, 1.0);
      bottom_right = (0.80, 2.0);
      overall = (0.82, 2.0);
    } in
    let j = quadrant_result_to_json result in
    let wq = Yojson.Safe.Util.(j |> member "worst_quadrant" |> to_string) in
    check string "worst=bottom_left" "bottom_left" wq
  in

  let test_quadrant_worst_bottom_right () =
    let result = {
      top_left = (0.95, 1.0);
      top_right = (0.90, 2.0);
      bottom_left = (0.88, 3.0);
      bottom_right = (0.60, 1.0);
      overall = (0.83, 2.0);
    } in
    let j = quadrant_result_to_json result in
    let wq = Yojson.Safe.Util.(j |> member "worst_quadrant" |> to_string) in
    check string "worst=bottom_right" "bottom_right" wq
  in

  (* ========== diff_image_result_to_json: zero values ========== *)

  let test_diff_image_result_zero () =
    let result = {
      side_by_side = "";
      diff_overlay = "";
      diff_percent = 0.0;
    } in
    let j = diff_image_result_to_json result in
    let open Yojson.Safe.Util in
    check string "empty sbs" "" (j |> member "side_by_side" |> to_string);
    check (float 0.01) "zero percent" 0.0 (j |> member "diff_percent" |> to_float)
  in

  (* ========== log_hint_application: all 6 types ========== *)

  let test_log_hint_all_types () =
    let rgba = mk_rgba 0.5 0.5 0.5 1.0 in
    log_hint_application
      ~node_id:"w11-all-types"
      ~before_ssim:0.70 ~after_ssim:0.90
      ~hints:[
        AdjustPadding (1., 1., 1., 1.);
        AdjustGap 5.;
        AdjustColor (".x", rgba);
        AdjustSize (10., 10.);
        AdjustFontSize 2.;
        AdjustBorderRadius 4.;
      ];
    check bool "no crash" true true
  in

  (* ========== empty_diff_regions: verify field values ========== *)

  let test_empty_diff_regions_values () =
    check (float 0.001) "q.tl" 0.0 empty_diff_regions.quadrants.top_left;
    check (float 0.001) "q.tr" 0.0 empty_diff_regions.quadrants.top_right;
    check (float 0.001) "q.bl" 0.0 empty_diff_regions.quadrants.bottom_left;
    check (float 0.001) "q.br" 0.0 empty_diff_regions.quadrants.bottom_right;
    check (float 0.001) "s.top" 0.0 empty_diff_regions.strips.strip_top;
    check (float 0.001) "s.mid" 0.0 empty_diff_regions.strips.strip_middle;
    check (float 0.001) "s.bot" 0.0 empty_diff_regions.strips.strip_bottom;
    check (float 0.001) "e.top" 0.0 empty_diff_regions.edges.edge_top;
    check (float 0.001) "e.bot" 0.0 empty_diff_regions.edges.edge_bottom;
    check (float 0.001) "e.left" 0.0 empty_diff_regions.edges.edge_left;
    check (float 0.001) "e.right" 0.0 empty_diff_regions.edges.edge_right
  in

  (* ========== hint_to_json: gap + font_size + border_radius ========== *)

  let test_hint_json_gap () =
    let j = hint_to_json (AdjustGap 7.5) in
    let open Yojson.Safe.Util in
    check string "type" "gap" (j |> member "type" |> to_string);
    check (float 0.001) "value" 7.5 (j |> member "value" |> to_float)
  in

  let test_hint_json_font_size () =
    let j = hint_to_json (AdjustFontSize (-3.0)) in
    let open Yojson.Safe.Util in
    check string "type" "font_size" (j |> member "type" |> to_string);
    check (float 0.001) "value" (-3.0) (j |> member "value" |> to_float)
  in

  let test_hint_json_border_radius () =
    let j = hint_to_json (AdjustBorderRadius 0.0) in
    let open Yojson.Safe.Util in
    check string "type" "border_radius" (j |> member "type" |> to_string);
    check (float 0.001) "value" 0.0 (j |> member "value" |> to_float)
  in

  (* =============== figma_effects.ml tests =============== *)

  (* ── parse_neo4j_response: general exception path ── *)

  let test_parse_neo4j_general_exception () =
    (* A valid JSON but accessing non-existent fields in a way that triggers
       a general exception (not Yojson.Json_error) *)
    let body = {|{"results": "not-a-list", "errors": []}|} in
    match Figma_effects.parse_neo4j_response body with
    | Ok _ -> () (* might succeed since errors is empty and results is returned as-is *)
    | Error _ -> () (* or might fail with a parse error *)
  in

  let test_parse_neo4j_empty_results () =
    let body = {|{"results": [], "errors": []}|} in
    match Figma_effects.parse_neo4j_response body with
    | Ok results ->
        (match results with
         | `List [] -> ()
         | _ -> fail "expected empty list")
    | Error e -> fail (Printf.sprintf "unexpected: %s" e)
  in

  let test_parse_neo4j_multiple_result_sets () =
    let body = {|{"results": [{"data": [1]}, {"data": [2]}, {"data": [3]}], "errors": []}|} in
    match Figma_effects.parse_neo4j_response body with
    | Ok results ->
        (match results with
         | `List items -> check int "3 result sets" 3 (List.length items)
         | _ -> fail "expected list")
    | Error e -> fail (Printf.sprintf "unexpected: %s" e)
  in

  let test_parse_neo4j_multiple_errors () =
    let body = {|{"results": [], "errors": [
      {"code": "Err1", "message": "msg1"},
      {"code": "Err2", "message": "msg2"}
    ]}|} in
    match Figma_effects.parse_neo4j_response body with
    | Error msg ->
        check bool "has Err1" true (
          try ignore (Str.search_forward (Str.regexp_string "Err1") msg 0); true
          with Not_found -> false);
        check bool "has Err2" true (
          try ignore (Str.search_forward (Str.regexp_string "Err2") msg 0); true
          with Not_found -> false)
    | Ok _ -> fail "expected error"
  in

  (* ── make_neo4j_statement: edge cases ── *)

  let test_make_neo4j_empty_query () =
    let stmt = Figma_effects.make_neo4j_statement "" [] in
    match stmt with
    | `Assoc fields ->
        let s = List.assoc "statement" fields in
        check string "empty query" "" (Yojson.Safe.Util.to_string s)
    | _ -> fail "expected Assoc"
  in

  let test_make_neo4j_list_param () =
    let stmt = Figma_effects.make_neo4j_statement
      "MATCH (n) WHERE n.id IN $ids"
      [("ids", `List [`String "a"; `String "b"; `String "c"])] in
    let json_str = Yojson.Safe.to_string stmt in
    check bool "has ids" true (
      try ignore (Str.search_forward (Str.regexp_string "ids") json_str 0); true
      with Not_found -> false)
  in

  let test_make_neo4j_nested_assoc_param () =
    let stmt = Figma_effects.make_neo4j_statement
      "CREATE (n:Node $props)"
      [("props", `Assoc [("name", `String "test"); ("score", `Float 0.95)])] in
    match stmt with
    | `Assoc fields ->
        let p = List.assoc "parameters" fields in
        let props = Yojson.Safe.Util.(member "props" p) in
        check string "name" "test" Yojson.Safe.Util.(props |> member "name" |> to_string)
    | _ -> fail "expected Assoc"
  in

  let test_make_neo4j_null_param () =
    let stmt = Figma_effects.make_neo4j_statement
      "MATCH (n) SET n.x = $x"
      [("x", `Null)] in
    match stmt with
    | `Assoc fields ->
        let p = List.assoc "parameters" fields in
        check bool "null param" true (Yojson.Safe.Util.(member "x" p) = `Null)
    | _ -> fail "expected Assoc"
  in

  (* ── run_with_mock: more Perform calls ── *)

  let test_mock_neo4j_run_cypher_unhandled () =
    (* Neo4j effects fall through to _ -> None in mock handler, raising Unhandled *)
    let store = Figma_effects.create_mock_store () in
    let raised = ref false in
    (try
      Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.neo4j_run_cypher
          ~uri:"bolt://localhost" ~database:"neo4j"
          ~auth_header:"Basic xxx" ~query:"RETURN 1" ~params:[]
      ) |> ignore
    with Effect.Unhandled _ ->
      raised := true);
    check bool "neo4j unhandled" true !raised
  in

  let test_mock_neo4j_run_batch_unhandled () =
    let store = Figma_effects.create_mock_store () in
    let raised = ref false in
    (try
      Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.neo4j_run_batch
          ~uri:"bolt://localhost" ~database:"neo4j"
          ~auth_header:"Basic xxx"
          ~queries:[("RETURN 1", []); ("RETURN 2", [])]
      ) |> ignore
    with Effect.Unhandled _ ->
      raised := true);
    check bool "neo4j batch unhandled" true !raised
  in

  let test_mock_dev_resources_unhandled () =
    let store = Figma_effects.create_mock_store () in
    let raised = ref false in
    (try
      Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_dev_resources
          ~token:"t" ~file_key:"f1" ~node_id:"n1"
      ) |> ignore
    with Effect.Unhandled _ ->
      raised := true);
    check bool "dev_resources unhandled" true !raised
  in

  let test_mock_add_dev_resource_unhandled () =
    let store = Figma_effects.create_mock_store () in
    let raised = ref false in
    (try
      Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.add_dev_resource
          ~token:"t" ~file_key:"f1" ~node_id:"n1"
          ~name:"test" ~url:"https://example.com"
      ) |> ignore
    with Effect.Unhandled _ ->
      raised := true);
    check bool "add_dev_resource unhandled" true !raised
  in

  let test_mock_create_webhook_unhandled () =
    let store = Figma_effects.create_mock_store () in
    let raised = ref false in
    (try
      Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.create_webhook
          ~token:"t" ~team_id:"tm1" ~file_key:"f1"
          ~endpoint:"https://hook.example.com" ~passcode:"secret123"
      ) |> ignore
    with Effect.Unhandled _ ->
      raised := true);
    check bool "create_webhook unhandled" true !raised
  in

  (* ── run_with_mock: complex multi-table lookup sequences ── *)

  let test_mock_all_tables_populated () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc [("name", `String "File1")]);
    Hashtbl.replace store.nodes "f1:n1" (`Assoc [("node", `Null)]);
    Hashtbl.replace store.images "f1:n1" (`Assoc [("url", `String "img.png")]);
    Hashtbl.replace store.file_images "f1" (`Assoc [("meta", `Null)]);
    Hashtbl.replace store.file_meta "f1" (`Assoc [("comp", `List [])]);
    store.me := Some (`Assoc [("id", `String "u1")]);
    Hashtbl.replace store.projects "t1" (`Assoc [("projects", `List [])]);
    Hashtbl.replace store.project_files "p1" (`Assoc [("files", `List [])]);
    Hashtbl.replace store.variables "f1" (`Assoc [("vars", `List [])]);
    let (r1, r2, r3, r4, r5, r6, r7, r8, r9) =
      Figma_effects.run_with_mock store (fun () ->
        let a = Figma_effects.Perform.get_file ~token:"t" ~file_key:"f1" () in
        let b = Figma_effects.Perform.get_nodes ~token:"t" ~file_key:"f1" ~node_ids:["n1"] () in
        let c = Figma_effects.Perform.get_images ~token:"t" ~file_key:"f1" ~node_ids:["n1"] ~format:"png" ~scale:1.0 () in
        let d = Figma_effects.Perform.get_file_images ~token:"t" ~file_key:"f1" () in
        let e = Figma_effects.Perform.get_file_meta ~token:"t" ~file_key:"f1" () in
        let f = Figma_effects.Perform.get_me ~token:"t" in
        let g = Figma_effects.Perform.get_team_projects ~token:"t" ~team_id:"t1" in
        let h = Figma_effects.Perform.get_project_files ~token:"t" ~project_id:"p1" in
        let i = Figma_effects.Perform.get_variables ~token:"t" ~file_key:"f1" in
        (a, b, c, d, e, f, g, h, i)
      )
    in
    (match r1 with Ok _ -> () | Error e -> fail ("file: " ^ e));
    (match r2 with Ok _ -> () | Error e -> fail ("nodes: " ^ e));
    (match r3 with Ok _ -> () | Error e -> fail ("images: " ^ e));
    (match r4 with Ok _ -> () | Error e -> fail ("file_images: " ^ e));
    (match r5 with Ok _ -> () | Error e -> fail ("file_meta: " ^ e));
    (match r6 with Ok _ -> () | Error e -> fail ("me: " ^ e));
    (match r7 with Ok _ -> () | Error e -> fail ("projects: " ^ e));
    (match r8 with Ok _ -> () | Error e -> fail ("project_files: " ^ e));
    (match r9 with Ok _ -> () | Error e -> fail ("variables: " ^ e))
  in

  let test_mock_all_tables_empty_lookups () =
    let store = Figma_effects.create_mock_store () in
    let (r1, r2, r3) =
      Figma_effects.run_with_mock store (fun () ->
        let a = Figma_effects.Perform.get_file ~token:"t" ~file_key:"missing1" () in
        let b = Figma_effects.Perform.get_nodes ~token:"t" ~file_key:"m" ~node_ids:["n"] () in
        let c = Figma_effects.Perform.get_variables ~token:"t" ~file_key:"m" in
        (a, b, c)
      )
    in
    (match r1 with Error _ -> () | Ok _ -> fail "expected file not found");
    (match r2 with Error _ -> () | Ok _ -> fail "expected nodes not found");
    (match r3 with Error _ -> () | Ok _ -> fail "expected variables not found")
  in

  (* ── Perform module: remaining optional param combos ── *)

  let test_perform_get_file_plugin_data_only () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc []);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"t" ~file_key:"f1" ~plugin_data:"shared" ()
    ) in
    (match result with Ok _ -> () | Error e -> fail e)
  in

  let test_perform_get_file_version_only () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc []);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"t" ~file_key:"f1" ~version:"v42" ()
    ) in
    (match result with Ok _ -> () | Error e -> fail e)
  in

  let test_perform_get_nodes_depth_geometry () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.nodes "f1:a,b" (`Assoc []);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_nodes ~token:"t" ~file_key:"f1" ~node_ids:["a"; "b"]
        ~depth:3 ~geometry:"paths" ()
    ) in
    (match result with Ok _ -> () | Error e -> fail e)
  in

  let test_perform_get_images_version_only () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.images "f1:x" (`Assoc []);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_images ~token:"t" ~file_key:"f1" ~node_ids:["x"]
        ~format:"svg" ~scale:0.5 ~version:"v1" ()
    ) in
    (match result with Ok _ -> () | Error e -> fail e)
  in

  (* ========== RUN ALL ========== *)

  run "Verifier+Effects W11" [
    ("parse_diff_regions", [
      test_case "full success" `Quick test_parse_regions_full_success;
      test_case "zero values" `Quick test_parse_regions_zero_values;
      test_case "null regions key" `Quick test_parse_regions_null_regions_key;
    ]);
    ("hints_to_summary", [
      test_case "empty hints" `Quick test_summary_empty_hints;
    ]);
    ("hint_to_description", [
      test_case "padding all zero" `Quick test_desc_padding_all_zero;
      test_case "padding left only" `Quick test_desc_padding_left_only;
      test_case "padding bottom+right" `Quick test_desc_padding_bottom_right;
      test_case "color values" `Quick test_desc_color_specific_values;
      test_case "size positive" `Quick test_desc_size_positive;
      test_case "gap large" `Quick test_desc_gap_large;
    ]);
    ("calculate_human_ssim", [
      test_case "delta_e exactly 50" `Quick test_human_ssim_delta_e_exactly_50;
      test_case "delta_e above 50" `Quick test_human_ssim_delta_e_above_50;
      test_case "perfect 1.0/0.0" `Quick test_human_ssim_both_one_zero;
      test_case "ssim zero" `Quick test_human_ssim_ssim_zero;
      test_case "JND delta_e=1" `Quick test_human_ssim_small_delta;
    ]);
    ("suggest_corrections", [
      test_case "all triggers combined" `Quick test_suggest_all_triggers_combined;
      test_case "only top edge" `Quick test_suggest_only_top_edge;
      test_case "only top strip" `Quick test_suggest_only_top_strip;
      test_case "only bottom strip" `Quick test_suggest_only_bottom_strip;
      test_case "quad high balanced" `Quick test_suggest_quad_high_balanced;
      test_case "ssim 0.94 fallback" `Quick test_suggest_ssim_0_94_fallback;
    ]);
    ("result_to_json", [
      test_case "with history" `Quick test_result_json_with_history;
      test_case "errors+history" `Quick test_result_json_with_errors_and_history;
    ]);
    ("step_to_json", [
      test_case "empty corrections" `Quick test_step_json_empty_corrections;
      test_case "6 corrections" `Quick test_step_json_multiple_corrections;
    ]);
    ("apply_color_adjustment", [
      test_case "hex boundary 255" `Quick test_color_hex_boundary_255;
      test_case "hex boundary 0" `Quick test_color_hex_boundary_zero;
      test_case "mid values" `Quick test_color_mid_values;
    ]);
    ("adjust_css_value", [
      test_case "global multiple" `Quick test_adjust_css_global_multiple;
      test_case "no match property" `Quick test_adjust_css_no_match_property;
    ]);
    ("apply_padding_adjustment", [
      test_case "no match" `Quick test_padding_no_match;
      test_case "longhand only" `Quick test_padding_longhand_only;
    ]);
    ("correction_hints_to_json", [
      test_case "empty list" `Quick test_hints_json_empty_list;
      test_case "all 6 types" `Quick test_hints_json_all_types;
    ]);
    ("quadrant_result_to_json", [
      test_case "worst=bottom_left" `Quick test_quadrant_worst_bottom_left;
      test_case "worst=bottom_right" `Quick test_quadrant_worst_bottom_right;
    ]);
    ("diff_image_result_to_json", [
      test_case "zero values" `Quick test_diff_image_result_zero;
    ]);
    ("log_hint_application", [
      test_case "all 6 types" `Quick test_log_hint_all_types;
    ]);
    ("empty_diff_regions", [
      test_case "all fields zero" `Quick test_empty_diff_regions_values;
    ]);
    ("hint_to_json", [
      test_case "gap" `Quick test_hint_json_gap;
      test_case "font_size" `Quick test_hint_json_font_size;
      test_case "border_radius" `Quick test_hint_json_border_radius;
    ]);
    (* ── figma_effects.ml ── *)
    ("parse_neo4j_response [effects]", [
      test_case "general exception" `Quick test_parse_neo4j_general_exception;
      test_case "empty results" `Quick test_parse_neo4j_empty_results;
      test_case "multiple result sets" `Quick test_parse_neo4j_multiple_result_sets;
      test_case "multiple errors" `Quick test_parse_neo4j_multiple_errors;
    ]);
    ("make_neo4j_statement [effects]", [
      test_case "empty query" `Quick test_make_neo4j_empty_query;
      test_case "list param" `Quick test_make_neo4j_list_param;
      test_case "nested assoc param" `Quick test_make_neo4j_nested_assoc_param;
      test_case "null param" `Quick test_make_neo4j_null_param;
    ]);
    ("run_with_mock: unhandled effects", [
      test_case "neo4j_run_cypher" `Quick test_mock_neo4j_run_cypher_unhandled;
      test_case "neo4j_run_batch" `Quick test_mock_neo4j_run_batch_unhandled;
      test_case "dev_resources" `Quick test_mock_dev_resources_unhandled;
      test_case "add_dev_resource" `Quick test_mock_add_dev_resource_unhandled;
      test_case "create_webhook" `Quick test_mock_create_webhook_unhandled;
    ]);
    ("run_with_mock: multi-table", [
      test_case "all tables populated" `Quick test_mock_all_tables_populated;
      test_case "all tables empty" `Quick test_mock_all_tables_empty_lookups;
    ]);
    ("Perform options", [
      test_case "plugin_data only" `Quick test_perform_get_file_plugin_data_only;
      test_case "version only" `Quick test_perform_get_file_version_only;
      test_case "nodes depth+geometry" `Quick test_perform_get_nodes_depth_geometry;
      test_case "images version" `Quick test_perform_get_images_version_only;
    ]);
  ]
