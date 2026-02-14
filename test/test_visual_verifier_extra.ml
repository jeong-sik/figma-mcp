(** Extra coverage tests for visual_verifier.ml — pure functions.
    Targets uncovered points: calculate_human_ssim, hint_to_json,
    hint_to_description, hints_to_summary, correction_hints_to_json,
    suggest_corrections, parse_diff_regions, empty_diff_regions *)

let () =
  let open Alcotest in
  let open Visual_verifier in

  (* ============== calculate_human_ssim ============== *)
  let test_human_ssim_perfect () =
    check (float 0.001) "perfect" 1.0 (calculate_human_ssim 1.0 0.0)
  in
  let test_human_ssim_with_delta_e () =
    (* delta_e=25 → penalty=0.5 → 0.95 * 0.5 = 0.475 *)
    check (float 0.001) "half penalty" 0.475 (calculate_human_ssim 0.95 25.0)
  in
  let test_human_ssim_high_delta_e () =
    (* delta_e >= 50 → penalty=1.0 → result=0.0 *)
    check (float 0.001) "max penalty" 0.0 (calculate_human_ssim 0.9 50.0)
  in
  let test_human_ssim_over_50_delta_e () =
    (* delta_e=100 → min(1.0, 100/50)=1.0 → penalty capped *)
    check (float 0.001) "capped" 0.0 (calculate_human_ssim 0.9 100.0)
  in
  let test_human_ssim_zero_ssim () =
    check (float 0.001) "zero ssim" 0.0 (calculate_human_ssim 0.0 10.0)
  in

  (* ============== empty_diff_regions ============== *)
  let test_empty_regions () =
    let r = empty_diff_regions in
    check (float 0.001) "tl" 0.0 r.quadrants.top_left;
    check (float 0.001) "tr" 0.0 r.quadrants.top_right;
    check (float 0.001) "bl" 0.0 r.quadrants.bottom_left;
    check (float 0.001) "br" 0.0 r.quadrants.bottom_right;
    check (float 0.001) "st" 0.0 r.strips.strip_top;
    check (float 0.001) "sm" 0.0 r.strips.strip_middle;
    check (float 0.001) "sb" 0.0 r.strips.strip_bottom;
    check (float 0.001) "et" 0.0 r.edges.edge_top;
    check (float 0.001) "eb" 0.0 r.edges.edge_bottom;
    check (float 0.001) "el" 0.0 r.edges.edge_left;
    check (float 0.001) "er" 0.0 r.edges.edge_right
  in

  (* ============== parse_diff_regions ============== *)
  let test_parse_regions_full () =
    let json = Yojson.Safe.from_string {|{
      "regions": {
        "quadrants": {"topLeft": 0.1, "topRight": 0.2, "bottomLeft": 0.3, "bottomRight": 0.4},
        "strips": {"top": 0.5, "middle": 0.6, "bottom": 0.7},
        "edges": {"top": 0.8, "bottom": 0.9, "left": 0.15, "right": 0.25}
      }
    }|} in
    let r = parse_diff_regions json in
    check (float 0.001) "tl" 0.1 r.quadrants.top_left;
    check (float 0.001) "br" 0.4 r.quadrants.bottom_right;
    check (float 0.001) "strip mid" 0.6 r.strips.strip_middle;
    check (float 0.001) "edge left" 0.15 r.edges.edge_left
  in
  let test_parse_regions_null () =
    let json = `Assoc [("regions", `Null)] in
    let r = parse_diff_regions json in
    check (float 0.001) "fallback tl" 0.0 r.quadrants.top_left
  in
  let test_parse_regions_missing () =
    let json = `Assoc [("other", `Int 1)] in
    let r = parse_diff_regions json in
    check (float 0.001) "fallback" 0.0 r.quadrants.top_left
  in
  let test_parse_regions_malformed () =
    let json = `Assoc [("regions", `String "bad")] in
    let r = parse_diff_regions json in
    check (float 0.001) "fallback on error" 0.0 r.quadrants.top_left
  in

  (* ============== hint_to_json ============== *)
  let test_hint_json_padding () =
    let j = hint_to_json (AdjustPadding (10., 5., 10., 5.)) in
    let typ = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "padding" typ;
    let top = Yojson.Safe.Util.(j |> member "top" |> to_float) in
    check (float 0.001) "top" 10.0 top
  in
  let test_hint_json_gap () =
    let j = hint_to_json (AdjustGap 8.0) in
    let typ = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "gap" typ;
    let v = Yojson.Safe.Util.(j |> member "value" |> to_float) in
    check (float 0.001) "value" 8.0 v
  in
  let test_hint_json_color () =
    let rgba = Figma_types.{ r = 1.0; g = 0.5; b = 0.0; a = 1.0 } in
    let j = hint_to_json (AdjustColor (".btn", rgba)) in
    let typ = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "color" typ;
    let sel = Yojson.Safe.Util.(j |> member "selector" |> to_string) in
    check string "selector" ".btn" sel
  in
  let test_hint_json_size () =
    let j = hint_to_json (AdjustSize (10., -5.)) in
    let typ = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "size" typ
  in
  let test_hint_json_font_size () =
    let j = hint_to_json (AdjustFontSize 2.0) in
    let typ = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "font_size" typ
  in
  let test_hint_json_border_radius () =
    let j = hint_to_json (AdjustBorderRadius 8.0) in
    let typ = Yojson.Safe.Util.(j |> member "type" |> to_string) in
    check string "type" "border_radius" typ
  in

  (* ============== hint_to_description ============== *)
  let test_desc_padding_all () =
    let d = hint_to_description (AdjustPadding (10., 5., 10., 5.)) in
    check bool "has padding" true (String.length d > 0);
    check bool "has 상단" true (
      try ignore (Str.search_forward (Str.regexp_string "\xec\x83\x81\xeb\x8b\xa8") d 0); true
      with Not_found -> false)
  in
  let test_desc_padding_zero () =
    let d = hint_to_description (AdjustPadding (0., 0., 0., 0.)) in
    check bool "fallback" true (String.length d > 0)
  in
  let test_desc_gap () =
    let d = hint_to_description (AdjustGap 8.0) in
    check bool "has gap" true (String.length d > 0)
  in
  let test_desc_color () =
    let rgba = Figma_types.{ r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
    let d = hint_to_description (AdjustColor ("div", rgba)) in
    check bool "has selector" true (
      try ignore (Str.search_forward (Str.regexp_string "div") d 0); true
      with Not_found -> false)
  in
  let test_desc_size () =
    let d = hint_to_description (AdjustSize (10., -5.)) in
    check bool "has size" true (String.length d > 0)
  in
  let test_desc_font_size () =
    let d = hint_to_description (AdjustFontSize 2.0) in
    check bool "has font" true (String.length d > 0)
  in
  let test_desc_border_radius () =
    let d = hint_to_description (AdjustBorderRadius 8.0) in
    check bool "has radius" true (String.length d > 0)
  in

  (* ============== hints_to_summary ============== *)
  let test_summary_empty () =
    let s = hints_to_summary [] in
    check bool "has check mark" true (
      try ignore (Str.search_forward (Str.regexp_string "\xe2\x9c\x85") s 0); true
      with Not_found -> false)
  in
  let test_summary_one () =
    let s = hints_to_summary [AdjustGap 1.0] in
    check bool "has count" true (
      try ignore (Str.search_forward (Str.regexp_string "1") s 0); true
      with Not_found -> false)
  in
  let test_summary_multiple () =
    let s = hints_to_summary [AdjustGap 1.0; AdjustSize (1., 1.)] in
    check bool "has 2" true (
      try ignore (Str.search_forward (Str.regexp_string "2") s 0); true
      with Not_found -> false)
  in

  (* ============== correction_hints_to_json ============== *)
  let test_hints_json_empty () =
    let j = correction_hints_to_json [] in
    check bool "empty list" true (j = `List [])
  in
  let test_hints_json_one () =
    let j = correction_hints_to_json [AdjustGap 5.0] in
    match j with
    | `List [_] -> ()
    | _ -> fail "expected list of 1"
  in

  (* ============== suggest_corrections ============== *)
  let test_suggest_perfect () =
    let hints = suggest_corrections ~ssim:0.99 ~diff_regions:empty_diff_regions in
    check int "no hints" 0 (List.length hints)
  in
  let test_suggest_edge_issue () =
    let regions = {
      empty_diff_regions with
      edges = { edge_top = 0.10; edge_bottom = 0.0; edge_left = 0.0; edge_right = 0.0 }
    } in
    let hints = suggest_corrections ~ssim:0.90 ~diff_regions:regions in
    check bool "has padding" true (List.exists (function AdjustPadding _ -> true | _ -> false) hints)
  in
  let test_suggest_strip_bottom () =
    let regions = {
      empty_diff_regions with
      strips = { strip_top = 0.0; strip_middle = 0.0; strip_bottom = 0.15 }
    } in
    let hints = suggest_corrections ~ssim:0.85 ~diff_regions:regions in
    check bool "has padding" true (List.exists (function AdjustPadding _ -> true | _ -> false) hints)
  in
  let test_suggest_strip_top () =
    let regions = {
      empty_diff_regions with
      strips = { strip_top = 0.15; strip_middle = 0.0; strip_bottom = 0.0 }
    } in
    let hints = suggest_corrections ~ssim:0.85 ~diff_regions:regions in
    check bool "has padding" true (List.exists (function AdjustPadding _ -> true | _ -> false) hints)
  in
  let test_suggest_quadrant_imbalance () =
    let regions = {
      empty_diff_regions with
      quadrants = { top_left = 0.15; top_right = 0.02; bottom_left = 0.0; bottom_right = 0.0 }
    } in
    let hints = suggest_corrections ~ssim:0.85 ~diff_regions:regions in
    check bool "has gap" true (List.exists (function AdjustGap _ -> true | _ -> false) hints);
    check bool "has size" true (List.exists (function AdjustSize _ -> true | _ -> false) hints)
  in
  let test_suggest_fallback_low () =
    (* ssim < 0.90, no region issues → fallback padding+gap *)
    let hints = suggest_corrections ~ssim:0.80 ~diff_regions:empty_diff_regions in
    check bool "has hints" true (List.length hints > 0);
    check bool "has padding" true (List.exists (function AdjustPadding _ -> true | _ -> false) hints);
    check bool "has gap" true (List.exists (function AdjustGap _ -> true | _ -> false) hints)
  in
  let test_suggest_fallback_mid () =
    (* ssim 0.90-0.95, no region issues → small padding *)
    let hints = suggest_corrections ~ssim:0.92 ~diff_regions:empty_diff_regions in
    check bool "has hints" true (List.length hints > 0)
  in
  let test_suggest_fallback_high () =
    (* ssim 0.95-0.99, no region issues → tiny padding *)
    let hints = suggest_corrections ~ssim:0.97 ~diff_regions:empty_diff_regions in
    check bool "has hints" true (List.length hints > 0)
  in
  let test_suggest_all_regions_active () =
    let regions = {
      quadrants = { top_left = 0.20; top_right = 0.05; bottom_left = 0.05; bottom_right = 0.05 };
      strips = { strip_top = 0.10; strip_middle = 0.02; strip_bottom = 0.10 };
      edges = { edge_top = 0.10; edge_bottom = 0.10; edge_left = 0.10; edge_right = 0.10 };
    } in
    let hints = suggest_corrections ~ssim:0.70 ~diff_regions:regions in
    check bool "multiple hints" true (List.length hints >= 3)
  in

  run "Visual_verifier Extra" [
    ("calculate_human_ssim", [
      test_case "perfect" `Quick test_human_ssim_perfect;
      test_case "with delta_e" `Quick test_human_ssim_with_delta_e;
      test_case "high delta_e" `Quick test_human_ssim_high_delta_e;
      test_case "over 50" `Quick test_human_ssim_over_50_delta_e;
      test_case "zero ssim" `Quick test_human_ssim_zero_ssim;
    ]);
    ("empty_diff_regions", [
      test_case "all zeros" `Quick test_empty_regions;
    ]);
    ("parse_diff_regions", [
      test_case "full" `Quick test_parse_regions_full;
      test_case "null" `Quick test_parse_regions_null;
      test_case "missing" `Quick test_parse_regions_missing;
      test_case "malformed" `Quick test_parse_regions_malformed;
    ]);
    ("hint_to_json", [
      test_case "padding" `Quick test_hint_json_padding;
      test_case "gap" `Quick test_hint_json_gap;
      test_case "color" `Quick test_hint_json_color;
      test_case "size" `Quick test_hint_json_size;
      test_case "font_size" `Quick test_hint_json_font_size;
      test_case "border_radius" `Quick test_hint_json_border_radius;
    ]);
    ("hint_to_description", [
      test_case "padding all" `Quick test_desc_padding_all;
      test_case "padding zero" `Quick test_desc_padding_zero;
      test_case "gap" `Quick test_desc_gap;
      test_case "color" `Quick test_desc_color;
      test_case "size" `Quick test_desc_size;
      test_case "font_size" `Quick test_desc_font_size;
      test_case "border_radius" `Quick test_desc_border_radius;
    ]);
    ("hints_to_summary", [
      test_case "empty" `Quick test_summary_empty;
      test_case "one" `Quick test_summary_one;
      test_case "multiple" `Quick test_summary_multiple;
    ]);
    ("correction_hints_to_json", [
      test_case "empty" `Quick test_hints_json_empty;
      test_case "one" `Quick test_hints_json_one;
    ]);
    ("suggest_corrections", [
      test_case "perfect" `Quick test_suggest_perfect;
      test_case "edge issue" `Quick test_suggest_edge_issue;
      test_case "strip bottom" `Quick test_suggest_strip_bottom;
      test_case "strip top" `Quick test_suggest_strip_top;
      test_case "quadrant imbalance" `Quick test_suggest_quadrant_imbalance;
      test_case "fallback low" `Quick test_suggest_fallback_low;
      test_case "fallback mid" `Quick test_suggest_fallback_mid;
      test_case "fallback high" `Quick test_suggest_fallback_high;
      test_case "all regions" `Quick test_suggest_all_regions_active;
    ]);
  ]
