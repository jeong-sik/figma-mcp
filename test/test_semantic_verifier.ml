(** Tests for semantic_verifier.ml

    These tests are pure (no Playwright/Node). We feed in a synthetic Figma node tree
    and synthetic HTML metrics to validate matching + coordinate normalization. *)

open Alcotest

open Figma_types

let solid (c: rgba) : paint =
  { paint_type = Solid;
    visible = true;
    opacity = 1.0;
    color = Some c;
    gradient_stops = [];
    image_ref = None;
    scale_mode = None;
  }

let mk_bbox x y w h = { x; y; width = w; height = h }

let mk_root ~bg ~radius ~bbox ~children : ui_node =
  { default_node with
    id = "root";
    name = "Root";
    node_type = Frame;
    bbox = Some bbox;
    fills = [solid bg];
    border_radius = radius;
    children;
  }

let mk_text ~text ~color ~bbox ~font_size ~font_weight : ui_node =
  let typo = { default_typography with font_size; font_weight } in
  { default_node with
    id = "text";
    name = "Text";
    node_type = Text;
    bbox = Some bbox;
    characters = Some text;
    typography = Some typo;
    fills = [solid color];
    children = [];
  }

let mk_metrics ~root_bbox ~bg ~radius ~text_bbox ~text ~font_size ~font_weight ~text_color : Html_metrics.t =
  { viewport_width = 300;
    viewport_height = 200;
    root = Some { bbox = root_bbox; background = Some bg; border_radius_px = Some radius };
    bg_nodes = [];
    text_nodes = [
      { text;
        bbox = text_bbox;
        font_size_px = Some font_size;
        font_weight = Some font_weight;
        font_family = None;
        color = Some text_color;
      }
    ];
  }

let test_semantic_pass_exact () =
  let bg = { r = 1.0; g = 1.0; b = 1.0; a = 1.0 } in
  let text_color = { r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
  let root_abs = mk_bbox 100. 200. 300. 200. in
  let text_abs = mk_bbox 120. 240. 100. 20. in
  let root = mk_root ~bg ~radius:8.0 ~bbox:root_abs ~children:[
    mk_text ~text:"Hello" ~color:text_color ~bbox:text_abs ~font_size:16.0 ~font_weight:700
  ] in

  let html_root = mk_bbox 0. 0. 300. 200. in
  let html_text = mk_bbox 20. 40. 100. 20. in
  let metrics = mk_metrics ~root_bbox:html_root ~bg ~radius:8.0
    ~text_bbox:html_text ~text:"Hello" ~font_size:16.0 ~font_weight:700 ~text_color
  in

  match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
  | Error e -> fail e
  | Ok r ->
      check bool "passed" true r.passed;
      check int "mismatches" 0 (List.length r.mismatches)

let test_semantic_bbox_mismatch_detected () =
  let bg = { r = 1.0; g = 1.0; b = 1.0; a = 1.0 } in
  let text_color = { r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
  let root_abs = mk_bbox 100. 200. 300. 200. in
  let text_abs = mk_bbox 120. 240. 100. 20. in
  let root = mk_root ~bg ~radius:8.0 ~bbox:root_abs ~children:[
    mk_text ~text:"Hello" ~color:text_color ~bbox:text_abs ~font_size:16.0 ~font_weight:700
  ] in

  let html_root = mk_bbox 0. 0. 300. 200. in
  (* x shifted +20px: should violate default 4px tolerance *)
  let html_text = mk_bbox 40. 40. 100. 20. in
  let metrics = mk_metrics ~root_bbox:html_root ~bg ~radius:8.0
    ~text_bbox:html_text ~text:"Hello" ~font_size:16.0 ~font_weight:700 ~text_color
  in

  let cfg = { Semantic_verifier.default_config with score_threshold = 0.99 } in
  match Semantic_verifier.verify ~config:cfg ~dsl_node:root ~html:metrics () with
  | Error e -> fail e
  | Ok r ->
      check bool "passed" false r.passed;
      check bool "has mismatches" true (List.length r.mismatches > 0);
      let has_bbox =
        List.exists (function Semantic_verifier.TextBBoxMismatch _ -> true | _ -> false) r.mismatches
      in
      check bool "has TextBBoxMismatch" true has_bbox

let test_semantic_missing_text_fails () =
  let bg = { r = 1.0; g = 1.0; b = 1.0; a = 1.0 } in
  let text_color = { r = 1.0; g = 0.0; b = 0.0; a = 1.0 } in
  let root_abs = mk_bbox 100. 200. 300. 200. in
  let text_abs = mk_bbox 120. 240. 100. 20. in
  let root = mk_root ~bg ~radius:8.0 ~bbox:root_abs ~children:[
    mk_text ~text:"Hello" ~color:text_color ~bbox:text_abs ~font_size:16.0 ~font_weight:700
  ] in

  let metrics : Html_metrics.t =
    { viewport_width = 300;
      viewport_height = 200;
      root = Some { bbox = mk_bbox 0. 0. 300. 200.; background = Some bg; border_radius_px = Some 8.0 };
      bg_nodes = [];
      text_nodes = [];
    }
  in

  match Semantic_verifier.verify ~dsl_node:root ~html:metrics () with
  | Error e -> fail e
  | Ok r ->
      check bool "passed" false r.passed;
      let has_missing =
        List.exists (function Semantic_verifier.MissingText _ -> true | _ -> false) r.mismatches
      in
      check bool "has MissingText" true has_missing

let () =
  run "semantic_verifier" [
    ( "semantic_verifier",
      [
        test_case "pass exact" `Quick test_semantic_pass_exact;
        test_case "bbox mismatch detected" `Quick test_semantic_bbox_mismatch_detected;
        test_case "missing text fails" `Quick test_semantic_missing_text_fails;
      ] );
  ]
