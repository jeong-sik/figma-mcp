(** Extra Coverage Tests for uncovered pure functions in figma_codegen.ml *)

open Alcotest
open Figma_types
open Figma_codegen

(** ============== Test Fixtures ============== *)

let make_rgba ?(a=1.0) r g b = { r; g; b; a }

let make_fx ?(visible=true) fx_type =
  { fx_type; visible; radius = 4.; color = None; offset = None; spread = None }

(** ============== 1. Gradient to CSS Tests ============== *)

let test_gradient_to_css_empty () =
  let result = gradient_to_css [] in
  check bool "empty gradient returns None" true (Option.is_none result)

let test_gradient_to_css_simple () =
  let stops = [
    (0.0, make_rgba 1.0 0.0 0.0);
    (1.0, make_rgba 0.0 0.0 1.0)
  ] in
  let result = gradient_to_css stops in
  check bool "returns Some" true (Option.is_some result);
  let css = Option.get result in
  check bool "starts with linear-gradient" true (String.sub css 0 15 = "linear-gradient");
  check bool "contains #FF0000" true
    (String.length (Str.global_replace (Str.regexp "#FF0000") "" css) < String.length css);
  check bool "contains #0000FF" true
    (String.length (Str.global_replace (Str.regexp "#0000FF") "" css) < String.length css)

let test_gradient_to_css_precise () =
  let stops = [
    (0.0, make_rgba 0.5 0.5 0.5);
    (1.0, make_rgba 0.0 0.0 0.0)
  ] in
  let result = gradient_to_css ~precise:true stops in
  check bool "returns Some" true (Option.is_some result);
  let css = Option.get result in
  check bool "uses rgb() format" true
    (String.length (Str.global_replace (Str.regexp "rgb(") "" css) < String.length css)

let test_gradient_to_css_with_alpha () =
  let stops = [
    (0.0, make_rgba ~a:0.5 1.0 0.0 0.0);
    (0.5, make_rgba ~a:0.8 0.0 1.0 0.0);
    (1.0, make_rgba ~a:1.0 0.0 0.0 1.0)
  ] in
  let result = gradient_to_css stops in
  check bool "returns Some" true (Option.is_some result);
  let css = Option.get result in
  check bool "contains gradient" true (String.length css > 20)

let gradient_tests = [
  "gradient empty", `Quick, test_gradient_to_css_empty;
  "gradient simple", `Quick, test_gradient_to_css_simple;
  "gradient precise mode", `Quick, test_gradient_to_css_precise;
  "gradient with alpha", `Quick, test_gradient_to_css_with_alpha;
]

(** ============== 2. Effects to CSS Tests ============== *)

let test_effects_to_css_empty () =
  let result = effects_to_css [] in
  check string "empty effects" "" result

let test_effects_to_css_drop_shadow () =
  let fx = { (make_fx DropShadow) with
    offset = Some (4., 4.);
    radius = 8.;
    spread = Some 2.;
    color = Some (make_rgba ~a:0.25 0.0 0.0 0.0)
  } in
  let result = effects_to_css [fx] in
  check bool "contains box-shadow" true
    (String.length (Str.global_replace (Str.regexp "box-shadow") "" result) < String.length result);
  check bool "contains rgba" true
    (String.length (Str.global_replace (Str.regexp "rgba") "" result) < String.length result)

let test_effects_to_css_inner_shadow () =
  let fx = { (make_fx InnerShadow) with
    offset = Some (2., 2.);
    radius = 4.;
    spread = Some 1.;
    color = Some (make_rgba ~a:0.3 0.0 0.0 0.0)
  } in
  let result = effects_to_css [fx] in
  check bool "contains inset" true
    (String.length (Str.global_replace (Str.regexp "inset") "" result) < String.length result)

let test_effects_to_css_layer_blur () =
  let fx = { (make_fx LayerBlur) with radius = 10. } in
  let result = effects_to_css [fx] in
  check bool "contains filter:blur" true
    (String.length (Str.global_replace (Str.regexp "filter:blur") "" result) < String.length result)

let test_effects_to_css_background_blur () =
  let fx = { (make_fx BackgroundBlur) with radius = 20. } in
  let result = effects_to_css [fx] in
  check bool "contains backdrop-filter" true
    (String.length (Str.global_replace (Str.regexp "backdrop-filter") "" result) < String.length result)

let test_effects_to_css_multiple () =
  let fx1 = { (make_fx DropShadow) with offset = Some (2., 2.); radius = 4. } in
  let fx2 = { (make_fx LayerBlur) with radius = 5. } in
  let result = effects_to_css [fx1; fx2] in
  check bool "contains box-shadow and filter" true
    ((String.length (Str.global_replace (Str.regexp "box-shadow") "" result) < String.length result) &&
     (String.length (Str.global_replace (Str.regexp "filter") "" result) < String.length result))

let test_effects_to_css_invisible () =
  let fx = { (make_fx ~visible:false DropShadow) with offset = Some (4., 4.) } in
  let result = effects_to_css [fx] in
  check string "invisible effect ignored" "" result

let test_effects_to_css_precise () =
  let fx = { (make_fx DropShadow) with
    offset = Some (3., 3.);
    radius = 6.;
    color = Some (make_rgba ~a:0.333 0.5 0.5 0.5)
  } in
  let result = effects_to_css ~precise:true [fx] in
  check bool "precise uses rgba with 2 decimals" true
    (String.length (Str.global_replace (Str.regexp "rgba([0-9]+,[0-9]+,[0-9]+,0\\.33)") "" result) < String.length result)

let effects_css_tests = [
  "effects empty", `Quick, test_effects_to_css_empty;
  "effects drop shadow", `Quick, test_effects_to_css_drop_shadow;
  "effects inner shadow", `Quick, test_effects_to_css_inner_shadow;
  "effects layer blur", `Quick, test_effects_to_css_layer_blur;
  "effects background blur", `Quick, test_effects_to_css_background_blur;
  "effects multiple", `Quick, test_effects_to_css_multiple;
  "effects invisible", `Quick, test_effects_to_css_invisible;
  "effects precise mode", `Quick, test_effects_to_css_precise;
]

(** ============== 3. JSON Collection Tests ============== *)

let test_json_collect_image_refs_nested () =
  let json = `Assoc [
    ("children", `List [
      `Assoc [
        ("fills", `List [
          `Assoc [("imageRef", `String "img:abc123")]
        ])
      ];
      `Assoc [
        ("fills", `List [
          `Assoc [("imageRef", `String "img:def456")]
        ])
      ]
    ])
  ] in
  let refs = json_collect_image_refs json in
  check int "2 refs" 2 (List.length refs);
  check bool "contains img:abc123" true (List.mem "img:abc123" refs);
  check bool "contains img:def456" true (List.mem "img:def456" refs)

let test_json_collect_image_refs_duplicates () =
  let json = `Assoc [
    ("fills", `List [
      `Assoc [("imageRef", `String "img:same")];
      `Assoc [("imageRef", `String "img:same")];
      `Assoc [("imageRef", `String "img:other")]
    ])
  ] in
  let refs = json_collect_image_refs json in
  (* Duplicates are not deduplicated in collect, but sort_uniq is applied in fidelity_node *)
  check bool "contains refs" true (List.length refs >= 2)

let test_json_collect_image_refs_no_images () =
  let json = `Assoc [
    ("fills", `List [
      `Assoc [("color", `String "#FF0000")]
    ])
  ] in
  let refs = json_collect_image_refs json in
  check int "no refs" 0 (List.length refs)

let test_json_collect_image_refs_non_string () =
  let json = `Assoc [
    ("fills", `List [
      `Assoc [("imageRef", `Int 123)]
    ])
  ] in
  let refs = json_collect_image_refs json in
  check int "no refs from non-string" 0 (List.length refs)

let json_collection_tests = [
  "collect nested refs", `Quick, test_json_collect_image_refs_nested;
  "collect duplicates", `Quick, test_json_collect_image_refs_duplicates;
  "collect no images", `Quick, test_json_collect_image_refs_no_images;
  "collect non-string imageRef", `Quick, test_json_collect_image_refs_non_string;
]

(** ============== 4. Extract Root Node Tests ============== *)

let test_extract_root_node_document () =
  let json = `Assoc [
    ("document", `Assoc [
      ("type", `String "DOCUMENT");
      ("children", `List [])
    ])
  ] in
  let root = extract_root_node json in
  match json_member "type" root with
  | Some (`String t) -> check string "type is DOCUMENT" "DOCUMENT" t
  | _ -> fail "expected DOCUMENT type"

let test_extract_root_node_nodes () =
  let json = `Assoc [
    ("nodes", `Assoc [
      ("1:2", `Assoc [
        ("document", `Assoc [
          ("type", `String "FRAME");
          ("name", `String "TestFrame")
        ])
      ])
    ])
  ] in
  let root = extract_root_node json in
  match json_member "type" root with
  | Some (`String t) -> check string "type is FRAME" "FRAME" t
  | _ -> fail "expected FRAME type"

let test_extract_root_node_fallback () =
  let json = `Assoc [
    ("type", `String "CANVAS");
    ("name", `String "Canvas1")
  ] in
  let root = extract_root_node json in
  check bool "returns json itself" true (root = json)

let extract_root_tests = [
  "extract document key", `Quick, test_extract_root_node_document;
  "extract from nodes", `Quick, test_extract_root_node_nodes;
  "extract fallback", `Quick, test_extract_root_node_fallback;
]

(** ============== 5. Extract Screens Tests ============== *)

let make_node ?(id="node-1") ?(name="TestNode") ?(node_type=Frame)
    ?(visible=true) ?(children=[]) () =
  { default_node with id; name; node_type; visible; children }

let test_extract_screens_canvas () =
  let screen1 = make_node ~id:"s1" ~name:"Screen1" ~node_type:Frame () in
  let screen2 = make_node ~id:"s2" ~name:"Screen2" ~node_type:Frame () in
  let hidden = make_node ~id:"s3" ~name:"Hidden" ~node_type:Frame ~visible:false () in
  let canvas = make_node ~node_type:Canvas ~children:[screen1; screen2; hidden] () in
  let screens = extract_screens canvas in
  check int "2 visible screens" 2 (List.length screens);
  check bool "no hidden screen" true (not (List.exists (fun (n, _) -> n = "Hidden") screens))

let test_extract_screens_document_type () =
  let frame1 = make_node ~id:"f1" ~name:"Frame1" ~node_type:Frame () in
  let frame2 = make_node ~id:"f2" ~name:"Frame2" ~node_type:Frame () in
  let doc = make_node ~node_type:Document ~children:[frame1; frame2] () in
  let screens = extract_screens doc in
  check int "2 frames" 2 (List.length screens)

let test_extract_screens_non_container () =
  let node = make_node ~id:"n1" ~name:"Single" ~node_type:Frame () in
  let screens = extract_screens node in
  check int "1 item" 1 (List.length screens);
  check string "name is Single" "Single" (fst (List.hd screens))

let extract_screens_tests = [
  "extract from Canvas", `Quick, test_extract_screens_canvas;
  "extract from Document", `Quick, test_extract_screens_document_type;
  "extract non-container", `Quick, test_extract_screens_non_container;
]

(** ============== 6. Split to Components Tests ============== *)

let test_split_to_components_document () =
  let comp1 = make_node ~name:"Header" () in
  let comp2 = make_node ~name:"Footer" () in
  let doc = make_node ~node_type:Document ~children:[comp1; comp2] () in
  let result = split_to_components doc in
  check bool "contains ## 1. Header" true
    (String.length (Str.global_replace (Str.regexp "## 1\\. Header") "" result) < String.length result);
  check bool "contains ## 2. Footer" true
    (String.length (Str.global_replace (Str.regexp "## 2\\. Footer") "" result) < String.length result)

let test_split_to_components_frame () =
  let child1 = make_node ~name:"Child1" () in
  let child2 = make_node ~name:"Child2" () in
  let frame = make_node ~node_type:Frame ~children:[child1; child2] () in
  let result = split_to_components frame in
  check bool "contains 2 components" true
    ((String.length (Str.global_replace (Str.regexp "## 1\\.") "" result) < String.length result) &&
     (String.length (Str.global_replace (Str.regexp "## 2\\.") "" result) < String.length result))

let test_split_to_components_single () =
  (* Non-Frame/Document/Canvas types hit the [node] fallback branch *)
  let node = make_node ~name:"Single" ~node_type:Group () in
  let result = split_to_components node in
  check bool "contains Single" true
    (String.length (Str.global_replace (Str.regexp "Single") "" result) < String.length result)

let test_split_to_components_filters_hidden () =
  let visible = make_node ~name:"Visible" () in
  let hidden = make_node ~name:"Hidden" ~visible:false () in
  let doc = make_node ~node_type:Document ~children:[visible; hidden] () in
  let result = split_to_components doc in
  check bool "contains Visible" true
    (String.length (Str.global_replace (Str.regexp "Visible") "" result) < String.length result);
  check bool "no Hidden" true
    (String.length (Str.global_replace (Str.regexp "Hidden") "" result) = String.length result)

let split_components_tests = [
  "split document", `Quick, test_split_to_components_document;
  "split frame", `Quick, test_split_to_components_frame;
  "split single", `Quick, test_split_to_components_single;
  "split filters hidden", `Quick, test_split_to_components_filters_hidden;
]

(** ============== 7. Analyze Compression Tests ============== *)

let test_analyze_compression_basic () =
  let node = make_node () in
  let original_json = "{\"type\":\"FRAME\",\"name\":\"Test\",\"visible\":true}" in
  let stats = analyze_compression node original_json in
  check bool "original > 0" true (stats.original_chars > 0);
  check bool "compact > 0" true (stats.compact_chars > 0);
  check bool "verbose > 0" true (stats.verbose_chars > 0);
  check bool "ratio is float" true (stats.compression_ratio >= 0.0)

let test_analyze_compression_ratio () =
  let node = make_node ~name:"TestNode" () in
  let original = String.make 1000 'x' in
  let stats = analyze_compression node original in
  check bool "compression_ratio calculation" true
    (stats.compression_ratio = 1.0 -. (float_of_int stats.compact_chars /. float_of_int stats.original_chars))

let analyze_compression_tests = [
  "analyze basic", `Quick, test_analyze_compression_basic;
  "analyze ratio", `Quick, test_analyze_compression_ratio;
]

(** ============== 8. Format Stats Tests ============== *)

let test_format_stats_basic () =
  let stats = {
    original_chars = 1000;
    compact_chars = 300;
    verbose_chars = 500;
    compression_ratio = 0.7;
  } in
  let result = format_stats stats in
  check bool "contains Original JSON: 1000" true
    (String.length (Str.global_replace (Str.regexp "Original JSON: 1000") "" result) < String.length result);
  check bool "contains Compact DSL:   300" true
    (String.length (Str.global_replace (Str.regexp "Compact DSL:   300") "" result) < String.length result);
  check bool "contains Token savings: 70.0%" true
    (String.length (Str.global_replace (Str.regexp "Token savings: 70\\.0%") "" result) < String.length result)

let test_format_stats_zero_compression () =
  let stats = {
    original_chars = 100;
    compact_chars = 100;
    verbose_chars = 100;
    compression_ratio = 0.0;
  } in
  let result = format_stats stats in
  check bool "contains Token savings: 0.0%" true
    (String.length (Str.global_replace (Str.regexp "Token savings: 0\\.0%") "" result) < String.length result)

let format_stats_tests = [
  "format stats basic", `Quick, test_format_stats_basic;
  "format stats zero", `Quick, test_format_stats_zero_compression;
]

(** ============== Run All Tests ============== *)

let () =
  run "Figma Codegen Extra Coverage" [
    "1. Gradient CSS", gradient_tests;
    "2. Effects CSS", effects_css_tests;
    "3. JSON Collection", json_collection_tests;
    "4. Extract Root", extract_root_tests;
    "5. Extract Screens", extract_screens_tests;
    "6. Split Components", split_components_tests;
    "7. Analyze Compression", analyze_compression_tests;
    "8. Format Stats", format_stats_tests;
  ]
