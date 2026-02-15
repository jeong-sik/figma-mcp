(** Extra Coverage Tests for uncovered pure functions in figma_codegen.ml *)

open Alcotest
open Figma_types
open Figma_codegen

(** ============== Test Fixtures ============== *)

let make_rgba ?(a=1.0) r g b = { r; g; b; a }

let make_fx ?(visible=true) fx_type =
  { fx_type; visible; radius = 4.; color = None; offset = None; spread = None }

let make_paint ?(visible=true) paint_type color =
  { paint_type; visible; opacity = 1.0; color; gradient_stops = [];
    image_ref = None; scale_mode = None }

let make_gradient_paint ?(visible=true) stops =
  { paint_type = GradientLinear; visible; opacity = 1.0; color = None;
    gradient_stops = stops; image_ref = None; scale_mode = None }

let mk ?(id="node-1") ?(name="TestNode") ?(node_type=Frame)
    ?(visible=true) ?(fills=[]) ?(strokes=[]) ?(children=[])
    ?(border_radius=0.) ?(padding=(0.,0.,0.,0.)) ?(gap=0.)
    ?(opacity=1.0) ?(layout_mode=None') ?(bbox=None)
    ?(typography=None) ?(characters=None) ?(stroke_weight=0.)
    ?(effects=[]) ?(rotation=0.) ?(border_radii=None)
    ?(primary_axis_align=Min) ?(counter_axis_align=Min)
    ?(layout_sizing_h=Fixed) ?(layout_sizing_v=Fixed) () =
  { default_node with
    id; name; node_type; visible; fills; strokes; children;
    border_radius; padding; gap; opacity; layout_mode; bbox;
    typography; characters; stroke_weight; effects; rotation;
    border_radii; primary_axis_align; counter_axis_align;
    layout_sizing_h; layout_sizing_v }

(** helper: check substring presence *)
let has_sub needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if String.sub haystack i nlen = needle then found := true
    done;
    !found

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

(** ============== 9. Verbose DSL Tests ============== *)

let test_verbose_text_with_typography () =
  let node = mk ~node_type:Text ~characters:(Some "Hello")
    ~typography:(Some { default_typography with
      font_size = 20.; font_weight = 700; text_align_h = Center })
    ~fills:[make_paint Solid (Some (make_rgba 1.0 0.0 0.0))]
    ~opacity:0.8 () in
  let result = node_to_verbose node in
  check bool "starts with T" true (String.sub result 0 1 = "T");
  check bool "has font size" true (has_sub "s:20" result);
  check bool "has font weight" true (has_sub "w:700" result);
  check bool "has align center" true (has_sub "a:c" result);
  check bool "has color" true (has_sub "c:#FF0000" result);
  check bool "has opacity" true (has_sub "op:0.8" result);
  check bool "has visible" true (has_sub "vis:true" result)

let test_verbose_text_no_typography () =
  let node = mk ~node_type:Text ~characters:(Some "Plain") () in
  let result = node_to_verbose node in
  check bool "starts with T" true (String.sub result 0 1 = "T");
  check bool "has vis" true (has_sub "vis:" result)

let test_verbose_text_no_fill_color () =
  let node = mk ~node_type:Text ~characters:(Some "NoColor")
    ~typography:(Some default_typography) () in
  let result = node_to_verbose node in
  check bool "has default color c:#000" true (has_sub "c:#000" result)

let test_verbose_frame_with_attrs () =
  let node = mk ~layout_mode:Horizontal
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 100. })
    ~fills:[make_paint Solid (Some (make_rgba 0.0 0.0 1.0))]
    ~border_radius:8.
    ~padding:(10., 20., 10., 20.)
    ~gap:12. ~opacity:0.9
    ~strokes:[make_paint Solid (Some (make_rgba 0.5 0.5 0.5))] () in
  let result = node_to_verbose node in
  check bool "starts with F(row" true (has_sub "F(row" result);
  check bool "has bg" true (has_sub "bg:#0000FF" result);
  check bool "has radius" true (has_sub "r:8" result);
  check bool "has padding" true (has_sub "p:10,20,10,20" result);
  check bool "has gap" true (has_sub "g:12" result);
  check bool "has opacity" true (has_sub "op:0.9" result);
  check bool "has border" true (has_sub "bd:" result)

let test_verbose_frame_no_bg () =
  let node = mk ~layout_mode:Vertical
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 200. }) () in
  let result = node_to_verbose node in
  check bool "has bg:none" true (has_sub "bg:none" result);
  check bool "has col" true (has_sub "F(col" result)

let test_verbose_frame_no_bbox () =
  let node = mk ~layout_mode:Horizontal () in
  let result = node_to_verbose node in
  check bool "has size 0,0" true (has_sub ",0,0)" result)

let test_verbose_frame_with_children () =
  let child = mk ~node_type:Text ~characters:(Some "child") () in
  let node = mk ~layout_mode:Horizontal ~children:[child]
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let result = node_to_verbose node in
  check bool "has braces" true (has_sub "{" result && has_sub "}" result)

let test_verbose_other_types_fallback () =
  (* SemButton, SemInput etc. fall back to node_to_compact in verbose *)
  let node = mk ~name:"button_test" ~node_type:Instance () in
  let result = node_to_verbose node in
  check bool "falls back to compact (B prefix)" true (String.sub result 0 1 = "B")

let verbose_tests = [
  "verbose text with typo", `Quick, test_verbose_text_with_typography;
  "verbose text no typo", `Quick, test_verbose_text_no_typography;
  "verbose text no fill", `Quick, test_verbose_text_no_fill_color;
  "verbose frame attrs", `Quick, test_verbose_frame_with_attrs;
  "verbose frame no bg", `Quick, test_verbose_frame_no_bg;
  "verbose frame no bbox", `Quick, test_verbose_frame_no_bbox;
  "verbose frame children", `Quick, test_verbose_frame_with_children;
  "verbose other fallback", `Quick, test_verbose_other_types_fallback;
]

(** ============== 10. Classify Node Extended Tests ============== *)

let test_classify_line_as_icon () =
  let node = mk ~node_type:Line
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 50. }) () in
  check bool "Line with normal size -> SemIcon" true (classify_node node = SemIcon)

let test_classify_ellipse_as_icon () =
  let node = mk ~node_type:Ellipse
    ~bbox:(Some { x = 0.; y = 0.; width = 40.; height = 40. }) () in
  check bool "Ellipse -> SemIcon" true (classify_node node = SemIcon)

let test_classify_star_as_icon () =
  let node = mk ~node_type:Star
    ~bbox:(Some { x = 0.; y = 0.; width = 20.; height = 20. }) () in
  check bool "Star -> SemIcon" true (classify_node node = SemIcon)

let test_classify_regular_polygon () =
  let node = mk ~node_type:RegularPolygon
    ~bbox:(Some { x = 0.; y = 0.; width = 30.; height = 30. }) () in
  check bool "RegularPolygon -> SemIcon" true (classify_node node = SemIcon)

let test_classify_thin_width_divider () =
  (* Vertical thin line = divider *)
  let node = mk ~node_type:Rectangle
    ~bbox:(Some { x = 0.; y = 0.; width = 1.; height = 200. }) () in
  check bool "thin width -> SemDivider" true (classify_node node = SemDivider)

let test_classify_component_field () =
  let node = mk ~node_type:Component ~name:"Field_email" () in
  check bool "field prefix -> SemInput" true (classify_node node = SemInput)

let test_classify_component_textfield () =
  let node = mk ~node_type:Component ~name:"textfield_search" () in
  check bool "textfield prefix -> SemInput" true (classify_node node = SemInput)

let test_classify_component_ic () =
  let node = mk ~node_type:Component ~name:"ic_home" () in
  check bool "ic_ prefix -> SemIcon" true (classify_node node = SemIcon)

let test_classify_component_photo () =
  let node = mk ~node_type:Instance ~name:"photo_profile" () in
  check bool "photo prefix -> SemImage" true (classify_node node = SemImage)

let test_classify_instance_default () =
  let node = mk ~node_type:Instance ~name:"SomeWidget" () in
  check bool "instance default -> SemFrame" true (classify_node node = SemFrame)

let test_classify_frame_input () =
  let node = mk ~node_type:Frame ~name:"input_email" () in
  check bool "frame input -> SemInput" true (classify_node node = SemInput)

let test_classify_frame_field () =
  let node = mk ~node_type:Frame ~name:"field_name" () in
  check bool "frame field -> SemInput" true (classify_node node = SemInput)

let test_classify_frame_image () =
  let node = mk ~node_type:Frame ~name:"image_banner" () in
  check bool "frame image -> SemImage" true (classify_node node = SemImage)

let test_classify_frame_img () =
  let node = mk ~node_type:Frame ~name:"img_avatar" () in
  check bool "frame img -> SemImage" true (classify_node node = SemImage)

let test_classify_section () =
  let node = mk ~node_type:Section ~name:"Section1" () in
  check bool "Section -> SemFrame" true (classify_node node = SemFrame)

let test_classify_section_button () =
  let node = mk ~node_type:Section ~name:"button_section" () in
  check bool "Section btn -> SemButton" true (classify_node node = SemButton)

let test_classify_unknown () =
  let node = mk ~node_type:(Unknown "WIDGET") ~name:"SomeWidget" () in
  check bool "Unknown -> SemFrame" true (classify_node node = SemFrame)

let test_classify_boolean_op () =
  let node = mk ~node_type:BooleanOperation
    ~bbox:(Some { x = 0.; y = 0.; width = 24.; height = 24. }) () in
  check bool "BooleanOp -> SemFrame (wildcard)" true (classify_node node = SemFrame)

let classify_extended_tests = [
  "Line -> SemIcon", `Quick, test_classify_line_as_icon;
  "Ellipse -> SemIcon", `Quick, test_classify_ellipse_as_icon;
  "Star -> SemIcon", `Quick, test_classify_star_as_icon;
  "RegularPolygon -> SemIcon", `Quick, test_classify_regular_polygon;
  "thin width divider", `Quick, test_classify_thin_width_divider;
  "Component field", `Quick, test_classify_component_field;
  "Component textfield", `Quick, test_classify_component_textfield;
  "Component ic_", `Quick, test_classify_component_ic;
  "Instance photo", `Quick, test_classify_component_photo;
  "Instance default", `Quick, test_classify_instance_default;
  "Frame input", `Quick, test_classify_frame_input;
  "Frame field", `Quick, test_classify_frame_field;
  "Frame image", `Quick, test_classify_frame_image;
  "Frame img", `Quick, test_classify_frame_img;
  "Section default", `Quick, test_classify_section;
  "Section button", `Quick, test_classify_section_button;
  "Unknown type", `Quick, test_classify_unknown;
  "BooleanOp wildcard", `Quick, test_classify_boolean_op;
]

(** ============== 11. Typography CSS Extended Tests ============== *)

let test_typography_letter_spacing () =
  let typo = { default_typography with letter_spacing = Some 1.5 } in
  let css = typography_to_css typo in
  check bool "has letter-spacing" true (has_sub "letter-spacing:" css)

let test_typography_letter_spacing_zero () =
  let typo = { default_typography with letter_spacing = Some 0. } in
  let css = typography_to_css typo in
  check bool "zero ls omitted" false (has_sub "letter-spacing:" css)

let test_typography_justified () =
  let typo = { default_typography with text_align_h = Justified } in
  let css = typography_to_css typo in
  check bool "has text-align:justify" true (has_sub "text-align:justify" css)

let test_typography_right_align () =
  let typo = { default_typography with text_align_h = Right } in
  let css = typography_to_css typo in
  check bool "has text-align:right" true (has_sub "text-align:right" css)

let test_typography_strikethrough () =
  let typo = { default_typography with text_decoration = Strikethrough } in
  let css = typography_to_css typo in
  check bool "has line-through" true (has_sub "text-decoration:line-through" css)

let test_typography_lowercase () =
  let typo = { default_typography with text_case = Lower } in
  let css = typography_to_css typo in
  check bool "has lowercase" true (has_sub "text-transform:lowercase" css)

let test_typography_title () =
  let typo = { default_typography with text_case = Title } in
  let css = typography_to_css typo in
  check bool "has capitalize" true (has_sub "text-transform:capitalize" css)

let test_typography_smallcaps () =
  let typo = { default_typography with text_case = SmallCaps } in
  let css = typography_to_css typo in
  check bool "has small-caps" true (has_sub "font-variant:small-caps" css)

let test_typography_smallcaps_forced () =
  let typo = { default_typography with text_case = SmallCapsForced } in
  let css = typography_to_css typo in
  check bool "has all-small-caps" true (has_sub "font-variant:all-small-caps" css)

let test_typography_left_align_omitted () =
  let typo = { default_typography with text_align_h = Left } in
  let css = typography_to_css typo in
  check bool "no text-align for Left" false (has_sub "text-align:" css)

let typography_extended_tests = [
  "letter spacing", `Quick, test_typography_letter_spacing;
  "letter spacing zero", `Quick, test_typography_letter_spacing_zero;
  "justified", `Quick, test_typography_justified;
  "right align", `Quick, test_typography_right_align;
  "strikethrough", `Quick, test_typography_strikethrough;
  "lowercase", `Quick, test_typography_lowercase;
  "title case", `Quick, test_typography_title;
  "small caps", `Quick, test_typography_smallcaps;
  "forced small caps", `Quick, test_typography_smallcaps_forced;
  "left align omitted", `Quick, test_typography_left_align_omitted;
]

(** ============== 12. Style to CSS Extended Tests ============== *)

let test_style_to_css_precise () =
  let node = mk ~fills:[make_paint Solid (Some (make_rgba 0.5 0.5 0.5))] () in
  let css = style_to_css ~precise:true node in
  check bool "precise uses rgb()" true (has_sub "rgb(" css)

let test_style_to_css_stroke_border () =
  let node = mk
    ~strokes:[make_paint Solid (Some (make_rgba 0.0 0.0 0.0))]
    ~stroke_weight:2. () in
  let css = style_to_css node in
  check bool "has border" true (has_sub "border:" css);
  check bool "has solid" true (has_sub "solid" css)

let test_style_to_css_effects () =
  let fx = { (make_fx DropShadow) with
    offset = Some (2., 2.); radius = 4.;
    color = Some (make_rgba ~a:0.5 0.0 0.0 0.0) } in
  let node = mk ~effects:[fx] () in
  let css = style_to_css node in
  check bool "has box-shadow" true (has_sub "box-shadow:" css)

let test_style_to_css_no_stroke_zero_weight () =
  let node = mk
    ~strokes:[make_paint Solid (Some (make_rgba 0.0 0.0 0.0))]
    ~stroke_weight:0. () in
  let css = style_to_css node in
  check bool "no border when weight=0" false (has_sub "border:" css)

let style_extended_tests = [
  "precise mode rgb()", `Quick, test_style_to_css_precise;
  "stroke border", `Quick, test_style_to_css_stroke_border;
  "effects in style", `Quick, test_style_to_css_effects;
  "no border zero weight", `Quick, test_style_to_css_no_stroke_zero_weight;
]

(** ============== 13. node_to_compact Extended Tests ============== *)

let test_compact_input () =
  let node = mk ~node_type:Component ~name:"input_email"
    ~bbox:(Some { x = 0.; y = 0.; width = 300.; height = 48. }) () in
  let result = node_to_compact node in
  check bool "starts with N" true (String.sub result 0 1 = "N");
  check bool "has width" true (has_sub "w:300" result)

let test_compact_input_no_bbox () =
  let node = mk ~node_type:Component ~name:"input_search" () in
  let result = node_to_compact node in
  check bool "starts with N" true (String.sub result 0 1 = "N")

let test_compact_icon () =
  let node = mk ~node_type:Vector ~name:"icon_star"
    ~bbox:(Some { x = 0.; y = 0.; width = 24.; height = 24. }) () in
  let result = node_to_compact node in
  check bool "starts with V" true (String.sub result 0 1 = "V");
  check bool "has icon name" true (has_sub "icon_star" result)

let test_compact_frame_abs () =
  let node = mk ~layout_mode:None'
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 100. }) () in
  let result = node_to_compact node in
  check bool "has abs direction" true (has_sub "F(abs" result)

let test_compact_frame_with_opacity () =
  let node = mk ~layout_mode:Horizontal ~opacity:0.5
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 50. }) () in
  let result = node_to_compact node in
  check bool "has op:" true (has_sub "op:" result)

let test_compact_frame_with_stroke () =
  let node = mk ~layout_mode:Horizontal
    ~strokes:[make_paint Solid (Some (make_rgba 1.0 0.0 0.0))]
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 50. }) () in
  let result = node_to_compact node in
  check bool "has bd:" true (has_sub "bd:" result)

let test_compact_frame_empty_no_children () =
  let node = mk ~layout_mode:Horizontal
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 50. }) () in
  let result = node_to_compact node in
  check bool "no braces for empty" false (has_sub "{" result)

let test_compact_text_align_right () =
  let node = mk ~node_type:Text ~characters:(Some "Right")
    ~typography:(Some { default_typography with text_align_h = Right }) () in
  let result = node_to_compact node in
  check bool "has a:r" true (has_sub "a:r" result)

let test_compact_text_no_typography () =
  let node = mk ~node_type:Text ~characters:(Some "Plain") () in
  let result = node_to_compact node in
  check bool "starts with T" true (String.sub result 0 1 = "T");
  check bool "no attrs when no typo" true (has_sub "T\"Plain\"" result)

let test_compact_button_no_children () =
  let node = mk ~node_type:Instance ~name:"button_primary"
    ~fills:[make_paint Solid (Some (make_rgba 0.0 0.5 1.0))]
    ~border_radius:4. () in
  let result = node_to_compact node in
  check bool "button uses name as label" true (has_sub "button_primary" result);
  check bool "has bg" true (has_sub "bg:" result);
  check bool "has radius" true (has_sub "r:4" result)

let test_compact_image_no_bbox () =
  let node = mk ~node_type:Instance ~name:"image_avatar" ~id:"img:42" () in
  let result = node_to_compact node in
  check bool "starts with I" true (String.sub result 0 1 = "I");
  check bool "has id" true (has_sub "id:img:42" result)

let test_compact_frame_no_bbox () =
  let node = mk ~layout_mode:Horizontal () in
  let result = node_to_compact node in
  check bool "F(row) without size" true (has_sub "F(row)" result)

let compact_extended_tests = [
  "compact input", `Quick, test_compact_input;
  "compact input no bbox", `Quick, test_compact_input_no_bbox;
  "compact icon", `Quick, test_compact_icon;
  "compact abs frame", `Quick, test_compact_frame_abs;
  "compact opacity", `Quick, test_compact_frame_with_opacity;
  "compact stroke", `Quick, test_compact_frame_with_stroke;
  "compact empty frame", `Quick, test_compact_frame_empty_no_children;
  "compact text right", `Quick, test_compact_text_align_right;
  "compact text no typo", `Quick, test_compact_text_no_typography;
  "compact btn no children", `Quick, test_compact_button_no_children;
  "compact img no bbox", `Quick, test_compact_image_no_bbox;
  "compact frame no bbox", `Quick, test_compact_frame_no_bbox;
]

(** ============== 14. node_to_fidelity Extended Tests ============== *)

let test_fidelity_button () =
  let text_child = mk ~node_type:Text ~characters:(Some "Submit") () in
  let node = mk ~node_type:Instance ~name:"button_primary"
    ~fills:[make_paint Solid (Some (make_rgba 0.0 0.5 1.0))]
    ~strokes:[make_paint Solid (Some (make_rgba 0.0 0.0 0.0))]
    ~stroke_weight:1. ~border_radius:8. ~opacity:0.9
    ~bbox:(Some { x = 10.; y = 20.; width = 150.; height = 48. })
    ~children:[text_child] () in
  let result = node_to_fidelity ~is_root:false node in
  check bool "starts with B" true (String.sub result 0 1 = "B");
  check bool "has bg" true (has_sub "bg:" result);
  check bool "has radius" true (has_sub "r:8" result);
  check bool "has border" true (has_sub "bd:" result);
  check bool "has border weight" true (has_sub "bw:1" result);
  check bool "has opacity" true (has_sub "op:0.9" result)

let test_fidelity_input () =
  let node = mk ~node_type:Component ~name:"input_email"
    ~fills:[make_paint Solid (Some (make_rgba 1.0 1.0 1.0))]
    ~strokes:[make_paint Solid (Some (make_rgba 0.8 0.8 0.8))]
    ~stroke_weight:1.
    ~bbox:(Some { x = 10.; y = 20.; width = 300.; height = 48. }) () in
  let result = node_to_fidelity ~is_root:false node in
  check bool "starts with N" true (String.sub result 0 1 = "N");
  check bool "has bg" true (has_sub "bg:" result);
  check bool "has border" true (has_sub "bd:" result);
  check bool "has bw" true (has_sub "bw:1" result)

let test_fidelity_image () =
  let node = mk ~node_type:Instance ~name:"image_avatar" ~id:"img:99"
    ~bbox:(Some { x = 10.; y = 20.; width = 80.; height = 80. }) () in
  let result = node_to_fidelity ~is_root:false node in
  check bool "starts with I" true (String.sub result 0 1 = "I");
  check bool "has id" true (has_sub "id:img:99" result)

let test_fidelity_icon () =
  let node = mk ~node_type:Vector ~name:"star_icon"
    ~bbox:(Some { x = 10.; y = 20.; width = 24.; height = 24. }) () in
  let result = node_to_fidelity ~is_root:false node in
  check bool "starts with V" true (String.sub result 0 1 = "V");
  check bool "has name" true (has_sub "star_icon" result);
  check bool "has sz" true (has_sub "sz:" result)

let test_fidelity_text_with_letter_spacing () =
  let node = mk ~node_type:Text ~characters:(Some "Spaced")
    ~typography:(Some { default_typography with
      font_size = 16.; letter_spacing = Some 2.0; text_align_h = Right })
    ~fills:[make_paint Solid (Some (make_rgba 0.0 0.0 0.0))]
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 20. }) () in
  let result = node_to_fidelity ~is_root:true node in
  check bool "has ls" true (has_sub "ls:" result);
  check bool "has align r" true (has_sub "a:r" result);
  check bool "has ff" true (has_sub "ff:" result)

let test_fidelity_text_justified () =
  let node = mk ~node_type:Text ~characters:(Some "Justified")
    ~typography:(Some { default_typography with text_align_h = Justified })
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 20. }) () in
  let result = node_to_fidelity ~is_root:true node in
  check bool "has a:j" true (has_sub "a:j" result)

let test_fidelity_text_with_opacity () =
  let node = mk ~node_type:Text ~characters:(Some "Faded")
    ~typography:(Some default_typography)
    ~opacity:0.6
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 20. }) () in
  let result = node_to_fidelity ~is_root:true node in
  check bool "has op" true (has_sub "op:0.6" result)

let test_fidelity_frame_border_radii () =
  let node = mk ~layout_mode:Vertical
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 300. })
    ~border_radii:(Some (8., 8., 0., 0.))
    ~gap:16. ~opacity:0.95
    ~strokes:[make_paint Solid (Some (make_rgba 0.0 0.0 0.0))]
    ~stroke_weight:2. ~rotation:45. () in
  let result = node_to_fidelity ~is_root:true node in
  check bool "has rr" true (has_sub "rr:8,8,0,0" result);
  check bool "has gap" true (has_sub "g:16" result);
  check bool "has opacity" true (has_sub "op:" result);
  check bool "has border" true (has_sub "bd:" result);
  check bool "has bw" true (has_sub "bw:2" result);
  check bool "has rotation" true (has_sub "rot:45" result)

let test_fidelity_frame_with_children () =
  let child = mk ~node_type:Text ~characters:(Some "item")
    ~bbox:(Some { x = 10.; y = 20.; width = 80.; height = 20. }) () in
  let node = mk ~layout_mode:Vertical
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 100. })
    ~children:[child] () in
  let result = node_to_fidelity ~is_root:true node in
  check bool "has children block" true (has_sub "{" result)

let test_fidelity_frame_empty () =
  let node = mk ~layout_mode:Horizontal
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 50. }) () in
  let result = node_to_fidelity ~is_root:true node in
  check bool "no braces for empty" false (has_sub "{" result)

let test_fidelity_divider () =
  let node = mk ~node_type:Rectangle
    ~bbox:(Some { x = 0.; y = 0.; width = 300.; height = 1. }) () in
  let result = node_to_fidelity ~is_root:false node in
  check string "divider ---" "---" result

let fidelity_extended_tests = [
  "fidelity button", `Quick, test_fidelity_button;
  "fidelity input", `Quick, test_fidelity_input;
  "fidelity image", `Quick, test_fidelity_image;
  "fidelity icon", `Quick, test_fidelity_icon;
  "fidelity text letter_spacing", `Quick, test_fidelity_text_with_letter_spacing;
  "fidelity text justified", `Quick, test_fidelity_text_justified;
  "fidelity text opacity", `Quick, test_fidelity_text_with_opacity;
  "fidelity frame border_radii", `Quick, test_fidelity_frame_border_radii;
  "fidelity frame children", `Quick, test_fidelity_frame_with_children;
  "fidelity frame empty", `Quick, test_fidelity_frame_empty;
  "fidelity divider", `Quick, test_fidelity_divider;
]

(** ============== 15. node_to_html Extended Tests ============== *)

let test_html_input () =
  let node = mk ~node_type:Component ~name:"input_email"
    ~bbox:(Some { x = 0.; y = 0.; width = 300.; height = 48. }) () in
  let html = node_to_html node in
  check bool "has input tag" true (has_sub "<input" html);
  check bool "has placeholder" true (has_sub "placeholder=" html)

let test_html_image () =
  let node = mk ~node_type:Instance ~name:"image_profile"
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 100. }) () in
  let html = node_to_html node in
  check bool "has img tag" true (has_sub "<img" html);
  check bool "has alt" true (has_sub "alt=" html)

let test_html_icon () =
  let node = mk ~node_type:Vector ~name:"icon_search"
    ~bbox:(Some { x = 0.; y = 0.; width = 24.; height = 24. }) () in
  let html = node_to_html node in
  check bool "has div tag" true (has_sub "<div" html);
  check bool "has comment" true (has_sub "<!--" html)

let test_html_divider () =
  let node = mk ~node_type:Rectangle
    ~bbox:(Some { x = 0.; y = 0.; width = 300.; height = 1. }) () in
  let html = node_to_html node in
  check bool "has div tag" true (has_sub "<div" html)

let test_html_frame_none_layout () =
  let node = mk ~layout_mode:None'
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 100. }) () in
  let html = node_to_html node in
  check bool "has position:relative" true (has_sub "position:relative" html)

let test_html_frame_justify_center () =
  let node = mk ~layout_mode:Horizontal ~primary_axis_align:Center
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "has justify-content:center" true (has_sub "justify-content:center" html)

let test_html_frame_justify_end () =
  let node = mk ~layout_mode:Horizontal ~primary_axis_align:Max
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "has justify-content:flex-end" true (has_sub "justify-content:flex-end" html)

let test_html_frame_justify_space_between () =
  let node = mk ~layout_mode:Horizontal ~primary_axis_align:SpaceBetween
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "has justify-content:space-between" true (has_sub "justify-content:space-between" html)

let test_html_frame_align_center () =
  let node = mk ~layout_mode:Horizontal ~counter_axis_align:Center
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "has align-items:center" true (has_sub "align-items:center" html)

let test_html_frame_align_end () =
  let node = mk ~layout_mode:Horizontal ~counter_axis_align:Max
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "has align-items:flex-end" true (has_sub "align-items:flex-end" html)

let test_html_frame_align_baseline () =
  let node = mk ~layout_mode:Horizontal ~counter_axis_align:Baseline
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "has align-items:baseline" true (has_sub "align-items:baseline" html)

let test_html_frame_empty_with_gap () =
  let node = mk ~layout_mode:Horizontal ~gap:16.
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "has gap" true (has_sub "gap:" html);
  check bool "empty div" true (has_sub "></div>" html)

let test_html_frame_empty_no_gap () =
  let node = mk ~layout_mode:Horizontal
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "no gap attr" false (has_sub "gap:" html)

let test_html_frame_children_with_gap () =
  let child = mk ~node_type:Text ~characters:(Some "item") () in
  let node = mk ~layout_mode:Horizontal ~gap:12. ~children:[child]
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "has gap" true (has_sub "gap:" html);
  check bool "has child content" true (has_sub "item" html)

let test_html_frame_children_no_gap () =
  let child = mk ~node_type:Text ~characters:(Some "item") () in
  let node = mk ~layout_mode:Horizontal ~children:[child]
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let html = node_to_html node in
  check bool "has child content" true (has_sub "item" html);
  check bool "no gap" false (has_sub "gap:" html)

let test_html_text_with_color () =
  let node = mk ~node_type:Text ~characters:(Some "Colored")
    ~typography:(Some default_typography)
    ~fills:[make_paint Solid (Some (make_rgba 1.0 0.0 0.0))] () in
  let html = node_to_html node in
  check bool "has color" true (has_sub "color:" html)

let test_html_text_no_typography () =
  let node = mk ~node_type:Text ~characters:(Some "Raw") () in
  let html = node_to_html node in
  check bool "has span" true (has_sub "<span" html);
  check bool "has text" true (has_sub "Raw" html)

let test_html_button () =
  let text_child = mk ~node_type:Text ~characters:(Some "Click") () in
  let node = mk ~node_type:Frame ~name:"button_act"
    ~children:[text_child]
    ~fills:[make_paint Solid (Some (make_rgba 0.0 0.5 1.0))] () in
  let html = node_to_html node in
  check bool "has button tag" true (has_sub "<button" html);
  check bool "has label" true (has_sub "Click" html)

let html_extended_tests = [
  "html input", `Quick, test_html_input;
  "html image", `Quick, test_html_image;
  "html icon", `Quick, test_html_icon;
  "html divider", `Quick, test_html_divider;
  "html None' layout", `Quick, test_html_frame_none_layout;
  "html justify center", `Quick, test_html_frame_justify_center;
  "html justify end", `Quick, test_html_frame_justify_end;
  "html justify space-between", `Quick, test_html_frame_justify_space_between;
  "html align center", `Quick, test_html_frame_align_center;
  "html align end", `Quick, test_html_frame_align_end;
  "html align baseline", `Quick, test_html_frame_align_baseline;
  "html empty with gap", `Quick, test_html_frame_empty_with_gap;
  "html empty no gap", `Quick, test_html_frame_empty_no_gap;
  "html children with gap", `Quick, test_html_frame_children_with_gap;
  "html children no gap", `Quick, test_html_frame_children_no_gap;
  "html text with color", `Quick, test_html_text_with_color;
  "html text no typo", `Quick, test_html_text_no_typography;
  "html button", `Quick, test_html_button;
]

(** ============== 16. BG Color Extended Tests ============== *)

let test_get_bg_color_radial_gradient () =
  let stops = [(0.0, make_rgba 1.0 0.0 0.0); (1.0, make_rgba 0.0 0.0 1.0)] in
  let fills = [{ paint_type = GradientRadial; visible = true; opacity = 1.0;
    color = None; gradient_stops = stops; image_ref = None; scale_mode = None }] in
  let result = get_bg_color fills in
  check bool "radial gradient returns Some" true (Option.is_some result)

let test_get_bg_color_angular_gradient () =
  let stops = [(0.0, make_rgba 1.0 0.0 0.0); (1.0, make_rgba 0.0 1.0 0.0)] in
  let fills = [{ paint_type = GradientAngular; visible = true; opacity = 1.0;
    color = None; gradient_stops = stops; image_ref = None; scale_mode = None }] in
  let result = get_bg_color fills in
  check bool "angular gradient returns Some" true (Option.is_some result)

let test_get_bg_color_diamond_gradient () =
  let stops = [(0.0, make_rgba 1.0 1.0 0.0); (1.0, make_rgba 0.0 1.0 1.0)] in
  let fills = [{ paint_type = GradientDiamond; visible = true; opacity = 1.0;
    color = None; gradient_stops = stops; image_ref = None; scale_mode = None }] in
  let result = get_bg_color fills in
  check bool "diamond gradient returns Some" true (Option.is_some result)

let test_get_bg_color_emoji () =
  let fills = [{ paint_type = Emoji; visible = true; opacity = 1.0;
    color = None; gradient_stops = []; image_ref = None; scale_mode = None }] in
  let result = get_bg_color fills in
  check bool "emoji returns None" true (Option.is_none result)

let test_get_bg_color_precise_gradient () =
  let stops = [(0.0, make_rgba 0.5 0.0 0.0); (1.0, make_rgba 0.0 0.0 0.5)] in
  let fills = [make_gradient_paint stops] in
  let result = get_bg_color_precise fills in
  check bool "precise gradient uses rgb()" true
    (Option.is_some result && has_sub "rgb(" (Option.get result))

let test_get_bg_color_precise_radial () =
  let stops = [(0.0, make_rgba 1.0 0.0 0.0); (1.0, make_rgba 0.0 0.0 1.0)] in
  let fills = [{ paint_type = GradientRadial; visible = true; opacity = 1.0;
    color = None; gradient_stops = stops; image_ref = None; scale_mode = None }] in
  let result = get_bg_color_precise fills in
  check bool "precise radial returns Some" true (Option.is_some result)

let test_get_bg_fidelity_emoji () =
  let fills = [{ paint_type = Emoji; visible = true; opacity = 1.0;
    color = None; gradient_stops = []; image_ref = None; scale_mode = None }] in
  let result = get_bg_fidelity fills in
  check string "emoji returns 'emoji'" "emoji" (Option.get result)

let test_get_bg_fidelity_empty () =
  let result = get_bg_fidelity [] in
  check bool "empty returns None" true (Option.is_none result)

let test_get_bg_fidelity_radial () =
  let stops = [(0.0, make_rgba 1.0 0.0 0.0)] in
  let fills = [{ paint_type = GradientRadial; visible = true; opacity = 1.0;
    color = None; gradient_stops = stops; image_ref = None; scale_mode = None }] in
  let result = get_bg_fidelity fills in
  check string "radial returns 'grad'" "grad" (Option.get result)

let bg_extended_tests = [
  "radial gradient", `Quick, test_get_bg_color_radial_gradient;
  "angular gradient", `Quick, test_get_bg_color_angular_gradient;
  "diamond gradient", `Quick, test_get_bg_color_diamond_gradient;
  "emoji returns None", `Quick, test_get_bg_color_emoji;
  "precise gradient rgb()", `Quick, test_get_bg_color_precise_gradient;
  "precise radial", `Quick, test_get_bg_color_precise_radial;
  "fidelity emoji", `Quick, test_get_bg_fidelity_emoji;
  "fidelity empty", `Quick, test_get_bg_fidelity_empty;
  "fidelity radial grad", `Quick, test_get_bg_fidelity_radial;
]

(** ============== 17. StyleRegistry Extended Tests ============== *)

let test_registry_font_finalize () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_font counter 16.;
  StyleRegistry.count_font counter 16.;
  StyleRegistry.count_font counter 20.;
  let reg = StyleRegistry.finalize counter 2 in
  check int "one font var" 1 (Hashtbl.length reg.fonts);
  check bool "16 variablized" true (Hashtbl.mem reg.fonts 16.)

let test_registry_weight_finalize () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_weight counter 700;
  StyleRegistry.count_weight counter 700;
  StyleRegistry.count_weight counter 400;
  let reg = StyleRegistry.finalize counter 2 in
  check int "one weight var" 1 (Hashtbl.length reg.weights);
  check bool "700 variablized" true (Hashtbl.mem reg.weights 700)

let test_registry_lookup_font () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_font counter 18.;
  StyleRegistry.count_font counter 18.;
  let reg = StyleRegistry.finalize counter 2 in
  let looked = StyleRegistry.lookup_font reg 18. in
  check bool "starts with $f" true (has_sub "$f" looked);
  let not_var = StyleRegistry.lookup_font reg 24. in
  check string "unknown returns fmt_num" "24" not_var

let test_registry_lookup_weight () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_weight counter 600;
  StyleRegistry.count_weight counter 600;
  let reg = StyleRegistry.finalize counter 2 in
  let looked = StyleRegistry.lookup_weight reg 600 in
  check bool "starts with $w" true (has_sub "$w" looked);
  let not_var = StyleRegistry.lookup_weight reg 300 in
  check string "unknown returns string_of_int" "300" not_var

let test_registry_to_defs_full () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_color counter "#AABBCC";
  StyleRegistry.count_color counter "#AABBCC";
  StyleRegistry.count_font counter 16.;
  StyleRegistry.count_font counter 16.;
  StyleRegistry.count_weight counter 700;
  StyleRegistry.count_weight counter 700;
  let reg = StyleRegistry.finalize counter 2 in
  let defs = StyleRegistry.to_defs reg in
  check bool "starts with @vars" true (has_sub "@vars{" defs);
  check bool "has color var" true (has_sub "$c" defs);
  check bool "has font var" true (has_sub "$f" defs);
  check bool "has weight var" true (has_sub "$w" defs)

let test_registry_apply_to_dsl () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_color counter "#FF0000";
  StyleRegistry.count_color counter "#FF0000";
  let reg = StyleRegistry.finalize counter 2 in
  let dsl = "F(row)[bg:#FF0000]{T\"hello\"[c:#FF0000]}" in
  let result = StyleRegistry.apply_to_dsl reg dsl in
  check bool "colors replaced" true (has_sub "$c" result);
  check bool "no raw hex" false (has_sub "#FF0000" result)

let test_registry_collect_from_node () =
  let counter = StyleRegistry.create_counter () in
  let child1 = mk ~fills:[make_paint Solid (Some (make_rgba 0.0 0.5 1.0))]
    ~typography:(Some { default_typography with font_size = 18.; font_weight = 700 }) () in
  let child2 = mk ~fills:[make_paint Solid (Some (make_rgba 0.0 0.5 1.0))]
    ~typography:(Some { default_typography with font_size = 18.; font_weight = 700 }) () in
  let parent = mk ~children:[child1; child2] () in
  StyleRegistry.collect_from_node counter parent;
  let font_freq = Hashtbl.find_opt counter.font_freq 18. in
  check bool "font counted" true (Option.is_some font_freq);
  check bool "font >= 2" true (Option.get font_freq >= 2)

let registry_extended_tests = [
  "font finalize", `Quick, test_registry_font_finalize;
  "weight finalize", `Quick, test_registry_weight_finalize;
  "lookup font", `Quick, test_registry_lookup_font;
  "lookup weight", `Quick, test_registry_lookup_weight;
  "to_defs full", `Quick, test_registry_to_defs_full;
  "apply_to_dsl", `Quick, test_registry_apply_to_dsl;
  "collect_from_node", `Quick, test_registry_collect_from_node;
]

(** ============== 18. fidelity_node JSON Tests ============== *)

let test_fidelity_node_text () =
  let json = `Assoc [
    ("type", `String "TEXT");
    ("id", `String "1:1");
    ("name", `String "Title");
    ("characters", `String "Hello World");
    ("style", `Assoc [("fontSize", `Float 24.)]);
    ("opacity", `Float 0.8);
    ("effects", `List []);
  ] in
  let result = fidelity_node json in
  (* Check meta has type TEXT *)
  (match json_member "meta" result with
   | Some meta ->
     (match json_member "type" meta with
      | Some (`String t) -> check string "type TEXT" "TEXT" t
      | _ -> fail "missing type in meta")
   | None -> fail "missing meta");
  (* Check text fields are present *)
  (match json_member "text" result with
   | Some (`Assoc fields) ->
     check bool "has characters" true (List.exists (fun (k, _) -> k = "characters") fields)
   | _ -> fail "missing text assoc")

let test_fidelity_node_vector () =
  let json = `Assoc [
    ("type", `String "VECTOR");
    ("id", `String "2:1");
    ("name", `String "Arrow");
    ("fills", `List []);
    ("vectorPaths", `List []);
  ] in
  let result = fidelity_node json in
  (match json_member "vector" result with
   | Some (`Assoc fields) ->
     check bool "has vectorPaths" true (List.exists (fun (k, _) -> k = "vectorPaths") fields)
   | _ -> fail "missing vector assoc")

let test_fidelity_node_instance () =
  let json = `Assoc [
    ("type", `String "INSTANCE");
    ("id", `String "3:1");
    ("name", `String "ButtonInstance");
    ("componentId", `String "comp:42");
  ] in
  let result = fidelity_node json in
  (match json_member "instance" result with
   | Some (`Assoc fields) ->
     check bool "has componentId" true (List.exists (fun (k, _) -> k = "componentId") fields)
   | _ -> fail "missing instance assoc")

let test_fidelity_node_component () =
  let json = `Assoc [
    ("type", `String "COMPONENT");
    ("id", `String "4:1");
    ("name", `String "ButtonComp");
  ] in
  let result = fidelity_node json in
  (match json_member "instance_missing" result with
   | Some (`List _) -> ()  (* instance keys are missing, which is expected *)
   | _ -> fail "missing instance_missing")

let test_fidelity_node_with_layout () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "5:1");
    ("name", `String "Container");
    ("layoutMode", `String "HORIZONTAL");
    ("primaryAxisAlignItems", `String "CENTER");
    ("counterAxisAlignItems", `String "CENTER");
    ("paddingTop", `Int 16);
  ] in
  let result = fidelity_node json in
  (match json_member "layout" result with
   | Some (`Assoc fields) ->
     check bool "has layoutMode" true (List.exists (fun (k, _) -> k = "layoutMode") fields)
   | _ -> fail "missing layout assoc")

let test_fidelity_node_with_children () =
  let child = `Assoc [
    ("type", `String "TEXT");
    ("id", `String "6:1");
    ("name", `String "child_text");
  ] in
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "5:1");
    ("name", `String "Parent");
    ("children", `List [child]);
  ] in
  let result = fidelity_node json in
  (match json_member "children_present" result with
   | Some (`Bool b) -> check bool "children present" true b
   | _ -> fail "missing children_present");
  (match json_member "children" result with
   | Some (`List items) -> check int "1 child" 1 (List.length items)
   | _ -> fail "missing children list")

let test_fidelity_node_with_image_refs () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "7:1");
    ("name", `String "ImageFrame");
    ("fills", `List [
      `Assoc [("imageRef", `String "img:abc")]
    ]);
  ] in
  let result = fidelity_node json in
  (match json_member "assets" result with
   | Some assets ->
     (match json_member "image_refs" assets with
      | Some (`List refs) -> check bool "has image ref" true (List.length refs > 0)
      | _ -> fail "missing image_refs")
   | None -> fail "missing assets")

let test_fidelity_node_document () =
  let json = `Assoc [
    ("type", `String "DOCUMENT");
    ("id", `String "0:0");
    ("name", `String "Doc");
  ] in
  let result = fidelity_node json in
  (match json_member "structure_missing" result with
   | Some (`List items) ->
     check bool "missing children for DOCUMENT" true (List.length items > 0)
   | _ -> fail "missing structure_missing")

let test_fidelity_node_canvas () =
  let json = `Assoc [
    ("type", `String "CANVAS");
    ("id", `String "0:1");
    ("name", `String "Page 1");
    ("children", `List []);
  ] in
  let result = fidelity_node json in
  (match json_member "structure" result with
   | Some structure ->
     (match json_member "children_present" structure with
      | Some (`Bool b) -> check bool "children present" true b
      | _ -> fail "missing children_present in structure")
   | None -> fail "missing structure")

let test_fidelity_node_bound_variables () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "8:1");
    ("name", `String "VarFrame");
    ("boundVariables", `Assoc [("fills", `String "var:color1")]);
  ] in
  let result = fidelity_node json in
  (match json_member "variables" result with
   | Some (`Assoc fields) ->
     check bool "has boundVariables" true (List.exists (fun (k, _) -> k = "boundVariables") fields)
   | _ -> fail "missing variables assoc")

let fidelity_node_tests = [
  "fidelity_node TEXT", `Quick, test_fidelity_node_text;
  "fidelity_node VECTOR", `Quick, test_fidelity_node_vector;
  "fidelity_node INSTANCE", `Quick, test_fidelity_node_instance;
  "fidelity_node COMPONENT", `Quick, test_fidelity_node_component;
  "fidelity_node with layout", `Quick, test_fidelity_node_with_layout;
  "fidelity_node with children", `Quick, test_fidelity_node_with_children;
  "fidelity_node image refs", `Quick, test_fidelity_node_with_image_refs;
  "fidelity_node DOCUMENT", `Quick, test_fidelity_node_document;
  "fidelity_node CANVAS", `Quick, test_fidelity_node_canvas;
  "fidelity_node bound vars", `Quick, test_fidelity_node_bound_variables;
]

(** ============== 19. generate_flat_html Extended Tests ============== *)

let test_flat_html_single_structure () =
  (* outer and inner are the same size -> single structure path *)
  (* text child has NO fills so find_inner_frame stays on outer node *)
  let text = mk ~node_type:Text ~characters:(Some "Label")
    ~typography:(Some default_typography) () in
  let node = mk ~name:"single"
    ~fills:[make_paint Solid (Some (make_rgba 0.0 0.5 1.0))]
    ~border_radius:4.
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 48. })
    ~children:[text] () in
  let html = generate_flat_html node in
  check bool "has DOCTYPE" true (has_sub "<!DOCTYPE" html);
  check bool "has Label" true (has_sub "Label" html);
  check bool "has border-radius" true (has_sub "border-radius:" html)

let test_flat_html_no_text () =
  let node = mk ~name:"NoText"
    ~fills:[make_paint Solid (Some (make_rgba 0.0 0.5 1.0))]
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 48. }) () in
  let html = generate_flat_html node in
  check bool "uses name as fallback" true (has_sub "NoText" html)

let test_flat_html_no_bbox () =
  let node = mk ~name:"NoBbox" () in
  let html = generate_flat_html node in
  (* Should use defaults 343x48 *)
  check bool "has DOCTYPE" true (has_sub "<!DOCTYPE" html);
  check bool "has fallback size" true (has_sub "343" html)

let test_flat_html_no_typography () =
  let text = mk ~node_type:Text ~characters:(Some "NoTypo") () in
  let node = mk ~children:[text]
    ~fills:[make_paint Solid (Some (make_rgba 0.5 0.5 0.5))]
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 48. }) () in
  let html = generate_flat_html node in
  check bool "uses default font" true (has_sub "Noto Sans KR" html)

let flat_html_extended_tests = [
  "flat html single struct", `Quick, test_flat_html_single_structure;
  "flat html no text", `Quick, test_flat_html_no_text;
  "flat html no bbox", `Quick, test_flat_html_no_bbox;
  "flat html no typography", `Quick, test_flat_html_no_typography;
]

(** ============== 20. Effects with No Color Tests ============== *)

let test_effects_drop_shadow_no_color () =
  let fx = { (make_fx DropShadow) with
    offset = Some (2., 2.); radius = 4.; spread = Some 1.; color = None } in
  let result = effects_to_css [fx] in
  check bool "has default rgba" true (has_sub "rgba(0,0,0,0.25)" result)

let test_effects_inner_shadow_no_color () =
  let fx = { (make_fx InnerShadow) with
    offset = Some (1., 1.); radius = 2.; color = None } in
  let result = effects_to_css [fx] in
  check bool "has inset with default" true (has_sub "inset" result);
  check bool "has default rgba" true (has_sub "rgba(0,0,0,0.25)" result)

let test_effects_drop_shadow_no_offset () =
  let fx = { (make_fx DropShadow) with
    offset = None; radius = 4.; color = Some (make_rgba ~a:0.5 0.0 0.0 0.0) } in
  let result = effects_to_css [fx] in
  check bool "has box-shadow" true (has_sub "box-shadow:" result)

let effects_extended_tests = [
  "drop shadow no color", `Quick, test_effects_drop_shadow_no_color;
  "inner shadow no color", `Quick, test_effects_inner_shadow_no_color;
  "drop shadow no offset", `Quick, test_effects_drop_shadow_no_offset;
]

(** ============== 21. extract_root_node Extended Tests ============== *)

let test_extract_root_nodes_multiple () =
  (* Multiple nodes in "nodes" -> fallback to json *)
  let json = `Assoc [
    ("nodes", `Assoc [
      ("1:1", `Assoc [("document", `Assoc [("type", `String "FRAME")])]);
      ("1:2", `Assoc [("document", `Assoc [("type", `String "FRAME")])])
    ])
  ] in
  let root = extract_root_node json in
  check bool "multiple nodes returns json" true (root = json)

let test_extract_root_nodes_no_document () =
  (* Single node without "document" key *)
  let json = `Assoc [
    ("nodes", `Assoc [
      ("1:1", `Assoc [("type", `String "FRAME")])
    ])
  ] in
  let root = extract_root_node json in
  check bool "fallback to json" true (root = json)

let test_extract_root_nodes_not_assoc () =
  let json = `Assoc [
    ("nodes", `String "invalid")
  ] in
  let root = extract_root_node json in
  check bool "non-assoc nodes -> fallback" true (root = json)

let extract_root_extended_tests = [
  "nodes multiple -> fallback", `Quick, test_extract_root_nodes_multiple;
  "nodes no document", `Quick, test_extract_root_nodes_no_document;
  "nodes not assoc", `Quick, test_extract_root_nodes_not_assoc;
]

(** ============== 22. Split Canvas Tests ============== *)

let test_split_canvas () =
  let child1 = make_node ~name:"Page1" () in
  let child2 = make_node ~name:"Page2" () in
  let canvas = make_node ~node_type:Canvas ~children:[child1; child2] () in
  let result = split_to_components canvas in
  check bool "has Page1" true (has_sub "Page1" result);
  check bool "has Page2" true (has_sub "Page2" result)

let split_extended_tests = [
  "split canvas children", `Quick, test_split_canvas;
]

(** ============== 23. add_xy_size_attrs Extended Tests ============== *)

let test_add_xy_no_origin_non_root () =
  let node = mk ~bbox:(Some { x = 50.; y = 60.; width = 100.; height = 80. }) () in
  let attrs = add_xy_size_attrs [] node in
  check int "2 attrs (xy + sz)" 2 (List.length attrs);
  check bool "has xy" true (List.exists (fun a -> has_sub "xy:" a) attrs);
  check bool "has sz" true (List.exists (fun a -> has_sub "sz:" a) attrs)

let xy_extended_tests = [
  "xy no origin non-root", `Quick, test_add_xy_no_origin_non_root;
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
    "9. Verbose DSL", verbose_tests;
    "10. Classify Extended", classify_extended_tests;
    "11. Typography Extended", typography_extended_tests;
    "12. Style CSS Extended", style_extended_tests;
    "13. Compact Extended", compact_extended_tests;
    "14. Fidelity Extended", fidelity_extended_tests;
    "15. HTML Extended", html_extended_tests;
    "16. BG Color Extended", bg_extended_tests;
    "17. Registry Extended", registry_extended_tests;
    "18. Fidelity Node JSON", fidelity_node_tests;
    "19. Flat HTML Extended", flat_html_extended_tests;
    "20. Effects Extended", effects_extended_tests;
    "21. Extract Root Extended", extract_root_extended_tests;
    "22. Split Extended", split_extended_tests;
    "23. XY Attrs Extended", xy_extended_tests;
  ]
