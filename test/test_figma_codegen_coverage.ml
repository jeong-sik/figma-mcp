(** Comprehensive Coverage Tests for figma_codegen.ml *)

open Alcotest
open Figma_types
open Figma_codegen

(** ============== Test Fixtures ============== *)

let make_paint ?(visible=true) ?(opacity=1.0) paint_type color =
  { paint_type; visible; opacity; color; gradient_stops = []; image_ref = None; scale_mode = None }

let make_gradient_paint ?(visible=true) stops =
  { paint_type = GradientLinear; visible; opacity = 1.0; color = None;
    gradient_stops = stops; image_ref = None; scale_mode = None }

let make_rgba ?(a=1.0) r g b = { r; g; b; a }

let make_node ?(id="node-1") ?(name="TestNode") ?(node_type=Frame)
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

(** ============== 1. Number Formatting Tests ============== *)

let test_fmt_num_integer () =
  check string "integer 100" "100" (fmt_num 100.0);
  check string "integer 0" "0" (fmt_num 0.0);
  check string "integer -50" "-50" (fmt_num (-50.0))

let test_fmt_num_decimal () =
  check string "1.5" "1.5" (fmt_num 1.5);
  check string "3.7" "3.7" (fmt_num 3.7);
  check string "0.1" "0.1" (fmt_num 0.1)

let test_fmt_num_opt_some () =
  check string "Some 10.0" "10" (fmt_num_opt (Some 10.0));
  check string "Some 5.5" "5.5" (fmt_num_opt (Some 5.5))

let test_fmt_num_opt_none () =
  check string "None returns empty" "" (fmt_num_opt None)

let number_formatting_tests = [
  "fmt_num integers", `Quick, test_fmt_num_integer;
  "fmt_num decimals", `Quick, test_fmt_num_decimal;
  "fmt_num_opt Some", `Quick, test_fmt_num_opt_some;
  "fmt_num_opt None", `Quick, test_fmt_num_opt_none;
]

(** ============== 2. Background Color Extraction Tests ============== *)

let test_get_bg_color_solid () =
  let fills = [make_paint Solid (Some (make_rgba 1.0 0.0 0.0))] in
  let result = get_bg_color fills in
  check bool "should return Some" true (Option.is_some result);
  check string "red hex" "#FF0000" (Option.get result)

let test_get_bg_color_solid_with_alpha () =
  let fills = [make_paint Solid (Some (make_rgba ~a:0.5 0.0 1.0 0.0))] in
  let result = get_bg_color fills in
  check bool "should return Some" true (Option.is_some result);
  let hex = Option.get result in
  check bool "contains 8 chars (with alpha)" true (String.length hex = 9)

let test_get_bg_color_gradient () =
  let stops = [(0.0, make_rgba 1.0 0.0 0.0); (1.0, make_rgba 0.0 0.0 1.0)] in
  let fills = [make_gradient_paint stops] in
  let result = get_bg_color fills in
  check bool "should return Some" true (Option.is_some result);
  check bool "contains linear-gradient" true
    (String.sub (Option.get result) 0 15 = "linear-gradient")

let test_get_bg_color_invisible () =
  let fills = [make_paint ~visible:false Solid (Some (make_rgba 1.0 0.0 0.0))] in
  let result = get_bg_color fills in
  check bool "invisible returns None" true (Option.is_none result)

let test_get_bg_color_empty () =
  let result = get_bg_color [] in
  check bool "empty fills returns None" true (Option.is_none result)

let test_get_bg_color_image () =
  let fills = [{ paint_type = Image; visible = true; opacity = 1.0;
                 color = None; gradient_stops = []; image_ref = Some "img:123";
                 scale_mode = Some "FILL" }] in
  let result = get_bg_color fills in
  check bool "image returns None" true (Option.is_none result)

let test_get_bg_color_precise () =
  let fills = [make_paint Solid (Some (make_rgba 0.5 0.5 0.5))] in
  let result = get_bg_color_precise fills in
  check bool "should return Some" true (Option.is_some result);
  check bool "uses rgb()" true (String.sub (Option.get result) 0 4 = "rgb(")

let test_get_bg_fidelity_solid () =
  let fills = [make_paint Solid (Some (make_rgba 1.0 0.0 0.0))] in
  let result = get_bg_fidelity fills in
  check string "solid returns hex" "#FF0000" (Option.get result)

let test_get_bg_fidelity_gradient () =
  let stops = [(0.0, make_rgba 1.0 0.0 0.0); (1.0, make_rgba 0.0 0.0 1.0)] in
  let fills = [make_gradient_paint stops] in
  let result = get_bg_fidelity fills in
  check string "gradient returns 'grad'" "grad" (Option.get result)

let test_get_bg_fidelity_image () =
  let fills = [{ paint_type = Image; visible = true; opacity = 1.0;
                 color = None; gradient_stops = []; image_ref = Some "img:123";
                 scale_mode = Some "FILL" }] in
  let result = get_bg_fidelity fills in
  check string "image returns 'img'" "img" (Option.get result)

let bg_color_tests = [
  "solid color hex", `Quick, test_get_bg_color_solid;
  "solid with alpha", `Quick, test_get_bg_color_solid_with_alpha;
  "gradient to CSS", `Quick, test_get_bg_color_gradient;
  "invisible paint", `Quick, test_get_bg_color_invisible;
  "empty fills", `Quick, test_get_bg_color_empty;
  "image returns None", `Quick, test_get_bg_color_image;
  "precise uses rgb()", `Quick, test_get_bg_color_precise;
  "fidelity solid", `Quick, test_get_bg_fidelity_solid;
  "fidelity gradient", `Quick, test_get_bg_fidelity_gradient;
  "fidelity image", `Quick, test_get_bg_fidelity_image;
]

(** ============== 3. Stroke Color Tests ============== *)

let test_get_stroke_color_solid () =
  let strokes = [make_paint Solid (Some (make_rgba 0.0 0.0 0.0))] in
  let result = get_stroke_color strokes in
  check string "black stroke" "#000000" (Option.get result)

let test_get_stroke_color_invisible () =
  let strokes = [make_paint ~visible:false Solid (Some (make_rgba 1.0 0.0 0.0))] in
  let result = get_stroke_color strokes in
  check bool "invisible returns None" true (Option.is_none result)

let test_get_stroke_color_gradient () =
  let strokes = [make_gradient_paint [(0.0, make_rgba 1.0 0.0 0.0)]] in
  let result = get_stroke_color strokes in
  check bool "gradient stroke returns None" true (Option.is_none result)

let stroke_color_tests = [
  "solid stroke", `Quick, test_get_stroke_color_solid;
  "invisible stroke", `Quick, test_get_stroke_color_invisible;
  "gradient stroke None", `Quick, test_get_stroke_color_gradient;
]

(** ============== 4. Padding Formatting Tests ============== *)

let test_fmt_padding_all_zero () =
  let result = fmt_padding (0., 0., 0., 0.) in
  check bool "all zero returns None" true (Option.is_none result)

let test_fmt_padding_uniform () =
  let result = fmt_padding (16., 16., 16., 16.) in
  check string "uniform 16" "16" (Option.get result)

let test_fmt_padding_different () =
  let result = fmt_padding (10., 20., 10., 20.) in
  check string "different values" "10,20,10,20" (Option.get result)

let test_fmt_padding_decimal () =
  let result = fmt_padding (8.5, 8.5, 8.5, 8.5) in
  check string "decimal uniform" "8.5" (Option.get result)

let padding_tests = [
  "all zero None", `Quick, test_fmt_padding_all_zero;
  "uniform single", `Quick, test_fmt_padding_uniform;
  "different 4-tuple", `Quick, test_fmt_padding_different;
  "decimal values", `Quick, test_fmt_padding_decimal;
]

(** ============== 5. Alignment/Sizing Code Tests ============== *)

let test_align_to_code () =
  check string "Min" "min" (align_to_code Min);
  check string "Center" "c" (align_to_code Center);
  check string "Max" "max" (align_to_code Max);
  check string "SpaceBetween" "sb" (align_to_code SpaceBetween);
  check string "Baseline" "base" (align_to_code Baseline)

let test_sizing_to_code () =
  check string "Fixed" "fix" (sizing_to_code Fixed);
  check string "Hug" "hug" (sizing_to_code Hug);
  check string "Fill" "fill" (sizing_to_code Fill)

let code_conversion_tests = [
  "align_to_code all", `Quick, test_align_to_code;
  "sizing_to_code all", `Quick, test_sizing_to_code;
]

(** ============== 6. Node Classification Tests ============== *)

let test_classify_text_node () =
  let node = make_node ~node_type:Text () in
  check bool "Text -> SemText" true (classify_node node = SemText)

let test_classify_button_by_name () =
  let node = make_node ~node_type:Instance ~name:"Button_Primary" () in
  check bool "button name -> SemButton" true (classify_node node = SemButton)

let test_classify_btn_prefix () =
  let node = make_node ~node_type:Frame ~name:"btn_submit" () in
  check bool "btn_ prefix -> SemButton" true (classify_node node = SemButton)

let test_classify_input_by_name () =
  let node = make_node ~node_type:Component ~name:"Input Field" () in
  check bool "input name -> SemInput" true (classify_node node = SemInput)

let test_classify_image_by_name () =
  let node = make_node ~node_type:Instance ~name:"image_avatar" () in
  check bool "image name -> SemImage" true (classify_node node = SemImage)

let test_classify_icon_by_name () =
  let node = make_node ~node_type:Instance ~name:"icon_search" () in
  check bool "icon name -> SemIcon" true (classify_node node = SemIcon)

let test_classify_vector_as_icon () =
  let node = make_node ~node_type:Vector () in
  check bool "Vector -> SemIcon" true (classify_node node = SemIcon)

let test_classify_thin_rect_divider () =
  let node = make_node ~node_type:Rectangle
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 1. }) () in
  check bool "thin rect -> SemDivider" true (classify_node node = SemDivider)

let test_classify_frame_default () =
  let node = make_node ~node_type:Frame ~name:"Container" () in
  check bool "default Frame -> SemFrame" true (classify_node node = SemFrame)

let classification_tests = [
  "Text node", `Quick, test_classify_text_node;
  "Button by name", `Quick, test_classify_button_by_name;
  "btn_ prefix", `Quick, test_classify_btn_prefix;
  "Input by name", `Quick, test_classify_input_by_name;
  "Image by name", `Quick, test_classify_image_by_name;
  "Icon by name", `Quick, test_classify_icon_by_name;
  "Vector as Icon", `Quick, test_classify_vector_as_icon;
  "Thin rect Divider", `Quick, test_classify_thin_rect_divider;
  "Frame default", `Quick, test_classify_frame_default;
]

(** ============== 7. Compact DSL Generation Tests ============== *)

let test_node_to_compact_text () =
  let node = make_node ~node_type:Text ~characters:(Some "Hello")
    ~typography:(Some { default_typography with font_size = 18.; font_weight = 700 }) () in
  let result = node_to_compact node in
  check bool "contains T\"Hello\"" true (String.length result > 0);
  check bool "has text marker" true (String.sub result 0 1 = "T")

let test_node_to_compact_button () =
  let text_child = make_node ~node_type:Text ~characters:(Some "Submit") () in
  let node = make_node ~name:"button_primary" ~children:[text_child]
    ~fills:[make_paint Solid (Some (make_rgba 0.0 0.478 1.0))]
    ~border_radius:8. () in
  let result = node_to_compact node in
  check bool "contains B\"Submit\"" true (String.sub result 0 1 = "B")

let test_node_to_compact_frame_row () =
  let node = make_node ~layout_mode:Horizontal
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 50. }) () in
  let result = node_to_compact node in
  check bool "contains F(row" true
    (String.length (Str.global_replace (Str.regexp "F(row") "" result) < String.length result)

let test_node_to_compact_frame_col () =
  let node = make_node ~layout_mode:Vertical
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 300. }) () in
  let result = node_to_compact node in
  check bool "contains F(col" true
    (String.length (Str.global_replace (Str.regexp "F(col") "" result) < String.length result)

let test_node_to_compact_with_children () =
  let child1 = make_node ~node_type:Text ~characters:(Some "Item 1") () in
  let child2 = make_node ~node_type:Text ~characters:(Some "Item 2") () in
  let node = make_node ~layout_mode:Vertical ~children:[child1; child2] () in
  let result = node_to_compact node in
  check bool "contains children block" true
    (String.contains result '{' && String.contains result '}')

let test_node_to_compact_hidden_children () =
  let visible_child = make_node ~node_type:Text ~characters:(Some "Visible") () in
  let hidden_child = make_node ~visible:false ~node_type:Text ~characters:(Some "Hidden") () in
  let node = make_node ~children:[visible_child; hidden_child] () in
  let result = node_to_compact node in
  check bool "contains Visible" true
    (String.length (Str.global_replace (Str.regexp "Visible") "" result) < String.length result);
  check bool "not contains Hidden" true
    (String.length (Str.global_replace (Str.regexp "Hidden") "" result) = String.length result)

let test_node_to_compact_image () =
  let node = make_node ~node_type:Instance ~name:"image_profile" ~id:"img:123"
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 100. }) () in
  let result = node_to_compact node in
  check bool "contains I(" true (String.sub result 0 2 = "I(")

let test_node_to_compact_divider () =
  (* Thin rectangle with height <= 2 is classified as divider *)
  let node = make_node ~node_type:Rectangle
    ~bbox:(Some { x = 0.; y = 0.; width = 300.; height = 1. }) () in
  let result = node_to_compact node in
  check string "divider is ---" "---" result

let compact_dsl_tests = [
  "text node", `Quick, test_node_to_compact_text;
  "button with label", `Quick, test_node_to_compact_button;
  "frame row layout", `Quick, test_node_to_compact_frame_row;
  "frame col layout", `Quick, test_node_to_compact_frame_col;
  "frame with children", `Quick, test_node_to_compact_with_children;
  "hidden children filtered", `Quick, test_node_to_compact_hidden_children;
  "image node", `Quick, test_node_to_compact_image;
  "divider node", `Quick, test_node_to_compact_divider;
]

(** ============== 8. Style Registry Tests ============== *)

let test_style_registry_create () =
  let reg = StyleRegistry.create () in
  check int "empty colors" 0 (Hashtbl.length reg.colors);
  check int "threshold default 2" 2 reg.threshold

let test_style_registry_counter () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_color counter "#FF0000";
  StyleRegistry.count_color counter "#FF0000";
  StyleRegistry.count_color counter "#00FF00";
  let freq = Hashtbl.find counter.color_freq "#FF0000" in
  check int "red counted twice" 2 freq

let test_style_registry_finalize () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_color counter "#FF0000";
  StyleRegistry.count_color counter "#FF0000";
  StyleRegistry.count_color counter "#00FF00"; (* only once - should not be variable *)
  let reg = StyleRegistry.finalize counter 2 in
  check int "one color variable" 1 (Hashtbl.length reg.colors);
  check bool "red is variablized" true (Hashtbl.mem reg.colors "#FF0000")

let test_style_registry_lookup () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_color counter "#FF0000";
  StyleRegistry.count_color counter "#FF0000";
  let reg = StyleRegistry.finalize counter 2 in
  let looked_up = StyleRegistry.lookup_color reg "#FF0000" in
  check bool "starts with $c" true (String.sub looked_up 0 2 = "$c");
  let not_var = StyleRegistry.lookup_color reg "#00FF00" in
  check string "unknown returns original" "#00FF00" not_var

let test_style_registry_to_defs_empty () =
  let reg = StyleRegistry.create () in
  let defs = StyleRegistry.to_defs reg in
  check string "empty defs" "" defs

let test_style_registry_to_defs_with_vars () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_color counter "#AABBCC";
  StyleRegistry.count_color counter "#AABBCC";
  let reg = StyleRegistry.finalize counter 2 in
  let defs = StyleRegistry.to_defs reg in
  check bool "contains @vars" true (String.sub defs 0 5 = "@vars")

let style_registry_tests = [
  "create empty", `Quick, test_style_registry_create;
  "counter counting", `Quick, test_style_registry_counter;
  "finalize threshold", `Quick, test_style_registry_finalize;
  "lookup variable", `Quick, test_style_registry_lookup;
  "to_defs empty", `Quick, test_style_registry_to_defs_empty;
  "to_defs with vars", `Quick, test_style_registry_to_defs_with_vars;
]

(** ============== 9. Typography CSS Tests ============== *)

let test_typography_basic () =
  let typo = { default_typography with font_size = 16.; font_weight = 400 } in
  let css = typography_to_css typo in
  check bool "has font-size" true
    (String.length (Str.global_replace (Str.regexp "font-size:16px") "" css) < String.length css);
  check bool "has font-family" true
    (String.length (Str.global_replace (Str.regexp "font-family") "" css) < String.length css)

let test_typography_bold () =
  let typo = { default_typography with font_weight = 700 } in
  let css = typography_to_css typo in
  check bool "has font-weight:700" true
    (String.length (Str.global_replace (Str.regexp "font-weight:700") "" css) < String.length css)

let test_typography_center () =
  let typo = { default_typography with text_align_h = Center } in
  let css = typography_to_css typo in
  check bool "has text-align:center" true
    (String.length (Str.global_replace (Str.regexp "text-align:center") "" css) < String.length css)

let test_typography_underline () =
  let typo = { default_typography with text_decoration = Underline } in
  let css = typography_to_css typo in
  check bool "has text-decoration:underline" true
    (String.length (Str.global_replace (Str.regexp "text-decoration:underline") "" css) < String.length css)

let test_typography_uppercase () =
  let typo = { default_typography with text_case = Upper } in
  let css = typography_to_css typo in
  check bool "has text-transform:uppercase" true
    (String.length (Str.global_replace (Str.regexp "text-transform:uppercase") "" css) < String.length css)

let test_typography_line_height () =
  let typo = { default_typography with line_height = Some 24. } in
  let css = typography_to_css typo in
  check bool "has line-height:24px" true
    (String.length (Str.global_replace (Str.regexp "line-height:24px") "" css) < String.length css)

let typography_css_tests = [
  "basic typography", `Quick, test_typography_basic;
  "bold weight", `Quick, test_typography_bold;
  "center align", `Quick, test_typography_center;
  "underline", `Quick, test_typography_underline;
  "uppercase", `Quick, test_typography_uppercase;
  "line height", `Quick, test_typography_line_height;
]

(** ============== 10. Style to CSS Tests ============== *)

let test_style_to_css_size () =
  let node = make_node ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 100. }) () in
  let css = style_to_css node in
  check bool "has width:200px" true
    (String.length (Str.global_replace (Str.regexp "width:200px") "" css) < String.length css);
  check bool "has height:100px" true
    (String.length (Str.global_replace (Str.regexp "height:100px") "" css) < String.length css)

let test_style_to_css_background () =
  let node = make_node ~fills:[make_paint Solid (Some (make_rgba 1.0 0.0 0.0))] () in
  let css = style_to_css node in
  check bool "has background" true
    (String.length (Str.global_replace (Str.regexp "background") "" css) < String.length css)

let test_style_to_css_border_radius () =
  let node = make_node ~border_radius:12. () in
  let css = style_to_css node in
  check bool "has border-radius:12px" true
    (String.length (Str.global_replace (Str.regexp "border-radius:12px") "" css) < String.length css)

let test_style_to_css_opacity () =
  let node = make_node ~opacity:0.5 () in
  let css = style_to_css node in
  check bool "has opacity:0.50" true
    (String.length (Str.global_replace (Str.regexp "opacity:0.50") "" css) < String.length css)

let test_style_to_css_padding () =
  let node = make_node ~padding:(16., 8., 16., 8.) () in
  let css = style_to_css node in
  check bool "has padding" true
    (String.length (Str.global_replace (Str.regexp "padding") "" css) < String.length css)

let style_to_css_tests = [
  "size in CSS", `Quick, test_style_to_css_size;
  "background in CSS", `Quick, test_style_to_css_background;
  "border radius", `Quick, test_style_to_css_border_radius;
  "opacity", `Quick, test_style_to_css_opacity;
  "padding", `Quick, test_style_to_css_padding;
]

(** ============== 11. HTML Generation Tests ============== *)

let test_node_to_html_text () =
  let node = make_node ~node_type:Text ~characters:(Some "Hello World")
    ~typography:(Some default_typography) () in
  let html = node_to_html node in
  check bool "has span tag" true (String.sub html 0 5 = "<span");
  check bool "contains text" true
    (String.length (Str.global_replace (Str.regexp "Hello World") "" html) < String.length html)

let test_node_to_html_button () =
  let text_child = make_node ~node_type:Text ~characters:(Some "Click Me") () in
  let node = make_node ~name:"button_test" ~children:[text_child] () in
  let html = node_to_html node in
  check bool "has button tag" true
    (String.length (Str.global_replace (Str.regexp "<button") "" html) < String.length html)

let test_node_to_html_frame () =
  let node = make_node ~layout_mode:Horizontal () in
  let html = node_to_html node in
  check bool "has div tag" true (String.sub html 0 4 = "<div");
  check bool "has display:flex" true
    (String.length (Str.global_replace (Str.regexp "display:flex") "" html) < String.length html)

let test_node_to_html_vertical () =
  let node = make_node ~layout_mode:Vertical () in
  let html = node_to_html node in
  check bool "has flex-direction:column" true
    (String.length (Str.global_replace (Str.regexp "flex-direction:column") "" html) < String.length html)

let test_node_to_html_gap () =
  let node = make_node ~layout_mode:Horizontal ~gap:16. () in
  let html = node_to_html node in
  check bool "has gap:16px" true
    (String.length (Str.global_replace (Str.regexp "gap:16px") "" html) < String.length html)

let test_generate_html_wrapper () =
  let node = make_node () in
  let html = generate_html node in
  check bool "has DOCTYPE" true (String.sub html 0 9 = "<!DOCTYPE");
  check bool "has body" true
    (String.length (Str.global_replace (Str.regexp "<body>") "" html) < String.length html)

let html_generation_tests = [
  "text as span", `Quick, test_node_to_html_text;
  "button element", `Quick, test_node_to_html_button;
  "frame as flex div", `Quick, test_node_to_html_frame;
  "vertical flex", `Quick, test_node_to_html_vertical;
  "gap property", `Quick, test_node_to_html_gap;
  "full HTML wrapper", `Quick, test_generate_html_wrapper;
]

(** ============== 12. Generator Function Tests ============== *)

let test_generate_compact () =
  let node = make_node () in
  let result = generate_compact node in
  check bool "returns string" true (String.length result > 0)

let test_generate_verbose () =
  let node = make_node ~layout_mode:Horizontal
    ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 50. }) () in
  let result = generate_verbose node in
  check bool "contains F(" true (String.sub result 0 2 = "F(")

let test_generate_compact_memoized () =
  let red_paint = make_paint Solid (Some (make_rgba 1.0 0.0 0.0)) in
  let child1 = make_node ~fills:[red_paint] ~name:"child1" () in
  let child2 = make_node ~fills:[red_paint] ~name:"child2" () in
  let node = make_node ~children:[child1; child2] () in
  let result = generate_compact_memoized ~threshold:2 node in
  (* With repeated colors, should have @vars header *)
  check bool "non-empty result" true (String.length result > 0)

let test_generate_compact_smart_small () =
  let node = make_node () in
  let result = generate_compact_smart node in
  check bool "small tree returns result" true (String.length result > 0)

let test_generate_compact_smart_large () =
  (* Create tree with >20 nodes *)
  let leaf = make_node ~name:"leaf" () in
  let children = List.init 25 (fun i ->
    make_node ~name:(Printf.sprintf "child_%d" i) ~children:[leaf] ()) in
  let node = make_node ~children () in
  let result = generate_compact_smart node in
  check bool "large tree returns result" true (String.length result > 0)

let generator_tests = [
  "generate_compact", `Quick, test_generate_compact;
  "generate_verbose", `Quick, test_generate_verbose;
  "generate_compact_memoized", `Quick, test_generate_compact_memoized;
  "generate_compact_smart small", `Quick, test_generate_compact_smart_small;
  "generate_compact_smart large", `Quick, test_generate_compact_smart_large;
]

(** ============== 13. Compression Analysis Tests ============== *)

let test_analyze_compression () =
  let node = make_node ~layout_mode:Horizontal
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 100. }) () in
  let original_json = "{\"type\":\"FRAME\",\"layoutMode\":\"HORIZONTAL\",\"absoluteBoundingBox\":{\"x\":0,\"y\":0,\"width\":200,\"height\":100}}" in
  let stats = analyze_compression node original_json in
  check bool "original_chars > 0" true (stats.original_chars > 0);
  check bool "compact_chars > 0" true (stats.compact_chars > 0);
  check bool "compression_ratio positive" true (stats.compression_ratio > 0.)

let test_format_stats () =
  let stats = { original_chars = 1000; compact_chars = 200; verbose_chars = 400; compression_ratio = 0.8 } in
  let formatted = format_stats stats in
  check bool "contains Original JSON" true
    (String.length (Str.global_replace (Str.regexp "Original JSON") "" formatted) < String.length formatted);
  check bool "contains savings" true
    (String.length (Str.global_replace (Str.regexp "Token savings") "" formatted) < String.length formatted)

let compression_tests = [
  "analyze_compression", `Quick, test_analyze_compression;
  "format_stats", `Quick, test_format_stats;
]

(** ============== 14. Screen Extraction Tests ============== *)

let test_extract_screens_document () =
  let screen1 = make_node ~id:"s1" ~name:"Home" ~node_type:Frame () in
  let screen2 = make_node ~id:"s2" ~name:"Profile" ~node_type:Frame () in
  let doc = make_node ~node_type:Document ~children:[screen1; screen2] () in
  let screens = extract_screens doc in
  check int "2 screens" 2 (List.length screens);
  check string "first is Home" "Home" (fst (List.hd screens))

let test_extract_screens_single () =
  let node = make_node ~id:"n1" ~name:"Single" () in
  let screens = extract_screens node in
  check int "1 screen" 1 (List.length screens);
  check string "name is Single" "Single" (fst (List.hd screens))

let test_split_to_components () =
  let comp1 = make_node ~name:"Header" () in
  let comp2 = make_node ~name:"Footer" () in
  let doc = make_node ~node_type:Document ~children:[comp1; comp2] () in
  let result = split_to_components doc in
  check bool "contains Header" true
    (String.length (Str.global_replace (Str.regexp "Header") "" result) < String.length result);
  check bool "contains Footer" true
    (String.length (Str.global_replace (Str.regexp "Footer") "" result) < String.length result)

let screen_extraction_tests = [
  "extract from Document", `Quick, test_extract_screens_document;
  "extract from single", `Quick, test_extract_screens_single;
  "split_to_components", `Quick, test_split_to_components;
]

(** ============== 15. JSON Utility Tests ============== *)

let test_json_member_found () =
  let json = `Assoc [("name", `String "Test"); ("id", `String "123")] in
  let result = json_member "name" json in
  check bool "found name" true (Option.is_some result);
  match result with
  | Some (`String s) -> check string "value is Test" "Test" s
  | _ -> fail "expected String"

let test_json_member_not_found () =
  let json = `Assoc [("name", `String "Test")] in
  let result = json_member "missing" json in
  check bool "missing returns None" true (Option.is_none result)

let test_json_has_key () =
  let json = `Assoc [("name", `String "Test")] in
  check bool "has name" true (json_has_key "name" json);
  check bool "no missing" false (json_has_key "missing" json)

let test_json_pick_keys () =
  let json = `Assoc [("a", `Int 1); ("b", `Int 2); ("c", `Int 3)] in
  let picked = json_pick_keys ["a"; "c"; "d"] json in
  check int "picked 2 keys" 2 (List.length picked)

let test_json_missing_keys () =
  let json = `Assoc [("a", `Int 1)] in
  let missing = json_missing_keys ["a"; "b"; "c"] json in
  check int "missing 2 keys" 2 (List.length missing);
  check bool "b is missing" true (List.mem "b" missing);
  check bool "c is missing" true (List.mem "c" missing)

let test_json_list_of_strings () =
  let result = json_list_of_strings ["one"; "two"; "three"] in
  match result with
  | `List items -> check int "3 items" 3 (List.length items)
  | _ -> fail "expected List"

let test_json_has_any_key () =
  let json = `Assoc [("name", `String "Test")] in
  check bool "has any [name, id]" true (json_has_any_key ["name"; "id"] json);
  check bool "not has any [missing]" false (json_has_any_key ["missing"; "other"] json)

let test_json_collect_image_refs () =
  let json = `Assoc [
    ("fills", `List [
      `Assoc [("imageRef", `String "img:123")];
      `Assoc [("imageRef", `String "img:456")]
    ])
  ] in
  let refs = json_collect_image_refs json in
  check int "2 image refs" 2 (List.length refs);
  check bool "has img:123" true (List.mem "img:123" refs)

let json_utility_tests = [
  "json_member found", `Quick, test_json_member_found;
  "json_member not found", `Quick, test_json_member_not_found;
  "json_has_key", `Quick, test_json_has_key;
  "json_pick_keys", `Quick, test_json_pick_keys;
  "json_missing_keys", `Quick, test_json_missing_keys;
  "json_list_of_strings", `Quick, test_json_list_of_strings;
  "json_has_any_key", `Quick, test_json_has_any_key;
  "json_collect_image_refs", `Quick, test_json_collect_image_refs;
]

(** ============== 16. Fidelity DSL Tests ============== *)

let test_node_to_fidelity_text () =
  let node = make_node ~node_type:Text ~characters:(Some "Fidelity Text")
    ~typography:(Some { default_typography with font_size = 20. })
    ~bbox:(Some { x = 10.; y = 20.; width = 100.; height = 30. }) () in
  let result = node_to_fidelity ~is_root:true node in
  check bool "starts with T" true (String.sub result 0 1 = "T");
  check bool "has size attr" true
    (String.length (Str.global_replace (Str.regexp "sz:") "" result) < String.length result)

let test_node_to_fidelity_frame () =
  let node = make_node ~layout_mode:Vertical
    ~bbox:(Some { x = 0.; y = 0.; width = 300.; height = 400. })
    ~gap:12. ~padding:(16., 16., 16., 16.) () in
  let result = node_to_fidelity ~is_root:true node in
  check bool "starts with F" true (String.sub result 0 1 = "F");
  check bool "has ax:" true
    (String.length (Str.global_replace (Str.regexp "ax:") "" result) < String.length result)

let test_node_to_fidelity_with_origin () =
  let child = make_node ~bbox:(Some { x = 50.; y = 60.; width = 80.; height = 40. }) () in
  let parent = make_node ~children:[child]
    ~bbox:(Some { x = 10.; y = 20.; width = 200.; height = 300. }) () in
  let result = node_to_fidelity ~is_root:true parent in
  check bool "has xy positions" true
    (String.length (Str.global_replace (Str.regexp "xy:") "" result) < String.length result)

let test_extract_root_node () =
  let doc = `Assoc [
    ("document", `Assoc [("type", `String "DOCUMENT")])
  ] in
  let root = extract_root_node doc in
  match json_member "type" root with
  | Some (`String t) -> check string "type is DOCUMENT" "DOCUMENT" t
  | _ -> fail "expected type DOCUMENT"

let test_generate_fidelity () =
  let json = `Assoc [
    ("document", `Assoc [
      ("type", `String "FRAME");
      ("id", `String "1:2");
      ("name", `String "TestFrame")
    ])
  ] in
  let result = generate_fidelity json in
  check bool "non-empty result" true (String.length result > 0)

let fidelity_tests = [
  "fidelity text node", `Quick, test_node_to_fidelity_text;
  "fidelity frame node", `Quick, test_node_to_fidelity_frame;
  "fidelity with origin", `Quick, test_node_to_fidelity_with_origin;
  "extract_root_node", `Quick, test_extract_root_node;
  "generate_fidelity", `Quick, test_generate_fidelity;
]

(** ============== 17. Flat HTML Tests ============== *)

let test_generate_flat_html () =
  let text = make_node ~node_type:Text ~characters:(Some "Button")
    ~typography:(Some default_typography)
    ~fills:[make_paint Solid (Some (make_rgba 1.0 1.0 1.0))] () in
  let inner = make_node ~name:"inner"
    ~fills:[make_paint Solid (Some (make_rgba 0.0 0.478 1.0))]
    ~border_radius:8. ~children:[text]
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 48. }) () in
  let outer = make_node ~name:"outer" ~children:[inner]
    ~bbox:(Some { x = 0.; y = 0.; width = 200.; height = 48. }) () in
  let html = generate_flat_html outer in
  check bool "has DOCTYPE" true (String.sub html 0 9 = "<!DOCTYPE");
  check bool "has Button text" true
    (String.length (Str.global_replace (Str.regexp "Button") "" html) < String.length html)

let flat_html_tests = [
  "generate_flat_html", `Quick, test_generate_flat_html;
]

(** ============== 18. Add XY Size Attrs Tests ============== *)

let test_add_xy_size_attrs_with_origin () =
  let node = make_node ~bbox:(Some { x = 100.; y = 150.; width = 80.; height = 40. }) () in
  let attrs = add_xy_size_attrs ~origin:(50., 100.) [] node in
  check int "2 attrs added" 2 (List.length attrs)

let test_add_xy_size_attrs_root () =
  let node = make_node ~bbox:(Some { x = 0.; y = 0.; width = 100.; height = 50. }) () in
  let attrs = add_xy_size_attrs ~is_root:true [] node in
  (* Root should not have xy, only sz *)
  check int "1 attr (sz only)" 1 (List.length attrs)

let test_add_xy_size_attrs_no_bbox () =
  let node = make_node () in
  let attrs = add_xy_size_attrs [] node in
  check int "no attrs without bbox" 0 (List.length attrs)

let xy_size_tests = [
  "with origin offset", `Quick, test_add_xy_size_attrs_with_origin;
  "root skips xy", `Quick, test_add_xy_size_attrs_root;
  "no bbox no attrs", `Quick, test_add_xy_size_attrs_no_bbox;
]

(** ============== Run All Tests ============== *)

let () =
  run "Figma Codegen Coverage" [
    "1. Number Formatting", number_formatting_tests;
    "2. Background Color", bg_color_tests;
    "3. Stroke Color", stroke_color_tests;
    "4. Padding Formatting", padding_tests;
    "5. Code Conversions", code_conversion_tests;
    "6. Node Classification", classification_tests;
    "7. Compact DSL", compact_dsl_tests;
    "8. Style Registry", style_registry_tests;
    "9. Typography CSS", typography_css_tests;
    "10. Style to CSS", style_to_css_tests;
    "11. HTML Generation", html_generation_tests;
    "12. Generator Functions", generator_tests;
    "13. Compression Analysis", compression_tests;
    "14. Screen Extraction", screen_extraction_tests;
    "15. JSON Utilities", json_utility_tests;
    "16. Fidelity DSL", fidelity_tests;
    "17. Flat HTML", flat_html_tests;
    "18. XY Size Attrs", xy_size_tests;
  ]
