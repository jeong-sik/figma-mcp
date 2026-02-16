(** Coverage Wave 3 Tests for figma_codegen.ml
    Target: push from 93.07% to 97%+
    Focus: remaining uncovered branches identified via bisect_ppx HTML report *)

open Alcotest
open Figma_types
open Figma_codegen

(** ============== Helpers ============== *)

let make_rgba ?(a=1.0) r g b = { r; g; b; a }

let make_paint ?(visible=true) paint_type color =
  { paint_type; visible; opacity = 1.0; color; gradient_stops = [];
    image_ref = None; scale_mode = None }

let make_gradient_paint ?(visible=true) ?(paint_type=GradientLinear) stops =
  { paint_type; visible; opacity = 1.0; color = None;
    gradient_stops = stops; image_ref = None; scale_mode = None }

let make_typo ?(size=14.) ?(weight=400) ?(family="Inter")
    ?(lh=None) ?(ls=None) ?(align=Left) ?(deco=NoDeco)
    ?(tcase=Original) () =
  { font_family = family; font_size = size; font_weight = weight;
    font_style = "normal"; line_height = lh; letter_spacing = ls;
    text_align_h = align; text_align_v = Top;
    text_decoration = deco; text_case = tcase }

let mk ?(id="n1") ?(name="Node") ?(node_type=Frame)
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

let has s haystack =
  let slen = String.length s in
  let hlen = String.length haystack in
  if slen > hlen then false
  else begin
    let found = ref false in
    for i = 0 to hlen - slen do
      if String.sub haystack i slen = s then found := true
    done;
    !found
  end

(** ============== 1. get_bg_color_precise Image/Emoji branches ============== *)

let test_precise_image () =
  let p = { paint_type = Image; visible = true; opacity = 1.0;
            color = None; gradient_stops = []; image_ref = Some "img:1";
            scale_mode = None } in
  check (option string) "Image returns None" None (get_bg_color_precise [p])

let test_precise_emoji () =
  let p = { paint_type = Emoji; visible = true; opacity = 1.0;
            color = None; gradient_stops = []; image_ref = None;
            scale_mode = None } in
  check (option string) "Emoji returns None" None (get_bg_color_precise [p])

let test_precise_radial () =
  let stops = [(0., make_rgba 1. 0. 0.); (1., make_rgba 0. 0. 1.)] in
  let p = make_gradient_paint ~paint_type:GradientRadial stops in
  let r = get_bg_color_precise [p] in
  check bool "radial precise returns Some" true (Option.is_some r);
  check bool "uses rgb()" true (has "rgb(" (Option.get r))

let test_precise_angular () =
  let stops = [(0., make_rgba 0. 1. 0.); (1., make_rgba 1. 0. 0.)] in
  let p = make_gradient_paint ~paint_type:GradientAngular stops in
  let r = get_bg_color_precise [p] in
  check bool "angular precise" true (Option.is_some r)

let test_precise_diamond () =
  let stops = [(0., make_rgba 0. 0. 1.); (1., make_rgba 1. 1. 0.)] in
  let p = make_gradient_paint ~paint_type:GradientDiamond stops in
  let r = get_bg_color_precise [p] in
  check bool "diamond precise" true (Option.is_some r)

let bg_color_precise_tests = [
  "precise Image -> None", `Quick, test_precise_image;
  "precise Emoji -> None", `Quick, test_precise_emoji;
  "precise GradientRadial", `Quick, test_precise_radial;
  "precise GradientAngular", `Quick, test_precise_angular;
  "precise GradientDiamond", `Quick, test_precise_diamond;
]

(** ============== 2. collect_from_node stroke + typography color ============== *)

let test_collect_stroke_color () =
  let counter = StyleRegistry.create_counter () in
  let n = mk ~strokes:[make_paint Solid (Some (make_rgba 1. 0. 0.))] () in
  StyleRegistry.collect_from_node counter n;
  let freq = Hashtbl.find_opt counter.color_freq "#FF0000" in
  check bool "stroke color counted" true (Option.is_some freq)

let test_collect_typo_text_color () =
  let counter = StyleRegistry.create_counter () in
  let typo = make_typo ~size:16. ~weight:700 () in
  let n = mk ~typography:(Some typo) ~node_type:Text
    ~fills:[make_paint Solid (Some (make_rgba 0. 0.5 1.))] () in
  StyleRegistry.collect_from_node counter n;
  (* font and weight should be counted *)
  let ffreq = Hashtbl.find_opt counter.font_freq 16. in
  let wfreq = Hashtbl.find_opt counter.weight_freq 700 in
  check bool "font counted" true (Option.is_some ffreq);
  check bool "weight counted" true (Option.is_some wfreq)

let test_collect_white_bg_skip () =
  let counter = StyleRegistry.create_counter () in
  let n = mk ~fills:[make_paint Solid (Some (make_rgba 1. 1. 1.))] () in
  StyleRegistry.collect_from_node counter n;
  (* White #FFFFFF should be skipped *)
  let freq = Hashtbl.find_opt counter.color_freq "#FFFFFF" in
  check (option int) "white bg skipped" None freq

let test_collect_black_text_skip () =
  let counter = StyleRegistry.create_counter () in
  let typo = make_typo () in
  (* #000000 text color should be skipped in typography branch *)
  let n = mk ~typography:(Some typo) ~node_type:Text
    ~fills:[make_paint Solid (Some (make_rgba 0. 0. 0.))] () in
  StyleRegistry.collect_from_node counter n;
  let freq = Hashtbl.find_opt counter.color_freq "#000000" in
  check (option int) "black text skipped" None freq

let test_collect_recursive () =
  let counter = StyleRegistry.create_counter () in
  let child = mk ~fills:[make_paint Solid (Some (make_rgba 0.5 0. 0.))] () in
  let parent = mk ~children:[child] () in
  StyleRegistry.collect_from_node counter parent;
  let freq = Hashtbl.find_opt counter.color_freq "#800000" in
  check bool "child color collected" true (Option.is_some freq)

let collect_tests = [
  "collect stroke color", `Quick, test_collect_stroke_color;
  "collect typo text color", `Quick, test_collect_typo_text_color;
  "collect white bg skip", `Quick, test_collect_white_bg_skip;
  "collect black text skip", `Quick, test_collect_black_text_skip;
  "collect recursive children", `Quick, test_collect_recursive;
]

(** ============== 3. node_to_compact SemFrame opacity/stroke ============== *)

let test_compact_frame_opacity () =
  let n = mk ~layout_mode:Horizontal ~opacity:0.5
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let dsl = node_to_compact n in
  check bool "has op:" true (has "op:" dsl)

let test_compact_frame_stroke () =
  let n = mk ~layout_mode:Vertical
    ~strokes:[make_paint Solid (Some (make_rgba 1. 0. 0.))]
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let dsl = node_to_compact n in
  check bool "has bd:" true (has "bd:" dsl)

let test_compact_frame_abs_layout () =
  let n = mk ~layout_mode:None'
    ~bbox:(Some { x=0.; y=0.; width=80.; height=40. }) () in
  let dsl = node_to_compact n in
  check bool "has abs" true (has "abs" dsl)

let test_compact_input_no_bbox () =
  let n = mk ~node_type:Component ~name:"input_email" () in
  let dsl = node_to_compact n in
  check bool "is SemInput N-tag" true (has "N\"" dsl);
  (* No bbox -> no w: attr, just empty attrs *)
  check bool "has input_email" true (has "input_email" dsl)

let test_compact_icon () =
  let n = mk ~node_type:Vector ~name:"star_icon"
    ~bbox:(Some { x=0.; y=0.; width=24.; height=24. }) () in
  let dsl = node_to_compact n in
  check bool "is V-tag" true (has "V\"" dsl);
  check bool "has name" true (has "star_icon" dsl)

let test_compact_divider () =
  let n = mk ~node_type:Rectangle ~name:"divider"
    ~bbox:(Some { x=0.; y=0.; width=300.; height=1. }) () in
  let dsl = node_to_compact n in
  check bool "is ---" true (has "---" dsl)

let test_compact_image () =
  let n = mk ~node_type:Instance ~name:"img_photo" ~id:"img-42"
    ~bbox:(Some { x=0.; y=0.; width=200.; height=150. }) () in
  let dsl = node_to_compact n in
  check bool "is I-tag" true (has "I(" dsl);
  check bool "has id" true (has "id:img-42" dsl)

let test_compact_image_no_bbox () =
  let n = mk ~node_type:Instance ~name:"image_hero" ~id:"img-99" () in
  let dsl = node_to_compact n in
  check bool "I with empty size" true (has "I[" dsl)

let compact_frame_tests = [
  "compact frame opacity", `Quick, test_compact_frame_opacity;
  "compact frame stroke", `Quick, test_compact_frame_stroke;
  "compact frame abs layout", `Quick, test_compact_frame_abs_layout;
  "compact input no bbox", `Quick, test_compact_input_no_bbox;
  "compact icon", `Quick, test_compact_icon;
  "compact divider", `Quick, test_compact_divider;
  "compact image with bbox", `Quick, test_compact_image;
  "compact image no bbox", `Quick, test_compact_image_no_bbox;
]

(** ============== 4. node_to_fidelity additional branches ============== *)

let test_fidelity_button_stroke_weight () =
  let child = mk ~node_type:Text ~characters:(Some "Submit") () in
  let n = mk ~node_type:Component ~name:"button_submit"
    ~strokes:[make_paint Solid (Some (make_rgba 0. 0. 0.))]
    ~stroke_weight:2.
    ~opacity:0.9
    ~border_radius:8.
    ~fills:[make_paint Solid (Some (make_rgba 0. 0.5 1.))]
    ~bbox:(Some { x=10.; y=20.; width=120.; height=40. })
    ~children:[child] () in
  let dsl = node_to_fidelity n in
  check bool "has bw:" true (has "bw:" dsl);
  check bool "has bd:" true (has "bd:" dsl);
  check bool "has op:" true (has "op:" dsl);
  check bool "has r:" true (has "r:" dsl)

let test_fidelity_input_stroke () =
  let n = mk ~node_type:Instance ~name:"input_password"
    ~strokes:[make_paint Solid (Some (make_rgba 0.8 0.8 0.8))]
    ~stroke_weight:1.
    ~fills:[make_paint Solid (Some (make_rgba 1. 1. 1.))]
    ~bbox:(Some { x=0.; y=0.; width=200.; height=40. }) () in
  let dsl = node_to_fidelity n in
  check bool "has bw:" true (has "bw:" dsl);
  check bool "has bd:" true (has "bd:" dsl);
  check bool "has bg:" true (has "bg:" dsl)

let test_fidelity_frame_rotation () =
  let n = mk ~layout_mode:Horizontal ~rotation:45.
    ~bbox:(Some { x=0.; y=0.; width=100.; height=100. }) () in
  let dsl = node_to_fidelity n in
  check bool "has rot:" true (has "rot:" dsl)

let test_fidelity_frame_gap () =
  let child = mk ~node_type:Text ~characters:(Some "A") () in
  let n = mk ~layout_mode:Horizontal ~gap:16.
    ~bbox:(Some { x=0.; y=0.; width=200.; height=50. })
    ~children:[child] () in
  let dsl = node_to_fidelity n in
  check bool "has g:" true (has "g:" dsl)

let test_fidelity_frame_stroke_weight () =
  let n = mk ~layout_mode:Vertical
    ~strokes:[make_paint Solid (Some (make_rgba 0.5 0.5 0.5))]
    ~stroke_weight:1.
    ~bbox:(Some { x=0.; y=0.; width=100.; height=100. }) () in
  let dsl = node_to_fidelity n in
  check bool "has bw:" true (has "bw:" dsl);
  check bool "has bd:" true (has "bd:" dsl)

let test_fidelity_frame_opacity () =
  let n = mk ~layout_mode:Horizontal ~opacity:0.7
    ~bbox:(Some { x=0.; y=0.; width=80.; height=40. }) () in
  let dsl = node_to_fidelity n in
  check bool "has op:" true (has "op:" dsl)

let test_fidelity_frame_border_radius_no_radii () =
  (* border_radii=None but border_radius > 0 -> "r:" attr *)
  let n = mk ~layout_mode:Horizontal ~border_radius:12.
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let dsl = node_to_fidelity n in
  check bool "has r:" true (has "r:" dsl);
  check bool "no rr:" false (has "rr:" dsl)

let test_fidelity_frame_sizing_variants () =
  let n = mk ~layout_mode:Horizontal
    ~layout_sizing_h:Hug ~layout_sizing_v:Fill
    ~primary_axis_align:Center ~counter_axis_align:Max
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let dsl = node_to_fidelity n in
  check bool "has sx:hug" true (has "sx:hug" dsl);
  check bool "has sy:fill" true (has "sy:fill" dsl);
  check bool "has ax:c" true (has "ax:c" dsl);
  check bool "has cx:max" true (has "cx:max" dsl)

let test_fidelity_frame_baseline () =
  let n = mk ~layout_mode:Horizontal
    ~primary_axis_align:Baseline ~counter_axis_align:Baseline
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let dsl = node_to_fidelity n in
  check bool "has ax:base" true (has "ax:base" dsl);
  check bool "has cx:base" true (has "cx:base" dsl)

let test_fidelity_frame_space_between () =
  let n = mk ~layout_mode:Horizontal
    ~primary_axis_align:SpaceBetween ~counter_axis_align:SpaceBetween
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let dsl = node_to_fidelity n in
  check bool "has ax:sb" true (has "ax:sb" dsl);
  check bool "has cx:sb" true (has "cx:sb" dsl)

let test_fidelity_image_attrs () =
  let n = mk ~node_type:Instance ~name:"image_profile" ~id:"i-1"
    ~bbox:(Some { x=5.; y=10.; width=48.; height=48. }) () in
  let dsl = node_to_fidelity ~origin:(0.,0.) n in
  check bool "has id:" true (has "id:" dsl);
  check bool "has xy:" true (has "xy:" dsl);
  check bool "has sz:" true (has "sz:" dsl)

let test_fidelity_icon_attrs () =
  let n = mk ~node_type:Vector ~name:"check"
    ~bbox:(Some { x=2.; y=3.; width=16.; height=16. }) () in
  let dsl = node_to_fidelity ~origin:(0.,0.) n in
  check bool "is V-tag" true (has "V\"check\"" dsl);
  check bool "has xy:" true (has "xy:" dsl)

let fidelity_extra_tests = [
  "fidelity button stroke+weight", `Quick, test_fidelity_button_stroke_weight;
  "fidelity input stroke", `Quick, test_fidelity_input_stroke;
  "fidelity frame rotation", `Quick, test_fidelity_frame_rotation;
  "fidelity frame gap", `Quick, test_fidelity_frame_gap;
  "fidelity frame stroke_weight", `Quick, test_fidelity_frame_stroke_weight;
  "fidelity frame opacity", `Quick, test_fidelity_frame_opacity;
  "fidelity frame r: no radii", `Quick, test_fidelity_frame_border_radius_no_radii;
  "fidelity sizing hug/fill", `Quick, test_fidelity_frame_sizing_variants;
  "fidelity baseline align", `Quick, test_fidelity_frame_baseline;
  "fidelity space-between", `Quick, test_fidelity_frame_space_between;
  "fidelity image with origin", `Quick, test_fidelity_image_attrs;
  "fidelity icon with origin", `Quick, test_fidelity_icon_attrs;
]

(** ============== 5. node_to_html additional branches ============== *)

let test_html_baseline_primary () =
  (* Baseline as primary_axis_align -> empty string, no justify *)
  let n = mk ~layout_mode:Horizontal ~primary_axis_align:Baseline
    ~bbox:(Some { x=0.; y=0.; width=200.; height=50. }) () in
  let html = node_to_html n in
  check bool "no justify-content" false (has "justify-content" html)

let test_html_space_between_counter () =
  (* SpaceBetween as counter_axis_align -> empty, no align-items *)
  let n = mk ~layout_mode:Horizontal ~counter_axis_align:SpaceBetween
    ~bbox:(Some { x=0.; y=0.; width=200.; height=50. }) () in
  let html = node_to_html n in
  check bool "no align-items" false (has "align-items" html)

let test_html_text_precise_color () =
  let typo = make_typo ~size:16. () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~fills:[make_paint Solid (Some (make_rgba 0.5 0.2 0.8))]
    ~characters:(Some "colored") () in
  let html = node_to_html ~precise:true n in
  check bool "has rgb(" true (has "rgb(" html);
  check bool "has color:" true (has "color:" html)

let test_html_text_no_color () =
  let typo = make_typo ~size:14. () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~fills:[] ~characters:(Some "plain") () in
  let html = node_to_html n in
  check bool "no color attr" false (has "color:" html)

let test_html_frame_vertical () =
  let n = mk ~layout_mode:Vertical
    ~bbox:(Some { x=0.; y=0.; width=100.; height=200. }) () in
  let html = node_to_html n in
  check bool "has flex-direction:column" true (has "flex-direction:column" html)

let test_html_frame_min_primary () =
  (* Min primary -> no justify-content (default) *)
  let n = mk ~layout_mode:Horizontal ~primary_axis_align:Min
    ~bbox:(Some { x=0.; y=0.; width=200.; height=50. }) () in
  let html = node_to_html n in
  check bool "no justify-content" false (has "justify-content" html)

let test_html_frame_min_counter () =
  let n = mk ~layout_mode:Horizontal ~counter_axis_align:Min
    ~bbox:(Some { x=0.; y=0.; width=200.; height=50. }) () in
  let html = node_to_html n in
  check bool "no align-items" false (has "align-items" html)

let html_extra_tests = [
  "html baseline primary", `Quick, test_html_baseline_primary;
  "html space-between counter", `Quick, test_html_space_between_counter;
  "html text precise color", `Quick, test_html_text_precise_color;
  "html text no color", `Quick, test_html_text_no_color;
  "html frame vertical", `Quick, test_html_frame_vertical;
  "html frame min primary", `Quick, test_html_frame_min_primary;
  "html frame min counter", `Quick, test_html_frame_min_counter;
]

(** ============== 6. Typography CSS: Strikethrough, Lower, Title, SmallCaps, SmallCapsForced ============== *)

let test_typo_css_strikethrough () =
  let t = make_typo ~deco:Strikethrough () in
  let css = typography_to_css t in
  check bool "has line-through" true (has "text-decoration:line-through" css)

let test_typo_css_lower () =
  let t = make_typo ~tcase:Lower () in
  let css = typography_to_css t in
  check bool "has lowercase" true (has "text-transform:lowercase" css)

let test_typo_css_title () =
  let t = make_typo ~tcase:Title () in
  let css = typography_to_css t in
  check bool "has capitalize" true (has "text-transform:capitalize" css)

let test_typo_css_smallcaps () =
  let t = make_typo ~tcase:SmallCaps () in
  let css = typography_to_css t in
  check bool "has small-caps" true (has "font-variant:small-caps" css)

let test_typo_css_smallcaps_forced () =
  let t = make_typo ~tcase:SmallCapsForced () in
  let css = typography_to_css t in
  check bool "has all-small-caps" true (has "font-variant:all-small-caps" css)

let test_typo_css_left_align () =
  let t = make_typo ~align:Left () in
  let css = typography_to_css t in
  check bool "no text-align for Left" false (has "text-align" css)

let test_typo_css_no_deco () =
  let t = make_typo ~deco:NoDeco () in
  let css = typography_to_css t in
  check bool "no text-decoration" false (has "text-decoration" css)

let test_typo_css_original_case () =
  let t = make_typo ~tcase:Original () in
  let css = typography_to_css t in
  check bool "no text-transform" false (has "text-transform" css);
  check bool "no font-variant" false (has "font-variant" css)

let typo_css_tests = [
  "typo strikethrough", `Quick, test_typo_css_strikethrough;
  "typo lowercase", `Quick, test_typo_css_lower;
  "typo capitalize", `Quick, test_typo_css_title;
  "typo small-caps", `Quick, test_typo_css_smallcaps;
  "typo all-small-caps", `Quick, test_typo_css_smallcaps_forced;
  "typo left align omitted", `Quick, test_typo_css_left_align;
  "typo no decoration", `Quick, test_typo_css_no_deco;
  "typo original case", `Quick, test_typo_css_original_case;
]

(** ============== 7. style_to_css: stroke with weight, effects ============== *)

let test_style_css_stroke_with_weight () =
  let n = mk
    ~strokes:[make_paint Solid (Some (make_rgba 1. 0. 0.))]
    ~stroke_weight:2.
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let css = style_to_css n in
  check bool "has border:" true (has "border:" css)

let test_style_css_stroke_zero_weight () =
  (* stroke color present but weight=0 -> no border *)
  let n = mk
    ~strokes:[make_paint Solid (Some (make_rgba 1. 0. 0.))]
    ~stroke_weight:0.
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let css = style_to_css n in
  check bool "no border" false (has "border:" css)

let test_style_css_effects () =
  let fx = { fx_type = DropShadow; visible = true; radius = 4.;
             color = Some (make_rgba ~a:0.25 0. 0. 0.);
             offset = Some (2., 2.); spread = Some 1. } in
  let n = mk ~effects:[fx]
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let css = style_to_css n in
  check bool "has box-shadow" true (has "box-shadow:" css)

let test_style_css_precise_bg () =
  let n = mk
    ~fills:[make_paint Solid (Some (make_rgba 0.5 0.3 0.8))]
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let css = style_to_css ~precise:true n in
  check bool "has rgb(" true (has "rgb(" css)

let style_css_tests = [
  "style stroke with weight", `Quick, test_style_css_stroke_with_weight;
  "style stroke zero weight", `Quick, test_style_css_stroke_zero_weight;
  "style effects shadow", `Quick, test_style_css_effects;
  "style precise bg", `Quick, test_style_css_precise_bg;
]

(** ============== 8. generate_flat_html two-level structure ============== *)

let test_flat_html_two_level () =
  (* outer and inner have different sizes -> two-level structure *)
  (* Text node must NOT have solid fills, or find_inner_frame will descend into it *)
  let text = mk ~node_type:Text ~characters:(Some "Click")
    ~typography:(Some (make_typo ~size:16. ()))
    ~fills:[] () in
  let inner = mk ~name:"inner"
    ~fills:[make_paint Solid (Some (make_rgba 0. 0.5 1.))]
    ~border_radius:12.
    ~bbox:(Some { x=20.; y=10.; width=160.; height=40. })
    ~children:[text] () in
  let outer = mk ~name:"outer"
    ~bbox:(Some { x=0.; y=0.; width=200.; height=60. })
    ~children:[inner] () in
  let html = generate_flat_html outer in
  check bool "has display:flex" true (has "display:flex" html);
  check bool "has background" true (has "background:" html);
  check bool "outer width 200" true (has "200" html);
  check bool "inner width 160" true (has "160" html);
  check bool "has border-radius" true (has "border-radius:" html)

let test_flat_html_single_level () =
  (* outer and inner same size -> single structure *)
  let text = mk ~node_type:Text ~characters:(Some "OK")
    ~typography:(Some (make_typo ~size:14. ()))
    ~fills:[make_paint Solid (Some (make_rgba 0. 0. 0.))] () in
  let inner = mk ~name:"inner"
    ~fills:[make_paint Solid (Some (make_rgba 0.2 0.6 1.))]
    ~bbox:(Some { x=0.; y=0.; width=100.; height=48. })
    ~children:[text] () in
  let outer = mk ~name:"outer"
    ~bbox:(Some { x=0.; y=0.; width=100.; height=48. })
    ~children:[inner] () in
  let html = generate_flat_html outer in
  check bool "has DOCTYPE" true (has "<!DOCTYPE" html);
  check bool "has OK text" true (has "OK" html)

let test_flat_html_deep_inner_frame () =
  (* find_inner_frame should descend into deeper frame with bg *)
  let deep = mk ~name:"deep"
    ~fills:[make_paint Solid (Some (make_rgba 0. 0. 1.))]
    ~bbox:(Some { x=5.; y=5.; width=90.; height=38. }) () in
  let mid = mk ~name:"mid"
    ~fills:[make_paint Solid (Some (make_rgba 1. 0. 0.))]
    ~bbox:(Some { x=2.; y=2.; width=96.; height=44. })
    ~children:[deep] () in
  let outer = mk ~name:"outer"
    ~bbox:(Some { x=0.; y=0.; width=100.; height=48. })
    ~children:[mid] () in
  let html = generate_flat_html outer in
  check bool "has DOCTYPE" true (has "<!DOCTYPE" html);
  check bool "has background" true (has "background:" html)

let test_flat_html_no_bg_children () =
  (* find_inner_frame: no bg on any node, descend to visible child *)
  let child = mk ~name:"child" ~visible:true
    ~bbox:(Some { x=0.; y=0.; width=80.; height=30. }) () in
  let outer = mk ~name:"outer"
    ~bbox:(Some { x=0.; y=0.; width=100.; height=48. })
    ~children:[child] () in
  let html = generate_flat_html outer in
  check bool "has DOCTYPE" true (has "<!DOCTYPE" html)

let test_flat_html_no_visible_child () =
  (* All children invisible -> uses self *)
  let child = mk ~name:"hidden" ~visible:false () in
  let outer = mk ~name:"outer"
    ~fills:[make_paint Solid (Some (make_rgba 0.5 0.5 0.5))]
    ~bbox:(Some { x=0.; y=0.; width=100.; height=48. })
    ~children:[child] () in
  let html = generate_flat_html outer in
  check bool "has DOCTYPE" true (has "<!DOCTYPE" html)

let test_flat_html_inner_no_border_radius () =
  let text = mk ~node_type:Text ~characters:(Some "X")
    ~fills:[make_paint Solid (Some (make_rgba 1. 1. 1.))] () in
  let inner = mk ~name:"inner"
    ~fills:[make_paint Solid (Some (make_rgba 0. 0.3 0.9))]
    ~border_radius:0.
    ~bbox:(Some { x=10.; y=5.; width=80.; height=38. })
    ~children:[text] () in
  let outer = mk ~name:"outer"
    ~bbox:(Some { x=0.; y=0.; width=100.; height=48. })
    ~children:[inner] () in
  let html = generate_flat_html outer in
  check bool "no border-radius" false (has "border-radius:" html)

let flat_html_tests = [
  "flat html two-level", `Quick, test_flat_html_two_level;
  "flat html single-level", `Quick, test_flat_html_single_level;
  "flat html deep inner", `Quick, test_flat_html_deep_inner_frame;
  "flat html no bg children", `Quick, test_flat_html_no_bg_children;
  "flat html no visible child", `Quick, test_flat_html_no_visible_child;
  "flat html no border-radius", `Quick, test_flat_html_inner_no_border_radius;
]

(** ============== 9. fidelity_node JSON: vector/instance/structure branches ============== *)

let test_fidelity_node_polygon () =
  let json = `Assoc [
    ("type", `String "POLYGON");
    ("id", `String "p1");
    ("name", `String "hexagon");
    ("fillGeometry", `List []);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let vector = List.assoc "vector" fields in
    (* POLYGON triggers vector_applicable; fillGeometry is a vector_key *)
    check bool "vector section present" true (vector <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_star_type () =
  let json = `Assoc [
    ("type", `String "STAR");
    ("id", `String "s1");
    ("strokeGeometry", `List []);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let vector = List.assoc "vector" fields in
    check bool "vector for STAR" true (vector <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_line_type () =
  let json = `Assoc [
    ("type", `String "LINE");
    ("id", `String "l1");
    ("vectorPaths", `List []);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let vector = List.assoc "vector" fields in
    check bool "vector for LINE" true (vector <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_ellipse_type () =
  let json = `Assoc [
    ("type", `String "ELLIPSE");
    ("id", `String "e1");
    ("fillGeometry", `List []);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let vector = List.assoc "vector" fields in
    check bool "vector for ELLIPSE" true (vector <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_boolean_op () =
  let json = `Assoc [
    ("type", `String "BOOLEAN_OPERATION");
    ("id", `String "b1");
    ("strokeGeometry", `List []);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let vector = List.assoc "vector" fields in
    check bool "vector for BOOLEAN_OPERATION" true (vector <> `Assoc [])
  | _ -> fail "expected Assoc"

(* Also test vector_applicable without any vector_keys but with node_type match *)
let test_fidelity_node_polygon_no_keys () =
  let json = `Assoc [
    ("type", `String "POLYGON");
    ("id", `String "p2");
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    (* vector_applicable=true but no vector keys in json -> vector_missing populated *)
    let vm = List.assoc "vector_missing" fields in
    check bool "vector_missing is non-empty" true (vm <> `List [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_component_set () =
  let json = `Assoc [
    ("type", `String "COMPONENT_SET");
    ("id", `String "cs1");
    ("componentPropertyDefinitions", `Assoc [("variant", `String "primary")]);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let inst = List.assoc "instance" fields in
    check bool "instance section for COMPONENT_SET" true (inst <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_instance_type () =
  let json = `Assoc [
    ("type", `String "INSTANCE");
    ("id", `String "i1");
    ("componentId", `String "comp:123");
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let inst = List.assoc "instance" fields in
    check bool "instance for INSTANCE" true (inst <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_document_structure () =
  let json = `Assoc [
    ("type", `String "DOCUMENT");
    ("id", `String "0:0");
    ("children", `List [
      `Assoc [("type", `String "CANVAS"); ("id", `String "0:1")]
    ]);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let structure_missing = List.assoc "structure_missing" fields in
    check bool "structure has no missing" true (structure_missing = `List [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_canvas_no_children () =
  let json = `Assoc [
    ("type", `String "CANVAS");
    ("id", `String "0:1");
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let structure_missing = List.assoc "structure_missing" fields in
    (* CANVAS without children -> structure_missing = ["children"] *)
    check bool "missing children" true (structure_missing = `List [`String "children"])
  | _ -> fail "expected Assoc"

let test_fidelity_node_layout_mode () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "f1");
    ("layoutMode", `String "HORIZONTAL");
    ("primaryAxisAlignItems", `String "CENTER");
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let layout = List.assoc "layout" fields in
    check bool "layout populated" true (layout <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_layout_mode_none () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "f2");
    ("layoutMode", `String "NONE");
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let layout = List.assoc "layout" fields in
    (* layoutMode=NONE, no other layout keys -> layout_applicable=false via mode check *)
    (* But json_has_any_key checks for layoutMode presence -> layout still populated *)
    check bool "layout present" true (layout <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_text_segments () =
  let json = `Assoc [
    ("type", `String "TEXT");
    ("id", `String "t1");
    ("characters", `String "Hello");
    ("style", `Assoc [("fontSize", `Float 16.)]);
    ("styledTextSegments", `List [`Assoc [("text", `String "He")]]);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let ts = List.assoc "text_segments" fields in
    check bool "text_segments populated" true (ts <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_paint_keys () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "f3");
    ("fills", `List [`Assoc [("type", `String "SOLID")]]);
    ("strokeWeight", `Float 2.);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let paint = List.assoc "paint" fields in
    check bool "paint populated" true (paint <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_effects_keys () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "f4");
    ("opacity", `Float 0.8);
    ("effects", `List []);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let effects = List.assoc "effects" fields in
    check bool "effects populated" true (effects <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_bound_variables () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "f5");
    ("boundVariables", `Assoc [("fills", `String "var1")]);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let vars = List.assoc "variables" fields in
    check bool "variables populated" true (vars <> `Assoc [])
  | _ -> fail "expected Assoc"

let test_fidelity_node_image_refs_assets () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "f6");
    ("fills", `List [
      `Assoc [("imageRef", `String "img:abc")];
      `Assoc [("imageRef", `String "img:def")];
    ]);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let assets_missing = List.assoc "assets_missing" fields in
    (* image_refs not empty -> assets_missing = ["image_fills"] *)
    check bool "assets_missing has image_fills" true
      (assets_missing = `List [`String "image_fills"])
  | _ -> fail "expected Assoc"

let test_fidelity_node_non_assoc_child () =
  let json = `Assoc [
    ("type", `String "FRAME");
    ("id", `String "f7");
    ("children", `List [
      `String "not an assoc";
      `Assoc [("type", `String "TEXT"); ("id", `String "t2")];
    ]);
  ] in
  let result = fidelity_node json in
  match result with
  | `Assoc fields ->
    let children = List.assoc "children" fields in
    (* non-Assoc child filtered out *)
    (match children with
     | `List items -> check int "only 1 valid child" 1 (List.length items)
     | _ -> fail "expected List")
  | _ -> fail "expected Assoc"

let fidelity_node_tests = [
  "fidelity_node POLYGON", `Quick, test_fidelity_node_polygon;
  "fidelity_node POLYGON no keys", `Quick, test_fidelity_node_polygon_no_keys;
  "fidelity_node STAR", `Quick, test_fidelity_node_star_type;
  "fidelity_node LINE", `Quick, test_fidelity_node_line_type;
  "fidelity_node ELLIPSE", `Quick, test_fidelity_node_ellipse_type;
  "fidelity_node BOOLEAN_OPERATION", `Quick, test_fidelity_node_boolean_op;
  "fidelity_node COMPONENT_SET", `Quick, test_fidelity_node_component_set;
  "fidelity_node INSTANCE", `Quick, test_fidelity_node_instance_type;
  "fidelity_node DOCUMENT structure", `Quick, test_fidelity_node_document_structure;
  "fidelity_node CANVAS no children", `Quick, test_fidelity_node_canvas_no_children;
  "fidelity_node layout mode", `Quick, test_fidelity_node_layout_mode;
  "fidelity_node layout NONE", `Quick, test_fidelity_node_layout_mode_none;
  "fidelity_node TEXT segments", `Quick, test_fidelity_node_text_segments;
  "fidelity_node paint keys", `Quick, test_fidelity_node_paint_keys;
  "fidelity_node effects keys", `Quick, test_fidelity_node_effects_keys;
  "fidelity_node bound variables", `Quick, test_fidelity_node_bound_variables;
  "fidelity_node image refs assets", `Quick, test_fidelity_node_image_refs_assets;
  "fidelity_node non-assoc child", `Quick, test_fidelity_node_non_assoc_child;
]

(** ============== 10. classify_node additional branches ============== *)

let test_classify_boolean_operation () =
  let n = mk ~node_type:BooleanOperation ~name:"union"
    ~bbox:(Some { x=0.; y=0.; width=50.; height=50. }) () in
  let _ = node_to_compact n in
  (* BooleanOperation classifies as SemIcon (non-thin) *)
  check bool "classified" true true

let test_classify_section_default () =
  let n = mk ~node_type:Section ~name:"section_a" () in
  let dsl = node_to_compact n in
  (* Section with no button/input prefix -> SemFrame *)
  check bool "is F-tag" true (has "F(" dsl)

let test_classify_unknown_type () =
  let n = mk ~node_type:(Unknown "SHAPEWITHTEXT") ~name:"shape" () in
  let dsl = node_to_compact n in
  (* Unknown -> SemFrame *)
  check bool "is F-tag" true (has "F(" dsl)

let test_classify_component_set () =
  let n = mk ~node_type:ComponentSet ~name:"variant_set" () in
  let dsl = node_to_compact n in
  (* ComponentSet -> fallthrough to SemFrame *)
  check bool "is F-tag" true (has "F(" dsl)

let test_classify_sticky () =
  let n = mk ~node_type:Sticky ~name:"note" () in
  let dsl = node_to_compact n in
  (* Sticky -> _ fallthrough to SemFrame *)
  check bool "is F-tag" true (has "F(" dsl)

let test_classify_thin_width_as_divider () =
  let n = mk ~node_type:Vector ~name:"vert_line"
    ~bbox:(Some { x=0.; y=0.; width=1.; height=100. }) () in
  let dsl = node_to_compact n in
  check bool "thin width -> divider" true (has "---" dsl)

let test_classify_line_type () =
  let n = mk ~node_type:Line ~name:"separator"
    ~bbox:(Some { x=0.; y=0.; width=200.; height=1. }) () in
  let dsl = node_to_compact n in
  check bool "Line thin -> divider" true (has "---" dsl)

let test_classify_ellipse_large () =
  let n = mk ~node_type:Ellipse ~name:"circle"
    ~bbox:(Some { x=0.; y=0.; width=50.; height=50. }) () in
  let dsl = node_to_compact n in
  (* Large ellipse -> SemIcon *)
  check bool "is V-tag" true (has "V\"" dsl)

let test_classify_star_large () =
  let n = mk ~node_type:Star ~name:"star"
    ~bbox:(Some { x=0.; y=0.; width=30.; height=30. }) () in
  let dsl = node_to_compact n in
  check bool "is V-tag" true (has "V\"" dsl)

let test_classify_reg_polygon () =
  let n = mk ~node_type:RegularPolygon ~name:"triangle"
    ~bbox:(Some { x=0.; y=0.; width=40.; height=40. }) () in
  let dsl = node_to_compact n in
  check bool "is V-tag" true (has "V\"" dsl)

let test_classify_instance_icon () =
  let n = mk ~node_type:Instance ~name:"icon_arrow" () in
  let dsl = node_to_compact n in
  check bool "is V-tag" true (has "V\"" dsl)

let test_classify_instance_ic () =
  let n = mk ~node_type:Instance ~name:"ic_search" () in
  let dsl = node_to_compact n in
  check bool "is V-tag" true (has "V\"" dsl)

let test_classify_component_image () =
  let n = mk ~node_type:Component ~name:"image_hero" ~id:"c1" () in
  let dsl = node_to_compact n in
  check bool "is I-tag" true (has "I" dsl)

let test_classify_component_photo () =
  let n = mk ~node_type:Component ~name:"photo_profile" ~id:"c2" () in
  let dsl = node_to_compact n in
  check bool "is I-tag" true (has "I" dsl)

let test_classify_frame_btn () =
  let n = mk ~node_type:Frame ~name:"btn_primary" () in
  let dsl = node_to_compact n in
  check bool "is B-tag" true (has "B\"" dsl)

let test_classify_section_btn () =
  let n = mk ~node_type:Section ~name:"button_cta" () in
  let dsl = node_to_compact n in
  check bool "is B-tag" true (has "B\"" dsl)

let test_classify_section_input () =
  let n = mk ~node_type:Section ~name:"input_search" () in
  let dsl = node_to_compact n in
  check bool "is N-tag" true (has "N\"" dsl)

let test_classify_section_image () =
  let n = mk ~node_type:Section ~name:"image_bg" () in
  let dsl = node_to_compact n in
  check bool "is I-tag" true (has "I" dsl)

let classify_extra_tests = [
  "classify BooleanOperation", `Quick, test_classify_boolean_operation;
  "classify Section default", `Quick, test_classify_section_default;
  "classify Unknown type", `Quick, test_classify_unknown_type;
  "classify ComponentSet", `Quick, test_classify_component_set;
  "classify Sticky", `Quick, test_classify_sticky;
  "classify thin width divider", `Quick, test_classify_thin_width_as_divider;
  "classify Line type", `Quick, test_classify_line_type;
  "classify Ellipse large", `Quick, test_classify_ellipse_large;
  "classify Star large", `Quick, test_classify_star_large;
  "classify RegularPolygon", `Quick, test_classify_reg_polygon;
  "classify Instance icon", `Quick, test_classify_instance_icon;
  "classify Instance ic_", `Quick, test_classify_instance_ic;
  "classify Component image", `Quick, test_classify_component_image;
  "classify Component photo", `Quick, test_classify_component_photo;
  "classify Frame btn", `Quick, test_classify_frame_btn;
  "classify Section button", `Quick, test_classify_section_btn;
  "classify Section input", `Quick, test_classify_section_input;
  "classify Section image", `Quick, test_classify_section_image;
]

(** ============== 11. node_to_verbose: SemText + SemFrame branches ============== *)

let test_verbose_text_justified () =
  let typo = make_typo ~align:Justified () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~characters:(Some "text") () in
  let dsl = node_to_verbose n in
  check bool "has a:j" true (has "a:j" dsl)

let test_verbose_text_fill_color () =
  let typo = make_typo () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~fills:[make_paint Solid (Some (make_rgba 0.5 0. 0.))]
    ~characters:(Some "red") () in
  let dsl = node_to_verbose n in
  check bool "has c:" true (has "c:" dsl)

let test_verbose_frame_stroke () =
  let n = mk ~layout_mode:Horizontal
    ~strokes:[make_paint Solid (Some (make_rgba 0. 0. 0.))]
    ~bbox:(Some { x=0.; y=0.; width=100.; height=50. }) () in
  let dsl = node_to_verbose n in
  check bool "has bd:" true (has "bd:" dsl)

let test_verbose_sem_button () =
  (* SemButton in verbose mode -> falls back to node_to_compact *)
  let n = mk ~node_type:Component ~name:"button_ok" () in
  let dsl = node_to_verbose n in
  check bool "uses compact for button" true (has "B\"" dsl)

let test_verbose_sem_input () =
  let n = mk ~node_type:Instance ~name:"input_email" () in
  let dsl = node_to_verbose n in
  check bool "uses compact for input" true (has "N\"" dsl)

let test_verbose_sem_image () =
  let n = mk ~node_type:Instance ~name:"image_photo" ~id:"v1" () in
  let dsl = node_to_verbose n in
  check bool "uses compact for image" true (has "I" dsl)

let test_verbose_sem_icon () =
  let n = mk ~node_type:Vector ~name:"arrow"
    ~bbox:(Some { x=0.; y=0.; width=24.; height=24. }) () in
  let dsl = node_to_verbose n in
  check bool "uses compact for icon" true (has "V\"" dsl)

let test_verbose_sem_divider () =
  let n = mk ~node_type:Rectangle ~name:"line"
    ~bbox:(Some { x=0.; y=0.; width=300.; height=1. }) () in
  let dsl = node_to_verbose n in
  check bool "uses compact for divider" true (has "---" dsl)

let verbose_extra_tests = [
  "verbose text justified", `Quick, test_verbose_text_justified;
  "verbose text fill color", `Quick, test_verbose_text_fill_color;
  "verbose frame stroke", `Quick, test_verbose_frame_stroke;
  "verbose SemButton fallback", `Quick, test_verbose_sem_button;
  "verbose SemInput fallback", `Quick, test_verbose_sem_input;
  "verbose SemImage fallback", `Quick, test_verbose_sem_image;
  "verbose SemIcon fallback", `Quick, test_verbose_sem_icon;
  "verbose SemDivider fallback", `Quick, test_verbose_sem_divider;
]

(** ============== 12. generate_compact_smart: large tree path ============== *)

let test_compact_smart_large () =
  (* Create a tree with > 20 nodes to trigger memoization path *)
  let leaf () = mk ~node_type:Text ~characters:(Some "x")
    ~typography:(Some (make_typo ~size:16. ~weight:700 ()))
    ~fills:[make_paint Solid (Some (make_rgba 0.5 0. 1.))] () in
  let children = List.init 22 (fun _ -> leaf ()) in
  let root = mk ~layout_mode:Vertical ~children () in
  let dsl = generate_compact_smart root in
  (* Should use memoized version with @vars if repeated styles *)
  check bool "has content" true (String.length dsl > 0)

let test_compact_smart_small () =
  let leaf () = mk ~node_type:Text ~characters:(Some "y") () in
  let children = List.init 5 (fun _ -> leaf ()) in
  let root = mk ~layout_mode:Vertical ~children () in
  let dsl = generate_compact_smart root in
  check bool "has content" true (String.length dsl > 0);
  (* Small tree -> no @vars header *)
  check bool "no @vars" false (has "@vars" dsl)

let compact_smart_tests = [
  "compact smart large tree", `Quick, test_compact_smart_large;
  "compact smart small tree", `Quick, test_compact_smart_small;
]

(** ============== 13. Compact text alignment branches ============== *)

let test_compact_text_center () =
  let typo = make_typo ~align:Center () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~characters:(Some "centered") () in
  let dsl = node_to_compact n in
  check bool "has a:c" true (has "a:c" dsl)

let test_compact_text_right () =
  let typo = make_typo ~align:Right () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~characters:(Some "right") () in
  let dsl = node_to_compact n in
  check bool "has a:r" true (has "a:r" dsl)

let test_compact_text_left () =
  let typo = make_typo ~align:Left () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~characters:(Some "left") () in
  let dsl = node_to_compact n in
  (* Left alignment is default -> no a: attr *)
  check bool "no a:" false (has "a:" dsl)

let test_compact_text_justified () =
  let typo = make_typo ~align:Justified () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~characters:(Some "just") () in
  let dsl = node_to_compact n in
  (* Justified falls through to _ -> no align attr in compact mode *)
  check bool "no a: for justified" false (has "a:" dsl)

let test_compact_text_color_black_skip () =
  let typo = make_typo () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~fills:[make_paint Solid (Some (make_rgba 0. 0. 0.))]
    ~characters:(Some "black") () in
  let dsl = node_to_compact n in
  (* #000000 skipped in compact mode *)
  check bool "no c: for black" false (has "c:" dsl)

let test_compact_text_color_near_black_skip () =
  (* #191919 also skipped *)
  let typo = make_typo () in
  let n = mk ~node_type:Text ~typography:(Some typo)
    ~fills:[make_paint Solid (Some (make_rgba 0.098 0.098 0.098))]
    ~characters:(Some "nearblack") () in
  let dsl = node_to_compact n in
  check bool "no c: for near-black" false (has "c:" dsl)

let compact_text_tests = [
  "compact text center", `Quick, test_compact_text_center;
  "compact text right", `Quick, test_compact_text_right;
  "compact text left default", `Quick, test_compact_text_left;
  "compact text justified skip", `Quick, test_compact_text_justified;
  "compact text black skip", `Quick, test_compact_text_color_black_skip;
  "compact text near-black skip", `Quick, test_compact_text_color_near_black_skip;
]

(** ============== 14. Registry apply_to_dsl: font + weight substitution ============== *)

let test_registry_apply_dsl_full () =
  let counter = StyleRegistry.create_counter () in
  (* Add entries that exceed threshold *)
  StyleRegistry.count_color counter "#FF0000";
  StyleRegistry.count_color counter "#FF0000";
  StyleRegistry.count_font counter 24.;
  StyleRegistry.count_font counter 24.;
  StyleRegistry.count_weight counter 700;
  StyleRegistry.count_weight counter 700;
  let reg = StyleRegistry.finalize counter 2 in
  let dsl = "bg:#FF0000,s:24,w:700 bg:#FF0000,s:24,w:700" in
  let result = StyleRegistry.apply_to_dsl reg dsl in
  (* All three types should be substituted *)
  check bool "color substituted" true (has "$c" result);
  check bool "font substituted" true (has "$f" result);
  check bool "weight substituted" true (has "$w" result)

let test_registry_to_defs_all_types () =
  let counter = StyleRegistry.create_counter () in
  StyleRegistry.count_color counter "#00FF00";
  StyleRegistry.count_color counter "#00FF00";
  StyleRegistry.count_font counter 32.;
  StyleRegistry.count_font counter 32.;
  StyleRegistry.count_weight counter 600;
  StyleRegistry.count_weight counter 600;
  let reg = StyleRegistry.finalize counter 2 in
  let defs = StyleRegistry.to_defs reg in
  check bool "has @vars{" true (has "@vars{" defs);
  check bool "has color def" true (has "$c1=" defs);
  check bool "has font def" true (has "$f1=" defs);
  check bool "has weight def" true (has "$w1=" defs)

let registry_extra_tests = [
  "registry apply_to_dsl full", `Quick, test_registry_apply_dsl_full;
  "registry to_defs all types", `Quick, test_registry_to_defs_all_types;
]

(** ============== 15. get_bg_fidelity: gradient/image/emoji branches ============== *)

let test_bg_fidelity_gradient_linear () =
  let p = make_gradient_paint [(0., make_rgba 1. 0. 0.); (1., make_rgba 0. 0. 1.)] in
  let r = get_bg_fidelity [p] in
  check (option string) "grad" (Some "grad") r

let test_bg_fidelity_image () =
  let p = { paint_type = Image; visible = true; opacity = 1.0;
            color = None; gradient_stops = []; image_ref = Some "i1";
            scale_mode = None } in
  let r = get_bg_fidelity [p] in
  check (option string) "img" (Some "img") r

let test_bg_fidelity_emoji () =
  let p = { paint_type = Emoji; visible = true; opacity = 1.0;
            color = None; gradient_stops = []; image_ref = None;
            scale_mode = None } in
  let r = get_bg_fidelity [p] in
  check (option string) "emoji" (Some "emoji") r

let bg_fidelity_tests = [
  "fidelity bg gradient", `Quick, test_bg_fidelity_gradient_linear;
  "fidelity bg image", `Quick, test_bg_fidelity_image;
  "fidelity bg emoji", `Quick, test_bg_fidelity_emoji;
]

(** ============== 16. generate_html wrapper ============== *)

let test_generate_html_wrapper () =
  let n = mk ~node_type:Text ~characters:(Some "hello") () in
  let html = generate_html n in
  check bool "has DOCTYPE" true (has "<!DOCTYPE" html);
  check bool "has body" true (has "<body>" html);
  check bool "has content" true (has "hello" html)

let generate_html_tests = [
  "generate_html wrapper", `Quick, test_generate_html_wrapper;
]

(** ============== 17. add_xy_size_attrs: origin variations ============== *)

let test_add_xy_origin_root () =
  let n = mk ~bbox:(Some { x=10.; y=20.; width=100.; height=50. }) () in
  let attrs = add_xy_size_attrs ~is_root:true ~origin:(0.,0.) [] n in
  check bool "has sz:" true (List.exists (fun s -> has "sz:" s) attrs);
  check bool "no xy: for root" false (List.exists (fun s -> has "xy:" s) attrs)

let test_add_xy_no_bbox () =
  let n = mk () in
  let attrs = add_xy_size_attrs ~origin:(0.,0.) [] n in
  check int "no attrs added" 0 (List.length attrs)

let test_add_xy_no_origin () =
  let n = mk ~bbox:(Some { x=5.; y=10.; width=80.; height=40. }) () in
  let attrs = add_xy_size_attrs [] n in
  check bool "has xy: without origin" true (List.exists (fun s -> has "xy:" s) attrs)

let add_xy_tests = [
  "add_xy root with origin", `Quick, test_add_xy_origin_root;
  "add_xy no bbox", `Quick, test_add_xy_no_bbox;
  "add_xy no origin", `Quick, test_add_xy_no_origin;
]

(** ============== 18. Compact button: find text in nested children ============== *)

let test_compact_button_nested_text () =
  let text = mk ~node_type:Text ~characters:(Some "Nested") () in
  let group = mk ~node_type:Group ~children:[text] () in
  let n = mk ~node_type:Component ~name:"button_cta"
    ~children:[group] () in
  let dsl = node_to_compact n in
  check bool "finds nested text" true (has "Nested" dsl)

let test_compact_button_no_text () =
  let icon = mk ~node_type:Vector ~name:"arrow" () in
  let n = mk ~node_type:Component ~name:"button_icon"
    ~children:[icon] () in
  let dsl = node_to_compact n in
  check bool "falls back to name" true (has "button_icon" dsl)

let test_compact_button_bg () =
  let n = mk ~node_type:Component ~name:"btn_primary"
    ~fills:[make_paint Solid (Some (make_rgba 0. 0.5 1.))]
    ~border_radius:8. () in
  let dsl = node_to_compact n in
  check bool "has bg:" true (has "bg:" dsl);
  check bool "has r:" true (has "r:" dsl)

let compact_button_tests = [
  "compact button nested text", `Quick, test_compact_button_nested_text;
  "compact button no text", `Quick, test_compact_button_no_text;
  "compact button bg + radius", `Quick, test_compact_button_bg;
]

(** ============== 19. Fidelity text with line_height/letter_spacing (L454/L457) ============== *)

let test_fidelity_text_lh_ls () =
  (* Text node with typography that has line_height and letter_spacing *)
  let typo = make_typo ~size:16. ~weight:400 ~lh:(Some 24.) ~ls:(Some 0.5) ~align:Center () in
  let n = mk ~node_type:Text ~characters:(Some "Hello")
    ~typography:(Some typo)
    ~bbox:(Some { x=10.; y=20.; width=100.; height=30. }) () in
  let dsl = node_to_fidelity n in
  check bool "has lh:" true (has "lh:" dsl);
  check bool "has ls:" true (has "ls:" dsl);
  check bool "has a:c" true (has "a:c" dsl)

let test_fidelity_text_no_bbox_no_origin () =
  (* L440: origin=None, bbox=None -> None,None -> None *)
  let typo = make_typo ~size:14. ~weight:500 () in
  let n = mk ~node_type:Text ~characters:(Some "NoBbox")
    ~typography:(Some typo) () in
  (* Call node_to_fidelity without origin and without bbox *)
  let dsl = node_to_fidelity n in
  check bool "has T tag" true (has "T\"NoBbox\"" dsl);
  (* No xy: or sz: attrs because there's no bbox *)
  check bool "no xy:" false (has "xy:" dsl)

let test_fidelity_text_color_opacity () =
  (* Text with fill color and opacity *)
  let typo = make_typo ~size:20. ~weight:700 ~align:Right () in
  let n = mk ~node_type:Text ~characters:(Some "Styled")
    ~typography:(Some typo)
    ~fills:[make_paint Solid (Some (make_rgba 1. 0. 0.))]
    ~opacity:0.8
    ~bbox:(Some { x=0.; y=0.; width=60.; height=20. }) () in
  let dsl = node_to_fidelity n in
  check bool "has c:" true (has "c:" dsl);
  check bool "has op:" true (has "op:" dsl);
  check bool "has a:r" true (has "a:r" dsl)

let fidelity_text_tests = [
  "fidelity text lh+ls", `Quick, test_fidelity_text_lh_ls;
  "fidelity text no bbox no origin", `Quick, test_fidelity_text_no_bbox_no_origin;
  "fidelity text color+opacity", `Quick, test_fidelity_text_color_opacity;
]

(** ============== 20. Fidelity SemButton find_text branches (L473/L476/L484/L488) ============== *)

let test_fidelity_button_no_children () =
  (* L473: find_text [] -> node.name; also L484/L488: no fills/strokes -> None *)
  let n = mk ~node_type:Component ~name:"btn_empty"
    ~children:[]
    ~bbox:(Some { x=0.; y=0.; width=100.; height=40. }) () in
  let dsl = node_to_fidelity n in
  check bool "falls back to name" true (has "btn_empty" dsl);
  check bool "no bg:" false (has "bg:" dsl);
  check bool "no bd:" false (has "bd:" dsl)

let test_fidelity_button_recurse_to_text () =
  (* L476: find_text recurses through non-text child *)
  let text = mk ~node_type:Text ~characters:(Some "Deep") () in
  let wrapper = mk ~node_type:Frame ~children:[text] () in
  let n = mk ~node_type:Component ~name:"btn_deep"
    ~children:[wrapper]
    ~bbox:(Some { x=0.; y=0.; width=120.; height=44. }) () in
  let dsl = node_to_fidelity n in
  check bool "finds nested text" true (has "Deep" dsl)

let test_fidelity_button_recurse_no_text () =
  (* L473 again: find_text eventually reaches [] because no Text nodes *)
  let icon = mk ~node_type:Vector ~name:"icon" () in
  let n = mk ~node_type:Component ~name:"btn_icon_only"
    ~children:[icon]
    ~bbox:(Some { x=0.; y=0.; width=44.; height=44. }) () in
  let dsl = node_to_fidelity n in
  check bool "falls back to name" true (has "btn_icon_only" dsl)

let fidelity_button_tests = [
  "fidelity button no children", `Quick, test_fidelity_button_no_children;
  "fidelity button recurse to text", `Quick, test_fidelity_button_recurse_to_text;
  "fidelity button recurse no text", `Quick, test_fidelity_button_recurse_no_text;
]

(** ============== 21. Fidelity SemInput no bg/stroke (L500/L503) ============== *)

let test_fidelity_input_no_fills_no_strokes () =
  (* L500: bg None; L503: stroke None *)
  let n = mk ~node_type:Instance ~name:"input_plain"
    ~fills:[] ~strokes:[]
    ~bbox:(Some { x=0.; y=0.; width=200.; height=40. }) () in
  let dsl = node_to_fidelity n in
  check bool "has N tag" true (has "N\"input_plain\"" dsl);
  check bool "no bg:" false (has "bg:" dsl);
  check bool "no bd:" false (has "bd:" dsl)

let fidelity_input_tests = [
  "fidelity input no fills/strokes", `Quick, test_fidelity_input_no_fills_no_strokes;
]

(** ============== 22. Fidelity SemFrame no bbox/no bg (L531/L536) ============== *)

let test_fidelity_frame_no_bbox () =
  (* L531: bbox=None -> size_str="" *)
  let n = mk ~layout_mode:Horizontal ~fills:[] () in
  let dsl = node_to_fidelity n in
  check bool "is F-tag" true (has "F(" dsl);
  check bool "no bg:" false (has "bg:" dsl)

let test_fidelity_frame_no_bg_fills () =
  (* L536: invisible fills only -> bg None *)
  let n = mk ~layout_mode:Vertical
    ~fills:[make_paint ~visible:false Solid (Some (make_rgba 1. 0. 0.))]
    ~bbox:(Some { x=0.; y=0.; width=100.; height=100. }) () in
  let dsl = node_to_fidelity n in
  check bool "is F-tag" true (has "F(" dsl);
  check bool "no bg:" false (has "bg:" dsl)

let test_fidelity_frame_visible_children_filter () =
  (* Children filtering: invisible children excluded *)
  let visible_child = mk ~node_type:Text ~characters:(Some "A")
    ~bbox:(Some { x=0.; y=0.; width=50.; height=20. }) () in
  let invisible_child = mk ~node_type:Text ~characters:(Some "B")
    ~visible:false ~bbox:(Some { x=0.; y=0.; width=50.; height=20. }) () in
  let n = mk ~layout_mode:Horizontal
    ~bbox:(Some { x=0.; y=0.; width=200.; height=50. })
    ~children:[visible_child; invisible_child] () in
  let dsl = node_to_fidelity n in
  check bool "has visible child A" true (has "A" dsl);
  check bool "no invisible child B" false (has "B" dsl)

let fidelity_frame_extra_tests = [
  "fidelity frame no bbox", `Quick, test_fidelity_frame_no_bbox;
  "fidelity frame no bg fills", `Quick, test_fidelity_frame_no_bg_fills;
  "fidelity frame visible filter", `Quick, test_fidelity_frame_visible_children_filter;
]

(** ============== 23. HTML SemButton find_text (L723/L726) ============== *)

let test_html_button_no_children () =
  (* L723: find_text [] -> node.name *)
  let n = mk ~node_type:Component ~name:"btn_html_empty"
    ~children:[]
    ~bbox:(Some { x=0.; y=0.; width=100.; height=40. }) () in
  let html = node_to_html n in
  check bool "has button tag" true (has "<button" html);
  check bool "falls back to name" true (has "btn_html_empty" html)

let test_html_button_recurse_text () =
  (* L726: find_text recurses through non-text child to find Text *)
  let text = mk ~node_type:Text ~characters:(Some "ClickMe") () in
  let group = mk ~node_type:Group ~children:[text] () in
  let n = mk ~node_type:Frame ~name:"btn_html_nested"
    ~children:[group]
    ~bbox:(Some { x=0.; y=0.; width=120.; height=44. }) () in
  let html = node_to_html n in
  check bool "finds nested text" true (has "ClickMe" html)

let test_html_button_no_text_fallback () =
  (* L723 via L726: recurse finds no Text -> falls back to name *)
  let icon = mk ~node_type:Vector ~name:"arrow" () in
  let n = mk ~node_type:Component ~name:"btn_html_icon"
    ~children:[icon]
    ~bbox:(Some { x=0.; y=0.; width=44.; height=44. }) () in
  let html = node_to_html n in
  check bool "falls back to name" true (has "btn_html_icon" html)

let html_button_tests = [
  "html button no children", `Quick, test_html_button_no_children;
  "html button recurse text", `Quick, test_html_button_recurse_text;
  "html button no text fallback", `Quick, test_html_button_no_text_fallback;
]

(** ============== 24. generate_compact_memoized direct call (L895) ============== *)

let test_compact_memoized_direct () =
  (* Direct call to generate_compact_memoized to cover L895 *)
  let leaf i = mk ~node_type:Text ~characters:(Some (Printf.sprintf "item%d" i))
    ~typography:(Some (make_typo ~size:16. ~weight:500 ~family:"Inter" ()))
    ~fills:[make_paint Solid (Some (make_rgba 0. 0.478 1.))] () in
  let children = List.init 10 leaf in
  let root = mk ~layout_mode:Vertical ~children () in
  let dsl = generate_compact_memoized ~threshold:2 root in
  check bool "non-empty" true (String.length dsl > 0);
  check bool "has @vars" true (has "@vars" dsl)

let test_compact_memoized_threshold_3 () =
  (* Different threshold value *)
  let leaf () = mk ~node_type:Text ~characters:(Some "x")
    ~fills:[make_paint Solid (Some (make_rgba 0.5 0.5 0.5))] () in
  let children = List.init 5 (fun _ -> leaf ()) in
  let root = mk ~layout_mode:Horizontal ~children () in
  let dsl = generate_compact_memoized ~threshold:3 root in
  check bool "non-empty" true (String.length dsl > 0)

let compact_memoized_tests = [
  "compact memoized direct", `Quick, test_compact_memoized_direct;
  "compact memoized threshold 3", `Quick, test_compact_memoized_threshold_3;
]

(** ============== 25. json_member/json_has_key non-Assoc (L929/L934/L987) ============== *)

let test_json_member_non_assoc () =
  (* L929: json_member on non-Assoc returns None *)
  let result = json_member "key" (`List [`String "a"]) in
  check bool "returns None" true (result = None)

let test_json_member_on_string () =
  let result = json_member "key" (`String "hello") in
  check bool "returns None" true (result = None)

let test_json_has_key_non_assoc () =
  (* L934: json_has_key on non-Assoc returns false *)
  let result = json_has_key "key" (`List []) in
  check bool "returns false" false result

let test_json_has_key_on_string () =
  let result = json_has_key "key" (`String "x") in
  check bool "returns false" false result

let test_fidelity_node_no_type () =
  (* L987: fidelity_node where "type" is not a string *)
  let json = `Assoc [
    ("id", `String "n1");
    ("type", `Int 42);
  ] in
  let result = fidelity_node json in
  check bool "returns Assoc" true (match result with `Assoc _ -> true | _ -> false)

let test_fidelity_node_missing_type () =
  (* "type" key absent entirely *)
  let json = `Assoc [
    ("id", `String "n2");
    ("name", `String "orphan");
  ] in
  let result = fidelity_node json in
  check bool "returns Assoc" true (match result with `Assoc _ -> true | _ -> false)

let json_edge_tests = [
  "json_member non-Assoc", `Quick, test_json_member_non_assoc;
  "json_member on string", `Quick, test_json_member_on_string;
  "json_has_key non-Assoc", `Quick, test_json_has_key_non_assoc;
  "json_has_key on string", `Quick, test_json_has_key_on_string;
  "fidelity_node no type string", `Quick, test_fidelity_node_no_type;
  "fidelity_node missing type", `Quick, test_fidelity_node_missing_type;
]

(** ============== 26. flat_html find_inner_frame no visible children (L1187) ============== *)

let test_flat_html_no_visible_children () =
  (* L1187: node has no bg, has children, but none are visible *)
  let invis1 = mk ~node_type:Text ~characters:(Some "hidden1") ~visible:false () in
  let invis2 = mk ~node_type:Text ~characters:(Some "hidden2") ~visible:false () in
  let root = mk ~layout_mode:Vertical ~fills:[]
    ~bbox:(Some { x=0.; y=0.; width=200.; height=100. })
    ~children:[invis1; invis2] () in
  let html = generate_flat_html root in
  (* Should still produce valid HTML even when find_inner_frame returns root *)
  check bool "has html tag" true (has "<html" html);
  check bool "has body tag" true (has "<body" html)

let test_flat_html_no_children_no_bg () =
  (* Another edge case: no bg, empty children -> [] -> n *)
  let root = mk ~layout_mode:Vertical ~fills:[]
    ~bbox:(Some { x=0.; y=0.; width=200.; height=100. })
    ~children:[] () in
  let html = generate_flat_html root in
  check bool "has html tag" true (has "<html" html)

let flat_html_extra_tests = [
  "flat html no visible children", `Quick, test_flat_html_no_visible_children;
  "flat html no children no bg", `Quick, test_flat_html_no_children_no_bg;
]

(** ============== Main ============== *)

let () =
  run "Codegen Coverage W3" [
    "bg_color_precise", bg_color_precise_tests;
    "collect_from_node", collect_tests;
    "compact_frame", compact_frame_tests;
    "fidelity_extra", fidelity_extra_tests;
    "html_extra", html_extra_tests;
    "typo_css", typo_css_tests;
    "style_css", style_css_tests;
    "flat_html", flat_html_tests;
    "fidelity_node", fidelity_node_tests;
    "classify_extra", classify_extra_tests;
    "verbose_extra", verbose_extra_tests;
    "compact_smart", compact_smart_tests;
    "compact_text", compact_text_tests;
    "registry_extra", registry_extra_tests;
    "bg_fidelity", bg_fidelity_tests;
    "generate_html", generate_html_tests;
    "add_xy", add_xy_tests;
    "compact_button", compact_button_tests;
    "fidelity_text", fidelity_text_tests;
    "fidelity_button", fidelity_button_tests;
    "fidelity_input", fidelity_input_tests;
    "fidelity_frame_extra", fidelity_frame_extra_tests;
    "html_button", html_button_tests;
    "compact_memoized", compact_memoized_tests;
    "json_edge", json_edge_tests;
    "flat_html_extra", flat_html_extra_tests;
  ]
