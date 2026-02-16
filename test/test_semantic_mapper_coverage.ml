(** Semantic Mapper Coverage Tests
    Target: Push from 57.84% to 85%+
    Focus: uncovered branches, composite functions, edge cases *)

open Alcotest
open Figma_types

(** Helper: default node with overrides *)
let node
    ?(layout_mode = None')
    ?(bbox = None)
    ?(gap = 0.)
    ?(padding = (0., 0., 0., 0.))
    ?(border_radii = None)
    ?(border_radius = 0.)
    ?(primary_axis_align = Min)
    ?(counter_axis_align = Min)
    ?(layout_sizing_h = Fixed)
    ?(layout_sizing_v = Fixed)
    ?(opacity = 1.)
    ?(typography = None)
    ?(fills = [])
    ?(characters = None)
    () =
  { default_node with
    layout_mode; bbox; gap; padding; border_radii; border_radius;
    primary_axis_align; counter_axis_align;
    layout_sizing_h; layout_sizing_v;
    opacity; typography; fills; characters }

(** Helper: make a solid paint fill *)
let solid_fill ?(opacity=1.) r g b a =
  { paint_type = Solid; visible = true; opacity;
    color = Some { r; g; b; a };
    gradient_stops = []; image_ref = None; scale_mode = None }

(** Helper: paint fill without color *)
let colorless_fill () =
  { paint_type = Solid; visible = true; opacity = 1.;
    color = None;
    gradient_stops = []; image_ref = None; scale_mode = None }

(* ============== fmt edge cases ============== *)

let test_fmt_integer () =
  (* 10.0 = Float.round 10.0 -> "%.0f" path *)
  check string "integer" "10" (Semantic_mapper.fmt 10.)

let test_fmt_zero () =
  check string "zero" "0" (Semantic_mapper.fmt 0.)

let test_fmt_negative () =
  check string "negative int" "-5" (Semantic_mapper.fmt (-5.))

let test_fmt_sub_pixel () =
  (* 0.5 <> Float.round 0.5 (1.0) -> "%.2g" path *)
  check string "sub-pixel" "0.5" (Semantic_mapper.fmt 0.5)

let test_fmt_small_fraction () =
  (* 0.25 -> "%.2g" = "0.25" *)
  check string "small fraction" "0.25" (Semantic_mapper.fmt 0.25)

let test_fmt_trailing_dot () =
  (* A value where %.2g produces trailing dot.
     100. = Float.round 100. -> integer path, so no trailing dot.
     We need a non-integer where %.2g gives trailing dot.
     Actually %.2g with 2 significant digits: 9.5 -> "9.5", 95. -> "95" (integer path).
     The trailing-dot case requires %.2g to produce "X." which is rare.
     Let's try: 1e1 = 10.0 -> integer path.
     For trailing dot: Printf.sprintf "%.2g" value must end with ".".
     That happens with values like 10.5 where round is 11:
     10.5 <> Float.round 10.5 (10., actually Float.round uses ties-to-even) -> nope
     Actually Float.round 10.5 = 10. in OCaml (ties-to-even).
     So 10.5 <> 10. -> true -> "%.2g" 10.5 -> "10." -> trailing dot -> trimmed to "10"
     Wait: 10.5 and Float.round 10.5 is 10. (OCaml ties to even).
     So 10.5 <> 10. = true, goes to %.2g branch.
     Printf.sprintf "%.2g" 10.5 = "10." (2 significant digits) -> ends with "." -> trim
  *)
  let result = Semantic_mapper.fmt 10.5 in
  check string "trailing dot trimmed" "10" result

let test_fmt_negative_fraction () =
  check string "neg fraction" "-0.5" (Semantic_mapper.fmt (-0.5))

let fmt_tests = [
  "fmt integer", `Quick, test_fmt_integer;
  "fmt zero", `Quick, test_fmt_zero;
  "fmt negative", `Quick, test_fmt_negative;
  "fmt sub-pixel", `Quick, test_fmt_sub_pixel;
  "fmt small fraction", `Quick, test_fmt_small_fraction;
  "fmt trailing dot", `Quick, test_fmt_trailing_dot;
  "fmt negative fraction", `Quick, test_fmt_negative_fraction;
]

(* ============== shadow edge cases ============== *)

let test_shadow_all_zero () =
  (* x=0,y=0,blur=0,spread=0 -> skip *)
  let r = Semantic_mapper.shadow ~x:0. ~y:0. ~blur:0. ~spread:0. ~color:"#000" () in
  check string "all-zero shadow" "" r

let test_shadow_empty_color () =
  let r = Semantic_mapper.shadow ~x:1. ~y:2. ~blur:3. ~spread:0. ~color:"" () in
  check string "empty color shadow" "" r

let test_shadow_inset_with_spread () =
  let r = Semantic_mapper.shadow ~inset:true ~x:1. ~y:2. ~blur:3. ~spread:4. ~color:"#FF0000" () in
  check string "inset+spread" "sh:i,1,2,3,4,#FF0000" r

let test_shadow_sub_pixel_values () =
  let r = Semantic_mapper.shadow ~x:0.5 ~y:1.5 ~blur:2.5 ~spread:0. ~color:"#000" () in
  check string "sub-pixel shadow" "sh:0.5,1.5,2.5,#000" r

let shadow_tests = [
  "shadow all-zero skip", `Quick, test_shadow_all_zero;
  "shadow empty-color skip", `Quick, test_shadow_empty_color;
  "shadow inset+spread combo", `Quick, test_shadow_inset_with_spread;
  "shadow sub-pixel values", `Quick, test_shadow_sub_pixel_values;
]

(* ============== Uncovered variant branches ============== *)

let test_text_align_h_justified () =
  check string "justified" "ta:justify" (Semantic_mapper.text_align_h Justified)

let test_text_align_v_bottom () =
  check string "bottom" "va:bottom" (Semantic_mapper.text_align_v Bottom)

let test_cross_space_between () =
  check string "cross between" "cross:between" (Semantic_mapper.cross SpaceBetween)

let test_cross_baseline () =
  check string "cross baseline" "cross:baseline" (Semantic_mapper.cross Baseline)

let test_blend_mode_pass_through () =
  check string "pass-through" "" (Semantic_mapper.blend_mode "PASS_THROUGH")

let test_blend_mode_empty () =
  check string "empty blend" "" (Semantic_mapper.blend_mode "")

let test_blend_mode_screen () =
  check string "screen" "blend:screen" (Semantic_mapper.blend_mode "SCREEN")

let test_constraint_h_center () =
  check string "pin hcenter" "pin:hcenter" (Semantic_mapper.constraint_h `Center)

let test_constraint_h_scale () =
  check string "pin hscale" "pin:hscale" (Semantic_mapper.constraint_h `Scale)

let test_constraint_v_topbottom () =
  check string "pin tb" "pin:tb" (Semantic_mapper.constraint_v `TopBottom)

let test_constraint_v_center () =
  check string "pin vcenter" "pin:vcenter" (Semantic_mapper.constraint_v `Center)

let test_constraint_v_scale () =
  check string "pin vscale" "pin:vscale" (Semantic_mapper.constraint_v `Scale)

let test_radii_all_zero () =
  check string "radii all zero" "" (Semantic_mapper.radii (Some (0., 0., 0., 0.)))

let test_grid_pattern_grid () =
  check string "grid pattern" "ggrid:4"
    (Semantic_mapper.grid_pattern_to_semantic 4 Semantic_mapper.Grid)

let variant_tests = [
  "text_align_h Justified", `Quick, test_text_align_h_justified;
  "text_align_v Bottom", `Quick, test_text_align_v_bottom;
  "cross SpaceBetween", `Quick, test_cross_space_between;
  "cross Baseline", `Quick, test_cross_baseline;
  "blend_mode PASS_THROUGH", `Quick, test_blend_mode_pass_through;
  "blend_mode empty", `Quick, test_blend_mode_empty;
  "blend_mode SCREEN", `Quick, test_blend_mode_screen;
  "constraint_h Center", `Quick, test_constraint_h_center;
  "constraint_h Scale", `Quick, test_constraint_h_scale;
  "constraint_v TopBottom", `Quick, test_constraint_v_topbottom;
  "constraint_v Center", `Quick, test_constraint_v_center;
  "constraint_v Scale", `Quick, test_constraint_v_scale;
  "radii all zero", `Quick, test_radii_all_zero;
  "grid_pattern Grid", `Quick, test_grid_pattern_grid;
]

(* ============== Wildcard fallthrough branches ============== *)

let test_letter_spacing_zero () =
  (* Some 0. -> when ls <> 0. fails -> wildcard _ -> "" *)
  check string "ls zero" "" (Semantic_mapper.letter_spacing (Some 0.))

let test_line_height_zero () =
  (* Some 0. -> when lh > 0. fails -> wildcard _ -> "" *)
  check string "lh zero" "" (Semantic_mapper.line_height (Some 0.))

let test_line_height_negative () =
  (* Some -1. -> when lh > 0. fails -> wildcard _ -> "" *)
  check string "lh negative" "" (Semantic_mapper.line_height (Some (-1.)))

let test_text_color_empty () =
  check string "empty text color" "" (Semantic_mapper.text_color "")

let test_font_size_zero () =
  check string "font size zero" "" (Semantic_mapper.font_size 0.)

let test_font_size_negative () =
  check string "font size neg" "" (Semantic_mapper.font_size (-1.))

let test_gap_negative () =
  check string "gap negative" "" (Semantic_mapper.gap (-5.))

let test_border_sub_pixel () =
  check string "border 0.5px" "b:0.5,#CCC" (Semantic_mapper.border ~weight:0.5 ~color:"#CCC")

let test_opacity_zero () =
  check string "opacity 0" "o:0.00" (Semantic_mapper.opacity 0.)

let test_opacity_near_full () =
  check string "opacity 0.99" "o:0.99" (Semantic_mapper.opacity 0.99)

let wildcard_tests = [
  "letter_spacing Some 0.", `Quick, test_letter_spacing_zero;
  "line_height Some 0.", `Quick, test_line_height_zero;
  "line_height Some -1.", `Quick, test_line_height_negative;
  "text_color empty", `Quick, test_text_color_empty;
  "font_size 0", `Quick, test_font_size_zero;
  "font_size negative", `Quick, test_font_size_negative;
  "gap negative", `Quick, test_gap_negative;
  "border sub-pixel", `Quick, test_border_sub_pixel;
  "opacity zero", `Quick, test_opacity_zero;
  "opacity near-full", `Quick, test_opacity_near_full;
]

(* ============== node_to_semantic (composite) ============== *)

let test_node_default () =
  (* All defaults -> most attrs "" -> empty string *)
  let n = node () in
  check string "default node" "" (Semantic_mapper.node_to_semantic n)

let test_node_horizontal_layout () =
  let n = node ~layout_mode:Horizontal
    ~bbox:(Some { x = 0.; y = 0.; width = 320.; height = 200. })
    ~gap:12.
    ~padding:(16., 16., 16., 16.)
    ~primary_axis_align:Center
    ~counter_axis_align:Min
    () in
  let r = Semantic_mapper.node_to_semantic n in
  check bool "contains row" true (String.length r > 0);
  check bool "has row" true (try let _ = Str.search_forward (Str.regexp_string "row") r 0 in true with Not_found -> false);
  check bool "has size" true (try let _ = Str.search_forward (Str.regexp_string "320×200") r 0 in true with Not_found -> false);
  check bool "has gap" true (try let _ = Str.search_forward (Str.regexp_string "g:12") r 0 in true with Not_found -> false);
  check bool "has padding" true (try let _ = Str.search_forward (Str.regexp_string "p:16") r 0 in true with Not_found -> false);
  check bool "has align" true (try let _ = Str.search_forward (Str.regexp_string "align:center") r 0 in true with Not_found -> false)

let test_node_vertical_with_sizing () =
  let n = node ~layout_mode:Vertical
    ~layout_sizing_h:Hug
    ~layout_sizing_v:Fill
    () in
  let r = Semantic_mapper.node_to_semantic n in
  check bool "has col" true (try let _ = Str.search_forward (Str.regexp_string "col") r 0 in true with Not_found -> false);
  check bool "has w:hug" true (try let _ = Str.search_forward (Str.regexp_string "w:hug") r 0 in true with Not_found -> false);
  check bool "has h:fill" true (try let _ = Str.search_forward (Str.regexp_string "h:fill") r 0 in true with Not_found -> false)

let test_node_with_opacity () =
  let n = node ~opacity:0.5 () in
  let r = Semantic_mapper.node_to_semantic n in
  check string "has opacity" "o:0.50" r

let test_node_with_typography () =
  let typo = { default_typography with
    font_size = 24.;
    font_weight = 700;
    letter_spacing = Some 0.05;
    line_height = Some 32.;
    text_align_h = Center;
    text_align_v = Bottom;
    text_decoration = Underline;
  } in
  let n = node ~typography:(Some typo) () in
  let r = Semantic_mapper.node_to_semantic n in
  check bool "has font size" true (try let _ = Str.search_forward (Str.regexp_string "s:24") r 0 in true with Not_found -> false);
  check bool "has weight" true (try let _ = Str.search_forward (Str.regexp_string "w:700") r 0 in true with Not_found -> false);
  check bool "has letter spacing" true (try let _ = Str.search_forward (Str.regexp_string "ls:0.05") r 0 in true with Not_found -> false);
  check bool "has line height" true (try let _ = Str.search_forward (Str.regexp_string "lh:32") r 0 in true with Not_found -> false);
  check bool "has text align" true (try let _ = Str.search_forward (Str.regexp_string "ta:center") r 0 in true with Not_found -> false);
  check bool "has vert align" true (try let _ = Str.search_forward (Str.regexp_string "va:bottom") r 0 in true with Not_found -> false);
  check bool "has underline" true (try let _ = Str.search_forward (Str.regexp_string "u") r 0 in true with Not_found -> false)

let test_node_no_layout_skips_align () =
  (* When layout_mode = None', alignment is skipped even if non-default *)
  let n = node ~layout_mode:None'
    ~primary_axis_align:Center
    ~counter_axis_align:Max
    () in
  let r = Semantic_mapper.node_to_semantic n in
  check bool "no align for None'" true
    (try let _ = Str.search_forward (Str.regexp_string "align:") r 0 in false with Not_found -> true)

let test_node_with_radii () =
  let n = node ~border_radii:(Some (8., 8., 0., 0.)) () in
  let r = Semantic_mapper.node_to_semantic n in
  check bool "has radii" true (try let _ = Str.search_forward (Str.regexp_string "r:8,8,0,0") r 0 in true with Not_found -> false)

let test_node_with_radius () =
  let n = node ~border_radius:12. () in
  let r = Semantic_mapper.node_to_semantic n in
  check bool "has radius" true (try let _ = Str.search_forward (Str.regexp_string "r:12") r 0 in true with Not_found -> false)

let test_node_no_bbox () =
  let n = node ~layout_mode:Horizontal () in
  let r = Semantic_mapper.node_to_semantic n in
  check bool "no size when no bbox" true
    (try let _ = Str.search_forward (Str.regexp_string "×") r 0 in false with Not_found -> true)

let test_node_default_typography () =
  (* Typography with all defaults -> font_size 14 > 0 -> "s:14", weight 400 -> "", rest defaults *)
  let n = node ~typography:(Some default_typography) () in
  let r = Semantic_mapper.node_to_semantic n in
  check bool "has default size" true (try let _ = Str.search_forward (Str.regexp_string "s:14") r 0 in true with Not_found -> false)

let test_node_layout_with_space_between () =
  let n = node ~layout_mode:Horizontal
    ~primary_axis_align:SpaceBetween
    ~counter_axis_align:Baseline
    () in
  let r = Semantic_mapper.node_to_semantic n in
  check bool "has space-between" true (try let _ = Str.search_forward (Str.regexp_string "align:between") r 0 in true with Not_found -> false);
  check bool "has baseline" true (try let _ = Str.search_forward (Str.regexp_string "cross:baseline") r 0 in true with Not_found -> false)

let node_to_semantic_tests = [
  "node default", `Quick, test_node_default;
  "node horizontal layout", `Quick, test_node_horizontal_layout;
  "node vertical + sizing", `Quick, test_node_vertical_with_sizing;
  "node with opacity", `Quick, test_node_with_opacity;
  "node with typography", `Quick, test_node_with_typography;
  "node None' skips align", `Quick, test_node_no_layout_skips_align;
  "node with radii", `Quick, test_node_with_radii;
  "node with radius", `Quick, test_node_with_radius;
  "node no bbox", `Quick, test_node_no_bbox;
  "node default typography", `Quick, test_node_default_typography;
  "node SpaceBetween + Baseline", `Quick, test_node_layout_with_space_between;
]

(* ============== frame_dsl (composite) ============== *)

let test_frame_dsl_default () =
  let n = node () in
  let r = Semantic_mapper.frame_dsl n in
  check string "empty frame" "F()" r

let test_frame_dsl_with_bg () =
  let fill = solid_fill 1. 1. 1. 1. in
  let n = node ~fills:[fill]
    ~layout_mode:Horizontal
    ~bbox:(Some { x = 0.; y = 0.; width = 375.; height = 812. })
    () in
  let r = Semantic_mapper.frame_dsl n in
  check bool "has F(" true (String.length r > 2 && String.sub r 0 2 = "F(");
  check bool "has bg" true (try let _ = Str.search_forward (Str.regexp_string "bg:#FFFFFF") r 0 in true with Not_found -> false)

let test_frame_dsl_fill_without_color () =
  let fill = colorless_fill () in
  let n = node ~fills:[fill] () in
  let r = Semantic_mapper.frame_dsl n in
  (* fill without color -> bg "" -> no bg attr *)
  check bool "no bg for colorless" true
    (try let _ = Str.search_forward (Str.regexp_string "bg:") r 0 in false with Not_found -> true)

let test_frame_dsl_semi_transparent () =
  (* a < 1.0 -> 8-char hex with alpha *)
  let fill = solid_fill 0. 0. 0. 0.5 in
  let n = node ~fills:[fill] () in
  let r = Semantic_mapper.frame_dsl n in
  check bool "has alpha hex" true (try let _ = Str.search_forward (Str.regexp_string "bg:#00000080") r 0 in true with Not_found -> false)

let test_frame_dsl_multiple_fills () =
  let fill1 = solid_fill 1. 0. 0. 1. in
  let fill2 = solid_fill 0. 0. 1. 1. in
  let n = node ~fills:[fill1; fill2] () in
  let r = Semantic_mapper.frame_dsl n in
  (* Only first fill is used *)
  check bool "uses first fill" true (try let _ = Str.search_forward (Str.regexp_string "bg:#FF0000") r 0 in true with Not_found -> false)

let test_frame_dsl_no_fills () =
  let n = node ~fills:[] () in
  let r = Semantic_mapper.frame_dsl n in
  check bool "no bg" true
    (try let _ = Str.search_forward (Str.regexp_string "bg:") r 0 in false with Not_found -> true)

let frame_dsl_tests = [
  "frame_dsl default", `Quick, test_frame_dsl_default;
  "frame_dsl with bg", `Quick, test_frame_dsl_with_bg;
  "frame_dsl fill without color", `Quick, test_frame_dsl_fill_without_color;
  "frame_dsl semi-transparent", `Quick, test_frame_dsl_semi_transparent;
  "frame_dsl multiple fills", `Quick, test_frame_dsl_multiple_fills;
  "frame_dsl no fills", `Quick, test_frame_dsl_no_fills;
]

(* ============== text_dsl (composite) ============== *)

let test_text_dsl_plain () =
  let n = node ~characters:(Some "Hello") () in
  let r = Semantic_mapper.text_dsl n in
  check string "plain text" "T\"Hello\"" r

let test_text_dsl_with_typography () =
  let typo = { default_typography with font_size = 16.; font_weight = 700 } in
  let n = node ~characters:(Some "Bold") ~typography:(Some typo) () in
  let r = Semantic_mapper.text_dsl n in
  check bool "has T\"Bold\"[" true (try let _ = Str.search_forward (Str.regexp_string "T\"Bold\"[") r 0 in true with Not_found -> false);
  check bool "has s:16" true (try let _ = Str.search_forward (Str.regexp_string "s:16") r 0 in true with Not_found -> false);
  check bool "has w:700" true (try let _ = Str.search_forward (Str.regexp_string "w:700") r 0 in true with Not_found -> false)

let test_text_dsl_with_color () =
  let typo = { default_typography with font_size = 14.; font_weight = 400 } in
  let fill = solid_fill (51. /. 255.) (51. /. 255.) (51. /. 255.) 1. in
  let n = node ~characters:(Some "Colored") ~typography:(Some typo) ~fills:[fill] () in
  let r = Semantic_mapper.text_dsl n in
  check bool "has color" true (try let _ = Str.search_forward (Str.regexp_string "c:#") r 0 in true with Not_found -> false)

let test_text_dsl_default_suppression () =
  (* Typography with default weight 400 -> suppressed, only font_size shows *)
  let typo = { default_typography with font_size = 14.; font_weight = 400 } in
  let n = node ~characters:(Some "Default") ~typography:(Some typo) () in
  let r = Semantic_mapper.text_dsl n in
  check bool "has size" true (try let _ = Str.search_forward (Str.regexp_string "s:14") r 0 in true with Not_found -> false);
  check bool "no weight" true
    (try let _ = Str.search_forward (Str.regexp_string "w:") r 0 in false with Not_found -> true)

let test_text_dsl_no_characters () =
  let n = node () in
  let r = Semantic_mapper.text_dsl n in
  check string "empty text" "T\"\"" r

let test_text_dsl_fill_without_color () =
  let typo = { default_typography with font_size = 16. } in
  let fill = colorless_fill () in
  let n = node ~characters:(Some "NoColor") ~typography:(Some typo) ~fills:[fill] () in
  let r = Semantic_mapper.text_dsl n in
  check bool "no color attr" true
    (try let _ = Str.search_forward (Str.regexp_string "c:") r 0 in false with Not_found -> true)

let test_text_dsl_no_fills () =
  let typo = { default_typography with font_size = 18.; font_weight = 600 } in
  let n = node ~characters:(Some "NoFill") ~typography:(Some typo) ~fills:[] () in
  let r = Semantic_mapper.text_dsl n in
  check bool "no color for empty fills" true
    (try let _ = Str.search_forward (Str.regexp_string "c:") r 0 in false with Not_found -> true)

let test_text_dsl_no_typography () =
  let n = node ~characters:(Some "Plain") () in
  let r = Semantic_mapper.text_dsl n in
  (* No typography -> attrs = "" -> T"Plain" without brackets *)
  check string "plain no typo" "T\"Plain\"" r

let test_text_dsl_attrs_empty_suppresses_brackets () =
  (* Typography where all attrs suppress to empty: font_size 0 -> "", weight 400 -> "", no fills *)
  let typo = { default_typography with font_size = 0.; font_weight = 400 } in
  let n = node ~characters:(Some "Minimal") ~typography:(Some typo) () in
  let r = Semantic_mapper.text_dsl n in
  (* font_size 0 -> "", font_weight 400 -> "", no color -> attrs = "" -> no brackets *)
  check string "no brackets" "T\"Minimal\"" r

let text_dsl_tests = [
  "text_dsl plain", `Quick, test_text_dsl_plain;
  "text_dsl with typography", `Quick, test_text_dsl_with_typography;
  "text_dsl with color", `Quick, test_text_dsl_with_color;
  "text_dsl default suppression", `Quick, test_text_dsl_default_suppression;
  "text_dsl no characters", `Quick, test_text_dsl_no_characters;
  "text_dsl fill without color", `Quick, test_text_dsl_fill_without_color;
  "text_dsl no fills", `Quick, test_text_dsl_no_fills;
  "text_dsl no typography", `Quick, test_text_dsl_no_typography;
  "text_dsl empty attrs no brackets", `Quick, test_text_dsl_attrs_empty_suppresses_brackets;
]

(* ============== Misc edge cases ============== *)

let test_var_binding_complex () =
  check string "var spacing" "$spacing:tokens/base/4"
    (Semantic_mapper.var_binding "spacing" "tokens/base/4")

let test_sizing_h_axis () =
  check string "w:fill" "w:fill" (Semantic_mapper.sizing "w" Fill)

let test_sizing_v_axis () =
  check string "h:hug" "h:hug" (Semantic_mapper.sizing "h" Hug)

let test_size_large () =
  check string "large size" "1920×1080" (Semantic_mapper.size 1920. 1080.)

let test_padding_asymmetric () =
  (* t=b but r<>l -> 4-value form *)
  check string "asymmetric" "p:10,20,10,30" (Semantic_mapper.padding (10., 20., 10., 30.))

let misc_tests = [
  "var_binding complex", `Quick, test_var_binding_complex;
  "sizing w:fill", `Quick, test_sizing_h_axis;
  "sizing h:hug", `Quick, test_sizing_v_axis;
  "size large", `Quick, test_size_large;
  "padding asymmetric", `Quick, test_padding_asymmetric;
]

(* ============== Main ============== *)

let () =
  run "Semantic Mapper Coverage" [
    "fmt", fmt_tests;
    "shadow edge cases", shadow_tests;
    "uncovered variants", variant_tests;
    "wildcard fallthrough", wildcard_tests;
    "node_to_semantic", node_to_semantic_tests;
    "frame_dsl", frame_dsl_tests;
    "text_dsl", text_dsl_tests;
    "misc edge cases", misc_tests;
  ]
