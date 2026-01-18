(** Semantic Mapper 테스트 스위트

    UIFormer 영감의 Semantic DSL 매퍼 검증
    - Tier 1: Layout, Size, Gap, Padding, bg, radius (SSIM 80%+)
    - Tier 2: Alignment, Typography basics (SSIM +10%)
    - Tier 3: Font details, Letter spacing (SSIM +5%)
*)

open Alcotest

(** ============== Tier 1: High ROI Tests ============== *)

let test_layout_mode_horizontal () =
  let result = Semantic_mapper.layout_mode Figma_types.Horizontal in
  check string "horizontal" "row" result

let test_layout_mode_vertical () =
  let result = Semantic_mapper.layout_mode Figma_types.Vertical in
  check string "vertical" "col" result

let test_layout_mode_none () =
  let result = Semantic_mapper.layout_mode Figma_types.None' in
  check string "none" "" result

let test_gap_positive () =
  let result = Semantic_mapper.gap 12. in
  check string "gap 12" "g:12" result

let test_gap_zero () =
  let result = Semantic_mapper.gap 0. in
  check string "gap 0" "" result

let test_padding_uniform () =
  let result = Semantic_mapper.padding (16., 16., 16., 16.) in
  check string "uniform padding" "p:16" result

let test_padding_vertical_horizontal () =
  let result = Semantic_mapper.padding (8., 16., 8., 16.) in
  check string "v/h padding" "p:8,16" result

let test_padding_all_different () =
  let result = Semantic_mapper.padding (4., 8., 12., 16.) in
  check string "all different" "p:4,8,12,16" result

let test_padding_zero () =
  let result = Semantic_mapper.padding (0., 0., 0., 0.) in
  check string "zero padding" "" result

let test_bg_color () =
  let result = Semantic_mapper.bg "#FFFFFF" in
  check string "white bg" "bg:#FFFFFF" result

let test_bg_empty () =
  let result = Semantic_mapper.bg "" in
  check string "no bg" "" result

let test_radius_positive () =
  let result = Semantic_mapper.radius 8. in
  check string "radius 8" "r:8" result

let test_radius_zero () =
  let result = Semantic_mapper.radius 0. in
  check string "radius 0" "" result

let test_radii_uniform () =
  let result = Semantic_mapper.radii (Some (12., 12., 12., 12.)) in
  check string "uniform radii" "r:12" result

let test_radii_different () =
  let result = Semantic_mapper.radii (Some (8., 12., 8., 12.)) in
  check string "different radii" "r:8,12,8,12" result

let test_radii_none () =
  let result = Semantic_mapper.radii None in
  check string "no radii" "" result

let test_size () =
  let result = Semantic_mapper.size 320. 200. in
  check string "size" "320×200" result

let tier1_tests = [
  "layout_mode horizontal", `Quick, test_layout_mode_horizontal;
  "layout_mode vertical", `Quick, test_layout_mode_vertical;
  "layout_mode none", `Quick, test_layout_mode_none;
  "gap positive", `Quick, test_gap_positive;
  "gap zero", `Quick, test_gap_zero;
  "padding uniform", `Quick, test_padding_uniform;
  "padding vertical/horizontal", `Quick, test_padding_vertical_horizontal;
  "padding all different", `Quick, test_padding_all_different;
  "padding zero", `Quick, test_padding_zero;
  "bg color", `Quick, test_bg_color;
  "bg empty", `Quick, test_bg_empty;
  "radius positive", `Quick, test_radius_positive;
  "radius zero", `Quick, test_radius_zero;
  "radii uniform", `Quick, test_radii_uniform;
  "radii different", `Quick, test_radii_different;
  "radii none", `Quick, test_radii_none;
  "size", `Quick, test_size;
]

(** ============== Tier 2: Medium ROI Tests ============== *)

let test_align_min () =
  let result = Semantic_mapper.align Figma_types.Min in
  check string "align min" "align:start" result

let test_align_center () =
  let result = Semantic_mapper.align Figma_types.Center in
  check string "align center" "align:center" result

let test_align_max () =
  let result = Semantic_mapper.align Figma_types.Max in
  check string "align max" "align:end" result

let test_align_space_between () =
  let result = Semantic_mapper.align Figma_types.SpaceBetween in
  check string "align space-between" "align:between" result

let test_align_baseline () =
  let result = Semantic_mapper.align Figma_types.Baseline in
  check string "align baseline" "align:baseline" result

let test_cross_min () =
  let result = Semantic_mapper.cross Figma_types.Min in
  check string "cross min" "cross:start" result

let test_cross_center () =
  let result = Semantic_mapper.cross Figma_types.Center in
  check string "cross center" "cross:center" result

let test_cross_max () =
  let result = Semantic_mapper.cross Figma_types.Max in
  check string "cross max" "cross:end" result

let test_sizing_fixed () =
  let result = Semantic_mapper.sizing "w" Figma_types.Fixed in
  check string "sizing fixed" "" result

let test_sizing_hug () =
  let result = Semantic_mapper.sizing "w" Figma_types.Hug in
  check string "sizing hug" "w:hug" result

let test_sizing_fill () =
  let result = Semantic_mapper.sizing "h" Figma_types.Fill in
  check string "sizing fill" "h:fill" result

let test_shadow () =
  let result = Semantic_mapper.shadow ~x:0. ~y:4. ~blur:8. ~spread:0. ~color:"#00000040" () in
  check string "shadow" "sh:0,4,8,#00000040" result

let test_shadow_with_spread () =
  let result = Semantic_mapper.shadow ~x:0. ~y:4. ~blur:8. ~spread:2. ~color:"#00000040" () in
  check string "shadow with spread" "sh:0,4,8,2,#00000040" result

let test_inset_shadow () =
  let result = Semantic_mapper.shadow ~inset:true ~x:0. ~y:2. ~blur:4. ~spread:0. ~color:"#00000020" () in
  check string "inset shadow" "sh:i,0,2,4,#00000020" result

let test_border () =
  let result = Semantic_mapper.border ~weight:1. ~color:"#E0E0E0" in
  check string "border" "b:1,#E0E0E0" result

let test_border_zero () =
  let result = Semantic_mapper.border ~weight:0. ~color:"#E0E0E0" in
  check string "no border" "" result

let test_text_color () =
  let result = Semantic_mapper.text_color "#333333" in
  check string "text color" "c:#333333" result

let test_font_size () =
  let result = Semantic_mapper.font_size 16. in
  check string "font size" "s:16" result

let tier2_tests = [
  "align min", `Quick, test_align_min;
  "align center", `Quick, test_align_center;
  "align max", `Quick, test_align_max;
  "align space-between", `Quick, test_align_space_between;
  "align baseline", `Quick, test_align_baseline;
  "cross min", `Quick, test_cross_min;
  "cross center", `Quick, test_cross_center;
  "cross max", `Quick, test_cross_max;
  "sizing fixed", `Quick, test_sizing_fixed;
  "sizing hug", `Quick, test_sizing_hug;
  "sizing fill", `Quick, test_sizing_fill;
  "shadow", `Quick, test_shadow;
  "shadow with spread", `Quick, test_shadow_with_spread;
  "inset shadow", `Quick, test_inset_shadow;
  "border", `Quick, test_border;
  "border zero", `Quick, test_border_zero;
  "text color", `Quick, test_text_color;
  "font size", `Quick, test_font_size;
]

(** ============== Tier 3: Low ROI Tests ============== *)

let test_font_weight_400 () =
  let result = Semantic_mapper.font_weight 400 in
  check string "font weight 400 (default)" "" result

let test_font_weight_700 () =
  let result = Semantic_mapper.font_weight 700 in
  check string "font weight 700" "w:700" result

let test_letter_spacing_none () =
  let result = Semantic_mapper.letter_spacing None in
  check string "no letter spacing" "" result

let test_letter_spacing_some () =
  let result = Semantic_mapper.letter_spacing (Some 0.05) in
  check string "letter spacing" "ls:0.05" result

let test_line_height_none () =
  let result = Semantic_mapper.line_height None in
  check string "no line height" "" result

let test_line_height_some () =
  let result = Semantic_mapper.line_height (Some 24.) in
  check string "line height" "lh:24" result

let test_text_align_h_left () =
  let result = Semantic_mapper.text_align_h Figma_types.Left in
  check string "text align left (default)" "" result

let test_text_align_h_center () =
  let result = Semantic_mapper.text_align_h Figma_types.Center in
  check string "text align center" "ta:center" result

let test_text_align_h_right () =
  let result = Semantic_mapper.text_align_h Figma_types.Right in
  check string "text align right" "ta:right" result

let test_text_align_v_top () =
  let result = Semantic_mapper.text_align_v Figma_types.Top in
  check string "text align top (default)" "" result

let test_text_align_v_center () =
  let result = Semantic_mapper.text_align_v (Figma_types.Center) in
  check string "text align v center" "va:center" result

let test_text_decoration_none () =
  let result = Semantic_mapper.text_decoration Figma_types.NoDeco in
  check string "no decoration" "" result

let test_text_decoration_underline () =
  let result = Semantic_mapper.text_decoration Figma_types.Underline in
  check string "underline" "u" result

let test_text_decoration_strikethrough () =
  let result = Semantic_mapper.text_decoration Figma_types.Strikethrough in
  check string "strikethrough" "s" result

let test_opacity_full () =
  let result = Semantic_mapper.opacity 1. in
  check string "full opacity" "" result

let test_opacity_half () =
  let result = Semantic_mapper.opacity 0.5 in
  check string "half opacity" "o:0.50" result

let tier3_tests = [
  "font weight 400", `Quick, test_font_weight_400;
  "font weight 700", `Quick, test_font_weight_700;
  "letter spacing none", `Quick, test_letter_spacing_none;
  "letter spacing some", `Quick, test_letter_spacing_some;
  "line height none", `Quick, test_line_height_none;
  "line height some", `Quick, test_line_height_some;
  "text align h left", `Quick, test_text_align_h_left;
  "text align h center", `Quick, test_text_align_h_center;
  "text align h right", `Quick, test_text_align_h_right;
  "text align v top", `Quick, test_text_align_v_top;
  "text align v center", `Quick, test_text_align_v_center;
  "text decoration none", `Quick, test_text_decoration_none;
  "text decoration underline", `Quick, test_text_decoration_underline;
  "text decoration strikethrough", `Quick, test_text_decoration_strikethrough;
  "opacity full", `Quick, test_opacity_full;
  "opacity half", `Quick, test_opacity_half;
]

(** ============== Tier 4: Specialist Tests ============== *)

let test_blend_mode_normal () =
  let result = Semantic_mapper.blend_mode "NORMAL" in
  check string "normal blend" "" result

let test_blend_mode_multiply () =
  let result = Semantic_mapper.blend_mode "MULTIPLY" in
  check string "multiply blend" "blend:multiply" result

let test_constraint_h_left () =
  let result = Semantic_mapper.constraint_h `Left in
  check string "pin left" "pin:left" result

let test_constraint_h_right () =
  let result = Semantic_mapper.constraint_h `Right in
  check string "pin right" "pin:right" result

let test_constraint_h_leftright () =
  let result = Semantic_mapper.constraint_h `LeftRight in
  check string "pin lr" "pin:lr" result

let test_constraint_v_top () =
  let result = Semantic_mapper.constraint_v `Top in
  check string "pin top" "pin:top" result

let test_constraint_v_bottom () =
  let result = Semantic_mapper.constraint_v `Bottom in
  check string "pin bottom" "pin:bottom" result

let test_grid_pattern_columns () =
  let result = Semantic_mapper.grid_pattern_to_semantic 12 Semantic_mapper.Columns in
  check string "grid columns" "gcols:12" result

let test_grid_pattern_rows () =
  let result = Semantic_mapper.grid_pattern_to_semantic 3 Semantic_mapper.Rows in
  check string "grid rows" "grows:3" result

let test_var_binding () =
  let result = Semantic_mapper.var_binding "fill" "colors/primary" in
  check string "var binding" "$fill:colors/primary" result

let tier4_tests = [
  "blend mode normal", `Quick, test_blend_mode_normal;
  "blend mode multiply", `Quick, test_blend_mode_multiply;
  "constraint h left", `Quick, test_constraint_h_left;
  "constraint h right", `Quick, test_constraint_h_right;
  "constraint h leftright", `Quick, test_constraint_h_leftright;
  "constraint v top", `Quick, test_constraint_v_top;
  "constraint v bottom", `Quick, test_constraint_v_bottom;
  "grid pattern columns", `Quick, test_grid_pattern_columns;
  "grid pattern rows", `Quick, test_grid_pattern_rows;
  "variable binding", `Quick, test_var_binding;
]

(** ============== Integration Tests ============== *)

let test_join_attrs () =
  let attrs = ["row"; "g:12"; ""; "p:16"; ""] in
  let result = Semantic_mapper.join_attrs attrs in
  check string "join attrs" "row g:12 p:16" result

let test_join_attrs_empty () =
  let attrs = [""; ""; ""] in
  let result = Semantic_mapper.join_attrs attrs in
  check string "join empty" "" result

let integration_tests = [
  "join attrs", `Quick, test_join_attrs;
  "join empty attrs", `Quick, test_join_attrs_empty;
]

(** ============== 메인 ============== *)

let () =
  run "Semantic Mapper" [
    "Tier 1 (High ROI)", tier1_tests;
    "Tier 2 (Medium ROI)", tier2_tests;
    "Tier 3 (Low ROI)", tier3_tests;
    "Tier 4 (Specialist)", tier4_tests;
    "Integration", integration_tests;
  ]
