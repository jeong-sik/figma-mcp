(** P0 CSS Fidelity Gap 테스트 스위트 *)

open Alcotest
open Figma_types
open Figma_codegen

(** ============== P0-4: Gradient 변환 테스트 ============== *)

let test_gradient_to_css_basic () =
  let stops = [
    (0.0, { r = 1.0; g = 0.0; b = 0.0; a = 1.0 });  (* Red *)
    (1.0, { r = 0.0; g = 0.0; b = 1.0; a = 1.0 });  (* Blue *)
  ] in
  let result = gradient_to_css stops in
  check bool "should produce Some" true (Option.is_some result);
  let css = Option.get result in
  check bool "contains linear-gradient" true (String.sub css 0 15 = "linear-gradient");
  check bool "contains red" true (String.length (Str.global_replace (Str.regexp "#FF0000") "" css) < String.length css);
  check bool "contains blue" true (String.length (Str.global_replace (Str.regexp "#0000FF") "" css) < String.length css)

let test_gradient_to_css_empty () =
  let stops = [] in
  let result = gradient_to_css stops in
  check bool "empty stops returns None" true (Option.is_none result)

let test_gradient_to_css_three_stops () =
  let stops = [
    (0.0, { r = 1.0; g = 0.0; b = 0.0; a = 1.0 });
    (0.5, { r = 0.0; g = 1.0; b = 0.0; a = 1.0 });
    (1.0, { r = 0.0; g = 0.0; b = 1.0; a = 1.0 });
  ] in
  let result = gradient_to_css stops in
  check bool "should produce Some" true (Option.is_some result);
  let css = Option.get result in
  check bool "contains 50%" true (String.length (Str.global_replace (Str.regexp "50%") "" css) < String.length css)

let test_gradient_to_css_precise () =
  let stops = [
    (0.0, { r = 0.5; g = 0.5; b = 0.5; a = 1.0 });
    (1.0, { r = 1.0; g = 1.0; b = 1.0; a = 1.0 });
  ] in
  let result = gradient_to_css ~precise:true stops in
  check bool "precise mode returns Some" true (Option.is_some result);
  let css = Option.get result in
  (* precise 모드는 rgb() 형식 사용 *)
  check bool "contains rgb(" true (String.length (Str.global_replace (Str.regexp "rgb(") "" css) < String.length css)

let gradient_tests = [
  "basic two-stop gradient", `Quick, test_gradient_to_css_basic;
  "empty gradient returns None", `Quick, test_gradient_to_css_empty;
  "three-stop gradient", `Quick, test_gradient_to_css_three_stops;
  "precise mode uses rgb()", `Quick, test_gradient_to_css_precise;
]

(** ============== P0-3: Effects 변환 테스트 ============== *)

let test_effects_drop_shadow () =
  let effects = [{
    fx_type = DropShadow;
    visible = true;
    radius = 10.0;
    offset = Some (4.0, 4.0);
    spread = Some 2.0;
    color = Some { r = 0.0; g = 0.0; b = 0.0; a = 0.25 };
  }] in
  let css = effects_to_css effects in
  check bool "contains box-shadow" true (String.length (Str.global_replace (Str.regexp "box-shadow:") "" css) < String.length css);
  check bool "has offset 4px" true (String.length (Str.global_replace (Str.regexp "4px") "" css) < String.length css);
  check bool "has blur 10px" true (String.length (Str.global_replace (Str.regexp "10px") "" css) < String.length css)

let test_effects_inner_shadow () =
  let effects = [{
    fx_type = InnerShadow;
    visible = true;
    radius = 5.0;
    offset = Some (2.0, 2.0);
    spread = None;
    color = Some { r = 1.0; g = 1.0; b = 1.0; a = 0.5 };
  }] in
  let css = effects_to_css effects in
  check bool "contains inset" true (String.length (Str.global_replace (Str.regexp "inset") "" css) < String.length css)

let test_effects_layer_blur () =
  let effects = [{
    fx_type = LayerBlur;
    visible = true;
    radius = 8.0;
    offset = None;
    spread = None;
    color = None;
  }] in
  let css = effects_to_css effects in
  check bool "contains filter:blur" true (String.length (Str.global_replace (Str.regexp "filter:blur") "" css) < String.length css)

let test_effects_background_blur () =
  let effects = [{
    fx_type = BackgroundBlur;
    visible = true;
    radius = 12.0;
    offset = None;
    spread = None;
    color = None;
  }] in
  let css = effects_to_css effects in
  check bool "contains backdrop-filter" true (String.length (Str.global_replace (Str.regexp "backdrop-filter:blur") "" css) < String.length css)

let test_effects_invisible_skipped () =
  let effects = [{
    fx_type = DropShadow;
    visible = false;  (* invisible - should be skipped *)
    radius = 10.0;
    offset = Some (4.0, 4.0);
    spread = None;
    color = Some { r = 0.0; g = 0.0; b = 0.0; a = 0.25 };
  }] in
  let css = effects_to_css effects in
  check string "invisible effect produces empty CSS" "" css

let test_effects_multiple () =
  let effects = [
    { fx_type = DropShadow; visible = true; radius = 10.0; offset = Some (4.0, 4.0); spread = None; color = Some { r = 0.0; g = 0.0; b = 0.0; a = 0.25 } };
    { fx_type = LayerBlur; visible = true; radius = 5.0; offset = None; spread = None; color = None };
  ] in
  let css = effects_to_css effects in
  check bool "contains box-shadow" true (String.length (Str.global_replace (Str.regexp "box-shadow") "" css) < String.length css);
  check bool "contains filter:blur" true (String.length (Str.global_replace (Str.regexp "filter:blur") "" css) < String.length css)

let effects_tests = [
  "drop shadow", `Quick, test_effects_drop_shadow;
  "inner shadow has inset", `Quick, test_effects_inner_shadow;
  "layer blur uses filter", `Quick, test_effects_layer_blur;
  "background blur uses backdrop-filter", `Quick, test_effects_background_blur;
  "invisible effect skipped", `Quick, test_effects_invisible_skipped;
  "multiple effects combined", `Quick, test_effects_multiple;
]

(** ============== 테스트 실행 ============== *)

let () =
  run "P0 CSS Fidelity" [
    "P0-4: gradient_to_css", gradient_tests;
    "P0-3: effects_to_css", effects_tests;
  ]
