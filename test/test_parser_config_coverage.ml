(** Comprehensive coverage tests for figma_parser.ml and figma_config.ml *)

open Alcotest
open Figma_types

(* ================================================================
   Helper: set/unset env vars safely during a test
   ================================================================ *)
let with_env name value f =
  let prev = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Unix.putenv name v
      | None ->
          (* Unix has no unsetenv; set to empty as best effort *)
          Unix.putenv name "")
    f

(* For "unset" testing, we use env var names with a unique prefix
   that are extremely unlikely to be set in the environment.
   This avoids the problem that Unix.putenv "" still makes Sys.getenv_opt
   return Some "". *)
let _unused_env_counter = ref 0
let fresh_env_name prefix =
  incr _unused_env_counter;
  Printf.sprintf "__NEVER_SET_%s_%d_%d" prefix !_unused_env_counter (Unix.getpid ())

(* ================================================================
   JSON construction helpers (Yojson.Safe)
   ================================================================ *)
let jobj fields : Yojson.Safe.t = `Assoc fields
let jstr s : Yojson.Safe.t = `String s
let jint i : Yojson.Safe.t = `Int i
let jfloat f : Yojson.Safe.t = `Float f
let jbool b : Yojson.Safe.t = `Bool b
let jlist l : Yojson.Safe.t = `List l
let jnull : Yojson.Safe.t = `Null

(* ================================================================
   1. JSON HELPERS: get_string / get_float / get_int / get_bool /
      get_list / get_assoc
   ================================================================ *)

let test_get_string_found () =
  let j = jobj [("k", jstr "hello")] in
  check (option string) "found" (Some "hello") (Figma_parser.get_string j "k")

let test_get_string_missing () =
  let j = jobj [("x", jstr "y")] in
  check (option string) "missing key" None (Figma_parser.get_string j "k")

let test_get_string_wrong_type () =
  let j = jobj [("k", jint 42)] in
  check (option string) "wrong type" None (Figma_parser.get_string j "k")

let test_get_string_non_assoc () =
  check (option string) "non-assoc" None (Figma_parser.get_string (jstr "s") "k")

let test_get_float_found () =
  let j = jobj [("k", jfloat 3.14)] in
  check (option (float 0.001)) "float found" (Some 3.14) (Figma_parser.get_float j "k")

let test_get_float_from_int () =
  let j = jobj [("k", jint 7)] in
  check (option (float 0.001)) "int -> float" (Some 7.0) (Figma_parser.get_float j "k")

let test_get_float_missing () =
  let j = jobj [] in
  check (option (float 0.001)) "missing" None (Figma_parser.get_float j "k")

let test_get_float_wrong_type () =
  let j = jobj [("k", jstr "nope")] in
  check (option (float 0.001)) "wrong type" None (Figma_parser.get_float j "k")

let test_get_float_non_assoc () =
  check (option (float 0.001)) "non-assoc" None (Figma_parser.get_float (jlist []) "k")

let test_get_int_found () =
  let j = jobj [("k", jint 42)] in
  check (option int) "int found" (Some 42) (Figma_parser.get_int j "k")

let test_get_int_from_float () =
  let j = jobj [("k", jfloat 9.7)] in
  check (option int) "float -> int" (Some 9) (Figma_parser.get_int j "k")

let test_get_int_missing () =
  let j = jobj [] in
  check (option int) "missing" None (Figma_parser.get_int j "k")

let test_get_int_wrong_type () =
  let j = jobj [("k", jbool true)] in
  check (option int) "wrong type" None (Figma_parser.get_int j "k")

let test_get_int_non_assoc () =
  check (option int) "non-assoc" None (Figma_parser.get_int jnull "k")

let test_get_bool_found () =
  let j = jobj [("k", jbool true)] in
  check (option bool) "bool found" (Some true) (Figma_parser.get_bool j "k")

let test_get_bool_false () =
  let j = jobj [("k", jbool false)] in
  check (option bool) "bool false" (Some false) (Figma_parser.get_bool j "k")

let test_get_bool_missing () =
  let j = jobj [] in
  check (option bool) "missing" None (Figma_parser.get_bool j "k")

let test_get_bool_wrong_type () =
  let j = jobj [("k", jstr "true")] in
  check (option bool) "wrong type" None (Figma_parser.get_bool j "k")

let test_get_bool_non_assoc () =
  check (option bool) "non-assoc" None (Figma_parser.get_bool (jint 1) "k")

let test_get_list_found () =
  let j = jobj [("k", jlist [jint 1; jint 2])] in
  let result = Figma_parser.get_list j "k" in
  check bool "is Some" true (Option.is_some result);
  check int "len" 2 (List.length (Option.get result))

let test_get_list_missing () =
  let j = jobj [] in
  check bool "missing" true (Option.is_none (Figma_parser.get_list j "k"))

let test_get_list_wrong_type () =
  let j = jobj [("k", jstr "not a list")] in
  check bool "wrong type" true (Option.is_none (Figma_parser.get_list j "k"))

let test_get_list_non_assoc () =
  check bool "non-assoc" true (Option.is_none (Figma_parser.get_list (jfloat 1.0) "k"))

let test_get_assoc_found () =
  let inner = jobj [("nested", jint 1)] in
  let j = jobj [("k", inner)] in
  check bool "found" true (Option.is_some (Figma_parser.get_assoc j "k"))

let test_get_assoc_missing () =
  let j = jobj [] in
  check bool "missing" true (Option.is_none (Figma_parser.get_assoc j "k"))

let test_get_assoc_wrong_type () =
  let j = jobj [("k", jlist [])] in
  check bool "wrong type" true (Option.is_none (Figma_parser.get_assoc j "k"))

let test_get_assoc_non_assoc () =
  check bool "non-assoc" true (Option.is_none (Figma_parser.get_assoc (jstr "x") "k"))

(* ================================================================
   2. Operators: >>= and |?
   ================================================================ *)

let test_bind_some () =
  let open Figma_parser in
  let result = Some 5 >>= (fun x -> Some (x + 1)) in
  check (option int) "bind some" (Some 6) result

let test_bind_none () =
  let open Figma_parser in
  let result = None >>= (fun x -> Some (x + 1)) in
  check (option int) "bind none" None result

let test_default_some () =
  let open Figma_parser in
  let result = Some 10 |? 0 in
  check int "default some" 10 result

let test_default_none () =
  let open Figma_parser in
  let result = None |? 42 in
  check int "default none" 42 result

(* ================================================================
   3. RGBA parsing
   ================================================================ *)

let rgba_eq a b =
  Float.abs (a.r -. b.r) < 0.001 &&
  Float.abs (a.g -. b.g) < 0.001 &&
  Float.abs (a.b -. b.b) < 0.001 &&
  Float.abs (a.a -. b.a) < 0.001

let pp_rgba fmt c =
  Format.fprintf fmt "{r=%.3f;g=%.3f;b=%.3f;a=%.3f}" c.r c.g c.b c.a

let rgba_t = testable pp_rgba rgba_eq

let test_parse_rgba_full () =
  let j = jobj [("r", jfloat 0.5); ("g", jfloat 0.6); ("b", jfloat 0.7); ("a", jfloat 0.8)] in
  let c = Figma_parser.parse_rgba j in
  check rgba_t "full" { r=0.5; g=0.6; b=0.7; a=0.8 } c

let test_parse_rgba_defaults () =
  let j = jobj [] in
  let c = Figma_parser.parse_rgba j in
  check rgba_t "defaults" { r=0.0; g=0.0; b=0.0; a=1.0 } c

let test_parse_rgba_int_values () =
  let j = jobj [("r", jint 1); ("g", jint 0); ("b", jint 1); ("a", jint 1)] in
  let c = Figma_parser.parse_rgba j in
  check rgba_t "int values" { r=1.0; g=0.0; b=1.0; a=1.0 } c

let test_parse_rgba_partial () =
  let j = jobj [("r", jfloat 0.3)] in
  let c = Figma_parser.parse_rgba j in
  check rgba_t "partial" { r=0.3; g=0.0; b=0.0; a=1.0 } c

(* ================================================================
   4. Paint type + paint parsing
   ================================================================ *)

let test_parse_paint_type_all_variants () =
  let cases = [
    ("SOLID", Solid); ("IMAGE", Image);
    ("GRADIENT_LINEAR", GradientLinear); ("GRADIENT_RADIAL", GradientRadial);
    ("GRADIENT_ANGULAR", GradientAngular); ("GRADIENT_DIAMOND", GradientDiamond);
    ("EMOJI", Emoji); ("UNKNOWN_TYPE", Solid);
  ] in
  List.iter (fun (s, expected) ->
    let got = Figma_parser.parse_paint_type s in
    check bool (Printf.sprintf "paint_type %s" s) true (got = expected)
  ) cases

let test_parse_paint_solid () =
  let j = jobj [
    ("type", jstr "SOLID");
    ("visible", jbool true);
    ("opacity", jfloat 0.9);
    ("color", jobj [("r", jfloat 1.0); ("g", jfloat 0.0); ("b", jfloat 0.0); ("a", jfloat 1.0)]);
  ] in
  let p = Figma_parser.parse_paint j in
  check bool "type" true (p.paint_type = Solid);
  check bool "visible" true p.visible;
  check (float 0.001) "opacity" 0.9 p.opacity;
  check bool "has color" true (Option.is_some p.color)

let test_parse_paint_image () =
  let j = jobj [
    ("type", jstr "IMAGE");
    ("imageRef", jstr "ref123");
    ("scaleMode", jstr "FILL");
  ] in
  let p = Figma_parser.parse_paint j in
  check bool "type" true (p.paint_type = Image);
  check (option string) "imageRef" (Some "ref123") p.image_ref;
  check (option string) "scaleMode" (Some "FILL") p.scale_mode

let test_parse_paint_gradient () =
  let stop1 = jobj [
    ("position", jfloat 0.0);
    ("color", jobj [("r", jfloat 1.0); ("g", jfloat 0.0); ("b", jfloat 0.0); ("a", jfloat 1.0)]);
  ] in
  let stop2 = jobj [
    ("position", jfloat 1.0);
    ("color", jobj [("r", jfloat 0.0); ("g", jfloat 0.0); ("b", jfloat 1.0); ("a", jfloat 1.0)]);
  ] in
  let j = jobj [
    ("type", jstr "GRADIENT_LINEAR");
    ("gradientStops", jlist [stop1; stop2]);
  ] in
  let p = Figma_parser.parse_paint j in
  check bool "type" true (p.paint_type = GradientLinear);
  check int "gradient_stops count" 2 (List.length p.gradient_stops)

let test_parse_paint_gradient_bad_stop () =
  (* stop without position or color -> filtered out *)
  let bad_stop = jobj [("position", jfloat 0.5)] in  (* no color *)
  let j = jobj [
    ("type", jstr "GRADIENT_RADIAL");
    ("gradientStops", jlist [bad_stop]);
  ] in
  let p = Figma_parser.parse_paint j in
  check int "bad stops filtered" 0 (List.length p.gradient_stops)

let test_parse_paint_no_type () =
  let j = jobj [] in
  let p = Figma_parser.parse_paint j in
  check bool "default type" true (p.paint_type = Solid)

let test_parse_paint_invisible () =
  let j = jobj [("visible", jbool false)] in
  let p = Figma_parser.parse_paint j in
  check bool "invisible" false p.visible

let test_parse_paints_list () =
  let p1 = jobj [("type", jstr "SOLID")] in
  let p2 = jobj [("type", jstr "IMAGE")] in
  let j = jobj [("fills", jlist [p1; jstr "bad"; p2])] in
  let paints = Figma_parser.parse_paints j "fills" in
  (* non-assoc entries are filtered out *)
  check int "count" 2 (List.length paints)

let test_parse_paints_missing () =
  let j = jobj [] in
  let paints = Figma_parser.parse_paints j "fills" in
  check int "empty" 0 (List.length paints)

let test_parse_paint_emoji () =
  let j = jobj [("type", jstr "EMOJI")] in
  let p = Figma_parser.parse_paint j in
  check bool "emoji" true (p.paint_type = Emoji)

let test_parse_paint_gradient_angular () =
  let j = jobj [("type", jstr "GRADIENT_ANGULAR")] in
  let p = Figma_parser.parse_paint j in
  check bool "angular" true (p.paint_type = GradientAngular)

let test_parse_paint_gradient_diamond () =
  let j = jobj [("type", jstr "GRADIENT_DIAMOND")] in
  let p = Figma_parser.parse_paint j in
  check bool "diamond" true (p.paint_type = GradientDiamond)

(* ================================================================
   5. Effect parsing
   ================================================================ *)

let test_parse_effect_type_all () =
  let cases = [
    ("INNER_SHADOW", InnerShadow); ("DROP_SHADOW", DropShadow);
    ("LAYER_BLUR", LayerBlur); ("BACKGROUND_BLUR", BackgroundBlur);
    ("UNKNOWN_FX", DropShadow);
  ] in
  List.iter (fun (s, expected) ->
    check bool (Printf.sprintf "effect_type %s" s) true
      (Figma_parser.parse_effect_type s = expected)
  ) cases

let test_parse_fx_shadow () =
  let j = jobj [
    ("type", jstr "DROP_SHADOW");
    ("visible", jbool true);
    ("radius", jfloat 4.0);
    ("color", jobj [("r", jfloat 0.0); ("g", jfloat 0.0); ("b", jfloat 0.0); ("a", jfloat 0.25)]);
    ("offset", jobj [("x", jfloat 2.0); ("y", jfloat 3.0)]);
    ("spread", jfloat 1.5);
  ] in
  let fx = Figma_parser.parse_fx j in
  check bool "type" true (fx.fx_type = DropShadow);
  check bool "visible" true fx.visible;
  check (float 0.001) "radius" 4.0 fx.radius;
  check bool "has color" true (Option.is_some fx.color);
  check bool "has offset" true (Option.is_some fx.offset);
  (match fx.offset with
   | Some (x, y) ->
     check (float 0.001) "offset x" 2.0 x;
     check (float 0.001) "offset y" 3.0 y
   | None -> fail "expected offset");
  check (option (float 0.001)) "spread" (Some 1.5) fx.spread

let test_parse_fx_blur_no_offset () =
  let j = jobj [
    ("type", jstr "LAYER_BLUR");
    ("radius", jfloat 10.0);
  ] in
  let fx = Figma_parser.parse_fx j in
  check bool "type" true (fx.fx_type = LayerBlur);
  check bool "no offset" true (Option.is_none fx.offset);
  check bool "no spread" true (Option.is_none fx.spread)

let test_parse_fx_inner_shadow () =
  let j = jobj [("type", jstr "INNER_SHADOW")] in
  let fx = Figma_parser.parse_fx j in
  check bool "inner shadow" true (fx.fx_type = InnerShadow)

let test_parse_fx_background_blur () =
  let j = jobj [("type", jstr "BACKGROUND_BLUR")] in
  let fx = Figma_parser.parse_fx j in
  check bool "bg blur" true (fx.fx_type = BackgroundBlur)

let test_parse_fx_defaults () =
  let j = jobj [] in
  let fx = Figma_parser.parse_fx j in
  check bool "default type" true (fx.fx_type = DropShadow);
  check bool "default visible" true fx.visible;
  check (float 0.001) "default radius" 0.0 fx.radius;
  check bool "no color" true (Option.is_none fx.color);
  check bool "no offset" true (Option.is_none fx.offset)

let test_parse_fx_invisible () =
  let j = jobj [("visible", jbool false)] in
  let fx = Figma_parser.parse_fx j in
  check bool "invisible" false fx.visible

let test_parse_effects_list () =
  let e1 = jobj [("type", jstr "DROP_SHADOW")] in
  let e2 = jobj [("type", jstr "LAYER_BLUR")] in
  let j = jobj [("effects", jlist [e1; jint 999; e2])] in
  let effects = Figma_parser.parse_effects j in
  check int "count (non-assoc filtered)" 2 (List.length effects)

let test_parse_effects_missing () =
  let j = jobj [] in
  let effects = Figma_parser.parse_effects j in
  check int "empty" 0 (List.length effects)

let test_parse_fx_offset_defaults () =
  (* offset present but x/y missing -> default 0.0, 0.0 *)
  let j = jobj [("offset", jobj [])] in
  let fx = Figma_parser.parse_fx j in
  (match fx.offset with
   | Some (x, y) ->
     check (float 0.001) "x" 0.0 x;
     check (float 0.001) "y" 0.0 y
   | None -> fail "expected offset")

(* ================================================================
   6. BBox parsing
   ================================================================ *)

let test_parse_bbox_full () =
  let j = jobj [("absoluteBoundingBox", jobj [
    ("x", jfloat 10.0); ("y", jfloat 20.0);
    ("width", jfloat 100.0); ("height", jfloat 50.0);
  ])] in
  match Figma_parser.parse_bbox j with
  | Some b ->
    check (float 0.001) "x" 10.0 b.x;
    check (float 0.001) "y" 20.0 b.y;
    check (float 0.001) "w" 100.0 b.width;
    check (float 0.001) "h" 50.0 b.height
  | None -> fail "expected bbox"

let test_parse_bbox_missing () =
  let j = jobj [] in
  check bool "no bbox" true (Option.is_none (Figma_parser.parse_bbox j))

let test_parse_bbox_incomplete () =
  let j = jobj [("absoluteBoundingBox", jobj [
    ("x", jfloat 10.0); ("y", jfloat 20.0);
    (* missing width and height *)
  ])] in
  check bool "incomplete" true (Option.is_none (Figma_parser.parse_bbox j))

let test_parse_bbox_partial_fields () =
  let j = jobj [("absoluteBoundingBox", jobj [
    ("x", jfloat 1.0); ("y", jfloat 2.0); ("width", jfloat 3.0);
    (* height missing *)
  ])] in
  check bool "partial" true (Option.is_none (Figma_parser.parse_bbox j))

(* ================================================================
   7. Layout parsing
   ================================================================ *)

let test_parse_layout_mode_vertical () =
  let j = jobj [("layoutMode", jstr "VERTICAL")] in
  check bool "vertical" true (Figma_parser.parse_layout_mode j = Vertical)

let test_parse_layout_mode_horizontal () =
  let j = jobj [("layoutMode", jstr "HORIZONTAL")] in
  check bool "horizontal" true (Figma_parser.parse_layout_mode j = Horizontal)

let test_parse_layout_mode_none () =
  let j = jobj [] in
  check bool "none" true (Figma_parser.parse_layout_mode j = None')

let test_parse_layout_mode_unknown () =
  let j = jobj [("layoutMode", jstr "GRID")] in
  check bool "unknown -> None'" true (Figma_parser.parse_layout_mode j = None')

let test_parse_layout_align_all () =
  let cases = [
    ("MIN", Min); ("CENTER", Center); ("MAX", Max);
    ("SPACE_BETWEEN", SpaceBetween); ("BASELINE", Baseline);
    ("UNKNOWN", Min);
  ] in
  List.iter (fun (s, expected) ->
    check bool (Printf.sprintf "align %s" s) true
      (Figma_parser.parse_layout_align s = expected)
  ) cases

let test_parse_layout_sizing_all () =
  let cases = [("FIXED", Fixed); ("HUG", Hug); ("FILL", Fill); ("X", Fixed)] in
  List.iter (fun (s, expected) ->
    check bool (Printf.sprintf "sizing %s" s) true
      (Figma_parser.parse_layout_sizing s = expected)
  ) cases

(* ================================================================
   8. Typography parsing
   ================================================================ *)

let test_parse_text_align_h_all () =
  let cases = [("LEFT", Left); ("CENTER", Center); ("RIGHT", Right); ("JUSTIFIED", Justified); ("OOPS", Left)] in
  List.iter (fun (s, expected) ->
    check bool (Printf.sprintf "text_align_h %s" s) true
      (Figma_parser.parse_text_align_h s = expected)
  ) cases

let test_parse_text_align_v_all () =
  let cases = [("TOP", Top); ("CENTER", Center); ("BOTTOM", Bottom); ("OOPS", Top)] in
  List.iter (fun (s, expected) ->
    check bool (Printf.sprintf "text_align_v %s" s) true
      (Figma_parser.parse_text_align_v s = expected)
  ) cases

let test_parse_text_decoration_all () =
  let cases = [("UNDERLINE", Underline); ("STRIKETHROUGH", Strikethrough); ("NONE", NoDeco); ("X", NoDeco)] in
  List.iter (fun (s, expected) ->
    check bool (Printf.sprintf "text_deco %s" s) true
      (Figma_parser.parse_text_decoration s = expected)
  ) cases

let test_parse_text_case_all () =
  let cases = [
    ("UPPER", Upper); ("LOWER", Lower); ("TITLE", Title);
    ("SMALL_CAPS", SmallCaps); ("SMALL_CAPS_FORCED", SmallCapsForced);
    ("ORIGINAL", Original); ("OOPS", Original);
  ] in
  List.iter (fun (s, expected) ->
    check bool (Printf.sprintf "text_case %s" s) true
      (Figma_parser.parse_text_case s = expected)
  ) cases

let test_parse_typography_full () =
  let style = jobj [
    ("fontFamily", jstr "Roboto");
    ("fontSize", jfloat 18.0);
    ("fontWeight", jint 700);
    ("fontPostScriptName", jstr "Roboto-Bold");
    ("lineHeightPx", jfloat 24.0);
    ("letterSpacing", jfloat 0.5);
    ("textAlignHorizontal", jstr "CENTER");
    ("textAlignVertical", jstr "BOTTOM");
    ("textDecoration", jstr "UNDERLINE");
    ("textCase", jstr "UPPER");
  ] in
  let j = jobj [("style", style)] in
  match Figma_parser.parse_typography j with
  | Some t ->
    check string "font" "Roboto" t.font_family;
    check (float 0.001) "size" 18.0 t.font_size;
    check int "weight" 700 t.font_weight;
    check string "style" "Roboto-Bold" t.font_style;
    check (option (float 0.001)) "line_height" (Some 24.0) t.line_height;
    check (option (float 0.001)) "letter_spacing" (Some 0.5) t.letter_spacing;
    check bool "align_h" true (t.text_align_h = Center);
    check bool "align_v" true (t.text_align_v = Bottom);
    check bool "decoration" true (t.text_decoration = Underline);
    check bool "text_case" true (t.text_case = Upper)
  | None -> fail "expected typography"

let test_parse_typography_defaults () =
  let style = jobj [] in
  let j = jobj [("style", style)] in
  match Figma_parser.parse_typography j with
  | Some t ->
    check string "font" "Inter" t.font_family;
    check (float 0.001) "size" 14.0 t.font_size;
    check int "weight" 400 t.font_weight;
    check string "style" "normal" t.font_style;
    check bool "no line_height" true (Option.is_none t.line_height);
    check bool "no letter_spacing" true (Option.is_none t.letter_spacing);
    check bool "align_h" true (t.text_align_h = Left);
    check bool "align_v" true (t.text_align_v = Top);
    check bool "decoration" true (t.text_decoration = NoDeco);
    check bool "text_case" true (t.text_case = Original)
  | None -> fail "expected typography"

let test_parse_typography_no_style () =
  let j = jobj [] in
  check bool "no style" true (Option.is_none (Figma_parser.parse_typography j))

(* ================================================================
   9. Node parsing
   ================================================================ *)

let test_parse_node_minimal () =
  let j = jobj [("id", jstr "1:1"); ("name", jstr "Rect"); ("type", jstr "RECTANGLE")] in
  match Figma_parser.parse_node j with
  | Some n ->
    check string "id" "1:1" n.id;
    check string "name" "Rect" n.name;
    check bool "type" true (n.node_type = Rectangle);
    check bool "visible" true n.visible;
    check bool "locked" false n.locked
  | None -> fail "expected node"

let test_parse_node_text_with_typography () =
  let style = jobj [("fontFamily", jstr "Arial"); ("fontSize", jfloat 16.0); ("fontWeight", jint 400); ("fontPostScriptName", jstr "normal")] in
  let j = jobj [
    ("id", jstr "2:1"); ("name", jstr "Label"); ("type", jstr "TEXT");
    ("characters", jstr "Hello World");
    ("style", style);
  ] in
  match Figma_parser.parse_node j with
  | Some n ->
    check bool "type" true (n.node_type = Text);
    check (option string) "chars" (Some "Hello World") n.characters;
    check bool "has typo" true (Option.is_some n.typography)
  | None -> fail "expected node"

let test_parse_node_children () =
  let child = jobj [("id", jstr "c1"); ("name", jstr "child"); ("type", jstr "RECTANGLE")] in
  let j = jobj [
    ("id", jstr "p1"); ("name", jstr "parent"); ("type", jstr "FRAME");
    ("children", jlist [child]);
  ] in
  match Figma_parser.parse_node j with
  | Some n ->
    check int "children" 1 (List.length n.children);
    check string "child id" "c1" (List.hd n.children).id
  | None -> fail "expected node"

let test_parse_node_depth_limit () =
  let j = jobj [("id", jstr "x"); ("name", jstr "x"); ("type", jstr "FRAME")] in
  check bool "over limit" true
    (Option.is_none (Figma_parser.parse_node ~depth:21 ~max_depth:20 j))

let test_parse_node_at_max_depth () =
  let j = jobj [("id", jstr "x"); ("name", jstr "x"); ("type", jstr "FRAME")] in
  check bool "at limit" true
    (Option.is_some (Figma_parser.parse_node ~depth:20 ~max_depth:20 j))

let test_parse_node_border_radii_float () =
  let j = jobj [
    ("id", jstr "r1"); ("name", jstr "r"); ("type", jstr "RECTANGLE");
    ("rectangleCornerRadii", jlist [jfloat 1.0; jfloat 2.0; jfloat 3.0; jfloat 4.0]);
  ] in
  match Figma_parser.parse_node j with
  | Some n ->
    check bool "has radii" true (Option.is_some n.border_radii);
    (match n.border_radii with
     | Some (tl, tr, br, bl) ->
       check (float 0.001) "tl" 1.0 tl;
       check (float 0.001) "tr" 2.0 tr;
       check (float 0.001) "br" 3.0 br;
       check (float 0.001) "bl" 4.0 bl
     | None -> fail "expected radii")
  | None -> fail "expected node"

let test_parse_node_border_radii_int () =
  let j = jobj [
    ("id", jstr "r2"); ("name", jstr "r"); ("type", jstr "RECTANGLE");
    ("rectangleCornerRadii", jlist [jint 5; jint 6; jint 7; jint 8]);
  ] in
  match Figma_parser.parse_node j with
  | Some n ->
    (match n.border_radii with
     | Some (tl, tr, br, bl) ->
       check (float 0.001) "tl" 5.0 tl;
       check (float 0.001) "tr" 6.0 tr;
       check (float 0.001) "br" 7.0 br;
       check (float 0.001) "bl" 8.0 bl
     | None -> fail "expected int radii")
  | None -> fail "expected node"

let test_parse_node_border_radii_none () =
  let j = jobj [("id", jstr "r3"); ("name", jstr "r"); ("type", jstr "RECTANGLE")] in
  match Figma_parser.parse_node j with
  | Some n -> check bool "no radii" true (Option.is_none n.border_radii)
  | None -> fail "expected node"

let test_parse_node_border_radii_wrong_count () =
  (* 3 elements instead of 4 -> None *)
  let j = jobj [
    ("id", jstr "r4"); ("name", jstr "r"); ("type", jstr "RECTANGLE");
    ("rectangleCornerRadii", jlist [jfloat 1.0; jfloat 2.0; jfloat 3.0]);
  ] in
  match Figma_parser.parse_node j with
  | Some n -> check bool "wrong count" true (Option.is_none n.border_radii)
  | None -> fail "expected node"

let test_parse_node_all_types () =
  let types = [
    "DOCUMENT"; "CANVAS"; "FRAME"; "GROUP"; "VECTOR";
    "BOOLEAN_OPERATION"; "STAR"; "LINE"; "ELLIPSE"; "REGULAR_POLYGON";
    "RECTANGLE"; "TEXT"; "SLICE"; "COMPONENT"; "COMPONENT_SET";
    "INSTANCE"; "STICKY"; "SECTION"; "MYSTERY_TYPE";
  ] in
  List.iter (fun t ->
    let j = jobj [("id", jstr "n"); ("name", jstr "n"); ("type", jstr t)] in
    match Figma_parser.parse_node j with
    | Some n ->
      let expected = node_type_of_string t in
      check bool (Printf.sprintf "node type %s" t) true (n.node_type = expected)
    | None -> fail (Printf.sprintf "failed to parse type %s" t)
  ) types

let test_parse_node_no_type () =
  let j = jobj [("id", jstr "n"); ("name", jstr "n")] in
  match Figma_parser.parse_node j with
  | Some n -> check bool "default Frame" true (n.node_type = Frame)
  | None -> fail "expected node"

let test_parse_node_layout () =
  let j = jobj [
    ("id", jstr "l1"); ("name", jstr "layout"); ("type", jstr "FRAME");
    ("layoutMode", jstr "HORIZONTAL");
    ("paddingTop", jfloat 8.0); ("paddingRight", jfloat 12.0);
    ("paddingBottom", jfloat 8.0); ("paddingLeft", jfloat 12.0);
    ("itemSpacing", jfloat 4.0);
    ("primaryAxisAlignItems", jstr "SPACE_BETWEEN");
    ("counterAxisAlignItems", jstr "CENTER");
    ("layoutSizingHorizontal", jstr "FILL");
    ("layoutSizingVertical", jstr "HUG");
  ] in
  match Figma_parser.parse_node j with
  | Some n ->
    check bool "horizontal" true (n.layout_mode = Horizontal);
    let pt, pr, pb, pl = n.padding in
    check (float 0.001) "pt" 8.0 pt;
    check (float 0.001) "pr" 12.0 pr;
    check (float 0.001) "pb" 8.0 pb;
    check (float 0.001) "pl" 12.0 pl;
    check (float 0.001) "gap" 4.0 n.gap;
    check bool "primary" true (n.primary_axis_align = SpaceBetween);
    check bool "counter" true (n.counter_axis_align = Center);
    check bool "sizing h" true (n.layout_sizing_h = Fill);
    check bool "sizing v" true (n.layout_sizing_v = Hug)
  | None -> fail "expected node"

let test_parse_node_locked_invisible () =
  let j = jobj [
    ("id", jstr "n1"); ("name", jstr "n"); ("type", jstr "FRAME");
    ("visible", jbool false); ("locked", jbool true);
  ] in
  match Figma_parser.parse_node j with
  | Some n ->
    check bool "invisible" false n.visible;
    check bool "locked" true n.locked
  | None -> fail "expected node"

let test_parse_node_fills_strokes_effects () =
  let fill = jobj [("type", jstr "SOLID"); ("color", jobj [("r", jfloat 1.0); ("g", jfloat 0.0); ("b", jfloat 0.0); ("a", jfloat 1.0)])] in
  let stroke = jobj [("type", jstr "SOLID"); ("color", jobj [("r", jfloat 0.0); ("g", jfloat 1.0); ("b", jfloat 0.0); ("a", jfloat 1.0)])] in
  let fx_json = jobj [("type", jstr "DROP_SHADOW"); ("radius", jfloat 4.0)] in
  let j = jobj [
    ("id", jstr "s1"); ("name", jstr "styled"); ("type", jstr "RECTANGLE");
    ("fills", jlist [fill]);
    ("strokes", jlist [stroke]);
    ("strokeWeight", jfloat 2.0);
    ("effects", jlist [fx_json]);
    ("opacity", jfloat 0.5);
    ("cornerRadius", jfloat 8.0);
    ("rotation", jfloat 45.0);
  ] in
  match Figma_parser.parse_node j with
  | Some n ->
    check int "fills" 1 (List.length n.fills);
    check int "strokes" 1 (List.length n.strokes);
    check (float 0.001) "stroke_weight" 2.0 n.stroke_weight;
    check int "effects" 1 (List.length n.effects);
    check (float 0.001) "opacity" 0.5 n.opacity;
    check (float 0.001) "border_radius" 8.0 n.border_radius;
    check (float 0.001) "rotation" 45.0 n.rotation
  | None -> fail "expected node"

let test_parse_node_component_id () =
  let j = jobj [
    ("id", jstr "i1"); ("name", jstr "inst"); ("type", jstr "INSTANCE");
    ("componentId", jstr "comp:123");
  ] in
  match Figma_parser.parse_node j with
  | Some n ->
    check (option string) "componentId" (Some "comp:123") n.component_id
  | None -> fail "expected node"

let test_parse_node_non_text_no_typography () =
  let style = jobj [("fontFamily", jstr "Arial")] in
  let j = jobj [
    ("id", jstr "f1"); ("name", jstr "frame"); ("type", jstr "FRAME");
    ("style", style);
  ] in
  match Figma_parser.parse_node j with
  | Some n -> check bool "no typography for non-text" true (Option.is_none n.typography)
  | None -> fail "expected node"

(* ================================================================
   10. parse_json_string and parse_json
   ================================================================ *)

let test_parse_json_string_valid () =
  let s = {|{"id":"1:1","name":"test","type":"RECTANGLE"}|} in
  match Figma_parser.parse_json_string s with
  | Some n -> check string "id" "1:1" n.id
  | None -> fail "expected parse"

let test_parse_json_string_invalid () =
  check bool "invalid json" true (Option.is_none (Figma_parser.parse_json_string "not json {"))

let test_parse_json () =
  let j = jobj [("id", jstr "j1"); ("name", jstr "j"); ("type", jstr "FRAME")] in
  match Figma_parser.parse_json j with
  | Some n -> check string "id" "j1" n.id
  | None -> fail "expected parse"


(* ================================================================
   PART 2: FIGMA_CONFIG TESTS
   ================================================================ *)

(* Note: Figma_config module values are computed at module load time.
   The get_* functions are still accessible for testing. We test the
   raw getter functions directly. *)

(* ================================================================
   11. get_string
   ================================================================ *)

let test_config_get_string_from_env () =
  with_env "__TEST_CFG_STR" "hello" (fun () ->
    check string "from env" "hello"
      (Figma_config.get_string ~default:"fallback" "__TEST_CFG_STR"))

let test_config_get_string_default () =
  let name = fresh_env_name "STR" in
  check string "default" "fallback"
    (Figma_config.get_string ~default:"fallback" name)

(* ================================================================
   12. get_int
   ================================================================ *)

let test_config_get_int_valid () =
  with_env "__TEST_CFG_INT" "42" (fun () ->
    check int "valid" 42
      (Figma_config.get_int ~default:0 "__TEST_CFG_INT"))

let test_config_get_int_invalid () =
  with_env "__TEST_CFG_INT" "notanum" (fun () ->
    check int "invalid -> default" 99
      (Figma_config.get_int ~default:99 "__TEST_CFG_INT"))

let test_config_get_int_default () =
  let name = fresh_env_name "INT" in
  check int "default" 7
    (Figma_config.get_int ~default:7 name)

(* ================================================================
   13. get_float
   ================================================================ *)

let test_config_get_float_valid () =
  with_env "__TEST_CFG_FLT" "3.14" (fun () ->
    check (float 0.001) "valid" 3.14
      (Figma_config.get_float ~default:0.0 "__TEST_CFG_FLT"))

let test_config_get_float_invalid () =
  with_env "__TEST_CFG_FLT" "nope" (fun () ->
    check (float 0.001) "invalid -> default" 1.5
      (Figma_config.get_float ~default:1.5 "__TEST_CFG_FLT"))

let test_config_get_float_default () =
  let name = fresh_env_name "FLT" in
  check (float 0.001) "default" 2.7
    (Figma_config.get_float ~default:2.7 name)

(* ================================================================
   14. get_bool — all 9+9+invalid+default branches
   ================================================================ *)

let test_config_get_bool_true_variants () =
  let trues = ["1"; "true"; "yes"; "y"; "on"; "TRUE"; "True"; "YES"; "ON"] in
  List.iter (fun v ->
    with_env "__TEST_CFG_BOOL" v (fun () ->
      check bool (Printf.sprintf "true variant %s" v) true
        (Figma_config.get_bool ~default:false "__TEST_CFG_BOOL"))
  ) trues

let test_config_get_bool_false_variants () =
  let falses = ["0"; "false"; "no"; "n"; "off"; "FALSE"; "False"; "NO"; "OFF"] in
  List.iter (fun v ->
    with_env "__TEST_CFG_BOOL" v (fun () ->
      check bool (Printf.sprintf "false variant %s" v) false
        (Figma_config.get_bool ~default:true "__TEST_CFG_BOOL"))
  ) falses

let test_config_get_bool_invalid () =
  with_env "__TEST_CFG_BOOL" "maybe" (fun () ->
    check bool "invalid -> default true" true
      (Figma_config.get_bool ~default:true "__TEST_CFG_BOOL");
    check bool "invalid -> default false" false
      (Figma_config.get_bool ~default:false "__TEST_CFG_BOOL"))

let test_config_get_bool_default () =
  let name = fresh_env_name "BOOL" in
  check bool "default true" true
    (Figma_config.get_bool ~default:true name);
  check bool "default false" false
    (Figma_config.get_bool ~default:false name)

(* ================================================================
   15. get_string_list
   ================================================================ *)

let test_config_get_string_list_csv () =
  with_env "__TEST_CFG_LST" "a, b , c" (fun () ->
    check (list string) "csv" ["a"; "b"; "c"]
      (Figma_config.get_string_list ~default:[] "__TEST_CFG_LST"))

let test_config_get_string_list_empty () =
  let name = fresh_env_name "LST" in
  check (list string) "default" ["x"; "y"]
    (Figma_config.get_string_list ~default:["x"; "y"] name)

let test_config_get_string_list_commas_only () =
  with_env "__TEST_CFG_LST" ",,," (fun () ->
    check (list string) "commas only -> default" ["fallback"]
      (Figma_config.get_string_list ~default:["fallback"] "__TEST_CFG_LST"))

let test_config_get_string_list_single () =
  with_env "__TEST_CFG_LST" "single" (fun () ->
    check (list string) "single item" ["single"]
      (Figma_config.get_string_list ~default:[] "__TEST_CFG_LST"))

(* ================================================================
   16. Module accessor smoke tests
       These confirm the modules are reachable and return values.
       Module-level values are already computed, so we just check types.
   ================================================================ *)

let test_config_cache_module () =
  check bool "dir is string" true (String.length Figma_config.Cache.dir > 0);
  check bool "ttl_hours > 0" true (Figma_config.Cache.ttl_hours > 0.0);
  check bool "ttl_variables_hours > 0" true (Figma_config.Cache.ttl_variables_hours > 0.0);
  check bool "ttl_search_hours > 0" true (Figma_config.Cache.ttl_search_hours > 0.0);
  check bool "l1_max > 0" true (Figma_config.Cache.l1_max > 0);
  check bool "l2_max_mb > 0" true (Figma_config.Cache.l2_max_mb > 0)

let test_config_plugin_module () =
  check bool "ttl > 0" true (Figma_config.Plugin.ttl_seconds > 0.0);
  check bool "poll_max > 0" true (Figma_config.Plugin.poll_max_ms > 0);
  check bool "max_commands > 0" true (Figma_config.Plugin.max_commands > 0);
  check bool "max_results > 0" true (Figma_config.Plugin.max_results > 0);
  check bool "max_payload > 0" true (Figma_config.Plugin.max_payload_bytes > 0);
  check bool "max_waiters > 0" true (Figma_config.Plugin.max_waiters > 0);
  check bool "result_ttl > 0" true (Figma_config.Plugin.result_ttl_seconds > 0.0);
  check bool "cleanup_interval > 0" true (Figma_config.Plugin.cleanup_interval_seconds > 0.0)

let test_config_visual_module () =
  check bool "temp_dir" true (String.length Figma_config.Visual.temp_dir > 0);
  (* render_script is None unless env set *)
  ignore Figma_config.Visual.render_script

let test_config_http_module () =
  check bool "timeout > 0" true (Figma_config.Http.timeout_seconds > 0.0);
  check bool "max_body > 0" true (Figma_config.Http.max_body_bytes > 0);
  ignore Figma_config.Http.log_response_body

let test_config_response_module () =
  check bool "max_inline > 0" true (Figma_config.Response.max_inline > 0);
  check bool "large_dir" true (String.length Figma_config.Response.large_dir > 0);
  check bool "ttl > 0" true (Figma_config.Response.ttl_seconds > 0);
  check bool "max_dir_mb > 0" true (Figma_config.Response.max_dir_mb > 0)

let test_config_cors_module () =
  let _mode = Figma_config.Cors.mode () in
  let _origins = Figma_config.Cors.allowed_origins () in
  let _pn = Figma_config.Cors.allow_private_network () in
  let _h = Figma_config.Cors.allow_headers () in
  check bool "mode is string" true (String.length _mode > 0);
  check bool "origins non-empty" true (List.length _origins > 0);
  ignore _pn;
  check bool "headers" true (String.length _h > 0)

let contains_substr s sub =
  let slen = String.length s in
  let sublen = String.length sub in
  if sublen > slen then false
  else
    let found = ref false in
    for i = 0 to slen - sublen do
      if not !found && String.sub s i sublen = sub then found := true
    done;
    !found

let test_config_cors_strict () =
  with_env "FIGMA_MCP_CORS_PROFILE" "strict" (fun () ->
    let p = Figma_config.Cors.profile () in
    check bool "strict" true (p = Figma_config.Cors.Strict);
    let origins = Figma_config.Cors.allowed_origins_default () in
    check (list string) "strict origins" ["https://www.figma.com"] origins;
    check bool "no private network" false (Figma_config.Cors.allow_private_network_default ());
    let h = Figma_config.Cors.allow_headers_default () in
    check bool "no private network header in strict" false
      (contains_substr h "Access-Control-Request-Private-Network"))

let test_config_cors_compat () =
  with_env "FIGMA_MCP_CORS_PROFILE" "compat" (fun () ->
    let p = Figma_config.Cors.profile () in
    check bool "compat" true (p = Figma_config.Cors.Compat);
    let origins = Figma_config.Cors.allowed_origins_default () in
    check bool "has null" true (List.mem "null" origins);
    check bool "private network" true (Figma_config.Cors.allow_private_network_default ()))

let test_config_cors_unknown_profile () =
  with_env "FIGMA_MCP_CORS_PROFILE" "relaxed" (fun () ->
    let p = Figma_config.Cors.profile () in
    check bool "unknown -> compat" true (p = Figma_config.Cors.Compat))

let test_config_cors_get_string_nonempty () =
  with_env "__TEST_CORS_NE" "  " (fun () ->
    check string "whitespace -> default" "fallback"
      (Figma_config.Cors.get_string_nonempty ~default:"fallback" "__TEST_CORS_NE"));
  with_env "__TEST_CORS_NE" "actual" (fun () ->
    check string "actual value" "actual"
      (Figma_config.Cors.get_string_nonempty ~default:"fallback" "__TEST_CORS_NE"));
  let name = fresh_env_name "CORS_NE" in
  check string "unset -> default" "fallback"
    (Figma_config.Cors.get_string_nonempty ~default:"fallback" name)

let test_config_asset_module () =
  (* Asset.dir has a 3-level fallback chain *)
  check bool "asset dir" true (String.length Figma_config.Asset.dir > 0)

let test_config_auth_module () =
  (* Auth.token may or may not be set *)
  ignore Figma_config.Auth.token

let test_config_auth_require_no_token () =
  (* If FIGMA_TOKEN is not set, require_token should fail *)
  let prev = Sys.getenv_opt "FIGMA_TOKEN" in
  (match prev with
   | Some _ ->
     (* Token is set, require_token should succeed *)
     let t = Figma_config.Auth.require_token () in
     check bool "got token" true (String.length t > 0)
   | None ->
     (* Token not set, require_token should raise *)
     try
       let _ = Figma_config.Auth.require_token () in
       fail "should have raised"
     with Failure _ -> ())

let test_config_llm_module () =
  ignore Figma_config.Llm.provider

let test_config_print_summary () =
  (* Just check it doesn't raise *)
  Figma_config.print_summary ()

(* ================================================================
   17. CORS allowed_origins, allow_private_network, allow_headers with env override
   ================================================================ *)

let test_config_cors_allowed_origins_override () =
  with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "https://a.com, https://b.com" (fun () ->
    let origins = Figma_config.Cors.allowed_origins () in
    check (list string) "override" ["https://a.com"; "https://b.com"] origins)

let test_config_cors_allow_private_network_override () =
  with_env "FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK" "false" (fun () ->
    check bool "override false" false (Figma_config.Cors.allow_private_network ()))

let test_config_cors_allow_headers_override () =
  with_env "FIGMA_MCP_CORS_ALLOW_HEADERS" "Content-Type, Accept" (fun () ->
    check string "override" "Content-Type, Accept" (Figma_config.Cors.allow_headers ()))

let test_config_cors_mode_override () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    check string "permissive" "permissive" (Figma_config.Cors.mode ()))


(* ================================================================
   REGISTER ALL TESTS
   ================================================================ *)

let () =
  run "parser_config_coverage"
    [
      (* JSON helpers *)
      ( "get_string",
        [ test_case "found" `Quick test_get_string_found;
          test_case "missing" `Quick test_get_string_missing;
          test_case "wrong type" `Quick test_get_string_wrong_type;
          test_case "non-assoc" `Quick test_get_string_non_assoc ] );
      ( "get_float",
        [ test_case "found" `Quick test_get_float_found;
          test_case "from int" `Quick test_get_float_from_int;
          test_case "missing" `Quick test_get_float_missing;
          test_case "wrong type" `Quick test_get_float_wrong_type;
          test_case "non-assoc" `Quick test_get_float_non_assoc ] );
      ( "get_int",
        [ test_case "found" `Quick test_get_int_found;
          test_case "from float" `Quick test_get_int_from_float;
          test_case "missing" `Quick test_get_int_missing;
          test_case "wrong type" `Quick test_get_int_wrong_type;
          test_case "non-assoc" `Quick test_get_int_non_assoc ] );
      ( "get_bool",
        [ test_case "found true" `Quick test_get_bool_found;
          test_case "found false" `Quick test_get_bool_false;
          test_case "missing" `Quick test_get_bool_missing;
          test_case "wrong type" `Quick test_get_bool_wrong_type;
          test_case "non-assoc" `Quick test_get_bool_non_assoc ] );
      ( "get_list",
        [ test_case "found" `Quick test_get_list_found;
          test_case "missing" `Quick test_get_list_missing;
          test_case "wrong type" `Quick test_get_list_wrong_type;
          test_case "non-assoc" `Quick test_get_list_non_assoc ] );
      ( "get_assoc",
        [ test_case "found" `Quick test_get_assoc_found;
          test_case "missing" `Quick test_get_assoc_missing;
          test_case "wrong type" `Quick test_get_assoc_wrong_type;
          test_case "non-assoc" `Quick test_get_assoc_non_assoc ] );

      (* Operators *)
      ( "operators",
        [ test_case ">>= some" `Quick test_bind_some;
          test_case ">>= none" `Quick test_bind_none;
          test_case "|? some" `Quick test_default_some;
          test_case "|? none" `Quick test_default_none ] );

      (* RGBA *)
      ( "rgba",
        [ test_case "full" `Quick test_parse_rgba_full;
          test_case "defaults" `Quick test_parse_rgba_defaults;
          test_case "int values" `Quick test_parse_rgba_int_values;
          test_case "partial" `Quick test_parse_rgba_partial ] );

      (* Paint *)
      ( "paint_type",
        [ test_case "all variants" `Quick test_parse_paint_type_all_variants ] );
      ( "paint",
        [ test_case "solid" `Quick test_parse_paint_solid;
          test_case "image" `Quick test_parse_paint_image;
          test_case "gradient" `Quick test_parse_paint_gradient;
          test_case "gradient bad stop" `Quick test_parse_paint_gradient_bad_stop;
          test_case "no type" `Quick test_parse_paint_no_type;
          test_case "invisible" `Quick test_parse_paint_invisible;
          test_case "emoji" `Quick test_parse_paint_emoji;
          test_case "angular" `Quick test_parse_paint_gradient_angular;
          test_case "diamond" `Quick test_parse_paint_gradient_diamond ] );
      ( "paints",
        [ test_case "list" `Quick test_parse_paints_list;
          test_case "missing" `Quick test_parse_paints_missing ] );

      (* Effects *)
      ( "effect_type",
        [ test_case "all variants" `Quick test_parse_effect_type_all ] );
      ( "fx",
        [ test_case "shadow" `Quick test_parse_fx_shadow;
          test_case "blur no offset" `Quick test_parse_fx_blur_no_offset;
          test_case "inner shadow" `Quick test_parse_fx_inner_shadow;
          test_case "background blur" `Quick test_parse_fx_background_blur;
          test_case "defaults" `Quick test_parse_fx_defaults;
          test_case "invisible" `Quick test_parse_fx_invisible;
          test_case "offset defaults" `Quick test_parse_fx_offset_defaults ] );
      ( "effects",
        [ test_case "list" `Quick test_parse_effects_list;
          test_case "missing" `Quick test_parse_effects_missing ] );

      (* BBox *)
      ( "bbox",
        [ test_case "full" `Quick test_parse_bbox_full;
          test_case "missing" `Quick test_parse_bbox_missing;
          test_case "incomplete" `Quick test_parse_bbox_incomplete;
          test_case "partial fields" `Quick test_parse_bbox_partial_fields ] );

      (* Layout *)
      ( "layout_mode",
        [ test_case "vertical" `Quick test_parse_layout_mode_vertical;
          test_case "horizontal" `Quick test_parse_layout_mode_horizontal;
          test_case "none" `Quick test_parse_layout_mode_none;
          test_case "unknown" `Quick test_parse_layout_mode_unknown ] );
      ( "layout_align",
        [ test_case "all" `Quick test_parse_layout_align_all ] );
      ( "layout_sizing",
        [ test_case "all" `Quick test_parse_layout_sizing_all ] );

      (* Typography *)
      ( "text_align_h",
        [ test_case "all" `Quick test_parse_text_align_h_all ] );
      ( "text_align_v",
        [ test_case "all" `Quick test_parse_text_align_v_all ] );
      ( "text_decoration",
        [ test_case "all" `Quick test_parse_text_decoration_all ] );
      ( "text_case",
        [ test_case "all" `Quick test_parse_text_case_all ] );
      ( "typography",
        [ test_case "full" `Quick test_parse_typography_full;
          test_case "defaults" `Quick test_parse_typography_defaults;
          test_case "no style" `Quick test_parse_typography_no_style ] );

      (* Node *)
      ( "node",
        [ test_case "minimal" `Quick test_parse_node_minimal;
          test_case "text with typography" `Quick test_parse_node_text_with_typography;
          test_case "children" `Quick test_parse_node_children;
          test_case "depth limit" `Quick test_parse_node_depth_limit;
          test_case "at max depth" `Quick test_parse_node_at_max_depth;
          test_case "border radii float" `Quick test_parse_node_border_radii_float;
          test_case "border radii int" `Quick test_parse_node_border_radii_int;
          test_case "border radii none" `Quick test_parse_node_border_radii_none;
          test_case "border radii wrong count" `Quick test_parse_node_border_radii_wrong_count;
          test_case "all types" `Quick test_parse_node_all_types;
          test_case "no type" `Quick test_parse_node_no_type;
          test_case "layout" `Quick test_parse_node_layout;
          test_case "locked invisible" `Quick test_parse_node_locked_invisible;
          test_case "fills strokes effects" `Quick test_parse_node_fills_strokes_effects;
          test_case "component id" `Quick test_parse_node_component_id;
          test_case "non-text no typography" `Quick test_parse_node_non_text_no_typography ] );

      (* parse_json_string / parse_json *)
      ( "json_string",
        [ test_case "valid" `Quick test_parse_json_string_valid;
          test_case "invalid" `Quick test_parse_json_string_invalid;
          test_case "parse_json" `Quick test_parse_json ] );

      (* ===== figma_config ===== *)
      ( "config_get_string",
        [ test_case "from env" `Quick test_config_get_string_from_env;
          test_case "default" `Quick test_config_get_string_default ] );
      ( "config_get_int",
        [ test_case "valid" `Quick test_config_get_int_valid;
          test_case "invalid" `Quick test_config_get_int_invalid;
          test_case "default" `Quick test_config_get_int_default ] );
      ( "config_get_float",
        [ test_case "valid" `Quick test_config_get_float_valid;
          test_case "invalid" `Quick test_config_get_float_invalid;
          test_case "default" `Quick test_config_get_float_default ] );
      ( "config_get_bool",
        [ test_case "true variants" `Quick test_config_get_bool_true_variants;
          test_case "false variants" `Quick test_config_get_bool_false_variants;
          test_case "invalid" `Quick test_config_get_bool_invalid;
          test_case "default" `Quick test_config_get_bool_default ] );
      ( "config_get_string_list",
        [ test_case "csv" `Quick test_config_get_string_list_csv;
          test_case "empty" `Quick test_config_get_string_list_empty;
          test_case "commas only" `Quick test_config_get_string_list_commas_only;
          test_case "single" `Quick test_config_get_string_list_single ] );
      ( "config_modules",
        [ test_case "cache" `Quick test_config_cache_module;
          test_case "plugin" `Quick test_config_plugin_module;
          test_case "visual" `Quick test_config_visual_module;
          test_case "http" `Quick test_config_http_module;
          test_case "response" `Quick test_config_response_module;
          test_case "asset" `Quick test_config_asset_module;
          test_case "auth" `Quick test_config_auth_module;
          test_case "auth require" `Quick test_config_auth_require_no_token;
          test_case "llm" `Quick test_config_llm_module;
          test_case "print_summary" `Quick test_config_print_summary ] );
      ( "config_cors",
        [ test_case "module" `Quick test_config_cors_module;
          test_case "strict" `Quick test_config_cors_strict;
          test_case "compat" `Quick test_config_cors_compat;
          test_case "unknown profile" `Quick test_config_cors_unknown_profile;
          test_case "get_string_nonempty" `Quick test_config_cors_get_string_nonempty;
          test_case "origins override" `Quick test_config_cors_allowed_origins_override;
          test_case "private network override" `Quick test_config_cors_allow_private_network_override;
          test_case "headers override" `Quick test_config_cors_allow_headers_override;
          test_case "mode override" `Quick test_config_cors_mode_override ] );
    ]
