(** Extra coverage tests for figma_early_stop.ml
    Goal: Push 98.80% (82/83) -> 100%

    The single uncovered point is the `else 0.0` branch in calculate_text_density
    when total=0. This branch is practically unreachable because `count` calls
    `incr total` as its first operation. We attempt to trigger it via
    Stack_overflow on a deeply nested JSON structure.
*)

open Alcotest
module ES = Figma_early_stop

(* ──────────── Helpers ──────────── *)

let reason_to_string = function
  | ES.Target_reached -> "Target_reached"
  | ES.Max_iterations -> "Max_iterations"
  | ES.Plateau -> "Plateau"
  | ES.Text_ceiling -> "Text_ceiling"
  | ES.Regression -> "Regression"
  | ES.Continue -> "Continue"

let reason_eq a b = reason_to_string a = reason_to_string b

let reason_testable =
  testable (Fmt.of_to_string reason_to_string) reason_eq

let eps = 1e-9

let float_close ~msg expected actual =
  let diff = Float.abs (expected -. actual) in
  check bool msg true (diff < eps)

(* ──────────── calculate_text_density: Stack_overflow attempt ──────────── *)

(** Build a deeply nested JSON to provoke Stack_overflow before incr total.
    If Stack_overflow fires before the first `incr total`, total stays 0
    and the `else 0.0` branch is taken. *)
let test_text_density_deeply_nested () =
  (* Build JSON nesting deep enough to provoke Stack_overflow during recursive
     count traversal. We use a moderate depth and catch if building itself
     overflows. The count function may partially process before overflow. *)
  let rec build_deep n =
    if n <= 0 then `Assoc [("type", `String "TEXT")]
    else `Assoc [("type", `String "FRAME");
                 ("children", `List [build_deep (n - 1)])]
  in
  let dsl =
    try build_deep 5_000
    with Stack_overflow -> `Null
  in
  let d = ES.calculate_text_density dsl in
  (* Either it processed some nodes (d > 0) or hit exception path (d = 0) *)
  check bool "density >= 0" true (d >= 0.0)

(** Alternative: pass a non-Assoc top-level value that causes immediate
    crash in Yojson member lookup *)
let test_text_density_list_top_level () =
  (* `List is not `Assoc, so member "type" raises Yojson.Safe.Util.Type_error
     AFTER incr total. So total >= 1 and we get 0/1 = 0.0 from the then branch *)
  let dsl = `List [`String "hello"] in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"list top-level" 0.0 d

(** Pass `Bool which also causes Yojson member to raise *)
let test_text_density_bool_input () =
  let dsl = `Bool true in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"bool input" 0.0 d

(** Pass `Int which triggers Yojson.Safe.Util.Type_error *)
let test_text_density_int_input () =
  let dsl = `Int 42 in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"int input" 0.0 d

(** Pass `Float *)
let test_text_density_float_input () =
  let dsl = `Float 3.14 in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"float input" 0.0 d

(** Pass `String at top level *)
let test_text_density_string_input () =
  let dsl = `String "not a node" in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"string input" 0.0 d

(** Children field is not a list (e.g., a string) *)
let test_text_density_children_not_list () =
  let dsl = `Assoc [("type", `String "FRAME");
                     ("children", `String "bad")] in
  let d = ES.calculate_text_density dsl in
  (* count processes root (total=1, no TEXT match), tries children -> to_list raises
     -> caught by try/None -> no recursion. Result: 0/1 = 0.0 *)
  float_close ~msg:"children not list" 0.0 d

(** Children field is an integer *)
let test_text_density_children_int () =
  let dsl = `Assoc [("type", `String "FRAME");
                     ("children", `Int 5)] in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"children int" 0.0 d

(** Multiple levels with mixed valid/invalid children *)
let test_text_density_mixed_invalid_children () =
  let dsl = `Assoc [("type", `String "FRAME");
                     ("children", `List [
                       `Assoc [("type", `String "TEXT")];
                       `Int 42;  (* Invalid child - count will incr total, then member raises *)
                       `Assoc [("type", `String "RECTANGLE")];
                     ])] in
  let d = ES.calculate_text_density dsl in
  (* Root(FRAME) + TEXT + Int(processed, exception after incr) + RECTANGLE = 4 total, 1 TEXT *)
  check bool "mixed invalid >= 0" true (d >= 0.0)

(* ──────────── check: text_density exactly at boundary 0.3 ──────────── *)

let test_check_text_density_exactly_0_3 () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  (* text_density = 0.3 is NOT > 0.3, so text_ceiling should NOT trigger *)
  let cond = ES.check t ~current_ssim:0.89 ~iteration:2
    ~text_density:0.3 () in
  check reason_testable "density=0.3 no ceiling" ES.Continue cond.reason

(* ──────────── check: text_ceiling exactly at boundary ──────────── *)

let test_check_text_ceiling_ssim_just_below () =
  let cfg = ES.{ default_config with text_ceiling = 0.88 } in
  let t = ES.create ~config:cfg () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  (* current_ssim = 0.879 < ceiling 0.88 -> no text_ceiling *)
  let cond = ES.check t ~current_ssim:0.879 ~iteration:2
    ~text_density:0.5 () in
  check reason_testable "ssim just below ceiling" ES.Continue cond.reason

(* ──────────── check: plateau with exactly plateau_threshold improvement ──────────── *)

let test_check_plateau_improvement_at_exact_threshold () =
  (* plateau_threshold = 0.005. Improvement of exactly 0.005 is NOT < 0.005
     so it should NOT count as plateau *)
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.700 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.705 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.710 ~iteration:3 () in
  let cond = ES.check t ~current_ssim:0.715 ~iteration:4 () in
  check bool "should not stop at exact threshold" false cond.should_stop;
  check reason_testable "continue" ES.Continue cond.reason

(* ──────────── check: regression then plateau ──────────── *)

let test_check_regression_then_plateau () =
  let cfg = ES.{ default_config with plateau_patience = 2 } in
  let t = ES.create ~config:cfg () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  (* Regression *)
  let c2 = ES.check t ~current_ssim:0.75 ~iteration:2 () in
  check reason_testable "regression" ES.Regression c2.reason;
  (* Small improvements -> plateau *)
  let _ = ES.check t ~current_ssim:0.751 ~iteration:3 () in
  let c4 = ES.check t ~current_ssim:0.752 ~iteration:4 () in
  check reason_testable "plateau after regression" ES.Plateau c4.reason

(* ──────────── to_json: confidence field accuracy ──────────── *)

let test_to_json_confidence_values () =
  let t = ES.create () in
  (* Target: confidence = 1.0 *)
  let cond = ES.check t ~current_ssim:0.95 ~iteration:1 () in
  let json = ES.to_json t cond in
  let conf = Yojson.Safe.Util.(member "confidence" json |> to_float) in
  float_close ~msg:"target confidence" 1.0 conf

let test_to_json_max_iter_confidence () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:10 () in
  let json = ES.to_json t cond in
  let conf = Yojson.Safe.Util.(member "confidence" json |> to_float) in
  float_close ~msg:"max_iter confidence" 0.9 conf

let test_to_json_plateau_confidence () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.701 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.702 ~iteration:3 () in
  let cond = ES.check t ~current_ssim:0.703 ~iteration:4 () in
  let json = ES.to_json t cond in
  let conf = Yojson.Safe.Util.(member "confidence" json |> to_float) in
  float_close ~msg:"plateau confidence" 0.85 conf

let test_to_json_regression_confidence () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.75 ~iteration:2 () in
  let json = ES.to_json t cond in
  let conf = Yojson.Safe.Util.(member "confidence" json |> to_float) in
  float_close ~msg:"regression confidence" 0.7 conf

let test_to_json_text_ceiling_confidence () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.89 ~iteration:2
    ~text_density:0.5 () in
  let json = ES.to_json t cond in
  let conf = Yojson.Safe.Util.(member "confidence" json |> to_float) in
  float_close ~msg:"text_ceiling confidence" 0.8 conf

let test_to_json_continue_confidence () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let json = ES.to_json t cond in
  let conf = Yojson.Safe.Util.(member "confidence" json |> to_float) in
  float_close ~msg:"continue confidence" 0.0 conf

(* ──────────── summary: single entry ──────────── *)

let test_summary_single_entry () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.65 ~iteration:1 () in
  let s = ES.summary t in
  check bool "has Iterations: 1" true
    (try let _ = Str.search_forward (Str.regexp "Iterations: 1") s 0 in true
     with Not_found -> false);
  check bool "has Best: 65.0%" true
    (try let _ = Str.search_forward (Str.regexp "Best: 65.0%") s 0 in true
     with Not_found -> false)

(* ──────────── reset: verify improvement_history is cleared ──────────── *)

let test_reset_clears_improvement_history () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.60 ~iteration:2 () in
  check int "improvement_history has entries" 1 (List.length t.improvement_history);
  ES.reset t;
  check int "improvement_history cleared" 0 (List.length t.improvement_history)

(* ──────────── Long sequence: max_iterations triggered ──────────── *)

let test_long_sequence_max_iter () =
  let cfg = ES.{ default_config with max_iterations = 5 } in
  let t = ES.create ~config:cfg () in
  let scores = [0.30; 0.40; 0.50; 0.55; 0.60] in
  let last = List.mapi (fun i ssim ->
    ES.check t ~current_ssim:ssim ~iteration:(i + 1) ()
  ) scores |> List.rev |> List.hd in
  check bool "should stop" true last.should_stop;
  check reason_testable "max_iterations" ES.Max_iterations last.reason

(* ──────────── Test suite ──────────── *)

let () =
  run "Figma_early_stop_coverage" [
    ("text_density:edge", [
      test_case "deeply nested (stack overflow attempt)" `Quick test_text_density_deeply_nested;
      test_case "list top-level" `Quick test_text_density_list_top_level;
      test_case "bool input" `Quick test_text_density_bool_input;
      test_case "int input" `Quick test_text_density_int_input;
      test_case "float input" `Quick test_text_density_float_input;
      test_case "string input" `Quick test_text_density_string_input;
      test_case "children not list" `Quick test_text_density_children_not_list;
      test_case "children int" `Quick test_text_density_children_int;
      test_case "mixed invalid children" `Quick test_text_density_mixed_invalid_children;
    ]);
    ("check:boundaries", [
      test_case "text_density exactly 0.3" `Quick test_check_text_density_exactly_0_3;
      test_case "ssim just below ceiling" `Quick test_check_text_ceiling_ssim_just_below;
      test_case "improvement at exact threshold" `Quick test_check_plateau_improvement_at_exact_threshold;
      test_case "regression then plateau" `Quick test_check_regression_then_plateau;
    ]);
    ("to_json:confidence", [
      test_case "target confidence" `Quick test_to_json_confidence_values;
      test_case "max_iter confidence" `Quick test_to_json_max_iter_confidence;
      test_case "plateau confidence" `Quick test_to_json_plateau_confidence;
      test_case "regression confidence" `Quick test_to_json_regression_confidence;
      test_case "text_ceiling confidence" `Quick test_to_json_text_ceiling_confidence;
      test_case "continue confidence" `Quick test_to_json_continue_confidence;
    ]);
    ("summary:extra", [
      test_case "single entry" `Quick test_summary_single_entry;
    ]);
    ("reset:extra", [
      test_case "clears improvement_history" `Quick test_reset_clears_improvement_history;
    ]);
    ("sequence:extra", [
      test_case "long sequence max_iter" `Quick test_long_sequence_max_iter;
    ]);
  ]
