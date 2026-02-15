(** Comprehensive tests for figma_early_stop.ml (206 LOC)
    Target: 0% -> 90%+ coverage

    Covers:
    - create (default/custom config)
    - calculate_text_density (TEXT nodes, children, empty, error)
    - check: all 6 branches (Target_reached, Max_iterations, Plateau,
      Regression, Text_ceiling, Continue)
    - summary (empty, single, multi history)
    - to_json (all reason variants)
    - reset
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

(* ──────────── create ──────────── *)

let test_create_default () =
  let t = ES.create () in
  check (float eps) "target_ssim" 0.90 t.config.target_ssim;
  check (float eps) "plateau_threshold" 0.005 t.config.plateau_threshold;
  check int "plateau_patience" 3 t.config.plateau_patience;
  check (float eps) "text_ceiling" 0.88 t.config.text_ceiling;
  check int "max_iterations" 10 t.config.max_iterations;
  check (list (float eps)) "ssim_history empty" [] t.ssim_history;
  check (list (float eps)) "improvement_history empty" [] t.improvement_history;
  float_close ~msg:"best_ssim" 0.0 t.best_ssim;
  check int "best_iteration" 0 t.best_iteration

let test_create_custom_config () =
  let cfg = ES.{
    target_ssim = 0.95;
    plateau_threshold = 0.01;
    plateau_patience = 5;
    text_ceiling = 0.92;
    max_iterations = 20;
  } in
  let t = ES.create ~config:cfg () in
  check (float eps) "target_ssim" 0.95 t.config.target_ssim;
  check int "plateau_patience" 5 t.config.plateau_patience;
  check int "max_iterations" 20 t.config.max_iterations

(* ──────────── calculate_text_density ──────────── *)

let test_text_density_all_text () =
  let dsl = `Assoc [("type", `String "TEXT")] in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"all text" 1.0 d

let test_text_density_no_text () =
  let dsl = `Assoc [("type", `String "FRAME");
                     ("children", `List [
                       `Assoc [("type", `String "RECTANGLE")];
                       `Assoc [("type", `String "ELLIPSE")];
                     ])] in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"no text" 0.0 d

let test_text_density_mixed () =
  let dsl = `Assoc [("type", `String "FRAME");
                     ("children", `List [
                       `Assoc [("type", `String "TEXT")];
                       `Assoc [("type", `String "RECTANGLE")];
                       `Assoc [("type", `String "TEXT")];
                     ])] in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"mixed" 0.5 d

let test_text_density_nested () =
  let dsl = `Assoc [("type", `String "FRAME");
                     ("children", `List [
                       `Assoc [("type", `String "GROUP");
                               ("children", `List [
                                 `Assoc [("type", `String "TEXT")];
                               ])];
                     ])] in
  let d = ES.calculate_text_density dsl in
  let expected = 1.0 /. 3.0 in
  let diff = Float.abs (expected -. d) in
  check bool "nested" true (diff < 0.01)

let test_text_density_empty_children () =
  let dsl = `Assoc [("type", `String "FRAME");
                     ("children", `List [])] in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"empty children" 0.0 d

let test_text_density_no_type () =
  let dsl = `Assoc [("name", `String "something")] in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"no type" 0.0 d

let test_text_density_null_node () =
  let dsl = `Null in
  let d = ES.calculate_text_density dsl in
  check bool "null density >= 0" true (d >= 0.0)

let test_text_density_no_children_field () =
  let dsl = `Assoc [("type", `String "RECTANGLE")] in
  let d = ES.calculate_text_density dsl in
  float_close ~msg:"no children field" 0.0 d

(* ──────────── check: Target_reached ──────────── *)

let test_check_target_reached () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.92 ~iteration:1 () in
  check bool "should_stop" true cond.should_stop;
  check reason_testable "reason" ES.Target_reached cond.reason;
  float_close ~msg:"confidence" 1.0 cond.confidence

let test_check_target_exact_boundary () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.90 ~iteration:1 () in
  check bool "should_stop at exact boundary" true cond.should_stop;
  check reason_testable "reason" ES.Target_reached cond.reason

(* ──────────── check: Max_iterations ──────────── *)

let test_check_max_iterations () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:10 () in
  check bool "should_stop" true cond.should_stop;
  check reason_testable "reason" ES.Max_iterations cond.reason;
  float_close ~msg:"confidence" 0.9 cond.confidence

let test_check_max_iterations_exceeded () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:15 () in
  check bool "should_stop" true cond.should_stop;
  check reason_testable "reason" ES.Max_iterations cond.reason

(* ──────────── check: Plateau ──────────── *)

let test_check_plateau () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.701 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.702 ~iteration:3 () in
  let cond = ES.check t ~current_ssim:0.703 ~iteration:4 () in
  check bool "should_stop" true cond.should_stop;
  check reason_testable "reason" ES.Plateau cond.reason;
  float_close ~msg:"confidence" 0.85 cond.confidence

let test_check_no_plateau_insufficient_history () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.701 ~iteration:2 () in
  check bool "should not stop yet" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason

let test_check_no_plateau_big_improvement_breaks_it () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.701 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.702 ~iteration:3 () in
  let cond = ES.check t ~current_ssim:0.75 ~iteration:4 () in
  check bool "should not stop" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason

let test_check_plateau_custom_patience () =
  let cfg = ES.{ default_config with plateau_patience = 2 } in
  let t = ES.create ~config:cfg () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.701 ~iteration:2 () in
  let cond = ES.check t ~current_ssim:0.702 ~iteration:3 () in
  check bool "should_stop patience=2" true cond.should_stop;
  check reason_testable "reason" ES.Plateau cond.reason

(* ──────────── check: Regression ──────────── *)

let test_check_regression () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.75 ~iteration:2 () in
  check bool "should not stop (warning only)" false cond.should_stop;
  check reason_testable "reason" ES.Regression cond.reason;
  float_close ~msg:"confidence" 0.7 cond.confidence

let test_check_regression_message_contains_percentage () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.78 ~iteration:2 () in
  check bool "message non-empty" true (String.length cond.message > 0);
  check reason_testable "reason" ES.Regression cond.reason

(* ──────────── check: Text_ceiling ──────────── *)

let test_check_text_ceiling () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.89 ~iteration:2
    ~text_density:0.5 () in
  check bool "should_stop" true cond.should_stop;
  check reason_testable "reason" ES.Text_ceiling cond.reason;
  float_close ~msg:"confidence" 0.8 cond.confidence

let test_check_text_ceiling_exact_boundary () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.88 ~iteration:2
    ~text_density:0.31 () in
  check bool "should_stop at exact ceiling" true cond.should_stop;
  check reason_testable "reason" ES.Text_ceiling cond.reason

let test_check_text_ceiling_low_density_no_stop () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.89 ~iteration:2
    ~text_density:0.2 () in
  check bool "should not stop" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason

let test_check_text_ceiling_below_threshold_no_stop () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.85 ~iteration:2
    ~text_density:0.5 () in
  check bool "should not stop" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason

(* ──────────── check: Continue ──────────── *)

let test_check_continue () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  check bool "should not stop" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason;
  float_close ~msg:"confidence" 0.0 cond.confidence

let test_check_continue_improving () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.60 ~iteration:2 () in
  check bool "should not stop" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason

(* ──────────── check: Priority ordering ──────────── *)

let test_check_target_overrides_max_iterations () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.95 ~iteration:10 () in
  check bool "should_stop" true cond.should_stop;
  check reason_testable "target wins" ES.Target_reached cond.reason

let test_check_target_overrides_text_ceiling () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.92 ~iteration:2
    ~text_density:0.5 () in
  check bool "should_stop" true cond.should_stop;
  check reason_testable "target wins" ES.Target_reached cond.reason

(* ──────────── check: best_ssim tracking ──────────── *)

let test_check_best_ssim_tracking () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.60 ~iteration:3 () in
  float_close ~msg:"best_ssim" 0.70 t.best_ssim;
  check int "best_iteration" 2 t.best_iteration

let test_check_best_ssim_updates_on_equal () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.50 ~iteration:2 () in
  check int "best_iteration stays" 1 t.best_iteration

(* ──────────── check: ssim_history accumulation ──────────── *)

let test_check_history_accumulation () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.60 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:3 () in
  check (list (float eps)) "ssim_history" [0.50; 0.60; 0.70] t.ssim_history;
  check int "improvement_history length" 2 (List.length t.improvement_history)

(* ──────────── summary ──────────── *)

let test_summary_empty () =
  let t = ES.create () in
  let s = ES.summary t in
  check bool "contains Iterations: 0" true (String.length s > 0);
  check bool "has 'Iterations: 0'" true
    (try let _ = Str.search_forward (Str.regexp "Iterations: 0") s 0 in true
     with Not_found -> false)

let test_summary_with_history () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.60 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:3 () in
  let s = ES.summary t in
  check bool "has iterations 3" true
    (try let _ = Str.search_forward (Str.regexp "Iterations: 3") s 0 in true
     with Not_found -> false);
  check bool "has best 70.0%" true
    (try let _ = Str.search_forward (Str.regexp "Best: 70.0%") s 0 in true
     with Not_found -> false)

let test_summary_no_improvement () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let s = ES.summary t in
  check bool "has Avg" true
    (try let _ = Str.search_forward (Str.regexp "Avg") s 0 in true
     with Not_found -> false)

(* ──────────── to_json ──────────── *)

let test_to_json_target_reached () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.95 ~iteration:1 () in
  let json = ES.to_json t cond in
  let reason_str = Yojson.Safe.Util.(member "reason" json |> to_string) in
  check string "reason" "TARGET" reason_str;
  let should_stop = Yojson.Safe.Util.(member "should_stop" json |> to_bool) in
  check bool "should_stop" true should_stop

let test_to_json_max_iter () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:10 () in
  let json = ES.to_json t cond in
  let reason_str = Yojson.Safe.Util.(member "reason" json |> to_string) in
  check string "reason" "MAX_ITER" reason_str

let test_to_json_plateau () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.701 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.702 ~iteration:3 () in
  let cond = ES.check t ~current_ssim:0.703 ~iteration:4 () in
  let json = ES.to_json t cond in
  let reason_str = Yojson.Safe.Util.(member "reason" json |> to_string) in
  check string "reason" "PLATEAU" reason_str

let test_to_json_regression () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.75 ~iteration:2 () in
  let json = ES.to_json t cond in
  let reason_str = Yojson.Safe.Util.(member "reason" json |> to_string) in
  check string "reason" "REGRESSION" reason_str;
  let should_stop = Yojson.Safe.Util.(member "should_stop" json |> to_bool) in
  check bool "should_stop false" false should_stop

let test_to_json_text_ceiling () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.89 ~iteration:2
    ~text_density:0.5 () in
  let json = ES.to_json t cond in
  let reason_str = Yojson.Safe.Util.(member "reason" json |> to_string) in
  check string "reason" "TEXT_CEILING" reason_str

let test_to_json_continue () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let json = ES.to_json t cond in
  let reason_str = Yojson.Safe.Util.(member "reason" json |> to_string) in
  check string "reason" "CONTINUE" reason_str

let test_to_json_stats_structure () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.60 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:2 () in
  let cond = ES.check t ~current_ssim:0.80 ~iteration:3 () in
  let json = ES.to_json t cond in
  let stats = Yojson.Safe.Util.member "stats" json in
  let iters = Yojson.Safe.Util.(member "iterations" stats |> to_int) in
  check int "iterations" 3 iters;
  let best = Yojson.Safe.Util.(member "best_ssim" stats |> to_float) in
  float_close ~msg:"best_ssim" 0.80 best;
  let best_iter = Yojson.Safe.Util.(member "best_iteration" stats |> to_int) in
  check int "best_iteration" 3 best_iter;
  let history = Yojson.Safe.Util.(member "ssim_history" stats |> to_list) in
  check int "history length" 3 (List.length history);
  let summary_str = Yojson.Safe.Util.(member "summary" json |> to_string) in
  check bool "summary non-empty" true (String.length summary_str > 0)

let test_to_json_message_present () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  let json = ES.to_json t cond in
  let msg = Yojson.Safe.Util.(member "message" json |> to_string) in
  check bool "message non-empty" true (String.length msg > 0);
  let conf = Yojson.Safe.Util.(member "confidence" json |> to_float) in
  check bool "confidence >= 0" true (conf >= 0.0)

(* ──────────── reset ──────────── *)

let test_reset () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:2 () in
  ES.reset t;
  check (list (float eps)) "ssim_history cleared" [] t.ssim_history;
  check (list (float eps)) "improvement_history cleared" [] t.improvement_history;
  float_close ~msg:"best_ssim reset" 0.0 t.best_ssim;
  check int "best_iteration reset" 0 t.best_iteration

let test_reset_then_reuse () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  ES.reset t;
  let cond = ES.check t ~current_ssim:0.50 ~iteration:1 () in
  check bool "should not stop" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason;
  check (list (float eps)) "new history" [0.50] t.ssim_history

(* ──────────── Edge cases ──────────── *)

let test_check_first_iteration_no_improvement () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.40 ~iteration:1 () in
  check bool "should not stop" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason;
  check int "improvement_history empty" 0 (List.length t.improvement_history)

let test_check_zero_ssim () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.0 ~iteration:1 () in
  check bool "should not stop" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason

let test_check_ssim_exactly_1 () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:1.0 ~iteration:1 () in
  check bool "should stop (target reached)" true cond.should_stop;
  check reason_testable "reason" ES.Target_reached cond.reason

let test_check_iteration_0 () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:0 () in
  check bool "should not stop" false cond.should_stop

let test_check_default_text_density () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.80 ~iteration:1 () in
  let cond = ES.check t ~current_ssim:0.89 ~iteration:2 () in
  check reason_testable "no text ceiling" ES.Continue cond.reason

let test_check_oscillating_scores () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.75 ~iteration:2 () in
  let c3 = ES.check t ~current_ssim:0.72 ~iteration:3 () in
  check reason_testable "regression" ES.Regression c3.reason;
  let c4 = ES.check t ~current_ssim:0.76 ~iteration:4 () in
  check reason_testable "continue" ES.Continue c4.reason

let test_check_plateau_at_threshold_boundary () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.705 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.710 ~iteration:3 () in
  let cond = ES.check t ~current_ssim:0.715 ~iteration:4 () in
  check bool "should not stop (at boundary)" false cond.should_stop;
  check reason_testable "reason" ES.Continue cond.reason

let test_check_max_iterations_at_boundary () =
  let t = ES.create () in
  let cond = ES.check t ~current_ssim:0.50 ~iteration:9 () in
  check bool "should not stop" false cond.should_stop

(* ──────────── Sequence simulation ──────────── *)

let test_full_improving_sequence () =
  let t = ES.create () in
  let scores = [0.50; 0.55; 0.60; 0.70; 0.80; 0.95] in
  let last_cond = ref (ES.check t ~current_ssim:0.0 ~iteration:0 ()) in
  List.iteri (fun i ssim ->
    let c = ES.check t ~current_ssim:ssim ~iteration:(i+1) () in
    last_cond := c
  ) scores;
  check bool "stopped" true (!last_cond).should_stop;
  check reason_testable "reason" ES.Target_reached (!last_cond).reason

let test_full_flat_sequence () =
  let t = ES.create () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:1 () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:2 () in
  let _ = ES.check t ~current_ssim:0.70 ~iteration:3 () in
  let cond = ES.check t ~current_ssim:0.70 ~iteration:4 () in
  check bool "plateau detected" true cond.should_stop;
  check reason_testable "reason" ES.Plateau cond.reason

(* ──────────── Test suite ──────────── *)

let () =
  run "Figma_early_stop" [
    ("create", [
      test_case "default config" `Quick test_create_default;
      test_case "custom config" `Quick test_create_custom_config;
    ]);
    ("calculate_text_density", [
      test_case "all TEXT" `Quick test_text_density_all_text;
      test_case "no TEXT" `Quick test_text_density_no_text;
      test_case "mixed" `Quick test_text_density_mixed;
      test_case "nested" `Quick test_text_density_nested;
      test_case "empty children" `Quick test_text_density_empty_children;
      test_case "no type field" `Quick test_text_density_no_type;
      test_case "null node" `Quick test_text_density_null_node;
      test_case "no children field" `Quick test_text_density_no_children_field;
    ]);
    ("check:Target_reached", [
      test_case "above target" `Quick test_check_target_reached;
      test_case "exact boundary" `Quick test_check_target_exact_boundary;
    ]);
    ("check:Max_iterations", [
      test_case "at max" `Quick test_check_max_iterations;
      test_case "past max" `Quick test_check_max_iterations_exceeded;
    ]);
    ("check:Plateau", [
      test_case "3 consecutive low improvements" `Quick test_check_plateau;
      test_case "insufficient history" `Quick test_check_no_plateau_insufficient_history;
      test_case "big improvement breaks plateau" `Quick test_check_no_plateau_big_improvement_breaks_it;
      test_case "custom patience=2" `Quick test_check_plateau_custom_patience;
      test_case "at threshold boundary (not plateau)" `Quick test_check_plateau_at_threshold_boundary;
    ]);
    ("check:Regression", [
      test_case "score drops" `Quick test_check_regression;
      test_case "message content" `Quick test_check_regression_message_contains_percentage;
    ]);
    ("check:Text_ceiling", [
      test_case "high density + near ceiling" `Quick test_check_text_ceiling;
      test_case "exact boundary" `Quick test_check_text_ceiling_exact_boundary;
      test_case "low density no stop" `Quick test_check_text_ceiling_low_density_no_stop;
      test_case "below ceiling no stop" `Quick test_check_text_ceiling_below_threshold_no_stop;
    ]);
    ("check:Continue", [
      test_case "normal continue" `Quick test_check_continue;
      test_case "improving continue" `Quick test_check_continue_improving;
    ]);
    ("check:Priority", [
      test_case "target > max_iterations" `Quick test_check_target_overrides_max_iterations;
      test_case "target > text_ceiling" `Quick test_check_target_overrides_text_ceiling;
    ]);
    ("check:Tracking", [
      test_case "best_ssim tracking" `Quick test_check_best_ssim_tracking;
      test_case "equal ssim no update" `Quick test_check_best_ssim_updates_on_equal;
      test_case "history accumulation" `Quick test_check_history_accumulation;
    ]);
    ("check:Edge", [
      test_case "first iteration" `Quick test_check_first_iteration_no_improvement;
      test_case "zero ssim" `Quick test_check_zero_ssim;
      test_case "ssim = 1.0" `Quick test_check_ssim_exactly_1;
      test_case "iteration 0" `Quick test_check_iteration_0;
      test_case "default text_density" `Quick test_check_default_text_density;
      test_case "oscillating scores" `Quick test_check_oscillating_scores;
      test_case "max_iterations boundary" `Quick test_check_max_iterations_at_boundary;
    ]);
    ("check:Sequence", [
      test_case "improving to target" `Quick test_full_improving_sequence;
      test_case "flat plateau" `Quick test_full_flat_sequence;
    ]);
    ("summary", [
      test_case "empty" `Quick test_summary_empty;
      test_case "with history" `Quick test_summary_with_history;
      test_case "no improvement" `Quick test_summary_no_improvement;
    ]);
    ("to_json", [
      test_case "TARGET" `Quick test_to_json_target_reached;
      test_case "MAX_ITER" `Quick test_to_json_max_iter;
      test_case "PLATEAU" `Quick test_to_json_plateau;
      test_case "REGRESSION" `Quick test_to_json_regression;
      test_case "TEXT_CEILING" `Quick test_to_json_text_ceiling;
      test_case "CONTINUE" `Quick test_to_json_continue;
      test_case "stats structure" `Quick test_to_json_stats_structure;
      test_case "message present" `Quick test_to_json_message_present;
    ]);
    ("reset", [
      test_case "clears state" `Quick test_reset;
      test_case "reuse after reset" `Quick test_reset_then_reuse;
    ]);
  ]
