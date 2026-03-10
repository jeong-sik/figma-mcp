(** Alcotest suite for Figma_eval_harness. *)

open Figma_eval_harness

(* ================================================================ *)
(* String.is_prefix polyfill (OCaml stdlib lacks it)                *)
(* ================================================================ *)

module String = struct
  include String

  let is_prefix ~affix s =
    let len_a = String.length affix in
    let len_s = String.length s in
    len_a <= len_s && String.sub s 0 len_a = affix
end

(* ================================================================ *)
(* Helpers                                                          *)
(* ================================================================ *)

let float_eq ~eps a b = Float.abs (a -. b) < eps

let mk_scenario ?(id = "s1") ?(name = "test") ?(desc = "test scenario")
    ?(input = "input.png") ?(reference = "ref.png")
    ?(max_dur = 10.0) ?(tags = [ "test" ]) ?(difficulty = "easy")
    graders =
  {
    id;
    name;
    description = desc;
    input_image = input;
    reference_image = reference;
    graders;
    max_duration_sec = max_dur;
    tags;
    difficulty;
  }

let mk_visual_grader ?(metric = "ssim") ?(expected = 0.95)
    ?(desc = "test grader") mode =
  Visual { metric; expected; mode; description = desc }

(* ================================================================ *)
(* 1. pass_at_k calculation                                         *)
(* ================================================================ *)

let test_pass_at_k_all_pass () =
  (* All 5 runs pass, k=1 -> 1.0 *)
  let result = pass_at_k ~n:5 ~c:5 ~k:1 in
  Alcotest.(check bool) "all pass -> 1.0" true (float_eq ~eps:1e-9 result 1.0)

let test_pass_at_k_none_pass () =
  (* 0 of 5 pass, k=1 -> 0.0 *)
  let result = pass_at_k ~n:5 ~c:0 ~k:1 in
  Alcotest.(check bool) "none pass -> 0.0" true (float_eq ~eps:1e-9 result 0.0)

let test_pass_at_k_partial () =
  (* 3 of 5 pass, k=1 -> 1 - C(2,1)/C(5,1) = 1 - 2/5 = 0.6 *)
  let result = pass_at_k ~n:5 ~c:3 ~k:1 in
  Alcotest.(check bool) "3/5 k=1 -> 0.6" true (float_eq ~eps:1e-9 result 0.6)

let test_pass_at_k_k_equals_n () =
  (* n=5, c=3, k=5 -> 1 - C(2,5)/C(5,5). C(2,5)=0 -> 1.0 *)
  let result = pass_at_k ~n:5 ~c:3 ~k:5 in
  Alcotest.(check bool) "c<n, k=n, remaining<k -> 1.0" true
    (float_eq ~eps:1e-9 result 1.0)

let test_pass_at_k_invalid () =
  (* n < k -> 0.0 *)
  let result = pass_at_k ~n:2 ~c:1 ~k:5 in
  Alcotest.(check bool) "n < k -> 0.0" true (float_eq ~eps:1e-9 result 0.0)

let test_pass_at_k_zero_k () =
  let result = pass_at_k ~n:5 ~c:3 ~k:0 in
  Alcotest.(check bool) "k=0 -> 0.0" true (float_eq ~eps:1e-9 result 0.0)

let test_pass_at_k_negative_c () =
  let result = pass_at_k ~n:5 ~c:(-1) ~k:1 in
  Alcotest.(check bool) "c<0 -> 0.0" true (float_eq ~eps:1e-9 result 0.0)

(* ================================================================ *)
(* 2. Visual grader: Within mode                                    *)
(* ================================================================ *)

let test_visual_within_pass () =
  let g =
    { metric = "ssim"; expected = 0.96; mode = Within 0.02; description = "" }
  in
  let r = apply_visual_grader g 0.95 in
  Alcotest.(check bool) "0.95 within 0.02 of 0.96" true r.passed

let test_visual_within_fail () =
  let g =
    { metric = "ssim"; expected = 0.96; mode = Within 0.005; description = "" }
  in
  let r = apply_visual_grader g 0.90 in
  Alcotest.(check bool) "0.90 NOT within 0.005 of 0.96" false r.passed

(* ================================================================ *)
(* 3. Visual grader: AtLeast mode                                   *)
(* ================================================================ *)

let test_visual_at_least_pass () =
  let g =
    {
      metric = "ssim";
      expected = 0.85;
      mode = AtLeast 0.85;
      description = "";
    }
  in
  let r = apply_visual_grader g 0.90 in
  Alcotest.(check bool) "0.90 >= 0.85" true r.passed

let test_visual_at_least_fail () =
  let g =
    {
      metric = "ssim";
      expected = 0.85;
      mode = AtLeast 0.85;
      description = "";
    }
  in
  let r = apply_visual_grader g 0.80 in
  Alcotest.(check bool) "0.80 < 0.85" false r.passed

(* ================================================================ *)
(* 4. Visual grader: AtMost mode                                    *)
(* ================================================================ *)

let test_visual_at_most_pass () =
  let g =
    {
      metric = "ciede2000";
      expected = 3.0;
      mode = AtMost 3.0;
      description = "";
    }
  in
  let r = apply_visual_grader g 2.0 in
  Alcotest.(check bool) "2.0 <= 3.0" true r.passed

let test_visual_at_most_fail () =
  let g =
    {
      metric = "ciede2000";
      expected = 3.0;
      mode = AtMost 3.0;
      description = "";
    }
  in
  let r = apply_visual_grader g 5.0 in
  Alcotest.(check bool) "5.0 > 3.0" false r.passed

(* ================================================================ *)
(* 5. Scenario run with mock eval_fn                                *)
(* ================================================================ *)

let test_scenario_run_pass () =
  let scenario =
    mk_scenario
      [
        mk_visual_grader ~metric:"ssim" ~expected:0.90 (AtLeast 0.90);
        mk_visual_grader ~metric:"ciede2000" ~expected:3.0 (AtMost 3.0);
      ]
  in
  let eval_fn _s = [ ("ssim", 0.95); ("ciede2000", 1.5) ] in
  let run = run_scenario ~eval_fn scenario in
  Alcotest.(check bool) "run passed" true run.passed;
  Alcotest.(check int) "2 grader results" 2 (List.length run.grader_results);
  Alcotest.(check bool) "no error" true (Option.is_none run.error);
  Alcotest.(check bool) "has trace_id" true
    (String.length run.trace_id > 0)

let test_scenario_run_fail () =
  let scenario =
    mk_scenario
      [ mk_visual_grader ~metric:"ssim" ~expected:0.95 (AtLeast 0.95) ]
  in
  let eval_fn _s = [ ("ssim", 0.80) ] in
  let run = run_scenario ~eval_fn scenario in
  Alcotest.(check bool) "run failed" false run.passed

(* ================================================================ *)
(* 6. Suite run with pass@k                                         *)
(* ================================================================ *)

let test_suite_run () =
  let s1 =
    mk_scenario ~id:"s1"
      [ mk_visual_grader ~metric:"ssim" ~expected:0.90 (AtLeast 0.90) ]
  in
  let s2 =
    mk_scenario ~id:"s2"
      [ mk_visual_grader ~metric:"ciede2000" ~expected:3.0 (AtMost 3.0) ]
  in
  let eval_fn _s = [ ("ssim", 0.95); ("ciede2000", 1.0) ] in
  let suite =
    run_suite ~suite_name:"test-suite" ~eval_fn ~k:1 ~repeats:3 [ s1; s2 ]
  in
  Alcotest.(check string) "suite name" "test-suite" suite.suite_name;
  Alcotest.(check int) "2 results" 2 (List.length suite.results);
  Alcotest.(check bool) "overall 100%" true
    (float_eq ~eps:1e-9 suite.overall_pass_rate 1.0);
  Alcotest.(check bool) "pass_at_k 1.0" true
    (float_eq ~eps:1e-9 suite.pass_at_k 1.0)

let test_suite_empty () =
  let eval_fn _s = [] in
  let suite = run_suite ~suite_name:"empty" ~eval_fn ~k:1 ~repeats:1 [] in
  Alcotest.(check bool) "empty overall 0.0" true
    (float_eq ~eps:1e-9 suite.overall_pass_rate 0.0);
  Alcotest.(check bool) "empty pass_at_k 0.0" true
    (float_eq ~eps:1e-9 suite.pass_at_k 0.0)

(* ================================================================ *)
(* 7. Failed scenario                                               *)
(* ================================================================ *)

let test_scenario_exception () =
  let scenario =
    mk_scenario
      [ mk_visual_grader ~metric:"ssim" ~expected:0.90 (AtLeast 0.90) ]
  in
  let eval_fn _s = failwith "boom" in
  let run = run_scenario ~eval_fn scenario in
  Alcotest.(check bool) "run failed on exception" false run.passed;
  Alcotest.(check bool) "has error" true (Option.is_some run.error);
  Alcotest.(check bool) "error contains 'boom'" true
    (match run.error with
    | Some e -> String.length e > 0 && String.is_prefix ~affix:"Failure" e
    | None -> false)

(* ================================================================ *)
(* 8. JSON serialization                                            *)
(* ================================================================ *)

let test_match_mode_json () =
  let cases =
    [
      (Exact, "exact");
      (Within 0.01, "within");
      (AtLeast 0.5, "at_least");
      (AtMost 3.0, "at_most");
    ]
  in
  List.iter
    (fun (mode, expected_key) ->
      let json = match_mode_to_json mode in
      let s = Yojson.Safe.to_string json in
      Alcotest.(check bool)
        (Printf.sprintf "%s in JSON" expected_key)
        true
        (String.length s > 0))
    cases

let test_suite_result_json () =
  let scenario =
    mk_scenario [ mk_visual_grader ~metric:"ssim" ~expected:0.95 (Exact) ]
  in
  let eval_fn _s = [ ("ssim", 0.95) ] in
  let suite =
    run_suite ~suite_name:"json-test" ~eval_fn ~k:1 ~repeats:1 [ scenario ]
  in
  let json = suite_result_to_json suite in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "JSON is non-empty" true (String.length s > 10)

(* ================================================================ *)
(* 9. match_mode_to_string                                          *)
(* ================================================================ *)

let test_match_mode_to_string () =
  Alcotest.(check string) "exact" "exact" (match_mode_to_string Exact);
  let within_s = match_mode_to_string (Within 0.01) in
  Alcotest.(check bool) "within contains value" true
    (String.is_prefix ~affix:"within(" within_s)

(* ================================================================ *)
(* 10. Deterministic grader in run_scenario                          *)
(* ================================================================ *)

let test_deterministic_grader_in_scenario () =
  let scenario =
    mk_scenario
      [
        Deterministic
          { field = "ssim"; expected = "0.950000"; mode = Exact };
      ]
  in
  let eval_fn _s = [ ("ssim", 0.95) ] in
  let run = run_scenario ~eval_fn scenario in
  Alcotest.(check bool) "deterministic grader ran" true
    (List.length run.grader_results = 1)

(* ================================================================ *)
(* 11. Missing metric defaults to 0.0                               *)
(* ================================================================ *)

let test_missing_metric_defaults () =
  let scenario =
    mk_scenario
      [
        mk_visual_grader ~metric:"nonexistent" ~expected:0.0 (Exact);
      ]
  in
  let eval_fn _s = [ ("ssim", 0.95) ] in
  let run = run_scenario ~eval_fn scenario in
  Alcotest.(check bool) "missing metric -> actual 0.0, passes with Exact 0.0"
    true run.passed

(* ================================================================ *)
(* Test suite registration                                          *)
(* ================================================================ *)

let () =
  Alcotest.run "figma_eval_harness"
    [
      ( "pass_at_k",
        [
          Alcotest.test_case "all pass" `Quick test_pass_at_k_all_pass;
          Alcotest.test_case "none pass" `Quick test_pass_at_k_none_pass;
          Alcotest.test_case "partial" `Quick test_pass_at_k_partial;
          Alcotest.test_case "k=n" `Quick test_pass_at_k_k_equals_n;
          Alcotest.test_case "invalid n<k" `Quick test_pass_at_k_invalid;
          Alcotest.test_case "k=0" `Quick test_pass_at_k_zero_k;
          Alcotest.test_case "negative c" `Quick test_pass_at_k_negative_c;
        ] );
      ( "visual_grader",
        [
          Alcotest.test_case "within pass" `Quick test_visual_within_pass;
          Alcotest.test_case "within fail" `Quick test_visual_within_fail;
          Alcotest.test_case "at_least pass" `Quick test_visual_at_least_pass;
          Alcotest.test_case "at_least fail" `Quick test_visual_at_least_fail;
          Alcotest.test_case "at_most pass" `Quick test_visual_at_most_pass;
          Alcotest.test_case "at_most fail" `Quick test_visual_at_most_fail;
        ] );
      ( "scenario",
        [
          Alcotest.test_case "run pass" `Quick test_scenario_run_pass;
          Alcotest.test_case "run fail" `Quick test_scenario_run_fail;
          Alcotest.test_case "exception" `Quick test_scenario_exception;
          Alcotest.test_case "deterministic" `Quick
            test_deterministic_grader_in_scenario;
          Alcotest.test_case "missing metric" `Quick
            test_missing_metric_defaults;
        ] );
      ( "suite",
        [
          Alcotest.test_case "full run" `Quick test_suite_run;
          Alcotest.test_case "empty" `Quick test_suite_empty;
        ] );
      ( "json",
        [
          Alcotest.test_case "match_mode" `Quick test_match_mode_json;
          Alcotest.test_case "suite_result" `Quick test_suite_result_json;
          Alcotest.test_case "mode_to_string" `Quick test_match_mode_to_string;
        ] );
    ]
