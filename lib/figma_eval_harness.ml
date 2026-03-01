(** Figma Eval Harness — Scenario-based evaluation for visual accuracy.

    Adapted from masc-mcp's eval_harness pattern. Instead of text-based
    graders, uses visual accuracy metrics:
    - CIEDE2000 color distance
    - SSIM structural similarity
    - Pixel-level comparison

    @since 1.5.0 *)

(* ================================================================ *)
(* Types                                                            *)
(* ================================================================ *)

type match_mode =
  | Exact            (** exact float equality *)
  | Within of float  (** within epsilon *)
  | AtLeast of float (** >= threshold *)
  | AtMost of float  (** <= threshold *)

type visual_grader = {
  metric : string;       (** "ciede2000" | "ssim" | "pixel_diff" *)
  expected : float;
  mode : match_mode;
  description : string;
}

type grader =
  | Visual of visual_grader
  | Deterministic of { field : string; expected : string; mode : match_mode }

type scenario = {
  id : string;
  name : string;
  description : string;
  input_image : string;     (** path or reference to input *)
  reference_image : string; (** path or reference to expected output *)
  graders : grader list;
  max_duration_sec : float;
  tags : string list;
  difficulty : string;      (** "easy" | "medium" | "hard" *)
}

type grader_result = {
  grader : grader;
  passed : bool;
  actual_value : float;
  expected_value : float;
  details : string;
}

type eval_run = {
  scenario : scenario;
  passed : bool;
  grader_results : grader_result list;
  duration_sec : float;
  trace_id : string;
  error : string option;
}

type eval_result = {
  scenario_id : string;
  runs : eval_run list;
  pass_rate : float;
}

type eval_suite_result = {
  suite_name : string;
  results : eval_result list;
  overall_pass_rate : float;
  pass_at_k : float;
}

(* ================================================================ *)
(* Grader application                                               *)
(* ================================================================ *)

let apply_match_mode (mode : match_mode) (actual : float) (expected : float)
    : bool =
  match mode with
  | Exact -> Float.equal actual expected
  | Within eps -> Float.abs (actual -. expected) <= eps
  | AtLeast threshold -> actual >= threshold
  | AtMost threshold -> actual <= threshold

let match_mode_to_string = function
  | Exact -> "exact"
  | Within eps -> Printf.sprintf "within(%.4f)" eps
  | AtLeast t -> Printf.sprintf "at_least(%.4f)" t
  | AtMost t -> Printf.sprintf "at_most(%.4f)" t

let apply_visual_grader (g : visual_grader) (actual_value : float)
    : grader_result =
  let passed = apply_match_mode g.mode actual_value g.expected in
  {
    grader = Visual g;
    passed;
    actual_value;
    expected_value = g.expected;
    details =
      Printf.sprintf "%s: actual=%.6f expected=%.6f mode=%s %s" g.metric
        actual_value g.expected
        (match_mode_to_string g.mode)
        (if passed then "PASS" else "FAIL");
  }

(* ================================================================ *)
(* Pass@k calculation                                               *)
(* ================================================================ *)

(** Calculate pass@k metric.
    pass\@k = 1 - C(n-c, k) / C(n, k)
    where n = total runs, c = passing runs, k = attempts.
    Uses log-space for numerical stability. *)
let pass_at_k ~(n : int) ~(c : int) ~(k : int) : float =
  if n < k || c < 0 || k <= 0 then 0.0
  else if c >= n then 1.0
  else begin
    let log_comb total choose =
      let result = ref 0.0 in
      for i = 0 to choose - 1 do
        result :=
          !result
          +. Float.log (Float.of_int (total - i))
          -. Float.log (Float.of_int (i + 1))
      done;
      !result
    in
    let remaining = n - c in
    if remaining < k then 1.0
    else
      let log_total = log_comb n k in
      let log_fail = log_comb remaining k in
      1.0 -. Float.exp (log_fail -. log_total)
  end

(* ================================================================ *)
(* Suite runner                                                     *)
(* ================================================================ *)

(** Run a single scenario with a provided evaluation function.
    [eval_fn] takes a scenario and returns [(metric_name, actual_value)] list. *)
let run_scenario ~(eval_fn : scenario -> (string * float) list)
    (scenario : scenario) : eval_run =
  let trace_id =
    Printf.sprintf "figma-eval-%s-%d" scenario.id
      (int_of_float (Unix.gettimeofday () *. 1000.0))
  in
  let t0 = Unix.gettimeofday () in
  try
    let metrics = eval_fn scenario in
    let duration_sec = Unix.gettimeofday () -. t0 in
    let grader_results =
      List.map
        (fun grader ->
          match grader with
          | Visual vg ->
              let actual =
                List.assoc_opt vg.metric metrics
                |> Option.value ~default:0.0
              in
              apply_visual_grader vg actual
          | Deterministic { field; expected; mode } ->
              let actual_str =
                List.assoc_opt field
                  (List.map
                     (fun (k, v) -> (k, Printf.sprintf "%.6f" v))
                     metrics)
                |> Option.value ~default:""
              in
              let passed =
                match mode with
                | Exact -> String.equal actual_str expected
                | _ -> String.equal actual_str expected
              in
              {
                grader = Deterministic { field; expected; mode };
                passed;
                actual_value = 0.0;
                expected_value = 0.0;
                details =
                  Printf.sprintf "field=%s actual=%s expected=%s %s" field
                    actual_str expected
                    (if passed then "PASS" else "FAIL");
              })
        scenario.graders
    in
    let passed = List.for_all (fun (r : grader_result) -> r.passed) grader_results in
    { scenario; passed; grader_results; duration_sec; trace_id; error = None }
  with exn ->
    let duration_sec = Unix.gettimeofday () -. t0 in
    {
      scenario;
      passed = false;
      grader_results = [];
      duration_sec;
      trace_id;
      error = Some (Printexc.to_string exn);
    }

(** Run a suite of scenarios, each repeated [repeats] times for pass\@k. *)
let run_suite ~(suite_name : string)
    ~(eval_fn : scenario -> (string * float) list) ~(k : int)
    ~(repeats : int) (scenarios : scenario list) : eval_suite_result =
  let results =
    List.map
      (fun scenario ->
        let runs =
          List.init repeats (fun _ -> run_scenario ~eval_fn scenario)
        in
        let pass_count =
          List.length (List.filter (fun r -> r.passed) runs)
        in
        let pass_rate = Float.of_int pass_count /. Float.of_int repeats in
        { scenario_id = scenario.id; runs; pass_rate })
      scenarios
  in
  let overall_pass_rate =
    let total = List.length results in
    if total = 0 then 0.0
    else
      let passing =
        List.length (List.filter (fun r -> r.pass_rate >= 1.0) results)
      in
      Float.of_int passing /. Float.of_int total
  in
  let pass_at_k_val =
    let total = List.length results in
    if total = 0 then 0.0
    else
      let sum =
        List.fold_left
          (fun acc r ->
            let n = List.length r.runs in
            let c =
              List.length (List.filter (fun run -> run.passed) r.runs)
            in
            acc +. pass_at_k ~n ~c ~k)
          0.0 results
      in
      sum /. Float.of_int total
  in
  { suite_name; results; overall_pass_rate; pass_at_k = pass_at_k_val }

(* ================================================================ *)
(* JSON serialization                                               *)
(* ================================================================ *)

let match_mode_to_json = function
  | Exact -> `String "exact"
  | Within eps -> `Assoc [ ("within", `Float eps) ]
  | AtLeast t -> `Assoc [ ("at_least", `Float t) ]
  | AtMost t -> `Assoc [ ("at_most", `Float t) ]

let grader_result_to_json (r : grader_result) : Yojson.Safe.t =
  `Assoc
    [
      ("passed", `Bool r.passed);
      ("actual", `Float r.actual_value);
      ("expected", `Float r.expected_value);
      ("details", `String r.details);
    ]

let eval_run_to_json (r : eval_run) : Yojson.Safe.t =
  `Assoc
    [
      ("scenario_id", `String r.scenario.id);
      ("passed", `Bool r.passed);
      ("duration_sec", `Float r.duration_sec);
      ("trace_id", `String r.trace_id);
      ( "error",
        match r.error with None -> `Null | Some e -> `String e );
      ( "grader_results",
        `List (List.map grader_result_to_json r.grader_results) );
    ]

let suite_result_to_json (s : eval_suite_result) : Yojson.Safe.t =
  `Assoc
    [
      ("suite_name", `String s.suite_name);
      ("overall_pass_rate", `Float s.overall_pass_rate);
      ("pass_at_k", `Float s.pass_at_k);
      ( "results",
        `List
          (List.map
             (fun r ->
               `Assoc
                 [
                   ("scenario_id", `String r.scenario_id);
                   ("pass_rate", `Float r.pass_rate);
                   ("runs", `List (List.map eval_run_to_json r.runs));
                 ])
             s.results) );
    ]
