(** Figma Eval Harness — Scenario-based evaluation for visual accuracy.

    Adapted from masc-mcp's eval_harness pattern. Instead of text-based
    graders, uses visual accuracy metrics:
    - CIEDE2000 color distance
    - SSIM structural similarity
    - Pixel-level comparison

    @since 1.5.0 *)

(** {1 Types} *)

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
  input_image : string;
  reference_image : string;
  graders : grader list;
  max_duration_sec : float;
  tags : string list;
  difficulty : string;
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

(** {1 Grader application} *)

val apply_match_mode : match_mode -> float -> float -> bool
(** [apply_match_mode mode actual expected] tests whether [actual] satisfies
    [mode] relative to [expected]. *)

val match_mode_to_string : match_mode -> string
(** Human-readable representation of a match mode. *)

val apply_visual_grader : visual_grader -> float -> grader_result
(** [apply_visual_grader grader actual_value] runs a single visual grader
    against the measured value. *)

(** {1 Pass\@k} *)

val pass_at_k : n:int -> c:int -> k:int -> float
(** [pass_at_k ~n ~c ~k] computes the pass\@k metric.
    [n] = total runs, [c] = passing runs, [k] = attempts.
    Returns 0.0 when [n < k] or inputs are invalid. *)

(** {1 Suite runner} *)

val run_scenario :
  eval_fn:(scenario -> (string * float) list) -> scenario -> eval_run
(** Run a single scenario. [eval_fn] returns metric name-value pairs. *)

val run_suite :
  suite_name:string ->
  eval_fn:(scenario -> (string * float) list) ->
  k:int ->
  repeats:int ->
  scenario list ->
  eval_suite_result
(** Run all scenarios with [repeats] repetitions and compute pass\@k. *)

(** {1 JSON serialization} *)

val match_mode_to_json : match_mode -> Yojson.Safe.t
val grader_result_to_json : grader_result -> Yojson.Safe.t
val eval_run_to_json : eval_run -> Yojson.Safe.t
val suite_result_to_json : eval_suite_result -> Yojson.Safe.t
