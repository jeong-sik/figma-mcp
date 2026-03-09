(** Figma Trajectory — JSONL tool call logging for visual pipeline evaluation.

    Records each step of the visual processing pipeline:
    - Image fetch
    - Color extraction
    - SSIM computation
    - CIEDE2000 computation

    @since 1.5.0 *)

(** {1 Types} *)

type gate_decision =
  | Pass
  | Reject of string

type pipeline_step = {
  ts : float;
  ts_iso : string;
  step_name : string;
  input_summary : string;
  output_summary : string option;
  gate_decision : gate_decision;
  duration_ms : int;
  error : string option;
}

type trajectory_outcome =
  | Completed
  | Failed of string
  | Timeout
  | Gated of string

type trajectory = {
  scenario_id : string option;
  pipeline_name : string;
  trace_id : string;
  started_at : float;
  ended_at : float;
  steps : pipeline_step list;
  total_steps : int;
  outcome : trajectory_outcome;
}

(** {1 Accumulator} *)

type accumulator
(** Mutable per-session state for building trajectories incrementally. *)

val create_accumulator :
  ?scenario_id:string ->
  pipeline_name:string ->
  trace_id:string ->
  unit ->
  accumulator
(** Create a new accumulator. Records start time automatically. *)

val add_step : accumulator -> pipeline_step -> unit
(** Append a step to the accumulator. *)

val finalize : accumulator -> trajectory_outcome -> trajectory
(** Finalize the accumulator into an immutable trajectory.
    Records end time and reverses step order. *)

(** {1 Helpers} *)

val iso8601_of_float : float -> string
(** Convert a Unix timestamp to ISO8601 UTC string. *)

val make_step :
  step_name:string ->
  input_summary:string ->
  ?output_summary:string ->
  ?gate_decision:gate_decision ->
  duration_ms:int ->
  ?error:string ->
  unit ->
  pipeline_step
(** Convenience builder. Fills in [ts] and [ts_iso] automatically. *)

(** {1 JSON serialization} *)

val gate_decision_to_json : gate_decision -> Yojson.Safe.t
val gate_decision_of_json : Yojson.Safe.t -> gate_decision
val step_to_json : pipeline_step -> Yojson.Safe.t
val step_of_json : Yojson.Safe.t -> pipeline_step option
val outcome_to_json : trajectory_outcome -> Yojson.Safe.t
val outcome_of_json : Yojson.Safe.t -> trajectory_outcome
val trajectory_to_json : trajectory -> Yojson.Safe.t

(** {1 JSONL file I/O} *)

val append_step : path:string -> pipeline_step -> unit
(** Append a single step as one JSONL line. *)

val append_trajectory : path:string -> trajectory -> unit
(** Append a full trajectory as one JSONL line. *)

val read_steps : path:string -> pipeline_step list
(** Read all steps from a JSONL file. Returns empty list if file missing. *)
