(** Figma Trajectory — JSONL tool call logging for visual pipeline evaluation.

    Records each step of the visual processing pipeline:
    - Image fetch
    - Color extraction
    - SSIM computation
    - CIEDE2000 computation

    @since 1.5.0 *)

(* ================================================================ *)
(* Types                                                            *)
(* ================================================================ *)

type gate_decision =
  | Pass
  | Reject of string

type pipeline_step = {
  ts : float;
  ts_iso : string;
  step_name : string;          (** "fetch" | "extract_colors" | "compute_ssim" | ... *)
  input_summary : string;     (** brief description of input *)
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

(* ================================================================ *)
(* Accumulator (mutable per-session state)                          *)
(* ================================================================ *)

type accumulator = {
  acc_scenario_id : string option;
  acc_pipeline_name : string;
  acc_trace_id : string;
  acc_started_at : float;
  mutable acc_steps : pipeline_step list;
}

let create_accumulator ?scenario_id ~pipeline_name ~trace_id () : accumulator =
  {
    acc_scenario_id = scenario_id;
    acc_pipeline_name = pipeline_name;
    acc_trace_id = trace_id;
    acc_started_at = Unix.gettimeofday ();
    acc_steps = [];
  }

let add_step (acc : accumulator) (step : pipeline_step) : unit =
  acc.acc_steps <- step :: acc.acc_steps

let finalize (acc : accumulator) (outcome : trajectory_outcome) : trajectory =
  let steps = List.rev acc.acc_steps in
  {
    scenario_id = acc.acc_scenario_id;
    pipeline_name = acc.acc_pipeline_name;
    trace_id = acc.acc_trace_id;
    started_at = acc.acc_started_at;
    ended_at = Unix.gettimeofday ();
    steps;
    total_steps = List.length steps;
    outcome;
  }

(* ================================================================ *)
(* ISO8601 timestamp helper                                         *)
(* ================================================================ *)

let iso8601_of_float (ts : float) : string =
  let tm = Unix.gmtime ts in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

(* ================================================================ *)
(* Convenience step builder                                         *)
(* ================================================================ *)

let make_step ~step_name ~input_summary ?output_summary
    ?(gate_decision = Pass) ~duration_ms ?error () : pipeline_step =
  let ts = Unix.gettimeofday () in
  {
    ts;
    ts_iso = iso8601_of_float ts;
    step_name;
    input_summary;
    output_summary;
    gate_decision;
    duration_ms;
    error;
  }

(* ================================================================ *)
(* JSON serialization                                               *)
(* ================================================================ *)

let gate_decision_to_json = function
  | Pass -> `String "pass"
  | Reject reason -> `Assoc [ ("reject", `String reason) ]

let gate_decision_of_json (json : Yojson.Safe.t) : gate_decision =
  match json with
  | `String "pass" -> Pass
  | `Assoc [ ("reject", `String reason) ] -> Reject reason
  | _ -> Pass

let step_to_json (s : pipeline_step) : Yojson.Safe.t =
  `Assoc
    [
      ("ts", `Float s.ts);
      ("ts_iso", `String s.ts_iso);
      ("step_name", `String s.step_name);
      ("input_summary", `String s.input_summary);
      ( "output_summary",
        match s.output_summary with
        | None -> `Null
        | Some o -> `String o );
      ("gate_decision", gate_decision_to_json s.gate_decision);
      ("duration_ms", `Int s.duration_ms);
      ( "error",
        match s.error with None -> `Null | Some e -> `String e );
    ]

let step_of_json (json : Yojson.Safe.t) : pipeline_step option =
  match json with
  | `Assoc fields -> (
      let open Yojson.Safe.Util in
      try
        let ts = json |> member "ts" |> to_float in
        let ts_iso = json |> member "ts_iso" |> to_string in
        let step_name = json |> member "step_name" |> to_string in
        let input_summary = json |> member "input_summary" |> to_string in
        let output_summary =
          match List.assoc_opt "output_summary" fields with
          | Some (`String s) -> Some s
          | _ -> None
        in
        let gate_decision =
          gate_decision_of_json (json |> member "gate_decision")
        in
        let duration_ms = json |> member "duration_ms" |> to_int in
        let error =
          match List.assoc_opt "error" fields with
          | Some (`String s) -> Some s
          | _ -> None
        in
        Some
          {
            ts;
            ts_iso;
            step_name;
            input_summary;
            output_summary;
            gate_decision;
            duration_ms;
            error;
          }
      with _ -> None)
  | _ -> None

let outcome_to_json = function
  | Completed -> `String "completed"
  | Failed msg -> `Assoc [ ("failed", `String msg) ]
  | Timeout -> `String "timeout"
  | Gated msg -> `Assoc [ ("gated", `String msg) ]

let outcome_of_json (json : Yojson.Safe.t) : trajectory_outcome =
  match json with
  | `String "completed" -> Completed
  | `String "timeout" -> Timeout
  | `Assoc [ ("failed", `String msg) ] -> Failed msg
  | `Assoc [ ("gated", `String msg) ] -> Gated msg
  | _ -> Completed

let trajectory_to_json (t : trajectory) : Yojson.Safe.t =
  `Assoc
    [
      ( "scenario_id",
        match t.scenario_id with
        | None -> `Null
        | Some s -> `String s );
      ("pipeline_name", `String t.pipeline_name);
      ("trace_id", `String t.trace_id);
      ("started_at", `Float t.started_at);
      ("ended_at", `Float t.ended_at);
      ("steps", `List (List.map step_to_json t.steps));
      ("total_steps", `Int t.total_steps);
      ("outcome", outcome_to_json t.outcome);
    ]

(* ================================================================ *)
(* JSONL file I/O                                                   *)
(* ================================================================ *)

let append_jsonl ~(path : string) (json : Yojson.Safe.t) : unit =
  let oc = open_out_gen [ Open_append; Open_creat; Open_text ] 0o644 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc (Yojson.Safe.to_string json);
      output_char oc '\n')

let append_step ~(path : string) (step : pipeline_step) : unit =
  append_jsonl ~path (step_to_json step)

let append_trajectory ~(path : string) (traj : trajectory) : unit =
  append_jsonl ~path (trajectory_to_json traj)

let read_steps ~(path : string) : pipeline_step list =
  if not (Sys.file_exists path) then []
  else begin
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let steps = ref [] in
        (try
           while true do
             let line = input_line ic in
             if String.length line > 0 then begin
               let json = Yojson.Safe.from_string line in
               match step_of_json json with
               | Some step -> steps := step :: !steps
               | None -> ()
             end
           done
         with End_of_file -> ());
        List.rev !steps)
  end
