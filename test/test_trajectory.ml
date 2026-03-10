(** Alcotest suite for Figma_trajectory. *)

open Figma_trajectory

(* ================================================================ *)
(* Helpers                                                          *)
(* ================================================================ *)

let tmpdir = Filename.get_temp_dir_name ()

let tmp_path suffix =
  Filename.concat tmpdir
    (Printf.sprintf "test_trajectory_%d_%s.jsonl"
       (int_of_float (Unix.gettimeofday () *. 1000.0))
       suffix)

(* ================================================================ *)
(* 1. Step serialization                                            *)
(* ================================================================ *)

let test_step_json_roundtrip () =
  let step =
    {
      ts = 1700000000.0;
      ts_iso = "2023-11-14T22:13:20Z";
      step_name = "compute_ssim";
      input_summary = "two 256x256 images";
      output_summary = Some "ssim=0.95";
      gate_decision = Pass;
      duration_ms = 42;
      error = None;
    }
  in
  let json = step_to_json step in
  let decoded = step_of_json json in
  Alcotest.(check bool) "roundtrip succeeds" true (Option.is_some decoded);
  let step' = Option.get decoded in
  Alcotest.(check string) "step_name" step.step_name step'.step_name;
  Alcotest.(check string) "input_summary" step.input_summary step'.input_summary;
  Alcotest.(check int) "duration_ms" step.duration_ms step'.duration_ms;
  Alcotest.(check bool) "output_summary" true
    (step'.output_summary = Some "ssim=0.95")

let test_step_with_error () =
  let step =
    {
      ts = 1700000000.0;
      ts_iso = "2023-11-14T22:13:20Z";
      step_name = "fetch";
      input_summary = "url=https://figma.com/...";
      output_summary = None;
      gate_decision = Reject "rate limited";
      duration_ms = 1000;
      error = Some "HTTP 429";
    }
  in
  let json = step_to_json step in
  let decoded = step_of_json json in
  Alcotest.(check bool) "roundtrip succeeds" true (Option.is_some decoded);
  let step' = Option.get decoded in
  Alcotest.(check bool) "error present" true (step'.error = Some "HTTP 429");
  Alcotest.(check bool) "gate = Reject" true
    (match step'.gate_decision with Reject _ -> true | _ -> false)

let test_step_invalid_json () =
  let json = `String "not a step" in
  let decoded = step_of_json json in
  Alcotest.(check bool) "invalid returns None" true (Option.is_none decoded)

(* ================================================================ *)
(* 2. Trajectory serialization                                      *)
(* ================================================================ *)

let test_trajectory_json () =
  let traj =
    {
      scenario_id = Some "scenario-1";
      pipeline_name = "visual-compare";
      trace_id = "trace-abc";
      started_at = 1700000000.0;
      ended_at = 1700000005.0;
      steps = [];
      total_steps = 0;
      outcome = Completed;
    }
  in
  let json = trajectory_to_json traj in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "contains trace_id" true
    (let open Yojson.Safe.Util in
     json |> member "trace_id" |> to_string = "trace-abc");
  Alcotest.(check bool) "JSON non-empty" true (String.length s > 10)

let test_trajectory_no_scenario () =
  let traj =
    {
      scenario_id = None;
      pipeline_name = "test";
      trace_id = "t1";
      started_at = 0.0;
      ended_at = 1.0;
      steps = [];
      total_steps = 0;
      outcome = Timeout;
    }
  in
  let json = trajectory_to_json traj in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "scenario_id is null" true
    (json |> member "scenario_id" = `Null)

(* ================================================================ *)
(* 3. Accumulator                                                   *)
(* ================================================================ *)

let test_accumulator_basic () =
  let acc =
    create_accumulator ~scenario_id:"s1" ~pipeline_name:"test"
      ~trace_id:"tr1" ()
  in
  let step1 =
    make_step ~step_name:"fetch" ~input_summary:"url" ~duration_ms:10 ()
  in
  let step2 =
    make_step ~step_name:"compare" ~input_summary:"two images"
      ~output_summary:"ssim=0.92" ~duration_ms:50 ()
  in
  add_step acc step1;
  add_step acc step2;
  let traj = finalize acc Completed in
  Alcotest.(check int) "total_steps" 2 traj.total_steps;
  Alcotest.(check int) "steps list length" 2 (List.length traj.steps);
  Alcotest.(check string) "first step name" "fetch"
    (List.hd traj.steps).step_name;
  Alcotest.(check string) "pipeline_name" "test" traj.pipeline_name;
  Alcotest.(check bool) "scenario_id" true
    (traj.scenario_id = Some "s1");
  Alcotest.(check bool) "outcome Completed" true
    (traj.outcome = Completed)

let test_accumulator_empty () =
  let acc =
    create_accumulator ~pipeline_name:"empty" ~trace_id:"tr0" ()
  in
  let traj = finalize acc (Failed "no steps") in
  Alcotest.(check int) "total_steps 0" 0 traj.total_steps;
  Alcotest.(check bool) "outcome Failed" true
    (match traj.outcome with Failed _ -> true | _ -> false)

(* ================================================================ *)
(* 4. JSONL file I/O                                                *)
(* ================================================================ *)

let test_jsonl_write_read () =
  let path = tmp_path "write_read" in
  (* Clean up if leftover from a previous test run *)
  (if Sys.file_exists path then Sys.remove path);
  let step1 =
    {
      ts = 1700000001.0;
      ts_iso = "2023-11-14T22:13:21Z";
      step_name = "step_a";
      input_summary = "input_a";
      output_summary = Some "output_a";
      gate_decision = Pass;
      duration_ms = 10;
      error = None;
    }
  in
  let step2 =
    {
      ts = 1700000002.0;
      ts_iso = "2023-11-14T22:13:22Z";
      step_name = "step_b";
      input_summary = "input_b";
      output_summary = None;
      gate_decision = Reject "bad input";
      duration_ms = 20;
      error = Some "validation failed";
    }
  in
  append_step ~path step1;
  append_step ~path step2;
  let steps = read_steps ~path in
  Alcotest.(check int) "read 2 steps" 2 (List.length steps);
  Alcotest.(check string) "first step name" "step_a"
    (List.nth steps 0).step_name;
  Alcotest.(check string) "second step name" "step_b"
    (List.nth steps 1).step_name;
  Alcotest.(check bool) "second has error" true
    ((List.nth steps 1).error = Some "validation failed");
  (* Clean up *)
  Sys.remove path

let test_jsonl_read_missing_file () =
  let steps = read_steps ~path:"/tmp/nonexistent_test_file_12345.jsonl" in
  Alcotest.(check int) "missing file -> empty" 0 (List.length steps)

let test_jsonl_append_trajectory () =
  let path = tmp_path "trajectory" in
  (if Sys.file_exists path then Sys.remove path);
  let traj =
    {
      scenario_id = Some "s1";
      pipeline_name = "test";
      trace_id = "t1";
      started_at = 100.0;
      ended_at = 200.0;
      steps = [];
      total_steps = 0;
      outcome = Completed;
    }
  in
  append_trajectory ~path traj;
  (* Verify the file contains valid JSON *)
  let ic = open_in path in
  let line = input_line ic in
  close_in ic;
  let json = Yojson.Safe.from_string line in
  Alcotest.(check string) "trace_id in file" "t1"
    (Yojson.Safe.Util.(json |> member "trace_id" |> to_string));
  Sys.remove path

(* ================================================================ *)
(* 5. Gate decision serialization                                   *)
(* ================================================================ *)

let test_gate_pass_json () =
  let json = gate_decision_to_json Pass in
  Alcotest.(check string) "pass -> string" "\"pass\""
    (Yojson.Safe.to_string json);
  let decoded = gate_decision_of_json json in
  Alcotest.(check bool) "roundtrip Pass" true (decoded = Pass)

let test_gate_reject_json () =
  let json = gate_decision_to_json (Reject "too slow") in
  let decoded = gate_decision_of_json json in
  Alcotest.(check bool) "roundtrip Reject" true
    (match decoded with Reject "too slow" -> true | _ -> false)

let test_gate_unknown_json () =
  let decoded = gate_decision_of_json (`Int 42) in
  Alcotest.(check bool) "unknown -> Pass" true (decoded = Pass)

(* ================================================================ *)
(* 6. Outcome serialization                                         *)
(* ================================================================ *)

let test_outcome_completed () =
  let json = outcome_to_json Completed in
  let decoded = outcome_of_json json in
  Alcotest.(check bool) "completed roundtrip" true (decoded = Completed)

let test_outcome_timeout () =
  let json = outcome_to_json Timeout in
  let decoded = outcome_of_json json in
  Alcotest.(check bool) "timeout roundtrip" true (decoded = Timeout)

let test_outcome_failed () =
  let json = outcome_to_json (Failed "crash") in
  let decoded = outcome_of_json json in
  Alcotest.(check bool) "failed roundtrip" true
    (match decoded with Failed "crash" -> true | _ -> false)

let test_outcome_gated () =
  let json = outcome_to_json (Gated "cost limit") in
  let decoded = outcome_of_json json in
  Alcotest.(check bool) "gated roundtrip" true
    (match decoded with Gated "cost limit" -> true | _ -> false)

let test_outcome_unknown () =
  let decoded = outcome_of_json (`Int 0) in
  Alcotest.(check bool) "unknown -> Completed" true (decoded = Completed)

(* ================================================================ *)
(* 7. iso8601_of_float                                              *)
(* ================================================================ *)

let test_iso8601 () =
  (* 2023-11-14T22:13:20Z = 1700000000.0 *)
  let s = iso8601_of_float 1700000000.0 in
  Alcotest.(check string) "ISO8601 format" "2023-11-14T22:13:20Z" s

(* ================================================================ *)
(* Test suite registration                                          *)
(* ================================================================ *)

let () =
  Alcotest.run "figma_trajectory"
    [
      ( "step_json",
        [
          Alcotest.test_case "roundtrip" `Quick test_step_json_roundtrip;
          Alcotest.test_case "with error" `Quick test_step_with_error;
          Alcotest.test_case "invalid" `Quick test_step_invalid_json;
        ] );
      ( "trajectory_json",
        [
          Alcotest.test_case "basic" `Quick test_trajectory_json;
          Alcotest.test_case "no scenario" `Quick test_trajectory_no_scenario;
        ] );
      ( "accumulator",
        [
          Alcotest.test_case "basic" `Quick test_accumulator_basic;
          Alcotest.test_case "empty" `Quick test_accumulator_empty;
        ] );
      ( "jsonl_io",
        [
          Alcotest.test_case "write/read" `Quick test_jsonl_write_read;
          Alcotest.test_case "missing file" `Quick
            test_jsonl_read_missing_file;
          Alcotest.test_case "append trajectory" `Quick
            test_jsonl_append_trajectory;
        ] );
      ( "gate_decision",
        [
          Alcotest.test_case "pass" `Quick test_gate_pass_json;
          Alcotest.test_case "reject" `Quick test_gate_reject_json;
          Alcotest.test_case "unknown" `Quick test_gate_unknown_json;
        ] );
      ( "outcome",
        [
          Alcotest.test_case "completed" `Quick test_outcome_completed;
          Alcotest.test_case "timeout" `Quick test_outcome_timeout;
          Alcotest.test_case "failed" `Quick test_outcome_failed;
          Alcotest.test_case "gated" `Quick test_outcome_gated;
          Alcotest.test_case "unknown" `Quick test_outcome_unknown;
        ] );
      ( "helpers",
        [
          Alcotest.test_case "iso8601" `Quick test_iso8601;
        ] );
    ]
