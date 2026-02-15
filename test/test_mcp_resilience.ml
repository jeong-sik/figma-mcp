(** Comprehensive tests for mcp_resilience.ml — Circuit Breaker, Retry, Timeout.

    All functions use Eio.Mutex, so every test wraps in Eio_main.run.
    Coverage target: 90%+ of 224 lines. *)

open Alcotest

(* ============================================ *)
(* Helpers                                       *)
(* ============================================ *)

(** Collect log messages for assertion *)
let make_spy_logger () =
  let log = ref [] in
  let logger level msg =
    let lvl = match level with
      | Mcp_resilience.Debug -> "DEBUG"
      | Mcp_resilience.Info -> "INFO"
      | Mcp_resilience.Warn -> "WARN"
      | Mcp_resilience.Err -> "ERR"
    in
    log := (lvl, msg) :: !log
  in
  (logger, log)

(** Check that at least one log entry contains the substring *)
let assert_log_contains log substr =
  let found = List.exists (fun (_, msg) -> try
    let _ = Str.search_forward (Str.regexp_string substr) msg 0 in true
  with Not_found -> false) !log in
  if not found then
    fail (Printf.sprintf "Expected log containing %S, got: [%s]"
      substr
      (String.concat "; " (List.map (fun (l,m) -> l ^ ":" ^ m) !log)))

(* ============================================ *)
(* Circuit Breaker: create + defaults            *)
(* ============================================ *)

let test_create_defaults () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker ~name:"test" () in
  check int "failure_threshold default" 5 cb.failure_threshold;
  check int "success_threshold default" 2 cb.success_threshold;
  check int "timeout_ms default" 30000 cb.timeout_ms;
  check string "name" "test" cb.name;
  (* Initial state is Closed *)
  (match cb.state with
   | Mcp_resilience.Closed -> ()
   | _ -> fail "expected Closed state");
  check int "failure_count" 0 cb.failure_count;
  check int "success_count" 0 cb.success_count;
  check bool "probe_in_progress" false cb.probe_in_progress

let test_create_custom_params () =
  Eio_main.run @@ fun _env ->
  let logger, _log = make_spy_logger () in
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:2
    ~success_threshold:1
    ~timeout_ms:5000
    ~logger
    ~name:"custom"
    ()
  in
  check int "failure_threshold" 2 cb.failure_threshold;
  check int "success_threshold" 1 cb.success_threshold;
  check int "timeout_ms" 5000 cb.timeout_ms;
  check string "name" "custom" cb.name

(* ============================================ *)
(* Circuit Breaker: circuit_allows               *)
(* ============================================ *)

let test_closed_allows () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker ~name:"t" () in
  let result = Mcp_resilience.circuit_allows cb in
  check bool "closed allows" true result

let test_open_blocks () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:1 ~timeout_ms:60000 ~name:"t" () in
  (* Force open state *)
  Mcp_resilience.circuit_record_failure cb;
  (match cb.state with
   | Mcp_resilience.Open -> ()
   | _ -> fail "expected Open after failure_threshold=1");
  (* Open with large timeout => blocks *)
  let result = Mcp_resilience.circuit_allows cb in
  check bool "open blocks" false result

let test_open_transitions_to_halfopen_after_timeout () =
  Eio_main.run @@ fun _env ->
  let logger, log = make_spy_logger () in
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:1 ~timeout_ms:0 ~logger ~name:"halfopen-test" () in
  (* Force open *)
  Mcp_resilience.circuit_record_failure cb;
  (match cb.state with
   | Mcp_resilience.Open -> ()
   | _ -> fail "expected Open");
  (* timeout_ms=0 means elapsed always >= timeout, so allows => HalfOpen *)
  let result = Mcp_resilience.circuit_allows cb in
  check bool "allows after timeout" true result;
  (match cb.state with
   | Mcp_resilience.HalfOpen -> ()
   | _ -> fail "expected HalfOpen after timeout");
  check int "success_count reset to 0" 0 cb.success_count;
  check bool "probe_in_progress set" true cb.probe_in_progress;
  assert_log_contains log "HalfOpen"

let test_halfopen_blocks_while_probe_in_progress () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:1 ~timeout_ms:0 ~name:"t" () in
  (* Force open then transition to HalfOpen *)
  Mcp_resilience.circuit_record_failure cb;
  let _ = Mcp_resilience.circuit_allows cb in (* transitions to HalfOpen, sets probe *)
  check bool "probe_in_progress" true cb.probe_in_progress;
  (* Second call while probe_in_progress => blocked *)
  let result = Mcp_resilience.circuit_allows cb in
  check bool "blocked during probe" false result

let test_halfopen_allows_when_no_probe () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:1 ~timeout_ms:0 ~name:"t" () in
  (* Force to HalfOpen *)
  Mcp_resilience.circuit_record_failure cb;
  let _ = Mcp_resilience.circuit_allows cb in
  (* Manually clear probe_in_progress to test the else branch *)
  cb.probe_in_progress <- false;
  let result = Mcp_resilience.circuit_allows cb in
  check bool "allows when no probe" true result;
  check bool "probe set again" true cb.probe_in_progress

(* ============================================ *)
(* Circuit Breaker: record_success               *)
(* ============================================ *)

let test_success_in_closed_resets_failure_count () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:5 ~name:"t" () in
  (* Accumulate some failures without crossing threshold *)
  Mcp_resilience.circuit_record_failure cb;
  Mcp_resilience.circuit_record_failure cb;
  check int "failure_count after 2" 2 cb.failure_count;
  (* Success resets failure_count *)
  Mcp_resilience.circuit_record_success cb;
  check int "failure_count after success" 0 cb.failure_count;
  (match cb.state with
   | Mcp_resilience.Closed -> ()
   | _ -> fail "should remain Closed")

let test_success_in_halfopen_increments_and_closes () =
  Eio_main.run @@ fun _env ->
  let logger, log = make_spy_logger () in
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:1 ~success_threshold:2 ~timeout_ms:0 ~logger ~name:"recovery" () in
  (* Force Open -> HalfOpen *)
  Mcp_resilience.circuit_record_failure cb;
  let _ = Mcp_resilience.circuit_allows cb in
  (match cb.state with Mcp_resilience.HalfOpen -> () | _ -> fail "expected HalfOpen");
  (* First success: count=1, still HalfOpen *)
  Mcp_resilience.circuit_record_success cb;
  check int "success_count after 1 success" 1 cb.success_count;
  check bool "probe cleared" false cb.probe_in_progress;
  (match cb.state with Mcp_resilience.HalfOpen -> () | _ -> fail "still HalfOpen");
  (* Need to allow again so probe_in_progress is set for the second attempt *)
  let _ = Mcp_resilience.circuit_allows cb in
  (* Second success: reaches threshold => Closed *)
  Mcp_resilience.circuit_record_success cb;
  (match cb.state with
   | Mcp_resilience.Closed -> ()
   | _ -> fail "expected Closed after reaching success_threshold");
  check int "failure_count reset" 0 cb.failure_count;
  check int "success_count reset" 0 cb.success_count;
  assert_log_contains log "recovered"

let test_success_in_open_is_noop () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:1 ~timeout_ms:999999 ~name:"t" () in
  (* Force open *)
  Mcp_resilience.circuit_record_failure cb;
  let fc = cb.failure_count in
  let sc = cb.success_count in
  (* Success in Open does nothing *)
  Mcp_resilience.circuit_record_success cb;
  check int "failure_count unchanged" fc cb.failure_count;
  check int "success_count unchanged" sc cb.success_count;
  (match cb.state with Mcp_resilience.Open -> () | _ -> fail "still Open")

(* ============================================ *)
(* Circuit Breaker: record_failure               *)
(* ============================================ *)

let test_failure_in_closed_increments () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:3 ~name:"t" () in
  Mcp_resilience.circuit_record_failure cb;
  check int "failure_count 1" 1 cb.failure_count;
  Mcp_resilience.circuit_record_failure cb;
  check int "failure_count 2" 2 cb.failure_count;
  (match cb.state with Mcp_resilience.Closed -> () | _ -> fail "still Closed")

let test_failure_in_closed_opens_at_threshold () =
  Eio_main.run @@ fun _env ->
  let logger, log = make_spy_logger () in
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:3 ~logger ~name:"threshold-test" () in
  Mcp_resilience.circuit_record_failure cb;
  Mcp_resilience.circuit_record_failure cb;
  Mcp_resilience.circuit_record_failure cb;
  (match cb.state with
   | Mcp_resilience.Open -> ()
   | _ -> fail "expected Open at threshold");
  assert_log_contains log "opened after 3 failures"

let test_failure_in_halfopen_reopens () =
  Eio_main.run @@ fun _env ->
  let logger, log = make_spy_logger () in
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:1 ~timeout_ms:0 ~logger ~name:"reopen-test" () in
  (* Force Open -> HalfOpen *)
  Mcp_resilience.circuit_record_failure cb;
  let _ = Mcp_resilience.circuit_allows cb in
  (match cb.state with Mcp_resilience.HalfOpen -> () | _ -> fail "expected HalfOpen");
  (* Failure during HalfOpen => back to Open *)
  Mcp_resilience.circuit_record_failure cb;
  (match cb.state with
   | Mcp_resilience.Open -> ()
   | _ -> fail "expected Open after HalfOpen failure");
  check bool "probe cleared" false cb.probe_in_progress;
  check int "success_count reset" 0 cb.success_count;
  assert_log_contains log "reopened during probe"

let test_failure_in_open_is_noop () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:1 ~timeout_ms:999999 ~name:"t" () in
  Mcp_resilience.circuit_record_failure cb;
  (match cb.state with Mcp_resilience.Open -> () | _ -> fail "expected Open");
  let fc = cb.failure_count in
  (* Another failure in Open does not change count *)
  Mcp_resilience.circuit_record_failure cb;
  check int "failure_count unchanged in Open" fc cb.failure_count

(* ============================================ *)
(* Full lifecycle: Closed -> Open -> HalfOpen -> Closed *)
(* ============================================ *)

let test_full_lifecycle () =
  Eio_main.run @@ fun _env ->
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:2 ~success_threshold:1 ~timeout_ms:0 ~name:"lifecycle" () in
  (* Closed -> Open *)
  Mcp_resilience.circuit_record_failure cb;
  Mcp_resilience.circuit_record_failure cb;
  (match cb.state with Mcp_resilience.Open -> () | _ -> fail "should be Open");
  (* Open -> HalfOpen via circuit_allows (timeout=0) *)
  let allowed = Mcp_resilience.circuit_allows cb in
  check bool "allowed for probe" true allowed;
  (match cb.state with Mcp_resilience.HalfOpen -> () | _ -> fail "should be HalfOpen");
  (* HalfOpen -> Closed via success *)
  Mcp_resilience.circuit_record_success cb;
  (match cb.state with Mcp_resilience.Closed -> () | _ -> fail "should be Closed again")

(* ============================================ *)
(* calculate_delay: backoff logic                *)
(* ============================================ *)

let test_calculate_delay_no_jitter () =
  (* No Eio needed — but calculate_delay is a pure function;
     however the module uses Random for jitter, so we test no-jitter path *)
  let policy = { Mcp_resilience.default_policy with
    initial_delay_ms = 100;
    max_delay_ms = 10000;
    backoff_multiplier = 2.0;
    jitter = false;
  } in
  (* attempt 1: 100 * 2^0 = 100 *)
  let d1 = Mcp_resilience.calculate_delay policy 1 in
  check (float 0.001) "attempt 1" 100.0 d1;
  (* attempt 2: 100 * 2^1 = 200 *)
  let d2 = Mcp_resilience.calculate_delay policy 2 in
  check (float 0.001) "attempt 2" 200.0 d2;
  (* attempt 3: 100 * 2^2 = 400 *)
  let d3 = Mcp_resilience.calculate_delay policy 3 in
  check (float 0.001) "attempt 3" 400.0 d3;
  (* attempt 7: 100 * 2^6 = 6400 *)
  let d7 = Mcp_resilience.calculate_delay policy 7 in
  check (float 0.001) "attempt 7" 6400.0 d7

let test_calculate_delay_cap () =
  let policy = { Mcp_resilience.default_policy with
    initial_delay_ms = 1000;
    max_delay_ms = 5000;
    backoff_multiplier = 3.0;
    jitter = false;
  } in
  (* attempt 1: 1000 * 3^0 = 1000 *)
  let d1 = Mcp_resilience.calculate_delay policy 1 in
  check (float 0.001) "attempt 1 under cap" 1000.0 d1;
  (* attempt 2: 1000 * 3^1 = 3000 *)
  let d2 = Mcp_resilience.calculate_delay policy 2 in
  check (float 0.001) "attempt 2 under cap" 3000.0 d2;
  (* attempt 3: 1000 * 3^2 = 9000, capped to 5000 *)
  let d3 = Mcp_resilience.calculate_delay policy 3 in
  check (float 0.001) "attempt 3 capped" 5000.0 d3;
  (* attempt 10: huge exponent, still capped *)
  let d10 = Mcp_resilience.calculate_delay policy 10 in
  check (float 0.001) "attempt 10 capped" 5000.0 d10

let test_calculate_delay_with_jitter () =
  let policy = { Mcp_resilience.default_policy with
    initial_delay_ms = 1000;
    max_delay_ms = 100000;
    backoff_multiplier = 1.0;
    jitter = true;
  } in
  (* With backoff_multiplier=1.0, base is always 1000.
     Jitter multiplies by [0.75, 1.25), so result is in [750, 1250) *)
  let results = List.init 20 (fun _ -> Mcp_resilience.calculate_delay policy 1) in
  List.iteri (fun i d ->
    if d < 750.0 || d >= 1250.0 then
      fail (Printf.sprintf "jitter sample %d out of range: %f" i d)
  ) results

(* ============================================ *)
(* with_retry_eio: success on first attempt      *)
(* ============================================ *)

let test_retry_immediate_success () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let result = Mcp_resilience.with_retry_eio
    ~clock
    ~op_name:"test-op"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () ->
      incr call_count;
      Mcp_resilience.Ok 42)
  in
  check int "called once" 1 !call_count;
  (match result with
   | Mcp_resilience.Ok v -> check int "value" 42 v
   | _ -> fail "expected Ok")

(* ============================================ *)
(* with_retry_eio: retries then succeeds         *)
(* ============================================ *)

let test_retry_succeeds_after_retries () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 5;
    initial_delay_ms = 1;  (* minimal delay for fast tests *)
    max_delay_ms = 2;
    jitter = false;
  } in
  let result = Mcp_resilience.with_retry_eio
    ~clock ~policy
    ~op_name:"test-op"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () ->
      incr call_count;
      if !call_count < 3 then
        Mcp_resilience.Error "transient"
      else
        Mcp_resilience.Ok "done")
  in
  check int "called 3 times" 3 !call_count;
  (match result with
   | Mcp_resilience.Ok v -> check string "value" "done" v
   | _ -> fail "expected Ok after retries")

(* ============================================ *)
(* with_retry_eio: exhausts all attempts         *)
(* ============================================ *)

let test_retry_exhausts_attempts () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 3;
    initial_delay_ms = 1;
    max_delay_ms = 2;
    jitter = false;
  } in
  let result = Mcp_resilience.with_retry_eio
    ~clock ~policy
    ~op_name:"test-op"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () ->
      incr call_count;
      Mcp_resilience.Error "always fails")
  in
  check int "called max_attempts times" 3 !call_count;
  (match result with
   | Mcp_resilience.Error msg ->
     check string "error message" "Retryable error" msg
   | _ -> fail "expected Error after exhaustion")

(* ============================================ *)
(* with_retry_eio: classify returns Fail         *)
(* ============================================ *)

let test_retry_classify_fail_stops_immediately () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 5;
    initial_delay_ms = 1;
    max_delay_ms = 2;
    jitter = false;
  } in
  let result = Mcp_resilience.with_retry_eio
    ~clock ~policy
    ~op_name:"test-op"
    ~classify:(fun e -> Mcp_resilience.Fail (Printf.sprintf "fatal: %s" e))
    (fun () ->
      incr call_count;
      Mcp_resilience.Error "bad request")
  in
  check int "called once (no retry)" 1 !call_count;
  (match result with
   | Mcp_resilience.Error msg ->
     check string "error" "fatal: bad request" msg
   | _ -> fail "expected Error")

(* ============================================ *)
(* with_retry_eio: circuit breaker integration   *)
(* ============================================ *)

let test_retry_with_circuit_breaker_open () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let logger, log = make_spy_logger () in
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:1 ~timeout_ms:999999 ~name:"retry-cb" () in
  (* Force open *)
  Mcp_resilience.circuit_record_failure cb;
  let call_count = ref 0 in
  let result = Mcp_resilience.with_retry_eio
    ~clock ~logger
    ~circuit_breaker:(Some cb)
    ~op_name:"blocked-op"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () ->
      incr call_count;
      Mcp_resilience.Ok "never reached")
  in
  check int "never called" 0 !call_count;
  (match result with
   | Mcp_resilience.CircuitOpen -> ()
   | _ -> fail "expected CircuitOpen");
  assert_log_contains log "circuit breaker OPEN"

let test_retry_success_records_to_circuit_breaker () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:5 ~name:"success-cb" () in
  (* Add some failures first *)
  Mcp_resilience.circuit_record_failure cb;
  Mcp_resilience.circuit_record_failure cb;
  check int "failures before" 2 cb.failure_count;
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 1;
    initial_delay_ms = 1;
    jitter = false;
  } in
  let _result = Mcp_resilience.with_retry_eio
    ~clock ~policy
    ~circuit_breaker:(Some cb)
    ~op_name:"test"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () -> Mcp_resilience.Ok "ok")
  in
  (* circuit_record_success in Closed resets failure_count *)
  check int "failures reset" 0 cb.failure_count

let test_retry_failure_records_to_circuit_breaker () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:10 ~name:"fail-cb" () in
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 3;
    initial_delay_ms = 1;
    max_delay_ms = 2;
    jitter = false;
  } in
  let _result = Mcp_resilience.with_retry_eio
    ~clock ~policy
    ~circuit_breaker:(Some cb)
    ~op_name:"test"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () -> Mcp_resilience.Error "fail")
  in
  (* Each retry attempt records a failure => 3 failures *)
  check int "failures recorded" 3 cb.failure_count

let test_retry_classify_fail_records_failure () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let cb = Mcp_resilience.create_circuit_breaker
    ~failure_threshold:10 ~name:"fail-cb2" () in
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 5;
    initial_delay_ms = 1;
    jitter = false;
  } in
  let _result = Mcp_resilience.with_retry_eio
    ~clock ~policy
    ~circuit_breaker:(Some cb)
    ~op_name:"test"
    ~classify:(fun _ -> Mcp_resilience.Fail "fatal")
    (fun () -> Mcp_resilience.Error "err")
  in
  (* Fail classification records exactly 1 failure (no retry) *)
  check int "one failure recorded" 1 cb.failure_count

(* ============================================ *)
(* with_retry_eio: passthrough CircuitOpen/TimedOut *)
(* ============================================ *)

let test_retry_passthrough_circuit_open () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let result = Mcp_resilience.with_retry_eio
    ~clock
    ~op_name:"test"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () -> Mcp_resilience.CircuitOpen)
  in
  (match result with
   | Mcp_resilience.CircuitOpen -> ()
   | _ -> fail "expected CircuitOpen passthrough")

let test_retry_passthrough_timed_out () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let result = Mcp_resilience.with_retry_eio
    ~clock
    ~op_name:"test"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () -> Mcp_resilience.TimedOut)
  in
  (match result with
   | Mcp_resilience.TimedOut -> ()
   | _ -> fail "expected TimedOut passthrough")

(* ============================================ *)
(* with_retry_eio: zero/one attempt edge cases   *)
(* ============================================ *)

let test_retry_zero_attempts () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 0;
    initial_delay_ms = 1;
    jitter = false;
  } in
  let result = Mcp_resilience.with_retry_eio
    ~clock ~policy
    ~op_name:"test"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () ->
      incr call_count;
      Mcp_resilience.Error "fail")
  in
  (* n=1 > max_attempts=0, so immediately returns Error *)
  check int "never called" 0 !call_count;
  (match result with
   | Mcp_resilience.Error msg ->
     check string "default error" "Max attempts reached" msg
   | _ -> fail "expected Error")

let test_retry_single_attempt_fail () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 1;
    initial_delay_ms = 1;
    jitter = false;
  } in
  let result = Mcp_resilience.with_retry_eio
    ~clock ~policy
    ~op_name:"test"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () ->
      incr call_count;
      Mcp_resilience.Error "fail")
  in
  check int "called once" 1 !call_count;
  (match result with
   | Mcp_resilience.Error msg ->
     check string "error" "Retryable error" msg
   | _ -> fail "expected Error")

(* ============================================ *)
(* with_retry_eio: no circuit breaker (None)     *)
(* ============================================ *)

let test_retry_no_circuit_breaker () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 2;
    initial_delay_ms = 1;
    max_delay_ms = 2;
    jitter = false;
  } in
  let result = Mcp_resilience.with_retry_eio
    ~clock ~policy
    ~circuit_breaker:None
    ~op_name:"no-cb"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () ->
      incr call_count;
      if !call_count < 2 then Mcp_resilience.Error "transient"
      else Mcp_resilience.Ok "recovered")
  in
  check int "called twice" 2 !call_count;
  (match result with
   | Mcp_resilience.Ok v -> check string "value" "recovered" v
   | _ -> fail "expected Ok")

(* ============================================ *)
(* with_retry_eio: logger is called              *)
(* ============================================ *)

let test_retry_logs_retries () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let logger, log = make_spy_logger () in
  let call_count = ref 0 in
  let policy = { Mcp_resilience.default_policy with
    max_attempts = 3;
    initial_delay_ms = 1;
    max_delay_ms = 2;
    jitter = false;
  } in
  let _result = Mcp_resilience.with_retry_eio
    ~clock ~policy ~logger
    ~op_name:"logged-op"
    ~classify:(fun _ -> Mcp_resilience.Retry)
    (fun () ->
      incr call_count;
      if !call_count < 3 then Mcp_resilience.Error "err"
      else Mcp_resilience.Ok "ok")
  in
  assert_log_contains log "retrying"

(* ============================================ *)
(* with_timeout_eio: fast operation completes    *)
(* ============================================ *)

let test_timeout_success () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let result = Mcp_resilience.with_timeout_eio
    ~clock ~timeout_ms:5000
    (fun () -> 42)
  in
  (match result with
   | Mcp_resilience.Ok v -> check int "value" 42 v
   | _ -> fail "expected Ok")

(* ============================================ *)
(* with_timeout_eio: slow operation times out    *)
(* ============================================ *)

let test_timeout_expired () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let result = Mcp_resilience.with_timeout_eio
    ~clock ~timeout_ms:10
    (fun () ->
      Eio.Time.sleep clock 5.0;
      "never")
  in
  (match result with
   | Mcp_resilience.Error msg -> check string "timeout msg" "Timeout" msg
   | _ -> fail "expected timeout error")

(* ============================================ *)
(* null_logger coverage                          *)
(* ============================================ *)

let test_null_logger () =
  (* null_logger simply discards — should not raise *)
  Mcp_resilience.null_logger Mcp_resilience.Debug "test";
  Mcp_resilience.null_logger Mcp_resilience.Info "test";
  Mcp_resilience.null_logger Mcp_resilience.Warn "test";
  Mcp_resilience.null_logger Mcp_resilience.Err "test"

(* ============================================ *)
(* default_policy coverage                       *)
(* ============================================ *)

let test_default_policy () =
  let p = Mcp_resilience.default_policy in
  check int "max_attempts" 3 p.max_attempts;
  check int "initial_delay_ms" 100 p.initial_delay_ms;
  check int "max_delay_ms" 10000 p.max_delay_ms;
  check (float 0.001) "backoff_multiplier" 2.0 p.backoff_multiplier;
  check bool "jitter" true p.jitter

(* ============================================ *)
(* Test runner                                   *)
(* ============================================ *)

let () =
  run "mcp_resilience"
    [ ( "circuit_breaker_create"
      , [ test_case "default parameters" `Quick test_create_defaults
        ; test_case "custom parameters" `Quick test_create_custom_params
        ] )
    ; ( "circuit_allows"
      , [ test_case "closed allows" `Quick test_closed_allows
        ; test_case "open blocks" `Quick test_open_blocks
        ; test_case "open -> halfopen after timeout" `Quick
            test_open_transitions_to_halfopen_after_timeout
        ; test_case "halfopen blocks during probe" `Quick
            test_halfopen_blocks_while_probe_in_progress
        ; test_case "halfopen allows when no probe" `Quick
            test_halfopen_allows_when_no_probe
        ] )
    ; ( "circuit_record_success"
      , [ test_case "closed: resets failure_count" `Quick
            test_success_in_closed_resets_failure_count
        ; test_case "halfopen: increments and closes" `Quick
            test_success_in_halfopen_increments_and_closes
        ; test_case "open: noop" `Quick test_success_in_open_is_noop
        ] )
    ; ( "circuit_record_failure"
      , [ test_case "closed: increments" `Quick test_failure_in_closed_increments
        ; test_case "closed: opens at threshold" `Quick
            test_failure_in_closed_opens_at_threshold
        ; test_case "halfopen: reopens" `Quick test_failure_in_halfopen_reopens
        ; test_case "open: noop" `Quick test_failure_in_open_is_noop
        ] )
    ; ( "lifecycle"
      , [ test_case "full closed->open->halfopen->closed" `Quick
            test_full_lifecycle
        ] )
    ; ( "calculate_delay"
      , [ test_case "exponential no jitter" `Quick test_calculate_delay_no_jitter
        ; test_case "capped at max_delay_ms" `Quick test_calculate_delay_cap
        ; test_case "jitter within range" `Quick test_calculate_delay_with_jitter
        ] )
    ; ( "with_retry_eio"
      , [ test_case "immediate success" `Quick test_retry_immediate_success
        ; test_case "succeeds after retries" `Quick test_retry_succeeds_after_retries
        ; test_case "exhausts all attempts" `Quick test_retry_exhausts_attempts
        ; test_case "classify Fail stops immediately" `Quick
            test_retry_classify_fail_stops_immediately
        ; test_case "circuit breaker open rejects" `Quick
            test_retry_with_circuit_breaker_open
        ; test_case "success records to cb" `Quick
            test_retry_success_records_to_circuit_breaker
        ; test_case "failure records to cb" `Quick
            test_retry_failure_records_to_circuit_breaker
        ; test_case "classify Fail records failure" `Quick
            test_retry_classify_fail_records_failure
        ; test_case "passthrough CircuitOpen" `Quick
            test_retry_passthrough_circuit_open
        ; test_case "passthrough TimedOut" `Quick
            test_retry_passthrough_timed_out
        ; test_case "zero attempts" `Quick test_retry_zero_attempts
        ; test_case "single attempt fail" `Quick test_retry_single_attempt_fail
        ; test_case "no circuit breaker (None)" `Quick test_retry_no_circuit_breaker
        ; test_case "logs retries" `Quick test_retry_logs_retries
        ] )
    ; ( "with_timeout_eio"
      , [ test_case "fast operation succeeds" `Quick test_timeout_success
        ; test_case "slow operation times out" `Quick test_timeout_expired
        ] )
    ; ( "misc"
      , [ test_case "null_logger is noop" `Quick test_null_logger
        ; test_case "default_policy values" `Quick test_default_policy
        ] )
    ]
