(** Resilience Module - Circuit Breaker, Retry, and Timeout patterns for MCP servers *)

(* ============================================ *)
(* Types & Logger Interface (DI)                *)
(* ============================================ *)

type log_level = Debug | Info | Warn | Err

(** Logger function type - injected by caller *)
type logger = log_level -> string -> unit

(** No-op logger *)
let null_logger _ _ = ()

(* ============================================ *)
(* Retry Policy Configuration                   *)
(* ============================================ *)

type retry_policy = {
  max_attempts: int;
  initial_delay_ms: int;
  max_delay_ms: int;
  backoff_multiplier: float;
  jitter: bool;
}

let default_policy = {
  max_attempts = 3;
  initial_delay_ms = 100;
  max_delay_ms = 10000;
  backoff_multiplier = 2.0;
  jitter = true;
}

(* ============================================ *)
(* Circuit Breaker                              *)
(* ============================================ *)

type circuit_state =
  | Closed      (* Normal operation *)
  | Open        (* Failing, reject requests *)
  | HalfOpen    (* Testing if service recovered *)

type circuit_breaker = {
  name: string;
  failure_threshold: int;
  success_threshold: int;
  timeout_ms: int;
  mutable state: circuit_state;
  mutable failure_count: int;
  mutable success_count: int;
  mutable last_failure_time: float;
  mutable probe_in_progress: bool;
  mutex: Eio.Mutex.t;
  logger: logger;
}

let create_circuit_breaker
    ?(failure_threshold=5)
    ?(success_threshold=2)
    ?(timeout_ms=30000)
    ?(logger=null_logger)
    ~name
    () =
  {
    name;
    failure_threshold;
    success_threshold;
    timeout_ms;
    state = Closed;
    failure_count = 0;
    success_count = 0;
    last_failure_time = 0.0;
    probe_in_progress = false;
    mutex = Eio.Mutex.create ();
    logger;
  }

let circuit_allows cb =
  Eio.Mutex.use_rw ~protect:true cb.mutex (fun () ->
    match cb.state with
    | Closed -> true
    | Open ->
        let now = Unix.gettimeofday () in
        let elapsed_ms = (now -. cb.last_failure_time) *. 1000.0 in
        if elapsed_ms >= float_of_int cb.timeout_ms then begin
          cb.state <- HalfOpen;
          cb.success_count <- 0;
          cb.probe_in_progress <- true;
          cb.logger Debug (Printf.sprintf "Circuit '%s' entering HalfOpen state" cb.name);
          true
        end else
          false
    | HalfOpen ->
        if cb.probe_in_progress then false
        else begin
          cb.probe_in_progress <- true;
          true
        end
  )

let circuit_record_success cb =
  Eio.Mutex.use_rw ~protect:true cb.mutex (fun () ->
    match cb.state with
    | Closed ->
        cb.failure_count <- 0
    | HalfOpen ->
        cb.probe_in_progress <- false;
        cb.success_count <- cb.success_count + 1;
        if cb.success_count >= cb.success_threshold then begin
          cb.state <- Closed;
          cb.failure_count <- 0;
          cb.success_count <- 0;
          cb.logger Info (Printf.sprintf "Circuit '%s' closed (recovered)" cb.name)
        end
    | Open -> ()
  )

let circuit_record_failure cb =
  Eio.Mutex.use_rw ~protect:true cb.mutex (fun () ->
    cb.last_failure_time <- Unix.gettimeofday ();
    match cb.state with
    | Closed ->
        cb.failure_count <- cb.failure_count + 1;
        if cb.failure_count >= cb.failure_threshold then begin
          cb.state <- Open;
          cb.logger Warn (Printf.sprintf "Circuit '%s' opened after %d failures" cb.name cb.failure_count)
        end
    | HalfOpen ->
        cb.probe_in_progress <- false;
        cb.state <- Open;
        cb.success_count <- 0;
        cb.logger Warn (Printf.sprintf "Circuit '%s' reopened during probe" cb.name)
    | Open -> ()
  )

(* ============================================ *)
(* Retry Logic (Pure & Eio)                     *)
(* ============================================ *)

type 'a retry_result =
  | Ok of 'a
  | Error of string
  | CircuitOpen
  | TimedOut

(** Retry action classification *)
type retry_action =
  | Retry
  | Fail of string

let calculate_delay policy attempt =
  let base_delay = float_of_int policy.initial_delay_ms in
  let multiplied = base_delay *. (policy.backoff_multiplier ** float_of_int (attempt - 1)) in
  let capped = min multiplied (float_of_int policy.max_delay_ms) in
  if policy.jitter then
    let jitter_factor = 0.75 +. (Random.float 0.5) in
    capped *. jitter_factor
  else
    capped

(** Eio-based retry with structured error classification.
    @param classify Function that maps domain errors ('e) to retry actions *)
let with_retry_eio
    ~clock
    ?(policy=default_policy)
    ?(circuit_breaker=None)
    ?(logger=null_logger)
    ~op_name
    ~classify
    f =
  let rec attempt n last_error =
    let cb_allows = match circuit_breaker with
      | None -> true
      | Some cb -> circuit_allows cb
    in
    if not cb_allows then begin
      logger Warn (Printf.sprintf "%s: circuit breaker OPEN, rejecting" op_name);
      CircuitOpen
    end
    else if n > policy.max_attempts then begin
      Error (match last_error with Some e -> e | None -> "Max attempts reached")
    end
    else begin
      if n > 1 then begin
        let delay_ms = calculate_delay policy (n - 1) in
        logger Debug (Printf.sprintf "%s: retrying in %.0fms (attempt %d)" op_name delay_ms n);
        Eio.Time.sleep clock (delay_ms /. 1000.0)
      end;

      match f () with
      | Ok v ->
          (match circuit_breaker with Some cb -> circuit_record_success cb | None -> ());
          Ok v
      | Error e ->
          (match classify e with
          | Fail msg ->
              (match circuit_breaker with Some cb -> circuit_record_failure cb | None -> ());
              Error msg
          | Retry ->
              (match circuit_breaker with Some cb -> circuit_record_failure cb | None -> ());
              attempt (n + 1) (Some (match classify e with Fail m -> m | Retry -> "Retryable error")))
      | CircuitOpen -> CircuitOpen
      | TimedOut -> TimedOut
    end
  in
  attempt 1 None

(** Eio-based timeout wrapper using Fiber.first *)
let with_timeout_eio ~clock ~timeout_ms f =
  let timeout_sec = float_of_int timeout_ms /. 1000.0 in
  let result =
    Eio.Fiber.first
      (fun () ->
         try
           Eio.Time.sleep clock timeout_sec;
           Error "Timeout"
         with Eio.Cancel.Cancelled _ -> Error "Cancelled")
      (fun () -> Ok (f ()))
  in
  match result with
  | Ok res -> Ok res
  | Error _ -> Error "Timeout"
  | _ -> Error "Unknown error"