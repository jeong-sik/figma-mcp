(** Figma Plugin Bridge - in-memory channel/command broker. *)

type command = {
  id: string;
  name: string;
  payload: Yojson.Safe.t;
  created_at: float;
}

type result = {
  id: string;
  ok: bool;
  payload: Yojson.Safe.t;
  received_at: float;
}

type waiter = {
  id: int;
  notify: unit -> unit;
}

type session = {
  mutable last_seen: float;
  commands: command Queue.t;
  results: (string, result) Hashtbl.t;
  mutable waiters: waiter list;
}

let channels : (string, session) Hashtbl.t = Hashtbl.create 32
let lock = Mutex.create ()
let default_channel : string option ref = ref None
let next_waiter_id = ref 0

let () = Random.self_init ()

let now () = Unix.gettimeofday ()

let with_lock f =
  Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) f

let new_id prefix =
  Printf.sprintf "%s-%d-%d" prefix (int_of_float (now () *. 1000.0)) (Random.bits ())

let ensure_session channel_id =
  match Hashtbl.find_opt channels channel_id with
  | Some session -> session
  | None ->
      let session = {
        last_seen = now ();
        commands = Queue.create ();
        results = Hashtbl.create 32;
        waiters = [];
      } in
      Hashtbl.add channels channel_id session;
      session

let register_channel ?channel_id () =
  with_lock (fun () ->
    let id = match channel_id with
      | Some id -> id
      | None -> new_id "ch"
    in
    ignore (ensure_session id);
    default_channel := Some id;
    id)

let set_default_channel channel_id =
  with_lock (fun () -> default_channel := Some channel_id)

let get_default_channel () = !default_channel

let list_channels () =
  with_lock (fun () ->
    Hashtbl.fold (fun id _ acc -> id :: acc) channels []
    |> List.sort String.compare)

let enqueue_command ~channel_id ~name ~payload =
  let pending_waiters = ref [] in
  let id = with_lock (fun () ->
    let session = ensure_session channel_id in
    session.last_seen <- now ();
    let id = new_id "cmd" in
    Queue.add { id; name; payload; created_at = now () } session.commands;
    let waiters = session.waiters in
    session.waiters <- [];
    pending_waiters := waiters;
    id)
  in
  List.iter (fun waiter ->
    try waiter.notify () with _ -> ()
  ) !pending_waiters;
  id

let register_waiter ~channel_id ~notify =
  with_lock (fun () ->
    let session = ensure_session channel_id in
    incr next_waiter_id;
    let id = !next_waiter_id in
    session.waiters <- { id; notify } :: session.waiters;
    id)

let unregister_waiter ~channel_id ~waiter_id =
  with_lock (fun () ->
    match Hashtbl.find_opt channels channel_id with
    | None -> ()
    | Some session ->
        session.waiters <- List.filter (fun waiter -> waiter.id <> waiter_id) session.waiters)

let poll_commands ~channel_id ~max =
  with_lock (fun () ->
    let session = ensure_session channel_id in
    session.last_seen <- now ();
    let rec take acc remaining =
      if remaining <= 0 || Queue.is_empty session.commands then
        List.rev acc
      else
        let cmd = Queue.take session.commands in
        take (cmd :: acc) (remaining - 1)
    in
    take [] max)

let store_result ~channel_id ~command_id ~ok ~payload =
  with_lock (fun () ->
    let session = ensure_session channel_id in
    session.last_seen <- now ();
    Hashtbl.replace session.results command_id
      { id = command_id; ok; payload; received_at = now () })

let take_result ~channel_id ~command_id =
  with_lock (fun () ->
    match Hashtbl.find_opt channels channel_id with
    | None -> None
    | Some session ->
        session.last_seen <- now ();
        match Hashtbl.find_opt session.results command_id with
        | None -> None
        | Some result ->
            Hashtbl.remove session.results command_id;
            Some result)

exception PluginPending

let wait_for_result ~channel_id ~command_id ~timeout_ms =
  (* Use Resilience module for structured polling/retry *)
  let poll_policy = {
    Resilience.default_policy with
    max_attempts = max 1 (timeout_ms / 200); (* 200ms poll interval *)
    initial_delay_ms = 200;
    max_delay_ms = 200;
    jitter = false;
  } in
  let op () =
    match take_result ~channel_id ~command_id with
    | Some r -> r
    | None -> raise PluginPending
  in
  match Resilience.with_retry_sync ~policy:poll_policy ~op_name:"plugin_poll" op with
  | Success r -> Some r
  | Exhausted _ | CircuitOpen | TimedOut _ -> None

let cleanup_inactive ~ttl_seconds =
  with_lock (fun () ->
    let cutoff = now () -. ttl_seconds in
    Hashtbl.iter (fun channel_id session ->
      if session.last_seen < cutoff then
        Hashtbl.remove channels channel_id
    ) channels)