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
  mutable results_order: string Queue.t;
  mutable waiters: waiter list;
}

let channels : (string, session) Hashtbl.t = Hashtbl.create 32
let lock = Mutex.create ()
let default_channel : string option ref = ref None
let next_waiter_id = ref 0
let last_cleanup_at = ref 0.0

let max_commands = max 1 Figma_config.Plugin.max_commands
let max_results = max 1 Figma_config.Plugin.max_results
let max_waiters = max 1 Figma_config.Plugin.max_waiters
let result_ttl_seconds = max 0.0 Figma_config.Plugin.result_ttl_seconds
let cleanup_interval_seconds = max 1.0 Figma_config.Plugin.cleanup_interval_seconds
let max_payload_bytes = max 0 Figma_config.Plugin.max_payload_bytes

let () = Random.self_init ()

let now () = Unix.gettimeofday ()

let check_payload_limit payload =
  if max_payload_bytes <= 0 then Ok ()
  else
    let size = String.length (Yojson.Safe.to_string payload) in
    if size > max_payload_bytes then Error size else Ok ()

type channel_stats = {
  id: string;
  last_seen: float;
  commands: int;
  results: int;
  waiters: int;
}

let cleanup_inactive_unlocked ~ttl_seconds =
  let cutoff = now () -. ttl_seconds in
  let stale = ref [] in
  Hashtbl.iter (fun channel_id (session:session) ->
    if session.last_seen < cutoff then stale := channel_id :: !stale
  ) channels;
  List.iter (Hashtbl.remove channels) !stale

let compact_results_order (session:session) =
  let q = Queue.create () in
  Queue.iter (fun id ->
    if Hashtbl.mem session.results id then Queue.add id q
  ) session.results_order;
  session.results_order <- q

let cleanup_results_unlocked ~result_ttl_seconds =
  if result_ttl_seconds <= 0.0 then ()
  else
    let cutoff = now () -. result_ttl_seconds in
    Hashtbl.iter (fun _ (session:session) ->
      let stale = ref [] in
      Hashtbl.iter (fun id result ->
        if result.received_at < cutoff then stale := id :: !stale
      ) session.results;
      List.iter (Hashtbl.remove session.results) !stale;
      if Queue.length session.results_order > max_results * 3 then
        compact_results_order session
    ) channels

let maybe_cleanup_unlocked () =
  let current = now () in
  if current -. !last_cleanup_at >= cleanup_interval_seconds then begin
    cleanup_inactive_unlocked ~ttl_seconds:Figma_config.Plugin.ttl_seconds;
    cleanup_results_unlocked ~result_ttl_seconds;
    last_cleanup_at := current
  end

let with_lock f =
  Mutex.lock lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock lock)
    (fun () ->
      maybe_cleanup_unlocked ();
      f ())

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
        results_order = Queue.create ();
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

let list_channel_stats () =
  with_lock (fun () ->
    Hashtbl.fold (fun id (session:session) acc ->
      let stats = {
        id;
        last_seen = session.last_seen;
        commands = Queue.length session.commands;
        results = Hashtbl.length session.results;
        waiters = List.length session.waiters;
      } in
      stats :: acc
    ) channels []
    |> List.sort (fun a b -> String.compare a.id b.id))

let store_result_unlocked (session:session) ~command_id ~ok ~payload =
  session.last_seen <- now ();
  if not (Hashtbl.mem session.results command_id) then
    Queue.add command_id session.results_order;
  Hashtbl.replace session.results command_id
    { id = command_id; ok; payload; received_at = now () };
  let rec evict () =
    if Hashtbl.length session.results > max_results then
      if Queue.is_empty session.results_order then ()
      else begin
        let id = Queue.take session.results_order in
        if Hashtbl.mem session.results id then Hashtbl.remove session.results id;
        evict ()
      end
  in
  evict ()

let trim_waiters (session:session) =
  let rec split keep n lst =
    if n <= 0 then (List.rev keep, lst)
    else match lst with
      | [] -> (List.rev keep, [])
      | x :: xs -> split (x :: keep) (n - 1) xs
  in
  if List.length session.waiters <= max_waiters then []
  else
    let keep, dropped = split [] max_waiters session.waiters in
    session.waiters <- keep;
    dropped

let enqueue_command ~channel_id ~name ~payload =
  let pending_waiters = ref [] in
  let id = with_lock (fun () ->
    let session = ensure_session channel_id in
    session.last_seen <- now ();
    let id = new_id "cmd" in
    begin
      match check_payload_limit payload with
      | Error size ->
          let error_payload = `Assoc [
            ("error", `String "Command payload too large");
            ("size", `Int size);
            ("limit", `Int max_payload_bytes);
          ] in
          store_result_unlocked session ~command_id:id ~ok:false ~payload:error_payload
      | Ok () ->
          if Queue.length session.commands >= max_commands then begin
            let error_payload = `Assoc [
              ("error", `String "Command queue full");
              ("limit", `Int max_commands);
            ] in
            store_result_unlocked session ~command_id:id ~ok:false ~payload:error_payload;
          end else begin
            Queue.add { id; name; payload; created_at = now () } session.commands;
            let waiters = session.waiters in
            session.waiters <- [];
            pending_waiters := waiters;
          end
    end;
    id)
  in
  List.iter (fun waiter ->
    try waiter.notify () with _ -> ()
  ) !pending_waiters;
  id

let register_waiter ~channel_id ~notify =
  with_lock (fun () ->
    let session = ensure_session channel_id in
    session.last_seen <- now ();
    incr next_waiter_id;
    let id = !next_waiter_id in
    session.waiters <- { id; notify } :: session.waiters;
    let dropped = trim_waiters session in
    List.iter (fun waiter ->
      try waiter.notify () with _ -> ()
    ) dropped;
    id)

let unregister_waiter ~channel_id ~waiter_id =
  with_lock (fun () ->
    match Hashtbl.find_opt channels channel_id with
    | None -> ()
    | Some (session:session) ->
        session.waiters <- List.filter (fun (waiter:waiter) -> waiter.id <> waiter_id) session.waiters)

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
    match check_payload_limit payload with
    | Ok () ->
        store_result_unlocked session ~command_id ~ok ~payload
    | Error size ->
        let error_payload = `Assoc [
          ("error", `String "Result payload too large");
          ("size", `Int size);
          ("limit", `Int max_payload_bytes);
        ] in
        store_result_unlocked session ~command_id ~ok:false ~payload:error_payload)

let take_result ~channel_id ~command_id =
  with_lock (fun () ->
    match Hashtbl.find_opt channels channel_id with
    | None -> None
    | Some (session:session) ->
        session.last_seen <- now ();
        match Hashtbl.find_opt session.results command_id with
        | None -> None
        | Some result ->
            Hashtbl.remove session.results command_id;
            Some result)

let wait_for_result_with_sleep ~sleep ~channel_id ~command_id ~timeout_ms =
  let start_time = now () in
  let timeout_sec = float_of_int timeout_ms /. 1000.0 in
  
  let rec poll attempt =
    match take_result ~channel_id ~command_id with
    | Some result -> Some result
    | None ->
        let elapsed = now () -. start_time in
        if elapsed >= timeout_sec then None
        else
          (* Exponential backoff with cap: 50ms, 100ms, 200ms... capped at 500ms *)
          let backoff = min 0.5 (0.05 *. (2.0 ** float_of_int attempt)) in
          let remaining = timeout_sec -. elapsed in
          let sleep_time = min backoff remaining in
          if sleep_time <= 0.0 then None
          else begin
            sleep sleep_time;
            poll (attempt + 1)
          end
  in
  poll 0

let wait_for_result ~channel_id ~command_id ~timeout_ms =
  wait_for_result_with_sleep
    ~sleep:Unix.sleepf
    ~channel_id
    ~command_id
    ~timeout_ms

let cleanup_inactive ~ttl_seconds =
  with_lock (fun () ->
    cleanup_inactive_unlocked ~ttl_seconds;
    cleanup_results_unlocked ~result_ttl_seconds)
