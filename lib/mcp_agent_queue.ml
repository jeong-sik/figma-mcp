(** Agent Queue for MCP-style async codegen.

    Provides a priority-based task queue with claim/heartbeat/abandon semantics
    for asynchronous code generation workers. Thread-safe via [Eio.Mutex]. *)

open Printf

type agent_status =
  | Pending
  | Claimed
  | Completed
  | Failed

let agent_status_to_string = function
  | Pending -> "pending"
  | Claimed -> "claimed"
  | Completed -> "completed"
  | Failed -> "failed"

type agent_request = {
  id: string;
  request_secret: string;
  node: Yojson.Safe.t;
  platform: string;
  prompt: string;
  context_digest: string;
  priority: int;
  created_at: float;
  mutable status: agent_status;
  mutable claimed_by: string option;
  mutable claim_token: string option;
  mutable claimed_at: float option;
  mutable last_heartbeat: float option;
  mutable attempts: int;
  mutable result: string option;
  mutable error: string option;
  mutable drifted: bool;
}

let agent_queue : (string, agent_request) Hashtbl.t = Hashtbl.create 16
let agent_queue_mutex = Eio.Mutex.create ()

let now () = Unix.gettimeofday ()

let parse_positive_int value =
  try
    let v = int_of_string value in
    if v > 0 then Some v else None
  with _ -> None

let env_int ~name ~default =
  match Sys.getenv_opt name with
  | Some v -> (match parse_positive_int v with Some n -> n | None -> default)
  | None -> default

let agent_claim_ttl_sec = env_int ~name:"FIGMA_MCP_AGENT_CLAIM_TTL_SEC" ~default:120
let agent_heartbeat_ttl_sec = env_int ~name:"FIGMA_MCP_AGENT_HEARTBEAT_TTL_SEC" ~default:45
let agent_max_age_sec = env_int ~name:"FIGMA_MCP_AGENT_MAX_AGE_SEC" ~default:900
let agent_max_attempts = env_int ~name:"FIGMA_MCP_AGENT_MAX_ATTEMPTS" ~default:3

let hex_of_bytes (b : bytes) =
  let hex = "0123456789abcdef" in
  let len = Bytes.length b in
  let out = Bytes.create (len * 2) in
  for i = 0 to (len - 1) do
    let v = Char.code (Bytes.get b i) in
    Bytes.set out (i * 2) hex.[v lsr 4];
    Bytes.set out (i * 2 + 1) hex.[v land 0x0f];
  done;
  Bytes.to_string out

let random_bytes len =
  let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () ->
      try Unix.close fd
      with exn -> eprintf "[mcp_protocol] Warning: /dev/urandom fd close failed: %s\n%!" (Printexc.to_string exn))
    (fun () ->
      let buf = Bytes.create len in
      let rec loop off =
        if off >= len then ()
        else
          let n = Unix.read fd buf off (len - off) in
          if n = 0 then failwith "Unexpected EOF reading /dev/urandom"
          else loop (off + n)
      in
      loop 0;
      buf)

let random_hex len = hex_of_bytes (random_bytes len)

let new_request_id () = "req_" ^ random_hex 16
let new_request_secret () = random_hex 16
let new_claim_token () = random_hex 16

let agent_add_request ~priority ~context_digest node platform prompt =
  let id = new_request_id () in
  let request_secret = new_request_secret () in
  let req = {
    id;
    request_secret;
    node;
    platform;
    prompt;
    context_digest;
    priority;
    created_at = now ();
    status = Pending;
    claimed_by = None;
    claim_token = None;
    claimed_at = None;
    last_heartbeat = None;
    attempts = 0;
    result = None;
    error = None;
    drifted = false;
  } in
  Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
    Hashtbl.add agent_queue id req;
    (id, request_secret))

let agent_get_pending () =
  Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
    let pending = Hashtbl.fold (fun _ req acc ->
      if req.status = Pending then req :: acc else acc
    ) agent_queue [] in
    List.sort (fun a b ->
      let by_priority = compare b.priority a.priority in
      if by_priority <> 0 then by_priority else compare a.created_at b.created_at
    ) pending)

let agent_claim ~worker_id =
  Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
    let pending = Hashtbl.fold (fun _ req acc ->
      if req.status = Pending then req :: acc else acc
    ) agent_queue [] in
    let sorted = List.sort (fun a b ->
      let by_priority = compare b.priority a.priority in
      if by_priority <> 0 then by_priority else compare a.created_at b.created_at
    ) pending in
    let rec pick = function
      | [] -> None
      | req :: rest ->
          if req.attempts >= agent_max_attempts then begin
            req.status <- Failed;
            req.error <- Some "max_attempts_exceeded";
            pick rest
          end else begin
            req.status <- Claimed;
            req.claimed_by <- Some worker_id;
            req.claim_token <- Some (new_claim_token ());
            req.claimed_at <- Some (now ());
            req.last_heartbeat <- Some (now ());
            req.attempts <- req.attempts + 1;
            Some req
          end
    in
    pick sorted)

let agent_heartbeat ~worker_id ~claim_token req_id =
  Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
    match Hashtbl.find_opt agent_queue req_id with
    | Some req when req.status = Claimed && req.claimed_by = Some worker_id -> (
        match req.claim_token with
        | Some t when t = claim_token ->
        req.last_heartbeat <- Some (now ());
        Ok ()
        | _ -> Error "invalid_claim_token")
    | Some _ -> Error "not_claimed_by_worker"
    | None -> Error "not_found")

let agent_abandon ~worker_id ~claim_token ~reason req_id =
  Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
    match Hashtbl.find_opt agent_queue req_id with
    | Some req when req.status = Claimed && req.claimed_by = Some worker_id -> (
        match req.claim_token with
        | Some t when t = claim_token ->
        req.status <- Pending;
        req.claimed_by <- None;
        req.claim_token <- None;
        req.claimed_at <- None;
        req.last_heartbeat <- None;
        req.error <- Some reason;
        Ok ()
        | _ -> Error "invalid_claim_token")
    | Some _ -> Error "not_claimed_by_worker"
    | None -> Error "not_found")

let agent_submit_result ?worker_id ?context_digest ~claim_token req_id code =
  Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
    match Hashtbl.find_opt agent_queue req_id with
    | None -> Error "not_found"
    | Some req ->
        let drifted = ref req.drifted in
        let error = ref None in
        (match req.status with
         | Claimed -> (
             match req.claim_token with
             | Some t when t = claim_token -> ()
             | _ ->
                 drifted := true;
                 error := Some "invalid_claim_token")
         | _ ->
             drifted := true;
             if !error = None then error := Some "not_claimed");
        if !error = None then
          (match req.status, worker_id, req.claimed_by with
           | Claimed, Some w, Some c when w <> c ->
               drifted := true;
               error := Some "worker_mismatch"
           | Claimed, None, Some _ ->
               drifted := true
           | Pending, _, _ ->
               drifted := true
           | _ -> ());
        if !error = None then
          (match context_digest with
           | Some d when d <> "" && d <> req.context_digest ->
               drifted := true;
               error := Some "context_drift"
           | _ -> ());
        req.drifted <- !drifted;
        (match !error with
         | Some msg ->
             req.error <- Some msg;
             Error msg
         | None ->
             req.result <- Some code;
             req.status <- Completed;
             req.claim_token <- None;
             Ok ()))

let agent_get_result req_id =
  Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
    Hashtbl.find_opt agent_queue req_id)

let agent_cleanup_old () =
  let t = now () in
  let claim_ttl = float_of_int agent_claim_ttl_sec in
  let heartbeat_ttl = float_of_int agent_heartbeat_ttl_sec in
  let max_age = float_of_int agent_max_age_sec in
  Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
    let to_remove = ref [] in
    Hashtbl.iter (fun id req ->
      (match req.status with
       | Claimed ->
           let last = Option.value ~default:(Option.value ~default:req.created_at req.claimed_at) req.last_heartbeat in
           if (t -. last) > heartbeat_ttl || (t -. Option.value ~default:req.created_at req.claimed_at) > claim_ttl then begin
             req.status <- Pending;
             req.claimed_by <- None;
             req.claim_token <- None;
             req.claimed_at <- None;
             req.last_heartbeat <- None;
             req.error <- Some "claim_timeout";
           end
       | _ -> ());
      if (t -. req.created_at) > max_age then to_remove := id :: !to_remove
    ) agent_queue;
    List.iter (Hashtbl.remove agent_queue) !to_remove)

type agent_queue_stats = {
  total: int;
  pending: int;
  claimed: int;
  completed: int;
  failed: int;
  drifted: int;
  oldest_pending_sec: float option;
  oldest_claimed_sec: float option;
  claim_ttl_sec: int;
  heartbeat_ttl_sec: int;
  max_age_sec: int;
  max_attempts: int;
}

let agent_queue_stats () =
  let t = now () in
  Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
    let total = ref 0 in
    let pending = ref 0 in
    let claimed = ref 0 in
    let completed = ref 0 in
    let failed = ref 0 in
    let drifted = ref 0 in
    let oldest_pending = ref None in
    let oldest_claimed = ref None in
    Hashtbl.iter (fun _ (req : agent_request) ->
      total := !total + 1;
      if req.drifted then drifted := !drifted + 1;
      (match req.status with
       | Pending ->
           pending := !pending + 1;
           let age = t -. req.created_at in
           (match !oldest_pending with
            | None -> oldest_pending := Some age
            | Some v when age > v -> oldest_pending := Some age
            | _ -> ())
       | Claimed ->
           claimed := !claimed + 1;
           let base = Option.value ~default:req.created_at req.claimed_at in
           let age = t -. base in
           (match !oldest_claimed with
            | None -> oldest_claimed := Some age
            | Some v when age > v -> oldest_claimed := Some age
            | _ -> ())
       | Completed -> completed := !completed + 1
       | Failed -> failed := !failed + 1)
    ) agent_queue;
    {
      total = !total;
      pending = !pending;
      claimed = !claimed;
      completed = !completed;
      failed = !failed;
      drifted = !drifted;
      oldest_pending_sec = !oldest_pending;
      oldest_claimed_sec = !oldest_claimed;
      claim_ttl_sec = agent_claim_ttl_sec;
      heartbeat_ttl_sec = agent_heartbeat_ttl_sec;
      max_age_sec = agent_max_age_sec;
      max_attempts = agent_max_attempts;
    })

let agent_queue_stats_json () =
  let s = agent_queue_stats () in
  `Assoc [
    ("total", `Int s.total);
    ("pending", `Int s.pending);
    ("claimed", `Int s.claimed);
    ("completed", `Int s.completed);
    ("failed", `Int s.failed);
    ("drifted", `Int s.drifted);
    ("oldest_pending_sec", (match s.oldest_pending_sec with Some v -> `Float v | None -> `Null));
    ("oldest_claimed_sec", (match s.oldest_claimed_sec with Some v -> `Float v | None -> `Null));
    ("claim_ttl_sec", `Int s.claim_ttl_sec);
    ("heartbeat_ttl_sec", `Int s.heartbeat_ttl_sec);
    ("max_age_sec", `Int s.max_age_sec);
    ("max_attempts", `Int s.max_attempts);
  ]

(** Serialize an [agent_request] to JSON for API responses.
    Control field inclusion via [~include_node], [~include_prompt],
    and [~include_claim_token]. *)
let agent_request_json ?(include_claim_token=false) ~include_node ~include_prompt req =
  let base = [
    ("id", `String req.id);
    ("platform", `String req.platform);
    ("priority", `Int req.priority);
    ("context_digest", `String req.context_digest);
    ("status", `String (agent_status_to_string req.status));
    ("attempts", `Int req.attempts);
    ("claimed_by", (match req.claimed_by with Some v -> `String v | None -> `Null));
    ("claim_token", (match include_claim_token, req.claim_token with true, Some v -> `String v | _ -> `Null));
    ("claimed_at", (match req.claimed_at with Some v -> `Float v | None -> `Null));
    ("last_heartbeat", (match req.last_heartbeat with Some v -> `Float v | None -> `Null));
    ("drifted", `Bool req.drifted);
    ("error", (match req.error with Some v -> `String v | None -> `Null));
    ("age_sec", `Float (Unix.gettimeofday () -. req.created_at));
  ] in
  let with_prompt = if include_prompt then ("prompt", `String req.prompt) :: base else base in
  let with_node = if include_node then ("node", req.node) :: with_prompt else with_prompt in
  `Assoc with_node
