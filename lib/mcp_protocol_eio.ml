(** MCP Protocol Eio - Pure Eio HTTP/stdio 서버

    Pure Eio-native server for MCP protocol.
    No Lwt dependencies - uses cohttp-eio for all HTTP operations.

    Architecture:
    - HTTP Server: httpun-eio (Eio native, Effect-based)
    - HTTP Client: cohttp-eio (Pure Eio)
    - JSON-RPC: Reuses types from mcp_protocol.ml
*)

open Printf

(** ============== Server Configuration ============== *)

type config = {
  port: int;
  host: string;
  max_connections: int;
}

let default_config = {
  port = 8933;
  host = "localhost";
  max_connections = 64;
}

let allow_no_auth =
  ref (Mcp_http_auth.env_truthy "FIGMA_MCP_ALLOW_NO_AUTH"
       || Mcp_http_auth.env_truthy "MCP_ALLOW_NO_AUTH")

let set_allow_no_auth v = allow_no_auth := v

(** ============== Agent Queue for MCP-style async codegen ============== *)

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
  Bytes.unsafe_to_string out

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

(** ============== Request/Response Helpers ============== *)

module Cors = struct
  let mode () = String.lowercase_ascii (Figma_config.Cors.mode ())
  let allowed_origins () = Figma_config.Cors.allowed_origins ()
  let allow_private_network () = Figma_config.Cors.allow_private_network ()
  let allow_headers () = Figma_config.Cors.allow_headers ()

  let origin_of reqd =
    let request = Httpun.Reqd.request reqd in
    Httpun.Headers.get request.Httpun.Request.headers "origin"

  let normalize_lower s = String.lowercase_ascii s

  let default_port_for_scheme = function
    | "http" -> 80
    | "https" -> 443
    | _ -> 0

  let parse_origin_components value =
    try
      let uri = Uri.of_string value in
      match (Uri.scheme uri, Uri.host uri) with
      | (Some scheme, Some host) ->
          let scheme = normalize_lower scheme in
          let host = normalize_lower host in
          let path = Uri.path uri in
          let query = Uri.query uri in
          let fragment = Uri.fragment uri in
          if (path <> "" && path <> "/") || query <> [] || fragment <> None then None
          else if scheme <> "http" && scheme <> "https" then None
          else Some (scheme, host, Uri.port uri)
      | _ -> None
    with _ -> None

  type origin_value =
    | Origin_null
    | Origin of { scheme : string; host : string; port : int option }

  let parse_origin_value value =
    let value = String.trim value in
    if value = "null" then Some Origin_null
    else
      match parse_origin_components value with
      | Some (scheme, host, port) -> Some (Origin { scheme; host; port })
      | None -> None

  type origin_pattern =
    | Any
    | Null
    | Exact of { scheme : string; host : string; port : int option }
    | Any_port of { scheme : string; host : string }

  let parse_pattern pattern =
    let pattern = String.trim pattern in
    if pattern = "*" then Some Any
    else if pattern = "null" then Some Null
    else
      let len = String.length pattern in
      let ends_with suffix =
        let ls = String.length suffix in
        len >= ls && String.sub pattern (len - ls) ls = suffix
      in
      if ends_with ":*" then
        let base = String.sub pattern 0 (len - 2) in
        match parse_origin_components base with
        | Some (scheme, host, _) -> Some (Any_port { scheme; host })
        | None -> None
      else if ends_with "*" then
        let base = String.sub pattern 0 (len - 1) in
        match parse_origin_components base with
        | Some (scheme, host, _) -> Some (Any_port { scheme; host })
        | None -> None
      else
        match parse_origin_components pattern with
        | Some (scheme, host, port) -> Some (Exact { scheme; host; port })
        | None -> None

  let normalized_port scheme port_opt =
    match port_opt with
    | Some p -> p
    | None -> default_port_for_scheme scheme

  let matches_pattern origin pattern =
    match (origin, pattern) with
    | (_, Any) -> true
    | (Origin_null, Null) -> true
    | (Origin_null, _) -> false
    | (Origin o, Exact p) ->
        o.scheme = p.scheme
        && o.host = p.host
        && normalized_port o.scheme o.port = normalized_port p.scheme p.port
    | (Origin o, Any_port p) ->
        o.scheme = p.scheme && o.host = p.host
    | _ -> false

  let origin_allowed origin =
    match parse_origin_value origin with
    | None -> false
    | Some origin_value ->
        let allowed = allowed_origins () in
        List.exists
          (fun pattern ->
            match parse_pattern pattern with
            | Some parsed -> matches_pattern origin_value parsed
            | None -> false)
          allowed

  let is_allowed reqd =
    match mode () with
    | "restrict" -> (
        match origin_of reqd with
        | None -> true
        | Some origin -> origin_allowed origin)
    | _ -> true

  let allow_origin_value_of_origin_opt origin_opt =
    match mode () with
    | "permissive" -> Some "*"
    | "restrict" -> (
        match origin_opt with
        | Some origin when origin_allowed origin -> Some origin
        | _ -> None)
    | _ -> Some "*"

  let headers_for_origin_opt origin_opt ~include_methods ~include_headers =
    match allow_origin_value_of_origin_opt origin_opt with
    | None -> []
    | Some origin ->
        let base =
          let vary = if origin = "*" then [] else [("vary", "Origin")] in
          ("access-control-allow-origin", origin) :: vary
        in
        let headers =
          if include_methods then
            ("access-control-allow-methods", "GET, POST, OPTIONS") :: base
          else base
        in
        let headers =
          if include_headers then
            ("access-control-allow-headers", allow_headers ()) :: headers
          else headers
        in
        if allow_private_network () then
          ("access-control-allow-private-network", "true") :: headers
        else headers

  let headers reqd ~include_methods ~include_headers =
    headers_for_origin_opt (origin_of reqd) ~include_methods ~include_headers
end

module Response = struct
  let text ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ Cors.headers reqd ~include_methods:false ~include_headers:false) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let json ?(status = `OK) body reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let api_key_error message reqd =
    let body = Yojson.Safe.to_string (`Assoc [
      ("error", `String message);
    ]) in
    let headers = Httpun.Headers.of_list ([
      ("content-type", "application/json; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("www-authenticate", "API-Key");
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers `Unauthorized in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd `Unauthorized

  let accepted reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-length", "0");
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers `Accepted in
    Httpun.Reqd.respond_with_string reqd response "";
    Server_metrics.finish_reqd ~bytes:0 reqd `Accepted

  let not_found reqd =
    text ~status:`Not_found "404 Not Found" reqd

  let cors_preflight reqd =
    let headers = Httpun.Headers.of_list ([
      ("content-length", "0");
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers `No_content in
    Httpun.Reqd.respond_with_string reqd response "";
    Server_metrics.finish_reqd ~bytes:0 reqd `No_content

  (** SSE streaming response for MCP streamable-http protocol *)
  let sse_stream reqd ~on_write =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/event-stream");
      ("cache-control", "no-cache");
      ("connection", "keep-alive");
    ] @ Cors.headers reqd ~include_methods:false ~include_headers:false) in
    let response = Httpun.Response.create ~headers `OK in
    let body = Httpun.Reqd.respond_with_streaming reqd response in
    Server_metrics.finish_reqd ~bytes:0 reqd `OK;
    on_write body

  (** SSE single message response for POST→SSE (MCP Streamable HTTP) *)
  let sse_message ?(session_id="") json_str reqd =
    let event_id = Printf.sprintf "s%d-%d" (Unix.getpid ()) (Random.int 10000) in
    let prime = Printf.sprintf "retry: 5000\nid: %s:2\n\n" event_id in
    let message = Printf.sprintf "id: %s:1\ndata: %s\n\n" event_id json_str in
    let body = prime ^ message in
    let session_headers = if session_id = "" then [] else [("mcp-session-id", session_id)] in
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/event-stream");
      ("content-length", string_of_int (String.length body));
      ("cache-control", "no-cache");
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true
      @ session_headers) in
    let response = Httpun.Response.create ~headers `OK in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd `OK
end

module Request = struct
  (** Read request body - accumulates chunks until EOF.
      Uses callback pattern - the response MUST be sent from within the callback. *)
  let default_max_body_bytes = 50 * 1024 * 1024

  let parse_positive_int value =
    try
      let v = int_of_string value in
      if v > 0 then Some v else None
    with _ -> None

  let max_body_bytes =
    let from_env name =
      match Sys.getenv_opt name with
      | Some v -> parse_positive_int v
      | None -> None
    in
    match from_env "FIGMA_MCP_MAX_BODY_BYTES" with
    | Some v -> v
    | None ->
        (match from_env "MCP_MAX_BODY_BYTES" with
         | Some v -> v
         | None -> default_max_body_bytes)

  let respond_error reqd status body =
    let headers = Httpun.Headers.of_list ([
      ("content-type", "text/plain; charset=utf-8");
      ("content-length", string_of_int (String.length body));
      ("connection", "close");
    ] @ Cors.headers reqd ~include_methods:true ~include_headers:true) in
    let response = Httpun.Response.create ~headers status in
    Httpun.Reqd.respond_with_string reqd response body;
    Server_metrics.finish_reqd ~bytes:(String.length body) reqd status

  let respond_too_large reqd max_bytes =
    let body = Printf.sprintf
      "413 Request Entity Too Large (max %d bytes)" max_bytes
    in
    respond_error reqd `Payload_too_large body

  let respond_internal_error reqd exn =
    eprintf "[http] internal error: %s\n%!" (Printexc.to_string exn);
    respond_error reqd `Internal_server_error "500 Internal Server Error"

  let read_body_async reqd callback =
    let request = Httpun.Reqd.request reqd in
    let content_length =
      match Httpun.Headers.get request.Httpun.Request.headers "content-length" with
      | Some v -> parse_positive_int v
      | None -> None
    in
    let body = Httpun.Reqd.request_body reqd in
    let stopped = ref false in
    let stop () =
      if not !stopped then begin
        stopped := true;
        (try Httpun.Body.Reader.close body
         with exn -> eprintf "[mcp_protocol] Warning: body reader close failed: %s\n%!" (Printexc.to_string exn))
      end
    in
    match content_length with
    | Some len when len > max_body_bytes ->
        stop ();
        respond_too_large reqd max_body_bytes
    | _ ->
        let initial_capacity =
          match content_length with
          | Some len when len > 0 && len < max_body_bytes -> len
          | _ -> 1024
        in
        let buf = Buffer.create initial_capacity in
        let seen_bytes = ref 0 in
        let rec read_loop () =
          Httpun.Body.Reader.schedule_read body
            ~on_eof:(fun () ->
              let body_str = Buffer.contents buf in
              try callback body_str with exn ->
                respond_internal_error reqd exn)
            ~on_read:(fun buffer ~off ~len ->
              if !stopped then ()
              else
                let next_bytes = !seen_bytes + len in
                if next_bytes > max_body_bytes then begin
                  stop ();
                  respond_too_large reqd max_body_bytes
                end else begin
                  seen_bytes := next_bytes;
                  let chunk = Bigstringaf.substring buffer ~off ~len in
                  Buffer.add_string buf chunk;
                  read_loop ()
                end)
        in
        read_loop ()

  let path (request : Httpun.Request.t) =
    request.target |> String.split_on_char '?' |> List.hd

  let method_ (request : Httpun.Request.t) =
    request.meth

  (** Check if client accepts SSE (MCP Streamable HTTP) *)
  let accepts_sse (request : Httpun.Request.t) =
    match Httpun.Headers.get request.Httpun.Request.headers "accept" with
    | Some accept ->
        let accept_lower = String.lowercase_ascii accept in
        (try
          let _ = Str.search_forward (Str.regexp_string "text/event-stream") accept_lower 0 in
          true
        with Not_found -> false)
    | None -> false
end

(** ============== MCP Request Processing ============== *)

(** Process MCP request synchronously (Eio-native, no Lwt).
    Uses process_request_sync which calls handlers_sync directly. *)
let process_mcp_request_sync (server : Mcp_protocol.mcp_server) body_str =
  match Mcp_protocol.parse_request body_str with
  | Ok req ->
      (* process_request_sync: Lwt 없이 직접 실행 *)
      let response_json = Mcp_protocol.process_request_sync server req in
      Yojson.Safe.to_string response_json
  | Error msg ->
      let err_response = Mcp_protocol.make_error_response
        `Null Mcp_protocol.parse_error msg None in
      Yojson.Safe.to_string err_response

type mcp_message_kind =
  [ `Request | `Notification | `Response | `Unknown ]

let classify_message body_str =
  match Yojson.Safe.from_string body_str with
  | exception _ -> `Unknown
  | `Assoc fields ->
      let has_method = List.mem_assoc "method" fields in
      let id = List.assoc_opt "id" fields in
      let has_result = List.mem_assoc "result" fields in
      let has_error = List.mem_assoc "error" fields in
      (match has_method, id with
       | true, None
       | true, Some `Null -> `Notification
       | true, Some _ -> `Request
       | false, Some _ when has_result || has_error -> `Response
       | _ -> `Unknown)
  | _ -> `Unknown

(** ============== SSE Helpers ============== *)

(** SSE client registry for shutdown notification *)
type sse_client = {
  body: Httpun.Body.Writer.t;
  mutex: Eio.Mutex.t;
  mutable connected: bool;
}
let sse_clients : (int, sse_client) Hashtbl.t = Hashtbl.create 16

(* JS safe integer: 2^53 - 1. We keep SSE client ids numeric for compatibility,
   but make them unguessable to mitigate client_id hijacking. *)
let max_js_safe_client_id = Int64.sub (Int64.shift_left 1L 53) 1L
let sse_client_id_mask =
  (* Keep within OCaml int range for 32-bit builds, while staying JS-safe. *)
  let max_int64 = Int64.of_int max_int in
  if Int64.compare max_int64 max_js_safe_client_id < 0 then max_int64 else max_js_safe_client_id

let random_sse_client_id () =
  let int64_of_be_string s =
    let rec go i acc =
      if i = 8 then acc
      else
        let acc =
          Int64.(logor (shift_left acc 8) (of_int (Char.code s.[i])))
        in
        go (i + 1) acc
    in
    go 0 0L
  in
  let rec loop () =
    let bytes = Mirage_crypto_rng.generate 8 in
    let v = int64_of_be_string bytes |> fun x -> Int64.logand x sse_client_id_mask in
    if v = 0L then loop ()
    else
      let id = Int64.to_int v in
      if Hashtbl.mem sse_clients id then loop () else id
  in
  loop ()

let format_sse_data data =
  if data = "" then
    "data: "
  else
    data
    |> String.split_on_char '\n'
    |> List.map (fun line -> "data: " ^ line)
    |> String.concat "\n"

let register_sse_client body =
  let id = random_sse_client_id () in
  let client = { body; mutex = Eio.Mutex.create (); connected = true } in
  Hashtbl.add sse_clients id client;
  Server_metrics.sse_open ();
  (id, client)

let unregister_sse_client id =
  let was_connected =
    match Hashtbl.find_opt sse_clients id with
    | Some c ->
        c.connected <- false;
        true
    | None -> false
  in
  Hashtbl.remove sse_clients id;
  if was_connected then Server_metrics.sse_close ()

(** Send SSE event and flush immediately *)
let send_sse_event client ~event ~data =
  let data_lines = format_sse_data data in
  let msg = sprintf "event: %s\n%s\n\n" event data_lines in
  Eio.Mutex.use_rw ~protect:true client.mutex (fun () ->
    Httpun.Body.Writer.write_string client.body msg;
    Httpun.Body.Writer.flush client.body ignore
  )

let broadcast_sse_shutdown reason =
  let data = sprintf
    {|{"jsonrpc":"2.0","method":"notifications/shutdown","params":{"reason":"%s","message":"Server is shutting down, please reconnect"}}|}
    reason
  in
  Hashtbl.iter (fun _ client ->
    if client.connected then
      try
        send_sse_event client ~event:"notification" ~data
      with exn ->
        eprintf "[mcp_protocol] SSE broadcast failed for client, marking disconnected: %s\n%!" (Printexc.to_string exn);
        client.connected <- false
  ) sse_clients

(** Close all SSE connections gracefully - for shutdown *)
let close_all_sse_connections () =
  let client_ids = Hashtbl.fold (fun k _ acc -> k :: acc) sse_clients [] in
  List.iter (fun id ->
    (match Hashtbl.find_opt sse_clients id with
     | Some client ->
         let was_connected = client.connected in
         client.connected <- false;
         (try Httpun.Body.Writer.close client.body
          with exn -> eprintf "[mcp_protocol] Warning: SSE writer close failed: %s\n%!" (Printexc.to_string exn))
         ; if was_connected then Server_metrics.sse_close ()
     | None -> ());
    Hashtbl.remove sse_clients id
  ) client_ids;
  eprintf "🎨 Figma MCP: Closed %d SSE connections\n%!" (List.length client_ids)

let find_sse_client client_id =
  match client_id with
  | None -> None
  | Some id ->
      (match Hashtbl.find_opt sse_clients id with
       | Some client when client.connected -> Some (id, client)
       | _ -> None)

(** ============== Progress Notifications ============== *)

(** Generic SSE broadcast - sends data to all connected clients *)
let broadcast_sse_data data =
  (* Collect failed clients to remove after iteration *)
  let failed = ref [] in
  Hashtbl.iter (fun client_id client ->
    if client.connected then
      try send_sse_event client ~event:"notification" ~data
      with _ -> failed := client_id :: !failed
  ) sse_clients;
  (* Remove failed clients to prevent zombie accumulation *)
  List.iter unregister_sse_client !failed

(** Initialize Mcp_progress with broadcast function *)
let () = Mcp_progress.set_broadcast_fn broadcast_sse_data

(** ============== HTTP Handlers ============== *)

let health_handler _request reqd =
  let json = sprintf {|{"status":"ok","server":"%s","version":"%s","protocol":"%s"}|}
    Mcp_protocol.server_name
    Mcp_protocol.server_version
    Mcp_protocol.protocol_version
  in
  Response.json json reqd

(** MCP POST handler - async body reading with callback-based response *)
let run_mcp_request ~domain_mgr ~eio_ctx server body_str =
  let run () =
    Mcp_tools.install_eio_context eio_ctx;
    process_mcp_request_sync server body_str
  in
  match domain_mgr with
  | None -> run ()
  | Some mgr -> Eio.Domain_manager.run mgr run

let mcp_post_handler ~sw ~domain_mgr ~eio_ctx server request reqd =
  let { Httpun.Request.headers; target = request_target; _ } = request in
  let header_first keys =
    let rec loop = function
      | [] -> None
      | key :: rest ->
          (match Httpun.Headers.get headers key with
           | Some value -> Some value
           | None -> loop rest)
    in
    loop keys
  in
  let query_first keys =
    let uri = Uri.of_string request_target in
    let rec loop = function
      | [] -> None
      | key :: rest ->
          (match Uri.get_query_param uri key with
           | Some value -> Some value
           | None -> loop rest)
    in
    loop keys
  in
  let client_id =
    let raw =
      match header_first [
        "mcp-client-id";
        "x-mcp-client-id";
        "mcp-session";
        "mcp-session-id";
      ] with
      | Some value -> Some value
      | None ->
          query_first [
            "client_id";
            "clientId";
            "session";
            "session_id";
            "mcp_session";
          ]
    in
    match raw with
    | Some value -> int_of_string_opt value
    | None -> None
  in
  Request.read_body_async reqd (fun body_str ->
    match classify_message body_str with
    | `Notification ->
        Eio.Fiber.fork ~sw (fun () ->
          try
            ignore (run_mcp_request ~domain_mgr ~eio_ctx server body_str)
          with exn ->
            eprintf "[MCP] notification failed: %s\n%!" (Printexc.to_string exn));
        Response.accepted reqd
    | `Response ->
        Response.accepted reqd
    | `Request | `Unknown ->
        (match find_sse_client client_id with
         | Some (id, client) ->
             Response.accepted reqd;
             Eio.Fiber.fork ~sw (fun () ->
               try
                 let response_str = run_mcp_request ~domain_mgr ~eio_ctx server body_str in
                 send_sse_event client ~event:"message" ~data:response_str
               with exn ->
                 eprintf "[MCP] SSE request failed (client=%d): %s\n%!" id (Printexc.to_string exn);
                 unregister_sse_client id)
         | None ->
             (* Check Accept header for SSE support (MCP Streamable HTTP) *)
             let wants_sse = Request.accepts_sse request in
             (try
               let response_str = run_mcp_request ~domain_mgr ~eio_ctx server body_str in
               if wants_sse then
                 Response.sse_message response_str reqd
               else
                 Response.json response_str reqd
             with exn ->
               eprintf "[MCP] request failed: %s\n%!" (Printexc.to_string exn);
                let err = Mcp_protocol.make_error_response `Null
                  Mcp_protocol.internal_error "Internal server error" None in
                if wants_sse then
                  Response.sse_message (Yojson.Safe.to_string err) reqd
                else
                  Response.json ~status:`Internal_server_error (Yojson.Safe.to_string err) reqd)))

(** MCP SSE handler for streamable-http protocol (GET /mcp) *)
let mcp_sse_handler ~clock _request reqd =
  Response.sse_stream reqd ~on_write:(fun body ->
    (* Register client for shutdown broadcast *)
    let client_id, client = register_sse_client body in

    (* Send initial endpoint event (MCP protocol requirement) *)
    let endpoint = sprintf "/mcp?client_id=%d" client_id in
    send_sse_event client ~event:"endpoint" ~data:endpoint;

    (* Keep connection alive with periodic pings *)
    let rec ping_loop () =
      try
        Eio.Time.sleep clock 15.0;
        let timestamp = string_of_float (Unix.gettimeofday ()) in
        send_sse_event client ~event:"ping" ~data:timestamp;
        ping_loop ()
      with _ ->
        (* Client disconnected or error - unregister and close *)
        unregister_sse_client client_id;
        Httpun.Body.Writer.close body
    in
    ping_loop ()
  )

(** ============== Plugin Bridge Handlers ============== *)

(* Plugin configuration from centralized Figma_config *)
let plugin_ttl_seconds = Figma_config.Plugin.ttl_seconds
let plugin_poll_max_ms = Figma_config.Plugin.poll_max_ms

let plugin_cleanup () =
  Figma_plugin_bridge.cleanup_inactive ~ttl_seconds:plugin_ttl_seconds

let json_error ?(status=`Bad_request) msg reqd =
  let body = Yojson.Safe.to_string (`Assoc [("error", `String msg)]) in
  Response.json ~status body reqd

let parse_json body_str =
  if String.trim body_str = "" then Ok `Null
  else
    try Ok (Yojson.Safe.from_string body_str)
    with Yojson.Json_error msg -> Error msg

let get_string_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) -> Some s
       | _ -> None)
  | _ -> None

let get_int_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Int i) -> Some i
       | Some (`Float f) -> Some (int_of_float f)
       | _ -> None)
  | _ -> None

let get_bool_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`Bool b) -> Some b
       | _ -> None)
  | _ -> None

let get_payload_field key = function
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some json -> Some json
       | _ -> None)
  | _ -> None

let clamp_poll_ms value =
  let value = max 0 value in
  if value > plugin_poll_max_ms then plugin_poll_max_ms else value

let clamp_max_commands value =
  let value = max 1 value in
  if value > Figma_config.Plugin.max_commands then
    Figma_config.Plugin.max_commands
  else
    value

let wait_for_commands ~clock ~channel_id ~max ~timeout_ms =
  let commands = Figma_plugin_bridge.poll_commands ~channel_id ~max in
  if commands <> [] || timeout_ms <= 0 then
    commands
  else begin
    let promise, resolver = Eio.Promise.create () in
    let waiter_id =
      Figma_plugin_bridge.register_waiter ~channel_id ~notify:(fun () ->
        try Eio.Promise.resolve resolver ()
        with exn -> eprintf "[mcp_protocol] Warning: promise double-resolve: %s\n%!" (Printexc.to_string exn))
    in
    let commands_after = Figma_plugin_bridge.poll_commands ~channel_id ~max in
    if commands_after <> [] then begin
      Figma_plugin_bridge.unregister_waiter ~channel_id ~waiter_id;
      commands_after
    end else begin
      let wait_s = float_of_int timeout_ms /. 1000.0 in
      let result =
        match Eio.Time.with_timeout clock wait_s (fun () ->
          Eio.Promise.await promise;
          Ok `Woke) with
        | Ok `Woke -> `Woke
        | Error `Timeout -> `Timeout
      in
      Figma_plugin_bridge.unregister_waiter ~channel_id ~waiter_id;
      match result with
      | `Woke -> Figma_plugin_bridge.poll_commands ~channel_id ~max
      | `Timeout -> []
    end
  end

let plugin_connect_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    plugin_cleanup ();
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let channel_id = get_string_field "channel_id" json in
        let channel_id = Figma_plugin_bridge.register_channel ?channel_id () in
        eprintf "[Plugin] connect channel=%s\n%!" channel_id;
        let body = `Assoc [
          ("status", `String "ok");
          ("channel_id", `String channel_id);
        ] in
        Response.json (Yojson.Safe.to_string body) reqd
  )

let plugin_poll_handler ~clock _request reqd =
  Request.read_body_async reqd (fun body_str ->
    plugin_cleanup ();
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        (match get_string_field "channel_id" json with
         | None -> json_error "Missing channel_id" reqd
         | Some channel_id ->
             let max_commands =
               get_int_field "max_commands" json
               |> Option.value ~default:1
               |> clamp_max_commands
             in
             let wait_ms =
               match get_int_field "wait_ms" json with
               | Some value -> clamp_poll_ms value
               | None ->
                   (match get_int_field "timeout_ms" json with
                    | Some value -> clamp_poll_ms value
                    | None -> 0)
             in
             let commands : Figma_plugin_bridge.command list =
               if wait_ms > 0 then
                 wait_for_commands ~clock ~channel_id ~max:max_commands ~timeout_ms:wait_ms
               else
                 Figma_plugin_bridge.poll_commands ~channel_id ~max:max_commands
             in
             if commands <> [] then
               eprintf "[Plugin] poll channel=%s max=%d wait_ms=%d -> %d commands\n%!"
                 channel_id max_commands wait_ms (List.length commands);
             let commands_json =
               `List (List.map (fun (cmd : Figma_plugin_bridge.command) ->
                 `Assoc [
                   ("id", `String cmd.id);
                   ("name", `String cmd.name);
                   ("payload", cmd.payload);
                   ("created_at", `Float cmd.created_at);
                 ]) commands)
             in
             let body = `Assoc [
               ("channel_id", `String channel_id);
               ("commands", commands_json);
             ] in
             Response.json (Yojson.Safe.to_string body) reqd))

let plugin_result_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    plugin_cleanup ();
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let channel_id = get_string_field "channel_id" json in
        let command_id = get_string_field "command_id" json in
        let ok = get_bool_field "ok" json |> Option.value ~default:true in
        let payload =
          match get_payload_field "payload" json with
          | Some (`String s) -> (
              try Yojson.Safe.from_string s
              with _ -> `Assoc [
                ("error", `String "Failed to parse payload string");
                ("raw", `String s);
              ])
          | Some payload -> payload
          | None -> `Null
        in
        (match (channel_id, command_id) with
         | (Some channel_id, Some command_id) ->
             Figma_plugin_bridge.store_result ~channel_id ~command_id ~ok ~payload;
             eprintf "[Plugin] result channel=%s cmd=%s ok=%b\n%!"
               channel_id command_id ok;
             let body = `Assoc [("status", `String "ok")] in
             Response.json (Yojson.Safe.to_string body) reqd
         | _ ->
             json_error "Missing channel_id or command_id" reqd))

let plugin_event_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    plugin_cleanup ();
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let channel_id = get_string_field "channel_id" json in
        let event_type = get_string_field "event_type" json in
        let payload =
          match get_payload_field "payload" json with
          | Some p -> p
          | None -> `Null
        in
        (match (channel_id, event_type) with
         | (Some channel_id, Some event_type) ->
             Figma_plugin_bridge.publish_event ~channel_id ~event_type ~payload;
             eprintf "[Plugin] event channel=%s type=%s\n%!"
               channel_id event_type;
             let body = `Assoc [("status", `String "ok")] in
             Response.json (Yojson.Safe.to_string body) reqd
         | _ ->
             json_error "Missing channel_id or event_type" reqd))

let plugin_status_handler _request reqd =
  plugin_cleanup ();
  let channels = Figma_plugin_bridge.list_channels () in
  let stats = Figma_plugin_bridge.list_channel_stats () in
  let default_channel = Figma_plugin_bridge.get_default_channel () in
  let stats_json =
    `List (List.map (fun (s : Figma_plugin_bridge.channel_stats) ->
      `Assoc [
        ("id", `String s.id);
        ("last_seen", `Float s.last_seen);
        ("commands", `Int s.commands);
        ("results", `Int s.results);
        ("waiters", `Int s.waiters);
      ]) stats)
  in
  let limits = `Assoc [
    ("max_commands", `Int Figma_config.Plugin.max_commands);
    ("max_results", `Int Figma_config.Plugin.max_results);
    ("max_waiters", `Int Figma_config.Plugin.max_waiters);
    ("result_ttl_seconds", `Float Figma_config.Plugin.result_ttl_seconds);
    ("cleanup_interval_seconds", `Float Figma_config.Plugin.cleanup_interval_seconds);
    ("poll_max_ms", `Int Figma_config.Plugin.poll_max_ms);
  ] in
  let body = `Assoc [
    ("channels", `List (List.map (fun id -> `String id) channels));
    ("stats", stats_json);
    ("limits", limits);
    ("default_channel", match default_channel with Some id -> `String id | None -> `Null);
  ] in
  Response.json (Yojson.Safe.to_string body) reqd

(** Semantic analyzer - extracts structured info with exact measurements *)
let analyze_node_semantic node =
  let open Yojson.Safe.Util in
  let buf = Buffer.create 2048 in
  let add s = Buffer.add_string buf s; Buffer.add_char buf '\n' in

  let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in
  let to_int json = int_of_float (to_num json) in

  (* Extract design tokens (colors) *)
  let colors = Hashtbl.create 16 in
  let rec collect_colors n =
    (match member "fills" n with
     | `List fills -> List.iter (fun fill ->
         match member "color" fill with
         | `String c -> Hashtbl.replace colors c (member "name" n |> to_string_option |> Option.value ~default:"")
         | _ -> ()
       ) fills
     | _ -> ());
    (match member "children" n with
     | `List kids -> List.iter collect_colors kids
     | _ -> ())
  in
  collect_colors node;

  (* Root info *)
  let name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
  let w = member "width" node |> to_int in
  let h = member "height" node |> to_int in
  let bg = match member "fills" node with
    | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> c | _ -> "transparent")
    | _ -> "transparent"
  in

  add (sprintf "## Layout: %s (%dx%d, background: %s)" name w h bg);
  add "";

  (* Analyze children with exact measurements *)
  let rec analyze_child depth n =
    let indent = String.make (depth * 2) ' ' in
    let cname = member "name" n |> to_string_option |> Option.value ~default:"Layer" in
    let ctype = member "type" n |> to_string_option |> Option.value ~default:"" in
    let cx = member "x" node |> to_int in
    let cy = member "y" node |> to_int in
    let cw = member "width" n |> to_int in
    let ch = member "height" n |> to_int in
    let radius = member "cornerRadius" n |> to_num in

    (* Component pattern detection *)
    let pattern =
      let ln = String.lowercase_ascii cname in
      if String.length ln >= 4 && String.sub ln 0 4 = "side" then "Sidebar"
      else if String.length ln >= 6 && String.sub ln 0 6 = "header" then "Header"
      else if String.length ln >= 4 && String.sub ln 0 4 = "card" then "Card"
      else if String.length ln >= 4 && String.sub ln 0 4 = "stat" then "StatCard"
      else if String.length ln >= 3 && String.sub ln 0 3 = "nav" then "NavItem"
      else if String.length ln >= 6 && String.sub ln 0 6 = "button" then "Button"
      else if String.length ln >= 6 && String.sub ln 0 6 = "search" then "SearchInput"
      else if String.length ln >= 4 && String.sub ln 0 4 = "logo" then "Logo"
      else if String.length ln >= 6 && String.sub ln 0 6 = "avatar" then "Avatar"
      else ""
    in

    let pattern_hint = if pattern <> "" then sprintf " [%s]" pattern else "" in

    (match ctype with
     | "TEXT" ->
         let text = member "text" n in
         let chars = member "characters" text |> to_string_option |> Option.value ~default:"" in
         let fontSize = member "fontSize" text |> to_num in
         let fill = match member "fills" n with
           | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> c | _ -> "")
           | _ -> ""
         in
         add (sprintf "%s- TEXT \"%s\": %dpx, color %s%s" indent chars (int_of_float fontSize) fill pattern_hint)
     | "FRAME" | "RECTANGLE" | "GROUP" ->
         let fill = match member "fills" n with
           | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> ", bg " ^ c | _ -> "")
           | _ -> ""
         in
         let rad = if radius > 0.0 then sprintf ", radius %.0fpx" radius else "" in
         add (sprintf "%s- %s \"%s\": %dx%d at (%d,%d)%s%s%s" indent ctype cname cw ch cx cy fill rad pattern_hint);
         (match member "children" n with
          | `List kids when depth < 3 -> List.iter (analyze_child (depth + 1)) kids
          | `List kids -> add (sprintf "%s  ... %d more children" indent (List.length kids))
          | _ -> ())
     | "ELLIPSE" ->
         let fill = match member "fills" n with
           | `List (`Assoc f :: _) -> (match List.assoc_opt "color" f with Some (`String c) -> ", fill " ^ c | _ -> "")
           | _ -> ""
         in
         add (sprintf "%s- ELLIPSE \"%s\": %dx%d%s%s" indent cname cw ch fill pattern_hint)
     | _ ->
         add (sprintf "%s- %s \"%s\": %dx%d%s" indent ctype cname cw ch pattern_hint));
  in

  (match member "children" node with
   | `List kids ->
       add "## Structure:";
       List.iter (analyze_child 0) kids
   | _ -> ());

  (* Design tokens *)
  add "";
  add "## Design Tokens:";
  Hashtbl.iter (fun color ctx ->
    add (sprintf "- %s (used in: %s)" color (if ctx = "" then "fill" else ctx))
  ) colors;

  Buffer.contents buf

(** Generate code template for a Figma node - with recursive children generation *)
let rec generate_code_template ?(depth=0) node platform =
  let open Yojson.Safe.Util in
  let max_depth = 3 in (* Limit recursion depth *)
  let name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
  let safe_name = String.map (fun c -> if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_') name in
  let safe_name = if String.length safe_name > 0 && safe_name.[0] >= '0' && safe_name.[0] <= '9' then "_" ^ safe_name else safe_name in
  let to_number json = match json with
    | `Float f -> f | `Int i -> float_of_int i | _ -> 100.0
  in
  let w = member "width" node |> to_number |> int_of_float in
  let h = member "height" node |> to_number |> int_of_float in
  let node_type = member "type" node |> to_string_option |> Option.value ~default:"FRAME" in

  (* Extract background color from fills *)
  let bg_color = match member "fills" node with
    | `List ((`Assoc fields) :: _) ->
        (match List.assoc_opt "color" fields with
         | Some (`String c) -> Some c
         | _ -> None)
    | _ -> None
  in

  (* Extract children and generate code recursively *)
  let children = match member "children" node with
    | `List kids -> kids
    | _ -> []
  in
  let child_count = List.length children in

  (* Generate child components recursively (up to max_depth) *)
  let generate_child_code child =
    let cname = member "name" child |> to_string_option |> Option.value ~default:"Layer" in
    let ctype = member "type" child |> to_string_option |> Option.value ~default:"FRAME" in
    let cw = member "width" child |> to_number |> int_of_float in
    let ch = member "height" child |> to_number |> int_of_float in
    let safe_cname = String.map (fun c -> if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_') cname in

    if depth >= max_depth then
      (* At max depth, just generate placeholder *)
      match platform with
      | "react" -> sprintf "      <div style={{width: %d, height: %d}}>{/* %s */}</div>" cw ch cname
      | "swiftui" -> sprintf "            Rectangle().frame(width: %d, height: %d) // %s" cw ch cname
      | "compose" -> sprintf "        Box(Modifier.size(%d.dp, %d.dp)) // %s" cw ch cname
      | "flutter" -> sprintf "        SizedBox(width: %d, height: %d), // %s" cw ch cname
      | _ -> sprintf "  <div style=\"width: %dpx; height: %dpx;\"><!-- %s --></div>" cw ch cname
    else if ctype = "TEXT" then
      (* TEXT nodes become text elements *)
      let text_content = member "characters" child |> to_string_option |> Option.value ~default:cname in
      match platform with
      | "react" -> sprintf "      <span>%s</span>" text_content
      | "swiftui" -> sprintf "            Text(\"%s\")" text_content
      | "compose" -> sprintf "        Text(\"%s\")" text_content
      | "flutter" -> sprintf "        Text('%s')," text_content
      | _ -> sprintf "  <span>%s</span>" text_content
    else
      (* Recursively generate for FRAME/GROUP/COMPONENT *)
      match platform with
      | "react" -> sprintf "      <%s />" safe_cname
      | "swiftui" -> sprintf "            %s()" safe_cname
      | "compose" -> sprintf "        %s()" safe_cname
      | "flutter" -> sprintf "        %s()," safe_cname
      | _ -> sprintf "  <%s></%s>" (String.lowercase_ascii safe_cname) (String.lowercase_ascii safe_cname)
  in

  let children_code =
    if child_count = 0 then ""
    else
      let limited_children = if child_count > 10 then List.filteri (fun i _ -> i < 10) children else children in
      String.concat "\n" (List.map generate_child_code limited_children) ^
      (if child_count > 10 then sprintf "\n      {/* ... and %d more children */}" (child_count - 10) else "")
  in

  let children_comment =
    if child_count = 0 || depth > 0 then ""
    else
      let child_lines = List.mapi (fun _i child ->
        let cname = member "name" child |> to_string_option |> Option.value ~default:"Layer" in
        let ctype = member "type" child |> to_string_option |> Option.value ~default:"" in
        sprintf "//   - %s (%s)" cname ctype
      ) (if child_count > 8 then List.filteri (fun i _ -> i < 8) children else children) in
      let truncated = if child_count > 8 then sprintf "\n//   ... and %d more" (child_count - 8) else "" in
      sprintf "\n// Children (%d):\n%s%s\n" child_count (String.concat "\n" child_lines) truncated
  in

  (* Generate sub-component definitions for deeper children *)
  let sub_components =
    if depth >= max_depth || child_count = 0 then ""
    else
      let sub_defs = List.filter_map (fun child ->
        let ctype = member "type" child |> to_string_option |> Option.value ~default:"" in
        if ctype = "TEXT" then None
        else Some (generate_code_template ~depth:(depth + 1) child platform)
      ) (if child_count > 5 then List.filteri (fun i _ -> i < 5) children else children) in
      if List.length sub_defs = 0 then "" else "\n" ^ String.concat "\n" sub_defs
  in

  let _ = node_type in (* suppress unused warning *)

  (* Extract auto-layout info *)
  let layout_mode = match member "autoLayout" node with
    | `Assoc fields ->
        (match List.assoc_opt "mode" fields with
         | Some (`String m) -> Some m
         | _ -> None)
    | _ -> None
  in
  let layout_spacing = match member "autoLayout" node with
    | `Assoc fields ->
        (match List.assoc_opt "spacing" fields with
         | Some (`Int s) -> Some s
         | Some (`Float s) -> Some (int_of_float s)
         | _ -> None)
    | _ -> None
  in

  (* Build child content or empty placeholder *)
  let empty_children = if child_count = 0 then "{/* Empty */}" else "" in
  let react_children = if children_code = "" then empty_children else "\n" ^ children_code ^ "\n    " in
  let swift_children = if children_code = "" then "EmptyView()" else "\n" ^ children_code ^ "\n        " in
  let compose_children = if children_code = "" then "// Empty" else "\n" ^ children_code ^ "\n    " in
  let flutter_children = if children_code = "" then "// Empty" else "\n" ^ children_code ^ "\n      " in
  let html_children = if children_code = "" then "<!-- Empty -->" else "\n" ^ children_code ^ "\n" in

  match platform with
  | "react" ->
      let bg_style = match bg_color with Some c -> sprintf "backgroundColor: '%s',\n        " c | None -> "" in
      let flex_style = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "display: 'flex', flexDirection: 'row', gap: %d,\n        " sp
        | Some "VERTICAL", Some sp -> sprintf "display: 'flex', flexDirection: 'column', gap: %d,\n        " sp
        | _ -> ""
      in
      sprintf "import React from 'react';\n%s\nexport const %s: React.FC = () => (\n  <div style={{\n        width: %d, height: %d,\n        %s%s}}>%s</div>\n);\n%s"
        children_comment safe_name w h bg_style flex_style react_children sub_components

  | "swiftui" ->
      let bg_mod = match bg_color with Some c -> sprintf "\n        .background(Color(hex: \"%s\"))" c | None -> "" in
      let stack = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "HStack(spacing: %d)" sp
        | Some "VERTICAL", Some sp -> sprintf "VStack(spacing: %d)" sp
        | _ -> "ZStack"
      in
      sprintf "import SwiftUI\n%s\nstruct %s: View {\n    var body: some View {\n        %s {%s}\n        .frame(width: %d, height: %d)%s\n    }\n}\n%s"
        children_comment safe_name stack swift_children w h bg_mod sub_components

  | "compose" ->
      let bg_mod = match bg_color with Some c -> sprintf ".background(Color(0xFF%s))\n            " (String.sub c 1 (String.length c - 1)) | None -> "" in
      let layout = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "Row(horizontalArrangement = Arrangement.spacedBy(%d.dp))" sp
        | Some "VERTICAL", Some sp -> sprintf "Column(verticalArrangement = Arrangement.spacedBy(%d.dp))" sp
        | _ -> "Box"
      in
      sprintf "@Composable\nfun %s(modifier: Modifier = Modifier) {%s\n    %s(modifier.size(%d.dp, %d.dp)%s) {%s}\n}\n%s"
        safe_name children_comment layout w h bg_mod compose_children sub_components

  | "flutter" ->
      let bg_prop = match bg_color with Some c -> sprintf "color: Color(0xFF%s),\n      " (String.sub c 1 (String.length c - 1)) | None -> "" in
      let layout = match layout_mode with Some "HORIZONTAL" -> "Row" | Some "VERTICAL" -> "Column" | _ -> "Stack" in
      sprintf "import 'package:flutter/material.dart';\n%s\nclass %s extends StatelessWidget {\n  const %s({super.key});\n\n  @override\n  Widget build(BuildContext context) {\n    return Container(\n      width: %d, height: %d,\n      %schild: %s(children: [%s]),\n    );\n  }\n}\n%s"
        children_comment safe_name safe_name w h bg_prop layout flutter_children sub_components

  | _ ->
      let bg_css = match bg_color with Some c -> sprintf "background-color: %s;\n  " c | None -> "" in
      let flex_css = match layout_mode, layout_spacing with
        | Some "HORIZONTAL", Some sp -> sprintf "display: flex; flex-direction: row; gap: %dpx;\n  " sp
        | Some "VERTICAL", Some sp -> sprintf "display: flex; flex-direction: column; gap: %dpx;\n  " sp
        | _ -> ""
      in
      sprintf "<!-- %s -->%s\n<div class=\"%s\">%s</div>\n\n<style>\n.%s {\n  width: %dpx; height: %dpx;\n  %s%s}\n</style>\n%s"
        name children_comment (String.lowercase_ascii safe_name) html_children (String.lowercase_ascii safe_name) w h bg_css flex_css sub_components

(** POST /plugin/template - Direct template generation (for testing) *)
let template_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platform = member "platform" json |> to_string_option |> Option.value ~default:"react" in
        let code = generate_code_template node platform in
        let result = `Assoc [
          ("code", `String code);
          ("platform", `String platform);
          ("source", `String "template");
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** Plugin codegen handler - calls llm-mcp for code generation *)
let plugin_codegen_handler ~sw ~eio_ctx _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platform = member "platform" json |> to_string_option |> Option.value ~default:"react" in
        let prompt = member "prompt" json |> to_string_option |> Option.value ~default:"" in

        (* Build LLM request with semantic analysis *)
        let semantic_info = analyze_node_semantic node in
        let platform_instruction = match platform with
          | "react" -> "Generate production-ready React/TypeScript code. Use functional components with proper typing. Include all exact pixel values for width, height, padding, margin, fontSize, borderRadius."
          | "swiftui" -> "Generate production-ready SwiftUI code. Use proper View modifiers with exact pixel values for frame, padding, cornerRadius, fontSize."
          | "flutter" -> "Generate production-ready Flutter/Dart code. Use exact pixel values in SizedBox, Container, EdgeInsets, BorderRadius."
          | "compose" -> "Generate production-ready Jetpack Compose/Kotlin code. Use exact Dp values for size, padding, corner radius."
          | _ -> "Generate production-ready code with exact pixel measurements."
        in
        let full_prompt = if prompt = "" then
          sprintf "Convert this Figma design to %s code.\n\n%s\n\n%s\n\nIMPORTANT: Use EXACT pixel values from the design. Do not approximate." platform semantic_info platform_instruction
        else
          prompt
        in

        (* Fallback to template *)
        let send_template () =
          let template = generate_code_template node platform in
          let result = `Assoc [("code", `String template); ("platform", `String platform); ("fallback", `Bool true)] in
          Response.json (Yojson.Safe.to_string result) reqd
        in

        (* Try Ollama *)
        let try_ollama () =
          let ollama_url = "http://127.0.0.1:11434/api/generate" in
          let ollama_body = `Assoc [
            ("model", `String "qwen3-coder:30b");
            ("prompt", `String full_prompt);
            ("stream", `Bool false);
          ] in
          let cohttp = Figma_api_eio.get_cohttp_client eio_ctx.Mcp_tools.client in
          let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
          let req_body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string ollama_body) in
          let uri = Uri.of_string ollama_url in
          let resp, resp_body = Cohttp_eio.Client.post cohttp ~sw ~headers ~body:req_body uri in
          let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
          if status_code < 200 || status_code >= 300 then failwith "Ollama HTTP error";
          let ollama_resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:(10 * 1024 * 1024) in
          let ollama_resp = Yojson.Safe.from_string ollama_resp_str in
          let gen_code = member "response" ollama_resp |> to_string_option |> Option.value ~default:"" in
          if String.length gen_code > 10 then
            let result_json = `Assoc [("code", `String gen_code); ("platform", `String platform); ("source", `String "ollama")] in
            Response.json (Yojson.Safe.to_string result_json) reqd
          else
            send_template ()
        in

        (* Try Claude API first if key available *)
        let anthropic_key = Sys.getenv_opt "ANTHROPIC_API_KEY" in
        (match anthropic_key with
        | Some key when String.length key > 10 ->
            printf "[Codegen] Trying Claude API...\n%!";
            (try
              let cohttp = Figma_api_eio.get_cohttp_client eio_ctx.Mcp_tools.client in
              let claude_body = `Assoc [
                ("model", `String "claude-sonnet-4-20250514");  (* Claude 4 Sonnet *)
                ("max_tokens", `Int 4096);
                ("messages", `List [
                  `Assoc [
                    ("role", `String "user");
                    ("content", `String full_prompt)
                  ]
                ]);
              ] in
              let headers = Cohttp.Header.of_list [
                ("Content-Type", "application/json");
                ("x-api-key", key);
                ("anthropic-version", "2023-06-01");
              ] in
              let req_body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string claude_body) in
              let uri = Uri.of_string "https://api.anthropic.com/v1/messages" in
              let resp, resp_body = Cohttp_eio.Client.post cohttp ~sw ~headers ~body:req_body uri in
              let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
              if status_code < 200 || status_code >= 300 then begin
                let err_body = try Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:4096 with _ -> "" in
                printf "[Codegen] Claude HTTP %d: %s\n%!" status_code err_body;
                failwith (sprintf "Claude HTTP %d" status_code)
              end;
              let claude_resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:(10 * 1024 * 1024) in
              let claude_resp = Yojson.Safe.from_string claude_resp_str in
              let content_blocks = member "content" claude_resp |> to_list in
              let gen_code = List.fold_left (fun acc block ->
                let text = member "text" block |> to_string_option |> Option.value ~default:"" in
                acc ^ text
              ) "" content_blocks in
              if String.length gen_code > 10 then
                let result_json = `Assoc [("code", `String gen_code); ("platform", `String platform); ("source", `String "claude")] in
                Response.json (Yojson.Safe.to_string result_json) reqd
              else begin
                printf "[Codegen] Claude returned empty, fallback to Ollama\n%!";
                try_ollama ()
              end
            with exn ->
              printf "[Codegen] Claude error: %s, fallback to Ollama\n%!" (Printexc.to_string exn);
              try_ollama ())
        | _ ->
            (* No Claude key, use Ollama *)
            (try try_ollama () with exn ->
              Printf.eprintf "[Codegen] Ollama fallback: %s, using template\n%!" (Printexc.to_string exn);
              send_template ()))
  )

(** Plugin analyze handler - analyzes node structure with LLM insights *)
let plugin_analyze_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let prompt = member "prompt" json |> to_string_option |> Option.value ~default:"" in

        (* Build analysis from node info *)
        let node_info = Yojson.Safe.to_string node in
        let full_prompt = if prompt = "" then
          sprintf "Analyze this Figma node and provide insights:\n%s\n\nProvide: 1) Structure overview, 2) Design patterns used, 3) Accessibility considerations, 4) Implementation recommendations." node_info
        else
          prompt
        in

        (* Try llm-mcp, fallback to local analysis *)
        (* Local analysis - fast and reliable, no LLM dependency *)
        let _ = full_prompt in (* suppress unused warning *)
        let to_number json = match json with
          | `Float f -> f | `Int i -> float_of_int i | _ -> 0.0
        in
        let name = member "name" node |> to_string_option |> Option.value ~default:"Unnamed" in
        let node_type = member "type" node |> to_string_option |> Option.value ~default:"UNKNOWN" in
        let w = member "width" node |> to_number |> int_of_float in
        let h = member "height" node |> to_number |> int_of_float in
        let children = match member "children" node with `List kids -> List.length kids | _ -> 0 in
        let has_autolayout = match member "autoLayout" node with `Null -> false | _ -> true in
        let fills_count = match member "fills" node with `List f -> List.length f | _ -> 0 in

        (* Build children list *)
        let children_detail = match member "children" node with
          | `List kids ->
              let child_items = List.mapi (fun i c ->
                let cname = member "name" c |> to_string_option |> Option.value ~default:(sprintf "Layer %d" i) in
                let ctype = member "type" c |> to_string_option |> Option.value ~default:"UNKNOWN" in
                sprintf "  - %s (%s)" cname ctype
              ) (List.filteri (fun i _ -> i < 10) kids) in
              if List.length kids > 10 then
                String.concat "\n" child_items ^ sprintf "\n  - ... and %d more" (List.length kids - 10)
              else
                String.concat "\n" child_items
          | _ -> "  (none)"
        in

        let analysis = sprintf "## Analysis: %s\n\n**Type**: %s\n**Dimensions**: %d × %d px\n**Children**: %d layer(s)\n%s\n\n**Auto-layout**: %s\n**Fills**: %d\n\n### Recommendations\n- %s\n- Consider adding semantic naming for accessibility\n- %s"
          name node_type w h children children_detail
          (if has_autolayout then "Yes (responsive)" else "No (fixed)")
          fills_count
          (if children > 5 then "Consider grouping related layers" else "Structure looks manageable")
          (if w > 1200 then "Large width - ensure responsive breakpoints" else "Width suitable for most viewports")
        in
        let result_json = `Assoc [("analysis", `String analysis); ("source", `String "local")] in
        Response.json (Yojson.Safe.to_string result_json) reqd
  )

(** ============== Agent Queue Handlers ============== *)

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

(** POST /agent/request - Plugin submits a codegen request to queue *)
let agent_request_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platform = member "platform" json |> to_string_option |> Option.value ~default:"react" in
        let prompt = member "prompt" json |> to_string_option |> Option.value ~default:"" in
        let priority = member "priority" json |> to_int_option |> Option.value ~default:0 in
        let ctx_digest = member "context_digest" json |> to_string_option |> Option.value ~default:"" in
        let node_info = Yojson.Safe.to_string node in
        let full_prompt = if prompt = "" then
          sprintf "Convert this Figma node to %s code:\n%s\n\nGenerate clean, production-ready code." platform node_info
        else prompt in
        let context_digest =
          if ctx_digest <> "" then ctx_digest
          else Digest.to_hex (Digest.string (full_prompt ^ "\n" ^ node_info))
        in
        let (req_id, request_secret) =
          agent_add_request ~priority ~context_digest node platform full_prompt
        in
        let result = `Assoc [
          ("request_id", `String req_id);
          ("request_secret", `String request_secret);
          ("status", `String "queued");
          ("priority", `Int priority);
          ("context_digest", `String context_digest);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** GET /agent/pending - Agent polls for pending requests *)
let agent_pending_handler _request reqd =
  agent_cleanup_old ();
  let pending = agent_get_pending () in
  let requests = List.map (fun req ->
    agent_request_json ~include_node:false ~include_prompt:false req
  ) pending in
  let result = `Assoc [("pending", `List requests); ("count", `Int (List.length pending))] in
  Response.json (Yojson.Safe.to_string result) reqd

(** POST /agent/claim - Agent claims a pending request *)
let agent_claim_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let worker_id = member "worker_id" json |> to_string_option |> Option.value ~default:"" in
        if worker_id = "" then json_error "worker_id required" reqd
        else begin
          agent_cleanup_old ();
          match agent_claim ~worker_id with
          | None ->
              let result = `Assoc [("status", `String "empty")] in
              Response.json (Yojson.Safe.to_string result) reqd
          | Some req ->
              let result = `Assoc [
                ("status", `String "claimed");
                ("request", agent_request_json ~include_claim_token:true ~include_node:true ~include_prompt:true req);
              ] in
              Response.json (Yojson.Safe.to_string result) reqd
        end
  )

(** POST /agent/heartbeat - Agent keeps claim alive *)
let agent_heartbeat_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let worker_id = member "worker_id" json |> to_string_option |> Option.value ~default:"" in
        let req_id = member "request_id" json |> to_string_option |> Option.value ~default:"" in
        let claim_token = member "claim_token" json |> to_string_option |> Option.value ~default:"" in
        if worker_id = "" || req_id = "" || claim_token = "" then
          json_error "worker_id, request_id and claim_token required" reqd
        else
          match agent_heartbeat ~worker_id ~claim_token req_id with
          | Ok () ->
              let result = `Assoc [("status", `String "ok"); ("request_id", `String req_id)] in
              Response.json (Yojson.Safe.to_string result) reqd
          | Error msg -> json_error msg reqd
  )

(** POST /agent/abandon - Agent releases claim *)
let agent_abandon_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let worker_id = member "worker_id" json |> to_string_option |> Option.value ~default:"" in
        let req_id = member "request_id" json |> to_string_option |> Option.value ~default:"" in
        let reason = member "reason" json |> to_string_option |> Option.value ~default:"abandoned" in
        let claim_token = member "claim_token" json |> to_string_option |> Option.value ~default:"" in
        if worker_id = "" || req_id = "" || claim_token = "" then
          json_error "worker_id, request_id and claim_token required" reqd
        else
          match agent_abandon ~worker_id ~claim_token ~reason req_id with
          | Ok () ->
              let result = `Assoc [("status", `String "ok"); ("request_id", `String req_id)] in
              Response.json (Yojson.Safe.to_string result) reqd
          | Error msg -> json_error msg reqd
  )

(** POST /agent/result - Agent submits generated code *)
let agent_result_handler _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let req_id = member "request_id" json |> to_string_option |> Option.value ~default:"" in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let claim_token = member "claim_token" json |> to_string_option |> Option.value ~default:"" in
        let worker_id = member "worker_id" json |> to_string_option in
        let context_digest = member "context_digest" json |> to_string_option in
        if req_id = "" || code = "" || claim_token = "" then
          json_error "request_id, code and claim_token required" reqd
        else
          match agent_submit_result ?worker_id ?context_digest ~claim_token req_id code with
          | Ok () ->
              let result = `Assoc [("status", `String "submitted"); ("request_id", `String req_id)] in
              Response.json (Yojson.Safe.to_string result) reqd
          | Error msg -> json_error msg reqd
  )

(** GET /agent/status/:id - Check request status *)
let agent_status_handler request reqd =
  let path = Request.path request in
  let req_id = String.sub path 14 (String.length path - 14) in (* /agent/status/ = 14 chars *)
  let request_secret =
    match Httpun.Headers.get request.Httpun.Request.headers "x-mcp-request-secret" with
    | Some v ->
        let v = String.trim v in
        if v = "" then None else Some v
    | None -> None
  in
  match agent_get_result req_id with
  | Some req ->
      let authorized =
        match request_secret with
        | Some s -> s = req.request_secret
        | None -> false
      in
      let base = [
        ("status", `String (agent_status_to_string req.status));
        ("request_id", `String req.id);
        ("priority", `Int req.priority);
        ("context_digest", `String req.context_digest);
        ("claimed_by", (match req.claimed_by with Some v -> `String v | None -> `Null));
        ("claimed_at", (match req.claimed_at with Some v -> `Float v | None -> `Null));
        ("last_heartbeat", (match req.last_heartbeat with Some v -> `Float v | None -> `Null));
        ("attempts", `Int req.attempts);
        ("drifted", `Bool req.drifted);
        ("error", (match req.error with Some v -> `String v | None -> `Null));
        ("age_sec", `Float (Unix.gettimeofday () -. req.created_at));
        ("code_available", `Bool (req.status = Completed && req.result <> None));
        ("authorized", `Bool authorized);
      ] in
      let result =
        match req.status, req.result with
        | Completed, Some code when authorized -> `Assoc (("code", `String code) :: base)
        | _ -> `Assoc base
      in
      Response.json (Yojson.Safe.to_string result) reqd
  | None ->
      let result = `Assoc [("status", `String "not_found")] in
      Response.json (Yojson.Safe.to_string result) reqd

(** GET /agent/queue - Queue monitoring *)
let agent_queue_handler _request reqd =
  agent_cleanup_old ();
  let items =
    Eio.Mutex.use_rw ~protect:true agent_queue_mutex (fun () ->
      Hashtbl.fold (fun _ req acc -> req :: acc) agent_queue [])
  in
  let payload = List.map (fun req ->
    agent_request_json ~include_node:false ~include_prompt:false req
  ) items in
  let result = `Assoc [
    ("stats", agent_queue_stats_json ());
    ("items", `List payload);
    ("count", `Int (List.length items));
  ] in
  Response.json (Yojson.Safe.to_string result) reqd

(** POST /plugin/extract-tokens - Extract design tokens from Figma node *)
let extract_tokens_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let format = member "format" json |> to_string_option |> Option.value ~default:"tokens-studio" in

        (* Token collection *)
        let colors = Hashtbl.create 32 in
        let typography = Hashtbl.create 16 in
        let spacing = Hashtbl.create 16 in
        let radii = Hashtbl.create 16 in
        let effects = Hashtbl.create 16 in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* RGBA color to hex with alpha *)
        let rgba_to_hex r g b a =
          let ri = int_of_float (r *. 255.0) in
          let gi = int_of_float (g *. 255.0) in
          let bi = int_of_float (b *. 255.0) in
          if a < 1.0 then
            sprintf "rgba(%d, %d, %d, %.2f)" ri gi bi a
          else
            sprintf "#%02x%02x%02x" ri gi bi
        in

        (* Extract color from fill/stroke *)
        let extract_color prefix fill =
          match member "color" fill with
          | `Assoc color_obj ->
              let r = List.assoc_opt "r" color_obj |> Option.map to_num |> Option.value ~default:0.0 in
              let g = List.assoc_opt "g" color_obj |> Option.map to_num |> Option.value ~default:0.0 in
              let b = List.assoc_opt "b" color_obj |> Option.map to_num |> Option.value ~default:0.0 in
              let a = List.assoc_opt "a" color_obj |> Option.map to_num |> Option.value ~default:1.0 in
              let hex = rgba_to_hex r g b a in
              Hashtbl.replace colors (prefix ^ "-" ^ hex) hex
          | _ -> ()
        in

        (* Recursive token extraction *)
        let rec extract_from_node n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in

          (* Colors from fills *)
          (match member "fills" n with
           | `List fills -> List.iteri (fun i fill ->
               let visible = member "visible" fill |> to_bool_option |> Option.value ~default:true in
               if visible then extract_color (sprintf "fill-%s-%d" name i) fill
             ) fills
           | _ -> ());

          (* Colors from strokes *)
          (match member "strokes" n with
           | `List strokes -> List.iteri (fun i stroke ->
               extract_color (sprintf "stroke-%s-%d" name i) stroke
             ) strokes
           | _ -> ());

          (* Typography from TEXT nodes *)
          if node_type = "TEXT" then begin
            let style = member "style" n in
            let font_family = member "fontFamily" style |> to_string_option |> Option.value ~default:"" in
            let font_size = member "fontSize" style |> to_num in
            let font_weight = member "fontWeight" style |> to_num in
            let line_height = member "lineHeightPx" style |> to_num in
            let letter_spacing = member "letterSpacing" style |> to_num in
            if font_family <> "" then
              Hashtbl.replace typography name (`Assoc [
                ("fontFamily", `String font_family);
                ("fontSize", `Float font_size);
                ("fontWeight", `Int (int_of_float font_weight));
                ("lineHeight", `Float line_height);
                ("letterSpacing", `Float letter_spacing);
              ])
          end;

          (* Spacing from auto-layout *)
          let item_spacing = member "itemSpacing" n |> to_num in
          let padding_top = member "paddingTop" n |> to_num in
          let padding_right = member "paddingRight" n |> to_num in
          let padding_bottom = member "paddingBottom" n |> to_num in
          let padding_left = member "paddingLeft" n |> to_num in

          if item_spacing > 0.0 then
            Hashtbl.replace spacing (sprintf "gap-%s" name) (`Float item_spacing);
          if padding_top > 0.0 || padding_right > 0.0 || padding_bottom > 0.0 || padding_left > 0.0 then
            Hashtbl.replace spacing (sprintf "padding-%s" name) (`Assoc [
              ("top", `Float padding_top);
              ("right", `Float padding_right);
              ("bottom", `Float padding_bottom);
              ("left", `Float padding_left);
            ]);

          (* Border radius *)
          let corner_radius = member "cornerRadius" n |> to_num in
          let top_left = member "topLeftRadius" n |> to_num in
          let top_right = member "topRightRadius" n |> to_num in
          let bottom_right = member "bottomRightRadius" n |> to_num in
          let bottom_left = member "bottomLeftRadius" n |> to_num in

          if corner_radius > 0.0 then
            Hashtbl.replace radii (sprintf "radius-%s" name) (`Float corner_radius)
          else if top_left > 0.0 || top_right > 0.0 || bottom_right > 0.0 || bottom_left > 0.0 then
            Hashtbl.replace radii (sprintf "radius-%s" name) (`Assoc [
              ("topLeft", `Float top_left);
              ("topRight", `Float top_right);
              ("bottomRight", `Float bottom_right);
              ("bottomLeft", `Float bottom_left);
            ]);

          (* Effects (shadows, blurs) *)
          (match member "effects" n with
           | `List effs -> List.iteri (fun i eff ->
               let eff_type = member "type" eff |> to_string_option |> Option.value ~default:"" in
               let visible = member "visible" eff |> to_bool_option |> Option.value ~default:true in
               if visible then begin
                 match eff_type with
                 | "DROP_SHADOW" | "INNER_SHADOW" ->
                     let offset_x = member "offset" eff |> member "x" |> to_num in
                     let offset_y = member "offset" eff |> member "y" |> to_num in
                     let blur = member "radius" eff |> to_num in
                     let spread = member "spread" eff |> to_num in
                     let color = member "color" eff in
                     let r = member "r" color |> to_num in
                     let g = member "g" color |> to_num in
                     let b = member "b" color |> to_num in
                     let a = member "a" color |> to_num in
                     Hashtbl.replace effects (sprintf "shadow-%s-%d" name i) (`Assoc [
                       ("type", `String eff_type);
                       ("offsetX", `Float offset_x);
                       ("offsetY", `Float offset_y);
                       ("blur", `Float blur);
                       ("spread", `Float spread);
                       ("color", `String (rgba_to_hex r g b a));
                     ])
                 | "LAYER_BLUR" | "BACKGROUND_BLUR" ->
                     let blur = member "radius" eff |> to_num in
                     Hashtbl.replace effects (sprintf "blur-%s-%d" name i) (`Assoc [
                       ("type", `String eff_type);
                       ("radius", `Float blur);
                     ])
                 | _ -> ()
               end
             ) effs
           | _ -> ());

          (* Recurse into children *)
          (match member "children" n with
           | `List kids -> List.iter extract_from_node kids
           | _ -> ())
        in

        extract_from_node node;

        (* Build output based on format *)
        let color_list = Hashtbl.fold (fun k v acc -> (k, `String v) :: acc) colors [] in
        let typo_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) typography [] in
        let spacing_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) spacing [] in
        let radii_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) radii [] in
        let effects_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) effects [] in

        let result = match format with
          | "css-variables" ->
              (* CSS Custom Properties format *)
              let css_vars = Buffer.create 1024 in
              Buffer.add_string css_vars ":root {\n";
              List.iter (fun (k, v) ->
                match v with `String s -> Buffer.add_string css_vars (sprintf "  --%s: %s;\n" k s) | _ -> ()
              ) color_list;
              List.iter (fun (k, v) ->
                match v with `Float f -> Buffer.add_string css_vars (sprintf "  --%s: %.0fpx;\n" k f) | _ -> ()
              ) spacing_list;
              List.iter (fun (k, v) ->
                match v with `Float f -> Buffer.add_string css_vars (sprintf "  --%s: %.0fpx;\n" k f) | _ -> ()
              ) radii_list;
              Buffer.add_string css_vars "}\n";
              `Assoc [
                ("format", `String "css-variables");
                ("css", `String (Buffer.contents css_vars));
                ("tokenCount", `Int (List.length color_list + List.length typo_list + List.length spacing_list));
              ]
          | _ -> (* tokens-studio format *)
              `Assoc [
                ("format", `String "tokens-studio");
                ("tokens", `Assoc [
                  ("colors", `Assoc color_list);
                  ("typography", `Assoc typo_list);
                  ("spacing", `Assoc spacing_list);
                  ("borderRadius", `Assoc radii_list);
                  ("effects", `Assoc effects_list);
                ]);
                ("stats", `Assoc [
                  ("colors", `Int (List.length color_list));
                  ("typography", `Int (List.length typo_list));
                  ("spacing", `Int (List.length spacing_list));
                  ("borderRadius", `Int (List.length radii_list));
                  ("effects", `Int (List.length effects_list));
                  ("total", `Int (List.length color_list + List.length typo_list +
                                  List.length spacing_list + List.length radii_list + List.length effects_list));
                ]);
              ]
        in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/generate-story - Generate Storybook story from Figma node *)
let generate_story_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let figma_url = member "figmaUrl" json |> to_string_option |> Option.value ~default:"" in
        let framework = member "framework" json |> to_string_option |> Option.value ~default:"react" in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* Extract component info *)
        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in
        let safe_name = if String.length safe_name > 0 && safe_name.[0] >= '0' && safe_name.[0] <= '9'
          then "C" ^ safe_name else safe_name in

        (* Extract design tokens for controls *)
        let colors = ref [] in
        let spacings = ref [] in
        let radii = ref [] in

        let rec extract_tokens n =
          let _name = member "name" n |> to_string_option |> Option.value ~default:"" in

          (* Colors *)
          (match member "fills" n with
           | `List fills -> List.iter (fun fill ->
               match member "color" fill with
               | `Assoc c ->
                   let r = List.assoc_opt "r" c |> Option.map to_num |> Option.value ~default:0.0 in
                   let g = List.assoc_opt "g" c |> Option.map to_num |> Option.value ~default:0.0 in
                   let b = List.assoc_opt "b" c |> Option.map to_num |> Option.value ~default:0.0 in
                   let hex = sprintf "#%02x%02x%02x" (int_of_float (r *. 255.0)) (int_of_float (g *. 255.0)) (int_of_float (b *. 255.0)) in
                   if not (List.mem hex !colors) then colors := hex :: !colors
               | _ -> ()
             ) fills
           | _ -> ());

          (* Spacing *)
          let padding = member "paddingTop" n |> to_num in
          let gap = member "itemSpacing" n |> to_num in
          if padding > 0.0 && not (List.mem (int_of_float padding) !spacings) then
            spacings := (int_of_float padding) :: !spacings;
          if gap > 0.0 && not (List.mem (int_of_float gap) !spacings) then
            spacings := (int_of_float gap) :: !spacings;

          (* Border radius *)
          let radius = member "cornerRadius" n |> to_num in
          if radius > 0.0 && not (List.mem (int_of_float radius) !radii) then
            radii := (int_of_float radius) :: !radii;

          (* Recurse *)
          (match member "children" n with
           | `List kids -> List.iter extract_tokens kids
           | _ -> ())
        in
        extract_tokens node;

        (* Generate story based on framework *)
        let story_code = match framework with
          | "react" ->
              let color_options = String.concat ", " (List.map (sprintf "'%s'") !colors) in
              let spacing_options = String.concat ", " (List.map string_of_int !spacings) in
              let radius_options = String.concat ", " (List.map string_of_int !radii) in

              sprintf {|import type { Meta, StoryObj } from '@storybook/react';
import { %s } from './%s';

/**
 * %s Component
 *
 * Generated from Figma design.
 * @see %s
 */
const meta: Meta<typeof %s> = {
  title: 'Components/%s',
  component: %s,
  parameters: {
    layout: 'centered',
    design: {
      type: 'figma',
      url: '%s',
    },
  },
  tags: ['autodocs'],
  argTypes: {%s%s%s
  },
};

export default meta;
type Story = StoryObj<typeof meta>;

/**
 * Default state from Figma design
 */
export const Default: Story = {
  args: {},
};

/**
 * Interactive playground with all controls
 */
export const Playground: Story = {
  args: {},
  parameters: {
    docs: {
      canvas: { sourceState: 'shown' },
    },
  },
};
|}
                safe_name safe_name
                component_name
                (if figma_url = "" then "Figma" else figma_url)
                safe_name component_name safe_name
                figma_url
                (if !colors = [] then "" else sprintf "\n    backgroundColor: {\n      control: 'select',\n      options: [%s],\n    }," color_options)
                (if !spacings = [] then "" else sprintf "\n    padding: {\n      control: 'select',\n      options: [%s],\n    }," spacing_options)
                (if !radii = [] then "" else sprintf "\n    borderRadius: {\n      control: 'select',\n      options: [%s],\n    }," radius_options)

          | "vue" ->
              sprintf {|import type { Meta, StoryObj } from '@storybook/vue3';
import %s from './%s.vue';

const meta: Meta<typeof %s> = {
  title: 'Components/%s',
  component: %s,
  parameters: {
    design: {
      type: 'figma',
      url: '%s',
    },
  },
  tags: ['autodocs'],
};

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {
  args: {},
};
|}
                safe_name safe_name
                safe_name component_name safe_name
                figma_url

          | _ -> (* generic *)
              sprintf {|// Storybook story for %s
// Figma: %s

export default {
  title: 'Components/%s',
  parameters: {
    design: {
      type: 'figma',
      url: '%s',
    },
  },
};

export const Default = {};
|}
                component_name figma_url component_name figma_url
        in

        (* Generate component code if not provided *)
        let component_code = if code = "" then
          sprintf {|import React from 'react';

interface %sProps {
  backgroundColor?: string;
  padding?: number;
  borderRadius?: number;
  children?: React.ReactNode;
}

export const %s: React.FC<%sProps> = ({
  backgroundColor = '%s',
  padding = %d,
  borderRadius = %d,
  children,
}) => {
  return (
    <div
      style={{
        backgroundColor,
        padding,
        borderRadius,
      }}
    >
      {children}
    </div>
  );
};
|}
            safe_name safe_name safe_name
            (if !colors = [] then "#ffffff" else List.hd !colors)
            (if !spacings = [] then 16 else List.hd !spacings)
            (if !radii = [] then 8 else List.hd !radii)
        else
          code
        in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("storyFile", `String (sprintf "%s.stories.tsx" safe_name));
          ("componentFile", `String (sprintf "%s.tsx" safe_name));
          ("storyCode", `String story_code);
          ("componentCode", `String component_code);
          ("figmaUrl", `String figma_url);
          ("designTokens", `Assoc [
            ("colors", `List (List.map (fun c -> `String c) !colors));
            ("spacings", `List (List.map (fun s -> `Int s) !spacings));
            ("borderRadii", `List (List.map (fun r -> `Int r) !radii));
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/codegen-multi - Generate code for multiple platforms simultaneously *)
let codegen_multi_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let platforms = match member "platforms" json with
          | `List ps -> List.filter_map (fun p -> to_string_option p) ps
          | _ -> ["react"; "swiftui"; "compose"]  (* default all 3 *)
        in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in
        let to_int_safe json = int_of_float (to_num json) in

        (* Extract component info *)
        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in
        let safe_name = if String.length safe_name > 0 && safe_name.[0] >= '0' && safe_name.[0] <= '9'
          then "C" ^ safe_name else safe_name in

        let w = member "width" node |> to_int_safe in
        let h = member "height" node |> to_int_safe in
        let radius = member "cornerRadius" node |> to_num in
        let padding_t = member "paddingTop" node |> to_int_safe in
        let padding_r = member "paddingRight" node |> to_int_safe in
        let padding_b = member "paddingBottom" node |> to_int_safe in
        let padding_l = member "paddingLeft" node |> to_int_safe in
        let gap = member "itemSpacing" node |> to_int_safe in

        (* Extract primary colors *)
        let bg_color = match member "fills" node with
          | `List (`Assoc fill :: _) ->
              (match List.assoc_opt "color" fill with
               | Some (`Assoc c) ->
                   let r = List.assoc_opt "r" c |> Option.map to_num |> Option.value ~default:1.0 in
                   let g = List.assoc_opt "g" c |> Option.map to_num |> Option.value ~default:1.0 in
                   let b = List.assoc_opt "b" c |> Option.map to_num |> Option.value ~default:1.0 in
                   (r, g, b)
               | _ -> (1.0, 1.0, 1.0))
          | _ -> (1.0, 1.0, 1.0)
        in
        let (bg_r, bg_g, bg_b) = bg_color in
        let bg_hex = sprintf "#%02x%02x%02x" (int_of_float (bg_r *. 255.0)) (int_of_float (bg_g *. 255.0)) (int_of_float (bg_b *. 255.0)) in

        (* Check layout mode *)
        let layout_mode = member "layoutMode" node |> to_string_option |> Option.value ~default:"NONE" in
        let is_vertical = layout_mode = "VERTICAL" in
        let is_horizontal = layout_mode = "HORIZONTAL" in

        (* Generate code for each platform *)
        let generate_for_platform platform =
          let code = match platform with
            | "react" ->
                let flex_dir = if is_vertical then "column" else if is_horizontal then "row" else "column" in
                sprintf {|import React from 'react';

interface %sProps {
  children?: React.ReactNode;
}

export const %s: React.FC<%sProps> = ({ children }) => {
  return (
    <div
      style={{
        width: %d,
        height: %d,
        backgroundColor: '%s',
        borderRadius: %.0f,
        padding: '%dpx %dpx %dpx %dpx',
        display: 'flex',
        flexDirection: '%s',
        gap: %d,
      }}
    >
      {children}
    </div>
  );
};
|}
                  safe_name safe_name safe_name
                  w h bg_hex radius
                  padding_t padding_r padding_b padding_l
                  flex_dir gap

            | "swiftui" ->
                let stack_type = if is_horizontal then "HStack" else "VStack" in
                sprintf {|import SwiftUI

struct %s: View {
    var body: some View {
        %s(spacing: %d) {
            // Children go here
        }
        .frame(width: %d, height: %d)
        .padding(.top, %d)
        .padding(.trailing, %d)
        .padding(.bottom, %d)
        .padding(.leading, %d)
        .background(Color(red: %.3f, green: %.3f, blue: %.3f))
        .cornerRadius(%.0f)
    }
}

#Preview {
    %s()
}
|}
                  safe_name
                  stack_type gap
                  w h
                  padding_t padding_r padding_b padding_l
                  bg_r bg_g bg_b
                  radius
                  safe_name

            | "compose" ->
                let arrangement = if is_horizontal then "Arrangement.spacedBy" else "Arrangement.spacedBy" in
                let container = if is_horizontal then "Row" else "Column" in
                sprintf {|import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp

@Composable
fun %s(
    modifier: Modifier = Modifier,
    content: @Composable %sScope.() -> Unit = {}
) {
    %s(
        modifier = modifier
            .size(width = %d.dp, height = %d.dp)
            .clip(RoundedCornerShape(%.0f.dp))
            .background(Color(0xFF%s))
            .padding(
                top = %d.dp,
                end = %d.dp,
                bottom = %d.dp,
                start = %d.dp
            ),
        %s(%d.dp),
        content = content
    )
}

@Preview
@Composable
private fun %sPreview() {
    %s()
}
|}
                  safe_name container
                  container
                  w h radius
                  (String.sub bg_hex 1 6)  (* remove # *)
                  padding_t padding_r padding_b padding_l
                  arrangement gap
                  safe_name safe_name

            | "flutter" ->
                let container = if is_horizontal then "Row" else "Column" in
                sprintf {|import 'package:flutter/material.dart';

class %s extends StatelessWidget {
  final List<Widget> children;

  const %s({
    super.key,
    this.children = const [],
  });

  @override
  Widget build(BuildContext context) {
    return Container(
      width: %d,
      height: %d,
      padding: const EdgeInsets.fromLTRB(%d, %d, %d, %d),
      decoration: BoxDecoration(
        color: const Color(0xFF%s),
        borderRadius: BorderRadius.circular(%.0f),
      ),
      child: %s(
        mainAxisSize: MainAxisSize.min,
        spacing: %d,
        children: children,
      ),
    );
  }
}
|}
                  safe_name safe_name
                  w h
                  padding_l padding_t padding_r padding_b
                  (String.sub bg_hex 1 6)
                  radius
                  container gap

            | _ -> sprintf "// Unsupported platform: %s" platform
          in
          (platform, code)
        in

        let results = List.map generate_for_platform platforms in
        let code_assoc = List.map (fun (p, c) -> (p, `String c)) results in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("platforms", `List (List.map (fun p -> `String p) platforms));
          ("code", `Assoc code_assoc);
          ("sharedTokens", `Assoc [
            ("width", `Int w);
            ("height", `Int h);
            ("backgroundColor", `String bg_hex);
            ("borderRadius", `Float radius);
            ("padding", `Assoc [
              ("top", `Int padding_t);
              ("right", `Int padding_r);
              ("bottom", `Int padding_b);
              ("left", `Int padding_l);
            ]);
            ("gap", `Int gap);
            ("layoutMode", `String layout_mode);
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/extract-variants - Extract component variants from Figma component set *)
let extract_variants_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* Extract variant properties from component set or variants *)
        let variants = ref [] in
        let props = Hashtbl.create 16 in

        let rec extract_variants_from ?(parent_is_set=false) n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in

          (* If parent is COMPONENT_SET and child has no type, treat as COMPONENT *)
          let effective_type = if node_type = "" && parent_is_set then "COMPONENT" else node_type in

          (* Parse variant name like "State=Default, Size=Medium" *)
          if effective_type = "COMPONENT" then begin
            let parts = String.split_on_char ',' name in
            let variant_props = List.filter_map (fun part ->
              match String.split_on_char '=' (String.trim part) with
              | [key; value] ->
                  let k = String.trim key in
                  let v = String.trim value in
                  (* Collect all values for each property *)
                  let existing = try Hashtbl.find props k with Not_found -> [] in
                  if not (List.mem v existing) then Hashtbl.replace props k (v :: existing);
                  Some (k, v)
              | _ -> None
            ) parts in
            if variant_props <> [] then
              variants := (name, variant_props, n) :: !variants
          end;

          (* Recurse into children (for component sets) *)
          let is_set = effective_type = "COMPONENT_SET" in
          (match member "children" n with
           | `List kids -> List.iter (extract_variants_from ~parent_is_set:is_set) kids
           | _ -> ())
        in
        extract_variants_from ~parent_is_set:false node;

        (* Generate TypeScript types from variants *)
        let prop_types = Hashtbl.fold (fun key values acc ->
          let union = String.concat " | " (List.map (sprintf "'%s'") (List.rev values)) in
          sprintf "  %s: %s;" key union :: acc
        ) props [] in

        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in

        let ts_types = sprintf {|interface %sProps {
%s
}

type %sVariant = {
  name: string;
  props: Partial<%sProps>;
};
|} safe_name (String.concat "\n" prop_types) safe_name safe_name in

        (* Build variant mapping *)
        let variant_list = List.map (fun (name, vprops, _n) ->
          let props_json = List.map (fun (k, v) -> (k, `String v)) vprops in
          `Assoc [("name", `String name); ("props", `Assoc props_json)]
        ) !variants in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("variantCount", `Int (List.length !variants));
          ("properties", `Assoc (Hashtbl.fold (fun k v acc ->
            (k, `List (List.map (fun x -> `String x) (List.rev v))) :: acc
          ) props []));
          ("variants", `List variant_list);
          ("typescript", `String ts_types);
        ] in
        let _ = to_num in (* suppress warning *)
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/responsive-breakpoints - Generate responsive code with breakpoints *)
let responsive_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let breakpoints = match member "breakpoints" json with
          | `List bps -> List.filter_map (fun bp ->
              match (member "name" bp |> to_string_option, member "width" bp |> to_int_option) with
              | (Some n, Some w) -> Some (n, w)
              | _ -> None
            ) bps
          | _ -> [("mobile", 375); ("tablet", 768); ("desktop", 1440)]
        in

        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in
        let to_int_safe json = int_of_float (to_num json) in

        let component_name = member "name" node |> to_string_option |> Option.value ~default:"Component" in
        let safe_name = String.map (fun c ->
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_'
        ) component_name in

        let base_w = member "width" node |> to_int_safe in
        let base_h = member "height" node |> to_int_safe in
        let radius = member "cornerRadius" node |> to_num in
        let padding = member "paddingTop" node |> to_int_safe in

        (* Generate CSS with media queries *)
        let css_code = sprintf {|.%s {
  width: %dpx;
  height: %dpx;
  border-radius: %.0fpx;
  padding: %dpx;
}

%s
|}
          safe_name base_w base_h radius padding
          (String.concat "\n\n" (List.map (fun (bp_name, bp_width) ->
            let scale = float_of_int bp_width /. float_of_int base_w in
            sprintf {|/* %s (%dpx) */
@media (max-width: %dpx) {
  .%s {
    width: %dpx;
    height: %dpx;
    border-radius: %.0fpx;
    padding: %dpx;
  }
}|}
              bp_name bp_width bp_width
              safe_name
              bp_width
              (int_of_float (float_of_int base_h *. scale))
              (radius *. scale)
              (int_of_float (float_of_int padding *. scale))
          ) breakpoints))
        in

        (* Generate Tailwind classes *)
        let tailwind = sprintf "%s w-[%dpx] h-[%dpx] rounded-[%.0fpx] p-[%dpx] %s"
          safe_name base_w base_h radius padding
          (String.concat " " (List.map (fun (bp_name, bp_width) ->
            let scale = float_of_int bp_width /. float_of_int base_w in
            sprintf "%s:w-[%dpx] %s:h-[%dpx]"
              bp_name bp_width
              bp_name (int_of_float (float_of_int base_h *. scale))
          ) breakpoints))
        in

        let result = `Assoc [
          ("componentName", `String safe_name);
          ("baseWidth", `Int base_w);
          ("breakpoints", `List (List.map (fun (n, w) -> `Assoc [("name", `String n); ("width", `Int w)]) breakpoints));
          ("css", `String css_code);
          ("tailwind", `String tailwind);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/accessibility - Generate accessibility attributes *)
let accessibility_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in

        let suggestions = ref [] in
        let aria_attrs = ref [] in

        let rec analyze_accessibility n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in
          let ln = String.lowercase_ascii name in

          (* Helper to check if name contains keyword (anywhere, not just prefix) *)
          let contains_word s word =
            let re = Str.regexp_string word in
            try ignore (Str.search_forward re s 0); true
            with Not_found -> false
          in

          (* Detect interactive elements - check anywhere in name *)
          let is_button = contains_word ln "button" || contains_word ln "btn" || contains_word ln "cta" ||
                          contains_word ln "submit" || contains_word ln "cancel" || contains_word ln "action" in
          let is_link = contains_word ln "link" || contains_word ln "anchor" in
          let is_input = contains_word ln "input" || contains_word ln "search" || contains_word ln "field" ||
                         contains_word ln "text" || contains_word ln "email" || contains_word ln "password" in
          let is_image = contains_word ln "image" || contains_word ln "icon" || contains_word ln "avatar" ||
                         contains_word ln "photo" || contains_word ln "picture" || contains_word ln "img" in
          let is_nav = contains_word ln "nav" || contains_word ln "menu" || contains_word ln "sidebar" ||
                       contains_word ln "header" || contains_word ln "footer" in
          let is_modal = contains_word ln "modal" || contains_word ln "dialog" || contains_word ln "popup" ||
                         contains_word ln "overlay" || contains_word ln "drawer" in

          if is_button then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "button");
              ("aria-label", `String name);
              ("tabIndex", `Int 0);
            ] :: !aria_attrs;
            suggestions := sprintf "Button '%s': Add aria-label and ensure keyboard accessibility" name :: !suggestions
          end;

          if is_link then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "link");
              ("href", `String "#");
            ] :: !aria_attrs
          end;

          if is_input then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "textbox");
              ("aria-label", `String name);
              ("aria-required", `Bool false);
            ] :: !aria_attrs;
            suggestions := sprintf "Input '%s': Add label element or aria-label" name :: !suggestions
          end;

          if is_image then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "img");
              ("alt", `String name);
            ] :: !aria_attrs;
            suggestions := sprintf "Image '%s': Ensure alt text is descriptive" name :: !suggestions
          end;

          if is_nav then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "navigation");
              ("aria-label", `String name);
            ] :: !aria_attrs
          end;

          if is_modal then begin
            aria_attrs := `Assoc [
              ("element", `String name);
              ("role", `String "dialog");
              ("aria-modal", `Bool true);
              ("aria-labelledby", `String (name ^ "-title"));
            ] :: !aria_attrs;
            suggestions := sprintf "Modal '%s': Implement focus trap and escape key handling" name :: !suggestions
          end;

          (* Check for text contrast (simplified) *)
          if node_type = "TEXT" then begin
            let chars = member "text" n |> member "characters" |> to_string_option |> Option.value ~default:"" in
            if String.length chars > 0 then
              suggestions := sprintf "Text '%s': Verify color contrast meets WCAG AA (4.5:1)" name :: !suggestions
          end;

          (* Recurse *)
          (match member "children" n with
           | `List kids -> List.iter analyze_accessibility kids
           | _ -> ())
        in
        analyze_accessibility node;

        let result = `Assoc [
          ("ariaAttributes", `List (List.rev !aria_attrs));
          ("suggestions", `List (List.map (fun s -> `String s) (List.rev !suggestions)));
          ("summary", `Assoc [
            ("interactiveElements", `Int (List.length !aria_attrs));
            ("suggestions", `Int (List.length !suggestions));
          ]);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/export-assets - Export SVG/PNG assets from node *)
let export_assets_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let formats = match member "formats" json with
          | `List fs -> List.filter_map to_string_option fs
          | _ -> ["svg"; "png@1x"; "png@2x"]
        in

        (* Collect exportable assets (icons, images, logos) *)
        let assets = ref [] in

        let rec find_assets n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in
          let node_type = member "type" n |> to_string_option |> Option.value ~default:"" in
          let node_id = member "id" n |> to_string_option |> Option.value ~default:"" in
          let ln = String.lowercase_ascii name in

          let is_icon = String.length ln >= 4 && String.sub ln 0 4 = "icon" in
          let is_logo = String.length ln >= 4 && String.sub ln 0 4 = "logo" in
          let is_image = node_type = "VECTOR" || node_type = "ELLIPSE" ||
                         (node_type = "FRAME" && (is_icon || is_logo)) in

          if is_image || is_icon || is_logo then begin
            let export_settings = List.map (fun fmt ->
              let (format, scale) = match String.split_on_char '@' fmt with
                | [f; s] -> (f, (try float_of_string (String.sub s 0 (String.length s - 1)) with _ -> 1.0))
                | _ -> (fmt, 1.0)
              in
              `Assoc [
                ("format", `String format);
                ("scale", `Float scale);
                ("filename", `String (sprintf "%s%s.%s" name (if scale > 1.0 then sprintf "@%.0fx" scale else "") format));
              ]
            ) formats in
            assets := `Assoc [
              ("name", `String name);
              ("nodeId", `String node_id);
              ("type", `String node_type);
              ("exports", `List export_settings);
            ] :: !assets
          end;

          (match member "children" n with
           | `List kids -> List.iter find_assets kids
           | _ -> ())
        in
        find_assets node;

        let result = `Assoc [
          ("assets", `List (List.rev !assets));
          ("formats", `List (List.map (fun f -> `String f) formats));
          ("totalAssets", `Int (List.length !assets));
          ("note", `String "Use Figma API /images endpoint with nodeId to download actual files");
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /plugin/extract-animations - Extract animations and generate CSS/SwiftUI *)
let extract_animations_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let node = member "node" json in
        let to_num json = match json with `Float f -> f | `Int i -> float_of_int i | _ -> 0.0 in

        (* Extract transition/animation properties from prototype interactions *)
        let animations = ref [] in

        let rec extract_anims n =
          let name = member "name" n |> to_string_option |> Option.value ~default:"" in

          (* Safe member access for nested JSON *)
          let safe_member_type obj =
            match obj with
            | `Null -> ""
            | _ -> member "type" obj |> to_string_option |> Option.value ~default:""
          in

          (* Check for Figma prototype interactions *)
          (match member "reactions" n with
           | `List reactions -> List.iter (fun reaction ->
               let trigger_obj = member "trigger" reaction in
               let trigger = safe_member_type trigger_obj in
               let action = member "action" reaction in
               let anim_type = safe_member_type action in

               if anim_type = "NODE" then begin
                 let transition = member "transition" action in
                 let trans_type = match transition with
                   | `Null -> "DISSOLVE"
                   | _ -> member "type" transition |> to_string_option |> Option.value ~default:"DISSOLVE"
                 in
                 let duration = match transition with
                   | `Null -> 0.3
                   | _ -> member "duration" transition |> to_num
                 in
                 let easing_obj = match transition with
                   | `Null -> `Null
                   | _ -> member "easing" transition
                 in
                 let easing = safe_member_type easing_obj in
                 let easing = if easing = "" then "EASE_OUT" else easing in

                 animations := `Assoc [
                   ("element", `String name);
                   ("trigger", `String trigger);
                   ("type", `String trans_type);
                   ("duration", `Float duration);
                   ("easing", `String easing);
                 ] :: !animations
               end
             ) reactions
           | _ -> ());

          (* Also check for smart animate properties *)
          let opacity = member "opacity" n |> to_num in
          let rotation = member "rotation" n |> to_num in
          if opacity < 1.0 || rotation <> 0.0 then begin
            animations := `Assoc [
              ("element", `String name);
              ("trigger", `String "state_change");
              ("properties", `Assoc [
                ("opacity", `Float opacity);
                ("rotation", `Float rotation);
              ]);
            ] :: !animations
          end;

          (match member "children" n with
           | `List kids -> List.iter extract_anims kids
           | _ -> ())
        in
        extract_anims node;

        (* Generate CSS keyframes based on actual transition type *)
        let safe_num key obj = match obj with `Null -> 0.0 | _ -> member key obj |> to_num in
        let generate_css_keyframes trans_type props =
          let opacity_from = safe_num "opacity" props in
          let opacity_to = if opacity_from < 1.0 then 1.0 else opacity_from in
          let rotation = safe_num "rotation" props in
          match trans_type with
          | "DISSOLVE" ->
              sprintf "from { opacity: %.2f; }\n  to { opacity: %.2f; }" opacity_from opacity_to
          | "MOVE_IN" | "SLIDE_IN" ->
              sprintf "from { opacity: 0; transform: translateX(-100%%); }\n  to { opacity: 1; transform: translateX(0); }"
          | "MOVE_OUT" | "SLIDE_OUT" ->
              sprintf "from { opacity: 1; transform: translateX(0); }\n  to { opacity: 0; transform: translateX(100%%); }"
          | "PUSH" ->
              sprintf "from { transform: scale(0.8); opacity: 0; }\n  to { transform: scale(1); opacity: 1; }"
          | "SMART_ANIMATE" ->
              let rot_css = if rotation <> 0.0 then sprintf " rotate(%.1fdeg)" rotation else "" in
              sprintf "from { opacity: %.2f; transform: translateY(20px)%s; }\n  to { opacity: 1; transform: translateY(0)%s; }"
                opacity_from rot_css rot_css
          | _ -> (* Default fade-in *)
              sprintf "from { opacity: %.2f; }\n  to { opacity: 1; }" (min opacity_from 0.0)
        in

        let css_animations = String.concat "\n\n" (List.mapi (fun i anim ->
          let name = member "element" anim |> to_string_option |> Option.value ~default:"element" in
          let duration = member "duration" anim |> to_num in
          let dur = if duration > 0.0 then duration else 0.3 in
          let easing = member "easing" anim |> to_string_option |> Option.value ~default:"EASE_OUT" in
          let trans_type = member "type" anim |> to_string_option |> Option.value ~default:"DISSOLVE" in
          let props = member "properties" anim in
          let css_easing = match easing with
            | "EASE_IN" -> "ease-in"
            | "EASE_OUT" -> "ease-out"
            | "EASE_IN_AND_OUT" -> "ease-in-out"
            | "LINEAR" -> "linear"
            | "CUSTOM_BEZIER" -> "cubic-bezier(0.4, 0, 0.2, 1)"
            | _ -> "ease-out"
          in
          let keyframes = generate_css_keyframes trans_type props in
          sprintf {|/* Animation %d: %s (type: %s) */
@keyframes %s_anim {
  %s
}

.%s {
  animation: %s_anim %.2fs %s forwards;
}|} i name trans_type name keyframes name name dur css_easing
        ) !animations) in

        (* Generate SwiftUI animations based on actual transition type *)
        let generate_swift_modifiers trans_type props =
          let opacity_from = safe_num "opacity" props in
          let rotation = safe_num "rotation" props in
          match trans_type with
          | "DISSOLVE" ->
              sprintf ".opacity(isAnimating ? 1 : %.2f)" opacity_from
          | "MOVE_IN" | "SLIDE_IN" ->
              ".opacity(isAnimating ? 1 : 0)\n.offset(x: isAnimating ? 0 : -UIScreen.main.bounds.width)"
          | "MOVE_OUT" | "SLIDE_OUT" ->
              ".opacity(isAnimating ? 0 : 1)\n.offset(x: isAnimating ? UIScreen.main.bounds.width : 0)"
          | "PUSH" ->
              ".scaleEffect(isAnimating ? 1 : 0.8)\n.opacity(isAnimating ? 1 : 0)"
          | "SMART_ANIMATE" ->
              let rot_mod = if rotation <> 0.0 then sprintf "\n.rotationEffect(.degrees(isAnimating ? 0 : %.1f))" rotation else "" in
              sprintf ".opacity(isAnimating ? 1 : %.2f)\n.offset(y: isAnimating ? 0 : 20)%s" opacity_from rot_mod
          | _ ->
              sprintf ".opacity(isAnimating ? 1 : %.2f)" (min opacity_from 0.0)
        in

        let swiftui_animations = String.concat "\n\n" (List.mapi (fun i anim ->
          let name = member "element" anim |> to_string_option |> Option.value ~default:"element" in
          let duration = member "duration" anim |> to_num in
          let dur = if duration > 0.0 then duration else 0.3 in
          let easing = member "easing" anim |> to_string_option |> Option.value ~default:"EASE_OUT" in
          let trans_type = member "type" anim |> to_string_option |> Option.value ~default:"DISSOLVE" in
          let props = member "properties" anim in
          let swift_easing = match easing with
            | "EASE_IN" -> ".easeIn"
            | "EASE_OUT" -> ".easeOut"
            | "EASE_IN_AND_OUT" -> ".easeInOut"
            | "LINEAR" -> ".linear"
            | "CUSTOM_BEZIER" -> ".timingCurve(0.4, 0, 0.2, 1)"
            | _ -> ".easeOut"
          in
          let modifiers = generate_swift_modifiers trans_type props in
          sprintf {|// Animation %d: %s (type: %s)
%s
.animation(%s(duration: %.2f), value: isAnimating)|} i name trans_type modifiers swift_easing dur
        ) !animations) in

        let result = `Assoc [
          ("animations", `List (List.rev !animations));
          ("count", `Int (List.length !animations));
          ("css", `String css_animations);
          ("swiftui", `String swiftui_animations);
        ] in
        Response.json (Yojson.Safe.to_string result) reqd
  )

(** POST /webhook/figma - Figma webhook handler for design changes *)
let constant_time_equal a b =
  let a = String.trim a in
  let b = String.trim b in
  let la = String.length a in
  let lb = String.length b in
  if la <> lb then false
  else
    let diff = ref 0 in
    for i = 0 to la - 1 do
      diff := !diff lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !diff = 0

let webhook_passcode_secret_opt () =
  let nonempty name =
    match Sys.getenv_opt name with
    | Some v ->
        let v = String.trim v in
        if v = "" then None else Some v
    | None -> None
  in
  match nonempty "FIGMA_MCP_WEBHOOK_PASSCODE" with
  | Some _ as v -> v
  | None -> nonempty "FIGMA_WEBHOOK_PASSCODE"

let validate_webhook_passcode ~allow_no_auth ~secret_opt ~passcode =
  match secret_opt with
  | None ->
      if allow_no_auth then
        Error "Webhook passcode required when no-auth mode is enabled (set FIGMA_MCP_WEBHOOK_PASSCODE)"
      else
        Ok ()
  | Some secret ->
      let passcode = String.trim passcode in
      if passcode = "" then Error "Missing webhook passcode"
      else if constant_time_equal secret passcode then Ok ()
      else Error "Invalid webhook passcode"

let webhook_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in

        (* Parse Figma webhook payload *)
        let event_type = member "event_type" json |> to_string_option |> Option.value ~default:"" in
        let file_key = member "file_key" json |> to_string_option |> Option.value ~default:"" in
        let file_name = member "file_name" json |> to_string_option |> Option.value ~default:"" in
        let timestamp = member "timestamp" json |> to_string_option |> Option.value ~default:"" in
        let passcode = member "passcode" json |> to_string_option |> Option.value ~default:"" in

        let secret_opt = webhook_passcode_secret_opt () in
        (match validate_webhook_passcode ~allow_no_auth:!allow_no_auth ~secret_opt ~passcode with
         | Error err ->
             let body = `Assoc [("error", `String err)] in
             Response.json ~status:`Forbidden (Yojson.Safe.to_string body) reqd
         | Ok () ->

             (* Log the webhook event *)
             printf "[Webhook] %s: file=%s (%s) at %s\n%!" event_type file_key file_name timestamp;

        (* Determine action based on event type *)
        let action = match event_type with
          | "FILE_UPDATE" -> "regenerate_code"
          | "FILE_VERSION_UPDATE" -> "sync_version"
          | "FILE_DELETE" -> "archive_code"
          | "LIBRARY_PUBLISH" -> "update_tokens"
          | _ -> "unknown"
        in

        (* Build response with recommended action *)
        let result = `Assoc [
          ("status", `String "received");
          ("event_type", `String event_type);
          ("file_key", `String file_key);
          ("file_name", `String file_name);
          ("timestamp", `String timestamp);
          ("recommended_action", `String action);
          ("webhook_endpoints", `Assoc [
            ("codegen", `String "/plugin/codegen");
            ("tokens", `String "/plugin/extract-tokens");
            ("variants", `String "/plugin/extract-variants");
          ]);
        ] in
             Response.json (Yojson.Safe.to_string result) reqd)
  )

(** POST /plugin/code-to-figma - Convert code to Figma DSL *)
let code_to_figma_handler ~sw ~eio_ctx _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let target_name = member "name" json |> to_string_option |> Option.value ~default:"GeneratedComponent" in

        if String.length code < 10 then begin
          json_error "Code too short" reqd
        end else begin
          (* Build prompt for code → Figma DSL conversion *)
          let prompt = sprintf {|Analyze this React component and generate Figma node creation instructions.

CODE:
%s

Generate a JSON array of Figma node creation operations. Each operation should have:
- "action": one of "create_frame", "create_rectangle", "create_text", "create_ellipse"
- "name": node name
- "width", "height": dimensions in pixels
- "x", "y": position relative to parent
- "fills": array of {type: "SOLID", color: "#hexcolor"}
- "cornerRadius": optional, for rounded corners
- "children": nested operations array
- For text: "text", "fontSize", "fontWeight"

Output ONLY valid JSON array, no explanation. Example:
[
  {"action": "create_frame", "name": "Card", "width": 270, "height": 120, "fills": [{"type": "SOLID", "color": "#ffffff"}], "cornerRadius": 16, "children": [
    {"action": "create_text", "name": "Title", "x": 24, "y": 20, "text": "Hello", "fontSize": 16, "fills": [{"type": "SOLID", "color": "#1a1a26"}]}
  ]}
]|} code
          in

          (* Try LLM - Ollama for now *)
          let send_error msg =
            let result = `Assoc [("error", `String msg)] in
            Response.json (Yojson.Safe.to_string result) reqd
          in

          (try
            let cohttp = Figma_api_eio.get_cohttp_client eio_ctx.Mcp_tools.client in
            let ollama_url = "http://127.0.0.1:11434/api/generate" in
            let ollama_body = `Assoc [
              ("model", `String "qwen3-coder:30b");
              ("prompt", `String prompt);
              ("stream", `Bool false);
            ] in
            let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
            let req_body = Cohttp_eio.Body.of_string (Yojson.Safe.to_string ollama_body) in
            let uri = Uri.of_string ollama_url in
            let resp, resp_body = Cohttp_eio.Client.post cohttp ~sw ~headers ~body:req_body uri in
            let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
            if status_code < 200 || status_code >= 300 then
              send_error "LLM request failed"
            else begin
              let ollama_resp_str = Eio.Buf_read.(parse_exn take_all) resp_body ~max_size:(10 * 1024 * 1024) in
              let ollama_resp = Yojson.Safe.from_string ollama_resp_str in
              let llm_output = member "response" ollama_resp |> to_string_option |> Option.value ~default:"[]" in
              (* Extract JSON from response (might have markdown code blocks) *)
              let json_str =
                if String.length llm_output > 0 && llm_output.[0] = '[' then llm_output
                else
                  let re = Str.regexp {|\[\(.*\)\]|} in
                  if Str.string_match re llm_output 0 then Str.matched_string llm_output
                  else "[]"
              in
              let result = `Assoc [
                ("operations", Yojson.Safe.from_string json_str);
                ("name", `String target_name);
                ("source", `String "ollama")
              ] in
              Response.json (Yojson.Safe.to_string result) reqd
            end
          with exn ->
            send_error (sprintf "Error: %s" (Printexc.to_string exn)))
        end
  )

(** ============== Vision Compare Safety ============== *)

let has_suffix ~suffix s =
  let ls = String.length s in
  let l = String.length suffix in
  ls >= l && String.sub s (ls - l) l = suffix

let trim s = String.trim s

let split_csv s =
  s
  |> String.split_on_char ','
  |> List.map trim
  |> List.filter (fun x -> x <> "")

let normalize_dir_prefix path =
  let p = trim path in
  if p = "" then None
  else
    let rp = try Unix.realpath p with exn ->
      Printf.eprintf "[mcp_protocol] Warning: realpath failed for '%s': %s, using original\n%!" p (Printexc.to_string exn);
      p
    in
    if rp = "/" then Some rp
    else if has_suffix ~suffix:"/" rp then Some rp
    else Some (rp ^ "/")

let is_under_dir ~dir_prefix path =
  if dir_prefix = "/" then true
  else
    let lp = String.length dir_prefix in
    String.length path >= lp && String.sub path 0 lp = dir_prefix

let validate_reference_image_path ~roots ~max_bytes path : (string, string) result =
  if trim path = "" then
    Error "reference path required"
  else if not (Sys.file_exists path) then
    Error "Reference image not found"
  else
    let lower = String.lowercase_ascii path in
    if not (has_suffix ~suffix:".png" lower) then
      Error "Reference must be a .png file"
    else
      let st =
        try Ok (Unix.stat path)
        with Unix.Unix_error (e, _, _) ->
          Error (Printf.sprintf "Failed to stat reference image: %s" (Unix.error_message e))
      in
      match st with
      | Error e -> Error e
      | Ok st ->
          if st.Unix.st_kind <> Unix.S_REG then
            Error "Reference must be a regular file"
          else if st.Unix.st_size > max_bytes then
            Error "Reference image too large"
          else
            let rp =
              try Ok (Unix.realpath path)
              with _ -> Error "Failed to resolve reference image path"
            in
            match rp with
            | Error e -> Error e
            | Ok rp ->
                let prefixes =
                  roots
                  |> List.filter_map normalize_dir_prefix
                in
                if prefixes = [] then Ok rp
                else if List.exists (fun dir_prefix -> is_under_dir ~dir_prefix rp) prefixes then Ok rp
                else Error "Reference path not allowed (set FIGMA_MCP_VISION_REFERENCE_ROOTS)"

(** POST /plugin/vision-compare - Compare Figma export with rendered code *)
let vision_compare_handler ~sw:_ ~eio_ctx:_ _request reqd =
  Request.read_body_async reqd (fun body_str ->
    match parse_json body_str with
    | Error msg -> json_error msg reqd
    | Ok json ->
        let open Yojson.Safe.Util in
        let reference_path = member "reference" json |> to_string_option |> Option.value ~default:"" in
        let code = member "code" json |> to_string_option |> Option.value ~default:"" in
        let width = member "width" json |> to_int_option |> Option.value ~default:375 in
        let height = member "height" json |> to_int_option |> Option.value ~default:812 in
        let threshold = member "threshold" json |> to_float_option |> Option.value ~default:0.95 in

        if reference_path = "" then begin
          json_error "reference path required" reqd
        end else if code = "" then begin
          json_error "code required" reqd
        end else begin
          let roots =
            match Sys.getenv_opt "FIGMA_MCP_VISION_REFERENCE_ROOTS" with
            | None -> []
            | Some v -> split_csv v
          in
          let max_bytes =
            env_int ~name:"FIGMA_MCP_VISION_REFERENCE_MAX_BYTES" ~default:(50 * 1024 * 1024)
          in
          match validate_reference_image_path ~roots ~max_bytes reference_path with
          | Error err ->
              let result = `Assoc [
                ("error", `String err);
                ("hint", `String "Configure FIGMA_MCP_VISION_REFERENCE_ROOTS to restrict allowed reference paths.");
              ] in
              Response.json ~status:`Bad_request (Yojson.Safe.to_string result) reqd
          | Ok reference_path ->
          (* Wrap code in HTML boilerplate *)
          let html_content = sprintf {|<!DOCTYPE html>
<html><head>
<meta charset="UTF-8">
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: 'Inter', -apple-system, sans-serif; }
</style>
</head><body>
<div id="root">%s</div>
</body></html>|} code
          in
          (* Render HTML -> PNG (Playwright) *)
          match Visual_verifier.render_html_to_png ~width ~height html_content with
          | Error err ->
              let result = `Assoc [
                ("error", `String ("Render failed: " ^ err));
                ("reference", `String reference_path);
                ("hint", `String "Ensure Node + Playwright deps are installed under ./scripts (npm ci) and try again.");
              ] in
              Response.json ~status:`Internal_server_error (Yojson.Safe.to_string result) reqd
          | Ok rendered_path ->
              (* Compare with SSIM + region analysis (Node script fallback if needed) *)
              (match Visual_verifier.compare_renders_with_regions ~figma_png:reference_path ~html_png:rendered_path with
               | Error err ->
                   let result = `Assoc [
                     ("error", `String ("SSIM comparison failed: " ^ err));
                     ("reference", `String reference_path);
                     ("rendered", `String rendered_path);
                   ] in
                   Response.json ~status:`Internal_server_error (Yojson.Safe.to_string result) reqd
               | Ok metrics ->
                   let ssim_score = metrics.ssim in
                   let delta_e = metrics.delta_e in
                   let human_ssim = Visual_verifier.calculate_human_ssim ssim_score delta_e in
                   let passed = ssim_score >= threshold in
                   if passed then (try Sys.remove rendered_path with _ -> ());
                   let advanced_json =
                     match metrics.advanced with
                     | None -> `Null
                     | Some adv ->
                         `Assoc [
                           ("true_ssim", `Float adv.true_ssim);
                           ("ms_ssim", `Float adv.ms_ssim);
                           ("pixel_match", `Float adv.pixel_match);
                           ("lpips", (match adv.lpips with Some v -> `Float v | None -> `Null));
                         ]
                   in
                   let result = `Assoc [
                     ("ssim", `Float ssim_score);
                     ("delta_e", `Float delta_e);
                     ("human_ssim", `Float human_ssim);
                     ("threshold", `Float threshold);
                     ("pass", `Bool passed);
                     ("reference", `String reference_path);
                     ("rendered", `String (if passed then "(cleaned up)" else rendered_path));
                     ("regions", `Assoc [
                       ("quadrants", `Assoc [
                         ("top_left", `Float metrics.regions.quadrants.top_left);
                         ("top_right", `Float metrics.regions.quadrants.top_right);
                         ("bottom_left", `Float metrics.regions.quadrants.bottom_left);
                         ("bottom_right", `Float metrics.regions.quadrants.bottom_right);
                       ]);
                       ("strips", `Assoc [
                         ("top", `Float metrics.regions.strips.strip_top);
                         ("middle", `Float metrics.regions.strips.strip_middle);
                         ("bottom", `Float metrics.regions.strips.strip_bottom);
                       ]);
                       ("edges", `Assoc [
                         ("top", `Float metrics.regions.edges.edge_top);
                         ("bottom", `Float metrics.regions.edges.edge_bottom);
                         ("left", `Float metrics.regions.edges.edge_left);
                         ("right", `Float metrics.regions.edges.edge_right);
                       ]);
                     ]);
	                     ("advanced", advanced_json);
	                   ] in
	                   Response.json (Yojson.Safe.to_string result) reqd)
        end
  )

(** ============== Router ============== *)

let is_public_path meth path =
  match (meth, path) with
  | (`OPTIONS, _) -> true
  | (`GET, "/health") -> true
  | _ -> false

let normalize_env value =
  match value with
  | None -> None
  | Some v ->
      let trimmed = String.trim v in
      if trimmed = "" then None else Some trimmed

let api_key_env_name () =
  match normalize_env (Sys.getenv_opt "FIGMA_MCP_API_KEY") with
  | Some _ -> "FIGMA_MCP_API_KEY"
  | None ->
      (match normalize_env (Sys.getenv_opt "MCP_API_KEY") with
       | Some _ -> "MCP_API_KEY"
       | None -> "FIGMA_MCP_API_KEY")

let check_api_key request =
  let env_name = api_key_env_name () in
  match Mcp_http_auth.check_api_key
          ~env_name
          ~allow_no_auth:!allow_no_auth
          request.Httpun.Request.headers with
  | Ok () -> Ok ()
  | Error Mcp_http_auth.Missing -> Error "API key required"
  | Error Mcp_http_auth.Invalid -> Error "Invalid API key"

let route_request ~clock ~domain_mgr ~sw ~eio_ctx server request reqd =
  let path = Request.path request in
  let meth = Request.method_ request in
  let public_path = is_public_path meth path in

  if not (Cors.is_allowed reqd) then
    Response.text ~status:`Forbidden "Forbidden" reqd
  else
    let route () =
      match (meth, path) with
      | `OPTIONS, _ ->
          Response.cors_preflight reqd

      | `GET, "/health" ->
          health_handler request reqd

      | `GET, "/metrics" ->
          Response.text (Server_metrics.to_prometheus_text ()) reqd

      | `GET, "/stats" ->
          let result = `Assoc [
            ("server_metrics", Server_metrics.to_json ());
            ("agent_queue", agent_queue_stats_json ());
          ] in
          Response.json (Yojson.Safe.to_string result) reqd

      | `GET, "/" ->
          Response.text (sprintf "🎨 %s MCP Server (Eio)" Mcp_protocol.server_name) reqd

      | `GET, "/mcp" ->
          (* SSE stream for MCP streamable-http protocol *)
          mcp_sse_handler ~clock request reqd

      | `GET, "/plugin/status" ->
          plugin_status_handler request reqd

      | `POST, "/" | `POST, "/mcp" ->
          mcp_post_handler ~sw ~domain_mgr ~eio_ctx server request reqd

      | `POST, "/plugin/connect" ->
          plugin_connect_handler request reqd

      | `POST, "/plugin/poll" ->
          plugin_poll_handler ~clock request reqd

      | `POST, "/plugin/result" ->
          plugin_result_handler request reqd

      | `POST, "/plugin/event" ->
          plugin_event_handler request reqd

      | `POST, "/plugin/codegen" ->
          plugin_codegen_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/template" ->
          template_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/code-to-figma" ->
          code_to_figma_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/vision-compare" ->
          vision_compare_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/analyze" ->
          plugin_analyze_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/extract-tokens" ->
          extract_tokens_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/generate-story" ->
          generate_story_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/codegen-multi" ->
          codegen_multi_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/extract-variants" ->
          extract_variants_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/responsive-breakpoints" ->
          responsive_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/accessibility" ->
          accessibility_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/export-assets" ->
          export_assets_handler ~sw ~eio_ctx request reqd

      | `POST, "/plugin/extract-animations" ->
          extract_animations_handler ~sw ~eio_ctx request reqd

      | `POST, "/webhook/figma" ->
          webhook_handler ~sw ~eio_ctx request reqd

      (* Agent Queue endpoints *)
      | `POST, "/agent/request" ->
          agent_request_handler request reqd

      | `POST, "/agent/claim" ->
          agent_claim_handler request reqd

      | `POST, "/agent/heartbeat" ->
          agent_heartbeat_handler request reqd

      | `POST, "/agent/abandon" ->
          agent_abandon_handler request reqd

      | `GET, "/agent/pending" ->
          agent_pending_handler request reqd

      | `POST, "/agent/result" ->
          agent_result_handler request reqd

      | `GET, path when String.length path > 14 && String.sub path 0 14 = "/agent/status/" ->
          agent_status_handler request reqd

      | `GET, "/agent/queue" ->
          agent_queue_handler request reqd

      | _ ->
          Response.not_found reqd
    in
    if public_path then
      route ()
    else
      match check_api_key request with
      | Ok () -> route ()
      | Error msg -> Response.api_key_error msg reqd

(** ============== httpun-eio Server ============== *)

let make_request_handler ~clock ~domain_mgr ~sw ~eio_ctx server =
  fun _client_addr gluten_reqd ->
    let reqd = gluten_reqd.Gluten.Reqd.reqd in
    let request = Httpun.Reqd.request reqd in
    Server_metrics.register_reqd reqd request;
    try
      route_request ~clock ~domain_mgr ~sw ~eio_ctx server request reqd
    with exn ->
      eprintf "[http] request handler exception: %s\n%!" (Printexc.to_string exn);
      Response.text ~status:`Internal_server_error "Internal Server Error" reqd

let error_handler _client_addr ?request error start_response =
  let status =
    match error with
    | `Bad_request -> `Bad_request
    | `Bad_gateway -> `Bad_gateway
    | `Internal_server_error -> `Internal_server_error
    | `Exn _ -> `Internal_server_error
  in
  let msg =
    match error with
    | `Exn exn ->
        eprintf "[http] error handler exception: %s\n%!" (Printexc.to_string exn);
        "Internal Server Error"
    | `Bad_request -> "Bad Request"
    | `Bad_gateway -> "Bad Gateway"
    | `Internal_server_error -> "Internal Server Error"
  in
  let origin_opt =
    match request with
    | None -> None
    | Some req -> Httpun.Headers.get req.Httpun.Request.headers "origin"
  in
  let cors_headers =
    Cors.headers_for_origin_opt origin_opt ~include_methods:true ~include_headers:true
  in
  let headers = Httpun.Headers.of_list ([
    ("content-type", "text/plain; charset=utf-8");
    ("content-length", string_of_int (String.length msg));
    ("connection", "close");
  ] @ cors_headers) in
  let response_body = start_response headers in
  Httpun.Body.Writer.write_string response_body msg;
  Httpun.Body.Writer.close response_body;
  Server_metrics.record_untracked_response ~bytes:(String.length msg) status

(** Run HTTP server with Eio *)
let run ~sw ~net ~clock ~domain_mgr config server =
  (* Set Eio context for pure Eio handlers (Lwt-free path) *)
  let eio_client = Figma_api_eio.make_client net in
  let eio_ctx = Mcp_tools.set_eio_context ~sw ~net ~clock ~client:eio_client in
  let request_handler = make_request_handler ~clock ~domain_mgr ~sw ~eio_ctx server in
  let resolve_listen_ips host =
    match String.lowercase_ascii host with
    | "localhost" ->
        [Eio.Net.Ipaddr.V4.loopback; Eio.Net.Ipaddr.V6.loopback]
    | _ ->
        (match Ipaddr.of_string host with
         | Ok addr -> [Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets addr)]
         | Error _ -> [Eio.Net.Ipaddr.V4.loopback])
  in
  let listen_socket ip =
    let addr = `Tcp (ip, config.port) in
    try Some (Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:config.max_connections addr)
    with exn ->
      let ip_str = Format.asprintf "%a" Eio.Net.Ipaddr.pp ip in
      eprintf "[%s] Failed to listen on %s:%d (%s)\n%!"
        Mcp_protocol.server_name
        ip_str
        config.port
        (Printexc.to_string exn);
      None
  in
  let sockets =
    resolve_listen_ips config.host
    |> List.filter_map listen_socket
  in
  let is_cancelled exn =
    match exn with
    | Eio.Cancel.Cancelled _ -> true
    | _ -> false
  in
  let initial_backoff_s = 0.05 in
  let max_backoff_s = 1.0 in
  let make_accept_loop socket =
    let backoff_s = ref initial_backoff_s in
    let reset_backoff () = backoff_s := initial_backoff_s in
    let bump_backoff () = backoff_s := min max_backoff_s (!backoff_s *. 2.0) in
    let rec accept_loop () =
      try
        (try
           let flow, client_addr = Eio.Net.accept ~sw socket in
           reset_backoff ();
           Eio.Fiber.fork ~sw (fun () ->
             try
               Httpun_eio.Server.create_connection_handler
                 ~sw
                 ~request_handler
                 ~error_handler
                 client_addr
                 flow
             with exn ->
               eprintf "[%s] Connection error: %s\n%!"
                 Mcp_protocol.server_name
                 (Printexc.to_string exn)
           )
         with exn ->
           if is_cancelled exn then raise exn;
           let delay = !backoff_s in
           eprintf "[%s] Accept error: %s (backoff %.2fs)\n%!"
             Mcp_protocol.server_name
             (Printexc.to_string exn)
             delay;
           Eio.Time.sleep clock delay;
           bump_backoff ());
        accept_loop ()
      with exn ->
        if is_cancelled exn then ()
        else
          let delay = !backoff_s in
          eprintf "[%s] Accept loop error: %s (backoff %.2fs)\n%!"
            Mcp_protocol.server_name
            (Printexc.to_string exn)
            delay;
          Eio.Time.sleep clock delay;
          bump_backoff ();
          accept_loop ()
    in
    accept_loop
  in
  let first_socket =
    match sockets with
    | [] -> failwith "No listening sockets available"
    | socket :: rest ->
        List.iter
          (fun extra ->
            Eio.Fiber.fork ~sw (fun () ->
              make_accept_loop extra ()))
          rest;
        socket
  in

  eprintf "🎨 %s MCP Server (Eio)\n" Mcp_protocol.server_name;
  eprintf "   Protocol: %s\n" Mcp_protocol.protocol_version;
  eprintf "   HTTP:     http://%s:%d\n" config.host config.port;
  eprintf "   MCP:      GET  /mcp -> SSE stream (streamable-http)\n";
  eprintf "             POST /mcp -> JSON-RPC requests\n";
  eprintf "   Graceful shutdown: SIGTERM/SIGINT supported\n%!";

  (* Periodic cleanup fiber for idle plugin channels - prevents memory leaks *)
  Eio.Fiber.fork ~sw (fun () ->
    let is_cancelled exn =
      match exn with
      | Eio.Cancel.Cancelled _ -> true
      | _ -> false
    in
    let rec cleanup_loop () =
      (try
         Eio.Time.sleep clock 60.0 (* Clean up every 1 minute *)
       with exn ->
         if is_cancelled exn then raise exn;
         eprintf "[Plugin] cleanup sleep error: %s\n%!" (Printexc.to_string exn));
      (try
         Figma_plugin_bridge.cleanup_inactive ~ttl_seconds:300.0 (* 5 min TTL *)
       with exn ->
         if is_cancelled exn then raise exn;
         eprintf "[Plugin] cleanup loop error: %s\n%!" (Printexc.to_string exn);
         Eio.Time.sleep clock 5.0);  (* backoff before retry *)
      cleanup_loop ()
    in
    try cleanup_loop () with exn ->
      if is_cancelled exn then ()
      else eprintf "[Plugin] cleanup fatal error: %s\n%!" (Printexc.to_string exn)
  );

  let accept_loop = make_accept_loop first_socket in
  accept_loop ()

(** Graceful shutdown exception *)
exception Shutdown

(** Start the server - entry point for main.ml (Pure Eio, no Lwt) *)
let start_server ?(config = default_config) server =
  (* Initialize crypto RNG for HTTPS/TLS *)
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let domain_mgr = Some (Eio.Stdenv.domain_mgr env) in

  (* Graceful shutdown setup *)
  let switch_ref = ref None in
  let shutdown_initiated = ref false in
  let initiate_shutdown signal_name =
    if not !shutdown_initiated then begin
      shutdown_initiated := true;
      eprintf "\n🎨 %s: Received %s, shutting down gracefully...\n%!" Mcp_protocol.server_name signal_name;

      (* Broadcast shutdown notification to all SSE clients *)
      broadcast_sse_shutdown signal_name;
      eprintf "🎨 %s: Sent shutdown notification to %d SSE clients\n%!" Mcp_protocol.server_name (Hashtbl.length sse_clients);

      (* Give clients 200ms to receive the notification *)
      Unix.sleepf 0.2;

      (* Gracefully close all SSE connections before Switch.fail *)
      close_all_sse_connections ();

      (* Give connections 200ms to complete close handshake *)
      Unix.sleepf 0.2;

      match !switch_ref with
      | Some sw -> Eio.Switch.fail sw Shutdown
      | None -> ()
    end
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGTERM"));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGINT"));

  (try
    Eio.Switch.run @@ fun sw ->
    switch_ref := Some sw;
    run ~sw ~net ~clock ~domain_mgr config server
  with
  | Shutdown ->
      eprintf "🎨 %s: Shutdown complete.\n%!" Mcp_protocol.server_name
  | Eio.Cancel.Cancelled _ ->
      eprintf "🎨 %s: Shutdown complete.\n%!" Mcp_protocol.server_name)

(** ============== stdio Server (Pure Eio) ============== *)

(** Run stdio server with Eio - blocking loop reading from stdin *)
let run_stdio ~sw ~env ~net ~clock server =
  (* Set Eio context for pure Eio handlers *)
  let eio_client = Figma_api_eio.make_client net in
  ignore (Mcp_tools.set_eio_context ~sw ~net ~clock ~client:eio_client);

  eprintf "[%s] MCP Server started (protocol: %s, mode: stdio/Eio)\n%!"
    Mcp_protocol.server_name Mcp_protocol.protocol_version;

  (* Create buffered reader for stdin *)
  let stdin_flow = Eio.Stdenv.stdin env in
  let buf_read = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdin_flow in

  let rec read_loop () =
    match Eio.Buf_read.line buf_read with
    | line ->
        if String.trim line <> "" then begin
          match Mcp_protocol.parse_request line with
          | Ok req ->
              if Mcp_protocol.is_notification req then
                (* Notification: no response on stdout per JSON-RPC *)
                ignore (Mcp_protocol.process_request_sync server req)
              else begin
                (* Process request using sync handler (runs in Eio context) *)
                let response = Mcp_protocol.process_request_sync server req in
                let response_str = Yojson.Safe.to_string response in
                print_endline response_str;
                flush stdout
              end
          | Error msg ->
              let err_response = Mcp_protocol.make_error_response `Null Mcp_protocol.parse_error msg None in
              print_endline (Yojson.Safe.to_string err_response);
              flush stdout
        end;
        read_loop ()
    | exception End_of_file ->
        eprintf "[%s] Connection closed (EOF)\n%!" Mcp_protocol.server_name
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
        eprintf "[%s] Error: Input line too long\n%!" Mcp_protocol.server_name
    | exception exn ->
        eprintf "[%s] Error: %s\n%!" Mcp_protocol.server_name (Printexc.to_string exn)
  in
  read_loop ()

(** Start stdio server - entry point that sets up Eio runtime *)
let start_stdio_server server =
  (* Initialize crypto RNG for HTTPS/TLS *)
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in

  (* Graceful shutdown setup *)
  let switch_ref = ref None in
  let shutdown_initiated = ref false in
  let initiate_shutdown signal_name =
    if not !shutdown_initiated then begin
      shutdown_initiated := true;
      eprintf "\n[%s] Received %s, shutting down...\n%!" Mcp_protocol.server_name signal_name;
      match !switch_ref with
      | Some sw -> Eio.Switch.fail sw Shutdown
      | None -> ()
    end
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGTERM"));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> initiate_shutdown "SIGINT"));

  (try
    Eio.Switch.run @@ fun sw ->
    switch_ref := Some sw;
    run_stdio ~sw ~env ~net ~clock server
  with
  | Shutdown ->
      eprintf "[%s] Shutdown complete.\n%!" Mcp_protocol.server_name
  | Eio.Cancel.Cancelled _ ->
      eprintf "[%s] Shutdown complete.\n%!" Mcp_protocol.server_name)
