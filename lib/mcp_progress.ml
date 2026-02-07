(** MCP Progress Notification Module
    Separated to avoid circular dependencies between mcp_tools and mcp_protocol_eio *)

open Printf

(** Progress token type *)
type progress_token = string

(** Fiber-local client id (HTTP streamable SSE). *)
let client_id_key : int Eio.Fiber.key = Eio.Fiber.create_key ()

let with_client_id client_id fn =
  Eio.Fiber.with_binding client_id_key client_id fn

(** SSE client sender functions (set by mcp_protocol_eio at startup) *)
let sse_clients_ref : (int, (string -> unit)) Hashtbl.t ref = ref (Hashtbl.create 10)

(** Register progress sender function *)
let register_progress_sender ~client_id ~sender =
  Hashtbl.replace !sse_clients_ref client_id sender

(** Unregister progress sender *)
let unregister_progress_sender client_id =
  Hashtbl.remove !sse_clients_ref client_id

(** Broadcast function reference (set by mcp_protocol_eio) *)
let broadcast_fn : (string -> unit) option ref = ref None

(** Set broadcast function *)
let set_broadcast_fn fn =
  broadcast_fn := Some fn

(** Generate unique progress token *)
let make_progress_token () =
  sprintf "progress_%d_%d" (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.))

let progress_notification_json ~token ~current ~total ~message =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "notifications/progress");
    ("params",
      `Assoc [
        ("progressToken", `String token);
        ("progress", `Int current);
        ("total", `Int total);
        ("message", `String message);
      ]);
  ]
  |> Yojson.Safe.to_string

(** Send progress notification.

    Prefer sending to the current fiber's bound SSE client (streamable HTTP).
    Falls back to broadcast (best-effort) for non-streamable contexts. *)
let send_progress ~token ~current ~total ~message () =
  let data = progress_notification_json ~token ~current ~total ~message in
  eprintf "[progress] %s: %d/%d - %s\n%!" token current total message;
  let try_scoped () =
    match Eio.Fiber.get client_id_key with
    | None -> false
    | Some client_id ->
        (match Hashtbl.find_opt !sse_clients_ref client_id with
         | None -> false
         | Some sender ->
             (try
               sender data;
               true
             with _ ->
               (* Client is likely disconnected; avoid retry storms. *)
               Hashtbl.remove !sse_clients_ref client_id;
               false))
  in
  if try_scoped () then ()
  else
    match !broadcast_fn with
    | Some fn -> fn data
    | None -> ()

(** Alias for send_progress *)
let update_progress = send_progress
