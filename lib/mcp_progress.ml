(** MCP Progress Notification Module
    Separated to avoid circular dependencies between mcp_tools and mcp_protocol_eio *)

open Printf

(** Progress token type *)
type progress_token = string

(** SSE client reference (set by mcp_protocol_eio at startup) *)
let sse_clients_ref : (int, (unit -> unit)) Hashtbl.t ref = ref (Hashtbl.create 10)

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

(** Send progress notification via broadcast *)
let send_progress ~token ~current ~total ~message () =
  let data = sprintf
    {|{"jsonrpc":"2.0","method":"notifications/progress","params":{"progressToken":"%s","progress":%d,"total":%d,"message":"%s"}}|}
    token current total message
  in
  eprintf "[progress] %s: %d/%d - %s\n%!" token current total message;
  match !broadcast_fn with
  | Some fn -> fn data
  | None -> ()

(** Alias for send_progress *)
let update_progress = send_progress
