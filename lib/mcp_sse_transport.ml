(** SSE Transport layer for MCP server.

    Manages Server-Sent Events client connections, message routing,
    and the GET /mcp SSE handler. Thread-safe via [Eio.Mutex]. *)

open Printf

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

(** Re-serialize JSON string to compact single-line form.
    Prevents multi-line data: fields in SSE output.
    Returns original string unchanged if not valid JSON. *)
let compact_json_string s =
  match Yojson.Safe.from_string s with
  | json -> Yojson.Safe.to_string json
  | exception (Yojson.Json_error _) -> s

let format_sse_data data =
  if data = "" then
    "data: "
  else
    (* Compact JSON to single line to avoid multi-line data: split *)
    let compacted = compact_json_string data in
    if not (String.contains compacted '\n') then
      "data: " ^ compacted
    else
      (* Non-JSON multi-line: split per SSE spec *)
      let lines = String.split_on_char '\n' compacted in
      String.concat "\n" (List.map (fun line -> "data: " ^ line) lines)

[@@@coverage off]
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
  if not client.connected then ()
  else
    let data_lines = format_sse_data data in
    let msg = sprintf "event: %s\n%s\n\n" event data_lines in
    try
      Eio.Mutex.use_rw ~protect:true client.mutex (fun () ->
        Httpun.Body.Writer.write_string client.body msg;
        Httpun.Body.Writer.flush client.body ignore
      )
    with exn ->
      client.connected <- false;
      if Mcp_tools.is_network_error exn then
        eprintf "[sse] client disconnected: %s\n%!" (Printexc.to_string exn)
      else
        eprintf "[sse] send error: %s\n%!" (Printexc.to_string exn)

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

(** MCP SSE handler for streamable-http protocol (GET /mcp) *)
let mcp_sse_handler ~clock _request reqd =
  Mcp_http_helpers.Response.sse_stream reqd ~on_write:(fun body ->
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
