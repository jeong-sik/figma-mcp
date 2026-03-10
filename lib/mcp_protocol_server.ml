open Printf
open Mcp_protocol_router
open Mcp_sse_transport
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

(** Run HTTP server with Eio *)
let run ~sw ~net ~clock ~domain_mgr config server =
  (* Set Eio context for pure Eio handlers (Lwt-free path) *)
  let eio_client = Figma_api_eio.make_client net in
  let eio_ctx = Mcp_helpers.set_eio_context ~sw ~net ~clock ~client:eio_client in
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
        Figma_mcp_protocol.server_name
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
                 Figma_mcp_protocol.server_name
                 (Printexc.to_string exn)
           )
         with exn ->
           if is_cancelled exn then raise exn;
           let delay = !backoff_s in
           eprintf "[%s] Accept error: %s (backoff %.2fs)\n%!"
             Figma_mcp_protocol.server_name
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
            Figma_mcp_protocol.server_name
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

  eprintf "🎨 %s MCP Server %s (Eio)\n" Figma_mcp_protocol.server_name Figma_mcp_protocol.server_version;
  eprintf "   Protocol: %s\n" Figma_mcp_protocol.protocol_version;
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
      eprintf "\n🎨 %s: Received %s, shutting down gracefully...\n%!" Figma_mcp_protocol.server_name signal_name;

      (* Broadcast shutdown notification to all SSE clients *)
      broadcast_sse_shutdown signal_name;
      eprintf "🎨 %s: Sent shutdown notification to %d SSE clients\n%!" Figma_mcp_protocol.server_name (Hashtbl.length sse_clients);

      (* Give clients 200ms to receive the notification.
         NOTE: Unix.sleepf is intentional here. This closure runs as a POSIX signal
         handler (Sys.set_signal) which executes outside Eio fiber context, so
         Eio.Time.sleep is not available. Blocking the OS thread is acceptable
         during shutdown. *)
      Unix.sleepf 0.2;

      (* Gracefully close all SSE connections before Switch.fail *)
      close_all_sse_connections ();

      (* Give connections 200ms to complete close handshake (same signal handler
         context constraint as above) *)
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
      eprintf "🎨 %s: Shutdown complete.\n%!" Figma_mcp_protocol.server_name
  | Eio.Cancel.Cancelled _ ->
      eprintf "🎨 %s: Shutdown complete.\n%!" Figma_mcp_protocol.server_name)

(** ============== stdio Server (Pure Eio) ============== *)

(** Run stdio server with Eio - blocking loop reading from stdin *)
let run_stdio ~sw ~env ~net ~clock server =
  (* Set Eio context for pure Eio handlers *)
  let eio_client = Figma_api_eio.make_client net in
  ignore (Mcp_helpers.set_eio_context ~sw ~net ~clock ~client:eio_client);

  eprintf "[%s] MCP Server started (protocol: %s, mode: stdio/Eio)\n%!"
    Figma_mcp_protocol.server_name Figma_mcp_protocol.protocol_version;

  (* Create buffered reader for stdin *)
  let stdin_flow = Eio.Stdenv.stdin env in
  let buf_read = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdin_flow in

  let rec read_loop () =
    match Eio.Buf_read.line buf_read with
    | line ->
        if String.trim line <> "" then begin
          match Figma_mcp_protocol.parse_request line with
          | Ok req ->
              if Figma_mcp_protocol.is_notification req then
                (* Notification: no response on stdout per JSON-RPC *)
                ignore (Figma_mcp_protocol.process_request_sync server req)
              else begin
                (* Process request using sync handler (runs in Eio context) *)
                let response = Figma_mcp_protocol.process_request_sync server req in
                let response_str = Yojson.Safe.to_string response in
                print_endline response_str;
                flush stdout
              end
          | Error msg ->
              let err_response = Figma_mcp_protocol.make_error_response `Null Figma_mcp_protocol.parse_error msg None in
              print_endline (Yojson.Safe.to_string err_response);
              flush stdout
        end;
        read_loop ()
    | exception End_of_file ->
        eprintf "[%s] Connection closed (EOF)\n%!" Figma_mcp_protocol.server_name
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
        eprintf "[%s] Error: Input line too long\n%!" Figma_mcp_protocol.server_name
    | exception exn ->
        eprintf "[%s] Error: %s\n%!" Figma_mcp_protocol.server_name (Printexc.to_string exn)
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
      eprintf "\n[%s] Received %s, shutting down...\n%!" Figma_mcp_protocol.server_name signal_name;
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
      eprintf "[%s] Shutdown complete.\n%!" Figma_mcp_protocol.server_name
  | Eio.Cancel.Cancelled _ ->
      eprintf "[%s] Shutdown complete.\n%!" Figma_mcp_protocol.server_name)
