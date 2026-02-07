(** MCP progress notification tests *)

open Alcotest

let test_progress_notification_json () =
  let token = "t-123" in
  let message = "hello \"world\"\nline2" in
  let json_str =
    Mcp_progress.progress_notification_json ~token ~current:1 ~total:2 ~message
  in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  check string "jsonrpc" "2.0" (json |> member "jsonrpc" |> to_string);
  check string "method" "notifications/progress" (json |> member "method" |> to_string);
  let params = json |> member "params" in
  check string "progressToken" token (params |> member "progressToken" |> to_string);
  check int "progress" 1 (params |> member "progress" |> to_int);
  check int "total" 2 (params |> member "total" |> to_int);
  check string "message" message (params |> member "message" |> to_string)

let test_send_progress_scoped_sender () =
  let scoped = ref None in
  let broadcast = ref None in
  Mcp_progress.set_broadcast_fn (fun data -> broadcast := Some data);
  Mcp_progress.register_progress_sender ~client_id:123 ~sender:(fun data -> scoped := Some data);

  Eio_main.run @@ fun _env ->
  Mcp_progress.with_client_id 123 (fun () ->
    Mcp_progress.send_progress ~token:"tok" ~current:0 ~total:1 ~message:"m" ());

  check bool "scoped sender called" true (Option.is_some !scoped);
  check bool "broadcast not called" true (Option.is_none !broadcast);
  Mcp_progress.unregister_progress_sender 123

let test_send_progress_broadcast_fallback () =
  let broadcast = ref None in
  Mcp_progress.set_broadcast_fn (fun data -> broadcast := Some data);

  Eio_main.run @@ fun _env ->
  Mcp_progress.send_progress ~token:"tok" ~current:0 ~total:1 ~message:"m" ();

  check bool "broadcast called" true (Option.is_some !broadcast)

let () =
  run "MCP Progress" [
    "json", [
      "progress notification json", `Quick, test_progress_notification_json;
    ];
    "routing", [
      "scoped sender", `Quick, test_send_progress_scoped_sender;
      "broadcast fallback", `Quick, test_send_progress_broadcast_fallback;
    ];
  ]

