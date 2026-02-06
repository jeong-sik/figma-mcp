open Alcotest

let test_record_untracked_5xx () =
  Eio_main.run @@ fun _env ->
  let before = Server_metrics.snapshot () in
  Server_metrics.record_untracked_response ~bytes:10 `Internal_server_error;
  let after = Server_metrics.snapshot () in
  check int "total" (before.total + 1) after.total;
  check int "status_5xx" (before.status_5xx + 1) after.status_5xx;
  check int "errors" (before.errors + 1) after.errors;
  check int "bytes_out" (before.bytes_out + 10) after.bytes_out

let test_record_untracked_4xx () =
  Eio_main.run @@ fun _env ->
  let before = Server_metrics.snapshot () in
  Server_metrics.record_untracked_response ~bytes:0 `Bad_request;
  let after = Server_metrics.snapshot () in
  check int "total" (before.total + 1) after.total;
  check int "status_4xx" (before.status_4xx + 1) after.status_4xx;
  check int "errors" (before.errors + 1) after.errors

let () =
  run "server_metrics"
    [ ( "untracked",
        [ test_case "record untracked 5xx" `Quick test_record_untracked_5xx;
          test_case "record untracked 4xx" `Quick test_record_untracked_4xx
        ] ) ]
