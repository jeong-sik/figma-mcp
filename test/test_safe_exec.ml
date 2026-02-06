open Alcotest

let write_temp ~size =
  let path = Filename.temp_file "safe-exec" ".txt" in
  Out_channel.with_open_bin path (fun oc ->
    let buf = Bytes.make 4096 'a' in
    let rec loop remaining =
      if remaining <= 0 then ()
      else begin
        let n = min remaining (Bytes.length buf) in
        Out_channel.output oc buf 0 n;
        loop (remaining - n)
      end
    in
    loop size
  );
  path

let test_output_cap () =
  let path = write_temp ~size:(300 * 1024) in
  match Safe_exec.run ~timeout_ms:2000 ~output_limit:(64 * 1024) "cat" [| "cat"; path |] with
  | Error e -> fail e
  | Ok out ->
      check bool "stdout truncated" true out.stdout_truncated;
      check bool "stderr empty" true (out.stderr = "");
      check bool "stdout length capped" true (String.length out.stdout <= 64 * 1024);
      (try Sys.remove path with _ -> ())

let test_timeout () =
  match Safe_exec.run ~timeout_ms:200 ~output_limit:1024 "sleep" [| "sleep"; "2" |] with
  | Error e -> fail e
  | Ok out ->
      check bool "timed out" true out.timed_out

let test_run_stdout_exit () =
  match Safe_exec.run_stdout ~timeout_ms:2000 ~output_limit:1024 "false" [| "false" |] with
  | Ok _ -> fail "expected failure"
  | Error _ -> ()

let () =
  run "Safe Exec" [
    "limits", [
      "output cap", `Quick, test_output_cap;
      "timeout", `Quick, test_timeout;
      "nonzero exit", `Quick, test_run_stdout_exit;
    ];
  ]
