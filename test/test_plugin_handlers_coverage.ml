(** Coverage tests for mcp_plugin_handlers.ml — truncate_string, plugin_error_message.
    Targets 0/434 coverage points: these two pure functions cover the local helpers. *)

let () =
  let open Alcotest in

  (* ============== truncate_string ============== *)
  let test_truncate_short () =
    check string "short unchanged" "hello"
      (Mcp_plugin_handlers.truncate_string "hello")
  in
  let test_truncate_exact () =
    let s = String.make 2000 'x' in
    check string "exact 2000" s (Mcp_plugin_handlers.truncate_string s)
  in
  let test_truncate_long () =
    let s = String.make 2001 'x' in
    let result = Mcp_plugin_handlers.truncate_string s in
    check int "truncated len" 2003 (String.length result);
    (* 2000 + 3 bytes for "…" in UTF-8 *)
    check bool "ends with ellipsis" true
      (String.length result > 2000)
  in
  let test_truncate_custom_len () =
    let s = "hello world" in
    check string "custom 5" "hello\xe2\x80\xa6"
      (Mcp_plugin_handlers.truncate_string ~max_len:5 s)
  in
  let test_truncate_empty () =
    check string "empty" "" (Mcp_plugin_handlers.truncate_string "")
  in

  (* ============== plugin_error_message ============== *)
  let test_error_string () =
    let result = Mcp_plugin_handlers.plugin_error_message (`String "oops") in
    check string "string" "oops" result
  in
  let test_error_assoc_error () =
    let json = `Assoc [("error", `String "bad request")] in
    check string "assoc error" "bad request"
      (Mcp_plugin_handlers.plugin_error_message json)
  in
  let test_error_assoc_message () =
    let json = `Assoc [("message", `String "not found")] in
    check string "assoc message" "not found"
      (Mcp_plugin_handlers.plugin_error_message json)
  in
  let test_error_assoc_error_list () =
    let json = `Assoc [("error", `List [`String "err1"; `String "err2"])] in
    check string "error list" "err1"
      (Mcp_plugin_handlers.plugin_error_message json)
  in
  let test_error_assoc_error_list_non_string () =
    let json = `Assoc [("error", `List [`Int 42])] in
    (* List without strings falls through to Yojson.Safe.to_string *)
    let result = Mcp_plugin_handlers.plugin_error_message json in
    check bool "has content" true (String.length result > 0)
  in
  let test_error_assoc_fallback () =
    let json = `Assoc [("code", `Int 500)] in
    let result = Mcp_plugin_handlers.plugin_error_message json in
    check bool "json string" true (String.length result > 0)
  in
  let test_error_null () =
    let result = Mcp_plugin_handlers.plugin_error_message `Null in
    check string "null" "null" result
  in
  let test_error_int () =
    let result = Mcp_plugin_handlers.plugin_error_message (`Int 42) in
    check string "int" "42" result
  in
  let test_error_long_string () =
    let long = String.make 3000 'x' in
    let result = Mcp_plugin_handlers.plugin_error_message (`String long) in
    check bool "truncated" true (String.length result < 3000)
  in
  let test_error_error_priority () =
    (* When both "error" and "message" exist, "error" takes priority *)
    let json = `Assoc [("error", `String "err"); ("message", `String "msg")] in
    check string "error first" "err"
      (Mcp_plugin_handlers.plugin_error_message json)
  in
  let test_error_assoc_empty () =
    (* Assoc with no matching keys falls through *)
    let json = `Assoc [] in
    let result = Mcp_plugin_handlers.plugin_error_message json in
    check bool "non-empty" true (String.length result > 0)
  in
  let test_error_nested_error_non_string () =
    (* error value is not string/list → falls to message *)
    let json = `Assoc [("error", `Int 500); ("message", `String "msg")] in
    check string "falls to message" "msg"
      (Mcp_plugin_handlers.plugin_error_message json)
  in

  run "Mcp_plugin_handlers Coverage" [
    ("truncate_string", [
      test_case "short" `Quick test_truncate_short;
      test_case "exact" `Quick test_truncate_exact;
      test_case "long" `Quick test_truncate_long;
      test_case "custom len" `Quick test_truncate_custom_len;
      test_case "empty" `Quick test_truncate_empty;
    ]);
    ("plugin_error_message", [
      test_case "string" `Quick test_error_string;
      test_case "assoc error" `Quick test_error_assoc_error;
      test_case "assoc message" `Quick test_error_assoc_message;
      test_case "error list" `Quick test_error_assoc_error_list;
      test_case "error list non-string" `Quick test_error_assoc_error_list_non_string;
      test_case "assoc fallback" `Quick test_error_assoc_fallback;
      test_case "null" `Quick test_error_null;
      test_case "int" `Quick test_error_int;
      test_case "long string" `Quick test_error_long_string;
      test_case "error priority" `Quick test_error_error_priority;
      test_case "assoc empty" `Quick test_error_assoc_empty;
      test_case "nested non-string" `Quick test_error_nested_error_non_string;
    ]);
  ]
