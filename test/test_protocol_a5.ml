(** Coverage A5: mcp_protocol_eio.ml — pure function edge cases.
    Targets: classify_message (edge cases), clamp_poll_ms, clamp_max_commands,
             is_public_path, normalize_env, api_key_env_name, default_config. *)

open Alcotest
open Figma_protocol_eio

(* ============== classify_message ============== *)

let test_classify_request_with_int_id () =
  let msg = {|{"jsonrpc":"2.0","method":"initialize","id":1}|} in
  let kind = classify_message msg in
  check bool "request" true (kind = `Request)

let test_classify_request_with_string_id () =
  let msg = {|{"jsonrpc":"2.0","method":"tools/list","id":"abc-123"}|} in
  let kind = classify_message msg in
  check bool "request" true (kind = `Request)

let test_classify_notification_no_id () =
  let msg = {|{"jsonrpc":"2.0","method":"notifications/initialized"}|} in
  let kind = classify_message msg in
  check bool "notification" true (kind = `Notification)

let test_classify_notification_null_id () =
  let msg = {|{"jsonrpc":"2.0","method":"notifications/cancelled","id":null}|} in
  let kind = classify_message msg in
  check bool "notification" true (kind = `Notification)

let test_classify_response_result () =
  let msg = {|{"jsonrpc":"2.0","id":1,"result":{"ok":true}}|} in
  let kind = classify_message msg in
  check bool "response" true (kind = `Response)

let test_classify_response_error () =
  let msg = {|{"jsonrpc":"2.0","id":2,"error":{"code":-32600,"message":"bad"}}|} in
  let kind = classify_message msg in
  check bool "response" true (kind = `Response)

let test_classify_unknown_no_method_no_result () =
  let msg = {|{"jsonrpc":"2.0","id":1}|} in
  let kind = classify_message msg in
  check bool "unknown" true (kind = `Unknown)

let test_classify_unknown_empty_object () =
  let msg = {|{}|} in
  let kind = classify_message msg in
  check bool "unknown" true (kind = `Unknown)

let test_classify_unknown_invalid_json () =
  let msg = "not json at all" in
  let kind = classify_message msg in
  check bool "unknown" true (kind = `Unknown)

let test_classify_unknown_json_array () =
  let msg = {|[1, 2, 3]|} in
  let kind = classify_message msg in
  check bool "unknown" true (kind = `Unknown)

let test_classify_unknown_json_string () =
  let msg = {|"just a string"|} in
  let kind = classify_message msg in
  check bool "unknown" true (kind = `Unknown)

let test_classify_unknown_json_number () =
  let msg = "42" in
  let kind = classify_message msg in
  check bool "unknown" true (kind = `Unknown)

let test_classify_response_with_both () =
  (* Has both result and error — still classified as response *)
  let msg = {|{"jsonrpc":"2.0","id":1,"result":null,"error":{"code":-1,"message":"x"}}|} in
  let kind = classify_message msg in
  check bool "response" true (kind = `Response)

(* ============== clamp_poll_ms ============== *)

let max_poll = Figma_config.Plugin.poll_max_ms

let test_clamp_poll_negative () =
  check int "neg" 0 (clamp_poll_ms (-1))

let test_clamp_poll_zero () =
  check int "zero" 0 (clamp_poll_ms 0)

let test_clamp_poll_normal () =
  check int "100" 100 (clamp_poll_ms 100)

let test_clamp_poll_at_max () =
  check int "at max" max_poll (clamp_poll_ms max_poll)

let test_clamp_poll_over_max () =
  check int "over max" max_poll (clamp_poll_ms (max_poll + 1))

let test_clamp_poll_large_negative () =
  check int "large neg" 0 (clamp_poll_ms (-999999))

let test_clamp_poll_one () =
  check int "one" 1 (clamp_poll_ms 1)

(* ============== clamp_max_commands ============== *)

let max_cmd = Figma_config.Plugin.max_commands

let test_clamp_cmd_zero () =
  check int "0->1" 1 (clamp_max_commands 0)

let test_clamp_cmd_negative () =
  check int "neg->1" 1 (clamp_max_commands (-5))

let test_clamp_cmd_one () =
  check int "1->1" 1 (clamp_max_commands 1)

let test_clamp_cmd_normal () =
  check int "10" 10 (clamp_max_commands 10)

let test_clamp_cmd_at_max () =
  check int "at max" max_cmd (clamp_max_commands max_cmd)

let test_clamp_cmd_over_max () =
  check int "over max" max_cmd (clamp_max_commands (max_cmd + 1))

let test_clamp_cmd_large () =
  check int "huge" max_cmd (clamp_max_commands 999999)

(* ============== is_public_path ============== *)

let test_ipp_options_any () =
  check bool "OPTIONS /" true (is_public_path `OPTIONS "/")

let test_ipp_options_random () =
  check bool "OPTIONS /foo" true (is_public_path `OPTIONS "/foo/bar")

let test_ipp_get_health () =
  check bool "GET /health" true (is_public_path `GET "/health")

let test_ipp_post_health () =
  check bool "POST /health" false (is_public_path `POST "/health")

let test_ipp_get_root () =
  check bool "GET /" false (is_public_path `GET "/")

let test_ipp_get_metrics () =
  check bool "GET /metrics" true (is_public_path `GET "/metrics")

let test_ipp_post_mcp () =
  check bool "POST /mcp" false (is_public_path `POST "/mcp")

let test_ipp_get_sse () =
  check bool "GET /sse" false (is_public_path `GET "/sse")

let test_ipp_delete_health () =
  check bool "DELETE /health" false (is_public_path `DELETE "/health")

let test_ipp_put_health () =
  check bool "PUT /health" false (is_public_path `PUT "/health")

(* ============== normalize_env ============== *)

let os = Alcotest.option Alcotest.string

let test_norm_none () =
  check os "None" None (normalize_env None)

let test_norm_empty () =
  check os "empty" None (normalize_env (Some ""))

let test_norm_whitespace () =
  check os "spaces" None (normalize_env (Some "   "))

let test_norm_tabs () =
  check os "tabs" None (normalize_env (Some "\t\t"))

let test_norm_value () =
  check os "value" (Some "hello") (normalize_env (Some "hello"))

let test_norm_trimmed () =
  check os "trim" (Some "hi") (normalize_env (Some "  hi  "))

let test_norm_newlines () =
  (* String.trim removes \n too *)
  check os "newlines" None (normalize_env (Some "\n\n"))

let test_norm_mixed_ws () =
  check os "mixed" (Some "x") (normalize_env (Some " \t x \n "))

(* ============== default_config ============== *)

let test_default_port () =
  check int "port" 8933 default_config.port

let test_default_host () =
  check string "host" "localhost" default_config.host

let test_default_max_connections () =
  check int "max_connections" 64 default_config.max_connections

(* ============== api_key_env_name ============== *)

(* api_key_env_name reads env vars, so we test it returns a string.
   Exact value depends on whether FIGMA_MCP_API_KEY or MCP_API_KEY is set. *)
let test_api_key_env_name_returns_string () =
  let name = api_key_env_name () in
  check bool "non-empty" true (String.length name > 0);
  check bool "is expected" true
    (name = "FIGMA_MCP_API_KEY" || name = "MCP_API_KEY")

let () =
  Alcotest.run "protocol_a5"
    [ ("classify_message", [
        test_case "request int id" `Quick test_classify_request_with_int_id;
        test_case "request string id" `Quick test_classify_request_with_string_id;
        test_case "notification no id" `Quick test_classify_notification_no_id;
        test_case "notification null id" `Quick test_classify_notification_null_id;
        test_case "response result" `Quick test_classify_response_result;
        test_case "response error" `Quick test_classify_response_error;
        test_case "response both" `Quick test_classify_response_with_both;
        test_case "unknown no method" `Quick test_classify_unknown_no_method_no_result;
        test_case "unknown empty obj" `Quick test_classify_unknown_empty_object;
        test_case "unknown invalid json" `Quick test_classify_unknown_invalid_json;
        test_case "unknown array" `Quick test_classify_unknown_json_array;
        test_case "unknown string" `Quick test_classify_unknown_json_string;
        test_case "unknown number" `Quick test_classify_unknown_json_number;
      ]);
      ("clamp_poll_ms", [
        test_case "negative" `Quick test_clamp_poll_negative;
        test_case "zero" `Quick test_clamp_poll_zero;
        test_case "normal" `Quick test_clamp_poll_normal;
        test_case "at max" `Quick test_clamp_poll_at_max;
        test_case "over max" `Quick test_clamp_poll_over_max;
        test_case "large neg" `Quick test_clamp_poll_large_negative;
        test_case "one" `Quick test_clamp_poll_one;
      ]);
      ("clamp_max_commands", [
        test_case "zero" `Quick test_clamp_cmd_zero;
        test_case "negative" `Quick test_clamp_cmd_negative;
        test_case "one" `Quick test_clamp_cmd_one;
        test_case "normal" `Quick test_clamp_cmd_normal;
        test_case "at max" `Quick test_clamp_cmd_at_max;
        test_case "over max" `Quick test_clamp_cmd_over_max;
        test_case "huge" `Quick test_clamp_cmd_large;
      ]);
      ("is_public_path", [
        test_case "OPTIONS /" `Quick test_ipp_options_any;
        test_case "OPTIONS random" `Quick test_ipp_options_random;
        test_case "GET /health" `Quick test_ipp_get_health;
        test_case "POST /health" `Quick test_ipp_post_health;
        test_case "GET /" `Quick test_ipp_get_root;
        test_case "GET /metrics" `Quick test_ipp_get_metrics;
        test_case "POST /mcp" `Quick test_ipp_post_mcp;
        test_case "GET /sse" `Quick test_ipp_get_sse;
        test_case "DELETE /health" `Quick test_ipp_delete_health;
        test_case "PUT /health" `Quick test_ipp_put_health;
      ]);
      ("normalize_env", [
        test_case "None" `Quick test_norm_none;
        test_case "empty" `Quick test_norm_empty;
        test_case "whitespace" `Quick test_norm_whitespace;
        test_case "tabs" `Quick test_norm_tabs;
        test_case "value" `Quick test_norm_value;
        test_case "trimmed" `Quick test_norm_trimmed;
        test_case "newlines" `Quick test_norm_newlines;
        test_case "mixed ws" `Quick test_norm_mixed_ws;
      ]);
      ("default_config", [
        test_case "port" `Quick test_default_port;
        test_case "host" `Quick test_default_host;
        test_case "max_connections" `Quick test_default_max_connections;
      ]);
      ("api_key_env_name", [
        test_case "returns valid name" `Quick test_api_key_env_name_returns_string;
      ]);
    ]
