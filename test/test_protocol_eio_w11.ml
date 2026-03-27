(** Coverage Wave 11: mcp_protocol_eio.ml pure functions
    Targets: parse_json, get_*_field, clamp_*, classify_message,
    Cors.normalized_port/matches_pattern/origin_allowed, agent_request_json,
    format_sse_data, compact_json_string, webhook_passcode, api_key_env,
    validate_reference_image_path, Request.parse_positive_int, env helpers *)

open Figma_protocol_eio

let check_s msg expected actual = Alcotest.(check string) msg expected actual
let check_i msg expected actual = Alcotest.(check int) msg expected actual
let check_b msg expected actual = Alcotest.(check bool) msg expected actual
let check_os msg expected actual = Alcotest.(check (option string)) msg expected actual
let check_oi msg expected actual = Alcotest.(check (option int)) msg expected actual
let check_ob msg expected actual = Alcotest.(check (option bool)) msg expected actual

(* ---- parse_json ---- *)

let test_parse_json_empty () =
  match parse_json "" with Ok `Null -> () | _ -> Alcotest.fail "empty -> Ok Null"

let test_parse_json_whitespace () =
  match parse_json "  \n\t " with Ok `Null -> () | _ -> Alcotest.fail "ws -> Ok Null"

let test_parse_json_object () =
  match parse_json {|{"k":"v"}|} with
  | Ok (`Assoc [("k", `String "v")]) -> ()
  | _ -> Alcotest.fail "object"

let test_parse_json_array () =
  match parse_json "[1,2]" with
  | Ok (`List [`Int 1; `Int 2]) -> ()
  | _ -> Alcotest.fail "array"

let test_parse_json_string () =
  match parse_json {|"hi"|} with Ok (`String "hi") -> () | _ -> Alcotest.fail "string"

let test_parse_json_number () =
  match parse_json "42" with Ok (`Int 42) -> () | _ -> Alcotest.fail "number"

let test_parse_json_null () =
  match parse_json "null" with Ok `Null -> () | _ -> Alcotest.fail "null"

let test_parse_json_bool () =
  match parse_json "true" with Ok (`Bool true) -> () | _ -> Alcotest.fail "bool"

let test_parse_json_invalid () =
  match parse_json "{bad" with Error _ -> () | Ok _ -> Alcotest.fail "should error"

let test_parse_json_truncated () =
  match parse_json {|{"k":|} with Error _ -> () | Ok _ -> Alcotest.fail "truncated"

(* ---- get_string_field ---- *)

let test_gsf_present () =
  check_os "found" (Some "alice") (get_string_field "name" (`Assoc [("name", `String "alice")]))
let test_gsf_missing () =
  check_os "missing" None (get_string_field "age" (`Assoc [("name", `String "a")]))
let test_gsf_wrong_type () =
  check_os "wrong type" None (get_string_field "n" (`Assoc [("n", `Int 1)]))
let test_gsf_not_assoc () =
  check_os "not assoc" None (get_string_field "x" (`String "s"))
let test_gsf_null_val () =
  check_os "null" None (get_string_field "n" (`Assoc [("n", `Null)]))
let test_gsf_empty_assoc () =
  check_os "empty" None (get_string_field "n" (`Assoc []))

(* ---- get_int_field ---- *)

let test_gif_int () =
  check_oi "int" (Some 10) (get_int_field "c" (`Assoc [("c", `Int 10)]))
let test_gif_float () =
  check_oi "float" (Some 7) (get_int_field "c" (`Assoc [("c", `Float 7.9)]))
let test_gif_missing () =
  check_oi "missing" None (get_int_field "x" (`Assoc [("c", `Int 1)]))
let test_gif_wrong_type () =
  check_oi "string" None (get_int_field "c" (`Assoc [("c", `String "10")]))
let test_gif_not_assoc () =
  check_oi "list" None (get_int_field "x" (`List [`Int 1]))
let test_gif_null () =
  check_oi "null" None (get_int_field "c" (`Assoc [("c", `Null)]))
let test_gif_bool () =
  check_oi "bool" None (get_int_field "c" (`Assoc [("c", `Bool true)]))

(* ---- get_bool_field ---- *)

let test_gbf_true () =
  check_ob "true" (Some true) (get_bool_field "a" (`Assoc [("a", `Bool true)]))
let test_gbf_false () =
  check_ob "false" (Some false) (get_bool_field "a" (`Assoc [("a", `Bool false)]))
let test_gbf_missing () =
  check_ob "missing" None (get_bool_field "x" (`Assoc [("a", `Bool true)]))
let test_gbf_wrong_type () =
  check_ob "int" None (get_bool_field "a" (`Assoc [("a", `Int 1)]))
let test_gbf_not_assoc () =
  check_ob "null" None (get_bool_field "x" `Null)

(* ---- get_payload_field ---- *)

let test_gpf_string () =
  (match get_payload_field "d" (`Assoc [("d", `String "hi")]) with
   | Some (`String "hi") -> () | _ -> Alcotest.fail "string payload")
let test_gpf_object () =
  let inner = `Assoc [("x", `Int 1)] in
  (match get_payload_field "d" (`Assoc [("d", inner)]) with
   | Some v when v = inner -> () | _ -> Alcotest.fail "object payload")
let test_gpf_missing () =
  (match get_payload_field "x" (`Assoc [("d", `Int 1)]) with
   | None -> () | _ -> Alcotest.fail "missing")
let test_gpf_not_assoc () =
  (match get_payload_field "x" (`List []) with None -> () | _ -> Alcotest.fail "list")
let test_gpf_null_val () =
  (match get_payload_field "d" (`Assoc [("d", `Null)]) with
   | Some `Null -> () | _ -> Alcotest.fail "null val")

(* ---- clamp_poll_ms ---- *)

let test_cpm_neg () = check_i "neg" 0 (clamp_poll_ms (-100))
let test_cpm_zero () = check_i "zero" 0 (clamp_poll_ms 0)
let test_cpm_mid () = check_i "mid" 5000 (clamp_poll_ms 5000)
let test_cpm_at_max () =
  let m = Figma_config.Plugin.poll_max_ms in check_i "at max" m (clamp_poll_ms m)
let test_cpm_over () =
  let m = Figma_config.Plugin.poll_max_ms in check_i "over" m (clamp_poll_ms (m + 999))

(* ---- clamp_max_commands ---- *)

let test_cmc_zero () = check_i "zero" 1 (clamp_max_commands 0)
let test_cmc_neg () = check_i "neg" 1 (clamp_max_commands (-10))
let test_cmc_one () = check_i "one" 1 (clamp_max_commands 1)
let test_cmc_mid () = check_i "mid" 100 (clamp_max_commands 100)
let test_cmc_over () =
  let m = Figma_config.Plugin.max_commands in check_i "over" m (clamp_max_commands (m + 100))
let test_cmc_at_max () =
  let m = Figma_config.Plugin.max_commands in check_i "at" m (clamp_max_commands m)

(* ---- Cors.normalized_port ---- *)

let test_cnp_some () = check_i "explicit" 8080 (Cors.normalized_port "http" (Some 8080))
let test_cnp_http () = check_i "http" 80 (Cors.normalized_port "http" None)
let test_cnp_https () = check_i "https" 443 (Cors.normalized_port "https" None)
let test_cnp_unknown () = check_i "ftp" 0 (Cors.normalized_port "ftp" None)

(* ---- Cors.matches_pattern ---- *)

let test_mp_any_origin () =
  check_b "any" true
    (Cors.matches_pattern (Origin {scheme="https";host="e.com";port=None}) Any)
let test_mp_any_null () =
  check_b "any null" true (Cors.matches_pattern Origin_null Any)
let test_mp_null_null () =
  check_b "null" true (Cors.matches_pattern Origin_null Null)
let test_mp_null_exact () =
  check_b "null vs exact" false
    (Cors.matches_pattern Origin_null (Exact {scheme="https";host="e.com";port=Some 443}))
let test_mp_null_any_port () =
  check_b "null vs anyport" false
    (Cors.matches_pattern Origin_null (Any_port {scheme="https";host="e.com"}))
let test_mp_exact_default_port () =
  check_b "exact default" true
    (Cors.matches_pattern
       (Origin {scheme="https";host="e.com";port=None})
       (Exact {scheme="https";host="e.com";port=Some 443}))
let test_mp_exact_host_mismatch () =
  check_b "host" false
    (Cors.matches_pattern
       (Origin {scheme="https";host="other.com";port=None})
       (Exact {scheme="https";host="e.com";port=Some 443}))
let test_mp_exact_scheme_mismatch () =
  check_b "scheme" false
    (Cors.matches_pattern
       (Origin {scheme="http";host="e.com";port=None})
       (Exact {scheme="https";host="e.com";port=Some 443}))
let test_mp_exact_port_mismatch () =
  check_b "port" false
    (Cors.matches_pattern
       (Origin {scheme="https";host="e.com";port=Some 8080})
       (Exact {scheme="https";host="e.com";port=Some 443}))
let test_mp_any_port_match () =
  check_b "any_port ok" true
    (Cors.matches_pattern
       (Origin {scheme="https";host="e.com";port=Some 9999})
       (Any_port {scheme="https";host="e.com"}))
let test_mp_any_port_scheme () =
  check_b "any_port scheme" false
    (Cors.matches_pattern
       (Origin {scheme="http";host="e.com";port=None})
       (Any_port {scheme="https";host="e.com"}))
let test_mp_origin_vs_null () =
  check_b "origin vs null" false
    (Cors.matches_pattern
       (Origin {scheme="https";host="e.com";port=None}) Null)

(* ---- classify_message ---- *)

let test_cm_request () =
  (match classify_message {|{"jsonrpc":"2.0","method":"tools/list","id":1}|} with
   | `Request -> () | _ -> Alcotest.fail "request")
let test_cm_notif_null_id () =
  (match classify_message {|{"method":"init","id":null}|} with
   | `Notification -> () | _ -> Alcotest.fail "notif null")
let test_cm_notif_no_id () =
  (match classify_message {|{"method":"init"}|} with
   | `Notification -> () | _ -> Alcotest.fail "notif no id")
let test_cm_response_result () =
  (match classify_message {|{"id":1,"result":{}}|} with
   | `Response -> () | _ -> Alcotest.fail "response result")
let test_cm_response_error () =
  (match classify_message {|{"id":1,"error":{"code":-1}}|} with
   | `Response -> () | _ -> Alcotest.fail "response error")
let test_cm_unknown_id_only () =
  (match classify_message {|{"id":1}|} with
   | `Unknown -> () | _ -> Alcotest.fail "unknown id")
let test_cm_unknown_bad_json () =
  (match classify_message "broken" with
   | `Unknown -> () | _ -> Alcotest.fail "unknown bad")
let test_cm_unknown_array () =
  (match classify_message "[1]" with
   | `Unknown -> () | _ -> Alcotest.fail "unknown array")
let test_cm_unknown_no_fields () =
  (match classify_message {|{"foo":"bar"}|} with
   | `Unknown -> () | _ -> Alcotest.fail "unknown no method")

(* ---- compact_json_string ---- *)

let test_cjs_compact () =
  check_s "compact" {|{"a":1}|} (compact_json_string {|{"a":1}|})
let test_cjs_pretty () =
  check_s "pretty" {|{"a":1,"b":2}|} (compact_json_string "{\n  \"a\": 1,\n  \"b\": 2\n}")
let test_cjs_not_json () =
  check_s "not json" "hello" (compact_json_string "hello")
let test_cjs_empty () =
  check_s "empty" "" (compact_json_string "")
let test_cjs_null () =
  check_s "null" "null" (compact_json_string "null")
let test_cjs_array () =
  check_s "array" "[1,2,3]" (compact_json_string "[ 1, 2, 3 ]")

(* ---- format_sse_data ---- *)

let test_fsd_empty () = check_s "empty" "data: " (format_sse_data "")
let test_fsd_single () = check_s "single" "data: hi" (format_sse_data "hi")
let test_fsd_json () = check_s "json" {|data: {"a":1}|} (format_sse_data {|{"a":  1}|})
let test_fsd_multiline () =
  check_s "multiline" "data: l1\ndata: l2\ndata: l3" (format_sse_data "l1\nl2\nl3")
let test_fsd_pretty_json () =
  check_s "pretty" {|data: {"x":1}|} (format_sse_data "{\n  \"x\": 1\n}")

(* ---- Request.parse_positive_int ---- *)

let test_rpi_valid () = check_oi "42" (Some 42) (Request.parse_positive_int "42")
let test_rpi_one () = check_oi "1" (Some 1) (Request.parse_positive_int "1")
let test_rpi_zero () = check_oi "0" None (Request.parse_positive_int "0")
let test_rpi_neg () = check_oi "-5" None (Request.parse_positive_int "-5")
let test_rpi_abc () = check_oi "abc" None (Request.parse_positive_int "abc")
let test_rpi_float () = check_oi "3.14" None (Request.parse_positive_int "3.14")
let test_rpi_large () = check_oi "999k" (Some 999999) (Request.parse_positive_int "999999")

(* ---- parse_positive_int (top-level) ---- *)

let test_ppi_valid () = check_oi "10" (Some 10) (parse_positive_int "10")
let test_ppi_zero () = check_oi "0" None (parse_positive_int "0")
let test_ppi_neg () = check_oi "-1" None (parse_positive_int "-1")
let test_ppi_garbage () = check_oi "xyz" None (parse_positive_int "xyz")
let test_ppi_empty () = check_oi "empty" None (parse_positive_int "")

(* ---- hex_of_bytes ---- *)

let test_hob_empty () = check_s "empty" "" (hex_of_bytes (Bytes.of_string ""))
let test_hob_ff () = check_s "ff" "ff" (hex_of_bytes (Bytes.of_string "\xff"))
let test_hob_zero () = check_s "00" "00" (hex_of_bytes (Bytes.of_string "\x00"))
let test_hob_dead () = check_s "dead" "deadbeef" (hex_of_bytes (Bytes.of_string "\xde\xad\xbe\xef"))
let test_hob_ascii () = check_s "AB" "4142" (hex_of_bytes (Bytes.of_string "AB"))
let test_hob_hello () = check_s "hello" "68656c6c6f" (hex_of_bytes (Bytes.of_string "hello"))

(* ---- agent_status_to_string ---- *)

let test_ast_pending () = check_s "p" "pending" (agent_status_to_string Pending)
let test_ast_claimed () = check_s "c" "claimed" (agent_status_to_string Claimed)
let test_ast_completed () = check_s "o" "completed" (agent_status_to_string Completed)
let test_ast_failed () = check_s "f" "failed" (agent_status_to_string Failed)

(* ---- constant_time_equal ---- *)

let test_cte_same () = check_b "same" true (constant_time_equal "secret" "secret")
let test_cte_diff () = check_b "diff" false (constant_time_equal "secret" "public")
let test_cte_len () = check_b "len" false (constant_time_equal "short" "longstring")
let test_cte_empty () = check_b "empty" true (constant_time_equal "" "")
let test_cte_one_empty () = check_b "one" false (constant_time_equal "" "x")

(* ---- has_suffix ---- *)

let test_hs_match () = check_b "png" true (has_suffix ~suffix:".png" "img.png")
let test_hs_no () = check_b "jpg" false (has_suffix ~suffix:".png" "img.jpg")
let test_hs_empty () = check_b "empty" true (has_suffix ~suffix:"" "x")
let test_hs_longer () = check_b "longer" false (has_suffix ~suffix:"longsuffix" "x")
let test_hs_exact () = check_b "exact" true (has_suffix ~suffix:".png" ".png")

(* ---- split_csv ---- *)

let test_sc_basic () = Alcotest.(check (list string)) "basic" ["a";"b";"c"] (split_csv "a,b,c")
let test_sc_spaces () = Alcotest.(check (list string)) "sp" ["a";"b"] (split_csv " a , b ")
let test_sc_empty () = Alcotest.(check (list string)) "empty" [] (split_csv "")
let test_sc_trail () = Alcotest.(check (list string)) "trail" ["a";"b"] (split_csv "a,b,")
let test_sc_commas () = Alcotest.(check (list string)) "commas" [] (split_csv ",,")
let test_sc_single () = Alcotest.(check (list string)) "single" ["hi"] (split_csv "hi")

(* ---- trim ---- *)

let test_tr_spaces () = check_s "sp" "hello" (trim "  hello  ")
let test_tr_tabs () = check_s "tab" "hello" (trim "\thello\t")
let test_tr_empty () = check_s "empty" "" (trim "   ")
let test_tr_none () = check_s "none" "hello" (trim "hello")

(* ---- is_under_dir ---- *)

let test_iud_root () = check_b "root" true (is_under_dir ~dir_prefix:"/" "/any")
let test_iud_match () = check_b "match" true (is_under_dir ~dir_prefix:"/home/u/" "/home/u/f.txt")
let test_iud_no () = check_b "no" false (is_under_dir ~dir_prefix:"/home/u/" "/var/f")
let test_iud_partial () = check_b "partial" false (is_under_dir ~dir_prefix:"/home/u/" "/home")

(* ---- is_public_path ---- *)

let test_ipp_options () = check_b "opt" true (is_public_path `OPTIONS "/x")
let test_ipp_health () = check_b "health" true (is_public_path `GET "/health")
let test_ipp_post_h () = check_b "post h" false (is_public_path `POST "/health")
let test_ipp_get_r () = check_b "get /" false (is_public_path `GET "/")
let test_ipp_get_m () = check_b "metrics" true (is_public_path `GET "/metrics")

(* ---- normalize_env ---- *)

let test_ne_none () = check_os "none" None (normalize_env None)
let test_ne_empty () = check_os "empty" None (normalize_env (Some ""))
let test_ne_ws () = check_os "ws" None (normalize_env (Some "   "))
let test_ne_val () = check_os "val" (Some "hi") (normalize_env (Some " hi "))

(* ---- validate_webhook_passcode ---- *)

let test_vwp_no_secret_allow () =
  (match validate_webhook_passcode ~allow_no_auth:true ~secret_opt:None ~passcode:"" with
   | Error _ -> () | Ok () -> Alcotest.fail "should error")
let test_vwp_no_secret_strict () =
  (match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:None ~passcode:"" with
   | Ok () -> () | Error _ -> Alcotest.fail "should ok")
let test_vwp_valid () =
  (match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:(Some "s") ~passcode:"s" with
   | Ok () -> () | Error m -> Alcotest.fail m)
let test_vwp_invalid () =
  (match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:(Some "s") ~passcode:"x" with
   | Error _ -> () | Ok () -> Alcotest.fail "should fail")
let test_vwp_empty_passcode () =
  (match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:(Some "s") ~passcode:"" with
   | Error _ -> () | Ok () -> Alcotest.fail "empty passcode")
let test_vwp_ws_passcode () =
  (match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:(Some "s") ~passcode:"   " with
   | Error _ -> () | Ok () -> Alcotest.fail "ws passcode")

(* ---- Cors.origin_allowed (env var based) ---- *)

let with_env name value f =
  let saved = Sys.getenv_opt name in
  Unix.putenv name value;
  let result = try f () with e -> (match saved with Some v -> Unix.putenv name v | None -> (try Unix.putenv name "" with _ -> ())); raise e in
  (match saved with Some v -> Unix.putenv name v | None -> (try Unix.putenv name "" with _ -> ()));
  result

let test_coa_star () =
  with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "*" (fun () ->
    check_b "star" true (Cors.origin_allowed "https://example.com"))

let test_coa_specific () =
  with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "https://example.com" (fun () ->
    check_b "specific" true (Cors.origin_allowed "https://example.com"))

let test_coa_mismatch () =
  with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "https://example.com" (fun () ->
    check_b "mismatch" false (Cors.origin_allowed "https://other.com"))

let test_coa_invalid () =
  with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "*" (fun () ->
    check_b "invalid" false (Cors.origin_allowed "not-a-url"))

(* ---- Cors.parse_origin_value ---- *)

let test_cpov_null () =
  (match Cors.parse_origin_value "null" with Some Origin_null -> () | _ -> Alcotest.fail "null")
let test_cpov_https () =
  (match Cors.parse_origin_value "https://example.com" with
   | Some (Origin {scheme="https";host="example.com";port=None}) -> ()
   | _ -> Alcotest.fail "https")
let test_cpov_port () =
  (match Cors.parse_origin_value "http://localhost:3000" with
   | Some (Origin {scheme="http";host="localhost";port=Some 3000}) -> ()
   | _ -> Alcotest.fail "port")
let test_cpov_invalid () =
  (match Cors.parse_origin_value "foobar" with None -> () | _ -> Alcotest.fail "invalid")
let test_cpov_ftp () =
  (match Cors.parse_origin_value "ftp://files.com" with None -> () | _ -> Alcotest.fail "ftp")
let test_cpov_path () =
  (match Cors.parse_origin_value "https://example.com/path" with
   | None -> () | _ -> Alcotest.fail "path")

(* ---- Cors.parse_pattern ---- *)

let test_cpp_star () =
  (match Cors.parse_pattern "*" with Some Any -> () | _ -> Alcotest.fail "star")
let test_cpp_null () =
  (match Cors.parse_pattern "null" with Some Null -> () | _ -> Alcotest.fail "null")
let test_cpp_any_port () =
  (match Cors.parse_pattern "https://e.com:*" with
   | Some (Any_port {scheme="https";host="e.com"}) -> () | _ -> Alcotest.fail "any_port")
let test_cpp_exact () =
  (match Cors.parse_pattern "https://e.com:443" with
   | Some (Exact {scheme="https";host="e.com";port=Some 443}) -> () | _ -> Alcotest.fail "exact")
let test_cpp_invalid () =
  (match Cors.parse_pattern "not valid" with None -> () | _ -> Alcotest.fail "invalid")

(* ---- normalize_dir_prefix ---- *)

let test_ndp_empty () = check_os "empty" None (normalize_dir_prefix "")
let test_ndp_ws () = check_os "ws" None (normalize_dir_prefix "   ")
let test_ndp_root () = check_os "root" (Some "/") (normalize_dir_prefix "/")
let test_ndp_tmp () =
  let r = normalize_dir_prefix "/tmp" in
  check_b "some" true (r <> None);
  (match r with
   | Some s -> check_b "slash" true (s.[String.length s - 1] = '/')
   | None -> ())

(* ---- agent_request_json ---- *)

let mk_req () = {
  id = "req_t"; request_secret = "sec"; node = `Assoc [("type", `String "FRAME")];
  platform = "react"; prompt = "Convert"; context_digest = "d";
  priority = 5; created_at = 1e6; status = Pending;
  claimed_by = None; claim_token = None; claimed_at = None;
  last_heartbeat = None; attempts = 0; result = None; error = None; drifted = false;
}

let has_field k = function `Assoc fs -> List.mem_assoc k fs | _ -> false

let test_arj_minimal () =
  let j = agent_request_json ~include_node:false ~include_prompt:false (mk_req ()) in
  check_b "no node" false (has_field "node" j);
  check_b "no prompt" false (has_field "prompt" j);
  check_b "has id" true (has_field "id" j)

let test_arj_with_node () =
  let j = agent_request_json ~include_node:true ~include_prompt:false (mk_req ()) in
  check_b "has node" true (has_field "node" j);
  check_b "no prompt" false (has_field "prompt" j)

let test_arj_with_prompt () =
  let j = agent_request_json ~include_node:false ~include_prompt:true (mk_req ()) in
  check_b "no node" false (has_field "node" j);
  check_b "has prompt" true (has_field "prompt" j)

let test_arj_both () =
  let j = agent_request_json ~include_node:true ~include_prompt:true (mk_req ()) in
  check_b "node" true (has_field "node" j);
  check_b "prompt" true (has_field "prompt" j)

let test_arj_claim_hidden () =
  let r = mk_req () in r.claim_token <- Some "tok";
  let j = agent_request_json ~include_node:false ~include_prompt:false r in
  (match j with
   | `Assoc fs -> (match List.assoc_opt "claim_token" fs with
       | Some `Null -> () | _ -> Alcotest.fail "should be null")
   | _ -> Alcotest.fail "assoc")

let test_arj_claim_shown () =
  let r = mk_req () in r.claim_token <- Some "tok";
  let j = agent_request_json ~include_claim_token:true ~include_node:false ~include_prompt:false r in
  (match j with
   | `Assoc fs -> (match List.assoc_opt "claim_token" fs with
       | Some (`String "tok") -> () | _ -> Alcotest.fail "should show")
   | _ -> Alcotest.fail "assoc")

let test_arj_claimed () =
  let r = mk_req () in
  r.claimed_by <- Some "w1"; r.status <- Claimed;
  r.claimed_at <- Some 1e6; r.last_heartbeat <- Some 1e6; r.attempts <- 2;
  let j = agent_request_json ~include_node:false ~include_prompt:false r in
  (match j with
   | `Assoc fs ->
       (match List.assoc_opt "claimed_by" fs with
        | Some (`String "w1") -> () | _ -> Alcotest.fail "claimed_by");
       (match List.assoc_opt "status" fs with
        | Some (`String "claimed") -> () | _ -> Alcotest.fail "status");
       (match List.assoc_opt "attempts" fs with
        | Some (`Int 2) -> () | _ -> Alcotest.fail "attempts")
   | _ -> Alcotest.fail "assoc")

let test_arj_error_drifted () =
  let r = mk_req () in r.error <- Some "timeout"; r.drifted <- true;
  let j = agent_request_json ~include_node:false ~include_prompt:false r in
  (match j with
   | `Assoc fs ->
       (match List.assoc_opt "error" fs with
        | Some (`String "timeout") -> () | _ -> Alcotest.fail "error");
       (match List.assoc_opt "drifted" fs with
        | Some (`Bool true) -> () | _ -> Alcotest.fail "drifted")
   | _ -> Alcotest.fail "assoc")

(* ---- validate_reference_image_path ---- *)

let test_vrip_nonexistent () =
  (match validate_reference_image_path ~roots:[] ~max_bytes:1000 "/nonexistent.png" with
   | Error _ -> () | Ok _ -> Alcotest.fail "should error")

(* ============== Runner ============== *)

let () =
  Alcotest.run "mcp_protocol_eio_w11" [
    ("parse_json", [
      Alcotest.test_case "empty" `Quick test_parse_json_empty;
      Alcotest.test_case "whitespace" `Quick test_parse_json_whitespace;
      Alcotest.test_case "object" `Quick test_parse_json_object;
      Alcotest.test_case "array" `Quick test_parse_json_array;
      Alcotest.test_case "string" `Quick test_parse_json_string;
      Alcotest.test_case "number" `Quick test_parse_json_number;
      Alcotest.test_case "null" `Quick test_parse_json_null;
      Alcotest.test_case "bool" `Quick test_parse_json_bool;
      Alcotest.test_case "invalid" `Quick test_parse_json_invalid;
      Alcotest.test_case "truncated" `Quick test_parse_json_truncated;
    ]);
    ("get_string_field", [
      Alcotest.test_case "present" `Quick test_gsf_present;
      Alcotest.test_case "missing" `Quick test_gsf_missing;
      Alcotest.test_case "wrong_type" `Quick test_gsf_wrong_type;
      Alcotest.test_case "not_assoc" `Quick test_gsf_not_assoc;
      Alcotest.test_case "null" `Quick test_gsf_null_val;
      Alcotest.test_case "empty_assoc" `Quick test_gsf_empty_assoc;
    ]);
    ("get_int_field", [
      Alcotest.test_case "int" `Quick test_gif_int;
      Alcotest.test_case "float" `Quick test_gif_float;
      Alcotest.test_case "missing" `Quick test_gif_missing;
      Alcotest.test_case "wrong_type" `Quick test_gif_wrong_type;
      Alcotest.test_case "not_assoc" `Quick test_gif_not_assoc;
      Alcotest.test_case "null" `Quick test_gif_null;
      Alcotest.test_case "bool" `Quick test_gif_bool;
    ]);
    ("get_bool_field", [
      Alcotest.test_case "true" `Quick test_gbf_true;
      Alcotest.test_case "false" `Quick test_gbf_false;
      Alcotest.test_case "missing" `Quick test_gbf_missing;
      Alcotest.test_case "wrong_type" `Quick test_gbf_wrong_type;
      Alcotest.test_case "not_assoc" `Quick test_gbf_not_assoc;
    ]);
    ("get_payload_field", [
      Alcotest.test_case "string" `Quick test_gpf_string;
      Alcotest.test_case "object" `Quick test_gpf_object;
      Alcotest.test_case "missing" `Quick test_gpf_missing;
      Alcotest.test_case "not_assoc" `Quick test_gpf_not_assoc;
      Alcotest.test_case "null" `Quick test_gpf_null_val;
    ]);
    ("clamp_poll_ms", [
      Alcotest.test_case "neg" `Quick test_cpm_neg;
      Alcotest.test_case "zero" `Quick test_cpm_zero;
      Alcotest.test_case "mid" `Quick test_cpm_mid;
      Alcotest.test_case "at_max" `Quick test_cpm_at_max;
      Alcotest.test_case "over" `Quick test_cpm_over;
    ]);
    ("clamp_max_commands", [
      Alcotest.test_case "zero" `Quick test_cmc_zero;
      Alcotest.test_case "neg" `Quick test_cmc_neg;
      Alcotest.test_case "one" `Quick test_cmc_one;
      Alcotest.test_case "mid" `Quick test_cmc_mid;
      Alcotest.test_case "over" `Quick test_cmc_over;
      Alcotest.test_case "at_max" `Quick test_cmc_at_max;
    ]);
    ("cors_normalized_port", [
      Alcotest.test_case "some" `Quick test_cnp_some;
      Alcotest.test_case "http" `Quick test_cnp_http;
      Alcotest.test_case "https" `Quick test_cnp_https;
      Alcotest.test_case "unknown" `Quick test_cnp_unknown;
    ]);
    ("cors_matches_pattern", [
      Alcotest.test_case "any_origin" `Quick test_mp_any_origin;
      Alcotest.test_case "any_null" `Quick test_mp_any_null;
      Alcotest.test_case "null_null" `Quick test_mp_null_null;
      Alcotest.test_case "null_exact" `Quick test_mp_null_exact;
      Alcotest.test_case "null_any_port" `Quick test_mp_null_any_port;
      Alcotest.test_case "exact_default" `Quick test_mp_exact_default_port;
      Alcotest.test_case "host_mismatch" `Quick test_mp_exact_host_mismatch;
      Alcotest.test_case "scheme_mismatch" `Quick test_mp_exact_scheme_mismatch;
      Alcotest.test_case "port_mismatch" `Quick test_mp_exact_port_mismatch;
      Alcotest.test_case "any_port_match" `Quick test_mp_any_port_match;
      Alcotest.test_case "any_port_scheme" `Quick test_mp_any_port_scheme;
      Alcotest.test_case "origin_vs_null" `Quick test_mp_origin_vs_null;
    ]);
    ("classify_message", [
      Alcotest.test_case "request" `Quick test_cm_request;
      Alcotest.test_case "notif_null" `Quick test_cm_notif_null_id;
      Alcotest.test_case "notif_no_id" `Quick test_cm_notif_no_id;
      Alcotest.test_case "response_result" `Quick test_cm_response_result;
      Alcotest.test_case "response_error" `Quick test_cm_response_error;
      Alcotest.test_case "unknown_id" `Quick test_cm_unknown_id_only;
      Alcotest.test_case "unknown_bad" `Quick test_cm_unknown_bad_json;
      Alcotest.test_case "unknown_array" `Quick test_cm_unknown_array;
      Alcotest.test_case "unknown_no_method" `Quick test_cm_unknown_no_fields;
    ]);
    ("compact_json_string", [
      Alcotest.test_case "compact" `Quick test_cjs_compact;
      Alcotest.test_case "pretty" `Quick test_cjs_pretty;
      Alcotest.test_case "not_json" `Quick test_cjs_not_json;
      Alcotest.test_case "empty" `Quick test_cjs_empty;
      Alcotest.test_case "null" `Quick test_cjs_null;
      Alcotest.test_case "array" `Quick test_cjs_array;
    ]);
    ("format_sse_data", [
      Alcotest.test_case "empty" `Quick test_fsd_empty;
      Alcotest.test_case "single" `Quick test_fsd_single;
      Alcotest.test_case "json" `Quick test_fsd_json;
      Alcotest.test_case "multiline" `Quick test_fsd_multiline;
      Alcotest.test_case "pretty" `Quick test_fsd_pretty_json;
    ]);
    ("request_parse_positive_int", [
      Alcotest.test_case "valid" `Quick test_rpi_valid;
      Alcotest.test_case "one" `Quick test_rpi_one;
      Alcotest.test_case "zero" `Quick test_rpi_zero;
      Alcotest.test_case "neg" `Quick test_rpi_neg;
      Alcotest.test_case "abc" `Quick test_rpi_abc;
      Alcotest.test_case "float" `Quick test_rpi_float;
      Alcotest.test_case "large" `Quick test_rpi_large;
    ]);
    ("parse_positive_int", [
      Alcotest.test_case "valid" `Quick test_ppi_valid;
      Alcotest.test_case "zero" `Quick test_ppi_zero;
      Alcotest.test_case "neg" `Quick test_ppi_neg;
      Alcotest.test_case "garbage" `Quick test_ppi_garbage;
      Alcotest.test_case "empty" `Quick test_ppi_empty;
    ]);
    ("hex_of_bytes", [
      Alcotest.test_case "empty" `Quick test_hob_empty;
      Alcotest.test_case "ff" `Quick test_hob_ff;
      Alcotest.test_case "zero" `Quick test_hob_zero;
      Alcotest.test_case "deadbeef" `Quick test_hob_dead;
      Alcotest.test_case "ascii" `Quick test_hob_ascii;
      Alcotest.test_case "hello" `Quick test_hob_hello;
    ]);
    ("agent_status_to_string", [
      Alcotest.test_case "pending" `Quick test_ast_pending;
      Alcotest.test_case "claimed" `Quick test_ast_claimed;
      Alcotest.test_case "completed" `Quick test_ast_completed;
      Alcotest.test_case "failed" `Quick test_ast_failed;
    ]);
    ("constant_time_equal", [
      Alcotest.test_case "same" `Quick test_cte_same;
      Alcotest.test_case "diff" `Quick test_cte_diff;
      Alcotest.test_case "len" `Quick test_cte_len;
      Alcotest.test_case "empty" `Quick test_cte_empty;
      Alcotest.test_case "one_empty" `Quick test_cte_one_empty;
    ]);
    ("has_suffix", [
      Alcotest.test_case "match" `Quick test_hs_match;
      Alcotest.test_case "no" `Quick test_hs_no;
      Alcotest.test_case "empty" `Quick test_hs_empty;
      Alcotest.test_case "longer" `Quick test_hs_longer;
      Alcotest.test_case "exact" `Quick test_hs_exact;
    ]);
    ("split_csv", [
      Alcotest.test_case "basic" `Quick test_sc_basic;
      Alcotest.test_case "spaces" `Quick test_sc_spaces;
      Alcotest.test_case "empty" `Quick test_sc_empty;
      Alcotest.test_case "trailing" `Quick test_sc_trail;
      Alcotest.test_case "commas" `Quick test_sc_commas;
      Alcotest.test_case "single" `Quick test_sc_single;
    ]);
    ("trim", [
      Alcotest.test_case "spaces" `Quick test_tr_spaces;
      Alcotest.test_case "tabs" `Quick test_tr_tabs;
      Alcotest.test_case "empty" `Quick test_tr_empty;
      Alcotest.test_case "none" `Quick test_tr_none;
    ]);
    ("is_under_dir", [
      Alcotest.test_case "root" `Quick test_iud_root;
      Alcotest.test_case "match" `Quick test_iud_match;
      Alcotest.test_case "no" `Quick test_iud_no;
      Alcotest.test_case "partial" `Quick test_iud_partial;
    ]);
    ("is_public_path", [
      Alcotest.test_case "options" `Quick test_ipp_options;
      Alcotest.test_case "health" `Quick test_ipp_health;
      Alcotest.test_case "post_h" `Quick test_ipp_post_h;
      Alcotest.test_case "get_root" `Quick test_ipp_get_r;
      Alcotest.test_case "metrics" `Quick test_ipp_get_m;
    ]);
    ("normalize_env", [
      Alcotest.test_case "none" `Quick test_ne_none;
      Alcotest.test_case "empty" `Quick test_ne_empty;
      Alcotest.test_case "ws" `Quick test_ne_ws;
      Alcotest.test_case "val" `Quick test_ne_val;
    ]);
    ("validate_webhook_passcode", [
      Alcotest.test_case "no_secret_allow" `Quick test_vwp_no_secret_allow;
      Alcotest.test_case "no_secret_strict" `Quick test_vwp_no_secret_strict;
      Alcotest.test_case "valid" `Quick test_vwp_valid;
      Alcotest.test_case "invalid" `Quick test_vwp_invalid;
      Alcotest.test_case "empty" `Quick test_vwp_empty_passcode;
      Alcotest.test_case "ws" `Quick test_vwp_ws_passcode;
    ]);
    ("cors_origin_allowed", [
      Alcotest.test_case "star" `Quick test_coa_star;
      Alcotest.test_case "specific" `Quick test_coa_specific;
      Alcotest.test_case "mismatch" `Quick test_coa_mismatch;
      Alcotest.test_case "invalid" `Quick test_coa_invalid;
    ]);
    ("cors_parse_origin_value", [
      Alcotest.test_case "null" `Quick test_cpov_null;
      Alcotest.test_case "https" `Quick test_cpov_https;
      Alcotest.test_case "port" `Quick test_cpov_port;
      Alcotest.test_case "invalid" `Quick test_cpov_invalid;
      Alcotest.test_case "ftp" `Quick test_cpov_ftp;
      Alcotest.test_case "path" `Quick test_cpov_path;
    ]);
    ("cors_parse_pattern", [
      Alcotest.test_case "star" `Quick test_cpp_star;
      Alcotest.test_case "null" `Quick test_cpp_null;
      Alcotest.test_case "any_port" `Quick test_cpp_any_port;
      Alcotest.test_case "exact" `Quick test_cpp_exact;
      Alcotest.test_case "invalid" `Quick test_cpp_invalid;
    ]);
    ("normalize_dir_prefix", [
      Alcotest.test_case "empty" `Quick test_ndp_empty;
      Alcotest.test_case "ws" `Quick test_ndp_ws;
      Alcotest.test_case "root" `Quick test_ndp_root;
      Alcotest.test_case "tmp" `Quick test_ndp_tmp;
    ]);
    ("agent_request_json", [
      Alcotest.test_case "minimal" `Quick test_arj_minimal;
      Alcotest.test_case "with_node" `Quick test_arj_with_node;
      Alcotest.test_case "with_prompt" `Quick test_arj_with_prompt;
      Alcotest.test_case "both" `Quick test_arj_both;
      Alcotest.test_case "claim_hidden" `Quick test_arj_claim_hidden;
      Alcotest.test_case "claim_shown" `Quick test_arj_claim_shown;
      Alcotest.test_case "claimed" `Quick test_arj_claimed;
      Alcotest.test_case "error_drifted" `Quick test_arj_error_drifted;
    ]);
    ("validate_reference_image_path", [
      Alcotest.test_case "nonexistent" `Quick test_vrip_nonexistent;
    ]);
  ]
