(** Coverage Wave 9: mcp_protocol_eio.ml — pure/testable functions
    Target: 200+ new bisect_ppx points on mcp_protocol_eio.ml
    Avoids duplicating: test_protocol_cors_w5, test_protocol_agent_w5,
                        test_protocol_codegen_w5, test_protocol_api_w7 *)

open Mcp_protocol_eio

let check_s = Alcotest.(check string)
let check_b = Alcotest.(check bool)
let check_i = Alcotest.(check int)
let check_f msg ~eps expected actual =
  if Float.abs (expected -. actual) > eps then
    Alcotest.fail (Printf.sprintf "%s: expected %.6f got %.6f" msg expected actual)

(* ===== 1. config type + default_config ===== *)

let test_default_config_port () =
  check_i "port" 8933 default_config.port

let test_default_config_host () =
  check_s "host" "localhost" default_config.host

let test_default_config_max_connections () =
  check_i "max_connections" 64 default_config.max_connections

(* ===== 2. set_allow_no_auth ===== *)

let test_set_allow_no_auth_true () =
  let prev = !allow_no_auth in
  set_allow_no_auth true;
  check_b "allow_no_auth" true !allow_no_auth;
  set_allow_no_auth prev

let test_set_allow_no_auth_false () =
  let prev = !allow_no_auth in
  set_allow_no_auth false;
  check_b "allow_no_auth" false !allow_no_auth;
  set_allow_no_auth prev

(* ===== 3. agent_request_json — pure JSON builder ===== *)

let make_test_req ?(claimed_by=None) ?(claim_token=None)
    ?(claimed_at=None) ?(last_heartbeat=None)
    ?(drifted=false) ?(error=None) ?(attempts=0)
    ?(result=None) ?(status=Pending) ?(priority=0) () =
  {
    id = "req-test-001";
    request_secret = "secret-xyz";
    node = `Assoc [("type", `String "FRAME"); ("name", `String "Card")];
    platform = "react";
    prompt = "Convert to React";
    priority;
    context_digest = "abc123";
    status;
    attempts;
    claimed_by;
    claim_token;
    claimed_at;
    last_heartbeat;
    created_at = 1000.0;
    drifted;
    error;
    result;
  }

let get_field key json =
  match json with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let get_string key json =
  match get_field key json with
  | Some (`String s) -> s
  | _ -> ""

let get_int key json =
  match get_field key json with
  | Some (`Int i) -> i
  | _ -> -999

let get_bool key json =
  match get_field key json with
  | Some (`Bool b) -> b
  | _ -> false

let is_null key json =
  match get_field key json with
  | Some `Null -> true
  | None -> true
  | _ -> false

(* 3a: basic fields *)
let test_agent_request_json_basic () =
  let req = make_test_req () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  check_s "id" "req-test-001" (get_string "id" j);
  check_s "platform" "react" (get_string "platform" j);
  check_i "priority" 0 (get_int "priority" j);
  check_s "context_digest" "abc123" (get_string "context_digest" j);
  check_s "status" "pending" (get_string "status" j);
  check_i "attempts" 0 (get_int "attempts" j);
  check_b "drifted" false (get_bool "drifted" j);
  check_b "claimed_by null" true (is_null "claimed_by" j);
  check_b "claim_token null" true (is_null "claim_token" j);
  check_b "claimed_at null" true (is_null "claimed_at" j);
  check_b "last_heartbeat null" true (is_null "last_heartbeat" j);
  check_b "error null" true (is_null "error" j);
  (* age_sec should be positive (Unix.gettimeofday() - 1000.0) *)
  (match get_field "age_sec" j with
   | Some (`Float f) -> check_b "age_sec positive" true (f > 0.0)
   | _ -> Alcotest.fail "expected age_sec float");
  (* node/prompt not included *)
  check_b "no node" true (get_field "node" j = None);
  check_b "no prompt" true (get_field "prompt" j = None)

(* 3b: include_node *)
let test_agent_request_json_include_node () =
  let req = make_test_req () in
  let j = agent_request_json ~include_node:true ~include_prompt:false req in
  check_b "has node" true (get_field "node" j <> None);
  check_b "no prompt" true (get_field "prompt" j = None)

(* 3c: include_prompt *)
let test_agent_request_json_include_prompt () =
  let req = make_test_req () in
  let j = agent_request_json ~include_node:false ~include_prompt:true req in
  check_b "no node" true (get_field "node" j = None);
  check_s "prompt" "Convert to React" (get_string "prompt" j)

(* 3d: include both *)
let test_agent_request_json_include_both () =
  let req = make_test_req () in
  let j = agent_request_json ~include_node:true ~include_prompt:true req in
  check_b "has node" true (get_field "node" j <> None);
  check_s "prompt" "Convert to React" (get_string "prompt" j)

(* 3e: include_claim_token when token present *)
let test_agent_request_json_claim_token_shown () =
  let req = make_test_req ~claim_token:(Some "secret-tok") () in
  let j = agent_request_json ~include_claim_token:true ~include_node:false ~include_prompt:false req in
  check_s "claim_token" "secret-tok" (get_string "claim_token" j)

(* 3f: include_claim_token default (hidden) *)
let test_agent_request_json_claim_token_hidden () =
  let req = make_test_req ~claim_token:(Some "secret-tok") () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  check_b "claim_token null" true (is_null "claim_token" j)

(* 3g: claimed_by present *)
let test_agent_request_json_claimed_by () =
  let req = make_test_req ~claimed_by:(Some "agent-42") () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  check_s "claimed_by" "agent-42" (get_string "claimed_by" j)

(* 3h: claimed_at present *)
let test_agent_request_json_claimed_at () =
  let req = make_test_req ~claimed_at:(Some 2000.0) () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  (match get_field "claimed_at" j with
   | Some (`Float f) -> check_f "claimed_at" ~eps:0.01 2000.0 f
   | _ -> Alcotest.fail "expected claimed_at float")

(* 3i: last_heartbeat present *)
let test_agent_request_json_last_heartbeat () =
  let req = make_test_req ~last_heartbeat:(Some 3000.0) () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  (match get_field "last_heartbeat" j with
   | Some (`Float f) -> check_f "last_heartbeat" ~eps:0.01 3000.0 f
   | _ -> Alcotest.fail "expected last_heartbeat float")

(* 3j: error present *)
let test_agent_request_json_error () =
  let req = make_test_req ~error:(Some "timeout") () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  check_s "error" "timeout" (get_string "error" j)

(* 3k: drifted true *)
let test_agent_request_json_drifted () =
  let req = make_test_req ~drifted:true () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  check_b "drifted" true (get_bool "drifted" j)

(* 3l: all statuses *)
let test_agent_request_json_status_claimed () =
  let req = make_test_req ~status:Claimed () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  check_s "status" "claimed" (get_string "status" j)

let test_agent_request_json_status_completed () =
  let req = make_test_req ~status:Completed () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  check_s "status" "completed" (get_string "status" j)

let test_agent_request_json_status_failed () =
  let req = make_test_req ~status:Failed () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  check_s "status" "failed" (get_string "status" j)

(* 3m: high priority and attempts *)
let test_agent_request_json_priority_attempts () =
  let req = make_test_req ~priority:5 ~attempts:3 () in
  let j = agent_request_json ~include_node:false ~include_prompt:false req in
  check_i "priority" 5 (get_int "priority" j);
  check_i "attempts" 3 (get_int "attempts" j)

(* ===== 4. constant_time_equal ===== *)

let test_cte_equal () =
  check_b "same" true (constant_time_equal "abc" "abc")

let test_cte_different () =
  check_b "diff" false (constant_time_equal "abc" "xyz")

let test_cte_length_mismatch () =
  check_b "length" false (constant_time_equal "abc" "abcd")

let test_cte_empty () =
  check_b "empty" true (constant_time_equal "" "")

let test_cte_whitespace_trim () =
  check_b "trimmed" true (constant_time_equal "  abc  " "abc")

let test_cte_single_char () =
  check_b "single" true (constant_time_equal "x" "x")

let test_cte_single_char_diff () =
  check_b "single diff" false (constant_time_equal "x" "y")

(* ===== 5. validate_webhook_passcode ===== *)

let test_vwp_no_secret_no_auth () =
  (* allow_no_auth=true, secret=None -> Error *)
  match validate_webhook_passcode ~allow_no_auth:true ~secret_opt:None ~passcode:"" with
  | Error msg -> check_b "has message" true (String.length msg > 0)
  | Ok () -> Alcotest.fail "should error when no secret + allow_no_auth"

let test_vwp_no_secret_auth_required () =
  (* allow_no_auth=false, secret=None -> Ok (no passcode needed) *)
  match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:None ~passcode:"" with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)

let test_vwp_secret_match () =
  match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:(Some "s3cret") ~passcode:"s3cret" with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)

let test_vwp_secret_mismatch () =
  match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:(Some "s3cret") ~passcode:"wrong" with
  | Error msg -> check_b "has invalid" true (String.length msg > 0)
  | Ok () -> Alcotest.fail "should reject wrong passcode"

let test_vwp_secret_empty_passcode () =
  match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:(Some "s3cret") ~passcode:"" with
  | Error msg -> check_b "missing" true (String.length msg > 0)
  | Ok () -> Alcotest.fail "should reject empty passcode"

let test_vwp_secret_whitespace_passcode () =
  match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:(Some "s3cret") ~passcode:"   " with
  | Error msg -> check_b "missing trimmed" true (String.length msg > 0)
  | Ok () -> Alcotest.fail "should reject whitespace passcode"

let test_vwp_secret_with_whitespace_trimmed () =
  (* constant_time_equal trims whitespace *)
  match validate_webhook_passcode ~allow_no_auth:false ~secret_opt:(Some "s3cret") ~passcode:"  s3cret  " with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)

(* ===== 6. has_suffix ===== *)

let test_has_suffix_match () =
  check_b "png" true (has_suffix ~suffix:".png" "image.png")

let test_has_suffix_no_match () =
  check_b "jpg" false (has_suffix ~suffix:".png" "image.jpg")

let test_has_suffix_exact () =
  check_b "exact" true (has_suffix ~suffix:".png" ".png")

let test_has_suffix_empty_suffix () =
  check_b "empty suffix" true (has_suffix ~suffix:"" "anything")

let test_has_suffix_longer () =
  check_b "longer suffix" false (has_suffix ~suffix:"longerthanstring" "short")

let test_has_suffix_empty_string () =
  check_b "empty string nonempty suffix" false (has_suffix ~suffix:".png" "")

let test_has_suffix_both_empty () =
  check_b "both empty" true (has_suffix ~suffix:"" "")

(* ===== 7. split_csv ===== *)

let test_split_csv_simple () =
  let result = split_csv "a,b,c" in
  Alcotest.(check (list string)) "simple" ["a"; "b"; "c"] result

let test_split_csv_spaces () =
  let result = split_csv " a , b , c " in
  Alcotest.(check (list string)) "trimmed" ["a"; "b"; "c"] result

let test_split_csv_empty () =
  let result = split_csv "" in
  Alcotest.(check (list string)) "empty" [] result

let test_split_csv_trailing_comma () =
  let result = split_csv "a,b," in
  Alcotest.(check (list string)) "trailing" ["a"; "b"] result

let test_split_csv_single () =
  let result = split_csv "only" in
  Alcotest.(check (list string)) "single" ["only"] result

let test_split_csv_empty_fields () =
  let result = split_csv "a,,b,,,c" in
  Alcotest.(check (list string)) "skip empties" ["a"; "b"; "c"] result

(* ===== 8. is_under_dir ===== *)

let test_is_under_dir_root () =
  check_b "root matches all" true (is_under_dir ~dir_prefix:"/" "/any/path")

let test_is_under_dir_match () =
  check_b "prefix match" true (is_under_dir ~dir_prefix:"/home/user/" "/home/user/file.txt")

let test_is_under_dir_no_match () =
  check_b "no match" false (is_under_dir ~dir_prefix:"/home/user/" "/etc/file.txt")

let test_is_under_dir_exact () =
  check_b "exact" true (is_under_dir ~dir_prefix:"/home/" "/home/")

let test_is_under_dir_shorter () =
  check_b "shorter path" false (is_under_dir ~dir_prefix:"/home/user/" "/home")

(* ===== 9. is_public_path ===== *)

let test_is_public_options () =
  check_b "OPTIONS any" true (is_public_path `OPTIONS "/anything")

let test_is_public_health () =
  check_b "GET /health" true (is_public_path `GET "/health")

let test_is_public_post_health () =
  check_b "POST /health" false (is_public_path `POST "/health")

let test_is_public_get_other () =
  check_b "GET /mcp" false (is_public_path `GET "/mcp")

let test_is_public_post_mcp () =
  check_b "POST /mcp" false (is_public_path `POST "/mcp")

let test_is_public_options_health () =
  check_b "OPTIONS /health" true (is_public_path `OPTIONS "/health")

(* ===== 10. normalize_env ===== *)

let test_normalize_env_none () =
  check_b "None" true (normalize_env None = None)

let test_normalize_env_empty () =
  check_b "empty" true (normalize_env (Some "") = None)

let test_normalize_env_whitespace () =
  check_b "spaces" true (normalize_env (Some "   ") = None)

let test_normalize_env_value () =
  check_b "value" true (normalize_env (Some "hello") = Some "hello")

let test_normalize_env_trimmed () =
  check_b "trimmed" true (normalize_env (Some "  hello  ") = Some "hello")

(* ===== 11. max_js_safe_client_id / sse_client_id_mask ===== *)

let test_max_js_safe () =
  (* 2^53 - 1 = 9007199254740991 *)
  let expected = Int64.sub (Int64.shift_left 1L 53) 1L in
  check_b "max_js_safe" true (Int64.equal max_js_safe_client_id expected)

let test_sse_client_id_mask_positive () =
  check_b "mask positive" true (Int64.compare sse_client_id_mask 0L > 0)

let test_sse_client_id_mask_bound () =
  (* mask <= max_js_safe and mask <= max_int *)
  check_b "mask <= js safe" true (Int64.compare sse_client_id_mask max_js_safe_client_id <= 0);
  check_b "mask <= max_int" true (Int64.compare sse_client_id_mask (Int64.of_int max_int) <= 0)

(* ===== 12. Cors.allow_origin_value_of_origin_opt — branch coverage ===== *)

(* We need to temporarily set env vars for CORS mode tests *)
let with_env name value f =
  let prev = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect f ~finally:(fun () ->
    match prev with
    | Some v -> Unix.putenv name v
    | None ->
        (* Can't truly unsetenv in OCaml stdlib, set to empty *)
        Unix.putenv name "")

let test_cors_allow_origin_permissive () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    match Cors.allow_origin_value_of_origin_opt None with
    | Some "*" -> ()
    | Some s -> Alcotest.fail ("expected *, got " ^ s)
    | None -> Alcotest.fail "expected Some *")

let test_cors_allow_origin_permissive_with_origin () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    match Cors.allow_origin_value_of_origin_opt (Some "http://example.com") with
    | Some "*" -> ()
    | _ -> Alcotest.fail "permissive should always return *")

let test_cors_allow_origin_default () =
  (* Default mode is "restrict" when env is empty/unset (via Figma_config) *)
  with_env "FIGMA_MCP_CORS_MODE" "" (fun () ->
    with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "http://example.com" (fun () ->
      match Cors.allow_origin_value_of_origin_opt (Some "http://example.com") with
      | Some "http://example.com" -> ()
      | Some s -> Alcotest.fail ("expected origin, got " ^ s)
      | None -> Alcotest.fail "expected Some origin"))

let test_cors_allow_origin_restrict_allowed () =
  with_env "FIGMA_MCP_CORS_MODE" "restrict" (fun () ->
    with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "http://localhost:3000" (fun () ->
      match Cors.allow_origin_value_of_origin_opt (Some "http://localhost:3000") with
      | Some "http://localhost:3000" -> ()
      | Some s -> Alcotest.fail ("expected origin, got " ^ s)
      | None -> Alcotest.fail "expected Some origin"))

let test_cors_allow_origin_restrict_denied () =
  with_env "FIGMA_MCP_CORS_MODE" "restrict" (fun () ->
    with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "http://localhost:3000" (fun () ->
      match Cors.allow_origin_value_of_origin_opt (Some "http://evil.com") with
      | None -> ()
      | Some _ -> Alcotest.fail "should deny non-allowed origin"))

let test_cors_allow_origin_restrict_none () =
  with_env "FIGMA_MCP_CORS_MODE" "restrict" (fun () ->
    with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "http://localhost:3000" (fun () ->
      match Cors.allow_origin_value_of_origin_opt None with
      | None -> ()
      | Some _ -> Alcotest.fail "should deny None origin in restrict"))

(* ===== 13. Cors.is_allowed — branch coverage via Cors.mode ===== *)
(* is_allowed requires an Httpun.Reqd; we test allow_origin_value instead *)

(* ===== 14. process_mcp_request_sync ===== *)

let test_process_mcp_request_sync_invalid_json () =
  let server = Figma_mcp_protocol.create_server [] [] [] (fun _uri -> Error "not implemented") in
  let result = process_mcp_request_sync server "not json" in
  let json = Yojson.Safe.from_string result in
  (match json with
   | `Assoc fields ->
       check_b "has error" true (List.mem_assoc "error" fields)
   | _ -> Alcotest.fail "expected JSON object")

let test_process_mcp_request_sync_bad_jsonrpc () =
  let server = Figma_mcp_protocol.create_server [] [] [] (fun _uri -> Error "not implemented") in
  let result = process_mcp_request_sync server {|{"jsonrpc":"1.0","id":1,"method":"test"}|} in
  let json = Yojson.Safe.from_string result in
  (match json with
   | `Assoc fields ->
       check_b "has error" true (List.mem_assoc "error" fields)
   | _ -> Alcotest.fail "expected JSON object")

let test_process_mcp_request_sync_tools_list () =
  let server = Figma_mcp_protocol.create_server [] [] [] (fun _uri -> Error "not implemented") in
  (* Initialize first *)
  let init_req = {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}|} in
  let _ = process_mcp_request_sync server init_req in
  let result = process_mcp_request_sync server {|{"jsonrpc":"2.0","id":2,"method":"tools/list","params":null}|} in
  let json = Yojson.Safe.from_string result in
  (match json with
   | `Assoc fields ->
       check_b "has result" true (List.mem_assoc "result" fields)
   | _ -> Alcotest.fail "expected JSON object")

(* ===== 15. classify_message — extended branches ===== *)
(* Note: basic cases covered in test_protocol_cors_w5. We add edge cases. *)

let test_classify_response_with_error () =
  let json = {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"bad"}}|} in
  check_b "response with error" true (classify_message json = `Response)

let test_classify_notification_null_id () =
  let json = {|{"jsonrpc":"2.0","id":null,"method":"notifications/initialized"}|} in
  check_b "null id notification" true (classify_message json = `Notification)

let test_classify_unknown_no_method_no_result () =
  let json = {|{"jsonrpc":"2.0","id":1}|} in
  check_b "no method no result" true (classify_message json = `Unknown)

let test_classify_unknown_no_fields () =
  let json = {|{"jsonrpc":"2.0"}|} in
  check_b "no fields" true (classify_message json = `Unknown)

let test_classify_non_assoc () =
  let json = {|[1,2,3]|} in
  check_b "array" true (classify_message json = `Unknown)

(* ===== 16. Request.default_max_body_bytes ===== *)

let test_default_max_body_bytes () =
  check_i "50MB" (50 * 1024 * 1024) Request.default_max_body_bytes

(* ===== 17. compact_json_string — extra edge cases ===== *)

let test_compact_json_nested () =
  let input = {|{
    "key": [1, 2,
    3]
  }|} in
  let result = compact_json_string input in
  check_b "no newlines" true (not (String.contains result '\n'))

let test_compact_json_already_compact () =
  let input = {|{"a":"b"}|} in
  check_s "unchanged" {|{"a":"b"}|} (compact_json_string input)

let test_compact_json_not_json () =
  let input = "just plain text" in
  check_s "pass through" "just plain text" (compact_json_string input)

let test_compact_json_empty_object () =
  check_s "empty obj" "{}" (compact_json_string "{}")

let test_compact_json_empty_array () =
  check_s "empty arr" "[]" (compact_json_string "[]")

let test_compact_json_with_unicode () =
  let input = {|{"name": "카드 컴포넌트"}|} in
  let result = compact_json_string input in
  check_b "contains name" true (String.length result > 0);
  check_b "no newlines" true (not (String.contains result '\n'))

(* ===== 18. format_sse_data — extra edge cases ===== *)

let test_format_sse_multiline_json () =
  let input = "{\n  \"key\": \"value\"\n}" in
  let result = format_sse_data input in
  (* Should compact JSON to single line *)
  check_b "starts with data:" true (String.length result > 5);
  (* Should NOT have multiple data: lines *)
  let lines = String.split_on_char '\n' result in
  let data_lines = List.filter (fun l -> String.length l >= 5 && String.sub l 0 5 = "data:") lines in
  check_i "single data line" 1 (List.length data_lines)

let test_format_sse_empty () =
  check_s "empty" "data: " (format_sse_data "")

let test_format_sse_plain_text () =
  let result = format_sse_data "hello world" in
  check_b "starts with data:" true (String.length result > 5 && String.sub result 0 5 = "data:")

(* ===== 19. validate_reference_image_path ===== *)

let test_validate_ref_path_empty () =
  match validate_reference_image_path ~roots:[] ~max_bytes:1000 "" with
  | Error msg -> check_b "required" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should reject empty path"

let test_validate_ref_path_not_found () =
  match validate_reference_image_path ~roots:[] ~max_bytes:1000 "/nonexistent/file.png" with
  | Error msg -> check_b "not found" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should reject nonexistent path"

let test_validate_ref_path_not_png () =
  (* Create a temp file with .jpg extension *)
  let path = Filename.temp_file "test" ".jpg" in
  Fun.protect (fun () ->
    match validate_reference_image_path ~roots:[] ~max_bytes:1000000 path with
    | Error msg -> check_b "must be png" true (String.length msg > 0)
    | Ok _ -> Alcotest.fail "should reject non-png"
  ) ~finally:(fun () -> try Sys.remove path with _ -> ())

let test_validate_ref_path_valid_no_roots () =
  (* Create a temp .png file *)
  let path = Filename.temp_file "test" ".png" in
  Fun.protect (fun () ->
    let oc = open_out path in
    output_string oc "fake png data";
    close_out oc;
    match validate_reference_image_path ~roots:[] ~max_bytes:1000000 path with
    | Ok resolved -> check_b "resolved" true (String.length resolved > 0)
    | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  ) ~finally:(fun () -> try Sys.remove path with _ -> ())

let test_validate_ref_path_too_large () =
  let path = Filename.temp_file "test" ".png" in
  Fun.protect (fun () ->
    let oc = open_out path in
    output_string oc "fake png data";
    close_out oc;
    match validate_reference_image_path ~roots:[] ~max_bytes:5 path with
    | Error msg -> check_b "too large" true (String.length msg > 0)
    | Ok _ -> Alcotest.fail "should reject too large"
  ) ~finally:(fun () -> try Sys.remove path with _ -> ())

let test_validate_ref_path_under_root () =
  let tmpdir = Filename.get_temp_dir_name () in
  let path = Filename.temp_file "test" ".png" in
  Fun.protect (fun () ->
    let oc = open_out path in
    output_string oc "fake png data";
    close_out oc;
    match validate_reference_image_path ~roots:[tmpdir] ~max_bytes:1000000 path with
    | Ok _ -> ()
    | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  ) ~finally:(fun () -> try Sys.remove path with _ -> ())

let test_validate_ref_path_outside_root () =
  let path = Filename.temp_file "test" ".png" in
  Fun.protect (fun () ->
    let oc = open_out path in
    output_string oc "fake png data";
    close_out oc;
    match validate_reference_image_path ~roots:["/nonexistent/root"] ~max_bytes:1000000 path with
    | Error msg -> check_b "not allowed" true (String.length msg > 0)
    | Ok _ -> Alcotest.fail "should reject path outside root"
  ) ~finally:(fun () -> try Sys.remove path with _ -> ())

(* ===== 20. normalize_dir_prefix ===== *)

let test_normalize_dir_prefix_empty () =
  check_b "empty" true (normalize_dir_prefix "" = None)

let test_normalize_dir_prefix_spaces () =
  check_b "spaces" true (normalize_dir_prefix "   " = None)

let test_normalize_dir_prefix_root () =
  match normalize_dir_prefix "/" with
  | Some "/" -> ()
  | Some s -> Alcotest.fail ("expected /, got " ^ s)
  | None -> Alcotest.fail "expected Some"

let test_normalize_dir_prefix_adds_slash () =
  let tmpdir = Filename.get_temp_dir_name () in
  match normalize_dir_prefix tmpdir with
  | Some s ->
      check_b "ends with /" true (has_suffix ~suffix:"/" s)
  | None -> Alcotest.fail "expected Some"

let test_normalize_dir_prefix_keeps_slash () =
  let tmpdir = Filename.get_temp_dir_name () ^ "/" in
  match normalize_dir_prefix tmpdir with
  | Some s ->
      check_b "ends with /" true (has_suffix ~suffix:"/" s)
  | None -> Alcotest.fail "expected Some"

let test_normalize_dir_prefix_nonexistent () =
  (* realpath may fail for nonexistent paths; function should handle gracefully *)
  match normalize_dir_prefix "/this_path_probably_does_not_exist_xyz" with
  | Some s ->
      check_b "ends with /" true (has_suffix ~suffix:"/" s)
  | None -> Alcotest.fail "expected Some (fallback to original)"

(* ===== 21. Cors.headers_for_origin_opt — branch permutations ===== *)

let test_cors_headers_methods_only () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    let hdrs = Cors.headers_for_origin_opt (Some "http://localhost") ~include_methods:true ~include_headers:false in
    let has_methods = List.exists (fun (k, _) -> k = "access-control-allow-methods") hdrs in
    let has_hdrs = List.exists (fun (k, _) -> k = "access-control-allow-headers") hdrs in
    check_b "has methods" true has_methods;
    check_b "no headers" false has_hdrs)

let test_cors_headers_headers_only () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    let hdrs = Cors.headers_for_origin_opt (Some "http://localhost") ~include_methods:false ~include_headers:true in
    let has_methods = List.exists (fun (k, _) -> k = "access-control-allow-methods") hdrs in
    let has_hdrs = List.exists (fun (k, _) -> k = "access-control-allow-headers") hdrs in
    check_b "no methods" false has_methods;
    check_b "has headers" true has_hdrs)

let test_cors_headers_both () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    let hdrs = Cors.headers_for_origin_opt (Some "http://localhost") ~include_methods:true ~include_headers:true in
    let has_methods = List.exists (fun (k, _) -> k = "access-control-allow-methods") hdrs in
    let has_hdrs = List.exists (fun (k, _) -> k = "access-control-allow-headers") hdrs in
    check_b "has methods" true has_methods;
    check_b "has headers" true has_hdrs)

let test_cors_headers_neither () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    let hdrs = Cors.headers_for_origin_opt (Some "http://localhost") ~include_methods:false ~include_headers:false in
    let has_methods = List.exists (fun (k, _) -> k = "access-control-allow-methods") hdrs in
    let has_hdrs = List.exists (fun (k, _) -> k = "access-control-allow-headers") hdrs in
    check_b "no methods" false has_methods;
    check_b "no headers" false has_hdrs)

let test_cors_headers_restrict_denied () =
  with_env "FIGMA_MCP_CORS_MODE" "restrict" (fun () ->
    with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "http://localhost:3000" (fun () ->
      let hdrs = Cors.headers_for_origin_opt (Some "http://evil.com") ~include_methods:true ~include_headers:true in
      check_b "empty headers" true (hdrs = [])))

let test_cors_headers_vary_with_specific_origin () =
  with_env "FIGMA_MCP_CORS_MODE" "restrict" (fun () ->
    with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "http://localhost:3000" (fun () ->
      let hdrs = Cors.headers_for_origin_opt (Some "http://localhost:3000") ~include_methods:false ~include_headers:false in
      let has_vary = List.exists (fun (k, _) -> k = "vary") hdrs in
      check_b "has Vary header" true has_vary))

let test_cors_headers_no_vary_with_wildcard () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    let hdrs = Cors.headers_for_origin_opt None ~include_methods:false ~include_headers:false in
    let has_vary = List.exists (fun (k, _) -> k = "vary") hdrs in
    check_b "no Vary for *" false has_vary)

let test_cors_private_network () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    with_env "FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK" "true" (fun () ->
      let hdrs = Cors.headers_for_origin_opt (Some "http://localhost") ~include_methods:false ~include_headers:false in
      let has_private = List.exists (fun (k, _) -> k = "access-control-allow-private-network") hdrs in
      check_b "has private network" true has_private))

(* ===== 22. Cors module — mode branching ===== *)

let test_cors_mode_permissive () =
  with_env "FIGMA_MCP_CORS_MODE" "permissive" (fun () ->
    check_s "mode" "permissive" (Cors.mode ()))

let test_cors_mode_restrict () =
  with_env "FIGMA_MCP_CORS_MODE" "restrict" (fun () ->
    check_s "mode" "restrict" (Cors.mode ()))

let test_cors_mode_default () =
  (* Default mode is "restrict" when env is empty/unset *)
  with_env "FIGMA_MCP_CORS_MODE" "" (fun () ->
    check_s "mode empty -> restrict" "restrict" (Cors.mode ()))

(* ===== 23. agent_status_to_string — all branches (already in w5 but we verify completeness) ===== *)

let test_status_pending () =
  check_s "pending" "pending" (agent_status_to_string Pending)

let test_status_claimed () =
  check_s "claimed" "claimed" (agent_status_to_string Claimed)

let test_status_completed () =
  check_s "completed" "completed" (agent_status_to_string Completed)

let test_status_failed () =
  check_s "failed" "failed" (agent_status_to_string Failed)

(* ===== 24. Cors.allow_headers / Cors.allow_private_network ===== *)

let test_cors_allow_headers_default () =
  with_env "FIGMA_MCP_CORS_ALLOW_HEADERS" "" (fun () ->
    let h = Cors.allow_headers () in
    check_b "nonempty" true (String.length h > 0);
    check_b "has content-type" true (
      try ignore (Str.search_forward (Str.regexp_string "content-type") (String.lowercase_ascii h) 0); true
      with Not_found -> false))

let test_cors_allow_headers_custom () =
  with_env "FIGMA_MCP_CORS_ALLOW_HEADERS" "X-Custom,Authorization" (fun () ->
    check_s "custom" "X-Custom,Authorization" (Cors.allow_headers ()))

let test_cors_allow_private_network_false () =
  with_env "FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK" "false" (fun () ->
    check_b "false" false (Cors.allow_private_network ()))

let test_cors_allow_private_network_true () =
  with_env "FIGMA_MCP_CORS_ALLOW_PRIVATE_NETWORK" "true" (fun () ->
    check_b "true" true (Cors.allow_private_network ()))

(* ===== 25. Cors.allowed_origins ===== *)

let test_cors_allowed_origins_default () =
  (* When env var is empty, returns profile defaults (non-empty for compat) *)
  with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "" (fun () ->
    let origins = Cors.allowed_origins () in
    check_b "nonempty default" true (List.length origins > 0))

let test_cors_allowed_origins_csv () =
  with_env "FIGMA_MCP_CORS_ALLOWED_ORIGINS" "http://a.com,http://b.com" (fun () ->
    let origins = Cors.allowed_origins () in
    check_i "count" 2 (List.length origins))

(* ===== Runner ===== *)

let () =
  Alcotest.run "mcp_protocol_eio_w9" [
    ("config", [
      Alcotest.test_case "default port" `Quick test_default_config_port;
      Alcotest.test_case "default host" `Quick test_default_config_host;
      Alcotest.test_case "default max_connections" `Quick test_default_config_max_connections;
    ]);
    ("set_allow_no_auth", [
      Alcotest.test_case "set true" `Quick test_set_allow_no_auth_true;
      Alcotest.test_case "set false" `Quick test_set_allow_no_auth_false;
    ]);
    ("agent_request_json", [
      Alcotest.test_case "basic fields" `Quick test_agent_request_json_basic;
      Alcotest.test_case "include node" `Quick test_agent_request_json_include_node;
      Alcotest.test_case "include prompt" `Quick test_agent_request_json_include_prompt;
      Alcotest.test_case "include both" `Quick test_agent_request_json_include_both;
      Alcotest.test_case "claim token shown" `Quick test_agent_request_json_claim_token_shown;
      Alcotest.test_case "claim token hidden" `Quick test_agent_request_json_claim_token_hidden;
      Alcotest.test_case "claimed_by" `Quick test_agent_request_json_claimed_by;
      Alcotest.test_case "claimed_at" `Quick test_agent_request_json_claimed_at;
      Alcotest.test_case "last_heartbeat" `Quick test_agent_request_json_last_heartbeat;
      Alcotest.test_case "error" `Quick test_agent_request_json_error;
      Alcotest.test_case "drifted" `Quick test_agent_request_json_drifted;
      Alcotest.test_case "status claimed" `Quick test_agent_request_json_status_claimed;
      Alcotest.test_case "status completed" `Quick test_agent_request_json_status_completed;
      Alcotest.test_case "status failed" `Quick test_agent_request_json_status_failed;
      Alcotest.test_case "priority + attempts" `Quick test_agent_request_json_priority_attempts;
    ]);
    ("constant_time_equal", [
      Alcotest.test_case "equal" `Quick test_cte_equal;
      Alcotest.test_case "different" `Quick test_cte_different;
      Alcotest.test_case "length mismatch" `Quick test_cte_length_mismatch;
      Alcotest.test_case "empty" `Quick test_cte_empty;
      Alcotest.test_case "whitespace trim" `Quick test_cte_whitespace_trim;
      Alcotest.test_case "single char" `Quick test_cte_single_char;
      Alcotest.test_case "single char diff" `Quick test_cte_single_char_diff;
    ]);
    ("validate_webhook_passcode", [
      Alcotest.test_case "no secret + allow_no_auth" `Quick test_vwp_no_secret_no_auth;
      Alcotest.test_case "no secret + auth required" `Quick test_vwp_no_secret_auth_required;
      Alcotest.test_case "secret match" `Quick test_vwp_secret_match;
      Alcotest.test_case "secret mismatch" `Quick test_vwp_secret_mismatch;
      Alcotest.test_case "empty passcode" `Quick test_vwp_secret_empty_passcode;
      Alcotest.test_case "whitespace passcode" `Quick test_vwp_secret_whitespace_passcode;
      Alcotest.test_case "trimmed passcode" `Quick test_vwp_secret_with_whitespace_trimmed;
    ]);
    ("has_suffix", [
      Alcotest.test_case "match" `Quick test_has_suffix_match;
      Alcotest.test_case "no match" `Quick test_has_suffix_no_match;
      Alcotest.test_case "exact" `Quick test_has_suffix_exact;
      Alcotest.test_case "empty suffix" `Quick test_has_suffix_empty_suffix;
      Alcotest.test_case "longer suffix" `Quick test_has_suffix_longer;
      Alcotest.test_case "empty string" `Quick test_has_suffix_empty_string;
      Alcotest.test_case "both empty" `Quick test_has_suffix_both_empty;
    ]);
    ("split_csv", [
      Alcotest.test_case "simple" `Quick test_split_csv_simple;
      Alcotest.test_case "spaces" `Quick test_split_csv_spaces;
      Alcotest.test_case "empty" `Quick test_split_csv_empty;
      Alcotest.test_case "trailing comma" `Quick test_split_csv_trailing_comma;
      Alcotest.test_case "single" `Quick test_split_csv_single;
      Alcotest.test_case "empty fields" `Quick test_split_csv_empty_fields;
    ]);
    ("is_under_dir", [
      Alcotest.test_case "root" `Quick test_is_under_dir_root;
      Alcotest.test_case "match" `Quick test_is_under_dir_match;
      Alcotest.test_case "no match" `Quick test_is_under_dir_no_match;
      Alcotest.test_case "exact" `Quick test_is_under_dir_exact;
      Alcotest.test_case "shorter path" `Quick test_is_under_dir_shorter;
    ]);
    ("is_public_path", [
      Alcotest.test_case "OPTIONS" `Quick test_is_public_options;
      Alcotest.test_case "GET /health" `Quick test_is_public_health;
      Alcotest.test_case "POST /health" `Quick test_is_public_post_health;
      Alcotest.test_case "GET /mcp" `Quick test_is_public_get_other;
      Alcotest.test_case "POST /mcp" `Quick test_is_public_post_mcp;
      Alcotest.test_case "OPTIONS /health" `Quick test_is_public_options_health;
    ]);
    ("normalize_env", [
      Alcotest.test_case "None" `Quick test_normalize_env_none;
      Alcotest.test_case "empty" `Quick test_normalize_env_empty;
      Alcotest.test_case "whitespace" `Quick test_normalize_env_whitespace;
      Alcotest.test_case "value" `Quick test_normalize_env_value;
      Alcotest.test_case "trimmed" `Quick test_normalize_env_trimmed;
    ]);
    ("sse_client_id", [
      Alcotest.test_case "max_js_safe" `Quick test_max_js_safe;
      Alcotest.test_case "mask positive" `Quick test_sse_client_id_mask_positive;
      Alcotest.test_case "mask bound" `Quick test_sse_client_id_mask_bound;
    ]);
    ("cors_allow_origin_value", [
      Alcotest.test_case "permissive" `Quick test_cors_allow_origin_permissive;
      Alcotest.test_case "permissive with origin" `Quick test_cors_allow_origin_permissive_with_origin;
      Alcotest.test_case "default" `Quick test_cors_allow_origin_default;
      Alcotest.test_case "restrict allowed" `Quick test_cors_allow_origin_restrict_allowed;
      Alcotest.test_case "restrict denied" `Quick test_cors_allow_origin_restrict_denied;
      Alcotest.test_case "restrict none" `Quick test_cors_allow_origin_restrict_none;
    ]);
    ("cors_headers_for_origin_opt", [
      Alcotest.test_case "methods only" `Quick test_cors_headers_methods_only;
      Alcotest.test_case "headers only" `Quick test_cors_headers_headers_only;
      Alcotest.test_case "both" `Quick test_cors_headers_both;
      Alcotest.test_case "neither" `Quick test_cors_headers_neither;
      Alcotest.test_case "restrict denied" `Quick test_cors_headers_restrict_denied;
      Alcotest.test_case "vary with origin" `Quick test_cors_headers_vary_with_specific_origin;
      Alcotest.test_case "no vary wildcard" `Quick test_cors_headers_no_vary_with_wildcard;
      Alcotest.test_case "private network" `Quick test_cors_private_network;
    ]);
    ("cors_mode", [
      Alcotest.test_case "permissive" `Quick test_cors_mode_permissive;
      Alcotest.test_case "restrict" `Quick test_cors_mode_restrict;
      Alcotest.test_case "default" `Quick test_cors_mode_default;
    ]);
    ("cors_config", [
      Alcotest.test_case "allow_headers default" `Quick test_cors_allow_headers_default;
      Alcotest.test_case "allow_headers custom" `Quick test_cors_allow_headers_custom;
      Alcotest.test_case "private_network false" `Quick test_cors_allow_private_network_false;
      Alcotest.test_case "private_network true" `Quick test_cors_allow_private_network_true;
      Alcotest.test_case "allowed_origins default" `Quick test_cors_allowed_origins_default;
      Alcotest.test_case "allowed_origins csv" `Quick test_cors_allowed_origins_csv;
    ]);
    ("agent_status", [
      Alcotest.test_case "pending" `Quick test_status_pending;
      Alcotest.test_case "claimed" `Quick test_status_claimed;
      Alcotest.test_case "completed" `Quick test_status_completed;
      Alcotest.test_case "failed" `Quick test_status_failed;
    ]);
    ("process_mcp_request_sync", [
      Alcotest.test_case "invalid json" `Quick test_process_mcp_request_sync_invalid_json;
      Alcotest.test_case "bad jsonrpc" `Quick test_process_mcp_request_sync_bad_jsonrpc;
      Alcotest.test_case "tools/list" `Quick test_process_mcp_request_sync_tools_list;
    ]);
    ("classify_message_extra", [
      Alcotest.test_case "response with error" `Quick test_classify_response_with_error;
      Alcotest.test_case "null id notification" `Quick test_classify_notification_null_id;
      Alcotest.test_case "no method no result" `Quick test_classify_unknown_no_method_no_result;
      Alcotest.test_case "no fields" `Quick test_classify_unknown_no_fields;
      Alcotest.test_case "non assoc" `Quick test_classify_non_assoc;
    ]);
    ("request_module", [
      Alcotest.test_case "default_max_body_bytes" `Quick test_default_max_body_bytes;
    ]);
    ("compact_json_extra", [
      Alcotest.test_case "nested" `Quick test_compact_json_nested;
      Alcotest.test_case "already compact" `Quick test_compact_json_already_compact;
      Alcotest.test_case "not json" `Quick test_compact_json_not_json;
      Alcotest.test_case "empty object" `Quick test_compact_json_empty_object;
      Alcotest.test_case "empty array" `Quick test_compact_json_empty_array;
      Alcotest.test_case "unicode" `Quick test_compact_json_with_unicode;
    ]);
    ("format_sse_data_extra", [
      Alcotest.test_case "multiline json" `Quick test_format_sse_multiline_json;
      Alcotest.test_case "empty" `Quick test_format_sse_empty;
      Alcotest.test_case "plain text" `Quick test_format_sse_plain_text;
    ]);
    ("validate_reference_image_path", [
      Alcotest.test_case "empty path" `Quick test_validate_ref_path_empty;
      Alcotest.test_case "not found" `Quick test_validate_ref_path_not_found;
      Alcotest.test_case "not png" `Quick test_validate_ref_path_not_png;
      Alcotest.test_case "valid no roots" `Quick test_validate_ref_path_valid_no_roots;
      Alcotest.test_case "too large" `Quick test_validate_ref_path_too_large;
      Alcotest.test_case "under root" `Quick test_validate_ref_path_under_root;
      Alcotest.test_case "outside root" `Quick test_validate_ref_path_outside_root;
    ]);
    ("normalize_dir_prefix", [
      Alcotest.test_case "empty" `Quick test_normalize_dir_prefix_empty;
      Alcotest.test_case "spaces" `Quick test_normalize_dir_prefix_spaces;
      Alcotest.test_case "root" `Quick test_normalize_dir_prefix_root;
      Alcotest.test_case "adds slash" `Quick test_normalize_dir_prefix_adds_slash;
      Alcotest.test_case "keeps slash" `Quick test_normalize_dir_prefix_keeps_slash;
      Alcotest.test_case "nonexistent" `Quick test_normalize_dir_prefix_nonexistent;
    ]);
  ]
