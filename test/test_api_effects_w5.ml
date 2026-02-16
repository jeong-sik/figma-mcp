(** Coverage Wave 5: figma_api_eio.ml + figma_effects.ml remaining gaps

    Tests for pure functions not covered by existing tests:
    - figma_api_eio.ml: string helpers, body keyword matching, suggestion generators,
      truncation, HTML detection, DNS failure detection, EINVAL check, URL log strip,
      chunked edge cases, friendly error strings, retry_after JSON Float path
    - figma_effects.ml: neo4j parse edge cases, multi-effect sequences,
      mock handler composition, Perform chaining
*)

open Alcotest

(* ============== figma_api_eio: string_contains ============== *)

let test_string_contains_basic () =
  check bool "substring found" true
    (Figma_api_eio.string_contains "hello world" "world");
  check bool "substring not found" false
    (Figma_api_eio.string_contains "hello world" "xyz")

let test_string_contains_empty_sub () =
  check bool "empty substring always matches" true
    (Figma_api_eio.string_contains "anything" "")

let test_string_contains_empty_string () =
  check bool "empty string, non-empty sub" false
    (Figma_api_eio.string_contains "" "abc")

let test_string_contains_equal () =
  check bool "exact match" true
    (Figma_api_eio.string_contains "abc" "abc")

let test_string_contains_longer_sub () =
  check bool "sub longer than string" false
    (Figma_api_eio.string_contains "ab" "abcd")

(* ============== figma_api_eio: body_contains / body_contains_any ============== *)

let test_body_contains_case_insensitive () =
  check bool "case insensitive match" true
    (Figma_api_eio.body_contains "Invalid ID format" "invalid");
  check bool "case insensitive no match" false
    (Figma_api_eio.body_contains "OK response" "invalid")

let test_body_contains_any_some_match () =
  check bool "any match" true
    (Figma_api_eio.body_contains_any "File not found" ["file"; "missing"]);
  check bool "none match" false
    (Figma_api_eio.body_contains_any "OK response" ["error"; "fail"])

let test_body_contains_any_empty_keywords () =
  check bool "empty keywords list" false
    (Figma_api_eio.body_contains_any "anything" [])

(* ============== figma_api_eio: first_match ============== *)

let test_first_match_hit () =
  let result = Figma_api_eio.first_match "invalid id error" [
    (["invalid"; "id"], "matched-invalid-id");
    (["missing"], "matched-missing");
  ] "default-suggestion" in
  check string "first rule matched" "matched-invalid-id" result

let test_first_match_second_rule () =
  let result = Figma_api_eio.first_match "something is missing here" [
    (["invalid"; "id"], "matched-invalid-id");
    (["missing"], "matched-missing");
  ] "default-suggestion" in
  check string "second rule matched" "matched-missing" result

let test_first_match_no_hit () =
  let result = Figma_api_eio.first_match "all good" [
    (["invalid"; "id"], "bad");
    (["missing"], "also bad");
  ] "default-suggestion" in
  check string "default returned" "default-suggestion" result

(* ============== figma_api_eio: suggestion_for_400 ============== *)

let test_suggestion_400_invalid_id () =
  let s = Figma_api_eio.suggestion_for_400 "Invalid ID format in request" in
  check bool "mentions ID" true
    (Figma_api_eio.string_contains s "Invalid ID")

let test_suggestion_400_missing () =
  let s = Figma_api_eio.suggestion_for_400 "Missing required parameter" in
  check bool "mentions missing" true
    (Figma_api_eio.string_contains s "Missing")

let test_suggestion_400_node () =
  let s = Figma_api_eio.suggestion_for_400 "Node error occurred" in
  check bool "mentions node" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "node")

let test_suggestion_400_default () =
  let s = Figma_api_eio.suggestion_for_400 "some other error" in
  check bool "default suggestion" true
    (Figma_api_eio.string_contains s "Invalid request")

(* ============== figma_api_eio: suggestion_for_404 ============== *)

let test_suggestion_404_file () =
  let s = Figma_api_eio.suggestion_for_404 "File not found" in
  check bool "mentions file" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "file")

let test_suggestion_404_node () =
  let s = Figma_api_eio.suggestion_for_404 "Node not found" in
  check bool "mentions node" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "node")

let test_suggestion_404_version () =
  let s = Figma_api_eio.suggestion_for_404 "Version not found" in
  check bool "mentions version" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "version")

let test_suggestion_404_default () =
  let s = Figma_api_eio.suggestion_for_404 "generic 404" in
  check bool "default suggestion" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "not found")

(* ============== figma_api_eio: suggestion_for_403 ============== *)

let test_suggestion_403_scope () =
  let s = Figma_api_eio.suggestion_for_403 "file_variables:read scope required" in
  check bool "mentions scope" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "scope")

let test_suggestion_403_invalid_scope () =
  let s = Figma_api_eio.suggestion_for_403 "invalid scope for this token" in
  check bool "mentions scope" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "scope")

let test_suggestion_403_generic () =
  let s = Figma_api_eio.suggestion_for_403 "access denied" in
  check bool "mentions permission" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "permission")

(* ============== figma_api_eio: truncate_body ============== *)

let test_truncate_body_short () =
  let result = Figma_api_eio.truncate_body "short body" in
  check string "no truncation" "short body" result

let test_truncate_body_exactly_200 () =
  let body = String.make 200 'x' in
  let result = Figma_api_eio.truncate_body body in
  check int "not truncated" 200 (String.length result)

let test_truncate_body_over_200 () =
  let body = String.make 300 'y' in
  let result = Figma_api_eio.truncate_body body in
  check int "truncated to 203" 203 (String.length result);
  check bool "ends with ellipsis" true
    (String.length result >= 3 &&
     String.sub result (String.length result - 3) 3 = "...")

(* ============== figma_api_eio: is_html_response ============== *)

let test_is_html_response_true () =
  check bool "detects HTML" true
    (Figma_api_eio.is_html_response "  <HTML><body>error</body></HTML>");
  check bool "detects lowercase html" true
    (Figma_api_eio.is_html_response "<html><head></head></html>")

let test_is_html_response_false () =
  check bool "JSON not HTML" false
    (Figma_api_eio.is_html_response {|{"error": "not found"}|});
  check bool "empty not HTML" false
    (Figma_api_eio.is_html_response "")

(* ============== figma_api_eio: is_dns_failure ============== *)

let test_is_dns_failure_resolve () =
  let exn = Failure "Could not resolve host" in
  check bool "resolve keyword" true (Figma_api_eio.is_dns_failure exn)

let test_is_dns_failure_dns () =
  let exn = Failure "DNS lookup failed" in
  check bool "dns keyword" true (Figma_api_eio.is_dns_failure exn)

let test_is_dns_failure_other () =
  let exn = Failure "Connection refused" in
  check bool "not dns failure" false (Figma_api_eio.is_dns_failure exn)

(* ============== figma_api_eio: is_einval_error ============== *)

let test_is_einval_error_match () =
  let exn = Unix.Unix_error (Unix.EINVAL, "select", "") in
  check bool "EINVAL select" true (Figma_api_eio.is_einval_error exn)

let test_is_einval_error_wrong_func () =
  let exn = Unix.Unix_error (Unix.EINVAL, "read", "") in
  check bool "EINVAL read" false (Figma_api_eio.is_einval_error exn)

let test_is_einval_error_wrong_code () =
  let exn = Unix.Unix_error (Unix.EPERM, "select", "") in
  check bool "EPERM select" false (Figma_api_eio.is_einval_error exn)

let test_is_einval_error_other_exn () =
  let exn = Failure "not unix" in
  check bool "not unix error" false (Figma_api_eio.is_einval_error exn)

(* ============== figma_api_eio: strip_query_for_log ============== *)

let test_strip_query_with_query () =
  let result = Figma_api_eio.strip_query_for_log
    "https://api.figma.com/v1/files/abc?depth=2&version=123" in
  check string "query stripped" "https://api.figma.com/v1/files/abc" result

let test_strip_query_no_query () =
  let result = Figma_api_eio.strip_query_for_log
    "https://api.figma.com/v1/files/abc" in
  check string "unchanged" "https://api.figma.com/v1/files/abc" result

(* ============== figma_api_eio: api_error_to_friendly_string all variants ============== *)

let test_friendly_string_network () =
  let err = Figma_api_eio.Network_error "DNS resolution failed" in
  let s = Figma_api_eio.api_error_to_friendly_string err in
  check bool "contains dns" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "dns")

let test_friendly_string_json () =
  let err = Figma_api_eio.Json_error "unexpected token at position 0" in
  let s = Figma_api_eio.api_error_to_friendly_string err in
  check bool "contains invalid" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "invalid")

let test_friendly_string_timeout () =
  let err = Figma_api_eio.Timeout_error in
  let s = Figma_api_eio.api_error_to_friendly_string err in
  check bool "contains timed out" true
    (Figma_api_eio.string_contains (String.lowercase_ascii s) "timed out")

(* ============== figma_api_eio: get_retry_delay Network_error ============== *)

let test_retry_delay_network () =
  let err = Figma_api_eio.Network_error "DNS resolution failed" in
  let delay = Figma_api_eio.get_retry_delay err in
  check (float 0.01) "DNS delay is 1.0" 1.0 delay

let test_retry_delay_network_connect () =
  let err = Figma_api_eio.Network_error "connect: refused" in
  let delay = Figma_api_eio.get_retry_delay err in
  check (float 0.01) "connect delay is 2.0" 2.0 delay

(* ============== figma_api_eio: HTTP 429 JSON retry_after as Float ============== *)

let test_http_429_retry_after_json_float () =
  let body = {|{"retry_after": 30.5}|} in
  let recovery = Figma_api_eio.get_http_error_recovery 429 body None in
  check (float 0.01) "float retry_after from body" 30.5 recovery.retry_after

let test_http_429_retry_after_header_zero () =
  (* retry_after header is 0.0 or negative -> fallback to body *)
  let body = {|{"retry_after": 15}|} in
  let recovery = Figma_api_eio.get_http_error_recovery 429 body (Some 0.0) in
  check (float 0.01) "falls back to body" 15.0 recovery.retry_after

let test_http_429_retry_after_negative_header () =
  let body = {|{"retry_after": 20}|} in
  let recovery = Figma_api_eio.get_http_error_recovery 429 body (Some (-1.0)) in
  check (float 0.01) "negative header, falls back to body" 20.0 recovery.retry_after

let test_http_429_retry_after_no_json_field () =
  let body = {|{"other_field": 123}|} in
  let recovery = Figma_api_eio.get_http_error_recovery 429 body None in
  check (float 0.01) "default 60s" 60.0 recovery.retry_after

(* ============== figma_api_eio: parse_http_response with chunked ============== *)

let test_parse_http_response_chunked () =
  let response =
    "HTTP/1.1 200 OK\r\n\
     Transfer-Encoding: chunked\r\n\
     \r\n\
     5\r\nHello\r\n0\r\n\r\n"
  in
  let status, body = Figma_api_eio.parse_http_response response in
  check int "status 200" 200 status;
  check string "decoded body" "Hello" body

let test_parse_http_response_malformed_status () =
  let response = "GARBAGE LINE\r\n\r\nbody" in
  let status, _body = Figma_api_eio.parse_http_response response in
  check int "fallback to 500" 500 status

(* ============== figma_api_eio: decode_chunked edge cases ============== *)

let test_decode_chunked_invalid_hex () =
  (* invalid hex size -> treated as 0, terminates *)
  let chunked = "gg\r\ndata\r\n0\r\n\r\n" in
  let result = Figma_api_eio.decode_chunked chunked in
  check string "empty on bad hex" "" result

let test_decode_chunked_empty_input () =
  let result = Figma_api_eio.decode_chunked "" in
  check string "empty input" "" result

(* ============== figma_api_eio: get_frames_from_page edge cases ============== *)

let test_get_frames_missing_id () =
  let page = `Assoc [
    ("children", `List [
      `Assoc [("name", `String "NoId"); ("type", `String "FRAME")]
    ])
  ] in
  let frames = Figma_api_eio.get_frames_from_page page in
  check int "skipped - no id" 0 (List.length frames)

let test_get_frames_missing_name () =
  let page = `Assoc [
    ("children", `List [
      `Assoc [("id", `String "1:1"); ("type", `String "FRAME")]
    ])
  ] in
  let frames = Figma_api_eio.get_frames_from_page page in
  check int "skipped - no name" 0 (List.length frames)

let test_get_frames_non_assoc_child () =
  let page = `Assoc [
    ("children", `List [`String "not-an-object"])
  ] in
  let frames = Figma_api_eio.get_frames_from_page page in
  check int "skipped - non-assoc" 0 (List.length frames)

let test_get_frames_non_list_page () =
  let page = `Int 42 in
  let frames = Figma_api_eio.get_frames_from_page page in
  check int "empty for non-assoc page" 0 (List.length frames)

(* ============== figma_effects: parse_neo4j_response edge cases ============== *)

let test_parse_neo4j_code_only_error () =
  let body = {|{"results": [], "errors": [{"code": "Neo.Error", "message": ""}]}|} in
  let result = Figma_effects.parse_neo4j_response body in
  match result with
  | Error msg ->
      check bool "has error code" true
        (Figma_api_eio.string_contains msg "Neo.Error")
  | Ok _ -> fail "expected error"

let test_parse_neo4j_message_only_error () =
  let body = {|{"results": [], "errors": [{"code": "", "message": "something wrong"}]}|} in
  let result = Figma_effects.parse_neo4j_response body in
  match result with
  | Error msg ->
      check bool "has error message" true
        (Figma_api_eio.string_contains msg "something wrong")
  | Ok _ -> fail "expected error"

let test_parse_neo4j_multiple_errors () =
  let body = {|{"results": [], "errors": [
    {"code": "Error1", "message": "first"},
    {"code": "Error2", "message": "second"}
  ]}|} in
  let result = Figma_effects.parse_neo4j_response body in
  match result with
  | Error msg ->
      check bool "has first" true (Figma_api_eio.string_contains msg "Error1");
      check bool "has second" true (Figma_api_eio.string_contains msg "Error2")
  | Ok _ -> fail "expected error"

let test_parse_neo4j_missing_code_key () =
  (* error object without "code" key at all *)
  let body = {|{"results": [], "errors": [{"message": "oops"}]}|} in
  let result = Figma_effects.parse_neo4j_response body in
  match result with
  | Error msg ->
      check bool "has message" true (Figma_api_eio.string_contains msg "oops")
  | Ok _ -> fail "expected error"

(* ============== figma_effects: make_neo4j_statement edge cases ============== *)

let test_make_neo4j_statement_multiple_params () =
  let stmt = Figma_effects.make_neo4j_statement
    "MATCH (n) WHERE n.name = $name AND n.age = $age RETURN n"
    [("name", `String "test"); ("age", `Int 25)] in
  match stmt with
  | `Assoc fields ->
      (match List.assoc_opt "parameters" fields with
       | Some (`Assoc params) ->
           check bool "has name" true (List.assoc_opt "name" params = Some (`String "test"));
           check bool "has age" true (List.assoc_opt "age" params = Some (`Int 25))
       | _ -> fail "expected params assoc")
  | _ -> fail "expected assoc"

(* ============== figma_effects: mock handler multi-step sequences ============== *)

let test_mock_multi_effect_sequence () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.files "f1" (`Assoc [("name", `String "File1")]);
  Hashtbl.replace store.files "f2" (`Assoc [("name", `String "File2")]);
  let result = Figma_effects.run_with_mock store (fun () ->
    let r1 = Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f1" () in
    let r2 = Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f2" () in
    (r1, r2)
  ) in
  let (r1, r2) = result in
  (match r1 with
   | Ok json ->
       let name = Yojson.Safe.Util.(json |> member "name" |> to_string) in
       check string "file 1" "File1" name
   | Error e -> fail e);
  (match r2 with
   | Ok json ->
       let name = Yojson.Safe.Util.(json |> member "name" |> to_string) in
       check string "file 2" "File2" name
   | Error e -> fail e)

let test_mock_log_then_api () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.files "fk" (`Assoc [("ok", `Bool true)]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.log_info "fetching file...";
    Figma_effects.Perform.log_debug "debug details";
    let r = Figma_effects.Perform.get_file ~token:"tk" ~file_key:"fk" () in
    Figma_effects.Perform.log_info "done";
    r
  ) in
  match result with
  | Ok _ -> ()
  | Error e -> fail e

let test_mock_sleep_then_api () =
  let store = Figma_effects.create_mock_store () in
  store.me := Some (`Assoc [("id", `String "u1")]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.eio_sleep 0.5;
    Figma_effects.Perform.get_me ~token:"tk"
  ) in
  match result with
  | Ok json ->
      let id = Yojson.Safe.Util.(json |> member "id" |> to_string) in
      check string "user id" "u1" id
  | Error e -> fail e

(* ============== figma_effects: Perform with optional args for more branch coverage ============== *)

let test_perform_get_images_with_options () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.images "fk:1:1" (`Assoc [("images", `Assoc [("1:1", `String "url")])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_images ~token:"tk" ~file_key:"fk" ~node_ids:["1:1"]
      ~format:"svg" ~scale:1.0 ~use_absolute_bounds:true ~version:"v1" ()
  ) in
  match result with
  | Ok _ -> ()
  | Error e -> fail e

let test_perform_get_file_meta_with_version () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.file_meta "fk" (`Assoc [("version", `String "v2")]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_file_meta ~token:"tk" ~file_key:"fk" ~version:"v2" ()
  ) in
  match result with
  | Ok _ -> ()
  | Error e -> fail e

let test_perform_get_file_images_with_version () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.file_images "fk" (`Assoc [("data", `List [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_file_images ~token:"tk" ~file_key:"fk" ~version:"v3" ()
  ) in
  match result with
  | Ok _ -> ()
  | Error e -> fail e

let test_perform_get_nodes_with_options () =
  let store = Figma_effects.create_mock_store () in
  Hashtbl.replace store.nodes "fk:2:2,3:3" (`Assoc [("nodes", `Assoc [])]);
  let result = Figma_effects.run_with_mock store (fun () ->
    Figma_effects.Perform.get_nodes ~token:"tk" ~file_key:"fk" ~node_ids:["2:2"; "3:3"]
      ~depth:2 ~geometry:"paths" ~plugin_data:"shared" ~version:"v1" ()
  ) in
  match result with
  | Ok _ -> ()
  | Error e -> fail e

(* ============== figma_api_eio: header_value ============== *)

let test_header_value_found () =
  let headers = [("content-type", "application/json"); ("x-custom", "value")] in
  let result = Figma_api_eio.header_value headers "x-custom" in
  check (option string) "found" (Some "value") result

let test_header_value_not_found () =
  let headers = [("content-type", "application/json")] in
  let result = Figma_api_eio.header_value headers "x-missing" in
  check (option string) "not found" None result

let test_header_value_empty_list () =
  let result = Figma_api_eio.header_value [] "any" in
  check (option string) "empty headers" None result

(* ============== figma_api_eio: extract_pages with non-Assoc document ============== *)

let test_extract_pages_non_assoc_document () =
  let json = `Assoc [("document", `String "not-an-object")] in
  let pages = Figma_api_eio.extract_pages json in
  check int "no pages for non-assoc document" 0 (List.length pages)

let test_extract_pages_non_list_children () =
  let json = `Assoc [("document", `Assoc [("children", `String "not-a-list")])] in
  let pages = Figma_api_eio.extract_pages json in
  check int "no pages for non-list children" 0 (List.length pages)

(* ============== Test registration ============== *)

let string_helper_tests = [
  "string_contains basic", `Quick, test_string_contains_basic;
  "string_contains empty sub", `Quick, test_string_contains_empty_sub;
  "string_contains empty string", `Quick, test_string_contains_empty_string;
  "string_contains equal", `Quick, test_string_contains_equal;
  "string_contains longer sub", `Quick, test_string_contains_longer_sub;
]

let body_match_tests = [
  "body_contains case insensitive", `Quick, test_body_contains_case_insensitive;
  "body_contains_any some match", `Quick, test_body_contains_any_some_match;
  "body_contains_any empty keywords", `Quick, test_body_contains_any_empty_keywords;
  "first_match hit", `Quick, test_first_match_hit;
  "first_match second rule", `Quick, test_first_match_second_rule;
  "first_match no hit", `Quick, test_first_match_no_hit;
]

let suggestion_tests = [
  "400 invalid id", `Quick, test_suggestion_400_invalid_id;
  "400 missing", `Quick, test_suggestion_400_missing;
  "400 node", `Quick, test_suggestion_400_node;
  "400 default", `Quick, test_suggestion_400_default;
  "404 file", `Quick, test_suggestion_404_file;
  "404 node", `Quick, test_suggestion_404_node;
  "404 version", `Quick, test_suggestion_404_version;
  "404 default", `Quick, test_suggestion_404_default;
  "403 scope", `Quick, test_suggestion_403_scope;
  "403 invalid scope", `Quick, test_suggestion_403_invalid_scope;
  "403 generic", `Quick, test_suggestion_403_generic;
]

let truncate_tests = [
  "truncate short", `Quick, test_truncate_body_short;
  "truncate exactly 200", `Quick, test_truncate_body_exactly_200;
  "truncate over 200", `Quick, test_truncate_body_over_200;
]

let detection_tests = [
  "is_html_response true", `Quick, test_is_html_response_true;
  "is_html_response false", `Quick, test_is_html_response_false;
  "is_dns_failure resolve", `Quick, test_is_dns_failure_resolve;
  "is_dns_failure dns", `Quick, test_is_dns_failure_dns;
  "is_dns_failure other", `Quick, test_is_dns_failure_other;
  "is_einval_error match", `Quick, test_is_einval_error_match;
  "is_einval_error wrong func", `Quick, test_is_einval_error_wrong_func;
  "is_einval_error wrong code", `Quick, test_is_einval_error_wrong_code;
  "is_einval_error other exn", `Quick, test_is_einval_error_other_exn;
  "strip_query with query", `Quick, test_strip_query_with_query;
  "strip_query no query", `Quick, test_strip_query_no_query;
]

let friendly_error_tests = [
  "friendly network error", `Quick, test_friendly_string_network;
  "friendly json error", `Quick, test_friendly_string_json;
  "friendly timeout error", `Quick, test_friendly_string_timeout;
  "retry delay network dns", `Quick, test_retry_delay_network;
  "retry delay network connect", `Quick, test_retry_delay_network_connect;
]

let retry_after_429_tests = [
  "429 retry_after JSON float", `Quick, test_http_429_retry_after_json_float;
  "429 retry_after header zero", `Quick, test_http_429_retry_after_header_zero;
  "429 retry_after negative header", `Quick, test_http_429_retry_after_negative_header;
  "429 retry_after no json field", `Quick, test_http_429_retry_after_no_json_field;
]

let parse_response_extra_tests = [
  "parse chunked response", `Quick, test_parse_http_response_chunked;
  "parse malformed status", `Quick, test_parse_http_response_malformed_status;
  "decode chunked invalid hex", `Quick, test_decode_chunked_invalid_hex;
  "decode chunked empty input", `Quick, test_decode_chunked_empty_input;
]

let frames_edge_tests = [
  "get_frames missing id", `Quick, test_get_frames_missing_id;
  "get_frames missing name", `Quick, test_get_frames_missing_name;
  "get_frames non-assoc child", `Quick, test_get_frames_non_assoc_child;
  "get_frames non-list page", `Quick, test_get_frames_non_list_page;
  "extract_pages non-assoc document", `Quick, test_extract_pages_non_assoc_document;
  "extract_pages non-list children", `Quick, test_extract_pages_non_list_children;
]

let header_tests = [
  "header_value found", `Quick, test_header_value_found;
  "header_value not found", `Quick, test_header_value_not_found;
  "header_value empty list", `Quick, test_header_value_empty_list;
]

let neo4j_tests = [
  "neo4j code-only error", `Quick, test_parse_neo4j_code_only_error;
  "neo4j message-only error", `Quick, test_parse_neo4j_message_only_error;
  "neo4j multiple errors", `Quick, test_parse_neo4j_multiple_errors;
  "neo4j missing code key", `Quick, test_parse_neo4j_missing_code_key;
  "neo4j statement multi-params", `Quick, test_make_neo4j_statement_multiple_params;
]

let effects_sequence_tests = [
  "mock multi-effect sequence", `Quick, test_mock_multi_effect_sequence;
  "mock log then api", `Quick, test_mock_log_then_api;
  "mock sleep then api", `Quick, test_mock_sleep_then_api;
  "perform get_images with options", `Quick, test_perform_get_images_with_options;
  "perform get_file_meta with version", `Quick, test_perform_get_file_meta_with_version;
  "perform get_file_images with version", `Quick, test_perform_get_file_images_with_version;
  "perform get_nodes with options", `Quick, test_perform_get_nodes_with_options;
]

let () =
  run "API + Effects Coverage Wave 5" [
    "String Helpers", string_helper_tests;
    "Body Match", body_match_tests;
    "Suggestions", suggestion_tests;
    "Truncate Body", truncate_tests;
    "Detection Helpers", detection_tests;
    "Friendly Errors", friendly_error_tests;
    "429 Retry-After", retry_after_429_tests;
    "Parse Response Extra", parse_response_extra_tests;
    "Frames Edge Cases", frames_edge_tests;
    "Header Value", header_tests;
    "Neo4j Helpers", neo4j_tests;
    "Effects Sequences", effects_sequence_tests;
  ]
