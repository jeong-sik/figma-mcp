(** Coverage Wave 11: figma_api_eio.ml — pure functions only (no HTTP I/O).

    Targets:
    - string_contains
    - body_contains / body_contains_any
    - first_match
    - get_http_error_recovery
    - get_network_error_recovery
    - api_error_to_friendly_string
    - truncate_body
    - is_retryable_error
    - get_retry_delay
    - is_dns_failure
    - is_html_response
    - retry_after_of_headers
    - strip_query_for_log
*)

open Figma_api_eio

(* ---- string_contains ---- *)

let test_string_contains_basic () =
  Alcotest.(check bool) "substring present" true (string_contains "hello world" "world");
  Alcotest.(check bool) "substring absent" false (string_contains "hello world" "xyz")

let test_string_contains_empty_sub () =
  Alcotest.(check bool) "empty sub always matches" true (string_contains "anything" "")

let test_string_contains_empty_haystack () =
  Alcotest.(check bool) "empty haystack, non-empty sub" false (string_contains "" "a")

let test_string_contains_both_empty () =
  Alcotest.(check bool) "both empty" true (string_contains "" "")

let test_string_contains_equal () =
  Alcotest.(check bool) "exact match" true (string_contains "abc" "abc")

let test_string_contains_longer_sub () =
  Alcotest.(check bool) "sub longer than haystack" false (string_contains "ab" "abc")

let test_string_contains_at_end () =
  Alcotest.(check bool) "match at end" true (string_contains "foobar" "bar")

let test_string_contains_at_start () =
  Alcotest.(check bool) "match at start" true (string_contains "foobar" "foo")

(* ---- body_contains / body_contains_any ---- *)

let test_body_contains_case_insensitive () =
  Alcotest.(check bool) "case insensitive" true (body_contains "INVALID ID" "invalid");
  Alcotest.(check bool) "mixed case" true (body_contains "Invalid Id" "invalid")

let test_body_contains_no_match () =
  Alcotest.(check bool) "no match" false (body_contains "all good" "error")

let test_body_contains_any_some_match () =
  Alcotest.(check bool) "any match" true
    (body_contains_any "something missing here" ["error"; "missing"; "broken"])

let test_body_contains_any_none () =
  Alcotest.(check bool) "none match" false
    (body_contains_any "all good" ["error"; "missing"])

let test_body_contains_any_empty_list () =
  Alcotest.(check bool) "empty keyword list" false
    (body_contains_any "anything" [])

(* ---- first_match ---- *)

let test_first_match_hit () =
  let rules = [
    (["error"; "token"], "token suggestion");
    (["missing"], "missing suggestion");
  ] in
  Alcotest.(check string) "first rule matches"
    "token suggestion"
    (first_match "Error with Token" rules "default")

let test_first_match_second_rule () =
  let rules = [
    (["xyz"; "abc"], "first");
    (["missing"], "second");
  ] in
  Alcotest.(check string) "second rule matches"
    "second"
    (first_match "missing field" rules "default")

let test_first_match_default () =
  let rules = [
    (["never"], "nope");
  ] in
  Alcotest.(check string) "falls to default"
    "default"
    (first_match "nothing here" rules "default")

let test_first_match_empty_rules () =
  Alcotest.(check string) "empty rules"
    "fallback"
    (first_match "anything" [] "fallback")

(* ---- get_http_error_recovery ---- *)

let test_http_400_invalid_id () =
  let r = get_http_error_recovery 400 "invalid id format" None in
  Alcotest.(check bool) "not retryable" false r.retryable;
  Alcotest.(check bool) "mentions invalid ID" true (string_contains r.suggestion "Invalid ID")

let test_http_400_missing () =
  let r = get_http_error_recovery 400 "missing parameter" None in
  Alcotest.(check bool) "mentions missing" true (string_contains r.suggestion "Missing")

let test_http_400_node () =
  let r = get_http_error_recovery 400 "node error" None in
  Alcotest.(check bool) "mentions node" true (string_contains r.suggestion "Node")

let test_http_400_generic () =
  let r = get_http_error_recovery 400 "something else" None in
  Alcotest.(check bool) "generic 400" true (string_contains r.suggestion "Invalid request")

let test_http_401 () =
  let r = get_http_error_recovery 401 "" None in
  Alcotest.(check string) "message" "Auth error: Invalid or expired token" r.message;
  Alcotest.(check bool) "not retryable" false r.retryable

let test_http_403_scope () =
  let r = get_http_error_recovery 403 "file_variables:read scope required" None in
  Alcotest.(check bool) "mentions scope" true (string_contains r.suggestion "file_variables:read")

let test_http_403_invalid_scope () =
  let r = get_http_error_recovery 403 "invalid scope detected" None in
  Alcotest.(check bool) "mentions scope" true (string_contains r.suggestion "file_variables:read")

let test_http_403_generic () =
  let r = get_http_error_recovery 403 "no access" None in
  Alcotest.(check bool) "mentions permission" true (string_contains r.suggestion "permission")

let test_http_404_file () =
  let r = get_http_error_recovery 404 "file not accessible" None in
  Alcotest.(check bool) "mentions file" true (string_contains r.suggestion "File not found")

let test_http_404_node () =
  let r = get_http_error_recovery 404 "node not found" None in
  Alcotest.(check bool) "mentions node" true (string_contains r.suggestion "Node not found")

let test_http_404_version () =
  let r = get_http_error_recovery 404 "version not found" None in
  Alcotest.(check bool) "mentions version" true (string_contains r.suggestion "Version not found")

let test_http_404_generic () =
  let r = get_http_error_recovery 404 "who knows" None in
  Alcotest.(check bool) "generic 404" true (string_contains r.suggestion "Resource not found")

let test_http_429_with_header () =
  let r = get_http_error_recovery 429 "" (Some 30.0) in
  Alcotest.(check bool) "retryable" true r.retryable;
  Alcotest.(check bool) "retry 30s" true (Float.equal r.retry_after 30.0)

let test_http_429_with_body_int () =
  let body = {|{"retry_after": 45}|} in
  let r = get_http_error_recovery 429 body None in
  Alcotest.(check bool) "retry 45s" true (Float.equal r.retry_after 45.0)

let test_http_429_with_body_float () =
  let body = {|{"retry_after": 12.5}|} in
  let r = get_http_error_recovery 429 body None in
  Alcotest.(check bool) "retry 12.5s" true (Float.equal r.retry_after 12.5)

let test_http_429_bad_body () =
  let r = get_http_error_recovery 429 "not json" None in
  Alcotest.(check bool) "fallback 60s" true (Float.equal r.retry_after 60.0)

let test_http_429_body_no_field () =
  let body = {|{"other": 10}|} in
  let r = get_http_error_recovery 429 body None in
  Alcotest.(check bool) "fallback 60s" true (Float.equal r.retry_after 60.0)

let test_http_429_header_zero () =
  (* Header value 0.0 should fall through to body parsing *)
  let r = get_http_error_recovery 429 {|{"retry_after": 20}|} (Some 0.0) in
  Alcotest.(check bool) "fallback to body" true (Float.equal r.retry_after 20.0)

let test_http_429_header_negative () =
  let r = get_http_error_recovery 429 {|{"retry_after": 15}|} (Some (-1.0)) in
  Alcotest.(check bool) "fallback to body" true (Float.equal r.retry_after 15.0)

let test_http_500 () =
  let r = get_http_error_recovery 500 "" None in
  Alcotest.(check bool) "retryable" true r.retryable;
  Alcotest.(check bool) "retry 2s" true (Float.equal r.retry_after 2.0)

let test_http_502 () =
  let r = get_http_error_recovery 502 "" None in
  Alcotest.(check bool) "retryable" true r.retryable

let test_http_503 () =
  let r = get_http_error_recovery 503 "" None in
  Alcotest.(check bool) "retryable" true r.retryable

let test_http_504 () =
  let r = get_http_error_recovery 504 "" None in
  Alcotest.(check bool) "retryable" true r.retryable

let test_http_unknown () =
  let r = get_http_error_recovery 418 "" None in
  Alcotest.(check bool) "not retryable" false r.retryable;
  Alcotest.(check bool) "mentions code" true (string_contains r.message "418")

(* ---- get_network_error_recovery ---- *)

let test_network_dns () =
  let r = get_network_error_recovery "DNS resolution failed" in
  Alcotest.(check string) "message" "DNS resolution failed" r.message;
  Alcotest.(check bool) "retryable" true r.retryable

let test_network_connect () =
  let r = get_network_error_recovery "connect: connection refused" in
  Alcotest.(check string) "message" "Connection failed" r.message

let test_network_unix () =
  let r = get_network_error_recovery "Unix error: EPIPE" in
  Alcotest.(check string) "message" "System error" r.message

let test_network_other () =
  let r = get_network_error_recovery "something strange" in
  Alcotest.(check string) "message" "Network error" r.message;
  Alcotest.(check string) "suggestion is the msg" "something strange" r.suggestion

let test_network_empty () =
  let r = get_network_error_recovery "" in
  Alcotest.(check string) "message" "Network error" r.message

let test_network_short () =
  let r = get_network_error_recovery "DN" in
  Alcotest.(check string) "message" "Network error" r.message

(* ---- api_error_to_friendly_string ---- *)

let test_friendly_http () =
  let s = api_error_to_friendly_string (Http_error (401, "", None)) in
  Alcotest.(check bool) "contains auth" true (string_contains s "Auth error")

let test_friendly_json () =
  let s = api_error_to_friendly_string (Json_error "bad json") in
  Alcotest.(check bool) "contains json msg" true (string_contains s "bad json")

let test_friendly_network () =
  let s = api_error_to_friendly_string (Network_error "DNS failed") in
  Alcotest.(check bool) "contains DNS" true (string_contains s "DNS")

let test_friendly_timeout () =
  let s = api_error_to_friendly_string Timeout_error in
  Alcotest.(check bool) "contains timeout" true (string_contains s "timed out")

(* ---- truncate_body ---- *)

let test_truncate_short () =
  Alcotest.(check string) "short body unchanged" "hello" (truncate_body "hello")

let test_truncate_exact_200 () =
  let s = String.make 200 'x' in
  Alcotest.(check string) "exact 200" s (truncate_body s)

let test_truncate_201 () =
  let s = String.make 201 'x' in
  let result = truncate_body s in
  Alcotest.(check int) "length 203" 203 (String.length result);
  Alcotest.(check bool) "ends with ..." true (string_contains result "...")

let test_truncate_empty () =
  Alcotest.(check string) "empty" "" (truncate_body "")

(* ---- is_retryable_error ---- *)

let test_retryable_429 () =
  Alcotest.(check bool) "429 retryable" true (is_retryable_error (Http_error (429, "", None)))

let test_retryable_500 () =
  Alcotest.(check bool) "500 retryable" true (is_retryable_error (Http_error (500, "", None)))

let test_retryable_401 () =
  Alcotest.(check bool) "401 not retryable" false (is_retryable_error (Http_error (401, "", None)))

let test_retryable_network () =
  Alcotest.(check bool) "network retryable" true (is_retryable_error (Network_error "timeout"))

let test_retryable_timeout () =
  Alcotest.(check bool) "timeout retryable" true (is_retryable_error Timeout_error)

let test_retryable_json () =
  Alcotest.(check bool) "json not retryable" false (is_retryable_error (Json_error "bad"))

(* ---- get_retry_delay ---- *)

let test_delay_429 () =
  let d = get_retry_delay (Http_error (429, {|{"retry_after": 10}|}, None)) in
  Alcotest.(check bool) "delay 10s" true (Float.equal d 10.0)

let test_delay_500 () =
  let d = get_retry_delay (Http_error (500, "", None)) in
  Alcotest.(check bool) "delay 2s" true (Float.equal d 2.0)

let test_delay_timeout () =
  let d = get_retry_delay Timeout_error in
  Alcotest.(check bool) "delay 1s" true (Float.equal d 1.0)

let test_delay_json () =
  let d = get_retry_delay (Json_error "bad") in
  Alcotest.(check bool) "delay 0s" true (Float.equal d 0.0)

let test_delay_network_dns () =
  let d = get_retry_delay (Network_error "DNS failed") in
  Alcotest.(check bool) "delay 1s" true (Float.equal d 1.0)

let test_delay_network_connect () =
  let d = get_retry_delay (Network_error "connect refused") in
  Alcotest.(check bool) "delay 2s" true (Float.equal d 2.0)

(* ---- is_dns_failure ---- *)

let test_dns_failure_resolve () =
  Alcotest.(check bool) "resolve keyword" true
    (is_dns_failure (Failure "Could not resolve host"))

let test_dns_failure_dns () =
  Alcotest.(check bool) "dns keyword" true
    (is_dns_failure (Failure "DNS lookup failed"))

let test_dns_failure_case () =
  Alcotest.(check bool) "case insensitive" true
    (is_dns_failure (Failure "RESOLVE error"))

let test_dns_failure_no_match () =
  Alcotest.(check bool) "no dns-related keywords" false
    (is_dns_failure (Failure "connection timeout"))

let test_dns_failure_connection_failed () =
  Alcotest.(check bool) "Connection_failed with dns" true
    (is_dns_failure (Connection_failed "dns error in lookup"))

(* ---- is_html_response ---- *)

let test_html_basic () =
  Alcotest.(check bool) "html tag" true (is_html_response "<html><body>hi</body></html>")

let test_html_uppercase () =
  Alcotest.(check bool) "HTML uppercase" true (is_html_response "<HTML><BODY>Hi</BODY></HTML>")

let test_html_with_whitespace () =
  Alcotest.(check bool) "leading whitespace" true (is_html_response "  \n<html>...")

let test_html_json () =
  Alcotest.(check bool) "json not html" false (is_html_response {|{"key": "value"}|})

let test_html_empty () =
  Alcotest.(check bool) "empty not html" false (is_html_response "")

let test_html_partial () =
  Alcotest.(check bool) "partial tag" true (is_html_response "<html ")

(* ---- retry_after_of_headers ---- *)

let test_retry_after_present () =
  let h = Cohttp.Header.init_with "retry-after" "30" in
  Alcotest.(check (option (float 0.01))) "30s" (Some 30.0)
    (retry_after_of_headers h)

let test_retry_after_whitespace () =
  let h = Cohttp.Header.init_with "retry-after" "  45  " in
  Alcotest.(check (option (float 0.01))) "trimmed" (Some 45.0)
    (retry_after_of_headers h)

let test_retry_after_missing () =
  let h = Cohttp.Header.init () in
  Alcotest.(check (option (float 0.01))) "none" None
    (retry_after_of_headers h)

let test_retry_after_non_numeric () =
  let h = Cohttp.Header.init_with "retry-after" "Thu, 01 Jan 2099" in
  Alcotest.(check (option (float 0.01))) "date => None" None
    (retry_after_of_headers h)

let test_retry_after_zero () =
  let h = Cohttp.Header.init_with "retry-after" "0" in
  Alcotest.(check (option (float 0.01))) "zero" (Some 0.0)
    (retry_after_of_headers h)

(* ---- strip_query_for_log ---- *)

let test_strip_query_present () =
  Alcotest.(check string) "strips query"
    "https://api.figma.com/v1/files/abc"
    (strip_query_for_log "https://api.figma.com/v1/files/abc?token=secret")

let test_strip_query_absent () =
  Alcotest.(check string) "no query unchanged"
    "https://api.figma.com/v1/files/abc"
    (strip_query_for_log "https://api.figma.com/v1/files/abc")

let test_strip_query_empty () =
  Alcotest.(check string) "empty" "" (strip_query_for_log "")

let test_strip_query_only_question () =
  Alcotest.(check string) "just question mark"
    ""
    (strip_query_for_log "?foo")

let test_strip_query_multiple_question () =
  Alcotest.(check string) "first question mark wins"
    "url"
    (strip_query_for_log "url?a=1?b=2")

(* ---- Test suite registration ---- *)

let () =
  Alcotest.run "figma_api_eio pure functions (w11)" [
    ("string_contains", [
      Alcotest.test_case "basic" `Quick test_string_contains_basic;
      Alcotest.test_case "empty sub" `Quick test_string_contains_empty_sub;
      Alcotest.test_case "empty haystack" `Quick test_string_contains_empty_haystack;
      Alcotest.test_case "both empty" `Quick test_string_contains_both_empty;
      Alcotest.test_case "equal strings" `Quick test_string_contains_equal;
      Alcotest.test_case "sub longer" `Quick test_string_contains_longer_sub;
      Alcotest.test_case "match at end" `Quick test_string_contains_at_end;
      Alcotest.test_case "match at start" `Quick test_string_contains_at_start;
    ]);
    ("body_contains", [
      Alcotest.test_case "case insensitive" `Quick test_body_contains_case_insensitive;
      Alcotest.test_case "no match" `Quick test_body_contains_no_match;
      Alcotest.test_case "any - some match" `Quick test_body_contains_any_some_match;
      Alcotest.test_case "any - none" `Quick test_body_contains_any_none;
      Alcotest.test_case "any - empty list" `Quick test_body_contains_any_empty_list;
    ]);
    ("first_match", [
      Alcotest.test_case "first rule" `Quick test_first_match_hit;
      Alcotest.test_case "second rule" `Quick test_first_match_second_rule;
      Alcotest.test_case "default" `Quick test_first_match_default;
      Alcotest.test_case "empty rules" `Quick test_first_match_empty_rules;
    ]);
    ("get_http_error_recovery", [
      Alcotest.test_case "400 invalid id" `Quick test_http_400_invalid_id;
      Alcotest.test_case "400 missing" `Quick test_http_400_missing;
      Alcotest.test_case "400 node" `Quick test_http_400_node;
      Alcotest.test_case "400 generic" `Quick test_http_400_generic;
      Alcotest.test_case "401" `Quick test_http_401;
      Alcotest.test_case "403 scope" `Quick test_http_403_scope;
      Alcotest.test_case "403 invalid scope" `Quick test_http_403_invalid_scope;
      Alcotest.test_case "403 generic" `Quick test_http_403_generic;
      Alcotest.test_case "404 file" `Quick test_http_404_file;
      Alcotest.test_case "404 node" `Quick test_http_404_node;
      Alcotest.test_case "404 version" `Quick test_http_404_version;
      Alcotest.test_case "404 generic" `Quick test_http_404_generic;
      Alcotest.test_case "429 with header" `Quick test_http_429_with_header;
      Alcotest.test_case "429 body int" `Quick test_http_429_with_body_int;
      Alcotest.test_case "429 body float" `Quick test_http_429_with_body_float;
      Alcotest.test_case "429 bad body" `Quick test_http_429_bad_body;
      Alcotest.test_case "429 no field" `Quick test_http_429_body_no_field;
      Alcotest.test_case "429 header zero" `Quick test_http_429_header_zero;
      Alcotest.test_case "429 header negative" `Quick test_http_429_header_negative;
      Alcotest.test_case "500" `Quick test_http_500;
      Alcotest.test_case "502" `Quick test_http_502;
      Alcotest.test_case "503" `Quick test_http_503;
      Alcotest.test_case "504" `Quick test_http_504;
      Alcotest.test_case "unknown code" `Quick test_http_unknown;
    ]);
    ("get_network_error_recovery", [
      Alcotest.test_case "dns" `Quick test_network_dns;
      Alcotest.test_case "connect" `Quick test_network_connect;
      Alcotest.test_case "unix" `Quick test_network_unix;
      Alcotest.test_case "other" `Quick test_network_other;
      Alcotest.test_case "empty" `Quick test_network_empty;
      Alcotest.test_case "short msg" `Quick test_network_short;
    ]);
    ("api_error_to_friendly_string", [
      Alcotest.test_case "http" `Quick test_friendly_http;
      Alcotest.test_case "json" `Quick test_friendly_json;
      Alcotest.test_case "network" `Quick test_friendly_network;
      Alcotest.test_case "timeout" `Quick test_friendly_timeout;
    ]);
    ("truncate_body", [
      Alcotest.test_case "short" `Quick test_truncate_short;
      Alcotest.test_case "exact 200" `Quick test_truncate_exact_200;
      Alcotest.test_case "201 chars" `Quick test_truncate_201;
      Alcotest.test_case "empty" `Quick test_truncate_empty;
    ]);
    ("is_retryable_error", [
      Alcotest.test_case "429" `Quick test_retryable_429;
      Alcotest.test_case "500" `Quick test_retryable_500;
      Alcotest.test_case "401" `Quick test_retryable_401;
      Alcotest.test_case "network" `Quick test_retryable_network;
      Alcotest.test_case "timeout" `Quick test_retryable_timeout;
      Alcotest.test_case "json" `Quick test_retryable_json;
    ]);
    ("get_retry_delay", [
      Alcotest.test_case "429" `Quick test_delay_429;
      Alcotest.test_case "500" `Quick test_delay_500;
      Alcotest.test_case "timeout" `Quick test_delay_timeout;
      Alcotest.test_case "json" `Quick test_delay_json;
      Alcotest.test_case "network dns" `Quick test_delay_network_dns;
      Alcotest.test_case "network connect" `Quick test_delay_network_connect;
    ]);
    ("is_dns_failure", [
      Alcotest.test_case "resolve keyword" `Quick test_dns_failure_resolve;
      Alcotest.test_case "dns keyword" `Quick test_dns_failure_dns;
      Alcotest.test_case "case insensitive" `Quick test_dns_failure_case;
      Alcotest.test_case "no match" `Quick test_dns_failure_no_match;
      Alcotest.test_case "Connection_failed" `Quick test_dns_failure_connection_failed;
    ]);
    ("is_html_response", [
      Alcotest.test_case "basic" `Quick test_html_basic;
      Alcotest.test_case "uppercase" `Quick test_html_uppercase;
      Alcotest.test_case "whitespace" `Quick test_html_with_whitespace;
      Alcotest.test_case "json" `Quick test_html_json;
      Alcotest.test_case "empty" `Quick test_html_empty;
      Alcotest.test_case "partial tag" `Quick test_html_partial;
    ]);
    ("retry_after_of_headers", [
      Alcotest.test_case "present" `Quick test_retry_after_present;
      Alcotest.test_case "whitespace" `Quick test_retry_after_whitespace;
      Alcotest.test_case "missing" `Quick test_retry_after_missing;
      Alcotest.test_case "non-numeric" `Quick test_retry_after_non_numeric;
      Alcotest.test_case "zero" `Quick test_retry_after_zero;
    ]);
    ("strip_query_for_log", [
      Alcotest.test_case "with query" `Quick test_strip_query_present;
      Alcotest.test_case "without query" `Quick test_strip_query_absent;
      Alcotest.test_case "empty" `Quick test_strip_query_empty;
      Alcotest.test_case "only question" `Quick test_strip_query_only_question;
      Alcotest.test_case "multiple ?" `Quick test_strip_query_multiple_question;
    ]);
  ]
