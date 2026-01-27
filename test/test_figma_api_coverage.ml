(** Comprehensive tests for Figma_api_eio module

    Target: 25+ tests covering:
    - Error handling and recovery
    - URL utilities
    - HTTP response parsing
    - JSON utilities
    - Figma URL parsing
*)

open Alcotest

(** ============== Error Recovery Tests ============== *)

let test_http_400_error_recovery () =
  let recovery = Figma_api_eio.get_http_error_recovery 400 "" None in
  check string "message" "Invalid request" recovery.message;
  check bool "not retryable" false recovery.retryable;
  check (float 0.01) "no retry delay" 0.0 recovery.retry_after

let test_http_401_error_recovery () =
  let recovery = Figma_api_eio.get_http_error_recovery 401 "" None in
  check string "message" "Auth error: Invalid or expired token" recovery.message;
  check bool "not retryable" false recovery.retryable;
  check bool "has suggestion" true (String.length recovery.suggestion > 0)

let test_http_403_scope_error () =
  let body = "Token missing scope: file_variables:read" in
  let recovery = Figma_api_eio.get_http_error_recovery 403 body None in
  check string "message" "Access denied" recovery.message;
  check bool "suggestion mentions scope" true
    (String.length recovery.suggestion > 0 &&
     (try ignore (Str.search_forward (Str.regexp "scope") recovery.suggestion 0); true
      with Not_found -> false))

let test_http_403_permission_error () =
  let recovery = Figma_api_eio.get_http_error_recovery 403 "access denied" None in
  check string "message" "Access denied" recovery.message;
  check bool "not retryable" false recovery.retryable

let test_http_404_error_recovery () =
  let recovery = Figma_api_eio.get_http_error_recovery 404 "" None in
  check string "message" "Not found" recovery.message;
  check bool "not retryable" false recovery.retryable

let test_http_429_rate_limit_with_header () =
  let recovery = Figma_api_eio.get_http_error_recovery 429 "" (Some 120.0) in
  check string "message" "Rate limited" recovery.message;
  check bool "retryable" true recovery.retryable;
  check (float 0.01) "retry delay from header" 120.0 recovery.retry_after

let test_http_429_rate_limit_with_body () =
  let body = {|{"retry_after": 45}|} in
  let recovery = Figma_api_eio.get_http_error_recovery 429 body None in
  check (float 0.01) "retry delay from body" 45.0 recovery.retry_after

let test_http_429_rate_limit_default () =
  let recovery = Figma_api_eio.get_http_error_recovery 429 "invalid json" None in
  check (float 0.01) "default retry delay" 60.0 recovery.retry_after

let test_http_500_server_error () =
  let recovery = Figma_api_eio.get_http_error_recovery 500 "" None in
  check string "message" "Figma server error" recovery.message;
  check bool "retryable" true recovery.retryable;
  check (float 0.01) "retry delay" 2.0 recovery.retry_after

let test_http_502_gateway_error () =
  let recovery = Figma_api_eio.get_http_error_recovery 502 "" None in
  check bool "retryable" true recovery.retryable

let test_http_503_service_unavailable () =
  let recovery = Figma_api_eio.get_http_error_recovery 503 "" None in
  check bool "retryable" true recovery.retryable

let test_http_504_gateway_timeout () =
  let recovery = Figma_api_eio.get_http_error_recovery 504 "" None in
  check bool "retryable" true recovery.retryable

let test_http_unknown_error () =
  let recovery = Figma_api_eio.get_http_error_recovery 418 "" None in  (* I'm a teapot *)
  check bool "not retryable" false recovery.retryable

let test_network_dns_error () =
  let recovery = Figma_api_eio.get_network_error_recovery "DNS resolution failed" in
  check string "message" "DNS resolution failed" recovery.message;
  check bool "retryable" true recovery.retryable

let test_network_connection_error () =
  let recovery = Figma_api_eio.get_network_error_recovery "connect: Connection refused" in
  check string "message" "Connection failed" recovery.message;
  check bool "retryable" true recovery.retryable

let test_network_unix_error () =
  let recovery = Figma_api_eio.get_network_error_recovery "Unix error: EPIPE" in
  check string "message" "System error" recovery.message;
  check bool "retryable" true recovery.retryable

let test_network_generic_error () =
  let recovery = Figma_api_eio.get_network_error_recovery "Something went wrong" in
  check string "message" "Network error" recovery.message;
  check bool "retryable" true recovery.retryable

(** ============== Error Conversion Tests ============== *)

let test_api_error_to_string_http () =
  let err = Figma_api_eio.Http_error (404, "Not Found", None) in
  let s = Figma_api_eio.api_error_to_string err in
  check bool "contains HTTP code" true (String.length s > 0);
  check bool "contains 404" true
    (try ignore (Str.search_forward (Str.regexp "404") s 0); true
     with Not_found -> false)

let test_api_error_to_string_json () =
  let err = Figma_api_eio.Json_error "unexpected token" in
  let s = Figma_api_eio.api_error_to_string err in
  check bool "contains message" true
    (try ignore (Str.search_forward (Str.regexp "unexpected") s 0); true
     with Not_found -> false)

let test_api_error_to_string_network () =
  let err = Figma_api_eio.Network_error "connection reset" in
  let s = Figma_api_eio.api_error_to_string err in
  check bool "contains network" true
    (String.lowercase_ascii s |> fun l ->
     try ignore (Str.search_forward (Str.regexp "network") l 0); true
     with Not_found -> false)

let test_api_error_to_string_timeout () =
  let err = Figma_api_eio.Timeout_error in
  let s = Figma_api_eio.api_error_to_string err in
  check bool "contains timeout" true
    (String.lowercase_ascii s |> fun l ->
     try ignore (Str.search_forward (Str.regexp "timeout") l 0); true
     with Not_found -> false)

let test_api_error_to_friendly_string () =
  let err = Figma_api_eio.Http_error (429, "", Some 60.0) in
  let s = Figma_api_eio.api_error_to_friendly_string err in
  check bool "is friendly" true (String.length s > 0)

(** ============== Retryable Error Tests ============== *)

let test_is_retryable_http_429 () =
  let err = Figma_api_eio.Http_error (429, "", None) in
  check bool "429 is retryable" true (Figma_api_eio.is_retryable_error err)

let test_is_retryable_http_500 () =
  let err = Figma_api_eio.Http_error (500, "", None) in
  check bool "500 is retryable" true (Figma_api_eio.is_retryable_error err)

let test_is_retryable_http_404 () =
  let err = Figma_api_eio.Http_error (404, "", None) in
  check bool "404 is not retryable" false (Figma_api_eio.is_retryable_error err)

let test_is_retryable_json_error () =
  let err = Figma_api_eio.Json_error "parse error" in
  check bool "json error not retryable" false (Figma_api_eio.is_retryable_error err)

let test_is_retryable_network_error () =
  let err = Figma_api_eio.Network_error "connection refused" in
  check bool "network error is retryable" true (Figma_api_eio.is_retryable_error err)

let test_is_retryable_timeout () =
  let err = Figma_api_eio.Timeout_error in
  check bool "timeout is retryable" true (Figma_api_eio.is_retryable_error err)

(** ============== Retry Delay Tests ============== *)

let test_get_retry_delay_429 () =
  let err = Figma_api_eio.Http_error (429, "", Some 30.0) in
  check (float 0.01) "429 delay from header" 30.0 (Figma_api_eio.get_retry_delay err)

let test_get_retry_delay_500 () =
  let err = Figma_api_eio.Http_error (500, "", None) in
  check (float 0.01) "500 delay" 2.0 (Figma_api_eio.get_retry_delay err)

let test_get_retry_delay_json () =
  let err = Figma_api_eio.Json_error "error" in
  check (float 0.01) "json delay is 0" 0.0 (Figma_api_eio.get_retry_delay err)

let test_get_retry_delay_timeout () =
  let err = Figma_api_eio.Timeout_error in
  check (float 0.01) "timeout delay" 1.0 (Figma_api_eio.get_retry_delay err)

(** ============== Node ID Normalization Tests ============== *)

let test_normalize_node_id_hyphen () =
  let result = Figma_api_eio.normalize_node_id "123-456" in
  check string "hyphen to colon" "123:456" result

let test_normalize_node_id_colon () =
  let result = Figma_api_eio.normalize_node_id "123:456" in
  check string "colon stays" "123:456" result

let test_normalize_node_id_no_separator () =
  let result = Figma_api_eio.normalize_node_id "abc" in
  check string "no change" "abc" result

let test_normalize_node_ids_multiple () =
  let result = Figma_api_eio.normalize_node_ids ["1-2"; "3-4"; "5:6"] in
  check (list string) "all normalized" ["1:2"; "3:4"; "5:6"] result

let test_normalize_node_ids_empty () =
  let result = Figma_api_eio.normalize_node_ids [] in
  check (list string) "empty list" [] result

(** ============== URL Query Parameter Tests ============== *)

let test_add_param_with_value () =
  let params = Figma_api_eio.add_param "key" (Some "value") [] in
  check int "one param" 1 (List.length params);
  check (list string) "value" ["value"] (List.assoc "key" params)

let test_add_param_without_value () =
  let params = Figma_api_eio.add_param "key" None [] in
  check int "no params" 0 (List.length params)

let test_add_param_chain () =
  let params =
    []
    |> Figma_api_eio.add_param "a" (Some "1")
    |> Figma_api_eio.add_param "b" None
    |> Figma_api_eio.add_param "c" (Some "3")
  in
  check int "two params" 2 (List.length params)

let test_with_query_no_params () =
  let url = Figma_api_eio.with_query "https://api.figma.com/v1/files/abc" [] in
  check string "no query string" "https://api.figma.com/v1/files/abc" url

let test_with_query_single_param () =
  let url = Figma_api_eio.with_query "https://api.figma.com/v1/files/abc" [("depth", ["2"])] in
  check bool "has query param" true
    (try ignore (Str.search_forward (Str.regexp "depth=2") url 0); true
     with Not_found -> false)

let test_with_query_multiple_params () =
  let url = Figma_api_eio.with_query "https://api.figma.com/v1/files/abc"
    [("depth", ["2"]); ("version", ["123"])] in
  check bool "has depth" true
    (try ignore (Str.search_forward (Str.regexp "depth=2") url 0); true
     with Not_found -> false);
  check bool "has version" true
    (try ignore (Str.search_forward (Str.regexp "version=123") url 0); true
     with Not_found -> false)

(** ============== Chunked Encoding Tests ============== *)

let test_decode_chunked_simple () =
  let chunked = "5\r\nHello\r\n5\r\nWorld\r\n0\r\n\r\n" in
  let result = Figma_api_eio.decode_chunked chunked in
  check string "decoded" "HelloWorld" result

let test_decode_chunked_single () =
  let chunked = "4\r\nTest\r\n0\r\n\r\n" in
  let result = Figma_api_eio.decode_chunked chunked in
  check string "decoded" "Test" result

let test_decode_chunked_empty () =
  let chunked = "0\r\n\r\n" in
  let result = Figma_api_eio.decode_chunked chunked in
  check string "empty" "" result

let test_decode_chunked_hex () =
  (* a = 10 in hex *)
  let chunked = "a\r\n0123456789\r\n0\r\n\r\n" in
  let result = Figma_api_eio.decode_chunked chunked in
  check string "10 bytes" "0123456789" result

(** ============== HTTP Response Parsing Tests ============== *)

let test_parse_http_response_200 () =
  let response = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"ok\":true}" in
  let status, body = Figma_api_eio.parse_http_response response in
  check int "status 200" 200 status;
  check string "body" "{\"ok\":true}" body

let test_parse_http_response_404 () =
  let response = "HTTP/1.1 404 Not Found\r\n\r\n{\"error\":\"not found\"}" in
  let status, body = Figma_api_eio.parse_http_response response in
  check int "status 404" 404 status;
  check string "body" "{\"error\":\"not found\"}" body

let test_parse_http_response_500 () =
  let response = "HTTP/1.1 500 Internal Server Error\r\n\r\n" in
  let status, _ = Figma_api_eio.parse_http_response response in
  check int "status 500" 500 status

let test_parse_http_response_empty () =
  (* String.split_on_char '\n' "" returns [""] not [], so status extraction fails to 500 *)
  let status, body = Figma_api_eio.parse_http_response "" in
  check int "status 500 fallback" 500 status;
  check string "empty body" "" body  (* Actual behavior: empty string returns empty body *)

(** ============== JSON Utility Tests ============== *)

let test_json_string_valid () =
  let result = Figma_api_eio.json_string (`String "hello") in
  check (option string) "extracts string" (Some "hello") result

let test_json_string_invalid () =
  let result = Figma_api_eio.json_string (`Int 42) in
  check (option string) "returns None" None result

let test_json_string_null () =
  let result = Figma_api_eio.json_string `Null in
  check (option string) "returns None for null" None result

let test_json_int_from_int () =
  let result = Figma_api_eio.json_int (`Int 42) in
  check (option int) "extracts int" (Some 42) result

let test_json_int_from_float () =
  let result = Figma_api_eio.json_int (`Float 42.7) in
  check (option int) "truncates float" (Some 42) result

let test_json_int_invalid () =
  let result = Figma_api_eio.json_int (`String "42") in
  check (option string) "returns None" None (Option.map string_of_int result)

let test_json_field_exists () =
  let json = `Assoc [("name", `String "test"); ("id", `Int 123)] in
  let result = Figma_api_eio.json_field "name" json in
  check (option string) "finds field" (Some "test")
    (Option.bind result (function `String s -> Some s | _ -> None))

let test_json_field_missing () =
  let json = `Assoc [("name", `String "test")] in
  let result = Figma_api_eio.json_field "missing" json in
  check bool "returns None" true (Option.is_none result)

let test_json_field_not_object () =
  let json = `List [`Int 1; `Int 2] in
  let result = Figma_api_eio.json_field "key" json in
  check bool "returns None" true (Option.is_none result)

let test_member_alias () =
  let json = `Assoc [("key", `String "value")] in
  let result1 = Figma_api_eio.json_field "key" json in
  let result2 = Figma_api_eio.member "key" json in
  check bool "same result" true (result1 = result2)

(** ============== Document/Page Extraction Tests ============== *)

let test_extract_document () =
  let json = `Assoc [
    ("document", `Assoc [("id", `String "0:0"); ("name", `String "Document")])
  ] in
  let result = Figma_api_eio.extract_document json in
  check bool "extracts document" true (Option.is_some result)

let test_extract_document_missing () =
  let json = `Assoc [("other", `String "value")] in
  let result = Figma_api_eio.extract_document json in
  check bool "returns None" true (Option.is_none result)

let test_extract_pages () =
  let json = `Assoc [
    ("document", `Assoc [
      ("children", `List [
        `Assoc [("id", `String "1:0"); ("name", `String "Page 1")];
        `Assoc [("id", `String "2:0"); ("name", `String "Page 2")]
      ])
    ])
  ] in
  let pages = Figma_api_eio.extract_pages json in
  check int "two pages" 2 (List.length pages)

let test_extract_pages_empty () =
  let json = `Assoc [("document", `Assoc [])] in
  let pages = Figma_api_eio.extract_pages json in
  check int "no pages" 0 (List.length pages)

let test_extract_pages_no_document () =
  let json = `Assoc [] in
  let pages = Figma_api_eio.extract_pages json in
  check int "no pages" 0 (List.length pages)

let test_get_frames_from_page () =
  let page = `Assoc [
    ("children", `List [
      `Assoc [("id", `String "1:1"); ("name", `String "Frame1"); ("type", `String "FRAME")];
      `Assoc [("id", `String "1:2"); ("name", `String "Text1"); ("type", `String "TEXT")];
      `Assoc [("id", `String "1:3"); ("name", `String "Comp1"); ("type", `String "COMPONENT")]
    ])
  ] in
  let frames = Figma_api_eio.get_frames_from_page page in
  check int "two frames" 2 (List.length frames);
  let ids = List.map fst frames in
  check bool "has Frame1" true (List.mem "1:1" ids);
  check bool "has Comp1" true (List.mem "1:3" ids);
  check bool "no Text1" false (List.mem "1:2" ids)

let test_get_frames_from_page_component_set () =
  let page = `Assoc [
    ("children", `List [
      `Assoc [("id", `String "1:1"); ("name", `String "Variants"); ("type", `String "COMPONENT_SET")]
    ])
  ] in
  let frames = Figma_api_eio.get_frames_from_page page in
  check int "one frame" 1 (List.length frames)

let test_get_frames_from_page_empty () =
  let page = `Assoc [] in
  let frames = Figma_api_eio.get_frames_from_page page in
  check int "no frames" 0 (List.length frames)

let test_get_all_screens () =
  let json = `Assoc [
    ("document", `Assoc [
      ("children", `List [
        `Assoc [
          ("id", `String "1:0");
          ("name", `String "Page 1");
          ("children", `List [
            `Assoc [("id", `String "1:1"); ("name", `String "Screen1"); ("type", `String "FRAME")]
          ])
        ];
        `Assoc [
          ("id", `String "2:0");
          ("name", `String "Page 2");
          ("children", `List [
            `Assoc [("id", `String "2:1"); ("name", `String "Screen2"); ("type", `String "FRAME")]
          ])
        ]
      ])
    ])
  ] in
  let screens = Figma_api_eio.get_all_screens json in
  check int "two screens" 2 (List.length screens)

(** ============== URL Parsing Tests ============== *)

let test_parse_figma_url_file () =
  let info = Figma_api_eio.parse_figma_url "https://www.figma.com/file/ABC123/MyDesign" in
  check (option string) "file_key" (Some "ABC123") info.file_key;
  check (option string) "node_id" None info.node_id

let test_parse_figma_url_design () =
  let info = Figma_api_eio.parse_figma_url "https://www.figma.com/design/XYZ789/Test?node-id=100-200" in
  check (option string) "file_key" (Some "XYZ789") info.file_key;
  check (option string) "node_id normalized" (Some "100:200") info.node_id

let test_parse_figma_url_proto () =
  let info = Figma_api_eio.parse_figma_url "https://www.figma.com/proto/PROTO123/Prototype?node-id=1:1" in
  check (option string) "file_key" (Some "PROTO123") info.file_key;
  check (option string) "node_id" (Some "1:1") info.node_id

let test_parse_figma_url_team () =
  let info = Figma_api_eio.parse_figma_url "https://www.figma.com/files/team/12345/project/67890" in
  check (option string) "team_id" (Some "12345") info.team_id;
  check (option string) "project_id" (Some "67890") info.project_id;
  check (option string) "file_key" None info.file_key

let test_parse_figma_url_team_only () =
  let info = Figma_api_eio.parse_figma_url "https://www.figma.com/files/team/12345" in
  check (option string) "team_id" (Some "12345") info.team_id;
  check (option string) "project_id" None info.project_id

let test_parse_figma_url_invalid () =
  let info = Figma_api_eio.parse_figma_url "https://example.com/not-figma" in
  check (option string) "file_key" None info.file_key;
  check (option string) "team_id" None info.team_id

let test_parse_figma_url_empty () =
  let info = Figma_api_eio.parse_figma_url "" in
  check (option string) "file_key" None info.file_key

(** ============== Test Suites ============== *)

let error_recovery_tests = [
  "HTTP 400 error recovery", `Quick, test_http_400_error_recovery;
  "HTTP 401 error recovery", `Quick, test_http_401_error_recovery;
  "HTTP 403 scope error", `Quick, test_http_403_scope_error;
  "HTTP 403 permission error", `Quick, test_http_403_permission_error;
  "HTTP 404 error recovery", `Quick, test_http_404_error_recovery;
  "HTTP 429 with Retry-After header", `Quick, test_http_429_rate_limit_with_header;
  "HTTP 429 with body retry_after", `Quick, test_http_429_rate_limit_with_body;
  "HTTP 429 default delay", `Quick, test_http_429_rate_limit_default;
  "HTTP 500 server error", `Quick, test_http_500_server_error;
  "HTTP 502 gateway error", `Quick, test_http_502_gateway_error;
  "HTTP 503 service unavailable", `Quick, test_http_503_service_unavailable;
  "HTTP 504 gateway timeout", `Quick, test_http_504_gateway_timeout;
  "HTTP unknown error", `Quick, test_http_unknown_error;
  "Network DNS error", `Quick, test_network_dns_error;
  "Network connection error", `Quick, test_network_connection_error;
  "Network Unix error", `Quick, test_network_unix_error;
  "Network generic error", `Quick, test_network_generic_error;
]

let error_conversion_tests = [
  "api_error_to_string HTTP", `Quick, test_api_error_to_string_http;
  "api_error_to_string JSON", `Quick, test_api_error_to_string_json;
  "api_error_to_string network", `Quick, test_api_error_to_string_network;
  "api_error_to_string timeout", `Quick, test_api_error_to_string_timeout;
  "api_error_to_friendly_string", `Quick, test_api_error_to_friendly_string;
]

let retryable_tests = [
  "429 is retryable", `Quick, test_is_retryable_http_429;
  "500 is retryable", `Quick, test_is_retryable_http_500;
  "404 not retryable", `Quick, test_is_retryable_http_404;
  "JSON error not retryable", `Quick, test_is_retryable_json_error;
  "Network error retryable", `Quick, test_is_retryable_network_error;
  "Timeout retryable", `Quick, test_is_retryable_timeout;
]

let retry_delay_tests = [
  "429 delay from header", `Quick, test_get_retry_delay_429;
  "500 delay", `Quick, test_get_retry_delay_500;
  "JSON delay is 0", `Quick, test_get_retry_delay_json;
  "Timeout delay", `Quick, test_get_retry_delay_timeout;
]

let node_id_tests = [
  "normalize hyphen to colon", `Quick, test_normalize_node_id_hyphen;
  "normalize colon stays", `Quick, test_normalize_node_id_colon;
  "normalize no separator", `Quick, test_normalize_node_id_no_separator;
  "normalize multiple IDs", `Quick, test_normalize_node_ids_multiple;
  "normalize empty list", `Quick, test_normalize_node_ids_empty;
]

let url_param_tests = [
  "add_param with value", `Quick, test_add_param_with_value;
  "add_param without value", `Quick, test_add_param_without_value;
  "add_param chain", `Quick, test_add_param_chain;
  "with_query no params", `Quick, test_with_query_no_params;
  "with_query single param", `Quick, test_with_query_single_param;
  "with_query multiple params", `Quick, test_with_query_multiple_params;
]

let chunked_tests = [
  "decode simple chunked", `Quick, test_decode_chunked_simple;
  "decode single chunk", `Quick, test_decode_chunked_single;
  "decode empty chunked", `Quick, test_decode_chunked_empty;
  "decode hex chunk size", `Quick, test_decode_chunked_hex;
]

let http_response_tests = [
  "parse HTTP 200", `Quick, test_parse_http_response_200;
  "parse HTTP 404", `Quick, test_parse_http_response_404;
  "parse HTTP 500", `Quick, test_parse_http_response_500;
  "parse empty response", `Quick, test_parse_http_response_empty;
]

let json_util_tests = [
  "json_string valid", `Quick, test_json_string_valid;
  "json_string invalid", `Quick, test_json_string_invalid;
  "json_string null", `Quick, test_json_string_null;
  "json_int from int", `Quick, test_json_int_from_int;
  "json_int from float", `Quick, test_json_int_from_float;
  "json_int invalid", `Quick, test_json_int_invalid;
  "json_field exists", `Quick, test_json_field_exists;
  "json_field missing", `Quick, test_json_field_missing;
  "json_field not object", `Quick, test_json_field_not_object;
  "member alias", `Quick, test_member_alias;
]

let document_extraction_tests = [
  "extract_document", `Quick, test_extract_document;
  "extract_document missing", `Quick, test_extract_document_missing;
  "extract_pages", `Quick, test_extract_pages;
  "extract_pages empty", `Quick, test_extract_pages_empty;
  "extract_pages no document", `Quick, test_extract_pages_no_document;
  "get_frames_from_page", `Quick, test_get_frames_from_page;
  "get_frames component_set", `Quick, test_get_frames_from_page_component_set;
  "get_frames empty page", `Quick, test_get_frames_from_page_empty;
  "get_all_screens", `Quick, test_get_all_screens;
]

let url_parsing_tests = [
  "parse file URL", `Quick, test_parse_figma_url_file;
  "parse design URL with node", `Quick, test_parse_figma_url_design;
  "parse proto URL", `Quick, test_parse_figma_url_proto;
  "parse team/project URL", `Quick, test_parse_figma_url_team;
  "parse team only URL", `Quick, test_parse_figma_url_team_only;
  "parse invalid URL", `Quick, test_parse_figma_url_invalid;
  "parse empty URL", `Quick, test_parse_figma_url_empty;
]

let () =
  run "Figma_api_eio Coverage" [
    "Error Recovery", error_recovery_tests;
    "Error Conversion", error_conversion_tests;
    "Retryable Errors", retryable_tests;
    "Retry Delays", retry_delay_tests;
    "Node ID Normalization", node_id_tests;
    "URL Parameters", url_param_tests;
    "Chunked Encoding", chunked_tests;
    "HTTP Response Parsing", http_response_tests;
    "JSON Utilities", json_util_tests;
    "Document Extraction", document_extraction_tests;
    "URL Parsing", url_parsing_tests;
  ]
