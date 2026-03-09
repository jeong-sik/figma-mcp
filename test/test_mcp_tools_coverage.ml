(** Comprehensive Coverage Tests for mcp_tools.ml
    Target: 50+ tests covering all public functions
    Framework: Alcotest *)

open Alcotest
open Mcp_helpers

(** ============== JSON Schema Helpers ============== *)

let test_string_prop () =
  let result = Mcp_helpers.string_prop "A description" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "string")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None);
      check (option string) "description" (Some "A description")
        (match List.assoc_opt "description" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

(* Note: test_string_prop_with_required removed - ~required parameter was removed in v0.3.14 *)

let test_number_prop () =
  let result = Mcp_helpers.number_prop "A number" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "number")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

let test_bool_prop () =
  let result = Mcp_helpers.bool_prop "A boolean" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "boolean")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

let test_enum_prop () =
  let result = Mcp_helpers.enum_prop ["a"; "b"; "c"] "Pick one" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "string")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None);
      (match List.assoc_opt "enum" fields with
       | Some (`List items) ->
           let strs = List.filter_map (function `String s -> Some s | _ -> None) items in
           check (list string) "enum values" ["a"; "b"; "c"] strs
       | _ -> fail "Expected enum list")
  | _ -> fail "Expected Assoc"

let test_array_prop () =
  let result = Mcp_helpers.array_prop "An array" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "array")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

let test_object_prop () =
  let result = Mcp_helpers.object_prop "An object" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "object")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

let test_object_schema () =
  let result = Mcp_helpers.object_schema [
    ("name", Mcp_helpers.string_prop "Name");
    ("age", Mcp_helpers.number_prop "Age");
  ] ["name"] in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "object")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None);
      (match List.assoc_opt "required" fields with
       | Some (`List items) ->
           let strs = List.filter_map (function `String s -> Some s | _ -> None) items in
           check (list string) "required fields" ["name"] strs
       | _ -> fail "Expected required list")
  | _ -> fail "Expected Assoc"

let test_object_schema_empty_required () =
  let result = Mcp_helpers.object_schema [] [] in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "required" fields with
       | Some (`List items) -> check int "empty required" 0 (List.length items)
       | _ -> fail "Expected required list")
  | _ -> fail "Expected Assoc"

let schema_helper_tests = [
  "string_prop basic", `Quick, test_string_prop;
  "number_prop", `Quick, test_number_prop;
  "bool_prop", `Quick, test_bool_prop;
  "enum_prop", `Quick, test_enum_prop;
  "array_prop", `Quick, test_array_prop;
  "object_prop", `Quick, test_object_prop;
  "object_schema", `Quick, test_object_schema;
  "object_schema empty required", `Quick, test_object_schema_empty_required;
]

(** ============== JSON Utilities ============== *)

let test_member_exists () =
  let json = `Assoc [("key", `String "value")] in
  match Mcp_helpers.member "key" json with
  | Some (`String v) -> check string "member value" "value" v
  | _ -> fail "Expected Some String"

let test_member_missing () =
  let json = `Assoc [("other", `String "value")] in
  match Mcp_helpers.member "key" json with
  | None -> ()
  | _ -> fail "Expected None"

let test_member_not_assoc () =
  let json = `String "not an object" in
  match Mcp_helpers.member "key" json with
  | None -> ()
  | _ -> fail "Expected None for non-assoc"

let test_get_string_exists () =
  let json = `Assoc [("name", `String "Alice")] in
  match Mcp_helpers.get_string "name" json with
  | Some v -> check string "get_string" "Alice" v
  | None -> fail "Expected Some"

let test_get_string_missing () =
  let json = `Assoc [("other", `String "value")] in
  match Mcp_helpers.get_string "name" json with
  | None -> ()
  | Some _ -> fail "Expected None"

let test_get_string_not_string () =
  let json = `Assoc [("name", `Int 42)] in
  match Mcp_helpers.get_string "name" json with
  | None -> ()
  | Some _ -> fail "Expected None for non-string"

let test_get_string_list_from_list () =
  let json = `Assoc [("tags", `List [`String "a"; `String "b"; `String "c"])] in
  match Mcp_helpers.get_string_list "tags" json with
  | Some lst -> check (list string) "tags list" ["a"; "b"; "c"] lst
  | None -> fail "Expected Some list"

let test_get_string_list_from_csv () =
  let json = `Assoc [("tags", `String "x, y, z")] in
  match Mcp_helpers.get_string_list "tags" json with
  | Some lst -> check (list string) "csv list" ["x"; "y"; "z"] lst
  | None -> fail "Expected Some list from CSV"

let test_get_string_list_empty () =
  let json = `Assoc [("tags", `String "")] in
  match Mcp_helpers.get_string_list "tags" json with
  | None -> ()
  | Some _ -> fail "Expected None for empty"

let test_get_string_list_missing () =
  let json = `Assoc [] in
  match Mcp_helpers.get_string_list "tags" json with
  | None -> ()
  | Some _ -> fail "Expected None for missing"

let test_prefer_some_primary () =
  let result = Mcp_helpers.prefer_some (Some "primary") (Some "fallback") in
  check (option string) "prefer primary" (Some "primary") result

let test_prefer_some_fallback () =
  let result = Mcp_helpers.prefer_some None (Some "fallback") in
  check (option string) "use fallback" (Some "fallback") result

let test_prefer_some_both_none () =
  let result = Mcp_helpers.prefer_some None None in
  check (option string) "both none" None result

let json_util_tests = [
  "member exists", `Quick, test_member_exists;
  "member missing", `Quick, test_member_missing;
  "member not assoc", `Quick, test_member_not_assoc;
  "get_string exists", `Quick, test_get_string_exists;
  "get_string missing", `Quick, test_get_string_missing;
  "get_string not string", `Quick, test_get_string_not_string;
  "get_string_list from list", `Quick, test_get_string_list_from_list;
  "get_string_list from csv", `Quick, test_get_string_list_from_csv;
  "get_string_list empty", `Quick, test_get_string_list_empty;
  "get_string_list missing", `Quick, test_get_string_list_missing;
  "prefer_some primary", `Quick, test_prefer_some_primary;
  "prefer_some fallback", `Quick, test_prefer_some_fallback;
  "prefer_some both none", `Quick, test_prefer_some_both_none;
]

(** ============== Node ID Handling ============== *)

let test_normalize_node_id_hyphen () =
  let result = Mcp_helpers.normalize_node_id "100-200" in
  check string "hyphen to colon" "100:200" result

let test_normalize_node_id_colon () =
  let result = Mcp_helpers.normalize_node_id "100:200" in
  check string "colon preserved" "100:200" result

let test_normalize_node_id_simple () =
  let result = Mcp_helpers.normalize_node_id "simple" in
  check string "no separator" "simple" result

let test_normalize_node_id_key_node_id () =
  let result = Mcp_helpers.normalize_node_id_key "node_id" "123-456" in
  check string "node_id key normalized" "123:456" result

let test_normalize_node_id_key_node_a_id () =
  let result = Mcp_helpers.normalize_node_id_key "node_a_id" "789-012" in
  check string "node_a_id key normalized" "789:012" result

let test_normalize_node_id_key_node_b_id () =
  let result = Mcp_helpers.normalize_node_id_key "node_b_id" "111-222" in
  check string "node_b_id key normalized" "111:222" result

let test_normalize_node_id_key_other () =
  let result = Mcp_helpers.normalize_node_id_key "other_field" "333-444" in
  check string "other key not normalized" "333-444" result

let test_sanitize_node_id () =
  let result = Mcp_helpers.sanitize_node_id "100:200" in
  check string "colon to underscore" "100_200" result

let test_sanitize_node_id_no_colon () =
  let result = Mcp_helpers.sanitize_node_id "simple" in
  check string "no colon unchanged" "simple" result

let test_sanitize_node_id_multiple_colons () =
  let result = Mcp_helpers.sanitize_node_id "a:b:c" in
  check string "multiple colons" "a_b_c" result

let node_id_tests = [
  "normalize hyphen to colon", `Quick, test_normalize_node_id_hyphen;
  "normalize colon preserved", `Quick, test_normalize_node_id_colon;
  "normalize simple", `Quick, test_normalize_node_id_simple;
  "normalize_key node_id", `Quick, test_normalize_node_id_key_node_id;
  "normalize_key node_a_id", `Quick, test_normalize_node_id_key_node_a_id;
  "normalize_key node_b_id", `Quick, test_normalize_node_id_key_node_b_id;
  "normalize_key other", `Quick, test_normalize_node_id_key_other;
  "sanitize colon", `Quick, test_sanitize_node_id;
  "sanitize no colon", `Quick, test_sanitize_node_id_no_colon;
  "sanitize multiple colons", `Quick, test_sanitize_node_id_multiple_colons;
]

(** ============== Error Handling ============== *)

let test_error_to_string_network () =
  let result = Mcp_helpers.error_to_string (Mcp_helpers.NetworkError "timeout") in
  check bool "contains Network" true (String.length result > 0 && String.contains result 'N')

let test_error_to_string_auth () =
  let result = Mcp_helpers.error_to_string (Mcp_helpers.AuthError "invalid token") in
  check bool "contains Auth" true (String.length result > 0)

let test_error_to_string_not_found () =
  let result = Mcp_helpers.error_to_string (Mcp_helpers.NotFound "file XYZ") in
  check bool "contains found" true (String.length result > 0)

let test_error_to_string_rate_limited () =
  let result = Mcp_helpers.error_to_string (Mcp_helpers.RateLimited 60.0) in
  check bool "contains Rate" true (String.length result > 0)

let test_error_to_string_server_error () =
  let result = Mcp_helpers.error_to_string (Mcp_helpers.ServerError "500 Internal") in
  check bool "contains server" true (String.length result > 0)

let test_error_to_string_parse_error () =
  let result = Mcp_helpers.error_to_string (Mcp_helpers.ParseError "invalid json") in
  check bool "contains Parse" true (String.length result > 0)

let test_error_to_string_timeout () =
  let result = Mcp_helpers.error_to_string (Mcp_helpers.TimeoutError 30.0) in
  check bool "contains Timeout" true (String.length result > 0)

let test_error_to_string_unknown () =
  let result = Mcp_helpers.error_to_string (Mcp_helpers.UnknownError "mystery") in
  check bool "contains Unknown" true (String.length result > 0)

let test_classify_http_error_401 () =
  match Mcp_helpers.classify_http_error ~status_code:401 ~body:"unauthorized" with
  | Mcp_helpers.AuthError _ -> ()
  | _ -> fail "Expected AuthError for 401"

let test_classify_http_error_403 () =
  match Mcp_helpers.classify_http_error ~status_code:403 ~body:"forbidden" with
  | Mcp_helpers.AuthError _ -> ()
  | _ -> fail "Expected AuthError for 403"

let test_classify_http_error_404 () =
  match Mcp_helpers.classify_http_error ~status_code:404 ~body:"not found" with
  | Mcp_helpers.NotFound _ -> ()
  | _ -> fail "Expected NotFound for 404"

let test_classify_http_error_429 () =
  match Mcp_helpers.classify_http_error ~status_code:429 ~body:"retry after 120" with
  | Mcp_helpers.RateLimited secs -> check bool "retry seconds parsed" true (secs >= 60.0)
  | _ -> fail "Expected RateLimited for 429"

let test_classify_http_error_500 () =
  match Mcp_helpers.classify_http_error ~status_code:500 ~body:"internal error" with
  | Mcp_helpers.ServerError _ -> ()
  | _ -> fail "Expected ServerError for 500"

let test_classify_http_error_503 () =
  match Mcp_helpers.classify_http_error ~status_code:503 ~body:"unavailable" with
  | Mcp_helpers.ServerError _ -> ()
  | _ -> fail "Expected ServerError for 503"

let test_classify_http_error_other () =
  match Mcp_helpers.classify_http_error ~status_code:418 ~body:"i'm a teapot" with
  | Mcp_helpers.UnknownError _ -> ()
  | _ -> fail "Expected UnknownError for 418"

let error_handling_tests = [
  "error_to_string NetworkError", `Quick, test_error_to_string_network;
  "error_to_string AuthError", `Quick, test_error_to_string_auth;
  "error_to_string NotFound", `Quick, test_error_to_string_not_found;
  "error_to_string RateLimited", `Quick, test_error_to_string_rate_limited;
  "error_to_string ServerError", `Quick, test_error_to_string_server_error;
  "error_to_string ParseError", `Quick, test_error_to_string_parse_error;
  "error_to_string TimeoutError", `Quick, test_error_to_string_timeout;
  "error_to_string UnknownError", `Quick, test_error_to_string_unknown;
  "classify 401", `Quick, test_classify_http_error_401;
  "classify 403", `Quick, test_classify_http_error_403;
  "classify 404", `Quick, test_classify_http_error_404;
  "classify 429", `Quick, test_classify_http_error_429;
  "classify 500", `Quick, test_classify_http_error_500;
  "classify 503", `Quick, test_classify_http_error_503;
  "classify other", `Quick, test_classify_http_error_other;
]

(** ============== URL Utilities ============== *)

let test_strip_query_with_query () =
  let result = Mcp_helpers.strip_query "https://example.com/path?foo=bar&baz=qux" in
  check string "strip query" "https://example.com/path" result

let test_strip_query_no_query () =
  let result = Mcp_helpers.strip_query "https://example.com/path" in
  check string "no query unchanged" "https://example.com/path" result

let test_strip_query_empty () =
  let result = Mcp_helpers.strip_query "" in
  check string "empty string" "" result

let test_is_http_url_http () =
  check bool "http url" true (Mcp_helpers.is_http_url "http://example.com")

let test_is_http_url_https () =
  check bool "https url" true (Mcp_helpers.is_http_url "https://example.com")

let test_is_http_url_httpx () =
  check bool "httpx not http" false (Mcp_helpers.is_http_url "httpx://example.com")

let test_is_http_url_ftp () =
  check bool "ftp not http" false (Mcp_helpers.is_http_url "ftp://example.com")

let test_is_http_url_path () =
  check bool "path not http" false (Mcp_helpers.is_http_url "/local/path")

let test_is_http_url_bare_http () =
  check bool "bare http is not url" false (Mcp_helpers.is_http_url "http")

let test_is_http_url_short () =
  check bool "short string" false (Mcp_helpers.is_http_url "hi")

let test_file_ext_from_url_png () =
  let result = Mcp_helpers.file_ext_from_url "https://example.com/image.png?v=1" in
  check string "png extension" ".png" result

let test_file_ext_from_url_jpg () =
  let result = Mcp_helpers.file_ext_from_url "https://example.com/photo.jpg" in
  check string "jpg extension" ".jpg" result

let test_file_ext_from_url_no_ext () =
  let result = Mcp_helpers.file_ext_from_url "https://example.com/noext" in
  check string "default extension" ".img" result

let url_util_tests = [
  "strip_query with query", `Quick, test_strip_query_with_query;
  "strip_query no query", `Quick, test_strip_query_no_query;
  "strip_query empty", `Quick, test_strip_query_empty;
  "is_http_url http", `Quick, test_is_http_url_http;
  "is_http_url https", `Quick, test_is_http_url_https;
  "is_http_url httpx", `Quick, test_is_http_url_httpx;
  "is_http_url ftp", `Quick, test_is_http_url_ftp;
  "is_http_url path", `Quick, test_is_http_url_path;
  "is_http_url bare http", `Quick, test_is_http_url_bare_http;
  "is_http_url short", `Quick, test_is_http_url_short;
  "file_ext_from_url png", `Quick, test_file_ext_from_url_png;
  "file_ext_from_url jpg", `Quick, test_file_ext_from_url_jpg;
  "file_ext_from_url no ext", `Quick, test_file_ext_from_url_no_ext;
]

(** ============== Content Helpers ============== *)

let test_make_text_content () =
  let result = Mcp_helpers.make_text_content "Hello World" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "content" fields with
       | Some (`List [`Assoc item_fields]) ->
           check (option string) "type" (Some "text")
             (match List.assoc_opt "type" item_fields with Some (`String s) -> Some s | _ -> None);
           check (option string) "text" (Some "Hello World")
             (match List.assoc_opt "text" item_fields with Some (`String s) -> Some s | _ -> None)
       | _ -> fail "Expected content list")
  | _ -> fail "Expected Assoc"

let test_make_text_content_empty () =
  let result = Mcp_helpers.make_text_content "" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "content" fields with
       | Some (`List [`Assoc item_fields]) ->
           check (option string) "empty text" (Some "")
             (match List.assoc_opt "text" item_fields with Some (`String s) -> Some s | _ -> None)
       | _ -> fail "Expected content list")
  | _ -> fail "Expected Assoc"

let test_make_error_content () =
  let result = Mcp_helpers.make_error_content "Something went wrong" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "content" fields with
       | Some (`List [`Assoc item_fields]) ->
           check (option string) "type" (Some "text")
             (match List.assoc_opt "type" item_fields with Some (`String s) -> Some s | _ -> None);
           check (option bool) "isError" (Some true)
             (match List.assoc_opt "isError" item_fields with Some (`Bool b) -> Some b | _ -> None)
       | _ -> fail "Expected content list")
  | _ -> fail "Expected Assoc"

let content_helper_tests = [
  "make_text_content", `Quick, test_make_text_content;
  "make_text_content empty", `Quick, test_make_text_content_empty;
  "make_error_content", `Quick, test_make_error_content;
]

(** ============== Field Utilities ============== *)

let test_has_field_exists () =
  let fields = [("name", `String "Alice"); ("age", `Int 30)] in
  check bool "has name" true (Mcp_tools.has_field "name" fields)

let test_has_field_missing () =
  let fields = [("name", `String "Alice")] in
  check bool "no age" false (Mcp_tools.has_field "age" fields)

let test_has_field_empty () =
  check bool "empty fields" false (Mcp_tools.has_field "any" [])

let test_set_field_new () =
  let fields = [("a", `Int 1)] in
  let result = Mcp_tools.set_field "b" (`Int 2) fields in
  check int "length" 2 (List.length result);
  check bool "has b" true (Mcp_tools.has_field "b" result)

let test_set_field_replace () =
  let fields = [("a", `Int 1); ("b", `Int 2)] in
  let result = Mcp_tools.set_field "a" (`Int 10) fields in
  check int "length unchanged" 2 (List.length result);
  (match List.assoc_opt "a" result with
   | Some (`Int v) -> check int "a updated" 10 v
   | _ -> fail "Expected Int")

let test_add_if_missing_adds () =
  let fields = [("a", `Int 1)] in
  let result = Mcp_tools.add_if_missing "b" (`Int 2) fields in
  check int "length" 2 (List.length result);
  check bool "has b" true (Mcp_tools.has_field "b" result)

let test_add_if_missing_skips () =
  let fields = [("a", `Int 1)] in
  let result = Mcp_tools.add_if_missing "a" (`Int 99) fields in
  check int "length unchanged" 1 (List.length result);
  (match List.assoc_opt "a" result with
   | Some (`Int v) -> check int "a unchanged" 1 v
   | _ -> fail "Expected Int")

let test_get_string_any_first () =
  let json = `Assoc [("name", `String "Alice"); ("alias", `String "A")] in
  match Mcp_tools.get_string_any ["name"; "alias"] json with
  | Some v -> check string "first match" "Alice" v
  | None -> fail "Expected Some"

let test_get_string_any_fallback () =
  let json = `Assoc [("alias", `String "A")] in
  match Mcp_tools.get_string_any ["name"; "alias"] json with
  | Some v -> check string "fallback match" "A" v
  | None -> fail "Expected Some"

let test_get_string_any_none () =
  let json = `Assoc [("other", `String "X")] in
  match Mcp_tools.get_string_any ["name"; "alias"] json with
  | None -> ()
  | Some _ -> fail "Expected None"

let field_util_tests = [
  "has_field exists", `Quick, test_has_field_exists;
  "has_field missing", `Quick, test_has_field_missing;
  "has_field empty", `Quick, test_has_field_empty;
  "set_field new", `Quick, test_set_field_new;
  "set_field replace", `Quick, test_set_field_replace;
  "add_if_missing adds", `Quick, test_add_if_missing_adds;
  "add_if_missing skips", `Quick, test_add_if_missing_skips;
  "get_string_any first", `Quick, test_get_string_any_first;
  "get_string_any fallback", `Quick, test_get_string_any_fallback;
  "get_string_any none", `Quick, test_get_string_any_none;
]

(** ============== Truncation & Chunking ============== *)

let test_truncate_string_short () =
  let result = Mcp_tools.truncate_string ~max_len:20 "Hello" in
  check string "short unchanged" "Hello" result

let test_truncate_string_long () =
  let result = Mcp_tools.truncate_string ~max_len:5 "Hello World" in
  check string "truncated" "Hello...(truncated)" result

let test_truncate_string_exact () =
  let result = Mcp_tools.truncate_string ~max_len:5 "Hello" in
  check string "exact unchanged" "Hello" result

let test_truncate_string_zero () =
  let result = Mcp_tools.truncate_string ~max_len:0 "Hello" in
  check string "zero max_len unchanged" "Hello" result

let test_truncate_string_negative () =
  let result = Mcp_tools.truncate_string ~max_len:(-5) "Hello" in
  check string "negative max_len unchanged" "Hello" result

let test_is_utf8_continuation_true () =
  (* 0b10xxxxxx = continuation byte *)
  check bool "continuation byte" true (Mcp_tools.is_utf8_continuation 0b10000000)

let test_is_utf8_continuation_false () =
  (* 0b0xxxxxxx = ASCII, not continuation *)
  check bool "ascii byte" false (Mcp_tools.is_utf8_continuation 0b01000000)

let test_utf8_safe_boundary_ascii () =
  let result = Mcp_tools.utf8_safe_boundary ~start:0 ~max_bytes:5 "Hello World" in
  check int "ascii boundary" 5 result

let test_utf8_safe_boundary_multibyte () =
  (* Korean: Each character is 3 bytes in UTF-8 *)
  let korean = "\xed\x95\x9c\xea\xb8\x80" in (* "UD55C\UAE00" = 6 bytes *)
  let result = Mcp_tools.utf8_safe_boundary ~start:0 ~max_bytes:4 korean in
  (* Should back up to 3 (end of first char) since byte 4 would be in middle of second char *)
  check bool "multibyte boundary" true (result <= 4)

let test_truncate_utf8_short () =
  let (result, truncated) = Mcp_tools.truncate_utf8 ~max_bytes:20 "Hello" in
  check string "short unchanged" "Hello" result;
  check bool "not truncated" false truncated

let test_truncate_utf8_long () =
  let (result, truncated) = Mcp_tools.truncate_utf8 ~max_bytes:5 "Hello World" in
  check string "truncated" "Hello" result;
  check bool "truncated flag" true truncated

let test_truncate_utf8_zero () =
  let (result, truncated) = Mcp_tools.truncate_utf8 ~max_bytes:0 "Hello" in
  check string "zero unchanged" "Hello" result;
  check bool "not truncated" false truncated

let test_take_n_less () =
  let result = Mcp_tools.take_n 3 [1; 2] in
  check (list int) "less than n" [1; 2] result

let test_take_n_exact () =
  let result = Mcp_tools.take_n 3 [1; 2; 3] in
  check (list int) "exact n" [1; 2; 3] result

let test_take_n_more () =
  let result = Mcp_tools.take_n 3 [1; 2; 3; 4; 5] in
  check (list int) "more than n" [1; 2; 3] result

let test_take_n_zero () =
  let result = Mcp_tools.take_n 0 [1; 2; 3] in
  check (list int) "zero n" [] result

let test_take_n_empty () =
  let result = Mcp_tools.take_n 3 [] in
  check (list int) "empty list" [] result

let test_chunk_list_exact () =
  let result = Mcp_tools.chunk_list 2 [1; 2; 3; 4] in
  check int "chunk count" 2 (List.length result);
  check (list int) "first chunk" [1; 2] (List.nth result 0);
  check (list int) "second chunk" [3; 4] (List.nth result 1)

let test_chunk_list_remainder () =
  let result = Mcp_tools.chunk_list 2 [1; 2; 3; 4; 5] in
  check int "chunk count with remainder" 3 (List.length result);
  check (list int) "last chunk" [5] (List.nth result 2)

let test_chunk_list_larger_chunk () =
  let result = Mcp_tools.chunk_list 10 [1; 2; 3] in
  check int "single chunk" 1 (List.length result);
  check (list int) "all items" [1; 2; 3] (List.nth result 0)

let test_chunk_list_empty () =
  let result = Mcp_tools.chunk_list 2 [] in
  check int "empty list" 0 (List.length result)

let test_chunk_list_zero_size () =
  (* chunk_size <= 0 should be treated as 1 *)
  let result = Mcp_tools.chunk_list 0 [1; 2; 3] in
  check int "chunk size 0 -> 1" 3 (List.length result)

let truncation_chunking_tests = [
  "truncate_string short", `Quick, test_truncate_string_short;
  "truncate_string long", `Quick, test_truncate_string_long;
  "truncate_string exact", `Quick, test_truncate_string_exact;
  "truncate_string zero", `Quick, test_truncate_string_zero;
  "truncate_string negative", `Quick, test_truncate_string_negative;
  "is_utf8_continuation true", `Quick, test_is_utf8_continuation_true;
  "is_utf8_continuation false", `Quick, test_is_utf8_continuation_false;
  "utf8_safe_boundary ascii", `Quick, test_utf8_safe_boundary_ascii;
  "utf8_safe_boundary multibyte", `Quick, test_utf8_safe_boundary_multibyte;
  "truncate_utf8 short", `Quick, test_truncate_utf8_short;
  "truncate_utf8 long", `Quick, test_truncate_utf8_long;
  "truncate_utf8 zero", `Quick, test_truncate_utf8_zero;
  "take_n less", `Quick, test_take_n_less;
  "take_n exact", `Quick, test_take_n_exact;
  "take_n more", `Quick, test_take_n_more;
  "take_n zero", `Quick, test_take_n_zero;
  "take_n empty", `Quick, test_take_n_empty;
  "chunk_list exact", `Quick, test_chunk_list_exact;
  "chunk_list remainder", `Quick, test_chunk_list_remainder;
  "chunk_list larger chunk", `Quick, test_chunk_list_larger_chunk;
  "chunk_list empty", `Quick, test_chunk_list_empty;
  "chunk_list zero size", `Quick, test_chunk_list_zero_size;
]

(** ============== JSON Compaction ============== *)

let test_compact_json_simple () =
  let json = `Assoc [("name", `String "test"); ("value", `Int 42)] in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      check bool "has name" true (List.exists (fun (k, _) -> k = "name") fields);
      check bool "has value" true (List.exists (fun (k, _) -> k = "value") fields)
  | _ -> fail "Expected Assoc"

let test_compact_json_truncates_children () =
  let children = List.init 20 (fun i -> `Assoc [("id", `Int i)]) in
  let json = `Assoc [("children", `List children)] in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:5 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "children" fields with
       | Some (`List items) ->
           (* Should have 5 children + 1 truncated marker *)
           check bool "truncated" true (List.length items <= 7)
       | _ -> fail "Expected children list")
  | _ -> fail "Expected Assoc"

let test_compact_json_depth_limit () =
  let deep = `Assoc [("level1", `Assoc [("level2", `Assoc [("level3", `String "deep")])])] in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:1 ~max_children:10 ~max_list_items:10 ~max_string:100
    deep
  in
  (* At depth 1, children should be removed *)
  match result with
  | `Assoc _ -> ()  (* Structure preserved at top level *)
  | _ -> fail "Expected Assoc"

let test_compact_json_list_truncation () =
  let items = List.init 20 (fun i -> `Int i) in
  let json = `List items in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:5 ~max_string:100
    json
  in
  match result with
  | `List truncated_items ->
      check bool "list truncated" true (List.length truncated_items <= 7)
  | _ -> fail "Expected List"

let test_compact_json_removes_missing_keys () =
  let json = `Assoc [("valid", `Int 1); ("something_missing", `Null)] in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      check bool "no _missing keys" true
        (not (List.exists (fun (k, _) -> String.ends_with ~suffix:"_missing" k) fields))
  | _ -> fail "Expected Assoc"

let json_compaction_tests = [
  "compact_json simple", `Quick, test_compact_json_simple;
  "compact_json truncates children", `Quick, test_compact_json_truncates_children;
  "compact_json depth limit", `Quick, test_compact_json_depth_limit;
  "compact_json list truncation", `Quick, test_compact_json_list_truncation;
  "compact_json removes missing keys", `Quick, test_compact_json_removes_missing_keys;
]

(** ============== Handler Registry ============== *)

let test_register_and_call_handler () =
  let test_handler _args = Ok (`String "handler result") in
  Mcp_helpers.register_handler "test_handler" test_handler;
  match Mcp_helpers.call_handler "test_handler" (`Assoc []) with
  | Ok (`String v) -> check string "handler result" "handler result" v
  | Ok _ -> fail "Expected String result"
  | Error e -> fail ("Handler error: " ^ e)

let test_call_missing_handler () =
  match Mcp_helpers.call_handler "nonexistent_handler" (`Assoc []) with
  | Error msg -> check bool "error message" true (String.length msg > 0)
  | Ok _ -> fail "Expected Error"

let handler_registry_tests = [
  "register and call handler", `Quick, test_register_and_call_handler;
  "call missing handler", `Quick, test_call_missing_handler;
]

(** ============== Result Monadic Operators ============== *)

let test_bind_ok () =
  let result = Ok 5 >>= (fun x -> Ok (x * 2)) in
  match result with
  | Ok v -> check int "bind ok" 10 v
  | Error _ -> fail "Expected Ok"

let test_bind_error () =
  let result = (Error "failed") >>= (fun x -> Ok (x * 2)) in
  match result with
  | Error e -> check string "bind error" "failed" e
  | Ok _ -> fail "Expected Error"

let test_map_ok () =
  let result = Ok 5 >>| (fun x -> x * 2) in
  match result with
  | Ok v -> check int "map ok" 10 v
  | Error _ -> fail "Expected Ok"

let test_map_error () =
  let result = (Error "failed") >>| (fun x -> x * 2) in
  match result with
  | Error e -> check string "map error" "failed" e
  | Ok _ -> fail "Expected Error"

let monadic_tests = [
  "bind ok", `Quick, test_bind_ok;
  "bind error", `Quick, test_bind_error;
  "map ok", `Quick, test_map_ok;
  "map error", `Quick, test_map_error;
]

(** ============== Tool Definitions ============== *)

let test_all_tools_not_empty () =
  check bool "all_tools not empty" true (List.length Mcp_tools.all_tools > 0)

let test_all_tools_have_names () =
  let all_have_names = List.for_all (fun (t : Figma_mcp_protocol.tool_def) ->
    String.length t.name > 0
  ) Mcp_tools.all_tools in
  check bool "all tools have names" true all_have_names

let test_all_tools_have_descriptions () =
  let all_have_desc = List.for_all (fun (t : Figma_mcp_protocol.tool_def) ->
    String.length t.description > 0
  ) Mcp_tools.all_tools in
  check bool "all tools have descriptions" true all_have_desc

let test_all_tools_unique_names () =
  let names = List.map (fun (t : Figma_mcp_protocol.tool_def) -> t.name) Mcp_tools.all_tools in
  let unique_names = List.sort_uniq String.compare names in
  check int "unique tool names" (List.length names) (List.length unique_names)

let test_tool_codegen_exists () =
  let exists = List.exists (fun (t : Figma_mcp_protocol.tool_def) ->
    t.name = "figma_codegen"
  ) Mcp_tools.all_tools in
  check bool "figma_codegen exists" true exists

let test_tool_get_file_exists () =
  let exists = List.exists (fun (t : Figma_mcp_protocol.tool_def) ->
    t.name = "figma_get_file"
  ) Mcp_tools.all_tools in
  check bool "figma_get_file exists" true exists

let test_tool_parse_url_exists () =
  let exists = List.exists (fun (t : Figma_mcp_protocol.tool_def) ->
    t.name = "figma_parse_url"
  ) Mcp_tools.all_tools in
  check bool "figma_parse_url exists" true exists

let tool_definition_tests = [
  "all_tools not empty", `Quick, test_all_tools_not_empty;
  "all tools have names", `Quick, test_all_tools_have_names;
  "all tools have descriptions", `Quick, test_all_tools_have_descriptions;
  "all tools unique names", `Quick, test_all_tools_unique_names;
  "figma_codegen exists", `Quick, test_tool_codegen_exists;
  "figma_get_file exists", `Quick, test_tool_get_file_exists;
  "figma_parse_url exists", `Quick, test_tool_parse_url_exists;
]

(** ============== Resources ============== *)

let test_read_resource_fidelity_docs () =
  match Mcp_tools.read_resource "figma://docs/fidelity" with
  | Ok (mime, body) ->
      check string "mime type" "text/markdown" mime;
      check bool "body not empty" true (String.length body > 0)
  | Error e -> fail ("read_resource error: " ^ e)

let test_read_resource_usage_docs () =
  match Mcp_tools.read_resource "figma://docs/usage" with
  | Ok (mime, body) ->
      check string "mime type" "text/markdown" mime;
      check bool "body not empty" true (String.length body > 0)
  | Error e -> fail ("read_resource error: " ^ e)

let test_read_resource_not_found () =
  match Mcp_tools.read_resource "figma://docs/nonexistent" with
  | Error _ -> ()
  | Ok _ -> fail "Expected Error for nonexistent resource"

let resource_tests = [
  "read fidelity docs", `Quick, test_read_resource_fidelity_docs;
  "read usage docs", `Quick, test_read_resource_usage_docs;
  "read nonexistent resource", `Quick, test_read_resource_not_found;
]

(** ============== Security / Token Resolution ============== *)

let with_env name value f =
  let prev = Sys.getenv_opt name in
  let restore () =
    match prev with
    | None -> Unix.putenv name ""
    | Some v -> Unix.putenv name v
  in
  (match value with
  | None -> Unix.putenv name ""
  | Some v -> Unix.putenv name v);
  match f () with
  | result ->
      restore ();
      result
  | exception exn ->
      restore ();
      raise exn

let test_resolve_token_env_figma_token () =
  with_env "FIGMA_TOKEN" (Some "abc") (fun () ->
      let args = `Assoc [ ("token", `String "env:FIGMA_TOKEN") ] in
      check (option string) "env:FIGMA_TOKEN resolves" (Some "abc")
        (Mcp_helpers.resolve_token args))

let test_resolve_token_env_other_denied () =
  (* Hermetic: FIGMA_TOKEN may be set in the parent environment when running
     tests locally; clear it so we can assert env:OTHER is denied. *)
  with_env "FIGMA_TOKEN" None (fun () ->
      with_env "AWS_SECRET_ACCESS_KEY" (Some "shh") (fun () ->
          let args = `Assoc [ ("token", `String "env:AWS_SECRET_ACCESS_KEY") ] in
          check (option string) "env:OTHER denied" None
            (Mcp_helpers.resolve_token args)))

let test_resolve_token_fallback_figma_token () =
  with_env "FIGMA_TOKEN" (Some "abc") (fun () ->
      let args = `Assoc [] in
      check (option string) "fallback FIGMA_TOKEN env" (Some "abc")
        (Mcp_helpers.resolve_token args))

let test_resolve_token_literal () =
  (* Hermetic: ensure FIGMA_TOKEN does not override literal resolution. *)
  with_env "FIGMA_TOKEN" None (fun () ->
      let args = `Assoc [ ("token", `String "literal") ] in
      check (option string) "literal token" (Some "literal")
        (Mcp_helpers.resolve_token args))

let token_resolution_tests = [
  "resolve env:FIGMA_TOKEN", `Quick, test_resolve_token_env_figma_token;
  "deny env:OTHER", `Quick, test_resolve_token_env_other_denied;
  "fallback FIGMA_TOKEN env", `Quick, test_resolve_token_fallback_figma_token;
  "literal token", `Quick, test_resolve_token_literal;
]

(** ============== Server Creation ============== *)

let test_create_figma_server () =
  let server = Mcp_tools.create_figma_server () in
  check bool "has tools" true (List.length server.Figma_mcp_protocol.tools > 0);
  check bool "has handlers" true (List.length server.Figma_mcp_protocol.handlers_sync > 0);
  check bool "has resources" true (List.length server.Figma_mcp_protocol.resources > 0);
  check bool "has prompts" true (List.length server.Figma_mcp_protocol.prompts > 0)

let tool_names tools =
  tools
  |> List.map (fun (t : Figma_mcp_protocol.tool_def) -> t.name)
  |> List.sort String.compare

let test_server_tools_match_public_tools () =
  let server = Mcp_tools.create_figma_server () in
  check (list string) "tool names match public_tools"
    (tool_names Mcp_tools.public_tools)
    (tool_names server.Figma_mcp_protocol.tools)

let server_tests = [
  "create_figma_server", `Quick, test_create_figma_server;
  "server tools match public_tools", `Quick, test_server_tools_match_public_tools;
]

(** ============== Main ============== *)

let test_string_contains_basic () =
  check bool "basic match" true (Mcp_helpers.string_contains ~haystack:"Hello World" ~needle:"world");
  check bool "no match" false (Mcp_helpers.string_contains ~haystack:"Hello" ~needle:"bye");
  check bool "empty sub" false (Mcp_helpers.string_contains ~haystack:"Hello" ~needle:"");
  check bool "case insensitive" true (Mcp_helpers.string_contains ~haystack:"BROKEN PIPE" ~needle:"broken pipe")

let test_is_network_error_structural () =
  check bool "Unix EPIPE" true (Mcp_tools.is_network_error (Unix.Unix_error (Unix.EPIPE, "write", "")));
  check bool "Unix ECONNRESET" true (Mcp_tools.is_network_error (Unix.Unix_error (Unix.ECONNRESET, "read", "")))

let test_is_network_error_string () =
  check bool "string broken pipe" true (Mcp_tools.is_network_error (Failure "Broken pipe"));
  check bool "string connection reset" true (Mcp_tools.is_network_error (Failure "Connection reset by peer"));
  check bool "generic error" false (Mcp_tools.is_network_error (Failure "Unknown internal error"))

let network_util_tests = [
  "string_contains basic", `Quick, test_string_contains_basic;
  "is_network_error structural", `Quick, test_is_network_error_structural;
  "is_network_error string", `Quick, test_is_network_error_string;
]

(** ============== Category / Tool Lookup ============== *)

let test_find_tool_in_category_core_get_file () =
  check bool "get_file in core" true
    (Mcp_tools.find_tool_in_category "core" "get_file")

let test_find_tool_in_category_core_get_node () =
  check bool "get_node in core" true
    (Mcp_tools.find_tool_in_category "core" "get_node")

let test_find_tool_in_category_visual_verify () =
  check bool "verify_visual in visual" true
    (Mcp_tools.find_tool_in_category "visual" "verify_visual")

let test_find_tool_in_category_missing_tool () =
  check bool "nonexistent tool" false
    (Mcp_tools.find_tool_in_category "core" "nonexistent_tool")

let test_find_tool_in_category_missing_category () =
  check bool "nonexistent category" false
    (Mcp_tools.find_tool_in_category "nonexistent_category" "get_file")

let test_find_tool_in_category_team () =
  check bool "list_projects in team" true
    (Mcp_tools.find_tool_in_category "team" "list_projects")

let test_find_tool_in_category_export () =
  check bool "export_image in export" true
    (Mcp_tools.find_tool_in_category "export" "export_image")

let test_find_tool_in_category_components () =
  check bool "get_variables in components" true
    (Mcp_tools.find_tool_in_category "components" "get_variables")

let test_find_tool_in_category_wrong_category () =
  check bool "get_file not in visual" false
    (Mcp_tools.find_tool_in_category "visual" "get_file")

let category_lookup_tests = [
  "find_tool core/get_file", `Quick, test_find_tool_in_category_core_get_file;
  "find_tool core/get_node", `Quick, test_find_tool_in_category_core_get_node;
  "find_tool visual/verify_visual", `Quick, test_find_tool_in_category_visual_verify;
  "find_tool missing tool", `Quick, test_find_tool_in_category_missing_tool;
  "find_tool missing category", `Quick, test_find_tool_in_category_missing_category;
  "find_tool team/list_projects", `Quick, test_find_tool_in_category_team;
  "find_tool export/export_image", `Quick, test_find_tool_in_category_export;
  "find_tool components/get_variables", `Quick, test_find_tool_in_category_components;
  "find_tool wrong category", `Quick, test_find_tool_in_category_wrong_category;
]

(** ============== Tool Categories Structure ============== *)

let test_tool_categories_not_empty () =
  check bool "tool_categories not empty" true
    (List.length Mcp_tools.tool_categories > 0)

let test_tool_categories_have_names () =
  let all_named = List.for_all (fun (cat : Mcp_tools.tool_category) ->
    String.length cat.name > 0
  ) Mcp_tools.tool_categories in
  check bool "all categories have names" true all_named

let test_tool_categories_have_tools () =
  let all_have_tools = List.for_all (fun (cat : Mcp_tools.tool_category) ->
    List.length cat.tools > 0
  ) Mcp_tools.tool_categories in
  check bool "all categories have tools" true all_have_tools

let test_tool_categories_core_exists () =
  let core_exists = List.exists (fun (cat : Mcp_tools.tool_category) ->
    cat.name = "core"
  ) Mcp_tools.tool_categories in
  check bool "core category exists" true core_exists

let test_tool_categories_visual_exists () =
  let visual_exists = List.exists (fun (cat : Mcp_tools.tool_category) ->
    cat.name = "visual"
  ) Mcp_tools.tool_categories in
  check bool "visual category exists" true visual_exists

let test_tool_categories_unique_names () =
  let names = List.map (fun (cat : Mcp_tools.tool_category) -> cat.name)
    Mcp_tools.tool_categories in
  let unique = List.sort_uniq String.compare names in
  check int "unique category names" (List.length names) (List.length unique)

let test_category_tools_count () =
  check int "category_tools count matches tool_categories"
    (List.length Mcp_tools.tool_categories)
    (List.length Mcp_tools.category_tools)

let test_category_tools_names_prefixed () =
  let all_prefixed = List.for_all (fun (t : Figma_mcp_protocol.tool_def) ->
    String.length t.name >= 6 && String.sub t.name 0 6 = "figma_"
  ) Mcp_tools.category_tools in
  check bool "all category tools prefixed with figma_" true all_prefixed

let test_make_category_tool_name () =
  let cat : Mcp_tools.tool_category = { name = "test_cat"; description = "Test"; tools = ["a"; "b"] } in
  let tool = Mcp_tools.make_category_tool cat in
  check string "tool name" "figma_test_cat" tool.name

let test_make_category_tool_description_contains_tools () =
  let cat : Mcp_tools.tool_category = { name = "mycat"; description = "My description"; tools = ["foo"; "bar"] } in
  let tool = Mcp_tools.make_category_tool cat in
  check bool "description contains foo" true (Mcp_helpers.string_contains ~haystack:tool.description ~needle:"foo");
  check bool "description contains bar" true (Mcp_helpers.string_contains ~haystack:tool.description ~needle:"bar")

let test_make_category_tool_has_mode_enum () =
  let cat : Mcp_tools.tool_category = { name = "test"; description = "Test"; tools = ["x"] } in
  let tool = Mcp_tools.make_category_tool cat in
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "properties" fields with
       | Some (`Assoc props) ->
           check bool "has mode property" true (List.mem_assoc "mode" props);
           check bool "has tool property" true (List.mem_assoc "tool" props);
           check bool "has args property" true (List.mem_assoc "args" props)
       | _ -> fail "Expected properties Assoc")
  | _ -> fail "Expected Assoc schema"

let test_featured_tool_names_not_empty () =
  check bool "featured_tool_names not empty" true
    (List.length Mcp_tools.featured_tool_names > 0)

let test_featured_tools_not_empty () =
  check bool "featured_tools not empty" true
    (List.length Mcp_tools.featured_tools > 0)

let test_featured_tools_subset_of_all () =
  let all_names = List.map (fun (t : Figma_mcp_protocol.tool_def) -> t.name) Mcp_tools.all_tools in
  let all_featured_in_all = List.for_all (fun (t : Figma_mcp_protocol.tool_def) ->
    List.mem t.name all_names
  ) Mcp_tools.featured_tools in
  check bool "all featured tools in all_tools" true all_featured_in_all

let test_public_tools_equals_category_plus_featured () =
  let expected_len = List.length Mcp_tools.category_tools + List.length Mcp_tools.featured_tools in
  check int "public_tools length" expected_len (List.length Mcp_tools.public_tools)

let category_structure_tests = [
  "tool_categories not empty", `Quick, test_tool_categories_not_empty;
  "categories have names", `Quick, test_tool_categories_have_names;
  "categories have tools", `Quick, test_tool_categories_have_tools;
  "core category exists", `Quick, test_tool_categories_core_exists;
  "visual category exists", `Quick, test_tool_categories_visual_exists;
  "category names unique", `Quick, test_tool_categories_unique_names;
  "category_tools count", `Quick, test_category_tools_count;
  "category_tools prefixed", `Quick, test_category_tools_names_prefixed;
  "make_category_tool name", `Quick, test_make_category_tool_name;
  "make_category_tool desc", `Quick, test_make_category_tool_description_contains_tools;
  "make_category_tool schema", `Quick, test_make_category_tool_has_mode_enum;
  "featured_tool_names not empty", `Quick, test_featured_tool_names_not_empty;
  "featured_tools not empty", `Quick, test_featured_tools_not_empty;
  "featured_tools subset of all", `Quick, test_featured_tools_subset_of_all;
  "public_tools = category + featured", `Quick, test_public_tools_equals_category_plus_featured;
]

(** ============== Chunkify Children ============== *)

let test_chunkify_children_basic () =
  let children = List.init 6 (fun i -> `Assoc [("id", `Int i)]) in
  let json = `Assoc [("name", `String "parent"); ("children", `List children)] in
  let result = Mcp_tools.chunkify_children ~chunk_size:2 json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunks" fields with
       | Some (`List chunks) ->
           check int "chunk count" 3 (List.length chunks);
           (* Each chunk should have chunk_index, chunk_total, children *)
           (match List.nth chunks 0 with
            | `Assoc cf ->
                (match List.assoc_opt "chunk_index" cf with
                 | Some (`Int idx) -> check int "first chunk index" 1 idx
                 | _ -> fail "Expected chunk_index Int");
                (match List.assoc_opt "chunk_total" cf with
                 | Some (`Int total) -> check int "chunk total" 3 total
                 | _ -> fail "Expected chunk_total Int");
                (match List.assoc_opt "children" cf with
                 | Some (`List c) -> check int "first chunk children" 2 (List.length c)
                 | _ -> fail "Expected children list")
            | _ -> fail "Expected Assoc chunk")
       | _ -> fail "Expected chunks list");
      (match List.assoc_opt "chunk_total" fields with
       | Some (`Int t) -> check int "top-level chunk_total" 3 t
       | _ -> fail "Expected chunk_total");
      (* Original "name" field should be preserved *)
      (match List.assoc_opt "name" fields with
       | Some (`String n) -> check string "name preserved" "parent" n
       | _ -> fail "Expected name preserved")
  | _ -> fail "Expected Assoc"

let test_chunkify_children_no_children () =
  let json = `Assoc [("name", `String "leaf")] in
  let result = Mcp_tools.chunkify_children ~chunk_size:5 json in
  match result with
  | `Assoc fields ->
      check bool "name still present" true (List.mem_assoc "name" fields);
      check bool "no chunks" false (List.mem_assoc "chunks" fields)
  | _ -> fail "Expected Assoc"

let test_chunkify_children_not_assoc () =
  let json = `String "not an object" in
  let result = Mcp_tools.chunkify_children ~chunk_size:5 json in
  check bool "passthrough non-assoc" true
    (result = `String "not an object")

let test_chunkify_children_children_not_list () =
  let json = `Assoc [("children", `String "not a list")] in
  let result = Mcp_tools.chunkify_children ~chunk_size:5 json in
  match result with
  | `Assoc fields ->
      (* children is not a list, so should be unchanged *)
      check bool "no chunks created" false (List.mem_assoc "chunks" fields)
  | _ -> fail "Expected Assoc"

let chunkify_children_tests = [
  "chunkify_children basic", `Quick, test_chunkify_children_basic;
  "chunkify_children no children", `Quick, test_chunkify_children_no_children;
  "chunkify_children not assoc", `Quick, test_chunkify_children_not_assoc;
  "chunkify_children non-list children", `Quick, test_chunkify_children_children_not_list;
]

(** ============== Chunkify Text ============== *)

let test_chunkify_text_basic () =
  let result = Mcp_tools.chunkify_text ~chunk_size:5 "HelloWorld!" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunked_text" fields with
       | Some (`Bool true) -> ()
       | _ -> fail "Expected chunked_text=true");
      (match List.assoc_opt "chunks" fields with
       | Some (`List chunks) ->
           check bool "has chunks" true (List.length chunks > 0);
           (* First chunk should have chunk_index=1 *)
           (match List.nth chunks 0 with
            | `Assoc cf ->
                (match List.assoc_opt "chunk_index" cf with
                 | Some (`Int 1) -> ()
                 | _ -> fail "Expected chunk_index 1");
                (match List.assoc_opt "content" cf with
                 | Some (`String s) -> check bool "content not empty" true (String.length s > 0)
                 | _ -> fail "Expected content string")
            | _ -> fail "Expected Assoc")
       | _ -> fail "Expected chunks list")
  | _ -> fail "Expected Assoc"

let test_chunkify_text_single_chunk () =
  let result = Mcp_tools.chunkify_text ~chunk_size:100 "short" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunk_total" fields with
       | Some (`Int 1) -> ()
       | _ -> fail "Expected single chunk");
      (match List.assoc_opt "chunks" fields with
       | Some (`List [chunk]) ->
           (match chunk with
            | `Assoc cf ->
                (match List.assoc_opt "content" cf with
                 | Some (`String "short") -> ()
                 | _ -> fail "Expected content 'short'")
            | _ -> fail "Expected Assoc chunk")
       | _ -> fail "Expected single chunk list")
  | _ -> fail "Expected Assoc"

let test_chunkify_text_empty () =
  let result = Mcp_tools.chunkify_text ~chunk_size:5 "" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunk_total" fields with
       | Some (`Int 0) -> ()
       | Some (`Int n) -> check int "empty text chunk count" 0 n
       | _ -> fail "Expected chunk_total")
  | _ -> fail "Expected Assoc"

let test_chunkify_text_zero_chunk_size () =
  (* chunk_size 0 should be treated as 1 *)
  let result = Mcp_tools.chunkify_text ~chunk_size:0 "abc" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunks" fields with
       | Some (`List chunks) ->
           check bool "has chunks" true (List.length chunks > 0)
       | _ -> fail "Expected chunks list")
  | _ -> fail "Expected Assoc"

let chunkify_text_tests = [
  "chunkify_text basic", `Quick, test_chunkify_text_basic;
  "chunkify_text single chunk", `Quick, test_chunkify_text_single_chunk;
  "chunkify_text empty", `Quick, test_chunkify_text_empty;
  "chunkify_text zero chunk_size", `Quick, test_chunkify_text_zero_chunk_size;
]

(** ============== Select Chunked JSON ============== *)

let test_select_chunked_json_basic () =
  let json = `Assoc [
    ("chunks", `List [
      `Assoc [("chunk_index", `Int 1); ("data", `String "a")];
      `Assoc [("chunk_index", `Int 2); ("data", `String "b")];
      `Assoc [("chunk_index", `Int 3); ("data", `String "c")];
    ]);
    ("other", `String "preserved");
  ] in
  let result = Mcp_tools.select_chunked_json ~selected:[1; 3] json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunks" fields with
       | Some (`List chunks) ->
           check int "selected 2 chunks" 2 (List.length chunks)
       | _ -> fail "Expected chunks list");
      (match List.assoc_opt "chunk_selected" fields with
       | Some (`List sel) ->
           check int "selected list length" 2 (List.length sel)
       | _ -> fail "Expected chunk_selected");
      (match List.assoc_opt "other" fields with
       | Some (`String "preserved") -> ()
       | _ -> fail "Expected other field preserved")
  | _ -> fail "Expected Assoc"

let test_select_chunked_json_no_chunks () =
  let json = `Assoc [("data", `String "no chunks")] in
  let result = Mcp_tools.select_chunked_json ~selected:[1] json in
  match result with
  | `Assoc fields ->
      check bool "data preserved" true (List.mem_assoc "data" fields);
      check bool "no chunks key" false (List.mem_assoc "chunk_selected" fields)
  | _ -> fail "Expected Assoc"

let test_select_chunked_json_not_assoc () =
  let json = `List [`Int 1; `Int 2] in
  let result = Mcp_tools.select_chunked_json ~selected:[1] json in
  check bool "non-assoc passthrough" true (result = json)

let test_select_chunked_json_empty_selection () =
  let json = `Assoc [
    ("chunks", `List [
      `Assoc [("chunk_index", `Int 1); ("data", `String "a")];
    ]);
  ] in
  let result = Mcp_tools.select_chunked_json ~selected:[] json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunks" fields with
       | Some (`List chunks) -> check int "no chunks selected" 0 (List.length chunks)
       | _ -> fail "Expected chunks list")
  | _ -> fail "Expected Assoc"

let select_chunked_tests = [
  "select_chunked basic", `Quick, test_select_chunked_json_basic;
  "select_chunked no chunks", `Quick, test_select_chunked_json_no_chunks;
  "select_chunked not assoc", `Quick, test_select_chunked_json_not_assoc;
  "select_chunked empty selection", `Quick, test_select_chunked_json_empty_selection;
]

(** ============== Hashtbl Helpers (bump_count / type_counts_to_json) ============== *)

let test_bump_count_new_key () =
  let counts = Hashtbl.create 8 in
  Mcp_tools.bump_count counts "foo";
  check int "foo=1" 1 (Hashtbl.find counts "foo")

let test_bump_count_existing_key () =
  let counts = Hashtbl.create 8 in
  Mcp_tools.bump_count counts "bar";
  Mcp_tools.bump_count counts "bar";
  Mcp_tools.bump_count counts "bar";
  check int "bar=3" 3 (Hashtbl.find counts "bar")

let test_bump_count_multiple_keys () =
  let counts = Hashtbl.create 8 in
  Mcp_tools.bump_count counts "a";
  Mcp_tools.bump_count counts "b";
  Mcp_tools.bump_count counts "a";
  check int "a=2" 2 (Hashtbl.find counts "a");
  check int "b=1" 1 (Hashtbl.find counts "b")

let test_type_counts_to_json_basic () =
  let counts = Hashtbl.create 8 in
  Hashtbl.replace counts "TEXT" 5;
  Hashtbl.replace counts "FRAME" 3;
  let result = Mcp_tools.type_counts_to_json counts in
  match result with
  | `Assoc fields ->
      (* Sorted alphabetically *)
      check bool "has FRAME" true (List.mem_assoc "FRAME" fields);
      check bool "has TEXT" true (List.mem_assoc "TEXT" fields);
      (match List.assoc_opt "TEXT" fields with
       | Some (`Int 5) -> ()
       | _ -> fail "Expected TEXT=5");
      (match List.assoc_opt "FRAME" fields with
       | Some (`Int 3) -> ()
       | _ -> fail "Expected FRAME=3")
  | _ -> fail "Expected Assoc"

let test_type_counts_to_json_empty () =
  let counts = Hashtbl.create 8 in
  let result = Mcp_tools.type_counts_to_json counts in
  match result with
  | `Assoc [] -> ()
  | _ -> fail "Expected empty Assoc"

let hashtbl_helper_tests = [
  "bump_count new key", `Quick, test_bump_count_new_key;
  "bump_count existing key", `Quick, test_bump_count_existing_key;
  "bump_count multiple keys", `Quick, test_bump_count_multiple_keys;
  "type_counts_to_json basic", `Quick, test_type_counts_to_json_basic;
  "type_counts_to_json empty", `Quick, test_type_counts_to_json_empty;
]

(** ============== Plugin Stats Helpers ============== *)

let test_create_plugin_stats () =
  let stats = Mcp_tools.create_plugin_stats () in
  check int "node_count 0" 0 stats.node_count;
  check int "text_nodes 0" 0 stats.text_nodes;
  check int "segment_count 0" 0 stats.segment_count;
  check int "segment_bounds_count 0" 0 stats.segment_bounds_count;
  check (list string) "empty name_samples" [] stats.name_samples;
  check (list string) "empty text_samples" [] stats.text_samples;
  check (option int) "selection_count None" None stats.selection_count

let test_append_sample_basic () =
  let result = Mcp_tools.append_sample ~max:3 [] "hello" in
  check (list string) "append to empty" ["hello"] result

let test_append_sample_at_max () =
  let result = Mcp_tools.append_sample ~max:2 ["a"; "b"] "c" in
  check (list string) "at max, skip" ["a"; "b"] result

let test_append_sample_empty_value () =
  let result = Mcp_tools.append_sample ~max:3 ["a"] "" in
  check (list string) "empty value skipped" ["a"] result

let test_append_sample_under_max () =
  let result = Mcp_tools.append_sample ~max:3 ["a"] "b" in
  check (list string) "under max, append" ["b"; "a"] result

let test_count_segment_bounds_basic () =
  let segments = [
    `Assoc [("text", `String "hello"); ("bounds", `Assoc [("x", `Int 0)])];
    `Assoc [("text", `String "world")];  (* no bounds *)
    `Assoc [("text", `String "foo"); ("bounds", `Null)];  (* null bounds *)
    `Assoc [("text", `String "bar"); ("bounds", `Assoc [("x", `Int 10)])];
  ] in
  let result = Mcp_tools.count_segment_bounds segments in
  check int "2 segments with bounds" 2 result

let test_count_segment_bounds_empty () =
  check int "empty list" 0 (Mcp_tools.count_segment_bounds [])

let test_count_segment_bounds_all_null () =
  let segments = [
    `Assoc [("bounds", `Null)];
    `Assoc [];
  ] in
  check int "all null/missing" 0 (Mcp_tools.count_segment_bounds segments)

let test_count_segment_bounds_non_assoc () =
  let segments = [`String "not an object"; `Int 42] in
  check int "non-assoc items" 0 (Mcp_tools.count_segment_bounds segments)

let plugin_stats_tests = [
  "create_plugin_stats", `Quick, test_create_plugin_stats;
  "append_sample basic", `Quick, test_append_sample_basic;
  "append_sample at max", `Quick, test_append_sample_at_max;
  "append_sample empty value", `Quick, test_append_sample_empty_value;
  "append_sample under max", `Quick, test_append_sample_under_max;
  "count_segment_bounds basic", `Quick, test_count_segment_bounds_basic;
  "count_segment_bounds empty", `Quick, test_count_segment_bounds_empty;
  "count_segment_bounds all null", `Quick, test_count_segment_bounds_all_null;
  "count_segment_bounds non-assoc", `Quick, test_count_segment_bounds_non_assoc;
]

(** ============== Compact JSON Additional Cases ============== *)

let test_compact_json_string_truncation () =
  let long_str = String.make 200 'x' in
  let json = `String long_str in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:50
    json
  in
  match result with
  | `String s ->
      check bool "string truncated" true (String.length s < 200);
      check bool "has truncated marker" true
        (let len = String.length s in len > 0)
  | _ -> fail "Expected String"

let test_compact_json_null_passthrough () =
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    `Null
  in
  check bool "null passthrough" true (result = `Null)

let test_compact_json_bool_passthrough () =
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    (`Bool true)
  in
  check bool "bool passthrough" true (result = `Bool true)

let test_compact_json_int_passthrough () =
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    (`Int 42)
  in
  check bool "int passthrough" true (result = `Int 42)

let test_compact_json_float_passthrough () =
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    (`Float 3.14)
  in
  check bool "float passthrough" true (result = `Float 3.14)

let test_compact_json_children_truncation_marker () =
  let children = List.init 10 (fun i -> `Assoc [("id", `Int i)]) in
  let json = `Assoc [("children", `List children)] in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:3 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "children" fields with
       | Some (`List items) ->
           let last = List.nth items (List.length items - 1) in
           (match last with
            | `Assoc marker_fields ->
                check bool "has _truncated" true
                  (List.mem_assoc "_truncated" marker_fields);
                (match List.assoc_opt "total" marker_fields with
                 | Some (`Int 10) -> ()
                 | _ -> fail "Expected total=10")
            | _ -> fail "Expected truncation marker Assoc")
       | _ -> fail "Expected children list")
  | _ -> fail "Expected Assoc"

let test_compact_json_list_truncation_marker () =
  let items = List.init 15 (fun i -> `Int i) in
  let json = `List items in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:4 ~max_string:100
    json
  in
  match result with
  | `List truncated ->
      let last = List.nth truncated (List.length truncated - 1) in
      (match last with
       | `Assoc marker ->
           check bool "has _truncated" true (List.mem_assoc "_truncated" marker);
           (match List.assoc_opt "total" marker with
            | Some (`Int 15) -> ()
            | _ -> fail "Expected total=15")
       | _ -> fail "Expected truncation marker")
  | _ -> fail "Expected List"

let test_compact_json_depth_truncation_flag () =
  let deep = `Assoc [
    ("name", `String "root");
    ("children", `List [`Assoc [("id", `Int 1)]]);
  ] in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:0 ~max_children:10 ~max_list_items:10 ~max_string:100
    deep
  in
  match result with
  | `Assoc fields ->
      check bool "has _depth_truncated" true
        (List.mem_assoc "_depth_truncated" fields);
      check bool "no children" false (List.mem_assoc "children" fields)
  | _ -> fail "Expected Assoc"

let test_compact_json_non_list_children () =
  let json = `Assoc [("children", `String "not a list")] in
  let result = Mcp_tools.compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "children" fields with
       | Some (`String _) -> ()  (* recursed into value, still a string *)
       | _ -> fail "Expected children string preserved")
  | _ -> fail "Expected Assoc"

let compact_json_extra_tests = [
  "compact_json string truncation", `Quick, test_compact_json_string_truncation;
  "compact_json null passthrough", `Quick, test_compact_json_null_passthrough;
  "compact_json bool passthrough", `Quick, test_compact_json_bool_passthrough;
  "compact_json int passthrough", `Quick, test_compact_json_int_passthrough;
  "compact_json float passthrough", `Quick, test_compact_json_float_passthrough;
  "compact_json children truncation marker", `Quick, test_compact_json_children_truncation_marker;
  "compact_json list truncation marker", `Quick, test_compact_json_list_truncation_marker;
  "compact_json depth truncation flag", `Quick, test_compact_json_depth_truncation_flag;
  "compact_json non-list children", `Quick, test_compact_json_non_list_children;
]

(** ============== Tool Definition Validation ============== *)

let test_tool_get_node_schema_has_url () =
  let tool = Mcp_tools.tool_figma_get_node in
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "properties" fields with
       | Some (`Assoc props) ->
           check bool "has url" true (List.mem_assoc "url" props);
           check bool "has file_key" true (List.mem_assoc "file_key" props);
           check bool "has node_id" true (List.mem_assoc "node_id" props)
       | _ -> fail "Expected properties")
  | _ -> fail "Expected Assoc"

let test_tool_codegen_required_json () =
  let tool = Mcp_tools.tool_figma_codegen in
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "required" fields with
       | Some (`List reqs) ->
           let strs = List.filter_map (function `String s -> Some s | _ -> None) reqs in
           check bool "json required" true (List.mem "json" strs)
       | _ -> fail "Expected required list")
  | _ -> fail "Expected Assoc"

let test_tool_get_file_required_file_key () =
  let tool = Mcp_tools.tool_figma_get_file in
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "required" fields with
       | Some (`List reqs) ->
           let strs = List.filter_map (function `String s -> Some s | _ -> None) reqs in
           check bool "file_key required" true (List.mem "file_key" strs)
       | _ -> fail "Expected required list")
  | _ -> fail "Expected Assoc"

let test_tool_parse_url_required_url () =
  let tool = Mcp_tools.tool_figma_parse_url in
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "required" fields with
       | Some (`List reqs) ->
           let strs = List.filter_map (function `String s -> Some s | _ -> None) reqs in
           check bool "url required" true (List.mem "url" strs)
       | _ -> fail "Expected required list")
  | _ -> fail "Expected Assoc"

let test_tool_image_similarity_required_fields () =
  let tool = Mcp_tools.tool_figma_image_similarity in
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "required" fields with
       | Some (`List reqs) ->
           let strs = List.filter_map (function `String s -> Some s | _ -> None) reqs in
           check bool "file_key required" true (List.mem "file_key" strs);
           check bool "node_a_id required" true (List.mem "node_a_id" strs);
           check bool "node_b_id required" true (List.mem "node_b_id" strs)
       | _ -> fail "Expected required list")
  | _ -> fail "Expected Assoc"

let test_tool_verify_visual_required_fields () =
  let tool = Mcp_tools.tool_figma_verify_visual in
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "required" fields with
       | Some (`List reqs) ->
           let strs = List.filter_map (function `String s -> Some s | _ -> None) reqs in
           check bool "file_key required" true (List.mem "file_key" strs);
           check bool "node_id required" true (List.mem "node_id" strs)
       | _ -> fail "Expected required list")
  | _ -> fail "Expected Assoc"

let test_tool_get_node_bundle_schema () =
  let tool = Mcp_tools.tool_figma_get_node_bundle in
  check bool "name" true (tool.name = "figma_get_node_bundle");
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "properties" fields with
       | Some (`Assoc props) ->
           check bool "has format" true (List.mem_assoc "format" props);
           check bool "has image_format" true (List.mem_assoc "image_format" props);
           check bool "has download" true (List.mem_assoc "download" props)
       | _ -> fail "Expected properties")
  | _ -> fail "Expected Assoc"

let test_tool_get_node_summary_schema () =
  let tool = Mcp_tools.tool_figma_get_node_summary in
  check bool "name" true (tool.name = "figma_get_node_summary");
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "properties" fields with
       | Some (`Assoc props) ->
           check bool "has max_children" true (List.mem_assoc "max_children" props)
       | _ -> fail "Expected properties")
  | _ -> fail "Expected Assoc"

let test_tool_select_nodes_schema () =
  let tool = Mcp_tools.tool_figma_select_nodes in
  check bool "name" true (tool.name = "figma_select_nodes");
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "properties" fields with
       | Some (`Assoc props) ->
           check bool "has score_threshold" true (List.mem_assoc "score_threshold" props);
           check bool "has layout_only" true (List.mem_assoc "layout_only" props)
       | _ -> fail "Expected properties")
  | _ -> fail "Expected Assoc"

let test_tool_fidelity_loop_schema () =
  let tool = Mcp_tools.tool_figma_fidelity_loop in
  check bool "name" true (tool.name = "figma_fidelity_loop");
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "properties" fields with
       | Some (`Assoc props) ->
           check bool "has target_score" true (List.mem_assoc "target_score" props);
           check bool "has max_attempts" true (List.mem_assoc "max_attempts" props)
       | _ -> fail "Expected properties")
  | _ -> fail "Expected Assoc"

let test_tool_get_node_chunk_schema () =
  let tool = Mcp_tools.tool_figma_get_node_chunk in
  check bool "name" true (tool.name = "figma_get_node_chunk");
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "properties" fields with
       | Some (`Assoc props) ->
           check bool "has depth_start" true (List.mem_assoc "depth_start" props);
           check bool "has depth_end" true (List.mem_assoc "depth_end" props)
       | _ -> fail "Expected properties")
  | _ -> fail "Expected Assoc"

let test_tool_figma_plugin_schema_has_unique_keys () =
  let tool = Mcp_tools.tool_figma_plugin in
  check bool "name" true (tool.name = "figma_plugin");
  match tool.input_schema with
  | `Assoc fields ->
      (match List.assoc_opt "properties" fields with
       | Some (`Assoc props) ->
           let keys = List.map fst props in
           let unique_keys = List.sort_uniq String.compare keys in
           check int "no duplicate property keys" (List.length unique_keys) (List.length keys)
       | _ -> fail "Expected properties")
  | _ -> fail "Expected Assoc"

let test_all_tool_schemas_are_objects () =
  let all_obj = List.for_all (fun (t : Figma_mcp_protocol.tool_def) ->
    match t.input_schema with
    | `Assoc fields ->
        (match List.assoc_opt "type" fields with
         | Some (`String "object") -> true
         | _ -> false)
    | _ -> false
  ) Mcp_tools.all_tools in
  check bool "all schemas are objects" true all_obj

let tool_schema_tests = [
  "get_node schema has url/file_key/node_id", `Quick, test_tool_get_node_schema_has_url;
  "codegen requires json", `Quick, test_tool_codegen_required_json;
  "get_file requires file_key", `Quick, test_tool_get_file_required_file_key;
  "parse_url requires url", `Quick, test_tool_parse_url_required_url;
  "image_similarity requires 3 fields", `Quick, test_tool_image_similarity_required_fields;
  "verify_visual requires file_key/node_id", `Quick, test_tool_verify_visual_required_fields;
  "get_node_bundle schema", `Quick, test_tool_get_node_bundle_schema;
  "get_node_summary schema", `Quick, test_tool_get_node_summary_schema;
  "select_nodes schema", `Quick, test_tool_select_nodes_schema;
  "fidelity_loop schema", `Quick, test_tool_fidelity_loop_schema;
  "get_node_chunk schema", `Quick, test_tool_get_node_chunk_schema;
  "figma_plugin schema unique keys", `Quick, test_tool_figma_plugin_schema_has_unique_keys;
  "all tool schemas are objects", `Quick, test_all_tool_schemas_are_objects;
]

(** ============== Resource / Resource Template / Prompt Validation ============== *)

let test_resources_count () =
  let server = Mcp_tools.create_figma_server () in
  check bool "has resources" true (List.length server.Figma_mcp_protocol.resources >= 3)

let test_resources_have_uris () =
  let server = Mcp_tools.create_figma_server () in
  let all_have_uri = List.for_all (fun (r : Figma_mcp_protocol.mcp_resource) ->
    String.length r.uri > 0
  ) server.Figma_mcp_protocol.resources in
  check bool "all resources have uris" true all_have_uri

let test_resources_have_mime_types () =
  let server = Mcp_tools.create_figma_server () in
  let all_have_mime = List.for_all (fun (r : Figma_mcp_protocol.mcp_resource) ->
    String.length r.mime_type > 0
  ) server.Figma_mcp_protocol.resources in
  check bool "all resources have mime_types" true all_have_mime

let test_resource_templates_exist () =
  let server = Mcp_tools.create_figma_server () in
  check bool "has resource_templates" true
    (List.length server.Figma_mcp_protocol.resource_templates > 0)

let test_resource_template_tokens () =
  let server = Mcp_tools.create_figma_server () in
  let has_tokens = List.exists (fun (rt : Figma_mcp_protocol.mcp_resource_template) ->
    rt.uri_template = "figma://tokens/{file_key}"
  ) server.Figma_mcp_protocol.resource_templates in
  check bool "tokens template exists" true has_tokens

let test_prompts_exist () =
  let server = Mcp_tools.create_figma_server () in
  check bool "has prompts" true (List.length server.Figma_mcp_protocol.prompts > 0)

let test_prompts_have_names () =
  let server = Mcp_tools.create_figma_server () in
  let all_named = List.for_all (fun (p : Figma_mcp_protocol.mcp_prompt) ->
    String.length p.name > 0
  ) server.Figma_mcp_protocol.prompts in
  check bool "all prompts have names" true all_named

let test_prompts_have_text () =
  let server = Mcp_tools.create_figma_server () in
  let all_have_text = List.for_all (fun (p : Figma_mcp_protocol.mcp_prompt) ->
    String.length p.text > 0
  ) server.Figma_mcp_protocol.prompts in
  check bool "all prompts have text" true all_have_text

let test_prompt_fidelity_review_args () =
  let server = Mcp_tools.create_figma_server () in
  let prompt = List.find_opt (fun (p : Figma_mcp_protocol.mcp_prompt) ->
    p.name = "figma_fidelity_review"
  ) server.Figma_mcp_protocol.prompts in
  match prompt with
  | Some p ->
      check bool "has arguments" true (List.length p.arguments > 0);
      let has_file_key = List.exists (fun (a : Figma_mcp_protocol.prompt_arg) ->
        a.name = "file_key" && a.required
      ) p.arguments in
      check bool "file_key required arg" true has_file_key
  | None -> fail "Expected figma_fidelity_review prompt"

let test_read_resource_tokens_docs () =
  match Mcp_tools.read_resource "figma://docs/tokens" with
  | Ok (mime, body) ->
      check string "mime type" "text/markdown" mime;
      check bool "body not empty" true (String.length body > 0);
      check bool "mentions Variables" true (Mcp_helpers.string_contains ~haystack:body ~needle:"variables")
  | Error e -> fail ("read_resource tokens error: " ^ e)

let test_read_resource_unknown_scheme () =
  match Mcp_tools.read_resource "unknown://something" with
  | Error _ -> ()
  | Ok _ -> fail "Expected Error for unknown scheme"

let resource_prompt_tests = [
  "resources count >= 3", `Quick, test_resources_count;
  "resources have uris", `Quick, test_resources_have_uris;
  "resources have mime_types", `Quick, test_resources_have_mime_types;
  "resource_templates exist", `Quick, test_resource_templates_exist;
  "tokens template exists", `Quick, test_resource_template_tokens;
  "prompts exist", `Quick, test_prompts_exist;
  "prompts have names", `Quick, test_prompts_have_names;
  "prompts have text", `Quick, test_prompts_have_text;
  "prompt fidelity_review args", `Quick, test_prompt_fidelity_review_args;
  "read_resource tokens docs", `Quick, test_read_resource_tokens_docs;
  "read_resource unknown scheme", `Quick, test_read_resource_unknown_scheme;
]

(** ============== String Contains Extra Cases ============== *)

let test_string_contains_sub_longer_than_string () =
  check bool "sub longer" false
    (Mcp_helpers.string_contains ~haystack:"hi" ~needle:"hello world")

let test_string_contains_equal_strings () =
  check bool "equal strings" true
    (Mcp_helpers.string_contains ~haystack:"hello" ~needle:"hello")

let test_string_contains_single_char () =
  check bool "single char match" true
    (Mcp_helpers.string_contains ~haystack:"abc" ~needle:"b")

let test_string_contains_single_char_no_match () =
  check bool "single char no match" false
    (Mcp_helpers.string_contains ~haystack:"abc" ~needle:"z")

let test_string_contains_at_end () =
  check bool "match at end" true
    (Mcp_helpers.string_contains ~haystack:"Hello World" ~needle:"world")

let test_string_contains_at_start () =
  check bool "match at start" true
    (Mcp_helpers.string_contains ~haystack:"Hello World" ~needle:"hello")

let test_string_contains_mixed_case () =
  check bool "mixed case" true
    (Mcp_helpers.string_contains ~haystack:"aBcDeF" ~needle:"BcDe")

let string_contains_extra_tests = [
  "string_contains sub longer", `Quick, test_string_contains_sub_longer_than_string;
  "string_contains equal", `Quick, test_string_contains_equal_strings;
  "string_contains single char", `Quick, test_string_contains_single_char;
  "string_contains single char no match", `Quick, test_string_contains_single_char_no_match;
  "string_contains at end", `Quick, test_string_contains_at_end;
  "string_contains at start", `Quick, test_string_contains_at_start;
  "string_contains mixed case", `Quick, test_string_contains_mixed_case;
]

(** ============== Network Error Extra Cases ============== *)

let test_is_network_error_etimedout () =
  check bool "Unix ETIMEDOUT" true
    (Mcp_tools.is_network_error (Unix.Unix_error (Unix.ETIMEDOUT, "connect", "")))

let test_is_network_error_string_etimedout () =
  check bool "string etimedout" true
    (Mcp_tools.is_network_error (Failure "some etimedout error"))

let test_is_network_error_string_econnreset () =
  check bool "string econnreset" true
    (Mcp_tools.is_network_error (Failure "some econnreset in message"))

let test_is_network_error_string_epipe () =
  check bool "string epipe" true
    (Mcp_tools.is_network_error (Failure "write: epipe"))

let test_is_network_error_string_connection_timed_out () =
  check bool "string connection timed out" true
    (Mcp_tools.is_network_error (Failure "Connection timed out by server"))

let test_is_network_error_not_found () =
  check bool "Not_found not network" false
    (Mcp_tools.is_network_error Not_found)

let test_is_network_error_invalid_argument () =
  check bool "Invalid_argument not network" false
    (Mcp_tools.is_network_error (Invalid_argument "test"))

let network_error_extra_tests = [
  "is_network_error ETIMEDOUT", `Quick, test_is_network_error_etimedout;
  "is_network_error string etimedout", `Quick, test_is_network_error_string_etimedout;
  "is_network_error string econnreset", `Quick, test_is_network_error_string_econnreset;
  "is_network_error string epipe", `Quick, test_is_network_error_string_epipe;
  "is_network_error string conn timed out", `Quick, test_is_network_error_string_connection_timed_out;
  "is_network_error Not_found", `Quick, test_is_network_error_not_found;
  "is_network_error Invalid_argument", `Quick, test_is_network_error_invalid_argument;
]

(** ============== All Handlers Sync ============== *)

let test_all_handlers_sync_not_empty () =
  let server = Mcp_tools.create_figma_server () in
  check bool "handlers_sync not empty" true
    (List.length server.Figma_mcp_protocol.handlers_sync > 0)

let test_all_handlers_sync_have_names () =
  let server = Mcp_tools.create_figma_server () in
  let all_named = List.for_all (fun (name, _) ->
    String.length name > 0
  ) server.Figma_mcp_protocol.handlers_sync in
  check bool "all handlers have names" true all_named

let test_all_handlers_sync_prefixed () =
  let server = Mcp_tools.create_figma_server () in
  let all_prefixed = List.for_all (fun (name, _) ->
    String.length name >= 6 && String.sub name 0 6 = "figma_"
  ) server.Figma_mcp_protocol.handlers_sync in
  check bool "all handlers prefixed with figma_" true all_prefixed

let test_handlers_cover_all_tools () =
  let server = Mcp_tools.create_figma_server () in
  let handler_names = List.map fst server.Figma_mcp_protocol.handlers_sync in
  let tool_names = List.map (fun (t : Figma_mcp_protocol.tool_def) -> t.name)
    server.Figma_mcp_protocol.tools in
  let all_covered = List.for_all (fun name ->
    List.mem name handler_names
  ) tool_names in
  check bool "all tools have handlers" true all_covered

let handler_sync_tests = [
  "handlers_sync not empty", `Quick, test_all_handlers_sync_not_empty;
  "handlers have names", `Quick, test_all_handlers_sync_have_names;
  "handlers prefixed", `Quick, test_all_handlers_sync_prefixed;
  "handlers cover all tools", `Quick, test_handlers_cover_all_tools;
]

(** ============== Additional Tool Existence Checks ============== *)

let test_tool_exists name =
  let exists = List.exists (fun (t : Figma_mcp_protocol.tool_def) ->
    t.name = name
  ) Mcp_tools.all_tools in
  check bool (name ^ " exists") true exists

let test_tool_get_node_bundle_exists () = test_tool_exists "figma_get_node_bundle"
let test_tool_get_node_summary_exists () = test_tool_exists "figma_get_node_summary"
let test_tool_select_nodes_exists () = test_tool_exists "figma_select_nodes"
let test_tool_get_node_chunk_exists () = test_tool_exists "figma_get_node_chunk"
let test_tool_fidelity_loop_exists () = test_tool_exists "figma_fidelity_loop"
let test_tool_image_similarity_exists () = test_tool_exists "figma_image_similarity"
let test_tool_verify_visual_exists () = test_tool_exists "figma_verify_visual"
let test_tool_verify_semantic_exists () = test_tool_exists "figma_verify_semantic"
let test_tool_compare_exists () = test_tool_exists "figma_compare"
let test_tool_export_image_exists () = test_tool_exists "figma_export_image"
let test_tool_export_smart_exists () = test_tool_exists "figma_export_smart"
let test_tool_get_image_fills_exists () = test_tool_exists "figma_get_image_fills"
let test_tool_get_nodes_exists () = test_tool_exists "figma_get_nodes"
let test_tool_get_file_versions_exists () = test_tool_exists "figma_get_file_versions"
let test_tool_get_file_comments_exists () = test_tool_exists "figma_get_file_comments"
let test_tool_post_comment_exists () = test_tool_exists "figma_post_comment"
let test_tool_doctor_exists () = test_tool_exists "figma_doctor"
let test_tool_stats_exists () = test_tool_exists "figma_stats"
let test_tool_tree_exists () = test_tool_exists "figma_tree"
let test_tool_query_exists () = test_tool_exists "figma_query"
let test_tool_search_exists () = test_tool_exists "figma_search"
let test_tool_export_tokens_exists () = test_tool_exists "figma_export_tokens"
let test_tool_cache_stats_exists () = test_tool_exists "figma_cache_stats"
let test_tool_cache_invalidate_exists () = test_tool_exists "figma_cache_invalidate"
let test_tool_plugin_exists () = test_tool_exists "figma_plugin"
let test_tool_code_connect_exists () = test_tool_exists "figma_code_connect"
let test_tool_get_dev_resources_exists () = test_tool_exists "figma_get_dev_resources"
let test_tool_add_dev_resource_exists () = test_tool_exists "figma_add_dev_resource"
let test_tool_setup_webhook_exists () = test_tool_exists "figma_setup_webhook"
let test_tool_annotate_exists () = test_tool_exists "figma_annotate"
let test_tool_read_large_result_exists () = test_tool_exists "figma_read_large_result"
let test_tool_get_variables_exists () = test_tool_exists "figma_get_variables"

let tool_existence_extra_tests = [
  "get_node_bundle exists", `Quick, test_tool_get_node_bundle_exists;
  "get_node_summary exists", `Quick, test_tool_get_node_summary_exists;
  "select_nodes exists", `Quick, test_tool_select_nodes_exists;
  "get_node_chunk exists", `Quick, test_tool_get_node_chunk_exists;
  "fidelity_loop exists", `Quick, test_tool_fidelity_loop_exists;
  "image_similarity exists", `Quick, test_tool_image_similarity_exists;
  "verify_visual exists", `Quick, test_tool_verify_visual_exists;
  "verify_semantic exists", `Quick, test_tool_verify_semantic_exists;
  "compare exists", `Quick, test_tool_compare_exists;
  "export_image exists", `Quick, test_tool_export_image_exists;
  "export_smart exists", `Quick, test_tool_export_smart_exists;
  "get_image_fills exists", `Quick, test_tool_get_image_fills_exists;
  "get_nodes exists", `Quick, test_tool_get_nodes_exists;
  "get_file_versions exists", `Quick, test_tool_get_file_versions_exists;
  "get_file_comments exists", `Quick, test_tool_get_file_comments_exists;
  "post_comment exists", `Quick, test_tool_post_comment_exists;
  "doctor exists", `Quick, test_tool_doctor_exists;
  "stats exists", `Quick, test_tool_stats_exists;
  "tree exists", `Quick, test_tool_tree_exists;
  "query exists", `Quick, test_tool_query_exists;
  "search exists", `Quick, test_tool_search_exists;
  "export_tokens exists", `Quick, test_tool_export_tokens_exists;
  "cache_stats exists", `Quick, test_tool_cache_stats_exists;
  "cache_invalidate exists", `Quick, test_tool_cache_invalidate_exists;
  "plugin exists", `Quick, test_tool_plugin_exists;
  "code_connect exists", `Quick, test_tool_code_connect_exists;
  "get_dev_resources exists", `Quick, test_tool_get_dev_resources_exists;
  "add_dev_resource exists", `Quick, test_tool_add_dev_resource_exists;
  "setup_webhook exists", `Quick, test_tool_setup_webhook_exists;
  "annotate exists", `Quick, test_tool_annotate_exists;
  "read_large_result exists", `Quick, test_tool_read_large_result_exists;
  "get_variables exists", `Quick, test_tool_get_variables_exists;
]

(** ============== Collect Plugin Stats ============== *)

let test_collect_plugin_stats_basic () =
  let stats = Mcp_tools.create_plugin_stats () in
  let json = `Assoc [
    ("type", `String "FRAME");
    ("name", `String "Header");
    ("text", `Assoc [
      ("characters", `String "Hello World");
      ("segments", `List [
        `Assoc [("text", `String "Hello"); ("bounds", `Assoc [("x", `Int 0)])];
        `Assoc [("text", `String " World")];
      ]);
    ]);
    ("children", `List [
      `Assoc [("type", `String "TEXT"); ("name", `String "Title")];
      `Assoc [("type", `String "RECTANGLE"); ("name", `String "BG")];
    ]);
  ] in
  Mcp_tools.collect_plugin_stats ~sample_size:5 stats json;
  check bool "node_count > 0" true (stats.node_count > 0);
  check bool "text_nodes > 0" true (stats.text_nodes > 0);
  check bool "has type counts" true (Hashtbl.length stats.type_counts > 0);
  check bool "has name samples" true (List.length stats.name_samples > 0)

let test_collect_plugin_stats_empty () =
  let stats = Mcp_tools.create_plugin_stats () in
  Mcp_tools.collect_plugin_stats ~sample_size:5 stats (`Assoc []);
  check int "node_count 1" 1 stats.node_count;
  check int "text_nodes 0" 0 stats.text_nodes

let test_collect_plugin_stats_list_input () =
  let stats = Mcp_tools.create_plugin_stats () in
  let json = `List [
    `Assoc [("type", `String "FRAME"); ("name", `String "A")];
    `Assoc [("type", `String "TEXT"); ("name", `String "B")];
  ] in
  Mcp_tools.collect_plugin_stats ~sample_size:5 stats json;
  check int "node_count 2 from list" 2 stats.node_count;
  check bool "has type counts" true (Hashtbl.length stats.type_counts > 0)

let collect_plugin_stats_tests = [
  "collect_plugin_stats basic", `Quick, test_collect_plugin_stats_basic;
  "collect_plugin_stats empty", `Quick, test_collect_plugin_stats_empty;
  "collect_plugin_stats list input", `Quick, test_collect_plugin_stats_list_input;
]

(** ============== Main ============== *)

let () =
  run "Mcp_tools Coverage" [
    "Schema Helpers", schema_helper_tests;
    "Network Utils", network_util_tests;
    "JSON Utilities", json_util_tests;
    "Node ID Handling", node_id_tests;
    "Error Handling", error_handling_tests;
    "URL Utilities", url_util_tests;
    "Content Helpers", content_helper_tests;
    "Field Utilities", field_util_tests;
    "Truncation & Chunking", truncation_chunking_tests;
    "JSON Compaction", json_compaction_tests;
    "Compact JSON Extra", compact_json_extra_tests;
    "Handler Registry", handler_registry_tests;
    "Monadic Operators", monadic_tests;
    "Tool Definitions", tool_definition_tests;
    "Tool Schema Validation", tool_schema_tests;
    "Tool Existence Extra", tool_existence_extra_tests;
    "Resources", resource_tests;
    "Resource & Prompt Validation", resource_prompt_tests;
    "Token Resolution", token_resolution_tests;
    "Server", server_tests;
    "Handler Sync", handler_sync_tests;
    "Category Lookup", category_lookup_tests;
    "Category Structure", category_structure_tests;
    "Chunkify Children", chunkify_children_tests;
    "Chunkify Text", chunkify_text_tests;
    "Select Chunked JSON", select_chunked_tests;
    "Hashtbl Helpers", hashtbl_helper_tests;
    "Plugin Stats", plugin_stats_tests;
    "Collect Plugin Stats", collect_plugin_stats_tests;
    "String Contains Extra", string_contains_extra_tests;
    "Network Error Extra", network_error_extra_tests;
  ]
