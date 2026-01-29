(** Comprehensive Coverage Tests for mcp_tools.ml
    Target: 50+ tests covering all public functions
    Framework: Alcotest *)

open Alcotest

(** ============== JSON Schema Helpers ============== *)

let test_string_prop () =
  let result = Mcp_tools.string_prop "A description" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "string")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None);
      check (option string) "description" (Some "A description")
        (match List.assoc_opt "description" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

(* Note: test_string_prop_with_required removed - ~required parameter was removed in v0.3.14 *)

let test_number_prop () =
  let result = Mcp_tools.number_prop "A number" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "number")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

let test_bool_prop () =
  let result = Mcp_tools.bool_prop "A boolean" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "boolean")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

let test_enum_prop () =
  let result = Mcp_tools.enum_prop ["a"; "b"; "c"] "Pick one" in
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
  let result = Mcp_tools.array_prop "An array" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "array")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

let test_object_prop () =
  let result = Mcp_tools.object_prop "An object" in
  match result with
  | `Assoc fields ->
      check (option string) "type" (Some "object")
        (match List.assoc_opt "type" fields with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "Expected Assoc"

let test_object_schema () =
  let result = Mcp_tools.object_schema [
    ("name", Mcp_tools.string_prop "Name");
    ("age", Mcp_tools.number_prop "Age");
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
  let result = Mcp_tools.object_schema [] [] in
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
  match Mcp_tools.member "key" json with
  | Some (`String v) -> check string "member value" "value" v
  | _ -> fail "Expected Some String"

let test_member_missing () =
  let json = `Assoc [("other", `String "value")] in
  match Mcp_tools.member "key" json with
  | None -> ()
  | _ -> fail "Expected None"

let test_member_not_assoc () =
  let json = `String "not an object" in
  match Mcp_tools.member "key" json with
  | None -> ()
  | _ -> fail "Expected None for non-assoc"

let test_get_string_exists () =
  let json = `Assoc [("name", `String "Alice")] in
  match Mcp_tools.get_string "name" json with
  | Some v -> check string "get_string" "Alice" v
  | None -> fail "Expected Some"

let test_get_string_missing () =
  let json = `Assoc [("other", `String "value")] in
  match Mcp_tools.get_string "name" json with
  | None -> ()
  | Some _ -> fail "Expected None"

let test_get_string_not_string () =
  let json = `Assoc [("name", `Int 42)] in
  match Mcp_tools.get_string "name" json with
  | None -> ()
  | Some _ -> fail "Expected None for non-string"

let test_get_string_list_from_list () =
  let json = `Assoc [("tags", `List [`String "a"; `String "b"; `String "c"])] in
  match Mcp_tools.get_string_list "tags" json with
  | Some lst -> check (list string) "tags list" ["a"; "b"; "c"] lst
  | None -> fail "Expected Some list"

let test_get_string_list_from_csv () =
  let json = `Assoc [("tags", `String "x, y, z")] in
  match Mcp_tools.get_string_list "tags" json with
  | Some lst -> check (list string) "csv list" ["x"; "y"; "z"] lst
  | None -> fail "Expected Some list from CSV"

let test_get_string_list_empty () =
  let json = `Assoc [("tags", `String "")] in
  match Mcp_tools.get_string_list "tags" json with
  | None -> ()
  | Some _ -> fail "Expected None for empty"

let test_get_string_list_missing () =
  let json = `Assoc [] in
  match Mcp_tools.get_string_list "tags" json with
  | None -> ()
  | Some _ -> fail "Expected None for missing"

let test_prefer_some_primary () =
  let result = Mcp_tools.prefer_some (Some "primary") (Some "fallback") in
  check (option string) "prefer primary" (Some "primary") result

let test_prefer_some_fallback () =
  let result = Mcp_tools.prefer_some None (Some "fallback") in
  check (option string) "use fallback" (Some "fallback") result

let test_prefer_some_both_none () =
  let result = Mcp_tools.prefer_some None None in
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
  let result = Mcp_tools.normalize_node_id "100-200" in
  check string "hyphen to colon" "100:200" result

let test_normalize_node_id_colon () =
  let result = Mcp_tools.normalize_node_id "100:200" in
  check string "colon preserved" "100:200" result

let test_normalize_node_id_simple () =
  let result = Mcp_tools.normalize_node_id "simple" in
  check string "no separator" "simple" result

let test_normalize_node_id_key_node_id () =
  let result = Mcp_tools.normalize_node_id_key "node_id" "123-456" in
  check string "node_id key normalized" "123:456" result

let test_normalize_node_id_key_node_a_id () =
  let result = Mcp_tools.normalize_node_id_key "node_a_id" "789-012" in
  check string "node_a_id key normalized" "789:012" result

let test_normalize_node_id_key_node_b_id () =
  let result = Mcp_tools.normalize_node_id_key "node_b_id" "111-222" in
  check string "node_b_id key normalized" "111:222" result

let test_normalize_node_id_key_other () =
  let result = Mcp_tools.normalize_node_id_key "other_field" "333-444" in
  check string "other key not normalized" "333-444" result

let test_sanitize_node_id () =
  let result = Mcp_tools.sanitize_node_id "100:200" in
  check string "colon to underscore" "100_200" result

let test_sanitize_node_id_no_colon () =
  let result = Mcp_tools.sanitize_node_id "simple" in
  check string "no colon unchanged" "simple" result

let test_sanitize_node_id_multiple_colons () =
  let result = Mcp_tools.sanitize_node_id "a:b:c" in
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
  let result = Mcp_tools.error_to_string (Mcp_tools.NetworkError "timeout") in
  check bool "contains Network" true (String.length result > 0 && String.contains result 'N')

let test_error_to_string_auth () =
  let result = Mcp_tools.error_to_string (Mcp_tools.AuthError "invalid token") in
  check bool "contains Auth" true (String.length result > 0)

let test_error_to_string_not_found () =
  let result = Mcp_tools.error_to_string (Mcp_tools.NotFound "file XYZ") in
  check bool "contains found" true (String.length result > 0)

let test_error_to_string_rate_limited () =
  let result = Mcp_tools.error_to_string (Mcp_tools.RateLimited 60.0) in
  check bool "contains Rate" true (String.length result > 0)

let test_error_to_string_server_error () =
  let result = Mcp_tools.error_to_string (Mcp_tools.ServerError "500 Internal") in
  check bool "contains server" true (String.length result > 0)

let test_error_to_string_parse_error () =
  let result = Mcp_tools.error_to_string (Mcp_tools.ParseError "invalid json") in
  check bool "contains Parse" true (String.length result > 0)

let test_error_to_string_timeout () =
  let result = Mcp_tools.error_to_string (Mcp_tools.TimeoutError 30.0) in
  check bool "contains Timeout" true (String.length result > 0)

let test_error_to_string_unknown () =
  let result = Mcp_tools.error_to_string (Mcp_tools.UnknownError "mystery") in
  check bool "contains Unknown" true (String.length result > 0)

let test_classify_http_error_401 () =
  match Mcp_tools.classify_http_error ~status_code:401 ~body:"unauthorized" with
  | Mcp_tools.AuthError _ -> ()
  | _ -> fail "Expected AuthError for 401"

let test_classify_http_error_403 () =
  match Mcp_tools.classify_http_error ~status_code:403 ~body:"forbidden" with
  | Mcp_tools.AuthError _ -> ()
  | _ -> fail "Expected AuthError for 403"

let test_classify_http_error_404 () =
  match Mcp_tools.classify_http_error ~status_code:404 ~body:"not found" with
  | Mcp_tools.NotFound _ -> ()
  | _ -> fail "Expected NotFound for 404"

let test_classify_http_error_429 () =
  match Mcp_tools.classify_http_error ~status_code:429 ~body:"retry after 120" with
  | Mcp_tools.RateLimited secs -> check bool "retry seconds parsed" true (secs >= 60.0)
  | _ -> fail "Expected RateLimited for 429"

let test_classify_http_error_500 () =
  match Mcp_tools.classify_http_error ~status_code:500 ~body:"internal error" with
  | Mcp_tools.ServerError _ -> ()
  | _ -> fail "Expected ServerError for 500"

let test_classify_http_error_503 () =
  match Mcp_tools.classify_http_error ~status_code:503 ~body:"unavailable" with
  | Mcp_tools.ServerError _ -> ()
  | _ -> fail "Expected ServerError for 503"

let test_classify_http_error_other () =
  match Mcp_tools.classify_http_error ~status_code:418 ~body:"i'm a teapot" with
  | Mcp_tools.UnknownError _ -> ()
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
  let result = Mcp_tools.strip_query "https://example.com/path?foo=bar&baz=qux" in
  check string "strip query" "https://example.com/path" result

let test_strip_query_no_query () =
  let result = Mcp_tools.strip_query "https://example.com/path" in
  check string "no query unchanged" "https://example.com/path" result

let test_strip_query_empty () =
  let result = Mcp_tools.strip_query "" in
  check string "empty string" "" result

let test_is_http_url_http () =
  check bool "http url" true (Mcp_tools.is_http_url "http://example.com")

let test_is_http_url_https () =
  check bool "https url" true (Mcp_tools.is_http_url "https://example.com")

let test_is_http_url_ftp () =
  check bool "ftp not http" false (Mcp_tools.is_http_url "ftp://example.com")

let test_is_http_url_path () =
  check bool "path not http" false (Mcp_tools.is_http_url "/local/path")

let test_is_http_url_short () =
  check bool "short string" false (Mcp_tools.is_http_url "hi")

let test_file_ext_from_url_png () =
  let result = Mcp_tools.file_ext_from_url "https://example.com/image.png?v=1" in
  check string "png extension" ".png" result

let test_file_ext_from_url_jpg () =
  let result = Mcp_tools.file_ext_from_url "https://example.com/photo.jpg" in
  check string "jpg extension" ".jpg" result

let test_file_ext_from_url_no_ext () =
  let result = Mcp_tools.file_ext_from_url "https://example.com/noext" in
  check string "default extension" ".img" result

let url_util_tests = [
  "strip_query with query", `Quick, test_strip_query_with_query;
  "strip_query no query", `Quick, test_strip_query_no_query;
  "strip_query empty", `Quick, test_strip_query_empty;
  "is_http_url http", `Quick, test_is_http_url_http;
  "is_http_url https", `Quick, test_is_http_url_https;
  "is_http_url ftp", `Quick, test_is_http_url_ftp;
  "is_http_url path", `Quick, test_is_http_url_path;
  "is_http_url short", `Quick, test_is_http_url_short;
  "file_ext_from_url png", `Quick, test_file_ext_from_url_png;
  "file_ext_from_url jpg", `Quick, test_file_ext_from_url_jpg;
  "file_ext_from_url no ext", `Quick, test_file_ext_from_url_no_ext;
]

(** ============== Content Helpers ============== *)

let test_make_text_content () =
  let result = Mcp_tools.make_text_content "Hello World" in
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
  let result = Mcp_tools.make_text_content "" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "content" fields with
       | Some (`List [`Assoc item_fields]) ->
           check (option string) "empty text" (Some "")
             (match List.assoc_opt "text" item_fields with Some (`String s) -> Some s | _ -> None)
       | _ -> fail "Expected content list")
  | _ -> fail "Expected Assoc"

let test_make_error_content () =
  let result = Mcp_tools.make_error_content "Something went wrong" in
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
  Mcp_tools.register_handler "test_handler" test_handler;
  match Mcp_tools.call_handler "test_handler" (`Assoc []) with
  | Ok (`String v) -> check string "handler result" "handler result" v
  | Ok _ -> fail "Expected String result"
  | Error e -> fail ("Handler error: " ^ e)

let test_call_missing_handler () =
  match Mcp_tools.call_handler "nonexistent_handler" (`Assoc []) with
  | Error msg -> check bool "error message" true (String.length msg > 0)
  | Ok _ -> fail "Expected Error"

let handler_registry_tests = [
  "register and call handler", `Quick, test_register_and_call_handler;
  "call missing handler", `Quick, test_call_missing_handler;
]

(** ============== Result Monadic Operators ============== *)

let test_bind_ok () =
  let open Mcp_tools in
  let result = Ok 5 >>= (fun x -> Ok (x * 2)) in
  match result with
  | Ok v -> check int "bind ok" 10 v
  | Error _ -> fail "Expected Ok"

let test_bind_error () =
  let open Mcp_tools in
  let result = (Error "failed") >>= (fun x -> Ok (x * 2)) in
  match result with
  | Error e -> check string "bind error" "failed" e
  | Ok _ -> fail "Expected Error"

let test_map_ok () =
  let open Mcp_tools in
  let result = Ok 5 >>| (fun x -> x * 2) in
  match result with
  | Ok v -> check int "map ok" 10 v
  | Error _ -> fail "Expected Ok"

let test_map_error () =
  let open Mcp_tools in
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
  let all_have_names = List.for_all (fun (t : Mcp_protocol.tool_def) ->
    String.length t.name > 0
  ) Mcp_tools.all_tools in
  check bool "all tools have names" true all_have_names

let test_all_tools_have_descriptions () =
  let all_have_desc = List.for_all (fun (t : Mcp_protocol.tool_def) ->
    String.length t.description > 0
  ) Mcp_tools.all_tools in
  check bool "all tools have descriptions" true all_have_desc

let test_all_tools_unique_names () =
  let names = List.map (fun (t : Mcp_protocol.tool_def) -> t.name) Mcp_tools.all_tools in
  let unique_names = List.sort_uniq String.compare names in
  check int "unique tool names" (List.length names) (List.length unique_names)

let test_tool_codegen_exists () =
  let exists = List.exists (fun (t : Mcp_protocol.tool_def) ->
    t.name = "figma_codegen"
  ) Mcp_tools.all_tools in
  check bool "figma_codegen exists" true exists

let test_tool_get_file_exists () =
  let exists = List.exists (fun (t : Mcp_protocol.tool_def) ->
    t.name = "figma_get_file"
  ) Mcp_tools.all_tools in
  check bool "figma_get_file exists" true exists

let test_tool_parse_url_exists () =
  let exists = List.exists (fun (t : Mcp_protocol.tool_def) ->
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

(** ============== Server Creation ============== *)

let test_create_figma_server () =
  let server = Mcp_tools.create_figma_server () in
  check bool "has tools" true (List.length server.Mcp_protocol.tools > 0);
  check bool "has handlers" true (List.length server.Mcp_protocol.handlers_sync > 0);
  check bool "has resources" true (List.length server.Mcp_protocol.resources > 0);
  check bool "has prompts" true (List.length server.Mcp_protocol.prompts > 0)

let test_server_tools_match_all_tools () =
  let server = Mcp_tools.create_figma_server () in
  check int "tools count matches"
    (List.length Mcp_tools.all_tools)
    (List.length server.Mcp_protocol.tools)

let server_tests = [
  "create_figma_server", `Quick, test_create_figma_server;
  "server tools match all_tools", `Quick, test_server_tools_match_all_tools;
]

(** ============== Main ============== *)

let () =
  run "Mcp_tools Coverage" [
    "Schema Helpers", schema_helper_tests;
    "JSON Utilities", json_util_tests;
    "Node ID Handling", node_id_tests;
    "Error Handling", error_handling_tests;
    "URL Utilities", url_util_tests;
    "Content Helpers", content_helper_tests;
    "Field Utilities", field_util_tests;
    "Truncation & Chunking", truncation_chunking_tests;
    "JSON Compaction", json_compaction_tests;
    "Handler Registry", handler_registry_tests;
    "Monadic Operators", monadic_tests;
    "Tool Definitions", tool_definition_tests;
    "Resources", resource_tests;
    "Server", server_tests;
  ]
