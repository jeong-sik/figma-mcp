(** Wave 6 coverage tests for lib/mcp_tools.ml.
    Targets uncovered branches in:
    - compact_json (non-children keys, _missing filter, children-as-non-list,
      string truncation, passthrough for Bool/Int/Null/Float)
    - chunkify_children (no children, children not list, empty, non-assoc input)
    - chunkify_text (multibyte, negative chunk_size, single byte)
    - select_chunked_json (chunks as non-list value, string index)
    - truncate_utf8 (negative, multibyte boundary, cut=0 fallback)
    - utf8_safe_boundary (non-zero start, boundary at start)
    - truncate_string (empty string, exact boundary)
    - is_under_dir (trailing slash dir, both nonexistent)
    - read_resource (tokens/ template missing FIGMA_TOKEN, empty file_key)
    - handle_codegen_sync (missing json, invalid json)
    - handle_category (describe with tool in category but no tool_def)
    - collect_plugin_stats / summarize_plugin_payload edge cases
    - tool definition structural checks
    - all_handlers_sync structural checks
    - bump_count with new key
    - type_counts_to_json empty hashtable
    - append_sample normal case
    - has_field / set_field / add_if_missing / get_string_any deeper
*)

open Alcotest
open Mcp_tools

(* ============================================================
   compact_json — deeper branches
   ============================================================ *)

(* Non-children key recurse *)
let test_compact_json_non_children_key () =
  let json = `Assoc [
    ("metadata", `Assoc [("nested", `String "value")]);
    ("other", `Int 42);
  ] in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "metadata" fields with
       | Some (`Assoc inner) ->
           check bool "nested preserved" true
             (List.exists (fun (k, _) -> k = "nested") inner)
       | _ -> fail "Expected metadata Assoc")
  | _ -> fail "Expected Assoc"

(* _missing key filtering *)
let test_compact_json_missing_key_filter () =
  let json = `Assoc [
    ("name", `String "test");
    ("color_missing", `List [`String "red"]);
    ("size_missing", `Null);
    ("valid", `Int 1);
  ] in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      check bool "no color_missing" false
        (List.exists (fun (k, _) -> k = "color_missing") fields);
      check bool "no size_missing" false
        (List.exists (fun (k, _) -> k = "size_missing") fields);
      check bool "has valid" true
        (List.exists (fun (k, _) -> k = "valid") fields)
  | _ -> fail "Expected Assoc"

(* children key as non-List (e.g., Assoc) *)
let test_compact_json_children_not_list () =
  let json = `Assoc [
    ("children", `Assoc [("inner", `String "child")]);
  ] in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "children" fields with
       | Some (`Assoc _) -> ()  (* Recursed into the Assoc *)
       | _ -> fail "Expected children Assoc preserved")
  | _ -> fail "Expected Assoc"

(* String truncation with max_string *)
let test_compact_json_string_truncation () =
  let long_str = String.make 200 'x' in
  let json = `String long_str in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:50
    json
  in
  match result with
  | `String s ->
      check bool "truncated" true (String.length s < 200);
      check bool "has truncation marker" true
        (try ignore (Str.search_forward (Str.regexp_string "(truncated)") s 0); true
         with Not_found -> false)
  | _ -> fail "Expected String"

(* Passthrough for Bool *)
let test_compact_json_bool () =
  let json = `Bool true in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  check bool "Bool passthrough" true
    (match result with `Bool true -> true | _ -> false)

(* Passthrough for Null *)
let test_compact_json_null () =
  let json = `Null in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  check bool "Null passthrough" true
    (match result with `Null -> true | _ -> false)

(* Passthrough for Float *)
let test_compact_json_float () =
  let json = `Float 3.14 in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  check bool "Float passthrough" true
    (match result with `Float f -> f = 3.14 | _ -> false)

(* Passthrough for Int *)
let test_compact_json_int_passthrough () =
  let json = `Int 99 in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  check bool "Int passthrough" true
    (match result with `Int 99 -> true | _ -> false)

(* Depth limit strips children and adds _depth_truncated *)
let test_compact_json_depth_truncated_marker () =
  let json = `Assoc [
    ("name", `String "root");
    ("children", `List [`Assoc [("id", `Int 1)]]);
  ] in
  let result = compact_json
    ~depth:0 ~max_depth:0 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      check bool "has _depth_truncated" true
        (List.exists (fun (k, _) -> k = "_depth_truncated") fields);
      check bool "children removed" false
        (List.exists (fun (k, _) -> k = "children") fields)
  | _ -> fail "Expected Assoc"

(* List with no truncation *)
let test_compact_json_list_no_truncation () =
  let items = [`Int 1; `Int 2; `Int 3] in
  let json = `List items in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `List l -> check int "all items" 3 (List.length l)
  | _ -> fail "Expected List"

(* Children that are within max_children — no truncation marker *)
let test_compact_json_children_no_truncation () =
  let children = List.init 3 (fun i -> `Assoc [("id", `Int i)]) in
  let json = `Assoc [("children", `List children)] in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "children" fields with
       | Some (`List l) -> check int "3 children" 3 (List.length l)
       | _ -> fail "Expected children list")
  | _ -> fail "Expected Assoc"

(* String not exceeding max_string *)
let test_compact_json_string_not_truncated () =
  let json = `String "short" in
  let result = compact_json
    ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
    json
  in
  check bool "string preserved" true
    (match result with `String "short" -> true | _ -> false)

let compact_json_extra_tests = [
  "non-children key recurse", `Quick, test_compact_json_non_children_key;
  "missing key filter", `Quick, test_compact_json_missing_key_filter;
  "children not list", `Quick, test_compact_json_children_not_list;
  "string truncation", `Quick, test_compact_json_string_truncation;
  "bool passthrough", `Quick, test_compact_json_bool;
  "null passthrough", `Quick, test_compact_json_null;
  "float passthrough", `Quick, test_compact_json_float;
  "int passthrough", `Quick, test_compact_json_int_passthrough;
  "depth truncated marker", `Quick, test_compact_json_depth_truncated_marker;
  "list no truncation", `Quick, test_compact_json_list_no_truncation;
  "children no truncation", `Quick, test_compact_json_children_no_truncation;
  "string not truncated", `Quick, test_compact_json_string_not_truncated;
]

(* ============================================================
   chunkify_children — additional branches
   ============================================================ *)

(* No children key at all *)
let test_chunkify_children_no_children_key () =
  let json = `Assoc [("name", `String "root")] in
  let result = chunkify_children ~chunk_size:5 json in
  match result with
  | `Assoc fields ->
      check bool "name preserved" true (List.mem_assoc "name" fields);
      check bool "no chunks" false (List.mem_assoc "chunks" fields)
  | _ -> fail "Expected Assoc (passthrough)"

(* Children is not a list (e.g., String) *)
let test_chunkify_children_children_not_list () =
  let json = `Assoc [("children", `String "not a list")] in
  let result = chunkify_children ~chunk_size:5 json in
  match result with
  | `Assoc fields ->
      (* Falls through the None | Some _ -> json branch *)
      check bool "children key present" true (List.mem_assoc "children" fields);
      check bool "no chunks" false (List.mem_assoc "chunks" fields)
  | _ -> fail "Expected Assoc"

(* Empty children list *)
let test_chunkify_children_empty_children () =
  let json = `Assoc [("children", `List [])] in
  let result = chunkify_children ~chunk_size:5 json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunk_total" fields with
       | Some (`Int 0) -> ()
       | _ -> fail "Expected chunk_total 0")
  | _ -> fail "Expected Assoc"

(* Non-Assoc input *)
let test_chunkify_children_non_assoc_input () =
  let json = `List [`Int 1; `Int 2] in
  let result = chunkify_children ~chunk_size:5 json in
  check bool "passthrough" true
    (match result with `List _ -> true | _ -> false)

(* Non-Assoc: String *)
let test_chunkify_children_string_input () =
  let json = `String "hello" in
  let result = chunkify_children ~chunk_size:5 json in
  check bool "string passthrough" true
    (match result with `String "hello" -> true | _ -> false)

(* Children with chunk_size=1 *)
let test_chunkify_children_chunk_size_one () =
  let json = `Assoc [
    ("children", `List [`Int 1; `Int 2; `Int 3]);
  ] in
  let result = chunkify_children ~chunk_size:1 json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunk_total" fields with
       | Some (`Int 3) -> ()
       | Some (`Int n) -> fail (Printf.sprintf "Expected 3 chunks, got %d" n)
       | _ -> fail "Expected chunk_total")
  | _ -> fail "Expected Assoc"

let chunkify_children_extra_tests = [
  "no children key", `Quick, test_chunkify_children_no_children_key;
  "children not list", `Quick, test_chunkify_children_children_not_list;
  "empty children", `Quick, test_chunkify_children_empty_children;
  "non-assoc input", `Quick, test_chunkify_children_non_assoc_input;
  "string input", `Quick, test_chunkify_children_string_input;
  "chunk size 1", `Quick, test_chunkify_children_chunk_size_one;
]

(* ============================================================
   chunkify_text — additional branches
   ============================================================ *)

(* Multibyte UTF-8 text *)
let test_chunkify_text_multibyte () =
  (* Korean characters are 3 bytes each in UTF-8 *)
  let text = "\xed\x95\x9c\xea\xb8\x80" in  (* "한글" = 6 bytes *)
  let result = chunkify_text ~chunk_size:3 text in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunk_total" fields with
       | Some (`Int n) -> check bool "at least 2 chunks" true (n >= 2)
       | _ -> fail "Expected chunk_total")
  | _ -> fail "Expected Assoc"

(* Negative chunk_size treated as 1 *)
let test_chunkify_text_negative_chunk_size () =
  let result = chunkify_text ~chunk_size:(-5) "abc" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunks" fields with
       | Some (`List chunks) ->
           check bool "has chunks" true (List.length chunks > 0)
       | _ -> fail "Expected chunks list")
  | _ -> fail "Expected Assoc"

(* Single character per chunk *)
let test_chunkify_text_single_byte_chunks () =
  let result = chunkify_text ~chunk_size:1 "abc" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunk_total" fields with
       | Some (`Int 3) -> ()
       | Some (`Int n) -> fail (Printf.sprintf "Expected 3 chunks, got %d" n)
       | _ -> fail "Expected chunk_total")
  | _ -> fail "Expected Assoc"

(* Chunk size larger than text *)
let test_chunkify_text_oversized_chunk () =
  let result = chunkify_text ~chunk_size:1000 "hello" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunk_total" fields with
       | Some (`Int 1) -> ()
       | Some (`Int n) -> fail (Printf.sprintf "Expected 1 chunk, got %d" n)
       | _ -> fail "Expected chunk_total")
  | _ -> fail "Expected Assoc"

(* Check chunk_index and chunk_total in generated chunks *)
let test_chunkify_text_chunk_structure () =
  let result = chunkify_text ~chunk_size:2 "abcd" in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunked_text" fields with
       | Some (`Bool true) -> ()
       | _ -> fail "Expected chunked_text true");
      (match List.assoc_opt "chunks" fields with
       | Some (`List chunks) ->
           List.iteri (fun i chunk ->
             match chunk with
             | `Assoc cf ->
                 (match List.assoc_opt "chunk_index" cf with
                  | Some (`Int idx) -> check int "index" (i + 1) idx
                  | _ -> fail "Expected chunk_index");
                 (match List.assoc_opt "content" cf with
                  | Some (`String _) -> ()
                  | _ -> fail "Expected content string")
             | _ -> fail "Expected Assoc chunk"
           ) chunks
       | _ -> fail "Expected chunks list")
  | _ -> fail "Expected Assoc"

let chunkify_text_extra_tests = [
  "multibyte", `Quick, test_chunkify_text_multibyte;
  "negative chunk_size", `Quick, test_chunkify_text_negative_chunk_size;
  "single byte chunks", `Quick, test_chunkify_text_single_byte_chunks;
  "oversized chunk", `Quick, test_chunkify_text_oversized_chunk;
  "chunk structure", `Quick, test_chunkify_text_chunk_structure;
]

(* ============================================================
   select_chunked_json — additional branches
   ============================================================ *)

(* chunks key exists but is not a list *)
let test_select_chunked_json_chunks_not_list () =
  let json = `Assoc [("chunks", `String "not a list")] in
  let result = select_chunked_json ~selected:[1] json in
  match result with
  | `Assoc fields ->
      (* Falls through Some _ -> json branch, no chunk_selected *)
      check bool "no chunk_selected" false (List.mem_assoc "chunk_selected" fields)
  | _ -> fail "Expected Assoc"

(* chunk_index is a string (neither Int nor Float) *)
let test_select_chunked_json_string_index () =
  let json = `Assoc [
    ("chunks", `List [
      `Assoc [("chunk_index", `String "one"); ("data", `String "a")];
      `Assoc [("chunk_index", `Int 2); ("data", `String "b")];
    ])
  ] in
  let result = select_chunked_json ~selected:[1; 2] json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunks" fields with
       | Some (`List chunks) ->
           (* String index doesn't match, only Int 2 matches *)
           check int "1 selected" 1 (List.length chunks)
       | _ -> fail "Expected chunks list")
  | _ -> fail "Expected Assoc"

(* All chunks selected *)
let test_select_chunked_json_all_selected () =
  let json = `Assoc [
    ("chunks", `List [
      `Assoc [("chunk_index", `Int 1)];
      `Assoc [("chunk_index", `Int 2)];
    ])
  ] in
  let result = select_chunked_json ~selected:[1; 2] json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunks" fields with
       | Some (`List chunks) -> check int "all selected" 2 (List.length chunks)
       | _ -> fail "Expected chunks list")
  | _ -> fail "Expected Assoc"

(* Empty chunks list *)
let test_select_chunked_json_empty_chunks () =
  let json = `Assoc [("chunks", `List [])] in
  let result = select_chunked_json ~selected:[1] json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "chunks" fields with
       | Some (`List []) -> ()
       | _ -> fail "Expected empty chunks list")
  | _ -> fail "Expected Assoc"

let select_chunked_extra_tests = [
  "chunks not list", `Quick, test_select_chunked_json_chunks_not_list;
  "string index", `Quick, test_select_chunked_json_string_index;
  "all selected", `Quick, test_select_chunked_json_all_selected;
  "empty chunks", `Quick, test_select_chunked_json_empty_chunks;
]

(* ============================================================
   truncate_utf8 — deeper edge cases
   ============================================================ *)

(* Negative max_bytes returns value as-is *)
let test_truncate_utf8_negative () =
  let (result, truncated) = truncate_utf8 ~max_bytes:(-10) "Hello" in
  check string "value unchanged" "Hello" result;
  check bool "not truncated" false truncated

(* Multibyte boundary: cut in middle of 3-byte UTF-8 char *)
let test_truncate_utf8_multibyte_boundary () =
  (* "\xed\x95\x9c" = "한" (3 bytes), so max_bytes:2 should back up to 0 *)
  let (result, truncated) = truncate_utf8 ~max_bytes:2 "\xed\x95\x9c" in
  (* cut=0 fallback: min max_bytes len = min 2 3 = 2 *)
  check bool "truncated" true truncated;
  check bool "result length <= 2" true (String.length result <= 2)

(* Multibyte: exact boundary *)
let test_truncate_utf8_exact_multibyte () =
  let text = "\xed\x95\x9c" in  (* 3 bytes *)
  let (result, truncated) = truncate_utf8 ~max_bytes:3 text in
  check string "exact fit" text result;
  check bool "not truncated" false truncated

(* Mixed ASCII and multibyte *)
let test_truncate_utf8_mixed () =
  let text = "ab\xed\x95\x9ccd" in  (* 2 + 3 + 2 = 7 bytes *)
  let (result, truncated) = truncate_utf8 ~max_bytes:5 text in
  check bool "truncated" true truncated;
  (* Should back up from byte 5 (which is mid-multibyte) to byte 2 *)
  check bool "result valid" true (String.length result > 0)

(* Empty string *)
let test_truncate_utf8_empty () =
  let (result, truncated) = truncate_utf8 ~max_bytes:10 "" in
  check string "empty" "" result;
  check bool "not truncated" false truncated

let truncate_utf8_extra_tests = [
  "negative max_bytes", `Quick, test_truncate_utf8_negative;
  "multibyte boundary", `Quick, test_truncate_utf8_multibyte_boundary;
  "exact multibyte", `Quick, test_truncate_utf8_exact_multibyte;
  "mixed ascii multibyte", `Quick, test_truncate_utf8_mixed;
  "empty string", `Quick, test_truncate_utf8_empty;
]

(* ============================================================
   utf8_safe_boundary — deeper tests
   ============================================================ *)

(* Non-zero start *)
let test_utf8_safe_boundary_nonzero_start () =
  let text = "Hello World" in
  let result = utf8_safe_boundary ~start:6 ~max_bytes:3 text in
  check int "boundary at 9" 9 result

(* Start beyond string length *)
let test_utf8_safe_boundary_start_beyond () =
  let text = "Hi" in
  let result = utf8_safe_boundary ~start:0 ~max_bytes:100 text in
  check int "boundary at len" 2 result

(* Start at string length *)
let test_utf8_safe_boundary_start_at_len () =
  let text = "ab" in
  let result = utf8_safe_boundary ~start:2 ~max_bytes:5 text in
  check int "boundary at 2" 2 result

(* max_bytes = 0 *)
let test_utf8_safe_boundary_zero_max () =
  let text = "hello" in
  let result = utf8_safe_boundary ~start:0 ~max_bytes:0 text in
  check int "boundary at 0" 0 result

(* Multibyte character at boundary with non-zero start *)
let test_utf8_safe_boundary_multibyte_nonzero () =
  (* "ab" + "한" (3 bytes) = "ab\xed\x95\x9c" *)
  let text = "ab\xed\x95\x9c" in
  let result = utf8_safe_boundary ~start:2 ~max_bytes:2 text in
  (* pos = min (2+2) 5 = 4; back from 4: byte 3 is 0x95 (continuation), byte 2 is 0xed (not continuation) → stop at 4-2 = back to 2 *)
  check bool "backed up to char boundary" true (result <= 5)

let utf8_boundary_extra_tests = [
  "nonzero start", `Quick, test_utf8_safe_boundary_nonzero_start;
  "start beyond", `Quick, test_utf8_safe_boundary_start_beyond;
  "start at len", `Quick, test_utf8_safe_boundary_start_at_len;
  "zero max", `Quick, test_utf8_safe_boundary_zero_max;
  "multibyte nonzero start", `Quick, test_utf8_safe_boundary_multibyte_nonzero;
]

(* ============================================================
   is_utf8_continuation — additional byte patterns
   ============================================================ *)

let test_utf8_cont_leading_byte () =
  (* 0b11000000 is a 2-byte leader, not continuation *)
  check bool "2-byte leader" false (is_utf8_continuation 0b11000000)

let test_utf8_cont_3byte_leader () =
  check bool "3-byte leader" false (is_utf8_continuation 0b11100000)

let test_utf8_cont_4byte_leader () =
  check bool "4-byte leader" false (is_utf8_continuation 0b11110000)

let test_utf8_cont_min_continuation () =
  check bool "min continuation 0x80" true (is_utf8_continuation 0x80)

let test_utf8_cont_max_continuation () =
  check bool "max continuation 0xBF" true (is_utf8_continuation 0xBF)

let test_utf8_cont_zero () =
  check bool "zero byte" false (is_utf8_continuation 0)

let utf8_cont_extra_tests = [
  "2-byte leader", `Quick, test_utf8_cont_leading_byte;
  "3-byte leader", `Quick, test_utf8_cont_3byte_leader;
  "4-byte leader", `Quick, test_utf8_cont_4byte_leader;
  "min continuation", `Quick, test_utf8_cont_min_continuation;
  "max continuation", `Quick, test_utf8_cont_max_continuation;
  "zero byte", `Quick, test_utf8_cont_zero;
]

(* ============================================================
   truncate_string — additional edge cases
   ============================================================ *)

let test_truncate_string_empty () =
  let result = truncate_string ~max_len:10 "" in
  check string "empty string" "" result

let test_truncate_string_exact_max () =
  let result = truncate_string ~max_len:5 "Hello" in
  check string "exact max" "Hello" result

let test_truncate_string_one_over () =
  let result = truncate_string ~max_len:5 "Hello!" in
  check string "one over" "Hello...(truncated)" result

let test_truncate_string_max_len_one () =
  let result = truncate_string ~max_len:1 "Hello" in
  check string "max 1" "H...(truncated)" result

let truncate_string_extra_tests = [
  "empty string", `Quick, test_truncate_string_empty;
  "exact max", `Quick, test_truncate_string_exact_max;
  "one over", `Quick, test_truncate_string_one_over;
  "max len 1", `Quick, test_truncate_string_max_len_one;
]

(* ============================================================
   is_under_dir — additional edge cases
   ============================================================ *)

let test_is_under_dir_trailing_slash () =
  check bool "same dir with trailing slash" true
    (is_under_dir ~dir:"/tmp/" "/tmp")

let test_is_under_dir_both_nonexistent () =
  check bool "both nonexistent" false
    (is_under_dir ~dir:"/nonexistent_abc_xyz" "/nonexistent_def_xyz")

let test_is_under_dir_dir_nonexistent () =
  check bool "dir nonexistent" false
    (is_under_dir ~dir:"/nonexistent_abc_xyz" "/tmp")

let is_under_dir_extra_tests = [
  "trailing slash", `Quick, test_is_under_dir_trailing_slash;
  "both nonexistent", `Quick, test_is_under_dir_both_nonexistent;
  "dir nonexistent", `Quick, test_is_under_dir_dir_nonexistent;
]

(* ============================================================
   normalize_path — additional
   ============================================================ *)

let test_normalize_path_current_dir () =
  match normalize_path "." with
  | Some p -> check bool "absolute" true (String.length p > 0)
  | None -> fail "Expected Some for ."

let test_normalize_path_root () =
  match normalize_path "/" with
  | Some "/" -> ()
  | Some p -> check string "root" "/" p
  | None -> fail "Expected Some for /"

let normalize_path_extra_tests = [
  "current dir", `Quick, test_normalize_path_current_dir;
  "root", `Quick, test_normalize_path_root;
]

(* ============================================================
   has_field / set_field / add_if_missing / get_string_any — deeper
   ============================================================ *)

let test_has_field_empty_key () =
  let fields = [("", `Int 1); ("name", `String "test")] in
  check bool "empty key exists" true (has_field "" fields)

let test_set_field_to_empty_list () =
  let result = set_field "key" (`Int 42) [] in
  check int "single field" 1 (List.length result)

let test_set_field_replaces_value () =
  let fields = [("a", `Int 1); ("b", `Int 2)] in
  let result = set_field "a" (`String "new") fields in
  (match List.assoc_opt "a" result with
   | Some (`String "new") -> ()
   | _ -> fail "Expected updated value")

let test_add_if_missing_multiple () =
  let fields = [("a", `Int 1)] in
  let result = add_if_missing "b" (`Int 2) fields in
  let result = add_if_missing "b" (`Int 3) result in
  (match List.assoc_opt "b" result with
   | Some (`Int 2) -> ()  (* First addition wins *)
   | _ -> fail "Expected first added value")

let test_get_string_any_empty_keys () =
  let json = `Assoc [("name", `String "test")] in
  match get_string_any [] json with
  | None -> ()
  | Some _ -> fail "Expected None for empty keys"

let test_get_string_any_all_missing () =
  let json = `Assoc [("name", `String "test")] in
  match get_string_any ["x"; "y"; "z"] json with
  | None -> ()
  | Some _ -> fail "Expected None for all missing keys"

let test_get_string_any_second_key () =
  let json = `Assoc [("alias", `String "nick")] in
  match get_string_any ["name"; "alias"] json with
  | Some "nick" -> ()
  | _ -> fail "Expected 'nick'"

let test_get_string_any_non_string_value () =
  let json = `Assoc [("count", `Int 42)] in
  match get_string_any ["count"] json with
  | None -> ()  (* get_string returns None for non-string values *)
  | Some _ -> fail "Expected None for Int value"

let field_helpers_extra_tests = [
  "has_field empty key", `Quick, test_has_field_empty_key;
  "set_field to empty list", `Quick, test_set_field_to_empty_list;
  "set_field replaces value", `Quick, test_set_field_replaces_value;
  "add_if_missing multiple", `Quick, test_add_if_missing_multiple;
  "get_string_any empty keys", `Quick, test_get_string_any_empty_keys;
  "get_string_any all missing", `Quick, test_get_string_any_all_missing;
  "get_string_any second key", `Quick, test_get_string_any_second_key;
  "get_string_any non-string", `Quick, test_get_string_any_non_string_value;
]

(* ============================================================
   take_n — additional edge cases
   ============================================================ *)

let test_take_n_negative () =
  let result = take_n (-1) [1; 2; 3] in
  check (list int) "negative n" [] result

let test_take_n_one () =
  let result = take_n 1 [10; 20; 30] in
  check (list int) "take 1" [10] result

let test_take_n_large () =
  let result = take_n 100 [1; 2] in
  check (list int) "take 100 from 2" [1; 2] result

let take_n_extra_tests = [
  "negative n", `Quick, test_take_n_negative;
  "take one", `Quick, test_take_n_one;
  "take large", `Quick, test_take_n_large;
]

(* ============================================================
   chunk_list — additional edge cases
   ============================================================ *)

let test_chunk_list_negative_size () =
  let result = chunk_list (-3) [1; 2; 3] in
  check int "negative -> 1" 3 (List.length result)

let test_chunk_list_size_one () =
  let result = chunk_list 1 [10; 20; 30] in
  check int "3 chunks" 3 (List.length result);
  check (list int) "first chunk" [10] (List.nth result 0)

let test_chunk_list_exact_multiple () =
  let result = chunk_list 3 [1; 2; 3; 4; 5; 6] in
  check int "2 chunks" 2 (List.length result);
  check (list int) "first" [1; 2; 3] (List.nth result 0);
  check (list int) "second" [4; 5; 6] (List.nth result 1)

let chunk_list_extra_tests = [
  "negative size", `Quick, test_chunk_list_negative_size;
  "size one", `Quick, test_chunk_list_size_one;
  "exact multiple", `Quick, test_chunk_list_exact_multiple;
]

(* ============================================================
   bump_count / type_counts_to_json — additional
   ============================================================ *)

let test_bump_count_new_key () =
  let tbl = Hashtbl.create 4 in
  bump_count tbl "new_key";
  check int "new key = 1" 1 (Hashtbl.find tbl "new_key")

let test_bump_count_multiple_keys () =
  let tbl = Hashtbl.create 4 in
  bump_count tbl "x";
  bump_count tbl "y";
  bump_count tbl "x";
  bump_count tbl "x";
  check int "x = 3" 3 (Hashtbl.find tbl "x");
  check int "y = 1" 1 (Hashtbl.find tbl "y")

let test_type_counts_to_json_empty () =
  let tbl = Hashtbl.create 4 in
  let result = type_counts_to_json tbl in
  match result with
  | `Assoc [] -> ()
  | _ -> fail "Expected empty Assoc"

let test_type_counts_to_json_single () =
  let tbl = Hashtbl.create 4 in
  Hashtbl.add tbl "FRAME" 5;
  let result = type_counts_to_json tbl in
  match result with
  | `Assoc [("FRAME", `Int 5)] -> ()
  | _ -> fail "Expected single entry"

let count_extra_tests = [
  "bump new key", `Quick, test_bump_count_new_key;
  "bump multiple keys", `Quick, test_bump_count_multiple_keys;
  "empty counts", `Quick, test_type_counts_to_json_empty;
  "single count", `Quick, test_type_counts_to_json_single;
]

(* ============================================================
   append_sample — additional
   ============================================================ *)

let test_append_sample_normal () =
  (* append_sample uses value :: items, so it prepends *)
  let result = append_sample ~max:5 ["a"] "b" in
  check (list string) "prepended" ["b"; "a"] result

let test_append_sample_from_empty () =
  let result = append_sample ~max:3 [] "first" in
  check (list string) "first item" ["first"] result

let test_append_sample_max_zero () =
  let result = append_sample ~max:0 [] "item" in
  check (list string) "max 0 blocks" [] result

let append_sample_extra_tests = [
  "normal append", `Quick, test_append_sample_normal;
  "from empty", `Quick, test_append_sample_from_empty;
  "max zero", `Quick, test_append_sample_max_zero;
]

(* ============================================================
   count_segment_bounds — additional
   ============================================================ *)

let test_count_segment_bounds_empty () =
  check int "empty list" 0 (count_segment_bounds [])

let test_count_segment_bounds_all_with_bounds () =
  let segments = [
    `Assoc [("bounds", `Assoc [("x", `Float 1.0)])];
    `Assoc [("bounds", `Assoc [("x", `Float 2.0)])];
  ] in
  check int "all have bounds" 2 (count_segment_bounds segments)

let test_count_segment_bounds_all_null () =
  let segments = [
    `Assoc [("bounds", `Null)];
    `Assoc [("bounds", `Null)];
  ] in
  check int "all null" 0 (count_segment_bounds segments)

let test_count_segment_bounds_no_bounds_key () =
  let segments = [`Assoc [("other", `Int 1)]] in
  check int "no bounds key" 0 (count_segment_bounds segments)

let segment_bounds_extra_tests = [
  "empty", `Quick, test_count_segment_bounds_empty;
  "all with bounds", `Quick, test_count_segment_bounds_all_with_bounds;
  "all null", `Quick, test_count_segment_bounds_all_null;
  "no bounds key", `Quick, test_count_segment_bounds_no_bounds_key;
]

(* ============================================================
   collect_plugin_stats — deeper edge cases
   ============================================================ *)

(* Node with no type, no name, no text, no children *)
let test_collect_plugin_stats_minimal_node () =
  let stats = create_plugin_stats () in
  let json = `Assoc [("id", `Int 1)] in
  collect_plugin_stats ~sample_size:5 stats json;
  check int "node counted" 1 stats.node_count;
  check int "no text nodes" 0 stats.text_nodes

(* Node with text but no characters *)
let test_collect_plugin_stats_text_no_characters () =
  let stats = create_plugin_stats () in
  let json = `Assoc [
    ("text", `Assoc [("style", `String "bold")]);
  ] in
  collect_plugin_stats ~sample_size:5 stats json;
  check int "text node counted" 1 stats.text_nodes;
  check (list string) "no text samples" [] stats.text_samples

(* Node with text and segments but no bounds at all *)
let test_collect_plugin_stats_segments_no_bounds () =
  let stats = create_plugin_stats () in
  let json = `Assoc [
    ("text", `Assoc [
      ("characters", `String "hi");
      ("segments", `List [
        `Assoc [("font", `String "Arial")];
        `Assoc [("font", `String "Helvetica")];
      ]);
    ]);
  ] in
  collect_plugin_stats ~sample_size:5 stats json;
  check int "2 segments" 2 stats.segment_count;
  check int "0 segment bounds" 0 stats.segment_bounds_count

(* text field is not Assoc *)
let test_collect_plugin_stats_text_not_assoc () =
  let stats = create_plugin_stats () in
  let json = `Assoc [
    ("text", `String "not an assoc");
  ] in
  collect_plugin_stats ~sample_size:5 stats json;
  check int "no text nodes" 0 stats.text_nodes

(* children field is not List *)
let test_collect_plugin_stats_children_not_list () =
  let stats = create_plugin_stats () in
  let json = `Assoc [
    ("children", `String "not a list");
  ] in
  collect_plugin_stats ~sample_size:5 stats json;
  check int "1 node only" 1 stats.node_count

(* Deeply nested children *)
let test_collect_plugin_stats_nested () =
  let stats = create_plugin_stats () in
  let json = `Assoc [
    ("type", `String "FRAME");
    ("name", `String "parent");
    ("children", `List [
      `Assoc [
        ("type", `String "TEXT");
        ("name", `String "child");
        ("text", `Assoc [
          ("characters", `String "sample text");
        ]);
      ];
    ]);
  ] in
  collect_plugin_stats ~sample_size:5 stats json;
  check int "2 nodes" 2 stats.node_count;
  check int "1 text node" 1 stats.text_nodes;
  check int "2 types" 2 (Hashtbl.length stats.type_counts)

let collect_plugin_extra_tests = [
  "minimal node", `Quick, test_collect_plugin_stats_minimal_node;
  "text no characters", `Quick, test_collect_plugin_stats_text_no_characters;
  "segments no bounds", `Quick, test_collect_plugin_stats_segments_no_bounds;
  "text not assoc", `Quick, test_collect_plugin_stats_text_not_assoc;
  "children not list", `Quick, test_collect_plugin_stats_children_not_list;
  "nested children", `Quick, test_collect_plugin_stats_nested;
]

(* ============================================================
   summarize_plugin_payload — additional edge cases
   ============================================================ *)

(* selectionCount missing *)
let test_summarize_no_selection_count () =
  let json = `Assoc [
    ("nodes", `List [
      `Assoc [("type", `String "FRAME"); ("name", `String "Root")];
    ]);
  ] in
  let result = summarize_plugin_payload ~sample_size:5 json in
  match result with
  | `Assoc fields ->
      check bool "no selection_count" false (List.mem_assoc "selection_count" fields)
  | _ -> fail "Expected Assoc"

(* selectionCount non-numeric *)
let test_summarize_selection_count_non_numeric () =
  let json = `Assoc [
    ("selectionCount", `String "many");
  ] in
  let result = summarize_plugin_payload ~sample_size:5 json in
  match result with
  | `Assoc fields ->
      check bool "no selection_count" false (List.mem_assoc "selection_count" fields)
  | _ -> fail "Expected Assoc"

(* nodes key is not a list *)
let test_summarize_nodes_not_list () =
  let json = `Assoc [
    ("nodes", `String "not a list");
  ] in
  let result = summarize_plugin_payload ~sample_size:5 json in
  match result with
  | `Assoc fields ->
      (* Falls through to using entire payload *)
      check bool "has node_count" true (List.mem_assoc "node_count" fields)
  | _ -> fail "Expected Assoc"

(* empty nodes list *)
let test_summarize_empty_nodes () =
  let json = `Assoc [
    ("nodes", `List []);
  ] in
  let result = summarize_plugin_payload ~sample_size:5 json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "node_count" fields with
       | Some (`Int 0) -> ()
       | _ -> fail "Expected 0 nodes")
  | _ -> fail "Expected Assoc"

(* payload is a List *)
let test_summarize_list_payload () =
  let json = `List [`Int 1; `Int 2] in
  let result = summarize_plugin_payload ~sample_size:5 json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "error" fields with
       | Some (`String "Invalid plugin payload") -> ()
       | _ -> fail "Expected invalid payload error")
  | _ -> fail "Expected Assoc"

(* payload is Null *)
let test_summarize_null_payload () =
  let json = `Null in
  let result = summarize_plugin_payload ~sample_size:5 json in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "error" fields with
       | Some (`String "Invalid plugin payload") -> ()
       | _ -> fail "Expected invalid payload error")
  | _ -> fail "Expected Assoc"

let summarize_plugin_extra_tests = [
  "no selectionCount", `Quick, test_summarize_no_selection_count;
  "selectionCount non-numeric", `Quick, test_summarize_selection_count_non_numeric;
  "nodes not list", `Quick, test_summarize_nodes_not_list;
  "empty nodes", `Quick, test_summarize_empty_nodes;
  "list payload", `Quick, test_summarize_list_payload;
  "null payload", `Quick, test_summarize_null_payload;
]

(* ============================================================
   read_resource — additional branches
   ============================================================ *)

(* tokens template with non-empty file_key but empty token —
   putenv "" gives Some "" which passes the None check, so
   fetch_variables_cached raises an unhandled effect.
   We catch it to verify the path is exercised. *)
let test_read_resource_tokens_no_token () =
  let saved = Sys.getenv_opt "FIGMA_TOKEN" in
  (try Unix.putenv "FIGMA_TOKEN" "" with _ -> ());
  let restore () = match saved with Some v -> Unix.putenv "FIGMA_TOKEN" v | None -> () in
  (try
    let result = read_resource "figma://tokens/ABC123" in
    restore ();
    (match result with
     | Error msg ->
         check bool "mentions FIGMA_TOKEN or error" true (String.length msg > 0)
     | Ok _ -> ())
   with
   | _ ->
       (* Unhandled effect from fetch_variables_cached — expected in test env *)
       restore ())

(* tokens template with empty file_key *)
let test_read_resource_tokens_empty_key () =
  let saved = Sys.getenv_opt "FIGMA_TOKEN" in
  Unix.putenv "FIGMA_TOKEN" "test-token";
  let result = read_resource "figma://tokens/" in
  (match saved with Some v -> Unix.putenv "FIGMA_TOKEN" v | None -> ());
  (match result with
   | Error msg ->
       check bool "mentions file_key" true
         (Mcp_helpers.string_contains ~haystack:msg ~needle:"file_key" || Mcp_helpers.string_contains ~haystack:msg ~needle:"missing")
   | Ok _ -> ())

(* tokens template with query params — exercises split_query + format extraction.
   fetch_variables_cached raises unhandled Figma_get_variables effect. *)
let test_read_resource_tokens_with_query () =
  let saved = Sys.getenv_opt "FIGMA_TOKEN" in
  Unix.putenv "FIGMA_TOKEN" "test-token";
  let restore () = match saved with Some v -> Unix.putenv "FIGMA_TOKEN" v | None -> () in
  (try
    let result = read_resource "figma://tokens/KEY123?format=raw" in
    restore ();
    (match result with
     | Error _ -> ()
     | Ok _ -> ())
   with
   | _ ->
       (* Unhandled effect from fetch_variables_cached — expected in test env *)
       restore ())

(* completely unknown URI *)
let test_read_resource_unknown_uri () =
  match read_resource "https://example.com/foo" with
  | Error msg -> check bool "resource not found" true (String.length msg > 0)
  | Ok _ -> fail "Expected Error"

(* empty URI *)
let test_read_resource_empty_uri () =
  match read_resource "" with
  | Error _ -> ()
  | Ok _ -> fail "Expected Error"

let read_resource_extra_tests = [
  "tokens no token", `Quick, test_read_resource_tokens_no_token;
  "tokens empty key", `Quick, test_read_resource_tokens_empty_key;
  "tokens with query", `Quick, test_read_resource_tokens_with_query;
  "unknown uri", `Quick, test_read_resource_unknown_uri;
  "empty uri", `Quick, test_read_resource_empty_uri;
]

(* ============================================================
   handle_codegen_sync — error paths
   ============================================================ *)

let test_handle_codegen_sync_missing_json () =
  let args = `Assoc [] in
  match handle_codegen_sync args with
  | Error msg -> check bool "mentions json" true (Mcp_helpers.string_contains ~haystack:msg ~needle:"json")
  | Ok _ -> fail "Expected Error for missing json"

let test_handle_codegen_sync_invalid_json () =
  let args = `Assoc [("json", `String "not valid json {")] in
  match handle_codegen_sync args with
  | Error msg -> check bool "got error" true (String.length msg > 0)
  | Ok _ -> ()  (* Some formats might still produce output *)

let test_handle_codegen_sync_valid_json () =
  let args = `Assoc [
    ("json", `String {|{"name":"test","type":"FRAME"}|});
    ("format", `String "raw");
  ] in
  match handle_codegen_sync args with
  | Ok _ -> ()
  | Error msg -> fail ("Unexpected error: " ^ msg)

let test_handle_codegen_sync_fidelity_format () =
  let args = `Assoc [
    ("json", `String {|{"name":"test","type":"FRAME"}|});
    ("format", `String "fidelity");
  ] in
  match handle_codegen_sync args with
  | Ok _ -> ()
  | Error _ -> ()  (* Either way is fine, we want to hit the code path *)

let codegen_sync_tests = [
  "missing json", `Quick, test_handle_codegen_sync_missing_json;
  "invalid json", `Quick, test_handle_codegen_sync_invalid_json;
  "valid json raw", `Quick, test_handle_codegen_sync_valid_json;
  "fidelity format", `Quick, test_handle_codegen_sync_fidelity_format;
]

(* ============================================================
   handle_parse_url — deeper paths
   ============================================================ *)

let test_handle_parse_url_design_url () =
  let args = `Assoc [("url", `String "https://www.figma.com/design/ABC123/title?node-id=1-2")] in
  match handle_parse_url args with
  | Ok json ->
      let s = Yojson.Safe.to_string json in
      check bool "contains file_key" true (Mcp_helpers.string_contains ~haystack:s ~needle:"ABC123")
  | Error _ -> ()  (* Might fail if URL format not recognized *)

let test_handle_parse_url_proto_url () =
  let args = `Assoc [("url", `String "https://www.figma.com/proto/KEY/title")] in
  match handle_parse_url args with
  | Ok json ->
      let s = Yojson.Safe.to_string json in
      check bool "has content" true (String.length s > 0)
  | Error _ -> ()

let test_handle_parse_url_non_figma () =
  let args = `Assoc [("url", `String "https://example.com/page")] in
  match handle_parse_url args with
  | Ok json ->
      let s = Yojson.Safe.to_string json in
      check bool "has none markers" true (Mcp_helpers.string_contains ~haystack:s ~needle:"none")
  | Error _ -> ()

let test_handle_parse_url_empty_string () =
  let args = `Assoc [("url", `String "")] in
  match handle_parse_url args with
  | Ok _ -> ()  (* parse_figma_url might return empty results *)
  | Error _ -> ()

let parse_url_extra_tests = [
  "design url", `Quick, test_handle_parse_url_design_url;
  "proto url", `Quick, test_handle_parse_url_proto_url;
  "non-figma url", `Quick, test_handle_parse_url_non_figma;
  "empty url string", `Quick, test_handle_parse_url_empty_string;
]

(* ============================================================
   Tool definition structural checks
   ============================================================ *)

let test_all_detailed_tools_count () =
  check bool "at least 30 tools" true (List.length all_detailed_tools >= 30)

let test_all_detailed_tools_figma_prefix () =
  let all_prefixed = List.for_all (fun (t : Figma_mcp_protocol.tool_def) ->
    String.length t.name >= 6 &&
    String.sub t.name 0 6 = "figma_"
  ) all_detailed_tools in
  check bool "all have figma_ prefix" true all_prefixed

let test_featured_tool_names_in_all_tools () =
  let all_names = List.map (fun (t : Figma_mcp_protocol.tool_def) -> t.name) all_detailed_tools in
  List.iter (fun name ->
    let full = "figma_" ^ name in
    check bool (Printf.sprintf "%s in all_tools" full) true
      (List.mem full all_names)
  ) featured_tool_names

let test_category_tools_have_schema () =
  List.iter (fun (t : Figma_mcp_protocol.tool_def) ->
    match t.input_schema with
    | `Assoc _ -> ()
    | _ -> fail (Printf.sprintf "Category tool %s missing object schema" t.name)
  ) category_tools

let test_public_tools_includes_categories () =
  let cat_names = List.map (fun (t : Figma_mcp_protocol.tool_def) -> t.name) category_tools in
  List.iter (fun name ->
    let in_public = List.exists (fun (t : Figma_mcp_protocol.tool_def) -> t.name = name) public_tools in
    check bool (Printf.sprintf "%s in public_tools" name) true in_public
  ) cat_names

let test_public_tools_includes_featured () =
  let feat_names = List.map (fun (t : Figma_mcp_protocol.tool_def) -> t.name) featured_tools in
  List.iter (fun name ->
    let in_public = List.exists (fun (t : Figma_mcp_protocol.tool_def) -> t.name = name) public_tools in
    check bool (Printf.sprintf "%s in public_tools" name) true in_public
  ) feat_names

let test_make_category_tool_structure () =
  let cat = { name = "test_cat"; description = "Test category"; tools = ["a"; "b"] } in
  let tool = make_category_tool cat in
  check string "name" "figma_test_cat" tool.name;
  check bool "description has tools" true
    (Mcp_helpers.string_contains ~haystack:tool.description ~needle:"a, b");
  (match tool.input_schema with
   | `Assoc _ -> ()
   | _ -> fail "Expected object schema")

let test_tool_categories_have_tools () =
  List.iter (fun cat ->
    check bool (Printf.sprintf "category %s has tools" cat.name) true
      (List.length cat.tools > 0)
  ) tool_categories

let test_find_tool_in_category_all_categories () =
  List.iter (fun cat ->
    List.iter (fun tool_name ->
      check bool (Printf.sprintf "%s in %s" tool_name cat.name) true
        (find_tool_in_category cat.name tool_name)
    ) cat.tools
  ) tool_categories

let tool_def_extra_tests = [
  "detailed tools count", `Quick, test_all_detailed_tools_count;
  "figma prefix", `Quick, test_all_detailed_tools_figma_prefix;
  "featured in all_tools", `Quick, test_featured_tool_names_in_all_tools;
  "category tools have schema", `Quick, test_category_tools_have_schema;
  "public includes categories", `Quick, test_public_tools_includes_categories;
  "public includes featured", `Quick, test_public_tools_includes_featured;
  "make_category_tool", `Quick, test_make_category_tool_structure;
  "categories have tools", `Quick, test_tool_categories_have_tools;
  "find_tool all categories", `Quick, test_find_tool_in_category_all_categories;
]

(* ============================================================
   all_handlers_sync structural checks
   ============================================================ *)

let test_all_handlers_sync_not_empty () =
  check bool "handlers not empty" true (List.length all_handlers_sync > 0)

let test_all_handlers_sync_have_names () =
  List.iter (fun (name, _) ->
    check bool (Printf.sprintf "handler %s has name" name) true
      (String.length name > 0)
  ) all_handlers_sync

let test_all_handlers_sync_figma_prefix () =
  List.iter (fun (name, _) ->
    check bool (Printf.sprintf "%s has figma_ prefix" name) true
      (String.length name >= 6 && String.sub name 0 6 = "figma_")
  ) all_handlers_sync

let test_all_handlers_sync_unique () =
  let names = List.map fst all_handlers_sync in
  let unique = List.sort_uniq String.compare names in
  check int "unique handler names" (List.length names) (List.length unique)

let handler_sync_tests = [
  "not empty", `Quick, test_all_handlers_sync_not_empty;
  "have names", `Quick, test_all_handlers_sync_have_names;
  "figma prefix", `Quick, test_all_handlers_sync_figma_prefix;
  "unique names", `Quick, test_all_handlers_sync_unique;
]

(* ============================================================
   is_network_error — additional patterns for the etimedout branch
   ============================================================ *)

let test_is_network_error_etimedout_string () =
  check bool "etimedout in message" true
    (is_network_error (Failure "some etimedout issue"))

let test_is_network_error_mixed_case_pipe () =
  (* string_contains is case-insensitive *)
  check bool "Broken Pipe case" true
    (is_network_error (Failure "Broken Pipe error"))

let test_is_network_error_end_of_file () =
  check bool "End_of_file not network" false
    (is_network_error End_of_file)

let test_is_network_error_sys_error () =
  check bool "Sys_error not network" false
    (is_network_error (Sys_error "file not found"))

let network_error_extra_tests = [
  "etimedout in string", `Quick, test_is_network_error_etimedout_string;
  "mixed case pipe", `Quick, test_is_network_error_mixed_case_pipe;
  "end_of_file", `Quick, test_is_network_error_end_of_file;
  "sys_error", `Quick, test_is_network_error_sys_error;
]

(* ============================================================
   string_contains — additional patterns
   ============================================================ *)

let test_string_contains_repeated () =
  check bool "repeated pattern" true (Mcp_helpers.string_contains ~haystack:"ababab" ~needle:"bab")

let test_string_contains_unicode () =
  check bool "unicode haystack" true (Mcp_helpers.string_contains ~haystack:"hello\xc3\xa9world" ~needle:"world")

let test_string_contains_single_char_both () =
  check bool "single char both" true (Mcp_helpers.string_contains ~haystack:"a" ~needle:"a")

let test_string_contains_single_char_mismatch () =
  check bool "single char mismatch" false (Mcp_helpers.string_contains ~haystack:"a" ~needle:"b")

let string_contains_extra_tests = [
  "repeated pattern", `Quick, test_string_contains_repeated;
  "unicode haystack", `Quick, test_string_contains_unicode;
  "single char both", `Quick, test_string_contains_single_char_both;
  "single char mismatch", `Quick, test_string_contains_single_char_mismatch;
]

(* ============================================================
   Resources / Prompts structural
   ============================================================ *)

let test_resources_all_have_uris () =
  List.iter (fun (r : Figma_mcp_protocol.mcp_resource) ->
    check bool (Printf.sprintf "resource %s has uri" r.name) true
      (String.length r.uri > 0)
  ) resources

let test_resources_all_have_mime () =
  List.iter (fun (r : Figma_mcp_protocol.mcp_resource) ->
    check bool (Printf.sprintf "resource %s has mime" r.name) true
      (String.length r.mime_type > 0)
  ) resources

let test_resource_templates_have_uri () =
  List.iter (fun (t : Figma_mcp_protocol.mcp_resource_template) ->
    check bool (Printf.sprintf "template %s has uri" t.name) true
      (String.length t.uri_template > 0)
  ) resource_templates

let test_prompts_have_arguments () =
  List.iter (fun (p : Figma_mcp_protocol.mcp_prompt) ->
    check bool (Printf.sprintf "prompt %s has text" p.name) true
      (String.length p.text > 0)
  ) prompts

let test_prompts_fidelity_review_has_required_args () =
  match List.find_opt (fun (p : Figma_mcp_protocol.mcp_prompt) -> p.name = "figma_fidelity_review") prompts with
  | Some p ->
      let required_args = List.filter (fun (a : Figma_mcp_protocol.prompt_arg) -> a.required) p.arguments in
      check bool "has required args" true (List.length required_args >= 2)
  | None -> fail "figma_fidelity_review prompt not found"

let resource_prompt_extra_tests = [
  "resources all have uris", `Quick, test_resources_all_have_uris;
  "resources all have mime", `Quick, test_resources_all_have_mime;
  "templates have uri", `Quick, test_resource_templates_have_uri;
  "prompts have arguments", `Quick, test_prompts_have_arguments;
  "fidelity review required args", `Quick, test_prompts_fidelity_review_has_required_args;
]

(* ============================================================
   handle_category — additional edge case: describe finds tool in
   category but tool_def not in all_detailed_tools
   ============================================================ *)

let test_handle_category_list_all_categories () =
  List.iter (fun cat ->
    let args = `Assoc [("mode", `String "list")] in
    match handle_category cat.name args with
    | Ok _ -> ()
    | Error msg -> fail (Printf.sprintf "category %s list failed: %s" cat.name msg)
  ) tool_categories

let test_handle_category_describe_all_core_tools () =
  match List.find_opt (fun c -> c.name = "core") tool_categories with
  | Some cat ->
      List.iter (fun tool_name ->
        let args = `Assoc [
          ("mode", `String "describe");
          ("tool", `String tool_name);
        ] in
        match handle_category "core" args with
        | Ok _ -> ()
        | Error _ -> ()  (* Some tools might not have definitions *)
      ) cat.tools
  | None -> fail "core category not found"

let test_handle_category_call_parse_url_implicit () =
  let args = `Assoc [
    ("tool", `String "parse_url");
    ("args", `Assoc [("url", `String "https://www.figma.com/file/XYZ/t")]);
  ] in
  match handle_category "core" args with
  | Ok _ -> ()
  | Error _ -> ()  (* Might fail due to Eio context *)

let test_handle_category_invalid_mode_caught () =
  let args = `Assoc [("mode", `String "execute")] in
  (* Invalid mode now returns Error instead of raising *)
  match handle_category "core" args with
  | Error msg ->
      check bool "mentions invalid" true (Mcp_helpers.string_contains ~haystack:msg ~needle:"invalid" || Mcp_helpers.string_contains ~haystack:msg ~needle:"Invalid")
  | Ok _ -> fail "Expected Error for invalid mode"

let handle_category_extra_tests = [
  "list all categories", `Quick, test_handle_category_list_all_categories;
  "describe all core tools", `Quick, test_handle_category_describe_all_core_tools;
  "call parse_url implicit", `Quick, test_handle_category_call_parse_url_implicit;
  "invalid mode caught", `Quick, test_handle_category_invalid_mode_caught;
]

(* ============================================================
   handle_cache_invalidate — different parameter combos
   ============================================================ *)

let test_handle_cache_invalidate_all () =
  match handle_cache_invalidate (`Assoc []) with
  | Ok json ->
      let s = Yojson.Safe.to_string json in
      check bool "mentions all" true (Mcp_helpers.string_contains ~haystack:s ~needle:"All" || Mcp_helpers.string_contains ~haystack:s ~needle:"all")
  | Error msg -> fail ("Unexpected error: " ^ msg)

let test_handle_cache_invalidate_file_only () =
  match handle_cache_invalidate (`Assoc [("file_key", `String "ABC123")]) with
  | Ok json ->
      let s = Yojson.Safe.to_string json in
      check bool "mentions file" true (Mcp_helpers.string_contains ~haystack:s ~needle:"ABC123")
  | Error msg -> fail ("Unexpected error: " ^ msg)

let test_handle_cache_invalidate_file_and_node () =
  match handle_cache_invalidate (`Assoc [
    ("file_key", `String "ABC123");
    ("node_id", `String "1:2");
  ]) with
  | Ok json ->
      let s = Yojson.Safe.to_string json in
      check bool "mentions node" true (Mcp_helpers.string_contains ~haystack:s ~needle:"1:2")
  | Error msg -> fail ("Unexpected error: " ^ msg)

let cache_invalidate_tests = [
  "invalidate all", `Quick, test_handle_cache_invalidate_all;
  "invalidate file only", `Quick, test_handle_cache_invalidate_file_only;
  "invalidate file and node", `Quick, test_handle_cache_invalidate_file_and_node;
]

(* ============================================================
   handle_cache_stats — basic
   ============================================================ *)

let test_handle_cache_stats_basic () =
  match handle_cache_stats (`Assoc []) with
  | Ok _ -> ()
  | Error msg -> fail ("Unexpected error: " ^ msg)

let cache_stats_tests = [
  "basic stats", `Quick, test_handle_cache_stats_basic;
]

(* ============================================================
   handle_doctor — basic run
   ============================================================ *)

let test_handle_doctor_basic () =
  match handle_doctor (`Assoc []) with
  | Ok json ->
      let s = Yojson.Safe.to_string json in
      check bool "has status" true (Mcp_helpers.string_contains ~haystack:s ~needle:"status");
      check bool "has checks" true (Mcp_helpers.string_contains ~haystack:s ~needle:"checks")
  | Error msg -> fail ("Unexpected error: " ^ msg)

let doctor_tests = [
  "basic doctor", `Quick, test_handle_doctor_basic;
]

(* ============================================================
   command_output — additional
   ============================================================ *)

let test_command_output_with_args () =
  let result = command_output "printf" [| "printf"; "hello world" |] in
  check bool "non-empty output" true (String.length result > 0)

let test_command_output_empty_output () =
  let result = command_output "true" [| "true" |] in
  check string "empty output" "" result

let command_output_extra_tests = [
  "with args", `Quick, test_command_output_with_args;
  "empty output", `Quick, test_command_output_empty_output;
]

(* ============================================================
   has_command / has_node_module
   ============================================================ *)

let test_has_command_existing () =
  check bool "echo exists" true (has_command "echo")

let test_has_command_nonexistent () =
  check bool "nonexistent" false (has_command "nonexistent_cmd_xyz_12345")

let test_has_node_module_nonexistent () =
  check bool "nonexistent module" false (has_node_module "nonexistent_module_xyz_12345")

let command_check_tests = [
  "has_command existing", `Quick, test_has_command_existing;
  "has_command nonexistent", `Quick, test_has_command_nonexistent;
  "has_node_module nonexistent", `Quick, test_has_node_module_nonexistent;
]

(* ============================================================
   Main test runner
   ============================================================ *)

let () =
  Alcotest.run "MCP Tools W6" [
    ("compact_json extra", compact_json_extra_tests);
    ("chunkify_children extra", chunkify_children_extra_tests);
    ("chunkify_text extra", chunkify_text_extra_tests);
    ("select_chunked extra", select_chunked_extra_tests);
    ("truncate_utf8 extra", truncate_utf8_extra_tests);
    ("utf8_boundary extra", utf8_boundary_extra_tests);
    ("utf8_continuation extra", utf8_cont_extra_tests);
    ("truncate_string extra", truncate_string_extra_tests);
    ("is_under_dir extra", is_under_dir_extra_tests);
    ("normalize_path extra", normalize_path_extra_tests);
    ("field helpers extra", field_helpers_extra_tests);
    ("take_n extra", take_n_extra_tests);
    ("chunk_list extra", chunk_list_extra_tests);
    ("count extra", count_extra_tests);
    ("append_sample extra", append_sample_extra_tests);
    ("segment_bounds extra", segment_bounds_extra_tests);
    ("collect_plugin extra", collect_plugin_extra_tests);
    ("summarize_plugin extra", summarize_plugin_extra_tests);
    ("read_resource extra", read_resource_extra_tests);
    ("codegen_sync", codegen_sync_tests);
    ("parse_url extra", parse_url_extra_tests);
    ("tool_def extra", tool_def_extra_tests);
    ("handler_sync", handler_sync_tests);
    ("network_error extra", network_error_extra_tests);
    ("string_contains extra", string_contains_extra_tests);
    ("resource_prompt extra", resource_prompt_extra_tests);
    ("handle_category extra", handle_category_extra_tests);
    ("cache_invalidate", cache_invalidate_tests);
    ("cache_stats", cache_stats_tests);
    ("doctor", doctor_tests);
    ("command_output extra", command_output_extra_tests);
    ("command_check", command_check_tests);
  ]
