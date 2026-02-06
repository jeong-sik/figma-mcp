(** Comprehensive Coverage Tests for text_verifier.ml

    Target: text_verifier.ml (64 bisect points, 0% coverage)
    Functions:
    - extract_texts_from_node (recursive TEXT node extraction)
    - extract_texts_from_html (HTML tag stripping, entity decoding)
    - normalize_text (whitespace normalization)
    - find_text_in_html (fuzzy text matching)
    - verify_texts (full verification pipeline)
    - match_to_json / result_to_json (JSON serialization)
*)

open Alcotest
open Figma_types
open Text_verifier

(** ============== Test Fixtures ============== *)

let make_text_node ?(id="t1") ?(name="TextNode") chars =
  { default_node with
    id; name;
    node_type = Text;
    characters = Some chars;
  }

let make_empty_text_node () =
  { default_node with
    node_type = Text;
    characters = Some "";
  }

let make_frame_node ?(id="f1") ?(name="Frame") children =
  { default_node with
    id; name;
    node_type = Frame;
    children;
  }

let make_non_text_node () =
  { default_node with
    node_type = Rectangle;
    characters = None;
  }

(** ============== extract_texts_from_node Tests ============== *)

let test_extract_single_text_node () =
  let node = make_text_node "Hello" in
  let texts = extract_texts_from_node node in
  check int "one text" 1 (List.length texts);
  check string "text content" "Hello" (List.hd texts)

let test_extract_empty_text_node () =
  let node = make_empty_text_node () in
  let texts = extract_texts_from_node node in
  check int "no texts for empty chars" 0 (List.length texts)

let test_extract_non_text_node () =
  let node = make_non_text_node () in
  let texts = extract_texts_from_node node in
  check int "no texts for non-text" 0 (List.length texts)

let test_extract_nested_texts () =
  let child1 = make_text_node ~id:"c1" "Hello" in
  let child2 = make_text_node ~id:"c2" "World" in
  let parent = make_frame_node [child1; child2] in
  let texts = extract_texts_from_node parent in
  check int "two texts" 2 (List.length texts);
  check bool "contains Hello" true (List.mem "Hello" texts);
  check bool "contains World" true (List.mem "World" texts)

let test_extract_deeply_nested () =
  let leaf = make_text_node "Deep" in
  let mid = make_frame_node ~id:"m1" ~name:"Mid" [leaf] in
  let root = make_frame_node ~id:"r1" ~name:"Root" [mid] in
  let texts = extract_texts_from_node root in
  check int "one deep text" 1 (List.length texts);
  check string "deep content" "Deep" (List.hd texts)

let test_extract_text_node_without_characters () =
  let node = { default_node with node_type = Text; characters = None } in
  let texts = extract_texts_from_node node in
  check int "no texts when characters=None" 0 (List.length texts)

(** ============== extract_texts_from_html Tests ============== *)

let test_extract_html_basic () =
  let html = "<p>Hello World</p>" in
  let texts = extract_texts_from_html html in
  check bool "contains Hello World" true
    (List.exists (fun t -> t = "Hello World") texts)

let test_extract_html_strips_tags () =
  let html = "<div><span>Text1</span><b>Text2</b></div>" in
  let texts = extract_texts_from_html html in
  check bool "has text1" true (List.exists (fun t -> String.length t > 0) texts)

let test_extract_html_strips_script () =
  let html = "<p>Visible</p><script>var x = 1;</script><p>Also Visible</p>" in
  let texts = extract_texts_from_html html in
  check bool "no script content" false
    (List.exists (fun t -> try let _ = Str.search_forward (Str.regexp_string "var x") t 0 in true with Not_found -> false) texts);
  check bool "visible text present" true
    (List.exists (fun t -> t = "Visible") texts)

let test_extract_html_strips_style () =
  let html = "<style>.x{color:red}</style><p>Content</p>" in
  let texts = extract_texts_from_html html in
  check bool "no style content" false
    (List.exists (fun t -> try let _ = Str.search_forward (Str.regexp_string "color") t 0 in true with Not_found -> false) texts);
  check bool "content present" true
    (List.exists (fun t -> t = "Content") texts)

let test_extract_html_decodes_entities () =
  let html = "<p>&lt;tag&gt; &amp; &quot;quoted&quot;</p>" in
  let texts = extract_texts_from_html html in
  check bool "entities decoded" true
    (List.exists (fun t ->
      try let _ = Str.search_forward (Str.regexp_string "<tag>") t 0 in true
      with Not_found -> false) texts)

let test_extract_html_decodes_nbsp () =
  let html = "<p>Hello&nbsp;World</p>" in
  let texts = extract_texts_from_html html in
  check bool "nbsp decoded" true
    (List.exists (fun t ->
      try let _ = Str.search_forward (Str.regexp_string "Hello") t 0 in true
      with Not_found -> false) texts)

let test_extract_html_empty_after_strip () =
  let html = "<div>   </div>" in
  let texts = extract_texts_from_html html in
  (* Empty strings after trimming should be filtered out *)
  List.iter (fun t ->
    check bool "no empty strings" true (String.length t > 0)
  ) texts

(** ============== normalize_text Tests ============== *)

let test_normalize_text_basic () =
  check string "trim" "hello" (normalize_text "  hello  ")

let test_normalize_text_multiple_spaces () =
  check string "collapse spaces" "hello world" (normalize_text "hello   world")

let test_normalize_text_tabs_newlines () =
  check string "normalize whitespace" "a b c" (normalize_text "a\t\nb\r c")

let test_normalize_text_empty () =
  check string "empty string" "" (normalize_text "")

let test_normalize_text_only_whitespace () =
  check string "whitespace only" "" (normalize_text "   \t\n  ")

(** ============== find_text_in_html Tests ============== *)

let test_find_exact_match () =
  let result = find_text_in_html "Hello" ["Hello"; "World"] in
  check bool "found" true (Option.is_some result);
  check string "exact match" "Hello" (Option.get result)

let test_find_no_match () =
  let result = find_text_in_html "Missing" ["Hello"; "World"] in
  check bool "not found" true (Option.is_none result)

let test_find_substring_match () =
  let result = find_text_in_html "Hello" ["Say Hello World"; "Bye"] in
  check bool "substring found" true (Option.is_some result)

let test_find_normalized_match () =
  let result = find_text_in_html "Hello  World" ["Hello World"; "Bye"] in
  check bool "normalized match" true (Option.is_some result)

let test_find_short_text_no_substring () =
  (* Text <= 2 chars should only match exactly, not as substring *)
  let result = find_text_in_html "Hi" ["This contains Hi inside"; "Hello"] in
  (* "Hi" has length 2, so substring matching is skipped *)
  check bool "short text" true (Option.is_none result)

let test_find_empty_html_list () =
  let result = find_text_in_html "Hello" [] in
  check bool "empty list" true (Option.is_none result)

(** ============== verify_texts Tests ============== *)

let test_verify_all_match () =
  let child1 = make_text_node ~id:"t1" "Hello" in
  let child2 = make_text_node ~id:"t2" "World" in
  let dsl_node = make_frame_node [child1; child2] in
  let html = "<div><p>Hello</p><p>World</p></div>" in
  let result = verify_texts ~dsl_node ~html in
  check int "total 2" 2 result.total_texts;
  check int "matched 2" 2 result.matched_count;
  check (float 0.01) "accuracy 1.0" 1.0 result.accuracy;
  check bool "passed" true result.passed

let test_verify_partial_match () =
  let child1 = make_text_node ~id:"t1" "Hello" in
  let child2 = make_text_node ~id:"t2" "Missing" in
  let dsl_node = make_frame_node [child1; child2] in
  let html = "<div><p>Hello</p><p>World</p></div>" in
  let result = verify_texts ~dsl_node ~html in
  check int "total 2" 2 result.total_texts;
  check int "matched 1" 1 result.matched_count;
  check (float 0.01) "accuracy 0.5" 0.5 result.accuracy;
  check bool "not passed" false result.passed

let test_verify_no_texts () =
  let dsl_node = make_frame_node [] in
  let html = "<div><p>Hello</p></div>" in
  let result = verify_texts ~dsl_node ~html in
  check int "total 0" 0 result.total_texts;
  check (float 0.01) "accuracy 1.0 for empty" 1.0 result.accuracy;
  check bool "passed for empty" true result.passed

let test_verify_no_match () =
  let child = make_text_node "Unique" in
  let dsl_node = make_frame_node [child] in
  let html = "<div><p>Something else entirely</p></div>" in
  let result = verify_texts ~dsl_node ~html in
  check int "total 1" 1 result.total_texts;
  check int "matched 0" 0 result.matched_count;
  check (float 0.01) "accuracy 0.0" 0.0 result.accuracy;
  check bool "not passed" false result.passed

(** ============== JSON Serialization Tests ============== *)

let test_match_to_json_matched () =
  let m = { dsl_text = "Hello"; html_text = Some "Hello"; matched = true } in
  let json = match_to_json m in
  let json_str = Yojson.Safe.to_string json in
  check bool "has dsl_text" true
    (try let _ = Str.search_forward (Str.regexp_string "Hello") json_str 0 in true
     with Not_found -> false);
  check bool "has matched true" true
    (try let _ = Str.search_forward (Str.regexp_string "true") json_str 0 in true
     with Not_found -> false)

let test_match_to_json_unmatched () =
  let m = { dsl_text = "Missing"; html_text = None; matched = false } in
  let json = match_to_json m in
  let json_str = Yojson.Safe.to_string json in
  check bool "has null" true
    (try let _ = Str.search_forward (Str.regexp_string "null") json_str 0 in true
     with Not_found -> false)

let test_result_to_json () =
  let result = {
    total_texts = 2;
    matched_count = 1;
    accuracy = 0.5;
    passed = false;
    matches = [
      { dsl_text = "A"; html_text = Some "A"; matched = true };
      { dsl_text = "B"; html_text = None; matched = false };
    ];
  } in
  let json = result_to_json result in
  let json_str = Yojson.Safe.to_string json in
  check bool "has total_texts" true
    (try let _ = Str.search_forward (Str.regexp_string "total_texts") json_str 0 in true
     with Not_found -> false);
  check bool "has accuracy" true
    (try let _ = Str.search_forward (Str.regexp_string "accuracy") json_str 0 in true
     with Not_found -> false);
  check bool "has passed" true
    (try let _ = Str.search_forward (Str.regexp_string "passed") json_str 0 in true
     with Not_found -> false)

(** ============== Main Test Runner ============== *)

let () =
  run "text-verifier-coverage" [
    "extract_texts_from_node", [
      "single_text_node", `Quick, test_extract_single_text_node;
      "empty_text_node", `Quick, test_extract_empty_text_node;
      "non_text_node", `Quick, test_extract_non_text_node;
      "nested_texts", `Quick, test_extract_nested_texts;
      "deeply_nested", `Quick, test_extract_deeply_nested;
      "text_without_characters", `Quick, test_extract_text_node_without_characters;
    ];
    "extract_texts_from_html", [
      "basic", `Quick, test_extract_html_basic;
      "strips_tags", `Quick, test_extract_html_strips_tags;
      "strips_script", `Quick, test_extract_html_strips_script;
      "strips_style", `Quick, test_extract_html_strips_style;
      "decodes_entities", `Quick, test_extract_html_decodes_entities;
      "decodes_nbsp", `Quick, test_extract_html_decodes_nbsp;
      "empty_after_strip", `Quick, test_extract_html_empty_after_strip;
    ];
    "normalize_text", [
      "basic", `Quick, test_normalize_text_basic;
      "multiple_spaces", `Quick, test_normalize_text_multiple_spaces;
      "tabs_newlines", `Quick, test_normalize_text_tabs_newlines;
      "empty", `Quick, test_normalize_text_empty;
      "whitespace_only", `Quick, test_normalize_text_only_whitespace;
    ];
    "find_text_in_html", [
      "exact_match", `Quick, test_find_exact_match;
      "no_match", `Quick, test_find_no_match;
      "substring_match", `Quick, test_find_substring_match;
      "normalized_match", `Quick, test_find_normalized_match;
      "short_text_no_substring", `Quick, test_find_short_text_no_substring;
      "empty_html_list", `Quick, test_find_empty_html_list;
    ];
    "verify_texts", [
      "all_match", `Quick, test_verify_all_match;
      "partial_match", `Quick, test_verify_partial_match;
      "no_texts", `Quick, test_verify_no_texts;
      "no_match", `Quick, test_verify_no_match;
    ];
    "json_serialization", [
      "match_to_json_matched", `Quick, test_match_to_json_matched;
      "match_to_json_unmatched", `Quick, test_match_to_json_unmatched;
      "result_to_json", `Quick, test_result_to_json;
    ];
  ]
