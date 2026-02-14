(** Extra Coverage Tests for large_response.ml
    Targets: human_size, text_content, add_meta, sanitize_prefix,
    estimate_tokens, warning_level_of_count, warning_emoji,
    warning_message, analyze_node_count *)

let () =
  let open Alcotest in

  (* ============== human_size ============== *)
  let test_human_size_bytes () =
    check string "small" "500 B" (Large_response.human_size 500)
  in
  let test_human_size_zero () =
    check string "zero" "0 B" (Large_response.human_size 0)
  in
  let test_human_size_kb () =
    (* 2048 bytes = 2.0 KB *)
    check string "kb" "2.0 KB" (Large_response.human_size 2048)
  in
  let test_human_size_mb () =
    (* 2 * 1024 * 1024 = 2097152 *)
    check string "mb" "2.0 MB" (Large_response.human_size (2 * 1024 * 1024))
  in
  let test_human_size_boundary_kb () =
    (* Exactly 1024 = 1.0 KB *)
    check string "1024" "1.0 KB" (Large_response.human_size 1024)
  in
  let test_human_size_boundary_mb () =
    (* Exactly 1MB *)
    check string "1MB" "1.0 MB" (Large_response.human_size (1024 * 1024))
  in

  (* ============== text_content ============== *)
  let test_text_content () =
    let result = Large_response.text_content "hello" in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "content" fields with
         | Some (`List [`Assoc inner]) ->
             let typ = List.assoc_opt "type" inner in
             let txt = List.assoc_opt "text" inner in
             check bool "type is text" true (typ = Some (`String "text"));
             check bool "text is hello" true (txt = Some (`String "hello"))
         | _ -> fail "unexpected content structure")
    | _ -> fail "expected assoc"
  in

  (* ============== add_meta ============== *)
  let test_add_meta_assoc () =
    let base = `Assoc [("a", `Int 1)] in
    let result = Large_response.add_meta base [("b", `Int 2)] in
    (match result with
     | `Assoc fields ->
         check bool "has a" true (List.assoc_opt "a" fields = Some (`Int 1));
         check bool "has b" true (List.assoc_opt "b" fields = Some (`Int 2))
     | _ -> fail "expected assoc")
  in
  let test_add_meta_non_assoc () =
    let base = `String "raw" in
    let result = Large_response.add_meta base [("x", `Int 1)] in
    (* Non-assoc base is returned as-is *)
    check bool "unchanged" true (result = `String "raw")
  in

  (* ============== sanitize_prefix ============== *)
  let test_sanitize_prefix_clean () =
    check string "clean" "hello" (Large_response.sanitize_prefix "hello")
  in
  let test_sanitize_prefix_special () =
    check string "special chars" "a_b_c" (Large_response.sanitize_prefix "a/b:c")
  in
  let test_sanitize_prefix_empty () =
    check string "empty" "response" (Large_response.sanitize_prefix "")
  in
  let test_sanitize_prefix_long () =
    let long = String.make 100 'x' in
    let result = Large_response.sanitize_prefix long in
    check int "max 64" 64 (String.length result)
  in
  let test_sanitize_prefix_dots_hyphens () =
    check string "dots ok" "file.name-v2" (Large_response.sanitize_prefix "file.name-v2")
  in

  (* ============== estimate_tokens ============== *)
  let test_estimate_tokens_zero () =
    check int "zero" 0 (Large_response.estimate_tokens 0)
  in
  let test_estimate_tokens_normal () =
    check int "100 nodes" 5000 (Large_response.estimate_tokens 100)
  in

  (* ============== warning_level_of_count ============== *)
  let test_warning_info () =
    check bool "info" true (Large_response.warning_level_of_count 10 = Large_response.Info)
  in
  let test_warning_info_boundary () =
    check bool "info at 50" true (Large_response.warning_level_of_count 50 = Large_response.Info)
  in
  let test_warning_caution () =
    check bool "caution" true (Large_response.warning_level_of_count 100 = Large_response.Caution)
  in
  let test_warning_caution_boundary () =
    check bool "caution at 200" true (Large_response.warning_level_of_count 200 = Large_response.Caution)
  in
  let test_warning_warning () =
    check bool "warning" true (Large_response.warning_level_of_count 300 = Large_response.Warning)
  in
  let test_warning_warning_boundary () =
    check bool "warning at 500" true (Large_response.warning_level_of_count 500 = Large_response.Warning)
  in
  let test_warning_critical () =
    check bool "critical" true (Large_response.warning_level_of_count 1000 = Large_response.Critical)
  in
  let test_warning_critical_boundary () =
    check bool "critical at 501" true (Large_response.warning_level_of_count 501 = Large_response.Critical)
  in

  (* ============== warning_emoji ============== *)
  let test_emoji_info () =
    check string "info emoji" "\xe2\x9c\x85" (Large_response.warning_emoji Large_response.Info)
  in
  let test_emoji_critical () =
    check string "critical emoji" "\xf0\x9f\x9a\xa8" (Large_response.warning_emoji Large_response.Critical)
  in

  (* ============== warning_message ============== *)
  let test_warning_message_info () =
    let msg = Large_response.warning_message Large_response.Info 10 500 in
    check bool "has Small" true (String.length msg > 0 && (try let _ = Str.search_forward (Str.regexp_string "Small") msg 0 in true with Not_found -> false))
  in
  let test_warning_message_critical () =
    let msg = Large_response.warning_message Large_response.Critical 1000 50000 in
    check bool "has MUST" true (try let _ = Str.search_forward (Str.regexp_string "MUST") msg 0 in true with Not_found -> false)
  in
  let test_warning_message_caution () =
    let msg = Large_response.warning_message Large_response.Caution 100 5000 in
    check bool "has memoization" true (try let _ = Str.search_forward (Str.regexp_string "memoization") msg 0 in true with Not_found -> false)
  in
  let test_warning_message_warning () =
    let msg = Large_response.warning_message Large_response.Warning 300 15000 in
    check bool "has progressive" true (try let _ = Str.search_forward (Str.regexp_string "progressive") msg 0 in true with Not_found -> false)
  in

  (* ============== analyze_node_count ============== *)
  let test_analyze_small () =
    let a = Large_response.analyze_node_count 10 in
    check int "10 nodes" 10 a.total_nodes;
    check int "500 tokens" 500 a.estimated_tokens;
    check bool "info level" true (a.level = Large_response.Info);
    check int "no recommendations" 0 (List.length a.recommendations)
  in
  let test_analyze_large () =
    let a = Large_response.analyze_node_count 300 in
    check int "300 nodes" 300 a.total_nodes;
    check int "15000 tokens" 15000 a.estimated_tokens;
    check bool "warning level" true (a.level = Large_response.Warning);
    check bool "has recommendations" true (List.length a.recommendations > 0)
  in
  let test_analyze_critical () =
    let a = Large_response.analyze_node_count 1500 in
    check bool "critical" true (a.level = Large_response.Critical);
    check bool "has recommendations" true (List.length a.recommendations > 0)
  in
  let test_analyze_caution () =
    let a = Large_response.analyze_node_count 100 in
    check bool "caution" true (a.level = Large_response.Caution);
    check int "5000 tokens" 5000 a.estimated_tokens
  in

  (* ============== warning_emoji (caution + warning) ============== *)
  let test_emoji_caution () =
    let e = Large_response.warning_emoji Large_response.Caution in
    check bool "caution emoji" true (String.length e > 0)
  in
  let test_emoji_warning () =
    let e = Large_response.warning_emoji Large_response.Warning in
    check bool "warning emoji" true (String.length e > 0)
  in

  (* ============== count_nodes_json ============== *)
  let test_count_nodes_empty () =
    check int "null" 0 (Large_response.count_nodes_json `Null)
  in
  let test_count_nodes_single () =
    let json = `Assoc [("type", `String "FRAME")] in
    check int "single node" 1 (Large_response.count_nodes_json json)
  in
  let test_count_nodes_with_children () =
    let json = `Assoc [
      ("type", `String "FRAME");
      ("children", `List [
        `Assoc [("type", `String "TEXT")];
        `Assoc [("type", `String "RECT")];
      ])
    ] in
    check int "parent + 2 children" 3 (Large_response.count_nodes_json json)
  in
  let test_count_nodes_nested () =
    let json = `Assoc [
      ("type", `String "FRAME");
      ("children", `List [
        `Assoc [
          ("type", `String "GROUP");
          ("children", `List [
            `Assoc [("type", `String "TEXT")];
          ])
        ];
      ])
    ] in
    check int "3 deep" 3 (Large_response.count_nodes_json json)
  in
  let test_count_nodes_list () =
    let json = `List [
      `Assoc [("type", `String "A")];
      `Assoc [("type", `String "B")];
    ] in
    check int "list of 2" 2 (Large_response.count_nodes_json json)
  in
  let test_count_nodes_string () =
    check int "string" 0 (Large_response.count_nodes_json (`String "hi"))
  in
  let test_count_nodes_no_children_key () =
    let json = `Assoc [("type", `String "LEAF"); ("name", `String "x")] in
    check int "no children" 1 (Large_response.count_nodes_json json)
  in
  let test_count_nodes_children_not_list () =
    let json = `Assoc [("type", `String "LEAF"); ("children", `String "bad")] in
    check int "children not list" 1 (Large_response.count_nodes_json json)
  in

  (* ============== analysis_to_json ============== *)
  let test_analysis_to_json_info () =
    let a = Large_response.analyze_node_count 10 in
    let json = Large_response.analysis_to_json a in
    match json with
    | `Assoc fields ->
        check bool "has total_nodes" true (List.assoc_opt "total_nodes" fields = Some (`Int 10));
        check bool "has estimated_tokens" true (List.assoc_opt "estimated_tokens" fields = Some (`Int 500));
        check bool "has warning_level" true (List.assoc_opt "warning_level" fields = Some (`String "info"));
        check bool "has message" true (List.assoc_opt "message" fields <> None);
        (match List.assoc_opt "recommendations" fields with
         | Some (`List []) -> ()
         | _ -> fail "expected empty recommendations for info level")
    | _ -> fail "expected assoc"
  in
  let test_analysis_to_json_caution () =
    let a = Large_response.analyze_node_count 100 in
    let json = Large_response.analysis_to_json a in
    match json with
    | `Assoc fields ->
        check bool "caution level" true (List.assoc_opt "warning_level" fields = Some (`String "caution"))
    | _ -> fail "expected assoc"
  in
  let test_analysis_to_json_warning () =
    let a = Large_response.analyze_node_count 300 in
    let json = Large_response.analysis_to_json a in
    match json with
    | `Assoc fields ->
        check bool "warning level" true (List.assoc_opt "warning_level" fields = Some (`String "warning"))
    | _ -> fail "expected assoc"
  in
  let test_analysis_to_json_critical () =
    let a = Large_response.analyze_node_count 1000 in
    let json = Large_response.analysis_to_json a in
    match json with
    | `Assoc fields ->
        check bool "critical level" true (List.assoc_opt "warning_level" fields = Some (`String "critical"))
    | _ -> fail "expected assoc"
  in

  (* ============== add_warning_to_response ============== *)
  let test_add_warning_info_no_change () =
    let resp = `Assoc [("data", `String "x")] in
    let result = Large_response.add_warning_to_response ~node_count:10 resp in
    check bool "info level unchanged" true (result = resp)
  in
  let test_add_warning_caution_adds_field () =
    let resp = `Assoc [("data", `String "x")] in
    let result = Large_response.add_warning_to_response ~node_count:100 resp in
    match result with
    | `Assoc fields ->
        check bool "has _warning" true (List.assoc_opt "_warning" fields <> None)
    | _ -> fail "expected assoc"
  in
  let test_add_warning_non_assoc () =
    let resp = `String "raw" in
    let result = Large_response.add_warning_to_response ~node_count:1000 resp in
    (* Non-assoc just returned as-is even for critical *)
    check bool "non-assoc unchanged" true (result = `String "raw")
  in

  (* ============== generate_filename ============== *)
  let test_generate_filename_format () =
    let name = Large_response.generate_filename ~prefix:"test_prefix" in
    check bool "ends with .json" true (Filename.check_suffix name ".json");
    check bool "starts with prefix" true
      (try let _ = Str.search_forward (Str.regexp_string "test_prefix") name 0 in true
       with Not_found -> false)
  in
  let test_generate_filename_sanitized () =
    let name = Large_response.generate_filename ~prefix:"a/b:c" in
    check bool "no slashes" true
      (not (String.contains name '/'));
    check bool "ends with .json" true (Filename.check_suffix name ".json")
  in

  (* ============== handle_response: small inline JSON ============== *)
  let test_handle_response_small_json () =
    let content = {|{"key": "value"}|} in
    let result = Large_response.handle_response ~prefix:"test" ~format:"raw" content in
    match result with
    | `Assoc fields ->
        check bool "has key" true (List.assoc_opt "key" fields = Some (`String "value"))
    | _ -> fail "expected parsed json assoc"
  in

  (* ============== handle_response: small inline invalid JSON ============== *)
  let test_handle_response_small_invalid_json () =
    let content = "not-json" in
    let result = Large_response.handle_response ~prefix:"test" ~format:"raw" content in
    match result with
    | `String s -> check string "wrapped as string" "not-json" s
    | _ -> fail "expected string wrapping"
  in

  (* ============== handle_response: large result ============== *)
  let test_handle_response_large () =
    (* Create content larger than max_inline_size (50000) *)
    let content = String.make 60000 'x' in
    let result = Large_response.handle_response ~prefix:"large_test" ~format:"fidelity" content in
    match result with
    | `Assoc fields ->
        check bool "has status" true (List.assoc_opt "status" fields = Some (`String "large_result"));
        check bool "has file_path" true (List.assoc_opt "file_path" fields <> None);
        check bool "has size_bytes" true (List.assoc_opt "size_bytes" fields = Some (`Int 60000));
        check bool "has size_human" true (List.assoc_opt "size_human" fields <> None);
        check bool "has format" true (List.assoc_opt "format" fields = Some (`String "fidelity"));
        check bool "has preview" true (List.assoc_opt "preview" fields <> None);
        check bool "has hint" true (List.assoc_opt "hint" fields <> None);
        check bool "has next_chunk" true (List.assoc_opt "next_chunk" fields <> None);
        check bool "has examples" true (List.assoc_opt "examples" fields <> None);
        (* Clean up the file *)
        (match List.assoc_opt "file_path" fields with
         | Some (`String path) ->
             (try Unix.unlink path with Unix.Unix_error _ -> ())
         | _ -> ())
    | _ -> fail "expected assoc for large result"
  in

  (* ============== wrap_json_result (delegates to handle_response) ============== *)
  let test_wrap_json_result_small () =
    let json = `Assoc [("x", `Int 1)] in
    let result = Large_response.wrap_json_result ~prefix:"wjr" ~format:"raw" json in
    match result with
    | `Assoc fields ->
        check bool "has x" true (List.assoc_opt "x" fields = Some (`Int 1))
    | _ -> fail "expected assoc"
  in

  (* ============== wrap_string_result ============== *)
  let test_wrap_string_result_small () =
    let result = Large_response.wrap_string_result ~prefix:"wsr" ~format:"html" "hello world" in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "content" fields with
         | Some (`List [`Assoc inner]) ->
             check bool "text content" true (List.assoc_opt "text" inner = Some (`String "hello world"))
         | _ -> fail "expected content list")
    | _ -> fail "expected assoc"
  in
  let test_wrap_string_result_large () =
    let big = String.make 60000 'y' in
    let result = Large_response.wrap_string_result ~prefix:"wsr_large" ~format:"fidelity" big in
    match result with
    | `Assoc fields ->
        check bool "has status" true (List.assoc_opt "status" fields = Some (`String "large_result"));
        check bool "has file_path" true (List.assoc_opt "file_path" fields <> None);
        check bool "has preview_bytes" true (List.assoc_opt "preview_bytes" fields <> None);
        check bool "has next_chunk" true (List.assoc_opt "next_chunk" fields <> None);
        check bool "has hint" true (List.assoc_opt "hint" fields <> None);
        (* Clean up *)
        (match List.assoc_opt "file_path" fields with
         | Some (`String path) -> (try Unix.unlink path with Unix.Unix_error _ -> ())
         | _ -> ())
    | _ -> fail "expected assoc for large string result"
  in

  (* ============== analyze_and_wrap_json ============== *)
  let test_analyze_wrap_small_info () =
    let json = `Assoc [("type", `String "TEXT")] in
    let result = Large_response.analyze_and_wrap_json ~prefix:"aw" ~format:"raw" json in
    (* Small + Info = json as-is *)
    match result with
    | `Assoc fields ->
        check bool "has type" true (List.assoc_opt "type" fields = Some (`String "TEXT"));
        check bool "no _warning" true (List.assoc_opt "_warning" fields = None)
    | _ -> fail "expected assoc"
  in
  let test_analyze_wrap_small_caution () =
    (* Build a tree with ~100 nodes for Caution level *)
    let children = List.init 99 (fun i ->
      `Assoc [("type", `String (Printf.sprintf "CHILD_%d" i))]
    ) in
    let json = `Assoc [("type", `String "FRAME"); ("children", `List children)] in
    let result = Large_response.analyze_and_wrap_json ~prefix:"aw_c" ~format:"raw" json in
    match result with
    | `Assoc fields ->
        check bool "has _warning" true (List.assoc_opt "_warning" fields <> None)
    | _ -> fail "expected assoc with warning"
  in
  let test_analyze_wrap_non_assoc_small () =
    let json = `String "non-assoc" in
    let result = Large_response.analyze_and_wrap_json ~prefix:"aw_na" ~format:"raw" json in
    (* Non-assoc with Info level returns as-is *)
    check bool "same string" true (result = `String "non-assoc")
  in

  (* ============== wrap_dsl_with_warning ============== *)
  let test_wrap_dsl_info_small () =
    let result = Large_response.wrap_dsl_with_warning ~prefix:"dsl" ~format:"dsl" ~node_count:10 "FRAME { TEXT }" in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "content" fields with
         | Some (`List _) -> ()
         | _ -> fail "expected content list");
        check bool "no _warning for info" true (List.assoc_opt "_warning" fields = None)
    | _ -> fail "expected assoc"
  in
  let test_wrap_dsl_caution_small () =
    let result = Large_response.wrap_dsl_with_warning ~prefix:"dsl_c" ~format:"dsl" ~node_count:100 "FRAME { TEXT }" in
    match result with
    | `Assoc fields ->
        check bool "has _warning" true (List.assoc_opt "_warning" fields <> None)
    | _ -> fail "expected assoc"
  in
  let test_wrap_dsl_large () =
    let big_dsl = String.make 60000 'z' in
    let result = Large_response.wrap_dsl_with_warning ~prefix:"dsl_l" ~format:"dsl" ~node_count:1000 big_dsl in
    match result with
    | `Assoc fields ->
        check bool "has _warning" true (List.assoc_opt "_warning" fields <> None);
        check bool "has status" true (List.assoc_opt "status" fields = Some (`String "large_result"));
        check bool "has file_path" true (List.assoc_opt "file_path" fields <> None);
        (* Clean up *)
        (match List.assoc_opt "file_path" fields with
         | Some (`String path) -> (try Unix.unlink path with Unix.Unix_error _ -> ())
         | _ -> ())
    | _ -> fail "expected assoc"
  in

  (* ============== wrap_text_with_warning ============== *)
  let test_wrap_text_info_small () =
    let result = Large_response.wrap_text_with_warning ~prefix:"tw" ~format:"text" ~node_count:10 "hello" in
    match result with
    | `Assoc fields ->
        check bool "has content" true (List.assoc_opt "content" fields <> None);
        check bool "no _warning" true (List.assoc_opt "_warning" fields = None)
    | _ -> fail "expected assoc"
  in
  let test_wrap_text_caution_small () =
    let result = Large_response.wrap_text_with_warning ~prefix:"tw_c" ~format:"text" ~node_count:100 "hello" in
    match result with
    | `Assoc fields ->
        check bool "has _warning" true (List.assoc_opt "_warning" fields <> None)
    | _ -> fail "expected assoc"
  in
  let test_wrap_text_large () =
    let big = String.make 60000 'w' in
    let result = Large_response.wrap_text_with_warning ~prefix:"tw_l" ~format:"text" ~node_count:500 big in
    match result with
    | `Assoc fields ->
        check bool "has _warning" true (List.assoc_opt "_warning" fields <> None);
        check bool "has status" true (List.assoc_opt "status" fields = Some (`String "large_result"));
        check bool "has file_path" true (List.assoc_opt "file_path" fields <> None);
        check bool "has hint" true (List.assoc_opt "hint" fields <> None);
        (* Clean up *)
        (match List.assoc_opt "file_path" fields with
         | Some (`String path) -> (try Unix.unlink path with Unix.Unix_error _ -> ())
         | _ -> ())
    | _ -> fail "expected assoc"
  in

  (* ============== node_thresholds (just ensure it's a string) ============== *)
  let test_node_thresholds () =
    check bool "non-empty" true (String.length Large_response.node_thresholds > 0)
  in

  (* ============== save_to_file + cleanup_old_files ============== *)
  let test_save_and_cleanup () =
    let filepath = Large_response.save_to_file ~prefix:"test_cleanup" "test content" in
    check bool "file exists" true (Sys.file_exists filepath);
    (* Cleanup should not remove fresh files *)
    Large_response.cleanup_old_files ();
    check bool "fresh file preserved" true (Sys.file_exists filepath);
    (* Clean up *)
    (try Unix.unlink filepath with Unix.Unix_error _ -> ())
  in

  (* ============== ensure_dir ============== *)
  let test_ensure_dir () =
    let tmp = Filename.temp_file "ensure_dir_test" "" in
    Unix.unlink tmp;
    let dir = tmp ^ "_dir" in
    Large_response.ensure_dir dir;
    check bool "dir exists" true (Sys.file_exists dir);
    Unix.rmdir dir
  in

  run "Large_response Extra" [
    ("human_size", [
      test_case "bytes" `Quick test_human_size_bytes;
      test_case "zero" `Quick test_human_size_zero;
      test_case "kb" `Quick test_human_size_kb;
      test_case "mb" `Quick test_human_size_mb;
      test_case "boundary kb" `Quick test_human_size_boundary_kb;
      test_case "boundary mb" `Quick test_human_size_boundary_mb;
    ]);
    ("text_content", [
      test_case "basic" `Quick test_text_content;
    ]);
    ("add_meta", [
      test_case "assoc" `Quick test_add_meta_assoc;
      test_case "non-assoc" `Quick test_add_meta_non_assoc;
    ]);
    ("sanitize_prefix", [
      test_case "clean" `Quick test_sanitize_prefix_clean;
      test_case "special chars" `Quick test_sanitize_prefix_special;
      test_case "empty" `Quick test_sanitize_prefix_empty;
      test_case "long" `Quick test_sanitize_prefix_long;
      test_case "dots hyphens" `Quick test_sanitize_prefix_dots_hyphens;
    ]);
    ("estimate_tokens", [
      test_case "zero" `Quick test_estimate_tokens_zero;
      test_case "normal" `Quick test_estimate_tokens_normal;
    ]);
    ("warning_level_of_count", [
      test_case "info" `Quick test_warning_info;
      test_case "info boundary" `Quick test_warning_info_boundary;
      test_case "caution" `Quick test_warning_caution;
      test_case "caution boundary" `Quick test_warning_caution_boundary;
      test_case "warning" `Quick test_warning_warning;
      test_case "warning boundary" `Quick test_warning_warning_boundary;
      test_case "critical" `Quick test_warning_critical;
      test_case "critical boundary" `Quick test_warning_critical_boundary;
    ]);
    ("warning_emoji", [
      test_case "info" `Quick test_emoji_info;
      test_case "critical" `Quick test_emoji_critical;
      test_case "caution" `Quick test_emoji_caution;
      test_case "warning" `Quick test_emoji_warning;
    ]);
    ("warning_message", [
      test_case "info" `Quick test_warning_message_info;
      test_case "critical" `Quick test_warning_message_critical;
      test_case "caution" `Quick test_warning_message_caution;
      test_case "warning" `Quick test_warning_message_warning;
    ]);
    ("analyze_node_count", [
      test_case "small" `Quick test_analyze_small;
      test_case "large" `Quick test_analyze_large;
      test_case "critical" `Quick test_analyze_critical;
      test_case "caution" `Quick test_analyze_caution;
    ]);
    ("count_nodes_json", [
      test_case "empty (null)" `Quick test_count_nodes_empty;
      test_case "single node" `Quick test_count_nodes_single;
      test_case "with children" `Quick test_count_nodes_with_children;
      test_case "nested" `Quick test_count_nodes_nested;
      test_case "list" `Quick test_count_nodes_list;
      test_case "string" `Quick test_count_nodes_string;
      test_case "no children key" `Quick test_count_nodes_no_children_key;
      test_case "children not list" `Quick test_count_nodes_children_not_list;
    ]);
    ("analysis_to_json", [
      test_case "info" `Quick test_analysis_to_json_info;
      test_case "caution" `Quick test_analysis_to_json_caution;
      test_case "warning" `Quick test_analysis_to_json_warning;
      test_case "critical" `Quick test_analysis_to_json_critical;
    ]);
    ("add_warning_to_response", [
      test_case "info no change" `Quick test_add_warning_info_no_change;
      test_case "caution adds field" `Quick test_add_warning_caution_adds_field;
      test_case "non-assoc" `Quick test_add_warning_non_assoc;
    ]);
    ("generate_filename", [
      test_case "format" `Quick test_generate_filename_format;
      test_case "sanitized" `Quick test_generate_filename_sanitized;
    ]);
    ("handle_response", [
      test_case "small json" `Quick test_handle_response_small_json;
      test_case "small invalid json" `Quick test_handle_response_small_invalid_json;
      test_case "large" `Quick test_handle_response_large;
    ]);
    ("wrap_json_result", [
      test_case "small" `Quick test_wrap_json_result_small;
    ]);
    ("wrap_string_result", [
      test_case "small" `Quick test_wrap_string_result_small;
      test_case "large" `Quick test_wrap_string_result_large;
    ]);
    ("analyze_and_wrap_json", [
      test_case "small info" `Quick test_analyze_wrap_small_info;
      test_case "small caution" `Quick test_analyze_wrap_small_caution;
      test_case "non-assoc small" `Quick test_analyze_wrap_non_assoc_small;
    ]);
    ("wrap_dsl_with_warning", [
      test_case "info small" `Quick test_wrap_dsl_info_small;
      test_case "caution small" `Quick test_wrap_dsl_caution_small;
      test_case "large" `Quick test_wrap_dsl_large;
    ]);
    ("wrap_text_with_warning", [
      test_case "info small" `Quick test_wrap_text_info_small;
      test_case "caution small" `Quick test_wrap_text_caution_small;
      test_case "large" `Quick test_wrap_text_large;
    ]);
    ("node_thresholds", [
      test_case "non-empty" `Quick test_node_thresholds;
    ]);
    ("save_and_cleanup", [
      test_case "save and cleanup" `Quick test_save_and_cleanup;
    ]);
    ("ensure_dir", [
      test_case "create" `Quick test_ensure_dir;
    ]);
  ]
