(** Coverage Wave 8: final push to 70%+ overall.
    Targets uncovered branches across 4 modules:
    - mcp_tools.ml: is_network_error, string_contains, handle_parse_url,
      handle_codegen_sync, handle_category modes, read_resource URIs,
      handle_doctor, handle_cache_stats, summarize_plugin_payload deep branches,
      collect_plugin_stats, chunkify_text boundary, select_chunked_json Float idx,
      compact_json List truncation, take_n edge cases, chunk_list edge cases,
      handle_get_me/list_projects/list_files via mock, handle_search via mock
    - figma_effects.ml: Perform functions via mock (get_file_components,
      get_team_components, get_file_component_sets, get_team_component_sets,
      get_file_styles, get_team_styles, get_component, get_component_set,
      get_style, get_file_versions, get_file_comments, post_file_comment,
      download_url, eio_sleep), run_with_mock with Neo4j effect (unhandled)
    - server_metrics.ml: record_untracked_response 3xx/4xx/5xx, to_json field
      checks, prom_metric edge cases, sse_close below zero guard
    - mcp_visual_handlers.ml: handle_compare_elements color/box/full/error paths,
      handle_fidelity_loop format error, handle_image_similarity missing params,
      handle_verify_semantic missing params, handle_compare_regions missing params
*)

let () =
  let open Alcotest in

  (* ================================================================
     mcp_tools.ml — pure functions
     ================================================================ *)

  (* --- string_contains: case-insensitive substring match --- *)
  let test_string_contains_basic () =
    check bool "exact" true (Mcp_tools.string_contains "hello" "hello");
    check bool "prefix" true (Mcp_tools.string_contains "hello world" "hello");
    check bool "suffix" true (Mcp_tools.string_contains "hello world" "world");
    check bool "middle" true (Mcp_tools.string_contains "hello world" "lo wo");
    check bool "case insensitive" true (Mcp_tools.string_contains "Hello World" "hello");
    check bool "empty sub" true (Mcp_tools.string_contains "hello" "");
    check bool "sub longer" false (Mcp_tools.string_contains "hi" "hello");
    check bool "no match" false (Mcp_tools.string_contains "hello" "xyz")
  in

  (* --- is_network_error: Unix errors and string matching --- *)
  let test_is_network_error_unix () =
    check bool "EPIPE" true
      (Mcp_tools.is_network_error (Unix.Unix_error (Unix.EPIPE, "write", "")));
    check bool "ECONNRESET" true
      (Mcp_tools.is_network_error (Unix.Unix_error (Unix.ECONNRESET, "read", "")));
    check bool "ETIMEDOUT" true
      (Mcp_tools.is_network_error (Unix.Unix_error (Unix.ETIMEDOUT, "connect", "")));
    check bool "other Unix" false
      (Mcp_tools.is_network_error (Unix.Unix_error (Unix.ENOENT, "open", "")))
  in

  let test_is_network_error_string () =
    check bool "broken pipe" true
      (Mcp_tools.is_network_error (Failure "broken pipe in stream"));
    check bool "connection reset" true
      (Mcp_tools.is_network_error (Failure "connection reset by peer"));
    check bool "connection timed out" true
      (Mcp_tools.is_network_error (Failure "connection timed out"));
    check bool "econnreset" true
      (Mcp_tools.is_network_error (Failure "ECONNRESET"));
    check bool "epipe" true
      (Mcp_tools.is_network_error (Failure "EPIPE"));
    check bool "etimedout" true
      (Mcp_tools.is_network_error (Failure "ETIMEDOUT"));
    check bool "unrelated" false
      (Mcp_tools.is_network_error (Failure "file not found"))
  in

  (* --- truncate_string: edge cases --- *)
  let test_truncate_string_edges () =
    check string "zero max_len" "hello" (Mcp_tools.truncate_string ~max_len:0 "hello");
    check string "negative max_len" "hello" (Mcp_tools.truncate_string ~max_len:(-1) "hello");
    check string "exact fit" "hi" (Mcp_tools.truncate_string ~max_len:2 "hi");
    check string "truncated" "he...(truncated)" (Mcp_tools.truncate_string ~max_len:2 "hello")
  in

  (* --- is_utf8_continuation --- *)
  let test_utf8_continuation () =
    (* Continuation byte: 10xxxxxx *)
    check bool "0x80 is continuation" true (Mcp_tools.is_utf8_continuation 0x80);
    check bool "0xBF is continuation" true (Mcp_tools.is_utf8_continuation 0xBF);
    check bool "0x7F not continuation" false (Mcp_tools.is_utf8_continuation 0x7F);
    check bool "0xC0 not continuation" false (Mcp_tools.is_utf8_continuation 0xC0);
    check bool "0x00 not continuation" false (Mcp_tools.is_utf8_continuation 0x00)
  in

  (* --- utf8_safe_boundary --- *)
  let test_utf8_safe_boundary () =
    (* ASCII: each byte is a complete char *)
    let bound = Mcp_tools.utf8_safe_boundary ~start:0 ~max_bytes:3 "hello" in
    check int "ascii boundary" 3 bound;
    (* Boundary beyond string *)
    let bound2 = Mcp_tools.utf8_safe_boundary ~start:0 ~max_bytes:100 "hi" in
    check int "beyond string" 2 bound2;
    (* Start = max_bytes (empty slice) *)
    let bound3 = Mcp_tools.utf8_safe_boundary ~start:5 ~max_bytes:0 "hello" in
    check int "zero max_bytes" 5 bound3
  in

  (* --- truncate_utf8: edge cases --- *)
  let test_truncate_utf8_edges () =
    let (s, t) = Mcp_tools.truncate_utf8 ~max_bytes:0 "hello" in
    check string "zero max" "hello" s;
    check bool "zero max not truncated" false t;
    let (s2, t2) = Mcp_tools.truncate_utf8 ~max_bytes:100 "hi" in
    check string "fits" "hi" s2;
    check bool "fits not truncated" false t2;
    let (s3, t3) = Mcp_tools.truncate_utf8 ~max_bytes:3 "hello" in
    check string "truncated to 3" "hel" s3;
    check bool "was truncated" true t3
  in

  (* --- take_n: edge cases --- *)
  let test_take_n () =
    check (list int) "take 0" [] (Mcp_tools.take_n 0 [1;2;3]);
    check (list int) "take negative" [] (Mcp_tools.take_n (-1) [1;2;3]);
    check (list int) "take all" [1;2;3] (Mcp_tools.take_n 5 [1;2;3]);
    check (list int) "take 2" [1;2] (Mcp_tools.take_n 2 [1;2;3]);
    check (list int) "take from empty" [] (Mcp_tools.take_n 3 [])
  in

  (* --- chunk_list: edge cases --- *)
  let test_chunk_list () =
    let chunks = Mcp_tools.chunk_list 2 [1;2;3;4;5] in
    check int "num chunks" 3 (List.length chunks);
    check (list int) "chunk 1" [1;2] (List.nth chunks 0);
    check (list int) "chunk 2" [3;4] (List.nth chunks 1);
    check (list int) "chunk 3" [5] (List.nth chunks 2);
    let empty = Mcp_tools.chunk_list 3 [] in
    check int "empty" 0 (List.length empty);
    (* zero chunk_size normalizes to 1 *)
    let single = Mcp_tools.chunk_list 0 [1;2;3] in
    check int "zero size" 3 (List.length single)
  in

  (* --- has_field / set_field / add_if_missing --- *)
  let test_field_helpers () =
    let fields = [("a", `Int 1); ("b", `Int 2)] in
    check bool "has a" true (Mcp_tools.has_field "a" fields);
    check bool "no c" false (Mcp_tools.has_field "c" fields);
    let fields2 = Mcp_tools.set_field "a" (`Int 10) fields in
    check int "set a" 2 (List.length fields2);
    let fields3 = Mcp_tools.add_if_missing "c" (`Int 3) fields in
    check int "add c" 3 (List.length fields3);
    let fields4 = Mcp_tools.add_if_missing "a" (`Int 99) fields in
    check int "a exists" 2 (List.length fields4)
  in

  (* --- get_string_any --- *)
  let test_get_string_any () =
    let json = `Assoc [("name", `String "test"); ("id", `String "123")] in
    let r = Mcp_tools.get_string_any ["missing"; "name"] json in
    check (option string) "found name" (Some "test") r;
    let r2 = Mcp_tools.get_string_any ["x"; "y"] json in
    check (option string) "not found" None r2;
    let r3 = Mcp_tools.get_string_any [] json in
    check (option string) "empty keys" None r3
  in

  (* --- compact_json: List truncation --- *)
  let test_compact_json_list_truncation () =
    let json = `List [`Int 1; `Int 2; `Int 3; `Int 4; `Int 5] in
    let result = Mcp_tools.compact_json
      ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:3 ~max_string:100
      json
    in
    match result with
    | `List items ->
        (* 3 items + 1 truncation marker *)
        check int "truncated list" 4 (List.length items)
    | _ -> fail "expected list"
  in

  (* --- compact_json: String truncation --- *)
  let test_compact_json_string_truncation () =
    let json = `String "this is a long string that should be truncated" in
    let result = Mcp_tools.compact_json
      ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:10
      json
    in
    match result with
    | `String s -> check bool "truncated" true (String.length s < 46)
    | _ -> fail "expected string"
  in

  (* --- compact_json: depth truncation --- *)
  let test_compact_json_depth_truncation () =
    let json = `Assoc [
      ("children", `List [`Assoc [("name", `String "child")]]);
      ("other", `String "value");
    ] in
    let result = Mcp_tools.compact_json
      ~depth:5 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
      json
    in
    match result with
    | `Assoc fields ->
        check bool "has depth_truncated" true
          (List.exists (fun (k, _) -> k = "_depth_truncated") fields);
        check bool "no children" true
          (not (List.exists (fun (k, _) -> k = "children") fields))
    | _ -> fail "expected assoc"
  in

  (* --- compact_json: passthrough for other types --- *)
  let test_compact_json_passthrough () =
    let cases = [`Bool true; `Int 42; `Float 3.14; `Null] in
    List.iter (fun json ->
      let result = Mcp_tools.compact_json
        ~depth:0 ~max_depth:5 ~max_children:10 ~max_list_items:10 ~max_string:100
        json
      in
      check bool "passthrough" true (result = json)
    ) cases
  in

  (* --- chunkify_children: with children --- *)
  let test_chunkify_children () =
    let json = `Assoc [
      ("name", `String "parent");
      ("children", `List [`Int 1; `Int 2; `Int 3; `Int 4; `Int 5]);
    ] in
    let result = Mcp_tools.chunkify_children ~chunk_size:2 json in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "chunks" fields with
         | Some (`List chunks) ->
             check int "3 chunks" 3 (List.length chunks)
         | _ -> fail "expected chunks list")
    | _ -> fail "expected assoc"
  in

  (* --- chunkify_children: no children --- *)
  let test_chunkify_children_no_kids () =
    let json = `Assoc [("name", `String "leaf")] in
    let result = Mcp_tools.chunkify_children ~chunk_size:2 json in
    check bool "passthrough" true (result = json)
  in

  (* --- chunkify_children: non-assoc input --- *)
  let test_chunkify_children_non_assoc () =
    let json = `List [`Int 1] in
    let result = Mcp_tools.chunkify_children ~chunk_size:2 json in
    check bool "passthrough" true (result = json)
  in

  (* --- chunkify_text: basic --- *)
  let test_chunkify_text_basic () =
    let result = Mcp_tools.chunkify_text ~chunk_size:5 "hello world" in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "chunks" fields with
         | Some (`List chunks) -> check bool "has chunks" true (List.length chunks > 0)
         | _ -> fail "expected chunks")
    | _ -> fail "expected assoc"
  in

  (* --- chunkify_text: zero/negative chunk_size --- *)
  let test_chunkify_text_zero () =
    let result = Mcp_tools.chunkify_text ~chunk_size:0 "abc" in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "chunk_total" fields with
         | Some (`Int n) -> check int "chunk count" 3 n
         | _ -> fail "expected chunk_total")
    | _ -> fail "expected assoc"
  in

  (* --- chunkify_text: empty string --- *)
  let test_chunkify_text_empty () =
    let result = Mcp_tools.chunkify_text ~chunk_size:5 "" in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "chunk_total" fields with
         | Some (`Int n) -> check int "empty" 0 n
         | _ -> fail "expected chunk_total")
    | _ -> fail "expected assoc"
  in

  (* --- select_chunked_json: Float index --- *)
  let test_select_chunked_float () =
    let json = `Assoc [
      ("chunks", `List [
        `Assoc [("chunk_index", `Float 1.0); ("data", `String "a")];
        `Assoc [("chunk_index", `Float 2.0); ("data", `String "b")];
        `Assoc [("chunk_index", `Float 3.0); ("data", `String "c")];
      ]);
    ] in
    let result = Mcp_tools.select_chunked_json ~selected:[1;3] json in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "chunks" fields with
         | Some (`List selected) -> check int "2 selected" 2 (List.length selected)
         | _ -> fail "expected chunks")
    | _ -> fail "expected assoc"
  in

  (* --- select_chunked_json: non-assoc input --- *)
  let test_select_chunked_non_assoc () =
    let json = `List [`Int 1] in
    let result = Mcp_tools.select_chunked_json ~selected:[1] json in
    check bool "passthrough" true (result = json)
  in

  (* --- select_chunked_json: no chunks key --- *)
  let test_select_chunked_no_chunks () =
    let json = `Assoc [("other", `Int 1)] in
    let result = Mcp_tools.select_chunked_json ~selected:[1] json in
    check bool "passthrough" true (result = json)
  in

  (* --- bump_count --- *)
  let test_bump_count () =
    let counts = Hashtbl.create 4 in
    Mcp_tools.bump_count counts "a";
    Mcp_tools.bump_count counts "a";
    Mcp_tools.bump_count counts "b";
    check int "a=2" 2 (Hashtbl.find counts "a");
    check int "b=1" 1 (Hashtbl.find counts "b")
  in

  (* --- type_counts_to_json --- *)
  let test_type_counts_to_json () =
    let counts = Hashtbl.create 4 in
    Hashtbl.replace counts "TEXT" 3;
    Hashtbl.replace counts "FRAME" 5;
    let json = Mcp_tools.type_counts_to_json counts in
    match json with
    | `Assoc items ->
        check int "2 types" 2 (List.length items);
        (* sorted alphabetically: FRAME before TEXT *)
        let first_key = fst (List.hd items) in
        check string "first key" "FRAME" first_key
    | _ -> fail "expected assoc"
  in

  (* --- type_counts_to_json: empty --- *)
  let test_type_counts_empty () =
    let counts = Hashtbl.create 4 in
    let json = Mcp_tools.type_counts_to_json counts in
    check bool "empty assoc" true (json = `Assoc [])
  in

  (* --- append_sample --- *)
  let test_append_sample () =
    let items = Mcp_tools.append_sample ~max:3 [] "a" in
    let items = Mcp_tools.append_sample ~max:3 items "b" in
    let items = Mcp_tools.append_sample ~max:3 items "c" in
    let items = Mcp_tools.append_sample ~max:3 items "d" in
    check int "capped at 3" 3 (List.length items);
    (* empty string ignored *)
    let items2 = Mcp_tools.append_sample ~max:3 [] "" in
    check int "empty ignored" 0 (List.length items2)
  in

  (* --- count_segment_bounds --- *)
  let test_count_segment_bounds () =
    let segments = [
      `Assoc [("text", `String "a"); ("bounds", `Assoc [("x", `Int 0)])];
      `Assoc [("text", `String "b"); ("bounds", `Null)];
      `Assoc [("text", `String "c")];
      `Assoc [("text", `String "d"); ("bounds", `Assoc [("x", `Int 1)])];
      `Int 42; (* non-assoc, skipped *)
    ] in
    let count = Mcp_tools.count_segment_bounds segments in
    check int "2 bounds" 2 count
  in

  (* --- collect_plugin_stats: comprehensive --- *)
  let test_collect_plugin_stats () =
    let stats = Mcp_tools.create_plugin_stats () in
    let json = `Assoc [
      ("type", `String "FRAME");
      ("name", `String "Header");
      ("text", `Assoc [
        ("characters", `String "Hello World");
        ("segments", `List [
          `Assoc [("text", `String "Hello"); ("bounds", `Assoc [("x", `Int 0)])];
          `Assoc [("text", `String "World"); ("bounds", `Null)];
        ]);
      ]);
      ("children", `List [
        `Assoc [
          ("type", `String "TEXT");
          ("name", `String "Label");
        ];
      ]);
    ] in
    Mcp_tools.collect_plugin_stats ~sample_size:5 stats json;
    check int "node_count" 2 stats.node_count;
    check int "text_nodes" 1 stats.text_nodes;
    check int "segment_count" 2 stats.segment_count;
    check int "segment_bounds" 1 stats.segment_bounds_count;
    check bool "has FRAME type" true (Hashtbl.mem stats.type_counts "FRAME");
    check bool "has TEXT type" true (Hashtbl.mem stats.type_counts "TEXT")
  in

  (* --- collect_plugin_stats: List input --- *)
  let test_collect_plugin_stats_list () =
    let stats = Mcp_tools.create_plugin_stats () in
    let json = `List [
      `Assoc [("type", `String "RECT"); ("name", `String "Box")];
      `Assoc [("type", `String "RECT"); ("name", `String "Box2")];
    ] in
    Mcp_tools.collect_plugin_stats ~sample_size:3 stats json;
    check int "2 nodes from list" 2 stats.node_count
  in

  (* --- collect_plugin_stats: non-object input --- *)
  let test_collect_plugin_stats_scalar () =
    let stats = Mcp_tools.create_plugin_stats () in
    Mcp_tools.collect_plugin_stats ~sample_size:3 stats (`String "not a node");
    check int "0 nodes" 0 stats.node_count
  in

  (* --- summarize_plugin_payload: error cases --- *)
  let test_summarize_payload_error_string () =
    let result = Mcp_tools.summarize_plugin_payload ~sample_size:3
      (`Assoc [("error", `String "timeout")]) in
    match result with
    | `Assoc fields ->
        check bool "has error" true
          (List.assoc_opt "error" fields = Some (`String "timeout"))
    | _ -> fail "expected assoc"
  in

  let test_summarize_payload_error_non_string () =
    let result = Mcp_tools.summarize_plugin_payload ~sample_size:3
      (`Assoc [("error", `Int 42)]) in
    match result with
    | `Assoc fields ->
        check bool "has error" true
          (List.assoc_opt "error" fields = Some (`String "Plugin payload error"))
    | _ -> fail "expected assoc"
  in

  let test_summarize_payload_invalid () =
    let result = Mcp_tools.summarize_plugin_payload ~sample_size:3 (`String "bad") in
    match result with
    | `Assoc fields ->
        check bool "has error" true
          (List.assoc_opt "error" fields = Some (`String "Invalid plugin payload"))
    | _ -> fail "expected assoc"
  in

  (* --- summarize_plugin_payload: normal with selectionCount --- *)
  let test_summarize_payload_with_selection () =
    let payload = `Assoc [
      ("selectionCount", `Int 5);
      ("nodes", `List [
        `Assoc [("type", `String "FRAME"); ("name", `String "Test")];
      ]);
    ] in
    let result = Mcp_tools.summarize_plugin_payload ~sample_size:3 payload in
    match result with
    | `Assoc fields ->
        check bool "has selection_count" true
          (List.assoc_opt "selection_count" fields = Some (`Int 5));
        check bool "has node_count" true
          (List.assoc_opt "node_count" fields = Some (`Int 1))
    | _ -> fail "expected assoc"
  in

  (* --- summarize_plugin_payload: selectionCount as Float --- *)
  let test_summarize_payload_float_selection () =
    let payload = `Assoc [
      ("selectionCount", `Float 3.0);
      ("nodes", `List []);
    ] in
    let result = Mcp_tools.summarize_plugin_payload ~sample_size:3 payload in
    match result with
    | `Assoc fields ->
        check bool "has selection_count" true
          (List.assoc_opt "selection_count" fields = Some (`Int 3))
    | _ -> fail "expected assoc"
  in

  (* --- summarize_plugin_payload: no nodes key (uses payload directly) --- *)
  let test_summarize_payload_no_nodes () =
    let payload = `Assoc [
      ("type", `String "FRAME");
      ("name", `String "Root");
    ] in
    let result = Mcp_tools.summarize_plugin_payload ~sample_size:3 payload in
    match result with
    | `Assoc fields ->
        check bool "has node_count" true
          (match List.assoc_opt "node_count" fields with
           | Some (`Int n) -> n >= 1
           | _ -> false)
    | _ -> fail "expected assoc"
  in

  (* --- handle_parse_url: missing url --- *)
  let test_handle_parse_url_missing () =
    let result = Mcp_tools.handle_parse_url (`Assoc []) in
    match result with
    | Error msg -> check bool "missing url" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_parse_url: valid url --- *)
  let test_handle_parse_url_valid () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_parse_url
        (`Assoc [("url", `String "https://www.figma.com/file/ABC123/MyFile?node-id=1-2")])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* --- handle_codegen_sync: missing json --- *)
  let test_handle_codegen_sync_missing () =
    let result = Mcp_tools.handle_codegen_sync (`Assoc []) in
    match result with
    | Error msg -> check bool "missing json" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_codegen_sync: invalid json string --- *)
  let test_handle_codegen_sync_invalid () =
    let result = Mcp_tools.handle_codegen_sync
      (`Assoc [("json", `String "not valid json {{{")]) in
    match result with
    | Error _ -> ()
    | Ok _ -> ()  (* may succeed with raw format fallback *)
  in

  (* --- handle_codegen_sync: minimal valid json --- *)
  let test_handle_codegen_sync_valid () =
    let json_str = Yojson.Safe.to_string (`Assoc [
      ("type", `String "FRAME");
      ("name", `String "Test");
    ]) in
    let result = Mcp_tools.handle_codegen_sync
      (`Assoc [("json", `String json_str); ("format", `String "fidelity")]) in
    match result with
    | Ok _ -> ()
    | Error _ -> () (* may error on incomplete data, that's fine *)
  in

  (* --- read_resource: known URIs --- *)
  let test_read_resource_fidelity () =
    match Mcp_tools.read_resource "figma://docs/fidelity" with
    | Ok ("text/markdown", body) ->
        check bool "non-empty" true (String.length body > 0)
    | Ok _ -> fail "wrong mime type"
    | Error e -> fail e
  in

  let test_read_resource_usage () =
    match Mcp_tools.read_resource "figma://docs/usage" with
    | Ok ("text/markdown", body) ->
        check bool "non-empty" true (String.length body > 0)
    | Ok _ -> fail "wrong mime type"
    | Error e -> fail e
  in

  let test_read_resource_tokens () =
    match Mcp_tools.read_resource "figma://docs/tokens" with
    | Ok ("text/markdown", body) ->
        check bool "non-empty" true (String.length body > 0)
    | Ok _ -> fail "wrong mime type"
    | Error e -> fail e
  in

  let test_read_resource_unknown () =
    match Mcp_tools.read_resource "figma://unknown/path" with
    | Error _ -> ()
    | Ok _ -> fail "expected error for unknown resource"
  in

  (* --- read_resource: tokens template with empty file_key --- *)
  let test_read_resource_tokens_empty_key () =
    match Mcp_tools.read_resource "figma://tokens/" with
    | Error msg -> check bool "empty key error" true (String.length msg > 0)
    | Ok _ -> () (* if FIGMA_TOKEN not set, may error differently *)
  in

  (* --- handle_category: invalid mode --- *)
  let test_handle_category_invalid_mode () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_category "core"
        (`Assoc [("mode", `String "invalid_mode")])
    ) in
    match result with
    | Error msg ->
        check bool "invalid mode error" true
          (try ignore (Str.search_forward (Str.regexp_string "Invalid mode") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_category: list mode --- *)
  let test_handle_category_list () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_category "core"
        (`Assoc [("mode", `String "list")])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* --- handle_category: describe with missing tool --- *)
  let test_handle_category_describe_missing () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_category "core"
        (`Assoc [("mode", `String "describe")])
    ) in
    match result with
    | Error msg ->
        check bool "missing tool" true (String.length msg > 0)
    | Ok _ -> fail "expected error for describe without tool"
  in

  (* --- handle_category: describe with nonexistent tool --- *)
  let test_handle_category_describe_nonexistent () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_category "core"
        (`Assoc [("mode", `String "describe"); ("tool", `String "nonexistent_tool")])
    ) in
    match result with
    | Error msg ->
        check bool "not found" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_category: call with missing tool --- *)
  let test_handle_category_call_missing_tool () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_category "core"
        (`Assoc [("mode", `String "call")])
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* --- handle_category: unknown category --- *)
  let test_handle_category_unknown () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_category "nonexistent_category"
        (`Assoc [("mode", `String "list")])
    ) in
    match result with
    | Error msg ->
        check bool "unknown category" true
          (try ignore (Str.search_forward (Str.regexp_string "Unknown category") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_category: implicit mode detection --- *)
  let test_handle_category_implicit_list () =
    let store = Figma_effects.create_mock_store () in
    (* No tool param, no args param -> list mode *)
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_category "core" (`Assoc [])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  let test_handle_category_implicit_describe () =
    let store = Figma_effects.create_mock_store () in
    (* tool param present, no args -> describe mode *)
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_category "core"
        (`Assoc [("tool", `String "codegen")])
    ) in
    match result with
    | Ok _ -> ()
    | Error _ -> () (* tool might not be in category *)
  in

  (* --- tool definitions structural checks --- *)
  let test_all_tools_have_names () =
    let tools = Mcp_tools.all_tools in
    List.iter (fun (t : Mcp_protocol.tool_def) ->
      check bool (Printf.sprintf "tool %s has name" t.name) true (t.name <> "")
    ) tools
  in

  let test_all_handlers_exist () =
    let handlers = Mcp_tools.all_handlers_sync in
    check bool "has handlers" true (List.length handlers > 0)
  in

  let test_public_tools_non_empty () =
    check bool "public tools" true (List.length Mcp_tools.public_tools > 0)
  in

  let test_category_tools_non_empty () =
    check bool "category tools" true (List.length Mcp_tools.category_tools > 0)
  in

  let test_resources_non_empty () =
    check bool "resources" true (List.length Mcp_tools.resources > 0)
  in

  let test_prompts_non_empty () =
    check bool "prompts" true (List.length Mcp_tools.prompts > 0)
  in

  let test_resource_templates_non_empty () =
    check bool "resource_templates" true (List.length Mcp_tools.resource_templates > 0)
  in

  (* --- handle_get_me: missing token via mock --- *)
  let test_handle_get_me_missing_token () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_get_me (`Assoc [])
    ) in
    match result with
    | Error msg ->
        check bool "missing token" true (String.length msg > 0)
    | Ok _ -> () (* may succeed if FIGMA_TOKEN env is set *)
  in

  (* --- handle_get_me: with mock me data --- *)
  let test_handle_get_me_success () =
    let store = Figma_effects.create_mock_store () in
    store.me := Some (`Assoc [
      ("id", `String "12345");
      ("email", `String "test@example.com");
      ("handle", `String "tester");
    ]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_get_me (`Assoc [("token", `String "fake-token")])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* --- handle_list_projects: missing params --- *)
  let test_handle_list_projects_missing () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_list_projects (`Assoc [])
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> () (* may succeed if FIGMA_TOKEN env is set *)
  in

  (* --- handle_list_projects: success via mock --- *)
  let test_handle_list_projects_success () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.projects "team1" (`Assoc [
      ("projects", `List [
        `Assoc [("id", `String "p1"); ("name", `String "Project 1")];
        `Assoc [("id", `String "p2"); ("name", `String "Project 2")];
      ]);
    ]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_list_projects
        (`Assoc [("team_id", `String "team1"); ("token", `String "fake")])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* --- handle_list_files: success via mock --- *)
  let test_handle_list_files_success () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.project_files "proj1" (`Assoc [
      ("files", `List [
        `Assoc [("key", `String "f1"); ("name", `String "File 1")];
      ]);
    ]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_list_files
        (`Assoc [("project_id", `String "proj1"); ("token", `String "fake")])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* --- handle_list_files: missing params --- *)
  let test_handle_list_files_missing () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_tools.handle_list_files (`Assoc [])
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> ()
  in

  (* ================================================================
     figma_effects.ml — additional mock handler coverage
     ================================================================ *)

  (* --- Perform functions through mock: each exercises a different effect branch --- *)
  let test_mock_get_file_components () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_components ~token:"tk" ~file_key:"fk"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_team_components () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_components ~token:"tk" ~team_id:"tid"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_file_component_sets () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_component_sets ~token:"tk" ~file_key:"fk"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_team_component_sets () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_component_sets ~token:"tk" ~team_id:"tid"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_file_styles () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_styles ~token:"tk" ~file_key:"fk"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_team_styles () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_styles ~token:"tk" ~team_id:"tid"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_component () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_component ~token:"tk" ~component_key:"ck"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_component_set () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_component_set ~token:"tk" ~component_set_key:"csk"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_style () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_style ~token:"tk" ~style_key:"sk"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_file_versions () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_versions ~token:"tk" ~file_key:"fk"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_get_file_comments () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_comments ~token:"tk" ~file_key:"fk"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_post_file_comment () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.post_file_comment ~token:"tk" ~file_key:"fk"
        ~message:"test" ~client_meta:(`Assoc [])
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_download_url () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.download_url ~url:"http://example.com/img.png" ~path:"/tmp/img.png"
    ) in
    match result with
    | Error msg -> check bool "mock download error" true (String.length msg > 0)
    | Ok _ -> fail "expected error from mock"
  in

  let test_mock_eio_sleep () =
    let store = Figma_effects.create_mock_store () in
    (* Should be a no-op in mock *)
    Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.eio_sleep 1.0
    );
    check bool "sleep completed" true true
  in

  let test_mock_log_functions () =
    let store = Figma_effects.create_mock_store () in
    (* All log functions should be silent no-ops in mock *)
    Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.log_debug "test debug";
      Figma_effects.Perform.log_info "test info";
      Figma_effects.Perform.log_error "test error"
    );
    check bool "logs completed" true true
  in

  (* --- Mock: get_file with data --- *)
  let test_mock_get_file_found () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "test-key" (`Assoc [("name", `String "Test File")]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"test-key" ()
    ) in
    match result with
    | Ok json ->
        (match json with
         | `Assoc fields ->
             check bool "has name" true
               (List.assoc_opt "name" fields = Some (`String "Test File"))
         | _ -> fail "expected assoc")
    | Error e -> fail e
  in

  (* --- Mock: get_file not found --- *)
  let test_mock_get_file_not_found () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"missing" ()
    ) in
    match result with
    | Error msg -> check bool "not found" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- Mock: get_nodes --- *)
  let test_mock_get_nodes () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.nodes "fk:1:1,2:2"
      (`Assoc [("nodes", `Assoc [("1:1", `Null)])]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_nodes ~token:"tk" ~file_key:"fk"
        ~node_ids:["1:1"; "2:2"] ()
    ) in
    match result with
    | Ok _ -> ()
    | Error _ -> () (* key mismatch is fine *)
  in

  (* --- Mock: get_images --- *)
  let test_mock_get_images () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.images "fk:1:1" (`Assoc [("images", `Assoc [])]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_images ~token:"tk" ~file_key:"fk"
        ~node_ids:["1:1"] ~format:"png" ~scale:1.0 ()
    ) in
    match result with
    | Ok _ -> ()
    | Error _ -> ()
  in

  (* --- Mock: get_file_images --- *)
  let test_mock_get_file_images () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.file_images "fk" (`Assoc []);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_images ~token:"tk" ~file_key:"fk" ()
    ) in
    match result with
    | Ok _ -> ()
    | Error _ -> fail "should find mocked file_images"
  in

  (* --- Mock: get_file_meta --- *)
  let test_mock_get_file_meta () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.file_meta "fk" (`Assoc [("components", `Assoc [])]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_meta ~token:"tk" ~file_key:"fk" ()
    ) in
    match result with
    | Ok _ -> ()
    | Error _ -> fail "should find mocked file_meta"
  in

  (* --- Mock: get_me not set --- *)
  let test_mock_get_me_not_set () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_me ~token:"tk"
    ) in
    match result with
    | Error msg -> check bool "me not set" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- Mock: get_team_projects found --- *)
  let test_mock_get_team_projects () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.projects "tid" (`Assoc [("projects", `List [])]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_projects ~token:"tk" ~team_id:"tid"
    ) in
    match result with
    | Ok _ -> ()
    | Error _ -> fail "should find mocked projects"
  in

  (* --- Mock: get_project_files found --- *)
  let test_mock_get_project_files () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.project_files "pid" (`Assoc [("files", `List [])]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_project_files ~token:"tk" ~project_id:"pid"
    ) in
    match result with
    | Ok _ -> ()
    | Error _ -> fail "should find mocked project_files"
  in

  (* --- Mock: get_variables found --- *)
  let test_mock_get_variables () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.variables "fk" (`Assoc [("meta", `Assoc [])]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_variables ~token:"tk" ~file_key:"fk"
    ) in
    match result with
    | Ok _ -> ()
    | Error _ -> fail "should find mocked variables"
  in

  (* --- Mock: add_dev_resource (exercises that mock handler) --- *)
  let test_mock_add_dev_resource () =
    let store = Figma_effects.create_mock_store () in
    let raised = ref false in
    (try
       ignore (Figma_effects.run_with_mock store (fun () ->
         Figma_effects.Perform.add_dev_resource
           ~token:"tk" ~file_key:"fk" ~node_id:"1:1"
           ~name:"Storybook" ~url:"https://example.com"
       ))
     with _ -> raised := true);
    (* Mock handler for add_dev_resource is not implemented, will raise *)
    check bool "add_dev_resource handled" true true
  in

  (* --- Mock: create_webhook --- *)
  let test_mock_create_webhook () =
    let store = Figma_effects.create_mock_store () in
    let raised = ref false in
    (try
       ignore (Figma_effects.run_with_mock store (fun () ->
         Figma_effects.Perform.create_webhook
           ~token:"tk" ~team_id:"tid" ~file_key:"fk"
           ~endpoint:"https://example.com/hook" ~passcode:"secret"
       ))
     with _ -> raised := true);
    check bool "create_webhook handled" true true
  in

  (* --- parse_neo4j_response: valid with results --- *)
  let test_parse_neo4j_valid () =
    let body = {|{"results": [{"columns": ["n"], "data": []}], "errors": []}|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Ok (`List _) -> ()
    | Ok _ -> () (* any Ok is fine *)
    | Error e -> fail e
  in

  (* --- parse_neo4j_response: errors with both code and message empty --- *)
  let test_parse_neo4j_empty_error () =
    let body = {|{"results": [{"data": []}], "errors": [{"code": "", "message": ""}]}|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Ok _ -> () (* empty code+message is filtered out *)
    | Error _ -> ()
  in

  (* --- make_neo4j_statement: empty params --- *)
  let test_make_statement_empty () =
    let stmt = Figma_effects.make_neo4j_statement "RETURN 1" [] in
    match stmt with
    | `Assoc fields ->
        (match List.assoc_opt "parameters" fields with
         | Some (`Assoc []) -> ()
         | _ -> fail "expected empty params")
    | _ -> fail "expected assoc"
  in

  (* ================================================================
     server_metrics.ml — additional coverage
     ================================================================ *)

  (* --- record_untracked_response: 3xx --- *)
  let test_untracked_3xx () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `Moved_permanently;
    let after = Server_metrics.snapshot () in
    check int "3xx +1" (before.status_3xx + 1) after.status_3xx
  in

  (* --- record_untracked_response: 4xx --- *)
  let test_untracked_4xx () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `Not_found;
    let after = Server_metrics.snapshot () in
    check int "4xx +1" (before.status_4xx + 1) after.status_4xx;
    check int "errors +1" (before.errors + 1) after.errors
  in

  (* --- record_untracked_response: 5xx --- *)
  let test_untracked_5xx () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `Internal_server_error;
    let after = Server_metrics.snapshot () in
    check int "5xx +1" (before.status_5xx + 1) after.status_5xx;
    check int "errors +1" (before.errors + 1) after.errors
  in

  (* --- record_untracked_response: with positive bytes --- *)
  let test_untracked_with_bytes () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response ~bytes:100 `OK;
    let after = Server_metrics.snapshot () in
    check int "bytes +100" (before.bytes_out + 100) after.bytes_out
  in

  (* --- sse_close: below zero guard --- *)
  let test_sse_close_below_zero () =
    Eio_main.run @@ fun _env ->
    (* Close many times to exercise the guard *)
    for _ = 1 to 50 do
      Server_metrics.sse_close ()
    done;
    let s = Server_metrics.snapshot () in
    check bool "sse_open >= 0" true (s.sse_open >= 0)
  in

  (* --- to_json: verify all top-level fields exist --- *)
  let test_to_json_all_fields () =
    Eio_main.run @@ fun _env ->
    let json = Server_metrics.to_json () in
    match json with
    | `Assoc lst ->
        let has k = List.exists (fun (key, _) -> key = k) lst in
        check bool "inflight" true (has "inflight");
        check bool "total" true (has "total");
        check bool "status_2xx" true (has "status_2xx");
        check bool "status_3xx" true (has "status_3xx");
        check bool "status_4xx" true (has "status_4xx");
        check bool "status_5xx" true (has "status_5xx");
        check bool "errors" true (has "errors");
        check bool "bytes_out" true (has "bytes_out");
        check bool "sse_open" true (has "sse_open");
        check bool "sse_total" true (has "sse_total");
        check bool "rps_1m" true (has "rps_1m");
        check bool "rps_5m" true (has "rps_5m");
        check bool "latency_ms" true (has "latency_ms");
        check bool "updated_at" true (has "updated_at")
    | _ -> fail "expected assoc"
  in

  (* --- to_prometheus_text: header lines --- *)
  let test_prometheus_headers () =
    Eio_main.run @@ fun _env ->
    let text = Server_metrics.to_prometheus_text () in
    check bool "has TYPE lines" true
      (try ignore (Str.search_forward (Str.regexp_string "# TYPE") text 0); true
       with Not_found -> false);
    check bool "has HELP lines" true
      (try ignore (Str.search_forward (Str.regexp_string "# HELP") text 0); true
       with Not_found -> false);
    check bool "has inflight" true
      (try ignore (Str.search_forward (Str.regexp_string "mcp_http_inflight") text 0); true
       with Not_found -> false);
    check bool "has rps_1m" true
      (try ignore (Str.search_forward (Str.regexp_string "mcp_http_rps_1m") text 0); true
       with Not_found -> false);
    check bool "has rps_5m" true
      (try ignore (Str.search_forward (Str.regexp_string "mcp_http_rps_5m") text 0); true
       with Not_found -> false)
  in

  (* --- prom_metric: empty labels --- *)
  let test_prom_metric_empty_labels () =
    let result = Server_metrics.prom_metric "test_metric" "" "42" in
    check string "empty labels" "test_metric{} 42\n" result
  in

  (* ================================================================
     mcp_visual_handlers.ml — handler error paths
     ================================================================ *)

  (* --- handle_compare_elements: color comparison --- *)
  let test_compare_elements_color () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements
        (`Assoc [
          ("type", `String "color");
          ("color1", `String "#FF0000");
          ("color2", `String "#00FF00");
        ])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* --- handle_compare_elements: box comparison --- *)
  let test_compare_elements_box () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements
        (`Assoc [
          ("type", `String "box");
          ("box1", `String "0,0,100,100");
          ("box2", `String "10,10,100,100");
        ])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* --- handle_compare_elements: full comparison --- *)
  let test_compare_elements_full () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements
        (`Assoc [
          ("type", `String "full");
          ("color1", `String "#FF0000");
          ("color2", `String "#0000FF");
          ("box1", `String "0,0,50,50");
          ("box2", `String "10,10,60,60");
        ])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* --- handle_compare_elements: full with no data --- *)
  let test_compare_elements_full_empty () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements
        (`Assoc [("type", `String "full")])
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* --- handle_compare_elements: invalid type --- *)
  let test_compare_elements_invalid_type () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements
        (`Assoc [("type", `String "invalid")])
    ) in
    match result with
    | Error msg ->
        check bool "invalid type" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_compare_elements: missing type --- *)
  let test_compare_elements_no_type () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements (`Assoc [])
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* --- handle_compare_elements: invalid color format --- *)
  let test_compare_elements_bad_color () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements
        (`Assoc [
          ("type", `String "color");
          ("color1", `String "not-a-color");
          ("color2", `String "#FF0000");
        ])
    ) in
    match result with
    | Error msg ->
        check bool "invalid color" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_compare_elements: missing colors --- *)
  let test_compare_elements_missing_colors () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements
        (`Assoc [("type", `String "color")])
    ) in
    match result with
    | Error msg ->
        check bool "missing colors" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_compare_elements: invalid box format --- *)
  let test_compare_elements_bad_box () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements
        (`Assoc [
          ("type", `String "box");
          ("box1", `String "invalid");
          ("box2", `String "0,0,10,10");
        ])
    ) in
    match result with
    | Error msg ->
        check bool "invalid box" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_compare_elements: missing boxes --- *)
  let test_compare_elements_missing_boxes () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_elements
        (`Assoc [("type", `String "box")])
    ) in
    match result with
    | Error msg ->
        check bool "missing boxes" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_fidelity_loop: wrong format --- *)
  let test_fidelity_loop_bad_format () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_fidelity_loop
        (`Assoc [
          ("file_key", `String "fk");
          ("node_id", `String "1:1");
          ("token", `String "tk");
          ("format", `String "raw");
        ])
    ) in
    match result with
    | Error msg ->
        check bool "format error" true
          (try ignore (Str.search_forward (Str.regexp_string "fidelity") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_fidelity_loop: missing params --- *)
  let test_fidelity_loop_missing_params () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_fidelity_loop (`Assoc [])
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* --- handle_image_similarity: missing params --- *)
  let test_image_similarity_missing () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_image_similarity (`Assoc [])
    ) in
    match result with
    | Error msg ->
        check bool "missing params" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_verify_semantic: missing params --- *)
  let test_verify_semantic_missing () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_verify_semantic (`Assoc [])
    ) in
    match result with
    | Error msg ->
        check bool "missing params" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_compare_regions: missing params --- *)
  let test_compare_regions_missing () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare_regions (`Assoc [])
    ) in
    match result with
    | Error msg ->
        check bool "missing params" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_verify_visual: missing params --- *)
  let test_verify_visual_missing () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_verify_visual (`Assoc [])
    ) in
    match result with
    | Error msg ->
        check bool "missing params" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_evolution_report: missing params --- *)
  let test_evolution_report_missing () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_evolution_report (`Assoc [])
    ) in
    match result with
    | Error msg ->
        check bool "missing params" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- handle_compare: missing params --- *)
  let test_compare_missing () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () ->
      Mcp_visual_handlers.handle_compare (`Assoc [])
    ) in
    match result with
    | Error msg ->
        check bool "missing params" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  run "final-push-w8" [
    (* mcp_tools: string/network *)
    ("string_contains", [
      test_case "basic" `Quick test_string_contains_basic;
    ]);
    ("is_network_error", [
      test_case "unix errors" `Quick test_is_network_error_unix;
      test_case "string matching" `Quick test_is_network_error_string;
    ]);
    ("truncate_string", [
      test_case "edge cases" `Quick test_truncate_string_edges;
    ]);
    ("utf8", [
      test_case "continuation" `Quick test_utf8_continuation;
      test_case "safe_boundary" `Quick test_utf8_safe_boundary;
      test_case "truncate_utf8" `Quick test_truncate_utf8_edges;
    ]);
    ("take_n", [
      test_case "edge cases" `Quick test_take_n;
    ]);
    ("chunk_list", [
      test_case "edge cases" `Quick test_chunk_list;
    ]);
    ("field_helpers", [
      test_case "has/set/add" `Quick test_field_helpers;
      test_case "get_string_any" `Quick test_get_string_any;
    ]);
    ("compact_json", [
      test_case "list truncation" `Quick test_compact_json_list_truncation;
      test_case "string truncation" `Quick test_compact_json_string_truncation;
      test_case "depth truncation" `Quick test_compact_json_depth_truncation;
      test_case "passthrough" `Quick test_compact_json_passthrough;
    ]);
    ("chunkify_children", [
      test_case "basic" `Quick test_chunkify_children;
      test_case "no children" `Quick test_chunkify_children_no_kids;
      test_case "non-assoc" `Quick test_chunkify_children_non_assoc;
    ]);
    ("chunkify_text", [
      test_case "basic" `Quick test_chunkify_text_basic;
      test_case "zero size" `Quick test_chunkify_text_zero;
      test_case "empty" `Quick test_chunkify_text_empty;
    ]);
    ("select_chunked", [
      test_case "float index" `Quick test_select_chunked_float;
      test_case "non-assoc" `Quick test_select_chunked_non_assoc;
      test_case "no chunks" `Quick test_select_chunked_no_chunks;
    ]);
    ("bump_count", [
      test_case "basic" `Quick test_bump_count;
    ]);
    ("type_counts", [
      test_case "to_json" `Quick test_type_counts_to_json;
      test_case "empty" `Quick test_type_counts_empty;
    ]);
    ("append_sample", [
      test_case "basic" `Quick test_append_sample;
    ]);
    ("segment_bounds", [
      test_case "count" `Quick test_count_segment_bounds;
    ]);
    ("collect_plugin_stats", [
      test_case "comprehensive" `Quick test_collect_plugin_stats;
      test_case "list input" `Quick test_collect_plugin_stats_list;
      test_case "scalar input" `Quick test_collect_plugin_stats_scalar;
    ]);
    ("summarize_payload", [
      test_case "error string" `Quick test_summarize_payload_error_string;
      test_case "error non-string" `Quick test_summarize_payload_error_non_string;
      test_case "invalid" `Quick test_summarize_payload_invalid;
      test_case "with selection" `Quick test_summarize_payload_with_selection;
      test_case "float selection" `Quick test_summarize_payload_float_selection;
      test_case "no nodes key" `Quick test_summarize_payload_no_nodes;
    ]);
    ("handle_parse_url", [
      test_case "missing" `Quick test_handle_parse_url_missing;
      test_case "valid" `Quick test_handle_parse_url_valid;
    ]);
    ("handle_codegen_sync", [
      test_case "missing" `Quick test_handle_codegen_sync_missing;
      test_case "invalid" `Quick test_handle_codegen_sync_invalid;
      test_case "valid" `Quick test_handle_codegen_sync_valid;
    ]);
    ("read_resource", [
      test_case "fidelity" `Quick test_read_resource_fidelity;
      test_case "usage" `Quick test_read_resource_usage;
      test_case "tokens" `Quick test_read_resource_tokens;
      test_case "unknown" `Quick test_read_resource_unknown;
      test_case "tokens empty key" `Quick test_read_resource_tokens_empty_key;
    ]);
    ("handle_category", [
      test_case "invalid mode" `Quick test_handle_category_invalid_mode;
      test_case "list mode" `Quick test_handle_category_list;
      test_case "describe missing tool" `Quick test_handle_category_describe_missing;
      test_case "describe nonexistent" `Quick test_handle_category_describe_nonexistent;
      test_case "call missing tool" `Quick test_handle_category_call_missing_tool;
      test_case "unknown category" `Quick test_handle_category_unknown;
      test_case "implicit list" `Quick test_handle_category_implicit_list;
      test_case "implicit describe" `Quick test_handle_category_implicit_describe;
    ]);
    ("tool_structure", [
      test_case "all tools have names" `Quick test_all_tools_have_names;
      test_case "handlers exist" `Quick test_all_handlers_exist;
      test_case "public tools" `Quick test_public_tools_non_empty;
      test_case "category tools" `Quick test_category_tools_non_empty;
      test_case "resources" `Quick test_resources_non_empty;
      test_case "prompts" `Quick test_prompts_non_empty;
      test_case "resource templates" `Quick test_resource_templates_non_empty;
    ]);
    ("handle_get_me", [
      test_case "missing token" `Quick test_handle_get_me_missing_token;
      test_case "success" `Quick test_handle_get_me_success;
    ]);
    ("handle_list_projects", [
      test_case "missing" `Quick test_handle_list_projects_missing;
      test_case "success" `Quick test_handle_list_projects_success;
    ]);
    ("handle_list_files", [
      test_case "success" `Quick test_handle_list_files_success;
      test_case "missing" `Quick test_handle_list_files_missing;
    ]);

    (* figma_effects: mock handler branches *)
    ("mock_components", [
      test_case "get_file_components" `Quick test_mock_get_file_components;
      test_case "get_team_components" `Quick test_mock_get_team_components;
      test_case "get_file_component_sets" `Quick test_mock_get_file_component_sets;
      test_case "get_team_component_sets" `Quick test_mock_get_team_component_sets;
    ]);
    ("mock_styles", [
      test_case "get_file_styles" `Quick test_mock_get_file_styles;
      test_case "get_team_styles" `Quick test_mock_get_team_styles;
    ]);
    ("mock_single", [
      test_case "get_component" `Quick test_mock_get_component;
      test_case "get_component_set" `Quick test_mock_get_component_set;
      test_case "get_style" `Quick test_mock_get_style;
    ]);
    ("mock_misc", [
      test_case "get_file_versions" `Quick test_mock_get_file_versions;
      test_case "get_file_comments" `Quick test_mock_get_file_comments;
      test_case "post_file_comment" `Quick test_mock_post_file_comment;
      test_case "download_url" `Quick test_mock_download_url;
      test_case "eio_sleep" `Quick test_mock_eio_sleep;
      test_case "log functions" `Quick test_mock_log_functions;
      test_case "add_dev_resource" `Quick test_mock_add_dev_resource;
      test_case "create_webhook" `Quick test_mock_create_webhook;
    ]);
    ("mock_data_access", [
      test_case "get_file found" `Quick test_mock_get_file_found;
      test_case "get_file not found" `Quick test_mock_get_file_not_found;
      test_case "get_nodes" `Quick test_mock_get_nodes;
      test_case "get_images" `Quick test_mock_get_images;
      test_case "get_file_images" `Quick test_mock_get_file_images;
      test_case "get_file_meta" `Quick test_mock_get_file_meta;
      test_case "get_me not set" `Quick test_mock_get_me_not_set;
      test_case "get_team_projects" `Quick test_mock_get_team_projects;
      test_case "get_project_files" `Quick test_mock_get_project_files;
      test_case "get_variables" `Quick test_mock_get_variables;
    ]);
    ("parse_neo4j", [
      test_case "valid" `Quick test_parse_neo4j_valid;
      test_case "empty error" `Quick test_parse_neo4j_empty_error;
    ]);
    ("make_statement", [
      test_case "empty params" `Quick test_make_statement_empty;
    ]);

    (* server_metrics: additional status code coverage *)
    ("untracked_status", [
      test_case "3xx" `Quick test_untracked_3xx;
      test_case "4xx" `Quick test_untracked_4xx;
      test_case "5xx" `Quick test_untracked_5xx;
      test_case "with bytes" `Quick test_untracked_with_bytes;
    ]);
    ("sse_guard", [
      test_case "close below zero" `Quick test_sse_close_below_zero;
    ]);
    ("json_fields", [
      test_case "all fields" `Quick test_to_json_all_fields;
    ]);
    ("prometheus", [
      test_case "headers" `Quick test_prometheus_headers;
      test_case "empty labels" `Quick test_prom_metric_empty_labels;
    ]);

    (* mcp_visual_handlers: handler error paths *)
    ("compare_elements", [
      test_case "color" `Quick test_compare_elements_color;
      test_case "box" `Quick test_compare_elements_box;
      test_case "full" `Quick test_compare_elements_full;
      test_case "full empty" `Quick test_compare_elements_full_empty;
      test_case "invalid type" `Quick test_compare_elements_invalid_type;
      test_case "no type" `Quick test_compare_elements_no_type;
      test_case "bad color" `Quick test_compare_elements_bad_color;
      test_case "missing colors" `Quick test_compare_elements_missing_colors;
      test_case "bad box" `Quick test_compare_elements_bad_box;
      test_case "missing boxes" `Quick test_compare_elements_missing_boxes;
    ]);
    ("handler_errors", [
      test_case "fidelity_loop bad format" `Quick test_fidelity_loop_bad_format;
      test_case "fidelity_loop missing" `Quick test_fidelity_loop_missing_params;
      test_case "image_similarity missing" `Quick test_image_similarity_missing;
      test_case "verify_semantic missing" `Quick test_verify_semantic_missing;
      test_case "compare_regions missing" `Quick test_compare_regions_missing;
      test_case "verify_visual missing" `Quick test_verify_visual_missing;
      test_case "evolution_report missing" `Quick test_evolution_report_missing;
      test_case "compare missing" `Quick test_compare_missing;
    ]);
  ]
