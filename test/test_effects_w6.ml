(** Wave 6 coverage tests for figma_effects.ml
    Targets remaining uncovered branches in:
    - parse_neo4j_response: generic exception handler (| e ->), multi-error,
      edge-case JSON structures
    - make_neo4j_statement: nested params, unicode, large param sets
    - run_with_mock: continuation composition, error propagation,
      store mutation patterns, effect interleaving
    - Perform module: all optional-arg combinations, multiple node_ids,
      empty lists

    Note: run_with_pure_eio_api and neo4j_http_post are excluded because
    they require real Eio network I/O (HTTP to api.figma.com / Neo4j).
*)

let () =
  let open Alcotest in

  (* ================================================================
     parse_neo4j_response — coverage for the generic exception handler
     ================================================================ *)

  (* Valid JSON but NOT an object → member raises Type_error → caught by | e -> *)
  let test_parse_response_json_array () =
    let body = "[1, 2, 3]" in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "contains Parse error" true
          (String.length msg > 0 &&
           (try ignore (Str.search_forward (Str.regexp_string "Parse error") msg 0); true
            with Not_found -> false))
    | Ok _ -> fail "expected error for JSON array input"
  in

  (* Valid JSON string literal → member raises Type_error *)
  let test_parse_response_json_string () =
    let body = {|"just a string"|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "error msg non-empty" true (String.length msg > 0)
    | Ok _ -> fail "expected error for JSON string input"
  in

  (* Valid JSON number → member raises Type_error *)
  let test_parse_response_json_number () =
    let body = "42" in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "error msg non-empty" true (String.length msg > 0)
    | Ok _ -> fail "expected error for JSON number input"
  in

  (* Valid JSON boolean → member raises Type_error *)
  let test_parse_response_json_bool () =
    let body = "true" in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "error msg non-empty" true (String.length msg > 0)
    | Ok _ -> fail "expected error for JSON boolean input"
  in

  (* Valid JSON null → member "errors" on null raises Type_error → caught by | e -> *)
  let test_parse_response_json_null () =
    let body = "null" in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "error msg non-empty" true (String.length msg > 0)
    | Ok _ -> fail "expected error for JSON null input"
  in

  (* Multiple errors in list — verifies String.concat ";" joins them *)
  let test_parse_response_multi_errors () =
    let body = {|{
      "results": [],
      "errors": [
        {"code": "Err.A", "message": "first problem"},
        {"code": "Err.B", "message": "second problem"},
        {"code": "Err.C", "message": "third problem"}
      ]
    }|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        let has s = try ignore (Str.search_forward (Str.regexp_string s) msg 0); true with Not_found -> false in
        check bool "has first error" true (has "Err.A");
        check bool "has second error" true (has "Err.B");
        check bool "has third error" true (has "Err.C");
        check bool "has semicolon" true (has ";")
    | Ok _ -> fail "expected error for multi-error response"
  in

  (* Mix of empty and non-empty errors — empty ones are filtered *)
  let test_parse_response_mixed_errors () =
    let body = {|{
      "results": [],
      "errors": [
        {"code": "", "message": ""},
        {"code": "Real.Error", "message": "something broke"},
        {"code": "", "message": ""}
      ]
    }|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "has real error" true
          (try ignore (Str.search_forward (Str.regexp_string "Real.Error") msg 0); true
           with Not_found -> false);
        (* Only 1 error should appear, not 3 *)
        let count = ref 0 in
        let pos = ref 0 in
        (try while true do
           pos := Str.search_forward (Str.regexp_string ": ") msg !pos + 1;
           incr count
         done with Not_found -> ());
        check bool "filtered empty errors" true (!count <= 2)
    | Ok _ -> fail "expected error"
  in

  (* Errors list with non-assoc items → member "code" on int/string/null raises Type_error
     which propagates from List.filter_map, caught by the generic | e -> handler *)
  let test_parse_response_errors_non_assoc_items () =
    let body = {|{
      "results": [],
      "errors": [42, "bad", null, {"code": "X", "message": "Y"}]
    }|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        (* Type_error from member "code" on integer 42 — hits generic handler *)
        check bool "parse error" true
          (try ignore (Str.search_forward (Str.regexp_string "Parse error") msg 0); true
           with Not_found ->
             try ignore (Str.search_forward (Str.regexp_string "Type_error") msg 0); true
             with Not_found -> false)
    | Ok _ -> fail "expected error"
  in

  (* Errors with code but no message key → code non-empty, message = "" → not filtered *)
  let test_parse_response_error_code_only () =
    let body = {|{"results": [], "errors": [{"code": "Neo.Syntax"}]}|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "has code" true
          (try ignore (Str.search_forward (Str.regexp_string "Neo.Syntax") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected error"
  in

  (* Errors with message but no code key → code = "", message non-empty → not filtered *)
  let test_parse_response_error_message_only () =
    let body = {|{"results": [], "errors": [{"message": "broken query"}]}|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "has message" true
          (try ignore (Str.search_forward (Str.regexp_string "broken query") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected error"
  in

  (* Large response body — exercises parse path fully *)
  let test_parse_response_large_results () =
    let data = List.init 100 (fun i ->
      Printf.sprintf {|{"row": [%d], "meta": [null]}|} i
    ) |> String.concat ", " in
    let body = Printf.sprintf {|{"results": [{"columns": ["n"], "data": [%s]}], "errors": []}|} data in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Ok _ -> ()
    | Error e -> fail (Printf.sprintf "unexpected error: %s" e)
  in

  (* Completely empty JSON object — no errors, no results keys *)
  let test_parse_response_empty_object () =
    let body = "{}" in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Ok `Null -> ()  (* member "results" on {} returns `Null *)
    | Ok _ -> ()
    | Error e -> fail (Printf.sprintf "unexpected error: %s" e)
  in

  (* Empty string body → Yojson parse error *)
  let test_parse_response_empty_string () =
    let body = "" in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "json parse error" true
          (try ignore (Str.search_forward (Str.regexp_string "JSON parse error") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected error for empty string"
  in

  (* Truncated JSON → parse error *)
  let test_parse_response_truncated () =
    let body = {|{"results": [{"data":|}  in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error for truncated JSON"
  in

  (* ================================================================
     make_neo4j_statement — additional edge cases
     ================================================================ *)

  (* Unicode param values *)
  let test_neo4j_statement_unicode () =
    let stmt = Figma_effects.make_neo4j_statement
      "CREATE (n:Node {name: $name})"
      [("name", `String "한국어 테스트")] in
    match stmt with
    | `Assoc fields ->
        (match List.assoc_opt "parameters" fields with
         | Some (`Assoc params) ->
             check bool "has unicode name" true
               (List.assoc_opt "name" params = Some (`String "한국어 테스트"))
         | _ -> fail "expected params")
    | _ -> fail "expected assoc"
  in

  (* Nested JSON param values *)
  let test_neo4j_statement_nested_params () =
    let nested = `Assoc [
      ("x", `Float 1.5);
      ("y", `Float 2.5);
      ("meta", `Assoc [("label", `String "A")])
    ] in
    let stmt = Figma_effects.make_neo4j_statement
      "CREATE (n:Point $props)" [("props", nested)] in
    match stmt with
    | `Assoc fields ->
        (match List.assoc_opt "parameters" fields with
         | Some (`Assoc params) ->
             (match List.assoc_opt "props" params with
              | Some (`Assoc _) -> ()
              | _ -> fail "expected nested assoc")
         | _ -> fail "expected params")
    | _ -> fail "expected assoc"
  in

  (* Null param value *)
  let test_neo4j_statement_null_param () =
    let stmt = Figma_effects.make_neo4j_statement "SET n.val = $val" [("val", `Null)] in
    match stmt with
    | `Assoc fields ->
        (match List.assoc_opt "parameters" fields with
         | Some (`Assoc params) ->
             check bool "null param" true (List.assoc_opt "val" params = Some `Null)
         | _ -> fail "expected params")
    | _ -> fail "expected assoc"
  in

  (* Many params *)
  let test_neo4j_statement_many_params () =
    let params = List.init 20 (fun i ->
      (Printf.sprintf "p%d" i, `Int i)
    ) in
    let stmt = Figma_effects.make_neo4j_statement "RETURN 1" params in
    match stmt with
    | `Assoc fields ->
        (match List.assoc_opt "parameters" fields with
         | Some (`Assoc ps) -> check int "20 params" 20 (List.length ps)
         | _ -> fail "expected params")
    | _ -> fail "expected assoc"
  in

  (* Boolean param *)
  let test_neo4j_statement_bool_param () =
    let stmt = Figma_effects.make_neo4j_statement "SET n.active = $a" [("a", `Bool true)] in
    match stmt with
    | `Assoc fields ->
        (match List.assoc_opt "parameters" fields with
         | Some (`Assoc params) ->
             check bool "bool param" true (List.assoc_opt "a" params = Some (`Bool true))
         | _ -> fail "expected params")
    | _ -> fail "expected assoc"
  in

  (* Float param *)
  let test_neo4j_statement_float_param () =
    let stmt = Figma_effects.make_neo4j_statement "SET n.score = $s" [("s", `Float 0.95)] in
    match stmt with
    | `Assoc fields ->
        (match List.assoc_opt "parameters" fields with
         | Some (`Assoc params) ->
             check bool "float param" true (List.assoc_opt "s" params = Some (`Float 0.95))
         | _ -> fail "expected params")
    | _ -> fail "expected assoc"
  in

  (* ================================================================
     run_with_mock — additional edge cases
     ================================================================ *)

  (* Multiple node IDs in get_nodes key construction *)
  let test_mock_get_nodes_multi_ids () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.nodes "f:1:1,2:2,3:3"
      (`Assoc [("nodes", `Assoc [("1:1", `String "a"); ("2:2", `String "b"); ("3:3", `String "c")])]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_nodes ~token:"tk" ~file_key:"f"
        ~node_ids:["1:1"; "2:2"; "3:3"] ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail (Printf.sprintf "unexpected: %s" e)
  in

  (* Multiple node IDs in get_images *)
  let test_mock_get_images_multi_ids () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.images "f:1:1,2:2"
      (`Assoc [("images", `Assoc [("1:1", `String "url1"); ("2:2", `String "url2")])]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_images ~token:"tk" ~file_key:"f"
        ~node_ids:["1:1"; "2:2"] ~format:"png" ~scale:1.0 ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail (Printf.sprintf "unexpected: %s" e)
  in

  (* Empty node IDs list *)
  let test_mock_get_nodes_empty_ids () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.nodes "f:" (`Assoc [("nodes", `Assoc [])]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_nodes ~token:"tk" ~file_key:"f" ~node_ids:[] ()
    ) in
    match result with
    | Ok _ -> ()
    | Error _ -> ()  (* either is acceptable for empty ids *)
  in

  (* Store overwrite — replacing existing entries *)
  let test_mock_store_overwrite () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc [("name", `String "v1")]);
    let r1 = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f1" ()
    ) in
    (match r1 with
     | Ok json ->
         let name = Yojson.Safe.Util.(json |> member "name" |> to_string) in
         check string "first version" "v1" name
     | Error e -> fail e);
    (* Overwrite *)
    Hashtbl.replace store.files "f1" (`Assoc [("name", `String "v2")]);
    let r2 = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f1" ()
    ) in
    (match r2 with
     | Ok json ->
         let name = Yojson.Safe.Util.(json |> member "name" |> to_string) in
         check string "second version" "v2" name
     | Error e -> fail e)
  in

  (* Store removal — entry exists then is removed *)
  let test_mock_store_remove () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc [("name", `String "x")]);
    let r1 = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f1" ()
    ) in
    check bool "found before remove" true (Result.is_ok r1);
    Hashtbl.remove store.files "f1";
    let r2 = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f1" ()
    ) in
    check bool "missing after remove" true (Result.is_error r2)
  in

  (* me ref toggling *)
  let test_mock_me_toggle () =
    let store = Figma_effects.create_mock_store () in
    let r1 = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_me ~token:"tk"
    ) in
    check bool "me initially none" true (Result.is_error r1);
    store.me := Some (`Assoc [("id", `String "u1")]);
    let r2 = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_me ~token:"tk"
    ) in
    check bool "me set to some" true (Result.is_ok r2);
    store.me := None;
    let r3 = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_me ~token:"tk"
    ) in
    check bool "me reset to none" true (Result.is_error r3)
  in

  (* Chained effect with error propagation *)
  let test_mock_error_propagation () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc [("name", `String "test")]);
    let result = Figma_effects.run_with_mock store (fun () ->
      let f1 = Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f1" () in
      let f2 = Figma_effects.Perform.get_file ~token:"tk" ~file_key:"missing" () in
      Figma_effects.Perform.log_debug "after both calls";
      (f1, f2)
    ) in
    let (f1, f2) = result in
    check bool "f1 ok" true (Result.is_ok f1);
    check bool "f2 error" true (Result.is_error f2)
  in

  (* Long chain of log effects *)
  let test_mock_many_logs () =
    let store = Figma_effects.create_mock_store () in
    Figma_effects.run_with_mock store (fun () ->
      for i = 1 to 50 do
        Figma_effects.Perform.log_debug (Printf.sprintf "debug %d" i);
        Figma_effects.Perform.log_info (Printf.sprintf "info %d" i);
        Figma_effects.Perform.log_error (Printf.sprintf "error %d" i)
      done
    )
  in

  (* Exception in user code after successful effects *)
  let test_mock_exception_after_effect () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc [("name", `String "test")]);
    let raised = ref false in
    (try
       Figma_effects.run_with_mock store (fun () ->
         let _ = Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f1" () in
         Figma_effects.Perform.log_info "about to fail";
         failwith "intentional failure"
       )
     with Failure msg ->
       raised := true;
       check string "correct message" "intentional failure" msg);
    check bool "exception propagated" true !raised
  in

  (* Invalid_argument exception propagation *)
  let test_mock_invalid_arg_exception () =
    let store = Figma_effects.create_mock_store () in
    let raised = ref false in
    (try
       Figma_effects.run_with_mock store (fun () ->
         ignore (String.sub "short" 10 5);
         ()
       )
     with Invalid_argument _ ->
       raised := true);
    check bool "invalid_arg propagated" true !raised
  in

  (* Pure return value types *)
  let test_mock_pure_string () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () -> "hello") in
    check string "pure string" "hello" result
  in

  let test_mock_pure_list () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () -> [1; 2; 3]) in
    check int "list length" 3 (List.length result)
  in

  let test_mock_pure_unit () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () -> ()) in
    check unit "unit" () result
  in

  let test_mock_pure_tuple () =
    let store = Figma_effects.create_mock_store () in
    let (a, b) = Figma_effects.run_with_mock store (fun () -> (42, "x")) in
    check int "fst" 42 a;
    check string "snd" "x" b
  in

  (* All "not-implemented" mock handlers return error with descriptive message *)
  let test_mock_not_impl_messages () =
    let store = Figma_effects.create_mock_store () in
    let assert_error_contains key result =
      match result with
      | Error msg ->
          check bool (Printf.sprintf "%s: has content" key) true (String.length msg > 0)
      | Ok _ -> fail (Printf.sprintf "%s: expected error" key)
    in
    assert_error_contains "file_components"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_file_components ~token:"tk" ~file_key:"f1"));
    assert_error_contains "team_components"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_team_components ~token:"tk" ~team_id:"t1"));
    assert_error_contains "file_component_sets"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_file_component_sets ~token:"tk" ~file_key:"f1"));
    assert_error_contains "team_component_sets"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_team_component_sets ~token:"tk" ~team_id:"t1"));
    assert_error_contains "file_styles"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_file_styles ~token:"tk" ~file_key:"f1"));
    assert_error_contains "team_styles"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_team_styles ~token:"tk" ~team_id:"t1"));
    assert_error_contains "component"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_component ~token:"tk" ~component_key:"c1"));
    assert_error_contains "component_set"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_component_set ~token:"tk" ~component_set_key:"cs1"));
    assert_error_contains "style"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_style ~token:"tk" ~style_key:"s1"));
    assert_error_contains "file_versions"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_file_versions ~token:"tk" ~file_key:"f1"));
    assert_error_contains "file_comments"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_file_comments ~token:"tk" ~file_key:"f1"));
    assert_error_contains "post_comment"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.post_file_comment ~token:"tk" ~file_key:"f1"
          ~message:"hi" ~client_meta:`Null));
    assert_error_contains "download"
      (Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.download_url ~url:"http://x" ~path:"/tmp/x"))
  in

  (* Verify error messages contain the file_key / team_id *)
  let test_mock_error_contains_key () =
    let store = Figma_effects.create_mock_store () in
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"my-special-file" ()
    ) in
    match r with
    | Error msg ->
        check bool "contains file key" true
          (try ignore (Str.search_forward (Str.regexp_string "my-special-file") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected missing file error"
  in

  let test_mock_error_contains_team_id () =
    let store = Figma_effects.create_mock_store () in
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_projects ~token:"tk" ~team_id:"my-team-99"
    ) in
    match r with
    | Error msg ->
        check bool "contains team id" true
          (try ignore (Str.search_forward (Str.regexp_string "my-team-99") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected missing team error"
  in

  let test_mock_error_contains_project_id () =
    let store = Figma_effects.create_mock_store () in
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_project_files ~token:"tk" ~project_id:"proj-ABC"
    ) in
    match r with
    | Error msg ->
        check bool "contains project id" true
          (try ignore (Str.search_forward (Str.regexp_string "proj-ABC") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected missing project error"
  in

  let test_mock_error_contains_variable_key () =
    let store = Figma_effects.create_mock_store () in
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_variables ~token:"tk" ~file_key:"var-file-XYZ"
    ) in
    match r with
    | Error msg ->
        check bool "contains file key" true
          (try ignore (Str.search_forward (Str.regexp_string "var-file-XYZ") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected missing variables error"
  in

  (* ================================================================
     Perform — exhaustive optional arg combinations
     ================================================================ *)

  (* get_file: no optional args *)
  let test_perform_get_file_minimal () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f" (`Assoc []);
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"t" ~file_key:"f" ()
    ) in
    check bool "ok" true (Result.is_ok r)
  in

  (* get_file: only depth *)
  let test_perform_get_file_depth_only () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f" (`Assoc []);
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"t" ~file_key:"f" ~depth:1 ()
    ) in
    check bool "ok" true (Result.is_ok r)
  in

  (* get_file: only geometry *)
  let test_perform_get_file_geometry_only () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f" (`Assoc []);
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"t" ~file_key:"f" ~geometry:"paths" ()
    ) in
    check bool "ok" true (Result.is_ok r)
  in

  (* get_nodes: no optional args *)
  let test_perform_get_nodes_minimal () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.nodes "f:1:1" (`Assoc []);
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_nodes ~token:"t" ~file_key:"f" ~node_ids:["1:1"] ()
    ) in
    check bool "ok" true (Result.is_ok r)
  in

  (* get_images: no optional args *)
  let test_perform_get_images_minimal () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.images "f:1:1" (`Assoc []);
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_images ~token:"t" ~file_key:"f" ~node_ids:["1:1"]
        ~format:"png" ~scale:1.0 ()
    ) in
    check bool "ok" true (Result.is_ok r)
  in

  (* get_images: with use_absolute_bounds only *)
  let test_perform_get_images_abs_bounds () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.images "f:1:1" (`Assoc []);
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_images ~token:"t" ~file_key:"f" ~node_ids:["1:1"]
        ~format:"svg" ~scale:2.0 ~use_absolute_bounds:false ()
    ) in
    check bool "ok" true (Result.is_ok r)
  in

  (* get_file_images: no version *)
  let test_perform_get_file_images_no_version () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.file_images "f" (`Assoc []);
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_images ~token:"t" ~file_key:"f" ()
    ) in
    check bool "ok" true (Result.is_ok r)
  in

  (* get_file_meta: no version *)
  let test_perform_get_file_meta_no_version () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.file_meta "f" (`Assoc []);
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_meta ~token:"t" ~file_key:"f" ()
    ) in
    check bool "ok" true (Result.is_ok r)
  in

  (* ================================================================
     create_mock_store — field isolation
     ================================================================ *)

  (* Two stores are independent *)
  let test_mock_stores_independent () =
    let s1 = Figma_effects.create_mock_store () in
    let s2 = Figma_effects.create_mock_store () in
    Hashtbl.replace s1.files "f1" (`String "store1");
    check int "s2 files empty" 0 (Hashtbl.length s2.files);
    check int "s1 files has 1" 1 (Hashtbl.length s1.files);
    s1.me := Some (`String "me1");
    check bool "s2 me still none" true (!(s2.me) = None)
  in

  (* ================================================================
     Effect interleaving — complex patterns
     ================================================================ *)

  (* Alternate between file reads and logs *)
  let test_interleaved_effects () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc [("n", `String "F1")]);
    Hashtbl.replace store.files "f2" (`Assoc [("n", `String "F2")]);
    store.me := Some (`Assoc [("id", `String "me")]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.log_info "start";
      let r1 = Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f1" () in
      Figma_effects.Perform.log_debug "got f1";
      Figma_effects.Perform.eio_sleep 0.0;
      let me = Figma_effects.Perform.get_me ~token:"tk" in
      Figma_effects.Perform.log_error "test error";
      let r2 = Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f2" () in
      Figma_effects.Perform.log_info "end";
      (r1, me, r2)
    ) in
    let (r1, me, r2) = result in
    check bool "r1 ok" true (Result.is_ok r1);
    check bool "me ok" true (Result.is_ok me);
    check bool "r2 ok" true (Result.is_ok r2)
  in

  (* Effect inside a loop *)
  let test_effects_in_loop () =
    let store = Figma_effects.create_mock_store () in
    for i = 0 to 4 do
      Hashtbl.replace store.files (Printf.sprintf "f%d" i)
        (`Assoc [("i", `Int i)])
    done;
    let results = Figma_effects.run_with_mock store (fun () ->
      List.init 5 (fun i ->
        let key = Printf.sprintf "f%d" i in
        Figma_effects.Perform.log_debug (Printf.sprintf "fetching %s" key);
        Figma_effects.Perform.get_file ~token:"tk" ~file_key:key ()
      )
    ) in
    check int "5 results" 5 (List.length results);
    List.iter (fun r ->
      check bool "each ok" true (Result.is_ok r)
    ) results
  in

  (* Effect result used in subsequent effect *)
  let test_effect_result_chaining () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc [("name", `String "TestFile")]);
    Hashtbl.replace store.nodes "f1:1:1" (`Assoc [("document", `String "node-data")]);
    let result = Figma_effects.run_with_mock store (fun () ->
      match Figma_effects.Perform.get_file ~token:"tk" ~file_key:"f1" () with
      | Ok json ->
          let name = Yojson.Safe.Util.(json |> member "name" |> to_string) in
          Figma_effects.Perform.log_info (Printf.sprintf "File: %s" name);
          let nodes = Figma_effects.Perform.get_nodes ~token:"tk" ~file_key:"f1"
            ~node_ids:["1:1"] () in
          (Ok name, nodes)
      | Error e ->
          Figma_effects.Perform.log_error (Printf.sprintf "Failed: %s" e);
          (Error e, Error "skipped")
    ) in
    let (name_result, nodes_result) = result in
    (match name_result with
     | Ok name -> check string "file name" "TestFile" name
     | Error e -> fail e);
    check bool "nodes ok" true (Result.is_ok nodes_result)
  in

  (* ================================================================
     Perform — functions not yet exercised with specific values
     ================================================================ *)

  (* eio_sleep with zero *)
  let test_perform_sleep_zero () =
    let store = Figma_effects.create_mock_store () in
    Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.eio_sleep 0.0
    )
  in

  (* eio_sleep with large value (still no-op in mock) *)
  let test_perform_sleep_large () =
    let store = Figma_effects.create_mock_store () in
    Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.eio_sleep 999999.0
    )
  in

  (* download_url error message contains path *)
  let test_perform_download_error_path () =
    let store = Figma_effects.create_mock_store () in
    let r = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.download_url ~url:"http://example.com/file.png"
        ~path:"/tmp/my-download.png"
    ) in
    match r with
    | Error msg ->
        check bool "contains path" true
          (try ignore (Str.search_forward (Str.regexp_string "/tmp/my-download.png") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected error"
  in

  run "Figma Effects W6" [
    ("parse_neo4j: generic exception", [
      test_case "JSON array input" `Quick test_parse_response_json_array;
      test_case "JSON string input" `Quick test_parse_response_json_string;
      test_case "JSON number input" `Quick test_parse_response_json_number;
      test_case "JSON boolean input" `Quick test_parse_response_json_bool;
      test_case "JSON null input" `Quick test_parse_response_json_null;
    ]);
    ("parse_neo4j: multi-error", [
      test_case "3 errors" `Quick test_parse_response_multi_errors;
      test_case "mixed empty/non-empty" `Quick test_parse_response_mixed_errors;
      test_case "non-assoc error items" `Quick test_parse_response_errors_non_assoc_items;
      test_case "code only" `Quick test_parse_response_error_code_only;
      test_case "message only" `Quick test_parse_response_error_message_only;
    ]);
    ("parse_neo4j: edge cases", [
      test_case "large results" `Quick test_parse_response_large_results;
      test_case "empty object" `Quick test_parse_response_empty_object;
      test_case "empty string" `Quick test_parse_response_empty_string;
      test_case "truncated JSON" `Quick test_parse_response_truncated;
    ]);
    ("make_neo4j_statement: params", [
      test_case "unicode" `Quick test_neo4j_statement_unicode;
      test_case "nested" `Quick test_neo4j_statement_nested_params;
      test_case "null" `Quick test_neo4j_statement_null_param;
      test_case "many params" `Quick test_neo4j_statement_many_params;
      test_case "bool" `Quick test_neo4j_statement_bool_param;
      test_case "float" `Quick test_neo4j_statement_float_param;
    ]);
    ("mock: multi-id operations", [
      test_case "get_nodes multi ids" `Quick test_mock_get_nodes_multi_ids;
      test_case "get_images multi ids" `Quick test_mock_get_images_multi_ids;
      test_case "get_nodes empty ids" `Quick test_mock_get_nodes_empty_ids;
    ]);
    ("mock: store mutation", [
      test_case "overwrite" `Quick test_mock_store_overwrite;
      test_case "remove" `Quick test_mock_store_remove;
      test_case "me toggle" `Quick test_mock_me_toggle;
      test_case "stores independent" `Quick test_mock_stores_independent;
    ]);
    ("mock: error propagation", [
      test_case "mixed ok/error" `Quick test_mock_error_propagation;
      test_case "many logs" `Quick test_mock_many_logs;
      test_case "exception after effect" `Quick test_mock_exception_after_effect;
      test_case "invalid_arg exception" `Quick test_mock_invalid_arg_exception;
    ]);
    ("mock: pure returns", [
      test_case "string" `Quick test_mock_pure_string;
      test_case "list" `Quick test_mock_pure_list;
      test_case "unit" `Quick test_mock_pure_unit;
      test_case "tuple" `Quick test_mock_pure_tuple;
    ]);
    ("mock: error messages", [
      test_case "not-impl messages" `Quick test_mock_not_impl_messages;
      test_case "error contains file key" `Quick test_mock_error_contains_key;
      test_case "error contains team id" `Quick test_mock_error_contains_team_id;
      test_case "error contains project id" `Quick test_mock_error_contains_project_id;
      test_case "error contains variable key" `Quick test_mock_error_contains_variable_key;
    ]);
    ("perform: optional arg combos", [
      test_case "get_file minimal" `Quick test_perform_get_file_minimal;
      test_case "get_file depth only" `Quick test_perform_get_file_depth_only;
      test_case "get_file geometry only" `Quick test_perform_get_file_geometry_only;
      test_case "get_nodes minimal" `Quick test_perform_get_nodes_minimal;
      test_case "get_images minimal" `Quick test_perform_get_images_minimal;
      test_case "get_images abs bounds" `Quick test_perform_get_images_abs_bounds;
      test_case "get_file_images no ver" `Quick test_perform_get_file_images_no_version;
      test_case "get_file_meta no ver" `Quick test_perform_get_file_meta_no_version;
    ]);
    ("perform: sleep edge", [
      test_case "zero" `Quick test_perform_sleep_zero;
      test_case "large" `Quick test_perform_sleep_large;
    ]);
    ("perform: download", [
      test_case "error has path" `Quick test_perform_download_error_path;
    ]);
    ("effect interleaving", [
      test_case "alternating" `Quick test_interleaved_effects;
      test_case "loop" `Quick test_effects_in_loop;
      test_case "result chaining" `Quick test_effect_result_chaining;
    ]);
  ]
