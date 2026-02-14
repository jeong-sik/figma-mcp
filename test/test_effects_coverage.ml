(** Coverage tests for figma_effects.ml — mock handler + Perform module.
    Tests all effect handler branches in run_with_mock. *)

let () =
  let open Alcotest in
  let store = Figma_effects.create_mock_store () in

  (* Seed mock data *)
  Hashtbl.replace store.files "test-file" (`Assoc [("name", `String "Test File")]);
  Hashtbl.replace store.nodes "test-file:1:1" (`Assoc [("document", `String "node-data")]);
  Hashtbl.replace store.images "test-file:1:1" (`Assoc [("images", `Assoc [("1:1", `String "https://img.url")])]);
  Hashtbl.replace store.file_images "test-file" (`Assoc [("meta", `Assoc [("images", `Assoc [])])]);
  Hashtbl.replace store.file_meta "test-file" (`Assoc [("name", `String "Test"); ("version", `String "123")]);
  Hashtbl.replace store.projects "team-1" (`Assoc [("projects", `List [])]);
  Hashtbl.replace store.project_files "proj-1" (`Assoc [("files", `List [])]);
  Hashtbl.replace store.variables "test-file" (`Assoc [("variables", `Assoc [])]);
  store.me := Some (`Assoc [("id", `String "user-1"); ("handle", `String "test")]);

  (* === Perform + run_with_mock: file found === *)
  let test_get_file_ok () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"test-file" ()
    ) in
    match result with
    | Ok json ->
        let name = Yojson.Safe.Util.(json |> member "name" |> to_string) in
        check string "file name" "Test File" name
    | Error e -> fail (Printf.sprintf "unexpected error: %s" e)
  in

  (* === file not found === *)
  let test_get_file_missing () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"nonexistent" ()
    ) in
    match result with
    | Error msg -> check bool "has error" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* === get_nodes === *)
  let test_get_nodes_ok () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_nodes ~token:"tk" ~file_key:"test-file" ~node_ids:["1:1"] ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_nodes_missing () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_nodes ~token:"tk" ~file_key:"test-file" ~node_ids:["9:9"] ()
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* === get_images === *)
  let test_get_images_ok () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_images ~token:"tk" ~file_key:"test-file" ~node_ids:["1:1"]
        ~format:"png" ~scale:2.0 ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_images_missing () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_images ~token:"tk" ~file_key:"test-file" ~node_ids:["9:9"]
        ~format:"png" ~scale:1.0 ()
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* === get_file_images === *)
  let test_get_file_images_ok () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_images ~token:"tk" ~file_key:"test-file" ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_file_images_missing () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_images ~token:"tk" ~file_key:"missing" ()
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* === get_file_meta === *)
  let test_get_file_meta_ok () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_meta ~token:"tk" ~file_key:"test-file" ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_file_meta_missing () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_meta ~token:"tk" ~file_key:"missing" ()
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* === not-implemented handlers — triggers the error branches === *)
  let test_get_file_components () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_components ~token:"tk" ~file_key:"test-file"
    ) in
    match result with
    | Error msg -> check bool "not implemented" true (String.length msg > 0)
    | Ok _ -> fail "expected not-implemented error"
  in
  let test_get_team_components () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_components ~token:"tk" ~team_id:"team-1"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_get_file_component_sets () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_component_sets ~token:"tk" ~file_key:"test-file"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_get_team_component_sets () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_component_sets ~token:"tk" ~team_id:"team-1"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_get_file_styles () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_styles ~token:"tk" ~file_key:"test-file"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_get_team_styles () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_styles ~token:"tk" ~team_id:"team-1"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_get_component () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_component ~token:"tk" ~component_key:"comp-1"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_get_component_set () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_component_set ~token:"tk" ~component_set_key:"cset-1"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_get_style () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_style ~token:"tk" ~style_key:"style-1"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_get_file_versions () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_versions ~token:"tk" ~file_key:"test-file"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_get_file_comments () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_comments ~token:"tk" ~file_key:"test-file"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_post_file_comment () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.post_file_comment ~token:"tk" ~file_key:"test-file"
        ~message:"hello" ~client_meta:`Null
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in
  let test_download_url () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.download_url ~url:"https://example.com/img.png" ~path:"/tmp/test.png"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* === get_me === *)
  let test_get_me_ok () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_me ~token:"tk"
    ) in
    match result with
    | Ok json ->
        let handle = Yojson.Safe.Util.(json |> member "handle" |> to_string) in
        check string "handle" "test" handle
    | Error e -> fail e
  in
  let test_get_me_not_set () =
    let store2 = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store2 (fun () ->
      Figma_effects.Perform.get_me ~token:"tk"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* === get_team_projects === *)
  let test_get_team_projects_ok () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_projects ~token:"tk" ~team_id:"team-1"
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_team_projects_missing () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_team_projects ~token:"tk" ~team_id:"missing"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* === get_project_files === *)
  let test_get_project_files_ok () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_project_files ~token:"tk" ~project_id:"proj-1"
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_project_files_missing () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_project_files ~token:"tk" ~project_id:"missing"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* === get_variables === *)
  let test_get_variables_ok () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_variables ~token:"tk" ~file_key:"test-file"
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_variables_missing () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_variables ~token:"tk" ~file_key:"missing"
    ) in
    match result with
    | Error _ -> ()
    | Ok _ -> fail "expected error"
  in

  (* === Log effects (silent in tests) === *)
  let test_log_debug () =
    Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.log_debug "test debug message"
    );
    (* No assertion needed — just verifies the handler doesn't crash *)
    ()
  in
  let test_log_info () =
    Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.log_info "test info message"
    );
    ()
  in

  (* === log_error (not previously tested) === *)
  let test_log_error () =
    Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.log_error "test error message"
    );
    ()
  in

  (* === eio_sleep (no-op in mock) === *)
  let test_eio_sleep () =
    Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.eio_sleep 1.0
    );
    ()
  in

  (* === get_dev_resources (not in run_with_mock, exercises _ -> None branch) === *)
  let test_get_dev_resources () =
    let raised = ref false in
    (try
      let _result = Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.get_dev_resources ~token:"tk" ~file_key:"test-file" ~node_id:"1:1"
      ) in ()
    with _ -> raised := true);
    check bool "dev_resources unhandled in mock" true !raised
  in

  (* === add_dev_resource (not in run_with_mock) === *)
  let test_add_dev_resource () =
    let raised = ref false in
    (try
      let _result = Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.add_dev_resource ~token:"tk" ~file_key:"test-file"
          ~node_id:"1:1" ~name:"link" ~url:"https://example.com"
      ) in ()
    with _ -> raised := true);
    check bool "add_dev_resource unhandled in mock" true !raised
  in

  (* === create_webhook (not in run_with_mock) === *)
  let test_create_webhook () =
    let raised = ref false in
    (try
      let _result = Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.create_webhook ~token:"tk" ~team_id:"team-1"
          ~file_key:"test-file" ~endpoint:"https://hook.example.com" ~passcode:"secret"
      ) in ()
    with _ -> raised := true);
    check bool "create_webhook unhandled in mock" true !raised
  in

  (* === neo4j_run_cypher (mock has no Neo4j handler, falls through to _ -> None) === *)
  let test_neo4j_run_cypher_unhandled () =
    (* Neo4j effects are NOT handled in run_with_mock, so they go to _ -> None
       and raise Unhandled. We catch that. *)
    let raised = ref false in
    (try
      let _result = Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.neo4j_run_cypher ~uri:"bolt://localhost"
          ~database:"neo4j" ~auth_header:"Basic xxx" ~query:"RETURN 1" ~params:[]
      ) in
      ()
    with _ -> raised := true);
    check bool "neo4j unhandled in mock" true !raised
  in

  (* === neo4j_run_batch (also unhandled in mock) === *)
  let test_neo4j_run_batch_unhandled () =
    let raised = ref false in
    (try
      let _result = Figma_effects.run_with_mock store (fun () ->
        Figma_effects.Perform.neo4j_run_batch ~uri:"bolt://localhost"
          ~database:"neo4j" ~auth_header:"Basic xxx" ~queries:[("RETURN 1", [])]
      ) in
      ()
    with _ -> raised := true);
    check bool "neo4j batch unhandled in mock" true !raised
  in

  (* === make_neo4j_statement === *)
  let test_make_neo4j_statement () =
    let stmt = Figma_effects.make_neo4j_statement "MATCH (n) RETURN n" [("limit", `Int 10)] in
    match stmt with
    | `Assoc fields ->
        let q = List.assoc_opt "statement" fields in
        let p = List.assoc_opt "parameters" fields in
        check bool "has statement" true (q = Some (`String "MATCH (n) RETURN n"));
        (match p with
         | Some (`Assoc params) ->
             check bool "has limit param" true (List.assoc_opt "limit" params = Some (`Int 10))
         | _ -> fail "expected params assoc")
    | _ -> fail "expected assoc"
  in
  let test_make_neo4j_statement_empty_params () =
    let stmt = Figma_effects.make_neo4j_statement "RETURN 1" [] in
    match stmt with
    | `Assoc fields ->
        check bool "has statement" true (List.assoc_opt "statement" fields = Some (`String "RETURN 1"));
        check bool "has empty params" true (List.assoc_opt "parameters" fields = Some (`Assoc []))
    | _ -> fail "expected assoc"
  in

  (* === parse_neo4j_response === *)
  let test_parse_neo4j_ok () =
    let body = {|{"results": [{"columns": ["n"], "data": []}], "errors": []}|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Ok (`List _) -> ()
    | Ok _ -> ()
    | Error e -> fail (Printf.sprintf "expected ok: %s" e)
  in
  let test_parse_neo4j_errors () =
    let body = {|{"results": [], "errors": [{"code": "Neo.Error", "message": "bad query"}]}|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "contains error code" true
          (try let _ = Str.search_forward (Str.regexp_string "Neo.Error") msg 0 in true
           with Not_found -> false)
    | Ok _ -> fail "expected error"
  in
  let test_parse_neo4j_invalid_json () =
    let body = "not json at all" in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Error msg ->
        check bool "json parse error" true
          (try let _ = Str.search_forward (Str.regexp_string "JSON parse error") msg 0 in true
           with Not_found -> false)
    | Ok _ -> fail "expected error"
  in
  let test_parse_neo4j_empty_errors () =
    let body = {|{"results": [{"data": []}], "errors": []}|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_parse_neo4j_error_empty_code_msg () =
    (* errors list with empty code and message should be filtered out *)
    let body = {|{"results": [{"data": []}], "errors": [{"code": "", "message": ""}]}|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Ok _ -> ()
    | Error _ -> fail "empty code+msg should be filtered"
  in
  let test_parse_neo4j_no_errors_field () =
    (* errors field is not a list *)
    let body = {|{"results": [{"data": []}], "errors": "none"}|} in
    let result = Figma_effects.parse_neo4j_response body in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* === Perform with optional args covering more match arms === *)
  let test_get_file_with_all_options () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file ~token:"tk" ~file_key:"test-file"
        ~depth:3 ~geometry:"paths" ~plugin_data:"shared" ~version:"123" ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_nodes_with_all_options () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_nodes ~token:"tk" ~file_key:"test-file" ~node_ids:["1:1"]
        ~depth:2 ~geometry:"bounds" ~plugin_data:"all" ~version:"456" ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_images_with_options () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_images ~token:"tk" ~file_key:"test-file" ~node_ids:["1:1"]
        ~format:"svg" ~scale:4.0 ~use_absolute_bounds:true ~version:"789" ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_file_images_with_version () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_images ~token:"tk" ~file_key:"test-file" ~version:"v1" ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in
  let test_get_file_meta_with_version () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_file_meta ~token:"tk" ~file_key:"test-file" ~version:"v1" ()
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  (* === create_mock_store covers creation of all Hashtbl fields === *)
  let test_create_mock_store () =
    let s = Figma_effects.create_mock_store () in
    check int "files empty" 0 (Hashtbl.length s.files);
    check int "nodes empty" 0 (Hashtbl.length s.nodes);
    check int "images empty" 0 (Hashtbl.length s.images);
    check int "file_images empty" 0 (Hashtbl.length s.file_images);
    check int "file_meta empty" 0 (Hashtbl.length s.file_meta);
    check bool "me is None" true (!(s.me) = None);
    check int "projects empty" 0 (Hashtbl.length s.projects);
    check int "project_files empty" 0 (Hashtbl.length s.project_files);
    check int "variables empty" 0 (Hashtbl.length s.variables)
  in

  (* === Multiple effects in sequence (exercising continuation chaining) === *)
  let test_chained_effects () =
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.log_info "starting chain";
      let file_result = Figma_effects.Perform.get_file ~token:"tk" ~file_key:"test-file" () in
      Figma_effects.Perform.log_debug "got file";
      Figma_effects.Perform.eio_sleep 0.0;
      file_result
    ) in
    match result with
    | Ok _ -> ()
    | Error e -> fail e
  in

  run "Figma_effects Coverage" [
    ("get_file", [
      test_case "ok" `Quick test_get_file_ok;
      test_case "missing" `Quick test_get_file_missing;
      test_case "with all options" `Quick test_get_file_with_all_options;
    ]);
    ("get_nodes", [
      test_case "ok" `Quick test_get_nodes_ok;
      test_case "missing" `Quick test_get_nodes_missing;
      test_case "with all options" `Quick test_get_nodes_with_all_options;
    ]);
    ("get_images", [
      test_case "ok" `Quick test_get_images_ok;
      test_case "missing" `Quick test_get_images_missing;
      test_case "with options" `Quick test_get_images_with_options;
    ]);
    ("get_file_images", [
      test_case "ok" `Quick test_get_file_images_ok;
      test_case "missing" `Quick test_get_file_images_missing;
      test_case "with version" `Quick test_get_file_images_with_version;
    ]);
    ("get_file_meta", [
      test_case "ok" `Quick test_get_file_meta_ok;
      test_case "missing" `Quick test_get_file_meta_missing;
      test_case "with version" `Quick test_get_file_meta_with_version;
    ]);
    ("not-implemented", [
      test_case "file_components" `Quick test_get_file_components;
      test_case "team_components" `Quick test_get_team_components;
      test_case "file_component_sets" `Quick test_get_file_component_sets;
      test_case "team_component_sets" `Quick test_get_team_component_sets;
      test_case "file_styles" `Quick test_get_file_styles;
      test_case "team_styles" `Quick test_get_team_styles;
      test_case "component" `Quick test_get_component;
      test_case "component_set" `Quick test_get_component_set;
      test_case "style" `Quick test_get_style;
      test_case "file_versions" `Quick test_get_file_versions;
      test_case "file_comments" `Quick test_get_file_comments;
      test_case "post_comment" `Quick test_post_file_comment;
      test_case "download_url" `Quick test_download_url;
      test_case "dev_resources" `Quick test_get_dev_resources;
      test_case "add_dev_resource" `Quick test_add_dev_resource;
      test_case "create_webhook" `Quick test_create_webhook;
    ]);
    ("get_me", [
      test_case "ok" `Quick test_get_me_ok;
      test_case "not set" `Quick test_get_me_not_set;
    ]);
    ("get_team_projects", [
      test_case "ok" `Quick test_get_team_projects_ok;
      test_case "missing" `Quick test_get_team_projects_missing;
    ]);
    ("get_project_files", [
      test_case "ok" `Quick test_get_project_files_ok;
      test_case "missing" `Quick test_get_project_files_missing;
    ]);
    ("get_variables", [
      test_case "ok" `Quick test_get_variables_ok;
      test_case "missing" `Quick test_get_variables_missing;
    ]);
    ("log_effects", [
      test_case "debug" `Quick test_log_debug;
      test_case "info" `Quick test_log_info;
      test_case "error" `Quick test_log_error;
    ]);
    ("sleep", [
      test_case "eio_sleep" `Quick test_eio_sleep;
    ]);
    ("neo4j_mock", [
      test_case "cypher unhandled" `Quick test_neo4j_run_cypher_unhandled;
      test_case "batch unhandled" `Quick test_neo4j_run_batch_unhandled;
    ]);
    ("make_neo4j_statement", [
      test_case "with params" `Quick test_make_neo4j_statement;
      test_case "empty params" `Quick test_make_neo4j_statement_empty_params;
    ]);
    ("parse_neo4j_response", [
      test_case "ok" `Quick test_parse_neo4j_ok;
      test_case "errors" `Quick test_parse_neo4j_errors;
      test_case "invalid json" `Quick test_parse_neo4j_invalid_json;
      test_case "empty errors" `Quick test_parse_neo4j_empty_errors;
      test_case "empty code+msg" `Quick test_parse_neo4j_error_empty_code_msg;
      test_case "no errors field" `Quick test_parse_neo4j_no_errors_field;
    ]);
    ("create_mock_store", [
      test_case "all fields" `Quick test_create_mock_store;
    ]);
    ("chained_effects", [
      test_case "sequential" `Quick test_chained_effects;
    ]);
  ]
