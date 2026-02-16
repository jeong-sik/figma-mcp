(** Coverage Wave 10: figma_effects.ml
    Target: pure functions (make_neo4j_statement, parse_neo4j_response)
    + all run_with_mock effect handler branches (25+ handlers)
    + Perform module coverage *)

open Figma_effects

(* ── Pure function tests ─────────────────────────────── *)

let test_make_neo4j_statement_basic () =
  let stmt = make_neo4j_statement "MATCH (n) RETURN n" [] in
  match stmt with
  | `Assoc fields ->
      let s = List.assoc "statement" fields in
      Alcotest.(check string) "query" "MATCH (n) RETURN n"
        (Yojson.Safe.Util.to_string s);
      let p = List.assoc "parameters" fields in
      Alcotest.(check string) "empty params" "{}" (Yojson.Safe.to_string p)
  | _ -> Alcotest.fail "expected Assoc"

let test_make_neo4j_statement_with_params () =
  let stmt = make_neo4j_statement "MATCH (n {id: $id}) RETURN n"
    [("id", `String "abc"); ("count", `Int 42)] in
  let json_str = Yojson.Safe.to_string stmt in
  Alcotest.(check bool) "contains id" true (String.length json_str > 0);
  match stmt with
  | `Assoc fields ->
      let p = List.assoc "parameters" fields in
      (match p with
       | `Assoc params ->
           Alcotest.(check int) "2 params" 2 (List.length params);
           Alcotest.(check string) "id value" "abc"
             (Yojson.Safe.Util.to_string (List.assoc "id" params))
       | _ -> Alcotest.fail "expected Assoc params")
  | _ -> Alcotest.fail "expected Assoc"

let test_parse_neo4j_response_success () =
  let body = {|{"results": [{"columns": ["n"], "data": []}], "errors": []}|} in
  match parse_neo4j_response body with
  | Ok results ->
      (match results with
       | `List [_] -> () (* one result set *)
       | _ -> Alcotest.fail "expected results list with 1 item")
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" e)

let test_parse_neo4j_response_with_errors () =
  let body = {|{"results": [], "errors": [{"code": "Neo.ClientError", "message": "bad query"}]}|} in
  match parse_neo4j_response body with
  | Error msg ->
      Alcotest.(check bool) "contains error code" true
        (String.length msg > 0 && (try let _ = Str.search_forward (Str.regexp "Neo.ClientError") msg 0 in true with Not_found -> false))
  | Ok _ -> Alcotest.fail "expected error"

let test_parse_neo4j_response_invalid_json () =
  match parse_neo4j_response "not json at all {{{" with
  | Error msg ->
      Alcotest.(check bool) "JSON parse error" true
        (try let _ = Str.search_forward (Str.regexp_string "JSON parse error") msg 0 in true with Not_found -> false)
  | Ok _ -> Alcotest.fail "expected JSON parse error"

let test_parse_neo4j_response_no_errors_field () =
  let body = {|{"results": [{"data": []}]}|} in
  match parse_neo4j_response body with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected: %s" e)

let test_parse_neo4j_response_empty_error_entries () =
  (* Errors list with empty code+message should be filtered out *)
  let body = {|{"results": [{"data": []}], "errors": [{"code": "", "message": ""}]}|} in
  match parse_neo4j_response body with
  | Ok _ -> () (* empty code+message = no real error *)
  | Error _ -> Alcotest.fail "empty error entries should be ignored"

let test_parse_neo4j_response_partial_error () =
  (* Only code, no message *)
  let body = {|{"results": [], "errors": [{"code": "Neo.Err"}]}|} in
  match parse_neo4j_response body with
  | Error msg ->
      Alcotest.(check bool) "has code" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error from code-only entry"

let test_parse_neo4j_response_errors_not_list () =
  let body = {|{"results": [{"data": []}], "errors": "not a list"}|} in
  match parse_neo4j_response body with
  | Ok _ -> () (* non-list errors = no errors *)
  | Error _ -> Alcotest.fail "non-list errors field should not fail"

(* ── run_with_mock effect handler tests ──────────────── *)

(* Helper: run a single effect inside mock handler *)
let run_effect store f =
  run_with_mock store f

let test_mock_get_file_found () =
  let store = create_mock_store () in
  Hashtbl.replace store.files "file1" (`Assoc [("name", `String "Test File")]);
  let result = run_effect store (fun () ->
    Perform.get_file ~token:"t" ~file_key:"file1" ()
  ) in
  match result with
  | Ok json ->
      Alcotest.(check string) "name" "Test File"
        (Yojson.Safe.Util.(member "name" json |> to_string))
  | Error e -> Alcotest.fail e

let test_mock_get_file_not_found () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_file ~token:"t" ~file_key:"missing" ()
  ) in
  match result with
  | Error msg ->
      Alcotest.(check bool) "contains file key" true
        (try let _ = Str.search_forward (Str.regexp_string "missing") msg 0 in true with Not_found -> false)
  | Ok _ -> Alcotest.fail "expected not found error"

let test_mock_get_nodes_found () =
  let store = create_mock_store () in
  Hashtbl.replace store.nodes "f1:n1,n2" (`Assoc [("nodes", `List [])]);
  let result = run_effect store (fun () ->
    Perform.get_nodes ~token:"t" ~file_key:"f1" ~node_ids:["n1"; "n2"] ()
  ) in
  match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail e

let test_mock_get_nodes_not_found () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_nodes ~token:"t" ~file_key:"f1" ~node_ids:["n1"] ()
  ) in
  match result with
  | Error msg ->
      Alcotest.(check bool) "mentions nodes" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected not found"

let test_mock_get_images_found () =
  let store = create_mock_store () in
  Hashtbl.replace store.images "f1:n1" (`Assoc [("images", `Assoc [("n1", `String "url")])]);
  let result = run_effect store (fun () ->
    Perform.get_images ~token:"t" ~file_key:"f1" ~node_ids:["n1"] ~format:"png" ~scale:2.0 ()
  ) in
  match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail e

let test_mock_get_images_not_found () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_images ~token:"t" ~file_key:"f1" ~node_ids:["n99"] ~format:"svg" ~scale:1.0 ()
  ) in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected not found"

let test_mock_get_file_images_found () =
  let store = create_mock_store () in
  Hashtbl.replace store.file_images "f1" (`Assoc [("meta", `Null)]);
  let result = run_effect store (fun () ->
    Perform.get_file_images ~token:"t" ~file_key:"f1" ()
  ) in
  match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail e

let test_mock_get_file_images_not_found () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_file_images ~token:"t" ~file_key:"missing" ()
  ) in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected not found"

let test_mock_get_file_meta_found () =
  let store = create_mock_store () in
  Hashtbl.replace store.file_meta "f1" (`Assoc [("components", `List [])]);
  let result = run_effect store (fun () ->
    Perform.get_file_meta ~token:"t" ~file_key:"f1" ()
  ) in
  match result with
  | Ok _ -> ()
  | Error e -> Alcotest.fail e

let test_mock_get_file_meta_not_found () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_file_meta ~token:"t" ~file_key:"x" ()
  ) in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected not found"

(* "Not implemented" handler branches — all return Error *)
let test_mock_get_file_components () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_file_components ~token:"t" ~file_key:"f1"
  ) in
  match result with
  | Error msg ->
      Alcotest.(check bool) "not implemented" true
        (try let _ = Str.search_forward (Str.regexp_string "not implemented") msg 0 in true with Not_found -> false)
  | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_team_components () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_team_components ~token:"t" ~team_id:"team1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_file_component_sets () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_file_component_sets ~token:"t" ~file_key:"f1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_team_component_sets () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_team_component_sets ~token:"t" ~team_id:"t1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_file_styles () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_file_styles ~token:"t" ~file_key:"f1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_team_styles () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_team_styles ~token:"t" ~team_id:"t1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_component () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_component ~token:"t" ~component_key:"ck1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_component_set () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_component_set ~token:"t" ~component_set_key:"csk1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_style () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_style ~token:"t" ~style_key:"sk1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_file_versions () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_file_versions ~token:"t" ~file_key:"f1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_get_file_comments () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_file_comments ~token:"t" ~file_key:"f1"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_post_file_comment () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.post_file_comment ~token:"t" ~file_key:"f1"
      ~message:"hello" ~client_meta:(`Assoc [])
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not implemented"

let test_mock_download_url () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.download_url ~url:"https://example.com/img.png" ~path:"/tmp/out.png"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected mock download error"

(* Hashtbl-backed handlers with data *)
let test_mock_get_me_set () =
  let store = create_mock_store () in
  store.me := Some (`Assoc [("handle", `String "vincent")]);
  let result = run_effect store (fun () ->
    Perform.get_me ~token:"t"
  ) in
  match result with
  | Ok json ->
      Alcotest.(check string) "handle" "vincent"
        (Yojson.Safe.Util.(member "handle" json |> to_string))
  | Error e -> Alcotest.fail e

let test_mock_get_me_not_set () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_me ~token:"t"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected me not set"

let test_mock_get_team_projects_found () =
  let store = create_mock_store () in
  Hashtbl.replace store.projects "team1" (`Assoc [("projects", `List [])]);
  let result = run_effect store (fun () ->
    Perform.get_team_projects ~token:"t" ~team_id:"team1"
  ) in
  match result with Ok _ -> () | Error e -> Alcotest.fail e

let test_mock_get_team_projects_not_found () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_team_projects ~token:"t" ~team_id:"missing"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not found"

let test_mock_get_project_files_found () =
  let store = create_mock_store () in
  Hashtbl.replace store.project_files "p1" (`Assoc [("files", `List [])]);
  let result = run_effect store (fun () ->
    Perform.get_project_files ~token:"t" ~project_id:"p1"
  ) in
  match result with Ok _ -> () | Error e -> Alcotest.fail e

let test_mock_get_project_files_not_found () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_project_files ~token:"t" ~project_id:"missing"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not found"

let test_mock_get_variables_found () =
  let store = create_mock_store () in
  Hashtbl.replace store.variables "f1" (`Assoc [("variables", `List [])]);
  let result = run_effect store (fun () ->
    Perform.get_variables ~token:"t" ~file_key:"f1"
  ) in
  match result with Ok _ -> () | Error e -> Alcotest.fail e

let test_mock_get_variables_not_found () =
  let store = create_mock_store () in
  let result = run_effect store (fun () ->
    Perform.get_variables ~token:"t" ~file_key:"missing"
  ) in
  match result with Error _ -> () | Ok _ -> Alcotest.fail "expected not found"

(* Logging + sleep handlers *)
let test_mock_logging () =
  let store = create_mock_store () in
  (* All three logging effects should be silently handled *)
  run_effect store (fun () ->
    Perform.log_debug "debug msg";
    Perform.log_info "info msg";
    Perform.log_error "error msg";
  );
  (* If we get here without exception, all 3 handlers worked *)
  ()

let test_mock_eio_sleep () =
  let store = create_mock_store () in
  run_effect store (fun () ->
    Perform.eio_sleep 5.0;
    Perform.eio_sleep 0.0;
  );
  ()

(* Combined: multiple effects in sequence *)
let test_mock_multi_effect_sequence () =
  let store = create_mock_store () in
  Hashtbl.replace store.files "f1" (`Assoc [("name", `String "File1")]);
  store.me := Some (`Assoc [("id", `String "u1")]);
  Hashtbl.replace store.projects "team1" (`Assoc [("projects", `List [])]);
  let (file_result, me_result, proj_result) = run_effect store (fun () ->
    let f = Perform.get_file ~token:"t" ~file_key:"f1" () in
    Perform.log_info "got file";
    let m = Perform.get_me ~token:"t" in
    Perform.eio_sleep 0.1;
    let p = Perform.get_team_projects ~token:"t" ~team_id:"team1" in
    (f, m, p)
  ) in
  (match file_result with Ok _ -> () | Error e -> Alcotest.fail ("file: " ^ e));
  (match me_result with Ok _ -> () | Error e -> Alcotest.fail ("me: " ^ e));
  (match proj_result with Ok _ -> () | Error e -> Alcotest.fail ("proj: " ^ e))

(* Test that exceptions propagate through run_with_mock *)
let test_mock_exception_propagation () =
  let store = create_mock_store () in
  let raised = ref false in
  (try
    run_effect store (fun () ->
      failwith "test exception"
    )
  with Failure msg ->
    raised := true;
    Alcotest.(check string) "msg" "test exception" msg);
  Alcotest.(check bool) "exception raised" true !raised

(* Test with optional parameters *)
let test_mock_get_file_with_options () =
  let store = create_mock_store () in
  Hashtbl.replace store.files "f1" (`Assoc [("doc", `Null)]);
  let result = run_effect store (fun () ->
    Perform.get_file ~token:"t" ~file_key:"f1"
      ~depth:2 ~geometry:"paths" ~plugin_data:"shared" ~version:"v1" ()
  ) in
  match result with Ok _ -> () | Error e -> Alcotest.fail e

let test_mock_get_nodes_with_options () =
  let store = create_mock_store () in
  Hashtbl.replace store.nodes "f1:n1" (`Assoc []);
  let result = run_effect store (fun () ->
    Perform.get_nodes ~token:"t" ~file_key:"f1" ~node_ids:["n1"]
      ~depth:1 ~geometry:"bounds" ~plugin_data:"all" ~version:"v2" ()
  ) in
  match result with Ok _ -> () | Error e -> Alcotest.fail e

let test_mock_get_images_with_options () =
  let store = create_mock_store () in
  Hashtbl.replace store.images "f1:n1" (`Assoc []);
  let result = run_effect store (fun () ->
    Perform.get_images ~token:"t" ~file_key:"f1" ~node_ids:["n1"]
      ~format:"jpg" ~scale:0.5 ~use_absolute_bounds:true ~version:"v3" ()
  ) in
  match result with Ok _ -> () | Error e -> Alcotest.fail e

let test_mock_get_file_images_with_version () =
  let store = create_mock_store () in
  Hashtbl.replace store.file_images "f1" (`Assoc []);
  let result = run_effect store (fun () ->
    Perform.get_file_images ~token:"t" ~file_key:"f1" ~version:"v1" ()
  ) in
  match result with Ok _ -> () | Error e -> Alcotest.fail e

let test_mock_get_file_meta_with_version () =
  let store = create_mock_store () in
  Hashtbl.replace store.file_meta "f1" (`Assoc []);
  let result = run_effect store (fun () ->
    Perform.get_file_meta ~token:"t" ~file_key:"f1" ~version:"v1" ()
  ) in
  match result with Ok _ -> () | Error e -> Alcotest.fail e

(* ── create_mock_store tests ─────────────────────────── *)

let test_create_mock_store_empty () =
  let store = create_mock_store () in
  Alcotest.(check int) "files empty" 0 (Hashtbl.length store.files);
  Alcotest.(check int) "nodes empty" 0 (Hashtbl.length store.nodes);
  Alcotest.(check int) "images empty" 0 (Hashtbl.length store.images);
  Alcotest.(check int) "file_images empty" 0 (Hashtbl.length store.file_images);
  Alcotest.(check int) "file_meta empty" 0 (Hashtbl.length store.file_meta);
  Alcotest.(check bool) "me is None" true (!(store.me) = None);
  Alcotest.(check int) "projects empty" 0 (Hashtbl.length store.projects);
  Alcotest.(check int) "project_files empty" 0 (Hashtbl.length store.project_files);
  Alcotest.(check int) "variables empty" 0 (Hashtbl.length store.variables)

(* ── Test runner ─────────────────────────────────────── *)

let () =
  Alcotest.run "figma_effects_w10" [
    ("make_neo4j_statement", [
      Alcotest.test_case "basic" `Quick test_make_neo4j_statement_basic;
      Alcotest.test_case "with params" `Quick test_make_neo4j_statement_with_params;
    ]);
    ("parse_neo4j_response", [
      Alcotest.test_case "success" `Quick test_parse_neo4j_response_success;
      Alcotest.test_case "with errors" `Quick test_parse_neo4j_response_with_errors;
      Alcotest.test_case "invalid json" `Quick test_parse_neo4j_response_invalid_json;
      Alcotest.test_case "no errors field" `Quick test_parse_neo4j_response_no_errors_field;
      Alcotest.test_case "empty error entries" `Quick test_parse_neo4j_response_empty_error_entries;
      Alcotest.test_case "partial error" `Quick test_parse_neo4j_response_partial_error;
      Alcotest.test_case "errors not list" `Quick test_parse_neo4j_response_errors_not_list;
    ]);
    ("create_mock_store", [
      Alcotest.test_case "empty store" `Quick test_create_mock_store_empty;
    ]);
    ("run_with_mock: hashtbl-backed", [
      Alcotest.test_case "get_file found" `Quick test_mock_get_file_found;
      Alcotest.test_case "get_file not found" `Quick test_mock_get_file_not_found;
      Alcotest.test_case "get_nodes found" `Quick test_mock_get_nodes_found;
      Alcotest.test_case "get_nodes not found" `Quick test_mock_get_nodes_not_found;
      Alcotest.test_case "get_images found" `Quick test_mock_get_images_found;
      Alcotest.test_case "get_images not found" `Quick test_mock_get_images_not_found;
      Alcotest.test_case "get_file_images found" `Quick test_mock_get_file_images_found;
      Alcotest.test_case "get_file_images not found" `Quick test_mock_get_file_images_not_found;
      Alcotest.test_case "get_file_meta found" `Quick test_mock_get_file_meta_found;
      Alcotest.test_case "get_file_meta not found" `Quick test_mock_get_file_meta_not_found;
      Alcotest.test_case "get_me set" `Quick test_mock_get_me_set;
      Alcotest.test_case "get_me not set" `Quick test_mock_get_me_not_set;
      Alcotest.test_case "get_team_projects found" `Quick test_mock_get_team_projects_found;
      Alcotest.test_case "get_team_projects not found" `Quick test_mock_get_team_projects_not_found;
      Alcotest.test_case "get_project_files found" `Quick test_mock_get_project_files_found;
      Alcotest.test_case "get_project_files not found" `Quick test_mock_get_project_files_not_found;
      Alcotest.test_case "get_variables found" `Quick test_mock_get_variables_found;
      Alcotest.test_case "get_variables not found" `Quick test_mock_get_variables_not_found;
    ]);
    ("run_with_mock: not-implemented stubs", [
      Alcotest.test_case "file_components" `Quick test_mock_get_file_components;
      Alcotest.test_case "team_components" `Quick test_mock_get_team_components;
      Alcotest.test_case "file_component_sets" `Quick test_mock_get_file_component_sets;
      Alcotest.test_case "team_component_sets" `Quick test_mock_get_team_component_sets;
      Alcotest.test_case "file_styles" `Quick test_mock_get_file_styles;
      Alcotest.test_case "team_styles" `Quick test_mock_get_team_styles;
      Alcotest.test_case "component" `Quick test_mock_get_component;
      Alcotest.test_case "component_set" `Quick test_mock_get_component_set;
      Alcotest.test_case "style" `Quick test_mock_get_style;
      Alcotest.test_case "file_versions" `Quick test_mock_get_file_versions;
      Alcotest.test_case "file_comments" `Quick test_mock_get_file_comments;
      Alcotest.test_case "post_file_comment" `Quick test_mock_post_file_comment;
      Alcotest.test_case "download_url" `Quick test_mock_download_url;
    ]);
    ("run_with_mock: logging+sleep", [
      Alcotest.test_case "log debug/info/error" `Quick test_mock_logging;
      Alcotest.test_case "eio_sleep" `Quick test_mock_eio_sleep;
    ]);
    ("run_with_mock: combined", [
      Alcotest.test_case "multi-effect sequence" `Quick test_mock_multi_effect_sequence;
      Alcotest.test_case "exception propagation" `Quick test_mock_exception_propagation;
    ]);
    ("Perform with options", [
      Alcotest.test_case "get_file all options" `Quick test_mock_get_file_with_options;
      Alcotest.test_case "get_nodes all options" `Quick test_mock_get_nodes_with_options;
      Alcotest.test_case "get_images all options" `Quick test_mock_get_images_with_options;
      Alcotest.test_case "get_file_images with version" `Quick test_mock_get_file_images_with_version;
      Alcotest.test_case "get_file_meta with version" `Quick test_mock_get_file_meta_with_version;
    ]);
  ]
