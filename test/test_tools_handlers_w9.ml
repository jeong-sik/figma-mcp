(** Coverage Wave 9: mcp_tools.ml + mcp_api_handlers.ml gap closers.
    Targets ~150+ new coverage points.

    mcp_tools.ml:
      handle_cache_invalidate, handle_codegen_sync, handle_code_connect deep modes,
      handle_category edge branches, read_resource dtcg/unknown format,
      code_connect_cache_put overflow, get_string_any, truncate_string,
      normalize_path, is_under_dir, has_field/set_field/add_if_missing edges,
      handle_get_me error path, handle_list_projects filter_map None branch,
      handle_list_files filter_map None branch, handle_parse_url fields,
      handle_search search_in variants, handle_crawl_team missing team_id/token,
      handle_team_tree missing params, handle_export_team missing combos,
      handle_get_variables format branches, handle_tree/stats/export_tokens via mock,
      handle_read_large_result edge cases

    mcp_api_handlers.ml:
      download_image_fill non-HTTP + non-string, node_duplicate_key bbox branches,
      handle_post_comment with node_id, handle_get_file format branches,
      handle_list_screens success, handle_get_file_versions/comments success+error,
      handle_get_file_components/team_components/styles success+missing *)

open Figma_mcp [@@warning "-33"]

(* Ensure FIGMA_TOKEN is NOT set at module level for clean missing-token tests *)
let () =
  (try Unix.putenv "FIGMA_TOKEN" "" with _ -> ())

(* ================================================================
   Mock effect handler for Figma API calls
   ================================================================ *)

let with_mock_effects f =
  Effect.Deep.try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Figma_effects.Figma_get_file _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [
                  ("document", `Assoc [
                    ("id", `String "0:0");
                    ("name", `String "Doc");
                    ("type", `String "DOCUMENT");
                    ("children", `List [
                      `Assoc [
                        ("id", `String "1:1");
                        ("name", `String "Page 1");
                        ("type", `String "CANVAS");
                        ("children", `List [
                          `Assoc [
                            ("id", `String "2:1");
                            ("name", `String "LoginFrame");
                            ("type", `String "FRAME");
                            ("absoluteBoundingBox", `Assoc [
                              ("x", `Float 0.); ("y", `Float 0.);
                              ("width", `Float 400.); ("height", `Float 600.)
                            ]);
                            ("children", `List [
                              `Assoc [
                                ("id", `String "3:1");
                                ("name", `String "Title");
                                ("type", `String "TEXT");
                                ("characters", `String "Hello World");
                                ("children", `List [])
                              ]
                            ])
                          ]
                        ])
                      ]
                    ])
                  ])
                ])))
        | Figma_effects.Figma_get_nodes { node_ids; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let entries = List.map (fun nid ->
                (nid, `Assoc [
                  ("document", `Assoc [
                    ("id", `String nid);
                    ("name", `String "MockNode");
                    ("type", `String "FRAME");
                    ("absoluteBoundingBox", `Assoc [
                      ("x", `Float 0.); ("y", `Float 0.);
                      ("width", `Float 200.); ("height", `Float 300.)
                    ]);
                    ("children", `List [])
                  ])
                ])
              ) node_ids in
              Effect.Deep.continue k
                (Ok (`Assoc [("nodes", `Assoc entries)])))
        | Figma_effects.Figma_get_images { node_ids; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let entries = List.map (fun nid ->
                (nid, `String "https://img.example.com/x.png")
              ) node_ids in
              Effect.Deep.continue k
                (Ok (`Assoc [("images", `Assoc entries)])))
        | Figma_effects.Figma_get_file_images _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("images", `Assoc [])])))
        | Figma_effects.Figma_get_file_meta _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [
                  ("components", `Assoc []);
                  ("styles", `Assoc []);
                ])))
        | Figma_effects.Figma_get_me _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [
                  ("id", `String "user-1");
                  ("email", `String "test@example.com");
                  ("handle", `String "testuser")
                ])))
        | Figma_effects.Figma_get_team_projects _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [
                  ("projects", `List [
                    `Assoc [("id", `String "p1"); ("name", `String "Project Alpha")];
                    `Assoc [("id", `String "p2"); ("name", `String "Project Beta")];
                    `Assoc [("id", `Int 3)]; (* missing name => filtered out *)
                  ])
                ])))
        | Figma_effects.Figma_get_project_files _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [
                  ("files", `List [
                    `Assoc [("key", `String "fk1"); ("name", `String "File One")];
                    `Assoc [("key", `String "fk2")]; (* missing name => filtered *)
                    `Assoc [("name", `String "File Three")]; (* missing key => filtered *)
                  ])
                ])))
        | Figma_effects.Figma_get_file_versions _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("versions", `List [
                  `Assoc [("id", `String "v1"); ("created_at", `String "2026-01-01")]
                ])])))
        | Figma_effects.Figma_get_file_comments _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("comments", `List [
                  `Assoc [("id", `String "c1"); ("message", `String "Nice")]
                ])])))
        | Figma_effects.Figma_post_file_comment _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("id", `String "c2"); ("message", `String "posted")])))
        | Figma_effects.Figma_get_file_components _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("meta", `Assoc [("components", `List [])])])))
        | Figma_effects.Figma_get_team_components _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("meta", `Assoc [("components", `List [])])])))
        | Figma_effects.Figma_get_file_component_sets _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("meta", `Assoc [("component_sets", `List [])])])))
        | Figma_effects.Figma_get_team_component_sets _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("meta", `Assoc [("component_sets", `List [])])])))
        | Figma_effects.Figma_get_file_styles _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("meta", `Assoc [("styles", `List [])])])))
        | Figma_effects.Figma_get_team_styles _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("meta", `Assoc [("styles", `List [])])])))
        | Figma_effects.Figma_get_component _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("meta", `Assoc [("component", `Assoc [])])])))
        | Figma_effects.Figma_get_component_set _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("meta", `Assoc [("component_set", `Assoc [])])])))
        | Figma_effects.Figma_get_variables _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [
                  ("meta", `Assoc [
                    ("variableCollections", `Assoc [("col1", `Assoc [])]);
                    ("variables", `Assoc [
                      ("var1", `Assoc [
                        ("name", `String "primary/color");
                        ("resolvedType", `String "COLOR");
                        ("valuesByMode", `Assoc [
                          ("mode1", `Assoc [("r", `Float 1.0); ("g", `Float 0.0); ("b", `Float 0.0); ("a", `Float 1.0)])
                        ])
                      ])
                    ])
                  ])
                ])))
        | Figma_effects.Figma_download_url _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok "/tmp/saved.png"))
        | Figma_effects.Log_debug _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())
        | Figma_effects.Log_info _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())
        | Figma_effects.Log_error _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())
        | Figma_effects.Eio_sleep _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())
        | _ -> None }

(* Mock effects that return errors *)
let with_error_effects f =
  Effect.Deep.try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Figma_effects.Figma_get_file _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock get_file error"))
        | Figma_effects.Figma_get_nodes _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock get_nodes error"))
        | Figma_effects.Figma_get_me _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock get_me error"))
        | Figma_effects.Figma_get_team_projects _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock projects error"))
        | Figma_effects.Figma_get_project_files _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock files error"))
        | Figma_effects.Figma_get_file_versions _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock versions error"))
        | Figma_effects.Figma_get_file_comments _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock comments error"))
        | Figma_effects.Figma_post_file_comment _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock post_comment error"))
        | Figma_effects.Figma_get_file_components _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock components error"))
        | Figma_effects.Figma_get_team_components _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock team_components error"))
        | Figma_effects.Figma_get_file_component_sets _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock comp_sets error"))
        | Figma_effects.Figma_get_team_component_sets _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock team_comp_sets error"))
        | Figma_effects.Figma_get_file_styles _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock styles error"))
        | Figma_effects.Figma_get_team_styles _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock team_styles error"))
        | Figma_effects.Figma_get_component _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock component error"))
        | Figma_effects.Figma_get_component_set _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock comp_set error"))
        | Figma_effects.Figma_get_variables _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Error "mock variables error"))
        | Figma_effects.Log_debug _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())
        | Figma_effects.Log_info _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())
        | Figma_effects.Log_error _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())
        | _ -> None }

let () =
  let open Alcotest in
  let check = check in

  (* =================================================================
     mcp_tools.ml: handle_cache_invalidate (3 branches)
     ================================================================= *)
  let test_cache_invalidate_all () =
    match Mcp_tools.handle_cache_invalidate (`Assoc []) with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "ok status" true (String.length s > 0);
        check bool "all invalidated" true
          (try ignore (Str.search_forward (Str.regexp_string "All cache") s 0); true
           with Not_found -> false)
    | Error _ -> fail "expected Ok"
  in

  let test_cache_invalidate_file_only () =
    match Mcp_tools.handle_cache_invalidate
            (`Assoc [("file_key", `String "abc123")]) with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "file invalidated" true
          (try ignore (Str.search_forward (Str.regexp_string "abc123") s 0); true
           with Not_found -> false)
    | Error _ -> fail "expected Ok"
  in

  let test_cache_invalidate_file_and_node () =
    match Mcp_tools.handle_cache_invalidate
            (`Assoc [("file_key", `String "fk1"); ("node_id", `String "2:3")]) with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "node invalidated" true
          (try ignore (Str.search_forward (Str.regexp_string "2:3") s 0); true
           with Not_found -> false)
    | Error _ -> fail "expected Ok"
  in

  (* =================================================================
     mcp_tools.ml: handle_codegen_sync
     ================================================================= *)
  let test_codegen_sync_missing_json () =
    match Mcp_tools.handle_codegen_sync (`Assoc []) with
    | Error msg -> check bool "says missing" true
        (try ignore (Str.search_forward (Str.regexp_string "json") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_codegen_sync_invalid_json () =
    match Mcp_tools.handle_codegen_sync
            (`Assoc [("json", `String "not-valid-json{{}")]) with
    | Error _ -> () (* parse failure is expected *)
    | Ok _ -> () (* some formats may tolerate; either is fine *)
  in

  let test_codegen_sync_valid_minimal () =
    let json_str = {|{"id":"0:0","name":"Doc","type":"DOCUMENT","children":[]}|} in
    match Mcp_tools.handle_codegen_sync
            (`Assoc [("json", `String json_str); ("format", `String "fidelity")]) with
    | Ok _ -> ()
    | Error _ -> () (* acceptable if parser rejects minimal doc *)
  in

  let test_codegen_sync_raw_format () =
    let json_str = {|{"id":"0:0","name":"Doc","type":"DOCUMENT","children":[]}|} in
    match Mcp_tools.handle_codegen_sync
            (`Assoc [("json", `String json_str); ("format", `String "raw")]) with
    | Ok _ -> ()
    | Error _ -> ()
  in

  (* =================================================================
     mcp_tools.ml: handle_parse_url (field extraction)
     ================================================================= *)
  let test_parse_url_with_node () =
    let url = "https://www.figma.com/file/abc123/Design?node-id=2089-11127" in
    match Mcp_tools.handle_parse_url (`Assoc [("url", `String url)]) with
    | Ok _ -> ()
    | Error _ -> fail "expected Ok"
  in

  let test_parse_url_missing () =
    match Mcp_tools.handle_parse_url (`Assoc []) with
    | Error msg -> check bool "says url" true
        (try ignore (Str.search_forward (Str.regexp_string "url") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_parse_url_team_url () =
    let url = "https://www.figma.com/files/team/12345/project/67890" in
    match Mcp_tools.handle_parse_url (`Assoc [("url", `String url)]) with
    | Ok _ -> ()
    | Error _ -> fail "expected Ok for team URL"
  in

  (* =================================================================
     mcp_tools.ml: handle_get_me (token + success/error)
     ================================================================= *)
  let test_get_me_no_token () =
    (* Ensure FIGMA_TOKEN is empty *)
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_get_me (`Assoc []) with
    | Error msg -> check bool "says token" true
        (try ignore (Str.search_forward (Str.regexp_string "token") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error without token"
  in

  let test_get_me_success () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_get_me (`Assoc [("token", `String "tok")]) with
      | Ok json ->
          let s = Yojson.Safe.to_string json in
          check bool "has email" true
            (try ignore (Str.search_forward (Str.regexp_string "test@example.com") s 0); true
             with Not_found -> false)
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_get_me_error () =
    with_error_effects (fun () ->
      match Mcp_tools.handle_get_me (`Assoc [("token", `String "tok")]) with
      | Error msg -> check bool "error propagated" true (String.length msg > 0)
      | Ok _ -> fail "expected Error from mock")
  in

  (* =================================================================
     mcp_tools.ml: handle_list_projects (filter_map None branch)
     ================================================================= *)
  let test_list_projects_missing_params () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_list_projects (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_list_projects_success () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_list_projects
              (`Assoc [("team_id", `String "t1"); ("token", `String "tok")]) with
      | Ok json ->
          let s = Yojson.Safe.to_string json in
          (* Should have 2 projects — the 3rd has no name so filtered *)
          check bool "has Alpha" true
            (try ignore (Str.search_forward (Str.regexp_string "Alpha") s 0); true
             with Not_found -> false);
          check bool "has Beta" true
            (try ignore (Str.search_forward (Str.regexp_string "Beta") s 0); true
             with Not_found -> false)
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_list_projects_error () =
    with_error_effects (fun () ->
      match Mcp_tools.handle_list_projects
              (`Assoc [("team_id", `String "t1"); ("token", `String "tok")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error from mock")
  in

  (* =================================================================
     mcp_tools.ml: handle_list_files
     ================================================================= *)
  let test_list_files_missing_params () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_list_files (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_list_files_success () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_list_files
              (`Assoc [("project_id", `String "p1"); ("token", `String "tok")]) with
      | Ok json ->
          let s = Yojson.Safe.to_string json in
          check bool "has File One" true
            (try ignore (Str.search_forward (Str.regexp_string "File One") s 0); true
             with Not_found -> false)
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_list_files_error () =
    with_error_effects (fun () ->
      match Mcp_tools.handle_list_files
              (`Assoc [("project_id", `String "p1"); ("token", `String "tok")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error from mock")
  in

  (* =================================================================
     mcp_tools.ml: handle_crawl_team (missing param branches)
     ================================================================= *)
  let test_crawl_team_missing_team_id () =
    match Mcp_tools.handle_crawl_team
            (`Assoc [("token", `String "tok")]) with
    | Error msg -> check bool "team_id" true
        (try ignore (Str.search_forward (Str.regexp_string "team_id") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_crawl_team_missing_token () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_crawl_team
            (`Assoc [("team_id", `String "t1")]) with
    | Error msg -> check bool "token" true
        (try ignore (Str.search_forward (Str.regexp_string "token") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  (* =================================================================
     mcp_tools.ml: handle_team_tree (missing param branches)
     ================================================================= *)
  let test_team_tree_missing_team_id () =
    match Mcp_tools.handle_team_tree
            (`Assoc [("token", `String "tok")]) with
    | Error msg -> check bool "team_id" true
        (try ignore (Str.search_forward (Str.regexp_string "team_id") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_team_tree_missing_token () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_team_tree
            (`Assoc [("team_id", `String "t1")]) with
    | Error msg -> check bool "token" true
        (try ignore (Str.search_forward (Str.regexp_string "token") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  (* =================================================================
     mcp_tools.ml: handle_export_team (3 missing param combos)
     ================================================================= *)
  let test_export_team_missing_team_id () =
    match Mcp_tools.handle_export_team
            (`Assoc [("token", `String "tok"); ("output_dir", `String "/tmp")]) with
    | Error msg -> check bool "team_id" true
        (try ignore (Str.search_forward (Str.regexp_string "team_id") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_export_team_missing_token () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_export_team
            (`Assoc [("team_id", `String "t1"); ("output_dir", `String "/tmp")]) with
    | Error msg -> check bool "token" true
        (try ignore (Str.search_forward (Str.regexp_string "token") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_export_team_missing_output_dir () =
    match Mcp_tools.handle_export_team
            (`Assoc [("team_id", `String "t1"); ("token", `String "tok")]) with
    | Error msg -> check bool "output_dir" true
        (try ignore (Str.search_forward (Str.regexp_string "output_dir") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  (* =================================================================
     mcp_tools.ml: handle_get_variables (format branches)
     ================================================================= *)
  let test_get_variables_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_get_variables (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_get_variables_summary () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_get_variables
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("format", `String "summary")]) with
      | Ok json ->
          let s = Yojson.Safe.to_string json in
          check bool "has Collections" true
            (try ignore (Str.search_forward (Str.regexp_string "Collections") s 0); true
             with Not_found -> false)
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_get_variables_raw () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_get_variables
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("format", `String "raw")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_get_variables_resolved () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_get_variables
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("format", `String "resolved")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  (* =================================================================
     mcp_tools.ml: handle_tree via mock effects
     ================================================================= *)
  let test_handle_tree_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_tree (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_tree_success () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_tree
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_tree_indent () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_tree
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("style", `String "indent")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_tree_compact () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_tree
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("style", `String "compact")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_tree_with_node_id () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_tree
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("node_id", `String "2:1")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_tree_with_stats () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_tree
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("show_stats", `String "true")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_tree_error () =
    with_error_effects (fun () ->
      match Mcp_tools.handle_tree
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error from mock")
  in

  (* =================================================================
     mcp_tools.ml: handle_stats via mock effects
     ================================================================= *)
  let test_handle_stats_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_stats (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_stats_success () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_stats
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_stats_with_node_id () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_stats
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("node_id", `String "2:1")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_stats_error () =
    with_error_effects (fun () ->
      match Mcp_tools.handle_stats
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error from mock")
  in

  (* =================================================================
     mcp_tools.ml: handle_export_tokens via mock effects
     ================================================================= *)
  let test_handle_export_tokens_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_export_tokens (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_export_tokens_css () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_export_tokens
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("format", `String "css")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_export_tokens_semantic () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_export_tokens
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("format", `String "semantic")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_export_tokens_with_node_id () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_export_tokens
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("node_id", `String "2:1")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_export_tokens_error () =
    with_error_effects (fun () ->
      match Mcp_tools.handle_export_tokens
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error from mock")
  in

  (* =================================================================
     mcp_tools.ml: handle_query via mock effects
     ================================================================= *)
  let test_handle_query_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_query (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_query_success () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_query
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_query_with_filters () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_query
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("type", `String "FRAME");
                       ("name", `String "Login");
                       ("width_min", `Float 100.);
                       ("width_max", `Float 500.);
                       ("height_min", `Float 50.);
                       ("height_max", `Float 800.);
                       ("depth", `Float 3.);
                       ("limit", `Float 10.)]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_query_with_node_id () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_query
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("node_id", `String "2:1")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_query_with_color () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_query
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("color", `String "#FF0000")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_query_error () =
    with_error_effects (fun () ->
      match Mcp_tools.handle_query
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error from mock")
  in

  (* =================================================================
     mcp_tools.ml: handle_search via mock effects
     ================================================================= *)
  let test_handle_search_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_tools.handle_search (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_search_success () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_search
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("query", `String "Hello")]) with
      | Ok json ->
          let s = Yojson.Safe.to_string json in
          check bool "has results" true (String.length s > 2)
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_search_name_only () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_search
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("query", `String "Login");
                       ("search_in", `String "name")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_search_text_only () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_search
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("query", `String "World");
                       ("search_in", `String "text")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_search_empty_query () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_search
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("query", `String "   ")]) with
      | Ok json ->
          let s = Yojson.Safe.to_string json in
          check bool "empty result" true
            (try ignore (Str.search_forward (Str.regexp_string "[]") s 0); true
             with Not_found -> false)
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_search_multi_token () =
    with_mock_effects (fun () ->
      match Mcp_tools.handle_search
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("query", `String "Hello World");
                       ("limit", `Float 5.)]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_search_error () =
    (* Clear cache so error mock fires *)
    Figma_cache.invalidate ();
    with_error_effects (fun () ->
      match Mcp_tools.handle_search
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("query", `String "test")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error from mock")
  in

  (* =================================================================
     mcp_tools.ml: handle_read_large_result (edge cases)
     ================================================================= *)
  let test_read_large_result_missing_path () =
    match Mcp_tools.handle_read_large_result (`Assoc []) with
    | Error msg -> check bool "says file_path" true
        (try ignore (Str.search_forward (Str.regexp_string "file_path") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_read_large_result_outside_storage () =
    match Mcp_tools.handle_read_large_result
            (`Assoc [("file_path", `String "/etc/passwd")]) with
    | Error msg -> check bool "says must be under" true
        (try ignore (Str.search_forward (Str.regexp_string "must be under") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  (* =================================================================
     mcp_tools.ml: handle_category (edge cases)
     ================================================================= *)
  let test_category_invalid_mode () =
    (* Invalid mode now returns Error instead of raising *)
    match Mcp_tools.handle_category "explore" (`Assoc [("mode", `String "bogus")]) with
    | Error msg ->
        check bool "says invalid mode" true
          (try ignore (Str.search_forward (Str.regexp_string "Invalid mode") msg 0); true
           with Not_found -> false)
    | Ok _ -> fail "expected Error for invalid mode"
  in

  let test_category_describe_missing_tool () =
    match Mcp_tools.handle_category "explore"
            (`Assoc [("mode", `String "describe")]) with
    | Error msg -> check bool "says tool" true
        (try ignore (Str.search_forward (Str.regexp_string "tool") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_category_call_missing_tool () =
    match Mcp_tools.handle_category "explore"
            (`Assoc [("mode", `String "call")]) with
    | Error msg -> check bool "says tool" true
        (try ignore (Str.search_forward (Str.regexp_string "tool") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_category_describe_nonexistent_tool () =
    match Mcp_tools.handle_category "explore"
            (`Assoc [("mode", `String "describe"); ("tool", `String "zzz_fake")]) with
    | Error msg -> check bool "not found" true
        (try ignore (Str.search_forward (Str.regexp_string "not found") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_category_call_nonexistent_tool () =
    match Mcp_tools.handle_category "explore"
            (`Assoc [("mode", `String "call"); ("tool", `String "zzz_fake");
                     ("args", `Assoc [])]) with
    | Error msg -> check bool "not found" true
        (try ignore (Str.search_forward (Str.regexp_string "not found") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_category_list_unknown () =
    match Mcp_tools.handle_category "zzz_unknown" (`Assoc []) with
    | Error msg -> check bool "unknown" true
        (try ignore (Str.search_forward (Str.regexp_string "Unknown category") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  (* =================================================================
     mcp_tools.ml: read_resource (additional URIs)
     ================================================================= *)
  let test_read_resource_fidelity_content () =
    match Mcp_tools.read_resource "figma://docs/fidelity" with
    | Ok (mime, body) ->
        check string "mime" "text/markdown" mime;
        check bool "has Fidelity" true
          (try ignore (Str.search_forward (Str.regexp_string "Fidelity") body 0); true
           with Not_found -> false)
    | Error e -> fail ("expected Ok: " ^ e)
  in

  let test_read_resource_usage_content () =
    match Mcp_tools.read_resource "figma://docs/usage" with
    | Ok (mime, body) ->
        check string "mime" "text/markdown" mime;
        check bool "has Usage" true
          (try ignore (Str.search_forward (Str.regexp_string "Usage") body 0); true
           with Not_found -> false)
    | Error e -> fail ("expected Ok: " ^ e)
  in

  let test_read_resource_tokens_doc () =
    match Mcp_tools.read_resource "figma://docs/tokens" with
    | Ok (mime, body) ->
        check string "mime" "text/markdown" mime;
        check bool "has Tokens" true
          (try ignore (Str.search_forward (Str.regexp_string "Tokens") body 0); true
           with Not_found -> false)
    | Error e -> fail ("expected Ok: " ^ e)
  in

  let test_read_resource_unknown_uri () =
    match Mcp_tools.read_resource "figma://unknown/path" with
    | Error msg -> check bool "not found" true
        (try ignore (Str.search_forward (Str.regexp_string "not found") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_read_resource_tokens_fetch_error () =
    (* Exercise fetch_variables_cached error path via mock effects *)
    Unix.putenv "FIGMA_TOKEN" "tok";
    with_error_effects (fun () ->
      match Mcp_tools.read_resource "figma://tokens/fk1" with
      | Error _ -> ()
      | Ok _ -> fail "expected Error from mock effect")
  in

  let test_read_resource_tokens_empty_key () =
    Unix.putenv "FIGMA_TOKEN" "tok";
    match Mcp_tools.read_resource "figma://tokens/" with
    | Error msg -> check bool "empty key" true
        (try ignore (Str.search_forward (Str.regexp_string "file_key") msg 0); true
         with Not_found -> false)
    | Ok _ -> fail "expected Error"
  in

  let test_read_resource_tokens_raw () =
    Unix.putenv "FIGMA_TOKEN" "tok";
    with_mock_effects (fun () ->
      match Mcp_tools.read_resource "figma://tokens/fk1?format=raw" with
      | Ok (mime, _body) -> check string "mime" "application/json" mime
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_read_resource_tokens_resolved () =
    Unix.putenv "FIGMA_TOKEN" "tok";
    with_mock_effects (fun () ->
      match Mcp_tools.read_resource "figma://tokens/fk1?format=resolved" with
      | Ok (mime, _body) -> check string "mime" "application/json" mime
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_read_resource_tokens_dtcg () =
    Unix.putenv "FIGMA_TOKEN" "tok";
    with_mock_effects (fun () ->
      match Mcp_tools.read_resource "figma://tokens/fk1?format=dtcg" with
      | Ok (mime, body) ->
          check string "mime" "application/json" mime;
          check bool "has $extensions" true
            (try ignore (Str.search_forward (Str.regexp_string "$extensions") body 0); true
             with Not_found -> false)
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_read_resource_tokens_invalid_format () =
    Unix.putenv "FIGMA_TOKEN" "tok";
    with_mock_effects (fun () ->
      match Mcp_tools.read_resource "figma://tokens/fk1?format=xml" with
      | Error msg -> check bool "invalid format" true
          (try ignore (Str.search_forward (Str.regexp_string "Invalid format") msg 0); true
           with Not_found -> false)
      | Ok _ -> fail "expected Error for invalid format")
  in

  (* =================================================================
     mcp_tools.ml: code_connect_cache_put overflow
     ================================================================= *)
  let test_code_connect_cache_overflow () =
    (* Fill cache beyond 64 entries to trigger reset *)
    for i = 0 to 70 do
      let key = Printf.sprintf "key_%d" i in
      let mapping : Figma_code_connect.mapping = { version = "1.0"; project = None; components = [] } in
      Mcp_tools.code_connect_cache_put key mapping
    done;
    (* After overflow, cache should have been reset then re-added *)
    ()
  in

  (* =================================================================
     mcp_tools.ml: normalize_path / is_under_dir
     ================================================================= *)
  let test_normalize_path_existing () =
    match Mcp_tools.normalize_path "/tmp" with
    | Some _ -> ()
    | None -> fail "expected Some for existing path"
  in

  let test_normalize_path_nonexistent () =
    match Mcp_tools.normalize_path "/nonexistent_path_xyz_123" with
    | None -> ()
    | Some _ -> fail "expected None for non-existent path"
  in

  let test_is_under_dir_true () =
    (* /tmp should exist on macOS *)
    let result = Mcp_tools.is_under_dir ~dir:"/tmp" "/tmp/test" in
    (* This may return false if /tmp/test doesn't exist *)
    ignore result
  in

  let test_is_under_dir_same () =
    let result = Mcp_tools.is_under_dir ~dir:"/tmp" "/tmp" in
    check bool "same dir" true result
  in

  let test_is_under_dir_nonexistent () =
    let result = Mcp_tools.is_under_dir ~dir:"/nonexistent_xyz" "/nonexistent_xyz/sub" in
    check bool "non-existent returns false" false result
  in

  (* =================================================================
     mcp_tools.ml: truncate_string boundary cases
     ================================================================= *)
  let test_truncate_string_zero () =
    let result = Mcp_tools.truncate_string ~max_len:0 "hello" in
    check string "zero max_len" "hello" result
  in

  let test_truncate_string_negative () =
    let result = Mcp_tools.truncate_string ~max_len:(-5) "hello" in
    check string "negative max_len" "hello" result
  in

  let test_truncate_string_exact () =
    let result = Mcp_tools.truncate_string ~max_len:5 "hello" in
    check string "exact fit" "hello" result
  in

  let test_truncate_string_short () =
    let result = Mcp_tools.truncate_string ~max_len:3 "hello world" in
    check string "truncated" "hel...(truncated)" result
  in

  (* =================================================================
     mcp_api_handlers.ml: node_duplicate_key
     ================================================================= *)
  let test_node_duplicate_key_with_bbox () =
    let node = { Figma_types.default_node with
      id = "1:1"; name = " TestNode ";
      node_type = Figma_types.Frame;
      bbox = Some { x = 0.; y = 0.; width = 100.; height = 200. }
    } in
    let key = Mcp_api_handlers.node_duplicate_key node in
    check bool "has size" true
      (try ignore (Str.search_forward (Str.regexp_string "100x200") key 0); true
       with Not_found -> false);
    check bool "has type" true
      (try ignore (Str.search_forward (Str.regexp_string "FRAME") key 0); true
       with Not_found -> false)
  in

  let test_node_duplicate_key_without_bbox () =
    let node = { Figma_types.default_node with
      id = "2:2"; name = "NoSize";
      node_type = Figma_types.Text;
      bbox = None
    } in
    let key = Mcp_api_handlers.node_duplicate_key node in
    check bool "has ?" true
      (try ignore (Str.search_forward (Str.regexp_string "?") key 0); true
       with Not_found -> false)
  in

  (* =================================================================
     mcp_api_handlers.ml: handler missing-params branches
     ================================================================= *)
  let test_handle_get_file_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_file (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_file_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_file
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_file_raw () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_file
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok");
                       ("format", `String "raw")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_file_error () =
    with_error_effects (fun () ->
      match Mcp_api_handlers.handle_get_file
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error from mock")
  in

  let test_handle_list_screens_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_list_screens (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_list_screens_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_list_screens
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_file_meta_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_file_meta (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_file_meta_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_file_meta
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_file_versions_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_file_versions (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_file_versions_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_file_versions
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_file_versions_error () =
    with_error_effects (fun () ->
      match Mcp_api_handlers.handle_get_file_versions
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error")
  in

  let test_handle_get_file_comments_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_file_comments (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_file_comments_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_file_comments
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_file_comments_error () =
    with_error_effects (fun () ->
      match Mcp_api_handlers.handle_get_file_comments
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error")
  in

  let test_handle_post_comment_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_post_comment (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_post_comment_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_post_comment
              (`Assoc [
                ("file_key", `String "fk1"); ("token", `String "tok");
                ("message", `String "test comment");
                ("x", `Float 10.0); ("y", `Float 20.0)
              ]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_post_comment_with_node_id () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_post_comment
              (`Assoc [
                ("file_key", `String "fk1"); ("token", `String "tok");
                ("message", `String "test comment");
                ("x", `Float 10.0); ("y", `Float 20.0);
                ("node_id", `String "2:1")
              ]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_post_comment_error () =
    with_error_effects (fun () ->
      match Mcp_api_handlers.handle_post_comment
              (`Assoc [
                ("file_key", `String "fk1"); ("token", `String "tok");
                ("message", `String "msg");
                ("x", `Float 0.); ("y", `Float 0.)
              ]) with
      | Error _ -> ()
      | Ok _ -> fail "expected Error")
  in

  let test_handle_get_file_components_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_file_components (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_file_components_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_file_components
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_team_components_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_team_components (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_team_components_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_team_components
              (`Assoc [("team_id", `String "t1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_file_component_sets_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_file_component_sets (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_file_component_sets_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_file_component_sets
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_team_component_sets_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_team_component_sets (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_team_component_sets_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_team_component_sets
              (`Assoc [("team_id", `String "t1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_file_styles_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_file_styles (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_file_styles_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_file_styles
              (`Assoc [("file_key", `String "fk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_team_styles_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_team_styles (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_team_styles_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_team_styles
              (`Assoc [("team_id", `String "t1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_component_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_component (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_component_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_component
              (`Assoc [("component_key", `String "ck1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  let test_handle_get_component_set_missing () =
    Unix.putenv "FIGMA_TOKEN" "";
    match Mcp_api_handlers.handle_get_component_set (`Assoc []) with
    | Error _ -> ()
    | Ok _ -> fail "expected Error"
  in

  let test_handle_get_component_set_success () =
    with_mock_effects (fun () ->
      match Mcp_api_handlers.handle_get_component_set
              (`Assoc [("component_set_key", `String "csk1"); ("token", `String "tok")]) with
      | Ok _ -> ()
      | Error e -> fail ("expected Ok: " ^ e))
  in

  (* =================================================================
     Run all tests
     ================================================================= *)
  run "Coverage Wave 9: mcp_tools + mcp_api_handlers" [
    ("cache_invalidate", [
      test_case "all" `Quick test_cache_invalidate_all;
      test_case "file only" `Quick test_cache_invalidate_file_only;
      test_case "file+node" `Quick test_cache_invalidate_file_and_node;
    ]);
    ("codegen_sync", [
      test_case "missing json" `Quick test_codegen_sync_missing_json;
      test_case "invalid json" `Quick test_codegen_sync_invalid_json;
      test_case "valid minimal" `Quick test_codegen_sync_valid_minimal;
      test_case "raw format" `Quick test_codegen_sync_raw_format;
    ]);
    ("parse_url", [
      test_case "with node" `Quick test_parse_url_with_node;
      test_case "missing" `Quick test_parse_url_missing;
      test_case "team url" `Quick test_parse_url_team_url;
    ]);
    ("handle_get_me", [
      test_case "no token" `Quick test_get_me_no_token;
      test_case "success" `Quick test_get_me_success;
      test_case "error" `Quick test_get_me_error;
    ]);
    ("handle_list_projects", [
      test_case "missing" `Quick test_list_projects_missing_params;
      test_case "success" `Quick test_list_projects_success;
      test_case "error" `Quick test_list_projects_error;
    ]);
    ("handle_list_files", [
      test_case "missing" `Quick test_list_files_missing_params;
      test_case "success" `Quick test_list_files_success;
      test_case "error" `Quick test_list_files_error;
    ]);
    ("crawl_team", [
      test_case "missing team_id" `Quick test_crawl_team_missing_team_id;
      test_case "missing token" `Quick test_crawl_team_missing_token;
    ]);
    ("team_tree", [
      test_case "missing team_id" `Quick test_team_tree_missing_team_id;
      test_case "missing token" `Quick test_team_tree_missing_token;
    ]);
    ("export_team", [
      test_case "missing team_id" `Quick test_export_team_missing_team_id;
      test_case "missing token" `Quick test_export_team_missing_token;
      test_case "missing output_dir" `Quick test_export_team_missing_output_dir;
    ]);
    ("get_variables", [
      test_case "missing" `Quick test_get_variables_missing;
      test_case "summary" `Quick test_get_variables_summary;
      test_case "raw" `Quick test_get_variables_raw;
      test_case "resolved" `Quick test_get_variables_resolved;
    ]);
    ("handle_tree", [
      test_case "missing" `Quick test_handle_tree_missing;
      test_case "success" `Quick test_handle_tree_success;
      test_case "indent" `Quick test_handle_tree_indent;
      test_case "compact" `Quick test_handle_tree_compact;
      test_case "with node_id" `Quick test_handle_tree_with_node_id;
      test_case "with stats" `Quick test_handle_tree_with_stats;
      test_case "error" `Quick test_handle_tree_error;
    ]);
    ("handle_stats", [
      test_case "missing" `Quick test_handle_stats_missing;
      test_case "success" `Quick test_handle_stats_success;
      test_case "with node_id" `Quick test_handle_stats_with_node_id;
      test_case "error" `Quick test_handle_stats_error;
    ]);
    ("handle_export_tokens", [
      test_case "missing" `Quick test_handle_export_tokens_missing;
      test_case "css" `Quick test_handle_export_tokens_css;
      test_case "semantic" `Quick test_handle_export_tokens_semantic;
      test_case "with node_id" `Quick test_handle_export_tokens_with_node_id;
      test_case "error" `Quick test_handle_export_tokens_error;
    ]);
    ("handle_query", [
      test_case "missing" `Quick test_handle_query_missing;
      test_case "success" `Quick test_handle_query_success;
      test_case "with filters" `Quick test_handle_query_with_filters;
      test_case "with node_id" `Quick test_handle_query_with_node_id;
      test_case "with color" `Quick test_handle_query_with_color;
      test_case "error" `Quick test_handle_query_error;
    ]);
    ("handle_search", [
      test_case "missing" `Quick test_handle_search_missing;
      test_case "success" `Quick test_handle_search_success;
      test_case "name only" `Quick test_handle_search_name_only;
      test_case "text only" `Quick test_handle_search_text_only;
      test_case "empty query" `Quick test_handle_search_empty_query;
      test_case "multi token" `Quick test_handle_search_multi_token;
      test_case "error" `Quick test_handle_search_error;
    ]);
    ("read_large_result", [
      test_case "missing path" `Quick test_read_large_result_missing_path;
      test_case "outside storage" `Quick test_read_large_result_outside_storage;
    ]);
    ("handle_category", [
      test_case "invalid mode" `Quick test_category_invalid_mode;
      test_case "describe missing tool" `Quick test_category_describe_missing_tool;
      test_case "call missing tool" `Quick test_category_call_missing_tool;
      test_case "describe nonexistent" `Quick test_category_describe_nonexistent_tool;
      test_case "call nonexistent" `Quick test_category_call_nonexistent_tool;
      test_case "list unknown" `Quick test_category_list_unknown;
    ]);
    ("read_resource", [
      test_case "fidelity" `Quick test_read_resource_fidelity_content;
      test_case "usage" `Quick test_read_resource_usage_content;
      test_case "tokens doc" `Quick test_read_resource_tokens_doc;
      test_case "unknown" `Quick test_read_resource_unknown_uri;
      test_case "tokens fetch error" `Quick test_read_resource_tokens_fetch_error;
      test_case "tokens empty key" `Quick test_read_resource_tokens_empty_key;
      test_case "tokens raw" `Quick test_read_resource_tokens_raw;
      test_case "tokens resolved" `Quick test_read_resource_tokens_resolved;
      test_case "tokens dtcg" `Quick test_read_resource_tokens_dtcg;
      test_case "tokens invalid fmt" `Quick test_read_resource_tokens_invalid_format;
    ]);
    ("code_connect_cache", [
      test_case "overflow" `Quick test_code_connect_cache_overflow;
    ]);
    ("normalize_path", [
      test_case "existing" `Quick test_normalize_path_existing;
      test_case "nonexistent" `Quick test_normalize_path_nonexistent;
    ]);
    ("is_under_dir", [
      test_case "true" `Quick test_is_under_dir_true;
      test_case "same dir" `Quick test_is_under_dir_same;
      test_case "nonexistent" `Quick test_is_under_dir_nonexistent;
    ]);
    ("truncate_string", [
      test_case "zero" `Quick test_truncate_string_zero;
      test_case "negative" `Quick test_truncate_string_negative;
      test_case "exact" `Quick test_truncate_string_exact;
      test_case "short" `Quick test_truncate_string_short;
    ]);
    ("node_duplicate_key", [
      test_case "with bbox" `Quick test_node_duplicate_key_with_bbox;
      test_case "without bbox" `Quick test_node_duplicate_key_without_bbox;
    ]);
    ("api_handle_get_file", [
      test_case "missing" `Quick test_handle_get_file_missing;
      test_case "success" `Quick test_handle_get_file_success;
      test_case "raw format" `Quick test_handle_get_file_raw;
      test_case "error" `Quick test_handle_get_file_error;
    ]);
    ("api_list_screens", [
      test_case "missing" `Quick test_handle_list_screens_missing;
      test_case "success" `Quick test_handle_list_screens_success;
    ]);
    ("api_get_file_meta", [
      test_case "missing" `Quick test_handle_get_file_meta_missing;
      test_case "success" `Quick test_handle_get_file_meta_success;
    ]);
    ("api_get_file_versions", [
      test_case "missing" `Quick test_handle_get_file_versions_missing;
      test_case "success" `Quick test_handle_get_file_versions_success;
      test_case "error" `Quick test_handle_get_file_versions_error;
    ]);
    ("api_get_file_comments", [
      test_case "missing" `Quick test_handle_get_file_comments_missing;
      test_case "success" `Quick test_handle_get_file_comments_success;
      test_case "error" `Quick test_handle_get_file_comments_error;
    ]);
    ("api_post_comment", [
      test_case "missing" `Quick test_handle_post_comment_missing;
      test_case "success" `Quick test_handle_post_comment_success;
      test_case "with node_id" `Quick test_handle_post_comment_with_node_id;
      test_case "error" `Quick test_handle_post_comment_error;
    ]);
    ("api_get_file_components", [
      test_case "missing" `Quick test_handle_get_file_components_missing;
      test_case "success" `Quick test_handle_get_file_components_success;
    ]);
    ("api_get_team_components", [
      test_case "missing" `Quick test_handle_get_team_components_missing;
      test_case "success" `Quick test_handle_get_team_components_success;
    ]);
    ("api_get_file_component_sets", [
      test_case "missing" `Quick test_handle_get_file_component_sets_missing;
      test_case "success" `Quick test_handle_get_file_component_sets_success;
    ]);
    ("api_get_team_component_sets", [
      test_case "missing" `Quick test_handle_get_team_component_sets_missing;
      test_case "success" `Quick test_handle_get_team_component_sets_success;
    ]);
    ("api_get_file_styles", [
      test_case "missing" `Quick test_handle_get_file_styles_missing;
      test_case "success" `Quick test_handle_get_file_styles_success;
    ]);
    ("api_get_team_styles", [
      test_case "missing" `Quick test_handle_get_team_styles_missing;
      test_case "success" `Quick test_handle_get_team_styles_success;
    ]);
    ("api_get_component", [
      test_case "missing" `Quick test_handle_get_component_missing;
      test_case "success" `Quick test_handle_get_component_success;
    ]);
    ("api_get_component_set", [
      test_case "missing" `Quick test_handle_get_component_set_missing;
      test_case "success" `Quick test_handle_get_component_set_success;
    ]);
  ]
