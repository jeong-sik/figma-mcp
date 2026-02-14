(** Coverage tests for mcp_api_handlers.ml — all 26 handler functions
    exercised through mock algebraic effect handlers.

    For each handler:
    1. Missing args test — call with empty args, expect Error
    2. Success test — call with valid args inside mock effects, expect Ok *)

open Figma_mcp [@@warning "-33"]

(* Set FIGMA_TOKEN for resolve_token *)
let () = Unix.putenv "FIGMA_TOKEN" "test-token-12345"

(* ================================================================
   Mock effect handler — intercepts ALL Figma effects with canned OK
   ================================================================ *)

let with_mock_effects f =
  Effect.Deep.try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        (* --- Figma_get_file: return a minimal document --- *)
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
                        ("children", `List [])
                      ]
                    ])
                  ])
                ])))

        (* --- Figma_get_nodes: return nodes map with document --- *)
        | Figma_effects.Figma_get_nodes { node_ids; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let node_entries = List.map (fun nid ->
                (nid, `Assoc [
                  ("document", `Assoc [
                    ("id", `String nid);
                    ("name", `String "Frame1");
                    ("type", `String "FRAME");
                    ("children", `List []);
                    ("absoluteBoundingBox", `Assoc [
                      ("x", `Float 0.0); ("y", `Float 0.0);
                      ("width", `Float 100.0); ("height", `Float 100.0)
                    ])
                  ])
                ])
              ) node_ids in
              Effect.Deep.continue k
                (Ok (`Assoc [("nodes", `Assoc node_entries)])))

        (* --- Figma_get_images: image URL map --- *)
        | Figma_effects.Figma_get_images { node_ids; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              let img_entries = List.map (fun nid ->
                (nid, `String "https://example.com/img.png")
              ) node_ids in
              Effect.Deep.continue k
                (Ok (`Assoc [("images", `Assoc img_entries)])))

        (* --- Figma_get_file_images --- *)
        | Figma_effects.Figma_get_file_images _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("images", `Assoc [])])))

        (* --- Figma_get_file_meta --- *)
        | Figma_effects.Figma_get_file_meta _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [
                  ("components", `Assoc []);
                  ("componentSets", `Assoc []);
                  ("styles", `Assoc [])
                ])))

        (* --- Simple passthrough effects returning data list --- *)
        | Figma_effects.Figma_get_file_components _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_team_components _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_file_component_sets _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_team_component_sets _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_file_styles _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_team_styles _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_component _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_component_set _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_style _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_file_versions _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))
        | Figma_effects.Figma_get_file_comments _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("data", `List [])])))

        (* --- Figma_post_file_comment --- *)
        | Figma_effects.Figma_post_file_comment _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("id", `String "comment-1")])))

        (* --- Figma_create_webhook --- *)
        | Figma_effects.Figma_create_webhook _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("id", `String "webhook-1")])))

        (* --- Figma_download_url --- *)
        | Figma_effects.Figma_download_url _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok "/tmp/downloaded.png"))

        (* --- Figma_get_variables --- *)
        | Figma_effects.Figma_get_variables _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("variables", `Assoc [])])))

        (* --- Figma_get_dev_resources --- *)
        | Figma_effects.Figma_get_dev_resources _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("dev_resources", `List [])])))

        (* --- Figma_add_dev_resource --- *)
        | Figma_effects.Figma_add_dev_resource _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k
                (Ok (`Assoc [("id", `String "dev-res-1")])))

        (* --- Logging: silenced --- *)
        | Figma_effects.Log_debug _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())
        | Figma_effects.Log_info _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())
        | Figma_effects.Log_error _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())

        (* --- Eio_sleep: no-op --- *)
        | Figma_effects.Eio_sleep _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k ())

        | _ -> None }

(* ================================================================
   Helper: assert result is Ok or Error
   ================================================================ *)

let assert_ok label = function
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "%s: unexpected error: %s" label e)

let assert_error label = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (Printf.sprintf "%s: expected error but got Ok" label)

(* ================================================================
   1. handle_get_file
   ================================================================ *)

let test_get_file_missing () =
  let r = Mcp_api_handlers.handle_get_file (`Assoc []) in
  assert_error "get_file missing" r

let test_get_file_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("file_key", `String "ABC123")] in
    let r = Mcp_api_handlers.handle_get_file args in
    assert_ok "get_file success" r)

(* ================================================================
   2. handle_get_file_meta
   ================================================================ *)

let test_get_file_meta_missing () =
  let r = Mcp_api_handlers.handle_get_file_meta (`Assoc []) in
  assert_error "get_file_meta missing" r

let test_get_file_meta_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("file_key", `String "ABC123")] in
    let r = Mcp_api_handlers.handle_get_file_meta args in
    assert_ok "get_file_meta success" r)

(* ================================================================
   3. handle_list_screens
   ================================================================ *)

let test_list_screens_missing () =
  let r = Mcp_api_handlers.handle_list_screens (`Assoc []) in
  assert_error "list_screens missing" r

let test_list_screens_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("file_key", `String "ABC123")] in
    let r = Mcp_api_handlers.handle_list_screens args in
    assert_ok "list_screens success" r)

(* ================================================================
   4. handle_get_node
   ================================================================ *)

let test_get_node_missing () =
  let r = Mcp_api_handlers.handle_get_node (`Assoc []) in
  assert_error "get_node missing" r

let test_get_node_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [
      ("file_key", `String "ABC123");
      ("node_id", `String "1:2")
    ] in
    let r = Mcp_api_handlers.handle_get_node args in
    assert_ok "get_node success" r)

(* ================================================================
   5. handle_get_node_bundle
   ================================================================ *)

let test_get_node_bundle_missing () =
  let r = Mcp_api_handlers.handle_get_node_bundle (`Assoc []) in
  assert_error "get_node_bundle missing" r

let test_get_node_bundle_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [
      ("file_key", `String "ABC123");
      ("node_id", `String "1:2");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
    ] in
    let r = Mcp_api_handlers.handle_get_node_bundle args in
    assert_ok "get_node_bundle success" r)

(* ================================================================
   6. handle_get_node_summary
   ================================================================ *)

let test_get_node_summary_missing () =
  let r = Mcp_api_handlers.handle_get_node_summary (`Assoc []) in
  assert_error "get_node_summary missing" r

let test_get_node_summary_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [
      ("file_key", `String "ABC123");
      ("node_id", `String "1:2")
    ] in
    let r = Mcp_api_handlers.handle_get_node_summary args in
    assert_ok "get_node_summary success" r)

(* ================================================================
   7. handle_select_nodes
   ================================================================ *)

let test_select_nodes_missing () =
  let r = Mcp_api_handlers.handle_select_nodes (`Assoc []) in
  assert_error "select_nodes missing" r

let test_select_nodes_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [
      ("file_key", `String "ABC123");
      ("node_id", `String "1:2");
      ("preview", `Bool false)
    ] in
    let r = Mcp_api_handlers.handle_select_nodes args in
    assert_ok "select_nodes success" r)

(* ================================================================
   8. handle_get_node_chunk
   ================================================================ *)

let test_get_node_chunk_missing () =
  let r = Mcp_api_handlers.handle_get_node_chunk (`Assoc []) in
  assert_error "get_node_chunk missing" r

let test_get_node_chunk_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [
      ("file_key", `String "ABC123");
      ("node_id", `String "1:2")
    ] in
    let r = Mcp_api_handlers.handle_get_node_chunk args in
    assert_ok "get_node_chunk success" r)

(* ================================================================
   9. handle_export_image
   ================================================================ *)

let test_export_image_missing () =
  let r = Mcp_api_handlers.handle_export_image (`Assoc []) in
  assert_error "export_image missing" r

let test_export_image_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [
      ("file_key", `String "ABC123");
      ("node_ids", `String "1:2,3:4")
    ] in
    let r = Mcp_api_handlers.handle_export_image args in
    assert_ok "export_image success" r)

(* ================================================================
   10. handle_export_smart
   ================================================================ *)

let test_export_smart_missing () =
  let r = Mcp_api_handlers.handle_export_smart (`Assoc []) in
  assert_error "export_smart missing" r

let test_export_smart_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [
      ("file_key", `String "ABC123");
      ("node_id", `String "1:2")
    ] in
    let r = Mcp_api_handlers.handle_export_smart args in
    assert_ok "export_smart success" r)

(* ================================================================
   11. handle_get_image_fills
   ================================================================ *)

let test_get_image_fills_missing () =
  let r = Mcp_api_handlers.handle_get_image_fills (`Assoc []) in
  assert_error "get_image_fills missing" r

let test_get_image_fills_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("file_key", `String "ABC123")] in
    let r = Mcp_api_handlers.handle_get_image_fills args in
    assert_ok "get_image_fills success" r)

(* ================================================================
   12. handle_get_nodes (plural, comma-separated)
   ================================================================ *)

let test_get_nodes_missing () =
  let r = Mcp_api_handlers.handle_get_nodes (`Assoc []) in
  assert_error "get_nodes missing" r

let test_get_nodes_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [
      ("file_key", `String "ABC123");
      ("node_ids", `String "1:2,3:4")
    ] in
    let r = Mcp_api_handlers.handle_get_nodes args in
    assert_ok "get_nodes success" r)

(* ================================================================
   13. handle_get_file_versions
   ================================================================ *)

let test_get_file_versions_missing () =
  let r = Mcp_api_handlers.handle_get_file_versions (`Assoc []) in
  assert_error "get_file_versions missing" r

let test_get_file_versions_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("file_key", `String "ABC123")] in
    let r = Mcp_api_handlers.handle_get_file_versions args in
    assert_ok "get_file_versions success" r)

(* ================================================================
   14. handle_get_file_comments
   ================================================================ *)

let test_get_file_comments_missing () =
  let r = Mcp_api_handlers.handle_get_file_comments (`Assoc []) in
  assert_error "get_file_comments missing" r

let test_get_file_comments_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("file_key", `String "ABC123")] in
    let r = Mcp_api_handlers.handle_get_file_comments args in
    assert_ok "get_file_comments success" r)

(* ================================================================
   15. handle_post_comment
   ================================================================ *)

let test_post_comment_missing () =
  let r = Mcp_api_handlers.handle_post_comment (`Assoc []) in
  assert_error "post_comment missing" r

let test_post_comment_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [
      ("file_key", `String "ABC123");
      ("message", `String "Hello");
      ("x", `Float 10.0);
      ("y", `Float 20.0)
    ] in
    let r = Mcp_api_handlers.handle_post_comment args in
    assert_ok "post_comment success" r)

(* ================================================================
   16. handle_get_file_components
   ================================================================ *)

let test_get_file_components_missing () =
  let r = Mcp_api_handlers.handle_get_file_components (`Assoc []) in
  assert_error "get_file_components missing" r

let test_get_file_components_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("file_key", `String "ABC123")] in
    let r = Mcp_api_handlers.handle_get_file_components args in
    assert_ok "get_file_components success" r)

(* ================================================================
   17. handle_get_team_components
   ================================================================ *)

let test_get_team_components_missing () =
  let r = Mcp_api_handlers.handle_get_team_components (`Assoc []) in
  assert_error "get_team_components missing" r

let test_get_team_components_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("team_id", `String "TEAM1")] in
    let r = Mcp_api_handlers.handle_get_team_components args in
    assert_ok "get_team_components success" r)

(* ================================================================
   18. handle_get_file_component_sets
   ================================================================ *)

let test_get_file_component_sets_missing () =
  let r = Mcp_api_handlers.handle_get_file_component_sets (`Assoc []) in
  assert_error "get_file_component_sets missing" r

let test_get_file_component_sets_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("file_key", `String "ABC123")] in
    let r = Mcp_api_handlers.handle_get_file_component_sets args in
    assert_ok "get_file_component_sets success" r)

(* ================================================================
   19. handle_get_team_component_sets
   ================================================================ *)

let test_get_team_component_sets_missing () =
  let r = Mcp_api_handlers.handle_get_team_component_sets (`Assoc []) in
  assert_error "get_team_component_sets missing" r

let test_get_team_component_sets_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("team_id", `String "TEAM1")] in
    let r = Mcp_api_handlers.handle_get_team_component_sets args in
    assert_ok "get_team_component_sets success" r)

(* ================================================================
   20. handle_get_file_styles
   ================================================================ *)

let test_get_file_styles_missing () =
  let r = Mcp_api_handlers.handle_get_file_styles (`Assoc []) in
  assert_error "get_file_styles missing" r

let test_get_file_styles_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("file_key", `String "ABC123")] in
    let r = Mcp_api_handlers.handle_get_file_styles args in
    assert_ok "get_file_styles success" r)

(* ================================================================
   21. handle_get_team_styles
   ================================================================ *)

let test_get_team_styles_missing () =
  let r = Mcp_api_handlers.handle_get_team_styles (`Assoc []) in
  assert_error "get_team_styles missing" r

let test_get_team_styles_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("team_id", `String "TEAM1")] in
    let r = Mcp_api_handlers.handle_get_team_styles args in
    assert_ok "get_team_styles success" r)

(* ================================================================
   22. handle_get_component
   ================================================================ *)

let test_get_component_missing () =
  let r = Mcp_api_handlers.handle_get_component (`Assoc []) in
  assert_error "get_component missing" r

let test_get_component_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("component_key", `String "comp-key-1")] in
    let r = Mcp_api_handlers.handle_get_component args in
    assert_ok "get_component success" r)

(* ================================================================
   23. handle_get_component_set
   ================================================================ *)

let test_get_component_set_missing () =
  let r = Mcp_api_handlers.handle_get_component_set (`Assoc []) in
  assert_error "get_component_set missing" r

let test_get_component_set_success () =
  with_mock_effects (fun () ->
    let args = `Assoc [("component_set_key", `String "cs-key-1")] in
    let r = Mcp_api_handlers.handle_get_component_set args in
    assert_ok "get_component_set success" r)

(* ================================================================
   24. handle_get_dev_resources (NOT IMPLEMENTED — always Error)
   ================================================================ *)

let test_get_dev_resources_missing () =
  let r = Mcp_api_handlers.handle_get_dev_resources (`Assoc []) in
  assert_error "get_dev_resources missing" r

let test_get_dev_resources_not_impl () =
  let args = `Assoc [
    ("file_key", `String "ABC123");
    ("node_id", `String "1:2")
  ] in
  let r = Mcp_api_handlers.handle_get_dev_resources args in
  assert_error "get_dev_resources not implemented" r

(* ================================================================
   25. handle_add_dev_resource (NOT IMPLEMENTED — always Error)
   ================================================================ *)

let test_add_dev_resource_missing () =
  let r = Mcp_api_handlers.handle_add_dev_resource (`Assoc []) in
  assert_error "add_dev_resource missing" r

let test_add_dev_resource_not_impl () =
  let args = `Assoc [
    ("file_key", `String "ABC123");
    ("node_id", `String "1:2");
    ("name", `String "Spec");
    ("url", `String "https://example.com")
  ] in
  let r = Mcp_api_handlers.handle_add_dev_resource args in
  assert_error "add_dev_resource not implemented" r

(* ================================================================
   26. handle_setup_webhook (NOT IMPLEMENTED — always Error)
   ================================================================ *)

let test_setup_webhook_missing () =
  let r = Mcp_api_handlers.handle_setup_webhook (`Assoc []) in
  assert_error "setup_webhook missing" r

let test_setup_webhook_not_impl () =
  let args = `Assoc [
    ("team_id", `String "TEAM1");
    ("file_key", `String "ABC123")
  ] in
  let r = Mcp_api_handlers.handle_setup_webhook args in
  assert_error "setup_webhook not implemented" r

(* ================================================================
   Test runner
   ================================================================ *)

let () =
  let open Alcotest in
  run "API Handlers Effects" [
    ("handle_get_file", [
      test_case "missing args" `Quick test_get_file_missing;
      test_case "success" `Quick test_get_file_success;
    ]);
    ("handle_get_file_meta", [
      test_case "missing args" `Quick test_get_file_meta_missing;
      test_case "success" `Quick test_get_file_meta_success;
    ]);
    ("handle_list_screens", [
      test_case "missing args" `Quick test_list_screens_missing;
      test_case "success" `Quick test_list_screens_success;
    ]);
    ("handle_get_node", [
      test_case "missing args" `Quick test_get_node_missing;
      test_case "success" `Quick test_get_node_success;
    ]);
    ("handle_get_node_bundle", [
      test_case "missing args" `Quick test_get_node_bundle_missing;
      test_case "success" `Quick test_get_node_bundle_success;
    ]);
    ("handle_get_node_summary", [
      test_case "missing args" `Quick test_get_node_summary_missing;
      test_case "success" `Quick test_get_node_summary_success;
    ]);
    ("handle_select_nodes", [
      test_case "missing args" `Quick test_select_nodes_missing;
      test_case "success" `Quick test_select_nodes_success;
    ]);
    ("handle_get_node_chunk", [
      test_case "missing args" `Quick test_get_node_chunk_missing;
      test_case "success" `Quick test_get_node_chunk_success;
    ]);
    ("handle_export_image", [
      test_case "missing args" `Quick test_export_image_missing;
      test_case "success" `Quick test_export_image_success;
    ]);
    ("handle_export_smart", [
      test_case "missing args" `Quick test_export_smart_missing;
      test_case "success" `Quick test_export_smart_success;
    ]);
    ("handle_get_image_fills", [
      test_case "missing args" `Quick test_get_image_fills_missing;
      test_case "success" `Quick test_get_image_fills_success;
    ]);
    ("handle_get_nodes", [
      test_case "missing args" `Quick test_get_nodes_missing;
      test_case "success" `Quick test_get_nodes_success;
    ]);
    ("handle_get_file_versions", [
      test_case "missing args" `Quick test_get_file_versions_missing;
      test_case "success" `Quick test_get_file_versions_success;
    ]);
    ("handle_get_file_comments", [
      test_case "missing args" `Quick test_get_file_comments_missing;
      test_case "success" `Quick test_get_file_comments_success;
    ]);
    ("handle_post_comment", [
      test_case "missing args" `Quick test_post_comment_missing;
      test_case "success" `Quick test_post_comment_success;
    ]);
    ("handle_get_file_components", [
      test_case "missing args" `Quick test_get_file_components_missing;
      test_case "success" `Quick test_get_file_components_success;
    ]);
    ("handle_get_team_components", [
      test_case "missing args" `Quick test_get_team_components_missing;
      test_case "success" `Quick test_get_team_components_success;
    ]);
    ("handle_get_file_component_sets", [
      test_case "missing args" `Quick test_get_file_component_sets_missing;
      test_case "success" `Quick test_get_file_component_sets_success;
    ]);
    ("handle_get_team_component_sets", [
      test_case "missing args" `Quick test_get_team_component_sets_missing;
      test_case "success" `Quick test_get_team_component_sets_success;
    ]);
    ("handle_get_file_styles", [
      test_case "missing args" `Quick test_get_file_styles_missing;
      test_case "success" `Quick test_get_file_styles_success;
    ]);
    ("handle_get_team_styles", [
      test_case "missing args" `Quick test_get_team_styles_missing;
      test_case "success" `Quick test_get_team_styles_success;
    ]);
    ("handle_get_component", [
      test_case "missing args" `Quick test_get_component_missing;
      test_case "success" `Quick test_get_component_success;
    ]);
    ("handle_get_component_set", [
      test_case "missing args" `Quick test_get_component_set_missing;
      test_case "success" `Quick test_get_component_set_success;
    ]);
    ("handle_get_dev_resources", [
      test_case "missing args" `Quick test_get_dev_resources_missing;
      test_case "not implemented" `Quick test_get_dev_resources_not_impl;
    ]);
    ("handle_add_dev_resource", [
      test_case "missing args" `Quick test_add_dev_resource_missing;
      test_case "not implemented" `Quick test_add_dev_resource_not_impl;
    ]);
    ("handle_setup_webhook", [
      test_case "missing args" `Quick test_setup_webhook_missing;
      test_case "not implemented" `Quick test_setup_webhook_not_impl;
    ]);
  ]
