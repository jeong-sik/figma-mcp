(** Coverage Wave 4: mcp_api_handlers.ml — exercise UNCOVERED handler branches.

    Focus: optional params, format variations, error paths, download paths,
    text_mode filtering, scoring logic, depth filtering, node_chunk trimming,
    export_smart split/debug, image_fills download, post_comment node_id,
    get_nodes fidelity conversion, node_summary truncation.

    Uses the same Effect.Deep.try_with mock pattern as test_api_handlers_effects.ml
    but with richer mock data to exercise deeper branches. *)

open Figma_mcp [@@warning "-33"]

(* Set FIGMA_TOKEN for resolve_token *)
let () = Unix.putenv "FIGMA_TOKEN" "test-token-w4"

(* ================================================================
   Helpers
   ================================================================ *)

let args_of pairs =
  `Assoc pairs

let check_ok label = function
  | Ok _ -> ()
  | Error msg -> Alcotest.fail (Printf.sprintf "%s: expected Ok, got Error %s" label msg)

let check_error label = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (Printf.sprintf "%s: expected Error, got Ok" label)

let str_contains haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in true
  with Not_found -> false

(* ================================================================
   Rich mock node data for deep branch coverage
   ================================================================ *)

(* A FRAME node with children of various types for select_nodes scoring *)
let rich_frame_node =
  `Assoc [
    ("id", `String "1:1");
    ("name", `String "MainFrame");
    ("type", `String "FRAME");
    ("visible", `Bool true);
    ("opacity", `Float 1.0);
    ("absoluteBoundingBox", `Assoc [
      ("x", `Float 0.0); ("y", `Float 0.0);
      ("width", `Float 800.0); ("height", `Float 600.0);
    ]);
    ("children", `List [
      (* Text node — scores text:+2, area OK *)
      `Assoc [
        ("id", `String "2:1");
        ("name", `String "Title");
        ("type", `String "TEXT");
        ("characters", `String "Hello World");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 10.0); ("y", `Float 10.0);
          ("width", `Float 200.0); ("height", `Float 50.0);
        ]);
        ("children", `List []);
      ];
      (* COMPONENT_SET — scores component:+1 *)
      `Assoc [
        ("id", `String "2:2");
        ("name", `String "ButtonGroup");
        ("type", `String "COMPONENT_SET");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 10.0); ("y", `Float 80.0);
          ("width", `Float 300.0); ("height", `Float 100.0);
        ]);
        ("children", `List []);
      ];
      (* Small invisible node — excluded invisible *)
      `Assoc [
        ("id", `String "2:3");
        ("name", `String "Hidden");
        ("type", `String "RECTANGLE");
        ("visible", `Bool false);
        ("opacity", `Float 1.0);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 0.0);
          ("width", `Float 5.0); ("height", `Float 5.0);
        ]);
        ("children", `List []);
      ];
      (* RECTANGLE with image fill — scores image_fill:+2 *)
      `Assoc [
        ("id", `String "2:4");
        ("name", `String "Hero Image");
        ("type", `String "RECTANGLE");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("fills", `List [
          `Assoc [("type", `String "IMAGE"); ("imageRef", `String "img:abc")]
        ]);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 200.0);
          ("width", `Float 400.0); ("height", `Float 300.0);
        ]);
        ("children", `List []);
      ];
      (* FRAME with auto-layout — scores auto_layout:+1.5 *)
      `Assoc [
        ("id", `String "2:5");
        ("name", `String "AutoRow");
        ("type", `String "FRAME");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("layoutMode", `String "HORIZONTAL");
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 500.0);
          ("width", `Float 600.0); ("height", `Float 80.0);
        ]);
        ("children", `List []);
      ];
      (* Tiny node — small_penalty area < 64 *)
      `Assoc [
        ("id", `String "2:6");
        ("name", `String "Dot");
        ("type", `String "ELLIPSE");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 0.0);
          ("width", `Float 4.0); ("height", `Float 4.0);
        ]);
        ("children", `List []);
      ];
      (* Duplicate of AutoRow — duplicate:-1 penalty *)
      `Assoc [
        ("id", `String "2:7");
        ("name", `String "AutoRow");
        ("type", `String "FRAME");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("layoutMode", `String "HORIZONTAL");
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 580.0);
          ("width", `Float 600.0); ("height", `Float 80.0);
        ]);
        ("children", `List []);
      ];
      (* "as-is" named node — as_is:-0.5 penalty *)
      `Assoc [
        ("id", `String "2:8");
        ("name", `String "as-is placeholder");
        ("type", `String "RECTANGLE");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 0.0);
          ("width", `Float 100.0); ("height", `Float 100.0);
        ]);
        ("children", `List []);
      ];
      (* Mask hint node *)
      `Assoc [
        ("id", `String "2:9");
        ("name", `String "ClipMask");
        ("type", `String "RECTANGLE");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("isMask", `Bool true);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 0.0);
          ("width", `Float 200.0); ("height", `Float 200.0);
        ]);
        ("children", `List []);
      ];
      (* Zero opacity — invisible *)
      `Assoc [
        ("id", `String "2:10");
        ("name", `String "Phantom");
        ("type", `String "RECTANGLE");
        ("visible", `Bool true);
        ("opacity", `Float 0.0);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 0.0);
          ("width", `Float 100.0); ("height", `Float 100.0);
        ]);
        ("children", `List []);
      ];
      (* Note text — should be captured in notes and excluded *)
      `Assoc [
        ("id", `String "2:11");
        ("name", `String "TODO: fix spacing");
        ("type", `String "TEXT");
        ("characters", `String "TODO: fix spacing issue here");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 0.0);
          ("width", `Float 200.0); ("height", `Float 30.0);
        ]);
        ("children", `List []);
      ];
    ]);
  ]

(* Document wrapper for the rich frame *)
let rich_document =
  `Assoc [
    ("document", `Assoc [
      ("id", `String "0:0");
      ("name", `String "Doc");
      ("type", `String "DOCUMENT");
      ("children", `List [
        `Assoc [
          ("id", `String "1:0");
          ("name", `String "Page 1");
          ("type", `String "CANVAS");
          ("children", `List [rich_frame_node])
        ]
      ])
    ])
  ]

(* Node wrapper for rich frame *)
let rich_nodes_response node_id =
  `Assoc [("nodes", `Assoc [
    (node_id, `Assoc [("document", rich_frame_node)])
  ])]

(* Node with many children for large node testing *)
let make_many_children n =
  List.init n (fun i ->
    `Assoc [
      ("id", `String (Printf.sprintf "c:%d" i));
      ("name", `String (Printf.sprintf "Child%d" i));
      ("type", `String "RECTANGLE");
      ("visible", `Bool true);
      ("opacity", `Float 1.0);
      ("absoluteBoundingBox", `Assoc [
        ("x", `Float (float_of_int (i * 10)));
        ("y", `Float 0.0);
        ("width", `Float 50.0); ("height", `Float 50.0);
      ]);
      ("children", `List []);
    ])

let large_node_response node_id n =
  `Assoc [("nodes", `Assoc [
    (node_id, `Assoc [
      ("document", `Assoc [
        ("id", `String node_id);
        ("name", `String "LargeFrame");
        ("type", `String "FRAME");
        ("visible", `Bool true);
        ("opacity", `Float 1.0);
        ("absoluteBoundingBox", `Assoc [
          ("x", `Float 0.0); ("y", `Float 0.0);
          ("width", `Float 1000.0); ("height", `Float 1000.0);
        ]);
        ("children", `List (make_many_children n));
      ])
    ])
  ])]

(* ================================================================
   Mock effects with configurable responses
   ================================================================ *)

(* Default mock that returns rich data *)
let with_mock_effects ?(get_file_result=Ok rich_document)
                       ?(get_nodes_fn=fun _node_ids -> Ok (rich_nodes_response "1:1"))
                       ?(get_images_fn=fun node_ids ->
                          let img_entries = List.map (fun nid ->
                            (nid, `String "https://example.com/img.png")
                          ) node_ids in
                          Ok (`Assoc [("images", `Assoc img_entries)]))
                       ?(get_file_images_result=Ok (`Assoc [("images", `Assoc [
                          ("img:abc", `String "https://example.com/fill.png");
                          ("img:def", `String "not-a-url");
                       ])]))
                       ?(get_file_meta_result=Ok (`Assoc [
                          ("components", `Assoc []);
                          ("componentSets", `Assoc []);
                          ("styles", `Assoc [])
                        ]))
                       ?(download_url_result=Ok "/tmp/downloaded.png")
                       ?(get_variables_result=Ok (`Assoc [("variables", `Assoc [])]))
                       ?(get_file_styles_result=Ok (`Assoc [("data", `List [])]))
                       f =
  Effect.Deep.try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Figma_effects.Figma_get_file _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k get_file_result)

        | Figma_effects.Figma_get_nodes { node_ids; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (get_nodes_fn node_ids))

        | Figma_effects.Figma_get_images { node_ids; _ } ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (get_images_fn node_ids))

        | Figma_effects.Figma_get_file_images _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k get_file_images_result)

        | Figma_effects.Figma_get_file_meta _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k get_file_meta_result)

        | Figma_effects.Figma_download_url _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k download_url_result)

        | Figma_effects.Figma_get_variables _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k get_variables_result)

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
              Effect.Deep.continue k get_file_styles_result)
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
        | Figma_effects.Figma_post_file_comment _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("id", `String "comment-1")])))
        | Figma_effects.Figma_create_webhook _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("id", `String "webhook-1")])))
        | Figma_effects.Figma_get_dev_resources _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("dev_resources", `List [])])))
        | Figma_effects.Figma_add_dev_resource _ ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (Ok (`Assoc [("id", `String "dev-res-1")])))
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


(* ================================================================
   Tests: handle_get_file branches
   ================================================================ *)

let test_get_file_format_raw () =
  with_mock_effects (fun () ->
    let args = args_of [("file_key", `String "f1"); ("format", `String "raw")] in
    check_ok "get_file raw" (Mcp_api_handlers.handle_get_file args))

let test_get_file_with_optional_params () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("depth", `Int 2);
      ("geometry", `String "paths");
      ("plugin_data", `String "shared");
      ("version", `String "v123");
    ] in
    check_ok "get_file with opts" (Mcp_api_handlers.handle_get_file args))

let test_get_file_document_none () =
  (* Return file without "document" key -> falls back to full JSON *)
  with_mock_effects
    ~get_file_result:(Ok (`Assoc [("name", `String "NoDoc")]))
    (fun () ->
      let args = args_of [("file_key", `String "f1")] in
      check_ok "get_file no document" (Mcp_api_handlers.handle_get_file args))

let test_get_file_api_error () =
  with_mock_effects
    ~get_file_result:(Error "API timeout")
    (fun () ->
      let args = args_of [("file_key", `String "f1")] in
      check_error "get_file api error" (Mcp_api_handlers.handle_get_file args))

(* ================================================================
   Tests: handle_get_file_meta branches
   ================================================================ *)

let test_get_file_meta_with_version () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("version", `String "v456");
    ] in
    check_ok "get_file_meta with version" (Mcp_api_handlers.handle_get_file_meta args))

(* ================================================================
   Tests: handle_get_node branches
   ================================================================ *)

let test_get_node_format_raw () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("format", `String "raw");
    ] in
    check_ok "get_node raw" (Mcp_api_handlers.handle_get_node args))

let test_get_node_with_opts () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("depth", `Int 3);
      ("geometry", `String "paths");
      ("plugin_data", `String "shared");
      ("version", `String "v789");
    ] in
    check_ok "get_node with opts" (Mcp_api_handlers.handle_get_node args))

let test_get_node_not_found () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("nodes", `Assoc [("other:id", `Assoc [("document", `Null)])])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
      ] in
      check_error "get_node not found" (Mcp_api_handlers.handle_get_node args))

let test_get_node_data_none () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      (* node entry exists but has no "document" field *)
      Ok (`Assoc [("nodes", `Assoc [("1:1", `Assoc [("other", `String "val")])])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1-1");
      ] in
      (* node_data is None because document key is missing *)
      check_error "get_node data none" (Mcp_api_handlers.handle_get_node args))

(* ================================================================
   Tests: handle_get_node_bundle branches
   ================================================================ *)

let test_bundle_include_meta_false () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
      ("include_raw", `Bool false);
    ] in
    check_ok "bundle meta false" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_download_true () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("download", `Bool true);
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool true);
      ("include_plugin", `Bool false);
    ] in
    check_ok "bundle download" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_node_not_found () =
  Figma_cache.invalidate ();
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("nodes", `Assoc [])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f-notfound");
        ("node_id", `String "99:99");
      ] in
      check_error "bundle node not found" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_nodes_missing_field () =
  Figma_cache.invalidate ();
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("other", `Null)]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f-nomissing");
        ("node_id", `String "88:88");
      ] in
      check_error "bundle nodes missing" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_document_null () =
  Figma_cache.invalidate ();
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      (* document key absent → handler's member returns None → Error path *)
      Ok (`Assoc [("nodes", `Assoc [("77:77", `Assoc [("components", `Assoc [])])])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f-docnull");
        ("node_id", `String "77:77");
      ] in
      check_error "bundle doc null" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_api_error () =
  Figma_cache.invalidate ();
  with_mock_effects
    ~get_nodes_fn:(fun _ids -> Error "Rate limited")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f-apierr");
        ("node_id", `String "66:66");
      ] in
      check_error "bundle api error" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_image_error () =
  with_mock_effects
    ~get_images_fn:(fun _ids -> Error "Image service down")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("include_meta", `Bool false);
        ("include_variables", `Bool false);
        ("include_image_fills", `Bool false);
        ("include_plugin", `Bool false);
      ] in
      check_ok "bundle image error" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_download_no_url () =
  with_mock_effects
    ~get_images_fn:(fun _ids ->
      Ok (`Assoc [("images", `Assoc [("1:1", `Null)])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("download", `Bool true);
        ("include_meta", `Bool false);
        ("include_variables", `Bool false);
        ("include_image_fills", `Bool false);
        ("include_plugin", `Bool false);
      ] in
      check_ok "bundle download no url" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_format_raw () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("format", `String "raw");
      ("include_meta", `Bool false);
      ("include_variables", `Bool false);
      ("include_image_fills", `Bool false);
      ("include_plugin", `Bool false);
    ] in
    check_ok "bundle format raw" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_nodes_unexpected_format () =
  Figma_cache.invalidate ();
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("nodes", `String "unexpected")]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f-unexpfmt");
        ("node_id", `String "55:55");
      ] in
      check_error "bundle unexpected format" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_meta_error () =
  with_mock_effects
    ~get_file_meta_result:(Error "Meta service down")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("include_meta", `Bool true);
        ("include_variables", `Bool false);
        ("include_image_fills", `Bool false);
        ("include_plugin", `Bool false);
      ] in
      check_ok "bundle meta error" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_variables_error () =
  with_mock_effects
    ~get_variables_result:(Error "Variables API error")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("include_meta", `Bool false);
        ("include_variables", `Bool true);
        ("include_image_fills", `Bool false);
        ("include_plugin", `Bool false);
      ] in
      check_ok "bundle var error" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_file_images_error () =
  with_mock_effects
    ~get_file_images_result:(Error "File images error")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("include_meta", `Bool false);
        ("include_variables", `Bool false);
        ("include_image_fills", `Bool true);
        ("include_plugin", `Bool false);
      ] in
      check_ok "bundle fill error" (Mcp_api_handlers.handle_get_node_bundle args))

let test_bundle_download_error () =
  with_mock_effects
    ~download_url_result:(Error "Disk full")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("download", `Bool true);
        ("include_meta", `Bool false);
        ("include_variables", `Bool false);
        ("include_image_fills", `Bool false);
        ("include_plugin", `Bool false);
      ] in
      check_ok "bundle dl error" (Mcp_api_handlers.handle_get_node_bundle args))

(* ================================================================
   Tests: handle_get_node_summary branches
   ================================================================ *)

let test_summary_with_children () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
    ] in
    check_ok "summary with children" (Mcp_api_handlers.handle_get_node_summary args))

let test_summary_truncated () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("max_children", `Int 2);
    ] in
    let result = Mcp_api_handlers.handle_get_node_summary args in
    check_ok "summary truncated" result;
    match result with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        Alcotest.(check bool) "truncated true" true (str_contains s "true")
    | Error _ -> ())

let test_summary_node_not_found () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("nodes", `Assoc [])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
      ] in
      check_error "summary not found" (Mcp_api_handlers.handle_get_node_summary args))

let test_summary_api_error () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids -> Error "timeout")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
      ] in
      check_error "summary api error" (Mcp_api_handlers.handle_get_node_summary args))

let test_summary_doc_null () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("nodes", `Assoc [("1:1", `Assoc [("document", `Null)])])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1-1");
      ] in
      check_error "summary doc null" (Mcp_api_handlers.handle_get_node_summary args))

(* ================================================================
   Tests: handle_select_nodes branches
   ================================================================ *)

let test_select_text_mode_exclude () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("text_mode", `String "exclude");
    ] in
    check_ok "select text_mode exclude" (Mcp_api_handlers.handle_select_nodes args))

let test_select_text_mode_only () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("text_mode", `String "only");
    ] in
    check_ok "select text_mode only" (Mcp_api_handlers.handle_select_nodes args))

let test_select_text_mode_invalid () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("text_mode", `String "bogus");
    ] in
    let result = Mcp_api_handlers.handle_select_nodes args in
    check_ok "select invalid text_mode" result;
    match result with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        Alcotest.(check bool) "warning present" true
          (str_contains s "Invalid text_mode")
    | Error _ -> ())

let test_select_layout_only () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("layout_only", `Bool true);
    ] in
    check_ok "select layout_only" (Mcp_api_handlers.handle_select_nodes args))

let test_select_auto_layout_only () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("auto_layout_only", `Bool true);
    ] in
    check_ok "select auto_layout_only" (Mcp_api_handlers.handle_select_nodes args))

let test_select_high_threshold_fallback () =
  (* Very high threshold -> no nodes pass -> fallback_top_scores mode *)
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("score_threshold", `Float 999.0);
    ] in
    let result = Mcp_api_handlers.handle_select_nodes args in
    check_ok "select fallback" result;
    match result with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        Alcotest.(check bool) "fallback mode" true
          (str_contains s "fallback_top_scores")
    | Error _ -> ())

let test_select_preview_false () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("preview", `Bool false);
    ] in
    check_ok "select no preview" (Mcp_api_handlers.handle_select_nodes args))

let test_select_preview_error () =
  with_mock_effects
    ~get_images_fn:(fun _ids -> Error "Preview error")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("preview", `Bool true);
      ] in
      check_ok "select preview error" (Mcp_api_handlers.handle_select_nodes args))

let test_select_summary_depth_clamped () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("summary_depth", `Int 100);
    ] in
    let result = Mcp_api_handlers.handle_select_nodes args in
    check_ok "select depth clamped" result;
    match result with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        Alcotest.(check bool) "depth clamped warning" true
          (str_contains s "summary_depth clamped")
    | Error _ -> ())

let test_select_preview_scale_clamped () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("preview_scale", `Float 10.0);
    ] in
    let result = Mcp_api_handlers.handle_select_nodes args in
    check_ok "select scale clamped" result;
    match result with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        Alcotest.(check bool) "scale warning" true
          (str_contains s "preview_scale clamped")
    | Error _ -> ())

let test_select_negative_summary_depth () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("summary_depth", `Int (-1));
    ] in
    check_ok "select negative depth" (Mcp_api_handlers.handle_select_nodes args))

let test_select_node_null () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("nodes", `Assoc [("1:1", `Null)])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1-1");
      ] in
      check_error "select node null" (Mcp_api_handlers.handle_select_nodes args))

let test_select_doc_null () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("nodes", `Assoc [("1:1", `Assoc [("document", `Null)])])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1-1");
      ] in
      check_error "select doc null" (Mcp_api_handlers.handle_select_nodes args))

(* ================================================================
   Tests: handle_get_node_chunk branches
   ================================================================ *)

let test_chunk_depth_end_lt_start () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("depth_start", `Int 5);
      ("depth_end", `Int 2);
    ] in
    check_error "chunk depth error" (Mcp_api_handlers.handle_get_node_chunk args))

let test_chunk_with_max_children () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("max_children", `Int 3);
    ] in
    check_ok "chunk max_children" (Mcp_api_handlers.handle_get_node_chunk args))

let test_chunk_auto_trim () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("auto_trim_children", `Bool true);
      ("auto_trim_limit", `Int 5);
    ] in
    check_ok "chunk auto_trim" (Mcp_api_handlers.handle_get_node_chunk args))

let test_chunk_include_styles () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("include_styles", `Bool true);
    ] in
    check_ok "chunk styles" (Mcp_api_handlers.handle_get_node_chunk args))

let test_chunk_error_on_large () =
  Figma_cache.invalidate ();
  (* Use mock that returns a node with > 500 children *)
  with_mock_effects
    ~get_nodes_fn:(fun _ids -> Ok (large_node_response "44:44" 600))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f-chunkerr");
        ("node_id", `String "44:44");
        ("error_on_large", `Bool true);
        ("warn_threshold", `Int 500);
      ] in
      check_error "chunk error large" (Mcp_api_handlers.handle_get_node_chunk args))

let test_chunk_warn_large () =
  Figma_cache.invalidate ();
  with_mock_effects
    ~get_nodes_fn:(fun _ids -> Ok (large_node_response "43:43" 600))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f-chunkwarn");
        ("node_id", `String "43:43");
        ("warn_large", `Bool true);
        ("warn_threshold", `Int 500);
      ] in
      let result = Mcp_api_handlers.handle_get_node_chunk args in
      (* Returns Ok even with large node when error_on_large=false *)
      check_ok "chunk warn large" result;
      (* Warning may be in saved file if result exceeds max_inline_size.
         Just verify the handler completed successfully with Ok. *)
      match result with
      | Ok json ->
          let s = Yojson.Safe.to_string json in
          (* Check either inline warning or large_result indicator *)
          let has_warning = str_contains s "Large node" in
          let has_large_result = str_contains s "large_result" in
          Alcotest.(check bool) "warning or large_result" true (has_warning || has_large_result)
      | Error _ -> ())

let test_chunk_format_raw () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("format", `String "raw");
    ] in
    check_ok "chunk raw" (Mcp_api_handlers.handle_get_node_chunk args))

let test_chunk_node_not_found () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("nodes", `Assoc [("1:1", `Null)])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1-1");
      ] in
      check_error "chunk not found" (Mcp_api_handlers.handle_get_node_chunk args))

let test_chunk_doc_null () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids ->
      Ok (`Assoc [("nodes", `Assoc [("1:1", `Assoc [("document", `Null)])])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1-1");
      ] in
      check_error "chunk doc null" (Mcp_api_handlers.handle_get_node_chunk args))

let test_chunk_depth_range () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_id", `String "1:1");
      ("depth_start", `Int 1);
      ("depth_end", `Int 1);
    ] in
    check_ok "chunk depth range" (Mcp_api_handlers.handle_get_node_chunk args))

let test_chunk_styles_error () =
  with_mock_effects
    ~get_file_styles_result:(Error "Styles unavailable")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("include_styles", `Bool true);
      ] in
      check_ok "chunk styles error" (Mcp_api_handlers.handle_get_node_chunk args))

(* ================================================================
   Tests: handle_export_image branches
   ================================================================ *)

let test_export_image_download () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_ids", `String "1:1,2:1");
      ("download", `Bool true);
    ] in
    check_ok "export download" (Mcp_api_handlers.handle_export_image args))

let test_export_image_download_error () =
  with_mock_effects
    ~download_url_result:(Error "Disk error")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_ids", `String "1:1");
        ("download", `Bool true);
      ] in
      check_ok "export dl error" (Mcp_api_handlers.handle_export_image args))

let test_export_image_multi_progress () =
  (* 3+ images triggers progress *)
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_ids", `String "1:1,2:1,3:1");
      ("download", `Bool true);
    ] in
    check_ok "export multi progress" (Mcp_api_handlers.handle_export_image args))

let test_export_image_no_images () =
  with_mock_effects
    ~get_images_fn:(fun _ids -> Ok (`Assoc [("images", `Null)]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_ids", `String "1:1");
      ] in
      check_ok "export no images" (Mcp_api_handlers.handle_export_image args))

(* ================================================================
   Tests: handle_export_smart branches
   ================================================================ *)

let test_export_smart_basic () =
  with_mock_effects
    ~get_nodes_fn:(fun ids ->
      let nid = List.hd ids in
      Ok (`Assoc [("nodes", `Assoc [
        (nid, `Assoc [
          ("document", `Assoc [
            ("id", `String nid);
            ("name", `String "SmartFrame");
            ("type", `String "FRAME");
            ("absoluteBoundingBox", `Assoc [
              ("x", `Float 0.0); ("y", `Float 0.0);
              ("width", `Float 200.0); ("height", `Float 200.0);
            ]);
            ("children", `List []);
          ])
        ])
      ])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
      ] in
      check_ok "export smart basic" (Mcp_api_handlers.handle_export_smart args))

let test_export_smart_download () =
  with_mock_effects
    ~get_nodes_fn:(fun ids ->
      let nid = List.hd ids in
      Ok (`Assoc [("nodes", `Assoc [
        (nid, `Assoc [
          ("document", `Assoc [
            ("id", `String nid);
            ("name", `String "SmartFrame");
            ("type", `String "FRAME");
            ("absoluteBoundingBox", `Assoc [
              ("x", `Float 0.0); ("y", `Float 0.0);
              ("width", `Float 200.0); ("height", `Float 200.0);
            ]);
            ("children", `List []);
          ])
        ])
      ])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("download", `Bool true);
      ] in
      check_ok "export smart download" (Mcp_api_handlers.handle_export_smart args))

let test_export_smart_debug () =
  with_mock_effects
    ~get_nodes_fn:(fun ids ->
      let nid = List.hd ids in
      Ok (`Assoc [("nodes", `Assoc [
        (nid, `Assoc [
          ("document", `Assoc [
            ("id", `String nid);
            ("name", `String "SmartFrame");
            ("type", `String "FRAME");
            ("absoluteBoundingBox", `Assoc [
              ("x", `Float 0.0); ("y", `Float 0.0);
              ("width", `Float 200.0); ("height", `Float 200.0);
            ]);
            ("children", `List []);
          ])
        ])
      ])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
        ("debug", `Bool true);
      ] in
      check_ok "export smart debug" (Mcp_api_handlers.handle_export_smart args))

let test_export_smart_large_auto_scale () =
  (* Large node that triggers auto_scale *)
  with_mock_effects
    ~get_nodes_fn:(fun ids ->
      let nid = List.hd ids in
      Ok (`Assoc [("nodes", `Assoc [
        (nid, `Assoc [
          ("document", `Assoc [
            ("id", `String nid);
            ("name", `String "HugeFrame");
            ("type", `String "FRAME");
            ("absoluteBoundingBox", `Assoc [
              ("x", `Float 0.0); ("y", `Float 0.0);
              ("width", `Float 10000.0); ("height", `Float 10000.0);
            ]);
            ("children", `List []);
          ])
        ])
      ])]))
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_id", `String "1:1");
      ] in
      check_ok "export smart auto_scale" (Mcp_api_handlers.handle_export_smart args))

(* ================================================================
   Tests: handle_get_image_fills branches
   ================================================================ *)

let test_image_fills_download () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("download", `Bool true);
    ] in
    check_ok "image_fills download" (Mcp_api_handlers.handle_get_image_fills args))

let test_image_fills_no_download () =
  with_mock_effects (fun () ->
    let args = args_of [("file_key", `String "f1")] in
    check_ok "image_fills no download" (Mcp_api_handlers.handle_get_image_fills args))

let test_image_fills_api_error () =
  with_mock_effects
    ~get_file_images_result:(Error "Fill error")
    (fun () ->
      let args = args_of [("file_key", `String "f1")] in
      check_error "image_fills error" (Mcp_api_handlers.handle_get_image_fills args))

(* ================================================================
   Tests: handle_get_nodes branches
   ================================================================ *)

let test_get_nodes_format_fidelity () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_ids", `String "1:1");
      ("format", `String "fidelity");
    ] in
    check_ok "get_nodes fidelity" (Mcp_api_handlers.handle_get_nodes args))

let test_get_nodes_format_raw () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_ids", `String "1:1");
      ("format", `String "raw");
    ] in
    check_ok "get_nodes raw" (Mcp_api_handlers.handle_get_nodes args))

let test_get_nodes_api_error () =
  with_mock_effects
    ~get_nodes_fn:(fun _ids -> Error "API error")
    (fun () ->
      let args = args_of [
        ("file_key", `String "f1");
        ("node_ids", `String "1:1");
      ] in
      check_error "get_nodes error" (Mcp_api_handlers.handle_get_nodes args))

let test_get_nodes_with_opts () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("node_ids", `String "1:1,2:1");
      ("depth", `Int 2);
      ("geometry", `String "paths");
      ("plugin_data", `String "shared");
      ("version", `String "v1");
    ] in
    check_ok "get_nodes with opts" (Mcp_api_handlers.handle_get_nodes args))

(* ================================================================
   Tests: handle_post_comment branches
   ================================================================ *)

let test_post_comment_with_node_id () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("message", `String "Nice work");
      ("x", `Float 10.0);
      ("y", `Float 20.0);
      ("node_id", `String "1:1");
    ] in
    check_ok "post_comment with node_id" (Mcp_api_handlers.handle_post_comment args))

let test_post_comment_no_node_id () =
  with_mock_effects (fun () ->
    let args = args_of [
      ("file_key", `String "f1");
      ("message", `String "General comment");
      ("x", `Float 10.0);
      ("y", `Float 20.0);
    ] in
    check_ok "post_comment no node_id" (Mcp_api_handlers.handle_post_comment args))

(* ================================================================
   Tests: handle_list_screens
   ================================================================ *)

let test_list_screens_ok () =
  with_mock_effects (fun () ->
    let args = args_of [("file_key", `String "f1")] in
    check_ok "list_screens ok" (Mcp_api_handlers.handle_list_screens args))

let test_list_screens_error () =
  with_mock_effects
    ~get_file_result:(Error "File not found")
    (fun () ->
      let args = args_of [("file_key", `String "f1")] in
      check_error "list_screens error" (Mcp_api_handlers.handle_list_screens args))

(* ================================================================
   Tests: passthrough handlers (components, styles)
   ================================================================ *)

let test_get_file_components () =
  with_mock_effects (fun () ->
    let args = args_of [("file_key", `String "f1")] in
    check_ok "file_components" (Mcp_api_handlers.handle_get_file_components args))

let test_get_file_component_sets () =
  with_mock_effects (fun () ->
    let args = args_of [("file_key", `String "f1")] in
    check_ok "file_component_sets" (Mcp_api_handlers.handle_get_file_component_sets args))

let test_get_file_styles () =
  with_mock_effects (fun () ->
    let args = args_of [("file_key", `String "f1")] in
    check_ok "file_styles" (Mcp_api_handlers.handle_get_file_styles args))

let test_get_component () =
  with_mock_effects (fun () ->
    let args = args_of [("component_key", `String "comp1")] in
    check_ok "component" (Mcp_api_handlers.handle_get_component args))

let test_get_component_set () =
  with_mock_effects (fun () ->
    let args = args_of [("component_set_key", `String "cs1")] in
    check_ok "component_set" (Mcp_api_handlers.handle_get_component_set args))

let test_get_team_components () =
  with_mock_effects (fun () ->
    let args = args_of [("team_id", `String "t1")] in
    check_ok "team_components" (Mcp_api_handlers.handle_get_team_components args))

let test_get_team_component_sets () =
  with_mock_effects (fun () ->
    let args = args_of [("team_id", `String "t1")] in
    check_ok "team_component_sets" (Mcp_api_handlers.handle_get_team_component_sets args))

let test_get_team_styles () =
  with_mock_effects (fun () ->
    let args = args_of [("team_id", `String "t1")] in
    check_ok "team_styles" (Mcp_api_handlers.handle_get_team_styles args))

(* ================================================================
   Tests: handle_get_file_versions / handle_get_file_comments
   ================================================================ *)

let test_get_file_versions () =
  with_mock_effects (fun () ->
    let args = args_of [("file_key", `String "f1")] in
    check_ok "file_versions" (Mcp_api_handlers.handle_get_file_versions args))

let test_get_file_comments () =
  with_mock_effects (fun () ->
    let args = args_of [("file_key", `String "f1")] in
    check_ok "file_comments" (Mcp_api_handlers.handle_get_file_comments args))


(* ================================================================
   Test runner
   ================================================================ *)

let () =
  Alcotest.run "mcp_api_handlers_w4" [
    ("get_file", [
      Alcotest.test_case "format raw" `Quick test_get_file_format_raw;
      Alcotest.test_case "optional params" `Quick test_get_file_with_optional_params;
      Alcotest.test_case "document none" `Quick test_get_file_document_none;
      Alcotest.test_case "api error" `Quick test_get_file_api_error;
    ]);
    ("get_file_meta", [
      Alcotest.test_case "with version" `Quick test_get_file_meta_with_version;
    ]);
    ("get_node", [
      Alcotest.test_case "format raw" `Quick test_get_node_format_raw;
      Alcotest.test_case "with opts" `Quick test_get_node_with_opts;
      Alcotest.test_case "not found" `Quick test_get_node_not_found;
      Alcotest.test_case "data none" `Quick test_get_node_data_none;
    ]);
    ("get_node_bundle", [
      Alcotest.test_case "meta false" `Quick test_bundle_include_meta_false;
      Alcotest.test_case "download true" `Quick test_bundle_download_true;
      Alcotest.test_case "node not found" `Quick test_bundle_node_not_found;
      Alcotest.test_case "nodes missing field" `Quick test_bundle_nodes_missing_field;
      Alcotest.test_case "document null" `Quick test_bundle_document_null;
      Alcotest.test_case "api error" `Quick test_bundle_api_error;
      Alcotest.test_case "image error" `Quick test_bundle_image_error;
      Alcotest.test_case "download no url" `Quick test_bundle_download_no_url;
      Alcotest.test_case "format raw" `Quick test_bundle_format_raw;
      Alcotest.test_case "unexpected format" `Quick test_bundle_nodes_unexpected_format;
      Alcotest.test_case "meta error" `Quick test_bundle_meta_error;
      Alcotest.test_case "variables error" `Quick test_bundle_variables_error;
      Alcotest.test_case "file images error" `Quick test_bundle_file_images_error;
      Alcotest.test_case "download error" `Quick test_bundle_download_error;
    ]);
    ("get_node_summary", [
      Alcotest.test_case "with children" `Quick test_summary_with_children;
      Alcotest.test_case "truncated" `Quick test_summary_truncated;
      Alcotest.test_case "not found" `Quick test_summary_node_not_found;
      Alcotest.test_case "api error" `Quick test_summary_api_error;
      Alcotest.test_case "doc null" `Quick test_summary_doc_null;
    ]);
    ("select_nodes", [
      Alcotest.test_case "text_mode exclude" `Quick test_select_text_mode_exclude;
      Alcotest.test_case "text_mode only" `Quick test_select_text_mode_only;
      Alcotest.test_case "text_mode invalid" `Quick test_select_text_mode_invalid;
      Alcotest.test_case "layout_only" `Quick test_select_layout_only;
      Alcotest.test_case "auto_layout_only" `Quick test_select_auto_layout_only;
      Alcotest.test_case "high threshold fallback" `Quick test_select_high_threshold_fallback;
      Alcotest.test_case "preview false" `Quick test_select_preview_false;
      Alcotest.test_case "preview error" `Quick test_select_preview_error;
      Alcotest.test_case "summary_depth clamped" `Quick test_select_summary_depth_clamped;
      Alcotest.test_case "preview_scale clamped" `Quick test_select_preview_scale_clamped;
      Alcotest.test_case "negative summary_depth" `Quick test_select_negative_summary_depth;
      Alcotest.test_case "node null" `Quick test_select_node_null;
      Alcotest.test_case "doc null" `Quick test_select_doc_null;
    ]);
    ("get_node_chunk", [
      Alcotest.test_case "depth_end < start" `Quick test_chunk_depth_end_lt_start;
      Alcotest.test_case "max_children" `Quick test_chunk_with_max_children;
      Alcotest.test_case "auto_trim" `Quick test_chunk_auto_trim;
      Alcotest.test_case "include_styles" `Quick test_chunk_include_styles;
      Alcotest.test_case "error_on_large" `Quick test_chunk_error_on_large;
      Alcotest.test_case "warn_large" `Quick test_chunk_warn_large;
      Alcotest.test_case "format raw" `Quick test_chunk_format_raw;
      Alcotest.test_case "not found" `Quick test_chunk_node_not_found;
      Alcotest.test_case "doc null" `Quick test_chunk_doc_null;
      Alcotest.test_case "depth range" `Quick test_chunk_depth_range;
      Alcotest.test_case "styles error" `Quick test_chunk_styles_error;
    ]);
    ("export_image", [
      Alcotest.test_case "download" `Quick test_export_image_download;
      Alcotest.test_case "download error" `Quick test_export_image_download_error;
      Alcotest.test_case "multi progress" `Quick test_export_image_multi_progress;
      Alcotest.test_case "no images" `Quick test_export_image_no_images;
    ]);
    ("export_smart", [
      Alcotest.test_case "basic" `Quick test_export_smart_basic;
      Alcotest.test_case "download" `Quick test_export_smart_download;
      Alcotest.test_case "debug" `Quick test_export_smart_debug;
      Alcotest.test_case "auto_scale" `Quick test_export_smart_large_auto_scale;
    ]);
    ("image_fills", [
      Alcotest.test_case "download" `Quick test_image_fills_download;
      Alcotest.test_case "no download" `Quick test_image_fills_no_download;
      Alcotest.test_case "api error" `Quick test_image_fills_api_error;
    ]);
    ("get_nodes", [
      Alcotest.test_case "fidelity" `Quick test_get_nodes_format_fidelity;
      Alcotest.test_case "raw" `Quick test_get_nodes_format_raw;
      Alcotest.test_case "api error" `Quick test_get_nodes_api_error;
      Alcotest.test_case "with opts" `Quick test_get_nodes_with_opts;
    ]);
    ("post_comment", [
      Alcotest.test_case "with node_id" `Quick test_post_comment_with_node_id;
      Alcotest.test_case "no node_id" `Quick test_post_comment_no_node_id;
    ]);
    ("list_screens", [
      Alcotest.test_case "ok" `Quick test_list_screens_ok;
      Alcotest.test_case "error" `Quick test_list_screens_error;
    ]);
    ("passthrough", [
      Alcotest.test_case "file_components" `Quick test_get_file_components;
      Alcotest.test_case "file_component_sets" `Quick test_get_file_component_sets;
      Alcotest.test_case "file_styles" `Quick test_get_file_styles;
      Alcotest.test_case "component" `Quick test_get_component;
      Alcotest.test_case "component_set" `Quick test_get_component_set;
      Alcotest.test_case "team_components" `Quick test_get_team_components;
      Alcotest.test_case "team_component_sets" `Quick test_get_team_component_sets;
      Alcotest.test_case "team_styles" `Quick test_get_team_styles;
      Alcotest.test_case "file_versions" `Quick test_get_file_versions;
      Alcotest.test_case "file_comments" `Quick test_get_file_comments;
    ]);
  ]
