(** Coverage Wave 4 tests for mcp_helpers.ml, figma_effects.ml, server_metrics.ml.
    Tests mcp_helpers internal functions INDIRECTLY through exported API
    (fidelity_score_of_dsl, fidelity_score_of_bundle, count_text_segments, etc.)
    since the .mli hides internal helpers. *)

let () =
  Unix.putenv "FIGMA_TOKEN" "test-token-12345";
  let open Alcotest in

  (* ===== mcp_helpers.ml — remaining uncovered branches ===== *)

  (* --- fidelity_score_of_dsl: exercises coverage_for_section,
         count_assoc_fields, count_list_items, assoc_or_empty, list_member --- *)

  (* All sections present and full -> high score *)
  let test_fidelity_dsl_full () =
    let json = `Assoc [
      ("meta", `Assoc [("type", `String "FRAME"); ("name", `String "root")]);
      ("meta_missing", `List []);
      ("structure", `Assoc [("depth", `Int 2)]);
      ("structure_missing", `List []);
      ("geometry", `Assoc [("x", `Float 0.0); ("y", `Float 0.0)]);
      ("geometry_missing", `List []);
      ("layout", `Assoc [("mode", `String "NONE")]);
      ("layout_missing", `List []);
      ("paint", `Assoc [("fills", `List [])]);
      ("paint_missing", `List []);
    ] in
    let (score, missing, _details) = Mcp_helpers.fidelity_score_of_dsl json in
    check bool "high score" true (score > 0.5);
    check int "0 missing" 0 missing
  in

  (* All sections empty (no keys) -> score = 1.0 (no fields = perfect coverage) *)
  let test_fidelity_dsl_empty () =
    let json = `Assoc [] in
    let (score, missing, _details) = Mcp_helpers.fidelity_score_of_dsl json in
    check (float 0.01) "empty = 1.0" 1.0 score;
    check int "0 missing" 0 missing
  in

  (* Some sections have missing fields -> partial score *)
  let test_fidelity_dsl_partial () =
    let json = `Assoc [
      ("meta", `Assoc [("type", `String "FRAME")]);
      ("meta_missing", `List [`String "name"; `String "visible"]);
      ("structure", `Assoc []);
      ("structure_missing", `List [`String "depth"]);
    ] in
    let (score, missing, _details) = Mcp_helpers.fidelity_score_of_dsl json in
    check bool "partial score < 1.0" true (score < 1.0);
    check int "3 total missing" 3 missing
  in

  (* Section present but missing all -> coverage 0/N *)
  let test_fidelity_dsl_all_missing () =
    let json = `Assoc [
      ("meta", `Assoc []);
      ("meta_missing", `List [`String "type"; `String "name"]);
      ("geometry", `Assoc []);
      ("geometry_missing", `List [`String "x"; `String "y"; `String "w"]);
    ] in
    let (score, missing, _details) = Mcp_helpers.fidelity_score_of_dsl json in
    check bool "low score" true (score < 1.0);
    check int "5 total missing" 5 missing
  in

  (* --- fidelity_score_with_overrides: explicit override replaces section --- *)
  let test_fidelity_with_overrides () =
    let json = `Assoc [
      ("meta", `Assoc [("type", `String "FRAME")]);
      ("meta_missing", `List [`String "name"]);
    ] in
    (* Override meta to be perfect *)
    let overrides = [("meta", (1.0, 5, 0, 5))] in
    let (score, missing, details) = Mcp_helpers.fidelity_score_with_overrides json overrides in
    (* meta override -> 5 present, 0 missing *)
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "meta" lst with
          | Some (`Assoc fields) ->
              check (float 0.01) "override score" 1.0
                (match List.assoc_opt "score" fields with Some (`Float f) -> f | _ -> -1.0);
              check int "override present" 5
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected meta details")
     | _ -> fail "expected assoc details");
    (* Only non-overridden sections contribute to missing *)
    check bool "score improved" true (score > 0.0);
    ignore missing
  in

  (* Override with explicit score value *)
  let test_override_section_with_score () =
    let (score, present, missing, total) =
      Mcp_helpers.override_section ~score:0.75 ~present:3 ~missing:1 ~total:4 ()
    in
    check (float 0.01) "custom score" 0.75 score;
    check int "present" 3 present;
    check int "missing" 1 missing;
    check int "total" 4 total
  in

  (* Override without explicit score -> computed from present/total *)
  let test_override_section_auto_score () =
    let (score, present, missing, total) =
      Mcp_helpers.override_section ~present:2 ~missing:3 ~total:5 ()
    in
    check (float 0.01) "auto score" 0.4 score;
    check int "present" 2 present;
    check int "missing" 3 missing;
    check int "total" 5 total
  in

  (* Override with total = 0 -> score = 1.0 *)
  let test_override_section_zero_total () =
    let (score, _, _, _) =
      Mcp_helpers.override_section ~present:0 ~missing:0 ~total:0 ()
    in
    check (float 0.01) "zero total = 1.0" 1.0 score
  in

  (* --- fidelity_score_of_bundle: exercises image_refs_of_dsl,
         image_fill_map, plugin_ok, count_text_nodes_dsl,
         count_text_nodes_with_segments, plugin_text_nodes_with_segments,
         variables_counts, string_list_of_json --- *)

  (* include_image_fills with refs that match *)
  let test_bundle_image_fills_match () =
    let dsl_json = `Assoc [
      ("assets", `Assoc [
        ("image_refs", `List [`String "ref1"; `String "ref2"; `String "ref3"])
      ]);
      ("assets_missing", `List []);
    ] in
    let image_fills = `Assoc [
      ("images", `Assoc [
        ("ref1", `String "https://example.com/img1.png");
        ("ref2", `String "https://example.com/img2.png");
      ])
    ] in
    let (score, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:true ~include_plugin:false
    in
    check bool "score > 0" true (score > 0.0);
    (* assets override: 2 present out of 3 refs *)
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "assets" lst with
          | Some (`Assoc fields) ->
              check int "present" 2
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1);
              check int "total" 3
                (match List.assoc_opt "total" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected assets details")
     | _ -> fail "expected assoc details")
  in

  (* include_image_fills with no refs -> no override *)
  let test_bundle_image_fills_no_refs () =
    let dsl_json = `Assoc [
      ("assets", `Assoc [("other", `String "x")]);
    ] in
    let image_fills = `Assoc [("images", `Assoc [("r", `String "u")])] in
    let (_score, _, _details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:true ~include_plugin:false
    in
    ()  (* no crash = success, assets not overridden *)
  in

  (* include_image_fills with non-assoc image_fills *)
  let test_bundle_image_fills_non_assoc () =
    let dsl_json = `Assoc [
      ("assets", `Assoc [
        ("image_refs", `List [`String "ref1"])
      ]);
    ] in
    let (_score, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:true ~include_plugin:false
    in
    ()  (* image_fill_map returns [] for non-assoc, so 0 present *)
  in

  (* include_image_fills: mixed types in image_refs (non-string filtered) *)
  let test_bundle_image_refs_mixed () =
    let dsl_json = `Assoc [
      ("assets", `Assoc [
        ("image_refs", `List [`String "r1"; `Int 42; `Bool true; `String "r2"])
      ]);
    ] in
    let image_fills = `Assoc [("images", `Assoc [("r1", `String "url1")])] in
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:true ~include_plugin:false
    in
    (* Only 2 string refs: r1 and r2. r1 matched, r2 not. total=2, present=1 *)
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "assets" lst with
          | Some (`Assoc fields) ->
              check int "total 2 string refs" 2
                (match List.assoc_opt "total" fields with Some (`Int n) -> n | _ -> -1);
              check int "present 1" 1
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected assets details")
     | _ -> fail "expected assoc details")
  in

  (* include_image_fills: non-assoc assets in dsl -> no refs *)
  let test_bundle_image_fills_bad_assets () =
    let dsl_json = `Assoc [("assets", `String "bad")] in
    let (_score, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:true ~include_plugin:false
    in
    ()
  in

  (* include_image_fills: images key not assoc *)
  let test_bundle_image_fills_images_not_assoc () =
    let dsl_json = `Assoc [
      ("assets", `Assoc [("image_refs", `List [`String "r1"])]);
    ] in
    let image_fills = `Assoc [("images", `List [])] in
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:true ~include_plugin:false
    in
    (* image_fill_map returns [] for non-assoc images, so 0 present *)
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "assets" lst with
          | Some (`Assoc fields) ->
              check int "present 0" 0
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected assets details")
     | _ -> fail "expected assoc details")
  in

  (* include_plugin with ok=true plugin -> text_segments override *)
  let test_bundle_plugin_ok () =
    let text_node = `Assoc [("meta", `Assoc [("type", `String "TEXT")])] in
    let dsl_json = `Assoc [
      ("children", `List [text_node; text_node; text_node]);
    ] in
    let plugin_snapshot = `Assoc [
      ("ok", `Bool true);
      ("payload", `Assoc [
        ("text", `Assoc [("segments", `List [`String "s1"])]);
        ("children", `List [
          `Assoc [("text", `Assoc [("segments", `List [`String "s2"])])];
        ]);
      ]);
    ] in
    let (score, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills:`Null ~plugin_snapshot
      ~include_variables:false ~include_image_fills:false ~include_plugin:true
    in
    check bool "score > 0" true (score > 0.0);
    (* plugin override: text_segments section with 2 present out of 3 text nodes *)
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "text_segments" lst with
          | Some (`Assoc fields) ->
              check int "present 2" 2
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1);
              check int "total 3" 3
                (match List.assoc_opt "total" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected text_segments details")
     | _ -> fail "expected assoc details")
  in

  (* include_plugin with ok=false -> plugin section override (0/1) *)
  let test_bundle_plugin_not_ok () =
    let plugin_snapshot = `Assoc [("ok", `Bool false)] in
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:(`Assoc []) ~variables:`Null ~image_fills:`Null ~plugin_snapshot
      ~include_variables:false ~include_image_fills:false ~include_plugin:true
    in
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "plugin" lst with
          | Some (`Assoc fields) ->
              check int "present 0" 0
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1);
              check int "total 1" 1
                (match List.assoc_opt "total" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected plugin details")
     | _ -> fail "expected assoc details")
  in

  (* include_plugin with ok=true but 0 text nodes -> no text_segments override *)
  let test_bundle_plugin_ok_no_text () =
    let dsl_json = `Assoc [
      ("meta", `Assoc [("type", `String "FRAME")]);
    ] in
    let plugin_snapshot = `Assoc [("ok", `Bool true); ("payload", `Assoc [])] in
    let (_score, _, _details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills:`Null ~plugin_snapshot
      ~include_variables:false ~include_image_fills:false ~include_plugin:true
    in
    ()  (* no text nodes -> text_segments not overridden *)
  in

  (* include_plugin: missing ok field -> plugin_ok = false *)
  let test_bundle_plugin_missing_ok () =
    let plugin_snapshot = `Assoc [("payload", `Assoc [])] in
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:(`Assoc []) ~variables:`Null ~image_fills:`Null ~plugin_snapshot
      ~include_variables:false ~include_image_fills:false ~include_plugin:true
    in
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "plugin" lst with
          | Some (`Assoc fields) ->
              check int "present 0" 0
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected plugin details")
     | _ -> fail "expected assoc details")
  in

  (* include_plugin: ok is non-bool -> plugin_ok = false *)
  let test_bundle_plugin_ok_non_bool () =
    let plugin_snapshot = `Assoc [("ok", `String "yes")] in
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:(`Assoc []) ~variables:`Null ~image_fills:`Null ~plugin_snapshot
      ~include_variables:false ~include_image_fills:false ~include_plugin:true
    in
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "plugin" lst with
          | Some (`Assoc fields) ->
              check int "present 0" 0
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected plugin details")
     | _ -> fail "expected assoc details")
  in

  (* include_plugin: non-assoc plugin_snapshot -> plugin_ok false *)
  let test_bundle_plugin_non_assoc () =
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:(`Assoc []) ~variables:`Null ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:false ~include_image_fills:false ~include_plugin:true
    in
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "plugin" lst with
          | Some (`Assoc fields) ->
              check int "present 0" 0
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected plugin details")
     | _ -> fail "expected assoc details")
  in

  (* include_plugin: nested text nodes in dsl *)
  let test_bundle_plugin_nested_text () =
    let text_node = `Assoc [("meta", `Assoc [("type", `String "TEXT")])] in
    let frame = `Assoc [
      ("meta", `Assoc [("type", `String "FRAME")]);
      ("children", `List [text_node; text_node]);
    ] in
    let dsl_json = `Assoc [("children", `List [frame; text_node])] in
    (* 3 total text nodes: 2 nested + 1 direct *)
    let seg_node = `Assoc [("text", `Assoc [("segments", `List [`String "s"])])] in
    let plugin_snapshot = `Assoc [
      ("ok", `Bool true);
      ("payload", `Assoc [("children", `List [seg_node])]);
    ] in
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills:`Null ~plugin_snapshot
      ~include_variables:false ~include_image_fills:false ~include_plugin:true
    in
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "text_segments" lst with
          | Some (`Assoc fields) ->
              check int "total 3" 3
                (match List.assoc_opt "total" fields with Some (`Int n) -> n | _ -> -1);
              check int "present 1" 1
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected text_segments details")
     | _ -> fail "expected assoc details")
  in

  (* include_plugin: plugin_text_nodes_with_segments with no payload *)
  let test_bundle_plugin_no_payload () =
    let text_node = `Assoc [("meta", `Assoc [("type", `String "TEXT")])] in
    let dsl_json = `Assoc [("children", `List [text_node])] in
    let plugin_snapshot = `Assoc [("ok", `Bool true)] in  (* no payload key *)
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables:`Null ~image_fills:`Null ~plugin_snapshot
      ~include_variables:false ~include_image_fills:false ~include_plugin:true
    in
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "text_segments" lst with
          | Some (`Assoc fields) ->
              check int "present 0" 0
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected text_segments details")
     | _ -> fail "expected assoc details")
  in

  (* include_variables with resolved/raw data *)
  let test_bundle_variables_ok () =
    let variables = `Assoc [
      ("variables", `Assoc [("v1", `String "a"); ("v2", `String "b")]);
      ("resolved", `Assoc [("v1", `String "resolved_a")]);
    ] in
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:(`Assoc []) ~variables ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:true ~include_image_fills:false ~include_plugin:false
    in
    (* variables_counts returns (2, 1) -> total=2, present=1 *)
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "variables_resolved" lst with
          | Some (`Assoc fields) ->
              check int "total 2" 2
                (match List.assoc_opt "total" fields with Some (`Int n) -> n | _ -> -1);
              check int "present 1" 1
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected variables_resolved details")
     | _ -> fail "expected assoc details")
  in

  (* include_variables with error *)
  let test_bundle_variables_error () =
    let variables = `Assoc [("error", `String "no access")] in
    let (_, _, details) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:(`Assoc []) ~variables ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:true ~include_image_fills:false ~include_plugin:false
    in
    (* variables_counts for error returns (1, 0) -> total=1, present=0 *)
    (match details with
     | `Assoc lst ->
         (match List.assoc_opt "variables_resolved" lst with
          | Some (`Assoc fields) ->
              check int "total 1" 1
                (match List.assoc_opt "total" fields with Some (`Int n) -> n | _ -> -1);
              check int "present 0" 0
                (match List.assoc_opt "present" fields with Some (`Int n) -> n | _ -> -1)
          | _ -> fail "expected variables_resolved details")
     | _ -> fail "expected assoc details")
  in

  (* include_variables with non-assoc -> (0, 0) -> no override *)
  let test_bundle_variables_non_assoc () =
    let (_, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:(`Assoc []) ~variables:`Null ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:true ~include_image_fills:false ~include_plugin:false
    in
    ()  (* variables_counts returns (0, 0), total=0 -> no override *)
  in

  (* include_variables with empty assoc -> (0, 0) -> no override *)
  let test_bundle_variables_empty () =
    let variables = `Assoc [] in
    let (_, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json:(`Assoc []) ~variables ~image_fills:`Null ~plugin_snapshot:`Null
      ~include_variables:true ~include_image_fills:false ~include_plugin:false
    in
    ()
  in

  (* All flags enabled *)
  let test_bundle_all_flags () =
    let text_node = `Assoc [("meta", `Assoc [("type", `String "TEXT")])] in
    let dsl_json = `Assoc [
      ("children", `List [text_node]);
      ("assets", `Assoc [("image_refs", `List [`String "r1"])]);
    ] in
    let image_fills = `Assoc [("images", `Assoc [("r1", `String "url")])] in
    let variables = `Assoc [
      ("variables", `Assoc [("v1", `String "x")]);
      ("resolved", `Assoc [("v1", `String "y")]);
    ] in
    let plugin_snapshot = `Assoc [
      ("ok", `Bool true);
      ("payload", `Assoc [
        ("text", `Assoc [("segments", `List [`String "s"])]);
      ]);
    ] in
    let (score, _, _) = Mcp_helpers.fidelity_score_of_bundle
      ~dsl_json ~variables ~image_fills ~plugin_snapshot
      ~include_variables:true ~include_image_fills:true ~include_plugin:true
    in
    check bool "score > 0" true (score > 0.0)
  in

  (* --- count_text_segments (directly exported) --- *)
  let test_count_text_segments_one () =
    let json = `Assoc [
      ("text", `Assoc [("segments", `List [`String "s1"; `String "s2"])]);
    ] in
    check int "2 segments" 2 (Mcp_helpers.count_text_segments json)
  in
  let test_count_text_segments_no_segments () =
    let json = `Assoc [("text", `Assoc [("value", `String "hello")])] in
    check int "0 segments" 0 (Mcp_helpers.count_text_segments json)
  in
  let test_count_text_segments_no_text () =
    let json = `Assoc [("meta", `Assoc [])] in
    check int "0" 0 (Mcp_helpers.count_text_segments json)
  in
  let test_count_text_segments_nested () =
    let child = `Assoc [("text", `Assoc [("segments", `List [`String "s"])])] in
    let parent = `Assoc [("children", `List [child; child])] in
    check int "2 nested" 2 (Mcp_helpers.count_text_segments parent)
  in
  let test_count_text_segments_list () =
    let node = `Assoc [("text", `Assoc [("segments", `List [`String "s"])])] in
    check int "3 from list" 3 (Mcp_helpers.count_text_segments (`List [node; node; node]))
  in
  let test_count_text_segments_primitive () =
    check int "null" 0 (Mcp_helpers.count_text_segments `Null);
    check int "int" 0 (Mcp_helpers.count_text_segments (`Int 1))
  in

  (* --- plugin_payload_if_ok (directly exported) --- *)
  let test_plugin_payload_if_ok_true () =
    let json = `Assoc [("ok", `Bool true); ("payload", `Assoc [("data", `Int 1)])] in
    match Mcp_helpers.plugin_payload_if_ok json with
    | Some (`Assoc _) -> ()
    | _ -> fail "expected Some payload"
  in
  let test_plugin_payload_if_ok_false () =
    let json = `Assoc [("ok", `Bool false); ("payload", `Assoc [])] in
    check bool "false -> None" true (Mcp_helpers.plugin_payload_if_ok json = None)
  in
  let test_plugin_payload_if_ok_no_ok () =
    let json = `Assoc [("payload", `Assoc [])] in
    check bool "no ok -> None" true (Mcp_helpers.plugin_payload_if_ok json = None)
  in
  let test_plugin_payload_if_ok_no_payload () =
    let json = `Assoc [("ok", `Bool true)] in
    check bool "no payload -> None" true (Mcp_helpers.plugin_payload_if_ok json = None)
  in
  let test_plugin_payload_if_ok_non_assoc () =
    check bool "null -> None" true (Mcp_helpers.plugin_payload_if_ok `Null = None);
    check bool "list -> None" true (Mcp_helpers.plugin_payload_if_ok (`List []) = None)
  in

  (* --- resolve_plugin_variables (directly exported) --- *)
  let test_resolve_plugin_variables_ok () =
    let payload = `Assoc [
      ("collections", `Assoc []);
      ("variables", `Assoc [
        ("v1", `Assoc [
          ("name", `String "test");
          ("valuesByMode", `Assoc [("mode1", `String "val1")]);
        ])
      ]);
    ] in
    let result = Mcp_helpers.resolve_plugin_variables payload in
    match result with
    | `Assoc _ -> ()  (* valid structure *)
    | _ -> fail "expected assoc"
  in
  let test_resolve_plugin_variables_no_collections () =
    let payload = `Assoc [("variables", `Assoc [])] in
    let result = Mcp_helpers.resolve_plugin_variables payload in
    match result with
    | `Assoc lst ->
        check bool "error" true (List.assoc_opt "error" lst <> None)
    | _ -> fail "expected assoc"
  in
  let test_resolve_plugin_variables_non_assoc () =
    let result = Mcp_helpers.resolve_plugin_variables `Null in
    match result with
    | `Assoc lst ->
        check bool "error" true (List.assoc_opt "error" lst <> None)
    | _ -> fail "expected assoc"
  in

  (* --- classify_http_error: edge cases --- *)
  let test_classify_429_with_float () =
    match Mcp_helpers.classify_http_error ~status_code:429 ~body:"retry after 45.5" with
    | Mcp_helpers.RateLimited secs -> check (float 0.1) "parsed" 45.5 secs
    | _ -> fail "expected RateLimited"
  in
  let test_classify_400 () =
    match Mcp_helpers.classify_http_error ~status_code:400 ~body:"bad request" with
    | Mcp_helpers.UnknownError _ -> ()
    | _ -> fail "expected UnknownError"
  in
  let test_classify_301 () =
    match Mcp_helpers.classify_http_error ~status_code:301 ~body:"moved" with
    | Mcp_helpers.UnknownError _ -> ()
    | _ -> fail "expected UnknownError"
  in

  (* --- resolve_variables edge: empty valuesByMode --- *)
  let test_resolve_variables_empty_values_by_mode () =
    let json = `Assoc [
      ("meta", `Assoc [
        ("variableCollections", `Assoc []);
        ("variables", `Assoc [
          ("var1", `Assoc [
            ("name", `String "test");
            ("valuesByMode", `Assoc []);
          ])
        ])
      ])
    ] in
    let result = Mcp_helpers.resolve_variables json in
    match result with
    | `Assoc lst ->
        (match List.assoc_opt "resolved" lst with
         | Some (`Assoc [("var1", `Assoc fields)]) ->
             check bool "null default" true
               (List.assoc_opt "defaultValue" fields = Some `Null)
         | _ -> fail "expected resolved")
    | _ -> fail "expected assoc"
  in

  (* --- resolve_variables: non-assoc variables --- *)
  let test_resolve_variables_non_assoc_variables () =
    let json = `Assoc [
      ("meta", `Assoc [
        ("variableCollections", `Assoc []);
        ("variables", `String "bad");
      ])
    ] in
    let result = Mcp_helpers.resolve_variables json in
    match result with
    | `Assoc lst ->
        check bool "null resolved" true (List.assoc_opt "resolved" lst = Some `Null)
    | _ -> fail "expected assoc"
  in

  (* --- process_json_string with "pixel" and "accuracy" formats --- *)
  let test_process_json_pixel () =
    let json_str = Yojson.Safe.to_string (`Assoc [("type", `String "FRAME")]) in
    match Mcp_helpers.process_json_string ~format:"pixel" json_str with
    | Ok s -> check bool "pixel output" true (String.length s > 0)
    | Error e -> fail (Printf.sprintf "pixel error: %s" e)
  in
  let test_process_json_accuracy () =
    let json_str = Yojson.Safe.to_string (`Assoc [("type", `String "FRAME")]) in
    match Mcp_helpers.process_json_string ~format:"accuracy" json_str with
    | Ok s -> check bool "accuracy output" true (String.length s > 0)
    | Error e -> fail (Printf.sprintf "accuracy error: %s" e)
  in

  (* --- make_error_json with empty debug_info (no debug field) --- *)
  let test_make_error_json_empty_debug () =
    let result = Mcp_helpers.make_error_json ~operation:"op"
      ~error:(Mcp_helpers.NetworkError "net") ~debug_info:[] () in
    match result with
    | `Assoc lst ->
        check bool "no debug" true (List.assoc_opt "debug" lst = None)
    | _ -> fail "expected assoc"
  in

  (* --- eio_context_key (Domain.DLS) --- *)
  let test_get_eio_context_none () =
    let ctx = Mcp_helpers.get_eio_context () in
    ignore ctx  (* may or may not be None depending on other tests *)
  in

  (* --- member (directly exported) --- *)
  let test_member_found () =
    let json = `Assoc [("key", `String "val")] in
    match Mcp_helpers.member "key" json with
    | Some (`String "val") -> ()
    | _ -> fail "expected Some string"
  in
  let test_member_missing () =
    let json = `Assoc [("other", `Int 1)] in
    check bool "missing" true (Mcp_helpers.member "key" json = None)
  in
  let test_member_non_assoc () =
    check bool "null" true (Mcp_helpers.member "key" `Null = None)
  in

  (* ===== figma_effects.ml — additional branches ===== *)

  (* --- create_mock_store: verify all hashtable fields --- *)
  let test_effects_create_mock_store_fresh () =
    let s = Figma_effects.create_mock_store () in
    check int "files" 0 (Hashtbl.length s.files);
    check int "nodes" 0 (Hashtbl.length s.nodes);
    check int "images" 0 (Hashtbl.length s.images);
    check int "file_images" 0 (Hashtbl.length s.file_images);
    check int "file_meta" 0 (Hashtbl.length s.file_meta);
    check bool "me none" true (!(s.me) = None);
    check int "projects" 0 (Hashtbl.length s.projects);
    check int "project_files" 0 (Hashtbl.length s.project_files);
    check int "variables" 0 (Hashtbl.length s.variables)
  in

  (* --- run_with_mock: exercise exception path --- *)
  let test_run_with_mock_exception () =
    let store = Figma_effects.create_mock_store () in
    let raised = ref false in
    (try
      Figma_effects.run_with_mock store (fun () ->
        failwith "test exception"
      )
    with Failure msg ->
      raised := true;
      check string "msg" "test exception" msg);
    check bool "raised" true !raised
  in

  (* --- run_with_mock: pure computation (no effects) --- *)
  let test_run_with_mock_pure () =
    let store = Figma_effects.create_mock_store () in
    let result = Figma_effects.run_with_mock store (fun () -> 42) in
    check int "pure value" 42 result
  in

  (* --- run_with_mock with populated store: verify me ref works --- *)
  let test_run_with_mock_me_set_unset () =
    let store = Figma_effects.create_mock_store () in
    (* me not set *)
    let r1 = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_me ~token:"tk"
    ) in
    check bool "me not set" true (Result.is_error r1);
    (* set me *)
    store.me := Some (`Assoc [("id", `String "u1")]);
    let r2 = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.get_me ~token:"tk"
    ) in
    check bool "me set" true (Result.is_ok r2)
  in

  (* --- make_neo4j_statement with complex params --- *)
  let test_neo4j_statement_complex_params () =
    let stmt = Figma_effects.make_neo4j_statement
      "MERGE (n:Node {id: $id}) SET n.tags = $tags"
      [("id", `String "n1"); ("tags", `List [`String "a"; `String "b"])]
    in
    match stmt with
    | `Assoc fields ->
        (match List.assoc_opt "parameters" fields with
         | Some (`Assoc params) ->
             check bool "has id" true (List.assoc_opt "id" params = Some (`String "n1"));
             (match List.assoc_opt "tags" params with
              | Some (`List _) -> ()
              | _ -> fail "expected tags list")
         | _ -> fail "expected params")
    | _ -> fail "expected assoc"
  in

  (* --- parse_neo4j_response edge: errors with partial code/message --- *)
  let test_parse_neo4j_partial_error () =
    let body = {|{"results":[],"errors":[{"code":"Neo.Err","message":""}]}|} in
    match Figma_effects.parse_neo4j_response body with
    | Error msg -> check bool "has code" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in
  let test_parse_neo4j_only_message () =
    let body = {|{"results":[],"errors":[{"code":"","message":"something wrong"}]}|} in
    match Figma_effects.parse_neo4j_response body with
    | Error msg -> check bool "has message" true (String.length msg > 0)
    | Ok _ -> fail "expected error"
  in

  (* --- parse_neo4j_response: no results field --- *)
  let test_parse_neo4j_no_results () =
    let body = {|{"errors":[]}|} in
    match Figma_effects.parse_neo4j_response body with
    | Ok _ -> ()
    | Error e -> fail (Printf.sprintf "unexpected error: %s" e)
  in

  (* --- run_with_mock: multiple sequential effects in one run --- *)
  let test_run_with_mock_multi_effects () =
    let store = Figma_effects.create_mock_store () in
    Hashtbl.replace store.files "f1" (`Assoc [("name", `String "File1")]);
    Hashtbl.replace store.variables "f1" (`Assoc [("vars", `Assoc [])]);
    store.me := Some (`Assoc [("id", `String "me")]);
    let result = Figma_effects.run_with_mock store (fun () ->
      Figma_effects.Perform.log_info "start";
      let f = Figma_effects.Perform.get_file ~token:"t" ~file_key:"f1" () in
      Figma_effects.Perform.log_debug "got file";
      let v = Figma_effects.Perform.get_variables ~token:"t" ~file_key:"f1" in
      Figma_effects.Perform.log_error "test error log";
      Figma_effects.Perform.eio_sleep 0.1;
      let m = Figma_effects.Perform.get_me ~token:"t" in
      (f, v, m)
    ) in
    let (f, v, m) = result in
    check bool "file ok" true (Result.is_ok f);
    check bool "vars ok" true (Result.is_ok v);
    check bool "me ok" true (Result.is_ok m)
  in

  (* ===== server_metrics.ml — target 60%+ ===== *)

  (* --- prom_metric (pure function) --- *)
  let test_prom_metric_basic () =
    let result = Server_metrics.prom_metric "test_metric" "label=\"val\"" "42" in
    check string "format" "test_metric{label=\"val\"} 42\n" result
  in
  let test_prom_metric_empty_labels () =
    let result = Server_metrics.prom_metric "m" "" "0" in
    check string "empty labels" "m{} 0\n" result
  in
  let test_prom_metric_float_value () =
    let result = Server_metrics.prom_metric "latency" "q=\"p50\"" "1.234" in
    check string "float" "latency{q=\"p50\"} 1.234\n" result
  in

  (* --- snapshot, to_json, to_prometheus_text via Eio_main.run --- *)
  let test_snapshot_initial () =
    Eio_main.run @@ fun _env ->
    let s = Server_metrics.snapshot () in
    check bool "inflight >= 0" true (s.inflight >= 0);
    check bool "total >= 0" true (s.total >= 0);
    check bool "updated_at > 0" true (s.updated_at > 0.0)
  in

  let test_to_json () =
    Eio_main.run @@ fun _env ->
    let json = Server_metrics.to_json () in
    match json with
    | `Assoc lst ->
        check bool "has inflight" true (List.assoc_opt "inflight" lst <> None);
        check bool "has total" true (List.assoc_opt "total" lst <> None);
        check bool "has status_2xx" true (List.assoc_opt "status_2xx" lst <> None);
        check bool "has status_3xx" true (List.assoc_opt "status_3xx" lst <> None);
        check bool "has status_4xx" true (List.assoc_opt "status_4xx" lst <> None);
        check bool "has status_5xx" true (List.assoc_opt "status_5xx" lst <> None);
        check bool "has errors" true (List.assoc_opt "errors" lst <> None);
        check bool "has bytes_out" true (List.assoc_opt "bytes_out" lst <> None);
        check bool "has sse_open" true (List.assoc_opt "sse_open" lst <> None);
        check bool "has sse_total" true (List.assoc_opt "sse_total" lst <> None);
        check bool "has rps_1m" true (List.assoc_opt "rps_1m" lst <> None);
        check bool "has rps_5m" true (List.assoc_opt "rps_5m" lst <> None);
        check bool "has latency_ms" true (List.assoc_opt "latency_ms" lst <> None);
        check bool "has updated_at" true (List.assoc_opt "updated_at" lst <> None);
        (match List.assoc_opt "latency_ms" lst with
         | Some (`Assoc lat) ->
             check bool "lat count" true (List.assoc_opt "count" lat <> None);
             check bool "lat avg" true (List.assoc_opt "avg" lat <> None);
             check bool "lat p50" true (List.assoc_opt "p50" lat <> None);
             check bool "lat p95" true (List.assoc_opt "p95" lat <> None);
             check bool "lat p99" true (List.assoc_opt "p99" lat <> None);
             check bool "lat min" true (List.assoc_opt "min" lat <> None);
             check bool "lat max" true (List.assoc_opt "max" lat <> None)
         | _ -> fail "expected latency_ms assoc")
    | _ -> fail "expected assoc"
  in

  let test_to_prometheus_text () =
    Eio_main.run @@ fun _env ->
    let text = Server_metrics.to_prometheus_text () in
    check bool "has header" true (String.length text > 0);
    let has_str s = try let _ = Str.search_forward (Str.regexp_string s) text 0 in true with Not_found -> false in
    check bool "has HELP total" true (has_str "# HELP mcp_http_requests_total");
    check bool "has TYPE total" true (has_str "# TYPE mcp_http_requests_total counter");
    check bool "has inflight metric" true (has_str "mcp_http_inflight");
    check bool "has errors metric" true (has_str "mcp_http_errors_total");
    check bool "has rps_1m metric" true (has_str "mcp_http_rps_1m");
    check bool "has rps_5m metric" true (has_str "mcp_http_rps_5m");
    check bool "has sse_open metric" true (has_str "mcp_sse_open");
    check bool "has sse_total metric" true (has_str "mcp_sse_total");
    check bool "has latency metric" true (has_str "mcp_http_latency_ms");
    check bool "has 2xx class" true (has_str "class=\"2xx\"");
    check bool "has 3xx class" true (has_str "class=\"3xx\"");
    check bool "has 4xx class" true (has_str "class=\"4xx\"");
    check bool "has 5xx class" true (has_str "class=\"5xx\"");
    check bool "has quantile p50" true (has_str "quantile=\"0.50\"");
    check bool "has quantile p95" true (has_str "quantile=\"0.95\"");
    check bool "has quantile p99" true (has_str "quantile=\"0.99\"");
    check bool "has stat avg" true (has_str "stat=\"avg\"");
    check bool "has stat min" true (has_str "stat=\"min\"");
    check bool "has stat max" true (has_str "stat=\"max\"")
  in

  (* --- sse_open / sse_close --- *)
  let test_sse_open_close () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.sse_open ();
    let after_open = Server_metrics.snapshot () in
    check int "sse_open +1" (before.sse_open + 1) after_open.sse_open;
    check int "sse_total +1" (before.sse_total + 1) after_open.sse_total;
    Server_metrics.sse_close ();
    let after_close = Server_metrics.snapshot () in
    check int "sse_open back" before.sse_open after_close.sse_open;
    check int "sse_total unchanged" after_open.sse_total after_close.sse_total
  in

  (* --- sse_close when already 0 --- *)
  let test_sse_close_floor () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    for _ = 1 to before.sse_open + 1 do
      Server_metrics.sse_close ()
    done;
    let after = Server_metrics.snapshot () in
    check bool "sse_open >= 0" true (after.sse_open >= 0)
  in

  (* --- record_untracked_response: 2xx --- *)
  let test_record_untracked_2xx () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `OK;
    let after = Server_metrics.snapshot () in
    check int "total +1" (before.total + 1) after.total;
    check int "2xx +1" (before.status_2xx + 1) after.status_2xx;
    check int "errors same" before.errors after.errors
  in

  (* --- record_untracked_response: 3xx --- *)
  let test_record_untracked_3xx () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `Moved_permanently;
    let after = Server_metrics.snapshot () in
    check int "total +1" (before.total + 1) after.total;
    check int "3xx +1" (before.status_3xx + 1) after.status_3xx;
    check int "errors same" before.errors after.errors
  in

  (* --- record_untracked_response: no bytes --- *)
  let test_record_untracked_no_bytes () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `OK;
    let after = Server_metrics.snapshot () in
    check int "bytes unchanged" before.bytes_out after.bytes_out
  in

  (* --- record_untracked_response: with bytes on 2xx --- *)
  let test_record_untracked_with_bytes () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response ~bytes:500 `OK;
    let after = Server_metrics.snapshot () in
    check int "bytes +500" (before.bytes_out + 500) after.bytes_out
  in

  (* --- latency snapshot fields --- *)
  let test_latency_empty_snapshot () =
    Eio_main.run @@ fun _env ->
    let s = Server_metrics.snapshot () in
    check bool "count >= 0" true (s.latency.count >= 0);
    check bool "avg >= 0" true (s.latency.avg_ms >= 0.0);
    check bool "p50 >= 0" true (s.latency.p50_ms >= 0.0);
    check bool "p95 >= 0" true (s.latency.p95_ms >= 0.0);
    check bool "p99 >= 0" true (s.latency.p99_ms >= 0.0);
    check bool "min >= 0" true (s.latency.min_ms >= 0.0);
    check bool "max >= 0" true (s.latency.max_ms >= 0.0)
  in

  (* --- multiple record_untracked in sequence --- *)
  let test_record_untracked_sequence () =
    Eio_main.run @@ fun _env ->
    let before = Server_metrics.snapshot () in
    Server_metrics.record_untracked_response `OK;
    Server_metrics.record_untracked_response `Not_found;
    Server_metrics.record_untracked_response `Internal_server_error;
    Server_metrics.record_untracked_response ~bytes:100 `OK;
    let after = Server_metrics.snapshot () in
    check int "total +4" (before.total + 4) after.total;
    check int "2xx +2" (before.status_2xx + 2) after.status_2xx;
    check int "4xx +1" (before.status_4xx + 1) after.status_4xx;
    check int "5xx +1" (before.status_5xx + 1) after.status_5xx;
    check int "errors +2" (before.errors + 2) after.errors
  in

  run "helpers-effects-w4" [
    (* mcp_helpers.ml — fidelity score functions *)
    ("fidelity_score_of_dsl", [
      test_case "full" `Quick test_fidelity_dsl_full;
      test_case "empty" `Quick test_fidelity_dsl_empty;
      test_case "partial" `Quick test_fidelity_dsl_partial;
      test_case "all missing" `Quick test_fidelity_dsl_all_missing;
    ]);
    ("fidelity_with_overrides", [
      test_case "override section" `Quick test_fidelity_with_overrides;
    ]);
    ("override_section", [
      test_case "with score" `Quick test_override_section_with_score;
      test_case "auto score" `Quick test_override_section_auto_score;
      test_case "zero total" `Quick test_override_section_zero_total;
    ]);

    (* mcp_helpers.ml — fidelity_score_of_bundle + image fills *)
    ("bundle image_fills", [
      test_case "match" `Quick test_bundle_image_fills_match;
      test_case "no refs" `Quick test_bundle_image_fills_no_refs;
      test_case "non-assoc" `Quick test_bundle_image_fills_non_assoc;
      test_case "mixed refs" `Quick test_bundle_image_refs_mixed;
      test_case "bad assets" `Quick test_bundle_image_fills_bad_assets;
      test_case "images not assoc" `Quick test_bundle_image_fills_images_not_assoc;
    ]);

    (* mcp_helpers.ml — fidelity_score_of_bundle + plugin *)
    ("bundle plugin", [
      test_case "ok" `Quick test_bundle_plugin_ok;
      test_case "not ok" `Quick test_bundle_plugin_not_ok;
      test_case "ok no text" `Quick test_bundle_plugin_ok_no_text;
      test_case "missing ok" `Quick test_bundle_plugin_missing_ok;
      test_case "ok non-bool" `Quick test_bundle_plugin_ok_non_bool;
      test_case "non-assoc" `Quick test_bundle_plugin_non_assoc;
      test_case "nested text" `Quick test_bundle_plugin_nested_text;
      test_case "no payload" `Quick test_bundle_plugin_no_payload;
    ]);

    (* mcp_helpers.ml — fidelity_score_of_bundle + variables *)
    ("bundle variables", [
      test_case "ok" `Quick test_bundle_variables_ok;
      test_case "error" `Quick test_bundle_variables_error;
      test_case "non-assoc" `Quick test_bundle_variables_non_assoc;
      test_case "empty" `Quick test_bundle_variables_empty;
    ]);

    (* mcp_helpers.ml — all flags *)
    ("bundle all flags", [
      test_case "all enabled" `Quick test_bundle_all_flags;
    ]);

    (* mcp_helpers.ml — count_text_segments (exported) *)
    ("count_text_segments", [
      test_case "one" `Quick test_count_text_segments_one;
      test_case "no segments" `Quick test_count_text_segments_no_segments;
      test_case "no text" `Quick test_count_text_segments_no_text;
      test_case "nested" `Quick test_count_text_segments_nested;
      test_case "list" `Quick test_count_text_segments_list;
      test_case "primitive" `Quick test_count_text_segments_primitive;
    ]);

    (* mcp_helpers.ml — plugin_payload_if_ok (exported) *)
    ("plugin_payload_if_ok", [
      test_case "true" `Quick test_plugin_payload_if_ok_true;
      test_case "false" `Quick test_plugin_payload_if_ok_false;
      test_case "no ok" `Quick test_plugin_payload_if_ok_no_ok;
      test_case "no payload" `Quick test_plugin_payload_if_ok_no_payload;
      test_case "non-assoc" `Quick test_plugin_payload_if_ok_non_assoc;
    ]);

    (* mcp_helpers.ml — resolve_plugin_variables (exported) *)
    ("resolve_plugin_variables", [
      test_case "ok" `Quick test_resolve_plugin_variables_ok;
      test_case "no collections" `Quick test_resolve_plugin_variables_no_collections;
      test_case "non-assoc" `Quick test_resolve_plugin_variables_non_assoc;
    ]);

    (* mcp_helpers.ml — classify_http_error edges *)
    ("classify_http_error w4", [
      test_case "429 float" `Quick test_classify_429_with_float;
      test_case "400" `Quick test_classify_400;
      test_case "301" `Quick test_classify_301;
    ]);

    (* mcp_helpers.ml — resolve_variables edges *)
    ("resolve_variables w4", [
      test_case "empty valuesByMode" `Quick test_resolve_variables_empty_values_by_mode;
      test_case "non-assoc variables" `Quick test_resolve_variables_non_assoc_variables;
    ]);

    (* mcp_helpers.ml — process_json_string *)
    ("process_json_string w4", [
      test_case "pixel format" `Quick test_process_json_pixel;
      test_case "accuracy format" `Quick test_process_json_accuracy;
    ]);

    (* mcp_helpers.ml — make_error_json *)
    ("make_error_json w4", [
      test_case "empty debug_info" `Quick test_make_error_json_empty_debug;
    ]);

    (* mcp_helpers.ml — other exported *)
    ("eio_context", [
      test_case "get none" `Quick test_get_eio_context_none;
    ]);
    ("member w4", [
      test_case "found" `Quick test_member_found;
      test_case "missing" `Quick test_member_missing;
      test_case "non-assoc" `Quick test_member_non_assoc;
    ]);

    (* figma_effects.ml *)
    ("effects create_mock_store", [
      test_case "fresh" `Quick test_effects_create_mock_store_fresh;
    ]);
    ("effects run_with_mock", [
      test_case "exception" `Quick test_run_with_mock_exception;
      test_case "pure" `Quick test_run_with_mock_pure;
      test_case "me set/unset" `Quick test_run_with_mock_me_set_unset;
      test_case "multi effects" `Quick test_run_with_mock_multi_effects;
    ]);
    ("neo4j statement w4", [
      test_case "complex params" `Quick test_neo4j_statement_complex_params;
    ]);
    ("parse_neo4j w4", [
      test_case "partial error" `Quick test_parse_neo4j_partial_error;
      test_case "only message" `Quick test_parse_neo4j_only_message;
      test_case "no results" `Quick test_parse_neo4j_no_results;
    ]);

    (* server_metrics.ml *)
    ("prom_metric", [
      test_case "basic" `Quick test_prom_metric_basic;
      test_case "empty labels" `Quick test_prom_metric_empty_labels;
      test_case "float value" `Quick test_prom_metric_float_value;
    ]);
    ("snapshot", [
      test_case "initial" `Quick test_snapshot_initial;
    ]);
    ("to_json", [
      test_case "structure" `Quick test_to_json;
    ]);
    ("to_prometheus_text", [
      test_case "format" `Quick test_to_prometheus_text;
    ]);
    ("sse", [
      test_case "open/close" `Quick test_sse_open_close;
      test_case "close floor" `Quick test_sse_close_floor;
    ]);
    ("record_untracked w4", [
      test_case "2xx" `Quick test_record_untracked_2xx;
      test_case "3xx" `Quick test_record_untracked_3xx;
      test_case "no bytes" `Quick test_record_untracked_no_bytes;
      test_case "with bytes" `Quick test_record_untracked_with_bytes;
      test_case "sequence" `Quick test_record_untracked_sequence;
    ]);
    ("latency", [
      test_case "empty snapshot" `Quick test_latency_empty_snapshot;
    ]);
  ]
