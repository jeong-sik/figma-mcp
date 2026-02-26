(** Coverage tests for mcp_plugin_handlers.ml — dispatch, combinators, and
    handler error paths.  Targets the 400+ uncovered points beyond the pure
    helpers already tested in test_plugin_handlers_coverage.ml.

    Strategy: exercise every handler's arg-parsing and early-error branches.
    We avoid blocking by never reaching plugin_wait with a live channel.
    - Missing required args → immediate Error (before plugin_exec)
    - resolve_channel_id Error → no explicit channel_id AND no default
    - connect/status/use_channel don't call plugin_wait at all
    - edit_node with registered channel + unknown property → no dispatch call

    IMPORTANT: Tests that would reach plugin_wait with an auto-created session
    are excluded because wait_for_result blocks indefinitely without Eio. *)

(* Helper: check if string contains a substring *)
let contains haystack needle =
  try let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in true
  with Not_found -> false

let () =
  let open Alcotest in

  (* ============================================================
     GROUP 1: Tests that run BEFORE any default channel is set.
     resolve_channel_id with no explicit + no default → Error.
     This exercises the "Missing channel_id" error path in every
     handler that calls resolve_channel_id.
     ============================================================ *)

  (* --- known_plugin_actions --- *)

  let test_known_actions_nonempty () =
    let actions = Mcp_plugin_handlers.known_plugin_actions in
    check bool "non-empty" true (List.length actions > 0)
  in
  let test_known_actions_has_connect () =
    check bool "contains connect" true
      (List.mem "connect" Mcp_plugin_handlers.known_plugin_actions)
  in
  let test_known_actions_has_annotate () =
    check bool "contains annotate" true
      (List.mem "annotate" Mcp_plugin_handlers.known_plugin_actions)
  in
  let test_known_actions_count () =
    check bool "many actions" true
      (List.length Mcp_plugin_handlers.known_plugin_actions > 50)
  in

  (* --- suggest_action --- *)

  let test_suggest_nonempty () =
    let suggestion = Mcp_plugin_handlers.suggest_action "connekt" in
    check bool "non-empty suggestion" true (String.length suggestion > 0)
  in
  let test_suggest_short_input () =
    let suggestion = Mcp_plugin_handlers.suggest_action "x" in
    check bool "suggestion returned" true (String.contains suggestion '\'')
  in
  let test_suggest_empty_input () =
    let suggestion = Mcp_plugin_handlers.suggest_action "" in
    check bool "has suggestion" true (String.length suggestion > 0)
  in

  (* --- resolve_channel_id with explicit --- *)

  let test_resolve_channel_explicit () =
    let args = `Assoc [("channel_id", `String "test-ch-1")] in
    match Mcp_plugin_handlers.resolve_channel_id args with
    | Ok id -> check string "explicit id" "test-ch-1" id
    | Error msg -> fail msg
  in

  (* --- handle_figma_plugin: missing action --- *)

  let test_dispatch_missing_action () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Error msg -> check string "missing action" "Missing action" msg
    | Ok _ -> fail "should error on missing action"
  in

  (* --- handle_figma_plugin: unknown action -> strict Error with suggestion --- *)

  let test_dispatch_unknown_action () =
    let args = `Assoc [("action", `String "export_imag")] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Error msg ->
        let lower = String.lowercase_ascii msg in
        check bool "unknown action must error" true (contains lower "unknown plugin action");
        check bool "error suggests alternatives" true (contains lower "did you mean");
        check bool "suggested action" true (contains lower "export_image")
    | Ok _ -> fail "should error on unknown action"
  in

  (* --- Handler error paths: no channel_id, no default → Error --- *)
  (* These all exercise resolve_channel_id → Error before plugin_exec. *)

  let test_read_selection_no_channel () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_read_selection args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_get_node_missing_node_id () =
    (* No node_id and no channel → errors on node_id first *)
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_get_node args with
    | Error msg -> check bool "missing node_id" true (contains msg "node_id")
    | Ok _ -> fail "should error"
  in

  let test_get_node_no_channel () =
    (* Has node_id but no channel_id, no default *)
    let args = `Assoc [("node_id", `String "123:456")] in
    match Mcp_plugin_handlers.handle_plugin_get_node args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_export_missing_node () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_export_node_image args with
    | Error msg -> check bool "missing node_id" true (contains msg "node_id")
    | Ok _ -> fail "should error"
  in

  let test_export_no_channel () =
    let args = `Assoc [("node_id", `String "1:1")] in
    match Mcp_plugin_handlers.handle_plugin_export_node_image args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_get_variables_no_channel () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_get_variables args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_apply_ops_missing_ops () =
    (* No ops and no channel → errors on ops first *)
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_apply_ops args with
    | Error msg -> check bool "missing ops" true (contains msg "ops")
    | Ok _ -> fail "should error"
  in

  let test_apply_ops_no_channel () =
    let args = `Assoc [("ops", `List [`String "op1"])] in
    match Mcp_plugin_handlers.handle_plugin_apply_ops args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_edit_node_missing_node_id () =
    let args = `Assoc [("properties", `Assoc [])] in
    match Mcp_plugin_handlers.handle_plugin_edit_node args with
    | Error msg -> check bool "missing node_id" true (contains msg "node_id")
    | Ok _ -> fail "should error"
  in

  let test_edit_node_missing_properties () =
    let args = `Assoc [("node_id", `String "1:1")] in
    match Mcp_plugin_handlers.handle_plugin_edit_node args with
    | Error msg -> check bool "missing properties" true (contains msg "properties")
    | Ok _ -> fail "should error"
  in

  let test_edit_node_no_channel () =
    (* Has node_id + properties but no channel *)
    let args = `Assoc [
      ("node_id", `String "1:1");
      ("properties", `Assoc [("fill", `String "#FF0000")]);
    ] in
    match Mcp_plugin_handlers.handle_plugin_edit_node args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_create_node_missing_type () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_create_node args with
    | Error msg -> check bool "missing type" true (contains msg "type")
    | Ok _ -> fail "should error"
  in

  let test_create_node_no_channel () =
    let args = `Assoc [("type", `String "rectangle")] in
    match Mcp_plugin_handlers.handle_plugin_create_node args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_delete_nodes_missing () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
    | Error msg -> check bool "missing node_ids" true (contains msg "node_ids")
    | Ok _ -> fail "should error"
  in

  let test_delete_nodes_no_channel () =
    let args = `Assoc [("node_ids", `List [`String "1:1"])] in
    match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_delete_nodes_empty_array () =
    (* Has channel_id but empty node_ids — but channel will fail first *)
    let args = `Assoc [("node_ids", `List [])] in
    match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
    | Error msg -> check bool "error" true (String.length msg > 0)
    | Ok _ -> fail "should error"
  in

  let test_delete_nodes_non_string_items () =
    let args = `Assoc [("node_ids", `List [`Int 1; `Int 2])] in
    match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
    | Error msg -> check bool "error" true (String.length msg > 0)
    | Ok _ -> fail "should error"
  in

  let test_delete_nodes_not_list () =
    let args = `Assoc [("node_ids", `String "not-a-list")] in
    match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
    | Error msg -> check bool "error" true (String.length msg > 0)
    | Ok _ -> fail "should error"
  in

  let test_batch_missing_actions () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_batch args with
    | Error msg -> check bool "missing actions" true (contains msg "actions")
    | Ok _ -> fail "should error"
  in

  let test_batch_no_channel () =
    let args = `Assoc [("actions", `List [`Assoc [("action", `String "test")]])] in
    match Mcp_plugin_handlers.handle_plugin_batch args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_subscribe_no_channel () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_use_channel_missing () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_use_channel args with
    | Error msg -> check bool "mentions channel_id" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_annotate_no_channel () =
    (* annotate → batch → resolve_channel_id → Error (no explicit, no default) *)
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_annotate args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_annotate_with_args_no_channel () =
    (* Exercise the color and node_id parsing before channel fails *)
    let args = `Assoc [
      ("message", `String "test note");
      ("color", `String "blue");
      ("node_id", `String "99:1");
    ] in
    match Mcp_plugin_handlers.handle_annotate args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  (* --- Combinator error paths (no channel, no default) --- *)

  let test_plugin_simple_no_channel () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.plugin_simple ~name:"test_cmd"
            ~build_payload:(fun _ -> `Assoc []) args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_plugin_node_missing_node_id () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.plugin_node ~name:"test_cmd"
            ~build_payload:(fun _nid _args -> `Assoc []) args with
    | Error msg -> check bool "missing node_id" true (contains msg "node_id")
    | Ok _ -> fail "should error"
  in

  let test_plugin_node_no_channel () =
    let args = `Assoc [("node_id", `String "1:1")] in
    match Mcp_plugin_handlers.plugin_node ~name:"test_cmd"
            ~build_payload:(fun _nid _args -> `Assoc []) args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_plugin_nodes_missing () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.plugin_nodes ~name:"test_cmd"
            ~build_payload:(fun _ids _args -> `Assoc []) args with
    | Error msg -> check bool "missing node_ids" true (contains msg "node_ids")
    | Ok _ -> fail "should error"
  in

  let test_plugin_nodes_empty () =
    let args = `Assoc [("node_ids", `List [])] in
    match Mcp_plugin_handlers.plugin_nodes ~name:"test_cmd"
            ~build_payload:(fun _ids _args -> `Assoc []) args with
    | Error msg -> check bool "empty node_ids" true (contains msg "node_ids")
    | Ok _ -> fail "should error"
  in

  let test_plugin_nodes_no_channel () =
    let args = `Assoc [("node_ids", `List [`String "1:1"])] in
    match Mcp_plugin_handlers.plugin_nodes ~name:"test_cmd"
            ~build_payload:(fun _ids _args -> `Assoc []) args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_plugin_custom_no_channel () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.plugin_custom ~name:"test_cmd"
            ~validate:(fun _args -> Ok "validated")
            ~build_payload:(fun _v _args -> `Assoc []) args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_plugin_custom_validation_fail () =
    (* Need a channel for this — register one so resolve_channel_id passes,
       then fail on validation, which is before plugin_exec *)
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"custom-val-ch" () in
    let args = `Assoc [("channel_id", `String ch)] in
    match Mcp_plugin_handlers.plugin_custom ~name:"test_cmd"
            ~validate:(fun _args -> Error "validation failed")
            ~build_payload:(fun _v _args -> `Assoc []) args with
    | Error msg -> check string "validation error" "validation failed" msg
    | Ok _ -> fail "should error"
  in

  (* --- Dispatch routing: actions that error before plugin_wait --- *)

  let test_dispatch_get_node_no_args () =
    let args = `Assoc [("action", `String "get_node")] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Error msg -> check bool "missing node_id" true (contains msg "node_id")
    | Ok _ -> fail "should error"
  in

  let test_dispatch_batch_no_args () =
    let args = `Assoc [("action", `String "batch")] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Error msg -> check bool "missing actions" true (contains msg "actions")
    | Ok _ -> fail "should error"
  in

  let test_dispatch_annotate_no_args () =
    let args = `Assoc [("action", `String "annotate")] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_dispatch_use_channel_no_args () =
    let args = `Assoc [("action", `String "use_channel")] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Error msg -> check bool "missing channel_id" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  let test_dispatch_read_selection_no_args () =
    let args = `Assoc [("action", `String "read_selection")] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Error msg -> check bool "missing channel" true (contains msg "channel_id")
    | Ok _ -> fail "should error"
  in

  (* ============================================================
     GROUP 2: Handlers that don't call plugin_wait.
     connect, status, use_channel are safe to call with real args.
     ============================================================ *)

  let test_connect_no_args () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_connect args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "contains status ok" true (contains s "ok")
    | Error msg -> fail msg
  in

  let test_connect_with_channel_id () =
    let args = `Assoc [("channel_id", `String "my-test-channel")] in
    match Mcp_plugin_handlers.handle_plugin_connect args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "has custom channel" true (contains s "my-test-channel")
    | Error msg -> fail msg
  in

  let test_dispatch_connect () =
    let args = `Assoc [("action", `String "connect")] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "has channel_id" true (contains s "channel_id")
    | Error msg -> fail msg
  in

  let test_status_ok () =
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_status args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "has limits" true (contains s "limits")
    | Error msg -> fail msg
  in

  let test_status_with_registered_channel () =
    let _ch = Figma_plugin_bridge.register_channel ~channel_id:"status-test-ch" () in
    let args = `Assoc [] in
    match Mcp_plugin_handlers.handle_plugin_status args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "has status-test-ch" true (contains s "status-test-ch")
    | Error msg -> fail msg
  in

  let test_dispatch_status () =
    let args = `Assoc [("action", `String "status")] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "has channels key" true (contains s "channels")
    | Error msg -> fail msg
  in

  let test_use_channel_ok () =
    let args = `Assoc [("channel_id", `String "ch-use-test")] in
    match Mcp_plugin_handlers.handle_plugin_use_channel args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "has ch-use-test" true (contains s "ch-use-test")
    | Error msg -> fail msg
  in

  (* ============================================================
     GROUP 3: Tests with registered channels that DON'T reach
     plugin_wait. These exercise code paths beyond resolve_channel_id.
     ============================================================ *)

  (* resolve_channel_id with default set *)
  let test_resolve_channel_default () =
    Figma_plugin_bridge.set_default_channel "default-ch";
    let args = `Assoc [] in
    match Mcp_plugin_handlers.resolve_channel_id args with
    | Ok id -> check string "default id" "default-ch" id
    | Error msg -> fail msg
  in

  let test_resolve_channel_explicit_wins () =
    (* Even with default set, explicit takes priority *)
    let args = `Assoc [("channel_id", `String "explicit-ch")] in
    match Mcp_plugin_handlers.resolve_channel_id args with
    | Ok id -> check string "explicit wins" "explicit-ch" id
    | Error msg -> fail msg
  in

  (* edit_node with registered channel + unknown property → no dispatch call.
     Properties that are unknown get added to errors list without calling
     Figma_plugin_bridge.enqueue_command, so no blocking. *)
  let test_edit_node_unknown_property () =
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"edit-unk-ch" () in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("node_id", `String "1:1");
      ("properties", `Assoc [("unknown_prop_xyz", `String "val")]);
    ] in
    match Mcp_plugin_handlers.handle_plugin_edit_node args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "has unknown error" true (contains s "Unknown property")
    | Error msg -> fail msg
  in

  (* edit_node: properties not an assoc → empty property list → no dispatch *)
  let test_edit_node_properties_not_assoc () =
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"edit-noa-ch" () in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("node_id", `String "5:5");
      ("properties", `List [`String "a"]);
    ] in
    match Mcp_plugin_handlers.handle_plugin_edit_node args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "applied 0" true (contains s "applied")
    | Error msg -> fail msg
  in

  (* batch: empty actions array → error before enqueue_command *)
  let test_batch_empty_actions () =
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"batch-empty-ch" () in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("actions", `List []);
    ] in
    match Mcp_plugin_handlers.handle_plugin_batch args with
    | Error msg -> check bool "non-empty error" true (contains msg "non-empty")
    | Ok _ -> fail "should error"
  in

  (* batch: actions not a list → actions = [] → non-empty error *)
  let test_batch_actions_not_list () =
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"batch-notlist-ch" () in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("actions", `String "not-a-list");
    ] in
    match Mcp_plugin_handlers.handle_plugin_batch args with
    | Error msg -> check bool "non-empty error" true (contains msg "non-empty")
    | Ok _ -> fail "should error"
  in

  (* delete_nodes: empty array after filter → error before enqueue *)
  let test_delete_nodes_empty_with_channel () =
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"del-empty-ch" () in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("node_ids", `List []);
    ] in
    match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
    | Error msg -> check bool "non-empty error" true (contains msg "non-empty")
    | Ok _ -> fail "should error"
  in

  let test_delete_nodes_non_strings_with_channel () =
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"del-ns-ch" () in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("node_ids", `List [`Int 1; `Bool true]);
    ] in
    match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
    | Error msg -> check bool "non-empty error" true (contains msg "non-empty")
    | Ok _ -> fail "should error"
  in

  let test_delete_nodes_not_list_with_channel () =
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"del-nl-ch" () in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("node_ids", `String "not-a-list");
    ] in
    match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
    | Error msg -> check bool "non-empty error" true (contains msg "non-empty")
    | Ok _ -> fail "should error"
  in

  (* subscribe_events: with registered channel + timeout_ms=0 → immediate Ok *)
  let test_subscribe_with_channel () =
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"sub-test-ch" () in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("timeout_ms", `Int 0);
    ] in
    match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "has events" true (contains s "events")
    | Error msg -> fail msg
  in

  let test_subscribe_with_event_types () =
    let ch = Figma_plugin_bridge.register_channel ~channel_id:"sub-filter-ch" () in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("timeout_ms", `Int 0);
      ("event_types", `List [`String "selection_change"; `String "document_change"]);
      ("max_events", `Int 5);
    ] in
    match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
    | Ok json ->
        let s = Yojson.Safe.to_string json in
        check bool "has count" true (contains s "count")
    | Error msg -> fail msg
  in

  (* ============================================================
     GROUP 4: Annotate color-parsing branches.
     annotate builds batch_args internally. Without a channel
     (no default, no explicit) → Error from resolve_channel_id.
     But the color-parsing code executes BEFORE resolve_channel_id
     in handle_plugin_batch because annotate runs its color logic
     first then delegates.
     ============================================================ *)

  (* Clear default channel for clean color tests.
     We can't clear it directly, so we use explicit missing. *)

  let test_annotate_color_default () =
    (* No color specified → yellow (default). No channel → error. *)
    let args = `Assoc [("message", `String "note")] in
    match Mcp_plugin_handlers.handle_annotate args with
    | Error _ -> ()  (* Expected: channel error. Color code ran. *)
    | Ok _ -> fail "should error without channel"
  in

  let test_annotate_color_blue () =
    let args = `Assoc [("color", `String "blue")] in
    match Mcp_plugin_handlers.handle_annotate args with
    | Error _ -> ()
    | Ok _ -> fail "should error"
  in

  let test_annotate_color_green () =
    let args = `Assoc [("color", `String "green")] in
    match Mcp_plugin_handlers.handle_annotate args with
    | Error _ -> ()
    | Ok _ -> fail "should error"
  in

  let test_annotate_color_red () =
    let args = `Assoc [("color", `String "red")] in
    match Mcp_plugin_handlers.handle_annotate args with
    | Error _ -> ()
    | Ok _ -> fail "should error"
  in

  let test_annotate_with_node_id () =
    let args = `Assoc [
      ("node_id", `String "99:1");
      ("message", `String "annotation msg");
    ] in
    match Mcp_plugin_handlers.handle_annotate args with
    | Error _ -> ()
    | Ok _ -> fail "should error"
  in

  (* ============== Run ============== *)

  run "Plugin Dispatch Coverage" [
    ("known_plugin_actions", [
      test_case "non-empty" `Quick test_known_actions_nonempty;
      test_case "has connect" `Quick test_known_actions_has_connect;
      test_case "has annotate" `Quick test_known_actions_has_annotate;
      test_case "count >50" `Quick test_known_actions_count;
    ]);
    ("suggest_action", [
      test_case "non-empty" `Quick test_suggest_nonempty;
      test_case "short input" `Quick test_suggest_short_input;
      test_case "empty input" `Quick test_suggest_empty_input;
    ]);
    ("resolve_channel_id", [
      test_case "explicit" `Quick test_resolve_channel_explicit;
    ]);
    ("handle_figma_plugin dispatch (no channel)", [
      test_case "missing action" `Quick test_dispatch_missing_action;
      test_case "unknown action" `Quick test_dispatch_unknown_action;
      test_case "get_node no args" `Quick test_dispatch_get_node_no_args;
      test_case "batch no args" `Quick test_dispatch_batch_no_args;
      test_case "annotate no args" `Quick test_dispatch_annotate_no_args;
      test_case "use_channel no args" `Quick test_dispatch_use_channel_no_args;
      test_case "read_selection no args" `Quick test_dispatch_read_selection_no_args;
    ]);
    ("handler error paths (no channel)", [
      test_case "read_selection" `Quick test_read_selection_no_channel;
      test_case "get_node missing node_id" `Quick test_get_node_missing_node_id;
      test_case "get_node no channel" `Quick test_get_node_no_channel;
      test_case "export missing node" `Quick test_export_missing_node;
      test_case "export no channel" `Quick test_export_no_channel;
      test_case "get_variables" `Quick test_get_variables_no_channel;
      test_case "apply_ops missing" `Quick test_apply_ops_missing_ops;
      test_case "apply_ops no channel" `Quick test_apply_ops_no_channel;
      test_case "edit_node missing node_id" `Quick test_edit_node_missing_node_id;
      test_case "edit_node missing props" `Quick test_edit_node_missing_properties;
      test_case "edit_node no channel" `Quick test_edit_node_no_channel;
      test_case "create_node missing type" `Quick test_create_node_missing_type;
      test_case "create_node no channel" `Quick test_create_node_no_channel;
      test_case "delete_nodes missing" `Quick test_delete_nodes_missing;
      test_case "delete_nodes no channel" `Quick test_delete_nodes_no_channel;
      test_case "delete_nodes empty" `Quick test_delete_nodes_empty_array;
      test_case "delete_nodes non-string" `Quick test_delete_nodes_non_string_items;
      test_case "delete_nodes not list" `Quick test_delete_nodes_not_list;
      test_case "batch missing" `Quick test_batch_missing_actions;
      test_case "batch no channel" `Quick test_batch_no_channel;
      test_case "subscribe" `Quick test_subscribe_no_channel;
      test_case "use_channel missing" `Quick test_use_channel_missing;
      test_case "annotate no channel" `Quick test_annotate_no_channel;
      test_case "annotate with args" `Quick test_annotate_with_args_no_channel;
    ]);
    (* Annotate color branches must run BEFORE default channel is set *)
    ("annotate color branches (no channel)", [
      test_case "default (yellow)" `Quick test_annotate_color_default;
      test_case "blue" `Quick test_annotate_color_blue;
      test_case "green" `Quick test_annotate_color_green;
      test_case "red" `Quick test_annotate_color_red;
      test_case "with node_id" `Quick test_annotate_with_node_id;
    ]);
    ("combinator error paths (no channel)", [
      test_case "plugin_simple" `Quick test_plugin_simple_no_channel;
      test_case "plugin_node missing" `Quick test_plugin_node_missing_node_id;
      test_case "plugin_node no ch" `Quick test_plugin_node_no_channel;
      test_case "plugin_nodes missing" `Quick test_plugin_nodes_missing;
      test_case "plugin_nodes empty" `Quick test_plugin_nodes_empty;
      test_case "plugin_nodes no ch" `Quick test_plugin_nodes_no_channel;
      test_case "plugin_custom no ch" `Quick test_plugin_custom_no_channel;
      test_case "plugin_custom valid fail" `Quick test_plugin_custom_validation_fail;
    ]);
    ("handle_plugin_connect (safe)", [
      test_case "no args" `Quick test_connect_no_args;
      test_case "with channel_id" `Quick test_connect_with_channel_id;
      test_case "dispatch connect" `Quick test_dispatch_connect;
    ]);
    ("handle_plugin_status (safe)", [
      test_case "ok" `Quick test_status_ok;
      test_case "with channel" `Quick test_status_with_registered_channel;
      test_case "dispatch status" `Quick test_dispatch_status;
    ]);
    ("handle_plugin_use_channel (safe)", [
      test_case "ok" `Quick test_use_channel_ok;
    ]);
    ("resolve_channel_id (with default)", [
      test_case "default" `Quick test_resolve_channel_default;
      test_case "explicit wins" `Quick test_resolve_channel_explicit_wins;
    ]);
    ("edit_node (registered ch, no dispatch)", [
      test_case "unknown property" `Quick test_edit_node_unknown_property;
      test_case "properties not assoc" `Quick test_edit_node_properties_not_assoc;
    ]);
    ("batch (registered ch, early error)", [
      test_case "empty actions" `Quick test_batch_empty_actions;
      test_case "actions not list" `Quick test_batch_actions_not_list;
    ]);
    ("delete_nodes (registered ch, early error)", [
      test_case "empty array" `Quick test_delete_nodes_empty_with_channel;
      test_case "non-strings" `Quick test_delete_nodes_non_strings_with_channel;
      test_case "not list" `Quick test_delete_nodes_not_list_with_channel;
    ]);
    ("subscribe_events (registered ch, safe)", [
      test_case "with channel" `Quick test_subscribe_with_channel;
      test_case "with event types" `Quick test_subscribe_with_event_types;
    ]);
  ]
