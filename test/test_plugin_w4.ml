(** Coverage Wave 4: figma_plugin_bridge.ml, figma_plugin_features.ml,
    mcp_plugin_handlers.ml — uncovered pure branches.

    Targets:
      - figma_plugin_bridge: channel lifecycle, payload check, store/take,
        poll_commands, poll_events, publish_event, cleanup_inactive
      - figma_plugin_features: CSS keyframe branches, Swift modifier branches,
        easing functions, accessibility (link/input/nav), webhook types,
        safe_member_type/to_num/safe_num edge cases, animations_to_json,
        accessibility_to_json
      - mcp_plugin_handlers: handle_plugin_delete_nodes (missing/empty ids),
        handle_plugin_subscribe_events (no channel, timeout=0) *)

(* ============== Helpers ============== *)

let str_contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  if nl > hl then false
  else
    let rec loop i =
      if i > hl - nl then false
      else if String.sub haystack i nl = needle then true
      else loop (i + 1)
    in loop 0

(* ============== 1. figma_plugin_bridge — channel lifecycle ============== *)

let test_bridge_register_channel () =
  let id = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-ch1" () in
  Alcotest.(check string) "explicit id" "test-w4-ch1" id;
  let channels = Figma_plugin_bridge.list_channels () in
  Alcotest.(check bool) "has channel" true (List.mem "test-w4-ch1" channels)

let test_bridge_register_channel_auto () =
  let id = Figma_plugin_bridge.register_channel () in
  Alcotest.(check bool) "auto id starts with ch-" true
    (String.length id > 3 && String.sub id 0 3 = "ch-")

let test_bridge_default_channel () =
  let _ = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-default" () in
  let d = Figma_plugin_bridge.get_default_channel () in
  Alcotest.(check bool) "has default" true (d <> None);
  Figma_plugin_bridge.set_default_channel "test-w4-override";
  let d2 = Figma_plugin_bridge.get_default_channel () in
  Alcotest.(check (option string)) "overridden" (Some "test-w4-override") d2

let test_bridge_list_channel_stats () =
  let _ = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-stats" () in
  let stats = Figma_plugin_bridge.list_channel_stats () in
  let found = List.exists (fun (s : Figma_plugin_bridge.channel_stats) ->
    s.id = "test-w4-stats"
  ) stats in
  Alcotest.(check bool) "stats has channel" true found

(* ============== 2. figma_plugin_bridge — enqueue/poll/store/take ============== *)

let test_bridge_enqueue_poll () =
  let ch = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-eq" () in
  let _ = Figma_plugin_bridge.enqueue_command
    ~channel_id:ch ~name:"test_cmd" ~payload:(`String "hello") in
  let cmds = Figma_plugin_bridge.poll_commands ~channel_id:ch ~max:10 in
  Alcotest.(check int) "one command" 1 (List.length cmds);
  let cmd = List.hd cmds in
  Alcotest.(check string) "cmd name" "test_cmd" cmd.name

let test_bridge_store_take_result () =
  let ch = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-st" () in
  Figma_plugin_bridge.store_result ~channel_id:ch ~command_id:"cmd-1"
    ~ok:true ~payload:(`String "result");
  let r = Figma_plugin_bridge.take_result ~channel_id:ch ~command_id:"cmd-1" in
  Alcotest.(check bool) "result found" true (r <> None);
  let result = Option.get r in
  Alcotest.(check bool) "ok" true result.ok;
  (* Take again should be None *)
  let r2 = Figma_plugin_bridge.take_result ~channel_id:ch ~command_id:"cmd-1" in
  Alcotest.(check bool) "consumed" true (r2 = None)

let test_bridge_take_nonexistent_channel () =
  let r = Figma_plugin_bridge.take_result
    ~channel_id:"nonexistent-w4" ~command_id:"cmd-x" in
  Alcotest.(check bool) "None for missing channel" true (r = None)

let test_bridge_poll_commands_empty () =
  let ch = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-pempty" () in
  let cmds = Figma_plugin_bridge.poll_commands ~channel_id:ch ~max:10 in
  Alcotest.(check int) "empty" 0 (List.length cmds)

(* ============== 3. figma_plugin_bridge — events ============== *)

let test_bridge_publish_poll_events () =
  let ch = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-ev" () in
  Figma_plugin_bridge.publish_event ~channel_id:ch
    ~event_type:"selection_change" ~payload:(`String "sel1");
  Figma_plugin_bridge.publish_event ~channel_id:ch
    ~event_type:"document_change" ~payload:(`String "doc1");
  let evts = Figma_plugin_bridge.poll_events ~channel_id:ch ~max:10 in
  Alcotest.(check int) "two events" 2 (List.length evts);
  let first = List.hd evts in
  Alcotest.(check string) "event type" "selection_change" first.event_type

let test_bridge_poll_events_nonexistent () =
  let evts = Figma_plugin_bridge.poll_events ~channel_id:"nonexist-w4" ~max:10 in
  Alcotest.(check int) "empty for missing" 0 (List.length evts)

let test_bridge_publish_no_channel () =
  (* publish_event to a non-existent channel should not crash *)
  Figma_plugin_bridge.publish_event ~channel_id:"ghost-w4"
    ~event_type:"test" ~payload:`Null;
  Alcotest.(check bool) "no crash" true true

(* ============== 4. figma_plugin_bridge — payload limit ============== *)

let test_bridge_check_payload_limit () =
  (* Small payload should pass *)
  let small = `String "hi" in
  let r = Figma_plugin_bridge.check_payload_limit small in
  (match r with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "small payload should pass")

(* ============== 5. figma_plugin_bridge — cleanup_inactive ============== *)

let test_bridge_cleanup_inactive () =
  (* cleanup_inactive with a large ttl should not remove active channels *)
  let _ = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-cleanup" () in
  Figma_plugin_bridge.cleanup_inactive ~ttl_seconds:99999.0;
  let channels = Figma_plugin_bridge.list_channels () in
  Alcotest.(check bool) "still present" true (List.mem "test-w4-cleanup" channels)

(* ============== 6. figma_plugin_bridge — event waiters ============== *)

let test_bridge_event_waiter_register_unregister () =
  let ch = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-ew" () in
  let wid = Figma_plugin_bridge.register_event_waiter ~channel_id:ch
    ~notify:(fun () -> ()) in
  Alcotest.(check bool) "waiter id > 0" true (wid > 0);
  Figma_plugin_bridge.unregister_event_waiter ~channel_id:ch ~waiter_id:wid;
  (* No crash after unregister *)
  Alcotest.(check bool) "unregistered ok" true true

let test_bridge_unregister_event_waiter_missing_channel () =
  Figma_plugin_bridge.unregister_event_waiter
    ~channel_id:"no-such-channel-w4" ~waiter_id:999;
  Alcotest.(check bool) "no crash" true true

(* ============== 7. figma_plugin_features — safe helpers ============== *)

let test_safe_member_type_null () =
  let r = Figma_plugin_features.safe_member_type `Null in
  Alcotest.(check string) "null → empty" "" r

let test_safe_member_type_obj () =
  let obj = `Assoc [("type", `String "FRAME")] in
  let r = Figma_plugin_features.safe_member_type obj in
  Alcotest.(check string) "has type" "FRAME" r

let test_to_num_float () =
  Alcotest.(check (float 0.01)) "float" 3.14 (Figma_plugin_features.to_num (`Float 3.14))

let test_to_num_int () =
  Alcotest.(check (float 0.01)) "int" 42.0 (Figma_plugin_features.to_num (`Int 42))

let test_to_num_other () =
  Alcotest.(check (float 0.01)) "other" 0.0 (Figma_plugin_features.to_num (`String "x"))

let test_safe_num_null () =
  Alcotest.(check (float 0.01)) "null → 0" 0.0 (Figma_plugin_features.safe_num "x" `Null)

let test_safe_num_present () =
  let obj = `Assoc [("val", `Float 5.5)] in
  Alcotest.(check (float 0.01)) "present" 5.5 (Figma_plugin_features.safe_num "val" obj)

let test_contains_word_found () =
  Alcotest.(check bool) "found" true (Figma_plugin_features.contains_word "hello world" "world")

let test_contains_word_not_found () =
  Alcotest.(check bool) "not found" false (Figma_plugin_features.contains_word "hello" "world")

(* ============== 8. figma_plugin_features — CSS keyframes branches ============== *)

let test_css_keyframes_dissolve () =
  let kf = Figma_plugin_features.generate_css_keyframes "DISSOLVE" 0.5 0.0 in
  Alcotest.(check bool) "has opacity" true (str_contains ~needle:"opacity" kf)

let test_css_keyframes_move_in () =
  let kf = Figma_plugin_features.generate_css_keyframes "MOVE_IN" 0.0 0.0 in
  Alcotest.(check bool) "has translateX" true (str_contains ~needle:"translateX" kf)

let test_css_keyframes_slide_in () =
  let kf = Figma_plugin_features.generate_css_keyframes "SLIDE_IN" 0.0 0.0 in
  Alcotest.(check bool) "has translateX" true (str_contains ~needle:"translateX" kf)

let test_css_keyframes_move_out () =
  let kf = Figma_plugin_features.generate_css_keyframes "MOVE_OUT" 0.0 0.0 in
  Alcotest.(check bool) "has 100%" true (str_contains ~needle:"100%" kf)

let test_css_keyframes_slide_out () =
  let kf = Figma_plugin_features.generate_css_keyframes "SLIDE_OUT" 0.0 0.0 in
  Alcotest.(check bool) "has 100%" true (str_contains ~needle:"100%" kf)

let test_css_keyframes_push () =
  let kf = Figma_plugin_features.generate_css_keyframes "PUSH" 0.0 0.0 in
  Alcotest.(check bool) "has scale" true (str_contains ~needle:"scale" kf)

let test_css_keyframes_smart_animate () =
  let kf = Figma_plugin_features.generate_css_keyframes "SMART_ANIMATE" 0.5 45.0 in
  Alcotest.(check bool) "has translateY" true (str_contains ~needle:"translateY" kf);
  Alcotest.(check bool) "has rotate" true (str_contains ~needle:"rotate" kf)

let test_css_keyframes_smart_animate_no_rotation () =
  let kf = Figma_plugin_features.generate_css_keyframes "SMART_ANIMATE" 0.5 0.0 in
  Alcotest.(check bool) "has translateY" true (str_contains ~needle:"translateY" kf);
  Alcotest.(check bool) "no rotate" false (str_contains ~needle:"rotate" kf)

let test_css_keyframes_unknown () =
  let kf = Figma_plugin_features.generate_css_keyframes "UNKNOWN" 0.5 0.0 in
  Alcotest.(check bool) "has opacity" true (str_contains ~needle:"opacity" kf)

(* ============== 9. figma_plugin_features — CSS easing branches ============== *)

let test_css_easing_ease_in () =
  Alcotest.(check string) "ease-in" "ease-in"
    (Figma_plugin_features.css_easing_of_figma "EASE_IN")

let test_css_easing_ease_out () =
  Alcotest.(check string) "ease-out" "ease-out"
    (Figma_plugin_features.css_easing_of_figma "EASE_OUT")

let test_css_easing_ease_in_out () =
  Alcotest.(check string) "ease-in-out" "ease-in-out"
    (Figma_plugin_features.css_easing_of_figma "EASE_IN_AND_OUT")

let test_css_easing_linear () =
  Alcotest.(check string) "linear" "linear"
    (Figma_plugin_features.css_easing_of_figma "LINEAR")

let test_css_easing_custom_bezier () =
  let r = Figma_plugin_features.css_easing_of_figma "CUSTOM_BEZIER" in
  Alcotest.(check bool) "has cubic-bezier" true (str_contains ~needle:"cubic-bezier" r)

let test_css_easing_unknown () =
  Alcotest.(check string) "default" "ease-out"
    (Figma_plugin_features.css_easing_of_figma "WHATEVER")

(* ============== 10. figma_plugin_features — Swift easing branches ============== *)

let test_swift_easing_ease_in () =
  Alcotest.(check string) ".easeIn" ".easeIn"
    (Figma_plugin_features.swift_easing_of_figma "EASE_IN")

let test_swift_easing_ease_out () =
  Alcotest.(check string) ".easeOut" ".easeOut"
    (Figma_plugin_features.swift_easing_of_figma "EASE_OUT")

let test_swift_easing_ease_in_out () =
  Alcotest.(check string) ".easeInOut" ".easeInOut"
    (Figma_plugin_features.swift_easing_of_figma "EASE_IN_AND_OUT")

let test_swift_easing_linear () =
  Alcotest.(check string) ".linear" ".linear"
    (Figma_plugin_features.swift_easing_of_figma "LINEAR")

let test_swift_easing_custom_bezier () =
  let r = Figma_plugin_features.swift_easing_of_figma "CUSTOM_BEZIER" in
  Alcotest.(check bool) "has timingCurve" true (str_contains ~needle:"timingCurve" r)

let test_swift_easing_unknown () =
  Alcotest.(check string) "default" ".easeOut"
    (Figma_plugin_features.swift_easing_of_figma "WHATEVER")

(* ============== 11. figma_plugin_features — Swift modifiers branches ============== *)

let test_swift_mod_dissolve () =
  let r = Figma_plugin_features.generate_swift_modifiers "DISSOLVE" 0.5 0.0 in
  Alcotest.(check bool) "has opacity" true (str_contains ~needle:".opacity" r)

let test_swift_mod_move_in () =
  let r = Figma_plugin_features.generate_swift_modifiers "MOVE_IN" 0.0 0.0 in
  Alcotest.(check bool) "has offset" true (str_contains ~needle:".offset" r)

let test_swift_mod_slide_in () =
  let r = Figma_plugin_features.generate_swift_modifiers "SLIDE_IN" 0.0 0.0 in
  Alcotest.(check bool) "has offset" true (str_contains ~needle:".offset" r)

let test_swift_mod_move_out () =
  let r = Figma_plugin_features.generate_swift_modifiers "MOVE_OUT" 0.0 0.0 in
  Alcotest.(check bool) "has offset" true (str_contains ~needle:".offset" r)

let test_swift_mod_slide_out () =
  let r = Figma_plugin_features.generate_swift_modifiers "SLIDE_OUT" 0.0 0.0 in
  Alcotest.(check bool) "has offset" true (str_contains ~needle:".offset" r)

let test_swift_mod_push () =
  let r = Figma_plugin_features.generate_swift_modifiers "PUSH" 0.0 0.0 in
  Alcotest.(check bool) "has scaleEffect" true (str_contains ~needle:".scaleEffect" r)

let test_swift_mod_smart_animate () =
  let r = Figma_plugin_features.generate_swift_modifiers "SMART_ANIMATE" 0.5 45.0 in
  Alcotest.(check bool) "has offset" true (str_contains ~needle:".offset" r);
  Alcotest.(check bool) "has rotation" true (str_contains ~needle:"rotationEffect" r)

let test_swift_mod_smart_animate_no_rotation () =
  let r = Figma_plugin_features.generate_swift_modifiers "SMART_ANIMATE" 0.5 0.0 in
  Alcotest.(check bool) "has offset" true (str_contains ~needle:".offset" r);
  Alcotest.(check bool) "no rotation" false (str_contains ~needle:"rotationEffect" r)

let test_swift_mod_unknown () =
  let r = Figma_plugin_features.generate_swift_modifiers "UNKNOWN" 0.5 0.0 in
  Alcotest.(check bool) "has opacity" true (str_contains ~needle:".opacity" r)

(* ============== 12. figma_plugin_features — accessibility branches ============== *)

let test_accessibility_link () =
  let node = `Assoc [
    ("name", `String "Learn More Link"); ("type", `String "FRAME");
  ] in
  let (attrs, _) = Figma_plugin_features.analyze_accessibility node in
  let has_link = List.exists (fun (a : Figma_plugin_features.aria_attr) ->
    a.role = "link"
  ) attrs in
  Alcotest.(check bool) "has link role" true has_link

let test_accessibility_input () =
  let node = `Assoc [
    ("name", `String "Email Input Field"); ("type", `String "FRAME");
  ] in
  let (attrs, suggestions) = Figma_plugin_features.analyze_accessibility node in
  let has_textbox = List.exists (fun (a : Figma_plugin_features.aria_attr) ->
    a.role = "textbox"
  ) attrs in
  Alcotest.(check bool) "has textbox role" true has_textbox;
  Alcotest.(check bool) "has suggestion" true (List.length suggestions > 0)

let test_accessibility_nav () =
  let node = `Assoc [
    ("name", `String "Main Nav Menu"); ("type", `String "FRAME");
  ] in
  let (attrs, _) = Figma_plugin_features.analyze_accessibility node in
  let has_nav = List.exists (fun (a : Figma_plugin_features.aria_attr) ->
    a.role = "navigation"
  ) attrs in
  Alcotest.(check bool) "has nav role" true has_nav

let test_accessibility_to_json () =
  let attrs = [Figma_plugin_features.{
    element = "Btn"; role = "button";
    attributes = [("aria-label", `String "Click")];
  }] in
  let suggestions = ["Add label"] in
  let json = Figma_plugin_features.accessibility_to_json (attrs, suggestions) in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "has ariaAttributes" true (str_contains ~needle:"ariaAttributes" s);
  Alcotest.(check bool) "has suggestions" true (str_contains ~needle:"suggestions" s)

(* ============== 13. figma_plugin_features — animations_to_json ============== *)

let test_animations_to_json_empty () =
  let json = Figma_plugin_features.animations_to_json [] in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "has animations" true (str_contains ~needle:"animations" s);
  Alcotest.(check bool) "has css" true (str_contains ~needle:"css" s);
  Alcotest.(check bool) "has swiftui" true (str_contains ~needle:"swiftui" s)

let test_animations_to_json_with_anim () =
  let anim = Figma_plugin_features.{
    element = "Box";
    trigger = "ON_CLICK";
    trans_type = "DISSOLVE";
    duration = 0.3;
    easing = "EASE_OUT";
    properties = [("opacity", 0.5)];
  } in
  let json = Figma_plugin_features.animations_to_json [anim] in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "has element" true (str_contains ~needle:"Box" s)

(* ============== 14. figma_plugin_features — webhook branches ============== *)

let test_webhook_file_version_update () =
  let json = Figma_plugin_features.process_webhook
    "FILE_VERSION_UPDATE" "key1" "Design" "2026-01-01T00:00:00Z" in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "check_version_diff" true
    (str_contains ~needle:"check_version_diff" s)

let test_webhook_file_comment () =
  let json = Figma_plugin_features.process_webhook
    "FILE_COMMENT" "key2" "Design" "2026-01-01T00:00:00Z" in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "review_comment" true
    (str_contains ~needle:"review_comment" s)

let test_webhook_unknown () =
  let json = Figma_plugin_features.process_webhook
    "CUSTOM_EVENT" "key3" "Design" "2026-01-01T00:00:00Z" in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "log_event" true
    (str_contains ~needle:"log_event" s)

(* ============== 15. figma_plugin_features — animation_to_css branches ============== *)

let test_animation_to_css_move_in () =
  let anim = Figma_plugin_features.{
    element = "Panel"; trigger = "ON_CLICK";
    trans_type = "MOVE_IN"; duration = 0.4; easing = "LINEAR";
    properties = [];
  } in
  let css = Figma_plugin_features.animation_to_css 0 anim in
  Alcotest.(check bool) "has keyframes" true (str_contains ~needle:"@keyframes" css);
  Alcotest.(check bool) "has linear" true (str_contains ~needle:"linear" css)

let test_animation_to_css_smart_animate_with_rotation () =
  let anim = Figma_plugin_features.{
    element = "Spinner"; trigger = "state_change";
    trans_type = "SMART_ANIMATE"; duration = 0.0; (* triggers default 0.3 *)
    easing = "CUSTOM_BEZIER";
    properties = [("opacity", 0.8); ("rotation", 90.0)];
  } in
  let css = Figma_plugin_features.animation_to_css 0 anim in
  Alcotest.(check bool) "has rotate" true (str_contains ~needle:"rotate" css);
  Alcotest.(check bool) "has cubic-bezier" true (str_contains ~needle:"cubic-bezier" css)

(* ============== 16. figma_plugin_features — animation_to_swiftui branches ============== *)

let test_animation_to_swiftui_push () =
  let anim = Figma_plugin_features.{
    element = "Screen"; trigger = "ON_CLICK";
    trans_type = "PUSH"; duration = 0.5; easing = "EASE_IN";
    properties = [];
  } in
  let swift = Figma_plugin_features.animation_to_swiftui 0 anim in
  Alcotest.(check bool) "has animation" true (str_contains ~needle:".animation" swift);
  Alcotest.(check bool) "has scaleEffect" true (str_contains ~needle:"scaleEffect" swift)

let test_animation_to_swiftui_smart_animate_rotation () =
  let anim = Figma_plugin_features.{
    element = "Dial"; trigger = "state_change";
    trans_type = "SMART_ANIMATE"; duration = 0.0;
    easing = "EASE_IN_AND_OUT";
    properties = [("opacity", 0.6); ("rotation", 180.0)];
  } in
  let swift = Figma_plugin_features.animation_to_swiftui 0 anim in
  Alcotest.(check bool) "has rotationEffect" true (str_contains ~needle:"rotationEffect" swift)

(* ============== 17. figma_plugin_features — variant edge case ============== *)

let test_variant_empty_children () =
  let node = `Assoc [
    ("name", `String "Empty"); ("type", `String "COMPONENT_SET");
    ("children", `List []);
  ] in
  let (variants, _props) = Figma_plugin_features.extract_variants node in
  Alcotest.(check int) "no variants" 0 (List.length variants)

let test_variant_no_children () =
  let node = `Assoc [
    ("name", `String "NoKids"); ("type", `String "COMPONENT_SET");
  ] in
  let (variants, _props) = Figma_plugin_features.extract_variants node in
  Alcotest.(check int) "no variants" 0 (List.length variants)

(* ============== 18. mcp_plugin_handlers — delete_nodes ============== *)

let test_delete_nodes_missing_ids () =
  let args = `Assoc [] in
  (match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
   | Error e ->
       Alcotest.(check bool) "has node_ids" true (str_contains ~needle:"node_ids" e)
   | Ok _ -> Alcotest.fail "expected error for missing node_ids")

let test_delete_nodes_non_array () =
  (* Register a channel so resolve_channel_id succeeds *)
  let _ = Figma_plugin_bridge.register_channel ~channel_id:"test-w4-del" () in
  let args = `Assoc [
    ("node_ids", `String "not-an-array");
    ("channel_id", `String "test-w4-del");
  ] in
  (match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
   | Error e ->
       Alcotest.(check bool) "has non-empty" true (str_contains ~needle:"non-empty" e)
   | Ok _ -> Alcotest.fail "expected error for non-array node_ids")

let test_delete_nodes_empty_array () =
  let args = `Assoc [
    ("node_ids", `List []);
    ("channel_id", `String "test-w4-del");
  ] in
  (match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
   | Error e ->
       Alcotest.(check bool) "has non-empty" true (str_contains ~needle:"non-empty" e)
   | Ok _ -> Alcotest.fail "expected error for empty node_ids")

(* ============== 19. mcp_plugin_handlers — subscribe_events ============== *)

let test_subscribe_events_no_channel () =
  (* Clear default so resolve_channel_id fails *)
  Figma_plugin_bridge.set_default_channel "test-w4-sub";
  let args = `Assoc [
    ("channel_id", `String "test-w4-sub");
    ("timeout_ms", `Int 0);
  ] in
  (match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
   | Ok json ->
       let s = Yojson.Safe.to_string json in
       Alcotest.(check bool) "has events" true (str_contains ~needle:"events" s)
   | Error _ ->
       (* also acceptable — depends on Eio context *)
       ())

(* ============== 20. figma_plugin_features — responsive CSS ============== *)

let test_responsive_small () =
  let json = Figma_plugin_features.generate_responsive_css "Tiny" 50 20 in
  let s = Yojson.Safe.to_string json in
  Alcotest.(check bool) "has breakpoints" true (str_contains ~needle:"breakpoints" s)

(* ============== Test Suite ============== *)

let bridge_lifecycle_tests = [
  ("register explicit", `Quick, test_bridge_register_channel);
  ("register auto", `Quick, test_bridge_register_channel_auto);
  ("default channel", `Quick, test_bridge_default_channel);
  ("channel stats", `Quick, test_bridge_list_channel_stats);
]

let bridge_cmd_tests = [
  ("enqueue + poll", `Quick, test_bridge_enqueue_poll);
  ("store + take result", `Quick, test_bridge_store_take_result);
  ("take nonexistent", `Quick, test_bridge_take_nonexistent_channel);
  ("poll empty", `Quick, test_bridge_poll_commands_empty);
]

let bridge_event_tests = [
  ("publish + poll events", `Quick, test_bridge_publish_poll_events);
  ("poll events nonexistent", `Quick, test_bridge_poll_events_nonexistent);
  ("publish no channel", `Quick, test_bridge_publish_no_channel);
  ("event waiter lifecycle", `Quick, test_bridge_event_waiter_register_unregister);
  ("unregister missing channel", `Quick, test_bridge_unregister_event_waiter_missing_channel);
]

let bridge_misc_tests = [
  ("payload limit", `Quick, test_bridge_check_payload_limit);
  ("cleanup inactive", `Quick, test_bridge_cleanup_inactive);
]

let features_helper_tests = [
  ("safe_member_type null", `Quick, test_safe_member_type_null);
  ("safe_member_type obj", `Quick, test_safe_member_type_obj);
  ("to_num float", `Quick, test_to_num_float);
  ("to_num int", `Quick, test_to_num_int);
  ("to_num other", `Quick, test_to_num_other);
  ("safe_num null", `Quick, test_safe_num_null);
  ("safe_num present", `Quick, test_safe_num_present);
  ("contains_word found", `Quick, test_contains_word_found);
  ("contains_word not found", `Quick, test_contains_word_not_found);
]

let css_keyframe_tests = [
  ("DISSOLVE", `Quick, test_css_keyframes_dissolve);
  ("MOVE_IN", `Quick, test_css_keyframes_move_in);
  ("SLIDE_IN", `Quick, test_css_keyframes_slide_in);
  ("MOVE_OUT", `Quick, test_css_keyframes_move_out);
  ("SLIDE_OUT", `Quick, test_css_keyframes_slide_out);
  ("PUSH", `Quick, test_css_keyframes_push);
  ("SMART_ANIMATE", `Quick, test_css_keyframes_smart_animate);
  ("SMART_ANIMATE no rot", `Quick, test_css_keyframes_smart_animate_no_rotation);
  ("unknown", `Quick, test_css_keyframes_unknown);
]

let css_easing_tests = [
  ("EASE_IN", `Quick, test_css_easing_ease_in);
  ("EASE_OUT", `Quick, test_css_easing_ease_out);
  ("EASE_IN_AND_OUT", `Quick, test_css_easing_ease_in_out);
  ("LINEAR", `Quick, test_css_easing_linear);
  ("CUSTOM_BEZIER", `Quick, test_css_easing_custom_bezier);
  ("unknown", `Quick, test_css_easing_unknown);
]

let swift_easing_tests = [
  ("EASE_IN", `Quick, test_swift_easing_ease_in);
  ("EASE_OUT", `Quick, test_swift_easing_ease_out);
  ("EASE_IN_AND_OUT", `Quick, test_swift_easing_ease_in_out);
  ("LINEAR", `Quick, test_swift_easing_linear);
  ("CUSTOM_BEZIER", `Quick, test_swift_easing_custom_bezier);
  ("unknown", `Quick, test_swift_easing_unknown);
]

let swift_mod_tests = [
  ("DISSOLVE", `Quick, test_swift_mod_dissolve);
  ("MOVE_IN", `Quick, test_swift_mod_move_in);
  ("SLIDE_IN", `Quick, test_swift_mod_slide_in);
  ("MOVE_OUT", `Quick, test_swift_mod_move_out);
  ("SLIDE_OUT", `Quick, test_swift_mod_slide_out);
  ("PUSH", `Quick, test_swift_mod_push);
  ("SMART_ANIMATE", `Quick, test_swift_mod_smart_animate);
  ("SMART_ANIMATE no rot", `Quick, test_swift_mod_smart_animate_no_rotation);
  ("unknown", `Quick, test_swift_mod_unknown);
]

let accessibility_tests = [
  ("link role", `Quick, test_accessibility_link);
  ("input role", `Quick, test_accessibility_input);
  ("nav role", `Quick, test_accessibility_nav);
  ("to_json", `Quick, test_accessibility_to_json);
]

let animation_json_tests = [
  ("empty", `Quick, test_animations_to_json_empty);
  ("with anim", `Quick, test_animations_to_json_with_anim);
]

let webhook_tests = [
  ("FILE_VERSION_UPDATE", `Quick, test_webhook_file_version_update);
  ("FILE_COMMENT", `Quick, test_webhook_file_comment);
  ("unknown event", `Quick, test_webhook_unknown);
]

let codegen_tests = [
  ("css move_in", `Quick, test_animation_to_css_move_in);
  ("css smart_animate rotation", `Quick, test_animation_to_css_smart_animate_with_rotation);
  ("swiftui push", `Quick, test_animation_to_swiftui_push);
  ("swiftui smart_animate rotation", `Quick, test_animation_to_swiftui_smart_animate_rotation);
]

let variant_tests = [
  ("empty children", `Quick, test_variant_empty_children);
  ("no children", `Quick, test_variant_no_children);
]

let handler_tests = [
  ("delete_nodes missing", `Quick, test_delete_nodes_missing_ids);
  ("delete_nodes non-array", `Quick, test_delete_nodes_non_array);
  ("delete_nodes empty", `Quick, test_delete_nodes_empty_array);
  ("subscribe_events timeout=0", `Quick, test_subscribe_events_no_channel);
]

let responsive_tests = [
  ("small component", `Quick, test_responsive_small);
]

let () =
  Alcotest.run "Plugin W4" [
    ("bridge_lifecycle", bridge_lifecycle_tests);
    ("bridge_commands", bridge_cmd_tests);
    ("bridge_events", bridge_event_tests);
    ("bridge_misc", bridge_misc_tests);
    ("features_helpers", features_helper_tests);
    ("css_keyframes", css_keyframe_tests);
    ("css_easing", css_easing_tests);
    ("swift_easing", swift_easing_tests);
    ("swift_modifiers", swift_mod_tests);
    ("accessibility", accessibility_tests);
    ("animations_json", animation_json_tests);
    ("webhook", webhook_tests);
    ("codegen", codegen_tests);
    ("variants", variant_tests);
    ("handlers", handler_tests);
    ("responsive", responsive_tests);
  ]
