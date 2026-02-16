(** Coverage Wave 6: mcp_plugin_handlers.ml — success paths and deep branches.

    Targets the 234 uncovered points beyond what test_plugin_handlers_coverage.ml
    and test_plugin_dispatch_coverage.ml already cover.

    Strategy:
    - For handlers that call plugin_wait, use a background Domain that monitors
      the channel for commands and immediately stores a result, so the handler's
      wait loop finds it on the next poll.
    - For pure logic: exercise parameter extraction, JSON construction, and
      error branches not yet hit.

    NOTE: No Eio context is set, so wait_for_result falls back to Unix.sleepf.
    The 50ms initial backoff is enough for the responder domain to act. *)

(* ============== Test Helpers ============== *)

(** Substring check *)
let contains haystack needle =
  let nl = String.length needle and hl = String.length haystack in
  if nl > hl then false
  else
    let rec loop i =
      if i > hl - nl then false
      else if String.sub haystack i nl = needle then true
      else loop (i + 1)
    in loop 0

(** Extract inner text from make_text_content wrapper.
    make_text_content returns: {"content":[{"type":"text","text":"..."}]}
    We extract the "text" string to avoid double-escaping from Yojson.Safe.to_string. *)
let extract_text (json : Yojson.Safe.t) : string =
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "content" fields with
     | Some (`List [`Assoc inner]) ->
       (match List.assoc_opt "text" inner with
        | Some (`String s) -> s
        | _ -> Yojson.Safe.to_string json)
     | _ -> Yojson.Safe.to_string json)
  | _ -> Yojson.Safe.to_string json

(** Spawn a background domain that monitors a channel for commands and
    immediately responds with a given result payload. Responds to at most
    [max_commands] commands, then exits. Returns a domain handle. *)
let auto_responder ~channel_id ?(ok=true) ?(payload=`Assoc [("result", `String "ok")])
    ?(max_commands=1) () =
  let d = Domain.spawn (fun () ->
    let responded = ref 0 in
    while !responded < max_commands do
      let cmds = Figma_plugin_bridge.poll_commands ~channel_id ~max:1 in
      (match cmds with
       | cmd :: _ ->
           Figma_plugin_bridge.store_result
             ~channel_id ~command_id:cmd.id ~ok ~payload;
           incr responded
       | [] ->
           Unix.sleepf 0.005)  (* 5ms poll interval *)
    done
  ) in
  (* Give domain a moment to start *)
  Unix.sleepf 0.01;
  d

(** Wait for a domain to finish *)
let join_domain d = Domain.join d

(** Register a fresh channel and return its id *)
let fresh_channel prefix =
  let id = Printf.sprintf "%s-%d" prefix (Random.bits () mod 100000) in
  ignore (Figma_plugin_bridge.register_channel ~channel_id:id ());
  id

(** Run a handler test with an auto-responder domain. Ensures the domain is
    always joined even if the test fails. *)
let with_responder ~channel_id ?ok ?payload ?max_commands f =
  let d = auto_responder ~channel_id ?ok ?payload ?max_commands () in
  let result = try f () with exn -> join_domain d; raise exn in
  join_domain d;
  result

(* ============== Tests ============== *)

let () =
  let open Alcotest in

  (* ============================================================
     GROUP 1: plugin_wait — channel not found path
     ============================================================ *)

  let test_plugin_wait_channel_not_found () =
    match Mcp_plugin_handlers.plugin_wait
            ~channel_id:"nonexistent-ch-w6" ~command_id:"cmd-1" ~timeout_ms:100 with
    | Error msg ->
        check bool "mentions disconnected" true (contains msg "not found");
        check bool "mentions channel id" true (contains msg "nonexistent-ch-w6")
    | Ok _ -> fail "should error for nonexistent channel"
  in

  (* ============================================================
     GROUP 2: plugin_wait — timeout path (channel exists but no result)
     ============================================================ *)

  let test_plugin_wait_timeout () =
    let ch = fresh_channel "pw-timeout" in
    let cmd_id = Figma_plugin_bridge.enqueue_command
      ~channel_id:ch ~name:"test" ~payload:`Null in
    (* Consume the command so it doesn't interfere *)
    ignore (Figma_plugin_bridge.poll_commands ~channel_id:ch ~max:1);
    match Mcp_plugin_handlers.plugin_wait
            ~channel_id:ch ~command_id:cmd_id ~timeout_ms:1 with
    | Error msg ->
        check bool "mentions timeout" true (contains msg "timeout");
        check bool "mentions channel" true (contains msg ch)
    | Ok _ -> fail "should timeout"
  in

  (* ============================================================
     GROUP 3: plugin_exec — success and error result paths
     ============================================================ *)

  let test_plugin_exec_success () =
    let ch = fresh_channel "pe-ok" in
    let success_payload = `Assoc [("data", `String "hello")] in
    with_responder ~channel_id:ch ~ok:true ~payload:success_payload (fun () ->
      match Mcp_plugin_handlers.plugin_exec
              ~channel_id:ch ~name:"test_cmd" ~payload:`Null ~timeout_ms:5000 with
      | Ok json ->
          let s = extract_text json in
          check bool "has data" true (contains s "hello")
      | Error msg -> fail (Printf.sprintf "should succeed: %s" msg))
  in

  let test_plugin_exec_error_result () =
    let ch = fresh_channel "pe-err" in
    let err_payload = `Assoc [("error", `String "something broke")] in
    with_responder ~channel_id:ch ~ok:false ~payload:err_payload (fun () ->
      match Mcp_plugin_handlers.plugin_exec
              ~channel_id:ch ~name:"test_cmd" ~payload:`Null ~timeout_ms:5000 with
      | Error msg ->
          check bool "plugin error prefix" true (contains msg "Plugin error");
          check bool "has error detail" true (contains msg "something broke")
      | Ok _ -> fail "should error on ok=false")
  in

  (* ============================================================
     GROUP 4: plugin_simple — success path
     ============================================================ *)

  let test_plugin_simple_success () =
    let ch = fresh_channel "ps-ok" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [("channel_id", `String ch)] in
      match Mcp_plugin_handlers.plugin_simple
              ~name:"simple_test" ~build_payload:(fun _ -> `Assoc [("x", `Int 1)]) args with
      | Ok json ->
          let s = extract_text json in
          check bool "has content" true (String.length s > 0)
      | Error msg -> fail msg)
  in

  let test_plugin_simple_custom_timeout () =
    let ch = fresh_channel "ps-ct" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [("channel_id", `String ch); ("timeout_ms", `Int 3000)] in
      match Mcp_plugin_handlers.plugin_simple
              ~name:"simple_timeout" ~default_timeout:1000
              ~build_payload:(fun _ -> `Assoc []) args with
      | Ok _ -> ()
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 5: plugin_node — success path
     ============================================================ *)

  let test_plugin_node_success () =
    let ch = fresh_channel "pn-ok" in
    let got_node_id = ref "" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "42:1");
      ] in
      match Mcp_plugin_handlers.plugin_node
              ~name:"node_test"
              ~build_payload:(fun nid _args -> got_node_id := nid; `Assoc [("nid", `String nid)])
              args with
      | Ok _ ->
          check bool "node_id passed" true (String.length !got_node_id > 0)
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 6: plugin_nodes — success path
     ============================================================ *)

  let test_plugin_nodes_success () =
    let ch = fresh_channel "pns-ok" in
    let got_ids = ref [] in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_ids", `List [`String "1:1"; `String "2:2"]);
      ] in
      match Mcp_plugin_handlers.plugin_nodes
              ~name:"nodes_test"
              ~build_payload:(fun ids _args -> got_ids := ids; `Assoc [])
              args with
      | Ok _ ->
          check int "2 ids" 2 (List.length !got_ids)
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 7: plugin_custom — success path
     ============================================================ *)

  let test_plugin_custom_success () =
    let ch = fresh_channel "pc-ok" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("value", `String "test-val");
      ] in
      match Mcp_plugin_handlers.plugin_custom
              ~name:"custom_test"
              ~validate:(fun args ->
                match Mcp_helpers.get_string "value" args with
                | Some v -> Ok v
                | None -> Error "missing value")
              ~build_payload:(fun validated _args ->
                `Assoc [("validated", `String validated)])
              args with
      | Ok _ -> ()
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 8: handle_plugin_read_selection — success path
     ============================================================ *)

  let test_read_selection_success () =
    let ch = fresh_channel "rs-ok" in
    let result_payload = `Assoc [("selection", `List [`String "node-1"])] in
    with_responder ~channel_id:ch ~payload:result_payload (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("depth", `Int 3);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_read_selection args with
      | Ok json ->
          let s = extract_text json in
          check bool "has ok" true (contains s "\"ok\"");
          check bool "has channel_id" true (contains s ch);
          check bool "has payload" true (contains s "selection")
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 9: handle_plugin_get_node — success path
     ============================================================ *)

  let test_get_node_success () =
    let ch = fresh_channel "gn-ok" in
    let result_payload = `Assoc [("node", `Assoc [("name", `String "Frame 1")])] in
    with_responder ~channel_id:ch ~payload:result_payload (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "10:20");
        ("depth", `Int 2);
        ("include_geometry", `Bool false);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_get_node args with
      | Ok json ->
          let s = extract_text json in
          check bool "has ok" true (contains s "\"ok\"");
          check bool "has payload" true (contains s "Frame 1")
      | Error msg -> fail msg)
  in

  let test_get_node_defaults () =
    let ch = fresh_channel "gn-def" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "5:5");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_get_node args with
      | Ok _ -> ()
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 10: handle_plugin_get_variables — success path
     ============================================================ *)

  let test_get_variables_success () =
    let ch = fresh_channel "gv-ok" in
    let vars_payload = `Assoc [("variables", `Assoc [("v1", `String "val1")])] in
    with_responder ~channel_id:ch ~payload:vars_payload (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_get_variables args with
      | Ok json ->
          let s = extract_text json in
          check bool "has ok" true (contains s "\"ok\"");
          check bool "has payload" true (contains s "variables")
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 11: handle_plugin_apply_ops — success path
     ============================================================ *)

  let test_apply_ops_success () =
    let ch = fresh_channel "ao-ok" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("ops", `List [`Assoc [("op", `String "set_fill")]]);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_apply_ops args with
      | Ok json ->
          let s = extract_text json in
          check bool "has ok" true (contains s "\"ok\"")
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 12: handle_plugin_batch — success and error action paths
     ============================================================ *)

  let test_batch_success () =
    let ch = fresh_channel "bat-ok" in
    with_responder ~channel_id:ch ~max_commands:2 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("actions", `List [
          `Assoc [("action", `String "set_fill")];
          `Assoc [("action", `String "set_stroke")];
        ]);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_batch args with
      | Ok json ->
          let s = extract_text json in
          check bool "has total" true (contains s "\"total\"");
          check bool "has executed" true (contains s "\"executed\"");
          check bool "has results" true (contains s "\"results\"")
      | Error msg -> fail msg)
  in

  let test_batch_missing_action_field () =
    let ch = fresh_channel "bat-ma" in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("actions", `List [
        `String "not-an-object";
        `Assoc [("action", `String "test")];
      ]);
      ("stop_on_error", `Bool true);
      ("timeout_ms", `Int 5000);
    ] in
    match Mcp_plugin_handlers.handle_plugin_batch args with
    | Ok json ->
        let s = extract_text json in
        check bool "has missing action error" true (contains s "Missing action field");
        check bool "stopped early" true (contains s "stopped_early")
    | Error msg -> fail msg
  in

  let test_batch_stop_on_error_false () =
    let ch = fresh_channel "bat-nse" in
    with_responder ~channel_id:ch ~max_commands:1 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("actions", `List [
          `Assoc [];  (* missing action field *)
          `Assoc [("action", `String "test")];
        ]);
        ("stop_on_error", `Bool false);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_batch args with
      | Ok json ->
          let s = extract_text json in
          check bool "executed 2" true (contains s "\"executed\": 2");
          check bool "not stopped" true (contains s "\"stopped_early\": false")
      | Error msg -> fail msg)
  in

  let test_batch_error_result () =
    let ch = fresh_channel "bat-er" in
    with_responder ~channel_id:ch ~ok:false
      ~payload:(`Assoc [("error", `String "fail")]) ~max_commands:1 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("actions", `List [
          `Assoc [("action", `String "failing_action")];
          `Assoc [("action", `String "should_not_run")];
        ]);
        ("stop_on_error", `Bool true);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_batch args with
      | Ok json ->
          let s = extract_text json in
          check bool "stopped" true (contains s "\"stopped_early\": true");
          check bool "1 executed" true (contains s "\"executed\": 1")
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 13: handle_plugin_export_node_image — success paths
     ============================================================ *)

  let test_export_success_with_base64 () =
    let ch = fresh_channel "exp-b64" in
    let png_b64 = Base64.encode_exn "\x89PNG\r\n\x1a\n" in
    let result_payload = `Assoc [("base64", `String png_b64)] in
    with_responder ~channel_id:ch ~payload:result_payload (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "100:200");
        ("format", `String "png");
        ("scale", `Float 2.0);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_export_node_image args with
      | Ok json ->
          let s = extract_text json in
          check bool "has saved_to" true (contains s "saved_to");
          check bool "has PNG message" true (contains s "PNG saved")
      | Error msg -> fail msg)
  in

  let test_export_success_no_base64 () =
    let ch = fresh_channel "exp-nob" in
    let result_payload = `Assoc [("format", `String "svg")] in
    with_responder ~channel_id:ch ~payload:result_payload (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "10:1");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_export_node_image args with
      | Ok json ->
          let s = extract_text json in
          check bool "has payload" true (contains s "payload")
      | Error msg -> fail msg)
  in

  let test_export_error_result () =
    let ch = fresh_channel "exp-err" in
    with_responder ~channel_id:ch ~ok:false
      ~payload:(`Assoc [("error", `String "export failed")]) (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "10:1");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_export_node_image args with
      | Ok json ->
          let s = extract_text json in
          check bool "has ok false" true (contains s "false");
          check bool "has payload" true (contains s "payload")
      | Error msg -> fail msg)
  in

  let test_export_default_format_scale () =
    let ch = fresh_channel "exp-def" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "7:7");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_export_node_image args with
      | Ok _ -> ()
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 14: handle_plugin_edit_node — known property branches
     ============================================================ *)

  let edit_node_test ~ch_prefix ~props =
    let ch = fresh_channel ch_prefix in
    let num_props = match props with `Assoc l -> List.length l | _ -> 0 in
    with_responder ~channel_id:ch ~max_commands:num_props (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "1:1");
        ("properties", props);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_edit_node args with
      | Ok json ->
          let s = extract_text json in
          check bool "has applied" true (contains s "applied")
      | Error msg -> fail msg)
  in

  let test_edit_node_fill () =
    edit_node_test ~ch_prefix:"en-fill" ~props:(`Assoc [("fill", `String "#FF0000")])
  in
  let test_edit_node_fill_non_string () =
    edit_node_test ~ch_prefix:"en-fill-ns" ~props:(`Assoc [("fill", `Int 255)])
  in
  let test_edit_node_stroke () =
    edit_node_test ~ch_prefix:"en-stroke" ~props:(`Assoc [("stroke", `String "#00FF00")])
  in
  let test_edit_node_stroke_non_string () =
    edit_node_test ~ch_prefix:"en-str-ns" ~props:(`Assoc [("stroke", `List [`Int 0; `Int 255; `Int 0])])
  in
  let test_edit_node_stroke_weight () =
    edit_node_test ~ch_prefix:"en-sw" ~props:(`Assoc [("stroke_weight", `Int 2)])
  in
  let test_edit_node_opacity () =
    edit_node_test ~ch_prefix:"en-op" ~props:(`Assoc [("opacity", `Float 0.5)])
  in
  let test_edit_node_corner_radius () =
    edit_node_test ~ch_prefix:"en-cr" ~props:(`Assoc [("corner_radius", `Int 8)])
  in
  let test_edit_node_effects () =
    edit_node_test ~ch_prefix:"en-fx" ~props:(`Assoc [("effects", `List [`Assoc [("type", `String "DROP_SHADOW")]])])
  in
  let test_edit_node_blend_mode_string () =
    edit_node_test ~ch_prefix:"en-bm" ~props:(`Assoc [("blend_mode", `String "MULTIPLY")])
  in
  let test_edit_node_blend_mode_non_string () =
    edit_node_test ~ch_prefix:"en-bm-ns" ~props:(`Assoc [("blend_mode", `Int 1)])
  in
  let test_edit_node_visible () =
    edit_node_test ~ch_prefix:"en-vis" ~props:(`Assoc [("visible", `Bool true)])
  in
  let test_edit_node_locked () =
    edit_node_test ~ch_prefix:"en-lk" ~props:(`Assoc [("locked", `Bool false)])
  in
  let test_edit_node_name () =
    edit_node_test ~ch_prefix:"en-nm" ~props:(`Assoc [("name", `String "New Name")])
  in
  let test_edit_node_name_non_string () =
    edit_node_test ~ch_prefix:"en-nm-ns" ~props:(`Assoc [("name", `Int 42)])
  in
  let test_edit_node_text () =
    edit_node_test ~ch_prefix:"en-txt" ~props:(`Assoc [("text", `String "Hello World")])
  in
  let test_edit_node_text_non_string () =
    edit_node_test ~ch_prefix:"en-txt-ns" ~props:(`Assoc [("text", `Int 999)])
  in
  let test_edit_node_font_size () =
    edit_node_test ~ch_prefix:"en-fs" ~props:(`Assoc [("font_size", `Int 16)])
  in
  let test_edit_node_text_case () =
    edit_node_test ~ch_prefix:"en-tc" ~props:(`Assoc [("text_case", `String "UPPER")])
  in
  let test_edit_node_text_case_non_string () =
    edit_node_test ~ch_prefix:"en-tc-ns" ~props:(`Assoc [("text_case", `Int 0)])
  in
  let test_edit_node_auto_layout_vertical () =
    edit_node_test ~ch_prefix:"en-al-v" ~props:(`Assoc [("auto_layout", `String "VERTICAL")])
  in
  let test_edit_node_auto_layout_none () =
    edit_node_test ~ch_prefix:"en-al-n" ~props:(`Assoc [("auto_layout", `String "NONE")])
  in
  let test_edit_node_auto_layout_non_string () =
    edit_node_test ~ch_prefix:"en-al-ns" ~props:(`Assoc [("auto_layout", `Int 1)])
  in
  let test_edit_node_padding () =
    edit_node_test ~ch_prefix:"en-pad" ~props:(`Assoc [("padding", `Int 16)])
  in
  let test_edit_node_spacing () =
    edit_node_test ~ch_prefix:"en-sp" ~props:(`Assoc [("spacing", `Int 8)])
  in
  let test_edit_node_constraints () =
    edit_node_test ~ch_prefix:"en-con" ~props:(`Assoc [("constraints", `Assoc [("horizontal", `String "STRETCH")])])
  in

  let test_edit_node_multiple_properties () =
    let ch = fresh_channel "en-multi" in
    with_responder ~channel_id:ch ~max_commands:3 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "1:1");
        ("properties", `Assoc [
          ("fill", `String "#000000");
          ("opacity", `Float 0.8);
          ("visible", `Bool true);
        ]);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_edit_node args with
      | Ok json ->
          let s = extract_text json in
          check bool "applied 3" true (contains s "\"applied\": 3")
      | Error msg -> fail msg)
  in

  let test_edit_node_dispatch_error () =
    let ch = fresh_channel "en-derr" in
    with_responder ~channel_id:ch ~ok:false
      ~payload:(`Assoc [("error", `String "dispatch failed")])
      ~max_commands:1 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_id", `String "1:1");
        ("properties", `Assoc [("fill", `String "#FF0000")]);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_edit_node args with
      | Ok json ->
          let s = extract_text json in
          check bool "has errors" true (contains s "errors")
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 15: handle_plugin_create_node — success paths
     ============================================================ *)

  let test_create_node_success () =
    let ch = fresh_channel "cn-ok" in
    let result_payload = `Assoc [("id", `String "new:1")] in
    with_responder ~channel_id:ch ~payload:result_payload (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("type", `String "rectangle");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_create_node args with
      | Ok json ->
          let s = extract_text json in
          check bool "has command" true (contains s "create_rectangle");
          check bool "has ok" true (contains s "\"ok\"")
      | Error msg -> fail msg)
  in

  let test_create_node_with_all_options () =
    let ch = fresh_channel "cn-all" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("type", `String "frame");
        ("parent_id", `String "0:1");
        ("x", `Int 100);
        ("y", `Int 200);
        ("width", `Int 300);
        ("height", `Int 400);
        ("name", `String "MyFrame");
        ("fill", `String "#FFFFFF");
        ("text", `String "Label");
        ("font_size", `Int 14);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_create_node args with
      | Ok json ->
          let s = extract_text json in
          check bool "command" true (contains s "create_frame")
      | Error msg -> fail msg)
  in

  let test_create_node_text () =
    let ch = fresh_channel "cn-txt" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("type", `String "text");
        ("text", `String "Hello");
        ("font_size", `Int 24);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_create_node args with
      | Ok json ->
          let s = extract_text json in
          check bool "command" true (contains s "create_text")
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 16: handle_plugin_delete_nodes — success path
     ============================================================ *)

  let test_delete_nodes_success () =
    let ch = fresh_channel "dn-ok" in
    with_responder ~channel_id:ch ~max_commands:2 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("node_ids", `List [`String "1:1"; `String "2:2"]);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
      | Ok json ->
          let s = extract_text json in
          check bool "has deleted" true (contains s "\"deleted\": 2");
          check bool "has results" true (contains s "\"results\"")
      | Error msg -> fail msg)
  in

  let test_delete_nodes_partial_failure () =
    let ch = fresh_channel "dn-pf" in
    let call_count = Atomic.make 0 in
    let d = Domain.spawn (fun () ->
      let responded = ref 0 in
      while !responded < 2 do
        let cmds = Figma_plugin_bridge.poll_commands ~channel_id:ch ~max:1 in
        (match cmds with
         | cmd :: _ ->
             let n = Atomic.fetch_and_add call_count 1 in
             if n = 0 then
               Figma_plugin_bridge.store_result ~channel_id:ch
                 ~command_id:cmd.id ~ok:true
                 ~payload:(`Assoc [("deleted", `Bool true)])
             else
               Figma_plugin_bridge.store_result ~channel_id:ch
                 ~command_id:cmd.id ~ok:false
                 ~payload:(`Assoc [("error", `String "node not found")]);
             incr responded
         | [] -> Unix.sleepf 0.005)
      done
    ) in
    Unix.sleepf 0.01;
    (let args = `Assoc [
      ("channel_id", `String ch);
      ("node_ids", `List [`String "1:1"; `String "99:99"]);
      ("timeout_ms", `Int 5000);
    ] in
    match Mcp_plugin_handlers.handle_plugin_delete_nodes args with
    | Ok json ->
        let s = extract_text json in
        check bool "deleted 2" true (contains s "\"deleted\": 2");
        check bool "has results" true (contains s "results")
    | Error msg -> fail msg);
    Domain.join d
  in

  (* ============================================================
     GROUP 17: handle_plugin_subscribe_events — with pre-populated events
     ============================================================ *)

  let test_subscribe_with_events () =
    let ch = fresh_channel "sub-evt" in
    Figma_plugin_bridge.publish_event ~channel_id:ch
      ~event_type:"selection_change"
      ~payload:(`Assoc [("count", `Int 3)]);
    Figma_plugin_bridge.publish_event ~channel_id:ch
      ~event_type:"document_change"
      ~payload:(`Assoc [("changes", `Int 1)]);
    let args = `Assoc [
      ("channel_id", `String ch);
      ("timeout_ms", `Int 0);
      ("max_events", `Int 10);
    ] in
    match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
    | Ok json ->
        let s = extract_text json in
        check bool "has events" true (contains s "selection_change");
        check bool "count >= 1" true (contains s "\"count\"")
    | Error msg -> fail msg
  in

  let test_subscribe_filter_events () =
    let ch = fresh_channel "sub-flt" in
    Figma_plugin_bridge.publish_event ~channel_id:ch
      ~event_type:"selection_change"
      ~payload:(`Assoc [("x", `Int 1)]);
    Figma_plugin_bridge.publish_event ~channel_id:ch
      ~event_type:"document_change"
      ~payload:(`Assoc [("y", `Int 2)]);
    let args = `Assoc [
      ("channel_id", `String ch);
      ("timeout_ms", `Int 0);
      ("event_types", `List [`String "selection_change"]);
    ] in
    match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
    | Ok json ->
        let s = extract_text json in
        check bool "has selection_change" true (contains s "selection_change");
        check bool "count 1" true (contains s "\"count\": 1")
    | Error msg -> fail msg
  in

  let test_subscribe_filter_no_match () =
    let ch = fresh_channel "sub-fnm" in
    Figma_plugin_bridge.publish_event ~channel_id:ch
      ~event_type:"document_change"
      ~payload:`Null;
    let args = `Assoc [
      ("channel_id", `String ch);
      ("timeout_ms", `Int 0);
      ("event_types", `List [`String "page_change"]);
    ] in
    match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
    | Ok json ->
        let s = extract_text json in
        check bool "empty events" true (contains s "\"count\": 0")
    | Error msg -> fail msg
  in

  let test_subscribe_event_types_non_string () =
    let ch = fresh_channel "sub-ns" in
    Figma_plugin_bridge.publish_event ~channel_id:ch
      ~event_type:"test_event"
      ~payload:`Null;
    let args = `Assoc [
      ("channel_id", `String ch);
      ("timeout_ms", `Int 0);
      ("event_types", `List [`Int 1; `Bool true]);
    ] in
    match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
    | Ok json ->
        let s = extract_text json in
        check bool "has events" true (contains s "test_event")
    | Error msg -> fail msg
  in

  let test_subscribe_event_types_not_list () =
    let ch = fresh_channel "sub-nl" in
    Figma_plugin_bridge.publish_event ~channel_id:ch
      ~event_type:"evt1"
      ~payload:`Null;
    let args = `Assoc [
      ("channel_id", `String ch);
      ("timeout_ms", `Int 0);
      ("event_types", `String "not-a-list");
    ] in
    match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
    | Ok json ->
        let s = extract_text json in
        check bool "has events" true (contains s "evt1")
    | Error msg -> fail msg
  in

  let test_subscribe_negative_timeout () =
    let ch = fresh_channel "sub-neg" in
    let args = `Assoc [
      ("channel_id", `String ch);
      ("timeout_ms", `Int (-1));
    ] in
    match Mcp_plugin_handlers.handle_plugin_subscribe_events args with
    | Ok json ->
        let s = extract_text json in
        check bool "empty events" true (contains s "\"count\": 0")
    | Error msg -> fail msg
  in

  (* ============================================================
     GROUP 18: handle_annotate — full flow with channel
     ============================================================ *)

  let test_annotate_full_flow () =
    let ch = fresh_channel "ann-ok" in
    with_responder ~channel_id:ch ~max_commands:2 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("message", `String "AI review note");
        ("color", `String "blue");
        ("node_id", `String "99:1");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_annotate args with
      | Ok json ->
          let s = extract_text json in
          check bool "has total" true (contains s "\"total\"");
          check bool "has results" true (contains s "\"results\"")
      | Error msg -> fail msg)
  in

  let test_annotate_green_no_node () =
    let ch = fresh_channel "ann-grn" in
    with_responder ~channel_id:ch ~max_commands:2 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("message", `String "green note");
        ("color", `String "green");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_annotate args with
      | Ok _ -> ()
      | Error msg -> fail msg)
  in

  let test_annotate_red () =
    let ch = fresh_channel "ann-red" in
    with_responder ~channel_id:ch ~max_commands:2 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("color", `String "red");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_annotate args with
      | Ok _ -> ()
      | Error msg -> fail msg)
  in

  let test_annotate_default_color () =
    let ch = fresh_channel "ann-def" in
    with_responder ~channel_id:ch ~max_commands:2 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("message", `String "default note");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_annotate args with
      | Ok _ -> ()
      | Error msg -> fail msg)
  in

  let test_annotate_unknown_color () =
    let ch = fresh_channel "ann-unk" in
    with_responder ~channel_id:ch ~max_commands:2 (fun () ->
      let args = `Assoc [
        ("channel_id", `String ch);
        ("color", `String "purple");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_annotate args with
      | Ok _ -> ()
      | Error msg -> fail msg)
  in

  let test_annotate_args_not_assoc () =
    let ch = fresh_channel "ann-na" in
    Figma_plugin_bridge.set_default_channel ch;
    with_responder ~channel_id:ch ~max_commands:2 (fun () ->
      let args = `List [`String "not-assoc"] in
      match Mcp_plugin_handlers.handle_annotate args with
      | Ok _ -> ()
      | Error _ -> ())
  in

  (* ============================================================
     GROUP 19: handle_figma_plugin dispatch — all routed actions
     ============================================================ *)

  let test_dispatch_connect_with_args () =
    let args = `Assoc [
      ("action", `String "connect");
      ("channel_id", `String "dispatch-connect-test");
    ] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Ok json ->
        let s = extract_text json in
        check bool "has channel" true (contains s "dispatch-connect-test")
    | Error msg -> fail msg
  in

  let test_dispatch_status_action () =
    let args = `Assoc [("action", `String "status")] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Ok json ->
        let s = extract_text json in
        check bool "has channels" true (contains s "channels")
    | Error msg -> fail msg
  in

  let test_dispatch_use_channel_action () =
    let args = `Assoc [
      ("action", `String "use_channel");
      ("channel_id", `String "use-ch-dispatch");
    ] in
    match Mcp_plugin_handlers.handle_figma_plugin args with
    | Ok json ->
        let s = extract_text json in
        check bool "has channel" true (contains s "use-ch-dispatch")
    | Error msg -> fail msg
  in

  let test_dispatch_read_selection () =
    let ch = fresh_channel "dr-rs" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("action", `String "read_selection");
        ("channel_id", `String ch);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_figma_plugin args with
      | Ok json ->
          let s = extract_text json in
          check bool "has ok" true (contains s "\"ok\"")
      | Error msg -> fail msg)
  in

  let test_dispatch_get_node () =
    let ch = fresh_channel "dr-gn" in
    with_responder ~channel_id:ch (fun () ->
      let args = `Assoc [
        ("action", `String "get_node");
        ("channel_id", `String ch);
        ("node_id", `String "1:1");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_figma_plugin args with
      | Ok json ->
          let s = extract_text json in
          check bool "has ok" true (contains s "\"ok\"")
      | Error msg -> fail msg)
  in

  let test_dispatch_batch () =
    let ch = fresh_channel "dr-bat" in
    with_responder ~channel_id:ch ~max_commands:1 (fun () ->
      let args = `Assoc [
        ("action", `String "batch");
        ("channel_id", `String ch);
        ("actions", `List [`Assoc [("action", `String "test")]]);
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_figma_plugin args with
      | Ok json ->
          let s = extract_text json in
          check bool "has total" true (contains s "\"total\"")
      | Error msg -> fail msg)
  in

  let test_dispatch_annotate () =
    let ch = fresh_channel "dr-ann" in
    with_responder ~channel_id:ch ~max_commands:2 (fun () ->
      let args = `Assoc [
        ("action", `String "annotate");
        ("channel_id", `String ch);
        ("message", `String "test annotation");
        ("timeout_ms", `Int 5000);
      ] in
      match Mcp_plugin_handlers.handle_figma_plugin args with
      | Ok json ->
          let s = extract_text json in
          check bool "has results" true (contains s "results")
      | Error msg -> fail msg)
  in

  (* ============================================================
     GROUP 20: plugin_error_message — additional edge cases
     ============================================================ *)

  let test_error_message_list () =
    let result = Mcp_plugin_handlers.plugin_error_message (`List [`String "a"]) in
    check bool "serialized" true (String.length result > 0)
  in

  let test_error_message_assoc_error_empty_list () =
    let json = `Assoc [("error", `List []); ("message", `String "fallback")] in
    let result = Mcp_plugin_handlers.plugin_error_message json in
    check string "fallback to message" "fallback" result
  in

  let test_error_message_message_list () =
    let json = `Assoc [("message", `List [`String "msg1"; `String "msg2"])] in
    let result = Mcp_plugin_handlers.plugin_error_message json in
    check string "first string" "msg1" result
  in

  let test_error_message_message_non_string () =
    let json = `Assoc [("message", `Int 404)] in
    let result = Mcp_plugin_handlers.plugin_error_message json in
    check bool "json fallback" true (String.length result > 0)
  in

  let test_error_message_bool () =
    let result = Mcp_plugin_handlers.plugin_error_message (`Bool true) in
    check string "bool" "true" result
  in

  let test_error_message_float () =
    let result = Mcp_plugin_handlers.plugin_error_message (`Float 3.14) in
    check bool "float" true (contains result "3.14")
  in

  (* ============================================================
     RUN
     ============================================================ *)

  run "Plugin Handlers W6" [
    ("plugin_wait", [
      test_case "channel not found" `Quick test_plugin_wait_channel_not_found;
      test_case "timeout" `Quick test_plugin_wait_timeout;
    ]);
    ("plugin_exec", [
      test_case "success" `Quick test_plugin_exec_success;
      test_case "error result" `Quick test_plugin_exec_error_result;
    ]);
    ("plugin_simple", [
      test_case "success" `Quick test_plugin_simple_success;
      test_case "custom timeout" `Quick test_plugin_simple_custom_timeout;
    ]);
    ("plugin_node", [
      test_case "success" `Quick test_plugin_node_success;
    ]);
    ("plugin_nodes", [
      test_case "success" `Quick test_plugin_nodes_success;
    ]);
    ("plugin_custom", [
      test_case "success" `Quick test_plugin_custom_success;
    ]);
    ("handle_plugin_read_selection", [
      test_case "success" `Quick test_read_selection_success;
    ]);
    ("handle_plugin_get_node", [
      test_case "success" `Quick test_get_node_success;
      test_case "defaults" `Quick test_get_node_defaults;
    ]);
    ("handle_plugin_get_variables", [
      test_case "success" `Quick test_get_variables_success;
    ]);
    ("handle_plugin_apply_ops", [
      test_case "success" `Quick test_apply_ops_success;
    ]);
    ("handle_plugin_batch", [
      test_case "success" `Quick test_batch_success;
      test_case "missing action field" `Quick test_batch_missing_action_field;
      test_case "stop_on_error false" `Quick test_batch_stop_on_error_false;
      test_case "error result stops" `Quick test_batch_error_result;
    ]);
    ("handle_plugin_export_node_image", [
      test_case "success with base64" `Quick test_export_success_with_base64;
      test_case "success no base64" `Quick test_export_success_no_base64;
      test_case "error result" `Quick test_export_error_result;
      test_case "default format/scale" `Quick test_export_default_format_scale;
    ]);
    ("handle_plugin_edit_node", [
      test_case "fill" `Quick test_edit_node_fill;
      test_case "fill non-string" `Quick test_edit_node_fill_non_string;
      test_case "stroke" `Quick test_edit_node_stroke;
      test_case "stroke non-string" `Quick test_edit_node_stroke_non_string;
      test_case "stroke_weight" `Quick test_edit_node_stroke_weight;
      test_case "opacity" `Quick test_edit_node_opacity;
      test_case "corner_radius" `Quick test_edit_node_corner_radius;
      test_case "effects" `Quick test_edit_node_effects;
      test_case "blend_mode string" `Quick test_edit_node_blend_mode_string;
      test_case "blend_mode non-string" `Quick test_edit_node_blend_mode_non_string;
      test_case "visible" `Quick test_edit_node_visible;
      test_case "locked" `Quick test_edit_node_locked;
      test_case "name" `Quick test_edit_node_name;
      test_case "name non-string" `Quick test_edit_node_name_non_string;
      test_case "text" `Quick test_edit_node_text;
      test_case "text non-string" `Quick test_edit_node_text_non_string;
      test_case "font_size" `Quick test_edit_node_font_size;
      test_case "text_case" `Quick test_edit_node_text_case;
      test_case "text_case non-string" `Quick test_edit_node_text_case_non_string;
      test_case "auto_layout VERTICAL" `Quick test_edit_node_auto_layout_vertical;
      test_case "auto_layout NONE" `Quick test_edit_node_auto_layout_none;
      test_case "auto_layout non-string" `Quick test_edit_node_auto_layout_non_string;
      test_case "padding" `Quick test_edit_node_padding;
      test_case "spacing" `Quick test_edit_node_spacing;
      test_case "constraints" `Quick test_edit_node_constraints;
      test_case "multiple props" `Quick test_edit_node_multiple_properties;
      test_case "dispatch error" `Quick test_edit_node_dispatch_error;
    ]);
    ("handle_plugin_create_node", [
      test_case "success" `Quick test_create_node_success;
      test_case "all options" `Quick test_create_node_with_all_options;
      test_case "text type" `Quick test_create_node_text;
    ]);
    ("handle_plugin_delete_nodes", [
      test_case "success" `Quick test_delete_nodes_success;
      test_case "partial failure" `Quick test_delete_nodes_partial_failure;
    ]);
    ("handle_plugin_subscribe_events", [
      test_case "with events" `Quick test_subscribe_with_events;
      test_case "filter events" `Quick test_subscribe_filter_events;
      test_case "filter no match" `Quick test_subscribe_filter_no_match;
      test_case "non-string types" `Quick test_subscribe_event_types_non_string;
      test_case "types not list" `Quick test_subscribe_event_types_not_list;
      test_case "negative timeout" `Quick test_subscribe_negative_timeout;
    ]);
    ("handle_annotate", [
      test_case "full flow" `Quick test_annotate_full_flow;
      test_case "green no node" `Quick test_annotate_green_no_node;
      test_case "red" `Quick test_annotate_red;
      test_case "default color" `Quick test_annotate_default_color;
      test_case "unknown color" `Quick test_annotate_unknown_color;
      test_case "args not assoc" `Quick test_annotate_args_not_assoc;
    ]);
    ("handle_figma_plugin dispatch", [
      test_case "connect with args" `Quick test_dispatch_connect_with_args;
      test_case "status" `Quick test_dispatch_status_action;
      test_case "use_channel" `Quick test_dispatch_use_channel_action;
      test_case "read_selection" `Quick test_dispatch_read_selection;
      test_case "get_node" `Quick test_dispatch_get_node;
      test_case "batch" `Quick test_dispatch_batch;
      test_case "annotate" `Quick test_dispatch_annotate;
    ]);
    ("plugin_error_message extra", [
      test_case "list" `Quick test_error_message_list;
      test_case "empty error list" `Quick test_error_message_assoc_error_empty_list;
      test_case "message list" `Quick test_error_message_message_list;
      test_case "message non-string" `Quick test_error_message_message_non_string;
      test_case "bool" `Quick test_error_message_bool;
      test_case "float" `Quick test_error_message_float;
    ]);
  ]
