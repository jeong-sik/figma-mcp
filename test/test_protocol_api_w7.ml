(** Coverage Wave 7: mcp_protocol.ml (52 uncov) + figma_api.ml (21 uncov) — pure functions *)

open Figma_mcp_protocol
open Figma_api

let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)

(* ========== mcp_protocol.ml — parse_request ========== *)

let test_parse_request_valid () =
  let json = {|{"jsonrpc":"2.0","id":1,"method":"tools/list","params":null}|} in
  match parse_request json with
  | Ok req ->
      check_string "method" "tools/list" req.method_;
      check_string "jsonrpc" "2.0" req.jsonrpc
  | Error msg -> Alcotest.fail ("parse failed: " ^ msg)

let test_parse_request_no_params () =
  let json = {|{"jsonrpc":"2.0","id":"abc","method":"initialize"}|} in
  match parse_request json with
  | Ok req ->
      check_string "method" "initialize" req.method_;
      Alcotest.(check bool) "params is none" true (req.params = None)
  | Error msg -> Alcotest.fail ("parse failed: " ^ msg)

let test_parse_request_bad_jsonrpc () =
  let json = {|{"jsonrpc":"1.0","id":1,"method":"test"}|} in
  match parse_request json with
  | Error msg -> Alcotest.(check bool) "has version error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should reject non-2.0 version"

let test_parse_request_empty_method () =
  let json = {|{"jsonrpc":"2.0","id":1,"method":""}|} in
  match parse_request json with
  | Error msg -> Alcotest.(check bool) "has method error" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should reject empty method"

let test_parse_request_invalid_json () =
  match parse_request "not json at all" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on invalid JSON"

let test_parse_request_missing_method () =
  let json = {|{"jsonrpc":"2.0","id":1}|} in
  match parse_request json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail when method missing"

let test_parse_request_null_id () =
  let json = {|{"jsonrpc":"2.0","id":null,"method":"ping"}|} in
  match parse_request json with
  | Ok req ->
      check_string "method" "ping" req.method_;
      Alcotest.(check bool) "id is Some `Null" true (req.id = Some `Null)
  | Error msg -> Alcotest.fail ("parse failed: " ^ msg)

let test_parse_request_string_id () =
  let json = {|{"jsonrpc":"2.0","id":"req-42","method":"test"}|} in
  match parse_request json with
  | Ok req ->
      (match req.id with
       | Some (`String s) -> check_string "string id" "req-42" s
       | _ -> Alcotest.fail "expected string id")
  | Error msg -> Alcotest.fail ("parse failed: " ^ msg)

let test_parse_request_with_params () =
  let json = {|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"test","arguments":{}}}|} in
  match parse_request json with
  | Ok req ->
      check_string "method" "tools/call" req.method_;
      Alcotest.(check bool) "has params" true (req.params <> None)
  | Error msg -> Alcotest.fail ("parse failed: " ^ msg)

(* ========== mcp_protocol.ml — notification helpers ========== *)

let test_is_notification_id_none () =
  check_bool "None is notification" true (is_notification_id None)

let test_is_notification_id_null () =
  check_bool "Null is notification" true (is_notification_id (Some `Null))

let test_is_notification_id_int () =
  check_bool "Int is not notification" false (is_notification_id (Some (`Int 1)))

let test_is_notification_id_string () =
  check_bool "String is not notification" false (is_notification_id (Some (`String "abc")))

let test_is_notification_true () =
  let req = { jsonrpc = "2.0"; id = None; method_ = "initialized"; params = None } in
  check_bool "no id = notification" true (is_notification req)

let test_is_notification_false () =
  let req = { jsonrpc = "2.0"; id = Some (`Int 1); method_ = "tools/list"; params = None } in
  check_bool "int id = not notification" false (is_notification req)

(* ========== mcp_protocol.ml — response builders ========== *)

let test_make_success_response () =
  let resp = make_success_response (`Int 1) (`String "ok") in
  match resp with
  | `Assoc lst ->
      let jsonrpc = List.assoc "jsonrpc" lst in
      let id = List.assoc "id" lst in
      let result = List.assoc "result" lst in
      Alcotest.(check string) "jsonrpc" "2.0" (match jsonrpc with `String s -> s | _ -> "");
      Alcotest.(check bool) "id=1" true (id = `Int 1);
      Alcotest.(check bool) "result=ok" true (result = `String "ok")
  | _ -> Alcotest.fail "expected assoc"

let test_make_error_response_no_data () =
  let resp = make_error_response (`Int 1) (-32600) "bad request" None in
  match resp with
  | `Assoc lst ->
      let err = List.assoc "error" lst in
      (match err with
       | `Assoc elst ->
           let code = List.assoc "code" elst in
           let msg = List.assoc "message" elst in
           Alcotest.(check bool) "code" true (code = `Int (-32600));
           Alcotest.(check bool) "message" true (msg = `String "bad request");
           Alcotest.(check bool) "no data" true (not (List.mem_assoc "data" elst))
       | _ -> Alcotest.fail "expected error assoc")
  | _ -> Alcotest.fail "expected assoc"

let test_make_error_response_with_data () =
  let resp = make_error_response (`String "x") (-32603) "internal" (Some (`String "details")) in
  match resp with
  | `Assoc lst ->
      let err = List.assoc "error" lst in
      (match err with
       | `Assoc elst ->
           let data = List.assoc_opt "data" elst in
           Alcotest.(check bool) "has data" true (data = Some (`String "details"))
       | _ -> Alcotest.fail "expected error assoc")
  | _ -> Alcotest.fail "expected assoc"

(* ========== mcp_protocol.ml — protocol version ========== *)

let test_normalize_known_version () =
  check_string "known" "2024-11-05" (normalize_protocol_version "2024-11-05")

let test_normalize_latest_version () =
  check_string "latest" "2025-11-25" (normalize_protocol_version "2025-11-25")

let test_normalize_unknown_version () =
  check_string "unknown fallback" "2025-11-25" (normalize_protocol_version "9999-01-01")

let test_protocol_version_from_params_some () =
  let params = Some (`Assoc [("protocolVersion", `String "2024-11-05")]) in
  check_string "from params" "2024-11-05" (protocol_version_from_params params)

let test_protocol_version_from_params_missing () =
  let params = Some (`Assoc [("other", `String "x")]) in
  check_string "missing" "2025-11-25" (protocol_version_from_params params)

let test_protocol_version_from_params_none () =
  check_string "none" "2025-11-25" (protocol_version_from_params None)

let test_protocol_version_from_params_non_assoc () =
  let params = Some (`List [`Int 1]) in
  check_string "list" "2025-11-25" (protocol_version_from_params params)

(* ========== mcp_protocol.ml — JSON serializers ========== *)

let test_tool_to_json_basic () =
  let tool = { name = "test_tool"; description = "A test tool"; input_schema = `Assoc [] } in
  let json = tool_to_json tool in
  match json with
  | `Assoc lst ->
      let name = List.assoc "name" lst in
      Alcotest.(check bool) "name" true (name = `String "test_tool")
  | _ -> Alcotest.fail "expected assoc"

let test_tool_to_json_deprecated () =
  let tool = { name = "old_tool"; description = "[DEPRECATED] Use new_tool instead"; input_schema = `Assoc [] } in
  let json = tool_to_json tool in
  match json with
  | `Assoc lst ->
      (* deprecated tools should have deprecated annotation *)
      let desc = List.assoc "description" lst in
      (match desc with
       | `String s -> Alcotest.(check bool) "has deprecated" true (String.length s > 0)
       | _ -> Alcotest.fail "expected string description")
  | _ -> Alcotest.fail "expected assoc"

let test_tool_to_json_short_description () =
  let tool = { name = "t"; description = "Short"; input_schema = `Assoc [] } in
  let json = tool_to_json tool in
  match json with
  | `Assoc lst ->
      let desc = List.assoc "description" lst in
      Alcotest.(check bool) "desc" true (desc = `String "Short")
  | _ -> Alcotest.fail "expected assoc"

let test_resource_to_json () =
  let r = { uri = "figma://test"; name = "Test"; description = "A resource"; mime_type = "text/plain" } in
  let json = resource_to_json r in
  match json with
  | `Assoc lst ->
      Alcotest.(check bool) "uri" true (List.assoc "uri" lst = `String "figma://test");
      Alcotest.(check bool) "name" true (List.assoc "name" lst = `String "Test")
  | _ -> Alcotest.fail "expected assoc"

let test_resource_template_to_json () =
  let t = { uri_template = "figma://tokens/{file_key}"; name = "Tokens"; description = "Design tokens"; mime_type = "application/json" } in
  let json = resource_template_to_json t in
  match json with
  | `Assoc lst ->
      Alcotest.(check bool) "uriTemplate" true (List.mem_assoc "uriTemplate" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_prompt_to_json () =
  let p = { name = "review"; description = "Code review"; arguments = [
    { name = "code"; description = "The code"; required = true }
  ]; text = "Review this code" } in
  let json = prompt_to_json p in
  match json with
  | `Assoc lst ->
      Alcotest.(check bool) "name" true (List.assoc "name" lst = `String "review")
  | _ -> Alcotest.fail "expected assoc"

let test_prompt_to_detail_json () =
  let p = { name = "review"; description = "Code review"; arguments = [
    { name = "code"; description = "The code"; required = true };
    { name = "lang"; description = "Language"; required = false };
  ]; text = "Review this" } in
  let json = prompt_to_detail_json p in
  match json with
  | `Assoc lst ->
      Alcotest.(check bool) "has description" true (List.mem_assoc "description" lst);
      Alcotest.(check bool) "has arguments" true (List.mem_assoc "arguments" lst)
  | _ -> Alcotest.fail "expected assoc"

(* ========== mcp_protocol.ml — member helper ========== *)

let test_proto_member_found () =
  let json = `Assoc [("key", `String "value")] in
  Alcotest.(check bool) "found" true (Figma_mcp_protocol.member "key" json = Some (`String "value"))

let test_proto_member_not_found () =
  let json = `Assoc [("key", `String "value")] in
  Alcotest.(check bool) "not found" true (Figma_mcp_protocol.member "other" json = None)

let test_proto_member_non_assoc () =
  Alcotest.(check bool) "non-assoc" true (Figma_mcp_protocol.member "x" (`String "s") = None)

(* ========== mcp_protocol.ml — handle_initialize ========== *)

let test_handle_initialize_known_version () =
  let params = Some (`Assoc [("protocolVersion", `String "2024-11-05")]) in
  let result = handle_initialize params in
  match result with
  | `Assoc lst ->
      let pv = List.assoc "protocolVersion" lst in
      Alcotest.(check bool) "negotiated" true (pv = `String "2024-11-05");
      Alcotest.(check bool) "has capabilities" true (List.mem_assoc "capabilities" lst);
      Alcotest.(check bool) "has serverInfo" true (List.mem_assoc "serverInfo" lst);
      Alcotest.(check bool) "has instructions" true (List.mem_assoc "instructions" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_handle_initialize_unknown_version () =
  let params = Some (`Assoc [("protocolVersion", `String "1999-01-01")]) in
  let result = handle_initialize params in
  match result with
  | `Assoc lst ->
      let pv = List.assoc "protocolVersion" lst in
      Alcotest.(check bool) "fallback" true (pv = `String "2025-11-25")
  | _ -> Alcotest.fail "expected assoc"

let test_handle_initialize_no_version () =
  let result = handle_initialize None in
  match result with
  | `Assoc lst ->
      let pv = List.assoc "protocolVersion" lst in
      Alcotest.(check bool) "default" true (pv = `String "2025-11-25")
  | _ -> Alcotest.fail "expected assoc"

(* ========== mcp_protocol.ml — handle_tools_list / resources_list ========== *)

let sample_server =
  let tools = [
    { name = "tool_a"; description = "Tool A"; input_schema = `Assoc [] };
    { name = "tool_b"; description = "Tool B"; input_schema = `Assoc [("type", `String "object")] };
  ] in
  let resources = [
    { uri = "res://1"; name = "R1"; description = "Res 1"; mime_type = "text/plain" };
  ] in
  let prompts = [
    { name = "p1"; description = "Prompt 1"; arguments = []; text = "Hello" };
  ] in
  let resource_templates = [
    { uri_template = "res://{id}"; name = "RT1"; description = "Template"; mime_type = "text/plain" };
  ] in
  let read_resource uri =
    if uri = "res://1" then Ok ("text/plain", "content here")
    else Error ("Not found: " ^ uri)
  in
  let handlers_sync = [
    ("tool_a", fun _args -> Ok (`Assoc [("result", `String "ok")]));
    ("tool_b", fun _args -> Error "tool_b error");
  ] in
  create_server ~handlers_sync ~resource_templates tools resources prompts read_resource

let test_handle_tools_list () =
  let result = handle_tools_list sample_server None in
  match result with
  | `Assoc lst ->
      (match List.assoc "tools" lst with
       | `List tools -> check_int "2 tools" 2 (List.length tools)
       | _ -> Alcotest.fail "expected tools list")
  | _ -> Alcotest.fail "expected assoc"

let test_handle_resources_list () =
  let result = handle_resources_list sample_server None in
  match result with
  | `Assoc lst ->
      (match List.assoc "resources" lst with
       | `List resources -> check_int "1 resource" 1 (List.length resources)
       | _ -> Alcotest.fail "expected resources list")
  | _ -> Alcotest.fail "expected assoc"

let test_handle_resource_templates_list () =
  let result = handle_resource_templates_list sample_server None in
  match result with
  | `Assoc lst ->
      (match List.assoc "resourceTemplates" lst with
       | `List ts -> check_int "1 template" 1 (List.length ts)
       | _ -> Alcotest.fail "expected templates list")
  | _ -> Alcotest.fail "expected assoc"

let test_handle_prompts_list () =
  let result = handle_prompts_list sample_server None in
  match result with
  | `Assoc lst ->
      (match List.assoc "prompts" lst with
       | `List ps -> check_int "1 prompt" 1 (List.length ps)
       | _ -> Alcotest.fail "expected prompts list")
  | _ -> Alcotest.fail "expected assoc"

(* ========== mcp_protocol.ml — handle_prompts_get ========== *)

let test_handle_prompts_get_found () =
  let params = Some (`Assoc [("name", `String "p1")]) in
  match handle_prompts_get sample_server params with
  | Ok json ->
      (match json with
       | `Assoc lst -> Alcotest.(check bool) "has prompt" true (List.mem_assoc "prompt" lst)
       | _ -> Alcotest.fail "expected assoc")
  | Error _ -> Alcotest.fail "should find prompt p1"

let test_handle_prompts_get_not_found () =
  let params = Some (`Assoc [("name", `String "nonexistent")]) in
  match handle_prompts_get sample_server params with
  | Error (code, msg) ->
      check_int "invalid_params code" invalid_params code;
      Alcotest.(check bool) "has name in msg" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should not find nonexistent"

let test_handle_prompts_get_missing_name () =
  let params = Some (`Assoc [("other", `String "x")]) in
  match handle_prompts_get sample_server params with
  | Error (code, _msg) -> check_int "invalid_params" invalid_params code
  | Ok _ -> Alcotest.fail "should fail missing name"

let test_handle_prompts_get_invalid_params () =
  match handle_prompts_get sample_server (Some (`List [])) with
  | Error (code, _) -> check_int "invalid_params" invalid_params code
  | Ok _ -> Alcotest.fail "should fail on list params"

let test_handle_prompts_get_none_params () =
  match handle_prompts_get sample_server None with
  | Error (code, _) -> check_int "invalid_params" invalid_params code
  | Ok _ -> Alcotest.fail "should fail on None params"

(* ========== mcp_protocol.ml — handle_resources_read ========== *)

let test_handle_resources_read_found () =
  let params = Some (`Assoc [("uri", `String "res://1")]) in
  match handle_resources_read sample_server params with
  | Ok json ->
      (match json with
       | `Assoc lst -> Alcotest.(check bool) "has contents" true (List.mem_assoc "contents" lst)
       | _ -> Alcotest.fail "expected assoc")
  | Error _ -> Alcotest.fail "should read res://1"

let test_handle_resources_read_not_found () =
  let params = Some (`Assoc [("uri", `String "res://999")]) in
  match handle_resources_read sample_server params with
  | Error (code, _) -> check_int "internal_error" internal_error code
  | Ok _ -> Alcotest.fail "should fail for unknown uri"

let test_handle_resources_read_missing_uri () =
  let params = Some (`Assoc [("other", `String "x")]) in
  match handle_resources_read sample_server params with
  | Error (code, _) -> check_int "invalid_params" invalid_params code
  | Ok _ -> Alcotest.fail "should fail missing uri"

let test_handle_resources_read_invalid_params () =
  match handle_resources_read sample_server (Some (`String "bad")) with
  | Error (code, _) -> check_int "invalid_params" invalid_params code
  | Ok _ -> Alcotest.fail "should fail on string params"

let test_handle_resources_read_none_params () =
  match handle_resources_read sample_server None with
  | Error (code, _) -> check_int "invalid_params" invalid_params code
  | Ok _ -> Alcotest.fail "should fail on None"

(* ========== mcp_protocol.ml — handle_tools_call_sync ========== *)

let test_handle_tools_call_success () =
  let params = Some (`Assoc [("name", `String "tool_a"); ("arguments", `Assoc [])]) in
  match handle_tools_call_sync sample_server params with
  | Ok json ->
      (match json with
       | `Assoc lst -> Alcotest.(check bool) "has result" true (List.assoc "result" lst = `String "ok")
       | _ -> Alcotest.fail "expected assoc result")
  | Error _ -> Alcotest.fail "tool_a should succeed"

let test_handle_tools_call_handler_error () =
  let params = Some (`Assoc [("name", `String "tool_b"); ("arguments", `Assoc [])]) in
  match handle_tools_call_sync sample_server params with
  | Error (code, msg) ->
      check_int "internal_error" internal_error code;
      check_string "error msg" "tool_b error" msg
  | Ok _ -> Alcotest.fail "tool_b should error"

let test_handle_tools_call_not_found () =
  let params = Some (`Assoc [("name", `String "nonexistent"); ("arguments", `Assoc [])]) in
  match handle_tools_call_sync sample_server params with
  | Error (code, msg) ->
      check_int "method_not_found" method_not_found code;
      Alcotest.(check bool) "msg has tool name" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "should fail for unknown tool"

let test_handle_tools_call_missing_name () =
  let params = Some (`Assoc [("arguments", `Assoc [])]) in
  match handle_tools_call_sync sample_server params with
  | Error (code, _) -> check_int "invalid_params" invalid_params code
  | Ok _ -> Alcotest.fail "should fail missing name"

let test_handle_tools_call_no_arguments () =
  (* arguments missing → defaults to empty assoc *)
  let params = Some (`Assoc [("name", `String "tool_a")]) in
  match handle_tools_call_sync sample_server params with
  | Ok _ -> ()
  | Error (_, msg) -> Alcotest.fail ("should succeed: " ^ msg)

let test_handle_tools_call_invalid_params () =
  match handle_tools_call_sync sample_server (Some (`List [])) with
  | Error (code, _) -> check_int "invalid_params" invalid_params code
  | Ok _ -> Alcotest.fail "should fail on list params"

let test_handle_tools_call_none_params () =
  match handle_tools_call_sync sample_server None with
  | Error (code, _) -> check_int "invalid_params" invalid_params code
  | Ok _ -> Alcotest.fail "should fail on None"

(* ========== mcp_protocol.ml — process_request_sync ========== *)

let make_req method_ params =
  { jsonrpc = "2.0"; id = Some (`Int 1); method_; params }

let test_process_initialize () =
  let req = make_req "initialize" (Some (`Assoc [("protocolVersion", `String "2025-03-26")])) in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst ->
      Alcotest.(check bool) "has result" true (List.mem_assoc "result" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_initialized () =
  let req = make_req "initialized" None in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has result" true (List.mem_assoc "result" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_notifications_initialized () =
  let req = make_req "notifications/initialized" None in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has result" true (List.mem_assoc "result" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_tools_list () =
  let req = make_req "tools/list" None in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst ->
      let result = List.assoc "result" lst in
      (match result with
       | `Assoc rlst -> Alcotest.(check bool) "has tools" true (List.mem_assoc "tools" rlst)
       | _ -> Alcotest.fail "expected result assoc")
  | _ -> Alcotest.fail "expected assoc"

let test_process_tools_call_ok () =
  let params = Some (`Assoc [("name", `String "tool_a"); ("arguments", `Assoc [])]) in
  let req = make_req "tools/call" params in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has result" true (List.mem_assoc "result" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_tools_call_error () =
  let params = Some (`Assoc [("name", `String "tool_b")]) in
  let req = make_req "tools/call" params in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has error" true (List.mem_assoc "error" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_resources_list () =
  let req = make_req "resources/list" None in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has result" true (List.mem_assoc "result" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_resource_templates_list () =
  let req = make_req "resources/templates/list" None in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has result" true (List.mem_assoc "result" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_resources_read_ok () =
  let params = Some (`Assoc [("uri", `String "res://1")]) in
  let req = make_req "resources/read" params in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has result" true (List.mem_assoc "result" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_resources_read_error () =
  let params = Some (`Assoc [("uri", `String "res://999")]) in
  let req = make_req "resources/read" params in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has error" true (List.mem_assoc "error" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_prompts_list () =
  let req = make_req "prompts/list" None in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has result" true (List.mem_assoc "result" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_prompts_get_ok () =
  let params = Some (`Assoc [("name", `String "p1")]) in
  let req = make_req "prompts/get" params in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has result" true (List.mem_assoc "result" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_prompts_get_error () =
  let params = Some (`Assoc [("name", `String "nope")]) in
  let req = make_req "prompts/get" params in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has error" true (List.mem_assoc "error" lst)
  | _ -> Alcotest.fail "expected assoc"

let test_process_unknown_method () =
  let req = make_req "nonexistent/method" None in
  let resp = process_request_sync sample_server req in
  match resp with
  | `Assoc lst -> Alcotest.(check bool) "has error" true (List.mem_assoc "error" lst)
  | _ -> Alcotest.fail "expected assoc"

(* ========== mcp_protocol.ml — create_server ========== *)

let test_create_server_defaults () =
  let s = create_server [] [] [] (fun _ -> Error "no") in
  check_int "no tools" 0 (List.length s.tools);
  check_int "no handlers" 0 (List.length s.handlers_sync);
  check_int "no templates" 0 (List.length s.resource_templates)

let test_create_server_with_templates () =
  let tmpl = [{ uri_template = "t://{x}"; name = "T"; description = "D"; mime_type = "text/plain" }] in
  let s = create_server ~resource_templates:tmpl [] [] [] (fun _ -> Error "no") in
  check_int "1 template" 1 (List.length s.resource_templates)

(* ========== figma_api.ml — node ID conversion ========== *)

let test_url_to_api_node_id () =
  check_string "convert" "2089:11127" (url_to_api_node_id "2089-11127")

let test_url_to_api_node_id_multiple () =
  check_string "multi-dash" "1:2:3" (url_to_api_node_id "1-2-3")

let test_api_to_url_node_id () =
  check_string "convert" "2089-11127" (api_to_url_node_id "2089:11127")

let test_normalize_node_id_dash () =
  check_string "dash to colon" "10:20" (normalize_node_id "10-20")

let test_normalize_node_id_colon () =
  check_string "already colon" "10:20" (normalize_node_id "10:20")

let test_normalize_node_id_both () =
  (* has both dash and colon — keep as-is since colon present *)
  check_string "mixed" "10:20-30" (normalize_node_id "10:20-30")

let test_normalize_node_id_no_separator () =
  check_string "plain" "12345" (normalize_node_id "12345")

let test_normalize_node_ids () =
  let result = normalize_node_ids ["1-2"; "3:4"; "5-6-7"] in
  Alcotest.(check (list string)) "batch" ["1:2"; "3:4"; "5:6:7"] result

(* ========== figma_api.ml — error_to_string ========== *)

let test_error_network () =
  let s = error_to_string (NetworkError "timeout") in
  Alcotest.(check bool) "has network" true (String.length s > 0)

let test_error_auth () =
  let s = error_to_string (AuthError "invalid token") in
  Alcotest.(check bool) "has auth" true (String.length s > 0)

let test_error_not_found () =
  let s = error_to_string (NotFound "file abc") in
  Alcotest.(check bool) "has not found" true (String.length s > 0)

let test_error_rate_limited () =
  let s = error_to_string RateLimited in
  Alcotest.(check bool) "has rate" true (String.length s > 0)

let test_error_parse () =
  let s = error_to_string (ParseError "unexpected token") in
  Alcotest.(check bool) "has parse" true (String.length s > 0)

let test_error_unknown () =
  let s = error_to_string (UnknownError (500, "server error")) in
  Alcotest.(check bool) "has 500" true (String.length s > 0)

(* ========== figma_api.ml — JSON helpers ========== *)

let test_member_assoc_found () =
  let json = `Assoc [("key", `String "val")] in
  Alcotest.(check bool) "found" true (Figma_api.member "key" json = Some (`String "val"))

let test_member_assoc_missing () =
  let json = `Assoc [("key", `String "val")] in
  Alcotest.(check bool) "missing" true (Figma_api.member "other" json = None)

let test_member_non_assoc () =
  Alcotest.(check bool) "list" true (Figma_api.member "x" (`List []) = None)

let test_extract_document_present () =
  let json = `Assoc [("document", `Assoc [("id", `String "doc")])] in
  Alcotest.(check bool) "found doc" true (extract_document json <> None)

let test_extract_document_missing () =
  let json = `Assoc [("other", `Null)] in
  Alcotest.(check bool) "no doc" true (extract_document json = None)

let test_extract_pages_normal () =
  let json = `Assoc [("document", `Assoc [("children", `List [
    `Assoc [("name", `String "Page 1")];
    `Assoc [("name", `String "Page 2")];
  ])])] in
  let pages = extract_pages json in
  check_int "2 pages" 2 (List.length pages)

let test_extract_pages_no_children () =
  let json = `Assoc [("document", `Assoc [("type", `String "DOCUMENT")])] in
  let pages = extract_pages json in
  check_int "0 pages" 0 (List.length pages)

let test_extract_pages_no_document () =
  let json = `Assoc [] in
  let pages = extract_pages json in
  check_int "0 pages" 0 (List.length pages)

let test_get_page_names () =
  let json = `Assoc [("document", `Assoc [("children", `List [
    `Assoc [("name", `String "Home")];
    `Assoc [("name", `String "Settings")];
    `Assoc [("type", `String "no-name")]; (* no name field *)
  ])])] in
  let names = get_page_names json in
  Alcotest.(check (list string)) "page names" ["Home"; "Settings"] names

let test_get_frames_from_page_mixed () =
  let page = `Assoc [("children", `List [
    `Assoc [("id", `String "1:1"); ("name", `String "Frame1"); ("type", `String "FRAME")];
    `Assoc [("id", `String "1:2"); ("name", `String "Comp1"); ("type", `String "COMPONENT")];
    `Assoc [("id", `String "1:3"); ("name", `String "Group1"); ("type", `String "GROUP")]; (* filtered out *)
    `Assoc [("name", `String "NoId"); ("type", `String "FRAME")]; (* no id *)
  ])] in
  let frames = get_frames_from_page page in
  check_int "2 frames" 2 (List.length frames);
  let ids = List.map fst frames in
  Alcotest.(check (list string)) "ids" ["1:1"; "1:2"] ids

let test_get_frames_from_page_no_children () =
  let page = `Assoc [("type", `String "CANVAS")] in
  let frames = get_frames_from_page page in
  check_int "0 frames" 0 (List.length frames)

let test_get_all_screens () =
  let json = `Assoc [("document", `Assoc [("children", `List [
    `Assoc [("name", `String "Page1"); ("children", `List [
      `Assoc [("id", `String "1:1"); ("name", `String "F1"); ("type", `String "FRAME")];
    ])];
    `Assoc [("name", `String "Page2"); ("children", `List [
      `Assoc [("id", `String "2:1"); ("name", `String "F2"); ("type", `String "COMPONENT")];
    ])];
  ])])] in
  let screens = get_all_screens json in
  check_int "2 screens" 2 (List.length screens)

(* ========== figma_api.ml — parse_figma_url ========== *)

let test_parse_url_design () =
  let info = parse_figma_url "https://www.figma.com/design/ABC123/My-File?node-id=10-20" in
  check_string "file_key" "ABC123" (Option.get info.file_key);
  check_string "node_id" "10:20" (Option.get info.node_id)

let test_parse_url_file () =
  let info = parse_figma_url "https://www.figma.com/file/XYZ789/My-File" in
  check_string "file_key" "XYZ789" (Option.get info.file_key);
  Alcotest.(check bool) "no node_id" true (info.node_id = None)

let test_parse_url_proto () =
  let info = parse_figma_url "https://www.figma.com/proto/DEF456/Prototype?node-id=5-6" in
  check_string "file_key" "DEF456" (Option.get info.file_key);
  check_string "node_id" "5:6" (Option.get info.node_id)

let test_parse_url_team () =
  let info = parse_figma_url "https://www.figma.com/files/team/12345" in
  check_string "team_id" "12345" (Option.get info.team_id);
  Alcotest.(check bool) "no file_key" true (info.file_key = None)

let test_parse_url_team_project () =
  let info = parse_figma_url "https://www.figma.com/files/team/12345/project/67890" in
  check_string "team_id" "12345" (Option.get info.team_id);
  check_string "project_id" "67890" (Option.get info.project_id)

let test_parse_url_unrecognized () =
  let info = parse_figma_url "https://www.figma.com/community/plugin/xxx" in
  Alcotest.(check bool) "no file_key" true (info.file_key = None);
  Alcotest.(check bool) "no team_id" true (info.team_id = None)

let test_parse_url_invalid () =
  let info = parse_figma_url "not a url at all" in
  Alcotest.(check bool) "no file_key" true (info.file_key = None)

let test_parse_url_empty () =
  let info = parse_figma_url "" in
  Alcotest.(check bool) "no file_key" true (info.file_key = None)

let test_parse_url_node_id_colon_format () =
  let info = parse_figma_url "https://www.figma.com/design/ABC/File?node-id=10:20" in
  check_string "node_id stays colon" "10:20" (Option.get info.node_id)

(* ========== Test runner ========== *)

let () =
  Alcotest.run "protocol_api_w7" [
    ("parse_request", [
      Alcotest.test_case "valid request" `Quick test_parse_request_valid;
      Alcotest.test_case "no params" `Quick test_parse_request_no_params;
      Alcotest.test_case "bad jsonrpc version" `Quick test_parse_request_bad_jsonrpc;
      Alcotest.test_case "empty method" `Quick test_parse_request_empty_method;
      Alcotest.test_case "invalid json" `Quick test_parse_request_invalid_json;
      Alcotest.test_case "missing method" `Quick test_parse_request_missing_method;
      Alcotest.test_case "null id" `Quick test_parse_request_null_id;
      Alcotest.test_case "string id" `Quick test_parse_request_string_id;
      Alcotest.test_case "with params" `Quick test_parse_request_with_params;
    ]);
    ("notification", [
      Alcotest.test_case "id=None" `Quick test_is_notification_id_none;
      Alcotest.test_case "id=Null" `Quick test_is_notification_id_null;
      Alcotest.test_case "id=Int" `Quick test_is_notification_id_int;
      Alcotest.test_case "id=String" `Quick test_is_notification_id_string;
      Alcotest.test_case "notification true" `Quick test_is_notification_true;
      Alcotest.test_case "notification false" `Quick test_is_notification_false;
    ]);
    ("responses", [
      Alcotest.test_case "success" `Quick test_make_success_response;
      Alcotest.test_case "error no data" `Quick test_make_error_response_no_data;
      Alcotest.test_case "error with data" `Quick test_make_error_response_with_data;
    ]);
    ("protocol_version", [
      Alcotest.test_case "known version" `Quick test_normalize_known_version;
      Alcotest.test_case "latest version" `Quick test_normalize_latest_version;
      Alcotest.test_case "unknown fallback" `Quick test_normalize_unknown_version;
      Alcotest.test_case "from params some" `Quick test_protocol_version_from_params_some;
      Alcotest.test_case "from params missing" `Quick test_protocol_version_from_params_missing;
      Alcotest.test_case "from params none" `Quick test_protocol_version_from_params_none;
      Alcotest.test_case "from params non-assoc" `Quick test_protocol_version_from_params_non_assoc;
    ]);
    ("json_serializers", [
      Alcotest.test_case "tool basic" `Quick test_tool_to_json_basic;
      Alcotest.test_case "tool deprecated" `Quick test_tool_to_json_deprecated;
      Alcotest.test_case "tool short desc" `Quick test_tool_to_json_short_description;
      Alcotest.test_case "resource" `Quick test_resource_to_json;
      Alcotest.test_case "resource template" `Quick test_resource_template_to_json;
      Alcotest.test_case "prompt" `Quick test_prompt_to_json;
      Alcotest.test_case "prompt detail" `Quick test_prompt_to_detail_json;
    ]);
    ("member", [
      Alcotest.test_case "found" `Quick test_proto_member_found;
      Alcotest.test_case "not found" `Quick test_proto_member_not_found;
      Alcotest.test_case "non-assoc" `Quick test_proto_member_non_assoc;
    ]);
    ("handle_initialize", [
      Alcotest.test_case "known version" `Quick test_handle_initialize_known_version;
      Alcotest.test_case "unknown version" `Quick test_handle_initialize_unknown_version;
      Alcotest.test_case "no version" `Quick test_handle_initialize_no_version;
    ]);
    ("handle_lists", [
      Alcotest.test_case "tools list" `Quick test_handle_tools_list;
      Alcotest.test_case "resources list" `Quick test_handle_resources_list;
      Alcotest.test_case "templates list" `Quick test_handle_resource_templates_list;
      Alcotest.test_case "prompts list" `Quick test_handle_prompts_list;
    ]);
    ("handle_prompts_get", [
      Alcotest.test_case "found" `Quick test_handle_prompts_get_found;
      Alcotest.test_case "not found" `Quick test_handle_prompts_get_not_found;
      Alcotest.test_case "missing name" `Quick test_handle_prompts_get_missing_name;
      Alcotest.test_case "invalid params" `Quick test_handle_prompts_get_invalid_params;
      Alcotest.test_case "none params" `Quick test_handle_prompts_get_none_params;
    ]);
    ("handle_resources_read", [
      Alcotest.test_case "found" `Quick test_handle_resources_read_found;
      Alcotest.test_case "not found" `Quick test_handle_resources_read_not_found;
      Alcotest.test_case "missing uri" `Quick test_handle_resources_read_missing_uri;
      Alcotest.test_case "invalid params" `Quick test_handle_resources_read_invalid_params;
      Alcotest.test_case "none params" `Quick test_handle_resources_read_none_params;
    ]);
    ("handle_tools_call", [
      Alcotest.test_case "success" `Quick test_handle_tools_call_success;
      Alcotest.test_case "handler error" `Quick test_handle_tools_call_handler_error;
      Alcotest.test_case "tool not found" `Quick test_handle_tools_call_not_found;
      Alcotest.test_case "missing name" `Quick test_handle_tools_call_missing_name;
      Alcotest.test_case "no arguments" `Quick test_handle_tools_call_no_arguments;
      Alcotest.test_case "invalid params" `Quick test_handle_tools_call_invalid_params;
      Alcotest.test_case "none params" `Quick test_handle_tools_call_none_params;
    ]);
    ("process_request_sync", [
      Alcotest.test_case "initialize" `Quick test_process_initialize;
      Alcotest.test_case "initialized" `Quick test_process_initialized;
      Alcotest.test_case "notifications/initialized" `Quick test_process_notifications_initialized;
      Alcotest.test_case "tools/list" `Quick test_process_tools_list;
      Alcotest.test_case "tools/call ok" `Quick test_process_tools_call_ok;
      Alcotest.test_case "tools/call error" `Quick test_process_tools_call_error;
      Alcotest.test_case "resources/list" `Quick test_process_resources_list;
      Alcotest.test_case "resources/templates/list" `Quick test_process_resource_templates_list;
      Alcotest.test_case "resources/read ok" `Quick test_process_resources_read_ok;
      Alcotest.test_case "resources/read error" `Quick test_process_resources_read_error;
      Alcotest.test_case "prompts/list" `Quick test_process_prompts_list;
      Alcotest.test_case "prompts/get ok" `Quick test_process_prompts_get_ok;
      Alcotest.test_case "prompts/get error" `Quick test_process_prompts_get_error;
      Alcotest.test_case "unknown method" `Quick test_process_unknown_method;
    ]);
    ("create_server", [
      Alcotest.test_case "defaults" `Quick test_create_server_defaults;
      Alcotest.test_case "with templates" `Quick test_create_server_with_templates;
    ]);
    ("figma_api_node_id", [
      Alcotest.test_case "url to api" `Quick test_url_to_api_node_id;
      Alcotest.test_case "url to api multi" `Quick test_url_to_api_node_id_multiple;
      Alcotest.test_case "api to url" `Quick test_api_to_url_node_id;
      Alcotest.test_case "normalize dash" `Quick test_normalize_node_id_dash;
      Alcotest.test_case "normalize colon" `Quick test_normalize_node_id_colon;
      Alcotest.test_case "normalize both" `Quick test_normalize_node_id_both;
      Alcotest.test_case "normalize plain" `Quick test_normalize_node_id_no_separator;
      Alcotest.test_case "batch normalize" `Quick test_normalize_node_ids;
    ]);
    ("figma_api_errors", [
      Alcotest.test_case "network" `Quick test_error_network;
      Alcotest.test_case "auth" `Quick test_error_auth;
      Alcotest.test_case "not found" `Quick test_error_not_found;
      Alcotest.test_case "rate limited" `Quick test_error_rate_limited;
      Alcotest.test_case "parse" `Quick test_error_parse;
      Alcotest.test_case "unknown" `Quick test_error_unknown;
    ]);
    ("figma_api_json", [
      Alcotest.test_case "member found" `Quick test_member_assoc_found;
      Alcotest.test_case "member missing" `Quick test_member_assoc_missing;
      Alcotest.test_case "member non-assoc" `Quick test_member_non_assoc;
      Alcotest.test_case "extract doc present" `Quick test_extract_document_present;
      Alcotest.test_case "extract doc missing" `Quick test_extract_document_missing;
      Alcotest.test_case "extract pages" `Quick test_extract_pages_normal;
      Alcotest.test_case "pages no children" `Quick test_extract_pages_no_children;
      Alcotest.test_case "pages no document" `Quick test_extract_pages_no_document;
      Alcotest.test_case "page names" `Quick test_get_page_names;
      Alcotest.test_case "frames mixed" `Quick test_get_frames_from_page_mixed;
      Alcotest.test_case "frames no children" `Quick test_get_frames_from_page_no_children;
      Alcotest.test_case "all screens" `Quick test_get_all_screens;
    ]);
    ("figma_api_url", [
      Alcotest.test_case "design url" `Quick test_parse_url_design;
      Alcotest.test_case "file url" `Quick test_parse_url_file;
      Alcotest.test_case "proto url" `Quick test_parse_url_proto;
      Alcotest.test_case "team url" `Quick test_parse_url_team;
      Alcotest.test_case "team+project" `Quick test_parse_url_team_project;
      Alcotest.test_case "unrecognized" `Quick test_parse_url_unrecognized;
      Alcotest.test_case "invalid url" `Quick test_parse_url_invalid;
      Alcotest.test_case "empty url" `Quick test_parse_url_empty;
      Alcotest.test_case "colon node-id" `Quick test_parse_url_node_id_colon_format;
    ]);
  ]
