(** Comprehensive Test Suite for MCP Protocol Modules

    Tests both mcp_protocol.ml and mcp_protocol_eio.ml
    Target: 40+ tests covering JSON-RPC parsing, serialization, roundtrip,
    MCP message types, and edge cases.

    @author Test Coverage Specialist Agent
    @date 2026-01-27
*)

open Alcotest

(** ============== Test Helpers ============== *)

let json_equal a b =
  Yojson.Safe.equal a b

let json_testable = testable Yojson.Safe.pp json_equal

(** ============== mcp_protocol.ml Tests ============== *)

module Protocol_tests = struct

  (** --- JSON Utilities --- *)

  let test_member_existing_key () =
    let json = `Assoc [("name", `String "test"); ("value", `Int 42)] in
    let result = Mcp_protocol.member "name" json in
    check (option json_testable) "finds existing key"
      (Some (`String "test")) result

  let test_member_missing_key () =
    let json = `Assoc [("name", `String "test")] in
    let result = Mcp_protocol.member "missing" json in
    check (option json_testable) "returns None for missing key"
      None result

  let test_member_non_object () =
    let json = `List [`String "a"; `String "b"] in
    let result = Mcp_protocol.member "key" json in
    check (option json_testable) "returns None for non-object"
      None result

  let test_member_null_json () =
    let json = `Null in
    let result = Mcp_protocol.member "key" json in
    check (option json_testable) "returns None for null"
      None result

  (** --- parse_request Tests --- *)

  let test_parse_valid_request () =
    let json_str = {|{"jsonrpc":"2.0","id":1,"method":"test","params":{}}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        check string "jsonrpc version" "2.0" req.jsonrpc;
        check string "method" "test" req.method_;
        check bool "has id" true (Option.is_some req.id);
        check bool "has params" true (Option.is_some req.params)
    | Error msg ->
        fail (Printf.sprintf "Expected Ok, got Error: %s" msg)

  let test_parse_request_string_id () =
    let json_str = {|{"jsonrpc":"2.0","id":"abc-123","method":"test"}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        check (option json_testable) "string id"
          (Some (`String "abc-123")) req.id
    | Error msg ->
        fail (Printf.sprintf "Expected Ok, got Error: %s" msg)

  let test_parse_request_null_id () =
    let json_str = {|{"jsonrpc":"2.0","id":null,"method":"notify"}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        check (option json_testable) "null id"
          (Some `Null) req.id
    | Error _ ->
        fail "Expected Ok for null id"

  let test_parse_request_no_id () =
    let json_str = {|{"jsonrpc":"2.0","method":"notify"}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        check (option json_testable) "no id" None req.id
    | Error _ ->
        fail "Expected Ok for no id"

  let test_parse_invalid_json () =
    let json_str = "{invalid json" in
    match Mcp_protocol.parse_request json_str with
    | Error msg ->
        check bool "contains parse error" true
          (String.length msg > 0)
    | Ok _ ->
        fail "Expected Error for invalid JSON"

  let test_parse_wrong_version () =
    let json_str = {|{"jsonrpc":"1.0","id":1,"method":"test"}|} in
    match Mcp_protocol.parse_request json_str with
    | Error msg ->
        check bool "contains version error" true
          (String.length msg > 0)
    | Ok _ ->
        fail "Expected Error for wrong version"

  let test_parse_missing_method () =
    let json_str = {|{"jsonrpc":"2.0","id":1}|} in
    match Mcp_protocol.parse_request json_str with
    | Error msg ->
        check bool "contains missing method error" true
          (String.length msg > 0)
    | Ok _ ->
        fail "Expected Error for missing method"

  let test_parse_empty_string () =
    let json_str = "" in
    match Mcp_protocol.parse_request json_str with
    | Error _ -> ()  (* Expected *)
    | Ok _ -> fail "Expected Error for empty string"

  let test_parse_with_complex_params () =
    let json_str = {|{"jsonrpc":"2.0","id":1,"method":"test","params":{"nested":{"deep":true},"array":[1,2,3]}}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        (match req.params with
         | Some (`Assoc _) -> ()
         | _ -> fail "Expected complex params")
    | Error msg ->
        fail (Printf.sprintf "Expected Ok, got Error: %s" msg)

  (** --- is_notification Tests --- *)

  let test_is_notification_no_id () =
    let req = { Mcp_protocol.jsonrpc = "2.0"; id = None; method_ = "test"; params = None } in
    check bool "no id is notification" true (Mcp_protocol.is_notification req)

  let test_is_notification_null_id () =
    let req = { Mcp_protocol.jsonrpc = "2.0"; id = Some `Null; method_ = "test"; params = None } in
    check bool "null id is notification" true (Mcp_protocol.is_notification req)

  let test_is_notification_with_id () =
    let req = { Mcp_protocol.jsonrpc = "2.0"; id = Some (`Int 1); method_ = "test"; params = None } in
    check bool "with id is not notification" false (Mcp_protocol.is_notification req)

  let test_is_notification_string_id () =
    let req = { Mcp_protocol.jsonrpc = "2.0"; id = Some (`String "abc"); method_ = "test"; params = None } in
    check bool "string id is not notification" false (Mcp_protocol.is_notification req)

  (** --- Response Generation Tests --- *)

  let test_make_success_response () =
    let id = `Int 42 in
    let result = `Assoc [("data", `String "ok")] in
    let response = Mcp_protocol.make_success_response id result in
    match response with
    | `Assoc fields ->
        check (option json_testable) "jsonrpc"
          (Some (`String "2.0")) (List.assoc_opt "jsonrpc" fields);
        check (option json_testable) "id"
          (Some (`Int 42)) (List.assoc_opt "id" fields);
        check bool "has result" true (List.mem_assoc "result" fields);
        check bool "no error" false (List.mem_assoc "error" fields)
    | _ -> fail "Expected Assoc"

  let test_make_error_response () =
    let id = `Int 1 in
    let response = Mcp_protocol.make_error_response id (-32600) "Invalid Request" None in
    match response with
    | `Assoc fields ->
        check (option json_testable) "jsonrpc"
          (Some (`String "2.0")) (List.assoc_opt "jsonrpc" fields);
        (match List.assoc_opt "error" fields with
         | Some (`Assoc err_fields) ->
             check (option json_testable) "error code"
               (Some (`Int (-32600))) (List.assoc_opt "code" err_fields);
             check (option json_testable) "error message"
               (Some (`String "Invalid Request")) (List.assoc_opt "message" err_fields)
         | _ -> fail "Expected error object")
    | _ -> fail "Expected Assoc"

  let test_make_error_response_with_data () =
    let id = `String "req-1" in
    let data = Some (`Assoc [("details", `String "extra info")]) in
    let response = Mcp_protocol.make_error_response id (-32603) "Internal Error" data in
    match response with
    | `Assoc fields ->
        (match List.assoc_opt "error" fields with
         | Some (`Assoc err_fields) ->
             check bool "has data" true (List.mem_assoc "data" err_fields)
         | _ -> fail "Expected error object")
    | _ -> fail "Expected Assoc"

  (** --- Type Serialization Tests --- *)

  let test_tool_to_json () =
    let tool : Mcp_protocol.tool_def = {
      name = "test_tool";
      description = "A test tool";
      input_schema = `Assoc [("type", `String "object")];
    } in
    let json = Mcp_protocol.tool_to_json tool in
    match json with
    | `Assoc fields ->
        check (option json_testable) "name"
          (Some (`String "test_tool")) (List.assoc_opt "name" fields);
        check (option json_testable) "description"
          (Some (`String "A test tool")) (List.assoc_opt "description" fields);
        check bool "has inputSchema" true (List.mem_assoc "inputSchema" fields)
    | _ -> fail "Expected Assoc"

  let test_resource_to_json () =
    let resource : Mcp_protocol.mcp_resource = {
      uri = "file:///test.txt";
      name = "Test Resource";
      description = "A test resource";
      mime_type = "text/plain";
    } in
    let json = Mcp_protocol.resource_to_json resource in
    match json with
    | `Assoc fields ->
        check (option json_testable) "uri"
          (Some (`String "file:///test.txt")) (List.assoc_opt "uri" fields);
        check (option json_testable) "mimeType"
          (Some (`String "text/plain")) (List.assoc_opt "mimeType" fields)
    | _ -> fail "Expected Assoc"

  let test_prompt_arg_to_json () =
    let arg : Mcp_protocol.prompt_arg = {
      name = "query";
      description = "The search query";
      required = true;
    } in
    let json = Mcp_protocol.prompt_arg_to_json arg in
    match json with
    | `Assoc fields ->
        check (option json_testable) "name"
          (Some (`String "query")) (List.assoc_opt "name" fields);
        check (option json_testable) "required"
          (Some (`Bool true)) (List.assoc_opt "required" fields)
    | _ -> fail "Expected Assoc"

  let test_prompt_to_json () =
    let prompt : Mcp_protocol.mcp_prompt = {
      name = "search";
      description = "Search prompt";
      arguments = [];
      text = "Search for: {query}";
    } in
    let json = Mcp_protocol.prompt_to_json prompt in
    match json with
    | `Assoc fields ->
        check (option json_testable) "name"
          (Some (`String "search")) (List.assoc_opt "name" fields);
        check (option json_testable) "arguments"
          (Some (`List [])) (List.assoc_opt "arguments" fields);
        (* prompt_to_json does NOT include text field *)
        check bool "no text field in list view" false
          (List.mem_assoc "text" fields)
    | _ -> fail "Expected Assoc"

  let test_prompt_to_detail_json () =
    let prompt : Mcp_protocol.mcp_prompt = {
      name = "search";
      description = "Search prompt";
      arguments = [];
      text = "Search for: {query}";
    } in
    let json = Mcp_protocol.prompt_to_detail_json prompt in
    match json with
    | `Assoc fields ->
        check (option json_testable) "text"
          (Some (`String "Search for: {query}")) (List.assoc_opt "text" fields)
    | _ -> fail "Expected Assoc"

  (** --- Protocol Version Tests --- *)

  let test_normalize_protocol_version_supported () =
    let version = Mcp_protocol.normalize_protocol_version "2024-11-05" in
    check string "supported version unchanged" "2024-11-05" version

  let test_normalize_protocol_version_unsupported () =
    let version = Mcp_protocol.normalize_protocol_version "1999-01-01" in
    check string "unsupported version normalized"
      Mcp_protocol.default_protocol_version version

  let test_protocol_version_from_params () =
    let params = Some (`Assoc [("protocolVersion", `String "2024-11-05")]) in
    let version = Mcp_protocol.protocol_version_from_params params in
    check string "extracts version from params" "2024-11-05" version

  let test_protocol_version_from_params_missing () =
    let params = Some (`Assoc [("other", `String "value")]) in
    let version = Mcp_protocol.protocol_version_from_params params in
    check string "default when missing"
      Mcp_protocol.default_protocol_version version

  let test_protocol_version_from_params_none () =
    let version = Mcp_protocol.protocol_version_from_params None in
    check string "default when None"
      Mcp_protocol.default_protocol_version version

  (** --- Handler Tests --- *)

  let make_test_server () =
    let tool : Mcp_protocol.tool_def = {
      name = "echo";
      description = "Echo tool";
      input_schema = `Assoc [("type", `String "object")];
    } in
    let resource : Mcp_protocol.mcp_resource = {
      uri = "file:///test.txt";
      name = "Test";
      description = "Test resource";
      mime_type = "text/plain";
    } in
    let prompt : Mcp_protocol.mcp_prompt = {
      name = "greeting";
      description = "Greeting prompt";
      arguments = [];
      text = "Hello!";
    } in
    let echo_handler params =
      Ok (`Assoc [("content", `List [`Assoc [
        ("type", `String "text");
        ("text", `String (Yojson.Safe.to_string params))
      ]])])
    in
    let read_resource uri =
      if uri = "file:///test.txt" then
        Ok ("text/plain", "Test content")
      else
        Error (Printf.sprintf "Resource not found: %s" uri)
    in
    Mcp_protocol.create_server
      ~handlers_sync:[("echo", echo_handler)]
      [tool] [resource] [prompt] read_resource

  let test_handle_initialize () =
    let params = Some (`Assoc [("protocolVersion", `String "2025-11-25")]) in
    let result = Mcp_protocol.handle_initialize params in
    match result with
    | `Assoc fields ->
        check (option json_testable) "protocolVersion"
          (Some (`String "2025-11-25")) (List.assoc_opt "protocolVersion" fields);
        check bool "has capabilities" true (List.mem_assoc "capabilities" fields);
        check bool "has serverInfo" true (List.mem_assoc "serverInfo" fields);
        check bool "has instructions" true (List.mem_assoc "instructions" fields)
    | _ -> fail "Expected Assoc"

  let test_handle_tools_list () =
    let server = make_test_server () in
    let result = Mcp_protocol.handle_tools_list server None in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "tools" fields with
         | Some (`List tools) ->
             check int "one tool" 1 (List.length tools)
         | _ -> fail "Expected tools list")
    | _ -> fail "Expected Assoc"

  let test_handle_resources_list () =
    let server = make_test_server () in
    let result = Mcp_protocol.handle_resources_list server None in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "resources" fields with
         | Some (`List resources) ->
             check int "one resource" 1 (List.length resources)
         | _ -> fail "Expected resources list")
    | _ -> fail "Expected Assoc"

  let test_handle_prompts_list () =
    let server = make_test_server () in
    let result = Mcp_protocol.handle_prompts_list server None in
    match result with
    | `Assoc fields ->
        (match List.assoc_opt "prompts" fields with
         | Some (`List prompts) ->
             check int "one prompt" 1 (List.length prompts)
         | _ -> fail "Expected prompts list")
    | _ -> fail "Expected Assoc"

  let test_handle_prompts_get_found () =
    let server = make_test_server () in
    let params = Some (`Assoc [("name", `String "greeting")]) in
    match Mcp_protocol.handle_prompts_get server params with
    | Ok result ->
        (match result with
         | `Assoc fields ->
             check bool "has prompt" true (List.mem_assoc "prompt" fields)
         | _ -> fail "Expected Assoc")
    | Error (code, msg) ->
        fail (Printf.sprintf "Expected Ok, got Error(%d, %s)" code msg)

  let test_handle_prompts_get_not_found () =
    let server = make_test_server () in
    let params = Some (`Assoc [("name", `String "nonexistent")]) in
    match Mcp_protocol.handle_prompts_get server params with
    | Error (code, _) ->
        check int "invalid_params error" Mcp_protocol.invalid_params code
    | Ok _ ->
        fail "Expected Error for nonexistent prompt"

  let test_handle_prompts_get_missing_name () =
    let server = make_test_server () in
    let params = Some (`Assoc []) in
    match Mcp_protocol.handle_prompts_get server params with
    | Error (code, _) ->
        check int "invalid_params error" Mcp_protocol.invalid_params code
    | Ok _ ->
        fail "Expected Error for missing name"

  let test_handle_resources_read_found () =
    let server = make_test_server () in
    let params = Some (`Assoc [("uri", `String "file:///test.txt")]) in
    match Mcp_protocol.handle_resources_read server params with
    | Ok result ->
        (match result with
         | `Assoc fields ->
             check bool "has contents" true (List.mem_assoc "contents" fields)
         | _ -> fail "Expected Assoc")
    | Error (code, msg) ->
        fail (Printf.sprintf "Expected Ok, got Error(%d, %s)" code msg)

  let test_handle_resources_read_not_found () =
    let server = make_test_server () in
    let params = Some (`Assoc [("uri", `String "file:///missing.txt")]) in
    match Mcp_protocol.handle_resources_read server params with
    | Error (code, _) ->
        check int "internal_error" Mcp_protocol.internal_error code
    | Ok _ ->
        fail "Expected Error for missing resource"

  let test_handle_tools_call_sync () =
    let server = make_test_server () in
    let params = Some (`Assoc [
      ("name", `String "echo");
      ("arguments", `Assoc [("input", `String "hello")])
    ]) in
    match Mcp_protocol.handle_tools_call_sync server params with
    | Ok result ->
        (match result with
         | `Assoc fields ->
             check bool "has content" true (List.mem_assoc "content" fields)
         | _ -> fail "Expected Assoc")
    | Error (code, msg) ->
        fail (Printf.sprintf "Expected Ok, got Error(%d, %s)" code msg)

  let test_handle_tools_call_not_found () =
    let server = make_test_server () in
    let params = Some (`Assoc [
      ("name", `String "nonexistent");
      ("arguments", `Assoc [])
    ]) in
    match Mcp_protocol.handle_tools_call_sync server params with
    | Error (code, _) ->
        check int "method_not_found" Mcp_protocol.method_not_found code
    | Ok _ ->
        fail "Expected Error for nonexistent tool"

  (** --- process_request_sync Tests --- *)

  let test_process_request_initialize () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 1);
      method_ = "initialize";
      params = Some (`Assoc []);
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        check bool "has result" true (List.mem_assoc "result" fields);
        check bool "no error" false (List.mem_assoc "error" fields)
    | _ -> fail "Expected Assoc"

  let test_process_request_unknown_method () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 1);
      method_ = "unknown/method";
      params = None;
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        check bool "has error" true (List.mem_assoc "error" fields);
        (match List.assoc_opt "error" fields with
         | Some (`Assoc err) ->
             check (option json_testable) "method_not_found code"
               (Some (`Int Mcp_protocol.method_not_found))
               (List.assoc_opt "code" err)
         | _ -> fail "Expected error object")
    | _ -> fail "Expected Assoc"

  let test_process_request_initialized () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 1);
      method_ = "initialized";
      params = None;
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        check (option json_testable) "result null"
          (Some `Null) (List.assoc_opt "result" fields)
    | _ -> fail "Expected Assoc"

  let test_process_request_resources_templates_list () =
    let server = make_test_server () in
    let req = {
      Mcp_protocol.jsonrpc = "2.0";
      id = Some (`Int 1);
      method_ = "resources/templates/list";
      params = None;
    } in
    let response = Mcp_protocol.process_request_sync server req in
    match response with
    | `Assoc fields ->
        (match List.assoc_opt "result" fields with
         | Some (`Assoc result_fields) ->
             check (option json_testable) "empty templates"
               (Some (`List []))
               (List.assoc_opt "resourceTemplates" result_fields)
         | _ -> fail "Expected result object")
    | _ -> fail "Expected Assoc"

  (** --- Roundtrip Tests --- *)

  let test_request_roundtrip () =
    let original = {|{"jsonrpc":"2.0","id":123,"method":"tools/list","params":{"filter":"all"}}|} in
    match Mcp_protocol.parse_request original with
    | Ok req ->
        check string "method preserved" "tools/list" req.method_;
        check (option json_testable) "id preserved"
          (Some (`Int 123)) req.id
    | Error msg ->
        fail (Printf.sprintf "Parse failed: %s" msg)

  let test_response_roundtrip () =
    let id = `Int 999 in
    let result = `Assoc [("data", `List [`Int 1; `Int 2; `Int 3])] in
    let response = Mcp_protocol.make_success_response id result in
    let json_str = Yojson.Safe.to_string response in
    let parsed = Yojson.Safe.from_string json_str in
    check json_testable "roundtrip preserves structure" response parsed

  (** --- Error Code Constants --- *)

  let test_error_codes () =
    check int "parse_error" (-32700) Mcp_protocol.parse_error;
    check int "invalid_request" (-32600) Mcp_protocol.invalid_request;
    check int "method_not_found" (-32601) Mcp_protocol.method_not_found;
    check int "invalid_params" (-32602) Mcp_protocol.invalid_params;
    check int "internal_error" (-32603) Mcp_protocol.internal_error

  (** --- Server Info --- *)

  let test_server_info () =
    check string "server_name" "figma-mcp" Mcp_protocol.server_name;
    check bool "version not empty" true (String.length Mcp_protocol.server_version > 0);
    check bool "protocol_version not empty" true (String.length Mcp_protocol.protocol_version > 0)

end

(** ============== mcp_protocol_eio.ml Tests ============== *)

module Eio_tests = struct

  (** --- Message Classification --- *)

  let test_classify_request () =
    let msg = {|{"jsonrpc":"2.0","id":1,"method":"test"}|} in
    let kind = Mcp_protocol_eio.classify_message msg in
    check bool "is request" true (kind = `Request)

  let test_classify_notification_no_id () =
    let msg = {|{"jsonrpc":"2.0","method":"notify"}|} in
    let kind = Mcp_protocol_eio.classify_message msg in
    check bool "is notification (no id)" true (kind = `Notification)

  let test_classify_notification_null_id () =
    let msg = {|{"jsonrpc":"2.0","id":null,"method":"notify"}|} in
    let kind = Mcp_protocol_eio.classify_message msg in
    check bool "is notification (null id)" true (kind = `Notification)

  let test_classify_response_result () =
    let msg = {|{"jsonrpc":"2.0","id":1,"result":{}}|} in
    let kind = Mcp_protocol_eio.classify_message msg in
    check bool "is response (result)" true (kind = `Response)

  let test_classify_response_error () =
    let msg = {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"test"}}|} in
    let kind = Mcp_protocol_eio.classify_message msg in
    check bool "is response (error)" true (kind = `Response)

  let test_classify_unknown_no_method () =
    let msg = {|{"jsonrpc":"2.0"}|} in
    let kind = Mcp_protocol_eio.classify_message msg in
    check bool "is unknown" true (kind = `Unknown)

  let test_classify_invalid_json () =
    let msg = "{invalid" in
    let kind = Mcp_protocol_eio.classify_message msg in
    check bool "invalid json is unknown" true (kind = `Unknown)

  let test_classify_non_object () =
    let msg = {|[1, 2, 3]|} in
    let kind = Mcp_protocol_eio.classify_message msg in
    check bool "non-object is unknown" true (kind = `Unknown)

  (** --- SSE Formatting --- *)

  let test_format_sse_data_single_line () =
    let result = Mcp_protocol_eio.format_sse_data "hello world" in
    check string "single line format" "data: hello world" result

  let test_format_sse_data_multiline () =
    let result = Mcp_protocol_eio.format_sse_data "line1\nline2\nline3" in
    check string "multiline format" "data: line1\ndata: line2\ndata: line3" result

  let test_format_sse_data_empty () =
    let result = Mcp_protocol_eio.format_sse_data "" in
    check string "empty data" "data: " result

  (** --- Config --- *)

  let test_default_config () =
    let config = Mcp_protocol_eio.default_config in
    check int "default port" 8933 config.port;
    check string "default host" "localhost" config.host;
    check int "default max_connections" 64 config.max_connections

  (** --- MCP Request Processing --- *)

  let make_test_server () =
    let echo_handler params =
      Ok (`Assoc [("content", `List [`Assoc [
        ("type", `String "text");
        ("text", `String (Yojson.Safe.to_string params))
      ]])])
    in
    let read_resource _uri = Error "Not implemented" in
    Mcp_protocol.create_server
      ~handlers_sync:[("echo", echo_handler)]
      [] [] [] read_resource

  let test_process_mcp_request_valid () =
    let server = make_test_server () in
    let body = {|{"jsonrpc":"2.0","id":1,"method":"tools/list"}|} in
    let response = Mcp_protocol_eio.process_mcp_request_sync server body in
    let parsed = Yojson.Safe.from_string response in
    match parsed with
    | `Assoc fields ->
        check bool "has result" true (List.mem_assoc "result" fields)
    | _ -> fail "Expected Assoc"

  let test_process_mcp_request_invalid () =
    let server = make_test_server () in
    let body = "{invalid json" in
    let response = Mcp_protocol_eio.process_mcp_request_sync server body in
    let parsed = Yojson.Safe.from_string response in
    match parsed with
    | `Assoc fields ->
        check bool "has error" true (List.mem_assoc "error" fields)
    | _ -> fail "Expected Assoc"

end

(** ============== Edge Case Tests ============== *)

module Edge_cases = struct

  let test_unicode_in_request () =
    let json_str = {|{"jsonrpc":"2.0","id":1,"method":"test","params":{"text":"한글 테스트 🎨"}}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        (match req.params with
         | Some (`Assoc fields) ->
             check (option json_testable) "unicode preserved"
               (Some (`String "한글 테스트 🎨"))
               (List.assoc_opt "text" fields)
         | _ -> fail "Expected params")
    | Error msg ->
        fail (Printf.sprintf "Parse failed: %s" msg)

  let test_large_numeric_id () =
    let json_str = {|{"jsonrpc":"2.0","id":9999999999999,"method":"test"}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        check bool "has id" true (Option.is_some req.id)
    | Error msg ->
        fail (Printf.sprintf "Parse failed: %s" msg)

  let test_nested_json_params () =
    let json_str = {|{"jsonrpc":"2.0","id":1,"method":"test","params":{"a":{"b":{"c":{"d":1}}}}}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        check bool "has params" true (Option.is_some req.params)
    | Error msg ->
        fail (Printf.sprintf "Parse failed: %s" msg)

  let test_array_params () =
    let json_str = {|{"jsonrpc":"2.0","id":1,"method":"test","params":[1,2,3]}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        (match req.params with
         | Some (`List _) -> ()
         | _ -> fail "Expected array params")
    | Error msg ->
        fail (Printf.sprintf "Parse failed: %s" msg)

  let test_float_id () =
    let json_str = {|{"jsonrpc":"2.0","id":1.5,"method":"test"}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        (match req.id with
         | Some (`Float f) -> check (float 0.001) "float id" 1.5 f
         | _ -> fail "Expected float id")
    | Error msg ->
        fail (Printf.sprintf "Parse failed: %s" msg)

  let test_special_characters_in_method () =
    let json_str = {|{"jsonrpc":"2.0","id":1,"method":"tools/call"}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        check string "method with slash" "tools/call" req.method_
    | Error msg ->
        fail (Printf.sprintf "Parse failed: %s" msg)

  let test_empty_object_params () =
    let json_str = {|{"jsonrpc":"2.0","id":1,"method":"test","params":{}}|} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        (match req.params with
         | Some (`Assoc []) -> ()
         | _ -> fail "Expected empty object params")
    | Error msg ->
        fail (Printf.sprintf "Parse failed: %s" msg)

  let test_whitespace_in_json () =
    let json_str = {|
    {
      "jsonrpc": "2.0",
      "id": 1,
      "method": "test"
    }
    |} in
    match Mcp_protocol.parse_request json_str with
    | Ok req ->
        check string "method" "test" req.method_
    | Error msg ->
        fail (Printf.sprintf "Parse failed: %s" msg)

end

(** ============== Test Registration ============== *)

let protocol_tests = [
  (* JSON Utilities *)
  "member existing key", `Quick, Protocol_tests.test_member_existing_key;
  "member missing key", `Quick, Protocol_tests.test_member_missing_key;
  "member non-object", `Quick, Protocol_tests.test_member_non_object;
  "member null json", `Quick, Protocol_tests.test_member_null_json;

  (* parse_request *)
  "parse valid request", `Quick, Protocol_tests.test_parse_valid_request;
  "parse request string id", `Quick, Protocol_tests.test_parse_request_string_id;
  "parse request null id", `Quick, Protocol_tests.test_parse_request_null_id;
  "parse request no id", `Quick, Protocol_tests.test_parse_request_no_id;
  "parse invalid json", `Quick, Protocol_tests.test_parse_invalid_json;
  "parse wrong version", `Quick, Protocol_tests.test_parse_wrong_version;
  "parse missing method", `Quick, Protocol_tests.test_parse_missing_method;
  "parse empty string", `Quick, Protocol_tests.test_parse_empty_string;
  "parse with complex params", `Quick, Protocol_tests.test_parse_with_complex_params;

  (* is_notification *)
  "is_notification no id", `Quick, Protocol_tests.test_is_notification_no_id;
  "is_notification null id", `Quick, Protocol_tests.test_is_notification_null_id;
  "is_notification with id", `Quick, Protocol_tests.test_is_notification_with_id;
  "is_notification string id", `Quick, Protocol_tests.test_is_notification_string_id;

  (* Response Generation *)
  "make success response", `Quick, Protocol_tests.test_make_success_response;
  "make error response", `Quick, Protocol_tests.test_make_error_response;
  "make error response with data", `Quick, Protocol_tests.test_make_error_response_with_data;

  (* Type Serialization *)
  "tool_to_json", `Quick, Protocol_tests.test_tool_to_json;
  "resource_to_json", `Quick, Protocol_tests.test_resource_to_json;
  "prompt_arg_to_json", `Quick, Protocol_tests.test_prompt_arg_to_json;
  "prompt_to_json", `Quick, Protocol_tests.test_prompt_to_json;
  "prompt_to_detail_json", `Quick, Protocol_tests.test_prompt_to_detail_json;

  (* Protocol Version *)
  "normalize supported version", `Quick, Protocol_tests.test_normalize_protocol_version_supported;
  "normalize unsupported version", `Quick, Protocol_tests.test_normalize_protocol_version_unsupported;
  "protocol version from params", `Quick, Protocol_tests.test_protocol_version_from_params;
  "protocol version from params missing", `Quick, Protocol_tests.test_protocol_version_from_params_missing;
  "protocol version from params none", `Quick, Protocol_tests.test_protocol_version_from_params_none;

  (* Handlers *)
  "handle_initialize", `Quick, Protocol_tests.test_handle_initialize;
  "handle_tools_list", `Quick, Protocol_tests.test_handle_tools_list;
  "handle_resources_list", `Quick, Protocol_tests.test_handle_resources_list;
  "handle_prompts_list", `Quick, Protocol_tests.test_handle_prompts_list;
  "handle_prompts_get found", `Quick, Protocol_tests.test_handle_prompts_get_found;
  "handle_prompts_get not found", `Quick, Protocol_tests.test_handle_prompts_get_not_found;
  "handle_prompts_get missing name", `Quick, Protocol_tests.test_handle_prompts_get_missing_name;
  "handle_resources_read found", `Quick, Protocol_tests.test_handle_resources_read_found;
  "handle_resources_read not found", `Quick, Protocol_tests.test_handle_resources_read_not_found;
  "handle_tools_call_sync", `Quick, Protocol_tests.test_handle_tools_call_sync;
  "handle_tools_call not found", `Quick, Protocol_tests.test_handle_tools_call_not_found;

  (* process_request_sync *)
  "process_request initialize", `Quick, Protocol_tests.test_process_request_initialize;
  "process_request unknown method", `Quick, Protocol_tests.test_process_request_unknown_method;
  "process_request initialized", `Quick, Protocol_tests.test_process_request_initialized;
  "process_request resources/templates/list", `Quick, Protocol_tests.test_process_request_resources_templates_list;

  (* Roundtrip *)
  "request roundtrip", `Quick, Protocol_tests.test_request_roundtrip;
  "response roundtrip", `Quick, Protocol_tests.test_response_roundtrip;

  (* Constants *)
  "error codes", `Quick, Protocol_tests.test_error_codes;
  "server info", `Quick, Protocol_tests.test_server_info;
]

let eio_tests = [
  (* Message Classification *)
  "classify request", `Quick, Eio_tests.test_classify_request;
  "classify notification no id", `Quick, Eio_tests.test_classify_notification_no_id;
  "classify notification null id", `Quick, Eio_tests.test_classify_notification_null_id;
  "classify response result", `Quick, Eio_tests.test_classify_response_result;
  "classify response error", `Quick, Eio_tests.test_classify_response_error;
  "classify unknown no method", `Quick, Eio_tests.test_classify_unknown_no_method;
  "classify invalid json", `Quick, Eio_tests.test_classify_invalid_json;
  "classify non-object", `Quick, Eio_tests.test_classify_non_object;

  (* SSE Formatting *)
  "format_sse_data single line", `Quick, Eio_tests.test_format_sse_data_single_line;
  "format_sse_data multiline", `Quick, Eio_tests.test_format_sse_data_multiline;
  "format_sse_data empty", `Quick, Eio_tests.test_format_sse_data_empty;

  (* Config *)
  "default_config", `Quick, Eio_tests.test_default_config;

  (* MCP Request Processing *)
  "process_mcp_request valid", `Quick, Eio_tests.test_process_mcp_request_valid;
  "process_mcp_request invalid", `Quick, Eio_tests.test_process_mcp_request_invalid;
]

let edge_case_tests = [
  "unicode in request", `Quick, Edge_cases.test_unicode_in_request;
  "large numeric id", `Quick, Edge_cases.test_large_numeric_id;
  "nested json params", `Quick, Edge_cases.test_nested_json_params;
  "array params", `Quick, Edge_cases.test_array_params;
  "float id", `Quick, Edge_cases.test_float_id;
  "special characters in method", `Quick, Edge_cases.test_special_characters_in_method;
  "empty object params", `Quick, Edge_cases.test_empty_object_params;
  "whitespace in json", `Quick, Edge_cases.test_whitespace_in_json;
]

let () =
  run "MCP Protocol Coverage" [
    "mcp_protocol", protocol_tests;
    "mcp_protocol_eio", eio_tests;
    "edge_cases", edge_case_tests;
  ]
