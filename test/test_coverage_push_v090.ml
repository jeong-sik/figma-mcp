(** Coverage push for v0.9.0 release — pure function tests.
    Targets uncovered modules:
    - mcp_planning_handlers.ml (0/77)
    - mcp_http_helpers.ml (33/158)
    - mcp_protocol_request.ml (21/79)
    - mcp_sdk_adapter_figma.ml (44/87)
    - figma_planning_context.ml (100/126)
    - figma_agent_plan_validator.ml (106/129)
    - telemetry_jsonl.ml (39/47) *)

open Alcotest
open Figma_types

(* ============== Test Helpers ============== *)

let make_node ?(id = "1:1") ?(name = "Node") ?(node_type = Frame)
    ?(visible = true) ?(children = []) ?(characters = None) ?(fills = [])
    ?(bbox = None) ?(layout_mode = None') ?(opacity = 1.0) ?(effects = [])
    () =
  {
    default_node with
    id;
    name;
    node_type;
    visible;
    children;
    characters;
    fills;
    bbox;
    layout_mode;
    opacity;
    effects;
  }

let make_bbox w h = Some { x = 0.; y = 0.; width = w; height = h }

let _make_paint ?(paint_type = Solid) ?(visible = true) ?(opacity = 1.0) () =
  {
    paint_type;
    visible;
    opacity;
    color = None;
    gradient_stops = [];
    image_ref = None;
    scale_mode = None;
  }

(* ============== Mcp_planning_handlers ============== *)

let test_clamp_summary_depth_default () =
  let result = Mcp_planning_handlers.clamp_summary_depth None 3 in
  check int "default applied" 3 result

let test_clamp_summary_depth_provided () =
  let result = Mcp_planning_handlers.clamp_summary_depth (Some 4) 3 in
  check int "provided value" 4 result

let test_clamp_summary_depth_negative () =
  let result = Mcp_planning_handlers.clamp_summary_depth (Some (-1)) 3 in
  check int "negative clamped to 1" 1 result

let test_clamp_summary_depth_too_large () =
  let result = Mcp_planning_handlers.clamp_summary_depth (Some 100) 3 in
  check int "clamped to max" 6 result

let test_clamp_summary_depth_zero () =
  let result = Mcp_planning_handlers.clamp_summary_depth (Some 0) 3 in
  check int "zero passes (not negative)" 0 result

let test_clamp_summary_depth_exact_max () =
  let result = Mcp_planning_handlers.clamp_summary_depth (Some 6) 3 in
  check int "exact max passes" 6 result

let test_clamp_preview_scale_normal () =
  let result = Mcp_planning_handlers.clamp_preview_scale 2.0 in
  check (float 0.001) "normal scale" 2.0 result

let test_clamp_preview_scale_too_small () =
  let result = Mcp_planning_handlers.clamp_preview_scale 0.001 in
  check (float 0.001) "clamped to 0.01" 0.01 result

let test_clamp_preview_scale_too_large () =
  let result = Mcp_planning_handlers.clamp_preview_scale 10.0 in
  check (float 0.001) "clamped to 4.0" 4.0 result

let test_clamp_preview_scale_boundary_low () =
  let result = Mcp_planning_handlers.clamp_preview_scale 0.01 in
  check (float 0.001) "exact min passes" 0.01 result

let test_clamp_preview_scale_boundary_high () =
  let result = Mcp_planning_handlers.clamp_preview_scale 4.0 in
  check (float 0.001) "exact max passes" 4.0 result

let test_resolve_plan_present () =
  let args = `Assoc [ ("plan", `Assoc [ ("tasks", `List []) ]) ] in
  match Mcp_planning_handlers.resolve_plan args with
  | Ok plan ->
      check bool "is assoc" true
        (match plan with `Assoc _ -> true | _ -> false)
  | Error _ -> fail "expected Ok"

let test_resolve_plan_missing () =
  let args = `Assoc [] in
  match Mcp_planning_handlers.resolve_plan args with
  | Error msg -> check bool "has message" true (String.length msg > 0)
  | Ok _ -> fail "expected Error"

let test_resolve_plan_null () =
  let args = `Null in
  match Mcp_planning_handlers.resolve_plan args with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

(* ============== Mcp_http_helpers: pure JSON utilities ============== *)

let test_parse_json_valid () =
  match Mcp_http_helpers.parse_json {|{"key": "value"}|} with
  | Ok json ->
      check bool "parsed" true
        (match json with `Assoc _ -> true | _ -> false)
  | Error _ -> fail "expected Ok"

let test_parse_json_empty () =
  match Mcp_http_helpers.parse_json "" with
  | Ok `Null -> ()
  | _ -> fail "expected Ok Null"

let test_parse_json_whitespace () =
  match Mcp_http_helpers.parse_json "   " with
  | Ok `Null -> ()
  | _ -> fail "expected Ok Null"

let test_parse_json_invalid () =
  match Mcp_http_helpers.parse_json "{bad json}" with
  | Error _ -> ()
  | Ok _ -> fail "expected Error"

let test_parse_json_array () =
  match Mcp_http_helpers.parse_json "[1,2,3]" with
  | Ok (`List _) -> ()
  | _ -> fail "expected Ok List"

let test_get_string_field_present () =
  let json = `Assoc [ ("name", `String "test") ] in
  check (option string) "found" (Some "test")
    (Mcp_http_helpers.get_string_field "name" json)

let test_get_string_field_missing () =
  let json = `Assoc [ ("other", `String "test") ] in
  check (option string) "none" None
    (Mcp_http_helpers.get_string_field "name" json)

let test_get_string_field_wrong_type () =
  let json = `Assoc [ ("name", `Int 42) ] in
  check (option string) "none for int" None
    (Mcp_http_helpers.get_string_field "name" json)

let test_get_string_field_non_object () =
  check (option string) "none for list" None
    (Mcp_http_helpers.get_string_field "name" (`List []))

let test_get_int_field_int () =
  let json = `Assoc [ ("count", `Int 42) ] in
  check (option int) "found" (Some 42)
    (Mcp_http_helpers.get_int_field "count" json)

let test_get_int_field_float () =
  let json = `Assoc [ ("count", `Float 3.7) ] in
  check (option int) "float to int" (Some 3)
    (Mcp_http_helpers.get_int_field "count" json)

let test_get_int_field_missing () =
  let json = `Assoc [] in
  check (option int) "none" None
    (Mcp_http_helpers.get_int_field "count" json)

let test_get_int_field_wrong_type () =
  let json = `Assoc [ ("count", `String "42") ] in
  check (option int) "none for string" None
    (Mcp_http_helpers.get_int_field "count" json)

let test_get_int_field_non_object () =
  check (option int) "none for null" None
    (Mcp_http_helpers.get_int_field "count" `Null)

let test_get_bool_field_true () =
  let json = `Assoc [ ("flag", `Bool true) ] in
  check (option bool) "true" (Some true)
    (Mcp_http_helpers.get_bool_field "flag" json)

let test_get_bool_field_false () =
  let json = `Assoc [ ("flag", `Bool false) ] in
  check (option bool) "false" (Some false)
    (Mcp_http_helpers.get_bool_field "flag" json)

let test_get_bool_field_missing () =
  let json = `Assoc [] in
  check (option bool) "none" None
    (Mcp_http_helpers.get_bool_field "flag" json)

let test_get_bool_field_wrong_type () =
  let json = `Assoc [ ("flag", `String "true") ] in
  check (option bool) "none for string" None
    (Mcp_http_helpers.get_bool_field "flag" json)

let test_get_bool_field_non_object () =
  check (option bool) "none for list" None
    (Mcp_http_helpers.get_bool_field "flag" (`List []))

let test_get_payload_field_present () =
  let nested = `Assoc [ ("x", `Int 1) ] in
  let json = `Assoc [ ("data", nested) ] in
  match Mcp_http_helpers.get_payload_field "data" json with
  | Some _ -> ()
  | None -> fail "expected Some"

let test_get_payload_field_missing () =
  let json = `Assoc [] in
  check bool "none" true
    (Mcp_http_helpers.get_payload_field "data" json = None)

let test_get_payload_field_non_object () =
  check bool "none for null" true
    (Mcp_http_helpers.get_payload_field "data" `Null = None)

let test_parse_positive_int_valid () =
  check (option int) "positive" (Some 42)
    (Mcp_http_helpers.Request.parse_positive_int "42")

let test_parse_positive_int_zero () =
  check (option int) "zero is not positive" None
    (Mcp_http_helpers.Request.parse_positive_int "0")

let test_parse_positive_int_negative () =
  check (option int) "negative" None
    (Mcp_http_helpers.Request.parse_positive_int "-5")

let test_parse_positive_int_invalid () =
  check (option int) "not a number" None
    (Mcp_http_helpers.Request.parse_positive_int "abc")

let test_parse_positive_int_empty () =
  check (option int) "empty string" None
    (Mcp_http_helpers.Request.parse_positive_int "")

let test_request_path () =
  let request =
    Httpun.Request.create ~headers:(Httpun.Headers.of_list []) `GET"/mcp?session=123"
  in
  check string "path without query" "/mcp"
    (Mcp_http_helpers.Request.path request)

let test_request_path_no_query () =
  let request =
    Httpun.Request.create ~headers:(Httpun.Headers.of_list []) `GET"/health"
  in
  check string "simple path" "/health"
    (Mcp_http_helpers.Request.path request)

let test_request_method () =
  let request =
    Httpun.Request.create ~headers:(Httpun.Headers.of_list []) `POST"/mcp"
  in
  check bool "is POST" true
    (Mcp_http_helpers.Request.method_ request = `POST)

let test_accepts_sse_true () =
  let request =
    Httpun.Request.create
      ~headers:
        (Httpun.Headers.of_list
           [ ("accept", "application/json, text/event-stream") ])
      `GET"/mcp"
  in
  check bool "accepts SSE" true
    (Mcp_http_helpers.Request.accepts_sse request)

let test_accepts_sse_false () =
  let request =
    Httpun.Request.create
      ~headers:
        (Httpun.Headers.of_list [ ("accept", "application/json") ])
      `GET"/mcp"
  in
  check bool "no SSE" false
    (Mcp_http_helpers.Request.accepts_sse request)

let test_accepts_sse_no_header () =
  let request =
    Httpun.Request.create ~headers:(Httpun.Headers.of_list []) `GET"/mcp"
  in
  check bool "no accept header" false
    (Mcp_http_helpers.Request.accepts_sse request)

let test_accepts_sse_mixed () =
  let request =
    Httpun.Request.create
      ~headers:
        (Httpun.Headers.of_list
           [ ("accept", "application/json, text/event-stream") ])
      `GET"/mcp"
  in
  check bool "mixed accept" true
    (Mcp_http_helpers.Request.accepts_sse request)

let test_accepts_sse_sse_only_false () =
  let request =
    Httpun.Request.create
      ~headers:
        (Httpun.Headers.of_list [ ("accept", "text/event-stream") ])
      `GET"/mcp"
  in
  check bool "SSE only is insufficient" false
    (Mcp_http_helpers.Request.accepts_sse request)

(* ============== Mcp_protocol_request: classify_message ============== *)

let test_classify_request () =
  let msg = {|{"jsonrpc":"2.0","id":1,"method":"test","params":{}}|} in
  check bool "is request" true
    (Mcp_protocol_request.classify_message msg = `Request)

let test_classify_notification_no_id () =
  let msg = {|{"jsonrpc":"2.0","method":"notify","params":{}}|} in
  check bool "is notification" true
    (Mcp_protocol_request.classify_message msg = `Notification)

let test_classify_notification_null_id () =
  let msg = {|{"jsonrpc":"2.0","id":null,"method":"notify","params":{}}|} in
  check bool "null id notification" true
    (Mcp_protocol_request.classify_message msg = `Notification)

let test_classify_response_result () =
  let msg = {|{"jsonrpc":"2.0","id":1,"result":{"data":"ok"}}|} in
  check bool "is response" true
    (Mcp_protocol_request.classify_message msg = `Response)

let test_classify_response_error () =
  let msg =
    {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"bad"}}|}
  in
  check bool "error response" true
    (Mcp_protocol_request.classify_message msg = `Response)

let test_classify_unknown_no_method () =
  let msg = {|{"jsonrpc":"2.0"}|} in
  check bool "unknown" true
    (Mcp_protocol_request.classify_message msg = `Unknown)

let test_classify_invalid_json () =
  check bool "invalid json" true
    (Mcp_protocol_request.classify_message "not json" = `Unknown)

let test_classify_non_object () =
  check bool "array" true
    (Mcp_protocol_request.classify_message "[1,2]" = `Unknown)

let test_classify_empty () =
  check bool "empty" true
    (Mcp_protocol_request.classify_message "" = `Unknown)

(* ============== Mcp_sdk_adapter_figma: pure conversions ============== *)

let test_sdk_tool_of_local () =
  let tool : Figma_mcp_protocol.tool_def =
    {
      name = "test_tool";
      description = "A test tool";
      input_schema = `Assoc [ ("type", `String "object") ];
    }
  in
  let sdk_tool = Mcp_sdk_adapter_figma.sdk_tool_of_local tool in
  check string "name" "test_tool" sdk_tool.name;
  check (option string) "desc" (Some "A test tool") sdk_tool.description;
  check bool "no title" true (sdk_tool.title = None);
  check bool "no annotations" true (sdk_tool.annotations = None)

let test_sdk_resource_of_local () =
  let resource : Figma_mcp_protocol.mcp_resource =
    {
      uri = "figma://docs/test";
      name = "Test Doc";
      description = "A test resource";
      mime_type = "text/plain";
    }
  in
  let sdk = Mcp_sdk_adapter_figma.sdk_resource_of_local resource in
  check string "uri" "figma://docs/test" sdk.uri;
  check string "name" "Test Doc" sdk.name;
  check (option string) "desc" (Some "A test resource") sdk.description;
  check (option string) "mime" (Some "text/plain") sdk.mime_type

let test_sdk_resource_template_of_local () =
  let template : Figma_mcp_protocol.mcp_resource_template =
    {
      uri_template = "figma://tokens/{file_key}";
      name = "Tokens";
      description = "Design tokens";
      mime_type = "application/json";
    }
  in
  let sdk =
    Mcp_sdk_adapter_figma.sdk_resource_template_of_local template
  in
  check string "uri_template" "figma://tokens/{file_key}"
    sdk.uri_template;
  check string "name" "Tokens" sdk.name;
  check (option string) "desc" (Some "Design tokens") sdk.description;
  check (option string) "mime" (Some "application/json") sdk.mime_type

let test_sdk_prompt_of_local () =
  let prompt : Figma_mcp_protocol.mcp_prompt =
    {
      name = "test_prompt";
      description = "A test prompt";
      text = "Hello {{name}}";
      arguments =
        [
          {
            Figma_mcp_protocol.name = "name";
            description = "The name";
            required = true;
          };
        ];
    }
  in
  let sdk = Mcp_sdk_adapter_figma.sdk_prompt_of_local prompt in
  check string "name" "test_prompt" sdk.name;
  check (option string) "desc" (Some "A test prompt") sdk.description;
  check bool "has args" true (sdk.arguments <> None)

let test_normalize_tool_result_text () =
  let json =
    `Assoc
      [
        ( "content",
          `List
            [
              `Assoc
                [
                  ("type", `String "text");
                  ("text", `String "hello");
                ];
            ] );
      ]
  in
  let result = Mcp_sdk_adapter_figma.normalize_local_tool_result json in
  check bool "not error" true (result.is_error <> Some true)

let test_normalize_tool_result_top_error () =
  let json =
    `Assoc
      [
        ("isError", `Bool true);
        ( "content",
          `List
            [
              `Assoc
                [
                  ("type", `String "text");
                  ("text", `String "fail");
                ];
            ] );
      ]
  in
  let result = Mcp_sdk_adapter_figma.normalize_local_tool_result json in
  check bool "is error" true (result.is_error = Some true)

let test_normalize_tool_result_item_error () =
  let json =
    `Assoc
      [
        ( "content",
          `List
            [
              `Assoc
                [
                  ("type", `String "text");
                  ("text", `String "fail");
                  ("isError", `Bool true);
                ];
            ] );
      ]
  in
  let result = Mcp_sdk_adapter_figma.normalize_local_tool_result json in
  check bool "item error propagated" true (result.is_error = Some true)

let test_normalize_tool_result_no_content () =
  let json = `Assoc [ ("data", `String "raw") ] in
  let result = Mcp_sdk_adapter_figma.normalize_local_tool_result json in
  check bool "not error for fallback" true (result.is_error <> Some true)

let test_normalize_tool_result_empty_content () =
  let json = `Assoc [ ("content", `List []) ] in
  let result = Mcp_sdk_adapter_figma.normalize_local_tool_result json in
  check bool "empty content handled" true (result.is_error <> Some true)

let test_render_prompt_text () =
  let prompt : Figma_mcp_protocol.mcp_prompt =
    {
      name = "test";
      description = "test";
      text = "Hello {{name}}, welcome to {{place}}";
      arguments = [];
    }
  in
  let result =
    Mcp_sdk_adapter_figma.render_prompt_text prompt
      [ ("name", "World"); ("place", "OCaml") ]
  in
  check string "rendered" "Hello World, welcome to OCaml" result

let test_render_prompt_text_no_args () =
  let prompt : Figma_mcp_protocol.mcp_prompt =
    {
      name = "test";
      description = "test";
      text = "No placeholders here";
      arguments = [];
    }
  in
  let result = Mcp_sdk_adapter_figma.render_prompt_text prompt [] in
  check string "unchanged" "No placeholders here" result

let test_find_direct_handler_parse_url () =
  check bool "parse_url found" true
    (Mcp_sdk_adapter_figma.find_direct_handler "figma_parse_url" <> None)

let test_find_direct_handler_cache_stats () =
  check bool "cache_stats found" true
    (Mcp_sdk_adapter_figma.find_direct_handler "figma_cache_stats" <> None)

let test_find_direct_handler_doctor () =
  check bool "doctor found" true
    (Mcp_sdk_adapter_figma.find_direct_handler "figma_doctor" <> None)

let test_find_direct_handler_unknown () =
  check bool "unknown not found" true
    (Mcp_sdk_adapter_figma.find_direct_handler "nonexistent" = None)

let test_noop_context () =
  let ctx = Mcp_sdk_adapter_figma.noop_context in
  check bool "notification ok" true
    (ctx.send_notification ~method_:"test" ~params:None = Ok ());
  check bool "log ok" true
    (ctx.send_log Mcp_protocol.Logging.Info "test" = Ok ());
  check bool "progress ok" true
    (ctx.send_progress ~token:(Mcp_protocol.Mcp_result.String_token "t") ~progress:0.5 ~total:None
    = Ok ());
  check bool "roots ok" true
    (ctx.request_roots_list () = Ok [])

(* ============== Figma_planning_context: pure tree helpers ============== *)

let test_layout_mode_to_string_none () =
  check string "none" "none"
    (Figma_planning_context.layout_mode_to_string None')

let test_layout_mode_to_string_horizontal () =
  check string "horizontal" "horizontal"
    (Figma_planning_context.layout_mode_to_string Horizontal)

let test_layout_mode_to_string_vertical () =
  check string "vertical" "vertical"
    (Figma_planning_context.layout_mode_to_string Vertical)

let test_bbox_to_json_none () =
  check bool "null" true
    (Figma_planning_context.bbox_to_json None = `Null)

let test_bbox_to_json_some () =
  let result =
    Figma_planning_context.bbox_to_json
      (Some { x = 1.0; y = 2.0; width = 100.0; height = 50.0 })
  in
  match result with
  | `Assoc fields ->
      check int "4 fields" 4 (List.length fields)
  | _ -> fail "expected Assoc"

let test_text_excerpt_none () =
  let node = make_node ~characters:None () in
  check (option string) "none" None
    (Figma_planning_context.text_excerpt node)

let test_text_excerpt_empty () =
  let node = make_node ~characters:(Some "   ") () in
  check (option string) "empty trimmed" None
    (Figma_planning_context.text_excerpt node)

let test_text_excerpt_short () =
  let node = make_node ~characters:(Some "Hello World") () in
  check (option string) "short" (Some "Hello World")
    (Figma_planning_context.text_excerpt node)

let test_text_excerpt_long () =
  let long_text = String.make 200 'a' in
  let node = make_node ~characters:(Some long_text) () in
  match Figma_planning_context.text_excerpt node with
  | Some excerpt ->
      check bool "truncated" true (String.length excerpt < 200);
      check bool "has ellipsis" true (String.length excerpt = 123)
  | None -> fail "expected Some"

let test_flatten_tree_single () =
  let node = make_node ~id:"root" () in
  let flats = Figma_planning_context.flatten_tree node in
  check int "single node" 1 (List.length flats);
  let first = List.hd flats in
  check string "root id" "root" first.node.id;
  check int "depth 0" 0 first.depth;
  check bool "no parent" true (first.parent_node_id = None)

let test_flatten_tree_nested () =
  let child1 = make_node ~id:"c1" ~name:"Child1" () in
  let child2 = make_node ~id:"c2" ~name:"Child2" () in
  let root = make_node ~id:"root" ~children:[ child1; child2 ] () in
  let flats = Figma_planning_context.flatten_tree root in
  check int "3 nodes" 3 (List.length flats);
  let c1 =
    List.find (fun (f : Figma_planning_context.flat_node) -> f.node.id = "c1") flats
  in
  check int "child depth" 1 c1.depth;
  check (option string) "parent" (Some "root") c1.parent_node_id

let test_edge_json_root () =
  let flat : Figma_planning_context.flat_node =
    { node = make_node (); parent_node_id = None; depth = 0 }
  in
  check bool "no edge for root" true
    (Figma_planning_context.edge_json flat = None)

let test_edge_json_child () =
  let flat : Figma_planning_context.flat_node =
    { node = make_node ~id:"c1" (); parent_node_id = Some "root"; depth = 1 }
  in
  match Figma_planning_context.edge_json flat with
  | Some (`Assoc fields) ->
      check int "3 fields" 3 (List.length fields)
  | _ -> fail "expected Some Assoc"

let test_bundle_refs_json () =
  let result =
    Figma_planning_context.bundle_refs_json ~file_key:"ABC" ~node_id:"1:2"
      ~summary_depth:3
  in
  match result with
  | `List items -> check int "2 refs" 2 (List.length items)
  | _ -> fail "expected List"

let test_root_summary_json () =
  let root =
    make_node ~id:"r1" ~name:"Root" ~layout_mode:Vertical
      ~bbox:(make_bbox 320. 180.) ~children:[ make_node () ]
      ()
  in
  let result = Figma_planning_context.root_summary_json root in
  match result with
  | `Assoc fields ->
      check int "6 fields" 6 (List.length fields)
  | _ -> fail "expected Assoc"

let test_aggregate_type_counts () =
  let flat1 : Figma_planning_context.flat_node =
    { node = make_node ~node_type:Frame (); parent_node_id = None; depth = 0 }
  in
  let flat2 : Figma_planning_context.flat_node =
    { node = make_node ~node_type:Text (); parent_node_id = None; depth = 0 }
  in
  let flat3 : Figma_planning_context.flat_node =
    { node = make_node ~node_type:Frame (); parent_node_id = None; depth = 0 }
  in
  let counts =
    Figma_planning_context.aggregate_type_counts [ flat1; flat2; flat3 ]
  in
  check int "2 types" 2 (List.length counts)

let test_feature_summary_json () =
  let text_node = make_node ~node_type:Text ~characters:(Some "hello") () in
  let invis_node = make_node ~visible:false () in
  let flat1 : Figma_planning_context.flat_node =
    { node = text_node; parent_node_id = None; depth = 1 }
  in
  let flat2 : Figma_planning_context.flat_node =
    { node = invis_node; parent_node_id = None; depth = 2 }
  in
  let result =
    Figma_planning_context.feature_summary_json ~all_nodes:[ flat1; flat2 ]
      ~candidate_count:1 ~notes_count:0 ~excluded_count:0
  in
  match result with
  | `Assoc fields ->
      check bool "has total_nodes" true (List.mem_assoc "total_nodes" fields);
      check bool "has max_depth" true (List.mem_assoc "max_depth" fields)
  | _ -> fail "expected Assoc"

let test_build_context_json () =
  let child = make_node ~id:"c1" ~name:"Child" ~bbox:(make_bbox 100. 50.) () in
  let root =
    make_node ~id:"root" ~name:"Root" ~bbox:(make_bbox 320. 180.)
      ~children:[ child ] ()
  in
  let result =
    Figma_planning_context.build_context_json ~file_key:"ABC" ~node_id:"root"
      ~summary_depth:2 ~preview_json:`Null root
  in
  match result with
  | `Assoc fields ->
      check bool "has file_key" true (List.mem_assoc "file_key" fields);
      check bool "has candidates" true (List.mem_assoc "candidates" fields);
      check bool "has feature_summary" true
        (List.mem_assoc "feature_summary" fields)
  | _ -> fail "expected Assoc"

let test_build_context_json_with_patterns () =
  let note_node =
    make_node ~id:"n1" ~name:"memo_note" ~node_type:Text
      ~characters:(Some "This is a memo") ()
  in
  let excl_node = make_node ~id:"e1" ~name:"guide_line" () in
  let root =
    make_node ~id:"root" ~children:[ note_node; excl_node ] ()
  in
  let result =
    Figma_planning_context.build_context_json
      ~note_patterns:[ "memo" ]
      ~exclude_patterns:[ "guide" ]
      ~file_key:"X" ~node_id:"root" ~summary_depth:2 ~preview_json:`Null root
  in
  match result with
  | `Assoc fields ->
      let notes =
        match List.assoc_opt "notes" fields with
        | Some (`List l) -> List.length l
        | _ -> 0
      in
      let excluded =
        match List.assoc_opt "excluded" fields with
        | Some (`List l) -> List.length l
        | _ -> 0
      in
      check bool "has notes" true (notes > 0);
      check bool "has excluded" true (excluded > 0)
  | _ -> fail "expected Assoc"

(* ============== Figma_agent_plan_validator: helper functions ============== *)

let test_validator_get_string_field_found () =
  let fields = [ ("name", `String "test") ] in
  check (option string) "found" (Some "test")
    (Figma_agent_plan_validator.get_string_field "name" fields)

let test_validator_get_string_field_empty () =
  let fields = [ ("name", `String "   ") ] in
  check (option string) "empty trimmed" None
    (Figma_agent_plan_validator.get_string_field "name" fields)

let test_validator_get_string_field_non_string () =
  let fields = [ ("name", `Int 42) ] in
  check (option string) "non-string" None
    (Figma_agent_plan_validator.get_string_field "name" fields)

let test_validator_get_string_list_field_present () =
  let fields = [ ("deps", `List [ `String "a"; `String "b" ]) ] in
  match Figma_agent_plan_validator.get_string_list_field "deps" fields with
  | Some l -> check int "2 items" 2 (List.length l)
  | None -> fail "expected Some"

let test_validator_get_string_list_field_missing () =
  match Figma_agent_plan_validator.get_string_list_field "deps" [] with
  | Some l -> check int "empty default" 0 (List.length l)
  | None -> fail "expected Some []"

let test_validator_get_string_list_field_wrong_type () =
  let fields = [ ("deps", `String "not a list") ] in
  check bool "none for wrong type" true
    (Figma_agent_plan_validator.get_string_list_field "deps" fields = None)

let test_validator_get_string_list_field_filters () =
  let fields =
    [ ("deps", `List [ `String "a"; `Int 1; `String ""; `String "b" ]) ]
  in
  match Figma_agent_plan_validator.get_string_list_field "deps" fields with
  | Some l -> check int "filtered to 2" 2 (List.length l)
  | None -> fail "expected Some"

let test_validator_append_unique () =
  let target = ref [ "a" ] in
  Figma_agent_plan_validator.append_unique target "b";
  check int "added" 2 (List.length !target);
  Figma_agent_plan_validator.append_unique target "a";
  check int "dedup" 2 (List.length !target)

let test_validate_json_valid_plan () =
  let root =
    make_node ~id:"1:1" ~name:"Root"
      ~children:[ make_node ~id:"1:2" ~name:"Child" () ]
      ()
  in
  let plan =
    `Assoc
      [
        ("root_node_id", `String "1:1");
        ( "tasks",
          `List
            [
              `Assoc
                [
                  ("id", `String "t1");
                  ("node_id", `String "1:2");
                  ("title", `String "Task 1");
                  ("depends_on", `List []);
                ];
            ] );
      ]
  in
  let result =
    Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root plan
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "valid" fields with
       | Some (`Bool true) -> ()
       | _ -> fail "expected valid=true")
  | _ -> fail "expected Assoc"

let test_validate_json_not_object () =
  let root = make_node () in
  let result =
    Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root
      (`String "bad")
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "valid" fields with
       | Some (`Bool false) -> ()
       | _ -> fail "expected valid=false")
  | _ -> fail "expected Assoc"

let test_validate_json_missing_tasks () =
  let root = make_node () in
  let plan = `Assoc [ ("root_node_id", `String "1:1") ] in
  let result =
    Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root plan
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "valid" fields with
       | Some (`Bool false) -> ()
       | _ -> fail "expected valid=false")
  | _ -> fail "expected Assoc"

let test_validate_json_tasks_not_list () =
  let root = make_node () in
  let plan =
    `Assoc [ ("tasks", `String "not a list") ]
  in
  let result =
    Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root plan
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "valid" fields with
       | Some (`Bool false) -> ()
       | _ -> fail "expected valid=false")
  | _ -> fail "expected Assoc"

let test_validate_json_self_dependency () =
  let root =
    make_node ~id:"1:1" ~children:[ make_node ~id:"1:2" () ] ()
  in
  let plan =
    `Assoc
      [
        ( "tasks",
          `List
            [
              `Assoc
                [
                  ("id", `String "t1");
                  ("node_id", `String "1:2");
                  ("depends_on", `List [ `String "t1" ]);
                ];
            ] );
      ]
  in
  let result =
    Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root plan
  in
  match result with
  | `Assoc fields ->
      let errors =
        match List.assoc_opt "errors" fields with
        | Some (`List l) -> l
        | _ -> []
      in
      check bool "has self-dep error" true
        (List.exists
           (function
             | `String s -> String.length s > 0 && (try ignore (Str.search_forward (Str.regexp_string "self_dependency") s 0); true with Not_found -> false)
             | _ -> false)
           errors)
  | _ -> fail "expected Assoc"

let test_validate_json_duplicate_task_id () =
  let root =
    make_node ~id:"1:1"
      ~children:[ make_node ~id:"1:2" (); make_node ~id:"1:3" () ]
      ()
  in
  let plan =
    `Assoc
      [
        ( "tasks",
          `List
            [
              `Assoc
                [
                  ("id", `String "t1");
                  ("node_id", `String "1:2");
                  ("depends_on", `List []);
                ];
              `Assoc
                [
                  ("id", `String "t1");
                  ("node_id", `String "1:3");
                  ("depends_on", `List []);
                ];
            ] );
      ]
  in
  let result =
    Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root plan
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "valid" fields with
       | Some (`Bool false) -> ()
       | _ -> fail "expected valid=false for duplicate ids")
  | _ -> fail "expected Assoc"

let test_validate_json_empty_tasks () =
  let root = make_node () in
  let plan = `Assoc [ ("tasks", `List []) ] in
  let result =
    Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root plan
  in
  match result with
  | `Assoc fields ->
      (match List.assoc_opt "valid" fields with
       | Some (`Bool false) -> ()
       | _ -> fail "expected valid=false for empty tasks")
  | _ -> fail "expected Assoc"

(* ============== Telemetry: expand_tilde ============== *)

let test_expand_tilde_with_home () =
  let result = Telemetry_jsonl.expand_tilde "~/test/path" in
  check bool "expanded" true (not (String.length result > 0 && result.[0] = '~'))

let test_expand_tilde_no_tilde () =
  let result = Telemetry_jsonl.expand_tilde "/absolute/path" in
  check string "unchanged" "/absolute/path" result

let test_expand_tilde_empty () =
  let result = Telemetry_jsonl.expand_tilde "" in
  check string "empty unchanged" "" result

(* ============== Test Runner ============== *)

let () =
  run "Coverage Push v0.9.0"
    [
      ( "Mcp_planning_handlers",
        [
          test_case "clamp depth default" `Quick test_clamp_summary_depth_default;
          test_case "clamp depth provided" `Quick test_clamp_summary_depth_provided;
          test_case "clamp depth negative" `Quick test_clamp_summary_depth_negative;
          test_case "clamp depth too large" `Quick test_clamp_summary_depth_too_large;
          test_case "clamp depth zero" `Quick test_clamp_summary_depth_zero;
          test_case "clamp depth exact max" `Quick test_clamp_summary_depth_exact_max;
          test_case "clamp scale normal" `Quick test_clamp_preview_scale_normal;
          test_case "clamp scale too small" `Quick test_clamp_preview_scale_too_small;
          test_case "clamp scale too large" `Quick test_clamp_preview_scale_too_large;
          test_case "clamp scale boundary low" `Quick test_clamp_preview_scale_boundary_low;
          test_case "clamp scale boundary high" `Quick test_clamp_preview_scale_boundary_high;
          test_case "resolve plan present" `Quick test_resolve_plan_present;
          test_case "resolve plan missing" `Quick test_resolve_plan_missing;
          test_case "resolve plan null" `Quick test_resolve_plan_null;
        ] );
      ( "Mcp_http_helpers",
        [
          test_case "parse_json valid" `Quick test_parse_json_valid;
          test_case "parse_json empty" `Quick test_parse_json_empty;
          test_case "parse_json whitespace" `Quick test_parse_json_whitespace;
          test_case "parse_json invalid" `Quick test_parse_json_invalid;
          test_case "parse_json array" `Quick test_parse_json_array;
          test_case "get_string_field present" `Quick test_get_string_field_present;
          test_case "get_string_field missing" `Quick test_get_string_field_missing;
          test_case "get_string_field wrong type" `Quick test_get_string_field_wrong_type;
          test_case "get_string_field non-object" `Quick test_get_string_field_non_object;
          test_case "get_int_field int" `Quick test_get_int_field_int;
          test_case "get_int_field float" `Quick test_get_int_field_float;
          test_case "get_int_field missing" `Quick test_get_int_field_missing;
          test_case "get_int_field wrong type" `Quick test_get_int_field_wrong_type;
          test_case "get_int_field non-object" `Quick test_get_int_field_non_object;
          test_case "get_bool_field true" `Quick test_get_bool_field_true;
          test_case "get_bool_field false" `Quick test_get_bool_field_false;
          test_case "get_bool_field missing" `Quick test_get_bool_field_missing;
          test_case "get_bool_field wrong type" `Quick test_get_bool_field_wrong_type;
          test_case "get_bool_field non-object" `Quick test_get_bool_field_non_object;
          test_case "get_payload_field present" `Quick test_get_payload_field_present;
          test_case "get_payload_field missing" `Quick test_get_payload_field_missing;
          test_case "get_payload_field non-object" `Quick test_get_payload_field_non_object;
          test_case "parse_positive_int valid" `Quick test_parse_positive_int_valid;
          test_case "parse_positive_int zero" `Quick test_parse_positive_int_zero;
          test_case "parse_positive_int negative" `Quick test_parse_positive_int_negative;
          test_case "parse_positive_int invalid" `Quick test_parse_positive_int_invalid;
          test_case "parse_positive_int empty" `Quick test_parse_positive_int_empty;
          test_case "request path" `Quick test_request_path;
          test_case "request path no query" `Quick test_request_path_no_query;
          test_case "request method" `Quick test_request_method;
          test_case "accepts_sse true" `Quick test_accepts_sse_true;
          test_case "accepts_sse false" `Quick test_accepts_sse_false;
          test_case "accepts_sse no header" `Quick test_accepts_sse_no_header;
          test_case "accepts_sse mixed" `Quick test_accepts_sse_mixed;
          test_case "accepts_sse sse-only false" `Quick test_accepts_sse_sse_only_false;
        ] );
      ( "Mcp_protocol_request",
        [
          test_case "classify request" `Quick test_classify_request;
          test_case "classify notification no id" `Quick test_classify_notification_no_id;
          test_case "classify notification null id" `Quick test_classify_notification_null_id;
          test_case "classify response result" `Quick test_classify_response_result;
          test_case "classify response error" `Quick test_classify_response_error;
          test_case "classify unknown" `Quick test_classify_unknown_no_method;
          test_case "classify invalid json" `Quick test_classify_invalid_json;
          test_case "classify non-object" `Quick test_classify_non_object;
          test_case "classify empty" `Quick test_classify_empty;
        ] );
      ( "Mcp_sdk_adapter_figma",
        [
          test_case "sdk_tool_of_local" `Quick test_sdk_tool_of_local;
          test_case "sdk_resource_of_local" `Quick test_sdk_resource_of_local;
          test_case "sdk_resource_template" `Quick test_sdk_resource_template_of_local;
          test_case "sdk_prompt_of_local" `Quick test_sdk_prompt_of_local;
          test_case "normalize result text" `Quick test_normalize_tool_result_text;
          test_case "normalize result top error" `Quick test_normalize_tool_result_top_error;
          test_case "normalize result item error" `Quick test_normalize_tool_result_item_error;
          test_case "normalize result no content" `Quick test_normalize_tool_result_no_content;
          test_case "normalize result empty content" `Quick test_normalize_tool_result_empty_content;
          test_case "render prompt text" `Quick test_render_prompt_text;
          test_case "render prompt no args" `Quick test_render_prompt_text_no_args;
          test_case "find direct parse_url" `Quick test_find_direct_handler_parse_url;
          test_case "find direct cache_stats" `Quick test_find_direct_handler_cache_stats;
          test_case "find direct doctor" `Quick test_find_direct_handler_doctor;
          test_case "find direct unknown" `Quick test_find_direct_handler_unknown;
          test_case "noop context" `Quick test_noop_context;
        ] );
      ( "Figma_planning_context",
        [
          test_case "layout_mode none" `Quick test_layout_mode_to_string_none;
          test_case "layout_mode horizontal" `Quick test_layout_mode_to_string_horizontal;
          test_case "layout_mode vertical" `Quick test_layout_mode_to_string_vertical;
          test_case "bbox_to_json none" `Quick test_bbox_to_json_none;
          test_case "bbox_to_json some" `Quick test_bbox_to_json_some;
          test_case "text_excerpt none" `Quick test_text_excerpt_none;
          test_case "text_excerpt empty" `Quick test_text_excerpt_empty;
          test_case "text_excerpt short" `Quick test_text_excerpt_short;
          test_case "text_excerpt long" `Quick test_text_excerpt_long;
          test_case "flatten single" `Quick test_flatten_tree_single;
          test_case "flatten nested" `Quick test_flatten_tree_nested;
          test_case "edge_json root" `Quick test_edge_json_root;
          test_case "edge_json child" `Quick test_edge_json_child;
          test_case "bundle_refs" `Quick test_bundle_refs_json;
          test_case "root_summary" `Quick test_root_summary_json;
          test_case "aggregate_type_counts" `Quick test_aggregate_type_counts;
          test_case "feature_summary" `Quick test_feature_summary_json;
          test_case "build_context" `Quick test_build_context_json;
          test_case "build_context with patterns" `Quick test_build_context_json_with_patterns;
        ] );
      ( "Figma_agent_plan_validator",
        [
          test_case "get_string_field found" `Quick test_validator_get_string_field_found;
          test_case "get_string_field empty" `Quick test_validator_get_string_field_empty;
          test_case "get_string_field non-string" `Quick test_validator_get_string_field_non_string;
          test_case "get_string_list_field present" `Quick test_validator_get_string_list_field_present;
          test_case "get_string_list_field missing" `Quick test_validator_get_string_list_field_missing;
          test_case "get_string_list_field wrong type" `Quick test_validator_get_string_list_field_wrong_type;
          test_case "get_string_list_field filters" `Quick test_validator_get_string_list_field_filters;
          test_case "append_unique" `Quick test_validator_append_unique;
          test_case "validate valid plan" `Quick test_validate_json_valid_plan;
          test_case "validate not object" `Quick test_validate_json_not_object;
          test_case "validate missing tasks" `Quick test_validate_json_missing_tasks;
          test_case "validate tasks not list" `Quick test_validate_json_tasks_not_list;
          test_case "validate self dependency" `Quick test_validate_json_self_dependency;
          test_case "validate duplicate id" `Quick test_validate_json_duplicate_task_id;
          test_case "validate empty tasks" `Quick test_validate_json_empty_tasks;
        ] );
      ( "Telemetry",
        [
          test_case "expand_tilde with home" `Quick test_expand_tilde_with_home;
          test_case "expand_tilde no tilde" `Quick test_expand_tilde_no_tilde;
          test_case "expand_tilde empty" `Quick test_expand_tilde_empty;
        ] );
    ]
