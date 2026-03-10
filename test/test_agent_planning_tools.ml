open Alcotest
open Figma_types

let assoc_field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let string_list_field key json =
  match assoc_field key json with
  | Some (`List items) ->
      items
      |> List.filter_map (function `String s -> Some s | _ -> None)
  | _ -> []

let list_field key json =
  match assoc_field key json with
  | Some (`List items) -> items
  | _ -> []

let make_bbox x y width height = Some { x; y; width; height }

let make_frame ?(children = []) ?(layout_mode = Horizontal) id name =
  {
    default_node with
    id;
    name;
    node_type = Frame;
    layout_mode;
    bbox = make_bbox 0. 0. 320. 180.;
    children;
  }

let make_text id name text =
  {
    default_node with
    id;
    name;
    node_type = Text;
    bbox = make_bbox 0. 0. 120. 24.;
    characters = Some text;
    typography = Some default_typography;
  }

let make_rect id name =
  {
    default_node with
    id;
    name;
    node_type = Rectangle;
    bbox = make_bbox 0. 0. 80. 80.;
  }

let contains_substring ~needle haystack =
  let re = Str.regexp_string needle in
  try
    ignore (Str.search_forward re haystack 0);
    true
  with Not_found -> false

let test_public_tools_include_agent_planning () =
  let names = List.map (fun (tool : Figma_mcp_protocol.tool_def) -> tool.name) Mcp_tool_defs.public_tools in
  check bool "has planning context tool" true (List.mem "figma_get_planning_context" names);
  check bool "has validate plan tool" true (List.mem "figma_validate_agent_plan" names)

let test_core_category_hides_legacy_select_nodes () =
  match List.find_opt (fun (cat : Mcp_tools.tool_category) -> cat.name = "core") Mcp_tools.tool_categories with
  | None -> fail "missing core category"
  | Some cat ->
      check bool "legacy select_nodes hidden from core category" false
        (List.mem "select_nodes" cat.tools);
      check bool "planning context in core category" true
        (List.mem "get_planning_context" cat.tools);
      check bool "validate plan in core category" true
        (List.mem "validate_agent_plan" cat.tools)

let test_legacy_select_nodes_blocked_by_default () =
  Unix.putenv "FIGMA_MCP_ENABLE_LEGACY_SELECT_NODES" "";
  match Mcp_api_handlers.handle_select_nodes (`Assoc []) with
  | Error msg ->
      check bool "deprecation error" true
        (contains_substring ~needle:"figma_select_nodes is disabled by default" msg)
  | Ok _ -> fail "expected legacy select_nodes to be blocked"

let test_planning_context_is_agent_first () =
  let keep = make_frame "1:2" "Card" in
  let note = make_text "1:3" "Note Layer" "Implementation note" in
  let excluded = make_rect "1:4" "Guide Box" in
  let root = make_frame ~children:[keep; note; excluded] "1:1" "Screen" in
  let json =
    Figma_planning_context.build_context_json
      ~note_patterns:["note"]
      ~exclude_patterns:["guide"]
      ~file_key:"FILE"
      ~node_id:"1:1"
      ~summary_depth:2
      ~preview_json:`Null
      root
  in
  check bool "no score_threshold" false (Option.is_some (assoc_field "score_threshold" json));
  check bool "no selection_mode" false (Option.is_some (assoc_field "selection_mode" json));
  let candidates = list_field "candidates" json in
  check int "candidate count" 1 (List.length candidates);
  let notes = list_field "notes" json in
  check int "note count" 1 (List.length notes);
  let excluded_nodes = list_field "excluded" json in
  check int "excluded count" 1 (List.length excluded_nodes);
  let tree_edges = list_field "tree_edges" json in
  check int "tree edges" 3 (List.length tree_edges);
  match candidates with
  | (`Assoc fields) :: _ ->
      check bool "candidate has no score" false (List.mem_assoc "score" fields);
      check bool "candidate has no priority" false (List.mem_assoc "priority" fields);
      check bool "candidate has feature flags" true (List.mem_assoc "feature_flags" fields)
  | _ -> fail "expected candidate object"

let test_validate_agent_plan_success () =
  let leaf = make_text "1:3" "Headline" "Hello" in
  let parent = make_frame ~children:[leaf] "1:2" "Card" in
  let root = make_frame ~children:[parent] "1:1" "Screen" in
  let plan =
    `Assoc [
      ("root_node_id", `String "1:1");
      ("tasks", `List [
        `Assoc [
          ("id", `String "task-parent");
          ("node_id", `String "1:2");
          ("title", `String "Build card");
        ];
        `Assoc [
          ("id", `String "task-leaf");
          ("node_id", `String "1:3");
          ("depends_on", `List [`String "task-parent"]);
        ];
      ]);
    ]
  in
  let result = Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root plan in
  match assoc_field "valid" result with
  | Some (`Bool true) ->
      check int "task count" 2
        (match assoc_field "task_count" result with Some (`Int n) -> n | _ -> -1);
      check int "resolved nodes" 2 (List.length (list_field "resolved_nodes" result));
      check int "errors empty" 0 (List.length (list_field "errors" result))
  | _ -> fail "expected valid plan"

let test_validate_agent_plan_invalid_shape_and_unknowns () =
  let keep = make_frame "1:2" "Card" in
  let root = make_frame ~children:[keep] "1:1" "Screen" in
  let plan =
    `Assoc [
      ("unknown_top", `Bool true);
      ("tasks", `List [
        `Assoc [
          ("id", `String "dup");
          ("node_id", `String "1:2");
          ("extra", `String "x");
        ];
        `Assoc [
          ("id", `String "dup");
          ("node_id", `String "9:9");
          ("depends_on", `List [`String "missing"]);
        ];
      ]);
    ]
  in
  let result = Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root plan in
  match assoc_field "valid" result with
  | Some (`Bool false) ->
      let warnings = string_list_field "warnings" result in
      let errors = string_list_field "errors" result in
      check bool "warns unknown top field" true
        (List.exists (contains_substring ~needle:"unknown_top_field:unknown_top") warnings);
      check bool "warns unknown task field" true
        (List.exists (contains_substring ~needle:"task[0].unknown_field:extra") warnings);
      check bool "duplicate id error" true
        (List.exists (contains_substring ~needle:"duplicate_task_id:dup") errors);
      check bool "out of root error" true
        (List.exists (contains_substring ~needle:"node_out_of_root:9:9") errors);
      check bool "unknown dependency error" true
        (List.exists (contains_substring ~needle:"unknown_dependency:missing") errors)
  | _ -> fail "expected invalid plan"

let test_validate_agent_plan_cycle () =
  let left = make_frame "1:2" "Left" in
  let right = make_frame "1:3" "Right" in
  let root = make_frame ~children:[left; right] "1:1" "Screen" in
  let plan =
    `Assoc [
      ("tasks", `List [
        `Assoc [
          ("id", `String "task-a");
          ("node_id", `String "1:2");
          ("depends_on", `List [`String "task-b"]);
        ];
        `Assoc [
          ("id", `String "task-b");
          ("node_id", `String "1:3");
          ("depends_on", `List [`String "task-a"]);
        ];
      ]);
    ]
  in
  let result = Figma_agent_plan_validator.validate_json ~root_node_id:"1:1" root plan in
  let errors = string_list_field "errors" result in
  check bool "cycle detected" true
    (List.exists (contains_substring ~needle:"dependency_cycle:task-a") errors
     || List.exists (contains_substring ~needle:"dependency_cycle:task-b") errors)

let () =
  run "agent_planning_tools"
    [
      ("tool_defs", [
        test_case "public tools include agent planning" `Quick
          test_public_tools_include_agent_planning;
        test_case "core category hides legacy select_nodes" `Quick
          test_core_category_hides_legacy_select_nodes;
        test_case "legacy select_nodes blocked by default" `Quick
          test_legacy_select_nodes_blocked_by_default;
      ]);
      ("planning_context", [
        test_case "context is agent first" `Quick test_planning_context_is_agent_first;
      ]);
      ("validator", [
        test_case "valid plan" `Quick test_validate_agent_plan_success;
        test_case "invalid shape and unknowns" `Quick
          test_validate_agent_plan_invalid_shape_and_unknowns;
        test_case "dependency cycle" `Quick test_validate_agent_plan_cycle;
      ]);
    ]
