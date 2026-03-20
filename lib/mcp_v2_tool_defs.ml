open Figma_mcp_protocol
open Mcp_helpers

let common_node_ref_props =
  [
    ("url", string_prop "Figma URL. If provided, file_key and node_id are derived from it.");
    ("file_key", string_prop "Figma file key.");
    ("node_id", string_prop "Figma node id in API format (123:456).");
  ]

let tool_figma_get_design_context : tool_def =
  {
    name = "figma_get_design_context";
    description =
      "Return fidelity-first design context for a Figma node. This is the v2 entrypoint for design-to-code workflows.";
    input_schema =
      object_schema
        (common_node_ref_props
        @ [
            ("client_frameworks", string_prop "Optional client framework label, such as React or SwiftUI.");
            ("client_languages", string_prop "Optional client language hint.");
            ("depth", number_prop "Optional node fetch depth.");
            ("include_variables", bool_prop "Include resolved variable definitions. Default: true.");
            ("include_code_connect", bool_prop "Attach Code Connect match information when a mapping is available. Default: false.");
            ("plugin_channel_id", string_prop "Optional plugin channel id for desktop enrichment.");
            ("version", string_prop "Optional Figma file version.");
          ])
        [];
  }

let tool_figma_get_metadata : tool_def =
  {
    name = "figma_get_metadata";
    description =
      "Return sparse XML metadata for a Figma node or file. Use this to inspect large selections before fetching full context.";
    input_schema =
      object_schema
        (common_node_ref_props
        @ [
            ("depth", number_prop "Max tree depth to include. Default: 2.");
            ("max_children", number_prop "Max children per node before truncation. Default: 100.");
            ("version", string_prop "Optional Figma file version.");
          ])
        [];
  }

let tool_figma_get_variable_defs : tool_def =
  {
    name = "figma_get_variable_defs";
    description =
      "Return variable and style definitions used by a Figma file, optimized for design-token consumption.";
    input_schema =
      object_schema
        [
          ("url", string_prop "Optional Figma URL used only to derive file_key.");
          ("file_key", string_prop "Figma file key.");
          ("format", enum_prop [ "summary"; "raw"; "resolved" ] "Output mode. Default: resolved.");
        ]
        [];
  }

let tool_figma_get_screenshot : tool_def =
  {
    name = "figma_get_screenshot";
    description =
      "Return export URLs or downloaded screenshot paths for a Figma node.";
    input_schema =
      object_schema
        (common_node_ref_props
        @ [
            ("format", enum_prop [ "png"; "jpg"; "svg"; "pdf" ] "Export format. Default: png.");
            ("scale", number_prop "Export scale. Default: 1.");
            ("download", bool_prop "Download the screenshot to disk. Default: false.");
            ("save_dir", string_prop "Directory used when download=true.");
            ("use_absolute_bounds", bool_prop "Use absolute bounds when exporting.");
            ("version", string_prop "Optional Figma file version.");
          ])
        [];
  }

let tool_figma_get_code_connect_map : tool_def =
  {
    name = "figma_get_code_connect_map";
    description =
      "Resolve Code Connect mappings from the local template/index configuration.";
    input_schema =
      object_schema
        [
          ("mode", enum_prop [ "index"; "match"; "validate"; "list" ] "Optional explicit mode. Default: match when a selector exists, otherwise index.");
          ("path", string_prop "Optional mapping file path. Defaults to ./figma-code-connect.json or ./.figma/code-connect.json.");
          ("json", string_prop "Optional inline mapping JSON.");
          ("index_id", string_prop "Optional cached index id from a previous index run.");
          ("node_id", string_prop "Match selector: Figma node id.");
          ("component_key", string_prop "Match selector: Figma component key.");
          ("name", string_prop "Match selector: Figma component name.");
          ("limit", number_prop "Max matches to return. Default: 5.");
        ]
        [];
  }

let tool_figma_whoami : tool_def =
  {
    name = "figma_whoami";
    description = "Return the authenticated Figma user.";
    input_schema = object_schema [] [];
  }

let tool_figma_verify_semantic : tool_def =
  {
    name = "figma_verify_semantic";
    description =
      "Verify HTML against Figma semantics using layout, typography, and style metrics.";
    input_schema =
      object_schema
        (common_node_ref_props
        @ [
            ("html", string_prop "Rendered HTML to verify.");
            ("width", number_prop "Viewport width. Default: 375.");
            ("height", number_prop "Viewport height. Default: 812.");
            ("score_threshold", number_prop "Passing score threshold.");
            ("version", string_prop "Optional Figma file version.");
          ])
        [ "html" ];
  }

let tool_figma_verify_visual : tool_def =
  {
    name = "figma_verify_visual";
    description =
      "Verify rendered HTML against a Figma screenshot using SSIM and text validation.";
    input_schema =
      object_schema
        (common_node_ref_props
        @ [
            ("html", string_prop "Rendered HTML to verify.");
            ("html_screenshot", string_prop "Optional pre-rendered screenshot path.");
            ("target_ssim", number_prop "Target SSIM score. Default: 0.95.");
            ("max_iterations", number_prop "Max corrective iterations. Default: 3.");
            ("width", number_prop "Viewport width. Default: 375.");
            ("height", number_prop "Viewport height. Default: 812.");
            ("version", string_prop "Optional Figma file version.");
          ])
        [ "html" ];
  }

let public_tools : tool_def list =
  [
    tool_figma_get_design_context;
    tool_figma_get_metadata;
    tool_figma_get_variable_defs;
    tool_figma_get_screenshot;
    tool_figma_get_code_connect_map;
    tool_figma_whoami;
    tool_figma_verify_semantic;
    tool_figma_verify_visual;
  ]
