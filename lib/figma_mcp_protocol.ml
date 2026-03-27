(** MCP Protocol - JSON-RPC 2.0 handler (2025-11-25 spec).

    Types are aliases for mcp_protocol SDK types where possible.
    Only [mcp_prompt] remains local because the SDK [prompt] type
    does not carry a template [text] field. *)

open Printf

(* SDK module aliases *)
module Sdk_error_codes = Mcp_protocol.Error_codes
module Sdk_jsonrpc = Mcp_protocol.Jsonrpc
module Sdk_version = Mcp_protocol.Version
module MT = Mcp_protocol.Mcp_types

(** ============== JSON-RPC types ============== *)

type json_rpc_request = {
  jsonrpc: string;
  id: Yojson.Safe.t option;  (* null, string, or number *)
  method_: string;
  params: Yojson.Safe.t option;
}

type json_rpc_response =
  | RpcSuccess of { id: Yojson.Safe.t; result: Yojson.Safe.t }
  | RpcError of { id: Yojson.Safe.t; code: int; message: string; data: Yojson.Safe.t option }

(** ============== MCP types (SDK aliases) ============== *)

type tool_def = MT.tool
(** SDK [tool] with fields: name, description, input_schema,
    output_schema, title, annotations, icon, execution. *)

(** Convenience constructor matching the old 3-field signature. *)
let make_tool_def ~name ~description ~input_schema : tool_def =
  MT.make_tool ~name ~description ~input_schema ()

type mcp_resource = MT.resource
(** SDK [resource] with fields: uri, name, title, description,
    mime_type, icon. *)

(** Convenience constructor matching the old 4-field signature. *)
let make_resource ~uri ~name ~description ~mime_type : mcp_resource =
  MT.make_resource ~uri ~name ~description ~mime_type ()

type mcp_resource_template = MT.resource_template
(** SDK [resource_template] with fields: uri_template, name, title,
    description, mime_type, icon. *)

(** Convenience constructor matching the old 4-field signature. *)
let make_resource_template ~uri_template ~name ~description ~mime_type
    : mcp_resource_template =
  { uri_template; name; title = None; description = Some description;
    mime_type = Some mime_type; icon = None }

type prompt_arg = MT.prompt_argument
(** SDK [prompt_argument] with fields: name, description, required.
    [description] and [required] are [option]. *)

(** Convenience constructor matching the old 3-field signature. *)
let make_prompt_arg ~name ~description ~required : prompt_arg =
  { MT.name; description = Some description; required = Some required }

(** Local prompt type -- extends SDK [prompt] with a template [text] field. *)
type mcp_prompt = {
  name: string;
  description: string;
  arguments: prompt_arg list;
  text: string;
}

type resource_reader = string -> (string * string, string) result

(** ============== Error codes (JSON-RPC 2.0) ============== *)
let parse_error = Sdk_error_codes.parse_error
let invalid_request = Sdk_error_codes.invalid_request
let method_not_found = Sdk_error_codes.method_not_found
let invalid_params = Sdk_error_codes.invalid_params
let internal_error = Sdk_error_codes.internal_error

(** ============== Server info ============== *)

let supported_protocol_versions = Sdk_version.supported_versions
let default_protocol_version = Sdk_version.latest

let normalize_protocol_version version =
  match Sdk_version.negotiate ~requested:version with
  | Some v -> v
  | None -> default_protocol_version

let protocol_version_from_params params =
  match params with
  | Some (`Assoc lst) ->
      (match List.assoc_opt "protocolVersion" lst with
       | Some (`String v) -> v
       | _ -> default_protocol_version)
  | _ -> default_protocol_version

let protocol_version = default_protocol_version  (* backward compat *)
let server_name = "figma-mcp"
let server_version = Version.version

(** ============== JSON utilities ============== *)

let member key json =
  match json with
  | `Assoc lst -> List.assoc_opt key lst
  | _ -> None

let sdk_id_of_json json =
  match Sdk_jsonrpc.id_of_yojson json with
  | Ok id -> id
  | Error _ -> Sdk_jsonrpc.Null

let parse_request json_str : (json_rpc_request, string) result =
  try
    let json = Yojson.Safe.from_string json_str in
    let jsonrpc = match member "jsonrpc" json with Some (`String s) -> s | _ -> "" in
    let id = member "id" json in
    let method_ = match member "method" json with Some (`String s) -> s | _ -> "" in
    let params = member "params" json in

    if jsonrpc <> "2.0" then
      Result.Error "Invalid JSON-RPC version"
    else if method_ = "" then
      Result.Error "Missing method"
    else
      Ok { jsonrpc; id; method_; params }
  with
  | Yojson.Json_error msg -> Result.Error (sprintf "JSON parse error: %s" msg)
  | _ -> Result.Error "Unknown parse error"

let is_notification_id = function
  | None -> true
  | Some `Null -> true
  | _ -> false

let is_notification req =
  is_notification_id req.id

(** ============== Response builders ============== *)

let make_success_response id result : Yojson.Safe.t =
  Sdk_jsonrpc.make_response_json ~id:(sdk_id_of_json id) ~result

let make_error_response id code message data : Yojson.Safe.t =
  Sdk_jsonrpc.make_error_json ~id:(sdk_id_of_json id) ~code ~message ?data ()

(** ============== Tool definition -> JSON ============== *)

let tool_to_json (tool : tool_def) : Yojson.Safe.t =
  (* P1.4: Auto-detect [DEPRECATED] prefix and add deprecated field *)
  let desc = Option.value tool.description ~default:"" in
  let is_deprecated = String.length desc >= 12 &&
    String.sub desc 0 12 = "[DEPRECATED]" in
  let base = MT.tool_to_yojson tool in
  if is_deprecated then
    match base with
    | `Assoc fields -> `Assoc (fields @ [("deprecated", `Bool true)])
    | other -> other
  else
    base

let resource_to_json (r : mcp_resource) : Yojson.Safe.t =
  (* Custom serializer: MCP spec uses camelCase "mimeType" *)
  let fields = [
    ("uri", `String r.uri);
    ("name", `String r.name);
  ] in
  let fields = match r.title with Some t -> ("title", `String t) :: fields | None -> fields in
  let fields = match r.description with Some d -> ("description", `String d) :: fields | None -> fields in
  let fields = match r.mime_type with Some m -> ("mimeType", `String m) :: fields | None -> fields in
  let fields = match r.icon with Some i -> ("icon", `String i) :: fields | None -> fields in
  `Assoc (List.rev fields)

let resource_template_to_json (t : mcp_resource_template) : Yojson.Safe.t =
  (* Custom serializer: MCP spec uses camelCase "uriTemplate", "mimeType" *)
  let fields = [
    ("uriTemplate", `String t.uri_template);
    ("name", `String t.name);
  ] in
  let fields = match t.title with Some v -> ("title", `String v) :: fields | None -> fields in
  let fields = match t.description with Some d -> ("description", `String d) :: fields | None -> fields in
  let fields = match t.mime_type with Some m -> ("mimeType", `String m) :: fields | None -> fields in
  let fields = match t.icon with Some i -> ("icon", `String i) :: fields | None -> fields in
  `Assoc (List.rev fields)

let prompt_arg_to_json (arg : prompt_arg) : Yojson.Safe.t =
  (* Custom serializer to produce non-optional output for backward compat *)
  let fields = [("name", `String arg.name)] in
  let fields = match arg.description with Some d -> ("description", `String d) :: fields | None -> fields in
  let fields = match arg.required with Some r -> ("required", `Bool r) :: fields | None -> fields in
  `Assoc (List.rev fields)

let prompt_to_json (p : mcp_prompt) : Yojson.Safe.t =
  `Assoc [
    ("name", `String p.name);
    ("description", `String p.description);
    ("arguments", `List (List.map prompt_arg_to_json p.arguments));
  ]

let prompt_to_detail_json (p : mcp_prompt) : Yojson.Safe.t =
  `Assoc [
    ("name", `String p.name);
    ("description", `String p.description);
    ("arguments", `List (List.map prompt_arg_to_json p.arguments));
    ("text", `String p.text);
  ]

(** ============== Handler types ============== *)

(** Sync handler type - Pure Eio based *)
type tool_handler_sync = Yojson.Safe.t -> (Yojson.Safe.t, string) result

type mcp_server = {
  tools: tool_def list;
  handlers_sync: (string * tool_handler_sync) list;
  resources: mcp_resource list;
  resource_templates: mcp_resource_template list;
  prompts: mcp_prompt list;
  read_resource: resource_reader;
}

(** ============== Default handler implementations ============== *)

(** MCP Instructions: guidelines for LLMs *)
let mcp_instructions = {|
## figma-mcp v2

This server is limited to design context extraction and verification.

### Public tools

- `figma_get_design_context`
- `figma_get_metadata`
- `figma_get_variable_defs`
- `figma_get_screenshot`
- `figma_get_code_connect_map`
- `figma_whoami`
- `figma_verify_semantic`
- `figma_verify_visual`

### Scope boundaries

- Use `url` directly on the v2 tools when you have a Figma URL.
- `FIGMA_TOKEN` is resolved from the environment by default.
- Agent orchestration, planning flows, category routers, and gRPC transport are out of scope.

### Recommended workflow

1. Use `figma_get_metadata` to inspect large selections safely.
2. Use `figma_get_design_context` for implementation context.
3. Use `figma_get_variable_defs` or `figma_get_code_connect_map` when token or component mapping data is needed.
4. Use `figma_verify_semantic` first, then `figma_verify_visual` when visual parity matters.

### Notes

- `plugin_channel_id` on `figma_get_design_context` enables plugin enrichment automatically.
- `figma_get_screenshot` returns export URLs or download results for a node.
- `figma://docs/v2-surface` and `figma://docs/verification` are the authoritative MCP resources in v2.
|}

let handle_initialize params : Yojson.Safe.t =
  let client_version = protocol_version_from_params params in
  let negotiated_version = normalize_protocol_version client_version in
  `Assoc [
    ("protocolVersion", `String negotiated_version);
    ("capabilities", `Assoc [
      ("tools", `Assoc []);
      ("resources", `Assoc [("listChanged", `Bool false)]);
      ("prompts", `Assoc [("listChanged", `Bool false)]);
    ]);
    ("serverInfo", `Assoc [
      ("name", `String server_name);
      ("version", `String server_version);
    ]);
    ("instructions", `String mcp_instructions);
  ]

let handle_tools_list server _params : Yojson.Safe.t =
  let tools_json = List.map tool_to_json server.tools in
  `Assoc [("tools", `List tools_json)]

let handle_resources_list server _params : Yojson.Safe.t =
  let resources_json = List.map resource_to_json server.resources in
  `Assoc [("resources", `List resources_json)]

let handle_resource_templates_list server _params : Yojson.Safe.t =
  let templates_json = List.map resource_template_to_json server.resource_templates in
  `Assoc [("resourceTemplates", `List templates_json)]

let handle_prompts_list server _params : Yojson.Safe.t =
  let prompts_json = List.map prompt_to_json server.prompts in
  `Assoc [("prompts", `List prompts_json)]

let handle_prompts_get server params : (Yojson.Safe.t, int * string) result =
  match params with
  | Some (`Assoc lst) ->
      let name = match List.assoc_opt "name" lst with Some (`String s) -> Some s | _ -> None in
      (match name with
       | Some prompt_name ->
           (match List.find_opt (fun p -> p.name = prompt_name) server.prompts with
            | Some prompt -> Ok (`Assoc [("prompt", prompt_to_detail_json prompt)])
            | None -> Error (invalid_params, sprintf "Prompt not found: %s" prompt_name))
       | None -> Error (invalid_params, "Missing name"))
  | _ -> Error (invalid_params, "Invalid params format")

let handle_resources_read server params : (Yojson.Safe.t, int * string) result =
  match params with
  | Some (`Assoc lst) ->
      let uri = match List.assoc_opt "uri" lst with Some (`String s) -> Some s | _ -> None in
      (match uri with
       | Some u ->
           (match server.read_resource u with
            | Ok (mime, text) ->
                Ok (`Assoc [
                  ("contents", `List [
                    `Assoc [
                      ("uri", `String u);
                      ("mimeType", `String mime);
                      ("text", `String text);
                    ]
                  ])
                ])
            | Error msg -> Error (internal_error, msg))
       | None -> Error (invalid_params, "Missing uri"))
  | _ -> Error (invalid_params, "Invalid params format")

(** ============== Sync request processing (Pure Eio) ============== *)

(** tools/call handler - sync execution *)
let handle_tools_call_sync server params : (Yojson.Safe.t, int * string) result =
  match params with
  | Some (`Assoc lst) ->
      let name = match List.assoc_opt "name" lst with Some (`String s) -> Some s | _ -> None in
      let arguments = List.assoc_opt "arguments" lst |> Option.value ~default:(`Assoc []) in
      (match name with
       | Some tool_name ->
           let start = Unix.gettimeofday () in
           let finish result =
             let duration_ms =
               int_of_float ((Unix.gettimeofday () -. start) *. 1000.0)
             in
             let success = Result.is_ok result in
             let error = match result with Ok _ -> None | Error (_, msg) -> Some msg in
             Telemetry_jsonl.log_tool_called ~tool_name ~duration_ms ~success ~error;
             result
           in
           (match List.assoc_opt tool_name server.handlers_sync with
            | Some handler ->
                let result =
                  match handler arguments with
                  | Ok res -> Ok res
                  | Error msg -> Error (internal_error, msg)
                in
                finish result
            | None ->
                finish (Error (method_not_found, sprintf "Tool not found: %s" tool_name)))
       | None -> Error (invalid_params, "Missing tool name"))
  | _ -> Error (invalid_params, "Invalid params format")

(** Main request processing - sync version (HTTP/Eio mode) *)
let process_request_sync server req : Yojson.Safe.t =
  let id = Option.value req.id ~default:`Null in

  match req.method_ with
  | "initialize" ->
      make_success_response id (handle_initialize req.params)

  | "initialized" | "notifications/initialized" ->
      make_success_response id `Null

  | "tools/list" ->
      make_success_response id (handle_tools_list server req.params)

  | "tools/call" ->
      (match handle_tools_call_sync server req.params with
       | Ok res -> make_success_response id res
       | Error (code, msg) -> make_error_response id code msg None)

  | "resources/list" ->
      make_success_response id (handle_resources_list server req.params)

  | "resources/templates/list" ->
      make_success_response id (handle_resource_templates_list server req.params)

  | "resources/read" ->
      (match handle_resources_read server req.params with
       | Ok res -> make_success_response id res
       | Error (code, msg) -> make_error_response id code msg None)

  | "prompts/list" ->
      make_success_response id (handle_prompts_list server req.params)

  | "prompts/get" ->
      (match handle_prompts_get server req.params with
       | Ok res -> make_success_response id res
       | Error (code, msg) -> make_error_response id code msg None)

  | _ ->
      make_error_response id method_not_found (sprintf "Unknown method: %s" req.method_) None

(** ============== stdio server loop ============== **)

let run_stdio_server server =
  eprintf "[%s] MCP Server %s started (protocol: %s)\n%!" server_name server_version protocol_version;

  try
    while true do
      let line = input_line stdin in
      if String.trim line <> "" then begin
        match parse_request line with
        | Ok req ->
            if is_notification req then
              ignore (process_request_sync server req)
            else begin
              let response = process_request_sync server req in
              let response_str = Yojson.Safe.to_string response in
              print_endline response_str;
              flush stdout
            end
        | Error msg ->
            let err_response = make_error_response `Null parse_error msg None in
            print_endline (Yojson.Safe.to_string err_response);
            flush stdout
      end
    done
  with
  | End_of_file ->
      eprintf "[%s] Connection closed\n%!" server_name
  | exn ->
      eprintf "[%s] Error: %s\n%!" server_name (Printexc.to_string exn)

(** ============== Server creation helper ============== *)

let create_server
  ?(handlers_sync=[])
  ?(resource_templates=[])
  tools
  resources
  prompts
  read_resource
  =
  { tools; handlers_sync; resources; resource_templates; prompts; read_resource }
