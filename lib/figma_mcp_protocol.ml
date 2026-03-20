(** MCP Protocol - JSON-RPC 2.0 핸들러 (2025-11-25 스펙) *)

open Printf

(* The local protocol wrapper lives under a figma-specific module name so the
   external MCP SDK can be linked without colliding on Mcp_protocol. *)
module Sdk_error_codes = Mcp_protocol.Error_codes
module Sdk_jsonrpc = Mcp_protocol.Jsonrpc

(** ============== JSON-RPC 타입 ============== *)

type json_rpc_request = {
  jsonrpc: string;
  id: Yojson.Safe.t option;  (* null, string, or number *)
  method_: string;
  params: Yojson.Safe.t option;
}

type json_rpc_response =
  | RpcSuccess of { id: Yojson.Safe.t; result: Yojson.Safe.t }
  | RpcError of { id: Yojson.Safe.t; code: int; message: string; data: Yojson.Safe.t option }

type tool_def = {
  name: string;
  description: string;
  input_schema: Yojson.Safe.t;
}

type mcp_resource = {
  uri: string;
  name: string;
  description: string;
  mime_type: string;
}

type mcp_resource_template = {
  uri_template: string;
  name: string;
  description: string;
  mime_type: string;
}

type prompt_arg = {
  name: string;
  description: string;
  required: bool;
}

type mcp_prompt = {
  name: string;
  description: string;
  arguments: prompt_arg list;
  text: string;
}

type resource_reader = string -> (string * string, string) result

(** ============== 에러 코드 (JSON-RPC 2.0) ============== *)
let parse_error = Sdk_error_codes.parse_error
let invalid_request = Sdk_error_codes.invalid_request
let method_not_found = Sdk_error_codes.method_not_found
let invalid_params = Sdk_error_codes.invalid_params
let internal_error = Sdk_error_codes.internal_error

(** ============== 서버 정보 ============== *)
let supported_protocol_versions = [
  "2024-11-05";
  "2025-03-26";
  "2025-11-25";
]

let default_protocol_version = "2025-11-25"

let normalize_protocol_version version =
  if List.mem version supported_protocol_versions then version
  else default_protocol_version

let protocol_version_from_params params =
  match params with
  | Some (`Assoc lst) ->
      (match List.assoc_opt "protocolVersion" lst with
       | Some (`String v) -> v
       | _ -> default_protocol_version)
  | _ -> default_protocol_version

let protocol_version = default_protocol_version  (* for backward compat *)
let server_name = "figma-mcp"
let server_version = Version.version

(** ============== JSON 유틸리티 ============== *)

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

(** ============== 응답 생성 ============== *)

let make_success_response id result : Yojson.Safe.t =
  Sdk_jsonrpc.make_response_json ~id:(sdk_id_of_json id) ~result

let make_error_response id code message data : Yojson.Safe.t =
  Sdk_jsonrpc.make_error_json ~id:(sdk_id_of_json id) ~code ~message ?data ()

(** ============== Tool 정의 → JSON ============== *)

let tool_to_json (tool : tool_def) : Yojson.Safe.t =
  (* P1.4: Auto-detect [DEPRECATED] prefix and add deprecated field *)
  let is_deprecated = String.length tool.description >= 12 &&
    String.sub tool.description 0 12 = "[DEPRECATED]" in
  let base_fields = [
    ("name", `String tool.name);
    ("description", `String tool.description);
    ("inputSchema", tool.input_schema);
  ] in
  if is_deprecated then
    `Assoc (base_fields @ [("deprecated", `Bool true)])
  else
    `Assoc base_fields

let resource_to_json (r : mcp_resource) : Yojson.Safe.t =
  `Assoc [
    ("uri", `String r.uri);
    ("name", `String r.name);
    ("description", `String r.description);
    ("mimeType", `String r.mime_type);
  ]

let resource_template_to_json (t : mcp_resource_template) : Yojson.Safe.t =
  `Assoc [
    ("uriTemplate", `String t.uri_template);
    ("name", `String t.name);
    ("description", `String t.description);
    ("mimeType", `String t.mime_type);
  ]

let prompt_arg_to_json (arg : prompt_arg) : Yojson.Safe.t =
  `Assoc [
    ("name", `String arg.name);
    ("description", `String arg.description);
    ("required", `Bool arg.required);
  ]

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

(** ============== 핸들러 타입 ============== *)

(** 동기 핸들러 타입 - Pure Eio 기반 *)
type tool_handler_sync = Yojson.Safe.t -> (Yojson.Safe.t, string) result

type mcp_server = {
  tools: tool_def list;
  handlers_sync: (string * tool_handler_sync) list;
  resources: mcp_resource list;
  resource_templates: mcp_resource_template list;
  prompts: mcp_prompt list;
  read_resource: resource_reader;
}

(** ============== 기본 핸들러 구현 ============== *)

(** MCP Instructions: LLM이 읽고 따라야 할 개발 가이드라인 *)
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

(** ============== 동기 요청 처리 (Pure Eio) ============== *)

(** tools/call 핸들러 - 동기 실행 *)
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

(** 메인 요청 처리 - 동기 버전 (HTTP/Eio 모드용) *)
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

(** ============== stdio 서버 루프 ============== **)

let run_stdio_server server =
  (* stderr로 로깅 *)
  eprintf "[%s] MCP Server %s started (protocol: %s)\n%!" server_name server_version protocol_version;

  try
    while true do
      let line = input_line stdin in
      if String.trim line <> "" then begin
        match parse_request line with
        | Ok req ->
            if is_notification req then
              (* Notification: no response on stdout per JSON-RPC *)
              ignore (process_request_sync server req)
            else begin
              (* stdio 모드: 동기 핸들러 직접 실행 *)
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

(** ============== 서버 생성 헬퍼 ============== *)

let create_server
  ?(handlers_sync=[])
  ?(resource_templates=[])
  tools
  resources
  prompts
  read_resource
  =
  { tools; handlers_sync; resources; resource_templates; prompts; read_resource }
