(** MCP Protocol - JSON-RPC 2.0 핸들러 (2025-11-25 스펙) *)

open Printf

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
let parse_error = -32700
let invalid_request = -32600
let method_not_found = -32601
let invalid_params = -32602
let internal_error = -32603

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
  | Some (`Assoc _ as p) ->
      (try
        match List.assoc_opt "protocolVersion" (match p with `Assoc lst -> lst | _ -> []) with
        | Some (`String v) -> v
        | _ -> default_protocol_version
       with _ -> default_protocol_version)
  | _ -> default_protocol_version

let protocol_version = default_protocol_version  (* for backward compat *)
let server_name = "figma-mcp"
let server_version = Version.version

(** ============== JSON 유틸리티 ============== *)

let member key json =
  match json with
  | `Assoc lst -> List.assoc_opt key lst
  | _ -> None


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
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result)
  ]

let make_error_response id code message data : Yojson.Safe.t =
  let error_obj = [
    ("code", `Int code);
    ("message", `String message);
  ] @ (match data with Some d -> [("data", d)] | None -> [])
  in
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("error", `Assoc error_obj)
  ]

(** ============== Tool 정의 → JSON ============== *)

let tool_to_json (tool : tool_def) : Yojson.Safe.t =
  `Assoc [
    ("name", `String tool.name);
    ("description", `String tool.description);
    ("inputSchema", tool.input_schema);
  ]

let resource_to_json (r : mcp_resource) : Yojson.Safe.t =
  `Assoc [
    ("uri", `String r.uri);
    ("name", `String r.name);
    ("description", `String r.description);
    ("mimeType", `String r.mime_type);
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
  prompts: mcp_prompt list;
  read_resource: resource_reader;
}

(** ============== 기본 핸들러 구현 ============== *)

(** MCP Instructions: LLM이 읽고 따라야 할 개발 가이드라인 *)
let mcp_instructions = {|
## Figma MCP - UI 구현 가이드라인

### 🎯 목표: 95%+ Fidelity UI 구현

### 📐 두 가지 구현 패턴

**1. Outside-In (Matryoshka) 패턴** - 대규모 디자인 추천
- `figma_get_node_summary`로 전체 구조 파악
- 최상위 컨테이너 먼저 구현 (레이아웃, 배경)
- 자식은 `{/* TODO: Title */}` placeholder로 표시
- `figma_get_node`로 각 자식을 점진적 확장
- 장점: 컨텍스트 절약, 구조 유지

**2. Inside-Out (Bottom-Up) 패턴** - 재사용 컴포넌트
- 가장 작은 원자 컴포넌트부터 구현 (Button, Icon)
- 조합하여 분자 → 유기체 → 템플릿 완성
- 장점: 재사용성, Atomic Design 친화

### 🔧 DSL 읽는 법
```
F(Card 320×200 col gap:12 ax:min cx:stretch bg:#FFF r:12,16,12,16)
│ ├─ F = Frame
│ ├─ 320×200 = 크기
│ ├─ col = 세로 레이아웃
│ ├─ gap:12 = 자식 간격
│ ├─ ax:min = 주축 정렬 (시작)
│ ├─ cx:stretch = 교차축 정렬 (늘리기)
│ ├─ bg:#FFF = 배경색
│ └─ r:12,16,12,16 = 모서리 (TL,TR,BR,BL)
```

### ⚠️ 대용량 응답 주의
- 500KB 이상 응답 시 구조 요약 먼저 확인
- `depth` 파라미터로 탐색 깊이 제한
- 반복되는 스타일은 CSS 변수로 추출
- 전체 재귀가 필요하면 gRPC `GetNodeStream`의 `recursive=true` 사용
- 분할정복 플랜은 gRPC `PlanTasks`의 `recursive=true`로 생성

### 🔄 권장 워크플로우
1. `figma_list_screens` → 화면 목록 확인
2. `figma_get_node_summary` → 구조 파악 (Outside-In)
3. `figma_tree` → 계층 시각화
4. `figma_get_node` → 상세 구현
5. `figma_export_tokens` → 디자인 토큰 추출

### 🎯 99%+ SSIM 달성 핵심 (Visual Verification)

**1. Flat HTML > Nested HTML**
- Figma 계층 그대로 복제 ❌ → 시각적 동등 HTML ✅
- 2-level 구조: 외부 컨테이너 + 내부 요소 + 텍스트

**2. 정밀 색상 변환**
- `#1F8CF8` (hex) ❌ → `rgb(32,141,249)` (rgb) ✅
- 반올림 필수: `Float.round(r * 255)`

**3. Typography 완전성**
- `letter-spacing: -0.32px` 필수 (텍스트 폭 정확도)
- `line-height: 24px` 필수 (텍스트 높이 정확도)

**4. 중앙 정렬 공식**
```css
display: flex;
align-items: center;
justify-content: center;
```

**5. `figma_verify_visual` 도구 사용**
- target_ssim: 0.95 (95% 이상 통과)
- max_iterations: 3 (자동 보정 시도)
- 초기 품질이 높으면 보정 불필요 (99%+ 즉시 달성)

### ⚠️ TEXT 노드 정확도 (Critical - SSIM은 텍스트를 검증하지 않음)

**SSIM의 한계**: SSIM은 픽셀 구조 유사도만 측정 → 같은 폰트/크기/색상이면 **다른 텍스트도 높은 점수**

**TEXT 노드 처리 규칙** (필수):
1. DSL의 `"text":{"characters":"..."}` 필드를 **반드시 그대로** HTML에 사용
2. **절대로** 텍스트를 hallucinate하거나 추측하지 말 것
3. DSL 요약/압축 시에도 TEXT 노드의 `characters`는 **반드시 보존**
4. 원본 텍스트가 한국어면 한국어 그대로 유지

**TEXT 노드 확인 체크리스트**:
- [ ] DSL에서 `characters` 필드 확인했는가?
- [ ] HTML의 텍스트가 DSL의 `characters`와 **정확히** 일치하는가?
- [ ] 어떤 텍스트도 임의로 생성하지 않았는가?

**Example**:
```
DSL: T("일괄 등록하기" 15 #333C47 weight:500)
HTML (✅): <span>일괄 등록하기</span>
HTML (❌): <span>Bulk Register</span>  <!-- hallucinated! -->
HTML (❌): <span>등록하기</span>  <!-- partial/modified! -->
```

### 🖥️ HTML 렌더링 우선순위 (Chrome-First Strategy)

Visual Verification 시 HTML을 PNG로 렌더링할 때 다음 우선순위를 따릅니다:

**1순위: claude-in-chrome (권장)**
`mcp__claude-in-chrome__*` 도구가 사용 가능한 경우:
1. `mcp__claude-in-chrome__navigate`로 HTML 파일 열기 (file:// 또는 data URI)
2. `mcp__claude-in-chrome__computer` action="screenshot"으로 스크린샷 캡처
3. `figma_image_similarity`로 Figma 렌더와 SSIM 비교
4. 장점: 실제 브라우저 환경, 폰트 렌더링 정확도 높음

**2순위: figma_verify_visual (Fallback)**
chrome 도구 불가 시 `figma_verify_visual` 내장 Playwright 사용:
- 자동 HTML 렌더링 + SSIM 비교 + CSS 자동 보정
- Playwright 설치 필요: `npx playwright install chromium`

**Chrome-First 워크플로우 예시**:
```
1. figma_get_node_bundle → DSL + Figma 렌더 이미지 획득
2. HTML 코드 생성 → 임시 파일 저장
3. [Chrome 가용 시]
   - claude-in-chrome navigate → screenshot
   - figma_image_similarity로 SSIM 측정
4. [Chrome 불가 시]
   - figma_verify_visual html=<생성한HTML>
```
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
           (match List.assoc_opt tool_name server.handlers_sync with
            | Some handler ->
                (match handler arguments with
                 | Ok res -> Ok res
                 | Error msg -> Error (internal_error, msg))
            | None -> Error (method_not_found, sprintf "Tool not found: %s" tool_name))
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
      make_success_response id (`Assoc [("resourceTemplates", `List [])])

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
  eprintf "[%s] MCP Server started (protocol: %s)\n%!" server_name protocol_version;

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

let create_server ?(handlers_sync=[]) tools resources prompts read_resource =
  { tools; handlers_sync; resources; prompts; read_resource }
