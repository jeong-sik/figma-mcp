open Figma_mcp_protocol
open Mcp_helpers
open Mcp_api_handlers
open Mcp_plugin_handlers
open Mcp_visual_handlers
open Mcp_planning_handlers
open Mcp_tool_defs
open Mcp_tool_handlers
(** 동기 핸들러 리스트 - HTTP/Eio 모드에서 사용 *)

let all_handlers_sync : (string * tool_handler_sync) list = [
  (* 기존 도구 - 동기 버전 *)
  ("figma_codegen", wrap_sync_pure handle_codegen_sync);
  ("figma_get_file", wrap_sync_pure handle_get_file);
  ("figma_get_file_meta", wrap_sync_pure handle_get_file_meta);
  ("figma_list_screens", wrap_sync_pure handle_list_screens);
  ("figma_get_node", wrap_sync_pure handle_get_node);
  ("figma_get_node_bundle", wrap_sync_pure handle_get_node_bundle);
  ("figma_get_node_summary", wrap_sync_pure handle_get_node_summary);
  ("figma_get_planning_context", wrap_sync_pure handle_get_planning_context);
  ("figma_select_nodes", wrap_sync_pure handle_select_nodes);
  ("figma_validate_agent_plan", wrap_sync_pure handle_validate_agent_plan);
  ("figma_get_node_chunk", wrap_sync_pure handle_get_node_chunk);
  ("figma_fidelity_loop", wrap_sync_pure handle_fidelity_loop);
  ("figma_image_similarity", wrap_sync_pure handle_image_similarity);
  ("figma_verify_visual", wrap_sync_pure handle_verify_visual);
  ("figma_verify_semantic", wrap_sync_pure handle_verify_semantic);
  ("figma_export_image", wrap_sync_pure handle_export_image);
  ("figma_export_smart", wrap_sync_pure handle_export_smart);
  ("figma_get_image_fills", wrap_sync_pure handle_get_image_fills);
  ("figma_get_nodes", wrap_sync_pure handle_get_nodes);
  ("figma_get_file_versions", wrap_sync_pure handle_get_file_versions);
  ("figma_get_file_comments", wrap_sync_pure handle_get_file_comments);
  ("figma_post_comment", wrap_sync_pure handle_post_comment);
  ("figma_get_file_components", wrap_sync_pure handle_get_file_components);
  ("figma_get_team_components", wrap_sync_pure handle_get_team_components);
  ("figma_get_file_component_sets", wrap_sync_pure handle_get_file_component_sets);
  ("figma_get_team_component_sets", wrap_sync_pure handle_get_team_component_sets);
  ("figma_get_file_styles", wrap_sync_pure handle_get_file_styles);
  ("figma_get_team_styles", wrap_sync_pure handle_get_team_styles);
  ("figma_get_component", wrap_sync_pure handle_get_component);
  ("figma_get_component_set", wrap_sync_pure handle_get_component_set);
  (* handle_get_style: not yet implemented, removed from dispatch *)

  (* STRAP 통합: 8개 plugin 핸들러 → 1개 라우터 + 4개 전용 mutation 핸들러 *)
  ("figma_plugin", wrap_sync_pure handle_figma_plugin);
  ("figma_plugin_edit_node", wrap_sync_pure handle_plugin_edit_node);
  ("figma_plugin_create_node", wrap_sync_pure handle_plugin_create_node);
  ("figma_plugin_delete_nodes", wrap_sync_pure handle_plugin_delete_nodes);
  ("figma_plugin_batch", wrap_sync_pure handle_plugin_batch);
  ("figma_plugin_subscribe_events", wrap_sync_pure handle_plugin_subscribe_events);
  (* Plugin Enhancement Tools *)
  ("figma_export_tokens_plugin", wrap_sync_pure handle_export_tokens_plugin);
  (* Phase 1: 탐색 도구 *)
  ("figma_parse_url", wrap_sync_pure handle_parse_url);
  ("figma_get_me", wrap_sync_pure handle_get_me);
  ("figma_list_projects", wrap_sync_pure handle_list_projects);
  ("figma_list_files", wrap_sync_pure handle_list_files);
  ("figma_crawl_team", wrap_sync_pure handle_crawl_team);
  ("figma_team_tree", wrap_sync_pure handle_team_tree);
  ("figma_export_team", wrap_sync_pure handle_export_team);
  ("figma_get_variables", wrap_sync_pure handle_get_variables);
  (* Phase 2: 고급 쿼리 *)
  ("figma_query", wrap_sync_pure handle_query);
  ("figma_search", wrap_sync_pure handle_search);
  ("figma_compare", wrap_sync_pure handle_compare);
  (* Phase 3: 분석/추출 *)
  ("figma_tree", wrap_sync_pure handle_tree);
  ("figma_stats", wrap_sync_pure handle_stats);
  ("figma_export_tokens", wrap_sync_pure handle_export_tokens);
  ("figma_doctor", wrap_sync_pure handle_doctor);
  ("figma_read_large_result", wrap_sync_pure handle_read_large_result);
  ("figma_code_connect", wrap_sync_pure handle_code_connect);
  (* 캐시 관리 *)
  ("figma_cache_stats", wrap_sync_pure handle_cache_stats);
  ("figma_cache_invalidate", wrap_sync_pure handle_cache_invalidate);
  (* 카테고리 도구 핸들러 *)
  ("figma_core", wrap_sync_pure (handle_category "core"));
  ("figma_visual", wrap_sync_pure (handle_category "visual"));
  (* figma_plugin: monolithic handler (line ~8408)가 List.assoc_opt에서 먼저 발견됨.
     category handler 등록 시 MCP dispatch는 List.assoc_opt (first-match)를 쓰므로
     monolithic handler가 우선됨. sub-handler 미등록으로 category 라우팅 불가하여 제거. *)
  ("figma_team", wrap_sync_pure (handle_category "team"));
  ("figma_export", wrap_sync_pure (handle_category "export"));
  ("figma_components", wrap_sync_pure (handle_category "components"));
  ("figma_code", wrap_sync_pure (handle_category "code"));
]

let find_sync_handler name = List.assoc_opt name all_handlers_sync

(** 핸들러 레지스트리 초기화 - module load 시 자동 실행 *)
let () =
  List.iter (fun (name, sync_handler) ->
    register_handler name sync_handler
  ) all_handlers_sync

(** ============== Resources / Prompts ============== **)

let resources : mcp_resource list = [
  {
    uri = "figma://docs/fidelity";
    name = "Fidelity DSL";
    description = "fidelity 출력 포맷(정확도 우선) 설명 및 키 목록";
    mime_type = "text/markdown";
  };
  {
    uri = "figma://docs/usage";
    name = "Usage";
    description = "정확도 우선 호출 패턴 및 옵션";
    mime_type = "text/markdown";
  };
  {
    uri = "figma://docs/tokens";
    name = "Tokens";
    description = "Figma Variables(Design Tokens) 리소스/템플릿 사용 가이드";
    mime_type = "text/markdown";
  };
]

let resource_templates : mcp_resource_template list = [
  {
    uri_template = "figma://tokens/{file_key}";
    name = "Design tokens";
    description = "Figma Variables API를 design tokens로 제공합니다. Query: format=raw|resolved|dtcg (default: resolved)";
    mime_type = "application/json";
  };
]

let prompts : mcp_prompt list = [
  {
    name = "figma_fidelity_review";
    description = "🔍 REVIEW: Fidelity DSL 누락 필드 점검. *_missing 목록 확인하고 재호출 파라미터 제안. 구현 전 품질 체크에 사용.";
    arguments = [
      { name = "file_key"; description = "Figma 파일 키"; required = true };
      { name = "node_id"; description = "노드 ID"; required = true };
      { name = "depth"; description = "트리 깊이 제한"; required = false };
    ];
    text = {|
당신은 Figma Fidelity DSL 리뷰어입니다.

입력:
- file_key: {{file_key}}
- node_id: {{node_id}}
- depth: {{depth}}

점검 항목:
1) meta/structure/geometry/vector/layout/paint/effects/text/text_segments/instance/variables/assets의 *_missing 목록 확인
2) children_present=false 인 경우 depth 조정 필요성 판단
3) 이미지가 있는 경우 image_fills 누락 확인 (필요 시 include_image_fills=true)
4) variables_resolved 누락 시 include_variables=true 제안
5) 텍스트 세그먼트/라인 이슈 시 include_plugin=true 제안
6) 렌더 정확도 이슈 시 figma_get_node_bundle + use_absolute_bounds=true 제안
7) 변수 API 오류 시 include_plugin_variables=true 제안
8) 플러그인 렌더가 필요하면 include_plugin_image=true 제안
9) 벡터/패스 누락이면 geometry=paths + depth 상향 제안
10) ⚠️ TEXT 노드 정확도 (Critical): DSL의 text.characters 필드가 HTML에 **정확히 그대로** 반영되었는지 확인
    - SSIM은 픽셀 구조만 측정 → 같은 폰트/크기면 다른 텍스트도 높은 점수
    - 텍스트를 hallucinate하거나 추측하지 말 것
    - 원본 텍스트가 한국어면 한국어 그대로 유지

출력:
- 누락/의심 항목 요약
- 필요한 재호출 파라미터 제안
|};
  };
  {
    name = "figma_error_troubleshoot";
    description = "🩺 TROUBLESHOOT: API 에러 원인 분석 및 해결책 제안. 에러 메시지와 파라미터를 입력하면 복구 방법 안내.";
    arguments = [
      { name = "error_message"; description = "발생한 에러 메시지"; required = true };
      { name = "tool_name"; description = "호출한 도구 이름"; required = true };
      { name = "params"; description = "사용한 파라미터 (JSON)"; required = false };
    ];
    text = {|
당신은 Figma MCP 에러 진단 전문가입니다.

입력:
- error_message: {{error_message}}
- tool_name: {{tool_name}}
- params: {{params}}

진단 체크리스트:
1) **node_id 형식 확인**
   - 올바른 형식: `123:456` (숫자:숫자)
   - URL에서 추출 시: `node-id=123-456` → `123:456`로 변환
   - figma_parse_url로 URL 파싱 권장

2) **file_key 확인**
   - figma.com/file/XXXXX/... 에서 XXXXX 부분
   - 영문+숫자 조합 (보통 22자)

3) **권한 문제 (403)**
   - FIGMA_TOKEN 환경변수 설정 확인
   - 토큰 만료 여부 확인
   - 파일이 팀/조직 내 공유되었는지 확인

4) **리소스 없음 (404)**
   - node_id가 해당 파일에 존재하는지 확인
   - 버전 파라미터가 올바른지 확인 (version=...)

5) **대용량 응답**
   - depth 파라미터로 깊이 제한
   - figma_get_node_chunk로 분할 로드
   - large_result 반환 시 figma_read_large_result 사용

출력:
- 원인 분석 (가장 가능성 높은 원인)
- 해결 단계 (구체적인 명령/파라미터)
- 예방책 (향후 같은 에러 방지)
|};
  };
]

let read_resource uri =
  let starts_with ~prefix s =
    let lp = String.length prefix in
    String.length s >= lp && String.sub s 0 lp = prefix
  in
  let parse_query q =
    q
    |> String.split_on_char '&'
    |> List.filter_map (fun part ->
      match String.split_on_char '=' part with
      | [k; v] when k <> "" -> Some (k, v)
      | [k] when k <> "" -> Some (k, "")
      | _ -> None)
  in
  let split_query s =
    match String.index_opt s '?' with
    | None -> (s, [])
    | Some i ->
        let base = String.sub s 0 i in
        let q = String.sub s (i + 1) (String.length s - i - 1) in
        (base, parse_query q)
  in
  match uri with
  | "figma://docs/fidelity" ->
      let body = {|
# Fidelity DSL (v3)

## Output shape
- JSON object with sections:
  `meta`, `structure`, `geometry`, `vector`, `layout`, `paint`, `effects`,
  `text`, `text_segments`, `instance`, `variables`, `variables_resolved`,
  `assets`, `plugin`, `children`
- Each section includes only keys present in the Figma JSON
- `*_missing` lists keys that were absent in the source JSON

## Notes
- Set `geometry=paths` to receive vector geometry (`vectorNetwork`, `fillGeometry`, etc)
- `assets_missing` includes `image_fills` when `image_refs` exist but fills are not fetched
- `variables_resolved` and `plugin` are filled when `include_variables` / `include_plugin` are enabled
- Plugin snapshots include `text.segments` + range bounds when available
- `plugin_variables` is available when `include_plugin_variables=true` (Variables API fallback)
- `plugin_image` is available when `include_plugin_image=true` (base64 render)
- `variables_source` indicates whether variables came from REST, cache, or plugin
- Use `use_absolute_bounds=true` for render bounds in image exports

## Fidelity scoring (all-axes)
- Sections weighted by importance; missing in any axis lowers score.
- `variables_resolved`: uses Variables API resolved/default values.
- `assets`: compares `image_refs` in DSL vs `image_fills` map.
- `plugin`: counts `text.segments` from plugin snapshot (line/segment detail).

## node_id format
- Figma URL shows `node-id=2089-11127` (hyphen), but API expects `2089:11127` (colon)
- MCP tools recommend colon format: `figma_get_node`, `figma_get_node_bundle`
- Convert: `2089-11127` -> `2089:11127`
- MCP tools normalize hyphen format automatically (URL format accepted)
|} in
      Ok ("text/markdown", body)
  | "figma://docs/usage" ->
      let body = {|
# Usage (accuracy-first)

## Recommended calls
- `figma_get_node` with `format=fidelity`
- Pixel-perfect bundle: `figma_get_node_bundle`
- Auto depth escalation: `figma_fidelity_loop` (target score 기반 반복)
- Render similarity: `figma_image_similarity` (SSIM/PSNR)

## Accuracy-first loop (suggested order)
1) `figma_fidelity_loop` with `include_variables=true`, `include_image_fills=true`, `include_plugin=true`
2) If still low: increase `max_depth` / `depth_step` and ensure `geometry=paths`
3) Fetch render with `figma_get_node_bundle` + `use_absolute_bounds=true`
4) Compare renders via `figma_image_similarity`

## Full-axes options
- `figma_fidelity_loop` + `include_variables=true` + `include_image_fills=true` + `include_plugin=true`
- `figma_get_node_bundle` + `include_plugin=true` for text segments/line bounds
- `include_plugin_variables=true` for Variables API fallback (Enterprise-free)
- `include_plugin_image=true` for plugin-rendered base64 images (large output)
- Pair DSL with images via `figma_get_node_bundle` (use_absolute_bounds=true)
- For plugin snapshots:
  - `figma_plugin_connect` → copy channel ID
  - `figma_plugin_use_channel` or pass `plugin_channel_id`
  - `figma_get_node_bundle` with `include_plugin=true`
- Use `depth` to control context size (API `depth`)
- Use `geometry=paths` to include vector geometry fields
- Use `plugin_data` if plugin-authored metadata is needed

## REST parity helpers
- `figma_get_nodes` for multi-node fetch
- `figma_get_file_components` / `figma_get_file_styles` for asset metadata
- `figma_get_file_versions` / `figma_get_file_comments` for change context

## node_id format
- Figma URL shows `node-id=2089-11127` (hyphen), but API expects `2089:11127` (colon)
- MCP tools recommend colon format: `figma_get_node`, `figma_get_node_bundle`
- Convert: `2089-11127` -> `2089:11127`
- MCP tools normalize hyphen format automatically (URL format accepted)
- Tip: `figma_parse_url` returns a ready-to-use `node_id`

## Variables
- `figma_get_variables` with `format=resolved` to get default-mode values

## Raw JSON
- Use `format=raw` to get lossless JSON (largest output)

## gRPC streaming
- `GetNodeStream` supports `recursive=true` to stream full subtrees
- Use `recursive_max_depth` / `recursive_max_nodes` for safety
- planning 기본 경로는 `figma_get_planning_context` + external agent planning + `figma_validate_agent_plan`
- `PlanTasks` is retained only as a legacy heuristic path
- `grpcurl` 사용 시 reflection이 비활성화되어 있으므로 `-import-path proto -proto figma.proto` 옵션 필요

## Pixel accuracy
- Pair DSL with images via `figma_get_node_bundle`
- Use `use_absolute_bounds=true` to include effects in render bounds
|} in
      Ok ("text/markdown", body)
  | "figma://docs/tokens" ->
      let body = {|
# Tokens (Figma Variables)

이 서버는 Figma Variables(Design Tokens)를 MCP **resource**로도 제공합니다.

## Static resource
- `figma://docs/tokens` (이 문서)

## Resource template
- `figma://tokens/{file_key}`
  - Query:
    - `format=resolved` (기본값): default mode 기준 값 포함
    - `format=raw`: Figma Variables API 원본
    - `format=dtcg`: 토큰 트리(leaf에 `$type`/`$value`), alias는 `{path}` 형태로 표현

## Notes
- 인증은 `FIGMA_TOKEN` 환경변수로 처리합니다.
- `format=dtcg`는 spec strict-compat을 보장하지 않습니다. (leaf 구조는 DTCG 스타일, 루트는 `$extensions` 메타 포함)
|} in
      Ok ("text/markdown", body)
  | uri when starts_with ~prefix:"figma://tokens/" uri ->
      let (base, q) = split_query uri in
      let prefix = "figma://tokens/" in
      let file_key =
        String.sub base (String.length prefix) (String.length base - String.length prefix)
        |> String.trim
      in
      let format = List.assoc_opt "format" q |> Option.value ~default:"resolved" in
      (match Sys.getenv_opt "FIGMA_TOKEN" with
       | None ->
           Error "Missing FIGMA_TOKEN env var (required for figma://tokens/{file_key})"
       | Some token ->
           if file_key = "" then
             Error "Missing file_key in resource URI. Use figma://tokens/{file_key}"
           else
             (match fetch_variables_cached ~file_key ~token with
              | Error err -> Error err
              | Ok (json, source) ->
                  (match format with
                   | "raw" ->
                       Ok ("application/json", Yojson.Safe.pretty_to_string json)
                   | "resolved" ->
                       let resolved = resolve_variables json in
                       Ok ("application/json", Yojson.Safe.pretty_to_string resolved)
                   | "dtcg" ->
                       let resolved = resolve_variables json in
                       let assoc_string_opt key = function
                         | `Assoc fields -> (
                             match List.assoc_opt key fields with
                             | Some (`String s) -> Some s
                             | _ -> None)
                         | _ -> None
                       in
                       let assoc_value key = function
                         | `Assoc fields -> List.assoc_opt key fields |> Option.value ~default:`Null
                         | _ -> `Null
                       in
                       let resolved_map =
                         match resolved with
                         | `Assoc fields -> (
                             match List.assoc_opt "resolved" fields with
                             | Some (`Assoc vars) -> vars
                             | _ -> [])
                         | _ -> []
                       in
                       let name_segments name =
                         name
                         |> String.split_on_char '/'
                         |> List.map String.trim
                         |> List.filter (fun s -> s <> "")
                       in
                       let path_of_segments segs = String.concat "/" segs in
                       let id_to_path =
                         resolved_map
                         |> List.filter_map (fun (var_id, var_json) ->
                           match assoc_string_opt "name" var_json with
                           | None -> None
                           | Some name ->
                               let segs = name_segments name in
                               let segs = if segs = [] then [var_id] else segs in
                               Some (var_id, path_of_segments segs))
                       in
                       let is_token_obj = function
                         | `Assoc fields ->
                             List.exists (fun (k, _) -> k = "$value" || k = "$type") fields
                         | _ -> false
                       in
                       let ensure_group_obj v =
                         match v with
                         | `Assoc _ when is_token_obj v -> `Assoc [("$self", v)]
                         | `Assoc _ as a -> a
                         | _ -> `Assoc []
                       in
                       let rec insert_token tree segments token_obj =
                         match (tree, segments) with
                         | (`Assoc fields, []) -> `Assoc fields
                         | (`Assoc fields, [last]) ->
                             let existing = List.assoc_opt last fields in
                             let fields_no_last = List.remove_assoc last fields in
                             (match existing with
                              | None -> `Assoc ((last, token_obj) :: fields_no_last)
                              | Some ex ->
                                  if is_token_obj ex then
                                    `Assoc ((last, token_obj) :: fields_no_last)
                                  else
                                    let group = ensure_group_obj ex in
                                    let group_fields = match group with `Assoc gf -> gf | _ -> [] in
                                    let group_fields = ("$self", token_obj) :: (List.remove_assoc "$self" group_fields) in
                                    `Assoc ((last, `Assoc group_fields) :: fields_no_last))
                         | (`Assoc fields, seg :: rest) ->
                             let child = List.assoc_opt seg fields |> Option.value ~default:(`Assoc []) |> ensure_group_obj in
                             let child' = insert_token child rest token_obj in
                             let fields_no_seg = List.remove_assoc seg fields in
                             `Assoc ((seg, child') :: fields_no_seg)
                         | (_other, _segments) -> tree
                       in
                       let dtcg_type resolved_type =
                         match String.lowercase_ascii resolved_type with
                         | "float" -> "number"
                         | other -> other
                       in
                       let alias_target_id = function
                         | `Assoc fields -> (
                             match List.assoc_opt "type" fields, List.assoc_opt "id" fields with
                             | Some (`String "VARIABLE_ALIAS"), Some (`String id) -> Some id
                             | _ -> None)
                         | _ -> None
                       in
                       let build_token var_id var_json =
                         let name = assoc_string_opt "name" var_json |> Option.value ~default:var_id in
                         let segs = name_segments name in
                         let segs = if segs = [] then [var_id] else segs in
                         let resolved_type =
                           assoc_string_opt "resolvedType" var_json |> Option.value ~default:"unknown"
                         in
                         let raw_value = assoc_value "defaultValue" var_json in
                         let value =
                           match alias_target_id raw_value with
                           | None -> raw_value
                           | Some target_id ->
                               (match List.assoc_opt target_id id_to_path with
                                | Some p -> `String ("{" ^ p ^ "}")
                                | None -> raw_value)
                         in
                         let figma_ext =
                           `Assoc [
                             ("id", `String var_id);
                             ("name", `String name);
                             ("collectionId", assoc_value "collectionId" var_json);
                             ("defaultModeId", assoc_value "defaultModeId" var_json);
                             ("defaultModeName", assoc_value "defaultModeName" var_json);
                             ("resolvedType", `String resolved_type);
                           ]
                         in
                         let token_obj =
                           `Assoc [
                             ("$type", `String (dtcg_type resolved_type));
                             ("$value", value);
                             ("$extensions", `Assoc [("figma", figma_ext)]);
                           ]
                         in
                         (segs, token_obj)
                       in
                       let tree =
                         List.fold_left (fun acc (var_id, var_json) ->
                           let (segs, token_obj) = build_token var_id var_json in
                           insert_token acc segs token_obj
                         ) (`Assoc []) resolved_map
                       in
                       let meta =
                         `Assoc [
                           ("figmaFileKey", `String file_key);
                           ("variablesSource", source);
                           ("generatedAtUnixMs", `Int (int_of_float (Unix.gettimeofday () *. 1000.0)));
                         ]
                       in
                       let tree_fields =
                         match tree with
                         | `Assoc fields -> fields
                         | _ -> []
                       in
                       let out = `Assoc (("$extensions", `Assoc [("figma", meta)]) :: tree_fields) in
                       Ok ("application/json", Yojson.Safe.pretty_to_string out)
                   | _ ->
                       Error "Invalid format. Use raw|resolved|dtcg")))
  | _ -> Error "Resource not found"

(** ============== 서버 생성 ============== *)

let create_figma_server () =
  Figma_mcp_protocol.create_server
    ~handlers_sync:all_handlers_sync
    ~resource_templates
    public_tools
    resources
    prompts
    read_resource
