(** Figma MCP Tools 정의 *)

open Mcp_protocol
open Printf

(** ============== JSON → DSL 변환 (Figma_mcp 순환 의존 방지) ============== *)
let process_json_string ~format json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    Ok (match format with
        | "fidelity" | "pixel" | "accuracy" -> Figma_codegen.generate_fidelity json
        | "raw" -> Yojson.Safe.pretty_to_string json
        | "html" -> (
            let node_json =
              match Figma_api.extract_document json with
              | Some d -> d
              | None -> json
            in
            match Figma_parser.parse_json node_json with
            | None -> "Failed to parse JSON for HTML output"
            | Some node -> Figma_codegen.generate_html node
          )
        | _ -> "Unknown format (use fidelity, raw, or html)")
  with
  | Yojson.Json_error _ -> Error "Failed to parse JSON"

(** ============== JSON Schema 헬퍼 ============== *)

let string_prop ?(required=false) desc : Yojson.Safe.t =
  ignore required;
  `Assoc [("type", `String "string"); ("description", `String desc)]

let number_prop desc : Yojson.Safe.t =
  `Assoc [("type", `String "number"); ("description", `String desc)]

let bool_prop desc : Yojson.Safe.t =
  `Assoc [("type", `String "boolean"); ("description", `String desc)]

let enum_prop options desc : Yojson.Safe.t =
  `Assoc [
    ("type", `String "string");
    ("enum", `List (List.map (fun s -> `String s) options));
    ("description", `String desc);
  ]

let array_prop desc : Yojson.Safe.t =
  `Assoc [
    ("type", `String "array");
    ("description", `String desc);
  ]

let object_prop desc : Yojson.Safe.t =
  `Assoc [
    ("type", `String "object");
    ("description", `String desc);
  ]

let object_schema props required : Yojson.Safe.t =
  `Assoc [
    ("type", `String "object");
    ("properties", `Assoc props);
    ("required", `List (List.map (fun s -> `String s) required));
  ]

(** ============== 캐시 헬퍼 ============== *)

let variables_cache_node_id = "__variables__"

let fetch_variables_cached ~file_key ~token =
  let cached_json =
    Figma_cache.get ~file_key ~node_id:variables_cache_node_id
      ~ttl_hours:Figma_cache.Config.ttl_variables_hours ()
  in
  match cached_json with
  | Some json -> Ok (json, `String "cache")
  | None ->
      (match Figma_effects.Perform.get_variables ~token ~file_key with
       | Ok json ->
           Figma_cache.set ~file_key ~node_id:variables_cache_node_id json;
           Ok (json, `String "rest")
       | Error err -> Error err)

(** ============== Tool 정의 ============== *)

let tool_figma_codegen : tool_def = {
  name = "figma_codegen";
  description = "Figma JSON을 정확도 우선 Fidelity DSL로 변환합니다.";
  input_schema = object_schema [
    ("json", string_prop "Figma JSON 데이터 (document 노드 또는 전체 응답)");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷: fidelity (정확도 우선), raw (원본 JSON), html (HTML 프리뷰)");
  ] ["json"];
}

let tool_figma_get_file : tool_def = {
  name = "figma_get_file";
  description = "Figma 파일 데이터를 가져와 Fidelity DSL로 변환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키 (URL에서 추출: figma.com/file/KEY/...)");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷 (기본값: fidelity)");
    ("depth", number_prop "트리 깊이 제한 (Figma API depth 파라미터)");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"; "token"];
}

let tool_figma_get_file_meta : tool_def = {
  name = "figma_get_file_meta";
  description = "Figma 파일의 컴포넌트/스타일 메타데이터를 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"; "token"];
}

let tool_figma_list_screens : tool_def = {
  name = "figma_list_screens";
  description = "Figma 파일 내 모든 화면(Frame/Component) 목록을 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
  ] ["file_key"; "token"];
}

let tool_figma_get_node : tool_def = {
  name = "figma_get_node";
  description = "특정 노드 ID의 데이터를 가져와 Fidelity DSL로 변환합니다. (전체 재귀는 gRPC GetNodeStream recursive 사용 권장)";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷");
    ("depth", number_prop "트리 깊이 제한 (Figma API depth 파라미터)");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["token"];
}

let tool_figma_get_node_with_image : tool_def = {
  name = "figma_get_node_with_image";
  description = "특정 노드의 Fidelity DSL과 이미지 URL을 동시에 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "DSL 출력 포맷 (기본값: fidelity)");
    ("image_format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "이미지 포맷 (기본값: png)");
    ("scale", number_prop "스케일 (1-4, 기본값: 1)");
    ("use_absolute_bounds", bool_prop "효과 포함한 렌더 바운즈 사용 여부");
    ("download", bool_prop "이미지 다운로드 여부 (기본값: false)");
    ("save_dir", string_prop "다운로드 저장 경로 (기본값: ~/me/download/figma-assets)");
    ("depth", number_prop "트리 깊이 제한 (Figma API depth 파라미터)");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["token"];
}

let tool_figma_get_node_bundle : tool_def = {
  name = "figma_get_node_bundle";
  description = "정확도 극대화 번들: 노드 DSL + 렌더 이미지 + 메타/변수/이미지 fills/플러그인 보강을 한번에 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "DSL 출력 포맷 (기본값: fidelity)");
    ("image_format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "이미지 포맷 (기본값: png)");
    ("scale", number_prop "스케일 (1-4, 기본값: 1)");
    ("use_absolute_bounds", bool_prop "효과 포함한 렌더 바운즈 사용 여부");
    ("download", bool_prop "이미지/에셋 다운로드 여부 (기본값: false)");
    ("save_dir", string_prop "다운로드 저장 경로 (기본값: ~/me/download/figma-assets)");
    ("include_raw", bool_prop "node_raw 포함 여부 (기본값: true)");
    ("include_meta", bool_prop "file meta 포함 여부 (기본값: true)");
    ("include_variables", bool_prop "변수/해석 포함 여부 (기본값: true)");
    ("include_image_fills", bool_prop "image fills 포함 여부 (기본값: true)");
    ("include_plugin", bool_prop "플러그인 스냅샷 포함 여부 (기본값: false)");
    ("auto_plugin", bool_prop "url 제공 시 플러그인 자동 포함 (기본값: url 존재 시 true)");
    ("include_plugin_variables", bool_prop "플러그인 변수 보강 포함 여부 (기본값: false)");
    ("include_plugin_image", bool_prop "플러그인 이미지(base64) 포함 여부 (기본값: false)");
    ("plugin_include_geometry", bool_prop "플러그인 스냅샷에 벡터/지오메트리 포함 여부 (기본값: false)");
    ("plugin_depth", number_prop "플러그인 스냅샷 depth (기본값: Figma depth 또는 6)");
    ("plugin_image_format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "플러그인 이미지 포맷 (기본값: png)");
    ("plugin_image_scale", number_prop "플러그인 이미지 스케일 (기본값: 1)");
    ("plugin_channel_id", string_prop "플러그인 채널 ID (옵션)");
    ("plugin_timeout_ms", number_prop "플러그인 응답 대기 시간 (기본값: 20000)");
    ("depth", number_prop "트리 깊이 제한 (Figma API depth 파라미터)");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["token"];
}

(** 경량 구조 요약 - 큰 노드를 탐색할 때 전체 로드 없이 구조 파악 *)
let tool_figma_get_node_summary : tool_def = {
  name = "figma_get_node_summary";
  description = "노드의 경량 구조 요약을 반환합니다. 전체 콘텐츠 없이 자식 노드 목록, 타입, 예상 크기만 포함하여 대형 노드 탐색에 적합합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token");
    ("max_children", number_prop "반환할 최대 자식 수 (기본값: 50)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["token"];
}

(** 노드 자동 선택 - 점수 기반 후보 선별 *)
let tool_figma_select_nodes : tool_def = {
  name = "figma_select_nodes";
  description = "URL/노드 기준으로 후보 노드를 점수화해 선택 목록과 노트 텍스트를 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token");
    ("summary_depth", number_prop "분석 depth (기본값: 1)");
    ("preview", bool_prop "프리뷰 이미지 포함 여부 (기본값: true)");
    ("preview_format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "프리뷰 이미지 포맷 (기본값: png)");
    ("preview_scale", number_prop "프리뷰 이미지 스케일 (1-4, 기본값: 1)");
    ("layout_only", bool_prop "컨테이너 위주 선택 (기본값: false)");
    ("auto_layout_only", bool_prop "Auto-layout 노드만 선택 (기본값: false)");
    ("text_mode", enum_prop ["include"; "exclude"; "only"] "텍스트 노드 선택 모드 (기본값: include)");
    ("score_threshold", number_prop "선택 점수 임계값 (기본값: 2.0)");
    ("max_parents", number_prop "선택할 부모 노드 최대 개수 (기본값: 8)");
    ("exclude_patterns", array_prop "제외할 이름 패턴 (기본값: guide/spec/annotation 등)");
    ("note_patterns", array_prop "노트로 분리할 텍스트 패턴 (기본값: note/memo/설명 등)");
    ("notes_limit", number_prop "노트 텍스트 최대 개수 (기본값: 50)");
    ("excluded_limit", number_prop "제외 목록 최대 개수 (기본값: 50)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["token"];
}

(** 깊이 범위별 청크 로드 - 대형 노드를 점진적으로 로드 *)
let tool_figma_get_node_chunk : tool_def = {
  name = "figma_get_node_chunk";
  description = "특정 깊이 범위의 노드 데이터만 가져옵니다. 대형 노드를 점진적으로 로드할 때 사용합니다. depth_start=0, depth_end=2면 루트부터 2단계까지만 반환.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token");
    ("depth_start", number_prop "시작 깊이 (기본값: 0)");
    ("depth_end", number_prop "종료 깊이 (기본값: 2)");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷 (기본값: fidelity)");
    ("include_styles", bool_prop "스타일 정의 포함 여부 (기본값: false)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["token"];
}

let tool_figma_chunk_index : tool_def = {
  name = "figma_chunk_index";
  description = "Figma DSL을 청킹한 뒤 청크 인덱스와 요약 통계를 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["fidelity"; "raw"] "청킹 대상 포맷 (기본값: fidelity)");
    ("depth", number_prop "Figma API depth");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("chunk_size", number_prop "청크당 children 수 (기본값: 50)");
    ("sample_size", number_prop "인덱스 샘플 크기 (기본값: 6)");
    ("context_max_depth", number_prop "컨텍스트 최대 깊이 (기본값: 6)");
    ("context_max_children", number_prop "컨텍스트 자식 최대 수 (기본값: 200)");
    ("context_max_list_items", number_prop "컨텍스트 리스트 최대 항목 수 (기본값: 200)");
    ("context_max_string", number_prop "컨텍스트 문자열 최대 길이 (기본값: 2000)");
    ("selection_mode", enum_prop ["none"; "heuristic"; "llm"] "청크 선택 전략 (기본값: none)");
    ("selection_limit", number_prop "선택할 청크 수 (기본값: 4)");
    ("selection_task", string_prop "청크 선택 기준 설명 (옵션)");
    ("selection_provider", enum_prop ["mcp-http"; "stub"] "LLM provider (기본값: mcp-http)");
    ("selection_llm_tool", enum_prop ["codex"; "claude-cli"; "gemini"; "ollama"] "LLM 도구 (기본값: codex)");
    ("selection_llm_args", object_prop "LLM 호출 인자 (model/timeout/...)");
    ("selection_mcp_url", string_prop "MCP endpoint URL override");
  ] ["token"];
}

let tool_figma_chunk_get : tool_def = {
  name = "figma_chunk_get";
  description = "청크 인덱스에서 특정 청크 데이터를 가져옵니다.";
  input_schema = object_schema [
    ("file_path", string_prop "청크 파일 경로 (figma_chunk_index 결과)");
    ("chunk_index", number_prop "청크 인덱스 (1-based)");
  ] ["file_path"; "chunk_index"];
}

let tool_figma_fidelity_loop : tool_def = {
  name = "figma_fidelity_loop";
  description = "DSL coverage 기반 fidelity 점수가 목표 미달이면 depth/geometry를 올리며 재조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token");
    ("target_score", number_prop "목표 fidelity score (0-1, 기본값: 0.92)");
    ("start_depth", number_prop "초기 depth (기본값: 4)");
    ("depth_step", number_prop "depth 증가폭 (기본값: 4)");
    ("max_depth", number_prop "최대 depth (기본값: 20)");
    ("max_attempts", number_prop "최대 시도 횟수 (기본값: 4)");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("format", enum_prop ["fidelity"] "현재 fidelity만 지원");
    ("include_meta", bool_prop "파일 메타 포함 여부 (기본값: true)");
    ("include_variables", bool_prop "변수/해석 포함 여부 (기본값: true)");
    ("include_image_fills", bool_prop "image fills 포함 여부 (기본값: true)");
    ("include_plugin", bool_prop "플러그인 스냅샷 포함 여부 (기본값: false)");
    ("auto_plugin", bool_prop "url 제공 시 플러그인 자동 포함 (기본값: url 존재 시 true)");
    ("include_plugin_variables", bool_prop "플러그인 변수 보강 포함 여부 (기본값: false)");
    ("plugin_channel_id", string_prop "플러그인 채널 ID (옵션)");
    ("plugin_depth", number_prop "플러그인 depth (기본값: 6)");
    ("plugin_timeout_ms", number_prop "플러그인 응답 대기 시간 (기본값: 20000)");
  ] ["token"];
}

let tool_figma_image_similarity : tool_def = {
  name = "figma_image_similarity";
  description = "렌더 이미지 SSIM/PSNR 비교로 정확도를 평가합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_a_id", string_prop "기준 노드 ID");
    ("node_b_id", string_prop "비교 노드 ID");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["png"; "jpg"] "이미지 포맷 (기본값: png)");
    ("start_scale", number_prop "시작 스케일 (기본값: 1)");
    ("max_scale", number_prop "최대 스케일 (기본값: start_scale)");
    ("scale_step", number_prop "스케일 증가폭 (기본값: 1)");
    ("target_ssim", number_prop "목표 SSIM (0-1, 옵션)");
    ("use_absolute_bounds", bool_prop "효과 포함한 렌더 바운즈 사용 여부");
    ("version", string_prop "특정 파일 버전 ID");
    ("save_dir", string_prop "이미지 저장 경로 (기본값: ~/me/download/figma-assets/compare)");
  ] ["file_key"; "node_a_id"; "node_b_id"; "token"];
}

(** Visual Feedback Loop - 코드 생성 및 시각적 검증 *)
let tool_figma_verify_visual : tool_def = {
  name = "figma_verify_visual";
  description = "코드를 생성하고 Figma 렌더와 비교하여 시각적 정확도(SSIM)와 텍스트 정확도를 검증합니다. SSIM과 TEXT 모두 통과해야 overall_passed=true. SSIM < target_ssim이면 자동으로 CSS를 조정합니다. 진화 과정은 자동으로 /tmp/figma-evolution/run_*에 저장됩니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("token", string_prop "Figma Personal Access Token");
    ("html", string_prop "검증할 HTML 코드 (없으면 자동 생성)");
    ("target_ssim", number_prop "목표 SSIM (0-1, 기본값: 0.95)");
    ("max_iterations", number_prop "최대 반복 횟수 (기본값: 3)");
    ("width", number_prop "뷰포트 너비 (기본값: 375)");
    ("height", number_prop "뷰포트 높이 (기본값: 812)");
    ("version", string_prop "특정 파일 버전 ID");
    ("mode", enum_prop ["full"; "structure"; "icons"; "text"; "layout"] "비교 모드: full(전체), structure(레이아웃만), icons(아이콘만), text(텍스트만), layout(박스/컨테이너)");
    ("checkpoints", string_prop "사용자 정의 체크포인트 JSON 배열 [{name, x, y, width, height}]");
  ] ["file_key"; "node_id"; "token"];
}

(** Region-based comparison - 영역별 상세 비교 *)
let tool_figma_compare_regions : tool_def = {
  name = "figma_compare_regions";
  description = "두 이미지의 특정 영역들을 비교합니다. 아이콘, 헤더, 푸터 등 개별 요소의 정확도를 측정할 때 사용합니다.";
  input_schema = object_schema [
    ("image_a", string_prop "기준 이미지 경로 (Figma 렌더)");
    ("image_b", string_prop "비교 이미지 경로 (HTML 렌더)");
    ("regions", string_prop "비교할 영역 JSON 배열 [{name, x, y, width, height}]");
    ("output_dir", string_prop "결과 저장 디렉토리 (기본값: /tmp/figma-evolution/regions)");
    ("generate_diff", bool_prop "차이 이미지 생성 여부 (기본값: true)");
  ] ["image_a"; "image_b"; "regions"];
}

(** Evolution Report - 진화 과정 리포트 조회 *)
let tool_figma_evolution_report : tool_def = {
  name = "figma_evolution_report";
  description = "Visual Feedback Loop의 진화 과정 리포트를 조회합니다. run_dir 없이 호출하면 최근 실행 목록을 반환하고, run_dir를 지정하면 해당 실행의 상세 리포트를 생성합니다.";
  input_schema = object_schema [
    ("run_dir", string_prop "Evolution 디렉토리 경로 (예: /tmp/figma-evolution/run_1234567890). 없으면 최근 실행 목록 반환");
    ("generate_image", bool_prop "비교 이미지 자동 생성 여부 (기본값: true)");
  ] [];
}

(** Compare Elements - 색상/박스 확장 메트릭 비교 *)
let tool_figma_compare_elements : tool_def = {
  name = "figma_compare_elements";
  description = "두 요소(색상 또는 박스)의 확장 메트릭을 비교합니다. 색상: OKLab, CIEDE2000, RGB Euclidean. 박스: IoU, GIoU, DIoU. Figma 시안과 구현체 비교에 유용합니다.";
  input_schema = object_schema [
    ("type", enum_prop ["color"; "box"; "full"] "비교 타입: color(색상), box(박스), full(둘 다)");
    ("color1", string_prop "첫 번째 색상 (#RRGGBB 또는 rgb(r,g,b))");
    ("color2", string_prop "두 번째 색상 (#RRGGBB 또는 rgb(r,g,b))");
    ("box1", string_prop "첫 번째 박스 (x,y,w,h 형식)");
    ("box2", string_prop "두 번째 박스 (x,y,w,h 형식)");
  ] ["type"];
}

let tool_figma_export_image : tool_def = {
  name = "figma_export_image";
  description = "노드를 이미지로 내보내기 위한 URL을 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_ids", string_prop "노드 ID들 (쉼표 구분)");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "이미지 포맷");
    ("scale", number_prop "스케일 (1-4, 기본값: 1)");
    ("use_absolute_bounds", bool_prop "효과 포함한 렌더 바운즈 사용 여부");
    ("version", string_prop "특정 파일 버전 ID");
    ("download", bool_prop "이미지 다운로드 여부 (기본값: false)");
    ("save_dir", string_prop "다운로드 저장 경로 (기본값: ~/me/download/figma-assets)");
  ] ["file_key"; "node_ids"; "token"];
}

let tool_figma_get_image_fills : tool_def = {
  name = "figma_get_image_fills";
  description = "파일 내 이미지 채움(image fills) 원본 URL 맵을 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("version", string_prop "특정 파일 버전 ID");
    ("download", bool_prop "이미지 다운로드 여부 (기본값: false)");
    ("save_dir", string_prop "다운로드 저장 경로 (기본값: ~/me/download/figma-assets)");
  ] ["file_key"; "token"];
}

let tool_figma_get_nodes : tool_def = {
  name = "figma_get_nodes";
  description = "여러 노드 ID의 데이터를 한 번에 가져옵니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_ids", string_prop "노드 ID들 (쉼표 구분: 1:2,3:4)");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["raw"; "fidelity"; "html"] "출력 포맷 (기본값: raw)");
    ("depth", number_prop "트리 깊이 제한");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"; "node_ids"; "token"];
}

let tool_figma_get_file_versions : tool_def = {
  name = "figma_get_file_versions";
  description = "파일 버전 목록을 조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
  ] ["file_key"; "token"];
}

let tool_figma_get_file_comments : tool_def = {
  name = "figma_get_file_comments";
  description = "파일 코멘트 목록을 조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
  ] ["file_key"; "token"];
}

let tool_figma_post_comment : tool_def = {
  name = "figma_post_comment";
  description = "파일에 코멘트를 추가합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("message", string_prop "코멘트 내용");
    ("x", number_prop "캔버스 좌표 x (client_meta)");
    ("y", number_prop "캔버스 좌표 y (client_meta)");
    ("node_id", string_prop "연결할 노드 ID (옵션)");
  ] ["file_key"; "token"; "message"; "x"; "y"];
}

let tool_figma_get_file_components : tool_def = {
  name = "figma_get_file_components";
  description = "파일의 컴포넌트 목록을 조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
  ] ["file_key"; "token"];
}

let tool_figma_get_team_components : tool_def = {
  name = "figma_get_team_components";
  description = "팀의 컴포넌트 목록을 조회합니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token");
  ] ["team_id"; "token"];
}

let tool_figma_get_file_component_sets : tool_def = {
  name = "figma_get_file_component_sets";
  description = "파일의 컴포넌트 셋 목록을 조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
  ] ["file_key"; "token"];
}

let tool_figma_get_team_component_sets : tool_def = {
  name = "figma_get_team_component_sets";
  description = "팀의 컴포넌트 셋 목록을 조회합니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token");
  ] ["team_id"; "token"];
}

let tool_figma_get_file_styles : tool_def = {
  name = "figma_get_file_styles";
  description = "파일의 스타일 목록을 조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
  ] ["file_key"; "token"];
}

let tool_figma_get_team_styles : tool_def = {
  name = "figma_get_team_styles";
  description = "팀의 스타일 목록을 조회합니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token");
  ] ["team_id"; "token"];
}

let tool_figma_get_component : tool_def = {
  name = "figma_get_component";
  description = "컴포넌트 키로 상세 정보를 조회합니다.";
  input_schema = object_schema [
    ("component_key", string_prop "컴포넌트 키");
    ("token", string_prop "Figma Personal Access Token");
  ] ["component_key"; "token"];
}

let tool_figma_get_component_set : tool_def = {
  name = "figma_get_component_set";
  description = "컴포넌트 셋 키로 상세 정보를 조회합니다.";
  input_schema = object_schema [
    ("component_set_key", string_prop "컴포넌트 셋 키");
    ("token", string_prop "Figma Personal Access Token");
  ] ["component_set_key"; "token"];
}

let tool_figma_get_style : tool_def = {
  name = "figma_get_style";
  description = "스타일 키로 상세 정보를 조회합니다.";
  input_schema = object_schema [
    ("style_key", string_prop "스타일 키");
    ("token", string_prop "Figma Personal Access Token");
  ] ["style_key"; "token"];
}

(** ============== Plugin Bridge 도구 ============== *)

let tool_figma_plugin_connect : tool_def = {
  name = "figma_plugin_connect";
  description = "Figma Plugin 채널을 생성하거나 연결합니다.";
  input_schema = object_schema [
    ("channel_id", string_prop "기존 채널 ID (옵션)");
  ] [];
}

let tool_figma_plugin_use_channel : tool_def = {
  name = "figma_plugin_use_channel";
  description = "기본 채널 ID를 설정합니다.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID");
  ] ["channel_id"];
}

let tool_figma_plugin_status : tool_def = {
  name = "figma_plugin_status";
  description = "현재 연결된 플러그인 채널 상태를 확인합니다.";
  input_schema = object_schema [] [];
}

let tool_figma_plugin_read_selection : tool_def = {
  name = "figma_plugin_read_selection";
  description = "플러그인에서 현재 선택된 노드 정보를 가져옵니다.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("depth", number_prop "자식 탐색 깊이 (기본값: 6)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
}

let tool_figma_plugin_get_node : tool_def = {
  name = "figma_plugin_get_node";
  description = "플러그인에서 특정 노드 정보를 가져옵니다.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (node_id 자동 추출)");
    ("depth", number_prop "자식 탐색 깊이 (기본값: 6)");
    ("include_geometry", bool_prop "벡터/지오메트리 포함 여부 (기본값: true)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
}

let tool_figma_plugin_export_node_image : tool_def = {
  name = "figma_plugin_export_node_image";
  description = "플러그인 exportAsync로 노드 이미지를 base64로 반환합니다.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (node_id 자동 추출)");
    ("format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "이미지 포맷 (기본값: png)");
    ("scale", number_prop "스케일 (기본값: 1)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
}

let tool_figma_plugin_get_variables : tool_def = {
  name = "figma_plugin_get_variables";
  description = "플러그인 Variables API로 로컬 변수/컬렉션을 가져옵니다.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
}

let tool_figma_plugin_apply_ops : tool_def = {
  name = "figma_plugin_apply_ops";
  description = "플러그인으로 노드 생성/수정/삭제 작업을 요청합니다.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("ops", array_prop "작업 목록 (create/update/delete 오브젝트 배열)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] ["ops"];
}

(** ============== LLM Bridge 도구 ============== *)

let tool_figma_llm_call : tool_def = {
  name = "figma_llm_call";
  description = "MCP endpoint를 통해 codex/claude-cli/gemini/ollama를 호출합니다.";
  input_schema = object_schema [
    ("provider", enum_prop ["mcp-http"; "stub"] "LLM provider (기본값: mcp-http)");
    ("llm_provider", string_prop "provider alias (하위 호환)");
    ("llm_tool", enum_prop ["codex"; "claude-cli"; "gemini"; "ollama"] "MCP tool 이름 (기본값: codex)");
    ("tool_name", string_prop "MCP tool 이름 override (llm_tool alias)");
    ("arguments", object_prop "MCP tool arguments (prompt/model/...)");
    ("prompt", string_prop "prompt 바로 전달 (arguments.prompt가 없을 때 사용)");
    ("response_format", enum_prop ["verbose"; "compact"; "binary"; "base85"; "compressed"; "auto"]
      "llm-mcp 응답 포맷 (기본값: verbose)");
    ("mcp_url", string_prop "MCP endpoint URL override");
    ("llm_url", string_prop "MCP endpoint alias (하위 호환)");
    ("return_metadata", bool_prop "raw JSON 및 메타데이터 반환 여부 (기본값: false)");
  ] [];
}

let tool_figma_llm_task : tool_def = {
  name = "figma_llm_task";
  description = "Figma DSL + Plugin 스냅샷을 컨텍스트로 MCP LLM 작업을 수행합니다.";
  input_schema = object_schema [
    ("task", string_prop "LLM 작업 지시문 (필수)");
    ("preset", enum_prop ["draft"; "balanced"; "fidelity"; "text"; "icon"] "LLM 작업 프리셋 (기본값: 없음)");
    ("quality", enum_prop ["best"; "balanced"; "fast"] "컨텍스트/속도 프리셋 (기본값: best)");
    ("provider", enum_prop ["mcp-http"; "stub"] "LLM provider (기본값: mcp-http)");
    ("llm_provider", string_prop "provider alias (하위 호환)");
    ("llm_tool", enum_prop ["auto"; "codex"; "claude-cli"; "gemini"; "ollama"] "MCP tool 이름 (기본값: codex)");
    ("tool_name", string_prop "MCP tool 이름 override (llm_tool alias)");
    ("llm_tool_selector_mode", enum_prop ["heuristic"; "llm"] "LLM 도구 선택 전략 (기본값: heuristic)");
    ("llm_tool_selector_tool", enum_prop ["codex"; "claude-cli"; "gemini"; "ollama"] "LLM 도구 선택용 LLM 도구 (기본값: gemini)");
    ("llm_tool_selector_provider", enum_prop ["mcp-http"; "stub"] "LLM 도구 선택 provider (기본값: mcp-http)");
    ("llm_tool_selector_args", object_prop "LLM 도구 선택용 LLM 인자");
    ("llm_tool_selector_task", string_prop "LLM 도구 선택 기준 설명 (옵션)");
    ("llm_tool_selector_mcp_url", string_prop "LLM 도구 선택 MCP endpoint URL override");
    ("llm_args", object_prop "MCP tool arguments (model/timeout/...)");
    ("mcp_url", string_prop "MCP endpoint URL override");
    ("llm_url", string_prop "MCP endpoint alias (하위 호환)");
    ("file_key", string_prop "Figma 파일 키 (DSL/변수/이미지 fill 추출용)");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token");
    ("depth", number_prop "Figma API depth");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("include_variables", bool_prop "변수 포함 여부 (기본값: quality에 따라 자동)");
    ("include_image_fills", bool_prop "이미지 fill 포함 여부 (기본값: quality에 따라 자동)");
    ("include_plugin", bool_prop "플러그인 스냅샷 포함 여부 (기본값: quality에 따라 자동)");
    ("auto_plugin", bool_prop "url 제공 시 플러그인 자동 포함 (기본값: url 존재 시 true)");
    ("plugin_channel_id", string_prop "플러그인 채널 ID (옵션)");
    ("plugin_mode", enum_prop ["selection"; "node"] "플러그인 스냅샷 모드 (기본값: selection)");
    ("plugin_depth", number_prop "플러그인 depth (기본값: 0)");
    ("plugin_include_geometry", bool_prop "플러그인 지오메트리 포함 여부 (기본값: false)");
    ("plugin_timeout_ms", number_prop "플러그인 응답 대기 시간 (기본값: 20000)");
    ("plugin_context_mode", enum_prop ["full"; "summary"; "both"] "플러그인 컨텍스트 모드 (기본값: full)");
    ("plugin_summary_sample_size", number_prop "플러그인 요약 샘플 수 (기본값: 5)");
    ("context_strategy", enum_prop ["raw"; "compact"; "chunked"] "컨텍스트 압축/청킹 전략 (기본값: raw)");
    ("context_max_depth", number_prop "컨텍스트 최대 깊이 (compact/chunked, 기본값: 6)");
    ("context_max_children", number_prop "컨텍스트 자식 최대 수 (compact/chunked, 기본값: 200)");
    ("context_max_list_items", number_prop "컨텍스트 리스트 최대 항목 수 (compact/chunked, 기본값: 200)");
    ("context_max_string", number_prop "컨텍스트 문자열 최대 길이 (compact/chunked, 기본값: 2000)");
    ("context_chunk_size", number_prop "chunked 모드에서 청크 크기 (기본값: 50)");
    ("chunk_select_mode", enum_prop ["none"; "heuristic"; "llm"] "청크 선택 전략 (기본값: none)");
    ("chunk_select_limit", number_prop "선택할 청크 수 (기본값: 4)");
    ("chunk_select_task", string_prop "청크 선택 기준 설명 (옵션)");
    ("chunk_select_llm_tool", enum_prop ["codex"; "claude-cli"; "gemini"; "ollama"] "청크 선택용 LLM 도구 (기본값: codex)");
    ("chunk_select_llm_args", object_prop "청크 선택용 LLM 인자 (model/timeout/...)");
    ("chunk_select_provider", enum_prop ["mcp-http"; "stub"] "청크 선택 LLM provider (기본값: mcp-http)");
    ("chunk_select_mcp_url", string_prop "청크 선택 MCP endpoint URL override");
    ("chunk_select_sample_size", number_prop "청크 인덱스 샘플 수 (기본값: 6)");
    ("llm_call_policy", enum_prop ["auto"; "require_ready"; "skip"; "force"] "LLM 호출 정책 (기본값: auto)");
    ("llm_dry_run", bool_prop "LLM 호출 없이 readiness 반환 (기본값: false)");
    ("preflight_max_truncation", number_prop "프리플라이트 트렁케이션 허용 비율 (0-1, 기본값: 0.2)");
    ("preflight_require_plugin", bool_prop "플러그인 스냅샷 필수 여부 (기본값: preset/quality에 따라 자동)");
    ("auto_fix_enabled", bool_prop "프리플라이트 실패 시 자동 보정 (기본값: true)");
    ("auto_fix_max_attempts", number_prop "자동 보정 재시도 횟수 (기본값: 2)");
    ("max_context_chars", number_prop "LLM 프롬프트 컨텍스트 최대 길이 (기본값: 120000)");
    ("retry_on_llm_error", bool_prop "LLM 에러 시 컨텍스트 축소 후 재시도 (기본값: false)");
    ("max_retries", number_prop "LLM 에러 재시도 횟수 (기본값: 1)");
    ("min_context_chars", number_prop "재시도 시 컨텍스트 최소 길이 (기본값: 120000)");
    ("retry_context_scale", number_prop "재시도 시 컨텍스트 축소 비율 (0-1, 기본값: 0.5)");
    ("critic_enabled", bool_prop "LLM 출력 품질 critic 사용 여부 (기본값: false)");
    ("critic_tool", enum_prop ["codex"; "claude-cli"; "gemini"; "ollama"] "critic LLM 도구 (기본값: gemini)");
    ("critic_provider", enum_prop ["mcp-http"; "stub"] "critic provider (기본값: mcp-http)");
    ("critic_args", object_prop "critic LLM 인자 (model/timeout/...)");
    ("critic_task", string_prop "critic 평가 기준 설명 (옵션)");
    ("critic_mcp_url", string_prop "critic MCP endpoint URL override");
    ("critic_min_score", number_prop "critic 수용 최소 점수 (0-1, 기본값: 0.7)");
    ("critic_max_retries", number_prop "critic 재시도 횟수 (기본값: 0)");
    ("critic_retry_context_scale", number_prop "critic 재시도 시 context 축소 비율 (0-1, 기본값: 0.7)");
    ("return_metadata", bool_prop "raw JSON 및 메타데이터 반환 여부 (기본값: false)");
  ] ["task"];
}

(** ============== Phase 1: 탐색 도구 ============== *)

let tool_figma_parse_url : tool_def = {
  name = "figma_parse_url";
  description = "Figma URL에서 team_id, project_id, file_key, node_id를 추출합니다. API 호출 없이 로컬에서 파싱합니다.";
  input_schema = object_schema [
    ("url", string_prop "Figma URL (팀/프로젝트/파일/노드 페이지 모두 지원)");
  ] ["url"];
}

let tool_figma_get_me : tool_def = {
  name = "figma_get_me";
  description = "현재 인증된 사용자 정보를 반환합니다.";
  input_schema = object_schema [
    ("token", string_prop "Figma Personal Access Token");
  ] ["token"];
}

let tool_figma_list_projects : tool_def = {
  name = "figma_list_projects";
  description = "팀의 모든 프로젝트 목록을 반환합니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID (URL에서 추출 또는 figma_parse_url 사용)");
    ("token", string_prop "Figma Personal Access Token");
  ] ["team_id"; "token"];
}

let tool_figma_list_files : tool_def = {
  name = "figma_list_files";
  description = "프로젝트의 모든 파일 목록을 반환합니다.";
  input_schema = object_schema [
    ("project_id", string_prop "프로젝트 ID");
    ("token", string_prop "Figma Personal Access Token");
  ] ["project_id"; "token"];
}

let tool_figma_get_variables : tool_def = {
  name = "figma_get_variables";
  description = "파일의 디자인 토큰/변수를 반환합니다 (색상, 타이포, 간격 등).";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["summary"; "raw"; "resolved"] "출력 포맷 (기본값: summary)");
  ] ["file_key"; "token"];
}

(** ============== Phase 2: 고급 쿼리 도구 ============== *)

let tool_figma_query : tool_def = {
  name = "figma_query";
  description = "노드를 조건으로 필터링합니다. SQL WHERE처럼 type, 크기, 색상 등으로 검색합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("node_id", string_prop "시작 노드 ID (생략시 전체 파일)");
    ("type", string_prop "노드 타입 필터 (FRAME, TEXT, COMPONENT 등, 쉼표 구분)");
    ("width_min", number_prop "최소 너비");
    ("width_max", number_prop "최대 너비");
    ("height_min", number_prop "최소 높이");
    ("height_max", number_prop "최대 높이");
    ("color", string_prop "색상 필터 (hex, 예: #FF0000)");
    ("name", string_prop "이름 패턴 (substring 매칭)");
    ("depth", number_prop "탐색 깊이 (1=자식만, 2=손자까지, 생략=무제한)");
    ("limit", number_prop "결과 개수 제한");
  ] ["file_key"; "token"];
}

let tool_figma_search : tool_def = {
  name = "figma_search";
  description = "텍스트 내용이나 이름으로 노드를 검색합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("query", string_prop "검색어 (텍스트 내용 또는 노드 이름)");
    ("search_in", enum_prop ["name"; "text"; "both"] "검색 대상 (기본값: both)");
    ("limit", number_prop "결과 개수 제한 (기본값: 20)");
  ] ["file_key"; "token"; "query"];
}

let tool_figma_compare : tool_def = {
  name = "figma_compare";
  description = "두 노드(또는 Web/Mobile 컴포넌트)를 비교하여 일관성을 검사합니다. 크기, 색상, 타이포그래피, 레이아웃 차이를 분석합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("node_a_id", string_prop "첫 번째 노드 ID (예: 100:200)");
    ("node_b_id", string_prop "두 번째 노드 ID");
    ("mode", enum_prop ["single"; "batch"] "비교 모드: single (단일 쌍), batch (Web/Mobile 일괄 매칭)");
    ("web_prefix", string_prop "Web 노드 이름 접두사 (batch 모드)");
    ("mobile_prefix", string_prop "Mobile 노드 이름 접두사 (batch 모드)");
  ] ["file_key"; "token"];
}

let tool_figma_tree : tool_def = {
  name = "figma_tree";
  description = "Figma 노드 트리를 시각적으로 표시합니다. ASCII 트리, 들여쓰기, 압축 포맷 지원.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("node_id", string_prop "시작 노드 ID (생략시 전체 문서)");
    ("style", enum_prop ["ascii"; "indent"; "compact"] "출력 스타일 (기본값: ascii)");
    ("max_depth", number_prop "최대 깊이 (기본값: 무제한)");
    ("show_size", enum_prop ["true"; "false"] "크기 표시 (기본값: true)");
    ("show_stats", enum_prop ["true"; "false"] "통계 포함 (기본값: false)");
  ] ["file_key"; "token"];
}

let tool_figma_stats : tool_def = {
  name = "figma_stats";
  description = "Figma 파일의 디자인 통계를 분석합니다. 색상, 폰트, 크기, 컴포넌트 사용 현황.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("node_id", string_prop "분석 시작 노드 ID (생략시 전체 문서)");
  ] ["file_key"; "token"];
}

let tool_figma_export_tokens : tool_def = {
  name = "figma_export_tokens";
  description = "Figma 파일에서 디자인 토큰을 추출합니다. CSS, Tailwind, JSON, Semantic DSL 포맷 지원.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token");
    ("format", enum_prop ["css"; "tailwind"; "json"; "semantic"] "출력 포맷 (기본값: css). semantic=UIFormer 스타일 DSL");
    ("node_id", string_prop "추출 시작 노드 ID (생략시 전체 문서)");
  ] ["file_key"; "token"];
}

(** 환경/의존성 점검 도구 *)
let tool_figma_doctor : tool_def = {
  name = "figma_doctor";
  description = "로컬 의존성(Node/Playwright/ImageMagick) 및 스크립트 경로를 점검합니다.";
  input_schema = object_schema [] [];
}

(** large_result 파일 읽기 *)
let tool_figma_read_large_result : tool_def = {
  name = "figma_read_large_result";
  description = "large_result로 저장된 파일을 offset/limit로 읽습니다.";
  input_schema = object_schema [
    ("file_path", string_prop "large_result file_path");
    ("offset", number_prop "읽기 시작 바이트 (기본값: 0)");
    ("limit", number_prop "최대 읽기 바이트 (기본값: 20000)");
  ] ["file_path"];
}

(** 캐시 관리 도구 *)
let tool_figma_cache_stats : tool_def = {
  name = "figma_cache_stats";
  description = "노드 캐시 통계를 조회합니다. L1(메모리) + L2(파일) 캐시 엔트리 수, TTL 설정 등.";
  input_schema = object_schema [] [];
}

let tool_figma_cache_invalidate : tool_def = {
  name = "figma_cache_invalidate";
  description = "노드 캐시를 무효화합니다. file_key와 node_id로 범위 지정 가능.";
  input_schema = object_schema [
    ("file_key", string_prop "무효화할 파일 키 (생략시 전체)");
    ("node_id", string_prop "무효화할 노드 ID (생략시 해당 파일 전체)");
  ] [];
}

(** ============== 모든 도구 목록 ============== *)

let all_tools = [
  (* 기존 도구 *)
  tool_figma_codegen;
  tool_figma_get_file;
  tool_figma_get_file_meta;
  tool_figma_list_screens;
  tool_figma_get_node;
  tool_figma_get_node_with_image;
  tool_figma_get_node_bundle;
  tool_figma_get_node_summary;
  tool_figma_select_nodes;
  tool_figma_get_node_chunk;
  tool_figma_chunk_index;
  tool_figma_chunk_get;
  tool_figma_fidelity_loop;
  tool_figma_image_similarity;
  tool_figma_verify_visual;
  tool_figma_compare_regions;
  tool_figma_evolution_report;
  tool_figma_compare_elements;
  tool_figma_export_image;
  tool_figma_get_image_fills;
  tool_figma_get_nodes;
  tool_figma_get_file_versions;
  tool_figma_get_file_comments;
  tool_figma_post_comment;
  tool_figma_get_file_components;
  tool_figma_get_team_components;
  tool_figma_get_file_component_sets;
  tool_figma_get_team_component_sets;
  tool_figma_get_file_styles;
  tool_figma_get_team_styles;
  tool_figma_get_component;
  tool_figma_get_component_set;
  tool_figma_get_style;
  tool_figma_plugin_connect;
  tool_figma_plugin_use_channel;
  tool_figma_plugin_status;
  tool_figma_plugin_read_selection;
  tool_figma_plugin_get_node;
  tool_figma_plugin_export_node_image;
  tool_figma_plugin_get_variables;
  tool_figma_plugin_apply_ops;
  tool_figma_llm_call;
  tool_figma_llm_task;
  (* Phase 1: 탐색 도구 *)
  tool_figma_parse_url;
  tool_figma_get_me;
  tool_figma_list_projects;
  tool_figma_list_files;
  tool_figma_get_variables;
  (* Phase 2: 고급 쿼리 *)
  tool_figma_query;
  tool_figma_search;
  tool_figma_compare;
  (* Phase 3: 분석/추출 *)
  tool_figma_tree;
  tool_figma_stats;
  tool_figma_export_tokens;
  tool_figma_doctor;
  tool_figma_read_large_result;
  (* 캐시 관리 *)
  tool_figma_cache_stats;
  tool_figma_cache_invalidate;
]

(** ============== Tool 핸들러 구현 ============== *)

let member key json =
  match json with
  | `Assoc lst -> List.assoc_opt key lst
  | _ -> None

let normalize_node_id value =
  Figma_api.normalize_node_id value

let normalize_node_id_key key value =
  match key with
  | "node_id" | "node_a_id" | "node_b_id" -> normalize_node_id value
  | _ -> value

let get_string key json =
  match member key json with
  | Some (`String s) -> Some (normalize_node_id_key key s)
  | _ -> None

let get_string_list key json =
  let trim = String.trim in
  match member key json with
  | Some (`List items) ->
      let values =
        items
        |> List.filter_map (function `String s -> Some (trim s) | _ -> None)
        |> List.filter (fun s -> s <> "")
      in
      if values = [] then None else Some values
  | Some (`String s) ->
      let values =
        s
        |> String.split_on_char ','
        |> List.map trim
        |> List.filter (fun v -> v <> "")
      in
      if values = [] then None else Some values
  | _ -> None

let prefer_some primary fallback =
  match primary with
  | Some _ -> primary
  | None -> fallback

let resolve_url_info args =
  match get_string "url" args with
  | Some url -> Some (Figma_api.parse_figma_url url)
  | None -> None

let resolve_file_key_node_id args =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  match resolve_url_info args with
  | None -> (file_key, node_id)
  | Some info ->
      let file_key = prefer_some file_key info.file_key in
      let node_id = prefer_some node_id info.node_id in
      (file_key, node_id)

let resolve_node_id args =
  match get_string "node_id" args with
  | Some _ as node_id -> node_id
  | None ->
      (match resolve_url_info args with
       | Some info -> info.node_id
       | None -> None)

let get_json key json =
  member key json

let get_bool key json =
  match member key json with
  | Some (`Bool b) -> Some b
  | _ -> None

let get_string_or key default json =
  match member key json with
  | Some (`String s) -> s
  | _ -> default

let get_float key json =
  match member key json with
  | Some (`Float f) -> Some f
  | Some (`Int i) -> Some (float_of_int i)
  | _ -> None

let get_int key json =
  match member key json with
  | Some (`Int i) -> Some i
  | Some (`Float f) -> Some (int_of_float f)
  | _ -> None

let get_float_or key default json =
  match get_float key json with
  | Some f -> f
  | None -> default

let get_bool_or key default json =
  match get_bool key json with
  | Some b -> b
  | None -> default

(** ============== Node selection helpers ============== *)

type selection_config = {
  layout_only: bool;
  auto_layout_only: bool;
  text_mode: string;
  score_threshold: float;
  max_parents: int;
  summary_depth: int;
  exclude_patterns: string list;
  note_patterns: string list;
  notes_limit: int;
  excluded_limit: int;
}

let default_exclude_patterns = [
  "guide"; "spec"; "measure"; "annotation"; "grid"; "flow"; "diagram"; "wip";
  "archive"; "note"; "memo"; "draft"; "unused"; "redline";
  "가이드"; "스펙"; "측정"; "주석"; "그리드"; "순서도"; "플로우"; "다이어그램";
  "메모"; "참고"; "설명"; "임시"; "미사용"; "가이드라인";
]

let default_note_patterns = [
  "note"; "memo"; "annotation"; "guide"; "spec"; "measure"; "as-is"; "as is";
  "to-be"; "to be";
  "주석"; "메모"; "참고"; "설명"; "가이드"; "스펙"; "측정"; "as-is"; "to-be";
]

let normalize_patterns patterns =
  patterns
  |> List.map String.trim
  |> List.filter (fun p -> p <> "")

let string_contains ~needle ~haystack =
  let needle = String.lowercase_ascii (String.trim needle) in
  if needle = "" then false
  else
    let haystack = String.lowercase_ascii haystack in
    try
      ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
      true
    with Not_found -> false

let matches_any patterns text =
  List.exists (fun p -> string_contains ~needle:p ~haystack:text) patterns

let find_matching_pattern patterns text =
  List.find_opt (fun p -> string_contains ~needle:p ~haystack:text) patterns

let node_text_blob node =
  match node.Figma_types.characters with
  | Some txt -> String.concat " " [node.Figma_types.name; txt]
  | None -> node.Figma_types.name

let node_is_text node =
  match node.Figma_types.node_type with
  | Figma_types.Text -> true
  | _ -> false

let node_is_container node =
  match node.Figma_types.node_type with
  | Figma_types.Document
  | Figma_types.Canvas
  | Figma_types.Frame
  | Figma_types.Group
  | Figma_types.Section
  | Figma_types.Component
  | Figma_types.ComponentSet
  | Figma_types.Instance -> true
  | _ -> false

let node_is_component node =
  match node.Figma_types.node_type with
  | Figma_types.Component
  | Figma_types.ComponentSet
  | Figma_types.Instance -> true
  | _ -> false

let node_has_image_fill node =
  List.exists
    (fun (paint : Figma_types.paint) ->
       paint.visible && paint.opacity > 0.01 && paint.paint_type = Figma_types.Image)
    node.Figma_types.fills

let node_area node =
  match node.Figma_types.bbox with
  | Some b -> max 0. (b.width *. b.height)
  | None -> 0.

let node_area_score area =
  Float.log10 (area +. 1.)

let node_has_auto_layout node =
  match node.Figma_types.layout_mode with
  | Figma_types.None' -> false
  | _ -> true

let node_has_mask_hint node =
  let text = node_text_blob node in
  matches_any ["mask"; "clip"] text

let node_duplicate_key node =
  let type_str = Figma_query.node_type_to_string node.Figma_types.node_type in
  let name = String.lowercase_ascii (String.trim node.Figma_types.name) in
  let size =
    match node.Figma_types.bbox with
    | Some b -> Printf.sprintf "%.0fx%.0f" b.width b.height
    | None -> "?"
  in
  String.concat "|" [type_str; name; size]

(** 실행 모드 설정 - HTTP 서버에서 변경됨 *)
let is_http_mode = ref false

(** Ensure effect handler in stdio mode for non-wrapped Lwt handlers. *)
let run_with_effects f =
  if !is_http_mode then f ()
  else Figma_effects.run_with_real_api f

let call_llm_tool ~(provider : Llm_provider.provider) ~url ~name ~arguments =
  if !is_http_mode then
    Lwt.return (Lwt_main.run (provider.call_tool ~url ~name ~arguments))
  else
    provider.call_tool ~url ~name ~arguments

let resolve_channel_id args =
  match get_string "channel_id" args with
  | Some id -> Ok id
  | None ->
      (match Figma_plugin_bridge.get_default_channel () with
       | Some id -> Ok id
       | None -> Error "Missing channel_id. Run figma_plugin_connect or figma_plugin_use_channel.")

let plugin_wait ~channel_id ~command_id ~timeout_ms =
  match Figma_plugin_bridge.wait_for_result ~channel_id ~command_id ~timeout_ms with
  | Some result -> Ok result
  | None -> Error "Plugin timeout waiting for response"

let assoc_or_empty json =
  match json with
  | `Assoc lst -> lst
  | _ -> []

let list_member key json =
  match member key json with
  | Some (`List lst) -> Some lst
  | _ -> None

let count_assoc_fields json =
  match json with
  | `Assoc lst -> List.length lst
  | _ -> 0

let count_list_items json =
  match json with
  | `List lst -> List.length lst
  | _ -> 0

let coverage_for_section json section_key missing_key weight =
  let present = member section_key json |> Option.value ~default:`Null |> count_assoc_fields in
  let missing = member missing_key json |> Option.value ~default:(`List []) |> count_list_items in
  let total = present + missing in
  let score = if total = 0 then 1.0 else (float_of_int present /. float_of_int total) in
  (score, present, missing, total, weight)

let fidelity_sections = [
  ("meta", "meta_missing", 0.4);
  ("structure", "structure_missing", 1.2);
  ("geometry", "geometry_missing", 1.2);
  ("vector", "vector_missing", 1.0);
  ("layout", "layout_missing", 2.0);
  ("paint", "paint_missing", 2.0);
  ("effects", "effects_missing", 1.0);
  ("text", "text_missing", 1.2);
  ("text_segments", "text_segments_missing", 1.0);
  ("instance", "instance_missing", 0.8);
  ("variables", "variables_missing", 0.6);
  ("variables_resolved", "variables_resolved_missing", 0.6);
  ("assets", "assets_missing", 0.8);
  ("plugin", "plugin_missing", 0.8);
]

let override_section ?score ~present ~missing ~total () =
  let score =
    match score with
    | Some s -> s
    | None -> if total = 0 then 1.0 else (float_of_int present /. float_of_int total)
  in
  (score, present, missing, total)

let fidelity_score_with_overrides json overrides =
  let override_for section = List.assoc_opt section overrides in
  let fold (score_sum, weight_sum, details, missing_total) (section, missing_key, weight) =
    let (score, present, missing, total) =
      match override_for section with
      | Some override -> override
      | None ->
          let (score, present, missing, total, _) =
            coverage_for_section json section missing_key weight
          in
          (score, present, missing, total)
    in
    let detail =
      `Assoc [
        ("score", `Float score);
        ("present", `Int present);
        ("missing", `Int missing);
        ("total", `Int total);
        ("weight", `Float weight);
      ]
    in
    (score_sum +. (score *. weight),
     weight_sum +. weight,
     (section, detail) :: details,
     missing_total + missing)
  in
  let (score_sum, weight_sum, details, missing_total) =
    List.fold_left fold (0.0, 0.0, [], 0) fidelity_sections
  in
  let overall = if weight_sum = 0.0 then 1.0 else score_sum /. weight_sum in
  let detail_json = `Assoc (List.rev details) in
  (overall, missing_total, detail_json)

let fidelity_score_of_dsl json =
  fidelity_score_with_overrides json []

let string_list_of_json json =
  match json with
  | `List items ->
      items
      |> List.filter_map (function `String s -> Some s | _ -> None)
  | _ -> []

let image_refs_of_dsl json =
  match member "assets" json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "image_refs" fields with
       | Some v -> string_list_of_json v
       | None -> [])
  | _ -> []

let image_fill_map image_fills =
  match image_fills with
  | `Assoc fields -> (
      match List.assoc_opt "images" fields with
      | Some (`Assoc items) -> items
      | _ -> [])
  | _ -> []

let variables_counts variables =
  let assoc_len json =
    match json with
    | `Assoc items -> List.length items
    | _ -> 0
  in
  match variables with
  | `Assoc fields when List.assoc_opt "error" fields <> None ->
      (1, 0)
  | `Assoc fields ->
      let raw_vars =
        match List.assoc_opt "variables" fields with
        | Some v -> assoc_len v
        | None -> 0
      in
      let resolved =
        match List.assoc_opt "resolved" fields with
        | Some v -> assoc_len v
        | None -> 0
      in
      (raw_vars, resolved)
  | _ -> (0, 0)

let plugin_ok plugin_snapshot =
  match plugin_snapshot with
  | `Assoc fields -> (
      match List.assoc_opt "ok" fields with
      | Some (`Bool b) -> b
      | _ -> false)
  | _ -> false

let rec count_text_segments json =
  match json with
  | `Assoc fields ->
      let self_count =
        match List.assoc_opt "text" fields with
        | Some (`Assoc text_fields) -> (
            match List.assoc_opt "segments" text_fields with
            | Some (`List segments) -> List.length segments
            | _ -> 0)
        | _ -> 0
      in
      let child_count =
        match List.assoc_opt "children" fields with
        | Some (`List children) ->
            List.fold_left (fun acc child -> acc + count_text_segments child) 0 children
        | _ -> 0
      in
      self_count + child_count
  | `List items ->
      List.fold_left (fun acc item -> acc + count_text_segments item) 0 items
  | _ -> 0

let rec count_text_nodes_dsl json =
  match json with
  | `Assoc fields ->
      let self_count =
        match List.assoc_opt "meta" fields with
        | Some (`Assoc meta_fields) -> (
            match List.assoc_opt "type" meta_fields with
            | Some (`String "TEXT") -> 1
            | _ -> 0)
        | _ -> 0
      in
      let child_count =
        match List.assoc_opt "children" fields with
        | Some (`List children) ->
            List.fold_left (fun acc child -> acc + count_text_nodes_dsl child) 0 children
        | _ -> 0
      in
      self_count + child_count
  | `List items ->
      List.fold_left (fun acc item -> acc + count_text_nodes_dsl item) 0 items
  | _ -> 0

let rec count_text_nodes_with_segments json =
  match json with
  | `Assoc fields ->
      let self_count =
        match List.assoc_opt "text" fields with
        | Some (`Assoc text_fields) -> (
            match List.assoc_opt "segments" text_fields with
            | Some (`List _) -> 1
            | _ -> 0)
        | _ -> 0
      in
      let child_count =
        match List.assoc_opt "children" fields with
        | Some (`List children) ->
            List.fold_left (fun acc child -> acc + count_text_nodes_with_segments child) 0 children
        | _ -> 0
      in
      self_count + child_count
  | `List items ->
      List.fold_left (fun acc item -> acc + count_text_nodes_with_segments item) 0 items
  | _ -> 0

let plugin_text_nodes_with_segments plugin_snapshot =
  match plugin_snapshot with
  | `Assoc fields -> (
      match List.assoc_opt "payload" fields with
      | Some payload -> count_text_nodes_with_segments payload
      | _ -> 0)
  | _ -> 0

let fidelity_score_of_bundle ~dsl_json ~variables ~image_fills ~plugin_snapshot ~include_variables ~include_image_fills ~include_plugin =
  let overrides = [] in
  let overrides =
    if include_image_fills then
      let refs = image_refs_of_dsl dsl_json in
      if refs = [] then
        overrides
      else
        let fill_map = image_fill_map image_fills in
        let present =
          List.fold_left (fun acc ref ->
            match List.assoc_opt ref fill_map with
            | Some (`String _) -> acc + 1
            | _ -> acc
          ) 0 refs
        in
        let total = List.length refs in
        let missing = max 0 (total - present) in
        ("assets", override_section ~present ~missing ~total ()) :: overrides
    else overrides
  in
  let overrides =
    if include_plugin && plugin_ok plugin_snapshot then
      let total_text_nodes = count_text_nodes_dsl dsl_json in
      if total_text_nodes = 0 then
        overrides
      else
        let present = plugin_text_nodes_with_segments plugin_snapshot in
        let present = min present total_text_nodes in
        ("text_segments", override_section ~present ~missing:(max 0 (total_text_nodes - present)) ~total:total_text_nodes ()) :: overrides
    else overrides
  in
  let overrides =
    if include_variables then
      let (total, present) = variables_counts variables in
      if total = 0 then
        overrides
      else
        ("variables_resolved", override_section ~present ~missing:(max 0 (total - present)) ~total ()) :: overrides
    else overrides
  in
  let overrides =
    if include_plugin then
      let present = if plugin_ok plugin_snapshot then 1 else 0 in
      ("plugin", override_section ~present ~missing:(1 - present) ~total:1 ()) :: overrides
    else overrides
  in
  fidelity_score_with_overrides dsl_json overrides

let default_asset_dir () =
  match Sys.getenv_opt "FIGMA_MCP_ASSET_DIR" with
  | Some dir -> dir
  | None ->
      (match Sys.getenv_opt "ME_ROOT" with
       | Some root -> root ^ "/download/figma-assets"
       | None ->
           (match Sys.getenv_opt "HOME" with
            | Some home -> home ^ "/me/download/figma-assets"
            | None -> "/tmp/figma-assets"))

let default_compare_dir () =
  default_asset_dir () ^ "/compare"

let sanitize_node_id id =
  let buf = Bytes.of_string id in
  Bytes.iteri (fun i c ->
    if c = ':' then Bytes.set buf i '_'
  ) buf;
  Bytes.to_string buf

let strip_query url =
  match String.index_opt url '?' with
  | Some i -> String.sub url 0 i
  | None -> url

let file_ext_from_url url =
  let base = strip_query url |> Filename.basename in
  let ext = Filename.extension base in
  if ext = "" then ".img" else ext

let is_http_url s =
  String.length s >= 4 && String.sub s 0 4 = "http"

let resolve_variables json =
  let member_opt key json =
    match json with
    | `Assoc lst -> List.assoc_opt key lst
    | _ -> None
  in
  let string_opt key json =
    match member_opt key json with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let meta = match member_opt "meta" json with
    | Some m -> m
    | None -> `Null
  in
  let collections = match member_opt "variableCollections" meta with
    | Some v -> v
    | None -> `Null
  in
  let variables = match member_opt "variables" meta with
    | Some v -> v
    | None -> `Null
  in
  let collection_map = assoc_or_empty collections in
  let default_mode_id_for collection_id =
    match List.assoc_opt collection_id collection_map with
    | Some col -> string_opt "defaultModeId" col
    | None -> None
  in
  let default_mode_name_for collection_id default_mode_id =
    match List.assoc_opt collection_id collection_map with
    | Some col ->
        (match list_member "modes" col with
         | Some modes ->
             let find_name = function
               | `Assoc fields -> (
                   match List.assoc_opt "modeId" fields, List.assoc_opt "name" fields with
                   | Some (`String mid), Some (`String name) when mid = default_mode_id -> Some name
                   | _ -> None)
               | _ -> None
             in
             List.find_map find_name modes
         | None -> None)
    | None -> None
  in
  let resolved =
    match variables with
    | `Assoc vars ->
        `Assoc (List.map (fun (var_id, var_json) ->
          let collection_id = string_opt "variableCollectionId" var_json in
          let default_mode_id = Option.bind collection_id default_mode_id_for in
          let default_mode_name =
            match (collection_id, default_mode_id) with
            | Some cid, Some mid -> default_mode_name_for cid mid
            | _ -> None
          in
          let values_by_mode = match member_opt "valuesByMode" var_json with
            | Some v -> v
            | None -> `Null
          in
          let default_value =
            match (default_mode_id, values_by_mode) with
            | Some mode_id, `Assoc values ->
                (match List.assoc_opt mode_id values with
                 | Some v -> v
                 | None -> `Null)
            | None, `Assoc values ->
                (match values with
                 | (_, v) :: _ -> v
                 | [] -> `Null)
            | _ -> `Null
          in
          let resolved_json =
            `Assoc [
              ("name", (match string_opt "name" var_json with Some s -> `String s | None -> `Null));
              ("resolvedType", (match string_opt "resolvedType" var_json with Some s -> `String s | None -> `Null));
              ("collectionId", (match collection_id with Some s -> `String s | None -> `Null));
              ("defaultModeId", (match default_mode_id with Some s -> `String s | None -> `Null));
              ("defaultModeName", (match default_mode_name with Some s -> `String s | None -> `Null));
              ("defaultValue", default_value);
              ("valuesByMode", values_by_mode);
            ]
          in
          (var_id, resolved_json)
        ) vars)
    | _ -> `Null
  in
  `Assoc [
    ("collections", collections);
    ("variables", variables);
    ("resolved", resolved);
  ]

let plugin_payload_if_ok plugin_result =
  match plugin_result with
  | `Assoc fields -> (
      match List.assoc_opt "ok" fields, List.assoc_opt "payload" fields with
      | Some (`Bool true), Some payload -> Some payload
      | _ -> None)
  | _ -> None

let resolve_plugin_variables payload =
  match payload with
  | `Assoc fields -> (
      match List.assoc_opt "collections" fields, List.assoc_opt "variables" fields with
      | Some collections, Some variables ->
          resolve_variables (`Assoc [
            ("meta", `Assoc [
              ("variableCollections", collections);
              ("variables", variables);
            ])
          ])
      | _ -> `Assoc [("error", `String "Missing plugin variables payload")])
  | _ -> `Assoc [("error", `String "Invalid plugin variables payload")]

let download_image_fill save_dir file_key (id, url) =
  match url with
  | `String url when is_http_url url ->
      let ext = file_ext_from_url url in
      let path = Printf.sprintf "%s/%s/%s%s"
        save_dir file_key (sanitize_node_id id) ext in
      (match Figma_effects.Perform.download_url ~url ~path with
       | Ok saved ->
           `Assoc [
             ("image_ref", `String id);
             ("url", `String url);
             ("saved", `String saved);
           ]
       | Error err ->
           `Assoc [
             ("image_ref", `String id);
             ("url", `String url);
             ("error", `String err);
           ])
  | `String url ->
      `Assoc [
        ("image_ref", `String id);
        ("url", `String url);
        ("error", `String "Download skipped: invalid URL");
      ]
  | _ ->
      `Assoc [
        ("image_ref", `String id);
        ("error", `String "Invalid image URL payload");
      ]

let build_file_meta json =
  let meta_root =
    match Yojson.Safe.Util.member "meta" json with
    | `Null -> json
    | m -> m
  in
  let pick key = Yojson.Safe.Util.member key meta_root in
  `Assoc [
    ("components", pick "components");
    ("componentSets", pick "componentSets");
    ("styles", pick "styles");
  ]

let make_text_content text : Yojson.Safe.t =
  `Assoc [
    ("content", `List [
      `Assoc [("type", `String "text"); ("text", `String text)]
    ])
  ]

let make_error_content msg : Yojson.Safe.t =
  `Assoc [
    ("content", `List [
      `Assoc [("type", `String "text"); ("text", `String msg); ("isError", `Bool true)]
    ])
  ]

let command_ok cmd =
  Sys.command (cmd ^ " >/dev/null 2>&1") = 0

let command_output cmd =
  let ic = Unix.open_process_in cmd in
  let output =
    try input_line ic with
    | End_of_file -> ""
    | _ -> ""
  in
  let _ = Unix.close_process_in ic in
  String.trim output

let has_command name =
  command_ok (Printf.sprintf "command -v %s" name)

let has_node_module name =
  command_ok (Printf.sprintf "node -e \"require('%s')\"" name)

let normalize_path path =
  try Some (Unix.realpath path) with _ -> None

let is_under_dir ~dir path =
  match (normalize_path dir, normalize_path path) with
  | (Some dir_norm, Some path_norm) ->
      let prefix = if String.ends_with ~suffix:"/" dir_norm then dir_norm else dir_norm ^ "/" in
      path_norm = dir_norm || String.starts_with ~prefix path_norm
  | _ -> false

(** figma_codegen 핸들러 *)
let handle_codegen args : (Yojson.Safe.t, string) result Lwt.t =
  let json_str = get_string "json" args in
  let format = get_string_or "format" "fidelity" args in

  match json_str with
  | None -> Lwt.return (Error "Missing required parameter: json")
  | Some json_str ->
      (match process_json_string ~format json_str with
       | Ok result -> Lwt.return (Ok (make_text_content result))
       | Error msg -> Lwt.return (Error msg))

(** figma_get_file 핸들러 *)
let handle_get_file args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let format = get_string_or "format" "fidelity" args in
  let depth = get_int "depth" args in
  let geometry = get_string "geometry" args in
  let plugin_data = get_string "plugin_data" args in
  let version = get_string "version" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file ~token ~file_key ?depth ?geometry ?plugin_data ?version () with
       | Ok json ->
           (* document 추출 *)
           let doc = Figma_api.extract_document json in
           let doc_str = match doc with
             | Some d -> Yojson.Safe.to_string d
             | None -> Yojson.Safe.to_string json
           in
           (match process_json_string ~format doc_str with
            | Ok result -> Ok (make_text_content result)
            | Error msg -> Error msg)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_file_meta 핸들러 *)
let handle_get_file_meta args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let version = get_string "version" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_meta ~token ~file_key ?version () with
       | Ok json ->
           let meta = build_file_meta json in
           Ok (make_text_content (Yojson.Safe.pretty_to_string meta))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_list_screens 핸들러 *)
let handle_list_screens args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           let screens = Figma_api.get_all_screens json in
           let screen_list = List.map (fun (id, name) ->
             sprintf "- %s (%s)" name id
           ) screens in
           let result = sprintf "Found %d screens:\n%s"
             (List.length screens)
             (String.concat "\n" screen_list)
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_node 핸들러 *)
let handle_get_node args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = get_string "token" args in
  let format = get_string_or "format" "fidelity" args in
  let depth = get_int "depth" args in
  let geometry = get_string "geometry" args in
  let plugin_data = get_string "plugin_data" args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ?depth ?geometry ?plugin_data ?version () with
       | Ok json ->
           let nodes = member "nodes" json in
           let node_data = match nodes with
             | Some (`Assoc nodes_map) ->
                 (match List.assoc_opt node_id nodes_map with
                  | Some n -> member "document" n
                  | None -> None)
             | _ -> None
           in
           (match node_data with
            | Some node ->
                let node_str = Yojson.Safe.to_string node in
                (match process_json_string ~format node_str with
                 | Ok result -> Ok (make_text_content result)
                 | Error msg -> Error msg)
            | None -> Error (sprintf "Node not found: %s" node_id))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_get_node_with_image 핸들러 *)
let handle_get_node_with_image args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = get_string "token" args in
  let format = get_string_or "format" "fidelity" args in
  let image_format = get_string_or "image_format" "png" args in
  let scale = get_float_or "scale" 1.0 args |> int_of_float in
  let use_absolute_bounds = get_bool "use_absolute_bounds" args in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in
  let depth = get_int "depth" args in
  let geometry = get_string "geometry" args in
  let plugin_data = get_string "plugin_data" args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ?depth ?geometry ?plugin_data ?version () with
       | Ok json ->
           let nodes = member "nodes" json in
           let node_data = match nodes with
             | Some (`Assoc nodes_map) ->
                 (match List.assoc_opt node_id nodes_map with
                  | Some n -> member "document" n
                  | None -> None)
             | _ -> None
           in
           (match node_data with
            | Some node ->
                let node_str = Yojson.Safe.to_string node in
                (match process_json_string ~format node_str with
                 | Ok dsl ->
                     (match Figma_effects.Perform.get_images
                              ~token ~file_key ~node_ids:[node_id]
                              ~format:image_format ~scale
                              ?use_absolute_bounds ?version () with
                      | Ok img_json ->
                          let image_url =
                            match member "images" img_json with
                            | Some (`Assoc img_map) ->
                                (match List.assoc_opt node_id img_map with
                                 | Some (`String url) -> url
                                 | _ -> "No image URL returned")
                            | _ -> "No images returned"
                          in
                          let download_info =
                            if download then
                              if is_http_url image_url then
                                let path = Printf.sprintf "%s/%s/%s.%s"
                                  save_dir file_key (sanitize_node_id node_id) image_format in
                                (match Figma_effects.Perform.download_url ~url:image_url ~path with
                                 | Ok saved -> sprintf "Saved: %s" saved
                                 | Error err -> sprintf "Download error: %s" err)
                              else
                                "Download skipped: no image URL"
                            else
                              ""
                          in
                          let result =
                            if download_info = "" then
                              sprintf "DSL:\n%s\n\nImage (%s, scale %d):\n%s"
                                dsl image_format scale image_url
                            else
                              sprintf "DSL:\n%s\n\nImage (%s, scale %d):\n%s\n%s"
                                dsl image_format scale image_url download_info
                          in
                          Ok (make_text_content result)
                      | Error err -> Error err)
                 | Error msg -> Error msg)
            | None -> Error (sprintf "Node not found: %s" node_id))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_get_node_bundle 핸들러 *)
let handle_get_node_bundle args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = get_string "token" args in
  let format = get_string_or "format" "fidelity" args in
  let image_format = get_string_or "image_format" "png" args in
  let scale = get_float_or "scale" 1.0 args in
  let use_absolute_bounds = get_bool "use_absolute_bounds" args in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in
  let include_raw = get_bool_or "include_raw" true args in
  let include_meta = get_bool_or "include_meta" true args in
  let include_variables = get_bool_or "include_variables" true args in
  let include_image_fills = get_bool_or "include_image_fills" true args in
  let auto_plugin =
    match get_bool "auto_plugin" args with
    | Some b -> b
    | None -> Option.is_some (get_string "url" args)
  in
  let include_plugin =
    match get_bool "include_plugin" args with
    | Some b -> b
    | None -> auto_plugin
  in
  let include_plugin_variables = get_bool_or "include_plugin_variables" false args in
  let include_plugin_image = get_bool_or "include_plugin_image" false args in
  let plugin_include_geometry = get_bool_or "plugin_include_geometry" false args in
  let depth = get_int "depth" args in
  let plugin_depth =
    match get_int "plugin_depth" args with
    | Some d when d >= 0 -> d
    | _ -> Option.value ~default:6 depth
  in
  let plugin_image_format = get_string_or "plugin_image_format" "png" args in
  let plugin_image_scale = get_float_or "plugin_image_scale" 1.0 args in
  let plugin_channel_id = get_string "plugin_channel_id" args in
  let plugin_timeout_ms = get_int "plugin_timeout_ms" args |> Option.value ~default:20000 in
  let geometry = get_string "geometry" args in
  let plugin_data = get_string "plugin_data" args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      (* 캐시 옵션 생성 *)
      let cache_options =
        List.filter_map Fun.id [
          Option.map (sprintf "depth:%d") depth;
          Option.map (sprintf "geometry:%s") geometry;
          Option.map (sprintf "plugin_data:%s") plugin_data;
          Option.map (sprintf "version:%s") version;
        ]
      in
      (* 캐시에서 먼저 조회 *)
      let cached_json = Figma_cache.get ~file_key ~node_id ~options:cache_options () in
      let json_result = match cached_json with
        | Some json ->
            Printf.eprintf "[Cache] HIT for node %s\n%!" node_id;
            Ok json
        | None ->
            Printf.eprintf "[Cache] MISS for node %s → fetching from API\n%!" node_id;
            match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ?depth ?geometry ?plugin_data ?version () with
            | Error err -> Error err
            | Ok json ->
                (* 성공 시 캐시에 저장 *)
                Figma_cache.set ~file_key ~node_id ~options:cache_options json;
                Ok json
      in
      (match json_result with
       | Error err -> Error err
       | Ok json ->
           let node_data = match member "nodes" json with
             | Some (`Assoc nodes_map) ->
                 (match List.assoc_opt node_id nodes_map with
                  | Some n -> member "document" n
                  | None -> None)
             | _ -> None
           in
           (match node_data with
            | None -> Error (sprintf "Node not found: %s" node_id)
            | Some node ->
                let node_str = Yojson.Safe.to_string node in
                let dsl_str = match process_json_string ~format node_str with
                  | Ok s -> s
                  | Error msg -> msg
                in
                let dsl_json =
                  try Yojson.Safe.from_string dsl_str
                  with _ -> `Null
                in
                let (image_url, image_download) =
                  match Figma_effects.Perform.get_images
                          ~token ~file_key ~node_ids:[node_id]
                          ~format:image_format ~scale:(int_of_float scale)
                          ?use_absolute_bounds ?version () with
                  | Ok img_json ->
                      let url =
                        match member "images" img_json with
                        | Some (`Assoc img_map) ->
                            (match List.assoc_opt node_id img_map with
                             | Some (`String u) -> u
                             | _ -> "No image URL returned")
                        | _ -> "No images returned"
                      in
                      if download then
                        if is_http_url url then
                          let path = Printf.sprintf "%s/%s/%s.%s"
                            save_dir file_key (sanitize_node_id node_id) image_format in
                          (match Figma_effects.Perform.download_url ~url ~path with
                           | Ok saved -> (url, `String saved)
                           | Error err -> (url, `String ("Download error: " ^ err)))
                        else
                          (url, `String "Download skipped: no image URL")
                      else
                        (url, `Null)
                  | Error err -> ("Image error: " ^ err, `Null)
                in
                let file_meta =
                  if include_meta then
                    match Figma_effects.Perform.get_file_meta ~token ~file_key ?version () with
                    | Ok meta_json -> build_file_meta meta_json
                    | Error err -> `Assoc [("error", `String err)]
                  else
                    `Null
                in
                let resolve_plugin_channel () =
                  match plugin_channel_id with
                  | Some id -> Ok id
                  | None -> resolve_channel_id args
                in
                let want_plugin_variables =
                  include_plugin_variables || (include_plugin && include_variables)
                in
                let plugin_variables =
                  if want_plugin_variables then
                    match resolve_plugin_channel () with
                    | Error msg -> `Assoc [("error", `String msg)]
                    | Ok channel_id ->
                        let payload = `Assoc [] in
                        let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_variables" ~payload in
                        (match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                         | Error err -> `Assoc [("error", `String err)]
                         | Ok result ->
                             `Assoc [
                               ("channel_id", `String channel_id);
                               ("command_id", `String command_id);
                               ("ok", `Bool result.ok);
                               ("payload", result.payload);
                             ])
                  else
                    `Null
                in
                let (variables, variables_source) =
                  if include_variables then
                    match fetch_variables_cached ~file_key ~token with
                    | Ok (vars_json, source) -> (resolve_variables vars_json, source)
                    | Error err ->
                        (match plugin_payload_if_ok plugin_variables with
                         | Some payload -> (resolve_plugin_variables payload, `String "plugin")
                         | None -> (`Assoc [("error", `String err)], `String "error"))
                  else
                    (`Null, `Null)
                in
                let image_fills =
                  if include_image_fills then
                    match Figma_effects.Perform.get_file_images ~token ~file_key ?version () with
                    | Ok img_json ->
                        let images =
                          match member "images" img_json with
                          | Some (`Assoc _ as m) -> m
                          | _ -> `Null
                        in
                        let downloads =
                          if download then
                            match images with
                            | `Assoc items ->
                                `List (List.map (download_image_fill save_dir file_key) items)
                            | _ -> `List []
                          else
                            `List []
                        in
                        `Assoc [("images", images); ("downloads", downloads)]
                    | Error err -> `Assoc [("error", `String err)]
                  else
                    `Null
                in
                let plugin_snapshot =
                  if include_plugin then
                    (match resolve_plugin_channel () with
                     | Error msg -> `Assoc [("error", `String msg)]
                     | Ok channel_id ->
                         let run_snapshot depth_used =
                           let payload = `Assoc [
                             ("node_id", `String node_id);
                             ("depth", `Int depth_used);
                             ("include_geometry", `Bool plugin_include_geometry);
                           ] in
                           let command_id =
                             Figma_plugin_bridge.enqueue_command
                               ~channel_id
                               ~name:"get_node"
                               ~payload
                           in
                           match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                           | Error err -> Error err
                           | Ok result ->
                               Ok (`Assoc [
                                 ("channel_id", `String channel_id);
                                 ("command_id", `String command_id);
                                 ("ok", `Bool result.ok);
                                 ("payload", result.payload);
                                 ("plugin_depth", `Int depth_used);
                               ])
                         in
                         match run_snapshot plugin_depth with
                         | Ok snapshot -> snapshot
                         | Error err ->
                             if plugin_depth > 0 then
                               (match run_snapshot 0 with
                                | Ok snapshot ->
                                    (match snapshot with
                                     | `Assoc fields ->
                                         `Assoc (("note", `String "plugin snapshot fallback to depth=0")
                                                 :: ("fallback_error", `String err)
                                                 :: fields)
                                     | _ -> snapshot)
                                | Error err2 -> `Assoc [("error", `String err2)])
                             else
                               `Assoc [("error", `String err)])
                  else
                    `Null
                in
                let plugin_image =
                  if include_plugin_image then
                    (match resolve_plugin_channel () with
                     | Error msg -> `Assoc [("error", `String msg)]
                     | Ok channel_id ->
                         let payload = `Assoc [
                           ("node_id", `String node_id);
                           ("format", `String plugin_image_format);
                           ("scale", `Float plugin_image_scale);
                         ] in
                         let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"export_node_image" ~payload in
                         (match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                          | Error err -> `Assoc [("error", `String err)]
                          | Ok result ->
                              let response =
                                `Assoc [
                                  ("channel_id", `String channel_id);
                                  ("command_id", `String command_id);
                                  ("ok", `Bool result.ok);
                                  ("payload", result.payload);
                                ]
                              in
                              if download then
                                `Assoc [
                                  ("note", `String "download=true is ignored for plugin_image (base64 only)");
                                  ("response", response);
                                ]
                              else
                                response))
                  else
                    `Null
                in
                let fidelity =
                  match dsl_json with
                  | `Assoc _ as json ->
                      let (overall, missing_total, sections) =
                        fidelity_score_of_bundle
                          ~dsl_json:json
                          ~variables
                          ~image_fills
                          ~plugin_snapshot
                          ~include_variables
                          ~include_image_fills
                          ~include_plugin
                      in
                      `Assoc [
                        ("overall", `Float overall);
                        ("missing_total", `Int missing_total);
                        ("sections", sections);
                      ]
                  | _ -> `Null
                in
                let result =
                  `Assoc [
                    ("file_key", `String file_key);
                    ("node_id", `String node_id);
                    ("dsl", `String dsl_str);
                    ("dsl_json", dsl_json);
                    ("node_raw", if include_raw then node else `Null);
                    ("image", `Assoc [
                      ("url", `String image_url);
                      ("download", image_download);
                    ]);
                    ("file_meta", file_meta);
                    ("variables", variables);
                    ("variables_source", variables_source);
                    ("plugin_variables", plugin_variables);
                    ("image_fills", image_fills);
                    ("plugin_snapshot", plugin_snapshot);
                    ("plugin_image", plugin_image);
                    ("fidelity", fidelity);
                  ]
                in
                (* Large Response Handler: 500KB 초과 시 파일로 저장 *)
                let result_str = Yojson.Safe.pretty_to_string result in
                let prefix = Printf.sprintf "node_%s" (sanitize_node_id node_id) in
                Ok (Large_response.wrap_string_result ~prefix ~format result_str)))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_get_node_summary 핸들러 - 경량 구조 요약 *)
let handle_get_node_summary args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = get_string "token" args in
  let max_children = match get_int "max_children" args with Some n when n > 0 -> n | _ -> 50 in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let node_id = Figma_api.normalize_node_id node_id in
      (* 최소 depth=1로 자식만 가져옴 *)
      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ~depth:1 ?version () with
       | Error err -> Error (Printf.sprintf "Figma API error: %s" err)
       | Ok nodes_json ->
           let open Yojson.Safe.Util in
           let nodes = nodes_json |> member "nodes" in
           let node_entry = nodes |> member node_id in
           (match node_entry with
            | `Null -> Error (Printf.sprintf "Node %s not found in file %s" node_id file_key)
            | _ ->
                let node_data = node_entry |> member "document" in
                (match node_data with
                 | `Null -> Error (Printf.sprintf "Document not found for node %s" node_id)
                 | _ ->

           (* 자식 요약 추출 *)
           let children = node_data |> member "children" |> to_list in
           let children_count = List.length children in
           let children_summary =
             children
             |> List.mapi (fun i child ->
                 if i >= max_children then None
                 else
                   let id = child |> member "id" |> to_string_option |> Option.value ~default:"" in
                   let name = child |> member "name" |> to_string_option |> Option.value ~default:"" in
                   let typ = child |> member "type" |> to_string_option |> Option.value ~default:"UNKNOWN" in
                   let sub_children = child |> member "children" |> to_list |> List.length in
                   Some (`Assoc [
                     ("id", `String id);
                     ("name", `String name);
                     ("type", `String typ);
                     ("children_count", `Int sub_children);
                   ]))
             |> List.filter_map Fun.id
           in

           let node_name = node_data |> member "name" |> to_string_option |> Option.value ~default:"" in
           let node_type = node_data |> member "type" |> to_string_option |> Option.value ~default:"UNKNOWN" in

           Ok (`Assoc [
             ("node_id", `String node_id);
             ("name", `String node_name);
             ("type", `String node_type);
             ("children_count", `Int children_count);
             ("children", `List children_summary);
             ("truncated", `Bool (children_count > max_children));
             ("hint", `String "Use figma_get_node_chunk for progressive loading of specific depth ranges");
           ]))))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_select_nodes 핸들러 - 점수 기반 후보 선별 *)
let handle_select_nodes args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = get_string "token" args in
  let summary_depth = match get_int "summary_depth" args with Some d when d >= 0 -> d | _ -> 1 in
  let preview = get_bool_or "preview" true args in
  let preview_format = get_string_or "preview_format" "png" args in
  let preview_scale = match get_int "preview_scale" args with Some s when s > 0 -> s | _ -> 1 in
  let layout_only = get_bool_or "layout_only" false args in
  let auto_layout_only = get_bool_or "auto_layout_only" false args in
  let raw_text_mode = get_string_or "text_mode" "include" args in
  let text_mode =
    match raw_text_mode with
    | "include" | "exclude" | "only" -> raw_text_mode
    | _ -> "include"
  in
  let score_threshold = get_float_or "score_threshold" 2.0 args in
  let max_parents = match get_int "max_parents" args with Some n when n > 0 -> n | _ -> 8 in
  let notes_limit = match get_int "notes_limit" args with Some n when n > 0 -> n | _ -> 50 in
  let excluded_limit = match get_int "excluded_limit" args with Some n when n > 0 -> n | _ -> 50 in
  let version = get_string "version" args in
  let exclude_patterns =
    get_string_list "exclude_patterns" args
    |> Option.value ~default:default_exclude_patterns
    |> normalize_patterns
  in
  let note_patterns =
    get_string_list "note_patterns" args
    |> Option.value ~default:default_note_patterns
    |> normalize_patterns
  in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let node_id = Figma_api.normalize_node_id node_id in
      let config = {
        layout_only;
        auto_layout_only;
        text_mode;
        score_threshold;
        max_parents;
        summary_depth;
        exclude_patterns;
        note_patterns;
        notes_limit;
        excluded_limit;
      } in

      let warnings = ref [] in
      if raw_text_mode <> text_mode then
        warnings := "Invalid text_mode, fallback to include" :: !warnings;

      let preview_json =
        if not preview then `Null
        else
          match Figma_effects.Perform.get_images
                  ~token ~file_key ~node_ids:[node_id]
                  ~format:preview_format ~scale:preview_scale ?version () with
          | Error err ->
              `Assoc [
                ("status", `String "error");
                ("error", `String err);
              ]
          | Ok json ->
              let open Yojson.Safe.Util in
              let images = json |> member "images" in
              let url =
                match images with
                | `Assoc map ->
                    (match List.assoc_opt node_id map with
                     | Some (`String url) -> Some url
                     | _ -> None)
                | _ -> None
              in
              (match url with
               | Some url ->
                   `Assoc [
                     ("status", `String "ok");
                     ("url", `String url);
                     ("format", `String preview_format);
                     ("scale", `Int preview_scale);
                   ]
               | None ->
                   `Assoc [
                     ("status", `String "missing");
                     ("format", `String preview_format);
                     ("scale", `Int preview_scale);
                   ])
      in

      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ~depth:summary_depth ?version () with
       | Error err -> Error (Printf.sprintf "Figma API error: %s" err)
       | Ok nodes_json ->
           let open Yojson.Safe.Util in
           let nodes = nodes_json |> member "nodes" in
           let node_entry = nodes |> member node_id in
           (match node_entry with
            | `Null -> Error (Printf.sprintf "Node %s not found in file %s" node_id file_key)
            | _ ->
                let node_data = node_entry |> member "document" in
                (match node_data with
                 | `Null -> Error (Printf.sprintf "Document not found for node %s" node_id)
                 | _ ->
                     (match Figma_parser.parse_node ~max_depth:summary_depth node_data with
                      | None -> Error "Failed to parse node JSON"
                      | Some root ->
                          let all_nodes = Figma_query.collect_nodes ~max_depth:(Some summary_depth) root in
                          let notes =
                            all_nodes
                            |> List.filter node_is_text
                            |> List.filter (fun node ->
                                matches_any config.note_patterns (node_text_blob node))
                            |> (fun nodes ->
                                let rec take acc count = function
                                  | [] -> List.rev acc
                                  | _ when count >= config.notes_limit -> List.rev acc
                                  | n :: rest -> take (n :: acc) (count + 1) rest
                                in
                                take [] 0 nodes)
                            |> List.map (fun node ->
                                let text = Option.value ~default:"" node.Figma_types.characters in
                                let pattern =
                                  find_matching_pattern config.note_patterns (node_text_blob node)
                                  |> Option.value ~default:""
                                in
                                `Assoc [
                                  ("id", `String node.Figma_types.id);
                                  ("name", `String node.Figma_types.name);
                                  ("type", `String (Figma_query.node_type_to_string node.Figma_types.node_type));
                                  ("text", `String text);
                                  ("pattern", `String pattern);
                                ])
                          in

                          let candidates =
                            if root.Figma_types.children = [] then [root] else root.Figma_types.children
                          in
                          let duplicates = Hashtbl.create 32 in
                          let next_duplicate_index node =
                            let key = node_duplicate_key node in
                            let count = Option.value ~default:0 (Hashtbl.find_opt duplicates key) in
                            Hashtbl.replace duplicates key (count + 1);
                            count
                          in

                          let scored = ref [] in
                          let excluded = ref [] in
                          List.iter (fun node ->
                              let name_blob = node_text_blob node in
                              let duplicate_index = next_duplicate_index node in
                              let exclusion_reason =
                                if (not node.Figma_types.visible) || node.Figma_types.opacity <= 0.01 then
                                  Some "invisible"
                                else if config.auto_layout_only && not (node_has_auto_layout node) then
                                  Some "auto_layout_only"
                                else if config.layout_only && not (node_is_container node) then
                                  Some "layout_only"
                                else if config.text_mode = "exclude" && node_is_text node then
                                  Some "text_mode_exclude"
                                else if config.text_mode = "only" && not (node_is_text node) then
                                  Some "text_mode_only"
                                else if matches_any config.exclude_patterns name_blob then
                                  Some "excluded_pattern"
                                else if node_is_text node && matches_any config.note_patterns name_blob then
                                  Some "note_text"
                                else
                                  None
                              in
                              match exclusion_reason with
                              | Some reason ->
                                  let pattern =
                                    if reason = "excluded_pattern" then
                                      find_matching_pattern config.exclude_patterns name_blob
                                    else if reason = "note_text" then
                                      find_matching_pattern config.note_patterns name_blob
                                    else
                                      None
                                  in
                                  let reason =
                                    match pattern with
                                    | Some p -> Printf.sprintf "%s:%s" reason p
                                    | None -> reason
                                  in
                                  excluded := (node, reason) :: !excluded
                              | None ->
                                  let score = ref 0.0 in
                                  let reasons = ref [] in
                                  let add amount label =
                                    if amount <> 0.0 then begin
                                      score := !score +. amount;
                                      reasons := label :: !reasons
                                    end
                                  in
                                  if node_is_text node then add 2.0 "text:+2";
                                  if node_has_image_fill node then add 2.0 "image_fill:+2";
                                  if node_has_auto_layout node then add 1.5 "auto_layout:+1.5";
                                  if node_is_component node then add 1.0 "component:+1";
                                  if node_has_mask_hint node then add 1.0 "mask_or_clip:+1";
                                  let area = node_area node in
                                  let area_score = node_area_score area in
                                  if area_score > 0.0 then
                                    add area_score (Printf.sprintf "area:+%.2f" area_score);
                                  let small_penalty =
                                    if area < 64.0 then 2.0
                                    else if area < 256.0 then 1.0
                                    else 0.0
                                  in
                                  if small_penalty > 0.0 then
                                    add (-. small_penalty)
                                      (Printf.sprintf "small_area:-%.1f" small_penalty);
                                  if duplicate_index > 0 then
                                    add (-. 1.0) "duplicate:-1";
                                  if matches_any ["as-is"; "as is"; "asis"] name_blob then
                                    add (-. 0.5) "as_is:-0.5";
                                  scored := (node, !score, List.rev !reasons, area) :: !scored
                            ) candidates;

                          let scored_sorted =
                            List.sort (fun (_, a, _, _) (_, b, _, _) -> Float.compare b a) !scored
                          in
                          let scored_selected =
                            scored_sorted
                            |> List.filter (fun (_, score, _, _) -> score >= config.score_threshold)
                            |> (fun nodes ->
                                let rec take acc count = function
                                  | [] -> List.rev acc
                                  | _ when count >= config.max_parents -> List.rev acc
                                  | n :: rest -> take (n :: acc) (count + 1) rest
                                in
                                take [] 0 nodes)
                          in
                          let selected, selection_mode =
                            if scored_selected = [] && scored_sorted <> [] then
                              let rec take acc count = function
                                | [] -> List.rev acc
                                | _ when count >= config.max_parents -> List.rev acc
                                | n :: rest -> take (n :: acc) (count + 1) rest
                              in
                              (take [] 0 scored_sorted, "fallback_top_scores")
                            else
                              (scored_selected, "threshold")
                          in

                          let selected_json =
                            selected
                            |> List.map (fun (node, score, reasons, area) ->
                                let (width, height) =
                                  match node.Figma_types.bbox with
                                  | Some b -> (b.width, b.height)
                                  | None -> (0., 0.)
                                in
                                `Assoc [
                                  ("id", `String node.Figma_types.id);
                                  ("name", `String node.Figma_types.name);
                                  ("type", `String (Figma_query.node_type_to_string node.Figma_types.node_type));
                                  ("score", `Float score);
                                  ("area", `Float area);
                                  ("width", `Float width);
                                  ("height", `Float height);
                                  ("reasons", `List (List.map (fun r -> `String r) reasons));
                                ])
                          in

                          let excluded_json =
                            !excluded
                            |> (fun nodes ->
                                let rec take acc count = function
                                  | [] -> List.rev acc
                                  | _ when count >= config.excluded_limit -> List.rev acc
                                  | n :: rest -> take (n :: acc) (count + 1) rest
                                in
                                take [] 0 nodes)
                            |> List.map (fun (node, reason) ->
                                `Assoc [
                                  ("id", `String node.Figma_types.id);
                                  ("name", `String node.Figma_types.name);
                                  ("type", `String (Figma_query.node_type_to_string node.Figma_types.node_type));
                                  ("reason", `String reason);
                                ])
                          in

                          let root_summary =
                            `Assoc [
                              ("id", `String root.Figma_types.id);
                              ("name", `String root.Figma_types.name);
                              ("type", `String (Figma_query.node_type_to_string root.Figma_types.node_type));
                              ("children_count", `Int (List.length root.Figma_types.children));
                            ]
                          in

                          let result =
                            `Assoc [
                              ("file_key", `String file_key);
                              ("node_id", `String node_id);
                              ("summary_depth", `Int summary_depth);
                              ("preview", preview_json);
                              ("root", root_summary);
                              ("selection_mode", `String selection_mode);
                              ("score_threshold", `Float config.score_threshold);
                              ("max_parents", `Int config.max_parents);
                              ("layout_only", `Bool config.layout_only);
                              ("auto_layout_only", `Bool config.auto_layout_only);
                              ("text_mode", `String config.text_mode);
                              ("selected", `List selected_json);
                              ("selected_count", `Int (List.length selected_json));
                              ("excluded", `List excluded_json);
                              ("excluded_count", `Int (List.length !excluded));
                              ("notes", `List notes);
                              ("notes_count", `Int (List.length notes));
                              ("warnings", `List (List.map (fun w -> `String w) (List.rev !warnings)));
                            ]
                          in
                          Ok (make_text_content (Yojson.Safe.pretty_to_string result))
                     )))
           )
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_get_node_chunk 핸들러 - 깊이 범위별 청크 로드 *)
let handle_get_node_chunk args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = get_string "token" args in
  let depth_start = match get_int "depth_start" args with Some d when d >= 0 -> d | _ -> 0 in
  let depth_end = match get_int "depth_end" args with Some d when d >= 0 -> d | _ -> 2 in
  let format = get_string_or "format" "fidelity" args in
  let include_styles = get_bool_or "include_styles" false args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let node_id = Figma_api.normalize_node_id node_id in
      if depth_end < depth_start then
        Error "depth_end must be >= depth_start"
      else
        (* depth_end까지만 가져옴 *)
        let api_depth = depth_end + 1 in
        (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ~depth:api_depth ?version () with
         | Error err -> Error (Printf.sprintf "Figma API error: %s" err)
         | Ok nodes_json ->
             let open Yojson.Safe.Util in
             let nodes = nodes_json |> member "nodes" in
             let node_entry = nodes |> member node_id in
             (match node_entry with
              | `Null -> Error (Printf.sprintf "Node %s not found in file %s" node_id file_key)
              | _ ->
                  let node_data = node_entry |> member "document" in
                  (match node_data with
                   | `Null -> Error (Printf.sprintf "Document not found for node %s" node_id)
                   | _ ->

             (* 깊이 범위에 따라 필터링하는 재귀 함수 *)
             let rec filter_by_depth current_depth json =
               if current_depth < depth_start then
                 (* 시작 깊이 미만: 자식만 재귀 처리 *)
                 let children = json |> member "children" |> to_list in
                 let filtered_children = List.filter_map (fun c ->
                     let result = filter_by_depth (current_depth + 1) c in
                     if result = `Null then None else Some result
                   ) children
                 in
                 if filtered_children = [] then `Null
                 else
                   let assoc = to_assoc json in
                   let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                   `Assoc (without_children @ [("children", `List filtered_children)])
               else if current_depth > depth_end then
                 (* 종료 깊이 초과: 자식 제거 *)
                 let assoc = to_assoc json in
                 let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                 let children = json |> member "children" |> to_list |> List.length in
                 `Assoc (without_children @ [("_truncated_children", `Int children)])
               else
                 (* 범위 내: 자식 재귀 처리 *)
                 let children = json |> member "children" |> to_list in
                 let filtered_children = List.map (fun c -> filter_by_depth (current_depth + 1) c) children in
                 let assoc = to_assoc json in
                 let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                 `Assoc (without_children @ [("children", `List filtered_children)])
             in

             let filtered = filter_by_depth 0 node_data in

             let result =
               let _ = include_styles in (* TODO: 스타일 추출 기능 추가 *)
               let filtered_str = Yojson.Safe.to_string filtered in
               match process_json_string ~format filtered_str with
               | Ok dsl ->
                   `Assoc [
                     ("type", `String "text");
                     ("text", `String dsl);
                     ("depth_range", `String (Printf.sprintf "%d-%d" depth_start depth_end));
                     ("format", `String format);
                   ]
               | Error msg ->
                   `Assoc [
                     ("error", `String msg);
                     ("node", filtered);
                     ("depth_range", `String (Printf.sprintf "%d-%d" depth_start depth_end));
                   ]
             in

             (* Large Response Handler 적용 *)
             let result_str = Yojson.Safe.pretty_to_string result in
             let prefix = Printf.sprintf "chunk_%s_%d_%d" (sanitize_node_id node_id) depth_start depth_end in
             Ok (Large_response.wrap_string_result ~prefix ~format result_str))))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_fidelity_loop 핸들러 *)
let handle_fidelity_loop args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = get_string "token" args in
  let format = get_string_or "format" "fidelity" args in
  let target_score = get_float_or "target_score" 0.92 args in
  let start_depth = match get_int "start_depth" args with Some d when d > 0 -> d | _ -> 4 in
  let depth_step = match get_int "depth_step" args with Some d when d > 0 -> d | _ -> 4 in
  let max_depth = match get_int "max_depth" args with Some d when d > 0 -> d | _ -> 20 in
  let max_attempts = match get_int "max_attempts" args with Some d when d > 0 -> d | _ -> 4 in
  let geometry = match get_string "geometry" args with Some g -> Some g | None -> Some "paths" in
  let plugin_data = get_string "plugin_data" args in
  let include_meta = get_bool_or "include_meta" true args in
  let include_variables = get_bool_or "include_variables" true args in
  let include_image_fills = get_bool_or "include_image_fills" true args in
  let auto_plugin =
    match get_bool "auto_plugin" args with
    | Some b -> b
    | None -> Option.is_some (get_string "url" args)
  in
  let include_plugin =
    match get_bool "include_plugin" args with
    | Some b -> b
    | None -> auto_plugin
  in
  let include_plugin_variables = get_bool_or "include_plugin_variables" false args in
  let plugin_channel_id = get_string "plugin_channel_id" args in
  let plugin_depth = match get_int "plugin_depth" args with Some d when d > 0 -> d | _ -> 6 in
  let plugin_timeout_ms = get_int "plugin_timeout_ms" args |> Option.value ~default:20000 in

  let clamp_score v =
    if v < 0.0 then 0.0 else if v > 1.0 then 1.0 else v
  in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      if format <> "fidelity" then
        Error "figma_fidelity_loop only supports format=fidelity"
      else
        let target_score = clamp_score target_score in
        let file_meta =
          if include_meta then
            match Figma_effects.Perform.get_file_meta ~token ~file_key () with
            | Ok meta_json -> build_file_meta meta_json
            | Error err -> `Assoc [("error", `String err)]
          else
            `Null
        in
        let resolve_plugin_channel () =
          match plugin_channel_id with
          | Some id -> Ok id
          | None -> resolve_channel_id args
        in
        let want_plugin_variables =
          include_plugin_variables || (include_plugin && include_variables)
        in
        let plugin_variables =
          if want_plugin_variables then
            match resolve_plugin_channel () with
            | Error msg -> `Assoc [("error", `String msg)]
            | Ok channel_id ->
                let payload = `Assoc [] in
                let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_variables" ~payload in
                (match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                 | Error err -> `Assoc [("error", `String err)]
                 | Ok result ->
                     `Assoc [
                       ("channel_id", `String channel_id);
                       ("command_id", `String command_id);
                       ("ok", `Bool result.ok);
                       ("payload", result.payload);
                     ])
          else
            `Null
        in
        let (variables, variables_source) =
          if include_variables then
            match fetch_variables_cached ~file_key ~token with
            | Ok (vars_json, source) -> (resolve_variables vars_json, source)
            | Error err ->
                (match plugin_payload_if_ok plugin_variables with
                 | Some payload -> (resolve_plugin_variables payload, `String "plugin")
                 | None -> (`Assoc [("error", `String err)], `String "error"))
          else
            (`Null, `Null)
        in
        let image_fills =
          if include_image_fills then
            match Figma_effects.Perform.get_file_images ~token ~file_key () with
            | Ok img_json ->
                let images =
                  match member "images" img_json with
                  | Some (`Assoc _ as m) -> m
                  | _ -> `Null
                in
                `Assoc [("images", images)]
            | Error err -> `Assoc [("error", `String err)]
          else
            `Null
        in
        let plugin_snapshot =
          if include_plugin then
            match resolve_plugin_channel () with
            | Error msg -> `Assoc [("error", `String msg)]
            | Ok channel_id ->
                let payload = `Assoc [
                  ("node_id", `String node_id);
                  ("depth", `Int plugin_depth);
                ] in
                let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_node" ~payload in
                (match plugin_wait ~channel_id ~command_id ~timeout_ms:plugin_timeout_ms with
                 | Error err -> `Assoc [("error", `String err)]
                 | Ok result ->
                     `Assoc [
                       ("channel_id", `String channel_id);
                       ("command_id", `String command_id);
                       ("ok", `Bool result.ok);
                       ("payload", result.payload);
                     ])
          else
            `Null
        in
        (* Early Stop 감지기 생성 *)
        let early_stop_config = Figma_early_stop.{
          target_ssim = target_score;
          plateau_threshold = 0.005;  (* 0.5% *)
          plateau_patience = 3;
          text_ceiling = 0.88;
          max_iterations = max_attempts;
        } in
        let early_stop_detector = Figma_early_stop.create ~config:early_stop_config () in

        let rec loop attempt depth best attempts =
          if attempt > max_attempts then
            (best, attempts, None)
          else
            (* 캐시 옵션: depth와 geometry 포함 *)
            let cache_options = List.filter_map Fun.id [
              Some (sprintf "depth:%d" depth);
              Option.map (sprintf "geometry:%s") geometry;
              Option.map (sprintf "plugin_data:%s") plugin_data;
            ] in
            let cached = Figma_cache.get ~file_key ~node_id ~options:cache_options () in
            let json_result = match cached with
              | Some json ->
                  Printf.eprintf "[FidelityLoop] Cache HIT: depth=%d\n%!" depth;
                  Ok json
              | None ->
                  Printf.eprintf "[FidelityLoop] Cache MISS: depth=%d → API call\n%!" depth;
                  match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id]
                          ?geometry ?plugin_data ~depth () with
                  | Error err -> Error err
                  | Ok json ->
                      Figma_cache.set ~file_key ~node_id ~options:cache_options json;
                      Ok json
            in
            (match json_result with
            | Error err -> (best, (`Assoc [("attempt", `Int attempt); ("error", `String err)]) :: attempts, None)
            | Ok json ->
                let node_data = match member "nodes" json with
                  | Some (`Assoc nodes_map) ->
                      (match List.assoc_opt node_id nodes_map with
                       | Some n -> member "document" n
                       | None -> None)
                  | _ -> None
                in
                (match node_data with
                 | None ->
                     let entry = `Assoc [
                       ("attempt", `Int attempt);
                       ("depth", `Int depth);
                       ("error", `String ("Node not found: " ^ node_id));
                     ] in
                     (best, entry :: attempts, None)
                 | Some node ->
                     let node_str = Yojson.Safe.to_string node in
                     let dsl_str =
                       match process_json_string ~format node_str with
                       | Ok s -> s
                       | Error msg -> msg
                     in
                     let dsl_json =
                       try Yojson.Safe.from_string dsl_str
                       with _ -> `Null
                     in
                     let (overall, missing_total, sections) =
                       match dsl_json with
                       | `Assoc _ as json ->
                           fidelity_score_of_bundle
                             ~dsl_json:json
                             ~variables
                             ~image_fills
                             ~plugin_snapshot
                             ~include_variables
                             ~include_image_fills
                             ~include_plugin
                       | _ -> (0.0, 0, `Null)
                     in
                     let fidelity = `Assoc [
                       ("overall", `Float overall);
                       ("missing_total", `Int missing_total);
                       ("sections", sections);
                     ] in
                     let best =
                       match best with
                       | None ->
                           let payload = `Assoc [
                             ("depth", `Int depth);
                             ("dsl", `String dsl_str);
                             ("dsl_json", dsl_json);
                             ("node_raw", node);
                             ("fidelity", fidelity);
                           ] in
                           Some (overall, payload)
                       | Some (best_score, _) when overall > best_score ->
                           let payload = `Assoc [
                             ("depth", `Int depth);
                             ("dsl", `String dsl_str);
                             ("dsl_json", dsl_json);
                             ("node_raw", node);
                             ("fidelity", fidelity);
                           ] in
                           Some (overall, payload)
                       | Some _ -> best
                     in
                     (* Early Stop 체크 *)
                     let text_density = Figma_early_stop.calculate_text_density dsl_json in
                     let stop_condition = Figma_early_stop.check early_stop_detector
                       ~current_ssim:overall ~iteration:attempt ~text_density () in
                     let entry_with_stop = `Assoc [
                       ("attempt", `Int attempt);
                       ("depth", `Int depth);
                       ("geometry", match geometry with Some g -> `String g | None -> `Null);
                       ("fidelity", fidelity);
                       ("early_stop", `Assoc [
                         ("should_stop", `Bool stop_condition.should_stop);
                         ("reason", `String stop_condition.message);
                         ("text_density", `Float text_density);
                       ]);
                     ] in
                     if stop_condition.should_stop || depth >= max_depth then
                       (best, entry_with_stop :: attempts, Some stop_condition)
                     else
                       let next_depth = min max_depth (depth + depth_step) in
                       if next_depth = depth then
                         (best, entry_with_stop :: attempts, Some stop_condition)
                       else
                         loop (attempt + 1) next_depth best (entry_with_stop :: attempts)))
        in
        let (best, attempts, final_stop) = loop 1 start_depth None [] in
        let (best_score, best_payload) =
          match best with
          | Some (score, payload) -> (score, payload)
          | None -> (0.0, `Null)
        in
        let early_stop_summary =
          match final_stop with
          | Some cond -> Figma_early_stop.to_json early_stop_detector cond
          | None -> `Assoc [("summary", `String (Figma_early_stop.summary early_stop_detector))]
        in
        let result = `Assoc [
          ("target_score", `Float target_score);
          ("early_stop", early_stop_summary);
          ("best_score", `Float best_score);
          ("achieved", `Bool (best_score >= target_score));
          ("best", best_payload);
          ("attempts", `List (List.rev attempts));
          ("file_meta", file_meta);
          ("variables", variables);
          ("variables_source", variables_source);
          ("plugin_variables", plugin_variables);
          ("image_fills", image_fills);
          ("plugin_snapshot", plugin_snapshot);
        ] in
        Ok (make_text_content (Yojson.Safe.pretty_to_string result))
  | _ -> Error "Missing required parameters: file_key, node_id, token"

(** figma_image_similarity 핸들러 *)
let handle_image_similarity args : (Yojson.Safe.t, string) result =
  let format = get_string_or "format" "png" args in
  let start_scale = match get_int "start_scale" args with Some s when s > 0 -> s | _ -> 1 in
  let max_scale = match get_int "max_scale" args with Some s when s > 0 -> s | _ -> start_scale in
  let scale_step = match get_int "scale_step" args with Some s when s > 0 -> s | _ -> 1 in
  let target_ssim = get_float "target_ssim" args in
  let use_absolute_bounds = get_bool "use_absolute_bounds" args in
  let version = get_string "version" args in
  let save_dir = get_string_or "save_dir" (default_compare_dir ()) args in

  let clamp_scale s = max 1 (min 4 s) in

  match (get_string "file_key" args, get_string "node_a_id" args, get_string "node_b_id" args, get_string "token" args) with
  | (Some file_key, Some node_a_id, Some node_b_id, Some token) ->
      let compare_scale scale =
        match Figma_effects.Perform.get_images ~token ~file_key
                ~node_ids:[node_a_id; node_b_id]
                ~format ~scale ?use_absolute_bounds ?version () with
        | Error err -> Error err
        | Ok json ->
            let images = match member "images" json with
              | Some (`Assoc map) -> map
              | _ -> []
            in
            let url_for id =
              match List.assoc_opt id images with
              | Some (`String url) -> Ok url
              | _ -> Error (Printf.sprintf "Image URL not found: %s" id)
            in
            (match (url_for node_a_id, url_for node_b_id) with
             | (Ok url_a, Ok url_b) ->
                 let path_a = Printf.sprintf "%s/%s/%s__%d.%s"
                   save_dir file_key (sanitize_node_id node_a_id) scale format in
                 let path_b = Printf.sprintf "%s/%s/%s__%d.%s"
                   save_dir file_key (sanitize_node_id node_b_id) scale format in
                 (match Figma_effects.Perform.download_url ~url:url_a ~path:path_a with
                  | Error err -> Error err
                  | Ok saved_a ->
                      (match Figma_effects.Perform.download_url ~url:url_b ~path:path_b with
                       | Error err -> Error err
                       | Ok saved_b ->
                           (match Figma_image_similarity.compare_paths ~path_a:saved_a ~path_b:saved_b with
                            | Error err -> Error err
                            | Ok metrics ->
                                let result = `Assoc [
                                  ("scale", `Int scale);
                                  ("format", `String format);
                                  ("image_a", `String saved_a);
                                  ("image_b", `String saved_b);
                                  ("metrics", `Assoc [
                                    ("ssim", `Float metrics.ssim);
                                    ("psnr", `Float metrics.psnr);
                                    ("mse", `Float metrics.mse);
                                    ("width_a", `Int metrics.width_a);
                                    ("height_a", `Int metrics.height_a);
                                    ("width_b", `Int metrics.width_b);
                                    ("height_b", `Int metrics.height_b);
                                    ("overlap_width", `Int metrics.overlap_width);
                                    ("overlap_height", `Int metrics.overlap_height);
                                  ]);
                                ] in
                                Ok result)))
             | (Error err, _) -> Error err
             | (_, Error err) -> Error err)
      in
      let max_scale = clamp_scale max_scale in
      let start_scale = clamp_scale start_scale in
      let rec loop scale best attempts =
        if scale > max_scale then
          (best, attempts)
        else
          let scale = clamp_scale scale in
          let result = compare_scale scale in
          let attempts = (match result with Ok r -> r | Error err ->
            `Assoc [("scale", `Int scale); ("error", `String err)]) :: attempts
          in
          let best =
            match (best, result) with
            | (None, Ok r) ->
                let ssim = match member "metrics" r with
                  | Some (`Assoc m) -> (match List.assoc_opt "ssim" m with Some (`Float v) -> v | _ -> 0.0)
                  | _ -> 0.0
                in
                Some (ssim, r)
            | (Some (best_score, _), Ok r) ->
                let ssim = match member "metrics" r with
                  | Some (`Assoc m) -> (match List.assoc_opt "ssim" m with Some (`Float v) -> v | _ -> 0.0)
                  | _ -> 0.0
                in
                if ssim > best_score then Some (ssim, r) else best
            | _ -> best
          in
          let should_stop =
            match target_ssim with
            | Some target ->
                (match result with
                 | Ok r ->
                     let ssim = match member "metrics" r with
                       | Some (`Assoc m) -> (match List.assoc_opt "ssim" m with Some (`Float v) -> v | _ -> 0.0)
                       | _ -> 0.0
                     in
                     ssim >= target
                 | Error _ -> false)
            | None -> true
          in
          if should_stop then
            (best, attempts)
          else
            loop (scale + scale_step) best attempts
      in
      let (best, attempts) = loop start_scale None [] in
      let (best_score, best_payload) =
        match best with
        | Some (score, payload) -> (score, payload)
        | None -> (0.0, `Null)
      in
      let result = `Assoc [
        ("file_key", `String file_key);
        ("node_a_id", `String node_a_id);
        ("node_b_id", `String node_b_id);
        ("target_ssim", match target_ssim with Some v -> `Float v | None -> `Null);
        ("best_score", `Float best_score);
        ("best", best_payload);
        ("attempts", `List (List.rev attempts));
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string result))
  | _ -> Error "Missing required parameters: file_key, node_a_id, node_b_id, token"

(** figma_verify_visual 핸들러 - Visual Feedback Loop *)
let handle_verify_visual args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  let token = get_string "token" args in
  let html = get_string "html" args in
  let target_ssim = get_float_or "target_ssim" 0.95 args in
  let max_iterations = match get_int "max_iterations" args with Some i when i > 0 -> i | _ -> 3 in
  let width = match get_int "width" args with Some w when w > 0 -> w | _ -> 375 in
  let height = match get_int "height" args with Some h when h > 0 -> h | _ -> 812 in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      (* 1. Figma에서 노드 PNG 내보내기 *)
      let figma_png_path = Printf.sprintf "/tmp/figma-visual/figma_%s_%s.png"
        file_key (sanitize_node_id node_id) in
      (match Figma_effects.Perform.get_images ~token ~file_key
              ~node_ids:[node_id] ~format:"png" ~scale:1 ?version () with
       | Error err -> Error (Printf.sprintf "Failed to get Figma image: %s" err)
       | Ok images_json ->
           let url_opt =
             match member "images" images_json with
             | Some (`Assoc map) ->
                 (match List.assoc_opt node_id map with
                  | Some (`String url) -> Some url
                  | _ -> None)
             | _ -> None
           in
           (match url_opt with
            | None -> Error (Printf.sprintf "Image URL not found for node: %s" node_id)
            | Some img_url ->
                (match Figma_effects.Perform.download_url ~url:img_url ~path:figma_png_path with
                 | Error err -> Error (Printf.sprintf "Failed to download Figma image: %s" err)
                 | Ok saved_figma_png ->
                     (* 2. 노드 데이터 가져오기 (HTML 생성 + 텍스트 검증용) *)
                     let parsed_node_opt, html_code =
                       match Figma_effects.Perform.get_nodes ~token ~file_key
                               ~node_ids:[node_id] ~depth:10 ?version () with
                       | Error _ -> (None, match html with Some h -> h | None -> "<html><body><div>Auto-generation failed</div></body></html>")
                       | Ok nodes_json ->
                           match member "nodes" nodes_json with
                           | Some (`Assoc nodes_map) ->
                               (match List.assoc_opt node_id nodes_map with
                                | Some node_data ->
                                    (match member "document" node_data with
                                     | Some doc_json ->
                                         let parsed = Figma_parser.parse_node doc_json in
                                         let generated_html = match parsed with
                                           | Some node -> Figma_codegen.generate_flat_html node
                                           | None -> "<html><body><div>Failed to parse node</div></body></html>"
                                         in
                                         (parsed, match html with Some h -> h | None -> generated_html)
                                     | _ -> (None, match html with Some h -> h | None -> "<html><body><div>No document</div></body></html>"))
                                | _ -> (None, match html with Some h -> h | None -> "<html><body><div>Node not found</div></body></html>"))
                           | _ -> (None, match html with Some h -> h | None -> "<html><body><div>No nodes</div></body></html>")
                     in
                     (* 3. Visual Feedback Loop 실행 (SSIM) *)
                     let result = Visual_verifier.verify_visual
                       ~target_ssim ~max_iterations ~width ~height
                       ~figma_png:saved_figma_png html_code
                     in
                     let result_json = Visual_verifier.result_to_json result in
                     (* 4. 텍스트 정확도 검증 *)
                     let text_verification_json = match parsed_node_opt with
                       | Some dsl_node ->
                           let text_result = Text_verifier.verify_texts ~dsl_node ~html:html_code in
                           Text_verifier.result_to_json text_result
                       | None -> `Assoc [
                           ("error", `String "Could not parse DSL node for text verification");
                           ("passed", `Bool false);
                         ]
                     in
                     (* 5. 종합 PASS/FAIL 결정 *)
                     let ssim_passed = result.Visual_verifier.passed in
                     let text_passed = match text_verification_json with
                       | `Assoc fields -> (match List.assoc_opt "passed" fields with Some (`Bool b) -> b | _ -> false)
                       | _ -> false
                     in
                     let overall_passed = ssim_passed && text_passed in
                     let full_result = `Assoc [
                       ("file_key", `String file_key);
                       ("node_id", `String node_id);
                       ("overall_passed", `Bool overall_passed);
                       ("visual_verification", result_json);
                       ("text_verification", text_verification_json);
                     ] in
                     Ok (make_text_content (Yojson.Safe.pretty_to_string full_result)))))
  | _ -> Error "Missing required parameters: file_key, node_id, token"

(** figma_compare_regions 핸들러 - 영역별 상세 비교 *)
let handle_compare_regions args : (Yojson.Safe.t, string) result =
  let output_dir = get_string_or "output_dir" "/tmp/figma-evolution/regions" args in
  let generate_diff = get_bool_or "generate_diff" true args in

  match (get_string "image_a" args, get_string "image_b" args, get_string "regions" args) with
  | (Some image_a, Some image_b, Some regions_json) ->
      (* 디렉토리 생성 *)
      let _ = Unix.system (Printf.sprintf "mkdir -p %s" (Filename.quote output_dir)) in

      (* regions JSON 파싱 *)
      let regions =
        try
          let json = Yojson.Safe.from_string regions_json in
          match json with
          | `List items ->
              List.filter_map (fun item ->
                let open Yojson.Safe.Util in
                try
                  let name = item |> member "name" |> to_string in
                  let x = item |> member "x" |> to_int in
                  let y = item |> member "y" |> to_int in
                  let width = item |> member "width" |> to_int in
                  let height = item |> member "height" |> to_int in
                  Some (name, x, y, width, height)
                with _ -> None
              ) items
          | _ -> []
        with _ -> []
      in

      if regions = [] then
        Error "Invalid regions JSON format. Expected: [{name, x, y, width, height}, ...]"
      else
        (* 각 영역별 SSIM 계산 *)
        let compare_region (name, x, y, w, h) =
          let crop_a = Printf.sprintf "%s/figma_%s.png" output_dir name in
          let crop_b = Printf.sprintf "%s/html_%s.png" output_dir name in

          (* ImageMagick으로 영역 crop *)
          let cmd_a = Printf.sprintf "magick %s -crop %dx%d+%d+%d +repage %s 2>/dev/null"
            (Filename.quote image_a) w h x y (Filename.quote crop_a) in
          let cmd_b = Printf.sprintf "magick %s -crop %dx%d+%d+%d +repage %s 2>/dev/null"
            (Filename.quote image_b) w h x y (Filename.quote crop_b) in
          let _ = Unix.system cmd_a in
          let _ = Unix.system cmd_b in

          (* SSIM 계산 *)
          let ssim_cmd = Printf.sprintf "magick compare -metric SSIM %s %s null: 2>&1"
            (Filename.quote crop_a) (Filename.quote crop_b) in
          let ic = Unix.open_process_in ssim_cmd in
          let output = try input_line ic with _ -> "" in
          let _ = Unix.close_process_in ic in

          (* 결과 파싱: "0.876543 (0.123457)" 형식 *)
          let ssim =
            try
              let re = Str.regexp "(\\([0-9.]+\\))" in
              if Str.string_match re output 0 then
                let diff = float_of_string (Str.matched_group 1 output) in
                (1.0 -. diff) *. 100.0  (* 유사도 = (1 - 차이율) * 100 *)
              else
                let parts = String.split_on_char ' ' output in
                match parts with
                | first :: _ -> float_of_string first *. 100.0
                | _ -> 0.0
            with _ -> 0.0
          in

          (* 차이 이미지 생성 *)
          let diff_image =
            if generate_diff then begin
              let diff_path = Printf.sprintf "%s/diff_%s.png" output_dir name in
              let diff_cmd = Printf.sprintf "magick compare %s %s %s 2>/dev/null"
                (Filename.quote crop_a) (Filename.quote crop_b) (Filename.quote diff_path) in
              let _ = Unix.system diff_cmd in
              Some diff_path
            end else None
          in

          `Assoc [
            ("name", `String name);
            ("region", `Assoc [
              ("x", `Int x);
              ("y", `Int y);
              ("width", `Int w);
              ("height", `Int h);
            ]);
            ("ssim_percent", `Float ssim);
            ("status", `String (if ssim >= 90.0 then "good" else if ssim >= 75.0 then "acceptable" else "needs_work"));
            ("figma_crop", `String crop_a);
            ("html_crop", `String crop_b);
            ("diff_image", match diff_image with Some p -> `String p | None -> `Null);
          ]
        in

        let results = List.map compare_region regions in

        (* 전체 통계 *)
        let ssims = List.filter_map (fun r ->
          match r with
          | `Assoc items ->
              (match List.assoc_opt "ssim_percent" items with
               | Some (`Float f) -> Some f
               | _ -> None)
          | _ -> None
        ) results in
        let avg_ssim = if ssims = [] then 0.0 else
          (List.fold_left (+.) 0.0 ssims) /. (float_of_int (List.length ssims)) in
        let min_ssim = if ssims = [] then 0.0 else List.fold_left min 100.0 ssims in
        let max_ssim = if ssims = [] then 0.0 else List.fold_left max 0.0 ssims in

        let summary = `Assoc [
          ("total_regions", `Int (List.length regions));
          ("average_ssim", `Float avg_ssim);
          ("min_ssim", `Float min_ssim);
          ("max_ssim", `Float max_ssim);
          ("overall_status", `String (
            if min_ssim >= 90.0 then "excellent"
            else if avg_ssim >= 85.0 then "good"
            else if avg_ssim >= 70.0 then "acceptable"
            else "needs_improvement"
          ));
        ] in

        let result = `Assoc [
          ("summary", summary);
          ("regions", `List results);
          ("output_dir", `String output_dir);
        ] in
        Ok (make_text_content (Yojson.Safe.pretty_to_string result))

  | _ -> Error "Missing required parameters: image_a, image_b, regions"

(** figma_evolution_report 핸들러 - 진화 과정 리포트 생성 *)
let handle_evolution_report args : (Yojson.Safe.t, string) result =
  let run_dir = get_string "run_dir" args in
  let generate_image = get_bool_or "generate_image" true args in

  (* 최근 evolution 디렉토리 목록 *)
  let list_recent_runs () =
    let cmd = "ls -dt /tmp/figma-evolution/run_* 2>/dev/null | head -10" in
    let ic = Unix.open_process_in cmd in
    let rec read_lines acc =
      try read_lines ((input_line ic) :: acc)
      with End_of_file -> List.rev acc
    in
    let lines = read_lines [] in
    let _ = Unix.close_process_in ic in
    lines
  in

  match run_dir with
  | None ->
      (* run_dir 없으면 최근 실행 목록 반환 *)
      let runs = list_recent_runs () in
      let runs_json = `List (List.map (fun r -> `String r) runs) in
      let result = `Assoc [
        ("recent_runs", runs_json);
        ("count", `Int (List.length runs));
        ("hint", `String "특정 run에 대한 리포트를 보려면 run_dir 파라미터를 지정하세요");
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string result))
  | Some dir ->
      if not (Sys.file_exists dir) then
        Error (sprintf "Evolution directory not found: %s" dir)
      else
        (* 해당 디렉토리의 진화 과정 분석 *)
        let figma_png = Filename.concat dir "figma_original.png" in
        let html_dir = Filename.concat dir "html" in

        (* step 파일들 읽기 *)
        let steps =
          if Sys.file_exists html_dir then
            let files = Sys.readdir html_dir |> Array.to_list in
            List.filter (fun f -> Filename.check_suffix f ".html") files
            |> List.sort compare
          else []
        in

        (* PNG 파일들 읽기 *)
        let pngs =
          Sys.readdir dir |> Array.to_list
          |> List.filter (fun f -> Filename.check_suffix f "_render.png")
          |> List.sort compare
        in

        (* 비교 이미지 생성 *)
        let comparison_image =
          if generate_image && List.length pngs > 0 then
            let last_png = Filename.concat dir (List.hd (List.rev pngs)) in
            let output = Filename.concat dir "evolution_comparison.png" in
            if Sys.file_exists figma_png && Sys.file_exists last_png then
              let cmd = sprintf "montage '%s' '%s' -tile 2x1 -geometry +5+5 -background '#1a1a1a' '%s' 2>/dev/null"
                figma_png last_png output in
              let _ = Sys.command cmd in
              if Sys.file_exists output then Some output else None
            else None
          else None
        in

        let result = `Assoc [
          ("run_dir", `String dir);
          ("figma_original", `String figma_png);
          ("html_steps", `List (List.map (fun f -> `String (Filename.concat html_dir f)) steps));
          ("png_renders", `List (List.map (fun f -> `String (Filename.concat dir f)) pngs));
          ("step_count", `Int (List.length steps));
          ("comparison_image", match comparison_image with Some p -> `String p | None -> `Null);
          ("summary", `String (sprintf "Evolution with %d steps. Final PNG: %s"
            (List.length steps)
            (if List.length pngs > 0 then List.hd (List.rev pngs) else "none")));
        ] in
        Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** figma_compare_elements 핸들러 - 색상/박스 확장 메트릭 비교 *)
let handle_compare_elements args : (Yojson.Safe.t, string) result =
  let compare_type = get_string "type" args in
  let color1 = get_string "color1" args in
  let color2 = get_string "color2" args in
  let box1 = get_string "box1" args in
  let box2 = get_string "box2" args in

  (* 색상 파싱 헬퍼 *)
  let parse_color str =
    let str = String.trim str in
    if String.length str > 0 && str.[0] = '#' then
      (* Hex format: #RRGGBB *)
      let hex = String.sub str 1 (String.length str - 1) in
      let r = int_of_string ("0x" ^ String.sub hex 0 2) in
      let g = int_of_string ("0x" ^ String.sub hex 2 2) in
      let b = int_of_string ("0x" ^ String.sub hex 4 2) in
      Some (float_of_int r /. 255.0, float_of_int g /. 255.0, float_of_int b /. 255.0)
    else if String.length str >= 4 && String.sub str 0 3 = "rgb" then
      (* RGB format: rgb(r,g,b) *)
      let re = Str.regexp "rgb(\\([0-9]+\\),[ ]*\\([0-9]+\\),[ ]*\\([0-9]+\\))" in
      if Str.string_match re str 0 then
        let r = int_of_string (Str.matched_group 1 str) in
        let g = int_of_string (Str.matched_group 2 str) in
        let b = int_of_string (Str.matched_group 3 str) in
        Some (float_of_int r /. 255.0, float_of_int g /. 255.0, float_of_int b /. 255.0)
      else None
    else None
  in

  (* 박스 파싱 헬퍼: "x,y,w,h" *)
  let parse_box str =
    match String.split_on_char ',' str |> List.map String.trim with
    | [x; y; w; h] ->
        (try Some (float_of_string x, float_of_string y, float_of_string w, float_of_string h)
         with _ -> None)
    | _ -> None
  in

  match compare_type with
  | Some "color" ->
      (match (color1, color2) with
       | (Some c1, Some c2) ->
           (match (parse_color c1, parse_color c2) with
            | (Some rgb1, Some rgb2) ->
                let metrics = Figma_similarity.compute_extended_color_metrics rgb1 rgb2 in
                let result = `Assoc [
                  ("type", `String "color");
                  ("color1", `String c1);
                  ("color2", `String c2);
                  ("oklab_distance", `Float metrics.oklab_distance);
                  ("oklab_similarity", `Float metrics.oklab_similarity);
                  ("ciede2000_distance", `Float metrics.ciede2000_distance);
                  ("ciede2000_similarity", `Float metrics.ciede2000_similarity);
                  ("rgb_euclidean", `Float metrics.rgb_euclidean);
                  ("formatted", `String (Figma_similarity.extended_color_to_string metrics));
                ] in
                Ok (make_text_content (Yojson.Safe.pretty_to_string result))
            | _ -> Error "Invalid color format. Use #RRGGBB or rgb(r,g,b)")
       | _ -> Error "Missing color1 or color2 for color comparison")

  | Some "box" ->
      (match (box1, box2) with
       | (Some b1, Some b2) ->
           (match (parse_box b1, parse_box b2) with
            | (Some bbox1, Some bbox2) ->
                let metrics = Figma_similarity.compute_extended_box_metrics bbox1 bbox2 in
                let result = `Assoc [
                  ("type", `String "box");
                  ("box1", `String b1);
                  ("box2", `String b2);
                  ("iou_value", `Float metrics.iou_value);
                  ("giou_value", `Float metrics.giou_value);
                  ("diou_value", `Float metrics.diou_value);
                  ("iou_similarity", `Float metrics.iou_similarity);
                  ("giou_similarity", `Float metrics.giou_similarity);
                  ("diou_similarity", `Float metrics.diou_similarity);
                  ("center_distance", `Float metrics.center_distance);
                  ("formatted", `String (Figma_similarity.extended_box_to_string metrics));
                ] in
                Ok (make_text_content (Yojson.Safe.pretty_to_string result))
            | _ -> Error "Invalid box format. Use x,y,w,h")
       | _ -> Error "Missing box1 or box2 for box comparison")

  | Some "full" ->
      let color_result =
        match (color1, color2) with
        | (Some c1, Some c2) ->
            (match (parse_color c1, parse_color c2) with
             | (Some rgb1, Some rgb2) ->
                 let m = Figma_similarity.compute_extended_color_metrics rgb1 rgb2 in
                 Some (`Assoc [
                   ("color1", `String c1);
                   ("color2", `String c2);
                   ("oklab_similarity", `Float m.oklab_similarity);
                   ("ciede2000_similarity", `Float m.ciede2000_similarity);
                   ("formatted", `String (Figma_similarity.extended_color_to_string m));
                 ])
             | _ -> None)
        | _ -> None
      in
      let box_result =
        match (box1, box2) with
        | (Some b1, Some b2) ->
            (match (parse_box b1, parse_box b2) with
             | (Some bbox1, Some bbox2) ->
                 let m = Figma_similarity.compute_extended_box_metrics bbox1 bbox2 in
                 Some (`Assoc [
                   ("box1", `String b1);
                   ("box2", `String b2);
                   ("iou_similarity", `Float m.iou_similarity);
                   ("giou_similarity", `Float m.giou_similarity);
                   ("diou_similarity", `Float m.diou_similarity);
                   ("formatted", `String (Figma_similarity.extended_box_to_string m));
                 ])
             | _ -> None)
        | _ -> None
      in
      let result = `Assoc [
        ("type", `String "full");
        ("color", match color_result with Some r -> r | None -> `Null);
        ("box", match box_result with Some r -> r | None -> `Null);
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string result))

  | _ -> Error "Invalid type. Use 'color', 'box', or 'full'"

(** figma_export_image 핸들러 *)
let handle_export_image args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_ids_str = get_string "node_ids" args in
  let token = get_string "token" args in
  let format = get_string_or "format" "png" args in
  let scale = get_float_or "scale" 1.0 args in
  let use_absolute_bounds = get_bool "use_absolute_bounds" args in
  let version = get_string "version" args in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in

  match (file_key, node_ids_str, token) with
  | (Some file_key, Some node_ids_str, Some token) ->
      let node_ids =
        node_ids_str
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
        |> List.map normalize_node_id
      in
      (match Figma_effects.Perform.get_images ~token ~file_key ~node_ids ~format ~scale:(int_of_float scale)
               ?use_absolute_bounds ?version () with
       | Ok json ->
           let images = member "images" json in
           let result = match images with
             | Some (`Assoc img_map) ->
                 List.map (fun (id, url) ->
                   match url with
                   | `String url ->
                       if download then
                         if is_http_url url then
                           let path = Printf.sprintf "%s/%s/%s.%s"
                             save_dir file_key (sanitize_node_id id) format in
                           (match Figma_effects.Perform.download_url ~url ~path with
                            | Ok saved -> sprintf "%s: %s -> %s" id url saved
                            | Error err -> sprintf "%s: %s (download error: %s)" id url err)
                         else
                           sprintf "%s: %s (download skipped: no URL)" id url
                       else
                         sprintf "%s: %s" id url
                   | _ -> sprintf "%s: (error)" id
                 ) img_map
                 |> String.concat "\n"
             | _ -> "No images returned"
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, node_ids, token"

(** figma_get_image_fills 핸들러 *)
let handle_get_image_fills args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let version = get_string "version" args in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_images ~token ~file_key ?version () with
       | Ok json ->
           let images =
             match member "images" json with
             | Some (`Assoc _ as m) -> m
             | _ -> `Null
           in
           let downloads =
             if download then
               match images with
               | `Assoc items ->
                   `List (List.map (download_image_fill save_dir file_key) items)
               | _ -> `List []
             else
               `List []
           in
           let result = `Assoc [
             ("images", images);
             ("downloads", downloads);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string result))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_nodes 핸들러 *)
let handle_get_nodes args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_ids_str = get_string "node_ids" args in
  let token = get_string "token" args in
  let format = get_string_or "format" "raw" args in
  let depth = get_int "depth" args in
  let geometry = get_string "geometry" args in
  let plugin_data = get_string "plugin_data" args in
  let version = get_string "version" args in

  match (file_key, node_ids_str, token) with
  | (Some file_key, Some node_ids_str, Some token) ->
      let node_ids =
        node_ids_str
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
        |> List.map normalize_node_id
      in
      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids ?depth ?geometry ?plugin_data ?version () with
       | Error err -> Error err
       | Ok json ->
           if format = "raw" then
             Ok (make_text_content (Yojson.Safe.pretty_to_string json))
           else
             let nodes = match member "nodes" json with
               | Some (`Assoc nodes_map) -> nodes_map
               | _ -> []
             in
             let converted =
               List.map (fun (id, node_json) ->
                 let doc = match member "document" node_json with
                   | Some d -> d
                   | None -> `Null
                 in
                 let dsl =
                   match process_json_string ~format (Yojson.Safe.to_string doc) with
                   | Ok s -> s
                   | Error msg -> "Error: " ^ msg
                 in
                 `Assoc [
                   ("node_id", `String id);
                   ("dsl", `String dsl);
                   ("node_raw", doc);
                 ]) nodes
             in
             let result = `Assoc [("nodes", `List converted)] in
             Ok (make_text_content (Yojson.Safe.pretty_to_string result)))
  | _ -> Error "Missing required parameters: file_key, node_ids, token"

(** figma_get_file_versions 핸들러 *)
let handle_get_file_versions args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_versions ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_file_comments 핸들러 *)
let handle_get_file_comments args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_comments ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_post_comment 핸들러 *)
let handle_post_comment args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let message = get_string "message" args in
  let x = get_float "x" args in
  let y = get_float "y" args in
  let node_id = get_string "node_id" args in

  match (file_key, token, message, x, y) with
  | (Some file_key, Some token, Some message, Some x, Some y) ->
      let client_meta =
        `Assoc (
          ("x", `Float x) ::
          ("y", `Float y) ::
          (match node_id with Some id -> [("node_id", `String id)] | None -> [])
        )
      in
      (match Figma_effects.Perform.post_file_comment ~token ~file_key ~message ~client_meta with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token, message, x, y"

(** figma_get_file_components 핸들러 *)
let handle_get_file_components args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_components ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_team_components 핸들러 *)
let handle_get_team_components args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = get_string "token" args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_components ~token ~team_id with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_get_file_component_sets 핸들러 *)
let handle_get_file_component_sets args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_component_sets ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_team_component_sets 핸들러 *)
let handle_get_team_component_sets args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = get_string "token" args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_component_sets ~token ~team_id with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_get_file_styles 핸들러 *)
let handle_get_file_styles args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_styles ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_team_styles 핸들러 *)
let handle_get_team_styles args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = get_string "token" args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_styles ~token ~team_id with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_get_component 핸들러 *)
let handle_get_component args : (Yojson.Safe.t, string) result =
  let component_key = get_string "component_key" args in
  let token = get_string "token" args in

  match (component_key, token) with
  | (Some component_key, Some token) ->
      (match Figma_effects.Perform.get_component ~token ~component_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: component_key, token"

(** figma_get_component_set 핸들러 *)
let handle_get_component_set args : (Yojson.Safe.t, string) result =
  let component_set_key = get_string "component_set_key" args in
  let token = get_string "token" args in

  match (component_set_key, token) with
  | (Some component_set_key, Some token) ->
      (match Figma_effects.Perform.get_component_set ~token ~component_set_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: component_set_key, token"

(** figma_get_style 핸들러 *)
let handle_get_style args : (Yojson.Safe.t, string) result =
  let style_key = get_string "style_key" args in
  let token = get_string "token" args in

  match (style_key, token) with
  | (Some style_key, Some token) ->
      (match Figma_effects.Perform.get_style ~token ~style_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: style_key, token"

(** figma_plugin_connect 핸들러 *)
let handle_plugin_connect args : (Yojson.Safe.t, string) result =
  let channel_id = get_string "channel_id" args in
  let channel_id = Figma_plugin_bridge.register_channel ?channel_id () in
  let result = `Assoc [
    ("status", `String "ok");
    ("channel_id", `String channel_id);
  ] in
  Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** figma_plugin_use_channel 핸들러 *)
let handle_plugin_use_channel args : (Yojson.Safe.t, string) result =
  match get_string "channel_id" args with
  | None -> Error "Missing required parameter: channel_id"
  | Some channel_id ->
      Figma_plugin_bridge.set_default_channel channel_id;
      let result = `Assoc [
        ("status", `String "ok");
        ("channel_id", `String channel_id);
      ] in
      Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** figma_plugin_status 핸들러 *)
let handle_plugin_status _args : (Yojson.Safe.t, string) result =
  let channels = Figma_plugin_bridge.list_channels () in
  let default_channel = Figma_plugin_bridge.get_default_channel () in
  let result = `Assoc [
    ("channels", `List (List.map (fun id -> `String id) channels));
    ("default_channel", match default_channel with Some id -> `String id | None -> `Null);
  ] in
  Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** figma_plugin_read_selection 핸들러 *)
let handle_plugin_read_selection args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let depth = get_int "depth" args |> Option.value ~default:6 in
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let payload = `Assoc [("depth", `Int depth)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"read_selection" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("channel_id", `String channel_id);
             ("command_id", `String command_id);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** figma_plugin_get_node 핸들러 *)
let handle_plugin_get_node args : (Yojson.Safe.t, string) result =
  match (resolve_node_id args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id (or url)"
  | (Some _, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let depth = get_int "depth" args |> Option.value ~default:6 in
      let include_geometry = get_bool_or "include_geometry" true args in
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let payload = `Assoc [
        ("node_id", `String node_id);
        ("depth", `Int depth);
        ("include_geometry", `Bool include_geometry);
      ] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_node" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("channel_id", `String channel_id);
             ("command_id", `String command_id);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** figma_plugin_export_node_image 핸들러 *)
let handle_plugin_export_node_image args : (Yojson.Safe.t, string) result =
  match (resolve_node_id args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id (or url)"
  | (Some _, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let format = get_string_or "format" "png" args in
      let scale = get_float_or "scale" 1.0 args in
      let payload = `Assoc [
        ("node_id", `String node_id);
        ("format", `String format);
        ("scale", `Float scale);
      ] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"export_node_image" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("channel_id", `String channel_id);
             ("command_id", `String command_id);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** figma_plugin_get_variables 핸들러 *)
let handle_plugin_get_variables args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let payload = `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_variables" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("channel_id", `String channel_id);
             ("command_id", `String command_id);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** figma_plugin_apply_ops 핸들러 *)
let handle_plugin_apply_ops args : (Yojson.Safe.t, string) result =
  match (get_json "ops" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: ops"
  | (_, Error msg) -> Error msg
  | (Some ops, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:20000 in
      let payload = `Assoc [("ops", ops)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"apply_ops" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result ->
           let response = `Assoc [
             ("channel_id", `String channel_id);
             ("command_id", `String command_id);
             ("ok", `Bool result.ok);
             ("payload", result.payload);
            ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string response)))

(** ============== LLM Bridge 핸들러 ============== *)

let has_field key fields =
  List.exists (fun (k, _) -> k = key) fields

let set_field key value fields =
  let filtered = List.filter (fun (k, _) -> k <> key) fields in
  (key, value) :: filtered

let add_if_missing key value fields =
  if has_field key fields then fields else (key, value) :: fields

let get_string_any keys json =
  let rec loop = function
    | [] -> None
    | key :: rest ->
        (match get_string key json with
         | Some v -> Some v
         | None -> loop rest)
  in
  loop keys

let truncate_string ~max_len value =
  if max_len <= 0 then value
  else if String.length value > max_len then
    String.sub value 0 max_len ^ "...(truncated)"
  else
    value

let is_utf8_continuation byte =
  byte land 0b1100_0000 = 0b1000_0000

let utf8_safe_boundary ~start ~max_bytes value =
  let len = String.length value in
  let pos = min (start + max_bytes) len in
  let rec back i =
    if i <= start then start
    else
      let byte = Char.code value.[i - 1] in
      if is_utf8_continuation byte then back (i - 1) else i
  in
  back pos

let truncate_utf8 ~max_bytes value =
  if max_bytes <= 0 then (value, false)
  else
    let len = String.length value in
    if len <= max_bytes then (value, false)
    else
      let cut = utf8_safe_boundary ~start:0 ~max_bytes value in
      let cut = if cut = 0 then min max_bytes len else cut in
      (String.sub value 0 cut, true)

let take_n n items =
  let rec loop acc remaining = function
    | [] -> List.rev acc
    | _ when remaining <= 0 -> List.rev acc
    | x :: xs -> loop (x :: acc) (remaining - 1) xs
  in
  loop [] n items

let chunk_list chunk_size items =
  let size = if chunk_size <= 0 then 1 else chunk_size in
  let rec loop acc current = function
    | [] ->
        let acc =
          if current = [] then acc
          else List.rev current :: acc
        in
        List.rev acc
    | x :: xs ->
        let current = x :: current in
        if List.length current >= size then
          loop (List.rev current :: acc) [] xs
        else
          loop acc current xs
  in
  loop [] [] items

let rec compact_json
    ~depth
    ~max_depth
    ~max_children
    ~max_list_items
    ~max_string
    json =
  match json with
  | `Assoc fields ->
      let fields =
        List.filter (fun (k, _) -> not (String.ends_with ~suffix:"_missing" k)) fields
      in
      let fields =
        if depth >= max_depth then
          List.filter (fun (k, _) -> k <> "children") fields
          |> fun filtered -> ("_depth_truncated", `Bool true) :: filtered
        else
          fields
      in
      let fields =
        List.map (fun (k, v) ->
          if k = "children" then
            match v with
            | `List items ->
                let total = List.length items in
                let items = take_n max_children items in
                let items =
                  List.map (compact_json
                              ~depth:(depth + 1)
                              ~max_depth
                              ~max_children
                              ~max_list_items
                              ~max_string) items
                in
                if total > List.length items then
                  (k, `List (items @ [`Assoc [("_truncated", `Bool true); ("total", `Int total)]]))
                else
                  (k, `List items)
            | _ ->
                (k, compact_json
                      ~depth:(depth + 1)
                      ~max_depth
                      ~max_children
                      ~max_list_items
                      ~max_string
                      v)
          else
            (k, compact_json
                  ~depth:(depth + 1)
                  ~max_depth
                  ~max_children
                  ~max_list_items
                  ~max_string
                  v)
        ) fields
      in
      `Assoc fields
  | `List items ->
      let total = List.length items in
      let items = take_n max_list_items items in
      let items =
        List.map (compact_json
                    ~depth:(depth + 1)
                    ~max_depth
                    ~max_children
                    ~max_list_items
                    ~max_string) items
      in
      if total > List.length items then
        `List (items @ [`Assoc [("_truncated", `Bool true); ("total", `Int total)]])
      else
        `List items
  | `String s -> `String (truncate_string ~max_len:max_string s)
  | other -> other

let chunkify_children ~chunk_size json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "children" fields with
      | Some (`List children) ->
          let chunks = chunk_list chunk_size children in
          let total = List.length chunks in
          let chunks =
            List.mapi (fun idx chunk ->
              `Assoc [
                ("chunk_index", `Int (idx + 1));
                ("chunk_total", `Int total);
                ("children", `List chunk);
              ]) chunks
          in
          let fields = List.filter (fun (k, _) -> k <> "children") fields in
          `Assoc (("chunks", `List chunks) :: ("chunk_total", `Int total) :: fields)
      | _ -> json)
  | _ -> json

let chunkify_text ~chunk_size text =
  let size = if chunk_size <= 0 then 1 else chunk_size in
  let len = String.length text in
  let rec loop idx acc =
    if idx >= len then List.rev acc
    else
      let next = utf8_safe_boundary ~start:idx ~max_bytes:size text in
      let next = if next <= idx then min (idx + size) len else next in
      let chunk = String.sub text idx (next - idx) in
      loop next (chunk :: acc)
  in
  let chunks = loop 0 [] in
  let total = List.length chunks in
  let chunks =
    List.mapi (fun idx chunk ->
      `Assoc [
        ("chunk_index", `Int (idx + 1));
        ("chunk_total", `Int total);
        ("content", `String chunk);
      ]) chunks
  in
  `Assoc [
    ("chunked_text", `Bool true);
    ("chunk_total", `Int total);
    ("chunks", `List chunks);
  ]

let select_chunked_json ~selected json =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "chunks" fields with
      | Some (`List chunks) ->
          let selected_set =
            selected
            |> List.map (fun v -> (v, ()))
            |> List.to_seq
            |> Hashtbl.of_seq
          in
          let keep chunk =
            match chunk with
            | `Assoc chunk_fields ->
                (match List.assoc_opt "chunk_index" chunk_fields with
                 | Some (`Int idx) -> Hashtbl.mem selected_set idx
                 | Some (`Float f) -> Hashtbl.mem selected_set (int_of_float f)
                 | _ -> false)
            | _ -> false
          in
          let chunks = List.filter keep chunks in
          let fields = List.filter (fun (k, _) -> k <> "chunks") fields in
          `Assoc (("chunks", `List chunks) :: ("chunk_selected", `List (List.map (fun v -> `Int v) selected)) :: fields)
      | _ -> json)
  | _ -> json

type chunk_stats = {
  mutable total_nodes: int;
  type_counts: (string, int) Hashtbl.t;
}

let create_chunk_stats () =
  { total_nodes = 0; type_counts = Hashtbl.create 32 }

let bump_count counts key =
  let current = match Hashtbl.find_opt counts key with
    | Some v -> v
    | None -> 0
  in
  Hashtbl.replace counts key (current + 1)

let rec collect_chunk_stats stats json =
  match json with
  | `Assoc fields ->
      stats.total_nodes <- stats.total_nodes + 1;
      (match List.assoc_opt "type" fields with
       | Some (`String t) -> bump_count stats.type_counts t
       | _ -> ());
      (match List.assoc_opt "children" fields with
       | Some (`List kids) ->
           List.iter (collect_chunk_stats stats) kids
       | _ -> ())
  | _ -> ()

let count_for stats key =
  match Hashtbl.find_opt stats.type_counts key with
  | Some v -> v
  | None -> 0

let score_for_chunk stats =
  let frames = count_for stats "FRAME" in
  let components =
    count_for stats "COMPONENT" + count_for stats "COMPONENT_SET"
  in
  let instances = count_for stats "INSTANCE" in
  let texts = count_for stats "TEXT" in
  let vectors =
    count_for stats "VECTOR"
    + count_for stats "BOOLEAN_OPERATION"
    + count_for stats "ELLIPSE"
    + count_for stats "RECTANGLE"
    + count_for stats "LINE"
  in
  stats.total_nodes
  + (frames * 5)
  + (components * 4)
  + (instances * 3)
  + (texts * 3)
  + vectors

let type_counts_to_json counts =
  let items =
    Hashtbl.to_seq counts
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  `Assoc (List.map (fun (k, v) -> (k, `Int v)) items)

let string_field key json =
  match json with
  | `Assoc fields ->
      (match List.assoc_opt key fields with
       | Some (`String s) -> Some s
       | _ -> None)
  | _ -> None

let collect_samples key children sample_size =
  children
  |> List.filter_map (string_field key)
  |> take_n sample_size

type chunk_entry = {
  index: int;
  total: int;
  child_count: int;
  node_count: int;
  type_counts: Yojson.Safe.t;
  name_samples: string list;
  id_samples: string list;
  size_chars: int;
  score: int;
}

let chunk_entry_to_json entry =
  `Assoc [
    ("chunk_index", `Int entry.index);
    ("chunk_total", `Int entry.total);
    ("child_count", `Int entry.child_count);
    ("node_count", `Int entry.node_count);
    ("type_counts", entry.type_counts);
    ("name_samples", `List (List.map (fun s -> `String s) entry.name_samples));
    ("id_samples", `List (List.map (fun s -> `String s) entry.id_samples));
    ("size_chars", `Int entry.size_chars);
    ("score", `Int entry.score);
  ]

let build_chunk_entries ~chunk_size ~sample_size json =
  let chunked = chunkify_children ~chunk_size json in
  match chunked with
  | `Assoc fields -> (
      match List.assoc_opt "chunks" fields with
      | Some (`List chunks) ->
          let total = List.length chunks in
          let entries =
            List.filter_map (fun chunk ->
              match chunk with
              | `Assoc chunk_fields ->
                  let index =
                    match List.assoc_opt "chunk_index" chunk_fields with
                    | Some (`Int i) -> i
                    | _ -> 0
                  in
                  let children =
                    match List.assoc_opt "children" chunk_fields with
                    | Some (`List kids) -> kids
                    | _ -> []
                  in
                  let stats = create_chunk_stats () in
                  List.iter (collect_chunk_stats stats) children;
                  let name_samples = collect_samples "name" children sample_size in
                  let id_samples = collect_samples "id" children sample_size in
                  let size_chars = String.length (Yojson.Safe.to_string chunk) in
                  let entry = {
                    index;
                    total;
                    child_count = List.length children;
                    node_count = stats.total_nodes;
                    type_counts = type_counts_to_json stats.type_counts;
                    name_samples;
                    id_samples;
                    size_chars;
                    score = score_for_chunk stats;
                  } in
                  Some entry
              | _ -> None
            ) chunks
          in
          (chunked, entries)
      | _ -> (chunked, [])
    )
  | _ -> (json, [])

let select_chunks_heuristic entries limit =
  let sorted =
    List.sort (fun a b -> compare b.score a.score) entries
  in
  take_n limit sorted |> List.map (fun e -> e.index)

type plugin_stats = {
  mutable node_count: int;
  mutable text_nodes: int;
  mutable segment_count: int;
  mutable segment_bounds_count: int;
  type_counts: (string, int) Hashtbl.t;
  mutable name_samples: string list;
  mutable text_samples: string list;
  mutable selection_count: int option;
}

let create_plugin_stats () =
  {
    node_count = 0;
    text_nodes = 0;
    segment_count = 0;
    segment_bounds_count = 0;
    type_counts = Hashtbl.create 32;
    name_samples = [];
    text_samples = [];
    selection_count = None;
  }

let append_sample ~max items value =
  if value = "" then items
  else if List.length items >= max then items
  else value :: items

let count_segment_bounds segments =
  List.fold_left (fun acc seg ->
    match seg with
    | `Assoc fields ->
        (match List.assoc_opt "bounds" fields with
         | Some (`Null) | None -> acc
         | _ -> acc + 1)
    | _ -> acc
  ) 0 segments

let rec collect_plugin_stats ~sample_size stats json =
  match json with
  | `Assoc fields ->
      stats.node_count <- stats.node_count + 1;
      (match List.assoc_opt "type" fields with
       | Some (`String t) -> bump_count stats.type_counts t
       | _ -> ());
      (match List.assoc_opt "name" fields with
       | Some (`String name) ->
           stats.name_samples <- append_sample ~max:sample_size stats.name_samples name
       | _ -> ());
      (match List.assoc_opt "text" fields with
       | Some (`Assoc text_fields) ->
           stats.text_nodes <- stats.text_nodes + 1;
           (match List.assoc_opt "characters" text_fields with
            | Some (`String chars) ->
                let snippet = truncate_string ~max_len:80 chars in
                stats.text_samples <- append_sample ~max:sample_size stats.text_samples snippet
            | _ -> ());
           (match List.assoc_opt "segments" text_fields with
            | Some (`List segments) ->
                stats.segment_count <- stats.segment_count + List.length segments;
                stats.segment_bounds_count <-
                  stats.segment_bounds_count + count_segment_bounds segments
            | _ -> ())
       | _ -> ());
      (match List.assoc_opt "children" fields with
       | Some (`List kids) ->
           List.iter (collect_plugin_stats ~sample_size stats) kids
       | _ -> ())
  | `List items ->
      List.iter (collect_plugin_stats ~sample_size stats) items
  | _ -> ()

let summarize_plugin_payload ~sample_size payload =
  match payload with
  | `Assoc fields -> (
      match List.assoc_opt "error" fields with
      | Some (`String err) -> `Assoc [("error", `String err)]
      | Some _ -> `Assoc [("error", `String "Plugin payload error")]
      | None ->
          let stats = create_plugin_stats () in
          (match List.assoc_opt "selectionCount" fields with
           | Some (`Int v) -> stats.selection_count <- Some v
           | Some (`Float f) -> stats.selection_count <- Some (int_of_float f)
           | _ -> ());
          let nodes =
            match List.assoc_opt "nodes" fields with
            | Some (`List nodes) -> `List nodes
            | _ -> payload
          in
          collect_plugin_stats ~sample_size stats nodes;
          let summary = [
            ("node_count", `Int stats.node_count);
            ("text_nodes", `Int stats.text_nodes);
            ("segment_count", `Int stats.segment_count);
            ("segment_bounds_count", `Int stats.segment_bounds_count);
            ("type_counts", type_counts_to_json stats.type_counts);
            ("name_samples", `List (List.rev_map (fun s -> `String s) stats.name_samples |> List.rev));
            ("text_samples", `List (List.rev_map (fun s -> `String s) stats.text_samples |> List.rev));
          ] in
          let summary =
            match stats.selection_count with
            | Some v -> ("selection_count", `Int v) :: summary
            | None -> summary
          in
          `Assoc summary
    )
  | _ -> `Assoc [("error", `String "Invalid plugin payload")]

let strip_llm_prefix text =
  if String.starts_with ~prefix:"RES|" text then
    let len = String.length text in
    let rec loop idx pipes =
      if idx >= len then text
      else if text.[idx] = '|' then
        if pipes >= 3 then String.sub text (idx + 1) (len - idx - 1)
        else loop (idx + 1) (pipes + 1)
      else
        loop (idx + 1) pipes
    in
    loop 0 0
  else
    text

let parse_json_int_list text =
  try
    match Yojson.Safe.from_string text with
    | `List items ->
        let values =
          List.filter_map (function
            | `Int i -> Some i
            | `Float f -> Some (int_of_float f)
            | `String s -> (try Some (int_of_string s) with _ -> None)
            | _ -> None) items
        in
        values
    | _ -> []
  with _ -> []

let extract_json_array text =
  match (String.index_opt text '[', String.rindex_opt text ']') with
  | Some start_idx, Some end_idx when end_idx > start_idx ->
      let len = end_idx - start_idx + 1 in
      let slice = String.sub text start_idx len in
      parse_json_int_list slice
  | _ -> []

let extract_ints text =
  let re = Str.regexp "[0-9]+" in
  let rec loop pos acc =
    try
      let _ = Str.search_forward re text pos in
      let value = int_of_string (Str.matched_string text) in
      loop (Str.match_end ()) (value :: acc)
    with Not_found -> List.rev acc
  in
  loop 0 []

let dedup_sorted values =
  List.sort_uniq compare values

let parse_chunk_indices ~chunk_total text =
  let text = strip_llm_prefix text |> String.trim in
  let values =
    let parsed = parse_json_int_list text in
    if parsed <> [] then parsed
    else
      let parsed = extract_json_array text in
      if parsed <> [] then parsed else extract_ints text
  in
  values
  |> List.filter (fun v -> v >= 1 && v <= chunk_total)
  |> dedup_sorted

let parse_chunk_selection_info selection_json =
  let selected_count =
    match selection_json with
    | `Assoc fields -> (
        match List.assoc_opt "selected" fields with
        | Some (`List items) -> List.length items
        | _ -> 0)
    | _ -> 0
  in
  let chunk_total =
    match selection_json with
    | `Assoc fields -> (
        match List.assoc_opt "chunk_total" fields with
        | Some (`Int n) -> n
        | Some (`Float f) -> int_of_float f
        | _ -> 0)
    | _ -> 0
  in
  (selected_count, chunk_total)

let parse_tool_choice text =
  let normalized = String.lowercase_ascii text in
  let contains_substring s substring =
    let len = String.length substring in
    let rec loop i =
      if i + len > String.length s then false
      else if String.sub s i len = substring then true
      else loop (i + 1)
    in
    if len = 0 then true else loop 0
  in
  let parse_json_tool () =
    try
      match Yojson.Safe.from_string text with
      | `Assoc fields -> (
          match List.assoc_opt "tool" fields with
          | Some (`String s) -> Some (String.lowercase_ascii s)
          | _ ->
              (match List.assoc_opt "name" fields with
               | Some (`String s) -> Some (String.lowercase_ascii s)
               | _ -> None))
      | _ -> None
    with _ -> None
  in
  match parse_json_tool () with
  | Some tool -> Some tool
  | None ->
      let tools = ["codex"; "claude-cli"; "gemini"; "ollama"] in
      List.find_opt (fun tool -> contains_substring normalized tool) tools

type llm_task_preset = {
  quality: string option;
  context_strategy: string option;
  chunk_select_mode: string option;
  chunk_select_limit: int option;
  chunk_select_sample_size: int option;
  chunk_select_task: string option;
  plugin_context_mode: string option;
  plugin_summary_sample_size: int option;
  plugin_depth: int option;
  plugin_include_geometry: bool option;
  include_plugin: bool option;
  include_variables: bool option;
  include_image_fills: bool option;
  max_context_chars: int option;
  retry_on_llm_error: bool option;
  max_retries: int option;
  min_context_chars: int option;
  retry_context_scale: float option;
}

type llm_task_context = {
  dsl_context: Yojson.Safe.t;
  chunk_selection: Yojson.Safe.t;
  variables: Yojson.Safe.t;
  variables_source: Yojson.Safe.t;
  image_fills: Yojson.Safe.t;
  plugin_snapshot_raw: Yojson.Safe.t;
  plugin_snapshot: Yojson.Safe.t;
  plugin_summary: Yojson.Safe.t;
  warnings: string list;
  context_json: Yojson.Safe.t;
  context_str_full: string;
}

type llm_tool_selection = {
  tool: string;
  mode: string;
  reason: string option;
}

type llm_task_config = {
  quality: string;
  provider_name: string option;
  llm_tool_raw: string;
  llm_tool_selector_mode: string;
  llm_tool_selector_tool: string;
  llm_tool_selector_provider: string option;
  llm_tool_selector_args: Yojson.Safe.t option;
  llm_tool_selector_task: string option;
  llm_tool_selector_mcp_url: string option;
  llm_call_policy: string;
  llm_dry_run: bool;
  preflight_max_truncation: float;
  preflight_require_plugin: bool;
  auto_fix_enabled: bool;
  auto_fix_max_attempts: int;
  critic_enabled: bool;
  critic_tool: string;
  critic_provider: string option;
  critic_args: Yojson.Safe.t option;
  critic_task: string option;
  critic_mcp_url: string option;
  critic_min_score: float;
  critic_max_retries: int;
  critic_retry_context_scale: float;
  file_key: string option;
  node_id: string option;
  token: string option;
  depth: int option;
  geometry: string option;
  include_variables: bool;
  include_image_fills: bool;
  include_plugin: bool;
  auto_plugin: bool;
  plugin_context_mode: string;
  plugin_summary_sample_size: int;
  plugin_depth: int;
  plugin_include_geometry: bool;
  plugin_timeout_ms: int;
  plugin_mode: string;
  plugin_channel_id: string option;
  context_strategy: string;
  context_max_depth: int;
  context_max_children: int;
  context_max_list_items: int;
  context_max_string: int;
  context_chunk_size: int;
  chunk_select_mode: string;
  chunk_select_limit: int;
  chunk_select_sample_size: int;
  chunk_select_task: string option;
  chunk_select_provider: string option;
  chunk_select_llm_tool: string;
  chunk_select_llm_args: Yojson.Safe.t option;
  chunk_select_mcp_url: string option;
  max_context_chars: int;
  retry_on_llm_error: bool;
  max_retries: int;
  min_context_chars: int;
  retry_context_scale: float;
  return_metadata: bool;
}

type preflight_issue = {
  code: string;
  message: string;
  suggestion: string option;
}

let make_issue ?suggestion code message =
  { code; message; suggestion }

let issue_to_json issue =
  let base = [
    ("code", `String issue.code);
    ("message", `String issue.message);
  ] in
  match issue.suggestion with
  | Some s -> `Assoc (("suggestion", `String s) :: base)
  | None -> `Assoc base

let empty_preset = {
  quality = None;
  context_strategy = None;
  chunk_select_mode = None;
  chunk_select_limit = None;
  chunk_select_sample_size = None;
  chunk_select_task = None;
  plugin_context_mode = None;
  plugin_summary_sample_size = None;
  plugin_depth = None;
  plugin_include_geometry = None;
  include_plugin = None;
  include_variables = None;
  include_image_fills = None;
  max_context_chars = None;
  retry_on_llm_error = None;
  max_retries = None;
  min_context_chars = None;
  retry_context_scale = None;
}

let resolve_llm_task_preset name =
  match String.lowercase_ascii name with
  | "draft" | "fast" ->
      Some { empty_preset with
        quality = Some "fast";
        include_plugin = Some false;
        include_variables = Some false;
        include_image_fills = Some false;
        context_strategy = Some "raw";
        max_context_chars = Some 120000;
        retry_on_llm_error = Some false;
      }
  | "balanced" ->
      Some { empty_preset with
        quality = Some "balanced";
        context_strategy = Some "chunked";
        chunk_select_mode = Some "heuristic";
        chunk_select_limit = Some 4;
        plugin_context_mode = Some "summary";
        plugin_summary_sample_size = Some 5;
        include_variables = Some true;
        include_image_fills = Some false;
        max_context_chars = Some 600000;
        retry_on_llm_error = Some true;
        max_retries = Some 1;
        min_context_chars = Some 400000;
        retry_context_scale = Some 0.6;
      }
  | "fidelity" | "pixel" ->
      Some { empty_preset with
        quality = Some "best";
        context_strategy = Some "chunked";
        chunk_select_mode = Some "llm";
        chunk_select_limit = Some 6;
        plugin_context_mode = Some "both";
        plugin_depth = Some 1;
        include_variables = Some true;
        include_image_fills = Some true;
        max_context_chars = Some 1000000;
        retry_on_llm_error = Some true;
        max_retries = Some 1;
        min_context_chars = Some 600000;
        retry_context_scale = Some 0.5;
      }
  | "text" ->
      Some { empty_preset with
        quality = Some "best";
        context_strategy = Some "chunked";
        chunk_select_mode = Some "heuristic";
        chunk_select_limit = Some 4;
        chunk_select_task = Some "Focus on text-heavy chunks and typography fidelity.";
        plugin_context_mode = Some "full";
        plugin_depth = Some 0;
        include_image_fills = Some false;
      }
  | "icon" | "vector" ->
      Some { empty_preset with
        quality = Some "best";
        context_strategy = Some "chunked";
        chunk_select_mode = Some "heuristic";
        chunk_select_limit = Some 4;
        chunk_select_task = Some "Focus on icon/vector-heavy chunks.";
        plugin_context_mode = Some "full";
        plugin_depth = Some 0;
        plugin_include_geometry = Some true;
        include_image_fills = Some false;
      }
  | _ -> None

let handle_chunk_index args : (Yojson.Safe.t, string) result Lwt.t =
  let open Lwt.Syntax in
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = get_string "token" args in
  let format = get_string_or "format" "fidelity" args in
  let depth = get_int "depth" args in
  let geometry = get_string "geometry" args in
  let chunk_size = match get_int "chunk_size" args with Some n when n > 0 -> n | _ -> 50 in
  let sample_size = match get_int "sample_size" args with Some n when n > 0 -> n | _ -> 6 in
  let context_max_depth = get_int "context_max_depth" args |> Option.value ~default:6 in
  let context_max_children = get_int "context_max_children" args |> Option.value ~default:200 in
  let context_max_list_items = get_int "context_max_list_items" args |> Option.value ~default:200 in
  let context_max_string = get_int "context_max_string" args |> Option.value ~default:2000 in
  let selection_mode = get_string_or "selection_mode" "none" args in
  let selection_limit = match get_int "selection_limit" args with Some n when n > 0 -> n | _ -> 4 in
  let selection_task = get_string "selection_task" args in
  let selection_provider =
    match get_string "selection_provider" args with
    | Some v -> Some v
    | None ->
        (match get_string "provider" args with
         | Some v -> Some v
         | None -> get_string "llm_provider" args)
  in
  let selection_llm_tool =
    match get_string "selection_llm_tool" args with
    | Some v -> v
    | None ->
        (match get_string "llm_tool" args with
         | Some v -> v
         | None ->
             (match get_string "tool_name" args with
              | Some v -> v
              | None -> "codex"))
  in
  let selection_llm_args = get_json "selection_llm_args" args in
  let selection_mcp_url = get_string "selection_mcp_url" args in

  let warnings = ref [] in
  let add_warning msg = warnings := msg :: !warnings in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let cache_options =
        List.filter_map Fun.id [
          Option.map (Printf.sprintf "depth:%d") depth;
          Option.map (Printf.sprintf "geometry:%s") geometry;
          Some (Printf.sprintf "format:%s" format);
        ]
      in
      let json_result =
        run_with_effects (fun () ->
          match Figma_cache.get ~file_key ~node_id ~options:cache_options () with
          | Some json -> Ok json
          | None ->
              match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ?depth ?geometry () with
              | Ok json ->
                  Figma_cache.set ~file_key ~node_id ~options:cache_options json;
                  Ok json
              | Error err -> Error err)
      in
      (match json_result with
       | Error err -> Lwt.return (Error err)
       | Ok json ->
           let node_data = match member "nodes" json with
             | Some (`Assoc nodes_map) ->
                 (match List.assoc_opt node_id nodes_map with
                  | Some n -> member "document" n
                  | None -> None)
             | _ -> None
           in
           (match node_data with
            | None -> Lwt.return (Error (Printf.sprintf "Node not found: %s" node_id))
            | Some node ->
                let base_json =
                  match format with
                  | "raw" -> node
                  | _ ->
                      let node_str = Yojson.Safe.to_string node in
                      (match process_json_string ~format:"fidelity" node_str with
                       | Ok dsl_str ->
                           (try Yojson.Safe.from_string dsl_str with _ ->
                              add_warning "Fidelity DSL parse failed; falling back to raw";
                              node)
                       | Error msg ->
                           add_warning ("DSL conversion failed: " ^ msg);
                           node)
                in
                let compacted =
                  compact_json
                    ~depth:0
                    ~max_depth:context_max_depth
                    ~max_children:context_max_children
                    ~max_list_items:context_max_list_items
                    ~max_string:context_max_string
                    base_json
                in
                let (chunked_json, entries) =
                  build_chunk_entries ~chunk_size ~sample_size compacted
                in
                let chunk_total = List.length entries in
                let effective_limit = min selection_limit chunk_total in
                let chunked_str = Yojson.Safe.to_string chunked_json in
                let prefix = Printf.sprintf "chunk_index_%s" (sanitize_node_id node_id) in
                let file_path = Large_response.save_to_file ~prefix chunked_str in
                let size_bytes = String.length chunked_str in
                let chunk_store = `Assoc [
                  ("file_path", `String file_path);
                  ("size_bytes", `Int size_bytes);
                  ("size_human", `String (Large_response.human_size size_bytes));
                  ("ttl_seconds", `Int Large_response.response_ttl);
                ] in

                let index_json =
                  `List (List.map chunk_entry_to_json entries)
                in

                let* selection_json =
                  if selection_mode = "heuristic" then
                    let selected =
                      if effective_limit <= 0 then []
                      else select_chunks_heuristic entries effective_limit
                    in
                    Lwt.return (`Assoc [
                      ("mode", `String "heuristic");
                      ("selected", `List (List.map (fun v -> `Int v) selected));
                    ])
                  else if selection_mode = "llm" then
                    if effective_limit <= 0 then
                      Lwt.return (`Assoc [
                        ("mode", `String "llm");
                        ("selected", `List []);
                      ])
                    else
                      let prompt_task =
                        match selection_task with
                        | Some t -> t
                        | None -> "Select the most relevant chunks for high-fidelity output."
                      in
                      let prompt =
                        Printf.sprintf
                          "Task: %s\n\nChunk index (JSON):\n%s\n\nReturn JSON array of chunk_index values (max %d)."
                          prompt_task
                          (Yojson.Safe.pretty_to_string (`Assoc [
                             ("chunk_total", `Int chunk_total);
                             ("chunk_size", `Int chunk_size);
                             ("chunks", index_json);
                           ]))
                          selection_limit
                      in
                      let llm_args_fields =
                        match selection_llm_args with
                        | None -> Ok []
                        | Some (`Assoc fields) -> Ok fields
                        | Some _ -> Error "selection_llm_args must be an object"
                      in
                      match llm_args_fields with
                      | Error msg ->
                          add_warning msg;
                          Lwt.return (`Assoc [("mode", `String "llm"); ("error", `String msg)])
                      | Ok fields ->
                          let fields = set_field "prompt" (`String prompt) fields in
                          let fields =
                            if has_field "response_format" fields then fields
                            else add_if_missing "response_format" (`String "compact") fields
                          in
                          let fields = add_if_missing "stream" (`Bool false) fields in
                          match Llm_provider.resolve ?provider:selection_provider () with
                          | Error msg ->
                              add_warning msg;
                              Lwt.return (`Assoc [("mode", `String "llm"); ("error", `String msg)])
                          | Ok provider ->
                              let llm_url =
                                Option.value selection_mcp_url ~default:provider.default_url
                              in
                              let* llm_result =
                                call_llm_tool ~provider ~url:llm_url ~name:selection_llm_tool ~arguments:(`Assoc fields)
                              in
                              match llm_result with
                              | Error err ->
                                  add_warning err;
                                  Lwt.return (`Assoc [("mode", `String "llm"); ("error", `String err)])
                              | Ok response ->
                                  let indices =
                                    parse_chunk_indices ~chunk_total response.text
                                    |> take_n effective_limit
                                  in
                                  let selected =
                                    if indices = [] then
                                      select_chunks_heuristic entries effective_limit
                                    else
                                      indices
                                  in
                                  Lwt.return (`Assoc [
                                    ("mode", `String "llm");
                                    ("selected", `List (List.map (fun v -> `Int v) selected));
                                    ("raw_response", `String response.text);
                                  ])
                  else
                    Lwt.return `Null
                in

                let result =
                  let base = [
                    ("file_key", `String file_key);
                    ("node_id", `String node_id);
                    ("format", `String format);
                    ("chunk_size", `Int chunk_size);
                    ("chunk_total", `Int chunk_total);
                    ("chunk_store", chunk_store);
                    ("index", index_json);
                    ("selection", selection_json);
                  ] in
                  let base =
                    if !warnings = [] then base
                    else ("warnings", `List (List.map (fun s -> `String s) (List.rev !warnings))) :: base
                  in
                  `Assoc base
                in
                Lwt.return (Ok result))
       )
  | _ -> Lwt.return (Error "Missing required parameters: file_key/node_id or url, token")

let handle_chunk_get args : (Yojson.Safe.t, string) result =
  let file_path = get_string "file_path" args in
  let chunk_index = get_int "chunk_index" args in
  match (file_path, chunk_index) with
  | (Some path, Some index) ->
      let storage_dir = Large_response.storage_dir in
      if not (is_under_dir ~dir:storage_dir path) then
        Error (Printf.sprintf "file_path must be under %s" storage_dir)
      else if not (Sys.file_exists path) then
        Error (Printf.sprintf "File not found: %s" path)
      else
        (match Yojson.Safe.from_file path with
         | json ->
             let chunks =
               match member "chunks" json with
               | Some (`List items) -> items
               | _ -> []
             in
             let chunk_total = List.length chunks in
             if index <= 0 || index > chunk_total then
               Error (Printf.sprintf "chunk_index out of range (1-%d)" chunk_total)
             else
               let chunk = List.nth chunks (index - 1) in
               let payload = `Assoc [
                 ("chunk_index", `Int index);
                 ("chunk_total", `Int chunk_total);
                 ("chunk", chunk);
               ] in
               let prefix = Printf.sprintf "chunk_%d" index in
               Ok (Large_response.wrap_json_result ~prefix ~format:"json" payload)
         | exception exn ->
             Error (Printexc.to_string exn))
  | _ -> Error "Missing required parameters: file_path, chunk_index"

let handle_llm_call args : (Yojson.Safe.t, string) result Lwt.t =
  let open Lwt.Syntax in
  let provider_name = get_string_any ["provider"; "llm_provider"] args in
  let llm_tool = get_string_any ["tool_name"; "llm_tool"] args |> Option.value ~default:"codex" in
  let return_metadata = get_bool_or "return_metadata" false args in
  let base_args =
    match get_json "arguments" args with
    | None -> Ok []
    | Some (`Assoc fields) -> Ok fields
    | Some _ -> Error "arguments must be an object"
  in
  match base_args with
  | Error msg -> Lwt.return (Error msg)
  | Ok fields ->
      let fields =
        match get_string "prompt" args with
        | Some prompt -> set_field "prompt" (`String prompt) fields
        | None -> fields
      in
      let fields =
        match get_string "response_format" args with
        | Some fmt -> set_field "response_format" (`String fmt) fields
        | None -> fields
      in
      let fields = add_if_missing "stream" (`Bool false) fields in
      let arguments = `Assoc fields in
      (match Llm_provider.resolve ?provider:provider_name () with
       | Error msg -> Lwt.return (Error msg)
       | Ok provider ->
           let llm_url =
             get_string_any ["mcp_url"; "llm_url"] args
             |> Option.value ~default:provider.default_url
           in
           let* result = call_llm_tool ~provider ~url:llm_url ~name:llm_tool ~arguments in
           (match result with
            | Error err -> Lwt.return (Error err)
            | Ok response ->
                if return_metadata then
                  let payload = `Assoc [
                    ("provider", `String provider.id);
                    ("llm_tool", `String llm_tool);
                    ("llm_url", `String llm_url);
                    ("is_error", `Bool response.is_error);
                    ("response_text", `String response.text);
                    ("raw", response.raw);
                  ] in
                  Lwt.return (Ok payload)
                else
                  let prefix = Printf.sprintf "llm_%s" llm_tool in
                  let output =
                    if response.is_error then "LLM error:\n" ^ response.text else response.text
                  in
                  Lwt.return (Ok (Large_response.wrap_string_result ~prefix ~format:"text" output))))

let handle_llm_task args : (Yojson.Safe.t, string) result Lwt.t =
  let open Lwt.Syntax in
  let task = get_string "task" args in
  match task with
  | None -> Lwt.return (Error "Missing required parameter: task")
  | Some task ->
      let provider_name = get_string_any ["provider"; "llm_provider"] args in
      let llm_tool_raw = get_string_any ["tool_name"; "llm_tool"] args |> Option.value ~default:"codex" in
      let preset_name = get_string "preset" args in
      let preset_cfg = Option.bind preset_name resolve_llm_task_preset in
      let preset_unknown =
        match (preset_name, preset_cfg) with
        | (Some name, None) -> Some name
        | _ -> None
      in
      let preset_or default f =
        match preset_cfg with
        | Some cfg -> Option.value ~default (f cfg)
        | None -> default
      in
      let quality_default = preset_or "best" (fun p -> p.quality) in
      let quality = get_string_or "quality" quality_default args in
      let return_metadata = get_bool_or "return_metadata" false args in
      let max_context_chars_default =
        preset_or 120000 (fun p -> p.max_context_chars)
      in
      let max_context_chars =
        get_int "max_context_chars" args |> Option.value ~default:max_context_chars_default
      in
      let retry_on_llm_error_default =
        preset_or false (fun p -> p.retry_on_llm_error)
      in
      let retry_on_llm_error =
        get_bool_or "retry_on_llm_error" retry_on_llm_error_default args
      in
      let max_retries_default = preset_or 1 (fun p -> p.max_retries) in
      let max_retries =
        get_int "max_retries" args |> Option.value ~default:max_retries_default |> max 0
      in
      let min_context_chars_default =
        preset_or 120000 (fun p -> p.min_context_chars)
      in
      let min_context_chars =
        get_int "min_context_chars" args |> Option.value ~default:min_context_chars_default |> max 1
      in
      let retry_context_scale_default =
        preset_or 0.5 (fun p -> p.retry_context_scale)
      in
      let retry_context_scale =
        let scale = get_float_or "retry_context_scale" retry_context_scale_default args in
        if scale > 0.0 && scale < 1.0 then scale else 0.5
      in
      let auto_plugin =
        match get_bool "auto_plugin" args with
        | Some b -> b
        | None -> Option.is_some (get_string "url" args)
      in
      let include_plugin_default =
        preset_or (if auto_plugin then true else (quality <> "fast")) (fun p -> p.include_plugin)
      in
      let include_plugin =
        get_bool_or "include_plugin" include_plugin_default args
      in
      let include_variables_default =
        preset_or (quality <> "fast") (fun p -> p.include_variables)
      in
      let include_variables =
        get_bool_or "include_variables" include_variables_default args
      in
      let include_image_fills_default =
        preset_or (quality = "best") (fun p -> p.include_image_fills)
      in
      let include_image_fills =
        get_bool_or "include_image_fills" include_image_fills_default args
      in
      let llm_tool_selector_mode =
        get_string_or "llm_tool_selector_mode" "heuristic" args
      in
      let llm_tool_selector_tool =
        get_string_or "llm_tool_selector_tool" "gemini" args
      in
      let llm_tool_selector_provider = get_string "llm_tool_selector_provider" args in
      let llm_tool_selector_args = get_json "llm_tool_selector_args" args in
      let llm_tool_selector_task = get_string "llm_tool_selector_task" args in
      let llm_tool_selector_mcp_url = get_string "llm_tool_selector_mcp_url" args in
      let llm_call_policy_default =
        if quality = "best" then "require_ready" else "auto"
      in
      let llm_call_policy =
        get_string_or "llm_call_policy" llm_call_policy_default args
      in
      let llm_dry_run = get_bool_or "llm_dry_run" false args in
      let preflight_max_truncation_default =
        if quality = "best" then 0.2 else if quality = "balanced" then 0.3 else 0.4
      in
      let preflight_max_truncation =
        get_float_or "preflight_max_truncation" preflight_max_truncation_default args
      in
      let preflight_require_plugin_default =
        match preset_name with
        | Some "fidelity"
        | Some "pixel" -> true
        | _ -> (quality = "best" && include_plugin)
      in
      let preflight_require_plugin =
        get_bool_or "preflight_require_plugin" preflight_require_plugin_default args
      in
      let auto_fix_enabled_default = quality <> "fast" in
      let auto_fix_enabled =
        get_bool_or "auto_fix_enabled" auto_fix_enabled_default args
      in
      let auto_fix_max_attempts =
        get_int "auto_fix_max_attempts" args |> Option.value ~default:2 |> max 0
      in
      let critic_enabled = get_bool_or "critic_enabled" false args in
      let critic_tool = get_string_or "critic_tool" "gemini" args in
      let critic_provider = get_string "critic_provider" args in
      let critic_args = get_json "critic_args" args in
      let critic_task = get_string "critic_task" args in
      let critic_mcp_url = get_string "critic_mcp_url" args in
      let critic_min_score = get_float_or "critic_min_score" 0.7 args in
      let critic_max_retries =
        get_int "critic_max_retries" args |> Option.value ~default:0 |> max 0
      in
      let critic_retry_context_scale =
        let scale = get_float_or "critic_retry_context_scale" 0.7 args in
        if scale > 0.0 && scale < 1.0 then scale else 0.7
      in
      let plugin_context_mode_default =
        preset_or "full" (fun p -> p.plugin_context_mode)
      in
      let plugin_context_mode =
        get_string_or "plugin_context_mode" plugin_context_mode_default args
      in
      let plugin_summary_sample_size_default =
        preset_or 5 (fun p -> p.plugin_summary_sample_size)
      in
      let plugin_summary_sample_size =
        get_int "plugin_summary_sample_size" args
        |> Option.value ~default:plugin_summary_sample_size_default
      in
      let context_strategy_default =
        preset_or "raw" (fun p -> p.context_strategy)
      in
      let context_strategy =
        get_string_or "context_strategy" context_strategy_default args
      in
      let context_max_depth =
        get_int "context_max_depth" args |> Option.value ~default:6
      in
      let context_max_children =
        get_int "context_max_children" args |> Option.value ~default:200
      in
      let context_max_list_items =
        get_int "context_max_list_items" args |> Option.value ~default:200
      in
      let context_max_string =
        get_int "context_max_string" args |> Option.value ~default:2000
      in
      let context_chunk_size =
        get_int "context_chunk_size" args |> Option.value ~default:50
      in
      let chunk_select_mode_default =
        preset_or "none" (fun p -> p.chunk_select_mode)
      in
      let chunk_select_mode =
        get_string_or "chunk_select_mode" chunk_select_mode_default args
      in
      let chunk_select_limit_default =
        preset_or 4 (fun p -> p.chunk_select_limit)
      in
      let chunk_select_limit =
        get_int "chunk_select_limit" args |> Option.value ~default:chunk_select_limit_default
      in
      let chunk_select_sample_size_default =
        preset_or 6 (fun p -> p.chunk_select_sample_size)
      in
      let chunk_select_sample_size =
        get_int "chunk_select_sample_size" args |> Option.value ~default:chunk_select_sample_size_default
      in
      let chunk_select_task =
        match get_string "chunk_select_task" args with
        | Some v -> Some v
        | None ->
            (match preset_cfg with
             | Some cfg -> cfg.chunk_select_task
             | None -> None)
      in
      let chunk_select_provider =
        match get_string "chunk_select_provider" args with
        | Some v -> Some v
        | None -> provider_name
      in
      let chunk_select_llm_tool =
        match get_string "chunk_select_llm_tool" args with
        | Some v -> v
        | None ->
            if llm_tool_raw = "auto" then "codex" else llm_tool_raw
      in
      let chunk_select_llm_args = get_json "chunk_select_llm_args" args in
      let chunk_select_mcp_url = get_string "chunk_select_mcp_url" args in
      let plugin_depth_default =
        preset_or 0 (fun p -> p.plugin_depth)
      in
      let plugin_depth = get_int "plugin_depth" args |> Option.value ~default:plugin_depth_default in
      let plugin_include_geometry_default =
        preset_or false (fun p -> p.plugin_include_geometry)
      in
      let plugin_include_geometry =
        get_bool_or "plugin_include_geometry" plugin_include_geometry_default args
      in
      let plugin_timeout_ms = get_int "plugin_timeout_ms" args |> Option.value ~default:20000 in
      let (file_key, node_id) = resolve_file_key_node_id args in
      let plugin_mode =
        match get_string "plugin_mode" args with
        | Some mode -> mode
        | None -> if node_id <> None then "node" else "selection"
      in
      let token = get_string "token" args in
      let depth = get_int "depth" args in
      let geometry = get_string "geometry" args in

      let base_warnings = ref [] in
      let () =
        match preset_unknown with
        | Some name -> base_warnings := ("Unknown preset: " ^ name) :: !base_warnings
        | None -> ()
      in

      let build_context cfg : llm_task_context Lwt.t =
        let warnings = ref !base_warnings in
        let add_warning msg = warnings := msg :: !warnings in
        let dsl =
          match (cfg.file_key, cfg.node_id, cfg.token) with
          | (Some file_key, Some node_id, Some token) ->
              let cache_options =
                List.filter_map Fun.id [
                  Option.map (Printf.sprintf "depth:%d") cfg.depth;
                  Option.map (Printf.sprintf "geometry:%s") cfg.geometry;
                ]
              in
              let json_result =
                run_with_effects (fun () ->
                  match Figma_cache.get ~file_key ~node_id ~options:cache_options () with
                  | Some json -> Ok json
                  | None ->
                      match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ?depth:cfg.depth ?geometry:cfg.geometry () with
                      | Ok json ->
                          Figma_cache.set ~file_key ~node_id ~options:cache_options json;
                          Ok json
                      | Error err -> Error err)
              in
              (match json_result with
               | Error err ->
                   add_warning ("DSL fetch error: " ^ err);
                   `Null
               | Ok json ->
                   let node_data = match member "nodes" json with
                     | Some (`Assoc nodes_map) ->
                         (match List.assoc_opt node_id nodes_map with
                          | Some n -> member "document" n
                          | None -> None)
                     | _ -> None
                   in
                   (match node_data with
                    | None ->
                        add_warning ("Node not found: " ^ node_id);
                        `Null
                    | Some node ->
                        let node_str = Yojson.Safe.to_string node in
                        let dsl_str = match process_json_string ~format:"fidelity" node_str with
                          | Ok s -> s
                          | Error msg -> msg
                        in
                        `String dsl_str))
          | _ ->
              add_warning "Missing file_key/node_id/token for DSL context";
              `Null
        in
        let dsl_json =
          match dsl with
          | `String s ->
              (try Some (Yojson.Safe.from_string s)
               with _ -> None)
          | _ -> None
        in
        let compact_if_needed json =
          if cfg.context_strategy = "raw" then
            json
          else
            compact_json
              ~depth:0
              ~max_depth:cfg.context_max_depth
              ~max_children:cfg.context_max_children
              ~max_list_items:cfg.context_max_list_items
              ~max_string:cfg.context_max_string
              json
        in
        let* (dsl_context, chunk_selection) =
          match cfg.context_strategy with
          | "compact" ->
              (match dsl_json with
               | Some json -> Lwt.return (compact_if_needed json, `Null)
               | None -> Lwt.return (dsl, `Null))
          | "chunked" ->
              (match dsl_json with
               | Some json ->
                   let compacted = compact_if_needed json in
                   if cfg.chunk_select_mode = "none" then
                     let chunked =
                       chunkify_children ~chunk_size:cfg.context_chunk_size compacted
                     in
                     Lwt.return (chunked, `Null)
                   else
                     let (chunked_json, entries) =
                       build_chunk_entries
                         ~chunk_size:cfg.context_chunk_size
                         ~sample_size:cfg.chunk_select_sample_size
                         compacted
                     in
                     let chunk_total = List.length entries in
                     let effective_limit = min cfg.chunk_select_limit chunk_total in
                     let selection_fallback () =
                       if effective_limit <= 0 then []
                       else select_chunks_heuristic entries effective_limit
                     in
                     let index_json = `List (List.map chunk_entry_to_json entries) in
                     let* selected =
                       if effective_limit <= 0 then
                         Lwt.return []
                       else if cfg.chunk_select_mode = "heuristic" then
                         Lwt.return (select_chunks_heuristic entries effective_limit)
                       else if cfg.chunk_select_mode = "llm" then
                         let prompt_task =
                           match cfg.chunk_select_task with
                           | Some t -> t
                           | None -> "Select the most relevant chunks for high-fidelity output."
                         in
                         let prompt =
                           Printf.sprintf
                             "Task: %s\n\nChunk index (JSON):\n%s\n\nReturn JSON array of chunk_index values (max %d)."
                             prompt_task
                             (Yojson.Safe.pretty_to_string (`Assoc [
                                ("chunk_total", `Int chunk_total);
                                ("chunk_size", `Int cfg.context_chunk_size);
                                ("chunks", index_json);
                              ]))
                             effective_limit
                         in
                        let llm_args_fields =
                          match cfg.chunk_select_llm_args with
                          | None -> Ok []
                          | Some (`Assoc fields) -> Ok fields
                          | Some _ -> Error "chunk_select_llm_args must be an object"
                        in
                        match llm_args_fields with
                        | Error msg ->
                            add_warning msg;
                            Lwt.return (selection_fallback ())
                        | Ok fields ->
                            let fields = set_field "prompt" (`String prompt) fields in
                            let fields =
                              if has_field "response_format" fields then fields
                              else add_if_missing "response_format" (`String "compact") fields
                            in
                            let fields = add_if_missing "stream" (`Bool false) fields in
                            match Llm_provider.resolve ?provider:cfg.chunk_select_provider () with
                            | Error msg ->
                                add_warning msg;
                                Lwt.return (selection_fallback ())
                            | Ok provider ->
                                let llm_url =
                                  Option.value cfg.chunk_select_mcp_url ~default:provider.default_url
                                in
                                let* llm_result =
                                  call_llm_tool ~provider ~url:llm_url ~name:cfg.chunk_select_llm_tool ~arguments:(`Assoc fields)
                                in
                                match llm_result with
                                | Error err ->
                                    add_warning err;
                                    Lwt.return (selection_fallback ())
                                | Ok response ->
                                    let indices =
                                      parse_chunk_indices ~chunk_total response.text
                                      |> take_n effective_limit
                                    in
                                    if indices = [] then
                                      Lwt.return (selection_fallback ())
                                    else
                                      Lwt.return indices
                       else
                         Lwt.return (selection_fallback ())
                     in
                     let selected =
                       selected |> List.filter (fun v -> v >= 1 && v <= chunk_total)
                     in
                     let selected =
                       if selected = [] then selection_fallback () else selected
                     in
                     let selection_meta =
                       `Assoc [
                         ("mode", `String cfg.chunk_select_mode);
                         ("selected", `List (List.map (fun v -> `Int v) selected));
                         ("chunk_total", `Int chunk_total);
                         ("chunk_size", `Int cfg.context_chunk_size);
                       ]
                     in
                     let selected_json =
                       if selected = [] then chunked_json
                       else select_chunked_json ~selected chunked_json
                     in
                     Lwt.return (selected_json, selection_meta)
               | None ->
                   (match dsl with
                    | `String s ->
                        let chunked =
                          chunkify_text ~chunk_size:cfg.context_chunk_size s
                        in
                        let chunk_total =
                          match chunked with
                          | `Assoc fields ->
                              (match List.assoc_opt "chunk_total" fields with
                               | Some (`Int n) -> n
                               | _ -> 0)
                          | _ -> 0
                        in
                        let selection_meta =
                          `Assoc [
                            ("mode", `String "fallback_text");
                            ("chunk_total", `Int chunk_total);
                            ("chunk_size", `Int cfg.context_chunk_size);
                          ]
                        in
                        Lwt.return (chunked, selection_meta)
                    | _ -> Lwt.return (dsl, `Null)))
          | _ -> Lwt.return (dsl, `Null)
        in

        let variables, variables_source =
          if cfg.include_variables then
            match (cfg.file_key, cfg.token) with
            | (Some file_key, Some token) ->
                let vars =
                  run_with_effects (fun () -> fetch_variables_cached ~file_key ~token)
                in
                (match vars with
                 | Ok (vars_json, source) -> (resolve_variables vars_json, source)
                 | Error err ->
                     add_warning ("Variables error: " ^ err);
                     (`Null, `String "error"))
            | _ ->
                add_warning "Missing file_key/token for variables context";
                (`Null, `Null)
          else
            (`Null, `Null)
        in

        let image_fills =
          if cfg.include_image_fills then
            match (cfg.file_key, cfg.token) with
            | (Some file_key, Some token) ->
                let images =
                  run_with_effects (fun () ->
                    match Figma_effects.Perform.get_file_images ~token ~file_key () with
                    | Ok img_json ->
                        (match member "images" img_json with
                         | Some (`Assoc _ as m) -> m
                         | _ -> `Null)
                    | Error err -> `Assoc [("error", `String err)])
                in
                images
            | _ ->
                add_warning "Missing file_key/token for image fills context";
                `Null
          else
            `Null
        in
        let variables = compact_if_needed variables in
        let image_fills = compact_if_needed image_fills in

        let plugin_snapshot_raw =
          if cfg.include_plugin then
            let resolve_plugin_channel () =
              match cfg.plugin_channel_id with
              | Some id -> Ok id
              | None ->
                  (match Figma_plugin_bridge.get_default_channel () with
                   | Some id -> Ok id
                   | None -> Error "Missing plugin_channel_id. Run figma_plugin_connect or figma_plugin_use_channel.")
            in
            (match resolve_plugin_channel () with
             | Error msg ->
                 add_warning ("Plugin channel error: " ^ msg);
                 `Assoc [("error", `String msg)]
             | Ok channel_id ->
                 let use_selection =
                   cfg.plugin_mode = "selection" || cfg.node_id = None
                 in
                 if use_selection then
                   let payload = `Assoc [("depth", `Int cfg.plugin_depth)] in
                   let command_id =
                     Figma_plugin_bridge.enqueue_command ~channel_id ~name:"read_selection" ~payload
                   in
                   (match plugin_wait ~channel_id ~command_id ~timeout_ms:cfg.plugin_timeout_ms with
                    | Error err ->
                        add_warning ("Plugin selection error: " ^ err);
                        `Assoc [("error", `String err)]
                    | Ok result ->
                        `Assoc [
                          ("mode", `String "selection");
                          ("channel_id", `String channel_id);
                          ("command_id", `String command_id);
                          ("ok", `Bool result.ok);
                          ("payload", result.payload);
                        ])
                 else
                   match cfg.node_id with
                   | None ->
                       `Assoc [("error", `String "node_id required for plugin_mode=node")]
                   | Some node_id ->
                       let payload = `Assoc [
                         ("node_id", `String node_id);
                         ("depth", `Int cfg.plugin_depth);
                         ("include_geometry", `Bool cfg.plugin_include_geometry);
                       ] in
                       let command_id =
                         Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_node" ~payload
                       in
                       (match plugin_wait ~channel_id ~command_id ~timeout_ms:cfg.plugin_timeout_ms with
                        | Error err ->
                            add_warning ("Plugin node error: " ^ err);
                            `Assoc [("error", `String err)]
                        | Ok result ->
                            `Assoc [
                              ("mode", `String "node");
                              ("channel_id", `String channel_id);
                              ("command_id", `String command_id);
                              ("ok", `Bool result.ok);
                              ("payload", result.payload);
                              ("plugin_depth", `Int cfg.plugin_depth);
                            ]))
          else
            `Null
        in
        let plugin_snapshot = compact_if_needed plugin_snapshot_raw in
        let plugin_summary =
          if cfg.include_plugin && (cfg.plugin_context_mode = "summary" || cfg.plugin_context_mode = "both") then
            let payload =
              match plugin_snapshot_raw with
              | `Assoc fields ->
                  (match List.assoc_opt "payload" fields with
                   | Some v -> v
                   | None -> `Null)
              | _ -> `Null
            in
            summarize_plugin_payload ~sample_size:cfg.plugin_summary_sample_size payload
          else
            `Null
        in
        let plugin_snapshot =
          if cfg.plugin_context_mode = "summary" then `Null else plugin_snapshot
        in

        let context_fields = [
          ("task", `String task);
          ("quality", `String quality);
          ("file_key", (match cfg.file_key with Some v -> `String v | None -> `Null));
          ("node_id", (match cfg.node_id with Some v -> `String v | None -> `Null));
          ("dsl", dsl_context);
          ("chunk_selection", chunk_selection);
          ("variables", variables);
          ("variables_source", variables_source);
          ("image_fills", image_fills);
          ("plugin_snapshot", plugin_snapshot);
          ("plugin_summary", plugin_summary);
        ] in
        let context_fields =
          if !warnings = [] then context_fields
          else ("warnings", `List (List.map (fun s -> `String s) (List.rev !warnings))) :: context_fields
        in
        let context_json = `Assoc context_fields in
        let context_str_full = Yojson.Safe.pretty_to_string context_json in
        Lwt.return {
          dsl_context;
          chunk_selection;
          variables;
          variables_source;
          image_fills;
          plugin_snapshot_raw;
          plugin_snapshot;
          plugin_summary;
          warnings = List.rev !warnings;
          context_json;
          context_str_full;
        }
      in

      let evaluate_preflight cfg ctx =
        let normalize_max_context_chars v = if v > 0 then v else 120000 in
        let max_chars = normalize_max_context_chars cfg.max_context_chars in
        let context_size_bytes = String.length ctx.context_str_full in
        let truncation_ratio =
          if context_size_bytes <= max_chars then 0.0
          else 1.0 -. (float_of_int max_chars /. float_of_int context_size_bytes)
        in
        let dsl_present =
          match ctx.dsl_context with
          | `Null -> false
          | `String s -> String.trim s <> ""
          | _ -> true
        in
        let (chunk_selected_count, chunk_total) =
          parse_chunk_selection_info ctx.chunk_selection
        in
        let chunk_ready =
          if cfg.context_strategy <> "chunked" then true
          else if cfg.chunk_select_mode = "none" then true
          else chunk_selected_count > 0
        in
        let issues = ref [] in
        let add_issue issue = issues := issue :: !issues in
        let () =
          if not dsl_present then
            add_issue (make_issue "dsl_missing" "DSL 컨텍스트가 비어있습니다."
              ~suggestion:"file_key/node_id/token 또는 url을 확인하세요.")
        in
        let () =
          if not chunk_ready then
            add_issue (make_issue "chunk_selection_empty" "청크 선택 결과가 없습니다."
              ~suggestion:"chunk_select_mode=heuristic 또는 llm으로 재시도하세요.")
        in
        let () =
          if cfg.preflight_require_plugin && not (plugin_ok ctx.plugin_snapshot_raw) then
            add_issue (make_issue "plugin_missing" "플러그인 스냅샷이 준비되지 않았습니다."
              ~suggestion:"plugin_channel_id/timeout/depth를 확인하세요.")
        in
        let () =
          if truncation_ratio > cfg.preflight_max_truncation then
            add_issue (make_issue "context_truncated" "컨텍스트가 과도하게 잘렸습니다."
              ~suggestion:"chunked 전략/청크 선택을 사용하세요.")
        in
        let issues = List.rev !issues in
        let ready = issues = [] in
        let preflight_json =
          `Assoc [
            ("ready", `Bool ready);
            ("context_size_bytes", `Int context_size_bytes);
            ("max_context_chars", `Int max_chars);
            ("truncation_ratio", `Float truncation_ratio);
            ("dsl_present", `Bool dsl_present);
            ("plugin_ok", `Bool (plugin_ok ctx.plugin_snapshot_raw));
            ("chunk_selected_count", `Int chunk_selected_count);
            ("chunk_total", `Int chunk_total);
            ("issues", `List (List.map issue_to_json issues));
            ("warnings", `List (List.map (fun s -> `String s) ctx.warnings));
          ]
        in
        (ready, preflight_json, issues, truncation_ratio, context_size_bytes, chunk_selected_count, chunk_total, dsl_present, plugin_ok ctx.plugin_snapshot_raw)
      in

      let apply_auto_fix cfg issues =
        let cfg_ref = ref cfg in
        let actions = ref [] in
        let changed = ref false in
        let add_action msg = actions := msg :: !actions in
        let update_config action f =
          let next = f !cfg_ref in
          if next <> !cfg_ref then begin
            cfg_ref := next;
            add_action action;
            changed := true;
          end
        in
        let has_issue code =
          List.exists (fun issue -> issue.code = code) issues
        in
        if has_issue "context_truncated" then begin
          update_config "auto_fix:chunked_context" (fun cfg ->
            let cfg =
              if cfg.context_strategy <> "chunked" then
                { cfg with context_strategy = "chunked" }
              else
                cfg
            in
            let cfg =
              if cfg.chunk_select_mode = "none" then
                { cfg with chunk_select_mode = "heuristic" }
              else
                cfg
            in
            if cfg.chunk_select_limit <= 0 then
              { cfg with chunk_select_limit = 4 }
            else
              cfg)
        end;
        if has_issue "chunk_selection_empty" then begin
          update_config "auto_fix:chunk_selection" (fun cfg ->
            let cfg =
              if cfg.chunk_select_mode = "none" then
                { cfg with chunk_select_mode = "heuristic" }
              else
                cfg
            in
            if cfg.chunk_select_limit <= 0 then
              { cfg with chunk_select_limit = 4 }
            else
              cfg)
        end;
        if has_issue "plugin_missing" then begin
          update_config "auto_fix:plugin_retry" (fun cfg ->
            let cfg =
              if not cfg.include_plugin then
                { cfg with include_plugin = true }
              else
                cfg
            in
            let cfg =
              if cfg.plugin_depth < 1 then
                { cfg with plugin_depth = 1 }
              else
                cfg
            in
            if cfg.plugin_timeout_ms < 30000 then
              { cfg with plugin_timeout_ms = 30000 }
            else
              cfg)
        end;
        (!cfg_ref, List.rev !actions, !changed)
      in

      let select_llm_tool cfg preflight_json =
        let context_size_bytes =
          match preflight_json with
          | `Assoc fields -> (
              match List.assoc_opt "context_size_bytes" fields with
              | Some (`Int n) -> n
              | _ -> 0)
          | _ -> 0
        in
        let heuristic_tool, heuristic_reason =
          if cfg.quality = "fast" then
            ("gemini", "quality=fast")
          else if cfg.include_plugin || cfg.include_image_fills || context_size_bytes > 400000 then
            ("gemini", "large_context_or_plugin")
          else
            ("codex", "default")
        in
        if cfg.llm_tool_raw <> "auto" then
          Lwt.return { tool = cfg.llm_tool_raw; mode = "fixed"; reason = None }
        else if cfg.llm_tool_selector_mode = "llm" then
          let selector_tool =
            if cfg.llm_tool_selector_tool = "auto" then "gemini" else cfg.llm_tool_selector_tool
          in
          let prompt_task =
            match cfg.llm_tool_selector_task with
            | Some t -> t
            | None -> "Select the best LLM tool for this task. Respond with JSON: {\"tool\":\"codex|claude-cli|gemini|ollama\",\"reason\":\"...\"}."
          in
          let prompt =
            Printf.sprintf
              "Task: %s\n\nPreflight (JSON):\n%s"
              prompt_task
              (Yojson.Safe.pretty_to_string preflight_json)
          in
          let selector_args_fields =
            match cfg.llm_tool_selector_args with
            | None -> Ok []
            | Some (`Assoc fields) -> Ok fields
            | Some _ -> Error "llm_tool_selector_args must be an object"
          in
          match selector_args_fields with
          | Error _ ->
              Lwt.return { tool = heuristic_tool; mode = "heuristic"; reason = Some heuristic_reason }
          | Ok fields ->
              let fields = set_field "prompt" (`String prompt) fields in
              let fields =
                if has_field "response_format" fields then fields
                else add_if_missing "response_format" (`String "compact") fields
              in
              let fields = add_if_missing "stream" (`Bool false) fields in
              match Llm_provider.resolve ?provider:cfg.llm_tool_selector_provider () with
              | Error _ ->
                  Lwt.return { tool = heuristic_tool; mode = "heuristic"; reason = Some heuristic_reason }
              | Ok provider ->
                  let llm_url =
                    Option.value cfg.llm_tool_selector_mcp_url ~default:provider.default_url
                  in
                  let* llm_result =
                    call_llm_tool ~provider ~url:llm_url ~name:selector_tool ~arguments:(`Assoc fields)
                  in
                  (match llm_result with
                   | Error _ ->
                       Lwt.return { tool = heuristic_tool; mode = "heuristic"; reason = Some heuristic_reason }
                   | Ok response ->
                       let choice = parse_tool_choice response.text in
                       (match choice with
                        | Some tool ->
                            Lwt.return { tool; mode = "llm"; reason = Some (strip_llm_prefix response.text |> String.trim) }
                        | None ->
                            Lwt.return { tool = heuristic_tool; mode = "heuristic"; reason = Some heuristic_reason }))
        else
          Lwt.return { tool = heuristic_tool; mode = "heuristic"; reason = Some heuristic_reason }
      in

      let llm_args_fields =
        match get_json "llm_args" args with
        | None -> Ok []
        | Some (`Assoc fields) -> Ok fields
        | Some _ -> Error "llm_args must be an object"
      in
      let base_fields_of llm_args_fields =
        let fields =
          if quality = "fast" then add_if_missing "budget_mode" (`Bool true) llm_args_fields
          else llm_args_fields
        in
        let fields =
          if has_field "response_format" fields then fields
          else if quality = "fast" then add_if_missing "response_format" (`String "compact") fields
          else add_if_missing "response_format" (`String "verbose") fields
        in
        add_if_missing "stream" (`Bool false) fields
      in

      let run_llm_with_critic cfg ctx tool_selection preflight_json auto_fix_actions =
        match llm_args_fields with
        | Error msg -> Lwt.return (Error msg)
        | Ok fields ->
            let base_fields = base_fields_of fields in
            match Llm_provider.resolve ?provider:cfg.provider_name () with
            | Error msg -> Lwt.return (Error msg)
            | Ok provider ->
                let llm_url =
                  get_string_any ["mcp_url"; "llm_url"] args
                  |> Option.value ~default:provider.default_url
                in
                let normalize_max_context_chars v = if v > 0 then v else 120000 in
                let make_prompt max_chars =
                  let max_chars = normalize_max_context_chars max_chars in
                  let (context_head, truncated) =
                    truncate_utf8 ~max_bytes:max_chars ctx.context_str_full
                  in
                  let context_str =
                    if truncated then context_head ^ "
...TRUNCATED..." else context_head
                  in
                  let base =
                    Printf.sprintf "Task:
%s

Context (JSON):
%s" task context_str
                  in
                  let prompt =
                    if truncated then
                      base ^ Printf.sprintf "

Note: context truncated to %d chars." max_chars
                    else
                      base
                  in
                  (prompt, truncated, max_chars)
                in
                let rec call_llm attempt max_chars =
                  let (prompt, truncated, max_chars) = make_prompt max_chars in
                  let fields = set_field "prompt" (`String prompt) base_fields in
                  let arguments = `Assoc fields in
                  let* llm_result = call_llm_tool ~provider ~url:llm_url ~name:tool_selection.tool ~arguments in
                  match llm_result with
                  | Error err -> Lwt.return (Error err)
                  | Ok response ->
                      if response.is_error && cfg.retry_on_llm_error && attempt < cfg.max_retries then
                        let scaled =
                          int_of_float (float_of_int max_chars *. cfg.retry_context_scale)
                        in
                        let next_max = max cfg.min_context_chars scaled in
                        if next_max < max_chars then
                          call_llm (attempt + 1) next_max
                        else
                          Lwt.return (Ok (response, truncated, attempt + 1, max_chars))
                      else
                        Lwt.return (Ok (response, truncated, attempt + 1, max_chars))
                in
                let rec run_with_critic attempt max_chars =
                  let* llm_result = call_llm 0 max_chars in
                  match llm_result with
                  | Error err -> Lwt.return (Error err)
                  | Ok (response, truncated, attempts, final_max) ->
                      if not cfg.critic_enabled then
                        Lwt.return (Ok (response, truncated, attempts, final_max, None))
                      else
                        let response_preview =
                          let (head, _) = truncate_utf8 ~max_bytes:4000 response.text in
                          head
                        in
                        let critic_prompt_task =
                          match cfg.critic_task with
                          | Some t -> t
                          | None -> "Evaluate output fidelity to the provided Figma context. Return JSON: {\"score\":0-1,\"decision\":\"accept|retry\",\"notes\":\"...\"}."
                        in
                        let critic_prompt =
                          Printf.sprintf
                            "Task: %s

Preflight (JSON):
%s

Output:
%s"
                            critic_prompt_task
                            (Yojson.Safe.pretty_to_string preflight_json)
                            response_preview
                        in
                        let critic_args_fields =
                          match cfg.critic_args with
                          | None -> Ok []
                          | Some (`Assoc fields) -> Ok fields
                          | Some _ -> Error "critic_args must be an object"
                        in
                        match critic_args_fields with
                        | Error _ ->
                            Lwt.return (Ok (response, truncated, attempts, final_max, None))
                        | Ok fields ->
                            let fields = set_field "prompt" (`String critic_prompt) fields in
                            let fields =
                              if has_field "response_format" fields then fields
                              else add_if_missing "response_format" (`String "compact") fields
                            in
                            let fields = add_if_missing "stream" (`Bool false) fields in
                            match Llm_provider.resolve ?provider:cfg.critic_provider () with
                            | Error _ ->
                                Lwt.return (Ok (response, truncated, attempts, final_max, None))
                            | Ok critic_provider ->
                                let critic_url =
                                  Option.value cfg.critic_mcp_url ~default:critic_provider.default_url
                                in
                                let* critic_result =
                                  call_llm_tool ~provider:critic_provider ~url:critic_url ~name:cfg.critic_tool ~arguments:(`Assoc fields)
                                in
                                (match critic_result with
                                 | Error _ ->
                                     Lwt.return (Ok (response, truncated, attempts, final_max, None))
                                 | Ok critic_response ->
                                     let critic_text = strip_llm_prefix critic_response.text |> String.trim in
                                     let critic_json =
                                       try Some (Yojson.Safe.from_string critic_text)
                                       with _ -> None
                                     in
                                     let score, decision =
                                       match critic_json with
                                       | Some (`Assoc fields) ->
                                           let score =
                                             match List.assoc_opt "score" fields with
                                             | Some (`Float f) -> f
                                             | Some (`Int i) -> float_of_int i
                                             | _ -> 1.0
                                           in
                                           let decision =
                                             match List.assoc_opt "decision" fields with
                                             | Some (`String s) -> String.lowercase_ascii s
                                             | _ -> "accept"
                                           in
                                           (score, decision)
                                       | _ -> (1.0, "accept")
                                     in
                                     if decision = "retry" && score < cfg.critic_min_score && attempt < cfg.critic_max_retries then
                                       let scaled =
                                         int_of_float (float_of_int final_max *. cfg.critic_retry_context_scale)
                                       in
                                       let next_max = max cfg.min_context_chars scaled in
                                       if next_max < final_max then
                                         run_with_critic (attempt + 1) next_max
                                       else
                                         Lwt.return (Ok (response, truncated, attempts, final_max, Some (critic_text, score, decision)))
                                     else
                                       Lwt.return (Ok (response, truncated, attempts, final_max, Some (critic_text, score, decision))))
                in
                let* llm_result = run_with_critic 0 cfg.max_context_chars in
                match llm_result with
                | Error err -> Lwt.return (Error err)
                | Ok (response, truncated, attempts, final_max, critic_result) ->
                    if cfg.return_metadata then
                      let payload = `Assoc [
                        ("provider", `String provider.id);
                        ("llm_tool", `String tool_selection.tool);
                        ("llm_url", `String llm_url);
                        ("quality", `String cfg.quality);
                        ("context_truncated", `Bool truncated);
                        ("context_max_chars", `Int final_max);
                        ("attempts", `Int attempts);
                        ("retry_on_llm_error", `Bool cfg.retry_on_llm_error);
                        ("max_retries", `Int cfg.max_retries);
                        ("is_error", `Bool response.is_error);
                        ("response_text", `String response.text);
                        ("raw", response.raw);
                        ("preflight", preflight_json);
                        ("tool_selection", `Assoc [
                            ("tool", `String tool_selection.tool);
                            ("mode", `String tool_selection.mode);
                            ("reason", (match tool_selection.reason with Some r -> `String r | None -> `Null));
                          ]);
                        ("auto_fix_actions", `List (List.map (fun s -> `String s) auto_fix_actions));
                        ("critic", (match critic_result with
                          | None -> `Null
                          | Some (text, score, decision) ->
                              `Assoc [
                                ("score", `Float score);
                                ("decision", `String decision);
                                ("raw", `String text);
                              ]));
                      ] in
                      Lwt.return (Ok payload)
                    else
                      let prefix = Printf.sprintf "llm_task_%s" tool_selection.tool in
                      let output =
                        if response.is_error then "LLM error:
" ^ response.text else response.text
                      in
                      Lwt.return (Ok (Large_response.wrap_string_result ~prefix ~format:"text" output))
      in

      let cfg0 = {
        quality;
        provider_name;
        llm_tool_raw;
        llm_tool_selector_mode;
        llm_tool_selector_tool;
        llm_tool_selector_provider;
        llm_tool_selector_args;
        llm_tool_selector_task;
        llm_tool_selector_mcp_url;
        llm_call_policy;
        llm_dry_run;
        preflight_max_truncation;
        preflight_require_plugin;
        auto_fix_enabled;
        auto_fix_max_attempts;
        critic_enabled;
        critic_tool;
        critic_provider;
        critic_args;
        critic_task;
        critic_mcp_url;
        critic_min_score;
        critic_max_retries;
        critic_retry_context_scale;
        file_key;
        node_id;
        token;
        depth;
        geometry;
        include_variables;
        include_image_fills;
        include_plugin;
        auto_plugin;
        plugin_context_mode;
        plugin_summary_sample_size;
        plugin_depth;
        plugin_include_geometry;
        plugin_timeout_ms;
        plugin_mode;
        plugin_channel_id = get_string "plugin_channel_id" args;
        context_strategy;
        context_max_depth;
        context_max_children;
        context_max_list_items;
        context_max_string;
        context_chunk_size;
        chunk_select_mode;
        chunk_select_limit;
        chunk_select_sample_size;
        chunk_select_task;
        chunk_select_provider;
        chunk_select_llm_tool;
        chunk_select_llm_args;
        chunk_select_mcp_url;
        max_context_chars;
        retry_on_llm_error;
        max_retries;
        min_context_chars;
        retry_context_scale;
        return_metadata;
      } in

      let rec run_attempt attempt cfg =
        let* ctx = build_context cfg in
        let (ready, preflight_json, issues, _ratio, _size, _sel_count, _chunk_total, _dsl_present, _plugin_ok) =
          evaluate_preflight cfg ctx
        in
        let make_preflight_payload actions =
          `Assoc [
            ("preflight", preflight_json);
            ("llm_call_policy", `String cfg.llm_call_policy);
            ("llm_dry_run", `Bool cfg.llm_dry_run);
            ("auto_fix_attempt", `Int attempt);
            ("auto_fix_actions", `List (List.map (fun s -> `String s) actions));
          ]
        in
        if cfg.llm_dry_run || cfg.llm_call_policy = "skip" then
          Lwt.return (Ok (Large_response.wrap_json_result ~prefix:"llm_task_preflight" ~format:"json" (make_preflight_payload [])))
        else if not ready then
          if cfg.auto_fix_enabled && attempt < cfg.auto_fix_max_attempts then
            let (next_cfg, actions, changed) = apply_auto_fix cfg issues in
            if changed then
              run_attempt (attempt + 1) next_cfg
            else if cfg.llm_call_policy = "force" then
              let* tool_selection = select_llm_tool cfg preflight_json in
              run_llm_with_critic cfg ctx tool_selection preflight_json actions
            else
              Lwt.return (Ok (Large_response.wrap_json_result ~prefix:"llm_task_preflight" ~format:"json" (make_preflight_payload actions)))
          else if cfg.llm_call_policy = "force" then
            let* tool_selection = select_llm_tool cfg preflight_json in
            run_llm_with_critic cfg ctx tool_selection preflight_json []
          else
            Lwt.return (Ok (Large_response.wrap_json_result ~prefix:"llm_task_preflight" ~format:"json" (make_preflight_payload [])))
        else
          let* tool_selection = select_llm_tool cfg preflight_json in
          run_llm_with_critic cfg ctx tool_selection preflight_json []
      in
      run_attempt 0 cfg0

(** ============== Phase 1: 탐색 도구 핸들러 ============== *)

(** figma_parse_url 핸들러 - 로컬 URL 파싱 *)
let handle_parse_url args : (Yojson.Safe.t, string) result =
  match get_string "url" args with
  | None -> Error "Missing required parameter: url"
  | Some url ->
      let info = Figma_api.parse_figma_url url in
      let result = sprintf "Parsed URL:\n- team_id: %s\n- project_id: %s\n- file_key: %s\n- node_id: %s"
        (Option.value ~default:"(none)" info.team_id)
        (Option.value ~default:"(none)" info.project_id)
        (Option.value ~default:"(none)" info.file_key)
        (Option.value ~default:"(none)" info.node_id)
      in
      Ok (make_text_content result)

(** figma_get_me 핸들러 - 현재 사용자 정보 *)
let handle_get_me args : (Yojson.Safe.t, string) result =
  match get_string "token" args with
  | None -> Error "Missing required parameter: token"
  | Some token ->
      (match Figma_effects.Perform.get_me ~token with
       | Ok json ->
           let id = get_string "id" json in
           let email = get_string "email" json in
           let handle = get_string "handle" json in
           let result = sprintf "User Info:\n- id: %s\n- email: %s\n- handle: %s"
             (Option.value ~default:"(unknown)" id)
             (Option.value ~default:"(unknown)" email)
             (Option.value ~default:"(unknown)" handle)
           in
           Ok (make_text_content result)
       | Error err -> Error err)

(** figma_list_projects 핸들러 - 팀의 프로젝트 목록 *)
let handle_list_projects args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = get_string "token" args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_projects ~token ~team_id with
       | Ok json ->
           let projects = match member "projects" json with
             | Some (`List lst) -> lst
             | _ -> []
           in
           let project_list = List.filter_map (fun p ->
             let id = get_string "id" p in
             let name = get_string "name" p in
             match (id, name) with
             | (Some id, Some name) -> Some (sprintf "- %s (id: %s)" name id)
             | _ -> None
           ) projects in
           let result = sprintf "Found %d projects:\n%s"
             (List.length project_list)
             (String.concat "\n" project_list)
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_list_files 핸들러 - 프로젝트의 파일 목록 *)
let handle_list_files args : (Yojson.Safe.t, string) result =
  let project_id = get_string "project_id" args in
  let token = get_string "token" args in

  match (project_id, token) with
  | (Some project_id, Some token) ->
      (match Figma_effects.Perform.get_project_files ~token ~project_id with
       | Ok json ->
           let files = match member "files" json with
             | Some (`List lst) -> lst
             | _ -> []
           in
           let file_list = List.filter_map (fun f ->
             let key = get_string "key" f in
             let name = get_string "name" f in
             match (key, name) with
             | (Some key, Some name) -> Some (sprintf "- %s (key: %s)" name key)
             | _ -> None
           ) files in
           let result = sprintf "Found %d files:\n%s"
             (List.length file_list)
             (String.concat "\n" file_list)
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: project_id, token"

(** figma_get_variables 핸들러 - 디자인 토큰/변수 *)
let handle_get_variables args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let format = get_string_or "format" "summary" args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      let json_result =
        match fetch_variables_cached ~file_key ~token with
        | Ok (json, _) -> Ok json
        | Error err -> Error err
      in
      (match json_result with
       | Ok json ->
           (match format with
            | "raw" ->
                Ok (make_text_content (Yojson.Safe.pretty_to_string json))
            | "resolved" ->
                let resolved = resolve_variables json in
                Ok (make_text_content (Yojson.Safe.pretty_to_string resolved))
            | _ ->
                (* 변수 컬렉션과 변수 목록 추출 *)
                let collections = match member "meta" json with
                  | Some meta -> (match member "variableCollections" meta with
                      | Some (`Assoc lst) -> List.length lst
                      | _ -> 0)
                  | _ -> 0
                in
                let variables = match member "meta" json with
                  | Some meta -> (match member "variables" meta with
                      | Some (`Assoc lst) -> List.length lst
                      | _ -> 0)
                  | _ -> 0
                in
                let result = sprintf "Design Tokens Summary:\n- Collections: %d\n- Variables: %d"
                  collections variables
                in
                Ok (make_text_content result))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** ============== Phase 2: 고급 쿼리 핸들러 ============== *)

(** figma_query 핸들러 - 노드 필터링 *)
let handle_query args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let node_id = get_string "node_id" args in
  let type_filter = get_string "type" args in
  let width_min = get_float "width_min" args in
  let width_max = get_float "width_max" args in
  let height_min = get_float "height_min" args in
  let height_max = get_float "height_max" args in
  let color = get_string "color" args in
  let name = get_string "name" args in
  let depth = get_float "depth" args |> Option.map int_of_float in
  let limit = get_float "limit" args |> Option.map int_of_float in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (* 파일 또는 특정 노드 가져오기 - Effect 사용 *)
      let json_result = match node_id with
        | Some nid -> Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[nid] ()
        | None -> Figma_effects.Perform.get_file ~token ~file_key ()
      in
      (match json_result with
       | Ok json ->
           (* JSON에서 document 추출 *)
           let doc_json = match node_id with
             | Some nid ->
                 (match member "nodes" json with
                  | Some (`Assoc nodes) ->
                      (match List.assoc_opt nid nodes with
                       | Some node -> member "document" node
                       | None -> None)
                  | _ -> None)
             | None -> Figma_api.extract_document json
           in
           (match doc_json with
            | Some doc_json ->
                let doc_str = Yojson.Safe.to_string doc_json in
                (match Figma_parser.parse_json_string doc_str with
                 | Some root ->
                     (* 쿼리 빌드 *)
                     let q = Figma_query.empty_query in
                     let q = match type_filter with
                       | Some t -> Figma_query.with_type (String.split_on_char ',' t |> List.map String.trim) q
                       | None -> q
                     in
                     let q = match width_min with Some w -> Figma_query.with_width_min w q | None -> q in
                     let q = match width_max with Some w -> Figma_query.with_width_max w q | None -> q in
                     let q = match height_min with Some h -> Figma_query.with_height_min h q | None -> q in
                     let q = match height_max with Some h -> Figma_query.with_height_max h q | None -> q in
                     let q = match color with Some c -> Figma_query.with_color c q | None -> q in
                     let q = match name with Some n -> Figma_query.with_name n q | None -> q in
                     let q = match depth with Some d -> Figma_query.with_depth d q | None -> q in
                     let q = match limit with Some l -> Figma_query.with_limit l q | None -> q in

                     (* 쿼리 실행 *)
                     let results = Figma_query.execute_query q root in
                     let result_str = Figma_query.results_to_string results in
                     Ok (make_text_content result_str)
                 | None -> Error "Failed to parse document")
            | None -> Error "Document not found")
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_search 핸들러 - 텍스트/이름 검색 *)
let handle_search args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let query = get_string "query" args in
  let search_in = get_string_or "search_in" "both" args in
  let limit = get_float "limit" args |> Option.map int_of_float |> Option.value ~default:20 in

  match (file_key, token, query) with
  | (Some file_key, Some token, Some query) ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           (match Figma_api.extract_document json with
            | Some doc_json ->
                let doc_str = Yojson.Safe.to_string doc_json in
                (match Figma_parser.parse_json_string doc_str with
                 | Some root ->
                     (* 모든 노드 수집 *)
                     let all_nodes = Figma_query.collect_nodes ~max_depth:None root in
                     let query_lower = String.lowercase_ascii query in

                     (* 검색 함수 *)
                     let matches_name node =
                       let name_lower = String.lowercase_ascii node.Figma_types.name in
                       try
                         let _ = Str.search_forward (Str.regexp_string query_lower) name_lower 0 in
                         true
                       with Not_found -> false
                     in
                     let matches_text node =
                       match node.Figma_types.characters with
                       | Some chars ->
                           let chars_lower = String.lowercase_ascii chars in
                           (try
                              let _ = Str.search_forward (Str.regexp_string query_lower) chars_lower 0 in
                              true
                            with Not_found -> false)
                       | None -> false
                     in
                     let matches node = match search_in with
                       | "name" -> matches_name node
                       | "text" -> matches_text node
                       | _ -> matches_name node || matches_text node
                     in

                     (* 필터링 *)
                     let results = List.filter matches all_nodes in
                     let results = List.filteri (fun i _ -> i < limit) results in
                     let result_str = Figma_query.results_to_string results in
                     Ok (make_text_content result_str)
                 | None -> Error "Failed to parse document")
            | None -> Error "Document not found")
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token, query"

(** figma_compare 핸들러 *)
let handle_compare args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let node_a_id = get_string "node_a_id" args in
  let node_b_id = get_string "node_b_id" args in
  let mode = get_string_or "mode" "single" args in
  let web_prefix = get_string_or "web_prefix" "Web" args in
  let mobile_prefix = get_string_or "mobile_prefix" "Mobile" args in

  match file_key, token with
  | Some file_key, Some token ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok file_data ->
           (match Yojson.Safe.Util.member "document" file_data with
            | `Null -> Error "Document not found"
            | doc_json ->
                (match Figma_parser.parse_node doc_json with
                 | Some root ->
                     let all_nodes = Figma_query.collect_nodes ~max_depth:None root in

                     if mode = "batch" then begin
                       (* Batch 모드: Web/Mobile 이름 매칭 *)
                       let web_nodes = List.filter (fun n ->
                         String.length n.Figma_types.name >= String.length web_prefix &&
                         String.sub (String.lowercase_ascii n.Figma_types.name) 0 (String.length web_prefix) =
                         String.lowercase_ascii web_prefix
                       ) all_nodes in
                       let mobile_nodes = List.filter (fun n ->
                         String.length n.Figma_types.name >= String.length mobile_prefix &&
                         String.sub (String.lowercase_ascii n.Figma_types.name) 0 (String.length mobile_prefix) =
                         String.lowercase_ascii mobile_prefix
                       ) all_nodes in

                       let (results, total, avg_sim, critical, major) =
                         Figma_compare.compare_web_mobile ~web_nodes ~mobile_nodes
                       in

                       let summary = Printf.sprintf
                         "=== Web/Mobile 일관성 검사 결과 ===\n매칭된 쌍: %d개\n평균 유사도: %.0f%%\nCritical 차이: %d개\nMajor 차이: %d개\n\n"
                         total (avg_sim *. 100.) critical major
                       in
                       let details = String.concat "\n---\n"
                         (List.map Figma_compare.result_to_string results)
                       in
                       Ok (make_text_content (summary ^ details))
                     end
                     else begin
                       (* Single 모드: 특정 노드 쌍 비교 *)
                       match node_a_id, node_b_id with
                       | Some id_a, Some id_b ->
                           let find_node id = List.find_opt (fun n -> n.Figma_types.id = id) all_nodes in
                           (match find_node id_a, find_node id_b with
                            | Some node_a, Some node_b ->
                                let result = Figma_compare.compare_nodes node_a node_b in
                                Ok (make_text_content (Figma_compare.result_to_string result))
                            | None, _ -> Error (Printf.sprintf "Node A not found: %s" id_a)
                            | _, None -> Error (Printf.sprintf "Node B not found: %s" id_b))
                       | _ -> Error "Single mode requires node_a_id and node_b_id"
                     end
                 | None -> Error "Failed to parse document"))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_tree 핸들러 *)
let handle_tree args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let node_id = get_string "node_id" args in
  let style_str = get_string_or "style" "ascii" args in
  let max_depth = get_float "max_depth" args |> Option.map int_of_float in
  let show_size = get_string_or "show_size" "true" args = "true" in
  let show_stats = get_string_or "show_stats" "false" args = "true" in

  let style = match style_str with
    | "indent" -> Figma_tree.Indent
    | "compact" -> Figma_tree.Compact
    | _ -> Figma_tree.Ascii
  in

  match file_key, token with
  | Some file_key, Some token ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           (match Yojson.Safe.Util.member "document" json with
            | `Null -> Error "Document not found"
            | doc_json ->
                (match Figma_parser.parse_node doc_json with
                 | Some root ->
                     let start_node = match node_id with
                       | Some id ->
                           let all = Figma_query.collect_nodes ~max_depth:None root in
                           (match List.find_opt (fun n -> n.Figma_types.id = id) all with
                            | Some n -> n
                            | None -> root)
                       | None -> root
                     in
                     let result = Figma_tree.render ~style ~max_depth ~show_size ~show_stats start_node in
                     Ok (make_text_content result)
                 | None -> Error "Failed to parse document"))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_stats 핸들러 *)
let handle_stats args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let node_id = get_string "node_id" args in

  match file_key, token with
  | Some file_key, Some token ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           (match Yojson.Safe.Util.member "document" json with
            | `Null -> Error "Document not found"
            | doc_json ->
                (match Figma_parser.parse_node doc_json with
                 | Some root ->
                     let start_node = match node_id with
                       | Some id ->
                           let all = Figma_query.collect_nodes ~max_depth:None root in
                           (match List.find_opt (fun n -> n.Figma_types.id = id) all with
                            | Some n -> n
                            | None -> root)
                       | None -> root
                     in
                     let all_nodes = Figma_query.collect_nodes ~max_depth:None start_node in
                     let stats = Figma_stats.generate_report all_nodes in
                     Ok (make_text_content (Figma_stats.report_to_string stats))
                 | None -> Error "Failed to parse document"))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_export_tokens 핸들러 *)
let handle_export_tokens args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = get_string "token" args in
  let format = get_string_or "format" "css" args in
  let node_id = get_string "node_id" args in

  match file_key, token with
  | Some file_key, Some token ->
      (match Figma_effects.Perform.get_file ~token ~file_key () with
       | Ok json ->
           (match Yojson.Safe.Util.member "document" json with
            | `Null -> Error "Document not found"
            | doc_json ->
                (match Figma_parser.parse_node doc_json with
                 | Some root ->
                     let start_node = match node_id with
                       | Some id ->
                           let all = Figma_query.collect_nodes ~max_depth:None root in
                           (match List.find_opt (fun n -> n.Figma_types.id = id) all with
                            | Some n -> n
                            | None -> root)
                       | None -> root
                     in
                     let all_nodes = Figma_query.collect_nodes ~max_depth:None start_node in
                     let result = match format with
                       | "semantic" ->
                         (* UIFormer-inspired Semantic DSL output *)
                         all_nodes
                         |> List.map (fun n ->
                           let dsl = Semantic_mapper.node_to_semantic n in
                           let prefix = match n.Figma_types.node_type with
                             | Figma_types.Frame | Figma_types.Component | Figma_types.Instance -> "F"
                             | Figma_types.Text -> "T"
                             | Figma_types.Rectangle | Figma_types.Ellipse | Figma_types.Vector -> "V"
                             | _ -> "N"
                           in
                           Printf.sprintf "%s(%s) ; %s" prefix dsl n.Figma_types.name)
                         |> String.concat "\n"
                       | _ ->
                         (* Design token extraction (CSS/Tailwind/JSON) *)
                         let tokens = Figma_tokens.extract_all all_nodes in
                         match format with
                         | "tailwind" -> Figma_tokens.to_tailwind tokens
                         | "json" -> Figma_tokens.to_json tokens
                         | _ -> Figma_tokens.to_css tokens
                     in
                     Ok (make_text_content result)
                 | None -> Error "Failed to parse document"))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** 환경/의존성 점검 핸들러 *)
let handle_doctor _args : (Yojson.Safe.t, string) result =
  let mk_check name ok detail =
    `Assoc [
      ("name", `String name);
      ("ok", `Bool ok);
      ("detail", `String detail);
    ]
  in
  let node_ok = has_command "node" in
  let node_version =
    if node_ok then command_output "node -v" else "missing"
  in
  let playwright_ok = node_ok && has_node_module "playwright" in
  let pngjs_ok = node_ok && has_node_module "pngjs" in
  let pixelmatch_ok = node_ok && has_node_module "pixelmatch" in
  let magick_ok = has_command "magick" || has_command "convert" in
  let magick_detail =
    if has_command "magick" then "magick"
    else if has_command "convert" then "convert"
    else "missing"
  in
  let sips_ok = has_command "sips" in
  let render_script = Visual_verifier.render_script_path in
  let ssim_script = Visual_verifier.ssim_script_path in
  let render_script_ok = Sys.file_exists render_script in
  let ssim_script_ok = Sys.file_exists ssim_script in

  let required_ok =
    node_ok
    && playwright_ok
    && pngjs_ok
    && pixelmatch_ok
    && magick_ok
    && render_script_ok
    && ssim_script_ok
  in

  let checks = `List [
    mk_check "node" node_ok node_version;
    mk_check "playwright" playwright_ok (if playwright_ok then "ok" else "missing");
    mk_check "pngjs" pngjs_ok (if pngjs_ok then "ok" else "missing");
    mk_check "pixelmatch" pixelmatch_ok (if pixelmatch_ok then "ok" else "missing");
    mk_check "imagemagick" magick_ok magick_detail;
    mk_check "sips" sips_ok (if sips_ok then "ok" else "missing");
    mk_check "render_script" render_script_ok render_script;
    mk_check "ssim_script" ssim_script_ok ssim_script;
  ] in

  let hints =
    List.filter_map Fun.id [
      if not node_ok then Some "Install Node.js (node required for render/compare scripts)." else None;
      if node_ok && not playwright_ok then Some "Install Playwright: npm i -D playwright && npx playwright install chromium." else None;
      if node_ok && (not pngjs_ok || not pixelmatch_ok) then Some "Install image deps: npm i -D pngjs pixelmatch." else None;
      if not magick_ok then Some "Install ImageMagick (magick/convert) for PPM conversion." else None;
      if not render_script_ok then Some "Ensure render-html.js path is valid (FIGMA_RENDER_SCRIPT or scripts/render-html.js)." else None;
      if not ssim_script_ok then Some "Ensure ssim-compare.js path is valid (scripts/ssim-compare.js)." else None;
    ]
  in

  let result = `Assoc [
    ("status", `String (if required_ok then "ok" else "needs_attention"));
    ("checks", checks);
    ("hints", `List (List.map (fun h -> `String h) hints));
  ] in
  Ok (make_text_content (Yojson.Safe.pretty_to_string result))

(** large_result 파일 읽기 핸들러 *)
let handle_read_large_result args : (Yojson.Safe.t, string) result =
  let file_path = get_string "file_path" args in
  let offset = get_int "offset" args |> Option.value ~default:0 in
  let limit = get_int "limit" args |> Option.value ~default:20000 in

  match file_path with
  | None -> Error "Missing required parameter: file_path"
  | Some path ->
      let storage_dir = Large_response.storage_dir in
      if not (is_under_dir ~dir:storage_dir path) then
        Error (Printf.sprintf "file_path must be under %s" storage_dir)
      else if not (Sys.file_exists path) then
        Error (Printf.sprintf "File not found: %s" path)
      else
        let safe_offset = max 0 offset in
        let safe_limit = if limit <= 0 then 20000 else limit in
        let ic = open_in_bin path in
        let total = in_channel_length ic in
        if safe_offset >= total then begin
          close_in ic;
          Error "offset is beyond EOF"
        end else begin
          seek_in ic safe_offset;
          let to_read = min safe_limit (total - safe_offset) in
          let chunk = really_input_string ic to_read in
          close_in ic;
          let result = `Assoc [
            ("file_path", `String path);
            ("offset", `Int safe_offset);
            ("limit", `Int safe_limit);
            ("read_bytes", `Int to_read);
            ("total_bytes", `Int total);
            ("eof", `Bool (safe_offset + to_read >= total));
            ("chunk", `String chunk);
          ] in
          Ok (make_text_content (Yojson.Safe.pretty_to_string result))
        end

(** 캐시 통계 핸들러 *)
let handle_cache_stats _args : (Yojson.Safe.t, string) result =
  let stats = Figma_cache.stats () in
  Ok stats

(** 캐시 무효화 핸들러 *)
let handle_cache_invalidate args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  Figma_cache.invalidate ?file_key ?node_id ();
  let message = match file_key, node_id with
    | None, _ -> "All cache invalidated"
    | Some fk, None -> sprintf "Cache invalidated for file: %s" fk
    | Some fk, Some nid -> sprintf "Cache invalidated for node: %s/%s" fk nid
  in
  Ok (`Assoc [("status", `String "ok"); ("message", `String message)])

(** ============== 핸들러 맵 ============== *)

(** sync 핸들러를 Lwt로 래핑.
    - stdio 모드: Effect 핸들러로 감싸서 실행 (테스트 가능)
    - HTTP 모드: Effect 없이 직접 실행 (Lwt_main.run 중첩 방지) *)
let wrap_sync (f : Yojson.Safe.t -> (Yojson.Safe.t, string) result) : tool_handler =
  fun args ->
    if !is_http_mode then
      (* HTTP 모드: Effect 없이 직접 실행 - Lwt가 이미 실행 중 *)
      Lwt.return (f args)
    else
      (* stdio 모드: Effect 핸들러로 감싸서 실행 *)
      let result = Figma_effects.run_with_real_api (fun () -> f args) in
      Lwt.return result

let all_handlers : (string * tool_handler) list = [
  (* 기존 도구 *)
  ("figma_codegen", handle_codegen);  (* 이미 Lwt *)
  ("figma_get_file", wrap_sync handle_get_file);
  ("figma_get_file_meta", wrap_sync handle_get_file_meta);
  ("figma_list_screens", wrap_sync handle_list_screens);
  ("figma_get_node", wrap_sync handle_get_node);
  ("figma_get_node_with_image", wrap_sync handle_get_node_with_image);
  ("figma_get_node_bundle", wrap_sync handle_get_node_bundle);
  ("figma_get_node_summary", wrap_sync handle_get_node_summary);
  ("figma_select_nodes", wrap_sync handle_select_nodes);
  ("figma_get_node_chunk", wrap_sync handle_get_node_chunk);
  ("figma_chunk_index", handle_chunk_index);
  ("figma_chunk_get", wrap_sync handle_chunk_get);
  ("figma_fidelity_loop", wrap_sync handle_fidelity_loop);
  ("figma_image_similarity", wrap_sync handle_image_similarity);
  ("figma_verify_visual", wrap_sync handle_verify_visual);
  ("figma_compare_regions", wrap_sync handle_compare_regions);
  ("figma_evolution_report", wrap_sync handle_evolution_report);
  ("figma_compare_elements", wrap_sync handle_compare_elements);
  ("figma_export_image", wrap_sync handle_export_image);
  ("figma_get_image_fills", wrap_sync handle_get_image_fills);
  ("figma_get_nodes", wrap_sync handle_get_nodes);
  ("figma_get_file_versions", wrap_sync handle_get_file_versions);
  ("figma_get_file_comments", wrap_sync handle_get_file_comments);
  ("figma_post_comment", wrap_sync handle_post_comment);
  ("figma_get_file_components", wrap_sync handle_get_file_components);
  ("figma_get_team_components", wrap_sync handle_get_team_components);
  ("figma_get_file_component_sets", wrap_sync handle_get_file_component_sets);
  ("figma_get_team_component_sets", wrap_sync handle_get_team_component_sets);
  ("figma_get_file_styles", wrap_sync handle_get_file_styles);
  ("figma_get_team_styles", wrap_sync handle_get_team_styles);
  ("figma_get_component", wrap_sync handle_get_component);
  ("figma_get_component_set", wrap_sync handle_get_component_set);
  ("figma_get_style", wrap_sync handle_get_style);
  ("figma_plugin_connect", wrap_sync handle_plugin_connect);
  ("figma_plugin_use_channel", wrap_sync handle_plugin_use_channel);
  ("figma_plugin_status", wrap_sync handle_plugin_status);
  ("figma_plugin_read_selection", wrap_sync handle_plugin_read_selection);
  ("figma_plugin_get_node", wrap_sync handle_plugin_get_node);
  ("figma_plugin_export_node_image", wrap_sync handle_plugin_export_node_image);
  ("figma_plugin_get_variables", wrap_sync handle_plugin_get_variables);
  ("figma_plugin_apply_ops", wrap_sync handle_plugin_apply_ops);
  ("figma_llm_call", handle_llm_call);
  ("figma_llm_task", handle_llm_task);
  (* Phase 1: 탐색 도구 *)
  ("figma_parse_url", wrap_sync handle_parse_url);
  ("figma_get_me", wrap_sync handle_get_me);
  ("figma_list_projects", wrap_sync handle_list_projects);
  ("figma_list_files", wrap_sync handle_list_files);
  ("figma_get_variables", wrap_sync handle_get_variables);
  (* Phase 2: 고급 쿼리 *)
  ("figma_query", wrap_sync handle_query);
  ("figma_search", wrap_sync handle_search);
  ("figma_compare", wrap_sync handle_compare);
  (* Phase 3: 분석/추출 *)
  ("figma_tree", wrap_sync handle_tree);
  ("figma_stats", wrap_sync handle_stats);
  ("figma_export_tokens", wrap_sync handle_export_tokens);
  ("figma_doctor", wrap_sync handle_doctor);
  ("figma_read_large_result", wrap_sync handle_read_large_result);
  (* 캐시 관리 *)
  ("figma_cache_stats", wrap_sync handle_cache_stats);
  ("figma_cache_invalidate", wrap_sync handle_cache_invalidate);
]

(** ============== Resources / Prompts ============== *)

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
]

let prompts : mcp_prompt list = [
  {
    name = "figma_fidelity_review";
    description = "레이아웃/페인트/타이포 누락 필드 확인용 리뷰 프롬프트";
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
6) 렌더 정확도 이슈 시 figma_get_node_with_image + use_absolute_bounds=true 제안
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
]

let read_resource uri =
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
- MCP tools recommend colon format: `figma_get_node`, `figma_get_node_with_image`
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
3) Validate pixels with `figma_get_node_with_image` + `use_absolute_bounds=true`
4) Compare renders via `figma_image_similarity`

## Full-axes options
- `figma_fidelity_loop` + `include_variables=true` + `include_image_fills=true` + `include_plugin=true`
- `figma_get_node_bundle` + `include_plugin=true` for text segments/line bounds
- `include_plugin_variables=true` for Variables API fallback (Enterprise-free)
- `include_plugin_image=true` for plugin-rendered base64 images (large output)
- Pair DSL with images via `figma_get_node_with_image` (use_absolute_bounds=true)
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
- MCP tools recommend colon format: `figma_get_node`, `figma_get_node_with_image`
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
- `PlanTasks` supports `recursive=true` to generate divide-and-conquer task lists
- `grpcurl` 사용 시 reflection이 비활성화되어 있으므로 `-import-path proto -proto figma.proto` 옵션 필요

## Pixel accuracy
- Pair DSL with images via `figma_get_node_with_image`
- Use `use_absolute_bounds=true` to include effects in render bounds
|} in
      Ok ("text/markdown", body)
  | _ -> Error "Resource not found"

(** ============== 서버 생성 ============== *)

let create_figma_server () =
  Mcp_protocol.create_server all_tools all_handlers resources prompts read_resource
