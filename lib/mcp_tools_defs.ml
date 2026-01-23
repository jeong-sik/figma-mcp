(** Figma MCP Tools 정의 *)

open Mcp_protocol

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
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷 (기본값: fidelity)");
    ("depth", number_prop "트리 깊이 제한 (Figma API depth 파라미터)");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"];
}

let tool_figma_get_file_meta : tool_def = {
  name = "figma_get_file_meta";
  description = "Figma 파일의 컴포넌트/스타일 메타데이터를 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"];
}

let tool_figma_list_screens : tool_def = {
  name = "figma_list_screens";
  description = "Figma 파일 내 모든 화면(Frame/Component) 목록을 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_node : tool_def = {
  name = "figma_get_node";
  description = "특정 노드 ID의 데이터를 가져와 Fidelity DSL로 변환합니다. (전체 재귀는 gRPC GetNodeStream recursive 사용 권장)";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷");
    ("depth", number_prop "트리 깊이 제한 (Figma API depth 파라미터)");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("version", string_prop "특정 파일 버전 ID");
  ] [];
}

let tool_figma_get_node_with_image : tool_def = {
  name = "figma_get_node_with_image";
  description = "특정 노드의 Fidelity DSL과 이미지 URL을 동시에 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
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
  ] [];
}

let tool_figma_get_node_bundle : tool_def = {
  name = "figma_get_node_bundle";
  description = "정확도 극대화 번들: 노드 DSL + 렌더 이미지 + 메타/변수/이미지 fills/플러그인 보강을 한번에 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
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
  ] [];
}

(** 경량 구조 요약 - 큰 노드를 탐색할 때 전체 로드 없이 구조 파악 *)
let tool_figma_get_node_summary : tool_def = {
  name = "figma_get_node_summary";
  description = "노드의 경량 구조 요약을 반환합니다. 전체 콘텐츠 없이 자식 노드 목록, 타입, 예상 크기만 포함하여 대형 노드 탐색에 적합합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("max_children", number_prop "반환할 최대 자식 수 (기본값: 50)");
    ("version", string_prop "특정 파일 버전 ID");
  ] [];
}

(** 노드 자동 선택 - 점수 기반 후보 선별 *)
let tool_figma_select_nodes : tool_def = {
  name = "figma_select_nodes";
  description = "URL/노드 기준으로 후보 노드를 점수화해 선택 목록과 노트 텍스트를 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("summary_depth", number_prop "분석 depth (기본값: 1, 최대: 6)");
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
  ] [];
}

(** 깊이 범위별 청크 로드 - 대형 노드를 점진적으로 로드 *)
let tool_figma_get_node_chunk : tool_def = {
  name = "figma_get_node_chunk";
  description = "특정 깊이 범위의 노드 데이터만 가져옵니다. 대형 노드를 점진적으로 로드할 때 사용합니다. depth_start=0, depth_end=2면 루트부터 2단계까지만 반환.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("depth_start", number_prop "시작 깊이 (기본값: 0)");
    ("depth_end", number_prop "종료 깊이 (기본값: 2)");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷 (기본값: fidelity)");
    ("include_styles", bool_prop "스타일 정의 포함 여부 (기본값: false)");
    ("version", string_prop "특정 파일 버전 ID");
  ] [];
}

(* NOTE: figma_chunk_index was removed - not implemented.
   Use figma_get_node + figma_codegen separately for chunked processing. *)

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
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
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
  ] [];
}

let tool_figma_image_similarity : tool_def = {
  name = "figma_image_similarity";
  description = "렌더 이미지 SSIM/PSNR 비교로 정확도를 평가합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_a_id", string_prop "기준 노드 ID");
    ("node_b_id", string_prop "비교 노드 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["png"; "jpg"] "이미지 포맷 (기본값: png)");
    ("start_scale", number_prop "시작 스케일 (기본값: 1)");
    ("max_scale", number_prop "최대 스케일 (기본값: start_scale)");
    ("scale_step", number_prop "스케일 증가폭 (기본값: 1)");
    ("target_ssim", number_prop "목표 SSIM (0-1, 옵션)");
    ("use_absolute_bounds", bool_prop "효과 포함한 렌더 바운즈 사용 여부");
    ("version", string_prop "특정 파일 버전 ID");
    ("save_dir", string_prop "이미지 저장 경로 (기본값: ~/me/download/figma-assets/compare)");
  ] ["file_key"; "node_a_id"; "node_b_id"];
}

(** Visual Feedback Loop - 코드 생성 및 시각적 검증 *)
let tool_figma_verify_visual : tool_def = {
  name = "figma_verify_visual";
  description = "코드를 생성하고 Figma 렌더와 비교하여 시각적 정확도(SSIM)와 텍스트 정확도를 검증합니다. SSIM과 TEXT 모두 통과해야 overall_passed=true. SSIM < target_ssim이면 자동으로 CSS를 조정합니다. 진화 과정은 자동으로 /tmp/figma-evolution/run_*에 저장됩니다. html_screenshot 제공 시 Playwright 대신 외부 렌더링 이미지를 사용합니다 (Chrome MCP 등).";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("html", string_prop "검증할 HTML 코드 (없으면 자동 생성)");
    ("html_screenshot", string_prop "외부 렌더링된 HTML 스크린샷 경로 (Chrome MCP 등). 제공 시 Playwright 스킵");
    ("target_ssim", number_prop "목표 SSIM (0-1, 기본값: 0.95)");
    ("max_iterations", number_prop "최대 반복 횟수 (기본값: 3)");
    ("width", number_prop "뷰포트 너비 (기본값: 375)");
    ("height", number_prop "뷰포트 높이 (기본값: 812)");
    ("version", string_prop "특정 파일 버전 ID");
    ("mode", enum_prop ["full"; "structure"; "icons"; "text"; "layout"] "비교 모드: full(전체), structure(레이아웃만), icons(아이콘만), text(텍스트만), layout(박스/컨테이너)");
    ("checkpoints", string_prop "사용자 정의 체크포인트 JSON 배열 [{name, x, y, width, height}]");
  ] ["file_key"; "node_id"];
}

(** Pixel-Perfect Loop - SSIM 기반 CSS 자동 보정 루프 *)
let tool_figma_pixel_perfect_loop : tool_def = {
  name = "figma_pixel_perfect_loop";
  description = "🧬 Figma DNA 분석 MCP - SSIM 차이 분석 + CSS 자동 보정 제안을 통해 99%+ Pixel-Perfect 구현을 달성합니다. Figma 노드와 구현된 HTML/스크린샷을 비교하고, 문제 영역(edges, quadrants, strips)을 분석하여 구체적인 CSS 수정 제안을 반환합니다. 전문가 수준의 에러 처리, 타임아웃, scale, tool chaining 지원 포함. Progress 알림을 SSE로 전송합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("html", string_prop "구현된 HTML 코드");
    ("html_screenshot", string_prop "구현된 HTML의 스크린샷 경로 (Chrome MCP 등)");
    ("target_ssim", number_prop "목표 SSIM (0-1, 기본값: 0.99)");
    ("width", number_prop "뷰포트 너비 (기본값: 375)");
    ("height", number_prop "뷰포트 높이 (기본값: 812)");
    ("scale", number_prop "🆕 Figma 이미지 스케일 (@1x=1.0, @2x=2.0, @3x=3.0, 기본값: 1.0, 범위: 0.5-4.0)");
    ("timeout", number_prop "🆕 타임아웃 초 (기본값: 30.0)");
    ("version", string_prop "특정 파일 버전 ID");
    (* 🆕 Tool Chaining 옵션 *)
    ("include_node_dsl", bool_prop "🆕 결과에 figma_get_node DSL 포함 (기본값: false)");
    ("include_tokens", bool_prop "🆕 결과에 figma_export_tokens 포함 (기본값: false)");
    ("auto_region_analysis", bool_prop "🆕 SSIM < 90% 시 자동 region 상세 분석 (기본값: false)");
  ] ["file_key"; "node_id"];
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
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "이미지 포맷");
    ("scale", number_prop "스케일 (1-4, 기본값: 1)");
    ("use_absolute_bounds", bool_prop "효과 포함한 렌더 바운즈 사용 여부");
    ("version", string_prop "특정 파일 버전 ID");
    ("download", bool_prop "이미지 다운로드 여부 (기본값: false)");
    ("save_dir", string_prop "다운로드 저장 경로 (기본값: ~/me/download/figma-assets)");
  ] ["file_key"; "node_ids"];
}

(** Smart export - 자동 scale 조정 및 재귀 분할 지원 *)
let tool_figma_export_smart : tool_def = {
  name = "figma_export_smart";
  description = "대형 노드를 자동으로 scale 조정하거나 자식 노드로 분할하여 내보냅니다. " ^
                "max_pixels 초과 시 자동으로 scale을 낮추거나, split_children=true면 자식 노드로 분할합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "대상 노드 ID (단일)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "이미지 포맷 (기본값: png)");
    ("max_pixels", number_prop "최대 픽셀 수 (기본값: 16777216 = 4096x4096). 초과 시 scale 자동 조정");
    ("split_children", bool_prop "true면 자식 노드별로 분할 내보내기 (기본값: false)");
    ("max_depth", number_prop "split_children 시 최대 재귀 깊이 (기본값: 1)");
    ("download", bool_prop "이미지 다운로드 여부 (기본값: false)");
    ("save_dir", string_prop "다운로드 저장 경로");
    ("debug", bool_prop "디버그 정보 포함 여부 (기본값: false)");
  ] ["file_key"; "node_id"];
}

let tool_figma_get_image_fills : tool_def = {
  name = "figma_get_image_fills";
  description = "파일 내 이미지 채움(image fills) 원본 URL 맵을 반환합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("version", string_prop "특정 파일 버전 ID");
    ("download", bool_prop "이미지 다운로드 여부 (기본값: false)");
    ("save_dir", string_prop "다운로드 저장 경로 (기본값: ~/me/download/figma-assets)");
  ] ["file_key"];
}

let tool_figma_get_nodes : tool_def = {
  name = "figma_get_nodes";
  description = "여러 노드 ID의 데이터를 한 번에 가져옵니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_ids", string_prop "노드 ID들 (쉼표 구분: 1:2,3:4)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["raw"; "fidelity"; "html"] "출력 포맷 (기본값: raw)");
    ("depth", number_prop "트리 깊이 제한");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"; "node_ids"];
}

let tool_figma_get_file_versions : tool_def = {
  name = "figma_get_file_versions";
  description = "파일 버전 목록을 조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_file_comments : tool_def = {
  name = "figma_get_file_comments";
  description = "파일 코멘트 목록을 조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_post_comment : tool_def = {
  name = "figma_post_comment";
  description = "파일에 코멘트를 추가합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
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
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_team_components : tool_def = {
  name = "figma_get_team_components";
  description = "팀의 컴포넌트 목록을 조회합니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
}

let tool_figma_get_file_component_sets : tool_def = {
  name = "figma_get_file_component_sets";
  description = "파일의 컴포넌트 셋 목록을 조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_team_component_sets : tool_def = {
  name = "figma_get_team_component_sets";
  description = "팀의 컴포넌트 셋 목록을 조회합니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
}

let tool_figma_get_file_styles : tool_def = {
  name = "figma_get_file_styles";
  description = "파일의 스타일 목록을 조회합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_team_styles : tool_def = {
  name = "figma_get_team_styles";
  description = "팀의 스타일 목록을 조회합니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
}

let tool_figma_get_component : tool_def = {
  name = "figma_get_component";
  description = "컴포넌트 키로 상세 정보를 조회합니다.";
  input_schema = object_schema [
    ("component_key", string_prop "컴포넌트 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["component_key"];
}

let tool_figma_get_component_set : tool_def = {
  name = "figma_get_component_set";
  description = "컴포넌트 셋 키로 상세 정보를 조회합니다.";
  input_schema = object_schema [
    ("component_set_key", string_prop "컴포넌트 셋 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["component_set_key"];
}

let tool_figma_get_style : tool_def = {
  name = "figma_get_style";
  description = "스타일 키로 상세 정보를 조회합니다.";
  input_schema = object_schema [
    ("style_key", string_prop "스타일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["style_key"];
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
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
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
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] [];
}

let tool_figma_list_projects : tool_def = {
  name = "figma_list_projects";
  description = "팀의 모든 프로젝트 목록을 반환합니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID (URL에서 추출 또는 figma_parse_url 사용)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
}

let tool_figma_list_files : tool_def = {
  name = "figma_list_files";
  description = "프로젝트의 모든 파일 목록을 반환합니다.";
  input_schema = object_schema [
    ("project_id", string_prop "프로젝트 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["project_id"];
}

let tool_figma_get_variables : tool_def = {
  name = "figma_get_variables";
  description = "파일의 디자인 토큰/변수를 반환합니다 (색상, 타이포, 간격 등).";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["summary"; "raw"; "resolved"] "출력 포맷 (기본값: summary)");
  ] ["file_key"];
}

(** ============== Phase 2: 고급 쿼리 도구 ============== *)

let tool_figma_query : tool_def = {
  name = "figma_query";
  description = "노드를 조건으로 필터링합니다. SQL WHERE처럼 type, 크기, 색상 등으로 검색합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
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
  ] ["file_key"];
}

let tool_figma_search : tool_def = {
  name = "figma_search";
  description = "텍스트 내용이나 이름으로 노드를 검색합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
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
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("node_a_id", string_prop "첫 번째 노드 ID (예: 100:200)");
    ("node_b_id", string_prop "두 번째 노드 ID");
    ("mode", enum_prop ["single"; "batch"] "비교 모드: single (단일 쌍), batch (Web/Mobile 일괄 매칭)");
    ("web_prefix", string_prop "Web 노드 이름 접두사 (batch 모드)");
    ("mobile_prefix", string_prop "Mobile 노드 이름 접두사 (batch 모드)");
  ] ["file_key"];
}

let tool_figma_tree : tool_def = {
  name = "figma_tree";
  description = "Figma 노드 트리를 시각적으로 표시합니다. ASCII 트리, 들여쓰기, 압축 포맷 지원.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("node_id", string_prop "시작 노드 ID (생략시 전체 문서)");
    ("style", enum_prop ["ascii"; "indent"; "compact"] "출력 스타일 (기본값: ascii)");
    ("max_depth", number_prop "최대 깊이 (기본값: 무제한)");
    ("show_size", enum_prop ["true"; "false"] "크기 표시 (기본값: true)");
    ("show_stats", enum_prop ["true"; "false"] "통계 포함 (기본값: false)");
  ] ["file_key"];
}

let tool_figma_stats : tool_def = {
  name = "figma_stats";
  description = "Figma 파일의 디자인 통계를 분석합니다. 색상, 폰트, 크기, 컴포넌트 사용 현황.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("node_id", string_prop "분석 시작 노드 ID (생략시 전체 문서)");
  ] ["file_key"];
}

let tool_figma_export_tokens : tool_def = {
  name = "figma_export_tokens";
  description = "Figma 파일에서 디자인 토큰을 추출합니다. CSS, Tailwind, JSON, Semantic DSL 포맷 지원.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["css"; "tailwind"; "json"; "semantic"] "출력 포맷 (기본값: css). semantic=UIFormer 스타일 DSL");
    ("node_id", string_prop "추출 시작 노드 ID (생략시 전체 문서)");
  ] ["file_key"];
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
  tool_figma_chunk_get;
  tool_figma_fidelity_loop;
  tool_figma_image_similarity;
  tool_figma_verify_visual;
  tool_figma_pixel_perfect_loop;
  tool_figma_compare_regions;
  tool_figma_evolution_report;
  tool_figma_compare_elements;
  tool_figma_export_image;
  tool_figma_export_smart;
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
