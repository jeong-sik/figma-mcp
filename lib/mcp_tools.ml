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

(** Note: required parameter is not used - JSON Schema required is at object level *)
let string_prop desc : Yojson.Safe.t =
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
  description = "[Advanced] JSON을 Fidelity DSL로 변환. 보통 figma_get_node가 자동 처리.";
  input_schema = object_schema [
    ("json", string_prop "Figma JSON 데이터 (document 노드 또는 전체 응답)");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷: fidelity (정확도 우선), raw (원본 JSON), html (HTML 프리뷰)");
  ] ["json"];
}

let tool_figma_get_file : tool_def = {
  name = "figma_get_file";
  description = "[Advanced] 전체 파일 데이터. 대용량 주의. 보통 figma_get_node로 충분.";
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
  description = "📋 QUICK: 파일의 컴포넌트/스타일 메타데이터. 디자인 시스템 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"];
}

let tool_figma_list_screens : tool_def = {
  name = "figma_list_screens";
  description = "📋 QUICK: 파일의 화면/Frame/Component 목록. 탐색 시작점 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_node : tool_def = {
  name = "figma_get_node";
  description = "🎯 CORE: Figma 노드를 Fidelity DSL로 변환. UI 구현의 첫 단계로 사용. URL 또는 file_key+node_id 지정. 대형 노드는 depth 제한 권장. 반환: DSL 문자열 + 구조 정보. (전체 재귀는 gRPC GetNodeStream recursive 사용)";
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

let tool_figma_get_node_bundle : tool_def = {
  name = "figma_get_node_bundle";
  description = "📦 RECOMMENDED: 구현에 필요한 모든 정보를 한번에. DSL + 렌더 이미지 + 변수 + 이미지 fills. Visual Verification 전 사용 권장. download=true로 에셋 저장. 반환: 번들 JSON.";
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
  description = "📋 QUICK: 대형 노드 탐색 전 구조 파악. 전체 로드 없이 자식 목록/타입/크기만. Outside-In 패턴의 첫 단계. 반환: children 배열 (id, name, type, size).";
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
  description = "🎯 CORE: 후보 노드 점수화 선택. 노트/주석 자동 분리. Outside-In 첫 단계.";
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
  description = "📦 CHUNK: 깊이 범위별 노드 로드. 대형 노드 점진적 탐색. depth_start=0, depth_end=2면 2단계까지.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("depth_start", number_prop "시작 깊이 (기본값: 0)");
    ("depth_end", number_prop "종료 깊이 (기본값: 2)");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷 (기본값: fidelity)");
    ("max_children", number_prop "자식 노드 최대 개수 (옵션, 초과 시 잘라냄)");
    ("warn_large", bool_prop "큰 노드 경고 활성화 (기본값: true)");
    ("warn_threshold", number_prop "경고 기준 children 수 (기본값: 500)");
    ("error_on_large", bool_prop "큰 노드면 에러 반환 (기본값: false)");
    ("auto_trim_children", bool_prop "큰 노드 자동 자르기 (기본값: false)");
    ("auto_trim_limit", number_prop "자동 자르기 최대 children 수 (기본값: 200)");
    ("include_styles", bool_prop "스타일 정의 포함 여부 (기본값: false)");
    ("version", string_prop "특정 파일 버전 ID");
  ] [];
}

let tool_figma_fidelity_loop : tool_def = {
  name = "figma_fidelity_loop";
  description = "🔄 AUTO: fidelity 점수 미달 시 depth/geometry 자동 증가. 목표 달성까지 반복.";
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
    ("summary_only", bool_prop "대용량 방지를 위해 요약만 반환 (기본값: false, 필요 시 자동 요약)");
    ("max_inline_bytes", number_prop "인라인 응답 최대 바이트 (기본값: LargeResponse 설정)");
  ] [];
}

let tool_figma_image_similarity : tool_def = {
  name = "figma_image_similarity";
  description = "✅ VERIFY: 렌더 이미지 SSIM/PSNR 비교. 노드 간 정확도 평가.";
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
  description = "✅ VERIFY: HTML을 Figma 렌더와 SSIM 비교. target_ssim=0.95 기본. 미달 시 CSS 자동 조정 (max 3회). html_screenshot으로 Chrome MCP 연동 가능. 반환: ssim_score, text_match, overall_passed, evolution_dir.";
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

(** Region-based comparison - 영역별 상세 비교 *)
let tool_figma_compare_regions : tool_def = {
  name = "figma_compare_regions";
  description = "[DEPRECATED] figma_compare(mode: \"regions\")를 사용하세요. 영역별 이미지 비교 기능은 통합 compare 도구로 이전되었습니다.";
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
  description = "[DEPRECATED] figma_compare(mode: \"evolution\")를 사용하세요. 진화 리포트 기능은 통합 compare 도구로 이전되었습니다.";
  input_schema = object_schema [
    ("run_dir", string_prop "Evolution 디렉토리 경로 (예: /tmp/figma-evolution/run_1234567890). 없으면 최근 실행 목록 반환");
    ("generate_image", bool_prop "비교 이미지 자동 생성 여부 (기본값: true)");
  ] [];
}

(** Compare Elements - 색상/박스 확장 메트릭 비교 *)
let tool_figma_compare_elements : tool_def = {
  name = "figma_compare_elements";
  description = "[DEPRECATED] figma_compare(mode: \"elements\")를 사용하세요. 색상/박스 비교 기능은 통합 compare 도구로 이전되었습니다.";
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
  description = "🖼️ ASSET: 노드 이미지 내보내기 URL. 에셋 다운로드용.";
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
  description = "🖼️ ASSET: 대형 노드 스마트 내보내기. max_pixels 초과 시 scale 조정 또는 분할.";
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
  description = "🖼️ ASSET: 이미지 채움(fills) URL 맵 반환. 에셋 수집용.";
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
  description = "📦 BATCH: 여러 노드 ID 데이터를 한번에. 반복 API 호출 절약.";
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
  description = "[Advanced] 파일 버전 목록. 히스토리 추적용.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_file_comments : tool_def = {
  name = "figma_get_file_comments";
  description = "[Advanced] 파일 코멘트 목록. 협업 히스토리 조회.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_post_comment : tool_def = {
  name = "figma_post_comment";
  description = "[Advanced] 파일에 코멘트 추가. 협업/피드백용.";
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
  description = "📋 QUICK: 파일의 컴포넌트 목록. 디자인 시스템 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_team_components : tool_def = {
  name = "figma_get_team_components";
  description = "[DEPRECATED] 팀 레벨 API는 Rate Limit이 엄격합니다. 대신 figma_get_file_components를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
}

let tool_figma_get_file_component_sets : tool_def = {
  name = "figma_get_file_component_sets";
  description = "📋 QUICK: 파일의 컴포넌트 셋(Variants). 디자인 시스템 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_team_component_sets : tool_def = {
  name = "figma_get_team_component_sets";
  description = "[DEPRECATED] 팀 레벨 API는 Rate Limit이 엄격합니다. 대신 figma_get_file_component_sets를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
}

let tool_figma_get_file_styles : tool_def = {
  name = "figma_get_file_styles";
  description = "📋 QUICK: 파일의 스타일 목록. 디자인 토큰 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
}

let tool_figma_get_team_styles : tool_def = {
  name = "figma_get_team_styles";
  description = "[DEPRECATED] 팀 레벨 API는 Rate Limit이 엄격합니다. 대신 figma_get_file_styles 또는 figma_export_tokens를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
}

let tool_figma_get_component : tool_def = {
  name = "figma_get_component";
  description = "[DEPRECATED] 개별 키 조회는 비효율적입니다. 대신 figma_get_file_components를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("component_key", string_prop "컴포넌트 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["component_key"];
}

let tool_figma_get_component_set : tool_def = {
  name = "figma_get_component_set";
  description = "[DEPRECATED] 개별 키 조회는 비효율적입니다. 대신 figma_get_file_component_sets를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("component_set_key", string_prop "컴포넌트 셋 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["component_set_key"];
}

let tool_figma_get_style : tool_def = {
  name = "figma_get_style";
  description = "[DEPRECATED] 개별 키 조회는 비효율적입니다. 대신 figma_get_file_styles 또는 figma_export_tokens를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("style_key", string_prop "스타일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["style_key"];
}

(** ============== Plugin Bridge 도구 ============== *)

let tool_figma_plugin_connect : tool_def = {
  name = "figma_plugin_connect";
  description = "🔌 PLUGIN: 채널 생성/연결. 실시간 동기화의 시작점.";
  input_schema = object_schema [
    ("channel_id", string_prop "기존 채널 ID (옵션)");
  ] [];
}

let tool_figma_plugin_use_channel : tool_def = {
  name = "figma_plugin_use_channel";
  description = "🔌 PLUGIN: 기본 채널 ID 설정. 매번 ID 지정 불필요.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID");
  ] ["channel_id"];
}

let tool_figma_plugin_status : tool_def = {
  name = "figma_plugin_status";
  description = "🔌 PLUGIN: 채널 상태 확인. 연결 디버깅용.";
  input_schema = object_schema [] [];
}

let tool_figma_plugin_read_selection : tool_def = {
  name = "figma_plugin_read_selection";
  description = "🔌 PLUGIN: 현재 선택 노드 정보. Desktop 앱과 연동.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("depth", number_prop "자식 탐색 깊이 (기본값: 6)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
}

let tool_figma_plugin_get_node : tool_def = {
  name = "figma_plugin_get_node";
  description = "🔌 PLUGIN: 특정 노드 정보. REST API 보다 빠름.";
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
  description = "🔌 PLUGIN: exportAsync로 이미지 내보내기. base64 반환.";
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
  description = "🔌 PLUGIN: Variables API로 로컬 변수/컬렉션. 디자인 토큰 추출.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
}

let tool_figma_plugin_apply_ops : tool_def = {
  name = "figma_plugin_apply_ops";
  description = "🔌 PLUGIN: 노드 생성/수정/삭제. 실시간 디자인 편집.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("ops", array_prop "작업 목록 (create/update/delete 오브젝트 배열)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] ["ops"];
}

(* STRAP 통합: plugin 도구 통합 (8→14 actions) *)
let tool_figma_plugin : tool_def = {
  name = "figma_plugin";
  description = "🔌 PLUGIN: Figma Desktop 앱과 실시간 연동. action으로 세부 동작 선택. 63개 action 지원.";
  input_schema = object_schema [
    ("action", enum_prop [
      "connect"; "use_channel"; "status";
      "read_selection"; "get_node"; "export_image";
      "get_variables"; "apply_ops";
      "list_pages"; "switch_page"; "list_components";
      "clone"; "group"; "ungroup";
      "set_selection"; "zoom_to"; "reorder";
      "set_locked"; "set_visible"; "flatten";
      "set_auto_layout"; "get_viewport"; "set_viewport"; "rename";
      "resize"; "move"; "set_opacity"; "set_corner_radius";
      "set_fill"; "set_stroke"; "set_effects";
      "create_component"; "detach_instance"; "set_text"; "find_all"; "notify";
      "create_frame"; "create_rectangle"; "create_ellipse"; "create_text";
      "create_line"; "create_polygon"; "create_star";
      "delete_node"; "duplicate"; "align"; "distribute";
      "boolean_union"; "boolean_subtract"; "boolean_intersect"; "boolean_exclude";
      "get_local_styles"; "set_constraints";
      "create_page"; "delete_page"; "rotate"; "flip";
      "outline_stroke"; "set_blend_mode"; "get_selection_colors";
      "swap_fill_stroke"; "copy_style"; "get_fonts"; "set_parent";
      "create_vector"; "set_image_fill"; "get_plugin_data"; "set_plugin_data";
      "get_doc_info"; "get_absolute_bounds"; "create_component_set"; "remove_auto_layout";
      "create_slice"; "set_export_settings"; "get_reactions"; "set_reactions";
      "rasterize"; "get_shared_plugin_data"; "set_shared_plugin_data";
      "swap_component"; "resize_to_fit"; "get_characters"; "set_range_fills";
      "set_range_font_size"; "insert_child"; "get_all_local_variables";
      "get_styles_by_type"; "apply_style"; "get_overrides"; "reset_overrides";
      "bring_to_front"; "send_to_back"; "set_grid"; "get_layer_list";
      "scroll_and_zoom"; "get_paint_styles"; "set_text_case";
      "get_stroke_details"; "set_stroke_weight"; "collapse_layer"
    ] "🎉 100개 action: 연결(3), 페이지(4), 문서(1), 생성(11), 조회(20), 편집(9), 변형(7), 불리언(4), 정렬(2), 스타일(21), 텍스트(5), 레이아웃(4), 컴포넌트(4), 내보내기(2), 프로토타입(2), 레이어(4)");
    ("channel_id", string_prop "채널 ID");
    ("node_id", string_prop "노드 ID");
    ("url", string_prop "Figma URL (node_id 자동 추출)");
    ("depth", number_prop "자식 탐색 깊이 (기본값: 6)");
    ("include_geometry", bool_prop "벡터/지오메트리 포함 여부");
    ("format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "이미지 포맷");
    ("scale", number_prop "스케일");
    ("ops", array_prop "작업 목록 (apply_ops)");
    ("timeout_ms", number_prop "응답 대기 시간");
    ("page_id", string_prop "페이지 ID");
    ("node_ids", array_prop "노드 ID 배열");
    ("offset_x", number_prop "X 오프셋");
    ("offset_y", number_prop "Y 오프셋");
    ("name", string_prop "이름");
    ("direction", enum_prop ["front"; "back"; "forward"; "backward"] "z-order 방향");
    ("locked", bool_prop "잠금 상태");
    ("visible", bool_prop "가시성");
    ("layout_mode", enum_prop ["HORIZONTAL"; "VERTICAL"; "NONE"] "레이아웃 모드");
    ("item_spacing", number_prop "아이템 간격");
    ("padding", number_prop "패딩");
    ("primary_alignment", enum_prop ["MIN"; "CENTER"; "MAX"; "SPACE_BETWEEN"] "주축 정렬");
    ("counter_alignment", enum_prop ["MIN"; "CENTER"; "MAX"] "교차축 정렬");
    ("center_x", number_prop "뷰포트 중심 X");
    ("center_y", number_prop "뷰포트 중심 Y");
    ("zoom", number_prop "줌 레벨");
    ("width", number_prop "너비 (resize)");
    ("height", number_prop "높이 (resize)");
    ("x", number_prop "X 좌표 (move)");
    ("y", number_prop "Y 좌표 (move)");
    ("opacity", number_prop "투명도 0-1 (set_opacity)");
    ("radius", number_prop "모서리 반경 (set_corner_radius)");
    ("top_left", number_prop "좌상단 반경");
    ("top_right", number_prop "우상단 반경");
    ("bottom_left", number_prop "좌하단 반경");
    ("bottom_right", number_prop "우하단 반경");
    ("r", number_prop "빨강 0-1 (set_fill)");
    ("g", number_prop "초록 0-1 (set_fill)");
    ("b", number_prop "파랑 0-1 (set_fill)");
    ("a", number_prop "알파 0-1 (set_fill)");
    ("stroke_r", number_prop "스트로크 빨강 (set_stroke)");
    ("stroke_g", number_prop "스트로크 초록 (set_stroke)");
    ("stroke_b", number_prop "스트로크 파랑 (set_stroke)");
    ("stroke_weight", number_prop "스트로크 두께 (set_stroke)");
    ("effects", array_prop "이펙트 배열 (set_effects)");
    ("text", string_prop "텍스트 내용 (set_text)");
    ("type", string_prop "노드 타입 필터 (find_all)");
    ("find_name", string_prop "이름 필터 (find_all)");
    ("name_contains", string_prop "이름 포함 필터 (find_all)");
    ("message", string_prop "알림 메시지 (notify)");
    ("notify_timeout", number_prop "알림 표시 시간 ms (notify, 기본값: 3000)");
    ("font_size", number_prop "폰트 크기 (create_text)");
    ("length", number_prop "선 길이 (create_line)");
    ("rotation", number_prop "회전 각도 (create_line)");
    ("point_count", number_prop "꼭지점 수 (create_polygon/create_star)");
    ("inner_radius", number_prop "내부 반경 0-1 (create_star)");
    ("align_direction", enum_prop ["left"; "center"; "right"; "top"; "middle"; "bottom"] "정렬 방향 (align)");
    ("distribute_direction", enum_prop ["horizontal"; "vertical"] "분배 방향 (distribute)");
    ("constraint_horizontal", enum_prop ["MIN"; "CENTER"; "MAX"; "STRETCH"; "SCALE"] "수평 제약 (set_constraints)");
    ("constraint_vertical", enum_prop ["MIN"; "CENTER"; "MAX"; "STRETCH"; "SCALE"] "수직 제약 (set_constraints)");
    ("angle", number_prop "회전 각도 (rotate)");
    ("flip_direction", enum_prop ["horizontal"; "vertical"] "뒤집기 방향 (flip)");
    ("blend_mode", enum_prop ["NORMAL"; "DARKEN"; "MULTIPLY"; "COLOR_BURN"; "LIGHTEN"; "SCREEN"; "COLOR_DODGE"; "OVERLAY"; "SOFT_LIGHT"; "HARD_LIGHT"; "DIFFERENCE"; "EXCLUSION"; "HUE"; "SATURATION"; "COLOR"; "LUMINOSITY"] "블렌드 모드 (set_blend_mode)");
    ("source_id", string_prop "소스 노드 ID (copy_style)");
    ("target_id", string_prop "타겟 노드 ID (copy_style)");
    ("parent_id", string_prop "부모 노드 ID (set_parent)");
    ("image_hash", string_prop "이미지 해시 (set_image_fill)");
    ("base64", string_prop "Base64 인코딩된 이미지 데이터 (set_image_fill)");
    ("scale_mode", enum_prop ["FILL"; "FIT"; "CROP"; "TILE"] "이미지 스케일 모드 (set_image_fill)");
    ("data_key", string_prop "플러그인 데이터 키 (get/set_plugin_data)");
    ("data_value", string_prop "플러그인 데이터 값 (set_plugin_data)");
    ("component_ids", array_prop "컴포넌트 ID 배열 (create_component_set, 최소 2개)");
    ("export_format", enum_prop ["PNG"; "JPG"; "SVG"; "PDF"] "내보내기 포맷 (set_export_settings)");
    ("suffix", string_prop "내보내기 파일 접미사 (set_export_settings)");
    ("append", bool_prop "기존 설정에 추가 여부 (set_export_settings)");
    ("trigger", enum_prop ["ON_CLICK"; "ON_HOVER"; "ON_PRESS"; "ON_DRAG"; "AFTER_TIMEOUT"; "MOUSE_ENTER"; "MOUSE_LEAVE"; "MOUSE_UP"; "MOUSE_DOWN"] "인터랙션 트리거 (set_reactions)");
    ("navigation", enum_prop ["NAVIGATE"; "SWAP"; "OVERLAY"; "SCROLL_TO"; "CHANGE_TO"] "네비게이션 타입 (set_reactions)");
    ("preserve_scroll", bool_prop "스크롤 위치 유지 (set_reactions)");
    ("namespace", string_prop "공유 플러그인 데이터 네임스페이스 (get/set_shared_plugin_data)");
    ("component_id", string_prop "교체할 컴포넌트 ID (swap_component)");
    ("axis", enum_prop ["horizontal"; "vertical"; "both"] "리사이즈 방향 (resize_to_fit)");
    ("padding", number_prop "패딩 (resize_to_fit)");
    ("start", number_prop "텍스트 범위 시작 인덱스 (set_range_fills/set_range_font_size)");
    ("end", number_prop "텍스트 범위 끝 인덱스 (set_range_fills/set_range_font_size)");
    ("r", number_prop "빨강 0-1 (set_range_fills)");
    ("g", number_prop "초록 0-1 (set_range_fills)");
    ("b", number_prop "파랑 0-1 (set_range_fills)");
    ("a", number_prop "알파 0-1 (set_range_fills)");
    ("font_size", number_prop "폰트 크기 (set_range_font_size)");
    ("index", number_prop "삽입 위치 (insert_child)");
    ("style_type", enum_prop ["FILL"; "PAINT"; "TEXT"; "EFFECT"; "GRID"; "STROKE"] "스타일 타입 (get_styles_by_type/apply_style)");
    ("style_id", string_prop "스타일 ID (apply_style)");
    ("pattern", enum_prop ["GRID"; "COLUMNS"; "ROWS"] "그리드 패턴 (set_grid)");
    ("count", number_prop "컬럼/로우 개수 (set_grid)");
    ("gutter", number_prop "거터 사이즈 (set_grid)");
    ("alignment", enum_prop ["MIN"; "CENTER"; "MAX"; "STRETCH"] "정렬 (set_grid)");
    ("visible", bool_prop "표시 여부 (set_grid)");
    ("zoom", number_prop "줌 레벨 (scroll_and_zoom)");
    ("text_case", enum_prop ["ORIGINAL"; "UPPER"; "LOWER"; "TITLE"; "SMALL_CAPS"; "SMALL_CAPS_FORCED"] "텍스트 대소문자 (set_text_case)");
    ("weight", number_prop "선 두께 (set_stroke_weight)");
    ("expand", bool_prop "레이어 펼치기 여부 (collapse_layer)");
  ] ["action"];
}

(** ============== Phase 1: 탐색 도구 ============== *)

let tool_figma_parse_url : tool_def = {
  name = "figma_parse_url";
  description = "🎯 CORE: URL 파싱 (Parse Don't Validate). file_key/node_id 추출. API 호출 없음.";
  input_schema = object_schema [
    ("url", string_prop "Figma URL (팀/프로젝트/파일/노드 페이지 모두 지원)");
  ] ["url"];
}

let tool_figma_get_me : tool_def = {
  name = "figma_get_me";
  description = "📋 QUICK: 인증 사용자 정보. 토큰 유효성 확인용.";
  input_schema = object_schema [
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] [];
}

let tool_figma_list_projects : tool_def = {
  name = "figma_list_projects";
  description = "📋 QUICK: 팀의 프로젝트 목록. 탐색 시작점 파악.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID (URL에서 추출 또는 figma_parse_url 사용)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
}

let tool_figma_list_files : tool_def = {
  name = "figma_list_files";
  description = "📋 QUICK: 프로젝트의 파일 목록. 탐색 시작점 파악.";
  input_schema = object_schema [
    ("project_id", string_prop "프로젝트 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["project_id"];
}

let tool_figma_crawl_team : tool_def = {
  name = "figma_crawl_team";
  description = "🕷️ CRAWL: 팀 전체를 재귀적으로 크롤링하여 Neo4j에 저장. Team→Projects→Files→Nodes 구조.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("team_name", string_prop "팀 이름 (선택, 그래프 노드에 표시)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("neo4j_uri", string_prop "Neo4j URI (기본값: NEO4J_URI 환경변수)");
    ("neo4j_user", string_prop "Neo4j 사용자 (기본값: NEO4J_USER 환경변수)");
    ("neo4j_password", string_prop "Neo4j 비밀번호 (기본값: NEO4J_PASSWORD 환경변수)");
    ("max_depth", string_prop "노드 탐색 최대 깊이 (기본값: 10)");
    ("rate_limit_ms", string_prop "API 호출 간 대기 시간 ms (기본값: 100)");
  ] ["team_id"];
}

let tool_figma_team_tree : tool_def = {
  name = "figma_team_tree";
  description = "🌳 TREE: 팀 구조를 ASCII 트리로 출력 (Neo4j 불필요). Team→Projects→Files 빠른 탐색.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("team_name", string_prop "팀 이름 (선택)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("include_nodes", bool_prop "파일 내 노드 포함 여부 (기본값: false)");
    ("node_depth", string_prop "노드 탐색 깊이 (기본값: 2, include_nodes=true일 때만 적용)");
  ] ["team_id"];
}

let tool_figma_export_team : tool_def = {
  name = "figma_export_team";
  description = "💾 EXPORT: 팀 전체를 파일 시스템으로 내보내기. 프로젝트/파일/노드를 JSON 파일로 저장.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("team_name", string_prop "팀 이름 (선택)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("output_dir", string_prop "출력 디렉토리 경로 (필수)");
    ("max_depth", string_prop "노드 깊이 (0=파일만, 1=페이지까지, 2+=노드까지, 기본값: 2)");
  ] ["team_id"; "output_dir"];
}

let tool_figma_get_variables : tool_def = {
  name = "figma_get_variables";
  description = "📦 TOKENS: 파일의 디자인 토큰/변수. 색상, 타이포, 간격 등.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["summary"; "raw"; "resolved"] "출력 포맷 (기본값: summary)");
  ] ["file_key"];
}

(** ============== Phase 2: 고급 쿼리 도구 ============== *)

let tool_figma_query : tool_def = {
  name = "figma_query";
  description = "🎛️ CORE: SQL WHERE처럼 조건 필터링. type=FRAME, width_min=300, color=#FF0000 등 조합. 특정 크기 버튼, 특정 색상 요소 찾기. 반환: 매칭 노드 목록 (id, name, bounds).";
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
  description = "🔍 CORE: 파일 내 노드를 텍스트/이름으로 검색. '버튼', 'Header' 등 키워드로 관련 노드 찾기. 반환: node_id, name, type, 좌표 목록. figma_get_node로 상세 조회 연계.";
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
  description = "✅ VERIFY: 통합 비교 도구. mode로 기능 선택: general(노드 비교), batch(Web/Mobile), regions(영역별), elements(색상/박스), evolution(리포트).";
  input_schema = object_schema [
    (* Common *)
    ("mode", enum_prop ["general"; "batch"; "regions"; "elements"; "evolution"] "비교 모드 선택");
    (* general/batch mode params *)
    ("file_key", string_prop "Figma 파일 키 (general/batch 모드)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("node_a_id", string_prop "첫 번째 노드 ID (general 모드)");
    ("node_b_id", string_prop "두 번째 노드 ID (general 모드)");
    ("web_prefix", string_prop "Web 노드 접두사 (batch 모드, 기본: Web)");
    ("mobile_prefix", string_prop "Mobile 노드 접두사 (batch 모드, 기본: Mobile)");
    (* regions mode params *)
    ("image_a", string_prop "기준 이미지 경로 (regions 모드)");
    ("image_b", string_prop "비교 이미지 경로 (regions 모드)");
    ("regions", string_prop "비교 영역 JSON [{name, x, y, width, height}] (regions 모드)");
    ("output_dir", string_prop "결과 저장 디렉토리 (regions 모드)");
    ("generate_diff", bool_prop "차이 이미지 생성 (regions 모드, 기본: true)");
    (* elements mode params *)
    ("type", enum_prop ["color"; "box"; "full"] "비교 타입 (elements 모드)");
    ("color1", string_prop "첫 번째 색상 (elements 모드)");
    ("color2", string_prop "두 번째 색상 (elements 모드)");
    ("box1", string_prop "첫 번째 박스 x,y,w,h (elements 모드)");
    ("box2", string_prop "두 번째 박스 x,y,w,h (elements 모드)");
    (* evolution mode params *)
    ("run_dir", string_prop "Evolution 디렉토리 (evolution 모드, 없으면 목록 반환)");
    ("generate_image", bool_prop "비교 이미지 생성 (evolution 모드, 기본: true)");
  ] ["mode"];
}

let tool_figma_tree : tool_def = {
  name = "figma_tree";
  description = "🌳 CORE: 노드 계층 구조를 ASCII 트리로 시각화. 파일/노드 구조 파악에 필수. style: ascii(기본), indent, compact. max_depth로 깊이 제한. 반환: 트리 문자열 + 노드 수 통계.";
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
  description = "📊 REPORT: 파일 디자인 통계. 색상/폰트/크기/컴포넌트 현황.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("node_id", string_prop "분석 시작 노드 ID (생략시 전체 문서)");
  ] ["file_key"];
}

let tool_figma_export_tokens : tool_def = {
  name = "figma_export_tokens";
  description = "📦 TOKENS: 디자인 토큰 추출. CSS/Tailwind/JSON/SwiftUI/Compose/Flutter/W3C-DTCG/semantic 지원.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["css"; "tailwind"; "json"; "dtcg"; "swiftui"; "compose"; "flutter"; "semantic"] "출력 포맷 (기본값: css). dtcg=W3C Design Tokens (DTCG). semantic=UIFormer 스타일 DSL");
    ("node_id", string_prop "추출 시작 노드 ID (생략시 전체 문서)");
  ] ["file_key"];
}

(** 환경/의존성 점검 도구 *)
let tool_figma_doctor : tool_def = {
  name = "figma_doctor";
  description = "🔧 UTIL: 로컬 의존성 점검. Node/Playwright/ImageMagick 확인.";
  input_schema = object_schema [] [];
}

(** large_result 파일 읽기 *)
let tool_figma_read_large_result : tool_def = {
  name = "figma_read_large_result";
  description = "🔧 UTIL: large_result 파일 읽기. offset/limit로 분할 읽기.";
  input_schema = object_schema [
    ("file_path", string_prop "large_result file_path");
    ("offset", number_prop "읽기 시작 바이트 (기본값: 0)");
    ("limit", number_prop "최대 읽기 바이트 (기본값: 20000)");
  ] ["file_path"];
}

(** 캐시 관리 도구 *)
let tool_figma_cache_stats : tool_def = {
  name = "figma_cache_stats";
  description = "🔧 UTIL: 캐시 통계. L1(메모리)+L2(파일) 엔트리/TTL 정보.";
  input_schema = object_schema [] [];
}

let tool_figma_cache_invalidate : tool_def = {
  name = "figma_cache_invalidate";
  description = "🔧 UTIL: 캐시 무효화. file_key/node_id로 범위 지정.";
  input_schema = object_schema [
    ("file_key", string_prop "무효화할 파일 키 (생략시 전체)");
    ("node_id", string_prop "무효화할 노드 ID (생략시 해당 파일 전체)");
  ] [];
}

(** Code Connect-style component mapping (repo-local) *)
let tool_figma_code_connect : tool_def = {
  name = "figma_code_connect";
  description = "🔗 CODE: Code Connect-style component mapping. mode=validate|index|match|list. 로컬 매핑 JSON을 파싱/검증하고, Figma 컴포넌트/노드를 코드 컴포넌트로 결정론적으로 매칭합니다.";
  input_schema = object_schema [
    ("mode", enum_prop ["validate"; "index"; "match"; "list"] "동작 모드");
    ("path", string_prop "매핑 파일 경로 (옵션, 기본: ./figma-code-connect.json → ./.figma/code-connect.json 검색)");
    ("json", string_prop "인라인 JSON (옵션, path보다 우선)");
    ("index_id", string_prop "index 모드 결과 재사용 (옵션)");
    ("cache_key", string_prop "index 캐시 키 (옵션, 기본: content hash)");
    ("node_id", string_prop "Figma node id (match 모드)");
    ("component_key", string_prop "Figma component key (match 모드)");
    ("name", string_prop "Figma component name (match 모드)");
    ("variant", object_prop "variant properties (match 모드, object: {key: value})");
    ("limit", number_prop "match 결과 개수 (기본: 3)");
  ] ["mode"];
}

(** ============== 카테고리 시스템 (Tool Dive) ============== *)

(** 카테고리별 도구 그룹 *)
type tool_category = {
  name: string;
  description: string;
  tools: string list;
}

let tool_categories = [
  { name = "core";
    description = "File/Node 기본 조회";
    tools = ["get_file"; "get_file_meta"; "get_node"; "get_nodes"; "get_node_bundle";
             "get_node_chunk"; "get_node_summary"; "select_nodes"; "list_screens";
             "tree"; "get_file_versions"; "parse_url"; "get_me"; "query"; "search"] };
  { name = "visual";
    description = "시각 검증 (SSIM, 비교)";
    (* NOTE: compare_elements, compare_regions, evolution_report는 DEPRECATED → figma_compare(mode=...)로 통합 *)
    tools = ["verify_visual"; "image_similarity"; "compare"; "fidelity_loop"; "fidelity_review"] };
  { name = "plugin";
    description = "Figma 플러그인 브릿지";
    tools = ["plugin"; "plugin_connect"; "plugin_use_channel"; "plugin_status";
             "plugin_read_selection"; "plugin_get_node"; "plugin_export_node_image";
             "plugin_get_variables"; "plugin_apply_ops"] };
  { name = "team";
    description = "팀/프로젝트 관리";
    tools = ["list_projects"; "list_files"; "crawl_team"; "team_tree"; "export_team";
             "get_team_components"; "get_team_component_sets"; "get_team_styles"] };
  { name = "export";
    description = "이미지/토큰 내보내기";
    tools = ["export_image"; "export_smart"; "export_tokens"; "get_image_fills"] };
  { name = "components";
    description = "컴포넌트/스타일/변수";
    tools = ["get_file_components"; "get_file_component_sets"; "get_file_styles";
             "get_component"; "get_component_set"; "get_style"; "get_variables"; "code_connect"] };
]

(** 카테고리에서 도구 찾기 *)
let find_tool_in_category category_name tool_name =
  List.find_opt (fun cat -> cat.name = category_name) tool_categories
  |> Option.map (fun cat -> List.mem tool_name cat.tools)
  |> Option.value ~default:false

(** 최상위 유지 도구 (자주 사용) *)
let featured_tool_names = ["codegen"; "doctor"; "stats"; "cache_stats"; "cache_invalidate"; "read_large_result"; "code_connect"; "error_troubleshoot"; "post_comment"; "get_file_comments"]

(** ============== 모든 도구 목록 (내부용) ============== *)

let all_detailed_tools = [
  (* 기존 도구 *)
  tool_figma_codegen;
  tool_figma_get_file;
  tool_figma_get_file_meta;
  tool_figma_list_screens;
  tool_figma_get_node;
  tool_figma_get_node_bundle;
  tool_figma_get_node_summary;
  tool_figma_select_nodes;
  tool_figma_get_node_chunk;
  tool_figma_fidelity_loop;
  tool_figma_image_similarity;
  tool_figma_verify_visual;
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
  (* STRAP 통합: 8개 plugin 도구 → 1개 *)
  tool_figma_plugin;
  (* Phase 1: 탐색 도구 *)
  tool_figma_parse_url;
  tool_figma_get_me;
  tool_figma_list_projects;
  tool_figma_list_files;
  tool_figma_crawl_team;
  tool_figma_team_tree;
  tool_figma_export_team;
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
  (* Code Connect-style component mapping *)
  tool_figma_code_connect;
  (* 캐시 관리 *)
  tool_figma_cache_stats;
  tool_figma_cache_invalidate;
]

(** 카테고리 도구 정의 생성 *)
let make_category_tool cat : tool_def =
  let tool_list = String.concat ", " cat.tools in
  {
    name = "figma_" ^ cat.name;
    description = sprintf "[Category] %s. 도구: %s" cat.description tool_list;
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("tool", `Assoc [
          ("type", `String "string");
          ("description", `String "실행할 도구 이름 (예: verify_visual). 생략시 목록 반환");
        ]);
        ("args", `Assoc [
          ("type", `String "object");
          ("description", `String "도구에 전달할 인자");
        ]);
      ]);
    ];
  }

(** 카테고리 도구들 *)
let category_tools : tool_def list = List.map make_category_tool tool_categories

(** 최상위 유지 도구 필터 *)
let featured_tools : tool_def list =
  List.filter (fun (t: tool_def) ->
    List.exists (fun name -> t.name = "figma_" ^ name) featured_tool_names
  ) all_detailed_tools

(** 공개 도구 목록 (카테고리 + 자주 쓰는 도구) *)
let public_tools : tool_def list = category_tools @ featured_tools

(** 전체 도구 (핸들러용) - 기존 all_tools 호환 *)
let all_tools = all_detailed_tools

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

let hyphenate_node_id value =
  String.map (fun c -> if c = ':' then '-' else c) value

(* Figma API and clients sometimes disagree on ":" vs "-" node IDs.
   Resolve by normalizing both the requested ID and map keys. *)
let find_node_entry (nodes_map : (string * Yojson.Safe.t) list) ~(node_id : string)
  : (string * Yojson.Safe.t) option =
  let target = normalize_node_id node_id in
  let direct_keys = [node_id; target; hyphenate_node_id target] in
  let direct_hit =
    List.find_map (fun key ->
      match List.assoc_opt key nodes_map with
      | Some v -> Some (key, v)
      | None -> None
    ) direct_keys
  in
  match direct_hit with
  | Some hit -> Some hit
  | None ->
      List.find_map (fun (key, value) ->
        if normalize_node_id key = target then Some (key, value) else None
      ) nodes_map

(** ============== 에러 처리 강화 + 모나딕 바인딩 ============== *)

(** 상세 에러 타입 *)
type api_error =
  | NetworkError of string
  | AuthError of string       (* 401/403 *)
  | NotFound of string        (* 404 *)
  | RateLimited of float      (* 429 + retry_after *)
  | ServerError of string     (* 5xx *)
  | ParseError of string
  | TimeoutError of float     (* timeout in seconds *)
  | UnknownError of string

(** 에러를 사용자 친화적 메시지로 변환 *)
let error_to_string = function
  | NetworkError msg -> Printf.sprintf "🌐 Network error: %s" msg
  | AuthError msg -> Printf.sprintf "🔐 Auth error: %s (check FIGMA_TOKEN)" msg
  | NotFound msg -> Printf.sprintf "🔍 Not found: %s" msg
  | RateLimited secs -> Printf.sprintf "⏳ Rate limited - retry after %.0fs" secs
  | ServerError msg -> Printf.sprintf "🔥 Figma server error: %s" msg
  | ParseError msg -> Printf.sprintf "📄 Parse error: %s" msg
  | TimeoutError secs -> Printf.sprintf "⏱️ Timeout after %.0fs" secs
  | UnknownError msg -> Printf.sprintf "❓ Unknown error: %s" msg

(** HTTP 상태 코드에서 에러 분류 *)
let classify_http_error ~status_code ~body =
  match status_code with
  | 401 | 403 -> AuthError body
  | 404 -> NotFound body
  | 429 ->
      (* Rate limit - retry_after 파싱 시도 *)
      let retry_after = try
        Scanf.sscanf body "retry after %f" (fun f -> f)
      with _ -> 60.0 in
      RateLimited retry_after
  | n when n >= 500 -> ServerError (Printf.sprintf "HTTP %d: %s" n body)
  | _ -> UnknownError (Printf.sprintf "HTTP %d: %s" status_code body)

(** Result 모나딕 바인딩 (let* 대용) *)
let ( >>= ) result f = match result with
  | Ok v -> f v
  | Error e -> Error e

let ( >>| ) result f = match result with
  | Ok v -> Ok (f v)
  | Error e -> Error e

(** 안전한 임시 파일 사용 (Common.protect 패턴) *)
let with_temp_file ~prefix ~suffix f =
  let path = Printf.sprintf "/tmp/figma-visual/%s_%d_%d%s"
    prefix
    (int_of_float (Unix.gettimeofday () *. 1000.0))
    (Random.int 100000)
    suffix
  in
  Common.protect
    ~module_name:"mcp_tools"
    ~finally_label:"Unix.unlink"
    ~finally:(fun () -> Unix.unlink path)
    (fun () -> f path)

(** 디버그 정보가 포함된 에러 JSON 생성 *)
let make_error_json ~operation ~error ?(debug_info=[]) () =
  let timestamp = Unix.gettimeofday () in
  let base = [
    ("error", `Bool true);
    ("operation", `String operation);
    ("message", `String (error_to_string error));
    ("timestamp", `Float timestamp);
    ("timestamp_iso", `String (
      let tm = Unix.localtime timestamp in
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
        (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec));
  ] in
  let debug = if debug_info = [] then [] else [("debug", `Assoc debug_info)] in
  `Assoc (base @ debug)

(** ============== 핸들러 런타임 조회 (상호 참조 해결) ============== *)

(** 핸들러 레지스트리 - lazy initialization으로 forward reference 해결 *)
let handler_registry : (string, Yojson.Safe.t -> (Yojson.Safe.t, string) result) Hashtbl.t =
  Hashtbl.create 64

(** 핸들러 등록 (파일 끝에서 호출) *)
let register_handler name handler =
  Hashtbl.replace handler_registry name handler

(** 런타임에 다른 핸들러 호출 (forward reference 가능) *)
let call_handler name args =
  match Hashtbl.find_opt handler_registry name with
  | Some handler -> handler args
  | None -> Error (Printf.sprintf "Handler not found: %s" name)

(** ============== 기존 헬퍼 함수 ============== *)

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

(** Get int with default value *)
let get_int_or key default json =
  match get_int key json with
  | Some i -> i
  | None -> default

(** Get int with default, requiring value > min (for positive constraints) *)
let get_int_positive ?(min=0) key default json =
  match get_int key json with
  | Some i when i > min -> i
  | _ -> default

(** Get int with default, requiring value >= min (for non-negative constraints) *)
let get_int_nonneg ?(min=0) key default json =
  match get_int key json with
  | Some i when i >= min -> i
  | _ -> default

let get_float_or key default json =
  match get_float key json with
  | Some f -> f
  | None -> default

let get_bool_or key default json =
  match get_bool key json with
  | Some b -> b
  | None -> default

(** Token resolution with environment variable expansion.
    Supports: "env:FIGMA_TOKEN" syntax to read from environment.
    Priority: 1) explicit "token" (with env: expansion) 2) FIGMA_TOKEN env var *)
let resolve_token args =
  match get_string "token" args with
  | Some t when String.length t > 0 ->
    (* Handle "env:VAR_NAME" syntax *)
    if String.length t > 4 && String.sub t 0 4 = "env:" then
      let var_name = String.sub t 4 (String.length t - 4) |> String.trim in
      (* Do NOT allow arbitrary env var reads from tool args.
         Only support the documented "env:FIGMA_TOKEN". *)
      if var_name = "FIGMA_TOKEN" then
        Sys.getenv_opt "FIGMA_TOKEN"
      else
        None
    else
      Some t
  | _ -> Sys.getenv_opt "FIGMA_TOKEN"

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

(** Eio context for pure Eio handlers (set by mcp_protocol_eio at startup) *)

(** Existential wrapper for clock to hide the type parameter *)
type any_clock = Clock : _ Eio.Time.clock -> any_clock

(** Existential wrapper for net to hide the type parameter *)
type any_net = Net : _ Eio.Net.t -> any_net

type eio_context = {
  sw: Eio.Switch.t;
  net: any_net;
  clock: any_clock;
  client: Figma_api_eio.client;
  domain: Domain.id;
}

let eio_context_key : eio_context option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

let get_eio_context () = Domain.DLS.get eio_context_key

let install_eio_context ctx = Domain.DLS.set eio_context_key (Some ctx)

(** Set Eio context from server startup *)
let set_eio_context ~sw ~net ~clock ~client =
  let ctx = {
    sw;
    net = Net net;
    clock = Clock clock;
    client;
    domain = Domain.self ();
  } in
  install_eio_context ctx;
  ctx

let resolve_channel_id args =
  match get_string "channel_id" args with
  | Some id -> Ok id
  | None ->
      (match Figma_plugin_bridge.get_default_channel () with
       | Some id -> Ok id
       | None -> Error "Missing channel_id. Run figma_plugin_connect or figma_plugin_use_channel.")

let plugin_wait ~channel_id ~command_id ~timeout_ms =
  match get_eio_context () with
  | Some ctx ->
      let (Clock clock) = ctx.clock in
      (match Figma_plugin_bridge.wait_for_result_with_sleep
               ~sleep:(Eio.Time.sleep clock)
               ~channel_id
               ~command_id
               ~timeout_ms with
       | Some result -> Ok result
       | None -> Error "Plugin timeout waiting for response")
  | None ->
      (match Figma_plugin_bridge.wait_for_result ~channel_id ~command_id ~timeout_ms with
       | Some result -> Ok result
       | None -> Error "Plugin timeout waiting for response")

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

let default_asset_dir () = Figma_config.Asset.dir

let default_compare_dir () =
  default_asset_dir () ^ "/compare"

let sanitize_node_id id =
  let buf = Bytes.of_string id in
  Bytes.iteri (fun i c ->
    if c = ':' then Bytes.set buf i '_'
  ) buf;
  Bytes.to_string buf

let sanitize_file_key key =
  let buf = Buffer.create (String.length key) in
  String.iter (fun c ->
    let is_safe =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c = '-' || c = '_'
    in
    Buffer.add_char buf (if is_safe then c else '-')
  ) key;
  let sanitized = String.trim (Buffer.contents buf) in
  if sanitized = "" then "unknown" else sanitized

let strip_query url =
  match String.index_opt url '?' with
  | Some i -> String.sub url 0 i
  | None -> url

let file_ext_from_url url =
  let base = strip_query url |> Filename.basename in
  let ext = Filename.extension base in
  if ext = "" then ".img" else ext

let is_http_url s =
  match Uri.scheme (Uri.of_string s) with
  | Some ("http" | "https") -> true
  | _ -> false

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
        save_dir (sanitize_file_key file_key) (sanitize_node_id id) ext in
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

(** ============== Generic Plugin Handler Builders ============== *)

let truncate_string ?(max_len=2000) s =
  if String.length s <= max_len then s
  else (String.sub s 0 max_len) ^ "…"

let plugin_error_message payload =
  let list_hd_opt = function
    | [] -> None
    | x :: _ -> Some x
  in
  let first_string_in_list = function
    | `List items ->
        items
        |> List.filter_map (function `String s -> Some s | _ -> None)
        |> list_hd_opt
    | _ -> None
  in
  let assoc_string key fields =
    match List.assoc_opt key fields with
    | Some (`String s) -> Some s
    | Some v -> (
        match first_string_in_list v with
        | Some s -> Some s
        | None -> None)
    | None -> None
  in
  let raw =
    match payload with
    | `String s -> s
    | `Assoc fields -> (
        match assoc_string "error" fields with
        | Some s -> s
        | None -> (
            match assoc_string "message" fields with
            | Some s -> s
            | None -> Yojson.Safe.to_string payload))
    | _ -> Yojson.Safe.to_string payload
  in
  truncate_string raw

(** Execute plugin command and return result *)
let plugin_exec ~channel_id ~name ~payload ~timeout_ms =
  let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name ~payload in
  match plugin_wait ~channel_id ~command_id ~timeout_ms with
  | Error err -> Error err
  | Ok result ->
      if result.ok then
        Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload))
      else
        Error (Printf.sprintf "Plugin error: %s" (plugin_error_message result.payload))

(** Simple handler - just needs channel_id *)
let plugin_simple ~name ?(default_timeout=10000) ~build_payload args =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:default_timeout in
      let payload = build_payload args in
      plugin_exec ~channel_id ~name ~payload ~timeout_ms

(** Node handler - needs node_id + channel_id *)
let plugin_node ~name ?(default_timeout=10000) ~build_payload args =
  match (resolve_node_id args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id (or url)"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:default_timeout in
      let payload = build_payload node_id args in
      plugin_exec ~channel_id ~name ~payload ~timeout_ms

(** Nodes handler - needs node_ids array + channel_id *)
let plugin_nodes ~name ?(default_timeout=10000) ~build_payload args =
  match (get_string_list "node_ids" args, resolve_channel_id args) with
  | (None, _) | (Some [], _) -> Error "Missing required parameter: node_ids"
  | (_, Error msg) -> Error msg
  | (Some ids, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:default_timeout in
      let payload = build_payload ids args in
      plugin_exec ~channel_id ~name ~payload ~timeout_ms

(** Custom handler - for special cases requiring custom validation *)
let plugin_custom ~name ?(default_timeout=10000) ~validate ~build_payload args =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      (match validate args with
       | Error msg -> Error msg
       | Ok validated ->
           let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:default_timeout in
           let payload = build_payload validated args in
           plugin_exec ~channel_id ~name ~payload ~timeout_ms)

let command_output cmd argv =
  match Safe_exec.run_stdout ~timeout_ms:5000 ~output_limit:(64 * 1024) cmd argv with
  | Ok out -> String.trim out
  | Error _ -> ""

let has_command name =
  Safe_exec.command_exists name

let has_node_module name =
  match Safe_exec.run ~timeout_ms:5000 ~output_limit:(32 * 1024) "node" [| "node"; "-e"; Printf.sprintf "require('%s')" name |] with
  | Ok out ->
      (match out.status with
       | Unix.WEXITED 0 -> true
       | _ -> false)
  | Error _ -> false

let mkdir_p path =
  let rec loop p =
    if p = Filename.dirname p then ()
    else if Sys.file_exists p then ()
    else begin
      loop (Filename.dirname p);
      try Unix.mkdir p 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  loop path

let normalize_path path =
  try Some (Unix.realpath path) with _ -> None

let is_under_dir ~dir path =
  match (normalize_path dir, normalize_path path) with
  | (Some dir_norm, Some path_norm) ->
      let prefix = if String.ends_with ~suffix:"/" dir_norm then dir_norm else dir_norm ^ "/" in
      path_norm = dir_norm || String.starts_with ~prefix path_norm
  | _ -> false

(** figma_get_file 핸들러 *)
let handle_get_file args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
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
  let token = resolve_token args in
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
  let token = resolve_token args in

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
  let token = resolve_token args in
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
                let node_count = Large_response.count_nodes_json node in
                let prefix = Printf.sprintf "node_%s" (sanitize_node_id node_id) in
                (match process_json_string ~format node_str with
                 | Ok result ->
                     Ok (Large_response.wrap_dsl_with_warning ~prefix ~format ~node_count result)
                 | Error msg -> Error msg)
            | None -> Error (sprintf "Node not found: %s" node_id))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_get_node_bundle 핸들러 *)
let handle_get_node_bundle args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
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
      let node_id = normalize_node_id node_id in
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
           let node_lookup =
             match member "nodes" json with
             | Some (`Assoc nodes_map) ->
                 (match find_node_entry nodes_map ~node_id with
                  | Some (node_key, node_entry) ->
                      (match member "document" node_entry with
                       | Some doc -> Ok (node_key, doc)
                       | None -> Error (sprintf "Node %s found but document is null" node_id))
                  | None -> 
                      let keys = List.map fst nodes_map in
                      let keys_str = if keys = [] then "none" else String.concat ", " keys in
                      Error (sprintf "Node %s not found. Available: [%s]" node_id keys_str))
             | Some _ -> Error "API returned nodes in unexpected format"
             | None -> Error "API response missing 'nodes' field"
           in
           (match node_lookup with
            | Error msg -> Error msg
            | Ok (node_key, node) ->
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
                          ~format:image_format ~scale
                          ?use_absolute_bounds ?version () with
                  | Ok img_json ->
                      let url =
                        match member "images" img_json with
                        | Some (`Assoc img_map) ->
                            (match List.assoc_opt node_key img_map with
                             | Some (`String u) -> u
                             | _ -> "No image URL returned")
                        | _ -> "No images returned"
                      in
                      if download then
                        if is_http_url url then
                          let path = Printf.sprintf "%s/%s/%s.%s"
                            save_dir (sanitize_file_key file_key) (sanitize_node_id node_id) image_format in
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
  let token = resolve_token args in
  let max_children = get_int_positive "max_children" 50 args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      let node_id = normalize_node_id node_id in
      (* 최소 depth=1로 자식만 가져옴 *)
      (match Figma_effects.Perform.get_nodes ~token ~file_key ~node_ids:[node_id] ~depth:1 ?version () with
       | Error err -> Error (Printf.sprintf "Figma API error: %s" err)
       | Ok nodes_json ->
           let module U = Yojson.Safe.Util in
           let nodes_map =
             match U.member "nodes" nodes_json with
             | `Assoc map -> map
             | _ -> []
           in
           let node_data =
             match find_node_entry nodes_map ~node_id with
             | Some (_key, node_entry) ->
                 (match U.member "document" node_entry with
                  | `Null -> None
                  | doc -> Some doc)
             | None -> None
           in
           (match node_data with
            | None ->
                (* Better error message with available keys *)
                let available_keys = List.map fst nodes_map in
                let keys_str = if available_keys = [] then "none" 
                  else String.concat ", " available_keys in
                Error (Printf.sprintf "Node %s not found in file %s. Available keys: [%s]" 
                  node_id file_key keys_str)
            | Some node_data ->
                let children =
                  match U.member "children" node_data with
                  | `List xs -> xs
                  | _ -> []
                in
                let children_count = List.length children in
                let children_summary =
                  children
                  |> List.mapi (fun i child ->
                      if i >= max_children then None
                      else
                        let id =
                          match U.member "id" child with
                          | `String s -> s
                          | _ -> ""
                        in
                        let name =
                          match U.member "name" child with
                          | `String s -> s
                          | _ -> ""
                        in
                        let typ =
                          match U.member "type" child with
                          | `String s -> s
                          | _ -> "UNKNOWN"
                        in
                        let sub_children =
                          match U.member "children" child with
                          | `List xs -> List.length xs
                          | _ -> 0
                        in
                        Some
                          (`Assoc
                            [
                              ("id", `String id);
                              ("name", `String name);
                              ("type", `String typ);
                              ("children_count", `Int sub_children);
                            ]))
                  |> List.filter_map Fun.id
                in
                let node_name =
                  match U.member "name" node_data with
                  | `String s -> s
                  | _ -> ""
                in
                let node_type =
                  match U.member "type" node_data with
                  | `String s -> s
                  | _ -> "UNKNOWN"
                in
                Ok
                  (`Assoc
                    [
                      ("node_id", `String node_id);
                      ("name", `String node_name);
                      ("type", `String node_type);
                      ("children_count", `Int children_count);
                      ("children", `List children_summary);
                      ("truncated", `Bool (children_count > max_children));
                      ( "hint",
                        `String
                          "Use figma_get_node_chunk for progressive loading of specific depth ranges" );
                    ])))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_select_nodes 핸들러 - 점수 기반 후보 선별 *)
let handle_select_nodes args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let max_summary_depth = 6 in
  let raw_summary_depth = match get_int "summary_depth" args with Some d -> d | _ -> 1 in
  let summary_depth =
    if raw_summary_depth < 0 then 1 else min raw_summary_depth max_summary_depth
  in
  let preview = get_bool_or "preview" true args in
  let preview_format = get_string_or "preview_format" "png" args in
  let raw_preview_scale = get_float_or "preview_scale" 1.0 args in
  let preview_scale =
    if raw_preview_scale < 0.01 then 0.01
    else if raw_preview_scale > 4.0 then 4.0
    else raw_preview_scale
  in
  let layout_only = get_bool_or "layout_only" false args in
  let auto_layout_only = get_bool_or "auto_layout_only" false args in
  let raw_text_mode = get_string_or "text_mode" "include" args in
  let text_mode =
    match raw_text_mode with
    | "include" | "exclude" | "only" -> raw_text_mode
    | _ -> "include"
  in
  let score_threshold = get_float_or "score_threshold" 2.0 args in
  let max_parents = get_int_positive "max_parents" 8 args in
  let notes_limit = get_int_positive "notes_limit" 50 args in
  let excluded_limit = get_int_positive "excluded_limit" 50 args in
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
      if raw_summary_depth <> summary_depth then
        warnings := Printf.sprintf "summary_depth clamped to %d" summary_depth :: !warnings;
      if raw_preview_scale <> preview_scale then
        warnings := "preview_scale clamped to 0.01-4.0" :: !warnings;

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
                     ("scale", `Float preview_scale);
                   ]
               | None ->
                   `Assoc [
                     ("status", `String "missing");
                     ("format", `String preview_format);
                     ("scale", `Float preview_scale);
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
  let token = resolve_token args in
  let depth_start = get_int_nonneg "depth_start" 0 args in
  let depth_end = get_int_nonneg "depth_end" 2 args in
  let format = get_string_or "format" "fidelity" args in
  let max_children = get_int "max_children" args in
  let warn_large = get_bool_or "warn_large" true args in
  let warn_threshold = get_int "warn_threshold" args |> Option.value ~default:500 in
  let error_on_large = get_bool_or "error_on_large" false args in
  let auto_trim_children = get_bool_or "auto_trim_children" false args in
  let auto_trim_limit = get_int "auto_trim_limit" args |> Option.value ~default:200 in
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

             let root_children_count =
               try (node_data |> member "children" |> to_list |> List.length)
               with _ -> 0
             in

             let effective_max_children =
               match max_children, auto_trim_children with
               | Some limit, _ -> Some limit
               | None, true -> Some (max 0 auto_trim_limit)
               | None, false -> None
             in

             let warnings = ref [] in
             let add_warning msg = warnings := msg :: !warnings in
             let is_large =
               warn_large && effective_max_children = None && root_children_count > warn_threshold
             in
             let large_error =
               if error_on_large && is_large then
                 Some (Printf.sprintf
                   "Large node %s: %d children at root (warn_threshold=%d). Set max_children/auto_trim_children or use figma_get_node_chunk + figma_read_large_result."
                   node_id root_children_count warn_threshold)
               else
                 None
             in
             (match effective_max_children, auto_trim_children with
              | Some limit, true when max_children = None ->
                  add_warning (Printf.sprintf "auto_trim_children applied: max_children=%d" limit)
              | _ -> ());
             (match warn_large, root_children_count, effective_max_children with
              | true, count, None when count > warn_threshold ->
                  add_warning (Printf.sprintf
                    "Large node %s: %d children at root (warn_threshold=%d). Consider max_children/auto_trim_children or figma_get_node_chunk + figma_read_large_result."
                    node_id count warn_threshold)
              | _ -> ());

             let take_n n lst =
               let rec loop acc i = function
                 | [] -> List.rev acc
                 | _ when i >= n -> List.rev acc
                 | x :: xs -> loop (x :: acc) (i + 1) xs
               in
               loop [] 0 lst
             in

             let trim_children children =
               match effective_max_children with
               | Some limit when limit >= 0 ->
                   let total = List.length children in
                   if total > limit then
                     (take_n limit children, Some (total - limit))
                   else
                     (children, None)
               | _ -> (children, None)
             in

             let append_truncated assoc truncated =
               match truncated with
               | Some n -> assoc @ [("_truncated_children", `Int n)]
               | None -> assoc
             in

             (* null-safe children 추출 *)
             let get_children_safe json =
               match json |> member "children" with
               | `Null -> []
               | `List lst -> lst
               | _ -> []
             in

             (* 깊이 범위에 따라 필터링하는 재귀 함수 *)
             let rec filter_by_depth current_depth json =
               if current_depth < depth_start then
                 (* 시작 깊이 미만: 자식만 재귀 처리 *)
                 let children = get_children_safe json in
                 let children, truncated = trim_children children in
                 let filtered_children = List.filter_map (fun c ->
                     let result = filter_by_depth (current_depth + 1) c in
                     if result = `Null then None else Some result
                   ) children
                 in
                 if filtered_children = [] then `Null
                 else
                   let assoc = to_assoc json in
                   let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                   let assoc = without_children @ [("children", `List filtered_children)] in
                   `Assoc (append_truncated assoc truncated)
               else if current_depth > depth_end then
                 (* 종료 깊이 초과: 자식 제거 *)
                 let assoc = to_assoc json in
                 let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                 let children_count = get_children_safe json |> List.length in
                 `Assoc (without_children @ [("_truncated_children", `Int children_count)])
               else
                 (* 범위 내: 자식 재귀 처리 *)
                 let children = get_children_safe json in
                 let children, truncated = trim_children children in
                 let filtered_children = List.map (fun c -> filter_by_depth (current_depth + 1) c) children in
                 let assoc = to_assoc json in
                 let without_children = List.filter (fun (k, _) -> k <> "children") assoc in
                 let assoc = without_children @ [("children", `List filtered_children)] in
                 `Assoc (append_truncated assoc truncated)
             in

             match large_error with
             | Some msg -> Error msg
             | None ->
                 let filtered = filter_by_depth 0 node_data in
                 let base =
                   let styles =
                     if include_styles then
                       match Figma_effects.Perform.get_file_styles ~token ~file_key with
                       | Ok json -> json
                       | Error err -> `Assoc [("error", `String err)]
                     else
                       `Null
                   in
                   let filtered_str = Yojson.Safe.to_string filtered in
                   match process_json_string ~format filtered_str with
                   | Ok dsl ->
                       `Assoc [
                         ("type", `String "text");
                         ("text", `String dsl);
                         ("depth_range", `String (Printf.sprintf "%d-%d" depth_start depth_end));
                         ("format", `String format);
                         ("styles", styles);
                       ]
                   | Error msg ->
                       `Assoc [
                         ("error", `String msg);
                         ("node", filtered);
                         ("depth_range", `String (Printf.sprintf "%d-%d" depth_start depth_end));
                         ("styles", styles);
                       ]
                 in
                 let result =
                   let warning =
                     match !warnings with
                     | [] -> None
                     | msgs -> Some (String.concat " | " (List.rev msgs))
                   in
                   match warning with
                   | Some msg ->
                       (match base with
                        | `Assoc fields -> `Assoc (fields @ [("warning", `String msg)])
                        | _ -> base)
                   | None -> base
                 in

                 (* Large Response Handler 적용 *)
                 let result_str = Yojson.Safe.pretty_to_string result in
                 let prefix = Printf.sprintf "chunk_%s_%d_%d" (sanitize_node_id node_id) depth_start depth_end in
                 Ok (Large_response.wrap_string_result ~prefix ~format result_str))))
  | _ -> Error "Missing required parameters: file_key/node_id or url, token"

(** figma_fidelity_loop 핸들러 *)
let handle_fidelity_loop args : (Yojson.Safe.t, string) result =
  let (file_key, node_id) = resolve_file_key_node_id args in
  let token = resolve_token args in
  let format = get_string_or "format" "fidelity" args in
  let target_score = get_float_or "target_score" 0.92 args in
  let start_depth = get_int_positive "start_depth" 4 args in
  let depth_step = get_int_positive "depth_step" 4 args in
  let max_depth = get_int_positive "max_depth" 20 args in
  let max_attempts = get_int_positive "max_attempts" 4 args in
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
  let plugin_depth = get_int_positive "plugin_depth" 6 args in
  let plugin_timeout_ms = get_int "plugin_timeout_ms" args |> Option.value ~default:20000 in
  let summary_only = get_bool_or "summary_only" false args in
  let max_inline_bytes =
    match get_int "max_inline_bytes" args with
    | Some n when n > 0 -> n
    | _ -> Large_response.max_inline_size
  in

  let clamp_score v =
    if v < 0.0 then 0.0 else if v > 1.0 then 1.0 else v
  in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      if format <> "fidelity" then
        Error "figma_fidelity_loop only supports format=fidelity"
      else
        let node_id = normalize_node_id node_id in
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
                let node_lookup =
                  match member "nodes" json with
                  | Some (`Assoc nodes_map) ->
                      (match find_node_entry nodes_map ~node_id with
                       | Some (_node_key, node_entry) ->
                           (match member "document" node_entry with
                            | Some doc -> Some doc
                            | None -> None)
                       | None -> None)
                  | _ -> None
                in
                (match node_lookup with
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
        let attempt_overall entry =
          match member "fidelity" entry with
          | Some fidelity ->
              (match member "overall" fidelity with
               | Some (`Float f) -> Some f
               | Some (`Int i) -> Some (float_of_int i)
               | _ -> None)
          | None -> None
        in
        let summarize_attempt entry =
          let overall_json =
            match attempt_overall entry with
            | Some f -> `Float f
            | None -> `Null
          in
          let missing_total =
            match member "fidelity" entry with
            | Some fidelity ->
                (match member "missing_total" fidelity with
                 | Some v -> v
                 | None -> `Null)
            | None -> `Null
          in
          `Assoc [
            ("attempt", member "attempt" entry |> Option.value ~default:`Null);
            ("depth", member "depth" entry |> Option.value ~default:`Null);
            ("fidelity", `Assoc [
              ("overall", overall_json);
              ("missing_total", missing_total);
            ]);
            ("early_stop", member "early_stop" entry |> Option.value ~default:`Null);
            ("error", member "error" entry |> Option.value ~default:`Null);
          ]
        in
        let summarize_best payload =
          match payload with
          | `Assoc _ ->
              let overall_json =
                match member "fidelity" payload with
                | Some fidelity ->
                    (match member "overall" fidelity with
                     | Some (`Float f) -> `Float f
                     | Some (`Int i) -> `Float (float_of_int i)
                     | _ -> `Null)
                | None -> `Null
              in
              let missing_total =
                match member "fidelity" payload with
                | Some fidelity ->
                    (match member "missing_total" fidelity with
                     | Some v -> v
                     | None -> `Null)
                | None -> `Null
              in
              `Assoc [
                ("depth", member "depth" payload |> Option.value ~default:`Null);
                ("fidelity", `Assoc [
                  ("overall", overall_json);
                  ("missing_total", missing_total);
                ]);
              ]
          | _ -> `Null
        in
        let attempts_list = List.rev attempts in
        let result = `Assoc [
          ("target_score", `Float target_score);
          ("early_stop", early_stop_summary);
          ("best_score", `Float best_score);
          ("achieved", `Bool (best_score >= target_score));
          ("best", best_payload);
          ("attempts", `List attempts_list);
          ("file_meta", file_meta);
          ("variables", variables);
          ("variables_source", variables_source);
          ("plugin_variables", plugin_variables);
          ("image_fills", image_fills);
          ("plugin_snapshot", plugin_snapshot);
        ] in
        let full_str = Yojson.Safe.pretty_to_string result in
        let full_size = String.length full_str in
        let prefix = Printf.sprintf "fidelity_%s" (sanitize_node_id node_id) in
        let needs_summary = summary_only || full_size > max_inline_bytes in
        if needs_summary then
          let summary_json = `Assoc [
            ("target_score", `Float target_score);
            ("early_stop", early_stop_summary);
            ("best_score", `Float best_score);
            ("achieved", `Bool (best_score >= target_score));
            ("best", summarize_best best_payload);
            ("attempts", `List (List.map summarize_attempt attempts_list));
            ("options", `Assoc [
              ("include_meta", `Bool include_meta);
              ("include_variables", `Bool include_variables);
              ("include_image_fills", `Bool include_image_fills);
              ("include_plugin", `Bool include_plugin);
            ]);
            ("full_result_size_bytes", `Int full_size);
          ] in
          if full_size > max_inline_bytes then
            let filepath = Large_response.save_to_file ~prefix full_str in
            let large_meta = [
              ("status", `String "large_result");
              ("file_path", `String filepath);
              ("size_bytes", `Int full_size);
              ("size_human", `String (Large_response.human_size full_size));
              ("format", `String format);
              ("ttl_seconds", `Int Large_response.response_ttl);
              ("hint", `String "Full result saved to file due to size. Use figma_read_large_result.");
            ] in
            let summary_content = make_text_content (Yojson.Safe.pretty_to_string summary_json) in
            (match summary_content with
             | `Assoc fields -> Ok (`Assoc (fields @ large_meta))
             | _ -> Ok summary_content)
          else
            Ok (make_text_content (Yojson.Safe.pretty_to_string summary_json))
        else
          Ok (Large_response.wrap_string_result ~prefix ~format full_str)
  | _ -> Error "Missing required parameters: file_key, node_id, token"

(** figma_image_similarity 핸들러 *)
let handle_image_similarity args : (Yojson.Safe.t, string) result =
  let format = get_string_or "format" "png" args in
  let start_scale = get_float_or "start_scale" 1.0 args in
  let max_scale = get_float_or "max_scale" start_scale args in
  let scale_step = get_float_or "scale_step" 1.0 args in
  let target_ssim = get_float "target_ssim" args in
  let use_absolute_bounds = get_bool "use_absolute_bounds" args in
  let version = get_string "version" args in
  let save_dir = get_string_or "save_dir" (default_compare_dir ()) args in

  let clamp_scale s = max 0.01 (min 4.0 s) in

  match (get_string "file_key" args, get_string "node_a_id" args, get_string "node_b_id" args, resolve_token args) with
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
                 let path_a = Printf.sprintf "%s/%s/%s__%.2f.%s"
                   save_dir (sanitize_file_key file_key) (sanitize_node_id node_a_id) scale format in
                 let path_b = Printf.sprintf "%s/%s/%s__%.2f.%s"
                   save_dir (sanitize_file_key file_key) (sanitize_node_id node_b_id) scale format in
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
                                  ("scale", `Float scale);
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
            `Assoc [("scale", `Float scale); ("error", `String err)]) :: attempts
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
            loop (scale +. scale_step) best attempts
      in
      let (best, attempts) = loop start_scale None [] in
      let (best_score, best_payload) =
        match best with
        | Some (score, payload) -> (score, payload)
        | None -> (0.0, `Null)
      in
      let result : Yojson.Safe.t = `Assoc [
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
  let token = resolve_token args in
  let html = get_string "html" args in
  let html_screenshot = get_string "html_screenshot" args in
  let target_ssim = get_float_or "target_ssim" 0.95 args in
  let max_iterations = get_int_positive "max_iterations" 3 args in
  let width = get_int_positive "width" 375 args in
  let height = get_int_positive "height" 812 args in
  let version = get_string "version" args in

  match (file_key, node_id, token) with
  | (Some file_key, Some node_id, Some token) ->
      (* 1. Figma에서 노드 PNG 내보내기 *)
      (* Use unique temp paths to avoid cross-run collisions. *)
      let figma_png_path =
        Visual_verifier.temp_file
          ~prefix:(Printf.sprintf "figma_%s_%s" (sanitize_file_key file_key) (sanitize_node_id node_id))
          ~ext:"png"
      in
      (match Figma_effects.Perform.get_images ~token ~file_key
              ~node_ids:[node_id] ~format:"png" ~scale:1.0 ?version () with
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
                       ?html_png_provided:html_screenshot
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
  let allowed_output_base = "/tmp/figma-evolution" in

  let starts_with ~prefix s =
    let lp = String.length prefix in
    String.length s >= lp && String.sub s 0 lp = prefix
  in

  let validate_output_dir dir =
    let dir = String.trim dir in
    if dir = "" then Error "output_dir cannot be empty"
    else if dir.[0] <> '/' then Error "output_dir must be an absolute path"
    else
      let segments =
        String.split_on_char '/' dir |> List.filter (fun s -> s <> "")
      in
      if List.exists (fun s -> s = "..") segments then
        Error "output_dir must not contain '..'"
      else if dir = allowed_output_base
              || starts_with ~prefix:(allowed_output_base ^ "/") dir then
        Ok dir
      else
        Error (Printf.sprintf "output_dir must be under %s" allowed_output_base)
  in

  let is_safe_region_name name =
    let len = String.length name in
    if len = 0 || len > 64 then false
    else
      let is_ok = function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' | '.' -> true
        | _ -> false
      in
      let rec loop i =
        if i = len then true
        else if is_ok name.[i] then loop (i + 1)
        else false
      in
      name <> "." && name <> ".." && loop 0
  in

  let output_dir_raw = get_string_or "output_dir" "/tmp/figma-evolution/regions" args in
  match validate_output_dir output_dir_raw with
  | Error e -> Error e
  | Ok output_dir ->
    let generate_diff = get_bool_or "generate_diff" true args in
    let trim = String.trim in
    let split_csv s =
      s
      |> String.split_on_char ','
      |> List.map trim
      |> List.filter (fun x -> x <> "")
    in
    let normalize_dir_prefix path =
      let p = trim path in
      if p = "" then None
      else
        let rp = try Unix.realpath p with _ -> p in
        if rp = "/" then Some rp
        else if String.ends_with ~suffix:"/" rp then Some rp
        else Some (rp ^ "/")
    in
    let is_under_dir ~dir_prefix path =
      if dir_prefix = "/" then true
      else
        let lp = String.length dir_prefix in
        String.length path >= lp && String.sub path 0 lp = dir_prefix
    in
    let roots =
      match Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_ROOTS" with
      | None -> []
      | Some v -> split_csv v
    in
    let max_bytes_default = 50 * 1024 * 1024 in
    let max_bytes =
      match Sys.getenv_opt "FIGMA_MCP_COMPARE_IMAGE_MAX_BYTES" with
      | None -> max_bytes_default
      | Some v ->
          (try int_of_string (trim v) with _ -> max_bytes_default)
    in
    let validate_png_path ~label path =
      if trim path = "" then
        Error (Printf.sprintf "%s path required" label)
      else if not (Sys.file_exists path) then
        Error (Printf.sprintf "%s image not found" label)
      else
        let lower = String.lowercase_ascii path in
        if not (String.ends_with ~suffix:".png" lower) then
          Error (Printf.sprintf "%s must be a .png file" label)
        else
          let st =
            try Ok (Unix.stat path)
            with Unix.Unix_error (e, _, _) ->
              Error (Printf.sprintf "Failed to stat %s: %s" label (Unix.error_message e))
          in
          match st with
          | Error e -> Error e
          | Ok st ->
              if st.Unix.st_kind <> Unix.S_REG then
                Error (Printf.sprintf "%s must be a regular file" label)
              else if st.Unix.st_size > max_bytes then
                Error (Printf.sprintf "%s image too large" label)
              else
                let rp =
                  try Ok (Unix.realpath path)
                  with _ -> Error (Printf.sprintf "Failed to resolve %s path" label)
                in
                match rp with
                | Error e -> Error e
                | Ok rp ->
                    let prefixes =
                      roots
                      |> List.filter_map normalize_dir_prefix
                    in
                    if prefixes = [] then Ok rp
                    else if List.exists (fun dir_prefix -> is_under_dir ~dir_prefix rp) prefixes then Ok rp
                    else
                      Error (Printf.sprintf "%s path not allowed (set FIGMA_MCP_COMPARE_IMAGE_ROOTS)" label)
    in

    match (get_string "image_a" args, get_string "image_b" args, get_string "regions" args) with
    | (Some image_a, Some image_b, Some regions_json) -> (
        match validate_png_path ~label:"image_a" image_a with
        | Error e -> Error e
        | Ok image_a ->
            match validate_png_path ~label:"image_b" image_b with
            | Error e -> Error e
            | Ok image_b ->
                (* regions JSON 파싱 *)
                let regions_result =
                  try
                    let json = Yojson.Safe.from_string regions_json in
                    match json with
                    | `List items ->
                        let open Yojson.Safe.Util in
                        let rec loop acc = function
                          | [] -> Ok (List.rev acc)
                          | item :: rest ->
                              try
                                let name = item |> member "name" |> to_string in
                                let x = item |> member "x" |> to_int in
                                let y = item |> member "y" |> to_int in
                                let width = item |> member "width" |> to_int in
                                let height = item |> member "height" |> to_int in
                                if not (is_safe_region_name name) then
                                  Error (Printf.sprintf "Invalid region name: %s" name)
                                else if x < 0 || y < 0 || width <= 0 || height <= 0 then
                                  Error (Printf.sprintf "Invalid region bounds for %s" name)
                                else
                                  loop ((name, x, y, width, height) :: acc) rest
                              with _ ->
                                Error "Invalid regions JSON format. Expected: [{name, x, y, width, height}, ...]"
                        in
                        loop [] items
                    | _ -> Error "Invalid regions JSON format. Expected: [{name, x, y, width, height}, ...]"
                  with _ -> Error "Invalid regions JSON format. Expected: [{name, x, y, width, height}, ...]"
                in

                (match regions_result with
                | Error e -> Error e
                | Ok regions ->
                    if regions = [] then
                      Error "Invalid regions JSON format. Expected: [{name, x, y, width, height}, ...]"
                    else begin
                      (* 디렉토리 생성 *)
                      mkdir_p output_dir;

                      (* 각 영역별 SSIM 계산 *)
                      let compare_region (name, x, y, w, h) =
                        let crop_a = Filename.concat output_dir (Printf.sprintf "figma_%s.png" name) in
                        let crop_b = Filename.concat output_dir (Printf.sprintf "html_%s.png" name) in

                        (* ImageMagick으로 영역 crop *)
                        let args_a = [| "magick"; image_a; "-crop"; Printf.sprintf "%dx%d+%d+%d" w h x y; "+repage"; crop_a |] in
                        let args_b = [| "magick"; image_b; "-crop"; Printf.sprintf "%dx%d+%d+%d" w h x y; "+repage"; crop_b |] in
                        let _ = Safe_exec.run_stdout ~timeout_ms:20000 ~output_limit:(64 * 1024) "magick" args_a in
                        let _ = Safe_exec.run_stdout ~timeout_ms:20000 ~output_limit:(64 * 1024) "magick" args_b in

                        (* SSIM 계산 *)
                        let args_ssim = [| "magick"; "compare"; "-metric"; "SSIM"; crop_a; crop_b; "null:" |] in
                        let output =
                          match Safe_exec.run ~timeout_ms:20000 ~output_limit:(64 * 1024) "magick" args_ssim with
                          | Ok out -> out.stderr
                          | Error _ -> ""
                        in

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
                            let diff_path = Filename.concat output_dir (Printf.sprintf "diff_%s.png" name) in
                            let args_diff = [| "magick"; "compare"; crop_a; crop_b; diff_path |] in
                            let _ = Safe_exec.run_stdout ~timeout_ms:20000 ~output_limit:(64 * 1024) "magick" args_diff in
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
                    end)
      )

    | _ -> Error "Missing required parameters: image_a, image_b, regions"

(** figma_evolution_report 핸들러 - 진화 과정 리포트 생성 *)
let handle_evolution_report args : (Yojson.Safe.t, string) result =
  let run_dir = get_string "run_dir" args in
  let generate_image = get_bool_or "generate_image" true args in

  (* 최근 evolution 디렉토리 목록 *)
  let list_recent_runs () =
    let base = "/tmp/figma-evolution" in
    if not (Sys.file_exists base) then []
    else
      let entries = Sys.readdir base |> Array.to_list in
      let runs =
        entries
        |> List.filter (fun name -> String.starts_with ~prefix:"run_" name)
        |> List.map (fun name ->
          let path = Filename.concat base name in
          let mtime =
            try (Unix.stat path).Unix.st_mtime with Unix.Unix_error _ -> 0.0
          in
          (mtime, path)
        )
        |> List.sort (fun (a, _) (b, _) -> compare b a)
        |> List.map snd
      in
      let rec take n xs = match (n, xs) with | (0, _) -> [] | (_, []) -> [] | (k, x :: tl) -> x :: take (k - 1) tl in
      take 10 runs
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
              let args = [| "montage"; figma_png; last_png; "-tile"; "2x1"; "-geometry"; "+5+5"; "-background"; "#1a1a1a"; output |] in
              let _ = Safe_exec.run_stdout ~timeout_ms:20000 ~output_limit:(64 * 1024) "montage" args in
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

(** figma_export_image 핸들러 - Streaming Progress 지원 *)
let handle_export_image args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_ids_str = get_string "node_ids" args in
  let token = resolve_token args in
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
      (match Figma_effects.Perform.get_images ~token ~file_key ~node_ids ~format ~scale
               ?use_absolute_bounds ?version () with
       | Ok json ->
           let images = member "images" json in
           let result = match images with
             | Some (`Assoc img_map) ->
                 let total = List.length img_map in
                 (* 3개 이상 이미지 다운로드 시 Progress 알림 활성화 *)
                 let progress_token =
                   if download && total >= 3 then
                     Some (Mcp_progress.make_progress_token ())
                   else None
                 in
                 let _ = match progress_token with
                   | Some pt ->
                       Mcp_progress.update_progress ~token:pt ~current:0 ~total
                         ~message:(sprintf "Starting export of %d images..." total) ()
                   | None -> ()
                 in
                 let results = List.mapi (fun idx (id, url) ->
                   let result_str = match url with
                     | `String url ->
                         if download then
                           if is_http_url url then
                             let path = Printf.sprintf "%s/%s/%s.%s"
                               save_dir (sanitize_file_key file_key) (sanitize_node_id id) format in
                             (match Figma_effects.Perform.download_url ~url ~path with
                              | Ok saved -> sprintf "%s: %s -> %s" id url saved
                              | Error err -> sprintf "%s: %s (download error: %s)" id url err)
                           else
                             sprintf "%s: %s (download skipped: no URL)" id url
                         else
                           sprintf "%s: %s" id url
                     | _ -> sprintf "%s: (error)" id
                   in
                   (* Progress 업데이트 *)
                   let _ = match progress_token with
                     | Some pt ->
                         Mcp_progress.update_progress ~token:pt ~current:(idx + 1) ~total
                           ~message:(sprintf "Downloaded %d/%d: %s" (idx + 1) total id) ()
                     | None -> ()
                   in
                   result_str
                 ) img_map in
                 (* 완료 알림 *)
                 let _ = match progress_token with
                   | Some pt ->
                       Mcp_progress.update_progress ~token:pt ~current:total ~total
                         ~message:(sprintf "Export complete: %d images" total) ()
                   | None -> ()
                 in
                 String.concat "\n" results
             | _ -> "No images returned"
           in
           Ok (make_text_content result)
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, node_ids, token"

(** figma_export_smart 핸들러 - 대형 노드 자동 scale 조정 및 재귀 분할 *)
let handle_export_smart args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let node_id = get_string "node_id" args in
  let token = resolve_token args in
  let format = get_string_or "format" "png" args in
  let max_pixels = get_float_or "max_pixels" 16777216.0 args in  (* 4096x4096 default *)
  let split_children = get_bool_or "split_children" false args in
  let max_depth = Option.value ~default:1 (get_int "max_depth" args) in
  let download = get_bool_or "download" false args in
  let save_dir = get_string_or "save_dir" (default_asset_dir ()) args in
  let include_debug = get_bool_or "debug" false args in

  (* Calculate optimal scale to fit within max_pixels *)
  let auto_scale ~width ~height =
    let actual = float_of_int (width * height) in
    if actual <= max_pixels then 1.0
    else
      let ratio = sqrt (max_pixels /. actual) in
      max 0.01 (min 4.0 ratio)  (* Figma API: scale must be 0.01-4.0 *)
  in

  (* Get node dimensions from absoluteBoundingBox *)
  let get_node_dims json =
    match member "absoluteBoundingBox" json with
    | Some box ->
        let w = match member "width" box with Some (`Float f) -> int_of_float f | Some (`Int i) -> i | _ -> 0 in
        let h = match member "height" box with Some (`Float f) -> int_of_float f | Some (`Int i) -> i | _ -> 0 in
        (w, h)
    | None -> (0, 0)
  in

  (* Get child node IDs *)
  let get_child_ids json =
    match member "children" json with
    | Some (`List children) ->
        List.filter_map (fun child ->
          match member "id" child with
          | Some (`String id) -> Some id
          | _ -> None
        ) children
    | _ -> []
  in

  (* Export a single node with calculated scale *)
  let export_node ~node_id ~scale =
    match Figma_effects.Perform.get_images ~token:(Option.get token)
            ~file_key:(Option.get file_key) ~node_ids:[node_id] ~format
            ~scale () with
    | Ok json ->
        (match member "images" json with
         | Some (`Assoc img_map) ->
             List.filter_map (fun (id, url) ->
               match url with
               | `String url_str ->
                   let final_path =
                     if download && is_http_url url_str then
                      let path = Printf.sprintf "%s/%s/%s.%s"
                        save_dir (sanitize_file_key (Option.get file_key)) (sanitize_node_id id) format in
                       match Figma_effects.Perform.download_url ~url:url_str ~path with
                       | Ok saved -> Some saved
                       | Error _ -> Some url_str
                     else Some url_str
                   in
                   Option.map (fun p -> `Assoc [
                     ("node_id", `String id);
                     ("url", `String url_str);
                     ("scale", `Float scale);
                     ("path", `String p);
                   ]) final_path
               | _ -> None
             ) img_map
         | _ -> [])
    | Error _ -> []
  in

  (* Debug info accumulator *)
  let debug_info = ref [] in

  (* Recursive export for split_children *)
  let rec export_recursive ~node_id ~depth results =
    if depth > max_depth then results
    else
      (* Get node info via get_nodes API *)
      match Figma_effects.Perform.get_nodes ~token:(Option.get token)
              ~file_key:(Option.get file_key) ~node_ids:[node_id] ~depth:1 () with
      | Ok json ->
          (* Extract node from "nodes" -> node_id -> "document" *)
          let nodes_opt = member "nodes" json in
          let node_json = match nodes_opt with
            | Some (`Assoc nodes) ->
                debug_info := !debug_info @ [Printf.sprintf "Found nodes with %d entries, looking for '%s'" (List.length nodes) node_id];
                debug_info := !debug_info @ [Printf.sprintf "Available keys: %s" (String.concat ", " (List.map fst nodes))];
                (match List.assoc_opt node_id nodes with
                 | Some node_data ->
                     let node_data_str = Yojson.Safe.to_string node_data in
                     let truncated = if String.length node_data_str > 200 then String.sub node_data_str 0 200 ^ "..." else node_data_str in
                     debug_info := !debug_info @ [Printf.sprintf "Found node_data: %s" truncated];
                     let doc_opt = member "document" node_data in
                     (match doc_opt with
                      | Some doc -> debug_info := !debug_info @ ["document found!"]; Some doc
                      | None ->
                          let keys = match node_data with `Assoc lst -> List.map fst lst | _ -> [] in
                          debug_info := !debug_info @ [Printf.sprintf "document NOT found. node_data keys: %s" (String.concat ", " keys)];
                          None)
                 | None ->
                     debug_info := !debug_info @ ["Node ID not found in nodes"];
                     None)
            | Some other ->
                let str = Yojson.Safe.to_string other in
                let truncated = if String.length str > 100 then String.sub str 0 100 ^ "..." else str in
                debug_info := !debug_info @ [Printf.sprintf "nodes is not Assoc: %s" truncated];
                None
            | None ->
                debug_info := !debug_info @ ["No 'nodes' key in response"];
                None
          in
          let (w, h) = match node_json with
            | Some n ->
                let dims = get_node_dims n in
                debug_info := !debug_info @ [Printf.sprintf "Got dimensions: %dx%d" (fst dims) (snd dims)];
                dims
            | None ->
                debug_info := !debug_info @ ["node_json is None"];
                (0, 0)
          in
          let actual_pixels = w * h in
          if actual_pixels = 0 then (debug_info := !debug_info @ ["actual_pixels=0, returning empty"]; results)
          else if float_of_int actual_pixels <= max_pixels then
            (* Node fits, export directly *)
            let scale = auto_scale ~width:w ~height:h in
            let exported = export_node ~node_id ~scale in
            results @ exported
          else if split_children && depth < max_depth then
            (* Too big, try children *)
            let child_ids = match node_json with Some n -> get_child_ids n | None -> [] in
            if child_ids = [] then
              (* No children, force scale down *)
              let scale = auto_scale ~width:w ~height:h in
              let exported = export_node ~node_id ~scale in
              results @ exported
            else
              (* Recurse into children *)
              List.fold_left (fun acc child_id ->
                export_recursive ~node_id:child_id ~depth:(depth + 1) acc
              ) results child_ids
          else
            (* Not splitting, just scale down *)
            let scale = auto_scale ~width:w ~height:h in
            let exported = export_node ~node_id ~scale in
            results @ exported
      | Error err ->
          debug_info := !debug_info @ [Printf.sprintf "get_nodes returned Error: %s" err];
          results
  in

  match (file_key, node_id, token) with
  | (Some _file_key, Some node_id, Some _token) ->
      let normalized = normalize_node_id node_id in
      debug_info := !debug_info @ [Printf.sprintf "Starting with node_id='%s', normalized='%s'" node_id normalized];
      let results = export_recursive ~node_id:normalized ~depth:0 [] in
      let base_fields = [
        ("total_exports", `Int (List.length results));
        ("max_pixels", `Float max_pixels);
        ("split_children", `Bool split_children);
        ("exports", `List results);
      ] in
      let summary = `Assoc (
        if include_debug then
          base_fields @ [("debug", `List (List.map (fun s -> `String s) !debug_info))]
        else
          base_fields
      ) in
      Ok (make_text_content (Yojson.Safe.pretty_to_string summary))
  | _ -> Error "Missing required parameters: file_key, node_id, token"

(** figma_get_image_fills 핸들러 *)
let handle_get_image_fills args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
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
  let token = resolve_token args in
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
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_versions ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_file_comments 핸들러 *)
let handle_get_file_comments args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_comments ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_post_comment 핸들러 *)
let handle_post_comment args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
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
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_components ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_team_components 핸들러 *)
let handle_get_team_components args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = resolve_token args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_components ~token ~team_id with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_get_file_component_sets 핸들러 *)
let handle_get_file_component_sets args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_component_sets ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_team_component_sets 핸들러 *)
let handle_get_team_component_sets args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = resolve_token args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_component_sets ~token ~team_id with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_get_file_styles 핸들러 *)
let handle_get_file_styles args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in

  match (file_key, token) with
  | (Some file_key, Some token) ->
      (match Figma_effects.Perform.get_file_styles ~token ~file_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token"

(** figma_get_team_styles 핸들러 *)
let handle_get_team_styles args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let token = resolve_token args in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_effects.Perform.get_team_styles ~token ~team_id with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: team_id, token"

(** figma_get_component 핸들러 *)
let handle_get_component args : (Yojson.Safe.t, string) result =
  let component_key = get_string "component_key" args in
  let token = resolve_token args in

  match (component_key, token) with
  | (Some component_key, Some token) ->
      (match Figma_effects.Perform.get_component ~token ~component_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: component_key, token"

(** figma_get_component_set 핸들러 *)
let handle_get_component_set args : (Yojson.Safe.t, string) result =
  let component_set_key = get_string "component_set_key" args in
  let token = resolve_token args in

  match (component_set_key, token) with
  | (Some component_set_key, Some token) ->
      (match Figma_effects.Perform.get_component_set ~token ~component_set_key with
       | Ok json -> Ok (make_text_content (Yojson.Safe.pretty_to_string json))
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: component_set_key, token"

(** figma_get_style 핸들러 *)
let handle_get_style args : (Yojson.Safe.t, string) result =
  let style_key = get_string "style_key" args in
  let token = resolve_token args in

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
  let stats = Figma_plugin_bridge.list_channel_stats () in
  let default_channel = Figma_plugin_bridge.get_default_channel () in
  let stats_json =
    `List (List.map (fun (s : Figma_plugin_bridge.channel_stats) ->
      `Assoc [
        ("id", `String s.id);
        ("last_seen", `Float s.last_seen);
        ("commands", `Int s.commands);
        ("results", `Int s.results);
        ("waiters", `Int s.waiters);
      ]) stats)
  in
  let limits = `Assoc [
    ("max_commands", `Int Figma_config.Plugin.max_commands);
    ("max_results", `Int Figma_config.Plugin.max_results);
    ("max_waiters", `Int Figma_config.Plugin.max_waiters);
    ("result_ttl_seconds", `Float Figma_config.Plugin.result_ttl_seconds);
    ("cleanup_interval_seconds", `Float Figma_config.Plugin.cleanup_interval_seconds);
    ("poll_max_ms", `Int Figma_config.Plugin.poll_max_ms);
  ] in
  let result = `Assoc [
    ("channels", `List (List.map (fun id -> `String id) channels));
    ("stats", stats_json);
    ("limits", limits);
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

(* list_pages 핸들러 *)
let handle_plugin_list_pages args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"list_pages" ~build_payload:(fun _ -> `Null) args

(* switch_page 핸들러 *)
let handle_plugin_switch_page args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"switch_page"
    ~validate:(fun args -> match get_string "page_id" args with
      | None -> Error "Missing required parameter: page_id"
      | Some id -> Ok id)
    ~build_payload:(fun page_id _ -> `Assoc [("page_id", `String page_id)])
    args

(* list_components 핸들러 *)
let handle_plugin_list_components args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"list_components" ~default_timeout:20000 ~build_payload:(fun _ -> `Null) args

(* clone 핸들러 *)
let handle_plugin_clone args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let offset_x = get_int "offset_x" args |> Option.value ~default:20 in
      let offset_y = get_int "offset_y" args |> Option.value ~default:20 in
      let name = get_string "name" args in
      let payload_fields = [
        ("node_id", `String node_id);
        ("offset_x", `Int offset_x);
        ("offset_y", `Int offset_y);
      ] @ (match name with Some n -> [("name", `String n)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"clone" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* group 핸들러 *)
let handle_plugin_group args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let name = get_string "name" args in
      let payload_fields =
        (match node_ids with Some ids -> [("node_ids", `List (List.map (fun s -> `String s) ids))] | None -> []) @
        (match name with Some n -> [("name", `String n)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"group" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* ungroup 핸들러 *)
let handle_plugin_ungroup args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"ungroup"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id"
      | Some id -> Ok id)
    ~build_payload:(fun node_id _ -> `Assoc [("node_id", `String node_id)])
    args

(* set_selection 핸들러 *)
let handle_plugin_set_selection args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"set_selection" ~build_payload:(fun args ->
    match get_string_list "node_ids" args with
    | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
    | None -> `Assoc [("node_ids", `List [])]) args

(* zoom_to 핸들러 *)
let handle_plugin_zoom_to args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"zoom_to" ~build_payload:(fun args ->
    let node_ids = get_string_list "node_ids" args in
    let node_id = get_string "node_id" args in
    `Assoc (
      (match node_ids with Some ids -> [("node_ids", `List (List.map (fun s -> `String s) ids))] | None -> []) @
      (match node_id with Some id -> [("node_id", `String id)] | None -> []))) args

(* reorder 핸들러 *)
let handle_plugin_reorder args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"reorder"
    ~validate:(fun args ->
      match (get_string "node_id" args, get_string "direction" args) with
      | (None, _) -> Error "Missing required parameter: node_id"
      | (_, None) -> Error "Missing required parameter: direction"
      | (Some n, Some d) -> Ok (n, d))
    ~build_payload:(fun (node_id, direction) _ ->
      `Assoc [("node_id", `String node_id); ("direction", `String direction)])
    args

(* set_locked 핸들러 *)
let handle_plugin_set_locked args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_locked"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      `Assoc [("node_id", `String node_id); ("locked", `Bool (get_bool "locked" args |> Option.value ~default:true))])
    args

(* set_visible 핸들러 *)
let handle_plugin_set_visible args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_visible"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      `Assoc [("node_id", `String node_id); ("visible", `Bool (get_bool "visible" args |> Option.value ~default:true))])
    args

(* flatten 핸들러 *)
let handle_plugin_flatten args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"flatten"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id _ -> `Assoc [("node_id", `String node_id)])
    args

(* set_auto_layout 핸들러 *)
let handle_plugin_set_auto_layout args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let layout_mode = get_string "layout_mode" args in
      let item_spacing = get_int "item_spacing" args in
      let padding = get_int "padding" args in
      let primary_alignment = get_string "primary_alignment" args in
      let counter_alignment = get_string "counter_alignment" args in
      let payload_fields = [("node_id", `String node_id)] @
        (match layout_mode with Some m -> [("layout_mode", `String m)] | None -> []) @
        (match item_spacing with Some s -> [("item_spacing", `Int s)] | None -> []) @
        (match padding with Some p -> [("padding", `Int p)] | None -> []) @
        (match primary_alignment with Some a -> [("primary_alignment", `String a)] | None -> []) @
        (match counter_alignment with Some a -> [("counter_alignment", `String a)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_auto_layout" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_viewport 핸들러 *)
let handle_plugin_get_viewport args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"get_viewport" ~build_payload:(fun _ -> `Null) args

(* set_viewport 핸들러 *)
let handle_plugin_set_viewport args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"set_viewport" ~build_payload:(fun args ->
    `Assoc (
      (match get_float "center_x" args with Some x -> [("center_x", `Float x)] | None -> []) @
      (match get_float "center_y" args with Some y -> [("center_y", `Float y)] | None -> []) @
      (match get_float "zoom" args with Some z -> [("zoom", `Float z)] | None -> []))) args

(* rename 핸들러 *)
let handle_plugin_rename args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"rename"
    ~validate:(fun args ->
      match (get_string "node_id" args, get_string "name" args) with
      | (None, _) -> Error "Missing required parameter: node_id"
      | (_, None) -> Error "Missing required parameter: name"
      | (Some n, Some name) -> Ok (n, name))
    ~build_payload:(fun (node_id, name) _ -> `Assoc [("node_id", `String node_id); ("name", `String name)])
    args

(* resize 핸들러 *)
let handle_plugin_resize args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"resize"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      `Assoc ([("node_id", `String node_id)] @
        (match get_float "width" args with Some w -> [("width", `Float w)] | None -> []) @
        (match get_float "height" args with Some h -> [("height", `Float h)] | None -> [])))
    args

(* move 핸들러 *)
let handle_plugin_move args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"move"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      `Assoc ([("node_id", `String node_id)] @
        (match get_float "x" args with Some v -> [("x", `Float v)] | None -> []) @
        (match get_float "y" args with Some v -> [("y", `Float v)] | None -> [])))
    args

(* set_opacity 핸들러 *)
let handle_plugin_set_opacity args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_opacity"
    ~validate:(fun args ->
      match (get_string "node_id" args, get_float "opacity" args) with
      | (None, _) -> Error "Missing required parameter: node_id"
      | (_, None) -> Error "Missing required parameter: opacity"
      | (Some n, Some o) -> Ok (n, o))
    ~build_payload:(fun (node_id, opacity) _ ->
      `Assoc [("node_id", `String node_id); ("opacity", `Float opacity)])
    args

(* set_corner_radius 핸들러 *)
let handle_plugin_set_corner_radius args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let radius = get_float "radius" args in
      let top_left = get_float "top_left" args in
      let top_right = get_float "top_right" args in
      let bottom_left = get_float "bottom_left" args in
      let bottom_right = get_float "bottom_right" args in
      let payload_fields = [("node_id", `String node_id)] @
        (match radius with Some r -> [("radius", `Float r)] | None -> []) @
        (match top_left with Some r -> [("topLeft", `Float r)] | None -> []) @
        (match top_right with Some r -> [("topRight", `Float r)] | None -> []) @
        (match bottom_left with Some r -> [("bottomLeft", `Float r)] | None -> []) @
        (match bottom_right with Some r -> [("bottomRight", `Float r)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_corner_radius" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_fill 핸들러 *)
let handle_plugin_set_fill args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let r = get_float "r" args |> Option.value ~default:0.0 in
      let g = get_float "g" args |> Option.value ~default:0.0 in
      let b = get_float "b" args |> Option.value ~default:0.0 in
      let a = get_float "a" args in
      let color_fields = [("r", `Float r); ("g", `Float g); ("b", `Float b)] @
        (match a with Some v -> [("a", `Float v)] | None -> []) in
      let payload = `Assoc [("node_id", `String node_id); ("color", `Assoc color_fields)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_fill" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_stroke 핸들러 *)
let handle_plugin_set_stroke args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let r = get_float "stroke_r" args in
      let g = get_float "stroke_g" args in
      let b = get_float "stroke_b" args in
      let weight = get_float "stroke_weight" args in
      let color_opt = match (r, g, b) with
        | (Some rv, Some gv, Some bv) -> Some (`Assoc [("r", `Float rv); ("g", `Float gv); ("b", `Float bv)])
        | _ -> None in
      let payload_fields = [("node_id", `String node_id)] @
        (match color_opt with Some c -> [("color", c)] | None -> []) @
        (match weight with Some w -> [("weight", `Float w)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_stroke" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_effects 핸들러 *)
let handle_plugin_set_effects args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_effects"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id args ->
      let effects = match args with
        | `Assoc fields -> (match List.assoc_opt "effects" fields with Some e -> e | None -> `List [])
        | _ -> `List [] in
      `Assoc [("node_id", `String node_id); ("effects", effects)])
    args

(* create_component 핸들러 *)
let handle_plugin_create_component args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"create_component"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id _ -> `Assoc [("node_id", `String node_id)])
    args

(* detach_instance 핸들러 *)
let handle_plugin_detach_instance args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"detach_instance"
    ~validate:(fun args -> match get_string "node_id" args with
      | None -> Error "Missing required parameter: node_id" | Some id -> Ok id)
    ~build_payload:(fun node_id _ -> `Assoc [("node_id", `String node_id)])
    args

(* set_text 핸들러 *)
let handle_plugin_set_text args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"set_text"
    ~validate:(fun args ->
      match (get_string "node_id" args, get_string "text" args) with
      | (None, _) -> Error "Missing required parameter: node_id"
      | (_, None) -> Error "Missing required parameter: text"
      | (Some n, Some t) -> Ok (n, t))
    ~build_payload:(fun (node_id, text) _ ->
      `Assoc [("node_id", `String node_id); ("text", `String text)])
    args

(* find_all 핸들러 *)
let handle_plugin_find_all args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"find_all" ~default_timeout:20000 ~build_payload:(fun args ->
    `Assoc (
      (match get_string "type" args with Some t -> [("type", `String t)] | None -> []) @
      (match get_string "find_name" args with Some n -> [("name", `String n)] | None -> []) @
      (match get_string "name_contains" args with Some n -> [("name_contains", `String n)] | None -> [])))
  args

(* notify 핸들러 *)
let handle_plugin_notify args : (Yojson.Safe.t, string) result =
  plugin_custom ~name:"notify"
    ~validate:(fun args -> match get_string "message" args with
      | None -> Error "Missing required parameter: message" | Some m -> Ok m)
    ~build_payload:(fun message args ->
      `Assoc [("message", `String message);
              ("timeout", `Int (get_int "notify_timeout" args |> Option.value ~default:3000))])
    args

(* create_frame 핸들러 *)
let handle_plugin_create_frame args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"create_frame" ~build_payload:(fun args ->
    `Assoc (
      (match get_string "name" args with Some n -> [("name", `String n)] | None -> []) @
      (match get_float "x" args with Some v -> [("x", `Float v)] | None -> []) @
      (match get_float "y" args with Some v -> [("y", `Float v)] | None -> []) @
      (match get_float "width" args with Some v -> [("width", `Float v)] | None -> []) @
      (match get_float "height" args with Some v -> [("height", `Float v)] | None -> [])))
  args

(* create_rectangle 핸들러 *)
let handle_plugin_create_rectangle args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"create_rectangle" ~build_payload:(fun args ->
    `Assoc (
      (match get_string "name" args with Some n -> [("name", `String n)] | None -> []) @
      (match get_float "x" args with Some v -> [("x", `Float v)] | None -> []) @
      (match get_float "y" args with Some v -> [("y", `Float v)] | None -> []) @
      (match get_float "width" args with Some v -> [("width", `Float v)] | None -> []) @
      (match get_float "height" args with Some v -> [("height", `Float v)] | None -> []) @
      (match get_float "radius" args with Some v -> [("cornerRadius", `Float v)] | None -> [])))
  args

(* create_ellipse 핸들러 *)
let handle_plugin_create_ellipse args : (Yojson.Safe.t, string) result =
  plugin_simple ~name:"create_ellipse" ~build_payload:(fun args ->
    `Assoc (
      (match get_string "name" args with Some n -> [("name", `String n)] | None -> []) @
      (match get_float "x" args with Some v -> [("x", `Float v)] | None -> []) @
      (match get_float "y" args with Some v -> [("y", `Float v)] | None -> []) @
      (match get_float "width" args with Some v -> [("width", `Float v)] | None -> []) @
      (match get_float "height" args with Some v -> [("height", `Float v)] | None -> [])))
  args

(* create_text 핸들러 *)
let handle_plugin_create_text args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let text = get_string "text" args in
      let font_size = get_int "font_size" args in
      let payload_fields =
        (match name with Some n -> [("name", `String n)] | None -> []) @
        (match x with Some v -> [("x", `Float v)] | None -> []) @
        (match y with Some v -> [("y", `Float v)] | None -> []) @
        (match text with Some t -> [("text", `String t)] | None -> []) @
        (match font_size with Some s -> [("fontSize", `Int s)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_text" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_line 핸들러 *)
let handle_plugin_create_line args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let length = get_float "length" args in
      let rotation = get_float "rotation" args in
      let stroke_weight = get_float "stroke_weight" args in
      let payload_fields =
        (match name with Some n -> [("name", `String n)] | None -> []) @
        (match x with Some v -> [("x", `Float v)] | None -> []) @
        (match y with Some v -> [("y", `Float v)] | None -> []) @
        (match length with Some v -> [("length", `Float v)] | None -> []) @
        (match rotation with Some v -> [("rotation", `Float v)] | None -> []) @
        (match stroke_weight with Some v -> [("stroke_weight", `Float v)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_line" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_polygon 핸들러 *)
let handle_plugin_create_polygon args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let width = get_float "width" args in
      let height = get_float "height" args in
      let point_count = get_int "point_count" args in
      let payload_fields =
        (match name with Some n -> [("name", `String n)] | None -> []) @
        (match x with Some v -> [("x", `Float v)] | None -> []) @
        (match y with Some v -> [("y", `Float v)] | None -> []) @
        (match width with Some v -> [("width", `Float v)] | None -> []) @
        (match height with Some v -> [("height", `Float v)] | None -> []) @
        (match point_count with Some c -> [("pointCount", `Int c)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_polygon" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_star 핸들러 *)
let handle_plugin_create_star args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let width = get_float "width" args in
      let height = get_float "height" args in
      let point_count = get_int "point_count" args in
      let inner_radius = get_float "inner_radius" args in
      let payload_fields =
        (match name with Some n -> [("name", `String n)] | None -> []) @
        (match x with Some v -> [("x", `Float v)] | None -> []) @
        (match y with Some v -> [("y", `Float v)] | None -> []) @
        (match width with Some v -> [("width", `Float v)] | None -> []) @
        (match height with Some v -> [("height", `Float v)] | None -> []) @
        (match point_count with Some c -> [("pointCount", `Int c)] | None -> []) @
        (match inner_radius with Some r -> [("innerRadius", `Float r)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_star" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* delete_node 핸들러 *)
let handle_plugin_delete_node args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"delete_node" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* duplicate 핸들러 *)
let handle_plugin_duplicate args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let offset_x = get_float "offset_x" args in
      let offset_y = get_float "offset_y" args in
      let name = get_string "name" args in
      let payload_fields = [("node_id", `String node_id)] @
        (match offset_x with Some v -> [("offset_x", `Float v)] | None -> []) @
        (match offset_y with Some v -> [("offset_y", `Float v)] | None -> []) @
        (match name with Some n -> [("name", `String n)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"duplicate" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* align 핸들러 *)
let handle_plugin_align args : (Yojson.Safe.t, string) result =
  match (get_string "align_direction" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: align_direction (left/center/right/top/middle/bottom)"
  | (_, Error msg) -> Error msg
  | (Some direction, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload_fields = [("direction", `String direction)] @
        (match node_ids with Some ids -> [("node_ids", `List (List.map (fun s -> `String s) ids))] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"align" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* distribute 핸들러 *)
let handle_plugin_distribute args : (Yojson.Safe.t, string) result =
  match (get_string "distribute_direction" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: distribute_direction (horizontal/vertical)"
  | (_, Error msg) -> Error msg
  | (Some direction, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload_fields = [("direction", `String direction)] @
        (match node_ids with Some ids -> [("node_ids", `List (List.map (fun s -> `String s) ids))] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"distribute" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* boolean_union 핸들러 *)
let handle_plugin_boolean_union args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload = match node_ids with
        | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"boolean_union" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* boolean_subtract 핸들러 *)
let handle_plugin_boolean_subtract args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload = match node_ids with
        | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"boolean_subtract" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* boolean_intersect 핸들러 *)
let handle_plugin_boolean_intersect args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload = match node_ids with
        | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"boolean_intersect" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* boolean_exclude 핸들러 *)
let handle_plugin_boolean_exclude args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_ids = get_string_list "node_ids" args in
      let payload = match node_ids with
        | Some ids -> `Assoc [("node_ids", `List (List.map (fun s -> `String s) ids))]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"boolean_exclude" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_local_styles 핸들러 *)
let handle_plugin_get_local_styles args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_local_styles" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_constraints 핸들러 *)
let handle_plugin_set_constraints args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let horizontal = get_string "constraint_horizontal" args in
      let vertical = get_string "constraint_vertical" args in
      let payload_fields = [("node_id", `String node_id)] @
        (match horizontal with Some h -> [("horizontal", `String h)] | None -> []) @
        (match vertical with Some v -> [("vertical", `String v)] | None -> []) in
      let payload = `Assoc payload_fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_constraints" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_page 핸들러 *)
let handle_plugin_create_page args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let payload = match name with
        | Some n -> `Assoc [("name", `String n)]
        | None -> `Assoc [] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_page" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* delete_page 핸들러 *)
let handle_plugin_delete_page args : (Yojson.Safe.t, string) result =
  match (get_string "page_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: page_id"
  | (_, Error msg) -> Error msg
  | (Some page_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("page_id", `String page_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"delete_page" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* rotate 핸들러 *)
let handle_plugin_rotate args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_float "angle" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: angle"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some angle, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id); ("angle", `Float angle)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"rotate" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* flip 핸들러 *)
let handle_plugin_flip args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_string "flip_direction" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: flip_direction (horizontal/vertical)"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some direction, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id); ("direction", `String direction)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"flip" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* outline_stroke 핸들러 *)
let handle_plugin_outline_stroke args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"outline_stroke" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_blend_mode 핸들러 *)
let handle_plugin_set_blend_mode args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_string "blend_mode" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: blend_mode"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some blend_mode, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id); ("blend_mode", `String blend_mode)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_blend_mode" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_selection_colors 핸들러 *)
let handle_plugin_get_selection_colors args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_selection_colors" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* swap_fill_stroke 핸들러 *)
let handle_plugin_swap_fill_stroke args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: node_id"
  | (_, Error msg) -> Error msg
  | (Some node_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"swap_fill_stroke" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* copy_style 핸들러 *)
let handle_plugin_copy_style args : (Yojson.Safe.t, string) result =
  match (get_string "source_id" args, get_string "target_id" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: source_id"
  | (_, None, _) -> Error "Missing required parameter: target_id"
  | (_, _, Error msg) -> Error msg
  | (Some source_id, Some target_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("source_id", `String source_id); ("target_id", `String target_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"copy_style" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_fonts 핸들러 *)
let handle_plugin_get_fonts args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_fonts" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_parent 핸들러 *)
let handle_plugin_set_parent args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_string "parent_id" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: parent_id"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some parent_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let payload = `Assoc [("node_id", `String node_id); ("parent_id", `String parent_id)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_parent" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_vector 핸들러 *)
let handle_plugin_create_vector args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let width = get_float "width" args in
      let height = get_float "height" args in
      let fields = [] in
      let fields = match name with Some v -> ("name", `String v) :: fields | None -> fields in
      let fields = match x with Some v -> ("x", `Float v) :: fields | None -> fields in
      let fields = match y with Some v -> ("y", `Float v) :: fields | None -> fields in
      let fields = match width with Some v -> ("width", `Float v) :: fields | None -> fields in
      let fields = match height with Some v -> ("height", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_vector" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_image_fill 핸들러 *)
let handle_plugin_set_image_fill args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let node_id = get_string "node_id" args in
      let image_hash = get_string "image_hash" args in
      let base64 = get_string "base64" args in
      let scale_mode = get_string "scale_mode" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match image_hash with Some v -> ("image_hash", `String v) :: fields | None -> fields in
      let fields = match base64 with Some v -> ("base64", `String v) :: fields | None -> fields in
      let fields = match scale_mode with Some v -> ("scale_mode", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_image_fill" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_plugin_data 핸들러 *)
let handle_plugin_get_plugin_data args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let key = get_string "data_key" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match key with Some v -> ("key", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_plugin_data" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_plugin_data 핸들러 *)
let handle_plugin_set_plugin_data args : (Yojson.Safe.t, string) result =
  match (get_string "data_key" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: data_key"
  | (_, Error msg) -> Error msg
  | (Some key, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let value = get_string "data_value" args |> Option.value ~default:"" in
      let fields = [("key", `String key); ("value", `String value)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_plugin_data" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_doc_info 핸들러 *)
let handle_plugin_get_doc_info args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_doc_info" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_absolute_bounds 핸들러 *)
let handle_plugin_get_absolute_bounds args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_absolute_bounds" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_component_set 핸들러 *)
let handle_plugin_create_component_set args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let component_ids = get_string_list "component_ids" args |> Option.value ~default:[] in
      let name = get_string "name" args in
      let fields = [("component_ids", `List (List.map (fun s -> `String s) component_ids))] in
      let fields = match name with Some v -> ("name", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_component_set" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* remove_auto_layout 핸들러 *)
let handle_plugin_remove_auto_layout args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"remove_auto_layout" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* create_slice 핸들러 *)
let handle_plugin_create_slice args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let name = get_string "name" args in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let width = get_float "width" args in
      let height = get_float "height" args in
      let fields = [] in
      let fields = match name with Some v -> ("name", `String v) :: fields | None -> fields in
      let fields = match x with Some v -> ("x", `Float v) :: fields | None -> fields in
      let fields = match y with Some v -> ("y", `Float v) :: fields | None -> fields in
      let fields = match width with Some v -> ("width", `Float v) :: fields | None -> fields in
      let fields = match height with Some v -> ("height", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"create_slice" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_export_settings 핸들러 *)
let handle_plugin_set_export_settings args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let format = get_string "export_format" args in
      let scale = get_float "scale" args in
      let suffix = get_string "suffix" args in
      let append = get_bool "append" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match format with Some v -> ("format", `String v) :: fields | None -> fields in
      let fields = match scale with Some v -> ("scale", `Float v) :: fields | None -> fields in
      let fields = match suffix with Some v -> ("suffix", `String v) :: fields | None -> fields in
      let fields = match append with Some v -> ("append", `Bool v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_export_settings" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_reactions 핸들러 *)
let handle_plugin_get_reactions args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_reactions" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_reactions 핸들러 *)
let handle_plugin_set_reactions args : (Yojson.Safe.t, string) result =
  match (get_string "target_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: target_id"
  | (_, Error msg) -> Error msg
  | (Some target_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let trigger = get_string "trigger" args |> Option.value ~default:"ON_CLICK" in
      let navigation = get_string "navigation" args in
      let preserve_scroll = get_bool "preserve_scroll" args in
      let fields = [("target_id", `String target_id); ("trigger", `String trigger)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match navigation with Some v -> ("navigation", `String v) :: fields | None -> fields in
      let fields = match preserve_scroll with Some v -> ("preserve_scroll", `Bool v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_reactions" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* rasterize 핸들러 *)
let handle_plugin_rasterize args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:30000 in
      let node_id = get_string "node_id" args in
      let format = get_string "format" args in
      let scale = get_float "scale" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match format with Some v -> ("format", `String v) :: fields | None -> fields in
      let fields = match scale with Some v -> ("scale", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"rasterize" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_shared_plugin_data 핸들러 *)
let handle_plugin_get_shared_plugin_data args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let namespace = get_string "namespace" args |> Option.value ~default:"shared" in
      let key = get_string "data_key" args in
      let fields = [("namespace", `String namespace)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match key with Some v -> ("key", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_shared_plugin_data" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_shared_plugin_data 핸들러 *)
let handle_plugin_set_shared_plugin_data args : (Yojson.Safe.t, string) result =
  match (get_string "data_key" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: data_key"
  | (_, Error msg) -> Error msg
  | (Some key, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let namespace = get_string "namespace" args |> Option.value ~default:"shared" in
      let value = get_string "data_value" args |> Option.value ~default:"" in
      let fields = [("namespace", `String namespace); ("key", `String key); ("value", `String value)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_shared_plugin_data" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* swap_component 핸들러 *)
let handle_plugin_swap_component args : (Yojson.Safe.t, string) result =
  match (get_string "component_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: component_id"
  | (_, Error msg) -> Error msg
  | (Some component_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = [("component_id", `String component_id)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"swap_component" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* resize_to_fit 핸들러 *)
let handle_plugin_resize_to_fit args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let axis = get_string "axis" args in
      let padding = get_float "padding" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match axis with Some v -> ("axis", `String v) :: fields | None -> fields in
      let fields = match padding with Some v -> ("padding", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"resize_to_fit" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_characters 핸들러 *)
let handle_plugin_get_characters args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_characters" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_range_fills 핸들러 *)
let handle_plugin_set_range_fills args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let start_idx = get_int "start" args in
      let end_idx = get_int "end" args in
      let r = get_float "r" args in
      let g = get_float "g" args in
      let b = get_float "b" args in
      let a = get_float "a" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match start_idx with Some v -> ("start", `Int v) :: fields | None -> fields in
      let fields = match end_idx with Some v -> ("end", `Int v) :: fields | None -> fields in
      let color_fields = [] in
      let color_fields = match r with Some v -> ("r", `Float v) :: color_fields | None -> color_fields in
      let color_fields = match g with Some v -> ("g", `Float v) :: color_fields | None -> color_fields in
      let color_fields = match b with Some v -> ("b", `Float v) :: color_fields | None -> color_fields in
      let color_fields = match a with Some v -> ("a", `Float v) :: color_fields | None -> color_fields in
      let fields = if color_fields <> [] then ("color", `Assoc color_fields) :: fields else fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_range_fills" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_range_font_size 핸들러 *)
let handle_plugin_set_range_font_size args : (Yojson.Safe.t, string) result =
  match (get_float "font_size" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: font_size"
  | (_, Error msg) -> Error msg
  | (Some font_size, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let start_idx = get_int "start" args in
      let end_idx = get_int "end" args in
      let fields = [("font_size", `Float font_size)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match start_idx with Some v -> ("start", `Int v) :: fields | None -> fields in
      let fields = match end_idx with Some v -> ("end", `Int v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_range_font_size" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* insert_child 핸들러 *)
let handle_plugin_insert_child args : (Yojson.Safe.t, string) result =
  match (get_string "node_id" args, get_string "parent_id" args, resolve_channel_id args) with
  | (None, _, _) -> Error "Missing required parameter: node_id"
  | (_, None, _) -> Error "Missing required parameter: parent_id"
  | (_, _, Error msg) -> Error msg
  | (Some node_id, Some parent_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let index = get_int "index" args |> Option.value ~default:0 in
      let payload = `Assoc [("node_id", `String node_id); ("parent_id", `String parent_id); ("index", `Int index)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"insert_child" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_all_local_variables 핸들러 *)
let handle_plugin_get_all_local_variables args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:15000 in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_all_local_variables" ~payload:`Null in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_styles_by_type 핸들러 *)
let handle_plugin_get_styles_by_type args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let style_type = get_string "style_type" args |> Option.value ~default:"FILL" in
      let payload = `Assoc [("style_type", `String style_type)] in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_styles_by_type" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* apply_style 핸들러 *)
let handle_plugin_apply_style args : (Yojson.Safe.t, string) result =
  match (get_string "style_id" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: style_id"
  | (_, Error msg) -> Error msg
  | (Some style_id, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let style_type = get_string "style_type" args |> Option.value ~default:"FILL" in
      let fields = [("style_id", `String style_id); ("style_type", `String style_type)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"apply_style" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_overrides 핸들러 *)
let handle_plugin_get_overrides args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_overrides" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* reset_overrides 핸들러 *)
let handle_plugin_reset_overrides args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"reset_overrides" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* bring_to_front 핸들러 *)
let handle_plugin_bring_to_front args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"bring_to_front" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* send_to_back 핸들러 *)
let handle_plugin_send_to_back args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"send_to_back" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_grid 핸들러 *)
let handle_plugin_set_grid args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let pattern = get_string "pattern" args in
      let count = get_int "count" args in
      let gutter = get_int "gutter" args in
      let offset = get_int "offset" args in
      let alignment = get_string "alignment" args in
      let size = get_int "size" args in
      let visible = get_bool "visible" args in
      let append = get_bool "append" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match pattern with Some v -> ("pattern", `String v) :: fields | None -> fields in
      let fields = match count with Some v -> ("count", `Int v) :: fields | None -> fields in
      let fields = match gutter with Some v -> ("gutter", `Int v) :: fields | None -> fields in
      let fields = match offset with Some v -> ("offset", `Int v) :: fields | None -> fields in
      let fields = match alignment with Some v -> ("alignment", `String v) :: fields | None -> fields in
      let fields = match size with Some v -> ("size", `Int v) :: fields | None -> fields in
      let fields = match visible with Some v -> ("visible", `Bool v) :: fields | None -> fields in
      let fields = match append with Some v -> ("append", `Bool v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_grid" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_layer_list 핸들러 *)
let handle_plugin_get_layer_list args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_layer_list" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* scroll_and_zoom 핸들러 *)
let handle_plugin_scroll_and_zoom args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let x = get_float "x" args in
      let y = get_float "y" args in
      let zoom = get_float "zoom" args in
      let fields = [] in
      let fields = match x with Some v -> ("x", `Float v) :: fields | None -> fields in
      let fields = match y with Some v -> ("y", `Float v) :: fields | None -> fields in
      let fields = match zoom with Some v -> ("zoom", `Float v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"scroll_and_zoom" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_paint_styles 핸들러 *)
let handle_plugin_get_paint_styles args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_paint_styles" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_text_case 핸들러 *)
let handle_plugin_set_text_case args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let text_case = get_string "text_case" args |> Option.value ~default:"ORIGINAL" in
      let fields = [("text_case", `String text_case)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_text_case" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* get_stroke_details 핸들러 *)
let handle_plugin_get_stroke_details args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = match node_id with Some v -> [("node_id", `String v)] | None -> [] in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"get_stroke_details" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* set_stroke_weight 핸들러 *)
let handle_plugin_set_stroke_weight args : (Yojson.Safe.t, string) result =
  match (get_float "weight" args, resolve_channel_id args) with
  | (None, _) -> Error "Missing required parameter: weight"
  | (_, Error msg) -> Error msg
  | (Some weight, Ok channel_id) ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let fields = [("weight", `Float weight)] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"set_stroke_weight" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* collapse_layer 핸들러 *)
let handle_plugin_collapse_layer args : (Yojson.Safe.t, string) result =
  match resolve_channel_id args with
  | Error msg -> Error msg
  | Ok channel_id ->
      let timeout_ms = get_int "timeout_ms" args |> Option.value ~default:10000 in
      let node_id = get_string "node_id" args in
      let expand = get_bool "expand" args in
      let fields = [] in
      let fields = match node_id with Some v -> ("node_id", `String v) :: fields | None -> fields in
      let fields = match expand with Some v -> ("expand", `Bool v) :: fields | None -> fields in
      let payload = `Assoc fields in
      let command_id = Figma_plugin_bridge.enqueue_command ~channel_id ~name:"collapse_layer" ~payload in
      (match plugin_wait ~channel_id ~command_id ~timeout_ms with
       | Error err -> Error err
       | Ok result -> Ok (make_text_content (Yojson.Safe.pretty_to_string result.payload)))

(* STRAP 통합 핸들러: action으로 라우팅, 기존 핸들러 재사용 *)
let handle_figma_plugin args : (Yojson.Safe.t, string) result =
  match get_string "action" args with
  | None -> Error "Missing required parameter: action"
  | Some action ->
      match action with
      | "connect" -> handle_plugin_connect args
      | "use_channel" -> handle_plugin_use_channel args
      | "status" -> handle_plugin_status args
      | "read_selection" -> handle_plugin_read_selection args
      | "get_node" -> handle_plugin_get_node args
      | "export_image" -> handle_plugin_export_node_image args
      | "get_variables" -> handle_plugin_get_variables args
      | "apply_ops" -> handle_plugin_apply_ops args
      | "list_pages" -> handle_plugin_list_pages args
      | "switch_page" -> handle_plugin_switch_page args
      | "list_components" -> handle_plugin_list_components args
      | "clone" -> handle_plugin_clone args
      | "group" -> handle_plugin_group args
      | "ungroup" -> handle_plugin_ungroup args
      | "set_selection" -> handle_plugin_set_selection args
      | "zoom_to" -> handle_plugin_zoom_to args
      | "reorder" -> handle_plugin_reorder args
      | "set_locked" -> handle_plugin_set_locked args
      | "set_visible" -> handle_plugin_set_visible args
      | "flatten" -> handle_plugin_flatten args
      | "set_auto_layout" -> handle_plugin_set_auto_layout args
      | "get_viewport" -> handle_plugin_get_viewport args
      | "set_viewport" -> handle_plugin_set_viewport args
      | "rename" -> handle_plugin_rename args
      | "resize" -> handle_plugin_resize args
      | "move" -> handle_plugin_move args
      | "set_opacity" -> handle_plugin_set_opacity args
      | "set_corner_radius" -> handle_plugin_set_corner_radius args
      | "set_fill" -> handle_plugin_set_fill args
      | "set_stroke" -> handle_plugin_set_stroke args
      | "set_effects" -> handle_plugin_set_effects args
      | "create_component" -> handle_plugin_create_component args
      | "detach_instance" -> handle_plugin_detach_instance args
      | "set_text" -> handle_plugin_set_text args
      | "find_all" -> handle_plugin_find_all args
      | "notify" -> handle_plugin_notify args
      | "create_frame" -> handle_plugin_create_frame args
      | "create_rectangle" -> handle_plugin_create_rectangle args
      | "create_ellipse" -> handle_plugin_create_ellipse args
      | "create_text" -> handle_plugin_create_text args
      | "create_line" -> handle_plugin_create_line args
      | "create_polygon" -> handle_plugin_create_polygon args
      | "create_star" -> handle_plugin_create_star args
      | "delete_node" -> handle_plugin_delete_node args
      | "duplicate" -> handle_plugin_duplicate args
      | "align" -> handle_plugin_align args
      | "distribute" -> handle_plugin_distribute args
      | "boolean_union" -> handle_plugin_boolean_union args
      | "boolean_subtract" -> handle_plugin_boolean_subtract args
      | "boolean_intersect" -> handle_plugin_boolean_intersect args
      | "boolean_exclude" -> handle_plugin_boolean_exclude args
      | "get_local_styles" -> handle_plugin_get_local_styles args
      | "set_constraints" -> handle_plugin_set_constraints args
      | "create_page" -> handle_plugin_create_page args
      | "delete_page" -> handle_plugin_delete_page args
      | "rotate" -> handle_plugin_rotate args
      | "flip" -> handle_plugin_flip args
      | "outline_stroke" -> handle_plugin_outline_stroke args
      | "set_blend_mode" -> handle_plugin_set_blend_mode args
      | "get_selection_colors" -> handle_plugin_get_selection_colors args
      | "swap_fill_stroke" -> handle_plugin_swap_fill_stroke args
      | "copy_style" -> handle_plugin_copy_style args
      | "get_fonts" -> handle_plugin_get_fonts args
      | "set_parent" -> handle_plugin_set_parent args
      | "create_vector" -> handle_plugin_create_vector args
      | "set_image_fill" -> handle_plugin_set_image_fill args
      | "get_plugin_data" -> handle_plugin_get_plugin_data args
      | "set_plugin_data" -> handle_plugin_set_plugin_data args
      | "get_doc_info" -> handle_plugin_get_doc_info args
      | "get_absolute_bounds" -> handle_plugin_get_absolute_bounds args
      | "create_component_set" -> handle_plugin_create_component_set args
      | "remove_auto_layout" -> handle_plugin_remove_auto_layout args
      | "create_slice" -> handle_plugin_create_slice args
      | "set_export_settings" -> handle_plugin_set_export_settings args
      | "get_reactions" -> handle_plugin_get_reactions args
      | "set_reactions" -> handle_plugin_set_reactions args
      | "rasterize" -> handle_plugin_rasterize args
      | "get_shared_plugin_data" -> handle_plugin_get_shared_plugin_data args
      | "set_shared_plugin_data" -> handle_plugin_set_shared_plugin_data args
      | "swap_component" -> handle_plugin_swap_component args
      | "resize_to_fit" -> handle_plugin_resize_to_fit args
      | "get_characters" -> handle_plugin_get_characters args
      | "set_range_fills" -> handle_plugin_set_range_fills args
      | "set_range_font_size" -> handle_plugin_set_range_font_size args
      | "insert_child" -> handle_plugin_insert_child args
      | "get_all_local_variables" -> handle_plugin_get_all_local_variables args
      | "get_styles_by_type" -> handle_plugin_get_styles_by_type args
      | "apply_style" -> handle_plugin_apply_style args
      | "get_overrides" -> handle_plugin_get_overrides args
      | "reset_overrides" -> handle_plugin_reset_overrides args
      | "bring_to_front" -> handle_plugin_bring_to_front args
      | "send_to_back" -> handle_plugin_send_to_back args
      | "set_grid" -> handle_plugin_set_grid args
      | "get_layer_list" -> handle_plugin_get_layer_list args
      | "scroll_and_zoom" -> handle_plugin_scroll_and_zoom args
      | "get_paint_styles" -> handle_plugin_get_paint_styles args
      | "set_text_case" -> handle_plugin_set_text_case args
      | "get_stroke_details" -> handle_plugin_get_stroke_details args
      | "set_stroke_weight" -> handle_plugin_set_stroke_weight args
      | "collapse_layer" -> handle_plugin_collapse_layer args
      | _ -> Error (sprintf "Unknown action: %s. 100 actions available." action)

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

let bump_count counts key =
  let current = match Hashtbl.find_opt counts key with
    | Some v -> v
    | None -> 0
  in
  Hashtbl.replace counts key (current + 1)

let type_counts_to_json counts =
  let items =
    Hashtbl.to_seq counts
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  `Assoc (List.map (fun (k, v) -> (k, `Int v)) items)

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
  match resolve_token args with
  | None -> Error "Missing required parameter: token (set FIGMA_TOKEN env var or pass explicitly)"
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
  let token = resolve_token args in

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
  let token = resolve_token args in

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

(** figma_crawl_team 핸들러 - 팀 전체 크롤링 + Neo4j 저장 *)
(* TODO: Effects 통합 후 실제 구현 활성화 *)
let handle_crawl_team args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let team_name = get_string_or "team_name" "Unknown Team" args in
  let token = resolve_token args in
  let max_depth = get_string "max_depth" args |> Option.map int_of_string |> Option.value ~default:10 in
  let rate_limit_ms = get_string "rate_limit_ms" args |> Option.map int_of_string |> Option.value ~default:100 in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (* Neo4j 설정 (환경변수에서 또는 파라미터에서) *)
      let neo4j_uri = get_string "neo4j_uri" args
        |> Option.value ~default:(Sys.getenv_opt "NEO4J_URI" |> Option.value ~default:"http://localhost:7474") in
      let neo4j_user = get_string "neo4j_user" args
        |> Option.value ~default:(Sys.getenv_opt "NEO4J_USER" |> Option.value ~default:"neo4j") in
      let neo4j_password = get_string "neo4j_password" args
        |> Option.value ~default:(Sys.getenv_opt "NEO4J_PASSWORD" |> Option.value ~default:"") in
      let neo4j_database = Sys.getenv_opt "NEO4J_DATABASE" |> Option.value ~default:"neo4j" in

      let neo4j_cfg = Figma_crawl.create_neo4j_config
        ~uri:neo4j_uri ~database:neo4j_database ~user:neo4j_user ~password:neo4j_password () in

      let options = {
        Figma_crawl.max_depth;
        include_hidden = false;
        batch_size = 100;
        rate_limit_ms;
        skip_files = [];
      } in

      (* 진행 상황 버퍼 *)
      let (on_progress, get_log) = Figma_crawl.buffer_progress () in

      (* 크롤링 실행 (Effects 기반) *)
      (match Figma_crawl.crawl_team ~token ~team_id ~neo4j_cfg ~options ~team_name ~on_progress () with
       | Ok progress ->
           let result_json = `Assoc [
             ("status", `String "success");
             ("summary", Figma_crawl.progress_to_json progress);
             ("log", `String (get_log ()));
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string result_json))
       | Error err ->
           let result_json = `Assoc [
             ("status", `String "error");
             ("error", `String err);
             ("log", `String (get_log ()));
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string result_json)))
  | (None, _) -> Error "Missing required parameter: team_id"
  | (_, None) -> Error "Missing required parameter: token (set FIGMA_TOKEN or pass token parameter)"

(** figma_team_tree 핸들러 - 팀 구조 트리 출력 (Neo4j 불필요) *)
let handle_team_tree args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let team_name = get_string_or "team_name" "Unknown Team" args in
  let token = resolve_token args in
  let include_nodes = get_bool_or "include_nodes" false args in
  let node_depth = get_string "node_depth" args |> Option.map int_of_string |> Option.value ~default:2 in

  match (team_id, token) with
  | (Some team_id, Some token) ->
      (match Figma_crawl.team_tree ~token ~team_id ~team_name ~include_nodes ~node_depth () with
       | Ok (tree_str, progress) ->
           Ok (make_text_content (sprintf "```\n%s```\n\n%s"
             tree_str (Yojson.Safe.pretty_to_string (`Assoc [("summary", Figma_crawl.progress_to_json progress)]))))
       | Error err -> Error err)
  | (None, _) -> Error "Missing required parameter: team_id"
  | (_, None) -> Error "Missing required parameter: token (set FIGMA_TOKEN or pass token parameter)"

(** figma_export_team 핸들러 - 파일 시스템으로 내보내기 *)
let handle_export_team args : (Yojson.Safe.t, string) result =
  let team_id = get_string "team_id" args in
  let team_name = get_string_or "team_name" "Unknown Team" args in
  let token = resolve_token args in
  let output_dir = get_string "output_dir" args in
  let max_depth = get_string "max_depth" args |> Option.map int_of_string |> Option.value ~default:2 in

  match (team_id, token, output_dir) with
  | (Some team_id, Some token, Some output_dir) ->
      let (on_progress, get_log) = Figma_crawl.buffer_progress () in
      (match Figma_crawl.export_team_to_fs ~token ~team_id ~output_dir ~max_depth ~team_name ~on_progress () with
       | Ok progress ->
           Ok (make_text_content (sprintf "```\n%s```\n\n%s"
             (get_log ()) (Yojson.Safe.pretty_to_string (`Assoc [
               ("status", `String "success");
               ("output_dir", `String output_dir);
               ("summary", Figma_crawl.progress_to_json progress);
             ]))))
       | Error err -> Error err)
  | (None, _, _) -> Error "Missing required parameter: team_id"
  | (_, None, _) -> Error "Missing required parameter: token"
  | (_, _, None) -> Error "Missing required parameter: output_dir"

(** figma_get_variables 핸들러 - 디자인 토큰/변수 *)
let handle_get_variables args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
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
  let token = resolve_token args in
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
  let token = resolve_token args in
  let query = get_string "query" args in
  let search_in = get_string_or "search_in" "both" args in
  let limit = get_float "limit" args |> Option.map int_of_float |> Option.value ~default:20 in

  match (file_key, token, query) with
  | (Some file_key, Some token, Some query) ->
      (* depth:4 covers Page→Frame→Group→Component; avoids 400 on large files *)
      (match Figma_effects.Perform.get_file ~token ~file_key ~depth:4 () with
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

(** figma_compare 핸들러 - P1.1: 통합 비교 도구 *)
let handle_compare args : (Yojson.Safe.t, string) result =
  let mode = get_string_or "mode" "general" args in

  (* P1.1: Mode-based dispatch to appropriate handler *)
  match mode with
  | "regions" -> handle_compare_regions args
  | "elements" -> handle_compare_elements args
  | "evolution" -> handle_evolution_report args
  | "batch" | "general" | _ ->
      (* Original general/batch logic *)
      let file_key = get_string "file_key" args in
      let token = resolve_token args in
      let node_a_id = get_string "node_a_id" args in
      let node_b_id = get_string "node_b_id" args in
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
                           (* General 모드: 특정 노드 쌍 비교 *)
                           match node_a_id, node_b_id with
                           | Some id_a, Some id_b ->
                               let find_node id = List.find_opt (fun n -> n.Figma_types.id = id) all_nodes in
                               (match find_node id_a, find_node id_b with
                                | Some node_a, Some node_b ->
                                    let result = Figma_compare.compare_nodes node_a node_b in
                                    Ok (make_text_content (Figma_compare.result_to_string result))
                                | None, _ -> Error (Printf.sprintf "Node A not found: %s" id_a)
                                | _, None -> Error (Printf.sprintf "Node B not found: %s" id_b))
                           | _ -> Error "General mode requires node_a_id and node_b_id"
                         end
                     | None -> Error "Failed to parse document"))
           | Error err -> Error err)
      | _ -> Error "Missing required parameters: file_key, token"

(** figma_tree 핸들러 *)
let handle_tree args : (Yojson.Safe.t, string) result =
  let file_key = get_string "file_key" args in
  let token = resolve_token args in
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
  let token = resolve_token args in
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
  let token = resolve_token args in
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
                         Figma_tokens.export_tokens tokens (Figma_tokens.format_of_string format)
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
    if node_ok then command_output "node" [| "node"; "-v" |] else "missing"
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
    mk_check "playwright" playwright_ok (if playwright_ok then "ok (fallback renderer)" else "missing");
    mk_check "pngjs" pngjs_ok (if pngjs_ok then "ok" else "missing");
    mk_check "pixelmatch" pixelmatch_ok (if pixelmatch_ok then "ok" else "missing");
    mk_check "imagemagick" magick_ok magick_detail;
    mk_check "sips" sips_ok (if sips_ok then "ok" else "missing");
    mk_check "render_script" render_script_ok render_script;
    mk_check "ssim_script" ssim_script_ok ssim_script;
    mk_check "claude-in-chrome" true "preferred renderer (Claude Code built-in, runtime detection)";
  ] in

  let hints =
    List.filter_map Fun.id [
      if not node_ok then Some "Install Node.js (node required for render/compare scripts)." else None;
      if node_ok && not playwright_ok then Some "Playwright missing - will use claude-in-chrome if available, otherwise install: npm i -D playwright && npx playwright install chromium." else None;
      if node_ok && (not pngjs_ok || not pixelmatch_ok) then Some "Install image deps: npm i -D pngjs pixelmatch." else None;
      if not magick_ok then Some "Install ImageMagick (magick/convert) for PPM conversion." else None;
      if not render_script_ok then Some "Ensure render-html.js path is valid (FIGMA_RENDER_SCRIPT or scripts/render-html.js)." else None;
      if not ssim_script_ok then Some "Ensure ssim-compare.js path is valid (scripts/ssim-compare.js)." else None;
      Some "Chrome-First: When claude-in-chrome is available, use it for HTML rendering before falling back to Playwright.";
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
        In_channel.with_open_bin path (fun ic ->
          let total = in_channel_length ic in
          if safe_offset >= total then
            Error "offset is beyond EOF"
          else begin
            seek_in ic safe_offset;
            let to_read = min safe_limit (total - safe_offset) in
            let chunk = really_input_string ic to_read in
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
          end)

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

(** Code Connect index cache (in-memory).
    This is intentionally small and bounded to avoid unbounded memory growth. *)
let code_connect_index_cache : (string, Figma_code_connect.mapping) Hashtbl.t =
  Hashtbl.create 32

let code_connect_cache_put key mapping =
  if Hashtbl.length code_connect_index_cache > 64 then Hashtbl.reset code_connect_index_cache;
  Hashtbl.replace code_connect_index_cache key mapping

let handle_code_connect args : (Yojson.Safe.t, string) result =
  let mode = get_string_or "mode" "" args |> String.lowercase_ascii |> String.trim in
  if mode = "" then Error "Missing required parameter: mode"
  else
    let is_safe_relative_path path =
      let path = String.trim path in
      if path = "" then false
      else if path.[0] = '/' || path.[0] = '~' then false
      else
        let segs = String.split_on_char '/' path in
        not (List.exists (fun seg -> seg = "..") segs)
    in
    let read_file path =
      try
        In_channel.with_open_bin path (fun ic ->
          let len = in_channel_length ic in
          Ok (really_input_string ic len))
      with Sys_error msg -> Error msg
    in
    let default_paths = [ "figma-code-connect.json"; ".figma/code-connect.json" ] in
    let resolve_source () =
      match get_string "json" args with
      | Some s when String.trim s <> "" -> Ok (s, None)
      | _ ->
          let path_opt =
            match get_string "path" args with
            | Some p when String.trim p <> "" -> Some p
            | _ -> List.find_opt Sys.file_exists default_paths
          in
          (match path_opt with
           | None ->
               Error
                 "Mapping not found. Provide 'json' or 'path' (default search: ./figma-code-connect.json, ./.figma/code-connect.json)."
           | Some path ->
               if not (is_safe_relative_path path) then
                 Error "Unsafe mapping path (must be relative, no '..', no '~', no absolute path)."
               else if not (Sys.file_exists path) then
                 Error (Printf.sprintf "Mapping file not found: %s" path)
               else
                 (match read_file path with
                  | Ok content -> Ok (content, Some path)
                  | Error msg -> Error (Printf.sprintf "Failed to read mapping file: %s" msg)))
    in
    let diag_to_json (d : Figma_code_connect.diagnostic) =
      `Assoc [ ("message", `String d.message); ("path", `String d.path) ]
    in
    let json_of_kv kv =
      `Assoc (List.map (fun (k, v) -> (k, `String v)) kv)
    in
    let json_of_figma (f : Figma_code_connect.figma) =
      `Assoc
        (List.filter_map
           (fun (k, v) -> v |> Option.map (fun v -> (k, `String v)))
           [ ("node_id", f.node_id); ("component_key", f.component_key) ]
         @ [
           ("name", `String f.name);
           ("variant", json_of_kv f.variant);
         ])
    in
    let json_of_code (c : Figma_code_connect.code) =
      `Assoc
        (List.filter_map
           (fun (k, v) -> v |> Option.map (fun v -> (k, `String v)))
           [ ("package", c.package); ("file", c.file) ]
         @ [
           ("export", `String c.export);
           ("props", json_of_kv c.props);
         ])
    in
    let json_of_component (c : Figma_code_connect.component) =
      `Assoc
        [
          ("id", `String c.id);
          ("figma", json_of_figma c.figma);
          ("code", json_of_code c.code);
          ("aliases", `List (List.map (fun s -> `String s) c.aliases));
          ("tags", `List (List.map (fun s -> `String s) c.tags));
        ]
    in
    let parse_mapping_content content =
      try
        let json = Yojson.Safe.from_string content in
        let mapping, parse_errors = Figma_code_connect.parse_json json in
        Ok (mapping, parse_errors)
      with Yojson.Json_error msg ->
        Error (Printf.sprintf "Failed to parse JSON: %s" msg)
    in

    let find_mapping_for_match () =
      match get_string "index_id" args with
      | Some index_id when String.trim index_id <> "" -> (
          match Hashtbl.find_opt code_connect_index_cache index_id with
          | Some mapping -> Ok (mapping, Some index_id, None)
          | None -> Error (Printf.sprintf "Unknown index_id: %s (run mode=index first)" index_id))
      | _ ->
          (match resolve_source () with
           | Ok (content, src_path) ->
               (match parse_mapping_content content with
                | Ok (mapping, parse_errors) ->
                    let semantic_errors = Figma_code_connect.validate mapping in
                    let errors = parse_errors @ semantic_errors in
                    if errors <> [] then
                      let result =
                        `Assoc
                          [
                            ("ok", `Bool false);
                            ("errors", `List (List.map diag_to_json errors));
                            ("warnings", `List []);
                          ]
                      in
                      Error (Yojson.Safe.pretty_to_string result)
                    else Ok (mapping, None, src_path)
                | Error msg -> Error msg)
           | Error msg -> Error msg)
    in

    match mode with
    | "validate" -> (
        match resolve_source () with
        | Ok (content, src_path) -> (
            match parse_mapping_content content with
            | Ok (mapping, parse_errors) ->
                let errors = parse_errors @ Figma_code_connect.validate mapping in
                let result =
                  `Assoc
                    [
                      ("ok", `Bool (errors = []));
                      ("errors", `List (List.map diag_to_json errors));
                      ("warnings", `List []);
                      ("source", (match src_path with Some p -> `String p | None -> `String "inline"));
                    ]
                in
                Ok (make_text_content (Yojson.Safe.pretty_to_string result))
            | Error msg -> Error msg)
        | Error msg -> Error msg)
    | "index" -> (
        match resolve_source () with
        | Ok (content, src_path) -> (
            match parse_mapping_content content with
            | Ok (mapping, parse_errors) ->
                let errors = parse_errors @ Figma_code_connect.validate mapping in
                if errors <> [] then
                  let result =
                    `Assoc
                      [
                        ("ok", `Bool false);
                        ("errors", `List (List.map diag_to_json errors));
                        ("warnings", `List []);
                      ]
                  in
                  Ok (make_text_content (Yojson.Safe.pretty_to_string result))
                else
                  let index_id =
                    match get_string "cache_key" args with
                    | Some k when String.trim k <> "" -> k
                    | _ -> Digest.(to_hex (string content))
                  in
                  code_connect_cache_put index_id mapping;
                  let result =
                    `Assoc
                      [
                        ("ok", `Bool true);
                        ("index_id", `String index_id);
                        ("component_count", `Int (List.length mapping.components));
                        ("warnings", `List []);
                        ("source", (match src_path with Some p -> `String p | None -> `String "inline"));
                      ]
                  in
                  Ok (make_text_content (Yojson.Safe.pretty_to_string result))
            | Error msg -> Error msg)
        | Error msg -> Error msg)
    | "list" -> (
        match find_mapping_for_match () with
        | Ok (mapping, index_id_opt, src_path) ->
            let result =
              `Assoc
                [
                  ("ok", `Bool true);
                  ("index_id", (match index_id_opt with Some id -> `String id | None -> `Null));
                  ("source", (match src_path with Some p -> `String p | None -> `Null));
                  ("component_count", `Int (List.length mapping.components));
                  ("components", `List (List.map json_of_component mapping.components));
                ]
            in
            Ok (make_text_content (Yojson.Safe.pretty_to_string result))
        | Error msg -> Error msg)
    | "match" -> (
        match find_mapping_for_match () with
        | Ok (mapping, index_id_opt, src_path) ->
            let query_node_id = get_string "node_id" args in
            let query_component_key = get_string "component_key" args in
            let query_name = get_string_or "name" "" args in
            let query_variant =
              match get_json "variant" args with
              | Some (`Assoc kv) ->
                  List.filter_map
                    (fun (k, v) ->
                      match v with
                      | `String s -> Some (k, s)
                      | `Int i -> Some (k, string_of_int i)
                      | `Float f -> Some (k, string_of_float f)
                      | _ -> None)
                    kv
              | _ -> []
            in
            let limit = get_int_positive ~min:0 "limit" 3 args in
            let matches =
              Figma_code_connect.choose ~limit ~query_name ~query_variant ~query_node_id
                ~query_component_key mapping.components
            in
            let matches_json =
              `List
                (List.map
                   (fun (score, reason, (c : Figma_code_connect.component)) ->
                     `Assoc
                       [
                         ("mapping_id", `String c.id);
                         ("score", `Float score);
                         ("reason", `String reason);
                         ("code", json_of_code c.code);
                       ])
                   matches)
            in
            let result =
              `Assoc
                [
                  ("ok", `Bool true);
                  ("index_id", (match index_id_opt with Some id -> `String id | None -> `Null));
                  ("source", (match src_path with Some p -> `String p | None -> `Null));
                  ("matches", matches_json);
                ]
            in
            Ok (make_text_content (Yojson.Safe.pretty_to_string result))
        | Error msg -> Error msg)
    | _ -> Error (Printf.sprintf "Unknown mode: %s (use validate|index|match|list)" mode)

(** ============== 핸들러 맵 (Pure Eio) ============== *)

(** 동기 래퍼 - Pure Eio Effect 핸들러로 감싸서 실행 *)
let wrap_sync_pure (f : Yojson.Safe.t -> (Yojson.Safe.t, string) result) : tool_handler_sync =
  fun args ->
    match get_eio_context () with
    | Some ctx ->
        let (Net net) = ctx.net in
        let (Clock clock) = ctx.clock in
        let same_domain = ctx.domain = Domain.self () in
        let run_with sw =
          Figma_effects.run_with_pure_eio_api ~sw ~net ~clock ~client:ctx.client (fun () -> f args)
        in
        if same_domain then
          run_with ctx.sw
        else
          Eio.Switch.run run_with
    | None ->
        Error "Eio context not set - server not properly initialized"

(** 순수 함수 핸들러들 *)
let handle_codegen_sync args : (Yojson.Safe.t, string) result =
  let json_str = get_string "json" args in
  let format = get_string_or "format" "fidelity" args in
  match json_str with
  | None -> Error "Missing required parameter: json"
  | Some json_str ->
      (match process_json_string ~format json_str with
       | Ok result -> Ok (make_text_content result)
       | Error msg -> Error msg)

(** 카테고리 도구 핸들러 - tool 파라미터로 하위 도구 실행 또는 목록 반환 *)
let handle_category category_name args =
  let tool_param = get_string "tool" args in
  let args_param = member "args" args in
  match tool_param with
  | None ->
      (* tool 미지정: 카테고리 도구 목록 반환 *)
      (match List.find_opt (fun c -> c.name = category_name) tool_categories with
       | Some cat ->
           let tools_info = List.map (fun tool_name ->
             `Assoc [
               ("name", `String tool_name);
               ("full_name", `String ("figma_" ^ tool_name));
             ]
           ) cat.tools in
           let info = `Assoc [
             ("category", `String category_name);
             ("description", `String cat.description);
             ("tool_count", `Int (List.length cat.tools));
             ("tools", `List tools_info);
             ("usage", `String (sprintf "figma_%s tool=<tool_name> args={...}" category_name));
           ] in
           Ok (make_text_content (Yojson.Safe.pretty_to_string info))
       | None ->
           Error (sprintf "Unknown category: %s" category_name))
  | Some tool_name ->
      (* tool 지정: 해당 도구 핸들러 호출 *)
      let full_name = "figma_" ^ tool_name in
      match Hashtbl.find_opt handler_registry full_name with
      | Some handler ->
          let actual_args = match args_param with
            | Some a -> a
            | None ->
              (* Forward top-level args (except "tool") for flat argument style *)
              (match args with
               | `Assoc lst -> `Assoc (List.filter (fun (k, _) -> k <> "tool") lst)
               | _ -> `Assoc [])
          in
          handler actual_args
      | None ->
          (* 카테고리에 속하는지 확인 *)
          if find_tool_in_category category_name tool_name then
            Error (sprintf "Tool '%s' exists but handler not found. Try 'figma_%s' directly." tool_name tool_name)
          else
            Error (sprintf "Tool '%s' not found in category '%s'. Use 'figma_%s' to see available tools." tool_name category_name category_name)

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
  ("figma_select_nodes", wrap_sync_pure handle_select_nodes);
  ("figma_get_node_chunk", wrap_sync_pure handle_get_node_chunk);
  ("figma_fidelity_loop", wrap_sync_pure handle_fidelity_loop);
  ("figma_image_similarity", wrap_sync_pure handle_image_similarity);
  ("figma_verify_visual", wrap_sync_pure handle_verify_visual);
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
  ("figma_get_style", wrap_sync_pure handle_get_style);
  (* STRAP 통합: 8개 plugin 핸들러 → 1개 라우터 *)
  ("figma_plugin", wrap_sync_pure handle_figma_plugin);
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
  ("figma_plugin", wrap_sync_pure (handle_category "plugin"));
  ("figma_team", wrap_sync_pure (handle_category "team"));
  ("figma_export", wrap_sync_pure (handle_category "export"));
  ("figma_components", wrap_sync_pure (handle_category "components"));
]

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
- `PlanTasks` supports `recursive=true` to generate divide-and-conquer task lists
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
  Mcp_protocol.create_server
    ~handlers_sync:all_handlers_sync
    ~resource_templates
    public_tools
    resources
    prompts
    read_resource
