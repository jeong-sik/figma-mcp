let is_network_error exn =
  let sc = Mcp_helpers.string_contains in
  match exn with
  | Unix.Unix_error (Unix.EPIPE, _, _)
  | Unix.Unix_error (Unix.ECONNRESET, _, _)
  | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> true
  | _ ->
      let s = Printexc.to_string exn in
      sc ~haystack:s ~needle:"broken pipe" ||
      sc ~haystack:s ~needle:"connection reset" ||
      sc ~haystack:s ~needle:"connection timed out" ||
      sc ~haystack:s ~needle:"econnreset" ||
      sc ~haystack:s ~needle:"epipe" ||
      sc ~haystack:s ~needle:"etimedout"

(** Figma MCP Tools 정의 *)

open Figma_mcp_protocol
open Mcp_helpers
open Printf

(** ============== Tool 정의 ============== *)

let tool_figma_codegen : tool_def = {
  name = "figma_codegen";
  description = Some "[Advanced] JSON을 Fidelity DSL로 변환. 보통 figma_get_node가 자동 처리.";
  input_schema = object_schema [
    ("json", string_prop "Figma JSON 데이터 (document 노드 또는 전체 응답)");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷: fidelity (정확도 우선), raw (원본 JSON), html (HTML 프리뷰)");
  ] ["json"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_file : tool_def = {
  name = "figma_get_file";
  description = Some "[Advanced] 전체 파일 데이터. 대용량 주의. 보통 figma_get_node로 충분.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키 (URL에서 추출: figma.com/file/KEY/...)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["fidelity"; "raw"; "html"] "출력 포맷 (기본값: fidelity)");
    ("depth", number_prop "트리 깊이 제한 (Figma API depth 파라미터)");
    ("geometry", enum_prop ["paths"] "벡터 경로 포함 (geometry=paths)");
    ("plugin_data", string_prop "plugin_data 파라미터 (쉼표 구분 plugin ID 또는 shared)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_file_meta : tool_def = {
  name = "figma_get_file_meta";
  description = Some "📋 QUICK: 파일의 컴포넌트/스타일 메타데이터. 디자인 시스템 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_list_screens : tool_def = {
  name = "figma_list_screens";
  description = Some "📋 QUICK: 파일의 화면/Frame/Component 목록. 탐색 시작점 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_node : tool_def = {
  name = "figma_get_node";
  description = Some "🎯 CORE: Figma 노드를 Fidelity DSL로 변환. ⚠️ REQUIRED: url 또는 (file_key + node_id) 중 하나를 반드시 제공하세요. 대형 노드는 depth 제한 권장. 반환: DSL 문자열 + 구조 정보. (전체 재귀는 gRPC GetNodeStream recursive 사용)";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_node_bundle : tool_def = {
  name = "figma_get_node_bundle";
  description = Some "📦 RECOMMENDED: 구현에 필요한 모든 정보를 한번에. ⚠️ REQUIRED: url 또는 (file_key + node_id) 중 하나를 반드시 제공하세요. DSL + 렌더 이미지 + 변수 + 이미지 fills. download=true로 에셋 저장. 반환: 번들 JSON.";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** 경량 구조 요약 - 큰 노드를 탐색할 때 전체 로드 없이 구조 파악 *)
let tool_figma_get_node_summary : tool_def = {
  name = "figma_get_node_summary";
  description = Some "📋 QUICK: 대형 노드 탐색 전 구조 파악. ⚠️ REQUIRED: url 또는 (file_key + node_id) 중 하나를 반드시 제공하세요. 전체 로드 없이 자식 목록/타입/크기만. Outside-In 패턴의 첫 단계. 반환: children 배열 (id, name, type, size).";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("max_children", number_prop "반환할 최대 자식 수 (기본값: 50)");
    ("version", string_prop "특정 파일 버전 ID");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_planning_context : tool_def = {
  name = "figma_get_planning_context";
  description = Some "🎯 AGENT: heuristic-free planning context. 상위 에이전트가 직접 계획을 세울 수 있도록 정렬되지 않은 노드 후보, 트리 edge, preview, feature summary를 제공합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("summary_depth", number_prop "분석 depth (기본값: 2, 최대: 6)");
    ("preview", bool_prop "preview 이미지 포함 여부 (기본값: true)");
    ("preview_format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "프리뷰 이미지 포맷 (기본값: png)");
    ("preview_scale", number_prop "프리뷰 이미지 스케일 (기본값: 1)");
    ("note_patterns", array_prop "note로 따로 태깅할 패턴 (옵션, 미지정 시 비활성)");
    ("exclude_patterns", array_prop "candidate에서 분리할 패턴 (옵션, 미지정 시 비활성)");
    ("version", string_prop "특정 파일 버전 ID");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** 노드 자동 선택 - 점수 기반 후보 선별 *)
let tool_figma_select_nodes : tool_def = {
  name = "figma_select_nodes";
  description = Some "[DEPRECATED] heuristic 후보 점수화 선택. 기본 비활성화 경로입니다. 새 경로는 figma_get_planning_context + agent planning + figma_validate_agent_plan 조합을 사용하세요.";
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
    ("allow_legacy", bool_prop "임시 legacy opt-in. true면 기본 차단을 우회합니다.");
    ("version", string_prop "특정 파일 버전 ID");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_validate_agent_plan : tool_def = {
  name = "figma_validate_agent_plan";
  description = Some "✅ AGENT: 상위 에이전트가 만든 plan JSON을 deterministic하게 검증합니다. node 존재 여부, dependency cycle, duplicate id, 루트 밖 참조를 확인합니다.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "루트 노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (file_key/node_id 자동 추출)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("summary_depth", number_prop "검증용 depth (기본값: 6, 최대: 6)");
    ("plan", object_prop "검증할 plan JSON. 최소 tasks 배열이 필요하며 각 task는 id, node_id, depends_on를 가질 수 있습니다.");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["plan"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** 깊이 범위별 청크 로드 - 대형 노드를 점진적으로 로드 *)
let tool_figma_get_node_chunk : tool_def = {
  name = "figma_get_node_chunk";
  description = Some "📦 CHUNK: 깊이 범위별 노드 로드. ⚠️ REQUIRED: url 또는 (file_key + node_id) 중 하나를 반드시 제공하세요. 대형 노드 점진적 탐색. depth_start=0, depth_end=2면 2단계까지.";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_fidelity_loop : tool_def = {
  name = "figma_fidelity_loop";
  description = Some "🔄 AUTO: fidelity 점수 미달 시 depth/geometry 자동 증가. ⚠️ REQUIRED: url 또는 (file_key + node_id) 중 하나를 반드시 제공하세요. 목표 달성까지 반복.";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_image_similarity : tool_def = {
  name = "figma_image_similarity";
  description = Some "✅ VERIFY: 두 노드의 렌더 이미지를 SSIM/PSNR로 비교. scale 자동 증가로 목표 SSIM 달성.";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** Visual Feedback Loop - 코드 생성 및 시각적 검증 *)
let tool_figma_verify_visual : tool_def = {
  name = "figma_verify_visual";
  description = Some "✅ VERIFY: HTML을 Figma 렌더와 SSIM 비교. target_ssim=0.95 기본. 미달 시 CSS 자동 조정 (max 3회). html_screenshot으로 Chrome MCP 연동 가능. 반환: ssim_score, text_match, overall_passed, evolution_dir.";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** Semantic Verification - Meaning-first checks (layout/text/style) *)
let tool_figma_verify_semantic : tool_def = {
  name = "figma_verify_semantic";
  description = Some "✅ VERIFY: Semantic-first 검증 (레이아웃/텍스트/스타일). Figma Design IR(absoluteBoundingBox, text nodes, basic style)과 HTML DOM metrics(Playwright) 비교. SSIM보다 빠르고 덜 flaky하므로 figma_verify_visual 전에 gate로 권장. 반환: passed, score, mismatches.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("html", string_prop "검증할 HTML 코드 또는 파일 경로");
    ("width", number_prop "뷰포트 너비 (기본값: 375)");
    ("height", number_prop "뷰포트 높이 (기본값: 812)");
    ("score_threshold", number_prop "통과 score threshold (0-1, 기본값: 0.90)");
    ("text_bbox_tol_px", number_prop "텍스트 bbox 허용 오차(px, 기본값: 4.0)");
    ("font_size_tol_px", number_prop "폰트 크기 허용 오차(px, 기본값: 1.5)");
    ("font_weight_tol", number_prop "폰트 weight 허용 오차(기본값: 150)");
    ("text_color_tol_rgb", number_prop "텍스트 색상 RGB 거리 허용치(0-1, 기본값: 0.15)");
    ("version", string_prop "특정 파일 버전 ID");
  ] ["file_key"; "node_id"; "html"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_export_image : tool_def = {
  name = "figma_export_image";
  description = Some "🖼️ ASSET: 노드 이미지 내보내기 URL. 에셋 다운로드용.";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** Smart export - 자동 scale 조정 및 재귀 분할 지원 *)
let tool_figma_export_smart : tool_def = {
  name = "figma_export_smart";
  description = Some "🖼️ ASSET: 대형 노드 스마트 내보내기. max_pixels 초과 시 scale 조정 또는 분할.";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_image_fills : tool_def = {
  name = "figma_get_image_fills";
  description = Some "🖼️ ASSET: 이미지 채움(fills) URL 맵 반환. 에셋 수집용.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("version", string_prop "특정 파일 버전 ID");
    ("download", bool_prop "이미지 다운로드 여부 (기본값: false)");
    ("save_dir", string_prop "다운로드 저장 경로 (기본값: ~/me/download/figma-assets)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_nodes : tool_def = {
  name = "figma_get_nodes";
  description = Some "📦 BATCH: 여러 노드 ID 데이터를 한번에. 반복 API 호출 절약.";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_file_versions : tool_def = {
  name = "figma_get_file_versions";
  description = Some "[Advanced] 파일 버전 목록. 히스토리 추적용.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_file_comments : tool_def = {
  name = "figma_get_file_comments";
  description = Some "[Advanced] 파일 코멘트 목록. 협업 히스토리 조회.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_post_comment : tool_def = {
  name = "figma_post_comment";
  description = Some "[Advanced] 파일에 코멘트 추가. 협업/피드백용.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("message", string_prop "코멘트 내용");
    ("x", number_prop "캔버스 좌표 x (client_meta)");
    ("y", number_prop "캔버스 좌표 y (client_meta)");
    ("node_id", string_prop "연결할 노드 ID (옵션)");
  ] ["file_key"; "message"; "x"; "y"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_file_components : tool_def = {
  name = "figma_get_file_components";
  description = Some "📋 QUICK: 파일의 컴포넌트 목록. 디자인 시스템 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_team_components : tool_def = {
  name = "figma_get_team_components";
  description = Some "[DEPRECATED] 팀 레벨 API는 Rate Limit이 엄격합니다. 대신 figma_get_file_components를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_file_component_sets : tool_def = {
  name = "figma_get_file_component_sets";
  description = Some "📋 QUICK: 파일의 컴포넌트 셋(Variants). 디자인 시스템 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_team_component_sets : tool_def = {
  name = "figma_get_team_component_sets";
  description = Some "[DEPRECATED] 팀 레벨 API는 Rate Limit이 엄격합니다. 대신 figma_get_file_component_sets를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_file_styles : tool_def = {
  name = "figma_get_file_styles";
  description = Some "📋 QUICK: 파일의 스타일 목록. 디자인 토큰 파악.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_team_styles : tool_def = {
  name = "figma_get_team_styles";
  description = Some "[DEPRECATED] 팀 레벨 API는 Rate Limit이 엄격합니다. 대신 figma_get_file_styles 또는 figma_export_tokens를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_component : tool_def = {
  name = "figma_get_component";
  description = Some "[DEPRECATED] 개별 키 조회는 비효율적입니다. 대신 figma_get_file_components를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("component_key", string_prop "컴포넌트 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["component_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_component_set : tool_def = {
  name = "figma_get_component_set";
  description = Some "[DEPRECATED] 개별 키 조회는 비효율적입니다. 대신 figma_get_file_component_sets를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("component_set_key", string_prop "컴포넌트 셋 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["component_set_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_style : tool_def = {
  name = "figma_get_style";
  description = Some "[DEPRECATED] 개별 키 조회는 비효율적입니다. 대신 figma_get_file_styles 또는 figma_export_tokens를 사용하세요. 향후 버전에서 제거될 수 있습니다.";
  input_schema = object_schema [
    ("style_key", string_prop "스타일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["style_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** ============== Plugin Bridge 도구 ============== *)

let tool_figma_plugin_connect = Mcp_plugin_tool_defs.tool_figma_plugin_connect
let tool_figma_plugin_use_channel = Mcp_plugin_tool_defs.tool_figma_plugin_use_channel
let tool_figma_plugin_status = Mcp_plugin_tool_defs.tool_figma_plugin_status
let tool_figma_plugin_read_selection = Mcp_plugin_tool_defs.tool_figma_plugin_read_selection
let tool_figma_plugin_get_node = Mcp_plugin_tool_defs.tool_figma_plugin_get_node
let tool_figma_plugin_export_node_image = Mcp_plugin_tool_defs.tool_figma_plugin_export_node_image
let tool_figma_plugin_get_variables = Mcp_plugin_tool_defs.tool_figma_plugin_get_variables
let tool_figma_plugin_apply_ops = Mcp_plugin_tool_defs.tool_figma_plugin_apply_ops
let tool_figma_plugin_edit_node = Mcp_plugin_tool_defs.tool_figma_plugin_edit_node
let tool_figma_plugin_create_node = Mcp_plugin_tool_defs.tool_figma_plugin_create_node
let tool_figma_plugin_delete_nodes = Mcp_plugin_tool_defs.tool_figma_plugin_delete_nodes
let tool_figma_plugin_batch = Mcp_plugin_tool_defs.tool_figma_plugin_batch
let tool_figma_plugin_subscribe_events = Mcp_plugin_tool_defs.tool_figma_plugin_subscribe_events
let tool_figma_export_tokens_plugin = Mcp_plugin_tool_defs.tool_figma_export_tokens_plugin
let tool_figma_plugin = Mcp_plugin_tool_defs.tool_figma_plugin

(** ============== Phase 1: 탐색 도구 ============== *)

let tool_figma_parse_url : tool_def = {
  name = "figma_parse_url";
  description = Some "🎯 CORE: URL 파싱 (Parse Don't Validate). file_key/node_id 추출. API 호출 없음.";
  input_schema = object_schema [
    ("url", string_prop "Figma URL (팀/프로젝트/파일/노드 페이지 모두 지원)");
  ] ["url"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_me : tool_def = {
  name = "figma_get_me";
  description = Some "📋 QUICK: 인증 사용자 정보. 토큰 유효성 확인용.";
  input_schema = object_schema [
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_list_projects : tool_def = {
  name = "figma_list_projects";
  description = Some "📋 QUICK: 팀의 프로젝트 목록. 탐색 시작점 파악.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID (URL에서 추출 또는 figma_parse_url 사용)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["team_id"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_list_files : tool_def = {
  name = "figma_list_files";
  description = Some "📋 QUICK: 프로젝트의 파일 목록. 탐색 시작점 파악.";
  input_schema = object_schema [
    ("project_id", string_prop "프로젝트 ID");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
  ] ["project_id"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_crawl_team : tool_def = {
  name = "figma_crawl_team";
  description = Some "🕷️ CRAWL: 팀 전체를 재귀적으로 크롤링하여 Neo4j에 저장. Team→Projects→Files→Nodes 구조.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("team_name", string_prop "팀 이름 (선택, 그래프 노드에 표시)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("neo4j_uri", string_prop "Neo4j URI (기본값: NEO4J_URI 환경변수)");
    ("neo4j_user", string_prop "Neo4j 사용자 (기본값: NEO4J_USER 환경변수)");
    ("neo4j_password", string_prop "Neo4j 비밀번호 (기본값: NEO4J_PASSWORD 환경변수)");
    ("max_depth", number_prop "노드 탐색 최대 깊이 (기본값: 10)");
    ("rate_limit_ms", number_prop "API 호출 간 대기 시간 ms (기본값: 100)");
  ] ["team_id"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_team_tree : tool_def = {
  name = "figma_team_tree";
  description = Some "🌳 TREE: 팀 구조를 ASCII 트리로 출력 (Neo4j 불필요). Team→Projects→Files 빠른 탐색.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("team_name", string_prop "팀 이름 (선택)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("include_nodes", bool_prop "파일 내 노드 포함 여부 (기본값: false)");
    ("node_depth", number_prop "노드 탐색 깊이 (기본값: 2, include_nodes=true일 때만 적용)");
  ] ["team_id"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_export_team : tool_def = {
  name = "figma_export_team";
  description = Some "💾 EXPORT: 팀 전체를 파일 시스템으로 내보내기. 프로젝트/파일/노드를 JSON 파일로 저장.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("team_name", string_prop "팀 이름 (선택)");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("output_dir", string_prop "출력 디렉토리 경로 (필수)");
    ("max_depth", number_prop "노드 깊이 (0=파일만, 1=페이지까지, 2+=노드까지, 기본값: 2)");
  ] ["team_id"; "output_dir"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_variables : tool_def = {
  name = "figma_get_variables";
  description = Some "📦 TOKENS: 파일의 디자인 토큰/변수. 색상, 타이포, 간격 등.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["summary"; "raw"; "resolved"] "출력 포맷 (기본값: summary)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** ============== Phase 2: 고급 쿼리 도구 ============== *)

let tool_figma_query : tool_def = {
  name = "figma_query";
  description = Some "🎛️ CORE: SQL WHERE처럼 조건 필터링. type=FRAME, width_min=300, color=#FF0000 등 조합. 특정 크기 버튼, 특정 색상 요소 찾기. 반환: 매칭 노드 목록 (id, name, bounds).";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_search : tool_def = {
  name = "figma_search";
  description = Some "🔍 CORE: 파일 내 노드를 텍스트/이름으로 검색. '버튼', 'Header' 등 키워드로 관련 노드 찾기. 반환: node_id, name, type, 좌표 목록. figma_get_node로 상세 조회 연계.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("query", string_prop "검색어 (텍스트 내용 또는 노드 이름)");
    ("search_in", enum_prop ["name"; "text"; "both"] "검색 대상 (기본값: both)");
    ("limit", number_prop "결과 개수 제한 (기본값: 20)");
  ] ["file_key"; "query"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_compare : tool_def = {
  name = "figma_compare";
  description = Some "✅ VERIFY: 통합 비교 도구. mode 필수. ▸general: file_key + node_a_id + node_b_id ▸batch: file_key (+ web_prefix/mobile_prefix) ▸regions: image_a + image_b + regions JSON ▸elements: type + color1/color2 또는 box1/box2 ▸evolution: run_dir (없으면 목록 반환)";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_tree : tool_def = {
  name = "figma_tree";
  description = Some "🌳 CORE: 노드 계층 구조를 ASCII 트리로 시각화. 파일/노드 구조 파악에 필수. style: ascii(기본), indent, compact. max_depth로 깊이 제한. 반환: 트리 문자열 + 노드 수 통계.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("node_id", string_prop "시작 노드 ID (생략시 전체 문서)");
    ("style", enum_prop ["ascii"; "indent"; "compact"] "출력 스타일 (기본값: ascii)");
    ("max_depth", number_prop "최대 깊이 (기본값: 무제한)");
    ("show_size", bool_prop "크기 표시 (기본값: true)");
    ("show_stats", bool_prop "통계 포함 (기본값: false)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_stats : tool_def = {
  name = "figma_stats";
  description = Some "📊 REPORT: 파일 디자인 통계. 색상/폰트/크기/컴포넌트 현황.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("node_id", string_prop "분석 시작 노드 ID (생략시 전체 문서)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_export_tokens : tool_def = {
  name = "figma_export_tokens";
  description = Some "📦 TOKENS: 디자인 토큰 추출. CSS/Tailwind/JSON/SwiftUI/Compose/Flutter/W3C-DTCG/semantic 지원.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("token", string_prop "Figma Personal Access Token (optional if FIGMA_TOKEN env var is set)");
    ("format", enum_prop ["css"; "tailwind"; "json"; "dtcg"; "swiftui"; "compose"; "flutter"; "semantic"] "출력 포맷 (기본값: css). dtcg=W3C Design Tokens (DTCG). semantic=UIFormer 스타일 DSL");
    ("node_id", string_prop "추출 시작 노드 ID (생략시 전체 문서)");
  ] ["file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** 환경/의존성 점검 도구 *)
let tool_figma_doctor : tool_def = {
  name = "figma_doctor";
  description = Some "🔧 UTIL: 로컬 의존성 점검. Node/Playwright/ImageMagick 확인.";
  input_schema = object_schema [] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** large_result 파일 읽기 *)
let tool_figma_read_large_result : tool_def = {
  name = "figma_read_large_result";
  description = Some "🔧 UTIL: large_result 파일 읽기. offset/limit로 분할 읽기.";
  input_schema = object_schema [
    ("file_path", string_prop "large_result file_path");
    ("offset", number_prop "읽기 시작 바이트 (기본값: 0)");
    ("limit", number_prop "최대 읽기 바이트 (기본값: 20000)");
  ] ["file_path"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** 캐시 관리 도구 *)
let tool_figma_cache_stats : tool_def = {
  name = "figma_cache_stats";
  description = Some "🔧 UTIL: 캐시 통계. L1(메모리)+L2(파일) 엔트리/TTL 정보.";
  input_schema = object_schema [] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_cache_invalidate : tool_def = {
  name = "figma_cache_invalidate";
  description = Some "🔧 UTIL: 캐시 무효화. file_key/node_id로 범위 지정.";
  input_schema = object_schema [
    ("file_key", string_prop "무효화할 파일 키 (생략시 전체)");
    ("node_id", string_prop "무효화할 노드 ID (생략시 해당 파일 전체)");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

(** Code Connect-style component mapping (repo-local) *)
let tool_figma_code_connect : tool_def = {
  name = "figma_code_connect";
  description = Some "🔗 CODE: Code Connect-style component mapping. mode 필수. ▸validate: path 또는 json으로 매핑 파일 검증 ▸index: path 또는 json으로 인덱스 생성 (index_id 반환) ▸match: node_id/component_key/name 중 하나로 매칭 (index_id 권장) ▸list: 등록된 매핑 목록 조회";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
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
             "get_planning_context"; "validate_agent_plan";
             "get_node_chunk"; "get_node_summary"; "list_screens";
             "tree"; "get_file_versions"; "parse_url"; "get_me"; "query"; "search"] };
  { name = "visual";
    description = "시각 검증 (SSIM, 비교)";
    tools = ["verify_semantic"; "verify_visual"; "image_similarity"; "compare"; "fidelity_loop"] };
  (* plugin: monolithic tool로 직접 노출 (sub-handlers 미등록으로 category 라우팅 불가) *)
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
             "get_component"; "get_component_set"; "get_variables"] };
  { name = "code";
    description = "코드 생성/연결";
    tools = ["codegen"; "code_connect"] };
]

(** 카테고리에서 도구 찾기 *)
let find_tool_in_category category_name tool_name =
  List.find_opt (fun cat -> cat.name = category_name) tool_categories
  |> Option.map (fun cat -> List.mem tool_name cat.tools)
  |> Option.value ~default:false

(** 최상위 유지 도구 (자주 사용) *)
let featured_tool_names = ["codegen"; "doctor"; "stats"; "cache_stats"; "cache_invalidate"; "read_large_result"; "code_connect"; "post_comment"; "get_file_comments"; "plugin"; "plugin_edit_node"; "plugin_create_node"; "plugin_delete_nodes"; "plugin_batch"; "plugin_subscribe_events"; "get_planning_context"; "validate_agent_plan"]

(** ============== 모든 도구 목록 (내부용) ============== *)

let tool_figma_get_dev_resources = Mcp_plugin_tool_defs.tool_figma_get_dev_resources
let tool_figma_add_dev_resource = Mcp_plugin_tool_defs.tool_figma_add_dev_resource
let tool_figma_setup_webhook = Mcp_plugin_tool_defs.tool_figma_setup_webhook
let tool_figma_annotate = Mcp_plugin_tool_defs.tool_figma_annotate

let all_detailed_tools = [
  (* New Honey Features *)
  tool_figma_get_dev_resources;
  tool_figma_add_dev_resource;
  tool_figma_setup_webhook;
  tool_figma_annotate;
  (* 기존 도구 *)
  tool_figma_codegen;
  tool_figma_get_file;
  tool_figma_get_file_meta;
  tool_figma_list_screens;
  tool_figma_get_node;
  tool_figma_get_node_bundle;
  tool_figma_get_node_summary;
  tool_figma_get_planning_context;
  tool_figma_select_nodes;
  tool_figma_validate_agent_plan;
  tool_figma_get_node_chunk;
  tool_figma_fidelity_loop;
  tool_figma_image_similarity;
  tool_figma_verify_visual;
  tool_figma_verify_semantic;
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
  (* STRAP 통합: 8개 plugin 도구 → 1개 + 4개 전용 mutation 도구 *)
  tool_figma_plugin_edit_node;
  tool_figma_plugin_create_node;
  tool_figma_plugin_delete_nodes;
  tool_figma_plugin_batch;
  tool_figma_plugin_subscribe_events;
  tool_figma_plugin;
  (* Plugin Enhancement Tools *)
  tool_figma_export_tokens_plugin;
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
  make_tool_def
    ~name:("figma_" ^ cat.name)
    ~description:(sprintf "[Category] %s. 도구: %s" cat.description tool_list)
    ~input_schema:(object_schema [
      ("mode", enum_prop ["list"; "describe"; "call"]
        "동작 모드. 생략 시 자동 판별: tool 미지정→list, tool만 지정→describe, tool+args 지정→call. 잘못된 mode 값은 에러 반환");
      ("tool", string_prop
        "하위 도구 이름 (figma_ 접두사 제외, 예: get_me). mode=describe 및 mode=call 시 필수");
      ("args", object_prop
        "도구에 전달할 인자 객체. mode=call 시 필수. 인자 없는 도구도 빈 객체 {} 전달 필요");
    ] [])

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
