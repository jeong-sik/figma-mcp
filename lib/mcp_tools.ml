let string_contains s sub = 
  let len_s = String.length s in 
  let len_sub = String.length sub in 
  if len_sub = 0 then true 
  else if len_sub > len_s then false 
  else 
    let found = ref false in 
    for i = 0 to len_s - len_sub do 
      if not !found then 
        let match_at_i = ref true in 
        for j = 0 to len_sub - 1 do 
          if Char.lowercase_ascii s.[i + j] <> Char.lowercase_ascii sub.[j] then 
            match_at_i := false 
        done; 
        if !match_at_i then found := true 
    done; 
    !found 

let is_network_error exn = 
  match exn with 
  | Unix.Unix_error (Unix.EPIPE, _, _) 
  | Unix.Unix_error (Unix.ECONNRESET, _, _) 
  | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> true 
  | _ -> 
      let s = Printexc.to_string exn in 
      string_contains s "broken pipe" || 
      string_contains s "connection reset" || 
      string_contains s "connection timed out" || 
      string_contains s "econnreset" || 
      string_contains s "epipe" || 
      string_contains s "etimedout" 

(** Figma MCP Tools 정의 *)

open Mcp_protocol
open Mcp_helpers
open Mcp_api_handlers
open Mcp_plugin_handlers
open Mcp_visual_handlers
open Printf

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

(** Semantic Verification - Meaning-first checks (layout/text/style) *)
let tool_figma_verify_semantic : tool_def = {
  name = "figma_verify_semantic";
  description = "✅ VERIFY: Semantic-first 검증 (레이아웃/텍스트/스타일). Figma Design IR(absoluteBoundingBox, text nodes, basic style)과 HTML DOM metrics(Playwright) 비교. SSIM보다 빠르고 덜 flaky하므로 figma_verify_visual 전에 gate로 권장. 반환: passed, score, mismatches.";
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
  ] ["file_key"; "message"; "x"; "y"];
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

let tool_figma_plugin_edit_node : tool_def = {
  name = "figma_plugin_edit_node";
  description = "🔌 PLUGIN WRITE: 기존 노드 속성 수정. fill/stroke/opacity/text/effects/layout/visibility 등.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션, 기본 채널 사용)");
    ("node_id", string_prop "수정할 노드 ID");
    ("properties", object_prop "수정할 속성 딕셔너리. 가능한 키: fill (hex color), stroke (hex color), stroke_weight (number), opacity (0-1), corner_radius (number), effects (array), blend_mode (string), visible (bool), locked (bool), name (string), text (string), font_size (number), text_case (ORIGINAL|UPPER|LOWER|TITLE), auto_layout (HORIZONTAL|VERTICAL|NONE), padding (number), spacing (number), constraints (object)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 10000)");
  ] ["node_id"; "properties"];
}

let tool_figma_plugin_create_node : tool_def = {
  name = "figma_plugin_create_node";
  description = "🔌 PLUGIN WRITE: 새 노드 생성. Frame/Rectangle/Ellipse/Text/Line/Component 등.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("type", enum_prop [
      "frame"; "rectangle"; "ellipse"; "text"; "line";
      "polygon"; "star"; "vector"; "component"; "component_set"; "slice"
    ] "생성할 노드 타입");
    ("parent_id", string_prop "부모 노드 ID (옵션, 기본값: 현재 페이지)");
    ("x", number_prop "X 좌표");
    ("y", number_prop "Y 좌표");
    ("width", number_prop "너비");
    ("height", number_prop "높이");
    ("name", string_prop "노드 이름 (옵션)");
    ("fill", string_prop "채우기 색상 hex (옵션, 예: #FF0000)");
    ("text", string_prop "텍스트 내용 (type=text일 때)");
    ("font_size", number_prop "폰트 크기 (type=text일 때)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 10000)");
  ] ["type"];
}

let tool_figma_plugin_delete_nodes : tool_def = {
  name = "figma_plugin_delete_nodes";
  description = "🔌 PLUGIN WRITE: 노드 삭제. 단일 또는 다수 노드 제거.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("node_ids", array_prop "삭제할 노드 ID 배열");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 10000)");
  ] ["node_ids"];
}

let tool_figma_plugin_batch : tool_def = {
  name = "figma_plugin_batch";
  description = "🔌 PLUGIN WRITE: 여러 작업을 순차 실행. 각 작업은 {action, ...params} 형태.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("actions", array_prop "실행할 작업 배열. 각 항목: {\"action\": \"set_fill\"|\"move\"|\"resize\"|..., \"node_id\": \"...\", ...params}");
    ("stop_on_error", bool_prop "에러 시 중단 여부 (기본값: true)");
    ("timeout_ms", number_prop "전체 대기 시간 (기본값: 30000)");
  ] ["actions"];
}

let tool_figma_plugin_subscribe_events : tool_def = {
  name = "figma_plugin_subscribe_events";
  description = "🔌 PLUGIN EVENTS: Figma 문서 변경/선택 변경 이벤트 구독. poll로 이벤트 수신.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("event_types", array_prop "구독할 이벤트 타입 (옵션, 기본: 전체). 가능: selection_change, document_change, page_change, disconnect");
    ("timeout_ms", number_prop "long-poll 대기 시간 (기본값: 30000)");
    ("max_events", number_prop "최대 반환 이벤트 수 (기본값: 50)");
  ] [];
}

(* STRAP 통합: plugin 도구 통합 (8→14 actions) *)
let tool_figma_plugin : tool_def = {
  name = "figma_plugin";
  description = "🔌 PLUGIN: Figma Desktop 앱과 실시간 연동. action으로 세부 동작 선택. 100개 action 지원. 전용 WRITE 도구도 있음: figma_plugin_edit_node, figma_plugin_create_node, figma_plugin_delete_nodes, figma_plugin_batch.";
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
      "create_component"; "create_instance"; "detach_instance"; "set_text"; "find_all"; "notify";
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
      "get_stroke_details"; "set_stroke_weight"; "collapse_layer";
      "export_viewport"; "export_selection"; "get_changes"; "watch_start"; "watch_stop";
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
    ("component_key", string_prop "외부 라이브러리 컴포넌트 키 (create_instance)");
    ("max_nodes", number_prop "뷰포트 내보내기 시 최대 노드 수 (export_viewport, default 5)");
    ("since", number_prop "이 타임스탬프 이후 변경사항만 반환 (get_changes)");
    ("clear", bool_prop "버퍼 비우기 (get_changes)");
    ("limit", number_prop "반환할 최대 변경사항 수 (get_changes, default 50)");
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
    tools = ["verify_semantic"; "verify_visual"; "image_similarity"; "compare"; "fidelity_loop"; "fidelity_review"] };
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
             "get_component"; "get_component_set"; "get_style"; "get_variables"; "code_connect"] };
]

(** 카테고리에서 도구 찾기 *)
let find_tool_in_category category_name tool_name =
  List.find_opt (fun cat -> cat.name = category_name) tool_categories
  |> Option.map (fun cat -> List.mem tool_name cat.tools)
  |> Option.value ~default:false

(** 최상위 유지 도구 (자주 사용) *)
let featured_tool_names = ["codegen"; "doctor"; "stats"; "cache_stats"; "cache_invalidate"; "read_large_result"; "code_connect"; "error_troubleshoot"; "post_comment"; "get_file_comments"; "plugin"; "plugin_edit_node"; "plugin_create_node"; "plugin_delete_nodes"; "plugin_batch"; "plugin_subscribe_events"]

(** ============== 모든 도구 목록 (내부용) ============== *)

let tool_figma_get_dev_resources : tool_def = {
  name = "figma_get_dev_resources";
  description = "📋 DEV MODE: 노드에 연결된 외부 문서(Jira, Wiki, PR, Storybook 등) 조회. 기획 의도 파악에 필수.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID");
  ] ["file_key"; "node_id"];
}

let tool_figma_add_dev_resource : tool_def = {
  name = "figma_add_dev_resource";
  description = "🔗 DEV MODE: 노드에 외부 링크(문서, PR 등) 추가. 디자인과 문서를 시맨틱하게 연결.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID");
    ("name", string_prop "링크 이름 (예: 기획서)");
    ("url", string_prop "연결할 URL (Jira, Wiki 등)");
  ] ["file_key"; "node_id"; "name"; "url"];
}

let tool_figma_setup_webhook : tool_def = {
  name = "figma_setup_webhook";
  description = "🔔 SYNC: 실시간 디자인 변경 감지를 위한 Webhook 자동 설정. 디자인 수정 시 자동 알림 수신.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("file_key", string_prop "대상 파일 키");
    ("endpoint", string_prop "알림을 받을 서버 URL (기본값: 우리 MCP 서버)");
    ("passcode", string_prop "Webhook 보안 패스코드 (기본값: 랜덤)");
  ] ["team_id"; "file_key"];
}

let tool_figma_annotate : tool_def = {
  name = "figma_annotate";
  description = "✍️ AGENTIC: 피그마 캔버스 특정 위치에 AI 포스트잇 남기기. 디자이너와 협업/질문용.";
  input_schema = object_schema [
    ("channel_id", string_prop "플러그인 채널 ID");
    ("node_id", string_prop "대상 노드 ID (이 노드 옆에 생성)");
    ("message", string_prop "남길 메시지 내용");
    ("color", enum_prop ["yellow"; "blue"; "green"; "red"] "포스트잇 색상 (기본값: yellow)");
  ] ["node_id"; "message"];
}

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
  tool_figma_select_nodes;
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
        ("mode", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "list"; `String "describe"; `String "call"]);
          ("description", `String "동작 모드. 기본(auto): tool/args 유무로 list/describe/call 결정");
        ]);
        ("tool", `Assoc [
          ("type", `String "string");
          ("description", `String "하위 도구 이름 (예: verify_visual). 생략시 목록(list). tool만 주고 args 생략하면 describe로 스키마/설명 반환");
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


(** Node selection helpers, core API handlers: moved to mcp_api_handlers.ml *)

(** Plugin handler builders: moved to mcp_plugin_handlers.ml *)

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

(** mkdir_p: moved to mcp_helpers.ml *)

let normalize_path path =
  try Some (Unix.realpath path) with Unix.Unix_error _ -> None

let is_under_dir ~dir path =
  match (normalize_path dir, normalize_path path) with
  | (Some dir_norm, Some path_norm) ->
      let prefix = if String.ends_with ~suffix:"/" dir_norm then dir_norm else dir_norm ^ "/" in
      path_norm = dir_norm || String.starts_with ~prefix path_norm
  | _ -> false


(** Core Figma API handlers (handle_get_file .. handle_get_style): moved to mcp_api_handlers.ml *)

(** Plugin handlers (handle_plugin_*, handle_figma_plugin, known_plugin_actions,
    suggest_action): moved to mcp_plugin_handlers.ml *)

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

(** figma_crawl_team 핸들러 - 팀 전체 크롤링 + Neo4j 저장 (Effects 기반) *)
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
      (match fetch_file_for_search_cached ~file_key ~token with
       | Ok (json, _source) ->
           (match Figma_api.extract_document json with
            | Some doc_json ->
                let doc_str = Yojson.Safe.to_string doc_json in
                (match Figma_parser.parse_json_string doc_str with
                 | Some root ->
                     (* 모든 노드 수집 *)
                     let all_nodes = Figma_query.collect_nodes ~max_depth:None root in
                     let query_lower = String.lowercase_ascii query |> String.trim in
                     if query_lower = "" then
                       Ok (make_text_content "[]")
                     else
                       let tokens =
                         query_lower
                         |> Str.split (Str.regexp "[ \t\r\n]+")
                         |> List.filter (fun s -> s <> "")
                       in
                       let tokens = if tokens = [] then [query_lower] else tokens in

                       let contains hay needle =
                         try
                           let _ = Str.search_forward (Str.regexp_string needle) hay 0 in
                           true
                         with Not_found -> false
                       in

                       let score_node node =
                         let name_lower = String.lowercase_ascii node.Figma_types.name in
                         let chars_lower =
                           match node.Figma_types.characters with
                           | Some chars -> Some (String.lowercase_ascii chars)
                           | None -> None
                         in

                         let token_in_name tok = contains name_lower tok in
                         let token_in_text tok =
                           match chars_lower with
                           | Some t -> contains t tok
                           | None -> false
                         in

                         let matched_name = List.exists token_in_name tokens in
                         let matched_text = List.exists token_in_text tokens in
                         let matches =
                           match search_in with
                           | "name" -> matched_name
                           | "text" -> matched_text
                           | _ -> matched_name || matched_text
                         in
                         if not matches then
                           None
                         else
                           let base_score =
                             List.fold_left
                               (fun acc tok ->
                                 acc
                                 +. (if token_in_name tok then 3.0 else 0.0)
                                 +. (if token_in_text tok then 1.0 else 0.0))
                               0.0
                               tokens
                           in
                           let exact_bonus =
                             if query_lower <> "" && name_lower = query_lower then 100.0 else 0.0
                           in
                           let prefix_bonus =
                             if query_lower <> ""
                                && String.length name_lower >= String.length query_lower
                                && String.sub name_lower 0 (String.length query_lower) = query_lower
                             then 10.0
                             else 0.0
                           in
                           let matched_in =
                             match (matched_name, matched_text) with
                             | (true, true) -> "both"
                             | (true, false) -> "name"
                             | (false, true) -> "text"
                             | _ -> "both"
                           in
                           Some (base_score +. exact_bonus +. prefix_bonus, matched_in, node)
                       in

                       let scored =
                         all_nodes
                         |> List.filter_map score_node
                         |> List.sort (fun (sa, _, a) (sb, _, b) ->
                              let c = compare sb sa in
                              if c <> 0 then c
                              else
                                let an = String.lowercase_ascii a.Figma_types.name in
                                let bn = String.lowercase_ascii b.Figma_types.name in
                                let cn = String.compare an bn in
                                if cn <> 0 then cn
                                else String.compare a.Figma_types.id b.Figma_types.id)
                         |> List.filteri (fun i _ -> i < limit)
                       in

                       (* Return JSON array in text content for robust client parsing.
                          Each item: {id,name,type,characters,score,matched_in}. *)
                       let items_json =
                         scored
                         |> List.map (fun (score, matched_in, node) ->
                              let type_str =
                                Figma_query.node_type_to_string node.Figma_types.node_type
                              in
                              let chars =
                                match node.Figma_types.characters with
                                | Some c -> truncate_string ~max_len:200 c
                                | None -> ""
                              in
                              `Assoc [
                                ("id", `String node.Figma_types.id);
                                ("name", `String node.Figma_types.name);
                                ("type", `String type_str);
                                ("characters", `String chars);
                                ("score", `Float score);
                                ("matched_in", `String matched_in);
                              ])
                       in
                       Ok (make_text_content (Yojson.Safe.to_string (`List items_json)))
                 | None -> Error "Failed to parse document")
            | None -> Error "Document not found")
       | Error err -> Error err)
  | _ -> Error "Missing required parameters: file_key, token, query"

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
  let mode_param = get_string "mode" args |> Option.map String.lowercase_ascii in
  let tool_param = get_string "tool" args in
  let args_param = member "args" args in

  let find_tool_def (full_name : string) : tool_def option =
    List.find_opt (fun (t : tool_def) -> t.name = full_name) all_detailed_tools
  in

  let effective_mode : [ `List | `Describe | `Call ] =
    match mode_param with
    | Some "list" -> `List
    | Some "describe" -> `Describe
    | Some "call" -> `Call
    | Some other ->
        (* Fail fast for invalid modes: prevents accidental calls when user mistypes. *)
        raise (Invalid_argument (sprintf "Invalid mode: %s (use list|describe|call)" other))
    | None ->
        match tool_param, args_param with
        | None, _ -> `List
        | Some _, None -> `Describe
        | Some _, Some _ -> `Call
  in

  try
    match effective_mode with
    | `List ->
        (match List.find_opt (fun c -> c.name = category_name) tool_categories with
         | Some cat ->
             let tools_info =
               List.map (fun tool_name ->
                 let full_name = "figma_" ^ tool_name in
                 let desc =
                   match find_tool_def full_name with
                   | Some t -> `String t.description
                   | None -> `Null
                 in
                 `Assoc [
                   ("name", `String tool_name);
                   ("full_name", `String full_name);
                   ("description", desc);
                 ]
               ) cat.tools
             in
             let info =
               `Assoc [
                 ("category", `String category_name);
                 ("description", `String cat.description);
                 ("tool_count", `Int (List.length cat.tools));
                 ("tools", `List tools_info);
                 ("usage", `String (sprintf "figma_%s mode=call tool=<tool_name> args={...}" category_name));
                 ("usage_describe", `String (sprintf "figma_%s mode=describe tool=<tool_name>" category_name));
               ]
             in
             Ok (make_text_content (Yojson.Safe.pretty_to_string info))
         | None ->
             Error (sprintf "Unknown category: %s" category_name))

    | `Describe ->
        (match tool_param with
         | None ->
             Error (sprintf "Missing required parameter: tool (category=%s)" category_name)
         | Some tool_name ->
             if not (find_tool_in_category category_name tool_name) then
               Error (sprintf "Tool '%s' not found in category '%s'. Use figma_%s mode=list to see available tools." tool_name category_name category_name)
             else
               let full_name = "figma_" ^ tool_name in
               (match find_tool_def full_name with
                | Some t ->
                    let info =
                      `Assoc [
                        ("category", `String category_name);
                        ("name", `String tool_name);
                        ("full_name", `String full_name);
                        ("description", `String t.description);
                        ("input_schema", t.input_schema);
                        ("usage", `String (sprintf "figma_%s mode=call tool=%s args={...}" category_name tool_name));
                      ]
                    in
                    Ok (make_text_content (Yojson.Safe.pretty_to_string info))
                | None ->
                    Error (sprintf "Tool '%s' exists but tool definition not found. Try 'tools/list'." full_name)))

    | `Call ->
        (match tool_param with
         | None ->
             Error (sprintf "Missing required parameter: tool (category=%s)" category_name)
         | Some tool_name ->
             if not (find_tool_in_category category_name tool_name) then
               Error (sprintf "Tool '%s' not found in category '%s'. Use figma_%s mode=list to see available tools." tool_name category_name category_name)
             else
               let full_name = "figma_" ^ tool_name in
               match Hashtbl.find_opt handler_registry full_name with
               | Some handler ->
                   (match args_param with
                    | None ->
                        Error "Missing required parameter: args (mode=call)"
                    | Some actual_args ->
                        handler actual_args)
               | None ->
                   Error (sprintf "Tool '%s' exists but handler not found. Try 'figma_%s' directly." tool_name tool_name))
  with
  | Invalid_argument msg -> Error msg

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
