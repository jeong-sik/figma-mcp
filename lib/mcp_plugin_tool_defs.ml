open Figma_mcp_protocol
open Mcp_helpers

let tool_figma_plugin_connect : tool_def = {
  name = "figma_plugin_connect";
  description = Some "🔌 PLUGIN: 채널 생성/연결. 실시간 동기화의 시작점.";
  input_schema = object_schema [
    ("channel_id", string_prop "기존 채널 ID (옵션)");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_use_channel : tool_def = {
  name = "figma_plugin_use_channel";
  description = Some "🔌 PLUGIN: 기본 채널 ID 설정. 매번 ID 지정 불필요.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID");
  ] ["channel_id"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_status : tool_def = {
  name = "figma_plugin_status";
  description = Some "🔌 PLUGIN: 채널 상태 확인. 연결 디버깅용.";
  input_schema = object_schema [] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_read_selection : tool_def = {
  name = "figma_plugin_read_selection";
  description = Some "🔌 PLUGIN: 현재 선택 노드 정보. Desktop 앱과 연동.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("depth", number_prop "자식 탐색 깊이 (기본값: 6)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_get_node : tool_def = {
  name = "figma_plugin_get_node";
  description = Some "🔌 PLUGIN: 특정 노드 정보. REST API 보다 빠름. ⚠️ REQUIRED: node_id 또는 url 중 하나를 반드시 제공하세요.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (node_id 자동 추출)");
    ("depth", number_prop "자식 탐색 깊이 (기본값: 6)");
    ("include_geometry", bool_prop "벡터/지오메트리 포함 여부 (기본값: true)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_export_node_image : tool_def = {
  name = "figma_plugin_export_node_image";
  description = Some "🔌 PLUGIN: exportAsync로 이미지 내보내기. base64 반환. ⚠️ REQUIRED: node_id 또는 url 중 하나를 반드시 제공하세요.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("node_id", string_prop "노드 ID (예: 123:456)");
    ("url", string_prop "Figma URL (node_id 자동 추출)");
    ("format", enum_prop ["png"; "jpg"; "svg"; "pdf"] "이미지 포맷 (기본값: png)");
    ("scale", number_prop "스케일 (기본값: 1)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_get_variables : tool_def = {
  name = "figma_plugin_get_variables";
  description = Some "🔌 PLUGIN: Variables API로 로컬 변수/컬렉션. 디자인 토큰 추출.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_apply_ops : tool_def = {
  name = "figma_plugin_apply_ops";
  description = Some "🔌 PLUGIN: 노드 생성/수정/삭제. 실시간 디자인 편집.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("ops", array_prop "작업 목록 (create/update/delete 오브젝트 배열)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] ["ops"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_edit_node : tool_def = {
  name = "figma_plugin_edit_node";
  description = Some "🔌 PLUGIN WRITE: 기존 노드 속성 수정. fill/stroke/opacity/text/effects/layout/visibility 등.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션, 기본 채널 사용)");
    ("node_id", string_prop "수정할 노드 ID");
    ("properties", object_prop "수정할 속성 딕셔너리. 가능한 키: fill (hex color), stroke (hex color), stroke_weight (number), opacity (0-1), corner_radius (number), effects (array), blend_mode (string), visible (bool), locked (bool), name (string), text (string), font_size (number), text_case (ORIGINAL|UPPER|LOWER|TITLE), auto_layout (HORIZONTAL|VERTICAL|NONE), padding (number), spacing (number), constraints (object)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 10000)");
  ] ["node_id"; "properties"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_create_node : tool_def = {
  name = "figma_plugin_create_node";
  description = Some "🔌 PLUGIN WRITE: 새 노드 생성. Frame/Rectangle/Ellipse/Text/Line/Component 등.";
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
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_delete_nodes : tool_def = {
  name = "figma_plugin_delete_nodes";
  description = Some "🔌 PLUGIN WRITE: 노드 삭제. 단일 또는 다수 노드 제거.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("node_ids", array_prop "삭제할 노드 ID 배열");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 10000)");
  ] ["node_ids"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_batch : tool_def = {
  name = "figma_plugin_batch";
  description = Some "🔌 PLUGIN WRITE: 여러 작업을 순차 실행. 각 작업은 {action, ...params} 형태.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("actions", array_prop "실행할 작업 배열. 각 항목: {\"action\": \"set_fill\"|\"move\"|\"resize\"|..., \"node_id\": \"...\", ...params}");
    ("stop_on_error", bool_prop "에러 시 중단 여부 (기본값: true)");
    ("timeout_ms", number_prop "전체 대기 시간 (기본값: 30000)");
  ] ["actions"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin_subscribe_events : tool_def = {
  name = "figma_plugin_subscribe_events";
  description = Some "🔌 PLUGIN EVENTS: Figma 문서 변경/선택 변경 이벤트 구독. poll로 이벤트 수신.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("event_types", array_prop "구독할 이벤트 타입 (옵션, 기본: 전체). 가능: selection_change, document_change, page_change, disconnect");
    ("timeout_ms", number_prop "long-poll 대기 시간 (기본값: 30000)");
    ("max_events", number_prop "최대 반환 이벤트 수 (기본값: 50)");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_export_tokens_plugin : tool_def = {
  name = "figma_export_tokens_plugin";
  description = Some "🔌 PLUGIN TOKENS: Plugin Bridge를 통한 디자인 토큰 추출. 색상, 타이포, 간격, 그림자 등.";
  input_schema = object_schema [
    ("channel_id", string_prop "채널 ID (옵션)");
    ("format", enum_prop ["json"; "css"; "scss"] "출력 포맷 (기본값: json)");
    ("timeout_ms", number_prop "응답 대기 시간 (기본값: 20000)");
  ] [];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_plugin : tool_def = {
  name = "figma_plugin";
  description = Some "🔌 PLUGIN: Figma Desktop 앱과 실시간 연동. action으로 세부 동작 선택. 106개 action 지원. 전용 WRITE 도구: figma_plugin_edit_node, figma_plugin_create_node, figma_plugin_delete_nodes, figma_plugin_batch.";
  input_schema = object_schema [
    ("action", enum_prop
      Mcp_plugin_actions.figma_plugin_action_values
      "106개 action: 연결(3), 페이지(4), 문서(1), 생성(12), 조회(20), 편집(9), 변형(7), 불리언(4), 정렬(2), 스타일(17), 텍스트(5), 레이아웃(4), 컴포넌트(4), 내보내기(3), 프로토타입(2), 레이어(4), 감시(3), 기타(2)");
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
    ("visible", bool_prop "가시성/표시 여부 (visible, set_grid)");
    ("layout_mode", enum_prop ["HORIZONTAL"; "VERTICAL"; "NONE"] "레이아웃 모드");
    ("item_spacing", number_prop "아이템 간격");
    ("padding", number_prop "패딩 (auto-layout, resize_to_fit)");
    ("primary_alignment", enum_prop ["MIN"; "CENTER"; "MAX"; "SPACE_BETWEEN"] "주축 정렬");
    ("counter_alignment", enum_prop ["MIN"; "CENTER"; "MAX"] "교차축 정렬");
    ("center_x", number_prop "뷰포트 중심 X");
    ("center_y", number_prop "뷰포트 중심 Y");
    ("zoom", number_prop "줌 레벨 (set_viewport, scroll_and_zoom)");
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
    ("r", number_prop "빨강 0-1 (set_fill, set_range_fills)");
    ("g", number_prop "초록 0-1 (set_fill, set_range_fills)");
    ("b", number_prop "파랑 0-1 (set_fill, set_range_fills)");
    ("a", number_prop "알파 0-1 (set_fill, set_range_fills)");
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
    ("font_size", number_prop "폰트 크기 (create_text, set_range_font_size)");
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
    ("start", number_prop "텍스트 범위 시작 인덱스 (set_range_fills/set_range_font_size)");
    ("end", number_prop "텍스트 범위 끝 인덱스 (set_range_fills/set_range_font_size)");
    ("index", number_prop "삽입 위치 (insert_child)");
    ("style_type", enum_prop ["FILL"; "PAINT"; "TEXT"; "EFFECT"; "GRID"; "STROKE"] "스타일 타입 (get_styles_by_type/apply_style)");
    ("style_id", string_prop "스타일 ID (apply_style)");
    ("pattern", enum_prop ["GRID"; "COLUMNS"; "ROWS"] "그리드 패턴 (set_grid)");
    ("count", number_prop "컬럼/로우 개수 (set_grid)");
    ("gutter", number_prop "거터 사이즈 (set_grid)");
    ("alignment", enum_prop ["MIN"; "CENTER"; "MAX"; "STRETCH"] "정렬 (set_grid)");
    ("text_case", enum_prop ["ORIGINAL"; "UPPER"; "LOWER"; "TITLE"; "SMALL_CAPS"; "SMALL_CAPS_FORCED"] "텍스트 대소문자 (set_text_case)");
    ("weight", number_prop "선 두께 (set_stroke_weight)");
    ("expand", bool_prop "레이어 펼치기 여부 (collapse_layer)");
  ] ["action"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_get_dev_resources : tool_def = {
  name = "figma_get_dev_resources";
  description = Some "📋 DEV MODE: 노드에 연결된 외부 문서(Jira, Wiki, PR, Storybook 등) 조회. 기획 의도 파악에 필수.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID");
  ] ["file_key"; "node_id"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_add_dev_resource : tool_def = {
  name = "figma_add_dev_resource";
  description = Some "🔗 DEV MODE: 노드에 외부 링크(문서, PR 등) 추가. 디자인과 문서를 시맨틱하게 연결.";
  input_schema = object_schema [
    ("file_key", string_prop "Figma 파일 키");
    ("node_id", string_prop "노드 ID");
    ("name", string_prop "링크 이름 (예: 기획서)");
    ("url", string_prop "연결할 URL (Jira, Wiki 등)");
  ] ["file_key"; "node_id"; "name"; "url"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_setup_webhook : tool_def = {
  name = "figma_setup_webhook";
  description = Some "🔔 SYNC: 실시간 디자인 변경 감지를 위한 Webhook 자동 설정. 디자인 수정 시 자동 알림 수신.";
  input_schema = object_schema [
    ("team_id", string_prop "팀 ID");
    ("file_key", string_prop "대상 파일 키");
    ("endpoint", string_prop "알림을 받을 서버 URL (기본값: 우리 MCP 서버)");
    ("passcode", string_prop "Webhook 보안 패스코드 (기본값: 랜덤)");
  ] ["team_id"; "file_key"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}

let tool_figma_annotate : tool_def = {
  name = "figma_annotate";
  description = Some "✍️ AGENTIC: 피그마 캔버스 특정 위치에 AI 포스트잇 남기기. 디자이너와 협업/질문용.";
  input_schema = object_schema [
    ("channel_id", string_prop "플러그인 채널 ID");
    ("node_id", string_prop "대상 노드 ID (이 노드 옆에 생성)");
    ("message", string_prop "남길 메시지 내용");
    ("color", enum_prop ["yellow"; "blue"; "green"; "red"] "포스트잇 색상 (기본값: yellow)");
  ] ["node_id"; "message"];
  output_schema = None;
  title = None;
  annotations = None;
  icon = None;
  execution = None;
}
