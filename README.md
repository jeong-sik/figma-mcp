# Figma MCP Server

[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/jeong-sik/figma-mcp)
[![OCaml](https://img.shields.io/badge/OCaml-5.x-orange.svg)](https://ocaml.org/)
[![MCP](https://img.shields.io/badge/MCP-2025--11--25-blue.svg)](https://spec.modelcontextprotocol.io/)
[![Status](https://img.shields.io/badge/status-Production%20Ready-green.svg)]()
[![License](https://img.shields.io/badge/license-Private-red.svg)]()

OCaml 5.x 네이티브 MCP 서버 - Figma 디자인을 정확도 우선 Fidelity DSL로 변환

Note: This is a personal project.

## 특징

- **MCP 2025-11-25 스펙 준수** - JSON-RPC 2.0 over stdio
- **정확도 우선** - 레이아웃/페인트/보더/타이포를 최대한 그대로 전달
- **타입 안전** - OCaml Variant/ADT 기반 파싱 (HTML 모드)
- **빠른 실행** - 네이티브 바이너리 (5.5MB)

## Capabilities

```
Capabilities: tools ✅ · resources ✅ · prompts ✅
```

| Capability | 상태 | 설명 |
|------------|------|------|
| **tools** | ✅ 지원 | 51개 도구 (아래 목록 참조) |
| **resources** | ✅ 지원 | `figma://docs/*` 가이드 |
| **prompts** | ✅ 지원 | Fidelity 리뷰 프롬프트 |

### Resources
```
figma://docs/fidelity  # Fidelity DSL 키 설명
figma://docs/usage     # 정확도 우선 호출 패턴
```

### Prompts

```bash
# 리스트 조회
echo '{"jsonrpc":"2.0","id":4,"method":"prompts/list","params":{}}' | ./start-figma-mcp.sh

# 단일 프롬프트 조회 (text 포함)
echo '{"jsonrpc":"2.0","id":5,"method":"prompts/get","params":{"name":"figma_fidelity_review"}}' | ./start-figma-mcp.sh
```

## Recipes

- `docs/RECIPES.md` - end-to-end usage patterns (quickstart, high fidelity, large nodes)

## 도구 목록 (51개)

### Phase 1: Core
| Tool | 설명 | 테스트 |
|------|------|--------|
| `figma_codegen` | Figma JSON → Fidelity DSL 변환 | ✅ |
| `figma_get_file` | Figma API에서 파일 가져와 DSL 변환 | ✅ |
| `figma_get_file_meta` | 파일 메타(components/componentSets/styles) 반환 | ✅ |
| `figma_list_screens` | 파일 내 화면(Frame/Component) 목록 | ✅ |
| `figma_get_node` | 특정 노드 ID만 가져와 DSL 변환 | ✅ |
| `figma_get_node_bundle` | 정확도 극대화 번들(DSL+렌더+메타+변수+fills) | ✅ |
| `figma_get_node_summary` | 경량 구조 요약 | ✅ |
| `figma_select_nodes` | 후보 노드 점수화/선별 + 노트 텍스트 분리 | ✅ |
| `figma_get_node_chunk` | 깊이 범위별 노드 청크 로드 | ✅ |
| `figma_fidelity_loop` | fidelity 점수 미달 시 depth/geometry 자동 상향 | ✅ |
| `figma_image_similarity` | 렌더 이미지 SSIM/PSNR 비교 | ✅ |
| `figma_export_image` | 노드를 PNG/JPG/SVG/PDF URL로 내보내기 | ✅ |
| `figma_get_image_fills` | 파일 내 이미지 채움(image fills) 원본 URL | ✅ |
| `figma_get_nodes` | 여러 노드 ID를 한번에 조회 | ✅ |
| `figma_get_file_versions` | 파일 버전 목록 조회 | ✅ |
| `figma_get_file_comments` | 파일 코멘트 목록 조회 | ✅ |
| `figma_post_comment` | 파일 코멘트 생성 | ✅ |
| `figma_get_file_components` | 파일 컴포넌트 목록 | ✅ |
| `figma_get_team_components` | 팀 컴포넌트 목록 | ✅ |
| `figma_get_file_component_sets` | 파일 컴포넌트 셋 목록 | ✅ |
| `figma_get_team_component_sets` | 팀 컴포넌트 셋 목록 | ✅ |
| `figma_get_file_styles` | 파일 스타일 목록 | ✅ |
| `figma_get_team_styles` | 팀 스타일 목록 | ✅ |
| `figma_get_component` | 컴포넌트 상세 조회 | ✅ |
| `figma_get_component_set` | 컴포넌트 셋 상세 조회 | ✅ |
| `figma_get_style` | 스타일 상세 조회 | ✅ |
| `figma_plugin_connect` | 플러그인 채널 생성/연결 | ✅ |
| `figma_plugin_use_channel` | 기본 채널 설정 | ✅ |
| `figma_plugin_status` | 플러그인 채널 상태 | ✅ |
| `figma_plugin_read_selection` | 플러그인에서 선택 노드 읽기 | ✅ |
| `figma_plugin_get_node` | 플러그인에서 특정 노드 읽기 | ✅ |
| `figma_plugin_export_node_image` | 플러그인 exportAsync 이미지(base64) | ✅ |
| `figma_plugin_get_variables` | 플러그인 Variables API 추출 | ✅ |
| `figma_plugin_apply_ops` | 플러그인으로 create/update/delete | ✅ |

### Phase 1.5: Visual Feedback Loop (95%+ 정확도)
| Tool | 설명 | 테스트 |
|------|------|--------|
| `figma_verify_visual` | HTML 생성 + Playwright 렌더 + SSIM 비교 → 자동 조정 | ✅ |

#### Evolution Tracking (진화 추적)

`figma_verify_visual`은 **진화 과정을 자동 저장**합니다:

```
/tmp/figma-evolution/run_1705123456789/
├── figma_original.png       # Figma 원본 렌더
├── step1_render.png         # 1차 시도 렌더
├── step2_render.png         # 2차 시도 렌더 (조정 후)
├── final_render.png         # 최종 렌더
├── html/
│   ├── step1.html           # 1차 시도 HTML
│   ├── step2.html           # 2차 시도 HTML
│   └── final.html           # 최종 HTML
└── evolution.json           # 메타데이터
```

#### 핵심 인사이트: Flat HTML > Nested HTML

| 접근 방식 | SSIM | 문제점 |
|-----------|------|--------|
| Nested (Figma 계층 복제) | 72% | padding/gap 누적으로 위치 틀어짐 |
| **Flat 2-level** | 99% | 중앙 정렬 + typography 완전 재현 |

```html
<!-- ❌ 실패: 중첩 div -->
<div style="display:flex;flex-direction:column;padding:0px 16px;gap:10px">
  <div style="padding:12px 20px;gap:8px">
    <div style="gap:4px"><span>더보기</span></div>
  </div>
</div>

<!-- ✅ 성공: Flat 2-level -->
<div style="display:flex;align-items:center;justify-content:center;width:343px;height:48px">
  <div style="display:flex;align-items:center;justify-content:center;background:rgb(32,141,249);border-radius:10px">
    <span style="letter-spacing:-0.32px;line-height:24px">더보기</span>
  </div>
</div>
```

### node_id 형식 (중요)
- Figma URL에서는 `node-id=2089-11127`(하이픈)처럼 보이지만, API는 `2089:11127`(콜론) 형식만 받습니다.
- MCP 도구(`figma_get_node`, `figma_get_node_bundle`)의 `node_id`는 콜론 형식이 권장입니다.
- 변환 규칙: `2089-11127` -> `2089:11127`
- MCP 도구/gRPC는 하이픈 형식도 자동 정규화합니다.
- 팁: `figma_parse_url`로 URL에서 `node_id`를 추출하면 바로 사용할 수 있습니다.

예시:
```
figma_get_node_bundle
  file_key: "YOUR_FIGMA_FILE_KEY"
  node_id: "2089:11127"
  format: "html"
  depth: 15
  download: true
```

### 정확도 최우선 호출
- `figma_get_node_bundle` 권장: DSL + 렌더 + 메타/변수/이미지 fills를 한번에 수집
- 플러그인 스냅샷이 필요한 경우 `include_plugin=true` + `plugin_channel_id` 사용 (텍스트 세그먼트/범위 포함)
- fidelity 점수 기반 자동 재조회: `figma_fidelity_loop` + `include_variables=true` + `include_image_fills=true` + `include_plugin=true`
- 렌더 기준 정밀 비교: `figma_image_similarity` (SSIM/PSNR)
- `format=raw` 사용 시 원본 JSON 그대로 반환 (출력 큼)
- `figma_get_variables`는 `format=resolved`로 기본 모드 값 포함

예시:
```
figma_fidelity_loop
  file_key: "YOUR_FIGMA_FILE_KEY"
  node_id: "2089:11127"
  target_score: 0.95
  include_variables: true
  include_image_fills: true
  include_plugin: true
```

렌더 비교는 PNG/JPG를 PPM으로 변환하기 위해 `sips`(macOS) 또는 ImageMagick(`magick`/`convert`)이 필요합니다.

### 정확도 측정 스크립트 (LLM-free)
Figma MCP 결과를 정량화된 리포트로 저장합니다.

```bash
./scripts/figma-accuracy-eval.py \
  --file-key "FILE_KEY" \
  --node-id "123:456" \
  --token "$FIGMA_TOKEN" \
  --include-plugin \
  --plugin-channel-id "ch-..." \
  --out "$HOME/me/logs/figma-accuracy-123_456.json"
```

- 기본 MCP URL: `http://localhost:8940/mcp` (`FIGMA_MCP_URL`로 변경 가능)
- 비교 노드가 있으면 `--compare-node-id`로 SSIM/PSNR도 기록

### Phase 2: Navigation & Search
| Tool | 설명 | 테스트 |
|------|------|--------|
| `figma_parse_url` | URL에서 team/project/file/node ID 추출 (API 호출 없음) | ✅ |
| `figma_get_me` | 현재 인증된 사용자 정보 | ✅ |
| `figma_list_projects` | 팀의 프로젝트 목록 | ✅ |
| `figma_list_files` | 프로젝트의 파일 목록 | ✅ |
| `figma_get_variables` | 디자인 토큰/변수 조회 | ❌ Enterprise 또는 `file_variables:read` 스코프 필요 |

### Phase 3: Analysis & Comparison
| Tool | 설명 | 테스트 |
|------|------|--------|
| `figma_search` | 텍스트/노드 이름 검색 | ✅ |
| `figma_query` | SQL-like 조건부 필터링 (type, size, color 등) | ✅ |
| `figma_tree` | 노드 트리 시각화 (ASCII/indent/compact) | ✅ |
| `figma_stats` | 디자인 통계 (색상, 폰트, 크기 분포) | ✅ |
| `figma_compare` | 두 노드 비교 (Web/Mobile 일관성 검사) | ✅ |
| `figma_export_tokens` | CSS/Tailwind/JSON 디자인 토큰 추출 | ✅ |

### Phase 4: Ops & Cache
| Tool | 설명 | 테스트 |
|------|------|--------|
| `figma_doctor` | 로컬 의존성/스크립트 경로 점검 | ✅ |
| `figma_read_large_result` | large_result 파일 chunk 읽기 | ✅ |
| `figma_cache_stats` | L1/L2 캐시 통계 조회 | ✅ |
| `figma_cache_invalidate` | 캐시 무효화 | ✅ |

### 테스트 현황 (2026-01-14)
- **성공**: 15/16 도구 (Core 기준)
- **제한**: `figma_get_variables` - Figma Variables API는 Enterprise 플랜 또는 `file_variables:read` OAuth 스코프 필요
- **신규 도구**: REST Parity/Plugin Bridge는 샘플 파일 기준 추가 검증 예정

## 설치

```bash
# opam 환경
eval $(opam env)

# 외부 의존성 pin (opam에 없음)
opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git -y

# 의존성 설치
opam install . --deps-only

# 빌드
dune build

# 실행 (로컬 빌드)
dune exec figma-mcp
```

## 토큰 설정 (Keychain)

`start-figma-mcp.sh`와 `start-figma-mcp-http.sh`는 Keychain에서 `FIGMA_TOKEN`을 읽습니다.

```bash
# 1) 환경변수로 실행 (일회성)
export FIGMA_TOKEN="YOUR_TOKEN"

# 2) Keychain 저장 (권장)
security add-generic-password -s "figma-mcp" -a "FIGMA_TOKEN" -w "YOUR_TOKEN"
```

## Claude Code MCP 설정

`~/.mcp.json` 또는 프로젝트 `.mcp.json`에 추가:

```json
{
  "mcpServers": {
    "figma": {
      "command": "/path/to/figma-mcp/start-figma-mcp.sh",
      "args": []
    }
  }
}
```

## Figma Plugin Bridge (고정밀 스냅샷)

REST API만으로 부족한 레이아웃/스타일 정보를 보강하려면 플러그인 브릿지를 함께 사용하세요.

1) HTTP 모드 서버 실행 (예: 8940)
```bash
./start-figma-mcp-http.sh --port 8940
```

2) Figma 플러그인 설치
- Figma → Plugins → Development → Import plugin from manifest…
- `plugin/manifest.json` 선택
- Import 실패 시: Figma → Plugins → Development → New Plugin으로 생성 후,
  생성된 `manifest.json`의 숫자 ID로 `plugin/manifest.json`의 `id` 교체
- `allowedDomains` 에 `http://localhost:...` 넣으면 오류가 날 수 있으니,
  로컬은 `devAllowedDomains`에만 넣고 `allowedDomains`는 https 도메인만 유지
- Figma는 `devAllowedDomains`에서 IP(예: `127.0.0.1`)를 거부할 수 있으니 `localhost`만 사용
- `plugin/manifest.loopback.json`은 placeholder id이므로 import 실패 시
  새 플러그인을 만들고 생성된 `id`로 교체하세요
- Dev Mode 패널에서 실행하려면 `capabilities: ["inspect", "codegen"]` + `codegenLanguages`가 필요

3) 플러그인 실행 후 채널 연결
- 플러그인 UI에서 Server URL 확인/수정 → Connect
- 표시된 Channel ID를 복사

연결 문제 해결:
- `POST /plugin/connect` 또는 `/plugin/poll`이 `net::ERR_CONNECTION_REFUSED`면 서버가 꺼져 있거나 포트가 다릅니다.
  `curl http://localhost:8940/health`로 먼저 확인하세요.
- `devAllowedDomains` 에 `127.0.0.1`를 넣으면 Figma가 거부할 수 있습니다.
  기본 `plugin/manifest.json`은 `localhost`만 허용합니다.
- 로컬 IP가 꼭 필요하면 `plugin/manifest.loopback.json`을 따로 import하세요.
  (Figma가 IP를 거부하면 `localhost`로 되돌리세요.)
- Channel ID가 안 뜨면 플러그인 창을 닫지 말고, 서버 로그/`/plugin/status`를 확인하세요.

4) MCP 도구로 채널 설정
```
figma_plugin_use_channel
  channel_id: "ch-..."
```

5) 번들에 플러그인 스냅샷 포함
```
figma_get_node_bundle
  file_key: "..."
  node_id: "123:456"
  include_plugin: true
  plugin_channel_id: "ch-..."
  plugin_depth: 0
  plugin_include_geometry: false
  include_plugin_variables: true
  include_plugin_image: true
```

URL만으로 호출 (선택 없이 node_id 사용):
```
figma_get_node_bundle
  url: "https://www.figma.com/design/...?...node-id=123-456"
  token: "$FIGMA_TOKEN"
  auto_plugin: true
  plugin_channel_id: "ch-..."
```
주의: 플러그인 스냅샷은 해당 파일이 Figma에서 열려 있어야 합니다.

플러그인 도구 직접 호출:
```
figma_plugin_export_node_image
  node_id: "123:456"

figma_plugin_get_variables
```

플러그인 스냅샷 옵션:
- `plugin_depth`: 큰 섹션은 `0`으로, 필요한 경우 `1~2`로 점진 증가
- `plugin_include_geometry`: 아이콘/벡터가 필요할 때만 `true`
- `figma_plugin_get_node`는 `include_geometry`로 벡터 포함 여부 제어
- `plugin_context_mode: summary` + `plugin_depth: 0`은 빠르지만 정밀도가 낮습니다.
  최종 패스는 `plugin_context_mode: both` + `plugin_depth: 1` 권장
주의: 플러그인 이미지 응답은 base64이므로 출력이 커집니다. (download 옵션은 REST 이미지에만 적용)

HTTP 엔드포인트:
- `POST /plugin/connect`
- `POST /plugin/poll`
- `POST /plugin/result`
- `GET  /plugin/status`
`/plugin/poll`은 `wait_ms`(또는 `timeout_ms`)를 지원합니다. (long-poll, ms 단위)
최대 대기 시간은 `FIGMA_PLUGIN_POLL_MAX_MS`로 제한됩니다. (기본 30000ms)

## gRPC Streaming (대용량 응답) - 권장

### 언제 gRPC를 사용해야 하나?

| 상황 | 권장 프로토콜 | 이유 |
|------|--------------|------|
| 7MB+ JSON 응답 | **gRPC** ✅ | 청크 스트리밍으로 메모리 효율적 |
| 대형 Figma 파일 (100+ 노드) | **gRPC** ✅ | 점진적 로딩으로 타임아웃 방지 |
| 재귀 탐색 (recursive: true) | **gRPC** ✅ | 실시간 진행 상황 표시 |
| 빠른 단일 노드 조회 | HTTP | 오버헤드 낮음 |
| Claude Code stdio 통합 | HTTP | MCP 프로토콜 호환 |

> **권장**: 대용량 응답이 예상되면 **HTTP + gRPC 동시 실행** 모드를 사용하세요.

```bash
# ⭐ 권장: HTTP + gRPC 동시 실행 (production)
./figma-mcp --port 8940 --grpc-port 50052

# gRPC 단독 실행 (streaming-only 환경)
./figma-mcp --grpc-port 50052

# HTTP 단독 실행 (소규모 요청)
./figma-mcp --port 8940
```

서비스/메서드:
- `figma.v1.FigmaService/GetNodeStream` (server streaming)
- `figma.v1.FigmaService/FidelityLoop` (server streaming)
- `figma.v1.FigmaService/GetSplitStream` (server streaming)
- `figma.v1.FigmaService/GetFileMeta` (unary)

테스트 (reflection 비활성화: proto 지정 필요):
```bash
grpcurl -plaintext -import-path proto -proto figma.proto \
  -d '{"file_key":"...","node_id":"...","token":"..."}' \
  localhost:50052 figma.v1.FigmaService/GetNodeStream
```

재귀 스트림(하위 노드 전체 확장):
```bash
grpcurl -plaintext -import-path proto -proto figma.proto \
  -d '{"file_key":"...","node_id":"...","token":"...","recursive":true}' \
  localhost:50052 figma.v1.FigmaService/GetNodeStream
```

옵션:
- `recursive_max_depth` (기본 20, env: `FIGMA_RECURSIVE_MAX_DEPTH`)
- `recursive_max_nodes` (기본 5000, env: `FIGMA_RECURSIVE_MAX_NODES`)
- `recursive_depth_per_call` (기본 1, env: `FIGMA_RECURSIVE_DEPTH_PER_CALL`)
- 재귀 모드는 중복을 피하려고 각 노드를 단일 레벨(자식 제거)로 스트림합니다.

요구사항 분석 + 분할정복 플랜:
```bash
grpcurl -plaintext -import-path proto -proto figma.proto \
  -d '{"file_key":"...","node_id":"...","token":"...","recursive":true}' \
  localhost:50052 figma.v1.FigmaService/PlanTasks
```

PlanTasks 응답 추가 필드:
- `summary`: 우선순위/토큰 요약
- `requirements_json`: 노드 타입/오토레이아웃/이미지 fill 등 분석 결과

프로토콜 정의는 `proto/figma.proto`를 참고하세요.

## Fidelity DSL 포맷 (정확도 우선)

`format: fidelity`는 JSON 기반의 구조화 출력입니다.

```json
{
  "meta": {"id":"1:2","name":"Card","type":"FRAME"},
  "geometry": {"absoluteBoundingBox":{"x":0,"y":0,"width":320,"height":200}},
  "layout": {"layoutMode":"VERTICAL","paddingTop":16,"itemSpacing":12},
  "paint": {"fills":[...],"strokes":[...],"strokeWeight":1},
  "text": {"characters":null,"style":null},
  "children": [ ... ],
  "layout_missing": ["layoutWrap","layoutAlign"]
}
```

### 이미지 다운로드 옵션
`figma_export_image`, `figma_get_node_bundle`에서 `download: true`와 `save_dir` 지정 가능.
기본 저장 경로는 `$ME_ROOT/download/figma-assets` 입니다. (`ME_ROOT` 미설정 시 `$HOME/me/download/figma-assets`, 없으면 `/tmp/figma-assets`)

## 테스트

```bash
# initialize
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | ./start-figma-mcp.sh

# tools/list
echo '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}' | ./start-figma-mcp.sh
```

## 의존성

- OCaml 5.x
- yojson
- cohttp-lwt-unix
- lwt
- uri
- cmdliner

## P0 CSS Fidelity Gap 수정 (2026-01)

Visual Feedback Loop에서 발견된 CSS 정확도 문제를 수정했습니다.

### P0-1, P0-2: Flexbox Alignment

Figma `primaryAxisAlignItems`/`counterAxisAlignItems` → CSS `justify-content`/`align-items` 매핑:

| Figma | justify-content | align-items |
|-------|-----------------|-------------|
| MIN | flex-start (기본값) | flex-start (기본값) |
| CENTER | center | center |
| MAX | flex-end | flex-end |
| SPACE_BETWEEN | space-between | - |
| BASELINE | - | baseline |

**Before**: 모든 값이 무시됨 → CENTER/MAX 레이아웃 틀어짐
**After**: 동적 매핑으로 정확한 정렬

### P0-3: Effects (Shadow, Blur)

4가지 Figma 효과를 CSS로 변환:

```css
/* DropShadow → box-shadow */
box-shadow: 4px 4px 10px 2px rgba(0,0,0,0.25);

/* InnerShadow → box-shadow inset */
box-shadow: inset 2px 2px 5px 0px rgba(255,255,255,0.5);

/* LayerBlur → filter:blur */
filter: blur(8px);

/* BackgroundBlur → backdrop-filter */
backdrop-filter: blur(12px);
```

**예제 출력**:
```css
box-shadow:4px 4px 10px 2px rgba(0,0,0,0.2),inset 2px 2px 5px 0px rgba(255,255,255,0.50);filter:blur(8px);backdrop-filter:blur(12px)
```

### P0-4: Gradient

Figma `gradientStops` → CSS `linear-gradient`:

```ocaml
(* 입력: Figma gradientStops *)
[
  (0.0, {r=1.0; g=0.0; b=0.0; a=1.0});   (* Red *)
  (0.5, {r=0.0; g=1.0; b=0.0; a=1.0});   (* Green *)
  (1.0, {r=0.0; g=0.0; b=1.0; a=1.0});   (* Blue *)
]

(* 출력: CSS *)
"linear-gradient(to right,#FF0000 0%,#00FF00 50%,#0000FF 100%)"
```

**현재 제한사항**:
- 방향은 `to right` 고정 (각도 계산은 P1)
- Radial/Angular/Diamond는 linear로 fallback

### 성능 벤치마크

```
gradient_to_css (5 stops)     : 4 µs/iter
effects_to_css (4 effects)    : 6 µs/iter
effects_to_css (all invisible): <1 µs/iter
```

### 테스트

```bash
# P0 유닛 테스트 (10개)
dune exec ./test/test_codegen_p0.exe

# P0 벤치마크
dune exec ./test/bench_p0.exe
```

---

## Future Work: 다중 유사도 측정 시스템

현재 `figma_compare`는 실용적 휴리스틱 기반입니다. 아래 학술적 기반 개선을 계획 중:

### 다중 유사도 지표 (Multi-Metric Similarity)

| 지표 | 공식/알고리즘 | 출처 |
|------|--------------|------|
| **Color** | CIEDE2000 (ΔE*₀₀) | CIE 표준, 인간 색지각 모델 |
| **Layout** | IoU (Intersection over Union) | 객체 탐지 표준 메트릭 |
| **Structure** | Tree Edit Distance (TED) | Zhang-Shasha 알고리즘 |
| **Visual** | SSIM (Structural Similarity Index) | Wang et al. 2004, IEEE TIP |
| **Embedding** | Cosine Similarity on UI Embedding | Rico (Google, UIST 2017) |

### 출력 예시 (계획)

```
비교: "B2C 홈 (Web)" vs "B2C 홈 (Mobile)"

┌─────────────────┬────────┬─────────────────────────────┐
│ 지표            │ 점수   │ 설명                        │
├─────────────────┼────────┼─────────────────────────────┤
│ Color (ΔE*₀₀)  │ 95.2%  │ 색상 차이 ΔE=2.3 (JND 이하) │
│ Layout (IoU)    │ 87.4%  │ 요소 위치 오버랩            │
│ Structure (TED) │ 92.0%  │ 트리 편집 거리 4            │
│ Visual (SSIM)   │ 89.1%  │ 구조적 유사도               │
│ Embedding       │ 94.7%  │ Rico-style 64dim cosine     │
├─────────────────┼────────┼─────────────────────────────┤
│ **종합**        │ 91.7%  │ 가중 평균                   │
└─────────────────┴────────┴─────────────────────────────┘
```

### 참고 논문

- [Rico: A Mobile App Dataset](https://dl.acm.org/doi/10.1145/3126594.3126651) (UIST 2017)
- [LTSim: Layout Transportation-based Similarity](https://arxiv.org/html/2407.12356v1) (2024)
- [SSIM: Image Quality Assessment](https://ieeexplore.ieee.org/document/1284395) (IEEE TIP 2004)
- [CIEDE2000 Color Difference](https://en.wikipedia.org/wiki/Color_difference#CIEDE2000)

### 구현 우선순위

1. ✅ 현재: 휴리스틱 가중치 (Critical/Major/Minor)
2. 🔜 Phase 1: CIEDE2000 색상 거리 적용
3. 🔜 Phase 2: IoU 레이아웃 유사도 추가
4. 🔜 Phase 3: SSIM 시각적 유사도 (렌더링 필요)
5. 🔜 Phase 4: Rico-style Embedding (ML 모델 필요)

## 라이선스

MIT
