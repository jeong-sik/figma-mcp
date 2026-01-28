# Figma MCP Server

[![Version](https://img.shields.io/badge/version-0.3.10-blue.svg)](https://github.com/jeong-sik/figma-mcp)
[![OCaml](https://img.shields.io/badge/OCaml-5.x-orange.svg)](https://ocaml.org/)
[![MCP](https://img.shields.io/badge/MCP-2025--11--25-blue.svg)](https://spec.modelcontextprotocol.io/)
[![Status](https://img.shields.io/badge/status-Personal%20Project-lightgrey.svg)]()
[![License](https://img.shields.io/badge/license-MIT-green.svg)]()

OCaml 5.x 기반 MCP 서버로 Figma 디자인을 Fidelity DSL/HTML 등으로 변환합니다.

Note: This is a personal project.

## Quickstart

```bash
eval $(opam env)
opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git -y
opam install . --deps-only
dune build
export FIGMA_TOKEN="YOUR_TOKEN"
./start-figma-mcp-http.sh --port 8940
```

```json
{
  "mcpServers": {
    "figma": {
      "type": "http",
      "url": "http://127.0.0.1:8940/mcp"
    }
  }
}
```

## 특징

- **MCP 2025-11-25 스펙 기준 구현** - JSON-RPC 2.0 over stdio
- **충실도 중심 옵션** - 레이아웃/페인트/보더/타이포 정보를 최대한 보존
- **타입 안전** - OCaml Variant/ADT 기반 파싱 (HTML 모드)
- **네이티브 실행** - 바이너리 배포 가능

## Capabilities

```
Capabilities: tools ✅ · resources ✅ · prompts ✅
```

| Capability | 상태 | 설명 |
|------------|------|------|
| **tools** | ✅ 지원 | 55개 도구 (`tools/list` 참고) |
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
- `docs/SETUP.md` - 설치/실행/연동 요약
- `docs/MCP-TEMPLATE.md` - ~/.mcp.json 템플릿
- `docs/INSTALL-CHECKLIST.md` - 설치 후 확인

## 도구 개요 (2026-01-27 기준)

- 코드상 `tools/list`에 등록된 도구는 55개입니다. (실제 값은 `tools/list` 결과를 기준으로 보세요.)
- 전체 목록 확인:

```bash
echo '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}' | ./start-figma-mcp.sh
```

자주 쓰는 도구:
- `figma_get_node_bundle`: DSL + 렌더 + 메타/변수/이미지 fills 묶음 조회
- `figma_fidelity_loop`: fidelity 점수 기반 재조회 루프
- `figma_verify_visual`: HTML 렌더/비교/조정 루프
- `figma_export_image`, `figma_export_smart`: 이미지 export
- `figma_parse_url`: URL에서 file/node id 파싱
- `figma_get_variables`, `figma_export_tokens`: 변수/토큰 조회
- `figma_plugin_*`: 플러그인 브릿지 연결/스냅샷
- `figma_cache_*`: 캐시 통계/무효화

## node_id 형식

- Figma URL의 `node-id=123-456`은 API에서는 `123:456` 형식을 사용합니다.
- `figma_parse_url`을 사용하면 변환 없이 `node_id`를 바로 얻을 수 있습니다.

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

상세 가이드(토큰 발급 경로, 릴리즈 바이너리 설치, 설정 JSON 예시):
- `docs/INSTALL-MANUAL.md`

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

## gRPC Streaming (대용량 응답)

### 언제 gRPC를 사용해야 하나?

| 상황 | 선호 프로토콜 | 이유 |
|------|--------------|------|
| 큰 JSON 응답 | **gRPC** ✅ | 청크 스트리밍으로 메모리 효율적 |
| 대형 Figma 파일 | **gRPC** ✅ | 점진적 로딩으로 타임아웃 방지 |
| 재귀 탐색 (recursive: true) | **gRPC** ✅ | 실시간 진행 상황 표시 |
| 빠른 단일 노드 조회 | HTTP | 오버헤드 낮음 |
| Claude Code stdio 통합 | HTTP | MCP 프로토콜 호환 |

> 대용량 응답이 예상되면 HTTP + gRPC 동시 실행 모드를 고려하세요.

```bash
# 예시: HTTP + gRPC 동시 실행
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

## 변경 이력과 노트

- 변경 이력: `CHANGELOG.md`
- 설치/연동/레시피: `docs/`

## 라이선스

MIT
