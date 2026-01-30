# Setup

간단 설치/실행/연동 방법만 정리합니다.

## 요구사항

- OCaml 5.x + opam
- dune 3.x

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

## 토큰 설정

`start-figma-mcp.sh`와 `start-figma-mcp-http.sh`는 Keychain에서 `FIGMA_TOKEN`을 읽습니다.

```bash
# 1) 환경변수로 실행 (일회성)
export FIGMA_TOKEN="YOUR_TOKEN"

# TLS 오류(Empty trust anchors)가 나면
# macOS: /etc/ssl/cert.pem
# Linux: /etc/ssl/certs/ca-certificates.crt (배포판별 상이)
export SSL_CERT_FILE="/etc/ssl/cert.pem"

# 2) Keychain 저장 (권장)
security add-generic-password -s "figma-mcp" -a "FIGMA_TOKEN" -w "YOUR_TOKEN"
```

## MCP 설정

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

## Plugin Bridge (선택)

정확도 보강이 필요하면 플러그인 브릿지를 사용합니다.

```bash
# HTTP 모드 서버 실행 (예: 8940)
./start-figma-mcp-http.sh --port 8940
```

플러그인 설치/연결 절차는 README의 **Figma Plugin Bridge** 섹션을 따릅니다.

## 확인

```bash
curl http://localhost:8940/health
```
