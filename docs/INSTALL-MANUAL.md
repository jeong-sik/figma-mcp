# Figma Token & MCP 설정 상세 매뉴얼

이 문서는 "사용자는 매번 빌드하지 않고, 안정적인 바이너리만 받아 쓰는" 흐름을 기준으로 작성되었습니다.

핵심 요약:
- 토큰은 Figma에서 발급
- 토큰은 Keychain에 저장 (권장)
- MCP 설정 JSON은 `~/.mcp.json` 또는 프로젝트 `.mcp.json`
- 실행 커맨드는 "토큰을 안전하게 주입하는 래퍼 스크립트"를 가리키게 구성

---

## 1) Figma Personal Access Token 발급 위치

Figma 웹에서 아래 경로로 이동합니다:

1. 우상단 프로필 메뉴
2. **Settings**
3. **Personal access tokens**
4. 새 토큰 생성 후 복사

이 값을 이후 `FIGMA_TOKEN`으로 사용합니다.

---

## 2) 토큰 저장 (Keychain 권장)

### 2-1) Keychain에 저장

```bash
security add-generic-password \
  -s "figma-mcp" \
  -a "FIGMA_TOKEN" \
  -w "YOUR_TOKEN"
```

### 2-2) 저장 확인

```bash
security find-generic-password -s "figma-mcp" -a "FIGMA_TOKEN" -w
```

### 2-3) 토큰 교체

기존 항목이 있으면 먼저 삭제 후 다시 추가하는 것이 가장 안전합니다.

```bash
security delete-generic-password -s "figma-mcp" -a "FIGMA_TOKEN" || true

security add-generic-password \
  -s "figma-mcp" \
  -a "FIGMA_TOKEN" \
  -w "YOUR_TOKEN"
```

---

## 3) 권장 사용자 흐름: 릴리즈 바이너리만 사용

레포를 클론해서 빌드하지 않아도 됩니다. 릴리즈 바이너리만 받으면 충분합니다.

### 3-1) 바이너리 다운로드 (GitHub CLI `gh` 사용)

아키텍처에 맞는 asset을 선택합니다:
- macOS Apple Silicon: `figma-mcp-macos-arm64`
- Linux x64: `figma-mcp-linux-x64`

예시 (macOS Apple Silicon, `~/bin` 설치):

```bash
TAG="v0.3.5"
ASSET="figma-mcp-macos-arm64"
INSTALL_DIR="$HOME/bin"

mkdir -p "$INSTALL_DIR"

gh release download "$TAG" \
  --repo jeong-sik/figma-mcp \
  --pattern "$ASSET" \
  --dir "$INSTALL_DIR"

mv "$INSTALL_DIR/$ASSET" "$INSTALL_DIR/figma-mcp"
chmod +x "$INSTALL_DIR/figma-mcp"
```

GitHub CLI가 없다면 GitHub의 Releases 탭에서 해당 asset을 직접 내려받아
`~/bin/figma-mcp` 같은 경로에 두고 실행 권한을 주면 됩니다.

---

## 4) 토큰을 안전하게 주입하는 래퍼 스크립트

바이너리를 직접 MCP 커맨드로 연결하면 `FIGMA_TOKEN` 주입이 빠지기 쉽습니다.
아래처럼 얇은 래퍼를 두는 것을 권장합니다.

### 4-1) 래퍼 스크립트 생성

```bash
cat > "$HOME/bin/figma-mcp-start" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

# If FIGMA_TOKEN is not set, read it from Keychain.
if [ -z "${FIGMA_TOKEN:-}" ]; then
  FIGMA_TOKEN="$(security find-generic-password -s figma-mcp -a FIGMA_TOKEN -w 2>/dev/null || true)"
fi

export FIGMA_TOKEN

# TLS 오류(Empty trust anchors)가 나면
if [ -z "${SSL_CERT_FILE:-}" ]; then
  for candidate in \
    "/etc/ssl/cert.pem" \
    "/etc/ssl/certs/ca-certificates.crt" \
    "/etc/pki/tls/certs/ca-bundle.crt" \
    "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem" \
    "/etc/ssl/ca-bundle.pem" \
    "/usr/local/share/certs/ca-root-nss.crt"; do
    if [ -f "$candidate" ]; then
      export SSL_CERT_FILE="$candidate"
      break
    fi
  done
fi

# Allow overriding the binary path if needed.
FIGMA_MCP_BIN="${FIGMA_MCP_BIN:-$HOME/bin/figma-mcp}"
exec "$FIGMA_MCP_BIN" "$@"
EOF

chmod +x "$HOME/bin/figma-mcp-start"
```

---

## 5) MCP 설정 JSON 위치와 예시

설정 파일은 아래 둘 중 하나를 사용합니다:
- 전역: `~/.mcp.json`
- 프로젝트 단위: `<project>/.mcp.json`

### 5-1) 바이너리 + 래퍼 스크립트 방식 (권장)

```json
{
  "mcpServers": {
    "figma": {
      "command": "/Users/you/bin/figma-mcp-start",
      "args": []
    }
  }
}
```

### 5-2) 레포의 start 스크립트 방식 (빌드/클론 사용자용)

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

---

## 6) 간단 스모크 테스트

아래는 서버 초기화 요청이므로 토큰 없이도 응답이 와야 정상입니다.

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' \
  | "$HOME/bin/figma-mcp-start"
```

실제 Figma API를 호출하는 툴은 `FIGMA_TOKEN`이 필요합니다.
