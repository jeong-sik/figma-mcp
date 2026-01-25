# Install Checklist

설치 후 동작 확인용 체크리스트입니다.

## Pre-flight

- [ ] opam + dune 설치
- [ ] FIGMA_TOKEN 준비 (API 호출용)
- [ ] 의존성 설치 (`opam install . --deps-only`)
- [ ] 빌드 완료 (`dune build`)

## Run

- [ ] 서버 실행 (`./start-figma-mcp.sh --port 8940`)

## Post-install checks

```bash
curl http://127.0.0.1:8940/health

curl -sS http://127.0.0.1:8940/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```
