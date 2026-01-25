# Install Checklist

## Prereqs
- [ ] OCaml 5.x + opam
- [ ] dune 3.x

## Build
- [ ] `opam pin add grpc-direct https://github.com/jeong-sik/grpc-direct.git -y`
- [ ] `opam install . --deps-only`
- [ ] `dune build`

## Token
- [ ] `export FIGMA_TOKEN="YOUR_TOKEN"` (일회성)
  - 또는 Keychain에 `figma-mcp` 저장

## Run
- [ ] `./start-figma-mcp-http.sh --port 8940`
- [ ] `curl http://127.0.0.1:8940/health`

## MCP Config
- [ ] `~/.mcp.json`에 서버 등록
```json
{
  "mcpServers": {
    "figma": { "type": "http", "url": "http://127.0.0.1:8940/mcp" }
  }
}
```
