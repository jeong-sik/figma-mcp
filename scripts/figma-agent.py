#!/usr/bin/env python3
"""Figma Agent - Claims figma-mcp agent queue and generates code via LLM.

Architecture:
  Figma Plugin → figma-mcp (agent queue) ← figma-agent (this) → LLM backend

Backends:
  1) llm-mcp (recommended): Call MCP tools (ollama/gemini/claude-cli/codex).
  2) ollama-direct (legacy): Call local Ollama HTTP API directly.

Designed for launchd deployment (macOS) or systemd (Linux).
Uses only stdlib - no external dependencies.
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import re
import signal
import socket
import threading
import time
import urllib.error
import urllib.request
from dataclasses import dataclass

log = logging.getLogger("figma-agent")


def _env(name: str, default: str | None = None) -> str | None:
    v = os.environ.get(name)
    if v is None:
        return default
    v = v.strip()
    return v if v else default


def _env_int(name: str, default: int) -> int:
    v = _env(name)
    if v is None:
        return default
    try:
        return int(v)
    except ValueError:
        return default


def _env_float(name: str, default: float) -> float:
    v = _env(name)
    if v is None:
        return default
    try:
        return float(v)
    except ValueError:
        return default


def _truthy(v: str | None) -> bool:
    if v is None:
        return False
    return v.strip().lower() in ("1", "true", "yes", "y", "on")


@dataclass(frozen=True)
class Config:
    figma_mcp_url: str
    figma_mcp_api_key: str | None
    poll_interval_sec: float
    heartbeat_interval_sec: float
    request_timeout_sec: int
    worker_id: str
    once: bool

    llm_backend: str  # llm-mcp | ollama-direct

    # llm-mcp
    llm_mcp_url: str
    llm_mcp_tool: str  # ollama | gemini | claude-cli | codex
    llm_mcp_model: str
    llm_mcp_timeout_sec: int
    llm_mcp_temperature: float
    gemini_thinking_level: str  # low|high

    # ollama-direct
    ollama_url: str
    ollama_model: str

    @staticmethod
    def from_env(once: bool = False) -> "Config":
        host = socket.gethostname()
        worker_id = _env("FIGMA_AGENT_WORKER_ID", f"figma-agent:{host}:{os.getpid()}")

        return Config(
            figma_mcp_url=_env("FIGMA_MCP_URL", "http://127.0.0.1:8940") or "http://127.0.0.1:8940",
            figma_mcp_api_key=_env("FIGMA_MCP_API_KEY") or _env("MCP_API_KEY"),
            poll_interval_sec=_env_float("FIGMA_AGENT_POLL_INTERVAL_SEC", 2.0),
            heartbeat_interval_sec=_env_float("FIGMA_AGENT_HEARTBEAT_INTERVAL_SEC", 15.0),
            request_timeout_sec=_env_int("FIGMA_AGENT_REQUEST_TIMEOUT_SEC", 300),
            worker_id=worker_id or f"figma-agent:{host}:{os.getpid()}",
            once=once,
            llm_backend=_env("FIGMA_AGENT_LLM_BACKEND", "llm-mcp") or "llm-mcp",
            llm_mcp_url=_env("LLM_MCP_URL", "http://127.0.0.1:8932/mcp") or "http://127.0.0.1:8932/mcp",
            llm_mcp_tool=_env("LLM_MCP_TOOL", "ollama") or "ollama",
            llm_mcp_model=_env("LLM_MCP_MODEL", "devstral") or "devstral",
            llm_mcp_timeout_sec=_env_int("LLM_MCP_TIMEOUT_SEC", 300),
            llm_mcp_temperature=_env_float("LLM_MCP_TEMPERATURE", 0.2),
            gemini_thinking_level=_env("GEMINI_THINKING_LEVEL", "high") or "high",
            ollama_url=_env("OLLAMA_URL", "http://127.0.0.1:11434/api/generate") or "http://127.0.0.1:11434/api/generate",
            ollama_model=_env("OLLAMA_MODEL", "qwen3-coder:30b") or "qwen3-coder:30b",
        )


_CODE_FENCE_RE = re.compile(r"```[a-zA-Z0-9_-]*\n(.*?)\n```", re.DOTALL)


def strip_llm_mcp_extra(text: str) -> str:
    marker = "\n\n[Extra]\n"
    if marker in text:
        return text.split(marker, 1)[0]
    return text


def strip_code_fences(text: str) -> str:
    m = _CODE_FENCE_RE.search(text)
    if not m:
        return text.strip()
    return m.group(1).strip()


def get_system_prompt(platform: str) -> str:
    prompts = {
        "react": (
            "You are a React/TypeScript expert. Generate production-ready React code with proper typing. "
            "Output ONLY code. No explanations. No markdown fences."
        ),
        "swiftui": (
            "You are a SwiftUI expert. Generate production-ready SwiftUI code. "
            "Output ONLY code. No explanations. No markdown fences."
        ),
        "flutter": (
            "You are a Flutter/Dart expert. Generate production-ready Flutter code. "
            "Output ONLY code. No explanations. No markdown fences."
        ),
        "compose": (
            "You are a Jetpack Compose expert. Generate production-ready Kotlin/Compose code. "
            "Output ONLY code. No explanations. No markdown fences."
        ),
    }
    return prompts.get(
        platform,
        "Generate production-ready code. Output ONLY code. No explanations. No markdown fences.",
    )


def _headers_json(api_key: str | None = None) -> dict[str, str]:
    headers = {"Content-Type": "application/json"}
    if api_key:
        headers["x-mcp-api-key"] = api_key
    return headers


def http_get_json(url: str, *, headers: dict[str, str] | None = None, timeout: int = 5) -> dict | None:
    try:
        req = urllib.request.Request(url, headers=headers or {}, method="GET")
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.loads(resp.read().decode())
    except urllib.error.URLError:
        return None
    except Exception as e:
        log.debug("GET error: %s", e)
        return None


def http_post_json(url: str, data: dict, *, headers: dict[str, str] | None = None, timeout: int = 120) -> dict | None:
    try:
        body = json.dumps(data).encode()
        req = urllib.request.Request(
            url,
            data=body,
            headers=headers or {},
            method="POST",
        )
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.loads(resp.read().decode())
    except urllib.error.URLError as e:
        log.debug("POST error: %s", e)
        return None
    except Exception as e:
        log.debug("POST error: %s", e)
        return None


def parse_llm_mcp_tool_text(resp_json: dict) -> str:
    if "error" in resp_json:
        raise RuntimeError(f"llm-mcp jsonrpc error: {resp_json['error']}")
    result = resp_json.get("result")
    if not isinstance(result, dict):
        raise RuntimeError("llm-mcp: missing result")
    if result.get("isError") is True:
        content = result.get("content") or []
        text = ""
        if isinstance(content, list) and content:
            item = content[0]
            if isinstance(item, dict):
                text = item.get("text") or ""
        raise RuntimeError(f"llm-mcp tool error: {text[:200]}")
    content = result.get("content")
    if not isinstance(content, list) or not content:
        raise RuntimeError("llm-mcp: missing content")
    item0 = content[0]
    if not isinstance(item0, dict) or item0.get("type") != "text":
        raise RuntimeError("llm-mcp: unexpected content shape")
    text = item0.get("text")
    if not isinstance(text, str):
        raise RuntimeError("llm-mcp: missing text")
    return text


def call_llm_mcp_tool(cfg: Config, *, tool_name: str, arguments: dict) -> str:
    payload = {
        "jsonrpc": "2.0",
        "id": int(time.time() * 1000),
        "method": "tools/call",
        "params": {"name": tool_name, "arguments": arguments},
    }
    resp = http_post_json(cfg.llm_mcp_url, payload, headers=_headers_json(), timeout=cfg.llm_mcp_timeout_sec)
    if resp is None:
        raise RuntimeError("llm-mcp request failed")
    return parse_llm_mcp_tool_text(resp)


def call_ollama_direct(cfg: Config, *, prompt: str, system: str) -> str:
    payload = {
        "model": cfg.ollama_model,
        "prompt": prompt,
        "system": system,
        "stream": False,
    }
    resp = http_post_json(cfg.ollama_url, payload, headers=_headers_json(), timeout=cfg.request_timeout_sec)
    if resp is None:
        raise RuntimeError("ollama request failed")
    text = resp.get("response")
    if not isinstance(text, str) or not text.strip():
        return "// No response from Ollama"
    return text


def generate_code(cfg: Config, *, prompt: str, platform: str) -> str:
    system_prompt = get_system_prompt(platform)

    if cfg.llm_backend == "ollama-direct":
        text = call_ollama_direct(cfg, prompt=prompt, system=system_prompt)
        return strip_code_fences(text)

    # llm-mcp backend (default)
    tool = cfg.llm_mcp_tool
    args: dict = {"timeout": cfg.llm_mcp_timeout_sec, "stream": False}
    if tool == "ollama":
        args.update(
            {
                "prompt": prompt,
                "model": cfg.llm_mcp_model,
                "system_prompt": system_prompt,
                "temperature": cfg.llm_mcp_temperature,
            }
        )
    elif tool == "gemini":
        # Gemini tool doesn't expose system_prompt; embed instructions into prompt.
        args.update(
            {
                "prompt": f"{system_prompt}\n\n{prompt}",
                "model": cfg.llm_mcp_model,
                "thinking_level": cfg.gemini_thinking_level,
            }
        )
    elif tool == "claude-cli":
        args.update(
            {
                "prompt": prompt,
                "model": cfg.llm_mcp_model,
                "system_prompt": system_prompt,
                "output_format": "text",
            }
        )
    elif tool == "codex":
        args.update(
            {
                "prompt": prompt,
                "model": cfg.llm_mcp_model,
                "system_prompt": system_prompt,
            }
        )
    else:
        raise RuntimeError(f"Unsupported LLM_MCP_TOOL: {tool}")

    text = call_llm_mcp_tool(cfg, tool_name=tool, arguments=args)
    text = strip_llm_mcp_extra(text)
    return strip_code_fences(text)


def claim_request(cfg: Config) -> dict | None:
    headers = _headers_json(cfg.figma_mcp_api_key)
    return http_post_json(
        f"{cfg.figma_mcp_url}/agent/claim",
        {"worker_id": cfg.worker_id},
        headers=headers,
        timeout=10,
    )


def send_heartbeat(cfg: Config, *, request_id: str) -> bool:
    headers = _headers_json(cfg.figma_mcp_api_key)
    resp = http_post_json(
        f"{cfg.figma_mcp_url}/agent/heartbeat",
        {"worker_id": cfg.worker_id, "request_id": request_id},
        headers=headers,
        timeout=10,
    )
    return bool(resp and resp.get("status") == "ok")


def abandon_request(cfg: Config, *, request_id: str, reason: str) -> None:
    headers = _headers_json(cfg.figma_mcp_api_key)
    http_post_json(
        f"{cfg.figma_mcp_url}/agent/abandon",
        {"worker_id": cfg.worker_id, "request_id": request_id, "reason": reason},
        headers=headers,
        timeout=10,
    )


def submit_result(cfg: Config, *, request_id: str, code: str, context_digest: str) -> bool:
    headers = _headers_json(cfg.figma_mcp_api_key)
    payload = {
        "request_id": request_id,
        "worker_id": cfg.worker_id,
        "context_digest": context_digest,
        "code": code,
    }
    resp = http_post_json(f"{cfg.figma_mcp_url}/agent/result", payload, headers=headers, timeout=20)
    return bool(resp and resp.get("status") == "submitted")


def heartbeat_loop(cfg: Config, *, request_id: str, stop_event: threading.Event) -> None:
    while not stop_event.wait(cfg.heartbeat_interval_sec):
        ok = send_heartbeat(cfg, request_id=request_id)
        if not ok:
            log.warning("heartbeat failed (request_id=%s)", request_id)


_running = True


def _signal_handler(_sig, _frame):
    global _running
    _running = False


signal.signal(signal.SIGINT, _signal_handler)
signal.signal(signal.SIGTERM, _signal_handler)


def run(cfg: Config) -> int:
    log.info("Figma Agent started")
    log.info("  figma-mcp=%s worker_id=%s", cfg.figma_mcp_url, cfg.worker_id)
    log.info("  backend=%s llm_mcp=%s tool=%s model=%s", cfg.llm_backend, cfg.llm_mcp_url, cfg.llm_mcp_tool, cfg.llm_mcp_model)

    while _running:
        claim = claim_request(cfg)
        if not claim:
            time.sleep(cfg.poll_interval_sec)
            continue

        status = claim.get("status")
        if status != "claimed":
            if cfg.once:
                return 0
            time.sleep(cfg.poll_interval_sec)
            continue

        req = claim.get("request") or {}
        request_id = req.get("id") or req.get("request_id")
        platform = req.get("platform", "react")
        prompt = req.get("prompt", "")
        context_digest = req.get("context_digest", "")

        if not request_id:
            log.error("claim response missing request id")
            time.sleep(cfg.poll_interval_sec)
            continue

        log.info("Processing request_id=%s platform=%s", request_id, platform)

        stop = threading.Event()
        hb = threading.Thread(target=heartbeat_loop, args=(cfg,), kwargs={"request_id": request_id, "stop_event": stop}, daemon=True)
        hb.start()

        try:
            t0 = time.time()
            code = generate_code(cfg, prompt=prompt, platform=platform)
            dt = time.time() - t0
            log.info("LLM done request_id=%s elapsed=%.2fs bytes=%d", request_id, dt, len(code))

            ok = submit_result(cfg, request_id=request_id, code=code, context_digest=context_digest)
            if ok:
                log.info("Completed request_id=%s", request_id)
            else:
                log.error("Failed to submit request_id=%s", request_id)
                abandon_request(cfg, request_id=request_id, reason="submit_failed")

        except Exception as e:
            log.error("Error request_id=%s: %s", request_id, e)
            abandon_request(cfg, request_id=request_id, reason=str(e)[:200])
        finally:
            stop.set()
            hb.join(timeout=2)

        if cfg.once:
            return 0

    log.info("Agent stopped")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="figma-agent (figma-mcp agent queue worker)")
    parser.add_argument("--once", action="store_true", help="Process at most one claimed request and exit.")
    parser.add_argument("--log-level", default=_env("FIGMA_AGENT_LOG_LEVEL", "INFO"), help="Python logging level")
    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, str(args.log_level).upper(), logging.INFO),
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )

    cfg = Config.from_env(once=args.once)
    return run(cfg)


if __name__ == "__main__":
    raise SystemExit(main())
