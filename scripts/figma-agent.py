#!/usr/bin/env python3
"""Figma Agent - Polls figma-mcp queue and processes with Ollama

Architecture:
  Figma Plugin → figma-mcp (queue) ← figma-agent (this) → Ollama

Designed for launchd deployment (macOS) or systemd (Linux).
Uses only stdlib - no external dependencies.
"""

import json
import time
import signal
import logging
import urllib.request
import urllib.error

# Configuration
FIGMA_MCP_URL = "http://localhost:8940"
OLLAMA_URL = "http://localhost:11434/api/generate"
OLLAMA_MODEL = "qwen3-coder:30b"
POLL_INTERVAL = 2  # seconds
TIMEOUT = 120  # Ollama can be slow

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s',
    datefmt='%H:%M:%S'
)
log = logging.getLogger("figma-agent")

running = True

def signal_handler(_sig, _frame):
    global running
    log.info("Shutting down...")
    running = False

signal.signal(signal.SIGINT, signal_handler)
signal.signal(signal.SIGTERM, signal_handler)


def http_get(url: str, timeout: int = 5) -> dict | None:
    """Simple HTTP GET using stdlib"""
    try:
        req = urllib.request.Request(url)
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.loads(resp.read().decode())
    except urllib.error.URLError:
        return None
    except Exception as e:
        log.debug(f"GET error: {e}")
        return None


def http_post(url: str, data: dict, timeout: int = 120) -> dict | None:
    """Simple HTTP POST using stdlib"""
    try:
        body = json.dumps(data).encode()
        req = urllib.request.Request(
            url,
            data=body,
            headers={"Content-Type": "application/json"},
            method="POST"
        )
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.loads(resp.read().decode())
    except urllib.error.URLError as e:
        log.debug(f"POST error: {e}")
        return None
    except Exception as e:
        log.debug(f"POST error: {e}")
        return None


def get_system_prompt(platform: str) -> str:
    """Platform-specific system prompts"""
    prompts = {
        "react": "You are a React/TypeScript expert. Generate production-ready React code with proper typing. Output ONLY code, no explanations.",
        "swiftui": "You are a SwiftUI expert. Generate production-ready SwiftUI code. Output ONLY code, no explanations.",
        "flutter": "You are a Flutter/Dart expert. Generate production-ready Flutter code. Output ONLY code, no explanations.",
        "compose": "You are a Jetpack Compose expert. Generate production-ready Kotlin/Compose code. Output ONLY code, no explanations.",
    }
    return prompts.get(platform, "Generate production-ready code. Output ONLY code, no explanations.")


def call_ollama(prompt: str, platform: str) -> str:
    """Call Ollama for code generation"""
    payload = {
        "model": OLLAMA_MODEL,
        "prompt": prompt,
        "system": get_system_prompt(platform),
        "stream": False,
    }
    result = http_post(OLLAMA_URL, payload, timeout=TIMEOUT)
    if result:
        return result.get("response", "// No response from Ollama")
    return "// Error: Ollama request failed"


def poll_and_process():
    """Main polling loop"""
    log.info("🤖 Figma Agent started")
    log.info(f"   MCP: {FIGMA_MCP_URL}")
    log.info(f"   Ollama: {OLLAMA_URL} ({OLLAMA_MODEL})")

    while running:
        try:
            # Poll for pending requests
            data = http_get(f"{FIGMA_MCP_URL}/agent/pending")
            if not data:
                time.sleep(POLL_INTERVAL)
                continue

            pending = data.get("pending", [])
            if not pending:
                time.sleep(POLL_INTERVAL)
                continue

            # Process first pending request
            req = pending[0]
            req_id = req["id"]
            platform = req.get("platform", "react")
            prompt = req.get("prompt", "")

            log.info(f"📥 Processing {req_id} ({platform})")

            # Call Ollama
            code = call_ollama(prompt, platform)

            # Submit result
            result_payload = {
                "request_id": req_id,
                "code": code
            }
            submit_result = http_post(f"{FIGMA_MCP_URL}/agent/result", result_payload, timeout=10)

            if submit_result:
                log.info(f"✅ Completed {req_id}")
            else:
                log.error(f"❌ Failed to submit {req_id}")

        except Exception as e:
            log.error(f"Error: {e}")
            time.sleep(POLL_INTERVAL)

    log.info("Agent stopped")


if __name__ == "__main__":
    poll_and_process()
