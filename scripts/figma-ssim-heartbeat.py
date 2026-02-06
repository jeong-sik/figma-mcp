#!/usr/bin/env python3
"""
figma-ssim-heartbeat.py

Periodic visual regression / similarity checks driven by figma-mcp tools.

Why a separate daemon?
  - Keeps the MCP server stateless and avoids long-lived background fibers.
  - Easy to run under launchd/systemd and easy to disable.

How it works:
  - Reads a JSON config with a list of "jobs".
  - Calls MCP tools via JSON-RPC over HTTP (tools/call).
  - Writes a JSONL log for trend analysis and stores artifacts via figma-mcp tools.

No external deps (stdlib only).
"""

from __future__ import annotations

import argparse
import json
import os
import random
import sys
import time
import urllib.request
import urllib.error
from typing import Any


DEFAULT_MCP_URL = os.getenv("FIGMA_MCP_URL", "http://localhost:8940/mcp")
DEFAULT_MCP_API_KEY = os.getenv("FIGMA_MCP_API_KEY", "") or os.getenv("MCP_API_KEY", "")
DEFAULT_INTERVAL_S = int(os.getenv("FIGMA_SSIM_HEARTBEAT_INTERVAL_S", "300"))
DEFAULT_LOCK = os.getenv("FIGMA_SSIM_HEARTBEAT_LOCK", "/tmp/figma-ssim-heartbeat.lock")
DEFAULT_LOG = os.getenv(
    "FIGMA_SSIM_HEARTBEAT_LOG",
    os.path.join(os.path.expanduser("~"), "me", "logs", "figma-ssim-heartbeat.jsonl"),
)


def now_iso() -> str:
    return time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())


def expand_path(path: str) -> str:
    return os.path.expanduser(path)


def ensure_parent_dir(path: str) -> None:
    parent = os.path.dirname(path)
    if parent and not os.path.exists(parent):
        os.makedirs(parent, exist_ok=True)


def _extract_error_message_from_body(body_text: str) -> str | None:
    try:
        j = json.loads(body_text)
    except json.JSONDecodeError:
        return None
    if not isinstance(j, dict):
        return None
    err = j.get("error")
    if isinstance(err, str):
        return err
    if isinstance(err, dict):
        msg = err.get("message")
        if isinstance(msg, str):
            return msg
    msg = j.get("message")
    return msg if isinstance(msg, str) else None


def post_json(url: str, payload: dict, timeout_s: int = 60, headers_extra: dict | None = None) -> dict:
    data = json.dumps(payload).encode("utf-8")
    headers = {"Content-Type": "application/json"}
    if isinstance(headers_extra, dict):
        headers.update(headers_extra)
    req = urllib.request.Request(
        url,
        data=data,
        headers=headers,
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout_s) as resp:
            body_bytes = resp.read()
    except urllib.error.HTTPError as e:
        try:
            body_text = e.read().decode("utf-8", errors="replace")
        except Exception:
            body_text = ""
        msg = _extract_error_message_from_body(body_text)
        if msg:
            raise RuntimeError(msg)
        truncated = body_text[:2000]
        raise RuntimeError(f"HTTP {e.code} {e.reason}: {truncated}")
    except urllib.error.URLError as e:
        raise RuntimeError(f"failed to reach MCP server: {e.reason}")

    body_text = body_bytes.decode("utf-8", errors="replace")
    try:
        j = json.loads(body_text)
    except json.JSONDecodeError:
        truncated = body_text[:2000]
        raise RuntimeError(f"MCP returned non-JSON response: {truncated}")
    if not isinstance(j, dict):
        raise RuntimeError("MCP returned unexpected JSON type")
    return j


def mcp_call_tool(
    mcp_url: str, name: str, args: dict, timeout_s: int = 120, mcp_api_key: str | None = None
) -> dict:
    payload = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/call",
        "params": {"name": name, "arguments": args},
    }
    headers_extra = {}
    if isinstance(mcp_api_key, str) and mcp_api_key.strip() != "":
        headers_extra["X-MCP-API-Key"] = mcp_api_key.strip()

    response = post_json(mcp_url, payload, timeout_s=timeout_s, headers_extra=headers_extra)
    err = response.get("error")
    if isinstance(err, dict):
        msg = err.get("message")
        if isinstance(msg, str):
            raise RuntimeError(msg)
        raise RuntimeError("Unknown MCP error")
    result = response.get("result")
    return result if isinstance(result, dict) else {}


def extract_text_content(result_obj: dict) -> str | None:
    if not isinstance(result_obj, dict):
        return None
    content = result_obj.get("content")
    if not isinstance(content, list) or not content:
        return None
    first = content[0]
    if not isinstance(first, dict):
        return None
    text = first.get("text")
    return text if isinstance(text, str) else None


def parse_json_maybe(text: str | None) -> dict | list | None:
    if not isinstance(text, str):
        return None
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        return None


def load_config(path: str) -> dict:
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def validate_jobs(config: dict) -> list[dict]:
    jobs = config.get("jobs")
    if not isinstance(jobs, list) or not jobs:
        raise ValueError("config.jobs must be a non-empty list")

    out: list[dict] = []
    for idx, job in enumerate(jobs):
        if not isinstance(job, dict):
            raise ValueError(f"config.jobs[{idx}] must be an object")
        enabled = job.get("enabled", True)
        if enabled is False:
            continue
        if enabled is not True:
            raise ValueError(f"config.jobs[{idx}].enabled must be a boolean")
        name = job.get("name") or f"job_{idx}"
        job_type = job.get("type")
        if job_type not in ("image_similarity", "verify_visual", "fidelity_loop"):
            raise ValueError(
                f"config.jobs[{idx}].type must be one of: image_similarity, verify_visual, fidelity_loop"
            )

        required: list[str]
        if job_type == "image_similarity":
            required = ["file_key", "node_a_id", "node_b_id"]
        else:
            required = ["file_key", "node_id"]
        missing = [k for k in required if k not in job]
        if missing:
            raise ValueError(
                f"config.jobs[{idx}] missing required field(s): {', '.join(missing)}"
            )

        out.append({**job, "name": name})
    return out


def sanitize_for_log(value: Any) -> Any:
    if isinstance(value, dict):
        out: dict[str, Any] = {}
        for k, v in value.items():
            if k in ("html", "plugin_data", "token", "final_html") and isinstance(v, str):
                out[k] = f"<redacted len={len(v)}>"
            else:
                out[k] = sanitize_for_log(v)
        return out
    if isinstance(value, list):
        return [sanitize_for_log(v) for v in value]
    if isinstance(value, str) and len(value) > 200:
        return value[:200] + "...<truncated>"
    return value


def jsonl_append(path: str, record: dict) -> None:
    path = expand_path(path)
    ensure_parent_dir(path)
    line = json.dumps(record, ensure_ascii=False)
    with open(path, "a", encoding="utf-8") as f:
        f.write(line + "\n")


def is_pid_running(pid: int) -> bool:
    try:
        os.kill(pid, 0)
        return True
    except OSError:
        return False


def acquire_lock(lock_path: str, stale_after_s: int) -> None:
    lock_path = expand_path(lock_path)
    now = time.time()
    payload = {"pid": os.getpid(), "started_at": now}

    try:
        fd = os.open(lock_path, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o644)
        try:
            os.write(fd, json.dumps(payload).encode("utf-8"))
        finally:
            os.close(fd)
        return
    except FileExistsError:
        pass

    # Lock exists: check if it's stale.
    try:
        with open(lock_path, "r", encoding="utf-8") as f:
            existing = json.load(f)
        pid = int(existing.get("pid", 0))
        started_at = float(existing.get("started_at", 0))
    except Exception:
        pid = 0
        started_at = 0.0

    age = now - started_at if started_at > 0 else None
    if pid > 0 and is_pid_running(pid):
        raise RuntimeError(f"heartbeat already running (pid={pid})")

    if age is None or age > stale_after_s:
        # Stale lock: best-effort replace.
        try:
            os.unlink(lock_path)
        except Exception:
            raise RuntimeError(f"failed to remove stale lock: {lock_path}")
        acquire_lock(lock_path, stale_after_s=stale_after_s)
        return

    raise RuntimeError(f"heartbeat locked (age={age:.0f}s) - try again later")


def release_lock(lock_path: str) -> None:
    try:
        os.unlink(expand_path(lock_path))
    except Exception:
        pass


def run_job(mcp_url: str, job: dict, mcp_api_key: str | None) -> dict:
    job_type = job["type"]
    name = job["name"]

    started = time.time()
    tool_name = ""
    args: dict = {}
    verdict = "unknown"

    if job_type == "image_similarity":
        tool_name = "figma_image_similarity"
        args = {
            "file_key": job["file_key"],
            "node_a_id": job["node_a_id"],
            "node_b_id": job["node_b_id"],
        }
        # Optional knobs
        for k in (
            "format",
            "token",
            "start_scale",
            "max_scale",
            "scale_step",
            "target_ssim",
            "use_absolute_bounds",
            "version",
            "save_dir",
        ):
            if k in job:
                args[k] = job[k]
    elif job_type == "verify_visual":
        tool_name = "figma_verify_visual"
        args = {
            "file_key": job["file_key"],
            "node_id": job["node_id"],
        }
        for k in (
            "token",
            "html",
            "html_screenshot",
            "target_ssim",
            "max_iterations",
            "width",
            "height",
            "version",
            "mode",
            "checkpoints",
        ):
            if k in job:
                args[k] = job[k]
    else:
        tool_name = "figma_fidelity_loop"
        args = {
            "file_key": job["file_key"],
            "node_id": job["node_id"],
        }
        for k in (
            "token",
            "target_score",
            "start_depth",
            "depth_step",
            "max_depth",
            "max_attempts",
            "geometry",
            "plugin_data",
            "format",
            "include_meta",
            "include_variables",
            "include_image_fills",
            "include_plugin",
            "auto_plugin",
            "include_plugin_variables",
            "plugin_channel_id",
            "plugin_depth",
            "plugin_timeout_ms",
            "summary_only",
            "max_inline_bytes",
        ):
            if k in job:
                args[k] = job[k]

    timeout_s = int(job.get("timeout_s", 180))
    raw_result = mcp_call_tool(mcp_url, tool_name, args, timeout_s=timeout_s, mcp_api_key=mcp_api_key)
    text = extract_text_content(raw_result)
    parsed = parse_json_maybe(text)
    args_for_log = sanitize_for_log(args)
    parsed_for_log = sanitize_for_log(parsed)

    if isinstance(text, str) and len(text) > 4000:
        text = text[:4000] + "...<truncated>"

    # Determine verdict
    if job_type == "verify_visual":
        overall_passed = False
        if isinstance(parsed, dict):
            overall_passed = bool(parsed.get("overall_passed", False))
        verdict = "pass" if overall_passed else "fail"
    elif job_type == "image_similarity":
        best_score = None
        if isinstance(parsed, dict):
            best_score = parsed.get("best_score")
        target = job.get("target_ssim")
        if isinstance(best_score, (int, float)) and isinstance(target, (int, float)):
            verdict = "pass" if float(best_score) >= float(target) else "fail"
        else:
            verdict = "ok"
    else:
        best_score = None
        achieved = None
        if isinstance(parsed, dict):
            best_score = parsed.get("best_score")
            achieved = parsed.get("achieved")
        if achieved is True:
            verdict = "pass"
        elif achieved is False:
            verdict = "fail"
        else:
            verdict = "ok"

    duration_ms = int((time.time() - started) * 1000)
    return {
        "ts": now_iso(),
        "job": name,
        "job_type": job_type,
        "tool": tool_name,
        "args": args_for_log,
        "duration_ms": duration_ms,
        "verdict": verdict,
        "result_text": text,
        "result_parsed": parsed_for_log,
    }


def main() -> int:
    ap = argparse.ArgumentParser(description="figma-mcp SSIM heartbeat loop")
    ap.add_argument("--mcp-url", default=DEFAULT_MCP_URL)
    ap.add_argument("--mcp-api-key", default=DEFAULT_MCP_API_KEY)
    ap.add_argument("--config", required=True, help="JSON config file (jobs list)")
    ap.add_argument("--interval-s", type=int, default=DEFAULT_INTERVAL_S)
    ap.add_argument("--jitter-s", type=int, default=5, help="Random jitter added to each sleep")
    ap.add_argument("--lock", default=DEFAULT_LOCK)
    ap.add_argument("--log", default=DEFAULT_LOG)
    ap.add_argument("--once", action="store_true")
    ap.add_argument("--stale-lock-after-s", type=int, default=3600)
    args = ap.parse_args()

    cfg = load_config(args.config)
    jobs = validate_jobs(cfg)

    acquire_lock(args.lock, stale_after_s=max(60, args.stale_lock_after_s))
    try:
        while True:
            cycle_ok = True
            cycle_started = time.time()
            for job in jobs:
                try:
                    rec = run_job(args.mcp_url, job, mcp_api_key=args.mcp_api_key)
                    jsonl_append(args.log, {**rec, "ok": True})
                    if rec.get("verdict") == "fail":
                        cycle_ok = False
                except Exception as e:
                    cycle_ok = False
                    jsonl_append(
                        args.log,
                        {
                            "ts": now_iso(),
                            "job": job.get("name", "unknown"),
                            "job_type": job.get("type"),
                            "ok": False,
                            "error": str(e),
                        },
                    )

            if args.once:
                return 0 if cycle_ok else 1

            elapsed = time.time() - cycle_started
            sleep_for = max(1, args.interval_s - int(elapsed))
            sleep_for += random.randint(0, max(0, args.jitter_s))
            time.sleep(sleep_for)
    finally:
        release_lock(args.lock)


if __name__ == "__main__":
    raise SystemExit(main())
