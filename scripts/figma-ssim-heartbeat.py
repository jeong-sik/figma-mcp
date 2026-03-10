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
import signal
import sys
import time
import urllib.request
import urllib.error
from typing import Any


DEFAULT_MCP_URL = os.getenv("FIGMA_MCP_URL", "")
DEFAULT_MCP_API_KEY = os.getenv("FIGMA_MCP_API_KEY", "") or os.getenv("MCP_API_KEY", "")
DEFAULT_INTERVAL_S = int(os.getenv("FIGMA_SSIM_HEARTBEAT_INTERVAL_S", "300"))
DEFAULT_LOCK = os.getenv("FIGMA_SSIM_HEARTBEAT_LOCK", "/tmp/figma-ssim-heartbeat.lock")
DEFAULT_LOG = os.getenv(
    "FIGMA_SSIM_HEARTBEAT_LOG",
    os.path.join(os.path.expanduser("~"), "me", "logs", "figma-ssim-heartbeat.jsonl"),
)
DEFAULT_MAX_LOG_BYTES = int(os.getenv("FIGMA_SSIM_HEARTBEAT_MAX_LOG_BYTES", str(50 * 1024 * 1024)))


_STOP_REQUESTED = False


def _request_stop(_signum: int, _frame: Any) -> None:
    global _STOP_REQUESTED
    _STOP_REQUESTED = True


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


class McpError(RuntimeError):
    pass


class McpHttpError(McpError):
    def __init__(self, code: int, reason: str, message: str):
        super().__init__(message)
        self.code = code
        self.reason = reason


class McpNetworkError(McpError):
    pass


class McpNonJsonError(McpError):
    pass


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
            raise McpHttpError(e.code, str(e.reason), msg)
        truncated = body_text[:2000]
        raise McpHttpError(e.code, str(e.reason), f"HTTP {e.code} {e.reason}: {truncated}")
    except urllib.error.URLError as e:
        raise McpNetworkError(f"failed to reach MCP server: {e.reason}")

    body_text = body_bytes.decode("utf-8", errors="replace")
    try:
        j = json.loads(body_text)
    except json.JSONDecodeError:
        truncated = body_text[:2000]
        raise McpNonJsonError(f"MCP returned non-JSON response: {truncated}")
    if not isinstance(j, dict):
        raise McpNonJsonError("MCP returned unexpected JSON type")
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
            raise McpError(msg)
        raise McpError("Unknown MCP error")
    result = response.get("result")
    return result if isinstance(result, dict) else {}


def _is_transient_http(code: int) -> bool:
    return code >= 500 or code in (408, 429)


def mcp_call_tool_with_retry(
    mcp_url: str,
    name: str,
    args: dict,
    timeout_s: int,
    mcp_api_key: str | None,
    retries: int,
    base_delay_s: float,
    max_delay_s: float,
) -> dict:
    attempts = 1 + max(0, int(retries))
    for attempt in range(attempts):
        try:
            return mcp_call_tool(mcp_url, name, args, timeout_s=timeout_s, mcp_api_key=mcp_api_key)
        except McpNetworkError:
            if attempt >= attempts - 1:
                raise
        except McpHttpError as e:
            if (not _is_transient_http(e.code)) or attempt >= attempts - 1:
                raise
        except McpNonJsonError:
            # Treat as transient once; if persistent, fail.
            if attempt >= attempts - 1:
                raise

        # Backoff + jitter
        delay = min(max_delay_s, base_delay_s * (2**attempt))
        delay += random.random() * 0.25
        time.sleep(delay)

    # Should never reach here.
    raise McpError("retry loop exhausted unexpectedly")


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


def resolve_job_html(job: dict) -> str | None:
    """Resolve HTML payload for semantic verification jobs.

    Priority:
      1) inline `html` field
      2) `html_path` file contents (if file exists locally)
      3) raw `html_path` string (lets the server treat it as a path if available there)
    """
    html = job.get("html")
    if isinstance(html, str) and html.strip() != "":
        return html
    html_path = job.get("html_path")
    if not isinstance(html_path, str) or html_path.strip() == "":
        return None
    html_path = expand_path(html_path.strip())
    try:
        with open(html_path, "r", encoding="utf-8") as f:
            return f.read()
    except OSError:
        return html_path


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
        if job_type not in ("image_similarity", "verify_visual", "verify_semantic", "verify_both", "fidelity_loop"):
            raise ValueError(
                f"config.jobs[{idx}].type must be one of: "
                "image_similarity, verify_visual, verify_semantic, verify_both, fidelity_loop"
            )

        required: list[str]
        if job_type == "image_similarity":
            required = ["file_key", "node_a_id", "node_b_id"]
        elif job_type in ("verify_semantic", "verify_both"):
            required = ["file_key", "node_id"]
        else:
            required = ["file_key", "node_id"]
        missing = [k for k in required if k not in job]
        if missing:
            raise ValueError(
                f"config.jobs[{idx}] missing required field(s): {', '.join(missing)}"
            )

        if job_type in ("verify_semantic", "verify_both"):
            if "html" not in job and "html_path" not in job:
                raise ValueError(
                    f"config.jobs[{idx}] must include one of: html, html_path"
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


def rotate_log(path: str, max_bytes: int, keep: int) -> None:
    if max_bytes <= 0:
        return
    path = expand_path(path)
    try:
        if not os.path.exists(path):
            return
        if os.path.getsize(path) < max_bytes:
            return
    except OSError:
        return

    keep = max(0, int(keep))
    if keep == 0:
        try:
            os.truncate(path, 0)
        except OSError:
            pass
        return

    # Rotate: file -> .1, .1 -> .2, ...
    for i in range(keep, 0, -1):
        src = f"{path}.{i}"
        dst = f"{path}.{i+1}"
        if os.path.exists(src):
            try:
                if i == keep:
                    os.unlink(src)
                else:
                    os.replace(src, dst)
            except OSError:
                pass
    try:
        os.replace(path, f"{path}.1")
    except OSError:
        pass


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

    # If the PID is not running, treat the lock as stale regardless of age.
    # This avoids blocking service restarts after crashes/SIGTERM.
    if pid <= 0 or not is_pid_running(pid):
        try:
            os.unlink(lock_path)
        except Exception:
            # If we can't remove it, fall back to age-based staleness.
            if age is None or age > stale_after_s:
                raise RuntimeError(f"failed to remove stale lock: {lock_path}")
            raise RuntimeError(f"heartbeat locked (age={age:.0f}s) - try again later")
        acquire_lock(lock_path, stale_after_s=stale_after_s)
        return

    raise RuntimeError("unreachable")


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

    timeout_s = int(job.get("timeout_s", 180))
    retries = int(job.get("retries", 2))

    def call_tool(tool: str, tool_args: dict) -> dict:
        return mcp_call_tool_with_retry(
            mcp_url,
            tool,
            tool_args,
            timeout_s=timeout_s,
            mcp_api_key=mcp_api_key,
            retries=retries,
            base_delay_s=0.5,
            max_delay_s=10.0,
        )

    def parse_result(raw: dict) -> tuple[str | None, dict | list | None]:
        text = extract_text_content(raw)
        parsed = parse_json_maybe(text)
        if isinstance(text, str) and len(text) > 4000:
            text = text[:4000] + "...<truncated>"
        return text, parsed

    if job_type == "image_similarity":
        tool_name = "figma_image_similarity"
        args = {
            "file_key": job["file_key"],
            "node_a_id": job["node_a_id"],
            "node_b_id": job["node_b_id"],
        }
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

        raw_result = call_tool(tool_name, args)
        text, parsed = parse_result(raw_result)

        best_score = parsed.get("best_score") if isinstance(parsed, dict) else None
        target = job.get("target_ssim")
        if isinstance(best_score, (int, float)) and isinstance(target, (int, float)):
            verdict = "pass" if float(best_score) >= float(target) else "fail"
        else:
            verdict = "ok"

        args_for_log = sanitize_for_log(args)
        parsed_for_log = sanitize_for_log(parsed)

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

        raw_result = call_tool(tool_name, args)
        text, parsed = parse_result(raw_result)

        overall_passed = bool(parsed.get("overall_passed", False)) if isinstance(parsed, dict) else False
        verdict = "pass" if overall_passed else "fail"

        args_for_log = sanitize_for_log(args)
        parsed_for_log = sanitize_for_log(parsed)

    elif job_type == "verify_semantic":
        tool_name = "figma_verify_semantic"
        html = resolve_job_html(job)
        if not isinstance(html, str) or html.strip() == "":
            raise ValueError("verify_semantic job requires non-empty html/html_path")
        args = {
            "file_key": job["file_key"],
            "node_id": job["node_id"],
            "html": html,
        }
        for k in (
            "token",
            "width",
            "height",
            "score_threshold",
            "text_bbox_tol_px",
            "font_size_tol_px",
            "font_weight_tol",
            "text_color_tol_rgb",
            "version",
        ):
            if k in job:
                args[k] = job[k]

        raw_result = call_tool(tool_name, args)
        text, parsed = parse_result(raw_result)

        sem_passed = False
        if isinstance(parsed, dict):
            sem = parsed.get("semantic_verification")
            if isinstance(sem, dict):
                sem_passed = bool(sem.get("passed", False))
        verdict = "pass" if sem_passed else "fail"

        args_for_log = sanitize_for_log(args)
        parsed_for_log = sanitize_for_log(parsed)

    elif job_type == "verify_both":
        html = resolve_job_html(job)
        if not isinstance(html, str) or html.strip() == "":
            raise ValueError("verify_both job requires non-empty html/html_path")

        require_semantic_pass = bool(job.get("require_semantic_pass", True))
        require_visual_pass = bool(job.get("require_visual_pass", False))
        run_visual_on_semantic_pass = bool(job.get("run_visual_on_semantic_pass", False))
        run_visual_on_semantic_fail = bool(job.get("run_visual_on_semantic_fail", True))

        sem_args = {
            "file_key": job["file_key"],
            "node_id": job["node_id"],
            "html": html,
        }
        for k in (
            "token",
            "width",
            "height",
            "score_threshold",
            "text_bbox_tol_px",
            "font_size_tol_px",
            "font_weight_tol",
            "text_color_tol_rgb",
            "version",
        ):
            if k in job:
                sem_args[k] = job[k]

        sem_raw = call_tool("figma_verify_semantic", sem_args)
        sem_text, sem_parsed = parse_result(sem_raw)

        semantic_passed = False
        if isinstance(sem_parsed, dict):
            sem = sem_parsed.get("semantic_verification")
            if isinstance(sem, dict):
                semantic_passed = bool(sem.get("passed", False))

        run_visual = require_visual_pass
        if semantic_passed and run_visual_on_semantic_pass:
            run_visual = True
        if (not semantic_passed) and run_visual_on_semantic_fail:
            run_visual = True

        vis_args: dict | None = None
        vis_text: str | None = None
        vis_parsed: dict | list | None = None
        visual_passed: bool | None = None

        if run_visual:
            vis_args = {
                "file_key": job["file_key"],
                "node_id": job["node_id"],
            }
            # If user provided HTML, pass it through to keep the comparison deterministic.
            vis_args["html"] = html
            for k in (
                "token",
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
                    vis_args[k] = job[k]

            vis_raw = call_tool("figma_verify_visual", vis_args)
            vis_text, vis_parsed = parse_result(vis_raw)
            visual_passed = bool(vis_parsed.get("overall_passed", False)) if isinstance(vis_parsed, dict) else False

        semantic_ok = semantic_passed if require_semantic_pass else True
        visual_ok = (visual_passed is True) if require_visual_pass else True
        overall_passed = bool(semantic_ok and visual_ok)

        verdict = "pass" if overall_passed else "fail"
        tool_name = "figma_verify_semantic+figma_verify_visual" if run_visual else "figma_verify_semantic"

        args = {
            "semantic": sem_args,
            "visual": (vis_args or {}),
            "policy": {
                "require_semantic_pass": require_semantic_pass,
                "require_visual_pass": require_visual_pass,
                "run_visual_on_semantic_pass": run_visual_on_semantic_pass,
                "run_visual_on_semantic_fail": run_visual_on_semantic_fail,
            },
        }
        parsed = {
            "semantic": sem_parsed,
            "visual": vis_parsed,
            "semantic_passed": semantic_passed,
            "visual_passed": visual_passed,
            "overall_passed": overall_passed,
        }
        text = json.dumps(parsed, ensure_ascii=False)
        if len(text) > 4000:
            text = text[:4000] + "...<truncated>"

        args_for_log = sanitize_for_log(args)
        parsed_for_log = sanitize_for_log(parsed)

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

        raw_result = call_tool(tool_name, args)
        text, parsed = parse_result(raw_result)

        achieved = parsed.get("achieved") if isinstance(parsed, dict) else None
        if achieved is True:
            verdict = "pass"
        elif achieved is False:
            verdict = "fail"
        else:
            verdict = "ok"

        args_for_log = sanitize_for_log(args)
        parsed_for_log = sanitize_for_log(parsed)

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
    ap.add_argument("--max-log-bytes", type=int, default=DEFAULT_MAX_LOG_BYTES, help="Rotate JSONL log when exceeding this size (0 disables)")
    ap.add_argument("--log-rotate-count", type=int, default=3, help="Number of rotated log files to keep")
    ap.add_argument("--once", action="store_true")
    ap.add_argument("--stale-lock-after-s", type=int, default=3600)
    args = ap.parse_args()
    if not args.mcp_url:
        ap.error("FIGMA_MCP_URL or --mcp-url is required")

    signal.signal(signal.SIGTERM, _request_stop)
    signal.signal(signal.SIGINT, _request_stop)

    cfg = load_config(args.config)
    jobs = validate_jobs(cfg)

    acquire_lock(args.lock, stale_after_s=max(60, args.stale_lock_after_s))
    try:
        while True:
            if _STOP_REQUESTED:
                return 0

            rotate_log(args.log, max_bytes=max(0, args.max_log_bytes), keep=max(0, args.log_rotate_count))

            cycle_ok = True
            cycle_started = time.time()
            for job in jobs:
                if _STOP_REQUESTED:
                    cycle_ok = True
                    break
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
