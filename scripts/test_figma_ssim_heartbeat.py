import importlib.util
import json
import os
import pathlib
import tempfile
import threading
import time
import unittest
from http.server import BaseHTTPRequestHandler, HTTPServer


def load_module():
    here = pathlib.Path(__file__).resolve()
    script = here.parent / "figma-ssim-heartbeat.py"
    spec = importlib.util.spec_from_file_location("figma_ssim_heartbeat", script)
    assert spec is not None
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(mod)
    return mod


M = load_module()


class TestConfigValidation(unittest.TestCase):
    def test_validate_jobs_requires_non_empty_jobs(self):
        with self.assertRaises(ValueError):
            M.validate_jobs({})
        with self.assertRaises(ValueError):
            M.validate_jobs({"jobs": []})

    def test_validate_jobs_skips_disabled(self):
        jobs = M.validate_jobs(
            {
                "jobs": [
                    {"enabled": False, "type": "verify_visual", "file_key": "k", "node_id": "1:2"},
                    {"type": "verify_visual", "file_key": "k", "node_id": "1:3"},
                ]
            }
        )
        self.assertEqual(len(jobs), 1)
        self.assertEqual(jobs[0]["node_id"], "1:3")

    def test_validate_jobs_enabled_must_be_boolean(self):
        with self.assertRaises(ValueError):
            M.validate_jobs(
                {
                    "jobs": [
                        {"enabled": "no", "type": "verify_visual", "file_key": "k", "node_id": "1:2"},
                    ]
                }
            )

    def test_validate_jobs_missing_required_fields(self):
        with self.assertRaises(ValueError):
            M.validate_jobs({"jobs": [{"type": "image_similarity", "file_key": "k"}]})
        with self.assertRaises(ValueError):
            M.validate_jobs({"jobs": [{"type": "verify_visual", "file_key": "k"}]})


class TestSanitization(unittest.TestCase):
    def test_sanitize_for_log_redacts_sensitive_fields(self):
        inp = {
            "html": "<div>" + ("x" * 10) + "</div>",
            "token": "secret-token",
            "final_html": "<html>" + ("y" * 10) + "</html>",
            "plugin_data": "plugin-data",
            "nested": {"ok": "hello", "long": "z" * 500},
        }
        out = M.sanitize_for_log(inp)
        self.assertTrue(str(out["html"]).startswith("<redacted"))
        self.assertTrue(str(out["token"]).startswith("<redacted"))
        self.assertTrue(str(out["final_html"]).startswith("<redacted"))
        self.assertTrue(str(out["plugin_data"]).startswith("<redacted"))
        self.assertIn("...<truncated>", out["nested"]["long"])


class TestErrorParsing(unittest.TestCase):
    def test_extract_error_message_from_body(self):
        self.assertEqual(M._extract_error_message_from_body('{"error":"nope"}'), "nope")
        self.assertEqual(
            M._extract_error_message_from_body('{"error":{"message":"bad"}}'),
            "bad",
        )
        self.assertEqual(M._extract_error_message_from_body('{"message":"m"}'), "m")
        self.assertIsNone(M._extract_error_message_from_body("not json"))


class _TestHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers.get("Content-Length", "0"))
        _ = self.rfile.read(length)
        api_key = self.headers.get("X-MCP-API-Key", "")
        if api_key != "secret":
            body = json.dumps({"error": "API key required"}).encode("utf-8")
            self.send_response(401)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            return

        body = json.dumps({"jsonrpc": "2.0", "id": 1, "result": {"content": [{"text": "{}"}]}}).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, _format, *_args):  # noqa: N802 - stdlib hook name
        # Silence noisy test logs
        return


class TestHttpClient(unittest.TestCase):
    def setUp(self):
        self.server = HTTPServer(("127.0.0.1", 0), _TestHandler)
        self.port = self.server.server_address[1]
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join(timeout=2)

    def test_mcp_call_tool_requires_api_key(self):
        url = f"http://127.0.0.1:{self.port}/mcp"
        with self.assertRaises(RuntimeError) as ctx:
            M.mcp_call_tool(url, "tool", {}, timeout_s=2, mcp_api_key="")
        self.assertIn("API key required", str(ctx.exception))

        ok = M.mcp_call_tool(url, "tool", {}, timeout_s=2, mcp_api_key="secret")
        self.assertIsInstance(ok, dict)


class TestLock(unittest.TestCase):
    def test_acquire_and_release_lock(self):
        with tempfile.TemporaryDirectory() as td:
            lock = os.path.join(td, "lock.json")
            M.acquire_lock(lock, stale_after_s=60)
            self.assertTrue(os.path.exists(lock))
            M.release_lock(lock)
            self.assertFalse(os.path.exists(lock))

    def test_lock_running_pid(self):
        with tempfile.TemporaryDirectory() as td:
            lock = os.path.join(td, "lock.json")
            with open(lock, "w", encoding="utf-8") as f:
                json.dump({"pid": os.getpid(), "started_at": time.time() - 5}, f)
            with self.assertRaises(RuntimeError) as ctx:
                M.acquire_lock(lock, stale_after_s=60)
            self.assertIn("already running", str(ctx.exception))

    def test_stale_lock_replaced(self):
        with tempfile.TemporaryDirectory() as td:
            lock = os.path.join(td, "lock.json")
            with open(lock, "w", encoding="utf-8") as f:
                json.dump({"pid": 999999, "started_at": time.time() - 10_000}, f)
            M.acquire_lock(lock, stale_after_s=60)
            with open(lock, "r", encoding="utf-8") as f:
                j = json.load(f)
            self.assertEqual(int(j.get("pid")), os.getpid())
            M.release_lock(lock)

    def test_recent_dead_pid_lock_replaced(self):
        with tempfile.TemporaryDirectory() as td:
            lock = os.path.join(td, "lock.json")
            with open(lock, "w", encoding="utf-8") as f:
                json.dump({"pid": 999999, "started_at": time.time()}, f)
            M.acquire_lock(lock, stale_after_s=60)
            with open(lock, "r", encoding="utf-8") as f:
                j = json.load(f)
            self.assertEqual(int(j.get("pid")), os.getpid())
            M.release_lock(lock)


if __name__ == "__main__":
    unittest.main()
