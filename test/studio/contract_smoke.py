"""Run the real worker, relay and Plugin against a controlled native fixture.

The HTTP host adapter below substitutes only for MASC's authenticated routing.
This does not verify installed MASC/Docker or a live Figma editor.
"""

from __future__ import annotations

import argparse
from contextlib import ExitStack
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
import urllib.request

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "studio"))
from client import McpClient, encode  # noqa: E402 - explicit companion checkout
from preview import png_from_result  # noqa: E402
from relay import Bridge, Server  # noqa: E402


class Worker:
    def __init__(self, path: Path, state: str):
        self.process = subprocess.Popen(
            [sys.executable, str(path / "server.py"), "--state-dir", state],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
        )
        self.lock = threading.Lock()
        self.sequence = 0

    def request(self, method, params):
        with self.lock:
            self.sequence += 1
            self.process.stdin.write(
                json.dumps(
                    {
                        "jsonrpc": "2.0",
                        "id": self.sequence,
                        "method": method,
                        "params": params,
                    }
                )
                + "\n"
            )
            self.process.stdin.flush()
            response = json.loads(self.process.stdout.readline())
            assert response["id"] == self.sequence
            return response["result"]

    def close(self):
        self.process.stdin.close()
        self.process.wait()
        self.process.stdout.close()
        assert self.process.returncode == 0


def running(stack, server):
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()

    def close():
        server.shutdown()
        server.server_close()
        thread.join()

    stack.callback(close)
    return server


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--worker", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=False)
    with ExitStack() as stack:
        state = stack.enter_context(tempfile.TemporaryDirectory())
        worker = Worker(args.worker, state)
        stack.callback(worker.close)
        worker.request("initialize", {})
        observed = worker.request(
            "tools/call",
            {
                "name": "lane_observe",
                "arguments": {
                    "sources": [],
                    "binding": {
                        "installation": "contract-fixture",
                        "keeper": "leader",
                        "relay_agent": "studio-relay",
                        "sources": [],
                    },
                },
            },
        )
        assert not observed.get("isError")

        class HostFixture(BaseHTTPRequestHandler):
            def log_message(self, *_):
                pass

            def do_POST(self):
                request = json.loads(
                    self.rfile.read(int(self.headers["Content-Length"]))
                )
                token = self.headers.get("Authorization")
                assert token in ("Bearer fixture-keeper", "Bearer fixture-relay")
                if "id" not in request:
                    self.send_response(202)
                    self.end_headers()
                    return
                params = request["params"]
                if request["method"] == "tools/call":
                    params = {
                        "name": "lane_call",
                        "arguments": {
                            "tool": params["name"],
                            "arguments": params["arguments"],
                            # Mirrors MASC Full-profile registered Worker
                            # credentials; an HTTP Admin is not an operator.
                            "caller": {"kind": "keeper", "name": "leader"}
                            if token == "Bearer fixture-keeper"
                            else {"kind": "agent", "name": "studio-relay"},
                        },
                    }
                result = worker.request(request["method"], params)
                response = encode(
                    {"jsonrpc": "2.0", "id": request["id"], "result": result}
                )
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(response)))
                self.end_headers()
                self.wfile.write(response)

        host = running(stack, ThreadingHTTPServer(("127.0.0.1", 0), HostFixture))
        url = f"http://127.0.0.1:{host.server_port}/mcp"
        relay_client, keeper = (
            McpClient(url, "fixture-relay"),
            McpClient(url, "fixture-keeper"),
        )
        relay_client.initialize()
        keeper.initialize()
        bridge = running(stack, Server(0, Bridge(relay_client, 1000), "fixture-pair"))
        plugin = subprocess.Popen(
            ["node", str(Path(__file__).with_name("plugin-driver.cjs"))],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
        )

        def close_plugin():
            plugin.stdin.close()
            plugin.wait()
            plugin.stdout.close()
            assert plugin.returncode == 0

        stack.callback(close_plugin)
        context = json.loads(plugin.stdout.readline())
        session, root = context["session_id"], context["root_id"]

        def post(path, values):
            request = urllib.request.Request(
                f"http://127.0.0.1:{bridge.server_port}" + path,
                data=encode({"session_id": session, **values}),
                headers={
                    "Authorization": "Bearer fixture-pair",
                    "Origin": "null",
                    "Content-Type": "application/json",
                },
            )
            with urllib.request.urlopen(request) as response:
                return json.load(response)

        post("/session", {"document": context["document"]})
        keeper.call("masc_figma_bind", {"session_id": session})

        def execute(identity, kind, **values):
            keeper.call(
                "masc_figma_submit",
                {
                    "operation_id": identity,
                    "session_id": session,
                    "root_id": root,
                    "kind": kind,
                    **values,
                },
            )
            command = post("/poll", {})["command"]
            assert command["id"] == identity
            plugin.stdin.write(json.dumps(command) + "\n")
            plugin.stdin.flush()
            reply = json.loads(plugin.stdout.readline())
            result = post(
                "/result",
                {
                    "operation_id": identity,
                    "ok": reply["ok"],
                    "payload": reply["payload"],
                },
            )
            repeated = post(
                "/result",
                {
                    "operation_id": identity,
                    "ok": reply["ok"],
                    "payload": reply["payload"],
                },
            )
            assert result == repeated
            return result

        before = execute("capture-1", "capture")["result"]["snapshot_id"]
        edited = execute(
            "edit-1",
            "apply",
            snapshot_id=before,
            ops=[
                {
                    "op": "create_frame",
                    "params": {
                        "ref": "@card",
                        "parent_id": root,
                        "name": "Card",
                        "width": 320,
                        "height": 240,
                    },
                },
                {
                    "op": "set_auto_layout",
                    "params": {
                        "node_id": "@card",
                        "direction": "VERTICAL",
                        "spacing": 12,
                        "padding": 24,
                    },
                },
                {
                    "op": "create_text",
                    "params": {
                        "ref": "@title",
                        "parent_id": "@card",
                        "text": "Hello Studio",
                        "font": {"family": "Inter", "style": "Regular"},
                        "font_size": 20,
                    },
                },
            ],
        )
        assert edited["state"] == "applied"
        snapshot = edited["result"]["snapshot_id"]
        # The host CLI downloads using the relay agent, never a forged owner.
        result = relay_client.call_result(
            "masc_figma_snapshot", {"snapshot_id": snapshot}
        )
        png = png_from_result(result)
        metadata = json.loads(result["content"][0]["text"])
        title = json.loads(metadata["tree_json"])["document"]["children"][0][
            "children"
        ][0]["id"]
        report = keeper.call(
            "masc_figma_validate",
            {
                "snapshot_id": snapshot,
                "checks": [
                    {"node_id": title, "path": ["characters"], "equals": "Hello Studio"}
                ],
            },
        )
        assert (
            report["verdict"] == "pass" and report["visual_verdict"] == "not_evaluated"
        )
        retained = worker.request(
            "tools/call",
            {
                "name": "lane_observe",
                "arguments": {
                    "binding": {
                        "installation": "contract-fixture",
                        "keeper": "leader",
                        "relay_agent": "studio-relay",
                        "sources": [],
                    },
                    "sources": [],
                },
            },
        )
        assert len(retained["structuredContent"]["artifacts"]) == 3
        result_path = args.output / "snapshot-result.json"
        result_path.write_bytes(encode(result))
        (args.output / "preview.png").write_bytes(png)
        (args.output / "validation.json").write_bytes(encode(report))
        kitty = subprocess.run(
            [
                sys.executable,
                str(ROOT / "studio/preview.py"),
                str(result_path),
                "--kitty",
                "--size",
                "80",
                "24",
            ],
            check=True,
            capture_output=True,
        )
        assert b"\x1b_G" in kitty.stdout
        (args.output / "kitty.bin").write_bytes(kitty.stdout)
        receipt = {
            "scope": "native fixture and host routing fixture; real worker, relay, Plugin and timg",
            "real_figma": False,
            "installed_masc": False,
            "snapshot_id": snapshot,
            "structural_verdict": report["verdict"],
            "visual_verdict": report["visual_verdict"],
            "kitty_protocol_emitted": True,
            "duplicate_result_retained": True,
        }
        (args.output / "receipt.json").write_bytes(encode(receipt))
        print(json.dumps(receipt, indent=2))


if __name__ == "__main__":
    main()
