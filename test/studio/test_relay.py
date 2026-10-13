import contextlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import io
import json
from pathlib import Path
import sys
import tempfile
import threading
import unittest
import urllib.error
import urllib.request

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "studio"))
from client import FigmaRest, McpClient, TransportError
from relay import Bridge, Server, read_config


@contextlib.contextmanager
def running(server):
    worker = threading.Thread(target=server.serve_forever, daemon=True)
    worker.start()
    try:
        yield server
    finally:
        server.shutdown()
        server.server_close()
        worker.join()


@contextlib.contextmanager
def scripted_mcp(dispatch):
    """Keep real MCP serialization and HTTP errors around the queue fixture."""

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_POST(self):
            request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
            if "id" not in request:
                self.send_response(202)
                self.end_headers()
                return
            if request["method"] == "initialize":
                status, result = 200, {"protocolVersion": "2025-06-18"}
            else:
                status, value = dispatch(request["params"]["arguments"])
                result = {"content": [{"type": "text", "text": json.dumps(value)}]}
            data = json.dumps(
                {"jsonrpc": "2.0", "id": request["id"], "result": result}
            ).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

    with running(ThreadingHTTPServer(("127.0.0.1", 0), Handler)) as server:
        client = McpClient(
            f"http://127.0.0.1:{server.server_port}/mcp", "fixture-token"
        )
        client.initialize()
        yield client


class McpHandler(BaseHTTPRequestHandler):
    messages = []

    def log_message(self, format, *args):
        pass

    def do_POST(self):
        request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        self.messages.append((request, dict(self.headers)))
        if "id" not in request:
            self.send_response(202)
            self.end_headers()
            return
        result = (
            {"protocolVersion": "2025-06-18"}
            if request["method"] == "initialize"
            else {
                "content": [
                    {"type": "text", "text": json.dumps({"worker_boot_id": "boot"})}
                ]
            }
        )
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Mcp-Session-Id", "test-session")
        self.end_headers()
        self.wfile.write(
            b'data: {"jsonrpc":"2.0","method":"notifications/tools/list_changed"}\n\n'
        )
        self.wfile.write(
            (
                "data: "
                + json.dumps({"jsonrpc": "2.0", "id": request["id"], "result": result})
                + "\n\n"
            ).encode()
        )


class FakeClient:
    max_bytes = 100000

    def __init__(self):
        self.calls = []

    def call(self, name, arguments):
        self.calls.append((name, arguments))
        if arguments["kind"] == "status":
            return {"worker_boot_id": "boot", "max_request_bytes": self.max_bytes}
        if arguments["kind"] == "poll":
            return {"command": None}
        return {"status": "recorded"}


class Response(io.BytesIO):
    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()


class RestOpener:
    def __init__(self, version="v7", image=True):
        self.requests = []
        self.version, self.image = version, image

    def open(self, request):
        self.requests.append(request)
        if "/nodes?" in request.full_url:
            value = {
                "version": self.version,
                "nodes": {"1:2": {"document": {"id": "1:2", "type": "FRAME"}}},
            }
        elif "/v1/images/" in request.full_url:
            value = {
                "images": {
                    "1:2": "https://cdn.example.test/asset.png" if self.image else None
                }
            }
        else:
            return Response(b"\x89PNG\r\n\x1a\nfixture")
        return Response(json.dumps(value).encode())


class RelayTests(unittest.TestCase):
    def test_large_rest_result_reports_small_failure_then_processes_next_read(self):
        commands = [
            {"id": identity, "file_key": identity, "root_id": "1:2"}
            for identity in ("large-read", "next-read")
        ]
        received = []
        captures = []
        bridge = None

        def dispatch(arguments):
            received.append(arguments)
            if arguments["kind"] == "rest_poll":
                if commands:
                    return 200, {"command": commands.pop(0)}
                bridge.stopping.set()
                return 200, {"command": None}
            if arguments["operation_id"] == "next-read":
                bridge.stopping.set()
            return 200, {"status": "recorded"}

        with scripted_mcp(dispatch) as client:

            class Rest:
                def capture(self, file_key, *_):
                    captures.append(file_key)
                    return (
                        {"tree_json": "x" * client.max_bytes}
                        if file_key == "large-read"
                        else {"tree_json": "{}"}
                    )

            bridge = Bridge(client, 1)
            bridge.run_rest(Rest(), "boot")
        results = [call for call in received if call["kind"] == "rest_result"]
        self.assertEqual(captures, ["large-read", "next-read"])
        self.assertEqual([call["operation_id"] for call in results], captures)
        self.assertFalse(results[0]["ok"])
        self.assertEqual(
            results[0]["payload"],
            {"error": "REST capture exceeds the MCP request envelope"},
        )
        self.assertTrue(results[1]["ok"])
        # The real client rejected the large request before HTTP dispatch:
        # only its compact failure receipt reached this endpoint.
        self.assertEqual(len(received), 4)

    def test_lost_rest_reply_refreshes_boot_without_repeating_previous_work(self):
        for lost_at in ("rest_poll", "rest_result"):
            with self.subTest(lost_at=lost_at):
                commands = [
                    {"id": identity, "file_key": identity, "root_id": "1:2"}
                    for identity in ("uncertain-read", "next-read")
                ]
                received, captures = [], []
                bridge = None

                def dispatch(arguments):
                    received.append(arguments)
                    if arguments["kind"] == "status":
                        return 200, {"worker_boot_id": "new-boot"}
                    if arguments["kind"] == "rest_poll":
                        if not commands:
                            bridge.stopping.set()
                            return 200, {"command": None}
                        command = commands.pop(0)
                        if command["id"] == "uncertain-read" and lost_at == "rest_poll":
                            # The server claimed this read, then the reply was lost.
                            return 503, {}
                        return 200, {"command": command}
                    if arguments["operation_id"] == "uncertain-read":
                        # The server retained this result, then its acknowledgement was lost.
                        return 503, {}
                    bridge.stopping.set()
                    return 200, {"status": "recorded"}

                class Rest:
                    def capture(self, file_key, *_):
                        captures.append(file_key)
                        return {"tree_json": "{}"}

                with scripted_mcp(dispatch) as client:
                    bridge = Bridge(client, 1)
                    with contextlib.redirect_stderr(io.StringIO()):
                        bridge.run_rest(Rest(), "old-boot")
                expected_captures = (
                    ["next-read"]
                    if lost_at == "rest_poll"
                    else ["uncertain-read", "next-read"]
                )
                self.assertEqual(captures, expected_captures)
                self.assertEqual(sum(call["kind"] == "status" for call in received), 1)
                polls = [call for call in received if call["kind"] == "rest_poll"]
                self.assertEqual(
                    [call["worker_boot_id"] for call in polls], ["old-boot", "new-boot"]
                )
                results = [call for call in received if call["kind"] == "rest_result"]
                self.assertEqual(
                    [call["operation_id"] for call in results], expected_captures
                )
                self.assertEqual(results[-1]["worker_boot_id"], "new-boot")

    def test_real_mcp_http_sse_session_and_model_text_result(self):
        McpHandler.messages = []
        with running(ThreadingHTTPServer(("127.0.0.1", 0), McpHandler)) as server:
            client = McpClient(
                f"http://127.0.0.1:{server.server_port}/mcp", "fixture-token"
            )
            client.initialize()
            self.assertEqual(
                client.call("masc_figma_relay", {"kind": "status"}),
                {"worker_boot_id": "boot"},
            )
        self.assertEqual(len(McpHandler.messages), 3)
        self.assertEqual(McpHandler.messages[-1][1]["Mcp-Session-Id"], "test-session")
        self.assertEqual(
            McpHandler.messages[-1][1]["Mcp-Protocol-Version"], "2025-06-18"
        )
        self.assertEqual(
            McpHandler.messages[-1][1]["Authorization"], "Bearer fixture-token"
        )

    def test_loopback_pairing_auth_origin_and_session_binding(self):
        client = FakeClient()
        with running(Server(0, Bridge(client, 1000), "pair-token")) as server:
            url = f"http://127.0.0.1:{server.server_port}"

            def post(path, body, token="pair-token", origin="null"):
                request = urllib.request.Request(
                    url + path,
                    data=json.dumps(body).encode(),
                    headers={
                        "Authorization": "Bearer " + token,
                        "Content-Type": "application/json",
                        "Origin": origin,
                    },
                )
                return urllib.request.urlopen(request)

            with self.assertRaises(urllib.error.HTTPError) as denied:
                post("/session", {"session_id": "one", "document": {}}, token="wrong")
            self.assertEqual(denied.exception.code, 401)
            denied.exception.close()
            with self.assertRaises(urllib.error.HTTPError) as denied:
                post(
                    "/session",
                    {"session_id": "one", "document": {}},
                    origin="https://untrusted.test",
                )
            self.assertEqual(denied.exception.code, 403)
            denied.exception.close()
            with post("/session", {"session_id": "one", "document": {}}) as response:
                self.assertEqual(json.load(response)["worker_boot_id"], "boot")
            with post("/poll", {"session_id": "one"}) as response:
                self.assertIsNone(json.load(response)["command"])
            self.assertEqual(client.calls[-1][1]["session_id"], "one")
            self.assertEqual(client.calls[-1][1]["worker_boot_id"], "boot")

    def test_rest_capture_pins_render_version_and_never_sends_pat_to_asset(self):
        rest = FigmaRest("private-pat", 100000)
        opener = RestOpener()
        rest.opener = opener
        capture = rest.capture("file-key", "1:2")
        self.assertEqual(capture["version"], "v7")
        self.assertEqual(capture["source"], "rest_version")
        self.assertIn("version=v7", opener.requests[1].full_url)
        self.assertEqual(opener.requests[0].get_header("X-figma-token"), "private-pat")
        self.assertIsNone(opener.requests[2].get_header("X-figma-token"))
        self.assertIsNone(opener.requests[2].get_header("Authorization"))
        rest.opener = RestOpener(version="wrong")
        with self.assertRaises(TransportError):
            rest.capture("file-key", "1:2", "v7")
        rest.opener = RestOpener(image=False)
        with self.assertRaises(TransportError):
            rest.capture("file-key", "1:2")

    def test_private_config_is_required(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "relay.toml"
            path.write_text('[relay]\npairing_token="fixture"\n')
            path.chmod(0o644)
            with self.assertRaises(ValueError):
                read_config(path)
            path.chmod(0o600)
            self.assertEqual(read_config(path)["relay"]["pairing_token"], "fixture")


if __name__ == "__main__":
    unittest.main()
