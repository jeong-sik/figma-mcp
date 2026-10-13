import contextlib
import base64
import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import io
import json
from pathlib import Path
import sys
import subprocess
import tempfile
import threading
import time
import unittest
import urllib.error
import urllib.request
import uuid

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "studio"))
from client import FigmaRest, McpClient, TransportError, encode, request_message
from relay import Bridge, Server, compact_plugin_result, read_config


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


def post_json(server, path, value):
    request = urllib.request.Request(
        f"http://127.0.0.1:{server.server_port}" + path,
        data=encode(value),
        headers={
            "Authorization": "Bearer pair-token",
            "Origin": "null",
            "Content-Type": "application/json",
        },
    )
    with urllib.request.urlopen(request, timeout=5) as response:
        return json.load(response)


@contextlib.contextmanager
def scripted_mcp(dispatch, *, requests=None):
    """Keep real MCP serialization and HTTP errors around the queue fixture."""

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *_):
            pass

        def do_POST(self):
            request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
            if requests is not None:
                requests.append(request)
            if "id" not in request:
                self.send_response(202)
                self.end_headers()
                return
            if request["method"] == "initialize":
                status, result = 200, {"protocolVersion": "2025-06-18"}
            else:
                status, value = dispatch(request["params"]["arguments"])
                if status == 0:
                    # Retain the request but lose the response connection.
                    self.close_connection = True
                    return
                result = (
                    value
                    if "isError" in value
                    else {"content": [{"type": "text", "text": json.dumps(value)}]}
                )
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
    request_timeout_seconds = 30.0

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
        self.timeouts = []
        self.version, self.image = version, image

    def open(self, request, *, timeout):
        self.requests.append(request)
        self.timeouts.append(timeout)
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
    def test_restart_refusal_retains_original_boot_and_does_not_block_new_poll(self):
        boot = "original-boot"
        failure = {"class": "workflow_rejection", "effect": "proven_pre_effect"}
        received = []

        def dispatch(arguments):
            received.append(arguments)
            if arguments["kind"] == "status":
                return 200, {"worker_boot_id": boot, "max_request_bytes": 10000}
            if arguments["kind"] == "result":
                return 200, {
                    "isError": True,
                    "content": [
                        {
                            "type": "text",
                            "text": "Result belongs to another session/worker",
                        }
                    ],
                    "_meta": {"io.github.jeong-sik/masc.lane.failure": failure},
                }
            if arguments["kind"] == "poll":
                return 200, {"command": {"id": "new-read", "kind": "capture"}}
            return 200, {"status": "reported"}

        with scripted_mcp(dispatch) as client:
            bridge = Bridge(client, 1000)
            with running(Server(0, bridge, "pair-token")) as server:
                document = {"session_id": "plugin", "document": {}}
                original = post_json(server, "/session", document)
                pending = {
                    "session_id": "plugin",
                    "worker_boot_id": original["worker_boot_id"],
                    "operation_id": "old-operation",
                    "ok": True,
                    "payload": {},
                }
                # A restarted worker accepts this same Plugin session with a
                # new boot, but cannot accept a receipt for the old operation.
                boot = "restarted-boot"
                current = post_json(server, "/session", document)
                self.assertEqual(current["worker_boot_id"], boot)
                self.assertEqual(bridge.max_bytes, 10000)
                self.assertEqual(client.max_bytes, 10000)
                self.assertEqual(current["request_timeout_seconds"], 30.0)
                with self.assertRaises(urllib.error.HTTPError) as refused:
                    post_json(server, "/result", pending)
                with refused.exception as response:
                    self.assertEqual(response.code, 409)
                    value = json.load(response)
                self.assertEqual(value["kind"], "application_refused")
                self.assertFalse(value["retryable"])
                self.assertEqual(received[-1]["worker_boot_id"], "original-boot")
                self.assertEqual(
                    post_json(server, "/poll", {"session_id": "plugin"})["command"][
                        "id"
                    ],
                    "new-read",
                )
                self.assertEqual(received[-1]["worker_boot_id"], boot)

                # The same error text does not establish a safe terminal
                # refusal without the exact worker effect metadata.
                for failure in (
                    {"class": "runtime_failure", "effect": "effect_outcome_unknown"},
                    {"class": "workflow_rejection", "effect": "effect_outcome_unknown"},
                    {},
                ):
                    with self.subTest(failure=failure):
                        with self.assertRaises(urllib.error.HTTPError) as uncertain:
                            post_json(server, "/result", pending)
                        with uncertain.exception as response:
                            self.assertEqual(response.code, 502)
                            value = json.load(response)
                        self.assertEqual(value["kind"], "transport_error")
                        self.assertTrue(value["retryable"])

                # A locally unrepresentable receipt is also terminal for its
                # delivery, but is not a worker acknowledgement of the edit.
                result_count = sum(item["kind"] == "result" for item in received)
                client.max_bytes = len(encode(pending))
                with self.assertRaises(urllib.error.HTTPError) as too_large:
                    post_json(server, "/result", pending)
                with too_large.exception as response:
                    self.assertEqual(response.code, 413)
                    value = json.load(response)
                self.assertEqual(value["kind"], "request_too_large")
                self.assertFalse(value["retryable"])
                self.assertEqual(
                    sum(item["kind"] == "result" for item in received), result_count
                )

    def test_silent_mcp_header_json_and_sse_reads_timeout_then_release_client(self):
        for stalled_at in ("headers", "json", "sse"):
            with self.subTest(stalled_at=stalled_at):
                release = threading.Event()
                received = []

                class Handler(BaseHTTPRequestHandler):
                    def log_message(self, *_):
                        pass

                    def do_POST(self):
                        request = json.loads(
                            self.rfile.read(int(self.headers["Content-Length"]))
                        )
                        args = request["params"]["arguments"]
                        received.append(args)
                        if args["kind"] == "result":
                            if stalled_at != "headers":
                                self.send_response(200)
                                self.send_header(
                                    "Content-Type",
                                    "text/event-stream"
                                    if stalled_at == "sse"
                                    else "application/json",
                                )
                                self.end_headers()
                                self.wfile.flush()
                            release.wait()
                            return
                        value = (
                            {"worker_boot_id": "boot", "max_request_bytes": 10000}
                            if args["kind"] == "status"
                            else {"command": None}
                            if args["kind"] == "poll"
                            else {"status": "reported"}
                        )
                        body = encode(
                            {
                                "jsonrpc": "2.0",
                                "id": request["id"],
                                "result": {"structuredContent": value},
                            }
                        )
                        self.send_response(200)
                        self.send_header("Content-Type", "application/json")
                        self.send_header("Content-Length", str(len(body)))
                        self.end_headers()
                        self.wfile.write(body)

                try:
                    with running(
                        ThreadingHTTPServer(("127.0.0.1", 0), Handler)
                    ) as upstream:
                        client = McpClient(
                            f"http://127.0.0.1:{upstream.server_port}/mcp",
                            "fixture-token",
                            request_timeout_seconds=0.1,
                        )
                        with running(
                            Server(0, Bridge(client, 1000), "pair-token")
                        ) as server:
                            post_json(
                                server,
                                "/session",
                                {"session_id": "plugin", "document": {}},
                            )
                            start = time.monotonic()
                            with self.assertRaises(urllib.error.HTTPError) as timeout:
                                post_json(
                                    server,
                                    "/result",
                                    {
                                        "session_id": "plugin",
                                        "worker_boot_id": "boot",
                                        "operation_id": "uncertain",
                                        "ok": True,
                                        "payload": {},
                                    },
                                )
                            with timeout.exception as response:
                                self.assertEqual(response.code, 502)
                                value = json.load(response)
                            self.assertEqual(value["kind"], "transport_error")
                            self.assertTrue(value["retryable"])
                            self.assertIn("outcome may be unknown", value["error"])
                            self.assertLess(time.monotonic() - start, 2)
                            # The first server request is still silent. The
                            # client lock must already allow an independent poll.
                            self.assertIsNone(
                                post_json(server, "/poll", {"session_id": "plugin"})[
                                    "command"
                                ]
                            )
                            self.assertEqual(
                                sum(item["kind"] == "result" for item in received), 1
                            )
                finally:
                    release.set()

    def test_lost_plugin_ack_retries_the_identical_compacted_receipt(self):
        received, requests = [], []
        acknowledgement = {"operation_id": "repeat-result", "state": "outcome_unknown"}

        def dispatch(arguments):
            if arguments["kind"] == "status":
                return 200, {
                    "worker_boot_id": "boot",
                    "max_request_bytes": client.max_bytes,
                }
            if arguments["kind"] == "result":
                received.append(arguments)
                if len(received) == 1:
                    return 0, {}
                return (200, acknowledgement) if received[0] == arguments else (409, {})
            if arguments["kind"] == "poll":
                return 200, {"command": {"id": "next-read", "kind": "capture"}}
            return 200, {"status": "recorded"}

        with scripted_mcp(dispatch, requests=requests) as client:
            # Regression: decimal ids 9 and 11 made the same receipt sizes
            # 697 and 698 bytes and therefore retained different node lists.
            client.max_bytes = 697
            with running(Server(0, Bridge(client, 1000), "pair-token")) as server:

                def post(path, value):
                    request = urllib.request.Request(
                        f"http://127.0.0.1:{server.server_port}" + path,
                        data=encode(value),
                        headers={
                            "Authorization": "Bearer pair-token",
                            "Origin": "null",
                            "Content-Type": "application/json",
                        },
                    )
                    with urllib.request.urlopen(request) as response:
                        return json.load(response)

                post("/session", {"session_id": "plugin", "document": {}})
                for _ in range(3):
                    client.call("masc_figma_relay", {"kind": "status"})
                value = {
                    "session_id": "plugin",
                    "worker_boot_id": "boot",
                    "operation_id": "repeat-result",
                    "ok": True,
                    "payload": {
                        "completed": [
                            {"index": n, "op": "rename", "node_id": f"{n}:1"}
                            for n in range(3)
                        ],
                        "padding": "",
                    },
                }
                arguments = {"kind": "result", "worker_boot_id": "boot", **value}
                reference = compact_plugin_result(arguments, lambda _: True)
                reference_size = len(
                    encode(
                        request_message(
                            "tools/call",
                            {"name": "masc_figma_relay", "arguments": reference},
                            9,
                        )
                    )
                )
                value["payload"]["completed"][-1]["node_id"] += "x" * (
                    client.max_bytes - reference_size
                )
                value["payload"]["padding"] = "x" * (634 - len(encode(value)))
                self.assertEqual(len(encode(value)), 634)
                with self.assertRaises(urllib.error.HTTPError) as lost:
                    post("/result", value)
                self.assertEqual(lost.exception.code, 502)
                lost.exception.close()
                self.assertEqual(post("/result", value), acknowledgement)
                self.assertEqual(
                    post("/poll", {"session_id": "plugin"})["command"]["id"],
                    "next-read",
                )
        self.assertEqual(len(received), 2)
        self.assertEqual(received[0], received[1])
        identities = [request["id"] for request in requests if "id" in request]
        self.assertEqual(len(identities), len(set(identities)))
        self.assertTrue(
            all(
                isinstance(identity, str) and uuid.UUID(identity).hex == identity
                for identity in identities
            )
        )

    def test_snapshot_download_uses_relay_credential_without_owner_section(self):
        png = b"\x89PNG\r\n\x1a\nfixture"
        snapshot = {
            "content": [
                {
                    "type": "text",
                    "text": json.dumps(
                        {
                            "snapshot_id": "retained-1",
                            "preview": {"sha256": hashlib.sha256(png).hexdigest()},
                        }
                    ),
                },
                {
                    "type": "image",
                    "mimeType": "image/png",
                    "data": base64.b64encode(png).decode(),
                },
            ],
        }
        requests = []

        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *_):
                pass

            def do_POST(self):
                request = json.loads(
                    self.rfile.read(int(self.headers["Content-Length"]))
                )
                requests.append((request, self.headers.get("Authorization")))
                if "id" not in request:
                    self.send_response(202)
                    self.end_headers()
                    return
                result = (
                    {"protocolVersion": "2025-06-18"}
                    if request["method"] == "initialize"
                    else snapshot
                )
                data = encode({"jsonrpc": "2.0", "id": request["id"], "result": result})
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Length", str(len(data)))
                self.end_headers()
                self.wfile.write(data)

        with tempfile.TemporaryDirectory() as directory:
            config, output = (
                Path(directory) / "relay.toml",
                Path(directory) / "snapshot.json",
            )
            with running(ThreadingHTTPServer(("127.0.0.1", 0), Handler)) as server:
                config.write_text(
                    f'[masc]\nurl="http://127.0.0.1:{server.server_port}/mcp"\ntoken="relay-only"\n'
                    '[relay]\npairing_token="pair-token"\n'
                )
                config.chmod(0o600)
                process = subprocess.run(
                    [
                        sys.executable,
                        str(Path(__file__).resolve().parents[2] / "studio/relay.py"),
                        "snapshot",
                        "--config",
                        str(config),
                        "--snapshot-id",
                        "retained-1",
                        "--output",
                        str(output),
                    ],
                    capture_output=True,
                    text=True,
                )
                self.assertEqual(process.returncode, 0, process.stderr)
                self.assertEqual(json.loads(output.read_text()), snapshot)
        self.assertTrue(
            all(authorization == "Bearer relay-only" for _, authorization in requests)
        )
        self.assertEqual(
            requests[-1][0]["params"],
            {"name": "masc_figma_snapshot", "arguments": {"snapshot_id": "retained-1"}},
        )

    def test_plugin_http_result_fits_only_after_full_mcp_receipt_compaction(self):
        received = []

        def dispatch(arguments):
            received.append(arguments)
            if arguments["kind"] == "status":
                return 200, {
                    "worker_boot_id": "boot",
                    "max_request_bytes": client.max_bytes,
                }
            if arguments["kind"] == "poll":
                return 200, {"command": {"id": "next-read", "kind": "capture"}}
            if arguments["kind"] == "result":
                return 200, {"state": "outcome_unknown"}
            return 200, {"status": "recorded"}

        with scripted_mcp(dispatch) as client:
            with running(Server(0, Bridge(client, 1000), "pair-token")) as server:

                def post(path, value):
                    request = urllib.request.Request(
                        f"http://127.0.0.1:{server.server_port}" + path,
                        data=encode(value),
                        headers={
                            "Authorization": "Bearer pair-token",
                            "Origin": "null",
                            "Content-Type": "application/json",
                        },
                    )
                    with urllib.request.urlopen(request) as response:
                        return json.load(response)

                post("/session", {"session_id": "plugin", "document": {}})
                completed = [{"index": 7, "op": "rename", "node_id": "1:2"}]
                value = {
                    "session_id": "plugin",
                    "worker_boot_id": "boot",
                    "operation_id": "large-result",
                    "ok": False,
                    "payload": {
                        "status": "outcome_unknown",
                        "completed": completed,
                        "completed_count": 4,
                        "completed_omitted": 3,
                        "padding": "",
                    },
                }
                value["payload"]["padding"] = "x" * (
                    client.max_bytes - len(encode(value))
                )
                # This is exactly the advertised Plugin HTTP-body limit.
                self.assertEqual(len(encode(value)), client.max_bytes)
                self.assertEqual(post("/result", value), {"state": "outcome_unknown"})
                self.assertEqual(
                    post("/poll", {"session_id": "plugin"})["command"]["id"],
                    "next-read",
                )
        results = [value for value in received if value["kind"] == "result"]
        self.assertEqual(len(results), 1)
        self.assertFalse(results[0]["ok"])
        self.assertEqual(results[0]["payload"]["status"], "outcome_unknown")
        self.assertEqual(results[0]["payload"]["completed"], completed)
        self.assertEqual(results[0]["payload"]["completed_count"], 4)
        self.assertEqual(results[0]["payload"]["completed_omitted"], 3)
        self.assertNotIn("padding", results[0]["payload"])

    def test_plugin_compaction_counts_further_omitted_completed_ids(self):
        received = []

        def dispatch(arguments):
            received.append(arguments)
            if arguments["kind"] == "status":
                return 200, {
                    "worker_boot_id": "boot",
                    "max_request_bytes": client.max_bytes,
                }
            return 200, {"status": "recorded"}

        with scripted_mcp(dispatch) as client:
            # Exercise the same configurable byte contract with a smaller
            # resource envelope, so a completed-ID list reaches its boundary.
            client.max_bytes = 4096
            bridge = Bridge(client, 1000)
            bridge.handle("/session", {"session_id": "plugin", "document": {}})
            completed = []
            value = {
                "session_id": "plugin",
                "worker_boot_id": "boot",
                "operation_id": "many-completions",
                "ok": False,
                "payload": {
                    "status": "outcome_unknown",
                    "completed": completed,
                    "completed_count": 7,
                    "completed_omitted": 7,
                    "details_omitted": True,
                },
            }
            while True:
                entry = {
                    "index": len(completed) + 7,
                    "op": "rename",
                    "node_id": f"{len(completed)}:1",
                }
                completed.append(entry)
                value["payload"]["completed_count"] = len(completed) + 7
                if len(encode(value)) > client.max_bytes:
                    completed.pop()
                    value["payload"]["completed_count"] -= 1
                    break
            completed[-1]["node_id"] += "x" * (client.max_bytes - len(encode(value)))
            self.assertEqual(len(encode(value)), client.max_bytes)
            bridge.handle("/result", value)
        result = received[-1]["payload"]
        self.assertLess(len(result["completed"]), len(completed))
        self.assertEqual(result["completed"], completed[: len(result["completed"])])
        self.assertEqual(result["completed_count"], len(completed) + 7)
        self.assertEqual(
            result["completed_omitted"], len(completed) + 7 - len(result["completed"])
        )

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
                        return 200, {
                            "worker_boot_id": "new-boot",
                            "max_request_bytes": client.max_bytes,
                        }
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
        rest = FigmaRest("private-pat", 100000, request_timeout_seconds=0.25)
        opener = RestOpener()
        rest.opener = opener
        capture = rest.capture("file-key", "1:2")
        self.assertEqual(capture["version"], "v7")
        self.assertEqual(capture["source"], "rest_version")
        self.assertIn("version=v7", opener.requests[1].full_url)
        self.assertEqual(opener.requests[0].get_header("X-figma-token"), "private-pat")
        self.assertIsNone(opener.requests[2].get_header("X-figma-token"))
        self.assertIsNone(opener.requests[2].get_header("Authorization"))
        self.assertEqual(opener.timeouts, [0.25, 0.25, 0.25])
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
            for value in ("0", "-1", "true", "nan", "inf", '"30"'):
                path.write_text(
                    '[relay]\npairing_token="fixture"\nrequest_timeout_seconds=' + value
                )
                with self.subTest(timeout=value), self.assertRaises(ValueError):
                    read_config(path)


if __name__ == "__main__":
    unittest.main()
