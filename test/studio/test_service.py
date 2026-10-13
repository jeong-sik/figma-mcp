import json
import base64
import os
from pathlib import Path
import stat
import subprocess
import sys
import tempfile
import threading
import unittest
import urllib.error
import urllib.request

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "studio"))
from service import PROTOCOL, Server, Service, snapshot_response
from state import Refused, encode


class ServiceTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.app = Service(Path(self.directory.name))
        self.server = Server(0, self.app, client_token="client", plugin_token="plugin")
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.plugin = subprocess.Popen(
            ["node", str(Path(__file__).with_name("plugin-driver.cjs"))],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True,
        )
        self.context = json.loads(self.plugin.stdout.readline())

    def tearDown(self):
        self.plugin.stdin.close()
        self.plugin.wait()
        self.plugin.stdout.close()
        self.server.shutdown()
        self.server.server_close()
        self.thread.join()
        self.app.store.close()
        self.directory.cleanup()

    def post(self, path, body, token="client"):
        request = urllib.request.Request(
            f"http://127.0.0.1:{self.server.server_port}" + path,
            data=json.dumps(body, ensure_ascii=False, separators=(",", ":")).encode(), headers={
                "Content-Type": "application/json", "Authorization": "Bearer " + token,
            },
        )
        try:
            with urllib.request.urlopen(request, timeout=5) as response:
                return json.load(response)
        except urllib.error.HTTPError as error:
            error.add_note(error.read().decode())
            error.close()
            raise

    def pair(self):
        response = self.post("/v1/plugin/session", {
            "protocol": PROTOCOL, "session_id": self.context["session_id"],
            "document": self.context["document"],
        }, "plugin")
        self.service_id = response["service_id"]
        return response

    def submit(self, identity, kind="capture", **extra):
        return self.post("/v1/operations", {
            "service_id": self.service_id, "operation_id": identity,
            "session_id": self.context["session_id"], "root_id": self.context["root_id"],
            "kind": kind, **extra,
        })

    def execute(self, identity, kind="capture", **extra):
        self.submit(identity, kind, **extra)
        command = self.post("/v1/plugin/poll", {
            "service_id": self.service_id, "session_id": self.context["session_id"],
        }, "plugin")["command"]
        self.assertEqual(command["id"], identity)
        self.plugin.stdin.write(json.dumps(command) + "\n")
        self.plugin.stdin.flush()
        result = json.loads(self.plugin.stdout.readline())
        wire = {"operation_id": identity, "service_id": self.service_id,
                "session_id": self.context["session_id"],
                "ok": result["ok"], "payload": result["payload"]}
        first = self.post("/v1/plugin/result", wire, "plugin")
        self.assertEqual(first, self.post("/v1/plugin/result", wire, "plugin"))
        return self.post("/v1/operation", {"operation_id": identity})

    def test_independent_client_capture_edit_and_snapshot(self):
        info = self.post("/v1/info", {})
        self.assertEqual(info["protocol"], PROTOCOL)
        self.assertIn("native.apply", info["capabilities"])
        self.pair()
        capture = self.execute("capture")
        self.assertEqual(capture["state"], "captured")
        snapshot = self.post("/v1/snapshot", {"snapshot_id": capture["result"]["snapshot_id"]})
        edited = self.execute("edit", "apply", expected_tree_json=snapshot["tree_json"], ops=[{
            "op": "create_frame", "params": {"ref": "@card", "parent_id": self.context["root_id"],
                                               "name": "Independent client", "width": 120, "height": 80},
        }])
        self.assertEqual(edited["state"], "applied")
        final = self.post("/v1/snapshot", {"snapshot_id": edited["result"]["snapshot_id"]})
        tree = json.loads(final["tree_json"])
        self.assertEqual(tree["document"]["children"][0]["name"], "Independent client")
        self.assertIsNotNone(final["preview"]["data_base64"])

    def test_credentials_and_protocol_fail_before_dispatch(self):
        with self.assertRaises(urllib.error.HTTPError) as caught:
            self.post("/v1/status", {}, "plugin")
        self.assertEqual(caught.exception.code, 409)
        with self.assertRaises(urllib.error.HTTPError):
            self.post("/v1/plugin/session", {
                "protocol": "figma-studio/2", "session_id": self.context["session_id"],
                "document": self.context["document"],
            }, "plugin")
        self.assertEqual(self.post("/v1/status", {})["sessions"], [])

    def test_restart_retains_outcomes_and_refuses_stale_delivery(self):
        self.pair()
        self.submit("pending")
        self.submit("in-flight")
        self.post("/v1/plugin/poll", {"service_id": self.service_id,
                  "session_id": self.context["session_id"]}, "plugin")
        self.app.store.close()
        self.app.store = Service(Path(self.directory.name)).store
        states = {self.post("/v1/operation", {"operation_id": key})["state"] for key in ["pending", "in-flight"]}
        self.assertEqual(states, {"rejected", "outcome_unknown"})
        with self.assertRaises(urllib.error.HTTPError):
            self.submit("new-stale")

    def test_operation_identity_cannot_change_input(self):
        self.pair()
        self.submit("same")
        self.submit("same")
        with self.assertRaises(urllib.error.HTTPError):
            self.submit("same", root_id="other")

    def test_duplicate_writer_preserves_live_sessions_and_prepared_work(self):
        self.pair()
        self.submit("pending")
        with self.assertRaisesRegex(ValueError, "already has a writer"):
            Service(Path(self.directory.name))
        self.assertEqual(self.post("/v1/operation", {"operation_id": "pending"})["state"], "prepared")
        self.assertEqual(len(self.post("/v1/status", {})["sessions"]), 1)

    def test_new_session_cannot_make_status_unreadable(self):
        self.app.max_bytes = 1200
        self.context["document"]["name"] = "a" * 500
        self.pair()
        with self.assertRaises(urllib.error.HTTPError):
            self.post("/v1/plugin/session", {"protocol": PROTOCOL, "session_id": "another",
                      "document": {**self.context["document"], "name": "b" * 500}}, "plugin")
        self.assertEqual(len(self.post("/v1/status", {})["sessions"]), 1)
        self.submit("readable")

    def test_state_is_private_even_with_permissive_umask(self):
        with tempfile.TemporaryDirectory() as root:
            previous = os.umask(0)
            try:
                path = Path(root) / "state"
                app = Service(path)
            finally:
                os.umask(previous)
            try:
                self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o700)
                for file in path.iterdir():
                    self.assertEqual(stat.S_IMODE(file.stat().st_mode), 0o600)
            finally:
                app.store.close()

    def rest_capture(self):
        return {"schema_version": 1, "source": "rest_version", "file_key": "file",
                "version": "v1", "root_id": "1:2", "tree_json": '{"document":{"id":"1:2"}}',
                "preview": {"mime_type": "image/png", "data_base64": base64.b64encode(b"\x89PNG\r\n\x1a\nfixture").decode()},
                "coverage": {"stable": True, "missing": []}, "observed_at": "now"}

    def test_actual_snapshot_envelope_is_checked_before_publication(self):
        capture = self.rest_capture()
        self.app.rest = type("Rest", (), {"capture": lambda _, *args: capture})()
        self.app.max_bytes = len(encode(capture).encode())
        with self.assertRaisesRegex(Refused, "envelope"):
            self.app.handle("/v1/rest-capture", {"file_key": "file", "root_id": "1:2"})
        self.assertIsNone(self.app.store.meta("latest_snapshot"))
        self.assertEqual(self.app.store.db.execute("SELECT COUNT(*) FROM snapshots").fetchone()[0], 0)
        # Native and REST captures share the exact response projection.
        self.app.max_bytes = 100000
        result = self.app.handle("/v1/rest-capture", {"file_key": "file", "root_id": "1:2"})
        metadata, image = self.app.store.snapshot(result["snapshot_id"])
        self.assertEqual(result, snapshot_response(result["snapshot_id"], metadata, image))

    def test_native_oversize_keeps_previous_snapshot_and_allows_next_capture(self):
        self.pair()
        before = self.execute("before")["result"]["snapshot_id"]
        self.submit("too-large")
        command = self.post("/v1/plugin/poll", {
            "service_id": self.service_id, "session_id": self.context["session_id"],
        }, "plugin")["command"]
        self.plugin.stdin.write(json.dumps(command) + "\n")
        self.plugin.stdin.flush()
        result = json.loads(self.plugin.stdout.readline())
        result["payload"]["coverage"]["missing"].append("fixture:" + "x" * 3000)
        wire = {"service_id": self.service_id, "session_id": self.context["session_id"],
                "operation_id": "too-large", "ok": result["ok"], "payload": result["payload"]}
        projected = []

        def inspect(identity, metadata, image):
            projected.append(snapshot_response(identity, metadata, image))
            raise Refused("probe stops before publication")

        with self.assertRaises(Refused):
            self.app.store.save_snapshot(result["payload"], self.context["session_id"], self.context["root_id"], check=inspect)
        maximum = len(encode(projected[0]).encode()) - 1
        self.app.max_bytes = maximum
        # Exercise native snapshot admission directly; the HTTP ingress limit
        # may independently reject this oversized receipt even earlier.
        self.app.handle("/v1/plugin/result", wire, plugin=True)
        operation = self.post("/v1/operation", {"operation_id": "too-large"})
        self.assertEqual(operation["state"], "outcome_unknown")
        self.assertTrue(operation["result"]["readback_available"])
        self.assertEqual(self.app.store.meta("latest_snapshot"), before)
        self.app.max_bytes = 16777216
        self.assertEqual(self.execute("after")["state"], "captured")

    def test_slow_rest_does_not_block_plugin_or_status(self):
        started, release = threading.Event(), threading.Event()
        capture = self.rest_capture()

        def delayed(*args):
            started.set()
            if not release.wait(5):
                raise RuntimeError("Test did not release REST capture")
            return capture

        self.app.rest = type("Rest", (), {"capture": delayed})()
        results = []
        thread = threading.Thread(target=lambda: results.append(self.post(
            "/v1/rest-capture", {"file_key": "file", "root_id": "1:2"})))
        thread.start()
        try:
            self.assertTrue(started.wait(2))
            self.pair()
            self.assertEqual(len(self.post("/v1/status", {})["sessions"]), 1)
        finally:
            release.set()
            thread.join(5)
        self.assertEqual(len(results), 1)


if __name__ == "__main__":
    unittest.main()
