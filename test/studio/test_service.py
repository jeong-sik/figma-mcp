import json
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
import unittest
import urllib.error
import urllib.request

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "studio"))
from service import PROTOCOL, Server, Service


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
        self.app.store.db.close()
        self.directory.cleanup()

    def post(self, path, body, token="client"):
        request = urllib.request.Request(
            f"http://127.0.0.1:{self.server.server_port}" + path,
            data=json.dumps(body).encode(), headers={
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
        self.app.store.db.close()
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


if __name__ == "__main__":
    unittest.main()
