import base64
import hashlib
import json
from pathlib import Path
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "studio"))
from service import Service
from state import encode, Refused


class ReadbackTests(unittest.TestCase):
    def test_large_readback_remains_exact_and_status_stays_readable(self):
        with tempfile.TemporaryDirectory() as directory:
            app = Service(Path(directory), max_bytes=1024)
            try:
                document = {"name": "doc", "page_id": "page", "selection": [], "editor_type": "figma", "mode": "default", "can_edit": True}
                pair = app.handle("/v1/plugin/session", {"protocol": "figma-studio/1", "session_id": "session", "document": document}, plugin=True)
                identity = pair["service_id"]
                app.handle("/v1/operations", {"service_id": identity, "operation_id": "op", "session_id": "session", "kind": "capture", "root_id": "root"})
                app.handle("/v1/plugin/poll", {"service_id": identity, "session_id": "session"}, plugin=True)
                payload = {"error": "readback unavailable", "completed": [{"node_id": "node:" + str(i)} for i in range(100)]}
                raw = {"ok": False, "payload": payload}
                app.handle("/v1/plugin/result", {"service_id": identity, "session_id": "session", "operation_id": "op", **raw}, plugin=True)
                operation = app.handle("/v1/operation", {"operation_id": "op"})
                app.check(operation)
                self.assertTrue(operation["result"]["readback_available"])
                self.assertNotIn("relay_result", operation["result"])
                output = bytearray()
                while True:
                    chunk = app.handle("/v1/readback", {"operation_id": "op", "offset": len(output), "limit": 177})
                    app.check(chunk)
                    output.extend(base64.b64decode(chunk["data_base64"], validate=True))
                    if chunk["next_offset"] is None:
                        break
                    self.assertEqual(chunk["next_offset"], len(output))
                self.assertEqual(bytes(output), encode(raw).encode())
                self.assertEqual(hashlib.sha256(output).hexdigest(), chunk["sha256"])
                self.assertEqual(json.loads(output), raw)
                with self.assertRaises(Refused):
                    app.handle("/v1/readback", {"operation_id": "op", "offset": -1})
            finally:
                app.store.close()


if __name__ == "__main__":
    unittest.main()
