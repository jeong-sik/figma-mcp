import pathlib
import re
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
UI = (ROOT / "plugin" / "ui.html").read_text(encoding="utf-8")
CODE = (ROOT / "plugin" / "code.js").read_text(encoding="utf-8")
RENDER = (ROOT / "scripts" / "render-html.js").read_text(encoding="utf-8")


class TestPluginSecurityRegression(unittest.TestCase):
    def test_ui_does_not_use_wildcard_postmessage(self):
        # PostMessage target origin should be a variable, not a literal "*".
        self.assertIsNone(
            re.search(r"parent\.postMessage\([^;]*,\s*['\"]\*['\"]\s*\)", UI),
            msg="Found parent.postMessage(..., '*') in ui.html",
        )

    def test_ui_validates_incoming_message_source(self):
        self.assertIn("ev.source !== parent", UI)
        self.assertIn("PARENT_ORIGIN", UI)
        self.assertIn("postToPlugin", UI)

    def test_nonce_handshake_exists(self):
        self.assertIn('type: "hello"', UI)
        self.assertIn('type === "init"', UI)
        self.assertIn("sessionNonce", UI)
        self.assertIn("sessionNonce", CODE)
        self.assertIn('type: "init"', CODE)
        self.assertIn("msg.nonce", CODE)

    def test_render_html_blocks_network_by_default(self):
        # The render script should prevent SSRF / local file exfil by default.
        self.assertRegex(RENDER, r"page\.route\(\s*['\"]\*\*/\*['\"]")
        self.assertIn("route.abort()", RENDER)
        self.assertIn("FIGMA_MCP_RENDER_HTML_ALLOW_NETWORK", RENDER)


if __name__ == "__main__":
    unittest.main()
