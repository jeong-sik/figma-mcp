import importlib.util
import pathlib
import sys
import unittest


def load_figma_agent_module():
    here = pathlib.Path(__file__).resolve().parent
    path = here / "figma-agent.py"
    spec = importlib.util.spec_from_file_location("figma_agent", path)
    assert spec and spec.loader
    mod = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = mod
    spec.loader.exec_module(mod)
    return mod


class TestFigmaAgentHelpers(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load_figma_agent_module()

    def test_strip_llm_mcp_extra(self):
        m = self.mod
        self.assertEqual(m.strip_llm_mcp_extra("code"), "code")
        self.assertEqual(
            m.strip_llm_mcp_extra("code\n\n[Extra]\n{\"k\":\"v\"}"),
            "code",
        )

    def test_strip_code_fences(self):
        m = self.mod
        self.assertEqual(m.strip_code_fences("const a = 1;"), "const a = 1;")
        self.assertEqual(
            m.strip_code_fences("```tsx\nconst a = 1;\n```"),
            "const a = 1;",
        )

    def test_parse_llm_mcp_tool_text_ok(self):
        m = self.mod
        resp = {
            "jsonrpc": "2.0",
            "id": 1,
            "result": {"content": [{"type": "text", "text": "hello"}], "isError": False},
        }
        self.assertEqual(m.parse_llm_mcp_tool_text(resp), "hello")

    def test_parse_llm_mcp_tool_text_is_error(self):
        m = self.mod
        resp = {
            "jsonrpc": "2.0",
            "id": 1,
            "result": {"content": [{"type": "text", "text": "boom"}], "isError": True},
        }
        with self.assertRaises(RuntimeError):
            m.parse_llm_mcp_tool_text(resp)

    def test_parse_llm_mcp_tool_text_jsonrpc_error(self):
        m = self.mod
        resp = {"jsonrpc": "2.0", "id": 1, "error": {"code": -1, "message": "nope"}}
        with self.assertRaises(RuntimeError):
            m.parse_llm_mcp_tool_text(resp)
