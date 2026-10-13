"""Host-side MCP and Figma REST transport. No credentials cross into the Plugin."""

from __future__ import annotations

import base64
import json
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
from typing import Any


class TransportError(RuntimeError):
    pass


class RequestTooLarge(TransportError):
    """A request rejected locally before any network effect."""


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        raise TransportError("Redirect refused for an authenticated request")


def encode(value: Any) -> bytes:
    return json.dumps(
        value, ensure_ascii=False, allow_nan=False, separators=(",", ":")
    ).encode()


def read_limited(response, limit: int) -> bytes:
    data = response.read(limit + 1)
    if len(data) > limit:
        raise TransportError("Response exceeds the configured package envelope")
    return data


class McpClient:
    def __init__(self, url: str, token: str, max_bytes: int = 16777216):
        parsed = urllib.parse.urlsplit(url)
        if parsed.scheme != "https" and not (
            parsed.scheme == "http"
            and parsed.hostname in ("127.0.0.1", "localhost", "::1")
        ):
            raise TransportError("MCP must use HTTPS or a loopback HTTP endpoint")
        if parsed.username or parsed.password or not token:
            raise TransportError(
                "Use a configured bearer credential, not URL credentials"
            )
        self.url, self.token, self.max_bytes = url, token, max_bytes
        self.session = None
        self.version = "2025-06-18"
        self.sequence = 0
        self.lock = threading.Lock()
        self.opener = urllib.request.build_opener(NoRedirect())

    def _request(
        self, method: str, params: dict, *, notification: bool = False
    ) -> dict:
        self.sequence += 1
        body: dict = {"jsonrpc": "2.0", "method": method, "params": params}
        if not notification:
            body["id"] = self.sequence
        headers = {
            "Authorization": "Bearer " + self.token,
            "Content-Type": "application/json",
            "Accept": "application/json, text/event-stream",
            "MCP-Protocol-Version": self.version,
        }
        if self.session:
            headers["Mcp-Session-Id"] = self.session
        encoded = encode(body)
        if len(encoded) > self.max_bytes:
            raise RequestTooLarge("MCP request exceeds the package envelope")
        request = urllib.request.Request(
            self.url, data=encoded, headers=headers, method="POST"
        )
        try:
            with self.opener.open(request) as response:
                session = response.headers.get("Mcp-Session-Id")
                if session:
                    self.session = session
                if notification:
                    return {}
                if response.headers.get_content_type() == "text/event-stream":
                    event = []
                    consumed = 0
                    message = None
                    while True:
                        line = response.readline(self.max_bytes - consumed + 1)
                        if not line:
                            break
                        consumed += len(line)
                        if consumed > self.max_bytes:
                            raise TransportError(
                                "MCP stream exceeds the package envelope"
                            )
                        line = line.decode("utf-8").rstrip("\r\n")
                        if line.startswith("data:"):
                            event.append(line[5:].lstrip(" "))
                        elif not line and event:
                            candidate = json.loads("\n".join(event))
                            event = []
                            if candidate.get("id") == body["id"]:
                                message = candidate
                                break
                    if message is None:
                        raise TransportError(
                            "MCP stream ended without this response; outcome may be unknown"
                        )
                else:
                    message = json.loads(read_limited(response, self.max_bytes))
        except urllib.error.HTTPError as error:
            if error.code == 404:
                self.session = None
            error.close()
            raise TransportError(
                f"MCP returned HTTP {error.code}; request was not replayed"
            ) from error
        except urllib.error.URLError as error:
            raise TransportError(
                "MCP connection failed; request was not replayed"
            ) from error
        if message.get("id") != body["id"] or message.get("jsonrpc") != "2.0":
            raise TransportError("MCP response identity mismatch")
        if "error" in message:
            raise TransportError(
                "MCP rejected the request: "
                + str(message["error"].get("message", "unknown error"))
            )
        return message["result"]

    def initialize(self) -> None:
        with self.lock:
            response = self._request(
                "initialize",
                {
                    "protocolVersion": self.version,
                    "capabilities": {},
                    "clientInfo": {"name": "figma-studio-relay", "version": "0.1.0"},
                },
            )
            self.version = response["protocolVersion"]
            self._request("notifications/initialized", {}, notification=True)

    def call_result(self, name: str, arguments: dict) -> dict:
        with self.lock:
            response = self._request(
                "tools/call", {"name": name, "arguments": arguments}
            )
        if response.get("isError"):
            texts = [
                x.get("text", "")
                for x in response.get("content", [])
                if x.get("type") == "text"
            ]
            raise TransportError("MASC tool refused: " + "\n".join(texts))
        return response

    def call(self, name: str, arguments: dict) -> dict:
        response = self.call_result(name, arguments)
        value = response.get("structuredContent")
        if not isinstance(value, dict):
            texts = [
                x["text"]
                for x in response.get("content", [])
                if x.get("type") == "text"
            ]
            if len(texts) != 1:
                raise TransportError("MASC tool returned no structured result")
            value = json.loads(texts[0])
        if not isinstance(value, dict):
            raise TransportError("MASC tool result is not an object")
        return value


class FigmaRest:
    def __init__(
        self, token: str, max_bytes: int, *, api_url: str = "https://api.figma.com"
    ):
        self.token, self.max_bytes, self.api_url = token, max_bytes, api_url.rstrip("/")
        self.opener = urllib.request.build_opener(NoRedirect())

    def get(self, path: str, query: dict | None = None) -> dict:
        url = (
            self.api_url + path + ("?" + urllib.parse.urlencode(query) if query else "")
        )
        request = urllib.request.Request(
            url, headers={"X-Figma-Token": self.token, "Accept": "application/json"}
        )
        try:
            with self.opener.open(request) as response:
                value = json.loads(read_limited(response, self.max_bytes))
        except urllib.error.HTTPError as error:
            retry = error.headers.get("Retry-After")
            detail = f"; Retry-After: {retry}" if error.code == 429 and retry else ""
            error.close()
            raise TransportError(f"Figma returned HTTP {error.code}{detail}") from error
        except urllib.error.URLError as error:
            raise TransportError("Figma connection failed") from error
        if not isinstance(value, dict):
            raise TransportError("Figma did not return an object")
        return value

    def capture(self, file_key: str, root_id: str, version: str | None = None) -> dict:
        if not self.token or not file_key or not root_id:
            raise TransportError("REST capture requires a PAT, file key and node ID")
        key = urllib.parse.quote(file_key, safe="")
        query = {"ids": root_id}
        if version is not None:
            query["version"] = version
        nodes = self.get(f"/v1/files/{key}/nodes", query)
        actual_version = nodes.get("version")
        entry = nodes.get("nodes", {}).get(root_id)
        if (
            not isinstance(actual_version, str)
            or not actual_version
            or not isinstance(entry, dict)
        ):
            raise TransportError("Figma did not return the requested node and version")
        if version is not None and actual_version != version:
            raise TransportError("Figma returned a different version")
        if entry.get("document", {}).get("id") != root_id:
            raise TransportError("Figma node identity mismatch")
        rendered = self.get(
            f"/v1/images/{key}",
            {"ids": root_id, "version": actual_version, "format": "png"},
        )
        url = rendered.get("images", {}).get(root_id)
        if not isinstance(url, str):
            raise TransportError("Figma failed to render the requested node")
        parsed = urllib.parse.urlsplit(url)
        if (
            parsed.scheme != "https"
            or not parsed.hostname
            or parsed.username
            or parsed.password
        ):
            raise TransportError("Figma asset URL must be HTTPS without credentials")
        # Asset requests never carry the Figma PAT or MASC credential.
        try:
            with self.opener.open(urllib.request.Request(url)) as response:
                png = read_limited(response, self.max_bytes)
        except urllib.error.URLError as error:
            raise TransportError("Figma asset download failed") from error
        if not png.startswith(b"\x89PNG\r\n\x1a\n"):
            raise TransportError("Figma render did not produce PNG bytes")
        return {
            "schema_version": 1,
            "source": "rest_version",
            "file_key": file_key,
            "version": actual_version,
            "root_id": root_id,
            "tree_json": encode(entry).decode(),
            "observed_at": time.time(),
            "preview": {
                "mime_type": "image/png",
                "data_base64": base64.b64encode(png).decode(),
            },
            "coverage": {
                "stable": True,
                "missing": ["font_files", "variable_values", "component_code_mapping"],
            },
        }
