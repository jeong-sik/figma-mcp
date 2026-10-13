"""Host-side MCP and Figma REST transport. No credentials cross into the Plugin."""

from __future__ import annotations

import base64
import http.client
import json
import math
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
import uuid
from collections.abc import Callable
from typing import Any


DEFAULT_REQUEST_TIMEOUT_SECONDS = 30.0


class TransportError(RuntimeError):
    pass


class ApplicationRefused(RuntimeError):
    """The worker proved that this delivery was rejected before any effect."""


class RequestTooLarge(TransportError):
    """A request rejected locally before any network effect."""


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        raise TransportError("Redirect refused for an authenticated request")


def encode(value: Any) -> bytes:
    return json.dumps(
        value, ensure_ascii=False, allow_nan=False, separators=(",", ":")
    ).encode()


def request_timeout(value: Any) -> float:
    if type(value) not in (int, float) or not math.isfinite(value) or value <= 0:
        raise ValueError("request_timeout_seconds must be a positive finite number")
    return float(value)


def request_message(method: str, params: dict, identity: str | int | None) -> dict:
    value = {"jsonrpc": "2.0", "method": method, "params": params}
    if identity is not None:
        value["id"] = identity
    return value


def read_limited(response, limit: int) -> bytes:
    data = response.read(limit + 1)
    if len(data) > limit:
        raise TransportError("Response exceeds the configured package envelope")
    return data


class McpClient:
    def __init__(
        self,
        url: str,
        token: str,
        max_bytes: int = 16777216,
        *,
        request_timeout_seconds: float = DEFAULT_REQUEST_TIMEOUT_SECONDS,
    ):
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
        self.request_timeout_seconds = request_timeout(request_timeout_seconds)
        self.session = None
        self.version = "2025-06-18"
        self.lock = threading.Lock()
        self.opener = urllib.request.build_opener(NoRedirect())

    def _request(
        self,
        method: str,
        params: dict,
        *,
        notification: bool = False,
        identity: str | None = None,
    ) -> dict:
        request_id = (
            None
            if notification
            else (uuid.uuid4().hex if identity is None else identity)
        )
        body = request_message(method, params, request_id)
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
            with self.opener.open(
                request, timeout=self.request_timeout_seconds
            ) as response:
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
        except (OSError, http.client.HTTPException) as error:
            raise TransportError(
                "MCP connection failed or timed out; outcome may be unknown; request was not replayed"
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

    def call_result(
        self,
        name: str,
        arguments: dict,
        *,
        compact: Callable[[Callable[[dict], bool]], dict] | None = None,
    ) -> dict:
        with self.lock:
            try:
                response = self._request(
                    "tools/call", {"name": name, "arguments": arguments}
                )
            except RequestTooLarge:
                if compact is None:
                    raise
                identity = uuid.uuid4().hex

                def fits(value: dict) -> bool:
                    return (
                        len(
                            encode(
                                request_message(
                                    "tools/call",
                                    {"name": name, "arguments": value},
                                    identity,
                                )
                            )
                        )
                        <= self.max_bytes
                    )

                # Fixed-width UUIDs keep receipt compaction identical on a
                # retry. Use this exact id for sizing and dispatch.
                response = self._request(
                    "tools/call",
                    {"name": name, "arguments": compact(fits)},
                    identity=identity,
                )
        if response.get("isError"):
            texts = [
                x.get("text", "")
                for x in response.get("content", [])
                if x.get("type") == "text"
            ]
            metadata = response.get("_meta")
            failure = (
                metadata.get("io.github.jeong-sik/masc.lane.failure")
                if isinstance(metadata, dict)
                else None
            )
            if (
                isinstance(failure, dict)
                and failure.get("class") == "workflow_rejection"
                and failure.get("effect") == "proven_pre_effect"
            ):
                raise ApplicationRefused("MASC tool refused: " + "\n".join(texts))
            raise TransportError(
                "MASC tool failed; outcome may be unknown: " + "\n".join(texts)
            )
        return response

    def call(
        self,
        name: str,
        arguments: dict,
        *,
        compact: Callable[[Callable[[dict], bool]], dict] | None = None,
    ) -> dict:
        response = self.call_result(name, arguments, compact=compact)
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
        self,
        token: str,
        max_bytes: int,
        *,
        api_url: str = "https://api.figma.com",
        request_timeout_seconds: float = DEFAULT_REQUEST_TIMEOUT_SECONDS,
    ):
        self.token, self.max_bytes, self.api_url = token, max_bytes, api_url.rstrip("/")
        self.request_timeout_seconds = request_timeout(request_timeout_seconds)
        self.opener = urllib.request.build_opener(NoRedirect())

    def get(self, path: str, query: dict | None = None) -> dict:
        url = (
            self.api_url + path + ("?" + urllib.parse.urlencode(query) if query else "")
        )
        request = urllib.request.Request(
            url, headers={"X-Figma-Token": self.token, "Accept": "application/json"}
        )
        try:
            with self.opener.open(
                request, timeout=self.request_timeout_seconds
            ) as response:
                value = json.loads(read_limited(response, self.max_bytes))
        except urllib.error.HTTPError as error:
            retry = error.headers.get("Retry-After")
            detail = f"; Retry-After: {retry}" if error.code == 429 and retry else ""
            error.close()
            raise TransportError(f"Figma returned HTTP {error.code}{detail}") from error
        except (OSError, http.client.HTTPException) as error:
            raise TransportError("Figma connection failed or timed out") from error
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
            with self.opener.open(
                urllib.request.Request(url), timeout=self.request_timeout_seconds
            ) as response:
                png = read_limited(response, self.max_bytes)
        except (OSError, http.client.HTTPException) as error:
            raise TransportError("Figma asset download failed or timed out") from error
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
