"""Figma REST transport. Credentials never enter the Plugin."""

from __future__ import annotations

import base64
import http.client
import json
import math
import time
import urllib.error
import urllib.parse
import urllib.request
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



def read_limited(response, limit: int) -> bytes:
    data = response.read(limit + 1)
    if len(data) > limit:
        raise TransportError("Response exceeds the configured envelope")
    return data


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
        # Asset requests never carry the Figma PAT.
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
