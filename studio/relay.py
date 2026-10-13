#!/usr/bin/env python3
"""Loopback transport joining the Figma Plugin to MASC's isolated Studio worker."""

from __future__ import annotations

import argparse
import base64
import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import secrets
import stat
import sys
import threading
import tomllib
from typing import Any, cast

from client import FigmaRest, McpClient, RequestTooLarge, TransportError, encode


class Bridge:
    def __init__(self, client: McpClient, poll_interval_ms: int):
        self.client = client
        self.poll_interval_ms = poll_interval_ms
        self.sessions: dict[str, str] = {}
        self.max_bytes = client.max_bytes
        self.stopping = threading.Event()

    def run_rest(self, rest: FigmaRest, worker_boot_id: str) -> None:
        boot: str | None = worker_boot_id
        while not self.stopping.wait(self.poll_interval_ms / 1000):
            try:
                if boot is None:
                    boot = self.client.call("masc_figma_relay", {"kind": "status"})[
                        "worker_boot_id"
                    ]
                answer = self.client.call(
                    "masc_figma_relay",
                    {"kind": "rest_poll", "worker_boot_id": boot},
                )
                command = answer["command"]
                if command is None:
                    continue
                try:
                    payload = rest.capture(
                        command["file_key"], command["root_id"], command.get("version")
                    )
                    ok = True
                except (TransportError, ValueError, KeyError) as error:
                    payload, ok = {"error": str(error)}, False
                result = {
                    "kind": "rest_result",
                    "worker_boot_id": boot,
                    "operation_id": command["id"],
                    "ok": ok,
                    "payload": payload,
                }
                try:
                    self.client.call("masc_figma_relay", result)
                except RequestTooLarge:
                    # The large request never left this process. Retain a small
                    # failure receipt so later independent reads can continue.
                    self.client.call(
                        "masc_figma_relay",
                        {
                            **result,
                            "ok": False,
                            "payload": {
                                "error": "REST capture exceeds the MCP request envelope"
                            },
                        },
                    )
            except (TransportError, ValueError, KeyError) as error:
                # Never resend the uncertain poll/result or repeat its capture.
                # A fresh status and poll can only deliver a new prepared read.
                print(f"Studio REST relay reconnecting: {error}", file=sys.stderr)
                boot = None

    def handle(self, path: str, data: dict[str, Any]) -> dict:
        session = data.get("session_id")
        if not isinstance(session, str) or not session:
            raise ValueError("Plugin session_id is required")
        if path == "/session":
            if set(data) != {"session_id", "document"}:
                raise ValueError("Session requires document metadata")
            info = self.client.call("masc_figma_relay", {"kind": "status"})
            self.max_bytes = info["max_request_bytes"]
            boot = info["worker_boot_id"]
            self.client.call(
                "masc_figma_relay", {"kind": "session", "worker_boot_id": boot, **data}
            )
            self.sessions[session] = boot
            return {
                "worker_boot_id": boot,
                "max_request_bytes": self.max_bytes,
                "poll_interval_ms": self.poll_interval_ms,
            }
        boot = self.sessions.get(session)
        if boot is None:
            raise ValueError("Pair this Plugin session first")
        if path == "/poll":
            if set(data) != {"session_id"}:
                raise ValueError("Poll requires only session_id")
            return self.client.call(
                "masc_figma_relay", {"kind": "poll", "worker_boot_id": boot, **data}
            )
        if path == "/result":
            if set(data) != {"session_id", "operation_id", "ok", "payload"}:
                raise ValueError("Result fields do not match the Studio contract")
            return self.client.call(
                "masc_figma_relay", {"kind": "result", "worker_boot_id": boot, **data}
            )
        raise ValueError("Unknown Studio endpoint")


class Server(ThreadingHTTPServer):
    daemon_threads = True

    def __init__(self, port: int, bridge: Bridge, token: str):
        super().__init__(("127.0.0.1", port), Handler)
        self.bridge, self.token = bridge, token


class Handler(BaseHTTPRequestHandler):
    @property
    def studio_server(self) -> Server:
        return cast(Server, self.server)

    def log_message(self, format: str, *args: Any) -> None:
        pass

    def allowed_origin(self) -> bool:
        return self.headers.get("Origin") in (
            None,
            "null",
            "https://www.figma.com",
            "https://figma.com",
        )

    def reply(self, status: int, value: dict) -> None:
        body = encode(value)
        self.send_response(status)
        origin = self.headers.get("Origin")
        if self.allowed_origin() and origin:
            self.send_header("Access-Control-Allow-Origin", origin)
            self.send_header("Vary", "Origin")
        self.send_header("Access-Control-Allow-Headers", "Authorization, Content-Type")
        self.send_header("Access-Control-Allow-Methods", "POST, OPTIONS")
        self.send_header("Access-Control-Allow-Private-Network", "true")
        self.send_header("Content-Type", "application/json")
        self.send_header("Cache-Control", "no-store")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_OPTIONS(self):
        self.reply(200 if self.allowed_origin() else 403, {})

    def do_POST(self):
        expected_host = f"127.0.0.1:{self.studio_server.server_port}"
        if self.headers.get("Host") != expected_host or not self.allowed_origin():
            self.reply(403, {"error": "Unexpected host or origin"})
            return
        if not secrets.compare_digest(
            self.headers.get("Authorization", ""), "Bearer " + self.studio_server.token
        ):
            self.reply(401, {"error": "Invalid Studio pairing token"})
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            if length <= 0 or length > self.studio_server.bridge.max_bytes:
                self.close_connection = True
                self.reply(413, {"error": "Request exceeds the Studio envelope"})
                return
            data = json.loads(self.rfile.read(length))
            if not isinstance(data, dict):
                raise ValueError("Request must be an object")
            result = self.studio_server.bridge.handle(self.path, data)
            self.reply(200, result)
        except (ValueError, KeyError, TypeError) as error:
            self.reply(400, {"error": str(error)})
        except TransportError as error:
            self.reply(502, {"error": str(error)})


def read_config(path: Path) -> dict:
    if (
        not stat.S_ISREG(path.stat().st_mode)
        or stat.S_IMODE(path.stat().st_mode) & 0o077
    ):
        raise ValueError(
            "Relay configuration must be a private regular file (chmod 600)"
        )
    config = tomllib.loads(path.read_text())
    if not config.get("relay", {}).get("pairing_token"):
        raise ValueError("Configure a Studio pairing token")
    return config


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "command",
        choices=["init", "serve", "pairing-token", "rest-capture", "snapshot"],
    )
    parser.add_argument("--config", type=Path, required=True)
    parser.add_argument("--file-key")
    parser.add_argument("--node-id")
    parser.add_argument("--version")
    parser.add_argument("--snapshot-id")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    try:
        if args.command == "init":
            args.config.parent.mkdir(parents=True, exist_ok=True)
            descriptor = os.open(
                args.config, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600
            )
            with os.fdopen(descriptor, "w") as output:
                output.write(
                    '[masc]\nurl = ""\ntoken = ""\n\n[relay]\nport = 8941\npoll_interval_ms = 1000\npairing_token = '
                    + json.dumps(secrets.token_urlsafe(32))
                    + '\n\n[figma]\ntoken = ""\n\n# Optional: snapshot downloads use an operator/owner credential, not the relay agent.\n[owner]\nurl = ""\ntoken = ""\n'
                )
            print(
                f"Created private configuration: {args.config}. Set the MASC endpoint and relay credential locally."
            )
            return
        config = read_config(args.config)
        if args.command == "pairing-token":
            print(config["relay"]["pairing_token"])
            return
        if args.command == "snapshot":
            if not args.snapshot_id or args.output is None:
                raise ValueError("snapshot needs --snapshot-id and --output")
            # Use the Keeper/operator credential separately from the relay identity.
            client = McpClient(config["owner"]["url"], config["owner"]["token"])
            client.initialize()
            result = client.call_result(
                "masc_figma_snapshot", {"snapshot_id": args.snapshot_id}
            )
            from preview import png_from_result

            png_from_result(result)
            with args.output.open("xb") as output:
                output.write(encode(result))
            print(f"Saved snapshot {args.snapshot_id} to {args.output}")
            return
        if args.command == "rest-capture":
            if not args.file_key or not args.node_id or args.output is None:
                raise ValueError(
                    "rest-capture needs --file-key, --node-id and --output"
                )
            capture = FigmaRest(config["figma"]["token"], 16777216).capture(
                args.file_key, args.node_id, args.version
            )
            image = capture.pop("preview")
            data = base64.b64decode(image["data_base64"], validate=True)
            capture["preview"] = {
                "mime_type": "image/png",
                "sha256": hashlib.sha256(data).hexdigest(),
            }
            result = {
                "structuredContent": capture,
                "content": [
                    {
                        "type": "image",
                        "mimeType": "image/png",
                        "data": image["data_base64"],
                    }
                ],
            }
            with args.output.open("xb") as output:
                output.write(encode(result))
            print(f"Saved version {capture['version']} to {args.output}")
            return
        port, interval = config["relay"]["port"], config["relay"]["poll_interval_ms"]
        if (
            type(port) is not int
            or type(interval) is not int
            or port != 8941
            or interval <= 0
        ):
            raise ValueError(
                "The development Plugin requires port 8941 and a positive integer polling interval"
            )
        client = McpClient(config["masc"]["url"], config["masc"]["token"])
        client.initialize()
        bridge = Bridge(client, interval)
        info = client.call("masc_figma_relay", {"kind": "status"})
        bridge.max_bytes = info["max_request_bytes"]
        rest = FigmaRest(config.get("figma", {}).get("token", ""), bridge.max_bytes)
        rest_thread = threading.Thread(
            target=bridge.run_rest, args=(rest, info["worker_boot_id"]), daemon=True
        )
        rest_thread.start()
        try:
            with Server(port, bridge, config["relay"]["pairing_token"]) as server:
                print(
                    f"Studio relay listening on http://127.0.0.1:{server.server_port}; Plugin pairing is ready.",
                    flush=True,
                )
                server.serve_forever()
        finally:
            bridge.stopping.set()
    except (ValueError, KeyError, OSError, TransportError) as error:
        print(f"Studio relay: {error}", file=sys.stderr)
        raise SystemExit(1) from error


if __name__ == "__main__":
    main()
