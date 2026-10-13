#!/usr/bin/env python3
"""Independent Figma Studio service: authenticated HTTP API and native Plugin bridge."""

from __future__ import annotations

import argparse
import base64
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import secrets
import stat
import threading
import tomllib
from typing import cast

from client import FigmaRest, TransportError, request_timeout
from state import Refused, Store, encode, fields, parse_wire_float, text
from verify import validate

PROTOCOL = "figma-studio/1"
CAPABILITIES = ["native.capture", "native.apply", "snapshot.read", "snapshot.validate", "rest.capture"]


class Service:
    def __init__(self, directory: Path, *, max_bytes: int = 16777216, rest=None):
        self.store = Store(directory)
        self.lock = threading.RLock()
        self.max_bytes = max_bytes
        self.rest = rest

    def check(self, value):
        if len(encode(value).encode()) > self.max_bytes:
            raise Refused("Response exceeds the configured HTTP envelope")

    def snapshot(self, identity):
        metadata, image = self.store.snapshot(identity)
        return {"snapshot_id": identity, **metadata, "preview": {
            **metadata["preview"], "data_base64": base64.b64encode(image).decode()
        } if image is not None else None}

    def handle(self, path, data, *, plugin=False):
        with self.lock:
            return self._handle(path, data, plugin=plugin)

    def _handle(self, path, data, *, plugin):
        if path == "/v1/info":
            fields(data, set())
            return {"protocol": PROTOCOL, "service_id": self.store.boot_id,
                    "capabilities": CAPABILITIES, "max_request_bytes": self.max_bytes}
        if plugin:
            action = {"/v1/plugin/session": "session", "/v1/plugin/poll": "poll",
                      "/v1/plugin/result": "result"}.get(path)
            if action is None:
                raise Refused("Plugin credential cannot call client operations")
            args = dict(data)
            if action == "session":
                fields(args, {"protocol", "session_id", "document"})
                if args.pop("protocol") != PROTOCOL:
                    raise Refused("Unsupported Plugin protocol; expected " + PROTOCOL)
                args["worker_boot_id"] = self.store.boot_id
            else:
                args["worker_boot_id"] = args.pop("service_id", None)
            result = self.store.relay(
                {"kind": action, **args}, check_poll=self.check,
                check_snapshot=lambda identity, metadata, image: self.check(
                    {"snapshot_id": identity, **metadata,
                     "preview": base64.b64encode(image).decode() if image else None}),
                check_session=lambda identity, document: self.check(document),
            )
            if action == "session":
                return {"protocol": PROTOCOL, "service_id": self.store.boot_id,
                        "max_request_bytes": self.max_bytes, "poll_interval_ms": 1000}
            if action == "result":
                return {"operation_id": result["operation_id"], "state": result["state"]}
            return result
        if path == "/v1/status":
            fields(data, set())
            value = self.store.status()
            value["service_id"] = value.pop("worker_boot_id")
            return value
        if path == "/v1/operations":
            args = dict(data)
            fields(args, {"service_id", "operation_id", "session_id", "kind", "root_id"},
                   {"expected_tree_json", "ops"})
            if args.pop("service_id") != self.store.boot_id:
                raise Refused("Service replaced; inspect its current state before submitting")
            return self.operation(self.store.submit(args))
        if path == "/v1/operation":
            fields(data, {"operation_id"})
            return self.operation(self.store.operation(data["operation_id"]))
        if path == "/v1/snapshot":
            fields(data, {"snapshot_id"})
            return self.snapshot(data["snapshot_id"])
        if path == "/v1/validate":
            fields(data, {"snapshot_id", "assertions"})
            snapshot, _ = self.store.snapshot(data["snapshot_id"])
            return validate(data["snapshot_id"], snapshot, data["assertions"])
        if path == "/v1/rest-capture":
            fields(data, {"file_key", "root_id"}, {"version"})
            if self.rest is None:
                raise Refused("Figma REST credential is not configured")
            capture = self.rest.capture(data["file_key"], data["root_id"], data.get("version"))
            with self.store.db:
                identity = self.store.save_snapshot(capture, None, data["root_id"],
                    check=lambda identity, metadata, image: self.check(capture))
            return self.snapshot(identity)
        raise Refused("Unknown API operation")

    @staticmethod
    def operation(value):
        return {key: item for key, item in value.items() if key != "request"}


class Server(ThreadingHTTPServer):
    daemon_threads = True

    def __init__(self, port, service, *, client_token, plugin_token):
        if not client_token or not plugin_token or client_token == plugin_token:
            raise ValueError("Use distinct nonempty client and Plugin credentials")
        super().__init__(("127.0.0.1", port), Handler)
        self.service = service
        self.client_token, self.plugin_token = client_token, plugin_token


class Handler(BaseHTTPRequestHandler):
    @property
    def studio(self):
        return cast(Server, self.server)

    def log_message(self, format, *args):
        pass

    def origin_allowed(self):
        return self.headers.get("Origin") in (None, "null", "https://www.figma.com", "https://figma.com")

    def reply(self, status, body):
        wire = encode(body).encode()
        self.send_response(status)
        origin = self.headers.get("Origin")
        if origin and self.origin_allowed():
            self.send_header("Access-Control-Allow-Origin", origin)
            self.send_header("Vary", "Origin")
        self.send_header("Access-Control-Allow-Headers", "Authorization, Content-Type")
        self.send_header("Access-Control-Allow-Methods", "POST, OPTIONS")
        self.send_header("Access-Control-Allow-Private-Network", "true")
        self.send_header("Content-Type", "application/json")
        self.send_header("Cache-Control", "no-store")
        self.send_header("Content-Length", str(len(wire)))
        self.end_headers()
        self.wfile.write(wire)

    def do_OPTIONS(self):
        self.reply(200 if self.origin_allowed() else 403, {})

    def do_POST(self):
        if self.headers.get("Host") != f"127.0.0.1:{self.studio.server_port}" or not self.origin_allowed():
            self.reply(403, {"error": "Unexpected host or origin"})
            return
        token = self.headers.get("Authorization", "")
        plugin = secrets.compare_digest(token, "Bearer " + self.studio.plugin_token)
        if not plugin and not secrets.compare_digest(token, "Bearer " + self.studio.client_token):
            self.reply(401, {"error": "Invalid credential"})
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            if length <= 0 or length > self.studio.service.max_bytes:
                self.close_connection = True
                self.reply(413, {"kind": "request_too_large", "error": "HTTP envelope exceeded"})
                return
            data = json.loads(self.rfile.read(length), parse_float=parse_wire_float)
            value = self.studio.service.handle(self.path, data, plugin=plugin)
            self.studio.service.check(value)
            self.reply(200, value)
        except (ValueError, KeyError, TypeError) as error:
            self.reply(409, {"kind": "application_refused", "error": str(error), "retryable": False})
        except TransportError as error:
            self.reply(502, {"kind": "transport_error", "error": str(error), "retryable": False})


def read_config(path):
    if not stat.S_ISREG(path.stat().st_mode) or stat.S_IMODE(path.stat().st_mode) & 0o077:
        raise ValueError("Service configuration must be a private regular file (chmod 600)")
    return tomllib.loads(path.read_text())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["init", "serve", "plugin-token"])
    parser.add_argument("--config", required=True, type=Path)
    args = parser.parse_args()
    try:
        if args.command == "init":
            args.config.parent.mkdir(parents=True, exist_ok=True)
            with os.fdopen(os.open(args.config, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600), "w") as f:
                f.write('[service]\nport = 8941\nstate_directory = "./studio-state"\nmax_bytes = 16777216\n')
                for key in ("client_token", "plugin_token"):
                    f.write(key + " = " + json.dumps(secrets.token_urlsafe(32)) + "\n")
                f.write('\n[figma]\ntoken = ""\nrequest_timeout_seconds = 30\n')
            print(f"Created private service configuration: {args.config}")
            return
        config = read_config(args.config)
        service = config["service"]
        if args.command == "plugin-token":
            print(service["plugin_token"])
            return
        port, maximum = service["port"], service["max_bytes"]
        if type(port) is not int or not 0 <= port <= 65535 or type(maximum) is not int or maximum <= 0:
            raise ValueError("Invalid service port or HTTP envelope size")
        state = Path(text(service["state_directory"]))
        if not state.is_absolute():
            state = args.config.resolve().parent / state
        figma = config.get("figma", {})
        rest = FigmaRest(figma["token"], maximum,
            request_timeout_seconds=request_timeout(figma.get("request_timeout_seconds", 30))) if figma.get("token") else None
        app = Service(state, max_bytes=maximum, rest=rest)
        with Server(port, app, client_token=service["client_token"], plugin_token=service["plugin_token"]) as server:
            print(f"Figma Studio {PROTOCOL} listening on http://127.0.0.1:{server.server_port}", flush=True)
            server.serve_forever()
    except (ValueError, KeyError, OSError) as error:
        print(f"Figma Studio: {error}", file=__import__("sys").stderr)
        raise SystemExit(1) from error


if __name__ == "__main__":
    main()
