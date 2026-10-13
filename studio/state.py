"""Durable document requests: dispatched mutations are never automatically replayed."""

from __future__ import annotations

import base64
import hashlib
import json
import math
import sqlite3
import time
import uuid
from collections.abc import Callable
from dataclasses import dataclass
from enum import StrEnum
from pathlib import Path
from typing import Any

from private_state import PrivateState

Json = dict[str, Any]


class State(StrEnum):
    PREPARED = "prepared"
    DISPATCHED = "dispatched"
    CAPTURED = "captured"
    APPLIED = "applied"
    REJECTED = "rejected"
    UNKNOWN = "outcome_unknown"


class Refused(ValueError):
    pass


@dataclass(frozen=True)
class _RawNumber:
    lexeme: str


def parse_wire_float(lexeme: str) -> float | _RawNumber:
    number = float(lexeme)
    return number if math.isfinite(number) else _RawNumber(lexeme)


def representation_issue(value: Any) -> str | None:
    pending = [value]
    while pending:
        item = pending.pop()
        if isinstance(item, _RawNumber) or (
            isinstance(item, float) and not math.isfinite(item)
        ):
            return "Number cannot be represented as a finite JSON value"
        if isinstance(item, str):
            try:
                item.encode("utf-8")
            except UnicodeError:
                return "Unpaired Unicode surrogate in JSON string"
        elif isinstance(item, dict):
            pending.extend(item.keys())
            pending.extend(item.values())
        elif isinstance(item, list):
            pending.extend(item)
    return None


def relay_readback(ok: bool, payload: Json) -> tuple[Json, str | None]:
    raw = {"ok": ok, "payload": payload}
    issue = representation_issue(raw)
    if issue is None:
        return raw, None
    numbers = []

    def project(value: Any, path: list[str | int]) -> Any:
        if isinstance(value, _RawNumber):
            numbers.append(
                {"path": path, "kind": "wire_number", "lexeme": value.lexeme}
            )
            return None
        if isinstance(value, float) and not math.isfinite(value):
            numbers.append({"path": path, "kind": "python_float", "value": repr(value)})
            return None
        if isinstance(value, dict):
            return {key: project(value[key], [*path, key]) for key in sorted(value)}
        if isinstance(value, list):
            return [project(item, [*path, index]) for index, item in enumerate(value)]
        return value

    evidence = {"value": project(raw, []), "number_lexemes": numbers}
    # Escaping keeps original surrogate code units without asking SQLite or
    # an outbound UTF-8 encoder to accept them as text. Numeric slots have a
    # separate typed path so they cannot collide with an actual null/object.
    serialized = json.dumps(
        evidence,
        ensure_ascii=True,
        allow_nan=False,
        sort_keys=True,
        separators=(",", ":"),
    )
    return {
        "ok": ok,
        "payload": {
            "representation_kind": "unrepresentable_json",
            "reported_result_json": serialized,
        },
    }, issue


def encode(value: Any) -> str:
    return json.dumps(
        value,
        ensure_ascii=False,
        allow_nan=False,
        sort_keys=True,
        separators=(",", ":"),
    )


def text(value: Any) -> str:
    if not isinstance(value, str) or not value or value != value.strip():
        raise Refused("Expected nonempty trimmed string")
    return value


def fields(value: Any, required: set[str], optional: set[str] | None = None) -> Json:
    if (
        not isinstance(value, dict)
        or not required <= value.keys()
        or value.keys() - required - (optional or set())
    ):
        raise Refused(
            f"Expected fields {sorted(required)}, optional {sorted(optional or set())}"
        )
    return value


def native_document(tree_json: Any) -> Json:
    """Parse the native tree shape used by snapshot identity and validation."""

    def reject_constant(value: str) -> Any:
        raise Refused(f"Non-finite number in native export: {value}")

    def finite_float(value: str) -> float:
        number = float(value)
        if not math.isfinite(number):
            raise Refused("Non-finite number in native export")
        return number

    try:
        tree = json.loads(
            text(tree_json), parse_constant=reject_constant, parse_float=finite_float
        )
    except Refused:
        raise
    except (ValueError, RecursionError) as error:
        raise Refused("Invalid native export JSON") from error
    if not isinstance(tree, dict) or not isinstance(tree.get("document"), dict):
        raise Refused("Native export requires a document object")
    issue = representation_issue(tree)
    if issue is not None:
        raise Refused(issue)
    document = tree["document"]
    pending = [document]
    identities = set()
    while pending:
        node = pending.pop()
        identity = text(node.get("id"))
        if identity in identities:
            raise Refused("Ambiguous duplicate node IDs in native export")
        identities.add(identity)
        children = node.get("children", [])
        if not isinstance(children, list) or any(
            not isinstance(child, dict) for child in children
        ):
            raise Refused("Native node children must be an array of objects")
        pending.extend(children)
    return document


def png_bytes(preview: Json | None) -> bytes | None:
    if preview is None:
        return None
    fields(preview, {"mime_type", "data_base64"})
    if preview["mime_type"] != "image/png":
        raise Refused("Preview must be PNG")
    encoded = text(preview["data_base64"])
    try:
        data = base64.b64decode(encoded, validate=True)
    except ValueError as error:
        raise Refused("Invalid canonical PNG encoding") from error
    if base64.b64encode(data).decode() != encoded or not data.startswith(
        b"\x89PNG\r\n\x1a\n"
    ):
        raise Refused("Invalid canonical PNG encoding")
    return data


class Store:
    def __init__(self, directory: Path):
        self.lease = PrivateState(directory)
        try:
            self.db = sqlite3.connect(self.lease.database("studio.sqlite3"), check_same_thread=False)
            self._initialize()
        except BaseException:
            if hasattr(self, "db"):
                self.db.close()
            self.lease.close()
            raise

    def close(self):
        try:
            self.db.close()
        finally:
            self.lease.close()

    def _initialize(self):
        self.db.row_factory = sqlite3.Row
        self.db.execute("PRAGMA journal_mode=WAL")
        self.db.execute("PRAGMA synchronous=FULL")
        self.db.executescript("""
          CREATE TABLE IF NOT EXISTS metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL);
          CREATE TABLE IF NOT EXISTS sessions (id TEXT PRIMARY KEY, document TEXT NOT NULL);
          CREATE TABLE IF NOT EXISTS snapshots (id TEXT PRIMARY KEY, metadata TEXT NOT NULL, png BLOB);
          CREATE TABLE IF NOT EXISTS reports (id TEXT PRIMARY KEY, content TEXT NOT NULL);
          CREATE TABLE IF NOT EXISTS operations (id TEXT PRIMARY KEY, boot TEXT NOT NULL,
            session TEXT NOT NULL, request TEXT NOT NULL, state TEXT NOT NULL, result TEXT,
            created REAL NOT NULL, updated REAL NOT NULL);
        """)
        self.boot_id = str(uuid.uuid4())
        with self.db:
            self.db.execute("DELETE FROM sessions")
            self.db.execute("DELETE FROM metadata WHERE key='active_session'")
            for old, new, reason in (
                (State.PREPARED, State.REJECTED, "Service restarted before dispatch"),
                (State.DISPATCHED, State.UNKNOWN, "Service restarted after dispatch; do not replay"),
            ):
                self.db.execute(
                    "UPDATE operations SET state=?,result=?,updated=? WHERE state=?",
                    (new, encode({"reason": reason}), time.time(), old),
                )

    def meta(self, key: str) -> str | None:
        row = self.db.execute(
            "SELECT value FROM metadata WHERE key=?", (key,)
        ).fetchone()
        return row[0] if row else None

    def set_meta(self, key: str, value: str) -> None:
        self.db.execute("INSERT OR REPLACE INTO metadata VALUES (?,?)", (key, value))

    def session(self, identity: str) -> Json:
        row = self.db.execute(
            "SELECT document FROM sessions WHERE id=?", (identity,)
        ).fetchone()
        if row is None:
            raise Refused("Plugin session has not reported to this worker")
        return json.loads(row[0])

    def operation(self, identity: str) -> Json:
        row = self.db.execute(
            "SELECT * FROM operations WHERE id=?", (text(identity),)
        ).fetchone()
        if row is None:
            raise Refused("Unknown operation")
        return {
            "operation_id": row["id"],
            "worker_boot_id": row["boot"],
            "session_id": row["session"],
            "state": row["state"],
            "request": json.loads(row["request"]),
            "result": json.loads(row["result"]) if row["result"] else None,
        }

    def snapshot(self, identity: str) -> tuple[Json, bytes | None]:
        row = self.db.execute(
            "SELECT metadata,png FROM snapshots WHERE id=?", (text(identity),)
        ).fetchone()
        if row is None:
            raise Refused("Unknown snapshot")
        return json.loads(row[0]), row[1]

    def submit(self, args: Json) -> Json:
        fields(
            args,
            {"operation_id", "session_id", "kind", "root_id"},
            {"expected_tree_json", "ops"},
        )
        identity, session = text(args["operation_id"]), text(args["session_id"])
        request = {key: value for key, value in args.items() if key != "operation_id"}
        prior = self.db.execute(
            "SELECT request FROM operations WHERE id=?", (identity,)
        ).fetchone()
        if prior:
            if prior[0] != encode(request):
                raise Refused("Operation ID already identifies different input")
            return self.operation(identity)
        document = self.session(session)
        text(args["root_id"])
        if args["kind"] == "apply":
            if not document["can_edit"]:
                raise Refused("Session does not report Design editing capability")
            baseline = native_document(args.get("expected_tree_json"))
            if baseline["id"] != args["root_id"]:
                raise Refused("Baseline belongs to another root")
            if not isinstance(args.get("ops"), list) or not args["ops"]:
                raise Refused("Edit needs nonempty native operations")
        elif args["kind"] != "capture" or set(args) != {
            "operation_id",
            "session_id",
            "kind",
            "root_id",
        }:
            raise Refused("Unknown or malformed operation")
        now = time.time()
        with self.db:
            self.db.execute(
                "INSERT INTO operations VALUES (?,?,?,?,?,NULL,?,?)",
                (
                    identity,
                    self.boot_id,
                    session,
                    encode(request),
                    State.PREPARED,
                    now,
                    now,
                ),
            )
        return self.operation(identity)

    def relay(
        self,
        args: Json,
        *,
        check_poll: Callable[[Json], None],
        check_snapshot: Callable[[str, Json, bytes | None], None],
        check_session: Callable[[str, Json], None],
    ) -> Json:
        fields(
            args,
            {"kind", "worker_boot_id", "session_id"},
            {"document", "operation_id", "ok", "payload"},
        )
        if args["worker_boot_id"] != self.boot_id:
            raise Refused("Worker replaced; pair again")
        session = text(args["session_id"])
        if args["kind"] == "session":
            fields(args, {"kind", "worker_boot_id", "session_id", "document"})
            doc = fields(
                args["document"],
                {"name", "page_id", "selection", "editor_type", "mode", "can_edit"},
            )
            if (
                not isinstance(doc["name"], str)
                or not isinstance(doc["selection"], list)
                or type(doc["can_edit"]) is not bool
            ):
                raise Refused("Malformed Plugin context")
            for key in ("page_id", "editor_type", "mode"):
                text(doc[key])
            check_session(session, doc)
            with self.db:
                self.db.execute(
                    "INSERT OR REPLACE INTO sessions VALUES (?,?)",
                    (session, encode(doc)),
                )
            return {"status": "reported"}
        self.session(session)
        if args["kind"] == "poll":
            fields(args, {"kind", "worker_boot_id", "session_id"})
            with self.db:
                row = self.db.execute(
                    "SELECT id FROM operations WHERE session=? AND boot=? AND state=? ORDER BY created,id LIMIT 1",
                    (session, self.boot_id, State.PREPARED),
                ).fetchone()
                if row is None:
                    return {"command": None}
                operation = self.operation(row[0])
                command = {"id": row[0], **operation["request"]}
                response = {"command": command}
                # The retained baseline can make this reply larger than the
                # submitted operation. Refuse before handing off ownership.
                try:
                    check_poll(response)
                except Refused as error:
                    rejection = {"reason": str(error), "effect": "proven_pre_effect"}
                    self.db.execute(
                        "UPDATE operations SET state=?,result=?,updated=? WHERE id=?",
                        (State.REJECTED, encode(rejection), time.time(), row[0]),
                    )
                    return {
                        "command": None,
                        "rejected_operation": {
                            "operation_id": row[0],
                            "state": State.REJECTED,
                            **rejection,
                        },
                    }
                self.db.execute(
                    "UPDATE operations SET state=?,updated=? WHERE id=?",
                    (State.DISPATCHED, time.time(), row[0]),
                )
            return response
        if args["kind"] != "result":
            raise Refused("Unknown relay action")
        fields(
            args,
            {"kind", "worker_boot_id", "session_id", "operation_id", "ok", "payload"},
        )
        operation = self.operation(args["operation_id"])
        if (
            operation["session_id"] != session
            or operation["worker_boot_id"] != self.boot_id
        ):
            raise Refused("Result belongs to another session/worker")
        if type(args["ok"]) is not bool or not isinstance(args["payload"], dict):
            raise Refused("Expected boolean ok and object payload")
        raw, issue = relay_readback(args["ok"], args["payload"])
        representation = "unrepresentable_json" if issue is not None else None
        if operation["state"] != State.DISPATCHED:
            if (
                operation["result"]
                and operation["result"].get("relay_result") == raw
                and operation["result"].get("relay_result_representation")
                == representation
            ):
                return operation
            raise Refused("Operation already has a result or was not dispatched")
        result: Json = {"relay_result": raw}
        state = (
            State.REJECTED
            if issue is None and args["payload"].get("status") == "rejected"
            else State.UNKNOWN
        )
        if issue is not None:
            result["reason"] = f"Unusable readback: {issue}"
            result["relay_result_representation"] = representation
        with self.db:
            if args["ok"] and issue is None:
                try:
                    result["snapshot_id"] = self.save_snapshot(
                        args["payload"],
                        session,
                        operation["request"]["root_id"],
                        check=check_snapshot,
                    )
                    state = (
                        State.CAPTURED
                        if operation["request"]["kind"] == "capture"
                        else State.APPLIED
                    )
                except Refused as error:
                    result["reason"] = f"Unusable readback: {error}"
                    state = State.UNKNOWN
            self.db.execute(
                "UPDATE operations SET state=?,result=?,updated=? WHERE id=?",
                (state, encode(result), time.time(), operation["operation_id"]),
            )
        return self.operation(operation["operation_id"])

    def save_snapshot(
        self,
        value: Json,
        session: str | None,
        root: str,
        *,
        check: Callable[[str, Json, bytes | None], None],
    ) -> str:
        origin = (
            {"session_id", "page_id"}
            if session is not None
            else {"file_key", "version"}
        )
        fields(
            value,
            origin
            | {
                "schema_version",
                "source",
                "root_id",
                "tree_json",
                "preview",
                "coverage",
                "observed_at",
            },
            {"completed"},
        )
        if (
            value["schema_version"] != 1
            or value["source"]
            != ("plugin_live" if session is not None else "rest_version")
            or (session is not None and value["session_id"] != session)
            or value["root_id"] != root
        ):
            raise Refused("Snapshot identity mismatch")
        document = native_document(value["tree_json"])
        if document["id"] != root:
            raise Refused("Native export root mismatch")
        coverage = fields(value["coverage"], {"stable", "missing"})
        if (
            type(coverage["stable"]) is not bool
            or not isinstance(coverage["missing"], list)
            or any(not isinstance(x, str) for x in coverage["missing"])
        ):
            raise Refused("Invalid capture coverage")
        data = png_bytes(value["preview"])
        if data is None and not any(
            x.startswith("preview:") for x in coverage["missing"]
        ):
            raise Refused("Missing preview must be explicit")
        metadata = {
            **value,
            "preview": {
                "mime_type": "image/png",
                "sha256": hashlib.sha256(data).hexdigest(),
            }
            if data
            else None,
        }
        try:
            encoded = encode(metadata)
            identity = hashlib.sha256(encoded.encode()).hexdigest()
        except (ValueError, TypeError, UnicodeError) as error:
            raise Refused("Snapshot must contain valid UTF-8 JSON values") from error
        # Validate both read and observation projections before publishing a
        # latest pointer. A rejected readback remains only in its operation.
        check(identity, metadata, data)
        self.db.execute(
            "INSERT OR IGNORE INTO snapshots VALUES (?,?,?)", (identity, encoded, data)
        )
        self.set_meta("latest_snapshot", identity)
        return identity

    def status(self, *, session: tuple[str, Json] | None = None) -> Json:
        sessions = {
            row[0]: {"session_id": row[0], **json.loads(row[1])}
            for row in self.db.execute("SELECT id,document FROM sessions ORDER BY id")
        }
        if session is not None:
            sessions[session[0]] = {"session_id": session[0], **session[1]}
        return {
            "worker_boot_id": self.boot_id,
            "latest_snapshot": self.meta("latest_snapshot"),
            "latest_validation": self.meta("latest_validation"),
            "sessions": [sessions[identity] for identity in sorted(sessions)],
        }

    def record_validation(
        self, report: Json, *, check: Callable[[str, str], None]
    ) -> Json:
        content = encode(report)
        identity = hashlib.sha256(content.encode()).hexdigest()
        check(identity, content)
        with self.db:
            self.db.execute(
                "INSERT OR IGNORE INTO reports VALUES (?,?)", (identity, content)
            )
            self.set_meta("latest_validation", identity)
        return {"report_id": identity, **report}
