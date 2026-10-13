"""Evaluate explicit structural expectations against one retained snapshot."""

from typing import Any

from state import Json, Refused, encode, fields, native_document, text


def validate(snapshot_id: str, snapshot: Json, checks: list[Json]) -> Json:
    if not isinstance(checks, list) or not checks:
        raise Refused("Provide explicit structural checks")
    pending = [native_document(snapshot["tree_json"])]
    nodes: dict[str, Json] = {}
    while pending:
        node = pending.pop()
        identity = text(node.get("id"))
        nodes[identity] = node
        pending.extend(node.get("children", []))
    results = []
    for check in checks:
        fields(check, {"node_id", "path", "equals"})
        path = check["path"]
        if (
            not isinstance(path, list)
            or not path
            or any(type(key) not in (str, int) for key in path)
        ):
            raise Refused(
                "path must contain property names or nonnegative array indices"
            )
        value: Any = nodes.get(text(check["node_id"]))
        found = value is not None
        for key in path:
            if isinstance(value, dict) and isinstance(key, str) and key in value:
                value = value[key]
            elif isinstance(value, list) and type(key) is int and 0 <= key < len(value):
                value = value[key]
            else:
                found = False
                break
        results.append(
            {
                **check,
                "found": found,
                "actual": value if found else None,
                "matches": found and encode(value) == encode(check["equals"]),
            }
        )
    verdict = "pass" if all(result["matches"] for result in results) else "fail"
    if not snapshot["coverage"]["stable"]:
        verdict = "inconclusive"
    return {
        "snapshot_id": snapshot_id,
        "kind": "structural_expectations",
        "verdict": verdict,
        "checks": results,
        "coverage": snapshot["coverage"],
        "visual_verdict": "not_evaluated",
    }
