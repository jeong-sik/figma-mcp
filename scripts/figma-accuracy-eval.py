#!/usr/bin/env python3
import argparse
import json
import os
import sys
import time
import urllib.request


def now_iso():
    return time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())


def post_json(url, payload, timeout=60):
    data = json.dumps(payload).encode("utf-8")
    req = urllib.request.Request(
        url,
        data=data,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return json.load(resp)


def call_tool(url, name, args):
    payload = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/call",
        "params": {"name": name, "arguments": args},
    }
    response = post_json(url, payload)
    if "error" in response:
        raise RuntimeError(response["error"].get("message", "Unknown MCP error"))
    result = response.get("result", {})
    text = None
    if isinstance(result, dict):
        content = result.get("content")
        if isinstance(content, list) and content:
            text = content[0].get("text")
    parsed = None
    if isinstance(text, str):
        try:
            parsed = json.loads(text)
        except json.JSONDecodeError:
            parsed = None
    return {"raw": result, "text": text, "parsed": parsed}


def count_text_nodes_dsl(dsl):
    if isinstance(dsl, dict):
        meta = dsl.get("meta", {})
        node_type = None
        if isinstance(meta, dict):
            node_type = meta.get("type")
        self_count = 1 if node_type == "TEXT" else 0
        children = dsl.get("children", [])
        if isinstance(children, list):
            return self_count + sum(count_text_nodes_dsl(c) for c in children)
        return self_count
    if isinstance(dsl, list):
        return sum(count_text_nodes_dsl(c) for c in dsl)
    return 0


def count_text_nodes_with_segments(payload):
    if isinstance(payload, dict):
        text = payload.get("text")
        self_count = 0
        if isinstance(text, dict) and isinstance(text.get("segments"), list):
            self_count = 1
        children = payload.get("children", [])
        if isinstance(children, list):
            return self_count + sum(count_text_nodes_with_segments(c) for c in children)
        return self_count
    if isinstance(payload, list):
        return sum(count_text_nodes_with_segments(c) for c in payload)
    return 0


def image_fill_coverage(bundle):
    dsl = bundle.get("dsl_json")
    image_refs = []
    if isinstance(dsl, dict):
        assets = dsl.get("assets", {})
        if isinstance(assets, dict):
            refs = assets.get("image_refs")
            if isinstance(refs, list):
                image_refs = [r for r in refs if isinstance(r, str)]
    fills = bundle.get("image_fills", {})
    images = {}
    if isinstance(fills, dict):
        images = fills.get("images", {}) if isinstance(fills.get("images"), dict) else {}
    total = len(image_refs)
    present = sum(1 for ref in image_refs if isinstance(images.get(ref), str))
    return {"total": total, "present": present, "missing": max(0, total - present)}


def variables_resolved_ratio(bundle):
    variables = bundle.get("variables", {})
    if isinstance(variables, dict) and "error" in variables:
        return {"total": 1, "resolved": 0, "error": variables.get("error")}
    raw = variables.get("variables", {}) if isinstance(variables, dict) else {}
    resolved = variables.get("resolved", {}) if isinstance(variables, dict) else {}
    total = len(raw) if isinstance(raw, dict) else 0
    present = len(resolved) if isinstance(resolved, dict) else 0
    return {"total": total, "resolved": present, "missing": max(0, total - present)}


def text_segment_coverage(bundle):
    dsl = bundle.get("dsl_json")
    total_text_nodes = count_text_nodes_dsl(dsl)
    plugin = bundle.get("plugin_snapshot", {})
    payload = plugin.get("payload") if isinstance(plugin, dict) else None
    present = count_text_nodes_with_segments(payload)
    return {
        "total_text_nodes": total_text_nodes,
        "nodes_with_segments": present,
        "missing": max(0, total_text_nodes - present),
        "plugin_ok": bool(plugin.get("ok")) if isinstance(plugin, dict) else False,
    }


def summarize_fidelity(result):
    if not isinstance(result, dict):
        return {}
    summary = {
        "target_score": result.get("target_score"),
        "best_score": result.get("best_score"),
        "achieved": result.get("achieved"),
        "attempts": len(result.get("attempts", [])) if isinstance(result.get("attempts"), list) else None,
    }
    best = result.get("best", {})
    if isinstance(best, dict):
        fidelity = best.get("fidelity", {})
        if isinstance(fidelity, dict):
            summary["fidelity"] = fidelity
    return summary


def summarize_bundle(result):
    if not isinstance(result, dict):
        return {}
    fidelity = result.get("fidelity", {})
    summary = {"fidelity": fidelity if isinstance(fidelity, dict) else None}
    summary["image_fill_coverage"] = image_fill_coverage(result)
    summary["variables_resolved"] = variables_resolved_ratio(result)
    summary["text_segments"] = text_segment_coverage(result)
    return summary


def main():
    parser = argparse.ArgumentParser(description="Figma MCP accuracy evaluation (LLM-free loop)")
    parser.add_argument("--mcp-url", default=os.getenv("FIGMA_MCP_URL", "http://localhost:8940/mcp"))
    parser.add_argument("--file-key", required=True)
    parser.add_argument("--node-id", required=True)
    parser.add_argument("--token", required=True)
    parser.add_argument("--compare-node-id", default=None)
    parser.add_argument("--target-score", type=float, default=0.95)
    parser.add_argument("--start-depth", type=int, default=4)
    parser.add_argument("--depth-step", type=int, default=4)
    parser.add_argument("--max-depth", type=int, default=20)
    parser.add_argument("--max-attempts", type=int, default=4)
    parser.add_argument("--geometry", default="paths")
    parser.add_argument("--include-plugin", action="store_true")
    parser.add_argument("--plugin-channel-id", default=None)
    parser.add_argument("--plugin-depth", type=int, default=6)
    parser.add_argument("--plugin-timeout-ms", type=int, default=20000)
    parser.add_argument("--download", action="store_true")
    parser.add_argument("--save-dir", default=os.getenv("FIGMA_MCP_ASSET_DIR", ""))
    parser.add_argument("--image-format", default="png")
    parser.add_argument("--scale", type=float, default=1.0)
    parser.add_argument("--use-absolute-bounds", action="store_true")
    parser.add_argument("--target-ssim", type=float, default=None)
    parser.add_argument("--start-scale", type=int, default=1)
    parser.add_argument("--max-scale", type=int, default=None)
    parser.add_argument("--scale-step", type=int, default=1)
    parser.add_argument("--full", action="store_true", help="Include full tool payloads in report")
    parser.add_argument("--out", default=None)
    args = parser.parse_args()

    save_dir = args.save_dir or os.path.join(os.path.expanduser("~"), "me", "download", "figma-assets")
    out_path = args.out
    if not out_path:
        safe_node = args.node_id.replace(":", "_")
        out_path = os.path.join(
            os.path.expanduser("~"),
            "me",
            "logs",
            f"figma-accuracy-{safe_node}.json",
        )

    fidelity_args = {
        "file_key": args.file_key,
        "node_id": args.node_id,
        "token": args.token,
        "target_score": args.target_score,
        "start_depth": args.start_depth,
        "depth_step": args.depth_step,
        "max_depth": args.max_depth,
        "max_attempts": args.max_attempts,
        "geometry": args.geometry,
        "include_variables": True,
        "include_image_fills": True,
        "include_plugin": bool(args.include_plugin),
        "plugin_depth": args.plugin_depth,
        "plugin_timeout_ms": args.plugin_timeout_ms,
    }
    if args.include_plugin and args.plugin_channel_id:
        fidelity_args["plugin_channel_id"] = args.plugin_channel_id

    bundle_args = {
        "file_key": args.file_key,
        "node_id": args.node_id,
        "token": args.token,
        "format": "fidelity",
        "image_format": args.image_format,
        "scale": args.scale,
        "download": bool(args.download),
        "save_dir": save_dir,
        "include_raw": True,
        "include_meta": True,
        "include_variables": True,
        "include_image_fills": True,
        "include_plugin": bool(args.include_plugin),
        "plugin_timeout_ms": args.plugin_timeout_ms,
        "geometry": args.geometry,
    }
    if args.include_plugin and args.plugin_channel_id:
        bundle_args["plugin_channel_id"] = args.plugin_channel_id
    if args.use_absolute_bounds:
        bundle_args["use_absolute_bounds"] = True

    fidelity_resp = call_tool(args.mcp_url, "figma_fidelity_loop", fidelity_args)
    bundle_resp = call_tool(args.mcp_url, "figma_get_node_bundle", bundle_args)

    image_similarity_resp = None
    if args.compare_node_id:
        max_scale = args.max_scale if args.max_scale is not None else args.start_scale
        image_args = {
            "file_key": args.file_key,
            "node_a_id": args.node_id,
            "node_b_id": args.compare_node_id,
            "token": args.token,
            "format": args.image_format,
            "start_scale": args.start_scale,
            "max_scale": max_scale,
            "scale_step": args.scale_step,
        }
        if args.use_absolute_bounds:
            image_args["use_absolute_bounds"] = True
        if args.target_ssim is not None:
            image_args["target_ssim"] = args.target_ssim
        image_similarity_resp = call_tool(args.mcp_url, "figma_image_similarity", image_args)

    report = {
        "generated_at": now_iso(),
        "mcp_url": args.mcp_url,
        "inputs": {
            "file_key": args.file_key,
            "node_id": args.node_id,
            "compare_node_id": args.compare_node_id,
            "target_score": args.target_score,
            "geometry": args.geometry,
            "include_plugin": bool(args.include_plugin),
        },
        "summary": {
            "fidelity": summarize_fidelity(fidelity_resp.get("parsed")),
            "bundle": summarize_bundle(bundle_resp.get("parsed")),
            "image_similarity": image_similarity_resp.get("parsed") if image_similarity_resp else None,
        },
    }

    if args.full:
        report["fidelity_loop"] = fidelity_resp
        report["bundle"] = bundle_resp
        if image_similarity_resp:
            report["image_similarity"] = image_similarity_resp

    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2, ensure_ascii=True)

    print(out_path)


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        sys.stderr.write(f"Error: {exc}\n")
        sys.exit(1)
