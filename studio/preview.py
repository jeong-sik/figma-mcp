#!/usr/bin/env python3
"""Display the exact retained Studio PNG with timg's Kitty backend."""

from __future__ import annotations

import argparse
import base64
import hashlib
import json
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any


def png_from_result(value: dict[str, Any]) -> bytes:
    result = value.get("result", value)
    images = [
        entry
        for entry in result.get("content", [])
        if entry.get("type") == "image" and entry.get("mimeType") == "image/png"
    ]
    if len(images) != 1:
        raise ValueError("Expected exactly one retained PNG image in the tool result")
    data = base64.b64decode(images[0]["data"], validate=True)
    metadata = result["structuredContent"]
    digest = metadata["preview"]["sha256"]
    if (
        not data.startswith(b"\x89PNG\r\n\x1a\n")
        or hashlib.sha256(data).hexdigest() != digest
    ):
        raise ValueError("PNG does not match the retained snapshot hash")
    return data


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "snapshot", type=Path, help="Saved masc_figma_snapshot tool result JSON"
    )
    parser.add_argument("--kitty", action="store_true", help="Display using timg -pk")
    parser.add_argument(
        "--output", type=Path, help="Also save PNG; existing files are not replaced"
    )
    args = parser.parse_args()
    try:
        data = png_from_result(json.loads(args.snapshot.read_text()))
        if not args.kitty and args.output is None:
            raise ValueError("Choose --kitty or --output")
        if args.output is not None:
            with args.output.open("xb") as output:
                output.write(data)
        if args.kitty:
            executable = shutil.which("timg")
            if executable is None:
                raise ValueError("timg is required for Kitty output")
            with tempfile.TemporaryDirectory(prefix="figma-studio-") as directory:
                image = Path(directory) / "preview.png"
                image.write_bytes(data)
                subprocess.run([executable, "-pk", str(image)], check=True)
    except (ValueError, KeyError, OSError, subprocess.CalledProcessError) as error:
        print(f"Studio preview: {error}", file=sys.stderr)
        raise SystemExit(1) from error


if __name__ == "__main__":
    main()
