#!/usr/bin/env python3
"""Prepare a development Plugin using the identity assigned by Figma."""

import argparse
import json
from pathlib import Path
import shutil
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--from-manifest", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        assigned = json.loads(args.from_manifest.read_text())["id"]
        if not isinstance(assigned, str) or not assigned.strip():
            raise ValueError(
                "The Figma-generated manifest must contain its assigned id"
            )
        source = Path(__file__).resolve().parents[1] / "plugin" / "studio"
        manifest = json.loads((source / "manifest.template.json").read_text())
        manifest["id"] = assigned
        args.output.mkdir(parents=True, exist_ok=False)
        for filename in ("code.js", "ui.html"):
            shutil.copyfile(source / filename, args.output / filename)
        (args.output / "manifest.json").write_text(
            json.dumps(manifest, indent=2) + "\n"
        )
        print(
            f"Import this development Plugin in Figma: {args.output / 'manifest.json'}"
        )
    except (OSError, ValueError, KeyError, TypeError) as error:
        print(f"Studio preparation: {error}", file=sys.stderr)
        raise SystemExit(1) from error


if __name__ == "__main__":
    main()
