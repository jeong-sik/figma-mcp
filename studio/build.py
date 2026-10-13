#!/usr/bin/env python3
"""Build a self-contained Figma Studio service and Plugin distribution."""

import argparse
from pathlib import Path
import zipfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    paths = [*sorted((root / "studio").glob("*.py")), root / "studio/README.md",
             root / "studio/protocol.json", *sorted((root / "plugin/studio").glob("*"))]
    with zipfile.ZipFile(args.output, "x", compression=zipfile.ZIP_DEFLATED) as archive:
        for path in paths:
            if path.is_file():
                archive.write(path, path.relative_to(root))
    print(args.output)


if __name__ == "__main__":
    main()
