#!/usr/bin/env python3
"""Generate reference SVGs from the official D2 CLI.

For each .d2 fixture file in tests/d2_conformance/fixtures/, runs the D2
CLI to produce a reference SVG in tests/d2_conformance/reference/.

Usage:
    python generate_reference.py [--layout dagre|elk]

Requires the `d2` CLI to be installed and on PATH.
"""

import argparse
import shutil
import subprocess
import sys
from pathlib import Path

# Project root: walk up from this script to find the repo root
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent  # tools/d2_conformance -> tools -> project root

FIXTURES_DIR = PROJECT_ROOT / "tests" / "d2_conformance" / "fixtures"
REFERENCE_DIR = PROJECT_ROOT / "tests" / "d2_conformance" / "reference"


def main() -> int:
    parser = argparse.ArgumentParser(description="Generate D2 reference SVGs")
    parser.add_argument(
        "--layout",
        choices=["dagre", "elk"],
        default="dagre",
        help="Layout engine to use (default: dagre)",
    )
    args = parser.parse_args()

    # Check d2 CLI availability
    d2_path = shutil.which("d2")
    if d2_path is None:
        print("ERROR: d2 CLI not found on PATH.", file=sys.stderr)
        print("Install from https://d2lang.com or add it to your PATH.", file=sys.stderr)
        return 1

    print(f"Using d2 at: {d2_path}")
    print(f"Layout engine: {args.layout}")

    # Ensure directories exist
    if not FIXTURES_DIR.exists():
        print(f"ERROR: Fixtures directory not found: {FIXTURES_DIR}", file=sys.stderr)
        return 1

    REFERENCE_DIR.mkdir(parents=True, exist_ok=True)

    # Find all .d2 fixture files
    fixtures = sorted(FIXTURES_DIR.glob("*.d2"))
    if not fixtures:
        print("WARNING: No .d2 files found in fixtures directory.")
        return 0

    print(f"Found {len(fixtures)} fixture(s)")
    print()

    failures = 0
    for fixture in fixtures:
        stem = fixture.stem
        output = REFERENCE_DIR / f"{stem}.svg"
        print(f"  {stem}.d2 -> reference/{stem}.svg ... ", end="", flush=True)

        try:
            cmd = ["d2", "--layout", args.layout, str(fixture), str(output)]
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=30,
            )
            if result.returncode == 0:
                print("OK")
            else:
                print("FAILED")
                print(f"    stderr: {result.stderr.strip()}")
                failures += 1
        except subprocess.TimeoutExpired:
            print("TIMEOUT")
            failures += 1
        except Exception as e:
            print(f"ERROR: {e}")
            failures += 1

    print()
    print(f"Done: {len(fixtures) - failures}/{len(fixtures)} succeeded")
    return 1 if failures > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
