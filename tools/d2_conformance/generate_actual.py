#!/usr/bin/env python3
"""Generate actual SVGs from our Rust D2 renderer.

Invokes `cargo test -p d2 -- d2_conformance` which runs the conformance
test harness. That harness renders each fixture and writes the SVG output
to tests/d2_conformance/actual/.

If the test harness hasn't been built yet, this script will compile it.

Usage:
    python generate_actual.py
"""

import subprocess
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

ACTUAL_DIR = PROJECT_ROOT / "tests" / "d2_conformance" / "actual"
FIXTURES_DIR = PROJECT_ROOT / "tests" / "d2_conformance" / "fixtures"


def main() -> int:
    # Ensure output directory exists
    ACTUAL_DIR.mkdir(parents=True, exist_ok=True)

    # Check that fixtures exist
    fixtures = sorted(FIXTURES_DIR.glob("*.d2"))
    if not fixtures:
        print(f"WARNING: No .d2 files found in {FIXTURES_DIR}", file=sys.stderr)
        return 0

    print(f"Found {len(fixtures)} fixture(s)")
    print(f"Output directory: {ACTUAL_DIR}")
    print()

    # Run the Rust test harness which generates the SVGs as a side effect
    print("Running: cargo test -p d2 -- d2_conformance --test-threads=1")
    print()

    try:
        result = subprocess.run(
            [
                "cargo", "test",
                "-p", "d2",
                "--", "d2_conformance",
                "--test-threads=1",
            ],
            cwd=str(PROJECT_ROOT),
            timeout=120,
        )

        if result.returncode != 0:
            print()
            print("WARNING: Some tests may have failed, but SVGs may still have been generated.")
            print("Check the actual/ directory for output.")

    except subprocess.TimeoutExpired:
        print("ERROR: Test harness timed out after 120 seconds.", file=sys.stderr)
        return 1
    except FileNotFoundError:
        print("ERROR: cargo not found. Is Rust installed?", file=sys.stderr)
        return 1

    # Report what was generated
    print()
    generated = sorted(ACTUAL_DIR.glob("*.svg"))
    print(f"Generated {len(generated)} SVG(s) in {ACTUAL_DIR}")
    for svg in generated:
        print(f"  {svg.name}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
