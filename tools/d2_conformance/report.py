#!/usr/bin/env python3
"""Generate a summary report from D2 conformance analysis results.

Reads JSON files from tests/d2_conformance/analysis/ (produced by compare.py)
and prints a human-readable summary table plus a markdown report.

Exits with code 1 if any invariant fails, 0 if all pass.

Usage:
    python report.py [--markdown report.md]
"""

import argparse
import json
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

ANALYSIS_DIR = PROJECT_ROOT / "tests" / "d2_conformance" / "analysis"

# Short names for the 4 invariants
INVARIANT_NAMES = [
    ("labels_not_overlapping_nodes", "Label/Node"),
    ("edges_not_through_nodes", "Edge/Node"),
    ("nodes_not_overlapping", "Node/Node"),
    ("labels_near_edges", "Label Near"),
]


def load_analyses() -> list[dict]:
    """Load all analysis JSON files, sorted by fixture name."""
    if not ANALYSIS_DIR.exists():
        return []
    files = sorted(ANALYSIS_DIR.glob("*.json"))
    analyses = []
    for f in files:
        with open(f) as fh:
            analyses.append(json.load(fh))
    return analyses


def print_summary_table(analyses: list[dict]) -> bool:
    """Print a summary table to stdout. Returns True if all pass."""
    if not analyses:
        print("No analysis results found.")
        return True

    # Column widths
    name_width = max(len(a["fixture"]) for a in analyses)
    name_width = max(name_width, 7)  # minimum "Fixture"

    # Header
    header_parts = [f"{'Fixture':<{name_width}}"]
    for _, short in INVARIANT_NAMES:
        header_parts.append(f"{short:^12}")
    header_parts.append(f"{'Overall':^9}")
    header = " | ".join(header_parts)
    separator = "-" * len(header)

    print(separator)
    print(header)
    print(separator)

    all_pass = True
    for analysis in analyses:
        parts = [f"{analysis['fixture']:<{name_width}}"]

        if "error" in analysis:
            parts.append(f"{'ERROR':^{12 * len(INVARIANT_NAMES) + 3 * (len(INVARIANT_NAMES) - 1)}}")
            parts.append(f"{'FAIL':^9}")
            all_pass = False
        else:
            inv = analysis.get("invariants", {})
            fixture_pass = True
            for key, _ in INVARIANT_NAMES:
                check = inv.get(key, {})
                if check.get("pass", False):
                    parts.append(f"{'PASS':^12}")
                else:
                    count = check.get("violation_count", "?")
                    parts.append(f"{'FAIL(' + str(count) + ')':^12}")
                    fixture_pass = False
                    all_pass = False

            parts.append(f"{'PASS' if fixture_pass else 'FAIL':^9}")

        print(" | ".join(parts))

    print(separator)
    print()

    # Summary counts
    total = len(analyses)
    passed = sum(
        1 for a in analyses
        if "error" not in a
        and all(a.get("invariants", {}).get(k, {}).get("pass", False) for k, _ in INVARIANT_NAMES)
    )
    print(f"Total: {passed}/{total} fixtures pass all invariants")

    return all_pass


def generate_markdown(analyses: list[dict]) -> str:
    """Generate a markdown report string."""
    lines = []
    lines.append("# D2 Conformance Test Report")
    lines.append("")

    # Summary table
    lines.append("## Summary")
    lines.append("")

    header = "| Fixture | " + " | ".join(short for _, short in INVARIANT_NAMES) + " | Overall |"
    align = "|" + "|".join(":---:" for _ in range(len(INVARIANT_NAMES) + 2)) + "|"
    lines.append(header)
    lines.append(align)

    for analysis in analyses:
        fixture = analysis["fixture"]
        if "error" in analysis:
            cells = ["ERROR"] * len(INVARIANT_NAMES)
            overall = "FAIL"
        else:
            inv = analysis.get("invariants", {})
            cells = []
            fixture_pass = True
            for key, _ in INVARIANT_NAMES:
                check = inv.get(key, {})
                if check.get("pass", False):
                    cells.append("PASS")
                else:
                    count = check.get("violation_count", "?")
                    cells.append(f"FAIL({count})")
                    fixture_pass = False
            overall = "PASS" if fixture_pass else "FAIL"

        row = f"| {fixture} | " + " | ".join(cells) + f" | {overall} |"
        lines.append(row)

    lines.append("")

    # Detailed violations
    lines.append("## Detailed Violations")
    lines.append("")

    any_violations = False
    for analysis in analyses:
        if "error" in analysis:
            lines.append(f"### {analysis['fixture']}")
            lines.append(f"Error: {analysis['error']}")
            lines.append("")
            any_violations = True
            continue

        inv = analysis.get("invariants", {})
        fixture_violations = []
        for key, short in INVARIANT_NAMES:
            check = inv.get(key, {})
            if not check.get("pass", True):
                fixture_violations.append((short, check))

        if fixture_violations:
            any_violations = True
            lines.append(f"### {analysis['fixture']}")
            for short, check in fixture_violations:
                lines.append(f"**{short}** ({check['violation_count']} violation(s)):")
                for v in check.get("violations", [])[:5]:  # limit to 5
                    lines.append(f"- {json.dumps(v)}")
                if check["violation_count"] > 5:
                    lines.append(f"- ... and {check['violation_count'] - 5} more")
            lines.append("")

    if not any_violations:
        lines.append("No violations found.")
        lines.append("")

    # Comparison summary
    lines.append("## Layout Comparison")
    lines.append("")
    for analysis in analyses:
        comp = analysis.get("comparison", {})
        if not comp or "error" in comp:
            continue
        fixture = analysis["fixture"]
        order_match = comp.get("node_order_matches", "N/A")
        lines.append(f"- **{fixture}**: node order {'matches' if order_match else 'differs'} reference")

    lines.append("")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description="D2 conformance test report")
    parser.add_argument(
        "--markdown",
        help="Write markdown report to this file",
    )
    args = parser.parse_args()

    analyses = load_analyses()
    if not analyses:
        print(f"No analysis results found in {ANALYSIS_DIR}")
        print("Run compare.py first to generate analysis data.")
        return 0

    all_pass = print_summary_table(analyses)

    if args.markdown:
        md = generate_markdown(analyses)
        md_path = Path(args.markdown)
        md_path.parent.mkdir(parents=True, exist_ok=True)
        md_path.write_text(md, encoding="utf-8")
        print(f"\nMarkdown report written to {md_path}")

    return 0 if all_pass else 1


if __name__ == "__main__":
    sys.exit(main())
