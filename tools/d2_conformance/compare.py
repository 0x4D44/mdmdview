#!/usr/bin/env python3
"""Compare actual D2 SVGs against reference, checking structural invariants.

Reads SVGs from tests/d2_conformance/reference/ and tests/d2_conformance/actual/,
parses both with d2_svg_parse, and checks 4 structural invariants on the
actual SVGs:

1. Labels must not overlap nodes (the primary bug we're tracking)
2. Edges must not pass through non-endpoint nodes
3. Nodes must not overlap other sibling nodes
4. Labels should be near their edge route

Also compares layout properties against the reference output:
- Relative node ordering (top-to-bottom, left-to-right)
- Edge routing style (number of waypoints)
- Label placement

Outputs per-fixture JSON to tests/d2_conformance/analysis/.

Usage:
    python compare.py [--fixture NAME]  # run for one fixture or all
"""

import argparse
import json
import sys
from pathlib import Path

# Allow importing from same directory
sys.path.insert(0, str(Path(__file__).resolve().parent))

from d2_svg_parse import (
    BBox,
    EdgeInfo,
    NodeInfo,
    ParsedSVG,
    Point,
    TextInfo,
    parse_svg_file,
    point_to_polyline_distance,
    segment_intersects_rect,
)

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

REFERENCE_DIR = PROJECT_ROOT / "tests" / "d2_conformance" / "reference"
ACTUAL_DIR = PROJECT_ROOT / "tests" / "d2_conformance" / "actual"
ANALYSIS_DIR = PROJECT_ROOT / "tests" / "d2_conformance" / "analysis"

# Maximum distance (px) an edge label may be from its edge route
LABEL_EDGE_MAX_DISTANCE = 50.0

# Margin (px) when checking edge-through-node intersection
EDGE_NODE_MARGIN = -2.0  # negative = slight inset to avoid false positives at boundaries


def check_labels_not_overlapping_nodes(
    parsed: ParsedSVG,
) -> list[dict]:
    """Invariant 1: Edge labels must not overlap node bounding boxes.

    For each edge label, check its estimated bbox doesn't intersect any node bbox.
    """
    violations = []
    for label in parsed.edge_labels:
        if label.estimated_bbox is None:
            continue
        for node in parsed.nodes:
            if label.estimated_bbox.intersects(node.bbox):
                violations.append({
                    "label": label.content,
                    "label_pos": {"x": label.x, "y": label.y},
                    "node_label": node.label,
                    "node_bbox": _bbox_dict(node.bbox),
                })
    return violations


def check_edges_not_through_nodes(
    parsed: ParsedSVG,
) -> list[dict]:
    """Invariant 2: Edge routes must not pass through non-endpoint nodes.

    For each edge, check that no segment intersects a node bbox that isn't
    the source or destination of that edge.
    """
    violations = []
    for edge_idx, edge in enumerate(parsed.edges):
        if len(edge.waypoints) < 2:
            continue

        # Determine endpoint nodes: the first and last waypoints should be
        # near the source and destination nodes
        start = edge.waypoints[0]
        end = edge.waypoints[-1]

        for node in parsed.nodes:
            # Skip nodes that are likely endpoints of this edge
            expanded = node.bbox.expanded(5.0)
            if expanded.contains_point(start.x, start.y):
                continue
            if expanded.contains_point(end.x, end.y):
                continue

            # Check each segment of the edge route
            for i in range(len(edge.waypoints) - 1):
                p1 = edge.waypoints[i]
                p2 = edge.waypoints[i + 1]
                if segment_intersects_rect(p1, p2, node.bbox, margin=EDGE_NODE_MARGIN):
                    violations.append({
                        "edge_index": edge_idx,
                        "segment": [i, i + 1],
                        "node_label": node.label,
                        "node_bbox": _bbox_dict(node.bbox),
                    })
                    break  # one violation per edge-node pair is enough

    return violations


def check_nodes_not_overlapping(
    parsed: ParsedSVG,
) -> list[dict]:
    """Invariant 3: Sibling nodes must not overlap each other.

    Containers may contain children, but no two nodes at the same level
    should have intersecting bounding boxes. As a simplification, we check
    all pairs — container-child overlaps are expected and filtered out by
    checking if one bbox fully contains the other.
    """
    violations = []
    nodes = parsed.nodes

    for i in range(len(nodes)):
        for j in range(i + 1, len(nodes)):
            a = nodes[i].bbox
            b = nodes[j].bbox
            if a.intersects(b):
                # Skip if one fully contains the other (container-child)
                if _contains(a, b) or _contains(b, a):
                    continue
                violations.append({
                    "node_a": nodes[i].label or f"node_{i}",
                    "node_a_bbox": _bbox_dict(a),
                    "node_b": nodes[j].label or f"node_{j}",
                    "node_b_bbox": _bbox_dict(b),
                })

    return violations


def check_labels_near_edges(
    parsed: ParsedSVG,
) -> list[dict]:
    """Invariant 4: Edge labels should be within 50px of their edge route.

    For each edge label, find the closest edge route and check the distance.
    """
    violations = []
    for label in parsed.edge_labels:
        label_pt = Point(label.x, label.y)

        # Find minimum distance to any edge
        min_dist = float("inf")
        for edge in parsed.edges:
            d = point_to_polyline_distance(label_pt, edge.waypoints)
            min_dist = min(min_dist, d)

        if min_dist > LABEL_EDGE_MAX_DISTANCE:
            violations.append({
                "label": label.content,
                "label_pos": {"x": label.x, "y": label.y},
                "min_distance": round(min_dist, 2),
                "threshold": LABEL_EDGE_MAX_DISTANCE,
            })

    return violations


def compare_layout(
    actual: ParsedSVG,
    reference: ParsedSVG,
) -> dict:
    """Compare layout properties between actual and reference SVGs."""
    comparison = {}

    # Node count comparison
    comparison["actual_node_count"] = len(actual.nodes)
    comparison["reference_node_count"] = len(reference.nodes)

    # Edge count comparison
    comparison["actual_edge_count"] = len(actual.edges)
    comparison["reference_edge_count"] = len(reference.edges)

    # Node ordering: sort by (y, x) and compare label sequence
    actual_order = [n.label for n in sorted(actual.nodes, key=lambda n: (n.bbox.cy, n.bbox.cx))]
    ref_order = [n.label for n in sorted(reference.nodes, key=lambda n: (n.bbox.cy, n.bbox.cx))]
    comparison["actual_node_order"] = actual_order
    comparison["reference_node_order"] = ref_order
    comparison["node_order_matches"] = actual_order == ref_order

    # Edge waypoint counts
    comparison["actual_edge_waypoints"] = [len(e.waypoints) for e in actual.edges]
    comparison["reference_edge_waypoints"] = [len(e.waypoints) for e in reference.edges]

    # Label counts
    comparison["actual_edge_label_count"] = len(actual.edge_labels)
    comparison["reference_edge_label_count"] = len(reference.edge_labels)

    return comparison


def analyze_fixture(fixture_name: str) -> dict:
    """Run all checks on a single fixture, return analysis dict."""
    actual_path = ACTUAL_DIR / f"{fixture_name}.svg"
    reference_path = REFERENCE_DIR / f"{fixture_name}.svg"

    result = {
        "fixture": fixture_name,
        "actual_exists": actual_path.exists(),
        "reference_exists": reference_path.exists(),
        "invariants": {},
        "comparison": {},
    }

    if not actual_path.exists():
        result["error"] = f"Actual SVG not found: {actual_path}"
        return result

    # Parse actual SVG
    try:
        actual = parse_svg_file(actual_path)
    except Exception as e:
        result["error"] = f"Failed to parse actual SVG: {e}"
        return result

    # Run invariant checks on actual SVG
    inv1 = check_labels_not_overlapping_nodes(actual)
    inv2 = check_edges_not_through_nodes(actual)
    inv3 = check_nodes_not_overlapping(actual)
    inv4 = check_labels_near_edges(actual)

    result["invariants"] = {
        "labels_not_overlapping_nodes": {
            "pass": len(inv1) == 0,
            "violation_count": len(inv1),
            "violations": inv1,
        },
        "edges_not_through_nodes": {
            "pass": len(inv2) == 0,
            "violation_count": len(inv2),
            "violations": inv2,
        },
        "nodes_not_overlapping": {
            "pass": len(inv3) == 0,
            "violation_count": len(inv3),
            "violations": inv3,
        },
        "labels_near_edges": {
            "pass": len(inv4) == 0,
            "violation_count": len(inv4),
            "violations": inv4,
        },
    }

    # Compare with reference if available
    if reference_path.exists():
        try:
            reference = parse_svg_file(reference_path)
            result["comparison"] = compare_layout(actual, reference)
        except Exception as e:
            result["comparison"]["error"] = f"Failed to parse reference SVG: {e}"

    return result


def _bbox_dict(bbox: BBox) -> dict:
    return {
        "x": round(bbox.x, 2),
        "y": round(bbox.y, 2),
        "width": round(bbox.width, 2),
        "height": round(bbox.height, 2),
    }


def _contains(outer: BBox, inner: BBox) -> bool:
    """Check if outer fully contains inner."""
    return (
        outer.x <= inner.x
        and outer.y <= inner.y
        and outer.x2 >= inner.x2
        and outer.y2 >= inner.y2
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Compare D2 SVGs and check invariants")
    parser.add_argument(
        "--fixture",
        help="Run for a single fixture name (without extension)",
    )
    args = parser.parse_args()

    # Ensure analysis directory exists
    ANALYSIS_DIR.mkdir(parents=True, exist_ok=True)

    if args.fixture:
        fixtures = [args.fixture]
    else:
        # Find all actual SVGs (or fixtures if no actuals yet)
        actual_svgs = sorted(ACTUAL_DIR.glob("*.svg"))
        if actual_svgs:
            fixtures = [p.stem for p in actual_svgs]
        else:
            fixture_d2s = sorted((PROJECT_ROOT / "tests" / "d2_conformance" / "fixtures").glob("*.d2"))
            fixtures = [p.stem for p in fixture_d2s]

    if not fixtures:
        print("No fixtures found to analyze.")
        return 0

    print(f"Analyzing {len(fixtures)} fixture(s)")
    print()

    all_pass = True
    for name in fixtures:
        print(f"  {name} ... ", end="", flush=True)
        analysis = analyze_fixture(name)

        # Write per-fixture JSON
        output_path = ANALYSIS_DIR / f"{name}.json"
        with open(output_path, "w") as f:
            json.dump(analysis, f, indent=2)

        # Check if all invariants pass
        if "error" in analysis:
            print(f"ERROR: {analysis['error']}")
            all_pass = False
        else:
            inv = analysis["invariants"]
            passes = all(v["pass"] for v in inv.values())
            if passes:
                print("PASS")
            else:
                failed = [k for k, v in inv.items() if not v["pass"]]
                print(f"FAIL ({', '.join(failed)})")
                all_pass = False

    print()
    print(f"Results written to {ANALYSIS_DIR}")
    return 0 if all_pass else 1


if __name__ == "__main__":
    sys.exit(main())
