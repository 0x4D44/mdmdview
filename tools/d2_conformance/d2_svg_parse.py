#!/usr/bin/env python3
"""Shared SVG parsing utilities for D2 conformance testing.

Extracts structural information from D2-generated SVGs:
- Node bounding boxes from <rect> elements
- Edge routes from <path> elements (M/L/Q/C commands)
- Text positions and content from <text> elements
- Classification of labels as node-labels vs edge-labels

The parser is designed for the SVG output produced by both the official D2
CLI (dagre/elk) and our Rust renderer (crates/d2).
"""

import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

from lxml import etree

SVG_NS = "http://www.w3.org/2000/svg"
NSMAP = {"svg": SVG_NS}


@dataclass
class BBox:
    """Axis-aligned bounding box."""
    x: float
    y: float
    width: float
    height: float

    @property
    def x2(self) -> float:
        return self.x + self.width

    @property
    def y2(self) -> float:
        return self.y + self.height

    @property
    def cx(self) -> float:
        return self.x + self.width / 2.0

    @property
    def cy(self) -> float:
        return self.y + self.height / 2.0

    def intersects(self, other: "BBox") -> bool:
        """Return True if this bbox overlaps another (non-zero area)."""
        if self.x >= other.x2 or other.x >= self.x2:
            return False
        if self.y >= other.y2 or other.y >= self.y2:
            return False
        return True

    def contains_point(self, px: float, py: float) -> bool:
        return self.x <= px <= self.x2 and self.y <= py <= self.y2

    def expanded(self, margin: float) -> "BBox":
        """Return a copy expanded by `margin` on each side."""
        return BBox(
            self.x - margin,
            self.y - margin,
            self.width + 2 * margin,
            self.height + 2 * margin,
        )


@dataclass
class Point:
    x: float
    y: float


@dataclass
class NodeInfo:
    """A parsed node (rectangle shape) from the SVG."""
    bbox: BBox
    label: str = ""
    node_id: str = ""  # from <!-- comment --> if available


@dataclass
class EdgeInfo:
    """A parsed edge (path) from the SVG."""
    waypoints: list[Point] = field(default_factory=list)
    label: Optional[str] = None
    label_bbox: Optional[BBox] = None


@dataclass
class TextInfo:
    """A parsed <text> element from the SVG."""
    x: float
    y: float
    content: str
    anchor: str = "start"  # text-anchor attribute
    baseline: str = "auto"  # dominant-baseline attribute
    estimated_bbox: Optional[BBox] = None


@dataclass
class ParsedSVG:
    """Complete parsed structure of a D2-generated SVG."""
    nodes: list[NodeInfo] = field(default_factory=list)
    edges: list[EdgeInfo] = field(default_factory=list)
    texts: list[TextInfo] = field(default_factory=list)
    viewbox: Optional[tuple[float, float, float, float]] = None

    # Classified labels (populated by classify_labels)
    node_labels: list[TextInfo] = field(default_factory=list)
    edge_labels: list[TextInfo] = field(default_factory=list)


def parse_svg_file(path: Path) -> ParsedSVG:
    """Parse an SVG file and extract D2 structural information."""
    tree = etree.parse(str(path))
    return parse_svg_tree(tree)


def parse_svg_string(svg_string: str) -> ParsedSVG:
    """Parse an SVG string and extract D2 structural information."""
    tree = etree.fromstring(svg_string.encode("utf-8"))
    # fromstring returns an Element, wrap it for consistent handling
    return _parse_root(tree)


def parse_svg_tree(tree: etree._ElementTree) -> ParsedSVG:
    """Parse an lxml ElementTree."""
    return _parse_root(tree.getroot())


def _parse_root(root: etree._Element) -> ParsedSVG:
    result = ParsedSVG()

    # Parse viewBox
    vb = root.get("viewBox")
    if vb:
        parts = vb.split()
        if len(parts) == 4:
            result.viewbox = tuple(float(p) for p in parts)

    # Extract all rects
    all_rects = _extract_rects(root)

    # Extract all paths (edges)
    result.edges = _extract_edges(root)

    # Extract all text elements
    result.texts = _extract_texts(root)

    # Classify rects: background rect is the first one matching the viewBox,
    # edge-label halos are small rects with opacity, the rest are nodes/containers.
    result.nodes = _classify_rects(all_rects, result.viewbox)

    # Classify labels
    _classify_labels(result)

    return result


def _extract_rects(root: etree._Element) -> list[tuple[BBox, dict]]:
    """Extract all <rect> elements with their attributes."""
    rects = []
    # Also grab HTML comments following rects for node IDs
    for elem in root.iter(f"{{{SVG_NS}}}rect", "rect"):
        x = _float_attr(elem, "x", 0.0)
        y = _float_attr(elem, "y", 0.0)
        w = _float_attr(elem, "width", 0.0)
        h = _float_attr(elem, "height", 0.0)
        attrs = dict(elem.attrib)
        bbox = BBox(x, y, w, h)
        rects.append((bbox, attrs))
    return rects


def _classify_rects(
    rects: list[tuple[BBox, dict]],
    viewbox: Optional[tuple[float, float, float, float]],
) -> list[NodeInfo]:
    """Classify rects into background, edge-label halos, and nodes."""
    nodes = []
    for bbox, attrs in rects:
        # Skip background rect (matches viewBox closely)
        if viewbox and _is_background_rect(bbox, viewbox):
            continue
        # Skip edge-label halo rects (have opacity attribute)
        opacity = attrs.get("opacity", "")
        if opacity and float(opacity) < 1.0:
            continue
        # Everything else is a node or container
        node = NodeInfo(bbox=bbox)
        # Try to extract node_id from the fill/stroke pattern or comment
        nodes.append(node)
    return nodes


def _is_background_rect(bbox: BBox, viewbox: tuple[float, float, float, float]) -> bool:
    """Check if a rect matches the SVG viewBox (background fill)."""
    vx, vy, vw, vh = viewbox
    return (
        abs(bbox.x - vx) < 1.0
        and abs(bbox.y - vy) < 1.0
        and abs(bbox.width - vw) < 1.0
        and abs(bbox.height - vh) < 1.0
    )


def _extract_edges(root: etree._Element) -> list[EdgeInfo]:
    """Extract edge routes from <path> elements with fill='none'."""
    edges = []
    for elem in root.iter(f"{{{SVG_NS}}}path", "path"):
        fill = elem.get("fill", "")
        # Edges have fill="none"; skip shape outlines with fill colors
        if fill.lower() != "none":
            continue
        d = elem.get("d", "")
        if not d:
            continue
        waypoints = parse_path_d(d)
        if len(waypoints) >= 2:
            edges.append(EdgeInfo(waypoints=waypoints))
    return edges


def parse_path_d(d: str) -> list[Point]:
    """Parse an SVG path `d` attribute into waypoints.

    Extracts actual coordinates from M, L, Q, C commands.
    For Q (quadratic bezier), we keep start, control, and end points.
    For C (cubic bezier), we keep start, controls, and end points.
    This gives us the edge route for intersection testing.
    """
    points = []
    # Tokenize: split on commands, keeping the command letter
    tokens = re.findall(r'[MLQCmlqc]|[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?', d)

    i = 0
    cx, cy = 0.0, 0.0  # current point (for relative commands)

    while i < len(tokens):
        cmd = tokens[i]
        if cmd in ("M", "m"):
            relative = cmd == "m"
            i += 1
            while i < len(tokens) and tokens[i] not in "MLQCmlqc":
                x, y = float(tokens[i]), float(tokens[i + 1])
                if relative:
                    x += cx
                    y += cy
                points.append(Point(x, y))
                cx, cy = x, y
                i += 2
        elif cmd in ("L", "l"):
            relative = cmd == "l"
            i += 1
            while i < len(tokens) and tokens[i] not in "MLQCmlqc":
                x, y = float(tokens[i]), float(tokens[i + 1])
                if relative:
                    x += cx
                    y += cy
                points.append(Point(x, y))
                cx, cy = x, y
                i += 2
        elif cmd in ("Q", "q"):
            relative = cmd == "q"
            i += 1
            while i + 3 < len(tokens) and tokens[i] not in "MLQCmlqc":
                # control point
                qx, qy = float(tokens[i]), float(tokens[i + 1])
                # end point
                ex, ey = float(tokens[i + 2]), float(tokens[i + 3])
                if relative:
                    qx += cx; qy += cy
                    ex += cx; ey += cy
                points.append(Point(qx, qy))
                points.append(Point(ex, ey))
                cx, cy = ex, ey
                i += 4
        elif cmd in ("C", "c"):
            relative = cmd == "c"
            i += 1
            while i + 5 < len(tokens) and tokens[i] not in "MLQCmlqc":
                c1x, c1y = float(tokens[i]), float(tokens[i + 1])
                c2x, c2y = float(tokens[i + 2]), float(tokens[i + 3])
                ex, ey = float(tokens[i + 4]), float(tokens[i + 5])
                if relative:
                    c1x += cx; c1y += cy
                    c2x += cx; c2y += cy
                    ex += cx; ey += cy
                points.append(Point(c1x, c1y))
                points.append(Point(c2x, c2y))
                points.append(Point(ex, ey))
                cx, cy = ex, ey
                i += 6
        else:
            i += 1  # skip unknown

    return points


def _extract_texts(root: etree._Element) -> list[TextInfo]:
    """Extract all <text> elements with position and content."""
    texts = []
    for elem in root.iter(f"{{{SVG_NS}}}text", "text"):
        x = _float_attr(elem, "x", 0.0)
        y = _float_attr(elem, "y", 0.0)

        # Get text content (may be in tspan children)
        content = _get_text_content(elem)
        if not content.strip():
            continue

        anchor = elem.get("text-anchor", "start")
        baseline = elem.get("dominant-baseline", "auto")

        text_info = TextInfo(
            x=x, y=y,
            content=content,
            anchor=anchor,
            baseline=baseline,
        )
        text_info.estimated_bbox = estimate_text_bbox(text_info)
        texts.append(text_info)

    return texts


def _get_text_content(elem: etree._Element) -> str:
    """Get the full text content of a <text> element including tspan children."""
    parts = []
    if elem.text:
        parts.append(elem.text)
    for child in elem:
        # Handle tspan
        if child.text:
            parts.append(child.text)
        if child.tail:
            parts.append(child.tail)
    return " ".join(parts).strip()


def estimate_text_bbox(text: TextInfo, char_width: float = 8.0, line_height: float = 16.0) -> BBox:
    """Estimate a bounding box for a text element.

    Uses approximate character metrics since we don't have font rendering.
    The char_width of ~8px assumes a ~14px font (which D2 commonly uses).
    """
    width = len(text.content) * char_width
    height = line_height

    # Adjust x based on text-anchor
    if text.anchor == "middle":
        x = text.x - width / 2.0
    elif text.anchor == "end":
        x = text.x - width
    else:
        x = text.x

    # Adjust y based on dominant-baseline
    if text.baseline == "central":
        y = text.y - height / 2.0
    elif text.baseline == "auto":
        y = text.y - height  # auto baseline ~ text bottom at y
    else:
        y = text.y - height / 2.0

    return BBox(x, y, width, height)


def _classify_labels(result: ParsedSVG) -> None:
    """Classify text elements as node-labels or edge-labels.

    Node labels are text elements whose estimated bbox center falls inside
    a node rect. Everything else is classified as an edge label.
    """
    for text in result.texts:
        is_node_label = False
        for node in result.nodes:
            # Check if text center is inside the node bbox (with small margin)
            expanded = node.bbox.expanded(2.0)
            if expanded.contains_point(text.x, text.y):
                is_node_label = True
                node.label = text.content
                break
        if is_node_label:
            result.node_labels.append(text)
        else:
            result.edge_labels.append(text)


def _float_attr(elem: etree._Element, attr: str, default: float = 0.0) -> float:
    """Get a float attribute from an XML element."""
    val = elem.get(attr)
    if val is None:
        return default
    try:
        return float(val)
    except ValueError:
        return default


# ---------------------------------------------------------------------------
# Geometry utilities for intersection testing
# ---------------------------------------------------------------------------

def segment_intersects_rect(p1: Point, p2: Point, rect: BBox, margin: float = 0.0) -> bool:
    """Test if a line segment (p1->p2) intersects an axis-aligned rectangle.

    Uses the Liang-Barsky line clipping algorithm.
    Optional margin expands the rect on each side.
    """
    rx = rect.x - margin
    ry = rect.y - margin
    rx2 = rect.x2 + margin
    ry2 = rect.y2 + margin

    dx = p2.x - p1.x
    dy = p2.y - p1.y

    # Parametric clipping
    t0, t1 = 0.0, 1.0

    for p, q in [
        (-dx, p1.x - rx),   # left
        (dx, rx2 - p1.x),   # right
        (-dy, p1.y - ry),   # top
        (dy, ry2 - p1.y),   # bottom
    ]:
        if abs(p) < 1e-10:
            if q < 0:
                return False
        else:
            t = q / p
            if p < 0:
                t0 = max(t0, t)
            else:
                t1 = min(t1, t)
            if t0 > t1:
                return False

    return True


def point_to_segment_distance(pt: Point, seg_start: Point, seg_end: Point) -> float:
    """Minimum distance from a point to a line segment."""
    dx = seg_end.x - seg_start.x
    dy = seg_end.y - seg_start.y
    len_sq = dx * dx + dy * dy

    if len_sq < 1e-10:
        # Degenerate segment
        return ((pt.x - seg_start.x) ** 2 + (pt.y - seg_start.y) ** 2) ** 0.5

    t = max(0.0, min(1.0, ((pt.x - seg_start.x) * dx + (pt.y - seg_start.y) * dy) / len_sq))
    proj_x = seg_start.x + t * dx
    proj_y = seg_start.y + t * dy
    return ((pt.x - proj_x) ** 2 + (pt.y - proj_y) ** 2) ** 0.5


def point_to_polyline_distance(pt: Point, waypoints: list[Point]) -> float:
    """Minimum distance from a point to any segment of a polyline."""
    if len(waypoints) < 2:
        if waypoints:
            return ((pt.x - waypoints[0].x) ** 2 + (pt.y - waypoints[0].y) ** 2) ** 0.5
        return float("inf")

    min_dist = float("inf")
    for i in range(len(waypoints) - 1):
        d = point_to_segment_distance(pt, waypoints[i], waypoints[i + 1])
        min_dist = min(min_dist, d)
    return min_dist
