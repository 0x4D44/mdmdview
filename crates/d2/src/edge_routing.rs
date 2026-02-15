//! Edge path computation and curve generation.
//!
//! Routes edges between shapes after all node positions are finalized.
//! Produces cubic Bézier control points for smooth SVG path rendering.
//! Handles shape-aware clipping, self-loops, and parallel edges.

use std::collections::HashMap;

use crate::geo::Point;
use crate::graph::D2Graph;
use crate::shapes::clip_to_shape;

/// Perpendicular offset between parallel edges (pixels).
const PARALLEL_EDGE_SPACING: f64 = 12.0;

/// Route all edges in the graph.
/// Parallel edges between the same pair of nodes are offset
/// perpendicular to the edge direction to prevent overlap.
pub fn route_all_edges(graph: &mut D2Graph) {
    let edge_indices: Vec<petgraph::stable_graph::EdgeIndex> = graph.edges.clone();

    // Count parallel edges between each ordered pair of nodes.
    // Key is (min(src,dst), max(src,dst)) so both directions count together.
    let mut pair_counts: HashMap<(petgraph::stable_graph::NodeIndex, petgraph::stable_graph::NodeIndex), usize> = HashMap::new();
    let mut pair_current: HashMap<(petgraph::stable_graph::NodeIndex, petgraph::stable_graph::NodeIndex), usize> = HashMap::new();

    for &eidx in &edge_indices {
        if let Some((src, dst)) = graph.graph.edge_endpoints(eidx) {
            if src != dst {
                let key = if src < dst { (src, dst) } else { (dst, src) };
                *pair_counts.entry(key).or_default() += 1;
            }
        }
    }

    for &eidx in &edge_indices {
        let (src_idx, dst_idx) = match graph.graph.edge_endpoints(eidx) {
            Some(endpoints) => endpoints,
            None => continue,
        };

        let src_rect = match graph.graph[src_idx].box_ {
            Some(r) => r,
            None => continue,
        };
        let dst_rect = match graph.graph[dst_idx].box_ {
            Some(r) => r,
            None => continue,
        };

        let src_shape = graph.graph[src_idx].shape;
        let dst_shape = graph.graph[dst_idx].shape;

        let src_center = src_rect.center();
        let dst_center = dst_rect.center();

        // Self-loop
        if src_idx == dst_idx {
            let route = self_loop_route(&src_rect);
            let label_pos = Some(Point::new(
                src_center.x + src_rect.width * 0.6,
                src_center.y - src_rect.height * 0.6,
            ));
            graph.graph[eidx].route = route;
            graph.graph[eidx].label_position = label_pos;
            continue;
        }

        // Compute perpendicular offset for parallel edges
        let key = if src_idx < dst_idx {
            (src_idx, dst_idx)
        } else {
            (dst_idx, src_idx)
        };
        let total = *pair_counts.get(&key).unwrap_or(&1);
        let index = {
            let cur = pair_current.entry(key).or_default();
            let i = *cur;
            *cur += 1;
            i
        };

        let perp_offset = if total > 1 {
            let spread = (total - 1) as f64 * PARALLEL_EDGE_SPACING;
            -spread / 2.0 + index as f64 * PARALLEL_EDGE_SPACING
        } else {
            0.0
        };

        // Apply perpendicular offset to the center-to-center line
        let (offset_src, offset_dst) = if perp_offset.abs() > 0.01 {
            let dx = dst_center.x - src_center.x;
            let dy = dst_center.y - src_center.y;
            let len = (dx * dx + dy * dy).sqrt();
            if len > 1e-6 {
                // Perpendicular unit vector
                let px = -dy / len;
                let py = dx / len;
                let ox = px * perp_offset;
                let oy = py * perp_offset;
                (
                    Point::new(src_center.x + ox, src_center.y + oy),
                    Point::new(dst_center.x + ox, dst_center.y + oy),
                )
            } else {
                (src_center, dst_center)
            }
        } else {
            (src_center, dst_center)
        };

        // Clip endpoints to shape boundaries using the offset line
        let start = clip_to_shape(src_shape, &src_rect, offset_src, offset_dst);
        let end = clip_to_shape(dst_shape, &dst_rect, offset_dst, offset_src);

        // Generate cubic Bézier control points
        let route = generate_bezier(start, end);

        // Label position: midpoint of the path
        let mid = bezier_midpoint(&route);
        let label_pos = Some(Point::new(mid.x, mid.y - 10.0)); // Slight offset above

        graph.graph[eidx].route = route;
        graph.graph[eidx].label_position = label_pos;
    }
}

/// Generate a cubic Bézier path between two points.
/// Returns [start, ctrl1, ctrl2, end].
fn generate_bezier(start: Point, end: Point) -> Vec<Point> {
    let dx = end.x - start.x;
    let dy = end.y - start.y;

    // For mostly vertical connections, use vertical control points
    // For mostly horizontal connections, use horizontal control points
    let (ctrl1, ctrl2) = if dy.abs() > dx.abs() {
        // Vertical: offset control points along Y
        (
            Point::new(start.x, start.y + dy * 0.4),
            Point::new(end.x, end.y - dy * 0.4),
        )
    } else {
        // Horizontal: offset control points along X
        (
            Point::new(start.x + dx * 0.4, start.y),
            Point::new(end.x - dx * 0.4, end.y),
        )
    };

    vec![start, ctrl1, ctrl2, end]
}

/// Generate a self-loop path as two cubic Bézier segments.
/// Exits from the top of the shape, arcs outward to the upper-right,
/// and re-enters from the right side.
/// Returns 7 points: [start, c1, c2, mid, c3, c4, end] forming two
/// cubic Bézier segments that the SVG renderer consumes as:
///   M start C c1 c2 mid C c3 c4 end
fn self_loop_route(rect: &crate::geo::Rect) -> Vec<Point> {
    let cx = rect.x + rect.width / 2.0;
    let top = rect.y;
    let right = rect.x + rect.width;
    let loop_size = rect.width.max(rect.height) * 0.4;

    // Segment 1: exit from top, arc up and to the right
    let start = Point::new(cx + rect.width * 0.15, top);
    let ctrl1 = Point::new(cx + rect.width * 0.15, top - loop_size);
    let ctrl2 = Point::new(right + loop_size * 0.5, top - loop_size);
    let mid = Point::new(right + loop_size * 0.5, top + rect.height * 0.25);

    // Segment 2: arc down and re-enter from the right side
    let ctrl3 = Point::new(right + loop_size * 0.5, top + rect.height * 0.6);
    let ctrl4 = Point::new(right + loop_size * 0.1, top + rect.height * 0.35);
    let end = Point::new(right, top + rect.height * 0.35);

    vec![start, ctrl1, ctrl2, mid, ctrl3, ctrl4, end]
}

/// Compute the midpoint of a cubic Bézier route.
fn bezier_midpoint(route: &[Point]) -> Point {
    if route.len() >= 4 {
        // Evaluate cubic Bézier at t=0.5
        let p0 = route[0];
        let p1 = route[1];
        let p2 = route[2];
        let p3 = route[3];

        let t = 0.5;
        let mt = 1.0 - t;
        let mt2 = mt * mt;
        let mt3 = mt2 * mt;
        let t2 = t * t;
        let t3 = t2 * t;

        Point::new(
            mt3 * p0.x + 3.0 * mt2 * t * p1.x + 3.0 * mt * t2 * p2.x + t3 * p3.x,
            mt3 * p0.y + 3.0 * mt2 * t * p1.y + 3.0 * mt * t2 * p2.y + t3 * p3.y,
        )
    } else if route.len() >= 2 {
        // Simple midpoint
        let mid_idx = route.len() / 2;
        route[mid_idx]
    } else if !route.is_empty() {
        route[0]
    } else {
        Point::new(0.0, 0.0)
    }
}

/// Compute arrowhead triangle at the endpoint of an edge.
/// `tip` is the point where the edge meets the shape.
/// `from` is the previous control point (gives direction).
/// Returns three polygon vertices: (tip, left, right).
pub fn arrowhead_polygon(tip: Point, from: Point, size: f64) -> (Point, Point, Point) {
    let dx = tip.x - from.x;
    let dy = tip.y - from.y;
    let len = (dx * dx + dy * dy).sqrt();

    if len < 1e-6 {
        // Degenerate: tip ≈ from. Default to pointing downward.
        return (
            tip,
            Point::new(tip.x - size * 0.4, tip.y - size),
            Point::new(tip.x + size * 0.4, tip.y - size),
        );
    }

    let ux = dx / len;
    let uy = dy / len;
    let base_x = tip.x - ux * size;
    let base_y = tip.y - uy * size;
    let half_width = size * 0.4;

    let left = Point::new(base_x - uy * half_width, base_y + ux * half_width);
    let right = Point::new(base_x + uy * half_width, base_y - ux * half_width);

    (tip, left, right)
}

/// Compute diamond arrowhead at the endpoint of an edge.
pub fn diamond_arrowhead(tip: Point, from: Point, size: f64) -> [Point; 4] {
    let dx = tip.x - from.x;
    let dy = tip.y - from.y;
    let len = (dx * dx + dy * dy).sqrt();

    if len < 1e-6 {
        return [tip; 4];
    }

    let ux = dx / len;
    let uy = dy / len;
    let half = size * 0.5;
    let quarter = size * 0.3;

    let mid = Point::new(tip.x - ux * half, tip.y - uy * half);
    let back = Point::new(tip.x - ux * size, tip.y - uy * size);
    let left = Point::new(mid.x - uy * quarter, mid.y + ux * quarter);
    let right = Point::new(mid.x + uy * quarter, mid.y - ux * quarter);

    [tip, left, back, right]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arrowhead_polygon() {
        let tip = Point::new(100.0, 50.0);
        let from = Point::new(50.0, 50.0);
        let (t, l, r) = arrowhead_polygon(tip, from, 10.0);
        assert!((t.x - 100.0).abs() < 0.01);
        // Left and right points should be offset perpendicular to the edge direction
        assert!((l.y - r.y).abs() > 1.0); // left and right are on opposite sides
        assert!(l.x < t.x); // both behind the tip
        assert!(r.x < t.x);
    }

    #[test]
    fn test_arrowhead_degenerate() {
        let tip = Point::new(50.0, 50.0);
        let from = Point::new(50.0, 50.0);
        let (t, _l, _r) = arrowhead_polygon(tip, from, 10.0);
        assert_eq!(t.x, tip.x);
        assert_eq!(t.y, tip.y);
    }

    #[test]
    fn test_bezier_midpoint() {
        let route = vec![
            Point::new(0.0, 0.0),
            Point::new(30.0, 0.0),
            Point::new(70.0, 100.0),
            Point::new(100.0, 100.0),
        ];
        let mid = bezier_midpoint(&route);
        assert!(mid.x > 0.0 && mid.x < 100.0);
        assert!(mid.y > 0.0 && mid.y < 100.0);
    }
}
