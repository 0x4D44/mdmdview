//! Edge path computation and curve generation.
//!
//! Routes edges between shapes after all node positions are finalized.
//! Produces cubic Bézier control points for smooth SVG path rendering.
//! Handles shape-aware clipping, self-loops, and parallel edges.

use crate::geo::Point;
use crate::graph::D2Graph;
use crate::shapes::clip_to_shape;

/// Route all edges in the graph.
pub fn route_all_edges(graph: &mut D2Graph) {
    let edge_indices: Vec<petgraph::stable_graph::EdgeIndex> = graph.edges.clone();

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

        // Clip endpoints to shape boundaries
        let start = clip_to_shape(src_shape, &src_rect, src_center, dst_center);
        let end = clip_to_shape(dst_shape, &dst_rect, dst_center, src_center);

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

/// Generate a self-loop path.
fn self_loop_route(rect: &crate::geo::Rect) -> Vec<Point> {
    let cx = rect.x + rect.width / 2.0;
    let cy = rect.y;
    let right = rect.x + rect.width;
    let top = rect.y;

    let loop_size = rect.width.max(rect.height) * 0.4;

    // Exit from top-right, arc outward, re-enter from right-top
    let start = Point::new(cx + rect.width * 0.15, top);
    let ctrl1 = Point::new(cx + rect.width * 0.15, top - loop_size);
    let ctrl2 = Point::new(right + loop_size * 0.5, top - loop_size);
    let mid = Point::new(right + loop_size * 0.5, cy);
    let ctrl3 = Point::new(right + loop_size * 0.5, cy + loop_size * 0.3);
    let ctrl4 = Point::new(right, cy + rect.height * 0.15);
    let end = Point::new(right, cy + rect.height * 0.15);

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
