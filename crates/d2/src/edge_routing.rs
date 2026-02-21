//! Edge path computation and curve generation.
//!
//! Routes edges between shapes after all node positions are finalized.
//! Produces cubic Bézier control points for smooth SVG path rendering.
//! Handles shape-aware clipping, self-loops, and parallel edges.

use std::collections::{HashMap, HashSet};

use petgraph::stable_graph::NodeIndex;

use crate::geo::Point;
use crate::graph::{D2Graph, Direction};
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

        // Label position: geometric center of the gap between shape boundaries.
        // `start` is clipped to source shape edge, `end` to destination shape edge.
        // Only compute for labeled edges; unlabeled edges get None.
        let label_pos = if graph.graph[eidx].label.is_some() {
            Some(Point::new(
                (start.x + end.x) / 2.0,
                (start.y + end.y) / 2.0,
            ))
        } else {
            None
        };

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

// ---------------------------------------------------------------------------
// Hierarchy utilities for edge routing
// ---------------------------------------------------------------------------

/// Collect all ancestors of `node` (inclusive) up to the root.
#[allow(dead_code)] // used by orthogonal routing (Stage 2B)
fn ancestor_chain(graph: &D2Graph, node: NodeIndex) -> HashSet<NodeIndex> {
    let mut ancestors = HashSet::new();
    let mut current = node;
    ancestors.insert(current);
    while let Some(parent) = graph.graph[current].parent {
        ancestors.insert(parent);
        current = parent;
    }
    ancestors
}

/// Find the lowest common ancestor of nodes `a` and `b`.
#[allow(dead_code)] // used by orthogonal routing (Stage 2B)
fn find_lca(graph: &D2Graph, a: NodeIndex, b: NodeIndex) -> NodeIndex {
    let a_ancestors = ancestor_chain(graph, a);
    let mut current = b;
    if a_ancestors.contains(&current) {
        return current;
    }
    while let Some(parent) = graph.graph[current].parent {
        if a_ancestors.contains(&parent) {
            return parent;
        }
        current = parent;
    }
    graph.root
}

/// Walk from `node` upward until we find the direct child of `ancestor`.
/// If `node == ancestor`, returns `node` (handles container-to-descendant edges).
#[allow(dead_code)] // used by orthogonal routing (Stage 2B)
fn child_ancestor_of(graph: &D2Graph, node: NodeIndex, ancestor: NodeIndex) -> NodeIndex {
    if node == ancestor {
        return node;
    }
    let mut current = node;
    while let Some(parent) = graph.graph[current].parent {
        if parent == ancestor {
            return current;
        }
        current = parent;
    }
    // Fallback (shouldn't happen in well-formed graph)
    node
}

/// Walk from `container` upward through its parent chain and return the
/// first explicit `direction` override found. If none is set on any
/// ancestor, fall back to `graph.direction`.
#[allow(dead_code)] // used by orthogonal routing (Stage 2B)
fn effective_direction(graph: &D2Graph, container: NodeIndex) -> Direction {
    let mut current = container;
    loop {
        if let Some(dir) = graph.graph[current].direction {
            return dir;
        }
        match graph.graph[current].parent {
            Some(parent) => current = parent,
            None => return graph.direction,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::RenderOptions;

    /// Helper: parse+compile+layout a D2 source string, return the graph.
    fn layout_ok(source: &str) -> crate::graph::D2Graph {
        let ast = crate::parse(source).ast;
        let mut graph = crate::compile(&ast).expect("compile should succeed");
        crate::layout(&mut graph, &RenderOptions::default()).expect("layout should succeed");
        graph
    }

    /// Find a non-root object by label, return its NodeIndex.
    fn find_node_by_label(
        graph: &crate::graph::D2Graph,
        label: &str,
    ) -> petgraph::stable_graph::NodeIndex {
        for &idx in &graph.objects {
            if graph.graph[idx].label == label {
                return idx;
            }
        }
        panic!("no object with label '{label}' found");
    }

    /// Find the edge between two nodes identified by label.
    fn find_edge_between<'a>(
        graph: &'a crate::graph::D2Graph,
        src_label: &str,
        dst_label: &str,
    ) -> &'a crate::graph::D2EdgeData {
        let src = find_node_by_label(graph, src_label);
        let dst = find_node_by_label(graph, dst_label);
        for &eidx in &graph.edges {
            if let Some((s, d)) = graph.graph.edge_endpoints(eidx) {
                if s == src && d == dst {
                    return &graph.graph[eidx];
                }
            }
        }
        panic!("no edge from '{src_label}' to '{dst_label}' found");
    }

    // --- edge label positioning tests ----------------------------------------

    #[test]
    fn test_label_between_nodes() {
        // Vertical layout (default direction=down): label should sit between
        // a's bottom edge and b's top edge.
        let graph = layout_ok("a -> b: hello");

        let a_rect = graph.graph[find_node_by_label(&graph, "a")]
            .box_
            .expect("a should have a box");
        let b_rect = graph.graph[find_node_by_label(&graph, "b")]
            .box_
            .expect("b should have a box");

        let edge = find_edge_between(&graph, "a", "b");
        let label_pos = edge
            .label_position
            .expect("labeled edge should have a label_position");

        let a_bottom = a_rect.bottom();
        let b_top = b_rect.y;

        assert!(
            label_pos.y > a_bottom && label_pos.y < b_top,
            "label y ({}) should be between a.bottom ({}) and b.top ({})",
            label_pos.y,
            a_bottom,
            b_top,
        );
    }

    #[test]
    fn test_label_between_nodes_horizontal() {
        // Horizontal layout (direction=right): label should sit between
        // a's right edge and b's left edge.
        let graph = layout_ok("direction: right\na -> b: hello");

        let a_rect = graph.graph[find_node_by_label(&graph, "a")]
            .box_
            .expect("a should have a box");
        let b_rect = graph.graph[find_node_by_label(&graph, "b")]
            .box_
            .expect("b should have a box");

        let edge = find_edge_between(&graph, "a", "b");
        let label_pos = edge
            .label_position
            .expect("labeled edge should have a label_position");

        let a_right = a_rect.right();
        let b_left = b_rect.x;

        assert!(
            label_pos.x > a_right && label_pos.x < b_left,
            "label x ({}) should be between a.right ({}) and b.left ({})",
            label_pos.x,
            a_right,
            b_left,
        );
    }

    #[test]
    fn test_unlabeled_edge_no_label_position() {
        // Unlabeled edge should have label_position = None.
        let graph = layout_ok("a -> b");

        let edge = find_edge_between(&graph, "a", "b");
        assert!(
            edge.label_position.is_none(),
            "unlabeled edge should have label_position = None, got {:?}",
            edge.label_position,
        );
    }

    // --- existing tests ------------------------------------------------------

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

    // --- hierarchy utility tests ---------------------------------------------

    #[test]
    fn test_find_lca_root_level() {
        // x and y live in different top-level containers → LCA is root
        let graph = layout_ok("a: { x }\nb: { y }\na.x -> b.y");
        let x = find_node_by_label(&graph, "x");
        let y = find_node_by_label(&graph, "y");
        let lca = find_lca(&graph, x, y);
        assert_eq!(lca, graph.root, "LCA of nodes in separate top-level containers should be root");
    }

    #[test]
    fn test_find_lca_siblings() {
        // a and b are siblings inside g → LCA is g
        let graph = layout_ok("g: { a; b; a -> b }");
        let a = find_node_by_label(&graph, "a");
        let b = find_node_by_label(&graph, "b");
        let g = find_node_by_label(&graph, "g");
        let lca = find_lca(&graph, a, b);
        assert_eq!(lca, g, "LCA of siblings should be their parent container");
    }

    #[test]
    fn test_find_lca_container_to_descendant() {
        // g contains a → LCA of g and a should be g
        let graph = layout_ok("g: { a }");
        let g = find_node_by_label(&graph, "g");
        let a = find_node_by_label(&graph, "a");
        let lca = find_lca(&graph, g, a);
        assert_eq!(lca, g, "LCA of container and its child should be the container");
    }

    #[test]
    fn test_child_ancestor_of_direct() {
        // a is a direct child of g
        let graph = layout_ok("g: { a; b }");
        let a = find_node_by_label(&graph, "a");
        let g = find_node_by_label(&graph, "g");
        let result = child_ancestor_of(&graph, a, g);
        assert_eq!(result, a, "direct child should return itself");
    }

    #[test]
    fn test_child_ancestor_of_nested() {
        // a is nested: g → inner → a; child_ancestor_of(a, g) should be inner
        let graph = layout_ok("g: { inner: { a } }");
        let a = find_node_by_label(&graph, "a");
        let g = find_node_by_label(&graph, "g");
        let inner = find_node_by_label(&graph, "inner");
        let result = child_ancestor_of(&graph, a, g);
        assert_eq!(result, inner, "should return the immediate child of ancestor on the path to node");
    }

    #[test]
    fn test_child_ancestor_of_guard() {
        // node == ancestor → should return node itself
        let graph = layout_ok("g: { a }");
        let g = find_node_by_label(&graph, "g");
        let result = child_ancestor_of(&graph, g, g);
        assert_eq!(result, g, "when node == ancestor, should return node");
    }

    #[test]
    fn test_effective_direction_inherit() {
        // g has no direction override → inherits graph-level "right"
        let graph = layout_ok("direction: right\ng: { a }");
        let g = find_node_by_label(&graph, "g");
        let dir = effective_direction(&graph, g);
        assert_eq!(dir, Direction::Right, "should inherit graph-level direction");
    }

    #[test]
    fn test_effective_direction_override() {
        // g overrides direction to "down"
        let graph = layout_ok("direction: right\ng: { direction: down; a }");
        let g = find_node_by_label(&graph, "g");
        let dir = effective_direction(&graph, g);
        assert_eq!(dir, Direction::Down, "should use container's own direction override");
    }

    #[test]
    fn test_effective_direction_root() {
        // Asking for effective_direction of root → should return graph.direction
        let graph = layout_ok("direction: right\na");
        let dir = effective_direction(&graph, graph.root);
        assert_eq!(dir, Direction::Right, "root effective direction should be graph.direction");
    }
}
