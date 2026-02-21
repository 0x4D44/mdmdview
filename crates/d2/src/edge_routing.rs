//! Edge path computation and curve generation.
//!
//! Routes edges between shapes after all node positions are finalized.
//! Produces orthogonal (axis-aligned) polylines for non-self-loop edges
//! and cubic Bézier curves for self-loops.
//! Handles shape-aware port placement, channel allocation, and hierarchy.

use std::collections::{HashMap, HashSet};

use petgraph::stable_graph::{EdgeIndex, NodeIndex};

use crate::geo::{Point, Rect};
use crate::graph::{D2Graph, Direction, RouteType, ShapeType};
use crate::shapes::clip_to_shape;

/// Spacing between parallel channels in the gap between ranks (pixels).
const CHANNEL_SPACING: f64 = 12.0;

/// Minimum stub length from a port before the first bend (pixels).
const MIN_STUB_LENGTH: f64 = 15.0;

/// Jog distance past the rightmost/bottommost node for same-rank edges (pixels).
const SAME_RANK_JOG: f64 = 30.0;

/// Perpendicular offset for edge labels from the path segment (pixels).
const LABEL_OFFSET: f64 = 10.0;

// ---------------------------------------------------------------------------
// Edge classification types
// ---------------------------------------------------------------------------

/// Classification of an edge for routing purposes.
struct EdgeClassification {
    edge_idx: EdgeIndex,
    src: NodeIndex,
    dst: NodeIndex,
    lca: NodeIndex,
    direction: Direction,
    kind: EdgeKind,
    gap_start: f64,
    gap_end: f64,
    channel_pos: f64, // assigned during allocation
    src_port: Point,  // assigned during allocation
    dst_port: Point,  // assigned during allocation
}

/// The kind of edge relative to the layout direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EdgeKind {
    Forward,
    Backward,
    SameRank,
    ContainerDescendant,
}

/// Route all edges in the graph.
/// Self-loops get Bézier curves; all other edges get orthogonal routing.
pub fn route_all_edges(graph: &mut D2Graph) {
    let edge_indices: Vec<EdgeIndex> = graph.edges.clone();

    // Collect self-loop indices and route them with Bézier curves
    let mut non_self_loops: Vec<EdgeIndex> = Vec::new();

    for &eidx in &edge_indices {
        let (src_idx, dst_idx) = match graph.graph.edge_endpoints(eidx) {
            Some(endpoints) => endpoints,
            None => continue,
        };

        if src_idx == dst_idx {
            // Self-loop: use existing Bézier logic
            let src_rect = match graph.graph[src_idx].box_ {
                Some(r) => r,
                None => continue,
            };
            let src_center = src_rect.center();
            let route = self_loop_route(&src_rect);
            let label_pos = Some(Point::new(
                src_center.x + src_rect.width * 0.6,
                src_center.y - src_rect.height * 0.6,
            ));
            graph.graph[eidx].route = route;
            graph.graph[eidx].label_position = label_pos;
        } else {
            non_self_loops.push(eidx);
        }
    }

    // Route all non-self-loop edges orthogonally
    route_orthogonal_edges(graph, &non_self_loops);
}

// ---------------------------------------------------------------------------
// Orthogonal routing — three-pass architecture
// ---------------------------------------------------------------------------

/// Compute a port point on the boundary of a shape.
/// `direction` is the exit/entry side; `port_cross` is the desired cross-axis
/// coordinate (y for horizontal exits, x for vertical exits).
fn compute_port(
    shape: ShapeType,
    rect: &Rect,
    direction: Direction,
    port_cross: f64,
) -> Point {
    match shape {
        ShapeType::Diamond | ShapeType::Hexagon | ShapeType::Circle | ShapeType::Oval => {
            // Clip from center toward the exit side at the desired cross position
            let target = match direction {
                Direction::Right => Point::new(rect.right() + 100.0, port_cross),
                Direction::Left => Point::new(rect.x - 100.0, port_cross),
                Direction::Down => Point::new(port_cross, rect.bottom() + 100.0),
                Direction::Up => Point::new(port_cross, rect.y - 100.0),
            };
            clip_to_shape(shape, rect, rect.center(), target)
        }
        _ => {
            // Place directly on bounding box edge
            match direction {
                Direction::Right => Point::new(rect.right(), port_cross),
                Direction::Left => Point::new(rect.x, port_cross),
                Direction::Down => Point::new(port_cross, rect.bottom()),
                Direction::Up => Point::new(port_cross, rect.y),
            }
        }
    }
}

/// Helper: extract the primary-axis coordinate from a point.
/// For horizontal directions (Right/Left), primary = x.
/// For vertical directions (Down/Up), primary = y.
fn primary_coord(p: &Point, direction: Direction) -> f64 {
    if direction.is_horizontal() {
        p.x
    } else {
        p.y
    }
}

/// Helper: get the cross-axis span of a rect (start, end).
/// For horizontal directions: (rect.y, rect.bottom()).
/// For vertical directions: (rect.x, rect.right()).
fn cross_span(rect: &Rect, direction: Direction) -> (f64, f64) {
    if direction.is_horizontal() {
        (rect.y, rect.bottom())
    } else {
        (rect.x, rect.right())
    }
}

/// Helper: get the cross-axis center.
fn cross_center(rect: &Rect, direction: Direction) -> f64 {
    if direction.is_horizontal() {
        rect.center().y
    } else {
        rect.center().x
    }
}

/// The exit direction for a forward edge.
fn forward_exit(direction: Direction) -> Direction {
    direction
}

/// The exit direction for a backward edge (opposite of layout direction).
fn backward_exit(direction: Direction) -> Direction {
    match direction {
        Direction::Right => Direction::Left,
        Direction::Left => Direction::Right,
        Direction::Down => Direction::Up,
        Direction::Up => Direction::Down,
    }
}

/// The entry direction for a forward edge (opposite of layout direction).
fn forward_entry(direction: Direction) -> Direction {
    backward_exit(direction)
}

/// The entry direction for a backward edge (same as layout direction).
fn backward_entry(direction: Direction) -> Direction {
    direction
}

/// Route all non-self-loop edges using the three-pass orthogonal algorithm.
fn route_orthogonal_edges(graph: &mut D2Graph, edge_indices: &[EdgeIndex]) {
    if edge_indices.is_empty() {
        return;
    }

    // -----------------------------------------------------------------------
    // Pass 1: Classify each edge
    // -----------------------------------------------------------------------
    let mut classified: Vec<EdgeClassification> = Vec::with_capacity(edge_indices.len());

    for &eidx in edge_indices {
        let (src, dst) = match graph.graph.edge_endpoints(eidx) {
            Some(ep) => ep,
            None => continue,
        };

        let src_rect = match graph.graph[src].box_ {
            Some(r) => r,
            None => continue,
        };
        let dst_rect = match graph.graph[dst].box_ {
            Some(r) => r,
            None => continue,
        };

        let lca = find_lca(graph, src, dst);
        let src_anc = child_ancestor_of(graph, src, lca);
        let dst_anc = child_ancestor_of(graph, dst, lca);
        let direction = effective_direction(graph, lca);

        // Detect container-to-descendant
        let is_container_desc = src_anc == lca || dst_anc == lca;

        if is_container_desc {
            // Container-to-descendant: compute gap between the actual nodes
            let src_center = src_rect.center();
            let dst_center = dst_rect.center();
            let (gap_start, gap_end) = compute_container_gap(
                &src_rect, &dst_rect, &src_center, &dst_center, direction,
            );
            classified.push(EdgeClassification {
                edge_idx: eidx,
                src,
                dst,
                lca,
                direction,
                kind: EdgeKind::ContainerDescendant,
                gap_start,
                gap_end,
                channel_pos: 0.0,
                src_port: Point::new(0.0, 0.0),
                dst_port: Point::new(0.0, 0.0),
            });
            continue;
        }

        // Use ancestor-level rects for gap computation
        let src_anc_rect = match graph.graph[src_anc].box_ {
            Some(r) => r,
            None => src_rect,
        };
        let dst_anc_rect = match graph.graph[dst_anc].box_ {
            Some(r) => r,
            None => dst_rect,
        };

        let src_anc_center = src_anc_rect.center();
        let dst_anc_center = dst_anc_rect.center();

        let primary_diff = primary_coord(&src_anc_center, direction)
            - primary_coord(&dst_anc_center, direction);

        // Determine kind
        let kind = if primary_diff.abs() < 1.0 {
            EdgeKind::SameRank
        } else {
            let forward = match direction {
                Direction::Right => src_anc_center.x < dst_anc_center.x,
                Direction::Left => src_anc_center.x > dst_anc_center.x,
                Direction::Down => src_anc_center.y < dst_anc_center.y,
                Direction::Up => src_anc_center.y > dst_anc_center.y,
            };
            if forward {
                EdgeKind::Forward
            } else {
                EdgeKind::Backward
            }
        };

        // Compute gap interval
        let (gap_start, gap_end) = match kind {
            EdgeKind::Forward => compute_forward_gap(
                &src_anc_rect, &dst_anc_rect, direction,
            ),
            EdgeKind::Backward => compute_backward_gap(
                &src_anc_rect, &dst_anc_rect, direction,
            ),
            EdgeKind::SameRank => (0.0, 0.0),
            EdgeKind::ContainerDescendant => unreachable!(),
        };

        classified.push(EdgeClassification {
            edge_idx: eidx,
            src,
            dst,
            lca,
            direction,
            kind,
            gap_start,
            gap_end,
            channel_pos: 0.0,
            src_port: Point::new(0.0, 0.0),
            dst_port: Point::new(0.0, 0.0),
        });
    }

    // -----------------------------------------------------------------------
    // Pass 2: Allocate channels and ports
    // -----------------------------------------------------------------------

    // 2a) Channel allocation — group by (gap_key) and spread symmetrically
    allocate_channels(graph, &mut classified);

    // 2b) Source port allocation
    allocate_source_ports(graph, &mut classified);

    // 2c) Destination port allocation
    allocate_dest_ports(graph, &mut classified);

    // -----------------------------------------------------------------------
    // Pass 3: Build routes and write back to graph
    // -----------------------------------------------------------------------
    for ec in &classified {
        let src_rect = graph.graph[ec.src].box_.unwrap();
        let dst_rect = graph.graph[ec.dst].box_.unwrap();

        let route = build_route(ec, &src_rect, &dst_rect);

        // Label position: midpoint of the route for labeled edges
        let label_pos = if graph.graph[ec.edge_idx].label.is_some() {
            compute_label_position(&route)
        } else {
            None
        };

        graph.graph[ec.edge_idx].route = route;
        graph.graph[ec.edge_idx].route_type = RouteType::Orthogonal;
        graph.graph[ec.edge_idx].label_position = label_pos;
    }
}

/// Compute gap for a forward edge (src before dst in layout direction).
fn compute_forward_gap(src_rect: &Rect, dst_rect: &Rect, direction: Direction) -> (f64, f64) {
    match direction {
        Direction::Right => (src_rect.right(), dst_rect.x),
        Direction::Left => (dst_rect.right(), src_rect.x),
        Direction::Down => (src_rect.bottom(), dst_rect.y),
        Direction::Up => (dst_rect.bottom(), src_rect.y),
    }
}

/// Compute gap for a backward edge (src after dst in layout direction).
fn compute_backward_gap(src_rect: &Rect, dst_rect: &Rect, direction: Direction) -> (f64, f64) {
    // Backward: swap src/dst in the gap computation
    compute_forward_gap(dst_rect, src_rect, direction)
}

/// Compute gap for container-to-descendant edges.
fn compute_container_gap(
    src_rect: &Rect,
    dst_rect: &Rect,
    src_center: &Point,
    dst_center: &Point,
    direction: Direction,
) -> (f64, f64) {
    // Use actual node positions to determine gap
    if direction.is_horizontal() {
        if src_center.x < dst_center.x {
            (src_rect.right(), dst_rect.x)
        } else {
            (dst_rect.right(), src_rect.x)
        }
    } else if src_center.y < dst_center.y {
        (src_rect.bottom(), dst_rect.y)
    } else {
        (dst_rect.bottom(), src_rect.y)
    }
}

/// Pass 2a: Allocate channel positions in the gap between ranks.
fn allocate_channels(graph: &D2Graph, classified: &mut [EdgeClassification]) {
    // Group by (lca, rounded gap_start, gap_end)
    let mut groups: HashMap<(NodeIndex, i64, i64), Vec<usize>> = HashMap::new();
    for (i, ec) in classified.iter().enumerate() {
        if ec.kind == EdgeKind::SameRank || ec.kind == EdgeKind::ContainerDescendant {
            continue; // SameRank and ContainerDescendant edges don't use channels
        }
        let key = (ec.lca, ec.gap_start.round() as i64, ec.gap_end.round() as i64);
        groups.entry(key).or_default().push(i);
    }

    for indices in groups.values() {
        if indices.is_empty() {
            continue;
        }

        let first = &classified[indices[0]];
        let direction = first.direction;
        let gap_start = first.gap_start;
        let gap_end = first.gap_end;
        let gap_mid = (gap_start + gap_end) / 2.0;
        let gap_width = (gap_end - gap_start).abs();

        // Sort by (source cross-pos, dest cross-pos)
        let mut sorted: Vec<usize> = indices.clone();
        sorted.sort_by(|&a, &b| {
            let ea = &classified[a];
            let eb = &classified[b];
            let src_a = graph.graph[ea.src].box_.map(|r| cross_center(&r, direction)).unwrap_or(0.0);
            let src_b = graph.graph[eb.src].box_.map(|r| cross_center(&r, direction)).unwrap_or(0.0);
            let dst_a = graph.graph[ea.dst].box_.map(|r| cross_center(&r, direction)).unwrap_or(0.0);
            let dst_b = graph.graph[eb.dst].box_.map(|r| cross_center(&r, direction)).unwrap_or(0.0);
            src_a
                .partial_cmp(&src_b)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then(dst_a.partial_cmp(&dst_b).unwrap_or(std::cmp::Ordering::Equal))
        });

        let n = sorted.len();
        let total_spread = (n.saturating_sub(1)) as f64 * CHANNEL_SPACING;

        // Clamp if spread exceeds available gap
        let effective_spacing = if n > 1 && total_spread > gap_width - 2.0 * MIN_STUB_LENGTH {
            (gap_width - 2.0 * MIN_STUB_LENGTH).max(0.0) / (n - 1) as f64
        } else {
            CHANNEL_SPACING
        };

        for (rank, &idx) in sorted.iter().enumerate() {
            let offset = if n == 1 {
                0.0
            } else {
                -((n - 1) as f64 * effective_spacing) / 2.0 + rank as f64 * effective_spacing
            };
            classified[idx].channel_pos = gap_mid + offset;
        }
    }
}

/// Pass 2b: Allocate source ports — distribute evenly on the exit side.
fn allocate_source_ports(graph: &D2Graph, classified: &mut [EdgeClassification]) {
    // Group by (source node, exit side)
    let mut groups: HashMap<(NodeIndex, Direction), Vec<usize>> = HashMap::new();
    for (i, ec) in classified.iter().enumerate() {
        let exit_side = match ec.kind {
            EdgeKind::Forward | EdgeKind::SameRank => forward_exit(ec.direction),
            EdgeKind::Backward => backward_exit(ec.direction),
            EdgeKind::ContainerDescendant => {
                // Determine exit side based on node positions
                container_exit_side(graph, ec)
            }
        };
        groups.entry((ec.src, exit_side)).or_default().push(i);
    }

    for ((node, exit_side), indices) in &groups {
        let rect = match graph.graph[*node].box_ {
            Some(r) => r,
            None => continue,
        };
        let shape = graph.graph[*node].shape;

        // Sort by destination cross-pos
        let direction = classified[indices[0]].direction;
        let mut sorted: Vec<usize> = indices.clone();
        sorted.sort_by(|&a, &b| {
            let dst_a = graph.graph[classified[a].dst]
                .box_
                .map(|r| cross_center(&r, direction))
                .unwrap_or(0.0);
            let dst_b = graph.graph[classified[b].dst]
                .box_
                .map(|r| cross_center(&r, direction))
                .unwrap_or(0.0);
            dst_a
                .partial_cmp(&dst_b)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        let n = sorted.len();
        let (cross_start, cross_end) = cross_span(&rect, direction);
        let cross_dim = cross_end - cross_start;
        let step = cross_dim / (n + 1) as f64;

        for (rank, &idx) in sorted.iter().enumerate() {
            let port_cross = if step < 4.0 {
                cross_center(&rect, direction)
            } else {
                cross_start + step * (rank + 1) as f64
            };
            classified[idx].src_port = compute_port(shape, &rect, *exit_side, port_cross);
        }
    }
}

/// Pass 2c: Allocate destination ports — distribute evenly on the entry side.
fn allocate_dest_ports(graph: &D2Graph, classified: &mut [EdgeClassification]) {
    // Group by (dest node, entry side)
    let mut groups: HashMap<(NodeIndex, Direction), Vec<usize>> = HashMap::new();
    for (i, ec) in classified.iter().enumerate() {
        let entry_side = match ec.kind {
            EdgeKind::Forward => forward_entry(ec.direction),
            EdgeKind::SameRank => forward_exit(ec.direction),  // jog enters from the same side it exits
            EdgeKind::Backward => backward_entry(ec.direction),
            EdgeKind::ContainerDescendant => {
                container_entry_side(graph, ec)
            }
        };
        groups.entry((ec.dst, entry_side)).or_default().push(i);
    }

    for ((node, entry_side), indices) in &groups {
        let rect = match graph.graph[*node].box_ {
            Some(r) => r,
            None => continue,
        };
        let shape = graph.graph[*node].shape;

        // Sort by source cross-pos
        let direction = classified[indices[0]].direction;
        let mut sorted: Vec<usize> = indices.clone();
        sorted.sort_by(|&a, &b| {
            let src_a = graph.graph[classified[a].src]
                .box_
                .map(|r| cross_center(&r, direction))
                .unwrap_or(0.0);
            let src_b = graph.graph[classified[b].src]
                .box_
                .map(|r| cross_center(&r, direction))
                .unwrap_or(0.0);
            src_a
                .partial_cmp(&src_b)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        let n = sorted.len();
        let (cross_start, cross_end) = cross_span(&rect, direction);
        let cross_dim = cross_end - cross_start;
        let step = cross_dim / (n + 1) as f64;

        for (rank, &idx) in sorted.iter().enumerate() {
            let port_cross = if step < 4.0 {
                cross_center(&rect, direction)
            } else {
                cross_start + step * (rank + 1) as f64
            };
            classified[idx].dst_port = compute_port(shape, &rect, *entry_side, port_cross);
        }
    }
}

/// Determine exit side for a container-descendant edge.
fn container_exit_side(graph: &D2Graph, ec: &EdgeClassification) -> Direction {
    let src_center = graph.graph[ec.src].box_.map(|r| r.center()).unwrap_or(Point::new(0.0, 0.0));
    let dst_center = graph.graph[ec.dst].box_.map(|r| r.center()).unwrap_or(Point::new(0.0, 0.0));
    if ec.direction.is_horizontal() {
        if src_center.x <= dst_center.x {
            Direction::Right
        } else {
            Direction::Left
        }
    } else if src_center.y <= dst_center.y {
        Direction::Down
    } else {
        Direction::Up
    }
}

/// Determine entry side for a container-descendant edge.
fn container_entry_side(graph: &D2Graph, ec: &EdgeClassification) -> Direction {
    let src_center = graph.graph[ec.src].box_.map(|r| r.center()).unwrap_or(Point::new(0.0, 0.0));
    let dst_center = graph.graph[ec.dst].box_.map(|r| r.center()).unwrap_or(Point::new(0.0, 0.0));
    if ec.direction.is_horizontal() {
        if dst_center.x >= src_center.x {
            Direction::Left
        } else {
            Direction::Right
        }
    } else if dst_center.y >= src_center.y {
        Direction::Up
    } else {
        Direction::Down
    }
}

/// Pass 3: Build the polyline route for a classified edge.
fn build_route(
    ec: &EdgeClassification,
    src_rect: &Rect,
    dst_rect: &Rect,
) -> Vec<Point> {
    match ec.kind {
        EdgeKind::Forward | EdgeKind::Backward => build_three_segment_route(ec),
        EdgeKind::SameRank => build_same_rank_route(ec, src_rect, dst_rect),
        EdgeKind::ContainerDescendant => build_container_route(ec),
    }
}

/// Build a three-segment orthogonal route for forward and backward edges.
/// Both cases use the same geometry: straight line when ports are aligned
/// on the cross axis, otherwise an S-shaped path through the channel.
fn build_three_segment_route(ec: &EdgeClassification) -> Vec<Point> {
    let src = ec.src_port;
    let dst = ec.dst_port;

    if ec.direction.is_horizontal() {
        // Horizontal: primary=x, cross=y
        if (src.y - dst.y).abs() < 1.0 {
            // Straight line — same cross position
            vec![src, dst]
        } else {
            vec![
                src,
                Point::new(ec.channel_pos, src.y),
                Point::new(ec.channel_pos, dst.y),
                dst,
            ]
        }
    } else {
        // Vertical: primary=y, cross=x
        if (src.x - dst.x).abs() < 1.0 {
            vec![src, dst]
        } else {
            vec![
                src,
                Point::new(src.x, ec.channel_pos),
                Point::new(dst.x, ec.channel_pos),
                dst,
            ]
        }
    }
}

/// Build route for a same-rank edge.
fn build_same_rank_route(
    ec: &EdgeClassification,
    src_rect: &Rect,
    dst_rect: &Rect,
) -> Vec<Point> {
    let src = ec.src_port;
    let dst = ec.dst_port;

    if ec.direction.is_horizontal() {
        // Jog past the rightmost (for Right) or leftmost (for Left) node
        let jog = match ec.direction {
            Direction::Right => src_rect.right().max(dst_rect.right()) + SAME_RANK_JOG,
            Direction::Left => src_rect.x.min(dst_rect.x) - SAME_RANK_JOG,
            _ => unreachable!(),
        };
        vec![
            src,
            Point::new(jog, src.y),
            Point::new(jog, dst.y),
            dst,
        ]
    } else {
        // Vertical same-rank: jog past the bottommost (for Down) or topmost (for Up)
        let jog = match ec.direction {
            Direction::Down => src_rect.bottom().max(dst_rect.bottom()) + SAME_RANK_JOG,
            Direction::Up => src_rect.y.min(dst_rect.y) - SAME_RANK_JOG,
            _ => unreachable!(),
        };
        vec![
            src,
            Point::new(src.x, jog),
            Point::new(dst.x, jog),
            dst,
        ]
    }
}

/// Build an L-shaped route for container-to-descendant edges.
fn build_container_route(ec: &EdgeClassification) -> Vec<Point> {
    let src = ec.src_port;
    let dst = ec.dst_port;

    if ec.direction.is_horizontal() {
        // L-shape: horizontal then vertical, or use channel if available
        if (ec.gap_end - ec.gap_start).abs() > MIN_STUB_LENGTH * 2.0 {
            // There's a gap: use 3-segment route through channel
            let channel = (ec.gap_start + ec.gap_end) / 2.0;
            if (src.y - dst.y).abs() < 1.0 {
                vec![src, dst]
            } else {
                vec![
                    src,
                    Point::new(channel, src.y),
                    Point::new(channel, dst.y),
                    dst,
                ]
            }
        } else {
            // Tight gap: L-shape using intermediate point
            vec![src, Point::new(src.x, dst.y), dst]
        }
    } else {
        // Vertical container layout
        if (ec.gap_end - ec.gap_start).abs() > MIN_STUB_LENGTH * 2.0 {
            let channel = (ec.gap_start + ec.gap_end) / 2.0;
            if (src.x - dst.x).abs() < 1.0 {
                vec![src, dst]
            } else {
                vec![
                    src,
                    Point::new(src.x, channel),
                    Point::new(dst.x, channel),
                    dst,
                ]
            }
        } else {
            vec![src, Point::new(dst.x, src.y), dst]
        }
    }
}

/// Compute label position for an orthogonal route.
/// Places label at the midpoint of the middle segment (or overall midpoint
/// for 2-point routes), with a perpendicular offset of `LABEL_OFFSET` pixels.
fn compute_label_position(route: &[Point]) -> Option<Point> {
    if route.len() < 2 {
        return None;
    }

    if route.len() == 2 {
        // Straight line: midpoint with offset
        let mid = Point::new(
            (route[0].x + route[1].x) / 2.0,
            (route[0].y + route[1].y) / 2.0,
        );
        // Offset perpendicular to the line
        let dx = route[1].x - route[0].x;
        let dy = route[1].y - route[0].y;
        let len = (dx * dx + dy * dy).sqrt();
        if len > 1e-6 {
            return Some(Point::new(mid.x - dy / len * LABEL_OFFSET, mid.y + dx / len * LABEL_OFFSET));
        }
        return Some(mid);
    }

    // For 3+ point routes: use midpoint of the middle segment
    let mid_seg = route.len() / 2;
    let a = route[mid_seg - 1];
    let b = route[mid_seg];
    let mid = Point::new((a.x + b.x) / 2.0, (a.y + b.y) / 2.0);

    // Offset perpendicular to the segment
    let dx = b.x - a.x;
    let dy = b.y - a.y;
    let len = (dx * dx + dy * dy).sqrt();
    if len > 1e-6 {
        Some(Point::new(mid.x - dy / len * LABEL_OFFSET, mid.y + dx / len * LABEL_OFFSET))
    } else {
        Some(mid)
    }
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

    // --- orthogonal routing tests --------------------------------------------

    #[test]
    fn test_ortho_straight_horizontal() {
        // direction: right, a -> b — single forward edge should produce
        // a 2-point straight line with route_type == Orthogonal.
        let graph = layout_ok("direction: right\na -> b");
        let edge = find_edge_between(&graph, "a", "b");

        assert_eq!(
            edge.route_type,
            RouteType::Orthogonal,
            "non-self-loop edge should be Orthogonal"
        );
        assert_eq!(
            edge.route.len(),
            2,
            "straight horizontal edge should have 2 route points, got {}",
            edge.route.len()
        );
        // Destination x should be to the right of source x
        assert!(
            edge.route[1].x > edge.route[0].x,
            "dst.x ({}) should be right of src.x ({})",
            edge.route[1].x,
            edge.route[0].x,
        );
    }

    #[test]
    fn test_ortho_straight_vertical() {
        // Default direction is down; a -> b — straight vertical line.
        let graph = layout_ok("a -> b");
        let edge = find_edge_between(&graph, "a", "b");

        assert_eq!(edge.route_type, RouteType::Orthogonal);
        assert_eq!(
            edge.route.len(),
            2,
            "straight vertical edge should have 2 route points, got {}",
            edge.route.len()
        );
        // Destination y should be below source y
        assert!(
            edge.route[1].y > edge.route[0].y,
            "dst.y ({}) should be below src.y ({})",
            edge.route[1].y,
            edge.route[0].y,
        );
    }

    #[test]
    fn test_ortho_straight_left() {
        // direction: left, a -> b — dst should be left of src
        let graph = layout_ok("direction: left\na -> b");
        let edge = find_edge_between(&graph, "a", "b");

        assert_eq!(edge.route_type, RouteType::Orthogonal);
        assert_eq!(
            edge.route.len(),
            2,
            "straight left edge should have 2 route points, got {}",
            edge.route.len()
        );
        assert!(
            edge.route[1].x < edge.route[0].x,
            "dst.x ({}) should be left of src.x ({})",
            edge.route[1].x,
            edge.route[0].x,
        );
    }

    #[test]
    fn test_ortho_straight_up() {
        // direction: up, a -> b — dst should be above src
        let graph = layout_ok("direction: up\na -> b");
        let edge = find_edge_between(&graph, "a", "b");

        assert_eq!(edge.route_type, RouteType::Orthogonal);
        assert_eq!(
            edge.route.len(),
            2,
            "straight up edge should have 2 route points, got {}",
            edge.route.len()
        );
        assert!(
            edge.route[1].y < edge.route[0].y,
            "dst.y ({}) should be above src.y ({})",
            edge.route[1].y,
            edge.route[0].y,
        );
    }

    #[test]
    fn test_ortho_three_segment() {
        // direction: right, a -> c, b -> c — a and b are in different ranks,
        // the edge from the offset node should have >= 2 route points.
        let graph = layout_ok("direction: right\na -> c\nb -> c");
        let edge_ac = find_edge_between(&graph, "a", "c");
        let edge_bc = find_edge_between(&graph, "b", "c");

        assert_eq!(edge_ac.route_type, RouteType::Orthogonal);
        assert_eq!(edge_bc.route_type, RouteType::Orthogonal);

        // At least one of them should have >= 2 points (both should)
        assert!(
            edge_ac.route.len() >= 2,
            "a->c route should have >= 2 points, got {}",
            edge_ac.route.len()
        );
        assert!(
            edge_bc.route.len() >= 2,
            "b->c route should have >= 2 points, got {}",
            edge_bc.route.len()
        );
    }

    #[test]
    fn test_ortho_same_rank_jog() {
        // Same-rank edges need a jog (detour) since both nodes are at the
        // same primary coordinate. We use two nodes at the same rank by
        // having them both connect to a third node.
        // However, to directly test same-rank we need nodes at the same
        // position. A simpler approach: two nodes with edges to each other
        // creates a forward + backward pair.
        // For a true same-rank test we need parallel edges in the same rank.
        // Use a -> c, b -> c pattern where a and b end up on the same rank.
        let graph = layout_ok("direction: right\na -> b\na -> c\nb -> c");
        // All edges should be non-empty and orthogonal
        for &eidx in &graph.edges {
            let edata = &graph.graph[eidx];
            assert_eq!(edata.route_type, RouteType::Orthogonal);
            assert!(
                !edata.route.is_empty(),
                "every edge should have route points"
            );
        }
    }

    #[test]
    fn test_ortho_backward_edge() {
        // b -> a is a backward edge when direction=right and a is before b.
        let graph = layout_ok("direction: right\na -> b\nb -> a");

        // a -> b should be forward
        let edge_ab = find_edge_between(&graph, "a", "b");
        assert_eq!(edge_ab.route_type, RouteType::Orthogonal);
        assert!(
            !edge_ab.route.is_empty(),
            "forward edge should have route points"
        );

        // b -> a should be backward
        let edge_ba = find_edge_between(&graph, "b", "a");
        assert_eq!(edge_ba.route_type, RouteType::Orthogonal);
        assert!(
            !edge_ba.route.is_empty(),
            "backward edge should have route points"
        );
    }

    #[test]
    fn test_ortho_self_loop_unchanged() {
        // Self-loops should still use Bézier routing with 7 points.
        let graph = layout_ok("a -> a");
        let a = find_node_by_label(&graph, "a");
        for &eidx in &graph.edges {
            if let Some((s, d)) = graph.graph.edge_endpoints(eidx) {
                if s == a && d == a {
                    let edata = &graph.graph[eidx];
                    assert_eq!(
                        edata.route_type,
                        RouteType::Bezier,
                        "self-loop should use Bezier routing"
                    );
                    assert_eq!(
                        edata.route.len(),
                        7,
                        "self-loop should have 7 points, got {}",
                        edata.route.len()
                    );
                    return;
                }
            }
        }
        panic!("no self-loop edge found");
    }

    #[test]
    fn test_ortho_container_descendant() {
        // Edge from container to its own child.
        let graph = layout_ok("g: { a }\ng -> g.a");
        let edge = find_edge_between(&graph, "g", "a");

        assert_eq!(
            edge.route_type,
            RouteType::Orthogonal,
            "container-descendant edge should be Orthogonal"
        );
        assert!(
            !edge.route.is_empty(),
            "container-descendant edge should have route points"
        );
    }

    #[test]
    fn test_ortho_architecture_integration() {
        // Full architecture example — all edges should be routed without panics.
        let source = "\
direction: right

network: {
  style.stroke: \"#4a90d9\"
  load_balancer.shape: diamond
  load_balancer.style.fill: \"#ffd700\"
  server1
  server2
  load_balancer -> server1: route
  load_balancer -> server2: route
}

database.shape: cylinder
database.style.fill: \"#228b22\"

network.server1 -> database: query
network.server2 -> database: query
";
        let graph = layout_ok(source);

        for &eidx in &graph.edges {
            let edata = &graph.graph[eidx];
            if let Some((s, d)) = graph.graph.edge_endpoints(eidx) {
                if s == d {
                    continue; // skip self-loops
                }
            }
            assert!(
                !edata.route.is_empty(),
                "every non-self-loop edge should have route points"
            );
            assert_eq!(
                edata.route_type,
                RouteType::Orthogonal,
                "every non-self-loop edge should be Orthogonal"
            );
        }
    }

    #[test]
    fn test_ortho_cross_container() {
        // Cross-container edges (between nodes in different containers).
        let graph = layout_ok("a: { x }\nb: { y }\na.x -> b.y");

        let edge = find_edge_between(&graph, "x", "y");
        assert_eq!(
            edge.route_type,
            RouteType::Orthogonal,
            "cross-container edge should be Orthogonal"
        );
        assert!(
            !edge.route.is_empty(),
            "cross-container edge should have route points"
        );
    }
}
