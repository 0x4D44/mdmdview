//! Sugiyama algorithm implementation for D2 layout.
//!
//! Implements the four classic Sugiyama phases with node-size awareness:
//! 1. Cycle removal (reverse back-edges)
//! 2. Rank assignment (longest-path)
//! 3. Crossing reduction (barycenter heuristic)
//! 4. Coordinate assignment (simple center-aligned placement)
//!
//! This operates on a subset of the graph: the direct children of a
//! container node, treating child containers as opaque nodes with
//! pre-computed dimensions.

use std::collections::{HashMap, HashSet, VecDeque};

use petgraph::stable_graph::{EdgeIndex, NodeIndex};

use crate::graph::{D2Graph, Direction};
use crate::layout::LayoutError;

/// Default spacing between nodes (horizontal).
const NODE_SPACING_H: f64 = 40.0;
/// Default spacing between ranks (vertical).
const NODE_SPACING_V: f64 = 60.0;

/// Gap between disconnected components (pixels).
const COMPONENT_GAP: f64 = 60.0;

/// Minimum spacing between container nodes in the same rank.
const CONTAINER_CROSS_SPACING: f64 = 60.0;
/// Minimum spacing between ranks when either rank has a container.
const CONTAINER_RANK_SPACING: f64 = 80.0;

/// Number of barycenter crossing-reduction sweeps.
/// Empirically sufficient for diagrams under ~200 nodes.
const CROSSING_REDUCTION_ITERATIONS: usize = 12;

/// Lay out the direct children of a container using the Sugiyama algorithm.
/// When the graph has multiple disconnected components, each is laid out
/// independently and then arranged side-by-side.
pub fn layout_children(
    graph: &mut D2Graph,
    container: NodeIndex,
    direction: Direction,
) -> Result<(), LayoutError> {
    let children: Vec<NodeIndex> = graph.graph[container].children.clone();
    if children.is_empty() {
        return Ok(());
    }

    let child_set: HashSet<NodeIndex> = children.iter().copied().collect();

    // Collect edges between children (including cross-container edges
    // that have both endpoints as descendants of this container)
    let edges: Vec<(NodeIndex, NodeIndex, EdgeIndex)> = graph
        .edges
        .iter()
        .filter_map(|&eidx| {
            let (src, dst) = graph.graph.edge_endpoints(eidx)?;
            // Find the child-level ancestor for each endpoint
            let src_child = find_child_ancestor(graph, src, container, &child_set);
            let dst_child = find_child_ancestor(graph, dst, container, &child_set);
            match (src_child, dst_child) {
                (Some(s), Some(d)) if s != d => Some((s, d, eidx)),
                _ => None,
            }
        })
        .collect();

    // If no edges, arrange in a simple line
    if edges.is_empty() {
        arrange_in_line(graph, &children, direction);
        return Ok(());
    }

    // Find connected components among children
    let components = find_components(&children, &edges);

    if components.len() <= 1 {
        // Single component: lay out directly
        layout_component(graph, &children, &edges, direction);
    } else {
        // Multiple disconnected components: lay out each independently,
        // then stack side-by-side perpendicular to the layout direction
        let is_horizontal = direction.is_horizontal();
        let mut cross_offset = 0.0;

        for component in &components {
            // Collect edges for this component
            let comp_set: HashSet<NodeIndex> = component.iter().copied().collect();
            let comp_edges: Vec<(NodeIndex, NodeIndex, EdgeIndex)> = edges
                .iter()
                .filter(|(s, d, _)| comp_set.contains(s) && comp_set.contains(d))
                .copied()
                .collect();

            if comp_edges.is_empty() {
                arrange_in_line(graph, component, direction);
            } else {
                layout_component(graph, component, &comp_edges, direction);
            }

            // Compute bounding box of this component
            let mut min_cross = f64::INFINITY;
            let mut max_cross = f64::NEG_INFINITY;

            for &node in component {
                if let Some(rect) = graph.graph[node].box_ {
                    let (lo, hi) = if is_horizontal {
                        (rect.y, rect.y + rect.height)
                    } else {
                        (rect.x, rect.x + rect.width)
                    };
                    min_cross = min_cross.min(lo);
                    max_cross = max_cross.max(hi);
                }
            }

            if min_cross == f64::INFINITY {
                continue;
            }

            // Shift this component so it starts at cross_offset
            let shift = cross_offset - min_cross;
            for &node in component {
                if is_horizontal {
                    graph.offset_node(node, 0.0, shift);
                } else {
                    graph.offset_node(node, shift, 0.0);
                }
            }

            cross_offset += (max_cross - min_cross) + COMPONENT_GAP;
        }
    }

    Ok(())
}

/// Find connected components using union-find on the children.
fn find_components(
    children: &[NodeIndex],
    edges: &[(NodeIndex, NodeIndex, EdgeIndex)],
) -> Vec<Vec<NodeIndex>> {
    let mut parent_map: HashMap<NodeIndex, NodeIndex> = HashMap::new();
    for &c in children {
        parent_map.insert(c, c);
    }

    fn find(parent_map: &mut HashMap<NodeIndex, NodeIndex>, mut x: NodeIndex) -> NodeIndex {
        // Iterative path compression — avoids stack overflow on deep chains.
        while parent_map[&x] != x {
            let grandparent = parent_map[&parent_map[&x]];
            parent_map.insert(x, grandparent);
            x = grandparent;
        }
        x
    }

    fn union(parent_map: &mut HashMap<NodeIndex, NodeIndex>, a: NodeIndex, b: NodeIndex) {
        let ra = find(parent_map, a);
        let rb = find(parent_map, b);
        if ra != rb {
            parent_map.insert(ra, rb);
        }
    }

    for &(src, dst, _) in edges {
        union(&mut parent_map, src, dst);
    }

    // Group by root
    let mut groups: HashMap<NodeIndex, Vec<NodeIndex>> = HashMap::new();
    for &c in children {
        let root = find(&mut parent_map, c);
        groups.entry(root).or_default().push(c);
    }

    groups.into_values().collect()
}

/// Lay out a single connected component using Sugiyama.
fn layout_component(
    graph: &mut D2Graph,
    children: &[NodeIndex],
    edges: &[(NodeIndex, NodeIndex, EdgeIndex)],
    direction: Direction,
) {
    // Phase 1: Cycle removal
    let (forward_edges, _back_edges) = remove_cycles(children, edges);

    // Phase 2: Rank assignment
    let ranks = assign_ranks(children, &forward_edges);

    // Phase 3: Crossing reduction
    let ordered_ranks = reduce_crossings(&ranks, &forward_edges);

    // Phase 4: Coordinate assignment
    assign_coordinates(graph, &ordered_ranks, direction);
}

/// Find the child of `container` that is an ancestor of (or equal to) `node`.
fn find_child_ancestor(
    graph: &D2Graph,
    mut node: NodeIndex,
    container: NodeIndex,
    child_set: &HashSet<NodeIndex>,
) -> Option<NodeIndex> {
    // Walk up the parent chain until we find a direct child of container
    loop {
        if child_set.contains(&node) {
            return Some(node);
        }
        match graph.graph[node].parent {
            Some(parent) if parent != container => node = parent,
            _ => return None,
        }
    }
}

/// Simple line arrangement when there are no edges.
fn arrange_in_line(graph: &mut D2Graph, children: &[NodeIndex], direction: Direction) {
    let mut offset = 0.0;

    for &child in children {
        let Some(rect) = graph.graph[child].box_ else { continue; };
        match direction {
            Direction::Down | Direction::Up => {
                graph.reposition_node(child, 0.0, offset);
                offset += rect.height + NODE_SPACING_V;
            }
            Direction::Right | Direction::Left => {
                graph.reposition_node(child, offset, 0.0);
                offset += rect.width + NODE_SPACING_H;
            }
        }
    }
}

/// Cycle removal via DFS. Returns (forward_edges, back_edges).
fn remove_cycles(
    children: &[NodeIndex],
    edges: &[(NodeIndex, NodeIndex, EdgeIndex)],
) -> (
    Vec<(NodeIndex, NodeIndex, EdgeIndex)>,
    Vec<(NodeIndex, NodeIndex, EdgeIndex)>,
) {
    let mut visited = HashSet::new();
    let mut in_stack = HashSet::new();
    let mut forward = Vec::new();
    let mut back = Vec::new();

    // Build adjacency
    let mut adj: HashMap<NodeIndex, Vec<(NodeIndex, EdgeIndex)>> = HashMap::new();
    for &(src, dst, eidx) in edges {
        adj.entry(src).or_default().push((dst, eidx));
    }

    fn dfs(
        node: NodeIndex,
        adj: &HashMap<NodeIndex, Vec<(NodeIndex, EdgeIndex)>>,
        visited: &mut HashSet<NodeIndex>,
        in_stack: &mut HashSet<NodeIndex>,
        forward: &mut Vec<(NodeIndex, NodeIndex, EdgeIndex)>,
        back: &mut Vec<(NodeIndex, NodeIndex, EdgeIndex)>,
    ) {
        visited.insert(node);
        in_stack.insert(node);

        if let Some(neighbors) = adj.get(&node) {
            for &(dst, eidx) in neighbors {
                if in_stack.contains(&dst) {
                    back.push((node, dst, eidx));
                } else if !visited.contains(&dst) {
                    forward.push((node, dst, eidx));
                    dfs(dst, adj, visited, in_stack, forward, back);
                } else {
                    forward.push((node, dst, eidx));
                }
            }
        }

        in_stack.remove(&node);
    }

    for &child in children {
        if !visited.contains(&child) {
            dfs(
                child,
                &adj,
                &mut visited,
                &mut in_stack,
                &mut forward,
                &mut back,
            );
        }
    }

    (forward, back)
}

/// Longest-path rank assignment.
fn assign_ranks(
    children: &[NodeIndex],
    edges: &[(NodeIndex, NodeIndex, EdgeIndex)],
) -> HashMap<usize, Vec<NodeIndex>> {
    // Build in-degree and adjacency
    let mut in_degree: HashMap<NodeIndex, usize> = HashMap::new();
    let mut adj: HashMap<NodeIndex, Vec<NodeIndex>> = HashMap::new();
    let mut rank: HashMap<NodeIndex, usize> = HashMap::new();

    for &child in children {
        in_degree.insert(child, 0);
    }

    for &(src, dst, _) in edges {
        *in_degree.entry(dst).or_default() += 1;
        adj.entry(src).or_default().push(dst);
    }

    // Topological sort with longest path
    let mut queue: VecDeque<NodeIndex> = VecDeque::new();
    for &child in children {
        if *in_degree.get(&child).unwrap_or(&0) == 0 {
            queue.push_back(child);
            rank.insert(child, 0);
        }
    }

    // Handle case where all nodes have incoming edges (residual cycle).
    // Seed all nodes with minimum in-degree so the BFS can reach everything.
    if queue.is_empty() {
        let min_deg = children
            .iter()
            .filter_map(|c| in_degree.get(c))
            .copied()
            .min()
            .unwrap_or(0);
        for &child in children {
            if *in_degree.get(&child).unwrap_or(&0) == min_deg {
                queue.push_back(child);
                rank.insert(child, 0);
            }
        }
    }

    while let Some(node) = queue.pop_front() {
        let node_rank = *rank.get(&node).unwrap_or(&0);

        if let Some(neighbors) = adj.get(&node) {
            for &next in neighbors {
                let new_rank = node_rank + 1;
                let current = rank.entry(next).or_default();
                if new_rank > *current {
                    *current = new_rank;
                }

                if let Some(deg) = in_degree.get_mut(&next) {
                    *deg -= 1;
                    if *deg == 0 {
                        queue.push_back(next);
                    }
                }
            }
        }
    }

    // Assign unranked nodes (disconnected) to rank 0
    for &child in children {
        rank.entry(child).or_insert(0);
    }

    // Group by rank
    let mut ranks: HashMap<usize, Vec<NodeIndex>> = HashMap::new();
    for (&node, &r) in &rank {
        ranks.entry(r).or_default().push(node);
    }

    ranks
}

/// Crossing reduction using barycenter heuristic.
fn reduce_crossings(
    ranks: &HashMap<usize, Vec<NodeIndex>>,
    edges: &[(NodeIndex, NodeIndex, EdgeIndex)],
) -> Vec<Vec<NodeIndex>> {
    let max_rank = ranks.keys().copied().max().unwrap_or(0);
    let mut ordered: Vec<Vec<NodeIndex>> = (0..=max_rank)
        .map(|r| ranks.get(&r).cloned().unwrap_or_default())
        .collect();

    // Build position maps
    let mut node_positions: HashMap<NodeIndex, usize> = HashMap::new();
    for rank_nodes in &ordered {
        for (i, &node) in rank_nodes.iter().enumerate() {
            node_positions.insert(node, i);
        }
    }

    for _iter in 0..CROSSING_REDUCTION_ITERATIONS {
        // Downward sweep
        for r in 1..=max_rank {
            let curr_rank = &ordered[r];

            let mut barycenters: Vec<(NodeIndex, f64)> = curr_rank
                .iter()
                .map(|&node| {
                    let incoming: Vec<f64> = edges
                        .iter()
                        .filter(|(_, dst, _)| *dst == node)
                        .filter_map(|(src, _, _)| node_positions.get(src))
                        .map(|&pos| pos as f64)
                        .collect();

                    let bc = if incoming.is_empty() {
                        node_positions.get(&node).map(|&p| p as f64).unwrap_or(0.0)
                    } else {
                        incoming.iter().sum::<f64>() / incoming.len() as f64
                    };
                    (node, bc)
                })
                .collect();

            barycenters.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
            ordered[r] = barycenters.iter().map(|(node, _)| *node).collect();

            // Update positions
            for (i, &node) in ordered[r].iter().enumerate() {
                node_positions.insert(node, i);
            }
        }

        // Upward sweep
        for r in (0..max_rank).rev() {
            let curr_rank = &ordered[r];

            let mut barycenters: Vec<(NodeIndex, f64)> = curr_rank
                .iter()
                .map(|&node| {
                    let outgoing: Vec<f64> = edges
                        .iter()
                        .filter(|(src, _, _)| *src == node)
                        .filter_map(|(_, dst, _)| node_positions.get(dst))
                        .map(|&pos| pos as f64)
                        .collect();

                    let bc = if outgoing.is_empty() {
                        node_positions.get(&node).map(|&p| p as f64).unwrap_or(0.0)
                    } else {
                        outgoing.iter().sum::<f64>() / outgoing.len() as f64
                    };
                    (node, bc)
                })
                .collect();

            barycenters.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
            ordered[r] = barycenters.iter().map(|(node, _)| *node).collect();

            for (i, &node) in ordered[r].iter().enumerate() {
                node_positions.insert(node, i);
            }
        }
    }

    ordered
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geo::Rect;
    use crate::graph::{D2Graph, D2Object, ShapeType, Style};

    /// Helper: build a minimal graph with N nodes as children of root,
    /// each with a pre-set box_ so the layout can place them.
    /// Returns (graph, vec_of_child_indices).
    fn make_graph(labels: &[&str]) -> (D2Graph, Vec<NodeIndex>) {
        let mut graph = D2Graph::new();
        let root = graph.root;
        let mut children = Vec::new();

        for &label in labels {
            let idx = graph.graph.add_node(D2Object {
                id: label.to_string(),
                label: label.to_string(),
                shape: ShapeType::Rectangle,
                style: Style::default(),
                parent: Some(root),
                children: Vec::new(),
                direction: None,
                box_: Some(Rect::new(0.0, 0.0, 60.0, 36.0)),
                label_dimensions: Some((50.0, 26.0)),
                label_lines: vec![label.to_string()],
                is_container: false,
            });
            graph.graph[root].children.push(idx);
            graph.objects.push(idx);
            children.push(idx);
        }

        (graph, children)
    }

    /// Helper: add an edge between two children and register it.
    fn add_edge(
        graph: &mut D2Graph,
        src: NodeIndex,
        dst: NodeIndex,
    ) -> EdgeIndex {
        use crate::graph::{ArrowheadType, D2EdgeData};

        let eidx = graph.graph.add_edge(
            src,
            dst,
            D2EdgeData {
                label: None,
                src_arrow: ArrowheadType::None,
                dst_arrow: ArrowheadType::Arrow,
                style: Style::default(),
                route: Vec::new(),
                label_position: None,
            },
        );
        graph.edges.push(eidx);
        eidx
    }

    // --- test_linear_chain_ranking -------------------------------------------

    #[test]
    fn test_linear_chain_ranking() {
        // a -> b -> c should produce 3 distinct ranks: a=0, b=1, c=2.
        let (mut graph, children) = make_graph(&["a", "b", "c"]);
        let a = children[0];
        let b = children[1];
        let c = children[2];

        let e1 = add_edge(&mut graph, a, b);
        let e2 = add_edge(&mut graph, b, c);

        let edges = vec![(a, b, e1), (b, c, e2)];
        let (forward, _back) = remove_cycles(&children, &edges);
        let ranks = assign_ranks(&children, &forward);

        // There should be 3 distinct ranks: 0, 1, 2
        assert_eq!(ranks.len(), 3, "expected 3 ranks, got {}", ranks.len());

        // Build a node-to-rank lookup
        let mut node_rank: HashMap<NodeIndex, usize> = HashMap::new();
        for (&rank, nodes) in &ranks {
            for &node in nodes {
                node_rank.insert(node, rank);
            }
        }

        let rank_a = node_rank[&a];
        let rank_b = node_rank[&b];
        let rank_c = node_rank[&c];

        assert!(
            rank_a < rank_b,
            "a (rank {rank_a}) should be before b (rank {rank_b})"
        );
        assert!(
            rank_b < rank_c,
            "b (rank {rank_b}) should be before c (rank {rank_c})"
        );
    }

    // --- test_cycle_removal --------------------------------------------------

    #[test]
    fn test_cycle_removal() {
        // a -> b -> c -> a (cycle). Should not panic or infinite loop.
        // Back-edges list should be non-empty (at least one edge reversed).
        let (mut graph, children) = make_graph(&["a", "b", "c"]);
        let a = children[0];
        let b = children[1];
        let c = children[2];

        let e1 = add_edge(&mut graph, a, b);
        let e2 = add_edge(&mut graph, b, c);
        let e3 = add_edge(&mut graph, c, a);

        let edges = vec![(a, b, e1), (b, c, e2), (c, a, e3)];
        let (forward, back) = remove_cycles(&children, &edges);

        // At least one edge should be classified as a back-edge
        assert!(
            !back.is_empty(),
            "expected at least one back-edge in a cycle"
        );

        // All edges should appear in either forward or back (none lost)
        assert_eq!(
            forward.len() + back.len(),
            edges.len(),
            "forward + back should account for all edges"
        );

        // The forward edges should be usable for ranking without panic
        let ranks = assign_ranks(&children, &forward);
        assert!(
            !ranks.is_empty(),
            "ranking should succeed after cycle removal"
        );
    }

    // --- test_crossing_reduction ---------------------------------------------

    #[test]
    fn test_crossing_reduction() {
        // Minimal case: a -> c, b -> d. Two independent edges, no crossings.
        // After crossing reduction, the order should not introduce crossings.
        let (mut graph, children) = make_graph(&["a", "b", "c", "d"]);
        let a = children[0];
        let b = children[1];
        let c = children[2];
        let d = children[3];

        let e1 = add_edge(&mut graph, a, c);
        let e2 = add_edge(&mut graph, b, d);

        let edges = vec![(a, c, e1), (b, d, e2)];
        let (forward, _) = remove_cycles(&children, &edges);
        let ranks = assign_ranks(&children, &forward);
        let ordered = reduce_crossings(&ranks, &forward);

        // Should produce 2 ranks
        assert_eq!(ordered.len(), 2, "expected 2 ranks, got {}", ordered.len());

        // Each rank should have 2 nodes
        assert_eq!(ordered[0].len(), 2);
        assert_eq!(ordered[1].len(), 2);

        // Verify no crossing: if a is before b in rank 0, then c should be
        // before d in rank 1 (or vice versa — the relative order should match).
        let a_pos = ordered[0].iter().position(|&n| n == a).unwrap();
        let b_pos = ordered[0].iter().position(|&n| n == b).unwrap();
        let c_pos = ordered[1].iter().position(|&n| n == c).unwrap();
        let d_pos = ordered[1].iter().position(|&n| n == d).unwrap();

        // If a < b then c < d (no crossing), or if b < a then d < c
        let same_order = (a_pos < b_pos && c_pos < d_pos)
            || (b_pos < a_pos && d_pos < c_pos);
        assert!(
            same_order,
            "crossing reduction should not introduce crossings: \
             a@{a_pos}, b@{b_pos}, c@{c_pos}, d@{d_pos}"
        );
    }

    // --- test_single_node ----------------------------------------------------

    #[test]
    fn test_single_node() {
        // A single node with no edges should not panic.
        let (mut graph, children) = make_graph(&["alone"]);
        let root = graph.root;

        // layout_children should succeed without edges
        let result = layout_children(&mut graph, root, Direction::Down);
        assert!(result.is_ok(), "single node layout should succeed");

        // The node should have a valid position
        let rect = graph.graph[children[0]]
            .box_
            .expect("single node should have a box after layout");
        assert!(
            rect.width > 0.0 && rect.height > 0.0,
            "node should have non-zero dimensions"
        );
    }

    // --- test_disconnected_nodes ---------------------------------------------

    #[test]
    fn test_disconnected_nodes() {
        // Multiple nodes with no edges: they should be arranged, not all
        // at the same position.
        let (mut graph, children) = make_graph(&["x", "y", "z"]);
        let root = graph.root;

        let result = layout_children(&mut graph, root, Direction::Down);
        assert!(result.is_ok(), "disconnected nodes layout should succeed");

        // Collect positions
        let positions: Vec<(f64, f64)> = children
            .iter()
            .map(|&idx| {
                let rect = graph.graph[idx]
                    .box_
                    .expect("node should have a box after layout");
                (rect.x, rect.y)
            })
            .collect();

        // Not all at the same position
        let all_same = positions
            .windows(2)
            .all(|w| (w[0].0 - w[1].0).abs() < 0.01 && (w[0].1 - w[1].1).abs() < 0.01);
        assert!(
            !all_same,
            "disconnected nodes should not all be at the same position: {positions:?}"
        );

        // For direction=Down, nodes should be stacked vertically (different y)
        let ys: Vec<f64> = positions.iter().map(|p| p.1).collect();
        assert!(
            ys[0] < ys[1] && ys[1] < ys[2],
            "nodes should be in increasing y order for direction=Down: {ys:?}"
        );
    }
}

/// Assign x,y coordinates to nodes based on rank and order.
fn assign_coordinates(
    graph: &mut D2Graph,
    ordered_ranks: &[Vec<NodeIndex>],
    direction: Direction,
) {
    if ordered_ranks.is_empty() {
        return;
    }

    let is_horizontal = direction.is_horizontal();

    // Compute rank offsets (cumulative height/width of each rank)
    let mut rank_offset = 0.0;
    let mut rank_offsets: Vec<f64> = Vec::new();

    for rank_nodes in ordered_ranks {
        rank_offsets.push(rank_offset);

        // Max size in the rank direction
        let max_size = rank_nodes
            .iter()
            .filter_map(|&node| graph.graph[node].box_)
            .map(|rect| if is_horizontal { rect.width } else { rect.height })
            .fold(0.0f64, f64::max);

        let has_container = rank_nodes.iter().any(|&n| graph.graph[n].is_container);
        let rank_spacing = if has_container { CONTAINER_RANK_SPACING } else { NODE_SPACING_V };
        rank_offset += max_size + rank_spacing;
    }

    // Place nodes
    for (rank_idx, rank_nodes) in ordered_ranks.iter().enumerate() {
        // Compute total cross-rank size for centering (container-aware spacing).
        // The gap after each node is determined by that node's is_container flag,
        // matching the placement loop below.
        let total_cross: f64 = {
            let nodes_with_boxes: Vec<_> = rank_nodes
                .iter()
                .filter(|&&n| graph.graph[n].box_.is_some())
                .copied()
                .collect();
            let mut total = 0.0;
            for (i, &n) in nodes_with_boxes.iter().enumerate() {
                let rect = graph.graph[n].box_.unwrap();
                total += if is_horizontal { rect.height } else { rect.width };
                if i + 1 < nodes_with_boxes.len() {
                    total += if graph.graph[n].is_container {
                        CONTAINER_CROSS_SPACING
                    } else {
                        NODE_SPACING_H
                    };
                }
            }
            total
        };

        let mut cross_offset = -total_cross / 2.0;

        for &node in rank_nodes {
            let Some(rect) = graph.graph[node].box_ else { continue; };
            let cross_spacing = if graph.graph[node].is_container {
                CONTAINER_CROSS_SPACING
            } else {
                NODE_SPACING_H
            };
            match direction {
                Direction::Down => {
                    graph.reposition_node(node, cross_offset, rank_offsets[rank_idx]);
                    cross_offset += rect.width + cross_spacing;
                }
                Direction::Up => {
                    graph.reposition_node(
                        node,
                        cross_offset,
                        -(rank_offsets[rank_idx] + rect.height),
                    );
                    cross_offset += rect.width + cross_spacing;
                }
                Direction::Right => {
                    graph.reposition_node(node, rank_offsets[rank_idx], cross_offset);
                    cross_offset += rect.height + cross_spacing;
                }
                Direction::Left => {
                    graph.reposition_node(
                        node,
                        -(rank_offsets[rank_idx] + rect.width),
                        cross_offset,
                    );
                    cross_offset += rect.height + cross_spacing;
                }
            }
        }
    }
}
