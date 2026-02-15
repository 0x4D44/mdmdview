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

/// Lay out the direct children of a container using the Sugiyama algorithm.
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

    // Phase 1: Cycle removal — find back edges via DFS
    let (forward_edges, _back_edges) = remove_cycles(&children, &edges);

    // Phase 2: Rank assignment — longest path
    let ranks = assign_ranks(&children, &forward_edges);

    // Phase 3: Crossing reduction — barycenter ordering
    let ordered_ranks = reduce_crossings(&ranks, &forward_edges);

    // Phase 4: Coordinate assignment
    assign_coordinates(graph, &ordered_ranks, direction);

    Ok(())
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
        if let Some(ref mut rect) = graph.graph[child].box_ {
            match direction {
                Direction::Down | Direction::Up => {
                    rect.x = 0.0;
                    rect.y = offset;
                    offset += rect.height + NODE_SPACING_V;
                }
                Direction::Right | Direction::Left => {
                    rect.x = offset;
                    rect.y = 0.0;
                    offset += rect.width + NODE_SPACING_H;
                }
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

    // Handle case where all nodes have incoming edges (cycle — should be broken already)
    if queue.is_empty() {
        // Fallback: assign rank 0 to first child
        if let Some(&first) = children.first() {
            queue.push_back(first);
            rank.insert(first, 0);
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

                let deg = in_degree.get_mut(&next).unwrap();
                *deg -= 1;
                if *deg == 0 {
                    queue.push_back(next);
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

    // Iterative sweeps (12 iterations)
    for _iter in 0..12 {
        // Downward sweep
        for r in 1..=max_rank {
            let _prev_rank = &ordered[r - 1];
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
            let _next_rank = &ordered[r + 1];
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

        rank_offset += max_size + NODE_SPACING_V;
    }

    // Place nodes
    for (rank_idx, rank_nodes) in ordered_ranks.iter().enumerate() {
        // Compute total cross-rank size for centering
        let total_cross: f64 = rank_nodes
            .iter()
            .filter_map(|&node| graph.graph[node].box_)
            .map(|rect| if is_horizontal { rect.height } else { rect.width })
            .sum::<f64>()
            + (rank_nodes.len().saturating_sub(1) as f64) * NODE_SPACING_H;

        let mut cross_offset = -total_cross / 2.0;

        for &node in rank_nodes {
            if let Some(ref mut rect) = graph.graph[node].box_ {
                match direction {
                    Direction::Down => {
                        rect.x = cross_offset;
                        rect.y = rank_offsets[rank_idx];
                        cross_offset += rect.width + NODE_SPACING_H;
                    }
                    Direction::Up => {
                        rect.x = cross_offset;
                        rect.y = -(rank_offsets[rank_idx] + rect.height);
                        cross_offset += rect.width + NODE_SPACING_H;
                    }
                    Direction::Right => {
                        rect.x = rank_offsets[rank_idx];
                        rect.y = cross_offset;
                        cross_offset += rect.height + NODE_SPACING_H;
                    }
                    Direction::Left => {
                        rect.x = -(rank_offsets[rank_idx] + rect.width);
                        rect.y = cross_offset;
                        cross_offset += rect.height + NODE_SPACING_H;
                    }
                }
            }
        }
    }
}
