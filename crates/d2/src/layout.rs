//! Layout orchestration for D2 diagrams.
//!
//! Coordinates the four layout sub-phases:
//! 1. Text measurement (compute node dimensions)
//! 2. Sugiyama node positioning (recursive, bottom-up)
//! 3. Container fitting
//! 4. Edge routing (last, after all positions finalized)

use petgraph::stable_graph::NodeIndex;

use crate::geo::Rect;
use crate::graph::{D2Graph, Direction};
use crate::layout_sugiyama;
use crate::edge_routing;
use crate::text;
use crate::RenderOptions;

/// Layout failure.
#[derive(Debug, thiserror::Error)]
#[error("layout: {0}")]
pub struct LayoutError(pub String);

/// Container padding (all sides).
const CONTAINER_PADDING: f64 = 30.0;
/// Container label height offset.
const CONTAINER_LABEL_HEIGHT: f64 = 24.0;

/// Perform layout on the graph.
/// Modifies graph in-place: sets node positions, edge routes,
/// and may append to graph.warnings.
pub fn layout(graph: &mut D2Graph, options: &RenderOptions) -> Result<(), LayoutError> {
    if graph.objects.is_empty() {
        return Ok(());
    }

    let theme = crate::theme::Theme::for_mode(options.dark_mode);
    let font_size = theme.font_size;
    let font_family = &options.font_family;

    // Phase 3: Text measurement — compute node dimensions
    measure_all_nodes(graph, font_size, font_family);

    // Phase 4a: Sugiyama node positioning (recursive, bottom-up through containers)
    layout_recursive(graph, graph.root, graph.direction)?;

    // Phase 4c: Container fitting
    fit_containers(graph, graph.root);

    // Phase 4d: Post-layout adjustments
    normalize_positions(graph);

    // Phase 4b: Edge routing (last!)
    edge_routing::route_all_edges(graph);

    Ok(())
}

/// Measure all nodes and set their dimensions.
fn measure_all_nodes(graph: &mut D2Graph, font_size: f64, font_family: &str) {
    let objects: Vec<NodeIndex> = graph.objects.clone();
    for &idx in &objects {
        let obj = &graph.graph[idx];
        let effective_font_size = obj.style.font_size.unwrap_or(font_size);

        // Get user-specified dimensions if any
        let user_width = obj.box_.map(|r| r.width).filter(|&w| w > 0.0);
        let user_height = obj.box_.map(|r| r.height).filter(|&h| h > 0.0);

        let measurement = text::compute_node_dimensions(
            &obj.label,
            effective_font_size,
            font_family,
            obj.shape,
            user_width,
            user_height,
        );

        let obj = &mut graph.graph[idx];
        obj.label_lines = measurement.lines;
        let (h_pad, v_pad) = text::shape_padding(obj.shape);
        obj.label_dimensions = Some((
            measurement.width - h_pad * 2.0,
            measurement.height - v_pad * 2.0,
        ));
        obj.box_ = Some(Rect::new(0.0, 0.0, measurement.width, measurement.height));
    }
}

/// Recursively lay out containers bottom-up.
fn layout_recursive(
    graph: &mut D2Graph,
    container: NodeIndex,
    parent_direction: Direction,
) -> Result<(), LayoutError> {
    let children: Vec<NodeIndex> = graph.graph[container].children.clone();

    if children.is_empty() {
        return Ok(());
    }

    // Determine effective direction for this container
    let direction = graph.graph[container]
        .direction
        .unwrap_or(parent_direction);

    // First, recursively lay out any child containers
    for &child in &children {
        if graph.graph[child].is_container {
            layout_recursive(graph, child, direction)?;
        }
    }

    // Now lay out this container's direct children using Sugiyama
    layout_sugiyama::layout_children(graph, container, direction)?;

    // Size the container to fit its children
    if container != graph.root {
        fit_container(graph, container);
    }

    Ok(())
}

/// Fit a container to its children's bounding box.
/// Offsets children downward if the container has a label, to avoid overlap.
fn fit_container(graph: &mut D2Graph, container: NodeIndex) {
    let children: Vec<NodeIndex> = graph.graph[container].children.clone();
    if children.is_empty() {
        return;
    }

    // Offset children to make room for container label at the top
    let label_height = if !graph.graph[container].label.is_empty() {
        CONTAINER_LABEL_HEIGHT
    } else {
        0.0
    };

    if label_height > 0.0 {
        offset_subtree(graph, &children, 0.0, label_height);
    }

    let mut min_x = f64::INFINITY;
    let mut min_y = f64::INFINITY;
    let mut max_x = f64::NEG_INFINITY;
    let mut max_y = f64::NEG_INFINITY;

    for &child in &children {
        if let Some(rect) = graph.graph[child].box_ {
            min_x = min_x.min(rect.x);
            min_y = min_y.min(rect.y);
            max_x = max_x.max(rect.x + rect.width);
            max_y = max_y.max(rect.y + rect.height);
        }
    }

    if min_x == f64::INFINITY {
        return;
    }

    // Container bounds: children bbox + padding + label area at top
    let container_x = min_x - CONTAINER_PADDING;
    let container_y = min_y - CONTAINER_PADDING - label_height;
    let container_width = (max_x - min_x) + CONTAINER_PADDING * 2.0;
    let container_height = (max_y - min_y) + CONTAINER_PADDING * 2.0 + label_height;

    graph.graph[container].box_ = Some(Rect::new(
        container_x,
        container_y,
        container_width,
        container_height,
    ));
}

/// Recursively offset all nodes in a subtree by (dx, dy).
fn offset_subtree(graph: &mut D2Graph, nodes: &[NodeIndex], dx: f64, dy: f64) {
    for &node in nodes {
        if let Some(ref mut rect) = graph.graph[node].box_ {
            rect.x += dx;
            rect.y += dy;
        }
        // Also offset grandchildren
        let grandchildren: Vec<NodeIndex> = graph.graph[node].children.clone();
        if !grandchildren.is_empty() {
            offset_subtree(graph, &grandchildren, dx, dy);
        }
    }
}

/// Fit all containers after layout is complete.
fn fit_containers(graph: &mut D2Graph, container: NodeIndex) {
    let children: Vec<NodeIndex> = graph.graph[container].children.clone();
    for &child in &children {
        if graph.graph[child].is_container {
            fit_containers(graph, child);
            fit_container(graph, child);
        }
    }

    if container != graph.root {
        fit_container(graph, container);
    } else {
        // Root: just compute bounding box
        let all_objects: Vec<NodeIndex> = graph.objects.clone();
        let mut min_x = f64::INFINITY;
        let mut min_y = f64::INFINITY;
        let mut max_x = f64::NEG_INFINITY;
        let mut max_y = f64::NEG_INFINITY;

        for &idx in &all_objects {
            if let Some(rect) = graph.graph[idx].box_ {
                min_x = min_x.min(rect.x);
                min_y = min_y.min(rect.y);
                max_x = max_x.max(rect.x + rect.width);
                max_y = max_y.max(rect.y + rect.height);
            }
        }

        if min_x != f64::INFINITY {
            graph.graph[container].box_ = Some(Rect::new(
                min_x,
                min_y,
                max_x - min_x,
                max_y - min_y,
            ));
        }
    }
}

/// Normalize positions so the diagram starts near origin.
fn normalize_positions(graph: &mut D2Graph) {
    let objects: Vec<NodeIndex> = graph.objects.clone();

    let mut min_x = f64::INFINITY;
    let mut min_y = f64::INFINITY;

    for &idx in &objects {
        if let Some(rect) = graph.graph[idx].box_ {
            min_x = min_x.min(rect.x);
            min_y = min_y.min(rect.y);
        }
    }

    if min_x == f64::INFINITY {
        return;
    }

    // Offset everything so top-left is near (0, 0)
    let offset_x = -min_x;
    let offset_y = -min_y;

    if offset_x.abs() < 0.01 && offset_y.abs() < 0.01 {
        return;
    }

    for &idx in &objects {
        if let Some(ref mut rect) = graph.graph[idx].box_ {
            rect.x += offset_x;
            rect.y += offset_y;
        }
    }

    // Also update root bounding box
    if let Some(ref mut rect) = graph.graph[graph.root].box_ {
        rect.x += offset_x;
        rect.y += offset_y;
    }
}
