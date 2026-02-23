//! AST → D2Graph compilation.
//!
//! The compiler walks the AST and builds the D2Graph:
//! 1. Collect declarations: walk all Key nodes, build object/edge lists
//! 2. Resolve hierarchy: dotted keys create parent-child relationships
//! 3. Apply reserved keywords: shape, label, style.*, direction, width, height
//! 4. Resolve named colors to hex during style processing
//! 5. Validate shape types, style values, connection endpoints
//! 6. Set defaults: Rectangle shape, Style fields left as None
//! 7. Collect warnings for recognized but deferred keywords

use std::collections::HashMap;

use petgraph::stable_graph::NodeIndex;

use crate::ast::*;
use crate::graph::*;
use crate::keywords;

/// Compilation failure.
#[derive(Debug, thiserror::Error)]
#[error("{message}")]
pub struct CompileError {
    pub message: String,
    pub range: Option<Range>,
}

/// Compile a D2 AST into a graph IR.
pub fn compile(ast: &D2Map) -> Result<D2Graph, CompileError> {
    let mut ctx = CompileContext::new();
    ctx.compile_map(ast, ctx.graph.root)?;
    Ok(ctx.graph)
}

// ---------------------------------------------------------------------------
// Compilation context
// ---------------------------------------------------------------------------

struct CompileContext {
    graph: D2Graph,
    /// Map from fully-qualified ID to NodeIndex.
    id_to_node: HashMap<String, NodeIndex>,
}

impl CompileContext {
    fn new() -> Self {
        CompileContext {
            graph: D2Graph::new(),
            id_to_node: HashMap::new(),
        }
    }

    /// Compile all nodes in a map under the given parent.
    fn compile_map(&mut self, map: &D2Map, parent: NodeIndex) -> Result<(), CompileError> {
        for node in &map.nodes {
            match node {
                MapNode::Comment(_) => {} // skip
                MapNode::Key(key) => {
                    self.compile_key(key, parent)?;
                }
            }
        }
        Ok(())
    }

    /// Compile a key declaration.
    fn compile_key(&mut self, key: &Key, parent: NodeIndex) -> Result<(), CompileError> {
        // Handle edges
        if !key.edges.is_empty() {
            self.compile_edges(key, parent)?;
            return Ok(());
        }

        let path = &key.key;
        let segments: Vec<&str> = path.segments.iter().map(|s| s.value.as_str()).collect();

        if segments.is_empty() {
            return Ok(());
        }

        // Check for top-level direction keyword
        if segments.len() == 1 && segments[0] == "direction" {
            if let Some(ref primary) = key.primary {
                let val = primary.as_str();
                if let Some(dir) = Direction::from_str(&val) {
                    // Set direction on parent (or graph-level)
                    if parent == self.graph.root {
                        self.graph.direction = dir;
                    } else {
                        self.graph.graph[parent].direction = Some(dir);
                    }
                }
            }
            return Ok(());
        }

        // Check for reserved/deferred keyword at top level
        if segments.len() == 1 && keywords::is_deferred(segments[0]) {
            self.graph.warnings.push(D2Warning {
                message: format!("'{}' is not yet supported in v1", segments[0]),
                range: Some(key.range),
            });
            return Ok(());
        }

        // Handle single-segment reserved keywords inside containers.
        // When inside a container map, `shape: circle` should set the container's
        // shape, not create a child named "shape".
        if segments.len() == 1 && parent != self.graph.root {
            let kw = segments[0];
            if kw == "shape" {
                if let Some(ref primary) = key.primary {
                    let val = primary.as_str();
                    if let Some(shape) = ShapeType::from_str(&val) {
                        self.graph.graph[parent].shape = shape;
                    } else {
                        return Err(CompileError {
                            message: format!("unknown shape type: '{}'", val),
                            range: Some(key.range),
                        });
                    }
                }
                return Ok(());
            }
            if kw == "label" {
                if let Some(ref primary) = key.primary {
                    self.graph.graph[parent].label = primary.as_str();
                }
                return Ok(());
            }
            if kw == "near" {
                self.graph.warnings.push(D2Warning {
                    message: "'near' has no layout effect in v1".to_string(),
                    range: Some(key.range),
                });
                return Ok(());
            }
            // source-arrowhead / target-arrowhead are only valid on edges, skip here
        }

        // Handle style sub-properties inside containers: `style.fill: red`
        if segments.len() == 2 && segments[0] == "style" && parent != self.graph.root {
            if let Some(ref primary) = key.primary {
                self.apply_style_property(parent, segments[1], primary, key.range)?;
            }
            return Ok(());
        }

        // Check for property assignments BEFORE creating nodes
        // (otherwise "x.shape: circle" would create a spurious "x.shape" node)
        if segments.len() >= 2 {
            let prop = segments[segments.len() - 1];
            let obj_segments = &segments[..segments.len() - 1];

            // Check if this is a property assignment on an existing or new object
            if prop == "shape" {
                let obj = self.resolve_or_create_path(obj_segments, parent)?;
                if let Some(ref primary) = key.primary {
                    let val = primary.as_str();
                    if let Some(shape) = ShapeType::from_str(&val) {
                        self.graph.graph[obj].shape = shape;
                    } else {
                        return Err(CompileError {
                            message: format!("unknown shape type: '{}'", val),
                            range: Some(key.range),
                        });
                    }
                }
                return Ok(());
            }

            if prop == "label" {
                let obj = self.resolve_or_create_path(obj_segments, parent)?;
                if let Some(ref primary) = key.primary {
                    self.graph.graph[obj].label = primary.as_str();
                }
                return Ok(());
            }

            if prop == "width" {
                let obj = self.resolve_or_create_path(obj_segments, parent)?;
                if let Some(ref primary) = key.primary {
                    if let ScalarValue::Number(n) = primary {
                        let rect = self.graph.graph[obj]
                            .box_
                            .get_or_insert(crate::geo::Rect::new(0.0, 0.0, 0.0, 0.0));
                        rect.width = *n;
                    }
                }
                return Ok(());
            }

            if prop == "height" {
                let obj = self.resolve_or_create_path(obj_segments, parent)?;
                if let Some(ref primary) = key.primary {
                    if let ScalarValue::Number(n) = primary {
                        let rect = self.graph.graph[obj]
                            .box_
                            .get_or_insert(crate::geo::Rect::new(0.0, 0.0, 0.0, 0.0));
                        rect.height = *n;
                    }
                }
                return Ok(());
            }

            if prop == "direction" {
                let obj = self.resolve_or_create_path(obj_segments, parent)?;
                if let Some(ref primary) = key.primary {
                    let val = primary.as_str();
                    if let Some(dir) = Direction::from_str(&val) {
                        self.graph.graph[obj].direction = Some(dir);
                    }
                }
                return Ok(());
            }

            // Check for style properties: x.style.fill, x.style.stroke, etc.
            if segments.len() >= 3 && segments[segments.len() - 2] == "style" {
                let style_prop = prop;
                let obj_segments = &segments[..segments.len() - 2];
                let obj = self.resolve_or_create_path(obj_segments, parent)?;

                if let Some(ref primary) = key.primary {
                    self.apply_style_property(obj, style_prop, primary, key.range)?;
                }
                return Ok(());
            }

            // Check for deferred keyword as property
            if keywords::is_deferred(prop) {
                self.graph.warnings.push(D2Warning {
                    message: format!("'{}' is not yet supported in v1", prop),
                    range: Some(key.range),
                });
                return Ok(());
            }

            if prop == "near" {
                // Parsed but no layout effect in v1
                self.graph.warnings.push(D2Warning {
                    message: "'near' has no layout effect in v1".to_string(),
                    range: Some(key.range),
                });
                return Ok(());
            }
        }

        // Not a property assignment — resolve or create the object hierarchy
        let target = self.resolve_or_create_path(&segments, parent)?;

        // Apply label from primary value
        if let Some(ref primary) = key.primary {
            self.graph.graph[target].label = primary.as_str();
        }

        // Process nested map value.
        // Only mark as container if the map actually creates child objects.
        // Property-only maps like `{shape: person}` or `{style.fill: red}`
        // don't make the node a container — they're just attribute blocks.
        if let Some(Value::Map(ref map)) = key.value {
            self.compile_map(map, target)?;
            if !self.graph.graph[target].children.is_empty() {
                self.graph.graph[target].is_container = true;
            }
        }

        Ok(())
    }

    /// Compile edge declarations.
    fn compile_edges(&mut self, key: &Key, parent: NodeIndex) -> Result<(), CompileError> {
        let edge_count = key.edges.len();
        for (i, edge) in key.edges.iter().enumerate() {
            let src_segments: Vec<&str> =
                edge.src.segments.iter().map(|s| s.value.as_str()).collect();
            let dst_segments: Vec<&str> =
                edge.dst.segments.iter().map(|s| s.value.as_str()).collect();

            let src_node = self.resolve_or_create_path(&src_segments, parent)?;
            let dst_node = self.resolve_or_create_path(&dst_segments, parent)?;

            let src_arrow = if edge.src_arrow {
                ArrowheadType::Arrow
            } else {
                ArrowheadType::None
            };
            let dst_arrow = if edge.dst_arrow {
                ArrowheadType::Arrow
            } else {
                ArrowheadType::None
            };

            // In D2, a connection chain label belongs only to the last edge
            let label = if i == edge_count - 1 {
                key.primary.as_ref().map(|p| p.as_str())
            } else {
                None
            };

            let edge_data = D2EdgeData {
                label,
                src_arrow,
                dst_arrow,
                style: Style::default(),
                route_type: RouteType::Bezier,
                route: Vec::new(),
                label_position: None,
                label_width: 0.0,
                label_height: 0.0,
            };

            let edge_idx = self.graph.graph.add_edge(src_node, dst_node, edge_data);
            self.graph.edges.push(edge_idx);
        }

        // Handle edge properties if this is an edge reference: (x -> y)[0].style.stroke
        if let Some(ref edge_key) = key.edge_key {
            if let Some(ref_index) = key.edge_index {
                let edge_key_segments: Vec<&str> =
                    edge_key.segments.iter().map(|s| s.value.as_str()).collect();
                if edge_key_segments.len() >= 2 && edge_key_segments[0] == "style" {
                    // Find the matching edge by source/destination and index.
                    // Edges in this key were just added; count matching pairs.
                    if !key.edges.is_empty() {
                        let first_edge = &key.edges[0];
                        let src_segs: Vec<&str> = first_edge
                            .src
                            .segments
                            .iter()
                            .map(|s| s.value.as_str())
                            .collect();
                        let dst_segs: Vec<&str> = first_edge
                            .dst
                            .segments
                            .iter()
                            .map(|s| s.value.as_str())
                            .collect();
                        let src_node = self.resolve_or_create_path(&src_segs, parent)?;
                        let dst_node = self.resolve_or_create_path(&dst_segs, parent)?;

                        // Find the nth edge between src and dst
                        let mut count = 0usize;
                        let mut target_edge = None;
                        for &eidx in &self.graph.edges {
                            if let Some((s, d)) = self.graph.graph.edge_endpoints(eidx) {
                                if s == src_node && d == dst_node {
                                    if count == ref_index {
                                        target_edge = Some(eidx);
                                        break;
                                    }
                                    count += 1;
                                }
                            }
                        }

                        if let Some(target) = target_edge {
                            if let Some(ref primary) = key.primary {
                                self.apply_edge_style_property(
                                    target,
                                    edge_key_segments[1],
                                    primary,
                                    key.range,
                                )?;
                            }
                        }
                    }
                }
            }
        }

        // Handle nested map on edges (for arrowhead configuration)
        if let Some(Value::Map(ref map)) = key.value {
            if let Some(&last_edge) = self.graph.edges.last() {
                self.compile_edge_map(map, last_edge)?;
            }
        }

        Ok(())
    }

    /// Compile a map nested under an edge (e.g., arrowhead configuration).
    fn compile_edge_map(
        &mut self,
        map: &D2Map,
        edge_idx: petgraph::stable_graph::EdgeIndex,
    ) -> Result<(), CompileError> {
        for node in &map.nodes {
            if let MapNode::Key(key) = node {
                let segments: Vec<&str> =
                    key.key.segments.iter().map(|s| s.value.as_str()).collect();
                if segments.is_empty() {
                    continue;
                }

                match segments[0] {
                    "source-arrowhead" => {
                        if let Some(Value::Map(ref arrowhead_map)) = key.value {
                            for sub_node in &arrowhead_map.nodes {
                                if let MapNode::Key(sub_key) = sub_node {
                                    let sub_segs: Vec<&str> = sub_key
                                        .key
                                        .segments
                                        .iter()
                                        .map(|s| s.value.as_str())
                                        .collect();
                                    if sub_segs.len() == 1 && sub_segs[0] == "shape" {
                                        if let Some(ref primary) = sub_key.primary {
                                            let val = primary.as_str();
                                            if let Some(arrow) = ArrowheadType::from_str(&val) {
                                                self.graph.graph[edge_idx].src_arrow = arrow;
                                            }
                                        }
                                    }
                                }
                            }
                        } else if let Some(ref primary) = key.primary {
                            let val = primary.as_str();
                            if let Some(arrow) = ArrowheadType::from_str(&val) {
                                self.graph.graph[edge_idx].src_arrow = arrow;
                            }
                        }
                    }
                    "target-arrowhead" => {
                        if let Some(Value::Map(ref arrowhead_map)) = key.value {
                            for sub_node in &arrowhead_map.nodes {
                                if let MapNode::Key(sub_key) = sub_node {
                                    let sub_segs: Vec<&str> = sub_key
                                        .key
                                        .segments
                                        .iter()
                                        .map(|s| s.value.as_str())
                                        .collect();
                                    if sub_segs.len() == 1 && sub_segs[0] == "shape" {
                                        if let Some(ref primary) = sub_key.primary {
                                            let val = primary.as_str();
                                            if let Some(arrow) = ArrowheadType::from_str(&val) {
                                                self.graph.graph[edge_idx].dst_arrow = arrow;
                                            }
                                        }
                                    }
                                }
                            }
                        } else if let Some(ref primary) = key.primary {
                            let val = primary.as_str();
                            if let Some(arrow) = ArrowheadType::from_str(&val) {
                                self.graph.graph[edge_idx].dst_arrow = arrow;
                            }
                        }
                    }
                    "style" if segments.len() >= 2 => {
                        if let Some(ref primary) = key.primary {
                            self.apply_edge_style_property(
                                edge_idx,
                                segments[1],
                                primary,
                                key.range,
                            )?;
                        }
                    }
                    _ => {
                        // Edge label as nested map key
                    }
                }
            }
        }
        Ok(())
    }

    /// Resolve or create objects along a dotted path.
    fn resolve_or_create_path(
        &mut self,
        segments: &[&str],
        parent: NodeIndex,
    ) -> Result<NodeIndex, CompileError> {
        let mut current = parent;

        for (i, &segment) in segments.iter().enumerate() {
            // Build the fully qualified ID for this level
            let parent_id = &self.graph.graph[current].id;
            let fq_id = if parent_id.is_empty() {
                segment.to_string()
            } else {
                format!("{}.{}", parent_id, segment)
            };

            if let Some(&existing) = self.id_to_node.get(&fq_id) {
                current = existing;
            } else {
                // Create new object
                let label = segment.to_string();
                let obj = D2Object {
                    id: fq_id.clone(),
                    label,
                    shape: ShapeType::Rectangle,
                    style: Style::default(),
                    parent: Some(current),
                    children: Vec::new(),
                    direction: None,
                    box_: None,
                    label_dimensions: None,
                    label_lines: Vec::new(),
                    is_container: i < segments.len() - 1, // intermediate segments are containers
                };

                let node_idx = self.graph.graph.add_node(obj);
                self.graph.objects.push(node_idx);
                self.graph.graph[current].children.push(node_idx);
                if !self.graph.graph[current].is_container && current != self.graph.root {
                    self.graph.graph[current].is_container = true;
                }
                self.id_to_node.insert(fq_id, node_idx);
                current = node_idx;
            }
        }

        Ok(current)
    }

    /// Apply a style property to an object.
    fn apply_style_property(
        &mut self,
        obj: NodeIndex,
        prop: &str,
        value: &ScalarValue,
        range: Range,
    ) -> Result<(), CompileError> {
        let style = &mut self.graph.graph[obj].style;
        let val_str = value.as_str();

        match prop {
            "fill" => {
                style.fill = Color::parse(&val_str);
            }
            "stroke" => {
                style.stroke = Color::parse(&val_str);
            }
            "stroke-width" => {
                if let Ok(n) = val_str.parse::<f64>() {
                    style.stroke_width = Some(n);
                }
            }
            "stroke-dash" => {
                if let Ok(n) = val_str.parse::<f64>() {
                    style.stroke_dash = Some(n);
                }
            }
            "font-size" => {
                if let Ok(n) = val_str.parse::<f64>() {
                    style.font_size = Some(n);
                }
            }
            "font-color" => {
                style.font_color = Color::parse(&val_str);
            }
            "bold" => {
                style.bold = val_str == "true";
            }
            "italic" => {
                style.italic = val_str == "true";
            }
            "underline" => {
                style.underline = val_str == "true";
            }
            "border-radius" => {
                if let Ok(n) = val_str.parse::<f64>() {
                    style.border_radius = Some(n);
                }
            }
            "opacity" => {
                if let Ok(n) = val_str.parse::<f64>() {
                    style.opacity = Some(n);
                }
            }
            "shadow" => {
                style.shadow = val_str == "true";
            }
            "3d" => {
                style.three_d = val_str == "true";
            }
            "multiple" => {
                style.multiple = val_str == "true";
            }
            "animated" => {
                style.animated = val_str == "true";
            }
            "double-border" => {
                style.double_border = val_str == "true";
            }
            _ => {
                self.graph.warnings.push(D2Warning {
                    message: format!("unknown style property: '{}'", prop),
                    range: Some(range),
                });
            }
        }

        Ok(())
    }

    /// Apply a style property to an edge.
    fn apply_edge_style_property(
        &mut self,
        edge_idx: petgraph::stable_graph::EdgeIndex,
        prop: &str,
        value: &ScalarValue,
        _range: Range,
    ) -> Result<(), CompileError> {
        let style = &mut self.graph.graph[edge_idx].style;
        let val_str = value.as_str();

        match prop {
            "stroke" => {
                style.stroke = Color::parse(&val_str);
            }
            "stroke-width" => {
                if let Ok(n) = val_str.parse::<f64>() {
                    style.stroke_width = Some(n);
                }
            }
            "stroke-dash" => {
                if let Ok(n) = val_str.parse::<f64>() {
                    style.stroke_dash = Some(n);
                }
            }
            "font-size" => {
                if let Ok(n) = val_str.parse::<f64>() {
                    style.font_size = Some(n);
                }
            }
            "font-color" => {
                style.font_color = Color::parse(&val_str);
            }
            "animated" => {
                style.animated = val_str == "true";
            }
            "opacity" => {
                if let Ok(n) = val_str.parse::<f64>() {
                    style.opacity = Some(n);
                }
            }
            _ => {}
        }

        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;

    fn compile_ok(source: &str) -> D2Graph {
        let parse_result = parser::parse(source);
        assert!(
            parse_result.errors.is_empty(),
            "parse errors: {:?}",
            parse_result.errors
        );
        compile(&parse_result.ast).expect("compilation should succeed")
    }

    #[test]
    fn test_empty() {
        let graph = compile_ok("");
        assert!(graph.objects.is_empty());
        assert!(graph.edges.is_empty());
    }

    #[test]
    fn test_single_shape() {
        let graph = compile_ok("server");
        assert_eq!(graph.objects.len(), 1);
        assert_eq!(graph.graph[graph.objects[0]].id, "server");
        assert_eq!(graph.graph[graph.objects[0]].label, "server");
        assert_eq!(graph.graph[graph.objects[0]].shape, ShapeType::Rectangle);
    }

    #[test]
    fn test_shape_with_label() {
        let graph = compile_ok("server: \"My Server\"");
        assert_eq!(graph.graph[graph.objects[0]].label, "My Server");
    }

    #[test]
    fn test_shape_type() {
        let graph = compile_ok("db.shape: cylinder");
        assert_eq!(graph.graph[graph.objects[0]].shape, ShapeType::Cylinder);
    }

    #[test]
    fn test_simple_edge() {
        let graph = compile_ok("a -> b");
        assert_eq!(graph.objects.len(), 2);
        assert_eq!(graph.edges.len(), 1);
        let (src, dst) = graph.graph.edge_endpoints(graph.edges[0]).unwrap();
        assert_eq!(graph.graph[src].id, "a");
        assert_eq!(graph.graph[dst].id, "b");
    }

    #[test]
    fn test_edge_with_label() {
        let graph = compile_ok("a -> b: hello");
        let edge = &graph.graph[graph.edges[0]];
        assert_eq!(edge.label, Some("hello".to_string()));
    }

    #[test]
    fn test_connection_chain() {
        let graph = compile_ok("a -> b -> c");
        assert_eq!(graph.objects.len(), 3);
        assert_eq!(graph.edges.len(), 2);
    }

    #[test]
    fn test_chain_label_on_last_edge() {
        let graph = compile_ok("a -> b -> c: hello");
        assert_eq!(graph.edges.len(), 2);
        // Label belongs only to the last edge (b -> c), not first (a -> b)
        let first = &graph.graph[graph.edges[0]];
        let last = &graph.graph[graph.edges[1]];
        assert_eq!(first.label, None);
        assert_eq!(last.label, Some("hello".to_string()));
    }

    #[test]
    fn test_container() {
        let graph = compile_ok("group: {\n  a\n  b\n}");
        // group + a + b = 3 objects
        assert_eq!(graph.objects.len(), 3);
        let group = graph.objects[0];
        assert!(graph.graph[group].is_container);
        assert_eq!(graph.graph[group].children.len(), 2);
    }

    #[test]
    fn test_dotted_container() {
        let graph = compile_ok("group.server");
        assert_eq!(graph.objects.len(), 2);
        let group = graph.objects[0];
        let server = graph.objects[1];
        assert!(graph.graph[group].is_container);
        assert_eq!(graph.graph[group].id, "group");
        assert_eq!(graph.graph[server].id, "group.server");
    }

    #[test]
    fn test_direction() {
        let graph = compile_ok("direction: right");
        assert_eq!(graph.direction, Direction::Right);
    }

    #[test]
    fn test_style_fill() {
        let graph = compile_ok("x.style.fill: \"#ff0000\"");
        assert_eq!(
            graph.graph[graph.objects[0]].style.fill,
            Some(Color::Hex(255, 0, 0))
        );
    }

    #[test]
    fn test_style_named_color() {
        let graph = compile_ok("x.style.stroke: blue");
        assert_eq!(
            graph.graph[graph.objects[0]].style.stroke,
            Some(Color::Hex(0, 0, 255))
        );
    }

    #[test]
    fn test_style_bold() {
        let graph = compile_ok("x.style.bold: true");
        assert!(graph.graph[graph.objects[0]].style.bold);
    }

    #[test]
    fn test_arrowhead_types() {
        let graph =
            compile_ok("a -> b: {\n  source-arrowhead: diamond\n  target-arrowhead: circle\n}");
        let edge = &graph.graph[graph.edges[0]];
        assert_eq!(edge.src_arrow, ArrowheadType::Diamond);
        assert_eq!(edge.dst_arrow, ArrowheadType::Circle);
    }

    #[test]
    fn test_deferred_keyword_warning() {
        let graph = compile_ok("icon: \"test.png\"");
        assert!(!graph.warnings.is_empty());
        assert!(graph.warnings[0].message.contains("not yet supported"));
    }

    #[test]
    fn test_bidirectional_edge() {
        let graph = compile_ok("a <-> b");
        let edge = &graph.graph[graph.edges[0]];
        assert_eq!(edge.src_arrow, ArrowheadType::Arrow);
        assert_eq!(edge.dst_arrow, ArrowheadType::Arrow);
    }

    #[test]
    fn test_container_with_label() {
        let graph = compile_ok("group: \"My Group\" {\n  a\n}");
        let group = graph.objects[0];
        assert_eq!(graph.graph[group].label, "My Group");
        assert!(graph.graph[group].is_container);
    }

    #[test]
    fn test_multiple_shapes() {
        let graph = compile_ok("a\nb\nc");
        assert_eq!(graph.objects.len(), 3);
    }

    #[test]
    fn test_complex_diagram() {
        let source = r#"
direction: right

api: "API Gateway" {
  shape: hexagon
}
db: "PostgreSQL" {
  shape: cylinder
}

api -> db: queries
"#;
        let graph = compile_ok(source);
        assert_eq!(graph.direction, Direction::Right);
        assert!(graph.objects.len() >= 2);
        assert!(!graph.edges.is_empty());
    }

    #[test]
    fn test_self_edge() {
        let graph = compile_ok("a -> a");
        assert_eq!(
            graph.objects.len(),
            1,
            "self-edge should create exactly 1 node"
        );
        assert_eq!(graph.graph[graph.objects[0]].id, "a");
        assert_eq!(
            graph.edges.len(),
            1,
            "self-edge should create exactly 1 edge"
        );
        let (src, dst) = graph.graph.edge_endpoints(graph.edges[0]).unwrap();
        assert_eq!(
            src, dst,
            "self-edge source and destination must be the same node"
        );
    }

    #[test]
    fn test_style_nested_map() {
        let graph = compile_ok("x: {\n  style.fill: red\n}");
        assert_eq!(graph.objects.len(), 1);
        assert_eq!(graph.graph[graph.objects[0]].id, "x");
        assert!(
            graph.graph[graph.objects[0]].style.fill.is_some(),
            "expected fill to be set via dotted-key style inside container"
        );
    }

    #[test]
    fn test_inline_shape_not_container() {
        // `user: User {shape: person}` should create a Person shape, NOT a container.
        // The `{...}` block here is an attribute map, not a child container.
        let graph = compile_ok("user: User {shape: person}");
        assert_eq!(graph.objects.len(), 1);
        let obj = &graph.graph[graph.objects[0]];
        assert_eq!(obj.id, "user");
        assert_eq!(obj.label, "User");
        assert_eq!(obj.shape, ShapeType::Person);
        assert!(
            !obj.is_container,
            "shape with inline property map should NOT be marked as container"
        );
    }

    #[test]
    fn test_inline_style_not_container() {
        // Property-only map should not create a container
        let graph = compile_ok("x: {\n  style.fill: red\n  shape: diamond\n}");
        let obj = &graph.graph[graph.objects[0]];
        assert_eq!(obj.shape, ShapeType::Diamond);
        assert!(
            !obj.is_container,
            "node with only properties should NOT be a container"
        );
    }

    #[test]
    fn test_container_with_children_and_shape() {
        // A node with both children AND a shape IS a container
        let graph = compile_ok("group: {\n  shape: hexagon\n  a\n  b\n}");
        let group = &graph.graph[graph.objects[0]];
        assert_eq!(group.shape, ShapeType::Hexagon);
        assert!(
            group.is_container,
            "node with children should be a container even if it also has shape"
        );
        assert_eq!(group.children.len(), 2);
    }
}
