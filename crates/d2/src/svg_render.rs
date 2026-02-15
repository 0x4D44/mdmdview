//! SVG output generation for D2 diagrams.
//!
//! Generates SVG strings from positioned D2Graph. The SVG uses a
//! computed viewBox based on the bounding box of all elements plus padding.
//!
//! Z-ordering (back to front):
//! 1. Background rectangle
//! 2. Container fills (outermost first)
//! 3. Shape fills + strokes
//! 4. Edges + arrowheads
//! 5. All labels (topmost — always readable)

use petgraph::stable_graph::NodeIndex;

use crate::edge_routing::{arrowhead_polygon, diamond_arrowhead};
use crate::geo::Point;
use crate::graph::*;
use crate::shapes::{multiple_svg, shadow_svg, shape_svg, three_d_svg};
use crate::theme::Theme;
use crate::RenderOptions;

/// Render a positioned D2Graph to SVG string.
pub fn render(graph: &D2Graph, options: &RenderOptions) -> String {
    let theme = Theme::for_mode(options.dark_mode);
    let mut svg = String::with_capacity(4096);

    // Compute viewBox
    let (vx, vy, vw, vh) = compute_viewbox(graph);

    // SVG header
    svg.push_str(&format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{} {} {} {}\">\n",
        vx, vy, vw, vh
    ));

    // Layer 1: Background
    let bg = theme.background.to_svg_string();
    svg.push_str(&format!(
        "  <rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"{}\"/>\n",
        vx, vy, vw, vh, bg
    ));

    // Layer 2: Container fills (outermost first via depth-first)
    render_container_fills(graph, &theme, &mut svg, graph.root, 0);

    // Layer 3: Shape fills + strokes (non-container nodes)
    render_shapes(graph, &theme, options, &mut svg);

    // Layer 4: Edges + arrowheads
    render_edges(graph, &theme, options, &mut svg);

    // Layer 5: All labels (topmost)
    render_labels(graph, &theme, options, &mut svg);

    svg.push_str("</svg>\n");
    svg
}

/// Compute the viewBox bounding box.
fn compute_viewbox(graph: &D2Graph) -> (f64, f64, f64, f64) {
    let padding = 20.0;
    let mut min_x = f64::INFINITY;
    let mut min_y = f64::INFINITY;
    let mut max_x = f64::NEG_INFINITY;
    let mut max_y = f64::NEG_INFINITY;

    // Include all node bounding boxes
    for &idx in &graph.objects {
        if let Some(rect) = &graph.graph[idx].box_ {
            let extra = if graph.graph[idx].style.shadow { 3.0 } else { 0.0 }
                + if graph.graph[idx].style.multiple {
                    8.0
                } else {
                    0.0
                };
            min_x = min_x.min(rect.x);
            min_y = min_y.min(rect.y);
            max_x = max_x.max(rect.x + rect.width + extra);
            max_y = max_y.max(rect.y + rect.height + extra);
        }
    }

    // Include all edge route control points
    for &idx in &graph.edges {
        let edge = &graph.graph[idx];
        for pt in &edge.route {
            min_x = min_x.min(pt.x);
            min_y = min_y.min(pt.y);
            max_x = max_x.max(pt.x);
            max_y = max_y.max(pt.y);
        }
    }

    if min_x == f64::INFINITY {
        // Empty diagram
        return (0.0, 0.0, 100.0, 100.0);
    }

    (
        min_x - padding,
        min_y - padding,
        (max_x - min_x) + 2.0 * padding,
        (max_y - min_y) + 2.0 * padding,
    )
}

/// Render container fills recursively (outermost first).
fn render_container_fills(
    graph: &D2Graph,
    theme: &Theme,
    svg: &mut String,
    container: NodeIndex,
    depth: usize,
) {
    // Skip root — it has no visual representation
    if container != graph.root {
        let obj = &graph.graph[container];
        if obj.is_container {
            if let Some(rect) = &obj.box_ {
                let fill = obj
                    .style
                    .fill
                    .unwrap_or(theme.container_fill)
                    .to_svg_string();
                let stroke = obj
                    .style
                    .stroke
                    .unwrap_or(theme.node_stroke)
                    .to_svg_string();
                let sw = obj.style.stroke_width.unwrap_or(theme.stroke_width);
                let rx = obj.style.border_radius.unwrap_or(4.0);

                let opacity_attr = obj
                    .style
                    .opacity
                    .map(|o| format!(" opacity=\"{}\"", o))
                    .unwrap_or_default();
                let dash_attr = obj
                    .style
                    .stroke_dash
                    .map(|d| format!(" stroke-dasharray=\"{} {}\"", d, d))
                    .unwrap_or_default();

                svg.push_str(&format!(
                    "  <rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" rx=\"{}\" \
                     fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"{}{}/><!-- container: {} -->\n",
                    rect.x, rect.y, rect.width, rect.height, rx, fill, stroke, sw,
                    opacity_attr, dash_attr, obj.id,
                ));
            }
        }
    }

    // Recurse into children
    let children: Vec<NodeIndex> = graph.graph[container].children.clone();
    for child in children {
        if graph.graph[child].is_container {
            render_container_fills(graph, theme, svg, child, depth + 1);
        }
    }
}

/// Render non-container shape fills and strokes.
fn render_shapes(graph: &D2Graph, theme: &Theme, _options: &RenderOptions, svg: &mut String) {
    for &idx in &graph.objects {
        // Skip root
        if idx == graph.root {
            continue;
        }

        let obj = &graph.graph[idx];

        // Skip containers (rendered in Layer 2)
        if obj.is_container {
            continue;
        }

        let rect = match &obj.box_ {
            Some(r) => *r,
            None => continue,
        };

        let fill = obj
            .style
            .fill
            .unwrap_or(theme.node_fill)
            .to_svg_string();
        let stroke = obj
            .style
            .stroke
            .unwrap_or(theme.node_stroke)
            .to_svg_string();
        let sw = obj.style.stroke_width.unwrap_or(theme.stroke_width);

        // Shadow effect (drawn behind main shape)
        if obj.style.shadow {
            svg.push_str("  ");
            svg.push_str(&shadow_svg(obj.shape, &rect, &fill, sw));
            svg.push('\n');
        }

        // Multiple effect (stacked copies behind main shape)
        if obj.style.multiple {
            svg.push_str("  ");
            svg.push_str(&multiple_svg(
                obj.shape,
                &rect,
                &fill,
                &stroke,
                sw,
                obj.style.border_radius,
            ));
        }

        // Main shape
        svg.push_str("  ");
        svg.push_str(&shape_svg(
            obj.shape,
            &rect,
            &fill,
            &stroke,
            sw,
            obj.style.border_radius,
            obj.style.opacity,
            obj.style.stroke_dash,
            obj.style.double_border,
        ));
        svg.push('\n');

        // 3D effect (drawn on top of main shape, behind labels)
        if obj.style.three_d {
            svg.push_str("  ");
            svg.push_str(&three_d_svg(obj.shape, &rect, &fill, &stroke, sw));
            svg.push('\n');
        }

        svg.push_str(&format!("<!-- {} -->\n", obj.id));
    }
}

/// Render edges and arrowheads.
fn render_edges(graph: &D2Graph, theme: &Theme, _options: &RenderOptions, svg: &mut String) {
    for &eidx in &graph.edges {
        let edge = &graph.graph[eidx];

        if edge.route.len() < 2 {
            continue;
        }

        let stroke = edge
            .style
            .stroke
            .unwrap_or(theme.edge_color)
            .to_svg_string();
        let sw = edge.style.stroke_width.unwrap_or(theme.stroke_width);

        let opacity_attr = edge
            .style
            .opacity
            .map(|o| format!(" opacity=\"{}\"", o))
            .unwrap_or_default();

        // Dash: animated edges or explicit stroke-dash
        let dash_attr = if edge.style.animated {
            format!(" stroke-dasharray=\"{} {}\"", sw * 3.0, sw * 3.0)
        } else {
            edge.style
                .stroke_dash
                .map(|d| format!(" stroke-dasharray=\"{} {}\"", d, d))
                .unwrap_or_default()
        };

        // Build SVG path
        let mut path = String::new();
        path.push_str(&format!("M {} {}", edge.route[0].x, edge.route[0].y));

        // Cubic Bézier segments
        let mut i = 1;
        while i + 2 < edge.route.len() {
            path.push_str(&format!(
                " C {} {} {} {} {} {}",
                edge.route[i].x,
                edge.route[i].y,
                edge.route[i + 1].x,
                edge.route[i + 1].y,
                edge.route[i + 2].x,
                edge.route[i + 2].y,
            ));
            i += 3;
        }

        // If remaining points, add as line segments
        while i < edge.route.len() {
            path.push_str(&format!(" L {} {}", edge.route[i].x, edge.route[i].y));
            i += 1;
        }

        svg.push_str(&format!(
            "  <path d=\"{}\" stroke=\"{}\" fill=\"none\" stroke-width=\"{}\"{}{}/>",
            path, stroke, sw, opacity_attr, dash_attr,
        ));
        svg.push('\n');

        // Source arrowhead
        if edge.src_arrow != ArrowheadType::None && edge.route.len() >= 2 {
            render_arrowhead(svg, &edge.src_arrow, edge.route[0], edge.route[1], &stroke, sw);
        }

        // Destination arrowhead
        if edge.dst_arrow != ArrowheadType::None && edge.route.len() >= 2 {
            let last = edge.route.len() - 1;
            render_arrowhead(
                svg,
                &edge.dst_arrow,
                edge.route[last],
                edge.route[last - 1],
                &stroke,
                sw,
            );
        }
    }
}

/// Render a single arrowhead.
fn render_arrowhead(
    svg: &mut String,
    arrow_type: &ArrowheadType,
    tip: Point,
    from: Point,
    color: &str,
    stroke_width: f64,
) {
    let size = stroke_width * 5.0;

    match arrow_type {
        ArrowheadType::Arrow | ArrowheadType::Triangle => {
            let (t, l, r) = arrowhead_polygon(tip, from, size);
            svg.push_str(&format!(
                "  <polygon points=\"{},{} {},{} {},{}\" fill=\"{}\"/>\n",
                t.x, t.y, l.x, l.y, r.x, r.y, color,
            ));
        }
        ArrowheadType::Diamond | ArrowheadType::FilledDiamond => {
            let pts = diamond_arrowhead(tip, from, size);
            let fill = if *arrow_type == ArrowheadType::FilledDiamond {
                color
            } else {
                "none"
            };
            svg.push_str(&format!(
                "  <polygon points=\"{},{} {},{} {},{} {},{}\" fill=\"{}\" stroke=\"{}\" stroke-width=\"1\"/>\n",
                pts[0].x, pts[0].y, pts[1].x, pts[1].y, pts[2].x, pts[2].y, pts[3].x, pts[3].y,
                fill, color,
            ));
        }
        ArrowheadType::Circle | ArrowheadType::FilledCircle => {
            let r = size * 0.4;
            let dx = tip.x - from.x;
            let dy = tip.y - from.y;
            let len = (dx * dx + dy * dy).sqrt();
            let (cx, cy) = if len > 1e-6 {
                let ux = dx / len;
                let uy = dy / len;
                (tip.x - ux * r, tip.y - uy * r)
            } else {
                (tip.x, tip.y - r)
            };
            let fill = if *arrow_type == ArrowheadType::FilledCircle {
                color
            } else {
                "none"
            };
            svg.push_str(&format!(
                "  <circle cx=\"{}\" cy=\"{}\" r=\"{}\" fill=\"{}\" stroke=\"{}\" stroke-width=\"1\"/>\n",
                cx, cy, r, fill, color,
            ));
        }
        ArrowheadType::CfOne | ArrowheadType::CfOneRequired => {
            // Crow's foot: single line perpendicular at tip
            let dx = tip.x - from.x;
            let dy = tip.y - from.y;
            let len = (dx * dx + dy * dy).sqrt();
            if len > 1e-6 {
                let ux = dx / len;
                let uy = dy / len;
                let w = size * 0.5;
                svg.push_str(&format!(
                    "  <line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>\n",
                    tip.x - uy * w, tip.y + ux * w,
                    tip.x + uy * w, tip.y - ux * w,
                    color, stroke_width,
                ));
                if *arrow_type == ArrowheadType::CfOneRequired {
                    let offset = size * 0.3;
                    let bx = tip.x - ux * offset;
                    let by = tip.y - uy * offset;
                    svg.push_str(&format!(
                        "  <line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>\n",
                        bx - uy * w, by + ux * w,
                        bx + uy * w, by - ux * w,
                        color, stroke_width,
                    ));
                }
            }
        }
        ArrowheadType::CfMany | ArrowheadType::CfManyRequired => {
            // Crow's foot: fork into 2-3 tines
            let dx = tip.x - from.x;
            let dy = tip.y - from.y;
            let len = (dx * dx + dy * dy).sqrt();
            if len > 1e-6 {
                let ux = dx / len;
                let uy = dy / len;
                let w = size * 0.5;
                let base = Point::new(tip.x - ux * size, tip.y - uy * size);
                // Three tines from base to tip area
                svg.push_str(&format!(
                    "  <line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>\n",
                    base.x, base.y, tip.x - uy * w, tip.y + ux * w, color, stroke_width,
                ));
                svg.push_str(&format!(
                    "  <line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>\n",
                    base.x, base.y, tip.x, tip.y, color, stroke_width,
                ));
                svg.push_str(&format!(
                    "  <line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>\n",
                    base.x, base.y, tip.x + uy * w, tip.y - ux * w, color, stroke_width,
                ));
                if *arrow_type == ArrowheadType::CfManyRequired {
                    let offset = size * 0.3;
                    let bx = tip.x - ux * (size + offset);
                    let by = tip.y - uy * (size + offset);
                    svg.push_str(&format!(
                        "  <line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>\n",
                        bx - uy * w, by + ux * w,
                        bx + uy * w, by - ux * w,
                        color, stroke_width,
                    ));
                }
            }
        }
        ArrowheadType::None => {}
    }
}

/// Render all labels (topmost layer).
fn render_labels(graph: &D2Graph, theme: &Theme, options: &RenderOptions, svg: &mut String) {
    let font_family = &options.font_family;
    let bg_color = theme.background.to_svg_string();

    // Container labels (top-left aligned)
    for &idx in &graph.objects {
        if idx == graph.root {
            continue;
        }

        let obj = &graph.graph[idx];
        if !obj.is_container || obj.label.is_empty() {
            continue;
        }

        let rect = match &obj.box_ {
            Some(r) => *r,
            None => continue,
        };

        let font_color = obj
            .style
            .font_color
            .unwrap_or(theme.font_color)
            .to_svg_string();
        let font_size = obj.style.font_size.unwrap_or(theme.font_size);
        let font_weight = if obj.style.bold { "bold" } else { "normal" };
        let font_style = if obj.style.italic { "italic" } else { "normal" };

        let text_x = rect.x + 8.0;
        let text_y = rect.y + font_size + 4.0;

        let decoration = if obj.style.underline {
            " text-decoration=\"underline\""
        } else {
            ""
        };

        svg.push_str(&format!(
            "  <text x=\"{}\" y=\"{}\" font-family=\"{}\" font-size=\"{}\" \
             fill=\"{}\" font-weight=\"{}\" font-style=\"{}\" \
             dominant-baseline=\"auto\"{}>{}</text>\n",
            text_x,
            text_y,
            font_family,
            font_size,
            font_color,
            font_weight,
            font_style,
            decoration,
            xml_escape(&obj.label),
        ));
    }

    // Edge labels with background halo
    for &eidx in &graph.edges {
        let edge = &graph.graph[eidx];
        let label = match &edge.label {
            Some(l) if !l.is_empty() => l,
            _ => continue,
        };

        let pos = match &edge.label_position {
            Some(p) => *p,
            None => continue,
        };

        let font_color = edge
            .style
            .font_color
            .unwrap_or(theme.font_color)
            .to_svg_string();
        let font_size = edge.style.font_size.unwrap_or(theme.font_size);

        // Estimate label dimensions for halo
        let char_width = font_size * 0.55;
        let label_width = label.chars().count() as f64 * char_width;
        let label_height = font_size * 1.2;
        let pad = 3.0;

        // Background halo
        svg.push_str(&format!(
            "  <rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" \
             fill=\"{}\" opacity=\"0.85\" rx=\"2\"/>\n",
            pos.x - label_width / 2.0 - pad,
            pos.y - label_height / 2.0 - pad,
            label_width + pad * 2.0,
            label_height + pad * 2.0,
            bg_color,
        ));

        // Label text
        svg.push_str(&format!(
            "  <text x=\"{}\" y=\"{}\" font-family=\"{}\" font-size=\"{}\" \
             fill=\"{}\" text-anchor=\"middle\" dominant-baseline=\"central\">{}</text>\n",
            pos.x,
            pos.y,
            font_family,
            font_size,
            font_color,
            xml_escape(label),
        ));
    }

    // Node labels (centered on shape)
    for &idx in &graph.objects {
        if idx == graph.root {
            continue;
        }

        let obj = &graph.graph[idx];
        if obj.is_container || obj.label.is_empty() {
            continue;
        }

        let rect = match &obj.box_ {
            Some(r) => *r,
            None => continue,
        };

        let font_color = obj
            .style
            .font_color
            .unwrap_or(theme.font_color)
            .to_svg_string();
        let font_size = obj.style.font_size.unwrap_or(theme.font_size);
        let font_weight = if obj.style.bold { "bold" } else { "normal" };
        let font_style = if obj.style.italic { "italic" } else { "normal" };

        let cx = rect.x + rect.width / 2.0;
        let cy = rect.y + rect.height / 2.0;

        let decoration = if obj.style.underline {
            " text-decoration=\"underline\""
        } else {
            ""
        };

        if obj.label_lines.len() <= 1 {
            svg.push_str(&format!(
                "  <text x=\"{}\" y=\"{}\" font-family=\"{}\" font-size=\"{}\" \
                 fill=\"{}\" font-weight=\"{}\" font-style=\"{}\" \
                 text-anchor=\"middle\" dominant-baseline=\"central\"{}>{}</text>\n",
                cx,
                cy,
                font_family,
                font_size,
                font_color,
                font_weight,
                font_style,
                decoration,
                xml_escape(&obj.label),
            ));
        } else {
            // Multi-line label using tspan
            let line_height = font_size * 1.4;
            let total_height = obj.label_lines.len() as f64 * line_height;
            let start_y = cy - total_height / 2.0 + line_height / 2.0;

            svg.push_str(&format!(
                "  <text x=\"{}\" font-family=\"{}\" font-size=\"{}\" \
                 fill=\"{}\" font-weight=\"{}\" font-style=\"{}\" \
                 text-anchor=\"middle\" dominant-baseline=\"central\"{}>\n",
                cx, font_family, font_size, font_color, font_weight, font_style, decoration,
            ));

            for (i, line) in obj.label_lines.iter().enumerate() {
                let y = start_y + i as f64 * line_height;
                svg.push_str(&format!(
                    "    <tspan x=\"{}\" y=\"{}\">{}</tspan>\n",
                    cx,
                    y,
                    xml_escape(line),
                ));
            }

            svg.push_str("  </text>\n");
        }
    }
}

/// XML-escape a string for safe inclusion in SVG.
fn xml_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_xml_escape() {
        assert_eq!(xml_escape("a < b & c"), "a &lt; b &amp; c");
        assert_eq!(xml_escape("\"hello\""), "&quot;hello&quot;");
    }

    #[test]
    fn test_empty_diagram_viewbox() {
        let graph = D2Graph::new();
        let (_, _, w, h) = compute_viewbox(&graph);
        assert!(w > 0.0);
        assert!(h > 0.0);
    }

    #[test]
    fn test_render_empty_diagram() {
        let graph = D2Graph::new();
        let options = RenderOptions::default();
        let svg = render(&graph, &options);
        assert!(svg.contains("<svg"));
        assert!(svg.contains("</svg>"));
        assert!(svg.contains("<rect")); // background
    }
}
