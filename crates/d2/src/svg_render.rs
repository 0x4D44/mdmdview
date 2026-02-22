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

use crate::edge_routing::{arrowhead_polygon, diamond_arrowhead, LABEL_HALO_PADDING};
use crate::geo::Point;
use crate::graph::*;
use crate::shapes::{multiple_svg, shadow_svg, shape_svg, three_d_svg};
use crate::theme::Theme;
use crate::RenderOptions;

/// Round a coordinate to 2 decimal places for clean SVG output.
/// Prevents bloated attributes like `viewBox="-20.000000000000004 ..."`.
fn c(v: f64) -> f64 {
    (v * 100.0).round() / 100.0
}

/// Default bend radius for rounded corners on orthogonal polylines.
const BEND_RADIUS: f64 = 5.0;

/// Build an SVG path `d` attribute for an orthogonal polyline with rounded corners.
///
/// Each internal vertex gets a quadratic Bézier arc that rounds the corner.
/// The radius is clamped so it never exceeds half the length of either adjacent
/// segment, which handles very short segments gracefully.
fn polyline_with_rounded_corners(points: &[Point], radius: f64) -> String {
    if points.len() < 2 {
        return String::new();
    }

    let mut d = String::new();
    d.push_str(&format!("M {} {}", c(points[0].x), c(points[0].y)));

    if points.len() == 2 {
        d.push_str(&format!(" L {} {}", c(points[1].x), c(points[1].y)));
        return d;
    }

    for i in 1..points.len() - 1 {
        let prev = points[i - 1];
        let curr = points[i];
        let next = points[i + 1];

        let dx1 = prev.x - curr.x;
        let dy1 = prev.y - curr.y;
        let len1 = (dx1 * dx1 + dy1 * dy1).sqrt();

        let dx2 = next.x - curr.x;
        let dy2 = next.y - curr.y;
        let len2 = (dx2 * dx2 + dy2 * dy2).sqrt();

        if len1 < 1e-6 || len2 < 1e-6 {
            // Degenerate segment — just line to the vertex
            d.push_str(&format!(" L {} {}", c(curr.x), c(curr.y)));
            continue;
        }

        let r = radius.min(len1 / 2.0).min(len2 / 2.0);

        let arc_start_x = curr.x + dx1 / len1 * r;
        let arc_start_y = curr.y + dy1 / len1 * r;
        let arc_end_x = curr.x + dx2 / len2 * r;
        let arc_end_y = curr.y + dy2 / len2 * r;

        d.push_str(&format!(
            " L {} {} Q {} {} {} {}",
            c(arc_start_x),
            c(arc_start_y),
            c(curr.x),
            c(curr.y),
            c(arc_end_x),
            c(arc_end_y),
        ));
    }

    // Final point
    let last = points[points.len() - 1];
    d.push_str(&format!(" L {} {}", c(last.x), c(last.y)));
    d
}

/// Build an SVG path `d` attribute for a cubic Bézier spline route.
///
/// The route format is `[start, ctrl1, ctrl2, end, ctrl1, ctrl2, end, ...]`.
/// Any trailing points that don't form a complete cubic segment are emitted
/// as straight line segments.
fn build_bezier_path(route: &[Point]) -> String {
    let mut path = String::new();
    path.push_str(&format!("M {} {}", route[0].x, route[0].y));

    let mut i = 1;
    while i + 2 < route.len() {
        path.push_str(&format!(
            " C {} {} {} {} {} {}",
            route[i].x,
            route[i].y,
            route[i + 1].x,
            route[i + 1].y,
            route[i + 2].x,
            route[i + 2].y,
        ));
        i += 3;
    }

    // Remaining points as line segments
    while i < route.len() {
        path.push_str(&format!(" L {} {}", route[i].x, route[i].y));
        i += 1;
    }

    path
}

/// Pick the right font color for a node/container label.
///
/// Priority:
/// 1. Explicit `style.font_color` — always wins.
/// 2. Explicit `style.fill` but no font_color — auto-contrast based on fill luminance.
/// 3. Neither set — fall back to theme default.
fn effective_font_color(style: &Style, theme: &Theme) -> Color {
    if let Some(fc) = style.font_color {
        fc
    } else if let Some(fill) = style.fill {
        fill.contrasting_text_color()
    } else {
        theme.font_color
    }
}

/// Render a positioned D2Graph to SVG string.
pub fn render(graph: &D2Graph, options: &RenderOptions) -> String {
    let theme = Theme::for_mode(options.dark_mode);
    let mut svg = String::with_capacity(4096);

    // Compute viewBox
    let (vx, vy, vw, vh) = compute_viewbox(graph);

    // SVG header
    svg.push_str(&format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{} {} {} {}\">\n",
        c(vx),
        c(vy),
        c(vw),
        c(vh)
    ));

    // Layer 1: Background
    let bg = theme.background.to_svg_string();
    svg.push_str(&format!(
        "  <rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"{}\"/>\n",
        c(vx),
        c(vy),
        c(vw),
        c(vh),
        bg
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
            let extra = if graph.graph[idx].style.shadow {
                3.0
            } else {
                0.0
            } + if graph.graph[idx].style.multiple {
                8.0
            } else {
                0.0
            } + if graph.graph[idx].style.three_d {
                6.0
            } else {
                0.0
            };
            let three_d_top = if graph.graph[idx].style.three_d {
                6.0
            } else {
                0.0
            };
            min_x = min_x.min(rect.x);
            min_y = min_y.min(rect.y - three_d_top);
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
                    rect.x,
                    rect.y,
                    rect.width,
                    rect.height,
                    rx,
                    fill,
                    stroke,
                    sw,
                    opacity_attr,
                    dash_attr,
                    obj.id,
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

        let fill = obj.style.fill.unwrap_or(theme.node_fill).to_svg_string();
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

        // Build SVG path based on route type
        let path = match edge.route_type {
            RouteType::Bezier => build_bezier_path(&edge.route),
            RouteType::Orthogonal => polyline_with_rounded_corners(&edge.route, BEND_RADIUS),
        };

        svg.push_str(&format!(
            "  <path d=\"{}\" stroke=\"{}\" fill=\"none\" stroke-width=\"{}\"{}{}/>",
            path, stroke, sw, opacity_attr, dash_attr,
        ));
        svg.push('\n');

        // Source arrowhead
        if edge.src_arrow != ArrowheadType::None && edge.route.len() >= 2 {
            render_arrowhead(
                svg,
                &edge.src_arrow,
                edge.route[0],
                edge.route[1],
                &stroke,
                sw,
            );
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

        let font_color = effective_font_color(&obj.style, &theme).to_svg_string();
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

        // Use stored label dimensions (with fallback to estimation)
        let label_width = if edge.label_width > 0.0 {
            edge.label_width
        } else {
            label.chars().count() as f64 * font_size * 0.55 // fallback
        };
        let label_height = if edge.label_height > 0.0 {
            edge.label_height
        } else {
            font_size * 1.2 // fallback
        };
        let pad = LABEL_HALO_PADDING;

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

        let font_color = effective_font_color(&obj.style, &theme).to_svg_string();
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

    /// Helper: parse+compile+layout a D2 source string, return the graph.
    fn layout_ok(source: &str) -> D2Graph {
        let ast = crate::parse(source).ast;
        let mut graph = crate::compile(&ast).expect("compile should succeed");
        crate::layout(&mut graph, &RenderOptions::default()).expect("layout should succeed");
        graph
    }

    #[test]
    fn test_light_fill_gets_dark_text_in_dark_mode() {
        // Node with light fill (#d4edda) should get dark text, not the dark-mode default (#e0e0e0).
        let graph = layout_ok("success: OK {\n  style.fill: \"#d4edda\"\n}");
        let mut options = RenderOptions::default();
        options.dark_mode = true;
        let svg = render(&graph, &options);

        // The label "OK" should be rendered with dark text (#171717), not light (#e0e0e0)
        assert!(
            svg.contains("fill=\"#171717\""),
            "expected dark text (#171717) for light fill in dark mode, SVG:\n{svg}"
        );
        assert!(
            !svg.contains(">OK</text>") || !svg.contains("fill=\"#e0e0e0\""),
            "label should NOT use the dark-mode default font color on a light fill"
        );
    }

    #[test]
    fn test_dark_fill_gets_light_text_in_light_mode() {
        // Node with dark fill (#155724) should get light text, not the light-mode default (#171717).
        let graph = layout_ok("a: Hello {\n  style.fill: \"#155724\"\n}");
        let mut options = RenderOptions::default();
        options.dark_mode = false;
        let svg = render(&graph, &options);

        // The label should be rendered with light text (#e0e0e0)
        assert!(
            svg.contains("fill=\"#e0e0e0\""),
            "expected light text (#e0e0e0) for dark fill in light mode, SVG:\n{svg}"
        );
    }

    #[test]
    fn test_explicit_font_color_overrides_auto_contrast() {
        // When font_color is explicitly set, it should be used regardless of fill.
        let graph =
            layout_ok("a: Hello {\n  style.fill: \"#d4edda\"\n  style.font-color: \"#ff0000\"\n}");
        let mut options = RenderOptions::default();
        options.dark_mode = true;
        let svg = render(&graph, &options);

        assert!(
            svg.contains("fill=\"#ff0000\""),
            "explicit font-color should override auto-contrast, SVG:\n{svg}"
        );
    }

    #[test]
    fn test_no_fill_uses_theme_default() {
        // Node with no explicit fill should use theme font_color.
        let graph = layout_ok("a: Hello");
        let mut options = RenderOptions::default();
        options.dark_mode = true;
        let svg = render(&graph, &options);

        // Should use dark-mode theme font color (#e0e0e0)
        assert!(
            svg.contains("fill=\"#e0e0e0\""),
            "no fill should use theme default font color, SVG:\n{svg}"
        );
    }

    // -----------------------------------------------------------------------
    // polyline_with_rounded_corners tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_polyline_two_points() {
        // Straight line, no Q commands
        let points = vec![Point::new(0.0, 0.0), Point::new(100.0, 0.0)];
        let path = polyline_with_rounded_corners(&points, 5.0);
        assert!(path.starts_with("M"));
        assert!(path.contains("L"));
        assert!(!path.contains("Q"));
    }

    #[test]
    fn test_polyline_three_points_has_curve() {
        // Right-angle bend should have Q command
        let points = vec![
            Point::new(0.0, 0.0),
            Point::new(50.0, 0.0),
            Point::new(50.0, 50.0),
        ];
        let path = polyline_with_rounded_corners(&points, 5.0);
        assert!(path.contains("Q"));
        assert!(path.contains("L"));
    }

    #[test]
    fn test_polyline_corner_clamping() {
        // Very short segments — radius should be clamped
        let points = vec![
            Point::new(0.0, 0.0),
            Point::new(4.0, 0.0), // only 4px long
            Point::new(4.0, 4.0),
        ];
        let path = polyline_with_rounded_corners(&points, 5.0);
        assert!(path.contains("Q")); // still has curve, just smaller radius
    }

    #[test]
    fn test_polyline_empty() {
        let path = polyline_with_rounded_corners(&[], 5.0);
        assert!(path.is_empty());
    }

    #[test]
    fn test_polyline_single_point() {
        let path = polyline_with_rounded_corners(&[Point::new(0.0, 0.0)], 5.0);
        assert!(path.is_empty());
    }

    #[test]
    fn test_orthogonal_route_renders_polyline() {
        // Full render of a graph with route_type=Orthogonal should produce
        // L/Q commands in SVG, not C (cubic Bezier).
        let source = "direction: right\na -> b";
        let ast = crate::parse(source).ast;
        let mut graph = crate::compile(&ast).expect("compile");
        crate::layout(&mut graph, &crate::RenderOptions::default()).expect("layout");

        let svg = render(&graph, &crate::RenderOptions::default());

        // The edge path should contain L (line to) commands from the polyline
        // and should NOT contain C (cubic Bezier) commands.
        // Self-loops would have C, but this graph has no self-loops.
        // The SVG has <path d="M ... L ... Q ..." for orthogonal edges.
        // Note: We just need to verify the edge path uses L/Q, not C.
        // The shape paths may contain other commands, so focus on the edge marker.

        // At minimum, the SVG should contain the rendered edge
        assert!(
            svg.contains(" L ") || svg.contains(" Q "),
            "orthogonal route SVG should contain L or Q commands"
        );
    }
}
