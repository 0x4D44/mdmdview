//! Shape definitions and SVG path generators.
//!
//! Each shape type has a corresponding SVG generator that produces
//! the appropriate SVG element(s) for that shape. Shapes are drawn
//! within a bounding Rect computed during layout.

use crate::geo::{Point, Rect};
use crate::graph::ShapeType;

/// Generate SVG markup for a shape within the given bounding rect.
/// Returns the SVG element string (no surrounding tags).
pub fn shape_svg(
    shape: ShapeType,
    rect: &Rect,
    fill: &str,
    stroke: &str,
    stroke_width: f64,
    border_radius: Option<f64>,
    opacity: Option<f64>,
    stroke_dash: Option<f64>,
    double_border: bool,
) -> String {
    let opacity_attr = opacity
        .map(|o| format!(" opacity=\"{}\"", o))
        .unwrap_or_default();
    let dash_attr = stroke_dash
        .map(|d| format!(" stroke-dasharray=\"{} {}\"", d, d))
        .unwrap_or_default();

    match shape {
        ShapeType::Rectangle | ShapeType::Square | ShapeType::Code => {
            let rx = border_radius.unwrap_or(0.0);
            let mut svg = format!(
                "<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" rx=\"{}\" \
                 fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"{}{}/>",
                rect.x, rect.y, rect.width, rect.height, rx, fill, stroke, stroke_width,
                opacity_attr, dash_attr,
            );
            if double_border {
                let inset = stroke_width * 2.0;
                svg.push_str(&format!(
                    "\n<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" rx=\"{}\" \
                     fill=\"none\" stroke=\"{}\" stroke-width=\"{}\"/>",
                    rect.x + inset,
                    rect.y + inset,
                    rect.width - inset * 2.0,
                    rect.height - inset * 2.0,
                    (rx - inset).max(0.0),
                    stroke,
                    stroke_width,
                ));
            }
            svg
        }
        ShapeType::Circle => {
            let cx = rect.x + rect.width / 2.0;
            let cy = rect.y + rect.height / 2.0;
            let r = rect.width.min(rect.height) / 2.0;
            let mut svg = format!(
                "<circle cx=\"{}\" cy=\"{}\" r=\"{}\" \
                 fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"{}{}/>",
                cx, cy, r, fill, stroke, stroke_width, opacity_attr, dash_attr,
            );
            if double_border {
                let inset = stroke_width * 2.0;
                svg.push_str(&format!(
                    "\n<circle cx=\"{}\" cy=\"{}\" r=\"{}\" \
                     fill=\"none\" stroke=\"{}\" stroke-width=\"{}\"/>",
                    cx,
                    cy,
                    (r - inset).max(0.0),
                    stroke,
                    stroke_width,
                ));
            }
            svg
        }
        ShapeType::Oval => {
            let cx = rect.x + rect.width / 2.0;
            let cy = rect.y + rect.height / 2.0;
            let rx = rect.width / 2.0;
            let ry = rect.height / 2.0;
            let mut svg = format!(
                "<ellipse cx=\"{}\" cy=\"{}\" rx=\"{}\" ry=\"{}\" \
                 fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"{}{}/>",
                cx, cy, rx, ry, fill, stroke, stroke_width, opacity_attr, dash_attr,
            );
            if double_border {
                let inset = stroke_width * 2.0;
                svg.push_str(&format!(
                    "\n<ellipse cx=\"{}\" cy=\"{}\" rx=\"{}\" ry=\"{}\" \
                     fill=\"none\" stroke=\"{}\" stroke-width=\"{}\"/>",
                    cx,
                    cy,
                    (rx - inset).max(0.0),
                    (ry - inset).max(0.0),
                    stroke,
                    stroke_width,
                ));
            }
            svg
        }
        ShapeType::Diamond => {
            let cx = rect.x + rect.width / 2.0;
            let cy = rect.y + rect.height / 2.0;
            let points = format!(
                "{},{} {},{} {},{} {},{}",
                cx,
                rect.y,
                rect.x + rect.width,
                cy,
                cx,
                rect.y + rect.height,
                rect.x,
                cy
            );
            format!(
                "<polygon points=\"{}\" \
                 fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"{}{}/>",
                points, fill, stroke, stroke_width, opacity_attr, dash_attr,
            )
        }
        ShapeType::Hexagon => {
            let x = rect.x;
            let y = rect.y;
            let w = rect.width;
            let h = rect.height;
            let inset = w * 0.25;
            let points = format!(
                "{},{} {},{} {},{} {},{} {},{} {},{}",
                x + inset,
                y,
                x + w - inset,
                y,
                x + w,
                y + h / 2.0,
                x + w - inset,
                y + h,
                x + inset,
                y + h,
                x,
                y + h / 2.0
            );
            format!(
                "<polygon points=\"{}\" \
                 fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"{}{}/>",
                points, fill, stroke, stroke_width, opacity_attr, dash_attr,
            )
        }
        ShapeType::Cylinder => {
            let x = rect.x;
            let y = rect.y;
            let w = rect.width;
            let h = rect.height;
            let ry = 10.0_f64.min(h * 0.15);
            // Cylinder: rectangle body + elliptical top cap
            format!(
                "<path d=\"M {x} {y1} \
                 A {rx} {ry} 0 0 1 {x2} {y1} \
                 V {y2} \
                 A {rx} {ry} 0 0 1 {x} {y2} \
                 Z\" \
                 fill=\"{fill}\" stroke=\"{stroke}\" stroke-width=\"{sw}\"{opacity}{dash}/>\
                 \n<ellipse cx=\"{cx}\" cy=\"{y1}\" rx=\"{rx}\" ry=\"{ry}\" \
                 fill=\"{fill}\" stroke=\"{stroke}\" stroke-width=\"{sw}\"/>",
                x = x,
                y1 = y + ry,
                x2 = x + w,
                y2 = y + h - ry,
                rx = w / 2.0,
                ry = ry,
                cx = x + w / 2.0,
                fill = fill,
                stroke = stroke,
                sw = stroke_width,
                opacity = opacity_attr,
                dash = dash_attr,
            )
        }
        ShapeType::Cloud => {
            let x = rect.x;
            let y = rect.y;
            let w = rect.width;
            let h = rect.height;
            // Cloud shape: a closed path of cubic Bézier arcs forming bumps.
            // The path traces clockwise: bottom-left → bottom-right (flat base),
            // then up the right side, across the top with 3 bumps, down the left.
            let bx = x;
            let by = y + h * 0.75; // base y
            format!(
                "<path d=\"M {bx} {by} \
                 C {c1x} {c1y} {c2x} {c2y} {p1x} {p1y} \
                 C {c3x} {c3y} {c4x} {c4y} {p2x} {p2y} \
                 C {c5x} {c5y} {c6x} {c6y} {p3x} {p3y} \
                 C {c7x} {c7y} {c8x} {c8y} {p4x} {p4y} \
                 C {c9x} {c9y} {c10x} {c10y} {bx} {by} \
                 Z\" \
                 fill=\"{fill}\" stroke=\"{stroke}\" stroke-width=\"{sw}\"{opacity}{dash}/>",
                bx = bx,
                by = by,
                // Bottom-right bump
                c1x = x + w * 0.15, c1y = y + h * 1.05,
                c2x = x + w * 0.85, c2y = y + h * 1.05,
                p1x = x + w, p1y = y + h * 0.65,
                // Right bump up
                c3x = x + w * 1.05, c3y = y + h * 0.35,
                c4x = x + w * 0.9,  c4y = y + h * 0.1,
                p2x = x + w * 0.7,  p2y = y + h * 0.15,
                // Top bump
                c5x = x + w * 0.6,  c5y = y - h * 0.05,
                c6x = x + w * 0.4,  c6y = y - h * 0.05,
                p3x = x + w * 0.3,  p3y = y + h * 0.15,
                // Left bump down
                c7x = x + w * 0.1,  c7y = y + h * 0.1,
                c8x = x - w * 0.05, c8y = y + h * 0.35,
                p4x = x,            p4y = y + h * 0.65,
                // Back to start
                c9x  = x - w * 0.05, c9y  = y + h * 0.85,
                c10x = x - w * 0.05, c10y = y + h * 0.95,
                fill = fill,
                stroke = stroke,
                sw = stroke_width,
                opacity = opacity_attr,
                dash = dash_attr,
            )
        }
        ShapeType::Parallelogram => {
            let x = rect.x;
            let y = rect.y;
            let w = rect.width;
            let h = rect.height;
            let skew = w * 0.2;
            let points = format!(
                "{},{} {},{} {},{} {},{}",
                x + skew,
                y,
                x + w,
                y,
                x + w - skew,
                y + h,
                x,
                y + h
            );
            format!(
                "<polygon points=\"{}\" \
                 fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"{}{}/>",
                points, fill, stroke, stroke_width, opacity_attr, dash_attr,
            )
        }
        ShapeType::Text => {
            // Text shape: no border, no fill
            String::new()
        }
        ShapeType::Person => {
            // Simplified person: circle head + rectangle body
            let head_r = rect.width.min(rect.height) * 0.2;
            let head_cx = rect.x + rect.width / 2.0;
            let head_cy = rect.y + head_r + 2.0;
            let body_x = rect.x + rect.width * 0.2;
            let body_y = head_cy + head_r + 4.0;
            let body_w = rect.width * 0.6;
            let body_h = rect.height - (body_y - rect.y) - 2.0;
            format!(
                "<circle cx=\"{}\" cy=\"{}\" r=\"{}\" fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>\
                 \n<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" rx=\"4\" \
                 fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>",
                head_cx, head_cy, head_r, fill, stroke, stroke_width, body_x, body_y, body_w,
                body_h.max(1.0),
                fill, stroke, stroke_width,
            )
        }
        ShapeType::Document => {
            let x = rect.x;
            let y = rect.y;
            let w = rect.width;
            let h = rect.height;
            let wave = h * 0.1;
            format!(
                "<path d=\"M {x} {y} h {w} v {bh} \
                 q {q1x} {q1y} {q2x} 0 \
                 q {q3x} {q3y} {q4x} 0 \
                 V {y} Z\" \
                 fill=\"{fill}\" stroke=\"{stroke}\" stroke-width=\"{sw}\"{opacity}{dash}/>",
                x = x,
                y = y,
                w = w,
                bh = h - wave,
                q1x = -w / 4.0,
                q1y = wave * 2.0,
                q2x = -w / 2.0,
                q3x = -w / 4.0,
                q3y = -wave * 2.0,
                q4x = -w / 2.0,
                fill = fill,
                stroke = stroke,
                sw = stroke_width,
                opacity = opacity_attr,
                dash = dash_attr,
            )
        }
        // Fallback for remaining shapes: rectangle
        _ => {
            let rx = border_radius.unwrap_or(0.0);
            format!(
                "<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" rx=\"{}\" \
                 fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"{}{}/>",
                rect.x, rect.y, rect.width, rect.height, rx, fill, stroke, stroke_width,
                opacity_attr, dash_attr,
            )
        }
    }
}

/// Clip a line (from → toward) to a shape boundary.
/// Returns the point on the shape boundary.
pub fn clip_to_shape(shape: ShapeType, rect: &Rect, from: Point, toward: Point) -> Point {
    match shape {
        ShapeType::Circle | ShapeType::Oval => clip_to_ellipse(rect, from, toward),
        ShapeType::Diamond => clip_to_diamond(rect, from, toward),
        ShapeType::Hexagon => clip_to_hexagon(rect, from, toward),
        _ => clip_to_rect(rect, from, toward),
    }
}

/// Clip to rectangle boundary.
pub fn clip_to_rect(rect: &Rect, from: Point, toward: Point) -> Point {
    let cx = rect.x + rect.width / 2.0;
    let cy = rect.y + rect.height / 2.0;
    let hw = rect.width / 2.0;
    let hh = rect.height / 2.0;

    let dx = toward.x - from.x;
    let dy = toward.y - from.y;

    if dx.abs() < 1e-10 && dy.abs() < 1e-10 {
        return Point::new(cx, cy - hh); // default: top
    }

    // Calculate intersection with each edge
    let mut min_t = f64::INFINITY;
    let mut result = from;

    // Use direction from center to toward
    let ddx = toward.x - cx;
    let ddy = toward.y - cy;

    // Right edge
    if ddx.abs() > 1e-10 {
        let t = hw / ddx.abs();
        let y = cy + ddy * t;
        if y >= rect.y && y <= rect.y + rect.height && t < min_t && t > 0.0 {
            min_t = t;
            result = Point::new(if ddx > 0.0 { rect.x + rect.width } else { rect.x }, y);
        }
    }

    // Top/bottom edge
    if ddy.abs() > 1e-10 {
        let t = hh / ddy.abs();
        let x = cx + ddx * t;
        if x >= rect.x && x <= rect.x + rect.width && t < min_t && t > 0.0 {
            min_t = t;
            result = Point::new(
                x,
                if ddy > 0.0 {
                    rect.y + rect.height
                } else {
                    rect.y
                },
            );
        }
    }

    if min_t == f64::INFINITY {
        Point::new(cx, cy - hh)
    } else {
        result
    }
}

/// Clip to ellipse boundary.
fn clip_to_ellipse(rect: &Rect, _from: Point, toward: Point) -> Point {
    let cx = rect.x + rect.width / 2.0;
    let cy = rect.y + rect.height / 2.0;
    let rx = rect.width / 2.0;
    let ry = rect.height / 2.0;

    let dx = toward.x - cx;
    let dy = toward.y - cy;
    let len = (dx * dx + dy * dy).sqrt();

    if len < 1e-10 {
        return Point::new(cx, cy - ry);
    }

    let ux = dx / len;
    let uy = dy / len;

    // Parametric intersection with ellipse
    let denom = ((ux * ux) / (rx * rx) + (uy * uy) / (ry * ry)).sqrt();
    let t = 1.0 / denom;

    Point::new(cx + ux * t, cy + uy * t)
}

/// Clip to diamond boundary.
fn clip_to_diamond(rect: &Rect, _from: Point, toward: Point) -> Point {
    let cx = rect.x + rect.width / 2.0;
    let cy = rect.y + rect.height / 2.0;
    let hw = rect.width / 2.0;
    let hh = rect.height / 2.0;

    let dx = toward.x - cx;
    let dy = toward.y - cy;

    if dx.abs() < 1e-10 && dy.abs() < 1e-10 {
        return Point::new(cx, cy - hh);
    }

    // Diamond edges: |x/hw| + |y/hh| = 1
    let scale = (dx.abs() / hw + dy.abs() / hh).max(1e-10);
    Point::new(cx + dx / scale, cy + dy / scale)
}

/// Clip to hexagon boundary.
/// Hexagon has 6 edges: top-left, top-right, right, bottom-right, bottom-left, left.
fn clip_to_hexagon(rect: &Rect, _from: Point, toward: Point) -> Point {
    let cx = rect.x + rect.width / 2.0;
    let cy = rect.y + rect.height / 2.0;
    let inset = rect.width * 0.25;

    // Hexagon vertices (matching the SVG generator)
    let vertices = [
        Point::new(rect.x + inset, rect.y),                   // top-left
        Point::new(rect.x + rect.width - inset, rect.y),      // top-right
        Point::new(rect.x + rect.width, cy),                  // right
        Point::new(rect.x + rect.width - inset, rect.y + rect.height), // bottom-right
        Point::new(rect.x + inset, rect.y + rect.height),     // bottom-left
        Point::new(rect.x, cy),                                // left
    ];

    let dx = toward.x - cx;
    let dy = toward.y - cy;

    if dx.abs() < 1e-10 && dy.abs() < 1e-10 {
        return vertices[0]; // default: top-left vertex
    }

    // Test intersection with each of the 6 edges
    let mut best_t = f64::INFINITY;
    let mut best_point = Point::new(cx, cy);

    for i in 0..6 {
        let v0 = vertices[i];
        let v1 = vertices[(i + 1) % 6];

        // Ray from center in direction (dx, dy): P = center + t * dir
        // Edge from v0 to v1: Q = v0 + s * (v1 - v0)
        let ex = v1.x - v0.x;
        let ey = v1.y - v0.y;
        let det = dx * ey - dy * ex;

        if det.abs() < 1e-10 {
            continue; // parallel
        }

        let ox = v0.x - cx;
        let oy = v0.y - cy;
        let t = (ox * ey - oy * ex) / det;
        let s = (ox * dy - oy * dx) / det;

        if t > 1e-10 && s >= 0.0 && s <= 1.0 && t < best_t {
            best_t = t;
            best_point = Point::new(cx + dx * t, cy + dy * t);
        }
    }

    best_point
}

/// Generate SVG for shadow effect (offset duplicate at 30% opacity).
pub fn shadow_svg(
    shape: ShapeType,
    rect: &Rect,
    fill: &str,
    stroke_width: f64,
) -> String {
    let shadow_rect = Rect::new(rect.x + 3.0, rect.y + 3.0, rect.width, rect.height);
    shape_svg(
        shape,
        &shadow_rect,
        fill,
        "none",
        stroke_width,
        None,
        Some(0.3),
        None,
        false,
    )
}

/// Generate SVG for 3D effect (extra parallelogram face on bottom-right).
/// The face is filled with a slightly darker shade of the shape's fill color.
pub fn three_d_svg(
    shape: ShapeType,
    rect: &Rect,
    _fill: &str,
    stroke: &str,
    stroke_width: f64,
) -> String {
    let depth = 6.0;
    // Darken the fill color for the 3D face. Use a fixed dark overlay.
    let face_fill = "#00000040"; // semi-transparent black overlay

    match shape {
        ShapeType::Rectangle | ShapeType::Square | ShapeType::Code => {
            // Right face: parallelogram on the right side
            let right_face = format!(
                "<polygon points=\"{},{} {},{} {},{} {},{}\" fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>",
                rect.x + rect.width, rect.y,
                rect.x + rect.width + depth, rect.y - depth,
                rect.x + rect.width + depth, rect.y + rect.height - depth,
                rect.x + rect.width, rect.y + rect.height,
                face_fill, stroke, stroke_width * 0.5,
            );
            // Top face: parallelogram on the top
            let top_face = format!(
                "<polygon points=\"{},{} {},{} {},{} {},{}\" fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>",
                rect.x, rect.y,
                rect.x + depth, rect.y - depth,
                rect.x + rect.width + depth, rect.y - depth,
                rect.x + rect.width, rect.y,
                face_fill, stroke, stroke_width * 0.5,
            );
            format!("{}\n{}", right_face, top_face)
        }
        ShapeType::Cylinder => {
            // Simplified: just a right-side parallelogram
            let right_face = format!(
                "<polygon points=\"{},{} {},{} {},{} {},{}\" fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>",
                rect.x + rect.width, rect.y + 10.0,
                rect.x + rect.width + depth, rect.y + 10.0 - depth,
                rect.x + rect.width + depth, rect.y + rect.height - depth,
                rect.x + rect.width, rect.y + rect.height,
                face_fill, stroke, stroke_width * 0.5,
            );
            right_face
        }
        _ => {
            // For other shapes, approximate with offset rectangle face
            let right_face = format!(
                "<polygon points=\"{},{} {},{} {},{} {},{}\" fill=\"{}\" stroke=\"{}\" stroke-width=\"{}\"/>",
                rect.x + rect.width, rect.y,
                rect.x + rect.width + depth, rect.y - depth,
                rect.x + rect.width + depth, rect.y + rect.height - depth,
                rect.x + rect.width, rect.y + rect.height,
                face_fill, stroke, stroke_width * 0.5,
            );
            right_face
        }
    }
}

/// Generate SVG for "multiple" effect (stacked copies behind main shape).
pub fn multiple_svg(
    shape: ShapeType,
    rect: &Rect,
    fill: &str,
    stroke: &str,
    stroke_width: f64,
    border_radius: Option<f64>,
) -> String {
    let mut svg = String::new();
    // Two offset copies behind main shape
    for offset in &[8.0, 4.0] {
        let offset_rect = Rect::new(rect.x + offset, rect.y + offset, rect.width, rect.height);
        svg.push_str(&shape_svg(
            shape,
            &offset_rect,
            fill,
            stroke,
            stroke_width,
            border_radius,
            None,
            None,
            false,
        ));
        svg.push('\n');
    }
    svg
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rectangle_svg() {
        let rect = Rect::new(10.0, 20.0, 100.0, 50.0);
        let svg = shape_svg(
            ShapeType::Rectangle,
            &rect,
            "#ffffff",
            "#000000",
            2.0,
            None,
            None,
            None,
            false,
        );
        assert!(svg.contains("<rect"));
        assert!(svg.contains("x=\"10\""));
    }

    #[test]
    fn test_circle_svg() {
        let rect = Rect::new(10.0, 20.0, 100.0, 100.0);
        let svg = shape_svg(
            ShapeType::Circle,
            &rect,
            "#ffffff",
            "#000000",
            2.0,
            None,
            None,
            None,
            false,
        );
        assert!(svg.contains("<circle"));
    }

    #[test]
    fn test_diamond_svg() {
        let rect = Rect::new(0.0, 0.0, 100.0, 100.0);
        let svg = shape_svg(
            ShapeType::Diamond,
            &rect,
            "#ffffff",
            "#000000",
            2.0,
            None,
            None,
            None,
            false,
        );
        assert!(svg.contains("<polygon"));
    }

    #[test]
    fn test_text_shape_empty() {
        let rect = Rect::new(0.0, 0.0, 100.0, 30.0);
        let svg = shape_svg(
            ShapeType::Text,
            &rect,
            "#ffffff",
            "#000000",
            2.0,
            None,
            None,
            None,
            false,
        );
        assert!(svg.is_empty());
    }

    #[test]
    fn test_clip_to_rect() {
        let rect = Rect::new(0.0, 0.0, 100.0, 100.0);
        let from = Point::new(50.0, 50.0);
        let toward = Point::new(200.0, 50.0); // going right
        let p = clip_to_rect(&rect, from, toward);
        assert!((p.x - 100.0).abs() < 1.0);
        assert!((p.y - 50.0).abs() < 1.0);
    }

    #[test]
    fn test_clip_to_ellipse() {
        let rect = Rect::new(0.0, 0.0, 100.0, 100.0);
        let from = Point::new(50.0, 50.0);
        let toward = Point::new(100.0, 50.0);
        let p = clip_to_ellipse(&rect, from, toward);
        assert!((p.x - 100.0).abs() < 1.0);
    }
}
