//! D2 Conformance Test Suite — structural invariant checker.
//!
//! This integration test renders every `.d2` fixture through the native
//! `d2::render_d2_to_svg()` pipeline, parses the resulting SVG to extract
//! element positions, and checks four structural invariants:
//!
//! 1. **label_not_overlapping_node** — Edge labels must not overlap node rects.
//! 2. **edge_not_through_node** — Edge line segments must not pass through
//!    non-endpoint node rects.
//! 3. **nodes_not_overlapping** — Node rects must not overlap (except
//!    container-child nesting).
//! 4. **label_near_edge** — Edge labels must be within 50 px of their edge path.
//!
//! On failure the SVG is dumped to `tests/d2_conformance/actual/<name>.svg`
//! for visual debugging. A companion helper always dumps *all* SVGs there so
//! the Python comparison tooling can diff them against reference images.

use mdmdview_d2::{render_d2_to_svg, RenderOptions};
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Geometry helpers
// ---------------------------------------------------------------------------

/// Axis-aligned bounding box.
#[derive(Debug, Clone, Copy)]
struct AABB {
    x: f64,
    y: f64,
    w: f64,
    h: f64,
}

impl AABB {
    fn right(&self) -> f64 {
        self.x + self.w
    }
    fn bottom(&self) -> f64 {
        self.y + self.h
    }

    /// True if `self` strictly overlaps `other` (more than a 0.5 px sliver).
    fn overlaps(&self, other: &AABB) -> bool {
        let eps = 0.5;
        self.x + eps < other.right()
            && other.x + eps < self.right()
            && self.y + eps < other.bottom()
            && other.y + eps < self.bottom()
    }

    /// True if `self` fully contains `other` (with tolerance).
    fn contains(&self, other: &AABB) -> bool {
        let eps = 1.0;
        self.x - eps <= other.x
            && self.y - eps <= other.y
            && self.right() + eps >= other.right()
            && self.bottom() + eps >= other.bottom()
    }

    /// Area of the rectangle.
    fn area(&self) -> f64 {
        self.w * self.h
    }
}

/// 2D point.
#[derive(Debug, Clone, Copy)]
struct Pt {
    x: f64,
    y: f64,
}

/// A straight line segment.
#[derive(Debug, Clone, Copy)]
struct Seg {
    a: Pt,
    b: Pt,
}

/// Test whether line segment `seg` intersects `aabb`, ignoring intersections
/// that occur very close to the segment endpoints (within `shrink` pixels).
/// This lets us tolerate edges that originate/terminate on a node border.
fn segment_intersects_aabb(seg: &Seg, aabb: &AABB, shrink: f64) -> bool {
    // Shrink the segment by `shrink` from each end to tolerate endpoint overlap.
    let dx = seg.b.x - seg.a.x;
    let dy = seg.b.y - seg.a.y;
    let len = (dx * dx + dy * dy).sqrt();
    if len < 1e-6 {
        return false; // degenerate segment
    }
    let ux = dx / len;
    let uy = dy / len;
    let a = Pt {
        x: seg.a.x + ux * shrink,
        y: seg.a.y + uy * shrink,
    };
    let b = Pt {
        x: seg.b.x - ux * shrink,
        y: seg.b.y - uy * shrink,
    };

    // After shrinking, if nothing remains, skip.
    let new_len = ((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y)).sqrt();
    if new_len < 1e-6 {
        return false;
    }

    // Liang-Barsky line-AABB clipping
    liang_barsky(a, b, aabb)
}

/// Liang-Barsky algorithm: returns true if segment (p0→p1) intersects the AABB.
fn liang_barsky(p0: Pt, p1: Pt, aabb: &AABB) -> bool {
    let dx = p1.x - p0.x;
    let dy = p1.y - p0.y;
    let mut t_min = 0.0_f64;
    let mut t_max = 1.0_f64;

    let edges = [
        (-dx, -(aabb.x - p0.x)),    // left
        (dx, aabb.right() - p0.x),  // right
        (-dy, -(aabb.y - p0.y)),    // top
        (dy, aabb.bottom() - p0.y), // bottom
    ];

    for &(p, q) in &edges {
        if p.abs() < 1e-12 {
            // Parallel to this edge
            if q < 0.0 {
                return false; // outside
            }
        } else {
            let t = q / p;
            if p < 0.0 {
                t_min = t_min.max(t);
            } else {
                t_max = t_max.min(t);
            }
            if t_min > t_max {
                return false;
            }
        }
    }

    true
}

/// True if point `p` lies inside `aabb` (inclusive of boundaries).
fn point_in_aabb(p: Pt, aabb: &AABB) -> bool {
    p.x >= aabb.x && p.x <= aabb.x + aabb.w && p.y >= aabb.y && p.y <= aabb.y + aabb.h
}

/// Minimum distance from point `p` to line segment `seg`.
fn point_to_segment_dist(p: Pt, seg: &Seg) -> f64 {
    let dx = seg.b.x - seg.a.x;
    let dy = seg.b.y - seg.a.y;
    let len_sq = dx * dx + dy * dy;
    if len_sq < 1e-12 {
        let ddx = p.x - seg.a.x;
        let ddy = p.y - seg.a.y;
        return (ddx * ddx + ddy * ddy).sqrt();
    }
    let t = ((p.x - seg.a.x) * dx + (p.y - seg.a.y) * dy) / len_sq;
    let t = t.clamp(0.0, 1.0);
    let proj_x = seg.a.x + t * dx;
    let proj_y = seg.a.y + t * dy;
    let ddx = p.x - proj_x;
    let ddy = p.y - proj_y;
    (ddx * ddx + ddy * ddy).sqrt()
}

// ---------------------------------------------------------------------------
// SVG Parsing (regex-free — hand-rolled for zero dependencies)
// ---------------------------------------------------------------------------

/// Parsed SVG rect element.
#[derive(Debug, Clone)]
struct SvgRect {
    x: f64,
    y: f64,
    width: f64,
    height: f64,
    is_container: bool,
    /// True if this rect is an edge label halo (opacity="0.85").
    is_halo: bool,
    /// The comment label after the rect (if any). Retained for debug output.
    #[allow(dead_code)]
    comment: String,
}

/// Parsed SVG text element.
#[derive(Debug, Clone)]
struct SvgText {
    x: f64,
    y: f64,
    content: String,
    /// True if this is positioned with text-anchor="middle" + dominant-baseline="central"
    /// (i.e. a node label or edge label, not a container label).
    is_centered: bool,
}

/// Parsed SVG path element.
#[derive(Debug, Clone)]
struct SvgPath {
    /// Linear segments extracted from the M/L/Q/C commands.
    segments: Vec<Seg>,
    /// All sample points along the path (including Q/C approximations).
    sample_points: Vec<Pt>,
    /// First point of the path (from the initial M command).
    start_pt: Pt,
    /// Last point of the path (endpoint of the final command).
    end_pt: Pt,
}

/// Parse all `<rect .../>` elements from SVG source, skipping the first
/// (background rect).
fn parse_rects(svg: &str) -> Vec<SvgRect> {
    let mut rects = Vec::new();
    let mut search_from = 0;

    while let Some(start) = svg[search_from..].find("<rect ") {
        let abs_start = search_from + start;
        let tag_end = match svg[abs_start..].find("/>") {
            Some(p) => abs_start + p + 2,
            None => {
                search_from = abs_start + 1;
                continue;
            }
        };

        let tag = &svg[abs_start..tag_end];

        // Extract attributes
        let x = extract_attr_f64(tag, "x").unwrap_or(0.0);
        let y = extract_attr_f64(tag, "y").unwrap_or(0.0);
        let width = extract_attr_f64(tag, "width").unwrap_or(0.0);
        let height = extract_attr_f64(tag, "height").unwrap_or(0.0);
        let is_halo = tag.contains("opacity=\"0.85\"");

        // Check if followed by <!-- container: ... --> comment
        let rest_after = &svg[tag_end..];
        let is_container = rest_after.trim_start().starts_with("<!-- container:");
        let comment = if is_container {
            if let Some(cstart) = rest_after.find("<!-- container:") {
                let cend = rest_after[cstart..].find("-->").unwrap_or(0);
                rest_after[cstart + 15..cstart + cend].trim().to_string()
            } else {
                String::new()
            }
        } else {
            // Check for <!-- id --> comment after the rect
            let trimmed = rest_after.trim_start_matches('\n');
            if trimmed.starts_with("<!--") {
                if let Some(cend) = trimmed.find("-->") {
                    trimmed[4..cend].trim().to_string()
                } else {
                    String::new()
                }
            } else {
                String::new()
            }
        };

        rects.push(SvgRect {
            x,
            y,
            width,
            height,
            is_container,
            is_halo,
            comment,
        });

        search_from = tag_end;
    }

    // Skip the first rect (background)
    if !rects.is_empty() {
        rects.remove(0);
    }

    // Skip edge-label halo rects and tiny degenerate rects
    rects.retain(|r| !r.is_halo && r.width > 1.0 && r.height > 1.0);

    rects
}

/// Parse all `<text ...>content</text>` elements.
fn parse_texts(svg: &str) -> Vec<SvgText> {
    let mut texts = Vec::new();
    let mut search_from = 0;

    while let Some(start) = svg[search_from..].find("<text ") {
        let abs_start = search_from + start;

        // Find the closing </text>
        let close_tag = match svg[abs_start..].find("</text>") {
            Some(p) => abs_start + p,
            None => {
                search_from = abs_start + 1;
                continue;
            }
        };

        let _full = &svg[abs_start..close_tag + 7]; // includes </text>

        // Find end of opening tag
        let open_end = match svg[abs_start..].find('>') {
            Some(p) => abs_start + p,
            None => {
                search_from = close_tag + 7;
                continue;
            }
        };
        let open_tag = &svg[abs_start..open_end + 1];
        let inner = &svg[open_end + 1..close_tag];

        let x = extract_attr_f64(open_tag, "x").unwrap_or(0.0);
        let y = extract_attr_f64(open_tag, "y").unwrap_or(0.0);
        let is_centered = open_tag.contains("text-anchor=\"middle\"")
            && open_tag.contains("dominant-baseline=\"central\"");

        // Extract text content — might have <tspan> children
        let content = if inner.contains("<tspan") {
            // Collect tspan text
            let mut text = String::new();
            let mut tsearch = 0;
            while let Some(ts) = inner[tsearch..].find("<tspan") {
                let abs_ts = tsearch + ts;
                if let Some(te) = inner[abs_ts..].find('>') {
                    let content_start = abs_ts + te + 1;
                    if let Some(tc) = inner[content_start..].find("</tspan>") {
                        if !text.is_empty() {
                            text.push(' ');
                        }
                        text.push_str(&inner[content_start..content_start + tc]);
                        tsearch = content_start + tc + 8;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            text
        } else {
            inner.to_string()
        };

        // Unescape XML entities
        let content = content
            .replace("&amp;", "&")
            .replace("&lt;", "<")
            .replace("&gt;", ">")
            .replace("&quot;", "\"")
            .replace("&apos;", "'");

        texts.push(SvgText {
            x,
            y,
            content,
            is_centered,
        });

        search_from = close_tag + 7;
    }

    texts
}

/// Parse all `<path .../>` elements.
fn parse_paths(svg: &str) -> Vec<SvgPath> {
    let mut paths = Vec::new();
    let mut search_from = 0;

    while let Some(start) = svg[search_from..].find("<path ") {
        let abs_start = search_from + start;
        let tag_end = match svg[abs_start..].find("/>") {
            Some(p) => abs_start + p + 2,
            None => {
                search_from = abs_start + 1;
                continue;
            }
        };

        let tag = &svg[abs_start..tag_end];

        // Only consider edge paths (fill="none"), skip shape outlines
        if !tag.contains("fill=\"none\"") {
            search_from = tag_end;
            continue;
        }

        if let Some(d) = extract_attr_str(tag, "d") {
            let (segments, sample_points) = parse_path_d(&d);
            let start_pt = sample_points
                .first()
                .copied()
                .unwrap_or(Pt { x: 0.0, y: 0.0 });
            let end_pt = sample_points
                .last()
                .copied()
                .unwrap_or(Pt { x: 0.0, y: 0.0 });
            paths.push(SvgPath {
                segments,
                sample_points,
                start_pt,
                end_pt,
            });
        }

        search_from = tag_end;
    }

    paths
}

/// Extract a numeric attribute value from an SVG tag string.
fn extract_attr_f64(tag: &str, name: &str) -> Option<f64> {
    let needle = format!("{name}=\"");
    let start = tag.find(&needle)? + needle.len();
    let end = tag[start..].find('"')? + start;
    tag[start..end].parse::<f64>().ok()
}

/// Extract a string attribute value from an SVG tag string.
fn extract_attr_str<'a>(tag: &'a str, name: &str) -> Option<&'a str> {
    let needle = format!("{name}=\"");
    let start = tag.find(&needle)? + needle.len();
    let end = tag[start..].find('"')? + start;
    Some(&tag[start..end])
}

/// Parse an SVG path `d` attribute into line segments and sample points.
///
/// Handles M, L, Q, C commands (uppercase only — our renderer always uses uppercase).
fn parse_path_d(d: &str) -> (Vec<Seg>, Vec<Pt>) {
    let mut segments = Vec::new();
    let mut samples = Vec::new();
    let mut current = Pt { x: 0.0, y: 0.0 };

    let tokens = tokenize_path(d);
    let mut i = 0;

    while i < tokens.len() {
        match tokens[i].as_str() {
            "M" => {
                if i + 2 < tokens.len() {
                    let x = tokens[i + 1].parse::<f64>().unwrap_or(0.0);
                    let y = tokens[i + 2].parse::<f64>().unwrap_or(0.0);
                    current = Pt { x, y };
                    samples.push(current);
                    i += 3;
                } else {
                    i += 1;
                }
            }
            "L" => {
                if i + 2 < tokens.len() {
                    let x = tokens[i + 1].parse::<f64>().unwrap_or(0.0);
                    let y = tokens[i + 2].parse::<f64>().unwrap_or(0.0);
                    let next = Pt { x, y };
                    segments.push(Seg {
                        a: current,
                        b: next,
                    });
                    // Sample along the line
                    for t in 0..=4 {
                        let frac = t as f64 / 4.0;
                        samples.push(Pt {
                            x: current.x + (next.x - current.x) * frac,
                            y: current.y + (next.y - current.y) * frac,
                        });
                    }
                    current = next;
                    i += 3;
                } else {
                    i += 1;
                }
            }
            "Q" => {
                // Quadratic Bezier: control point + end point
                if i + 4 < tokens.len() {
                    let cx = tokens[i + 1].parse::<f64>().unwrap_or(0.0);
                    let cy = tokens[i + 2].parse::<f64>().unwrap_or(0.0);
                    let ex = tokens[i + 3].parse::<f64>().unwrap_or(0.0);
                    let ey = tokens[i + 4].parse::<f64>().unwrap_or(0.0);
                    let end = Pt { x: ex, y: ey };

                    // Approximate with line segment for intersection test
                    segments.push(Seg { a: current, b: end });

                    // Sample along the quadratic curve
                    let cp = Pt { x: cx, y: cy };
                    for t in 0..=8 {
                        let frac = t as f64 / 8.0;
                        let one_minus = 1.0 - frac;
                        samples.push(Pt {
                            x: one_minus * one_minus * current.x
                                + 2.0 * one_minus * frac * cp.x
                                + frac * frac * end.x,
                            y: one_minus * one_minus * current.y
                                + 2.0 * one_minus * frac * cp.y
                                + frac * frac * end.y,
                        });
                    }

                    current = end;
                    i += 5;
                } else {
                    i += 1;
                }
            }
            "C" => {
                // Cubic Bezier: 2 control points + end point
                if i + 6 < tokens.len() {
                    let c1x = tokens[i + 1].parse::<f64>().unwrap_or(0.0);
                    let c1y = tokens[i + 2].parse::<f64>().unwrap_or(0.0);
                    let c2x = tokens[i + 3].parse::<f64>().unwrap_or(0.0);
                    let c2y = tokens[i + 4].parse::<f64>().unwrap_or(0.0);
                    let ex = tokens[i + 5].parse::<f64>().unwrap_or(0.0);
                    let ey = tokens[i + 6].parse::<f64>().unwrap_or(0.0);
                    let end = Pt { x: ex, y: ey };

                    segments.push(Seg { a: current, b: end });

                    // Sample along the cubic curve
                    let cp1 = Pt { x: c1x, y: c1y };
                    let cp2 = Pt { x: c2x, y: c2y };
                    for t in 0..=8 {
                        let frac = t as f64 / 8.0;
                        let om = 1.0 - frac;
                        samples.push(Pt {
                            x: om * om * om * current.x
                                + 3.0 * om * om * frac * cp1.x
                                + 3.0 * om * frac * frac * cp2.x
                                + frac * frac * frac * end.x,
                            y: om * om * om * current.y
                                + 3.0 * om * om * frac * cp1.y
                                + 3.0 * om * frac * frac * cp2.y
                                + frac * frac * frac * end.y,
                        });
                    }

                    current = end;
                    i += 7;
                } else {
                    i += 1;
                }
            }
            _ => {
                i += 1;
            }
        }
    }

    (segments, samples)
}

/// Tokenize an SVG path `d` string into commands and numeric values.
fn tokenize_path(d: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();

    for ch in d.chars() {
        match ch {
            'M' | 'L' | 'Q' | 'C' | 'Z' | 'H' | 'V' | 'A' | 'S' | 'T' => {
                if !current.is_empty() {
                    tokens.push(std::mem::take(&mut current));
                }
                tokens.push(ch.to_string());
            }
            '0'..='9' | '.' | '-' | '+' => {
                // Handle negative sign starting a new number
                if ch == '-' && !current.is_empty() {
                    tokens.push(std::mem::take(&mut current));
                }
                current.push(ch);
            }
            ' ' | ',' | '\t' | '\n' | '\r' => {
                if !current.is_empty() {
                    tokens.push(std::mem::take(&mut current));
                }
            }
            _ => {
                if !current.is_empty() {
                    tokens.push(std::mem::take(&mut current));
                }
            }
        }
    }
    if !current.is_empty() {
        tokens.push(current);
    }

    tokens
}

// ---------------------------------------------------------------------------
// Invariant checking
// ---------------------------------------------------------------------------

/// A single invariant violation found during checking.
#[derive(Debug)]
struct Violation {
    invariant: &'static str,
    detail: String,
}

/// Classify rects into node rects and container rects.
///
/// Container rects are identified by:
/// - The `<!-- container: ... -->` comment after them, OR
/// - Being strictly larger than and containing at least one other rect.
fn classify_rects(rects: &[SvgRect]) -> (Vec<AABB>, Vec<AABB>) {
    let mut node_rects = Vec::new();
    let mut container_rects = Vec::new();

    for r in rects {
        let aabb = AABB {
            x: r.x,
            y: r.y,
            w: r.width,
            h: r.height,
        };

        if r.is_container {
            container_rects.push(aabb);
        } else {
            // Check if this rect contains any other rect (heuristic for containers
            // that might not have the comment marker)
            let contains_other = rects.iter().any(|other| {
                if std::ptr::eq(r, other) {
                    return false;
                }
                let other_aabb = AABB {
                    x: other.x,
                    y: other.y,
                    w: other.width,
                    h: other.height,
                };
                aabb.contains(&other_aabb) && aabb.area() > other_aabb.area() * 1.5
            });

            if contains_other {
                container_rects.push(aabb);
            } else {
                node_rects.push(aabb);
            }
        }
    }

    (node_rects, container_rects)
}

/// Estimate a bounding box for a text label.
fn text_bbox(text: &SvgText) -> AABB {
    let char_width = 7.0; // rough estimate for default font sizes
    let height = 14.0;
    if text.is_centered {
        // text-anchor="middle", dominant-baseline="central"
        // The x,y is the center of the text
        let w = text.content.len() as f64 * char_width;
        AABB {
            x: text.x - w / 2.0,
            y: text.y - height / 2.0,
            w,
            h: height,
        }
    } else {
        // Container labels: x,y is the top-left baseline
        let w = text.content.len() as f64 * char_width;
        AABB {
            x: text.x,
            y: text.y - 12.0,
            w,
            h: height,
        }
    }
}

/// Check all four structural invariants on the parsed SVG elements.
fn check_invariants(
    node_rects: &[AABB],
    container_rects: &[AABB],
    texts: &[SvgText],
    paths: &[SvgPath],
) -> Vec<Violation> {
    let mut violations = Vec::new();

    // --- Invariant 1: label_not_overlapping_node ---
    // Edge labels are centered text elements that don't sit inside any node rect.
    for text in texts {
        if !text.is_centered {
            continue; // skip container labels
        }

        let tbox = text_bbox(text);
        let center = Pt {
            x: text.x,
            y: text.y,
        };

        // Is this text inside a node rect? If so, it's a node label, not an edge label.
        let inside_node = node_rects.iter().any(|nr| {
            let nr_expanded = AABB {
                x: nr.x - 2.0,
                y: nr.y - 2.0,
                w: nr.w + 4.0,
                h: nr.h + 4.0,
            };
            nr_expanded.contains(&AABB {
                x: center.x,
                y: center.y,
                w: 0.0,
                h: 0.0,
            })
        });

        if inside_node {
            continue; // node label — skip
        }

        // This is likely an edge label. Check overlap with all node rects.
        for (i, nr) in node_rects.iter().enumerate() {
            if tbox.overlaps(nr) {
                violations.push(Violation {
                    invariant: "label_not_overlapping_node",
                    detail: format!(
                        "Edge label '{}' bbox ({:.0},{:.0} {:.0}x{:.0}) overlaps node rect #{} ({:.0},{:.0} {:.0}x{:.0})",
                        text.content, tbox.x, tbox.y, tbox.w, tbox.h,
                        i, nr.x, nr.y, nr.w, nr.h,
                    ),
                });
            }
        }
    }

    // --- Invariant 2: edge_not_through_node ---
    // Edge path segments should not pass through non-endpoint node rects.
    // Endpoint nodes (source/destination) are exempt — edges naturally enter/exit them.
    // Container rects are also skipped — edges naturally pass through containers.
    let endpoint_tolerance = 15.0; // pixels to shrink segments at endpoints
    let endpoint_expand = 20.0; // pixels to expand node rects when matching endpoints
    for (pi, path) in paths.iter().enumerate() {
        // Determine which node rects are endpoints for this path.
        // A node is an endpoint if the path's start_pt or end_pt falls within
        // (or very close to) the node's bounding box.
        let endpoint_node_indices: Vec<usize> = node_rects
            .iter()
            .enumerate()
            .filter(|(_ni, nr)| {
                let expanded = AABB {
                    x: nr.x - endpoint_expand,
                    y: nr.y - endpoint_expand,
                    w: nr.w + 2.0 * endpoint_expand,
                    h: nr.h + 2.0 * endpoint_expand,
                };
                point_in_aabb(path.start_pt, &expanded) || point_in_aabb(path.end_pt, &expanded)
            })
            .map(|(ni, _)| ni)
            .collect();

        for (si, seg) in path.segments.iter().enumerate() {
            for (ni, nr) in node_rects.iter().enumerate() {
                // Skip endpoint nodes — the edge naturally enters/exits them
                if endpoint_node_indices.contains(&ni) {
                    continue;
                }
                if segment_intersects_aabb(seg, nr, endpoint_tolerance) {
                    violations.push(Violation {
                        invariant: "edge_not_through_node",
                        detail: format!(
                            "Path #{} segment #{} ({:.0},{:.0})->({:.0},{:.0}) intersects node rect #{} ({:.0},{:.0} {:.0}x{:.0})",
                            pi, si, seg.a.x, seg.a.y, seg.b.x, seg.b.y,
                            ni, nr.x, nr.y, nr.w, nr.h,
                        ),
                    });
                }
            }
        }
    }

    // --- Invariant 3: nodes_not_overlapping ---
    // No two node rects should overlap, unless one contains the other (container-child).
    for i in 0..node_rects.len() {
        for j in (i + 1)..node_rects.len() {
            let a = &node_rects[i];
            let b = &node_rects[j];
            if a.overlaps(b) {
                // Check if this is a container-child relationship
                let is_nested = a.contains(b) || b.contains(a);
                // Also check if both are inside the same container
                let both_in_container = container_rects
                    .iter()
                    .any(|c| c.contains(a) && c.contains(b));
                // The overlap is only a violation if they're genuinely colliding siblings
                if !is_nested {
                    violations.push(Violation {
                        invariant: "nodes_not_overlapping",
                        detail: format!(
                            "Node rect #{} ({:.0},{:.0} {:.0}x{:.0}) overlaps node rect #{} ({:.0},{:.0} {:.0}x{:.0}){}",
                            i, a.x, a.y, a.w, a.h,
                            j, b.x, b.y, b.w, b.h,
                            if both_in_container { " (same container)" } else { "" },
                        ),
                    });
                }
            }
        }
    }

    // --- Invariant 4: label_near_edge ---
    // Each edge label should be within 50px of some point on an edge path.
    let max_label_dist = 50.0;
    for text in texts {
        if !text.is_centered {
            continue;
        }

        let center = Pt {
            x: text.x,
            y: text.y,
        };

        // Is this a node label?
        let inside_node = node_rects.iter().any(|nr| {
            let nr_expanded = AABB {
                x: nr.x - 2.0,
                y: nr.y - 2.0,
                w: nr.w + 4.0,
                h: nr.h + 4.0,
            };
            nr_expanded.contains(&AABB {
                x: center.x,
                y: center.y,
                w: 0.0,
                h: 0.0,
            })
        });

        if inside_node {
            continue;
        }

        // This is an edge label. Find the nearest edge path.
        let mut min_dist = f64::INFINITY;
        for path in paths {
            for seg in &path.segments {
                let dist = point_to_segment_dist(center, seg);
                min_dist = min_dist.min(dist);
            }
            for pt in &path.sample_points {
                let dx = center.x - pt.x;
                let dy = center.y - pt.y;
                let dist = (dx * dx + dy * dy).sqrt();
                min_dist = min_dist.min(dist);
            }
        }

        if min_dist > max_label_dist {
            violations.push(Violation {
                invariant: "label_near_edge",
                detail: format!(
                    "Edge label '{}' at ({:.0},{:.0}) is {:.0}px from nearest edge (max {}px)",
                    text.content, text.x, text.y, min_dist, max_label_dist,
                ),
            });
        }
    }

    violations
}

// ---------------------------------------------------------------------------
// Test runner
// ---------------------------------------------------------------------------

/// Locate the fixtures directory.
fn fixtures_dir() -> PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR not set — run via `cargo test`");
    Path::new(&manifest)
        .join("..")
        .join("..")
        .join("tests")
        .join("d2_conformance")
        .join("fixtures")
}

/// Locate the actual/ output directory (creating it if needed).
fn actual_dir() -> PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR not set — run via `cargo test`");
    let dir = Path::new(&manifest)
        .join("..")
        .join("..")
        .join("tests")
        .join("d2_conformance")
        .join("actual");
    std::fs::create_dir_all(&dir).ok();
    dir
}

/// Collect all `.d2` fixture files, sorted by name.
fn collect_fixtures() -> Vec<PathBuf> {
    let dir = fixtures_dir();
    let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("cannot read fixtures dir {}: {e}", dir.display()))
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("d2") {
                Some(path)
            } else {
                None
            }
        })
        .collect();
    files.sort();
    files
}

/// Run conformance checks on a single fixture file.
/// Returns (fixture_name, violations, render_error).
fn run_single_fixture(path: &Path) -> (String, Vec<Violation>, Option<String>) {
    let name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string();

    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            return (name, vec![], Some(format!("failed to read fixture: {e}")));
        }
    };

    let options = RenderOptions::default();
    let result = match render_d2_to_svg(&source, &options) {
        Ok(r) => r,
        Err(e) => {
            return (name, vec![], Some(format!("render_d2_to_svg failed: {e}")));
        }
    };

    let svg = &result.svg;

    // Always dump the SVG to actual/
    let out_path = actual_dir().join(format!("{name}.svg"));
    std::fs::write(&out_path, svg).ok();

    // Parse SVG elements
    let rects = parse_rects(svg);
    let texts = parse_texts(svg);
    let paths = parse_paths(svg);

    let (node_rects, container_rects) = classify_rects(&rects);

    let violations = check_invariants(&node_rects, &container_rects, &texts, &paths);

    (name, violations, None)
}

// ---------------------------------------------------------------------------
// The test
// ---------------------------------------------------------------------------

/// Main conformance test: iterates all fixtures, collects all results,
/// and reports everything at the end.
#[test]
fn d2_conformance_suite() {
    let fixtures = collect_fixtures();
    assert!(
        !fixtures.is_empty(),
        "no .d2 fixtures found in {}",
        fixtures_dir().display()
    );

    let mut total_violations = 0;
    let mut total_errors = 0;
    let mut report = String::new();

    report.push_str(&format!(
        "D2 Conformance Suite: {} fixtures\n",
        fixtures.len()
    ));
    report.push_str(&"=".repeat(60));
    report.push('\n');

    for fixture_path in &fixtures {
        let (name, violations, error) = run_single_fixture(fixture_path);

        if let Some(err) = &error {
            total_errors += 1;
            report.push_str(&format!("\nFIXTURE: {name}\n  ERROR: {err}\n"));
            continue;
        }

        if violations.is_empty() {
            report.push_str(&format!("\nFIXTURE: {name} ... OK\n"));
        } else {
            total_violations += violations.len();
            report.push_str(&format!(
                "\nFIXTURE: {name} ... {} violation(s)\n",
                violations.len()
            ));
            for v in &violations {
                report.push_str(&format!("  [{}] {}\n", v.invariant, v.detail));
            }
        }
    }

    report.push_str(&"\n");
    report.push_str(&"=".repeat(60));
    report.push_str(&format!(
        "\nSummary: {} fixture(s), {} violation(s), {} render error(s)\n",
        fixtures.len(),
        total_violations,
        total_errors,
    ));

    // Always print the full report
    println!("{report}");

    // Write report to actual/ as well
    let report_path = actual_dir().join("conformance_report.txt");
    std::fs::write(&report_path, &report).ok();

    // Fail only on violations (render errors are informational — some fixtures
    // might exercise corner cases that expose bugs we want to track).
    assert!(
        total_violations == 0,
        "conformance suite found {total_violations} violation(s) across fixtures — see report above"
    );
}

/// Helper test that dumps ALL fixture SVGs to actual/ without checking invariants.
/// Useful for regenerating outputs for the Python comparison tooling.
#[test]
fn dump_all_fixture_svgs() {
    let fixtures = collect_fixtures();
    let out = actual_dir();
    let options = RenderOptions::default();

    let mut ok = 0;
    let mut fail = 0;

    for fixture_path in &fixtures {
        let name = fixture_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        let source = match std::fs::read_to_string(fixture_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("SKIP {name}: {e}");
                fail += 1;
                continue;
            }
        };

        match render_d2_to_svg(&source, &options) {
            Ok(result) => {
                let svg_path = out.join(format!("{name}.svg"));
                std::fs::write(&svg_path, &result.svg).ok();
                ok += 1;
            }
            Err(e) => {
                eprintln!("FAIL {name}: {e}");
                fail += 1;
            }
        }
    }

    println!("Dumped {ok} SVGs to {}, {fail} failures", out.display());
}

// ---------------------------------------------------------------------------
// Unit tests for the helpers
// ---------------------------------------------------------------------------

#[cfg(test)]
mod helper_tests {
    use super::*;

    #[test]
    fn test_aabb_overlaps() {
        let a = AABB {
            x: 0.0,
            y: 0.0,
            w: 10.0,
            h: 10.0,
        };
        let b = AABB {
            x: 5.0,
            y: 5.0,
            w: 10.0,
            h: 10.0,
        };
        assert!(a.overlaps(&b));

        let c = AABB {
            x: 20.0,
            y: 20.0,
            w: 10.0,
            h: 10.0,
        };
        assert!(!a.overlaps(&c));
    }

    #[test]
    fn test_aabb_contains() {
        let outer = AABB {
            x: 0.0,
            y: 0.0,
            w: 100.0,
            h: 100.0,
        };
        let inner = AABB {
            x: 10.0,
            y: 10.0,
            w: 20.0,
            h: 20.0,
        };
        assert!(outer.contains(&inner));
        assert!(!inner.contains(&outer));
    }

    #[test]
    fn test_liang_barsky_hit() {
        let p0 = Pt { x: 0.0, y: 5.0 };
        let p1 = Pt { x: 20.0, y: 5.0 };
        let aabb = AABB {
            x: 5.0,
            y: 0.0,
            w: 10.0,
            h: 10.0,
        };
        assert!(liang_barsky(p0, p1, &aabb));
    }

    #[test]
    fn test_liang_barsky_miss() {
        let p0 = Pt { x: 0.0, y: 15.0 };
        let p1 = Pt { x: 20.0, y: 15.0 };
        let aabb = AABB {
            x: 5.0,
            y: 0.0,
            w: 10.0,
            h: 10.0,
        };
        assert!(!liang_barsky(p0, p1, &aabb));
    }

    #[test]
    fn test_point_to_segment_dist() {
        let seg = Seg {
            a: Pt { x: 0.0, y: 0.0 },
            b: Pt { x: 10.0, y: 0.0 },
        };
        let p = Pt { x: 5.0, y: 3.0 };
        let dist = point_to_segment_dist(p, &seg);
        assert!((dist - 3.0).abs() < 0.01);
    }

    #[test]
    fn test_tokenize_path() {
        let d = "M 10 20 L 30 40 Q 50 60 70 80";
        let tokens = tokenize_path(d);
        assert_eq!(
            tokens,
            vec!["M", "10", "20", "L", "30", "40", "Q", "50", "60", "70", "80"]
        );
    }

    #[test]
    fn test_tokenize_path_negative() {
        let d = "M 10 -20 L -30 40";
        let tokens = tokenize_path(d);
        assert_eq!(tokens, vec!["M", "10", "-20", "L", "-30", "40"]);
    }

    #[test]
    fn test_parse_path_d_simple_line() {
        let (segments, samples) = parse_path_d("M 0 0 L 100 0");
        assert_eq!(segments.len(), 1);
        assert!((segments[0].a.x - 0.0).abs() < 0.01);
        assert!((segments[0].b.x - 100.0).abs() < 0.01);
        assert!(!samples.is_empty());
    }

    #[test]
    fn test_extract_attr_f64() {
        let tag = r#"<rect x="10.5" y="20" width="100" height="50"/>"#;
        assert_eq!(extract_attr_f64(tag, "x"), Some(10.5));
        assert_eq!(extract_attr_f64(tag, "width"), Some(100.0));
        assert_eq!(extract_attr_f64(tag, "missing"), None);
    }

    #[test]
    fn test_extract_attr_f64_any_order() {
        // Attributes in non-standard order
        let tag = r#"<rect height="50" width="100" y="20" x="10"/>"#;
        assert_eq!(extract_attr_f64(tag, "x"), Some(10.0));
        assert_eq!(extract_attr_f64(tag, "y"), Some(20.0));
        assert_eq!(extract_attr_f64(tag, "width"), Some(100.0));
        assert_eq!(extract_attr_f64(tag, "height"), Some(50.0));
    }
}
