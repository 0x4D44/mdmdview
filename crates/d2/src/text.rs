//! Text measurement and wrapping for D2 label sizing.
//!
//! Uses heuristic measurement based on average character widths for
//! proportional fonts, with word-wrapping for long text. This doesn't
//! need to be pixel-perfect — the layout just needs reasonable width/height
//! estimates. The final text rendering is done by resvg with real font metrics.

use crate::graph::ShapeType;

/// Maximum label width before wrapping (pixels at default font size of 14px).
const MAX_LABEL_WIDTH: f64 = 200.0;

/// Average character width multiplier for proportional fonts.
/// Arial at 14px: average ~7.5px per character → 0.55 * font_size.
const CHAR_WIDTH_FACTOR: f64 = 0.55;

/// Line height multiplier.
const LINE_HEIGHT_FACTOR: f64 = 1.4;

/// Result of text measurement.
pub struct TextMeasurement {
    /// Wrapped lines of text.
    pub lines: Vec<String>,
    /// Total text width in pixels.
    pub width: f64,
    /// Total text height in pixels.
    pub height: f64,
}

/// Measure and wrap a label, returning wrapped lines and total dimensions.
pub fn measure_and_wrap_label(text: &str, font_size: f64, _font_family: &str) -> TextMeasurement {
    if text.is_empty() {
        return TextMeasurement {
            lines: Vec::new(),
            width: 0.0,
            height: 0.0,
        };
    }

    let avg_char_width = font_size * CHAR_WIDTH_FACTOR;
    let line_height = font_size * LINE_HEIGHT_FACTOR;
    let max_width = MAX_LABEL_WIDTH * (font_size / 14.0);

    let mut lines = Vec::new();
    let mut current_line = String::new();
    let mut current_width = 0.0;

    for word in text.split_whitespace() {
        let word_width = word.chars().count() as f64 * avg_char_width;
        if current_width + word_width > max_width && !current_line.is_empty() {
            lines.push(current_line.clone());
            current_line.clear();
            current_width = 0.0;
        }
        if !current_line.is_empty() {
            current_line.push(' ');
            current_width += avg_char_width;
        }
        current_line.push_str(word);
        current_width += word_width;
    }
    if !current_line.is_empty() {
        lines.push(current_line);
    }

    let width = lines
        .iter()
        .map(|l| l.chars().count() as f64 * avg_char_width)
        .fold(0.0f64, f64::max);
    let height = lines.len() as f64 * line_height;

    TextMeasurement {
        lines,
        width,
        height,
    }
}

/// Shape padding: (horizontal, vertical) in pixels.
pub fn shape_padding(shape: ShapeType) -> (f64, f64) {
    match shape {
        ShapeType::Circle | ShapeType::Oval => (30.0, 20.0),
        ShapeType::Diamond => (40.0, 30.0),
        ShapeType::Hexagon => (25.0, 15.0),
        ShapeType::Cylinder => (20.0, 30.0),
        ShapeType::Person => (20.0, 30.0),
        ShapeType::Text => (0.0, 0.0),
        _ => (20.0, 12.0), // Rectangle/Square and all others
    }
}

/// Compute node dimensions from label + shape padding + optional user overrides.
pub fn compute_node_dimensions(
    label: &str,
    font_size: f64,
    font_family: &str,
    shape: ShapeType,
    user_width: Option<f64>,
    user_height: Option<f64>,
) -> TextMeasurement {
    let mut measurement = measure_and_wrap_label(label, font_size, font_family);
    let (h_pad, v_pad) = shape_padding(shape);

    // Apply shape padding
    measurement.width += h_pad * 2.0;
    measurement.height += v_pad * 2.0;

    // Enforce minimums
    measurement.width = measurement.width.max(60.0);
    measurement.height = measurement.height.max(36.0);

    // Special shapes: enforce aspect ratio
    match shape {
        ShapeType::Square => {
            let size = measurement.width.max(measurement.height);
            measurement.width = size;
            measurement.height = size;
        }
        ShapeType::Circle => {
            let diameter = measurement.width.max(measurement.height);
            measurement.width = diameter;
            measurement.height = diameter;
        }
        _ => {}
    }

    // Apply user overrides
    if let Some(w) = user_width {
        measurement.width = w;
    }
    if let Some(h) = user_height {
        measurement.height = h;
    }

    measurement
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_label() {
        let m = measure_and_wrap_label("", 14.0, "Arial");
        assert!(m.lines.is_empty());
        assert_eq!(m.width, 0.0);
        assert_eq!(m.height, 0.0);
    }

    #[test]
    fn test_short_label() {
        let m = measure_and_wrap_label("Hello", 14.0, "Arial");
        assert_eq!(m.lines.len(), 1);
        assert_eq!(m.lines[0], "Hello");
        assert!(m.width > 0.0);
        assert!(m.height > 0.0);
    }

    #[test]
    fn test_long_label_wraps() {
        let m = measure_and_wrap_label(
            "This is a very long label that should wrap onto multiple lines",
            14.0,
            "Arial",
        );
        assert!(m.lines.len() > 1);
    }

    #[test]
    fn test_shape_padding() {
        let (h, v) = shape_padding(ShapeType::Diamond);
        assert_eq!(h, 40.0);
        assert_eq!(v, 30.0);

        let (h, v) = shape_padding(ShapeType::Text);
        assert_eq!(h, 0.0);
        assert_eq!(v, 0.0);
    }

    #[test]
    fn test_node_dimensions_minimum() {
        let m = compute_node_dimensions("x", 14.0, "Arial", ShapeType::Rectangle, None, None);
        assert!(m.width >= 60.0);
        assert!(m.height >= 36.0);
    }

    #[test]
    fn test_node_dimensions_user_override() {
        let m =
            compute_node_dimensions("x", 14.0, "Arial", ShapeType::Rectangle, Some(300.0), None);
        assert_eq!(m.width, 300.0);
    }

    #[test]
    fn test_square_enforces_aspect() {
        let m = compute_node_dimensions("Hello", 14.0, "Arial", ShapeType::Square, None, None);
        assert!((m.width - m.height).abs() < 0.001);
    }
}
