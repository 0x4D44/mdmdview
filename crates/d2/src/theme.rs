//! Default colors and dark/light themes for D2 rendering.
//!
//! Theme provides default fill, stroke, font colors, and background
//! for both dark and light mode rendering. User-specified styles
//! override theme defaults.

use crate::graph::Color;

/// Theme with default colors for D2 rendering.
pub struct Theme {
    /// Default node fill.
    pub node_fill: Color,
    /// Default node stroke.
    pub node_stroke: Color,
    /// Default node label color.
    pub font_color: Color,
    /// Default edge stroke.
    pub edge_color: Color,
    /// Default container fill (slightly different from node).
    pub container_fill: Color,
    /// Background color for the diagram.
    pub background: Color,
    /// Default font size in pixels.
    pub font_size: f64,
    /// Default stroke width.
    pub stroke_width: f64,
}

impl Theme {
    pub fn dark() -> Self {
        Self {
            node_fill: Color::Hex(0x2A, 0x2A, 0x2A),
            node_stroke: Color::Hex(0x88, 0x88, 0x88),
            font_color: Color::Hex(0xE0, 0xE0, 0xE0),
            edge_color: Color::Hex(0x88, 0x88, 0x88),
            container_fill: Color::Hex(0x1A, 0x1A, 0x1A),
            background: Color::Hex(0x00, 0x00, 0x00),
            font_size: 14.0,
            stroke_width: 2.0,
        }
    }

    pub fn light() -> Self {
        Self {
            node_fill: Color::Hex(0xFF, 0xFF, 0xFF),
            node_stroke: Color::Hex(0x33, 0x33, 0x33),
            font_color: Color::Hex(0x17, 0x17, 0x17),
            edge_color: Color::Hex(0x33, 0x33, 0x33),
            container_fill: Color::Hex(0xF5, 0xF5, 0xF5),
            background: Color::Hex(0xFF, 0xFF, 0xFF),
            font_size: 14.0,
            stroke_width: 2.0,
        }
    }

    /// Select theme based on dark_mode flag.
    pub fn for_mode(dark_mode: bool) -> Self {
        if dark_mode {
            Self::dark()
        } else {
            Self::light()
        }
    }
}
