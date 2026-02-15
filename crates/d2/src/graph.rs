//! Graph IR types for D2 diagram compilation and layout.
//!
//! The graph IR is the output of the compiler (Phase 2) and the input
//! to the layout engine (Phase 4). It uses petgraph's `StableDiGraph`
//! as the backbone, with `D2Object` for nodes and `D2EdgeData` for edges.
//!
//! Key types:
//! - `D2Graph`: the complete diagram graph with hierarchy
//! - `D2Object`: a shape or container
//! - `D2EdgeData`: a connection/edge with routing data
//! - `Style`: visual properties (fill, stroke, etc.)
//! - `ShapeType`, `Direction`, `ArrowheadType`: enums

use crate::geo::{Point, Rect};
use petgraph::stable_graph::{EdgeIndex, NodeIndex, StableDiGraph};

// ---------------------------------------------------------------------------
// Source position tracking
// ---------------------------------------------------------------------------

/// Source position (0-indexed).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

/// Source range.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

// ---------------------------------------------------------------------------
// D2Graph
// ---------------------------------------------------------------------------

/// The complete diagram graph.
pub struct D2Graph {
    /// petgraph backing store.
    pub graph: StableDiGraph<D2Object, D2EdgeData>,

    /// Root object (implicit top-level container).
    /// The root is special: it has no shape, no label, no border.
    /// It exists only as the parent of top-level objects.
    /// Its `box_` is set to the bounding box of all children after layout.
    /// The SVG renderer must NOT draw a shape or label for the root.
    pub root: NodeIndex,

    /// All objects in declaration order (preserved for deterministic iteration).
    pub objects: Vec<NodeIndex>,

    /// All edges in declaration order (preserved for deterministic rendering).
    pub edges: Vec<EdgeIndex>,

    /// Top-level layout direction (default: Down).
    pub direction: Direction,

    /// Non-fatal warnings accumulated during compilation and layout.
    pub warnings: Vec<D2Warning>,
}

impl D2Graph {
    /// Create a new empty graph with a root node.
    pub fn new() -> Self {
        let mut graph = StableDiGraph::new();
        let root = graph.add_node(D2Object {
            id: String::new(),
            label: String::new(),
            shape: ShapeType::Rectangle,
            style: Style::default(),
            parent: None,
            children: Vec::new(),
            direction: None,
            box_: None,
            label_dimensions: None,
            label_lines: Vec::new(),
            is_container: true,
        });
        D2Graph {
            graph,
            root,
            objects: Vec::new(),
            edges: Vec::new(),
            direction: Direction::Down,
            warnings: Vec::new(),
        }
    }
}

impl Default for D2Graph {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// D2Object
// ---------------------------------------------------------------------------

/// A shape/container in the diagram.
pub struct D2Object {
    /// Fully qualified id: "group.server"
    pub id: String,
    /// Display text.
    pub label: String,
    /// Shape type (rectangle, circle, etc.)
    pub shape: ShapeType,
    /// Visual properties.
    pub style: Style,

    // Hierarchy
    pub parent: Option<NodeIndex>,
    pub children: Vec<NodeIndex>,

    /// Per-container direction override (None = inherit from parent/graph).
    pub direction: Option<Direction>,

    // Geometry (populated during layout)
    /// Position + size after layout.
    pub box_: Option<Rect>,
    /// Measured text size (pre-layout).
    pub label_dimensions: Option<(f64, f64)>,
    /// Label text after wrapping (pre-layout).
    pub label_lines: Vec<String>,

    /// Whether this object is a container (has children).
    pub is_container: bool,
}

// ---------------------------------------------------------------------------
// D2EdgeData
// ---------------------------------------------------------------------------

/// An edge/connection.
pub struct D2EdgeData {
    pub label: Option<String>,
    pub src_arrow: ArrowheadType,
    pub dst_arrow: ArrowheadType,
    pub style: Style,

    /// Cubic Bézier control points for the edge path.
    /// Format: [start, ctrl1, ctrl2, end, ctrl1, ctrl2, end, ...]
    pub route: Vec<Point>,
    /// Position for the edge label.
    pub label_position: Option<Point>,
}

// ---------------------------------------------------------------------------
// Enums
// ---------------------------------------------------------------------------

/// Shape types supported in v1.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShapeType {
    Rectangle,
    Square,
    Circle,
    Oval,
    Diamond,
    Hexagon,
    Cylinder,
    Cloud,
    Person,
    Package,
    Queue,
    Page,
    Parallelogram,
    Document,
    Step,
    Callout,
    StoredData,
    Text,
    Code,
}

impl Default for ShapeType {
    fn default() -> Self {
        ShapeType::Rectangle
    }
}

impl ShapeType {
    /// Parse a shape type from a D2 string value.
    pub fn from_str(s: &str) -> Option<ShapeType> {
        match s {
            "rectangle" => Some(ShapeType::Rectangle),
            "square" => Some(ShapeType::Square),
            "circle" => Some(ShapeType::Circle),
            "oval" => Some(ShapeType::Oval),
            "diamond" => Some(ShapeType::Diamond),
            "hexagon" => Some(ShapeType::Hexagon),
            "cylinder" => Some(ShapeType::Cylinder),
            "cloud" => Some(ShapeType::Cloud),
            "person" => Some(ShapeType::Person),
            "package" => Some(ShapeType::Package),
            "queue" => Some(ShapeType::Queue),
            "page" => Some(ShapeType::Page),
            "parallelogram" => Some(ShapeType::Parallelogram),
            "document" => Some(ShapeType::Document),
            "step" => Some(ShapeType::Step),
            "callout" => Some(ShapeType::Callout),
            "stored_data" => Some(ShapeType::StoredData),
            "text" => Some(ShapeType::Text),
            "code" => Some(ShapeType::Code),
            _ => None,
        }
    }
}

/// Layout direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Down,
    Up,
    Right,
    Left,
}

impl Default for Direction {
    fn default() -> Self {
        Direction::Down
    }
}

impl Direction {
    /// Parse a direction from a D2 string value.
    pub fn from_str(s: &str) -> Option<Direction> {
        match s {
            "down" => Some(Direction::Down),
            "up" => Some(Direction::Up),
            "right" => Some(Direction::Right),
            "left" => Some(Direction::Left),
            _ => None,
        }
    }

    /// Whether this direction is horizontal (Left or Right).
    pub fn is_horizontal(&self) -> bool {
        matches!(self, Direction::Left | Direction::Right)
    }
}

/// Arrowhead types supported in v1.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArrowheadType {
    Arrow,
    Triangle,
    Diamond,
    FilledDiamond,
    Circle,
    FilledCircle,
    CfOne,
    CfMany,
    CfOneRequired,
    CfManyRequired,
    None,
}

impl Default for ArrowheadType {
    fn default() -> Self {
        ArrowheadType::Arrow
    }
}

impl ArrowheadType {
    /// Parse an arrowhead type from a D2 string value.
    pub fn from_str(s: &str) -> Option<ArrowheadType> {
        match s {
            "arrow" => Some(ArrowheadType::Arrow),
            "triangle" => Some(ArrowheadType::Triangle),
            "diamond" => Some(ArrowheadType::Diamond),
            "filled-diamond" => Some(ArrowheadType::FilledDiamond),
            "circle" => Some(ArrowheadType::Circle),
            "filled-circle" => Some(ArrowheadType::FilledCircle),
            "cf-one" => Some(ArrowheadType::CfOne),
            "cf-many" => Some(ArrowheadType::CfMany),
            "cf-one-required" => Some(ArrowheadType::CfOneRequired),
            "cf-many-required" => Some(ArrowheadType::CfManyRequired),
            "none" => Some(ArrowheadType::None),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// Color
// ---------------------------------------------------------------------------

/// Color representation.
/// Named colors are resolved to RGB during compilation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
    Hex(u8, u8, u8),
    HexAlpha(u8, u8, u8, u8),
}

impl Color {
    /// Format as SVG color string (e.g., "#ff0000").
    pub fn to_svg_string(&self) -> String {
        match self {
            Color::Hex(r, g, b) => format!("#{:02x}{:02x}{:02x}", r, g, b),
            Color::HexAlpha(r, g, b, a) => format!("#{:02x}{:02x}{:02x}{:02x}", r, g, b, a),
        }
    }

    /// Parse a hex color string (e.g., "#ff0000" or "ff0000").
    pub fn from_hex(s: &str) -> Option<Color> {
        let s = s.strip_prefix('#').unwrap_or(s);
        match s.len() {
            6 => {
                let r = u8::from_str_radix(&s[0..2], 16).ok()?;
                let g = u8::from_str_radix(&s[2..4], 16).ok()?;
                let b = u8::from_str_radix(&s[4..6], 16).ok()?;
                Some(Color::Hex(r, g, b))
            }
            8 => {
                let r = u8::from_str_radix(&s[0..2], 16).ok()?;
                let g = u8::from_str_radix(&s[2..4], 16).ok()?;
                let b = u8::from_str_radix(&s[4..6], 16).ok()?;
                let a = u8::from_str_radix(&s[6..8], 16).ok()?;
                Some(Color::HexAlpha(r, g, b, a))
            }
            _ => None,
        }
    }

    /// Resolve a named CSS/SVG color to hex.
    pub fn from_named(name: &str) -> Option<Color> {
        let lower = name.to_ascii_lowercase();
        // SVG named color table (subset of the 148 CSS colors, covering common usage)
        match lower.as_str() {
            "black" => Some(Color::Hex(0, 0, 0)),
            "white" => Some(Color::Hex(255, 255, 255)),
            "red" => Some(Color::Hex(255, 0, 0)),
            "green" => Some(Color::Hex(0, 128, 0)),
            "blue" => Some(Color::Hex(0, 0, 255)),
            "yellow" => Some(Color::Hex(255, 255, 0)),
            "cyan" | "aqua" => Some(Color::Hex(0, 255, 255)),
            "magenta" | "fuchsia" => Some(Color::Hex(255, 0, 255)),
            "orange" => Some(Color::Hex(255, 165, 0)),
            "purple" => Some(Color::Hex(128, 0, 128)),
            "pink" => Some(Color::Hex(255, 192, 203)),
            "brown" => Some(Color::Hex(165, 42, 42)),
            "gray" | "grey" => Some(Color::Hex(128, 128, 128)),
            "lightgray" | "lightgrey" => Some(Color::Hex(211, 211, 211)),
            "darkgray" | "darkgrey" => Some(Color::Hex(169, 169, 169)),
            "navy" => Some(Color::Hex(0, 0, 128)),
            "teal" => Some(Color::Hex(0, 128, 128)),
            "olive" => Some(Color::Hex(128, 128, 0)),
            "maroon" => Some(Color::Hex(128, 0, 0)),
            "silver" => Some(Color::Hex(192, 192, 192)),
            "lime" => Some(Color::Hex(0, 255, 0)),
            "coral" => Some(Color::Hex(255, 127, 80)),
            "salmon" => Some(Color::Hex(250, 128, 114)),
            "gold" => Some(Color::Hex(255, 215, 0)),
            "indigo" => Some(Color::Hex(75, 0, 130)),
            "violet" => Some(Color::Hex(238, 130, 238)),
            "turquoise" => Some(Color::Hex(64, 224, 208)),
            "tomato" => Some(Color::Hex(255, 99, 71)),
            "skyblue" => Some(Color::Hex(135, 206, 235)),
            "steelblue" => Some(Color::Hex(70, 130, 180)),
            "slategray" | "slategrey" => Some(Color::Hex(112, 128, 144)),
            "royalblue" => Some(Color::Hex(65, 105, 225)),
            "plum" => Some(Color::Hex(221, 160, 221)),
            "orchid" => Some(Color::Hex(218, 112, 214)),
            "orangered" => Some(Color::Hex(255, 69, 0)),
            "mediumblue" => Some(Color::Hex(0, 0, 205)),
            "limegreen" => Some(Color::Hex(50, 205, 50)),
            "lightblue" => Some(Color::Hex(173, 216, 230)),
            "khaki" => Some(Color::Hex(240, 230, 140)),
            "ivory" => Some(Color::Hex(255, 255, 240)),
            "hotpink" => Some(Color::Hex(255, 105, 180)),
            "honeydew" => Some(Color::Hex(240, 255, 240)),
            "greenyellow" => Some(Color::Hex(173, 255, 47)),
            "forestgreen" => Some(Color::Hex(34, 139, 34)),
            "firebrick" => Some(Color::Hex(178, 34, 34)),
            "dodgerblue" => Some(Color::Hex(30, 144, 255)),
            "dimgray" | "dimgrey" => Some(Color::Hex(105, 105, 105)),
            "deeppink" => Some(Color::Hex(255, 20, 147)),
            "deepskyblue" => Some(Color::Hex(0, 191, 255)),
            "darkred" => Some(Color::Hex(139, 0, 0)),
            "darkgreen" => Some(Color::Hex(0, 100, 0)),
            "darkblue" => Some(Color::Hex(0, 0, 139)),
            "crimson" => Some(Color::Hex(220, 20, 60)),
            "cornflowerblue" => Some(Color::Hex(100, 149, 237)),
            "chocolate" => Some(Color::Hex(210, 105, 30)),
            "chartreuse" => Some(Color::Hex(127, 255, 0)),
            "cadetblue" => Some(Color::Hex(95, 158, 160)),
            "burlywood" => Some(Color::Hex(222, 184, 135)),
            "blueviolet" => Some(Color::Hex(138, 43, 226)),
            "bisque" => Some(Color::Hex(255, 228, 196)),
            "beige" => Some(Color::Hex(245, 245, 220)),
            "azure" => Some(Color::Hex(240, 255, 255)),
            "aquamarine" => Some(Color::Hex(127, 255, 212)),
            "antiquewhite" => Some(Color::Hex(250, 235, 215)),
            "aliceblue" => Some(Color::Hex(240, 248, 255)),
            "wheat" => Some(Color::Hex(245, 222, 179)),
            "tan" => Some(Color::Hex(210, 180, 140)),
            "snow" => Some(Color::Hex(255, 250, 250)),
            "sienna" => Some(Color::Hex(160, 82, 45)),
            "seagreen" => Some(Color::Hex(46, 139, 87)),
            "sandybrown" => Some(Color::Hex(244, 164, 96)),
            "rosybrown" => Some(Color::Hex(188, 143, 143)),
            "powderblue" => Some(Color::Hex(176, 224, 230)),
            "peru" => Some(Color::Hex(205, 133, 63)),
            "peachpuff" => Some(Color::Hex(255, 218, 185)),
            "papayawhip" => Some(Color::Hex(255, 239, 213)),
            "palegreen" => Some(Color::Hex(152, 251, 152)),
            "olivedrab" => Some(Color::Hex(107, 142, 35)),
            "oldlace" => Some(Color::Hex(253, 245, 230)),
            "navajowhite" => Some(Color::Hex(255, 222, 173)),
            "moccasin" => Some(Color::Hex(255, 228, 181)),
            "mistyrose" => Some(Color::Hex(255, 228, 225)),
            "mintcream" => Some(Color::Hex(245, 255, 250)),
            "midnightblue" => Some(Color::Hex(25, 25, 112)),
            "mediumseagreen" => Some(Color::Hex(60, 179, 113)),
            "mediumpurple" => Some(Color::Hex(147, 112, 219)),
            "mediumorchid" => Some(Color::Hex(186, 85, 211)),
            "linen" => Some(Color::Hex(250, 240, 230)),
            "lightyellow" => Some(Color::Hex(255, 255, 224)),
            "lightsteelblue" => Some(Color::Hex(176, 196, 222)),
            "lightsalmon" => Some(Color::Hex(255, 160, 122)),
            "lightpink" => Some(Color::Hex(255, 182, 193)),
            "lightgreen" => Some(Color::Hex(144, 238, 144)),
            "lightcyan" => Some(Color::Hex(224, 255, 255)),
            "lightcoral" => Some(Color::Hex(240, 128, 128)),
            "lemonchiffon" => Some(Color::Hex(255, 250, 205)),
            "lavender" => Some(Color::Hex(230, 230, 250)),
            "lavenderblush" => Some(Color::Hex(255, 240, 245)),
            "ghostwhite" => Some(Color::Hex(248, 248, 255)),
            "gainsboro" => Some(Color::Hex(220, 220, 220)),
            "floralwhite" => Some(Color::Hex(255, 250, 240)),
            "darkorange" => Some(Color::Hex(255, 140, 0)),
            "darkolivegreen" => Some(Color::Hex(85, 107, 47)),
            "darkmagenta" => Some(Color::Hex(139, 0, 139)),
            "darkkhaki" => Some(Color::Hex(189, 183, 107)),
            "darkgoldenrod" => Some(Color::Hex(184, 134, 11)),
            "darkcyan" => Some(Color::Hex(0, 139, 139)),
            "darkviolet" => Some(Color::Hex(148, 0, 211)),
            "darkturquoise" => Some(Color::Hex(0, 206, 209)),
            "darkslategray" | "darkslategrey" => Some(Color::Hex(47, 79, 79)),
            "darkslateblue" => Some(Color::Hex(72, 61, 139)),
            "darksalmon" => Some(Color::Hex(233, 150, 122)),
            "darkseagreen" => Some(Color::Hex(143, 188, 143)),
            "darkorchid" => Some(Color::Hex(153, 50, 204)),
            "cornsilk" => Some(Color::Hex(255, 248, 220)),
            "blanchedalmond" => Some(Color::Hex(255, 235, 205)),
            "mediumaquamarine" => Some(Color::Hex(102, 205, 170)),
            "mediumslateblue" => Some(Color::Hex(123, 104, 238)),
            "mediumspringgreen" => Some(Color::Hex(0, 250, 154)),
            "mediumturquoise" => Some(Color::Hex(72, 209, 204)),
            "mediumvioletred" => Some(Color::Hex(199, 21, 133)),
            "paleturquoise" => Some(Color::Hex(175, 238, 238)),
            "palevioletred" => Some(Color::Hex(219, 112, 147)),
            "palegoldenrod" => Some(Color::Hex(238, 232, 170)),
            "springgreen" => Some(Color::Hex(0, 255, 127)),
            "yellowgreen" => Some(Color::Hex(154, 205, 50)),
            "whitesmoke" => Some(Color::Hex(245, 245, 245)),
            "seashell" => Some(Color::Hex(255, 245, 238)),
            "lawngreen" => Some(Color::Hex(124, 252, 0)),
            _ => None,
        }
    }

    /// Try to parse a color from either hex or named format.
    pub fn parse(s: &str) -> Option<Color> {
        if s.starts_with('#') || (s.chars().all(|c| c.is_ascii_hexdigit()) && s.len() >= 6) {
            Color::from_hex(s)
        } else {
            Color::from_named(s)
        }
    }
}

// ---------------------------------------------------------------------------
// Style
// ---------------------------------------------------------------------------

/// Visual style properties for shapes and edges.
#[derive(Debug, Clone)]
pub struct Style {
    pub fill: Option<Color>,
    pub stroke: Option<Color>,
    pub stroke_width: Option<f64>,
    pub stroke_dash: Option<f64>,
    pub font_size: Option<f64>,
    pub font_color: Option<Color>,
    pub bold: bool,
    pub italic: bool,
    pub underline: bool,
    pub border_radius: Option<f64>,
    pub opacity: Option<f64>,
    pub shadow: bool,
    pub three_d: bool,
    pub multiple: bool,
    pub animated: bool,
    pub double_border: bool,
}

impl Default for Style {
    fn default() -> Self {
        Self {
            fill: None,
            stroke: None,
            stroke_width: None,
            stroke_dash: None,
            font_size: None,
            font_color: None,
            bold: false,
            italic: false,
            underline: false,
            border_radius: None,
            opacity: None,
            shadow: false,
            three_d: false,
            multiple: false,
            animated: false,
            double_border: false,
        }
    }
}

// ---------------------------------------------------------------------------
// D2Warning
// ---------------------------------------------------------------------------

/// Non-fatal warning (e.g., "feature not yet supported").
#[derive(Debug)]
pub struct D2Warning {
    pub message: String,
    pub range: Option<Range>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_color_from_hex() {
        assert_eq!(Color::from_hex("#ff0000"), Some(Color::Hex(255, 0, 0)));
        assert_eq!(Color::from_hex("00ff00"), Some(Color::Hex(0, 255, 0)));
        assert_eq!(
            Color::from_hex("#ff000080"),
            Some(Color::HexAlpha(255, 0, 0, 128))
        );
        assert_eq!(Color::from_hex("xyz"), None);
    }

    #[test]
    fn test_color_from_named() {
        assert_eq!(Color::from_named("red"), Some(Color::Hex(255, 0, 0)));
        assert_eq!(Color::from_named("Red"), Some(Color::Hex(255, 0, 0)));
        assert_eq!(Color::from_named("nonexistent"), None);
    }

    #[test]
    fn test_color_to_svg() {
        assert_eq!(Color::Hex(255, 0, 0).to_svg_string(), "#ff0000");
        assert_eq!(
            Color::HexAlpha(255, 0, 0, 128).to_svg_string(),
            "#ff000080"
        );
    }

    #[test]
    fn test_shape_type_from_str() {
        assert_eq!(ShapeType::from_str("rectangle"), Some(ShapeType::Rectangle));
        assert_eq!(ShapeType::from_str("circle"), Some(ShapeType::Circle));
        assert_eq!(ShapeType::from_str("nonexistent"), None);
    }

    #[test]
    fn test_direction_from_str() {
        assert_eq!(Direction::from_str("down"), Some(Direction::Down));
        assert_eq!(Direction::from_str("right"), Some(Direction::Right));
        assert!(Direction::Right.is_horizontal());
        assert!(!Direction::Down.is_horizontal());
    }
}
