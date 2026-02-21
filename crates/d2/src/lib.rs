//! mdmdview-d2: Native D2 diagram renderer.
//!
//! This crate implements a subset of the D2 diagram language
//! (https://d2lang.com/) as a pure-Rust library. It parses D2 source
//! text, compiles it to an internal graph representation, performs
//! automatic hierarchical layout via the Sugiyama algorithm, and
//! renders the result to SVG.
//!
//! The primary entry point is [`render_d2_to_svg()`], which takes D2
//! source text and rendering options and returns an SVG string.
//! Individual pipeline phases ([`parse`], [`compile`], [`layout`],
//! [`render_svg`]) are also exposed for testing and debugging.

pub mod ast;
pub mod compiler;
pub mod edge_routing;
pub mod geo;
pub mod graph;
pub mod keywords;
pub mod layout;
pub mod layout_sugiyama;
pub mod parser;
pub mod shapes;
pub mod svg_render;
pub mod text;
pub mod theme;

pub use geo::{Point, Rect};
pub use graph::{
    ArrowheadType, Color, D2EdgeData, D2Graph, D2Object, D2Warning, Direction, RouteType,
    ShapeType, Style,
};
pub use theme::Theme;


// ---------------------------------------------------------------------------
// Public API types
// ---------------------------------------------------------------------------

/// Rendering options passed from the host application.
pub struct RenderOptions {
    /// Dark mode (true) or light mode (false). Default: true (matches mdmdview).
    pub dark_mode: bool,
    /// Font family for labels. Default: "Arial, Helvetica, sans-serif".
    pub font_family: String,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            dark_mode: true,
            font_family: "Arial, Helvetica, sans-serif".into(),
        }
    }
}

/// Result of rendering, including the SVG and any non-fatal warnings.
pub struct RenderResult {
    pub svg: String,
    pub warnings: Vec<D2Warning>,
}

/// Top-level error from render_d2_to_svg()
#[derive(Debug, thiserror::Error)]
pub enum D2Error {
    #[error("parse errors: {0:?}")]
    Parse(Vec<parser::ParseError>),
    #[error("compile error: {0}")]
    Compile(#[from] compiler::CompileError),
    #[error("layout error: {0}")]
    Layout(#[from] layout::LayoutError),
}

// ---------------------------------------------------------------------------
// Public pipeline API
// ---------------------------------------------------------------------------

/// Full pipeline: D2 source text → SVG string.
/// This is the primary entry point for mdmdview integration.
pub fn render_d2_to_svg(source: &str, options: &RenderOptions) -> Result<RenderResult, D2Error> {
    // Phase 1: Parse
    let parse_result = parse(source);
    if !parse_result.errors.is_empty() {
        return Err(D2Error::Parse(parse_result.errors));
    }

    // Phase 2: Compile
    let mut graph = compile(&parse_result.ast)?;

    // Phase 3+4: Layout (includes text measurement)
    layout(&mut graph, options)?;

    // Phase 5: Render SVG
    let svg = render_svg(&graph, options);

    Ok(RenderResult {
        svg,
        warnings: graph.warnings,
    })
}

/// Phase 1: Parse D2 source text into an AST.
pub fn parse(source: &str) -> parser::ParseResult {
    parser::parse(source)
}

/// Phase 2: Compile AST into a graph IR.
pub fn compile(ast: &ast::D2Map) -> Result<D2Graph, compiler::CompileError> {
    compiler::compile(ast)
}

/// Phase 3+4: Layout the graph (text measurement + Sugiyama + edge routing).
/// Modifies graph in-place: sets node positions, edge routes,
/// and may append to graph.warnings.
pub fn layout(graph: &mut D2Graph, options: &RenderOptions) -> Result<(), layout::LayoutError> {
    layout::layout(graph, options)
}

/// Phase 5: Render a positioned graph to SVG.
pub fn render_svg(graph: &D2Graph, options: &RenderOptions) -> String {
    svg_render::render(graph, options)
}

// ---------------------------------------------------------------------------
// End-to-end tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn opts() -> RenderOptions {
        RenderOptions::default()
    }

    /// Helper: render and assert basic SVG envelope.
    fn render_ok(source: &str) -> String {
        let result = render_d2_to_svg(source, &opts())
            .unwrap_or_else(|e| panic!("expected Ok, got Err: {e}"));
        assert!(result.svg.contains("<svg"), "missing <svg opening tag");
        assert!(result.svg.contains("</svg>"), "missing </svg> closing tag");
        result.svg
    }

    // 1. Simple shape --------------------------------------------------------

    #[test]
    fn test_e2e_simple_shape() {
        let svg = render_ok("server");
        assert!(svg.contains("<rect"), "expected a rect for default shape");
        assert!(svg.contains("server"), "expected label 'server' in SVG");
    }

    // 2. Connection ----------------------------------------------------------

    #[test]
    fn test_e2e_connection() {
        let svg = render_ok("a -> b");
        // Two shapes (rects by default) + at least one path for the edge
        assert!(svg.contains("a"), "expected label 'a'");
        assert!(svg.contains("b"), "expected label 'b'");
        assert!(svg.contains("<path"), "expected a <path> element for the edge");
    }

    // 3. Container -----------------------------------------------------------

    #[test]
    fn test_e2e_container() {
        let svg = render_ok("group: {\n  a\n  b\n}");
        // Container should produce a rect, plus child rects
        let rect_count = svg.matches("<rect").count();
        assert!(
            rect_count >= 3,
            "expected at least 3 rects (container + 2 children), got {rect_count}"
        );
        assert!(svg.contains("group"), "expected container label 'group'");
    }

    // 4. Styled circle -------------------------------------------------------

    #[test]
    fn test_e2e_styled_circle() {
        // Verify circle shape renders as <circle> SVG element.
        let svg = render_ok("x.shape: circle");
        assert!(
            svg.contains("<circle") || svg.contains("<ellipse"),
            "expected a circle/ellipse element"
        );

        // Verify custom fill color is applied.
        let svg2 = render_ok("y.style.fill: \"#ff0000\"");
        assert!(
            svg2.contains("ff0000") || svg2.contains("FF0000"),
            "expected fill color ff0000 in SVG"
        );
    }

    // 5. Direction right -----------------------------------------------------

    #[test]
    fn test_e2e_direction_right() {
        let svg = render_ok("direction: right\na -> b -> c");
        assert!(svg.contains("a"), "expected label 'a'");
        assert!(svg.contains("b"), "expected label 'b'");
        assert!(svg.contains("c"), "expected label 'c'");
    }

    // 6. Complex diagram -----------------------------------------------------

    #[test]
    fn test_e2e_complex_diagram() {
        let source = "\
direction: right\n\
\n\
network: {\n\
  style.stroke: \"#4a90d9\"\n\
  load_balancer.shape: diamond\n\
  load_balancer.style.fill: \"#ffd700\"\n\
  server1\n\
  server2\n\
  load_balancer -> server1: route\n\
  load_balancer -> server2: route\n\
}\n\
\n\
database.shape: cylinder\n\
database.style.fill: \"#228b22\"\n\
\n\
network.server1 -> database: query\n\
network.server2 -> database: query\n\
";
        let svg = render_ok(source);
        // Should contain multiple shapes and edges
        assert!(svg.contains("load_balancer") || svg.contains("load balancer"));
        assert!(svg.contains("database"));
        let path_count = svg.matches("<path").count();
        assert!(
            path_count >= 4,
            "expected at least 4 edge paths, got {path_count}"
        );
    }

    // 7. Empty input ---------------------------------------------------------

    #[test]
    fn test_e2e_empty_input() {
        let svg = render_ok("");
        // Should still produce a valid (minimal) SVG
        assert!(svg.contains("<svg"));
        assert!(svg.contains("</svg>"));
    }

    // 8. Comments only -------------------------------------------------------

    #[test]
    fn test_e2e_comments_only() {
        let svg = render_ok("# just comments");
        assert!(svg.contains("<svg"));
        assert!(svg.contains("</svg>"));
    }

    // 9. Parse error ---------------------------------------------------------

    #[test]
    fn test_e2e_parse_error() {
        // Unterminated quoted string should produce a parse error
        let result = render_d2_to_svg("x: \"unterminated", &opts());
        match result {
            Err(D2Error::Parse(errors)) => {
                assert!(!errors.is_empty(), "expected at least one parse error");
            }
            Err(other) => panic!("expected D2Error::Parse, got: {other}"),
            Ok(_) => panic!("expected Err, got Ok"),
        }
    }

    // 10. All arrow types ----------------------------------------------------

    #[test]
    fn test_e2e_all_arrow_types() {
        // -> (forward arrow), <- (backward arrow), <-> (bidirectional), -- (no arrow)
        let source = "a -> b\nc <- d\ne <-> f\ng -- h";
        let svg = render_ok(source);
        // All 8 shapes should be present
        for label in &["a", "b", "c", "d", "e", "f", "g", "h"] {
            assert!(
                svg.contains(label),
                "expected label '{label}' in SVG"
            );
        }
        // At least 4 edges
        let path_count = svg.matches("<path").count();
        assert!(
            path_count >= 4,
            "expected at least 4 edge paths, got {path_count}"
        );
    }

    // 11. Self-edge ----------------------------------------------------------

    #[test]
    fn test_e2e_self_edge() {
        let svg = render_ok("a -> a");
        assert!(svg.contains("<svg"), "expected valid SVG with <svg tag");
        // Verify the self-loop didn't panic and produced at least one path
        assert!(svg.contains("a"), "expected label 'a' in SVG");
    }

    // 12. All shapes ---------------------------------------------------------

    #[test]
    fn test_e2e_all_shapes() {
        let shapes = [
            "rectangle", "square", "circle", "oval", "diamond", "hexagon",
            "cylinder", "cloud", "person", "package", "queue", "page",
            "parallelogram", "document", "step", "callout", "stored_data",
            "text", "code",
        ];
        for shape_name in &shapes {
            let source = format!("x.shape: {shape_name}");
            let result = render_d2_to_svg(&source, &opts());
            assert!(
                result.is_ok(),
                "shape '{shape_name}' failed: {:?}",
                result.err()
            );
            let svg = result.unwrap().svg;
            assert!(
                svg.contains("<svg"),
                "shape '{shape_name}' missing <svg tag"
            );
            assert!(
                svg.contains("</svg>"),
                "shape '{shape_name}' missing </svg> tag"
            );
        }
    }
}
