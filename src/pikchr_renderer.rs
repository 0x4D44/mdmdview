//! Pikchr diagram renderer.
//!
//! Renders Pikchr markup to SVG using the embedded pikchr C library,
//! then rasterizes SVG to egui textures via usvg/resvg. Heavy rendering
//! work (Pikchr C call + SVG rasterization) runs on a dedicated worker
//! thread; the main thread only handles the cheap GPU texture upload.
//!
//! This module delegates all shared caching, worker thread management, and
//! rasterization infrastructure to `DiagramRendererCore` in `diagram_common`.
//! Only the pikchr-specific source-to-SVG conversion and SVG font injection
//! remain here.
//!
//! This module is conditionally compiled behind `#[cfg(feature = "pikchr")]`.
//! It integrates with `MarkdownRenderer` following the same public API pattern
//! as `MermaidRenderer` (render_block, begin_frame, has_pending, release_gpu_textures).

use crate::diagram_common::{DiagramRendererConfig, DiagramRendererCore};

// ---------------------------------------------------------------------------
// Cache capacity constants
// ---------------------------------------------------------------------------

const PIKCHR_TEXTURE_CACHE_CAPACITY: usize = 64;
const PIKCHR_SVG_CACHE_CAPACITY: usize = 64;
const PIKCHR_ERROR_CACHE_CAPACITY: usize = 32;
/// Rasterization supersample factor. Pikchr SVGs have compact viewBox
/// dimensions (a single box is ~67x35 units), so rasterizing at 1x
/// produces textures with insufficient pixel density for crisp text.
/// 2x gives 4x the pixels, making anti-aliased text and thin lines
/// look sharp -- the standard "retina rendering" approach.
const PIKCHR_SUPERSAMPLE: f32 = 2.0;
/// Bounded channel capacity for job and result channels. With debounce
/// suppressing enqueue during resize, at most one job per diagram is
/// enqueued after the debounce expires. A document with ~6-8 diagrams
/// fits within 8 slots. The bounded channel provides natural backpressure
/// if the worker falls behind.
const CHANNEL_CAPACITY: usize = 8;

// ---------------------------------------------------------------------------
// PikchrRenderer
// ---------------------------------------------------------------------------

/// Pikchr diagram renderer.
///
/// Renders Pikchr markup to SVG using the embedded pikchr C library,
/// then rasterizes SVG to egui textures via usvg/resvg. Heavy rendering
/// runs on a dedicated worker thread; the main thread only uploads the
/// resulting RGBA data to the GPU (~0.1-1ms).
///
/// Wraps a `DiagramRendererCore` that holds all shared infrastructure.
pub(crate) struct PikchrRenderer {
    core: DiagramRendererCore,
}

impl PikchrRenderer {
    /// Create a new PikchrRenderer, loading system fonts once at startup.
    /// Without system fonts, usvg's default empty fontdb silently drops all
    /// text labels in rasterized SVGs. Takes ~50-200ms (one-time cost).
    /// Spawns a dedicated worker thread for background rasterization.
    pub(crate) fn new() -> Self {
        let config = DiagramRendererConfig {
            key_prefix: "pikchr",
            error_label: "Pikchr",
            supersample: PIKCHR_SUPERSAMPLE,
            texture_capacity: PIKCHR_TEXTURE_CACHE_CAPACITY,
            svg_capacity: PIKCHR_SVG_CACHE_CAPACITY,
            error_capacity: PIKCHR_ERROR_CACHE_CAPACITY,
            channel_capacity: CHANNEL_CAPACITY,
            thread_name: "pikchr-raster",
            render_source_to_svg: Self::render_pikchr_to_svg,
            svg_preprocessor: Some(Self::inject_svg_font_family),
        };
        Self {
            core: DiagramRendererCore::new(config),
        }
    }

    // -----------------------------------------------------------------------
    // Public API (thin wrappers delegating to core)
    // -----------------------------------------------------------------------

    /// Called once at the start of each egui frame.
    pub(crate) fn begin_frame(&self, ctx: &egui::Context) {
        self.core.begin_frame(ctx);
    }

    /// Returns true if any rasterization jobs are in-flight on the worker thread.
    pub(crate) fn has_pending(&self) -> bool {
        self.core.has_pending()
    }

    /// Release GPU textures to reduce idle GPU usage.
    pub(crate) fn release_gpu_textures(&self) {
        self.core.release_gpu_textures();
    }

    /// Main entry point for rendering a Pikchr code block.
    pub(crate) fn render_block(
        &self,
        ui: &mut egui::Ui,
        code: &str,
        ui_scale: f32,
        code_font_size: f32,
    ) -> bool {
        self.core.render_block(ui, code, ui_scale, code_font_size)
    }

    // -----------------------------------------------------------------------
    // Pikchr-specific static methods
    // -----------------------------------------------------------------------

    /// Render Pikchr source code to an SVG string.
    ///
    /// Static associated function (no `&self`) -- pure input/output.
    /// Uses the pikchr crate's builder-pattern API for flags.
    fn render_pikchr_to_svg(code: &str, dark_mode: bool) -> Result<String, String> {
        use pikchr::{Pikchr, PikchrFlags};

        let mut flags = PikchrFlags::default();
        flags.generate_plain_errors();
        if dark_mode {
            flags.use_dark_mode();
        }

        Pikchr::render(code, None, flags).map(|svg| svg.to_string())
        // Pikchr::render already returns Err(String); no map_err needed.
    }

    /// Inject a `font-family` declaration into a Pikchr SVG string.
    ///
    /// Pikchr outputs `<text>` elements with no `font-family` attribute,
    /// causing usvg to fall back to a potentially thin default font.
    /// We prepend a `font-family` to the existing `<svg style='...'>`
    /// so all text inherits a known good sans-serif font.
    fn inject_svg_font_family(svg: &str) -> String {
        // Pikchr outputs: <svg ... style='font-size:initial;' ...>
        // Inject font-family before the existing font-size declaration.
        if let Some(pos) = svg.find("style='font-size:") {
            let insert_at = pos + "style='".len();
            let mut result = String::with_capacity(svg.len() + 60);
            result.push_str(&svg[..insert_at]);
            result.push_str("font-family:\"Segoe UI\",\"Helvetica Neue\",Arial,sans-serif;");
            result.push_str(&svg[insert_at..]);
            result
        } else {
            svg.to_string()
        }
    }
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;
    use crate::diagram_common::{
        self, bucket_width, process_job, rasterize_svg, svg_cache_key, DiagramDebounce,
        DiagramRendererConfig, RasterJob, RasterResult,
    };
    use crate::lru_cache::hash_str;
    use std::cell::RefCell;
    use std::sync::Arc;
    use std::time::{Duration, Instant};

    fn test_raw_input(width: f32, height: f32) -> egui::RawInput {
        egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(width, height),
            )),
            ..Default::default()
        }
    }

    fn pikchr_config() -> DiagramRendererConfig {
        DiagramRendererConfig {
            key_prefix: "pikchr",
            error_label: "Pikchr",
            supersample: PIKCHR_SUPERSAMPLE,
            texture_capacity: 4,
            svg_capacity: 4,
            error_capacity: 4,
            channel_capacity: CHANNEL_CAPACITY,
            thread_name: "pikchr-raster-test",
            render_source_to_svg: PikchrRenderer::render_pikchr_to_svg,
            svg_preprocessor: Some(PikchrRenderer::inject_svg_font_family),
        }
    }

    fn test_renderer_with_channels(
        job_tx: Option<diagram_common::JobSender>,
        result_rx: diagram_common::ResultReceiver,
    ) -> PikchrRenderer {
        PikchrRenderer {
            core: DiagramRendererCore::new_with_channels(pikchr_config(), job_tx, result_rx),
        }
    }

    fn insert_test_texture(
        ui: &mut egui::Ui,
        renderer: &PikchrRenderer,
        key: &str,
        display_size: [u32; 2],
    ) {
        let image = egui::ColorImage::new([2, 2], egui::Color32::WHITE);
        let texture =
            ui.ctx()
                .load_texture(key.to_string(), image, egui::TextureOptions::default());
        renderer.core.textures.borrow_mut().insert(
            key.to_string(),
            diagram_common::DiagramTextureEntry {
                texture,
                display_size,
            },
        );
    }

    // -----------------------------------------------------------------------
    // Pikchr source-to-SVG tests (specific to Pikchr, NOT shared)
    // -----------------------------------------------------------------------

    #[test]
    fn test_render_simple_diagram() {
        let svg = PikchrRenderer::render_pikchr_to_svg("box \"Hello\" fit", false);
        assert!(svg.is_ok());
        let svg = svg.unwrap();
        assert!(svg.contains("<svg"));
        assert!(svg.contains("Hello"));
    }

    #[test]
    fn test_render_dark_mode_produces_different_svg() {
        let light = PikchrRenderer::render_pikchr_to_svg("box \"Test\" fit", false).unwrap();
        let dark = PikchrRenderer::render_pikchr_to_svg("box \"Test\" fit", true).unwrap();
        assert_ne!(light, dark);
    }

    #[test]
    fn test_render_invalid_syntax() {
        let result =
            PikchrRenderer::render_pikchr_to_svg("this is not valid pikchr syntax }{}{", false);
        assert!(result.is_err());
    }

    #[test]
    fn test_render_empty_input() {
        let result = PikchrRenderer::render_pikchr_to_svg("", false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_texture_key_includes_all_components() {
        let key = format!("pikchr:{:016x}:w{}:s{}:dm{}", 12345u64, 640, 100, 0);
        assert!(key.contains("pikchr:"));
        assert!(key.contains(":w640:"));
        assert!(key.contains(":s100:"));
        assert!(key.contains(":dm0"));
    }

    #[test]
    fn test_svg_contains_dimensions() {
        let svg = PikchrRenderer::render_pikchr_to_svg("box \"Test\" fit", false).unwrap();
        assert!(svg.contains("width") || svg.contains("viewBox"));
    }

    #[test]
    fn test_complex_diagram() {
        let code = r#"
            arrow right 200% "Markdown" "Source"
            box rad 10px "Pikchr" "Renderer" fit
            arrow right 200% "SVG" "Output"
            box rad 10px "Rasterizer" fit
        "#;
        let svg = PikchrRenderer::render_pikchr_to_svg(code, false);
        assert!(svg.is_ok());
    }

    #[test]
    fn test_renderer_new_creates_empty_caches() {
        let renderer = PikchrRenderer::new();
        assert!(!renderer.has_pending());
    }

    // -----------------------------------------------------------------------
    // Sample diagram verification tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_sample_simple_flow() {
        let code = "arrow right 200% \"Request\" above\nbox \"Server\" fit rad 10px\narrow right 200% \"Response\" above";
        let result = PikchrRenderer::render_pikchr_to_svg(code, false);
        assert!(
            result.is_ok(),
            "Simple flow sample failed: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_sample_architecture_diagram() {
        let code = "box \"Browser\" fit rad 10px\narrow right 250% \"HTTP\" above\nbox \"Web Server\" fit rad 10px\narrow right 250% \"SQL\" above\ncylinder \"Database\" fit";
        let result = PikchrRenderer::render_pikchr_to_svg(code, false);
        assert!(
            result.is_ok(),
            "Architecture sample failed: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_sample_decision_flow() {
        let code = "box \"Start\" fit rad 50%\narrow from last box.s down\ndiamond \"OK?\" fit\narrow from last diamond.e right 200% \"Yes\" above\nbox \"Done\" fit rad 50%\narrow from last diamond.s down \"No\" ljust\nbox \"Retry\" fit rad 10px\narrow from last box.w left 0.5in then up until even with 1st box then right to 1st box.w";
        let result = PikchrRenderer::render_pikchr_to_svg(code, false);
        assert!(
            result.is_ok(),
            "Decision flow sample failed: {:?}",
            result.err()
        );
    }

    // -----------------------------------------------------------------------
    // Shape type SVG output validation
    // -----------------------------------------------------------------------

    #[test]
    fn test_box_produces_rect_element() {
        let svg = PikchrRenderer::render_pikchr_to_svg("box \"Label\" fit", false).unwrap();
        assert!(
            svg.contains("<polygon") || svg.contains("<path") || svg.contains("<rect"),
            "box should produce a rect-like SVG element, got:\n{}",
            svg
        );
        assert!(svg.contains("Label"), "box label missing from SVG");
    }

    #[test]
    fn test_circle_produces_circle_element() {
        let svg = PikchrRenderer::render_pikchr_to_svg("circle \"C\" fit", false).unwrap();
        assert!(
            svg.contains("<circle"),
            "circle should produce <circle> element, got:\n{}",
            svg
        );
        assert!(svg.contains(">C<"), "circle label 'C' missing from SVG");
    }

    #[test]
    fn test_ellipse_produces_ellipse_element() {
        let svg = PikchrRenderer::render_pikchr_to_svg("ellipse \"E\" fit", false).unwrap();
        assert!(
            svg.contains("<ellipse") || svg.contains("<path") || svg.contains("<circle"),
            "ellipse should produce an ellipse-like SVG element, got:\n{}",
            svg
        );
        assert!(svg.contains(">E<"), "ellipse label 'E' missing from SVG");
    }

    #[test]
    fn test_cylinder_produces_svg() {
        let svg = PikchrRenderer::render_pikchr_to_svg("cylinder \"DB\" fit", false).unwrap();
        assert!(svg.contains("<svg"), "cylinder should produce valid SVG");
        assert!(svg.contains("DB"), "cylinder label 'DB' missing from SVG");
    }

    #[test]
    fn test_diamond_produces_svg() {
        let svg = PikchrRenderer::render_pikchr_to_svg("diamond \"?\" fit", false).unwrap();
        assert!(svg.contains("<svg"), "diamond should produce valid SVG");
        assert!(
            svg.contains("<polygon") || svg.contains("<path"),
            "diamond should produce polygon or path element"
        );
        assert!(svg.contains("?"), "diamond label '?' missing from SVG");
    }

    #[test]
    fn test_arrow_produces_polygon() {
        let svg = PikchrRenderer::render_pikchr_to_svg("arrow right", false).unwrap();
        assert!(
            svg.contains("<polygon"),
            "arrow should produce polygon (arrowhead), got:\n{}",
            svg
        );
    }

    #[test]
    fn test_line_produces_line_element() {
        let svg = PikchrRenderer::render_pikchr_to_svg("line right", false).unwrap();
        assert!(
            svg.contains("<line") || svg.contains("<path"),
            "line should produce <line> or <path>, got:\n{}",
            svg
        );
    }

    #[test]
    fn test_dot_produces_svg() {
        let svg = PikchrRenderer::render_pikchr_to_svg("dot", false).unwrap();
        assert!(svg.contains("<svg"), "dot should produce valid SVG");
        assert!(
            svg.contains("<circle"),
            "dot should produce a circle element"
        );
    }

    #[test]
    fn test_spline_produces_path() {
        let svg = PikchrRenderer::render_pikchr_to_svg("spline right then down", false).unwrap();
        assert!(
            svg.contains("<path"),
            "spline should produce <path> element, got:\n{}",
            svg
        );
    }

    #[test]
    fn test_file_shape_produces_svg() {
        let svg = PikchrRenderer::render_pikchr_to_svg("file \"f.txt\" fit", false).unwrap();
        assert!(svg.contains("<svg"), "file shape should produce valid SVG");
        assert!(svg.contains("f.txt"), "file label missing from SVG");
    }

    // -----------------------------------------------------------------------
    // SVG structure and dimension validation
    // -----------------------------------------------------------------------

    #[test]
    fn test_svg_has_proper_structure() {
        let svg = PikchrRenderer::render_pikchr_to_svg("box \"X\" fit", false).unwrap();
        assert!(svg.contains("<svg"), "SVG must have opening <svg tag");
        assert!(svg.contains("</svg>"), "SVG must have closing </svg> tag");
        assert!(
            svg.contains("viewBox") || (svg.contains("width") && svg.contains("height")),
            "SVG must have viewBox or width/height attributes"
        );
    }

    #[test]
    fn test_svg_dimensions_are_reasonable() {
        let svg = PikchrRenderer::render_pikchr_to_svg("box \"Test\" fit", false).unwrap();
        let (w, h) =
            extract_viewbox_size(&svg).expect("SVG should have a parseable viewBox attribute");
        assert!(w > 0.0, "viewBox width must be > 0, got {}", w);
        assert!(h > 0.0, "viewBox height must be > 0, got {}", h);
        assert!(w < 10000.0, "viewBox width must be < 10000, got {}", w);
        assert!(h < 10000.0, "viewBox height must be < 10000, got {}", h);
    }

    #[test]
    fn test_different_diagrams_produce_different_dimensions() {
        let small = PikchrRenderer::render_pikchr_to_svg("box", false).unwrap();
        let large = PikchrRenderer::render_pikchr_to_svg(
            "box; arrow right; box; arrow right; box; arrow right; box; arrow right; box",
            false,
        )
        .unwrap();
        let (w_small, _) = extract_viewbox_size(&small).expect("small SVG should have viewBox");
        let (w_large, _) = extract_viewbox_size(&large).expect("large SVG should have viewBox");
        assert!(
            w_large > w_small,
            "Chain of boxes should be wider than single box: {} vs {}",
            w_large,
            w_small
        );
    }

    /// Helper: extract width and height from an SVG `viewBox="x y w h"` attribute.
    fn extract_viewbox_size(svg: &str) -> Option<(f64, f64)> {
        let marker = "viewBox=\"";
        let start = svg.find(marker)? + marker.len();
        let rest = &svg[start..];
        let end = rest.find('"')?;
        let parts: Vec<f64> = rest[..end]
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();
        if parts.len() >= 4 {
            Some((parts[2], parts[3]))
        } else {
            None
        }
    }

    // -----------------------------------------------------------------------
    // Label and text content tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_labels_preserved_in_svg() {
        let code = r#"box "Alpha" fit; arrow right; box "Beta" fit; arrow right; box "Gamma" fit"#;
        let svg = PikchrRenderer::render_pikchr_to_svg(code, false).unwrap();
        assert!(svg.contains("Alpha"), "Label 'Alpha' missing from SVG");
        assert!(svg.contains("Beta"), "Label 'Beta' missing from SVG");
        assert!(svg.contains("Gamma"), "Label 'Gamma' missing from SVG");
    }

    #[test]
    fn test_unicode_labels() {
        let svg = PikchrRenderer::render_pikchr_to_svg("box \"日本語\" fit", false).unwrap();
        assert!(
            svg.contains("日本語"),
            "Unicode label '日本語' missing from SVG"
        );
    }

    #[test]
    fn test_special_characters_in_labels() {
        let svg = PikchrRenderer::render_pikchr_to_svg("box \"x < y\" fit", false).unwrap();
        assert!(
            svg.contains("&lt;"),
            "SVG should contain XML-escaped '<':\n{}",
            svg
        );

        let svg2 = PikchrRenderer::render_pikchr_to_svg("box \"A & B\" fit", false).unwrap();
        assert!(
            svg2.contains("&amp;"),
            "SVG should contain XML-escaped '&':\n{}",
            svg2
        );
    }

    // -----------------------------------------------------------------------
    // Error handling and content
    // -----------------------------------------------------------------------

    #[test]
    fn test_error_message_has_content() {
        let err = PikchrRenderer::render_pikchr_to_svg("this is not valid pikchr }{}{", false)
            .unwrap_err();
        assert!(
            !err.is_empty(),
            "Error message should not be empty for invalid syntax"
        );
        assert!(
            err.len() > 5,
            "Error message should be descriptive, got: '{}'",
            err
        );
    }

    #[test]
    fn test_error_cache_theme_independent() {
        let invalid = "totally broken pikchr syntax }{}{";
        let err_light = PikchrRenderer::render_pikchr_to_svg(invalid, false).unwrap_err();
        let err_dark = PikchrRenderer::render_pikchr_to_svg(invalid, true).unwrap_err();
        assert_eq!(
            err_light, err_dark,
            "Same syntax error should produce same message regardless of theme"
        );
    }

    #[test]
    fn test_multiple_errors_are_distinct() {
        let err1 =
            PikchrRenderer::render_pikchr_to_svg("invalid syntax aaa }{}{", false).unwrap_err();
        let err2 =
            PikchrRenderer::render_pikchr_to_svg("different invalid bbb }{}{", false).unwrap_err();
        assert!(!err1.is_empty(), "Error 1 should not be empty");
        assert!(!err2.is_empty(), "Error 2 should not be empty");
    }

    // -----------------------------------------------------------------------
    // Dark mode output inspection
    // -----------------------------------------------------------------------

    #[test]
    fn test_dark_mode_inverts_stroke_colors() {
        let light = PikchrRenderer::render_pikchr_to_svg("box \"X\" fit", false).unwrap();
        let dark = PikchrRenderer::render_pikchr_to_svg("box \"X\" fit", true).unwrap();

        let light_has_black =
            light.contains("black") || light.contains("rgb(0,0,0)") || light.contains("#000");
        let dark_has_white =
            dark.contains("white") || dark.contains("rgb(255,255,255)") || dark.contains("#fff");

        assert!(
            light_has_black,
            "Light mode SVG should reference dark colors:\n{}",
            light
        );
        assert!(
            dark_has_white,
            "Dark mode SVG should reference light colors:\n{}",
            dark
        );
    }

    #[test]
    fn test_dark_mode_preserves_labels() {
        let label = "DarkTest";
        let code = format!("box \"{}\" fit", label);
        let light = PikchrRenderer::render_pikchr_to_svg(&code, false).unwrap();
        let dark = PikchrRenderer::render_pikchr_to_svg(&code, true).unwrap();
        assert!(
            light.contains(label),
            "Light mode should contain label '{}'",
            label
        );
        assert!(
            dark.contains(label),
            "Dark mode should contain label '{}'",
            label
        );
    }

    // -----------------------------------------------------------------------
    // Stress and edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn test_large_diagram_renders() {
        let mut code = String::from("box \"B0\" fit\n");
        for i in 1..=50 {
            code.push_str(&format!("arrow right\nbox \"B{}\" fit\n", i));
        }
        let result = PikchrRenderer::render_pikchr_to_svg(&code, false);
        assert!(
            result.is_ok(),
            "Large diagram (50+ objects) should render: {:?}",
            result.err()
        );
        let svg = result.unwrap();
        assert!(svg.contains("B0"), "First box label missing");
        assert!(svg.contains("B50"), "Last box label missing");
    }

    #[test]
    fn test_whitespace_only_input() {
        let result = PikchrRenderer::render_pikchr_to_svg("   \n  \n  ", false);
        assert!(
            result.is_ok(),
            "Whitespace-only input should not error: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_comments_in_pikchr() {
        let code = "# This is a comment\nbox \"Visible\" fit\n# Another comment";
        let result = PikchrRenderer::render_pikchr_to_svg(code, false);
        assert!(
            result.is_ok(),
            "Comments should not cause errors: {:?}",
            result.err()
        );
        let svg = result.unwrap();
        assert!(
            svg.contains("Visible"),
            "Label should appear despite comments"
        );
        assert!(
            !svg.contains("This is a comment"),
            "Comment text should not leak into SVG output"
        );
    }

    // -----------------------------------------------------------------------
    // Output snapshot / characterization test
    // -----------------------------------------------------------------------

    #[test]
    fn test_svg_output_snapshot() {
        let code = r#"arrow right 200% "Input" above
box "Process" fit rad 10px
arrow right 200% "Output" above"#;
        let svg = PikchrRenderer::render_pikchr_to_svg(code, false).unwrap();

        assert!(svg.starts_with("<svg"), "SVG must start with <svg tag");
        assert!(
            svg.trim_end().ends_with("</svg>"),
            "SVG must end with </svg>"
        );
        assert!(svg.contains("viewBox"), "SVG should have viewBox attribute");
        assert!(
            svg.contains("<polygon"),
            "Arrows should produce <polygon> arrowheads"
        );
        assert!(
            svg.contains("<text"),
            "Labels should produce <text> elements"
        );
        assert!(svg.contains("Input"), "Label 'Input' must be in SVG");
        assert!(svg.contains("Process"), "Label 'Process' must be in SVG");
        assert!(svg.contains("Output"), "Label 'Output' must be in SVG");
        assert!(
            svg.contains("<line") || svg.contains("<path"),
            "Connectors should produce <line> or <path> elements"
        );
    }

    // -----------------------------------------------------------------------
    // Font injection and supersample tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_inject_svg_font_family_adds_declaration() {
        let svg = "<svg style='font-size:initial;' viewBox='0 0 100 50'></svg>";
        let result = PikchrRenderer::inject_svg_font_family(svg);
        assert!(
            result.contains("font-family:"),
            "Should inject font-family declaration"
        );
        assert!(
            result.contains("sans-serif"),
            "Should include sans-serif fallback"
        );
        assert!(
            result.contains("font-size:initial"),
            "Original style should be preserved"
        );
        assert!(
            result.contains("viewBox"),
            "Other attributes should be preserved"
        );
    }

    #[test]
    fn test_inject_svg_font_family_passthrough_without_style() {
        let svg = "<svg viewBox='0 0 100 50'><text>Hello</text></svg>";
        let result = PikchrRenderer::inject_svg_font_family(svg);
        assert_eq!(
            result, svg,
            "SVG without matching style should be unchanged"
        );
    }

    #[test]
    fn test_supersample_constant_is_reasonable() {
        const { assert!(PIKCHR_SUPERSAMPLE >= 1.0, "Supersample must be >= 1.0") };
        const {
            assert!(
                PIKCHR_SUPERSAMPLE <= 4.0,
                "Supersample > 4.0 would waste memory"
            )
        };
    }

    // -----------------------------------------------------------------------
    // Async worker and channel tests (using pikchr-specific render function)
    // -----------------------------------------------------------------------

    /// Helper: create a fontdb with system fonts loaded.
    fn make_fontdb() -> Arc<usvg::fontdb::Database> {
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        Arc::new(db)
    }

    #[test]
    fn test_render_block_basic_and_pending_placeholder() {
        let renderer = PikchrRenderer::new();
        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let code = "box \"Hello\" fit";
        let mut rendered_first = false;
        let mut rendered_second = false;

        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered_first = renderer.render_block(ui, code, 1.0, 14.0);
                rendered_second = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered_first);
        assert!(rendered_second);
        assert!(renderer.core.pending.borrow().contains(&hash_str(code)));
    }

    #[test]
    fn test_render_block_reports_cached_error() {
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        renderer
            .core
            .errors
            .borrow_mut()
            .insert(code_hash, "boom".to_string());

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert!(!renderer.has_pending());
    }

    #[test]
    fn test_render_block_enqueues_cached_svg() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        renderer.core.svg_cache.borrow_mut().insert(
            svg_cache_key(code_hash, false),
            "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_string(),
        );
        renderer.core.svg_cache.borrow_mut().insert(
            svg_cache_key(code_hash, true),
            "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_string(),
        );

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        let request = job_rx
            .recv_timeout(Duration::from_secs(1))
            .expect("request");
        assert!(request.svg.is_some());
        assert!(request.code.is_none());
        assert!(renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_render_block_queue_full_keeps_rendering() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx.clone()), result_rx);
        job_tx
            .send(RasterJob {
                code_hash: 1,
                texture_key: "pikchr:full:w224:s100:dm0".to_string(),
                code: Some("box".to_string()),
                svg: None,
                dark_mode: false,
                width_bucket: 224,
                scale_bucket: 100,
            })
            .expect("fill queue");

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, "box", 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert!(!renderer.has_pending());
        drop(job_rx);
    }

    #[test]
    fn test_render_block_disconnected_queue_sets_error() {
        let (job_tx, job_rx) = crossbeam_channel::bounded::<RasterJob>(1);
        drop(job_rx);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert!(renderer.core.errors.borrow_mut().get(&code_hash).is_some());
    }

    #[test]
    fn test_begin_frame_processes_result_and_uses_cached_texture() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(2);
        let (job_tx, _job_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        renderer.core.pending.borrow_mut().insert(code_hash);
        renderer.core.pending.borrow_mut().insert(999);
        let result_tx = RefCell::new(Some(result_tx));
        let texture_key = RefCell::new(String::new());

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                let width_bucket = bucket_width(ui.available_width().max(1.0));
                let key = format!(
                    "pikchr:{:016x}:w{}:s{}:dm{}",
                    code_hash,
                    width_bucket,
                    100,
                    ui.visuals().dark_mode as u8
                );
                *texture_key.borrow_mut() = key.clone();
                renderer
                    .core
                    .wanted_bucket
                    .borrow_mut()
                    .insert(code_hash, width_bucket);
                if let Some(tx) = result_tx.borrow_mut().take() {
                    tx.send(RasterResult {
                        code_hash,
                        texture_key: key,
                        dark_mode: ui.visuals().dark_mode,
                        svg: Some(
                            "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1 1\"><rect width=\"1\" height=\"1\" fill=\"red\"/></svg>"
                                .to_string(),
                        ),
                        rgba: Some(vec![255, 0, 0, 255]),
                        raster_size: Some([1, 1]),
                        display_size: Some([64, 32]),
                        error: None,
                    })
                    .expect("send result");
                }
                renderer.begin_frame(ctx);
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        let texture_key = texture_key.into_inner();
        assert!(renderer
            .core
            .textures
            .borrow_mut()
            .get(&texture_key)
            .is_some());
        assert!(renderer
            .core
            .svg_cache
            .borrow_mut()
            .get(&svg_cache_key(code_hash, ctx.style().visuals.dark_mode))
            .is_some());
        assert_eq!(
            renderer.core.latest_texture_key.borrow().get(&code_hash),
            Some(&texture_key)
        );
        assert!(!renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_result_missing_raster_fields_skips_texture_upload() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code_hash = hash_str("box \"Hello\" fit");
        renderer.core.pending.borrow_mut().insert(code_hash);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "pikchr:missing-fields:w224:s100:dm0".to_string(),
                dark_mode: false,
                svg: Some(
                    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1 1\"></svg>"
                        .to_string(),
                ),
                rgba: None,
                raster_size: None,
                display_size: None,
                error: None,
            })
            .expect("send result");

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |_ui| {
                renderer.begin_frame(ctx);
            });
        });

        assert_eq!(renderer.core.textures.borrow().len(), 0);
        assert!(!renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_result_missing_raster_size_skips_texture_upload() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code_hash = hash_str("box \"Tall\" fit");
        renderer.core.pending.borrow_mut().insert(code_hash);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "pikchr:partial-fields:w224:s100:dm0".to_string(),
                dark_mode: false,
                svg: Some(
                    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1 1\"></svg>"
                        .to_string(),
                ),
                rgba: Some(vec![255, 0, 0, 255]),
                raster_size: None,
                display_size: Some([64, 32]),
                error: None,
            })
            .expect("send result");

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |_ui| {
                renderer.begin_frame(ctx);
            });
        });

        assert_eq!(renderer.core.textures.borrow().len(), 0);
        assert!(!renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_keeps_polling_when_pending_remains() {
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        renderer.core.pending.borrow_mut().insert(999);

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |_ui| {
                renderer.begin_frame(ctx);
            });
        });

        assert!(renderer.core.pending.borrow().contains(&999));
        drop(result_tx);
    }

    #[test]
    fn test_begin_frame_caches_error_results() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code_hash = 7;
        renderer.core.pending.borrow_mut().insert(code_hash);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "pikchr:error:w224:s100:dm0".to_string(),
                dark_mode: false,
                svg: None,
                rgba: None,
                raster_size: None,
                display_size: None,
                error: Some("boom".to_string()),
            })
            .expect("send error");

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |_ui| {
                renderer.begin_frame(ctx);
            });
        });

        assert!(renderer.core.errors.borrow_mut().get(&code_hash).is_some());
        assert!(!renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_discards_stale_result() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code_hash = 11;
        renderer.core.pending.borrow_mut().insert(code_hash);
        renderer
            .core
            .wanted_bucket
            .borrow_mut()
            .insert(code_hash, 128);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "pikchr:stale:w224:s100:dm0".to_string(),
                dark_mode: false,
                svg: Some(
                    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1 1\"><rect width=\"1\" height=\"1\" fill=\"red\"/></svg>"
                        .to_string(),
                ),
                rgba: Some(vec![255, 0, 0, 255]),
                raster_size: Some([1, 1]),
                display_size: Some([64, 32]),
                error: None,
            })
            .expect("send result");

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |_ui| {
                renderer.begin_frame(ctx);
            });
        });

        assert_eq!(renderer.core.textures.borrow().len(), 0);
        assert!(renderer
            .core
            .svg_cache
            .borrow_mut()
            .get(&svg_cache_key(code_hash, false))
            .is_some());
        assert!(!renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_processes_result_with_unparseable_texture_key() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code_hash = 19;
        renderer.core.pending.borrow_mut().insert(code_hash);
        renderer
            .core
            .wanted_bucket
            .borrow_mut()
            .insert(code_hash, 128);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "pikchr:not-a-valid-width-key".to_string(),
                dark_mode: false,
                svg: Some(
                    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1 1\"><rect width=\"1\" height=\"1\" fill=\"red\"/></svg>"
                        .to_string(),
                ),
                rgba: Some(vec![255, 0, 0, 255]),
                raster_size: Some([1, 1]),
                display_size: Some([64, 32]),
                error: None,
            })
            .expect("send result");

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |_ui| {
                renderer.begin_frame(ctx);
            });
        });

        assert!(renderer
            .core
            .textures
            .borrow_mut()
            .get(&"pikchr:not-a-valid-width-key".to_string())
            .is_some());
        assert!(renderer.core.debounce.borrow().get(&code_hash).is_none());
        assert!(!renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_disconnected_clears_pending() {
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(1);
        drop(result_tx);
        let renderer = test_renderer_with_channels(None, result_rx);
        renderer.core.pending.borrow_mut().insert(1);

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |_ui| {
                renderer.begin_frame(ctx);
            });
        });

        assert!(renderer.core.pending.borrow().is_empty());
    }

    #[test]
    fn test_render_block_debounce_without_stale_texture_enqueues_job() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let code = "box \"A\"\narrow right 50\nbox \"B\"";
        let code_hash = hash_str(code);
        renderer.core.debounce.borrow_mut().insert(
            code_hash,
            DiagramDebounce {
                last_rasterized_bucket: 224,
                last_seen_bucket: 224,
                bucket_changed_at: Some(Instant::now()),
            },
        );

        let ctx = egui::Context::default();
        let input = test_raw_input(96.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(renderer.core.pending.borrow().contains(&code_hash));
        assert_eq!(
            renderer
                .core
                .debounce
                .borrow()
                .get(&code_hash)
                .map(|d| d.last_seen_bucket),
            Some(bucket_width(96.0))
        );
    }

    #[test]
    fn test_render_block_uses_stale_texture_during_debounce() {
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        let stale_key = format!("pikchr:{:016x}:w224:s100:dm0", code_hash);
        renderer
            .core
            .latest_texture_key
            .borrow_mut()
            .insert(code_hash, stale_key.clone());
        renderer.core.debounce.borrow_mut().insert(
            code_hash,
            DiagramDebounce {
                last_rasterized_bucket: 224,
                last_seen_bucket: 224,
                bucket_changed_at: Some(Instant::now()),
            },
        );

        let ctx = egui::Context::default();
        let input = test_raw_input(96.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                insert_test_texture(ui, &renderer, &stale_key, [320, 160]);
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert!(!renderer.has_pending());
        assert_eq!(
            renderer
                .core
                .debounce
                .borrow()
                .get(&code_hash)
                .map(|d| d.last_seen_bucket),
            Some(bucket_width(96.0))
        );
    }

    #[test]
    fn test_render_block_missing_stale_texture_during_debounce_enqueues_job() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        let stale_key = format!("pikchr:{:016x}:w224:s100:dm0", code_hash);
        renderer
            .core
            .latest_texture_key
            .borrow_mut()
            .insert(code_hash, stale_key);
        renderer.core.debounce.borrow_mut().insert(
            code_hash,
            DiagramDebounce {
                last_rasterized_bucket: 224,
                last_seen_bucket: 224,
                bucket_changed_at: Some(Instant::now()),
            },
        );

        let ctx = egui::Context::default();
        let input = test_raw_input(96.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_render_block_same_bucket_missing_texture_enqueues_job() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        let width_bucket = bucket_width(96.0);
        renderer.core.debounce.borrow_mut().insert(
            code_hash,
            DiagramDebounce {
                last_rasterized_bucket: width_bucket,
                last_seen_bucket: width_bucket,
                bucket_changed_at: None,
            },
        );

        let ctx = egui::Context::default();
        let input = test_raw_input(96.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_render_block_missing_timer_same_seen_bucket_enqueues_job() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        let width_bucket = bucket_width(96.0);
        renderer.core.debounce.borrow_mut().insert(
            code_hash,
            DiagramDebounce {
                last_rasterized_bucket: 224,
                last_seen_bucket: width_bucket,
                bucket_changed_at: None,
            },
        );

        let ctx = egui::Context::default();
        let input = test_raw_input(96.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_render_block_expired_debounce_enqueues_job() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        let width_bucket = bucket_width(96.0);
        renderer.core.debounce.borrow_mut().insert(
            code_hash,
            DiagramDebounce {
                last_rasterized_bucket: 224,
                last_seen_bucket: width_bucket,
                bucket_changed_at: Some(Instant::now() - Duration::from_millis(150 + 5)),
            },
        );

        let ctx = egui::Context::default();
        let input = test_raw_input(96.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(renderer.core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_show_stale_or_placeholder_ignores_missing_stale_texture() {
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code_hash = hash_str("missing-stale");
        renderer
            .core
            .latest_texture_key
            .borrow_mut()
            .insert(code_hash, "pikchr:missing:w224:s100:dm0".to_string());

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer
                    .core
                    .show_stale_or_placeholder(ui, code_hash, 240.0);
            });
        });

        assert!(rendered);
        assert_eq!(renderer.core.textures.borrow().len(), 0);
    }

    #[test]
    fn test_show_stale_or_placeholder_renders_stale_texture() {
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code_hash = hash_str("present-stale");
        let stale_key = "pikchr:present:w224:s100:dm0".to_string();
        renderer
            .core
            .latest_texture_key
            .borrow_mut()
            .insert(code_hash, stale_key.clone());

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                insert_test_texture(ui, &renderer, &stale_key, [320, 160]);
                rendered = renderer
                    .core
                    .show_stale_or_placeholder(ui, code_hash, 240.0);
            });
        });

        assert!(rendered);
        assert_eq!(renderer.core.textures.borrow().len(), 1);
    }

    #[test]
    fn test_rasterize_svg_uses_base_scale_when_width_bucket_is_zero_and_clamps() {
        let fontdb = make_fontdb();
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 5000 5000\"><rect width=\"5000\" height=\"5000\" fill=\"red\"/></svg>";

        let output = rasterize_svg(
            &fontdb,
            svg,
            0,
            100,
            PIKCHR_SUPERSAMPLE,
            Some(PikchrRenderer::inject_svg_font_family),
        )
        .expect("rasterize");

        assert_eq!(output.display_size, [5000, 5000]);
        assert!(output.raster_size[0] <= 4096);
        assert!(output.raster_size[1] <= 4096);
    }

    #[test]
    fn test_rasterize_svg_clamps_very_large_raster_dimensions() {
        let fontdb = make_fontdb();
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 10000 8000\"><rect width=\"10000\" height=\"8000\" fill=\"red\"/></svg>";

        let output = rasterize_svg(
            &fontdb,
            svg,
            0,
            100,
            PIKCHR_SUPERSAMPLE,
            Some(PikchrRenderer::inject_svg_font_family),
        )
        .expect("rasterize");

        assert_eq!(output.display_size, [10000, 8000]);
        assert_eq!(output.raster_size[0], 4096);
        assert!(output.raster_size[1] <= 4096);
    }

    #[test]
    fn test_rasterize_svg_clamps_very_tall_raster_dimensions() {
        let fontdb = make_fontdb();
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 5000\"><rect width=\"100\" height=\"5000\" fill=\"red\"/></svg>";

        let output = rasterize_svg(
            &fontdb,
            svg,
            0,
            100,
            PIKCHR_SUPERSAMPLE,
            Some(PikchrRenderer::inject_svg_font_family),
        )
        .expect("rasterize");

        assert_eq!(output.display_size, [100, 5000]);
        assert!(output.raster_size[0] < 4096);
        assert_eq!(output.raster_size[1], 4096);
    }

    #[test]
    fn test_rasterize_svg_invalid_svg_returns_error() {
        let fontdb = make_fontdb();
        let err = match rasterize_svg(
            &fontdb,
            "<svg",
            224,
            100,
            PIKCHR_SUPERSAMPLE,
            Some(PikchrRenderer::inject_svg_font_family),
        ) {
            Ok(_) => panic!("expected parse error"),
            Err(err) => err,
        };
        assert!(err.contains("SVG parse error"));
    }

    #[test]
    fn test_process_job_returns_raster_error_for_invalid_cached_svg() {
        let fontdb = make_fontdb();
        let code_hash = hash_str("cached-error");
        let result = process_job(
            &fontdb,
            RasterJob {
                code_hash,
                texture_key: format!("pikchr:{:016x}:w224:s100:dm0", code_hash),
                code: None,
                svg: Some("<svg".to_string()),
                dark_mode: false,
                width_bucket: 224,
                scale_bucket: 100,
            },
            PikchrRenderer::render_pikchr_to_svg,
            PIKCHR_SUPERSAMPLE,
            Some(PikchrRenderer::inject_svg_font_family),
        );

        assert!(result.error.is_some());
        assert!(result.svg.is_some());
        assert!(result.rgba.is_none());
    }

    #[test]
    fn test_worker_processes_job() {
        let fontdb = make_fontdb();
        let (job_tx, job_rx) = crossbeam_channel::bounded::<RasterJob>(4);
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(4);

        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        let texture_key = format!("pikchr:{:016x}:w640:s100:dm0", code_hash);
        let job = RasterJob {
            code_hash,
            texture_key: texture_key.clone(),
            code: Some(code.to_string()),
            svg: None,
            dark_mode: false,
            width_bucket: 640,
            scale_bucket: 100,
        };

        job_tx.send(job).unwrap();
        drop(job_tx);

        DiagramRendererCore::worker_loop(
            fontdb,
            job_rx,
            result_tx,
            PikchrRenderer::render_pikchr_to_svg,
            PIKCHR_SUPERSAMPLE,
            Some(PikchrRenderer::inject_svg_font_family),
        );

        let result = result_rx.recv_timeout(Duration::from_secs(5)).unwrap();
        assert!(result.rgba.is_some(), "Worker should produce RGBA data");
        assert!(
            result.raster_size.is_some(),
            "Worker should produce raster_size"
        );
        assert!(
            result.display_size.is_some(),
            "Worker should produce display_size"
        );
        assert!(result.error.is_none(), "Worker should not produce an error");
        assert!(result.svg.is_some(), "Worker should produce SVG on success");
    }

    #[test]
    fn test_worker_caches_svg_on_success() {
        let fontdb = make_fontdb();
        let (job_tx, job_rx) = crossbeam_channel::bounded::<RasterJob>(4);
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(4);

        let code = "box \"Hello\" fit";
        let code_hash = hash_str(code);
        let texture_key = format!("pikchr:{:016x}:w640:s100:dm0", code_hash);
        let job = RasterJob {
            code_hash,
            texture_key,
            code: Some(code.to_string()),
            svg: None,
            dark_mode: false,
            width_bucket: 640,
            scale_bucket: 100,
        };

        job_tx.send(job).unwrap();
        drop(job_tx);

        DiagramRendererCore::worker_loop(
            fontdb,
            job_rx,
            result_tx,
            PikchrRenderer::render_pikchr_to_svg,
            PIKCHR_SUPERSAMPLE,
            Some(PikchrRenderer::inject_svg_font_family),
        );

        let result = result_rx.recv_timeout(Duration::from_secs(5)).unwrap();
        assert!(result.svg.is_some(), "SVG should be cached on success");
        let svg = result.svg.unwrap();
        assert!(
            svg.contains("<svg"),
            "Cached SVG should contain <svg tag, got: {}",
            &svg[..svg.len().min(200)]
        );
    }

    #[test]
    fn test_worker_caches_error() {
        let fontdb = make_fontdb();
        let (job_tx, job_rx) = crossbeam_channel::bounded::<RasterJob>(4);
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(4);

        let code = "invalid syntax }{}{";
        let code_hash = hash_str(code);
        let texture_key = format!("pikchr:{:016x}:w640:s100:dm0", code_hash);
        let job = RasterJob {
            code_hash,
            texture_key,
            code: Some(code.to_string()),
            svg: None,
            dark_mode: false,
            width_bucket: 640,
            scale_bucket: 100,
        };

        job_tx.send(job).unwrap();
        drop(job_tx);

        DiagramRendererCore::worker_loop(
            fontdb,
            job_rx,
            result_tx,
            PikchrRenderer::render_pikchr_to_svg,
            PIKCHR_SUPERSAMPLE,
            Some(PikchrRenderer::inject_svg_font_family),
        );

        let result = result_rx.recv_timeout(Duration::from_secs(5)).unwrap();
        assert!(
            result.error.is_some(),
            "Worker should produce an error for invalid code"
        );
        assert!(
            result.rgba.is_none(),
            "Worker should not produce RGBA for invalid code"
        );
        assert!(
            result.svg.is_none(),
            "Worker should not produce SVG for invalid code"
        );
    }

    #[test]
    fn test_is_stale_result_width_mismatch() {
        let renderer = PikchrRenderer::new();
        let code_hash = 12345u64;

        renderer
            .core
            .wanted_bucket
            .borrow_mut()
            .insert(code_hash, 640);

        let stale_key = format!("pikchr:{:016x}:w1024:s100:dm0", code_hash);
        assert!(
            renderer.core.is_stale_result(&stale_key, code_hash),
            "Width 1024 vs wanted 640 should be stale"
        );

        let fresh_key = format!("pikchr:{:016x}:w640:s100:dm0", code_hash);
        assert!(
            !renderer.core.is_stale_result(&fresh_key, code_hash),
            "Width 640 vs wanted 640 should be fresh"
        );
    }

    #[test]
    fn test_is_stale_result_when_wanted_is_none() {
        let renderer = PikchrRenderer::new();
        let texture_key = format!("pikchr:{:016x}:w640:s100:dm0", 99999u64);
        assert!(
            renderer.core.is_stale_result(&texture_key, 99999),
            "No wanted_bucket entry should be treated as stale"
        );
    }

    #[test]
    fn test_is_stale_result_invalid_texture_key_is_assumed_fresh() {
        let renderer = PikchrRenderer::new();
        let code_hash = 55;
        renderer
            .core
            .wanted_bucket
            .borrow_mut()
            .insert(code_hash, 320);

        assert!(
            !renderer.core.is_stale_result("pikchr:malformed", code_hash),
            "Malformed texture keys should be treated as fresh"
        );
    }

    #[test]
    fn test_pending_set_basics() {
        let renderer = PikchrRenderer::new();

        assert!(
            !renderer.has_pending(),
            "Pending should be empty on new renderer"
        );

        renderer.core.pending.borrow_mut().insert(42);
        assert!(
            renderer.has_pending(),
            "Pending should be non-empty after insert"
        );

        renderer.core.pending.borrow_mut().remove(&42);
        assert!(
            !renderer.has_pending(),
            "Pending should be empty after remove"
        );
    }

    #[test]
    fn test_release_gpu_clears_pending_and_wanted() {
        let renderer = PikchrRenderer::new();

        renderer.core.pending.borrow_mut().insert(1);
        renderer.core.wanted_bucket.borrow_mut().insert(1, 640);
        renderer.core.debounce.borrow_mut().insert(
            1,
            DiagramDebounce {
                last_rasterized_bucket: 640,
                last_seen_bucket: 640,
                bucket_changed_at: None,
            },
        );
        renderer
            .core
            .latest_texture_key
            .borrow_mut()
            .insert(1, "pikchr:key".to_string());

        renderer.release_gpu_textures();

        assert!(renderer.core.pending.borrow().is_empty());
        assert!(renderer.core.wanted_bucket.borrow().is_empty());
        assert!(renderer.core.debounce.borrow().is_empty());
        assert!(renderer.core.latest_texture_key.borrow().is_empty());
    }

    #[test]
    fn test_queue_full_does_not_block() {
        let (tx, _rx) = crossbeam_channel::bounded::<RasterJob>(2);

        for i in 0..2 {
            tx.send(RasterJob {
                code_hash: i,
                texture_key: format!("pikchr:dummy:w640:s100:dm0:{}", i),
                code: Some("box".to_string()),
                svg: None,
                dark_mode: false,
                width_bucket: 640,
                scale_bucket: 100,
            })
            .unwrap();
        }

        let result = tx.try_send(RasterJob {
            code_hash: 99,
            texture_key: "pikchr:dummy:w640:s100:dm0:99".to_string(),
            code: Some("box".to_string()),
            svg: None,
            dark_mode: false,
            width_bucket: 640,
            scale_bucket: 100,
        });
        assert!(
            matches!(result, Err(crossbeam_channel::TrySendError::Full(_))),
            "try_send on full channel should return Err(Full), got {:?}",
            result.err()
        );
    }

    #[test]
    fn test_shutdown_joins_worker() {
        let start = Instant::now();
        {
            let renderer = PikchrRenderer::new();
            drop(renderer);
        }
        let elapsed = start.elapsed();
        assert!(
            elapsed < Duration::from_secs(5),
            "Drop should join worker within 5 seconds, took {:?}",
            elapsed
        );
    }

    #[test]
    fn test_pending_dedup_prevents_double_enqueue() {
        let renderer = PikchrRenderer::new();
        let code_hash = 42u64;

        renderer.core.pending.borrow_mut().insert(code_hash);

        assert!(
            renderer.core.pending.borrow().contains(&code_hash),
            "Pending set should contain the inserted code_hash"
        );

        renderer.core.pending.borrow_mut().insert(code_hash);
        assert_eq!(
            renderer.core.pending.borrow().len(),
            1,
            "HashSet dedup: inserting same hash twice should keep len at 1"
        );
    }

    #[test]
    fn test_worker_try_send_full_does_not_block() {
        let fontdb = make_fontdb();
        let (job_tx, job_rx) = crossbeam_channel::bounded::<RasterJob>(4);
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(1);

        result_tx
            .send(RasterResult {
                code_hash: 0,
                texture_key: "dummy".to_string(),
                dark_mode: false,
                svg: None,
                rgba: None,
                raster_size: None,
                display_size: None,
                error: None,
            })
            .unwrap();

        let code = "box \"Test\" fit";
        let code_hash = hash_str(code);
        job_tx
            .send(RasterJob {
                code_hash,
                texture_key: format!("pikchr:{:016x}:w640:s100:dm0", code_hash),
                code: Some(code.to_string()),
                svg: None,
                dark_mode: false,
                width_bucket: 640,
                scale_bucket: 100,
            })
            .unwrap();
        drop(job_tx);

        let handle = std::thread::spawn(move || {
            DiagramRendererCore::worker_loop(
                fontdb,
                job_rx,
                result_tx,
                PikchrRenderer::render_pikchr_to_svg,
                PIKCHR_SUPERSAMPLE,
                Some(PikchrRenderer::inject_svg_font_family),
            );
        });

        let start = Instant::now();
        loop {
            if handle.is_finished() {
                break;
            }
            if start.elapsed() > Duration::from_secs(5) {
                panic!("worker_loop blocked for >5s — try_send likely blocked instead of dropping the result");
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        handle.join().unwrap();

        let dummy = result_rx.try_recv().unwrap();
        assert_eq!(
            dummy.code_hash, 0,
            "Original dummy result should still be in the channel"
        );
    }

    #[test]
    fn test_process_job_with_cached_svg() {
        let fontdb = make_fontdb();
        let valid_svg = r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 50"><rect width="100" height="50" fill="red"/></svg>"#;
        let code_hash = hash_str("cached_diagram");
        let result = process_job(
            &fontdb,
            RasterJob {
                code_hash,
                texture_key: format!("pikchr:{:016x}:w640:s100:dm0", code_hash),
                code: None,
                svg: Some(valid_svg.to_string()),
                dark_mode: false,
                width_bucket: 640,
                scale_bucket: 100,
            },
            PikchrRenderer::render_pikchr_to_svg,
            PIKCHR_SUPERSAMPLE,
            Some(PikchrRenderer::inject_svg_font_family),
        );

        assert!(result.rgba.is_some());
        assert!(result.raster_size.is_some());
        assert!(result.display_size.is_some());
        assert!(result.error.is_none());
    }

    #[test]
    fn test_svg_cache_key_consistency() {
        let code_hash = 12345u64;

        let key_a = svg_cache_key(code_hash, false);
        let key_b = svg_cache_key(code_hash, false);
        assert_eq!(key_a, key_b, "svg_cache_key should be deterministic");

        let key_dark_a = svg_cache_key(code_hash, true);
        let key_dark_b = svg_cache_key(code_hash, true);
        assert_eq!(
            key_dark_a, key_dark_b,
            "svg_cache_key (dark) should be deterministic"
        );

        assert_ne!(
            key_a, key_dark_a,
            "Different dark_mode should produce different keys"
        );
    }

    // (shared cache key, bucketing, debounce, stale result tests already
    // covered in diagram_common::tests -- no need to duplicate here)
}
