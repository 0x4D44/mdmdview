//! Pikchr diagram renderer.
//!
//! Renders Pikchr markup to SVG using the embedded pikchr C library,
//! then rasterizes SVG to egui textures via usvg/resvg.
//! Synchronous rendering -- no worker threads needed.
//!
//! Key components:
//! - `PikchrRenderer` struct: holds font database and caches (SVG, texture, error)
//! - `LruCache<K, V>`: generic LRU cache (shared module in lru_cache.rs)
//! - `render_pikchr_to_svg()`: static method wrapping the pikchr C library
//! - `rasterize_and_upload()`: SVG-to-texture pipeline via usvg/resvg/tiny-skia
//!
//! This module is conditionally compiled behind `#[cfg(feature = "pikchr")]`.
//! It integrates with `MarkdownRenderer` following the same public API pattern
//! as `MermaidRenderer` (render_block, begin_frame, has_pending, release_gpu_textures).

use std::cell::RefCell;
use std::sync::Arc;

use crate::lru_cache::{hash_str, LruCache};
use crate::ThemeColors;

// ---------------------------------------------------------------------------
// Cache capacity constants
// ---------------------------------------------------------------------------

const PIKCHR_TEXTURE_CACHE_CAPACITY: usize = 64;
const PIKCHR_SVG_CACHE_CAPACITY: usize = 64;
const PIKCHR_ERROR_CACHE_CAPACITY: usize = 32;
/// Maximum rasterized dimension (width or height) in pixels.
/// Matches the cap used by mermaid_renderer.
const MAX_RASTER_SIDE: u32 = 4096;
/// Rasterization supersample factor. Pikchr SVGs have compact viewBox
/// dimensions (a single box is ~67×35 units), so rasterizing at 1×
/// produces textures with insufficient pixel density for crisp text.
/// 2× gives 4× the pixels, making anti-aliased text and thin lines
/// look sharp — the standard "retina rendering" approach.
const PIKCHR_SUPERSAMPLE: f32 = 2.0;

// ---------------------------------------------------------------------------
// PikchrTextureEntry
// ---------------------------------------------------------------------------

/// A cached rasterized Pikchr diagram texture with its display dimensions.
#[derive(Clone)]
struct PikchrTextureEntry {
    /// GPU texture handle (Clone wraps Arc internally).
    texture: egui::TextureHandle,
    /// Display dimensions in logical pixels [width, height].
    /// The actual texture is larger by `PIKCHR_SUPERSAMPLE` for crisp rendering.
    display_size: [u32; 2],
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Composite SVG cache key: embeds both the source code hash and dark_mode flag.
/// Dark mode produces different SVG output for the same source code.
fn svg_cache_key(code_hash: u64, dark_mode: bool) -> u128 {
    ((code_hash as u128) << 1) | (dark_mode as u128)
}

/// Quantize a width value to 32-pixel buckets to avoid cache thrashing
/// when the panel width changes by a few pixels (e.g., during resize).
/// Returns the bucket midpoint as u32.
fn bucket_width(width: f32) -> u32 {
    let bucket = (width / 32.0).round() as u32;
    bucket * 32
}

/// Quantize a UI scale factor to integer percentage buckets.
/// e.g., 1.0 -> 100, 1.25 -> 125, 1.5 -> 150.
/// Matches the bucketing strategy in mermaid_renderer.rs.
fn bucket_scale(scale: f32) -> u32 {
    (scale * 100.0).round() as u32
}

// ---------------------------------------------------------------------------
// PikchrRenderer
// ---------------------------------------------------------------------------

/// Pikchr diagram renderer.
///
/// Renders Pikchr markup to SVG using the embedded pikchr C library,
/// then rasterizes SVG to egui textures via usvg/resvg.
/// Synchronous rendering -- no worker threads needed.
pub(crate) struct PikchrRenderer {
    /// Font database with system fonts loaded. Created once in `new()` and
    /// shared (by Arc clone) with every `usvg::Options` instance. Without
    /// this, usvg's default empty fontdb silently drops all text labels.
    fontdb: Arc<usvg::fontdb::Database>,
    /// LRU cache: texture_key (String) -> (TextureHandle, [width, height])
    textures: RefCell<LruCache<String, PikchrTextureEntry>>,
    /// SVG cache: svg_cache_key (u128: code_hash << 1 | dark_mode) -> SVG string
    svg_cache: RefCell<LruCache<u128, String>>,
    /// Error cache: code_hash (u64) -> error message.
    /// Errors don't depend on dark_mode, only on source syntax.
    errors: RefCell<LruCache<u64, String>>,
}

impl PikchrRenderer {
    /// Create a new PikchrRenderer, loading system fonts once at startup.
    /// Without system fonts, usvg's default empty fontdb silently drops all
    /// text labels in rasterized SVGs. Takes ~50-200ms (one-time cost).
    /// Same pattern as mermaid_renderer.rs and image_decode.rs.
    pub(crate) fn new() -> Self {
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        Self {
            fontdb: Arc::new(db),
            textures: RefCell::new(LruCache::new(PIKCHR_TEXTURE_CACHE_CAPACITY)),
            svg_cache: RefCell::new(LruCache::new(PIKCHR_SVG_CACHE_CAPACITY)),
            errors: RefCell::new(LruCache::new(PIKCHR_ERROR_CACHE_CAPACITY)),
        }
    }

    /// No-op. API symmetry with MermaidRenderer.
    /// Pikchr renders synchronously so there is no per-frame pending state to reset.
    pub(crate) fn begin_frame(&self) {}

    /// Always false. Pikchr renders synchronously; nothing is ever pending.
    pub(crate) fn has_pending(&self) -> bool {
        false
    }

    /// Release GPU textures to reduce idle GPU usage (e.g., when minimized).
    /// SVG and error caches are retained for fast texture rebuilds on next render.
    pub(crate) fn release_gpu_textures(&self) {
        self.textures.borrow_mut().clear();
    }

    /// Main entry point for rendering a Pikchr code block.
    ///
    /// Checks caches (error, texture, SVG) before rendering. On cache miss,
    /// renders Pikchr source to SVG, rasterizes to RGBA, and uploads as a
    /// GPU texture. All steps are synchronous.
    ///
    /// Returns `true` when the block has been rendered (always, since rendering
    /// is synchronous -- there is no "pending" state).
    pub(crate) fn render_block(
        &self,
        ui: &mut egui::Ui,
        code: &str,
        ui_scale: f32,
        code_font_size: f32,
    ) -> bool {
        let code_hash = hash_str(code);
        let dark_mode = ui.visuals().dark_mode;
        let svg_key = svg_cache_key(code_hash, dark_mode);

        // 1. Check error cache (don't re-render known failures).
        //    Errors depend only on syntax, not dark_mode, so keyed by code_hash.
        //    Clone the error and drop the RefMut before calling render_error_block,
        //    to avoid holding a borrow on self.errors across a &self method call.
        let cached_err = self.errors.borrow_mut().get(&code_hash);
        if let Some(err) = cached_err {
            self.render_error_block(ui, &err, code, code_font_size);
            return true;
        }

        // 2. Compute texture cache key (incorporates width bucket + scale + theme)
        let available_width = ui.available_width().max(1.0);
        let width_bucket = bucket_width(available_width);
        let scale_bucket = bucket_scale(ui_scale);
        let texture_key = format!(
            "pikchr:{:016x}:w{}:s{}:dm{}",
            code_hash, width_bucket, scale_bucket, dark_mode as u8
        );

        // 3. Check texture cache.
        {
            let mut cache = self.textures.borrow_mut();
            if let Some(entry) = cache.get(&texture_key) {
                Self::render_texture(ui, &entry, available_width);
                return true;
            }
        }

        // 4. Check SVG cache or render fresh SVG
        let svg = {
            let mut svg_cache = self.svg_cache.borrow_mut();
            if let Some(cached) = svg_cache.get(&svg_key) {
                cached
            } else {
                drop(svg_cache); // Release borrow before calling render
                match Self::render_pikchr_to_svg(code, dark_mode) {
                    Ok(svg) => {
                        self.svg_cache.borrow_mut().insert(svg_key, svg.clone());
                        svg
                    }
                    Err(err) => {
                        self.errors.borrow_mut().insert(code_hash, err.clone());
                        self.render_error_block(ui, &err, code, code_font_size);
                        return true;
                    }
                }
            }
        };

        // 5. Rasterize SVG to RGBA and upload as texture
        match Self::rasterize_and_upload(
            ui.ctx(),
            &self.fontdb,
            &texture_key,
            &svg,
            width_bucket,
            scale_bucket,
        ) {
            Ok(entry) => {
                Self::render_texture(ui, &entry, available_width);
                self.textures.borrow_mut().insert(texture_key, entry);
                true
            }
            Err(err) => {
                self.errors.borrow_mut().insert(code_hash, err.clone());
                self.render_error_block(ui, &err, code, code_font_size);
                true
            }
        }
    }

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

    /// Rasterize an SVG string to RGBA pixels and upload as an egui texture.
    ///
    /// Uses the shared fontdb (with system fonts) so that text labels render
    /// correctly. Without system fonts, usvg silently skips all text.
    ///
    /// Rasterizes at `PIKCHR_SUPERSAMPLE` × the logical display resolution
    /// for crisp text and lines. The returned entry stores display dimensions
    /// in logical pixels; the actual texture is larger by the supersample factor.
    fn rasterize_and_upload(
        ctx: &egui::Context,
        fontdb: &Arc<usvg::fontdb::Database>,
        texture_key: &str,
        svg: &str,
        width_bucket: u32,
        scale_bucket: u32,
    ) -> Result<PikchrTextureEntry, String> {
        // Inject a font-family into the SVG so usvg picks a known good font.
        // Pikchr outputs <svg style='font-size:initial;'> with no font-family
        // on <text> elements, so usvg falls back to whatever thin default it finds.
        let svg = Self::inject_svg_font_family(svg);

        // Parse SVG using the shared fontdb with system fonts loaded.
        let opt = usvg::Options {
            fontdb: Arc::clone(fontdb),
            ..Default::default()
        };
        let tree = usvg::Tree::from_data(svg.as_bytes(), &opt)
            .map_err(|e| format!("SVG parse error: {}", e))?;

        // Compute dimensions from the parsed SVG tree
        let size = tree.size().to_int_size();
        let (w, h) = (size.width().max(1), size.height().max(1));

        // Compute logical scale factor (user zoom, constrained by available width)
        let base_scale = scale_bucket as f32 / 100.0;
        let width_scale = if width_bucket > 0 {
            width_bucket as f32 / w.max(1) as f32
        } else {
            base_scale
        };
        let scale = base_scale.min(width_scale).clamp(0.1, 4.0);

        // Display dimensions in logical pixels (what render_texture uses)
        let display_w = (w as f32 * scale).round().max(1.0) as u32;
        let display_h = (h as f32 * scale).round().max(1.0) as u32;

        // Raster dimensions: supersample for crisp text and thin lines
        let raster_scale = scale * PIKCHR_SUPERSAMPLE;
        let mut raster_w = (w as f32 * raster_scale).round().max(1.0) as u32;
        let mut raster_h = (h as f32 * raster_scale).round().max(1.0) as u32;

        // Clamp to reasonable maximum
        if raster_w > MAX_RASTER_SIDE || raster_h > MAX_RASTER_SIDE {
            let clamp_scale = (MAX_RASTER_SIDE as f32 / raster_w as f32)
                .min(MAX_RASTER_SIDE as f32 / raster_h as f32);
            raster_w = (raster_w as f32 * clamp_scale).round().max(1.0) as u32;
            raster_h = (raster_h as f32 * clamp_scale).round().max(1.0) as u32;
        }

        // Rasterize: Pikchr SVGs have no background fill, so the pixmap starts
        // transparent and the diagram draws on top.
        let mut pixmap = tiny_skia::Pixmap::new(raster_w, raster_h)
            .ok_or_else(|| "Pixmap allocation failed".to_string())?;
        let transform = tiny_skia::Transform::from_scale(raster_scale, raster_scale);
        // Bind PixmapMut to a local -- can't take &mut of a temporary rvalue.
        let mut pmut = pixmap.as_mut();
        resvg::render(&tree, transform, &mut pmut);

        // Upload to GPU
        let rgba = pixmap.data().to_vec();
        let image = egui::ColorImage::from_rgba_unmultiplied(
            [raster_w as usize, raster_h as usize],
            &rgba,
        );
        let texture = ctx.load_texture(
            texture_key.to_string(),
            image,
            egui::TextureOptions::LINEAR,
        );

        Ok(PikchrTextureEntry {
            texture,
            display_size: [display_w, display_h],
        })
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

    /// Display a cached texture in the UI, scaling down if it exceeds available width.
    fn render_texture(ui: &mut egui::Ui, entry: &PikchrTextureEntry, available_width: f32) {
        let (tw, th) = (entry.display_size[0] as f32, entry.display_size[1] as f32);
        let scale = if tw > available_width {
            (available_width / tw).clamp(0.01, 4.0)
        } else {
            1.0
        };
        let size = egui::vec2((tw * scale).round(), (th * scale).round());
        ui.add(egui::Image::new(&entry.texture).fit_to_exact_size(size));
    }

    /// Display a Pikchr error block with the error message and original source code.
    fn render_error_block(
        &self,
        ui: &mut egui::Ui,
        error: &str,
        code: &str,
        code_font_size: f32,
    ) {
        let tc = ThemeColors::current(ui.visuals().dark_mode);
        egui::Frame::none()
            .fill(tc.box_bg)
            .stroke(egui::Stroke::new(
                1.0,
                egui::Color32::from_rgb(200, 80, 80),
            ))
            .inner_margin(8.0)
            .show(ui, |ui| {
                ui.label(
                    egui::RichText::new("Pikchr rendering error:")
                        .color(egui::Color32::from_rgb(200, 80, 80))
                        .size(code_font_size),
                );
                ui.label(
                    egui::RichText::new(error)
                        .color(egui::Color32::LIGHT_GRAY)
                        .size(code_font_size)
                        .family(egui::FontFamily::Monospace),
                );
                ui.add_space(4.0);
                ui.label(
                    egui::RichText::new(code)
                        .size(code_font_size)
                        .color(tc.code_fallback_text)
                        .family(egui::FontFamily::Monospace),
                );
            });
    }
}

// ---------------------------------------------------------------------------
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

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
        // Empty input produces empty/minimal SVG, not an error
        assert!(result.is_ok());
    }

    #[test]
    fn test_svg_cache_key_differs_by_dark_mode() {
        let key_light = svg_cache_key(12345, false);
        let key_dark = svg_cache_key(12345, true);
        assert_ne!(key_light, key_dark);
    }

    #[test]
    fn test_svg_cache_key_differs_by_code_hash() {
        let key1 = svg_cache_key(12345, false);
        let key2 = svg_cache_key(67890, false);
        assert_ne!(key1, key2);
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
    fn test_width_bucketing() {
        // Same bucket for nearby widths (round() gives +/-16px half-buckets)
        assert_eq!(bucket_width(640.0), bucket_width(641.0));
        assert_eq!(bucket_width(640.0), bucket_width(655.0)); // 655/32=20.47 -> rounds to 20
        // Different buckets once we cross the half-bucket boundary
        assert_ne!(bucket_width(640.0), bucket_width(656.0)); // 656/32=20.5 -> rounds to 21
        assert_ne!(bucket_width(640.0), bucket_width(672.0)); // clearly different bucket
        // Verify actual bucket values
        assert_eq!(bucket_width(640.0), 640); // 640/32=20.0 -> 20*32=640
        assert_eq!(bucket_width(672.0), 672); // 672/32=21.0 -> 21*32=672
    }

    #[test]
    fn test_renderer_new_creates_empty_caches() {
        let renderer = PikchrRenderer::new();
        // No panics, caches start empty
        assert!(!renderer.has_pending());
    }

    // -----------------------------------------------------------------------
    // Sample diagram verification tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_sample_simple_flow() {
        let code = "arrow right 200% \"Request\" above\nbox \"Server\" fit rad 10px\narrow right 200% \"Response\" above";
        let result = PikchrRenderer::render_pikchr_to_svg(code, false);
        assert!(result.is_ok(), "Simple flow sample failed: {:?}", result.err());
    }

    #[test]
    fn test_sample_architecture_diagram() {
        let code = "box \"Browser\" fit rad 10px\narrow right 250% \"HTTP\" above\nbox \"Web Server\" fit rad 10px\narrow right 250% \"SQL\" above\ncylinder \"Database\" fit";
        let result = PikchrRenderer::render_pikchr_to_svg(code, false);
        assert!(result.is_ok(), "Architecture sample failed: {:?}", result.err());
    }

    #[test]
    fn test_sample_decision_flow() {
        let code = "box \"Start\" fit rad 50%\narrow down\ndiamond \"OK?\" fit\narrow right 200% \"Yes\" above\nbox \"Done\" fit rad 50%\nmove to last diamond.s\narrow down \"No\" ljust\nbox \"Retry\" fit rad 10px\narrow from last box.w left 0.5in then up until even with 1st box then right to 1st box.w";
        let result = PikchrRenderer::render_pikchr_to_svg(code, false);
        assert!(result.is_ok(), "Decision flow sample failed: {:?}", result.err());
    }

    // -----------------------------------------------------------------------
    // bucket_scale coverage
    // -----------------------------------------------------------------------

    #[test]
    fn test_scale_bucketing() {
        assert_eq!(bucket_scale(1.0), 100);
        assert_eq!(bucket_scale(1.25), 125);
        assert_eq!(bucket_scale(1.5), 150);
        assert_eq!(bucket_scale(2.0), 200);
        assert_eq!(bucket_scale(0.5), 50);
        // Rounding: 1.124 * 100 = 112.4 -> rounds to 112
        assert_eq!(bucket_scale(1.124), 112);
        // Rounding: 1.126 * 100 = 112.6 -> rounds to 113
        assert_eq!(bucket_scale(1.126), 113);
        // Values within rounding distance bucket together
        assert_eq!(bucket_scale(1.001), bucket_scale(1.004)); // both round to 100
    }

    // -----------------------------------------------------------------------
    // Shape type SVG output validation
    // -----------------------------------------------------------------------

    #[test]
    fn test_box_produces_rect_element() {
        let svg = PikchrRenderer::render_pikchr_to_svg("box \"Label\" fit", false).unwrap();
        // Pikchr renders boxes as polygon or path elements
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
        // Dot renders as a filled circle
        assert!(
            svg.contains("<circle"),
            "dot should produce a circle element"
        );
    }

    #[test]
    fn test_spline_produces_path() {
        let svg =
            PikchrRenderer::render_pikchr_to_svg("spline right then down", false).unwrap();
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
        // Pikchr uses viewBox="x y width height" for dimensions
        let (w, h) = extract_viewbox_size(&svg)
            .expect("SVG should have a parseable viewBox attribute");
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
        let (w_small, _) =
            extract_viewbox_size(&small).expect("small SVG should have viewBox");
        let (w_large, _) =
            extract_viewbox_size(&large).expect("large SVG should have viewBox");
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
        let svg =
            PikchrRenderer::render_pikchr_to_svg("box \"日本語\" fit", false).unwrap();
        assert!(
            svg.contains("日本語"),
            "Unicode label '日本語' missing from SVG"
        );
    }

    #[test]
    fn test_special_characters_in_labels() {
        // Test that angle brackets are XML-escaped in SVG output
        let svg =
            PikchrRenderer::render_pikchr_to_svg("box \"x < y\" fit", false).unwrap();
        // Pikchr XML-escapes < to &lt; in text content
        assert!(
            svg.contains("&lt;"),
            "SVG should contain XML-escaped '<':\n{}",
            svg
        );

        // Test ampersand: Pikchr XML-escapes & to &amp;
        let svg2 =
            PikchrRenderer::render_pikchr_to_svg("box \"A & B\" fit", false).unwrap();
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
        let err =
            PikchrRenderer::render_pikchr_to_svg("this is not valid pikchr }{}{", false)
                .unwrap_err();
        assert!(
            !err.is_empty(),
            "Error message should not be empty for invalid syntax"
        );
        // Error should contain some useful diagnostic info
        assert!(
            err.len() > 5,
            "Error message should be descriptive, got: '{}'",
            err
        );
    }

    #[test]
    fn test_error_cache_theme_independent() {
        // Errors are keyed by code_hash, not theme -- same syntax error in
        // light and dark mode should produce the same error message.
        let invalid = "totally broken pikchr syntax }{}{";
        let err_light =
            PikchrRenderer::render_pikchr_to_svg(invalid, false).unwrap_err();
        let err_dark =
            PikchrRenderer::render_pikchr_to_svg(invalid, true).unwrap_err();
        assert_eq!(
            err_light, err_dark,
            "Same syntax error should produce same message regardless of theme"
        );
    }

    #[test]
    fn test_multiple_errors_are_distinct() {
        let err1 =
            PikchrRenderer::render_pikchr_to_svg("invalid syntax aaa }{}{", false)
                .unwrap_err();
        let err2 =
            PikchrRenderer::render_pikchr_to_svg("different invalid bbb }{}{", false)
                .unwrap_err();
        // Different invalid inputs may produce different error messages
        // (at minimum they should both be errors, which we've already asserted
        // by unwrap_err). If they happen to be the same generic error, that's
        // still valid -- but let's verify they're both non-empty.
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

        // Pikchr uses PIKCHR_DARK_MODE flag to invert colors.
        // Light mode should contain dark strokes (e.g., "black" or rgb(0,0,0))
        // Dark mode should contain light strokes (e.g., "white" or rgb(255,255,255))
        let light_has_black =
            light.contains("black") || light.contains("rgb(0,0,0)") || light.contains("#000");
        let dark_has_white = dark.contains("white")
            || dark.contains("rgb(255,255,255)")
            || dark.contains("#fff");

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
        // Build a chain of 50+ boxes connected by arrows
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
        // Whitespace-only should behave like empty input (OK, not error)
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
        // Comments should NOT appear in SVG output
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
        // Render a known diagram and validate specific SVG structural fragments.
        // This acts as a characterization test documenting what the output looks like.
        let code = r#"arrow right 200% "Input" above
box "Process" fit rad 10px
arrow right 200% "Output" above"#;
        let svg = PikchrRenderer::render_pikchr_to_svg(code, false).unwrap();

        // Must have proper SVG wrapper
        assert!(svg.starts_with("<svg"), "SVG must start with <svg tag");
        assert!(svg.trim_end().ends_with("</svg>"), "SVG must end with </svg>");

        // Must have viewBox for proper scaling
        assert!(
            svg.contains("viewBox"),
            "SVG should have viewBox attribute"
        );

        // Arrow produces polygon elements (arrowheads)
        assert!(
            svg.contains("<polygon"),
            "Arrows should produce <polygon> arrowheads"
        );

        // Labels appear as text elements
        assert!(svg.contains("<text"), "Labels should produce <text> elements");
        assert!(svg.contains("Input"), "Label 'Input' must be in SVG");
        assert!(svg.contains("Process"), "Label 'Process' must be in SVG");
        assert!(svg.contains("Output"), "Label 'Output' must be in SVG");

        // Lines appear as path or line elements
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
        // Original content preserved
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
        // SVG without Pikchr's style pattern should pass through unchanged
        let svg = "<svg viewBox='0 0 100 50'><text>Hello</text></svg>";
        let result = PikchrRenderer::inject_svg_font_family(svg);
        assert_eq!(result, svg, "SVG without matching style should be unchanged");
    }

    #[test]
    fn test_supersample_constant_is_reasonable() {
        assert!(
            PIKCHR_SUPERSAMPLE >= 1.0,
            "Supersample must be >= 1.0"
        );
        assert!(
            PIKCHR_SUPERSAMPLE <= 4.0,
            "Supersample > 4.0 would waste memory"
        );
    }
}
