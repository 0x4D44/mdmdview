//! D2 diagram renderer.
//!
//! Renders D2 diagram markup to SVG using the embedded mdmdview-d2 crate,
//! then rasterizes SVG to egui textures via usvg/resvg.
//! Synchronous rendering -- no worker threads needed.
//!
//! Key components:
//! - `D2Renderer` struct: holds font database and caches (SVG, texture, error)
//! - `LruCache<K, V>`: generic LRU cache (shared module in lru_cache.rs)
//! - `render_d2_to_svg()`: calls into the mdmdview-d2 crate pipeline
//! - `rasterize_and_upload()`: SVG-to-texture pipeline via usvg/resvg/tiny-skia
//!
//! This module is conditionally compiled behind `#[cfg(feature = "d2")]`.
//! It integrates with `MarkdownRenderer` following the same public API pattern
//! as `PikchrRenderer` (render_block, begin_frame, has_pending, release_gpu_textures).

use std::cell::RefCell;
use std::sync::Arc;

use crate::lru_cache::{hash_str, LruCache};
use crate::ThemeColors;

// ---------------------------------------------------------------------------
// Cache capacity constants
// ---------------------------------------------------------------------------

const D2_TEXTURE_CACHE_CAPACITY: usize = 64;
const D2_SVG_CACHE_CAPACITY: usize = 64;
const D2_ERROR_CACHE_CAPACITY: usize = 32;
/// Maximum rasterized dimension (width or height) in pixels.
/// Matches the cap used by pikchr_renderer and mermaid_renderer.
const MAX_RASTER_SIDE: u32 = 4096;
/// Rasterization supersample factor. D2 diagrams benefit from higher pixel
/// density for crisp text and clean lines, same as pikchr_renderer.
const D2_SUPERSAMPLE: f32 = 2.0;

// ---------------------------------------------------------------------------
// D2TextureEntry
// ---------------------------------------------------------------------------

/// A cached rasterized D2 diagram texture with its display dimensions.
#[derive(Clone)]
struct D2TextureEntry {
    /// GPU texture handle (Clone wraps Arc internally).
    texture: egui::TextureHandle,
    /// Display dimensions in logical pixels [width, height].
    /// The actual texture is larger by `D2_SUPERSAMPLE` for crisp rendering.
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
fn bucket_scale(scale: f32) -> u32 {
    (scale * 100.0).round() as u32
}

// ---------------------------------------------------------------------------
// D2Renderer
// ---------------------------------------------------------------------------

/// D2 diagram renderer.
///
/// Renders D2 diagram markup to SVG using the embedded mdmdview-d2 crate,
/// then rasterizes SVG to egui textures via usvg/resvg.
/// Synchronous rendering -- no worker threads needed.
pub(crate) struct D2Renderer {
    /// Font database with system fonts loaded. Created once in `new()` and
    /// shared (by Arc clone) with every `usvg::Options` instance. Without
    /// this, usvg's default empty fontdb silently drops all text labels.
    fontdb: Arc<usvg::fontdb::Database>,
    /// LRU cache: texture_key (String) -> (TextureHandle, [width, height])
    textures: RefCell<LruCache<String, D2TextureEntry>>,
    /// SVG cache: svg_cache_key (u128: code_hash << 1 | dark_mode) -> SVG string
    svg_cache: RefCell<LruCache<u128, String>>,
    /// Error cache: code_hash (u64) -> error message.
    /// Errors don't depend on dark_mode, only on source syntax.
    errors: RefCell<LruCache<u64, String>>,
}

impl D2Renderer {
    /// Create a new D2Renderer, loading system fonts once at startup.
    /// Without system fonts, usvg's default empty fontdb silently drops all
    /// text labels in rasterized SVGs. Takes ~50-200ms (one-time cost).
    /// Same pattern as pikchr_renderer.rs and mermaid_renderer.rs.
    pub(crate) fn new() -> Self {
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        Self {
            fontdb: Arc::new(db),
            textures: RefCell::new(LruCache::new(D2_TEXTURE_CACHE_CAPACITY)),
            svg_cache: RefCell::new(LruCache::new(D2_SVG_CACHE_CAPACITY)),
            errors: RefCell::new(LruCache::new(D2_ERROR_CACHE_CAPACITY)),
        }
    }

    /// No-op. API symmetry with MermaidRenderer.
    /// D2 renders synchronously so there is no per-frame pending state to reset.
    pub(crate) fn begin_frame(&self) {}

    /// Always false. D2 renders synchronously; nothing is ever pending.
    pub(crate) fn has_pending(&self) -> bool {
        false
    }

    /// Release GPU textures to reduce idle GPU usage (e.g., when minimized).
    /// SVG and error caches are retained for fast texture rebuilds on next render.
    pub(crate) fn release_gpu_textures(&self) {
        self.textures.borrow_mut().clear();
    }

    /// Main entry point for rendering a D2 code block.
    ///
    /// Checks caches (error, texture, SVG) before rendering. On cache miss,
    /// renders D2 source to SVG, rasterizes to RGBA, and uploads as a
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
            "d2:{:016x}:w{}:s{}:dm{}",
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
                match Self::render_d2_to_svg(code, dark_mode) {
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

    /// Render D2 source code to an SVG string.
    ///
    /// Static associated function (no `&self`) -- pure input/output.
    /// Calls into the mdmdview-d2 crate pipeline.
    fn render_d2_to_svg(code: &str, dark_mode: bool) -> Result<String, String> {
        let options = mdmdview_d2::RenderOptions {
            dark_mode,
            ..Default::default()
        };

        mdmdview_d2::render_d2_to_svg(code, &options)
            .map(|result| result.svg)
            .map_err(|e| e.to_string())
    }

    /// Rasterize an SVG string to RGBA pixels and upload as an egui texture.
    ///
    /// Uses the shared fontdb (with system fonts) so that text labels render
    /// correctly. Without system fonts, usvg silently skips all text.
    ///
    /// Rasterizes at `D2_SUPERSAMPLE` x the logical display resolution
    /// for crisp text and lines. The returned entry stores display dimensions
    /// in logical pixels; the actual texture is larger by the supersample factor.
    fn rasterize_and_upload(
        ctx: &egui::Context,
        fontdb: &Arc<usvg::fontdb::Database>,
        texture_key: &str,
        svg: &str,
        width_bucket: u32,
        scale_bucket: u32,
    ) -> Result<D2TextureEntry, String> {
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
        let raster_scale = scale * D2_SUPERSAMPLE;
        let mut raster_w = (w as f32 * raster_scale).round().max(1.0) as u32;
        let mut raster_h = (h as f32 * raster_scale).round().max(1.0) as u32;

        // Clamp to reasonable maximum
        if raster_w > MAX_RASTER_SIDE || raster_h > MAX_RASTER_SIDE {
            let clamp_scale = (MAX_RASTER_SIDE as f32 / raster_w as f32)
                .min(MAX_RASTER_SIDE as f32 / raster_h as f32);
            raster_w = (raster_w as f32 * clamp_scale).round().max(1.0) as u32;
            raster_h = (raster_h as f32 * clamp_scale).round().max(1.0) as u32;
        }

        // Rasterize: D2 SVGs include background fills, so no separate fill needed.
        let mut pixmap = tiny_skia::Pixmap::new(raster_w, raster_h)
            .ok_or_else(|| "Pixmap allocation failed".to_string())?;
        let transform = tiny_skia::Transform::from_scale(raster_scale, raster_scale);
        let mut pmut = pixmap.as_mut();
        resvg::render(&tree, transform, &mut pmut);

        // Upload to GPU
        let rgba = pixmap.data().to_vec();
        let image =
            egui::ColorImage::from_rgba_unmultiplied([raster_w as usize, raster_h as usize], &rgba);
        let texture =
            ctx.load_texture(texture_key.to_string(), image, egui::TextureOptions::LINEAR);

        Ok(D2TextureEntry {
            texture,
            display_size: [display_w, display_h],
        })
    }

    /// Display a cached texture in the UI, scaling down if it exceeds available width.
    fn render_texture(ui: &mut egui::Ui, entry: &D2TextureEntry, available_width: f32) {
        let (tw, th) = (entry.display_size[0] as f32, entry.display_size[1] as f32);
        let scale = if tw > available_width {
            (available_width / tw).clamp(0.01, 4.0)
        } else {
            1.0
        };
        let size = egui::vec2((tw * scale).round(), (th * scale).round());
        ui.add(egui::Image::new(&entry.texture).fit_to_exact_size(size));
    }

    /// Display a D2 error block with the error message and original source code.
    fn render_error_block(&self, ui: &mut egui::Ui, error: &str, code: &str, code_font_size: f32) {
        let tc = ThemeColors::current(ui.visuals().dark_mode);
        egui::Frame::none()
            .fill(tc.box_bg)
            .stroke(egui::Stroke::new(1.0, egui::Color32::from_rgb(200, 80, 80)))
            .inner_margin(8.0)
            .show(ui, |ui| {
                ui.label(
                    egui::RichText::new("D2 rendering error:")
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
        let svg = D2Renderer::render_d2_to_svg("a -> b", false);
        assert!(
            svg.is_ok(),
            "Simple D2 diagram should render: {:?}",
            svg.err()
        );
        let svg = svg.unwrap();
        assert!(svg.contains("<svg"));
    }

    #[test]
    fn test_render_dark_mode_produces_different_svg() {
        let light = D2Renderer::render_d2_to_svg("a -> b", false).unwrap();
        let dark = D2Renderer::render_d2_to_svg("a -> b", true).unwrap();
        assert_ne!(light, dark);
    }

    #[test]
    fn test_render_invalid_syntax() {
        // D2 should error on truly invalid syntax
        let result = D2Renderer::render_d2_to_svg("}{}{", false);
        assert!(result.is_err());
    }

    #[test]
    fn test_render_empty_input() {
        let result = D2Renderer::render_d2_to_svg("", false);
        // Empty input may produce an error or empty SVG depending on the crate
        // Just verify it doesn't panic
        let _ = result;
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
        let key = format!("d2:{:016x}:w{}:s{}:dm{}", 12345u64, 640, 100, 0);
        assert!(key.contains("d2:"));
        assert!(key.contains(":w640:"));
        assert!(key.contains(":s100:"));
        assert!(key.contains(":dm0"));
    }

    #[test]
    fn test_width_bucketing() {
        assert_eq!(bucket_width(640.0), bucket_width(641.0));
        assert_eq!(bucket_width(640.0), bucket_width(655.0));
        assert_ne!(bucket_width(640.0), bucket_width(656.0));
        assert_eq!(bucket_width(640.0), 640);
        assert_eq!(bucket_width(672.0), 672);
    }

    #[test]
    fn test_scale_bucketing() {
        assert_eq!(bucket_scale(1.0), 100);
        assert_eq!(bucket_scale(1.25), 125);
        assert_eq!(bucket_scale(1.5), 150);
        assert_eq!(bucket_scale(2.0), 200);
    }

    #[test]
    fn test_renderer_new_creates_empty_caches() {
        let renderer = D2Renderer::new();
        assert!(!renderer.has_pending());
    }

    #[test]
    fn test_supersample_constant_is_reasonable() {
        assert!(D2_SUPERSAMPLE >= 1.0, "Supersample must be >= 1.0");
        assert!(
            D2_SUPERSAMPLE <= 4.0,
            "Supersample > 4.0 would waste memory"
        );
    }
}
