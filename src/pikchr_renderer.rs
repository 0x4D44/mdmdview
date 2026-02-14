//! Pikchr diagram renderer.
//!
//! Renders Pikchr markup to SVG using the embedded pikchr C library,
//! then rasterizes SVG to egui textures via usvg/resvg.
//! Synchronous rendering -- no worker threads needed.
//!
//! Key components:
//! - `PikchrRenderer` struct: holds font database and caches (SVG, texture, error)
//! - `LruCache<K, V>`: generic LRU cache (copied from mermaid_renderer.rs)
//! - `render_pikchr_to_svg()`: static method wrapping the pikchr C library
//! - `rasterize_and_upload()`: SVG-to-texture pipeline via usvg/resvg/tiny-skia
//!
//! This module is conditionally compiled behind `#[cfg(feature = "pikchr")]`.
//! It integrates with `MarkdownRenderer` following the same public API pattern
//! as `MermaidRenderer` (render_block, begin_frame, has_pending, release_gpu_textures).

use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use crate::ThemeColors;

// ---------------------------------------------------------------------------
// Cache capacity constants
// ---------------------------------------------------------------------------

const PIKCHR_TEXTURE_CACHE_CAPACITY: usize = 64;
const PIKCHR_SVG_CACHE_CAPACITY: usize = 64;
const PIKCHR_ERROR_CACHE_CAPACITY: usize = 32;

// ---------------------------------------------------------------------------
// LruCache<K, V> — copied from mermaid_renderer.rs (true LRU with access-order
// tracking via get(&mut self)).
// ---------------------------------------------------------------------------

struct LruCache<K, V> {
    entries: HashMap<K, V>,
    order: VecDeque<K>,
    capacity: usize,
}

impl<K, V> LruCache<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn new(capacity: usize) -> Self {
        Self {
            entries: HashMap::new(),
            order: VecDeque::new(),
            capacity: capacity.max(1),
        }
    }

    fn get(&mut self, key: &K) -> Option<V> {
        let value = self.entries.get(key).cloned();
        self.touch_if_present(&value, key);
        value
    }

    fn insert(&mut self, key: K, value: V) {
        if self.entries.contains_key(&key) {
            self.entries.insert(key.clone(), value);
            self.touch(&key);
            return;
        }
        while self.entries.len() >= self.capacity {
            if !self.evict_oldest() {
                break;
            }
        }
        self.order.push_back(key.clone());
        self.entries.insert(key, value);
    }

    fn remove(&mut self, key: &K) {
        self.entries.remove(key);
        self.order.retain(|entry| entry != key);
    }

    fn clear(&mut self) {
        self.entries.clear();
        self.order.clear();
    }

    fn touch_if_present(&mut self, value: &Option<V>, key: &K) {
        if value.is_some() {
            self.touch(key);
        }
    }

    fn evict_oldest(&mut self) -> bool {
        if let Some(old) = self.order.pop_front() {
            self.entries.remove(&old);
            true
        } else {
            false
        }
    }

    fn touch(&mut self, key: &K) {
        self.order.retain(|entry| entry != key);
        self.order.push_back(key.clone());
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.entries.len()
    }
}

// ---------------------------------------------------------------------------
// PikchrTextureEntry
// ---------------------------------------------------------------------------

/// A cached rasterized Pikchr diagram texture with its pixel dimensions.
#[derive(Clone)]
struct PikchrTextureEntry {
    /// GPU texture handle (Clone wraps Arc internally).
    texture: egui::TextureHandle,
    /// Pixel dimensions [width, height].
    size: [u32; 2],
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Hash a string to a u64 using the standard library's DefaultHasher.
/// Used for cache key generation -- not cryptographic.
fn hash_str(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}

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
    fn rasterize_and_upload(
        ctx: &egui::Context,
        fontdb: &Arc<usvg::fontdb::Database>,
        texture_key: &str,
        svg: &str,
        width_bucket: u32,
        scale_bucket: u32,
    ) -> Result<PikchrTextureEntry, String> {
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

        // Compute scale factor
        let base_scale = scale_bucket as f32 / 100.0;
        let width_scale = if width_bucket > 0 {
            width_bucket as f32 / w.max(1) as f32
        } else {
            base_scale
        };
        let scale = base_scale.min(width_scale).clamp(0.1, 4.0);

        let mut target_w = (w as f32 * scale).round().max(1.0) as u32;
        let mut target_h = (h as f32 * scale).round().max(1.0) as u32;

        // Clamp to reasonable maximum (same as Mermaid: 4096)
        let max_side: u32 = 4096;
        if target_w > max_side || target_h > max_side {
            let clamp_scale =
                (max_side as f32 / target_w as f32).min(max_side as f32 / target_h as f32);
            target_w = (target_w as f32 * clamp_scale).round().max(1.0) as u32;
            target_h = (target_h as f32 * clamp_scale).round().max(1.0) as u32;
        }

        // Rasterize: Pikchr SVGs have no background fill, so the pixmap starts
        // transparent and the diagram draws on top.
        let mut pixmap = tiny_skia::Pixmap::new(target_w, target_h)
            .ok_or_else(|| "Pixmap allocation failed".to_string())?;
        let transform = tiny_skia::Transform::from_scale(scale, scale);
        // Bind PixmapMut to a local -- can't take &mut of a temporary rvalue.
        let mut pmut = pixmap.as_mut();
        resvg::render(&tree, transform, &mut pmut);

        // Upload to GPU
        let rgba = pixmap.data().to_vec();
        let image = egui::ColorImage::from_rgba_unmultiplied(
            [target_w as usize, target_h as usize],
            &rgba,
        );
        let texture = ctx.load_texture(
            texture_key.to_string(),
            image,
            egui::TextureOptions::LINEAR,
        );

        Ok(PikchrTextureEntry {
            texture,
            size: [target_w, target_h],
        })
    }

    /// Display a cached texture in the UI, scaling down if it exceeds available width.
    fn render_texture(ui: &mut egui::Ui, entry: &PikchrTextureEntry, available_width: f32) {
        let (tw, th) = (entry.size[0] as f32, entry.size[1] as f32);
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
    // LruCache unit tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_lru_cache_insert_and_get() {
        let mut cache = LruCache::new(3);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        cache.insert("c".to_string(), 3);
        assert_eq!(cache.get(&"a".to_string()), Some(1));
        assert_eq!(cache.get(&"b".to_string()), Some(2));
        assert_eq!(cache.get(&"c".to_string()), Some(3));
        assert_eq!(cache.get(&"d".to_string()), None);
    }

    #[test]
    fn test_lru_cache_eviction() {
        let mut cache = LruCache::new(2);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        // This should evict "a" (oldest)
        cache.insert("c".to_string(), 3);
        assert_eq!(cache.get(&"a".to_string()), None);
        assert_eq!(cache.get(&"b".to_string()), Some(2));
        assert_eq!(cache.get(&"c".to_string()), Some(3));
    }

    #[test]
    fn test_lru_cache_access_order() {
        let mut cache = LruCache::new(2);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        // Access "a" to make it most recently used
        let _ = cache.get(&"a".to_string());
        // Insert "c" -- should evict "b" (least recently used), not "a"
        cache.insert("c".to_string(), 3);
        assert_eq!(cache.get(&"a".to_string()), Some(1));
        assert_eq!(cache.get(&"b".to_string()), None);
        assert_eq!(cache.get(&"c".to_string()), Some(3));
    }

    #[test]
    fn test_lru_cache_clear() {
        let mut cache = LruCache::new(3);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        assert_eq!(cache.len(), 2);
        cache.clear();
        assert_eq!(cache.len(), 0);
        assert_eq!(cache.get(&"a".to_string()), None);
    }

    #[test]
    fn test_lru_cache_remove() {
        let mut cache = LruCache::new(3);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        cache.remove(&"a".to_string());
        assert_eq!(cache.get(&"a".to_string()), None);
        assert_eq!(cache.get(&"b".to_string()), Some(2));
        assert_eq!(cache.len(), 1);
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
        let code = "box \"Start\" fit rad 50%\narrow down\ndiamond \"OK?\" fit\narrow right 200% \"Yes\" above\nbox \"Done\" fit rad 50%\nmove to last diamond.s\narrow down \"No\" ljust\nbox \"Retry\" fit rad 10px\narrow left then up then right to 1st box.w";
        let result = PikchrRenderer::render_pikchr_to_svg(code, false);
        assert!(result.is_ok(), "Decision flow sample failed: {:?}", result.err());
    }
}
