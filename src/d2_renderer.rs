//! D2 diagram renderer.
//!
//! Renders D2 diagram markup to SVG using the embedded mdmdview-d2 crate,
//! then rasterizes SVG to egui textures via usvg/resvg. Heavy rendering
//! work (D2 compilation + SVG rasterization) runs on a dedicated worker
//! thread; the main thread only handles the cheap GPU texture upload.
//!
//! This module delegates all shared caching, worker thread management, and
//! rasterization infrastructure to `DiagramRendererCore` in `diagram_common`.
//! Only the D2-specific source-to-SVG conversion remains here.
//!
//! This module is conditionally compiled behind `#[cfg(feature = "d2")]`.
//! It integrates with `MarkdownRenderer` following the same public API pattern
//! as `PikchrRenderer` (render_block, begin_frame, has_pending, release_gpu_textures).

use crate::diagram_common::{DiagramRendererConfig, DiagramRendererCore};

// ---------------------------------------------------------------------------
// Cache capacity constants
// ---------------------------------------------------------------------------

const D2_TEXTURE_CACHE_CAPACITY: usize = 64;
const D2_SVG_CACHE_CAPACITY: usize = 64;
const D2_ERROR_CACHE_CAPACITY: usize = 32;
/// Rasterization supersample factor. D2 diagrams benefit from higher pixel
/// density for crisp text and clean lines, same as pikchr_renderer.
const D2_SUPERSAMPLE: f32 = 2.0;
/// Bounded channel capacity for job and result channels. With debounce
/// suppressing enqueue during resize, at most one job per diagram is
/// enqueued after the debounce expires. A document with ~6-8 diagrams
/// fits within 8 slots. The bounded channel provides natural backpressure
/// if the worker falls behind.
const CHANNEL_CAPACITY: usize = 8;

// ---------------------------------------------------------------------------
// D2Renderer
// ---------------------------------------------------------------------------

/// D2 diagram renderer.
///
/// Renders D2 diagram markup to SVG using the embedded mdmdview-d2 crate,
/// then rasterizes SVG to egui textures via usvg/resvg. Heavy rendering
/// runs on a dedicated worker thread; the main thread only uploads the
/// resulting RGBA data to the GPU (~0.1-1ms).
///
/// Wraps a `DiagramRendererCore` that holds all shared infrastructure.
pub(crate) struct D2Renderer {
    core: DiagramRendererCore,
}

impl D2Renderer {
    /// Create a new D2Renderer, loading system fonts once at startup.
    /// Without system fonts, usvg's default empty fontdb silently drops all
    /// text labels in rasterized SVGs. Takes ~50-200ms (one-time cost).
    /// Spawns a dedicated worker thread for background rasterization.
    pub(crate) fn new() -> Self {
        let config = DiagramRendererConfig {
            key_prefix: "d2",
            error_label: "D2",
            supersample: D2_SUPERSAMPLE,
            texture_capacity: D2_TEXTURE_CACHE_CAPACITY,
            svg_capacity: D2_SVG_CACHE_CAPACITY,
            error_capacity: D2_ERROR_CACHE_CAPACITY,
            channel_capacity: CHANNEL_CAPACITY,
            thread_name: "d2-raster",
            render_source_to_svg: Self::render_d2_to_svg,
            svg_preprocessor: None, // D2 SVGs include proper font references
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

    /// Main entry point for rendering a D2 code block.
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
    // D2-specific static methods
    // -----------------------------------------------------------------------

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

    fn d2_config() -> DiagramRendererConfig {
        DiagramRendererConfig {
            key_prefix: "d2",
            error_label: "D2",
            supersample: D2_SUPERSAMPLE,
            texture_capacity: 4,
            svg_capacity: 4,
            error_capacity: 4,
            channel_capacity: CHANNEL_CAPACITY,
            thread_name: "d2-raster-test",
            render_source_to_svg: D2Renderer::render_d2_to_svg,
            svg_preprocessor: None,
        }
    }

    fn test_renderer_with_channels(
        job_tx: Option<diagram_common::JobSender>,
        result_rx: diagram_common::ResultReceiver,
    ) -> D2Renderer {
        D2Renderer {
            core: DiagramRendererCore::new_with_channels(d2_config(), job_tx, result_rx),
        }
    }

    fn insert_test_texture(
        ui: &mut egui::Ui,
        renderer: &D2Renderer,
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
    // D2 source-to-SVG tests (specific to D2, NOT shared)
    // -----------------------------------------------------------------------

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
        let result = D2Renderer::render_d2_to_svg("}{}{", false);
        assert!(result.is_err());
    }

    #[test]
    fn test_render_empty_input() {
        let result = D2Renderer::render_d2_to_svg("", false);
        let _ = result;
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
    fn test_renderer_new_creates_empty_caches() {
        let renderer = D2Renderer::new();
        assert!(!renderer.has_pending());
    }

    #[test]
    fn test_supersample_constant_is_reasonable() {
        const { assert!(D2_SUPERSAMPLE >= 1.0, "Supersample must be >= 1.0") };
        const {
            assert!(
                D2_SUPERSAMPLE <= 4.0,
                "Supersample > 4.0 would waste memory"
            )
        };
    }

    // -----------------------------------------------------------------------
    // Async worker and channel tests (using D2-specific render function)
    // -----------------------------------------------------------------------

    fn make_fontdb() -> Arc<usvg::fontdb::Database> {
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        Arc::new(db)
    }

    #[test]
    fn test_render_block_basic_and_pending_placeholder() {
        let renderer = D2Renderer::new();
        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let code = "a -> b";
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
        let code = "a -> b";
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
        let code = "a -> b";
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
                texture_key: "d2:full:w224:s100:dm0".to_string(),
                code: Some("a -> b".to_string()),
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
                rendered = renderer.render_block(ui, "a -> b", 1.0, 14.0);
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
        let code = "a -> b";
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
        let code = "a -> b";
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
                    "d2:{:016x}:w{}:s{}:dm{}",
                    code_hash,
                    width_bucket,
                    100,
                    ui.visuals().dark_mode as u8
                );
                *texture_key.borrow_mut() = key.clone();
                renderer.core.wanted_bucket.borrow_mut().insert(code_hash, width_bucket);
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
        let code_hash = hash_str("a -> b");
        renderer.core.pending.borrow_mut().insert(code_hash);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "d2:missing-fields:w224:s100:dm0".to_string(),
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
        let code_hash = hash_str("a -> tall");
        renderer.core.pending.borrow_mut().insert(code_hash);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "d2:partial-fields:w224:s100:dm0".to_string(),
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
                texture_key: "d2:error:w224:s100:dm0".to_string(),
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
                texture_key: "d2:stale:w224:s100:dm0".to_string(),
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
                texture_key: "d2:not-a-valid-width-key".to_string(),
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
            .get(&"d2:not-a-valid-width-key".to_string())
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
        let code = "a -> b";
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
        let code = "a -> b";
        let code_hash = hash_str(code);
        let stale_key = format!("d2:{:016x}:w224:s100:dm0", code_hash);
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
        let code = "a -> b";
        let code_hash = hash_str(code);
        let stale_key = format!("d2:{:016x}:w224:s100:dm0", code_hash);
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
        let code = "a -> b";
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
        let code = "a -> b";
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
        let code = "a -> b";
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
            .insert(code_hash, "d2:missing:w224:s100:dm0".to_string());

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
        let stale_key = "d2:present:w224:s100:dm0".to_string();
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

        let output = rasterize_svg(&fontdb, svg, 0, 100, D2_SUPERSAMPLE, None).expect("rasterize");

        assert_eq!(output.display_size, [5000, 5000]);
        assert!(output.raster_size[0] <= 4096);
        assert!(output.raster_size[1] <= 4096);
    }

    #[test]
    fn test_rasterize_svg_clamps_very_large_raster_dimensions() {
        let fontdb = make_fontdb();
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 10000 8000\"><rect width=\"10000\" height=\"8000\" fill=\"red\"/></svg>";

        let output = rasterize_svg(&fontdb, svg, 0, 100, D2_SUPERSAMPLE, None).expect("rasterize");

        assert_eq!(output.display_size, [10000, 8000]);
        assert_eq!(output.raster_size[0], 4096);
        assert!(output.raster_size[1] <= 4096);
    }

    #[test]
    fn test_rasterize_svg_clamps_very_tall_raster_dimensions() {
        let fontdb = make_fontdb();
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 5000\"><rect width=\"100\" height=\"5000\" fill=\"red\"/></svg>";

        let output = rasterize_svg(&fontdb, svg, 0, 100, D2_SUPERSAMPLE, None).expect("rasterize");

        assert_eq!(output.display_size, [100, 5000]);
        assert!(output.raster_size[0] < 4096);
        assert_eq!(output.raster_size[1], 4096);
    }

    #[test]
    fn test_rasterize_svg_invalid_svg_returns_error() {
        let fontdb = make_fontdb();
        let err = match rasterize_svg(&fontdb, "<svg", 224, 100, D2_SUPERSAMPLE, None) {
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
                texture_key: format!("d2:{:016x}:w224:s100:dm0", code_hash),
                code: None,
                svg: Some("<svg".to_string()),
                dark_mode: false,
                width_bucket: 224,
                scale_bucket: 100,
            },
            D2Renderer::render_d2_to_svg,
            D2_SUPERSAMPLE,
            None,
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

        let code = "a -> b";
        let code_hash = hash_str(code);
        let texture_key = format!("d2:{:016x}:w640:s100:dm0", code_hash);
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
            D2Renderer::render_d2_to_svg,
            D2_SUPERSAMPLE,
            None,
        );

        let result = result_rx.recv_timeout(Duration::from_secs(10)).unwrap();
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

        let code = "a -> b";
        let code_hash = hash_str(code);
        let texture_key = format!("d2:{:016x}:w640:s100:dm0", code_hash);
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
            D2Renderer::render_d2_to_svg,
            D2_SUPERSAMPLE,
            None,
        );

        let result = result_rx.recv_timeout(Duration::from_secs(10)).unwrap();
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

        let code = "}{}{";
        let code_hash = hash_str(code);
        let texture_key = format!("d2:{:016x}:w640:s100:dm0", code_hash);
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
            D2Renderer::render_d2_to_svg,
            D2_SUPERSAMPLE,
            None,
        );

        let result = result_rx.recv_timeout(Duration::from_secs(10)).unwrap();
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
        let renderer = D2Renderer::new();
        let code_hash = 12345u64;

        renderer
            .core
            .wanted_bucket
            .borrow_mut()
            .insert(code_hash, 640);

        let stale_key = format!("d2:{:016x}:w1024:s100:dm0", code_hash);
        assert!(
            renderer.core.is_stale_result(&stale_key, code_hash),
            "Width 1024 vs wanted 640 should be stale"
        );

        let fresh_key = format!("d2:{:016x}:w640:s100:dm0", code_hash);
        assert!(
            !renderer.core.is_stale_result(&fresh_key, code_hash),
            "Width 640 vs wanted 640 should be fresh"
        );
    }

    #[test]
    fn test_is_stale_result_when_wanted_is_none() {
        let renderer = D2Renderer::new();
        let texture_key = format!("d2:{:016x}:w640:s100:dm0", 99999u64);
        assert!(
            renderer.core.is_stale_result(&texture_key, 99999),
            "No wanted_bucket entry should be treated as stale"
        );
    }

    #[test]
    fn test_is_stale_result_invalid_texture_key_is_assumed_fresh() {
        let renderer = D2Renderer::new();
        let code_hash = 55;
        renderer
            .core
            .wanted_bucket
            .borrow_mut()
            .insert(code_hash, 320);

        assert!(
            !renderer.core.is_stale_result("d2:malformed", code_hash),
            "Malformed texture keys should be treated as fresh"
        );
    }

    #[test]
    fn test_pending_set_basics() {
        let renderer = D2Renderer::new();

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
        let renderer = D2Renderer::new();

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
            .insert(1, "d2:key".to_string());

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
                texture_key: format!("d2:dummy:w640:s100:dm0:{}", i),
                code: Some("a -> b".to_string()),
                svg: None,
                dark_mode: false,
                width_bucket: 640,
                scale_bucket: 100,
            })
            .unwrap();
        }

        let result = tx.try_send(RasterJob {
            code_hash: 99,
            texture_key: "d2:dummy:w640:s100:dm0:99".to_string(),
            code: Some("a -> b".to_string()),
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
            let renderer = D2Renderer::new();
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
        let renderer = D2Renderer::new();
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

        let code = "a -> b";
        let code_hash = hash_str(code);
        job_tx
            .send(RasterJob {
                code_hash,
                texture_key: format!("d2:{:016x}:w640:s100:dm0", code_hash),
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
                D2Renderer::render_d2_to_svg,
                D2_SUPERSAMPLE,
                None,
            );
        });

        let start = Instant::now();
        loop {
            if handle.is_finished() {
                break;
            }
            if start.elapsed() > Duration::from_secs(10) {
                panic!("worker_loop blocked for >10s — try_send likely blocked instead of dropping the result");
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
                texture_key: format!("d2:{:016x}:w640:s100:dm0", code_hash),
                code: None,
                svg: Some(valid_svg.to_string()),
                dark_mode: false,
                width_bucket: 640,
                scale_bucket: 100,
            },
            D2Renderer::render_d2_to_svg,
            D2_SUPERSAMPLE,
            None,
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
}
