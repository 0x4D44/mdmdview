//! D2 diagram renderer.
//!
//! Renders D2 diagram markup to SVG using the embedded mdmdview-d2 crate,
//! then rasterizes SVG to egui textures via usvg/resvg. Heavy rendering
//! work (D2 compilation + SVG rasterization) runs on a dedicated worker
//! thread; the main thread only handles the cheap GPU texture upload.
//!
//! Key components:
//! - `D2Renderer` struct: holds font database, caches, and worker channels
//! - `LruCache<K, V>`: generic LRU cache (shared module in lru_cache.rs)
//! - `render_d2_to_svg()`: static method calling into the mdmdview-d2 crate
//! - `rasterize_svg()`: pure function: SVG string -> RGBA bytes (runs on worker)
//! - `poll_results()`: drains worker results, uploads textures to GPU (main thread)
//!
//! This module is conditionally compiled behind `#[cfg(feature = "d2")]`.
//! It integrates with `MarkdownRenderer` following the same public API pattern
//! as `PikchrRenderer` (render_block, begin_frame, has_pending, release_gpu_textures).

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};

use crossbeam_channel::{Receiver, Sender, TrySendError};

use crate::lru_cache::{hash_str, LruCache};
use crate::ThemeColors;

// ---------------------------------------------------------------------------
// Cache capacity constants
// ---------------------------------------------------------------------------

const D2_TEXTURE_CACHE_CAPACITY: usize = 64;
const D2_SVG_CACHE_CAPACITY: usize = 64;
const D2_ERROR_CACHE_CAPACITY: usize = 32;
/// Trailing-edge debounce cooldown for resize. During window resize, suppress
/// rasterization enqueue and show the last rendered texture scaled to fit.
/// Only enqueue a new rasterization job once the width stabilizes for this
/// duration.
const RESIZE_DEBOUNCE_MS: u64 = 150;
/// Maximum rasterized dimension (width or height) in pixels.
/// Matches the cap used by pikchr_renderer and mermaid_renderer.
const MAX_RASTER_SIDE: u32 = 4096;
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
// Channel type aliases
// ---------------------------------------------------------------------------

type JobSender = Sender<RasterJob>;
type JobReceiver = Receiver<RasterJob>;
type ResultSender = Sender<RasterResult>;
type ResultReceiver = Receiver<RasterResult>;

// ---------------------------------------------------------------------------
// RasterJob / RasterResult
// ---------------------------------------------------------------------------

/// Job sent from main thread to worker.
struct RasterJob {
    /// Hash of the source code (for cache keying).
    code_hash: u64,
    /// Composite texture cache key (includes width, scale, theme).
    texture_key: String,
    /// Source code to render. None if SVG is provided directly.
    code: Option<String>,
    /// Pre-cached SVG string. If present, skip source->SVG step.
    svg: Option<String>,
    /// Dark mode flag (affects D2 source->SVG output).
    dark_mode: bool,
    /// Width bucket (32px quantized) for rasterization scaling.
    width_bucket: u32,
    /// Scale bucket (integer %, e.g., 100 = 1.0x zoom).
    scale_bucket: u32,
}

/// Result sent from worker back to main thread.
struct RasterResult {
    /// Hash of the source code (for cache keying).
    code_hash: u64,
    /// Composite texture cache key.
    texture_key: String,
    /// Dark mode flag (copied from job, for SVG cache key computation).
    dark_mode: bool,
    /// SVG string (always set on success, for caching).
    svg: Option<String>,
    /// RGBA pixel data from rasterization.
    rgba: Option<Vec<u8>>,
    /// Raster dimensions in actual pixels (for GPU upload).
    raster_size: Option<[usize; 2]>,
    /// Display dimensions in logical pixels (for UI layout).
    display_size: Option<[u32; 2]>,
    /// Error message (set on failure, mutually exclusive with rgba).
    error: Option<String>,
}

/// Intermediate output from `rasterize_svg`, before GPU upload.
struct RasterOutput {
    rgba: Vec<u8>,
    raster_size: [usize; 2],
    display_size: [u32; 2],
}

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

/// Per-diagram resize debounce state.
struct DiagramDebounce {
    /// Width bucket at which we last rasterized (and cached a texture).
    last_rasterized_bucket: u32,
    /// Width bucket seen on the most recent frame (for trailing-edge reset).
    last_seen_bucket: u32,
    /// When `last_seen_bucket` last changed. `None` when no resize is active.
    bucket_changed_at: Option<Instant>,
}

/// D2 diagram renderer.
///
/// Renders D2 diagram markup to SVG using the embedded mdmdview-d2 crate,
/// then rasterizes SVG to egui textures via usvg/resvg. Heavy rendering
/// runs on a dedicated worker thread; the main thread only uploads the
/// resulting RGBA data to the GPU (~0.1-1ms).
pub(crate) struct D2Renderer {
    /// LRU cache: texture_key (String) -> (TextureHandle, [width, height])
    textures: RefCell<LruCache<String, D2TextureEntry>>,
    /// SVG cache: svg_cache_key (u128: code_hash << 1 | dark_mode) -> SVG string
    svg_cache: RefCell<LruCache<u128, String>>,
    /// Error cache: code_hash (u64) -> error message.
    /// Errors don't depend on dark_mode, only on source syntax.
    errors: RefCell<LruCache<u64, String>>,
    /// Per-diagram debounce state for resize suppression.
    debounce: RefCell<HashMap<u64, DiagramDebounce>>,
    /// Maps code_hash -> texture_key of the most recently rasterized texture.
    /// Used to display a stale texture scaled to fit during debounce cooldown.
    latest_texture_key: RefCell<HashMap<u64, String>>,
    /// Job sender. Wrapped in Option so Drop can close the channel
    /// before joining the worker thread (same pattern as Pikchr/Mermaid).
    job_tx: Option<JobSender>,
    /// Result receiver. Polled each frame in poll_results().
    result_rx: ResultReceiver,
    /// Worker thread handle. Joined on drop for clean shutdown.
    worker_handle: Option<std::thread::JoinHandle<()>>,
    /// Set of code_hashes with in-flight jobs. Keyed by code_hash (not
    /// texture_key) to prevent redundant enqueues when the width changes
    /// while a job is already pending for the same diagram.
    pending: RefCell<HashSet<u64>>,
    /// Tracks the currently desired width bucket per diagram (for stale discard).
    wanted_bucket: RefCell<HashMap<u64, u32>>,
}

impl D2Renderer {
    /// Create a new D2Renderer, loading system fonts once at startup.
    /// Without system fonts, usvg's default empty fontdb silently drops all
    /// text labels in rasterized SVGs. Takes ~50-200ms (one-time cost).
    /// Spawns a dedicated worker thread for background rasterization.
    /// Same pattern as pikchr_renderer.rs and mermaid_renderer.rs.
    pub(crate) fn new() -> Self {
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        let fontdb = Arc::new(db);

        let (job_tx, job_rx) = crossbeam_channel::bounded(CHANNEL_CAPACITY);
        let (result_tx, result_rx) = crossbeam_channel::bounded(CHANNEL_CAPACITY);

        // Clone fontdb for the worker thread
        let worker_fontdb = Arc::clone(&fontdb);
        let worker_handle = std::thread::Builder::new()
            .name("d2-raster".into())
            .spawn(move || {
                Self::worker_loop(worker_fontdb, job_rx, result_tx);
            })
            .expect("failed to spawn d2 raster worker");

        Self {
            textures: RefCell::new(LruCache::new(D2_TEXTURE_CACHE_CAPACITY)),
            svg_cache: RefCell::new(LruCache::new(D2_SVG_CACHE_CAPACITY)),
            errors: RefCell::new(LruCache::new(D2_ERROR_CACHE_CAPACITY)),
            debounce: RefCell::new(HashMap::new()),
            latest_texture_key: RefCell::new(HashMap::new()),
            job_tx: Some(job_tx),
            result_rx,
            worker_handle: Some(worker_handle),
            pending: RefCell::new(HashSet::new()),
            wanted_bucket: RefCell::new(HashMap::new()),
        }
    }

    // -----------------------------------------------------------------------
    // Worker thread
    // -----------------------------------------------------------------------

    /// Main loop for the worker thread. Receives jobs, processes them, and
    /// sends results back. Uses `try_send` to avoid blocking if the result
    /// channel is full (prevents shutdown deadlock). Same pattern as Pikchr
    /// and Mermaid renderers.
    fn worker_loop(
        fontdb: Arc<usvg::fontdb::Database>,
        job_rx: JobReceiver,
        result_tx: ResultSender,
    ) {
        while let Ok(job) = job_rx.recv() {
            let result = Self::process_job(&fontdb, job);
            // Use try_send to avoid blocking if result channel is full.
            // This prevents deadlock during shutdown: if Drop joins workers
            // while they're blocked on send (because result_rx isn't being
            // polled), we'd hang forever. Dropping results is acceptable --
            // the main thread still has the code_hash in the pending set, so
            // render_block will show the stale texture or placeholder and the
            // pending entry will be leaked (harmless, cleared on next
            // release_gpu_textures or app restart).
            let _ = result_tx.try_send(result);
        }
        // job_rx disconnected (main thread dropped job_tx) -- exit
    }

    /// Process a single rasterization job: source->SVG (if needed), then
    /// SVG->RGBA. Returns a result with either RGBA data or an error.
    fn process_job(fontdb: &Arc<usvg::fontdb::Database>, job: RasterJob) -> RasterResult {
        // Step 1: Get SVG (from cache or render fresh)
        let svg = match job.svg {
            Some(cached_svg) => Ok(cached_svg),
            None => {
                let code = job.code.as_deref().unwrap_or("");
                Self::render_d2_to_svg(code, job.dark_mode)
            }
        };

        let svg = match svg {
            Ok(svg) => svg,
            Err(err) => {
                return RasterResult {
                    code_hash: job.code_hash,
                    texture_key: job.texture_key,
                    dark_mode: job.dark_mode,
                    svg: None,
                    rgba: None,
                    raster_size: None,
                    display_size: None,
                    error: Some(err),
                };
            }
        };

        // Step 2: Rasterize SVG to RGBA
        match Self::rasterize_svg(fontdb, &svg, job.width_bucket, job.scale_bucket) {
            Ok(output) => RasterResult {
                code_hash: job.code_hash,
                texture_key: job.texture_key,
                dark_mode: job.dark_mode,
                svg: Some(svg),
                rgba: Some(output.rgba),
                raster_size: Some(output.raster_size),
                display_size: Some(output.display_size),
                error: None,
            },
            Err(err) => RasterResult {
                code_hash: job.code_hash,
                texture_key: job.texture_key,
                dark_mode: job.dark_mode,
                svg: Some(svg), // SVG succeeded -- cache it even if rasterize failed
                rgba: None,
                raster_size: None,
                display_size: None,
                error: Some(err),
            },
        }
    }

    // -----------------------------------------------------------------------
    // Rasterization (runs on worker thread)
    // -----------------------------------------------------------------------

    /// Rasterize an SVG string to RGBA pixels. Pure function -- no GPU access,
    /// no egui context needed. Runs on the worker thread.
    ///
    /// Parses via usvg (using the shared fontdb with system fonts), and
    /// rasterizes via resvg/tiny-skia. D2 SVGs include proper font references,
    /// so no font-family injection is needed (unlike Pikchr).
    /// Rasterizes at `D2_SUPERSAMPLE` x the logical display resolution
    /// for crisp text and lines. Returns display dimensions in logical pixels
    /// and raster dimensions in actual pixels.
    fn rasterize_svg(
        fontdb: &Arc<usvg::fontdb::Database>,
        svg: &str,
        width_bucket: u32,
        scale_bucket: u32,
    ) -> Result<RasterOutput, String> {
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

        Ok(RasterOutput {
            rgba: pixmap.data().to_vec(),
            raster_size: [raster_w as usize, raster_h as usize],
            display_size: [display_w, display_h],
        })
    }

    // -----------------------------------------------------------------------
    // Main-thread polling
    // -----------------------------------------------------------------------

    /// Drain completed results from the worker thread. Called once per frame
    /// from `begin_frame`, NOT from `render_block`. This is critical:
    /// `render_block` only runs for visible diagrams, so if diagrams scroll
    /// off-screen while jobs are in-flight, results would pile up in the
    /// bounded channel and eventually block the worker thread (deadlock).
    /// Polling from `begin_frame` ensures results are always drained.
    fn poll_results(&self, ctx: &egui::Context) -> bool {
        let mut any_processed = false;

        loop {
            match self.result_rx.try_recv() {
                Ok(result) => {
                    any_processed = true;

                    // Remove from pending set
                    self.pending.borrow_mut().remove(&result.code_hash);

                    // Always cache SVG (content-addressed, width-independent)
                    if let Some(ref svg) = result.svg {
                        let sk = svg_cache_key(result.code_hash, result.dark_mode);
                        self.svg_cache.borrow_mut().insert(sk, svg.clone());
                    }

                    // Always cache errors (prevents infinite retry loops)
                    if let Some(ref err) = result.error {
                        self.errors
                            .borrow_mut()
                            .insert(result.code_hash, err.clone());
                        continue; // No RGBA to upload
                    }

                    // Stale result check: skip GPU upload if width bucket changed since enqueue
                    if self.is_stale_result(&result.texture_key, result.code_hash) {
                        continue;
                    }

                    // Upload RGBA to GPU texture
                    if let (Some(rgba), Some(raster_size), Some(display_size)) =
                        (result.rgba, result.raster_size, result.display_size)
                    {
                        let image =
                            egui::ColorImage::from_rgba_unmultiplied(raster_size, &rgba);
                        let texture = ctx.load_texture(
                            result.texture_key.clone(),
                            image,
                            egui::TextureOptions::LINEAR,
                        );
                        let entry = D2TextureEntry {
                            texture,
                            display_size,
                        };
                        self.textures
                            .borrow_mut()
                            .insert(result.texture_key.clone(), entry);
                        self.latest_texture_key
                            .borrow_mut()
                            .insert(result.code_hash, result.texture_key.clone());
                        // Update debounce state
                        if let Some(width_bucket) =
                            Self::parse_width_from_texture_key(&result.texture_key)
                        {
                            self.debounce.borrow_mut().insert(
                                result.code_hash,
                                DiagramDebounce {
                                    last_rasterized_bucket: width_bucket,
                                    last_seen_bucket: width_bucket,
                                    bucket_changed_at: None,
                                },
                            );
                        }
                    }
                }
                Err(crossbeam_channel::TryRecvError::Empty) => break,
                Err(crossbeam_channel::TryRecvError::Disconnected) => {
                    // Worker thread has died (panic or unexpected exit).
                    // Clear all pending entries so render_block can detect the dead
                    // worker via try_send(Disconnected) and show an error instead of
                    // showing "Rendering..." forever.
                    self.pending.borrow_mut().clear();
                    break;
                }
            }
        }

        if any_processed {
            ctx.request_repaint();
        }
        any_processed
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /// Called once at the start of each egui frame. Drains completed results
    /// from the worker thread and requests repaint if jobs are still pending.
    pub(crate) fn begin_frame(&self, ctx: &egui::Context) {
        // Drain completed results from worker thread. Must happen here (not in
        // render_block) because render_block only runs for visible diagrams --
        // off-screen results would pile up and block the worker.
        self.poll_results(ctx);

        // If jobs are still in-flight, ensure we get another frame to poll again.
        if self.has_pending() {
            ctx.request_repaint_after(Duration::from_millis(16));
        }
    }

    /// Returns true if any rasterization jobs are in-flight on the worker thread.
    pub(crate) fn has_pending(&self) -> bool {
        !self.pending.borrow().is_empty()
    }

    /// Release GPU textures to reduce idle GPU usage (e.g., when minimized).
    /// SVG and error caches are retained for fast texture rebuilds on next render.
    /// Also clears pending, wanted_bucket, and debounce to allow clean re-enqueue
    /// on restore. In-flight results will be detected as stale (wanted_bucket is
    /// empty) and discarded in poll_results.
    pub(crate) fn release_gpu_textures(&self) {
        self.textures.borrow_mut().clear();
        self.debounce.borrow_mut().clear();
        self.latest_texture_key.borrow_mut().clear();
        self.wanted_bucket.borrow_mut().clear();
        self.pending.borrow_mut().clear();
    }

    /// Main entry point for rendering a D2 code block.
    ///
    /// Checks caches (error, texture, SVG) before enqueuing. On cache miss,
    /// enqueues a job to the worker thread and shows a stale texture or
    /// placeholder while waiting.
    ///
    /// Returns `true` when the block has produced UI output (always true).
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

        // 2b. Update wanted_bucket (for stale discard in poll_results)
        self.wanted_bucket
            .borrow_mut()
            .insert(code_hash, width_bucket);

        // 3. Check texture cache.
        {
            let mut cache = self.textures.borrow_mut();
            if let Some(entry) = cache.get(&texture_key) {
                Self::render_texture(ui, &entry, available_width);
                return true;
            }
        }

        // 3b. Debounce gate: suppress rasterization enqueue during resize.
        //     Show the last rendered texture scaled to fit. Only enqueue
        //     once the width stabilizes for RESIZE_DEBOUNCE_MS.
        let db_entry = self
            .debounce
            .borrow()
            .get(&code_hash)
            .map(|d| (d.last_rasterized_bucket, d.last_seen_bucket, d.bucket_changed_at));
        if let Some((last_rasterized, last_seen, _changed_at)) = db_entry {
            if width_bucket != last_rasterized {
                // Width changed since last rasterize -- apply trailing-edge debounce
                if width_bucket != last_seen {
                    // New bucket since last frame -- reset timer
                    if let Some(d) = self.debounce.borrow_mut().get_mut(&code_hash) {
                        d.last_seen_bucket = width_bucket;
                        d.bucket_changed_at = Some(Instant::now());
                    }
                }
                let changed_at = self
                    .debounce
                    .borrow()
                    .get(&code_hash)
                    .and_then(|d| d.bucket_changed_at)
                    .unwrap_or_else(Instant::now);
                let elapsed = changed_at.elapsed();
                let debounce_dur = Duration::from_millis(RESIZE_DEBOUNCE_MS);
                if elapsed < debounce_dur {
                    // Still in cooldown -- show stale texture scaled to fit
                    if let Some(stale_key) =
                        self.latest_texture_key.borrow().get(&code_hash).cloned()
                    {
                        if let Some(entry) = self.textures.borrow_mut().get(&stale_key) {
                            Self::render_texture(ui, &entry, available_width);
                            ui.ctx().request_repaint_after(debounce_dur - elapsed);
                            return true;
                        }
                    }
                    // No stale texture available (first render) -- fall through to enqueue
                }
                // Debounce expired -- fall through to enqueue
            }
            // width == last_rasterized: texture was evicted -- fall through to enqueue
        }
        // No debounce entry: first render -- fall through to enqueue

        // 4. Check if already pending for this diagram (by code_hash, not texture_key,
        //    to prevent redundant enqueues when the width changes mid-flight)
        if self.pending.borrow().contains(&code_hash) {
            return self.show_stale_or_placeholder(ui, code_hash, available_width);
        }

        // 5. Enqueue job
        let cached_svg = self.svg_cache.borrow_mut().get(&svg_key);
        let job = RasterJob {
            code_hash,
            texture_key: texture_key.clone(),
            code: if cached_svg.is_none() {
                Some(code.to_string())
            } else {
                None
            },
            svg: cached_svg,
            dark_mode,
            width_bucket,
            scale_bucket,
        };

        match self.job_tx.as_ref().unwrap().try_send(job) {
            Ok(()) => {
                self.pending.borrow_mut().insert(code_hash);
            }
            Err(TrySendError::Full(_)) => {
                // Queue full -- will retry next frame
            }
            Err(TrySendError::Disconnected(_)) => {
                // Worker died -- show error
                self.errors
                    .borrow_mut()
                    .insert(code_hash, "Raster worker unavailable".to_string());
                self.render_error_block(ui, "Raster worker unavailable", code, code_font_size);
                return true;
            }
        }

        // 6. Show stale texture or placeholder while waiting
        self.show_stale_or_placeholder(ui, code_hash, available_width)
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    /// Show a stale texture (from a previous width) scaled to fit, or a
    /// minimal-height placeholder if no stale texture is available.
    /// Returns true (block always produces UI output).
    fn show_stale_or_placeholder(
        &self,
        ui: &mut egui::Ui,
        code_hash: u64,
        available_width: f32,
    ) -> bool {
        // Try stale texture first (gives a good visual approximation)
        if let Some(stale_key) = self.latest_texture_key.borrow().get(&code_hash).cloned() {
            if let Some(entry) = self.textures.borrow_mut().get(&stale_key) {
                Self::render_texture(ui, &entry, available_width);
                return true;
            }
        }
        // No stale texture -- show minimal placeholder (first render).
        // Use a small fixed height to reduce layout shift when the real texture arrives.
        let placeholder_height = 60.0;
        let (rect, _) = ui.allocate_exact_size(
            egui::vec2(available_width, placeholder_height),
            egui::Sense::hover(),
        );
        ui.painter().text(
            rect.center(),
            egui::Align2::CENTER_CENTER,
            "Rendering...",
            egui::FontId::proportional(12.0),
            ui.visuals().weak_text_color(),
        );
        true
    }

    /// Parse the width bucket value from a texture cache key.
    /// Key format: `d2:<hex>:w<width>:s<scale>:dm<0|1>`
    fn parse_width_from_texture_key(key: &str) -> Option<u32> {
        let w_start = key.find(":w")? + 2;
        let w_end = key[w_start..].find(':')? + w_start;
        key[w_start..w_end].parse().ok()
    }

    /// Check whether a result's width bucket no longer matches what the main
    /// thread currently wants for this diagram. Stale results have their SVG
    /// cached but skip the expensive GPU upload.
    fn is_stale_result(&self, texture_key: &str, code_hash: u64) -> bool {
        let result_bucket = Self::parse_width_from_texture_key(texture_key);
        let wanted = self.wanted_bucket.borrow().get(&code_hash).copied();
        match (result_bucket, wanted) {
            (Some(rb), Some(wb)) => rb != wb, // Width mismatch -> stale
            (Some(_), None) => true,          // No wanted bucket (e.g. after
            //                                   release_gpu_textures) -> stale
            _ => false, // Can't parse -> assume fresh
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

impl Drop for D2Renderer {
    fn drop(&mut self) {
        // Close job channel -- worker will see Disconnected on recv() and exit.
        // The worker uses try_send for results (never blocks), so it will
        // drain remaining jobs and exit promptly once job_rx disconnects.
        self.job_tx.take();
        // Join worker thread for clean shutdown
        if let Some(handle) = self.worker_handle.take() {
            let _ = handle.join();
        }
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

    // -----------------------------------------------------------------------
    // Stale result and texture key parsing tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_parse_width_from_texture_key() {
        assert_eq!(
            D2Renderer::parse_width_from_texture_key("d2:0000000000003039:w640:s100:dm0"),
            Some(640)
        );
        assert_eq!(
            D2Renderer::parse_width_from_texture_key("d2:abc:w1024:s150:dm1"),
            Some(1024)
        );
        assert_eq!(
            D2Renderer::parse_width_from_texture_key("d2:abc:w0:s100:dm0"),
            Some(0)
        );
        // No :w prefix
        assert_eq!(
            D2Renderer::parse_width_from_texture_key("d2:abc:s100:dm0"),
            None
        );
    }

    // -----------------------------------------------------------------------
    // Async worker and channel tests
    // -----------------------------------------------------------------------

    /// Helper: create a fontdb with system fonts loaded (shared across worker tests).
    fn make_fontdb() -> Arc<usvg::fontdb::Database> {
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        Arc::new(db)
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
        // Drop sender so worker_loop exits after processing the one job.
        drop(job_tx);

        D2Renderer::worker_loop(fontdb, job_rx, result_tx);

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
        assert!(
            result.svg.is_some(),
            "Worker should produce SVG on success"
        );
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

        D2Renderer::worker_loop(fontdb, job_rx, result_tx);

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

        D2Renderer::worker_loop(fontdb, job_rx, result_tx);

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

        // Insert wanted_bucket: code_hash -> width 640
        renderer.wanted_bucket.borrow_mut().insert(code_hash, 640);

        // Texture key with w1024 should be stale (mismatch)
        let stale_key = format!("d2:{:016x}:w1024:s100:dm0", code_hash);
        assert!(
            renderer.is_stale_result(&stale_key, code_hash),
            "Width 1024 vs wanted 640 should be stale"
        );

        // Texture key with w640 should be fresh (match)
        let fresh_key = format!("d2:{:016x}:w640:s100:dm0", code_hash);
        assert!(
            !renderer.is_stale_result(&fresh_key, code_hash),
            "Width 640 vs wanted 640 should be fresh"
        );
    }

    #[test]
    fn test_is_stale_result_when_wanted_is_none() {
        let renderer = D2Renderer::new();
        // Do NOT insert any wanted_bucket entry.
        let texture_key = format!("d2:{:016x}:w640:s100:dm0", 99999u64);
        assert!(
            renderer.is_stale_result(&texture_key, 99999),
            "No wanted_bucket entry should be treated as stale"
        );
    }

    #[test]
    fn test_pending_set_basics() {
        let renderer = D2Renderer::new();

        assert!(
            !renderer.has_pending(),
            "Pending should be empty on new renderer"
        );

        renderer.pending.borrow_mut().insert(42);
        assert!(
            renderer.has_pending(),
            "Pending should be non-empty after insert"
        );

        renderer.pending.borrow_mut().remove(&42);
        assert!(
            !renderer.has_pending(),
            "Pending should be empty after remove"
        );
    }

    #[test]
    fn test_release_gpu_clears_pending_and_wanted() {
        let renderer = D2Renderer::new();

        // Populate all four collections
        renderer.pending.borrow_mut().insert(1);
        renderer.wanted_bucket.borrow_mut().insert(1, 640);
        renderer.debounce.borrow_mut().insert(
            1,
            DiagramDebounce {
                last_rasterized_bucket: 640,
                last_seen_bucket: 640,
                bucket_changed_at: None,
            },
        );
        renderer
            .latest_texture_key
            .borrow_mut()
            .insert(1, "d2:key".to_string());

        renderer.release_gpu_textures();

        assert!(
            renderer.pending.borrow().is_empty(),
            "pending should be empty after release_gpu_textures"
        );
        assert!(
            renderer.wanted_bucket.borrow().is_empty(),
            "wanted_bucket should be empty after release_gpu_textures"
        );
        assert!(
            renderer.debounce.borrow().is_empty(),
            "debounce should be empty after release_gpu_textures"
        );
        assert!(
            renderer.latest_texture_key.borrow().is_empty(),
            "latest_texture_key should be empty after release_gpu_textures"
        );
    }

    #[test]
    fn test_queue_full_does_not_block() {
        let (tx, _rx) = crossbeam_channel::bounded::<RasterJob>(2);

        // Fill the channel with 2 dummy jobs
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

        // Third send should fail with Full, not block
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
            matches!(result, Err(TrySendError::Full(_))),
            "try_send on full channel should return Err(Full), got {:?}",
            result.err()
        );
    }

    #[test]
    fn test_shutdown_joins_worker() {
        let start = Instant::now();
        {
            let renderer = D2Renderer::new();
            // Drop renderer — should close channels and join worker
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

        renderer.pending.borrow_mut().insert(code_hash);

        // The dedup check in render_block uses contains()
        assert!(
            renderer.pending.borrow().contains(&code_hash),
            "Pending set should contain the inserted code_hash"
        );

        // Inserting again is a no-op on HashSet
        renderer.pending.borrow_mut().insert(code_hash);
        assert_eq!(
            renderer.pending.borrow().len(),
            1,
            "HashSet dedup: inserting same hash twice should keep len at 1"
        );
    }

    #[test]
    fn test_worker_try_send_full_does_not_block() {
        let fontdb = make_fontdb();
        let (job_tx, job_rx) = crossbeam_channel::bounded::<RasterJob>(4);
        // Result channel with capacity 1
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(1);

        // Fill the result channel with a dummy result
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

        // Submit a job that will produce a result the worker can't send (channel full)
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
        // Drop job_tx so worker_loop exits after processing the one job
        drop(job_tx);

        // Run worker_loop on a spawned thread with a timeout to detect hangs
        let handle = std::thread::spawn(move || {
            D2Renderer::worker_loop(fontdb, job_rx, result_tx);
        });

        // Wait up to 10 seconds for the worker to finish (D2 can be slower than pikchr)
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

        // The result channel should still contain the original dummy result
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
        let job = RasterJob {
            code_hash,
            texture_key: format!("d2:{:016x}:w640:s100:dm0", code_hash),
            code: None, // No source code — using cached SVG
            svg: Some(valid_svg.to_string()),
            dark_mode: false,
            width_bucket: 640,
            scale_bucket: 100,
        };

        let result = D2Renderer::process_job(&fontdb, job);
        assert!(
            result.rgba.is_some(),
            "process_job with cached SVG should produce RGBA data"
        );
        assert!(
            result.raster_size.is_some(),
            "process_job with cached SVG should produce raster_size"
        );
        assert!(
            result.display_size.is_some(),
            "process_job with cached SVG should produce display_size"
        );
        assert!(
            result.error.is_none(),
            "process_job with cached SVG should not produce an error"
        );
    }

    #[test]
    fn test_svg_cache_key_consistency() {
        let code_hash = 12345u64;

        // Same inputs should produce the same key across multiple calls
        let key_a = svg_cache_key(code_hash, false);
        let key_b = svg_cache_key(code_hash, false);
        assert_eq!(key_a, key_b, "svg_cache_key should be deterministic");

        let key_dark_a = svg_cache_key(code_hash, true);
        let key_dark_b = svg_cache_key(code_hash, true);
        assert_eq!(
            key_dark_a, key_dark_b,
            "svg_cache_key (dark) should be deterministic"
        );

        // Different dark_mode should produce different keys
        assert_ne!(
            key_a, key_dark_a,
            "Different dark_mode should produce different keys"
        );
    }
}
