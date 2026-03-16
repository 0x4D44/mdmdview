//! Shared diagram renderer infrastructure.
//!
//! Extracts the common data structures, caching logic, worker thread
//! management, and SVG rasterization pipeline shared by `pikchr_renderer`
//! and `d2_renderer`. Each concrete renderer wraps a `DiagramRendererCore`
//! that owns the caches, channels, and worker thread, parameterised by a
//! `DiagramRendererConfig` that captures the per-renderer differences
//! (key prefix, error label, supersample factor, cache capacities, thread
//! name, and function pointers for source-to-SVG and SVG preprocessing).
//!
//! This module is compiled unconditionally (no feature gates) and relies
//! on dead-code elimination for unused paths.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};

use crossbeam_channel::{Receiver, Sender, TrySendError};

use crate::lru_cache::{hash_str, LruCache};
use crate::ThemeColors;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Maximum rasterized dimension (width or height) in pixels.
/// Shared cap across all diagram renderers; matches mermaid_renderer.
const MAX_RASTER_SIDE: u32 = 4096;

/// Trailing-edge debounce cooldown for resize (milliseconds).  During
/// window resize, suppress rasterization enqueue and show the last
/// rendered texture scaled to fit.  Only enqueue a new rasterization job
/// once the width stabilises for this duration.
const RESIZE_DEBOUNCE_MS: u64 = 150;

// ---------------------------------------------------------------------------
// Channel type aliases
// ---------------------------------------------------------------------------

pub(crate) type JobSender = Sender<RasterJob>;
pub(crate) type JobReceiver = Receiver<RasterJob>;
pub(crate) type ResultSender = Sender<RasterResult>;
pub(crate) type ResultReceiver = Receiver<RasterResult>;

// ---------------------------------------------------------------------------
// RasterJob / RasterResult / RasterOutput
// ---------------------------------------------------------------------------

/// Job sent from main thread to worker.
pub(crate) struct RasterJob {
    /// Hash of the source code (for cache keying).
    pub(crate) code_hash: u64,
    /// Composite texture cache key (includes width, scale, theme).
    pub(crate) texture_key: String,
    /// Source code to render. None if SVG is provided directly.
    pub(crate) code: Option<String>,
    /// Pre-cached SVG string. If present, skip source->SVG step.
    pub(crate) svg: Option<String>,
    /// Dark mode flag (affects source->SVG for some renderers).
    pub(crate) dark_mode: bool,
    /// Width bucket (32px quantised) for rasterization scaling.
    pub(crate) width_bucket: u32,
    /// Scale bucket (integer %, e.g., 100 = 1.0x zoom).
    pub(crate) scale_bucket: u32,
}

/// Result sent from worker back to main thread.
pub(crate) struct RasterResult {
    /// Hash of the source code (for cache keying).
    pub(crate) code_hash: u64,
    /// Composite texture cache key.
    pub(crate) texture_key: String,
    /// Dark mode flag (copied from job, for SVG cache key computation).
    pub(crate) dark_mode: bool,
    /// SVG string (always set on success, for caching).
    pub(crate) svg: Option<String>,
    /// RGBA pixel data from rasterization.
    pub(crate) rgba: Option<Vec<u8>>,
    /// Raster dimensions in actual pixels (for GPU upload).
    pub(crate) raster_size: Option<[usize; 2]>,
    /// Display dimensions in logical pixels (for UI layout).
    pub(crate) display_size: Option<[u32; 2]>,
    /// Error message (set on failure, mutually exclusive with rgba).
    pub(crate) error: Option<String>,
}

/// Intermediate output from `rasterize_svg`, before GPU upload.
pub(crate) struct RasterOutput {
    pub(crate) rgba: Vec<u8>,
    pub(crate) raster_size: [usize; 2],
    pub(crate) display_size: [u32; 2],
}

// ---------------------------------------------------------------------------
// DiagramTextureEntry
// ---------------------------------------------------------------------------

/// A cached rasterized diagram texture with its display dimensions.
#[derive(Clone)]
pub(crate) struct DiagramTextureEntry {
    /// GPU texture handle (Clone wraps Arc internally).
    pub(crate) texture: egui::TextureHandle,
    /// Display dimensions in logical pixels [width, height].
    /// The actual texture is larger by the supersample factor for crisp rendering.
    pub(crate) display_size: [u32; 2],
}

// ---------------------------------------------------------------------------
// DiagramDebounce
// ---------------------------------------------------------------------------

/// Per-diagram resize debounce state.
pub(crate) struct DiagramDebounce {
    /// Width bucket at which we last rasterized (and cached a texture).
    pub(crate) last_rasterized_bucket: u32,
    /// Width bucket seen on the most recent frame (for trailing-edge reset).
    pub(crate) last_seen_bucket: u32,
    /// When `last_seen_bucket` last changed.  `None` when no resize is active.
    pub(crate) bucket_changed_at: Option<Instant>,
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Composite SVG cache key: embeds both the source code hash and dark_mode flag.
/// Dark mode produces different SVG output for the same source code.
pub(crate) fn svg_cache_key(code_hash: u64, dark_mode: bool) -> u128 {
    ((code_hash as u128) << 1) | (dark_mode as u128)
}

/// Quantise a width value to 32-pixel buckets to avoid cache thrashing
/// when the panel width changes by a few pixels (e.g., during resize).
/// Returns the bucket midpoint as u32.
pub(crate) fn bucket_width(width: f32) -> u32 {
    let bucket = (width / 32.0).round() as u32;
    bucket * 32
}

/// Quantise a UI scale factor to integer percentage buckets.
/// e.g., 1.0 -> 100, 1.25 -> 125, 1.5 -> 150.
pub(crate) fn bucket_scale(scale: f32) -> u32 {
    (scale * 100.0).round() as u32
}

// ---------------------------------------------------------------------------
// DiagramRendererConfig
// ---------------------------------------------------------------------------

/// Parameterises the differences between diagram renderers.
///
/// Each concrete renderer (Pikchr, D2, ...) creates a config at construction
/// time and passes it to `DiagramRendererCore::new()`.
pub(crate) struct DiagramRendererConfig {
    /// Prefix used in texture cache keys, e.g. `"pikchr"` or `"d2"`.
    pub(crate) key_prefix: &'static str,
    /// Human-readable label for error messages, e.g. `"Pikchr"` or `"D2"`.
    pub(crate) error_label: &'static str,
    /// Rasterization supersample factor (typically 2.0).
    pub(crate) supersample: f32,
    /// LRU capacity for the texture cache.
    pub(crate) texture_capacity: usize,
    /// LRU capacity for the SVG cache.
    pub(crate) svg_capacity: usize,
    /// LRU capacity for the error cache.
    pub(crate) error_capacity: usize,
    /// Bounded channel capacity for job and result channels.
    pub(crate) channel_capacity: usize,
    /// Thread name for the worker thread (e.g. `"pikchr-raster"`).
    pub(crate) thread_name: &'static str,
    /// Function pointer: render source code to SVG string.
    /// `fn(source: &str, dark_mode: bool) -> Result<String, String>`
    pub(crate) render_source_to_svg: fn(&str, bool) -> Result<String, String>,
    /// Optional SVG preprocessor applied before usvg parsing.
    /// Pikchr uses this to inject font-family; D2 passes `None`.
    pub(crate) svg_preprocessor: Option<fn(&str) -> String>,
}

// ---------------------------------------------------------------------------
// Rasterisation (pure function, runs on worker thread)
// ---------------------------------------------------------------------------

/// Rasterize an SVG string to RGBA pixels.  Pure function -- no GPU access,
/// no egui context needed.  Runs on the worker thread.
///
/// If `svg_preprocessor` is `Some`, applies it to the SVG string before
/// parsing (used by Pikchr to inject font-family declarations).
///
/// Rasterises at `supersample` x the logical display resolution for crisp
/// text and lines.  Returns display dimensions in logical pixels and raster
/// dimensions in actual pixels.
pub(crate) fn rasterize_svg(
    fontdb: &Arc<usvg::fontdb::Database>,
    svg: &str,
    width_bucket: u32,
    scale_bucket: u32,
    supersample: f32,
    svg_preprocessor: Option<fn(&str) -> String>,
) -> Result<RasterOutput, String> {
    // Apply optional SVG preprocessor (e.g. Pikchr font injection).
    let svg_owned;
    let svg = if let Some(preprocess) = svg_preprocessor {
        svg_owned = preprocess(svg);
        &svg_owned
    } else {
        svg
    };

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
    let raster_scale = scale * supersample;
    let mut raster_w = (w as f32 * raster_scale).round().max(1.0) as u32;
    let mut raster_h = (h as f32 * raster_scale).round().max(1.0) as u32;

    // Clamp to reasonable maximum
    if raster_w > MAX_RASTER_SIDE || raster_h > MAX_RASTER_SIDE {
        let clamp_scale = (MAX_RASTER_SIDE as f32 / raster_w as f32)
            .min(MAX_RASTER_SIDE as f32 / raster_h as f32);
        raster_w = (raster_w as f32 * clamp_scale).round().max(1.0) as u32;
        raster_h = (raster_h as f32 * clamp_scale).round().max(1.0) as u32;
    }

    // Rasterize
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

/// Process a single rasterization job: source->SVG (if needed), then
/// SVG->RGBA.  Returns a result with either RGBA data or an error.
pub(crate) fn process_job(
    fontdb: &Arc<usvg::fontdb::Database>,
    job: RasterJob,
    render_source_to_svg: fn(&str, bool) -> Result<String, String>,
    supersample: f32,
    svg_preprocessor: Option<fn(&str) -> String>,
) -> RasterResult {
    // Step 1: Get SVG (from cache or render fresh)
    let svg = match job.svg {
        Some(cached_svg) => Ok(cached_svg),
        None => {
            let code = job.code.as_deref().unwrap_or("");
            render_source_to_svg(code, job.dark_mode)
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
    match rasterize_svg(
        fontdb,
        &svg,
        job.width_bucket,
        job.scale_bucket,
        supersample,
        svg_preprocessor,
    ) {
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

// ---------------------------------------------------------------------------
// DiagramRendererCore
// ---------------------------------------------------------------------------

/// Shared state for diagram renderers.  Owns the LRU caches, worker thread
/// channels, debounce state, and pending-job tracking.  Each concrete
/// renderer (PikchrRenderer, D2Renderer) wraps one of these.
pub(crate) struct DiagramRendererCore {
    /// LRU cache: texture_key (String) -> (TextureHandle, [width, height])
    pub(crate) textures: RefCell<LruCache<String, DiagramTextureEntry>>,
    /// SVG cache: svg_cache_key (u128: code_hash << 1 | dark_mode) -> SVG string
    pub(crate) svg_cache: RefCell<LruCache<u128, String>>,
    /// Error cache: code_hash (u64) -> error message.
    /// Errors don't depend on dark_mode, only on source syntax.
    pub(crate) errors: RefCell<LruCache<u64, String>>,
    /// Per-diagram debounce state for resize suppression.
    pub(crate) debounce: RefCell<HashMap<u64, DiagramDebounce>>,
    /// Maps code_hash -> texture_key of the most recently rasterized texture.
    /// Used to display a stale texture scaled to fit during debounce cooldown.
    pub(crate) latest_texture_key: RefCell<HashMap<u64, String>>,
    /// Job sender. Wrapped in Option so Drop can close the channel
    /// before joining the worker thread.
    job_tx: Option<JobSender>,
    /// Result receiver. Polled each frame in poll_results().
    result_rx: ResultReceiver,
    /// Worker thread handle. Joined on drop for clean shutdown.
    worker_handle: Option<std::thread::JoinHandle<()>>,
    /// Set of code_hashes with in-flight jobs. Keyed by code_hash (not
    /// texture_key) to prevent redundant enqueues when the width changes
    /// while a job is already pending for the same diagram.
    pub(crate) pending: RefCell<HashSet<u64>>,
    /// Tracks the currently desired width bucket per diagram (for stale discard).
    pub(crate) wanted_bucket: RefCell<HashMap<u64, u32>>,
    /// Configuration for this renderer instance.
    config: DiagramRendererConfig,
}

impl DiagramRendererCore {
    /// Create a new core, loading system fonts once at startup and
    /// spawning a dedicated worker thread.
    pub(crate) fn new(config: DiagramRendererConfig) -> Self {
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        let fontdb = Arc::new(db);

        let (job_tx, job_rx) = crossbeam_channel::bounded(config.channel_capacity);
        let (result_tx, result_rx) = crossbeam_channel::bounded(config.channel_capacity);

        // Capture what the worker needs from config before moving config
        let render_fn = config.render_source_to_svg;
        let supersample = config.supersample;
        let svg_preprocessor = config.svg_preprocessor;
        let thread_name = config.thread_name;

        let worker_fontdb = Arc::clone(&fontdb);
        let worker_handle = std::thread::Builder::new()
            .name(thread_name.into())
            .spawn(move || {
                Self::worker_loop(
                    worker_fontdb,
                    job_rx,
                    result_tx,
                    render_fn,
                    supersample,
                    svg_preprocessor,
                );
            })
            .expect("failed to spawn diagram raster worker");

        Self {
            textures: RefCell::new(LruCache::new(config.texture_capacity)),
            svg_cache: RefCell::new(LruCache::new(config.svg_capacity)),
            errors: RefCell::new(LruCache::new(config.error_capacity)),
            debounce: RefCell::new(HashMap::new()),
            latest_texture_key: RefCell::new(HashMap::new()),
            job_tx: Some(job_tx),
            result_rx,
            worker_handle: Some(worker_handle),
            pending: RefCell::new(HashSet::new()),
            wanted_bucket: RefCell::new(HashMap::new()),
            config,
        }
    }

    /// Construct a core from pre-built channels (for testing without a live
    /// worker thread).  The caller provides job_tx and result_rx; no worker
    /// thread is spawned.
    #[cfg(test)]
    pub(crate) fn new_with_channels(
        config: DiagramRendererConfig,
        job_tx: Option<JobSender>,
        result_rx: ResultReceiver,
    ) -> Self {
        Self {
            textures: RefCell::new(LruCache::new(config.texture_capacity)),
            svg_cache: RefCell::new(LruCache::new(config.svg_capacity)),
            errors: RefCell::new(LruCache::new(config.error_capacity)),
            debounce: RefCell::new(HashMap::new()),
            latest_texture_key: RefCell::new(HashMap::new()),
            job_tx,
            result_rx,
            worker_handle: None,
            pending: RefCell::new(HashSet::new()),
            wanted_bucket: RefCell::new(HashMap::new()),
            config,
        }
    }

    // -----------------------------------------------------------------------
    // Worker thread
    // -----------------------------------------------------------------------

    /// Main loop for the worker thread. Receives jobs, processes them, and
    /// sends results back. Uses `try_send` to avoid blocking if the result
    /// channel is full (prevents shutdown deadlock).
    pub(crate) fn worker_loop(
        fontdb: Arc<usvg::fontdb::Database>,
        job_rx: JobReceiver,
        result_tx: ResultSender,
        render_source_to_svg: fn(&str, bool) -> Result<String, String>,
        supersample: f32,
        svg_preprocessor: Option<fn(&str) -> String>,
    ) {
        while let Ok(job) = job_rx.recv() {
            let result = process_job(
                &fontdb,
                job,
                render_source_to_svg,
                supersample,
                svg_preprocessor,
            );
            // Use try_send to avoid blocking if result channel is full.
            // This prevents deadlock during shutdown.
            let _ = result_tx.try_send(result);
        }
        // job_rx disconnected (main thread dropped job_tx) -- exit
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
    pub(crate) fn poll_results(&self, ctx: &egui::Context) -> bool {
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
                        let image = egui::ColorImage::from_rgba_unmultiplied(raster_size, &rgba);
                        let texture = ctx.load_texture(
                            result.texture_key.clone(),
                            image,
                            egui::TextureOptions::LINEAR,
                        );
                        let entry = DiagramTextureEntry {
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
                        if let Some(debounce) = Self::debounce_for_texture_key(&result.texture_key)
                        {
                            self.debounce
                                .borrow_mut()
                                .insert(result.code_hash, debounce);
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

    /// Release GPU textures to reduce idle GPU usage (e.g., when minimised).
    /// SVG and error caches are retained for fast texture rebuilds on next render.
    /// Also clears pending, wanted_bucket, and debounce to allow clean re-enqueue
    /// on restore.
    pub(crate) fn release_gpu_textures(&self) {
        self.textures.borrow_mut().clear();
        self.debounce.borrow_mut().clear();
        self.latest_texture_key.borrow_mut().clear();
        self.wanted_bucket.borrow_mut().clear();
        self.pending.borrow_mut().clear();
    }

    /// Main entry point for rendering a diagram code block.
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
        // borrow_mut because LruCache::get() updates LRU order.
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
            "{}:{:016x}:w{}:s{}:dm{}",
            self.config.key_prefix, code_hash, width_bucket, scale_bucket, dark_mode as u8
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
        let db_entry = self.debounce.borrow().get(&code_hash).map(|d| {
            (
                d.last_rasterized_bucket,
                d.last_seen_bucket,
                d.bucket_changed_at,
            )
        });
        if let Some((last_rasterized, last_seen, _changed_at)) = db_entry {
            if width_bucket != last_rasterized {
                // Width changed since last rasterize -- apply trailing-edge debounce
                if width_bucket != last_seen {
                    // New bucket since last frame -- reset timer
                    let changed_at = Instant::now();
                    self.debounce.borrow_mut().entry(code_hash).and_modify(|d| {
                        d.last_seen_bucket = width_bucket;
                        d.bucket_changed_at = Some(changed_at);
                    });
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

        // unwrap safe: job_tx is only taken in Drop, and render_block
        // is never called after the renderer is dropped.
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
    pub(crate) fn show_stale_or_placeholder(
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
    /// Key format: `<prefix>:<hex>:w<width>:s<scale>:dm<0|1>`
    pub(crate) fn parse_width_from_texture_key(key: &str) -> Option<u32> {
        let w_start = key.find(":w")? + 2;
        let w_end = key[w_start..].find(':')? + w_start;
        key[w_start..w_end].parse().ok()
    }

    pub(crate) fn debounce_for_texture_key(key: &str) -> Option<DiagramDebounce> {
        let width_bucket = Self::parse_width_from_texture_key(key)?;
        Some(DiagramDebounce {
            last_rasterized_bucket: width_bucket,
            last_seen_bucket: width_bucket,
            bucket_changed_at: None,
        })
    }

    /// Check whether a result's width bucket no longer matches what the main
    /// thread currently wants for this diagram. Stale results have their SVG
    /// cached but skip the expensive GPU upload.
    pub(crate) fn is_stale_result(&self, texture_key: &str, code_hash: u64) -> bool {
        let result_bucket = Self::parse_width_from_texture_key(texture_key);
        let wanted = self.wanted_bucket.borrow().get(&code_hash).copied();
        match (result_bucket, wanted) {
            (Some(rb), Some(wb)) => rb != wb, // Width mismatch -> stale
            (Some(_), None) => true,          // No wanted bucket -> stale
            _ => false,                       // Can't parse -> assume fresh
        }
    }

    /// Display a cached texture in the UI, scaling down if it exceeds available width.
    pub(crate) fn render_texture(
        ui: &mut egui::Ui,
        entry: &DiagramTextureEntry,
        available_width: f32,
    ) {
        let (tw, th) = (entry.display_size[0] as f32, entry.display_size[1] as f32);
        let scale = if tw > available_width {
            (available_width / tw).clamp(0.01, 4.0)
        } else {
            1.0
        };
        let size = egui::vec2((tw * scale).round(), (th * scale).round());
        ui.add(egui::Image::new(&entry.texture).fit_to_exact_size(size));
    }

    /// Display a diagram error block with the error message and original source code.
    pub(crate) fn render_error_block(
        &self,
        ui: &mut egui::Ui,
        error: &str,
        code: &str,
        code_font_size: f32,
    ) {
        let tc = ThemeColors::current(ui.visuals().dark_mode);
        let label = self.config.error_label;
        egui::Frame::none()
            .fill(tc.box_bg)
            .stroke(egui::Stroke::new(1.0, egui::Color32::from_rgb(200, 80, 80)))
            .inner_margin(8.0)
            .show(ui, |ui| {
                ui.label(
                    egui::RichText::new(format!("{} rendering error:", label))
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

impl Drop for DiagramRendererCore {
    fn drop(&mut self) {
        // Close job channel -- worker will see Disconnected on recv() and exit.
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
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;

    pub(crate) fn test_raw_input(width: f32, height: f32) -> egui::RawInput {
        egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(width, height),
            )),
            ..Default::default()
        }
    }

    // -----------------------------------------------------------------------
    // Cache key and bucketing tests
    // -----------------------------------------------------------------------

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

    #[test]
    fn test_width_bucketing() {
        assert_eq!(bucket_width(640.0), bucket_width(641.0));
        assert_eq!(bucket_width(640.0), bucket_width(655.0));
        assert_ne!(bucket_width(640.0), bucket_width(656.0));
        assert_ne!(bucket_width(640.0), bucket_width(672.0));
        assert_eq!(bucket_width(640.0), 640);
        assert_eq!(bucket_width(672.0), 672);
    }

    #[test]
    fn test_scale_bucketing() {
        assert_eq!(bucket_scale(1.0), 100);
        assert_eq!(bucket_scale(1.25), 125);
        assert_eq!(bucket_scale(1.5), 150);
        assert_eq!(bucket_scale(2.0), 200);
        assert_eq!(bucket_scale(0.5), 50);
        assert_eq!(bucket_scale(1.124), 112);
        assert_eq!(bucket_scale(1.126), 113);
        assert_eq!(bucket_scale(1.001), bucket_scale(1.004));
    }

    // -----------------------------------------------------------------------
    // Texture key parsing and debounce
    // -----------------------------------------------------------------------

    #[test]
    fn test_parse_width_from_texture_key() {
        assert_eq!(
            DiagramRendererCore::parse_width_from_texture_key(
                "pikchr:0000000000003039:w640:s100:dm0"
            ),
            Some(640)
        );
        assert_eq!(
            DiagramRendererCore::parse_width_from_texture_key("d2:abc:w1024:s150:dm1"),
            Some(1024)
        );
        assert_eq!(
            DiagramRendererCore::parse_width_from_texture_key("d2:abc:w0:s100:dm0"),
            Some(0)
        );
        // No :w prefix
        assert_eq!(
            DiagramRendererCore::parse_width_from_texture_key("d2:abc:s100:dm0"),
            None
        );
    }

    #[test]
    fn test_debounce_for_texture_key_builds_entry() {
        let debounce =
            DiagramRendererCore::debounce_for_texture_key("pikchr:0000000000003039:w640:s100:dm0")
                .expect("debounce entry");

        assert_eq!(debounce.last_rasterized_bucket, 640);
        assert_eq!(debounce.last_seen_bucket, 640);
        assert!(debounce.bucket_changed_at.is_none());
    }

    #[test]
    fn test_debounce_for_texture_key_invalid_returns_none() {
        assert!(DiagramRendererCore::debounce_for_texture_key("d2:abc:s100:dm0").is_none());
    }

    // -----------------------------------------------------------------------
    // Stale result tests
    // -----------------------------------------------------------------------

    /// Dummy render function for test configs.
    fn dummy_render(_code: &str, _dark_mode: bool) -> Result<String, String> {
        Ok("<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1 1\"></svg>".to_string())
    }

    fn test_config() -> DiagramRendererConfig {
        DiagramRendererConfig {
            key_prefix: "test",
            error_label: "Test",
            supersample: 2.0,
            texture_capacity: 4,
            svg_capacity: 4,
            error_capacity: 4,
            channel_capacity: 8,
            thread_name: "test-raster",
            render_source_to_svg: dummy_render,
            svg_preprocessor: None,
        }
    }

    fn test_core_with_channels(
        job_tx: Option<JobSender>,
        result_rx: ResultReceiver,
    ) -> DiagramRendererCore {
        DiagramRendererCore::new_with_channels(test_config(), job_tx, result_rx)
    }

    fn insert_test_texture(
        ui: &mut egui::Ui,
        core: &DiagramRendererCore,
        key: &str,
        display_size: [u32; 2],
    ) {
        let image = egui::ColorImage::new([2, 2], egui::Color32::WHITE);
        let texture =
            ui.ctx()
                .load_texture(key.to_string(), image, egui::TextureOptions::default());
        core.textures.borrow_mut().insert(
            key.to_string(),
            DiagramTextureEntry {
                texture,
                display_size,
            },
        );
    }

    #[test]
    fn test_is_stale_result_width_mismatch() {
        let core = DiagramRendererCore::new(test_config());
        let code_hash = 12345u64;

        core.wanted_bucket.borrow_mut().insert(code_hash, 640);

        let stale_key = format!("test:{:016x}:w1024:s100:dm0", code_hash);
        assert!(
            core.is_stale_result(&stale_key, code_hash),
            "Width 1024 vs wanted 640 should be stale"
        );

        let fresh_key = format!("test:{:016x}:w640:s100:dm0", code_hash);
        assert!(
            !core.is_stale_result(&fresh_key, code_hash),
            "Width 640 vs wanted 640 should be fresh"
        );
    }

    #[test]
    fn test_is_stale_result_when_wanted_is_none() {
        let core = DiagramRendererCore::new(test_config());
        let texture_key = format!("test:{:016x}:w640:s100:dm0", 99999u64);
        assert!(
            core.is_stale_result(&texture_key, 99999),
            "No wanted_bucket entry should be treated as stale"
        );
    }

    #[test]
    fn test_is_stale_result_invalid_texture_key_is_assumed_fresh() {
        let core = DiagramRendererCore::new(test_config());
        let code_hash = 55;
        core.wanted_bucket.borrow_mut().insert(code_hash, 320);

        assert!(
            !core.is_stale_result("test:malformed", code_hash),
            "Malformed texture keys should be treated as fresh"
        );
    }

    // -----------------------------------------------------------------------
    // Pending set tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_pending_set_basics() {
        let core = DiagramRendererCore::new(test_config());

        assert!(
            !core.has_pending(),
            "Pending should be empty on new renderer"
        );

        core.pending.borrow_mut().insert(42);
        assert!(
            core.has_pending(),
            "Pending should be non-empty after insert"
        );

        core.pending.borrow_mut().remove(&42);
        assert!(!core.has_pending(), "Pending should be empty after remove");
    }

    #[test]
    fn test_pending_dedup_prevents_double_enqueue() {
        let core = DiagramRendererCore::new(test_config());
        let code_hash = 42u64;

        core.pending.borrow_mut().insert(code_hash);
        assert!(core.pending.borrow().contains(&code_hash));

        core.pending.borrow_mut().insert(code_hash);
        assert_eq!(core.pending.borrow().len(), 1);
    }

    // -----------------------------------------------------------------------
    // Release GPU textures
    // -----------------------------------------------------------------------

    #[test]
    fn test_release_gpu_clears_pending_and_wanted() {
        let core = DiagramRendererCore::new(test_config());

        core.pending.borrow_mut().insert(1);
        core.wanted_bucket.borrow_mut().insert(1, 640);
        core.debounce.borrow_mut().insert(
            1,
            DiagramDebounce {
                last_rasterized_bucket: 640,
                last_seen_bucket: 640,
                bucket_changed_at: None,
            },
        );
        core.latest_texture_key
            .borrow_mut()
            .insert(1, "test:key".to_string());

        core.release_gpu_textures();

        assert!(core.pending.borrow().is_empty());
        assert!(core.wanted_bucket.borrow().is_empty());
        assert!(core.debounce.borrow().is_empty());
        assert!(core.latest_texture_key.borrow().is_empty());
    }

    // -----------------------------------------------------------------------
    // Shutdown
    // -----------------------------------------------------------------------

    #[test]
    fn test_shutdown_joins_worker() {
        let start = Instant::now();
        {
            let core = DiagramRendererCore::new(test_config());
            drop(core);
        }
        let elapsed = start.elapsed();
        assert!(
            elapsed < Duration::from_secs(5),
            "Drop should join worker within 5 seconds, took {:?}",
            elapsed
        );
    }

    // -----------------------------------------------------------------------
    // Channel tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_queue_full_does_not_block() {
        let (tx, _rx) = crossbeam_channel::bounded::<RasterJob>(2);

        for i in 0..2 {
            tx.send(RasterJob {
                code_hash: i,
                texture_key: format!("test:dummy:w640:s100:dm0:{}", i),
                code: Some("dummy".to_string()),
                svg: None,
                dark_mode: false,
                width_bucket: 640,
                scale_bucket: 100,
            })
            .unwrap();
        }

        let result = tx.try_send(RasterJob {
            code_hash: 99,
            texture_key: "test:dummy:w640:s100:dm0:99".to_string(),
            code: Some("dummy".to_string()),
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

    // -----------------------------------------------------------------------
    // Render block infrastructure tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_renderer_new_creates_empty_caches() {
        let core = DiagramRendererCore::new(test_config());
        assert!(!core.has_pending());
    }

    #[test]
    fn test_render_block_basic_and_pending_placeholder() {
        let core = DiagramRendererCore::new(test_config());
        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let code = "dummy source";
        let mut rendered_first = false;
        let mut rendered_second = false;

        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered_first = core.render_block(ui, code, 1.0, 14.0);
                rendered_second = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered_first);
        assert!(rendered_second);
        assert!(core.pending.borrow().contains(&hash_str(code)));
    }

    #[test]
    fn test_render_block_reports_cached_error() {
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(None, result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);
        core.errors
            .borrow_mut()
            .insert(code_hash, "boom".to_string());

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert!(!core.has_pending());
    }

    #[test]
    fn test_render_block_enqueues_cached_svg() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(Some(job_tx), result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);
        core.svg_cache.borrow_mut().insert(
            svg_cache_key(code_hash, false),
            "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_string(),
        );
        core.svg_cache.borrow_mut().insert(
            svg_cache_key(code_hash, true),
            "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_string(),
        );

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        let request = job_rx
            .recv_timeout(Duration::from_secs(1))
            .expect("request");
        assert!(request.svg.is_some());
        assert!(request.code.is_none());
        assert!(core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_render_block_queue_full_keeps_rendering() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(Some(job_tx.clone()), result_rx);
        job_tx
            .send(RasterJob {
                code_hash: 1,
                texture_key: "test:full:w224:s100:dm0".to_string(),
                code: Some("dummy".to_string()),
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
                rendered = core.render_block(ui, "dummy", 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert!(!core.has_pending());
        drop(job_rx);
    }

    #[test]
    fn test_render_block_disconnected_queue_sets_error() {
        let (job_tx, job_rx) = crossbeam_channel::bounded::<RasterJob>(1);
        drop(job_rx);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(Some(job_tx), result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert!(core.errors.borrow_mut().get(&code_hash).is_some());
    }

    // -----------------------------------------------------------------------
    // begin_frame tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_begin_frame_processes_result_and_uses_cached_texture() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(2);
        let (job_tx, _job_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(Some(job_tx), result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);
        core.pending.borrow_mut().insert(code_hash);
        core.pending.borrow_mut().insert(999);
        let result_tx = RefCell::new(Some(result_tx));
        let texture_key = RefCell::new(String::new());

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                let width_bucket = bucket_width(ui.available_width().max(1.0));
                let key = format!(
                    "test:{:016x}:w{}:s{}:dm{}",
                    code_hash,
                    width_bucket,
                    100,
                    ui.visuals().dark_mode as u8
                );
                *texture_key.borrow_mut() = key.clone();
                core.wanted_bucket
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
                core.begin_frame(ctx);
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        let texture_key = texture_key.into_inner();
        assert!(core.textures.borrow_mut().get(&texture_key).is_some());
        assert!(core
            .svg_cache
            .borrow_mut()
            .get(&svg_cache_key(code_hash, ctx.style().visuals.dark_mode))
            .is_some());
        assert_eq!(
            core.latest_texture_key.borrow().get(&code_hash),
            Some(&texture_key)
        );
        assert!(!core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_result_missing_raster_fields_skips_texture_upload() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(None, result_rx);
        let code_hash = hash_str("dummy source");
        core.pending.borrow_mut().insert(code_hash);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "test:missing-fields:w224:s100:dm0".to_string(),
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
                core.begin_frame(ctx);
            });
        });

        assert_eq!(core.textures.borrow().len(), 0);
        assert!(!core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_result_missing_raster_size_skips_texture_upload() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(None, result_rx);
        let code_hash = hash_str("dummy tall");
        core.pending.borrow_mut().insert(code_hash);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "test:partial-fields:w224:s100:dm0".to_string(),
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
                core.begin_frame(ctx);
            });
        });

        assert_eq!(core.textures.borrow().len(), 0);
        assert!(!core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_keeps_polling_when_pending_remains() {
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(1);
        let core = test_core_with_channels(None, result_rx);
        core.pending.borrow_mut().insert(999);

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |_ui| {
                core.begin_frame(ctx);
            });
        });

        assert!(core.pending.borrow().contains(&999));
        drop(result_tx);
    }

    #[test]
    fn test_begin_frame_caches_error_results() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(None, result_rx);
        let code_hash = 7;
        core.pending.borrow_mut().insert(code_hash);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "test:error:w224:s100:dm0".to_string(),
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
                core.begin_frame(ctx);
            });
        });

        assert!(core.errors.borrow_mut().get(&code_hash).is_some());
        assert!(!core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_discards_stale_result() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(None, result_rx);
        let code_hash = 11;
        core.pending.borrow_mut().insert(code_hash);
        core.wanted_bucket.borrow_mut().insert(code_hash, 128);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "test:stale:w224:s100:dm0".to_string(),
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
                core.begin_frame(ctx);
            });
        });

        assert_eq!(core.textures.borrow().len(), 0);
        assert!(core
            .svg_cache
            .borrow_mut()
            .get(&svg_cache_key(code_hash, false))
            .is_some());
        assert!(!core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_processes_result_with_unparseable_texture_key() {
        let (result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(None, result_rx);
        let code_hash = 19;
        core.pending.borrow_mut().insert(code_hash);
        core.wanted_bucket.borrow_mut().insert(code_hash, 128);
        result_tx
            .send(RasterResult {
                code_hash,
                texture_key: "test:not-a-valid-width-key".to_string(),
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
                core.begin_frame(ctx);
            });
        });

        assert!(core
            .textures
            .borrow_mut()
            .get(&"test:not-a-valid-width-key".to_string())
            .is_some());
        assert!(core.debounce.borrow().get(&code_hash).is_none());
        assert!(!core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_begin_frame_disconnected_clears_pending() {
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(1);
        drop(result_tx);
        let core = test_core_with_channels(None, result_rx);
        core.pending.borrow_mut().insert(1);

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |_ui| {
                core.begin_frame(ctx);
            });
        });

        assert!(core.pending.borrow().is_empty());
    }

    // -----------------------------------------------------------------------
    // Debounce interaction tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_render_block_debounce_without_stale_texture_enqueues_job() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(Some(job_tx), result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);
        core.debounce.borrow_mut().insert(
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
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(core.pending.borrow().contains(&code_hash));
        assert_eq!(
            core.debounce
                .borrow()
                .get(&code_hash)
                .map(|d| d.last_seen_bucket),
            Some(bucket_width(96.0))
        );
    }

    #[test]
    fn test_render_block_uses_stale_texture_during_debounce() {
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(None, result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);
        let stale_key = format!("test:{:016x}:w224:s100:dm0", code_hash);
        core.latest_texture_key
            .borrow_mut()
            .insert(code_hash, stale_key.clone());
        core.debounce.borrow_mut().insert(
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
                insert_test_texture(ui, &core, &stale_key, [320, 160]);
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert!(!core.has_pending());
        assert_eq!(
            core.debounce
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
        let core = test_core_with_channels(Some(job_tx), result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);
        let stale_key = format!("test:{:016x}:w224:s100:dm0", code_hash);
        core.latest_texture_key
            .borrow_mut()
            .insert(code_hash, stale_key);
        core.debounce.borrow_mut().insert(
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
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_render_block_same_bucket_missing_texture_enqueues_job() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(Some(job_tx), result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);
        let width_bucket = bucket_width(96.0);
        core.debounce.borrow_mut().insert(
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
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_render_block_missing_timer_same_seen_bucket_enqueues_job() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(Some(job_tx), result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);
        let width_bucket = bucket_width(96.0);
        core.debounce.borrow_mut().insert(
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
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_render_block_expired_debounce_enqueues_job() {
        let (job_tx, job_rx) = crossbeam_channel::bounded(1);
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(Some(job_tx), result_rx);
        let code = "dummy source";
        let code_hash = hash_str(code);
        let width_bucket = bucket_width(96.0);
        core.debounce.borrow_mut().insert(
            code_hash,
            DiagramDebounce {
                last_rasterized_bucket: 224,
                last_seen_bucket: width_bucket,
                bucket_changed_at: Some(
                    Instant::now() - Duration::from_millis(RESIZE_DEBOUNCE_MS + 5),
                ),
            },
        );

        let ctx = egui::Context::default();
        let input = test_raw_input(96.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = core.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered);
        assert_eq!(job_rx.len(), 1);
        assert!(core.pending.borrow().contains(&code_hash));
    }

    #[test]
    fn test_show_stale_or_placeholder_ignores_missing_stale_texture() {
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(None, result_rx);
        let code_hash = hash_str("missing-stale");
        core.latest_texture_key
            .borrow_mut()
            .insert(code_hash, "test:missing:w224:s100:dm0".to_string());

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = core.show_stale_or_placeholder(ui, code_hash, 240.0);
            });
        });

        assert!(rendered);
        assert_eq!(core.textures.borrow().len(), 0);
    }

    #[test]
    fn test_show_stale_or_placeholder_renders_stale_texture() {
        let (_result_tx, result_rx) = crossbeam_channel::bounded(1);
        let core = test_core_with_channels(None, result_rx);
        let code_hash = hash_str("present-stale");
        let stale_key = "test:present:w224:s100:dm0".to_string();
        core.latest_texture_key
            .borrow_mut()
            .insert(code_hash, stale_key.clone());

        let ctx = egui::Context::default();
        let input = test_raw_input(240.0, 160.0);
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                insert_test_texture(ui, &core, &stale_key, [320, 160]);
                rendered = core.show_stale_or_placeholder(ui, code_hash, 240.0);
            });
        });

        assert!(rendered);
        assert_eq!(core.textures.borrow().len(), 1);
    }

    // -----------------------------------------------------------------------
    // Rasterize SVG tests
    // -----------------------------------------------------------------------

    fn make_fontdb() -> Arc<usvg::fontdb::Database> {
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        Arc::new(db)
    }

    #[test]
    fn test_rasterize_svg_uses_base_scale_when_width_bucket_is_zero_and_clamps() {
        let fontdb = make_fontdb();
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 5000 5000\"><rect width=\"5000\" height=\"5000\" fill=\"red\"/></svg>";

        let output = rasterize_svg(&fontdb, svg, 0, 100, 2.0, None).expect("rasterize");

        assert_eq!(output.display_size, [5000, 5000]);
        assert!(output.raster_size[0] <= MAX_RASTER_SIDE as usize);
        assert!(output.raster_size[1] <= MAX_RASTER_SIDE as usize);
    }

    #[test]
    fn test_rasterize_svg_clamps_very_large_raster_dimensions() {
        let fontdb = make_fontdb();
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 10000 8000\"><rect width=\"10000\" height=\"8000\" fill=\"red\"/></svg>";

        let output = rasterize_svg(&fontdb, svg, 0, 100, 2.0, None).expect("rasterize");

        assert_eq!(output.display_size, [10000, 8000]);
        assert_eq!(output.raster_size[0], MAX_RASTER_SIDE as usize);
        assert!(output.raster_size[1] <= MAX_RASTER_SIDE as usize);
    }

    #[test]
    fn test_rasterize_svg_clamps_very_tall_raster_dimensions() {
        let fontdb = make_fontdb();
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 5000\"><rect width=\"100\" height=\"5000\" fill=\"red\"/></svg>";

        let output = rasterize_svg(&fontdb, svg, 0, 100, 2.0, None).expect("rasterize");

        assert_eq!(output.display_size, [100, 5000]);
        assert!(output.raster_size[0] < MAX_RASTER_SIDE as usize);
        assert_eq!(output.raster_size[1], MAX_RASTER_SIDE as usize);
    }

    #[test]
    fn test_rasterize_svg_invalid_svg_returns_error() {
        let fontdb = make_fontdb();
        let err = match rasterize_svg(&fontdb, "<svg", 224, 100, 2.0, None) {
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
                texture_key: format!("test:{:016x}:w224:s100:dm0", code_hash),
                code: None,
                svg: Some("<svg".to_string()),
                dark_mode: false,
                width_bucket: 224,
                scale_bucket: 100,
            },
            dummy_render,
            2.0,
            None,
        );

        assert!(result.error.is_some());
        assert!(result.svg.is_some());
        assert!(result.rgba.is_none());
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
                texture_key: format!("test:{:016x}:w640:s100:dm0", code_hash),
                code: None,
                svg: Some(valid_svg.to_string()),
                dark_mode: false,
                width_bucket: 640,
                scale_bucket: 100,
            },
            dummy_render,
            2.0,
            None,
        );

        assert!(result.rgba.is_some());
        assert!(result.raster_size.is_some());
        assert!(result.display_size.is_some());
        assert!(result.error.is_none());
    }

    #[test]
    fn test_worker_processes_job() {
        let fontdb = make_fontdb();
        let (job_tx, job_rx) = crossbeam_channel::bounded::<RasterJob>(4);
        let (result_tx, result_rx) = crossbeam_channel::bounded::<RasterResult>(4);

        let code = "dummy source";
        let code_hash = hash_str(code);
        let texture_key = format!("test:{:016x}:w640:s100:dm0", code_hash);
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

        DiagramRendererCore::worker_loop(fontdb, job_rx, result_tx, dummy_render, 2.0, None);

        let result = result_rx.recv_timeout(Duration::from_secs(5)).unwrap();
        assert!(result.rgba.is_some(), "Worker should produce RGBA data");
        assert!(result.raster_size.is_some());
        assert!(result.display_size.is_some());
        assert!(result.error.is_none());
        assert!(result.svg.is_some());
    }

    #[test]
    fn test_worker_try_send_full_does_not_block() {
        let fontdb = make_fontdb();
        let (job_tx, job_rx) = crossbeam_channel::bounded::<RasterJob>(4);
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

        let code = "dummy source";
        let code_hash = hash_str(code);
        job_tx
            .send(RasterJob {
                code_hash,
                texture_key: format!("test:{:016x}:w640:s100:dm0", code_hash),
                code: Some(code.to_string()),
                svg: None,
                dark_mode: false,
                width_bucket: 640,
                scale_bucket: 100,
            })
            .unwrap();
        drop(job_tx);

        let handle = std::thread::spawn(move || {
            DiagramRendererCore::worker_loop(fontdb, job_rx, result_tx, dummy_render, 2.0, None);
        });

        let start = Instant::now();
        loop {
            if handle.is_finished() {
                break;
            }
            if start.elapsed() > Duration::from_secs(5) {
                panic!("worker_loop blocked for >5s");
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        handle.join().unwrap();

        let dummy = result_rx.try_recv().unwrap();
        assert_eq!(dummy.code_hash, 0);
    }
}
