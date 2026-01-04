#[cfg(feature = "mermaid-quickjs")]
use crossbeam_channel::{bounded, Receiver, Sender, TrySendError};
use egui::{Color32, RichText, Stroke};
#[cfg(feature = "mermaid-quickjs")]
use std::cell::{Cell, RefCell};
#[cfg(feature = "mermaid-quickjs")]
use std::collections::HashSet;
#[cfg(any(test, feature = "mermaid-quickjs"))]
use std::collections::{hash_map::DefaultHasher, HashMap, VecDeque};
#[cfg(any(test, feature = "mermaid-quickjs"))]
use std::hash::{Hash, Hasher};

#[cfg(feature = "mermaid-quickjs")]
use std::path::PathBuf;
#[cfg(feature = "mermaid-quickjs")]
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};
#[cfg(feature = "mermaid-quickjs")]
use std::time::{SystemTime, UNIX_EPOCH};

#[cfg(feature = "mermaid-quickjs")]
mod mermaid_embed {
    include!(concat!(env!("OUT_DIR"), "/mermaid_js.rs"));
}

#[cfg(feature = "mermaid-quickjs")]
struct MermaidEngine {
    #[allow(dead_code)]
    rt: rquickjs::Runtime,
    ctx: rquickjs::Context,
}

#[cfg(feature = "mermaid-quickjs")]
type MermaidJobSender = Sender<MermaidRequest>;
#[cfg(feature = "mermaid-quickjs")]
type MermaidJobReceiver = Receiver<MermaidRequest>;
#[cfg(feature = "mermaid-quickjs")]
type MermaidResultSender = Sender<MermaidResult>;
#[cfg(feature = "mermaid-quickjs")]
type MermaidResultReceiver = Receiver<MermaidResult>;

#[cfg(feature = "mermaid-quickjs")]
#[derive(Clone)]
struct MermaidTextureEntry {
    texture: egui::TextureHandle,
    size: [u32; 2],
}

#[cfg(any(test, feature = "mermaid-quickjs"))]
struct LruCache<K, V> {
    entries: HashMap<K, V>,
    order: VecDeque<K>,
    capacity: usize,
}

#[cfg(any(test, feature = "mermaid-quickjs"))]
impl<K, V> LruCache<K, V>
where
    K: Eq + std::hash::Hash + Clone,
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
        if value.is_some() {
            self.touch(key);
        }
        value
    }

    fn insert(&mut self, key: K, value: V) {
        if self.entries.contains_key(&key) {
            self.entries.insert(key.clone(), value);
            self.touch(&key);
            return;
        }
        while self.entries.len() >= self.capacity {
            if let Some(old) = self.order.pop_front() {
                self.entries.remove(&old);
            } else {
                break;
            }
        }
        self.order.push_back(key.clone());
        self.entries.insert(key, value);
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn remove(&mut self, key: &K) {
        self.entries.remove(key);
        self.order.retain(|entry| entry != key);
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.entries.len()
    }

    fn touch(&mut self, key: &K) {
        self.order.retain(|entry| entry != key);
        self.order.push_back(key.clone());
    }
}

#[cfg(feature = "mermaid-quickjs")]
struct MermaidRequest {
    svg_key: u64,
    texture_key: String,
    code: Option<String>,
    svg: Option<String>,
    width_bucket: u32,
    scale_bucket: u32,
    viewport_width: u32,
    viewport_height: u32,
    bg: Option<[u8; 4]>,
}

#[cfg(feature = "mermaid-quickjs")]
struct MermaidResult {
    svg_key: u64,
    texture_key: String,
    svg: Option<String>,
    rgba: Option<Vec<u8>>,
    size: Option<(u32, u32)>,
    error: Option<String>,
}

#[cfg(feature = "mermaid-quickjs")]
#[derive(Debug, PartialEq, Eq)]
enum MermaidEnqueueError {
    QueueFull,
    Disconnected,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MermaidRenderPreference {
    Embedded,
    Off,
}

#[cfg(feature = "mermaid-quickjs")]
struct MermaidThemeValues {
    theme_name: String,
    main_bkg: String,
    primary: String,
    primary_border: String,
    primary_text: String,
    secondary: String,
    tertiary: String,
    line: String,
    text: String,
    cluster_bkg: String,
    cluster_border: String,
    default_link: String,
    title: String,
    label_bg: String,
    edge_label_bg: String,
    font_family: Option<String>,
}

pub(crate) struct MermaidRenderer {
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_textures: RefCell<LruCache<String, MermaidTextureEntry>>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_pending: RefCell<HashSet<String>>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_frame_pending: Cell<bool>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_svg_cache: RefCell<LruCache<u64, String>>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_errors: RefCell<LruCache<u64, String>>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_texture_errors: RefCell<LruCache<String, String>>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_job_tx: MermaidJobSender,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_result_rx: MermaidResultReceiver,
}

impl MermaidRenderer {
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_TEXTURE_CACHE_CAPACITY: usize = 128;
    #[cfg(any(test, feature = "mermaid-quickjs"))]
    const MERMAID_WIDTH_BUCKET_STEP: u32 = 32;
    #[cfg(any(test, feature = "mermaid-quickjs"))]
    const MERMAID_SCALE_BUCKET_FACTOR: f32 = 100.0;
    #[cfg(feature = "mermaid-quickjs")]
    const MAX_MERMAID_JOBS: usize = 4;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_SVG_CACHE_CAPACITY: usize = 64;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_ERROR_CACHE_CAPACITY: usize = 64;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_TEXTURE_ERROR_CACHE_CAPACITY: usize = 64;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_MAX_TEXT_SIZE: u32 = 50_000;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_MAX_RENDER_SIDE: u32 = 4096;

    pub(crate) fn new() -> Self {
        #[cfg(feature = "mermaid-quickjs")]
        let (mermaid_job_tx, mermaid_job_rx): (MermaidJobSender, MermaidJobReceiver) =
            bounded(Self::MAX_MERMAID_JOBS * 4);
        #[cfg(feature = "mermaid-quickjs")]
        let (mermaid_result_tx, mermaid_result_rx): (
            MermaidResultSender,
            MermaidResultReceiver,
        ) = bounded(Self::MAX_MERMAID_JOBS * 4);
        #[cfg(feature = "mermaid-quickjs")]
        Self::spawn_mermaid_workers(mermaid_job_rx, mermaid_result_tx);
        Self {
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_textures: RefCell::new(LruCache::new(Self::MERMAID_TEXTURE_CACHE_CAPACITY)),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_pending: RefCell::new(HashSet::new()),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_frame_pending: Cell::new(false),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_svg_cache: RefCell::new(LruCache::new(Self::MERMAID_SVG_CACHE_CAPACITY)),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_errors: RefCell::new(LruCache::new(Self::MERMAID_ERROR_CACHE_CAPACITY)),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_texture_errors: RefCell::new(LruCache::new(
                Self::MERMAID_TEXTURE_ERROR_CACHE_CAPACITY,
            )),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_job_tx,
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_result_rx,
        }
    }

    fn mermaid_renderer_preference() -> (MermaidRenderPreference, bool) {
        let has_embedded = cfg!(feature = "mermaid-quickjs");
        if let Ok(raw) = std::env::var("MDMDVIEW_MERMAID_RENDERER") {
            let normalized = raw.trim().to_ascii_lowercase();
            return match normalized.as_str() {
                "off" => (MermaidRenderPreference::Off, true),
                "embedded" => (MermaidRenderPreference::Embedded, true),
                _ => {
                    if has_embedded {
                        (MermaidRenderPreference::Embedded, false)
                    } else {
                        (MermaidRenderPreference::Off, false)
                    }
                }
            };
        }
        if has_embedded {
            (MermaidRenderPreference::Embedded, false)
        } else {
            (MermaidRenderPreference::Off, false)
        }
    }

    pub(crate) fn has_pending(&self) -> bool {
        #[cfg(feature = "mermaid-quickjs")]
        {
            if self.mermaid_frame_pending.get() || !self.mermaid_pending.borrow().is_empty() {
                return true;
            }
        }
        false
    }

    pub(crate) fn begin_frame(&self) {
        #[cfg(feature = "mermaid-quickjs")]
        self.mermaid_frame_pending.set(false);
    }

    pub(crate) fn render_block(
        &self,
        ui: &mut egui::Ui,
        code: &str,
        ui_scale: f32,
        code_font_size: f32,
    ) -> bool {
        let (preference, explicit) = Self::mermaid_renderer_preference();

        #[cfg(not(feature = "mermaid-quickjs"))]
        {
            let _ = code;
            let _ = ui_scale;
        }

        if preference == MermaidRenderPreference::Off {
            egui::Frame::none()
                .fill(Color32::from_rgb(25, 25, 25))
                .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                .inner_margin(8.0)
                .show(ui, |ui| {
                    ui.label(
                        RichText::new(
                            "Mermaid rendering is disabled. Set MDMDVIEW_MERMAID_RENDERER=embedded to enable.",
                        )
                        .color(Color32::from_rgb(200, 160, 80))
                        .family(egui::FontFamily::Monospace)
                        .size(code_font_size),
                    );
                    ui.add_space(6.0);
                    ui.label(
                        RichText::new(code)
                            .family(egui::FontFamily::Monospace)
                            .size(code_font_size)
                            .color(Color32::from_rgb(180, 180, 180)),
                    );
                });
            return true;
        }

        if explicit
            && preference == MermaidRenderPreference::Embedded
            && !cfg!(feature = "mermaid-quickjs")
        {
            egui::Frame::none()
                .fill(Color32::from_rgb(25, 25, 25))
                .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                .inner_margin(8.0)
                .show(ui, |ui| {
                    ui.label(
                        RichText::new(
                            "Mermaid rendering via embedded JS is unavailable (feature not enabled).",
                        )
                        .color(Color32::from_rgb(200, 160, 80))
                        .family(egui::FontFamily::Monospace)
                        .size(code_font_size),
                    );
                    ui.add_space(6.0);
                    ui.label(
                        RichText::new(code)
                            .family(egui::FontFamily::Monospace)
                            .size(code_font_size)
                            .color(Color32::from_rgb(180, 180, 180)),
                    );
                });
            return true;
        }

        #[cfg(feature = "mermaid-quickjs")]
        {
            if preference == MermaidRenderPreference::Embedded
                && mermaid_embed::MERMAID_JS.is_empty()
            {
                egui::Frame::none()
                    .fill(Color32::from_rgb(25, 25, 25))
                    .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                    .inner_margin(8.0)
                    .show(ui, |ui| {
                        ui.label(
                            RichText::new(
                                "Mermaid rendering is unavailable (embedded JS missing).",
                            )
                            .color(Color32::from_rgb(200, 160, 80))
                            .family(egui::FontFamily::Monospace)
                            .size(code_font_size),
                        );
                        ui.add_space(6.0);
                        ui.label(
                            RichText::new(code)
                                .family(egui::FontFamily::Monospace)
                                .size(code_font_size)
                                .color(Color32::from_rgb(180, 180, 180)),
                        );
                    });
                return true;
            } else if preference == MermaidRenderPreference::Embedded {
                let svg_key = Self::hash_str(code);
                let width_bucket = Self::width_bucket(ui.available_width());
                let scale_bucket = Self::scale_bucket(ui_scale);
                let (viewport_width, viewport_height) = Self::viewport_px(ui.ctx());
                let bg = Self::mermaid_bg_fill();
                let texture_key = Self::texture_key(svg_key, width_bucket, scale_bucket, bg);

                if self.poll_mermaid_results(ui.ctx()) {
                    ui.ctx().request_repaint();
                }

                if let Some(entry) = self.mermaid_textures.borrow_mut().get(&texture_key) {
                    let (tw, th) = (entry.size[0] as f32, entry.size[1] as f32);
                    let available_w = ui.available_width().max(1.0);
                    let scale = if tw > available_w {
                        (available_w / tw).clamp(0.01, 4.0)
                    } else {
                        1.0
                    };
                    let size = egui::vec2((tw * scale).round(), (th * scale).round());
                    ui.add(egui::Image::new(&entry.texture).fit_to_exact_size(size));
                    return true;
                }

                let svg_error = self.mermaid_errors.borrow_mut().get(&svg_key);
                let texture_error = self.mermaid_texture_errors.borrow_mut().get(&texture_key);
                if let Some(err) = svg_error.or(texture_error) {
                    egui::Frame::none()
                        .fill(Color32::from_rgb(25, 25, 25))
                        .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                        .inner_margin(8.0)
                        .show(ui, |ui| {
                            ui.label(
                                RichText::new("Mermaid render failed; showing source.")
                                    .color(Color32::from_rgb(200, 160, 80)),
                            );
                            ui.label(
                                RichText::new(format!(
                                    "Embedded bytes:{}\nHash:{:016x}\nError:{}",
                                    mermaid_embed::MERMAID_JS.len(),
                                    svg_key,
                                    err
                                ))
                                .family(egui::FontFamily::Monospace)
                                .size(code_font_size)
                                .color(Color32::from_rgb(180, 180, 180)),
                            );
                            ui.add_space(6.0);
                            ui.label(
                                RichText::new(code)
                                    .family(egui::FontFamily::Monospace)
                                    .size(code_font_size)
                                    .color(Color32::from_rgb(180, 180, 180)),
                            );
                        });
                    return true;
                } else {
                    let svg = self.mermaid_svg_cache.borrow_mut().get(&svg_key);
                    let pending = self.mermaid_pending.borrow().contains(&texture_key);
                    let mut waiting_for_slot = false;

                    if !pending {
                        let (request_code, request_svg) = match svg {
                            Some(svg) => (None, Some(svg)),
                            None => (Some(code.to_string()), None),
                        };
                        let request = MermaidRequest {
                            svg_key,
                            texture_key: texture_key.clone(),
                            code: request_code,
                            svg: request_svg,
                            width_bucket,
                            scale_bucket,
                            viewport_width,
                            viewport_height,
                            bg,
                        };
                        match self.enqueue_mermaid_job(request) {
                            Ok(()) => {
                                self.mermaid_pending
                                    .borrow_mut()
                                    .insert(texture_key.clone());
                                ui.ctx().request_repaint();
                            }
                            Err(MermaidEnqueueError::QueueFull) => {
                                waiting_for_slot = true;
                                ui.ctx().request_repaint();
                            }
                            Err(MermaidEnqueueError::Disconnected) => {
                                self.mermaid_errors
                                    .borrow_mut()
                                    .insert(svg_key, "Mermaid worker pool unavailable".to_string());
                                egui::Frame::none()
                                    .fill(Color32::from_rgb(25, 25, 25))
                                    .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                                    .inner_margin(8.0)
                                    .show(ui, |ui| {
                                        ui.label(
                                            RichText::new("Mermaid worker pool unavailable.")
                                                .color(Color32::from_rgb(200, 160, 80))
                                                .family(egui::FontFamily::Monospace)
                                                .size(code_font_size),
                                        );
                                        ui.add_space(6.0);
                                        ui.label(
                                            RichText::new(code)
                                                .family(egui::FontFamily::Monospace)
                                                .size(code_font_size)
                                                .color(Color32::from_rgb(180, 180, 180)),
                                        );
                                    });
                                return true;
                            }
                        }
                    }

                let inflight = self.mermaid_pending.borrow().len();
                self.mermaid_frame_pending.set(true);
                egui::Frame::none()
                        .fill(Color32::from_rgb(25, 25, 25))
                        .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                        .inner_margin(8.0)
                        .show(ui, |ui| {
                            if waiting_for_slot {
                                ui.label(
                                    RichText::new(format!(
                                        "Mermaid workers busy ({}/{}) - request queued.",
                                        inflight,
                                        Self::MAX_MERMAID_JOBS
                                    ))
                                    .color(Color32::from_rgb(200, 160, 80))
                                    .family(egui::FontFamily::Monospace)
                                    .size(code_font_size),
                                );
                            } else {
                                ui.label(
                                    RichText::new("Rendering diagram locally...")
                                        .color(Color32::from_rgb(160, 200, 240)),
                                );
                            }
                        });
                    return true;
                }
            }
        }
        false
    }

    #[cfg(any(test, feature = "mermaid-quickjs"))]
    fn width_bucket(available_width: f32) -> u32 {
        let step = Self::MERMAID_WIDTH_BUCKET_STEP;
        let width = available_width.max(1.0).ceil() as u32;
        width.div_ceil(step) * step
    }

    #[cfg(any(test, feature = "mermaid-quickjs"))]
    fn scale_bucket(ui_scale: f32) -> u32 {
        let clamped = ui_scale.clamp(0.5, 4.0);
        (clamped * Self::MERMAID_SCALE_BUCKET_FACTOR).round() as u32
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn viewport_px(ctx: &egui::Context) -> (u32, u32) {
        let screen_rect = ctx.input(|i| i.screen_rect());
        let pixels_per_point = ctx.input(|i| i.pixels_per_point).max(0.1);
        let width = (screen_rect.width() * pixels_per_point).round().max(1.0) as u32;
        let height = (screen_rect.height() * pixels_per_point).round().max(1.0) as u32;
        (width, height)
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn scale_from_bucket(scale_bucket: u32) -> f32 {
        let scale = scale_bucket as f32 / Self::MERMAID_SCALE_BUCKET_FACTOR;
        scale.clamp(0.5, 4.0)
    }

    #[cfg(any(test, feature = "mermaid-quickjs"))]
    fn mermaid_bg_key(bg: Option<[u8; 4]>) -> String {
        match bg {
            Some([r, g, b, a]) => format!("{:02x}{:02x}{:02x}{:02x}", r, g, b, a),
            None => "none".to_string(),
        }
    }

    #[cfg(any(test, feature = "mermaid-quickjs"))]
    fn texture_key(
        svg_key: u64,
        width_bucket: u32,
        scale_bucket: u32,
        bg: Option<[u8; 4]>,
    ) -> String {
        let bg_key = Self::mermaid_bg_key(bg);
        format!(
            "mermaid:{:016x}:w{}:s{}:bg{}",
            svg_key, width_bucket, scale_bucket, bg_key
        )
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn poll_mermaid_results(&self, ctx: &egui::Context) -> bool {
        let mut changed = false;
        while let Ok(result) = self.mermaid_result_rx.try_recv() {
            let MermaidResult {
                svg_key,
                texture_key,
                svg,
                rgba,
                size,
                error,
            } = result;
            let has_svg = svg.is_some();
            self.mermaid_pending.borrow_mut().remove(&texture_key);

            if let Some(svg) = svg {
                self.mermaid_svg_cache.borrow_mut().insert(svg_key, svg);
                self.mermaid_errors.borrow_mut().remove(&svg_key);
            }

            if let Some(rgba) = rgba {
                if let Some((w, h)) = size {
                    let img =
                        egui::ColorImage::from_rgba_unmultiplied([w as usize, h as usize], &rgba);
                    let tex =
                        ctx.load_texture(texture_key.clone(), img, egui::TextureOptions::LINEAR);
                    self.store_mermaid_texture(&texture_key, tex.clone(), [w, h]);
                    self.mermaid_texture_errors
                        .borrow_mut()
                        .remove(&texture_key);
                } else {
                    self.mermaid_texture_errors.borrow_mut().insert(
                        texture_key.clone(),
                        "Mermaid rasterization failed (missing size)".to_string(),
                    );
                }
            }

            if let Some(err) = error {
                if !has_svg {
                    self.mermaid_errors.borrow_mut().insert(svg_key, err);
                } else {
                    self.mermaid_texture_errors
                        .borrow_mut()
                        .insert(texture_key, err);
                }
            }

            changed = true;
        }
        changed
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn enqueue_mermaid_job(&self, request: MermaidRequest) -> Result<(), MermaidEnqueueError> {
        match self.mermaid_job_tx.try_send(request) {
            Ok(()) => Ok(()),
            Err(TrySendError::Full(_)) => Err(MermaidEnqueueError::QueueFull),
            Err(TrySendError::Disconnected(_)) => Err(MermaidEnqueueError::Disconnected),
        }
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn mermaid_worker_count() -> usize {
        if let Ok(raw) = std::env::var("MDMDVIEW_MERMAID_WORKERS") {
            if let Ok(value) = raw.trim().parse::<usize>() {
                return value.clamp(1, 16);
            }
        }
        let default = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);
        default.min(Self::MAX_MERMAID_JOBS.max(1))
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn mermaid_timeout_ms() -> u64 {
        if let Ok(raw) = std::env::var("MDMDVIEW_MERMAID_TIMEOUT_MS") {
            if let Ok(value) = raw.trim().parse::<u64>() {
                return value.max(100);
            }
        }
        12_000
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn spawn_mermaid_workers(job_rx: MermaidJobReceiver, result_tx: MermaidResultSender) {
        let worker_count = Self::mermaid_worker_count();
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        for worker_idx in 0..worker_count {
            let worker_rx = job_rx.clone();
            let worker_tx = result_tx.clone();
            let worker_fontdb = Arc::clone(&fontdb);
            if let Err(err) = std::thread::Builder::new()
                .name(format!("mdmdview-mermaid-{worker_idx}"))
                .spawn(move || {
                    let mut worker = MermaidWorker::new(worker_idx, worker_fontdb).map_err(|e| {
                        eprintln!("Mermaid worker init failed: {}", e);
                        e
                    });
                    for job in worker_rx.iter() {
                        let payload = match &mut worker {
                            Ok(state) => state.process_job(job),
                            Err(err) => {
                                let MermaidRequest {
                                    svg_key,
                                    texture_key,
                                    svg,
                                    ..
                                } = job;
                                MermaidResult {
                                    svg_key,
                                    texture_key,
                                    svg,
                                    rgba: None,
                                    size: None,
                                    error: Some(err.clone()),
                                }
                            }
                        };
                        let _ = worker_tx.send(payload);
                    }
                })
            {
                eprintln!("Failed to start Mermaid worker thread: {}", err);
            }
        }
        drop(job_rx);
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn store_mermaid_texture(&self, key: &str, texture: egui::TextureHandle, size: [u32; 2]) {
        self.mermaid_textures
            .borrow_mut()
            .insert(key.to_string(), MermaidTextureEntry { texture, size });
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn mermaid_theme_values() -> MermaidThemeValues {
        let def_main_bkg = "#FFF8DB";
        let def_primary = "#D7EEFF";
        let def_primary_border = "#9BB2C8";
        let def_primary_text = "#1C2430";
        let def_secondary = "#DFF5E1";
        let def_tertiary = "#E9E2FF";
        let def_line = "#6B7A90";
        let def_text = "#1C2430";
        let def_cluster_bkg = "#FFF1C1";
        let def_cluster_border = "#E5C07B";
        let def_default_link = def_line;
        let def_title = def_text;
        let def_label_bg = def_main_bkg;
        let def_edge_label_bg = def_main_bkg;

        let theme_name =
            std::env::var("MDMDVIEW_MERMAID_THEME").unwrap_or_else(|_| "base".to_string());
        let main_bkg =
            std::env::var("MDMDVIEW_MERMAID_MAIN_BKG").unwrap_or_else(|_| def_main_bkg.to_string());
        let primary = std::env::var("MDMDVIEW_MERMAID_PRIMARY_COLOR")
            .unwrap_or_else(|_| def_primary.to_string());
        let primary_border = std::env::var("MDMDVIEW_MERMAID_PRIMARY_BORDER")
            .unwrap_or_else(|_| def_primary_border.to_string());
        let primary_text = std::env::var("MDMDVIEW_MERMAID_PRIMARY_TEXT")
            .unwrap_or_else(|_| def_primary_text.to_string());
        let secondary = std::env::var("MDMDVIEW_MERMAID_SECONDARY_COLOR")
            .unwrap_or_else(|_| def_secondary.to_string());
        let tertiary = std::env::var("MDMDVIEW_MERMAID_TERTIARY_COLOR")
            .unwrap_or_else(|_| def_tertiary.to_string());
        let line =
            std::env::var("MDMDVIEW_MERMAID_LINE_COLOR").unwrap_or_else(|_| def_line.to_string());
        let text =
            std::env::var("MDMDVIEW_MERMAID_TEXT_COLOR").unwrap_or_else(|_| def_text.to_string());
        let cluster_bkg = std::env::var("MDMDVIEW_MERMAID_CLUSTER_BKG")
            .unwrap_or_else(|_| def_cluster_bkg.to_string());
        let cluster_border = std::env::var("MDMDVIEW_MERMAID_CLUSTER_BORDER")
            .unwrap_or_else(|_| def_cluster_border.to_string());
        let default_link = std::env::var("MDMDVIEW_MERMAID_LINK_COLOR")
            .unwrap_or_else(|_| def_default_link.to_string());
        let title =
            std::env::var("MDMDVIEW_MERMAID_TITLE_COLOR").unwrap_or_else(|_| def_title.to_string());
        let label_bg =
            std::env::var("MDMDVIEW_MERMAID_LABEL_BG").unwrap_or_else(|_| def_label_bg.to_string());
        let edge_label_bg = std::env::var("MDMDVIEW_MERMAID_EDGE_LABEL_BG")
            .unwrap_or_else(|_| def_edge_label_bg.to_string());
        let font_family = std::env::var("MDMDVIEW_MERMAID_FONT_FAMILY")
            .ok()
            .map(|value| value.trim().to_string())
            .filter(|value| !value.is_empty());

        MermaidThemeValues {
            theme_name,
            main_bkg,
            primary,
            primary_border,
            primary_text,
            secondary,
            tertiary,
            line,
            text,
            cluster_bkg,
            cluster_border,
            default_link,
            title,
            label_bg,
            edge_label_bg,
            font_family,
        }
    }

    #[cfg(any(test, feature = "mermaid-quickjs"))]
    fn mermaid_security_level() -> String {
        if let Ok(raw) = std::env::var("MDMDVIEW_MERMAID_SECURITY") {
            let normalized = raw.trim().to_ascii_lowercase();
            if normalized == "loose" {
                return "loose".to_string();
            }
        }
        "strict".to_string()
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn mermaid_site_config_json(svg_key: u64) -> String {
        let theme = Self::mermaid_theme_values();
        let security = Self::mermaid_security_level();
        let seed = format!("m{:016x}", svg_key);
        let mut entries = Vec::new();

        entries.push("\"startOnLoad\":false".to_string());
        entries.push(format!(
            "\"securityLevel\":\"{}\"",
            Self::json_escape(&security)
        ));
        entries.push("\"deterministicIds\":true".to_string());
        entries.push(format!(
            "\"deterministicIDSeed\":\"{}\"",
            Self::json_escape(&seed)
        ));
        entries.push(format!("\"maxTextSize\":{}", Self::MERMAID_MAX_TEXT_SIZE));
        if security == "strict" {
            entries.push("\"htmlLabels\":false".to_string());
            entries.push("\"flowchart\":{\"htmlLabels\":false}".to_string());
        }
        entries.push(format!(
            "\"theme\":\"{}\"",
            Self::json_escape(&theme.theme_name)
        ));

        let mut theme_vars = format!(
            concat!(
                "\"background\":\"{}\",",
                "\"mainBkg\":\"{}\",",
                "\"textColor\":\"{}\",",
                "\"titleColor\":\"{}\",",
                "\"primaryColor\":\"{}\",",
                "\"primaryBorderColor\":\"{}\",",
                "\"primaryTextColor\":\"{}\",",
                "\"secondaryColor\":\"{}\",",
                "\"tertiaryColor\":\"{}\",",
                "\"lineColor\":\"{}\",",
                "\"defaultLinkColor\":\"{}\",",
                "\"clusterBkg\":\"{}\",",
                "\"clusterBorder\":\"{}\",",
                "\"labelBackground\":\"{}\",",
                "\"edgeLabelBackground\":\"{}\""
            ),
            Self::json_escape(&theme.main_bkg),
            Self::json_escape(&theme.main_bkg),
            Self::json_escape(&theme.text),
            Self::json_escape(&theme.title),
            Self::json_escape(&theme.primary),
            Self::json_escape(&theme.primary_border),
            Self::json_escape(&theme.primary_text),
            Self::json_escape(&theme.secondary),
            Self::json_escape(&theme.tertiary),
            Self::json_escape(&theme.line),
            Self::json_escape(&theme.default_link),
            Self::json_escape(&theme.cluster_bkg),
            Self::json_escape(&theme.cluster_border),
            Self::json_escape(&theme.label_bg),
            Self::json_escape(&theme.edge_label_bg)
        );
        if let Some(font_family) = theme.font_family.as_ref() {
            theme_vars.push_str(&format!(
                ",\"fontFamily\":\"{}\"",
                Self::json_escape(font_family)
            ));
        }
        entries.push(format!("\"themeVariables\":{{{}}}", theme_vars));

        format!("{{{}}}", entries.join(","))
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn json_escape(input: &str) -> String {
        let mut out = String::with_capacity(input.len());
        for ch in input.chars() {
            match ch {
                '\\' => out.push_str("\\\\"),
                '"' => out.push_str("\\\""),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\t' => out.push_str("\\t"),
                _ => out.push(ch),
            }
        }
        out
    }

    #[cfg(any(test, feature = "mermaid-quickjs"))]
    fn mermaid_bg_fill() -> Option<[u8; 4]> {
        if let Ok(hex) = std::env::var("MDMDVIEW_MERMAID_BG_COLOR") {
            if let Some(rgba) = Self::parse_hex_color(&hex) {
                return Some(rgba);
            }
        }
        let mode = std::env::var("MDMDVIEW_MERMAID_BG").unwrap_or_else(|_| "theme".to_string());
        match mode.as_str() {
            "transparent" => None,
            "dark" => Some([20, 20, 20, 255]),
            "light" => Some([255, 255, 255, 255]),
            _ => {
                let hex = std::env::var("MDMDVIEW_MERMAID_MAIN_BKG")
                    .unwrap_or_else(|_| "#FFF8DB".to_string());
                Self::parse_hex_color(&hex).or(Some([255, 248, 219, 255]))
            }
        }
    }

    #[cfg(any(test, feature = "mermaid-quickjs"))]
    fn parse_hex_color(s: &str) -> Option<[u8; 4]> {
        let t = s.trim();
        let hex = t.strip_prefix('#').unwrap_or(t);
        let (r, g, b, a) = match hex.len() {
            6 => (
                u8::from_str_radix(&hex[0..2], 16).ok()?,
                u8::from_str_radix(&hex[2..4], 16).ok()?,
                u8::from_str_radix(&hex[4..6], 16).ok()?,
                255,
            ),
            8 => (
                u8::from_str_radix(&hex[0..2], 16).ok()?,
                u8::from_str_radix(&hex[2..4], 16).ok()?,
                u8::from_str_radix(&hex[4..6], 16).ok()?,
                u8::from_str_radix(&hex[6..8], 16).ok()?,
            ),
            _ => return None,
        };
        Some([r, g, b, a])
    }

    #[cfg(any(test, feature = "mermaid-quickjs"))]
    fn hash_str(s: &str) -> u64 {
        let mut h = DefaultHasher::new();
        s.hash(&mut h);
        h.finish()
    }
}

#[cfg(feature = "mermaid-quickjs")]
struct TextMeasurer {
    fontdb: Arc<usvg::fontdb::Database>,
    face_id: Option<usvg::fontdb::ID>,
}

#[cfg(feature = "mermaid-quickjs")]
impl TextMeasurer {
    fn new(fontdb: Arc<usvg::fontdb::Database>) -> Self {
        let families = [
            usvg::fontdb::Family::Name("Trebuchet MS"),
            usvg::fontdb::Family::Name("Verdana"),
            usvg::fontdb::Family::Name("Arial"),
            usvg::fontdb::Family::SansSerif,
        ];
        let query = usvg::fontdb::Query {
            families: &families,
            ..Default::default()
        };
        let face_id = fontdb.query(&query);
        Self { fontdb, face_id }
    }

    fn measure_text(&self, text: &str, font_size: f32) -> (f32, f32) {
        let size = if font_size.is_finite() && font_size > 0.0 {
            font_size
        } else {
            16.0
        };
        if let Some(result) = self.measure_with_face(text, size) {
            return result;
        }
        Self::fallback_measure(text, size)
    }

    fn measure_with_face(&self, text: &str, font_size: f32) -> Option<(f32, f32)> {
        let face_id = self.face_id?;
        self.fontdb
            .with_face_data(face_id, |data, index| {
                let face = ttf_parser::Face::parse(data, index).ok()?;
                let units_per_em = face.units_per_em() as f32;
                if !units_per_em.is_finite() || units_per_em <= 0.0 {
                    return None;
                }
                let scale = font_size / units_per_em;
                let fallback_advance = units_per_em * 0.5;
                let mut max_width_units = 0.0_f32;
                let mut line_count = 0u32;

                for line in text.split('\n') {
                    line_count += 1;
                    let mut width_units = 0.0_f32;
                    for ch in line.chars() {
                        if let Some(glyph) = face.glyph_index(ch) {
                            if let Some(adv) = face.glyph_hor_advance(glyph) {
                                width_units += adv as f32;
                            } else {
                                width_units += fallback_advance;
                            }
                        } else {
                            width_units += fallback_advance;
                        }
                    }
                    if width_units > max_width_units {
                        max_width_units = width_units;
                    }
                }

                if line_count == 0 {
                    line_count = 1;
                }

                let width = max_width_units * scale;
                let height = font_size * 0.72 * line_count as f32;
                if width.is_finite() && height.is_finite() {
                    Some((width, height))
                } else {
                    None
                }
            })
            .flatten()
    }

    fn fallback_measure(text: &str, font_size: f32) -> (f32, f32) {
        let mut max_len = 0usize;
        let mut lines = 0usize;
        for line in text.split('\n') {
            lines += 1;
            let len = line.chars().count();
            if len > max_len {
                max_len = len;
            }
        }
        if lines == 0 {
            lines = 1;
        }
        let width = max_len as f32 * font_size * 0.5;
        let height = lines as f32 * font_size * 0.72;
        (width, height)
    }
}

#[cfg(feature = "mermaid-quickjs")]
struct MermaidWorker {
    engine: MermaidEngine,
    deadline_ms: Arc<AtomicU64>,
    fontdb: Arc<usvg::fontdb::Database>,
    #[allow(dead_code)]
    text_measurer: Arc<TextMeasurer>,
}

#[cfg(feature = "mermaid-quickjs")]
impl MermaidWorker {
    const MEMORY_LIMIT_BYTES: usize = 2 * 1024 * 1024 * 1024;
    const STACK_LIMIT_BYTES: usize = 4 * 1024 * 1024;

    // DOMPurify expects a real browser DOM; stub sanitize for the QuickJS shim.
    fn patch_mermaid_js(js: &str) -> String {
        const TARGET: &str = "var hD=wRe();";
        const PATCH: &str =
            "var hD=wRe();if(!hD||typeof hD.sanitize!==\"function\"){hD={sanitize:function(html){return String(html);},addHook:function(){},removeHook:function(){},removeHooks:function(){},removeAllHooks:function(){},isSupported:true};}";
        const D3_TARGET: &str = "FY.prototype={constructor:FY,appendChild:function(i){return this._parent.insertBefore(i,this._next)},insertBefore:function(i,s){return this._parent.insertBefore(i,s)},querySelector:function(i){return this._parent.querySelector(i)},querySelectorAll:function(i){return this._parent.querySelectorAll(i)}};";
        const D3_PATCH: &str = "FY.prototype={constructor:FY,appendChild:function(i){var p=this._parent||((typeof document!==\"undefined\"&&document.body)?document.body:null);return p?p.insertBefore(i,this._next):i},insertBefore:function(i,s){var p=this._parent||((typeof document!==\"undefined\"&&document.body)?document.body:null);return p?p.insertBefore(i,s):i},querySelector:function(i){var p=this._parent||((typeof document!==\"undefined\"&&document.body)?document.body:null);return p&&p.querySelector?p.querySelector(i):null},querySelectorAll:function(i){var p=this._parent||((typeof document!==\"undefined\"&&document.body)?document.body:null);return p&&p.querySelectorAll?p.querySelectorAll(i):[]}};";
        const D3_CTOR_TARGET: &str =
            "function FY(i,s){this.ownerDocument=i.ownerDocument,this.namespaceURI=i.namespaceURI,this._next=null,this._parent=i,this.__data__=s}";
        const D3_CTOR_PATCH: &str =
            "function FY(i,s){i=i||((typeof document!==\"undefined\"&&document.body)?document.body:null);this.ownerDocument=i&&i.ownerDocument?i.ownerDocument:(typeof document!==\"undefined\"?document:null);this.namespaceURI=i&&i.namespaceURI?i.namespaceURI:null;this._next=null;this._parent=i;this.__data__=s}";
        const TEXT_WRAP_TARGET: &str = "u.text().split(/(\\s+|<br>)/).reverse()";
        const TEXT_WRAP_PATCH: &str = concat!(
            "String(typeof u.text===\"function\"?u.text():(u.textContent||\"\"))",
            ".split(/(\\s+|<br>)/).reverse()"
        );
        const MINDMAP_LAYOUT_TARGET: &str =
            "p.layout({name:\"cose-bilkent\",quality:\"proof\",styleEnabled:!1,animate:!1}).run()";
        const MINDMAP_LAYOUT_PATCH: &str =
            "p.layout({name:\"breadthfirst\",directed:!0,spacingFactor:1.4,animate:!1}).run()";
        let mut out = if js.contains(TARGET) {
            js.replacen(TARGET, PATCH, 1)
        } else {
            js.to_string()
        };
        let debug = std::env::var("MDMDVIEW_MERMAID_PATCH_DEBUG").is_ok();
        if out.contains(D3_CTOR_TARGET) {
            out = out.replacen(D3_CTOR_TARGET, D3_CTOR_PATCH, 1);
            if debug {
                eprintln!("Mermaid patch: D3 ctor applied");
            }
        } else if debug {
            eprintln!("Mermaid patch: D3 ctor not found");
        }
        if out.contains(D3_TARGET) {
            out = out.replacen(D3_TARGET, D3_PATCH, 1);
            if debug {
                eprintln!("Mermaid patch: D3 enter applied");
            }
        } else if debug {
            eprintln!("Mermaid patch: D3 enter not found");
        }
        if out.contains(TEXT_WRAP_TARGET) {
            out = out.replacen(TEXT_WRAP_TARGET, TEXT_WRAP_PATCH, 1);
            if debug {
                eprintln!("Mermaid patch: D3 text wrap applied");
            }
        } else if debug {
            eprintln!("Mermaid patch: D3 text wrap not found");
        }
        if out.contains(MINDMAP_LAYOUT_TARGET) {
            out = out.replacen(MINDMAP_LAYOUT_TARGET, MINDMAP_LAYOUT_PATCH, 1);
            if debug {
                eprintln!("Mermaid patch: mindmap layout applied");
            }
        } else if debug {
            eprintln!("Mermaid patch: mindmap layout not found");
        }
        out
    }

    fn format_js_error(ctx: &rquickjs::Ctx<'_>, err: rquickjs::Error) -> String {
        if let rquickjs::Error::Exception = err {
            let value = ctx.catch();
            if let Some(exception) = value.as_exception() {
                let message = exception.message();
                let stack = exception.stack();
                if let Some(stack) = stack {
                    if let Some(message) = message {
                        if stack.contains(&message) {
                            return stack;
                        }
                        if !stack.trim().is_empty() {
                            return format!("{}\n{}", message, stack);
                        }
                        if !message.trim().is_empty() {
                            return message;
                        }
                    } else if !stack.trim().is_empty() {
                        return stack;
                    }
                } else if let Some(message) = message {
                    if !message.trim().is_empty() {
                        return message;
                    }
                }
            }
            if let Some(js_string) = value.as_string().and_then(|s| s.to_string().ok()) {
                return js_string;
            }
            if let Some(number) = value.as_int() {
                return number.to_string();
            }
            if let Some(number) = value.as_float() {
                return number.to_string();
            }
            if let Some(flag) = value.as_bool() {
                return flag.to_string();
            }
            return format!("{:?}", value);
        }
        err.to_string()
    }

    fn new(worker_idx: usize, fontdb: Arc<usvg::fontdb::Database>) -> Result<Self, String> {
        use rquickjs::{Context, Function, Runtime};
        if mermaid_embed::MERMAID_JS.is_empty() {
            return Err("No embedded Mermaid JS".to_string());
        }
        let rt = Runtime::new().map_err(|e| format!("Mermaid runtime init error: {}", e))?;
        rt.set_memory_limit(Self::MEMORY_LIMIT_BYTES);
        rt.set_max_stack_size(Self::STACK_LIMIT_BYTES);
        let deadline_ms = Arc::new(AtomicU64::new(0));
        let deadline_guard = Arc::clone(&deadline_ms);
        rt.set_interrupt_handler(Some(Box::new(move || {
            let deadline = deadline_guard.load(Ordering::Relaxed);
            if deadline == 0 {
                return false;
            }
            Self::now_ms() >= deadline
        })));
        let ctx = Context::full(&rt).map_err(|e| format!("Mermaid context init error: {}", e))?;
        let engine = MermaidEngine { rt, ctx };
        let text_measurer = Arc::new(TextMeasurer::new(Arc::clone(&fontdb)));
        let js = std::str::from_utf8(mermaid_embed::MERMAID_JS)
            .map_err(|_| "Mermaid JS is not valid UTF-8".to_string())?;
        let js = Self::patch_mermaid_js(js);
        let init_result: Result<(), String> = engine.ctx.with(|ctx| {
            let measurer = Arc::clone(&text_measurer);
            let measure_fn = Function::new(ctx.clone(), move |text: String, font_size: f64| {
                let (width, height) = measurer.measure_text(&text, font_size as f32);
                vec![width as f64, height as f64]
            })
            .map_err(|err| {
                format!(
                    "Mermaid text measure init error: {}",
                    MermaidWorker::format_js_error(&ctx, err)
                )
            })?;
            ctx.globals()
                .set("__mdmdview_measure_text_native", measure_fn)
                .map_err(|err| {
                    format!(
                        "Mermaid text measure init error: {}",
                        MermaidWorker::format_js_error(&ctx, err)
                    )
                })?;
            let eval = |label: &str, source: &str| -> Result<(), String> {
                ctx.eval::<(), _>(source).map_err(|err| {
                    format!("{}: {}", label, MermaidWorker::format_js_error(&ctx, err))
                })
            };
            eval("Mermaid DOM shim", MERMAID_DOM_SHIM)?;
            eval("Mermaid JS", &js)?;
            eval("Mermaid init", MERMAID_INIT_SNIPPET)?;
            if std::env::var("MDMDVIEW_MERMAID_DOM_DEBUG").is_ok() {
                let dom_ok = ctx
                    .eval::<bool, _>(
                        "var root=document.querySelector('body');var d=document.createElement('div');d.setAttribute('id','__mdmdview_dom_test');root.appendChild(d);var f=document.getElementById('__mdmdview_dom_test');!!(f&&f===d);",
                    )
                    .unwrap_or(false);
                eprintln!("Mermaid DOM debug: {}", dom_ok);
            }
            Ok(())
        });
        if let Err(err) = init_result {
            return Err(format!("Mermaid init error: {}", err));
        }
        if worker_idx == 0 {
            eprintln!(
                "Mermaid embedded bytes: {}",
                mermaid_embed::MERMAID_JS.len()
            );
        }
        Ok(Self {
            engine,
            deadline_ms,
            fontdb,
            text_measurer,
        })
    }

    fn render_svg(
        &mut self,
        key: u64,
        code: &str,
        viewport_width: u32,
        viewport_height: u32,
    ) -> Result<String, String> {
        use rquickjs::{promise::MaybePromise, Function};
        let timeout_ms = MermaidRenderer::mermaid_timeout_ms();
        let deadline = Self::now_ms().saturating_add(timeout_ms);
        self.deadline_ms.store(deadline, Ordering::Relaxed);
        let result = self.engine.ctx.with(|ctx| {
            let wrapper = MERMAID_RENDER_WRAPPER;
            let func: Function = ctx
                .eval(wrapper)
                .map_err(|err| MermaidWorker::format_js_error(&ctx, err))?;
            let id = format!("m{:016x}", key);
            let site_config = MermaidRenderer::mermaid_site_config_json(key);
            let maybe: MaybePromise = func
                .call((id.as_str(), code, site_config.as_str(), viewport_width, viewport_height))
                .map_err(|err| MermaidWorker::format_js_error(&ctx, err))?;
            maybe
                .finish::<String>()
                .map_err(|err| MermaidWorker::format_js_error(&ctx, err))
        });
        self.deadline_ms.store(0, Ordering::Relaxed);
        match result {
            Ok(svg) => Ok(svg),
            Err(err) => {
                if Self::now_ms() >= deadline {
                    return Err("Mermaid render timed out".to_string());
                }
                Err(format!("Mermaid render error: {}", err))
            }
        }
    }

    fn maybe_dump_svg(svg_key: u64, code: Option<&str>, svg: &str) {
        let dir = match std::env::var("MDMDVIEW_MERMAID_DUMP_DIR") {
            Ok(value) if !value.trim().is_empty() => value,
            _ => return,
        };
        let dir = PathBuf::from(dir);
        if std::fs::create_dir_all(&dir).is_err() {
            return;
        }
        let label = code
            .and_then(|snippet| snippet.lines().next())
            .unwrap_or("mermaid");
        let mut name = String::new();
        for ch in label.chars() {
            if name.len() >= 32 {
                break;
            }
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                name.push(ch);
            } else {
                name.push('_');
            }
        }
        if name.is_empty() {
            name.push_str("mermaid");
        }
        let filename = format!("{:016x}_{}.svg", svg_key, name);
        let path = dir.join(filename);
        let _ = std::fs::write(path, svg);
    }

    fn maybe_dump_error(svg_key: u64, code: Option<&str>, err: &str) {
        let dir = match std::env::var("MDMDVIEW_MERMAID_DUMP_DIR") {
            Ok(value) if !value.trim().is_empty() => value,
            _ => return,
        };
        let dir = PathBuf::from(dir);
        if std::fs::create_dir_all(&dir).is_err() {
            return;
        }
        let label = code
            .and_then(|snippet| snippet.lines().next())
            .unwrap_or("mermaid");
        let mut name = String::new();
        for ch in label.chars() {
            if name.len() >= 32 {
                break;
            }
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                name.push(ch);
            } else {
                name.push('_');
            }
        }
        if name.is_empty() {
            name.push_str("mermaid");
        }
        let filename = format!("{:016x}_{}.err.txt", svg_key, name);
        let path = dir.join(filename);
        let mut payload = String::new();
        payload.push_str("error: ");
        payload.push_str(err);
        payload.push('\n');
        if let Some(code) = code {
            payload.push_str("\n---\n");
            payload.push_str(code);
            payload.push('\n');
        }
        let _ = std::fs::write(path, payload);
    }

    fn normalize_svg_size(svg: &str) -> String {
        let start = match svg.find("<svg") {
            Some(idx) => idx,
            None => return svg.to_string(),
        };
        let end = match svg[start..].find('>') {
            Some(idx) => start + idx,
            None => return svg.to_string(),
        };
        let tag = &svg[start..=end];
        let viewbox = match Self::find_svg_attr(tag, "viewBox") {
            Some(v) => v,
            None => return svg.to_string(),
        };
        let dims = Self::parse_viewbox_dims(&viewbox);
        let (width, height) = match dims {
            Some(pair) => pair,
            None => return svg.to_string(),
        };
        let mut new_tag = tag.to_string();
        let width_attr = Self::find_svg_attr(&new_tag, "width");
        if width_attr
            .as_deref()
            .map(|val| val.trim().ends_with('%'))
            .unwrap_or(true)
        {
            let value = Self::format_dim(width);
            new_tag = if width_attr.is_some() {
                Self::replace_svg_attr(&new_tag, "width", &value)
            } else {
                Self::insert_svg_attr(&new_tag, "width", &value)
            };
        }
        let height_attr = Self::find_svg_attr(&new_tag, "height");
        if height_attr
            .as_deref()
            .map(|val| val.trim().ends_with('%'))
            .unwrap_or(true)
        {
            let value = Self::format_dim(height);
            new_tag = if height_attr.is_some() {
                Self::replace_svg_attr(&new_tag, "height", &value)
            } else {
                Self::insert_svg_attr(&new_tag, "height", &value)
            };
        }
        if Self::find_svg_attr(&new_tag, "overflow").is_none() {
            new_tag = Self::insert_svg_attr(&new_tag, "overflow", "hidden");
        }
        if new_tag == tag {
            return svg.to_string();
        }
        let mut out = String::with_capacity(svg.len() + 32);
        out.push_str(&svg[..start]);
        out.push_str(&new_tag);
        out.push_str(&svg[end + 1..]);
        out
    }

    fn find_svg_attr(tag: &str, name: &str) -> Option<String> {
        let needle = format!("{name}=\"");
        let start = tag.find(&needle)? + needle.len();
        let end = tag[start..].find('"')? + start;
        Some(tag[start..end].to_string())
    }

    fn replace_svg_attr(tag: &str, name: &str, value: &str) -> String {
        let needle = format!("{name}=\"");
        let start = match tag.find(&needle) {
            Some(idx) => idx + needle.len(),
            None => return tag.to_string(),
        };
        let end = match tag[start..].find('"') {
            Some(idx) => start + idx,
            None => return tag.to_string(),
        };
        let mut out = String::with_capacity(tag.len() + value.len());
        out.push_str(&tag[..start]);
        out.push_str(value);
        out.push_str(&tag[end..]);
        out
    }

    fn insert_svg_attr(tag: &str, name: &str, value: &str) -> String {
        let insert_pos = match tag.find("<svg") {
            Some(idx) => idx + 4,
            None => return tag.to_string(),
        };
        let mut out = String::with_capacity(tag.len() + name.len() + value.len() + 4);
        out.push_str(&tag[..insert_pos]);
        out.push_str(&format!(" {name}=\"{value}\""));
        out.push_str(&tag[insert_pos..]);
        out
    }

    fn parse_viewbox_dims(viewbox: &str) -> Option<(f32, f32)> {
        let mut nums = Vec::new();
        for part in viewbox
            .split(|c: char| c.is_whitespace() || c == ',')
            .filter(|part| !part.is_empty())
        {
            if let Ok(value) = part.parse::<f32>() {
                nums.push(value);
            }
        }
        if nums.len() < 4 {
            return None;
        }
        let width = nums[2];
        let height = nums[3];
        if width.is_finite() && height.is_finite() && width > 0.0 && height > 0.0 {
            Some((width, height))
        } else {
            None
        }
    }

    fn format_dim(value: f32) -> String {
        let mut out = format!("{:.3}", value);
        while out.contains('.') && out.ends_with('0') {
            out.pop();
        }
        if out.ends_with('.') {
            out.pop();
        }
        if out.is_empty() {
            out.push('0');
        }
        out
    }

    fn process_job(&mut self, job: MermaidRequest) -> MermaidResult {
        let MermaidRequest {
            svg_key,
            texture_key,
            code,
            svg,
            width_bucket,
            scale_bucket,
            viewport_width,
            viewport_height,
            bg,
        } = job;
        let code_ref = code.as_deref();
        let svg_result = match svg {
            Some(svg) => Ok(svg),
            None => match code_ref {
                Some(code) => self.render_svg(svg_key, code, viewport_width, viewport_height),
                None => Err("Mermaid render request missing code".to_string()),
            },
        };

        match svg_result {
            Ok(svg) => {
                let svg = Self::normalize_svg_size(&svg);
                Self::maybe_dump_svg(svg_key, code_ref, &svg);
                match self.rasterize_svg(&svg, width_bucket, scale_bucket, bg) {
                    Ok((rgba, w, h)) => MermaidResult {
                        svg_key,
                        texture_key,
                        svg: Some(svg),
                        rgba: Some(rgba),
                        size: Some((w, h)),
                        error: None,
                    },
                    Err(err) => {
                        Self::maybe_dump_error(svg_key, code_ref, &err);
                        MermaidResult {
                            svg_key,
                            texture_key,
                            svg: Some(svg),
                            rgba: None,
                            size: None,
                            error: Some(err),
                        }
                    }
                }
            }
            Err(err) => {
                Self::maybe_dump_error(svg_key, code_ref, &err);
                MermaidResult {
                    svg_key,
                    texture_key,
                    svg: None,
                    rgba: None,
                    size: None,
                    error: Some(err),
                }
            }
        }
    }

    fn rasterize_svg(
        &self,
        svg: &str,
        width_bucket: u32,
        scale_bucket: u32,
        bg: Option<[u8; 4]>,
    ) -> Result<(Vec<u8>, u32, u32), String> {
        let opt = usvg::Options {
            resources_dir: None,
            fontdb: Arc::clone(&self.fontdb),
            image_href_resolver: Self::image_href_resolver(),
            ..Default::default()
        };

        let tree = usvg::Tree::from_data(svg.as_bytes(), &opt).map_err(|e| format!("{}", e))?;
        let sz = tree.size().to_int_size();
        let (mut w, mut h) = (sz.width(), sz.height());
        if w == 0 || h == 0 {
            w = 256;
            h = 256;
        }

        let base_scale = MermaidRenderer::scale_from_bucket(scale_bucket);
        let mut scale = base_scale;
        if width_bucket > 0 {
            let width_scale = width_bucket as f32 / w.max(1) as f32;
            if width_scale.is_finite() {
                scale = scale.min(width_scale);
            }
        }
        scale = scale.clamp(0.01, 4.0);

        let mut target_w = (w as f32 * scale).round() as u32;
        let mut target_h = (h as f32 * scale).round() as u32;
        if target_w == 0 || target_h == 0 {
            target_w = 1;
            target_h = 1;
        }

        let max_side = MermaidRenderer::MERMAID_MAX_RENDER_SIDE;
        if target_w > max_side || target_h > max_side {
            let clamp_scale =
                (max_side as f32 / target_w as f32).min(max_side as f32 / target_h as f32);
            scale = (scale * clamp_scale).clamp(0.01, 4.0);
            target_w = (w as f32 * scale).round() as u32;
            target_h = (h as f32 * scale).round() as u32;
        }

        let mut pixmap = tiny_skia::Pixmap::new(target_w, target_h)
            .ok_or_else(|| "Pixmap alloc failed".to_string())?;
        if let Some([r, g, b, a]) = bg {
            let color = tiny_skia::Color::from_rgba8(r, g, b, a);
            pixmap.fill(color);
        }

        let mut pmut = pixmap.as_mut();
        let transform = tiny_skia::Transform::from_scale(scale, scale);
        resvg::render(&tree, transform, &mut pmut);
        let data = pixmap.data().to_vec();
        Ok((data, target_w, target_h))
    }

    fn image_href_resolver() -> usvg::ImageHrefResolver<'static> {
        let resolve_data = usvg::ImageHrefResolver::default_data_resolver();
        let resolve_string = Box::new(|_: &str, _: &usvg::Options| None);
        usvg::ImageHrefResolver {
            resolve_data,
            resolve_string,
        }
    }

    fn now_ms() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as u64
    }
}

#[cfg(feature = "mermaid-quickjs")]
const MERMAID_DOM_SHIM: &str = r#"
var window = globalThis;
var __mdmdview_text_cache = {};
function __mdmdview_measure_text(text, fontSize) {
  var size = fontSize || 16;
  var raw = text ? String(text) : '';
  var key = size + '|' + raw;
  var hit = __mdmdview_text_cache[key];
  if (hit) { return hit; }
  if (typeof __mdmdview_measure_text_native === 'function') {
    try {
      var native = __mdmdview_measure_text_native(raw, size);
      if (native && typeof native.width === 'number' && typeof native.height === 'number') {
        __mdmdview_text_cache[key] = native;
        return native;
      }
      if (native && typeof native.length === 'number' && native.length >= 2) {
        var w = Number(native[0]);
        var h = Number(native[1]);
        if (isFinite(w) && isFinite(h)) {
          var converted = { width: w, height: h };
          __mdmdview_text_cache[key] = converted;
          return converted;
        }
      }
    } catch (e) {}
  }
  var lines = raw.split(/\n/);
  var maxLen = 0;
  for (var i = 0; i < lines.length; i++) {
    if (lines[i].length > maxLen) { maxLen = lines[i].length; }
  }
  var width = maxLen * size * 0.5;
  var height = lines.length * size * 0.72;
  var res = { width: width, height: height };
  __mdmdview_text_cache[key] = res;
  return res;
}
function __mdmdview_get_font_size(el) {
  var size = 16;
  if (el && el.style && el.style.fontSize) {
    var s = parseFloat(el.style.fontSize);
    if (!isNaN(s)) { size = s; }
  }
  if (el && el.getAttribute) {
    var attr = el.getAttribute('font-size');
    if (attr) {
      var a = parseFloat(attr);
      if (!isNaN(a)) { size = a; }
    }
  }
  return size;
}
function __mdmdview_parse_num(value) {
  var num = parseFloat(value);
  return isNaN(num) ? 0 : num;
}
function __mdmdview_parse_length(value, fontSize) {
  if (value === null || value === undefined) { return 0; }
  var raw = String(value).trim();
  if (!raw) { return 0; }
  var num = parseFloat(raw);
  if (isNaN(num)) { return 0; }
  if (raw.indexOf('em') >= 0) {
    return num * (fontSize || 16);
  }
  return num;
}
function __mdmdview_transform_point(el, x, y) {
  var box = __mdmdview_apply_transform(el, { x: x, y: y, width: 0, height: 0 });
  return { x: box.x, y: box.y };
}
function __mdmdview_path_points(el) {
  var d = el && el.getAttribute ? el.getAttribute('d') : null;
  if (!d) { return []; }
  var nums = String(d).match(/-?\d*\.?\d+(?:e[-+]?\d+)?/ig);
  if (!nums || nums.length < 2) { return []; }
  var points = [];
  for (var i = 0; i + 1 < nums.length; i += 2) {
    var x = __mdmdview_parse_num(nums[i]);
    var y = __mdmdview_parse_num(nums[i + 1]);
    points.push(__mdmdview_transform_point(el, x, y));
  }
  return points;
}
function __mdmdview_path_total_length(el) {
  var points = __mdmdview_path_points(el);
  if (points.length < 2) { return 0; }
  var total = 0;
  for (var i = 1; i < points.length; i++) {
    var dx = points[i].x - points[i - 1].x;
    var dy = points[i].y - points[i - 1].y;
    total += Math.sqrt(dx * dx + dy * dy);
  }
  return total;
}
function __mdmdview_path_point_at_length(el, length) {
  var points = __mdmdview_path_points(el);
  if (!points.length) { return { x: 0, y: 0 }; }
  if (points.length === 1) { return points[0]; }
  var remaining = Math.max(0, __mdmdview_parse_num(length));
  for (var i = 1; i < points.length; i++) {
    var p0 = points[i - 1];
    var p1 = points[i];
    var dx = p1.x - p0.x;
    var dy = p1.y - p0.y;
    var seg = Math.sqrt(dx * dx + dy * dy);
    if (seg <= 0) { continue; }
    if (remaining <= seg) {
      var t = remaining / seg;
      return { x: p0.x + dx * t, y: p0.y + dy * t };
    }
    remaining -= seg;
  }
  return points[points.length - 1];
}
function __mdmdview_text_metrics(el) {
  var size = __mdmdview_get_font_size(el);
  if (!el || !el.children || !el.children.length) {
    var raw = el ? __mdmdview_collect_text(el) : '';
    return __mdmdview_measure_text(raw, size);
  }
  var max_w = 0;
  var max_h = 0;
  var lines = 0;
  for (var i = 0; i < el.children.length; i++) {
    var child = el.children[i];
    var tag = (child.tagName || '').toLowerCase();
    if (tag !== 'tspan') { continue; }
    var text = __mdmdview_collect_text(child);
    var m = __mdmdview_measure_text(text, size);
    if (m.width > max_w) { max_w = m.width; }
    if (m.height > max_h) { max_h = m.height; }
    lines += 1;
  }
  if (!lines) {
    var fallback = __mdmdview_collect_text(el);
    return __mdmdview_measure_text(fallback, size);
  }
  var line_height = max_h > 0 ? max_h : size * 1.2;
  return { width: max_w, height: lines * line_height };
}
function __mdmdview_apply_matrix(bbox, a, b, c, d, e, f) {
  if (!bbox) { return bbox; }
  var x = bbox.x;
  var y = bbox.y;
  var w = bbox.width;
  var h = bbox.height;
  var pts = [
    [x, y],
    [x + w, y],
    [x, y + h],
    [x + w, y + h]
  ];
  var min_x = Infinity;
  var min_y = Infinity;
  var max_x = -Infinity;
  var max_y = -Infinity;
  for (var i = 0; i < pts.length; i++) {
    var px = pts[i][0];
    var py = pts[i][1];
    var nx = a * px + c * py + e;
    var ny = b * px + d * py + f;
    if (nx < min_x) { min_x = nx; }
    if (ny < min_y) { min_y = ny; }
    if (nx > max_x) { max_x = nx; }
    if (ny > max_y) { max_y = ny; }
  }
  if (!isFinite(min_x) || !isFinite(min_y) || !isFinite(max_x) || !isFinite(max_y)) {
    return bbox;
  }
  return { x: min_x, y: min_y, width: max_x - min_x, height: max_y - min_y };
}
function __mdmdview_apply_transform(el, bbox) {
  if (!el || !bbox) { return bbox; }
  var transform = el.getAttribute ? el.getAttribute('transform') : null;
  if (!transform) { return bbox; }
  var current = bbox;
  var re = /([a-zA-Z]+)\(([^)]+)\)/g;
  var match = null;
  while ((match = re.exec(String(transform))) !== null) {
    var name = match[1].toLowerCase();
    var params = match[2].split(/[, ]+/).filter(function(p) { return p.length; });
    var nums = [];
    for (var i = 0; i < params.length; i++) {
      nums.push(__mdmdview_parse_num(params[i]));
    }
    if (name === 'translate') {
      var tx = nums.length ? nums[0] : 0;
      var ty = nums.length > 1 ? nums[1] : 0;
      current = __mdmdview_apply_matrix(current, 1, 0, 0, 1, tx, ty);
    } else if (name === 'scale') {
      var sx = nums.length ? nums[0] : 1;
      var sy = nums.length > 1 ? nums[1] : sx;
      current = __mdmdview_apply_matrix(current, sx, 0, 0, sy, 0, 0);
    } else if (name === 'matrix' && nums.length >= 6) {
      current = __mdmdview_apply_matrix(
        current,
        nums[0],
        nums[1],
        nums[2],
        nums[3],
        nums[4],
        nums[5]
      );
    } else if (name === 'rotate' && nums.length) {
      var angle = nums[0] * Math.PI / 180.0;
      var cos = Math.cos(angle);
      var sin = Math.sin(angle);
      if (nums.length >= 3) {
        var cx = nums[1];
        var cy = nums[2];
        current = __mdmdview_apply_matrix(current, 1, 0, 0, 1, -cx, -cy);
        current = __mdmdview_apply_matrix(current, cos, sin, -sin, cos, 0, 0);
        current = __mdmdview_apply_matrix(current, 1, 0, 0, 1, cx, cy);
      } else {
        current = __mdmdview_apply_matrix(current, cos, sin, -sin, cos, 0, 0);
      }
    } else if (name === 'skewx' && nums.length) {
      var ax = nums[0] * Math.PI / 180.0;
      current = __mdmdview_apply_matrix(current, 1, 0, Math.tan(ax), 1, 0, 0);
    } else if (name === 'skewy' && nums.length) {
      var ay = nums[0] * Math.PI / 180.0;
      current = __mdmdview_apply_matrix(current, 1, Math.tan(ay), 0, 1, 0, 0);
    }
  }
  return current;
}
function __mdmdview_bbox(el) {
  if (!el) { return { x: 0, y: 0, width: 0, height: 0 }; }
  var tag = (el.tagName || '').toLowerCase();
  if (
    tag === 'style'
    || tag === 'defs'
    || tag === 'script'
    || tag === 'title'
    || tag === 'desc'
    || tag === 'metadata'
    || tag === 'marker'
    || tag === 'clippath'
    || tag === 'mask'
    || tag === 'pattern'
    || tag === 'lineargradient'
    || tag === 'radialgradient'
    || tag === 'stop'
  ) {
    return { x: 0, y: 0, width: 0, height: 0 };
  }
  if (tag === '#text') {
    var text = el.textContent || '';
    var size = __mdmdview_get_font_size(el);
    var m = __mdmdview_measure_text(text, size);
    return __mdmdview_apply_transform(el, { x: 0, y: 0, width: m.width, height: m.height });
  }
  if (tag === 'text' || tag === 'tspan') {
    var metrics = __mdmdview_text_metrics(el);
    var fontSize = __mdmdview_get_font_size(el);
    var tx = __mdmdview_parse_num(el.getAttribute ? el.getAttribute('x') : 0);
    var ty = __mdmdview_parse_num(el.getAttribute ? el.getAttribute('y') : 0);
    var dx = __mdmdview_parse_length(el.getAttribute ? el.getAttribute('dx') : 0, fontSize);
    var dy = __mdmdview_parse_length(el.getAttribute ? el.getAttribute('dy') : 0, fontSize);
    if (tag === 'text' && el.children && el.children.length) {
      var first = el.children[0];
      var firstTag = (first.tagName || '').toLowerCase();
      if (firstTag === 'tspan') {
        if (!dx) { dx = __mdmdview_parse_length(first.getAttribute('dx'), fontSize); }
        if (!dy) { dy = __mdmdview_parse_length(first.getAttribute('dy'), fontSize); }
      }
    }
    tx += dx;
    ty += dy;
    var anchor = el.getAttribute ? el.getAttribute('text-anchor') : null;
    if (!anchor && el.style && el.style.textAnchor) { anchor = el.style.textAnchor; }
    if (anchor === 'middle') {
      tx -= metrics.width * 0.5;
    } else if (anchor === 'end' || anchor === 'right') {
      tx -= metrics.width;
    }
    var baseline = el.getAttribute ? el.getAttribute('dominant-baseline') : null;
    if (!baseline && el.style && el.style.dominantBaseline) { baseline = el.style.dominantBaseline; }
    var y = ty - metrics.height * 0.3;
    if (baseline === 'middle' || baseline === 'central') {
      y = ty - metrics.height * 0.5;
    } else if (baseline === 'hanging') {
      y = ty;
    }
    return __mdmdview_apply_transform(el, {
      x: tx,
      y: y,
      width: metrics.width,
      height: metrics.height
    });
  }
  if (tag === 'rect') {
    var rx = __mdmdview_parse_num(el.getAttribute('x'));
    var ry = __mdmdview_parse_num(el.getAttribute('y'));
    var rw = __mdmdview_parse_num(el.getAttribute('width'));
    var rh = __mdmdview_parse_num(el.getAttribute('height'));
    return __mdmdview_apply_transform(el, { x: rx, y: ry, width: rw, height: rh });
  }
  if (tag === 'circle') {
    var cx = __mdmdview_parse_num(el.getAttribute('cx'));
    var cy = __mdmdview_parse_num(el.getAttribute('cy'));
    var r = __mdmdview_parse_num(el.getAttribute('r'));
    return __mdmdview_apply_transform(el, { x: cx - r, y: cy - r, width: r * 2, height: r * 2 });
  }
  if (tag === 'ellipse') {
    var ecx = __mdmdview_parse_num(el.getAttribute('cx'));
    var ecy = __mdmdview_parse_num(el.getAttribute('cy'));
    var erx = __mdmdview_parse_num(el.getAttribute('rx'));
    var ery = __mdmdview_parse_num(el.getAttribute('ry'));
    return __mdmdview_apply_transform(el, { x: ecx - erx, y: ecy - ery, width: erx * 2, height: ery * 2 });
  }
  if (tag === 'line') {
    var x1 = __mdmdview_parse_num(el.getAttribute('x1'));
    var y1 = __mdmdview_parse_num(el.getAttribute('y1'));
    var x2 = __mdmdview_parse_num(el.getAttribute('x2'));
    var y2 = __mdmdview_parse_num(el.getAttribute('y2'));
    var min_x = Math.min(x1, x2);
    var min_y = Math.min(y1, y2);
    var max_x = Math.max(x1, x2);
    var max_y = Math.max(y1, y2);
    return __mdmdview_apply_transform(el, { x: min_x, y: min_y, width: max_x - min_x, height: max_y - min_y });
  }
  if (tag === 'polygon' || tag === 'polyline') {
    var points = el.getAttribute('points');
    if (points) {
      var parts = String(points).trim().split(/[\s,]+/).filter(function(p) { return p.length; });
      if (parts.length >= 2) {
        var min_px = Infinity;
        var min_py = Infinity;
        var max_px = -Infinity;
        var max_py = -Infinity;
        for (var i = 0; i + 1 < parts.length; i += 2) {
          var px = __mdmdview_parse_num(parts[i]);
          var py = __mdmdview_parse_num(parts[i + 1]);
          if (px < min_px) { min_px = px; }
          if (py < min_py) { min_py = py; }
          if (px > max_px) { max_px = px; }
          if (py > max_py) { max_py = py; }
        }
        if (isFinite(min_px) && isFinite(min_py) && isFinite(max_px) && isFinite(max_py)) {
          return __mdmdview_apply_transform(el, {
            x: min_px,
            y: min_py,
            width: max_px - min_px,
            height: max_py - min_py
          });
        }
      }
    }
  }
  if (tag === 'path') {
    var d = el.getAttribute('d');
    if (d) {
      var nums = String(d).match(/-?\d*\.?\d+(?:e[-+]?\d+)?/ig);
      if (nums && nums.length >= 2) {
        var min_px = Infinity;
        var min_py = Infinity;
        var max_px = -Infinity;
        var max_py = -Infinity;
        for (var i = 0; i + 1 < nums.length; i += 2) {
          var px = __mdmdview_parse_num(nums[i]);
          var py = __mdmdview_parse_num(nums[i + 1]);
          if (px < min_px) { min_px = px; }
          if (py < min_py) { min_py = py; }
          if (px > max_px) { max_px = px; }
          if (py > max_py) { max_py = py; }
        }
        if (isFinite(min_px) && isFinite(min_py) && isFinite(max_px) && isFinite(max_py)) {
          return __mdmdview_apply_transform(el, { x: min_px, y: min_py, width: max_px - min_px, height: max_py - min_py });
        }
      }
    }
  }
  var font_size = __mdmdview_get_font_size(el);
  var width_attr = el.getAttribute ? el.getAttribute('width') : null;
  var height_attr = el.getAttribute ? el.getAttribute('height') : null;
  var bw = __mdmdview_parse_length(width_attr, font_size);
  var bh = __mdmdview_parse_length(height_attr, font_size);
  if ((!bw || !bh) && el.style) {
    if (!bw && el.style.width) { bw = __mdmdview_parse_length(el.style.width, font_size); }
    if (!bh && el.style.height) { bh = __mdmdview_parse_length(el.style.height, font_size); }
  }
  if ((!bw || !bh) && el.getAttribute) {
    var view_box = el.getAttribute('viewBox');
    if (view_box) {
      var parts = String(view_box).trim().split(/[\s,]+/).filter(function(p) { return p.length; });
      if (parts.length >= 4) {
        var vbw = __mdmdview_parse_num(parts[2]);
        var vbh = __mdmdview_parse_num(parts[3]);
        if (!bw && vbw) { bw = vbw; }
        if (!bh && vbh) { bh = vbh; }
      }
    }
  }
  if (!bw && !bh) {
    var html_tag = tag === 'div' || tag === 'body' || tag === 'html';
    if (html_tag) {
      bw = window.innerWidth || 0;
      bh = window.innerHeight || 0;
    }
  }
  if (bw || bh) {
    var bx = __mdmdview_parse_num(el.getAttribute ? el.getAttribute('x') : 0);
    var by = __mdmdview_parse_num(el.getAttribute ? el.getAttribute('y') : 0);
    return __mdmdview_apply_transform(el, { x: bx, y: by, width: bw, height: bh });
  }
  if (el.children && el.children.length) {
    var min_x = Infinity;
    var min_y = Infinity;
    var max_x = -Infinity;
    var max_y = -Infinity;
    for (var i = 0; i < el.children.length; i++) {
      var child_box = __mdmdview_bbox(el.children[i]);
      if (!child_box) { continue; }
      if (child_box.x < min_x) { min_x = child_box.x; }
      if (child_box.y < min_y) { min_y = child_box.y; }
      if (child_box.x + child_box.width > max_x) { max_x = child_box.x + child_box.width; }
      if (child_box.y + child_box.height > max_y) { max_y = child_box.y + child_box.height; }
    }
    if (isFinite(min_x) && isFinite(min_y) && isFinite(max_x) && isFinite(max_y)) {
      return __mdmdview_apply_transform(el, { x: min_x, y: min_y, width: max_x - min_x, height: max_y - min_y });
    }
  }
  var fallback_text = el.textContent || '';
  var fallback_size = __mdmdview_get_font_size(el);
  var fallback = __mdmdview_measure_text(fallback_text, fallback_size);
  return __mdmdview_apply_transform(el, { x: 0, y: 0, width: fallback.width, height: fallback.height });
}
function __mdmdview_escape_text(text) {
  return String(text)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}
function __mdmdview_collect_text(node) {
  if (!node) { return ''; }
  if (node.tagName === '#text') { return node.textContent || ''; }
  var out = '';
  if (node.children && node.children.length) {
    for (var i = 0; i < node.children.length; i++) {
      out += __mdmdview_collect_text(node.children[i]);
    }
  }
  if (out && out.length) { return out; }
  return node.textContent || '';
}
function __mdmdview_escape_attr(text) {
  return String(text)
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;');
}
function __mdmdview_style_text(style) {
  if (!style) { return ''; }
  if (typeof style.cssText === 'string' && style.cssText.trim().length) {
    return style.cssText;
  }
  var parts = [];
  for (var key in style) {
    if (!Object.prototype.hasOwnProperty.call(style, key)) { continue; }
    if (key === 'cssText') { continue; }
    var val = style[key];
    if (typeof val === 'function') { continue; }
    if (val === null || val === undefined || val === '') { continue; }
    parts.push(key + ':' + String(val));
  }
  return parts.join(';');
}
function __mdmdview_serialize_attrs(node) {
  var out = '';
  if (!node || !node.attributes) { return out; }
  var has_style = false;
  for (var key in node.attributes) {
    if (!Object.prototype.hasOwnProperty.call(node.attributes, key)) { continue; }
    var val = node.attributes[key];
    if (val === null || val === undefined) { continue; }
    if (key === 'style') { has_style = true; }
    out += ' ' + key + '="' + __mdmdview_escape_attr(val) + '"';
  }
  if (!has_style && node.style) {
    var style_text = __mdmdview_style_text(node.style);
    if (style_text) {
      out += ' style="' + __mdmdview_escape_attr(style_text) + '"';
    }
  }
  return out;
}
function __mdmdview_serialize(node) {
  if (!node) { return ''; }
  if (node.tagName === '#text') {
    return __mdmdview_escape_text(node.textContent || '');
  }
  var tag = node.tagName || '';
  var attrs = __mdmdview_serialize_attrs(node);
  var content = '';
  var lower = String(tag).toLowerCase();
  if (node.children && node.children.length) {
    if (lower === 'style' || lower === 'script') {
      content = __mdmdview_collect_text(node);
    } else {
      for (var i = 0; i < node.children.length; i++) {
        content += __mdmdview_serialize(node.children[i]);
      }
    }
  } else if (node.textContent) {
    if (lower === 'style' || lower === 'script') {
      content = node.textContent;
    } else {
      content = __mdmdview_escape_text(node.textContent);
    }
  }
  return '<' + tag + attrs + '>' + content + '</' + tag + '>';
}
function __mdmdview_serialize_children(node) {
  if (!node || !node.children) { return ''; }
  var content = '';
  for (var i = 0; i < node.children.length; i++) {
    content += __mdmdview_serialize(node.children[i]);
  }
  return content;
}
function __mdmdview_make_style() {
  var style = { cssText: '' };
  style.setProperty = function(key, value) { style[key] = String(value); };
  style.removeProperty = function(key) { delete style[key]; };
  style.getPropertyValue = function(key) {
    return Object.prototype.hasOwnProperty.call(style, key) ? style[key] : '';
  };
  return style;
}
function __mdmdview_detach(child) {
  if (!child || typeof child !== 'object') { return; }
  var parent = child.parentNode;
  if (!parent || !parent.children) { return; }
  var idx = parent.children.indexOf(child);
  if (idx >= 0) {
    parent.children.splice(idx, 1);
    parent.childNodes = parent.children;
    parent.firstChild = parent.children[0] || null;
  }
  child.parentNode = null;
}
function __mdmdview_parse_font_px(font) {
  if (!font) { return 16; }
  var match = String(font).match(/(\d+(?:\.\d+)?)px/);
  if (match && match[1]) {
    var value = parseFloat(match[1]);
    if (!isNaN(value)) { return value; }
  }
  return 16;
}
function __mdmdview_make_canvas_context(canvas) {
  var ctx = {
    canvas: canvas,
    font: '',
    lineWidth: 1,
    fillStyle: '#000',
    strokeStyle: '#000',
    textAlign: 'start',
    textBaseline: 'alphabetic',
    save: function() {},
    restore: function() {},
    beginPath: function() {},
    closePath: function() {},
    moveTo: function() {},
    lineTo: function() {},
    arc: function() {},
    rect: function() {},
    fill: function() {},
    stroke: function() {},
    clip: function() {},
    translate: function() {},
    scale: function() {},
    rotate: function() {},
    clearRect: function() {},
    fillRect: function() {},
    strokeRect: function() {},
    drawImage: function() {},
    setLineDash: function() {},
    measureText: function(text) {
      var size = __mdmdview_parse_font_px(ctx.font);
      var measured = __mdmdview_measure_text(text || '', size);
      return { width: measured.width || 0 };
    },
    fillText: function() {},
    strokeText: function() {},
    createLinearGradient: function() { return { addColorStop: function() {} }; },
    createRadialGradient: function() { return { addColorStop: function() {} }; },
    createPattern: function() { return null; }
  };
  return ctx;
}
function __mdmdview_make_element(tag, ownerDoc, ns) {
  var doc = ownerDoc;
  if (!doc && typeof document !== 'undefined' && document) { doc = document; }
  var parent = null;
  var parent_set = false;
  var node = {
    tagName: tag,
    namespaceURI: ns || null,
    firstChild: null,
    style: __mdmdview_make_style(),
    children: [],
    childNodes: [],
    attributes: {},
    textContent: '',
    setAttribute: function(key, value) {
      var v = String(value);
      this.attributes[key] = v;
      if (key === 'id') { this.id = v; }
    },
    setAttributeNS: function(ns, key, value) {
      var v = String(value);
      this.attributes[key] = v;
      if (key === 'id') { this.id = v; }
    },
    attr: function(key, value) {
      if (value === undefined) { return this.getAttribute(key); }
      if (value === null) { this.removeAttribute(key); return this; }
      this.setAttribute(key, value);
      return this;
    },
    getAttribute: function(key) {
      return Object.prototype.hasOwnProperty.call(this.attributes, key)
        ? this.attributes[key]
        : null;
    },
    getAttributeNS: function(ns, key) { return this.getAttribute(key); },
    text: function(value) {
      if (value === undefined) { return this.textContent || ''; }
      this.textContent = value === null ? '' : String(value);
      return this;
    },
    append: function(tag) {
      var child = __mdmdview_make_element(tag, this.ownerDocument, this.namespaceURI);
      this.appendChild(child);
      return child;
    },
    removeAttribute: function(key) { delete this.attributes[key]; },
    removeAttributeNS: function(ns, key) { delete this.attributes[key]; },
    appendChild: function(child) {
      __mdmdview_detach(child);
      this.children.push(child);
      this.childNodes = this.children;
      this.firstChild = this.children[0] || null;
      if (child && typeof child === 'object') {
        child.parentNode = this;
        if (!child.ownerDocument && this.ownerDocument) { child.ownerDocument = this.ownerDocument; }
      }
      return child;
    },
    removeChild: function(child) {
      var idx = this.children.indexOf(child);
      if (idx >= 0) {
        this.children.splice(idx, 1);
        this.childNodes = this.children;
      }
      if (child && typeof child === 'object') { child.parentNode = null; }
      this.firstChild = this.children[0] || null;
      return child;
    },
    hasChildNodes: function() { return this.children.length > 0; },
    insertBefore: function(child, before) {
      __mdmdview_detach(child);
      var idx = this.children.indexOf(before);
      if (idx < 0) { this.children.push(child); }
      else { this.children.splice(idx, 0, child); }
      this.childNodes = this.children;
      this.firstChild = this.children[0] || null;
      if (child && typeof child === 'object') {
        child.parentNode = this;
        if (!child.ownerDocument && this.ownerDocument) { child.ownerDocument = this.ownerDocument; }
      }
      return child;
    },
    remove: function() { return null; },
    querySelector: function(sel) { return __mdmdview_query_selector(this, sel); },
    querySelectorAll: function(sel) { return __mdmdview_query_selector_all(this, sel); },
    cloneNode: function(deep) {
      var copy = __mdmdview_make_element(this.tagName, this.ownerDocument, this.namespaceURI);
      copy.textContent = this.textContent;
      copy.innerHTML = this.innerHTML;
      for (var key in this.attributes) {
        if (!Object.prototype.hasOwnProperty.call(this.attributes, key)) { continue; }
        copy.attributes[key] = this.attributes[key];
      }
      if (this.style) {
        for (var prop in this.style) {
          if (!Object.prototype.hasOwnProperty.call(this.style, prop)) { continue; }
          if (typeof this.style[prop] === 'function') { continue; }
          copy.style[prop] = this.style[prop];
        }
      }
      if (deep && this.children && this.children.length) {
        for (var i = 0; i < this.children.length; i++) {
          copy.appendChild(this.children[i].cloneNode(true));
        }
      }
      return copy;
    },
    getBBox: function() {
      var box = __mdmdview_bbox(this);
      if (box) {
        box.w = box.width;
        box.h = box.height;
      }
      return box;
    },
    getBoundingClientRect: function() {
      var box = __mdmdview_bbox(this);
      if (box) {
        box.left = box.x;
        box.top = box.y;
        box.right = box.x + box.width;
        box.bottom = box.y + box.height;
        box.w = box.width;
        box.h = box.height;
      }
      return box;
    },
    getComputedTextLength: function() { return __mdmdview_bbox(this).width; },
    getTotalLength: function() { return __mdmdview_path_total_length(this); },
    getPointAtLength: function(len) { return __mdmdview_path_point_at_length(this, len); },
    addEventListener: function() {},
    removeEventListener: function() {},
    dispatchEvent: function() { return false; }
  };
  var lower = String(tag || '').toLowerCase();
  if (lower === 'canvas') {
    node.width = 300;
    node.height = 150;
    node.getContext = function(kind) {
      if (kind && String(kind).toLowerCase() !== '2d') { return null; }
      if (!node.__mdmdview_ctx) {
        node.__mdmdview_ctx = __mdmdview_make_canvas_context(node);
      }
      return node.__mdmdview_ctx;
    };
  }
  var classList = {
    add: function() {
      var list = __mdmdview_get_class_list(node);
      for (var i = 0; i < arguments.length; i++) {
        var cls = String(arguments[i]);
        if (cls && list.indexOf(cls) < 0) { list.push(cls); }
      }
      __mdmdview_set_class_list(node, list);
    },
    remove: function() {
      var list = __mdmdview_get_class_list(node);
      var next = [];
      for (var i = 0; i < list.length; i++) {
        var keep = true;
        for (var j = 0; j < arguments.length; j++) {
          if (list[i] === String(arguments[j])) { keep = false; break; }
        }
        if (keep) { next.push(list[i]); }
      }
      __mdmdview_set_class_list(node, next);
    },
    contains: function(cls) { return __mdmdview_has_class(node, cls); },
    toggle: function(cls, force) {
      var list = __mdmdview_get_class_list(node);
      var name = String(cls);
      var has = list.indexOf(name) >= 0;
      var should_add = force === undefined ? !has : !!force;
      if (should_add && !has) { list.push(name); }
      if (!should_add && has) { list = list.filter(function(item) { return item !== name; }); }
      __mdmdview_set_class_list(node, list);
      return should_add;
    }
  };
  node.classList = classList;
  Object.defineProperty(node, 'innerHTML', {
    get: function() { return __mdmdview_serialize_children(this); },
    set: function(value) {
      if (this.children && this.children.length) {
        for (var i = 0; i < this.children.length; i++) {
          var child = this.children[i];
          if (child && typeof child === 'object') { child.parentNode = null; }
        }
      }
      this.children = [];
      this.childNodes = this.children;
      this.firstChild = null;
      this.textContent = value ? String(value) : '';
    },
    configurable: true
  });
  Object.defineProperty(node, 'className', {
    get: function() {
      var cls = this.getAttribute('class');
      return cls ? String(cls) : '';
    },
    set: function(value) { this.setAttribute('class', value); },
    configurable: true
  });
  Object.defineProperty(node, 'outerHTML', {
    get: function() { return __mdmdview_serialize(this); },
    configurable: true
  });
  Object.defineProperty(node, 'ownerDocument', {
    get: function() {
      if (doc) { return doc; }
      if (typeof document !== 'undefined') { return document || null; }
      return null;
    },
    set: function(value) { doc = value; },
    configurable: true
  });
  Object.defineProperty(node, 'parentNode', {
    get: function() {
      if (parent_set) { return parent || null; }
      if (typeof document !== 'undefined' && document && document.body) { return document.body; }
      return null;
    },
    set: function(value) { parent = value; parent_set = true; },
    configurable: true
  });
  Object.defineProperty(node, 'nextSibling', {
    get: function() {
      var p = this.parentNode;
      if (!p || !p.children) { return null; }
      var idx = p.children.indexOf(this);
      return idx >= 0 && idx + 1 < p.children.length ? p.children[idx + 1] : null;
    },
    configurable: true
  });
  Object.defineProperty(node, 'previousSibling', {
    get: function() {
      var p = this.parentNode;
      if (!p || !p.children) { return null; }
      var idx = p.children.indexOf(this);
      return idx > 0 ? p.children[idx - 1] : null;
    },
    configurable: true
  });
  Object.defineProperty(node, 'firstElementChild', {
    get: function() {
      return this.children && this.children.length ? this.children[0] : null;
    },
    configurable: true
  });
  return node;
}
function __mdmdview_find_by_id(node, id) {
  if (!node || !id) { return null; }
  if (node.id === id) { return node; }
  if (node.attributes && node.attributes.id === id) { return node; }
  if (!node.children) { return null; }
  for (var i = 0; i < node.children.length; i++) {
    var hit = __mdmdview_find_by_id(node.children[i], id);
    if (hit) { return hit; }
  }
  return null;
}
function __mdmdview_find_by_tag(node, tag, matches) {
  if (!node || !tag) { return; }
  var name = (node.tagName || '').toLowerCase();
  if (name === tag) { matches.push(node); }
  if (!node.children) { return; }
  for (var i = 0; i < node.children.length; i++) {
    __mdmdview_find_by_tag(node.children[i], tag, matches);
  }
}
function __mdmdview_get_class_list(node) {
  if (!node) { return []; }
  var raw = null;
  if (node.getAttribute) { raw = node.getAttribute('class'); }
  if (raw === null || raw === undefined) { raw = node.attributes && node.attributes['class']; }
  if (!raw) { return []; }
  return String(raw).split(/\s+/).filter(function(item) { return item.length > 0; });
}
function __mdmdview_set_class_list(node, list) {
  if (!node) { return; }
  var value = (list || []).filter(function(item) { return item.length > 0; }).join(' ');
  if (node.setAttribute) { node.setAttribute('class', value); }
  else if (node.attributes) { node.attributes['class'] = value; }
}
function __mdmdview_has_class(node, className) {
  if (!node || !className) { return false; }
  var cls = node.attributes && node.attributes['class'];
  if (!cls) { return false; }
  var parts = String(cls).split(/\s+/);
  for (var i = 0; i < parts.length; i++) {
    if (parts[i] === className) { return true; }
  }
  return false;
}
function __mdmdview_find_by_class(node, className, matches) {
  if (!node || !className) { return; }
  if (__mdmdview_has_class(node, className)) { matches.push(node); }
  if (!node.children) { return; }
  for (var i = 0; i < node.children.length; i++) {
    __mdmdview_find_by_class(node.children[i], className, matches);
  }
}
function __mdmdview_get_attr(node, name) {
  if (!node || !name) { return null; }
  if (node.getAttribute) {
    var val = node.getAttribute(name);
    if (val !== null && val !== undefined) { return String(val); }
  }
  if (node.attributes && Object.prototype.hasOwnProperty.call(node.attributes, name)) {
    return String(node.attributes[name]);
  }
  return null;
}
function __mdmdview_parse_attr_selector(raw) {
  var match = raw.match(/^\[\s*([^\]=\s]+)\s*=\s*["']?([^\"'\]]+)["']?\s*\]$/);
  if (!match) { return null; }
  return { name: match[1], value: match[2] };
}
function __mdmdview_find_by_attr(node, name, value, matches) {
  if (!node || !name) { return null; }
  var attr = __mdmdview_get_attr(node, name);
  if (attr !== null && String(attr) === String(value)) {
    if (matches) { matches.push(node); }
    else { return node; }
  }
  if (!node.children) { return matches ? null : null; }
  for (var i = 0; i < node.children.length; i++) {
    var hit = __mdmdview_find_by_attr(node.children[i], name, value, matches);
    if (!matches && hit) { return hit; }
  }
  return matches ? null : null;
}
function __mdmdview_matches_simple(node, raw) {
  if (!node || !raw) { return false; }
  var attr = __mdmdview_parse_attr_selector(raw);
  if (attr) {
    var attrVal = __mdmdview_get_attr(node, attr.name);
    return attrVal !== null && String(attrVal) === String(attr.value);
  }
  if (raw[0] === '#') {
    var id = raw.slice(1);
    return node.id === id || __mdmdview_get_attr(node, 'id') === id;
  }
  if (raw[0] === '.') {
    return __mdmdview_has_class(node, raw.slice(1));
  }
  var hashIdx = raw.indexOf('#');
  if (hashIdx > 0) {
    var tagName = raw.slice(0, hashIdx).toLowerCase();
    var tagId = raw.slice(hashIdx + 1);
    return (node.tagName || '').toLowerCase() === tagName
      && (node.id === tagId || __mdmdview_get_attr(node, 'id') === tagId);
  }
  var dotIdx = raw.indexOf('.');
  if (dotIdx > 0) {
    var tag = raw.slice(0, dotIdx).toLowerCase();
    var cls = raw.slice(dotIdx + 1);
    return (node.tagName || '').toLowerCase() === tag && __mdmdview_has_class(node, cls);
  }
  return (node.tagName || '').toLowerCase() === raw.toLowerCase();
}
function __mdmdview_matches_selector(node, sel) {
  if (!node || !sel) { return false; }
  var raw = String(sel).trim();
  if (!raw) { return false; }
  var parts = raw.split(/\s+/).filter(function(part) { return part.length > 0; });
  if (parts.length <= 1) { return __mdmdview_matches_simple(node, raw); }
  var idx = parts.length - 1;
  var current = node;
  while (current && idx >= 0) {
    if (__mdmdview_matches_simple(current, parts[idx])) {
      idx -= 1;
    }
    current = current.parentNode;
  }
  return idx < 0;
}
function __mdmdview_query_selector(node, sel) {
  if (!sel) { return null; }
  var raw = String(sel).trim();
  if (!raw) { return null; }
  if (node && !node.tagName && node._parent) { node = node._parent; }
  if ((!node || !node.children) && document && document.body) { node = document.body; }
  var chain = raw.split(/\s+/).filter(function(part) { return part.length > 0; });
  if (chain.length > 1) {
    var current = node;
    for (var c = 0; c < chain.length; c++) {
      current = __mdmdview_query_selector(current, chain[c]);
      if (!current) { return null; }
    }
    return current;
  }
  var result = null;
  var attrSel = __mdmdview_parse_attr_selector(raw);
  if (attrSel) {
    result = __mdmdview_find_by_attr(node, attrSel.name, attrSel.value);
  } else if (raw[0] === '#') {
    result = __mdmdview_find_by_id(node, raw.slice(1));
  } else if (raw[0] === '.') {
    var className = raw.slice(1);
    var classMatches = [];
    __mdmdview_find_by_class(node, className, classMatches);
    result = classMatches.length ? classMatches[0] : null;
  } else if (raw.indexOf('#') > 0) {
    var hashParts = raw.split('#');
    var hashTag = hashParts[0].toLowerCase();
    var hashId = hashParts[1];
    var hashMatches = [];
    __mdmdview_find_by_tag(node, hashTag, hashMatches);
    for (var h = 0; h < hashMatches.length; h++) {
      if (hashMatches[h].id === hashId || __mdmdview_get_attr(hashMatches[h], 'id') === hashId) {
        result = hashMatches[h];
        break;
      }
    }
  } else if (raw.indexOf('.') > 0) {
    var pieces = raw.split('.');
    var tagName = pieces[0].toLowerCase();
    var clsName = pieces[1];
    var tagMatches = [];
    __mdmdview_find_by_tag(node, tagName, tagMatches);
    for (var t = 0; t < tagMatches.length; t++) {
      if (__mdmdview_has_class(tagMatches[t], clsName)) {
        result = tagMatches[t];
        break;
      }
    }
  } else {
    var tag = raw.toLowerCase();
    var matches = [];
    __mdmdview_find_by_tag(node, tag, matches);
    result = matches.length ? matches[0] : null;
  }
  return result;
}
function __mdmdview_query_selector_all(node, sel) {
  if (!sel) { return []; }
  var raw = String(sel).trim();
  if (!raw) { return []; }
  if (node && !node.tagName && node._parent) { node = node._parent; }
  if ((!node || !node.children) && document && document.body) { node = document.body; }
  var chain = raw.split(/\s+/).filter(function(part) { return part.length > 0; });
  if (chain.length > 1) {
    var current = node;
    for (var c = 0; c < chain.length; c++) {
      current = __mdmdview_query_selector(current, chain[c]);
      if (!current) { return []; }
    }
    return [current];
  }
  var attrSel = __mdmdview_parse_attr_selector(raw);
  if (attrSel) {
    var attrMatches = [];
    __mdmdview_find_by_attr(node, attrSel.name, attrSel.value, attrMatches);
    return attrMatches;
  }
  if (raw[0] === '#') {
    var hit = __mdmdview_find_by_id(node, raw.slice(1));
    return hit ? [hit] : [];
  }
  if (raw[0] === '.') {
    var className = raw.slice(1);
    var classMatches = [];
    __mdmdview_find_by_class(node, className, classMatches);
    return classMatches;
  }
  if (raw.indexOf('#') > 0) {
    var hashParts = raw.split('#');
    var hashTag = hashParts[0].toLowerCase();
    var hashId = hashParts[1];
    var hashMatches = [];
    __mdmdview_find_by_tag(node, hashTag, hashMatches);
    var hashHits = [];
    for (var h = 0; h < hashMatches.length; h++) {
      if (hashMatches[h].id === hashId || __mdmdview_get_attr(hashMatches[h], 'id') === hashId) {
        hashHits.push(hashMatches[h]);
      }
    }
    return hashHits;
  }
  if (raw.indexOf('.') > 0) {
    var pieces = raw.split('.');
    var tagName = pieces[0].toLowerCase();
    var clsName = pieces[1];
    var tagMatches = [];
    __mdmdview_find_by_tag(node, tagName, tagMatches);
    var hits = [];
    for (var t = 0; t < tagMatches.length; t++) {
      if (__mdmdview_has_class(tagMatches[t], clsName)) {
        hits.push(tagMatches[t]);
      }
    }
    return hits;
  }
  var tag = raw.toLowerCase();
  var matches = [];
  __mdmdview_find_by_tag(node, tag, matches);
  return matches;
}
var document = {
  body: __mdmdview_make_element('body'),
  head: __mdmdview_make_element('head'),
  children: [],
  childNodes: [],
  createElement: function(tag) { return __mdmdview_make_element(tag, document); },
  createElementNS: function(ns, tag) { return __mdmdview_make_element(tag, document, ns); },
  createTextNode: function(text) {
    var node = __mdmdview_make_element('#text', document);
    node.textContent = text || '';
    return node;
  },
  getElementById: function(id) { return __mdmdview_find_by_id(document, id); },
  getElementsByTagName: function(tag) {
    var raw = String(tag || '').trim().toLowerCase();
    if (!raw) { return []; }
    if (raw === 'head') { return document.head ? [document.head] : []; }
    if (raw === 'body') { return document.body ? [document.body] : []; }
    var matches = [];
    __mdmdview_find_by_tag(document, raw, matches);
    return matches;
  },
  querySelector: function(sel) {
    var raw = String(sel || '').trim().toLowerCase();
    var result = raw === 'head'
      ? document.head
      : (raw === 'body' ? document.body : __mdmdview_query_selector(document, sel));
    return result || (document && document.body ? document.body : null);
  },
  querySelectorAll: function(sel) {
    var raw = String(sel || '').trim().toLowerCase();
    var result = raw === 'head'
      ? [document.head]
      : (raw === 'body' ? [document.body] : __mdmdview_query_selector_all(document, sel));
    if (result && result.length) { return result; }
    return document && document.body ? [document.body] : [];
  },
  appendChild: function(child) {
    return document.documentElement
      ? document.documentElement.appendChild(child)
      : document.body.appendChild(child);
  },
  insertBefore: function(child, before) {
    return document.documentElement
      ? document.documentElement.insertBefore(child, before)
      : document.body.insertBefore(child, before);
  },
  removeChild: function(child) {
    return document.documentElement
      ? document.documentElement.removeChild(child)
      : document.body.removeChild(child);
  },
  hasChildNodes: function() {
    return document.documentElement
      ? document.documentElement.hasChildNodes()
      : document.body.hasChildNodes();
  },
  addEventListener: function() {}
};
window.document = document;
document.body.ownerDocument = document;
document.head.ownerDocument = document;
document.documentElement = __mdmdview_make_element('html');
document.documentElement.ownerDocument = document;
document.documentElement.appendChild(document.head);
document.documentElement.appendChild(document.body);
document.documentElement.parentNode = document;
document.children = [document.documentElement];
document.childNodes = document.children;
document.defaultView = window;
document.ownerDocument = document;
document.body.namespaceURI = 'http://www.w3.org/1999/xhtml';
document.head.namespaceURI = 'http://www.w3.org/1999/xhtml';
document.documentElement.namespaceURI = 'http://www.w3.org/1999/xhtml';
document.namespaceURI = document.body.namespaceURI;
window.ownerDocument = document;
window.namespaceURI = document.body.namespaceURI;
function __mdmdview_clear_node(node) {
  if (!node) { return; }
  if (node.children && node.children.length) {
    for (var i = 0; i < node.children.length; i++) {
      var child = node.children[i];
      if (child && typeof child === 'object') { child.parentNode = null; }
    }
  }
  node.children = [];
  node.childNodes = node.children;
  node.firstChild = null;
  node.textContent = '';
}
function __mdmdview_reset_dom() {
  __mdmdview_clear_node(document.head);
  __mdmdview_clear_node(document.body);
  document.documentElement.children = [document.head, document.body];
  document.documentElement.childNodes = document.documentElement.children;
  document.documentElement.firstChild = document.head || null;
  document.head.parentNode = document.documentElement;
  document.body.parentNode = document.documentElement;
  __mdmdview_text_cache = {};
  window.__mdmdview_timer_queue = [];
  window.__mdmdview_timer_map = {};
  window.__mdmdview_now_base = 0;
}
if (!Object.hasOwn) {
  Object.hasOwn = function(obj, prop) {
    return Object.prototype.hasOwnProperty.call(obj, prop);
  };
}
if (!Object.prototype.hasOwnProperty('ownerDocument')) {
  Object.defineProperty(Object.prototype, 'ownerDocument', {
    get: function() { return document || null; },
    set: function(value) {
      Object.defineProperty(this, 'ownerDocument', { value: value, writable: true, configurable: true });
    },
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('parentNode')) {
  Object.defineProperty(Object.prototype, 'parentNode', {
    get: function() {
      if (this && this.__mdmdview_parent_set) {
        return this.__mdmdview_parent || null;
      }
      if (typeof document !== 'undefined' && document && document.body) { return document.body; }
      return null;
    },
    set: function(value) {
      this.__mdmdview_parent = value;
      this.__mdmdview_parent_set = true;
    },
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('querySelector')) {
  Object.defineProperty(Object.prototype, 'querySelector', {
    value: function(sel) { return __mdmdview_query_selector(this, sel); },
    writable: true,
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('querySelectorAll')) {
  Object.defineProperty(Object.prototype, 'querySelectorAll', {
    value: function(sel) { return __mdmdview_query_selector_all(this, sel); },
    writable: true,
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('appendChild')) {
  Object.defineProperty(Object.prototype, 'appendChild', {
    value: function(child) {
      if (this && !this.tagName && document && document.body && this !== document.body) {
        return document.body.appendChild(child);
      }
      __mdmdview_detach(child);
      if (!this.children) { this.children = []; }
      this.children.push(child);
      this.childNodes = this.children;
      this.firstChild = this.children[0] || null;
      if (child && typeof child === 'object') {
        child.parentNode = this;
      }
      return child;
    },
    writable: true,
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('insertBefore')) {
  Object.defineProperty(Object.prototype, 'insertBefore', {
    value: function(child, before) {
      if (this && !this.tagName && document && document.body && this !== document.body) {
        return document.body.insertBefore(child, before);
      }
      __mdmdview_detach(child);
      if (!this.children) { this.children = []; }
      var idx = this.children.indexOf(before);
      if (idx < 0) { this.children.push(child); }
      else { this.children.splice(idx, 0, child); }
      this.childNodes = this.children;
      this.firstChild = this.children[0] || null;
      if (child && typeof child === 'object') {
        child.parentNode = this;
      }
      return child;
    },
    writable: true,
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('removeChild')) {
  Object.defineProperty(Object.prototype, 'removeChild', {
    value: function(child) {
      if (!this.children) { return child; }
      var idx = this.children.indexOf(child);
      if (idx >= 0) { this.children.splice(idx, 1); }
      this.childNodes = this.children;
      this.firstChild = this.children[0] || null;
      return child;
    },
    writable: true,
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('matches')) {
  Object.defineProperty(Object.prototype, 'matches', {
    value: function(sel) { return __mdmdview_matches_selector(this, sel); },
    writable: true,
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('nextSibling')) {
  Object.defineProperty(Object.prototype, 'nextSibling', {
    get: function() { return null; },
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('previousSibling')) {
  Object.defineProperty(Object.prototype, 'previousSibling', {
    get: function() { return null; },
    configurable: true
  });
}
window.getComputedStyle = function(el) {
  return (el && el.style) ? el.style : __mdmdview_make_style();
};
window.__mdmdview_timer_queue = [];
window.__mdmdview_timer_map = {};
window.__mdmdview_next_timer_id = 1;
window.setTimeout = function(fn, ms) {
  var id = window.__mdmdview_next_timer_id++;
  window.__mdmdview_timer_map[id] = { fn: fn };
  window.__mdmdview_timer_queue.push(id);
  return id;
};
window.clearTimeout = function(id) {
  if (window.__mdmdview_timer_map) {
    delete window.__mdmdview_timer_map[id];
  }
};
window.requestAnimationFrame = function(fn) {
  return window.setTimeout(function() { fn(0); }, 16);
};
window.cancelAnimationFrame = function(id) { window.clearTimeout(id); };
window.__mdmdview_run_timers = function(max_ticks) {
  var ticks = 0;
  var queue = window.__mdmdview_timer_queue || [];
  var map = window.__mdmdview_timer_map || {};
  var limit = typeof max_ticks === 'number' && max_ticks > 0 ? max_ticks : 1000;
  while (queue.length && ticks < limit) {
    var id = queue.shift();
    var entry = map[id];
    if (entry && typeof entry.fn === 'function') { entry.fn(); }
    delete map[id];
    ticks++;
  }
  return queue.length;
};
if (!window.devicePixelRatio) { window.devicePixelRatio = 1; }
if (!window.Image) {
  window.Image = function() {
    this.complete = true;
    this.addEventListener = function() {};
    this.removeEventListener = function() {};
    return this;
  };
}
window.addEventListener = function() {};
window.removeEventListener = function() {};
window.structuredClone = function(value) {
  if (value === null || value === undefined) { return value; }
  if (typeof value !== 'object') { return value; }
  try { return JSON.parse(JSON.stringify(value)); } catch (e) {}
  if (Array.isArray(value)) { return value.slice(); }
  var out = {};
  for (var key in value) {
    if (!Object.prototype.hasOwnProperty.call(value, key)) { continue; }
    out[key] = value[key];
  }
  return out;
};
window.__mdmdview_now_base = 0;
window.performance = { now: function(){
  if (typeof Date !== 'undefined' && typeof Date.now === 'function') { return Date.now(); }
  window.__mdmdview_now_base += 1;
  return window.__mdmdview_now_base;
} };
window.console = {
  log: function(){},
  warn: function(){},
  error: function(){},
  info: function(){},
  debug: function(){},
  trace: function(){}
};
window.fetch = function(){ throw new Error('fetch disabled'); };
window.XMLHttpRequest = function(){ throw new Error('XMLHttpRequest disabled'); };
window.DOMPurify = { sanitize: function(html){ return html; } };
window.getComputedStyle = function() {
  return {
    getPropertyValue: function() { return '0px'; }
  };
};
window.__mdmdview_viewport_width = 1200;
window.__mdmdview_viewport_height = 900;
Object.defineProperty(window, 'innerWidth', {
  get: function() { return window.__mdmdview_viewport_width || 0; },
  configurable: true
});
Object.defineProperty(window, 'innerHeight', {
  get: function() { return window.__mdmdview_viewport_height || 0; },
  configurable: true
});
if (!Object.prototype.hasOwnProperty('parentElement')) {
  Object.defineProperty(Object.prototype, 'parentElement', {
    get: function() {
      var p = this && this.parentNode ? this.parentNode : null;
      if (p && p.tagName) { return p; }
      return null;
    },
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('offsetWidth')) {
  Object.defineProperty(Object.prototype, 'offsetWidth', {
    get: function() {
      if (this && typeof this.getBBox === 'function') {
        var width = this.getBBox().width || 0;
        if (width) { return width; }
      }
      return window.innerWidth || 0;
    },
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('offsetHeight')) {
  Object.defineProperty(Object.prototype, 'offsetHeight', {
    get: function() {
      if (this && typeof this.getBBox === 'function') {
        var height = this.getBBox().height || 0;
        if (height) { return height; }
      }
      return window.innerHeight || 0;
    },
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('clientWidth')) {
  Object.defineProperty(Object.prototype, 'clientWidth', {
    get: function() { return this.offsetWidth || 0; },
    configurable: true
  });
}
if (!Object.prototype.hasOwnProperty('clientHeight')) {
  Object.defineProperty(Object.prototype, 'clientHeight', {
    get: function() { return this.offsetHeight || 0; },
    configurable: true
  });
}
"#;

#[cfg(feature = "mermaid-quickjs")]
const MERMAID_INIT_SNIPPET: &str = r##"
if (window.mermaid && mermaid.mermaidAPI) {
  mermaid.mermaidAPI.initialize({startOnLoad: false});
  window.__mdmdview_is_plain_object = function(value) {
    return value && typeof value === 'object' && !Array.isArray(value);
  };
  window.__mdmdview_merge_config = function(baseConfig, overrideConfig) {
    var out = {};
    function assign(target, source) {
      if (!window.__mdmdview_is_plain_object(source)) { return; }
      for (var key in source) {
        if (!Object.prototype.hasOwnProperty.call(source, key)) { continue; }
        var value = source[key];
        if (window.__mdmdview_is_plain_object(value)) {
          if (!window.__mdmdview_is_plain_object(target[key])) { target[key] = {}; }
          assign(target[key], value);
        } else if (Array.isArray(value)) {
          target[key] = value.slice();
        } else {
          target[key] = value;
        }
      }
    }
    assign(out, baseConfig);
    assign(out, overrideConfig);
    return out;
  };
  window.__mdmdview_sanitize_config = function(cfg) {
    if (!window.__mdmdview_is_plain_object(cfg)) { return null; }
    var disallowed = {
      securityLevel: true,
      startOnLoad: true,
      maxTextSize: true,
      deterministicIds: true,
      deterministicIDSeed: true,
      htmlLabels: true,
      themeCSS: true,
      dompurifyConfig: true,
      secure: true
    };
    var out = {};
    for (var key in cfg) {
      if (!Object.prototype.hasOwnProperty.call(cfg, key)) { continue; }
      if (key.indexOf('__') === 0 || key.indexOf('proto') >= 0 || key.indexOf('constructor') >= 0) {
        continue;
      }
      if (disallowed[key]) { continue; }
      var value = cfg[key];
      if (window.__mdmdview_is_plain_object(value)) {
        var nested = window.__mdmdview_sanitize_config(value);
        if (nested && Object.keys(nested).length) { out[key] = nested; }
      } else if (Array.isArray(value)) {
        out[key] = value.slice();
      } else {
        out[key] = value;
      }
    }
    if (out.themeVariables && window.__mdmdview_is_plain_object(out.themeVariables)) {
      for (var tvKey in out.themeVariables) {
        if (!Object.prototype.hasOwnProperty.call(out.themeVariables, tvKey)) { continue; }
        var tvVal = out.themeVariables[tvKey];
        if (typeof tvVal === 'string') {
          if (!tvVal.match(/^[\d "#%(),.;A-Za-z-]+$/)) {
            delete out.themeVariables[tvKey];
          }
        }
      }
    }
    return out;
  };
  window.__mdmdview_extract_init = function(code) {
    if (typeof code !== 'string') { return { code: code, config: null }; }
    var initRe = /%%\{\s*init\s*:\s*([\s\S]*?)\s*\}%%/g;
    var found = null;
    var cleaned = code.replace(initRe, function(match, json) {
      if (found === null) {
        try { found = JSON.parse(json); } catch (e) { found = null; }
      }
      return '';
    });
    return { code: cleaned, config: found };
  };
} else {
  throw new Error('Mermaid not available');
}
"##;

#[cfg(feature = "mermaid-quickjs")]
const MERMAID_RENDER_WRAPPER: &str = r#"
(function(id, code, siteConfigJson, viewportWidth, viewportHeight){
  if (typeof __mdmdview_reset_dom === 'function') {
    __mdmdview_reset_dom();
  }
  if (typeof viewportWidth === 'number' && viewportWidth > 0) {
    window.__mdmdview_viewport_width = viewportWidth;
  }
  if (typeof viewportHeight === 'number' && viewportHeight > 0) {
    window.__mdmdview_viewport_height = viewportHeight;
  }
  var siteConfig = {};
  if (typeof siteConfigJson === 'string' && siteConfigJson.length > 0) {
    try { siteConfig = JSON.parse(siteConfigJson); } catch (e) { siteConfig = {}; }
  }
  var extracted = window.__mdmdview_extract_init(code);
  var directiveConfig = window.__mdmdview_sanitize_config(extracted.config) || {};
  var merged = window.__mdmdview_merge_config(directiveConfig, siteConfig);
  if (merged && merged.securityLevel === 'strict') {
    merged.htmlLabels = false;
    if (!merged.flowchart) { merged.flowchart = {}; }
    merged.flowchart.htmlLabels = false;
  }
  function __mdmdview_flush_timers() {
    if (typeof __mdmdview_run_timers !== 'function') { return; }
    var remaining = 0;
    var cycles = 0;
    do {
      remaining = __mdmdview_run_timers(1000);
      cycles++;
    } while (remaining > 0 && cycles < 100);
    if (remaining > 0) { throw new Error('Mermaid timer queue did not drain'); }
  }
  mermaid.mermaidAPI.initialize(merged);
  var renderCode = extracted && extracted.code !== undefined ? extracted.code : code;
  var svgOut = null;
  var renderResult = null;
  try {
    renderResult = mermaid.mermaidAPI.render(id, renderCode);
    if (!renderResult) {
      renderResult = mermaid.mermaidAPI.render(id, renderCode, function(svg){ svgOut = svg; });
    }
    __mdmdview_flush_timers();
  } catch (err) {
    throw err;
  }
  if (renderResult && typeof renderResult.then === 'function') {
    return renderResult.then(function(out){
      __mdmdview_flush_timers();
      if (typeof out === 'string') { return out; }
      if (out && out.svg) { return out.svg; }
      if (typeof svgOut === 'string' && svgOut.length > 0) { return svgOut; }
      throw new Error('Mermaid render returned empty');
    });
  }
  if (typeof renderResult === 'string') { return renderResult; }
  if (renderResult && renderResult.svg) { return renderResult.svg; }
  if (typeof svgOut === 'string' && svgOut.length > 0) { return svgOut; }
  throw new Error('Mermaid render returned empty');
})
"#;

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Mutex, OnceLock};

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("env lock")
    }

    #[test]
    fn test_lru_cache_eviction_and_touch() {
        let mut cache = LruCache::new(2);
        let key_a = "a".to_string();
        let key_b = "b".to_string();
        let key_c = "c".to_string();

        cache.insert(key_a.clone(), 1);
        cache.insert(key_b.clone(), 2);
        assert_eq!(cache.get(&key_a), Some(1));
        cache.insert(key_c.clone(), 3);
        assert!(cache.get(&key_b).is_none());
        assert_eq!(cache.len(), 2);
    }

    #[test]
    fn test_mermaid_texture_key_changes_with_scale_and_bg() {
        let svg_key = 42;
        let width_bucket = 320;
        let scale_a = MermaidRenderer::scale_bucket(1.0);
        let scale_b = MermaidRenderer::scale_bucket(1.2);
        let bg_a = Some([255, 0, 0, 255]);
        let bg_b = Some([0, 0, 0, 255]);

        let key_a = MermaidRenderer::texture_key(svg_key, width_bucket, scale_a, bg_a);
        let key_b = MermaidRenderer::texture_key(svg_key, width_bucket, scale_b, bg_a);
        let key_c = MermaidRenderer::texture_key(svg_key, width_bucket, scale_a, bg_b);

        assert_ne!(key_a, key_b);
        assert_ne!(key_a, key_c);
    }

    #[test]
    fn test_mermaid_width_bucket_rounds_up() {
        let step = MermaidRenderer::MERMAID_WIDTH_BUCKET_STEP;
        assert_eq!(MermaidRenderer::width_bucket(1.0), step);
        assert_eq!(MermaidRenderer::width_bucket(step as f32 + 0.1), step * 2);
    }

    #[test]
    fn test_mermaid_renderer_preference_env() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_MERMAID_RENDERER");
        let (default_pref, explicit) = MermaidRenderer::mermaid_renderer_preference();
        assert!(!explicit);
        let expected = if cfg!(feature = "mermaid-quickjs") {
            MermaidRenderPreference::Embedded
        } else {
            MermaidRenderPreference::Off
        };
        assert_eq!(default_pref, expected);

        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
            let (pref, explicit) = MermaidRenderer::mermaid_renderer_preference();
            assert!(explicit);
            assert_eq!(pref, MermaidRenderPreference::Embedded);
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "off");
            let (pref, explicit) = MermaidRenderer::mermaid_renderer_preference();
            assert!(explicit);
            assert_eq!(pref, MermaidRenderPreference::Off);
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "bogus");
            let (pref, explicit) = MermaidRenderer::mermaid_renderer_preference();
            assert!(!explicit);
            assert_eq!(pref, expected);
        }
    }

    #[test]
    fn test_mermaid_security_level_default_and_env() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_MERMAID_SECURITY");
        assert_eq!(MermaidRenderer::mermaid_security_level(), "strict");
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_SECURITY", "loose");
            assert_eq!(MermaidRenderer::mermaid_security_level(), "loose");
        }
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_image_href_resolver_blocks_external() {
        let resolver = MermaidWorker::image_href_resolver();
        let options = usvg::Options::default();
        let resolve_string = &resolver.resolve_string;

        assert!(resolve_string("http://example.com/img.png", &options).is_none());
        assert!(resolve_string("file:///tmp/foo.png", &options).is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_embed_bytes_present() {
        assert!(!super::mermaid_embed::MERMAID_JS.is_empty());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_quickjs_render_smoke() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let mut worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let flow = "graph TD; A-->B;";
        let seq = "sequenceDiagram\nAlice->>Bob: Hello\nBob-->>Alice: Hi";
        let class = "classDiagram\nClass01 <|-- Class02\nClass01 : +int id\nClass02 : +String name";
        let er = "erDiagram\nCUSTOMER ||--o{ ORDER : places\nORDER ||--|{ LINE_ITEM : contains\nCUSTOMER {\n  int id\n  string name\n}\nORDER {\n  int id\n  date created\n}\nLINE_ITEM {\n  int id\n  int qty\n}\n";
        let gantt = "gantt\ntitle Sample Gantt\ndateFormat  YYYY-MM-DD\nsection A\nTask 1 :a1, 2024-01-01, 5d\nTask 2 :after a1, 3d\nsection B\nTask 3 :b1, 2024-01-06, 4d\n";

        let flow_svg = worker
            .render_svg(MermaidRenderer::hash_str(flow), flow)
            .expect("flowchart render");
        let seq_svg = worker
            .render_svg(MermaidRenderer::hash_str(seq), seq)
            .expect("sequence render");
        let class_svg = worker
            .render_svg(MermaidRenderer::hash_str(class), class)
            .expect("class render");
        let er_svg = worker
            .render_svg(MermaidRenderer::hash_str(er), er)
            .expect("er render");
        let gantt_svg = worker
            .render_svg(MermaidRenderer::hash_str(gantt), gantt)
            .expect("gantt render");

        assert!(flow_svg.contains("<svg"));
        assert!(seq_svg.contains("<svg"));
        assert!(class_svg.contains("<svg"));
        assert!(er_svg.contains("<svg"));
        assert!(gantt_svg.contains("<svg"));

        let width_bucket = MermaidRenderer::width_bucket(600.0);
        let scale_bucket = MermaidRenderer::scale_bucket(1.0);
        let (rgba, w, h) = worker
            .rasterize_svg(&flow_svg, width_bucket, scale_bucket, None)
            .expect("rasterize svg");
        assert!(!rgba.is_empty());
        assert_eq!(rgba.len(), (w as usize) * (h as usize) * 4);
        assert!(w > 0);
        assert!(h > 0);
        assert!(w <= MermaidRenderer::MERMAID_MAX_RENDER_SIDE);
        assert!(h <= MermaidRenderer::MERMAID_MAX_RENDER_SIDE);
    }

    struct EnvGuard {
        key: &'static str,
        original: Option<String>,
    }

    impl EnvGuard {
        fn set(key: &'static str, value: &str) -> Self {
            let original = std::env::var(key).ok();
            std::env::set_var(key, value);
            Self { key, original }
        }
    }

    impl Drop for EnvGuard {
        fn drop(&mut self) {
            if let Some(value) = &self.original {
                std::env::set_var(self.key, value);
            } else {
                std::env::remove_var(self.key);
            }
        }
    }

    #[test]
    fn test_parse_hex_color_and_mermaid_bg_fill() {
        let _lock = env_lock();
        assert_eq!(
            MermaidRenderer::parse_hex_color("#ff00ff"),
            Some([255, 0, 255, 255])
        );
        assert_eq!(
            MermaidRenderer::parse_hex_color("11223344"),
            Some([17, 34, 51, 68])
        );
        assert!(MermaidRenderer::parse_hex_color("bad").is_none());

        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_BG_COLOR", "#010203");
            assert_eq!(MermaidRenderer::mermaid_bg_fill(), Some([1, 2, 3, 255]));
        }

        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_BG", "transparent");
            assert_eq!(MermaidRenderer::mermaid_bg_fill(), None);
        }

        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_BG", "dark");
            assert_eq!(MermaidRenderer::mermaid_bg_fill(), Some([20, 20, 20, 255]));
        }
    }

    #[test]
    fn test_mermaid_bg_key_and_scale_bucket() {
        assert_eq!(MermaidRenderer::mermaid_bg_key(None), "none");
        assert_eq!(
            MermaidRenderer::mermaid_bg_key(Some([1, 2, 3, 4])),
            "01020304"
        );

        let factor = MermaidRenderer::MERMAID_SCALE_BUCKET_FACTOR;
        assert_eq!(
            MermaidRenderer::scale_bucket(0.1),
            (0.5 * factor).round() as u32
        );
        assert_eq!(
            MermaidRenderer::scale_bucket(10.0),
            (4.0 * factor).round() as u32
        );
    }

    #[test]
    fn test_mermaid_hash_str_stable_and_distinct() {
        let a = MermaidRenderer::hash_str("alpha");
        let b = MermaidRenderer::hash_str("alpha");
        let c = MermaidRenderer::hash_str("beta");
        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn test_mermaid_bg_fill_light_mode() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_BG", "light");
        assert_eq!(
            MermaidRenderer::mermaid_bg_fill(),
            Some([255, 255, 255, 255])
        );
    }

    #[test]
    fn test_mermaid_bg_fill_color_override() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_MERMAID_BG");
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_BG_COLOR", "#11223344");
        assert_eq!(MermaidRenderer::mermaid_bg_fill(), Some([17, 34, 51, 68]));
    }

    #[test]
    fn test_mermaid_bg_fill_theme_fallback() {
        let _lock = env_lock();
        let _guard_mode = EnvGuard::set("MDMDVIEW_MERMAID_BG", "theme");
        let _guard_bkg = EnvGuard::set("MDMDVIEW_MERMAID_MAIN_BKG", "#010203");
        assert_eq!(MermaidRenderer::mermaid_bg_fill(), Some([1, 2, 3, 255]));
    }

    #[test]
    fn test_render_block_off_and_embedded_without_feature() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_MERMAID_RENDERER");
        let renderer = MermaidRenderer::new();
        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(800.0, 600.0),
            )),
            ..Default::default()
        };

        let mut rendered = true;
        let _ = ctx.run(input.clone(), |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, "graph TD; A-->B;", 1.0, 14.0);
            });
        });
        assert!(rendered);

        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
        let renderer = MermaidRenderer::new();
        let mut rendered_embedded = true;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered_embedded = renderer.render_block(ui, "graph TD; A-->B;", 1.0, 14.0);
            });
        });
        assert!(rendered_embedded);
    }

    #[test]
    fn test_mermaid_cache_insert_existing_key_and_empty_order() {
        let mut cache = LruCache::new(1);
        cache.insert(1u8, "one".to_string());
        cache.insert(1u8, "uno".to_string());
        assert_eq!(cache.get(&1), Some("uno".to_string()));

        cache.entries.clear();
        cache.entries.insert(2u8, "two".to_string());
        cache.order.clear();
        cache.insert(3u8, "three".to_string());
        assert_eq!(cache.len(), 2);
    }

    #[test]
    fn test_mermaid_has_pending_default_false() {
        let renderer = MermaidRenderer::new();
        assert!(!renderer.has_pending());
    }
}
