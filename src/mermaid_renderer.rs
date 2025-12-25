use crate::image_decode;
use crossbeam_channel::{bounded, Receiver, Sender, TrySendError};
use egui::{Color32, RichText, Stroke};
use std::cell::RefCell;
use std::collections::{hash_map::DefaultHasher, HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::sync::mpsc::Receiver as StdReceiver;
use std::time::Duration;

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

type KrokiJobSender = Sender<KrokiRequest>;
type KrokiJobReceiver = Receiver<KrokiRequest>;

#[cfg(feature = "mermaid-quickjs")]
type MermaidJobSender = Sender<MermaidRequest>;
#[cfg(feature = "mermaid-quickjs")]
type MermaidJobReceiver = Receiver<MermaidRequest>;
#[cfg(feature = "mermaid-quickjs")]
type MermaidResultSender = Sender<MermaidResult>;
#[cfg(feature = "mermaid-quickjs")]
type MermaidResultReceiver = Receiver<MermaidResult>;

#[derive(Clone)]
struct MermaidTextureEntry {
    texture: egui::TextureHandle,
    size: [u32; 2],
}

struct LruCache<K, V> {
    entries: HashMap<K, V>,
    order: VecDeque<K>,
    capacity: usize,
}

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

struct KrokiRequest {
    key: u64,
    url: String,
    payload: String,
}

#[derive(Debug, PartialEq, Eq)]
enum KrokiEnqueueError {
    QueueFull,
    Disconnected,
}

#[cfg(feature = "mermaid-quickjs")]
struct MermaidRequest {
    svg_key: u64,
    texture_key: String,
    code: Option<String>,
    svg: Option<String>,
    width_bucket: u32,
    scale_bucket: u32,
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
    Kroki,
    Off,
}

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
}

pub(crate) struct MermaidRenderer {
    kroki_pending: RefCell<HashSet<u64>>,
    kroki_svg_cache: RefCell<HashMap<u64, Vec<u8>>>,
    kroki_errors: RefCell<HashMap<u64, String>>,
    kroki_job_tx: KrokiJobSender,
    kroki_rx: StdReceiver<(u64, Result<Vec<u8>, String>)>,
    mermaid_textures: RefCell<LruCache<String, MermaidTextureEntry>>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_pending: RefCell<HashSet<String>>,
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
    pub(crate) const MAX_KROKI_JOBS: usize = 4;
    const MERMAID_TEXTURE_CACHE_CAPACITY: usize = 128;
    const MERMAID_WIDTH_BUCKET_STEP: u32 = 32;
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
        let (tx, rx) = std::sync::mpsc::channel();
        let (job_tx, job_rx): (KrokiJobSender, KrokiJobReceiver) =
            bounded(Self::MAX_KROKI_JOBS * 4);
        for worker_idx in 0..Self::MAX_KROKI_JOBS {
            let worker_rx = job_rx.clone();
            let result_tx = tx.clone();
            if let Err(err) = std::thread::Builder::new()
                .name(format!("mdmdview-kroki-{worker_idx}"))
                .spawn(move || {
                    let agent = ureq::AgentBuilder::new()
                        .timeout_connect(Duration::from_secs(5))
                        .timeout_read(Duration::from_secs(10))
                        .timeout_write(Duration::from_secs(10))
                        .build();
                    for job in worker_rx.iter() {
                        let result = Self::perform_kroki_with_agent(&agent, &job.url, &job.payload);
                        let _ = result_tx.send((job.key, result));
                    }
                })
            {
                eprintln!("Failed to start Kroki worker thread: {}", err);
            }
        }
        drop(job_rx);
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
            kroki_pending: RefCell::new(HashSet::new()),
            kroki_svg_cache: RefCell::new(HashMap::new()),
            kroki_errors: RefCell::new(HashMap::new()),
            kroki_job_tx: job_tx,
            kroki_rx: rx,
            mermaid_textures: RefCell::new(LruCache::new(Self::MERMAID_TEXTURE_CACHE_CAPACITY)),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_pending: RefCell::new(HashSet::new()),
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

    fn mermaid_renderer_preference() -> MermaidRenderPreference {
        if let Ok(raw) = std::env::var("MDMDVIEW_MERMAID_RENDERER") {
            let normalized = raw.trim().to_ascii_lowercase();
            return match normalized.as_str() {
                "off" => MermaidRenderPreference::Off,
                "kroki" => MermaidRenderPreference::Kroki,
                "embedded" => MermaidRenderPreference::Embedded,
                _ => {
                    if cfg!(feature = "mermaid-quickjs") {
                        MermaidRenderPreference::Embedded
                    } else {
                        MermaidRenderPreference::Kroki
                    }
                }
            };
        }
        if cfg!(feature = "mermaid-quickjs") {
            MermaidRenderPreference::Embedded
        } else {
            MermaidRenderPreference::Kroki
        }
    }

    pub(crate) fn render_block(
        &self,
        ui: &mut egui::Ui,
        code: &str,
        ui_scale: f32,
        code_font_size: f32,
    ) -> bool {
        self.poll_kroki_results();
        let preference = Self::mermaid_renderer_preference();

        if preference == MermaidRenderPreference::Off {
            egui::Frame::none()
                .fill(Color32::from_rgb(25, 25, 25))
                .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                .inner_margin(8.0)
                .show(ui, |ui| {
                    ui.label(
                        RichText::new(
                            "Mermaid rendering is disabled (MDMDVIEW_MERMAID_RENDERER=off).",
                        )
                        .color(Color32::from_rgb(200, 160, 80))
                        .family(egui::FontFamily::Monospace)
                        .size(code_font_size),
                    );
                });
            return false;
        }

        #[cfg(feature = "mermaid-quickjs")]
        {
            if preference == MermaidRenderPreference::Embedded
                && mermaid_embed::MERMAID_JS.is_empty()
            {
                if !Self::kroki_enabled() {
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
                        });
                    return false;
                }
            } else if preference == MermaidRenderPreference::Embedded {
                let svg_key = Self::hash_str(code);
                let width_bucket = Self::width_bucket(ui.available_width());
                let scale_bucket = Self::scale_bucket(ui_scale);
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
                    if !Self::kroki_enabled() {
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
                            });
                        return false;
                    }
                } else {
                    let svg = self.mermaid_svg_cache.borrow_mut().get(&svg_key);
                    let pending = self.mermaid_pending.borrow().contains(&texture_key);
                    let mut waiting_for_slot = false;
                    let mut local_active = true;

                    if !pending {
                        let (code, svg) = match svg {
                            Some(svg) => (None, Some(svg)),
                            None => (Some(code.to_string()), None),
                        };
                        let request = MermaidRequest {
                            svg_key,
                            texture_key: texture_key.clone(),
                            code,
                            svg,
                            width_bucket,
                            scale_bucket,
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
                                if !Self::kroki_enabled() {
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
                                        });
                                    return false;
                                }
                                local_active = false;
                            }
                        }
                    }

                    if local_active {
                        let inflight = self.mermaid_pending.borrow().len();
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
        }

        let key = Self::hash_str(code);

        if !Self::kroki_enabled() {
            egui::Frame::none()
                .fill(Color32::from_rgb(25, 25, 25))
                .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                .inner_margin(8.0)
                .show(ui, |ui| {
                    ui.label(
                        RichText::new("Mermaid rendering via Kroki is disabled. Set MDMDVIEW_ENABLE_KROKI=1 to enable network rendering.")
                            .color(Color32::from_rgb(200, 160, 80))
                            .family(egui::FontFamily::Monospace)
                            .size(code_font_size),
                    );
                });
            return false;
        }

        if let Some(img_bytes) = self.kroki_svg_cache.borrow().get(&key) {
            let width_bucket = Self::width_bucket(ui.available_width());
            let scale_bucket = Self::scale_bucket(ui_scale);
            let bg = Self::mermaid_bg_fill();
            let cache_key = Self::texture_key(key, width_bucket, scale_bucket, bg);
            if let Some(entry) = self.mermaid_textures.borrow_mut().get(&cache_key) {
                let tex = entry.texture.clone();
                let (tw, th) = (entry.size[0] as f32, entry.size[1] as f32);
                let available_w = ui.available_width().max(1.0);
                let scaled_w = tw * ui_scale;
                let scale = if scaled_w > available_w {
                    (available_w / tw).clamp(0.01, 4.0)
                } else {
                    ui_scale
                };
                let size = egui::vec2((tw * scale).round(), (th * scale).round());
                ui.add(egui::Image::new(&tex).fit_to_exact_size(size));
                return true;
            }

            if let Some((img, w, h)) = image_decode::bytes_to_color_image_guess(img_bytes, bg) {
                let tex =
                    ui.ctx()
                        .load_texture(cache_key.clone(), img, egui::TextureOptions::LINEAR);
                self.store_mermaid_texture(&cache_key, tex.clone(), [w, h]);
                let (tw, th) = (w as f32, h as f32);
                let available_w = ui.available_width().max(1.0);
                let scaled_w = tw * ui_scale;
                let scale = if scaled_w > available_w {
                    (available_w / tw).clamp(0.01, 4.0)
                } else {
                    ui_scale
                };
                let size = egui::vec2((tw * scale).round(), (th * scale).round());
                ui.add(egui::Image::new(&tex).fit_to_exact_size(size));
                return true;
            } else {
                self.kroki_errors
                    .borrow_mut()
                    .insert(key, "Failed to decode diagram bytes".to_string());
            }
        }

        if let Some(err) = self.kroki_errors.borrow().get(&key) {
            egui::Frame::none()
                .fill(Color32::from_rgb(25, 25, 25))
                .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                .inner_margin(8.0)
                .show(ui, |ui| {
                    ui.label(
                        RichText::new(format!(
                            "Mermaid render failed via Kroki; showing source.\n{}",
                            err
                        ))
                        .color(Color32::from_rgb(200, 160, 80)),
                    );
                });
            return false;
        }

        let should_schedule = {
            let pending = self.kroki_pending.borrow();
            !pending.contains(&key)
        };
        let waiting_for_slot = if should_schedule {
            let inflight_before = {
                let pending = self.kroki_pending.borrow();
                pending.len()
            };
            match self.spawn_kroki_job(key, code) {
                Ok(()) => {
                    self.kroki_pending.borrow_mut().insert(key);
                    ui.ctx().request_repaint();
                    inflight_before >= Self::MAX_KROKI_JOBS
                }
                Err(KrokiEnqueueError::QueueFull) => {
                    ui.ctx().request_repaint();
                    true
                }
                Err(KrokiEnqueueError::Disconnected) => {
                    self.kroki_errors
                        .borrow_mut()
                        .insert(key, "Mermaid worker pool unavailable".to_string());
                    return false;
                }
            }
        } else {
            let pending = self.kroki_pending.borrow();
            pending.len() >= Self::MAX_KROKI_JOBS
        };

        let inflight_count = self.kroki_pending.borrow().len();

        egui::Frame::none()
            .fill(Color32::from_rgb(25, 25, 25))
            .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
            .inner_margin(8.0)
            .show(ui, |ui| {
                let url = self.kroki_base_url();
                if waiting_for_slot {
                    ui.label(
                        RichText::new(format!(
                            "Mermaid workers busy ({}/{}) - request queued.",
                            inflight_count,
                            Self::MAX_KROKI_JOBS
                        ))
                        .color(Color32::from_rgb(200, 160, 80))
                        .family(egui::FontFamily::Monospace)
                        .size(code_font_size),
                    );
                    ui.label(
                        RichText::new(format!("Queue target: {}", url))
                            .color(Color32::from_rgb(160, 200, 240)),
                    );
                } else {
                    ui.label(
                        RichText::new(format!("Rendering diagram via Kroki...\n{}", url))
                            .color(Color32::from_rgb(160, 200, 240)),
                    );
                }
            });
        true
    }

    fn width_bucket(available_width: f32) -> u32 {
        let step = Self::MERMAID_WIDTH_BUCKET_STEP;
        let width = available_width.max(1.0).ceil() as u32;
        width.div_ceil(step) * step
    }

    fn scale_bucket(ui_scale: f32) -> u32 {
        let clamped = ui_scale.clamp(0.5, 4.0);
        (clamped * Self::MERMAID_SCALE_BUCKET_FACTOR).round() as u32
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn scale_from_bucket(scale_bucket: u32) -> f32 {
        let scale = scale_bucket as f32 / Self::MERMAID_SCALE_BUCKET_FACTOR;
        scale.clamp(0.5, 4.0)
    }

    fn mermaid_bg_key(bg: Option<[u8; 4]>) -> String {
        match bg {
            Some([r, g, b, a]) => format!("{:02x}{:02x}{:02x}{:02x}", r, g, b, a),
            None => "none".to_string(),
        }
    }

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
        2_000
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

    fn store_mermaid_texture(&self, key: &str, texture: egui::TextureHandle, size: [u32; 2]) {
        self.mermaid_textures
            .borrow_mut()
            .insert(key.to_string(), MermaidTextureEntry { texture, size });
    }

    fn poll_kroki_results(&self) {
        loop {
            match self.kroki_rx.try_recv() {
                Ok((key, Ok(svg))) => {
                    self.kroki_svg_cache.borrow_mut().insert(key, svg);
                    self.kroki_pending.borrow_mut().remove(&key);
                }
                Ok((key, Err(err))) => {
                    self.kroki_errors.borrow_mut().insert(key, err);
                    self.kroki_pending.borrow_mut().remove(&key);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => break,
                Err(std::sync::mpsc::TryRecvError::Disconnected) => break,
            }
        }
    }

    fn kroki_enabled() -> bool {
        match std::env::var("MDMDVIEW_ENABLE_KROKI") {
            Ok(value) => {
                let normalized = value.trim().to_ascii_lowercase();
                matches!(normalized.as_str(), "1" | "true" | "yes" | "on")
            }
            Err(_) => false,
        }
    }

    #[cfg(test)]
    fn kroki_enabled_for_tests() -> bool {
        Self::kroki_enabled()
    }

    fn kroki_base_url(&self) -> String {
        std::env::var("MDMDVIEW_KROKI_URL").unwrap_or_else(|_| "https://kroki.io".to_string())
    }

    #[cfg(not(test))]
    fn perform_kroki_with_agent(
        agent: &ureq::Agent,
        url: &str,
        payload: &str,
    ) -> Result<Vec<u8>, String> {
        let resp = agent
            .post(url)
            .set("Content-Type", "text/plain")
            .send_string(payload)
            .map_err(|e| format!("ureq error: {}", e))?;
        if resp.status() >= 200 && resp.status() < 300 {
            let mut bytes: Vec<u8> = Vec::new();
            let mut reader = resp.into_reader();
            use std::io::Read as _;
            reader
                .read_to_end(&mut bytes)
                .map_err(|e| format!("read body: {}", e))?;
            if bytes.is_empty() {
                return Err("Empty SVG from Kroki".to_string());
            }
            Ok(bytes)
        } else {
            Err(format!("HTTP {} from Kroki", resp.status()))
        }
    }

    #[cfg(test)]
    fn perform_kroki_with_agent(
        _agent: &ureq::Agent,
        _url: &str,
        _payload: &str,
    ) -> Result<Vec<u8>, String> {
        Err("Kroki disabled in tests".to_string())
    }

    fn spawn_kroki_job(&self, key: u64, code: &str) -> Result<(), KrokiEnqueueError> {
        let url = format!("{}/mermaid/png", self.kroki_base_url());
        let payload = self.wrap_mermaid_theme(code);
        match self
            .kroki_job_tx
            .try_send(KrokiRequest { key, url, payload })
        {
            Ok(()) => Ok(()),
            Err(TrySendError::Full(_)) => Err(KrokiEnqueueError::QueueFull),
            Err(TrySendError::Disconnected(_)) => Err(KrokiEnqueueError::Disconnected),
        }
    }

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

        let theme_vars = format!(
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

    fn wrap_mermaid_theme(&self, code: &str) -> String {
        if code.contains("%%{") && code.contains("}%%") && code.contains("init") {
            return code.to_string();
        }
        let theme = Self::mermaid_theme_values();
        let theme = format!(
            concat!(
                "%%{{init: {{\"theme\": \"{}\", \"themeVariables\": {{",
                "\"background\": \"{}\", ",
                "\"mainBkg\": \"{}\", ",
                "\"textColor\": \"{}\", ",
                "\"titleColor\": \"{}\", ",
                "\"primaryColor\": \"{}\", ",
                "\"primaryBorderColor\": \"{}\", ",
                "\"primaryTextColor\": \"{}\", ",
                "\"secondaryColor\": \"{}\", ",
                "\"tertiaryColor\": \"{}\", ",
                "\"lineColor\": \"{}\", ",
                "\"defaultLinkColor\": \"{}\", ",
                "\"clusterBkg\": \"{}\", ",
                "\"clusterBorder\": \"{}\", ",
                "\"labelBackground\": \"{}\", ",
                "\"edgeLabelBackground\": \"{}\"",
                "}} }}%%\n"
            ),
            theme.theme_name,
            theme.main_bkg,
            theme.main_bkg,
            theme.text,
            theme.title,
            theme.primary,
            theme.primary_border,
            theme.primary_text,
            theme.secondary,
            theme.tertiary,
            theme.line,
            theme.default_link,
            theme.cluster_bkg,
            theme.cluster_border,
            theme.label_bg,
            theme.edge_label_bg
        );
        format!("{}{}", theme, code)
    }

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

    fn hash_str(s: &str) -> u64 {
        let mut h = DefaultHasher::new();
        s.hash(&mut h);
        h.finish()
    }
}

#[cfg(feature = "mermaid-quickjs")]
struct MermaidWorker {
    engine: MermaidEngine,
    deadline_ms: Arc<AtomicU64>,
    fontdb: Arc<usvg::fontdb::Database>,
}

#[cfg(feature = "mermaid-quickjs")]
impl MermaidWorker {
    const MEMORY_LIMIT_BYTES: usize = 64 * 1024 * 1024;
    const STACK_LIMIT_BYTES: usize = 4 * 1024 * 1024;

    fn new(worker_idx: usize, fontdb: Arc<usvg::fontdb::Database>) -> Result<Self, String> {
        use rquickjs::{Context, Runtime};
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
        let js = std::str::from_utf8(mermaid_embed::MERMAID_JS)
            .map_err(|_| "Mermaid JS is not valid UTF-8".to_string())?;
        engine
            .ctx
            .with(|ctx| {
                ctx.eval::<(), _>(MERMAID_DOM_SHIM)?;
                ctx.eval::<(), _>(js)?;
                ctx.eval::<(), _>(MERMAID_INIT_SNIPPET)?;
                Ok::<(), rquickjs::Error>(())
            })
            .map_err(|e| format!("Mermaid init error: {}", e))?;
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
        })
    }

    fn render_svg(&mut self, key: u64, code: &str) -> Result<String, String> {
        use rquickjs::{promise::MaybePromise, Function};
        let timeout_ms = MermaidRenderer::mermaid_timeout_ms();
        let deadline = Self::now_ms().saturating_add(timeout_ms);
        self.deadline_ms.store(deadline, Ordering::Relaxed);
        let result = self.engine.ctx.with(|ctx| {
            let wrapper = MERMAID_RENDER_WRAPPER;
            let func: Function = ctx.eval(wrapper)?;
            let id = format!("m{:016x}", key);
            let site_config = MermaidRenderer::mermaid_site_config_json(key);
            let maybe: MaybePromise = func.call((id.as_str(), code, site_config.as_str()))?;
            maybe.finish::<String>()
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

    fn process_job(&mut self, job: MermaidRequest) -> MermaidResult {
        let MermaidRequest {
            svg_key,
            texture_key,
            code,
            svg,
            width_bucket,
            scale_bucket,
            bg,
        } = job;
        let svg_result = match svg {
            Some(svg) => Ok(svg),
            None => match code {
                Some(code) => self.render_svg(svg_key, &code),
                None => Err("Mermaid render request missing code".to_string()),
            },
        };

        match svg_result {
            Ok(svg) => match self.rasterize_svg(&svg, width_bucket, scale_bucket, bg) {
                Ok((rgba, w, h)) => MermaidResult {
                    svg_key,
                    texture_key,
                    svg: Some(svg),
                    rgba: Some(rgba),
                    size: Some((w, h)),
                    error: None,
                },
                Err(err) => MermaidResult {
                    svg_key,
                    texture_key,
                    svg: Some(svg),
                    rgba: None,
                    size: None,
                    error: Some(err),
                },
            },
            Err(err) => MermaidResult {
                svg_key,
                texture_key,
                svg: None,
                rgba: None,
                size: None,
                error: Some(err),
            },
        }
    }

    fn rasterize_svg(
        &self,
        svg: &str,
        width_bucket: u32,
        scale_bucket: u32,
        bg: Option<[u8; 4]>,
    ) -> Result<(Vec<u8>, u32, u32), String> {
        let mut opt = usvg::Options::default();
        opt.resources_dir = None;
        opt.fontdb = Arc::clone(&self.fontdb);
        opt.image_href_resolver = Self::image_href_resolver();

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
  var key = size + '|' + text;
  var hit = __mdmdview_text_cache[key];
  if (hit) { return hit; }
  var width = (text ? text.length : 0) * size * 0.6;
  var height = size * 1.2;
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
function __mdmdview_bbox(el) {
  var text = (el && el.textContent) ? el.textContent : '';
  var size = __mdmdview_get_font_size(el);
  var m = __mdmdview_measure_text(text, size);
  return { x: 0, y: 0, width: m.width, height: m.height };
}
function __mdmdview_make_element(tag) {
  return {
    tagName: tag,
    style: {},
    children: [],
    attributes: {},
    textContent: '',
    innerHTML: '',
    setAttribute: function(key, value) { this.attributes[key] = String(value); },
    getAttribute: function(key) {
      return Object.prototype.hasOwnProperty.call(this.attributes, key)
        ? this.attributes[key]
        : null;
    },
    appendChild: function(child) { this.children.push(child); return child; },
    querySelector: function() { return null; },
    getBBox: function() { return __mdmdview_bbox(this); },
    getBoundingClientRect: function() { return __mdmdview_bbox(this); },
    getComputedTextLength: function() { return __mdmdview_bbox(this).width; }
  };
}
var document = {
  body: __mdmdview_make_element('body'),
  createElement: function(tag) { return __mdmdview_make_element(tag); },
  createElementNS: function(ns, tag) { return __mdmdview_make_element(tag); },
  getElementById: function() { return null; },
  querySelector: function() { return null; }
};
window.document = document;
window.getComputedStyle = function(el) { return (el && el.style) ? el.style : {}; };
window.setTimeout = function(fn, ms) { fn(); return 1; };
window.clearTimeout = function(id) {};
window.requestAnimationFrame = function(fn) { fn(0); return 1; };
window.performance = { now: function(){ return 0; } };
window.fetch = function(){ throw new Error('fetch disabled'); };
window.XMLHttpRequest = function(){ throw new Error('XMLHttpRequest disabled'); };
window.DOMPurify = { sanitize: function(html){ return html; } };
"#;

#[cfg(feature = "mermaid-quickjs")]
const MERMAID_INIT_SNIPPET: &str = r##"
if (window.mermaid && mermaid.mermaidAPI) {
  mermaid.mermaidAPI.initialize({startOnLoad: false});
  function __mdmdview_is_plain_object(value) {
    return value && typeof value === 'object' && !Array.isArray(value);
  }
  function __mdmdview_merge_config(baseConfig, overrideConfig) {
    var out = {};
    function assign(target, source) {
      if (!__mdmdview_is_plain_object(source)) { return; }
      for (var key in source) {
        if (!Object.prototype.hasOwnProperty.call(source, key)) { continue; }
        var value = source[key];
        if (__mdmdview_is_plain_object(value)) {
          if (!__mdmdview_is_plain_object(target[key])) { target[key] = {}; }
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
  }
  function __mdmdview_sanitize_config(cfg) {
    if (!__mdmdview_is_plain_object(cfg)) { return null; }
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
      if (__mdmdview_is_plain_object(value)) {
        var nested = __mdmdview_sanitize_config(value);
        if (nested && Object.keys(nested).length) { out[key] = nested; }
      } else if (Array.isArray(value)) {
        out[key] = value.slice();
      } else {
        out[key] = value;
      }
    }
    if (out.themeVariables && __mdmdview_is_plain_object(out.themeVariables)) {
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
  }
  function __mdmdview_extract_init(code) {
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
  }
} else {
  throw new Error('Mermaid not available');
}
"##;

#[cfg(feature = "mermaid-quickjs")]
const MERMAID_RENDER_WRAPPER: &str = r#"
(function(id, code, siteConfigJson){
  var siteConfig = {};
  if (typeof siteConfigJson === 'string' && siteConfigJson.length > 0) {
    try { siteConfig = JSON.parse(siteConfigJson); } catch (e) { siteConfig = {}; }
  }
  var extracted = __mdmdview_extract_init(code);
  var directiveConfig = __mdmdview_sanitize_config(extracted.config) || {};
  var merged = __mdmdview_merge_config(directiveConfig, siteConfig);
  if (merged && merged.securityLevel === 'strict') {
    merged.htmlLabels = false;
    if (!merged.flowchart) { merged.flowchart = {}; }
    merged.flowchart.htmlLabels = false;
  }
  mermaid.mermaidAPI.initialize(merged);
  var renderCode = extracted && extracted.code !== undefined ? extracted.code : code;
  var svgOut = null;
  var renderResult = mermaid.mermaidAPI.render(id, renderCode, function(svg){ svgOut = svg; });
  if (renderResult && typeof renderResult.then === 'function') {
    return renderResult.then(function(out){
      if (typeof out === 'string') { return out; }
      if (out && out.svg) { return out.svg; }
      throw new Error('Mermaid render returned empty');
    });
  }
  if (typeof renderResult === 'string') { return renderResult; }
  if (typeof svgOut === 'string' && svgOut.length > 0) { return svgOut; }
  throw new Error('Mermaid render returned empty');
})
"#;

#[cfg(test)]
mod tests {
    use super::*;
    use image::codecs::png::PngEncoder;
    use image::ColorType;
    use image::ImageEncoder;
    use std::sync::{Mutex, OnceLock};

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("env lock")
    }

    fn with_test_ui<F>(f: F)
    where
        F: FnOnce(&egui::Context, &mut egui::Ui),
    {
        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(1024.0, 768.0),
            )),
            ..Default::default()
        };
        ctx.begin_frame(input);
        egui::CentralPanel::default().show(&ctx, |ui| {
            f(&ctx, ui);
        });
        let _ = ctx.end_frame();
    }

    fn tiny_png_bytes() -> Vec<u8> {
        let width = 2u32;
        let height = 2u32;
        let pixels = vec![255u8; (width * height * 4) as usize];
        let mut out = Vec::new();
        let encoder = PngEncoder::new(&mut out);
        encoder
            .write_image(&pixels, width, height, ColorType::Rgba8)
            .expect("encode png");
        out
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
        let default_pref = MermaidRenderer::mermaid_renderer_preference();
        if cfg!(feature = "mermaid-quickjs") {
            assert_eq!(default_pref, MermaidRenderPreference::Embedded);
        } else {
            assert_eq!(default_pref, MermaidRenderPreference::Kroki);
        }

        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "kroki");
            assert_eq!(
                MermaidRenderer::mermaid_renderer_preference(),
                MermaidRenderPreference::Kroki
            );
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
            assert_eq!(
                MermaidRenderer::mermaid_renderer_preference(),
                MermaidRenderPreference::Embedded
            );
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "off");
            assert_eq!(
                MermaidRenderer::mermaid_renderer_preference(),
                MermaidRenderPreference::Off
            );
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
    fn test_kroki_enabled_env_flag() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_ENABLE_KROKI");
        assert!(!MermaidRenderer::kroki_enabled_for_tests());
        let _guard = EnvGuard::set("MDMDVIEW_ENABLE_KROKI", "1");
        assert!(MermaidRenderer::kroki_enabled_for_tests());
        let _guard = EnvGuard::set("MDMDVIEW_ENABLE_KROKI", "false");
        assert!(!MermaidRenderer::kroki_enabled_for_tests());
    }

    #[test]
    fn test_spawn_kroki_job_reports_queue_disconnect() {
        let mut renderer = MermaidRenderer::new();
        let (temp_tx, temp_rx) = crossbeam_channel::bounded::<KrokiRequest>(1);
        drop(temp_rx);
        renderer.kroki_job_tx = temp_tx;
        let result = renderer.spawn_kroki_job(1, "graph TD; A-->B;");
        assert!(matches!(result, Err(KrokiEnqueueError::Disconnected)));
    }

    #[test]
    fn test_spawn_kroki_job_reports_queue_full() {
        let mut renderer = MermaidRenderer::new();
        let (temp_tx, _temp_rx) = crossbeam_channel::bounded::<KrokiRequest>(1);
        let _ = temp_tx.try_send(KrokiRequest {
            key: 1,
            url: "u".to_string(),
            payload: "p".to_string(),
        });
        renderer.kroki_job_tx = temp_tx.clone();
        let result = renderer.spawn_kroki_job(42, "graph TD; B-->C;");
        assert!(matches!(result, Err(KrokiEnqueueError::QueueFull)));
    }

    #[test]
    fn test_poll_kroki_results_caches() {
        let mut renderer = MermaidRenderer::new();
        let (tx, rx) = std::sync::mpsc::channel();
        renderer.kroki_rx = rx;
        renderer.kroki_pending.borrow_mut().insert(1);
        renderer.kroki_pending.borrow_mut().insert(2);
        tx.send((1, Ok(vec![1, 2, 3]))).expect("send ok");
        tx.send((2, Err("boom".to_string()))).expect("send err");

        renderer.poll_kroki_results();

        assert!(renderer.kroki_svg_cache.borrow().contains_key(&1));
        assert!(renderer.kroki_errors.borrow().contains_key(&2));
        assert!(!renderer.kroki_pending.borrow().contains(&1));
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
    fn test_render_mermaid_block_cache_and_queue_paths() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_ENABLE_KROKI", "1");
        let renderer = MermaidRenderer::new();
        let code = "graph TD; A-->B;";
        let key = MermaidRenderer::hash_str(code);
        renderer
            .kroki_svg_cache
            .borrow_mut()
            .insert(key, tiny_png_bytes());

        with_test_ui(|_, ui| {
            ui.set_width(12.0);
            assert!(renderer.render_block(ui, code, 1.0, 12.0));
        });

        renderer.kroki_pending.borrow_mut().insert(key);
        with_test_ui(|_, ui| {
            assert!(renderer.render_block(ui, code, 1.0, 12.0));
        });
    }

    #[test]
    fn test_render_mermaid_block_disabled() {
        let _lock = env_lock();
        let renderer = MermaidRenderer::new();
        let _guard = EnvGuard::set("MDMDVIEW_ENABLE_KROKI", "0");
        with_test_ui(|_, ui| {
            assert!(!renderer.render_block(ui, "graph TD; A-->B;", 1.0, 12.0));
        });
    }
}
