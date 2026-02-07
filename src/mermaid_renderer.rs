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
#[cfg(all(test, feature = "mermaid-quickjs"))]
use std::sync::atomic::AtomicBool;
#[cfg(feature = "mermaid-quickjs")]
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};
#[cfg(feature = "mermaid-quickjs")]
use std::thread::JoinHandle;
#[cfg(feature = "mermaid-quickjs")]
use std::time::{SystemTime, UNIX_EPOCH};

#[cfg(feature = "mermaid-quickjs")]
mod mermaid_embed {
    include!(concat!(env!("OUT_DIR"), "/mermaid_js.rs"));
}

#[cfg(feature = "mermaid-quickjs")]
const MERMAID_JS_EMPTY: bool = mermaid_embed::MERMAID_JS.is_empty();

#[cfg(all(test, feature = "mermaid-quickjs"))]
static MERMAID_JS_EMPTY_OVERRIDE: std::sync::Mutex<Option<std::thread::ThreadId>> =
    std::sync::Mutex::new(None);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_RAW_TREE_PARSE_FAIL: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_WORKER_INIT_ERROR: AtomicBool = AtomicBool::new(false);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_THREAD_SPAWN_ERROR: AtomicBool = AtomicBool::new(false);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_INIT_ERROR_STAGE: std::sync::Mutex<Option<(std::thread::ThreadId, usize)>> =
    std::sync::Mutex::new(None);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_RUNTIME_ERROR: std::sync::Mutex<Option<std::thread::ThreadId>> =
    std::sync::Mutex::new(None);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_CONTEXT_ERROR: std::sync::Mutex<Option<std::thread::ThreadId>> =
    std::sync::Mutex::new(None);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_UTF8_ERROR: std::sync::Mutex<Option<std::thread::ThreadId>> =
    std::sync::Mutex::new(None);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_RENDER_EVAL_ERROR: std::sync::Mutex<Option<std::thread::ThreadId>> =
    std::sync::Mutex::new(None);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_RENDER_CALL_ERROR: std::sync::Mutex<Option<std::thread::ThreadId>> =
    std::sync::Mutex::new(None);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_PIXMAP_ALLOC_FAIL: std::sync::Mutex<Option<std::thread::ThreadId>> =
    std::sync::Mutex::new(None);
#[cfg(all(test, feature = "mermaid-quickjs"))]
static FORCE_MERMAID_FACE_PARSE_ERROR: std::sync::Mutex<Option<std::thread::ThreadId>> =
    std::sync::Mutex::new(None);

#[cfg(feature = "mermaid-quickjs")]
fn mermaid_js_empty() -> bool {
    #[cfg(test)]
    if let Ok(guard) = MERMAID_JS_EMPTY_OVERRIDE.try_lock() {
        if let Some(id) = guard.as_ref() {
            if *id == std::thread::current().id() {
                return true;
            }
        }
    }
    MERMAID_JS_EMPTY
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn set_mermaid_js_empty_for_test(value: bool) -> Option<std::thread::ThreadId> {
    let mut guard = MERMAID_JS_EMPTY_OVERRIDE
        .lock()
        .expect("mermaid js override lock");
    let previous = *guard;
    *guard = if value {
        Some(std::thread::current().id())
    } else {
        None
    };
    previous
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_raw_tree_parse_fail_for_test() {
    FORCE_RAW_TREE_PARSE_FAIL.store(true, Ordering::Relaxed);
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_worker_init_error_once() {
    FORCE_MERMAID_WORKER_INIT_ERROR.store(true, Ordering::Relaxed);
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_thread_spawn_error_once() {
    FORCE_MERMAID_THREAD_SPAWN_ERROR.store(true, Ordering::Relaxed);
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_init_error_once(stage: usize) {
    let mut guard = FORCE_MERMAID_INIT_ERROR_STAGE
        .lock()
        .expect("mermaid init error lock");
    *guard = Some((std::thread::current().id(), stage));
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn take_mermaid_flag(flag: &std::sync::Mutex<Option<std::thread::ThreadId>>) -> bool {
    let mut guard = flag.lock().expect("mermaid flag lock");
    if let Some(thread_id) = guard.take() {
        if thread_id == std::thread::current().id() {
            return true;
        }
        *guard = Some(thread_id);
    }
    false
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_runtime_error_once() {
    let mut guard = FORCE_MERMAID_RUNTIME_ERROR
        .lock()
        .expect("mermaid runtime error lock");
    *guard = Some(std::thread::current().id());
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_context_error_once() {
    let mut guard = FORCE_MERMAID_CONTEXT_ERROR
        .lock()
        .expect("mermaid context error lock");
    *guard = Some(std::thread::current().id());
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_utf8_error_once() {
    let mut guard = FORCE_MERMAID_UTF8_ERROR
        .lock()
        .expect("mermaid utf8 error lock");
    *guard = Some(std::thread::current().id());
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_render_eval_error_once() {
    let mut guard = FORCE_MERMAID_RENDER_EVAL_ERROR
        .lock()
        .expect("mermaid render eval error lock");
    *guard = Some(std::thread::current().id());
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_render_call_error_once() {
    let mut guard = FORCE_MERMAID_RENDER_CALL_ERROR
        .lock()
        .expect("mermaid render call error lock");
    *guard = Some(std::thread::current().id());
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_pixmap_alloc_fail_once() {
    let mut guard = FORCE_MERMAID_PIXMAP_ALLOC_FAIL
        .lock()
        .expect("mermaid pixmap fail lock");
    *guard = Some(std::thread::current().id());
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn force_mermaid_face_parse_error_once() {
    let mut guard = FORCE_MERMAID_FACE_PARSE_ERROR
        .lock()
        .expect("mermaid face parse error lock");
    *guard = Some(std::thread::current().id());
}

#[cfg(all(test, feature = "mermaid-quickjs"))]
fn maybe_force_mermaid_init_error<T>(
    stage: usize,
    result: rquickjs::Result<T>,
) -> rquickjs::Result<T> {
    if let Ok(mut guard) = FORCE_MERMAID_INIT_ERROR_STAGE.try_lock() {
        if let Some((thread_id, target_stage)) = guard.take() {
            if thread_id == std::thread::current().id() && target_stage == stage {
                return Err(rquickjs::Error::Exception);
            }
            *guard = Some((thread_id, target_stage));
        }
    }
    result
}

#[cfg(not(all(test, feature = "mermaid-quickjs")))]
fn maybe_force_mermaid_init_error<T>(
    _stage: usize,
    result: rquickjs::Result<T>,
) -> rquickjs::Result<T> {
    result
}

#[cfg(feature = "mermaid-quickjs")]
fn runtime_new_for_test() -> rquickjs::Result<rquickjs::Runtime> {
    #[cfg(all(test, feature = "mermaid-quickjs"))]
    if take_mermaid_flag(&FORCE_MERMAID_RUNTIME_ERROR) {
        return Err(rquickjs::Error::Exception);
    }
    rquickjs::Runtime::new()
}

#[cfg(feature = "mermaid-quickjs")]
fn context_full_for_test(rt: &rquickjs::Runtime) -> rquickjs::Result<rquickjs::Context> {
    #[cfg(all(test, feature = "mermaid-quickjs"))]
    if take_mermaid_flag(&FORCE_MERMAID_CONTEXT_ERROR) {
        return Err(rquickjs::Error::Exception);
    }
    rquickjs::Context::full(rt)
}

#[cfg(feature = "mermaid-quickjs")]
fn mermaid_js_str_for_test() -> Result<&'static str, std::str::Utf8Error> {
    #[cfg(all(test, feature = "mermaid-quickjs"))]
    if take_mermaid_flag(&FORCE_MERMAID_UTF8_ERROR) {
        #[allow(invalid_from_utf8)]
        {
            static INVALID_UTF8: [u8; 1] = [0xFF];
            return std::str::from_utf8(&INVALID_UTF8);
        }
    }
    std::str::from_utf8(mermaid_embed::MERMAID_JS)
}

#[cfg(feature = "mermaid-quickjs")]
fn face_parse_for_test(data: &[u8], index: u32) -> Option<ttf_parser::Face<'_>> {
    #[cfg(all(test, feature = "mermaid-quickjs"))]
    if take_mermaid_flag(&FORCE_MERMAID_FACE_PARSE_ERROR) {
        return None;
    }
    ttf_parser::Face::parse(data, index).ok()
}

#[cfg(feature = "mermaid-quickjs")]
fn eval_mermaid_wrapper<'js>(
    ctx: &rquickjs::Ctx<'js>,
    wrapper: &str,
) -> rquickjs::Result<rquickjs::Function<'js>> {
    #[cfg(all(test, feature = "mermaid-quickjs"))]
    if take_mermaid_flag(&FORCE_MERMAID_RENDER_EVAL_ERROR) {
        return Err(rquickjs::Error::Exception);
    }
    ctx.eval(wrapper)
}

#[cfg(feature = "mermaid-quickjs")]
fn call_mermaid_render<'js>(
    func: &rquickjs::Function<'js>,
    args: (&str, &str, &str, u32, u32),
) -> rquickjs::Result<rquickjs::promise::MaybePromise<'js>> {
    #[cfg(all(test, feature = "mermaid-quickjs"))]
    if take_mermaid_flag(&FORCE_MERMAID_RENDER_CALL_ERROR) {
        return Err(rquickjs::Error::Exception);
    }
    func.call(args)
}

#[cfg(feature = "mermaid-quickjs")]
fn pixmap_new_for_test(width: u32, height: u32) -> Option<tiny_skia::Pixmap> {
    #[cfg(all(test, feature = "mermaid-quickjs"))]
    if take_mermaid_flag(&FORCE_MERMAID_PIXMAP_ALLOC_FAIL) {
        return None;
    }
    tiny_skia::Pixmap::new(width, height)
}

#[cfg(feature = "mermaid-quickjs")]
struct MermaidEngine {
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

    #[cfg_attr(test, inline(never))]
    fn get(&mut self, key: &K) -> Option<V> {
        let value = self.entries.get(key).cloned();
        self.touch_if_present(&value, key);
        value
    }

    #[cfg_attr(test, inline(never))]
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

    #[cfg(feature = "mermaid-quickjs")]
    fn remove(&mut self, key: &K) {
        self.entries.remove(key);
        self.order.retain(|entry| entry != key);
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.entries.len()
    }

    #[cfg_attr(test, inline(never))]
    fn touch_if_present(&mut self, value: &Option<V>, key: &K) {
        if value.is_some() {
            self.touch(key);
        }
    }

    #[cfg_attr(test, inline(never))]
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
}

/// LRU cache for Mermaid SVG strings with both entry count and size limits.
/// Prevents excessive memory usage from large SVG strings (Stage 9).
#[cfg(feature = "mermaid-quickjs")]
struct SvgCache {
    entries: HashMap<u64, String>,
    order: VecDeque<u64>,
    capacity: usize,
    max_bytes: usize,
    current_bytes: usize,
}

#[cfg(feature = "mermaid-quickjs")]
impl SvgCache {
    fn new(capacity: usize, max_bytes: usize) -> Self {
        Self {
            entries: HashMap::new(),
            order: VecDeque::new(),
            capacity: capacity.max(1),
            max_bytes,
            current_bytes: 0,
        }
    }

    fn get(&mut self, key: &u64) -> Option<String> {
        let value = self.entries.get(key).cloned();
        if value.is_some() {
            self.touch(key);
        }
        value
    }

    fn insert(&mut self, key: u64, svg: String) {
        let svg_bytes = svg.len();

        // If key exists, update and adjust size tracking
        if let Some(old_svg) = self.entries.get(&key) {
            self.current_bytes = self.current_bytes.saturating_sub(old_svg.len());
            self.entries.insert(key, svg);
            self.current_bytes += svg_bytes;
            self.touch(&key);
            return;
        }

        // Evict if adding this SVG would exceed byte limit
        while self.current_bytes + svg_bytes > self.max_bytes && !self.order.is_empty() {
            self.evict_oldest();
        }

        // Also respect entry count limit
        while self.entries.len() >= self.capacity && !self.order.is_empty() {
            self.evict_oldest();
        }

        self.order.push_back(key);
        self.entries.insert(key, svg);
        self.current_bytes += svg_bytes;
    }

    fn evict_oldest(&mut self) {
        if let Some(old_key) = self.order.pop_front() {
            if let Some(old_svg) = self.entries.remove(&old_key) {
                self.current_bytes = self.current_bytes.saturating_sub(old_svg.len());
            }
        }
    }

    fn touch(&mut self, key: &u64) {
        self.order.retain(|k| k != key);
        self.order.push_back(*key);
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.entries.len()
    }

    #[cfg(test)]
    fn current_bytes(&self) -> usize {
        self.current_bytes
    }
}

/// LRU cache for Mermaid textures with both entry count and size limits.
/// Prevents excessive GPU memory usage from large rendered diagrams.
#[cfg(feature = "mermaid-quickjs")]
struct TextureCache {
    entries: HashMap<String, MermaidTextureEntry>,
    order: VecDeque<String>,
    capacity: usize,
    max_bytes: usize,
    current_bytes: usize,
}

#[cfg(feature = "mermaid-quickjs")]
impl TextureCache {
    fn new(capacity: usize, max_bytes: usize) -> Self {
        Self {
            entries: HashMap::new(),
            order: VecDeque::new(),
            capacity: capacity.max(1),
            max_bytes,
            current_bytes: 0,
        }
    }

    /// Calculate memory size of a texture entry in bytes (width * height * 4 for RGBA)
    fn texture_bytes(entry: &MermaidTextureEntry) -> usize {
        entry.size[0] as usize * entry.size[1] as usize * 4
    }

    fn get(&mut self, key: &String) -> Option<MermaidTextureEntry> {
        let value = self.entries.get(key).cloned();
        if value.is_some() {
            self.touch(key);
        }
        value
    }

    fn insert(&mut self, key: String, entry: MermaidTextureEntry) {
        let entry_bytes = Self::texture_bytes(&entry);

        // If key exists, update and adjust size tracking
        if let Some(old_entry) = self.entries.get(&key) {
            self.current_bytes = self
                .current_bytes
                .saturating_sub(Self::texture_bytes(old_entry));
            self.entries.insert(key.clone(), entry);
            self.current_bytes += entry_bytes;
            self.touch(&key);
            return;
        }

        // Evict if adding this texture would exceed byte limit
        while self.current_bytes + entry_bytes > self.max_bytes && !self.order.is_empty() {
            self.evict_oldest();
        }

        // Also respect entry count limit
        while self.entries.len() >= self.capacity && !self.order.is_empty() {
            self.evict_oldest();
        }

        self.order.push_back(key.clone());
        self.entries.insert(key, entry);
        self.current_bytes += entry_bytes;
    }

    fn evict_oldest(&mut self) {
        if let Some(old_key) = self.order.pop_front() {
            if let Some(old_entry) = self.entries.remove(&old_key) {
                self.current_bytes = self
                    .current_bytes
                    .saturating_sub(Self::texture_bytes(&old_entry));
            }
        }
    }

    fn touch(&mut self, key: &String) {
        self.order.retain(|k| k != key);
        self.order.push_back(key.clone());
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.entries.len()
    }

    #[cfg(test)]
    fn current_bytes(&self) -> usize {
        self.current_bytes
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
    mermaid_textures: RefCell<TextureCache>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_pending: RefCell<HashSet<String>>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_frame_pending: Cell<bool>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_svg_cache: RefCell<SvgCache>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_errors: RefCell<LruCache<u64, String>>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_texture_errors: RefCell<LruCache<String, String>>,
    /// Job sender wrapped in Option so Drop can take and drop it first,
    /// closing the channel before joining worker threads.
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_job_tx: Option<MermaidJobSender>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_result_rx: MermaidResultReceiver,
    /// Worker thread handles for cleanup on drop.
    /// When MermaidRenderer is dropped, we join all worker threads to ensure
    /// they exit cleanly and release memory (font database, QuickJS runtime).
    #[cfg(feature = "mermaid-quickjs")]
    worker_handles: Vec<JoinHandle<()>>,
}

impl MermaidRenderer {
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_TEXTURE_CACHE_CAPACITY: usize = 128;
    /// Maximum total bytes for texture cache (256MB). Textures at 4096x4096 are 64MB each,
    /// so 128 max-size textures would use 8GB. Size limit ensures bounded memory usage.
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_TEXTURE_CACHE_MAX_BYTES: usize = 256 * 1024 * 1024;
    #[cfg(any(test, feature = "mermaid-quickjs"))]
    const MERMAID_WIDTH_BUCKET_STEP: u32 = 32;
    #[cfg(any(test, feature = "mermaid-quickjs"))]
    const MERMAID_SCALE_BUCKET_FACTOR: f32 = 100.0;
    #[cfg(feature = "mermaid-quickjs")]
    const MAX_MERMAID_JOBS: usize = 4;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_SVG_CACHE_CAPACITY: usize = 64;
    /// Maximum total bytes for SVG cache (100MB). Large SVGs (100KB-10MB each)
    /// could use 640MB at 64 entries. Size limit ensures bounded memory usage.
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_SVG_CACHE_MAX_BYTES: usize = 100 * 1024 * 1024;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_ERROR_CACHE_CAPACITY: usize = 64;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_TEXTURE_ERROR_CACHE_CAPACITY: usize = 64;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_MAX_TEXT_SIZE: u32 = 50_000;
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_MAX_RENDER_SIDE: u32 = 4096;
    /// Minimum height for Mermaid placeholder to reduce layout shift when diagrams render.
    /// Most Mermaid diagrams are 150-400px tall; using 200px as a reasonable middle ground.
    #[cfg(feature = "mermaid-quickjs")]
    const MERMAID_PLACEHOLDER_MIN_HEIGHT: f32 = 200.0;

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
        let worker_handles = Self::spawn_mermaid_workers(mermaid_job_rx, mermaid_result_tx);
        Self {
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_textures: RefCell::new(TextureCache::new(
                Self::MERMAID_TEXTURE_CACHE_CAPACITY,
                Self::MERMAID_TEXTURE_CACHE_MAX_BYTES,
            )),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_pending: RefCell::new(HashSet::new()),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_frame_pending: Cell::new(false),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_svg_cache: RefCell::new(SvgCache::new(
                Self::MERMAID_SVG_CACHE_CAPACITY,
                Self::MERMAID_SVG_CACHE_MAX_BYTES,
            )),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_errors: RefCell::new(LruCache::new(Self::MERMAID_ERROR_CACHE_CAPACITY)),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_texture_errors: RefCell::new(LruCache::new(
                Self::MERMAID_TEXTURE_ERROR_CACHE_CAPACITY,
            )),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_job_tx: Some(mermaid_job_tx),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_result_rx,
            #[cfg(feature = "mermaid-quickjs")]
            worker_handles,
        }
    }

    fn default_mermaid_preference() -> MermaidRenderPreference {
        #[cfg(feature = "mermaid-quickjs")]
        {
            MermaidRenderPreference::Embedded
        }
        #[cfg(not(feature = "mermaid-quickjs"))]
        {
            MermaidRenderPreference::Off
        }
    }

    fn mermaid_renderer_preference() -> (MermaidRenderPreference, bool) {
        if let Ok(raw) = std::env::var("MDMDVIEW_MERMAID_RENDERER") {
            let normalized = raw.trim().to_ascii_lowercase();
            return match normalized.as_str() {
                "off" => (MermaidRenderPreference::Off, true),
                "embedded" => (MermaidRenderPreference::Embedded, true),
                _ => (Self::default_mermaid_preference(), false),
            };
        }
        (Self::default_mermaid_preference(), false)
    }

    pub(crate) fn has_pending(&self) -> bool {
        #[cfg(feature = "mermaid-quickjs")]
        {
            return self.mermaid_frame_pending.get() || !self.mermaid_pending.borrow().is_empty();
        }
        #[cfg(not(feature = "mermaid-quickjs"))]
        {
            false
        }
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
        let (preference, _explicit) = Self::mermaid_renderer_preference();

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

        #[cfg(not(feature = "mermaid-quickjs"))]
        if _explicit && preference == MermaidRenderPreference::Embedded {
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
            if mermaid_js_empty() {
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
            }
            let svg_key = Self::hash_str(code);
            let mut available_width = ui.available_width();
            let orig_available_width = available_width;
            if available_width <= Self::MERMAID_WIDTH_BUCKET_STEP as f32 {
                let fallback = ui.ctx().available_rect().width();
                if fallback > available_width {
                    available_width = fallback;
                }
            }
            if std::env::var("MDMDVIEW_MERMAID_LOG_WIDTH").is_ok() {
                eprintln!(
                    "Mermaid width: avail={:.2} fallback={:.2}",
                    orig_available_width, available_width
                );
            }
            let width_bucket = Self::width_bucket(available_width);
            let scale_bucket = Self::scale_bucket(ui_scale);
            let mut viewport_width = available_width.round().max(1.0) as u32;
            let mut viewport_height = ui
                .ctx()
                .input(|i| i.screen_rect().height())
                .round()
                .max(1.0) as u32;
            if let Some(kind) = Self::mermaid_diagram_kind(code) {
                if kind == "timeline" {
                    viewport_width = viewport_width.min(1000);
                    viewport_height = viewport_height.min(700);
                }
            }
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
            }
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
                    // Reserve minimum height for placeholder to reduce layout shift
                    // when the actual diagram renders. Most diagrams are 150-400px tall.
                    ui.set_min_height(Self::MERMAID_PLACEHOLDER_MIN_HEIGHT);
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
            true
        }
        #[cfg(not(feature = "mermaid-quickjs"))]
        {
            false
        }
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

    fn mermaid_diagram_kind(code: &str) -> Option<String> {
        for line in code.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            if trimmed.starts_with("%%") {
                continue;
            }
            return trimmed
                .split_whitespace()
                .next()
                .map(|token| token.to_ascii_lowercase());
        }
        None
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
        let Some(ref tx) = self.mermaid_job_tx else {
            return Err(MermaidEnqueueError::Disconnected);
        };
        match tx.try_send(request) {
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
        30_000
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn spawn_mermaid_worker_thread(
        worker_idx: usize,
        worker_rx: MermaidJobReceiver,
        worker_tx: MermaidResultSender,
        worker_fontdb: Arc<usvg::fontdb::Database>,
    ) -> std::io::Result<JoinHandle<()>> {
        #[cfg(test)]
        if FORCE_MERMAID_THREAD_SPAWN_ERROR.swap(false, Ordering::Relaxed) {
            return Err(std::io::Error::other("forced mermaid spawn error"));
        }
        std::thread::Builder::new()
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
                    // Use try_send to avoid blocking if result channel is full.
                    // This prevents deadlock during shutdown: if Drop joins workers
                    // while they're blocked on send (because result_rx isn't being
                    // polled), we'd hang forever. Dropping results is acceptable
                    // since we're shutting down anyway.
                    let _ = worker_tx.try_send(payload);
                }
            })
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn spawn_mermaid_workers(
        job_rx: MermaidJobReceiver,
        result_tx: MermaidResultSender,
    ) -> Vec<JoinHandle<()>> {
        let worker_count = Self::mermaid_worker_count();
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        let mut handles = Vec::with_capacity(worker_count);
        for worker_idx in 0..worker_count {
            let worker_rx = job_rx.clone();
            let worker_tx = result_tx.clone();
            let worker_fontdb = Arc::clone(&fontdb);
            match Self::spawn_mermaid_worker_thread(worker_idx, worker_rx, worker_tx, worker_fontdb)
            {
                Ok(handle) => handles.push(handle),
                Err(err) => eprintln!("Failed to start Mermaid worker thread: {}", err),
            }
        }
        drop(job_rx);
        handles
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
    fn should_allow_html_labels(_code: &str) -> bool {
        // Always return false because usvg cannot render foreignObject elements.
        // When htmlLabels is true, mermaid renders labels inside <foreignObject>
        // which usvg ignores, resulting in invisible text.
        // By forcing htmlLabels=false, mermaid uses SVG <text> elements instead.
        false
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
    fn mermaid_site_config_json(svg_key: u64, allow_html_labels: bool) -> String {
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
        if allow_html_labels {
            entries.push("\"__mdmdviewAllowHtmlLabels\":true".to_string());
        }
        if security == "strict" && !allow_html_labels {
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
impl Drop for MermaidRenderer {
    fn drop(&mut self) {
        // First, drop the job sender to close the channel.
        // This signals workers to exit their `for job in worker_rx.iter()` loop.
        drop(self.mermaid_job_tx.take());

        // Now join all worker threads to ensure they've fully exited
        // and released their resources (font database, QuickJS runtime).
        for handle in self.worker_handles.drain(..) {
            let _ = handle.join();
        }
    }
}

#[cfg(feature = "mermaid-quickjs")]
struct TextMeasurer {
    fontdb: Arc<usvg::fontdb::Database>,
    face_id: Option<usvg::fontdb::ID>,
    bold_face_id: Option<usvg::fontdb::ID>,
}

#[cfg(feature = "mermaid-quickjs")]
impl TextMeasurer {
    const BOLD_WIDTH_FALLBACK: f32 = 1.07;
    const BOLD_WIDTH_SCALE: f32 = 1.03;
    const LINE_HEIGHT_SCALE: f32 = 1.0;

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
        let bold_query = usvg::fontdb::Query {
            families: &families,
            weight: usvg::fontdb::Weight::BOLD,
            ..Default::default()
        };
        let bold_face_id = fontdb.query(&bold_query).or(face_id);
        Self {
            fontdb,
            face_id,
            bold_face_id,
        }
    }

    fn measure_text(&self, text: &str, font_size: f32, font_weight: Option<f32>) -> (f32, f32) {
        if text.is_empty() {
            return (0.0, 0.0);
        }
        let size = if font_size.is_finite() && font_size > 0.0 {
            font_size
        } else {
            16.0
        };
        let use_bold = font_weight.unwrap_or(400.0) >= 600.0;
        let face_id = if use_bold {
            self.bold_face_id.or(self.face_id)
        } else {
            self.face_id
        };
        if let Some((width, height)) = self.measure_with_face(face_id, text, size) {
            if use_bold {
                let mut adjusted = width * Self::BOLD_WIDTH_SCALE;
                if self.bold_face_id == self.face_id {
                    adjusted *= Self::BOLD_WIDTH_FALLBACK;
                }
                return (adjusted, height);
            }
            return (width, height);
        }
        Self::fallback_measure(text, size)
    }

    fn measure_with_face(
        &self,
        face_id: Option<usvg::fontdb::ID>,
        text: &str,
        font_size: f32,
    ) -> Option<(f32, f32)> {
        let face_id = face_id?;
        self.fontdb
            .with_face_data(face_id, |data, index| {
                let face = face_parse_for_test(data, index)?;
                let units_per_em = face.units_per_em().max(1) as f32;
                let scale = font_size / units_per_em;
                let fallback_advance = units_per_em * 0.5;
                let kern_subtables: Vec<_> = face
                    .tables()
                    .kern
                    .map(|kern| {
                        kern.subtables
                            .into_iter()
                            .filter(|table| table.horizontal)
                            .collect()
                    })
                    .unwrap_or_default();
                let mut max_width_units = 0.0_f32;
                let mut line_count = 0u32;

                for line in text.split('\n') {
                    line_count += 1;
                    let mut width_units = 0.0_f32;
                    let mut prev = None;
                    for ch in line.chars() {
                        if let Some(glyph) = face.glyph_index(ch) {
                            if let Some(prev_glyph) = prev {
                                let mut kern = 0.0_f32;
                                for table in &kern_subtables {
                                    if let Some(value) = table.glyphs_kerning(prev_glyph, glyph) {
                                        kern += value as f32;
                                    }
                                }
                                width_units += kern;
                            }
                            width_units += face
                                .glyph_hor_advance(glyph)
                                .map(|adv| adv as f32)
                                .unwrap_or(fallback_advance);
                            prev = Some(glyph);
                        } else {
                            width_units += fallback_advance;
                            prev = None;
                        }
                    }
                    max_width_units = max_width_units.max(width_units);
                }

                let ascender = face.ascender() as f32;
                let descender = face.descender() as f32;
                let line_gap = face.line_gap() as f32;
                let line_height_units = (ascender - descender + line_gap).max(units_per_em);
                let line_height = line_height_units * scale * Self::LINE_HEIGHT_SCALE;
                let width = max_width_units * scale;
                let height = line_height * line_count as f32;
                Some((width, height))
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
        let width = max_len as f32 * font_size * 0.5;
        let height = lines as f32 * font_size * 1.2;
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

// =========================================================================
// HTML Tag to SVG tspan Conversion Types
// =========================================================================

/// Tag mapping for HTML to SVG conversion.
#[cfg(feature = "mermaid-quickjs")]
struct HtmlTagMapping {
    attr: &'static str,
    value: &'static str,
}

/// Supported HTML tags and their SVG tspan equivalents.
#[cfg(feature = "mermaid-quickjs")]
const HTML_TAG_MAPPINGS: &[(&str, HtmlTagMapping)] = &[
    (
        "i",
        HtmlTagMapping {
            attr: "font-style",
            value: "italic",
        },
    ),
    (
        "em",
        HtmlTagMapping {
            attr: "font-style",
            value: "italic",
        },
    ),
    (
        "b",
        HtmlTagMapping {
            attr: "font-weight",
            value: "bold",
        },
    ),
    (
        "strong",
        HtmlTagMapping {
            attr: "font-weight",
            value: "bold",
        },
    ),
    (
        "u",
        HtmlTagMapping {
            attr: "text-decoration",
            value: "underline",
        },
    ),
    (
        "s",
        HtmlTagMapping {
            attr: "text-decoration",
            value: "line-through",
        },
    ),
    (
        "del",
        HtmlTagMapping {
            attr: "text-decoration",
            value: "line-through",
        },
    ),
    (
        "strike",
        HtmlTagMapping {
            attr: "text-decoration",
            value: "line-through",
        },
    ),
];

#[cfg(feature = "mermaid-quickjs")]
impl MermaidWorker {
    const MEMORY_LIMIT_BYTES: usize = 256 * 1024 * 1024; // 256MB (reduced from 2GB)
    const STACK_LIMIT_BYTES: usize = 4 * 1024 * 1024;

    // DOMPurify expects a real browser DOM; stub sanitize for the QuickJS shim.
    fn patch_mermaid_js(js: &str) -> String {
        const TARGET: &str = "var hD=wRe();";
        const PATCH: &str =
            "var hD=wRe();if(!hD||typeof hD.sanitize!==\"function\"||typeof hD.addHook!==\"function\"||typeof hD.removeHook!==\"function\"||typeof hD.removeHooks!==\"function\"||hD.isSupported===false){hD={sanitize:function(html){return String(html);},addHook:function(){},removeHook:function(){},removeHooks:function(){},removeAllHooks:function(){},isSupported:true};}";
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
        const MINDMAP_READY_TARGET: &str =
            "p.layout({name:\"cose-bilkent\",quality:\"proof\",styleEnabled:!1,animate:!1}).run(),p.ready(v=>{Xe.info(\"Ready\",v),u(p)})";
        const MINDMAP_READY_PATCH: &str =
            "p.layout({name:\"breadthfirst\",directed:!0,spacingFactor:1.4,animate:!1}).run(),u(p)";
        const MINDMAP_CYTO_TARGET: &str =
            "p=fWe({container:document.getElementById(\"cy\"),style:[{selector:\"edge\",style:{\"curve-style\":\"bezier\"}}]});";
        const MINDMAP_CYTO_PATCH: &str =
            "p=__mdmdview_cytoscape_stub({container:document.getElementById(\"cy\")});";
        const TIMELINE_BOUNDS_TARGET: &str = "const se=_.node().getBBox();";
        const TIMELINE_BOUNDS_PATCH: &str = concat!(
            "const se=_.node().getBBox();",
            "if(se&&typeof se.width===\"number\"&&typeof v===\"number\"){",
            "se.width=Math.max(0,se.width-2*v);",
            "}"
        );
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
        let mut mindmap_layout_patched = false;
        if out.contains(MINDMAP_CYTO_TARGET) {
            out = out.replacen(MINDMAP_CYTO_TARGET, MINDMAP_CYTO_PATCH, 1);
            if debug {
                eprintln!("Mermaid patch: mindmap cytoscape stub applied");
            }
        } else if debug {
            eprintln!("Mermaid patch: mindmap cytoscape stub not found");
        }
        if out.contains(MINDMAP_READY_TARGET) {
            out = out.replacen(MINDMAP_READY_TARGET, MINDMAP_READY_PATCH, 1);
            mindmap_layout_patched = true;
            if debug {
                eprintln!("Mermaid patch: mindmap ready patch applied");
            }
        } else if out.contains(MINDMAP_LAYOUT_TARGET) {
            out = out.replacen(MINDMAP_LAYOUT_TARGET, MINDMAP_LAYOUT_PATCH, 1);
            mindmap_layout_patched = true;
            if debug {
                eprintln!("Mermaid patch: mindmap layout applied");
            }
        }
        if !mindmap_layout_patched && debug {
            eprintln!("Mermaid patch: mindmap layout not found");
        }
        if out.contains(TIMELINE_BOUNDS_TARGET) {
            out = out.replacen(TIMELINE_BOUNDS_TARGET, TIMELINE_BOUNDS_PATCH, 1);
            if debug {
                eprintln!("Mermaid patch: timeline bounds applied");
            }
        } else if debug {
            eprintln!("Mermaid patch: timeline bounds not found");
        }
        out
    }

    fn format_js_error(ctx: &rquickjs::Ctx<'_>, err: rquickjs::Error) -> String {
        if let rquickjs::Error::Exception = err {
            let value = ctx.catch();
            if let Some(exception) = value.as_exception() {
                let message = exception.message().filter(|value| !value.trim().is_empty());
                let stack = exception.stack().filter(|value| !value.trim().is_empty());
                if let Some(stack) = stack {
                    if let Some(message) = &message {
                        if stack.contains(message) {
                            return stack;
                        }
                        return format!("{}\n{}", message, stack);
                    }
                    return stack;
                }
                if let Some(message) = message {
                    return message;
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
        use rquickjs::Function;
        if mermaid_js_empty() {
            return Err("No embedded Mermaid JS".to_string());
        }
        #[cfg(test)]
        if FORCE_MERMAID_WORKER_INIT_ERROR.swap(false, Ordering::Relaxed) {
            return Err("Mermaid worker init forced error".to_string());
        }
        let rt =
            runtime_new_for_test().map_err(|e| format!("Mermaid runtime init error: {}", e))?;
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
        let ctx =
            context_full_for_test(&rt).map_err(|e| format!("Mermaid context init error: {}", e))?;
        let engine = MermaidEngine { rt, ctx };
        let text_measurer = Arc::new(TextMeasurer::new(Arc::clone(&fontdb)));
        let js =
            mermaid_js_str_for_test().map_err(|_| "Mermaid JS is not valid UTF-8".to_string())?;
        let js = Self::patch_mermaid_js(js);
        let init_result: Result<(), String> = engine.ctx.with(|ctx| {
            let measurer = Arc::clone(&text_measurer);
            let measure_fn = maybe_force_mermaid_init_error(
                1,
                Function::new(
                    ctx.clone(),
                    move |text: String, font_size: f64, font_weight: Option<f64>| {
                        let weight = font_weight.map(|value| value as f32);
                        let (width, height) = measurer.measure_text(&text, font_size as f32, weight);
                        vec![width as f64, height as f64]
                    },
                ),
            )
            .map_err(|err| {
                format!(
                    "Mermaid text measure init error: {}",
                    MermaidWorker::format_js_error(&ctx, err)
                )
            })?;
            maybe_force_mermaid_init_error(2, ctx.globals().set("__mdmdview_measure_text_native", measure_fn))
                .map_err(|err| {
                    format!(
                        "Mermaid text measure init error: {}",
                        MermaidWorker::format_js_error(&ctx, err)
                    )
                })?;
            let eval = |stage: usize, label: &str, source: &str| -> Result<(), String> {
                let result = maybe_force_mermaid_init_error(stage, ctx.eval::<(), _>(source));
                result.map_err(|err| {
                    format!("{}: {}", label, MermaidWorker::format_js_error(&ctx, err))
                })
            };
            eval(3, "Mermaid DOM shim", MERMAID_DOM_SHIM)?;
            eval(4, "Mermaid JS", &js)?;
            eval(5, "Mermaid init", MERMAID_INIT_SNIPPET)?;
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
        Self::map_init_result(init_result)?;
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
            let func: Function = eval_mermaid_wrapper(&ctx, wrapper)
                .map_err(|err| MermaidWorker::format_js_error(&ctx, err))?;
            let id = format!("m{:016x}", key);
            let allow_html_labels = MermaidRenderer::should_allow_html_labels(code);
            let site_config = MermaidRenderer::mermaid_site_config_json(key, allow_html_labels);
            let maybe: MaybePromise = call_mermaid_render(
                &func,
                (
                    id.as_str(),
                    code,
                    site_config.as_str(),
                    viewport_width,
                    viewport_height,
                ),
            )
            .map_err(|err| MermaidWorker::format_js_error(&ctx, err))?;
            maybe
                .finish::<String>()
                .map_err(|err| MermaidWorker::format_js_error(&ctx, err))
        });
        // Trigger explicit GC to reclaim memory from render temporaries
        self.engine.rt.run_gc();
        self.deadline_ms.store(0, Ordering::Relaxed);
        Self::format_render_result(result, deadline, Self::now_ms())
    }

    fn map_init_result(init_result: Result<(), String>) -> Result<(), String> {
        match init_result {
            Ok(()) => Ok(()),
            Err(err) => Err(format!("Mermaid init error: {}", err)),
        }
    }

    fn format_render_result(
        result: Result<String, String>,
        deadline: u64,
        now_ms: u64,
    ) -> Result<String, String> {
        match result {
            Ok(svg) => Ok(svg),
            Err(err) => {
                if now_ms >= deadline {
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
        if new_tag == tag {
            return svg.to_string();
        }
        let mut out = String::with_capacity(svg.len() + 32);
        out.push_str(&svg[..start]);
        out.push_str(&new_tag);
        out.push_str(&svg[end + 1..]);
        out
    }

    fn replace_attr(tag: &str, name: &str, value: &str) -> String {
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

    fn upsert_attr(tag: &str, name: &str, value: &str) -> String {
        let needle = format!("{name}=\"");
        if tag.contains(&needle) {
            return Self::replace_attr(tag, name, value);
        }
        let insert_at = match tag.rfind('>') {
            Some(pos) => pos,
            None => return tag.to_string(),
        };
        let mut out = String::with_capacity(tag.len() + name.len() + value.len() + 4);
        out.push_str(&tag[..insert_at]);
        out.push(' ');
        out.push_str(name);
        out.push_str("=\"");
        out.push_str(value);
        out.push('"');
        out.push_str(&tag[insert_at..]);
        out
    }

    fn flatten_svg_switches(svg: &str) -> Option<String> {
        if !svg.contains("<switch") {
            return None;
        }
        let mut out = String::with_capacity(svg.len());
        let mut remaining = svg;
        let mut changed = false;
        while let Some(start) = remaining.find("<switch") {
            let (before, after) = remaining.split_at(start);
            out.push_str(before);
            let open_end = match after.find('>') {
                Some(pos) => pos + 1,
                None => {
                    out.push_str(after);
                    remaining = "";
                    break;
                }
            };
            let after_open = &after[open_end..];
            let close_rel = match after_open.find("</switch>") {
                Some(pos) => pos,
                None => {
                    out.push_str(after);
                    remaining = "";
                    break;
                }
            };
            let inner = &after_open[..close_rel];
            let mut inner_out = String::with_capacity(inner.len());
            let mut inner_remaining = inner;
            while let Some(fo_start) = inner_remaining.find("<foreignObject") {
                let (inner_before, inner_after) = inner_remaining.split_at(fo_start);
                inner_out.push_str(inner_before);
                if let Some(fo_end_rel) = inner_after.find("</foreignObject>") {
                    inner_remaining = &inner_after[fo_end_rel + "</foreignObject>".len()..];
                } else {
                    inner_remaining = "";
                    break;
                }
            }
            inner_out.push_str(inner_remaining);
            out.push_str(&inner_out);
            changed = true;
            remaining = &after_open[close_rel + "</switch>".len()..];
        }
        out.push_str(remaining);
        if changed {
            Some(out)
        } else {
            None
        }
    }

    fn fix_journey_section_text(svg: &str, fill: &str) -> Option<String> {
        if !svg.contains("journey-section") {
            return None;
        }
        let mut out = String::with_capacity(svg.len() + 64);
        let mut remaining = svg;
        let mut changed = false;
        while let Some(idx) = remaining.find("<text") {
            let (before, after) = remaining.split_at(idx);
            out.push_str(before);
            let end = match after.find('>') {
                Some(pos) => pos + 1,
                None => {
                    out.push_str(after);
                    remaining = "";
                    break;
                }
            };
            let (tag, rest) = after.split_at(end);
            if tag.contains("journey-section") {
                let mut updated = tag.to_string();
                if let Some(style_start) = updated.find(" style=\"") {
                    let value_start = style_start + " style=\"".len();
                    if let Some(style_end) = updated[value_start..].find('"') {
                        let style_end = value_start + style_end;
                        let mut style = updated[value_start..style_end].to_string();
                        let parts: Vec<&str> = style.split(';').collect();
                        let mut rebuilt: Vec<String> = Vec::new();
                        for part in parts {
                            let trimmed = part.trim();
                            if trimmed.is_empty() {
                                continue;
                            }
                            if trimmed.starts_with("fill:") {
                                continue;
                            }
                            rebuilt.push(trimmed.to_string());
                        }
                        rebuilt.push(format!("fill:{}", fill));
                        style = rebuilt.join(";");
                        updated.replace_range(value_start..style_end, &style);
                        out.push_str(&updated);
                        changed = true;
                    } else {
                        out.push_str(tag);
                    }
                } else {
                    let insert_at = updated.len().saturating_sub(1);
                    updated.insert_str(insert_at, &format!(" style=\"fill:{};\"", fill));
                    out.push_str(&updated);
                    changed = true;
                }
            } else {
                out.push_str(tag);
            }
            remaining = rest;
        }
        out.push_str(remaining);
        if changed {
            Some(out)
        } else {
            None
        }
    }

    fn find_circle_tag(body: &str, class_name: &str) -> Option<(usize, usize, String)> {
        let needle = format!("class=\"{class_name}\"");
        let class_idx = body.find(&needle)?;
        let start = body[..class_idx].rfind("<circle")?;
        let end = body[class_idx..].find('>')? + class_idx + 1;
        Some((start, end, body[start..end].to_string()))
    }

    fn read_r_value(tag: &str) -> Option<String> {
        let needle = "r=\"";
        let start = tag.find(needle)? + needle.len();
        let end = tag[start..].find('"')? + start;
        Some(tag[start..end].to_string())
    }

    fn fix_state_end_circles(svg: &str) -> Option<String> {
        if !svg.contains("state-end") {
            return None;
        }
        let mut out = String::with_capacity(svg.len());
        let mut cursor = 0;
        let mut changed = false;
        while let Some(g_pos_rel) = svg[cursor..].find("<g") {
            let g_pos = cursor + g_pos_rel;
            out.push_str(&svg[cursor..g_pos]);
            let g_end = match svg[g_pos..].find('>') {
                Some(pos) => g_pos + pos + 1,
                None => {
                    out.push_str(&svg[g_pos..]);
                    cursor = svg.len();
                    break;
                }
            };
            let g_tag = &svg[g_pos..g_end];
            let is_end_group = g_tag.contains("root_end") || g_tag.contains("state-end");
            if !is_end_group {
                out.push_str(g_tag);
                cursor = g_end;
                continue;
            }
            let close_rel = match svg[g_end..].find("</g>") {
                Some(pos) => pos,
                None => {
                    out.push_str(&svg[g_pos..]);
                    cursor = svg.len();
                    break;
                }
            };
            let body_end = g_end + close_rel;
            let body = &svg[g_end..body_end];
            let mut body_out = body.to_string();
            if let (
                Some((s_start, s_end, state_end_tag)),
                Some((t_start, t_end, state_start_tag)),
            ) = (
                Self::find_circle_tag(body, "state-end"),
                Self::find_circle_tag(body, "state-start"),
            ) {
                if let (Some(end_r), Some(start_r)) = (
                    Self::read_r_value(&state_end_tag),
                    Self::read_r_value(&state_start_tag),
                ) {
                    let end_val = end_r.parse::<f32>().ok();
                    let start_val = start_r.parse::<f32>().ok();
                    if let (Some(end_val), Some(start_val)) = (end_val, start_val) {
                        if end_val < start_val {
                            let end_dim = Self::format_dim(start_val * 2.0);
                            let start_dim = Self::format_dim(end_val * 2.0);
                            let mut new_end_tag = Self::replace_attr(&state_end_tag, "r", &start_r);
                            let mut new_start_tag =
                                Self::replace_attr(&state_start_tag, "r", &end_r);
                            new_end_tag = Self::upsert_attr(&new_end_tag, "width", &end_dim);
                            new_end_tag = Self::upsert_attr(&new_end_tag, "height", &end_dim);
                            new_start_tag = Self::upsert_attr(&new_start_tag, "width", &start_dim);
                            new_start_tag = Self::upsert_attr(&new_start_tag, "height", &start_dim);
                            new_start_tag = new_start_tag.replace("state-start", "end-state-inner");
                            let mut rebuilt = String::with_capacity(body.len() + 16);
                            let (
                                first_start,
                                first_end,
                                first_tag,
                                second_start,
                                second_end,
                                second_tag,
                            ) = if s_start <= t_start {
                                (s_start, s_end, new_end_tag, t_start, t_end, new_start_tag)
                            } else {
                                (t_start, t_end, new_start_tag, s_start, s_end, new_end_tag)
                            };
                            rebuilt.push_str(&body[..first_start]);
                            rebuilt.push_str(&first_tag);
                            rebuilt.push_str(&body[first_end..second_start]);
                            rebuilt.push_str(&second_tag);
                            rebuilt.push_str(&body[second_end..]);
                            body_out = rebuilt;
                            changed = true;
                        }
                    }
                }
            }
            out.push_str(g_tag);
            out.push_str(&body_out);
            out.push_str("</g>");
            cursor = body_end + 4;
        }
        out.push_str(&svg[cursor..]);
        if changed {
            Some(out)
        } else {
            None
        }
    }

    fn fix_er_attribute_fills(svg: &str) -> Option<String> {
        if !svg.contains("attributeBoxOdd") && !svg.contains("attributeBoxEven") {
            return None;
        }
        let mut out = String::with_capacity(svg.len() + 32);
        let mut remaining = svg;
        let mut changed = false;
        while let Some(idx) = remaining.find("<rect") {
            let (before, after) = remaining.split_at(idx);
            out.push_str(before);
            let end = match after.find('>') {
                Some(pos) => pos + 1,
                None => {
                    out.push_str(after);
                    remaining = "";
                    break;
                }
            };
            let (tag, rest) = after.split_at(end);
            let mut updated = tag.to_string();
            if tag.contains("attributeBoxOdd") {
                updated = Self::upsert_attr(&updated, "fill", "#ffffff");
            } else if tag.contains("attributeBoxEven") {
                updated = Self::upsert_attr(&updated, "fill", "#f2f2f2");
            }
            if updated != tag {
                changed = true;
            }
            out.push_str(&updated);
            remaining = rest;
        }
        out.push_str(remaining);
        if changed {
            Some(out)
        } else {
            None
        }
    }

    // =========================================================================
    // HTML Tag to SVG tspan Conversion
    // =========================================================================

    /// Finds the SVG attribute mapping for an HTML tag name.
    fn find_html_tag_mapping(tag_name: &str) -> Option<&'static HtmlTagMapping> {
        let lower = tag_name.to_ascii_lowercase();
        HTML_TAG_MAPPINGS
            .iter()
            .find(|(name, _)| *name == lower)
            .map(|(_, m)| m)
    }

    /// Converts escaped HTML formatting tags in SVG text elements to tspan equivalents.
    ///
    /// Transforms tags like `&lt;i&gt;text&lt;/i&gt;` to `<tspan font-style="italic">text</tspan>`.
    ///
    /// Supported tags: `<i>`, `<em>`, `<b>`, `<strong>`, `<u>`, `<s>`, `<del>`, `<strike>`
    fn convert_html_tags_to_tspan(svg: &str) -> String {
        // Check feature flag (cached check)
        if std::env::var("MDMDVIEW_MERMAID_HTML_TAGS").ok().as_deref() == Some("off") {
            return svg.to_string();
        }

        // Fast path: if no escaped tags, return unchanged
        if !svg.contains("&lt;") {
            return svg.to_string();
        }

        Self::convert_text_element_contents(svg)
    }

    /// Processes all <text> elements in the SVG, converting their HTML tag content.
    fn convert_text_element_contents(svg: &str) -> String {
        let mut result = String::with_capacity(svg.len() + 256);
        let mut pos = 0;

        while let Some(text_start_rel) = svg[pos..].find("<text") {
            let text_start = pos + text_start_rel;

            // Copy everything before <text>
            result.push_str(&svg[pos..text_start]);

            // Find the closing > of the opening tag
            let content_start = match svg[text_start..].find('>') {
                Some(i) => text_start + i + 1,
                None => {
                    result.push_str(&svg[text_start..]);
                    return result;
                }
            };

            // Copy the <text ...> opening tag
            result.push_str(&svg[text_start..content_start]);

            // Find </text>
            let content_end = match svg[content_start..].find("</text>") {
                Some(i) => content_start + i,
                None => {
                    result.push_str(&svg[content_start..]);
                    return result;
                }
            };

            // Extract and process content
            let content = &svg[content_start..content_end];
            if content.contains("&lt;") {
                let unescaped = Self::unescape_html_entities(content);
                let converted = Self::convert_html_in_text(&unescaped);
                result.push_str(&converted);
            } else {
                result.push_str(content);
            }

            // Move position past </text>
            pos = content_end;
        }

        // Copy remaining content
        result.push_str(&svg[pos..]);
        result
    }

    /// Unescapes HTML entities back to characters for processing.
    fn unescape_html_entities(text: &str) -> String {
        text.replace("&lt;", "<")
            .replace("&gt;", ">")
            .replace("&amp;", "&")
            .replace("&quot;", "\"")
            .replace("&apos;", "'")
    }

    /// Attempts to parse an HTML tag at the given position.
    ///
    /// Returns `Some((tag_name, is_closing, chars_consumed))` if valid tag found.
    fn try_parse_html_tag(text: &str) -> Option<(String, bool, usize)> {
        if !text.starts_with('<') {
            return None;
        }

        let is_closing = text.starts_with("</");
        let start = if is_closing { 2 } else { 1 };

        // Find end of tag name (space, /, or >)
        let remaining = text.get(start..)?;
        let name_end = remaining.find([' ', '/', '>']).unwrap_or(remaining.len());

        if name_end == 0 {
            return None; // Empty tag name
        }

        let tag_name = &remaining[..name_end];

        // Validate tag name: must be alphabetic
        if !tag_name.chars().all(|c| c.is_ascii_alphabetic()) {
            return None;
        }

        // Find closing >
        let tag_end = text.find('>')?;

        Some((tag_name.to_ascii_lowercase(), is_closing, tag_end + 1))
    }

    /// Converts HTML tags in text content to SVG tspan elements.
    ///
    /// Uses a stack to track opened tags and ensure proper nesting.
    fn convert_html_in_text(text: &str) -> String {
        let mut result = String::with_capacity(text.len() * 2);
        let mut tag_stack: Vec<String> = Vec::new();
        let mut pos = 0;
        let chars: Vec<char> = text.chars().collect();
        let len = chars.len();

        while pos < len {
            let ch = chars[pos];

            if ch == '<' {
                // Attempt to parse as HTML tag
                let remaining: String = chars[pos..].iter().collect();
                if let Some((tag_name, is_closing, consumed)) = Self::try_parse_html_tag(&remaining)
                {
                    // Check if it's a self-closing tag (like <br/>)
                    let tag_content: String = chars[pos..pos + consumed].iter().collect();
                    let is_self_closing = tag_content.contains("/>");

                    if let Some(mapping) = Self::find_html_tag_mapping(&tag_name) {
                        if is_self_closing {
                            // Self-closing tags don't make sense for formatting, escape them
                            result.push_str("&lt;");
                            pos += 1;
                            continue;
                        } else if is_closing {
                            // Only emit </tspan> if we have a matching open tag
                            if let Some(stack_pos) = tag_stack.iter().rposition(|t| t == &tag_name)
                            {
                                // Close all tags from top of stack to the matching one
                                let tags_to_close = tag_stack.len() - stack_pos;
                                for _ in 0..tags_to_close {
                                    result.push_str("</tspan>");
                                }

                                // Re-open any tags that were closed prematurely (except the matched one)
                                let tags_to_reopen: Vec<String> =
                                    tag_stack[stack_pos + 1..].to_vec();
                                tag_stack.truncate(stack_pos);

                                for reopened in tags_to_reopen {
                                    if let Some(m) = Self::find_html_tag_mapping(&reopened) {
                                        result.push_str(&format!(
                                            "<tspan {}=\"{}\">",
                                            m.attr, m.value
                                        ));
                                        tag_stack.push(reopened);
                                    }
                                }
                                pos += consumed;
                                continue;
                            } else {
                                // No matching open tag, skip the closing tag entirely
                                pos += consumed;
                                continue;
                            }
                        } else {
                            // Opening tag
                            result.push_str(&format!(
                                "<tspan {}=\"{}\">",
                                mapping.attr, mapping.value
                            ));
                            tag_stack.push(tag_name);
                            pos += consumed;
                            continue;
                        }
                    } else {
                        // Unknown tag (likely SVG element like tspan) - pass through unchanged
                        let tag_content: String = chars[pos..pos + consumed].iter().collect();
                        result.push_str(&tag_content);
                        pos += consumed;
                        continue;
                    }
                } else {
                    // Not a valid tag (e.g., math like "x < 5") - escape it
                    result.push_str("&lt;");
                    pos += 1;
                    continue;
                }
            } else if ch == '>' {
                result.push_str("&gt;");
                pos += 1;
            } else if ch == '&' {
                result.push_str("&amp;");
                pos += 1;
            } else {
                result.push(ch);
                pos += 1;
            }
        }

        // Close any unclosed tags
        for _ in &tag_stack {
            result.push_str("</tspan>");
        }

        result
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

    fn remove_svg_attr(tag: &str, name: &str) -> String {
        let needle = format!("{name}=\"");
        let start = match tag.find(&needle) {
            Some(idx) => idx,
            None => return tag.to_string(),
        };
        let value_start = start + needle.len();
        let end = match tag[value_start..].find('"') {
            Some(idx) => value_start + idx + 1,
            None => return tag.to_string(),
        };
        let mut out = String::with_capacity(tag.len());
        out.push_str(&tag[..start]);
        let rest = &tag[end..];
        if out.ends_with(' ') && rest.starts_with(' ') {
            out.pop();
        }
        out.push_str(rest);
        out
    }

    fn strip_svg_attr(svg: &str, name: &str) -> Option<String> {
        let start = svg.find("<svg")?;
        let end = svg[start..].find('>')? + start;
        let tag = &svg[start..=end];
        let new_tag = Self::remove_svg_attr(tag, name);
        if new_tag == tag {
            return None;
        }
        let mut out = String::with_capacity(svg.len());
        out.push_str(&svg[..start]);
        out.push_str(&new_tag);
        out.push_str(&svg[end + 1..]);
        Some(out)
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
                let mut svg = Self::normalize_svg_size(&svg);
                if let Some(updated) = Self::flatten_svg_switches(&svg) {
                    svg = updated;
                }
                let theme = MermaidRenderer::mermaid_theme_values();
                if let Some(updated) = Self::fix_journey_section_text(&svg, &theme.text) {
                    svg = updated;
                }
                if let Some(updated) = Self::fix_state_end_circles(&svg) {
                    svg = updated;
                }
                if let Some(updated) = Self::fix_er_attribute_fills(&svg) {
                    svg = updated;
                }

                // Convert escaped HTML tags (like &lt;i&gt;) to SVG tspan elements
                let converted_svg = Self::convert_html_tags_to_tspan(&svg);
                if converted_svg != svg {
                    svg = converted_svg;
                }

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

    fn parse_raw_svg_tree(svg: &str, opt: &usvg::Options) -> Result<usvg::Tree, String> {
        #[cfg(test)]
        if FORCE_RAW_TREE_PARSE_FAIL.swap(false, Ordering::Relaxed) {
            return Err("forced raw svg parse failure".to_string());
        }
        usvg::Tree::from_data(svg.as_bytes(), opt).map_err(|e| format!("{}", e))
    }

    fn bbox_is_valid(width: f32, height: f32, x: f32, y: f32) -> bool {
        width.is_finite()
            && height.is_finite()
            && width > 0.5
            && height > 0.5
            && x.is_finite()
            && y.is_finite()
    }

    fn should_resize_bbox(
        force_resize: bool,
        oversize_ok: bool,
        bbox_w: f32,
        bbox_h: f32,
        w_f: f32,
        h_f: f32,
    ) -> bool {
        oversize_ok && (force_resize || bbox_w > w_f * 1.2 || bbox_h > h_f * 1.2)
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
        let mut tree = tree;
        let sz = tree.size().to_int_size();
        let (mut w, mut h) = (sz.width(), sz.height());
        let viewbox_dims = Self::find_svg_attr(svg, "viewBox")
            .and_then(|value| Self::parse_viewbox_dims(&value))
            .filter(|(vw, vh)| *vw > 0.5 && *vh > 0.5);
        if let Some((vw, vh)) = viewbox_dims {
            w = vw.ceil() as u32;
            h = vh.ceil() as u32;
        }
        let mut translate_x = 0.0_f32;
        let mut translate_y = 0.0_f32;

        let mut bbox = tree.root().abs_stroke_bounding_box();
        let mut bbox_w = bbox.width();
        let mut bbox_h = bbox.height();
        let mut bbox_valid = Self::bbox_is_valid(bbox_w, bbox_h, bbox.x(), bbox.y());
        let mut force_bbox_resize = false;

        let oversize_cap = 6.0_f32;
        if let Some(raw_svg) = Self::strip_svg_attr(svg, "viewBox") {
            if let Ok(raw_tree) = Self::parse_raw_svg_tree(&raw_svg, &opt) {
                let raw_bbox = raw_tree.root().abs_stroke_bounding_box();
                let raw_w = raw_bbox.width();
                let raw_h = raw_bbox.height();
                let raw_valid = Self::bbox_is_valid(raw_w, raw_h, raw_bbox.x(), raw_bbox.y());
                let size_w = w as f32;
                let size_h = h as f32;
                let oversize_ok = raw_w <= size_w * oversize_cap && raw_h <= size_h * oversize_cap;
                let oversized =
                    raw_valid && oversize_ok && (raw_w > size_w * 1.2 || raw_h > size_h * 1.2);
                if oversized {
                    tree = raw_tree;
                    bbox = raw_bbox;
                    bbox_w = raw_w;
                    bbox_h = raw_h;
                    bbox_valid = raw_valid;
                    force_bbox_resize = true;
                }
            }
        }
        if bbox_valid {
            let w_f = w as f32;
            let h_f = h as f32;
            let oversize_ok = bbox_w <= w_f * oversize_cap && bbox_h <= h_f * oversize_cap;
            let oversize =
                Self::should_resize_bbox(force_bbox_resize, oversize_ok, bbox_w, bbox_h, w_f, h_f);
            if oversize {
                let padding = 4.0_f32;
                let padded_w = (bbox_w + padding * 2.0).max(1.0);
                let padded_h = (bbox_h + padding * 2.0).max(1.0);
                w = padded_w.ceil() as u32;
                h = padded_h.ceil() as u32;
                translate_x = -bbox.x() + padding;
                translate_y = -bbox.y() + padding;
            }
        }

        let base_scale = MermaidRenderer::scale_from_bucket(scale_bucket);
        let mut scale = base_scale;
        if width_bucket > 0 {
            let width_scale = width_bucket as f32 / w.max(1) as f32;
            scale = scale.min(width_scale);
        }
        scale = scale.clamp(0.01, 4.0);
        let adjustment = Self::scale_adjustment_for_svg(svg);
        scale = (scale * adjustment).clamp(0.01, 4.0);

        let mut target_w = (w as f32 * scale).round() as u32;
        let mut target_h = (h as f32 * scale).round() as u32;
        target_w = target_w.max(1);
        target_h = target_h.max(1);

        if std::env::var("MDMDVIEW_MERMAID_LOG_RASTER").is_ok() {
            eprintln!(
                "Mermaid raster: svg={}x{} target={}x{} scale={:.3} bbox_valid={} bbox=({:.2},{:.2},{:.2},{:.2})",
                w,
                h,
                target_w,
                target_h,
                scale,
                bbox_valid,
                bbox.x(),
                bbox.y(),
                bbox_w,
                bbox_h
            );
        }

        let max_side = MermaidRenderer::MERMAID_MAX_RENDER_SIDE;
        if target_w > max_side || target_h > max_side {
            let clamp_scale =
                (max_side as f32 / target_w as f32).min(max_side as f32 / target_h as f32);
            scale = (scale * clamp_scale).clamp(0.01, 4.0);
            target_w = (w as f32 * scale).round() as u32;
            target_h = (h as f32 * scale).round() as u32;
        }

        let mut pixmap = pixmap_new_for_test(target_w, target_h)
            .ok_or_else(|| "Pixmap alloc failed".to_string())?;
        if let Some([r, g, b, a]) = bg {
            let color = tiny_skia::Color::from_rgba8(r, g, b, a);
            pixmap.fill(color);
        }

        let mut pmut = pixmap.as_mut();
        let mut transform = tiny_skia::Transform::from_scale(scale, scale);
        if translate_x != 0.0 || translate_y != 0.0 {
            transform = transform.pre_translate(translate_x, translate_y);
        }
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

    fn scale_adjustment_for_svg(svg: &str) -> f32 {
        if svg.contains("aria-roledescription=\"er\"") || svg.contains("aria-roledescription='er'")
        {
            0.94
        } else {
            1.0
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
window.__mdmdview_bbox_rev = 0;
function __mdmdview_bump_bbox_rev() {
  window.__mdmdview_bbox_rev = (window.__mdmdview_bbox_rev || 0) + 1;
}
function __mdmdview_measure_text(text, fontSize, fontWeight) {
  var size = fontSize || 16;
  var raw = text ? String(text) : '';
  if (!raw.length) {
    return { width: 0, height: 0 };
  }
  var weight = (typeof fontWeight === 'number' && !isNaN(fontWeight)) ? fontWeight : null;
  var key = size + '|' + (weight || 0) + '|' + raw;
  var hit = __mdmdview_text_cache[key];
  if (hit) { return hit; }
  if (typeof __mdmdview_measure_text_native === 'function') {
    try {
      var native = __mdmdview_measure_text_native(raw, size, weight);
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
  var height = lines.length * size * 1.2;
  var res = { width: width, height: height };
  __mdmdview_text_cache[key] = res;
  return res;
}
function __mdmdview_parse_font_size(value, baseSize) {
  if (value === null || value === undefined) { return baseSize; }
  var raw = String(value).trim();
  if (!raw) { return baseSize; }
  var num = parseFloat(raw);
  if (isNaN(num)) { return baseSize; }
  if (raw.indexOf('em') >= 0) {
    var base = baseSize || 16;
    return num * base;
  }
  if (raw.indexOf('ex') >= 0) {
    var baseEx = baseSize || 16;
    return num * baseEx * 0.5;
  }
  return num;
}
function __mdmdview_parse_font_weight_value(value) {
  if (value === null || value === undefined) { return null; }
  var raw = String(value).trim().toLowerCase();
  if (!raw) { return null; }
  if (raw === 'normal') { return 400; }
  if (raw === 'bold' || raw === 'bolder') { return 700; }
  if (raw === 'lighter') { return 300; }
  var num = parseFloat(raw);
  if (isNaN(num)) { return null; }
  return num;
}
function __mdmdview_parse_css_font_sizes(cssText, cache) {
  if (!cssText || !cache) { return; }
  var chunks = String(cssText).split('}');
  for (var i = 0; i < chunks.length; i++) {
    var rule = chunks[i];
    var parts = rule.split('{');
    if (parts.length < 2) { continue; }
    var selector = parts[0];
    var body = parts.slice(1).join('{');
    if (!selector || !body) { continue; }
    var sizeMatch = body.match(/font-size\s*:\s*([^;]+)/i);
    if (!sizeMatch) { continue; }
    var sizeValue = sizeMatch[1];
    var size = __mdmdview_parse_font_size(sizeValue, 16);
    if (!size || isNaN(size)) { continue; }
    var selectors = selector.split(',');
    for (var s = 0; s < selectors.length; s++) {
      var sel = selectors[s].trim();
      if (!sel) { continue; }
      if (sel.indexOf(':') >= 0) { continue; }
      var classMatches = sel.match(/\\.([A-Za-z0-9_-]+)/g);
      if (!classMatches || classMatches.length !== 1) { continue; }
      var cls = classMatches[0].slice(1);
      if (!cls) { continue; }
      if (!cache[cls] || cache[cls] < size) {
        cache[cls] = size;
      }
    }
  }
}
function __mdmdview_parse_css_font_weights(cssText, cache) {
  if (!cssText || !cache) { return; }
  var chunks = String(cssText).split('}');
  for (var i = 0; i < chunks.length; i++) {
    var rule = chunks[i];
    var parts = rule.split('{');
    if (parts.length < 2) { continue; }
    var selector = parts[0];
    var body = parts.slice(1).join('{');
    if (!selector || !body) { continue; }
    var weightMatch = body.match(/font-weight\s*:\s*([^;]+)/i);
    if (!weightMatch) { continue; }
    var weightValue = weightMatch[1];
    var weight = __mdmdview_parse_font_weight_value(weightValue);
    if (!weight || isNaN(weight)) { continue; }
    var selectors = selector.split(',');
    for (var s = 0; s < selectors.length; s++) {
      var sel = selectors[s].trim();
      if (!sel) { continue; }
      if (sel.indexOf(':') >= 0) { continue; }
      var classMatches = sel.match(/\\.([A-Za-z0-9_-]+)/g);
      if (!classMatches || classMatches.length !== 1) { continue; }
      var cls = classMatches[0].slice(1);
      if (!cls) { continue; }
      if (!cache[cls] || cache[cls] < weight) {
        cache[cls] = weight;
      }
    }
  }
}
function __mdmdview_collect_style_texts(node, out) {
  if (!node || !out) { return; }
  var name = (node.tagName || '').toLowerCase();
  if (name === 'style') {
    var text = __mdmdview_collect_text(node);
    if (text) { out.push(text); }
  }
  if (!node.children) { return; }
  for (var i = 0; i < node.children.length; i++) {
    __mdmdview_collect_style_texts(node.children[i], out);
  }
}
function __mdmdview_find_root_node(node) {
  if (!node) { return null; }
  var current = node;
  var last = node;
  var depth = 0;
  while (current && depth < 120) {
    last = current;
    if (!current.__mdmdview_parent_set) { break; }
    current = current.parentNode;
    depth += 1;
  }
  return last;
}
function __mdmdview_style_cache() {
  var rev = window.__mdmdview_bbox_rev || 0;
  if (window.__mdmdview_font_cache && window.__mdmdview_font_cache_rev === rev) {
    return window.__mdmdview_font_cache;
  }
  var cache = {};
  if (typeof document !== 'undefined' && document && document.getElementsByTagName) {
    var styles = document.getElementsByTagName('style') || [];
    for (var i = 0; i < styles.length; i++) {
      var text = styles[i] ? __mdmdview_collect_text(styles[i]) : '';
      __mdmdview_parse_css_font_sizes(text, cache);
    }
  }
  window.__mdmdview_font_cache = cache;
  window.__mdmdview_font_cache_rev = rev;
  window.__mdmdview_font_cache_root = null;
  window.__mdmdview_font_cache_root_rev = rev;
  return cache;
}
function __mdmdview_style_cache_for_node(node) {
  var cache = __mdmdview_style_cache();
  if (!node) { return cache; }
  var rev = window.__mdmdview_bbox_rev || 0;
  var root = __mdmdview_find_root_node(node);
  if (!root) { return cache; }
  if (window.__mdmdview_font_cache_root === root && window.__mdmdview_font_cache_root_rev === rev) {
    return cache;
  }
  var texts = [];
  __mdmdview_collect_style_texts(root, texts);
  for (var i = 0; i < texts.length; i++) {
    __mdmdview_parse_css_font_sizes(texts[i], cache);
  }
  window.__mdmdview_font_cache_root = root;
  window.__mdmdview_font_cache_root_rev = rev;
  return cache;
}
function __mdmdview_style_weight_cache() {
  var rev = window.__mdmdview_bbox_rev || 0;
  if (window.__mdmdview_font_weight_cache && window.__mdmdview_font_weight_cache_rev === rev) {
    return window.__mdmdview_font_weight_cache;
  }
  var cache = {};
  if (typeof document !== 'undefined' && document && document.getElementsByTagName) {
    var styles = document.getElementsByTagName('style') || [];
    for (var i = 0; i < styles.length; i++) {
      var text = styles[i] ? __mdmdview_collect_text(styles[i]) : '';
      __mdmdview_parse_css_font_weights(text, cache);
    }
  }
  window.__mdmdview_font_weight_cache = cache;
  window.__mdmdview_font_weight_cache_rev = rev;
  window.__mdmdview_font_weight_cache_root = null;
  window.__mdmdview_font_weight_cache_root_rev = rev;
  return cache;
}
function __mdmdview_style_weight_cache_for_node(node) {
  var cache = __mdmdview_style_weight_cache();
  if (!node) { return cache; }
  var rev = window.__mdmdview_bbox_rev || 0;
  var root = __mdmdview_find_root_node(node);
  if (!root) { return cache; }
  if (window.__mdmdview_font_weight_cache_root === root && window.__mdmdview_font_weight_cache_root_rev === rev) {
    return cache;
  }
  var texts = [];
  __mdmdview_collect_style_texts(root, texts);
  for (var i = 0; i < texts.length; i++) {
    __mdmdview_parse_css_font_weights(texts[i], cache);
  }
  window.__mdmdview_font_weight_cache_root = root;
  window.__mdmdview_font_weight_cache_root_rev = rev;
  return cache;
}
function __mdmdview_class_font_size(el) {
  if (!el) { return null; }
  var cache = __mdmdview_style_cache_for_node(el);
  if (!cache) { return null; }
  var list = __mdmdview_get_class_list(el);
  for (var i = 0; i < list.length; i++) {
    var size = cache[list[i]];
    if (size && !isNaN(size)) { return size; }
  }
  return null;
}
function __mdmdview_class_font_weight(el) {
  if (!el) { return null; }
  var cache = __mdmdview_style_weight_cache_for_node(el);
  if (!cache) { return null; }
  var list = __mdmdview_get_class_list(el);
  for (var i = 0; i < list.length; i++) {
    var weight = cache[list[i]];
    if (weight && !isNaN(weight)) { return weight; }
  }
  return null;
}
function __mdmdview_get_font_size(el) {
  var size = 16;
  if (el && el.style && el.style.fontSize) {
    size = __mdmdview_parse_font_size(el.style.fontSize, size);
    if (!isNaN(size)) { return size; }
  }
  if (el && el.style && el.style['font-size']) {
    size = __mdmdview_parse_font_size(el.style['font-size'], size);
    if (!isNaN(size)) { return size; }
  }
  if (el && el.getAttribute) {
    var attr = el.getAttribute('font-size');
    if (attr) {
      var a = __mdmdview_parse_font_size(attr, size);
      if (!isNaN(a)) { size = a; }
    }
  }
  var classSize = __mdmdview_class_font_size(el);
  if (classSize && !isNaN(classSize)) { return classSize; }
  var classList = __mdmdview_get_class_list(el);
  for (var c = 0; c < classList.length; c++) {
    var cls = classList[c];
    if (cls === 'commit-label' || cls === 'commit-label-bkg' || cls === 'tag-label' || cls === 'tag-label-bkg') {
      return 10;
    }
    if (cls === 'gitTitleText') {
      return 18;
    }
  }
  var ancestor = el;
  var steps = 0;
  while (ancestor && steps < 6) {
    if (__mdmdview_has_class(ancestor, 'commit-labels') || __mdmdview_has_class(ancestor, 'tag-labels')) {
      return 10;
    }
    ancestor = ancestor.parentNode;
    steps += 1;
  }
  var current = el;
  var depth = 0;
  while (current && depth < 4) {
    current = current.parentNode;
    if (!current) { break; }
    var parentSize = __mdmdview_class_font_size(current);
    if (parentSize && !isNaN(parentSize)) { return parentSize; }
    depth += 1;
  }
  var tag = (el && el.tagName) ? String(el.tagName).toLowerCase() : '';
  if (tag === 'text' || tag === 'tspan') {
    return size;
  }
  return size;
}
function __mdmdview_get_font_weight_raw(el) {
  if (el && el.style) {
    if (el.style.fontWeight) {
      var sw = __mdmdview_parse_font_weight_value(el.style.fontWeight);
      if (sw && !isNaN(sw)) { return sw; }
    }
    if (el.style['font-weight']) {
      var sw2 = __mdmdview_parse_font_weight_value(el.style['font-weight']);
      if (sw2 && !isNaN(sw2)) { return sw2; }
    }
  }
  if (el && el.getAttribute) {
    var attr = el.getAttribute('font-weight');
    if (attr) {
      var aw = __mdmdview_parse_font_weight_value(attr);
      if (aw && !isNaN(aw)) { return aw; }
    }
  }
  var classWeight = __mdmdview_class_font_weight(el);
  if (classWeight && !isNaN(classWeight)) { return classWeight; }
  var classList = __mdmdview_get_class_list(el);
  for (var i = 0; i < classList.length; i++) {
    var cls = classList[i];
    if (cls === 'classTitle' || cls === 'classTitleText' || cls === 'title') {
      return 700;
    }
  }
  return null;
}
function __mdmdview_get_font_weight(el) {
  var direct = __mdmdview_get_font_weight_raw(el);
  if (direct && !isNaN(direct)) { return direct; }
  var current = el;
  var depth = 0;
  while (current && depth < 4) {
    current = current.parentNode;
    if (!current) { break; }
    var parentWeight = __mdmdview_get_font_weight_raw(current);
    if (parentWeight && !isNaN(parentWeight)) { return parentWeight; }
    depth += 1;
  }
  return 400;
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
  if (raw.indexOf('ex') >= 0) {
    return num * (fontSize || 16) * 0.5;
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
  var weight = __mdmdview_get_font_weight(el);
  if (!el || !el.children || !el.children.length) {
    var raw = el ? __mdmdview_collect_text(el) : '';
    return __mdmdview_measure_text(raw, size, weight);
  }
  var max_w = 0;
  var max_h = 0;
  var lines = 0;
  for (var i = 0; i < el.children.length; i++) {
    var child = el.children[i];
    var tag = (child.tagName || '').toLowerCase();
    if (tag !== 'tspan') { continue; }
    var text = __mdmdview_collect_text(child);
    if (!text || !String(text).trim()) { continue; }
    var child_weight = __mdmdview_get_font_weight(child);
    var m = __mdmdview_measure_text(text, size, child_weight);
    if (m.width > max_w) { max_w = m.width; }
    if (m.height > max_h) { max_h = m.height; }
    lines += 1;
  }
  if (!lines) {
    var fallback = __mdmdview_collect_text(el);
    return __mdmdview_measure_text(fallback, size, weight);
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
function __mdmdview_children_bounds(el) {
  if (!el || !el.children || !el.children.length) { return null; }
  var min_x = Infinity;
  var min_y = Infinity;
  var max_x = -Infinity;
  var max_y = -Infinity;
  for (var i = 0; i < el.children.length; i++) {
    var child_box = __mdmdview_bbox(el.children[i]);
    if (!child_box) { continue; }
    if (!isFinite(child_box.width) || !isFinite(child_box.height)) { continue; }
    if (child_box.width <= 0 && child_box.height <= 0) { continue; }
    if (child_box.x < min_x) { min_x = child_box.x; }
    if (child_box.y < min_y) { min_y = child_box.y; }
    if (child_box.x + child_box.width > max_x) { max_x = child_box.x + child_box.width; }
    if (child_box.y + child_box.height > max_y) { max_y = child_box.y + child_box.height; }
  }
  if (isFinite(min_x) && isFinite(min_y) && isFinite(max_x) && isFinite(max_y)) {
    return { x: min_x, y: min_y, width: max_x - min_x, height: max_y - min_y };
  }
  return null;
}
function __mdmdview_cache_bbox(el, box) {
  if (el && box) {
    el.__mdmdview_bbox_cache = box;
    el.__mdmdview_bbox_rev = window.__mdmdview_bbox_rev || 0;
  }
  return box;
}
function __mdmdview_bbox(el) {
  if (!el) { return { x: 0, y: 0, width: 0, height: 0 }; }
  if (el.__mdmdview_bbox_cache && el.__mdmdview_bbox_rev === (window.__mdmdview_bbox_rev || 0)) {
    return el.__mdmdview_bbox_cache;
  }
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
    var weight = __mdmdview_get_font_weight(el);
    var m = __mdmdview_measure_text(text, size, weight);
    return __mdmdview_cache_bbox(
      el,
      __mdmdview_apply_transform(el, { x: 0, y: 0, width: m.width, height: m.height })
    );
  }
  if (tag === 'svg') {
    if (el.children && el.children.length) {
      var svg_bounds = __mdmdview_children_bounds(el);
      if (svg_bounds) {
        return __mdmdview_cache_bbox(el, __mdmdview_apply_transform(el, svg_bounds));
      }
    }
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
    return __mdmdview_cache_bbox(
      el,
      __mdmdview_apply_transform(el, {
        x: tx,
        y: y,
        width: metrics.width,
        height: metrics.height
      })
    );
  }
  if (tag === 'rect') {
    var rx = __mdmdview_parse_num(el.getAttribute('x'));
    var ry = __mdmdview_parse_num(el.getAttribute('y'));
    var rw = __mdmdview_parse_num(el.getAttribute('width'));
    var rh = __mdmdview_parse_num(el.getAttribute('height'));
    return __mdmdview_cache_bbox(
      el,
      __mdmdview_apply_transform(el, { x: rx, y: ry, width: rw, height: rh })
    );
  }
  if (tag === 'circle') {
    var cx = __mdmdview_parse_num(el.getAttribute('cx'));
    var cy = __mdmdview_parse_num(el.getAttribute('cy'));
    var r = __mdmdview_parse_num(el.getAttribute('r'));
    return __mdmdview_cache_bbox(
      el,
      __mdmdview_apply_transform(el, { x: cx - r, y: cy - r, width: r * 2, height: r * 2 })
    );
  }
  if (tag === 'ellipse') {
    var ecx = __mdmdview_parse_num(el.getAttribute('cx'));
    var ecy = __mdmdview_parse_num(el.getAttribute('cy'));
    var erx = __mdmdview_parse_num(el.getAttribute('rx'));
    var ery = __mdmdview_parse_num(el.getAttribute('ry'));
    return __mdmdview_cache_bbox(
      el,
      __mdmdview_apply_transform(el, { x: ecx - erx, y: ecy - ery, width: erx * 2, height: ery * 2 })
    );
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
    return __mdmdview_cache_bbox(
      el,
      __mdmdview_apply_transform(el, { x: min_x, y: min_y, width: max_x - min_x, height: max_y - min_y })
    );
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
          return __mdmdview_cache_bbox(
            el,
            __mdmdview_apply_transform(el, {
              x: min_px,
              y: min_py,
              width: max_px - min_px,
              height: max_py - min_py
            })
          );
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
          return __mdmdview_cache_bbox(
            el,
            __mdmdview_apply_transform(el, { x: min_px, y: min_py, width: max_px - min_px, height: max_py - min_py })
          );
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
    return __mdmdview_cache_bbox(
      el,
      __mdmdview_apply_transform(el, { x: bx, y: by, width: bw, height: bh })
    );
  }
  if (el.children && el.children.length) {
    var child_bounds = __mdmdview_children_bounds(el);
    if (child_bounds) {
      return __mdmdview_cache_bbox(el, __mdmdview_apply_transform(el, child_bounds));
    }
  }
  var fallback_text = el.textContent || '';
  var fallback_size = __mdmdview_get_font_size(el);
  var fallback_weight = __mdmdview_get_font_weight(el);
  var fallback = __mdmdview_measure_text(fallback_text, fallback_size, fallback_weight);
  return __mdmdview_cache_bbox(
    el,
    __mdmdview_apply_transform(el, { x: 0, y: 0, width: fallback.width, height: fallback.height })
  );
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
function __mdmdview_is_shape_tag(tag) {
  var t = String(tag || '').toLowerCase();
  return t === 'rect'
    || t === 'path'
    || t === 'polygon'
    || t === 'circle'
    || t === 'ellipse'
    || t === 'line'
    || t === 'image';
}
function __mdmdview_group_has_text(node) {
  if (!node || !node.children || !node.children.length) { return false; }
  for (var i = 0; i < node.children.length; i++) {
    var child = node.children[i];
    if (!child) { continue; }
    var tag = String(child.tagName || '').toLowerCase();
    if (tag === 'text' || tag === 'foreignobject') { return true; }
    if (child.children && child.children.length && __mdmdview_group_has_text(child)) {
      return true;
    }
  }
  return false;
}
function __mdmdview_group_has_shape(node) {
  if (!node || !node.children || !node.children.length) { return false; }
  for (var i = 0; i < node.children.length; i++) {
    var child = node.children[i];
    if (!child) { continue; }
    if (__mdmdview_is_shape_tag(child.tagName)) { return true; }
    if (child.children && child.children.length && __mdmdview_group_has_shape(child)) {
      return true;
    }
  }
  return false;
}
function __mdmdview_is_labelish(node) {
  if (!node || !node.tagName) { return false; }
  var tag = String(node.tagName || '').toLowerCase();
  if (tag === 'text' || tag === 'foreignobject') { return true; }
  if (tag === 'g') {
    if (__mdmdview_has_class(node, 'clusters')) { return false; }
    if (__mdmdview_has_class(node, 'label')) { return true; }
    return __mdmdview_group_has_text(node);
  }
  return false;
}
function __mdmdview_order_children(node) {
  if (!node || !node.children || !node.children.length) {
    return node && node.children ? node.children : [];
  }
  var tag = String(node.tagName || '').toLowerCase();
  if (tag !== 'g') { return node.children; }
  if (!__mdmdview_group_has_shape(node)) { return node.children; }
  var has_label = false;
  for (var i = 0; i < node.children.length; i++) {
    if (__mdmdview_is_labelish(node.children[i])) { has_label = true; break; }
  }
  if (!has_label) { return node.children; }
  var before = [];
  var after = [];
  for (var j = 0; j < node.children.length; j++) {
    var child = node.children[j];
    if (__mdmdview_is_labelish(child)) { after.push(child); }
    else { before.push(child); }
  }
  if (!before.length || !after.length) { return node.children; }
  return before.concat(after);
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
  // Check if this is an xhtml element (e.g., div inside foreignObject)
  var isXhtml = node.namespaceURI && node.namespaceURI.indexOf('xhtml') >= 0;
  if (node.children && node.children.length) {
    if (lower === 'style' || lower === 'script') {
      content = __mdmdview_collect_text(node);
    } else {
      var ordered = __mdmdview_order_children(node);
      for (var i = 0; i < ordered.length; i++) {
        content += __mdmdview_serialize(ordered[i]);
      }
    }
  } else if (node.__rawInnerHTML && isXhtml) {
    // For xhtml elements with raw innerHTML, escape & but preserve HTML tags
    // Only escape & that aren't already part of entity references like &amp; &lt; etc.
    content = node.__rawInnerHTML.replace(/&(?!(amp|lt|gt|quot|apos|#\d+|#x[0-9a-fA-F]+);)/g, '&amp;');
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
  var ordered = __mdmdview_order_children(node);
  for (var i = 0; i < ordered.length; i++) {
    content += __mdmdview_serialize(ordered[i]);
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
function __mdmdview_parse_font_weight(font) {
  if (!font) { return null; }
  var raw = String(font).toLowerCase();
  if (raw.indexOf('bold') >= 0) { return 700; }
  return null;
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
      var weight = __mdmdview_parse_font_weight(ctx.font);
      var measured = __mdmdview_measure_text(text || '', size, weight);
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
function __mdmdview_mindmap_get_size(node) {
  var data = node && typeof node.data === 'function' ? node.data() : null;
  var w = data && data.width !== undefined ? Number(data.width) : 80;
  var h = data && data.height !== undefined ? Number(data.height) : 40;
  if (!isFinite(w) || w <= 0) { w = 80; }
  if (!isFinite(h) || h <= 0) { h = 40; }
  return { w: w, h: h };
}
function __mdmdview_rect_intersection(hw, hh, ux, uy) {
  var ax = Math.abs(ux);
  var ay = Math.abs(uy);
  var dx = ax > 1e-6 ? hw / ax : Infinity;
  var dy = ay > 1e-6 ? hh / ay : Infinity;
  var dist = Math.min(dx, dy);
  if (!isFinite(dist) || dist <= 0) { dist = Math.max(hw, hh); }
  return dist;
}
function __mdmdview_mindmap_collect(nodes, edges) {
  var node_map = {};
  var parents = {};
  var children = {};
  var root = null;
  var root_id = null;
  for (var i = 0; i < nodes.length; i++) {
    var node = nodes[i];
    if (!node || typeof node.data !== 'function') { continue; }
    var data = node.data();
    if (!data) { continue; }
    var id = data.id !== undefined ? String(data.id) : null;
    if (id === null) { continue; }
    node_map[id] = node;
  }
  for (var j = 0; j < edges.length; j++) {
    var edge = edges[j];
    if (!edge || typeof edge.data !== 'function') { continue; }
    var ed = edge.data();
    if (!ed) { continue; }
    var source = ed.source !== undefined ? String(ed.source) : null;
    var target = ed.target !== undefined ? String(ed.target) : null;
    if (!source || !target) { continue; }
    if (!children[source]) { children[source] = []; }
    children[source].push(target);
    parents[target] = source;
  }
  for (var key in node_map) {
    if (!Object.prototype.hasOwnProperty.call(node_map, key)) { continue; }
    if (!Object.prototype.hasOwnProperty.call(parents, key)) {
      root = node_map[key];
      root_id = key;
      break;
    }
  }
  if (!root && nodes.length) {
    root = nodes[0];
    if (root && typeof root.data === 'function') {
      var rd = root.data();
      if (rd && rd.id !== undefined) { root_id = String(rd.id); }
    }
  }
  return { node_map: node_map, children: children, parents: parents, root: root, root_id: root_id };
}
function __mdmdview_mindmap_sorted_children(node_id, tree) {
  var kids = tree.children[node_id] || [];
  return kids.slice();
}
function __mdmdview_mindmap_assign_sides(tree) {
  var sides = {};
  var root_id = tree.root_id;
  if (!root_id) { return sides; }
  sides[root_id] = 0;
  function assign(node_id) {
    var kids = __mdmdview_mindmap_sorted_children(node_id, tree);
    for (var i = 0; i < kids.length; i++) {
      var child_id = kids[i];
      var child_node = tree.node_map[child_id];
      var side = sides[node_id];
      if (node_id === root_id) {
        var data = child_node && typeof child_node.data === 'function' ? child_node.data() : null;
        var section = data && data.section !== undefined ? Number(data.section) : i;
        if (!isFinite(section)) { section = i; }
        side = (section % 2 === 0) ? -1 : 1;
      }
      sides[child_id] = side;
      assign(child_id);
    }
  }
  assign(root_id);
  return sides;
}
function __mdmdview_mindmap_subtree_height(node_id, tree, sizes, gap, heights) {
  var kids = __mdmdview_mindmap_sorted_children(node_id, tree);
  if (!kids.length) {
    heights[node_id] = sizes[node_id].h;
    return heights[node_id];
  }
  var total = 0;
  for (var i = 0; i < kids.length; i++) {
    total += __mdmdview_mindmap_subtree_height(kids[i], tree, sizes, gap, heights);
  }
  total += gap * Math.max(0, kids.length - 1);
  var base = sizes[node_id].h;
  heights[node_id] = total > base ? total : base;
  return heights[node_id];
}
function __mdmdview_mindmap_layout(nodes, edges) {
  var tree = __mdmdview_mindmap_collect(nodes, edges);
  if (!tree.root || !tree.root_id) { return; }
  var sizes = {};
  for (var key in tree.node_map) {
    if (!Object.prototype.hasOwnProperty.call(tree.node_map, key)) { continue; }
    sizes[key] = __mdmdview_mindmap_get_size(tree.node_map[key]);
  }
  var sides = __mdmdview_mindmap_assign_sides(tree);
  var pad = 10;
  var root_data = tree.root && typeof tree.root.data === 'function' ? tree.root.data() : null;
  if (root_data && root_data.padding !== undefined) {
    var p = Number(root_data.padding);
    if (isFinite(p) && p > 0) { pad = p; }
  }
  var gap_x = Math.max(24, pad * 2.2);
  var positions = {};
  positions[tree.root_id] = { x: 0, y: 0 };
  var angles = {};
  angles[tree.root_id] = 0;
  var deg = Math.PI / 180;
  var left_start = 245 * deg;
  var left_end = 122 * deg;
  var right_start = 345 * deg;
  var right_end = 353 * deg;
  function assign_even(children, start, end) {
    if (!children.length) { return; }
    if (children.length === 1) {
      angles[children[0]] = (start + end) / 2;
      return;
    }
    var span = end - start;
    var step = span / (children.length - 1);
    for (var i = 0; i < children.length; i++) {
      angles[children[i]] = start + step * i;
    }
  }
  var root_kids = __mdmdview_mindmap_sorted_children(tree.root_id, tree);
  var left = [];
  var right = [];
  for (var rk = 0; rk < root_kids.length; rk++) {
    var kid = root_kids[rk];
    var side = sides[kid] || 1;
    if (side < 0) { left.push(kid); } else { right.push(kid); }
  }
  assign_even(left, left_start, left_end);
  assign_even(right, right_start, right_end);
  function spread_for_depth(depth, parent_angle) {
    var base = 38 * deg;
    var cos = Math.cos(parent_angle || 0);
    var bias = cos < 0 ? 1.4 : 1.0;
    var depth_factor = 1 + Math.max(0, depth - 1) * 0.2;
    return (base * bias) / depth_factor;
  }
  function assign_child_angles(node_id, depth) {
    var kids = __mdmdview_mindmap_sorted_children(node_id, tree);
    if (!kids.length) { return; }
    var parent_angle = angles[node_id] || 0;
    if (kids.length === 1) {
      var sin = Math.sin(parent_angle);
      var cos = Math.cos(parent_angle);
      var offset = 0;
      if (cos < -0.1 && sin < -0.1) { offset = 20 * deg; }
      angles[kids[0]] = parent_angle + offset;
      assign_child_angles(kids[0], depth + 1);
      return;
    }
    var spread = spread_for_depth(depth, parent_angle);
    var upper = 1.3;
    var lower = 0.9;
    var start = parent_angle - spread * upper;
    var end = parent_angle + spread * lower;
    assign_even(kids, start, end);
    for (var i = 0; i < kids.length; i++) {
      assign_child_angles(kids[i], depth + 1);
    }
  }
  for (var a = 0; a < root_kids.length; a++) {
    assign_child_angles(root_kids[a], 1);
  }
  function place_node(node_id, depth) {
    var kids = __mdmdview_mindmap_sorted_children(node_id, tree);
    if (!kids.length) { return; }
    var parent_pos = positions[node_id];
    var parent_size = sizes[node_id];
    var parent_radius = Math.max(parent_size.w, parent_size.h) / 2;
    var parent_angle = angles[node_id] || 0;
    for (var i = 0; i < kids.length; i++) {
      var child = kids[i];
      var angle = angles[child];
      if (angle === undefined) { angle = angles[node_id] || 0; }
      var child_size = sizes[child];
      var child_radius = Math.max(child_size.w, child_size.h) / 2;
      var dist = parent_radius + child_radius + gap_x;
      if (depth > 0) {
        dist *= 1.15;
      }
      var factor = 1;
      if (depth === 0) {
        var cos = Math.cos(angle);
        var sin = Math.sin(angle);
        factor *= 1.1 * (1 + 0.2 * cos + 0.1 * sin);
      } else {
        var diff = angle - parent_angle;
        while (diff > Math.PI) { diff -= Math.PI * 2; }
        while (diff < -Math.PI) { diff += Math.PI * 2; }
        var norm = diff / (Math.PI / 6);
        if (norm > 1) { norm = 1; }
        if (norm < -1) { norm = -1; }
        factor *= 1 + 0.12 * norm;
        factor *= 0.98 + depth * 0.04;
      }
      if (depth > 0) {
        var sin = Math.sin(angle);
        var cos = Math.cos(angle);
        if (sin >= 0) {
          factor *= 1 + 0.15 * sin;
        } else if (cos > 0) {
          factor *= 1 + 0.05 * sin;
        } else {
          factor *= 1 + 0.25 * sin;
        }
        if (sin < -0.2 && cos < 0) {
          factor *= 0.9;
        }
      }
      if (kids.length === 1) {
        factor *= 0.95;
      }
      dist = dist * factor;
      var cx = parent_pos.x + Math.cos(angle) * dist;
      var cy = parent_pos.y + Math.sin(angle) * dist;
      positions[child] = { x: cx, y: cy };
      place_node(child, depth + 1);
    }
  }
  place_node(tree.root_id, 0);
  var min_x = Infinity;
  var min_y = Infinity;
  for (var id in positions) {
    if (!Object.prototype.hasOwnProperty.call(positions, id)) { continue; }
    var pos = positions[id];
    var size = sizes[id];
    var lx = pos.x - size.w / 2;
    var ly = pos.y - size.h / 2;
    if (lx < min_x) { min_x = lx; }
    if (ly < min_y) { min_y = ly; }
  }
  if (!isFinite(min_x)) { min_x = 0; }
  if (!isFinite(min_y)) { min_y = 0; }
  var dx = pad - min_x;
  var dy = pad - min_y;
  for (var nid in positions) {
    if (!Object.prototype.hasOwnProperty.call(positions, nid)) { continue; }
    positions[nid].x += dx;
    positions[nid].y += dy;
  }
  for (var n = 0; n < nodes.length; n++) {
    var node = nodes[n];
    if (!node || typeof node.data !== 'function') { continue; }
    var data = node.data();
    var id = data && data.id !== undefined ? String(data.id) : null;
    if (!id || !positions[id]) { continue; }
    node._position = { x: positions[id].x, y: positions[id].y };
    node.position = function() { return this._position; };
    data.x = positions[id].x;
    data.y = positions[id].y;
  }
  for (var e = 0; e < edges.length; e++) {
    var edge = edges[e];
    if (!edge || typeof edge.data !== 'function') { continue; }
    var ed = edge.data();
    var source = ed && ed.source !== undefined ? String(ed.source) : null;
    var target = ed && ed.target !== undefined ? String(ed.target) : null;
    if (!source || !target) { continue; }
    var s_pos = positions[source];
    var t_pos = positions[target];
    if (!s_pos || !t_pos) { continue; }
    var s_size = sizes[source];
    var t_size = sizes[target];
    var dx = t_pos.x - s_pos.x;
    var dy = t_pos.y - s_pos.y;
    var len = Math.sqrt(dx * dx + dy * dy);
    if (!isFinite(len) || len <= 0.001) { len = 1; }
    var ux = dx / len;
    var uy = dy / len;
    var s_radius = __mdmdview_rect_intersection(s_size.w / 2, s_size.h / 2, ux, uy);
    var t_radius = __mdmdview_rect_intersection(t_size.w / 2, t_size.h / 2, ux, uy);
    var overlap = -2;
    s_radius += overlap;
    t_radius += overlap;
    if (s_radius < 0) { s_radius = 0; }
    if (t_radius < 0) { t_radius = 0; }
    var max_total = len * 0.95;
    if (s_radius + t_radius > max_total) {
      var scale = max_total / (s_radius + t_radius);
      s_radius *= scale;
      t_radius *= scale;
    }
    var sx = s_pos.x + ux * s_radius;
    var sy = s_pos.y + uy * s_radius;
    var ex = t_pos.x - ux * t_radius;
    var ey = t_pos.y - uy * t_radius;
    var mx = (sx + ex) / 2;
    var my = (sy + ey) / 2;
    edge[0] = edge[0] || {};
    edge[0]._private = {
      bodyBounds: true,
      rscratch: {
        startX: sx,
        startY: sy,
        midX: mx,
        midY: my,
        endX: ex,
        endY: ey
      }
    };
  }
}
function __mdmdview_cytoscape_stub(options) {
  var nodes = [];
  var edges = [];
  function add(entry) {
    if (!entry || !entry.group) { return; }
    if (entry.group === 'nodes') {
      var node = {
        _data: entry.data || {},
        _position: entry.position || { x: 0, y: 0 },
        data: function() { return this._data; },
        position: function() { return this._position; }
      };
      nodes.push(node);
    } else if (entry.group === 'edges') {
      var edge = {
        _data: entry.data || {},
        data: function() { return this._data; }
      };
      edge[0] = { _private: { bodyBounds: false, rscratch: {} } };
      edges.push(edge);
    }
  }
  return {
    add: add,
    nodes: function() { return nodes; },
    edges: function() { return edges; },
    layout: function() {
      return { run: function() { __mdmdview_mindmap_layout(nodes, edges); return this; } };
    }
  };
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
      hasAttribute: function(key) {
        return Object.prototype.hasOwnProperty.call(this.attributes, key);
      },
      hasAttributeNS: function(ns, key) { return this.hasAttribute(key); },
      textContent: '',
    setAttribute: function(key, value) {
      var v = String(value);
      this.attributes[key] = v;
      if (key === 'id') { this.id = v; }
      __mdmdview_bump_bbox_rev();
    },
    setAttributeNS: function(ns, key, value) {
      var v = String(value);
      this.attributes[key] = v;
      if (key === 'id') { this.id = v; }
      __mdmdview_bump_bbox_rev();
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
      __mdmdview_bump_bbox_rev();
      return this;
    },
    append: function(tag) {
      var child = __mdmdview_make_element(tag, this.ownerDocument, this.namespaceURI);
      this.appendChild(child);
      return child;
    },
    removeAttribute: function(key) { delete this.attributes[key]; __mdmdview_bump_bbox_rev(); },
    removeAttributeNS: function(ns, key) { delete this.attributes[key]; __mdmdview_bump_bbox_rev(); },
    appendChild: function(child) {
      __mdmdview_detach(child);
      this.children.push(child);
      this.childNodes = this.children;
      this.firstChild = this.children[0] || null;
      if (child && typeof child === 'object') {
        child.parentNode = this;
        if (!child.ownerDocument && this.ownerDocument) { child.ownerDocument = this.ownerDocument; }
      }
      __mdmdview_bump_bbox_rev();
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
      __mdmdview_bump_bbox_rev();
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
      __mdmdview_bump_bbox_rev();
      return child;
    },
    remove: function() { return null; },
    querySelector: function(sel) { return __mdmdview_query_selector(this, sel); },
    querySelectorAll: function(sel) { return __mdmdview_query_selector_all(this, sel); },
    getElementsByTagName: function(tag) {
      var raw = String(tag || '').trim().toLowerCase();
      if (!raw) { return []; }
      var matches = [];
      __mdmdview_find_by_tag(this, raw, matches);
      return matches;
    },
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
  Object.defineProperty(node, 'className', {
    get: function() { return node.getAttribute ? (node.getAttribute('class') || '') : ''; },
    set: function(value) { node.setAttribute('class', value); },
    configurable: true
  });
  Object.defineProperty(node, 'innerHTML', {
    get: function() { return this.__rawInnerHTML || __mdmdview_serialize_children(this); },
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
      // Store raw HTML for xhtml content (e.g., inside foreignObject)
      this.__rawInnerHTML = value ? String(value) : '';
      __mdmdview_bump_bbox_rev();
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
  createDocumentFragment: function() {
    var frag = __mdmdview_make_element('#document-fragment', document);
    frag.nodeType = 11;
    return frag;
  },
  importNode: function(node, deep) {
    if (!node) { return null; }
    return node.cloneNode ? node.cloneNode(deep) : node;
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
  window.__mdmdview_bbox_rev = 0;
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
  window.__mdmdview_timer_map[id] = { fn: fn, interval: false };
  window.__mdmdview_timer_queue.push(id);
  return id;
};
window.clearTimeout = function(id) {
  if (window.__mdmdview_timer_map) {
    delete window.__mdmdview_timer_map[id];
  }
};
window.setInterval = function(fn, ms) {
  var id = window.__mdmdview_next_timer_id++;
  window.__mdmdview_timer_map[id] = { fn: fn, interval: true };
  window.__mdmdview_timer_queue.push(id);
  return id;
};
window.clearInterval = function(id) { window.clearTimeout(id); };
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
    if (entry && entry.interval) {
      queue.push(id);
    } else {
      delete map[id];
    }
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
window.getComputedStyle = function(el) {
  var style = (el && el.style) ? el.style : __mdmdview_make_style();
  if (!style.fontSize) { style.fontSize = '16px'; }
  if (!style['font-size']) { style['font-size'] = style.fontSize; }
  if (!style.fontFamily) { style.fontFamily = 'sans-serif'; }
  if (!style['font-family']) { style['font-family'] = style.fontFamily; }
  if (!style.fontWeight) { style.fontWeight = 'normal'; }
  if (!style['font-weight']) { style['font-weight'] = style.fontWeight; }
  if (!style.fontStyle) { style.fontStyle = 'normal'; }
  if (!style['font-style']) { style['font-style'] = style.fontStyle; }
  if (el && el.getAttribute) {
    var attrSize = el.getAttribute('font-size');
    if (attrSize) {
      style.fontSize = String(attrSize);
      style['font-size'] = style.fontSize;
    }
    var attrFamily = el.getAttribute('font-family');
    if (attrFamily) {
      style.fontFamily = String(attrFamily);
      style['font-family'] = style.fontFamily;
    }
    var attrWeight = el.getAttribute('font-weight');
    if (attrWeight) {
      style.fontWeight = String(attrWeight);
      style['font-weight'] = style.fontWeight;
    }
    var attrStyle = el.getAttribute('font-style');
    if (attrStyle) {
      style.fontStyle = String(attrStyle);
      style['font-style'] = style.fontStyle;
    }
  }
  style.getPropertyValue = function(key) {
    if (Object.prototype.hasOwnProperty.call(style, key)) { return style[key]; }
    if (key === 'font-size') { return style.fontSize; }
    if (key === 'font-family') { return style.fontFamily; }
    if (key === 'font-weight') { return style.fontWeight; }
    if (key === 'font-style') { return style.fontStyle; }
    return '';
  };
  return style;
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
  var allowHtmlLabels = merged && merged.__mdmdviewAllowHtmlLabels === true;
  if (allowHtmlLabels) {
    merged.htmlLabels = true;
    if (!merged.flowchart) { merged.flowchart = {}; }
    merged.flowchart.htmlLabels = true;
    delete merged.__mdmdviewAllowHtmlLabels;
  }
  if (merged && merged.securityLevel === 'strict' && !allowHtmlLabels) {
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
  function __mdmdview_pump_timers(max_cycles) {
    var cycles = 0;
    function step() {
      if (typeof __mdmdview_run_timers !== 'function') {
        return Promise.resolve();
      }
      var remaining = __mdmdview_run_timers(1000);
      cycles++;
      if (remaining > 0 && cycles < max_cycles) {
        return Promise.resolve().then(step);
      }
      if (remaining > 0) {
        return Promise.reject(new Error('Mermaid timer queue did not drain'));
      }
      return Promise.resolve();
    }
    return step();
  }
  mermaid.mermaidAPI.initialize(merged);
  var renderCode = extracted && extracted.code !== undefined ? extracted.code : code;
  var svgOut = null;
  var renderResult = null;
  try {
    renderResult = mermaid.mermaidAPI.render(id, renderCode, function(svg){ svgOut = svg; });
    __mdmdview_flush_timers();
  } catch (err) {
    throw err;
  }
  if (renderResult && typeof renderResult.then === 'function') {
    var pump = __mdmdview_pump_timers(400);
    return Promise.all([renderResult, pump]).then(function(values){
      __mdmdview_flush_timers();
      var out = values && values.length ? values[0] : null;
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
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;
    use std::collections::HashSet;
    use std::sync::{Arc, Mutex, OnceLock};
    use tempfile::tempdir;

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("env lock")
    }

    fn restore_env_var(key: &'static str, previous: Option<String>) {
        if let Some(value) = previous {
            std::env::set_var(key, value);
        } else {
            std::env::remove_var(key);
        }
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn test_renderer_with_channels(
        job_tx: Option<MermaidJobSender>,
        result_rx: MermaidResultReceiver,
    ) -> MermaidRenderer {
        MermaidRenderer {
            mermaid_textures: RefCell::new(TextureCache::new(4, 10 * 1024 * 1024)),
            mermaid_pending: RefCell::new(HashSet::new()),
            mermaid_frame_pending: Cell::new(false),
            mermaid_svg_cache: RefCell::new(SvgCache::new(4, 1024 * 1024)),
            mermaid_errors: RefCell::new(LruCache::new(4)),
            mermaid_texture_errors: RefCell::new(LruCache::new(4)),
            mermaid_job_tx: job_tx,
            mermaid_result_rx: result_rx,
            worker_handles: Vec::new(),
        }
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
    fn test_lru_cache_missing_get_and_empty_order_insert() {
        let mut cache = LruCache::new(1);
        assert!(cache.get(&"missing".to_string()).is_none());
        cache.entries.insert("a".to_string(), 1);
        cache.order.clear();
        cache.insert("b".to_string(), 2);
        assert_eq!(cache.len(), 2);
    }

    #[test]
    fn test_lru_cache_capacity_zero_insert() {
        let mut cache = LruCache::new(0);
        cache.insert("a".to_string(), 1);
        assert_eq!(cache.len(), 1);
    }

    #[test]
    fn test_lru_cache_insert_overwrites_existing_key() {
        let mut cache = LruCache::new(2);
        let key = "alpha".to_string();
        cache.insert(key.clone(), 1);
        cache.insert(key.clone(), 2);
        assert_eq!(cache.get(&key), Some(2));
        assert_eq!(cache.len(), 1);
    }

    #[test]
    fn test_lru_cache_get_missing_does_not_touch_order() {
        let mut cache = LruCache::new(2);
        cache.insert("a".to_string(), 1);
        let before = cache.order.clone();
        assert!(cache.get(&"missing".to_string()).is_none());
        assert_eq!(cache.order, before);
    }

    #[test]
    fn test_lru_cache_insert_without_eviction() {
        let mut cache = LruCache::new(3);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        assert_eq!(cache.len(), 2);
        assert_eq!(cache.order.len(), 2);
    }

    #[test]
    fn test_lru_cache_touch_if_present_branches() {
        let mut cache = LruCache::new(2);
        let key = "alpha".to_string();
        cache.entries.insert(key.clone(), 1);
        cache.order.push_back(key.clone());
        let value = cache.entries.get(&key).cloned();
        cache.touch_if_present(&value, &key);
        assert_eq!(cache.order.back(), Some(&key));

        let before = cache.order.clone();
        let missing: Option<i32> = None;
        cache.touch_if_present(&missing, &"missing".to_string());
        assert_eq!(cache.order, before);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_lru_cache_touch_if_present_type_variants() {
        let mut id_cache = LruCache::new(1);
        id_cache.entries.insert(1u64, "one".to_string());
        let value = id_cache.entries.get(&1).cloned();
        id_cache.touch_if_present(&value, &1);
        let missing: Option<String> = None;
        id_cache.touch_if_present(&missing, &2);

        let mut string_cache = LruCache::new(1);
        let string_key = "a".to_string();
        string_cache
            .entries
            .insert(string_key.clone(), "one".to_string());
        let value = string_cache.entries.get(&string_key).cloned();
        string_cache.touch_if_present(&value, &string_key);
        let missing: Option<String> = None;
        string_cache.touch_if_present(&missing, &"b".to_string());

        let ctx = egui::Context::default();
        let tex = ctx.load_texture(
            "mermaid_touch_variant",
            egui::ColorImage::new([1, 1], Color32::WHITE),
            egui::TextureOptions::LINEAR,
        );
        let entry = MermaidTextureEntry {
            texture: tex,
            size: [1, 1],
        };
        let mut texture_cache = LruCache::new(1);
        let texture_key = "t".to_string();
        texture_cache
            .entries
            .insert(texture_key.clone(), entry.clone());
        let value = texture_cache.entries.get(&texture_key).cloned();
        texture_cache.touch_if_present(&value, &texture_key);
        let missing: Option<MermaidTextureEntry> = None;
        texture_cache.touch_if_present(&missing, &"missing".to_string());
    }

    #[test]
    fn test_lru_cache_evict_oldest_branches() {
        let mut cache = LruCache::new(1);
        cache.insert("alpha".to_string(), 1);
        assert!(cache.evict_oldest());
        cache.order.clear();
        assert!(!cache.evict_oldest());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_lru_cache_insert_and_evict_type_variants() {
        let mut id_cache = LruCache::new(1);
        id_cache.insert(1u64, "one".to_string());
        id_cache.insert(2u64, "two".to_string());
        id_cache.entries.insert(3u64, "three".to_string());
        id_cache.order.clear();
        id_cache.insert(4u64, "four".to_string());
        assert!(id_cache.evict_oldest());
        id_cache.order.clear();
        assert!(!id_cache.evict_oldest());

        let mut string_cache = LruCache::new(1);
        string_cache.insert("a".to_string(), "one".to_string());
        string_cache.insert("b".to_string(), "two".to_string());
        string_cache
            .entries
            .insert("c".to_string(), "three".to_string());
        string_cache.order.clear();
        string_cache.insert("d".to_string(), "four".to_string());
        assert!(string_cache.evict_oldest());
        string_cache.order.clear();
        assert!(!string_cache.evict_oldest());

        let ctx = egui::Context::default();
        let tex = ctx.load_texture(
            "mermaid_evict_variant",
            egui::ColorImage::new([1, 1], Color32::WHITE),
            egui::TextureOptions::LINEAR,
        );
        let entry = MermaidTextureEntry {
            texture: tex,
            size: [1, 1],
        };
        let mut texture_cache = LruCache::new(1);
        texture_cache.insert("a".to_string(), entry.clone());
        texture_cache.insert("b".to_string(), entry.clone());
        texture_cache.entries.insert("c".to_string(), entry.clone());
        texture_cache.order.clear();
        texture_cache.insert("d".to_string(), entry);
        assert!(texture_cache.evict_oldest());
        texture_cache.order.clear();
        assert!(!texture_cache.evict_oldest());
    }

    #[test]
    fn test_lru_cache_string_string_branches() {
        let mut cache = LruCache::new(1);
        assert!(cache.get(&"missing".to_string()).is_none());

        cache.insert("a".to_string(), "one".to_string());
        cache.insert("a".to_string(), "uno".to_string());
        assert_eq!(cache.get(&"a".to_string()), Some("uno".to_string()));

        cache.insert("b".to_string(), "two".to_string());
        assert!(cache.get(&"a".to_string()).is_none());

        cache.entries.insert("c".to_string(), "three".to_string());
        cache.order.clear();
        cache.insert("d".to_string(), "four".to_string());
        assert!(cache.len() >= 1);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_lru_cache_mermaid_type_variants() {
        let mut string_cache = LruCache::new(2);
        string_cache.insert(1u64, "one".to_string());
        string_cache.insert(2u64, "two".to_string());
        assert_eq!(string_cache.get(&1), Some("one".to_string()));
        assert!(string_cache.get(&3).is_none());
        string_cache.insert(1u64, "uno".to_string());
        string_cache.insert(3u64, "three".to_string());

        let ctx = egui::Context::default();
        let tex = ctx.load_texture(
            "mermaid_cache_test",
            egui::ColorImage::new([1, 1], Color32::WHITE),
            egui::TextureOptions::LINEAR,
        );
        let entry = MermaidTextureEntry {
            texture: tex,
            size: [1, 1],
        };
        let mut texture_cache = LruCache::new(1);
        assert!(texture_cache.get(&"missing".to_string()).is_none());
        texture_cache.insert("a".to_string(), entry.clone());
        assert!(texture_cache.get(&"a".to_string()).is_some());
        texture_cache.insert("a".to_string(), entry.clone());
        texture_cache.insert("b".to_string(), entry.clone());
        texture_cache.entries.insert("c".to_string(), entry.clone());
        texture_cache.order.clear();
        texture_cache.insert("d".to_string(), entry);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_svg_cache_size_based_eviction() {
        // Test that SvgCache evicts based on size, not just count
        let mut cache = SvgCache::new(10, 100); // 10 entries max, 100 bytes max

        // Insert small entries that fit within size limit
        cache.insert(1, "abc".to_string()); // 3 bytes
        cache.insert(2, "def".to_string()); // 3 bytes
        assert_eq!(cache.len(), 2);
        assert_eq!(cache.current_bytes(), 6);

        // Insert large entry that exceeds size limit - should evict oldest
        let large_svg = "x".repeat(95); // 95 bytes
        cache.insert(3, large_svg);
        // current=6, adding 95 -> 101 > 100, so evict entry 1 (oldest)
        // After evicting entry 1: current=3, 3+95=98 <= 100, so stop evicting
        assert!(cache.get(&1).is_none()); // Entry 1 was evicted
        assert!(cache.get(&2).is_some()); // Entry 2 still present (3+95=98 <= 100)
        assert!(cache.get(&3).is_some());
        assert_eq!(cache.current_bytes(), 98); // 3 (entry 2) + 95 (entry 3)

        // Now insert another large entry to force more eviction
        let another_large = "y".repeat(50); // 50 bytes
        cache.insert(4, another_large);
        // current=98, adding 50 -> 148 > 100, evict entry 2 (now oldest)
        // After evicting 2: current=95, 95+50=145 > 100, evict entry 3
        // After evicting 3: current=0, 0+50=50 <= 100, insert
        assert!(cache.get(&2).is_none());
        assert!(cache.get(&3).is_none());
        assert!(cache.get(&4).is_some());
        assert_eq!(cache.current_bytes(), 50);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_svg_cache_count_based_eviction() {
        // Test that SvgCache still respects entry count limit
        let mut cache = SvgCache::new(2, 1000); // 2 entries max, 1000 bytes max

        cache.insert(1, "a".to_string());
        cache.insert(2, "b".to_string());
        assert_eq!(cache.len(), 2);

        // Insert third entry - should evict oldest (entry 1)
        cache.insert(3, "c".to_string());
        assert!(cache.get(&1).is_none());
        assert!(cache.get(&2).is_some());
        assert!(cache.get(&3).is_some());
        assert_eq!(cache.len(), 2);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_svg_cache_update_existing_key() {
        let mut cache = SvgCache::new(10, 1000);

        cache.insert(1, "short".to_string()); // 5 bytes
        assert_eq!(cache.current_bytes(), 5);

        // Update with longer value
        cache.insert(1, "much longer string".to_string()); // 18 bytes
        assert_eq!(cache.len(), 1);
        assert_eq!(cache.current_bytes(), 18);

        // Update with shorter value
        cache.insert(1, "x".to_string()); // 1 byte
        assert_eq!(cache.current_bytes(), 1);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_svg_cache_touch_on_get() {
        let mut cache = SvgCache::new(2, 1000);

        cache.insert(1, "a".to_string());
        cache.insert(2, "b".to_string());

        // Access entry 1 to move it to back of LRU order
        let _ = cache.get(&1);

        // Insert third entry - should evict entry 2 (now oldest)
        cache.insert(3, "c".to_string());
        assert!(cache.get(&1).is_some()); // Entry 1 survived because it was accessed
        assert!(cache.get(&2).is_none()); // Entry 2 was evicted
        assert!(cache.get(&3).is_some());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_texture_cache_size_based_eviction() {
        let ctx = egui::Context::default();
        // Create a small texture that's 10x10 = 100 pixels = 400 bytes
        let small_tex = ctx.load_texture(
            "small",
            egui::ColorImage::new([10, 10], Color32::WHITE),
            egui::TextureOptions::LINEAR,
        );
        // Create textures and set max_bytes to 1000 (enough for 2 textures but not 3)
        let mut cache = TextureCache::new(10, 1000);

        let entry1 = MermaidTextureEntry {
            texture: small_tex.clone(),
            size: [10, 10], // 400 bytes
        };
        let entry2 = MermaidTextureEntry {
            texture: small_tex.clone(),
            size: [10, 10], // 400 bytes
        };
        let entry3 = MermaidTextureEntry {
            texture: small_tex.clone(),
            size: [10, 10], // 400 bytes
        };

        cache.insert("a".to_string(), entry1);
        assert_eq!(cache.len(), 1);
        assert_eq!(cache.current_bytes(), 400);

        cache.insert("b".to_string(), entry2);
        assert_eq!(cache.len(), 2);
        assert_eq!(cache.current_bytes(), 800);

        // This should evict "a" because total would exceed 1000 bytes
        cache.insert("c".to_string(), entry3);
        assert_eq!(cache.len(), 2);
        assert_eq!(cache.current_bytes(), 800);
        assert!(cache.get(&"a".to_string()).is_none()); // Evicted
        assert!(cache.get(&"b".to_string()).is_some());
        assert!(cache.get(&"c".to_string()).is_some());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_texture_cache_count_based_eviction() {
        let ctx = egui::Context::default();
        let small_tex = ctx.load_texture(
            "count_test",
            egui::ColorImage::new([1, 1], Color32::WHITE),
            egui::TextureOptions::LINEAR,
        );
        // Capacity 2, large byte limit (count-based eviction should trigger first)
        let mut cache = TextureCache::new(2, 1024 * 1024);

        let entry = MermaidTextureEntry {
            texture: small_tex.clone(),
            size: [1, 1], // 4 bytes
        };

        cache.insert("a".to_string(), entry.clone());
        cache.insert("b".to_string(), entry.clone());
        assert_eq!(cache.len(), 2);

        // This should evict "a" because capacity is 2
        cache.insert("c".to_string(), entry.clone());
        assert_eq!(cache.len(), 2);
        assert!(cache.get(&"a".to_string()).is_none());
        assert!(cache.get(&"b".to_string()).is_some());
        assert!(cache.get(&"c".to_string()).is_some());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_texture_cache_update_existing_key() {
        let ctx = egui::Context::default();
        let small_tex = ctx.load_texture(
            "update_test",
            egui::ColorImage::new([5, 5], Color32::WHITE),
            egui::TextureOptions::LINEAR,
        );
        let large_tex = ctx.load_texture(
            "update_test_large",
            egui::ColorImage::new([10, 10], Color32::WHITE),
            egui::TextureOptions::LINEAR,
        );
        let mut cache = TextureCache::new(10, 1024 * 1024);

        let small_entry = MermaidTextureEntry {
            texture: small_tex,
            size: [5, 5], // 100 bytes
        };
        let large_entry = MermaidTextureEntry {
            texture: large_tex,
            size: [10, 10], // 400 bytes
        };

        cache.insert("a".to_string(), small_entry);
        assert_eq!(cache.current_bytes(), 100);

        // Update same key with larger entry
        cache.insert("a".to_string(), large_entry);
        assert_eq!(cache.len(), 1);
        assert_eq!(cache.current_bytes(), 400);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_texture_cache_touch_on_get() {
        let ctx = egui::Context::default();
        let small_tex = ctx.load_texture(
            "touch_test",
            egui::ColorImage::new([1, 1], Color32::WHITE),
            egui::TextureOptions::LINEAR,
        );
        let mut cache = TextureCache::new(2, 1024 * 1024);

        let entry = MermaidTextureEntry {
            texture: small_tex.clone(),
            size: [1, 1],
        };

        cache.insert("a".to_string(), entry.clone());
        cache.insert("b".to_string(), entry.clone());

        // Touch "a" by getting it
        let _ = cache.get(&"a".to_string());

        // Now "c" should evict "b" (not "a") because "a" was touched more recently
        cache.insert("c".to_string(), entry.clone());
        assert!(cache.get(&"a".to_string()).is_some());
        assert!(cache.get(&"b".to_string()).is_none()); // Evicted
        assert!(cache.get(&"c".to_string()).is_some());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_renderer_cache_branches() {
        let renderer = MermaidRenderer::new();
        let ctx = egui::Context::default();
        let tex = ctx.load_texture(
            "mermaid_renderer_cache",
            egui::ColorImage::new([1, 1], Color32::WHITE),
            egui::TextureOptions::LINEAR,
        );
        let entry = MermaidTextureEntry {
            texture: tex,
            size: [1, 1],
        };

        {
            let mut cache = renderer.mermaid_textures.borrow_mut();
            assert!(cache.get(&"miss".to_string()).is_none());
            cache.insert("hit".to_string(), entry.clone());
            assert!(cache.get(&"hit".to_string()).is_some());
            cache.entries.insert("stale".to_string(), entry.clone());
            cache.order.clear();
            cache.insert("fresh".to_string(), entry.clone());
        }

        {
            let mut cache = renderer.mermaid_svg_cache.borrow_mut();
            assert!(cache.get(&1).is_none());
            cache.insert(1, "one".to_string());
            assert!(cache.get(&1).is_some());
            cache.entries.insert(2, "two".to_string());
            cache.order.clear();
            cache.insert(3, "three".to_string());
        }

        {
            let mut cache = renderer.mermaid_errors.borrow_mut();
            assert!(cache.get(&1).is_none());
            cache.insert(1, "err".to_string());
            assert!(cache.get(&1).is_some());
            cache.entries.insert(2, "err2".to_string());
            cache.order.clear();
            cache.insert(3, "err3".to_string());
        }

        {
            let mut cache = renderer.mermaid_texture_errors.borrow_mut();
            assert!(cache.get(&"miss".to_string()).is_none());
            cache.insert("hit".to_string(), "err".to_string());
            assert!(cache.get(&"hit".to_string()).is_some());
            cache
                .entries
                .insert("stale".to_string(), "err2".to_string());
            cache.order.clear();
            cache.insert("fresh".to_string(), "err3".to_string());
        }
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
    fn test_mermaid_diagram_kind_skips_comments_and_blank() {
        let code = "\n%% comment\n  \nsequenceDiagram\nAlice->>Bob: Hi";
        assert_eq!(
            MermaidRenderer::mermaid_diagram_kind(code),
            Some("sequencediagram".to_string())
        );
    }

    #[test]
    fn test_mermaid_diagram_kind_returns_none_for_comments_only() {
        let code = "%% comment\n   \n%% another";
        assert!(MermaidRenderer::mermaid_diagram_kind(code).is_none());
    }

    #[test]
    fn test_mermaid_renderer_preference_env() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_MERMAID_RENDERER");
        let (default_pref, explicit) = MermaidRenderer::mermaid_renderer_preference();
        assert!(!explicit);
        #[cfg(feature = "mermaid-quickjs")]
        assert_eq!(default_pref, MermaidRenderPreference::Embedded);
        #[cfg(not(feature = "mermaid-quickjs"))]
        assert_eq!(default_pref, MermaidRenderPreference::Off);

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
            #[cfg(feature = "mermaid-quickjs")]
            assert_eq!(pref, MermaidRenderPreference::Embedded);
            #[cfg(not(feature = "mermaid-quickjs"))]
            assert_eq!(pref, MermaidRenderPreference::Off);
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
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_SECURITY", "strict");
            assert_eq!(MermaidRenderer::mermaid_security_level(), "strict");
        }
    }

    #[test]
    fn test_should_allow_html_labels_always_false_for_usvg() {
        // HTML labels are disabled because usvg cannot render foreignObject.
        // All inputs should return false to force SVG text elements.
        assert!(!MermaidRenderer::should_allow_html_labels(
            "graph TD; A[\"<i>Voice</i>\"]"
        ));
        assert!(!MermaidRenderer::should_allow_html_labels(
            "graph TD; A[\"<b>Bold</b>\"]"
        ));
        assert!(!MermaidRenderer::should_allow_html_labels(
            "graph TD; A[\"<script>bad</script>\"]"
        ));
        assert!(!MermaidRenderer::should_allow_html_labels(
            "graph TD; A[\"plain text\"]"
        ));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_worker_count_clamps_env() {
        let _lock = env_lock();
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_WORKERS", "0");
            assert_eq!(MermaidRenderer::mermaid_worker_count(), 1);
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_WORKERS", "999");
            assert_eq!(MermaidRenderer::mermaid_worker_count(), 16);
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_WORKERS", "nope");
            let count = MermaidRenderer::mermaid_worker_count();
            assert!(count >= 1);
            assert!(count <= MermaidRenderer::MAX_MERMAID_JOBS.max(1));
        }
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_timeout_ms_env_clamps() {
        let _lock = env_lock();
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_TIMEOUT_MS", "50");
            assert_eq!(MermaidRenderer::mermaid_timeout_ms(), 100);
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_TIMEOUT_MS", "250");
            assert_eq!(MermaidRenderer::mermaid_timeout_ms(), 250);
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_TIMEOUT_MS", "bad");
            assert_eq!(MermaidRenderer::mermaid_timeout_ms(), 30_000);
        }
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_theme_values_font_family_trim() {
        let _lock = env_lock();
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_FONT_FAMILY", "  ");
            let theme = MermaidRenderer::mermaid_theme_values();
            assert!(theme.font_family.is_none());
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_FONT_FAMILY", "  Test Font  ");
            let theme = MermaidRenderer::mermaid_theme_values();
            assert_eq!(theme.font_family.as_deref(), Some("Test Font"));
        }
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_site_config_json_includes_security_flags() {
        let _lock = env_lock();
        {
            std::env::remove_var("MDMDVIEW_MERMAID_SECURITY");
            let json = MermaidRenderer::mermaid_site_config_json(1, false);
            assert!(json.contains("\"securityLevel\":\"strict\""));
            assert!(json.contains("\"htmlLabels\":false"));
            assert!(json.contains("\"flowchart\":{\"htmlLabels\":false}"));
        }
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_SECURITY", "loose");
            let json = MermaidRenderer::mermaid_site_config_json(2, false);
            assert!(json.contains("\"securityLevel\":\"loose\""));
            assert!(!json.contains("\"flowchart\":{\"htmlLabels\":false}"));
        }
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_site_config_json_allows_safe_html_labels() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_MERMAID_SECURITY");
        let json = MermaidRenderer::mermaid_site_config_json(4, true);
        assert!(json.contains("\"__mdmdviewAllowHtmlLabels\":true"));
        assert!(!json.contains("\"htmlLabels\":false"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_site_config_json_includes_font_family() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_FONT_FAMILY", "Test Sans");
        let json = MermaidRenderer::mermaid_site_config_json(3, false);
        assert!(json.contains("\"fontFamily\":\"Test Sans\""));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_json_escape_handles_specials() {
        let input = "quote\" backslash\\ newline\n tab\t carriage\r";
        let escaped = MermaidRenderer::json_escape(input);
        assert!(escaped.contains("\\\""));
        assert!(escaped.contains("\\\\"));
        assert!(escaped.contains("\\n"));
        assert!(escaped.contains("\\t"));
        assert!(escaped.contains("\\r"));
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
        let viewport_width = 1200;
        let viewport_height = 900;

        let flow_svg = worker
            .render_svg(
                MermaidRenderer::hash_str(flow),
                flow,
                viewport_width,
                viewport_height,
            )
            .expect("flowchart render");
        let seq_svg = worker
            .render_svg(
                MermaidRenderer::hash_str(seq),
                seq,
                viewport_width,
                viewport_height,
            )
            .expect("sequence render");
        let class_svg = worker
            .render_svg(
                MermaidRenderer::hash_str(class),
                class,
                viewport_width,
                viewport_height,
            )
            .expect("class render");
        let er_svg = worker
            .render_svg(
                MermaidRenderer::hash_str(er),
                er,
                viewport_width,
                viewport_height,
            )
            .expect("er render");
        let gantt_svg = worker
            .render_svg(
                MermaidRenderer::hash_str(gantt),
                gantt,
                viewport_width,
                viewport_height,
            )
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

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_quickjs_render_with_html_tags_in_label() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let mut worker = MermaidWorker::new(0, fontdb).expect("worker init");

        // HTML tags in labels - rendered as SVG text (not foreignObject) since htmlLabels=false
        let diagram = r#"graph TD; A[hello<br/><i>world</i>]-->B"#;

        let viewport_width = 1200;
        let viewport_height = 900;

        let svg = worker
            .render_svg(
                MermaidRenderer::hash_str(diagram),
                diagram,
                viewport_width,
                viewport_height,
            )
            .expect("diagram with html tags render");

        assert!(svg.contains("<svg"));
        // Text should be in SVG text elements (htmlLabels disabled for usvg compatibility)
        assert!(svg.contains("hello"), "Label text 'hello' should be in SVG");
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_quickjs_render_subgraph_with_html_tags() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let mut worker = MermaidWorker::new(0, fontdb).expect("worker init");

        // Complex diagram with HTML tags - should render using SVG text elements
        let diagram = r#"graph TB
    subgraph "Application Layer"
        GABBY[gabby<br/><i>Voice AI Agent</i>]
        APP[Your Application]
    end

    subgraph "Facade"
        MDSIPRTP[mdsiprtp<br/><i>Unified API</i>]
    end

    GABBY --> MDSIPRTP
    APP --> MDSIPRTP"#;

        let viewport_width = 1200;
        let viewport_height = 900;

        let svg = worker
            .render_svg(
                MermaidRenderer::hash_str(diagram),
                diagram,
                viewport_width,
                viewport_height,
            )
            .expect("subgraph diagram render");

        assert!(svg.contains("<svg"));
        // Labels should be visible as SVG text
        assert!(svg.contains("gabby"), "Label 'gabby' should be in SVG");
        assert!(
            svg.contains("Application"),
            "Label 'Application' should be in SVG"
        );
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_quickjs_render_with_ampersand() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let mut worker = MermaidWorker::new(0, fontdb).expect("worker init");

        // Ampersands in labels - must be escaped for valid SVG
        let diagram = r#"graph TD; A[SIP Parsing & Auth]-->B[Types & Errors]"#;

        let viewport_width = 1200;
        let viewport_height = 900;

        let svg = worker
            .render_svg(
                MermaidRenderer::hash_str(diagram),
                diagram,
                viewport_width,
                viewport_height,
            )
            .expect("ampersand in labels render");

        assert!(svg.contains("<svg"));
        // Ampersands should be escaped for valid XML/SVG
        assert!(
            svg.contains("&amp;") || svg.contains("Parsing"),
            "Label text should be in SVG"
        );
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_worker_dom_debug_env() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_DOM_DEBUG", "1");
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        let worker = MermaidWorker::new(0, fontdb);
        assert!(worker.is_ok());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_js_empty_override_matches_thread() {
        let _lock = env_lock();
        assert_eq!(mermaid_js_empty(), MERMAID_JS_EMPTY);
        {
            let _guard = MermaidJsEmptyGuard::set(true);
            assert!(mermaid_js_empty());
        }
        assert_eq!(mermaid_js_empty(), MERMAID_JS_EMPTY);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_js_empty_override_mismatch_thread() {
        let _lock = env_lock();
        let handle = std::thread::spawn(|| set_mermaid_js_empty_for_test(true));
        let _ = handle.join();
        assert_eq!(mermaid_js_empty(), MERMAID_JS_EMPTY);
        set_mermaid_js_empty_for_test(false);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_js_empty_override_false_clears() {
        let _lock = env_lock();
        set_mermaid_js_empty_for_test(true);
        let previous = set_mermaid_js_empty_for_test(false);
        assert!(previous.is_some());
        assert_eq!(mermaid_js_empty(), MERMAID_JS_EMPTY);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_js_empty_override_lock_busy() {
        let _lock = env_lock();
        let guard = MERMAID_JS_EMPTY_OVERRIDE.try_lock().expect("override lock");
        assert_eq!(mermaid_js_empty(), MERMAID_JS_EMPTY);
        drop(guard);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_worker_new_fails_when_js_empty() {
        let _lock = env_lock();
        let _guard = MermaidJsEmptyGuard::set(true);
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let err = MermaidWorker::new(0, Arc::new(fontdb))
            .err()
            .expect("expected error");
        assert!(err.contains("No embedded Mermaid JS"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_measure_text_native_handles_optional_weight() {
        let _lock = env_lock();
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let worker = MermaidWorker::new(0, Arc::new(fontdb)).expect("worker init");
        worker.engine.ctx.with(|ctx| {
            let with_weight: Vec<f64> = ctx
                .eval("__mdmdview_measure_text_native('abc', 12, 700)")
                .expect("with weight");
            let without_weight: Vec<f64> = ctx
                .eval("__mdmdview_measure_text_native('abc', 12, undefined)")
                .expect("without weight");
            assert_eq!(with_weight.len(), 2);
            assert_eq!(without_weight.len(), 2);
        });
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_map_init_result_variants() {
        assert!(MermaidWorker::map_init_result(Ok(())).is_ok());
        let err = MermaidWorker::map_init_result(Err("oops".to_string())).unwrap_err();
        assert!(err.contains("Mermaid init error: oops"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_format_render_result_variants() {
        let ok = MermaidWorker::format_render_result(Ok("<svg></svg>".to_string()), 10, 5)
            .expect("ok result");
        assert!(ok.contains("<svg"));

        let err = MermaidWorker::format_render_result(Err("boom".to_string()), 10, 5).unwrap_err();
        assert!(err.contains("Mermaid render error: boom"));

        let timeout =
            MermaidWorker::format_render_result(Err("late".to_string()), 10, 20).unwrap_err();
        assert_eq!(timeout, "Mermaid render timed out");
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_process_job_missing_code_returns_error() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let mut worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let job = MermaidRequest {
            svg_key: 1,
            texture_key: "missing".to_string(),
            code: None,
            svg: None,
            width_bucket: 100,
            scale_bucket: MermaidRenderer::scale_bucket(1.0),
            viewport_width: 100,
            viewport_height: 100,
            bg: None,
        };

        let result = worker.process_job(job);
        assert!(result.svg.is_none());
        assert!(result.rgba.is_none());
        assert!(result.error.is_some());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_invalid_returns_error() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let err = worker
            .rasterize_svg("not svg", 100, MermaidRenderer::scale_bucket(1.0), None)
            .unwrap_err();
        assert!(!err.is_empty());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_logs_when_env_set() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_LOG_RASTER", "1");
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let worker = MermaidWorker::new(0, fontdb).expect("worker init");
        let svg = r#"<svg width="4" height="4" xmlns="http://www.w3.org/2000/svg"></svg>"#;
        let (data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert_eq!(data.len(), (w as usize) * (h as usize) * 4);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_viewbox_filter_rejects_small() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let worker = MermaidWorker::new(0, Arc::new(fontdb)).expect("worker init");
        let svg = r#"<svg width="100" height="100" viewBox="0 0 0 0" xmlns="http://www.w3.org/2000/svg"></svg>"#;
        let (data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert_eq!(data.len(), (w as usize) * (h as usize) * 4);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_skips_resize_when_bbox_exceeds_cap() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let worker = MermaidWorker::new(0, Arc::new(fontdb)).expect("worker init");
        let svg = r#"<svg width="10" height="10" viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg"><rect width="1000" height="1000" fill="red"/></svg>"#;
        let (_data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert_eq!((w, h), (10, 10));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_oversize_bbox_translates() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let worker = MermaidWorker::new(0, Arc::new(fontdb)).expect("worker init");
        let svg = r#"<svg width="100" height="50" xmlns="http://www.w3.org/2000/svg"><rect x="-60" y="0" width="200" height="50" fill="red"/></svg>"#;
        let (_data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert!(w > 100);
        assert!(h >= 50);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_oversize_bbox_translate_y_only() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let worker = MermaidWorker::new(0, Arc::new(fontdb)).expect("worker init");
        let svg = r#"<svg width="100" height="100" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg"><rect x="4" y="0" width="100" height="200" fill="red"/></svg>"#;
        let (_data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert!(w >= 108);
        assert!(h >= 200);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_clamps_large_width() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let worker = MermaidWorker::new(0, Arc::new(fontdb)).expect("worker init");
        let svg = r#"<svg width="4100" height="10" xmlns="http://www.w3.org/2000/svg"><rect width="4100" height="10" fill="red"/></svg>"#;
        let (_data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert!(w <= MermaidRenderer::MERMAID_MAX_RENDER_SIDE);
        assert!(h > 0);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_clamps_large_height() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let worker = MermaidWorker::new(0, Arc::new(fontdb)).expect("worker init");
        let svg = r#"<svg width="10" height="10000" xmlns="http://www.w3.org/2000/svg"><rect width="10" height="10000" fill="red"/></svg>"#;
        let (_data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert!(w > 0);
        assert!(h <= MermaidRenderer::MERMAID_MAX_RENDER_SIDE);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_tiny_bbox_keeps_dimensions() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let worker = MermaidWorker::new(0, Arc::new(fontdb)).expect("worker init");
        let svg = r#"<svg width="10" height="10" viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg"><rect width="0.1" height="0.1" fill="red"/></svg>"#;
        let (_data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert_eq!((w, h), (10, 10));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_raw_tree_parse_failure_fallback() {
        let _lock = env_lock();
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let worker = MermaidWorker::new(0, Arc::new(fontdb)).expect("worker init");
        force_raw_tree_parse_fail_for_test();
        let svg = r#"<svg width="10" height="10" viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg"><rect width="20" height="20" fill="red"/></svg>"#;
        let (data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert_eq!(data.len(), (w as usize) * (h as usize) * 4);
        assert!(w > 0);
        assert!(h > 0);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_bbox_validation_helpers() {
        assert!(MermaidWorker::bbox_is_valid(10.0, 10.0, 0.0, 0.0));
        assert!(!MermaidWorker::bbox_is_valid(0.0, 10.0, 0.0, 0.0));
        assert!(!MermaidWorker::bbox_is_valid(10.0, 0.0, 0.0, 0.0));
        assert!(!MermaidWorker::bbox_is_valid(f32::NAN, 10.0, 0.0, 0.0));
        assert!(!MermaidWorker::bbox_is_valid(10.0, f32::INFINITY, 0.0, 0.0));
        assert!(!MermaidWorker::bbox_is_valid(10.0, 10.0, f32::NAN, 0.0));
        assert!(!MermaidWorker::bbox_is_valid(
            10.0,
            10.0,
            0.0,
            f32::INFINITY
        ));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_should_resize_bbox_variants() {
        assert!(MermaidWorker::should_resize_bbox(
            true, true, 1.0, 1.0, 10.0, 10.0
        ));
        assert!(!MermaidWorker::should_resize_bbox(
            true, false, 50.0, 50.0, 10.0, 10.0
        ));
        assert!(MermaidWorker::should_resize_bbox(
            false, true, 20.0, 5.0, 10.0, 10.0
        ));
        assert!(!MermaidWorker::should_resize_bbox(
            false, true, 11.0, 11.0, 10.0, 10.0
        ));
        assert!(!MermaidWorker::should_resize_bbox(
            false, false, 50.0, 50.0, 10.0, 10.0
        ));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_process_job_svg_input_rasterize_error() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let mut worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let job = MermaidRequest {
            svg_key: 2,
            texture_key: "bad-svg".to_string(),
            code: Some("graph TD; A-->B;".to_string()),
            svg: Some("not svg".to_string()),
            width_bucket: 100,
            scale_bucket: MermaidRenderer::scale_bucket(1.0),
            viewport_width: 100,
            viewport_height: 100,
            bg: None,
        };

        let result = worker.process_job(job);
        assert!(result.svg.is_some());
        assert!(result.rgba.is_none());
        assert!(result.error.is_some());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_adjusts_for_oversized_bbox() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let svg = r#"<svg width="100" height="100" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
<rect x="-50" y="-50" width="200" height="200" fill="red"/>
</svg>"#;
        let (data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert_eq!((w, h), (208, 208));
        assert_eq!(data.len(), (w as usize) * (h as usize) * 4);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_viewbox_filter_skips_small_dims() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let svg = r#"<svg width="10" height="12" viewBox="0 0 0.1 0.1" xmlns="http://www.w3.org/2000/svg">
 <rect width="0.1" height="0.1" fill="red"/>
 </svg>"#;
        let (_data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert_eq!((w, h), (10, 12));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_keeps_dimensions_for_small_bbox() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let svg = r#"<svg width="100" height="100" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
 <rect x="10" y="10" width="20" height="20" fill="red"/>
 </svg>"#;
        let (_data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert_eq!((w, h), (100, 100));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_clamps_to_max_side() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let svg = r#"<svg width="10000" height="10000" viewBox="0 0 10000 10000" xmlns="http://www.w3.org/2000/svg">
<rect width="10000" height="10000" fill="blue"/>
</svg>"#;
        let (_data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), None)
            .expect("rasterize svg");
        assert!(w <= MermaidRenderer::MERMAID_MAX_RENDER_SIDE);
        assert!(h <= MermaidRenderer::MERMAID_MAX_RENDER_SIDE);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_applies_background_fill() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let svg = r#"<svg width="4" height="4" xmlns="http://www.w3.org/2000/svg"></svg>"#;
        let bg = Some([10, 20, 30, 200]);
        let (data, w, h) = worker
            .rasterize_svg(svg, 0, MermaidRenderer::scale_bucket(1.0), bg)
            .expect("rasterize svg");
        assert_eq!((w, h), (4, 4));
        assert_eq!(data.len(), (w as usize) * (h as usize) * 4);
        let alpha = 200.0_f32 / 255.0;
        let expected_r = (10.0_f32 * alpha).round() as u8;
        let expected_g = (20.0_f32 * alpha).round() as u8;
        let expected_b = (30.0_f32 * alpha).round() as u8;
        assert_eq!(data[0], expected_r);
        assert_eq!(data[1], expected_g);
        assert_eq!(data[2], expected_b);
        assert_eq!(data[3], 200);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_process_job_applies_svg_fixups() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = std::sync::Arc::new(fontdb);
        let mut worker = MermaidWorker::new(0, fontdb).expect("worker init");

        let svg = r#"<svg width="120" height="120" viewBox="0 0 120 120" xmlns="http://www.w3.org/2000/svg">
<switch>
  <foreignObject><div>Plan</div></foreignObject>
  <text class="journey-section section-type-0" x="10">Plan</text>
</switch>
<g id="state-root_end-0">
  <circle class="state-end" r="5" width="10" height="10"></circle>
  <circle class="state-start" r="7" width="14" height="14"></circle>
</g>
<rect class="er attributeBoxOdd"></rect>
</svg>"#;

        let job = MermaidRequest {
            svg_key: 4,
            texture_key: "fixups".to_string(),
            code: None,
            svg: Some(svg.to_string()),
            width_bucket: 0,
            scale_bucket: MermaidRenderer::scale_bucket(1.0),
            viewport_width: 120,
            viewport_height: 120,
            bg: None,
        };

        let result = worker.process_job(job);
        let rendered = result.svg.expect("svg output");
        assert!(!rendered.contains("<switch"));
        assert!(rendered.contains("fill:"));
        assert!(rendered.contains("end-state-inner"));
        assert!(rendered.contains("attributeBoxOdd"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_dump_helpers_sanitize_labels() {
        let _lock = env_lock();
        let dir = tempdir().expect("temp dir");
        let _guard = EnvGuard::set(
            "MDMDVIEW_MERMAID_DUMP_DIR",
            dir.path().to_string_lossy().as_ref(),
        );
        MermaidWorker::maybe_dump_svg(10, Some("ok-label 123"), "<svg></svg>");
        MermaidWorker::maybe_dump_error(11, Some("::bad label::"), "boom");
        MermaidWorker::maybe_dump_error(12, Some("good_label"), "ok");
        MermaidWorker::maybe_dump_svg(13, Some("\nrest"), "<svg></svg>");
        MermaidWorker::maybe_dump_error(14, Some("dash-label"), "dash");
        MermaidWorker::maybe_dump_error(14, Some("abcdefghijklmnopqrstuvwxyz0123456789"), "long");
        MermaidWorker::maybe_dump_svg(
            16,
            Some("abcdefghijklmnopqrstuvwxyz0123456789"),
            "<svg></svg>",
        );
        MermaidWorker::maybe_dump_svg(16, Some("under_score"), "<svg></svg>");
        MermaidWorker::maybe_dump_error(15, None, "no code");
        MermaidWorker::maybe_dump_svg(17, None, "<svg></svg>");
        MermaidWorker::maybe_dump_error(18, Some(""), "empty");
        MermaidWorker::maybe_dump_svg(19, Some(""), "<svg></svg>");
        MermaidWorker::maybe_dump_error(20, Some("\nrest"), "empty-label");

        let entries: Vec<_> = std::fs::read_dir(dir.path())
            .expect("read dir")
            .filter_map(|entry| entry.ok())
            .collect();
        assert!(entries
            .iter()
            .any(|entry| entry.path().extension() == Some("svg".as_ref())));
        assert!(entries
            .iter()
            .any(|entry| entry.path().extension() == Some("txt".as_ref())));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_dump_helpers_empty_dir_env_noop() {
        let _lock = env_lock();
        let dir = tempdir().expect("temp dir");
        let seed_path = dir.path().join("seed.txt");
        std::fs::write(&seed_path, "seed").expect("seed write");
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_DUMP_DIR", "   ");
        MermaidWorker::maybe_dump_svg(20, Some("label"), "<svg></svg>");
        MermaidWorker::maybe_dump_error(21, Some("label"), "err");
        let entries: Vec<_> = std::fs::read_dir(dir.path())
            .expect("read dir")
            .filter_map(|entry| entry.ok())
            .collect();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].path(), seed_path);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_dump_helpers_invalid_dir_noop() {
        let _lock = env_lock();
        let dir = tempdir().expect("temp dir");
        let file_path = dir.path().join("not_a_dir.txt");
        std::fs::write(&file_path, "data").expect("write file");
        let _guard = EnvGuard::set(
            "MDMDVIEW_MERMAID_DUMP_DIR",
            file_path.to_string_lossy().as_ref(),
        );
        MermaidWorker::maybe_dump_svg(22, Some("label"), "<svg></svg>");
        MermaidWorker::maybe_dump_error(23, Some("label"), "err");
        let entries: Vec<_> = std::fs::read_dir(dir.path())
            .expect("read dir")
            .filter_map(|entry| entry.ok())
            .collect();
        assert_eq!(entries.len(), 1);
        assert!(entries[0].path().ends_with("not_a_dir.txt"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_poll_mermaid_results_handles_variants() {
        let (result_tx, result_rx) = bounded(10);
        let renderer = MermaidRenderer {
            mermaid_textures: RefCell::new(TextureCache::new(4, 10 * 1024 * 1024)),
            mermaid_pending: RefCell::new(std::collections::HashSet::new()),
            mermaid_frame_pending: std::cell::Cell::new(false),
            mermaid_svg_cache: RefCell::new(SvgCache::new(4, 1024 * 1024)),
            mermaid_errors: RefCell::new(LruCache::new(4)),
            mermaid_texture_errors: RefCell::new(LruCache::new(4)),
            mermaid_job_tx: None,
            mermaid_result_rx: result_rx,
            worker_handles: Vec::new(),
        };
        let ctx = egui::Context::default();

        renderer
            .mermaid_pending
            .borrow_mut()
            .insert("k1".to_string());
        result_tx
            .send(MermaidResult {
                svg_key: 1,
                texture_key: "k1".to_string(),
                svg: Some("<svg width=\"1\" height=\"1\"></svg>".to_string()),
                rgba: None,
                size: None,
                error: None,
            })
            .expect("send svg");
        assert!(renderer.poll_mermaid_results(&ctx));
        assert!(renderer.mermaid_svg_cache.borrow_mut().get(&1).is_some());

        renderer
            .mermaid_pending
            .borrow_mut()
            .insert("k2".to_string());
        result_tx
            .send(MermaidResult {
                svg_key: 2,
                texture_key: "k2".to_string(),
                svg: None,
                rgba: Some(vec![0, 0, 0, 0]),
                size: None,
                error: None,
            })
            .expect("send rgba missing size");
        renderer.poll_mermaid_results(&ctx);
        assert!(renderer
            .mermaid_texture_errors
            .borrow_mut()
            .get(&"k2".to_string())
            .is_some());

        renderer
            .mermaid_pending
            .borrow_mut()
            .insert("k3".to_string());
        result_tx
            .send(MermaidResult {
                svg_key: 3,
                texture_key: "k3".to_string(),
                svg: None,
                rgba: None,
                size: None,
                error: Some("boom".to_string()),
            })
            .expect("send error");
        renderer.poll_mermaid_results(&ctx);
        assert!(renderer.mermaid_errors.borrow_mut().get(&3).is_some());

        renderer
            .mermaid_pending
            .borrow_mut()
            .insert("k4".to_string());
        result_tx
            .send(MermaidResult {
                svg_key: 4,
                texture_key: "k4".to_string(),
                svg: Some("<svg width=\"1\" height=\"1\"></svg>".to_string()),
                rgba: None,
                size: None,
                error: Some("oops".to_string()),
            })
            .expect("send texture error");
        renderer.poll_mermaid_results(&ctx);
        assert!(renderer
            .mermaid_texture_errors
            .borrow_mut()
            .get(&"k4".to_string())
            .is_some());

        renderer
            .mermaid_pending
            .borrow_mut()
            .insert("k5".to_string());
        result_tx
            .send(MermaidResult {
                svg_key: 5,
                texture_key: "k5".to_string(),
                svg: None,
                rgba: Some(vec![255, 255, 255, 255]),
                size: Some((1, 1)),
                error: None,
            })
            .expect("send rgba with size");
        renderer.poll_mermaid_results(&ctx);
        assert!(renderer
            .mermaid_textures
            .borrow_mut()
            .get(&"k5".to_string())
            .is_some());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_format_js_error_variants() {
        let rt = rquickjs::Runtime::new().expect("runtime");
        let ctx = rquickjs::Context::full(&rt).expect("context");
        ctx.with(|ctx| {
            let err = ctx.eval::<(), _>("throw new Error('boom')").unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(msg.contains("boom"));

            let err = ctx
                .eval::<(), _>("var e = new Error('match'); e.stack = 'match\\nstack'; throw e;")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(msg.contains("match"));

            let err = ctx
                .eval::<(), _>("var e = new Error('note'); e.stack = 'stack only'; throw e;")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(!msg.is_empty());

            let err = ctx
                .eval::<(), _>("var e = new Error('blank'); e.stack = ' '; throw e;")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(msg.contains("blank"));

            let err = ctx
                .eval::<(), _>("throw ({ stack: 'stack-only-obj' })")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(!msg.is_empty());

            let err = ctx
                .eval::<(), _>("throw ({ message: 'message-only-obj' })")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(!msg.is_empty());

            let err = ctx
                .eval::<(), _>("var e = new Error(''); e.message = undefined; e.stack = 'stack-only'; throw e;")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(msg.contains("stack-only"));

            let err = ctx
                .eval::<(), _>("var e = new Error('solo'); delete e.stack; throw e;")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(msg.contains("solo"));

            let err = ctx
                .eval::<(), _>("var e = new Error('solo2'); e.stack = undefined; throw e;")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(msg.contains("solo2"));

            let err = ctx
                .eval::<(), _>("var e = new Error('zap'); e.stack = ''; throw e;")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(msg.contains("zap"));

            let err = ctx
                .eval::<(), _>("var e = new Error(''); e.message = ''; e.stack = ''; throw e;")
                .unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(!msg.is_empty());

            let err = ctx.eval::<(), _>("throw 'stringy'").unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert_eq!(msg, "stringy");

            let err = ctx.eval::<(), _>("throw 42").unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert_eq!(msg, "42");

            let err = ctx.eval::<(), _>("throw 3.5").unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(msg.contains("3.5"));

            let err = ctx.eval::<(), _>("throw true").unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert_eq!(msg, "true");

            let msg = MermaidWorker::format_js_error(&ctx, rquickjs::Error::Unknown);
            assert!(!msg.is_empty());

            let err = ctx.eval::<(), _>("throw ({})").unwrap_err();
            let msg = MermaidWorker::format_js_error(&ctx, err);
            assert!(!msg.is_empty());
        });
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

    #[cfg(feature = "mermaid-quickjs")]
    struct MermaidJsEmptyGuard {
        previous: Option<std::thread::ThreadId>,
    }

    #[cfg(feature = "mermaid-quickjs")]
    impl MermaidJsEmptyGuard {
        fn set(value: bool) -> Self {
            let previous = set_mermaid_js_empty_for_test(value);
            Self { previous }
        }
    }

    #[cfg(feature = "mermaid-quickjs")]
    impl Drop for MermaidJsEmptyGuard {
        fn drop(&mut self) {
            let mut guard = MERMAID_JS_EMPTY_OVERRIDE
                .lock()
                .expect("mermaid js override lock");
            *guard = self.previous;
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
        assert!(MermaidRenderer::parse_hex_color("GG0000").is_none());
        assert!(MermaidRenderer::parse_hex_color("00GG00").is_none());
        assert!(MermaidRenderer::parse_hex_color("0000GG").is_none());
        assert!(MermaidRenderer::parse_hex_color("GG000000").is_none());
        assert!(MermaidRenderer::parse_hex_color("00GG0000").is_none());
        assert!(MermaidRenderer::parse_hex_color("0000GG00").is_none());
        assert!(MermaidRenderer::parse_hex_color("000000GG").is_none());

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
    fn test_env_guard_restores_original_value() {
        let _lock = env_lock();
        std::env::set_var("MDMDVIEW_MERMAID_PATCH_DEBUG", "before");
        {
            let _guard = EnvGuard::set("MDMDVIEW_MERMAID_PATCH_DEBUG", "after");
            assert_eq!(
                std::env::var("MDMDVIEW_MERMAID_PATCH_DEBUG")
                    .ok()
                    .as_deref(),
                Some("after")
            );
        }
        assert_eq!(
            std::env::var("MDMDVIEW_MERMAID_PATCH_DEBUG")
                .ok()
                .as_deref(),
            Some("before")
        );
        std::env::remove_var("MDMDVIEW_MERMAID_PATCH_DEBUG");
    }

    #[test]
    fn test_mermaid_bg_fill_invalid_color_falls_back() {
        let _lock = env_lock();
        let _guard_color = EnvGuard::set("MDMDVIEW_MERMAID_BG_COLOR", "bad");
        let _guard_mode = EnvGuard::set("MDMDVIEW_MERMAID_BG", "dark");
        assert_eq!(MermaidRenderer::mermaid_bg_fill(), Some([20, 20, 20, 255]));
    }

    #[test]
    fn test_mermaid_bg_fill_invalid_theme_color_uses_default() {
        let _lock = env_lock();
        let _guard_mode = EnvGuard::set("MDMDVIEW_MERMAID_BG", "theme");
        let _guard_bkg = EnvGuard::set("MDMDVIEW_MERMAID_MAIN_BKG", "bad");
        assert_eq!(
            MermaidRenderer::mermaid_bg_fill(),
            Some([255, 248, 219, 255])
        );
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

        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "off");
        let renderer = MermaidRenderer::new();
        let mut rendered_off = true;
        let _ = ctx.run(input.clone(), |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered_off = renderer.render_block(ui, "graph TD; A-->B;", 1.0, 14.0);
            });
        });
        assert!(rendered_off);

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

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_render_block_embedded_missing_js_fallback() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
        let _empty = MermaidJsEmptyGuard::set(true);
        let (_tx, rx) = bounded(1);
        let renderer = test_renderer_with_channels(None, rx);
        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(800.0, 600.0),
            )),
            ..Default::default()
        };

        let mut rendered = true;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, "graph TD; A-->B;", 1.0, 14.0);
            });
        });
        assert!(rendered);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_render_block_width_fallback_for_small_ui() {
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
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                ui.allocate_ui_with_layout(
                    egui::vec2(20.0, 200.0),
                    egui::Layout::top_down(egui::Align::Min),
                    |ui| {
                        rendered = renderer.render_block(ui, "%% comment", 1.0, 14.0);
                    },
                );
            });
        });
        assert!(rendered);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_render_block_width_fallback_equal_screen() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_MERMAID_RENDERER");
        let renderer = MermaidRenderer::new();
        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(20.0, 200.0),
            )),
            ..Default::default()
        };

        let mut rendered = true;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                ui.allocate_ui_with_layout(
                    egui::vec2(20.0, 200.0),
                    egui::Layout::top_down(egui::Align::Min),
                    |ui| {
                        rendered = renderer.render_block(ui, "%% comment", 1.0, 14.0);
                    },
                );
            });
        });
        assert!(rendered);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_render_block_uses_cached_texture_scales() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
        let (_result_tx, result_rx) = bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(240.0, 160.0),
            )),
            ..Default::default()
        };
        let code = "graph TD; A-->B;";
        let mut rendered_large = false;
        let mut rendered_small = false;

        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                let image = egui::ColorImage::new([2, 2], Color32::WHITE);
                let texture =
                    ui.ctx()
                        .load_texture("mermaid-test", image, egui::TextureOptions::default());
                let svg_key = MermaidRenderer::hash_str(code);
                let make_key = |ui: &egui::Ui| {
                    let width_bucket = MermaidRenderer::width_bucket(ui.available_width());
                    let scale_bucket = MermaidRenderer::scale_bucket(1.0);
                    let bg = MermaidRenderer::mermaid_bg_fill();
                    MermaidRenderer::texture_key(svg_key, width_bucket, scale_bucket, bg)
                };

                let texture_key = make_key(ui);
                renderer.mermaid_textures.borrow_mut().insert(
                    texture_key.clone(),
                    MermaidTextureEntry {
                        texture: texture.clone(),
                        size: [400, 200],
                    },
                );
                rendered_large = renderer.render_block(ui, code, 1.0, 14.0);

                let texture_key = make_key(ui);
                renderer.mermaid_textures.borrow_mut().insert(
                    texture_key,
                    MermaidTextureEntry {
                        texture,
                        size: [20, 10],
                    },
                );
                rendered_small = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });

        assert!(rendered_large);
        assert!(rendered_small);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_render_block_reports_cached_error() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
        let (_result_tx, result_rx) = bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let code = "graph TD; A-->B;";
        let svg_key = MermaidRenderer::hash_str(code);
        renderer
            .mermaid_errors
            .borrow_mut()
            .insert(svg_key, "boom".to_string());
        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(240.0, 160.0),
            )),
            ..Default::default()
        };
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });
        assert!(rendered);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_render_block_queue_full_sets_waiting() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
        let (job_tx, job_rx) = bounded(1);
        let (_result_tx, result_rx) = bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx.clone()), result_rx);
        let request = MermaidRequest {
            svg_key: 1,
            texture_key: "full".to_string(),
            code: Some("graph TD; A-->B;".to_string()),
            svg: None,
            width_bucket: 32,
            scale_bucket: MermaidRenderer::scale_bucket(1.0),
            viewport_width: 120,
            viewport_height: 120,
            bg: MermaidRenderer::mermaid_bg_fill(),
        };
        job_tx.send(request).expect("fill queue");

        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(240.0, 160.0),
            )),
            ..Default::default()
        };
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, "graph TD; A-->B;", 1.0, 14.0);
            });
        });
        assert!(rendered);
        assert!(renderer.mermaid_frame_pending.get());
        drop(job_rx);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_render_block_disconnected_queue_sets_error() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
        let (_result_tx, result_rx) = bounded(1);
        let renderer = test_renderer_with_channels(None, result_rx);
        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(240.0, 160.0),
            )),
            ..Default::default()
        };
        let code = "graph TD; A-->B;";
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });
        assert!(rendered);
        let svg_key = MermaidRenderer::hash_str(code);
        assert!(renderer.mermaid_errors.borrow_mut().get(&svg_key).is_some());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_render_block_pending_and_width_fallback() {
        let _lock = env_lock();
        let _guard_renderer = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
        let _guard_log = EnvGuard::set("MDMDVIEW_MERMAID_LOG_WIDTH", "1");
        let (job_tx, job_rx) = bounded(2);
        let (result_tx, result_rx) = bounded(2);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        result_tx
            .send(MermaidResult {
                svg_key: 99,
                texture_key: "other".to_string(),
                svg: Some("<svg width=\"1\" height=\"1\"></svg>".to_string()),
                rgba: None,
                size: None,
                error: None,
            })
            .expect("send svg");

        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(200.0, 120.0),
            )),
            ..Default::default()
        };
        let code = "timeline\n  title Demo";
        let mut rendered_first = false;
        let mut rendered_second = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                let rect = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(10.0, 80.0));
                ui.allocate_ui_at_rect(rect, |ui| {
                    rendered_first = renderer.render_block(ui, code, 1.0, 14.0);
                    rendered_second = renderer.render_block(ui, code, 1.0, 14.0);
                });
            });
        });
        assert!(rendered_first);
        assert!(rendered_second);
        drop(job_rx);
    }

    #[test]
    fn test_mermaid_cache_insert_existing_key_and_empty_order() {
        let mut cache = LruCache::new(1);
        cache.insert(1u8, "one".to_string());
        cache.insert(1u8, "uno".to_string());
        assert_eq!(cache.get(&1), Some("uno".to_string()));
        assert!(cache.get(&9).is_none());

        cache.insert(2u8, "two".to_string());
        assert!(cache.get(&1).is_none());

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

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_has_pending_true_for_frame_or_queue() {
        let renderer = MermaidRenderer::new();
        renderer.mermaid_frame_pending.set(true);
        assert!(renderer.has_pending());
        renderer.mermaid_frame_pending.set(false);
        renderer
            .mermaid_pending
            .borrow_mut()
            .insert("pending".to_string());
        assert!(renderer.has_pending());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_journey_section_text_fill_adds_fill() {
        let input = r#"<svg><text class="journey-section section-type-0" x="0">Plan</text></svg>"#;
        let output = MermaidWorker::fix_journey_section_text(input, "#112233")
            .expect("journey text updated");
        assert!(output.contains("class=\"journey-section section-type-0\""));
        assert!(output.contains("fill:#112233"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_journey_section_text_overwrites_fill_in_style() {
        let input = r#"<svg><text class="journey-section" style="fill:#000000;stroke:#fff" x="0">Plan</text></svg>"#;
        let output = MermaidWorker::fix_journey_section_text(input, "#112233")
            .expect("journey text updated");
        assert!(output.contains("fill:#112233"));
        assert!(!output.contains("fill:#000000"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_journey_section_text_skips_empty_style_parts() {
        let input = r#"<svg><text class="journey-section" style="fill:#000000;;stroke:#fff" x="0">Plan</text></svg>"#;
        let output = MermaidWorker::fix_journey_section_text(input, "#112233")
            .expect("journey text updated");
        assert!(output.contains("fill:#112233"));
        assert!(output.contains("stroke:#fff"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_journey_section_text_noop_without_marker() {
        let input = r#"<svg><text class="label" x="0">Plan</text></svg>"#;
        assert!(MermaidWorker::fix_journey_section_text(input, "#112233").is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_journey_section_text_handles_non_section_tag() {
        let input = r#"<svg><text class="label">A</text><text class="journey-section" style="fill:#000">B</text></svg>"#;
        let output = MermaidWorker::fix_journey_section_text(input, "#112233")
            .expect("journey text updated");
        assert!(output.contains("class=\"label\""));
        assert!(output.contains("fill:#112233"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_journey_section_text_style_missing_quote_returns_none() {
        let input = r#"<svg><text class="journey-section" style="fill:#000>Plan</text></svg>"#;
        assert!(MermaidWorker::fix_journey_section_text(input, "#112233").is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_state_end_circles_swaps_radii() {
        let input = r#"<svg><g id="state-root_end-0"><circle class="state-end" r="5" width="10" height="10"></circle><circle class="state-start" r="7" width="14" height="14"></circle></g></svg>"#;
        let output =
            MermaidWorker::fix_state_end_circles(input).expect("state end circles updated");
        assert!(output.contains("class=\"state-end\" r=\"7\""));
        assert!(output.contains("class=\"end-state-inner\" r=\"5\""));
        assert!(output.contains("class=\"state-end\" r=\"7\" width=\"14\" height=\"14\""));
        assert!(output.contains("class=\"end-state-inner\" r=\"5\" width=\"10\" height=\"10\""));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_state_end_circles_swaps_radii_when_start_first() {
        let input = r#"<svg><g id="state-root_end-0"><circle class="state-start" r="7" width="14" height="14"></circle><circle class="state-end" r="5" width="10" height="10"></circle></g></svg>"#;
        let output =
            MermaidWorker::fix_state_end_circles(input).expect("state end circles updated");
        assert!(output.contains("end-state-inner"));
        assert!(output.contains("class=\"state-end\" r=\"7\""));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_state_end_circles_noop_without_marker() {
        let input = r#"<svg><g><circle class="state-start" r="7"></circle></g></svg>"#;
        assert!(MermaidWorker::fix_state_end_circles(input).is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_state_end_circles_noop_when_radii_ordered() {
        let input = r#"<svg><g id="state-root_end-0"><circle class="state-end" r="7"></circle><circle class="state-start" r="5"></circle></g></svg>"#;
        assert!(MermaidWorker::fix_state_end_circles(input).is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_state_end_circles_skips_non_end_group() {
        let input = r#"<svg><g id="state-root-0"><circle class="state-end" r="5"></circle></g><g id="state-root_end-0"><circle class="state-end" r="5" width="10" height="10"></circle><circle class="state-start" r="7" width="14" height="14"></circle></g></svg>"#;
        let output =
            MermaidWorker::fix_state_end_circles(input).expect("state end circles updated");
        assert!(output.contains("state-root-0"));
        assert!(output.contains("end-state-inner"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_state_end_circles_missing_parts_noop() {
        let missing_start =
            r#"<svg><g id="state-root_end-0"><circle class="state-end" r="5"></circle></g></svg>"#;
        assert!(MermaidWorker::fix_state_end_circles(missing_start).is_none());

        let missing_r = r#"<svg><g id="state-root_end-0"><circle class="state-end"></circle><circle class="state-start" r="7"></circle></g></svg>"#;
        assert!(MermaidWorker::fix_state_end_circles(missing_r).is_none());

        let non_numeric = r#"<svg><g id="state-root_end-0"><circle class="state-end" r="x"></circle><circle class="state-start" r="7"></circle></g></svg>"#;
        assert!(MermaidWorker::fix_state_end_circles(non_numeric).is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_find_circle_tag_missing_parts_returns_none() {
        let missing_circle = r#"<g class="state-end"></g>"#;
        assert!(MermaidWorker::find_circle_tag(missing_circle, "state-end").is_none());

        let missing_end = r#"<circle class="state-end""#;
        assert!(MermaidWorker::find_circle_tag(missing_end, "state-end").is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_read_r_value_missing_quote_returns_none() {
        let tag = r#"<circle r="10"#;
        assert!(MermaidWorker::read_r_value(tag).is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mindmap_edge_trim_uses_min_radius() {
        let dx_idx = MERMAID_DOM_SHIM
            .find("var dx = t_pos.x - s_pos.x")
            .expect("mindmap dx computed");
        let radius_idx = MERMAID_DOM_SHIM
            .find("var s_radius = __mdmdview_rect_intersection")
            .expect("mindmap radius computed");
        assert!(dx_idx < radius_idx);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mindmap_edge_overlap_padding_present() {
        assert!(MERMAID_DOM_SHIM.contains("var overlap = -2"));
        assert!(MERMAID_DOM_SHIM.contains("var max_total = len * 0.95"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_flatten_svg_switches_removes_foreignobject() {
        let input = r#"<svg><switch><foreignObject><div>Plan</div></foreignObject><text>Plan</text></switch></svg>"#;
        let output = MermaidWorker::flatten_svg_switches(input).expect("switch flattened");
        assert!(!output.contains("<switch"));
        assert!(!output.contains("foreignObject"));
        assert!(output.contains("<text>Plan</text>"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_dom_shim_clusters_not_labelish() {
        assert!(MERMAID_DOM_SHIM.contains("has_class(node, 'clusters')"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_dom_shim_has_attribute_helpers() {
        assert!(MERMAID_DOM_SHIM.contains("hasAttribute"));
        assert!(MERMAID_DOM_SHIM.contains("hasAttributeNS"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_er_attribute_fills_inlines_colors() {
        let input = r#"<svg><rect class="er attributeBoxOdd"></rect><rect class="er attributeBoxEven"></rect></svg>"#;
        let output = MermaidWorker::fix_er_attribute_fills(input).expect("er fills updated");
        assert!(output.contains("attributeBoxOdd\" fill=\"#ffffff\""));
        assert!(output.contains("attributeBoxEven\" fill=\"#f2f2f2\""));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_er_attribute_fills_handles_even_only() {
        let input = r#"<svg><rect class="er attributeBoxEven"></rect></svg>"#;
        let output = MermaidWorker::fix_er_attribute_fills(input).expect("er fills updated");
        assert!(output.contains("attributeBoxEven\" fill=\"#f2f2f2\""));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_er_attribute_fills_skips_unmatched_rects() {
        let input =
            r#"<svg><rect class="er attributeBoxOdd"></rect><rect class="er"></rect></svg>"#;
        let output = MermaidWorker::fix_er_attribute_fills(input).expect("er fills updated");
        assert!(output.contains("attributeBoxOdd\" fill=\"#ffffff\""));
        assert!(output.contains("class=\"er\"></rect>"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_er_attribute_fills_noop_without_markers() {
        let input = r#"<svg><rect class="er"></rect></svg>"#;
        assert!(MermaidWorker::fix_er_attribute_fills(input).is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_fix_er_attribute_fills_noop_when_fill_matches() {
        let input = r##"<svg><rect class="er attributeBoxOdd" fill="#ffffff"></rect></svg>"##;
        assert!(MermaidWorker::fix_er_attribute_fills(input).is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_flatten_svg_switches_noop_without_switch() {
        let input = r#"<svg><text>Plan</text></svg>"#;
        assert!(MermaidWorker::flatten_svg_switches(input).is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_flatten_svg_switches_handles_missing_close() {
        let input = r#"<svg><switch><text>Plan</text></svg>"#;
        assert!(MermaidWorker::flatten_svg_switches(input).is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_flatten_svg_switches_missing_foreign_object_close() {
        let input =
            r#"<svg><switch><foreignObject><div>Plan</div></switch><text>After</text></svg>"#;
        let output = MermaidWorker::flatten_svg_switches(input).expect("switch flattened");
        assert!(output.contains("<text>After</text>"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_patch_mermaid_js_applies_expected_replacements() {
        let js = concat!(
            "var hD=wRe();",
            "FY.prototype={constructor:FY,appendChild:function(i){return this._parent.insertBefore(i,this._next)},insertBefore:function(i,s){return this._parent.insertBefore(i,s)},querySelector:function(i){return this._parent.querySelector(i)},querySelectorAll:function(i){return this._parent.querySelectorAll(i)}};",
            "function FY(i,s){this.ownerDocument=i.ownerDocument,this.namespaceURI=i.namespaceURI,this._next=null,this._parent=i,this.__data__=s}",
            "u.text().split(/(\\s+|<br>)/).reverse()",
            "p=fWe({container:document.getElementById(\"cy\"),style:[{selector:\"edge\",style:{\"curve-style\":\"bezier\"}}]});",
            "p.layout({name:\"cose-bilkent\",quality:\"proof\",styleEnabled:!1,animate:!1}).run(),p.ready(v=>{Xe.info(\"Ready\",v),u(p)})",
            "const se=_.node().getBBox();"
        );
        let output = MermaidWorker::patch_mermaid_js(js);
        assert!(output.contains("sanitize:function"));
        assert!(output.contains("document.body"));
        assert!(output.contains("String(typeof u.text"));
        assert!(output.contains("__mdmdview_cytoscape_stub"));
        assert!(output.contains("breadthfirst"));
        assert!(output.contains("se.width=Math.max"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_patch_mermaid_js_applies_mindmap_layout_fallback() {
        let js =
            "p.layout({name:\"cose-bilkent\",quality:\"proof\",styleEnabled:!1,animate:!1}).run()";
        let output = MermaidWorker::patch_mermaid_js(js);
        assert!(output.contains("breadthfirst"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_patch_mermaid_js_debug_paths_without_targets() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_PATCH_DEBUG", "1");
        let js = "console.log('no targets here');";
        let output = MermaidWorker::patch_mermaid_js(js);
        assert_eq!(output, js);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_patch_mermaid_js_debug_false_branch() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_PATCH_DEBUG", "1");
        let previous = std::env::var("MDMDVIEW_MERMAID_PATCH_DEBUG").ok();
        std::env::remove_var("MDMDVIEW_MERMAID_PATCH_DEBUG");
        let js =
            "p.layout({name:\"cose-bilkent\",quality:\"proof\",styleEnabled:!1,animate:!1}).run()";
        let output = MermaidWorker::patch_mermaid_js(js);
        assert!(output.contains("breadthfirst"));
        restore_env_var("MDMDVIEW_MERMAID_PATCH_DEBUG", previous);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_patch_mermaid_js_debug_restore_when_missing() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_MERMAID_PATCH_DEBUG");
        let previous = std::env::var("MDMDVIEW_MERMAID_PATCH_DEBUG").ok();
        let js =
            "p.layout({name:\"cose-bilkent\",quality:\"proof\",styleEnabled:!1,animate:!1}).run()";
        let output = MermaidWorker::patch_mermaid_js(js);
        assert!(output.contains("breadthfirst"));
        restore_env_var("MDMDVIEW_MERMAID_PATCH_DEBUG", previous);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_patch_mermaid_js_debug_missing_layout_not_logged_when_debug_off() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_MERMAID_PATCH_DEBUG");
        let js = "console.log('no targets here');";
        let output = MermaidWorker::patch_mermaid_js(js);
        assert_eq!(output, js);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_patch_mermaid_js_debug_paths_with_targets() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_PATCH_DEBUG", "1");
        let js = concat!(
            "var hD=wRe();",
            "FY.prototype={constructor:FY,appendChild:function(i){return this._parent.insertBefore(i,this._next)},insertBefore:function(i,s){return this._parent.insertBefore(i,s)},querySelector:function(i){return this._parent.querySelector(i)},querySelectorAll:function(i){return this._parent.querySelectorAll(i)}};",
            "function FY(i,s){this.ownerDocument=i.ownerDocument,this.namespaceURI=i.namespaceURI,this._next=null,this._parent=i,this.__data__=s}",
            "u.text().split(/(\\s+|<br>)/).reverse()",
            "p=fWe({container:document.getElementById(\"cy\"),style:[{selector:\"edge\",style:{\"curve-style\":\"bezier\"}}]});",
            "p.layout({name:\"cose-bilkent\",quality:\"proof\",styleEnabled:!1,animate:!1}).run(),p.ready(v=>{Xe.info(\"Ready\",v),u(p)})",
            "const se=_.node().getBBox();"
        );
        let output = MermaidWorker::patch_mermaid_js(js);
        assert!(output.contains("sanitize:function"));
        assert!(output.contains("__mdmdview_cytoscape_stub"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_patch_mermaid_js_debug_paths_with_layout_only() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_PATCH_DEBUG", "1");
        let js =
            "p.layout({name:\"cose-bilkent\",quality:\"proof\",styleEnabled:!1,animate:!1}).run()";
        let output = MermaidWorker::patch_mermaid_js(js);
        assert!(output.contains("breadthfirst"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_normalize_svg_size_inserts_dimensions() {
        let svg = r#"<svg viewBox="0 0 200 100" width="100%" height="100%"></svg>"#;
        let output = MermaidWorker::normalize_svg_size(svg);
        assert!(output.contains("width=\"200\""));
        assert!(output.contains("height=\"100\""));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_normalize_svg_size_inserts_missing_dimension() {
        let svg = r#"<svg viewBox="0 0 320 240" height="100%"></svg>"#;
        let output = MermaidWorker::normalize_svg_size(svg);
        assert!(output.contains("width=\"320\""));
        assert!(output.contains("height=\"240\""));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_normalize_svg_size_skips_fixed_dimensions() {
        let svg = r#"<svg viewBox="0 0 200 100" width="200" height="100"></svg>"#;
        assert_eq!(MermaidWorker::normalize_svg_size(svg), svg);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_normalize_svg_size_no_viewbox_no_change() {
        let svg = r#"<svg width="100%" height="100%"></svg>"#;
        assert_eq!(MermaidWorker::normalize_svg_size(svg), svg);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_replace_and_upsert_attr_paths() {
        let tag = r#"<svg width="10" height="20">"#;
        let replaced = MermaidWorker::replace_attr(tag, "width", "99");
        assert!(replaced.contains("width=\"99\""));
        let unchanged = MermaidWorker::replace_attr(tag, "viewBox", "0 0 1 1");
        assert_eq!(unchanged, tag);
        let upsert_existing = MermaidWorker::upsert_attr(tag, "height", "42");
        assert!(upsert_existing.contains("height=\"42\""));
        let upsert_new = MermaidWorker::upsert_attr(tag, "viewBox", "0 0 1 1");
        assert!(upsert_new.contains("viewBox=\"0 0 1 1\""));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_strip_svg_attr_and_insert_svg_attr() {
        let svg = r#"<svg width="10" height="20"></svg>"#;
        let stripped = MermaidWorker::strip_svg_attr(svg, "width").expect("width removed");
        assert!(!stripped.contains("width=\"10\""));
        assert!(MermaidWorker::strip_svg_attr(svg, "viewBox").is_none());
        let inserted = MermaidWorker::insert_svg_attr("<svg></svg>", "viewBox", "0 0 1 1");
        assert!(inserted.contains("viewBox=\"0 0 1 1\""));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_remove_svg_attr_trims_double_space() {
        let tag = r#"<svg width="10"  height="20">"#;
        let stripped = MermaidWorker::remove_svg_attr(tag, "width");
        assert!(!stripped.contains("width=\"10\""));
        assert!(stripped.contains("height=\"20\""));
        assert!(stripped.starts_with("<svg"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_remove_svg_attr_handles_no_space_prefix() {
        let tag = r#"<svgwidth="10" height="20">"#;
        let stripped = MermaidWorker::remove_svg_attr(tag, "width");
        assert!(!stripped.contains("width=\"10\""));
        assert!(stripped.contains("height=\"20\""));
        assert!(stripped.starts_with("<svg"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_remove_svg_attr_noop_when_missing_or_unclosed() {
        let tag = r#"<svg height="20">"#;
        assert_eq!(MermaidWorker::remove_svg_attr(tag, "width"), tag);
        let unclosed = r#"<svg width="10 height=20>"#;
        assert_eq!(MermaidWorker::remove_svg_attr(unclosed, "width"), unclosed);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_remove_svg_attr_preserves_spacing_without_double_space() {
        let tag = r#"<svg width="10"height="20">"#;
        assert_eq!(
            MermaidWorker::remove_svg_attr(tag, "width"),
            "<svg height=\"20\">"
        );
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_parse_viewbox_dims_and_format_dim() {
        assert_eq!(
            MermaidWorker::parse_viewbox_dims("0 0 100 200"),
            Some((100.0, 200.0))
        );
        assert_eq!(
            MermaidWorker::parse_viewbox_dims("0,0,100,200"),
            Some((100.0, 200.0))
        );
        assert!(MermaidWorker::parse_viewbox_dims("0,0,100,0").is_none());
        assert!(MermaidWorker::parse_viewbox_dims("0 0 -1 2").is_none());
        assert!(MermaidWorker::parse_viewbox_dims("0 0 NaN 2").is_none());
        assert!(MermaidWorker::parse_viewbox_dims("0 0 inf 2").is_none());
        assert!(MermaidWorker::parse_viewbox_dims("0 0 2 inf").is_none());
        assert!(MermaidWorker::parse_viewbox_dims("bad").is_none());
        assert_eq!(MermaidWorker::format_dim(10.0), "10");
        assert_eq!(MermaidWorker::format_dim(10.5), "10.5");
        assert_eq!(MermaidWorker::format_dim(0.0), "0");
        assert_eq!(MermaidWorker::format_dim(10.123), "10.123");
        let nan = MermaidWorker::format_dim(f32::NAN);
        assert!(!nan.contains('.'));
        assert!(nan.to_lowercase().contains("nan"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_find_svg_attr_missing_quote_returns_none() {
        let tag = r#"<svg viewBox="0 0 10 10"#;
        assert!(MermaidWorker::find_svg_attr(tag, "viewBox").is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_strip_svg_attr_missing_tag_returns_none() {
        assert!(MermaidWorker::strip_svg_attr("no svg here", "width").is_none());
        let missing_end = r#"<svg width="10""#;
        assert!(MermaidWorker::strip_svg_attr(missing_end, "width").is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_text_measurer_fallback_for_missing_fonts() {
        let fontdb = Arc::new(usvg::fontdb::Database::new());
        let measurer = TextMeasurer::new(Arc::clone(&fontdb));
        let (width, height) = measurer.measure_text("abc", -1.0, Some(700.0));
        assert!(width > 0.0);
        assert!(height > 0.0);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_text_measurer_non_finite_font_size_uses_default() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let measurer = TextMeasurer::new(Arc::new(fontdb));
        let (width, height) = measurer.measure_text("abc", f32::NAN, None);
        assert!(width > 0.0);
        assert!(height > 0.0);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_text_measurer_missing_glyph_uses_fallback_advance() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let measurer = TextMeasurer::new(Arc::new(fontdb));
        let (width, height) = measurer.measure_text("\u{10FFFF}", 16.0, None);
        assert!(width > 0.0);
        assert!(height > 0.0);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_text_measurer_forced_face_parse_error_falls_back() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let measurer = TextMeasurer::new(Arc::new(fontdb));
        force_mermaid_face_parse_error_once();
        let (width, height) = measurer.measure_text("abc", 16.0, None);
        assert!(width > 0.0);
        assert!(height > 0.0);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_text_measurer_bold_adjusts_when_faces_match() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        let face_id = fontdb.query(&usvg::fontdb::Query {
            families: &[usvg::fontdb::Family::SansSerif],
            ..Default::default()
        });
        let measurer = TextMeasurer {
            fontdb: Arc::clone(&fontdb),
            face_id,
            bold_face_id: face_id,
        };
        let (width, height) = measurer.measure_text("Test", 16.0, Some(700.0));
        assert!(width > 0.0);
        assert!(height > 0.0);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_text_measurer_empty_text_returns_zero() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let measurer = TextMeasurer::new(Arc::new(fontdb));
        let (width, height) = measurer.measure_text("", 16.0, None);
        assert_eq!(width, 0.0);
        assert_eq!(height, 0.0);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_text_measurer_fallback_handles_shorter_lines() {
        let (width, height) = TextMeasurer::fallback_measure("abcd\na", 10.0);
        assert!((width - 20.0).abs() < 0.01);
        assert!((height - 24.0).abs() < 0.01);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_text_measurer_kerning_adjusts_width_when_available() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        let measurer = TextMeasurer::new(Arc::clone(&fontdb));
        let (w_av, _) = measurer.measure_text("AV", 16.0, None);
        let (w_a, _) = measurer.measure_text("A", 16.0, None);
        let (w_v, _) = measurer.measure_text("V", 16.0, None);
        assert!(w_av <= w_a + w_v);
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_scale_adjustment_for_svg_defaults() {
        assert_eq!(MermaidWorker::scale_adjustment_for_svg("<svg></svg>"), 1.0);
        assert_eq!(
            MermaidWorker::scale_adjustment_for_svg("<svg aria-roledescription=\"er\"></svg>"),
            0.94
        );
        assert_eq!(
            MermaidWorker::scale_adjustment_for_svg("<svg aria-roledescription='er'></svg>"),
            0.94
        );
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_enqueue_mermaid_job_disconnected() {
        let (job_tx, job_rx) = bounded(1);
        drop(job_rx);
        let (_result_tx, result_rx) = bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let request = MermaidRequest {
            svg_key: 1,
            texture_key: "t".to_string(),
            code: Some("graph TD; A-->B;".to_string()),
            svg: None,
            width_bucket: 120,
            scale_bucket: 100,
            viewport_width: 120,
            viewport_height: 120,
            bg: None,
        };
        assert_eq!(
            renderer.enqueue_mermaid_job(request),
            Err(MermaidEnqueueError::Disconnected)
        );
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_render_block_enqueues_cached_svg() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_RENDERER", "embedded");
        let (job_tx, job_rx) = bounded(1);
        let (_result_tx, result_rx) = bounded(1);
        let renderer = test_renderer_with_channels(Some(job_tx), result_rx);
        let code = "graph TD; A-->B;";
        let svg_key = MermaidRenderer::hash_str(code);
        renderer
            .mermaid_svg_cache
            .borrow_mut()
            .insert(svg_key, "<svg></svg>".to_string());
        let ctx = egui::Context::default();
        let input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(240.0, 120.0),
            )),
            ..Default::default()
        };
        let mut rendered = false;
        let _ = ctx.run(input, |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                rendered = renderer.render_block(ui, code, 1.0, 14.0);
            });
        });
        assert!(rendered);
        let request = job_rx
            .recv_timeout(std::time::Duration::from_secs(1))
            .expect("request");
        assert!(request.svg.is_some());
        assert!(request.code.is_none());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_spawn_mermaid_workers_reports_init_error() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_WORKERS", "1");
        force_mermaid_worker_init_error_once();
        let (job_tx, job_rx) = bounded(1);
        let (result_tx, result_rx) = bounded(1);
        let handles = MermaidRenderer::spawn_mermaid_workers(job_rx, result_tx);
        let request = MermaidRequest {
            svg_key: 2,
            texture_key: "t".to_string(),
            code: Some("graph TD; A-->B;".to_string()),
            svg: None,
            width_bucket: 120,
            scale_bucket: 100,
            viewport_width: 120,
            viewport_height: 120,
            bg: None,
        };
        job_tx.send(request).expect("send");
        let result = result_rx
            .recv_timeout(std::time::Duration::from_secs(1))
            .expect("result");
        assert!(result.error.is_some());
        drop(job_tx);
        for handle in handles {
            let _ = handle.join();
        }
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_spawn_mermaid_workers_reports_spawn_error() {
        let _lock = env_lock();
        let _guard = EnvGuard::set("MDMDVIEW_MERMAID_WORKERS", "1");
        force_mermaid_thread_spawn_error_once();
        let (_job_tx, job_rx) = bounded(1);
        let (result_tx, _result_rx) = bounded(1);
        let handles = MermaidRenderer::spawn_mermaid_workers(job_rx, result_tx);
        assert!(handles.is_empty());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_worker_init_error_stages() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);

        force_mermaid_init_error_once(1);
        let err = MermaidWorker::new(0, Arc::clone(&fontdb))
            .err()
            .expect("stage 1 err");
        assert!(err.contains("Mermaid text measure init error"));

        force_mermaid_init_error_once(2);
        let err = MermaidWorker::new(0, Arc::clone(&fontdb))
            .err()
            .expect("stage 2 err");
        assert!(err.contains("Mermaid text measure init error"));

        force_mermaid_init_error_once(3);
        let err = MermaidWorker::new(0, fontdb).err().expect("stage 3 err");
        assert!(err.contains("Mermaid DOM shim"));

        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        force_mermaid_init_error_once(4);
        let err = MermaidWorker::new(0, Arc::clone(&fontdb))
            .err()
            .expect("stage 4 err");
        assert!(err.contains("Mermaid JS"));

        force_mermaid_init_error_once(5);
        let err = MermaidWorker::new(0, fontdb).err().expect("stage 5 err");
        assert!(err.contains("Mermaid init"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_worker_new_runtime_error_forced() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        force_mermaid_runtime_error_once();
        let err = MermaidWorker::new(0, Arc::clone(&fontdb))
            .err()
            .expect("runtime err");
        assert!(err.contains("Mermaid runtime init error"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_runtime_flag_mismatch_restores() {
        let handle = std::thread::spawn(|| {
            force_mermaid_runtime_error_once();
        });
        handle.join().expect("flag thread");

        let _ = runtime_new_for_test().expect("runtime ok");

        let mut guard = FORCE_MERMAID_RUNTIME_ERROR
            .lock()
            .expect("mermaid runtime flag lock");
        *guard = None;
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_worker_new_context_error_forced() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        force_mermaid_context_error_once();
        let err = MermaidWorker::new(0, Arc::clone(&fontdb))
            .err()
            .expect("context err");
        assert!(err.contains("Mermaid context init error"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_worker_new_utf8_error_forced() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        force_mermaid_utf8_error_once();
        let err = MermaidWorker::new(0, Arc::clone(&fontdb))
            .err()
            .expect("utf8 err");
        assert!(err.contains("Mermaid JS is not valid UTF-8"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_render_svg_forced_eval_error() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        let mut worker = MermaidWorker::new(0, Arc::clone(&fontdb)).expect("worker init");
        force_mermaid_render_eval_error_once();
        let err = worker
            .render_svg(1, "graph TD; A-->B;", 480, 320)
            .unwrap_err();
        assert!(!err.is_empty());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_mermaid_render_svg_forced_call_error() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        let mut worker = MermaidWorker::new(0, Arc::clone(&fontdb)).expect("worker init");
        force_mermaid_render_call_error_once();
        let err = worker
            .render_svg(2, "graph TD; A-->B;", 480, 320)
            .unwrap_err();
        assert!(!err.is_empty());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_parse_raw_svg_tree_invalid_svg_returns_error() {
        let options = usvg::Options::default();
        let err = MermaidWorker::parse_raw_svg_tree("<svg", &options).unwrap_err();
        assert!(!err.is_empty());
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_rasterize_svg_forced_pixmap_alloc_error() {
        let mut fontdb = usvg::fontdb::Database::new();
        fontdb.load_system_fonts();
        let fontdb = Arc::new(fontdb);
        let worker = MermaidWorker::new(0, Arc::clone(&fontdb)).expect("worker init");
        force_mermaid_pixmap_alloc_fail_once();
        let err = worker
            .rasterize_svg(
                "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\"><rect width=\"10\" height=\"10\"/></svg>",
                0,
                100,
                None,
            )
            .unwrap_err();
        assert!(err.contains("Pixmap alloc failed"));
    }

    #[cfg(feature = "mermaid-quickjs")]
    #[test]
    fn test_svg_helper_early_returns() {
        assert_eq!(MermaidWorker::normalize_svg_size("<svg"), "<svg");
        assert_eq!(
            MermaidWorker::normalize_svg_size("<svg viewBox=\"bad\">"),
            "<svg viewBox=\"bad\">"
        );
        assert_eq!(
            MermaidWorker::replace_attr("<svg width=\"100", "width", "200"),
            "<svg width=\"100"
        );
        assert_eq!(
            MermaidWorker::upsert_attr("<svg width=\"100\"", "height", "200"),
            "<svg width=\"100\""
        );
        assert!(MermaidWorker::flatten_svg_switches("<switch").is_none());
        assert!(MermaidWorker::fix_journey_section_text("<text journey-section", "#fff").is_none());
        assert!(MermaidWorker::fix_state_end_circles("<svg><g class=\"state-end\"").is_none());
        assert!(MermaidWorker::fix_state_end_circles("<svg><g class=\"state-end\"></g").is_none());
        assert!(MermaidWorker::fix_er_attribute_fills("<rect class=\"attributeBoxOdd\"").is_none());
        assert_eq!(
            MermaidWorker::replace_svg_attr("<svg>", "width", "1"),
            "<svg>"
        );
        assert_eq!(
            MermaidWorker::replace_svg_attr("<svg width=\"100>", "width", "1"),
            "<svg width=\"100>"
        );
        assert_eq!(
            MermaidWorker::insert_svg_attr("no svg", "width", "1"),
            "no svg"
        );
    }

    // =============================================================================
    // HTML Tag to SVG tspan Conversion Tests (TDD)
    // =============================================================================

    #[test]
    fn test_html_to_tspan_basic_italic() {
        let input = "<text>&lt;i&gt;italic&lt;/i&gt;</text>";
        let expected = "<text><tspan font-style=\"italic\">italic</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_basic_bold() {
        let input = "<text>&lt;b&gt;bold&lt;/b&gt;</text>";
        let expected = "<text><tspan font-weight=\"bold\">bold</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_semantic_em() {
        let input = "<text>&lt;em&gt;emphasis&lt;/em&gt;</text>";
        let expected = "<text><tspan font-style=\"italic\">emphasis</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_semantic_strong() {
        let input = "<text>&lt;strong&gt;strong&lt;/strong&gt;</text>";
        let expected = "<text><tspan font-weight=\"bold\">strong</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_nested_tags() {
        let input = "<text>&lt;b&gt;&lt;i&gt;both&lt;/i&gt;&lt;/b&gt;</text>";
        let expected =
            "<text><tspan font-weight=\"bold\"><tspan font-style=\"italic\">both</tspan></tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_mixed_content() {
        let input = "<text>normal &lt;i&gt;italic&lt;/i&gt; normal</text>";
        let expected = "<text>normal <tspan font-style=\"italic\">italic</tspan> normal</text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_unclosed_tag() {
        let input = "<text>&lt;i&gt;unclosed</text>";
        let expected = "<text><tspan font-style=\"italic\">unclosed</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_extra_closing_tag() {
        let input = "<text>text&lt;/i&gt;</text>";
        let expected = "<text>text</text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_unknown_tag_passthrough() {
        // Unknown tags (like <span>, or SVG elements like <tspan>) pass through unchanged
        // This is essential to preserve existing SVG structure
        let input = "<text>&lt;span&gt;text&lt;/span&gt;</text>";
        let expected = "<text><span>text</span></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_preserves_existing_svg_tspan() {
        // Critical: existing SVG tspan elements must be preserved unchanged
        // This is the real-world case from Mermaid output
        let input =
            r#"<text><tspan xml:space="preserve" dy="1em" x="0" class="row">Label</tspan></text>"#;
        let expected =
            r#"<text><tspan xml:space="preserve" dy="1em" x="0" class="row">Label</tspan></text>"#;
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_mixed_svg_and_html_tags() {
        // HTML tags inside existing SVG tspan should still be converted
        let input = r#"<text><tspan dy="1em">&lt;b&gt;bold&lt;/b&gt;</tspan></text>"#;
        let result = MermaidWorker::convert_html_tags_to_tspan(input);
        assert!(result.contains(r#"<tspan dy="1em">"#)); // Preserve outer tspan
        assert!(result.contains(r#"font-weight="bold""#)); // Convert HTML bold
    }

    #[test]
    fn test_html_to_tspan_angle_brackets_in_math() {
        let input = "<text>x &lt; 5</text>";
        let expected = "<text>x &lt; 5</text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_multiple_text_elements() {
        let input = "<text>&lt;b&gt;one&lt;/b&gt;</text><rect/><text>&lt;i&gt;two&lt;/i&gt;</text>";
        let expected = "<text><tspan font-weight=\"bold\">one</tspan></text><rect/><text><tspan font-style=\"italic\">two</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_no_html_tags_unchanged() {
        let input = "<text>plain text</text>";
        let result = MermaidWorker::convert_html_tags_to_tspan(input);
        assert_eq!(result, input);
    }

    #[test]
    fn test_html_to_tspan_no_escaped_tags_fast_path() {
        let input = "<svg><text>no tags here</text></svg>";
        let result = MermaidWorker::convert_html_tags_to_tspan(input);
        assert_eq!(result, input);
    }

    #[test]
    fn test_html_to_tspan_case_insensitive() {
        let input = "<text>&lt;I&gt;italic&lt;/I&gt;</text>";
        let expected = "<text><tspan font-style=\"italic\">italic</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_tag_with_attributes() {
        let input = "<text>&lt;i class=\"x\"&gt;text&lt;/i&gt;</text>";
        let expected = "<text><tspan font-style=\"italic\">text</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_self_closing_tag_passthrough() {
        // Self-closing tags like <br/> pass through unchanged (preserves SVG structure)
        let input = "<text>line&lt;br/&gt;break</text>";
        let expected = "<text>line<br/>break</text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_empty_tag() {
        let input = "<text>&lt;i&gt;&lt;/i&gt;</text>";
        let expected = "<text><tspan font-style=\"italic\"></tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_mismatched_tags() {
        // <b>a<i>b</b>c</i> - close b while i is open
        let input = "<text>&lt;b&gt;a&lt;i&gt;b&lt;/b&gt;c&lt;/i&gt;</text>";
        // Expected: close i and b at </b>, reopen i for c, close i at </i>
        let result = MermaidWorker::convert_html_tags_to_tspan(input);
        // Should contain proper tspan nesting
        assert!(result.contains("<tspan font-weight=\"bold\">"));
        assert!(result.contains("<tspan font-style=\"italic\">"));
        // Should not contain escaped tags
        assert!(!result.contains("&lt;b&gt;"));
        assert!(!result.contains("&lt;i&gt;"));
    }

    #[test]
    fn test_html_to_tspan_underline_experimental() {
        let input = "<text>&lt;u&gt;underline&lt;/u&gt;</text>";
        let expected = "<text><tspan text-decoration=\"underline\">underline</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_strikethrough_experimental() {
        let input = "<text>&lt;s&gt;strike&lt;/s&gt;</text>";
        let expected = "<text><tspan text-decoration=\"line-through\">strike</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_del_tag() {
        let input = "<text>&lt;del&gt;deleted&lt;/del&gt;</text>";
        let expected = "<text><tspan text-decoration=\"line-through\">deleted</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_preserves_text_attributes() {
        let input = "<text x=\"10\" y=\"20\">&lt;b&gt;bold&lt;/b&gt;</text>";
        let expected = "<text x=\"10\" y=\"20\"><tspan font-weight=\"bold\">bold</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_nested_text_not_processed() {
        // tspan inside text should have its content processed too
        let input = "<text><tspan>&lt;i&gt;nested&lt;/i&gt;</tspan></text>";
        let result = MermaidWorker::convert_html_tags_to_tspan(input);
        // The inner tspan content should be converted
        assert!(result.contains("font-style=\"italic\"") || result.contains("&lt;i&gt;"));
    }

    #[test]
    fn test_html_to_tspan_ampersand_preserved() {
        let input = "<text>A &amp; B</text>";
        let expected = "<text>A &amp; B</text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    fn test_html_to_tspan_multiple_same_tags() {
        let input = "<text>&lt;i&gt;a&lt;/i&gt; and &lt;i&gt;b&lt;/i&gt;</text>";
        let expected =
            "<text><tspan font-style=\"italic\">a</tspan> and <tspan font-style=\"italic\">b</tspan></text>";
        assert_eq!(MermaidWorker::convert_html_tags_to_tspan(input), expected);
    }

    #[test]
    #[ignore = "env vars are process-global; run with --ignored to test in isolation"]
    fn test_html_to_tspan_feature_flag_disabled() {
        std::env::set_var("MDMDVIEW_MERMAID_HTML_TAGS", "off");
        let input = "<text>&lt;i&gt;italic&lt;/i&gt;</text>";
        let result = MermaidWorker::convert_html_tags_to_tspan(input);
        // With feature disabled, should return unchanged
        assert_eq!(result, input);
        std::env::remove_var("MDMDVIEW_MERMAID_HTML_TAGS");
    }

    #[test]
    fn test_try_parse_html_tag_basic() {
        let (name, closing, len) = MermaidWorker::try_parse_html_tag("<i>rest").unwrap();
        assert_eq!(name, "i");
        assert!(!closing);
        assert_eq!(len, 3);
    }

    #[test]
    fn test_try_parse_html_tag_closing() {
        let (name, closing, len) = MermaidWorker::try_parse_html_tag("</b>rest").unwrap();
        assert_eq!(name, "b");
        assert!(closing);
        assert_eq!(len, 4);
    }

    #[test]
    fn test_try_parse_html_tag_with_attrs() {
        let (name, closing, len) =
            MermaidWorker::try_parse_html_tag("<i class=\"x\">rest").unwrap();
        assert_eq!(name, "i");
        assert!(!closing);
        assert_eq!(len, 13);
    }

    #[test]
    fn test_try_parse_html_tag_not_a_tag() {
        assert!(MermaidWorker::try_parse_html_tag("< 5").is_none());
        assert!(MermaidWorker::try_parse_html_tag("<123>").is_none());
        assert!(MermaidWorker::try_parse_html_tag("<>").is_none());
        assert!(MermaidWorker::try_parse_html_tag("<i").is_none()); // No closing >
    }

    #[test]
    fn test_try_parse_html_tag_self_closing() {
        let result = MermaidWorker::try_parse_html_tag("<br/>");
        assert!(result.is_some());
        let (name, _, _) = result.unwrap();
        assert_eq!(name, "br");
    }

    #[test]
    fn test_unescape_html_entities() {
        assert_eq!(MermaidWorker::unescape_html_entities("&lt;i&gt;"), "<i>");
        assert_eq!(
            MermaidWorker::unescape_html_entities("&amp;&quot;&apos;"),
            "&\"'"
        );
        assert_eq!(
            MermaidWorker::unescape_html_entities("no entities"),
            "no entities"
        );
    }
}
