use crate::image_decode;
use crate::mermaid_renderer::MermaidRenderer;
use crate::table_support::{
    column_spec::spans_to_text, compute_column_stats, derive_column_specs, ColumnPolicy,
    ColumnSpec, ColumnStat, TableColumnContext, TableMetrics, WidthChange,
};
use crate::ThemeColors;
use crate::{emoji_assets, emoji_catalog};
use anyhow::Result;
use crossbeam_channel::{bounded, Receiver, Sender};
use egui::{
    text::{Galley, LayoutJob, TextWrapping},
    Align, Color32, Context, FontSelection, Painter, RichText, Stroke, Vec2, Visuals,
};
use egui_extras::{Column, TableBuilder};
use pulldown_cmark::{Alignment, Event, LinkType, Options, Parser, Tag};
use std::cell::{Cell, RefCell};
use std::collections::{hash_map::DefaultHasher, HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::io::Read;
use std::ops::Range;
use std::path::{Path, PathBuf};
#[cfg(test)]
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime};
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::LinesWithEndings;
use unicode_casefold::UnicodeCaseFold;
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

#[derive(Clone, Copy, Default)]
struct InlineStyle {
    strong: bool,
    italics: bool,
    strike: bool,
    color: Option<Color32>,
}

/// A single syntax-highlighted token within a code block line.
/// Pre-computed during parsing to avoid re-highlighting every frame.
#[derive(Clone, Debug)]
pub struct HighlightedToken {
    pub text: String,
    pub color: Color32,
    pub bold: bool,
    pub italic: bool,
}

/// A line of syntax-highlighted tokens.
pub type HighlightedLine = Vec<HighlightedToken>;

#[cfg(test)]
thread_local! {
    static FORCED_RENDER_ACTIONS: RefCell<HashSet<&'static str>> = RefCell::new(HashSet::new());
    static FORCED_TABLE_POLICIES: RefCell<Option<Vec<ColumnPolicy>>> = const { RefCell::new(None) };
    static FORCED_PARSE_ERROR: RefCell<bool> = const { RefCell::new(false) };
}

#[cfg(test)]
static FORCE_THREAD_SPAWN_ERROR: AtomicBool = AtomicBool::new(false);

#[cfg(test)]
static FORCE_EMOJI_DECODE_ERROR: AtomicBool = AtomicBool::new(false);

#[cfg(test)]
fn render_action_triggered(triggered: bool, action: &'static str) -> bool {
    triggered || FORCED_RENDER_ACTIONS.with(|actions| actions.borrow().contains(action))
}

#[cfg(not(test))]
fn render_action_triggered(triggered: bool, _action: &'static str) -> bool {
    triggered
}

#[cfg(test)]
fn take_forced_table_policies() -> Option<Vec<ColumnPolicy>> {
    FORCED_TABLE_POLICIES.with(|policies| policies.borrow_mut().take())
}

#[cfg(test)]
pub(crate) fn force_parse_error_once() {
    FORCED_PARSE_ERROR.with(|flag| flag.replace(true));
}

#[cfg(test)]
fn force_emoji_decode_error_once() {
    FORCE_EMOJI_DECODE_ERROR.store(true, Ordering::Relaxed);
}

#[cfg(test)]
fn take_forced_parse_error() -> bool {
    FORCED_PARSE_ERROR.with(|flag| flag.replace(false))
}

/// Font size configuration
#[derive(Debug, Clone)]
pub struct FontSizes {
    pub body: f32,
    pub h1: f32,
    pub h2: f32,
    pub h3: f32,
    pub h4: f32,
    pub h5: f32,
    pub h6: f32,
    pub code: f32,
}

impl Default for FontSizes {
    fn default() -> Self {
        Self {
            body: 14.0,
            h1: 28.0,
            h2: 24.0,
            h3: 20.0,
            h4: 18.0,
            h5: 16.0,
            h6: 14.0,
            code: 12.0,
        }
    }
}

impl FontSizes {
    /// Apply a transformation to every font size field, clamping each result
    /// to its field-specific bounds. The closure receives (current_value,
    /// default_value, min, max) and returns the new value.
    fn transform_all(&mut self, f: impl Fn(f32, f32, f32, f32) -> f32) {
        let d = FontSizes::default();
        self.body = f(self.body, d.body, 8.0, 32.0);
        self.h1 = f(self.h1, d.h1, 16.0, 48.0);
        self.h2 = f(self.h2, d.h2, 14.0, 42.0);
        self.h3 = f(self.h3, d.h3, 12.0, 36.0);
        self.h4 = f(self.h4, d.h4, 11.0, 32.0);
        self.h5 = f(self.h5, d.h5, 10.0, 28.0);
        self.h6 = f(self.h6, d.h6, 9.0, 24.0);
        self.code = f(self.code, d.code, 8.0, 20.0);
    }
}

/// Represents an inline text span with formatting
#[derive(Debug, Clone)]
pub enum InlineSpan {
    Text(String),
    Code(String),
    Strong(String),
    Emphasis(String),
    Strikethrough(String),
    Link {
        text: String,
        url: String,
    },
    Image {
        src: String,
        alt: String,
        title: Option<String>,
    },
}

#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Clone)]
enum CellFragment<'a> {
    Text(&'a [InlineSpan]),
    Emoji(String),
    Image(&'a InlineSpan),
}

#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Clone)]
struct LayoutJobBuild {
    job: LayoutJob,
    #[allow(dead_code)]
    plain_text: String,
    #[cfg_attr(not(test), allow(dead_code))]
    link_ranges: Vec<LinkRange>,
}

#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Clone)]
struct LinkRange {
    char_range: Range<usize>,
    url: String,
}

const TABLE_LAYOUT_CACHE_CAPACITY: usize = 512;
const COLUMN_STATS_SAMPLE_ROWS: usize = 128;
const PIPE_SENTINEL: char = '\u{1F}';
const IMAGE_TEXTURE_CACHE_CAPACITY: usize = 256;
const IMAGE_MAX_PENDING: usize = 64;
const IMAGE_FAILURE_BACKOFF: Duration = Duration::from_secs(5);
const EMOJI_TEXTURE_CACHE_CAPACITY: usize = 512;
const IMAGE_FAILURE_CACHE_CAPACITY: usize = 256;
/// Maximum total bytes for the image texture cache (100 MB).
/// When exceeded, oldest entries are evicted until under the limit.
const MAX_IMAGE_TEXTURE_CACHE_BYTES: usize = 100 * 1024 * 1024;

/// Generic LRU (Least Recently Used) cache with configurable capacity.
///
/// This cache evicts the least recently accessed entries when capacity is exceeded.
/// Used for `emoji_textures` and `image_failures` to prevent unbounded memory growth.
///
/// # Performance Note
///
/// The `touch()`, `insert()` (for existing keys), and `remove()` operations use
/// `VecDeque::retain()` which is O(n) where n is the cache capacity. This is acceptable
/// because:
/// - Cache capacities are small and bounded (256-512 entries max)
/// - These operations are infrequent compared to `get()` which is O(1)
/// - For these cache sizes, the constant factors of a more complex O(1) data structure
///   (e.g., intrusive linked list with HashMap) would likely outweigh the O(n) cost
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
    /// Creates a new LRU cache with the specified capacity.
    fn new(capacity: usize) -> Self {
        Self {
            entries: HashMap::new(),
            order: VecDeque::new(),
            capacity,
        }
    }

    /// Gets a value by key, returning a clone if found.
    /// Does not update access order (use `touch` for that).
    fn get(&self, key: &K) -> Option<V> {
        self.entries.get(key).cloned()
    }

    /// Checks if the cache contains the given key.
    #[allow(dead_code)] // Used in tests
    fn contains_key(&self, key: &K) -> bool {
        self.entries.contains_key(key)
    }

    /// Inserts a key-value pair, evicting oldest entries if at capacity.
    fn insert(&mut self, key: K, value: V) {
        // Zero capacity means no storage
        if self.capacity == 0 {
            return;
        }

        // If key already exists, update value and move to back of order
        if self.entries.contains_key(&key) {
            self.entries.insert(key.clone(), value);
            self.order.retain(|k| k != &key);
            self.order.push_back(key);
            return;
        }

        // Evict oldest entries if at capacity
        while self.entries.len() >= self.capacity {
            if let Some(oldest) = self.order.pop_front() {
                self.entries.remove(&oldest);
            } else {
                break;
            }
        }

        // Insert new entry
        self.entries.insert(key.clone(), value);
        self.order.push_back(key);
    }

    /// Removes an entry by key.
    fn remove(&mut self, key: &K) {
        if self.entries.remove(key).is_some() {
            self.order.retain(|k| k != key);
        }
    }

    /// Clears all entries from the cache.
    #[allow(dead_code)] // Used in tests and Stage 6
    fn clear(&mut self) {
        self.entries.clear();
        self.order.clear();
    }

    /// Returns the number of entries in the cache.
    #[allow(dead_code)] // Used in tests and Stage 8 logging
    fn len(&self) -> usize {
        self.entries.len()
    }

    /// Marks a key as recently used, moving it to the back of the eviction order.
    #[allow(dead_code)] // Used in tests
    fn touch(&mut self, key: &K) {
        if self.entries.contains_key(key) {
            self.order.retain(|k| k != key);
            self.order.push_back(key.clone());
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct CellLayoutKey {
    row: Option<usize>,
    col: usize,
    width: u32,
    align: u8,
    strong: bool,
    text_color: [u8; 4],
    highlight_hash: u64,
    content_hash: u64,
}

struct CellLayoutCache {
    entries: HashMap<CellLayoutKey, LayoutJobBuild>,
    order: VecDeque<CellLayoutKey>,
    hits: u64,
    misses: u64,
    capacity: usize,
}

#[derive(Clone)]
struct ColumnStatsCacheEntry {
    content_hash: u64,
    stats: Vec<ColumnStat>,
}

impl CellLayoutCache {
    fn new(capacity: usize) -> Self {
        Self {
            entries: HashMap::new(),
            order: VecDeque::new(),
            hits: 0,
            misses: 0,
            capacity,
        }
    }

    fn get(&mut self, key: &CellLayoutKey) -> Option<LayoutJobBuild> {
        if let Some(build) = self.entries.get(key) {
            self.hits += 1;
            Some(build.clone())
        } else {
            self.misses += 1;
            None
        }
    }

    fn insert(&mut self, key: CellLayoutKey, build: LayoutJobBuild) {
        if self.entries.len() >= self.capacity && !self.entries.contains_key(&key) {
            while self.entries.len() >= self.capacity {
                if let Some(old) = self.order.pop_front() {
                    self.entries.remove(&old);
                } else {
                    break;
                }
            }
        }
        self.order.retain(|existing| existing != &key);
        self.order.push_back(key.clone());
        self.entries.insert(key, build);
    }

    fn clear(&mut self) {
        self.entries.clear();
        self.order.clear();
        self.hits = 0;
        self.misses = 0;
    }

    fn stats(&self) -> (u64, u64) {
        (self.hits, self.misses)
    }
}

#[derive(Debug, Clone)]
pub struct ListItem {
    pub blocks: Vec<MarkdownElement>,
}

/// Represents a rendered markdown element
#[derive(Debug, Clone)]
pub enum MarkdownElement {
    Paragraph(Vec<InlineSpan>),
    Header {
        level: u8,
        spans: Vec<InlineSpan>,
        id: String,
    },
    CodeBlock {
        language: Option<String>,
        text: String,
        /// Pre-computed syntax highlighting. None if language unknown or highlighting failed.
        highlighted: Option<Vec<HighlightedLine>>,
    },
    List {
        ordered: bool,
        items: Vec<ListItem>,
    }, // List items can contain block elements
    Quote {
        depth: u8,
        blocks: Vec<MarkdownElement>,
    },
    HorizontalRule,
    Table {
        headers: Vec<Vec<InlineSpan>>,   // header cells as inline spans
        rows: Vec<Vec<Vec<InlineSpan>>>, // rows -> cells -> spans
        alignments: Vec<Alignment>,
    },
}

/// Type alias for table parsing result
type TableParseResult = (Vec<Vec<InlineSpan>>, Vec<Vec<Vec<InlineSpan>>>, usize);

/// Type alias for quote blocks (nested markdown elements)
type QuoteBlocks = Vec<MarkdownElement>;

#[derive(Clone)]
struct ImageCacheEntry {
    texture: egui::TextureHandle,
    size: [u32; 2],
    modified: Option<SystemTime>,
    /// Approximate memory size in bytes (width * height * 4 for RGBA).
    byte_size: usize,
}

impl ImageCacheEntry {
    /// Estimate texture memory size: width * height * 4 bytes (RGBA).
    fn estimate_bytes(size: [u32; 2]) -> usize {
        (size[0] as usize) * (size[1] as usize) * 4
    }
}

/// LRU cache for image textures with both entry count and memory size limits.
///
/// Eviction occurs when either:
/// - Entry count exceeds `capacity`
/// - Total memory exceeds `MAX_IMAGE_TEXTURE_CACHE_BYTES`
///
/// # Performance Note
///
/// The `touch()` and `remove()` operations use `VecDeque::retain()` which is O(n).
/// This is acceptable because the cache capacity is bounded (256 entries max) and
/// these operations are infrequent compared to `get()` which is O(1).
struct ImageCache {
    entries: HashMap<String, ImageCacheEntry>,
    order: VecDeque<String>,
    capacity: usize,
    /// Total approximate bytes of all cached textures.
    total_bytes: usize,
    /// Maximum total bytes before size-based eviction triggers.
    max_bytes: usize,
}

impl ImageCache {
    fn new(capacity: usize) -> Self {
        Self {
            entries: HashMap::new(),
            order: VecDeque::new(),
            capacity: capacity.max(1),
            total_bytes: 0,
            max_bytes: MAX_IMAGE_TEXTURE_CACHE_BYTES,
        }
    }

    fn get(&mut self, key: &str) -> Option<ImageCacheEntry> {
        let entry = self.entries.get(key).cloned();
        if entry.is_some() {
            self.touch(key);
        }
        entry
    }

    fn insert(&mut self, key: String, entry: ImageCacheEntry) {
        let new_bytes = entry.byte_size;

        // If key already exists, update value and adjust total_bytes
        if let Some(old_entry) = self.entries.get(&key) {
            self.total_bytes = self.total_bytes.saturating_sub(old_entry.byte_size);
            self.entries.insert(key.clone(), entry);
            self.total_bytes += new_bytes;
            self.touch(&key);
            return;
        }

        // Evict oldest entries if at entry capacity or memory limit exceeded
        while self.entries.len() >= self.capacity
            || (self.total_bytes + new_bytes > self.max_bytes && !self.entries.is_empty())
        {
            if let Some(old_key) = self.order.pop_front() {
                if let Some(old_entry) = self.entries.remove(&old_key) {
                    self.total_bytes = self.total_bytes.saturating_sub(old_entry.byte_size);
                }
            } else {
                break;
            }
        }

        self.order.push_back(key.clone());
        self.total_bytes += new_bytes;
        self.entries.insert(key, entry);
    }

    fn remove(&mut self, key: &str) {
        if let Some(entry) = self.entries.remove(key) {
            self.total_bytes = self.total_bytes.saturating_sub(entry.byte_size);
        }
        self.order.retain(|entry| entry != key);
    }

    #[cfg(test)]
    fn contains_key(&self, key: &str) -> bool {
        self.entries.contains_key(key)
    }

    /// Returns the current total bytes used by cached textures.
    #[cfg(test)]
    fn current_bytes(&self) -> usize {
        self.total_bytes
    }

    fn touch(&mut self, key: &str) {
        self.order.retain(|entry| entry != key);
        self.order.push_back(key.to_string());
    }
}

#[derive(Clone)]
struct ImageFailure {
    last_attempt: Instant,
}

enum ImageLoadSource {
    Embedded(&'static [u8]),
    File(PathBuf),
    Remote(String),
}

struct ImageLoadRequest {
    key: String,
    source: ImageLoadSource,
}

enum ImageLoadResult {
    Loaded {
        key: String,
        image: egui::ColorImage,
        size: [u32; 2],
        modified: Option<SystemTime>,
    },
    Failed {
        key: String,
    },
}

/// Markdown renderer with proper inline element handling
pub struct MarkdownRenderer {
    font_sizes: FontSizes,
    syntax_set: SyntaxSet,
    theme_set: ThemeSet,
    emoji_textures: RefCell<LruCache<String, egui::TextureHandle>>,
    image_textures: RefCell<ImageCache>,
    image_pending: RefCell<HashSet<String>>,
    image_failures: RefCell<LruCache<String, ImageFailure>>,
    image_job_tx: Sender<ImageLoadRequest>,
    image_result_rx: Receiver<ImageLoadResult>,
    // Mapping of header id -> last rendered rect (for in-document navigation)
    header_rects: RefCell<HashMap<String, egui::Rect>>,
    // Last clicked internal anchor (e.g., "getting-started") for the app to consume
    pending_anchor: RefCell<Option<String>>,
    // Unique counter to avoid egui Id collisions for repeated links
    link_counter: RefCell<u64>,
    // Per-frame table counter to build stable table ids in render order
    table_counter: RefCell<u64>,
    // Per-frame rect for each top-level element in render order
    element_rects: RefCell<Vec<egui::Rect>>,
    // Optional highlight phrase (lowercased) for in-text highlighting
    highlight_phrase: RefCell<Option<String>>,
    // Cache for image/diagram textures
    // Base directory used to resolve relative image paths
    base_dir: RefCell<Option<PathBuf>>,
    mermaid: MermaidRenderer,
    table_layout_cache: RefCell<CellLayoutCache>,
    table_metrics: RefCell<TableMetrics>,
    column_stats_cache: RefCell<HashMap<u64, ColumnStatsCacheEntry>>,
    /// Allow loading images from remote URLs (http/https)
    allow_remote_images: Cell<bool>,
    /// Current theme mode — true for dark, false for light.
    /// Used by syntect highlighting at parse time.
    dark_mode: bool,
}

impl Default for MarkdownRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl MarkdownRenderer {
    #[cfg_attr(not(test), allow(dead_code))]
    fn cell_fragments<'a>(&'a self, spans: &'a [InlineSpan]) -> Vec<CellFragment<'a>> {
        if let Some(emoji) = self.cell_single_emoji(spans) {
            return vec![CellFragment::Emoji(emoji)];
        }
        let mut fragments = Vec::new();
        let mut run_start: Option<usize> = None;
        let flush_run =
            |start: &mut Option<usize>, end: usize, fragments: &mut Vec<CellFragment<'a>>| {
                if let Some(run_begin) = start.take() {
                    fragments.push(CellFragment::Text(&spans[run_begin..end]));
                }
            };

        for (idx, span) in spans.iter().enumerate() {
            if matches!(span, InlineSpan::Image { .. }) {
                flush_run(&mut run_start, idx, &mut fragments);
                fragments.push(CellFragment::Image(span));
                continue;
            }

            if run_start.is_none() {
                run_start = Some(idx);
            }
        }

        if let Some(start) = run_start {
            fragments.push(CellFragment::Text(&spans[start..]));
        }

        fragments
    }

    fn cell_single_emoji(&self, spans: &[InlineSpan]) -> Option<String> {
        let mut text = String::new();
        for span in spans {
            match span {
                InlineSpan::Text(t)
                | InlineSpan::Strong(t)
                | InlineSpan::Emphasis(t)
                | InlineSpan::Strikethrough(t) => text.push_str(t),
                InlineSpan::Code(_) | InlineSpan::Link { .. } | InlineSpan::Image { .. } => {
                    return None;
                }
            }
        }

        let trimmed = text.trim();
        let mut graphemes = trimmed.graphemes(true);
        let first = graphemes.next()?;
        if graphemes.next().is_some() {
            return None;
        }
        self.emoji_key_for_grapheme(first)
    }

    fn highlight_segments(&self, text: &str, highlight: Option<&str>) -> Vec<(Range<usize>, bool)> {
        if text.is_empty() {
            return Vec::new();
        }
        let Some(needle) = highlight.filter(|h| !h.is_empty()) else {
            return vec![(0..text.len(), false)];
        };

        let mut folded = String::new();
        let mut folded_to_char: Vec<usize> = Vec::new();
        let mut char_ranges: Vec<(usize, usize)> = Vec::new();
        for (char_idx, (byte_idx, ch)) in text.char_indices().enumerate() {
            let folded_piece: String = ch.to_string().case_fold().nfkc().collect();
            let before = folded.len();
            folded.push_str(&folded_piece);
            let after = folded.len();
            for _ in before..after {
                folded_to_char.push(char_idx);
            }
            char_ranges.push((byte_idx, byte_idx + ch.len_utf8()));
        }

        let mut segments = Vec::new();
        let mut rendered_until = 0usize;
        let mut search_at = 0usize;
        while let Some(pos) = folded[search_at..].find(needle) {
            let abs = search_at + pos;
            let start_char_idx = folded_to_char[abs];
            let (start_byte, _) = char_ranges[start_char_idx];
            if start_byte > rendered_until {
                segments.push((rendered_until..start_byte, false));
            }
            let match_end = abs + needle.len();
            let end_char_idx = folded_to_char[match_end.saturating_sub(1)];
            let (_, end_byte) = char_ranges[end_char_idx];
            segments.push((start_byte..end_byte, true));
            rendered_until = end_byte;
            search_at = match_end;
        }

        if rendered_until < text.len() {
            segments.push((rendered_until..text.len(), false));
        }

        segments
    }

    #[cfg_attr(not(test), allow(dead_code))]
    fn build_layout_job(
        &self,
        style: &egui::Style,
        spans: &[InlineSpan],
        wrap_width: f32,
        strong_override: bool,
        halign: Align,
    ) -> LayoutJobBuild {
        let mut job = LayoutJob {
            wrap: TextWrapping {
                max_width: wrap_width.max(1.0),
                ..Default::default()
            },
            break_on_newline: true,
            halign,
            ..Default::default()
        };

        let highlight = self
            .highlight_phrase
            .borrow()
            .clone()
            .filter(|s| !s.is_empty());

        let mut plain_text = String::new();
        let mut link_ranges = Vec::new();
        let mut job_char_offset = 0usize;

        for span in spans {
            match span {
                InlineSpan::Image { .. } => {}
                InlineSpan::Code(code) => {
                    job_char_offset +=
                        self.append_code_span(style, &mut job, &mut plain_text, code);
                }
                InlineSpan::Link { text, url } => {
                    let tc = ThemeColors::current(style.visuals.dark_mode);
                    let inline_style = InlineStyle {
                        strong: strong_override,
                        color: Some(if Self::is_external_url(url) {
                            tc.link
                        } else {
                            tc.link_internal
                        }),
                        ..Default::default()
                    };
                    let mut normalized = self.fix_unicode_chars(text);
                    normalized = Self::expand_shortcodes(&normalized);
                    normalized = Self::expand_superscripts(&normalized);
                    let appended = self.append_text_sections(
                        style,
                        &mut job,
                        &mut plain_text,
                        &normalized,
                        self.font_sizes.body,
                        inline_style,
                        highlight.as_deref(),
                    );
                    if appended > 0 {
                        let start_char = job_char_offset;
                        job_char_offset += appended;
                        link_ranges.push(LinkRange {
                            char_range: start_char..job_char_offset,
                            url: url.clone(),
                        });
                    }
                }
                InlineSpan::Strong(text) => {
                    job_char_offset += self.append_plain_span(
                        style,
                        &mut job,
                        &mut plain_text,
                        text,
                        InlineStyle {
                            strong: true,
                            ..Default::default()
                        },
                        highlight.as_deref(),
                    );
                }
                InlineSpan::Emphasis(text) => {
                    job_char_offset += self.append_plain_span(
                        style,
                        &mut job,
                        &mut plain_text,
                        text,
                        InlineStyle {
                            italics: true,
                            strong: strong_override,
                            ..Default::default()
                        },
                        highlight.as_deref(),
                    );
                }
                InlineSpan::Strikethrough(text) => {
                    job_char_offset += self.append_plain_span(
                        style,
                        &mut job,
                        &mut plain_text,
                        text,
                        InlineStyle {
                            strike: true,
                            strong: strong_override,
                            ..Default::default()
                        },
                        highlight.as_deref(),
                    );
                }
                InlineSpan::Text(text) => {
                    job_char_offset += self.append_plain_span(
                        style,
                        &mut job,
                        &mut plain_text,
                        text,
                        InlineStyle {
                            strong: strong_override,
                            ..Default::default()
                        },
                        highlight.as_deref(),
                    );
                }
            }
        }

        LayoutJobBuild {
            job,
            plain_text,
            link_ranges,
        }
    }

    fn append_plain_span(
        &self,
        style: &egui::Style,
        job: &mut LayoutJob,
        plain_text: &mut String,
        text: &str,
        inline_style: InlineStyle,
        highlight: Option<&str>,
    ) -> usize {
        if text.is_empty() {
            return 0;
        }
        let mut normalized = self.fix_unicode_chars(text);
        normalized = Self::expand_shortcodes(&normalized);
        normalized = Self::expand_superscripts(&normalized);
        self.append_text_sections(
            style,
            job,
            plain_text,
            &normalized,
            self.font_sizes.body,
            inline_style,
            highlight,
        )
    }

    fn append_code_span(
        &self,
        style: &egui::Style,
        job: &mut LayoutJob,
        plain_text: &mut String,
        code: &str,
    ) -> usize {
        if code.is_empty() {
            return 0;
        }
        plain_text.push_str(code);
        let tc = ThemeColors::current(style.visuals.dark_mode);
        let (bg, fg) = (tc.inline_code_bg, tc.inline_code_fg);
        let rich = RichText::new(code.to_string())
            .size(self.font_sizes.code)
            .monospace()
            .background_color(bg)
            .color(fg);
        rich.append_to(job, style, FontSelection::Default, Align::LEFT);
        code.chars().count()
    }

    #[allow(clippy::too_many_arguments)]
    fn append_text_sections(
        &self,
        style: &egui::Style,
        job: &mut LayoutJob,
        plain_text: &mut String,
        text: &str,
        font_size: f32,
        inline_style: InlineStyle,
        highlight: Option<&str>,
    ) -> usize {
        if text.is_empty() {
            return 0;
        }
        plain_text.push_str(text);
        let char_count = text.chars().count();
        let visuals = &style.visuals;
        let segments = self.highlight_segments(text, highlight);
        for (range, highlighted) in segments {
            let slice = &text[range];
            let mut rich = RichText::new(slice.to_string()).size(font_size);
            if inline_style.strong {
                rich = rich.strong();
            }
            if inline_style.italics {
                rich = rich.italics();
            }
            if inline_style.strike {
                rich = rich.strikethrough();
            }
            let mut text_color = inline_style.color;
            if highlighted {
                rich = rich.background_color(visuals.selection.bg_fill);
                if text_color.is_none() {
                    text_color = Some(visuals.selection.stroke.color);
                }
            } else if text_color.is_none() && inline_style.strong {
                if let Some(override_color) = visuals.override_text_color {
                    text_color = Some(override_color);
                }
            }
            if let Some(color) = text_color {
                rich = rich.color(color);
            }
            rich.append_to(job, style, FontSelection::Default, Align::LEFT);
        }
        char_count
    }

    /// Create a new markdown renderer
    pub fn new() -> Self {
        let mermaid = MermaidRenderer::new();
        let (image_job_tx, image_job_rx) = bounded(IMAGE_MAX_PENDING.max(1));
        let (image_result_tx, image_result_rx) = bounded(IMAGE_MAX_PENDING.max(1) * 2);
        Self::spawn_image_loader(image_job_rx, image_result_tx);
        Self {
            font_sizes: FontSizes::default(),
            syntax_set: SyntaxSet::load_defaults_newlines(),
            theme_set: ThemeSet::load_defaults(),
            emoji_textures: RefCell::new(LruCache::new(EMOJI_TEXTURE_CACHE_CAPACITY)),
            image_textures: RefCell::new(ImageCache::new(IMAGE_TEXTURE_CACHE_CAPACITY)),
            image_pending: RefCell::new(HashSet::new()),
            image_failures: RefCell::new(LruCache::new(IMAGE_FAILURE_CACHE_CAPACITY)),
            image_job_tx,
            image_result_rx,
            header_rects: RefCell::new(HashMap::new()),
            pending_anchor: RefCell::new(None),
            link_counter: RefCell::new(0),
            table_counter: RefCell::new(0),
            element_rects: RefCell::new(Vec::new()),
            highlight_phrase: RefCell::new(None),
            base_dir: RefCell::new(None),
            mermaid,
            table_layout_cache: RefCell::new(CellLayoutCache::new(TABLE_LAYOUT_CACHE_CAPACITY)),
            table_metrics: RefCell::new(TableMetrics::default()),
            column_stats_cache: RefCell::new(HashMap::new()),
            allow_remote_images: Cell::new(false),
            dark_mode: true,
        }
    }

    /// Switch between dark and light theme for syntect highlighting.
    pub fn set_dark_mode(&mut self, dark: bool) {
        self.dark_mode = dark;
    }

    /// Set whether remote images (http/https URLs) should be loaded
    pub fn set_allow_remote_images(&self, allow: bool) {
        self.allow_remote_images.set(allow);
    }

    /// Get whether remote images are currently allowed
    pub fn allow_remote_images(&self) -> bool {
        self.allow_remote_images.get()
    }

    fn spawn_image_loader(job_rx: Receiver<ImageLoadRequest>, result_tx: Sender<ImageLoadResult>) {
        if let Err(err) = Self::spawn_named_thread("mdmdview-image-loader", move || {
            for request in job_rx.iter() {
                let ImageLoadRequest { key, source } = request;
                let result = match source {
                    ImageLoadSource::Embedded(bytes) => {
                        match image_decode::bytes_to_color_image_guess(bytes, None) {
                            Some((image, w, h)) => ImageLoadResult::Loaded {
                                key,
                                image,
                                size: [w, h],
                                modified: None,
                            },
                            None => ImageLoadResult::Failed { key },
                        }
                    }
                    ImageLoadSource::File(path) => {
                        if !path.exists() {
                            ImageLoadResult::Failed { key }
                        } else {
                            match std::fs::read(&path) {
                                Ok(bytes) => {
                                    let modified = Self::disk_image_timestamp(&path);
                                    match image_decode::bytes_to_color_image_guess(&bytes, None) {
                                        Some((image, w, h)) => ImageLoadResult::Loaded {
                                            key,
                                            image,
                                            size: [w, h],
                                            modified,
                                        },
                                        None => ImageLoadResult::Failed { key },
                                    }
                                }
                                Err(_) => ImageLoadResult::Failed { key },
                            }
                        }
                    }
                    ImageLoadSource::Remote(url) => match ureq::get(&url).call() {
                        Ok(response) => {
                            let mut bytes = Vec::new();
                            match response.into_reader().read_to_end(&mut bytes) {
                                Ok(_) => {
                                    match image_decode::bytes_to_color_image_guess(&bytes, None) {
                                        Some((image, w, h)) => ImageLoadResult::Loaded {
                                            key,
                                            image,
                                            size: [w, h],
                                            modified: None,
                                        },
                                        None => ImageLoadResult::Failed { key },
                                    }
                                }
                                Err(_) => ImageLoadResult::Failed { key },
                            }
                        }
                        Err(_) => ImageLoadResult::Failed { key },
                    },
                };
                let _ = result_tx.send(result);
            }
        }) {
            eprintln!("Failed to start image loader thread: {err}");
        }
    }

    fn spawn_named_thread(
        name: &str,
        f: impl FnOnce() + Send + 'static,
    ) -> std::io::Result<std::thread::JoinHandle<()>> {
        #[cfg(test)]
        if FORCE_THREAD_SPAWN_ERROR.swap(false, Ordering::Relaxed) {
            return Err(std::io::Error::other("forced thread spawn failure"));
        }
        std::thread::Builder::new().name(name.to_string()).spawn(f)
    }

    #[cfg(test)]
    fn force_thread_spawn_error_for_test() {
        FORCE_THREAD_SPAWN_ERROR.store(true, Ordering::Relaxed);
    }

    /// UI scale factor derived from body font size relative to default.
    /// Used to scale non-text elements (e.g., images) consistently with zoom.
    fn ui_scale(&self) -> f32 {
        let default_body = FontSizes::default().body;
        (self.font_sizes.body / default_body).clamp(0.5, 4.0)
    }

    /// Get current font sizes
    pub fn font_sizes(&self) -> &FontSizes {
        &self.font_sizes
    }

    fn escape_table_pipes_in_inline_code(markdown: &str) -> String {
        let mut out = String::with_capacity(markdown.len());
        let mut in_table = false;
        let mut table_blockquote_level: Option<usize> = None;
        let mut table_list_indent: Option<usize> = None;
        let mut in_fenced_block = false;
        let mut fence_char = '\0';
        let mut fence_len = 0usize;
        let mut fence_blockquote_level: Option<usize> = None;

        let lines: Vec<&str> = markdown.split_inclusive('\n').collect();
        let mut i = 0;
        while i < lines.len() {
            let line = lines[i];
            let (blockquote_level, rest) = Self::table_line_info(line);
            let list_info = Self::list_marker_info(rest);
            let mut list_indent = list_info.map(|(_, _, content_indent)| content_indent);
            let mut list_stripped = list_info.map(|(content, _, _)| content).unwrap_or(rest);
            let mut list_marker_present = list_info.is_some();
            if !list_marker_present && i > 0 {
                let parent_list_indent =
                    Self::parent_list_indent_for_line(&lines, i, blockquote_level, rest);
                if let Some(parent_indent) = parent_list_indent {
                    if let Some((content, _indent_after, content_indent, leading_spaces)) =
                        Self::list_marker_info_any_indent(rest)
                    {
                        if leading_spaces < parent_indent + 4 {
                            list_marker_present = true;
                            list_indent = Some(content_indent);
                            list_stripped = content;
                        }
                    }
                    if list_indent.is_none() {
                        let code_strip = Self::strip_indent_columns(rest, parent_indent + 4);
                        if code_strip.is_none() {
                            list_indent = Some(parent_indent);
                        }
                    }
                }
            }
            let (fence_line_level, fence_line_rest) = if list_marker_present {
                let (nested_level, nested_rest) = Self::table_line_info(list_stripped);
                (blockquote_level + nested_level, nested_rest)
            } else {
                Self::table_line_info_with_list(line, list_indent)
            };
            let trimmed_fence = fence_line_rest.trim_start();

            if in_fenced_block {
                let fence_end = fence_blockquote_level == Some(fence_line_level)
                    && Self::is_fence_end(trimmed_fence, fence_char, fence_len);
                if fence_end {
                    in_fenced_block = false;
                    fence_char = '\0';
                    fence_len = 0;
                    fence_blockquote_level = None;
                }
                out.push_str(line);
                i += 1;
                continue;
            }

            if let Some((ch, len)) = Self::fence_start(trimmed_fence) {
                in_fenced_block = true;
                fence_char = ch;
                fence_len = len;
                fence_blockquote_level = Some(fence_line_level);
                out.push_str(line);
                i += 1;
                continue;
            }

            if in_table {
                if let Some((level, rest)) = Self::table_line_info_in_list(line, table_list_indent)
                {
                    if table_blockquote_level == Some(level) && Self::is_table_row_candidate(rest) {
                        out.push_str(&Self::escape_pipes_in_inline_code_line(line));
                        i += 1;
                        continue;
                    }
                }
                in_table = false;
                table_blockquote_level = None;
                table_list_indent = None;
                out.push_str(line);
                i += 1;
                continue;
            }

            if i + 1 < lines.len() {
                let header_from_list_marker = list_marker_present;
                let (level, rest) = if list_marker_present {
                    let (nested_level, nested_rest) = Self::table_line_info(list_stripped);
                    (blockquote_level + nested_level, nested_rest)
                } else {
                    Self::table_line_info_with_list(line, list_indent)
                };
                let next_info = if list_indent.is_some() {
                    Self::table_line_info_in_list(lines[i + 1], list_indent)
                } else {
                    Some(Self::table_line_info(lines[i + 1]))
                };
                let header_line = if header_from_list_marker {
                    let (_, stripped) = Self::table_line_info(list_stripped);
                    stripped
                } else {
                    rest
                };

                if let Some((next_level, next_rest)) = next_info {
                    if level == next_level
                        && Self::is_table_row_candidate(header_line)
                        && Self::is_table_delimiter_line(next_rest)
                    {
                        if !header_from_list_marker && i > 0 {
                            // Add blank line separator when the previous line has content
                            // at the same level, to help CommonMark parsers recognize tables.
                            // Skip this for plain list items (no blockquote) where the
                            // indentation alone is sufficient.
                            let in_blockquote = level > 0;
                            let (prev_level, prev_rest) =
                                Self::table_line_info_with_list(lines[i - 1], list_indent);
                            if prev_level == level
                                && !prev_rest.trim().is_empty()
                                && (list_indent.is_none() || in_blockquote)
                            {
                                let prefix_len = line.len().saturating_sub(rest.len());
                                out.push_str(&line[..prefix_len]);
                                let newline = if line.ends_with("\r\n") { "\r\n" } else { "\n" };
                                out.push_str(newline);
                            }
                        }
                        out.push_str(&Self::escape_pipes_in_inline_code_line(line));
                        out.push_str(lines[i + 1]);
                        in_table = true;
                        table_blockquote_level = Some(level);
                        table_list_indent = list_indent;
                        i += 2;
                        continue;
                    }
                }
            }

            out.push_str(line);
            i += 1;
        }

        out
    }

    fn escape_pipes_in_inline_code_line(line: &str) -> String {
        if !line.contains('|') || !line.contains('`') {
            return line.to_string();
        }
        let mut out: Vec<char> = Vec::with_capacity(line.len());
        let mut in_code = false;
        let mut delimiter_len = 0usize;
        let mut pending_pipes: Vec<usize> = Vec::new();
        let mut backslash_run = 0usize;
        let mut chars = line.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                backslash_run += 1;
                out.push(ch);
                continue;
            }

            let escaped = !in_code && backslash_run % 2 == 1;
            backslash_run = 0;

            if ch == '`' {
                if escaped {
                    out.push('`');
                    continue;
                }
                let mut run_len = 1usize;
                while matches!(chars.peek(), Some('`')) {
                    chars.next();
                    run_len += 1;
                }
                if in_code {
                    if run_len == delimiter_len {
                        in_code = false;
                        delimiter_len = 0;
                        pending_pipes.clear();
                    }
                } else {
                    in_code = true;
                    delimiter_len = run_len;
                    pending_pipes.clear();
                }
                out.extend(std::iter::repeat_n('`', run_len));
                continue;
            }

            if in_code && ch == '|' {
                pending_pipes.push(out.len());
                out.push(PIPE_SENTINEL);
            } else {
                out.push(ch);
            }
        }

        if in_code {
            for idx in pending_pipes {
                out[idx] = '|';
            }
        }

        out.into_iter().collect()
    }

    fn is_html_line_break(html: &str) -> bool {
        let trimmed = html.trim();
        if trimmed.len() < 4 {
            return false;
        }
        let normalized = trimmed.to_ascii_lowercase();
        if !normalized.starts_with("<br") || !normalized.ends_with('>') {
            return false;
        }
        let rest = &normalized[3..];
        let first = rest.chars().next();
        match first {
            Some('>') => true,
            Some('/') => rest[1..].trim_start().starts_with('>'),
            Some(ch) if ch.is_whitespace() => true,
            _ => false,
        }
    }

    fn tab_advance(col: usize) -> usize {
        let rem = col % 4;
        if rem == 0 {
            4
        } else {
            4 - rem
        }
    }

    fn is_indented_code_line(line: &str) -> bool {
        let mut col = 0usize;
        for ch in line.chars() {
            match ch {
                ' ' => {
                    col += 1;
                    if col >= 4 {
                        return true;
                    }
                }
                '\t' => {
                    col += Self::tab_advance(col);
                    return col >= 4;
                }
                _ => break,
            }
        }
        col >= 4
    }

    fn table_line_info(line: &str) -> (usize, &str) {
        let mut rest = line;
        let mut level = 0usize;
        loop {
            let bytes = rest.as_bytes();
            let mut idx = 0usize;
            while idx < bytes.len() && idx < 3 && bytes[idx] == b' ' {
                idx += 1;
            }
            if idx < bytes.len() && bytes[idx] == b'>' {
                idx += 1;
                if idx < bytes.len() && (bytes[idx] == b' ' || bytes[idx] == b'\t') {
                    idx += 1;
                }
                level += 1;
                rest = &rest[idx..];
                continue;
            }
            break;
        }
        (level, rest)
    }

    fn table_line_info_with_list(line: &str, list_indent: Option<usize>) -> (usize, &str) {
        let (level, rest) = Self::table_line_info(line);
        if let Some(indent) = list_indent {
            if let Some(stripped) = Self::strip_indent_columns(rest, indent) {
                let (nested_level, nested_rest) = Self::table_line_info(stripped);
                return (level + nested_level, nested_rest);
            }
            if let Some(stripped) = Self::strip_indent_columns(line, indent) {
                return Self::table_line_info(stripped);
            }
        }
        (level, rest)
    }

    fn table_line_info_in_list(line: &str, list_indent: Option<usize>) -> Option<(usize, &str)> {
        let (level, rest) = Self::table_line_info(line);
        if let Some(indent) = list_indent {
            if let Some(stripped) = Self::strip_indent_columns(rest, indent) {
                let (nested_level, nested_rest) = Self::table_line_info(stripped);
                return Some((level + nested_level, nested_rest));
            }
            let stripped = Self::strip_indent_columns(line, indent)?;
            return Some(Self::table_line_info(stripped));
        }
        Some((level, rest))
    }

    fn list_marker_info(line: &str) -> Option<(&str, usize, usize)> {
        let bytes = line.as_bytes();
        let mut idx = 0usize;
        let mut spaces = 0usize;
        while idx < bytes.len() && spaces < 3 && bytes[idx] == b' ' {
            idx += 1;
            spaces += 1;
        }
        if idx < bytes.len() && bytes[idx] == b'\t' {
            return None;
        }
        if idx >= bytes.len() {
            return None;
        }
        let marker_start = idx;
        match bytes[idx] {
            b'-' | b'+' | b'*' => {
                idx += 1;
            }
            b'0'..=b'9' => {
                while idx < bytes.len() && bytes[idx].is_ascii_digit() {
                    idx += 1;
                }
                if idx < bytes.len() && (bytes[idx] == b'.' || bytes[idx] == b')') {
                    idx += 1;
                } else {
                    return None;
                }
            }
            _ => return None,
        }
        let marker_width = idx.saturating_sub(marker_start);
        let mut indent = 0usize;
        let mut has_ws = false;
        let mut col = spaces + marker_width;
        while idx < bytes.len() {
            match bytes[idx] {
                b' ' => {
                    indent += 1;
                    col += 1;
                    idx += 1;
                    has_ws = true;
                }
                b'\t' => {
                    let advance = Self::tab_advance(col);
                    indent += advance;
                    col += advance;
                    idx += 1;
                    has_ws = true;
                }
                _ => break,
            }
        }
        if !has_ws {
            return None;
        }
        let content_indent = col;
        Some((&line[idx..], indent, content_indent))
    }

    fn list_marker_info_any_indent(line: &str) -> Option<(&str, usize, usize, usize)> {
        let bytes = line.as_bytes();
        let mut idx = 0usize;
        let mut leading_cols = 0usize;
        while idx < bytes.len() {
            match bytes[idx] {
                b' ' => {
                    idx += 1;
                    leading_cols += 1;
                }
                b'\t' => {
                    let advance = Self::tab_advance(leading_cols);
                    idx += 1;
                    leading_cols += advance;
                }
                _ => break,
            }
        }
        if idx >= bytes.len() {
            return None;
        }
        let marker_start = idx;
        match bytes[idx] {
            b'-' | b'+' | b'*' => {
                idx += 1;
            }
            b'0'..=b'9' => {
                while idx < bytes.len() && bytes[idx].is_ascii_digit() {
                    idx += 1;
                }
                if idx < bytes.len() && (bytes[idx] == b'.' || bytes[idx] == b')') {
                    idx += 1;
                } else {
                    return None;
                }
            }
            _ => return None,
        }
        let marker_width = idx.saturating_sub(marker_start);
        let mut indent = 0usize;
        let mut has_ws = false;
        let mut col = leading_cols + marker_width;
        while idx < bytes.len() {
            match bytes[idx] {
                b' ' => {
                    indent += 1;
                    col += 1;
                    idx += 1;
                    has_ws = true;
                }
                b'\t' => {
                    let advance = Self::tab_advance(col);
                    indent += advance;
                    col += advance;
                    idx += 1;
                    has_ws = true;
                }
                _ => break,
            }
        }
        if !has_ws {
            return None;
        }
        let content_indent = col;
        Some((&line[idx..], indent, content_indent, leading_cols))
    }

    fn parent_list_indent_for_line(
        lines: &[&str],
        idx: usize,
        level: usize,
        line_rest: &str,
    ) -> Option<usize> {
        if idx == 0 {
            return None;
        }
        for back_idx in (0..idx).rev() {
            let (prev_level, prev_rest) = Self::table_line_info(lines[back_idx]);
            if prev_level < level {
                break;
            }
            if prev_level > level {
                continue;
            }
            if prev_rest.trim().is_empty() {
                continue;
            }
            if let Some((_, _, content_indent)) = Self::list_marker_info(prev_rest) {
                if Self::strip_indent_columns(line_rest, content_indent).is_some() {
                    return Some(content_indent);
                }
            } else if let Some((_, _, content_indent, _)) =
                Self::list_marker_info_any_indent(prev_rest)
            {
                if Self::strip_indent_columns(line_rest, content_indent).is_some() {
                    return Some(content_indent);
                }
            } else if !prev_rest.starts_with(' ') && !prev_rest.starts_with('\t') {
                break;
            }
        }
        None
    }

    fn strip_indent_columns(line: &str, indent: usize) -> Option<&str> {
        if indent == 0 {
            return Some(line);
        }
        let mut col = 0usize;
        let mut cut = 0usize;
        for (idx, ch) in line.char_indices() {
            if col >= indent {
                break;
            }
            match ch {
                ' ' => {
                    col += 1;
                    cut = idx + ch.len_utf8();
                }
                '\t' => {
                    col += Self::tab_advance(col);
                    cut = idx + ch.len_utf8();
                }
                _ => return None,
            }
        }
        if col >= indent {
            Some(&line[cut..])
        } else {
            None
        }
    }

    fn has_pipe_outside_inline_code(line: &str) -> bool {
        if !line.contains('|') {
            return false;
        }
        let mut in_code = false;
        let mut delimiter_len = 0usize;
        let mut backslash_run = 0usize;
        let mut chars = line.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                backslash_run += 1;
                continue;
            }

            let escaped = !in_code && backslash_run % 2 == 1;
            backslash_run = 0;

            if ch == '`' {
                if escaped {
                    continue;
                }
                let mut run_len = 1usize;
                while matches!(chars.peek(), Some('`')) {
                    chars.next();
                    run_len += 1;
                }
                if in_code {
                    if run_len == delimiter_len {
                        in_code = false;
                        delimiter_len = 0;
                    }
                } else {
                    in_code = true;
                    delimiter_len = run_len;
                }
                continue;
            }

            if ch == '|' && !in_code && !escaped {
                return true;
            }
        }

        false
    }

    fn is_table_row_candidate(line: &str) -> bool {
        if Self::is_indented_code_line(line) {
            return false;
        }
        let trimmed_line = line.trim_end_matches(['\r', '\n']);
        if trimmed_line.trim().is_empty() {
            return false;
        }
        Self::has_pipe_outside_inline_code(trimmed_line)
    }

    fn is_table_delimiter_line(line: &str) -> bool {
        if Self::is_indented_code_line(line) {
            return false;
        }
        let trimmed = line.trim_end_matches(['\r', '\n']).trim();
        if trimmed.is_empty() {
            return false;
        }
        let mut has_dash = false;
        let mut has_pipe = false;
        for ch in trimmed.chars() {
            match ch {
                '-' => has_dash = true,
                '|' => has_pipe = true,
                ':' | ' ' | '\t' => {}
                _ => return false,
            }
        }
        has_dash && has_pipe
    }

    fn fence_start(line: &str) -> Option<(char, usize)> {
        let trimmed = line.trim_start();
        let mut chars = trimmed.chars();
        let first = chars.next()?;
        if first != '`' && first != '~' {
            return None;
        }
        let mut count = 1usize;
        for ch in chars {
            if ch == first {
                count += 1;
            } else {
                break;
            }
        }
        if count >= 3 {
            Some((first, count))
        } else {
            None
        }
    }

    fn is_fence_end(line: &str, fence_char: char, fence_len: usize) -> bool {
        let trimmed = line.trim_start();
        let mut count = 0usize;
        for ch in trimmed.chars() {
            if ch == fence_char {
                count += 1;
            } else {
                break;
            }
        }
        count >= fence_len
    }

    fn restore_pipe_sentinel(text: &str) -> String {
        if text.contains(PIPE_SENTINEL) {
            text.chars()
                .map(|ch| if ch == PIPE_SENTINEL { '|' } else { ch })
                .collect()
        } else {
            text.to_string()
        }
    }

    /// Parse markdown content into elements with proper inline handling
    pub fn parse(&self, markdown: &str) -> Result<Vec<MarkdownElement>, anyhow::Error> {
        #[cfg(test)]
        if take_forced_parse_error() {
            return Err(anyhow::anyhow!("forced parse error"));
        }

        let mut options = Options::empty();
        options.insert(Options::ENABLE_STRIKETHROUGH);
        options.insert(Options::ENABLE_TABLES);
        options.insert(Options::ENABLE_TASKLISTS);

        let prepared = Self::escape_table_pipes_in_inline_code(markdown);
        let parser = Parser::new_ext(&prepared, options);
        let mut elements = Vec::new();
        let events = parser.collect::<Vec<_>>();

        let mut i = 0;
        // Track header slug occurrences for stable de-duplication
        let mut slug_counts: HashMap<String, usize> = HashMap::new();
        while i < events.len() {
            i = self.parse_element(&events, i, &mut elements, &mut slug_counts);
        }

        Ok(elements)
    }

    /// Parse a single element from the event stream
    fn parse_element(
        &self,
        events: &[Event],
        start: usize,
        elements: &mut Vec<MarkdownElement>,
        slug_counts: &mut HashMap<String, usize>,
    ) -> usize {
        match &events[start] {
            Event::Start(Tag::Paragraph) => {
                // Always preserve line breaks in paragraphs (consistent with blockquotes)
                // This allows poetry, lyrics, and structured content to render correctly
                let (spans, next_idx) =
                    self.parse_inline_spans_with_breaks(events, start + 1, Tag::Paragraph, true);
                if !spans.is_empty() {
                    elements.push(MarkdownElement::Paragraph(spans));
                }
                next_idx
            }
            // Images encountered outside paragraphs: treat as a single paragraph with an inline image
            Event::Start(Tag::Image(_, url, title)) => {
                let (alt, next_idx) = self.collect_until_tag_end(
                    events,
                    start + 1,
                    Tag::Image(LinkType::Inline, "".into(), "".into()),
                    false,
                );
                let spans = vec![InlineSpan::Image {
                    src: url.to_string(),
                    alt,
                    title: if title.is_empty() {
                        None
                    } else {
                        Some(title.to_string())
                    },
                }];
                elements.push(MarkdownElement::Paragraph(spans));
                next_idx
            }
            Event::Start(Tag::Heading(level, _, _)) => {
                let (spans, next_idx) =
                    self.parse_inline_spans(events, start + 1, Tag::Heading(*level, None, vec![]));
                let id = Self::make_header_id(&spans, slug_counts);
                elements.push(MarkdownElement::Header {
                    level: *level as u8,
                    spans,
                    id,
                });
                next_idx
            }
            Event::Start(Tag::CodeBlock(_)) => {
                let (code_text, language, next_idx) = self.parse_code_block(events, start);
                let highlighted = self.highlight_code(language.as_deref(), &code_text);
                elements.push(MarkdownElement::CodeBlock {
                    language,
                    text: code_text,
                    highlighted,
                });
                next_idx
            }
            Event::Start(Tag::List(first_item)) => {
                let (items, next_idx) = self.parse_list(events, start + 1, slug_counts);
                elements.push(MarkdownElement::List {
                    ordered: first_item.is_some(),
                    items,
                });
                next_idx
            }
            Event::Rule => {
                elements.push(MarkdownElement::HorizontalRule);
                start + 1
            }
            Event::Start(Tag::BlockQuote) => {
                let (quotes, next_idx) =
                    self.collect_blockquotes(events, start + 1, 1, slug_counts);
                for (depth, blocks) in quotes {
                    elements.push(MarkdownElement::Quote { depth, blocks });
                }
                next_idx
            }
            Event::Start(Tag::Table(alignments)) => {
                let (headers, rows, next_idx) = self.parse_table(events, start + 1);
                elements.push(MarkdownElement::Table {
                    headers,
                    rows,
                    alignments: alignments.to_vec(),
                });
                next_idx
            }
            _ => {
                // Skip other events
                start + 1
            }
        }
    }

    // Build a slug from text similar to GitHub: lowercase, spaces->dashes, strip punctuation
    fn slugify(text: &str) -> String {
        let mut out = String::with_capacity(text.len());
        let mut last_dash = false;
        for ch in text.chars() {
            let c = ch.to_ascii_lowercase();
            if c.is_ascii_alphanumeric() {
                out.push(c);
                last_dash = false;
            } else if c.is_whitespace() || c == '-' {
                if !last_dash && !out.is_empty() {
                    out.push('-');
                    last_dash = true;
                }
            } else {
                // drop punctuation/symbols
            }
        }
        // Trim trailing dash if any
        if out.ends_with('-') {
            out.pop();
        }
        out
    }

    /// Generate a de-duplicated header slug and return the id string.
    /// Tracks occurrence counts in `slug_counts` to append `-N` suffixes for duplicates.
    fn make_header_id(spans: &[InlineSpan], slug_counts: &mut HashMap<String, usize>) -> String {
        let title_text = Self::spans_plain_text(spans);
        let base = Self::slugify(&title_text);
        let count = slug_counts.entry(base.clone()).or_insert(0);
        let id = if *count == 0 {
            base.clone()
        } else {
            format!("{}-{}", base, *count)
        };
        *count += 1;
        id
    }

    fn spans_plain_text(spans: &[InlineSpan]) -> String {
        let mut s = String::new();
        for span in spans {
            match span {
                InlineSpan::Text(t)
                | InlineSpan::Strong(t)
                | InlineSpan::Emphasis(t)
                | InlineSpan::Strikethrough(t) => s.push_str(t),
                InlineSpan::Code(t) => s.push_str(t),
                InlineSpan::Link { text, .. } => s.push_str(text),
                InlineSpan::Image { alt, title, .. } => {
                    if !alt.is_empty() {
                        s.push_str(alt);
                    }
                    if let Some(t) = title {
                        if !t.is_empty() {
                            if !s.is_empty() {
                                s.push(' ');
                            }
                            s.push_str(t);
                        }
                    }
                }
            }
        }
        s
    }

    /// Extract plain text from all markdown elements
    pub fn elements_to_plain_text(elements: &[MarkdownElement]) -> String {
        let mut result = String::new();
        for element in elements.iter() {
            match element {
                MarkdownElement::Paragraph(spans) | MarkdownElement::Header { spans, .. } => {
                    if !result.is_empty() {
                        result.push('\n');
                    }
                    result.push_str(&Self::spans_plain_text(spans));
                }
                MarkdownElement::CodeBlock { text, .. } => {
                    if !result.is_empty() {
                        result.push('\n');
                    }
                    result.push_str(text);
                }
                MarkdownElement::List { items, .. } => {
                    for item in items {
                        for block in &item.blocks {
                            if !result.is_empty() {
                                result.push('\n');
                            }
                            result.push_str(&Self::element_plain_text(block));
                        }
                    }
                }
                MarkdownElement::Quote { blocks, .. } => {
                    for block in blocks {
                        if !result.is_empty() {
                            result.push('\n');
                        }
                        result.push_str(&Self::element_plain_text(block));
                    }
                }
                MarkdownElement::Table {
                    headers,
                    rows,
                    alignments: _,
                } => {
                    // Headers
                    for header in headers {
                        if !result.is_empty() {
                            result.push('\n');
                        }
                        result.push_str(&Self::spans_plain_text(header));
                    }
                    // Rows
                    for row in rows {
                        for cell in row {
                            if !result.is_empty() {
                                result.push('\n');
                            }
                            result.push_str(&Self::spans_plain_text(cell));
                        }
                    }
                }
                MarkdownElement::HorizontalRule => {
                    if !result.is_empty() {
                        result.push_str("\n---\n");
                    }
                }
            }
        }
        result
    }

    /// Collect blockquotes into (depth, blocks) entries; supports nesting and block elements.
    fn collect_blockquotes(
        &self,
        events: &[Event],
        start: usize,
        depth: u8,
        slug_counts: &mut HashMap<String, usize>,
    ) -> (Vec<(u8, QuoteBlocks)>, usize) {
        let mut i = start;
        let mut result: Vec<(u8, QuoteBlocks)> = Vec::new();
        let mut blocks: QuoteBlocks = Vec::new();
        let mut spans: Vec<InlineSpan> = Vec::new();

        let flush_inline = |blocks: &mut QuoteBlocks, spans: &mut Vec<InlineSpan>| {
            if !spans.is_empty() {
                blocks.push(MarkdownElement::Paragraph(std::mem::take(spans)));
            }
        };

        while i < events.len() {
            match &events[i] {
                Event::Start(Tag::Paragraph) => {
                    flush_inline(&mut blocks, &mut spans);
                    let (ps, next) =
                        self.parse_inline_spans_with_breaks(events, i + 1, Tag::Paragraph, true);
                    if !ps.is_empty() {
                        blocks.push(MarkdownElement::Paragraph(ps));
                    }
                    i = next;
                }
                Event::Start(Tag::Heading(level, _, _)) => {
                    flush_inline(&mut blocks, &mut spans);
                    let (ps, next) =
                        self.parse_inline_spans(events, i + 1, Tag::Heading(*level, None, vec![]));
                    let id = Self::make_header_id(&ps, slug_counts);
                    blocks.push(MarkdownElement::Header {
                        level: *level as u8,
                        spans: ps,
                        id,
                    });
                    i = next;
                }
                Event::Start(Tag::Table(alignments)) => {
                    flush_inline(&mut blocks, &mut spans);
                    let (headers, rows, next_idx) = self.parse_table(events, i + 1);
                    blocks.push(MarkdownElement::Table {
                        headers,
                        rows,
                        alignments: alignments.to_vec(),
                    });
                    i = next_idx;
                }
                Event::Start(Tag::List(first_item)) => {
                    flush_inline(&mut blocks, &mut spans);
                    let (items, next_idx) = self.parse_list(events, i + 1, slug_counts);
                    blocks.push(MarkdownElement::List {
                        ordered: first_item.is_some(),
                        items,
                    });
                    i = next_idx;
                }
                Event::Start(Tag::CodeBlock(_)) => {
                    flush_inline(&mut blocks, &mut spans);
                    let (code_text, language, next_idx) = self.parse_code_block(events, i);
                    let highlighted = self.highlight_code(language.as_deref(), &code_text);
                    blocks.push(MarkdownElement::CodeBlock {
                        language,
                        text: code_text,
                        highlighted,
                    });
                    i = next_idx;
                }
                Event::Rule => {
                    flush_inline(&mut blocks, &mut spans);
                    blocks.push(MarkdownElement::HorizontalRule);
                    i += 1;
                }
                Event::Start(Tag::BlockQuote) => {
                    flush_inline(&mut blocks, &mut spans);
                    if !blocks.is_empty() {
                        result.push((depth, std::mem::take(&mut blocks)));
                    }
                    let (nested, next) =
                        self.collect_blockquotes(events, i + 1, depth + 1, slug_counts);
                    result.extend(nested);
                    i = next;
                }
                Event::End(Tag::BlockQuote) => {
                    flush_inline(&mut blocks, &mut spans);
                    if !blocks.is_empty() {
                        result.push((depth, blocks));
                    }
                    return (result, i + 1);
                }
                Event::Text(t) => {
                    spans.push(InlineSpan::Text(Self::restore_pipe_sentinel(t)));
                    i += 1;
                }
                Event::Code(code) => {
                    spans.push(InlineSpan::Code(Self::restore_pipe_sentinel(code)));
                    i += 1;
                }
                Event::Start(Tag::Emphasis) => {
                    let (inner_text, next) =
                        self.collect_until_tag_end(events, i + 1, Tag::Emphasis, true);
                    spans.push(InlineSpan::Emphasis(inner_text));
                    i = next;
                }
                Event::Start(Tag::Strong) => {
                    let (inner_text, next) =
                        self.collect_until_tag_end(events, i + 1, Tag::Strong, true);
                    spans.push(InlineSpan::Strong(inner_text));
                    i = next;
                }
                Event::Start(Tag::Strikethrough) => {
                    let (inner_text, next) =
                        self.collect_until_tag_end(events, i + 1, Tag::Strikethrough, true);
                    spans.push(InlineSpan::Strikethrough(inner_text));
                    i = next;
                }
                Event::Start(Tag::Link(_, url, _)) => {
                    let url_str = url.to_string();
                    let (link_text, next) = self.collect_until_tag_end(
                        events,
                        i + 1,
                        Tag::Link(LinkType::Inline, "".into(), "".into()),
                        true,
                    );
                    spans.push(InlineSpan::Link {
                        text: link_text,
                        url: url_str,
                    });
                    i = next;
                }
                Event::Start(Tag::Image(_, url, title)) => {
                    let url_str = url.to_string();
                    let (alt_text, next) = self.collect_until_tag_end(
                        events,
                        i + 1,
                        Tag::Image(LinkType::Inline, "".into(), "".into()),
                        true,
                    );
                    spans.push(InlineSpan::Image {
                        src: url_str,
                        alt: alt_text,
                        title: if title.is_empty() {
                            None
                        } else {
                            Some(title.to_string())
                        },
                    });
                    i = next;
                }
                Event::SoftBreak | Event::HardBreak => {
                    spans.push(InlineSpan::Text("\n".to_string()));
                    i += 1;
                }
                Event::Html(html) => {
                    if Self::is_html_line_break(html) {
                        spans.push(InlineSpan::Text("\n".to_string()));
                    }
                    i += 1;
                }
                _ => {
                    i += 1;
                }
            }
        }

        flush_inline(&mut blocks, &mut spans);
        if !blocks.is_empty() {
            result.push((depth, blocks));
        }
        (result, i)
    }

    /// Parse inline spans until reaching the end tag
    fn parse_inline_spans_with_breaks(
        &self,
        events: &[Event],
        start: usize,
        end_tag: Tag,
        keep_breaks: bool,
    ) -> (Vec<InlineSpan>, usize) {
        let mut spans = Vec::new();
        let mut i = start;
        let mut text_buffer = String::new();

        while i < events.len() {
            match &events[i] {
                Event::End(tag)
                    if std::mem::discriminant(tag) == std::mem::discriminant(&end_tag) =>
                {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    return (spans, i + 1);
                }
                Event::Text(text) => {
                    let restored = Self::restore_pipe_sentinel(text);
                    text_buffer.push_str(&restored);
                    i += 1;
                }
                Event::SoftBreak | Event::HardBreak => {
                    if keep_breaks {
                        if !text_buffer.is_empty() {
                            spans.push(InlineSpan::Text(text_buffer.clone()));
                            text_buffer.clear();
                        }
                        spans.push(InlineSpan::Text("\n".to_string()));
                    } else {
                        text_buffer.push(' ');
                    }
                    i += 1;
                }
                Event::Code(code) => {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    spans.push(InlineSpan::Code(Self::restore_pipe_sentinel(code)));
                    i += 1;
                }
                Event::Html(html) => {
                    if Self::is_html_line_break(html) {
                        if keep_breaks {
                            if !text_buffer.is_empty() {
                                spans.push(InlineSpan::Text(text_buffer.clone()));
                                text_buffer.clear();
                            }
                            spans.push(InlineSpan::Text("\n".to_string()));
                        } else {
                            text_buffer.push(' ');
                        }
                    }
                    i += 1;
                }
                Event::Start(Tag::Strong) => {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    let (inner_text, next_i) =
                        self.collect_until_tag_end(events, i + 1, Tag::Strong, keep_breaks);
                    spans.push(InlineSpan::Strong(inner_text));
                    i = next_i;
                }
                Event::Start(Tag::Emphasis) => {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    let (inner_text, next_i) =
                        self.collect_until_tag_end(events, i + 1, Tag::Emphasis, keep_breaks);
                    spans.push(InlineSpan::Emphasis(inner_text));
                    i = next_i;
                }
                Event::Start(Tag::Strikethrough) => {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    let (inner_text, next_i) =
                        self.collect_until_tag_end(events, i + 1, Tag::Strikethrough, keep_breaks);
                    spans.push(InlineSpan::Strikethrough(inner_text));
                    i = next_i;
                }
                Event::Start(Tag::Link(_, url, _)) => {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    let url_str = url.to_string();
                    let (link_text, next_i) = self.collect_until_tag_end(
                        events,
                        i + 1,
                        Tag::Link(LinkType::Inline, "".into(), "".into()),
                        keep_breaks,
                    );
                    spans.push(InlineSpan::Link {
                        text: link_text,
                        url: url_str,
                    });
                    i = next_i;
                }
                Event::Start(Tag::Image(_, url, title)) => {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    let url_str = url.to_string();
                    let (alt_text, next_i) = self.collect_until_tag_end(
                        events,
                        i + 1,
                        Tag::Image(LinkType::Inline, "".into(), "".into()),
                        keep_breaks,
                    );
                    spans.push(InlineSpan::Image {
                        src: url_str,
                        alt: alt_text,
                        title: if title.is_empty() {
                            None
                        } else {
                            Some(title.to_string())
                        },
                    });
                    i = next_i;
                }
                _ => {
                    i += 1;
                }
            }
        }

        if !text_buffer.is_empty() {
            spans.push(InlineSpan::Text(text_buffer));
        }

        (spans, i)
    }

    /// Default inline parsing without preserving explicit line breaks
    fn parse_inline_spans(
        &self,
        events: &[Event],
        start: usize,
        end_tag: Tag,
    ) -> (Vec<InlineSpan>, usize) {
        self.parse_inline_spans_with_breaks(events, start, end_tag, false)
    }

    /// Collect text until a specific end tag
    fn collect_until_tag_end(
        &self,
        events: &[Event],
        start: usize,
        end_tag: Tag,
        keep_breaks: bool,
    ) -> (String, usize) {
        let mut text = String::new();
        let mut i = start;

        while i < events.len() {
            match &events[i] {
                Event::End(tag)
                    if std::mem::discriminant(tag) == std::mem::discriminant(&end_tag) =>
                {
                    return (text, i + 1);
                }
                Event::Text(t) => {
                    let restored = Self::restore_pipe_sentinel(t);
                    text.push_str(&restored);
                }
                Event::Code(code) => {
                    let restored = Self::restore_pipe_sentinel(code);
                    text.push_str(&restored);
                }
                Event::SoftBreak | Event::HardBreak => {
                    if keep_breaks {
                        text.push('\n');
                    } else {
                        text.push(' ');
                    }
                }
                Event::Html(html) => {
                    if Self::is_html_line_break(html) {
                        if keep_breaks {
                            text.push('\n');
                        } else {
                            text.push(' ');
                        }
                    }
                }
                _ => {}
            }
            i += 1;
        }

        (text, i)
    }

    /// Parse a code block
    fn parse_code_block(&self, events: &[Event], start: usize) -> (String, Option<String>, usize) {
        let mut language = None;
        let mut code_text = String::new();
        let mut i = start;

        // Extract language from the start tag
        if let Event::Start(Tag::CodeBlock(kind)) = &events[start] {
            if let pulldown_cmark::CodeBlockKind::Fenced(lang) = kind {
                if !lang.is_empty() {
                    language = Some(lang.to_string());
                }
            }
            i += 1;
        }

        while i < events.len() {
            match &events[i] {
                Event::End(Tag::CodeBlock(_)) => {
                    return (code_text, language, i + 1);
                }
                Event::Text(text) => {
                    code_text.push_str(text);
                }
                _ => {}
            }
            i += 1;
        }

        (code_text, language, i)
    }

    /// Parse a list item into block-level markdown elements.
    fn parse_list(
        &self,
        events: &[Event],
        start: usize,
        slug_counts: &mut HashMap<String, usize>,
    ) -> (Vec<ListItem>, usize) {
        let mut items: Vec<ListItem> = Vec::new();
        let mut i = start;

        while i < events.len() {
            match &events[i] {
                Event::End(Tag::List(_)) => return (items, i + 1),
                Event::Start(Tag::Item) => {
                    i += 1;
                    let mut blocks: Vec<MarkdownElement> = Vec::new();
                    let mut spans: Vec<InlineSpan> = Vec::new();
                    let flush_inline =
                        |blocks: &mut Vec<MarkdownElement>, spans: &mut Vec<InlineSpan>| {
                            if !spans.is_empty() {
                                blocks.push(MarkdownElement::Paragraph(std::mem::take(spans)));
                            }
                        };

                    loop {
                        if i >= events.len() {
                            break;
                        }
                        match &events[i] {
                            Event::End(Tag::Item) => {
                                i += 1;
                                break;
                            }
                            Event::Start(Tag::Paragraph) => {
                                flush_inline(&mut blocks, &mut spans);
                                let (ps, next) =
                                    self.parse_inline_spans(events, i + 1, Tag::Paragraph);
                                if !ps.is_empty() {
                                    blocks.push(MarkdownElement::Paragraph(ps));
                                }
                                i = next;
                            }
                            Event::Start(Tag::Heading(level, _, _)) => {
                                flush_inline(&mut blocks, &mut spans);
                                let (ps, next) = self.parse_inline_spans(
                                    events,
                                    i + 1,
                                    Tag::Heading(*level, None, vec![]),
                                );
                                let id = Self::make_header_id(&ps, slug_counts);
                                blocks.push(MarkdownElement::Header {
                                    level: *level as u8,
                                    spans: ps,
                                    id,
                                });
                                i = next;
                            }
                            Event::Start(Tag::Table(alignments)) => {
                                flush_inline(&mut blocks, &mut spans);
                                let (headers, rows, next_idx) = self.parse_table(events, i + 1);
                                blocks.push(MarkdownElement::Table {
                                    headers,
                                    rows,
                                    alignments: alignments.to_vec(),
                                });
                                i = next_idx;
                            }
                            Event::Start(Tag::CodeBlock(_)) => {
                                flush_inline(&mut blocks, &mut spans);
                                let (code_text, language, next_idx) =
                                    self.parse_code_block(events, i);
                                let highlighted =
                                    self.highlight_code(language.as_deref(), &code_text);
                                blocks.push(MarkdownElement::CodeBlock {
                                    language,
                                    text: code_text,
                                    highlighted,
                                });
                                i = next_idx;
                            }
                            Event::Start(Tag::BlockQuote) => {
                                flush_inline(&mut blocks, &mut spans);
                                let (quotes, next_idx) =
                                    self.collect_blockquotes(events, i + 1, 1, slug_counts);
                                for (depth, quote_blocks) in quotes {
                                    blocks.push(MarkdownElement::Quote {
                                        depth,
                                        blocks: quote_blocks,
                                    });
                                }
                                i = next_idx;
                            }
                            Event::Start(Tag::List(child_first)) => {
                                flush_inline(&mut blocks, &mut spans);
                                let (child_items, next) =
                                    self.parse_list(events, i + 1, slug_counts);
                                blocks.push(MarkdownElement::List {
                                    ordered: child_first.is_some(),
                                    items: child_items,
                                });
                                i = next;
                            }
                            Event::Rule => {
                                flush_inline(&mut blocks, &mut spans);
                                blocks.push(MarkdownElement::HorizontalRule);
                                i += 1;
                            }
                            Event::Code(code) => {
                                spans.push(InlineSpan::Code(code.to_string()));
                                i += 1;
                            }
                            Event::Start(Tag::Emphasis) => {
                                let (inner_text, next) =
                                    self.collect_until_tag_end(events, i + 1, Tag::Emphasis, false);
                                spans.push(InlineSpan::Emphasis(inner_text));
                                i = next;
                            }
                            Event::Start(Tag::Strong) => {
                                let (inner_text, next) =
                                    self.collect_until_tag_end(events, i + 1, Tag::Strong, false);
                                spans.push(InlineSpan::Strong(inner_text));
                                i = next;
                            }
                            Event::Start(Tag::Strikethrough) => {
                                let (inner_text, next) = self.collect_until_tag_end(
                                    events,
                                    i + 1,
                                    Tag::Strikethrough,
                                    false,
                                );
                                spans.push(InlineSpan::Strikethrough(inner_text));
                                i = next;
                            }
                            Event::Start(Tag::Link(_, url, _)) => {
                                let url_str = url.to_string();
                                let (link_text, next) = self.collect_until_tag_end(
                                    events,
                                    i + 1,
                                    Tag::Link(LinkType::Inline, "".into(), "".into()),
                                    false,
                                );
                                spans.push(InlineSpan::Link {
                                    text: link_text,
                                    url: url_str,
                                });
                                i = next;
                            }
                            Event::Start(Tag::Image(_, url, title)) => {
                                let url_str = url.to_string();
                                let (alt_text, next) = self.collect_until_tag_end(
                                    events,
                                    i + 1,
                                    Tag::Image(LinkType::Inline, "".into(), "".into()),
                                    false,
                                );
                                spans.push(InlineSpan::Image {
                                    src: url_str,
                                    alt: alt_text,
                                    title: if title.is_empty() {
                                        None
                                    } else {
                                        Some(title.to_string())
                                    },
                                });
                                i = next;
                            }
                            Event::Text(t) => {
                                spans.push(InlineSpan::Text(t.to_string()));
                                i += 1;
                            }
                            Event::SoftBreak | Event::HardBreak => {
                                spans.push(InlineSpan::Text(" ".into()));
                                i += 1;
                            }
                            _ => {
                                i += 1;
                            }
                        }
                    }

                    flush_inline(&mut blocks, &mut spans);
                    items.push(ListItem { blocks });
                }
                _ => i += 1,
            }
        }

        (items, i)
    }

    /// Parse a table with headers and rows
    fn parse_table(&self, events: &[Event], start: usize) -> TableParseResult {
        let mut headers: Vec<Vec<InlineSpan>> = Vec::new();
        let mut rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let mut i = start;

        while i < events.len() {
            match &events[i] {
                Event::Start(Tag::TableHead) => {
                    i += 1;
                    while i < events.len() {
                        match &events[i] {
                            Event::Start(Tag::TableCell) => {
                                let (spans, next_idx) = self.parse_inline_spans_with_breaks(
                                    events,
                                    i + 1,
                                    Tag::TableCell,
                                    true,
                                );
                                headers.push(spans);
                                i = next_idx;
                            }
                            Event::End(Tag::TableHead) => {
                                i += 1;
                                break;
                            }
                            _ => i += 1,
                        }
                    }
                }
                Event::Start(Tag::TableRow) => {
                    i += 1;
                    let mut row: Vec<Vec<InlineSpan>> = Vec::new();
                    while i < events.len() {
                        match &events[i] {
                            Event::Start(Tag::TableCell) => {
                                let (spans, next_idx) = self.parse_inline_spans_with_breaks(
                                    events,
                                    i + 1,
                                    Tag::TableCell,
                                    true,
                                );
                                row.push(spans);
                                i = next_idx;
                            }
                            Event::End(Tag::TableRow) => {
                                i += 1;
                                if !row.is_empty() {
                                    rows.push(row);
                                }
                                break;
                            }
                            _ => i += 1,
                        }
                    }
                }
                Event::End(Tag::Table(_)) => return (headers, rows, i + 1),
                _ => i += 1,
            }
        }
        (headers, rows, i)
    }

    fn render_element_body(&self, ui: &mut egui::Ui, element: &MarkdownElement) {
        match element {
            MarkdownElement::Paragraph(spans) => {
                self.render_inline_spans(ui, spans);
                ui.add_space(4.0);
            }
            MarkdownElement::Quote { depth, blocks } => {
                ui.add_space(4.0);
                let bar_width = 3.0;
                let bar_gap = 6.0;
                let left_pad = 10.0 + (*depth as f32) * (bar_width + bar_gap);
                // Substack-like styling: themed block with orange accent bars
                let tc = ThemeColors::current(ui.visuals().dark_mode);

                let resp = egui::Frame::none()
                    .fill(tc.blockquote_bg)
                    .stroke(Stroke::new(1.0, tc.blockquote_border))
                    .rounding(egui::Rounding::same(6.0))
                    .inner_margin(egui::Margin {
                        left: left_pad,
                        right: 10.0,
                        top: 8.0,
                        bottom: 8.0,
                    })
                    .show(ui, |ui| {
                        let prev_override = ui.style().visuals.override_text_color;
                        ui.style_mut().visuals.override_text_color = Some(tc.blockquote_text);
                        for block in blocks {
                            self.render_element_body(ui, block);
                        }
                        ui.style_mut().visuals.override_text_color = prev_override;
                    });

                // Draw vertical orange quote bars on the left of the frame
                let rect = resp.response.rect;
                let top = rect.top() + 6.0;
                let bottom = rect.bottom() - 6.0;
                let bar_color = tc.blockquote_bar;
                for d in 0..*depth {
                    let x = rect.left() + 6.0 + (d as f32) * (bar_width + bar_gap);
                    let bar_rect = egui::Rect::from_min_max(
                        egui::pos2(x, top),
                        egui::pos2(x + bar_width, bottom),
                    );
                    ui.painter().rect_filled(bar_rect, 2.0, bar_color);
                }

                ui.add_space(6.0);
            }
            MarkdownElement::Header { level, spans, id } => {
                let font_size = match level {
                    1 => self.font_sizes.h1,
                    2 => self.font_sizes.h2,
                    3 => self.font_sizes.h3,
                    4 => self.font_sizes.h4,
                    5 => self.font_sizes.h5,
                    6 => self.font_sizes.h6,
                    _ => self.font_sizes.body,
                };

                ui.add_space(8.0);
                let resp = ui.horizontal_wrapped(|ui| {
                    // Avoid artificial gaps between header fragments
                    ui.spacing_mut().item_spacing.x = 0.0;
                    for span in spans {
                        self.render_inline_span(ui, span, Some(font_size), Some(true));
                    }
                });
                // Record the header rect for in-document navigation
                self.header_rects
                    .borrow_mut()
                    .insert(id.clone(), resp.response.rect);
                ui.add_space(6.0);
            }
            MarkdownElement::CodeBlock {
                language,
                text,
                highlighted,
            } => {
                self.render_code_block(ui, language.as_deref(), text, highlighted.as_ref());
            }
            MarkdownElement::List { ordered, items } => {
                self.render_list(ui, *ordered, items);
            }
            MarkdownElement::HorizontalRule => {
                ui.add_space(8.0);
                ui.separator();
                ui.add_space(8.0);
            }
            MarkdownElement::Table {
                headers,
                rows,
                alignments,
            } => {
                self.render_table(ui, headers, rows, alignments);
            }
        }
    }

    /// Render parsed markdown elements to egui UI
    pub fn render_to_ui(&self, ui: &mut egui::Ui, elements: &[MarkdownElement]) {
        self.poll_image_results(ui.ctx());
        self.mermaid.begin_frame();
        // Clear header rects before rendering a new frame
        self.header_rects.borrow_mut().clear();
        // Reset per-frame link counter to ensure link IDs are stable across frames
        *self.link_counter.borrow_mut() = 0;
        // Reset per-frame table counter
        *self.table_counter.borrow_mut() = 0;
        // Reset per-frame element rects
        self.element_rects.borrow_mut().clear();
        for element in elements.iter() {
            // Wrap each element in a no-op frame to capture its rect
            let ir = egui::Frame::none().show(ui, |ui| {
                self.render_element_body(ui, element);
            });
            self.element_rects.borrow_mut().push(ir.response.rect);
        }
        // Add a little extra breathing room at the end so
        // the final line doesn't sit flush under the status bar.
        ui.add_space(16.0);
    }

    /// Render inline spans in a wrapped horizontal layout
    fn render_inline_spans(&self, ui: &mut egui::Ui, spans: &[InlineSpan]) {
        ui.horizontal_wrapped(|ui| {
            // Avoid adding UI spacing between inline fragments
            ui.spacing_mut().item_spacing.x = 0.0;
            for span in spans {
                self.render_inline_span(ui, span, None, None);
            }
        });
    }

    /// Fix Unicode characters that may not render properly in the default font
    fn fix_unicode_chars(&self, text: &str) -> String {
        let normalized: String = text.nfc().collect();
        if !normalized.chars().any(|c| {
            matches!(
                c,
                '\u{2011}'
                    | '\u{00AD}'
                    | '\u{2010}'
                    | '\u{2212}'
                    | '\u{2013}'
                    | '\u{2014}'
                    | '\u{00A0}'
                    | '\u{202F}'
                    | '\u{2190}'
                    | '\u{2192}'
                    | '\u{2191}'
                    | '\u{2193}'
            )
        }) {
            return normalized;
        }

        let mut out = String::with_capacity(normalized.len());
        for ch in normalized.chars() {
            match ch {
                '\u{2011}' | '\u{00AD}' | '\u{2010}' | '\u{2212}' | '\u{2013}' | '\u{2014}' => {
                    out.push('-')
                }
                '\u{00A0}' | '\u{202F}' => out.push(' '),
                '\u{2190}' => {
                    out.push('<');
                    out.push('-');
                }
                '\u{2192}' => {
                    out.push('-');
                    out.push('>');
                }
                '\u{2191}' => {
                    out.push('^');
                }
                '\u{2193}' => {
                    out.push('v');
                }
                _ => out.push(ch),
            }
        }
        out
    }

    #[cfg(test)]
    pub(crate) fn normalize_text_for_test(&self, text: &str) -> String {
        self.fix_unicode_chars(text)
    }

    /// Render a single inline span
    fn render_inline_span(
        &self,
        ui: &mut egui::Ui,
        span: &InlineSpan,
        font_size: Option<f32>,
        strong: Option<bool>,
    ) {
        let size = font_size.unwrap_or(self.font_sizes.body);
        let is_strong = strong.unwrap_or(false);

        match span {
            InlineSpan::Text(text) => {
                let fixed_text = self.fix_unicode_chars(text);
                let style = InlineStyle {
                    strong: is_strong,
                    ..Default::default()
                };
                self.render_text_with_emojis(ui, &fixed_text, size, style);
            }
            InlineSpan::Code(code) => {
                // Inline code: adapt style to theme (light vs dark)
                ui.spacing_mut().item_spacing.x = 0.0;
                let tc = ThemeColors::current(ui.visuals().dark_mode);
                let (bg, fg) = (tc.inline_code_bg, tc.inline_code_fg);
                let response = ui.add(
                    egui::Label::new(
                        RichText::new(code.clone())
                            .size(self.font_sizes.code)
                            .family(egui::FontFamily::Monospace)
                            .background_color(bg)
                            .color(fg),
                    )
                    .wrap(true),
                );

                // Add context menu for code
                #[cfg(test)]
                {
                    let _ = &response;
                    self.render_inline_code_context_menu(ui, code);
                }
                #[cfg(not(test))]
                response.context_menu(|ui| {
                    self.render_inline_code_context_menu(ui, code);
                });
            }
            InlineSpan::Strong(text) => {
                let fixed_text = self.fix_unicode_chars(text);
                let style = InlineStyle {
                    strong: true,
                    ..Default::default()
                };
                self.render_text_with_emojis(ui, &fixed_text, size, style);
            }
            InlineSpan::Emphasis(text) => {
                let fixed_text = self.fix_unicode_chars(text);
                let style = InlineStyle {
                    italics: true,
                    strong: is_strong,
                    ..Default::default()
                };
                self.render_text_with_emojis(ui, &fixed_text, size, style);
            }
            InlineSpan::Strikethrough(text) => {
                let fixed_text = self.fix_unicode_chars(text);
                let style = InlineStyle {
                    strike: true,
                    strong: is_strong,
                    ..Default::default()
                };
                self.render_text_with_emojis(ui, &fixed_text, size, style);
            }
            InlineSpan::Link { text, url } => {
                let fixed_text = self.fix_unicode_chars(text);
                let tc = ThemeColors::current(ui.visuals().dark_mode);
                let color = if Self::is_external_url(url) {
                    tc.link
                } else {
                    tc.link_internal
                };

                // Render link content as clickable widgets directly in the parent
                // layout. Important: do NOT nest horizontal_wrapped here - egui 0.27
                // has interaction bugs with nested horizontal_wrapped (#5666) and
                // ui.interact() on overlapping rects (#4147, #2947).
                let (clicked, hovered, last_r) =
                    self.render_clickable_link(ui, &fixed_text, size, is_strong, color);

                if render_action_triggered(hovered, "link_hover") {
                    ui.output_mut(|o| o.cursor_icon = egui::CursorIcon::PointingHand);
                }
                if render_action_triggered(clicked, "link_click") {
                    self.trigger_link(url);
                }

                // Attach context menu to the last rendered widget
                if let Some(_r) = last_r {
                    #[cfg(test)]
                    self.render_link_context_menu(ui, text, url);
                    #[cfg(not(test))]
                    _r.context_menu(|ui| {
                        self.render_link_context_menu(ui, text, url);
                    });
                }
            }
            InlineSpan::Image { src, alt, title } => {
                // Resolve path
                let resolved = self.resolve_image_path(src);
                let available_w = ui.available_width().max(1.0);
                // Try to get or load texture
                if let Some((tex, w, h)) = self.get_or_load_image_texture(ui, &resolved) {
                    let (tw, th) = (w as f32, h as f32);
                    // Scale logic:
                    // - Start at UI scale (e.g., 1.0 for 100%, 1.1 for 110%).
                    // - Only downscale further if it would exceed available width.
                    let base_scale = self.ui_scale();
                    let scaled_w = tw * base_scale;
                    let scale = if scaled_w > available_w {
                        (available_w / tw).clamp(0.01, 4.0)
                    } else {
                        base_scale
                    };
                    let size = egui::vec2((tw * scale).round(), (th * scale).round());
                    let image = egui::Image::new(&tex).fit_to_exact_size(size);
                    let resp = ui.add(image);
                    if let Some(t) = title {
                        if !t.is_empty() {
                            if render_action_triggered(resp.hovered(), "image_hover") {
                                resp.on_hover_text(t.clone());
                            }
                            // Subtle caption below image
                            ui.add_space(2.0);
                            let tc = ThemeColors::current(ui.visuals().dark_mode);
                            ui.label(
                                RichText::new(t.clone())
                                    .size(self.font_sizes.body - 2.0)
                                    .color(tc.code_label),
                            );
                        }
                    }
                    ui.add_space(6.0);
                } else {
                    // Placeholder with alt and error info
                    let tc = ThemeColors::current(ui.visuals().dark_mode);
                    egui::Frame::none()
                        .fill(tc.code_bg)
                        .stroke(Stroke::new(1.0, tc.code_border))
                        .inner_margin(8.0)
                        .show(ui, |ui| {
                            let pending = self.image_pending.borrow().contains(&resolved);
                            let msg = if pending {
                                "Loading image..."
                            } else if src.starts_with("http://") || src.starts_with("https://") {
                                "Remote images are disabled"
                            } else {
                                "Image not found or unsupported"
                            };
                            let label = if alt.is_empty() {
                                src.as_str()
                            } else {
                                alt.as_str()
                            };
                            ui.label(
                                RichText::new(format!("{}\n{}", label, msg))
                                    .size(self.font_sizes.body),
                            );
                        });
                    ui.add_space(6.0);
                }
            }
        }
    }

    fn extract_fragment(url: &str) -> Option<String> {
        if let Some(stripped) = url.strip_prefix('#') {
            // Already a fragment within current document
            return Some(stripped.to_ascii_lowercase());
        }
        None
    }

    fn is_external_url(url: &str) -> bool {
        let lower = url.to_ascii_lowercase();
        lower.starts_with("http://")
            || lower.starts_with("https://")
            || lower.starts_with("mailto:")
            || lower.starts_with("www.")
    }

    /// Render a clickable link's content directly in the current layout.
    /// Returns (clicked, hovered, last_response) so the caller can handle
    /// link activation, cursor changes, and context menus.
    ///
    /// Each sub-widget (Label for text, Image for emoji) gets
    /// `Sense::click()` applied during creation - this is the egui 0.27
    /// approved pattern that avoids the broken `ui.interact()` overlay.
    fn render_clickable_link(
        &self,
        ui: &mut egui::Ui,
        text: &str,
        size: f32,
        is_strong: bool,
        color: Color32,
    ) -> (bool, bool, Option<egui::Response>) {
        let has_emoji = text
            .graphemes(true)
            .any(|g| self.emoji_key_for_grapheme(g).is_some());

        if !has_emoji {
            // Simple text link: single Label with click sense
            let expanded = Self::expand_shortcodes(text);
            let expanded = Self::expand_superscripts(&expanded);
            let mut rich = RichText::new(&expanded).size(size).color(color);
            if is_strong {
                rich = rich.strong();
            }
            let r = ui.add(egui::Label::new(rich).sense(egui::Sense::click()));
            let clicked = r.clicked();
            let hovered = r.hovered();
            return (clicked, hovered, Some(r));
        }

        // Emoji link: render each piece with click sense
        let mut any_clicked = false;
        let mut any_hovered = false;
        let mut responses: Vec<egui::Response> = Vec::new();
        let mut buffer = String::new();

        for g in text.graphemes(true) {
            if let Some(key) = self.emoji_key_for_grapheme(g) {
                // Flush text buffer as a clickable label
                if !buffer.is_empty() {
                    let expanded = Self::expand_shortcodes(&buffer);
                    let expanded = Self::expand_superscripts(&expanded);
                    let mut rich = RichText::new(&expanded).size(size).color(color);
                    if is_strong {
                        rich = rich.strong();
                    }
                    let r = ui.add(egui::Label::new(rich).sense(egui::Sense::click()));
                    any_clicked |= r.clicked();
                    any_hovered |= r.hovered();
                    responses.push(r);
                    buffer.clear();
                }
                // Emoji image with click sense
                let handle = self.get_or_make_emoji_texture(ui, &key);
                let sz = size * 1.2;
                let r = ui.add(
                    egui::Image::new(&handle)
                        .max_width(sz)
                        .max_height(sz)
                        .sense(egui::Sense::click()),
                );
                any_clicked |= r.clicked();
                any_hovered |= r.hovered();
                responses.push(r);
            } else {
                buffer.push_str(g);
            }
        }

        // Flush remaining text
        if !buffer.is_empty() {
            let expanded = Self::expand_shortcodes(&buffer);
            let expanded = Self::expand_superscripts(&expanded);
            let mut rich = RichText::new(&expanded).size(size).color(color);
            if is_strong {
                rich = rich.strong();
            }
            let r = ui.add(egui::Label::new(rich).sense(egui::Sense::click()));
            any_clicked |= r.clicked();
            any_hovered |= r.hovered();
            responses.push(r);
        }

        (any_clicked, any_hovered, responses.pop())
    }

    fn render_text_with_emojis(
        &self,
        ui: &mut egui::Ui,
        text: &str,
        size: f32,
        style: InlineStyle,
    ) {
        let mut buffer = String::new();
        for g in text.graphemes(true) {
            if let Some(key) = self.emoji_key_for_grapheme(g) {
                // Flush buffered text first (with highlighting if active)
                if !buffer.is_empty() {
                    self.render_text_segment_with_optional_highlight(ui, &buffer, size, style);
                    buffer.clear();
                }
                let handle = self.get_or_make_emoji_texture(ui, &key);
                let sz = size * 1.2;
                ui.add(egui::Image::new(&handle).max_width(sz).max_height(sz));
            } else {
                buffer.push_str(g);
            }
        }
        if !buffer.is_empty() {
            self.render_text_segment_with_optional_highlight(ui, &buffer, size, style);
        }
    }

    fn render_text_segment_with_optional_highlight(
        &self,
        ui: &mut egui::Ui,
        segment: &str,
        size: f32,
        style: InlineStyle,
    ) {
        // First expand emoji shortcodes, then superscript ^...^ notation
        let expanded = Self::expand_shortcodes(segment);
        let expanded = Self::expand_superscripts(&expanded);
        if let Some(h) = self
            .highlight_phrase
            .borrow()
            .as_ref()
            .filter(|s| !s.is_empty())
        {
            for (range, highlighted) in self.highlight_segments(&expanded, Some(h)) {
                let slice = &expanded[range];
                if highlighted {
                    self.render_highlighted_segment(ui, slice, size, style);
                } else {
                    self.render_plain_segment(ui, slice, size, style);
                }
            }
            return;
        }
        self.render_plain_segment(ui, &expanded, size, style);
    }

    /// Renders a text segment with optional search highlight background.
    /// When `highlighted` is true, applies the selection background color and
    /// uses the selection stroke color as a fallback text color.
    /// When `highlighted` is false, renders plain text with an optional
    /// override_text_color fallback for strong text.
    fn render_text_segment(
        &self,
        ui: &mut egui::Ui,
        text: &str,
        size: f32,
        style: InlineStyle,
        highlighted: bool,
    ) {
        if text.is_empty() {
            return;
        }
        let mut rich = RichText::new(text).size(size);

        // Apply formatting styles
        if style.strong {
            rich = rich.strong();
        }
        if style.italics {
            rich = rich.italics();
        }
        if style.strike {
            rich = rich.strikethrough();
        }

        // Apply color and optional highlight background
        if highlighted {
            let visuals = ui.visuals();
            rich = rich.background_color(visuals.selection.bg_fill);
            rich = rich.color(style.color.unwrap_or(visuals.selection.stroke.color));
        } else {
            let mut text_color = style.color;
            if text_color.is_none() && style.strong {
                if let Some(override_color) = ui.visuals().override_text_color {
                    text_color = Some(override_color);
                }
            }
            if let Some(color) = text_color {
                rich = rich.color(color);
            }
        }

        let response = ui.add(egui::Label::new(rich).wrap(true));

        // Context menu for text segment
        // Note: Due to egui limitations, selection is cleared on right-click
        // As a workaround, we provide "Copy Text" for the segment
        #[cfg(test)]
        {
            let _ = &response;
            self.render_text_context_menu(ui, text);
        }
        #[cfg(not(test))]
        response.context_menu(|ui| {
            self.render_text_context_menu(ui, text);
        });
    }

    /// Convenience wrapper: renders a plain (non-highlighted) text segment.
    fn render_plain_segment(&self, ui: &mut egui::Ui, text: &str, size: f32, style: InlineStyle) {
        self.render_text_segment(ui, text, size, style, false);
    }

    /// Convenience wrapper: renders a search-highlighted text segment.
    fn render_highlighted_segment(
        &self,
        ui: &mut egui::Ui,
        text: &str,
        size: f32,
        style: InlineStyle,
    ) {
        self.render_text_segment(ui, text, size, style, true);
    }

    fn render_text_context_menu(&self, ui: &mut egui::Ui, text: &str) {
        if render_action_triggered(ui.button("Copy Text").clicked(), "copy_text") {
            self.copy_text_and_close(ui, text);
        }
        ui.label("Tip: Use Ctrl+C to copy selected text");
    }

    fn render_inline_code_context_menu(&self, ui: &mut egui::Ui, code: &str) {
        if render_action_triggered(ui.button("Copy Code").clicked(), "copy_inline_code") {
            self.copy_text_and_close(ui, code);
        }
    }

    fn render_code_block_context_menu(
        &self,
        ui: &mut egui::Ui,
        code: &str,
        language: Option<&str>,
    ) {
        if render_action_triggered(ui.button("Copy Code").clicked(), "copy_code_block") {
            self.copy_text_and_close(ui, code);
        }
        if let Some(lang) = language {
            if render_action_triggered(
                ui.button(format!("Copy as {}", lang)).clicked(),
                "copy_code_block_lang",
            ) {
                self.copy_text_and_close(ui, &format!("```{}\n{}\n```", lang, code));
            }
        }
    }

    fn render_link_context_menu(&self, ui: &mut egui::Ui, text: &str, url: &str) {
        if render_action_triggered(ui.button("Open Link").clicked(), "open_link") {
            self.trigger_link(url);
            ui.close_menu();
        }
        ui.separator();
        if render_action_triggered(ui.button("Copy Link Text").clicked(), "copy_link_text") {
            self.copy_text_and_close(ui, text);
        }
        if render_action_triggered(ui.button("Copy Link URL").clicked(), "copy_link_url") {
            self.copy_text_and_close(ui, url);
        }
    }

    fn render_cell_context_menu(&self, ui: &mut egui::Ui, text: &str) {
        if render_action_triggered(ui.button("Copy Cell Text").clicked(), "copy_cell_text") {
            self.copy_text_and_close(ui, text);
        }
    }

    fn copy_text_and_close(&self, ui: &mut egui::Ui, text: &str) {
        ui.ctx().copy_text(text.to_string());
        ui.close_menu();
    }

    /// Map a grapheme cluster to an emoji image key if available.
    fn emoji_key_for_grapheme(&self, g: &str) -> Option<String> {
        if crate::emoji_catalog::image_bytes_for(g).is_some() {
            return Some(g.to_string());
        }
        let stripped: String = g.chars().filter(|&c| c != '\u{FE0F}').collect();
        if stripped != g && crate::emoji_catalog::image_bytes_for(&stripped).is_some() {
            return Some(stripped);
        }
        // If the grapheme contains VS16, only treat it as an emoji if we have a
        // procedural fallback. Otherwise let the system font render it naturally
        // instead of producing a grey circle placeholder.
        if g.chars().any(|c| c == '\u{FE0F}') && emoji_assets::has_fallback(&stripped) {
            return Some(stripped);
        }
        None
    }

    fn get_or_make_emoji_texture(&self, ui: &mut egui::Ui, emoji: &str) -> egui::TextureHandle {
        let key = emoji.to_string();
        if let Some(tex) = self.emoji_textures.borrow().get(&key) {
            return tex;
        }

        let img = if let Some(bytes) = emoji_catalog::image_bytes_for(emoji) {
            let bytes = {
                #[cfg(test)]
                {
                    if FORCE_EMOJI_DECODE_ERROR.swap(false, Ordering::Relaxed) {
                        b"mdmdview"
                    } else {
                        bytes
                    }
                }
                #[cfg(not(test))]
                {
                    bytes
                }
            };
            match image::load_from_memory(bytes) {
                Ok(dyn_img) => {
                    let rgba = dyn_img.to_rgba8();
                    let (w, h) = rgba.dimensions();
                    let pixels = rgba.into_vec();
                    egui::ColorImage::from_rgba_unmultiplied([w as usize, h as usize], &pixels)
                }
                Err(_) => match emoji_assets::make_image(emoji, 64) {
                    Some(img) => img,
                    None => self.generate_emoji_image(emoji, 64),
                },
            }
        } else {
            match emoji_assets::make_image(emoji, 64) {
                Some(img) => img,
                None => self.generate_emoji_image(emoji, 64),
            }
        };
        let handle = ui.ctx().load_texture(
            format!("emoji:{}", emoji),
            img,
            egui::TextureOptions::LINEAR,
        );
        self.emoji_textures.borrow_mut().insert(key, handle.clone());
        handle
    }

    #[cfg(test)]
    // Rough width measurement for inline spans without wrapping
    fn measure_inline_spans(&self, ui: &egui::Ui, spans: &[InlineSpan]) -> f32 {
        let mut max_line_width = 0.0f32;
        let mut current_line_width = 0.0f32;
        ui.fonts(|fonts| {
            let body = egui::FontId::proportional(self.font_sizes.body);
            let mono = egui::FontId::monospace(self.font_sizes.code);
            for span in spans {
                match span {
                    InlineSpan::Text(t)
                    | InlineSpan::Strong(t)
                    | InlineSpan::Emphasis(t)
                    | InlineSpan::Strikethrough(t)
                    | InlineSpan::Link { text: t, .. } => {
                        // Normalize to match render path before measuring.
                        let mut normalized = self.fix_unicode_chars(t);
                        normalized = Self::expand_shortcodes(&normalized);
                        normalized = Self::expand_superscripts(&normalized);
                        for (line_idx, line) in normalized.split('\n').enumerate() {
                            if line_idx > 0 {
                                max_line_width = max_line_width.max(current_line_width);
                                current_line_width = 0.0;
                            }
                            if line.is_empty() {
                                continue;
                            }
                            let galley = fonts.layout_no_wrap(
                                line.to_string(),
                                body.clone(),
                                Color32::WHITE,
                            );
                            let mut line_width = galley.size().x;
                            // Add a small extra width for emoji images (drawn larger than text).
                            let emoji_extra =
                                line.chars().filter(|c| Self::is_known_emoji(*c)).count() as f32
                                    * (self.font_sizes.body * 0.2);
                            line_width += emoji_extra;
                            if current_line_width > 0.0 {
                                current_line_width += 4.0;
                            }
                            current_line_width += line_width;
                        }
                    }
                    InlineSpan::Code(code) => {
                        for (line_idx, line) in code.split('\n').enumerate() {
                            if line_idx > 0 {
                                max_line_width = max_line_width.max(current_line_width);
                                current_line_width = 0.0;
                            }
                            if line.is_empty() {
                                continue;
                            }
                            let galley = fonts.layout_no_wrap(
                                line.to_string(),
                                mono.clone(),
                                Color32::WHITE,
                            );
                            let line_width = galley.size().x + 6.0; // padding for code background
                            if current_line_width > 0.0 {
                                current_line_width += 4.0;
                            }
                            current_line_width += line_width;
                        }
                    }
                    InlineSpan::Image { src, .. } => {
                        // Use cached texture size if available; otherwise a conservative thumbnail.
                        let cap = (ui.available_width() * 0.6).max(48.0);
                        let cached = self
                            .image_textures
                            .borrow_mut()
                            .get(src)
                            .map(|entry| entry.size[0] as f32 * self.ui_scale());
                        let approx = cached.unwrap_or(self.font_sizes.body * 12.0);
                        let line_width = approx.min(cap);
                        if current_line_width > 0.0 {
                            current_line_width += 4.0;
                        }
                        current_line_width += line_width;
                    }
                }
            }
        });
        max_line_width.max(current_line_width)
    }

    #[cfg(test)]
    fn is_known_emoji(c: char) -> bool {
        matches!(
            c,
            // Original 18
            '\u{1f389}' | '\u{2705}' | '\u{1f680}' | '\u{1f642}' | '\u{1f600}'
                | '\u{1f609}' | '\u{2b50}' | '\u{1f525}' | '\u{1f44d}' | '\u{1f44e}'
                | '\u{1f4a1}' | '\u{2753}' | '\u{2757}' | '\u{1f4dd}' | '\u{1f9e0}'
                | '\u{1f9ea}' | '\u{1f4e6}' | '\u{1f527}'
                // Status/Indicators
                | '\u{26a0}' | '\u{274c}' | '\u{2728}' | '\u{1f4af}' | '\u{23f3}'
                | '\u{1f504}' | '\u{2714}' | '\u{274e}' | '\u{1f6ab}'
                // Objects/Tools
                | '\u{1f517}' | '\u{1f4cb}' | '\u{1f4c1}' | '\u{1f4c2}' | '\u{1f512}'
                | '\u{1f513}' | '\u{2699}' | '\u{1f50d}' | '\u{1f4ca}' | '\u{1f4c8}'
                | '\u{1f4c9}' | '\u{1f4cc}' | '\u{1f528}' | '\u{270f}'
                // Arrows
                | '\u{27a1}' | '\u{2b06}' | '\u{2b07}' | '\u{2b05}'
                // Communication
                | '\u{1f4ac}' | '\u{1f4e2}' | '\u{1f4e7}'
                // Nature/Symbols
                | '\u{2764}' | '\u{1f48e}' | '\u{1f31f}' | '\u{1f3af}' | '\u{1f3a8}'
                | '\u{1f6e1}' | '\u{1f30a}' | '\u{26a1}'
                // People/Faces
                | '\u{1f440}' | '\u{1f914}' | '\u{1f44b}' | '\u{1f64f}' | '\u{1f4aa}'
                // Dev/Tech
                | '\u{1f41b}' | '\u{1f6a7}' | '\u{1f3d7}' | '\u{1f4bb}' | '\u{1f5a5}'
                | '\u{2328}'
                // Misc
                | '\u{1f3c6}' | '\u{1f3aa}' | '\u{1f4d6}' | '\u{1f4da}' | '\u{1f514}'
                | '\u{1f515}' | '\u{1f5c2}' | '\u{2139}' | '\u{1f534}' | '\u{1f7e2}'
                | '\u{1f7e1}' | '\u{1f535}' | '\u{2611}'
        )
    }

    fn expand_shortcodes(s: &str) -> String {
        use crate::emoji_catalog::shortcode_map;
        if !s.contains(':') {
            return s.to_string();
        }
        let mut out = String::new();
        let map = shortcode_map();
        let mut i = 0;
        let bytes = s.as_bytes();
        while i < s.len() {
            if bytes[i] == b':' {
                if let Some(end_rel) = bytes[i + 1..].iter().position(|&b| b == b':') {
                    let end = i + 1 + end_rel;
                    let code = &s[i..=end];
                    if let Some(&emoji) = map.get(code) {
                        out.push_str(emoji);
                        i = end + 1;
                        continue;
                    }
                }
            }
            // advance by one UTF-8 character
            // Safety: i < s.len() is guaranteed by the while condition, and s[i..] is
            // guaranteed to be valid UTF-8 since s is a &str, so chars().next() will
            // always return Some.
            let Some(ch) = s[i..].chars().next() else {
                break;
            };
            out.push(ch);
            i += ch.len_utf8();
        }
        out
    }

    // Expand ^...^ segments into Unicode superscript characters when available.
    // Example: "5^th^" -> "5??"
    fn expand_superscripts(s: &str) -> String {
        if !s.contains('^') {
            return s.to_string();
        }
        let mut out = String::new();
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '^' {
                // Look ahead to see if there's a matching closing caret within reasonable bounds
                let mut buf = String::new();
                let mut found_closing = false;
                let mut temp_chars = chars.clone();
                let mut char_count = 0;

                while let Some(&nc) = temp_chars.peek() {
                    if nc == '^' {
                        found_closing = true;
                        break;
                    }
                    if !nc.is_alphanumeric() && !matches!(nc, '+' | '-' | '=' | '(' | ')') {
                        break;
                    }
                    if char_count >= 10 {
                        break;
                    }
                    temp_chars.next();
                    char_count += 1;
                }

                if found_closing && char_count > 0 {
                    for nc in chars.by_ref().take(char_count) {
                        buf.push(nc);
                    }
                    // Skip the closing caret
                    chars.next();
                    out.push_str(&Self::to_superscript(&buf));
                } else {
                    out.push('^');
                }
            } else {
                out.push(c);
            }
        }
        out
    }

    fn to_superscript(s: &str) -> String {
        s.chars()
            .map(|c| match c {
                '0' => '\u{2070}',
                '1' => '\u{00b9}',
                '2' => '\u{00b2}',
                '3' => '\u{00b3}',
                '4' => '\u{2074}',
                '5' => '\u{2075}',
                '6' => '\u{2076}',
                '7' => '\u{2077}',
                '8' => '\u{2078}',
                '9' => '\u{2079}',
                '+' => '\u{207a}',
                '-' => '\u{207b}',
                '=' => '\u{207c}',
                '(' => '\u{207d}',
                ')' => '\u{207e}',
                'a' | 'A' => '\u{1d43}',
                'b' | 'B' => '\u{1d47}',
                'c' | 'C' => '\u{1d9c}',
                'd' | 'D' => '\u{1d48}',
                'e' | 'E' => '\u{1d49}',
                'f' | 'F' => '\u{1da0}',
                'g' | 'G' => '\u{1d4d}',
                'h' | 'H' => '\u{02b0}',
                'i' | 'I' => '\u{2071}',
                'j' | 'J' => '\u{02b2}',
                'k' | 'K' => '\u{1d4f}',
                'l' | 'L' => '\u{02e1}',
                'm' | 'M' => '\u{1d50}',
                'n' | 'N' => '\u{207f}',
                'o' | 'O' => '\u{1d52}',
                'p' | 'P' => '\u{1d56}',
                'q' | 'Q' => '\u{1d56}',
                'r' | 'R' => '\u{02b3}',
                's' | 'S' => '\u{02e2}',
                't' | 'T' => '\u{1d57}',
                'u' | 'U' => '\u{1d58}',
                'v' | 'V' => '\u{1d5b}',
                'w' | 'W' => '\u{02b7}',
                'x' | 'X' => '\u{02e3}',
                'y' | 'Y' => '\u{02b8}',
                'z' | 'Z' => '\u{1dbb}',
                other => other,
            })
            .collect()
    }

    fn generate_emoji_image(&self, emoji: &str, size: usize) -> egui::ColorImage {
        // Simple procedural placeholder icons to keep binary small and avoid external assets
        use egui::Color32 as C;
        let mut img = egui::ColorImage::new([size, size], C::TRANSPARENT);
        let cx = (size as i32) / 2;
        let cy = cx;
        let r = (size as i32) / 2 - 2;

        let (base, accent) = match emoji {
            // Original 18
            "\u{1f389}" => (C::from_rgb(255, 215, 0), C::from_rgb(255, 80, 80)),
            "\u{2705}" => (C::from_rgb(30, 150, 30), C::WHITE),
            "\u{1f680}" => (C::from_rgb(70, 70, 200), C::from_rgb(255, 100, 100)),
            "\u{1f642}" | "\u{1f600}" | "\u{1f609}" => {
                (C::from_rgb(255, 205, 64), C::from_rgb(90, 60, 10))
            }
            "\u{2b50}" => (C::from_rgb(255, 215, 0), C::WHITE),
            "\u{1f525}" => (C::from_rgb(255, 120, 50), C::from_rgb(255, 200, 120)),
            "\u{1f44d}" | "\u{1f44e}" => (C::from_rgb(200, 200, 200), C::from_rgb(80, 80, 80)),
            "\u{1f4a1}" => (C::from_rgb(255, 240, 120), C::from_rgb(255, 255, 255)),
            "\u{2753}" | "\u{2757}" => (C::from_rgb(70, 70, 200), C::from_rgb(255, 255, 255)),
            "\u{1f4dd}" => (C::from_rgb(240, 240, 200), C::from_rgb(140, 140, 140)),
            "\u{1f9e0}" => (C::from_rgb(220, 160, 200), C::from_rgb(255, 255, 255)),
            "\u{1f9ea}" => (C::from_rgb(180, 220, 255), C::from_rgb(120, 160, 240)),
            "\u{1f4e6}" => (C::from_rgb(205, 170, 125), C::from_rgb(150, 110, 70)),
            "\u{1f527}" => (C::from_rgb(160, 170, 180), C::from_rgb(220, 220, 220)),
            // Status/Indicators
            "\u{26a0}" => (C::from_rgb(255, 200, 0), C::from_rgb(60, 60, 60)),
            "\u{274c}" => (C::from_rgb(220, 50, 50), C::WHITE),
            "\u{2728}" => (C::from_rgb(255, 220, 100), C::from_rgb(255, 255, 200)),
            "\u{1f4af}" => (C::from_rgb(220, 50, 50), C::WHITE),
            "\u{23f3}" => (C::from_rgb(200, 180, 140), C::from_rgb(255, 220, 160)),
            "\u{1f504}" => (C::from_rgb(80, 180, 80), C::from_rgb(200, 255, 200)),
            "\u{2714}" => (C::from_rgb(30, 150, 30), C::WHITE),
            "\u{274e}" => (C::from_rgb(220, 50, 50), C::from_rgb(80, 80, 80)),
            "\u{1f6ab}" => (C::from_rgb(220, 50, 50), C::WHITE),
            // Objects/Tools
            "\u{1f517}" => (C::from_rgb(100, 140, 200), C::from_rgb(180, 200, 240)),
            "\u{1f4cb}" => (C::from_rgb(200, 180, 140), C::from_rgb(255, 255, 255)),
            "\u{1f4c1}" => (C::from_rgb(255, 200, 80), C::from_rgb(200, 160, 60)),
            "\u{1f4c2}" => (C::from_rgb(255, 200, 80), C::from_rgb(240, 240, 200)),
            "\u{1f512}" => (C::from_rgb(255, 200, 80), C::from_rgb(160, 160, 160)),
            "\u{1f513}" => (C::from_rgb(160, 160, 160), C::from_rgb(255, 200, 80)),
            "\u{2699}" => (C::from_rgb(140, 140, 160), C::from_rgb(200, 200, 220)),
            "\u{1f50d}" => (C::from_rgb(100, 140, 200), C::from_rgb(220, 220, 240)),
            "\u{1f4ca}" => (C::from_rgb(80, 140, 200), C::from_rgb(200, 220, 255)),
            "\u{1f4c8}" => (C::from_rgb(50, 180, 80), C::from_rgb(200, 255, 200)),
            "\u{1f4c9}" => (C::from_rgb(220, 60, 60), C::from_rgb(255, 200, 200)),
            "\u{1f4cc}" => (C::from_rgb(220, 60, 60), C::from_rgb(180, 180, 180)),
            "\u{1f528}" => (C::from_rgb(160, 140, 120), C::from_rgb(200, 200, 200)),
            "\u{270f}" => (C::from_rgb(255, 220, 60), C::from_rgb(60, 60, 60)),
            // Arrows
            "\u{27a1}" | "\u{2b06}" | "\u{2b07}" | "\u{2b05}" => {
                (C::from_rgb(60, 120, 200), C::WHITE)
            }
            // Communication
            "\u{1f4ac}" => (C::from_rgb(200, 200, 200), C::from_rgb(120, 120, 120)),
            "\u{1f4e2}" => (C::from_rgb(100, 140, 200), C::from_rgb(220, 220, 240)),
            "\u{1f4e7}" => (C::from_rgb(100, 140, 200), C::WHITE),
            // Nature/Symbols
            "\u{2764}" => (C::from_rgb(220, 20, 60), C::from_rgb(255, 120, 120)),
            "\u{1f48e}" => (C::from_rgb(80, 160, 255), C::from_rgb(200, 230, 255)),
            "\u{1f31f}" => (C::from_rgb(255, 215, 0), C::from_rgb(255, 255, 200)),
            "\u{1f3af}" => (C::from_rgb(220, 50, 50), C::WHITE),
            "\u{1f3a8}" => (C::from_rgb(255, 200, 80), C::from_rgb(80, 160, 255)),
            "\u{1f6e1}" => (C::from_rgb(100, 140, 200), C::from_rgb(200, 200, 200)),
            "\u{1f30a}" => (C::from_rgb(60, 140, 220), C::from_rgb(180, 220, 255)),
            "\u{26a1}" => (C::from_rgb(255, 220, 0), C::from_rgb(255, 255, 200)),
            // People/Faces
            "\u{1f440}" => (C::from_rgb(255, 255, 255), C::from_rgb(40, 40, 40)),
            "\u{1f914}" => (C::from_rgb(255, 205, 64), C::from_rgb(90, 60, 10)),
            "\u{1f44b}" | "\u{1f64f}" | "\u{1f4aa}" => {
                (C::from_rgb(255, 205, 120), C::from_rgb(200, 160, 80))
            }
            // Dev/Tech
            "\u{1f41b}" => (C::from_rgb(80, 180, 80), C::from_rgb(40, 80, 40)),
            "\u{1f6a7}" => (C::from_rgb(255, 160, 0), C::from_rgb(255, 255, 255)),
            "\u{1f3d7}" => (C::from_rgb(255, 200, 80), C::from_rgb(160, 160, 160)),
            "\u{1f4bb}" => (C::from_rgb(160, 170, 180), C::from_rgb(100, 180, 255)),
            "\u{1f5a5}" | "\u{2328}" => (C::from_rgb(60, 60, 80), C::from_rgb(200, 200, 200)),
            // Misc
            "\u{1f3c6}" => (C::from_rgb(255, 200, 0), C::from_rgb(255, 255, 200)),
            "\u{1f3aa}" => (C::from_rgb(220, 50, 80), C::from_rgb(255, 200, 80)),
            "\u{1f4d6}" => (C::from_rgb(240, 240, 200), C::from_rgb(140, 140, 140)),
            "\u{1f4da}" => (C::from_rgb(100, 140, 200), C::from_rgb(255, 200, 80)),
            "\u{1f514}" => (C::from_rgb(255, 200, 0), C::from_rgb(200, 160, 0)),
            "\u{1f515}" => (C::from_rgb(160, 160, 160), C::from_rgb(120, 120, 120)),
            "\u{1f5c2}" => (C::from_rgb(100, 140, 200), C::from_rgb(200, 200, 220)),
            "\u{2139}" | "\u{2611}" => (C::from_rgb(60, 120, 200), C::WHITE),
            "\u{1f534}" => (C::from_rgb(220, 50, 50), C::from_rgb(255, 120, 120)),
            "\u{1f7e2}" => (C::from_rgb(50, 180, 50), C::from_rgb(120, 255, 120)),
            "\u{1f7e1}" => (C::from_rgb(255, 200, 0), C::from_rgb(255, 255, 120)),
            "\u{1f535}" => (C::from_rgb(60, 120, 200), C::from_rgb(120, 180, 255)),
            _ => (C::from_rgb(180, 180, 180), C::WHITE),
        };

        // draw filled circle
        for y in 0..size as i32 {
            for x in 0..size as i32 {
                let dx = x - cx;
                let dy = y - cy;
                if dx * dx + dy * dy <= r * r {
                    img[(x as usize, y as usize)] = base;
                }
            }
        }

        // add a simple accent (diagonal highlight)
        for t in 0..size {
            let x = t as i32;
            let y = (t as i32) / 2;
            let xx = (x / 2 + 6).clamp(0, size as i32 - 1) as usize;
            let yy = (y + 6).clamp(0, size as i32 - 1) as usize;
            img[(xx, yy)] = accent;
        }

        img
    }

    fn render_list_paragraph(
        &self,
        ui: &mut egui::Ui,
        marker: Option<&str>,
        marker_color: Color32,
        indent_px: f32,
        spans: &[InlineSpan],
    ) {
        // Split into lines on embedded '\n'
        // Safety: `lines` is initialized with one empty Vec and we only ever push more,
        // so `last_mut()` will always return Some.
        let mut lines: Vec<Vec<InlineSpan>> = vec![Vec::new()];
        for s in spans.iter().cloned() {
            match s {
                InlineSpan::Text(t) if t.contains('\n') => {
                    let parts: Vec<&str> = t.split('\n').collect();
                    for (pi, part) in parts.iter().enumerate() {
                        if !part.is_empty() {
                            if let Some(last) = lines.last_mut() {
                                last.push(InlineSpan::Text(part.to_string()));
                            }
                        }
                        if pi < parts.len() - 1 {
                            lines.push(Vec::new());
                        }
                    }
                }
                other => {
                    if let Some(last) = lines.last_mut() {
                        last.push(other);
                    }
                }
            }
        }

        for (li, mut line) in lines.into_iter().enumerate() {
            ui.horizontal_wrapped(|ui| {
                if li == 0 {
                    if let Some(marker) = marker {
                        ui.label(
                            RichText::new(format!("{} ", marker))
                                .size(self.font_sizes.body)
                                .color(marker_color),
                        );
                    } else {
                        ui.add_space(indent_px);
                    }
                } else {
                    // Determine additional indentation from leading spaces in this line
                    let mut leading_spaces = 0usize;
                    if let Some(InlineSpan::Text(t0)) = line.first_mut() {
                        leading_spaces = t0.chars().take_while(|c| *c == ' ').count();
                        if leading_spaces > 0 {
                            let trimmed = t0.trim_start_matches(' ').to_string();
                            *t0 = trimmed;
                        }
                    }
                    let indent = indent_px + (leading_spaces as f32) * 6.0;
                    ui.add_space(indent);
                }
                ui.spacing_mut().item_spacing.x = 0.0;
                for span in &line {
                    self.render_inline_span(ui, span, None, None);
                }
            });
        }
    }

    /// Render a list with proper inline formatting, including nested block content
    fn render_list(&self, ui: &mut egui::Ui, ordered: bool, items: &[ListItem]) {
        if items.is_empty() {
            return;
        }

        ui.add_space(4.0);
        let marker_color = ui.visuals().text_color();
        let indent_px = 18.0;

        for (index, item) in items.iter().enumerate() {
            let marker = if ordered {
                format!("{}.", index + 1)
            } else {
                "-".to_string()
            };

            let mut blocks = item.blocks.iter();
            if let Some(first_block) = blocks.next() {
                match first_block {
                    MarkdownElement::Paragraph(spans) => {
                        self.render_list_paragraph(
                            ui,
                            Some(&marker),
                            marker_color,
                            indent_px,
                            spans,
                        );
                    }
                    _ => {
                        ui.horizontal_wrapped(|ui| {
                            ui.label(
                                RichText::new(format!("{} ", marker))
                                    .size(self.font_sizes.body)
                                    .color(marker_color),
                            );
                        });
                        self.render_list_block(ui, first_block, indent_px, marker_color);
                    }
                }
            } else {
                ui.horizontal_wrapped(|ui| {
                    ui.label(
                        RichText::new(format!("{} ", marker))
                            .size(self.font_sizes.body)
                            .color(marker_color),
                    );
                });
            }

            for block in blocks {
                self.render_list_block(ui, block, indent_px, marker_color);
            }
        }

        ui.add_space(4.0);
    }

    fn render_list_block(
        &self,
        ui: &mut egui::Ui,
        block: &MarkdownElement,
        indent_px: f32,
        marker_color: Color32,
    ) {
        match block {
            MarkdownElement::Paragraph(spans) => {
                self.render_list_paragraph(ui, None, marker_color, indent_px, spans);
            }
            _ => {
                ui.horizontal(|ui| {
                    ui.add_space(indent_px);
                    ui.vertical(|ui| {
                        self.render_element_body(ui, block);
                    });
                });
            }
        }
    }

    /// Render a code block using pre-computed syntax highlighting.
    /// The `highlighted` parameter contains cached token colors computed at parse time.
    fn render_code_block(
        &self,
        ui: &mut egui::Ui,
        language: Option<&str>,
        code: &str,
        highlighted: Option<&Vec<HighlightedLine>>,
    ) {
        ui.add_space(8.0);

        // Special handling for Mermaid diagrams
        if let Some(lang) = language {
            if lang.eq_ignore_ascii_case("mermaid") {
                let _ = self.render_mermaid_block(ui, code);
                ui.add_space(8.0);
                return;
            }
        }

        let tc = ThemeColors::current(ui.visuals().dark_mode);
        let frame_response = egui::Frame::none()
            .fill(tc.code_bg)
            .stroke(Stroke::new(1.0, tc.code_border))
            .inner_margin(8.0)
            .show(ui, |ui| {
                ui.with_layout(egui::Layout::top_down(egui::Align::LEFT), |ui| {
                    if let Some(lang) = language {
                        ui.label(
                            RichText::new(lang)
                                .size(self.font_sizes.code - 1.0)
                                .color(tc.code_label)
                                .family(egui::FontFamily::Monospace),
                        );
                        ui.add_space(2.0);
                    }

                    // Use pre-computed highlighting if available
                    if let Some(lines) = highlighted {
                        for tokens in lines {
                            ui.horizontal_wrapped(|ui| {
                                // Remove spacing between tokens to avoid visual gaps
                                ui.spacing_mut().item_spacing.x = 0.0;

                                for token in tokens {
                                    // Check if this token is pure whitespace
                                    if token.text.chars().all(|c| c == ' ' || c == '\t') {
                                        ui.label(
                                            RichText::new(&token.text)
                                                .size(self.font_sizes.code)
                                                .color(Color32::TRANSPARENT)
                                                .family(egui::FontFamily::Monospace),
                                        );
                                    } else {
                                        // Split by spaces and handle separately for proper spacing
                                        let parts: Vec<&str> = token.text.split(' ').collect();
                                        for (i, part) in parts.iter().enumerate() {
                                            if !part.is_empty() {
                                                let mut rich_text = RichText::new(*part)
                                                    .size(self.font_sizes.code)
                                                    .color(token.color)
                                                    .family(egui::FontFamily::Monospace);

                                                if token.bold {
                                                    rich_text = rich_text.strong();
                                                }
                                                if token.italic {
                                                    rich_text = rich_text.italics();
                                                }

                                                ui.label(rich_text);
                                            }

                                            // Add transparent space between parts
                                            if i < parts.len() - 1 {
                                                ui.label(
                                                    RichText::new(" ")
                                                        .size(self.font_sizes.code)
                                                        .color(Color32::TRANSPARENT)
                                                        .family(egui::FontFamily::Monospace),
                                                );
                                            }
                                        }
                                    }
                                }
                            });
                        }
                    } else {
                        // Fallback: render as plain text (no highlighting available)
                        let tc = ThemeColors::current(ui.visuals().dark_mode);
                        ui.label(
                            RichText::new(code)
                                .size(self.font_sizes.code)
                                .color(tc.code_fallback_text)
                                .family(egui::FontFamily::Monospace),
                        );
                    }
                });
            });

        // Add context menu for code blocks
        #[cfg(test)]
        {
            let _ = &frame_response;
            self.render_code_block_context_menu(ui, code, language);
        }
        #[cfg(not(test))]
        frame_response.response.context_menu(|ui| {
            self.render_code_block_context_menu(ui, code, language);
        });

        ui.add_space(8.0);
    }

    /// Try to render a Mermaid diagram. Returns true if handled (rendered or placeholder drawn).
    fn render_mermaid_block(&self, ui: &mut egui::Ui, code: &str) -> bool {
        self.mermaid
            .render_block(ui, code, self.ui_scale(), self.font_sizes.code)
    }

    fn hash_str(s: &str) -> u64 {
        let mut h = DefaultHasher::new();
        s.hash(&mut h);
        h.finish()
    }

    fn hash_inline_spans(spans: &[InlineSpan]) -> u64 {
        let mut h = DefaultHasher::new();
        for span in spans {
            match span {
                InlineSpan::Text(t) => {
                    0u8.hash(&mut h);
                    t.hash(&mut h);
                }
                InlineSpan::Code(t) => {
                    1u8.hash(&mut h);
                    t.hash(&mut h);
                }
                InlineSpan::Strong(t) => {
                    2u8.hash(&mut h);
                    t.hash(&mut h);
                }
                InlineSpan::Emphasis(t) => {
                    3u8.hash(&mut h);
                    t.hash(&mut h);
                }
                InlineSpan::Strikethrough(t) => {
                    4u8.hash(&mut h);
                    t.hash(&mut h);
                }
                InlineSpan::Link { text, url } => {
                    5u8.hash(&mut h);
                    text.hash(&mut h);
                    url.hash(&mut h);
                }
                InlineSpan::Image { src, alt, title } => {
                    6u8.hash(&mut h);
                    src.hash(&mut h);
                    alt.hash(&mut h);
                    title.hash(&mut h);
                }
            }
        }
        h.finish()
    }

    fn alignment_hash(align: Alignment) -> u8 {
        match align {
            Alignment::None => 0,
            Alignment::Left => 1,
            Alignment::Center => 2,
            Alignment::Right => 3,
        }
    }

    fn alignment_to_egui(align: Alignment) -> Align {
        match align {
            Alignment::Center => Align::Center,
            Alignment::Right => Align::RIGHT,
            Alignment::Left | Alignment::None => Align::LEFT,
        }
    }

    fn alignment_for_column(alignments: &[Alignment], col_idx: usize) -> Align {
        alignments
            .get(col_idx)
            .copied()
            .map(Self::alignment_to_egui)
            .unwrap_or(Align::LEFT)
    }

    fn align_to_u8(align: Align) -> u8 {
        if align == Align::LEFT {
            0
        } else if align == Align::Center {
            1
        } else {
            2
        }
    }

    fn next_table_index(&self) -> u64 {
        let mut counter = self.table_counter.borrow_mut();
        let idx = *counter;
        *counter += 1;
        idx
    }

    fn compute_table_id(
        &self,
        headers: &[Vec<InlineSpan>],
        rows: &[Vec<Vec<InlineSpan>>],
        alignments: &[Alignment],
        table_index: u64,
    ) -> u64 {
        let mut hasher = DefaultHasher::new();
        if let Some(base) = self.base_dir.borrow().as_ref() {
            base.hash(&mut hasher);
        }
        hasher.write_u64(table_index);
        hasher.write_u64(self.compute_table_content_hash(headers, rows, alignments));
        hasher.finish()
    }

    fn compute_table_content_hash(
        &self,
        headers: &[Vec<InlineSpan>],
        rows: &[Vec<Vec<InlineSpan>>],
        alignments: &[Alignment],
    ) -> u64 {
        let mut hasher = DefaultHasher::new();
        hasher.write_usize(headers.len());
        for header in headers {
            hasher.write_u64(Self::hash_inline_spans(header));
        }
        hasher.write_usize(alignments.len());
        for align in alignments {
            hasher.write_u8(Self::alignment_hash(*align));
        }
        let mut counted = 0usize;
        for row in rows {
            for cell in row {
                hasher.write_u64(Self::hash_inline_spans(cell));
            }
            counted += 1;
            if counted >= COLUMN_STATS_SAMPLE_ROWS {
                break;
            }
        }
        hasher.finish()
    }

    fn column_stats_for_table(
        &self,
        table_id: u64,
        headers: &[Vec<InlineSpan>],
        rows: &[Vec<Vec<InlineSpan>>],
        alignments: &[Alignment],
    ) -> Vec<ColumnStat> {
        let content_hash = self.compute_table_content_hash(headers, rows, alignments);
        if let Some(entry) = self
            .column_stats_cache
            .borrow()
            .get(&table_id)
            .filter(|entry| entry.content_hash == content_hash)
        {
            return entry.stats.clone();
        }
        let stats = compute_column_stats(headers, rows, COLUMN_STATS_SAMPLE_ROWS);
        self.column_stats_cache.borrow_mut().insert(
            table_id,
            ColumnStatsCacheEntry {
                content_hash,
                stats: stats.clone(),
            },
        );
        stats
    }

    /// Render a table using an egui Grid
    fn render_table(
        &self,
        ui: &mut egui::Ui,
        headers: &[Vec<InlineSpan>],
        rows: &[Vec<Vec<InlineSpan>>],
        alignments: &[Alignment],
    ) {
        if headers.is_empty() {
            return;
        }

        let table_index = self.next_table_index();
        let table_id = self.compute_table_id(headers, rows, alignments, table_index);
        self.render_table_tablebuilder(ui, headers, rows, alignments, table_id);
        ui.add_space(8.0);
    }

    fn extend_table_rect(target: &mut Option<egui::Rect>, rect: egui::Rect) {
        if rect.min.x.is_nan() || rect.min.y.is_nan() || rect.max.x.is_nan() || rect.max.y.is_nan()
        {
            return;
        }
        if let Some(existing) = target {
            existing.min.x = existing.min.x.min(rect.min.x);
            existing.min.y = existing.min.y.min(rect.min.y);
            existing.max.x = existing.max.x.max(rect.max.x);
            existing.max.y = existing.max.y.max(rect.max.y);
        } else {
            *target = Some(rect);
        }
    }

    fn render_table_tablebuilder(
        &self,
        ui: &mut egui::Ui,
        headers: &[Vec<InlineSpan>],
        rows: &[Vec<Vec<InlineSpan>>],
        alignments: &[Alignment],
        table_id: u64,
    ) {
        let column_stats = self.column_stats_for_table(table_id, headers, rows, alignments);
        let ctx =
            TableColumnContext::new(headers, rows, &column_stats, self.font_sizes.body, table_id);
        let mut column_specs = derive_column_specs(&ctx);
        #[cfg(test)]
        if let Some(policies) = take_forced_table_policies() {
            for (spec, policy) in column_specs.iter_mut().zip(policies.into_iter()) {
                spec.set_policy(policy);
            }
        }
        self.apply_persisted_widths(table_id, &mut column_specs);
        let column_aligns: Vec<Align> = (0..column_specs.len())
            .map(|ci| Self::alignment_for_column(alignments, ci))
            .collect();

        let column_spacing = ui.spacing().item_spacing.x.max(6.0);
        let viewport_width = ui.available_width().max(1.0);
        let min_floor = self.table_min_column_width();
        let resolved_widths = self.resolve_table_column_widths(table_id, &column_specs, min_floor);

        // Content-driven table width: estimate natural column widths from content
        // and policy, then determine if the table fits within the viewport.
        let natural_widths = self.estimate_natural_column_widths(
            &column_stats,
            headers,
            &resolved_widths,
            column_specs.len(),
        );
        let natural_total = Self::natural_table_width(&natural_widths, column_spacing);
        let effective_width = natural_total.min(viewport_width);
        let content_fits = effective_width < viewport_width - 0.5;

        let min_widths: Vec<f32> = column_specs
            .iter()
            .map(|spec| match spec.policy {
                ColumnPolicy::Fixed { width, .. } => width,
                ColumnPolicy::Resizable { min, .. } => min,
                ColumnPolicy::Remainder { .. } | ColumnPolicy::Auto => min_floor,
            })
            .collect();
        let spacing_total = column_spacing * column_specs.len().saturating_sub(1) as f32;
        let available_for_columns = viewport_width - spacing_total;
        let mut fixed_total = 0.0;
        let mut flexible_indices: Vec<usize> = Vec::new();
        let mut remainder_indices: Vec<usize> = Vec::new();
        for (idx, spec) in column_specs.iter().enumerate() {
            match spec.policy {
                ColumnPolicy::Fixed { width, .. } => fixed_total += width,
                ColumnPolicy::Resizable { .. } => flexible_indices.push(idx),
                ColumnPolicy::Remainder { .. } => {
                    flexible_indices.push(idx);
                    remainder_indices.push(idx);
                }
                ColumnPolicy::Auto => flexible_indices.push(idx),
            }
        }
        let min_flex_total: f32 = flexible_indices.iter().map(|idx| min_widths[*idx]).sum();
        let desired_total_width = resolved_widths.iter().sum::<f32>() + spacing_total;
        let content_width = desired_total_width.max(viewport_width);
        let remaining_for_flex = available_for_columns - fixed_total;
        let use_hscroll = available_for_columns <= 0.0
            || fixed_total > available_for_columns + 0.5
            || remaining_for_flex < min_flex_total - 0.5;
        let mut adjusted_widths = resolved_widths.clone();
        let mut scaled_down = false;
        if !use_hscroll {
            let remaining = remaining_for_flex.max(0.0);
            let flex_total: f32 = flexible_indices
                .iter()
                .map(|idx| resolved_widths[*idx])
                .sum();
            if flex_total > remaining + 0.5 {
                if remaining <= min_flex_total + 0.5 {
                    for idx in &flexible_indices {
                        adjusted_widths[*idx] = min_widths[*idx].max(1.0);
                    }
                } else {
                    let extra_total = flex_total - min_flex_total;
                    let extra_scale = ((remaining - min_flex_total) / extra_total).clamp(0.0, 1.0);
                    for idx in &flexible_indices {
                        let min_width = min_widths[*idx];
                        let desired = resolved_widths[*idx];
                        let extra = (desired - min_width).max(0.0);
                        adjusted_widths[*idx] = (min_width + extra * extra_scale).max(1.0);
                    }
                }
                scaled_down = true;
            }
        }
        let column_layout: Vec<Column> = if use_hscroll {
            column_specs.iter().map(|spec| spec.as_column()).collect()
        } else if content_fits {
            // CONTENT-FIT MODE — columns sized to content, no Remainder expansion.
            column_specs
                .iter()
                .enumerate()
                .map(|(idx, spec)| {
                    let width = natural_widths
                        .get(idx)
                        .copied()
                        .unwrap_or(min_floor)
                        .max(1.0);
                    let clip = match spec.policy {
                        ColumnPolicy::Fixed { clip, .. } => clip,
                        ColumnPolicy::Resizable { clip, .. } => clip,
                        ColumnPolicy::Remainder { clip } => clip,
                        ColumnPolicy::Auto => false,
                    };
                    let mut col = Column::initial(width)
                        .at_least(min_floor.min(width))
                        .resizable(true);
                    if clip {
                        col = col.clip(true);
                    }
                    col
                })
                .collect()
        } else {
            // VIEWPORT-FILL MODE — existing policy logic unchanged.
            column_specs
                .iter()
                .enumerate()
                .map(|(idx, spec)| {
                    let width = adjusted_widths
                        .get(idx)
                        .copied()
                        .unwrap_or(min_floor)
                        .max(1.0);
                    match spec.policy {
                        ColumnPolicy::Fixed { width, clip } => {
                            let mut col = Column::exact(width);
                            if clip {
                                col = col.clip(true);
                            }
                            col
                        }
                        ColumnPolicy::Resizable { min, clip, .. } => {
                            let mut col = Column::initial(width)
                                .at_least(min.min(width))
                                .resizable(true);
                            if clip {
                                col = col.clip(true);
                            }
                            col
                        }
                        ColumnPolicy::Remainder { clip } => {
                            let mut col = if scaled_down {
                                Column::initial(width).at_least(min_floor.min(width))
                            } else {
                                Column::remainder().at_least(min_floor)
                            };
                            if clip {
                                col = col.clip(true);
                            }
                            col
                        }
                        ColumnPolicy::Auto => {
                            if scaled_down {
                                Column::initial(width).at_least(min_floor.min(width))
                            } else {
                                Column::auto_with_initial_suggestion(width).at_least(min_floor)
                            }
                        }
                    }
                })
                .collect()
        };

        let render_table = |ui: &mut egui::Ui, max_width: f32| {
            let width_bucket = max_width.round() as i32;
            ui.push_id((table_id, width_bucket), |ui| {
                let layout = *ui.layout();
                ui.allocate_ui_with_layout(Vec2::new(max_width, 0.0), layout, |ui| {
                    ui.set_width(max_width);
                    let needs_estimate = {
                        let metrics = self.table_metrics.borrow();
                        metrics
                            .entry(table_id)
                            .is_none_or(|entry| entry.rows.is_empty())
                    };
                    self.begin_table_pass(table_id, rows.len());

                    let prev_spacing = ui.spacing().item_spacing;
                    if (prev_spacing.x - column_spacing).abs() > f32::EPSILON {
                        ui.spacing_mut().item_spacing.x = column_spacing;
                    }

                    let cached_header_height = self
                        .table_metrics
                        .borrow()
                        .entry(table_id)
                        .and_then(|entry| entry.header_height());
                    // Rough header height estimation using equally divided width; refines on next frame via cache.
                    let header_height = cached_header_height.unwrap_or_else(|| {
                        let mut estimate = self.row_height_fallback();
                        let column_count = column_specs.len().max(1);
                        let approx_width = (ui.available_width() / column_count as f32)
                            .max(self.font_sizes.body * 6.0)
                            .max(48.0);
                        let style = ui.style().clone();
                        estimate = headers
                            .iter()
                            .enumerate()
                            .map(|(ci, spans)| {
                                let halign = column_aligns.get(ci).copied().unwrap_or(Align::LEFT);
                                let build = self.cached_layout_job(
                                    &style,
                                    None,
                                    ci,
                                    spans,
                                    approx_width,
                                    true,
                                    halign,
                                );
                                ui.fonts(|f| f.layout_job(build.job.clone()).size().y + 6.0)
                            })
                            .fold(estimate, |acc, h| acc.max(h));
                        estimate.min(self.row_height_fallback() * 3.0)
                    });

                    let fallback_row_height = self.row_height_fallback();

                    // Use uniform row height to avoid scroll issues with heterogeneous_rows.
                    // Calculate the maximum height across all rows for consistency.
                    let uniform_row_height = if needs_estimate {
                        let approx_widths = if content_fits {
                            // Content-fit mode: use natural widths directly.
                            // These are the actual widths columns will render at.
                            natural_widths.clone()
                        } else {
                            self.estimate_table_row_widths(
                                &column_specs,
                                &resolved_widths,
                                &adjusted_widths,
                                available_for_columns,
                                min_floor,
                                use_hscroll,
                                scaled_down,
                            )
                        };
                        let style = ui.style().clone();
                        let mut max_height = fallback_row_height;
                        for row in rows.iter() {
                            let estimate = self.estimate_table_row_height(
                                ui,
                                &style,
                                row,
                                &column_aligns,
                                &approx_widths,
                                fallback_row_height,
                            );
                            max_height = max_height.max(estimate);
                        }
                        // Cache as uniform height for all rows
                        for idx in 0..rows.len() {
                            self.update_row_height(table_id, idx, max_height);
                        }
                        max_height
                    } else {
                        // Use cached max height from previous frames
                        (0..rows.len())
                            .filter_map(|idx| {
                                self.table_metrics
                                    .borrow()
                                    .entry(table_id)
                                    .and_then(|e| e.row(idx))
                                    .map(|r| r.max_height)
                            })
                            .fold(fallback_row_height, f32::max)
                    };

                    // Capture painter from outer UI BEFORE TableBuilder to ensure dividers
                    // paint on top of both header and body (fixes header divider visibility).
                    let outer_painter = ui.painter().clone();
                    let outer_visuals = ui.visuals().clone();
                    let outer_ctx = ui.ctx().clone();

                    let mut table = TableBuilder::new(ui).striped(true).vscroll(false);
                    for column in &column_layout {
                        table = table.column(*column);
                    }

                    // Use RefCell to allow capturing widths from body closure.
                    // BUG FIX: Previously captured ui.min_rect().width() which is the cell *content* width.
                    // Now we capture body.widths() which gives the actual *allocated* column widths.
                    // See: https://docs.rs/egui_extras/latest/egui_extras/struct.TableBody.html#method.widths
                    let column_widths: RefCell<Vec<f32>> =
                        RefCell::new(vec![0.0f32; column_specs.len()]);
                    // Track header and body rects separately for accurate table bounds calculation.
                    // The header rect captures the header row bounds, body_rect captures body row bounds.
                    // These are combined after rendering to get the full table rect.
                    let mut header_rect: Option<egui::Rect> = None;
                    let mut body_rect: Option<egui::Rect> = None;
                    // body_layout_rect captures the allocated layout region from body.max_rect(),
                    // which is more accurate than union of cell min_rects for table width.
                    let body_layout_rect: RefCell<Option<egui::Rect>> = RefCell::new(None);
                    // Track clip rects separately for header and body, then union them.
                    // This ensures dividers respect scroll boundaries for both regions.
                    let mut header_clip_rect: Option<egui::Rect> = None;
                    let body_clip_rect: RefCell<Option<egui::Rect>> = RefCell::new(None);

                    let mut height_change = false;
                    let header_height_actual = Cell::new(0.0f32);
                    table
                        .header(header_height, |mut header| {
                            for (ci, _) in column_specs.iter().enumerate() {
                                header.col(|ui| {
                                    let width = ui.available_width().max(1.0);
                                    let spans =
                                        headers.get(ci).map(|v| v.as_slice()).unwrap_or(&[]);
                                    let halign =
                                        column_aligns.get(ci).copied().unwrap_or(Align::LEFT);
                                    let cell_height = self.render_overhauled_cell(
                                        ui, spans, width, true, None, ci, halign,
                                    );
                                    header_height_actual
                                        .set(header_height_actual.get().max(cell_height));
                                    // NOTE: Use ui.max_rect() (allocated cell bounds), not ui.min_rect()
                                    // (content bounds). The allocated rect matches what egui uses for
                                    // striped backgrounds, ensuring the border aligns with the stripe extent.
                                    // Column widths are captured separately from body.widths() below.
                                    Self::extend_table_rect(&mut header_rect, ui.max_rect());
                                    if header_clip_rect.is_none() {
                                        header_clip_rect = Some(ui.clip_rect());
                                    }
                                });
                            }
                        })
                        .body(|body| {
                            // Capture the actual allocated column widths from the table layout system.
                            *column_widths.borrow_mut() = body.widths().to_vec();

                            // Capture body's layout rect for accurate table width calculation.
                            *body_layout_rect.borrow_mut() = Some(body.max_rect());

                            // Use uniform row height to avoid scroll calculation issues.
                            // The rows() method renders all rows without virtualization issues.
                            let row_count = rows.len();
                            body.rows(uniform_row_height, row_count, |mut row| {
                                let idx = row.index();
                                let row_cells = rows.get(idx);
                                let mut actual_row_height = fallback_row_height;
                                for (ci, _) in column_specs.iter().enumerate() {
                                    let mut cell_height = fallback_row_height;
                                    row.col(|ui| {
                                        let width = ui.available_width().max(1.0);
                                        let spans = row_cells
                                            .and_then(|cells| cells.get(ci))
                                            .map(|cell| cell.as_slice())
                                            .unwrap_or(&[]);
                                        let halign =
                                            column_aligns.get(ci).copied().unwrap_or(Align::LEFT);
                                        cell_height = self.render_overhauled_cell(
                                            ui,
                                            spans,
                                            width,
                                            false,
                                            Some(idx),
                                            ci,
                                            halign,
                                        );
                                        // Use max_rect() for allocated bounds; see header comment above.
                                        Self::extend_table_rect(&mut body_rect, ui.max_rect());
                                        if body_clip_rect.borrow().is_none() {
                                            *body_clip_rect.borrow_mut() = Some(ui.clip_rect());
                                        }
                                    });
                                    actual_row_height = actual_row_height.max(cell_height);
                                }
                                // Track actual heights to refine uniform height on next frame
                                let height_delta = actual_row_height - uniform_row_height;
                                if height_delta > 0.5 {
                                    height_change = true;
                                }
                                self.update_row_height(table_id, idx, actual_row_height);
                                self.note_row_rendered(table_id);
                            });
                        });

                    ui.spacing_mut().item_spacing = prev_spacing;

                    // Extract column widths from RefCell for use in divider painting
                    let widths = column_widths.into_inner();
                    let layout_rect = body_layout_rect.into_inner();
                    let body_clip = body_clip_rect.into_inner();
                    let measured_header_height = header_height_actual.get();
                    if self.update_header_height(table_id, measured_header_height) {
                        ui.ctx().request_repaint();
                    }

                    let (rect, clip_rect) = self.resolve_table_rects(
                        header_rect,
                        body_rect,
                        layout_rect,
                        header_clip_rect,
                        body_clip,
                        &widths,
                        column_spacing,
                    );
                    let frame_id = outer_ctx.frame_nr();
                    let change = self.record_resolved_widths(table_id, frame_id, &widths);
                    self.persist_resizable_widths(table_id, &column_specs, &widths);
                    self.handle_width_change(&outer_ctx, table_id, change);
                    // Use outer_painter (captured before TableBuilder) to ensure
                    // dividers paint on top of both header and body.
                    self.paint_table_dividers(
                        &outer_painter,
                        &outer_visuals,
                        rect,
                        clip_rect,
                        &widths,
                        header_height,
                        column_spacing,
                        prev_spacing.y,
                        !content_fits,
                    );
                    if height_change {
                        if std::env::var("MDMDVIEW_DEBUG_SCROLL").is_ok() {
                            use std::io::Write;
                            if let Ok(mut f) = std::fs::OpenOptions::new()
                                .create(true)
                                .append(true)
                                .open(r"c:\tmp\scroll-debug.log")
                            {
                                let _ =
                                    writeln!(f, "[REPAINT] table={} triggering repaint", table_id);
                            }
                        }
                        ui.ctx().request_repaint();
                    }
                });
            });
        };

        if use_hscroll {
            egui::ScrollArea::horizontal()
                .id_source(("md_table_hscroll_overhaul", table_id))
                .auto_shrink([false, true])
                .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::VisibleWhenNeeded)
                .show(ui, |ui| {
                    ui.set_min_width(content_width);
                    render_table(ui, content_width);
                });
        } else if content_fits {
            render_table(ui, effective_width);
        } else {
            render_table(ui, viewport_width);
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn resolve_table_rects(
        &self,
        header_rect: Option<egui::Rect>,
        body_rect: Option<egui::Rect>,
        layout_rect: Option<egui::Rect>,
        header_clip_rect: Option<egui::Rect>,
        body_clip: Option<egui::Rect>,
        widths: &[f32],
        column_spacing: f32,
    ) -> (egui::Rect, egui::Rect) {
        // Combine header and body clip rects to ensure dividers respect scroll bounds.
        let clip_rect = match (header_clip_rect, body_clip) {
            (Some(h), Some(b)) => Some(h.union(b)),
            (h, b) => h.or(b),
        };

        // Calculate accurate table width from column widths.
        // This is more accurate than union of cell min_rects which may be smaller
        // than the allocated column space.
        let calculated_width: f32 =
            widths.iter().sum::<f32>() + column_spacing * widths.len().saturating_sub(1) as f32;

        // Combine header/body bounds with layout bounds for accurate borders,
        // especially when the first column is centered or right-aligned.
        let left = layout_rect
            .map(|rect| rect.left())
            .or_else(|| header_rect.map(|rect| rect.left()))
            .or_else(|| body_rect.map(|rect| rect.left()));
        let right = left.map(|value| value + calculated_width);
        let top = header_rect
            .map(|rect| rect.top())
            .or_else(|| body_rect.map(|rect| rect.top()))
            .or_else(|| layout_rect.map(|rect| rect.top()));
        let bottom = body_rect
            .map(|rect| rect.bottom())
            .or_else(|| header_rect.map(|rect| rect.bottom()))
            .or_else(|| layout_rect.map(|rect| rect.bottom()));
        let table_rect = match (left, right, top, bottom) {
            (Some(left), Some(right), Some(top), Some(bottom)) => Some(egui::Rect::from_min_max(
                egui::pos2(left, top),
                egui::pos2(right, bottom),
            )),
            _ => None,
        };

        let zero_rect = egui::Rect::from_min_max(egui::pos2(0.0, 0.0), egui::pos2(0.0, 0.0));
        let rect = table_rect.or(layout_rect).unwrap_or(zero_rect);
        let clip_rect = clip_rect.unwrap_or(rect);
        (rect, clip_rect)
    }

    fn table_min_column_width(&self) -> f32 {
        (self.font_sizes.body * 6.0).max(48.0)
    }

    fn resolve_table_column_widths(
        &self,
        table_id: u64,
        column_specs: &[ColumnSpec],
        min_floor: f32,
    ) -> Vec<f32> {
        if column_specs.is_empty() {
            return Vec::new();
        }

        let stored_widths = {
            let metrics = self.table_metrics.borrow();
            metrics
                .entry(table_id)
                .map(|entry| entry.current_widths().to_vec())
                .unwrap_or_default()
        };

        column_specs
            .iter()
            .enumerate()
            .map(|(idx, spec)| {
                let stored = stored_widths
                    .get(idx)
                    .copied()
                    .filter(|width| width.is_finite() && *width > 0.0);
                let width = match spec.policy {
                    ColumnPolicy::Fixed { width, .. } => width,
                    ColumnPolicy::Resizable { min, preferred, .. } => {
                        stored.unwrap_or(preferred.max(min)).max(min)
                    }
                    ColumnPolicy::Remainder { .. } => min_floor,
                    ColumnPolicy::Auto => stored.unwrap_or(min_floor).max(min_floor),
                };
                width.max(1.0)
            })
            .collect()
    }

    #[cfg(test)]
    fn estimate_table_column_widths(
        &self,
        column_specs: &[ColumnSpec],
        max_width: f32,
        column_spacing: f32,
    ) -> Vec<f32> {
        let column_count = column_specs.len().max(1);
        let spacing_total = column_spacing * column_count.saturating_sub(1) as f32;
        let available = (max_width - spacing_total).max(1.0);
        let min_floor = self.table_min_column_width();
        let mut widths: Vec<f32> = column_specs
            .iter()
            .map(|spec| match spec.policy {
                ColumnPolicy::Fixed { width, .. } => width,
                ColumnPolicy::Resizable { min, .. } => min,
                ColumnPolicy::Remainder { .. } => min_floor,
                ColumnPolicy::Auto => min_floor,
            })
            .map(|width| width.max(1.0))
            .collect();

        if widths.is_empty() {
            return vec![available];
        }
        let sum: f32 = widths.iter().sum();
        if sum > available {
            let scale = available / sum;
            for width in &mut widths {
                *width = (*width * scale).max(1.0);
            }
        }
        widths
    }

    #[allow(clippy::too_many_arguments)]
    fn estimate_table_row_widths(
        &self,
        column_specs: &[ColumnSpec],
        resolved_widths: &[f32],
        adjusted_widths: &[f32],
        available_for_columns: f32,
        min_floor: f32,
        use_hscroll: bool,
        scaled_down: bool,
    ) -> Vec<f32> {
        if use_hscroll {
            return resolved_widths.to_vec();
        }

        let mut widths = adjusted_widths.to_vec();
        if scaled_down {
            return widths;
        }

        let remainder_indices: Vec<usize> = column_specs
            .iter()
            .enumerate()
            .filter(|(_, spec)| matches!(spec.policy, ColumnPolicy::Remainder { .. }))
            .map(|(idx, _)| idx)
            .collect();
        if remainder_indices.is_empty() {
            return widths;
        }

        let mut used = 0.0;
        for (idx, width) in widths.iter().enumerate() {
            if remainder_indices.contains(&idx) {
                continue;
            }
            used += *width;
        }
        let remaining = available_for_columns - used;
        if remaining <= 0.0 {
            return widths;
        }
        let per = (remaining / remainder_indices.len() as f32).max(min_floor);
        for idx in remainder_indices {
            if let Some(width) = widths.get_mut(idx) {
                *width = per;
            }
        }
        widths
    }

    #[cfg(test)]
    fn estimate_table_total_width(
        &self,
        table_id: u64,
        column_specs: &[ColumnSpec],
        column_spacing: f32,
    ) -> f32 {
        if column_specs.is_empty() {
            return 0.0;
        }
        let min_floor = self.table_min_column_width();
        let widths = self.resolve_table_column_widths(table_id, column_specs, min_floor);
        let spacing_total = column_spacing * widths.len().saturating_sub(1) as f32;
        widths.iter().sum::<f32>() + spacing_total
    }

    /// Estimate per-column natural widths from content statistics and
    /// policy-resolved widths (which include any user-persisted widths).
    ///
    /// For each column, takes the maximum of:
    /// - Content-based estimate (grapheme count * char_width_factor + cell_padding)
    /// - Policy-resolved width (from resolve_table_column_widths)
    ///
    /// Each column width is clamped to [MIN_COL_WIDTH, MAX_COL_WIDTH].
    fn estimate_natural_column_widths(
        &self,
        column_stats: &[ColumnStat],
        headers: &[Vec<InlineSpan>],
        resolved_widths: &[f32],
        column_count: usize,
    ) -> Vec<f32> {
        let body = self.font_sizes.body;
        let char_width_factor = body * 0.62;
        let cell_padding = body * 1.8;
        let min_col_width = body * 4.0;
        let max_col_width = body * 35.0;

        (0..column_count)
            .map(|i| {
                // Extract header text and compute its display width
                let header_width = headers
                    .get(i)
                    .map(|spans| {
                        let text = spans_to_text(spans);
                        UnicodeWidthStr::width(text.as_str())
                    })
                    .unwrap_or(0);

                // Content width from stats (already unicode-width-based)
                let content_graphemes = column_stats.get(i).map(|s| s.max_graphemes).unwrap_or(0);

                let display_graphemes = header_width.max(content_graphemes);
                let content_width = display_graphemes as f32 * char_width_factor + cell_padding;
                let policy_width = resolved_widths.get(i).copied().unwrap_or(0.0);
                let natural = content_width.max(policy_width);
                natural.clamp(min_col_width, max_col_width)
            })
            .collect()
    }

    /// Compute the total natural table width from per-column widths and inter-column spacing.
    fn natural_table_width(natural_widths: &[f32], column_spacing: f32) -> f32 {
        let spacing = column_spacing * natural_widths.len().saturating_sub(1) as f32;
        natural_widths.iter().sum::<f32>() + spacing
    }

    fn estimate_table_row_height(
        &self,
        ui: &egui::Ui,
        style: &egui::Style,
        row: &[Vec<InlineSpan>],
        column_aligns: &[Align],
        widths: &[f32],
        fallback: f32,
    ) -> f32 {
        let mut max_height = fallback;
        let column_count = widths.len().max(row.len()).max(1);
        for ci in 0..column_count {
            let spans = row.get(ci).map(|cell| cell.as_slice()).unwrap_or(&[]);
            if spans.is_empty() {
                continue;
            }
            let width = widths
                .get(ci)
                .copied()
                .or_else(|| widths.last().copied())
                .unwrap_or(1.0);
            let halign = column_aligns.get(ci).copied().unwrap_or(Align::LEFT);
            let height = self.estimate_table_cell_height(style, ui, spans, width, halign, fallback);
            max_height = max_height.max(height);
        }
        max_height
    }

    fn estimate_table_cell_height(
        &self,
        style: &egui::Style,
        ui: &egui::Ui,
        spans: &[InlineSpan],
        width: f32,
        halign: Align,
        fallback: f32,
    ) -> f32 {
        if spans.is_empty() {
            return fallback;
        }
        let fragments = self.cell_fragments(spans);
        let mut total = 0.0;
        for (idx, fragment) in fragments.into_iter().enumerate() {
            if idx > 0 {
                total += 2.0;
            }
            match fragment {
                CellFragment::Text(slice) => {
                    let build = self.build_layout_job(style, slice, width, false, halign);
                    let height = ui.fonts(|f| f.layout_job(build.job.clone()).size().y);
                    total += height;
                }
                CellFragment::Emoji(_) => {
                    total += self.font_sizes.body * 1.2;
                }
                CellFragment::Image(span) => {
                    total += self.estimate_table_image_height(ui, span, width);
                }
            }
        }
        total.max(fallback)
    }

    fn estimate_table_image_height(
        &self,
        ui: &egui::Ui,
        span: &InlineSpan,
        available_w: f32,
    ) -> f32 {
        let InlineSpan::Image { src, title, .. } = span else {
            return self.font_sizes.body * 1.2;
        };
        let resolved = self.resolve_image_path(src);
        let (tw, th) = self
            .get_or_load_image_texture(ui, &resolved)
            .map(|(_, w, h)| (w as f32, h as f32))
            .unwrap_or((self.font_sizes.body * 12.0, self.font_sizes.body * 8.0));
        let base_scale = self.ui_scale();
        let scaled_w = tw * base_scale;
        let scale = if scaled_w > available_w {
            (available_w / tw).clamp(0.01, 4.0)
        } else {
            base_scale
        };
        let mut height = (th * scale).round();
        if let Some(text) = title {
            if !text.is_empty() {
                height += 2.0 + (self.font_sizes.body - 2.0).max(1.0);
            }
        }
        height + 6.0
    }

    fn row_height_fallback(&self) -> f32 {
        self.font_sizes.body * 1.6
    }

    fn update_row_height(&self, table_id: u64, idx: usize, height: f32) {
        let clamped = height.max(self.row_height_fallback());
        let mut metrics = self.table_metrics.borrow_mut();
        let entry = metrics.entry_mut(table_id);
        let row = entry.ensure_row(idx);
        row.max_height = clamped;
        row.dirty = false;
    }

    fn update_header_height(&self, table_id: u64, height: f32) -> bool {
        let mut metrics = self.table_metrics.borrow_mut();
        let entry = metrics.entry_mut(table_id);
        entry.update_header_height(height)
    }

    fn begin_table_pass(&self, table_id: u64, total_rows: usize) {
        let mut metrics = self.table_metrics.borrow_mut();
        metrics.entry_mut(table_id).begin_pass(total_rows);
    }

    fn note_row_rendered(&self, table_id: u64) {
        let mut metrics = self.table_metrics.borrow_mut();
        metrics.entry_mut(table_id).note_row_rendered();
    }

    fn apply_persisted_widths(&self, table_id: u64, specs: &mut [ColumnSpec]) {
        if specs.is_empty() {
            return;
        }
        let mut metrics = self.table_metrics.borrow_mut();
        let entry = metrics.entry_mut(table_id);
        for spec in specs.iter_mut() {
            if let Some(width) = entry.persisted_width(spec.policy_hash) {
                spec.apply_preferred_width(width);
            }
        }
    }

    fn persist_resizable_widths(&self, table_id: u64, specs: &[ColumnSpec], widths: &[f32]) {
        if specs.is_empty() || widths.is_empty() {
            return;
        }
        let mut metrics = self.table_metrics.borrow_mut();
        let entry = metrics.entry_mut(table_id);

        // Check if font size changed since last persist - if so, clear old widths
        // to prevent size mismatch after zoom changes. Skip persisting in this
        // frame so the next layout can compute fresh widths before we store them.
        if entry.check_font_size_change(self.font_sizes.body) {
            return;
        }

        for (spec, width) in specs.iter().zip(widths.iter()) {
            if let ColumnPolicy::Resizable { .. } = spec.policy {
                let stored = entry.persisted_width(spec.policy_hash).unwrap_or(-1.0);
                if stored < 0.0 || (stored - width).abs() > 0.5 {
                    entry.set_persisted_width(spec.policy_hash, *width);
                }
            }
        }
    }

    fn record_resolved_widths(&self, table_id: u64, frame_id: u64, widths: &[f32]) -> WidthChange {
        if widths.is_empty() {
            return WidthChange::None;
        }
        let mut metrics = self.table_metrics.borrow_mut();
        metrics.entry_mut(table_id).update_widths(widths, frame_id)
    }

    fn handle_width_change(&self, ctx: &Context, table_id: u64, change: WidthChange) {
        if matches!(change, WidthChange::Large) {
            let frame_id = ctx.frame_nr();
            let mut metrics = self.table_metrics.borrow_mut();
            let entry = metrics.entry_mut(table_id);
            if entry.last_discard_frame != Some(frame_id) {
                ctx.request_repaint();
                entry.last_discard_frame = Some(frame_id);
            }
        }
    }

    /// Paint vertical dividers between table columns, a horizontal header separator,
    /// and an outer border.
    ///
    /// # Arguments
    /// * `widths` - The actual allocated column widths from `TableBody::widths()`.
    ///   IMPORTANT: These must NOT be cell content widths from `ui.min_rect()`,
    ///   which would cause dividers to be misaligned.
    /// * `header_height` - The height of the header row. A horizontal separator is
    ///   drawn at this y-offset to visually separate header from body rows.
    #[allow(clippy::too_many_arguments)]
    fn paint_table_dividers(
        &self,
        painter: &Painter,
        visuals: &Visuals,
        rect: egui::Rect,
        clip_rect: egui::Rect,
        widths: &[f32],
        header_height: f32,
        column_spacing: f32,
        row_spacing: f32,
        draw_dividers: bool,
    ) {
        if widths.is_empty() {
            return;
        }
        let separator_color = visuals
            .widgets
            .noninteractive
            .bg_stroke
            .color
            .gamma_multiply(0.9);
        let separator_stroke = Stroke::new(1.0, separator_color);
        let border_stroke = visuals.window_stroke();
        // Expand border rect by half the column/row spacing to match the visual
        // extent of egui's striped row backgrounds (which expand by
        // item_spacing/2 on each side). This eliminates the double-line effect
        // at table edges and provides padding between the border and cell text.
        let half_col_spacing = column_spacing * 0.5;
        let half_row_spacing = row_spacing * 0.5;
        let border_rect = egui::Rect::from_min_max(
            egui::pos2(
                rect.left() - half_col_spacing,
                rect.top() - half_row_spacing,
            ),
            egui::pos2(
                rect.right() + half_col_spacing,
                rect.bottom() + half_row_spacing,
            ),
        );
        // Expand clip_rect to include the full border rect so borders aren't clipped.
        // The cell clip_rect may not include the outer border area.
        let expanded_clip = clip_rect.union(border_rect);
        let painter = painter.with_clip_rect(expanded_clip);

        // Draw vertical dividers between columns
        if draw_dividers && widths.len() > 1 {
            let mut x = rect.left();
            for (idx, width) in widths
                .iter()
                .take(widths.len().saturating_sub(1))
                .enumerate()
            {
                x += *width;
                let divider_x = x + column_spacing * (idx as f32 + 0.5);
                let x_pos = (divider_x.round() + 0.5).clamp(rect.left(), rect.right());
                painter.vline(x_pos, border_rect.y_range(), separator_stroke);
            }
        }

        // Draw horizontal separator below header row
        if header_height > 0.0 {
            let header_y = rect.top() + header_height;
            if header_y < rect.bottom() {
                painter.hline(
                    border_rect.x_range(),
                    header_y.round() + 0.5,
                    separator_stroke,
                );
            }
        }

        // Draw outer border
        painter.rect_stroke(border_rect, 0.0, border_stroke);
    }

    #[allow(clippy::too_many_arguments)]
    fn render_overhauled_cell(
        &self,
        ui: &mut egui::Ui,
        spans: &[InlineSpan],
        width: f32,
        is_header: bool,
        row_idx: Option<usize>,
        col_idx: usize,
        halign: Align,
    ) -> f32 {
        let fallback_height = self.row_height_fallback();
        let fragments = self.cell_fragments(spans);
        let inner = ui.allocate_ui_with_layout(
            Vec2::new(width, 0.0),
            egui::Layout::top_down(halign),
            |ui| {
                ui.set_width(width);
                ui.set_max_width(width);
                ui.spacing_mut().item_spacing = egui::vec2(4.0, 2.0);
                if fragments.is_empty() {
                    ui.allocate_exact_size(
                        Vec2::new(width, self.font_sizes.body * 1.2),
                        egui::Sense::hover(),
                    );
                    return;
                }
                for fragment in fragments {
                    match fragment {
                        CellFragment::Text(slice) => {
                            let build = self.cached_layout_job(
                                ui.style(),
                                row_idx,
                                col_idx,
                                slice,
                                width,
                                is_header,
                                halign,
                            );
                            self.paint_table_text_job(ui, width, build);
                        }
                        CellFragment::Emoji(key) => {
                            self.render_table_emoji(ui, &key);
                        }
                        CellFragment::Image(span) => {
                            self.render_inline_span(ui, span, None, Some(is_header));
                        }
                    }
                }
            },
        );
        inner.response.rect.height().max(fallback_height)
    }

    fn paint_table_text_job(
        &self,
        ui: &mut egui::Ui,
        width: f32,
        build: LayoutJobBuild,
    ) -> egui::Response {
        let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
        let height = galley.size().y;
        let (rect, mut response) =
            ui.allocate_exact_size(Vec2::new(width, height), egui::Sense::click());
        let text_color = ui.visuals().text_color();
        let text_origin = Self::aligned_text_origin(rect, &galley, build.job.halign);
        ui.painter_at(rect)
            .galley(text_origin, galley.clone(), text_color);
        if galley.rows.len() > 1 || galley.size().x > width + 0.5 {
            response = response.on_hover_text(build.plain_text.clone());
        }
        #[cfg(test)]
        self.render_cell_context_menu(ui, &build.plain_text);
        #[cfg(not(test))]
        response.context_menu(|ui| {
            self.render_cell_context_menu(ui, &build.plain_text);
        });

        if let Some(link) = self.link_at_pointer(&response, &galley, &build, text_origin) {
            ui.output_mut(|o| o.cursor_icon = egui::CursorIcon::PointingHand);
            response = response.on_hover_text(link.url.clone());
            if response.clicked() {
                self.trigger_link(&link.url);
            }
        }

        response
    }

    #[allow(clippy::too_many_arguments)]
    fn cached_layout_job(
        &self,
        style: &egui::Style,
        row_idx: Option<usize>,
        col_idx: usize,
        spans: &[InlineSpan],
        width: f32,
        is_header: bool,
        halign: Align,
    ) -> LayoutJobBuild {
        let highlight_hash = self
            .highlight_phrase
            .borrow()
            .as_ref()
            .map_or(0, |s| Self::hash_str(s));
        let content_hash = Self::hash_inline_spans(spans);
        let text_color = style.visuals.text_color().to_array();
        let key = CellLayoutKey {
            row: row_idx,
            col: col_idx,
            width: width.round() as u32,
            align: Self::align_to_u8(halign),
            strong: is_header,
            text_color,
            highlight_hash,
            content_hash,
        };
        if let Some(build) = self.table_layout_cache.borrow_mut().get(&key) {
            return build;
        }
        let build = self.build_layout_job(style, spans, width, is_header, halign);
        self.table_layout_cache
            .borrow_mut()
            .insert(key, build.clone());
        build
    }

    fn link_at_pointer<'a>(
        &self,
        response: &egui::Response,
        galley: &Arc<Galley>,
        build: &'a LayoutJobBuild,
        text_origin: egui::Pos2,
    ) -> Option<&'a LinkRange> {
        let pointer = response.hover_pos()?;
        let text_rect = galley.rect.translate(text_origin.to_vec2());
        if !text_rect.contains(pointer) {
            return None;
        }
        let local = pointer - text_origin;
        let cursor = galley.cursor_from_pos(local);
        let idx = cursor.ccursor.index;
        build
            .link_ranges
            .iter()
            .find(|range| range.char_range.contains(&idx))
    }

    fn aligned_text_origin(rect: egui::Rect, galley: &Galley, halign: Align) -> egui::Pos2 {
        let galley_rect = galley.rect;
        let x = if halign == Align::RIGHT {
            rect.right() - galley_rect.right()
        } else if halign == Align::Center {
            rect.center().x - galley_rect.center().x
        } else {
            rect.left() - galley_rect.left()
        };
        egui::pos2(x, rect.top() - galley_rect.top())
    }

    fn render_table_emoji(&self, ui: &mut egui::Ui, emoji: &str) {
        let handle = self.get_or_make_emoji_texture(ui, emoji);
        let size = self.font_sizes.body * 1.2;
        ui.add(
            egui::Image::new(&handle)
                .max_size(Vec2::splat(size))
                .sense(egui::Sense::hover()),
        );
    }

    #[cfg(test)]
    fn resolve_table_widths(available: f32, mins: &[f32], desired: &[f32]) -> Vec<f32> {
        debug_assert_eq!(mins.len(), desired.len());
        if mins.is_empty() {
            return Vec::new();
        }
        let clamped_available = available.max(1.0);
        let desired_total: f32 = desired.iter().sum();
        let min_total: f32 = mins.iter().sum();

        let widths = if min_total >= clamped_available {
            mins.iter()
                .map(|m| m * (clamped_available / min_total))
                .collect()
        } else if desired_total <= clamped_available {
            let slack = desired_total - min_total;
            if slack <= f32::EPSILON {
                let bonus = (clamped_available - min_total) / mins.len() as f32;
                mins.iter().map(|m| m + bonus).collect()
            } else {
                desired.to_vec()
            }
        } else {
            let mut widths = mins.to_vec();
            let extra = clamped_available - min_total;
            let slack: Vec<f32> = desired
                .iter()
                .zip(mins.iter())
                .map(|(d, m)| (d - m).max(0.0))
                .collect();
            let total_slack: f32 = slack.iter().sum();
            for (w, s) in widths.iter_mut().zip(slack.iter()) {
                *w += extra * (*s / total_slack);
            }
            widths
        };

        widths
    }

    /// Open URL in default browser
    #[cfg(not(test))]
    fn open_url(&self, url: &str) {
        if let Err(e) = webbrowser::open(url) {
            eprintln!("Failed to open URL {}: {}", url, e);
        }
    }

    #[cfg(test)]
    fn open_url(&self, _url: &str) {}

    pub(crate) fn trigger_link(&self, url: &str) {
        if let Some(fragment) = Self::extract_fragment(url) {
            *self.pending_anchor.borrow_mut() = Some(fragment);
        } else if Self::is_allowed_scheme(url) {
            self.open_url(url);
        } else {
            eprintln!("Blocked link with unsupported scheme: {}", url);
        }
    }

    fn is_allowed_scheme(url: &str) -> bool {
        url.starts_with("http://") || url.starts_with("https://") || url.starts_with("mailto:")
    }

    /// Consume and return the last clicked internal anchor, if any
    pub fn take_pending_anchor(&self) -> Option<String> {
        self.pending_anchor.borrow_mut().take()
    }

    /// Lookup a header rect by its id (slug)
    pub fn header_rect_for(&self, id: &str) -> Option<egui::Rect> {
        self.header_rects.borrow().get(id).copied()
    }

    /// Lookup the rect for the n-th top-level element rendered in the last frame
    pub fn element_rect_at(&self, index: usize) -> Option<egui::Rect> {
        self.element_rects.borrow().get(index).copied()
    }

    pub fn layout_signature(&self) -> u64 {
        let rects = self.element_rects.borrow();
        let mut h = DefaultHasher::new();
        rects.len().hash(&mut h);
        for rect in rects.iter() {
            let min_x = (rect.min.x * 2.0).round() as i32;
            let min_y = (rect.min.y * 2.0).round() as i32;
            let max_x = (rect.max.x * 2.0).round() as i32;
            let max_y = (rect.max.y * 2.0).round() as i32;
            (min_x, min_y, max_x, max_y).hash(&mut h);
        }
        h.finish()
    }

    pub fn has_pending_renders(&self) -> bool {
        self.mermaid.has_pending() || !self.image_pending.borrow().is_empty()
    }

    /// Set or clear the highlight phrase (case-insensitive)
    pub fn set_highlight_phrase(&self, phrase: Option<&str>) {
        let normalized = phrase
            .map(|p| p.case_fold().nfkc().collect::<String>())
            .filter(|value| !value.is_empty());
        {
            let mut current = self.highlight_phrase.borrow_mut();
            if *current == normalized {
                return;
            }
            *current = normalized;
        }
        self.table_layout_cache.borrow_mut().clear();
    }

    pub fn clear_table_layout_cache(&self) {
        self.table_layout_cache.borrow_mut().clear();
        self.table_metrics.borrow_mut().clear();
        self.column_stats_cache.borrow_mut().clear();
    }

    /// Clear the image failure cache. Called on file load to allow retry of
    /// previously failed images which may now be available.
    pub fn clear_image_failure_cache(&self) {
        self.image_failures.borrow_mut().clear();
    }

    pub fn table_layout_cache_stats(&self) -> (u64, u64) {
        self.table_layout_cache.borrow().stats()
    }

    pub fn table_render_stats(&self) -> (usize, usize) {
        self.table_metrics.borrow().totals()
    }

    /// Get texture cache statistics for debugging.
    ///
    /// Returns (emoji_cache_len, image_cache_len).
    ///
    /// # Texture Memory Management (Stage 7 Research)
    ///
    /// egui's TextureHandle uses reference counting via `Arc<RwLock<TextureManager>>`.
    /// When all TextureHandles for a texture are dropped, egui automatically frees
    /// the GPU texture. Our LruCache eviction correctly drops TextureHandles,
    /// triggering this cleanup.
    ///
    /// Note: `ctx.forget_image(uri)` is for URI-based image loaders, not for
    /// TextureHandle-based textures created via `ctx.load_texture()`. No additional
    /// explicit cleanup API is needed for our use case.
    #[cfg(test)]
    pub fn texture_cache_stats(&self) -> (usize, usize) {
        (
            self.emoji_textures.borrow().len(),
            self.image_textures.borrow().entries.len(),
        )
    }

    /// Get a plain-text representation of a markdown element (for search)
    pub fn element_plain_text(element: &MarkdownElement) -> String {
        match element {
            MarkdownElement::Paragraph(spans) => Self::spans_plain_text(spans),
            MarkdownElement::Header { spans, .. } => Self::spans_plain_text(spans),
            MarkdownElement::CodeBlock { text, .. } => text.clone(),
            MarkdownElement::List { items, .. } => {
                let mut out = String::new();
                for item in items {
                    for block in &item.blocks {
                        if !out.is_empty() {
                            out.push('\n');
                        }
                        out.push_str(&Self::element_plain_text(block));
                    }
                }
                out
            }
            MarkdownElement::Quote { blocks, .. } => {
                let mut out = String::new();
                for block in blocks {
                    if !out.is_empty() {
                        out.push('\n');
                    }
                    out.push_str(&Self::element_plain_text(block));
                }
                out
            }
            MarkdownElement::HorizontalRule => String::from("---"),
            MarkdownElement::Table {
                headers,
                rows,
                alignments: _,
            } => {
                let mut out = String::new();
                for h in headers {
                    if !out.is_empty() {
                        out.push(' ');
                    }
                    out.push_str(&Self::spans_plain_text(h));
                }
                for row in rows {
                    for cell in row {
                        out.push(' ');
                        out.push_str(&Self::spans_plain_text(cell));
                    }
                }
                out
            }
        }
    }

    /// Set the base directory for resolving relative image paths
    pub fn set_base_dir(&self, dir: Option<&Path>) {
        *self.base_dir.borrow_mut() = dir.map(Path::to_path_buf);
    }

    fn poll_image_results(&self, ctx: &egui::Context) -> bool {
        let mut changed = false;
        while let Ok(result) = self.image_result_rx.try_recv() {
            match result {
                ImageLoadResult::Loaded {
                    key,
                    image,
                    size,
                    modified,
                } => {
                    let tex =
                        ctx.load_texture(format!("img:{key}"), image, egui::TextureOptions::LINEAR);
                    self.store_image_texture(&key, tex.clone(), size, modified);
                    self.image_failures.borrow_mut().remove(&key);
                    self.image_pending.borrow_mut().remove(&key);
                    changed = true;
                }
                ImageLoadResult::Failed { key } => {
                    self.image_pending.borrow_mut().remove(&key);
                    self.note_image_failure(&key);
                }
            }
        }
        if changed {
            ctx.request_repaint();
        }
        changed
    }

    fn enqueue_image_job(&self, request: ImageLoadRequest) -> Result<(), ()> {
        self.image_job_tx.try_send(request).map_err(|_| ())
    }

    fn should_retry_image(&self, key: &str) -> bool {
        let key_string = key.to_string();
        let mut failures = self.image_failures.borrow_mut();
        if let Some(failure) = failures.get(&key_string) {
            if failure.last_attempt.elapsed() < IMAGE_FAILURE_BACKOFF {
                return false;
            }
        }
        failures.remove(&key_string);
        true
    }

    fn note_image_failure(&self, key: &str) {
        self.image_failures.borrow_mut().insert(
            key.to_string(),
            ImageFailure {
                last_attempt: Instant::now(),
            },
        );
    }

    fn resolve_image_path(&self, src: &str) -> String {
        if src.starts_with("http://") || src.starts_with("https://") || src.starts_with("data:") {
            // Keep as-is; we don't fetch remote or parse data URIs yet
            return src.to_string();
        }
        let p = Path::new(src);
        if p.is_absolute() {
            return src.to_string();
        }
        if let Some(base) = self.base_dir.borrow().as_ref() {
            let joined = base.join(p);
            return joined.to_string_lossy().into_owned();
        }
        src.to_string()
    }

    fn get_or_load_image_texture(
        &self,
        _ui: &egui::Ui,
        resolved_src: &str,
    ) -> Option<(egui::TextureHandle, u32, u32)> {
        let is_remote = resolved_src.starts_with("http://") || resolved_src.starts_with("https://");

        // Check if remote images are disabled
        if is_remote && !self.allow_remote_images.get() {
            return None;
        }

        let path = Path::new(resolved_src);
        let embedded = Self::embedded_image_bytes(resolved_src);

        {
            let mut cache = self.image_textures.borrow_mut();
            if let Some(entry) = cache.get(resolved_src) {
                let stale = if embedded.is_some() || is_remote {
                    false // Embedded and remote images don't go stale
                } else {
                    Self::image_source_stale(entry.modified, path)
                };
                if !stale {
                    return Some((entry.texture.clone(), entry.size[0], entry.size[1]));
                }
                cache.remove(resolved_src);
            }
        }

        if self.image_pending.borrow().contains(resolved_src) {
            return None;
        }

        if !self.should_retry_image(resolved_src) {
            return None;
        }

        let source = if let Some(bytes) = embedded {
            ImageLoadSource::Embedded(bytes)
        } else if is_remote {
            ImageLoadSource::Remote(resolved_src.to_string())
        } else {
            if !path.exists() {
                self.note_image_failure(resolved_src);
                return None;
            }
            ImageLoadSource::File(path.to_path_buf())
        };

        let request = ImageLoadRequest {
            key: resolved_src.to_string(),
            source,
        };
        if self.enqueue_image_job(request).is_ok() {
            self.image_pending
                .borrow_mut()
                .insert(resolved_src.to_string());
        }
        None
    }

    fn disk_image_timestamp(path: &Path) -> Option<SystemTime> {
        std::fs::metadata(path).ok()?.modified().ok()
    }

    fn image_source_stale_with_timestamp(
        cached_modified: Option<SystemTime>,
        current: Option<SystemTime>,
    ) -> bool {
        cached_modified != current
    }

    fn image_source_stale(cached_modified: Option<SystemTime>, path: &Path) -> bool {
        if !path.exists() {
            return cached_modified.is_some();
        }
        let current = Self::disk_image_timestamp(path);
        Self::image_source_stale_with_timestamp(cached_modified, current)
    }

    fn store_image_texture(
        &self,
        key: &str,
        texture: egui::TextureHandle,
        size: [u32; 2],
        modified: Option<SystemTime>,
    ) {
        self.image_textures.borrow_mut().insert(
            key.to_string(),
            ImageCacheEntry {
                texture,
                size,
                modified,
                byte_size: ImageCacheEntry::estimate_bytes(size),
            },
        );
    }

    /// Return embedded image bytes for known assets used in sample files
    fn embedded_image_bytes(path: &str) -> Option<&'static [u8]> {
        match path.replace('\\', "/").as_str() {
            // Smiley PNG
            "assets/emoji/1f600.png" => Some(include_bytes!("../assets/emoji/1f600.png")),
            // SVG logo and WEBP sample used by images.md
            "assets/samples/logo.svg" => Some(include_bytes!("../assets/samples/logo.svg")),
            "assets/samples/webp_sample.webp" => {
                Some(include_bytes!("../assets/samples/webp_sample.webp"))
            }
            _ => None,
        }
    }

    /// Find syntax definition for a given language name
    /// Maps common language names to their syntax definitions
    fn find_syntax_for_language(&self, lang: &str) -> Option<&syntect::parsing::SyntaxReference> {
        // Create a mapping of common language names to their syntect equivalents
        let lang_lower = lang.to_lowercase();

        // Try direct name match first
        if let Some(syntax) = self.syntax_set.find_syntax_by_name(&lang_lower) {
            return Some(syntax);
        }

        // Try extension-based matching
        if let Some(syntax) = self.syntax_set.find_syntax_by_extension(&lang_lower) {
            return Some(syntax);
        }

        // Handle common language mappings
        let mapped_lang = match lang_lower.as_str() {
            "rust" => "rs",
            "python" => "py",
            "javascript" => "js",
            "typescript" => "ts",
            "c++" | "cpp" => "cpp",
            "c#" | "csharp" => "cs",
            "shell" | "bash" => "sh",
            "powershell" => "ps1",
            "yaml" => "yml",
            "markdown" => "md",
            "html" => "html",
            "css" => "css",
            "java" => "java",
            "go" => "go",
            "php" => "php",
            "ruby" => "rb",
            "xml" => "xml",
            "json" => "json",
            "sql" => "sql",
            "toml" => "toml",
            _ => &lang_lower,
        };

        // Try mapped extension
        self.syntax_set
            .find_syntax_by_extension(mapped_lang)
            .or_else(|| self.syntax_set.find_syntax_by_name(mapped_lang))
    }

    /// Pre-compute syntax highlighting for a code block.
    /// Returns None if the language is unknown or highlighting fails.
    fn highlight_code(&self, language: Option<&str>, code: &str) -> Option<Vec<HighlightedLine>> {
        let lang = language?;

        // Skip mermaid blocks - they're rendered as diagrams, not highlighted code
        if lang.eq_ignore_ascii_case("mermaid") {
            return None;
        }

        let syntax = self
            .find_syntax_for_language(lang)
            .or_else(|| self.syntax_set.find_syntax_by_first_line(code))?;

        let theme = &self.theme_set.themes[ThemeColors::syntect_theme(self.dark_mode)];
        let mut highlighter = HighlightLines::new(syntax, theme);
        let mut highlighted_lines = Vec::new();

        for line in LinesWithEndings::from(code) {
            let ranges = highlighter.highlight_line(line, &self.syntax_set).ok()?;

            let mut tokens = Vec::new();
            for (style, text) in ranges {
                // Drop newline characters - they're handled by the line structure
                let cleaned = text.replace(['\n', '\r'], "");
                if cleaned.is_empty() {
                    continue;
                }

                let color =
                    Color32::from_rgb(style.foreground.r, style.foreground.g, style.foreground.b);
                let bold = style
                    .font_style
                    .contains(syntect::highlighting::FontStyle::BOLD);
                let italic = style
                    .font_style
                    .contains(syntect::highlighting::FontStyle::ITALIC);

                tokens.push(HighlightedToken {
                    text: cleaned,
                    color,
                    bold,
                    italic,
                });
            }
            highlighted_lines.push(tokens);
        }

        Some(highlighted_lines)
    }

    /// Zoom in (increase font sizes)
    pub fn zoom_in(&mut self) {
        self.font_sizes
            .transform_all(|cur, _default, _min, max| (cur * 1.1).min(max));
        self.clear_table_layout_cache();
    }

    /// Zoom out (decrease font sizes)
    pub fn zoom_out(&mut self) {
        self.font_sizes
            .transform_all(|cur, _default, min, _max| (cur * 0.9).max(min));
        self.clear_table_layout_cache();
    }

    /// Reset zoom to default
    pub fn reset_zoom(&mut self) {
        self.font_sizes = FontSizes::default();
        self.clear_table_layout_cache();
    }

    /// Set zoom scale relative to default font sizes.
    pub fn set_zoom_scale(&mut self, scale: f32) {
        let scale = scale.clamp(0.5, 4.0);
        self.font_sizes
            .transform_all(|_cur, default, min, max| (default * scale).clamp(min, max));
        self.clear_table_layout_cache();
    }
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;
    use crate::SAMPLE_FILES;
    use image::codecs::png::PngEncoder;
    use image::ColorType;
    use image::ImageEncoder;
    use std::env;
    use std::fs;
    use std::str::FromStr;
    use std::sync::Mutex;
    use std::time::Duration;
    use syntect::highlighting::{Color, FontStyle, ScopeSelectors, StyleModifier, ThemeItem};
    use syntect::parsing::SyntaxDefinition;
    use tempfile::tempdir;

    static MERMAID_ENV_LOCK: Mutex<()> = Mutex::new(());

    struct EnvVarGuard {
        key: &'static str,
        previous: Option<String>,
        _lock: std::sync::MutexGuard<'static, ()>,
    }

    impl EnvVarGuard {
        fn set(key: &'static str, value: &str) -> Self {
            let lock = MERMAID_ENV_LOCK.lock().expect("lock mermaid env");
            let previous = env::var(key).ok();
            env::set_var(key, value);
            Self {
                key,
                previous,
                _lock: lock,
            }
        }
    }

    impl Drop for EnvVarGuard {
        fn drop(&mut self) {
            if let Some(prev) = &self.previous {
                env::set_var(self.key, prev);
            } else {
                env::remove_var(self.key);
            }
        }
    }

    #[test]
    fn test_spawn_image_loader_handles_error_path() {
        MarkdownRenderer::force_thread_spawn_error_for_test();
        let (_job_tx, job_rx) = crossbeam_channel::unbounded();
        let (result_tx, _result_rx) = crossbeam_channel::unbounded();
        MarkdownRenderer::spawn_image_loader(job_rx, result_tx);
    }

    struct ForcedRenderActions {
        actions: Vec<&'static str>,
    }

    impl ForcedRenderActions {
        fn new(actions: &[&'static str]) -> Self {
            FORCED_RENDER_ACTIONS.with(|set| {
                let mut set = set.borrow_mut();
                for action in actions {
                    set.insert(*action);
                }
            });
            Self {
                actions: actions.to_vec(),
            }
        }
    }

    impl Drop for ForcedRenderActions {
        fn drop(&mut self) {
            FORCED_RENDER_ACTIONS.with(|set| {
                let mut set = set.borrow_mut();
                for action in &self.actions {
                    set.remove(action);
                }
            });
        }
    }

    struct ForcedTablePolicies;

    impl ForcedTablePolicies {
        fn new(policies: Vec<ColumnPolicy>) -> Self {
            FORCED_TABLE_POLICIES.with(|slot| {
                *slot.borrow_mut() = Some(policies);
            });
            Self
        }
    }

    impl Drop for ForcedTablePolicies {
        fn drop(&mut self) {
            FORCED_TABLE_POLICIES.with(|slot| {
                slot.borrow_mut().take();
            });
        }
    }

    fn paragraph_spans(element: &MarkdownElement) -> Option<&[InlineSpan]> {
        if let MarkdownElement::Paragraph(spans) = element {
            Some(spans)
        } else {
            None
        }
    }

    fn image_span_fields(span: &InlineSpan) -> Option<(&str, &str, Option<&str>)> {
        if let InlineSpan::Image { src, alt, title } = span {
            Some((src.as_str(), alt.as_str(), title.as_deref()))
        } else {
            None
        }
    }

    fn text_span_content(span: &InlineSpan) -> Option<&str> {
        if let InlineSpan::Text(text) = span {
            Some(text.as_str())
        } else {
            None
        }
    }

    fn emoji_fragment_key<'a>(fragment: &'a CellFragment<'a>) -> Option<&'a str> {
        if let CellFragment::Emoji(key) = fragment {
            Some(key.as_str())
        } else {
            None
        }
    }

    fn with_test_ui<F>(f: F)
    where
        F: FnOnce(&egui::Context, &mut egui::Ui),
    {
        let ctx = egui::Context::default();
        ctx.begin_frame(screen_input(1024.0, 768.0));
        egui::CentralPanel::default().show(&ctx, |ui| {
            f(&ctx, ui);
        });
        let _ = ctx.end_frame();
    }

    fn wait_for_image(
        renderer: &MarkdownRenderer,
        ctx: &egui::Context,
        ui: &egui::Ui,
        path: &str,
    ) -> Option<(egui::TextureHandle, u32, u32)> {
        // Allow extra time for async image loading during tests (up to 10s total).
        wait_for_image_with_limit(renderer, ctx, ui, path, 200)
    }

    fn wait_for_image_with_limit(
        renderer: &MarkdownRenderer,
        ctx: &egui::Context,
        ui: &egui::Ui,
        path: &str,
        max_iters: usize,
    ) -> Option<(egui::TextureHandle, u32, u32)> {
        for _ in 0..max_iters {
            if let Some(loaded) = renderer.get_or_load_image_texture(ui, path) {
                return Some(loaded);
            }
            renderer.poll_image_results(ctx);
            std::thread::sleep(Duration::from_millis(50));
        }
        renderer.get_or_load_image_texture(ui, path)
    }

    fn run_frame_with_input<F>(ctx: &egui::Context, input: egui::RawInput, f: F)
    where
        F: FnOnce(&egui::Context, &mut egui::Ui),
    {
        ctx.begin_frame(input);
        egui::CentralPanel::default().show(ctx, |ui| {
            f(ctx, ui);
        });
        let _ = ctx.end_frame();
    }

    fn input_with_click(pos: egui::Pos2, button: egui::PointerButton) -> egui::RawInput {
        let mut input = screen_input(320.0, 240.0);
        input.events.push(egui::Event::PointerMoved(pos));
        input.events.push(egui::Event::PointerButton {
            pos,
            button,
            pressed: true,
            modifiers: egui::Modifiers::default(),
        });
        input.events.push(egui::Event::PointerButton {
            pos,
            button,
            pressed: false,
            modifiers: egui::Modifiers::default(),
        });
        input
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

    /// Creates a test ImageCacheEntry with the given pixel size inside a with_test_ui context.
    fn make_test_cache_entry(size: [usize; 2]) -> ImageCacheEntry {
        let mut entry_slot = None;
        with_test_ui(|ctx, _ui| {
            let tex = ctx.load_texture(
                "test_entry",
                egui::ColorImage::new(size, Color32::WHITE),
                egui::TextureOptions::LINEAR,
            );
            entry_slot = Some(ImageCacheEntry {
                texture: tex,
                size: [size[0] as u32, size[1] as u32],
                modified: None,
                byte_size: ImageCacheEntry::estimate_bytes([size[0] as u32, size[1] as u32]),
            });
        });
        entry_slot.expect("texture")
    }

    /// Creates an egui::RawInput with the specified screen dimensions.
    fn screen_input(width: f32, height: f32) -> egui::RawInput {
        egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(width, height),
            )),
            ..Default::default()
        }
    }

    /// Returns the rocket emoji character from the shortcode map.
    fn rocket_emoji() -> &'static str {
        crate::emoji_catalog::shortcode_map()
            .get(":rocket:")
            .expect("rocket shortcode")
    }

    /// Asserts that inline code pipes were escaped (sentinel is present).
    fn assert_pipes_escaped(prepared: &str) {
        let expected = format!("`a{}b{}c`", PIPE_SENTINEL, PIPE_SENTINEL);
        assert!(
            prepared.contains(&expected),
            "expected pipe sentinel in: {}",
            prepared
        );
    }

    /// Asserts that inline code pipes were NOT escaped (original backtick content preserved).
    fn assert_pipes_preserved(prepared: &str) {
        assert!(
            prepared.contains("`a|b|c`"),
            "expected preserved pipes in: {}",
            prepared
        );
        assert!(
            !prepared.contains(PIPE_SENTINEL),
            "unexpected pipe sentinel in: {}",
            prepared
        );
    }

    /// Creates a ColumnSpec with Resizable policy for test use.
    fn resizable_spec(col: usize, name: &str, min: f32, preferred: f32) -> ColumnSpec {
        ColumnSpec::new(
            col,
            name,
            ColumnPolicy::Resizable {
                min,
                preferred,
                clip: false,
            },
            None,
        )
    }

    /// Creates a ColumnSpec with Fixed policy for test use.
    fn fixed_spec(col: usize, name: &str, width: f32) -> ColumnSpec {
        ColumnSpec::new(col, name, ColumnPolicy::Fixed { width, clip: false }, None)
    }

    #[test]
    fn test_image_cache_get_missing_and_insert_empty_order() {
        let mut cache = ImageCache::new(1);
        assert!(cache.get("missing").is_none());

        let entry = make_test_cache_entry([1, 1]);
        cache.entries.insert("a".to_string(), entry.clone());
        cache.order.clear();
        cache.insert("b".to_string(), entry);
        assert!(cache.contains_key("b"));
    }

    #[test]
    fn test_image_cache_insert_existing_key_updates_entry() {
        let mut cache = ImageCache::new(2);
        let entry = make_test_cache_entry([1, 1]);
        cache.insert("a".to_string(), entry);

        let entry2 = make_test_cache_entry([2, 2]);
        cache.insert("a".to_string(), entry2);

        let stored = cache.get("a").expect("stored");
        assert_eq!(stored.size, [2, 2]);
    }

    #[test]
    fn test_image_cache_evicts_oldest_entry() {
        let mut cache = ImageCache::new(1);
        let entry = make_test_cache_entry([1, 1]);
        cache.insert("a".to_string(), entry.clone());
        cache.insert("b".to_string(), entry);
        assert!(cache.contains_key("b"));
        assert!(!cache.contains_key("a"));
    }

    #[test]
    fn test_image_cache_tracks_memory_and_evicts_on_size_limit() {
        // Create a cache with high entry limit but low memory limit
        let mut cache = ImageCache::new(100);
        cache.max_bytes = 32; // 8 pixels worth (2x2 RGBA = 16 bytes each)

        let entry = make_test_cache_entry([2, 2]);

        // Insert first entry (16 bytes)
        cache.insert("a".to_string(), entry.clone());
        assert_eq!(cache.current_bytes(), 16);
        assert!(cache.contains_key("a"));

        // Insert second entry (16 bytes) - should fit within 32 byte limit
        cache.insert("b".to_string(), entry.clone());
        assert_eq!(cache.current_bytes(), 32);
        assert!(cache.contains_key("a"));
        assert!(cache.contains_key("b"));

        // Insert third entry - should evict "a" due to memory limit
        cache.insert("c".to_string(), entry);
        // Memory should still be at 32 bytes (evicted "a", added "c")
        assert_eq!(cache.current_bytes(), 32);
        assert!(!cache.contains_key("a")); // Evicted due to memory limit
        assert!(cache.contains_key("b"));
        assert!(cache.contains_key("c"));
    }

    #[test]
    fn test_image_cache_remove_decrements_bytes() {
        let mut cache = ImageCache::new(10);
        let entry = make_test_cache_entry([4, 4]);
        cache.insert("x".to_string(), entry);
        assert_eq!(cache.current_bytes(), 64);

        cache.remove("x");
        assert_eq!(cache.current_bytes(), 0);
        assert!(!cache.contains_key("x"));
    }

    // --- LruCache Tests ---

    #[test]
    fn test_lru_cache_basic_insert_and_get() {
        let mut cache: LruCache<String, i32> = LruCache::new(10);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        cache.insert("c".to_string(), 3);

        assert_eq!(cache.get(&"a".to_string()), Some(1));
        assert_eq!(cache.get(&"b".to_string()), Some(2));
        assert_eq!(cache.get(&"c".to_string()), Some(3));
        assert_eq!(cache.get(&"d".to_string()), None);
        assert_eq!(cache.len(), 3);
    }

    #[test]
    fn test_lru_cache_evicts_oldest_entry() {
        let mut cache: LruCache<String, i32> = LruCache::new(2);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        // Cache is now full at capacity 2

        // Insert third entry, should evict "a" (oldest)
        cache.insert("c".to_string(), 3);

        assert!(!cache.contains_key(&"a".to_string())); // "a" evicted
        assert!(cache.contains_key(&"b".to_string()));
        assert!(cache.contains_key(&"c".to_string()));
        assert_eq!(cache.len(), 2);
    }

    #[test]
    fn test_lru_cache_touch_updates_order() {
        let mut cache: LruCache<String, i32> = LruCache::new(2);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);

        // Touch "a" to make it recently used
        cache.touch(&"a".to_string());

        // Insert third entry, should evict "b" now (oldest after touch)
        cache.insert("c".to_string(), 3);

        assert!(cache.contains_key(&"a".to_string())); // "a" preserved by touch
        assert!(!cache.contains_key(&"b".to_string())); // "b" evicted
        assert!(cache.contains_key(&"c".to_string()));
    }

    #[test]
    fn test_lru_cache_insert_existing_updates_value_and_order() {
        let mut cache: LruCache<String, i32> = LruCache::new(2);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);

        // Re-insert "a" with new value
        cache.insert("a".to_string(), 100);

        assert_eq!(cache.get(&"a".to_string()), Some(100));

        // Insert third entry, should evict "b" (oldest after "a" was updated)
        cache.insert("c".to_string(), 3);

        assert!(cache.contains_key(&"a".to_string()));
        assert!(!cache.contains_key(&"b".to_string())); // "b" evicted
        assert!(cache.contains_key(&"c".to_string()));
    }

    #[test]
    fn test_lru_cache_remove() {
        let mut cache: LruCache<String, i32> = LruCache::new(10);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);

        cache.remove(&"a".to_string());

        assert!(!cache.contains_key(&"a".to_string()));
        assert!(cache.contains_key(&"b".to_string()));
        assert_eq!(cache.len(), 1);

        // Removing non-existent key should be safe
        cache.remove(&"nonexistent".to_string());
        assert_eq!(cache.len(), 1);
    }

    #[test]
    fn test_lru_cache_clear() {
        let mut cache: LruCache<String, i32> = LruCache::new(10);
        cache.insert("a".to_string(), 1);
        cache.insert("b".to_string(), 2);
        cache.insert("c".to_string(), 3);

        cache.clear();

        assert_eq!(cache.len(), 0);
        assert!(!cache.contains_key(&"a".to_string()));
        assert!(!cache.contains_key(&"b".to_string()));
        assert!(!cache.contains_key(&"c".to_string()));
    }

    #[test]
    fn test_lru_cache_capacity_enforcement() {
        let mut cache: LruCache<String, i32> = LruCache::new(3);
        for i in 0..10 {
            cache.insert(format!("key{}", i), i);
        }

        // Only last 3 entries should remain
        assert_eq!(cache.len(), 3);
        assert!(cache.contains_key(&"key7".to_string()));
        assert!(cache.contains_key(&"key8".to_string()));
        assert!(cache.contains_key(&"key9".to_string()));
        assert!(!cache.contains_key(&"key0".to_string()));
        assert!(!cache.contains_key(&"key6".to_string()));
    }

    #[test]
    fn test_lru_cache_touch_nonexistent_key() {
        let mut cache: LruCache<String, i32> = LruCache::new(10);
        cache.insert("a".to_string(), 1);

        // Touching non-existent key should be safe (no-op)
        cache.touch(&"nonexistent".to_string());

        assert_eq!(cache.len(), 1);
        assert!(cache.contains_key(&"a".to_string()));
    }

    #[test]
    fn test_lru_cache_zero_capacity() {
        let mut cache: LruCache<String, i32> = LruCache::new(0);
        cache.insert("a".to_string(), 1);

        // With zero capacity, nothing should be stored
        assert_eq!(cache.len(), 0);
        assert!(!cache.contains_key(&"a".to_string()));
    }

    #[test]
    fn test_list_marker_info_rejects_empty_and_bad_numeric() {
        assert!(MarkdownRenderer::list_marker_info("").is_none());
        assert!(MarkdownRenderer::list_marker_info("1 abc").is_none());
        assert!(MarkdownRenderer::list_marker_info("   ").is_none());
    }

    #[test]
    fn test_list_marker_info_rejects_tabs_and_missing_whitespace() {
        assert!(MarkdownRenderer::list_marker_info("\t- item").is_none());
        assert!(MarkdownRenderer::list_marker_info("1.item").is_none());
        assert!(MarkdownRenderer::list_marker_info("1)item").is_none());
        assert!(MarkdownRenderer::list_marker_info("-item").is_none());
    }

    #[test]
    fn test_list_marker_info_rejects_marker_only() {
        assert!(MarkdownRenderer::list_marker_info("-").is_none());
        assert!(MarkdownRenderer::list_marker_info("1.").is_none());
    }

    #[test]
    fn test_parse_image_with_title_sets_title() {
        let renderer = MarkdownRenderer::new();
        let parsed = renderer
            .parse("![alt](path/to/img.png \"Title\")")
            .expect("parse");
        let spans = paragraph_spans(&parsed[0]).expect("paragraph spans");
        let (_src, _alt, title) = image_span_fields(&spans[0]).expect("image span");
        assert_eq!(title, Some("Title"));
    }

    #[test]
    fn test_is_external_url_variants() {
        assert!(MarkdownRenderer::is_external_url("http://example.com"));
        assert!(MarkdownRenderer::is_external_url("https://example.com"));
        assert!(MarkdownRenderer::is_external_url("mailto:test@example.com"));
        assert!(MarkdownRenderer::is_external_url("www.example.com"));
        assert!(!MarkdownRenderer::is_external_url("local/path.png"));
    }

    #[test]
    fn test_resolve_image_path_keeps_remote() {
        let renderer = MarkdownRenderer::new();
        let src = "https://example.com/image.png";
        let http_src = "http://example.com/image.png";
        assert_eq!(renderer.resolve_image_path(src), src);
        assert_eq!(renderer.resolve_image_path(http_src), http_src);
        let data_src = "data:image/png;base64,AAAA";
        assert_eq!(renderer.resolve_image_path(data_src), data_src);
    }

    #[test]
    fn test_compute_table_id_includes_base_dir() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![vec![InlineSpan::Text("A".to_string())]];
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let alignments = vec![Alignment::Left];
        let base_id = renderer.compute_table_id(&headers, &rows, &alignments, 0);

        *renderer.base_dir.borrow_mut() = Some(PathBuf::from("C:\\tmp"));
        let with_base = renderer.compute_table_id(&headers, &rows, &alignments, 0);
        assert_ne!(base_id, with_base);
    }

    #[test]
    fn test_render_table_early_return_on_empty_headers() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            renderer.render_table(ui, &[], &[], &[]);
        });
    }

    #[test]
    fn test_extend_table_rect_handles_nan() {
        let mut target = Some(egui::Rect::from_min_size(
            egui::pos2(0.0, 0.0),
            egui::vec2(10.0, 10.0),
        ));
        let nan_rect = egui::Rect::from_min_size(egui::pos2(f32::NAN, 0.0), egui::vec2(1.0, 1.0));
        MarkdownRenderer::extend_table_rect(&mut target, nan_rect);
        let stored = target.expect("target");
        assert!(!stored.min.x.is_nan());

        let mut target_none = None;
        MarkdownRenderer::extend_table_rect(
            &mut target_none,
            egui::Rect::from_min_size(egui::pos2(5.0, 6.0), egui::vec2(2.0, 2.0)),
        );
        assert!(target_none.is_some());
    }

    #[test]
    fn test_extend_table_rect_rejects_nan_components() {
        let base = egui::Rect::from_min_size(egui::pos2(1.0, 2.0), egui::vec2(3.0, 4.0));
        let mut target = Some(base);
        let nan_min_y = egui::Rect::from_min_size(egui::pos2(0.0, f32::NAN), egui::vec2(1.0, 1.0));
        MarkdownRenderer::extend_table_rect(&mut target, nan_min_y);
        assert_eq!(target, Some(base));

        let mut target = Some(base);
        let nan_max_x = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(f32::NAN, 1.0));
        MarkdownRenderer::extend_table_rect(&mut target, nan_max_x);
        assert_eq!(target, Some(base));

        let mut target = Some(base);
        let nan_max_y = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(1.0, f32::NAN));
        MarkdownRenderer::extend_table_rect(&mut target, nan_max_y);
        assert_eq!(target, Some(base));
    }

    #[test]
    fn test_persist_resizable_widths_returns_on_empty() {
        let renderer = MarkdownRenderer::new();
        renderer.persist_resizable_widths(1, &[], &[]);
    }

    #[test]
    fn test_persist_resizable_widths_returns_on_empty_widths() {
        let renderer = MarkdownRenderer::new();
        let spec = resizable_spec(0, "A", 20.0, 80.0);
        let table_id = 7u64;
        renderer.persist_resizable_widths(table_id, &[spec], &[]);
        let metrics = renderer.table_metrics.borrow();
        assert!(metrics.entry(table_id).is_none());
    }

    #[test]
    fn test_link_at_pointer_outside_rect_returns_none() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Link {
            text: "Link".to_string(),
            url: "https://example.com".to_string(),
        }];
        let ctx = egui::Context::default();
        let mut input = screen_input(320.0, 240.0);
        input
            .events
            .push(egui::Event::PointerMoved(egui::pos2(200.0, 200.0)));
        ctx.begin_frame(input);
        let mut found = false;
        egui::CentralPanel::default().show(&ctx, |ui| {
            let response = ui.label("anchor");
            let build = renderer.build_layout_job(ui.style(), &spans, 200.0, false, Align::LEFT);
            let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
            found = renderer
                .link_at_pointer(&response, &galley, &build, egui::pos2(0.0, 0.0))
                .is_some();
        });
        let _ = ctx.end_frame();
        assert!(!found);
    }

    #[test]
    fn test_spawn_image_loader_missing_file_reports_failed() {
        let (job_tx, job_rx) = bounded(1);
        let (result_tx, result_rx) = bounded(1);
        MarkdownRenderer::spawn_image_loader(job_rx, result_tx);

        let dir = tempdir().expect("temp dir");
        let path = dir.path().join("missing.png");
        job_tx
            .send(ImageLoadRequest {
                key: "missing".to_string(),
                source: ImageLoadSource::File(path),
            })
            .expect("send");
        let result = result_rx
            .recv_timeout(Duration::from_secs(3))
            .expect("result");
        assert!(matches!(
            result,
            ImageLoadResult::Failed { ref key } if key == "missing"
        ));
    }

    #[test]
    fn test_spawn_image_loader_embedded_invalid_bytes_reports_failed() {
        let (job_tx, job_rx) = bounded(1);
        let (result_tx, result_rx) = bounded(1);
        MarkdownRenderer::spawn_image_loader(job_rx, result_tx);

        job_tx
            .send(ImageLoadRequest {
                key: "bad-embedded".to_string(),
                source: ImageLoadSource::Embedded(b"not an image"),
            })
            .expect("send");
        let result = result_rx
            .recv_timeout(Duration::from_secs(8))
            .expect("result");
        assert!(matches!(
            result,
            ImageLoadResult::Failed { ref key } if key == "bad-embedded"
        ));
    }

    #[test]
    fn test_spawn_image_loader_file_invalid_bytes_reports_failed() -> Result<()> {
        let (job_tx, job_rx) = bounded(1);
        let (result_tx, result_rx) = bounded(1);
        MarkdownRenderer::spawn_image_loader(job_rx, result_tx);

        let dir = tempdir()?;
        let path = dir.path().join("bad.png");
        std::fs::write(&path, b"not an image")?;
        job_tx
            .send(ImageLoadRequest {
                key: "bad-file".to_string(),
                source: ImageLoadSource::File(path),
            })
            .expect("send");
        let result = result_rx
            .recv_timeout(Duration::from_secs(3))
            .expect("result");
        assert!(matches!(
            result,
            ImageLoadResult::Failed { ref key } if key == "bad-file"
        ));
        Ok(())
    }

    #[test]
    fn test_spawn_image_loader_file_read_error_reports_failed() -> Result<()> {
        let (job_tx, job_rx) = bounded(1);
        let (result_tx, result_rx) = bounded(1);
        MarkdownRenderer::spawn_image_loader(job_rx, result_tx);

        let dir = tempdir()?;
        let bad_path = dir.path().join("read_error_dir");
        std::fs::create_dir_all(&bad_path)?;
        job_tx
            .send(ImageLoadRequest {
                key: "read-error".to_string(),
                source: ImageLoadSource::File(bad_path),
            })
            .expect("send");
        let result = result_rx
            .recv_timeout(Duration::from_secs(1))
            .expect("result");
        assert!(matches!(
            result,
            ImageLoadResult::Failed { ref key } if key == "read-error"
        ));
        Ok(())
    }

    #[test]
    fn test_env_var_guard_removes_unset_value() {
        let key = "MDMDVIEW_TEST_ENV_GUARD";
        env::remove_var(key);
        {
            let _guard = EnvVarGuard::set(key, "value");
            assert_eq!(env::var(key).ok().as_deref(), Some("value"));
        }
        assert!(env::var(key).is_err());
    }

    #[test]
    fn test_parse_forced_error_returns_err_once() {
        let renderer = MarkdownRenderer::new();
        force_parse_error_once();
        assert!(renderer.parse("hello").is_err());
        assert!(renderer.parse("hello").is_ok());
    }

    #[test]
    fn test_fence_helpers_cover_branches() {
        assert_eq!(MarkdownRenderer::fence_start("```"), Some(('`', 3)));
        assert_eq!(MarkdownRenderer::fence_start("``"), None);
        assert_eq!(MarkdownRenderer::fence_start("~~~"), Some(('~', 3)));
        assert!(MarkdownRenderer::is_fence_end("```", '`', 3));
        assert!(!MarkdownRenderer::is_fence_end("``", '`', 3));
        assert!(MarkdownRenderer::is_fence_end("   ```", '`', 3));
    }

    #[test]
    fn test_restore_pipe_sentinel_round_trip() {
        let input = format!("a{}b", PIPE_SENTINEL);
        assert_eq!(MarkdownRenderer::restore_pipe_sentinel(&input), "a|b");
        assert_eq!(MarkdownRenderer::restore_pipe_sentinel("a|b"), "a|b");
    }

    #[test]
    fn test_list_marker_info_numeric_punctuation() {
        let (content, _indent, _content_indent) =
            MarkdownRenderer::list_marker_info("1. item").expect("dot marker");
        assert_eq!(content.trim_start(), "item");
        let (content, _indent, _content_indent) =
            MarkdownRenderer::list_marker_info("2) item").expect("paren marker");
        assert_eq!(content.trim_start(), "item");
    }

    #[test]
    fn test_table_line_info_blockquote_levels() {
        let (level, rest) = MarkdownRenderer::table_line_info("  >\t| a |");
        assert_eq!(level, 1);
        assert!(rest.trim_start().starts_with('|'));

        let (level, rest) = MarkdownRenderer::table_line_info("> > | b |");
        assert_eq!(level, 2);
        assert!(rest.trim_start().starts_with('|'));
    }

    #[test]
    fn test_strip_indent_columns_indent_zero_returns_line() {
        assert_eq!(
            MarkdownRenderer::strip_indent_columns("content", 0),
            Some("content")
        );
    }

    #[test]
    fn test_parent_list_indent_for_line_zero_index_returns_none() {
        let lines = vec!["- item"];
        let (level, rest) = MarkdownRenderer::table_line_info(lines[0]);
        assert!(MarkdownRenderer::parent_list_indent_for_line(&lines, 0, level, rest).is_none());
    }

    #[test]
    fn test_should_retry_image_backoff_respected() {
        let renderer = MarkdownRenderer::new();
        renderer.image_failures.borrow_mut().insert(
            "recent".to_string(),
            ImageFailure {
                last_attempt: std::time::Instant::now(),
            },
        );
        assert!(!renderer.should_retry_image("recent"));

        renderer.image_failures.borrow_mut().insert(
            "old".to_string(),
            ImageFailure {
                last_attempt: std::time::Instant::now()
                    - IMAGE_FAILURE_BACKOFF
                    - Duration::from_millis(1),
            },
        );
        assert!(renderer.should_retry_image("old"));
    }

    #[test]
    fn test_extend_table_rect_accepts_valid_rect() {
        let mut target = None;
        let rect = egui::Rect::from_min_size(egui::pos2(1.0, 2.0), egui::vec2(3.0, 4.0));
        MarkdownRenderer::extend_table_rect(&mut target, rect);
        assert_eq!(target, Some(rect));

        let rect2 = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(10.0, 10.0));
        MarkdownRenderer::extend_table_rect(&mut target, rect2);
        let merged = target.expect("merged rect");
        assert!(merged.contains(rect.min));
        assert!(merged.contains(rect2.max));
    }

    #[test]
    fn test_render_code_block_with_none_language() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            renderer.render_code_block(ui, None, "fn main() {}", None);
        });
    }

    #[test]
    fn test_render_action_triggered_forced_paths() {
        let _actions = ForcedRenderActions::new(&["forced_action"]);
        assert!(render_action_triggered(false, "forced_action"));
        assert!(!render_action_triggered(false, "other_action"));
        assert!(render_action_triggered(true, "other_action"));
    }

    #[test]
    fn test_markdown_renderer_creation() {
        let renderer = MarkdownRenderer::new();
        assert_eq!(renderer.font_sizes.body, 14.0);
        assert_eq!(renderer.font_sizes.h1, 28.0);
    }

    #[test]
    fn test_markdown_renderer_default_uses_defaults() {
        let renderer = MarkdownRenderer::default();
        let defaults = FontSizes::default();
        assert_eq!(renderer.font_sizes.body, defaults.body);
        assert_eq!(renderer.font_sizes.code, defaults.code);
    }

    #[test]
    fn test_superscript_expansion_basic() {
        let s = "5^th^ and m^2^";
        let out = MarkdownRenderer::expand_superscripts(s);
        assert!(out.contains("5ᵗʰ"));
        assert!(out.contains("m²"));
    }

    #[test]
    fn test_superscript_expansion_allows_plus_symbol() {
        let out = MarkdownRenderer::expand_superscripts("x^+^");
        assert!(out.contains('\u{207a}'));
    }

    #[test]
    fn test_superscript_expansion_single_caret() {
        // Test that single carets (not paired) are left as-is
        let s = "2^32 = 4,294,967,296";
        let out = MarkdownRenderer::expand_superscripts(s);
        assert_eq!(out, "2^32 = 4,294,967,296");

        // Test the problematic line from the bug report
        let problematic = "A 32-bit address bus would provide 2^32 = 4,294,967,296 bytes (4 GB) of addressable memory.";
        let fixed = MarkdownRenderer::expand_superscripts(problematic);
        assert_eq!(fixed, problematic);

        // Test mixed cases
        let mixed = "Use 2^32 for math and 5^th^ for ordinal";
        let result = MarkdownRenderer::expand_superscripts(mixed);
        assert!(result.contains("2^32"));
        assert!(result.contains("5ᵗʰ"));
    }

    #[test]
    fn test_superscript_expansion_rejects_invalid_sequences() {
        let s = "2^ab$^ and ^^";
        let out = MarkdownRenderer::expand_superscripts(s);
        assert_eq!(out, s);
        let no_close = "10^abc";
        let out = MarkdownRenderer::expand_superscripts(no_close);
        assert_eq!(out, no_close);
    }

    #[test]
    fn test_font_sizes_default() {
        let sizes = FontSizes::default();
        assert_eq!(sizes.body, 14.0);
        assert_eq!(sizes.h1, 28.0);
        assert_eq!(sizes.code, 12.0);
    }

    #[test]
    fn test_zoom_functionality() {
        let mut renderer = MarkdownRenderer::new();
        let original_body = renderer.font_sizes.body;

        renderer.zoom_in();
        assert!(renderer.font_sizes.body > original_body);

        renderer.zoom_out();
        assert!(renderer.font_sizes.body < original_body * 1.1);

        renderer.reset_zoom();
        assert_eq!(renderer.font_sizes.body, original_body);
    }

    #[test]
    fn test_set_zoom_scale() {
        let mut renderer = MarkdownRenderer::new();

        renderer.set_zoom_scale(1.5);
        assert!((renderer.font_sizes.body - 21.0).abs() < 0.1);

        renderer.set_zoom_scale(10.0);
        assert_eq!(renderer.font_sizes.body, 32.0);

        renderer.set_zoom_scale(0.1);
        assert_eq!(renderer.font_sizes.body, 8.0);
    }

    #[test]
    fn font_size_change_does_not_repersist_stale_widths() {
        let mut renderer = MarkdownRenderer::new();
        let table_id = 7u64;
        let specs = vec![resizable_spec(0, "A", 20.0, 100.0)];
        let widths = vec![100.0f32];

        // Initial persist at default font size.
        renderer.persist_resizable_widths(table_id, &specs, &widths);
        let policy_hash = specs[0].policy_hash;
        {
            let metrics = renderer.table_metrics.borrow();
            let entry = metrics.entry(table_id).expect("entry created");
            assert_eq!(entry.persisted_width(policy_hash), Some(100.0));
            assert_eq!(entry.persisted_font_size, Some(renderer.font_sizes.body));
        }

        // Simulate zoom: font size changes but widths captured are still the old ones.
        renderer.font_sizes.body = 16.0;
        renderer.persist_resizable_widths(table_id, &specs, &widths);
        {
            let metrics = renderer.table_metrics.borrow();
            let entry = metrics.entry(table_id).expect("entry exists");
            // Cleared and NOT re-saved in the same frame.
            assert_eq!(entry.persisted_width(policy_hash), None);
            assert_eq!(entry.persisted_font_size, Some(16.0));
        }

        // Next frame with new layout widths should persist again.
        let new_widths = vec![80.0f32];
        renderer.persist_resizable_widths(table_id, &specs, &new_widths);
        let metrics = renderer.table_metrics.borrow();
        let entry = metrics.entry(table_id).expect("entry exists");
        assert_eq!(entry.persisted_width(policy_hash), Some(80.0));
        assert_eq!(entry.persisted_font_size, Some(16.0));
    }

    #[test]
    fn highlight_phrase_keeps_persisted_table_widths() {
        let renderer = MarkdownRenderer::new();
        let table_id = 11u64;
        let specs = vec![resizable_spec(0, "A", 20.0, 100.0)];
        let widths = vec![120.0f32];
        let policy_hash = specs[0].policy_hash;

        renderer.persist_resizable_widths(table_id, &specs, &widths);
        renderer.set_highlight_phrase(Some("alpha"));
        renderer.set_highlight_phrase(Some("beta"));

        let metrics = renderer.table_metrics.borrow();
        let entry = metrics.entry(table_id).expect("entry exists");
        assert_eq!(entry.persisted_width(policy_hash), Some(120.0));
    }

    #[test]
    fn test_tight_list_inline_code_and_styles() {
        let renderer = MarkdownRenderer::new();
        let md = "Intro\n\n- Use `code` and **bold** and *italic* and ~~strike~~\n";
        let parsed = renderer.parse(md).expect("parse ok");
        let (ordered, items) = parsed
            .iter()
            .find_map(|el| {
                if let MarkdownElement::List { ordered, items } = el {
                    Some((*ordered, items))
                } else {
                    None
                }
            })
            .expect("list element");
        assert!(!ordered);
        assert_eq!(items.len(), 1);
        let mut blocks = Vec::new();
        blocks.push(MarkdownElement::HorizontalRule);
        blocks.extend(items[0].blocks.clone());
        let spans = blocks
            .iter()
            .find_map(|block| {
                if let MarkdownElement::Paragraph(spans) = block {
                    Some(spans)
                } else {
                    None
                }
            })
            .expect("paragraph block");
        let mut saw_code = false;
        let mut saw_strong = false;
        let mut saw_emphasis = false;
        let mut saw_strike = false;
        let mut saw_other = false;
        for span in spans {
            match span {
                InlineSpan::Code(_) => saw_code = true,
                InlineSpan::Strong(_) => saw_strong = true,
                InlineSpan::Emphasis(_) => saw_emphasis = true,
                InlineSpan::Strikethrough(_) => saw_strike = true,
                _ => saw_other = true,
            }
        }
        assert!(saw_code);
        assert!(saw_strong);
        assert!(saw_emphasis);
        assert!(saw_strike);
        assert!(saw_other);
    }

    #[test]
    fn test_expand_shortcodes_basic() {
        let rocket = MarkdownRenderer::expand_shortcodes(":rocket:");
        assert_ne!(rocket, ":rocket:");
        assert!(crate::emoji_catalog::image_bytes_for(&rocket).is_some());

        let tada = MarkdownRenderer::expand_shortcodes(":tada:");
        assert_ne!(tada, ":tada:");
        assert!(crate::emoji_catalog::image_bytes_for(&tada).is_some());

        assert_eq!(
            MarkdownRenderer::expand_shortcodes("Hello :tada:!"),
            format!("Hello {}!", tada)
        );
    }

    #[test]
    fn test_expand_shortcodes_unknown_code_keeps_text() {
        let input = "Unknown :notacode: stays the same.";
        let out = MarkdownRenderer::expand_shortcodes(input);
        assert_eq!(out, input);
    }

    #[test]
    fn test_fix_unicode_chars_normalizes_basic_cases() {
        let renderer = MarkdownRenderer::new();
        let input = "A\u{00A0}B\u{2013}C";
        let normalized = renderer.normalize_text_for_test(input);
        assert_eq!(normalized, "A B-C");

        let arrows = renderer.normalize_text_for_test("← → ↑ ↓");
        assert_eq!(arrows, "<- -> ^ v");

        let untouched = renderer.normalize_text_for_test("Plain text");
        assert_eq!(untouched, "Plain text");
    }

    #[test]
    fn test_elements_to_plain_text_basic() {
        let elements = vec![
            MarkdownElement::Header {
                level: 1,
                spans: vec![InlineSpan::Text("Test Header".to_string())],
                id: "test-header".to_string(),
            },
            MarkdownElement::Paragraph(vec![
                InlineSpan::Text("This is a ".to_string()),
                InlineSpan::Strong("bold".to_string()),
                InlineSpan::Text(" paragraph.".to_string()),
            ]),
        ];

        let plain_text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert!(plain_text.contains("Test Header"));
        assert!(plain_text.contains("This is a bold paragraph."));
    }

    #[test]
    fn test_elements_to_plain_text_code_block() {
        let elements = vec![MarkdownElement::CodeBlock {
            language: Some("rust".to_string()),
            text: "fn main() {\n    println!(\"Hello\");\n}".to_string(),
            highlighted: None,
        }];

        let plain_text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert_eq!(plain_text, "fn main() {\n    println!(\"Hello\");\n}");
    }

    #[test]
    fn test_elements_to_plain_text_with_links() {
        let elements = vec![MarkdownElement::Paragraph(vec![
            InlineSpan::Text("Visit ".to_string()),
            InlineSpan::Link {
                text: "GitHub".to_string(),
                url: "https://github.com".to_string(),
            },
            InlineSpan::Text(" for more.".to_string()),
        ])];

        let plain_text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert_eq!(plain_text, "Visit GitHub for more.");
    }

    #[test]
    fn test_elements_to_plain_text_horizontal_rule_after_text() {
        let elements = vec![
            MarkdownElement::Paragraph(vec![InlineSpan::Text("Hello".to_string())]),
            MarkdownElement::HorizontalRule,
        ];
        let plain_text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert_eq!(plain_text, "Hello\n---\n");
    }

    #[test]
    fn test_elements_to_plain_text_mixed_blocks_adds_newlines() {
        let elements = vec![
            MarkdownElement::Paragraph(vec![InlineSpan::Text("First".to_string())]),
            MarkdownElement::List {
                ordered: false,
                items: vec![ListItem {
                    blocks: vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
                        "List".to_string(),
                    )])],
                }],
            },
            MarkdownElement::Quote {
                depth: 1,
                blocks: vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
                    "Quote".to_string(),
                )])],
            },
            MarkdownElement::Table {
                headers: vec![
                    vec![InlineSpan::Text("H1".to_string())],
                    vec![InlineSpan::Text("H2".to_string())],
                ],
                rows: vec![vec![vec![InlineSpan::Text("R1".to_string())]]],
                alignments: vec![Alignment::Left, Alignment::Left],
            },
            MarkdownElement::HorizontalRule,
        ];
        let plain_text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert_eq!(plain_text, "First\nList\nQuote\nH1\nH2\nR1\n---\n");
    }

    #[test]
    fn test_elements_to_plain_text_list_only_starts_without_newline() {
        let elements = vec![MarkdownElement::List {
            ordered: false,
            items: vec![ListItem {
                blocks: vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
                    "Item".to_string(),
                )])],
            }],
        }];
        let plain_text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert_eq!(plain_text, "Item");
    }

    #[test]
    fn test_elements_to_plain_text_quote_only_starts_without_newline() {
        let elements = vec![MarkdownElement::Quote {
            depth: 1,
            blocks: vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
                "Quote".to_string(),
            )])],
        }];
        let plain_text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert_eq!(plain_text, "Quote");
    }

    #[test]
    fn test_elements_to_plain_text_table_headers_only_starts_without_newline() {
        let elements = vec![MarkdownElement::Table {
            headers: vec![vec![InlineSpan::Text("H".to_string())]],
            rows: Vec::new(),
            alignments: vec![Alignment::Left],
        }];
        let plain_text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert_eq!(plain_text, "H");
    }

    #[test]
    fn test_elements_to_plain_text_table_rows_only_starts_without_newline() {
        let elements = vec![MarkdownElement::Table {
            headers: Vec::new(),
            rows: vec![vec![vec![InlineSpan::Text("R".to_string())]]],
            alignments: vec![Alignment::Left],
        }];
        let plain_text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert_eq!(plain_text, "R");
    }

    #[test]
    fn test_spans_plain_text_image_title_without_alt() {
        let spans = vec![
            InlineSpan::Image {
                src: "img.png".to_string(),
                alt: "".to_string(),
                title: Some("Title".to_string()),
            },
            InlineSpan::Image {
                src: "img.png".to_string(),
                alt: "Alt".to_string(),
                title: Some("Caption".to_string()),
            },
            InlineSpan::Image {
                src: "img.png".to_string(),
                alt: "".to_string(),
                title: Some("".to_string()),
            },
        ];
        let plain_text = MarkdownRenderer::spans_plain_text(&spans);
        assert_eq!(plain_text, "TitleAlt Caption");
    }

    #[test]
    fn test_spans_plain_text_includes_strikethrough() {
        let spans = vec![
            InlineSpan::Text("alpha ".to_string()),
            InlineSpan::Strikethrough("beta".to_string()),
        ];
        let plain_text = MarkdownRenderer::spans_plain_text(&spans);
        assert_eq!(plain_text, "alpha beta");
    }

    #[test]
    fn test_image_source_stale_detects_file_changes() {
        use std::time::Duration as StdDuration;

        let dir = tempfile::tempdir().expect("temp dir");
        let file_path = dir.path().join("image.bin");
        std::fs::write(&file_path, [1u8, 2, 3, 4]).expect("write image");
        let initial = MarkdownRenderer::disk_image_timestamp(&file_path);
        assert!(!MarkdownRenderer::image_source_stale(initial, &file_path));

        std::thread::sleep(StdDuration::from_millis(5));
        std::fs::write(&file_path, [4u8, 3, 2, 1]).expect("rewrite image");

        assert!(MarkdownRenderer::image_source_stale(initial, &file_path));

        std::fs::remove_file(&file_path).expect("remove image");
        assert!(MarkdownRenderer::image_source_stale(initial, &file_path));
    }

    #[test]
    fn test_disk_image_timestamp_missing_returns_none() {
        let missing = std::path::Path::new("missing_image_timestamp.bin");
        assert!(MarkdownRenderer::disk_image_timestamp(missing).is_none());
    }

    #[test]
    fn test_image_source_stale_with_timestamp_combinations() {
        let stamp = SystemTime::UNIX_EPOCH;
        assert!(MarkdownRenderer::image_source_stale_with_timestamp(
            Some(stamp),
            None
        ));
        assert!(MarkdownRenderer::image_source_stale_with_timestamp(
            None,
            Some(stamp)
        ));
        assert!(!MarkdownRenderer::image_source_stale_with_timestamp(
            None, None
        ));
    }

    #[test]
    fn test_inline_code_preserves_whitespace() {
        let renderer = MarkdownRenderer::new();
        let md = "Start `code` end";
        let parsed = renderer.parse(md).expect("parse ok");
        let non_paragraph = MarkdownElement::HorizontalRule;
        let mut saw_paragraph = false;
        for element in [&parsed[0], &non_paragraph] {
            if let MarkdownElement::Paragraph(spans) = element {
                let code_span = spans.iter().find_map(|span| match span {
                    InlineSpan::Code(t) => Some(t),
                    _ => None,
                });
                assert_eq!(code_span, Some(&"code".to_string()));
                saw_paragraph = true;
            }
        }
        assert!(saw_paragraph);
    }

    #[test]
    fn test_footnote_markers_render_as_visible_text() {
        let renderer = MarkdownRenderer::new();
        let md = "Paragraph with footnote[^1].\n\n[^1]: footnote body.";
        let parsed = renderer.parse(md).expect("parse ok");
        assert_eq!(parsed.len(), 2);
        let first_plain = MarkdownRenderer::element_plain_text(&parsed[0]);
        assert!(first_plain.contains("footnote[^1]"));
        let second_plain = MarkdownRenderer::element_plain_text(&parsed[1]);
        assert!(second_plain.contains("[^1]: footnote body."));
    }

    #[test]
    fn test_parse_headers_assign_ids_and_dedupe() {
        let renderer = MarkdownRenderer::new();
        let md = "# Getting Started\n\nParagraph text.\n\n## Getting Started\n\n### API & Usage\n\n## API & Usage\n";
        let parsed = renderer.parse(md).expect("parse ok");

        let mut ids = vec![];
        for el in parsed {
            if let MarkdownElement::Header { id, .. } = el {
                ids.push(id);
            }
        }
        assert_eq!(ids.len(), 4);
        assert_eq!(ids[0], "getting-started");
        assert_eq!(ids[1], "getting-started-1");
        assert_eq!(ids[2], "api-usage");
        assert_eq!(ids[3], "api-usage-1");
    }

    #[test]
    fn test_formatting_sample_contains_expected_header_ids() {
        let renderer = MarkdownRenderer::new();
        let formatting = SAMPLE_FILES
            .iter()
            .find(|f| f.name == "formatting.md")
            .expect("formatting sample present");
        let parsed = renderer.parse(formatting.content).expect("parse ok");
        let ids: Vec<String> = parsed
            .into_iter()
            .filter_map(|el| match el {
                MarkdownElement::Header { id, .. } => Some(id),
                _ => None,
            })
            .collect();

        for expected in [
            "markdown-formatting-guide",
            "table-of-contents",
            "text-formatting",
            "headers",
            "lists",
            "links-and-images",
            "emojis",
            "blockquotes",
            "horizontal-rules",
            "tables",
        ] {
            assert!(ids.iter().any(|id| id == expected));
        }
    }

    #[test]
    fn test_inline_image_parsing() {
        let renderer = MarkdownRenderer::new();
        let md = "Here is an image: ![Alt text](images/pic.webp \"Title\") end.";
        let parsed = renderer.parse(md).expect("parse ok");
        assert_eq!(parsed.len(), 1);
        assert!(matches!(&parsed[0], MarkdownElement::Paragraph(_)));
        let non_paragraph = MarkdownElement::HorizontalRule;
        let mut saw_paragraph = false;
        for element in [&parsed[0], &non_paragraph] {
            if let MarkdownElement::Paragraph(spans) = element {
                let img = spans.iter().find(|s| matches!(s, InlineSpan::Image { .. }));
                assert!(img.is_some());
                let (src, alt, title) = image_span_fields(img.unwrap()).expect("image span");
                assert_eq!(src, "images/pic.webp");
                assert_eq!(alt, "Alt text");
                assert_eq!(title, Some("Title"));
                saw_paragraph = true;
            }
        }
        assert!(saw_paragraph);
        assert!(image_span_fields(&InlineSpan::Text("nope".into())).is_none());
    }

    #[test]
    fn test_image_text_in_plain_text_index() {
        let renderer = MarkdownRenderer::new();
        let md = "![Diagram](./a.png \"Flow\")";
        let parsed = renderer.parse(md).expect("parse ok");
        let text = MarkdownRenderer::element_plain_text(&parsed[0]);
        assert!(text.contains("Diagram"));
        assert!(text.contains("Flow"));
    }

    #[test]
    fn test_element_plain_text_header_and_code_block() {
        let header = MarkdownElement::Header {
            level: 2,
            spans: vec![InlineSpan::Text("Title".to_string())],
            id: "title".to_string(),
        };
        let code = MarkdownElement::CodeBlock {
            text: "fn main() {}".to_string(),
            language: Some("rust".to_string()),
            highlighted: None,
        };
        assert_eq!(
            MarkdownRenderer::element_plain_text(&header),
            "Title".to_string()
        );
        assert_eq!(
            MarkdownRenderer::element_plain_text(&code),
            "fn main() {}".to_string()
        );
    }

    #[test]
    fn test_cell_fragments_split_text_and_images() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![
            InlineSpan::Text("alpha".into()),
            InlineSpan::Strong("beta".into()),
            InlineSpan::Image {
                src: "img.png".into(),
                alt: "img".into(),
                title: None,
            },
            InlineSpan::Text("gamma".into()),
        ];
        let fragments = renderer.cell_fragments(&spans);
        assert_eq!(fragments.len(), 3);
        assert!(matches!(fragments[0], CellFragment::Text(_)));
        let mut saw_first_text = false;
        for fragment in [&fragments[0], &fragments[1]] {
            if let CellFragment::Text(slice) = fragment {
                assert_eq!(slice.len(), 2);
                saw_first_text = true;
            }
        }
        assert!(saw_first_text);
        assert!(matches!(fragments[1], CellFragment::Image(_)));
        let mut saw_image = false;
        for fragment in [&fragments[1], &fragments[0]] {
            if let CellFragment::Image(span) = fragment {
                let (src, _alt, _title) = image_span_fields(span).expect("image span");
                assert_eq!(src, "img.png");
                saw_image = true;
            }
        }
        assert!(saw_image);
        assert!(matches!(fragments[2], CellFragment::Text(_)));
        let mut saw_last_text = false;
        for fragment in [&fragments[2], &fragments[1]] {
            if let CellFragment::Text(slice) = fragment {
                assert_eq!(slice.len(), 1);
                let content = text_span_content(&slice[0]).expect("text span");
                assert_eq!(content, "gamma");
                saw_last_text = true;
            }
        }
        assert!(saw_last_text);
        assert!(text_span_content(&InlineSpan::Image {
            src: "img.png".into(),
            alt: "img".into(),
            title: None,
        })
        .is_none());
    }

    #[test]
    fn test_cell_fragments_leading_image() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![
            InlineSpan::Image {
                src: "cover.png".into(),
                alt: "cover".into(),
                title: None,
            },
            InlineSpan::Text("tail".into()),
        ];
        let fragments = renderer.cell_fragments(&spans);
        assert_eq!(fragments.len(), 2);
        assert!(matches!(fragments[0], CellFragment::Image(_)));
        assert!(matches!(fragments[1], CellFragment::Text(_)));
    }

    #[test]
    fn test_cell_fragments_detect_single_emoji_cell() {
        let renderer = MarkdownRenderer::new();
        let rocket = rocket_emoji();
        let spans = vec![InlineSpan::Strong((*rocket).to_string())];
        let fragments = renderer.cell_fragments(&spans);
        assert_eq!(fragments.len(), 1);
        let key = emoji_fragment_key(&fragments[0]).expect("emoji fragment");
        assert!(!key.is_empty());
        let text_spans = vec![InlineSpan::Text("plain".into())];
        let fragment = CellFragment::Text(&text_spans);
        assert!(emoji_fragment_key(&fragment).is_none());
    }

    #[test]
    fn test_cell_single_emoji_emphasis() {
        let renderer = MarkdownRenderer::new();
        let rocket = rocket_emoji();
        let spans = vec![InlineSpan::Emphasis((*rocket).to_string())];
        assert!(renderer.cell_single_emoji(&spans).is_some());
    }

    #[test]
    fn test_cell_single_emoji_strikethrough() {
        let renderer = MarkdownRenderer::new();
        let rocket = rocket_emoji();
        let spans = vec![InlineSpan::Strikethrough((*rocket).to_string())];
        assert!(renderer.cell_single_emoji(&spans).is_some());
    }

    #[test]
    fn test_cell_single_emoji_returns_none_for_code_and_link() {
        let renderer = MarkdownRenderer::new();
        let rocket = rocket_emoji();
        let code_spans = vec![InlineSpan::Code((*rocket).to_string())];
        assert!(renderer.cell_single_emoji(&code_spans).is_none());

        let link_spans = vec![InlineSpan::Link {
            text: (*rocket).to_string(),
            url: "https://example.com".to_string(),
        }];
        assert!(renderer.cell_single_emoji(&link_spans).is_none());
    }

    #[test]
    fn test_cell_single_emoji_multiple_graphemes_returns_none() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("ab".to_string())];
        assert!(renderer.cell_single_emoji(&spans).is_none());
    }

    #[test]
    fn test_get_or_make_emoji_texture_falls_back_on_decode_error() {
        let renderer = MarkdownRenderer::new();
        let rocket = rocket_emoji();
        force_emoji_decode_error_once();
        with_test_ui(|_, ui| {
            let _ = renderer.get_or_make_emoji_texture(ui, rocket);
        });
    }

    #[test]
    fn test_get_or_make_emoji_texture_generates_on_decode_error() {
        let renderer = MarkdownRenderer::new();
        let fallback = [
            "\u{2705}",
            "\u{1f389}",
            "\u{1f680}",
            "\u{2764}",
            "\u{1f496}",
            "\u{2b50}",
            "\u{1f525}",
        ];
        let emoji = crate::emoji_catalog::shortcode_map()
            .values()
            .find(|emoji| {
                !fallback.contains(emoji) && emoji_catalog::image_bytes_for(emoji).is_some()
            })
            .expect("emoji outside fallback");
        assert!(emoji_assets::make_image(emoji, 64).is_none());
        force_emoji_decode_error_once();
        with_test_ui(|_, ui| {
            let _ = renderer.get_or_make_emoji_texture(ui, emoji);
        });
    }

    #[test]
    fn test_cell_fragments_inline_emoji_stays_text() {
        let renderer = MarkdownRenderer::new();
        let rocket = rocket_emoji();
        let spans = vec![
            InlineSpan::Text((*rocket).to_string()),
            InlineSpan::Text("tail".into()),
        ];
        let fragments = renderer.cell_fragments(&spans);
        assert_eq!(fragments.len(), 1);
        assert!(matches!(fragments[0], CellFragment::Text(_)));
        let dummy_span = InlineSpan::Image {
            src: "dummy.png".into(),
            alt: "dummy".into(),
            title: None,
        };
        let dummy_fragment = CellFragment::Image(&dummy_span);
        let mut saw_text = false;
        for fragment in [&fragments[0], &dummy_fragment] {
            if let CellFragment::Text(slice) = fragment {
                assert_eq!(slice.len(), 2);
                saw_text = true;
            }
        }
        assert!(saw_text);
    }

    #[test]
    fn test_cell_fragments_keep_link_emoji_interactive() {
        let renderer = MarkdownRenderer::new();
        let rocket = rocket_emoji();
        let spans = vec![InlineSpan::Link {
            text: (*rocket).to_string(),
            url: "https://example.com".to_string(),
        }];
        let fragments = renderer.cell_fragments(&spans);
        assert_eq!(fragments.len(), 1);
        assert!(matches!(fragments[0], CellFragment::Text(_)));
        let dummy_span = InlineSpan::Image {
            src: "dummy.png".into(),
            alt: "dummy".into(),
            title: None,
        };
        let dummy_fragment = CellFragment::Image(&dummy_span);
        let mut saw_link = false;
        for fragment in [&fragments[0], &dummy_fragment] {
            if let CellFragment::Text(slice) = fragment {
                assert!(matches!(slice[0], InlineSpan::Link { .. }));
                saw_link = true;
            }
        }
        assert!(saw_link);
    }

    #[test]
    fn test_render_overhauled_cell_handles_emoji_fragment() {
        let renderer = MarkdownRenderer::new();
        let rocket = rocket_emoji();
        let spans = vec![InlineSpan::Text((*rocket).to_string())];
        let ctx = egui::Context::default();
        let input = screen_input(240.0, 120.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            let _ =
                renderer.render_overhauled_cell(ui, &spans, 120.0, false, Some(0), 0, Align::LEFT);
        });
    }

    #[test]
    fn test_layout_job_builder_respects_wrap_width() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text(
            "A long column entry that should wrap neatly within the supplied width.".into(),
        )];
        let style = egui::Style::default();
        let build = renderer.build_layout_job(&style, &spans, 180.0, false, Align::LEFT);
        assert_eq!(build.job.wrap.max_width, 180.0);
        assert!(build.job.text.contains("column entry"));
    }

    #[test]
    fn test_layout_job_builder_highlights_matches() {
        let renderer = MarkdownRenderer::new();
        renderer.set_highlight_phrase(Some("wrap"));
        let spans = vec![InlineSpan::Text("wrap me, wrap me again".into())];
        let style = egui::Style::default();
        let build = renderer.build_layout_job(&style, &spans, 200.0, false, Align::LEFT);
        let highlight_bg = style.visuals.selection.bg_fill;
        assert!(build
            .job
            .sections
            .iter()
            .any(|s| s.format.background == highlight_bg));
    }

    #[test]
    fn test_layout_job_builder_tracks_link_ranges() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Link {
            text: "Docs".into(),
            url: "https://example.org/docs".into(),
        }];
        let style = egui::Style::default();
        let build = renderer.build_layout_job(&style, &spans, 220.0, false, Align::LEFT);
        assert_eq!(build.link_ranges.len(), 1);
        let link = &build.link_ranges[0];
        assert_eq!(link.url, "https://example.org/docs");
        let char_len = link.char_range.end - link.char_range.start;
        let linked_text: String = build
            .job
            .text
            .chars()
            .skip(link.char_range.start)
            .take(char_len)
            .collect();
        assert_eq!(linked_text, "Docs");
    }

    #[test]
    fn test_layout_job_builder_skips_empty_link_text() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Link {
            text: String::new(),
            url: "https://example.org/docs".into(),
        }];
        let style = egui::Style::default();
        let build = renderer.build_layout_job(&style, &spans, 220.0, false, Align::LEFT);
        assert!(build.link_ranges.is_empty());
    }

    #[test]
    fn test_build_layout_job_skips_images() {
        let renderer = MarkdownRenderer::new();
        let style = egui::Style::default();
        let spans = vec![InlineSpan::Image {
            src: "dummy.png".to_string(),
            alt: "alt".to_string(),
            title: None,
        }];

        let build = renderer.build_layout_job(&style, &spans, 120.0, false, Align::LEFT);
        assert!(build.plain_text.is_empty());
    }

    #[test]
    fn test_append_spans_empty_and_code_light() {
        let renderer = MarkdownRenderer::new();
        let style = egui::Style::default();
        let mut job = LayoutJob::default();
        let mut plain_text = String::new();

        assert_eq!(
            renderer.append_plain_span(
                &style,
                &mut job,
                &mut plain_text,
                "",
                InlineStyle::default(),
                None
            ),
            0
        );
        assert_eq!(
            renderer.append_code_span(&style, &mut job, &mut plain_text, ""),
            0
        );

        let mut light_style = style.clone();
        light_style.visuals.dark_mode = false;
        assert!(renderer.append_code_span(&light_style, &mut job, &mut plain_text, "code") > 0);

        let mut empty_job = LayoutJob::default();
        let mut empty_plain = String::new();
        assert_eq!(
            renderer.append_text_sections(
                &style,
                &mut empty_job,
                &mut empty_plain,
                "",
                12.0,
                InlineStyle::default(),
                None
            ),
            0
        );
    }

    #[test]
    fn test_is_html_line_break_and_indented_code_tabs() {
        assert!(!MarkdownRenderer::is_html_line_break("<b"));
        assert!(MarkdownRenderer::is_html_line_break("<br >"));
        assert!(!MarkdownRenderer::is_html_line_break("<brx>"));
        assert!(MarkdownRenderer::is_indented_code_line("\tcode"));
    }

    #[test]
    fn test_is_html_line_break_rejects_missing_bracket() {
        assert!(!MarkdownRenderer::is_html_line_break("<br"));
        assert!(!MarkdownRenderer::is_html_line_break("<br/"));
    }

    #[test]
    fn test_is_html_line_break_slash_variants() {
        assert!(MarkdownRenderer::is_html_line_break("<br/>"));
        assert!(!MarkdownRenderer::is_html_line_break("<br/ x>"));
    }

    #[test]
    fn test_escape_pipes_inline_code_multi_backticks() {
        let out = MarkdownRenderer::escape_pipes_in_inline_code_line("``code|``");
        assert!(out.contains(PIPE_SENTINEL));
    }

    #[test]
    fn test_escape_pipes_inline_code_line_branches() {
        let plain = "no pipes here";
        assert_eq!(
            MarkdownRenderer::escape_pipes_in_inline_code_line(plain),
            plain
        );

        let line = "``code`|more``";
        let escaped = MarkdownRenderer::escape_pipes_in_inline_code_line(line);
        assert!(escaped.contains(PIPE_SENTINEL));
    }

    #[test]
    fn test_escape_table_pipes_list_indent_with_tabs_and_table() {
        let input = "- Parent\n\t- Child\n  | Head | Tail |\n  | --- | --- |\n  | a | b |\n";
        let output = MarkdownRenderer::escape_table_pipes_in_inline_code(input);
        assert!(output.contains("| Head | Tail |"));
    }

    #[test]
    fn test_escape_pipes_inline_code_line_restores_pending_on_unclosed() {
        let line = "`code|tail";
        let escaped = MarkdownRenderer::escape_pipes_in_inline_code_line(line);
        assert_eq!(escaped, line);
    }

    #[test]
    fn test_table_width_solver_keeps_short_columns_readable() {
        let mins = vec![90.0, 90.0, 90.0, 90.0];
        let desired = vec![120.0, 120.0, 360.0, 160.0];
        let widths = MarkdownRenderer::resolve_table_widths(420.0, &mins, &desired);
        assert_eq!(widths.len(), 4);
        // Narrow columns should stay near their minimums even when a wide column exists
        assert!(widths[0] >= 85.0);
        assert!(widths[2] > widths[0]);
        let sum: f32 = widths.iter().sum();
        assert!((sum - 420.0).abs() < 0.5);
    }

    #[test]
    fn test_table_width_solver_handles_constrained_space() {
        let mins = vec![100.0, 100.0, 100.0, 100.0];
        let desired = vec![200.0, 240.0, 360.0, 160.0];
        let widths = MarkdownRenderer::resolve_table_widths(260.0, &mins, &desired);
        assert_eq!(widths.len(), 4);
        assert!(widths.iter().all(|w| *w > 0.0));
        let sum: f32 = widths.iter().sum();
        assert!((sum - 260.0).abs() < 0.5);
    }

    #[test]
    fn test_resolve_table_rects_clip_rect_fallbacks() {
        let renderer = MarkdownRenderer::new();
        let widths = vec![12.0];
        let body_clip = egui::Rect::from_min_max(egui::pos2(1.0, 2.0), egui::pos2(3.0, 4.0));
        let (rect, clip) =
            renderer.resolve_table_rects(None, None, None, None, Some(body_clip), &widths, 4.0);
        assert_eq!(clip, body_clip);

        let (rect2, clip2) =
            renderer.resolve_table_rects(None, None, None, None, None, &widths, 4.0);
        assert_eq!(clip2, rect2);
        assert_eq!(rect2.min, egui::pos2(0.0, 0.0));
        assert_eq!(rect2.max, egui::pos2(0.0, 0.0));
        assert_eq!(rect.min, egui::pos2(0.0, 0.0));
    }

    #[test]
    fn test_resolve_table_rects_prefers_layout_then_header_then_body() {
        let renderer = MarkdownRenderer::new();
        let widths = vec![10.0, 20.0];
        let layout = egui::Rect::from_min_max(egui::pos2(5.0, 6.0), egui::pos2(25.0, 26.0));
        let header = egui::Rect::from_min_max(egui::pos2(1.0, 2.0), egui::pos2(9.0, 12.0));
        let body = egui::Rect::from_min_max(egui::pos2(2.0, 3.0), egui::pos2(14.0, 18.0));

        let (rect, _) = renderer.resolve_table_rects(
            Some(header),
            Some(body),
            Some(layout),
            None,
            None,
            &widths,
            4.0,
        );
        assert_eq!(rect.min.x, layout.min.x);

        let (rect, _) =
            renderer.resolve_table_rects(Some(header), None, None, None, None, &widths, 4.0);
        assert_eq!(rect.min.x, header.min.x);

        let (rect, _) =
            renderer.resolve_table_rects(None, Some(body), None, None, None, &widths, 4.0);
        assert_eq!(rect.min.x, body.min.x);
    }

    #[test]
    fn test_resolve_table_rects_layout_only_fallbacks() {
        let renderer = MarkdownRenderer::new();
        let widths = vec![12.0];
        let layout = egui::Rect::from_min_max(egui::pos2(4.0, 7.0), egui::pos2(18.0, 21.0));
        let (rect, _) =
            renderer.resolve_table_rects(None, None, Some(layout), None, None, &widths, 4.0);
        assert_eq!(rect.min.y, layout.min.y);
        assert_eq!(rect.max.y, layout.max.y);
    }

    #[test]
    fn table_cells_keep_images_and_formatting() {
        let renderer = MarkdownRenderer::new();
        let md = "\
Intro

| H1 | H2 |
| --- | --- |
| text **extra** ![Alt](img.png) | **bold** and [link](https://example.com) |";
        let elements = renderer.parse(md).expect("parse ok");
        let table = elements
            .iter()
            .find_map(|el| match el {
                MarkdownElement::Table {
                    headers: _,
                    rows,
                    alignments: _,
                } => Some(rows),
                _ => None,
            })
            .expect("table present");
        assert_eq!(table.len(), 1);
        let row = &table[0];
        let mut saw_image = false;
        let mut saw_text = false;
        for span in &row[0] {
            match span {
                InlineSpan::Image { .. } => saw_image = true,
                InlineSpan::Text(_) => saw_text = true,
                _ => {}
            }
        }
        assert!(saw_image);
        assert!(saw_text);
        let mut saw_strong = false;
        let mut saw_link = false;
        let mut saw_other = false;
        for span in &row[1] {
            match span {
                InlineSpan::Strong(_) => saw_strong = true,
                InlineSpan::Link { .. } => saw_link = true,
                _ => saw_other = true,
            }
        }
        assert!(saw_strong);
        assert!(saw_link);
        assert!(saw_other);
    }

    #[test]
    fn table_inline_code_keeps_pipes() {
        let renderer = MarkdownRenderer::new();
        let md = "\
Intro

| Col | Notes |
| --- | --- |
| code | see `a|b|c` |";
        let elements = renderer.parse(md).expect("parse ok");
        let rows = elements
            .iter()
            .find_map(|el| match el {
                MarkdownElement::Table {
                    headers: _,
                    rows,
                    alignments: _,
                } => Some(rows),
                _ => None,
            })
            .expect("table present");
        let code_text = rows[0][1]
            .iter()
            .find_map(|span| match span {
                InlineSpan::Code(text) => Some(text.as_str()),
                _ => None,
            })
            .expect("code span");
        assert_eq!(code_text, "a|b|c");
    }

    #[test]
    fn indented_code_block_table_like_preserves_pipes() {
        let md = "\
    | Col | Notes |
    | --- | --- |
    | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn blockquote_table_inline_code_escapes_pipes() {
        let md = "\
> | Col | Notes |
> | --- | --- |
> | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn blockquote_tab_table_inline_code_escapes_pipes() {
        let md = ">\t| Col | Notes |\n>\t| --- | --- |\n>\t| code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn blockquote_table_after_paragraph_parses() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md = "\
Prelude

> Intro line
> | Col | Notes |
> | --- | --- |
> | A | B |";
        let elements = renderer.parse(md)?;
        let blocks = elements
            .iter()
            .find_map(|el| match el {
                MarkdownElement::Quote { blocks, .. } => Some(blocks),
                _ => None,
            })
            .expect("quote block");
        assert!(blocks
            .iter()
            .any(|block| matches!(block, MarkdownElement::Table { .. })));
        Ok(())
    }

    #[test]
    fn blockquote_list_table_after_paragraph_parses() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md = "\
Prelude

> Note before list
> - Item:
>   | Key | Val |
>   | --- | --- |
>   | A | 1 |";
        let elements = renderer.parse(md)?;
        let blocks = elements
            .iter()
            .find_map(|el| match el {
                MarkdownElement::Quote { blocks, .. } => Some(blocks),
                _ => None,
            })
            .expect("quote block");
        let list = blocks
            .iter()
            .find_map(|block| match block {
                MarkdownElement::List { items, .. } => Some(items),
                _ => None,
            })
            .expect("list block");
        let first = list.first().expect("list item");
        assert!(first
            .blocks
            .iter()
            .any(|block| matches!(block, MarkdownElement::Table { .. })));
        Ok(())
    }

    #[test]
    fn blockquote_indented_code_block_preserves_pipes() {
        let md = "\
>     | Col | Notes |
>     | --- | --- |
>     | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn blockquote_fenced_code_block_preserves_pipes() {
        let md = "\
> ```
> | Col | Notes |
> | --- | --- |
> | code | `a|b|c` |
> ```";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn list_fenced_code_block_preserves_pipes() {
        let md = "\
- ```
  | Col | Notes |
  | --- | --- |
  | code | `a|b|c` |
  ```";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn list_blockquote_fenced_code_block_preserves_pipes() {
        let md = "\
- Item
    > ```
    > | Col | Notes |
    > | --- | --- |
    > | code | `a|b|c` |
    > ```";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn nested_list_fenced_code_block_preserves_pipes() {
        let md = "\
- Outer
    - ```
      | Col | Notes |
      | --- | --- |
      | code | `a|b|c` |
      ```";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn list_item_code_block_preserves_pipes() {
        let md = "\
- Item
      | Col | Notes |
      | --- | --- |
      | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn nested_list_table_inline_code_escapes_pipes() {
        let md = "\
- Outer
  - | Col | Notes |
    | --- | --- |
    | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn nested_list_tab_table_inline_code_escapes_pipes() {
        let md = "- Outer\n\t- | Col | Notes |\n\t  | --- | --- |\n\t  | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn nested_list_four_space_table_inline_code_escapes_pipes() {
        let md = "\
- Outer
    - | Col | Notes |
      | --- | --- |
      | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn list_marker_four_space_table_inline_code_escapes_pipes() {
        let md = "\
-    | Col | Notes |
     | --- | --- |
     | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn nested_list_indented_code_block_preserves_pipes() {
        let md = "\
- Outer
      - | Col | Notes |
        | --- | --- |
        | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn deeply_nested_list_table_inline_code_escapes_pipes() {
        let md = "\
- Outer
    - Inner
        - | Col | Notes |
          | --- | --- |
          | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn list_blockquote_table_inline_code_escapes_pipes() {
        let md = "\
- Item
    > | Col | Notes |
    > | --- | --- |
    > | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn list_marker_blockquote_table_inline_code_escapes_pipes() {
        let md = "\
- > | Col | Notes |
  > | --- | --- |
  > | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn list_marker_table_dedent_does_not_escape_pipes() {
        let md = "\
- | Col | `a|b|c` |
| --- | --- |
| row | ok |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn blockquote_list_table_inline_code_escapes_pipes() {
        let md = "\
> - | Col | Notes |
>   | --- | --- |
>   | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn list_marker_line_table_inline_code_escapes_pipes() {
        let md = "\
10. 
    | Col | Notes |
    | --- | --- |
    | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn list_marker_tab_after_marker_uses_tab_stops() {
        let line = "-\titem";
        let (_content, indent, content_indent) =
            MarkdownRenderer::list_marker_info(line).expect("list marker");
        assert_eq!(indent, 3);
        assert_eq!(content_indent, 4);
    }

    #[test]
    fn list_marker_any_indent_tab_stops_leading_whitespace() {
        let line = " \t- item";
        let (_content, _indent, _content_indent, leading) =
            MarkdownRenderer::list_marker_info_any_indent(line).expect("list marker");
        assert_eq!(leading, 4);
    }

    #[test]
    fn table_ends_before_inline_code_paragraph() {
        let md = "\
| Col | Notes |
| --- | --- |
| code | `a|b|c` |
Paragraph with `x|y` inline.";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
        assert!(prepared.contains("`x|y`"));
    }

    #[test]
    fn table_ends_before_escaped_pipe_paragraph() {
        let md = "\
| Col | Notes |
| --- | --- |
| code | `a|b|c` |
Paragraph with escaped \\| pipe.";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
        assert!(prepared.contains("escaped \\| pipe"));
    }

    #[test]
    fn list_table_ends_on_dedent_with_pipe_paragraph() {
        let md = "\
- | Col | Notes |
  | --- | --- |
  | code | `a|b|c` |
Paragraph with `x|y` and pipe | outside.";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
        assert!(prepared.contains("`x|y`"));
    }

    #[test]
    fn nested_list_table_ends_on_dedent_with_pipe_paragraph() {
        let md = "\
- Outer
  - | Col | Notes |
    | --- | --- |
    | code | `a|b|c` |
  Paragraph with `x|y` and pipe | outside.";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
        assert!(prepared.contains("`x|y`"));
    }

    #[test]
    fn list_table_tab_dedent_keeps_inline_code_unescaped() {
        let md =
            "- Outer\n\t- | Col | Notes |\n\t  | --- | --- |\n\t  | code | `a|b|c` |\nParagraph with `x|y` and pipe | outside.";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
        assert!(prepared.contains("`x|y`"));
    }

    #[test]
    fn list_blank_line_table_inline_code_escapes_pipes() {
        let md = "\
- Item

    | Col | Notes |
    | --- | --- |
    | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn table_delimiter_requires_pipe() {
        let md = "\
`a|b` | Header
---
Not a table";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(prepared.contains("`a|b`"));
        assert!(!prepared.contains(PIPE_SENTINEL));
    }

    #[test]
    fn table_unmatched_backticks_do_not_escape_pipes() {
        let md = "\
| Col | Notes |
| --- | --- |
| code | `a|b |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(!prepared.contains(PIPE_SENTINEL));
    }

    #[test]
    fn table_escaped_backticks_do_not_escape_pipes() {
        let md = "\
| Col | Notes |
| --- | --- |
| text | \\`a|b\\` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(prepared.contains("\\`a|b\\`"));
        assert!(!prepared.contains(PIPE_SENTINEL));
    }

    #[test]
    fn list_parent_indent_nested_marker_table_escapes_pipes() {
        let md = "\
- Parent
    - | Col | Notes |
      | --- | --- |
      | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn list_parent_indent_table_without_marker_escapes_pipes() {
        let md = "\
- Parent
  | Col | Notes |
  | --- | --- |
  | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn table_blockquote_level_mismatch_ends_table() {
        let md = "\
| Col | Notes |
| --- | --- |
> | code | `a|b|c` |";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn table_inserts_blank_line_before_header_with_crlf() {
        let md = "Intro line\r\n| Col | Notes |\r\n| --- | --- |\r\n| code | `a|b|c` |\r\n";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(prepared.contains("\r\n\r\n| Col | Notes |"));
        assert_pipes_escaped(&prepared);
    }

    #[test]
    fn fenced_block_quote_level_mismatch_keeps_block_open() {
        let md = "\
> ```
> | Col | Notes |
```
> | --- | --- |
> | code | `a|b|c` |
> ```";
        let prepared = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_pipes_preserved(&prepared);
    }

    #[test]
    fn table_ids_are_unique_per_position() {
        let renderer = MarkdownRenderer::new();
        let md = "\
Intro

| H |
| - |
| a |

| H |
| - |
| a |";
        let elements = renderer.parse(md).expect("parse ok");
        let tables: Vec<_> = elements
            .iter()
            .enumerate()
            .filter_map(|(idx, el)| match el {
                MarkdownElement::Table {
                    headers,
                    rows,
                    alignments,
                } => Some(renderer.compute_table_id(headers, rows, alignments, idx as u64)),
                _ => None,
            })
            .collect();
        assert_eq!(tables.len(), 2);
        assert_ne!(tables[0], tables[1]);
    }

    #[test]
    fn test_render_to_ui_rich_document_populates_rects_and_table_stats() {
        let renderer = MarkdownRenderer::new();
        let md = "\
# Coverage Demo

Paragraph with :tada: emoji and **bold** text and a [link](#target).

> Quote line one
> Quote line two

- item one
- item two

---

```rust
fn main() {}
```

| Col A | Col B |
| --- | --- |
| A1 | B1 |
| A2 | B2 |

![Logo](assets/samples/logo.svg \"Logo\")
![Sample](assets/samples/webp_sample.webp \"Sample\")
![Missing](missing_test_image.png \"Missing\")
![Remote](https://example.com/image.png \"Remote\")
";

        let elements = renderer.parse(md).expect("parse ok");
        renderer.set_highlight_phrase(Some("item"));

        with_test_ui(|_, ui| {
            renderer.render_to_ui(ui, &elements);
        });

        assert!(renderer.element_rect_at(0).is_some());
        assert!(renderer.header_rect_for("coverage-demo").is_some());
        let (rendered, total) = renderer.table_render_stats();
        assert!(total > 0);
        assert!(rendered > 0);
        let (_hits, misses) = renderer.table_layout_cache_stats();
        assert!(misses > 0);
    }

    #[test]
    fn test_table_layout_cache_records_hits() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("alpha".to_string())];
        let style = egui::Style::default();

        let _ = renderer.cached_layout_job(&style, Some(0), 0, &spans, 120.0, false, Align::LEFT);
        let _ = renderer.cached_layout_job(&style, Some(0), 0, &spans, 120.0, false, Align::LEFT);

        let (hits, misses) = renderer.table_layout_cache_stats();
        assert!(hits >= 1);
        assert!(misses >= 1);

        renderer.clear_table_layout_cache();
        assert_eq!(renderer.table_layout_cache_stats(), (0, 0));
    }

    #[test]
    fn test_cell_layout_cache_eviction() {
        let mut cache = CellLayoutCache::new(2);
        let build = LayoutJobBuild {
            job: LayoutJob::default(),
            plain_text: String::new(),
            link_ranges: Vec::new(),
        };
        let key = |row, col| CellLayoutKey {
            row: Some(row),
            col,
            width: 120,
            align: 0,
            strong: false,
            text_color: [0, 0, 0, 255],
            highlight_hash: 0,
            content_hash: row as u64,
        };

        let key_a = key(0, 0);
        let key_b = key(1, 0);
        let key_c = key(2, 0);

        cache.insert(key_a.clone(), build.clone());
        cache.insert(key_b.clone(), build.clone());
        cache.insert(key_c.clone(), build);

        assert!(cache.get(&key_a).is_none());
        assert!(cache.get(&key_b).is_some());
        assert!(cache.get(&key_c).is_some());
    }

    #[test]
    fn test_cell_layout_cache_handles_empty_order() {
        let mut cache = CellLayoutCache::new(1);
        let build = LayoutJobBuild {
            job: LayoutJob::default(),
            plain_text: String::new(),
            link_ranges: Vec::new(),
        };
        let key_a = CellLayoutKey {
            row: Some(0),
            col: 0,
            width: 120,
            align: 0,
            strong: false,
            text_color: [0, 0, 0, 255],
            highlight_hash: 0,
            content_hash: 1,
        };
        let key_b = CellLayoutKey {
            row: Some(1),
            col: 0,
            width: 120,
            align: 0,
            strong: false,
            text_color: [0, 0, 0, 255],
            highlight_hash: 0,
            content_hash: 2,
        };

        cache.insert(key_a, build.clone());
        cache.order.clear();
        cache.insert(key_b.clone(), build);

        assert!(cache.get(&key_b).is_some());
    }

    #[test]
    fn test_cell_layout_cache_reinsert_existing_key() {
        let mut cache = CellLayoutCache::new(1);
        let build = LayoutJobBuild {
            job: LayoutJob::default(),
            plain_text: String::new(),
            link_ranges: Vec::new(),
        };
        let key = CellLayoutKey {
            row: Some(0),
            col: 0,
            width: 120,
            align: 0,
            strong: false,
            text_color: [0, 0, 0, 255],
            highlight_hash: 0,
            content_hash: 42,
        };
        cache.insert(key.clone(), build.clone());
        cache.insert(key.clone(), build);
        assert_eq!(cache.entries.len(), 1);
        assert_eq!(cache.order.len(), 1);
    }

    #[test]
    fn test_table_layout_cache_separates_text_colors() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("alpha".to_string())];
        let mut light_style = egui::Style::default();
        let mut dark_style = egui::Style::default();
        light_style.visuals.override_text_color = Some(Color32::WHITE);
        dark_style.visuals.override_text_color = Some(Color32::BLACK);

        let _ =
            renderer.cached_layout_job(&light_style, Some(0), 0, &spans, 120.0, false, Align::LEFT);
        let _ =
            renderer.cached_layout_job(&dark_style, Some(0), 0, &spans, 120.0, false, Align::LEFT);

        let (hits, misses) = renderer.table_layout_cache_stats();
        assert_eq!(hits, 0);
        assert_eq!(misses, 2);
    }

    #[test]
    fn test_table_alignment_center_single_column() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md = "Intro\n\n| Item |\n| :---: |\n| Alpha |\n";
        let elements = renderer.parse(md)?;
        let alignments = elements
            .iter()
            .find_map(|el| match el {
                MarkdownElement::Table { alignments, .. } => Some(alignments),
                _ => None,
            })
            .expect("table element");
        assert_eq!(alignments.len(), 1);
        assert_eq!(alignments[0], Alignment::Center);
        Ok(())
    }

    #[test]
    fn test_single_column_table_parses_one_cell_per_row() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md =
            "Intro\n\n| Item |\n| :---: |\n| Alpha |\n| line one<br>line two |\n| **bold** and `code` |\n";
        let elements = renderer.parse(md)?;
        let (headers, rows) = elements
            .iter()
            .find_map(|el| match el {
                MarkdownElement::Table { headers, rows, .. } => Some((headers, rows)),
                _ => None,
            })
            .expect("table element");
        assert_eq!(headers.len(), 1);
        assert!(rows.iter().all(|row| row.len() == 1));
        Ok(())
    }

    #[test]
    fn test_table_first_column_alignment_parses_three_columns() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md = fs::read_to_string("tests/regression/cases/026-table-first-column-align.md")?;
        let elements = renderer.parse(&md)?;
        let tables: Vec<_> = elements
            .iter()
            .filter_map(|el| match el {
                MarkdownElement::Table {
                    headers,
                    rows,
                    alignments,
                } => Some((headers, rows, alignments)),
                _ => None,
            })
            .collect();
        assert!(tables.len() >= 3);
        let (first_headers, first_rows, first_alignments) = tables[0];
        for (headers, rows, alignments) in tables.iter() {
            assert_eq!(headers.len(), 3);
            assert_eq!(alignments.len(), 3);
            assert!(rows.iter().all(|row| row.len() == 3));
        }
        let table_id = renderer.compute_table_id(first_headers, first_rows, first_alignments, 0);
        let available = Cell::new(0.0f32);
        let spacing = Cell::new(0.0f32);
        let _estimated_before = {
            let column_stats = renderer.column_stats_for_table(
                table_id,
                first_headers,
                first_rows,
                first_alignments,
            );
            let ctx = TableColumnContext::new(
                first_headers,
                first_rows,
                &column_stats,
                renderer.font_sizes.body,
                table_id,
            );
            let column_specs = derive_column_specs(&ctx);
            let column_spacing = 6.0f32.max(egui::Style::default().spacing.item_spacing.x);
            renderer.estimate_table_total_width(table_id, &column_specs, column_spacing)
        };
        with_test_ui(|_, ui| {
            available.set(ui.available_width());
            spacing.set(ui.spacing().item_spacing.x.max(6.0));
            renderer.render_table_tablebuilder(
                ui,
                first_headers,
                first_rows,
                first_alignments,
                table_id,
            );
        });
        let widths = renderer
            .table_metrics
            .borrow()
            .entry(table_id)
            .map(|entry| entry.current_widths().to_vec())
            .unwrap_or_default();
        assert_eq!(widths.len(), 3);
        assert!(widths.iter().all(|w| *w > 8.0));
        Ok(())
    }

    #[test]
    fn test_trigger_link_handles_fragments_and_schemes() {
        let renderer = MarkdownRenderer::new();

        renderer.trigger_link("#Section-One");
        assert_eq!(
            renderer.take_pending_anchor(),
            Some("section-one".to_string())
        );
        assert!(renderer.take_pending_anchor().is_none());

        renderer.trigger_link("ftp://example.com");
        assert!(renderer.take_pending_anchor().is_none());

        renderer.trigger_link("https://example.com");
        assert!(renderer.take_pending_anchor().is_none());

        renderer.trigger_link("mailto:hello@example.com");
        assert!(renderer.take_pending_anchor().is_none());
    }

    #[test]
    fn test_resolve_image_path_with_base_dir() {
        let renderer = MarkdownRenderer::new();
        let temp = tempdir().expect("temp dir");
        renderer.set_base_dir(Some(temp.path()));

        let resolved = renderer.resolve_image_path("sample.png");
        assert!(resolved.contains("sample.png"));

        let abs = temp.path().join("abs.png");
        let abs_str = abs.to_string_lossy().into_owned();
        assert_eq!(renderer.resolve_image_path(&abs_str), abs_str);

        renderer.set_base_dir(None);
        assert_eq!(renderer.resolve_image_path("relative.png"), "relative.png");
    }

    #[test]
    fn test_disk_image_loads_and_caches() {
        let renderer = MarkdownRenderer::new();
        let temp = tempdir().expect("temp dir");
        let image_path = temp.path().join("disk.png");

        let mut img = image::RgbaImage::new(2, 2);
        for pixel in img.pixels_mut() {
            *pixel = image::Rgba([10, 20, 30, 255]);
        }
        img.save(&image_path).expect("save png");

        renderer.set_base_dir(Some(temp.path()));
        let resolved = renderer.resolve_image_path("disk.png");
        with_test_ui(|ctx, ui| {
            // Use wait_for_image to handle async image loading
            let loaded = wait_for_image(&renderer, ctx, ui, &resolved);
            assert!(loaded.is_some());
        });

        assert!(renderer.image_textures.borrow().contains_key(&resolved));
    }

    #[test]
    fn test_context_menu_helpers_execute() {
        let renderer = MarkdownRenderer::new();
        let _actions = ForcedRenderActions::new(&[
            "copy_text",
            "copy_inline_code",
            "copy_code_block",
            "copy_code_block_lang",
            "open_link",
            "copy_link_text",
            "copy_link_url",
            "copy_cell_text",
        ]);
        with_test_ui(|_, ui| {
            renderer.render_text_context_menu(ui, "text");
            renderer.render_inline_code_context_menu(ui, "code");
            renderer.render_code_block_context_menu(ui, "fn main() {}", Some("rust"));
            renderer.render_link_context_menu(ui, "label", "#anchor");
            renderer.render_cell_context_menu(ui, "cell");
            renderer.copy_text_and_close(ui, "direct-copy");
        });
    }

    #[test]
    fn test_context_menu_helpers_no_actions() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            renderer.render_text_context_menu(ui, "text");
            renderer.render_inline_code_context_menu(ui, "code");
            renderer.render_code_block_context_menu(ui, "fn main() {}", None);
            renderer.render_link_context_menu(ui, "label", "#anchor");
            renderer.render_cell_context_menu(ui, "cell");
        });
    }

    #[test]
    fn test_estimate_table_column_widths_scales_down() {
        let renderer = MarkdownRenderer::new();
        let specs = vec![
            resizable_spec(0, "A", 100.0, 100.0),
            resizable_spec(1, "B", 100.0, 100.0),
        ];

        let widths = renderer.estimate_table_column_widths(&specs, 150.0, 10.0);
        let total = widths.iter().sum::<f32>() + 10.0;
        assert!(total <= 150.5);
    }

    #[test]
    fn test_estimate_table_row_widths_distributes_remainder() {
        let renderer = MarkdownRenderer::new();
        let min_floor = renderer.table_min_column_width();
        let specs = vec![
            fixed_spec(0, "Ord", 80.0),
            ColumnSpec::new(1, "Params", ColumnPolicy::Remainder { clip: false }, None),
        ];
        let resolved = vec![80.0, min_floor];
        let adjusted = resolved.clone();
        let widths = renderer.estimate_table_row_widths(
            &specs, &resolved, &adjusted, 320.0, min_floor, false, false,
        );
        assert!((widths[0] - 80.0).abs() < 0.1);
        assert!(widths[1] > min_floor + 40.0);
    }

    #[test]
    fn test_estimate_table_image_height_no_scale_or_title() {
        let renderer = MarkdownRenderer::new();
        let span = InlineSpan::Image {
            src: "missing.png".to_string(),
            alt: "Alt".to_string(),
            title: Some(String::new()),
        };
        with_test_ui(|_, ui| {
            let height = renderer.estimate_table_image_height(ui, &span, 2000.0);
            assert!(height > 0.0);
        });
    }

    #[test]
    fn test_build_layout_job_covers_inline_styles() {
        let renderer = MarkdownRenderer::new();
        renderer.set_highlight_phrase(Some("code"));
        let spans = vec![
            InlineSpan::Text("plain".to_string()),
            InlineSpan::Strong("bold".to_string()),
            InlineSpan::Emphasis("italics".to_string()),
            InlineSpan::Strikethrough("strike".to_string()),
            InlineSpan::Code("code".to_string()),
            InlineSpan::Link {
                text: "ext".to_string(),
                url: "https://example.com".to_string(),
            },
            InlineSpan::Link {
                text: "local".to_string(),
                url: "#anchor".to_string(),
            },
        ];

        with_test_ui(|_, ui| {
            let style = ui.style().clone();
            let build = renderer.build_layout_job(&style, &spans, 200.0, false, Align::LEFT);
            assert!(build.plain_text.contains("plain"));
            assert_eq!(build.link_ranges.len(), 2);
        });
    }

    #[test]
    fn test_build_layout_job_strong_uses_override_text_color() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("Header".to_string())];
        let mut style = egui::Style::default();
        let override_color = Color32::from_rgb(10, 20, 30);
        style.visuals.override_text_color = Some(override_color);

        let build = renderer.build_layout_job(&style, &spans, 200.0, true, Align::LEFT);
        assert!(!build.job.sections.is_empty());
        assert_eq!(build.job.sections[0].format.color, override_color);
    }

    #[test]
    fn test_render_inline_span_variants() {
        let renderer = MarkdownRenderer::new();
        let temp = tempdir().expect("temp dir");
        let image_path = temp.path().join("image.png");
        std::fs::write(&image_path, tiny_png_bytes()).expect("write png");
        renderer.set_base_dir(Some(temp.path()));

        let image_span = InlineSpan::Image {
            src: "image.png".to_string(),
            alt: "Alt".to_string(),
            title: Some("Title".to_string()),
        };
        let missing_span = InlineSpan::Image {
            src: "missing.png".to_string(),
            alt: "".to_string(),
            title: None,
        };
        let remote_span = InlineSpan::Image {
            src: "https://example.com/image.png".to_string(),
            alt: "Remote".to_string(),
            title: None,
        };
        let remote_http_span = InlineSpan::Image {
            src: "http://example.com/image.png".to_string(),
            alt: "RemoteHttp".to_string(),
            title: None,
        };

        let _guard = ForcedRenderActions::new(&["link_hover", "link_click", "image_hover"]);
        with_test_ui(|ctx, ui| {
            ui.visuals_mut().dark_mode = false;
            renderer.render_inline_span(ui, &InlineSpan::Code("code".to_string()), None, None);

            ui.visuals_mut().dark_mode = true;
            renderer.render_inline_span(ui, &InlineSpan::Text("text".to_string()), None, None);
            renderer.render_inline_span(ui, &InlineSpan::Strong("bold".to_string()), None, None);
            renderer.render_inline_span(
                ui,
                &InlineSpan::Emphasis("italic".to_string()),
                None,
                None,
            );
            renderer.render_inline_span(
                ui,
                &InlineSpan::Strikethrough("strike".to_string()),
                None,
                None,
            );
            renderer.render_inline_span(
                ui,
                &InlineSpan::Link {
                    text: "ext".to_string(),
                    url: "https://example.com".to_string(),
                },
                None,
                None,
            );
            renderer.render_inline_span(
                ui,
                &InlineSpan::Link {
                    text: "local".to_string(),
                    url: "#anchor".to_string(),
                },
                None,
                None,
            );
            let resolved = renderer.resolve_image_path("image.png");
            let _loaded = wait_for_image(&renderer, ctx, ui, &resolved);
            renderer.render_inline_span(ui, &image_span, None, None);
            renderer.render_inline_span(ui, &missing_span, None, None);
            renderer.render_inline_span(ui, &remote_span, None, None);
            renderer.render_inline_span(ui, &remote_http_span, None, None);
        });
    }

    #[test]
    fn test_has_pending_renders_with_image_pending() {
        let renderer = MarkdownRenderer::new();
        assert!(!renderer.has_pending_renders());
        renderer
            .image_pending
            .borrow_mut()
            .insert("queued.png".to_string());
        assert!(renderer.has_pending_renders());
    }

    #[test]
    fn test_has_pending_renders_with_mermaid_pending() {
        let renderer = MarkdownRenderer::new();
        // Give worker threads time to start up under heavy parallel test execution.
        // Workers need to initialize QuickJS runtime which can be slow under load.
        std::thread::sleep(std::time::Duration::from_millis(200));
        with_test_ui(|_, ui| {
            renderer.mermaid.begin_frame();
            renderer
                .mermaid
                .render_block(ui, "graph TD; A-->B;", 1.0, 14.0);
        });
        assert!(renderer.has_pending_renders());
    }

    #[test]
    fn test_parse_lists_and_blockquotes() {
        let renderer = MarkdownRenderer::new();
        let md = "\
- Item 1
- Item 2
  - Nested

1. First
2. Second

> Quote line
> > Nested quote

---";

        let elements = renderer.parse(md).expect("parse ok");
        assert!(elements
            .iter()
            .any(|el| matches!(el, MarkdownElement::List { ordered: false, .. })));
        assert!(elements
            .iter()
            .any(|el| matches!(el, MarkdownElement::List { ordered: true, .. })));
        assert!(elements
            .iter()
            .any(|el| matches!(el, MarkdownElement::Quote { .. })));
        assert!(elements
            .iter()
            .any(|el| matches!(el, MarkdownElement::HorizontalRule)));
    }

    #[test]
    fn test_elements_to_plain_text_variants() {
        let elements = vec![
            MarkdownElement::Header {
                level: 1,
                spans: vec![InlineSpan::Text("Title".to_string())],
                id: "title".to_string(),
            },
            MarkdownElement::Paragraph(vec![
                InlineSpan::Text("Hello".to_string()),
                InlineSpan::Code("code".to_string()),
            ]),
            MarkdownElement::CodeBlock {
                language: Some("rust".to_string()),
                text: "fn main() {}".to_string(),
                highlighted: None,
            },
            MarkdownElement::List {
                ordered: false,
                items: vec![ListItem {
                    blocks: vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
                        "Item".to_string(),
                    )])],
                }],
            },
            MarkdownElement::Quote {
                depth: 1,
                blocks: vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
                    "Quote".to_string(),
                )])],
            },
            MarkdownElement::HorizontalRule,
            MarkdownElement::Table {
                headers: vec![vec![InlineSpan::Text("H".to_string())]],
                rows: vec![vec![vec![InlineSpan::Image {
                    src: "img.png".to_string(),
                    alt: "Alt".to_string(),
                    title: None,
                }]]],
                alignments: Vec::new(),
            },
        ];
        let text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert!(text.contains("Title"));
        assert!(text.contains("Hello"));
        assert!(text.contains("fn main"));
        assert!(text.contains("Item"));
        assert!(text.contains("Quote"));
        assert!(text.contains("Alt"));
    }

    #[test]
    fn test_elements_to_plain_text_image_title_and_empty_rule() {
        let elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Image {
            src: "img.png".to_string(),
            alt: "".to_string(),
            title: Some("Diagram Title".to_string()),
        }])];
        let text = MarkdownRenderer::elements_to_plain_text(&elements);
        assert!(text.contains("Diagram Title"));

        let hr_only = vec![MarkdownElement::HorizontalRule];
        assert_eq!(MarkdownRenderer::elements_to_plain_text(&hr_only), "");
    }

    #[test]
    fn test_table_rendering_overhaul() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![
            vec![InlineSpan::Text("H1".to_string())],
            vec![InlineSpan::Text("H2".to_string())],
        ];
        let rows = vec![vec![
            vec![InlineSpan::Text("Cell".to_string())],
            vec![InlineSpan::Link {
                text: "Link".to_string(),
                url: "https://example.com".to_string(),
            }],
        ]];
        let elements = vec![MarkdownElement::Table {
            headers,
            rows,
            alignments: Vec::new(),
        }];

        with_test_ui(|_, ui| {
            renderer.render_to_ui(ui, &elements);
        });
    }

    #[test]
    fn test_measure_inline_spans_and_emoji_texture() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![
            InlineSpan::Text("Hello".to_string()),
            InlineSpan::Text("🙂".to_string()),
        ];

        with_test_ui(|_, ui| {
            let width = renderer.measure_inline_spans(ui, &spans);
            assert!(width > 0.0);
            let tex = renderer.get_or_make_emoji_texture(ui, "👾");
            assert!(tex.size()[0] > 0);
        });

        let img = renderer.generate_emoji_image("👾", 16);
        assert_eq!(img.size[0], 16);
    }

    #[test]
    fn test_measure_inline_spans_respects_line_breaks() {
        let renderer = MarkdownRenderer::new();
        let single_line = vec![InlineSpan::Text(
            "Short This is a much longer line".to_string(),
        )];
        let multi_line = vec![InlineSpan::Text(
            "Short\nThis is a much longer line".to_string(),
        )];

        with_test_ui(|_, ui| {
            let single = renderer.measure_inline_spans(ui, &single_line);
            let multi = renderer.measure_inline_spans(ui, &multi_line);
            assert!(multi < single);
        });
    }

    #[test]
    fn test_measure_inline_spans_normalizes_unicode() {
        let renderer = MarkdownRenderer::new();
        let unicode = vec![InlineSpan::Text("A\u{2192}B".to_string())];
        let ascii = vec![InlineSpan::Text("A->B".to_string())];

        with_test_ui(|_, ui| {
            let unicode_width = renderer.measure_inline_spans(ui, &unicode);
            let ascii_width = renderer.measure_inline_spans(ui, &ascii);
            assert!((unicode_width - ascii_width).abs() < 0.5);
        });
    }

    #[test]
    fn test_to_superscript_full_mapping() {
        let input = "0123456789+-=()abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ#";
        let out = MarkdownRenderer::to_superscript(input);
        assert_eq!(out.chars().count(), input.chars().count());
        assert!(out.contains('\u{2070}'));
        assert!(out.contains('\u{00b9}'));
        assert!(out.contains('\u{1d43}'));
        assert!(out.contains('#'));
    }

    #[test]
    fn test_element_plain_text_variants() {
        let list = MarkdownElement::List {
            ordered: false,
            items: vec![ListItem {
                blocks: vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
                    "Item".to_string(),
                )])],
            }],
        };
        let quote = MarkdownElement::Quote {
            depth: 1,
            blocks: vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
                "Quote".to_string(),
            )])],
        };
        let table = MarkdownElement::Table {
            headers: vec![vec![InlineSpan::Text("Header".to_string())]],
            rows: vec![vec![vec![InlineSpan::Text("Cell".to_string())]]],
            alignments: Vec::new(),
        };

        assert!(MarkdownRenderer::element_plain_text(&list).contains("Item"));
        assert!(MarkdownRenderer::element_plain_text(&quote).contains("Quote"));
        assert_eq!(
            MarkdownRenderer::element_plain_text(&MarkdownElement::HorizontalRule),
            "---"
        );
        assert!(MarkdownRenderer::element_plain_text(&table).contains("Header"));
        assert!(MarkdownRenderer::element_plain_text(&table).contains("Cell"));
    }

    #[test]
    fn test_parse_element_image_outside_paragraph() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::Text("Alt".into()),
            Event::End(Tag::Image(LinkType::Inline, "".into(), "".into())),
        ];
        let mut elements = Vec::new();
        let mut slugs = std::collections::HashMap::new();
        let next = renderer.parse_element(&events, 0, &mut elements, &mut slugs);
        assert_eq!(next, events.len());
        assert!(matches!(
            elements.first(),
            Some(MarkdownElement::Paragraph(_))
        ));
        let spans = paragraph_spans(elements.first().expect("element")).expect("paragraph spans");
        let (_src, _alt, title) = image_span_fields(&spans[0]).expect("image span");
        assert_eq!(title, Some("Title"));
        assert!(paragraph_spans(&MarkdownElement::HorizontalRule).is_none());
    }

    #[test]
    fn test_parse_element_empty_paragraph_skips() {
        let renderer = MarkdownRenderer::new();
        let events = vec![Event::Start(Tag::Paragraph), Event::End(Tag::Paragraph)];
        let mut elements = Vec::new();
        let mut slugs = HashMap::new();
        let next = renderer.parse_element(&events, 0, &mut elements, &mut slugs);
        assert_eq!(next, events.len());
        assert!(elements.is_empty());
    }

    #[test]
    fn test_parse_element_empty_blockquote_skips() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::BlockQuote),
            Event::Start(Tag::BlockQuote),
            Event::End(Tag::BlockQuote),
            Event::End(Tag::BlockQuote),
        ];
        let mut elements = Vec::new();
        let mut slugs = HashMap::new();
        let next = renderer.parse_element(&events, 0, &mut elements, &mut slugs);
        assert_eq!(next, events.len());
        assert!(elements.is_empty());
    }

    #[test]
    fn test_parse_list_event_variants() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::List(None)),
            Event::Start(Tag::Item),
            Event::Start(Tag::Emphasis),
            Event::Text("em".into()),
            Event::End(Tag::Emphasis),
            Event::Start(Tag::Strong),
            Event::Text("strong".into()),
            Event::End(Tag::Strong),
            Event::Start(Tag::Strikethrough),
            Event::Text("strike".into()),
            Event::End(Tag::Strikethrough),
            Event::Start(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Text("link".into()),
            Event::End(Tag::Link(LinkType::Inline, "".into(), "".into())),
            Event::Start(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::Text("alt".into()),
            Event::End(Tag::Image(LinkType::Inline, "".into(), "".into())),
            Event::Code("code".into()),
            Event::Text("text".into()),
            Event::SoftBreak,
            Event::Start(Tag::List(Some(1))),
            Event::Start(Tag::Item),
            Event::Text("nested".into()),
            Event::End(Tag::Item),
            Event::End(Tag::List(Some(1))),
            Event::End(Tag::Item),
            Event::End(Tag::List(None)),
        ];

        let mut slugs = std::collections::HashMap::new();
        let (items, next) = renderer.parse_list(&events, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert_eq!(items.len(), 1);
        let item = &items[0];
        let mut blocks = Vec::new();
        blocks.push(MarkdownElement::HorizontalRule);
        blocks.extend(item.blocks.clone());
        let spans = blocks
            .iter()
            .find_map(|block| match block {
                MarkdownElement::Paragraph(spans) => Some(spans),
                _ => None,
            })
            .expect("paragraph block");
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Emphasis(_))));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Strong(_))));
        assert!(spans
            .iter()
            .any(|s| matches!(s, InlineSpan::Strikethrough(_))));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Link { .. })));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Image { .. })));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Code(_))));
        assert!(item
            .blocks
            .iter()
            .any(|block| matches!(block, MarkdownElement::List { ordered: true, .. })));
        Ok(())
    }

    #[test]
    fn test_parse_inline_spans_with_breaks_variants() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Paragraph),
            Event::Text("Hello".into()),
            Event::SoftBreak,
            Event::Code("code".into()),
            Event::Start(Tag::Strong),
            Event::Text("bold".into()),
            Event::End(Tag::Strong),
            Event::Start(Tag::Emphasis),
            Event::Text("em".into()),
            Event::End(Tag::Emphasis),
            Event::Start(Tag::Strikethrough),
            Event::Text("strike".into()),
            Event::End(Tag::Strikethrough),
            Event::Start(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Text("link".into()),
            Event::End(Tag::Link(LinkType::Inline, "".into(), "".into())),
            Event::Start(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::Text("alt".into()),
            Event::End(Tag::Image(LinkType::Inline, "".into(), "".into())),
            Event::End(Tag::Paragraph),
        ];

        let (spans, next) =
            renderer.parse_inline_spans_with_breaks(&events, 1, Tag::Paragraph, true);
        assert_eq!(next, events.len());
        let mut saw_code = false;
        let mut saw_strong = false;
        let mut saw_emphasis = false;
        let mut saw_strike = false;
        let mut saw_link = false;
        let mut saw_image = false;
        let mut saw_break_text = false;
        for span in &spans {
            match span {
                InlineSpan::Code(_) => saw_code = true,
                InlineSpan::Strong(_) => saw_strong = true,
                InlineSpan::Emphasis(_) => saw_emphasis = true,
                InlineSpan::Strikethrough(_) => saw_strike = true,
                InlineSpan::Link { .. } => saw_link = true,
                InlineSpan::Image { .. } => saw_image = true,
                InlineSpan::Text(t) if t.contains('\n') => saw_break_text = true,
                _ => {}
            }
        }
        assert!(saw_code);
        assert!(saw_strong);
        assert!(saw_emphasis);
        assert!(saw_strike);
        assert!(saw_link);
        assert!(saw_image);
        assert!(saw_break_text);
    }

    #[test]
    fn test_parse_inline_spans_html_breaks() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Paragraph),
            Event::Text("one".into()),
            Event::Html("<br>".into()),
            Event::Text("two".into()),
            Event::End(Tag::Paragraph),
        ];

        let (spans, next) =
            renderer.parse_inline_spans_with_breaks(&events, 1, Tag::Paragraph, true);
        assert_eq!(next, events.len());
        assert!(spans
            .iter()
            .any(|s| matches!(s, InlineSpan::Text(t) if t == "\n")));

        let (spans_no_break, _) = renderer.parse_inline_spans(&events, 1, Tag::Paragraph);
        let text = MarkdownRenderer::spans_plain_text(&spans_no_break);
        assert!(text.contains("one two"));

        let attr_events = vec![
            Event::Start(Tag::Paragraph),
            Event::Text("alpha".into()),
            Event::Html("<br class=\"tight\">".into()),
            Event::Text("beta".into()),
            Event::End(Tag::Paragraph),
        ];
        let (spans, next) =
            renderer.parse_inline_spans_with_breaks(&attr_events, 1, Tag::Paragraph, true);
        assert_eq!(next, attr_events.len());
        assert!(spans
            .iter()
            .any(|s| matches!(s, InlineSpan::Text(t) if t == "\n")));

        let strong_events = vec![
            Event::Start(Tag::Paragraph),
            Event::Text("lead ".into()),
            Event::Start(Tag::Strong),
            Event::Text("alpha".into()),
            Event::Html("<br>".into()),
            Event::Text("beta".into()),
            Event::End(Tag::Strong),
            Event::End(Tag::Paragraph),
        ];
        let (_spans, next) =
            renderer.parse_inline_spans_with_breaks(&strong_events, 1, Tag::Paragraph, true);
        assert_eq!(next, strong_events.len());

        let (spans_no_break, _) = renderer.parse_inline_spans(&strong_events, 1, Tag::Paragraph);
        let mut saw_non_strong = false;
        let mut strong_text = None;
        for span in &spans_no_break {
            if let InlineSpan::Strong(text) = span {
                strong_text = Some(text.as_str());
            } else {
                saw_non_strong = true;
            }
        }
        assert!(saw_non_strong);
        let mut saw_match = false;
        let mut saw_miss = false;
        let mut saw_none = false;
        for candidate in [strong_text, Some("nope"), None] {
            if let Some(text) = candidate {
                if text.contains("alpha beta") {
                    saw_match = true;
                } else {
                    saw_miss = true;
                }
            } else {
                saw_none = true;
            }
        }
        assert!(saw_match);
        assert!(saw_miss);
        assert!(saw_none);
    }

    #[test]
    fn test_parse_inline_spans_soft_break_without_keep() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Paragraph),
            Event::Text("alpha".into()),
            Event::SoftBreak,
            Event::Text("beta".into()),
            Event::End(Tag::Paragraph),
        ];

        let (spans, next) = renderer.parse_inline_spans(&events, 1, Tag::Paragraph);
        assert_eq!(next, events.len());
        assert_eq!(MarkdownRenderer::spans_plain_text(&spans), "alpha beta");
    }

    #[test]
    fn test_collect_blockquotes_nested_and_breaks() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::BlockQuote),
            Event::Start(Tag::Paragraph),
            Event::Text("Line1\nLine2".into()),
            Event::End(Tag::Paragraph),
            Event::SoftBreak,
            Event::Start(Tag::BlockQuote),
            Event::Start(Tag::Paragraph),
            Event::Text("Nested".into()),
            Event::End(Tag::Paragraph),
            Event::End(Tag::BlockQuote),
            Event::End(Tag::BlockQuote),
        ];
        let mut slugs = HashMap::new();
        let (quotes, next) = renderer.collect_blockquotes(&events, 1, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert!(quotes.iter().any(|(depth, _)| *depth == 1));
        assert!(quotes.iter().any(|(depth, _)| *depth == 2));
    }

    #[test]
    fn test_collect_blockquotes_event_variants() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::BlockQuote),
            Event::Start(Tag::Paragraph),
            Event::Text("para".into()),
            Event::End(Tag::Paragraph),
            Event::Start(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Text("Heading".into()),
            Event::End(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Start(Tag::List(None)),
            Event::Start(Tag::Item),
            Event::Text("item".into()),
            Event::End(Tag::Item),
            Event::End(Tag::List(None)),
            Event::Start(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Fenced(
                "rust".into(),
            ))),
            Event::Text("fn main() {}\n".into()),
            Event::End(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Fenced(
                "rust".into(),
            ))),
            Event::Start(Tag::Table(vec![Alignment::Left])),
            Event::Start(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("H".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::End(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("R".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::End(Tag::Table(vec![Alignment::Left])),
            Event::Start(Tag::Emphasis),
            Event::Text("em".into()),
            Event::End(Tag::Emphasis),
            Event::Start(Tag::Strong),
            Event::Text("strong".into()),
            Event::End(Tag::Strong),
            Event::Start(Tag::Strikethrough),
            Event::Text("strike".into()),
            Event::End(Tag::Strikethrough),
            Event::Start(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Text("link".into()),
            Event::End(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Start(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::Text("alt".into()),
            Event::End(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::Code("code".into()),
            Event::SoftBreak,
            Event::HardBreak,
            Event::Html("<br>".into()),
            Event::Rule,
            Event::Start(Tag::BlockQuote),
            Event::Start(Tag::Paragraph),
            Event::Text("Nested".into()),
            Event::End(Tag::Paragraph),
            Event::End(Tag::BlockQuote),
            Event::End(Tag::BlockQuote),
        ];
        let mut slugs = HashMap::new();
        let (quotes, next) = renderer.collect_blockquotes(&events, 1, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert!(!quotes.is_empty());
    }

    #[test]
    fn test_collect_blockquotes_empty_paragraph_and_html_ignored() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::BlockQuote),
            Event::Start(Tag::Paragraph),
            Event::End(Tag::Paragraph),
            Event::Html("<span>".into()),
            Event::End(Tag::BlockQuote),
        ];
        let mut slugs = HashMap::new();
        let (quotes, next) = renderer.collect_blockquotes(&events, 1, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert!(quotes.is_empty());
    }

    #[test]
    fn test_collect_blockquotes_empty_unclosed_returns_empty() {
        let renderer = MarkdownRenderer::new();
        let events = vec![Event::Start(Tag::BlockQuote)];
        let mut slugs = HashMap::new();
        let (quotes, next) = renderer.collect_blockquotes(&events, 1, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert!(quotes.is_empty());
    }

    #[test]
    fn test_parse_list_block_elements() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::List(Some(1))),
            Event::Start(Tag::Item),
            Event::Start(Tag::Heading(pulldown_cmark::HeadingLevel::H3, None, vec![])),
            Event::Text("Heading".into()),
            Event::End(Tag::Heading(pulldown_cmark::HeadingLevel::H3, None, vec![])),
            Event::Start(Tag::Table(vec![Alignment::Left])),
            Event::Start(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("H".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::End(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("R".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::End(Tag::Table(vec![Alignment::Left])),
            Event::Start(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Fenced(
                "rust".into(),
            ))),
            Event::Text("fn main() {}\n".into()),
            Event::End(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Fenced(
                "rust".into(),
            ))),
            Event::Start(Tag::BlockQuote),
            Event::Start(Tag::Paragraph),
            Event::Text("Quote".into()),
            Event::End(Tag::Paragraph),
            Event::End(Tag::BlockQuote),
            Event::Rule,
            Event::HardBreak,
            Event::End(Tag::Item),
            Event::End(Tag::List(Some(1))),
        ];
        let mut slugs = HashMap::new();
        let (items, next) = renderer.parse_list(&events, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert_eq!(items.len(), 1);
        Ok(())
    }

    #[test]
    fn test_parse_element_variants() {
        let renderer = MarkdownRenderer::new();
        let mut slugs = HashMap::new();
        let mut elements = Vec::new();

        let paragraph = vec![
            Event::Start(Tag::Paragraph),
            Event::Text("para".into()),
            Event::End(Tag::Paragraph),
        ];
        let next = renderer.parse_element(&paragraph, 0, &mut elements, &mut slugs);
        assert_eq!(next, paragraph.len());

        let heading = vec![
            Event::Start(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Text("Heading".into()),
            Event::End(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
        ];
        let next = renderer.parse_element(&heading, 0, &mut elements, &mut slugs);
        assert_eq!(next, heading.len());

        let code_block = vec![
            Event::Start(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Fenced(
                "rust".into(),
            ))),
            Event::Text("fn main() {}\n".into()),
            Event::End(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Fenced(
                "rust".into(),
            ))),
        ];
        let next = renderer.parse_element(&code_block, 0, &mut elements, &mut slugs);
        assert_eq!(next, code_block.len());

        let list = vec![
            Event::Start(Tag::List(None)),
            Event::Start(Tag::Item),
            Event::Text("item".into()),
            Event::End(Tag::Item),
            Event::End(Tag::List(None)),
        ];
        let next = renderer.parse_element(&list, 0, &mut elements, &mut slugs);
        assert_eq!(next, list.len());

        let quote = vec![
            Event::Start(Tag::BlockQuote),
            Event::Start(Tag::Paragraph),
            Event::Text("quote".into()),
            Event::End(Tag::Paragraph),
            Event::End(Tag::BlockQuote),
        ];
        let next = renderer.parse_element(&quote, 0, &mut elements, &mut slugs);
        assert_eq!(next, quote.len());

        let table = vec![
            Event::Start(Tag::Table(vec![Alignment::Left])),
            Event::Start(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("H".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::End(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("R".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::End(Tag::Table(vec![Alignment::Left])),
        ];
        let next = renderer.parse_element(&table, 0, &mut elements, &mut slugs);
        assert_eq!(next, table.len());

        let rule = vec![Event::Rule];
        let next = renderer.parse_element(&rule, 0, &mut elements, &mut slugs);
        assert_eq!(next, rule.len());
    }

    #[test]
    fn test_parse_code_block_with_language() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let elements = renderer.parse("Intro\n\n```rust\nfn main() {}\n```")?;
        let mut rust_lang = None;
        for element in &elements {
            if let MarkdownElement::CodeBlock { language, .. } = element {
                rust_lang = language.as_deref();
            }
        }
        assert_eq!(rust_lang, Some("rust"));
        Ok(())
    }

    #[test]
    fn test_parse_code_block_empty_language() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Fenced(
                "".into(),
            ))),
            Event::Text("fn main() {}".into()),
            Event::End(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Fenced(
                "".into(),
            ))),
        ];
        let (code, language, next) = renderer.parse_code_block(&events, 0);
        assert_eq!(language, None);
        assert_eq!(code, "fn main() {}");
        assert_eq!(next, events.len());
    }

    #[test]
    fn test_parse_code_block_indented() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Indented)),
            Event::Text("let x = 1;".into()),
            Event::End(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Indented)),
        ];
        let (code, language, next) = renderer.parse_code_block(&events, 0);
        assert_eq!(language, None);
        assert_eq!(code, "let x = 1;");
        assert_eq!(next, events.len());
    }

    #[test]
    fn test_parse_table_from_markdown() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md = "| H1 | H2 |\n| --- | --- |\n| A | B |\n| C | D |\n";
        let elements = renderer.parse(md)?;
        assert!(elements
            .iter()
            .any(|el| matches!(el, MarkdownElement::Table { .. })));
        Ok(())
    }

    #[test]
    fn test_hash_inline_spans_variants() {
        let spans = vec![
            InlineSpan::Text("t".to_string()),
            InlineSpan::Code("c".to_string()),
            InlineSpan::Strong("s".to_string()),
            InlineSpan::Emphasis("e".to_string()),
            InlineSpan::Strikethrough("x".to_string()),
            InlineSpan::Link {
                text: "link".to_string(),
                url: "https://example.com".to_string(),
            },
            InlineSpan::Image {
                src: "img.png".to_string(),
                alt: "alt".to_string(),
                title: Some("title".to_string()),
            },
        ];
        let hash = MarkdownRenderer::hash_inline_spans(&spans);
        let hash2 = MarkdownRenderer::hash_inline_spans(&[InlineSpan::Text("other".to_string())]);
        assert_ne!(hash, hash2);
    }

    #[test]
    fn test_highlight_segments_edge_cases() {
        let renderer = MarkdownRenderer::new();
        assert!(renderer.highlight_segments("", Some("a")).is_empty());
        assert_eq!(
            renderer.highlight_segments("abc", None),
            vec![(0..3, false)]
        );
        assert_eq!(
            renderer.highlight_segments("abc", Some("")),
            vec![(0..3, false)]
        );
        let segments = renderer.highlight_segments("Hello", Some("ell"));
        assert!(segments.iter().any(|(_, highlighted)| *highlighted));
    }

    #[test]
    fn test_resolve_table_widths_branches() {
        assert!(MarkdownRenderer::resolve_table_widths(50.0, &[], &[]).is_empty());
        assert_eq!(
            MarkdownRenderer::resolve_table_widths(100.0, &[10.0, 10.0], &[20.0, 30.0]),
            vec![20.0, 30.0]
        );

        assert_eq!(
            MarkdownRenderer::resolve_table_widths(10.0, &[0.0, 0.0], &[0.0, 0.0]),
            vec![5.0, 5.0]
        );

        assert_eq!(
            MarkdownRenderer::resolve_table_widths(10.0, &[10.0, 10.0], &[20.0, 20.0]),
            vec![5.0, 5.0]
        );

        assert_eq!(
            MarkdownRenderer::resolve_table_widths(30.0, &[10.0, 10.0], &[10.0, 10.0]),
            vec![15.0, 15.0]
        );

        let widths = MarkdownRenderer::resolve_table_widths(30.0, &[10.0, 10.0], &[20.0, 30.0]);
        assert_eq!(widths.len(), 2);
        assert!((widths[0] + widths[1] - 30.0).abs() < 0.1);
    }

    #[test]
    fn test_table_line_breaks_case_parses_all_rows() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md = fs::read_to_string("tests/regression/cases/017-table-line-breaks.md")?;
        let elements = renderer.parse(&md)?;
        let rows = elements.iter().find_map(|el| match el {
            MarkdownElement::Table { rows, .. } => Some(rows),
            _ => None,
        });
        let rows = rows.expect("table present");
        assert_eq!(rows.len(), 7);
        let plain = MarkdownRenderer::elements_to_plain_text(&elements);
        assert!(plain.contains("Strong"));
        assert!(plain.contains("Attr"));
        Ok(())
    }

    #[test]
    fn test_table_line_breaks_case_renders_all_rows() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md = fs::read_to_string("tests/regression/cases/017-table-line-breaks.md")?;
        let elements = renderer.parse(&md)?;
        with_test_ui(|ctx, ui| {
            ctx.set_visuals(egui::Visuals::light());
            renderer.render_to_ui(ui, &elements);
        });
        let (rendered, total) = renderer.table_render_stats();
        assert_eq!(rendered, total);
        assert_eq!(rendered, 7);
        Ok(())
    }

    #[test]
    fn test_table_first_column_alignment_case_parses_columns() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md = fs::read_to_string("tests/regression/cases/026-table-first-column-align.md")?;
        let elements = renderer.parse(&md)?;
        let mut tables = elements.iter().filter_map(|el| match el {
            MarkdownElement::Table {
                headers,
                rows,
                alignments,
            } => Some((headers, rows, alignments)),
            _ => None,
        });
        let (headers, rows, alignments) = tables.next().expect("expected table");
        assert_eq!(headers.len(), 3);
        assert_eq!(alignments.len(), 3);
        assert!(rows.iter().all(|row| row.len() == 3));
        Ok(())
    }

    #[test]
    fn test_handle_width_change_requests_repaint() {
        let renderer = MarkdownRenderer::new();
        let ctx = egui::Context::default();
        renderer.handle_width_change(&ctx, 42, WidthChange::Large);
        let metrics = renderer.table_metrics.borrow();
        let entry = metrics.entry(42).expect("metrics entry");
        assert!(entry.last_discard_frame.is_some());
    }

    #[test]
    fn test_handle_width_change_skips_duplicate_frame() {
        let renderer = MarkdownRenderer::new();
        let ctx = egui::Context::default();
        renderer.handle_width_change(&ctx, 42, WidthChange::Large);
        renderer.handle_width_change(&ctx, 42, WidthChange::Large);
        let metrics = renderer.table_metrics.borrow();
        let entry = metrics.entry(42).expect("metrics entry");
        assert_eq!(entry.last_discard_frame, Some(ctx.frame_nr()));
    }

    #[test]
    fn test_render_list_multiline_indent_and_empty() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("First\n  Nested".to_string())];
        let item = ListItem {
            blocks: vec![MarkdownElement::Paragraph(spans.clone())],
        };
        with_test_ui(|ctx, ui| {
            ctx.set_visuals(egui::Visuals::light());
            renderer.render_list(ui, false, &[]);
            renderer.render_list(ui, true, std::slice::from_ref(&item));
        });
    }

    #[test]
    fn test_render_list_paragraph_handles_blank_parts_and_indent() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![
            InlineSpan::Text("Line 1\n\n  indented".to_string()),
            InlineSpan::Image {
                src: "assets/emoji/1f600.png".to_string(),
                alt: "img".to_string(),
                title: None,
            },
        ];
        with_test_ui(|_, ui| {
            renderer.render_list_paragraph(ui, None, Color32::WHITE, 12.0, &spans);
        });
    }

    #[test]
    fn test_render_list_paragraph_multiline_without_leading_spaces() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("Line 1\nLine 2".to_string())];
        with_test_ui(|_, ui| {
            renderer.render_list_paragraph(ui, None, Color32::WHITE, 12.0, &spans);
        });
    }

    #[test]
    fn test_render_inline_span_image_hover_caption_uses_cached_texture() {
        let renderer = MarkdownRenderer::new();
        let span = InlineSpan::Image {
            src: "assets/emoji/1f600.png".to_string(),
            alt: "alt".to_string(),
            title: Some("Caption".to_string()),
        };
        let _guard = ForcedRenderActions::new(&["image_hover"]);
        with_test_ui(|_, ui| {
            let image = egui::ColorImage::new([2, 2], Color32::WHITE);
            let texture = ui.ctx().load_texture(
                "test-inline-image-hover",
                image,
                egui::TextureOptions::LINEAR,
            );
            renderer.store_image_texture("assets/emoji/1f600.png", texture, [2, 2], None);
            renderer.render_inline_span(ui, &span, None, None);
        });
    }

    #[test]
    fn test_render_inline_span_forced_actions_and_image_title() {
        let renderer = MarkdownRenderer::new();
        renderer.set_highlight_phrase(Some("hi"));
        let image_span = InlineSpan::Image {
            src: "assets/emoji/1f600.png".to_string(),
            alt: "alt".to_string(),
            title: Some("Caption".to_string()),
        };
        let missing_span = InlineSpan::Image {
            src: "missing.png".to_string(),
            alt: "missing".to_string(),
            title: None,
        };
        let link_span = InlineSpan::Link {
            text: "hi link".to_string(),
            url: "https://example.com".to_string(),
        };
        let _guard = ForcedRenderActions::new(&["link_hover", "link_click", "image_hover"]);
        with_test_ui(|_, ui| {
            renderer.render_inline_span(
                ui,
                &InlineSpan::Text("hi \u{1f600}".to_string()),
                None,
                None,
            );
            renderer.render_inline_span(ui, &InlineSpan::Strong("hi".to_string()), None, None);
            renderer.render_inline_span(ui, &InlineSpan::Emphasis("hi".to_string()), None, None);
            renderer.render_inline_span(
                ui,
                &InlineSpan::Strikethrough("hi".to_string()),
                None,
                None,
            );
            renderer.render_inline_span(ui, &InlineSpan::Code("code".to_string()), None, None);
            renderer.render_inline_span(ui, &link_span, None, None);
            renderer.render_inline_span(ui, &image_span, None, None);
            renderer.render_inline_span(ui, &missing_span, None, None);
        });
    }

    #[test]
    fn test_get_or_load_image_texture_embedded_and_remote() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|ctx, ui| {
            let embedded = wait_for_image(&renderer, ctx, ui, "assets/emoji/1f600.png");
            assert!(embedded.is_some());
            let remote = renderer.get_or_load_image_texture(ui, "https://example.com/img.png");
            assert!(remote.is_none());
        });
    }

    #[test]
    fn test_get_or_load_image_texture_embedded_cache_hit() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|ctx, ui| {
            let tex = ctx.load_texture(
                "embedded-cache",
                egui::ColorImage::new([2, 2], Color32::WHITE),
                Default::default(),
            );
            renderer.store_image_texture("assets/emoji/1f600.png", tex, [2, 2], None);
            let loaded = renderer.get_or_load_image_texture(ui, "assets/emoji/1f600.png");
            assert!(loaded.is_some());
        });
    }

    #[test]
    fn test_get_or_load_image_texture_skips_recent_failure() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            renderer.note_image_failure("missing.png");
            let loaded = renderer.get_or_load_image_texture(ui, "missing.png");
            assert!(loaded.is_none());
            assert!(renderer
                .image_failures
                .borrow()
                .contains_key(&"missing.png".to_string()));
        });
    }

    #[test]
    fn test_get_or_load_image_texture_skips_pending() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            renderer
                .image_pending
                .borrow_mut()
                .insert("pending.png".to_string());
            let loaded = renderer.get_or_load_image_texture(ui, "pending.png");
            assert!(loaded.is_none());
        });
    }

    #[test]
    fn test_render_inline_span_loaded_image_without_title() {
        let renderer = MarkdownRenderer::new();
        let span = InlineSpan::Image {
            src: "assets/emoji/1f600.png".to_string(),
            alt: "".to_string(),
            title: None,
        };
        with_test_ui(|ctx, ui| {
            let _ = wait_for_image(&renderer, ctx, ui, "assets/emoji/1f600.png");
            renderer.render_inline_span(ui, &span, None, None);
        });
    }

    #[test]
    fn test_get_or_make_emoji_texture_cache() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            let tex = renderer.get_or_make_emoji_texture(ui, "\u{1f600}");
            assert!(tex.size()[0] > 0);
            let cached = renderer.get_or_make_emoji_texture(ui, "\u{1f600}");
            assert_eq!(cached.size(), tex.size());
        });
    }

    #[test]
    fn test_get_or_make_emoji_texture_unknown_fallbacks() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            let tex = renderer.get_or_make_emoji_texture(ui, "not-emoji");
            assert!(tex.size()[0] > 0);
        });
    }

    #[test]
    fn test_render_overhauled_cell_fragments() {
        let renderer = MarkdownRenderer::new();
        let emoji_span = InlineSpan::Text("\u{1f600}".to_string());
        let image_span = InlineSpan::Image {
            src: "assets/emoji/1f600.png".to_string(),
            alt: "alt".to_string(),
            title: None,
        };
        with_test_ui(|_, ui| {
            renderer.render_overhauled_cell(ui, &[], 120.0, false, Some(0), 0, Align::LEFT);
            renderer.render_overhauled_cell(
                ui,
                &[emoji_span.clone(), image_span.clone()],
                120.0,
                false,
                Some(1),
                1,
                Align::LEFT,
            );
        });
    }

    #[test]
    fn test_paint_table_text_job_link_interaction() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Link {
            text: "Click here for more text".to_string(),
            url: "https://example.com".to_string(),
        }];
        let ctx = egui::Context::default();
        let click_pos = egui::pos2(5.0, 5.0);

        let input = input_with_click(click_pos, egui::PointerButton::Secondary);
        run_frame_with_input(&ctx, input, |_, ui| {
            ui.allocate_ui_at_rect(
                egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 80.0)),
                |ui| {
                    let build =
                        renderer.build_layout_job(ui.style(), &spans, 30.0, false, Align::LEFT);
                    renderer.paint_table_text_job(ui, 120.0, build);
                },
            );
        });

        let input = input_with_click(click_pos, egui::PointerButton::Primary);
        run_frame_with_input(&ctx, input, |_, ui| {
            ui.allocate_ui_at_rect(
                egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 80.0)),
                |ui| {
                    let build =
                        renderer.build_layout_job(ui.style(), &spans, 30.0, false, Align::LEFT);
                    renderer.paint_table_text_job(ui, 120.0, build);
                },
            );
        });
    }

    #[test]
    fn test_aligned_text_origin_anchors_to_rect() {
        let rect = egui::Rect::from_min_size(egui::pos2(10.0, 20.0), egui::vec2(100.0, 20.0));
        let galley = Arc::new(Galley {
            job: Arc::new(LayoutJob::default()),
            rows: Vec::new(),
            elided: false,
            rect: egui::Rect::from_min_max(egui::pos2(0.0, 0.0), egui::pos2(20.0, 10.0)),
            mesh_bounds: egui::Rect::NOTHING,
            num_vertices: 0,
            num_indices: 0,
            pixels_per_point: 1.0,
        });
        let left_origin = MarkdownRenderer::aligned_text_origin(rect, &galley, Align::LEFT);
        let center_origin = MarkdownRenderer::aligned_text_origin(rect, &galley, Align::Center);
        let right_origin = MarkdownRenderer::aligned_text_origin(rect, &galley, Align::RIGHT);

        assert_eq!(left_origin.x, rect.left());
        assert_eq!(center_origin.x, rect.center().x - galley.rect.center().x);
        assert_eq!(right_origin.x, rect.right() - galley.rect.right());
        assert_eq!(left_origin.y, rect.top());
    }

    #[test]
    fn test_layout_job_center_rect_is_origin() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("Item".to_string())];
        with_test_ui(|_, ui| {
            let build = renderer.build_layout_job(ui.style(), &spans, 200.0, true, Align::Center);
            let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
            assert!(galley.rect.center().x.abs() <= 0.01);
        });
    }

    #[test]
    fn test_render_table_tablebuilder_variants() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![vec![InlineSpan::Text("H1".to_string())]];
        let rows = vec![vec![
            vec![InlineSpan::Text("A".to_string())],
            vec![InlineSpan::Text("B".to_string())],
        ]];
        with_test_ui(|_, ui| {
            let table_id = renderer.compute_table_id(&headers, &rows, &[], 0);
            renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
            let empty_id = renderer.compute_table_id(&headers, &[], &[], 1);
            renderer.render_table_tablebuilder(ui, &headers, &[], &[], empty_id);
        });
    }

    #[test]
    fn test_render_code_block_context_menu_clicks() {
        let renderer = MarkdownRenderer::new();
        let ctx = egui::Context::default();
        let input = input_with_click(egui::pos2(5.0, 5.0), egui::PointerButton::Primary);
        run_frame_with_input(&ctx, input, |_, ui| {
            ui.allocate_ui_at_rect(
                egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 80.0)),
                |ui| {
                    renderer.render_code_block_context_menu(ui, "code", Some("rust"));
                },
            );
        });

        let input = input_with_click(egui::pos2(5.0, 30.0), egui::PointerButton::Primary);
        run_frame_with_input(&ctx, input, |_, ui| {
            ui.allocate_ui_at_rect(
                egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 80.0)),
                |ui| {
                    renderer.render_code_block_context_menu(ui, "code", Some("rust"));
                },
            );
        });
    }

    #[test]
    fn test_find_syntax_for_language() {
        let renderer = MarkdownRenderer::new();
        assert!(renderer.find_syntax_for_language("rust").is_some());
        assert!(renderer.find_syntax_for_language("nonexistent").is_none());
    }

    #[test]
    fn test_find_syntax_for_language_direct_name_match() -> Result<()> {
        let mut renderer = MarkdownRenderer::new();
        let syntax = SyntaxDefinition::load_from_str(
            r#"
name: testlang
scope: source.testlang
file_extensions: [tst]
contexts:
  main:
    - match: '.'
      scope: source.testlang
"#,
            true,
            None,
        )
        .expect("syntax");
        let mut builder = renderer.syntax_set.clone().into_builder();
        builder.add(syntax);
        renderer.syntax_set = builder.build();
        assert!(renderer.find_syntax_for_language("testlang").is_some());
        Ok(())
    }

    #[test]
    fn test_find_syntax_for_language_mappings() {
        let renderer = MarkdownRenderer::new();
        assert!(renderer.find_syntax_for_language("markdown").is_some());
        assert!(renderer.find_syntax_for_language("shell").is_some());
        assert!(renderer.find_syntax_for_language("yaml").is_some());
    }

    #[test]
    fn test_find_syntax_for_language_mapped_names() {
        let renderer = MarkdownRenderer::new();
        let langs = [
            "python",
            "javascript",
            "typescript",
            "c++",
            "cpp",
            "c#",
            "shell",
            "bash",
            "powershell",
            "yaml",
            "markdown",
            "sql",
            "toml",
        ];
        for lang in langs {
            let _ = renderer.find_syntax_for_language(lang);
        }
    }

    #[test]
    fn test_find_syntax_for_language_aliases_with_empty_syntax_set() {
        let mut renderer = MarkdownRenderer::new();
        renderer.syntax_set = SyntaxSet::new();
        let langs = [
            "rust",
            "python",
            "javascript",
            "typescript",
            "c++",
            "cpp",
            "c#",
            "csharp",
            "shell",
            "bash",
            "powershell",
            "yaml",
            "markdown",
            "html",
            "css",
            "java",
            "go",
            "php",
            "ruby",
            "xml",
            "json",
            "sql",
            "toml",
            "unknownlang",
        ];
        for lang in langs {
            assert!(renderer.find_syntax_for_language(lang).is_none());
        }
    }

    #[test]
    fn test_estimate_table_image_height_with_title() {
        let renderer = MarkdownRenderer::new();
        let span = InlineSpan::Image {
            src: "assets/samples/logo.svg".to_string(),
            alt: "logo".to_string(),
            title: Some("Caption".to_string()),
        };
        with_test_ui(|_, ui| {
            let height = renderer.estimate_table_image_height(ui, &span, 120.0);
            assert!(height > 0.0);
            let fallback = renderer.estimate_table_image_height(
                ui,
                &InlineSpan::Text("text".to_string()),
                120.0,
            );
            assert!(fallback > 0.0);
        });
    }

    #[test]
    fn test_render_table_emoji() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            renderer.render_table_emoji(ui, "\u{1f600}");
        });
    }

    #[test]
    fn test_render_list_block_non_paragraph() {
        let renderer = MarkdownRenderer::new();
        let block = MarkdownElement::CodeBlock {
            language: Some("rust".to_string()),
            text: "fn main() {}".to_string(),
            highlighted: None,
        };
        with_test_ui(|_, ui| {
            renderer.render_list_block(ui, &block, 12.0, Color32::WHITE);
        });
    }

    #[test]
    fn test_measure_inline_spans_with_image() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Image {
            src: "assets/samples/logo.svg".to_string(),
            alt: "logo".to_string(),
            title: None,
        }];
        with_test_ui(|_, ui| {
            let width = renderer.measure_inline_spans(ui, &spans);
            assert!(width > 0.0);
        });
    }

    #[test]
    fn test_parse_blockquote_with_nested_blocks() {
        let renderer = MarkdownRenderer::new();
        let md = r#"> # Quote Heading
> Paragraph **bold** *em* ~~strike~~ [link](https://example.com)
> ![alt](assets/samples/logo.svg "Logo")
> - Item one
>   - Nested item
> 
> ```rust
> let x = 1;
> ```
> 
> | H1 | H2 |
> | --- | --- |
> | 🎉 | ![a](assets/samples/logo.svg "Img") |
"#;
        let elements = renderer.parse(md).expect("parse");
        assert!(elements
            .iter()
            .any(|e| matches!(e, MarkdownElement::Quote { .. })));
    }

    #[test]
    fn test_parse_list_with_heading_and_table() {
        let renderer = MarkdownRenderer::new();
        let md = r#"- # Item heading
  Paragraph text with `code` and **strong**
  - Nested child
- | A | B |
  | - | - |
  | 1 | 2 |
- ```rust
  fn main() {}
  ```
- > Quote in list
- ---
"#;
        let elements = renderer.parse(md).expect("parse");
        assert!(elements
            .iter()
            .any(|e| matches!(e, MarkdownElement::List { .. })));
    }

    #[test]
    fn test_parse_list_inline_variants() {
        let renderer = MarkdownRenderer::new();
        let md = r#"- Item with *em* **strong** ~~strike~~ `code` [link](https://example.com) ![alt](assets/emoji/1f600.png "title")  
  next line"#;
        let elements = renderer.parse(md).expect("parse");
        assert!(elements
            .iter()
            .any(|e| matches!(e, MarkdownElement::List { .. })));
    }

    #[test]
    fn test_parse_blockquote_with_varied_content() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let md = r#"
> ### Quote Heading
> Paragraph with *em* **strong** ~~strike~~ `code` [link](https://example.com) ![alt](assets/emoji/1f600.png "title")  
> Next line<br>
>
> - item one
> - item two
>
> | h1 | h2 |
> | --- | --- |
> | a | b |
>
> ```rust
> fn main() {
>     // comment
> }
> ```
>
> > Nested quote
>
> ---
"#;
        let elements = renderer.parse(md)?;
        assert!(elements
            .iter()
            .any(|e| matches!(e, MarkdownElement::Quote { .. })));
        Ok(())
    }

    #[test]
    fn test_find_syntax_for_language_variants() {
        let renderer = MarkdownRenderer::new();
        assert!(renderer.find_syntax_for_language("rs").is_some());
        let cs_ext = renderer.syntax_set.find_syntax_by_extension("cs");
        let cs_lang = renderer.find_syntax_for_language("csharp");
        assert_eq!(cs_lang.is_some(), cs_ext.is_some());
        let ps_ext = renderer.syntax_set.find_syntax_by_extension("ps1");
        let ps_lang = renderer.find_syntax_for_language("powershell");
        assert_eq!(ps_lang.is_some(), ps_ext.is_some());
        assert!(renderer.find_syntax_for_language("madeuplang").is_none());
    }

    #[test]
    fn test_generate_emoji_image_variants() {
        let renderer = MarkdownRenderer::new();
        let emojis = [
            "\u{1f389}",
            "\u{2705}",
            "\u{1f680}",
            "\u{1f642}",
            "\u{1f600}",
            "\u{1f609}",
            "\u{2b50}",
            "\u{1f525}",
            "\u{1f44d}",
            "\u{1f44e}",
            "\u{1f4a1}",
            "\u{2753}",
            "\u{2757}",
            "\u{1f4dd}",
            "\u{1f9e0}",
            "\u{1f9ea}",
            "\u{1f4e6}",
            "\u{1f527}",
            "x",
        ];
        for emoji in emojis {
            let image = renderer.generate_emoji_image(emoji, 8);
            assert_eq!(image.size, [8, 8]);
        }
    }

    #[test]
    fn test_measure_inline_spans_code_and_image() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|ctx, ui| {
            let tex = ctx.load_texture(
                "test-image",
                egui::ColorImage::new([2, 2], Color32::WHITE),
                egui::TextureOptions::LINEAR,
            );
            renderer.image_textures.borrow_mut().insert(
                "image.png".to_string(),
                ImageCacheEntry {
                    texture: tex,
                    size: [64, 32],
                    modified: None,
                    byte_size: ImageCacheEntry::estimate_bytes([64, 32]),
                },
            );
            let spans = vec![
                InlineSpan::Text("Hello".to_string()),
                InlineSpan::Code("line1\nline2".to_string()),
                InlineSpan::Image {
                    src: "image.png".to_string(),
                    alt: "Alt".to_string(),
                    title: None,
                },
            ];
            let width = renderer.measure_inline_spans(ui, &spans);
            assert!(width > 0.0);
        });
    }

    #[test]
    fn test_get_or_load_image_texture_cache_and_remote() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            assert!(renderer
                .get_or_load_image_texture(ui, "https://example.com/logo.png")
                .is_none());
        });

        let temp = tempdir()?;
        let image_path = temp.path().join("image.png");
        std::fs::write(&image_path, tiny_png_bytes())?;
        let resolved = image_path.to_string_lossy().to_string();
        with_test_ui(|ctx, ui| {
            let first = wait_for_image(&renderer, ctx, ui, &resolved);
            assert!(first.is_some());
            let second = renderer.get_or_load_image_texture(ui, &resolved);
            assert!(second.is_some());
        });
        Ok(())
    }

    #[test]
    fn test_get_or_load_image_texture_stale_cache_evicted() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let temp = tempdir()?;
        let image_path = temp.path().join("image.png");
        std::fs::write(&image_path, tiny_png_bytes())?;
        let resolved = image_path.to_string_lossy().to_string();

        with_test_ui(|ctx, ui| {
            let tex = ctx.load_texture(
                "stale-image",
                egui::ColorImage::new([2, 2], Color32::WHITE),
                egui::TextureOptions::LINEAR,
            );
            renderer.image_textures.borrow_mut().insert(
                resolved.clone(),
                ImageCacheEntry {
                    texture: tex,
                    size: [2, 2],
                    modified: Some(SystemTime::UNIX_EPOCH),
                    byte_size: ImageCacheEntry::estimate_bytes([2, 2]),
                },
            );

            let result = renderer.get_or_load_image_texture(ui, &resolved);
            assert!(result.is_none());
            assert!(!renderer.image_textures.borrow().contains_key(&resolved));
            assert!(renderer.image_pending.borrow().contains(&resolved));
        });

        Ok(())
    }

    #[test]
    fn test_get_or_load_image_texture_enqueue_failure() -> Result<()> {
        let mut renderer = MarkdownRenderer::new();
        let (image_job_tx, image_job_rx) = bounded(0);
        drop(image_job_rx);
        renderer.image_job_tx = image_job_tx;

        let temp = tempdir()?;
        let image_path = temp.path().join("image.png");
        std::fs::write(&image_path, tiny_png_bytes())?;
        let resolved = image_path.to_string_lossy().to_string();

        with_test_ui(|_, ui| {
            let result = renderer.get_or_load_image_texture(ui, &resolved);
            assert!(result.is_none());
            assert!(!renderer.image_pending.borrow().contains(&resolved));
        });

        Ok(())
    }

    #[test]
    fn test_poll_image_results_records_failure() {
        let mut renderer = MarkdownRenderer::new();
        let (result_tx, result_rx) = crossbeam_channel::unbounded::<ImageLoadResult>();
        renderer.image_result_rx = result_rx;
        renderer
            .image_pending
            .borrow_mut()
            .insert("failed.png".to_string());

        result_tx
            .send(ImageLoadResult::Failed {
                key: "failed.png".to_string(),
            })
            .expect("send");
        let ctx = egui::Context::default();
        renderer.poll_image_results(&ctx);
        assert!(renderer
            .image_failures
            .borrow()
            .contains_key(&"failed.png".to_string()));
        assert!(!renderer.image_pending.borrow().contains("failed.png"));
    }

    #[test]
    fn test_enqueue_image_job_full_returns_err() {
        let mut renderer = MarkdownRenderer::new();
        let (job_tx, job_rx) = bounded(1);
        renderer.image_job_tx = job_tx;
        let request = ImageLoadRequest {
            key: "full".to_string(),
            source: ImageLoadSource::Embedded(b"png"),
        };
        renderer.image_job_tx.send(request).expect("fill queue");
        let request = ImageLoadRequest {
            key: "full-2".to_string(),
            source: ImageLoadSource::Embedded(b"png"),
        };
        assert!(renderer.enqueue_image_job(request).is_err());
        drop(job_rx);
    }

    #[test]
    fn test_wait_for_image_with_limit_returns_none() {
        let renderer = MarkdownRenderer::new();
        let ctx = egui::Context::default();
        let input = screen_input(320.0, 240.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            let missing = wait_for_image_with_limit(&renderer, &ctx, ui, "missing", 0);
            assert!(missing.is_none());
        });
    }

    #[test]
    fn test_render_table_tablebuilder_policy_mix() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![
            vec![InlineSpan::Text("Version".to_string())],
            vec![InlineSpan::Text("Author".to_string())],
            vec![InlineSpan::Text("Notes".to_string())],
            vec![InlineSpan::Text("Example".to_string())],
            vec![InlineSpan::Text("Misc".to_string())],
        ];
        let rows = vec![vec![
            vec![InlineSpan::Text("1".to_string())],
            vec![InlineSpan::Text("Ada".to_string())],
            vec![InlineSpan::Text("Line 1\nLine 2".to_string())],
            vec![InlineSpan::Image {
                src: "image.png".to_string(),
                alt: "Alt".to_string(),
                title: Some("Title".to_string()),
            }],
            vec![InlineSpan::Link {
                text: "Link".to_string(),
                url: "https://example.com".to_string(),
            }],
        ]];

        let ctx = egui::Context::default();
        let input = screen_input(1200.0, 720.0);
        run_frame_with_input(&ctx, input, |ctx, ui| {
            let tex = ctx.load_texture(
                "table-image",
                egui::ColorImage::new([2, 2], Color32::WHITE),
                egui::TextureOptions::LINEAR,
            );
            renderer.image_textures.borrow_mut().insert(
                "image.png".to_string(),
                ImageCacheEntry {
                    texture: tex,
                    size: [120, 80],
                    modified: None,
                    byte_size: ImageCacheEntry::estimate_bytes([120, 80]),
                },
            );
            let table_id = renderer.compute_table_id(&headers, &rows, &[], 12);
            renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
        });
        let input = screen_input(1200.0, 720.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            let table_id = renderer.compute_table_id(&headers, &rows, &[], 12);
            renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
        });
    }

    #[test]
    fn test_render_table_tablebuilder_extra_columns() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![vec![InlineSpan::Text("H1".to_string())]];
        let rows = vec![vec![
            vec![InlineSpan::Text("C1".to_string())],
            vec![InlineSpan::Text("C2".to_string())],
            vec![InlineSpan::Text("C3".to_string())],
        ]];
        with_test_ui(|_, ui| {
            let table_id = renderer.compute_table_id(&headers, &rows, &[], 13);
            renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
        });
    }

    #[test]
    fn test_render_table_tablebuilder_forced_policies() {
        let renderer = MarkdownRenderer::new();
        let _forced = ForcedTablePolicies::new(vec![
            ColumnPolicy::Auto,
            ColumnPolicy::Fixed {
                width: 120.0,
                clip: true,
            },
            ColumnPolicy::Resizable {
                min: 60.0,
                preferred: 120.0,
                clip: true,
            },
            ColumnPolicy::Remainder { clip: true },
        ]);
        let headers = vec![
            vec![InlineSpan::Text("A".to_string())],
            vec![InlineSpan::Text("B".to_string())],
            vec![InlineSpan::Text("C".to_string())],
            vec![InlineSpan::Text("D".to_string())],
        ];
        let rows = vec![vec![
            vec![InlineSpan::Text("1".to_string())],
            vec![InlineSpan::Text("2".to_string())],
            vec![InlineSpan::Text("3".to_string())],
            vec![InlineSpan::Text("4".to_string())],
        ]];
        with_test_ui(|_, ui| {
            let table_id = renderer.compute_table_id(&headers, &rows, &[], 14);
            renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
        });
    }

    #[test]
    fn test_render_table_tablebuilder_fixed_columns_no_clip() {
        let renderer = MarkdownRenderer::new();
        let _forced = ForcedTablePolicies::new(vec![
            ColumnPolicy::Fixed {
                width: 80.0,
                clip: false,
            },
            ColumnPolicy::Fixed {
                width: 100.0,
                clip: false,
            },
        ]);
        let headers = vec![
            vec![InlineSpan::Text("Left".to_string())],
            vec![InlineSpan::Text("Right".to_string())],
        ];
        let rows = vec![vec![
            vec![InlineSpan::Text("A".to_string())],
            vec![InlineSpan::Text("B".to_string())],
        ]];
        with_test_ui(|_, ui| {
            let layout = *ui.layout();
            ui.allocate_ui_with_layout(Vec2::new(400.0, 0.0), layout, |ui| {
                ui.spacing_mut().item_spacing.x = 6.0;
                ui.set_width(400.0);
                let table_id = renderer.compute_table_id(&headers, &rows, &[], 16);
                renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
            });
        });
    }

    #[test]
    fn test_render_table_tablebuilder_scaled_down_min_flex_forced() {
        let renderer = MarkdownRenderer::new();
        let _forced = ForcedTablePolicies::new(vec![
            ColumnPolicy::Resizable {
                min: 50.0,
                preferred: 120.0,
                clip: false,
            },
            ColumnPolicy::Resizable {
                min: 50.0,
                preferred: 120.0,
                clip: false,
            },
        ]);
        let headers = vec![
            vec![InlineSpan::Text("A".to_string())],
            vec![InlineSpan::Text("B".to_string())],
        ];
        let rows = vec![vec![
            vec![InlineSpan::Text("One".to_string())],
            vec![InlineSpan::Text("Two".to_string())],
        ]];
        with_test_ui(|_, ui| {
            let layout = *ui.layout();
            ui.allocate_ui_with_layout(Vec2::new(106.0, 0.0), layout, |ui| {
                ui.spacing_mut().item_spacing.x = 6.0;
                ui.set_width(106.0);
                let table_id = renderer.compute_table_id(&headers, &rows, &[], 17);
                renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
            });
        });
    }

    #[test]
    fn test_render_table_tablebuilder_scaled_down_min_flex() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![
            vec![InlineSpan::Text("Notes".to_string())],
            vec![InlineSpan::Text("Summary".to_string())],
            vec![InlineSpan::Text("Details".to_string())],
        ];
        let rows = vec![vec![
            vec![InlineSpan::Text("Long text 1".to_string())],
            vec![InlineSpan::Text("Long text 2".to_string())],
            vec![InlineSpan::Text("Long text 3".to_string())],
        ]];
        let ctx = egui::Context::default();
        let input = screen_input(120.0, 120.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            let table_id = renderer.compute_table_id(&headers, &rows, &[], 15);
            renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
        });
    }

    #[test]
    fn test_render_table_estimates_wrapped_row_height() {
        let renderer = MarkdownRenderer::new();
        let _forced = ForcedTablePolicies::new(vec![ColumnPolicy::Fixed {
            width: 80.0,
            clip: false,
        }]);
        let headers = vec![vec![InlineSpan::Text("Params".to_string())]];
        let rows = vec![vec![vec![InlineSpan::Text(
            "word word word word word word word word".to_string(),
        )]]];
        let table_id = renderer.compute_table_id(&headers, &rows, &[], 18);

        with_test_ui(|_, ui| {
            let layout = *ui.layout();
            ui.allocate_ui_with_layout(Vec2::new(120.0, 0.0), layout, |ui| {
                ui.spacing_mut().item_spacing.x = 6.0;
                ui.set_width(120.0);
                renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
            });
        });

        // Check stored row height in metrics
        let height = renderer
            .table_metrics
            .borrow()
            .entry(table_id)
            .and_then(|e| e.row(0))
            .map(|r| r.max_height)
            .unwrap_or(0.0);
        assert!(height > renderer.row_height_fallback() + 0.5);
    }

    #[test]
    fn test_render_list_variants() {
        let renderer = MarkdownRenderer::new();
        let items = vec![
            ListItem {
                blocks: vec![
                    MarkdownElement::Paragraph(vec![InlineSpan::Text("Paragraph".to_string())]),
                    MarkdownElement::Header {
                        level: 2,
                        spans: vec![InlineSpan::Text("Header".to_string())],
                        id: "header".to_string(),
                    },
                ],
            },
            ListItem {
                blocks: vec![
                    MarkdownElement::CodeBlock {
                        language: None,
                        text: "code".to_string(),
                        highlighted: None,
                    },
                    MarkdownElement::Paragraph(vec![InlineSpan::Text("After".to_string())]),
                ],
            },
            ListItem { blocks: vec![] },
        ];
        with_test_ui(|_, ui| {
            renderer.render_list(ui, false, &items);
            renderer.render_list(ui, true, &items);
            renderer.render_list(ui, false, &[]);
        });
    }

    #[test]
    fn test_render_element_body_header_levels() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            for level in 3..=7 {
                let element = MarkdownElement::Header {
                    level,
                    spans: vec![InlineSpan::Text(format!("H{}", level))],
                    id: format!("h{}", level),
                };
                renderer.render_element_body(ui, &element);
            }
        });
    }

    #[test]
    fn test_render_table_tablebuilder_hscroll() {
        let renderer = MarkdownRenderer::new();
        let headers: Vec<Vec<InlineSpan>> = (0..8)
            .map(|i| vec![InlineSpan::Text(format!("H{}", i + 1))])
            .collect();
        let row: Vec<Vec<InlineSpan>> = (0..8)
            .map(|i| vec![InlineSpan::Text(format!("col{}", i + 1))])
            .collect();
        let rows = vec![row];
        let ctx = egui::Context::default();
        let input = screen_input(200.0, 120.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            let table_id = renderer.compute_table_id(&headers, &rows, &[], 10);
            renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
        });
    }

    #[test]
    fn test_render_table_tablebuilder_tiny_width_hscroll() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![
            vec![InlineSpan::Text("A".to_string())],
            vec![InlineSpan::Text("B".to_string())],
        ];
        let rows = vec![vec![
            vec![InlineSpan::Text("1".to_string())],
            vec![InlineSpan::Text("2".to_string())],
        ]];
        with_test_ui(|_, ui| {
            let layout = *ui.layout();
            ui.allocate_ui_with_layout(Vec2::new(5.0, 0.0), layout, |ui| {
                ui.spacing_mut().item_spacing.x = 6.0;
                ui.set_width(5.0);
                let table_id = renderer.compute_table_id(&headers, &rows, &[], 18);
                renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
            });
        });
    }

    #[test]
    fn test_render_table_tablebuilder_scaled_down() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![
            vec![InlineSpan::Text("Wide Header One".to_string())],
            vec![InlineSpan::Text("Wide Header Two".to_string())],
        ];
        let long_text = "W".repeat(80);
        let rows = vec![vec![
            vec![InlineSpan::Text(long_text.clone())],
            vec![InlineSpan::Text(long_text)],
        ]];
        let ctx = egui::Context::default();
        let input = screen_input(180.0, 120.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            let table_id = renderer.compute_table_id(&headers, &rows, &[], 11);
            renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
        });
    }

    #[test]
    fn test_list_marker_info_any_indent_variants() {
        assert!(MarkdownRenderer::list_marker_info_any_indent("").is_none());
        assert!(MarkdownRenderer::list_marker_info_any_indent("   ").is_none());

        let (rest, indent, content_indent, leading_cols) =
            MarkdownRenderer::list_marker_info_any_indent(" 1. item").expect("marker");
        assert_eq!(rest, "item");
        assert_eq!(indent, 1);
        assert_eq!(content_indent, 4);
        assert_eq!(leading_cols, 1);

        let (rest, indent, content_indent, leading_cols) =
            MarkdownRenderer::list_marker_info_any_indent("2)\titem").expect("marker");
        assert_eq!(rest, "item");
        assert_eq!(indent, 2);
        assert_eq!(content_indent, 4);
        assert_eq!(leading_cols, 0);
    }

    #[test]
    fn test_list_marker_info_any_indent_requires_trailing_space() {
        assert!(MarkdownRenderer::list_marker_info_any_indent("-").is_none());
        assert!(MarkdownRenderer::list_marker_info_any_indent("   -").is_none());
    }

    #[test]
    fn test_list_marker_info_any_indent_rejects_missing_whitespace() {
        assert!(MarkdownRenderer::list_marker_info_any_indent("1.item").is_none());
        assert!(MarkdownRenderer::list_marker_info_any_indent("1)item").is_none());
        assert!(MarkdownRenderer::list_marker_info_any_indent("1item").is_none());
    }

    #[test]
    fn test_collect_until_tag_end_breaks_and_code() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Emphasis),
            Event::Text("alpha".into()),
            Event::Code("beta".into()),
            Event::SoftBreak,
            Event::Html("<br>".into()),
            Event::HardBreak,
            Event::Html("<span>".into()),
            Event::End(Tag::Emphasis),
        ];
        let (text, next) = renderer.collect_until_tag_end(&events, 1, Tag::Emphasis, true);
        assert_eq!(next, events.len());
        assert_eq!(text, "alphabeta\n\n\n");

        let (text, next) = renderer.collect_until_tag_end(&events, 1, Tag::Emphasis, false);
        assert_eq!(next, events.len());
        assert_eq!(text, "alphabeta   ");

        let unclosed = vec![Event::Text("tail".into()), Event::SoftBreak];
        let (text, next) = renderer.collect_until_tag_end(&unclosed, 0, Tag::Emphasis, true);
        assert_eq!(next, unclosed.len());
        assert_eq!(text, "tail\n");
    }

    #[test]
    fn test_parse_inline_spans_with_breaks_link_image_and_unclosed() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Paragraph),
            Event::Start(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Text("link".into()),
            Event::End(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Start(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::Text("alt".into()),
            Event::End(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::FootnoteReference("note".into()),
            Event::End(Tag::Paragraph),
        ];
        let (spans, next) =
            renderer.parse_inline_spans_with_breaks(&events, 1, Tag::Paragraph, true);
        assert_eq!(next, events.len());
        assert!(spans
            .iter()
            .any(|span| matches!(span, InlineSpan::Link { .. })));
        assert!(spans
            .iter()
            .any(|span| matches!(span, InlineSpan::Image { .. })));

        let unclosed = vec![Event::Text("tail".into())];
        let (spans, next) =
            renderer.parse_inline_spans_with_breaks(&unclosed, 0, Tag::Paragraph, true);
        assert_eq!(next, unclosed.len());
        assert_eq!(MarkdownRenderer::spans_plain_text(&spans), "tail");
    }

    #[test]
    fn test_parse_inline_spans_with_breaks_mismatched_end_tag() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Text("hello".into()),
            Event::End(Tag::Heading(pulldown_cmark::HeadingLevel::H1, None, vec![])),
        ];
        let (spans, next) =
            renderer.parse_inline_spans_with_breaks(&events, 0, Tag::Paragraph, true);
        assert_eq!(next, events.len());
        assert_eq!(MarkdownRenderer::spans_plain_text(&spans), "hello");
    }

    #[test]
    fn test_parse_inline_spans_unclosed_image_only() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
            Event::End(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
        ];
        let (spans, next) =
            renderer.parse_inline_spans_with_breaks(&events, 0, Tag::Paragraph, true);
        assert_eq!(next, events.len());
        assert!(spans
            .iter()
            .any(|span| matches!(span, InlineSpan::Image { .. })));
    }

    #[test]
    fn test_parse_inline_spans_image_only_skips_text_buffer() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Paragraph),
            Event::Start(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
            Event::End(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
            Event::End(Tag::Paragraph),
        ];
        let (spans, next) =
            renderer.parse_inline_spans_with_breaks(&events, 1, Tag::Paragraph, true);
        assert_eq!(next, events.len());
        assert_eq!(spans.len(), 1);
        assert!(matches!(
            spans[0],
            InlineSpan::Image { ref alt, .. } if alt.is_empty()
        ));
    }

    #[test]
    fn test_parse_list_inline_variants_unclosed() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::List(Some(1))),
            Event::Text("skip".into()),
            Event::Start(Tag::Item),
            Event::Start(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Text("Dup".into()),
            Event::End(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Start(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Text("Dup".into()),
            Event::End(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Start(Tag::Emphasis),
            Event::Text("em".into()),
            Event::End(Tag::Emphasis),
            Event::Start(Tag::Strong),
            Event::Text("strong".into()),
            Event::End(Tag::Strong),
            Event::Start(Tag::Strikethrough),
            Event::Text("strike".into()),
            Event::End(Tag::Strikethrough),
            Event::Start(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Text("link".into()),
            Event::End(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Start(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
            Event::Text("alt".into()),
            Event::End(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
            Event::Text("tail".into()),
            Event::SoftBreak,
            Event::FootnoteReference("note".into()),
        ];
        let mut slugs = HashMap::new();
        let (items, next) = renderer.parse_list(&events, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert_eq!(items.len(), 1);
        Ok(())
    }

    #[test]
    fn test_parse_list_empty_paragraph_skips_block() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::List(Some(1))),
            Event::Start(Tag::Item),
            Event::Start(Tag::Paragraph),
            Event::End(Tag::Paragraph),
            Event::End(Tag::Item),
            Event::End(Tag::List(Some(1))),
        ];
        let mut slugs = HashMap::new();
        let (items, next) = renderer.parse_list(&events, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert_eq!(items.len(), 1);
        assert!(items[0].blocks.is_empty());
        Ok(())
    }

    #[test]
    fn test_collect_blockquotes_unclosed_variants() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::BlockQuote),
            Event::Text("plain".into()),
            Event::Code("code".into()),
            Event::Start(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Text("Dup".into()),
            Event::End(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Start(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Text("Dup".into()),
            Event::End(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Start(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Text("link".into()),
            Event::End(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Start(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
            Event::Text("alt".into()),
            Event::End(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
            Event::FootnoteReference("note".into()),
        ];
        let mut slugs = HashMap::new();
        let (quotes, next) = renderer.collect_blockquotes(&events, 1, 1, &mut slugs);
        assert_eq!(next, events.len());
        let mut ids = Vec::new();
        for (_, blocks) in &quotes {
            for block in blocks {
                if let MarkdownElement::Header { id, .. } = block {
                    ids.push(id.clone());
                }
            }
        }
        assert_eq!(ids.len(), 2);
        assert_ne!(ids[0], ids[1]);
        Ok(())
    }

    #[test]
    fn test_render_inline_span_context_menus() {
        let renderer = MarkdownRenderer::new();
        let code_span = InlineSpan::Code("inline".to_string());
        let ctx = egui::Context::default();
        let click = input_with_click(egui::pos2(12.0, 12.0), egui::PointerButton::Secondary);
        run_frame_with_input(&ctx, click, |_, ui| {
            renderer.render_inline_span(ui, &code_span, None, None);
        });
        let input = screen_input(320.0, 240.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            renderer.render_inline_span(ui, &code_span, None, None);
        });

        let link_span = InlineSpan::Link {
            text: "Link".to_string(),
            url: "https://example.com".to_string(),
        };
        let ctx = egui::Context::default();
        let click = input_with_click(egui::pos2(12.0, 12.0), egui::PointerButton::Secondary);
        run_frame_with_input(&ctx, click, |_, ui| {
            renderer.render_inline_span(ui, &link_span, None, None);
        });
        let input = screen_input(320.0, 240.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            renderer.render_inline_span(ui, &link_span, None, None);
        });
    }

    #[test]
    fn test_render_inline_span_image_scale_and_title() {
        let renderer = MarkdownRenderer::new();
        let ctx = egui::Context::default();
        let input = screen_input(40.0, 240.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            let color_image = egui::ColorImage::new([200, 120], Color32::WHITE);
            let tex =
                ui.ctx()
                    .load_texture("img:test.png", color_image, egui::TextureOptions::LINEAR);
            renderer.image_textures.borrow_mut().insert(
                "test.png".to_string(),
                ImageCacheEntry {
                    texture: tex,
                    size: [200, 120],
                    modified: None,
                    byte_size: ImageCacheEntry::estimate_bytes([200, 120]),
                },
            );
            let span = InlineSpan::Image {
                src: "test.png".to_string(),
                alt: "Alt".to_string(),
                title: Some("Title".to_string()),
            };
            renderer.render_inline_span(ui, &span, None, None);
        });
    }

    #[test]
    fn test_paint_table_text_job_triggers_anchor_on_click() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Link {
            text: "Jump".to_string(),
            url: "#Section-One".to_string(),
        }];
        let ctx = egui::Context::default();
        let click_pos = std::cell::Cell::new(egui::pos2(1.0, 1.0));
        let layout_input = screen_input(220.0, 120.0);
        run_frame_with_input(&ctx, layout_input, |_, ui| {
            ui.allocate_ui_at_rect(
                egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 80.0)),
                |ui| {
                    let build =
                        renderer.build_layout_job(ui.style(), &spans, 60.0, false, Align::LEFT);
                    let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
                    let (rect, _response) = ui.allocate_exact_size(
                        egui::vec2(120.0, galley.size().y),
                        egui::Sense::click(),
                    );
                    let text_origin =
                        MarkdownRenderer::aligned_text_origin(rect, &galley, build.job.halign);
                    click_pos.set(text_origin + egui::vec2(2.0, 2.0));
                },
            );
        });

        let input = input_with_click(click_pos.get(), egui::PointerButton::Primary);
        run_frame_with_input(&ctx, input, |_, ui| {
            ui.allocate_ui_at_rect(
                egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 80.0)),
                |ui| {
                    let build =
                        renderer.build_layout_job(ui.style(), &spans, 60.0, false, Align::LEFT);
                    renderer.paint_table_text_job(ui, 120.0, build);
                },
            );
        });

        assert_eq!(
            renderer.take_pending_anchor(),
            Some("section-one".to_string())
        );
    }

    #[test]
    fn test_paint_table_text_job_hover_link_does_not_trigger() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Link {
            text: "Jump".to_string(),
            url: "#Section-One".to_string(),
        }];
        let ctx = egui::Context::default();
        let hover_pos = std::cell::Cell::new(egui::pos2(1.0, 1.0));
        let layout_input = screen_input(220.0, 120.0);
        run_frame_with_input(&ctx, layout_input, |_, ui| {
            ui.allocate_ui_at_rect(
                egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 80.0)),
                |ui| {
                    let build =
                        renderer.build_layout_job(ui.style(), &spans, 60.0, false, Align::LEFT);
                    let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
                    let (rect, _response) = ui.allocate_exact_size(
                        egui::vec2(120.0, galley.size().y),
                        egui::Sense::click(),
                    );
                    let text_origin =
                        MarkdownRenderer::aligned_text_origin(rect, &galley, build.job.halign);
                    hover_pos.set(text_origin + egui::vec2(2.0, 2.0));
                },
            );
        });

        let mut input = screen_input(220.0, 120.0);
        input
            .events
            .push(egui::Event::PointerMoved(hover_pos.get()));
        run_frame_with_input(&ctx, input, |_, ui| {
            ui.allocate_ui_at_rect(
                egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 80.0)),
                |ui| {
                    let build =
                        renderer.build_layout_job(ui.style(), &spans, 60.0, false, Align::LEFT);
                    renderer.paint_table_text_job(ui, 120.0, build);
                },
            );
        });

        assert!(renderer.take_pending_anchor().is_none());
    }

    #[test]
    fn test_render_inline_span_image_empty_title() {
        let renderer = MarkdownRenderer::new();
        let ctx = egui::Context::default();
        let input = screen_input(120.0, 120.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            let color_image = egui::ColorImage::new([20, 12], Color32::WHITE);
            let tex = ui.ctx().load_texture(
                "img:empty-title.png",
                color_image,
                egui::TextureOptions::LINEAR,
            );
            renderer.image_textures.borrow_mut().insert(
                "empty-title.png".to_string(),
                ImageCacheEntry {
                    texture: tex,
                    size: [20, 12],
                    modified: None,
                    byte_size: ImageCacheEntry::estimate_bytes([20, 12]),
                },
            );
            let span = InlineSpan::Image {
                src: "empty-title.png".to_string(),
                alt: "Alt".to_string(),
                title: Some(String::new()),
            };
            renderer.render_inline_span(ui, &span, None, None);
        });
    }

    #[test]
    fn test_measure_inline_spans_empty_lines_and_styles() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            let spans = vec![
                InlineSpan::Strong("Bold\n\nLine".to_string()),
                InlineSpan::Emphasis("Em\n".to_string()),
                InlineSpan::Strikethrough("Strike".to_string()),
                InlineSpan::Link {
                    text: "Link\n".to_string(),
                    url: "https://example.com".to_string(),
                },
                InlineSpan::Code("code\n\nmore".to_string()),
            ];
            let width = renderer.measure_inline_spans(ui, &spans);
            assert!(width > 0.0);
        });
    }

    #[test]
    fn test_estimate_table_cell_height_variants() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            let style = ui.style().clone();
            let empty =
                renderer.estimate_table_cell_height(&style, ui, &[], 100.0, Align::LEFT, 18.0);
            assert_eq!(empty, 18.0);

            let emoji_spans = vec![InlineSpan::Text("😀".to_string())];
            let emoji_height = renderer.estimate_table_cell_height(
                &style,
                ui,
                &emoji_spans,
                100.0,
                Align::LEFT,
                18.0,
            );
            assert!(emoji_height >= 18.0);

            let spans = vec![
                InlineSpan::Text("alpha".to_string()),
                InlineSpan::Image {
                    src: "missing.png".to_string(),
                    alt: "Alt".to_string(),
                    title: None,
                },
            ];
            let height =
                renderer.estimate_table_cell_height(&style, ui, &spans, 40.0, Align::LEFT, 18.0);
            assert!(height >= 18.0);
        });
    }

    #[test]
    fn test_estimate_table_column_widths_scaling() {
        let renderer = MarkdownRenderer::new();
        let empty = renderer.estimate_table_column_widths(&[], 120.0, 6.0);
        assert_eq!(empty, vec![120.0]);

        let specs = vec![fixed_spec(0, "A", 100.0), fixed_spec(1, "B", 100.0)];
        let widths = renderer.estimate_table_column_widths(&specs, 100.0, 10.0);
        assert_eq!(widths.len(), 2);
        let sum: f32 = widths.iter().sum();
        assert!((sum - 90.0).abs() < 0.5);
    }

    #[test]
    fn test_estimate_table_column_widths_no_scale_when_available() {
        let renderer = MarkdownRenderer::new();
        let specs = vec![fixed_spec(0, "A", 80.0), fixed_spec(1, "B", 60.0)];
        let widths = renderer.estimate_table_column_widths(&specs, 300.0, 10.0);
        assert_eq!(widths, vec![80.0, 60.0]);
    }

    // ── Stage 1: content-driven natural width tests ──────────────────────

    #[test]
    fn test_natural_widths_two_short_columns() {
        // Two columns with content wider than policy → content drives widths
        let renderer = MarkdownRenderer::new();
        let headers = vec![
            vec![InlineSpan::Text("Name".to_string())],
            vec![InlineSpan::Text("Value".to_string())],
        ];
        let stats = vec![
            ColumnStat {
                max_graphemes: 20,
                ..Default::default()
            },
            ColumnStat {
                max_graphemes: 18,
                ..Default::default()
            },
        ];
        // Small resolved widths so content should dominate
        let resolved = vec![60.0, 60.0];
        let widths = renderer.estimate_natural_column_widths(&stats, &headers, &resolved, 2);
        assert_eq!(widths.len(), 2);
        // body=14.0, char_width_factor=14*0.62=8.68, cell_padding=14*1.8=25.2
        // col0: max(4,20)=20 → 20*8.68+25.2 = 198.8 → clamp(198.8, 56, 490) = 198.8
        // col1: max(5,18)=18 → 18*8.68+25.2 = 181.44 → clamp(181.44, 56, 490) = 181.44
        assert!(widths[0] > 150.0, "col 0 = {} should be > 150", widths[0]);
        assert!(widths[1] > 150.0, "col 1 = {} should be > 150", widths[1]);
    }

    #[test]
    fn test_natural_widths_uses_max_of_content_and_policy() {
        // Short content (3 graphemes), policy width 98 should win
        let renderer = MarkdownRenderer::new();
        let headers = vec![vec![InlineSpan::Text("ID".to_string())]];
        let stats = vec![ColumnStat {
            max_graphemes: 3,
            ..Default::default()
        }];
        // body=14.0, char_width_factor=8.68, cell_padding=25.2
        // header "ID" → width 2; max(2,3)=3 → 3*8.68+25.2 = 51.24
        // policy = 98.0 → max(51.24, 98.0) = 98.0
        let resolved = vec![98.0];
        let widths = renderer.estimate_natural_column_widths(&stats, &headers, &resolved, 1);
        assert_eq!(widths.len(), 1);
        assert!(
            (widths[0] - 98.0).abs() < 0.1,
            "policy should win: got {}",
            widths[0]
        );
    }

    #[test]
    fn test_natural_widths_clamped_to_bounds() {
        let renderer = MarkdownRenderer::new();
        // body=14.0 → min_col_width=56.0, max_col_width=490.0
        let min_col = 14.0 * 4.0; // 56
        let max_col = 14.0 * 35.0; // 490

        // Tiny content: 1 grapheme → 1*8.68+25.2 = 33.88, policy=10 → clamp to min
        let headers_tiny = vec![vec![InlineSpan::Text("X".to_string())]];
        let stats_tiny = vec![ColumnStat {
            max_graphemes: 1,
            ..Default::default()
        }];
        let resolved_tiny = vec![10.0];
        let widths_tiny =
            renderer.estimate_natural_column_widths(&stats_tiny, &headers_tiny, &resolved_tiny, 1);
        assert!(
            (widths_tiny[0] - min_col).abs() < 0.1,
            "tiny should clamp to min {}: got {}",
            min_col,
            widths_tiny[0]
        );

        // Huge content: 200 graphemes → 200*8.68+25.2 = 1761.2 → clamp to max
        let headers_huge = vec![vec![InlineSpan::Text("Description".to_string())]];
        let stats_huge = vec![ColumnStat {
            max_graphemes: 200,
            ..Default::default()
        }];
        let resolved_huge = vec![10.0];
        let widths_huge =
            renderer.estimate_natural_column_widths(&stats_huge, &headers_huge, &resolved_huge, 1);
        assert!(
            (widths_huge[0] - max_col).abs() < 0.1,
            "huge should clamp to max {}: got {}",
            max_col,
            widths_huge[0]
        );
    }

    #[test]
    fn test_natural_table_width_sums_correctly() {
        // Empty input
        let empty = MarkdownRenderer::natural_table_width(&[], 10.0);
        assert!((empty - 0.0).abs() < f32::EPSILON);

        // Single column → no spacing
        let single = MarkdownRenderer::natural_table_width(&[120.0], 10.0);
        assert!((single - 120.0).abs() < f32::EPSILON);

        // Three columns: 100+200+150 = 450, spacing = 10*2 = 20, total = 470
        let three = MarkdownRenderer::natural_table_width(&[100.0, 200.0, 150.0], 10.0);
        assert!((three - 470.0).abs() < 0.1, "expected 470, got {}", three);
    }

    #[test]
    fn test_content_fit_mode_activated_for_small_table() {
        // 2-column table with short content → natural_total should be < 1100
        let renderer = MarkdownRenderer::new();
        let headers = vec![
            vec![InlineSpan::Text("Key".to_string())],
            vec![InlineSpan::Text("Value".to_string())],
        ];
        let stats = vec![
            ColumnStat {
                max_graphemes: 10,
                ..Default::default()
            },
            ColumnStat {
                max_graphemes: 12,
                ..Default::default()
            },
        ];
        let resolved = vec![60.0, 60.0];
        let widths = renderer.estimate_natural_column_widths(&stats, &headers, &resolved, 2);
        let total = MarkdownRenderer::natural_table_width(&widths, 6.0);
        assert!(
            total < 1100.0,
            "small 2-col table total {} should be < 1100",
            total
        );
    }

    #[test]
    fn test_viewport_fill_mode_for_wide_table() {
        // 9-column table with moderate content → natural_total should be > 1100
        let renderer = MarkdownRenderer::new();
        let headers: Vec<Vec<InlineSpan>> = (0..9)
            .map(|i| vec![InlineSpan::Text(format!("Column{i}"))])
            .collect();
        let stats: Vec<ColumnStat> = (0..9)
            .map(|_| ColumnStat {
                max_graphemes: 15,
                ..Default::default()
            })
            .collect();
        let resolved: Vec<f32> = vec![80.0; 9];
        let widths = renderer.estimate_natural_column_widths(&stats, &headers, &resolved, 9);
        let total = MarkdownRenderer::natural_table_width(&widths, 6.0);
        assert!(
            total > 1100.0,
            "9-col table total {} should be > 1100",
            total
        );
    }

    // Test: column_count exceeds headers/stats/resolved_widths length
    #[test]
    fn test_natural_widths_handles_mismatched_lengths() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![vec![InlineSpan::Text("Name".into())]]; // only 1 header
        let stats = vec![ColumnStat {
            max_graphemes: 10,
            longest_word: 5,
            ..Default::default()
        }]; // only 1 stat
        let resolved = vec![98.0]; // only 1 width
                                   // column_count = 3, exceeds all inputs
        let widths = renderer.estimate_natural_column_widths(&stats, &headers, &resolved, 3);
        assert_eq!(widths.len(), 3);
        // First column should be content/policy-driven
        assert!(widths[0] > 50.0);
        // Remaining columns should be clamped to MIN (body * 4.0 = 56.0)
        let min_col = renderer.font_sizes.body * 4.0;
        assert!(
            (widths[1] - min_col).abs() < 1.0,
            "extra col should be MIN, got {}",
            widths[1]
        );
        assert!(
            (widths[2] - min_col).abs() < 1.0,
            "extra col should be MIN, got {}",
            widths[2]
        );
    }

    // Test: CJK characters in header get double-width estimate
    #[test]
    fn test_natural_widths_cjk_header_gets_double_width() {
        let renderer = MarkdownRenderer::new();
        let headers_ascii = vec![vec![InlineSpan::Text("Name".into())]]; // 4 display width
        let headers_cjk = vec![vec![InlineSpan::Text(
            "\u{540D}\u{524D}\u{5217}\u{5E45}".into(),
        )]]; // 4 CJK chars = 8 display width
        let stats = vec![ColumnStat::default()]; // no content
        let resolved = vec![0.0]; // no policy
        let widths_ascii =
            renderer.estimate_natural_column_widths(&stats, &headers_ascii, &resolved, 1);
        let widths_cjk =
            renderer.estimate_natural_column_widths(&stats, &headers_cjk, &resolved, 1);
        // CJK header should produce a wider natural width than ASCII header
        assert!(
            widths_cjk[0] > widths_ascii[0],
            "CJK width {} should exceed ASCII width {}",
            widths_cjk[0],
            widths_ascii[0]
        );
    }

    #[test]
    fn test_escape_table_pipes_in_inline_code_nested_lists() {
        let md_parent = "\
 - Parent
   | Col | Notes |
  | --- | --- |
  | code | `a|b` |
";
        let escaped_parent = MarkdownRenderer::escape_table_pipes_in_inline_code(md_parent);
        assert!(escaped_parent.contains(PIPE_SENTINEL));

        let md_nested = "\
- Parent
    - | Col | Notes |
      | --- | --- |
      | code | `a|b` |
";
        let escaped_nested = MarkdownRenderer::escape_table_pipes_in_inline_code(md_nested);
        assert!(escaped_nested.contains(PIPE_SENTINEL));
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_list_marker_header() {
        let md = "\
- Parent
    - | H | I |
      | --- | --- |
      | 1 | 2 |";
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_eq!(escaped, md);
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_list_marker_any_indent() {
        let md = "\
- Parent
    - | H | I |
      | --- | --- |
      | `a|b` | c |
";
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(escaped.contains(PIPE_SENTINEL));
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_marker_within_range() {
        let md = "\
- Parent
    - | H | I |
      | --- | --- |
      | `a|b` | c |
";
        let lines: Vec<&str> = md.split_inclusive('\n').collect();
        let (level, rest) = MarkdownRenderer::table_line_info(lines[1]);
        let _parent_indent =
            MarkdownRenderer::parent_list_indent_for_line(&lines, 1, level, rest).unwrap();
        let _leading_spaces =
            MarkdownRenderer::list_marker_info_any_indent(rest).map(|(_, _, _, spaces)| spaces);
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(escaped.contains(PIPE_SENTINEL));
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_strip_without_code_indent() {
        let md = "\
- Parent
  continuation
  | H | I |
  | --- | --- |
  | `a|b` | c |
";
        let lines: Vec<&str> = md.split_inclusive('\n').collect();
        let (level, rest) = MarkdownRenderer::table_line_info(lines[1]);
        let parent_indent =
            MarkdownRenderer::parent_list_indent_for_line(&lines, 1, level, rest).unwrap();
        let strip = MarkdownRenderer::strip_indent_columns(rest, parent_indent);
        let code_strip = MarkdownRenderer::strip_indent_columns(rest, parent_indent + 4);
        assert!(strip.is_some());
        assert!(code_strip.is_none());
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(escaped.contains(PIPE_SENTINEL));
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_strips_non_code_row() {
        let md = "\
- Parent
  | H | I |
  | --- | --- |
  | 1 | 2 |";
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(escaped.contains("| H | I |"));
        assert!(escaped.contains("| --- | --- |"));
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_out_of_range() {
        let md = "\
- Parent
        - Child
        continuation
";
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_eq!(escaped, md);
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_marker_outside_range() {
        let md = "\
- Parent
      - Child
      continuation
";
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_eq!(escaped, md);
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_marker_too_deep() {
        let md = "\
- Parent
      - Child
      | H | I |
      | --- | --- |
      | `a|b` | c |
";
        let lines: Vec<&str> = md.split_inclusive('\n').collect();
        let (level, rest) = MarkdownRenderer::table_line_info(lines[1]);
        let _parent_indent =
            MarkdownRenderer::parent_list_indent_for_line(&lines, 1, level, rest).unwrap();
        let _leading_spaces =
            MarkdownRenderer::list_marker_info_any_indent(rest).map(|(_, _, _, spaces)| spaces);
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_eq!(escaped, md);
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_code_strip_present() {
        let md = "\
 - Parent
      continuation
      | H | I |
      | --- | --- |
      | `a|b` | c |
";
        let lines: Vec<&str> = md.split_inclusive('\n').collect();
        let (level, rest) = MarkdownRenderer::table_line_info(lines[1]);
        let parent_indent =
            MarkdownRenderer::parent_list_indent_for_line(&lines, 1, level, rest).unwrap();
        let strip = MarkdownRenderer::strip_indent_columns(rest, parent_indent);
        let code_strip = MarkdownRenderer::strip_indent_columns(rest, parent_indent + 4);
        assert!(strip.is_some());
        assert!(code_strip.is_some());
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_eq!(escaped, md);
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_strip_only() {
        let md = "\
- Parent
  | H | I |
  | --- | --- |
  | `a|b` | c |
";
        let lines: Vec<&str> = md.split_inclusive('\n').collect();
        let (level, rest) = MarkdownRenderer::table_line_info(lines[1]);
        let parent_indent =
            MarkdownRenderer::parent_list_indent_for_line(&lines, 1, level, rest).unwrap();
        let strip = MarkdownRenderer::strip_indent_columns(rest, parent_indent);
        let code_strip = MarkdownRenderer::strip_indent_columns(rest, parent_indent + 4);
        assert!(strip.is_some());
        assert!(code_strip.is_none());
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(escaped.contains(PIPE_SENTINEL));
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_marker_in_range_any_indent() {
        let md = "\
  - Parent
     - | H | I |
        | --- | --- |
       | `a|b` | c |
";
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert!(escaped.contains(PIPE_SENTINEL));
    }

    #[test]
    fn test_escape_table_pipes_parent_indent_deep_list_marker_table_like_lines() {
        let md = "\
- Parent
        - | H | I |
        | --- | --- |
        | `a|b` | c |
";
        let escaped = MarkdownRenderer::escape_table_pipes_in_inline_code(md);
        assert_eq!(escaped, md);
    }

    #[test]
    fn test_parse_table_unclosed_and_noise() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Table(vec![Alignment::Left])),
            Event::Start(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("H".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::End(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Text("noise".into()),
            Event::Start(Tag::TableCell),
            Event::Text("R".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::Text("after".into()),
        ];
        let (headers, rows, next) = renderer.parse_table(&events, 1);
        assert_eq!(headers.len(), 1);
        assert_eq!(rows.len(), 1);
        assert_eq!(next, events.len());
        Ok(())
    }

    #[test]
    fn test_parse_table_unclosed_head_exits_on_eof() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Table(vec![Alignment::Left])),
            Event::Start(Tag::TableHead),
            Event::Start(Tag::TableCell),
            Event::Text("H".into()),
            Event::End(Tag::TableCell),
        ];
        let (headers, rows, next) = renderer.parse_table(&events, 1);
        assert_eq!(headers.len(), 1);
        assert!(rows.is_empty());
        assert_eq!(next, events.len());
        Ok(())
    }

    #[test]
    fn test_parse_table_unclosed_row_exits_on_eof() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Table(vec![Alignment::Left])),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("R".into()),
            Event::End(Tag::TableCell),
        ];
        let (headers, rows, next) = renderer.parse_table(&events, 1);
        assert!(headers.is_empty());
        assert!(rows.is_empty());
        assert_eq!(next, events.len());
        Ok(())
    }

    #[test]
    fn test_parse_table_head_skips_noise() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let alignments = vec![Alignment::Left];
        let events = vec![
            Event::Start(Tag::Table(alignments.clone())),
            Event::Start(Tag::TableHead),
            Event::Text("noise".into()),
            Event::Start(Tag::TableCell),
            Event::Text("H".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableHead),
            Event::End(Tag::Table(alignments)),
        ];
        let (headers, rows, next) = renderer.parse_table(&events, 1);
        assert_eq!(headers.len(), 1);
        assert!(rows.is_empty());
        assert_eq!(next, events.len());
        Ok(())
    }

    #[test]
    fn test_parse_table_empty_row_skips_row() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let alignments = vec![Alignment::Left];
        let events = vec![
            Event::Start(Tag::Table(alignments.clone())),
            Event::Start(Tag::TableRow),
            Event::End(Tag::TableRow),
            Event::End(Tag::Table(alignments)),
        ];
        let (headers, rows, next) = renderer.parse_table(&events, 1);
        assert!(headers.is_empty());
        assert!(rows.is_empty());
        assert_eq!(next, events.len());
        Ok(())
    }

    #[test]
    fn test_parse_code_block_unclosed() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Fenced(
                "rust".into(),
            ))),
            Event::Text("fn main() {}".into()),
            Event::Html("<span>".into()),
        ];
        let (code, language, next) = renderer.parse_code_block(&events, 0);
        assert_eq!(language.as_deref(), Some("rust"));
        assert_eq!(code, "fn main() {}");
        assert_eq!(next, events.len());
        Ok(())
    }

    #[test]
    fn test_parse_code_block_without_start_tag() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Text("fn main() {}".into()),
            Event::End(Tag::CodeBlock(pulldown_cmark::CodeBlockKind::Indented)),
        ];
        let (code, language, next) = renderer.parse_code_block(&events, 0);
        assert_eq!(language, None);
        assert_eq!(code, "fn main() {}");
        assert_eq!(next, events.len());
        Ok(())
    }

    #[test]
    fn test_has_pipe_outside_inline_code_backticks() {
        assert!(MarkdownRenderer::has_pipe_outside_inline_code(r"\`|`"));
        assert!(!MarkdownRenderer::has_pipe_outside_inline_code("``a|b``"));
    }

    #[test]
    fn test_is_allowed_scheme_variants() {
        assert!(MarkdownRenderer::is_allowed_scheme("http://example.com"));
        assert!(MarkdownRenderer::is_allowed_scheme("https://example.com"));
        assert!(MarkdownRenderer::is_allowed_scheme(
            "mailto:test@example.com"
        ));
        assert!(!MarkdownRenderer::is_allowed_scheme("ftp://example.com"));
    }

    #[test]
    fn test_render_table_tablebuilder_scaled_down_adjustments() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![
            vec![InlineSpan::Text("A".to_string())],
            vec![InlineSpan::Text("B".to_string())],
            vec![InlineSpan::Text("C".to_string())],
        ];
        let rows = vec![vec![
            vec![InlineSpan::Text("row1".to_string())],
            vec![InlineSpan::Text("row2".to_string())],
            vec![InlineSpan::Text("row3".to_string())],
        ]];
        let ctx = egui::Context::default();
        let widths = [220.0, 240.0];
        for (idx, width) in widths.iter().enumerate() {
            let _forced = ForcedTablePolicies::new(vec![
                ColumnPolicy::Resizable {
                    min: 40.0,
                    preferred: 200.0,
                    clip: false,
                },
                ColumnPolicy::Auto,
                ColumnPolicy::Remainder { clip: false },
            ]);
            let input = screen_input(*width, 120.0);
            run_frame_with_input(&ctx, input, |_, ui| {
                ui.spacing_mut().item_spacing.x = 2.0;
                let table_id = renderer.compute_table_id(&headers, &rows, &[], 90 + idx as u64);
                renderer.render_table_tablebuilder(ui, &headers, &rows, &[], table_id);
            });
        }
    }

    #[test]
    fn test_render_code_block_mermaid_and_context_menu() {
        let renderer = MarkdownRenderer::new();
        let _guard = EnvVarGuard::set("MDMDVIEW_MERMAID_RENDERER", "off");
        with_test_ui(|_, ui| {
            renderer.render_code_block(ui, Some("mermaid"), "graph TD; A-->B;", None);
        });

        let ctx = egui::Context::default();
        let click = input_with_click(egui::pos2(12.0, 12.0), egui::PointerButton::Secondary);
        run_frame_with_input(&ctx, click, |_, ui| {
            renderer.render_code_block(ui, Some("rust"), "fn main() {}", None);
        });
        let input = screen_input(320.0, 240.0);
        run_frame_with_input(&ctx, input, |_, ui| {
            renderer.render_code_block(ui, Some("rust"), "fn main() {}", None);
        });
    }

    #[test]
    fn test_render_code_block_highlight_styles() {
        let mut renderer = MarkdownRenderer::new();
        let theme = renderer
            .theme_set
            .themes
            .get_mut("base16-ocean.dark")
            .expect("theme");
        theme.scopes.clear();
        let selector = ScopeSelectors::from_str("source.rust").expect("selector");
        theme.scopes.push(ThemeItem {
            scope: selector,
            style: StyleModifier {
                foreground: Some(Color::WHITE),
                background: Some(Color::BLACK),
                font_style: Some(FontStyle::BOLD | FontStyle::ITALIC),
            },
        });
        with_test_ui(|_, ui| {
            renderer.render_code_block(ui, Some("rust"), "fn main() {}", None);
        });
    }

    #[test]
    fn test_render_code_block_highlight_and_fallback() {
        let renderer = MarkdownRenderer::new();
        let code = "fn main() {\n    let value = 1;\n    // comment with  spaces\n    \n}\n";
        with_test_ui(|_, ui| {
            renderer.render_code_block(ui, Some("rust"), code, None);
        });

        let mut fallback_renderer = MarkdownRenderer::new();
        fallback_renderer.syntax_set = SyntaxSet::new();
        with_test_ui(|_, ui| {
            fallback_renderer.render_code_block(ui, Some("notalanguage"), "code", None);
        });
    }

    #[test]
    fn test_to_superscript_maps_values() {
        let mapped = MarkdownRenderer::to_superscript("Abc123!");
        assert_ne!(mapped, "Abc123!");
    }

    #[test]
    fn test_cell_single_emoji_whitespace_only() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("   ".to_string())];
        assert!(renderer.cell_single_emoji(&spans).is_none());
    }

    #[test]
    fn test_table_line_info_with_list_fallback_indent() {
        let line = "  >| A |";
        let (level, rest) = MarkdownRenderer::table_line_info_with_list(line, Some(2));
        assert_eq!(level, 1);
        assert!(rest.starts_with('|'));
    }

    #[test]
    fn test_list_marker_info_any_indent_missing_punctuation() {
        assert!(MarkdownRenderer::list_marker_info_any_indent("1 item").is_none());
    }

    #[test]
    fn test_strip_indent_columns_rejects_non_whitespace() {
        assert!(MarkdownRenderer::strip_indent_columns("x", 1).is_none());
    }

    #[test]
    fn test_parse_element_image_empty_title_outside_paragraph() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
            Event::Text("alt".into()),
            Event::End(Tag::Image(LinkType::Inline, "img.png".into(), "".into())),
        ];
        let mut elements = Vec::new();
        let mut slugs = HashMap::new();
        let next = renderer.parse_element(&events, 0, &mut elements, &mut slugs);
        assert_eq!(next, events.len());
        let spans = paragraph_spans(&elements[0]).expect("paragraph spans");
        let (_src, _alt, title) = image_span_fields(&spans[0]).expect("image span");
        assert!(title.is_none());
    }

    #[test]
    fn test_slugify_trailing_dash_trimmed() {
        let slug = MarkdownRenderer::slugify("Hello - ");
        assert_eq!(slug, "hello");
    }

    #[test]
    fn test_slugify_leading_whitespace_and_internal_spaces() {
        let slug = MarkdownRenderer::slugify("  Hello   World ");
        assert_eq!(slug, "hello-world");
    }

    #[test]
    fn test_render_plain_and_highlight_empty_text() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            ui.visuals_mut().override_text_color = Some(Color32::from_rgb(10, 20, 30));
            let style = InlineStyle {
                strong: true,
                ..Default::default()
            };
            renderer.render_plain_segment(ui, "", 12.0, style);
            renderer.render_plain_segment(ui, "Header", 12.0, style);
            renderer.render_highlighted_segment(ui, "", 12.0, style);
        });
    }

    #[test]
    fn test_render_plain_segment_without_color_override() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            ui.visuals_mut().override_text_color = None;
            renderer.render_plain_segment(ui, "Plain", 12.0, InlineStyle::default());
        });
    }

    #[test]
    fn test_emoji_key_for_grapheme_strips_vs16() {
        let renderer = MarkdownRenderer::new();
        let key = renderer.emoji_key_for_grapheme("\u{2705}\u{fe0f}");
        assert_eq!(key, Some("\u{2705}".to_string()));
    }

    #[test]
    fn test_expand_superscripts_long_sequence_no_expand() {
        let input = "value^abcdefghijk^";
        let out = MarkdownRenderer::expand_superscripts(input);
        assert_eq!(out, input);
    }

    #[test]
    fn test_compute_table_content_hash_stops_after_sample_rows() {
        let renderer = MarkdownRenderer::new();
        let headers = vec![vec![InlineSpan::Text("H".to_string())]];
        let rows: Vec<Vec<Vec<InlineSpan>>> = (0..(COLUMN_STATS_SAMPLE_ROWS + 1))
            .map(|idx| vec![vec![InlineSpan::Text(format!("R{idx}"))]])
            .collect();
        let hash = renderer.compute_table_content_hash(&headers, &rows, &[]);
        assert_ne!(hash, 0);
    }

    #[test]
    fn test_table_width_helpers_empty_inputs() {
        let renderer = MarkdownRenderer::new();
        let table_id = 100u64;
        let mut specs: Vec<ColumnSpec> = Vec::new();
        renderer.apply_persisted_widths(table_id, &mut specs);
        renderer.persist_resizable_widths(table_id, &[], &[]);
        let change = renderer.record_resolved_widths(table_id, 0, &[]);
        assert!(matches!(change, WidthChange::None));
        assert!(renderer
            .resolve_table_column_widths(table_id, &[], 10.0)
            .is_empty());
        assert_eq!(renderer.estimate_table_total_width(table_id, &[], 6.0), 0.0);
        with_test_ui(|_, ui| {
            let style = ui.style().clone();
            let row = vec![vec![]];
            let height =
                renderer.estimate_table_row_height(ui, &style, &row, &[Align::LEFT], &[40.0], 12.0);
            assert_eq!(height, 12.0);
            let row = vec![
                vec![InlineSpan::Text("a".to_string())],
                vec![InlineSpan::Text("b".to_string())],
            ];
            let height =
                renderer.estimate_table_row_height(ui, &style, &row, &[Align::LEFT], &[40.0], 12.0);
            assert!(height >= 12.0);
            let rect = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(10.0, 10.0));
            renderer.paint_table_dividers(
                ui.painter(),
                ui.visuals(),
                rect,
                rect,
                &[],
                0.0,
                0.0,
                0.0,
                true,
            );
        });
    }

    #[test]
    fn test_resolve_table_column_widths_ignores_invalid_stored_widths() {
        let renderer = MarkdownRenderer::new();
        let table_id = 101u64;
        {
            let mut metrics = renderer.table_metrics.borrow_mut();
            let entry = metrics.entry_mut(table_id);
            entry.resolved_widths = vec![f32::NAN, -5.0];
        }
        let specs = vec![
            resizable_spec(0, "A", 20.0, 40.0),
            ColumnSpec::new(1, "B", ColumnPolicy::Auto, None),
        ];
        let widths = renderer.resolve_table_column_widths(table_id, &specs, 30.0);
        assert_eq!(widths, vec![40.0, 30.0]);
    }

    #[test]
    fn test_persist_resizable_widths_stores_new_width() {
        let renderer = MarkdownRenderer::new();
        let table_id = 200u64;
        let specs = vec![resizable_spec(0, "A", 20.0, 100.0)];
        let widths = vec![140.0f32];
        renderer.persist_resizable_widths(table_id, &specs, &widths);
        let metrics = renderer.table_metrics.borrow();
        let entry = metrics.entry(table_id).expect("entry created");
        assert_eq!(entry.persisted_width(specs[0].policy_hash), Some(140.0));
    }

    #[test]
    fn test_persist_resizable_widths_skips_small_delta() {
        let renderer = MarkdownRenderer::new();
        let table_id = 201u64;
        let spec = resizable_spec(0, "A", 20.0, 100.0);
        let hash = spec.policy_hash;
        let specs = vec![spec];
        renderer.persist_resizable_widths(table_id, &specs, &[120.0]);
        renderer.persist_resizable_widths(table_id, &specs, &[120.2]);
        let metrics = renderer.table_metrics.borrow();
        let entry = metrics.entry(table_id).expect("entry created");
        let stored = entry.persisted_width(hash).unwrap_or_default();
        assert!((stored - 120.0).abs() < 0.01);
    }

    #[test]
    fn test_persist_resizable_widths_updates_on_large_delta() {
        let renderer = MarkdownRenderer::new();
        let table_id = 202u64;
        let spec = resizable_spec(0, "A", 20.0, 100.0);
        let hash = spec.policy_hash;
        let specs = vec![spec];
        renderer.persist_resizable_widths(table_id, &specs, &[120.0]);
        renderer.persist_resizable_widths(table_id, &specs, &[130.0]);
        let metrics = renderer.table_metrics.borrow();
        let entry = metrics.entry(table_id).expect("entry created");
        assert_eq!(entry.persisted_width(hash), Some(130.0));
    }

    #[test]
    fn test_paint_table_dividers_draws_header_separator() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            let rect = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(120.0, 40.0));
            renderer.paint_table_dividers(
                ui.painter(),
                ui.visuals(),
                rect,
                rect,
                &[40.0, 60.0],
                8.0,
                12.0,
                4.0,
                true,
            );
        });
    }

    #[test]
    fn test_paint_table_dividers_no_header_height() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            let rect = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(120.0, 20.0));
            renderer.paint_table_dividers(
                ui.painter(),
                ui.visuals(),
                rect,
                rect,
                &[40.0, 60.0],
                0.0,
                8.0,
                4.0,
                true,
            );
        });
    }

    #[test]
    fn test_paint_table_dividers_skips_header_separator_out_of_bounds() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            let rect = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(120.0, 20.0));
            renderer.paint_table_dividers(
                ui.painter(),
                ui.visuals(),
                rect,
                rect,
                &[40.0, 60.0],
                60.0,
                8.0,
                4.0,
                true,
            );
        });
    }

    #[test]
    fn test_paint_table_dividers_suppresses_dividers_when_false() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            let rect = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(120.0, 40.0));
            // Should not panic with draw_dividers=false; only outer border + header drawn
            renderer.paint_table_dividers(
                ui.painter(),
                ui.visuals(),
                rect,
                rect,
                &[40.0, 60.0],
                8.0,
                12.0,
                4.0,
                false,
            );
        });
    }

    #[test]
    fn test_paint_table_dividers_single_column_no_dividers() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            let rect = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(80.0, 30.0));
            renderer.paint_table_dividers(
                ui.painter(),
                ui.visuals(),
                rect,
                rect,
                &[80.0],
                8.0,
                6.0,
                4.0,
                false,
            );
        });
    }

    #[test]
    fn test_paint_table_text_job_adds_hover_for_wrapped_text() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text(
            "This is a long line that should wrap in a narrow cell.".to_string(),
        )];
        with_test_ui(|_, ui| {
            let build = renderer.build_layout_job(ui.style(), &spans, 20.0, false, Align::LEFT);
            let _response = renderer.paint_table_text_job(ui, 20.0, build);
        });
    }

    #[test]
    fn test_paint_table_text_job_adds_hover_for_multiline_text() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("Line 1\nLine 2".to_string())];
        with_test_ui(|_, ui| {
            let build = renderer.build_layout_job(ui.style(), &spans, 200.0, false, Align::LEFT);
            let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
            assert!(galley.rows.len() > 1);
            let _response = renderer.paint_table_text_job(ui, 200.0, build);
        });
    }

    #[test]
    fn test_paint_table_text_job_skips_hover_for_short_text() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("Short".to_string())];
        with_test_ui(|_, ui| {
            let build = renderer.build_layout_job(ui.style(), &spans, 200.0, false, Align::LEFT);
            let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
            assert_eq!(galley.rows.len(), 1);
            assert!(galley.size().x < 200.0);
            let _response = renderer.paint_table_text_job(ui, 200.0, build);
        });
    }

    #[test]
    fn test_paint_table_text_job_adds_hover_for_single_line_overflow() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text(
            "This is a single line that should overflow the render width.".to_string(),
        )];
        with_test_ui(|_, ui| {
            let build = renderer.build_layout_job(ui.style(), &spans, 400.0, false, Align::LEFT);
            let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
            assert_eq!(galley.rows.len(), 1);
            assert!(galley.size().x > 40.0);
            let _response = renderer.paint_table_text_job(ui, 40.0, build);
        });
    }

    #[test]
    fn test_link_at_pointer_outside_text_rect() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Link {
            text: "link".to_string(),
            url: "https://example.com".to_string(),
        }];
        let ctx = egui::Context::default();
        let outside_pos = std::cell::Cell::new(egui::pos2(1.0, 1.0));
        let layout_input = screen_input(220.0, 120.0);
        run_frame_with_input(&ctx, layout_input, |_, ui| {
            let build = renderer.build_layout_job(ui.style(), &spans, 40.0, false, Align::LEFT);
            let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
            let (rect, _response) =
                ui.allocate_exact_size(egui::vec2(200.0, galley.size().y), egui::Sense::hover());
            let text_origin =
                MarkdownRenderer::aligned_text_origin(rect, &galley, build.job.halign);
            let text_rect = galley.rect.translate(text_origin.to_vec2());
            let pos = egui::pos2(rect.right() - 2.0, text_rect.center().y);
            assert!(pos.x > text_rect.right());
            outside_pos.set(pos);
        });

        let mut input = screen_input(220.0, 120.0);
        input
            .events
            .push(egui::Event::PointerMoved(outside_pos.get()));
        run_frame_with_input(&ctx, input, |_, ui| {
            let build = renderer.build_layout_job(ui.style(), &spans, 40.0, false, Align::LEFT);
            let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
            let (rect, response) =
                ui.allocate_exact_size(egui::vec2(200.0, galley.size().y), egui::Sense::hover());
            let text_origin =
                MarkdownRenderer::aligned_text_origin(rect, &galley, build.job.halign);
            let link = renderer.link_at_pointer(&response, &galley, &build, text_origin);
            assert!(link.is_none());
        });
    }

    #[test]
    fn test_link_at_pointer_inside_text_rect_returns_link() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Link {
            text: "link".to_string(),
            url: "#anchor".to_string(),
        }];
        let ctx = egui::Context::default();
        let click_pos = std::cell::Cell::new(egui::pos2(1.0, 1.0));
        let layout_input = screen_input(220.0, 120.0);
        run_frame_with_input(&ctx, layout_input, |_, ui| {
            let build = renderer.build_layout_job(ui.style(), &spans, 40.0, false, Align::LEFT);
            let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
            let (rect, _response) =
                ui.allocate_exact_size(egui::vec2(200.0, galley.size().y), egui::Sense::hover());
            let text_origin =
                MarkdownRenderer::aligned_text_origin(rect, &galley, build.job.halign);
            click_pos.set(text_origin + egui::vec2(2.0, 2.0));
        });

        let mut input = screen_input(220.0, 120.0);
        input
            .events
            .push(egui::Event::PointerMoved(click_pos.get()));
        run_frame_with_input(&ctx, input, |_, ui| {
            let build = renderer.build_layout_job(ui.style(), &spans, 40.0, false, Align::LEFT);
            let galley = ui.fonts(|f| f.layout_job(build.job.clone()));
            let (rect, response) =
                ui.allocate_exact_size(egui::vec2(200.0, galley.size().y), egui::Sense::hover());
            let text_origin =
                MarkdownRenderer::aligned_text_origin(rect, &galley, build.job.halign);
            let link = renderer.link_at_pointer(&response, &galley, &build, text_origin);
            assert!(link.is_some());
        });
    }

    #[test]
    fn test_element_plain_text_list_quote_table_spacing() {
        let list = MarkdownElement::List {
            ordered: false,
            items: vec![ListItem {
                blocks: vec![
                    MarkdownElement::Paragraph(vec![InlineSpan::Text("One".to_string())]),
                    MarkdownElement::Paragraph(vec![InlineSpan::Text("Two".to_string())]),
                ],
            }],
        };
        let list_text = MarkdownRenderer::element_plain_text(&list);
        assert!(list_text.contains('\n'));

        let quote = MarkdownElement::Quote {
            depth: 1,
            blocks: vec![
                MarkdownElement::Paragraph(vec![InlineSpan::Text("Alpha".to_string())]),
                MarkdownElement::Paragraph(vec![InlineSpan::Text("Beta".to_string())]),
            ],
        };
        let quote_text = MarkdownRenderer::element_plain_text(&quote);
        assert!(quote_text.contains('\n'));

        let table = MarkdownElement::Table {
            headers: vec![
                vec![InlineSpan::Text("H1".to_string())],
                vec![InlineSpan::Text("H2".to_string())],
            ],
            rows: vec![vec![
                vec![InlineSpan::Text("A".to_string())],
                vec![InlineSpan::Text("B".to_string())],
            ]],
            alignments: vec![],
        };
        let table_text = MarkdownRenderer::element_plain_text(&table);
        assert_eq!(table_text, "H1 H2 A B");
    }

    #[test]
    fn test_resolve_image_path_data_uri_and_remote_texture_reject() {
        let renderer = MarkdownRenderer::new();
        let data_uri = "data:image/png;base64,AAAA";
        assert_eq!(renderer.resolve_image_path(data_uri), data_uri);
        with_test_ui(|_, ui| {
            assert!(renderer
                .get_or_load_image_texture(ui, "https://example.com/image.png")
                .is_none());
            assert!(renderer
                .get_or_load_image_texture(ui, "http://example.com/image.png")
                .is_none());
        });
    }

    #[test]
    fn test_is_html_line_break_rejects_non_br() {
        assert!(!MarkdownRenderer::is_html_line_break("<div>"));
        assert!(!MarkdownRenderer::is_html_line_break("br"));
    }

    #[test]
    fn test_table_line_info_variants_cover_edges() {
        let (level, rest) = MarkdownRenderer::table_line_info("text");
        assert_eq!(level, 0);
        assert_eq!(rest, "text");

        let (level, rest) = MarkdownRenderer::table_line_info("   text");
        assert_eq!(level, 0);
        assert_eq!(rest, "   text");

        let (level, rest) = MarkdownRenderer::table_line_info(">abc");
        assert_eq!(level, 1);
        assert_eq!(rest, "abc");

        let (level, rest) = MarkdownRenderer::table_line_info(" >\tabc");
        assert_eq!(level, 1);
        assert_eq!(rest, "abc");

        let (level, rest) = MarkdownRenderer::table_line_info(">");
        assert_eq!(level, 1);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_list_marker_info_rejects_missing_delimiter_or_spacing() {
        assert!(MarkdownRenderer::list_marker_info("1 item").is_none());
        assert!(MarkdownRenderer::list_marker_info("1.item").is_none());
    }

    #[test]
    fn test_list_marker_info_numeric_missing_punctuation() {
        assert!(MarkdownRenderer::list_marker_info("12").is_none());
        assert!(MarkdownRenderer::list_marker_info_any_indent("  12").is_none());
    }

    #[test]
    fn test_list_marker_info_any_indent_rejects_missing_delimiter_or_spacing() {
        assert!(MarkdownRenderer::list_marker_info_any_indent("   1 item").is_none());
        assert!(MarkdownRenderer::list_marker_info_any_indent("  2.item").is_none());
    }

    #[test]
    fn test_strip_indent_columns_insufficient_indent_returns_none() {
        assert!(MarkdownRenderer::strip_indent_columns("  text", 4).is_none());
    }

    #[test]
    fn test_strip_indent_columns_whitespace_short_returns_none() {
        assert!(MarkdownRenderer::strip_indent_columns("  ", 4).is_none());
    }

    #[test]
    fn test_has_pipe_outside_inline_code_unmatched_backticks() {
        let line = "`code`` | still in code";
        assert!(!MarkdownRenderer::has_pipe_outside_inline_code(line));
    }

    #[test]
    fn test_is_table_delimiter_line_requires_dash_and_pipe() {
        assert!(!MarkdownRenderer::is_table_delimiter_line("----"));
        assert!(!MarkdownRenderer::is_table_delimiter_line("| ::: |"));
    }

    #[test]
    fn test_append_text_sections_preserves_color_when_highlighted() {
        let renderer = MarkdownRenderer::new();
        let style = egui::Style::default();
        let mut job = LayoutJob::default();
        let mut plain_text = String::new();
        let inline_style = InlineStyle {
            strong: false,
            italics: false,
            strike: false,
            color: Some(Color32::from_rgb(10, 20, 30)),
        };
        let count = renderer.append_text_sections(
            &style,
            &mut job,
            &mut plain_text,
            "hit",
            renderer.font_sizes.body,
            inline_style,
            Some("hit"),
        );
        assert_eq!(count, 3);
        assert_eq!(plain_text, "hit");
    }

    #[test]
    fn test_render_text_with_emojis_leading_emoji() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            renderer.render_text_with_emojis(ui, "😀 test", 14.0, InlineStyle::default());
        });
    }

    #[test]
    fn test_persist_resizable_widths_records_change() {
        let renderer = MarkdownRenderer::new();
        let spec = resizable_spec(0, "col", 40.0, 80.0);
        let hash = spec.policy_hash;
        renderer.persist_resizable_widths(42, &[spec], &[120.0]);
        let metrics = renderer.table_metrics.borrow();
        let entry = metrics.entry(42).expect("metrics entry");
        assert_eq!(entry.persisted_width(hash), Some(120.0));
    }

    #[test]
    fn test_env_var_guard_restores_previous_value() {
        std::env::set_var("MDMDVIEW_TEST_ENV", "before");
        {
            let _guard = EnvVarGuard::set("MDMDVIEW_TEST_ENV", "after");
            assert_eq!(
                std::env::var("MDMDVIEW_TEST_ENV").ok().as_deref(),
                Some("after")
            );
        }
        assert_eq!(
            std::env::var("MDMDVIEW_TEST_ENV").ok().as_deref(),
            Some("before")
        );
        std::env::remove_var("MDMDVIEW_TEST_ENV");
    }

    #[test]
    fn test_parse_element_skips_unhandled_event() {
        let renderer = MarkdownRenderer::new();
        let events = vec![Event::Text("orphan".into())];
        let mut elements = Vec::new();
        let mut slugs = std::collections::HashMap::new();
        let next = renderer.parse_element(&events, 0, &mut elements, &mut slugs);
        assert_eq!(next, events.len());
        assert!(elements.is_empty());
    }

    #[test]
    fn test_parse_inline_spans_link_and_image() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Paragraph),
            Event::Start(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Text("Link".into()),
            Event::End(Tag::Link(LinkType::Inline, "".into(), "".into())),
            Event::Start(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::Text("Alt".into()),
            Event::End(Tag::Image(LinkType::Inline, "".into(), "".into())),
            Event::End(Tag::Paragraph),
        ];
        let (spans, next) = renderer.parse_inline_spans(&events, 1, Tag::Paragraph);
        assert_eq!(next, events.len());
        assert!(spans
            .iter()
            .any(|span| matches!(span, InlineSpan::Link { .. })));
        assert!(spans
            .iter()
            .any(|span| matches!(span, InlineSpan::Image { .. })));
    }

    #[test]
    fn test_parse_inline_spans_with_breaks_link_and_image() {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::Paragraph),
            Event::Start(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Text("Link".into()),
            Event::End(Tag::Link(LinkType::Inline, "".into(), "".into())),
            Event::Start(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::Text("Alt".into()),
            Event::End(Tag::Image(LinkType::Inline, "".into(), "".into())),
            Event::End(Tag::Paragraph),
        ];
        let (spans, next) =
            renderer.parse_inline_spans_with_breaks(&events, 1, Tag::Paragraph, false);
        assert_eq!(next, events.len());
        assert!(spans
            .iter()
            .any(|span| matches!(span, InlineSpan::Link { .. })));
        assert!(spans
            .iter()
            .any(|span| matches!(span, InlineSpan::Image { .. })));
    }

    #[test]
    fn test_parse_list_item_heading_builds_header() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::List(None)),
            Event::Start(Tag::Item),
            Event::Start(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::Text("Title".into()),
            Event::End(Tag::Heading(pulldown_cmark::HeadingLevel::H2, None, vec![])),
            Event::End(Tag::Item),
            Event::End(Tag::List(None)),
        ];
        let mut slugs = std::collections::HashMap::new();
        let (items, next) = renderer.parse_list(&events, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert!(matches!(items[0].blocks[0], MarkdownElement::Header { .. }));
        Ok(())
    }

    #[test]
    fn test_parse_list_inline_formats_without_paragraph() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let events = vec![
            Event::Start(Tag::List(None)),
            Event::Start(Tag::Item),
            Event::Start(Tag::Emphasis),
            Event::Text("em".into()),
            Event::End(Tag::Emphasis),
            Event::Start(Tag::Strikethrough),
            Event::Text("strike".into()),
            Event::End(Tag::Strikethrough),
            Event::Start(Tag::Link(
                LinkType::Inline,
                "https://example.com".into(),
                "".into(),
            )),
            Event::Text("link".into()),
            Event::End(Tag::Link(LinkType::Inline, "".into(), "".into())),
            Event::Start(Tag::Image(
                LinkType::Inline,
                "img.png".into(),
                "Title".into(),
            )),
            Event::Text("Alt".into()),
            Event::End(Tag::Image(LinkType::Inline, "".into(), "".into())),
            Event::End(Tag::Item),
            Event::End(Tag::List(None)),
        ];
        let mut slugs = std::collections::HashMap::new();
        let (items, next) = renderer.parse_list(&events, 1, &mut slugs);
        assert_eq!(next, events.len());
        assert!(matches!(items[0].blocks[0], MarkdownElement::Paragraph(_)));
        let dummy = MarkdownElement::HorizontalRule;
        let mut saw_paragraph = false;
        for element in [&items[0].blocks[0], &dummy] {
            if let MarkdownElement::Paragraph(spans) = element {
                assert!(spans
                    .iter()
                    .any(|span| matches!(span, InlineSpan::Emphasis(_))));
                assert!(spans
                    .iter()
                    .any(|span| matches!(span, InlineSpan::Strikethrough(_))));
                assert!(spans
                    .iter()
                    .any(|span| matches!(span, InlineSpan::Link { .. })));
                assert!(spans
                    .iter()
                    .any(|span| matches!(span, InlineSpan::Image { .. })));
                saw_paragraph = true;
            }
        }
        assert!(saw_paragraph);
        Ok(())
    }

    #[test]
    fn test_parse_table_parses_header_and_rows() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let alignments = vec![Alignment::Left];
        let events = vec![
            Event::Start(Tag::Table(alignments.clone())),
            Event::Start(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("H".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::End(Tag::TableHead),
            Event::Start(Tag::TableRow),
            Event::Start(Tag::TableCell),
            Event::Text("A".into()),
            Event::End(Tag::TableCell),
            Event::End(Tag::TableRow),
            Event::End(Tag::Table(alignments)),
        ];
        let (headers, rows, next) = renderer.parse_table(&events, 1);
        assert_eq!(next, events.len());
        assert_eq!(headers.len(), 1);
        assert_eq!(rows.len(), 1);
        Ok(())
    }

    #[test]
    fn test_allow_remote_images_getter() {
        let renderer = MarkdownRenderer::new();
        assert!(!renderer.allow_remote_images());
        renderer.set_allow_remote_images(true);
        assert!(renderer.allow_remote_images());
        renderer.set_allow_remote_images(false);
        assert!(!renderer.allow_remote_images());
    }

    #[test]
    fn test_render_link_with_emoji_characters() {
        let renderer = MarkdownRenderer::new();
        // Create a link whose text contains actual emoji characters (triggers emoji link path)
        let md = "[🚀 Launch](https://example.com)\n";
        let elements = renderer.parse(md).expect("parse ok");
        with_test_ui(|_, ui| {
            renderer.render_to_ui(ui, &elements);
        });
    }

    #[test]
    fn test_render_link_with_mixed_emoji_and_text() {
        let renderer = MarkdownRenderer::new();
        let md = "[Hello 🌍 World](https://example.com)\n";
        let elements = renderer.parse(md).expect("parse ok");
        with_test_ui(|_, ui| {
            renderer.render_to_ui(ui, &elements);
        });
    }

    #[test]
    fn test_render_text_with_search_highlighting_active() {
        let renderer = MarkdownRenderer::new();
        let md = "This is a test paragraph with some searchable words in it.\n";
        let elements = renderer.parse(md).expect("parse ok");
        renderer.set_highlight_phrase(Some("searchable"));
        with_test_ui(|_, ui| {
            renderer.render_to_ui(ui, &elements);
        });
    }

    #[test]
    fn test_render_strong_link_with_emoji() {
        let renderer = MarkdownRenderer::new();
        // Bold link with emoji in text
        let md = "[**🎉 Celebration**](https://example.com)\n";
        let elements = renderer.parse(md).expect("parse ok");
        with_test_ui(|_, ui| {
            renderer.render_to_ui(ui, &elements);
        });
    }

    #[test]
    fn test_render_text_with_emojis_and_styles() {
        let renderer = MarkdownRenderer::new();
        let md = "Text with 🚀 emoji and **bold** and *italic* and `code` inline.\n";
        let elements = renderer.parse(md).expect("parse ok");
        with_test_ui(|_, ui| {
            renderer.render_to_ui(ui, &elements);
        });
    }

    #[test]
    fn test_render_link_context_menu_in_test_mode() {
        let renderer = MarkdownRenderer::new();
        let md = "[Click here](https://example.com)\n";
        let elements = renderer.parse(md).expect("parse ok");
        with_test_ui(|_, ui| {
            renderer.render_to_ui(ui, &elements);
        });
    }

    #[test]
    fn test_set_dark_mode() {
        let mut renderer = MarkdownRenderer::new();
        assert!(renderer.dark_mode);
        renderer.set_dark_mode(false);
        assert!(!renderer.dark_mode);
        renderer.set_dark_mode(true);
        assert!(renderer.dark_mode);
    }

    #[test]
    fn test_highlight_code_light_theme() {
        let mut renderer = MarkdownRenderer::new();

        // Parse with dark theme
        let code = "let x = 42;";
        let dark_tokens = renderer
            .highlight_code(Some("rust"), code)
            .expect("dark highlight");

        // Switch to light theme and re-highlight
        renderer.set_dark_mode(false);
        let light_tokens = renderer
            .highlight_code(Some("rust"), code)
            .expect("light highlight");

        // The two themes should produce different token colors
        assert!(!dark_tokens.is_empty());
        assert!(!light_tokens.is_empty());
        // At least one token color should differ between themes
        let dark_first_color = dark_tokens[0][0].color;
        let light_first_color = light_tokens[0][0].color;
        assert_ne!(dark_first_color, light_first_color);
    }
}
