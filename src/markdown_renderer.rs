use crate::image_decode;
use crate::mermaid_renderer::MermaidRenderer;
use crate::table_support::{
    compute_column_stats, derive_column_specs, ColumnPolicy, ColumnSpec, ColumnStat,
    TableColumnContext, TableMetrics, WidthChange,
};
use crate::{emoji_assets, emoji_catalog};
use anyhow::Result;
use egui::{
    text::{Galley, LayoutJob, TextWrapping},
    Align, Color32, Context, FontSelection, Painter, RichText, Stroke, Vec2, Visuals,
};
use egui_extras::TableBuilder;
use pulldown_cmark::{Alignment, Event, LinkType, Options, Parser, Tag};
use std::cell::RefCell;
use std::collections::{hash_map::DefaultHasher, HashMap, VecDeque};
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::LinesWithEndings;
use unicode_casefold::UnicodeCaseFold;
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

#[cfg(test)]
use std::collections::HashSet;

#[derive(Clone, Copy, Default)]
struct InlineStyle {
    strong: bool,
    italics: bool,
    strike: bool,
    color: Option<Color32>,
}

#[cfg(test)]
thread_local! {
    static FORCED_RENDER_ACTIONS: RefCell<HashSet<&'static str>> = RefCell::new(HashSet::new());
}

#[cfg(test)]
fn render_action_triggered(triggered: bool, action: &'static str) -> bool {
    triggered || FORCED_RENDER_ACTIONS.with(|actions| actions.borrow().contains(action))
}

#[cfg(not(test))]
fn render_action_triggered(triggered: bool, _action: &'static str) -> bool {
    triggered
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

impl InlineSpan {
    fn text_content(&self) -> Option<&str> {
        match self {
            InlineSpan::Text(t)
            | InlineSpan::Code(t)
            | InlineSpan::Strong(t)
            | InlineSpan::Emphasis(t)
            | InlineSpan::Strikethrough(t) => Some(t.as_str()),
            InlineSpan::Link { text, .. } => Some(text.as_str()),
            InlineSpan::Image { .. } => None,
        }
    }

    fn is_text_like(&self) -> bool {
        !matches!(self, InlineSpan::Image { .. })
    }
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct CellLayoutKey {
    row: Option<usize>,
    col: usize,
    width: u32,
    strong: bool,
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
    },
    List {
        ordered: bool,
        items: Vec<Vec<InlineSpan>>,
    }, // List items are also inline spans
    Quote {
        depth: u8,
        lines: Vec<Vec<InlineSpan>>,
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

/// Type alias for quote lines (each line is a sequence of inline spans)
type QuoteLines = Vec<Vec<InlineSpan>>;

struct ImageCacheEntry {
    texture: egui::TextureHandle,
    size: [u32; 2],
    modified: Option<SystemTime>,
}

/// Markdown renderer with proper inline element handling
pub struct MarkdownRenderer {
    font_sizes: FontSizes,
    syntax_set: SyntaxSet,
    theme_set: ThemeSet,
    emoji_textures: RefCell<HashMap<String, egui::TextureHandle>>,
    image_textures: RefCell<HashMap<String, ImageCacheEntry>>,
    // Mapping of header id -> last rendered rect (for in-document navigation)
    header_rects: RefCell<HashMap<String, egui::Rect>>,
    // Last clicked internal anchor (e.g., "getting-started") for the app to consume
    pending_anchor: RefCell<Option<String>>,
    // Unique counter to avoid egui Id collisions for repeated links
    link_counter: RefCell<u64>,
    // Per-frame rect for each top-level element in render order
    element_rects: RefCell<Vec<egui::Rect>>,
    // Optional highlight phrase (lowercased) for in-text highlighting
    highlight_phrase: RefCell<Option<String>>,
    // Cache for image/diagram textures
    // Base directory used to resolve relative image paths
    base_dir: RefCell<Option<PathBuf>>,
    mermaid: MermaidRenderer,
    table_wrap_overhaul_enabled: bool,
    table_layout_cache: RefCell<CellLayoutCache>,
    table_metrics: RefCell<TableMetrics>,
    column_stats_cache: RefCell<HashMap<u64, ColumnStatsCacheEntry>>,
}

impl Default for MarkdownRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl MarkdownRenderer {
    #[cfg_attr(not(test), allow(dead_code))]
    fn cell_fragments<'a>(&'a self, spans: &'a [InlineSpan]) -> Vec<CellFragment<'a>> {
        let mut fragments = Vec::new();
        let mut run_start: Option<usize> = None;
        let flush_run =
            |start: &mut Option<usize>, end: usize, fragments: &mut Vec<CellFragment<'a>>| {
                if let Some(run_begin) = start.take() {
                    if run_begin < end {
                        fragments.push(CellFragment::Text(&spans[run_begin..end]));
                    }
                }
            };

        for (idx, span) in spans.iter().enumerate() {
            if let Some(emoji_key) = self.span_is_single_emoji(span) {
                flush_run(&mut run_start, idx, &mut fragments);
                fragments.push(CellFragment::Emoji(emoji_key));
                continue;
            }

            if matches!(span, InlineSpan::Image { .. }) {
                flush_run(&mut run_start, idx, &mut fragments);
                fragments.push(CellFragment::Image(span));
                continue;
            }

            if span.is_text_like() && run_start.is_none() {
                run_start = Some(idx);
            }
        }

        if let Some(start) = run_start {
            fragments.push(CellFragment::Text(&spans[start..]));
        }

        fragments
    }

    fn span_is_single_emoji(&self, span: &InlineSpan) -> Option<String> {
        let text = span.text_content()?;
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return None;
        }
        let mut graphemes = trimmed.graphemes(true);
        let first = graphemes.next()?;
        if graphemes.next().is_some() {
            return None;
        }
        if first != trimmed {
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

        if folded.is_empty() {
            return vec![(0..text.len(), false)];
        }

        let mut segments = Vec::new();
        let mut rendered_until = 0usize;
        let mut search_at = 0usize;
        while let Some(pos) = folded[search_at..].find(needle) {
            let abs = search_at + pos;
            if abs >= folded_to_char.len() {
                break;
            }
            let start_char_idx = folded_to_char[abs];
            let (start_byte, _) = char_ranges[start_char_idx];
            if start_byte > rendered_until {
                segments.push((rendered_until..start_byte, false));
            }
            let match_end = abs + needle.len();
            let end_char_idx = if match_end == 0 {
                start_char_idx
            } else {
                folded_to_char[match_end.saturating_sub(1)]
            };
            let (_, end_byte) = char_ranges[end_char_idx];
            segments.push((start_byte..end_byte, true));
            rendered_until = end_byte;
            search_at = match_end;
        }

        if rendered_until < text.len() {
            segments.push((rendered_until..text.len(), false));
        }

        if segments.is_empty() {
            segments.push((0..text.len(), false));
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
    ) -> LayoutJobBuild {
        let mut job = LayoutJob {
            wrap: TextWrapping {
                max_width: wrap_width.max(1.0),
                ..Default::default()
            },
            break_on_newline: true,
            halign: Align::LEFT,
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
                    let inline_style = InlineStyle {
                        strong: strong_override,
                        color: Some(if Self::is_external_url(url) {
                            Color32::from_rgb(120, 190, 255)
                        } else {
                            Color32::LIGHT_BLUE
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
        if normalized.is_empty() {
            return 0;
        }
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
        let visuals = &style.visuals;
        let (bg, fg) = if visuals.dark_mode {
            (
                Color32::from_rgb(30, 30, 30),
                Color32::from_rgb(180, 255, 180),
            )
        } else {
            (Color32::WHITE, Color32::from_rgb(60, 80, 150))
        };
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
            if range.is_empty() {
                continue;
            }
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
        Self {
            font_sizes: FontSizes::default(),
            syntax_set: SyntaxSet::load_defaults_newlines(),
            theme_set: ThemeSet::load_defaults(),
            emoji_textures: RefCell::new(HashMap::new()),
            image_textures: RefCell::new(HashMap::new()),
            header_rects: RefCell::new(HashMap::new()),
            pending_anchor: RefCell::new(None),
            link_counter: RefCell::new(0),
            element_rects: RefCell::new(Vec::new()),
            highlight_phrase: RefCell::new(None),
            base_dir: RefCell::new(None),
            mermaid,
            table_wrap_overhaul_enabled: true,
            table_layout_cache: RefCell::new(CellLayoutCache::new(TABLE_LAYOUT_CACHE_CAPACITY)),
            table_metrics: RefCell::new(TableMetrics::default()),
            column_stats_cache: RefCell::new(HashMap::new()),
        }
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

    /// Parse markdown content into elements with proper inline handling
    pub fn parse(&self, markdown: &str) -> Result<Vec<MarkdownElement>, anyhow::Error> {
        let mut options = Options::empty();
        options.insert(Options::ENABLE_STRIKETHROUGH);
        options.insert(Options::ENABLE_TABLES);
        options.insert(Options::ENABLE_TASKLISTS);

        let parser = Parser::new_ext(markdown, options);
        let mut elements = Vec::new();
        let events = parser.collect::<Vec<_>>();

        let mut i = 0;
        // Track header slug occurrences for stable de-duplication
        let mut slug_counts: HashMap<String, usize> = HashMap::new();
        while i < events.len() {
            i = self.parse_element(&events, i, &mut elements, &mut slug_counts)?;
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
    ) -> Result<usize, anyhow::Error> {
        match &events[start] {
            Event::Start(Tag::Paragraph) => {
                // Always preserve line breaks in paragraphs (consistent with blockquotes)
                // This allows poetry, lyrics, and structured content to render correctly
                let (spans, next_idx) =
                    self.parse_inline_spans_with_breaks(events, start + 1, Tag::Paragraph, true)?;
                if !spans.is_empty() {
                    elements.push(MarkdownElement::Paragraph(spans));
                }
                Ok(next_idx)
            }
            // Images encountered outside paragraphs: treat as a single paragraph with an inline image
            Event::Start(Tag::Image(_, url, title)) => {
                let (alt, next_idx) = self.collect_until_tag_end(
                    events,
                    start + 1,
                    Tag::Image(LinkType::Inline, "".into(), "".into()),
                )?;
                let mut spans: Vec<InlineSpan> = Vec::new();
                spans.push(InlineSpan::Image {
                    src: url.to_string(),
                    alt,
                    title: if title.is_empty() {
                        None
                    } else {
                        Some(title.to_string())
                    },
                });
                elements.push(MarkdownElement::Paragraph(spans));
                Ok(next_idx)
            }
            Event::Start(Tag::Heading(level, _, _)) => {
                let (spans, next_idx) =
                    self.parse_inline_spans(events, start + 1, Tag::Heading(*level, None, vec![]))?;
                let title_text = Self::spans_plain_text(&spans);
                let base = Self::slugify(&title_text);
                let count = slug_counts.entry(base.clone()).or_insert(0);
                let id = if *count == 0 {
                    base.clone()
                } else {
                    format!("{}-{}", base, *count)
                };
                *count += 1;
                elements.push(MarkdownElement::Header {
                    level: *level as u8,
                    spans,
                    id,
                });
                Ok(next_idx)
            }
            Event::Start(Tag::CodeBlock(_)) => {
                let (code_text, language, next_idx) = self.parse_code_block(events, start)?;
                elements.push(MarkdownElement::CodeBlock {
                    language,
                    text: code_text,
                });
                Ok(next_idx)
            }
            Event::Start(Tag::List(first_item)) => {
                let (items, next_idx) = self.parse_list(events, start + 1, *first_item, 0)?;
                elements.push(MarkdownElement::List {
                    ordered: first_item.is_some(),
                    items,
                });
                Ok(next_idx)
            }
            Event::Rule => {
                elements.push(MarkdownElement::HorizontalRule);
                Ok(start + 1)
            }
            Event::Start(Tag::BlockQuote) => {
                let (quotes, next_idx) = self.collect_blockquotes(events, start + 1, 1)?;
                for (depth, lines) in quotes {
                    if !lines.is_empty() {
                        elements.push(MarkdownElement::Quote { depth, lines });
                    }
                }
                Ok(next_idx)
            }
            Event::Start(Tag::Table(alignments)) => {
                let (headers, rows, next_idx) = self.parse_table(events, start + 1)?;
                elements.push(MarkdownElement::Table {
                    headers,
                    rows,
                    alignments: alignments.to_vec(),
                });
                Ok(next_idx)
            }
            _ => {
                // Skip other events
                Ok(start + 1)
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
                        if !result.is_empty() {
                            result.push('\n');
                        }
                        result.push_str(&Self::spans_plain_text(item));
                    }
                }
                MarkdownElement::Quote { lines, .. } => {
                    for line in lines {
                        if !result.is_empty() {
                            result.push('\n');
                        }
                        result.push_str(&Self::spans_plain_text(line));
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

    /// Collect blockquotes into (depth, lines) entries; supports nesting and multi-line content
    fn collect_blockquotes(
        &self,
        events: &[Event],
        start: usize,
        depth: u8,
    ) -> Result<(Vec<(u8, QuoteLines)>, usize), anyhow::Error> {
        let mut i = start;
        let mut result: Vec<(u8, QuoteLines)> = Vec::new();
        let mut lines: QuoteLines = Vec::new();
        let mut current: Vec<InlineSpan> = Vec::new();

        let push_line = |current: &mut Vec<InlineSpan>, lines: &mut QuoteLines| {
            lines.push(std::mem::take(current));
        };

        while i < events.len() {
            match &events[i] {
                Event::Start(Tag::Paragraph) => {
                    let (mut para, next) =
                        self.parse_inline_spans_with_breaks(events, i + 1, Tag::Paragraph, true)?;
                    // Split para into lines on explicit "\n"
                    let mut line: Vec<InlineSpan> = Vec::new();
                    while let Some(span) = para.first().cloned() {
                        para.remove(0);
                        match span {
                            InlineSpan::Text(t) if t.contains('\n') => {
                                let parts: Vec<&str> = t.split('\n').collect();
                                for (pi, part) in parts.iter().enumerate() {
                                    if !part.is_empty() {
                                        line.push(InlineSpan::Text(part.to_string()));
                                    }
                                    if pi < parts.len() - 1 {
                                        push_line(&mut line, &mut lines);
                                        line = Vec::new();
                                    }
                                }
                            }
                            other => line.push(other),
                        }
                    }
                    if !line.is_empty() {
                        push_line(&mut line, &mut lines);
                    }
                    i = next;
                }
                Event::SoftBreak | Event::HardBreak => {
                    push_line(&mut current, &mut lines);
                    i += 1;
                }
                Event::Start(Tag::BlockQuote) => {
                    // flush current collected lines for this depth
                    if !current.is_empty() {
                        push_line(&mut current, &mut lines);
                    }
                    if !lines.is_empty() {
                        result.push((depth, std::mem::take(&mut lines)));
                    }
                    // collect nested
                    let (nested, next) = self.collect_blockquotes(events, i + 1, depth + 1)?;
                    result.extend(nested);
                    i = next;
                }
                Event::End(Tag::BlockQuote) => {
                    if !current.is_empty() {
                        push_line(&mut current, &mut lines);
                    }
                    if !lines.is_empty() {
                        result.push((depth, lines));
                    }
                    return Ok((result, i + 1));
                }
                _ => {
                    i += 1;
                }
            }
        }
        if !current.is_empty() {
            push_line(&mut current, &mut lines);
        }
        if !lines.is_empty() {
            result.push((depth, lines));
        }
        Ok((result, i))
    }

    /// Parse inline spans until reaching the end tag
    fn parse_inline_spans_with_breaks(
        &self,
        events: &[Event],
        start: usize,
        end_tag: Tag,
        keep_breaks: bool,
    ) -> Result<(Vec<InlineSpan>, usize), anyhow::Error> {
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
                    return Ok((spans, i + 1));
                }
                Event::Text(text) => {
                    text_buffer.push_str(text);
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
                    spans.push(InlineSpan::Code(code.to_string()));
                    i += 1;
                }
                Event::Start(Tag::Strong) => {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    let (inner_text, next_i) =
                        self.collect_until_tag_end(events, i + 1, Tag::Strong)?;
                    spans.push(InlineSpan::Strong(inner_text));
                    i = next_i;
                }
                Event::Start(Tag::Emphasis) => {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    let (inner_text, next_i) =
                        self.collect_until_tag_end(events, i + 1, Tag::Emphasis)?;
                    spans.push(InlineSpan::Emphasis(inner_text));
                    i = next_i;
                }
                Event::Start(Tag::Strikethrough) => {
                    if !text_buffer.is_empty() {
                        spans.push(InlineSpan::Text(text_buffer.clone()));
                        text_buffer.clear();
                    }
                    let (inner_text, next_i) =
                        self.collect_until_tag_end(events, i + 1, Tag::Strikethrough)?;
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
                    )?;
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
                    )?;
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

        Ok((spans, i))
    }

    /// Default inline parsing without preserving explicit line breaks
    fn parse_inline_spans(
        &self,
        events: &[Event],
        start: usize,
        end_tag: Tag,
    ) -> Result<(Vec<InlineSpan>, usize), anyhow::Error> {
        self.parse_inline_spans_with_breaks(events, start, end_tag, false)
    }

    /// Collect text until a specific end tag
    fn collect_until_tag_end(
        &self,
        events: &[Event],
        start: usize,
        end_tag: Tag,
    ) -> Result<(String, usize), anyhow::Error> {
        let mut text = String::new();
        let mut i = start;

        while i < events.len() {
            match &events[i] {
                Event::End(tag)
                    if std::mem::discriminant(tag) == std::mem::discriminant(&end_tag) =>
                {
                    return Ok((text, i + 1));
                }
                Event::Text(t) => {
                    text.push_str(t);
                }
                Event::Code(code) => {
                    text.push_str(code);
                }
                _ => {}
            }
            i += 1;
        }

        Ok((text, i))
    }

    /// Parse a code block
    fn parse_code_block(
        &self,
        events: &[Event],
        start: usize,
    ) -> Result<(String, Option<String>, usize), anyhow::Error> {
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
                    return Ok((code_text, language, i + 1));
                }
                Event::Text(text) => {
                    code_text.push_str(text);
                }
                _ => {}
            }
            i += 1;
        }

        Ok((code_text, language, i))
    }

    /// Parse a list with basic nested-list flattening
    fn parse_list(
        &self,
        events: &[Event],
        start: usize,
        first_item: Option<u64>,
        depth: usize,
    ) -> Result<(Vec<Vec<InlineSpan>>, usize), anyhow::Error> {
        let mut items: Vec<Vec<InlineSpan>> = Vec::new();
        let mut i = start;
        let _is_ordered = first_item.is_some();

        while i < events.len() {
            match &events[i] {
                Event::End(Tag::List(_)) => return Ok((items, i + 1)),
                Event::Start(Tag::Item) => {
                    i += 1;
                    let mut spans: Vec<InlineSpan> = Vec::new();
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
                                let (ps, next) =
                                    self.parse_inline_spans(events, i + 1, Tag::Paragraph)?;
                                spans.extend(ps);
                                i = next;
                            }
                            Event::Code(code) => {
                                spans.push(InlineSpan::Code(code.to_string()));
                                i += 1;
                            }
                            Event::Start(Tag::Emphasis) => {
                                let (inner_text, next) =
                                    self.collect_until_tag_end(events, i + 1, Tag::Emphasis)?;
                                spans.push(InlineSpan::Emphasis(inner_text));
                                i = next;
                            }
                            Event::Start(Tag::Strong) => {
                                let (inner_text, next) =
                                    self.collect_until_tag_end(events, i + 1, Tag::Strong)?;
                                spans.push(InlineSpan::Strong(inner_text));
                                i = next;
                            }
                            Event::Start(Tag::Strikethrough) => {
                                let (inner_text, next) =
                                    self.collect_until_tag_end(events, i + 1, Tag::Strikethrough)?;
                                spans.push(InlineSpan::Strikethrough(inner_text));
                                i = next;
                            }
                            Event::Start(Tag::Link(_, url, _)) => {
                                let url_str = url.to_string();
                                let (link_text, next) = self.collect_until_tag_end(
                                    events,
                                    i + 1,
                                    Tag::Link(LinkType::Inline, "".into(), "".into()),
                                )?;
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
                                )?;
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
                            Event::Start(Tag::List(child_first)) => {
                                let (child_items, next) =
                                    self.parse_list(events, i + 1, *child_first, depth + 1)?;
                                i = next;
                                let ordered_child = child_first.is_some();
                                for (idx, child) in child_items.into_iter().enumerate() {
                                    spans.push(InlineSpan::Text("\n".to_string()));
                                    // Indentation: 4 spaces per nested level
                                    let indent = " ".repeat(4 * (depth + 1));
                                    let marker = if ordered_child {
                                        format!("{}.", idx + 1)
                                    } else {
                                        "-".to_string()
                                    };
                                    spans.push(InlineSpan::Text(format!("{}{} ", indent, marker)));
                                    spans.extend(child);
                                }
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
                    items.push(spans);
                }
                _ => i += 1,
            }
        }

        Ok((items, i))
    }

    /// Parse a table with headers and rows
    fn parse_table(
        &self,
        events: &[Event],
        start: usize,
    ) -> Result<TableParseResult, anyhow::Error> {
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
                                )?;
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
                                )?;
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
                Event::End(Tag::Table(_)) => return Ok((headers, rows, i + 1)),
                _ => i += 1,
            }
        }
        Ok((headers, rows, i))
    }

    /// Render parsed markdown elements to egui UI
    pub fn render_to_ui(&self, ui: &mut egui::Ui, elements: &[MarkdownElement]) {
        // Clear header rects before rendering a new frame
        self.header_rects.borrow_mut().clear();
        // Reset per-frame link counter to ensure link IDs are stable across frames
        *self.link_counter.borrow_mut() = 0;
        // Reset per-frame element rects
        self.element_rects.borrow_mut().clear();
        for (element_idx, element) in elements.iter().enumerate() {
            // Wrap each element in a no-op frame to capture its rect
            let ir = egui::Frame::none().show(ui, |ui| {
                match element {
                    MarkdownElement::Paragraph(spans) => {
                        self.render_inline_spans(ui, spans);
                        ui.add_space(4.0);
                    }
                    MarkdownElement::Quote { depth, lines } => {
                        ui.add_space(4.0);
                        let bar_width = 3.0;
                        let bar_gap = 6.0;
                        let left_pad = 10.0 + (*depth as f32) * (bar_width + bar_gap);
                        // Substack-like styling: dark grey block with orange accent bars and white text
                        let bg = Color32::from_rgb(24, 24, 24);

                        let resp = egui::Frame::none()
                            .fill(bg)
                            .stroke(Stroke::new(1.0, Color32::from_rgb(40, 40, 40)))
                            .rounding(egui::Rounding::same(6.0))
                            .inner_margin(egui::Margin {
                                left: left_pad,
                                right: 10.0,
                                top: 8.0,
                                bottom: 8.0,
                            })
                            .show(ui, |ui| {
                                for (li, line) in lines.iter().enumerate() {
                                    // White text for quote content
                                    ui.style_mut().visuals.override_text_color =
                                        Some(Color32::WHITE);
                                    self.render_inline_spans(ui, line);
                                    ui.style_mut().visuals.override_text_color = None;
                                    if li + 1 < lines.len() {
                                        ui.add_space(3.0);
                                    }
                                }
                            });

                        // Draw vertical orange quote bars on the left of the frame
                        let rect = resp.response.rect;
                        let top = rect.top() + 6.0;
                        let bottom = rect.bottom() - 6.0;
                        let bar_color = Color32::from_rgb(255, 103, 25); // Substack-like orange
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
                    MarkdownElement::CodeBlock { language, text } => {
                        self.render_code_block(ui, language.as_deref(), text);
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
                        self.render_table(ui, headers, rows, alignments, element_idx);
                    }
                }
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

    /// Render a single inline span    /// Render a single inline span
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
                let is_dark = ui.visuals().dark_mode;
                let (bg, fg) = if is_dark {
                    (
                        Color32::from_rgb(30, 30, 30),
                        Color32::from_rgb(180, 255, 180),
                    )
                } else {
                    // Light theme: white background with readable code color
                    (Color32::WHITE, Color32::from_rgb(60, 80, 150))
                };
                let response = ui.add(
                    egui::Label::new(
                        RichText::new(code.clone())
                            .size(self.font_sizes.code)
                            .family(egui::FontFamily::Monospace)
                            .background_color(bg)
                            .color(fg),
                    )
                    .wrap(false),
                );

                // Add context menu for code
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
                let group = ui.horizontal_wrapped(|ui| {
                    // Render link-like styled text with emoji expansion
                    let color = if Self::is_external_url(url) {
                        // Slightly different color to indicate external website links
                        Color32::from_rgb(120, 190, 255)
                    } else {
                        Color32::LIGHT_BLUE
                    };
                    let style = InlineStyle {
                        strong: is_strong,
                        color: Some(color),
                        ..Default::default()
                    };
                    self.render_text_with_emojis(ui, &fixed_text, size, style);
                });
                // Use a unique id per link occurrence to avoid collisions when the same URL appears multiple times
                let mut counter = self.link_counter.borrow_mut();
                let id = egui::Id::new(format!("link:{}:{}", *counter, url));
                *counter += 1;
                let r = ui.interact(group.response.rect, id, egui::Sense::click());
                if render_action_triggered(r.hovered(), "link_hover") {
                    ui.output_mut(|o| o.cursor_icon = egui::CursorIcon::PointingHand);
                }
                if render_action_triggered(r.clicked(), "link_click") {
                    self.trigger_link(url);
                }

                // Add context menu for links
                r.context_menu(|ui| {
                    self.render_link_context_menu(ui, text, url);
                });
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
                            ui.label(
                                RichText::new(t.clone())
                                    .size(self.font_sizes.body - 2.0)
                                    .color(Color32::from_rgb(140, 140, 140)),
                            );
                        }
                    }
                    ui.add_space(6.0);
                } else {
                    // Placeholder with alt and error info
                    egui::Frame::none()
                        .fill(Color32::from_rgb(30, 30, 30))
                        .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
                        .inner_margin(8.0)
                        .show(ui, |ui| {
                            let msg = if src.starts_with("http://") || src.starts_with("https://") {
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
                if range.is_empty() {
                    continue;
                }
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

    fn render_plain_segment(&self, ui: &mut egui::Ui, text: &str, size: f32, style: InlineStyle) {
        if text.is_empty() {
            return;
        }
        let mut rich = RichText::new(text).size(size);
        if style.strong {
            rich = rich.strong();
        }
        if style.italics {
            rich = rich.italics();
        }
        if style.strike {
            rich = rich.strikethrough();
        }
        if let Some(color) = style.color {
            rich = rich.color(color);
        }
        let response = ui.add(egui::Label::new(rich).wrap(true));

        // Add context menu for text
        // Note: Due to egui limitations, selection is cleared on right-click
        // As a workaround, we provide "Copy Text" for the segment
        response.context_menu(|ui| {
            self.render_text_context_menu(ui, text);
        });
    }

    fn render_highlighted_segment(
        &self,
        ui: &mut egui::Ui,
        text: &str,
        size: f32,
        style: InlineStyle,
    ) {
        if text.is_empty() {
            return;
        }
        let visuals = ui.visuals();
        let bg = visuals.selection.bg_fill;
        let mut text_color = style.color;
        let fallback_color = visuals.selection.stroke.color;
        if text_color.is_none() {
            text_color = Some(fallback_color);
        }

        let mut rich = RichText::new(text).size(size).background_color(bg);
        if style.strong {
            rich = rich.strong();
        }
        if style.italics {
            rich = rich.italics();
        }
        if style.strike {
            rich = rich.strikethrough();
        }
        if let Some(color) = text_color {
            rich = rich.color(color);
        }
        let response = ui.add(egui::Label::new(rich).wrap(true));

        // Add context menu for highlighted text
        response.context_menu(|ui| {
            self.render_text_context_menu(ui, text);
        });
    }

    fn render_text_context_menu(&self, ui: &mut egui::Ui, text: &str) {
        if ui.button("Copy Text").clicked() {
            self.copy_text_and_close(ui, text);
        }
        ui.label("Tip: Use Ctrl+C to copy selected text");
    }

    fn render_inline_code_context_menu(&self, ui: &mut egui::Ui, code: &str) {
        if ui.button("Copy Code").clicked() {
            self.copy_text_and_close(ui, code);
        }
    }

    fn render_code_block_context_menu(
        &self,
        ui: &mut egui::Ui,
        code: &str,
        language: Option<&str>,
    ) {
        if ui.button("Copy Code").clicked() {
            self.copy_text_and_close(ui, code);
        }
        if let Some(lang) = language {
            if ui.button(format!("Copy as {}", lang)).clicked() {
                self.copy_text_and_close(ui, &format!("```{}\n{}\n```", lang, code));
            }
        }
    }

    fn render_link_context_menu(&self, ui: &mut egui::Ui, text: &str, url: &str) {
        if ui.button("Open Link").clicked() {
            self.trigger_link(url);
            ui.close_menu();
        }
        ui.separator();
        if ui.button("Copy Link Text").clicked() {
            self.copy_text_and_close(ui, text);
        }
        if ui.button("Copy Link URL").clicked() {
            self.copy_text_and_close(ui, url);
        }
    }

    fn render_cell_context_menu(&self, ui: &mut egui::Ui, text: &str) {
        if ui.button("Copy Cell Text").clicked() {
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
        // If the grapheme explicitly requests emoji presentation (contains VS16),
        // treat it as an emoji even if we don't have a sprite; fall back to a
        // generated placeholder via emoji_assets to avoid stray tofu.
        if g.chars().any(|c| c == '\u{FE0F}') {
            return Some(stripped); // prefer stripped as the texture key
        }
        None
    }

    fn get_or_make_emoji_texture(&self, ui: &mut egui::Ui, emoji: &str) -> egui::TextureHandle {
        if let Some(tex) = self.emoji_textures.borrow().get(emoji) {
            return tex.clone();
        }

        let img = if let Some(bytes) = emoji_catalog::image_bytes_for(emoji) {
            match image::load_from_memory(bytes) {
                Ok(dyn_img) => {
                    let rgba = dyn_img.to_rgba8();
                    let (w, h) = rgba.dimensions();
                    let pixels = rgba.into_vec();
                    egui::ColorImage::from_rgba_unmultiplied([w as usize, h as usize], &pixels)
                }
                Err(_) => emoji_assets::make_image(emoji, 64)
                    .unwrap_or_else(|| self.generate_emoji_image(emoji, 64)),
            }
        } else {
            emoji_assets::make_image(emoji, 64)
                .unwrap_or_else(|| self.generate_emoji_image(emoji, 64))
        };
        let handle = ui.ctx().load_texture(
            format!("emoji:{}", emoji),
            img,
            egui::TextureOptions::LINEAR,
        );
        self.emoji_textures
            .borrow_mut()
            .insert(emoji.to_string(), handle.clone());
        handle
    }

    // Rough width measurement for inline spans without wrapping
    fn measure_inline_spans(&self, ui: &egui::Ui, spans: &[InlineSpan]) -> f32 {
        let mut width = 0.0f32;
        ui.fonts(|fonts| {
            for span in spans {
                match span {
                    InlineSpan::Text(t)
                    | InlineSpan::Strong(t)
                    | InlineSpan::Emphasis(t)
                    | InlineSpan::Strikethrough(t)
                    | InlineSpan::Link { text: t, .. } => {
                        let body = egui::FontId::proportional(self.font_sizes.body);
                        // Expand shortcodes to better estimate width and count emojis
                        let expanded = Self::expand_shortcodes(t);
                        let galley = fonts.layout_no_wrap(expanded.clone(), body, Color32::WHITE);
                        width += galley.size().x;
                        // Add a small extra width for emoji images (drawn larger than text)
                        let emoji_extra = expanded
                            .chars()
                            .filter(|c| Self::is_known_emoji(*c))
                            .count() as f32
                            * (self.font_sizes.body * 0.2);
                        width += emoji_extra;
                    }
                    InlineSpan::Code(code) => {
                        let mono = egui::FontId::monospace(self.font_sizes.code);
                        let galley = fonts.layout_no_wrap(code.to_string(), mono, Color32::WHITE);
                        width += galley.size().x + 6.0; // small padding for code background
                    }
                    InlineSpan::Image { src, .. } => {
                        // Use cached texture size if available; otherwise a conservative thumbnail.
                        let cap = (ui.available_width() * 0.6).max(48.0);
                        let cached = self
                            .image_textures
                            .borrow()
                            .get(src)
                            .map(|entry| entry.size[0] as f32 * self.ui_scale());
                        let approx = cached.unwrap_or(self.font_sizes.body * 12.0);
                        width += approx.min(cap);
                    }
                }
                width += 4.0; // spacing between spans
            }
        });
        width
    }

    fn is_known_emoji(c: char) -> bool {
        matches!(
            c,
            '\u{1f389}' // 🎉
                | '\u{2705}' // ✅
                | '\u{1f680}' // 🚀
                | '\u{1f642}' // 🙂
                | '\u{1f600}' // 😀
                | '\u{1f609}' // 😉
                | '\u{2b50}'  // ⭐
                | '\u{1f525}' // 🔥
                | '\u{1f44d}' // 👍
                | '\u{1f44e}' // 👎
                | '\u{1f4a1}' // 💡
                | '\u{2753}'  // ❓
                | '\u{2757}'  // ❗
                | '\u{1f4dd}' // 📝
                | '\u{1f9e0}' // 🧠
                | '\u{1f9ea}' // 🧪
                | '\u{1f4e6}' // 📦
                | '\u{1f527}' // 🔧
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
            let ch = s[i..].chars().next().unwrap();
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
                    for _ in 0..char_count {
                        if let Some(nc) = chars.next() {
                            buf.push(nc);
                        }
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

    /// Render a list with proper inline formatting, including simple nested lines
    /// Render a list with proper inline formatting, including simple nested lines
    fn render_list(&self, ui: &mut egui::Ui, ordered: bool, items: &[Vec<InlineSpan>]) {
        if items.is_empty() {
            return;
        }

        ui.add_space(4.0);

        for (index, spans) in items.iter().enumerate() {
            // Split into lines on embedded '\n'
            let mut lines: Vec<Vec<InlineSpan>> = vec![Vec::new()];
            for s in spans.clone() {
                match s {
                    InlineSpan::Text(t) if t.contains('\n') => {
                        let parts: Vec<&str> = t.split('\n').collect();
                        for (pi, part) in parts.iter().enumerate() {
                            if !part.is_empty() {
                                lines
                                    .last_mut()
                                    .unwrap()
                                    .push(InlineSpan::Text(part.to_string()));
                            }
                            if pi < parts.len() - 1 {
                                lines.push(Vec::new());
                            }
                        }
                    }
                    other => lines.last_mut().unwrap().push(other),
                }
            }

            for (li, mut line) in lines.into_iter().enumerate() {
                ui.horizontal_wrapped(|ui| {
                    if li == 0 {
                        let marker = if ordered {
                            format!("{}.", index + 1)
                        } else {
                            "-".to_string()
                        };
                        let marker_color = if ui.visuals().dark_mode {
                            Color32::from_rgb(180, 180, 180)
                        } else {
                            Color32::BLACK
                        };
                        ui.label(
                            RichText::new(format!("{} ", marker))
                                .size(self.font_sizes.body)
                                .color(marker_color),
                        );
                    } else {
                        // Determine additional indentation from leading spaces in this line
                        let mut leading_spaces = 0usize;
                        if let Some(InlineSpan::Text(t0)) = line.first() {
                            leading_spaces = t0.chars().take_while(|c| *c == ' ').count();
                        }
                        if leading_spaces > 0 {
                            if let Some(InlineSpan::Text(t0)) = line.get_mut(0) {
                                let trimmed = t0.trim_start_matches(' ').to_string();
                                *t0 = trimmed;
                            }
                        }
                        let indent_px = 18.0 + (leading_spaces as f32) * 6.0;
                        ui.add_space(indent_px);
                    }
                    ui.spacing_mut().item_spacing.x = 0.0;
                    for span in &line {
                        self.render_inline_span(ui, span, None, None);
                    }
                });
            }
        }

        ui.add_space(4.0);
    }

    /// Render a code block with syntax highlighting
    fn render_code_block(&self, ui: &mut egui::Ui, language: Option<&str>, code: &str) {
        ui.add_space(8.0);

        // Special handling for Mermaid diagrams
        if let Some(lang) = language {
            if lang.eq_ignore_ascii_case("mermaid") && self.render_mermaid_block(ui, code) {
                ui.add_space(8.0);
                return;
            }
        }

        let frame_response = egui::Frame::none()
            .fill(Color32::from_rgb(25, 25, 25))
            .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
            .inner_margin(8.0)
            .show(ui, |ui| {
                ui.with_layout(egui::Layout::top_down(egui::Align::LEFT), |ui| {
                    if let Some(lang) = language {
                        ui.label(
                            RichText::new(lang)
                                .size(self.font_sizes.code - 1.0)
                                .color(Color32::from_rgb(150, 150, 150))
                                .family(egui::FontFamily::Monospace),
                        );
                        ui.add_space(2.0);
                    }

                    // Try syntax highlighting
                    if let Some(lang) = language {
                        if let Some(syntax) = self
                            .find_syntax_for_language(lang)
                            .or_else(|| self.syntax_set.find_syntax_by_first_line(code))
                        {
                            let theme = &self.theme_set.themes["base16-ocean.dark"];
                            let mut h = HighlightLines::new(syntax, theme);

                            for line in LinesWithEndings::from(code) {
                                let ranges =
                                    h.highlight_line(line, &self.syntax_set).unwrap_or_default();

                                ui.horizontal_wrapped(|ui| {
                                    // Remove spacing between tokens to avoid visual gaps
                                    ui.spacing_mut().item_spacing.x = 0.0;
                                    for (style, text) in ranges {
                                        // Drop newline characters completely; they're handled by the outer line loop
                                        let cleaned = text.replace(['\n', '\r'], "");

                                        if cleaned.is_empty() {
                                            continue;
                                        }

                                        // Check if this token is pure whitespace (spaces or tabs only)
                                        if cleaned.chars().all(|c| c == ' ' || c == '\t') {
                                            // Render whitespace as transparent to preserve layout without visual gaps
                                            ui.label(
                                                RichText::new(cleaned)
                                                    .size(self.font_sizes.code)
                                                    .color(Color32::TRANSPARENT)
                                                    .family(egui::FontFamily::Monospace),
                                            );
                                        } else {
                                            // For non-whitespace, use normal highlighting but replace spaces with transparent ones
                                            let color = Color32::from_rgb(
                                                style.foreground.r,
                                                style.foreground.g,
                                                style.foreground.b,
                                            );

                                            // Split by spaces and handle separately
                                            let parts: Vec<&str> = cleaned.split(' ').collect();
                                            for (i, part) in parts.iter().enumerate() {
                                                if !part.is_empty() {
                                                    let mut rich_text = RichText::new(*part)
                                                        .size(self.font_sizes.code)
                                                        .color(color)
                                                        .family(egui::FontFamily::Monospace);

                                                    if style.font_style.contains(
                                                        syntect::highlighting::FontStyle::BOLD,
                                                    ) {
                                                        rich_text = rich_text.strong();
                                                    }
                                                    if style.font_style.contains(
                                                        syntect::highlighting::FontStyle::ITALIC,
                                                    ) {
                                                        rich_text = rich_text.italics();
                                                    }

                                                    ui.label(rich_text);
                                                }

                                                // Add transparent space between parts (except after last part)
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
                            return; // Early return if highlighting succeeded
                        }
                    }

                    // Fallback: render as plain text
                    ui.label(
                        RichText::new(code)
                            .size(self.font_sizes.code)
                            .color(Color32::from_rgb(220, 220, 220))
                            .family(egui::FontFamily::Monospace),
                    );
                });
            });

        // Add context menu for code blocks
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

    fn compute_table_id(
        &self,
        headers: &[Vec<InlineSpan>],
        rows: &[Vec<Vec<InlineSpan>>],
        element_index: usize,
    ) -> u64 {
        let mut hasher = DefaultHasher::new();
        if let Some(base) = self.base_dir.borrow().as_ref() {
            base.hash(&mut hasher);
        }
        hasher.write_usize(element_index);
        hasher.write_u64(self.compute_table_content_hash(headers, rows));
        hasher.finish()
    }

    fn compute_table_content_hash(
        &self,
        headers: &[Vec<InlineSpan>],
        rows: &[Vec<Vec<InlineSpan>>],
    ) -> u64 {
        let mut hasher = DefaultHasher::new();
        hasher.write_usize(headers.len());
        for header in headers {
            hasher.write_u64(Self::hash_inline_spans(header));
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
    ) -> Vec<ColumnStat> {
        let content_hash = self.compute_table_content_hash(headers, rows);
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
        element_index: usize,
    ) {
        if headers.is_empty() {
            return;
        }

        if self.table_wrap_overhaul_enabled {
            self.render_table_tablebuilder(ui, headers, rows, element_index);
            ui.add_space(8.0);
            return;
        }

        ui.add_space(8.0);

        const MIN_COL_MULTIPLIER: f32 = 6.0;
        const HARD_MIN_COL_WIDTH: f32 = 48.0;
        const MAX_WRAP_MULTIPLIER: f32 = 30.0;

        let min_floor = (self.font_sizes.body * MIN_COL_MULTIPLIER).max(HARD_MIN_COL_WIDTH);
        let wrap_cap = (self.font_sizes.body * MAX_WRAP_MULTIPLIER).min(640.0);

        // 1) Establish baseline widths from headers so small columns don't collapse
        let min_widths: Vec<f32> = headers
            .iter()
            .map(|h| self.measure_inline_spans(ui, h).max(min_floor))
            .collect();
        if min_widths.is_empty() {
            return;
        }

        // 2) Allow body cells to request more width (capped so single verbose cells don't dominate)
        let mut desired_widths = min_widths.clone();
        for row in rows {
            for (ci, cell) in row.iter().enumerate() {
                if ci >= desired_widths.len() {
                    break;
                }
                let measured = self.measure_inline_spans(ui, cell).max(min_floor);
                desired_widths[ci] = desired_widths[ci].max(measured.min(wrap_cap));
            }
        }

        let available = ui.available_width().max(100.0);
        let widths = Self::resolve_table_widths(available, &min_widths, &desired_widths);

        self.render_table_legacy(ui, headers, rows, &widths);
        ui.add_space(8.0);
    }

    fn render_table_legacy(
        &self,
        ui: &mut egui::Ui,
        headers: &[Vec<InlineSpan>],
        rows: &[Vec<Vec<InlineSpan>>],
        widths: &[f32],
    ) {
        egui::Frame::none()
            .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
            .show(ui, |ui| {
                egui::Grid::new("md_table").striped(true).show(ui, |ui| {
                    for (ci, h) in headers.iter().enumerate() {
                        let w = widths.get(ci).copied().unwrap_or(120.0);
                        ui.push_id(("header", ci), |ui| {
                            ui.allocate_ui_with_layout(
                                Vec2::new(w, 0.0),
                                egui::Layout::top_down(egui::Align::LEFT),
                                |ui| {
                                    ui.set_width(w);
                                    ui.set_max_width(w);
                                    self.render_table_cell_spans(ui, h, w, true);
                                },
                            );
                        });
                    }
                    ui.end_row();

                    for (ri, row) in rows.iter().enumerate() {
                        for (ci, cell) in row.iter().enumerate() {
                            if ci < headers.len() {
                                let w = widths.get(ci).copied().unwrap_or(120.0);
                                ui.push_id(("cell", ri, ci), |ui| {
                                    ui.allocate_ui_with_layout(
                                        Vec2::new(w, 0.0),
                                        egui::Layout::top_down(egui::Align::LEFT),
                                        |ui| {
                                            ui.set_width(w);
                                            ui.set_max_width(w);
                                            self.render_table_cell_spans(ui, cell, w, false);
                                        },
                                    );
                                });
                            }
                        }
                        ui.end_row();
                    }
                });
            });
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
        element_index: usize,
    ) {
        let table_id = self.compute_table_id(headers, rows, element_index);
        let column_stats = self.column_stats_for_table(table_id, headers, rows);
        let ctx =
            TableColumnContext::new(headers, rows, &column_stats, self.font_sizes.body, table_id);
        let mut column_specs = derive_column_specs(&ctx);
        let row_max = rows.iter().map(|r| r.len()).max().unwrap_or(0);
        let target_cols = column_specs.len().max(row_max).max(1);
        while column_specs.len() < target_cols {
            column_specs.push(ColumnSpec::new(
                format!("Column {}", column_specs.len() + 1),
                crate::table_support::ColumnPolicy::Remainder { clip: false },
                None,
            ));
        }
        self.begin_table_pass(table_id, rows.len());
        self.apply_persisted_widths(table_id, &mut column_specs);

        let prev_spacing = ui.spacing().item_spacing;
        if prev_spacing.x < 6.0 {
            ui.spacing_mut().item_spacing.x = 6.0;
        }

        // Rough header height estimation using equally divided width; refines on next frame via cache.
        let mut header_height = self.row_height_fallback();
        if !column_specs.is_empty() {
            let approx_width = (ui.available_width() / column_specs.len() as f32)
                .max(self.font_sizes.body * 6.0)
                .max(48.0);
            let style = ui.style().clone();
            header_height = headers
                .iter()
                .enumerate()
                .map(|(ci, spans)| {
                    let build = self.cached_layout_job(&style, None, ci, spans, approx_width, true);
                    ui.fonts(|f| f.layout_job(build.job.clone()).size().y + 6.0)
                })
                .fold(header_height, |acc, h| acc.max(h));
            header_height = header_height.min(self.row_height_fallback() * 3.0);
        }

        let mut table = TableBuilder::new(ui).striped(true);
        for spec in &column_specs {
            table = table.column(spec.as_column());
        }

        let fallback_row_height = self.row_height_fallback();
        let row_heights: Vec<f32> = (0..rows.len())
            .map(|idx| self.row_height_hint(table_id, idx))
            .collect();

        // Use RefCell to allow capturing widths from body closure.
        // BUG FIX: Previously captured ui.min_rect().width() which is the cell *content* width.
        // Now we capture body.widths() which gives the actual *allocated* column widths.
        // See: https://docs.rs/egui_extras/latest/egui_extras/struct.TableBody.html#method.widths
        let column_widths: RefCell<Vec<f32>> = RefCell::new(vec![0.0f32; column_specs.len()]);
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
        let mut painter: Option<Painter> = None;
        let mut visuals: Option<Visuals> = None;
        let mut ctx_snapshot: Option<Context> = None;

        let mut height_growth = false;
        table
            .header(header_height, |mut header| {
                for (ci, _) in column_specs.iter().enumerate() {
                    header.col(|ui| {
                        let width = ui.available_width().max(1.0);
                        let spans = headers.get(ci).map(|v| v.as_slice()).unwrap_or(&[]);
                        let _ = self.render_overhauled_cell(ui, spans, width, true, None, ci);
                        // NOTE: Do NOT capture ui.min_rect().width() here - that's the content width,
                        // not the column width. Column widths are captured from body.widths() below.
                        // Extend header_rect (not body_rect) for accurate header bounds.
                        Self::extend_table_rect(&mut header_rect, ui.min_rect());
                        if header_clip_rect.is_none() {
                            header_clip_rect = Some(ui.clip_rect());
                            painter = Some(ui.painter().clone());
                            visuals = Some(ui.visuals().clone());
                            ctx_snapshot = Some(ui.ctx().clone());
                        }
                    });
                }
            })
            .body(|body| {
                // Capture the actual allocated column widths from the table layout system.
                // This MUST be done before heterogeneous_rows() consumes the body.
                *column_widths.borrow_mut() = body.widths().to_vec();

                // Capture body's layout rect for accurate table width calculation.
                // This is the allocated region, not the content bounds.
                *body_layout_rect.borrow_mut() = Some(body.max_rect());

                let row_height_hints = row_heights.clone();
                body.heterogeneous_rows(row_height_hints.into_iter(), |mut row| {
                    let idx = row.index();
                    let row_cells = rows.get(idx);
                    let mut row_height = fallback_row_height;
                    for (ci, _) in column_specs.iter().enumerate() {
                        let mut cell_height = fallback_row_height;
                        row.col(|ui| {
                            let width = ui.available_width().max(1.0);
                            let spans = row_cells
                                .and_then(|cells| cells.get(ci))
                                .map(|cell| cell.as_slice())
                                .unwrap_or(&[]);
                            cell_height =
                                self.render_overhauled_cell(ui, spans, width, false, Some(idx), ci);
                            // Extend body_rect (not header_rect) for accurate body bounds.
                            Self::extend_table_rect(&mut body_rect, ui.min_rect());
                            if body_clip_rect.borrow().is_none() {
                                *body_clip_rect.borrow_mut() = Some(ui.clip_rect());
                                // Also capture painter/visuals/ctx if not already captured from header
                                if painter.is_none() {
                                    painter = Some(ui.painter().clone());
                                    visuals = Some(ui.visuals().clone());
                                    ctx_snapshot = Some(ui.ctx().clone());
                                }
                            }
                        });
                        row_height = row_height.max(cell_height);
                        if cell_height > row_heights[idx] + 0.5 {
                            height_growth = true;
                        }
                    }
                    self.update_row_height(table_id, idx, row_height);
                    self.note_row_rendered(table_id);
                });
            });

        ui.spacing_mut().item_spacing = prev_spacing;

        // Extract column widths from RefCell for use in divider painting
        let widths = column_widths.into_inner();
        let layout_rect = body_layout_rect.into_inner();
        let body_clip = body_clip_rect.into_inner();

        // Combine header and body clip rects to ensure dividers respect scroll bounds.
        let clip_rect = match (header_clip_rect, body_clip) {
            (Some(h), Some(b)) => Some(h.union(b)),
            (Some(h), None) => Some(h),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        };

        // Calculate accurate table width from column widths.
        // This is more accurate than union of cell min_rects which may be smaller
        // than the allocated column space.
        let calculated_width: f32 = widths.iter().sum();

        // Combine header and body rects into the full table rect.
        // Use calculated width for accuracy when column widths are available.
        let table_rect = match (header_rect, body_rect, layout_rect) {
            // Best case: use header for top/left, body for bottom, calculated width for right
            (Some(h), Some(b), _) if calculated_width > 0.0 => {
                let left = h.left().min(b.left());
                let top = h.top();
                let bottom = b.bottom();
                let right = left + calculated_width;
                Some(egui::Rect::from_min_max(
                    egui::pos2(left, top),
                    egui::pos2(right, bottom),
                ))
            }
            // Fallback: use union of header and body
            (Some(h), Some(b), _) => Some(h.union(b)),
            // Header only with layout rect
            (Some(h), None, Some(layout)) if calculated_width > 0.0 => {
                let left = h.left().min(layout.left());
                let right = left + calculated_width;
                Some(egui::Rect::from_min_max(
                    egui::pos2(left, h.top()),
                    egui::pos2(right, h.bottom()),
                ))
            }
            (Some(h), None, _) => Some(h),
            (None, Some(b), _) => Some(b),
            (None, None, Some(layout)) => Some(layout),
            (None, None, None) => None,
        };

        if let (Some(rect), Some(clip_rect), Some(painter), Some(visuals), Some(ctx)) =
            (table_rect, clip_rect, painter, visuals, ctx_snapshot)
        {
            if column_specs.len() == widths.len() && widths.iter().any(|w| *w > 0.0) {
                let frame_id = ctx.frame_nr();
                let change = self.record_resolved_widths(table_id, frame_id, &widths);
                self.persist_resizable_widths(table_id, &column_specs, &widths);
                self.handle_width_change(&ctx, table_id, change);
                self.paint_table_dividers(
                    &painter,
                    &visuals,
                    rect,
                    clip_rect,
                    &widths,
                    header_height,
                );
                if height_growth {
                    ctx.request_repaint();
                }
            }
        }
    }

    fn row_height_fallback(&self) -> f32 {
        self.font_sizes.body * 1.6
    }

    fn row_height_hint(&self, table_id: u64, idx: usize) -> f32 {
        let fallback = self.row_height_fallback();
        self.table_metrics
            .borrow()
            .entry(table_id)
            .and_then(|entry| entry.row(idx))
            .map(|m| {
                if m.max_height > 0.0 {
                    m.max_height
                } else {
                    fallback
                }
            })
            .unwrap_or(fallback)
    }

    fn update_row_height(&self, table_id: u64, idx: usize, height: f32) {
        let clamped = height.max(self.row_height_fallback());
        let mut metrics = self.table_metrics.borrow_mut();
        let entry = metrics.entry_mut(table_id);
        let row = entry.ensure_row(idx);
        row.max_height = clamped;
        row.dirty = false;
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
    fn paint_table_dividers(
        &self,
        painter: &Painter,
        visuals: &Visuals,
        rect: egui::Rect,
        clip_rect: egui::Rect,
        widths: &[f32],
        header_height: f32,
    ) {
        if widths.len() <= 1 {
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
        let painter = painter.with_clip_rect(clip_rect);

        // Draw vertical dividers between columns
        let mut x = rect.left();
        for width in widths.iter().take(widths.len().saturating_sub(1)) {
            x += *width;
            let x_pos = (x.round() + 0.5).clamp(rect.left(), rect.right());
            painter.vline(x_pos, rect.y_range(), separator_stroke);
        }

        // Draw horizontal separator below header row
        if header_height > 0.0 {
            let header_y = rect.top() + header_height;
            if header_y > rect.top() && header_y < rect.bottom() {
                painter.hline(rect.x_range(), header_y.round() + 0.5, separator_stroke);
            }
        }

        // Draw outer border
        painter.rect_stroke(rect, 0.0, border_stroke);
    }

    fn render_overhauled_cell(
        &self,
        ui: &mut egui::Ui,
        spans: &[InlineSpan],
        width: f32,
        is_header: bool,
        row_idx: Option<usize>,
        col_idx: usize,
    ) -> f32 {
        let fallback_height = self.row_height_fallback();
        let fragments = self.cell_fragments(spans);
        let inner = ui.allocate_ui_with_layout(
            Vec2::new(width, 0.0),
            egui::Layout::top_down(egui::Align::LEFT),
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
        ui.painter_at(rect)
            .galley(rect.left_top(), galley.clone(), text_color);
        if galley.rows.len() > 1 {
            response = response.on_hover_text(build.plain_text.clone());
        }
        response.context_menu(|ui| {
            self.render_cell_context_menu(ui, &build.plain_text);
        });

        if let Some(link) = self.link_at_pointer(&response, &galley, &build) {
            ui.output_mut(|o| o.cursor_icon = egui::CursorIcon::PointingHand);
            response = response.on_hover_text(link.url.clone());
            if response.clicked() {
                self.trigger_link(&link.url);
            }
        }

        response
    }

    fn cached_layout_job(
        &self,
        style: &egui::Style,
        row_idx: Option<usize>,
        col_idx: usize,
        spans: &[InlineSpan],
        width: f32,
        is_header: bool,
    ) -> LayoutJobBuild {
        if !self.table_wrap_overhaul_enabled {
            return self.build_layout_job(style, spans, width, is_header);
        }
        let highlight_hash = self
            .highlight_phrase
            .borrow()
            .as_ref()
            .map(|s| Self::hash_str(s))
            .unwrap_or(0);
        let content_hash = Self::hash_inline_spans(spans);
        let key = CellLayoutKey {
            row: row_idx,
            col: col_idx,
            width: width.round() as u32,
            strong: is_header,
            highlight_hash,
            content_hash,
        };
        if let Some(build) = self.table_layout_cache.borrow_mut().get(&key) {
            return build;
        }
        let build = self.build_layout_job(style, spans, width, is_header);
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
    ) -> Option<&'a LinkRange> {
        let pointer = response.hover_pos()?;
        let local = pointer - response.rect.left_top();
        let cursor = galley.cursor_from_pos(local);
        let idx = cursor.ccursor.index;
        build
            .link_ranges
            .iter()
            .find(|range| range.char_range.contains(&idx))
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

    /// Render table cell spans with proper text wrapping within the given width
    fn render_table_cell_spans(
        &self,
        ui: &mut egui::Ui,
        spans: &[InlineSpan],
        max_width: f32,
        is_header: bool,
    ) {
        // Render content with wrapping, forcing exact width to ensure wrapping occurs
        ui.vertical(|ui| {
            ui.set_width(max_width); // Force exact width
            ui.set_max_width(max_width);
            ui.spacing_mut().item_spacing.y = 0.0;

            // Render all spans in a wrapped horizontal layout
            ui.horizontal_wrapped(|ui| {
                ui.spacing_mut().item_spacing.x = 0.0;
                ui.set_width(max_width);
                for span in spans {
                    self.render_inline_span(ui, span, None, Some(is_header));
                }
            });
        });
    }

    fn resolve_table_widths(available: f32, mins: &[f32], desired: &[f32]) -> Vec<f32> {
        debug_assert_eq!(mins.len(), desired.len());
        if mins.is_empty() {
            return Vec::new();
        }
        let clamped_available = available.max(1.0);
        let desired_total: f32 = desired.iter().sum();
        let min_total: f32 = mins.iter().sum();

        let mut widths = if min_total >= clamped_available {
            if min_total <= f32::EPSILON {
                vec![clamped_available / mins.len() as f32; mins.len()]
            } else {
                mins.iter()
                    .map(|m| m * (clamped_available / min_total))
                    .collect()
            }
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
            if total_slack <= f32::EPSILON {
                let bonus = extra / widths.len() as f32;
                for w in &mut widths {
                    *w += bonus;
                }
            } else {
                for (w, s) in widths.iter_mut().zip(slack.iter()) {
                    *w += extra * (*s / total_slack);
                }
            }
            widths
        };

        let sum: f32 = widths.iter().sum();
        if sum > clamped_available + 0.5 {
            let ratio = clamped_available / sum;
            for w in &mut widths {
                *w *= ratio;
            }
        }
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
        self.mermaid.has_pending()
    }

    /// Set or clear the highlight phrase (case-insensitive)
    pub fn set_highlight_phrase(&self, phrase: Option<&str>) {
        if let Some(p) = phrase {
            self.highlight_phrase
                .borrow_mut()
                .replace(p.case_fold().nfkc().collect());
        } else {
            self.highlight_phrase.borrow_mut().take();
        }
        self.clear_table_layout_cache();
    }

    pub fn set_table_wrap_overhaul_enabled(&mut self, enabled: bool) {
        self.table_wrap_overhaul_enabled = enabled;
        self.clear_table_layout_cache();
    }

    pub fn table_wrap_overhaul_enabled(&self) -> bool {
        self.table_wrap_overhaul_enabled
    }

    pub fn clear_table_layout_cache(&self) {
        self.table_layout_cache.borrow_mut().clear();
        self.table_metrics.borrow_mut().clear();
        self.column_stats_cache.borrow_mut().clear();
    }

    pub fn table_layout_cache_stats(&self) -> (u64, u64) {
        self.table_layout_cache.borrow().stats()
    }

    pub fn table_render_stats(&self) -> (usize, usize) {
        self.table_metrics.borrow().totals()
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
                    if !out.is_empty() {
                        out.push('\n');
                    }
                    out.push_str(&Self::spans_plain_text(item));
                }
                out
            }
            MarkdownElement::Quote { lines, .. } => {
                let mut out = String::new();
                for line in lines {
                    if !out.is_empty() {
                        out.push('\n');
                    }
                    out.push_str(&Self::spans_plain_text(line));
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
        if let Some(d) = dir {
            self.base_dir.borrow_mut().replace(d.to_path_buf());
        } else {
            self.base_dir.borrow_mut().take();
        }
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
        ui: &egui::Ui,
        resolved_src: &str,
    ) -> Option<(egui::TextureHandle, u32, u32)> {
        // Reject remote for now
        if resolved_src.starts_with("http://") || resolved_src.starts_with("https://") {
            return None;
        }

        let path = Path::new(resolved_src);

        if let Some(entry) = self.image_textures.borrow().get(resolved_src) {
            let stale = Self::image_source_stale(entry.modified, path);
            if !stale {
                return Some((entry.texture.clone(), entry.size[0], entry.size[1]));
            }
        }

        // Try embedded assets first
        if let Some(bytes) = Self::embedded_image_bytes(resolved_src) {
            if let Some((color_image, w, h)) = image_decode::bytes_to_color_image_guess(bytes, None)
            {
                let tex = ui.ctx().load_texture(
                    format!("img:{}", resolved_src),
                    color_image,
                    egui::TextureOptions::LINEAR,
                );
                self.store_image_texture(resolved_src, tex.clone(), [w, h], None);
                return Some((tex, w, h));
            }
        }

        if !path.exists() {
            return None;
        }

        let bytes = std::fs::read(path).ok()?;
        let (color_image, w, h) = if Self::is_svg_path(resolved_src) {
            match image_decode::svg_bytes_to_color_image(&bytes) {
                Some((ci, w, h)) => (ci, w, h),
                None => return None,
            }
        } else {
            let img = image::load_from_memory(&bytes).ok()?;
            let rgba = img.to_rgba8();
            let (w, h) = rgba.dimensions();
            let ci = egui::ColorImage::from_rgba_unmultiplied([w as usize, h as usize], &rgba);
            (ci, w, h)
        };
        let tex = ui.ctx().load_texture(
            format!("img:{}", resolved_src),
            color_image,
            egui::TextureOptions::LINEAR,
        );
        let modified = Self::disk_image_timestamp(path);
        self.store_image_texture(resolved_src, tex.clone(), [w, h], modified);
        Some((tex, w, h))
    }

    fn disk_image_timestamp(path: &Path) -> Option<SystemTime> {
        std::fs::metadata(path).ok()?.modified().ok()
    }

    fn image_source_stale(cached_modified: Option<SystemTime>, path: &Path) -> bool {
        if !path.exists() {
            return false;
        }
        let current = Self::disk_image_timestamp(path);
        match (cached_modified, current) {
            (Some(prev), Some(cur)) => prev != cur,
            (Some(_), None) => true,
            (None, Some(_)) => true,
            (None, None) => false,
        }
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

    fn is_svg_path(p: &str) -> bool {
        p.rsplit('.')
            .next()
            .map(|e| e.eq_ignore_ascii_case("svg"))
            .unwrap_or(false)
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

    /// Zoom in (increase font sizes)
    pub fn zoom_in(&mut self) {
        self.font_sizes.body = (self.font_sizes.body * 1.1).min(32.0);
        self.font_sizes.h1 = (self.font_sizes.h1 * 1.1).min(48.0);
        self.font_sizes.h2 = (self.font_sizes.h2 * 1.1).min(42.0);
        self.font_sizes.h3 = (self.font_sizes.h3 * 1.1).min(36.0);
        self.font_sizes.h4 = (self.font_sizes.h4 * 1.1).min(32.0);
        self.font_sizes.h5 = (self.font_sizes.h5 * 1.1).min(28.0);
        self.font_sizes.h6 = (self.font_sizes.h6 * 1.1).min(24.0);
        self.font_sizes.code = (self.font_sizes.code * 1.1).min(20.0);
        self.clear_table_layout_cache();
    }

    /// Zoom out (decrease font sizes)
    pub fn zoom_out(&mut self) {
        self.font_sizes.body = (self.font_sizes.body * 0.9).max(8.0);
        self.font_sizes.h1 = (self.font_sizes.h1 * 0.9).max(16.0);
        self.font_sizes.h2 = (self.font_sizes.h2 * 0.9).max(14.0);
        self.font_sizes.h3 = (self.font_sizes.h3 * 0.9).max(12.0);
        self.font_sizes.h4 = (self.font_sizes.h4 * 0.9).max(11.0);
        self.font_sizes.h5 = (self.font_sizes.h5 * 0.9).max(10.0);
        self.font_sizes.h6 = (self.font_sizes.h6 * 0.9).max(9.0);
        self.font_sizes.code = (self.font_sizes.code * 0.9).max(8.0);
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
        let default = FontSizes::default();
        self.font_sizes.body = (default.body * scale).clamp(8.0, 32.0);
        self.font_sizes.h1 = (default.h1 * scale).clamp(16.0, 48.0);
        self.font_sizes.h2 = (default.h2 * scale).clamp(14.0, 42.0);
        self.font_sizes.h3 = (default.h3 * scale).clamp(12.0, 36.0);
        self.font_sizes.h4 = (default.h4 * scale).clamp(11.0, 32.0);
        self.font_sizes.h5 = (default.h5 * scale).clamp(10.0, 28.0);
        self.font_sizes.h6 = (default.h6 * scale).clamp(9.0, 24.0);
        self.font_sizes.code = (default.code * scale).clamp(8.0, 20.0);
        self.clear_table_layout_cache();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SAMPLE_FILES;
    use image::codecs::png::PngEncoder;
    use image::ColorType;
    use image::ImageEncoder;
    use tempfile::tempdir;

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
        let mut input = egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(320.0, 240.0),
            )),
            ..Default::default()
        };
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

    #[test]
    fn test_markdown_renderer_creation() {
        let renderer = MarkdownRenderer::new();
        assert_eq!(renderer.font_sizes.body, 14.0);
        assert_eq!(renderer.font_sizes.h1, 28.0);
    }

    #[test]
    fn test_superscript_expansion_basic() {
        let s = "5^th^ and m^2^";
        let out = MarkdownRenderer::expand_superscripts(s);
        assert!(out.contains("5ᵗʰ"));
        assert!(out.contains("m²"));
    }

    #[test]
    fn test_superscript_expansion_single_caret() {
        // Test that single carets (not paired) are left as-is
        let s = "2^32 = 4,294,967,296";
        let out = MarkdownRenderer::expand_superscripts(s);
        assert_eq!(
            out, "2^32 = 4,294,967,296",
            "Single caret should be preserved"
        );

        // Test the problematic line from the bug report
        let problematic = "A 32-bit address bus would provide 2^32 = 4,294,967,296 bytes (4 GB) of addressable memory.";
        let fixed = MarkdownRenderer::expand_superscripts(problematic);
        assert_eq!(
            fixed, problematic,
            "Text should remain unchanged when no paired carets exist"
        );

        // Test mixed cases
        let mixed = "Use 2^32 for math and 5^th^ for ordinal";
        let result = MarkdownRenderer::expand_superscripts(mixed);
        assert!(result.contains("2^32"), "Single caret should be preserved");
        assert!(result.contains("5ᵗʰ"), "Paired carets should be converted");
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
        let specs = vec![ColumnSpec::new(
            "A",
            ColumnPolicy::Resizable {
                min: 20.0,
                preferred: 100.0,
                clip: false,
            },
            None,
        )];
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
    fn test_tight_list_inline_code_and_styles() {
        let renderer = MarkdownRenderer::new();
        let md = "- Use `code` and **bold** and *italic* and ~~strike~~\\n";
        let parsed = renderer.parse(md).expect("parse ok");
        assert_eq!(parsed.len(), 1);
        match &parsed[0] {
            MarkdownElement::List { ordered, items } => {
                assert!(!ordered);
                assert_eq!(items.len(), 1);
                let spans = &items[0];
                assert!(spans
                    .iter()
                    .any(|s| matches!(s, InlineSpan::Code(c) if c == "code")));
                assert!(spans
                    .iter()
                    .any(|s| matches!(s, InlineSpan::Strong(t) if t.contains("bold"))));
                assert!(spans
                    .iter()
                    .any(|s| matches!(s, InlineSpan::Emphasis(t) if t.contains("italic"))));
                assert!(spans
                    .iter()
                    .any(|s| matches!(s, InlineSpan::Strikethrough(t) if t.contains("strike"))));
            }
            other => panic!("Expected List, got {:?}", other),
        }
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
    }

    #[test]
    fn test_inline_code_preserves_whitespace() {
        let renderer = MarkdownRenderer::new();
        let md = "Start `code` end";
        let parsed = renderer.parse(md).expect("parse ok");
        match &parsed[0] {
            MarkdownElement::Paragraph(spans) => {
                let code_span = spans.iter().find_map(|span| match span {
                    InlineSpan::Code(t) => Some(t),
                    _ => None,
                });
                assert_eq!(code_span, Some(&"code".to_string()));
            }
            other => panic!("Expected Paragraph, got {:?}", other),
        }
    }

    #[test]
    fn test_footnote_markers_render_as_visible_text() {
        let renderer = MarkdownRenderer::new();
        let md = "Paragraph with footnote[^1].\n\n[^1]: footnote body.";
        let parsed = renderer.parse(md).expect("parse ok");
        assert_eq!(parsed.len(), 2);
        let first_plain = MarkdownRenderer::element_plain_text(&parsed[0]);
        assert!(
            first_plain.contains("footnote[^1]"),
            "footnote marker should remain visible, got {first_plain}"
        );
        let second_plain = MarkdownRenderer::element_plain_text(&parsed[1]);
        assert!(
            second_plain.contains("[^1]: footnote body."),
            "footnote definition should remain visible, got {second_plain}"
        );
    }

    #[test]
    fn test_parse_headers_assign_ids_and_dedupe() {
        let renderer = MarkdownRenderer::new();
        let md = "# Getting Started\n\n## Getting Started\n\n### API & Usage\n\n## API & Usage\n";
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
            assert!(ids.iter().any(|id| id == expected), "missing id {expected}");
        }
    }

    #[test]
    fn test_inline_image_parsing() {
        let renderer = MarkdownRenderer::new();
        let md = "Here is an image: ![Alt text](images/pic.webp \"Title\") end.";
        let parsed = renderer.parse(md).expect("parse ok");
        assert_eq!(parsed.len(), 1);
        match &parsed[0] {
            MarkdownElement::Paragraph(spans) => {
                let img = spans.iter().find(|s| matches!(s, InlineSpan::Image { .. }));
                assert!(img.is_some(), "image inline span present");
                if let InlineSpan::Image { src, alt, title } = img.unwrap() {
                    assert_eq!(src, "images/pic.webp");
                    assert_eq!(alt, "Alt text");
                    assert_eq!(title.as_deref(), Some("Title"));
                }
            }
            other => panic!("Expected Paragraph, got {:?}", other),
        }
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
        match &fragments[0] {
            CellFragment::Text(slice) => assert_eq!(slice.len(), 2),
            other => panic!("expected text fragment, got {:?}", other),
        }
        match &fragments[1] {
            CellFragment::Image(span) => {
                if let InlineSpan::Image { src, .. } = span {
                    assert_eq!(src, "img.png");
                } else {
                    panic!("image fragment should point to inline image span");
                }
            }
            other => panic!("second fragment should be image, got {:?}", other),
        }
        match &fragments[2] {
            CellFragment::Text(slice) => {
                assert_eq!(slice.len(), 1);
                if let InlineSpan::Text(content) = &slice[0] {
                    assert_eq!(content, "gamma");
                } else {
                    panic!("expected trailing text span");
                }
            }
            other => panic!("expected trailing text fragment, got {:?}", other),
        }
    }

    #[test]
    fn test_cell_fragments_detect_single_emoji_span() {
        let renderer = MarkdownRenderer::new();
        let rocket = crate::emoji_catalog::shortcode_map()
            .get(":rocket:")
            .expect("rocket shortcode");
        let spans = vec![
            InlineSpan::Text((*rocket).to_string()),
            InlineSpan::Text("tail".into()),
        ];
        let fragments = renderer.cell_fragments(&spans);
        assert_eq!(fragments.len(), 2);
        assert!(matches!(
            &fragments[0],
            CellFragment::Emoji(e) if !e.is_empty()
        ));
        assert!(matches!(fragments[1], CellFragment::Text(_)));
    }

    #[test]
    fn test_layout_job_builder_respects_wrap_width() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text(
            "A long column entry that should wrap neatly within the supplied width.".into(),
        )];
        let style = egui::Style::default();
        let build = renderer.build_layout_job(&style, &spans, 180.0, false);
        assert_eq!(build.job.wrap.max_width, 180.0);
        assert!(
            build.job.text.contains("column entry"),
            "plain text should be preserved"
        );
    }

    #[test]
    fn test_layout_job_builder_highlights_matches() {
        let renderer = MarkdownRenderer::new();
        renderer.set_highlight_phrase(Some("wrap"));
        let spans = vec![InlineSpan::Text("wrap me, wrap me again".into())];
        let style = egui::Style::default();
        let build = renderer.build_layout_job(&style, &spans, 200.0, false);
        let highlight_bg = style.visuals.selection.bg_fill;
        assert!(
            build
                .job
                .sections
                .iter()
                .any(|s| s.format.background == highlight_bg),
            "at least one section should carry highlight background"
        );
    }

    #[test]
    fn test_layout_job_builder_tracks_link_ranges() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Link {
            text: "Docs".into(),
            url: "https://example.org/docs".into(),
        }];
        let style = egui::Style::default();
        let build = renderer.build_layout_job(&style, &spans, 220.0, false);
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
    fn test_table_width_solver_keeps_short_columns_readable() {
        let mins = vec![90.0, 90.0, 90.0, 90.0];
        let desired = vec![120.0, 120.0, 360.0, 160.0];
        let widths = MarkdownRenderer::resolve_table_widths(420.0, &mins, &desired);
        assert_eq!(widths.len(), 4);
        // Narrow columns should stay near their minimums even when a wide column exists
        assert!(
            widths[0] >= 85.0,
            "Version column shrank too far: {}",
            widths[0]
        );
        assert!(
            widths[2] > widths[0],
            "Wide column should retain more width"
        );
        let sum: f32 = widths.iter().sum();
        assert!(
            (sum - 420.0).abs() < 0.5,
            "Widths should consume available space, got {sum}"
        );
    }

    #[test]
    fn test_table_width_solver_handles_constrained_space() {
        let mins = vec![100.0, 100.0, 100.0, 100.0];
        let desired = vec![200.0, 240.0, 360.0, 160.0];
        let widths = MarkdownRenderer::resolve_table_widths(260.0, &mins, &desired);
        assert_eq!(widths.len(), 4);
        assert!(
            widths.iter().all(|w| *w > 0.0),
            "Widths must stay positive even when clamped hard"
        );
        let sum: f32 = widths.iter().sum();
        assert!(
            (sum - 260.0).abs() < 0.5,
            "Widths should sum to available space even under tight constraints"
        );
    }

    #[test]
    fn table_cells_keep_images_and_formatting() {
        let renderer = MarkdownRenderer::new();
        let md = "\
| H1 | H2 |
| --- | --- |
| text ![Alt](img.png) | **bold** and [link](https://example.com) |";
        let elements = renderer.parse(md).expect("parse ok");
        let table = elements
            .iter()
            .find_map(|el| match el {
                MarkdownElement::Table { headers: _, rows } => Some(rows),
                _ => None,
            })
            .expect("table present");
        assert_eq!(table.len(), 1);
        let row = &table[0];
        assert!(row[0]
            .iter()
            .any(|span| matches!(span, InlineSpan::Image { src, .. } if src == "img.png")));
        assert!(row[1]
            .iter()
            .any(|span| matches!(span, InlineSpan::Strong(text) if text.contains("bold"))));
        assert!(row[1].iter().any(|span| matches!(
            span,
            InlineSpan::Link { url, .. } if url == "https://example.com"
        )));
    }

    #[test]
    fn table_ids_are_unique_per_position() {
        let renderer = MarkdownRenderer::new();
        let md = "\
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
                MarkdownElement::Table { headers, rows } => {
                    Some(renderer.compute_table_id(headers, rows, idx))
                }
                _ => None,
            })
            .collect();
        assert_eq!(tables.len(), 2);
        assert_ne!(tables[0], tables[1], "table ids should differ by position");
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

        let _ = renderer.cached_layout_job(&style, Some(0), 0, &spans, 120.0, false);
        let _ = renderer.cached_layout_job(&style, Some(0), 0, &spans, 120.0, false);

        let (hits, misses) = renderer.table_layout_cache_stats();
        assert!(hits >= 1);
        assert!(misses >= 1);

        renderer.clear_table_layout_cache();
        assert_eq!(renderer.table_layout_cache_stats(), (0, 0));
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
        with_test_ui(|_, ui| {
            let loaded = renderer.get_or_load_image_texture(ui, &resolved);
            assert!(loaded.is_some());
        });

        assert!(renderer.image_textures.borrow().contains_key(&resolved));
    }

    #[test]
    fn test_context_menu_helpers_execute() {
        let renderer = MarkdownRenderer::new();
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
    fn test_table_wrap_overhaul_toggle() {
        let mut renderer = MarkdownRenderer::new();
        assert!(renderer.table_wrap_overhaul_enabled());
        renderer.set_table_wrap_overhaul_enabled(false);
        assert!(!renderer.table_wrap_overhaul_enabled());
        renderer.set_table_wrap_overhaul_enabled(true);
        assert!(renderer.table_wrap_overhaul_enabled());
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
            let build = renderer.build_layout_job(&style, &spans, 200.0, false);
            assert!(build.plain_text.contains("plain"));
            assert_eq!(build.link_ranges.len(), 2);
        });
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

        let _guard = ForcedRenderActions::new(&["link_hover", "link_click", "image_hover"]);
        with_test_ui(|_, ui| {
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
            renderer.render_inline_span(ui, &image_span, None, None);
            renderer.render_inline_span(ui, &missing_span, None, None);
        });
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
            },
            MarkdownElement::List {
                ordered: false,
                items: vec![vec![InlineSpan::Text("Item".to_string())]],
            },
            MarkdownElement::Quote {
                depth: 1,
                lines: vec![vec![InlineSpan::Text("Quote".to_string())]],
            },
            MarkdownElement::HorizontalRule,
            MarkdownElement::Table {
                headers: vec![vec![InlineSpan::Text("H".to_string())]],
                rows: vec![vec![vec![InlineSpan::Image {
                    src: "img.png".to_string(),
                    alt: "Alt".to_string(),
                    title: None,
                }]]],
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
    fn test_table_rendering_legacy_and_overhaul() {
        let mut renderer = MarkdownRenderer::new();
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
        let elements = vec![MarkdownElement::Table { headers, rows }];

        with_test_ui(|_, ui| {
            renderer.render_to_ui(ui, &elements);
        });

        renderer.set_table_wrap_overhaul_enabled(false);
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
            items: vec![vec![InlineSpan::Text("Item".to_string())]],
        };
        let quote = MarkdownElement::Quote {
            depth: 1,
            lines: vec![vec![InlineSpan::Text("Quote".to_string())]],
        };
        let table = MarkdownElement::Table {
            headers: vec![vec![InlineSpan::Text("Header".to_string())]],
            rows: vec![vec![vec![InlineSpan::Text("Cell".to_string())]]],
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
        let next = renderer
            .parse_element(&events, 0, &mut elements, &mut slugs)
            .expect("parse ok");
        assert_eq!(next, events.len());
        assert!(matches!(
            elements.first(),
            Some(MarkdownElement::Paragraph(_))
        ));
        if let Some(MarkdownElement::Paragraph(spans)) = elements.first() {
            assert!(matches!(
                spans.first(),
                Some(InlineSpan::Image { title: Some(t), .. }) if t == "Title"
            ));
        }
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

        let (items, next) = renderer.parse_list(&events, 1, None, 0)?;
        assert_eq!(next, events.len());
        assert_eq!(items.len(), 1);
        let spans = &items[0];
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Emphasis(_))));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Strong(_))));
        assert!(spans
            .iter()
            .any(|s| matches!(s, InlineSpan::Strikethrough(_))));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Link { .. })));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Image { .. })));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Code(_))));
        assert!(spans
            .iter()
            .any(|s| matches!(s, InlineSpan::Text(t) if t.contains('\n'))));
        Ok(())
    }

    #[test]
    fn test_parse_inline_spans_with_breaks_variants() -> Result<()> {
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
            renderer.parse_inline_spans_with_breaks(&events, 1, Tag::Paragraph, true)?;
        assert_eq!(next, events.len());
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Code(_))));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Strong(_))));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Emphasis(_))));
        assert!(spans
            .iter()
            .any(|s| matches!(s, InlineSpan::Strikethrough(_))));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Link { .. })));
        assert!(spans.iter().any(|s| matches!(s, InlineSpan::Image { .. })));
        assert!(spans
            .iter()
            .any(|s| matches!(s, InlineSpan::Text(t) if t.contains('\n'))));

        let (spans_no_break, _) = renderer.parse_inline_spans(&events, 1, Tag::Paragraph)?;
        assert!(spans_no_break
            .iter()
            .any(|s| matches!(s, InlineSpan::Text(t) if t.contains(' '))));
        Ok(())
    }

    #[test]
    fn test_collect_blockquotes_nested_and_breaks() -> Result<()> {
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
        let (quotes, next) = renderer.collect_blockquotes(&events, 1, 1)?;
        assert_eq!(next, events.len());
        assert!(quotes.iter().any(|(depth, _)| *depth == 1));
        assert!(quotes.iter().any(|(depth, _)| *depth == 2));
        Ok(())
    }

    #[test]
    fn test_parse_code_block_with_language() -> Result<()> {
        let renderer = MarkdownRenderer::new();
        let elements = renderer.parse("```rust\nfn main() {}\n```")?;
        assert!(elements.iter().any(|el| matches!(
            el,
            MarkdownElement::CodeBlock {
                language: Some(lang),
                ..
            } if lang == "rust"
        )));
        Ok(())
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
    fn test_handle_width_change_requests_repaint() {
        let renderer = MarkdownRenderer::new();
        let ctx = egui::Context::default();
        renderer.handle_width_change(&ctx, 42, WidthChange::Large);
        let metrics = renderer.table_metrics.borrow();
        let entry = metrics.entry(42).expect("metrics entry");
        assert!(entry.last_discard_frame.is_some());
    }

    #[test]
    fn test_render_list_multiline_indent_and_empty() {
        let renderer = MarkdownRenderer::new();
        let spans = vec![InlineSpan::Text("First\n  Nested".to_string())];
        with_test_ui(|ctx, ui| {
            ctx.set_visuals(egui::Visuals::light());
            renderer.render_list(ui, false, &[]);
            renderer.render_list(ui, true, std::slice::from_ref(&spans));
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
        with_test_ui(|_, ui| {
            let embedded = renderer.get_or_load_image_texture(ui, "assets/emoji/1f600.png");
            assert!(embedded.is_some());
            let remote = renderer.get_or_load_image_texture(ui, "https://example.com/img.png");
            assert!(remote.is_none());
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
    fn test_render_overhauled_cell_fragments() {
        let renderer = MarkdownRenderer::new();
        let emoji_span = InlineSpan::Text("\u{1f600}".to_string());
        let image_span = InlineSpan::Image {
            src: "assets/emoji/1f600.png".to_string(),
            alt: "alt".to_string(),
            title: None,
        };
        with_test_ui(|_, ui| {
            renderer.render_overhauled_cell(ui, &[], 120.0, false, Some(0), 0);
            renderer.render_overhauled_cell(
                ui,
                &[emoji_span.clone(), image_span.clone()],
                120.0,
                false,
                Some(1),
                1,
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
                    let build = renderer.build_layout_job(ui.style(), &spans, 30.0, false);
                    renderer.paint_table_text_job(ui, 120.0, build);
                },
            );
        });

        let input = input_with_click(click_pos, egui::PointerButton::Primary);
        run_frame_with_input(&ctx, input, |_, ui| {
            ui.allocate_ui_at_rect(
                egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 80.0)),
                |ui| {
                    let build = renderer.build_layout_job(ui.style(), &spans, 30.0, false);
                    renderer.paint_table_text_job(ui, 120.0, build);
                },
            );
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
            renderer.render_table_tablebuilder(ui, &headers, &rows, 0);
            renderer.render_table_tablebuilder(ui, &headers, &[], 1);
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
    fn test_render_code_block_highlight_and_fallback() {
        let renderer = MarkdownRenderer::new();
        with_test_ui(|_, ui| {
            renderer.render_code_block(ui, Some("rust"), "fn main() {}");
            renderer.render_code_block(ui, Some("notalanguage"), "code");
        });
    }

    #[test]
    fn test_to_superscript_maps_values() {
        let mapped = MarkdownRenderer::to_superscript("Abc123!");
        assert_ne!(mapped, "Abc123!");
    }
}
