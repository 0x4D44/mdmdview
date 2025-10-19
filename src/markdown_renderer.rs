use crate::{emoji_assets, emoji_catalog};
use anyhow::Result;
use crossbeam_channel::{
    bounded, Receiver as KrokiJobReceiver, Sender as KrokiJobSender, TrySendError,
};
use egui::{Color32, RichText, Stroke};
use pulldown_cmark::{Event, LinkType, Options, Parser, Tag};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::mpsc::Receiver;
use std::time::{Duration, SystemTime};
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::LinesWithEndings;
use unicode_casefold::UnicodeCaseFold;
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

// Embedded Mermaid JS bytes are generated in build.rs into OUT_DIR.
// Place mermaid.min.js at assets/vendor/mermaid.min.js to embed it.
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

#[derive(Clone, Copy, Default)]
struct InlineStyle {
    strong: bool,
    italics: bool,
    strike: bool,
    color: Option<Color32>,
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
    // Kroki (default Mermaid path) state
    kroki_pending: RefCell<HashSet<u64>>, // code_hashes in flight
    kroki_svg_cache: RefCell<HashMap<u64, Vec<u8>>>, // code_hash -> image bytes (PNG preferred)
    kroki_errors: RefCell<HashMap<u64, String>>, // code_hash -> last error
    kroki_job_tx: KrokiJobSender<KrokiRequest>, // UI -> worker queue
    kroki_rx: Receiver<(u64, Result<Vec<u8>, String>)>, // background -> UI thread
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_engine: RefCell<Option<MermaidEngine>>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_svg_cache: RefCell<HashMap<u64, String>>, // code_hash -> SVG string
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_failed: RefCell<HashSet<u64>>, // hashes that failed rendering; skip reattempts
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_init_logged: RefCell<bool>,
    #[cfg(feature = "mermaid-quickjs")]
    mermaid_last_error: RefCell<Option<String>>,
}

impl Default for MarkdownRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl MarkdownRenderer {
    const MAX_KROKI_JOBS: usize = 4;

    /// Create a new markdown renderer
    pub fn new() -> Self {
        let (tx, rx) = std::sync::mpsc::channel();
        let (job_tx, job_rx): (KrokiJobSender<KrokiRequest>, KrokiJobReceiver<KrokiRequest>) =
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
                        let result = MarkdownRenderer::perform_kroki_with_agent(
                            &agent,
                            &job.url,
                            &job.payload,
                        );
                        let _ = result_tx.send((job.key, result));
                    }
                })
            {
                eprintln!("Failed to start Kroki worker thread: {}", err);
            }
        }
        drop(job_rx);
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
            kroki_pending: RefCell::new(HashSet::new()),
            kroki_svg_cache: RefCell::new(HashMap::new()),
            kroki_errors: RefCell::new(HashMap::new()),
            kroki_job_tx: job_tx,
            kroki_rx: rx,
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_engine: RefCell::new(None),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_svg_cache: RefCell::new(HashMap::new()),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_failed: RefCell::new(HashSet::new()),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_init_logged: RefCell::new(false),
            #[cfg(feature = "mermaid-quickjs")]
            mermaid_last_error: RefCell::new(None),
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
                let (spans, next_idx) =
                    self.parse_inline_spans(events, start + 1, Tag::Paragraph)?;
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
            Event::Start(Tag::Table(_)) => {
                let (headers, rows, next_idx) = self.parse_table(events, start + 1)?;
                elements.push(MarkdownElement::Table { headers, rows });
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
                                        "•".to_string()
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

        // Parse inline spans within a table cell until End(TableCell)
        fn collect_cell_spans(
            this: &MarkdownRenderer,
            events: &[Event],
            mut j: usize,
        ) -> (Vec<InlineSpan>, usize) {
            let mut spans: Vec<InlineSpan> = Vec::new();
            while j < events.len() {
                match &events[j] {
                    Event::End(Tag::TableCell) => return (spans, j + 1),
                    Event::Text(t) => spans.push(InlineSpan::Text(t.to_string())),
                    Event::Code(t) => spans.push(InlineSpan::Code(t.to_string())),
                    Event::Start(Tag::Emphasis) => {
                        let (inner, next) = this
                            .collect_until_tag_end(events, j + 1, Tag::Emphasis)
                            .unwrap_or_default();
                        spans.push(InlineSpan::Emphasis(inner));
                        j = next - 1; // -1 to offset j+=1 below
                    }
                    Event::Start(Tag::Strong) => {
                        let (inner, next) = this
                            .collect_until_tag_end(events, j + 1, Tag::Strong)
                            .unwrap_or_default();
                        spans.push(InlineSpan::Strong(inner));
                        j = next - 1;
                    }
                    Event::Start(Tag::Strikethrough) => {
                        let (inner, next) = this
                            .collect_until_tag_end(events, j + 1, Tag::Strikethrough)
                            .unwrap_or_default();
                        spans.push(InlineSpan::Strikethrough(inner));
                        j = next - 1;
                    }
                    Event::Start(Tag::Link(_, url, _)) => {
                        let url_str = url.to_string();
                        let (text, next) = this
                            .collect_until_tag_end(
                                events,
                                j + 1,
                                Tag::Link(LinkType::Inline, "".into(), "".into()),
                            )
                            .unwrap_or_default();
                        spans.push(InlineSpan::Link { text, url: url_str });
                        j = next - 1;
                    }
                    _ => {}
                }
                j += 1;
            }
            (spans, j)
        }

        while i < events.len() {
            match &events[i] {
                Event::Start(Tag::TableHead) => {
                    i += 1;
                    while i < events.len() {
                        match &events[i] {
                            Event::Start(Tag::TableCell) => {
                                let (spans, next) = collect_cell_spans(self, events, i + 1);
                                headers.push(spans);
                                i = next;
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
                                let (spans, next) = collect_cell_spans(self, events, i + 1);
                                row.push(spans);
                                i = next;
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
        for element in elements {
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
                    MarkdownElement::Table { headers, rows } => {
                        self.render_table(ui, headers, rows);
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
                ui.add(
                    egui::Label::new(
                        RichText::new(code)
                            .size(self.font_sizes.code)
                            .family(egui::FontFamily::Monospace)
                            .background_color(bg)
                            .color(fg),
                    )
                    .wrap(false)
                    .selectable(false),
                );
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
                if r.hovered() {
                    ui.output_mut(|o| o.cursor_icon = egui::CursorIcon::PointingHand);
                }
                if r.clicked() {
                    if let Some(fragment) = Self::extract_fragment(url) {
                        // Store pending anchor for the app to handle scroll
                        *self.pending_anchor.borrow_mut() = Some(fragment);
                    } else {
                        self.open_url(url);
                    }
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
                            if resp.hovered() {
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
        if let Some(h) = self.highlight_phrase.borrow().as_ref() {
            if !h.is_empty() {
                let mut folded = String::new();
                let mut folded_to_char: Vec<usize> = Vec::new();
                let mut char_ranges: Vec<(usize, usize)> = Vec::new();
                for (char_idx, (byte_idx, ch)) in expanded.char_indices().enumerate() {
                    let folded_piece: String = ch.to_string().case_fold().nfkc().collect();
                    let before = folded.len();
                    folded.push_str(&folded_piece);
                    let after = folded.len();
                    for _ in before..after {
                        folded_to_char.push(char_idx);
                    }
                    char_ranges.push((byte_idx, byte_idx + ch.len_utf8()));
                }

                let mut rendered_until = 0usize;
                let mut search_at = 0usize;
                while let Some(pos) = folded[search_at..].find(h) {
                    let abs = search_at + pos;
                    if abs >= folded_to_char.len() {
                        break;
                    }
                    let start_char_idx = folded_to_char[abs];
                    let (start_byte, _) = char_ranges[start_char_idx];
                    if start_byte > rendered_until {
                        self.render_plain_segment(
                            ui,
                            &expanded[rendered_until..start_byte],
                            size,
                            style,
                        );
                    }

                    let match_end = abs + h.len();
                    let end_char_idx = if match_end == 0 {
                        start_char_idx
                    } else {
                        folded_to_char[match_end.saturating_sub(1)]
                    };
                    let (_, end_byte) = char_ranges[end_char_idx];
                    let highlight_slice = &expanded[start_byte..end_byte];
                    self.render_highlighted_segment(ui, highlight_slice, size, style);

                    rendered_until = end_byte;
                    search_at = match_end;
                }

                if rendered_until < expanded.len() {
                    let rest = &expanded[rendered_until..];
                    if !rest.is_empty() {
                        self.render_plain_segment(ui, rest, size, style);
                    }
                }
                return;
            }
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
        ui.label(rich);
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
        let mut rich = RichText::new(text)
            .size(size)
            .background_color(Color32::from_rgb(80, 80, 0));
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
        ui.label(rich);
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
                    InlineSpan::Image { .. } => {
                        // Approximate: image width at current scale, capped by available width
                        // We don't have the natural width here; approximate with available width.
                        width += ui.available_width();
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
            '🎉' | '✅'
                | '🚀'
                | '🙂'
                | '😀'
                | '😉'
                | '⭐'
                | '🔥'
                | '👍'
                | '👎'
                | '💡'
                | '❓'
                | '❗'
                | '📝'
                | '🧠'
                | '🧪'
                | '📦'
                | '🔧'
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
    // Example: "5^th^" -> "5ᵗʰ"
    fn expand_superscripts(s: &str) -> String {
        if !s.contains('^') {
            return s.to_string();
        }
        let mut out = String::new();
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '^' {
                // Collect until next caret
                let mut buf = String::new();
                while let Some(&nc) = chars.peek() {
                    chars.next();
                    if nc == '^' {
                        break;
                    }
                    buf.push(nc);
                }
                if !buf.is_empty() {
                    out.push_str(&Self::to_superscript(&buf));
                } else {
                    // Lone caret, keep as-is
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
                '0' => '⁰',
                '1' => '¹',
                '2' => '²',
                '3' => '³',
                '4' => '⁴',
                '5' => '⁵',
                '6' => '⁶',
                '7' => '⁷',
                '8' => '⁸',
                '9' => '⁹',
                '+' => '⁺',
                '-' => '⁻',
                '=' => '⁼',
                '(' => '⁽',
                ')' => '⁾',
                'a' | 'A' => 'ᵃ',
                'b' | 'B' => 'ᵇ',
                'c' | 'C' => 'ᶜ',
                'd' | 'D' => 'ᵈ',
                'e' | 'E' => 'ᵉ',
                'f' | 'F' => 'ᶠ',
                'g' | 'G' => 'ᵍ',
                'h' | 'H' => 'ʰ',
                'i' | 'I' => 'ⁱ',
                'j' | 'J' => 'ʲ',
                'k' | 'K' => 'ᵏ',
                'l' | 'L' => 'ˡ',
                'm' | 'M' => 'ᵐ',
                'n' | 'N' => 'ⁿ',
                'o' | 'O' => 'ᵒ',
                'p' | 'P' => 'ᵖ',
                'q' | 'Q' => 'ᑫ', // fallback; approximate
                'r' | 'R' => 'ʳ',
                's' | 'S' => 'ˢ',
                't' | 'T' => 'ᵗ',
                'u' | 'U' => 'ᵘ',
                'v' | 'V' => 'ᵛ',
                'w' | 'W' => 'ʷ',
                'x' | 'X' => 'ˣ',
                'y' | 'Y' => 'ʸ',
                'z' | 'Z' => 'ᶻ',
                other => other,
            })
            .collect()
    }

    fn generate_emoji_image(&self, emoji: &str, size: usize) -> egui::ColorImage {
        // Simple procedural placeholder icons to keep binary small and avoid external assets
        // Each emoji gets a colored circle and a simple accent
        use egui::Color32 as C;
        let mut img = egui::ColorImage::new([size, size], C::TRANSPARENT);
        let cx = (size as i32) / 2;
        let cy = cx;
        let r = (size as i32) / 2 - 2;

        let (base, accent) = match emoji {
            "🎉" => (C::from_rgb(255, 215, 0), C::from_rgb(255, 80, 80)),
            "✅" => (C::from_rgb(30, 150, 30), C::WHITE),
            "🚀" => (C::from_rgb(70, 70, 200), C::from_rgb(255, 100, 100)),
            "🙂" => (C::from_rgb(255, 205, 64), C::from_rgb(90, 60, 10)),
            "😀" => (C::from_rgb(255, 205, 64), C::from_rgb(90, 60, 10)),
            "😉" => (C::from_rgb(255, 205, 64), C::from_rgb(90, 60, 10)),
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
                            "•".to_string()
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

        egui::Frame::none()
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

        ui.add_space(8.0);
    }

    /// Try to render a Mermaid diagram. Returns true if handled (rendered or placeholder drawn).
    fn render_mermaid_block(&self, ui: &mut egui::Ui, code: &str) -> bool {
        // First, drain any completed Kroki jobs into local caches.
        self.poll_kroki_results();

        // If QuickJS feature is enabled, attempt rendering to SVG then rasterize like normal SVG.
        #[cfg(feature = "mermaid-quickjs")]
        {
            let key = Self::hash_str(code);
            if self.mermaid_failed.borrow().contains(&key) {
                // Previously failed; show brief diagnostics and bail
                let bytes = mermaid_embed::MERMAID_JS.len();
                let last = self.mermaid_last_error.borrow().clone().unwrap_or_default();
                egui::Frame::none().inner_margin(4.0).show(ui, |ui| {
                    ui.label(
                        RichText::new(format!(
                            "Mermaid skipped (prev fail). Bytes:{} Hash:{:016x}\n{}",
                            bytes, key, last
                        ))
                        .family(egui::FontFamily::Monospace)
                        .size(self.font_sizes.code),
                    );
                });
                return true;
            } else if let Some(tex) = self.get_or_load_mermaid_texture(ui, code) {
                let size = tex.size();
                let (tw, th) = (size[0] as f32, size[1] as f32);
                let available_w = ui.available_width().max(1.0);
                let base_scale = self.ui_scale();
                let scaled_w = tw * base_scale;
                let scale = if scaled_w > available_w {
                    (available_w / tw).clamp(0.01, 4.0)
                } else {
                    base_scale
                };
                let size = egui::vec2((tw * scale).round(), (th * scale).round());
                ui.add(egui::Image::new(&tex).fit_to_exact_size(size));
                return true;
            } else {
                // Mark as failed to avoid repeated attempts causing jank
                self.mermaid_failed.borrow_mut().insert(key);
                let bytes = mermaid_embed::MERMAID_JS.len();
                let last = self
                    .mermaid_last_error
                    .borrow()
                    .clone()
                    .unwrap_or_else(|| "unknown error".to_string());
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
                                bytes, key, last
                            ))
                            .family(egui::FontFamily::Monospace)
                            .size(self.font_sizes.code)
                            .color(Color32::from_rgb(180, 180, 180)),
                        );
                    });
                // fall through to code block rendering
            }
        }

        // Default path: render via Kroki service (non-blocking)
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
                            .size(self.font_sizes.code),
                    );
                });
            return false;
        }
        // If we already have an SVG for this diagram, rasterize to a texture and draw it
        if let Some(img_bytes) = self.kroki_svg_cache.borrow().get(&key) {
            // Use a width-bucketed texture cache key to avoid re-rasterizing too often
            let width_bucket = (ui.available_width().ceil() as u32) / 32 * 32;
            let cache_key = format!("mermaid:{}:w{}", key, width_bucket);
            if let Some(entry) = self.image_textures.borrow().get(&cache_key) {
                let tex = entry.texture.clone();
                let (tw, th) = (entry.size[0] as f32, entry.size[1] as f32);
                let available_w = ui.available_width().max(1.0);
                let base_scale = self.ui_scale();
                let scaled_w = tw * base_scale;
                let scale = if scaled_w > available_w {
                    (available_w / tw).clamp(0.01, 4.0)
                } else {
                    base_scale
                };
                let size = egui::vec2((tw * scale).round(), (th * scale).round());
                ui.add(egui::Image::new(&tex).fit_to_exact_size(size));
                return true;
            }

            // Decode image bytes (prefer PNG). Fallback to SVG if not raster.
            if let Some((img, w, h)) =
                Self::bytes_to_color_image_guess(img_bytes, Self::mermaid_bg_fill())
            {
                let tex =
                    ui.ctx()
                        .load_texture(cache_key.clone(), img, egui::TextureOptions::LINEAR);
                self.store_image_texture(&cache_key, tex.clone(), [w, h], None);
                // Draw now
                let (tw, th) = (w as f32, h as f32);
                let available_w = ui.available_width().max(1.0);
                let base_scale = self.ui_scale();
                let scaled_w = tw * base_scale;
                let scale = if scaled_w > available_w {
                    (available_w / tw).clamp(0.01, 4.0)
                } else {
                    base_scale
                };
                let size = egui::vec2((tw * scale).round(), (th * scale).round());
                ui.add(egui::Image::new(&tex).fit_to_exact_size(size));
                return true;
            } else {
                // Decode failed; record error and fall through to message/source
                self.kroki_errors
                    .borrow_mut()
                    .insert(key, "Failed to decode diagram bytes".to_string());
            }
        }

        // If there was a prior error, show diagnostics and let caller show source
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
            return false; // let caller render the source block
        }

        // If no job is pending, enqueue one for the worker pool
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

        // Show placeholder while rendering or waiting for an available worker
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
                        .size(self.font_sizes.code),
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

    #[cfg(feature = "mermaid-quickjs")]
    fn render_mermaid_to_svg_string(&self, code: &str) -> Option<String> {
        use rquickjs::{Context, Function, Runtime};
        // Quick check
        if mermaid_embed::MERMAID_JS.is_empty() {
            self.mermaid_last_error
                .borrow_mut()
                .replace("No embedded Mermaid JS".to_string());
            return None;
        }
        // Initialize engine once
        if self.mermaid_engine.borrow().is_none() {
            let rt = Runtime::new().ok()?;
            let ctx = Context::full(&rt).ok()?;
            let engine = MermaidEngine { rt, ctx };
            // Load library and shim
            let _ = engine.ctx.with(|ctx| {
                let shim = r#"
                    var window = globalThis;
                    var document = {
                      body: {},
                      createElement: function(tag){
                        return {
                          innerHTML: '',
                          setAttribute: function(){},
                          getAttribute: function(){ return null; },
                          appendChild: function(){},
                          querySelector: function(){ return null; },
                        };
                      },
                      createElementNS: function(ns, tag){ return this.createElement(tag); },
                      getElementById: function(){ return null; },
                      querySelector: function(){ return null; },
                    };
                    window.document = document;
                    // Minimal timers and raf
                    window.setTimeout = function(fn, ms){ fn(); return 1; };
                    window.clearTimeout = function(id){};
                    window.requestAnimationFrame = function(fn){ fn(0); return 1; };
                    window.performance = { now: function(){ return 0; } };
                "#;
                if let Err(e) = ctx.eval::<(), _>(shim) {
                    self.mermaid_last_error
                        .borrow_mut()
                        .replace(format!("Shim error: {}", e));
                }
                if let Ok(js) = std::str::from_utf8(mermaid_embed::MERMAID_JS) {
                    if let Err(e) = ctx.eval::<(), _>(js) {
                        self.mermaid_last_error
                            .borrow_mut()
                            .replace(format!("Mermaid eval error: {}", e));
                    }
                    if let Err(e) = ctx.eval::<(), _>(
                        r#"if (window.mermaid && mermaid.mermaidAPI) {
                               mermaid.mermaidAPI.initialize({startOnLoad: false, securityLevel: 'loose'});
                           } else { throw new Error('Mermaid not available'); }
                        "#,
                    ) {
                        self.mermaid_last_error
                            .borrow_mut()
                            .replace(format!("Mermaid init error: {}", e));
                    }
                }
                Ok::<(), ()>(())
            });
            self.mermaid_engine.borrow_mut().replace(engine);
            // One-time log for diagnostics
            if !*self.mermaid_init_logged.borrow() {
                eprintln!(
                    "Mermaid embedded bytes: {}",
                    mermaid_embed::MERMAID_JS.len()
                );
                *self.mermaid_init_logged.borrow_mut() = true;
            }
        }

        // Use cache if available
        let key = Self::hash_str(code);
        if let Some(svg) = self.mermaid_svg_cache.borrow().get(&key) {
            return Some(svg.clone());
        }

        // Render fresh
        let svg = self.mermaid_engine.borrow().as_ref().and_then(|engine| {
            engine.ctx.with(|ctx| {
                // Minimal DOM shim sufficient for mermaid.mermaidAPI.render
                let wrapper = r#"
                (function(id, code){
                    var svgOut = null;
                    mermaid.mermaidAPI.render(id, code, function(svg){ svgOut = svg; });
                    if (typeof svgOut !== 'string' || svgOut.length === 0) {
                        throw new Error('Mermaid render returned empty');
                    }
                    return svgOut;
                })
            "#;
                let func: Function = match ctx.eval(wrapper) {
                    Ok(f) => f,
                    Err(e) => {
                        self.mermaid_last_error
                            .borrow_mut()
                            .replace(format!("Wrapper eval error: {}", e));
                        return None;
                    }
                };
                let id = format!("m{:016x}", key);
                let svg: String = match func.call((id.as_str(), code)) {
                    Ok(s) => s,
                    Err(e) => {
                        self.mermaid_last_error
                            .borrow_mut()
                            .replace(format!("Mermaid call error: {}", e));
                        return None;
                    }
                };
                Some(svg)
            })
        });
        if let Some(svg) = svg {
            self.mermaid_svg_cache.borrow_mut().insert(key, svg.clone());
            self.mermaid_last_error.borrow_mut().take();
            return Some(svg);
        }
        self.mermaid_last_error
            .borrow_mut()
            .replace("Mermaid render produced no SVG".to_string());
        None
    }

    #[cfg(feature = "mermaid-quickjs")]
    fn get_or_load_mermaid_texture(
        &self,
        ui: &egui::Ui,
        code: &str,
    ) -> Option<egui::TextureHandle> {
        let key = Self::hash_str(code);
        let width_bucket = (ui.available_width().ceil() as u32).saturating_sub(0) / 16 * 16;
        let cache_key = format!("mermaid:{}:w{}", key, width_bucket);
        if let Some(entry) = self.image_textures.borrow().get(&cache_key) {
            return Some(entry.texture.clone());
        }
        let svg = self.render_mermaid_to_svg_string(code)?;
        let (img, w, h) = Self::svg_bytes_to_color_image(svg.as_bytes())?;
        let tex = ui
            .ctx()
            .load_texture(cache_key.clone(), img, egui::TextureOptions::LINEAR);
        self.store_image_texture(&cache_key, tex.clone(), [w, h], None);
        Some(tex)
    }

    fn hash_str(s: &str) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut h = DefaultHasher::new();
        s.hash(&mut h);
        h.finish()
    }

    // Drain any completed Kroki jobs and cache their results
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
    pub(crate) fn kroki_enabled_for_tests() -> bool {
        Self::kroki_enabled()
    }

    fn kroki_base_url(&self) -> String {
        std::env::var("MDMDVIEW_KROKI_URL").unwrap_or_else(|_| "https://kroki.io".to_string())
    }

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

    fn spawn_kroki_job(&self, key: u64, code: &str) -> Result<(), KrokiEnqueueError> {
        // Prefer PNG to avoid client-side SVG CSS/foreignObject limitations
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

    fn wrap_mermaid_theme(&self, code: &str) -> String {
        // If user already provided an init block, respect it
        if code.contains("%%{") && code.contains("}%%") && code.contains("init") {
            return code.to_string();
        }
        // Pastel palette defaults (nice contrast on dark/light UIs)
        let def_main_bkg = "#FFF8DB"; // soft warm yellow
        let def_primary = "#D7EEFF"; // pastel blue (nodes)
        let def_primary_border = "#9BB2C8"; // muted blue border
        let def_primary_text = "#1C2430"; // dark slate text
        let def_secondary = "#DFF5E1"; // pastel green
        let def_tertiary = "#E9E2FF"; // pastel lavender
        let def_line = "#6B7A90"; // slate lines/arrows
        let def_text = "#1C2430"; // general text
        let def_cluster_bkg = "#FFF1C1"; // soft yellow, slightly deeper
        let def_cluster_border = "#E5C07B"; // golden
        let def_default_link = def_line;
        let def_title = def_text;
        let def_label_bg = def_main_bkg;
        let def_edge_label_bg = def_main_bkg;

        // Allow overrides via env vars (keep defaults above if absent)
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
            theme_name,
            main_bkg,
            main_bkg,
            text,
            title,
            primary,
            primary_border,
            primary_text,
            secondary,
            tertiary,
            line,
            default_link,
            cluster_bkg,
            cluster_border,
            label_bg,
            edge_label_bg
        );
        format!("{}{}", theme, code)
    }

    // Choose background fill for Mermaid rasterization, based on env
    fn mermaid_bg_fill() -> Option<[u8; 4]> {
        // Preferred: explicit color
        if let Ok(hex) = std::env::var("MDMDVIEW_MERMAID_BG_COLOR") {
            if let Some(rgba) = Self::parse_hex_color(&hex) {
                return Some(rgba);
            }
        }
        // Mode selector; default to the theme's main background
        let mode = std::env::var("MDMDVIEW_MERMAID_BG").unwrap_or_else(|_| "theme".to_string());
        match mode.as_str() {
            "transparent" => None,
            "dark" => Some([20, 20, 20, 255]),
            "light" => Some([255, 255, 255, 255]),
            _ /* theme */ => {
                let hex = std::env::var("MDMDVIEW_MERMAID_MAIN_BKG").unwrap_or_else(|_| "#FFF8DB".to_string());
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

    /// Render a table using an egui Grid
    fn render_table(
        &self,
        ui: &mut egui::Ui,
        headers: &[Vec<InlineSpan>],
        rows: &[Vec<Vec<InlineSpan>>],
    ) {
        if headers.is_empty() {
            return;
        }

        ui.add_space(8.0);

        // 1) Measure intrinsic (natural) widths per column
        let mut natural: Vec<f32> = vec![0.0; headers.len()];
        // Header widths define a good minimum baseline
        for (ci, h) in headers.iter().enumerate() {
            natural[ci] = natural[ci].max(self.measure_inline_spans(ui, h));
        }
        // Include rows
        for row in rows {
            for (ci, cell) in row.iter().enumerate() {
                if ci < natural.len() {
                    natural[ci] = natural[ci].max(self.measure_inline_spans(ui, cell));
                }
            }
        }

        // 2) Compute target widths to fit viewport: if total natural > available, shrink with wrapping
        let available = ui.available_width().max(100.0);
        let total_natural: f32 = natural.iter().sum();
        let mut widths = natural.clone();

        if total_natural > available {
            // Initial proportional shrink
            let ratio = (available / total_natural).clamp(0.1, 1.0);
            for w in &mut widths {
                *w *= ratio;
            }
            // Enforce per-column minimums based on header widths (so headers remain readable)
            let mins = natural.clone(); // header/natural widths are our minima
                                        // If mins already exceed available, scale mins down proportionally
            let sum_mins: f32 = mins.iter().sum();
            if sum_mins > available {
                let r = (available / sum_mins).clamp(0.1, 1.0);
                for (w, m) in widths.iter_mut().zip(mins.iter()) {
                    *w = (*m * r).max(40.0); // absolute hard floor
                }
            } else {
                // Bring widths down but not below minima
                for (w, m) in widths.iter_mut().zip(mins.iter()) {
                    if *w < *m {
                        *w = (*m).max(40.0);
                    }
                }
                // If after clamping we still exceed available, shrink the slack columns
                let mut sum_w: f32 = widths.iter().sum();
                if sum_w > available {
                    let slack: Vec<f32> = widths
                        .iter()
                        .zip(mins.iter())
                        .map(|(w, m)| (w - m).max(0.0))
                        .collect();
                    let total_slack: f32 = slack.iter().sum();
                    if total_slack > 1.0 {
                        let excess = sum_w - available;
                        for (i, w) in widths.iter_mut().enumerate() {
                            if slack[i] > 0.0 {
                                let cut = excess * (slack[i] / total_slack);
                                *w -= cut;
                            }
                        }
                        // Recompute in case of rounding
                        sum_w = widths.iter().sum();
                        if sum_w > available {
                            let r2 = available / sum_w;
                            for w in &mut widths {
                                *w *= r2;
                            }
                        }
                    } else {
                        // No slack; scale everything
                        let r2 = available / sum_w;
                        for w in &mut widths {
                            *w *= r2;
                        }
                    }
                }
            }
        }

        // 3) Render the table with per-column width constraints; content wraps as needed
        egui::Frame::none()
            .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
            .show(ui, |ui| {
                egui::Grid::new("md_table").striped(true).show(ui, |ui| {
                    for (ci, h) in headers.iter().enumerate() {
                        let w = widths.get(ci).copied().unwrap_or(120.0);
                        ui.scope(|ui| {
                            ui.set_max_width(w);
                            ui.horizontal_wrapped(|ui| {
                                ui.spacing_mut().item_spacing.x = 0.0;
                                for span in h {
                                    // Emphasize header text
                                    self.render_inline_span(ui, span, None, Some(true));
                                }
                            });
                        });
                    }
                    ui.end_row();

                    for row in rows {
                        for (ci, cell) in row.iter().enumerate() {
                            if ci < headers.len() {
                                let w = widths.get(ci).copied().unwrap_or(120.0);
                                ui.scope(|ui| {
                                    ui.set_max_width(w);
                                    ui.horizontal_wrapped(|ui| {
                                        ui.spacing_mut().item_spacing.x = 0.0;
                                        for span in cell {
                                            self.render_inline_span(ui, span, None, None);
                                        }
                                    });
                                });
                            }
                        }
                        ui.end_row();
                    }
                });
            });
        ui.add_space(8.0);
    }

    /// Open URL in default browser
    fn open_url(&self, url: &str) {
        if let Err(e) = webbrowser::open(url) {
            eprintln!("Failed to open URL {}: {}", url, e);
        }
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

    /// Set or clear the highlight phrase (case-insensitive)
    pub fn set_highlight_phrase(&self, phrase: Option<&str>) {
        if let Some(p) = phrase {
            self.highlight_phrase
                .borrow_mut()
                .replace(p.case_fold().nfkc().collect());
        } else {
            self.highlight_phrase.borrow_mut().take();
        }
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
            MarkdownElement::Table { headers, rows } => {
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
            if let Some((color_image, w, h)) = Self::bytes_to_color_image_guess(bytes, None) {
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
            match Self::svg_bytes_to_color_image(&bytes) {
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

    fn svg_bytes_to_color_image(bytes: &[u8]) -> Option<(egui::ColorImage, u32, u32)> {
        Self::svg_bytes_to_color_image_with_bg(bytes, None)
    }

    fn svg_bytes_to_color_image_with_bg(
        bytes: &[u8],
        bg: Option<[u8; 4]>,
    ) -> Option<(egui::ColorImage, u32, u32)> {
        // Parse SVG
        let mut opt = usvg::Options::default();
        // Load system fonts so <text> elements render via resvg
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        opt.fontdb = std::sync::Arc::new(db);
        let tree = usvg::Tree::from_data(bytes, &opt).ok()?;
        // Determine output size in pixels
        let sz = tree.size();
        let pix = sz.to_int_size();
        let (mut w, mut h) = (pix.width(), pix.height());
        if w == 0 || h == 0 {
            // Fallback if unspecified
            w = 256;
            h = 256;
        }
        // Clamp to a reasonable texture size
        let max_side: u32 = 4096;
        if w > max_side || h > max_side {
            let scale = (max_side as f32 / w as f32).min(max_side as f32 / h as f32);
            w = (w as f32 * scale) as u32;
            h = (h as f32 * scale) as u32;
        }
        let mut pixmap = tiny_skia::Pixmap::new(w, h)?;
        // Optional background fill for better contrast against dark UI
        if let Some([r, g, b, a]) = bg {
            let color = tiny_skia::Color::from_rgba8(r, g, b, a);
            pixmap.fill(color);
        }
        let mut pmut = pixmap.as_mut();
        resvg::render(&tree, tiny_skia::Transform::identity(), &mut pmut);
        let data = pixmap.data();
        let img = egui::ColorImage::from_rgba_unmultiplied([w as usize, h as usize], data);
        Some((img, w, h))
    }

    // Try decode as raster image first (PNG/JPEG/WEBP). If that fails, try SVG.
    fn bytes_to_color_image_guess(
        bytes: &[u8],
        bg: Option<[u8; 4]>,
    ) -> Option<(egui::ColorImage, u32, u32)> {
        if let Ok(img) = image::load_from_memory(bytes) {
            let rgba = img.to_rgba8();
            let (w, h) = rgba.dimensions();
            let mut ci = egui::ColorImage::from_rgba_unmultiplied([w as usize, h as usize], &rgba);
            if let Some([br, bgc, bb, ba]) = bg {
                // Composite over solid bg
                for p in ci.pixels.iter_mut() {
                    let a = p[3] as f32 / 255.0;
                    let inv = 1.0 - a;
                    p[0] = (a * p[0] as f32 + inv * br as f32) as u8;
                    p[1] = (a * p[1] as f32 + inv * bgc as f32) as u8;
                    p[2] = (a * p[2] as f32 + inv * bb as f32) as u8;
                    p[3] = ba; // fully opaque output
                }
            }
            return Some((ci, w, h));
        }
        // Fallback to SVG
        Self::svg_bytes_to_color_image_with_bg(bytes, bg)
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
    }

    /// Reset zoom to default
    pub fn reset_zoom(&mut self) {
        self.font_sizes = FontSizes::default();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SAMPLE_FILES;

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
        assert!(out.contains("ᵗʰ"));
        assert!(out.contains("²"));
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
    fn test_kroki_enabled_env_flag() {
        std::env::remove_var("MDMDVIEW_ENABLE_KROKI");
        assert!(!MarkdownRenderer::kroki_enabled_for_tests());

        std::env::set_var("MDMDVIEW_ENABLE_KROKI", "true");
        assert!(MarkdownRenderer::kroki_enabled_for_tests());

        std::env::set_var("MDMDVIEW_ENABLE_KROKI", "0");
        assert!(!MarkdownRenderer::kroki_enabled_for_tests());

        std::env::remove_var("MDMDVIEW_ENABLE_KROKI");
    }

    #[test]
    fn test_spawn_kroki_job_reports_queue_disconnect() {
        let mut renderer = MarkdownRenderer::new();
        let (temp_tx, temp_rx) = crossbeam_channel::bounded::<KrokiRequest>(1);
        drop(temp_rx);
        renderer.kroki_job_tx = temp_tx;
        let result = renderer.spawn_kroki_job(1, "graph TD; A-->B;");
        assert!(matches!(result, Err(KrokiEnqueueError::Disconnected)));
    }

    #[test]
    fn test_spawn_kroki_job_reports_queue_full() {
        let mut renderer = MarkdownRenderer::new();
        let (temp_tx, temp_rx) = crossbeam_channel::bounded::<KrokiRequest>(1);
        // Fill the bounded channel so subsequent sends report Full
        temp_tx
            .try_send(KrokiRequest {
                key: 99,
                url: "https://example.invalid/diagram".to_string(),
                payload: "graph TD; A-->B;".to_string(),
            })
            .expect("pre-fill queue");
        renderer.kroki_job_tx = temp_tx.clone();
        // Keep receiver alive (unused) to avoid disconnecting the channel
        let _guard = temp_rx;
        let result = renderer.spawn_kroki_job(42, "graph TD; B-->C;");
        assert!(matches!(result, Err(KrokiEnqueueError::QueueFull)));
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
}
