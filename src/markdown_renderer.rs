use crate::{emoji_assets, emoji_catalog};
use anyhow::Result;
use egui::{Color32, RichText, Stroke};
use pulldown_cmark::{Event, LinkType, Options, Parser, Tag};
use std::cell::RefCell;
use std::collections::HashMap;
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::LinesWithEndings;

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
    Link { text: String, url: String },
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

/// Markdown renderer with proper inline element handling
pub struct MarkdownRenderer {
    font_sizes: FontSizes,
    syntax_set: SyntaxSet,
    theme_set: ThemeSet,
    emoji_textures: RefCell<HashMap<String, egui::TextureHandle>>,
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
}

impl Default for MarkdownRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl MarkdownRenderer {
    /// Create a new markdown renderer
    pub fn new() -> Self {
        Self {
            font_sizes: FontSizes::default(),
            syntax_set: SyntaxSet::load_defaults_newlines(),
            theme_set: ThemeSet::load_defaults(),
            emoji_textures: RefCell::new(HashMap::new()),
            header_rects: RefCell::new(HashMap::new()),
            pending_anchor: RefCell::new(None),
            link_counter: RefCell::new(0),
            element_rects: RefCell::new(Vec::new()),
            highlight_phrase: RefCell::new(None),
        }
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
        options.insert(Options::ENABLE_FOOTNOTES);
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
                let (items, next_idx) = self.parse_list(events, start + 1, *first_item)?;
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
                            Event::Start(Tag::List(child_first)) => {
                                let (child_items, next) =
                                    self.parse_list(events, i + 1, *child_first)?;
                                i = next;
                                let ordered_child = child_first.is_some();
                                for (idx, child) in child_items.into_iter().enumerate() {
                                    spans.push(InlineSpan::Text("\n".to_string()));
                                    let marker = if ordered_child {
                                        format!("{}.", idx + 1)
                                    } else {
                                        "•".to_string()
                                    };
                                    spans.push(InlineSpan::Text(format!("{} ", marker)));
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
        // For now, replace problematic Unicode arrows with ASCII equivalents
        // This is a conservative approach - only replace known problematic characters
        text.replace('→', " -> ")
            .replace('←', " <- ")
            .replace('↑', " ^ ")
            .replace('↓', " v ")
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
                // Create inline code with no extra spacing
                ui.spacing_mut().item_spacing.x = 0.0;
                ui.add(
                    egui::Label::new(
                        RichText::new(code.trim())
                            .size(self.font_sizes.code)
                            .family(egui::FontFamily::Monospace)
                            .background_color(Color32::from_rgb(30, 30, 30))
                            .color(Color32::from_rgb(180, 255, 180)),
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
        // Emojis to detect (starter set). Include heart with/without VS16.
        const EMOJIS: [&str; 20] = [
            "🎉", "✅", "🚀", "🙂", "😀", "😉", "⭐", "🔥", "👍", "👎", "💡", "❓", "❗", "📝",
            "🧠", "🧪", "📦", "🔧", "❤️", "❤",
        ];

        let mut i = 0;
        let chars: Vec<char> = text.chars().collect();
        while i < chars.len() {
            let mut matched: Option<&'static str> = None;
            for e in &EMOJIS {
                let ec = e.chars().next().unwrap();
                if chars[i] == ec {
                    matched = Some(*e);
                    break;
                }
            }
            if let Some(emoji) = matched {
                let handle = self.get_or_make_emoji_texture(ui, emoji);
                let sz = size * 1.2;
                ui.add(egui::Image::new(&handle).max_width(sz).max_height(sz));
                i += 1;
            } else {
                // Accumulate regular text until the next emoji
                let start = i;
                while i < chars.len() {
                    let c = chars[i];
                    if EMOJIS.iter().any(|e| e.chars().next().unwrap() == c) {
                        break;
                    }
                    i += 1;
                }
                if start < i {
                    let segment: String = chars[start..i].iter().collect();
                    let expanded = Self::expand_shortcodes(&segment);
                    if let Some(h) = self.highlight_phrase.borrow().as_ref() {
                        let lower = expanded.to_ascii_lowercase();
                        let mut start = 0usize;
                        while let Some(pos) = lower[start..].find(h) {
                            let abs = start + pos;
                            if abs > start {
                                let pre = &expanded[start..abs];
                                if !pre.is_empty() {
                                    let mut rich = RichText::new(pre).size(size);
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
                            }
                            let mat = &expanded[abs..abs + h.len()];
                            let mut rich = RichText::new(mat)
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
                            start = abs + h.len();
                        }
                        if start < expanded.len() {
                            let rest = &expanded[start..];
                            if !rest.is_empty() {
                                let mut rich = RichText::new(rest).size(size);
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
                        }
                    } else {
                        let mut rich = RichText::new(expanded).size(size);
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
                }
            }
        }
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
                        let galley =
                            fonts.layout_no_wrap(code.trim().to_string(), mono, Color32::WHITE);
                        width += galley.size().x + 6.0; // small padding for code background
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
        while i < bytes.len() {
            if bytes[i] == b':' {
                if let Some(end) = bytes[i + 1..].iter().position(|&b| b == b':') {
                    let code = &s[i..i + 2 + end];
                    if let Some(&e) = map.get(code) {
                        out.push_str(e);
                        i += end + 2;
                        continue;
                    }
                }
            }
            out.push(bytes[i] as char);
            i += 1;
        }
        out
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

            for (li, line) in lines.into_iter().enumerate() {
                ui.horizontal_wrapped(|ui| {
                    if li == 0 {
                        let marker = if ordered {
                            format!("{}.", index + 1)
                        } else {
                            "•".to_string()
                        };
                        ui.label(
                            RichText::new(format!("{} ", marker))
                                .size(self.font_sizes.body)
                                .color(Color32::from_rgb(180, 180, 180)),
                        );
                    } else {
                        ui.add_space(18.0);
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
                .replace(p.to_ascii_lowercase());
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
        let md = "- Use `code` and **bold** and *italic* and ~~strike~~\n";
        let parsed = renderer.parse(md).expect("parse ok");
        // Expect a single unordered list with one item
        assert_eq!(parsed.len(), 1);
        match &parsed[0] {
            MarkdownElement::List { ordered, items } => {
                assert!(!ordered);
                assert_eq!(items.len(), 1);
                let spans = &items[0];
                // Presence checks for various inline spans
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
        // Direct shortcode expansion
        assert_eq!(MarkdownRenderer::expand_shortcodes(":rocket:"), "🚀");
        assert_eq!(MarkdownRenderer::expand_shortcodes(":tada:"), "🎉");
        // Mixed text
        assert_eq!(
            MarkdownRenderer::expand_shortcodes("Hello :tada:!"),
            "Hello 🎉!"
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
        // Expect 4 headers with deduped slugs
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

        // Spot check key sections used by ToC
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
}
