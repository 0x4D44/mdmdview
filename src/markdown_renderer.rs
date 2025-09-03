use anyhow::Result;
use egui::{Color32, RichText, Stroke};
use pulldown_cmark::{Event, LinkType, Options, Parser, Tag};
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::LinesWithEndings;

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
    Paragraph(Vec<InlineSpan>), // Changed: paragraphs contain inline spans
    Header {
        level: u8,
        spans: Vec<InlineSpan>,
    }, // Headers can have inline formatting too
    CodeBlock {
        language: Option<String>,
        text: String,
    },
    List {
        ordered: bool,
        items: Vec<Vec<InlineSpan>>,
    }, // List items are also inline spans
    Quote { depth: u8, lines: Vec<Vec<InlineSpan>> },
    HorizontalRule,
    Table {
        headers: Vec<String>,
        rows: Vec<Vec<String>>,
    },
}

/// Type alias for table parsing result to reduce complexity
type TableParseResult = (Vec<String>, Vec<Vec<String>>, usize);

/// Type alias for quote lines (each line is a sequence of inline spans)
type QuoteLines = Vec<Vec<InlineSpan>>;

/// Markdown renderer with proper inline element handling
pub struct MarkdownRenderer {
    font_sizes: FontSizes,
    syntax_set: SyntaxSet,
    theme_set: ThemeSet,
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
        while i < events.len() {
            i = self.parse_element(&events, i, &mut elements)?;
        }

        Ok(elements)
    }

    /// Parse a single element from the event stream
    fn parse_element(
        &self,
        events: &[Event],
        start: usize,
        elements: &mut Vec<MarkdownElement>,
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
                elements.push(MarkdownElement::Header {
                    level: *level as u8,
                    spans,
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
                    let (mut para, next) = self.parse_inline_spans_with_breaks(
                        events,
                        i + 1,
                        Tag::Paragraph,
                        true,
                    )?;
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
                Event::Text(text) => { text_buffer.push_str(text); i += 1; }
                Event::SoftBreak | Event::HardBreak => {
                    if keep_breaks {
                        if !text_buffer.is_empty() { spans.push(InlineSpan::Text(text_buffer.clone())); text_buffer.clear(); }
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
                    let (inner_text, next_i) = self
                        .collect_until_tag_end(events, i + 1, Tag::Strikethrough)?;
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
                        if i >= events.len() { break; }
                        match &events[i] {
                            Event::End(Tag::Item) => { i += 1; break; }
                            Event::Start(Tag::Paragraph) => {
                                let (ps, next) = self.parse_inline_spans(events, i + 1, Tag::Paragraph)?;
                                spans.extend(ps);
                                i = next;
                            }
                            Event::Start(Tag::List(child_first)) => {
                                let (child_items, next) = self.parse_list(events, i + 1, *child_first)?;
                                i = next;
                                let ordered_child = child_first.is_some();
                                for (idx, child) in child_items.into_iter().enumerate() {
                                    spans.push(InlineSpan::Text("\n".to_string()));
                                    let marker = if ordered_child { format!("{}.", idx + 1) } else { "•".to_string() };
                                    spans.push(InlineSpan::Text(format!("{} ", marker)));
                                    spans.extend(child);
                                }
                            }
                            Event::Text(t) => { spans.push(InlineSpan::Text(t.to_string())); i += 1; }
                            Event::SoftBreak | Event::HardBreak => { spans.push(InlineSpan::Text(" ".into())); i += 1; }
                            _ => { i += 1; }
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
        let mut headers: Vec<String> = Vec::new();
        let mut rows: Vec<Vec<String>> = Vec::new();
        let mut i = start;

        fn collect_cell_text(events: &[Event], mut j: usize) -> (String, usize) {
            let mut cell = String::new();
            while j < events.len() {
                match &events[j] {
                    Event::End(Tag::TableCell) => return (cell.trim().to_string(), j + 1),
                    Event::Text(t) | Event::Code(t) => cell.push_str(t),
                    Event::Start(Tag::Link(_, url, _)) => {
                        // capture link text if present
                        let mut k = j + 1;
                        let mut link_text = String::new();
                        while k < events.len() {
                            match &events[k] {
                                Event::End(Tag::Link(_, _, _)) => break,
                                Event::Text(t) | Event::Code(t) => link_text.push_str(t),
                                _ => {}
                            }
                            k += 1;
                        }
                        if !link_text.is_empty() { cell.push_str(&link_text); } else { cell.push_str(url.as_ref()); }
                        j = k;
                    }
                    _ => {}
                }
                j += 1;
            }
            (cell.trim().to_string(), j)
        }

        while i < events.len() {
            match &events[i] {
                Event::Start(Tag::TableHead) => {
                    i += 1;
                    while i < events.len() {
                        match &events[i] {
                            Event::Start(Tag::TableCell) => {
                                let (text, next) = collect_cell_text(events, i + 1);
                                headers.push(text);
                                i = next;
                            }
                            Event::End(Tag::TableHead) => { i += 1; break; }
                            _ => i += 1,
                        }
                    }
                }
                Event::Start(Tag::TableRow) => {
                    i += 1;
                    let mut row: Vec<String> = Vec::new();
                    while i < events.len() {
                        match &events[i] {
                            Event::Start(Tag::TableCell) => {
                                let (text, next) = collect_cell_text(events, i + 1);
                                row.push(text);
                                i = next;
                            }
                            Event::End(Tag::TableRow) => { i += 1; if !row.is_empty() { rows.push(row); } break; }
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
        for element in elements {
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
                MarkdownElement::Header { level, spans } => {
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
                    ui.horizontal_wrapped(|ui| {
                        // Avoid artificial gaps between header fragments
                        ui.spacing_mut().item_spacing.x = 0.0;
                        for span in spans {
                            self.render_inline_span(ui, span, Some(font_size), Some(true));
                        }
                    });
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
                let mut rich_text = RichText::new(fixed_text).size(size);
                if is_strong {
                    rich_text = rich_text.strong();
                }
                ui.label(rich_text);
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
                ui.label(
                    RichText::new(fixed_text)
                        .size(size)
                        .strong()
                        .color(Color32::from_rgb(220, 220, 220)),
                );
            }
            InlineSpan::Emphasis(text) => {
                let fixed_text = self.fix_unicode_chars(text);
                ui.label(
                    RichText::new(fixed_text)
                        .size(size)
                        .italics()
                        .color(Color32::from_rgb(220, 220, 220)),
                );
            }
            InlineSpan::Strikethrough(text) => {
                let fixed_text = self.fix_unicode_chars(text);
                ui.label(
                    RichText::new(fixed_text)
                        .size(size)
                        .strikethrough()
                        .color(Color32::from_rgb(220, 220, 220)),
                );
            }
            InlineSpan::Link { text, url } => {
                let fixed_text = self.fix_unicode_chars(text);
                let response = ui.link(RichText::new(fixed_text).color(Color32::LIGHT_BLUE).size(size));
                if response.clicked() {
                    self.open_url(url);
                }
            }
        }
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
                                lines.last_mut().unwrap().push(InlineSpan::Text(part.to_string()));
                            }
                            if pi < parts.len() - 1 { lines.push(Vec::new()); }
                        }
                    }
                    other => lines.last_mut().unwrap().push(other),
                }
            }

            for (li, line) in lines.into_iter().enumerate() {
                ui.horizontal_wrapped(|ui| {
                    if li == 0 {
                        let marker = if ordered { format!("{}.", index + 1) } else { "•".to_string() };
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

                                                    if style
                                                        .font_style
                                                        .contains(syntect::highlighting::FontStyle::BOLD)
                                                    {
                                                        rich_text = rich_text.strong();
                                                    }
                                                    if style
                                                        .font_style
                                                        .contains(syntect::highlighting::FontStyle::ITALIC)
                                                    {
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
    fn render_table(&self, ui: &mut egui::Ui, headers: &[String], rows: &[Vec<String>]) {
        if headers.is_empty() {
            return;
        }

        ui.add_space(8.0);
        egui::Frame::none()
            .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
            .show(ui, |ui| {
                egui::Grid::new("md_table").striped(true).show(ui, |ui| {
                    for h in headers {
                        ui.label(RichText::new(h).strong());
                    }
                    ui.end_row();

                    for row in rows {
                        for (col, cell) in row.iter().enumerate() {
                            if col < headers.len() {
                                ui.label(cell);
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
}
