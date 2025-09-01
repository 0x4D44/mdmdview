/// Markdown rendering engine for egui
/// 
/// This module handles parsing markdown content using pulldown-cmark
/// and rendering it as egui widgets with proper styling and formatting.

use egui::{Color32, RichText, Stroke};
use pulldown_cmark::{Event, Parser, Tag, Options, CodeBlockKind};
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::LinesWithEndings;

/// Main markdown renderer that converts markdown to egui elements
pub struct MarkdownRenderer {
    syntax_set: SyntaxSet,
    theme_set: ThemeSet,
    font_sizes: FontSizes,
}

/// Font size configuration for different markdown elements
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
            code: 13.0,
        }
    }
}

/// Represents a rendered markdown element
#[derive(Debug, Clone)]
pub enum MarkdownElement {
    Text(String),
    Header { level: u8, text: String },
    Code { language: Option<String>, text: String },
    InlineCode(String),
    Link { text: String, url: String },
    List { ordered: bool, items: Vec<String> },
    Quote(String),
    HorizontalRule,
    Table { headers: Vec<String>, rows: Vec<Vec<String>> },
}

impl MarkdownRenderer {
    /// Create a new markdown renderer with syntax highlighting support
    pub fn new() -> Self {
        Self {
            syntax_set: SyntaxSet::load_defaults_newlines(),
            theme_set: ThemeSet::load_defaults(),
            font_sizes: FontSizes::default(),
        }
    }

    /// Parse markdown content into a list of renderable elements
    pub fn parse(&self, markdown: &str) -> Result<Vec<MarkdownElement>, anyhow::Error> {
        let mut options = Options::empty();
        options.insert(Options::ENABLE_STRIKETHROUGH);
        options.insert(Options::ENABLE_TABLES);
        options.insert(Options::ENABLE_FOOTNOTES);
        options.insert(Options::ENABLE_TASKLISTS);

        let parser = Parser::new_ext(markdown, options);
        let mut elements = Vec::new();
        let mut current_text = String::new();
        let mut in_code_block = false;
        let mut code_language: Option<String> = None;
        let mut code_text = String::new();

        for event in parser {
            match event {
                Event::Start(Tag::Heading(_level, _, _)) => {
                    if !current_text.is_empty() {
                        elements.push(MarkdownElement::Text(current_text.clone()));
                        current_text.clear();
                    }
                }
                Event::End(Tag::Heading(level, _, _)) => {
                    elements.push(MarkdownElement::Header {
                        level: level as u8,
                        text: current_text.clone(),
                    });
                    current_text.clear();
                }
                Event::Start(Tag::CodeBlock(kind)) => {
                    in_code_block = true;
                    code_language = match kind {
                        CodeBlockKind::Fenced(lang) => {
                            if lang.is_empty() { None } else { Some(lang.to_string()) }
                        }
                        _ => None,
                    };
                }
                Event::End(Tag::CodeBlock(_)) => {
                    in_code_block = false;
                    elements.push(MarkdownElement::Code {
                        language: code_language.clone(),
                        text: code_text.clone(),
                    });
                    code_text.clear();
                    code_language = None;
                }
                Event::Text(text) => {
                    if in_code_block {
                        code_text.push_str(&text);
                    } else {
                        current_text.push_str(&text);
                    }
                }
                Event::Code(text) => {
                    if !current_text.is_empty() {
                        elements.push(MarkdownElement::Text(current_text.clone()));
                        current_text.clear();
                    }
                    elements.push(MarkdownElement::InlineCode(text.to_string()));
                }
                Event::Start(Tag::Link(_, url, _)) => {
                    // Store URL for when we get the link text
                    current_text.push_str(&format!("__LINK_START__{}__LINK_URL__", url));
                }
                Event::End(Tag::Link(_, _, _)) => {
                    // Extract link text and URL
                    if let Some(link_start) = current_text.find("__LINK_START__") {
                        let pre_link = &current_text[..link_start];
                        let link_part = &current_text[link_start + 14..]; // Skip "__LINK_START__"
                        
                        if let Some(url_marker) = link_part.find("__LINK_URL__") {
                            let url = &link_part[..url_marker];
                            let link_text = &link_part[url_marker + 12..]; // Skip "__LINK_URL__"
                            
                            if !pre_link.is_empty() {
                                elements.push(MarkdownElement::Text(pre_link.to_string()));
                            }
                            
                            elements.push(MarkdownElement::Link {
                                text: link_text.to_string(),
                                url: url.to_string(),
                            });
                            
                            current_text.clear();
                        }
                    }
                }
                Event::Rule => {
                    if !current_text.is_empty() {
                        elements.push(MarkdownElement::Text(current_text.clone()));
                        current_text.clear();
                    }
                    elements.push(MarkdownElement::HorizontalRule);
                }
                Event::HardBreak => {
                    current_text.push('\n');
                }
                Event::SoftBreak => {
                    current_text.push(' ');
                }
                _ => {} // Handle other events as needed
            }
        }

        // Add any remaining text
        if !current_text.is_empty() {
            elements.push(MarkdownElement::Text(current_text));
        }

        Ok(elements)
    }

    /// Render parsed markdown elements to egui UI
    pub fn render_to_ui(&self, ui: &mut egui::Ui, elements: &[MarkdownElement]) {
        for element in elements {
            match element {
                MarkdownElement::Text(text) => {
                    if !text.trim().is_empty() {
                        ui.label(RichText::new(text).size(self.font_sizes.body));
                        ui.add_space(4.0);
                    }
                }
                MarkdownElement::Header { level, text } => {
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
                    ui.label(
                        RichText::new(text)
                            .size(font_size)
                            .strong()
                            .color(Color32::from_rgb(220, 220, 220))
                    );
                    ui.add_space(6.0);
                }
                MarkdownElement::Code { language, text } => {
                    self.render_code_block(ui, language.as_deref(), text);
                }
                MarkdownElement::InlineCode(text) => {
                    ui.label(
                        RichText::new(text)
                            .size(self.font_sizes.code)
                            .family(egui::FontFamily::Monospace)
                            .background_color(Color32::from_rgb(50, 50, 50))
                    );
                }
                MarkdownElement::Link { text, url } => {
                    let link_response = ui.link(RichText::new(text).color(Color32::LIGHT_BLUE));
                    if link_response.clicked() {
                        self.open_url(url);
                    }
                }
                MarkdownElement::HorizontalRule => {
                    ui.add_space(8.0);
                    ui.separator();
                    ui.add_space(8.0);
                }
                _ => {
                    // Handle other elements (lists, tables, quotes) if needed
                    ui.label(format!("Unsupported element: {:?}", element));
                }
            }
        }
    }

    /// Render a code block with syntax highlighting
    fn render_code_block(&self, ui: &mut egui::Ui, language: Option<&str>, text: &str) {
        ui.add_space(6.0);
        
        egui::Frame::none()
            .fill(Color32::from_rgb(30, 30, 30))
            .stroke(Stroke::new(1.0, Color32::from_rgb(60, 60, 60)))
            .inner_margin(8.0)
            .show(ui, |ui| {
                if let Some(lang) = language {
                    if let Some(syntax) = self.syntax_set.find_syntax_by_extension(lang) {
                        self.render_highlighted_code(ui, syntax, text);
                        return;
                    }
                }
                
                // Fallback to plain text rendering
                ui.label(
                    RichText::new(text)
                        .family(egui::FontFamily::Monospace)
                        .size(self.font_sizes.code)
                        .color(Color32::from_rgb(220, 220, 220))
                );
            });
        
        ui.add_space(6.0);
    }

    /// Render code with syntax highlighting using syntect
    fn render_highlighted_code(&self, ui: &mut egui::Ui, syntax: &syntect::parsing::SyntaxReference, text: &str) {
        let theme = &self.theme_set.themes["base16-ocean.dark"];
        let mut highlighter = HighlightLines::new(syntax, theme);
        
        for line in LinesWithEndings::from(text) {
            let ranges = highlighter
                .highlight_line(line, &self.syntax_set)
                .unwrap_or_default();
            
            ui.horizontal_wrapped(|ui| {
                for (style, text) in ranges {
                    let color = Color32::from_rgb(
                        style.foreground.r,
                        style.foreground.g,
                        style.foreground.b,
                    );
                    
                    ui.label(
                        RichText::new(text)
                            .family(egui::FontFamily::Monospace)
                            .size(self.font_sizes.code)
                            .color(color)
                    );
                }
            });
        }
    }

    /// Open URL in default browser
    fn open_url(&self, url: &str) {
        #[cfg(target_os = "windows")]
        {
            let _ = std::process::Command::new("cmd")
                .args(&["/c", "start", url])
                .spawn();
        }
        
        #[cfg(not(target_os = "windows"))]
        {
            let _ = std::process::Command::new("xdg-open")
                .arg(url)
                .spawn();
        }
    }

    /// Get font sizes configuration
    pub fn font_sizes(&self) -> &FontSizes {
        &self.font_sizes
    }

    /// Update font sizes (for zoom functionality)
    pub fn set_font_sizes(&mut self, font_sizes: FontSizes) {
        self.font_sizes = font_sizes;
    }

    /// Increase all font sizes by a factor
    pub fn zoom_in(&mut self) {
        let factor = 1.1;
        self.font_sizes.body *= factor;
        self.font_sizes.h1 *= factor;
        self.font_sizes.h2 *= factor;
        self.font_sizes.h3 *= factor;
        self.font_sizes.h4 *= factor;
        self.font_sizes.h5 *= factor;
        self.font_sizes.h6 *= factor;
        self.font_sizes.code *= factor;
    }

    /// Decrease all font sizes by a factor
    pub fn zoom_out(&mut self) {
        let factor = 0.9;
        self.font_sizes.body *= factor;
        self.font_sizes.h1 *= factor;
        self.font_sizes.h2 *= factor;
        self.font_sizes.h3 *= factor;
        self.font_sizes.h4 *= factor;
        self.font_sizes.h5 *= factor;
        self.font_sizes.h6 *= factor;
        self.font_sizes.code *= factor;
    }

    /// Reset font sizes to default
    pub fn reset_zoom(&mut self) {
        self.font_sizes = FontSizes::default();
    }
}

impl Default for MarkdownRenderer {
    fn default() -> Self {
        Self::new()
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
    fn test_parse_simple_text() {
        let renderer = MarkdownRenderer::new();
        let markdown = "Hello, world!";
        let elements = renderer.parse(markdown).unwrap();
        
        assert_eq!(elements.len(), 1);
        if let MarkdownElement::Text(text) = &elements[0] {
            assert_eq!(text, "Hello, world!");
        } else {
            panic!("Expected text element");
        }
    }

    #[test]
    fn test_parse_headers() {
        let renderer = MarkdownRenderer::new();
        let markdown = "# Header 1\n## Header 2\n### Header 3";
        let elements = renderer.parse(markdown).unwrap();
        
        assert!(elements.len() >= 3);
        
        // Check for headers in the elements
        let headers: Vec<_> = elements.iter()
            .filter_map(|e| match e {
                MarkdownElement::Header { level, text } => Some((*level, text.as_str())),
                _ => None,
            })
            .collect();
        
        assert!(headers.iter().any(|(level, text)| *level == 1 && text.contains("Header 1")));
        assert!(headers.iter().any(|(level, text)| *level == 2 && text.contains("Header 2")));
        assert!(headers.iter().any(|(level, text)| *level == 3 && text.contains("Header 3")));
    }

    #[test]
    fn test_parse_code_block() {
        let renderer = MarkdownRenderer::new();
        let markdown = "```rust\nfn main() {\n    println!(\"Hello\");\n}\n```";
        let elements = renderer.parse(markdown).unwrap();
        
        let code_elements: Vec<_> = elements.iter()
            .filter_map(|e| match e {
                MarkdownElement::Code { language, text } => Some((language.as_ref(), text.as_str())),
                _ => None,
            })
            .collect();
        
        assert!(!code_elements.is_empty());
        let (lang, code) = code_elements[0];
        assert_eq!(lang, Some(&"rust".to_string()));
        assert!(code.contains("fn main()"));
        assert!(code.contains("println!"));
    }

    #[test]
    fn test_parse_inline_code() {
        let renderer = MarkdownRenderer::new();
        let markdown = "This is `inline code` in text.";
        let elements = renderer.parse(markdown).unwrap();
        
        let has_inline_code = elements.iter().any(|e| matches!(e, MarkdownElement::InlineCode(_)));
        assert!(has_inline_code);
    }

    #[test]
    fn test_parse_horizontal_rule() {
        let renderer = MarkdownRenderer::new();
        let markdown = "Text above\n\n---\n\nText below";
        let elements = renderer.parse(markdown).unwrap();
        
        let has_hr = elements.iter().any(|e| matches!(e, MarkdownElement::HorizontalRule));
        assert!(has_hr);
    }

    #[test]
    fn test_font_sizes_default() {
        let font_sizes = FontSizes::default();
        assert_eq!(font_sizes.body, 14.0);
        assert_eq!(font_sizes.h1, 28.0);
        assert_eq!(font_sizes.h2, 24.0);
        assert_eq!(font_sizes.code, 13.0);
    }

    #[test]
    fn test_zoom_functionality() {
        let mut renderer = MarkdownRenderer::new();
        let original_body_size = renderer.font_sizes.body;
        
        renderer.zoom_in();
        assert!(renderer.font_sizes.body > original_body_size);
        
        renderer.zoom_out();
        assert!(renderer.font_sizes.body < original_body_size * 1.1);
        
        renderer.reset_zoom();
        assert_eq!(renderer.font_sizes.body, FontSizes::default().body);
    }

    #[test]
    fn test_complex_markdown_parsing() {
        let renderer = MarkdownRenderer::new();
        let markdown = r#"# Title

This is **bold** and *italic* text.

```python
def hello():
    print("world")
```

- List item 1
- List item 2

> This is a quote

[Link text](https://example.com)

---

The end.
"#;
        
        let elements = renderer.parse(markdown).unwrap();
        assert!(!elements.is_empty());
        
        // Should contain various element types
        let has_header = elements.iter().any(|e| matches!(e, MarkdownElement::Header { .. }));
        let has_code = elements.iter().any(|e| matches!(e, MarkdownElement::Code { .. }));
        let has_hr = elements.iter().any(|e| matches!(e, MarkdownElement::HorizontalRule));
        
        assert!(has_header);
        assert!(has_code);
        assert!(has_hr);
    }

    #[test]
    fn test_empty_markdown() {
        let renderer = MarkdownRenderer::new();
        let elements = renderer.parse("").unwrap();
        assert!(elements.is_empty());
    }

    #[test]
    fn test_markdown_with_special_characters() {
        let renderer = MarkdownRenderer::new();
        let markdown = "Text with émojis 🎉 and spëcial chàracters";
        let elements = renderer.parse(markdown).unwrap();
        
        assert!(!elements.is_empty());
        if let MarkdownElement::Text(text) = &elements[0] {
            assert!(text.contains("émojis"));
            assert!(text.contains("🎉"));
            assert!(text.contains("spëcial"));
        }
    }

    #[test]
    fn test_nested_formatting() {
        let renderer = MarkdownRenderer::new();
        let markdown = "This has **bold with *italic* inside** text.";
        let elements = renderer.parse(markdown).unwrap();
        
        // Should successfully parse without panicking
        assert!(!elements.is_empty());
    }
}
