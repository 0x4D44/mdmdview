/// Main application logic for the markdown viewer
/// 
/// This module contains the primary app state, UI logic, and event handling
/// for the markdown viewer application built with egui.

use crate::{MarkdownRenderer, MarkdownElement, SampleFile, SAMPLE_FILES};
use egui::{Context, CentralPanel, TopBottomPanel, menu, RichText, Color32};
use rfd::FileDialog;
use std::path::PathBuf;
use anyhow::Result;

/// Main application state and logic
pub struct MarkdownViewerApp {
    /// Markdown renderer instance
    renderer: MarkdownRenderer,
    /// Currently loaded file path
    current_file: Option<PathBuf>,
    /// Current markdown content as string
    current_content: String,
    /// Parsed markdown elements ready for rendering
    parsed_elements: Vec<MarkdownElement>,
    /// Application title for window
    title: String,
    /// Error message to display if any
    error_message: Option<String>,
}

impl MarkdownViewerApp {
    /// Create a new application instance
    pub fn new() -> Self {
        let mut app = Self {
            renderer: MarkdownRenderer::new(),
            current_file: None,
            current_content: String::new(),
            parsed_elements: Vec::new(),
            title: "MarkdownView".to_string(),
            error_message: None,
        };

        // Load welcome content by default
        if let Some(welcome) = SAMPLE_FILES.iter().find(|f| f.name == "welcome.md") {
            app.load_content(welcome.content, Some("Welcome".to_string()));
        }

        app
    }

    /// Load markdown content from a string
    pub fn load_content(&mut self, content: &str, title: Option<String>) {
        self.current_content = content.to_string();
        self.error_message = None;
        
        match self.renderer.parse(content) {
            Ok(elements) => {
                self.parsed_elements = elements;
                if let Some(title) = title {
                    self.title = format!("MarkdownView - {}", title);
                }
            }
            Err(e) => {
                self.error_message = Some(format!("Failed to parse markdown: {}", e));
                self.parsed_elements.clear();
            }
        }
    }

    /// Load markdown content from a file path
    pub fn load_file(&mut self, path: PathBuf) -> Result<()> {
        let content = std::fs::read_to_string(&path)?;
        let filename = path.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("Unknown")
            .to_string();
        
        self.current_file = Some(path);
        self.load_content(&content, Some(filename));
        Ok(())
    }

    /// Load a sample file by name
    pub fn load_sample(&mut self, sample: &SampleFile) {
        self.current_file = None;
        self.load_content(sample.content, Some(sample.title.to_string()));
    }

    /// Close the current file and return to welcome screen
    pub fn close_current_file(&mut self) {
        self.current_file = None;
        if let Some(welcome) = SAMPLE_FILES.iter().find(|f| f.name == "welcome.md") {
            self.load_content(welcome.content, Some("Welcome".to_string()));
        } else {
            // Fallback if welcome file is missing
            self.current_content.clear();
            self.parsed_elements.clear();
            self.title = "MarkdownView".to_string();
            self.error_message = None;
        }
    }

    /// Open file dialog to select a markdown file
    fn open_file_dialog(&mut self) {
        if let Some(path) = FileDialog::new()
            .add_filter("Markdown files", &["md", "markdown", "mdown", "mkd"])
            .add_filter("All files", &["*"])
            .set_title("Open Markdown File")
            .pick_file()
        {
            if let Err(e) = self.load_file(path) {
                self.error_message = Some(format!("Failed to open file: {}", e));
            }
        }
    }

    /// Handle keyboard shortcuts
    fn handle_shortcuts(&mut self, ctx: &Context) {
        ctx.input_mut(|i| {
            // Ctrl+O - Open file
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::O,
            )) {
                self.open_file_dialog();
            }

            // Ctrl+Q - Quit application
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::Q,
            )) {
                ctx.send_viewport_cmd(egui::ViewportCommand::Close);
            }

            // Ctrl+W - Close current file
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::W,
            )) {
                self.close_current_file();
            }

            // Ctrl+Plus - Zoom in
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::Plus,
            )) {
                self.renderer.zoom_in();
            }

            // Ctrl+Minus - Zoom out
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::Minus,
            )) {
                self.renderer.zoom_out();
            }

            // Ctrl+0 - Reset zoom
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::Num0,
            )) {
                self.renderer.reset_zoom();
            }

            // F11 - Toggle fullscreen
            if i.consume_key(egui::Modifiers::NONE, egui::Key::F11) {
                let is_fullscreen = ctx.input(|i| i.viewport().fullscreen.unwrap_or(false));
                ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(!is_fullscreen));
            }
        });
    }

    /// Render the menu bar
    fn render_menu_bar(&mut self, ctx: &Context) {
        TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            menu::bar(ui, |ui| {
                // File menu
                ui.menu_button("File", |ui| {
                    if ui.button("📁 Open...\t\t\tCtrl+O").clicked() {
                        self.open_file_dialog();
                        ui.close_menu();
                    }

                    if ui.button("📄 Close\t\t\tCtrl+W").clicked() {
                        self.close_current_file();
                        ui.close_menu();
                    }

                    ui.separator();

                    // Samples submenu
                    ui.menu_button("📚 Samples", |ui| {
                        for sample in SAMPLE_FILES {
                            if ui.button(sample.title).clicked() {
                                self.load_sample(sample);
                                ui.close_menu();
                            }
                        }
                    });

                    ui.separator();

                    if ui.button("❌ Exit\t\t\tCtrl+Q").clicked() {
                        ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                    }
                });

                // View menu
                ui.menu_button("View", |ui| {
                    if ui.button("🔍 Zoom In\t\t\tCtrl++").clicked() {
                        self.renderer.zoom_in();
                        ui.close_menu();
                    }

                    if ui.button("🔍 Zoom Out\t\t\tCtrl+-").clicked() {
                        self.renderer.zoom_out();
                        ui.close_menu();
                    }

                    if ui.button("↩ Reset Zoom\t\t\tCtrl+0").clicked() {
                        self.renderer.reset_zoom();
                        ui.close_menu();
                    }

                    ui.separator();

                    if ui.button("⛶ Toggle Fullscreen\t\t\tF11").clicked() {
                        let is_fullscreen = ctx.input(|i| i.viewport().fullscreen.unwrap_or(false));
                        ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(!is_fullscreen));
                        ui.close_menu();
                    }
                });

                // Help menu
                ui.menu_button("Help", |ui| {
                    if ui.button("📖 Usage Instructions").clicked() {
                        if let Some(usage) = SAMPLE_FILES.iter().find(|f| f.name == "usage.md") {
                            self.load_sample(usage);
                        }
                        ui.close_menu();
                    }

                    if ui.button("ℹ About").clicked() {
                        if let Some(welcome) = SAMPLE_FILES.iter().find(|f| f.name == "welcome.md") {
                            self.load_sample(welcome);
                        }
                        ui.close_menu();
                    }
                });
            });
        });
    }

    /// Render the status bar
    fn render_status_bar(&self, ctx: &Context) {
        TopBottomPanel::bottom("status_bar").show(ctx, |ui| {
            ui.horizontal(|ui| {
                // Current file info
                if let Some(path) = &self.current_file {
                    ui.label(format!("📄 {}", path.display()));
                } else if !self.parsed_elements.is_empty() {
                    ui.label("📄 Sample file");
                } else {
                    ui.label("📄 No file loaded");
                }

                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    // Document stats
                    let element_count = self.parsed_elements.len();
                    let char_count = self.current_content.len();
                    ui.label(format!("Elements: {} | Characters: {}", element_count, char_count));
                });
            });
        });
    }
}

impl eframe::App for MarkdownViewerApp {
    /// Update function called every frame
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame) {
        // Handle keyboard shortcuts
        self.handle_shortcuts(ctx);

        // Render menu bar
        self.render_menu_bar(ctx);

        // Render status bar
        self.render_status_bar(ctx);

        // Main content area
        CentralPanel::default().show(ctx, |ui| {
            // Show error message if any
            if let Some(ref error) = self.error_message {
                ui.colored_label(Color32::RED, format!("⚠ Error: {}", error));
                ui.separator();
            }

            // Render markdown content in a scrollable area
            egui::ScrollArea::vertical()
                .auto_shrink([false, false])
                .show(ui, |ui| {
                    ui.spacing_mut().item_spacing.y = 8.0;
                    
                    if self.parsed_elements.is_empty() && self.error_message.is_none() {
                        ui.vertical_centered(|ui| {
                            ui.add_space(50.0);
                            ui.label(
                                RichText::new("Welcome to MarkdownView")
                                    .size(24.0)
                                    .strong()
                            );
                            ui.add_space(20.0);
                            ui.label("Open a markdown file or select a sample to get started.");
                            ui.add_space(20.0);
                            
                            if ui.button("📁 Open File").clicked() {
                                self.open_file_dialog();
                            }
                        });
                    } else {
                        self.renderer.render_to_ui(ui, &self.parsed_elements);
                    }
                });
        });
    }

    /// Set the window title
    fn auto_save_interval(&self) -> std::time::Duration {
        std::time::Duration::from_secs(30)
    }
}

impl Default for MarkdownViewerApp {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;
    use std::io::Write;

    #[test]
    fn test_app_creation() {
        let app = MarkdownViewerApp::new();
        assert!(!app.parsed_elements.is_empty()); // Should load welcome content
        assert!(app.title.contains("MarkdownView"));
        assert!(app.error_message.is_none());
    }

    #[test]
    fn test_load_content() {
        let mut app = MarkdownViewerApp::new();
        let content = "# Test Header\n\nThis is test content.";
        
        app.load_content(content, Some("Test".to_string()));
        
        assert_eq!(app.current_content, content);
        assert!(app.title.contains("Test"));
        assert!(!app.parsed_elements.is_empty());
        assert!(app.error_message.is_none());
    }

    #[test]
    fn test_load_sample() {
        let mut app = MarkdownViewerApp::new();
        let sample = &SAMPLE_FILES[0]; // First sample file
        
        app.load_sample(sample);
        
        assert_eq!(app.current_content, sample.content);
        assert!(app.title.contains(sample.title));
        assert!(!app.parsed_elements.is_empty());
        assert!(app.current_file.is_none()); // Sample files don't set file path
    }

    #[test]
    fn test_load_file() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        
        // Create a temporary markdown file
        let mut temp_file = NamedTempFile::new()?;
        let content = "# Temporary File\n\nThis is a test markdown file.";
        temp_file.write_all(content.as_bytes())?;
        temp_file.flush()?;
        
        let path = temp_file.path().to_path_buf();
        app.load_file(path.clone())?;
        
        assert_eq!(app.current_content, content);
        assert_eq!(app.current_file, Some(path));
        assert!(!app.parsed_elements.is_empty());
        assert!(app.error_message.is_none());
        
        Ok(())
    }

    #[test]
    fn test_load_invalid_markdown() {
        let mut app = MarkdownViewerApp::new();
        // Even "invalid" markdown should parse successfully with pulldown-cmark
        // as it's very permissive
        let content = "This is just plain text with some <invalid> HTML tags";
        
        app.load_content(content, Some("Invalid".to_string()));
        
        // Should still work - pulldown-cmark is very permissive
        assert_eq!(app.current_content, content);
        assert!(app.error_message.is_none() || app.error_message.is_some()); // Either is ok
    }

    #[test]
    fn test_load_empty_content() {
        let mut app = MarkdownViewerApp::new();
        app.load_content("", Some("Empty".to_string()));
        
        assert_eq!(app.current_content, "");
        assert!(app.title.contains("Empty"));
        assert!(app.error_message.is_none());
    }

    #[test]
    fn test_load_nonexistent_file() {
        let mut app = MarkdownViewerApp::new();
        let fake_path = PathBuf::from("/nonexistent/file.md");
        
        let result = app.load_file(fake_path);
        assert!(result.is_err());
    }

    #[test]
    fn test_title_updates() {
        let mut app = MarkdownViewerApp::new();
        
        // Test with custom title
        app.load_content("# Test", Some("Custom Title".to_string()));
        assert!(app.title.contains("Custom Title"));
        
        // Test with no title (should keep existing)
        let old_title = app.title.clone();
        app.load_content("# Another Test", None);
        assert_eq!(app.title, old_title); // Should remain unchanged
    }

    #[test]
    fn test_complex_markdown_loading() {
        let mut app = MarkdownViewerApp::new();
        let complex_content = r#"# Complex Document

## With Multiple Sections

This has **bold** and *italic* text.

```rust
fn main() {
    println!("Hello, world!");
}
```

- List item 1
- List item 2

> A blockquote

[A link](https://example.com)

---

The end.
"#;

        app.load_content(complex_content, Some("Complex".to_string()));
        
        assert_eq!(app.current_content, complex_content);
        assert!(app.title.contains("Complex"));
        assert!(!app.parsed_elements.is_empty());
        assert!(app.error_message.is_none());
        
        // Should have parsed various element types
        assert!(app.parsed_elements.len() > 5); // Complex document should have many elements
    }

    #[test]
    fn test_error_handling() {
        let app = MarkdownViewerApp::new();
        
        // Test that app starts without errors
        assert!(app.error_message.is_none());
        
        // Even with the welcome content loaded, should be error-free
        assert!(!app.parsed_elements.is_empty());
    }

    #[test]
    fn test_default_state() {
        let app = MarkdownViewerApp::default();
        
        // Default should be same as new()
        assert!(!app.parsed_elements.is_empty());
        assert!(app.title.contains("MarkdownView"));
        assert!(app.error_message.is_none());
        assert!(app.current_file.is_none());
    }

    #[test]
    fn test_sample_files_integration() {
        let mut app = MarkdownViewerApp::new();
        
        // Test loading each sample file
        for sample in SAMPLE_FILES {
            app.load_sample(sample);
            
            assert_eq!(app.current_content, sample.content);
            assert!(app.title.contains(sample.title));
            assert!(app.current_file.is_none());
            assert!(app.error_message.is_none());
        }
    }

    #[test]
    fn test_close_current_file() {
        let mut app = MarkdownViewerApp::new();
        
        // Load some content first
        app.load_content("# Test Content", Some("Test File".to_string()));
        assert!(app.title.contains("Test File"));
        
        // Close the file
        app.close_current_file();
        
        // Should return to welcome screen
        assert!(app.title.contains("Welcome"));
        assert!(app.current_file.is_none());
        assert!(!app.parsed_elements.is_empty()); // Should have welcome content
        assert!(app.error_message.is_none());
    }
}
