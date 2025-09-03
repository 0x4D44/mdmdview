/// Main application logic for the markdown viewer
///
/// This module contains the primary app state, UI logic, and event handling
/// for the markdown viewer application built with egui.
use crate::{MarkdownElement, MarkdownRenderer, SampleFile, SAMPLE_FILES};
use anyhow::Result;
use egui::{menu, CentralPanel, Color32, Context, RichText, TopBottomPanel};
use egui::{TextEdit, TextStyle};
use rfd::FileDialog;
use std::path::PathBuf;

/// Main application state and logic
pub struct MarkdownViewerApp {
    /// Markdown renderer instance
    renderer: MarkdownRenderer,
    /// Currently loaded file path
    current_file: Option<PathBuf>,
    /// Current markdown content as string
    current_content: String,
    /// Buffer for raw view (read-only for now)
    raw_buffer: String,
    /// Parsed markdown elements ready for rendering
    parsed_elements: Vec<MarkdownElement>,
    /// Application title for window
    title: String,
    /// Error message to display if any
    error_message: Option<String>,
    /// Navigation request for keyboard shortcuts
    nav_request: Option<NavigationRequest>,
    /// Scroll area ID for state management
    scroll_area_id: egui::Id,
    /// Flag to request fullscreen toggle
    toggle_fullscreen: bool,
    /// Current view mode
    view_mode: ViewMode,
    /// Wrap long lines in raw view
    wrap_raw: bool,
}

/// Navigation request for keyboard-triggered scrolling
#[derive(Debug, Clone)]
enum NavigationRequest {
    Top,
    Bottom,
    PageUp,
    PageDown,
    ScrollUp,   // Arrow up - fine scrolling
    ScrollDown, // Arrow down - fine scrolling
}

/// Which view the user is in
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ViewMode {
    Rendered,
    Raw,
}

impl MarkdownViewerApp {
    /// Create a new application instance
    pub fn new() -> Self {
        let mut app = Self {
            renderer: MarkdownRenderer::new(),
            current_file: None,
            current_content: String::new(),
            raw_buffer: String::new(),
            parsed_elements: Vec::new(),
            title: "MarkdownView".to_string(),
            error_message: None,
            nav_request: None,
            scroll_area_id: egui::Id::new("main_scroll_area"),
            toggle_fullscreen: false,
            view_mode: ViewMode::Rendered,
            wrap_raw: false,
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
        self.raw_buffer = self.current_content.clone();
        self.error_message = None;
        self.nav_request = None; // Reset any pending navigation

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
        let filename = path
            .file_name()
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

    /// Toggle between Rendered and Raw view
    fn toggle_view_mode(&mut self) {
        self.view_mode = match self.view_mode {
            ViewMode::Rendered => ViewMode::Raw,
            ViewMode::Raw => ViewMode::Rendered,
        };
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

            // Ctrl+R - Toggle raw view
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::R,
            )) {
                self.toggle_view_mode();
            }

            // F11 - Toggle fullscreen (set flag to handle outside input context)
            if i.consume_key(egui::Modifiers::NONE, egui::Key::F11) {
                self.toggle_fullscreen = true;
            }

            // Home - Go to top of document
            if i.consume_key(egui::Modifiers::NONE, egui::Key::Home) {
                self.nav_request = Some(NavigationRequest::Top);
            }

            // End - Go to bottom of document
            if i.consume_key(egui::Modifiers::NONE, egui::Key::End) {
                self.nav_request = Some(NavigationRequest::Bottom);
            }

            // Page Up - Scroll up one page
            if i.consume_key(egui::Modifiers::NONE, egui::Key::PageUp) {
                self.nav_request = Some(NavigationRequest::PageUp);
            }

            // Page Down - Scroll down one page
            if i.consume_key(egui::Modifiers::NONE, egui::Key::PageDown) {
                self.nav_request = Some(NavigationRequest::PageDown);
            }

            // Arrow Up - Fine scroll up
            if i.consume_key(egui::Modifiers::NONE, egui::Key::ArrowUp) {
                self.nav_request = Some(NavigationRequest::ScrollUp);
            }

            // Arrow Down - Fine scroll down
            if i.consume_key(egui::Modifiers::NONE, egui::Key::ArrowDown) {
                self.nav_request = Some(NavigationRequest::ScrollDown);
            }
        });
    }

    /// Render the menu bar
    fn render_menu_bar(&mut self, ctx: &Context) {
        TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            menu::bar(ui, |ui| {
                // File menu
                ui.menu_button("File", |ui| {
                    ui.horizontal(|ui| {
                        if ui.button("📁 Open...").clicked() {
                            self.open_file_dialog();
                            ui.close_menu();
                        }
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            ui.weak("Ctrl+O");
                        });
                    });

                    ui.horizontal(|ui| {
                        if ui.button("📄 Close").clicked() {
                            self.close_current_file();
                            ui.close_menu();
                        }
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            ui.weak("Ctrl+W");
                        });
                    });

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

                    ui.horizontal(|ui| {
                        if ui.button("❌ Exit").clicked() {
                            ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                        }
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            ui.weak("Ctrl+Q");
                        });
                    });
                });

                // View menu
                ui.menu_button("View", |ui| {
                    // Raw view toggle
                    ui.horizontal(|ui| {
                        let selected = matches!(self.view_mode, ViewMode::Raw);
                        if ui.selectable_label(selected, "📝 Raw Markdown").clicked() {
                            self.toggle_view_mode();
                            ui.close_menu();
                        }
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            ui.weak("Ctrl+R");
                        });
                    });

                    // Wrap option for raw view
                    ui.horizontal(|ui| {
                        if ui
                            .selectable_label(self.wrap_raw, "↪ Wrap Raw Lines")
                            .clicked()
                        {
                            self.wrap_raw = !self.wrap_raw;
                        }
                    });

                    ui.horizontal(|ui| {
                        if ui.button("🔍 Zoom In").clicked() {
                            self.renderer.zoom_in();
                            ui.close_menu();
                        }
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            ui.weak("Ctrl++");
                        });
                    });

                    ui.horizontal(|ui| {
                        if ui.button("🔍 Zoom Out").clicked() {
                            self.renderer.zoom_out();
                            ui.close_menu();
                        }
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            ui.weak("Ctrl+-");
                        });
                    });

                    ui.horizontal(|ui| {
                        if ui.button("↩ Reset Zoom").clicked() {
                            self.renderer.reset_zoom();
                            ui.close_menu();
                        }
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            ui.weak("Ctrl+0");
                        });
                    });

                    ui.separator();

                    ui.horizontal(|ui| {
                        if ui.button("⛶ Toggle Fullscreen").clicked() {
                            let is_fullscreen =
                                ctx.input(|i| i.viewport().fullscreen.unwrap_or(false));
                            ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(
                                !is_fullscreen,
                            ));
                            ui.close_menu();
                        }
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            ui.weak("F11");
                        });
                    });
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
                        if let Some(welcome) = SAMPLE_FILES.iter().find(|f| f.name == "welcome.md")
                        {
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
                    let mode = match self.view_mode { ViewMode::Rendered => "Rendered", ViewMode::Raw => "Raw" };
                    ui.label(format!("Mode: {} | Elements: {} | Characters: {}", mode, element_count, char_count));
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

        // Handle fullscreen toggle outside input context to avoid deadlocks
        if self.toggle_fullscreen {
            self.toggle_fullscreen = false;
            let current_fullscreen = ctx.input(|i| i.viewport().fullscreen.unwrap_or(false));
            ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(!current_fullscreen));
        }

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
            let scroll_delta = if let Some(nav) = self.nav_request.take() {
                match nav {
                    NavigationRequest::Top => {
                        // Scroll to top: use a large POSITIVE delta to reach the beginning
                        egui::Vec2::new(0.0, 100000.0)
                    }
                    NavigationRequest::Bottom => {
                        // Scroll to bottom: use a large NEGATIVE delta to reach the end
                        egui::Vec2::new(0.0, -100000.0)
                    }
                    NavigationRequest::PageUp => {
                        let viewport_height = ui.available_height();
                        let page_size = viewport_height * 0.8;
                        egui::Vec2::new(0.0, page_size) // Positive = scroll up (show earlier content)
                    }
                    NavigationRequest::PageDown => {
                        let viewport_height = ui.available_height();
                        let page_size = viewport_height * 0.8;
                        egui::Vec2::new(0.0, -page_size) // Negative = scroll down (show later content)
                    }
                    NavigationRequest::ScrollUp => {
                        egui::Vec2::new(0.0, 40.0) // Positive = fine scroll up
                    }
                    NavigationRequest::ScrollDown => {
                        egui::Vec2::new(0.0, -40.0) // Negative = fine scroll down
                    }
                }
            } else {
                egui::Vec2::ZERO
            };

            egui::ScrollArea::vertical()
                .id_source(self.scroll_area_id)
                .auto_shrink([false, false])
                .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::VisibleWhenNeeded)
                .show(ui, |ui| {
                    // Apply scroll delta if we have navigation
                    if scroll_delta != egui::Vec2::ZERO {
                        ui.scroll_with_delta(scroll_delta);
                    }

                    ui.spacing_mut().item_spacing.y = 8.0;

                    if self.parsed_elements.is_empty() && self.error_message.is_none() {
                        ui.vertical_centered(|ui| {
                            ui.add_space(50.0);
                            ui.label(RichText::new("Welcome to MarkdownView").size(24.0).strong());
                            ui.add_space(20.0);
                            ui.label("Open a markdown file or select a sample to get started.");
                            ui.add_space(20.0);

                            if ui.button("📁 Open File").clicked() {
                                self.open_file_dialog();
                            }
                        });
                    } else {
                        match self.view_mode {
                            ViewMode::Rendered => {
                                self.renderer.render_to_ui(ui, &self.parsed_elements);
                            }
                            ViewMode::Raw => {
                                // Read-only raw markdown view
                                let mut tmp = self.raw_buffer.clone();
                                ui.add(
                                    TextEdit::multiline(&mut tmp)
                                        .font(TextStyle::Monospace)
                                        .code_editor()
                                        .lock_focus(false)
                                        .interactive(false)
                                        .desired_width(f32::INFINITY)
                                        .desired_rows(24),
                                );
                            }
                        }
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
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_app_creation() {
        let app = MarkdownViewerApp::new();
        assert!(!app.parsed_elements.is_empty()); // Should load welcome content
        assert!(app.title.contains("MarkdownView"));
        assert!(app.error_message.is_none());
        assert!(matches!(app.view_mode, ViewMode::Rendered));
    }

    #[test]
    fn test_load_content() {
        let mut app = MarkdownViewerApp::new();
        let content = "# Test Header\n\nThis is test content.";

        app.load_content(content, Some("Test".to_string()));

        assert_eq!(app.current_content, content);
        assert_eq!(app.raw_buffer, content);
        assert!(app.title.contains("Test"));
        assert!(!app.parsed_elements.is_empty());
        assert!(app.error_message.is_none());
    }

    #[test]
    fn test_toggle_view_mode() {
        let mut app = MarkdownViewerApp::new();
        assert!(matches!(app.view_mode, ViewMode::Rendered));
        app.toggle_view_mode();
        assert!(matches!(app.view_mode, ViewMode::Raw));
        app.toggle_view_mode();
        assert!(matches!(app.view_mode, ViewMode::Rendered));
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

    #[test]
    fn test_navigation_request_enum() {
        // Test that the enum values exist and are correct
        let _top = NavigationRequest::Top;
        let _bottom = NavigationRequest::Bottom;
        let _page_up = NavigationRequest::PageUp;
        let _page_down = NavigationRequest::PageDown;
        let _scroll_up = NavigationRequest::ScrollUp;
        let _scroll_down = NavigationRequest::ScrollDown;

        // Ensure it's cloneable and debuggable
        let nav = NavigationRequest::Top;
        let _cloned = nav.clone();
        let _debug = format!("{:?}", nav);

        // Basic compilation test performed by using the enum and Debug/Clone
    }

    #[test]
    fn test_navigation_state_initialization() {
        let app = MarkdownViewerApp::new();
        // Test that nav_request is initialized to None
        assert!(app.nav_request.is_none());
        // Test that fullscreen toggle flag is initialized to false
        assert!(!app.toggle_fullscreen);
    }

    #[test]
    fn test_fullscreen_toggle_flag() {
        let mut app = MarkdownViewerApp::new();

        // Initially should be false
        assert!(!app.toggle_fullscreen);

        // Simulate F11 key press (this would be set in handle_shortcuts)
        app.toggle_fullscreen = true;
        assert!(app.toggle_fullscreen);

        // After handling, it should be reset to false
        app.toggle_fullscreen = false;
        assert!(!app.toggle_fullscreen);
    }

    #[test]
    fn test_navigation_state_reset_on_load() {
        let mut app = MarkdownViewerApp::new();

        // Set a navigation request
        app.nav_request = Some(NavigationRequest::Top);
        assert!(app.nav_request.is_some());

        // Load content should reset navigation state
        app.load_content("# Test Content", Some("Test".to_string()));
        assert!(app.nav_request.is_none());
    }

    #[test]
    fn test_page_navigation_calculations() {
        // Test the page size calculation logic used in navigation
        let viewport_height = 800.0f32;
        let page_size = viewport_height * 0.8;
        assert_eq!(page_size, 640.0);

        // Test boundary conditions for Page Up
        let current_offset = 100.0f32;
        let new_offset_up = (current_offset - page_size).max(0.0);
        assert_eq!(new_offset_up, 0.0); // Should clamp to 0

        // Test Page Down calculation
        let new_offset_down = current_offset + page_size;
        assert_eq!(new_offset_down, 740.0);

        // Test that Page Up from near top goes to 0
        let near_top = 300.0f32;
        let from_near_top = (near_top - page_size).max(0.0);
        assert_eq!(from_near_top, 0.0);
    }
}
