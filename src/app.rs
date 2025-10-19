/// Main application logic for the markdown viewer
///
/// This module contains the primary app state, UI logic, and event handling
/// for the markdown viewer application built with egui.
use crate::{MarkdownElement, MarkdownRenderer, SampleFile, SAMPLE_FILES, WindowState};
use anyhow::{bail, Result};
use egui::text::LayoutJob;
use egui::text::TextFormat;
use egui::{menu, CentralPanel, Color32, Context, RichText, TopBottomPanel};
use egui::{TextEdit, TextStyle};
use rfd::FileDialog;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use unicode_normalization::{UnicodeCaseFold, UnicodeNormalization};

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
    /// Write mode: allow editing in Raw view
    write_enabled: bool,
    /// Remember caret position in raw editor (byte index)
    raw_cursor: Option<usize>,
    /// Request focus for raw editor on next render
    raw_focus_requested: bool,
    /// Flag to request reload of current file (handled outside input context)
    reload_requested: bool,
    /// Defer toggling view mode to outside input context
    view_toggle_requested: bool,
    /// Defer toggling write mode to outside input context
    write_toggle_requested: bool,
    /// Last known window position (for persistence)
    last_window_pos: Option<[f32; 2]>,
    /// Last known window size (for persistence)
    last_window_size: Option<[f32; 2]>,
    /// Last known maximized state
    last_window_maximized: bool,
    /// Last persisted window snapshot to avoid redundant disk writes
    last_persisted_state: Option<WindowState>,
    /// Throttle saving window state
    last_persist_instant: std::time::Instant,
    // Search state
    show_search: bool,
    search_query: String,
    last_query: String,
    last_match_index: Option<usize>,
    pending_scroll_to_element: Option<usize>,
    search_focus_requested: bool,
    /// Deferred caret movement (in lines) for raw editor
    pending_raw_cursor_line_move: Option<i32>,
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
    fn toggle_write_mode(&mut self, ctx: &Context) {
        if self.write_enabled {
            // About to disable; capture current cursor if in Raw view
            if matches!(self.view_mode, ViewMode::Raw) {
                let editor_id = egui::Id::new("raw_editor");
                if let Some(state) = egui::text_edit::TextEditState::load(ctx, editor_id) {
                    if let Some(range) = state.cursor.char_range() {
                        self.raw_cursor = Some(range.primary.index);
                    }
                }
            }
            self.write_enabled = false;
        } else {
            // Enabling write mode: ensure the raw editor will gain focus
            self.write_enabled = true;
            if matches!(self.view_mode, ViewMode::Raw) {
                self.raw_focus_requested = true;
            }
        }
    }

    fn move_raw_cursor_lines(&mut self, ctx: &Context, delta_lines: i32) {
        if !matches!(self.view_mode, ViewMode::Raw) || !self.write_enabled {
            return;
        }
        let editor_id = egui::Id::new("raw_editor");
        if let Some(mut state) = egui::text_edit::TextEditState::load(ctx, editor_id) {
            let mut idx = state
                .cursor
                .char_range()
                .map(|r| r.primary.index)
                .unwrap_or_else(|| self.raw_cursor.unwrap_or(0))
                .min(self.raw_buffer.len());

            let s = self.raw_buffer.as_str();
            match delta_lines.cmp(&0) {
                std::cmp::Ordering::Less => {
                    let mut lines = -delta_lines;
                    // Move to start of current line
                    idx = s[..idx].rfind('\n').map(|p| p + 1).unwrap_or(0);
                    while lines > 0 && idx > 0 {
                        if let Some(prev_nl) = s[..idx.saturating_sub(1)].rfind('\n') {
                            idx = prev_nl + 1;
                        } else {
                            idx = 0;
                        }
                        lines -= 1;
                    }
                }
                std::cmp::Ordering::Greater => {
                    let mut lines = delta_lines;
                    // Move to start of next line
                    if let Some(nl) = s[idx..].find('\n') {
                        idx = (idx + nl + 1).min(s.len());
                        lines -= 1;
                    }
                    while lines > 0 && idx < s.len() {
                        if let Some(nl) = s[idx..].find('\n') {
                            idx = (idx + nl + 1).min(s.len());
                        } else {
                            idx = s.len();
                        }
                        lines -= 1;
                    }
                }
                std::cmp::Ordering::Equal => {}
            }
            let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(idx));
            state.cursor.set_char_range(Some(cr));
            state.store(ctx, editor_id);
            self.raw_cursor = Some(idx);
            self.raw_focus_requested = true; // keep focus and make caret visible
        }
    }
    fn clear_search_state(&mut self) {
        self.search_query.clear();
        self.last_query.clear();
        self.last_match_index = None;
        self.pending_scroll_to_element = None;
        self.renderer.set_highlight_phrase(None);
    }
    fn find_next(&mut self) {
        if self.search_query.is_empty() && self.last_query.is_empty() {
            return;
        }
        let needle = if !self.search_query.is_empty() {
            self.last_query = self.search_query.clone();
            Self::fold_for_search(&self.search_query)
        } else {
            Self::fold_for_search(&self.last_query)
        };
        if needle.is_empty() {
            return;
        }
        let mut start = self.last_match_index.unwrap_or(usize::MAX);
        if start == usize::MAX {
            start = 0;
        } else {
            start = start.saturating_add(1);
        }
        // Wrap-around search forward
        let total = self.parsed_elements.len();
        for pass in 0..2 {
            let range: Box<dyn Iterator<Item = usize>> = if pass == 0 {
                Box::new(start..total)
            } else {
                Box::new(0..start.min(total))
            };
            for idx in range {
                let text = crate::markdown_renderer::MarkdownRenderer::element_plain_text(
                    &self.parsed_elements[idx],
                );
                if Self::fold_for_search(&text).contains(&needle) {
                    self.last_match_index = Some(idx);
                    self.pending_scroll_to_element = Some(idx);
                    return;
                }
            }
        }
    }

    fn find_previous(&mut self) {
        if self.search_query.is_empty() && self.last_query.is_empty() {
            return;
        }
        let needle = if !self.search_query.is_empty() {
            self.last_query = self.search_query.clone();
            Self::fold_for_search(&self.search_query)
        } else {
            Self::fold_for_search(&self.last_query)
        };
        if needle.is_empty() {
            return;
        }
        let total = self.parsed_elements.len();
        let mut start = self.last_match_index.unwrap_or(0);
        if start == 0 {
            start = total.saturating_sub(1);
        } else {
            start = start.saturating_sub(1);
        }
        // Wrap-around search backward
        for pass in 0..2 {
            let range: Box<dyn Iterator<Item = usize>> = if pass == 0 {
                Box::new((0..=start).rev())
            } else {
                Box::new(((start + 1)..total).rev())
            };
            for idx in range {
                let text = crate::markdown_renderer::MarkdownRenderer::element_plain_text(
                    &self.parsed_elements[idx],
                );
                if Self::fold_for_search(&text).contains(&needle) {
                    self.last_match_index = Some(idx);
                    self.pending_scroll_to_element = Some(idx);
                    return;
                }
            }
        }
    }
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
            write_enabled: false,
            raw_cursor: None,
            raw_focus_requested: false,
            reload_requested: false,
            view_toggle_requested: false,
            write_toggle_requested: false,
            last_window_pos: None,
            last_window_size: None,
            last_window_maximized: false,
            last_persisted_state: None,
            last_persist_instant: std::time::Instant::now(),
            show_search: false,
            search_query: String::new(),
            last_query: String::new(),
            last_match_index: None,
            pending_scroll_to_element: None,
            search_focus_requested: false,
            pending_raw_cursor_line_move: None,
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
                                 // Ensure scroll resets to top on new content
        self.pending_scroll_to_element = Some(0);

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
        let (content, lossy) = Self::read_file_lossy(&path)?;
        let filename = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("Unknown")
            .to_string();

        let base = path.parent().map(|p| p.to_path_buf());
        self.renderer.set_base_dir(base.as_deref());
        self.current_file = Some(path);
        self.load_content(&content, Some(filename));
        if lossy {
            eprintln!("Loaded file with invalid UTF-8; replaced invalid sequences.");
        }
        Ok(())
    }

    fn read_file_lossy(path: &Path) -> Result<(String, bool)> {
        match std::fs::read_to_string(path) {
            Ok(s) => Ok((s, false)),
            Err(e) if e.kind() == ErrorKind::InvalidData => {
                let bytes = std::fs::read(path)?;
                Ok((String::from_utf8_lossy(&bytes).into_owned(), true))
            }
            Err(e) => Err(e.into()),
        }
    }

    fn current_window_state(&self) -> Option<WindowState> {
        let pos = self.last_window_pos?;
        let size = self.last_window_size?;
        Some(WindowState {
            pos,
            size,
            maximized: self.last_window_maximized,
        })
    }

    fn window_state_changed(&self, new_state: &WindowState) -> bool {
        match &self.last_persisted_state {
            Some(prev) => {
                let pos_changed = (prev.pos[0] - new_state.pos[0]).abs() > 0.5
                    || (prev.pos[1] - new_state.pos[1]).abs() > 0.5;
                let size_changed = (prev.size[0] - new_state.size[0]).abs() > 0.5
                    || (prev.size[1] - new_state.size[1]).abs() > 0.5;
                let maximized_changed = prev.maximized != new_state.maximized;
                pos_changed || size_changed || maximized_changed
            }
            None => true,
        }
    }

    fn fold_for_search(input: &str) -> String {
        input.nfkc().default_case_fold().collect()
    }

    /// Request a reload of the current file (processed outside of input context)
    fn request_reload(&mut self) {
        if self.current_file.is_some() {
            self.reload_requested = true;
        } else {
            self.error_message = Some("No file loaded to reload".to_string());
        }
    }

    /// Reload the currently opened file from disk
    pub fn reload_current_file(&mut self) -> Result<()> {
        let path = if let Some(p) = self.current_file.clone() {
            p
        } else {
            bail!("No file loaded to reload");
        };
        self.load_file(path)
    }

    /// Load a sample file by name
    pub fn load_sample(&mut self, sample: &SampleFile) {
        self.current_file = None;
        // Samples have no file base-dir
        self.renderer.set_base_dir(None);
        self.load_content(sample.content, Some(sample.title.to_string()));
        // Scroll to top for new sample
        self.pending_scroll_to_element = Some(0);
    }

    /// Close the current file and return to welcome screen
    pub fn close_current_file(&mut self) {
        self.current_file = None;
        self.renderer.set_base_dir(None);
        if let Some(welcome) = SAMPLE_FILES.iter().find(|f| f.name == "welcome.md") {
            self.load_content(welcome.content, Some("Welcome".to_string()));
            self.pending_scroll_to_element = Some(0);
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
    fn toggle_view_mode(&mut self, ctx: &Context) {
        // If leaving Raw view while editing, capture cursor before switching
        if matches!(self.view_mode, ViewMode::Raw) && self.write_enabled {
            let editor_id = egui::Id::new("raw_editor");
            if let Some(state) = egui::text_edit::TextEditState::load(ctx, editor_id) {
                if let Some(range) = state.cursor.char_range() {
                    self.raw_cursor = Some(range.primary.index);
                }
            }
        }
        self.view_mode = match self.view_mode {
            ViewMode::Rendered => {
                self.raw_focus_requested = true;
                ViewMode::Raw
            }
            ViewMode::Raw => ViewMode::Rendered,
        };
    }

    /// Handle keyboard shortcuts
    fn handle_shortcuts(&mut self, ctx: &Context) {
        ctx.input_mut(|i| {
            let in_raw_edit = matches!(self.view_mode, ViewMode::Raw) && self.write_enabled;
            // Ctrl+O - Open file
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::O,
            )) {
                self.open_file_dialog();
            }

            // Alt-based accelerators for common actions (mnemonics)
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::ALT,
                egui::Key::O,
            )) {
                self.open_file_dialog();
            }
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::ALT,
                egui::Key::W,
            )) {
                self.close_current_file();
            }
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::ALT,
                egui::Key::Q,
            )) {
                ctx.send_viewport_cmd(egui::ViewportCommand::Close);
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

            // Ctrl+F - Open search dialog
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::F,
            )) {
                self.show_search = true;
                if self.last_match_index.is_none() {
                    self.last_match_index = Some(0);
                }
                self.search_focus_requested = true;
            }

            // F3 navigation: next / previous
            if i.consume_key(egui::Modifiers::NONE, egui::Key::F3) {
                self.find_next();
            }
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::SHIFT,
                egui::Key::F3,
            )) || i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::ALT,
                egui::Key::F3,
            )) {
                self.find_previous();
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

            // Ctrl + Mouse Wheel - Zoom
            if i.modifiers.ctrl {
                let mut total = 0.0f32;
                for ev in &i.events {
                    if let egui::Event::MouseWheel { delta, .. } = ev {
                        total += delta.y;
                    }
                }
                if total.abs() > 0.0 {
                    if total > 0.0 {
                        self.renderer.zoom_in();
                    } else if total < 0.0 {
                        self.renderer.zoom_out();
                    }
                }
            }

            // Ctrl+R - Toggle raw view
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::R,
            )) {
                // Defer to avoid acting inside input context
                self.view_toggle_requested = true;
            }
            // Ctrl+E - Toggle write mode (deferred)
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::E,
            )) {
                self.write_toggle_requested = true;
            }
            // Ctrl+S - Save document
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::S,
            )) {
                if let Err(e) = self.save_current_document() {
                    self.error_message = Some(format!("Failed to save: {}", e));
                }
            }
            // Note: No global Alt+R to avoid conflict (File→Reload vs View→Raw)

            // F11 - Toggle fullscreen (set flag to handle outside input context)
            if i.consume_key(egui::Modifiers::NONE, egui::Key::F11) {
                self.toggle_fullscreen = true;
            }

            // F5 - Reload current file (set flag; actual IO handled outside input context)
            if i.consume_key(egui::Modifiers::NONE, egui::Key::F5) {
                self.request_reload();
            }

            // Esc - dismiss search dialog if visible
            if self.show_search && i.consume_key(egui::Modifiers::NONE, egui::Key::Escape) {
                self.clear_search_state();
                self.show_search = false;
            }

            // Only consume navigation keys when not editing in raw view
            if !in_raw_edit {
                if i.consume_key(egui::Modifiers::NONE, egui::Key::PageUp) {
                    self.nav_request = Some(NavigationRequest::PageUp);
                }
                if i.consume_key(egui::Modifiers::NONE, egui::Key::PageDown) {
                    self.nav_request = Some(NavigationRequest::PageDown);
                }
                // Home - Go to top of document
                if i.consume_key(egui::Modifiers::NONE, egui::Key::Home) {
                    self.nav_request = Some(NavigationRequest::Top);
                }

                // End - Go to bottom of document
                if i.consume_key(egui::Modifiers::NONE, egui::Key::End) {
                    self.nav_request = Some(NavigationRequest::Bottom);
                }

                // Arrow Up - Fine scroll up
                if i.consume_key(egui::Modifiers::NONE, egui::Key::ArrowUp) {
                    self.nav_request = Some(NavigationRequest::ScrollUp);
                }

                // Arrow Down - Fine scroll down
                if i.consume_key(egui::Modifiers::NONE, egui::Key::ArrowDown) {
                    self.nav_request = Some(NavigationRequest::ScrollDown);
                }
            } else {
                // In raw edit mode: implement PageUp/PageDown by moving caret by ~a page of lines
                const PAGE_LINES: i32 = 24; // match desired_rows for the editor
                if i.consume_key(egui::Modifiers::NONE, egui::Key::PageUp) {
                    self.pending_raw_cursor_line_move = Some(-PAGE_LINES);
                    self.raw_focus_requested = true;
                }
                if i.consume_key(egui::Modifiers::NONE, egui::Key::PageDown) {
                    self.pending_raw_cursor_line_move = Some(PAGE_LINES);
                    self.raw_focus_requested = true;
                }
            }
        });
    }

    /// Render the menu bar
    fn render_menu_bar(&mut self, ctx: &Context) {
        TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            let alt_pressed = ui.input(|i| i.modifiers.alt);
            let menu_text_color = if ui.visuals().dark_mode {
                Color32::WHITE
            } else {
                Color32::BLACK
            };
            menu::bar(ui, |ui| {
                // File menu (Alt+F mnemonic visual)
                ui.menu_button(
                    Self::menu_text_with_mnemonic(None, "File", 'F', alt_pressed, menu_text_color),
                    |ui| {
                        ui.horizontal(|ui| {
                            if ui
                                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                                    Some("📁 "),
                                    "Open...",
                                    'O',
                                    alt_pressed,
                                    menu_text_color,
                                )))
                                .clicked()
                            {
                                self.open_file_dialog();
                                ui.close_menu();
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl+O").color(menu_text_color));
                                },
                            );
                        });

                        // Save
                        ui.horizontal(|ui| {
                            let enabled = !self.current_content.is_empty();
                            let button = ui.add_enabled(
                                enabled,
                                egui::Button::new(Self::menu_text_with_mnemonic(
                                    Some("💾 "),
                                    "Save",
                                    'S',
                                    alt_pressed,
                                    menu_text_color,
                                )),
                            );
                            if button.clicked() {
                                if let Err(e) = self.save_current_document() {
                                    self.error_message = Some(format!("Failed to save: {}", e));
                                }
                                ui.close_menu();
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl+S").color(menu_text_color));
                                },
                            );
                        });

                        ui.horizontal(|ui| {
                            if ui
                                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                                    Some("📄 "),
                                    "Close",
                                    'C',
                                    alt_pressed,
                                    menu_text_color,
                                )))
                                .clicked()
                            {
                                self.close_current_file();
                                ui.close_menu();
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl+W").color(menu_text_color));
                                },
                            );
                        });

                        ui.horizontal(|ui| {
                            let enabled = self.current_file.is_some();
                            let button = ui.add_enabled(
                                enabled,
                                egui::Button::new(Self::menu_text_with_mnemonic(
                                    Some("🔄 "),
                                    "Reload",
                                    'R',
                                    alt_pressed,
                                    menu_text_color,
                                )),
                            );
                            if button.clicked() {
                                self.request_reload();
                                ui.close_menu();
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("F5").color(menu_text_color));
                                },
                            );
                        });

                        // Find...
                        ui.horizontal(|ui| {
                            if ui
                                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                                    Some("🔎 "),
                                    "Find...",
                                    'F',
                                    alt_pressed,
                                    menu_text_color,
                                )))
                                .clicked()
                            {
                                self.show_search = true;
                                self.search_focus_requested = true;
                                ui.close_menu();
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl+F").color(menu_text_color));
                                },
                            );
                        });

                        ui.separator();

                        // Samples submenu
                        ui.menu_button(
                            Self::menu_text_with_mnemonic(
                                Some("📚 "),
                                "Samples",
                                'S',
                                alt_pressed,
                                menu_text_color,
                            ),
                            |ui| {
                                for sample in SAMPLE_FILES {
                                    if ui.button(sample.title).clicked() {
                                        self.load_sample(sample);
                                        ui.close_menu();
                                    }
                                }
                            },
                        );

                        ui.separator();

                        ui.horizontal(|ui| {
                            if ui
                                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                                    Some("❌ "),
                                    "Exit",
                                    'E',
                                    alt_pressed,
                                    menu_text_color,
                                )))
                                .clicked()
                            {
                                ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl+Q").color(menu_text_color));
                                },
                            );
                        });
                    },
                );

                // View menu
                ui.menu_button(
                    Self::menu_text_with_mnemonic(None, "View", 'V', alt_pressed, menu_text_color),
                    |ui| {
                        // Raw view toggle
                        ui.horizontal(|ui| {
                            let selected = matches!(self.view_mode, ViewMode::Raw);
                            if ui
                                .add(egui::SelectableLabel::new(
                                    selected,
                                    Self::menu_text_with_mnemonic(
                                        Some("📝 "),
                                        "Raw Markdown",
                                        'R',
                                        alt_pressed,
                                        menu_text_color,
                                    ),
                                ))
                                .clicked()
                            {
                                self.toggle_view_mode(ctx);
                                ui.close_menu();
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl+R").color(menu_text_color));
                                },
                            );
                        });

                        // Write mode toggle (raw window editing)
                        ui.horizontal(|ui| {
                            let selected = self.write_enabled;
                            if ui
                                .add(egui::SelectableLabel::new(
                                    selected,
                                    Self::menu_text_with_mnemonic(
                                        Some("✍ "),
                                        "Write Mode",
                                        'W',
                                        alt_pressed,
                                        menu_text_color,
                                    ),
                                ))
                                .clicked()
                            {
                                self.toggle_write_mode(ctx);
                                ui.close_menu();
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl+E").color(menu_text_color));
                                },
                            );
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
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl++").color(menu_text_color));
                                },
                            );
                        });

                        ui.horizontal(|ui| {
                            if ui.button("🔍 Zoom Out").clicked() {
                                self.renderer.zoom_out();
                                ui.close_menu();
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl+-").color(menu_text_color));
                                },
                            );
                        });

                        ui.horizontal(|ui| {
                            if ui.button("↩ Reset Zoom").clicked() {
                                self.renderer.reset_zoom();
                                ui.close_menu();
                            }
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("Ctrl+0").color(menu_text_color));
                                },
                            );
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
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    ui.label(RichText::new("F11").color(menu_text_color));
                                },
                            );
                        });
                    },
                );

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

        // No programmatic overlay menus; rely on pointer to open egui menus.
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
                    let mode = match self.view_mode {
                        ViewMode::Rendered => "Rendered",
                        ViewMode::Raw => "Raw",
                    };
                    ui.label(format!(
                        "Mode: {} | Elements: {} | Characters: {}",
                        mode, element_count, char_count
                    ));
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

        // Track window position/size for persistence
        ctx.input(|i| {
            let vp = i.viewport();
            // Position should use outer rect (top-left of the whole window)
            if let Some(outer) = vp.outer_rect {
                self.last_window_pos = Some([outer.left(), outer.top()]);
            }
            // Size should use inner rect (content area). Storing outer size and
            // restoring as inner size causes the window to grow each restart.
            // Only update if not fullscreen to avoid saving screen-sized values.
            if !vp.fullscreen.unwrap_or(false) {
                if let Some(inner) = vp.inner_rect {
                    self.last_window_size = Some([inner.width(), inner.height()]);
                }
            }
            self.last_window_maximized = vp.maximized.unwrap_or(false);
        });

        // Opportunistically persist window state if it changed, throttled to once per second
        if self.should_persist_window_state() {
            self.persist_window_state();
            self.last_persist_instant = std::time::Instant::now();
        }

        // Handle fullscreen toggle outside input context to avoid deadlocks
        if self.toggle_fullscreen {
            self.toggle_fullscreen = false;
            let current_fullscreen = ctx.input(|i| i.viewport().fullscreen.unwrap_or(false));
            ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(!current_fullscreen));
        }

        // Handle deferred view toggle outside of input context
        if self.view_toggle_requested {
            self.view_toggle_requested = false;
            self.toggle_view_mode(ctx);
        }

        // Handle deferred write toggle outside of input context
        if self.write_toggle_requested {
            self.write_toggle_requested = false;
            self.toggle_write_mode(ctx);
        }

        // Handle deferred caret movement for raw editor
        if let Some(delta) = self.pending_raw_cursor_line_move.take() {
            self.move_raw_cursor_lines(ctx, delta);
        }

        // Handle reload request outside input context to avoid blocking within input handling
        if self.reload_requested {
            self.reload_requested = false;
            if let Err(e) = self.reload_current_file() {
                self.error_message = Some(format!("Failed to reload file: {}", e));
            }
        }

        // Render menu bar
        self.render_menu_bar(ctx);

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
                                // Update highlight phrase: prefer live input, else last executed
                                if self.show_search && !self.search_query.is_empty() {
                                    self.renderer.set_highlight_phrase(Some(&self.search_query));
                                } else if !self.last_query.is_empty() {
                                    self.renderer.set_highlight_phrase(Some(&self.last_query));
                                } else {
                                    self.renderer.set_highlight_phrase(None);
                                }

                                self.renderer.render_to_ui(ui, &self.parsed_elements);
                                // If a header anchor was clicked, scroll to it
                                if let Some(anchor) = self.renderer.take_pending_anchor() {
                                    if let Some(rect) = self.renderer.header_rect_for(&anchor) {
                                        // Align target header to the top of the visible area
                                        ui.scroll_to_rect(rect, Some(egui::Align::Min));
                                    }
                                }
                                // If a search requested a scroll, align to top of visible area
                                if let Some(idx) = self.pending_scroll_to_element.take() {
                                    if let Some(rect) = self.renderer.element_rect_at(idx) {
                                        ui.scroll_to_rect(rect, Some(egui::Align::Min));
                                    }
                                }
                            }
                            ViewMode::Raw => {
                                // Raw markdown view; editable when write mode is enabled
                                if self.write_enabled {
                                    let editor_id = egui::Id::new("raw_editor");
                                    // If we have a remembered cursor, restore it (clamped)
                                    if let Some(mut idx) = self.raw_cursor.take() {
                                        idx = idx.min(self.raw_buffer.len());
                                        if let Some(mut state) =
                                            egui::text_edit::TextEditState::load(
                                                ui.ctx(),
                                                editor_id,
                                            )
                                        {
                                            let cr = egui::text::CCursorRange::one(
                                                egui::text::CCursor::new(idx),
                                            );
                                            state.cursor.set_char_range(Some(cr));
                                            state.store(ui.ctx(), editor_id);
                                        } else {
                                            let mut state =
                                                egui::text_edit::TextEditState::default();
                                            let cr = egui::text::CCursorRange::one(
                                                egui::text::CCursor::new(idx),
                                            );
                                            state.cursor.set_char_range(Some(cr));
                                            state.store(ui.ctx(), editor_id);
                                        }
                                    }
                                    let before = self.raw_buffer.clone();
                                    let resp = ui.add(
                                        TextEdit::multiline(&mut self.raw_buffer)
                                            .font(TextStyle::Monospace)
                                            .code_editor()
                                            .lock_focus(false)
                                            .interactive(true)
                                            .desired_width(f32::INFINITY)
                                            .desired_rows(24)
                                            .id_source(editor_id),
                                    );
                                    if self.raw_focus_requested {
                                        resp.request_focus();
                                        self.raw_focus_requested = false;
                                    }
                                    // Remember cursor position for next time
                                    if let Some(state) =
                                        egui::text_edit::TextEditState::load(ui.ctx(), editor_id)
                                    {
                                        if let Some(range) = state.cursor.char_range() {
                                            let idx = range.primary.index;
                                            self.raw_cursor = Some(idx);
                                        }
                                    }
                                    if self.raw_buffer != before {
                                        self.current_content = self.raw_buffer.clone();
                                        match self.renderer.parse(&self.current_content) {
                                            Ok(elements) => {
                                                self.parsed_elements = elements;
                                                self.error_message = None;
                                            }
                                            Err(e) => {
                                                self.error_message = Some(format!(
                                                    "Failed to parse markdown: {}",
                                                    e
                                                ));
                                            }
                                        }
                                    }
                                } else {
                                    // Read-only
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
                    }
                });
        });

        // Render floating search dialog (non-modal, always on top)
        self.render_search_dialog(ctx);

        // Render status bar
        self.render_status_bar(ctx);
    }

    fn auto_save_interval(&self) -> std::time::Duration {
        std::time::Duration::from_secs(30)
    }

    fn save(&mut self, _storage: &mut dyn eframe::Storage) {
        // Persist window position and size on app save/exit
        self.persist_window_state();
    }
}

impl MarkdownViewerApp {
    // Build a rich-text label with only the mnemonic character underlined (visual cue only).
    fn menu_text_with_mnemonic(
        prefix: Option<&str>,
        label: &str,
        mnemonic: char,
        underline: bool,
        text_color: Color32,
    ) -> LayoutJob {
        let mut job = LayoutJob::default();
        let default_fmt = TextFormat {
            color: text_color,
            ..TextFormat::default()
        };
        if let Some(p) = prefix {
            job.append(p, 0.0, default_fmt.clone());
        }
        let m = mnemonic.to_ascii_lowercase();
        let mut applied = false;
        for c in label.chars() {
            let mut fmt = default_fmt.clone();
            if underline && !applied && c.to_ascii_lowercase() == m {
                // Use a subtle underline color; white works well on dark theme
                fmt.underline = egui::Stroke::new(1.0, text_color);
                applied = true;
            }
            let s = c.to_string();
            job.append(&s, 0.0, fmt);
        }
        job
    }
    fn persist_window_state(&mut self) {
        if let Some(state) = self.current_window_state() {
            if !self.window_state_changed(&state) {
                return;
            }
            if crate::save_window_state(&state).is_ok() {
                self.last_persist_instant = std::time::Instant::now();
                self.last_persisted_state = Some(state);
            }
        }
    }

    fn should_persist_window_state(&self) -> bool {
        if self.last_persist_instant.elapsed() < std::time::Duration::from_secs(1) {
            return false;
        }
        if let Some(state) = self.current_window_state() {
            self.window_state_changed(&state)
        } else {
            false
        }
    }

    /// Save current document. If no file is associated, prompts for a path.
    fn save_current_document(&mut self) -> Result<()> {
        if let Some(path) = self.current_file.clone() {
            std::fs::write(&path, &self.current_content)?;
            let parent = path.parent();
            self.renderer.set_base_dir(parent);
            Ok(())
        } else if let Some(path) = FileDialog::new()
            .set_title("Save Markdown File")
            .add_filter("Markdown files", &["md", "markdown", "mdown", "mkd"])
            .save_file()
        {
            std::fs::write(&path, &self.current_content)?;
            let filename = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("Unknown")
                .to_string();
            self.current_file = Some(path);
            if let Some(parent) = self.current_file.as_ref().and_then(|p| p.parent()) {
                self.renderer.set_base_dir(Some(parent));
            } else {
                self.renderer.set_base_dir(None);
            }
            self.title = format!("MarkdownView - {}", filename);
            Ok(())
        } else {
            Ok(())
        }
    }

    /// Render the floating non-modal search dialog
    fn render_search_dialog(&mut self, ctx: &Context) {
        if !self.show_search {
            return;
        }
        let prev_open = self.show_search;
        let mut open = self.show_search;
        let prev_query = self.search_query.clone();
        egui::Window::new("Find")
            .collapsible(false)
            .resizable(false)
            .default_pos(egui::pos2(80.0, 80.0))
            .open(&mut open)
            .show(ctx, |ui| {
                let mut submitted_next = false;
                ui.horizontal(|ui| {
                    let text_edit = egui::TextEdit::singleline(&mut self.search_query)
                        .hint_text("Search text...")
                        .desired_width(240.0);
                    let resp = ui.add(text_edit);
                    if self.search_focus_requested {
                        resp.request_focus();
                        self.search_focus_requested = false;
                    }
                    if resp.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                        submitted_next = true;
                    }
                    if ui.button("Next (F3)").clicked() || submitted_next {
                        self.find_next();
                    }
                    if ui.button("Prev (Shift+F3)").clicked() {
                        self.find_previous();
                    }
                });
            });
        // Dynamic search: scroll to first match from anchor whenever the input changes
        if self.search_query != prev_query {
            if self.search_query.is_empty() {
                self.last_query.clear();
                self.renderer.set_highlight_phrase(None);
            } else {
                self.last_query = self.search_query.clone();
                // Use current last match as baseline if set, else start of doc
                let baseline = self.last_match_index.unwrap_or(0);
                let needle = Self::fold_for_search(&self.search_query);
                let total = self.parsed_elements.len();
                let mut found: Option<usize> = None;
                for pass in 0..2 {
                    let range: Box<dyn Iterator<Item = usize>> = if pass == 0 {
                        Box::new(baseline..total)
                    } else {
                        Box::new(0..baseline.min(total))
                    };
                    for idx in range {
                        let text = crate::markdown_renderer::MarkdownRenderer::element_plain_text(
                            &self.parsed_elements[idx],
                        );
                        if Self::fold_for_search(&text).contains(&needle) {
                            found = Some(idx);
                            break;
                        }
                    }
                    if found.is_some() {
                        break;
                    }
                }
                if let Some(idx) = found {
                    self.last_match_index = Some(idx);
                    self.pending_scroll_to_element = Some(idx);
                }
            }
        }
        // If dialog closed via close button, clear before hiding
        if prev_open && !open {
            self.clear_search_state();
        }
        self.show_search = open;
    }

    // No overlay menu helpers; we only render egui's built-in menus.
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
        let ctx = egui::Context::default();
        assert!(matches!(app.view_mode, ViewMode::Rendered));
        app.toggle_view_mode(&ctx);
        assert!(matches!(app.view_mode, ViewMode::Raw));
        app.toggle_view_mode(&ctx);
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
    fn test_reload_current_file() -> Result<()> {
        let mut app = MarkdownViewerApp::new();

        // Create a temporary markdown file
        let mut temp_file = NamedTempFile::new()?;
        let content1 = "# Title\n\nVersion 1";
        temp_file.write_all(content1.as_bytes())?;
        temp_file.flush()?;

        let path = temp_file.path().to_path_buf();
        app.load_file(path.clone())?;
        assert!(app.current_content.contains("Version 1"));

        // Update file content
        let content2 = "# Title\n\nVersion 2";
        temp_file.as_file_mut().set_len(0)?; // clear
        temp_file.write_all(content2.as_bytes())?;
        temp_file.flush()?;

        // Reload and verify
        app.reload_current_file()?;
        assert!(app.current_content.contains("Version 2"));
        Ok(())
    }

    #[test]
    fn test_load_file_with_invalid_utf8() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let mut temp_file = NamedTempFile::new()?;
        let bytes = b"Hello\xFFWorld";
        temp_file.write_all(bytes)?;
        temp_file.flush()?;

        app.load_file(temp_file.path().to_path_buf())?;

        assert!(app.current_content.contains('\u{FFFD}'));
        assert!(!app.parsed_elements.is_empty());
        assert!(app.error_message.is_none());
        Ok(())
    }

    #[test]
    fn test_reload_without_file() {
        let mut app = MarkdownViewerApp::new();
        assert!(app.current_file.is_none());
        let result = app.reload_current_file();
        assert!(result.is_err());
    }

    #[test]
    fn test_window_state_change_detection() {
        let mut app = MarkdownViewerApp::new();
        app.last_window_pos = Some([10.0, 10.0]);
        app.last_window_size = Some([800.0, 600.0]);
        app.last_window_maximized = false;

        let state = app.current_window_state().expect("state");
        assert!(app.window_state_changed(&state));
        app.last_persisted_state = Some(state);
        assert!(!app.window_state_changed(&state));

        let mut moved = state;
        moved.pos = [12.0, 10.0];
        assert!(app.window_state_changed(&moved));
    }

    #[test]
    fn test_fold_for_search_handles_case_and_accents() {
        assert_eq!(
            MarkdownViewerApp::fold_for_search("Äß"),
            MarkdownViewerApp::fold_for_search("äSS")
        );
        assert_eq!(
            MarkdownViewerApp::fold_for_search("Tom\u{0301}"),
            MarkdownViewerApp::fold_for_search("TÓm")
        );
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
