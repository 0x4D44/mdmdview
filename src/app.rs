/// Main application logic for the markdown viewer
///
/// This module contains the primary app state, UI logic, and event handling
/// for the markdown viewer application built with egui.
use crate::{MarkdownElement, MarkdownRenderer, SampleFile, WindowState, SAMPLE_FILES};
use anyhow::{bail, Result};
use crossbeam_channel::{unbounded, Receiver, Sender};
use egui::text::LayoutJob;
use egui::text::TextFormat;
use egui::{menu, CentralPanel, Color32, Context, RichText, TopBottomPanel};
use egui::{TextEdit, TextStyle};
use image::{imageops, RgbaImage};
#[cfg(not(test))]
use rfd::FileDialog;
#[cfg(test)]
use std::cell::RefCell;
#[cfg(test)]
use std::collections::HashSet;
use std::collections::VecDeque;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
#[cfg(test)]
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};
use unicode_casefold::UnicodeCaseFold;
use unicode_normalization::UnicodeNormalization;

/// Prefix used for application/window titles.
pub const APP_TITLE_PREFIX: &str = "mdmdview";
const BUILD_VERSION: &str = env!("CARGO_PKG_VERSION");
const BUILD_TIMESTAMP: &str = env!("MDMDVIEW_BUILD_TIMESTAMP");
const ASYNC_LOAD_THRESHOLD_BYTES: u64 = 2 * 1024 * 1024;

#[cfg(test)]
thread_local! {
    static FORCED_APP_ACTIONS: RefCell<HashSet<&'static str>> = RefCell::new(HashSet::new());
    static FORCED_OPEN_PATH: RefCell<Option<PathBuf>> = const { RefCell::new(None) };
    static FORCED_SAVE_PATH: RefCell<Option<PathBuf>> = const { RefCell::new(None) };
    static FORCED_LOAD_ERROR: RefCell<bool> = const { RefCell::new(false) };
    static FORCED_SCAN_ERROR: RefCell<bool> = const { RefCell::new(false) };
    static FORCED_SCAN_ENTRY_ERROR: RefCell<bool> = const { RefCell::new(false) };
    static FORCED_READ_LOSSY_ERROR: RefCell<bool> = const { RefCell::new(false) };
}

#[cfg(test)]
static FORCE_THREAD_SPAWN_ERROR: AtomicBool = AtomicBool::new(false);

#[cfg(test)]
fn app_action_triggered(triggered: bool, action: &'static str) -> bool {
    triggered || FORCED_APP_ACTIONS.with(|actions| actions.borrow().contains(action))
}

#[cfg(not(test))]
fn app_action_triggered(triggered: bool, _action: &'static str) -> bool {
    triggered
}

#[cfg(test)]
fn take_forced_open_path() -> Option<PathBuf> {
    FORCED_OPEN_PATH.with(|slot| slot.borrow_mut().take())
}

#[cfg(test)]
fn take_forced_save_path() -> Option<PathBuf> {
    FORCED_SAVE_PATH.with(|slot| slot.borrow_mut().take())
}

#[cfg(test)]
fn take_forced_load_error() -> bool {
    FORCED_LOAD_ERROR.with(|flag| flag.replace(false))
}

#[cfg(test)]
fn take_forced_scan_error() -> bool {
    FORCED_SCAN_ERROR.with(|flag| flag.replace(false))
}

#[cfg(test)]
fn take_forced_scan_entry_error() -> bool {
    FORCED_SCAN_ENTRY_ERROR.with(|flag| flag.replace(false))
}

#[cfg(test)]
fn take_forced_read_lossy_error() -> bool {
    FORCED_READ_LOSSY_ERROR.with(|flag| flag.replace(false))
}

/// Entry in navigation history for back/forward navigation
#[derive(Clone, Debug)]
struct HistoryEntry {
    file_path: Option<PathBuf>,
    title: String,
    content: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScreenshotTheme {
    Light,
    Dark,
}

impl ScreenshotTheme {
    fn as_str(self) -> &'static str {
        match self {
            ScreenshotTheme::Light => "light",
            ScreenshotTheme::Dark => "dark",
        }
    }
}

#[derive(Clone, Debug)]
pub struct ScreenshotConfig {
    pub output_path: PathBuf,
    pub viewport_width: f32,
    pub viewport_height: f32,
    pub content_only: bool,
    pub scroll_ratio: Option<f32>,
    pub wait_ms: u64,
    pub settle_frames: u32,
    pub zoom: f32,
    pub theme: ScreenshotTheme,
    pub font_source: Option<String>,
}

impl ScreenshotConfig {
    fn metadata_path(&self) -> PathBuf {
        self.output_path.with_extension("json")
    }
}

#[derive(Clone, Copy, Debug)]
struct ScrollSnapshot {
    content_size: egui::Vec2,
    inner_rect: egui::Rect,
    offset_y: f32,
}

#[derive(Debug, Clone, Copy)]
struct WindowAdjustment {
    pos: Option<egui::Pos2>,
    size: Option<egui::Vec2>,
}

struct FileLoadRequest {
    id: u64,
    path: PathBuf,
}

struct FileLoadResult {
    id: u64,
    content: Result<(String, bool), String>,
}

#[derive(Clone)]
struct PendingFileLoad {
    id: u64,
    path: PathBuf,
}

#[derive(Debug)]
struct ScreenshotState {
    config: ScreenshotConfig,
    started: Instant,
    stable_frames: u32,
    last_layout_hash: Option<u64>,
    last_scroll_offset: Option<f32>,
    scroll_offset: Option<f32>,
    content_rect: Option<egui::Rect>,
    last_content_size: Option<egui::Vec2>,
    last_inner_rect: Option<egui::Rect>,
    pixels_per_point: f32,
    viewport_adjusted: bool,
    requested: bool,
    done: bool,
    timed_out: bool,
    pending_renders: bool,
}

#[derive(Clone, Debug)]
struct ScreenshotSnapshot {
    config: ScreenshotConfig,
    content_rect: Option<egui::Rect>,
    pixels_per_point: f32,
    stable_frames: u32,
    timed_out: bool,
    pending_renders: bool,
    last_scroll_offset: Option<f32>,
    started: Instant,
}

impl ScreenshotState {
    fn new(config: ScreenshotConfig) -> Self {
        Self {
            config,
            started: Instant::now(),
            stable_frames: 0,
            last_layout_hash: None,
            last_scroll_offset: None,
            scroll_offset: None,
            content_rect: None,
            last_content_size: None,
            last_inner_rect: None,
            pixels_per_point: 1.0,
            viewport_adjusted: false,
            requested: false,
            done: false,
            timed_out: false,
            pending_renders: false,
        }
    }

    fn record_scroll(&mut self, snapshot: ScrollSnapshot) -> bool {
        self.last_content_size = Some(snapshot.content_size);
        self.last_inner_rect = Some(snapshot.inner_rect);
        self.last_scroll_offset = Some(snapshot.offset_y);

        if self.scroll_offset.is_none() {
            if let Some(ratio) = self.config.scroll_ratio {
                let max_scroll = (snapshot.content_size.y - snapshot.inner_rect.height()).max(0.0);
                self.scroll_offset = Some((max_scroll * ratio).round());
                return true;
            }
        }
        false
    }

    fn scroll_ready(&self) -> bool {
        match (
            self.config.scroll_ratio,
            self.scroll_offset,
            self.last_scroll_offset,
        ) {
            (None, _, _) => true,
            (Some(_), Some(target), Some(actual)) => (actual - target).abs() <= 0.5,
            _ => false,
        }
    }

    fn update_stability(&mut self, layout_hash: Option<u64>, scroll_offset: Option<f32>) {
        let mut changed = false;
        if let Some(hash) = layout_hash {
            if self.last_layout_hash != Some(hash) {
                changed = true;
            }
            self.last_layout_hash = Some(hash);
        } else {
            changed = true;
        }

        if let (Some(prev), Some(current)) = (self.last_scroll_offset, scroll_offset) {
            if (prev - current).abs() > 0.5 {
                changed = true;
            }
        }

        if changed {
            self.stable_frames = 0;
        } else {
            self.stable_frames = self.stable_frames.saturating_add(1);
        }
    }
}

impl From<&ScreenshotState> for ScreenshotSnapshot {
    fn from(state: &ScreenshotState) -> Self {
        Self {
            config: state.config.clone(),
            content_rect: state.content_rect,
            pixels_per_point: state.pixels_per_point,
            stable_frames: state.stable_frames,
            timed_out: state.timed_out,
            pending_renders: state.pending_renders,
            last_scroll_offset: state.last_scroll_offset,
            started: state.started,
        }
    }
}

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
    file_load_tx: Sender<FileLoadRequest>,
    file_load_rx: Receiver<FileLoadResult>,
    pending_file_load: Option<PendingFileLoad>,
    next_file_load_id: u64,
    // Search state
    show_search: bool,
    search_query: String,
    last_query: String,
    last_match_index: Option<usize>,
    pending_scroll_to_element: Option<usize>,
    search_focus_requested: bool,
    /// Deferred caret movement (in lines) for raw editor
    pending_raw_cursor_line_move: Option<i32>,
    // Navigation history
    /// History of visited files and samples for back/forward navigation
    history: Vec<HistoryEntry>,
    /// Current position in history
    history_index: usize,
    /// Maximum history entries to keep
    max_history: usize,
    // Drag and drop state
    /// Visual state: file is being dragged over window
    drag_hover: bool,
    /// Queue of files waiting to be opened (from multi-file drop)
    pending_files: VecDeque<PathBuf>,
    screenshot: Option<ScreenshotState>,
    /// Last title sent to the viewport (to avoid redundant updates)
    last_sent_title: Option<String>,
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
        let (file_load_tx, file_load_rx) = Self::spawn_file_loader();
        let mut app = Self {
            renderer: MarkdownRenderer::new(),
            current_file: None,
            current_content: String::new(),
            raw_buffer: String::new(),
            parsed_elements: Vec::new(),
            title: APP_TITLE_PREFIX.to_string(),
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
            file_load_tx,
            file_load_rx,
            pending_file_load: None,
            next_file_load_id: 0,
            show_search: false,
            search_query: String::new(),
            last_query: String::new(),
            last_match_index: None,
            pending_scroll_to_element: None,
            search_focus_requested: false,
            pending_raw_cursor_line_move: None,
            history: Vec::new(),
            history_index: 0,
            max_history: 50,
            drag_hover: false,
            pending_files: VecDeque::new(),
            screenshot: None,
            last_sent_title: None,
        };
        // Load welcome content by default
        app.load_welcome_from_samples(SAMPLE_FILES, false);

        app
    }

    pub fn set_zoom_scale(&mut self, scale: f32) {
        self.renderer.set_zoom_scale(scale);
    }

    pub fn set_screenshot_mode(&mut self, config: ScreenshotConfig) {
        self.screenshot = Some(ScreenshotState::new(config));
        self.view_mode = ViewMode::Rendered;
        self.write_enabled = false;
        self.show_search = false;
        self.search_focus_requested = false;
        self.nav_request = None;
    }

    /// Check if file has valid markdown extension
    fn is_valid_markdown_file(&self, path: &Path) -> bool {
        if let Some(ext) = path.extension() {
            let ext = ext.to_string_lossy().to_lowercase();
            matches!(ext.as_str(), "md" | "markdown" | "mdown" | "mkd" | "txt")
        } else {
            false
        }
    }

    /// Handle dropped files from drag-and-drop operation
    fn handle_file_drop(&mut self, paths: Vec<PathBuf>) {
        if paths.is_empty() {
            return;
        }

        let mut valid_files = Vec::new();
        let mut errors = Vec::new();

        // Validate all dropped files
        for path in paths {
            if !path.exists() {
                errors.push(format!("File not found: {}", path.display()));
                continue;
            }

            if path.is_dir() {
                // Handle directory by scanning for markdown files
                match self.scan_directory(&path) {
                    Ok(dir_files) => {
                        if dir_files.is_empty() {
                            errors.push(format!(
                                "No markdown files in directory: {}",
                                path.display()
                            ));
                        } else {
                            valid_files.extend(dir_files);
                        }
                    }
                    Err(e) => {
                        errors.push(format!("Cannot read directory {}: {}", path.display(), e));
                    }
                }
                continue;
            }

            if !self.is_valid_markdown_file(&path) {
                errors.push(format!(
                    "Not a markdown file: {}",
                    path.file_name().unwrap_or_default().to_string_lossy()
                ));
                continue;
            }

            valid_files.push(path);
        }

        // Limit to prevent memory issues
        const MAX_FILES: usize = 50;
        if valid_files.len() > MAX_FILES {
            self.error_message = Some(format!(
                "Too many files ({}). Maximum is {}.\n\
                 Please drop files in smaller batches.",
                valid_files.len(),
                MAX_FILES
            ));
            return;
        }

        // Handle valid files
        if !valid_files.is_empty() {
            // Open first file immediately
            let first_file = valid_files.remove(0);
            if let Err(e) = self.load_file(first_file, true) {
                self.error_message = Some(format!("Failed to load file: {}", e));
                return;
            }

            // Queue remaining files
            self.pending_files.extend(valid_files.iter().cloned());

            // Show info message if multiple files
            if !self.pending_files.is_empty() {
                eprintln!(
                    "Queued {} files. Use Alt+Right to navigate to the next file.",
                    self.pending_files.len()
                );
            }
        }

        // Show errors if any
        if !errors.is_empty() {
            let valid_count = if valid_files.is_empty() {
                0
            } else {
                valid_files.len() + 1
            };
            let error_msg = if errors.len() == 1 && valid_count == 0 {
                errors[0].clone()
            } else if valid_count == 0 {
                // All files failed
                format!("No valid files:\n{}", errors.join("\n"))
            } else {
                // Some succeeded, some failed
                format!(
                    "Opened {} files. Skipped {}:\n{}",
                    valid_count,
                    errors.len(),
                    errors.join("\n")
                )
            };
            self.error_message = Some(error_msg);
        }
    }

    /// Resolve a directory entry result, allowing test hooks to force failures.
    fn resolve_scan_entry(entry: std::io::Result<std::fs::DirEntry>) -> Result<std::fs::DirEntry> {
        #[cfg(test)]
        if take_forced_scan_entry_error() {
            bail!("Forced scan entry error");
        }
        Ok(entry?)
    }

    /// Scan directory for markdown files (non-recursive)
    fn scan_directory(&self, dir: &Path) -> Result<Vec<PathBuf>> {
        #[cfg(test)]
        if take_forced_scan_error() {
            bail!("Forced scan error");
        }

        let mut files = Vec::new();

        let entries = std::fs::read_dir(dir)?;

        for entry in entries {
            let entry = Self::resolve_scan_entry(entry)?;
            let path = entry.path();

            // Only include files (not subdirectories)
            if path.is_file() && self.is_valid_markdown_file(&path) {
                files.push(path);
            }
        }

        // Sort alphabetically for predictable order
        files.sort();

        Ok(files)
    }

    /// Load markdown content from a string
    pub fn load_content(&mut self, content: &str, title: Option<String>) {
        self.pending_file_load = None;
        self.current_content = content.to_string();
        self.raw_buffer = self.current_content.clone();
        self.error_message = None;
        self.nav_request = None; // Reset any pending navigation
                                 // Ensure scroll resets to top on new content
        self.pending_scroll_to_element = Some(0);
        self.renderer.clear_table_layout_cache();
        self.renderer.clear_image_failure_cache();

        match self.renderer.parse(content) {
            Ok(elements) => {
                self.parsed_elements = elements;
                if let Some(title) = title {
                    self.title = format!("{APP_TITLE_PREFIX} - {}", title);
                }
            }
            Err(e) => {
                self.error_message = Some(format!("Failed to parse markdown: {}", e));
                self.parsed_elements.clear();
            }
        }
    }

    /// Load markdown content from a file path
    pub fn load_file(&mut self, path: PathBuf, record_history: bool) -> Result<()> {
        #[cfg(test)]
        if take_forced_load_error() {
            bail!("Forced load error");
        }

        // Push current state to history before loading new file
        if record_history && !self.current_content.is_empty() {
            self.push_history();
        }

        let file_size = std::fs::metadata(&path).ok().map(|m| m.len());
        let use_async = self.screenshot.is_none()
            && file_size.is_some_and(|size| size >= ASYNC_LOAD_THRESHOLD_BYTES);
        if use_async {
            let load_id = self.next_file_load_id;
            self.next_file_load_id = self.next_file_load_id.wrapping_add(1);
            let request = FileLoadRequest {
                id: load_id,
                path: path.clone(),
            };
            if self.file_load_tx.send(request).is_ok() {
                self.pending_file_load = Some(PendingFileLoad { id: load_id, path });
                return Ok(());
            }
        }

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

    /// Normalize all line endings to Unix style (\n)
    /// Handles Windows (\r\n), Unix (\n), and old Mac (\r) formats
    fn normalize_line_endings(s: &str) -> String {
        // Order matters: replace \r\n first (Windows), then remaining \r (old Mac)
        let normalized = s.replace("\r\n", "\n").replace('\r', "\n");
        if let Some(stripped) = normalized.strip_prefix('\u{FEFF}') {
            stripped.to_string()
        } else {
            normalized
        }
    }

    fn read_lossy_bytes(path: &Path) -> std::io::Result<Vec<u8>> {
        #[cfg(test)]
        if take_forced_read_lossy_error() {
            return Err(std::io::Error::other("forced lossy read failure"));
        }
        std::fs::read(path)
    }

    fn read_file_lossy(path: &Path) -> Result<(String, bool)> {
        match std::fs::read_to_string(path) {
            Ok(s) => Ok((Self::normalize_line_endings(&s), false)),
            Err(e) if e.kind() == ErrorKind::InvalidData => {
                let bytes = Self::read_lossy_bytes(path)?;
                let s = String::from_utf8_lossy(&bytes).into_owned();
                Ok((Self::normalize_line_endings(&s), true))
            }
            Err(e) => Err(e.into()),
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

    fn spawn_file_loader() -> (Sender<FileLoadRequest>, Receiver<FileLoadResult>) {
        let (request_tx, request_rx) = unbounded::<FileLoadRequest>();
        let (result_tx, result_rx) = unbounded::<FileLoadResult>();
        if let Err(err) = Self::spawn_named_thread("mdmdview-file-loader", move || {
            for request in request_rx.iter() {
                let content = MarkdownViewerApp::read_file_lossy(&request.path)
                    .map_err(|err| err.to_string());
                let _ = result_tx.send(FileLoadResult {
                    id: request.id,
                    content,
                });
            }
        }) {
            eprintln!("Failed to start file loader thread: {err}");
        }
        (request_tx, result_rx)
    }

    fn poll_file_loads(&mut self) {
        while let Ok(result) = self.file_load_rx.try_recv() {
            let pending = match &self.pending_file_load {
                Some(pending) if pending.id == result.id => pending.clone(),
                _ => continue,
            };
            self.pending_file_load = None;
            match result.content {
                Ok((content, lossy)) => {
                    let filename = pending
                        .path
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("Unknown")
                        .to_string();
                    let base = pending.path.parent().map(|p| p.to_path_buf());
                    self.renderer.set_base_dir(base.as_deref());
                    self.current_file = Some(pending.path);
                    self.load_content(&content, Some(filename));
                    if lossy {
                        eprintln!("Loaded file with invalid UTF-8; replaced invalid sequences.");
                    }
                }
                Err(err) => {
                    self.error_message = Some(format!("Failed to load file: {err}"));
                }
            }
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

    fn compute_window_adjustment(
        outer_rect: Option<egui::Rect>,
        inner_rect: Option<egui::Rect>,
        monitor_size: Option<egui::Vec2>,
    ) -> Option<WindowAdjustment> {
        let outer = outer_rect?;
        let inner = inner_rect.unwrap_or(outer);
        let monitor = monitor_size?;

        if monitor.x <= 0.0 || monitor.y <= 0.0 {
            return None;
        }

        const MIN_WIDTH: f32 = 600.0;
        const MIN_HEIGHT: f32 = 400.0;
        const OFFSCREEN_TOLERANCE: f32 = 32.0;

        let mut pos = outer.min;
        let mut size = inner.size();
        let mut pos_adjusted = false;
        let mut size_adjusted = false;

        if !pos.x.is_finite() || !pos.y.is_finite() {
            pos = egui::pos2(0.0, 0.0);
            pos_adjusted = true;
        }
        if !size.x.is_finite() || !size.y.is_finite() {
            size = egui::vec2(MIN_WIDTH, MIN_HEIGHT);
            size_adjusted = true;
        }

        let outer_size = outer.size();
        let inner_size = inner.size();
        let frame = egui::vec2(
            (outer_size.x - inner_size.x).max(0.0),
            (outer_size.y - inner_size.y).max(0.0),
        );

        let available_width = (monitor.x - frame.x).max(MIN_WIDTH);
        let available_height = (monitor.y - frame.y).max(MIN_HEIGHT);

        if size.x < MIN_WIDTH {
            size.x = MIN_WIDTH.min(available_width);
            size_adjusted = true;
        }
        if size.y < MIN_HEIGHT {
            size.y = MIN_HEIGHT.min(available_height);
            size_adjusted = true;
        }
        if size.x > available_width {
            size.x = available_width;
            size_adjusted = true;
        }
        if size.y > available_height {
            size.y = available_height;
            size_adjusted = true;
        }

        let outer_size_for_bounds = size + frame;
        let max_pos = egui::pos2(
            (monitor.x - outer_size_for_bounds.x).max(0.0),
            (monitor.y - outer_size_for_bounds.y).max(0.0),
        );

        if pos.x <= -0.25 * outer_size_for_bounds.x {
            pos.x = 0.0;
            pos_adjusted = true;
        }
        if pos.y <= -0.25 * outer_size_for_bounds.y {
            pos.y = 0.0;
            pos_adjusted = true;
        }
        if pos.x > max_pos.x + OFFSCREEN_TOLERANCE {
            pos.x = max_pos.x;
            pos_adjusted = true;
        }
        if pos.y > max_pos.y + OFFSCREEN_TOLERANCE {
            pos.y = max_pos.y;
            pos_adjusted = true;
        }

        if pos_adjusted || size_adjusted {
            size.x = size.x.max(MIN_WIDTH.min(available_width));
            size.y = size.y.max(MIN_HEIGHT.min(available_height));
            Some(WindowAdjustment {
                pos: pos_adjusted.then_some(pos),
                size: size_adjusted.then_some(size),
            })
        } else {
            None
        }
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
        input.case_fold().nfkc().collect()
    }

    /// Push current state to navigation history
    fn push_history(&mut self) {
        // Truncate forward history if we're not at the end
        if self.history_index < self.history.len() {
            self.history.truncate(self.history_index);
        }

        // Create entry for current state
        let entry = HistoryEntry {
            file_path: self.current_file.clone(),
            title: self.title.clone(),
            content: self.current_content.clone(),
        };

        self.history.push(entry);

        // Limit history size
        if self.history.len() > self.max_history {
            self.history.remove(0);
        } else {
            self.history_index = self.history.len();
        }
    }

    /// Navigate back in history
    fn navigate_back(&mut self) -> bool {
        if self.history_index > 0 {
            self.history_index -= 1;
            self.restore_from_history();
            true
        } else {
            false
        }
    }

    /// Navigate forward in history
    fn navigate_forward(&mut self) -> bool {
        // First try pending files queue
        if let Some(next_file) = self.pending_files.pop_front() {
            // Load next file from queue
            if let Err(e) = self.load_file(next_file, true) {
                self.error_message = Some(format!("Failed to load file: {}", e));
            }
            return true;
        }

        // Otherwise use history navigation (existing code)
        if self.history_index < self.history.len().saturating_sub(1) {
            self.history_index += 1;
            self.restore_from_history();
            true
        } else {
            false
        }
    }

    /// Restore state from history at current index
    fn restore_from_history(&mut self) {
        if let Some(entry) = self.history.get(self.history_index) {
            self.current_file = entry.file_path.clone();
            self.title = entry.title.clone();
            self.current_content = entry.content.clone();
            self.raw_buffer = self.current_content.clone();

            // Re-parse content
            match self.renderer.parse(&self.current_content) {
                Ok(elements) => {
                    self.parsed_elements = elements;
                    self.error_message = None;
                }
                Err(e) => {
                    self.error_message = Some(format!("Failed to parse: {}", e));
                }
            }

            // Set base dir for images
            if let Some(ref path) = self.current_file {
                self.renderer.set_base_dir(path.parent());
            } else {
                self.renderer.set_base_dir(None);
            }

            // Scroll to top
            self.pending_scroll_to_element = Some(0);
        }
    }

    /// Check if we can navigate back
    fn can_navigate_back(&self) -> bool {
        self.history_index > 0
    }

    /// Check if we can navigate forward
    fn can_navigate_forward(&self) -> bool {
        !self.pending_files.is_empty() || self.history_index < self.history.len().saturating_sub(1)
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
        self.load_file(path, false)
    }

    /// Load a sample file by name
    pub fn load_sample(&mut self, sample: &SampleFile) {
        // Push current state to history before loading sample
        if !self.current_content.is_empty() {
            self.push_history();
        }

        self.current_file = None;
        // Samples have no file base-dir
        self.renderer.set_base_dir(None);
        self.load_content(sample.content, Some(sample.title.to_string()));
        // Scroll to top for new sample
        self.pending_scroll_to_element = Some(0);
    }

    fn load_sample_by_name(&mut self, samples: &[SampleFile], name: &str) -> bool {
        if let Some(sample) = samples.iter().find(|sample| sample.name == name) {
            self.load_sample(sample);
            return true;
        }
        false
    }

    fn load_welcome_from_samples(&mut self, samples: &[SampleFile], scroll_to_top: bool) {
        if let Some(welcome) = samples.iter().find(|sample| sample.name == "welcome.md") {
            self.load_content(welcome.content, Some("Welcome".to_string()));
            if scroll_to_top {
                self.pending_scroll_to_element = Some(0);
            }
        } else {
            // Fallback if welcome file is missing
            self.current_content.clear();
            self.parsed_elements.clear();
            self.title = APP_TITLE_PREFIX.to_string();
            self.error_message = None;
        }
    }

    /// Close the current file and return to welcome screen
    pub fn close_current_file(&mut self) {
        self.current_file = None;
        self.renderer.set_base_dir(None);
        self.load_welcome_from_samples(SAMPLE_FILES, true);
    }

    #[cfg(not(test))]
    fn request_close(&self, ctx: &Context) {
        ctx.send_viewport_cmd(egui::ViewportCommand::Close);
    }

    #[cfg(test)]
    fn request_close(&self, _ctx: &Context) {}

    #[cfg(not(test))]
    fn pick_open_path(&self) -> Option<PathBuf> {
        FileDialog::new()
            .add_filter("Markdown files", &["md", "markdown", "mdown", "mkd"])
            .add_filter("All files", &["*"])
            .set_title("Open Markdown File")
            .pick_file()
    }

    #[cfg(test)]
    fn pick_open_path(&self) -> Option<PathBuf> {
        take_forced_open_path()
    }

    #[cfg(not(test))]
    fn pick_save_path(&self) -> Option<PathBuf> {
        FileDialog::new()
            .set_title("Save Markdown File")
            .add_filter("Markdown files", &["md", "markdown", "mdown", "mkd"])
            .save_file()
    }

    #[cfg(test)]
    fn pick_save_path(&self) -> Option<PathBuf> {
        take_forced_save_path()
    }

    /// Open file dialog to select a markdown file
    fn open_file_dialog(&mut self) {
        if let Some(path) = self.pick_open_path() {
            if let Err(e) = self.load_file(path, true) {
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
                self.request_close(ctx);
            }

            // Ctrl+Q - Quit application
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::CTRL,
                egui::Key::Q,
            )) {
                self.request_close(ctx);
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
            let prev_shift = i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::SHIFT,
                egui::Key::F3,
            ));
            let prev_alt = i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::ALT,
                egui::Key::F3,
            ));
            if prev_shift || prev_alt {
                self.find_previous();
            } else if i.consume_key(egui::Modifiers::NONE, egui::Key::F3) {
                self.find_next();
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

            // Alt+Left - Navigate back
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::ALT,
                egui::Key::ArrowLeft,
            )) {
                self.navigate_back();
            }

            // Alt+Right - Navigate forward
            if i.consume_shortcut(&egui::KeyboardShortcut::new(
                egui::Modifiers::ALT,
                egui::Key::ArrowRight,
            )) {
                self.navigate_forward();
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
                    } else {
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
            // Note: No global Alt+R to avoid conflict (File > Reload vs View > Raw)

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

    fn render_file_menu_contents(
        &mut self,
        ui: &mut egui::Ui,
        alt_pressed: bool,
        menu_text_color: Color32,
    ) {
        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Open...",
                    'O',
                    alt_pressed,
                    menu_text_color,
                )))
                .clicked();
            if app_action_triggered(clicked, "menu_open") {
                self.open_file_dialog();
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Ctrl+O").color(menu_text_color));
            });
        });

        ui.horizontal(|ui| {
            let enabled = !self.current_content.is_empty();
            let button = ui.add_enabled(
                enabled,
                egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Save",
                    'S',
                    alt_pressed,
                    menu_text_color,
                )),
            );
            if app_action_triggered(button.clicked(), "menu_save") {
                if let Err(e) = self.save_current_document() {
                    self.error_message = Some(format!("Failed to save: {}", e));
                }
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Ctrl+S").color(menu_text_color));
            });
        });

        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Close",
                    'C',
                    alt_pressed,
                    menu_text_color,
                )))
                .clicked();
            if app_action_triggered(clicked, "menu_close") {
                self.close_current_file();
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Ctrl+W").color(menu_text_color));
            });
        });

        ui.horizontal(|ui| {
            let enabled = self.current_file.is_some();
            let button = ui.add_enabled(
                enabled,
                egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Reload",
                    'R',
                    alt_pressed,
                    menu_text_color,
                )),
            );
            if app_action_triggered(button.clicked(), "menu_reload") {
                self.request_reload();
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("F5").color(menu_text_color));
            });
        });

        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Find...",
                    'F',
                    alt_pressed,
                    menu_text_color,
                )))
                .clicked();
            if app_action_triggered(clicked, "menu_find") {
                self.show_search = true;
                self.search_focus_requested = true;
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Ctrl+F").color(menu_text_color));
            });
        });

        ui.separator();

        #[cfg(not(test))]
        ui.menu_button(
            Self::menu_text_with_mnemonic(None, "Samples", 'S', alt_pressed, menu_text_color),
            |ui| {
                self.render_samples_menu_contents(ui);
            },
        );
        #[cfg(test)]
        self.render_samples_menu_contents(ui);
        if app_action_triggered(false, "menu_samples") {
            self.render_samples_menu_contents(ui);
        }
    }

    fn render_samples_menu_contents(&mut self, ui: &mut egui::Ui) {
        for sample in SAMPLE_FILES {
            if app_action_triggered(ui.button(sample.title).clicked(), sample.name) {
                self.load_sample(sample);
                ui.close_menu();
            }
        }
    }

    fn render_view_menu_contents(
        &mut self,
        ui: &mut egui::Ui,
        ctx: &Context,
        alt_pressed: bool,
        menu_text_color: Color32,
    ) {
        ui.horizontal(|ui| {
            let enabled = self.can_navigate_back();
            let clicked = ui
                .add_enabled(enabled, egui::Button::new("<- Back"))
                .clicked();
            if app_action_triggered(clicked, "menu_back") {
                self.navigate_back();
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Alt+Left").color(menu_text_color));
            });
        });

        ui.horizontal(|ui| {
            let enabled = self.can_navigate_forward();
            let clicked = ui
                .add_enabled(enabled, egui::Button::new("Forward ->"))
                .clicked();
            if app_action_triggered(clicked, "menu_forward") {
                self.navigate_forward();
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Alt+Right").color(menu_text_color));
            });
        });

        ui.separator();

        ui.horizontal(|ui| {
            let selected = matches!(self.view_mode, ViewMode::Raw);
            let clicked = ui
                .add(egui::SelectableLabel::new(
                    selected,
                    Self::menu_text_with_mnemonic(
                        None,
                        "Raw Markdown",
                        'R',
                        alt_pressed,
                        menu_text_color,
                    ),
                ))
                .clicked();
            if app_action_triggered(clicked, "menu_raw") {
                self.toggle_view_mode(ctx);
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Ctrl+R").color(menu_text_color));
            });
        });

        ui.horizontal(|ui| {
            let selected = self.write_enabled;
            let clicked = ui
                .add(egui::SelectableLabel::new(
                    selected,
                    Self::menu_text_with_mnemonic(
                        None,
                        "Write Mode",
                        'W',
                        alt_pressed,
                        menu_text_color,
                    ),
                ))
                .clicked();
            if app_action_triggered(clicked, "menu_write") {
                self.toggle_write_mode(ctx);
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Ctrl+E").color(menu_text_color));
            });
        });

        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::SelectableLabel::new(
                    self.wrap_raw,
                    Self::menu_text_with_mnemonic(
                        None,
                        "Wrap Raw Lines",
                        'L',
                        alt_pressed,
                        menu_text_color,
                    ),
                ))
                .clicked();
            if app_action_triggered(clicked, "menu_wrap_raw") {
                self.wrap_raw = !self.wrap_raw;
            }
        });

        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Zoom In",
                    'I',
                    alt_pressed,
                    menu_text_color,
                )))
                .clicked();
            if app_action_triggered(clicked, "menu_zoom_in") {
                self.renderer.zoom_in();
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Ctrl++").color(menu_text_color));
            });
        });

        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Zoom Out",
                    'O',
                    alt_pressed,
                    menu_text_color,
                )))
                .clicked();
            if app_action_triggered(clicked, "menu_zoom_out") {
                self.renderer.zoom_out();
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Ctrl+-").color(menu_text_color));
            });
        });

        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Reset Zoom",
                    'Z',
                    alt_pressed,
                    menu_text_color,
                )))
                .clicked();
            if app_action_triggered(clicked, "menu_zoom_reset") {
                self.renderer.reset_zoom();
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("Ctrl+0").color(menu_text_color));
            });
        });

        ui.separator();

        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Toggle Fullscreen",
                    'T',
                    alt_pressed,
                    menu_text_color,
                )))
                .clicked();
            if app_action_triggered(clicked, "menu_fullscreen") {
                let is_fullscreen = ctx.input(|i| i.viewport().fullscreen.unwrap_or(false));
                ctx.send_viewport_cmd(egui::ViewportCommand::Fullscreen(!is_fullscreen));
                ui.close_menu();
            }
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(RichText::new("F11").color(menu_text_color));
            });
        });
    }

    fn render_help_menu_contents(
        &mut self,
        ui: &mut egui::Ui,
        alt_pressed: bool,
        menu_text_color: Color32,
    ) {
        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "Usage Instructions",
                    'U',
                    alt_pressed,
                    menu_text_color,
                )))
                .clicked();
            if app_action_triggered(clicked, "menu_help_usage") {
                let _ = self.load_sample_by_name(SAMPLE_FILES, "usage.md");
                ui.close_menu();
            }
        });

        ui.horizontal(|ui| {
            let clicked = ui
                .add(egui::Button::new(Self::menu_text_with_mnemonic(
                    None,
                    "About",
                    'A',
                    alt_pressed,
                    menu_text_color,
                )))
                .clicked();
            if app_action_triggered(clicked, "menu_help_about") {
                let _ = self.load_sample_by_name(SAMPLE_FILES, "welcome.md");
                ui.close_menu();
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
                #[cfg(not(test))]
                ui.menu_button(
                    Self::menu_text_with_mnemonic(None, "File", 'F', alt_pressed, menu_text_color),
                    |ui| {
                        self.render_file_menu_contents(ui, alt_pressed, menu_text_color);
                    },
                );
                #[cfg(test)]
                self.render_file_menu_contents(ui, alt_pressed, menu_text_color);
                if app_action_triggered(false, "menu_bar_file") {
                    self.render_file_menu_contents(ui, alt_pressed, menu_text_color);
                }

                // View menu
                #[cfg(not(test))]
                ui.menu_button(
                    Self::menu_text_with_mnemonic(None, "View", 'V', alt_pressed, menu_text_color),
                    |ui| {
                        self.render_view_menu_contents(ui, ctx, alt_pressed, menu_text_color);
                    },
                );
                #[cfg(test)]
                self.render_view_menu_contents(ui, ctx, alt_pressed, menu_text_color);
                if app_action_triggered(false, "menu_bar_view") {
                    self.render_view_menu_contents(ui, ctx, alt_pressed, menu_text_color);
                }

                #[cfg(not(test))]
                ui.menu_button(
                    Self::menu_text_with_mnemonic(None, "Help", 'H', alt_pressed, menu_text_color),
                    |ui| {
                        self.render_help_menu_contents(ui, alt_pressed, menu_text_color);
                    },
                );
                #[cfg(test)]
                self.render_help_menu_contents(ui, alt_pressed, menu_text_color);
                if app_action_triggered(false, "menu_bar_help") {
                    self.render_help_menu_contents(ui, alt_pressed, menu_text_color);
                }
            });
        });

        // No programmatic overlay menus; rely on pointer to open egui menus.
    }

    fn render_main_context_menu(&mut self, ui: &mut egui::Ui) {
        ui.label("Select text, then use Ctrl+C to copy");
        ui.separator();

        // Copy All Text option
        if app_action_triggered(ui.button("Copy All Text").clicked(), "ctx_copy_all") {
            let all_text = MarkdownRenderer::elements_to_plain_text(&self.parsed_elements);
            ui.ctx().copy_text(all_text);
            ui.close_menu();
        }

        // Copy as Markdown (Raw) option
        if app_action_triggered(ui.button("Copy as Markdown").clicked(), "ctx_copy_markdown") {
            ui.ctx().copy_text(self.current_content.clone());
            ui.close_menu();
        }

        ui.separator();

        // Navigation shortcuts
        if app_action_triggered(ui.button("Go to Top").clicked(), "ctx_nav_top") {
            self.nav_request = Some(NavigationRequest::Top);
            ui.close_menu();
        }

        if app_action_triggered(ui.button("Go to Bottom").clicked(), "ctx_nav_bottom") {
            self.nav_request = Some(NavigationRequest::Bottom);
            ui.close_menu();
        }
    }
    /// Render the status bar
    fn render_status_bar(&self, ctx: &Context) {
        TopBottomPanel::bottom("status_bar").show(ctx, |ui| {
            ui.horizontal(|ui| {
                // Current file info
                if let Some(path) = &self.current_file {
                    ui.label(format!("File: {}", path.display()));
                } else if !self.parsed_elements.is_empty() {
                    ui.label("Sample file");
                } else {
                    ui.label("No file loaded");
                }

                if let Some(pending) = &self.pending_file_load {
                    ui.separator();
                    ui.label(
                        RichText::new(format!("Loading {}", pending.path.display()))
                            .color(Color32::from_rgb(120, 200, 255)),
                    );
                }

                // Show pending file count if files are queued
                if !self.pending_files.is_empty() {
                    ui.separator();
                    ui.label(
                        RichText::new(format!("{} files in queue", self.pending_files.len()))
                            .color(egui::Color32::from_rgb(100, 150, 255)),
                    );

                    ui.label(
                        RichText::new("(Alt+Right for next)")
                            .color(egui::Color32::GRAY)
                            .italics(),
                    );
                }

                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    // Document stats
                    let element_count = self.parsed_elements.len();
                    let char_count = self.current_content.len();
                    let mode = match self.view_mode {
                        ViewMode::Rendered => "Rendered",
                        ViewMode::Raw => "Raw",
                    };
                    let status = format!(
                        "Mode: {} | Elements: {} | Characters: {}",
                        mode, element_count, char_count
                    );
                    #[cfg(test)]
                    {
                        ui.label(status);
                        self.render_status_tooltip(ui);
                    }
                    #[cfg(not(test))]
                    {
                        ui.label(status)
                            .on_hover_ui(|ui| self.render_status_tooltip(ui));
                    }
                    if app_action_triggered(false, "status_hover") {
                        self.render_status_tooltip(ui);
                    }
                });
            });
        });
    }

    fn render_status_tooltip(&self, ui: &mut egui::Ui) {
        ui.label(format!("Version: {}", BUILD_VERSION));
        ui.label(format!("Built: {}", BUILD_TIMESTAMP));
    }
    /// Handle drag-drop events from egui
    fn handle_drag_drop_events(&mut self, ctx: &Context) {
        ctx.input(|i| {
            // Check if files are being hovered
            self.drag_hover = !i.raw.hovered_files.is_empty();

            // Check if files were dropped
            if !i.raw.dropped_files.is_empty() {
                let paths: Vec<PathBuf> = i
                    .raw
                    .dropped_files
                    .iter()
                    .filter_map(|f| f.path.clone())
                    .collect();

                self.handle_file_drop(paths);
            }
        });
    }

    /// Render drag-and-drop overlay when files are hovered
    fn render_drag_overlay(&self, ctx: &Context) {
        if !self.drag_hover {
            return;
        }

        // Full-screen overlay
        egui::Area::new(egui::Id::new("drag_overlay"))
            .fixed_pos(egui::pos2(0.0, 0.0))
            .order(egui::Order::Foreground)
            .show(ctx, |ui| {
                let screen_rect = ctx.screen_rect();

                // Semi-transparent dark background
                ui.painter()
                    .rect_filled(screen_rect, 0.0, egui::Color32::from_black_alpha(180));

                // Dashed border effect using rounded rect with stroke
                let border_rect = screen_rect.shrink(20.0);
                let border_color = egui::Color32::from_rgb(100, 150, 255);

                // Draw border
                ui.painter()
                    .rect_stroke(border_rect, 8.0, egui::Stroke::new(4.0, border_color));

                // Center text
                ui.allocate_ui_at_rect(screen_rect, |ui| {
                    ui.vertical_centered(|ui| {
                        ui.add_space(screen_rect.height() / 2.0 - 80.0);

                        // Main message
                        ui.label(
                            RichText::new("Drop files to open")
                                .size(36.0)
                                .color(egui::Color32::WHITE)
                                .strong(),
                        );

                        ui.add_space(20.0);

                        // Supported formats
                        ui.label(
                            RichText::new("Supported: .md, .markdown, .mdown, .mkd, .txt")
                                .size(18.0)
                                .color(egui::Color32::LIGHT_GRAY),
                        );

                        ui.add_space(10.0);

                        // Additional hint
                        ui.label(
                            RichText::new("Drop multiple files to open them in sequence")
                                .size(14.0)
                                .color(egui::Color32::from_rgb(150, 150, 150))
                                .italics(),
                        );
                    });
                });
            });
    }

    fn update_impl(&mut self, ctx: &Context) {
        // Debug: log repaint causes and input state when MDMDVIEW_DEBUG_REPAINT is set
        if std::env::var("MDMDVIEW_DEBUG_REPAINT").is_ok() {
            ctx.input(|i| {
                let events_count = i.events.len();
                let pointer_delta = i.pointer.delta();
                let scroll_delta = i.raw_scroll_delta;
                if events_count > 0
                    || pointer_delta != egui::Vec2::ZERO
                    || scroll_delta != egui::Vec2::ZERO
                {
                    eprintln!(
                        "[INPUT] frame={} events={} pointer_delta={:?} scroll={:?}",
                        ctx.frame_nr(),
                        events_count,
                        pointer_delta,
                        scroll_delta
                    );
                }
            });
            let causes = ctx.repaint_causes();
            if !causes.is_empty() {
                eprintln!("[REPAINT] frame={} causes={:?}", ctx.frame_nr(), causes);
            }
        }

        self.handle_screenshot_events(ctx);
        self.poll_file_loads();
        if self.screenshot.as_ref().is_some_and(|state| state.done) {
            return;
        }

        let screenshot_active = self.screenshot.is_some();
        let hide_chrome = screenshot_active;

        // Handle drag-drop events
        if !screenshot_active {
            self.handle_drag_drop_events(ctx);
        }

        // Handle keyboard shortcuts
        if !screenshot_active {
            self.handle_shortcuts(ctx);
        }

        // Keep native window title in sync with the current document
        // Only send command when title actually changes to avoid continuous repaints
        if self.last_sent_title.as_ref() != Some(&self.title) {
            ctx.send_viewport_cmd(egui::ViewportCommand::Title(self.title.clone()));
            self.last_sent_title = Some(self.title.clone());
        }

        let (monitor_size, outer_rect, inner_rect, is_fullscreen, is_maximized) = ctx.input(|i| {
            let vp = i.viewport();
            (
                vp.monitor_size,
                vp.outer_rect,
                vp.inner_rect,
                vp.fullscreen.unwrap_or(false),
                vp.maximized.unwrap_or(false),
            )
        });

        let mut pos_adjusted = false;
        let mut size_adjusted = false;
        if !is_fullscreen {
            if let Some(adjustment) =
                Self::compute_window_adjustment(outer_rect, inner_rect, monitor_size)
            {
                if let Some(pos) = adjustment.pos {
                    pos_adjusted = true;
                    ctx.send_viewport_cmd(egui::ViewportCommand::OuterPosition(pos));
                    self.last_window_pos = Some([pos.x, pos.y]);
                }
                if let Some(size) = adjustment.size {
                    size_adjusted = true;
                    ctx.send_viewport_cmd(egui::ViewportCommand::InnerSize(size));
                    self.last_window_size = Some([size.x, size.y]);
                }
            }
        }

        if !pos_adjusted {
            if let Some(outer) = outer_rect {
                self.last_window_pos = Some([outer.left(), outer.top()]);
            }
        }
        if !size_adjusted && !is_fullscreen {
            if let Some(inner) = inner_rect {
                self.last_window_size = Some([inner.width(), inner.height()]);
            }
        }
        self.last_window_maximized = is_maximized;

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

        // Render chrome panels before the central area so they reserve space.
        if !hide_chrome {
            self.render_menu_bar(ctx);
            self.render_status_bar(ctx);
        }

        let mut layout_signature: Option<u64> = None;
        let mut scroll_snapshot: Option<ScrollSnapshot> = None;
        let screenshot_scroll_offset = self
            .screenshot
            .as_ref()
            .and_then(|state| state.scroll_offset);

        // Main content area
        let central_response = CentralPanel::default().show(ctx, |ui| {
            // Show error message if any
            if let Some(ref error) = self.error_message {
                ui.colored_label(Color32::RED, format!("Error: {}", error));
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

            // Use AlwaysVisible to prevent scrollbar appearance/disappearance from
            // changing the available width, which would cause Mermaid diagrams to
            // re-render at different widths and create layout oscillation/flickering.
            let mut scroll_area = egui::ScrollArea::vertical()
                .id_source(self.scroll_area_id)
                .auto_shrink([false, false])
                .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysVisible);
            if let Some(offset) = screenshot_scroll_offset {
                scroll_area = scroll_area.vertical_scroll_offset(offset);
            }
            if screenshot_active {
                scroll_area = scroll_area.enable_scrolling(false);
            }
            let scroll_output = scroll_area.show(ui, |ui| {
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

                        if app_action_triggered(ui.button("Open File").clicked(), "welcome_open") {
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
                            layout_signature = Some(self.renderer.layout_signature());
                        }
                        ViewMode::Raw => {
                            // Raw markdown view; editable when write mode is enabled
                            if self.write_enabled {
                                let editor_id = egui::Id::new("raw_editor");
                                // If we have a remembered cursor, restore it (clamped)
                                if let Some(mut idx) = self.raw_cursor.take() {
                                    idx = idx.min(self.raw_buffer.len());
                                    if let Some(mut state) =
                                        egui::text_edit::TextEditState::load(ui.ctx(), editor_id)
                                    {
                                        let cr = egui::text::CCursorRange::one(
                                            egui::text::CCursor::new(idx),
                                        );
                                        state.cursor.set_char_range(Some(cr));
                                        state.store(ui.ctx(), editor_id);
                                    } else {
                                        let mut state = egui::text_edit::TextEditState::default();
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
                                            self.error_message =
                                                Some(format!("Failed to parse markdown: {}", e));
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
            scroll_snapshot = Some(ScrollSnapshot {
                content_size: scroll_output.content_size,
                inner_rect: scroll_output.inner_rect,
                offset_y: scroll_output.state.offset.y,
            });
            // Debug scroll logging - writes to file to track content size changes
            {
                use std::io::Write;
                static DEBUG_SCROLL: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
                let enabled = *DEBUG_SCROLL.get_or_init(|| {
                    std::env::var("MDMDVIEW_DEBUG_SCROLL").is_ok()
                });
                if enabled {
                    let max_scroll = (scroll_output.content_size.y - scroll_output.inner_rect.height()).max(0.0);
                    let scroll_pct = if max_scroll > 0.0 {
                        (scroll_output.state.offset.y / max_scroll * 100.0) as i32
                    } else {
                        0
                    };
                    if let Ok(mut f) = std::fs::OpenOptions::new()
                        .create(true)
                        .append(true)
                        .open(r"c:\tmp\scroll-trace.log")
                    {
                        let _ = writeln!(
                            f,
                            "[SCROLL] content_h={:.0} viewport_h={:.0} offset={:.0} max={:.0} pct={}%",
                            scroll_output.content_size.y,
                            scroll_output.inner_rect.height(),
                            scroll_output.state.offset.y,
                            max_scroll,
                            scroll_pct
                        );
                    }
                }
            }
        });

        // Add context menu for the main panel
        #[cfg(not(test))]
        central_response
            .response
            .context_menu(|ui| self.render_main_context_menu(ui));

        if let Some(state) = self.screenshot.as_mut() {
            state.content_rect = Some(central_response.response.rect);
        }
        self.update_screenshot_state(ctx, layout_signature, scroll_snapshot);

        if !hide_chrome {
            // Render floating search dialog (non-modal, always on top)
            self.render_search_dialog(ctx);

            // Render drag-and-drop overlay (must be last to appear on top)
            self.render_drag_overlay(ctx);
        }
    }
}

impl eframe::App for MarkdownViewerApp {
    /// Update function called every frame
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame) {
        self.update_impl(ctx);
    }

    fn auto_save_interval(&self) -> std::time::Duration {
        std::time::Duration::from_secs(30)
    }

    fn save(&mut self, _storage: &mut dyn eframe::Storage) {
        // Persist window position and size on app save/exit
        if self.screenshot.is_none() {
            self.persist_window_state();
        }
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
        if self.screenshot.is_some() {
            return false;
        }
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
        } else if let Some(path) = self.pick_save_path() {
            std::fs::write(&path, &self.current_content)?;
            let filename = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("Unknown")
                .to_string();
            self.current_file = Some(path);
            let parent = self.current_file.as_ref().and_then(|p| p.parent());
            self.renderer.set_base_dir(parent);
            self.title = format!("{APP_TITLE_PREFIX} - {}", filename);
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
                    if Self::should_submit_search(
                        resp.lost_focus(),
                        ui.input(|i| i.key_pressed(egui::Key::Enter)),
                    ) {
                        submitted_next = true;
                    }
                    let next_clicked = ui.button("Next (F3)").clicked();
                    if app_action_triggered(next_clicked, "search_next") || submitted_next {
                        self.find_next();
                    }
                    let prev_clicked = ui.button("Prev (Shift+F3)").clicked();
                    if app_action_triggered(prev_clicked, "search_prev") {
                        self.find_previous();
                    }
                });
            });
        if app_action_triggered(false, "search_close") {
            open = false;
        }
        self.update_search_results(&prev_query);
        // If dialog closed via close button, clear before hiding
        if !open {
            self.clear_search_state();
        }
        self.show_search = open;
    }

    fn should_submit_search(lost_focus: bool, enter_pressed: bool) -> bool {
        lost_focus && enter_pressed
    }

    fn update_search_results(&mut self, prev_query: &str) {
        if self.search_query == prev_query {
            return;
        }
        if self.search_query.is_empty() {
            self.last_query.clear();
            self.renderer.set_highlight_phrase(None);
            return;
        }
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

    fn handle_screenshot_events(&mut self, ctx: &Context) {
        let screenshot = ctx.input(|i| {
            i.events.iter().find_map(|event| {
                if let egui::Event::Screenshot { image, .. } = event {
                    Some(std::sync::Arc::clone(image))
                } else {
                    None
                }
            })
        });
        let Some(image) = screenshot else {
            return;
        };
        let snapshot = {
            let Some(state) = self.screenshot.as_ref() else {
                return;
            };
            if state.done {
                return;
            }
            ScreenshotSnapshot::from(state)
        };
        if let Err(err) = Self::save_screenshot_image(&image, &snapshot) {
            eprintln!("Failed to save screenshot: {err}");
        }
        let state = self.screenshot.as_mut().expect("screenshot state");
        state.done = true;
        ctx.send_viewport_cmd(egui::ViewportCommand::Close);
    }

    fn update_screenshot_state(
        &mut self,
        ctx: &Context,
        layout_signature: Option<u64>,
        scroll_snapshot: Option<ScrollSnapshot>,
    ) {
        let Some(state) = self.screenshot.as_mut() else {
            return;
        };
        let native_ppp = ctx
            .input(|i| i.viewport().native_pixels_per_point)
            .unwrap_or(1.0);
        ctx.set_pixels_per_point(1.0);
        state.pixels_per_point = ctx.input(|i| i.pixels_per_point);

        if !state.viewport_adjusted {
            let width_points = (state.config.viewport_width / native_ppp).max(1.0);
            let height_points = (state.config.viewport_height / native_ppp).max(1.0);
            ctx.send_viewport_cmd(egui::ViewportCommand::InnerSize(egui::vec2(
                width_points,
                height_points,
            )));
            state.viewport_adjusted = true;
            ctx.request_repaint();
        }

        let mut scroll_offset = None;
        let mut scroll_changed = false;
        if let Some(snapshot) = scroll_snapshot {
            scroll_offset = Some(snapshot.offset_y);
            scroll_changed = state.record_scroll(snapshot);
        }

        state.update_stability(layout_signature, scroll_offset);

        let pending = self.renderer.has_pending_renders();
        state.pending_renders = pending;
        let elapsed = state.started.elapsed();
        let timed_out = elapsed >= Duration::from_millis(state.config.wait_ms);
        let stable = Self::screenshot_is_stable(
            state.scroll_ready(),
            state.stable_frames,
            state.config.settle_frames,
            pending,
        );

        if stable || timed_out {
            if timed_out && !stable {
                state.timed_out = true;
            }
            if !state.requested {
                ctx.send_viewport_cmd(egui::ViewportCommand::Screenshot);
                state.requested = true;
            }
        } else {
            ctx.request_repaint_after(Duration::from_millis(16));
        }

        if scroll_changed {
            ctx.request_repaint();
        }
        if state.requested && !state.done {
            ctx.request_repaint();
        }
    }

    fn screenshot_is_stable(
        scroll_ready: bool,
        stable_frames: u32,
        settle_frames: u32,
        pending: bool,
    ) -> bool {
        scroll_ready && stable_frames >= settle_frames && !pending
    }

    fn save_screenshot_image(image: &egui::ColorImage, state: &ScreenshotSnapshot) -> Result<()> {
        let full_width = image.size[0] as u32;
        let full_height = image.size[1] as u32;
        let mut rgba = Self::color_image_to_rgba(image);

        let mut crop_x = 0u32;
        let mut crop_y = 0u32;
        let mut crop_w = full_width;
        let mut crop_h = full_height;

        if state.config.content_only {
            if let Some(rect) = state.content_rect {
                let pixels_per_point = state.pixels_per_point.max(0.1);
                let min_x = (rect.min.x * pixels_per_point).round() as i32;
                let min_y = (rect.min.y * pixels_per_point).round() as i32;
                let max_x = (rect.max.x * pixels_per_point).round() as i32;
                let max_y = (rect.max.y * pixels_per_point).round() as i32;

                let min_x = min_x.clamp(0, full_width as i32);
                let min_y = min_y.clamp(0, full_height as i32);
                let max_x = max_x.clamp(min_x, full_width as i32);
                let max_y = max_y.clamp(min_y, full_height as i32);

                let width = (max_x - min_x) as u32;
                let height = (max_y - min_y) as u32;
                if width > 0 && height > 0 {
                    crop_x = min_x as u32;
                    crop_y = min_y as u32;
                    crop_w = width;
                    crop_h = height;
                }
            }
        }

        if crop_x != 0 || crop_y != 0 || crop_w != full_width || crop_h != full_height {
            rgba = imageops::crop_imm(&rgba, crop_x, crop_y, crop_w, crop_h).to_image();
        }

        if let Some(parent) = state.config.output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        rgba.save(&state.config.output_path)?;

        let metadata = Self::screenshot_metadata(state, full_width, full_height, crop_w, crop_h);
        std::fs::write(state.config.metadata_path(), metadata)?;
        Ok(())
    }

    fn screenshot_metadata(
        state: &ScreenshotSnapshot,
        full_width: u32,
        full_height: u32,
        crop_width: u32,
        crop_height: u32,
    ) -> String {
        let output_path = Self::json_escape(&state.config.output_path.to_string_lossy());
        let font_source = state
            .config
            .font_source
            .as_ref()
            .map(|value| format!("\"{}\"", Self::json_escape(value)))
            .unwrap_or_else(|| "null".to_string());
        let scroll_ratio = Self::json_opt_f32(state.config.scroll_ratio);
        let scroll_offset = Self::json_opt_f32(state.last_scroll_offset);
        let elapsed_ms = state.started.elapsed().as_millis();

        format!(
            "{{\n  \"version\": \"{}\",\n  \"build_timestamp\": \"{}\",\n  \"output\": \"{}\",\n  \"theme\": \"{}\",\n  \"zoom\": {:.3},\n  \"content_only\": {},\n  \"scroll_ratio\": {},\n  \"scroll_offset\": {},\n  \"viewport_px\": {{\"width\": {}, \"height\": {}}},\n  \"content_px\": {{\"width\": {}, \"height\": {}}},\n  \"pixels_per_point\": {:.3},\n  \"wait_ms\": {},\n  \"settle_frames\": {},\n  \"stable_frames\": {},\n  \"timed_out\": {},\n  \"pending_renders\": {},\n  \"font_source\": {},\n  \"elapsed_ms\": {}\n}}\n",
            BUILD_VERSION,
            BUILD_TIMESTAMP,
            output_path,
            state.config.theme.as_str(),
            state.config.zoom,
            state.config.content_only,
            scroll_ratio,
            scroll_offset,
            full_width,
            full_height,
            crop_width,
            crop_height,
            state.pixels_per_point,
            state.config.wait_ms,
            state.config.settle_frames,
            state.stable_frames,
            state.timed_out,
            state.pending_renders,
            font_source,
            elapsed_ms
        )
    }

    fn color_image_to_rgba(image: &egui::ColorImage) -> RgbaImage {
        let width = image.size[0] as u32;
        let height = image.size[1] as u32;
        let mut data = Vec::with_capacity(image.pixels.len() * 4);
        for pixel in &image.pixels {
            data.extend_from_slice(&[pixel.r(), pixel.g(), pixel.b(), pixel.a()]);
        }
        RgbaImage::from_raw(width, height, data).unwrap_or_else(|| RgbaImage::new(width, height))
    }

    fn json_escape(value: &str) -> String {
        let mut out = String::with_capacity(value.len() + 8);
        for ch in value.chars() {
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

    fn json_opt_f32(value: Option<f32>) -> String {
        value
            .map(|v| format!("{v:.3}"))
            .unwrap_or_else(|| "null".to_string())
    }

    // No overlay menu helpers; we only render egui's built-in menus.
}

impl Default for MarkdownViewerApp {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;
    use crate::markdown_renderer::InlineSpan;
    use eframe::{App, Storage};
    use std::io::Write;
    use std::sync::{Arc, Mutex, OnceLock};
    use tempfile::{NamedTempFile, TempDir};

    fn run_app_frame(app: &mut MarkdownViewerApp, ctx: &egui::Context, input: egui::RawInput) {
        let _ = ctx.run(input, |ctx| {
            app.update_impl(ctx);
        });
    }

    fn default_input() -> egui::RawInput {
        egui::RawInput {
            screen_rect: Some(egui::Rect::from_min_size(
                egui::pos2(0.0, 0.0),
                egui::vec2(960.0, 640.0),
            )),
            ..Default::default()
        }
    }

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("env lock")
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

    /// Helper to set platform-appropriate config env var and return expected config subdir.
    fn set_config_env(temp_path: &std::path::Path) -> (EnvGuard, std::path::PathBuf) {
        #[cfg(target_os = "windows")]
        {
            let guard = EnvGuard::set("APPDATA", temp_path.to_string_lossy().as_ref());
            let subdir = temp_path.join("MarkdownView");
            (guard, subdir)
        }
        #[cfg(target_os = "macos")]
        {
            let guard = EnvGuard::set("HOME", temp_path.to_string_lossy().as_ref());
            let subdir = temp_path.join("Library/Application Support/MarkdownView");
            (guard, subdir)
        }
        #[cfg(not(any(target_os = "windows", target_os = "macos")))]
        {
            let guard = EnvGuard::set("XDG_CONFIG_HOME", temp_path.to_string_lossy().as_ref());
            let subdir = temp_path.join("mdmdview");
            (guard, subdir)
        }
    }

    #[derive(Default)]
    struct DummyStorage;

    impl eframe::Storage for DummyStorage {
        fn get_string(&self, _key: &str) -> Option<String> {
            None
        }

        fn set_string(&mut self, _key: &str, _value: String) {}

        fn flush(&mut self) {}
    }

    #[test]
    fn test_dummy_storage_methods_cover_noops() {
        let mut storage = DummyStorage;
        assert!(storage.get_string("missing").is_none());
        storage.set_string("key", "value".to_string());
        storage.flush();
    }

    #[test]
    fn test_app_auto_save_interval_is_30_seconds() {
        let app = MarkdownViewerApp::new();
        assert_eq!(app.auto_save_interval(), std::time::Duration::from_secs(30));
    }

    #[test]
    fn test_app_update_calls_update_impl() {
        let mut app = MarkdownViewerApp::new();
        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |_| {});
        // SAFETY: eframe::Frame is unused by update(), so a dummy value is sufficient here.
        let mut frame = std::mem::MaybeUninit::<eframe::Frame>::zeroed();
        let frame = unsafe { frame.assume_init_mut() };
        app.update(&ctx, frame);
    }

    struct ForcedAppActions {
        actions: Vec<&'static str>,
    }

    impl ForcedAppActions {
        fn new(actions: &[&'static str]) -> Self {
            FORCED_APP_ACTIONS.with(|set| {
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

    impl Drop for ForcedAppActions {
        fn drop(&mut self) {
            FORCED_APP_ACTIONS.with(|set| {
                let mut set = set.borrow_mut();
                for action in &self.actions {
                    set.remove(action);
                }
            });
        }
    }

    struct ForcedDialogPaths;

    impl ForcedDialogPaths {
        fn new(open: Option<PathBuf>, save: Option<PathBuf>) -> Self {
            FORCED_OPEN_PATH.with(|slot| {
                *slot.borrow_mut() = open;
            });
            FORCED_SAVE_PATH.with(|slot| {
                *slot.borrow_mut() = save;
            });
            Self
        }
    }

    impl Drop for ForcedDialogPaths {
        fn drop(&mut self) {
            FORCED_OPEN_PATH.with(|slot| {
                slot.borrow_mut().take();
            });
            FORCED_SAVE_PATH.with(|slot| {
                slot.borrow_mut().take();
            });
        }
    }

    struct ForcedLoadError;

    impl ForcedLoadError {
        fn new() -> Self {
            FORCED_LOAD_ERROR.with(|flag| {
                *flag.borrow_mut() = true;
            });
            Self
        }
    }

    impl Drop for ForcedLoadError {
        fn drop(&mut self) {
            FORCED_LOAD_ERROR.with(|flag| {
                *flag.borrow_mut() = false;
            });
        }
    }

    struct ForcedScanError;

    impl ForcedScanError {
        fn new() -> Self {
            FORCED_SCAN_ERROR.with(|flag| {
                *flag.borrow_mut() = true;
            });
            Self
        }
    }

    impl Drop for ForcedScanError {
        fn drop(&mut self) {
            FORCED_SCAN_ERROR.with(|flag| {
                *flag.borrow_mut() = false;
            });
        }
    }

    struct ForcedScanEntryError;

    impl ForcedScanEntryError {
        fn new() -> Self {
            FORCED_SCAN_ENTRY_ERROR.with(|flag| {
                *flag.borrow_mut() = true;
            });
            Self
        }
    }

    impl Drop for ForcedScanEntryError {
        fn drop(&mut self) {
            FORCED_SCAN_ENTRY_ERROR.with(|flag| {
                *flag.borrow_mut() = false;
            });
        }
    }

    struct ForcedLossyReadError;

    impl ForcedLossyReadError {
        fn new() -> Self {
            FORCED_READ_LOSSY_ERROR.with(|flag| {
                *flag.borrow_mut() = true;
            });
            Self
        }
    }

    impl Drop for ForcedLossyReadError {
        fn drop(&mut self) {
            FORCED_READ_LOSSY_ERROR.with(|flag| {
                *flag.borrow_mut() = false;
            });
        }
    }

    #[test]
    fn test_app_action_triggered_with_forced_action() {
        let _guard = ForcedAppActions::new(&["menu_open"]);
        assert!(app_action_triggered(false, "menu_open"));
        assert!(app_action_triggered(true, "menu_open"));
    }

    #[test]
    fn test_normalize_line_endings() {
        // Windows style (\r\n)
        assert_eq!(
            MarkdownViewerApp::normalize_line_endings("Hello\r\nWorld"),
            "Hello\nWorld"
        );

        // Unix style (\n) - no change
        assert_eq!(
            MarkdownViewerApp::normalize_line_endings("Hello\nWorld"),
            "Hello\nWorld"
        );

        // Old Mac style (\r)
        assert_eq!(
            MarkdownViewerApp::normalize_line_endings("Hello\rWorld"),
            "Hello\nWorld"
        );

        // Mixed line endings
        assert_eq!(
            MarkdownViewerApp::normalize_line_endings("A\r\nB\nC\rD"),
            "A\nB\nC\nD"
        );

        // Multiple blank lines with Windows endings
        assert_eq!(
            MarkdownViewerApp::normalize_line_endings("A\r\n\r\nB"),
            "A\n\nB"
        );

        // Empty string
        assert_eq!(MarkdownViewerApp::normalize_line_endings(""), "");

        // No line endings
        assert_eq!(
            MarkdownViewerApp::normalize_line_endings("Single line"),
            "Single line"
        );
    }

    #[test]
    fn test_load_file_with_windows_line_endings() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let mut temp_file = NamedTempFile::new()?;

        // Write content with explicit Windows line endings
        temp_file.write_all(b"Line 1\r\nLine 2\r\n\r\nParagraph text")?;
        temp_file.flush()?;

        app.load_file(temp_file.path().to_path_buf(), true)?;

        // Should not contain any \r characters after normalization
        assert!(!app.current_content.contains('\r'));
        assert!(app.current_content.contains("Line 1\nLine 2"));
        assert!(app.current_content.contains("\n\nParagraph"));
        Ok(())
    }

    #[test]
    fn test_load_file_with_mixed_line_endings() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let mut temp_file = NamedTempFile::new()?;

        // Mix of \r\n (Windows), \n (Unix), and \r (old Mac)
        temp_file.write_all(b"Line 1\r\nLine 2\nLine 3\rLine 4")?;
        temp_file.flush()?;

        app.load_file(temp_file.path().to_path_buf(), true)?;

        // All should be normalized to \n
        assert!(!app.current_content.contains('\r'));
        assert_eq!(app.current_content.lines().count(), 4);
        let lines: Vec<&str> = app.current_content.lines().collect();
        assert_eq!(lines[0], "Line 1");
        assert_eq!(lines[1], "Line 2");
        assert_eq!(lines[2], "Line 3");
        assert_eq!(lines[3], "Line 4");
        Ok(())
    }

    #[test]
    fn test_load_file_strips_bom() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let mut temp_file = NamedTempFile::new()?;

        temp_file.write_all(b"\xEF\xBB\xBF# Heading\nContent")?;
        temp_file.flush()?;

        app.load_file(temp_file.path().to_path_buf(), true)?;

        assert!(!app.current_content.starts_with('\u{FEFF}'));
        assert!(app.current_content.starts_with("# Heading"));
        Ok(())
    }

    #[test]
    fn test_app_creation() {
        let app = MarkdownViewerApp::new();
        assert!(!app.parsed_elements.is_empty()); // Should load welcome content
        assert!(app.title.contains(APP_TITLE_PREFIX));
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
    fn test_toggle_view_mode_without_text_state() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        let ctx = egui::Context::default();
        app.toggle_view_mode(&ctx);
        assert!(matches!(app.view_mode, ViewMode::Rendered));
        assert!(app.raw_cursor.is_none());
    }

    #[test]
    fn test_toggle_view_mode_with_state_without_cursor_range() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        state.cursor.set_char_range(None);
        state.store(&ctx, editor_id);

        app.toggle_view_mode(&ctx);

        assert!(matches!(app.view_mode, ViewMode::Rendered));
        assert!(app.raw_cursor.is_none());
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
    fn test_load_sample_by_name_reports_missing() {
        let mut app = MarkdownViewerApp::new();
        app.current_content = "keep".to_string();

        let loaded = app.load_sample_by_name(&[], "missing.md");

        assert!(!loaded);
        assert_eq!(app.current_content, "keep");
    }

    #[test]
    fn test_load_sample_by_name_loads_content() {
        let mut app = MarkdownViewerApp::new();
        let sample = SAMPLE_FILES
            .iter()
            .find(|sample| sample.name == "welcome.md")
            .expect("welcome sample");

        let loaded = app.load_sample_by_name(SAMPLE_FILES, "welcome.md");

        assert!(loaded);
        assert_eq!(app.current_content, sample.content);
        assert_eq!(app.pending_scroll_to_element, Some(0));
    }

    #[test]
    fn test_load_welcome_from_samples_missing_clears_state() {
        let mut app = MarkdownViewerApp::new();
        app.current_content = "content".to_string();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "content".to_string(),
        )])];
        app.title = "Custom".to_string();
        app.error_message = Some("error".to_string());

        app.load_welcome_from_samples(&[], false);

        assert!(app.current_content.is_empty());
        assert!(app.parsed_elements.is_empty());
        assert_eq!(app.title, APP_TITLE_PREFIX);
        assert!(app.error_message.is_none());
    }

    #[test]
    fn test_load_welcome_from_samples_sets_scroll_when_requested() {
        let mut app = MarkdownViewerApp::new();
        app.pending_scroll_to_element = None;

        app.load_welcome_from_samples(SAMPLE_FILES, true);

        assert_eq!(app.pending_scroll_to_element, Some(0));
    }

    #[test]
    fn test_spawn_file_loader_handles_error_path() {
        MarkdownViewerApp::force_thread_spawn_error_for_test();
        let _ = MarkdownViewerApp::spawn_file_loader();
    }

    #[test]
    fn test_spawn_file_loader_processes_request() -> Result<()> {
        let temp = NamedTempFile::new()?;
        writeln!(&temp, "hello")?;

        let (request_tx, result_rx) = MarkdownViewerApp::spawn_file_loader();
        request_tx
            .send(FileLoadRequest {
                id: 99,
                path: temp.path().to_path_buf(),
            })
            .expect("send");
        let result = result_rx
            .recv_timeout(std::time::Duration::from_secs(1))
            .expect("result");
        let (content, lossy) = result.content.expect("content");
        assert!(content.contains("hello"));
        assert!(!lossy);
        drop(request_tx);
        Ok(())
    }

    #[test]
    fn test_spawn_file_loader_reports_missing_file() {
        let temp_dir = TempDir::new().expect("temp dir");
        let missing_path = temp_dir.path().join("missing.md");

        let (request_tx, result_rx) = MarkdownViewerApp::spawn_file_loader();
        request_tx
            .send(FileLoadRequest {
                id: 101,
                path: missing_path,
            })
            .expect("send");
        let result = result_rx
            .recv_timeout(std::time::Duration::from_secs(1))
            .expect("result");
        assert!(result.content.is_err());
        drop(request_tx);
    }

    #[test]
    fn test_load_sample_without_existing_content_skips_history() {
        let mut app = MarkdownViewerApp::new();
        app.current_content.clear();
        app.raw_buffer.clear();
        app.history.clear();
        app.history_index = 0;
        let sample = &SAMPLE_FILES[0];

        app.load_sample(sample);

        assert!(app.history.is_empty());
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
        app.load_file(path.clone(), true)?;

        assert_eq!(app.current_content, content);
        assert_eq!(app.current_file, Some(path));
        assert!(!app.parsed_elements.is_empty());
        assert!(app.error_message.is_none());

        Ok(())
    }

    #[test]
    fn test_load_file_async_sends_request() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let mut temp_file = NamedTempFile::new()?;
        let payload = vec![b'a'; ASYNC_LOAD_THRESHOLD_BYTES as usize];
        temp_file.write_all(&payload)?;
        temp_file.flush()?;

        let path = temp_file.path().to_path_buf();
        app.load_file(path.clone(), false)?;
        assert!(app.pending_file_load.is_some());
        assert_eq!(app.pending_file_load.as_ref().expect("pending").path, path);
        Ok(())
    }

    #[test]
    fn test_load_file_skips_async_when_screenshot_active() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let mut temp_file = NamedTempFile::new()?;
        let payload = vec![b'a'; ASYNC_LOAD_THRESHOLD_BYTES as usize];
        temp_file.write_all(&payload)?;
        temp_file.flush()?;

        let config = ScreenshotConfig {
            output_path: temp_file.path().with_extension("png"),
            viewport_width: 120.0,
            viewport_height: 80.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        app.screenshot = Some(ScreenshotState::new(config));

        let path = temp_file.path().to_path_buf();
        app.load_file(path.clone(), false)?;

        assert!(app.pending_file_load.is_none());
        assert_eq!(app.current_file, Some(path));
        Ok(())
    }

    #[test]
    fn test_load_file_async_falls_back_when_channel_closed() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let (tx, rx) = unbounded::<FileLoadRequest>();
        drop(rx);
        app.file_load_tx = tx;

        let mut temp_file = NamedTempFile::new()?;
        let payload = vec![b'a'; ASYNC_LOAD_THRESHOLD_BYTES as usize];
        temp_file.write_all(&payload)?;
        temp_file.flush()?;

        let path = temp_file.path().to_path_buf();
        app.load_file(path.clone(), false)?;
        assert_eq!(app.current_file, Some(path));
        assert!(app.pending_file_load.is_none());
        Ok(())
    }

    #[test]
    fn test_poll_file_loads_handles_mismatch_and_lossy() {
        let mut app = MarkdownViewerApp::new();
        let (result_tx, result_rx) = unbounded::<FileLoadResult>();
        app.file_load_rx = result_rx;

        let temp = NamedTempFile::new().expect("temp");
        let path = temp.path().to_path_buf();
        app.pending_file_load = Some(PendingFileLoad {
            id: 1,
            path: path.clone(),
        });

        result_tx
            .send(FileLoadResult {
                id: 2,
                content: Ok(("ignored".to_string(), false)),
            })
            .expect("send mismatch");
        app.poll_file_loads();
        assert!(app.pending_file_load.is_some());

        result_tx
            .send(FileLoadResult {
                id: 1,
                content: Ok(("loaded".to_string(), true)),
            })
            .expect("send match");
        app.poll_file_loads();
        assert!(app.pending_file_load.is_none());
        assert_eq!(app.current_file, Some(path));
    }

    #[test]
    fn test_poll_file_loads_handles_match_non_lossy() {
        let mut app = MarkdownViewerApp::new();
        let (result_tx, result_rx) = unbounded::<FileLoadResult>();
        app.file_load_rx = result_rx;

        let temp = NamedTempFile::new().expect("temp");
        let path = temp.path().to_path_buf();
        app.pending_file_load = Some(PendingFileLoad {
            id: 1,
            path: path.clone(),
        });

        result_tx
            .send(FileLoadResult {
                id: 1,
                content: Ok(("loaded".to_string(), false)),
            })
            .expect("send match");
        app.poll_file_loads();
        assert!(app.pending_file_load.is_none());
        assert_eq!(app.current_file, Some(path));
    }

    #[test]
    fn test_poll_file_loads_sets_error_on_failure() {
        let mut app = MarkdownViewerApp::new();
        let (result_tx, result_rx) = unbounded::<FileLoadResult>();
        app.file_load_rx = result_rx;

        let temp = NamedTempFile::new().expect("temp");
        let path = temp.path().to_path_buf();
        app.pending_file_load = Some(PendingFileLoad { id: 1, path });

        result_tx
            .send(FileLoadResult {
                id: 1,
                content: Err("boom".to_string()),
            })
            .expect("send match");
        app.poll_file_loads();
        assert!(app.pending_file_load.is_none());
        assert!(app.error_message.as_deref().unwrap_or("").contains("boom"));
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
        assert!(app.error_message.is_none());
    }

    #[test]
    fn test_load_content_parse_error_sets_message() {
        let mut app = MarkdownViewerApp::new();
        crate::markdown_renderer::force_parse_error_once();

        app.load_content("# Bad Parse", Some("Bad".to_string()));

        assert!(app
            .error_message
            .as_ref()
            .is_some_and(|msg| msg.contains("Failed to parse markdown")));
        assert!(app.parsed_elements.is_empty());
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

        let result = app.load_file(fake_path, true);
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
        app.load_file(path.clone(), true)?;
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

        app.load_file(temp_file.path().to_path_buf(), true)?;

        assert!(app.current_content.contains('\u{FFFD}'));
        assert!(!app.parsed_elements.is_empty());
        assert!(app.error_message.is_none());
        Ok(())
    }

    #[test]
    fn test_read_file_lossy_invalid_utf8_returns_lossy() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        std::fs::write(temp_file.path(), b"Hello\xFFWorld")?;

        let (content, lossy) = MarkdownViewerApp::read_file_lossy(temp_file.path())?;
        assert!(lossy);
        assert!(content.contains("Hello"));
        assert!(content.contains("World"));
        Ok(())
    }

    #[test]
    fn test_read_file_lossy_forced_read_error() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        std::fs::write(temp_file.path(), b"Hello\xFFWorld")?;

        let _guard = ForcedLossyReadError::new();
        assert!(MarkdownViewerApp::read_file_lossy(temp_file.path()).is_err());
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

        let mut resized = state;
        resized.size = [820.0, 610.0];
        assert!(app.window_state_changed(&resized));

        let mut maximized = state;
        maximized.maximized = true;
        assert!(app.window_state_changed(&maximized));
    }

    #[test]
    fn test_current_window_state_requires_size() {
        let mut app = MarkdownViewerApp::new();
        app.last_window_pos = Some([10.0, 10.0]);
        app.last_window_size = None;
        assert!(app.current_window_state().is_none());
    }

    #[test]
    fn test_fold_for_search_handles_case_and_accents() {
        assert_eq!(
            MarkdownViewerApp::fold_for_search("\u{00C4}\u{00DF}"),
            MarkdownViewerApp::fold_for_search("\u{00E4}SS")
        );
        assert_eq!(
            MarkdownViewerApp::fold_for_search("To\u{0301}m"),
            MarkdownViewerApp::fold_for_search("T\u{00D3}m")
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
        assert!(app.title.contains(APP_TITLE_PREFIX));
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

    #[test]
    fn test_compute_window_adjustment_clamps_offscreen_window() {
        let outer = egui::Rect::from_min_size(egui::pos2(1200.0, 900.0), egui::vec2(800.0, 600.0));
        let monitor = egui::vec2(1024.0, 768.0);
        let adjustment =
            MarkdownViewerApp::compute_window_adjustment(Some(outer), Some(outer), Some(monitor))
                .expect("should adjust window geometry");
        let pos = adjustment.pos.expect("expected position adjustment");
        let size = adjustment.size.unwrap_or_else(|| outer.size());
        assert!(pos.x <= monitor.x - size.x + 1.0);
        assert!(pos.y <= monitor.y - size.y + 1.0);
        assert!(adjustment.size.is_none());
    }

    #[test]
    fn test_compute_window_adjustment_respects_min_size() {
        let outer = egui::Rect::from_min_size(egui::pos2(-200.0, -100.0), egui::vec2(200.0, 100.0));
        let monitor = egui::vec2(1920.0, 1080.0);
        let adjustment =
            MarkdownViewerApp::compute_window_adjustment(Some(outer), Some(outer), Some(monitor))
                .expect("should enforce minimum window size");
        let pos = adjustment.pos.unwrap_or(outer.min);
        let size = adjustment.size.expect("expected size adjustment");
        assert!(size.x >= 600.0);
        assert!(size.y >= 400.0);
        assert!(pos.x >= 0.0);
        assert!(pos.y >= 0.0);
    }

    #[test]
    fn test_is_valid_markdown_file() {
        let app = MarkdownViewerApp::new();

        // Valid markdown extensions
        assert!(app.is_valid_markdown_file(Path::new("test.md")));
        assert!(app.is_valid_markdown_file(Path::new("test.markdown")));
        assert!(app.is_valid_markdown_file(Path::new("test.mdown")));
        assert!(app.is_valid_markdown_file(Path::new("test.mkd")));
        assert!(app.is_valid_markdown_file(Path::new("test.txt")));

        // Case insensitive
        assert!(app.is_valid_markdown_file(Path::new("test.MD")));
        assert!(app.is_valid_markdown_file(Path::new("test.Markdown")));

        // Invalid extensions
        assert!(!app.is_valid_markdown_file(Path::new("test.pdf")));
        assert!(!app.is_valid_markdown_file(Path::new("test.docx")));
        assert!(!app.is_valid_markdown_file(Path::new("test.html")));

        // No extension
        assert!(!app.is_valid_markdown_file(Path::new("test")));
    }

    #[test]
    fn test_scan_directory() -> Result<()> {
        let temp_dir = tempfile::TempDir::new()?;
        let dir_path = temp_dir.path();

        // Create test files
        std::fs::write(dir_path.join("zebra.md"), "# Z")?;
        std::fs::write(dir_path.join("alpha.md"), "# A")?;
        std::fs::write(dir_path.join("image.png"), "fake")?;
        std::fs::write(dir_path.join("beta.markdown"), "# B")?;

        // Create subdirectory (should be ignored)
        std::fs::create_dir(dir_path.join("subdir"))?;
        std::fs::write(dir_path.join("subdir/nested.md"), "# N")?;

        let app = MarkdownViewerApp::new();
        let files = app.scan_directory(dir_path)?;

        // Should find 3 markdown files (alpha, beta, zebra)
        // Should NOT find image.png or nested.md
        assert_eq!(files.len(), 3);

        // Should be sorted alphabetically
        assert!(files[0].ends_with("alpha.md"));
        assert!(files[1].ends_with("beta.markdown"));
        assert!(files[2].ends_with("zebra.md"));

        Ok(())
    }

    #[test]
    fn test_scan_empty_directory() -> Result<()> {
        let temp_dir = tempfile::TempDir::new()?;
        let app = MarkdownViewerApp::new();
        let files = app.scan_directory(temp_dir.path())?;

        assert_eq!(files.len(), 0);
        Ok(())
    }

    #[test]
    fn test_scan_directory_missing_dir_returns_error() -> Result<()> {
        let temp_dir = tempfile::TempDir::new()?;
        let missing = temp_dir.path().join("missing");
        let app = MarkdownViewerApp::new();
        assert!(app.scan_directory(&missing).is_err());
        Ok(())
    }

    #[test]
    fn test_scan_directory_entry_error() -> Result<()> {
        let temp_dir = tempfile::TempDir::new()?;
        std::fs::write(temp_dir.path().join("doc.md"), "# Doc")?;
        let _guard = ForcedScanEntryError::new();
        let app = MarkdownViewerApp::new();
        assert!(app.scan_directory(temp_dir.path()).is_err());
        Ok(())
    }

    #[test]
    fn test_resolve_scan_entry_propagates_error() {
        let err = std::io::Error::other("forced entry error");
        let result = MarkdownViewerApp::resolve_scan_entry(Err(err));
        assert!(result.is_err());
    }

    #[test]
    fn test_single_file_drop() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let file = temp_dir.path().join("test.md");
        std::fs::write(&file, "# Test")?;

        app.handle_file_drop(vec![file.clone()]);

        assert_eq!(app.current_file, Some(file));
        assert!(app.pending_files.is_empty());
        assert!(app.error_message.is_none());
        Ok(())
    }

    #[test]
    fn test_multiple_files_drop() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;

        let files: Vec<PathBuf> = (0..5)
            .map(|i| {
                let path = temp_dir.path().join(format!("file{}.md", i));
                std::fs::write(&path, format!("# File {}", i)).unwrap();
                path
            })
            .collect();

        app.handle_file_drop(files.clone());

        assert_eq!(app.current_file, Some(files[0].clone()));
        assert_eq!(app.pending_files.len(), 4);
        Ok(())
    }

    #[test]
    fn test_directory_drop() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;

        // Create files in directory
        std::fs::write(temp_dir.path().join("a.md"), "# A")?;
        std::fs::write(temp_dir.path().join("b.md"), "# B")?;
        std::fs::write(temp_dir.path().join("c.md"), "# C")?;

        app.handle_file_drop(vec![temp_dir.path().to_path_buf()]);

        assert!(app.current_file.is_some());
        assert_eq!(app.pending_files.len(), 2);
        Ok(())
    }

    #[test]
    fn test_invalid_file_drop() {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new().unwrap();
        let file = temp_dir.path().join("test.pdf");
        std::fs::write(&file, "fake pdf").unwrap();

        app.handle_file_drop(vec![file]);

        // Current file should remain None (or welcome sample)
        assert!(app.error_message.is_some());
        assert!(app
            .error_message
            .as_ref()
            .unwrap()
            .contains("Not a markdown file"));
    }

    #[test]
    fn test_too_many_files() {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new().unwrap();

        let files: Vec<PathBuf> = (0..60)
            .map(|i| {
                let path = temp_dir.path().join(format!("file{}.md", i));
                std::fs::write(&path, format!("# File {}", i)).unwrap();
                path
            })
            .collect();

        app.handle_file_drop(files);

        assert!(app.error_message.is_some());
        assert!(app
            .error_message
            .as_ref()
            .unwrap()
            .contains("Too many files"));
    }

    #[test]
    fn test_queue_navigation() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;

        let file1 = temp_dir.path().join("file1.md");
        let file2 = temp_dir.path().join("file2.md");

        std::fs::write(&file1, "# File 1")?;
        std::fs::write(&file2, "# File 2")?;

        // Load first file and queue second
        app.load_file(file1.clone(), true)?;
        app.pending_files.push_back(file2.clone());

        assert!(app.can_navigate_forward());
        app.navigate_forward();
        assert_eq!(app.current_file, Some(file2));
        assert!(app.pending_files.is_empty());

        Ok(())
    }

    #[test]
    fn test_mixed_valid_invalid_files() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;

        let md_file = temp_dir.path().join("test.md");
        let pdf_file = temp_dir.path().join("test.pdf");

        std::fs::write(&md_file, "# Test")?;
        std::fs::write(&pdf_file, "fake pdf")?;

        app.handle_file_drop(vec![md_file.clone(), pdf_file]);

        // Should open the valid markdown file
        assert_eq!(app.current_file, Some(md_file));
        // Should show error about the invalid file
        assert!(app.error_message.is_some());
        Ok(())
    }

    #[test]
    fn test_empty_directory_drop() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;

        app.handle_file_drop(vec![temp_dir.path().to_path_buf()]);

        assert!(app.error_message.is_some());
        assert!(app
            .error_message
            .as_ref()
            .unwrap()
            .contains("No markdown files"));
        Ok(())
    }

    #[test]
    fn test_toggle_write_mode_tracks_cursor_and_focus() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = false;
        app.raw_focus_requested = false;

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(3));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        app.toggle_write_mode(&ctx);
        assert!(app.write_enabled);
        assert!(app.raw_focus_requested);

        app.toggle_write_mode(&ctx);
        assert!(!app.write_enabled);
        assert_eq!(app.raw_cursor, Some(3));
    }

    #[test]
    fn test_toggle_write_mode_rendered_does_not_request_focus() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Rendered;
        app.write_enabled = false;
        app.raw_focus_requested = false;

        let ctx = egui::Context::default();
        app.toggle_write_mode(&ctx);

        assert!(app.write_enabled);
        assert!(!app.raw_focus_requested);
    }

    #[test]
    fn test_find_next_and_previous_wraps() {
        let mut app = MarkdownViewerApp::new();
        app.load_content("Alpha\n\nBeta\n\nGamma", Some("Search".to_string()));
        app.search_query = "beta".to_string();

        app.find_next();
        assert_eq!(app.last_match_index, Some(1));
        assert_eq!(app.pending_scroll_to_element, Some(1));

        app.find_next();
        assert_eq!(app.last_match_index, Some(1));

        app.find_previous();
        assert_eq!(app.last_match_index, Some(1));
    }

    #[test]
    fn test_update_impl_rendered_view_runs() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        app.nav_request = Some(NavigationRequest::PageDown);
        app.toggle_fullscreen = true;

        let mut temp_file = tempfile::Builder::new().suffix(".md").tempfile()?;
        temp_file.write_all(b"# Dropped\nContent")?;
        temp_file.flush()?;

        let ctx = egui::Context::default();
        let mut input = default_input();
        let vp = input
            .viewports
            .get_mut(&egui::ViewportId::ROOT)
            .expect("root viewport");
        vp.monitor_size = Some(egui::vec2(1024.0, 768.0));
        vp.outer_rect = Some(egui::Rect::from_min_size(
            egui::pos2(1200.0, 900.0),
            egui::vec2(800.0, 600.0),
        ));
        vp.inner_rect = Some(egui::Rect::from_min_size(
            egui::pos2(0.0, 0.0),
            egui::vec2(800.0, 600.0),
        ));
        vp.fullscreen = Some(false);
        vp.maximized = Some(false);
        input.hovered_files.push(egui::HoveredFile {
            path: Some(temp_file.path().to_path_buf()),
            ..Default::default()
        });
        input.dropped_files.push(egui::DroppedFile {
            path: Some(temp_file.path().to_path_buf()),
            ..Default::default()
        });

        run_app_frame(&mut app, &ctx, input);

        assert!(app.drag_hover);
        assert!(app.current_file.is_some());
        assert!(app.last_window_pos.is_some());
        assert!(app.last_window_size.is_some());
        Ok(())
    }

    #[test]
    fn test_update_impl_applies_size_adjustment() {
        let mut app = MarkdownViewerApp::new();
        let ctx = egui::Context::default();
        let mut input = default_input();
        let vp = input
            .viewports
            .get_mut(&egui::ViewportId::ROOT)
            .expect("root viewport");
        vp.monitor_size = Some(egui::vec2(800.0, 600.0));
        vp.outer_rect = Some(egui::Rect::from_min_size(
            egui::pos2(10.0, 10.0),
            egui::vec2(800.0, 600.0),
        ));
        vp.inner_rect = Some(egui::Rect::from_min_size(
            egui::pos2(0.0, 0.0),
            egui::vec2(200.0, 100.0),
        ));
        vp.fullscreen = Some(false);
        vp.maximized = Some(false);

        run_app_frame(&mut app, &ctx, input);

        assert!(app.last_window_size.is_some());
        assert!(app.last_window_pos.is_some());
    }

    #[test]
    fn test_update_impl_applies_deferred_toggles() {
        let mut app = MarkdownViewerApp::new();
        app.view_toggle_requested = true;
        app.write_toggle_requested = true;

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, default_input());

        assert_eq!(app.view_mode, ViewMode::Raw);
        assert!(app.write_enabled);
    }

    #[test]
    fn test_update_impl_reload_without_file_sets_error() {
        let mut app = MarkdownViewerApp::new();
        app.reload_requested = true;

        let ctx = egui::Context::default();
        let input = default_input();
        run_app_frame(&mut app, &ctx, input);

        assert!(app.error_message.is_some());
    }

    #[test]
    fn test_update_impl_raw_read_only_renders() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = false;
        app.raw_buffer = "Line 1\nLine 2".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        let input = default_input();
        run_app_frame(&mut app, &ctx, input);

        assert_eq!(app.raw_buffer, "Line 1\nLine 2");
    }

    #[test]
    fn test_update_impl_moves_raw_cursor() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line 1\nLine 2\nLine 3".to_string();
        app.current_content = app.raw_buffer.clone();
        app.pending_raw_cursor_line_move = Some(1);

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(0));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        let input = default_input();
        run_app_frame(&mut app, &ctx, input);

        assert!(app.raw_cursor.unwrap_or(0) > 0);
    }

    #[test]
    fn test_json_helpers_escape_and_opt() {
        assert_eq!(
            MarkdownViewerApp::json_escape("a\"b\\c\n\t\r"),
            "a\\\"b\\\\c\\n\\t\\r"
        );
        assert_eq!(MarkdownViewerApp::json_opt_f32(Some(1.23456)), "1.235");
        assert_eq!(MarkdownViewerApp::json_opt_f32(None), "null");
    }

    #[test]
    fn test_color_image_to_rgba_falls_back_on_invalid_buffer() {
        let image = egui::ColorImage {
            size: [1, 1],
            pixels: Vec::new(),
        };
        let rgba = MarkdownViewerApp::color_image_to_rgba(&image);
        assert_eq!(rgba.width(), 1);
        assert_eq!(rgba.height(), 1);
    }

    #[test]
    fn test_save_screenshot_image_crops_and_writes_metadata() -> Result<()> {
        let temp_dir = tempfile::TempDir::new()?;
        let output_path = temp_dir.path().join("shot.png");
        let config = ScreenshotConfig {
            output_path: output_path.clone(),
            viewport_width: 80.0,
            viewport_height: 60.0,
            content_only: true,
            scroll_ratio: Some(0.5),
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: Some("TestFont".to_string()),
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: Some(egui::Rect::from_min_size(
                egui::pos2(10.0, 5.0),
                egui::vec2(20.0, 10.0),
            )),
            pixels_per_point: 1.0,
            stable_frames: 3,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: Some(12.0),
            started: Instant::now(),
        };

        let mut image = egui::ColorImage::new([80, 60], Color32::BLACK);
        image.pixels[0] = Color32::from_rgb(10, 20, 30);

        MarkdownViewerApp::save_screenshot_image(&image, &snapshot)?;

        let saved = image::open(&output_path)?;
        assert_eq!(saved.width(), 20);
        assert_eq!(saved.height(), 10);

        let metadata = std::fs::read_to_string(output_path.with_extension("json"))?;
        assert!(metadata.contains("\"content_only\": true"));
        assert!(metadata.contains("\"font_source\": \"TestFont\""));
        Ok(())
    }

    #[test]
    fn test_update_screenshot_state_requests_and_scrolls() {
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let config = ScreenshotConfig {
            output_path: temp_dir.path().join("shot.png"),
            viewport_width: 100.0,
            viewport_height: 80.0,
            content_only: false,
            scroll_ratio: Some(0.5),
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Dark,
            font_source: None,
        };

        let mut app = MarkdownViewerApp::new();
        app.set_screenshot_mode(config);

        let ctx = egui::Context::default();
        let snapshot = ScrollSnapshot {
            content_size: egui::vec2(100.0, 200.0),
            inner_rect: egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(100.0, 100.0)),
            offset_y: 0.0,
        };
        app.update_screenshot_state(&ctx, Some(1), Some(snapshot));

        let target_offset = app
            .screenshot
            .as_ref()
            .and_then(|state| state.scroll_offset)
            .unwrap_or(0.0);
        let snapshot_ready = ScrollSnapshot {
            content_size: egui::vec2(100.0, 200.0),
            inner_rect: egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(100.0, 100.0)),
            offset_y: target_offset,
        };
        app.update_screenshot_state(&ctx, Some(1), Some(snapshot_ready));

        let state = app.screenshot.as_ref().expect("screenshot state");
        assert!(state.viewport_adjusted);
        assert!(state.requested);
        assert!(state.scroll_ready());
    }

    #[test]
    fn test_update_screenshot_state_stable_without_timeout() {
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let config = ScreenshotConfig {
            output_path: temp_dir.path().join("shot.png"),
            viewport_width: 120.0,
            viewport_height: 80.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 10_000,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let mut app = MarkdownViewerApp::new();
        app.screenshot = Some(ScreenshotState::new(config));

        let ctx = egui::Context::default();
        app.update_screenshot_state(&ctx, Some(1), None);

        let state = app.screenshot.as_ref().expect("screenshot state");
        assert!(state.requested);
        assert!(!state.timed_out);
    }

    #[test]
    fn test_update_screenshot_state_times_out_before_stable() {
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let config = ScreenshotConfig {
            output_path: temp_dir.path().join("shot.png"),
            viewport_width: 120.0,
            viewport_height: 80.0,
            content_only: false,
            scroll_ratio: Some(0.5),
            wait_ms: 0,
            settle_frames: 5,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let mut app = MarkdownViewerApp::new();
        app.screenshot = Some(ScreenshotState::new(config));

        let ctx = egui::Context::default();
        app.update_screenshot_state(&ctx, None, None);

        let state = app.screenshot.as_ref().expect("screenshot state");
        assert!(state.timed_out);
    }

    #[test]
    fn test_save_persists_window_state_when_no_screenshot() {
        let _lock = env_lock();
        let temp_dir = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp_dir.path());

        let mut app = MarkdownViewerApp::new();
        app.last_window_pos = Some([10.0, 20.0]);
        app.last_window_size = Some([800.0, 600.0]);
        let mut storage = DummyStorage;
        app.save(&mut storage);

        let path = config_dir.join("window_state.txt");
        assert!(path.exists());
    }

    #[test]
    fn test_save_skips_persist_when_screenshot_active() {
        let _lock = env_lock();
        let temp_dir = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp_dir.path());

        let mut app = MarkdownViewerApp::new();
        let config = ScreenshotConfig {
            output_path: temp_dir.path().join("shot.png"),
            viewport_width: 10.0,
            viewport_height: 10.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        app.set_screenshot_mode(config);
        let mut storage = DummyStorage;
        app.save(&mut storage);

        let path = temp_dir
            .path()
            .join("MarkdownView")
            .join("window_state.txt");
        assert!(!path.exists());
    }

    #[test]
    fn test_save_screenshot_image_crops_content_rect() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let output_path = temp_dir.path().join("shot.png");
        let config = ScreenshotConfig {
            output_path: output_path.clone(),
            viewport_width: 100.0,
            viewport_height: 80.0,
            content_only: true,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Dark,
            font_source: None,
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: Some(egui::Rect::from_min_max(
                egui::pos2(2.0, 2.0),
                egui::pos2(8.0, 8.0),
            )),
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: std::time::Instant::now(),
        };
        let image = egui::ColorImage::new([10, 10], Color32::from_rgb(10, 20, 30));
        MarkdownViewerApp::save_screenshot_image(&image, &snapshot)?;
        assert!(output_path.exists());
        assert!(output_path.with_extension("json").exists());
        Ok(())
    }

    #[test]
    fn test_save_screenshot_image_no_crop() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let output_path = temp_dir.path().join("shot.png");
        let config = ScreenshotConfig {
            output_path: output_path.clone(),
            viewport_width: 100.0,
            viewport_height: 80.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: None,
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: std::time::Instant::now(),
        };
        let image = egui::ColorImage::new([10, 10], Color32::from_rgb(10, 20, 30));
        MarkdownViewerApp::save_screenshot_image(&image, &snapshot)?;
        assert!(output_path.exists());
        Ok(())
    }

    #[test]
    fn test_save_screenshot_image_zero_content_rect_ignored() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let output_path = temp_dir.path().join("shot.png");
        let config = ScreenshotConfig {
            output_path: output_path.clone(),
            viewport_width: 100.0,
            viewport_height: 80.0,
            content_only: true,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Dark,
            font_source: None,
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: Some(egui::Rect::from_min_max(
                egui::pos2(5.0, 5.0),
                egui::pos2(5.0, 5.0),
            )),
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: std::time::Instant::now(),
        };
        let image = egui::ColorImage::new([10, 10], Color32::from_rgb(10, 20, 30));
        MarkdownViewerApp::save_screenshot_image(&image, &snapshot)?;
        let saved = image::open(&output_path)?;
        assert_eq!(saved.width(), 10);
        assert_eq!(saved.height(), 10);
        Ok(())
    }

    #[test]
    fn test_save_screenshot_image_content_only_without_rect() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let output_path = temp_dir.path().join("shot.png");
        let config = ScreenshotConfig {
            output_path: output_path.clone(),
            viewport_width: 100.0,
            viewport_height: 80.0,
            content_only: true,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: None,
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: std::time::Instant::now(),
        };
        let image = egui::ColorImage::new([10, 10], Color32::from_rgb(10, 20, 30));
        MarkdownViewerApp::save_screenshot_image(&image, &snapshot)?;
        assert!(output_path.exists());
        Ok(())
    }

    #[test]
    fn test_save_screenshot_image_height_zero_skips_crop() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let output_path = temp_dir.path().join("shot.png");
        let config = ScreenshotConfig {
            output_path: output_path.clone(),
            viewport_width: 100.0,
            viewport_height: 80.0,
            content_only: true,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: Some(egui::Rect::from_min_max(
                egui::pos2(2.0, 2.0),
                egui::pos2(8.0, 2.0),
            )),
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: std::time::Instant::now(),
        };
        let image = egui::ColorImage::new([10, 10], Color32::from_rgb(10, 20, 30));
        MarkdownViewerApp::save_screenshot_image(&image, &snapshot)?;
        let saved = image::open(&output_path)?;
        assert_eq!(saved.width(), 10);
        assert_eq!(saved.height(), 10);
        Ok(())
    }

    #[test]
    fn test_save_screenshot_image_empty_output_path_errors() {
        let config = ScreenshotConfig {
            output_path: PathBuf::new(),
            viewport_width: 100.0,
            viewport_height: 80.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: None,
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: std::time::Instant::now(),
        };
        let image = egui::ColorImage::new([10, 10], Color32::from_rgb(10, 20, 30));
        assert!(MarkdownViewerApp::save_screenshot_image(&image, &snapshot).is_err());
    }

    #[test]
    fn test_save_screenshot_image_crop_condition_variants() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let image = egui::ColorImage::new([10, 10], Color32::from_rgb(10, 20, 30));

        let make_config = |name: &str| ScreenshotConfig {
            output_path: temp_dir.path().join(name),
            viewport_width: 100.0,
            viewport_height: 80.0,
            content_only: true,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };

        let snapshot_y = ScreenshotSnapshot {
            config: make_config("shot_y.png"),
            content_rect: Some(egui::Rect::from_min_max(
                egui::pos2(0.0, 2.0),
                egui::pos2(10.0, 9.0),
            )),
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: std::time::Instant::now(),
        };
        MarkdownViewerApp::save_screenshot_image(&image, &snapshot_y)?;

        let snapshot_w = ScreenshotSnapshot {
            config: make_config("shot_w.png"),
            content_rect: Some(egui::Rect::from_min_max(
                egui::pos2(0.0, 0.0),
                egui::pos2(6.0, 10.0),
            )),
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: std::time::Instant::now(),
        };
        MarkdownViewerApp::save_screenshot_image(&image, &snapshot_w)?;

        let snapshot_h = ScreenshotSnapshot {
            config: make_config("shot_h.png"),
            content_rect: Some(egui::Rect::from_min_max(
                egui::pos2(0.0, 0.0),
                egui::pos2(10.0, 6.0),
            )),
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: std::time::Instant::now(),
        };
        MarkdownViewerApp::save_screenshot_image(&image, &snapshot_h)?;

        assert!(temp_dir.path().join("shot_y.png").exists());
        assert!(temp_dir.path().join("shot_w.png").exists());
        assert!(temp_dir.path().join("shot_h.png").exists());
        Ok(())
    }

    #[test]
    fn test_handle_screenshot_event_saves_file() -> Result<()> {
        let temp_dir = tempfile::TempDir::new()?;
        let output_path = temp_dir.path().join("shot.png");
        let config = ScreenshotConfig {
            output_path: output_path.clone(),
            viewport_width: 40.0,
            viewport_height: 30.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };

        let mut app = MarkdownViewerApp::new();
        app.set_screenshot_mode(config);

        let mut input = default_input();
        let image = egui::ColorImage::new([40, 30], Color32::WHITE);
        input.events.push(egui::Event::Screenshot {
            viewport_id: egui::ViewportId::ROOT,
            image: Arc::new(image),
        });

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, input);

        assert!(output_path.exists());
        assert!(output_path.with_extension("json").exists());
        assert!(app.screenshot.as_ref().is_some_and(|state| state.done));
        Ok(())
    }

    #[test]
    fn test_handle_shortcuts_sets_flags() {
        let mut app = MarkdownViewerApp::new();
        app.load_content("Alpha\n\nBeta\n\nGamma", Some("Search".to_string()));
        app.search_query = "beta".to_string();
        app.current_file = Some(PathBuf::from("dummy.md"));

        let mut input = default_input();
        input.events.extend([
            egui::Event::Key {
                key: egui::Key::F,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::CTRL,
            },
            egui::Event::Key {
                key: egui::Key::R,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::CTRL,
            },
            egui::Event::Key {
                key: egui::Key::E,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::CTRL,
            },
            egui::Event::Key {
                key: egui::Key::Plus,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::CTRL,
            },
            egui::Event::Key {
                key: egui::Key::Minus,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::CTRL,
            },
            egui::Event::Key {
                key: egui::Key::Num0,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::CTRL,
            },
            egui::Event::Key {
                key: egui::Key::ArrowLeft,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::ALT,
            },
            egui::Event::Key {
                key: egui::Key::ArrowRight,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::ALT,
            },
            egui::Event::Key {
                key: egui::Key::F3,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::NONE,
            },
            egui::Event::Key {
                key: egui::Key::F3,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::SHIFT,
            },
            egui::Event::Key {
                key: egui::Key::F11,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::NONE,
            },
            egui::Event::Key {
                key: egui::Key::F5,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::NONE,
            },
        ]);

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert!(app.show_search);
        assert!(app.search_focus_requested);
        assert!(app.view_toggle_requested);
        assert!(app.write_toggle_requested);
        assert!(app.toggle_fullscreen);
        assert!(app.reload_requested);
        assert!(app.last_match_index.is_some());
    }

    #[test]
    fn test_handle_shortcuts_navigation_keys() {
        let mut app = MarkdownViewerApp::new();
        let ctx = egui::Context::default();

        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::PageUp,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });
        assert!(matches!(app.nav_request, Some(NavigationRequest::PageUp)));

        app.nav_request = None;
        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::PageDown,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });
        assert!(matches!(app.nav_request, Some(NavigationRequest::PageDown)));

        app.nav_request = None;
        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::Home,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });
        assert!(matches!(app.nav_request, Some(NavigationRequest::Top)));

        app.nav_request = None;
        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::End,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });
        assert!(matches!(app.nav_request, Some(NavigationRequest::Bottom)));

        app.nav_request = None;
        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::ArrowUp,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });
        assert!(matches!(app.nav_request, Some(NavigationRequest::ScrollUp)));

        app.nav_request = None;
        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::ArrowDown,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });
        assert!(matches!(
            app.nav_request,
            Some(NavigationRequest::ScrollDown)
        ));
    }

    #[test]
    fn test_handle_shortcuts_escape_clears_search() {
        let mut app = MarkdownViewerApp::new();
        app.show_search = true;
        app.search_query = "alpha".to_string();
        app.last_query = "alpha".to_string();
        app.last_match_index = Some(0);
        app.pending_scroll_to_element = Some(1);

        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::Escape,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert!(!app.show_search);
        assert!(app.search_query.is_empty());
        assert!(app.last_query.is_empty());
        assert!(app.last_match_index.is_none());
        assert!(app.pending_scroll_to_element.is_none());
    }

    #[test]
    fn test_update_search_results_updates_matches() {
        let mut app = MarkdownViewerApp::new();
        app.load_content("Alpha\n\nBeta\n\nGamma", Some("Search".to_string()));

        app.search_query = "beta".to_string();
        app.update_search_results("");
        assert_eq!(app.last_query, "beta");
        assert_eq!(app.last_match_index, Some(1));
        assert_eq!(app.pending_scroll_to_element, Some(1));

        app.search_query.clear();
        app.update_search_results("beta");
        assert!(app.last_query.is_empty());
    }

    #[test]
    fn test_update_search_results_no_match_leaves_index_clear() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Alpha".to_string(),
        )])];
        app.search_query = "Beta".to_string();
        app.pending_scroll_to_element = None;

        app.update_search_results("");

        assert!(app.last_match_index.is_none());
        assert!(app.pending_scroll_to_element.is_none());
    }

    #[test]
    fn test_handle_shortcuts_ctrl_mouse_wheel_zoom() {
        let mut app = MarkdownViewerApp::new();
        let ctx = egui::Context::default();
        for delta in [20.0, -20.0] {
            let mut input = default_input();
            input.modifiers = egui::Modifiers::CTRL;
            input.events.push(egui::Event::MouseWheel {
                unit: egui::MouseWheelUnit::Point,
                delta: egui::vec2(0.0, delta),
                modifiers: egui::Modifiers::CTRL,
            });
            let _ = ctx.run(input, |ctx| {
                app.handle_shortcuts(ctx);
            });
        }
    }

    #[test]
    fn test_handle_shortcuts_ctrl_mouse_wheel_no_wheel_events() {
        let mut app = MarkdownViewerApp::new();
        let mut input = default_input();
        input.modifiers = egui::Modifiers::CTRL;
        input.events.push(egui::Event::Key {
            key: egui::Key::A,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::CTRL,
        });

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });
    }

    #[test]
    fn test_handle_shortcuts_raw_page_moves() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;

        let mut input = default_input();
        input.events.extend([
            egui::Event::Key {
                key: egui::Key::PageUp,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::NONE,
            },
            egui::Event::Key {
                key: egui::Key::PageDown,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::NONE,
            },
        ]);

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert!(app.pending_raw_cursor_line_move.is_some());
        assert!(app.raw_focus_requested);
    }

    #[test]
    fn test_render_helpers_cover_ui_paths() {
        let mut app = MarkdownViewerApp::new();
        app.current_content = "Hello".to_string();
        app.pending_files.push_back(PathBuf::from("queued.md"));
        app.drag_hover = true;
        app.show_search = true;
        app.search_query = "welcome".to_string();

        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            CentralPanel::default().show(ctx, |ui| {
                app.render_file_menu_contents(ui, false, Color32::WHITE);
                app.render_view_menu_contents(ui, ctx, false, Color32::WHITE);
                app.render_help_menu_contents(ui, false, Color32::WHITE);
            });
            app.render_status_bar(ctx);
            app.render_search_dialog(ctx);
            app.render_drag_overlay(ctx);
        });

        assert!(app.show_search);
    }

    #[test]
    fn test_screenshot_config_helpers() {
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let output_path = temp_dir.path().join("shot.png");
        let config = ScreenshotConfig {
            output_path: output_path.clone(),
            viewport_width: 100.0,
            viewport_height: 80.0,
            content_only: true,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };

        assert_eq!(config.metadata_path(), output_path.with_extension("json"));
        assert_eq!(config.theme.as_str(), "light");
        assert_eq!(ScreenshotTheme::Dark.as_str(), "dark");
    }

    #[test]
    fn test_open_file_dialog_forced_path() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let file_path = temp_dir.path().join("open.md");
        std::fs::write(&file_path, "# Open")?;
        let _forced = ForcedDialogPaths::new(Some(file_path.clone()), None);

        app.open_file_dialog();

        assert_eq!(app.current_file, Some(file_path));
        assert!(app.error_message.is_none());
        Ok(())
    }

    #[test]
    fn test_save_current_document_existing_path() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let file_path = temp_dir.path().join("save.md");
        app.current_file = Some(file_path.clone());
        app.current_content = "Saved content".to_string();

        app.save_current_document()?;

        let saved = std::fs::read_to_string(&file_path)?;
        assert_eq!(saved, "Saved content");
        Ok(())
    }

    #[test]
    fn test_save_current_document_forced_path() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let file_path = temp_dir.path().join("save_forced.md");
        let _forced = ForcedDialogPaths::new(None, Some(file_path.clone()));
        app.current_content = "Forced save".to_string();

        app.save_current_document()?;

        assert_eq!(app.current_file, Some(file_path.clone()));
        assert!(app.title.contains("save_forced.md"));
        let saved = std::fs::read_to_string(&file_path)?;
        assert_eq!(saved, "Forced save");
        Ok(())
    }

    #[test]
    fn test_save_current_document_forced_path_write_error() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let save_dir = temp_dir.path().join("save_dir");
        std::fs::create_dir_all(&save_dir)?;
        let _forced = ForcedDialogPaths::new(None, Some(save_dir));
        app.current_content = "Data".to_string();

        assert!(app.save_current_document().is_err());
        assert!(app.current_file.is_none());
        Ok(())
    }

    #[test]
    fn test_save_current_document_no_path_no_dialog() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let _forced = ForcedDialogPaths::new(None, None);
        app.current_content = "No save".to_string();

        app.save_current_document()?;

        assert!(app.current_file.is_none());
        Ok(())
    }

    #[test]
    fn test_restore_from_history_sets_state() {
        let mut app = MarkdownViewerApp::new();
        app.current_content = "first".to_string();
        app.current_file = Some(PathBuf::from("first.md"));
        app.title = "mdmdview - first".to_string();
        app.push_history();

        app.current_content = "second".to_string();
        app.current_file = None;
        app.title = "mdmdview - second".to_string();
        app.push_history();

        app.history_index = 0;
        app.restore_from_history();

        assert_eq!(app.current_file, Some(PathBuf::from("first.md")));
        assert_eq!(app.current_content, "first");
        assert_eq!(app.pending_scroll_to_element, Some(0));
    }

    #[test]
    fn test_restore_from_history_out_of_range_no_change() {
        let mut app = MarkdownViewerApp::new();
        app.current_content = "current".to_string();
        app.history_index = 3;

        app.restore_from_history();

        assert_eq!(app.current_content, "current");
        assert!(app.current_file.is_none());
    }

    #[test]
    fn test_handle_file_drop_mixed_files() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let good1 = temp_dir.path().join("good1.md");
        let good2 = temp_dir.path().join("good2.md");
        let bad = temp_dir.path().join("bad.pdf");
        std::fs::write(&good1, "# Good1")?;
        std::fs::write(&good2, "# Good2")?;
        std::fs::write(&bad, "nope")?;

        app.handle_file_drop(vec![bad, good1.clone(), good2.clone()]);

        assert_eq!(app.current_file, Some(good1));
        assert!(app.pending_files.contains(&good2));
        assert!(app.error_message.is_some());
        Ok(())
    }

    #[test]
    fn test_handle_file_drop_directory_no_markdown() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        app.handle_file_drop(vec![temp_dir.path().to_path_buf()]);
        assert!(app.error_message.is_some());
        Ok(())
    }

    #[test]
    fn test_handle_file_drop_too_many_files() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let mut files = Vec::new();
        for idx in 0..51 {
            let path = temp_dir.path().join(format!("file{}.md", idx));
            std::fs::write(&path, "# Test")?;
            files.push(path);
        }

        app.handle_file_drop(files);

        assert!(app.error_message.is_some());
        Ok(())
    }

    #[test]
    fn test_move_raw_cursor_lines_up_and_down() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line 1\nLine 2\nLine 3".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(0));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        app.move_raw_cursor_lines(&ctx, 1);
        let down_idx = app.raw_cursor.unwrap_or(0);
        assert!(down_idx > 0);

        app.move_raw_cursor_lines(&ctx, -1);
        let up_idx = app.raw_cursor.unwrap_or(0);
        assert!(up_idx <= down_idx);
    }

    #[test]
    fn test_move_raw_cursor_lines_early_return_when_not_raw() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Rendered;
        app.write_enabled = true;
        app.raw_cursor = Some(2);

        let ctx = egui::Context::default();
        app.move_raw_cursor_lines(&ctx, 1);

        assert_eq!(app.raw_cursor, Some(2));
    }

    #[test]
    fn test_move_raw_cursor_lines_write_disabled_returns() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = false;
        app.raw_cursor = Some(1);

        let ctx = egui::Context::default();
        app.move_raw_cursor_lines(&ctx, 1);

        assert_eq!(app.raw_cursor, Some(1));
    }

    #[test]
    fn test_move_raw_cursor_lines_without_text_state() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        app.move_raw_cursor_lines(&ctx, 1);

        assert!(app.raw_cursor.is_none());
    }

    #[test]
    fn test_move_raw_cursor_lines_uses_raw_cursor_when_char_range_missing() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line 1\nLine 2\nLine 3".to_string();
        app.current_content = app.raw_buffer.clone();
        app.raw_cursor = Some(7);

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        state.cursor.set_char_range(None);
        state.store(&ctx, editor_id);

        app.move_raw_cursor_lines(&ctx, 1);

        assert_eq!(app.raw_cursor, Some(14));
    }

    #[test]
    fn test_move_raw_cursor_lines_negative_at_start_no_loop() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line 1\nLine 2".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(0));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        app.move_raw_cursor_lines(&ctx, -1);
        assert_eq!(app.raw_cursor, Some(0));
    }

    #[test]
    fn test_move_raw_cursor_lines_down_without_newline() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(0));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        app.move_raw_cursor_lines(&ctx, 1);
        assert_eq!(app.raw_cursor, Some(app.raw_buffer.len()));
    }

    #[test]
    fn test_move_raw_cursor_lines_down_at_end_no_loop() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line 1\nLine 2".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(app.raw_buffer.len()));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        app.move_raw_cursor_lines(&ctx, 1);
        assert_eq!(app.raw_cursor, Some(app.raw_buffer.len()));
    }

    #[test]
    fn test_handle_shortcuts_open_accelerators() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let open_path = temp_dir.path().join("open.md");
        std::fs::write(&open_path, "# Open")?;
        let _forced = ForcedDialogPaths::new(Some(open_path.clone()), None);

        let mut input = default_input();
        input.events.extend([
            egui::Event::Key {
                key: egui::Key::O,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::CTRL,
            },
            egui::Event::Key {
                key: egui::Key::O,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::ALT,
            },
        ]);

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert_eq!(app.current_file, Some(open_path));
        Ok(())
    }

    #[test]
    fn test_handle_shortcuts_close_accelerators() {
        let mut app = MarkdownViewerApp::new();
        app.current_file = Some(PathBuf::from("dummy.md"));
        app.current_content = "# Dummy".to_string();

        let mut input = default_input();
        input.events.extend([
            egui::Event::Key {
                key: egui::Key::W,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::ALT,
            },
            egui::Event::Key {
                key: egui::Key::W,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::CTRL,
            },
            egui::Event::Key {
                key: egui::Key::Q,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::ALT,
            },
            egui::Event::Key {
                key: egui::Key::Q,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: egui::Modifiers::CTRL,
            },
        ]);

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert!(app.current_file.is_none());
    }

    #[test]
    fn test_handle_shortcuts_ctrl_save() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let file_path = temp_dir.path().join("save.md");
        app.current_file = Some(file_path.clone());
        app.current_content = "Saved".to_string();

        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::S,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::CTRL,
        });

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        let saved = std::fs::read_to_string(&file_path)?;
        assert_eq!(saved, "Saved");
        Ok(())
    }

    #[test]
    fn test_compute_window_adjustment_clamps() {
        let outer = egui::Rect::from_min_size(egui::pos2(-100.0, -100.0), egui::vec2(200.0, 100.0));
        let monitor = egui::vec2(800.0, 600.0);
        let adjusted =
            MarkdownViewerApp::compute_window_adjustment(Some(outer), Some(outer), Some(monitor));
        assert!(adjusted.is_some());

        let outer_ok = egui::Rect::from_min_size(egui::pos2(10.0, 10.0), egui::vec2(800.0, 600.0));
        let unchanged = MarkdownViewerApp::compute_window_adjustment(
            Some(outer_ok),
            Some(outer_ok),
            Some(monitor),
        );
        assert!(unchanged.is_none());
    }

    #[test]
    fn test_persist_window_state_updates_cache() {
        let _lock = env_lock();
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp_dir.path());
        let mut app = MarkdownViewerApp::new();
        app.last_window_pos = Some([10.0, 20.0]);
        app.last_window_size = Some([800.0, 600.0]);
        app.last_window_maximized = true;

        app.persist_window_state();

        assert!(app.last_persisted_state.is_some());
    }

    #[test]
    fn test_render_actions_trigger_menu_and_context() {
        let mut app = MarkdownViewerApp::new();
        app.current_content = "Hello".to_string();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Hello".to_string(),
        )])];
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let open_path = temp_dir.path().join("open.md");
        std::fs::write(&open_path, "# Open").expect("write");
        let save_path = temp_dir.path().join("save.md");
        let _forced = ForcedDialogPaths::new(Some(open_path), Some(save_path));
        let _actions = ForcedAppActions::new(&[
            "menu_open",
            "menu_save",
            "menu_close",
            "menu_reload",
            "menu_find",
            "menu_samples",
            "menu_bar_file",
            "menu_bar_view",
            "menu_bar_help",
            "menu_help_usage",
            "menu_help_about",
            "menu_back",
            "menu_forward",
            "menu_raw",
            "menu_write",
            "menu_wrap_raw",
            "menu_zoom_in",
            "menu_zoom_out",
            "menu_zoom_reset",
            "menu_fullscreen",
            "ctx_copy_all",
            "ctx_copy_markdown",
            "ctx_nav_top",
            "ctx_nav_bottom",
            "search_next",
            "search_prev",
            "search_close",
            "status_hover",
            "welcome_open",
            "welcome.md",
            "usage.md",
        ]);

        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            CentralPanel::default().show(ctx, |ui| {
                app.render_file_menu_contents(ui, false, Color32::WHITE);
                app.render_view_menu_contents(ui, ctx, false, Color32::WHITE);
                app.render_help_menu_contents(ui, false, Color32::WHITE);
                app.render_main_context_menu(ui);
            });
            app.show_search = true;
            app.render_search_dialog(ctx);
            app.render_menu_bar(ctx);
            app.render_status_bar(ctx);
        });
    }

    #[test]
    fn test_render_search_dialog_stays_open() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Alpha".to_string(),
        )])];
        app.show_search = true;
        app.search_query = "Alpha".to_string();

        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            app.render_search_dialog(ctx);
        });

        assert!(app.show_search);
        assert_eq!(app.search_query, "Alpha");
    }

    #[test]
    fn test_render_search_dialog_close_clears_state() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Alpha".to_string(),
        )])];
        app.show_search = true;
        app.search_query = "Alpha".to_string();
        app.last_query = "Alpha".to_string();
        app.last_match_index = Some(0);
        app.pending_scroll_to_element = Some(1);
        let _actions = ForcedAppActions::new(&["search_close"]);

        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            app.render_search_dialog(ctx);
        });

        assert!(!app.show_search);
        assert!(app.search_query.is_empty());
        assert!(app.last_query.is_empty());
        assert!(app.last_match_index.is_none());
        assert!(app.pending_scroll_to_element.is_none());
    }

    #[test]
    fn test_render_search_dialog_submitted_next_on_enter() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Alpha".to_string(),
        )])];
        app.show_search = true;
        app.search_query = "Alpha".to_string();
        app.search_focus_requested = true;

        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            app.render_search_dialog(ctx);
        });

        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::Enter,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });
        let _ = ctx.run(input, |ctx| {
            app.render_search_dialog(ctx);
        });

        assert!(app.last_match_index.is_some());
    }

    #[test]
    fn test_should_submit_search_requires_focus_and_enter() {
        assert!(MarkdownViewerApp::should_submit_search(true, true));
        assert!(!MarkdownViewerApp::should_submit_search(false, true));
        assert!(!MarkdownViewerApp::should_submit_search(true, false));
    }

    #[test]
    fn test_render_main_context_menu_no_actions() {
        let mut app = MarkdownViewerApp::new();
        app.current_content = "Hello".to_string();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Hello".to_string(),
        )])];

        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            CentralPanel::default().show(ctx, |ui| {
                app.render_main_context_menu(ui);
            });
        });

        assert!(app.nav_request.is_none());
    }

    #[test]
    fn test_render_menu_bar_light_mode() {
        let mut app = MarkdownViewerApp::new();
        let ctx = egui::Context::default();
        ctx.set_visuals(egui::Visuals::light());

        let _ = ctx.run(default_input(), |ctx| {
            app.render_menu_bar(ctx);
        });
    }

    #[test]
    fn test_handle_file_drop_empty_and_missing() {
        let mut app = MarkdownViewerApp::new();
        app.handle_file_drop(Vec::new());

        app.handle_file_drop(vec![PathBuf::from("missing.md")]);
        assert!(app
            .error_message
            .as_ref()
            .is_some_and(|msg| msg.contains("File not found")));
    }

    #[test]
    fn test_handle_file_drop_scan_error() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let _forced = ForcedScanError::new();

        app.handle_file_drop(vec![temp_dir.path().to_path_buf()]);

        assert!(app.error_message.is_some());
        Ok(())
    }

    #[test]
    fn test_handle_file_drop_load_failure() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let file_path = temp_dir.path().join("fail.md");
        std::fs::write(&file_path, "# Fail")?;
        let _forced = ForcedLoadError::new();

        app.handle_file_drop(vec![file_path]);

        assert!(app
            .error_message
            .as_ref()
            .is_some_and(|msg| msg.contains("Failed to load file")));
        Ok(())
    }

    #[test]
    fn test_handle_shortcuts_ctrl_f_sets_match_index() {
        let mut app = MarkdownViewerApp::new();
        app.last_match_index = None;

        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::F,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::CTRL,
        });

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert_eq!(app.last_match_index, Some(0));
    }

    #[test]
    fn test_handle_shortcuts_ctrl_f_preserves_match_index() {
        let mut app = MarkdownViewerApp::new();
        app.last_match_index = Some(2);

        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::F,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::CTRL,
        });

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert_eq!(app.last_match_index, Some(2));
    }

    #[test]
    fn test_handle_shortcuts_ctrl_mouse_wheel_zoom_directions() {
        let mut app = MarkdownViewerApp::new();
        let ctx = egui::Context::default();

        let mut input = default_input();
        input.modifiers = egui::Modifiers::CTRL;
        input.events.push(egui::Event::MouseWheel {
            unit: egui::MouseWheelUnit::Point,
            delta: egui::vec2(0.0, 20.0),
            modifiers: egui::Modifiers::CTRL,
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        let mut input = default_input();
        input.modifiers = egui::Modifiers::CTRL;
        input.events.push(egui::Event::MouseWheel {
            unit: egui::MouseWheelUnit::Point,
            delta: egui::vec2(0.0, -20.0),
            modifiers: egui::Modifiers::CTRL,
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });
    }

    #[test]
    fn test_handle_shortcuts_ctrl_mouse_wheel_without_events() {
        let mut app = MarkdownViewerApp::new();
        let ctx = egui::Context::default();
        let mut input = default_input();
        input.modifiers = egui::Modifiers::CTRL;
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });
    }

    #[test]
    fn test_handle_shortcuts_ctrl_save_error() {
        let mut app = MarkdownViewerApp::new();
        // Use a path that doesn't exist on any platform
        app.current_file = Some(PathBuf::from("/nonexistent/dir/save.md"));
        app.current_content = "Data".to_string();

        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::S,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::CTRL,
        });

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert!(app.error_message.is_some());
    }

    #[test]
    fn test_move_raw_cursor_lines_edge_cases() {
        let mut app = MarkdownViewerApp::new();
        app.raw_buffer = "Line 1\nLine 2\nLine 3\nLine 4".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        app.move_raw_cursor_lines(&ctx, 1);

        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(3));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        app.move_raw_cursor_lines(&ctx, 0);
        app.move_raw_cursor_lines(&ctx, 2);
        app.move_raw_cursor_lines(&ctx, -2);
    }

    #[test]
    fn test_render_status_bar_no_file_and_tooltip() {
        let mut app = MarkdownViewerApp::new();
        app.current_file = None;
        app.parsed_elements.clear();

        let _actions = ForcedAppActions::new(&["status_hover"]);
        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            app.render_status_bar(ctx);
        });
    }

    #[test]
    fn test_render_status_bar_table_stats() {
        let mut app = MarkdownViewerApp::new();
        app.current_file = None;
        app.parsed_elements = vec![MarkdownElement::Table {
            headers: vec![vec![InlineSpan::Text("H".to_string())]],
            rows: vec![vec![vec![InlineSpan::Text("C".to_string())]]],
            alignments: vec![pulldown_cmark::Alignment::Left],
        }];

        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            CentralPanel::default().show(ctx, |ui| {
                app.renderer.render_to_ui(ui, &app.parsed_elements);
            });
            app.render_status_bar(ctx);
        });
    }

    #[test]
    fn test_render_status_bar_pending_load_and_queue() {
        let mut app = MarkdownViewerApp::new();
        app.pending_file_load = Some(PendingFileLoad {
            id: 1,
            path: PathBuf::from("pending.md"),
        });
        app.pending_files.push_back(PathBuf::from("queued.md"));

        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            app.render_status_bar(ctx);
        });
    }

    #[test]
    fn test_update_impl_welcome_open_button() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements.clear();
        app.current_content.clear();
        app.error_message = None;

        let temp_dir = tempfile::TempDir::new()?;
        let file_path = temp_dir.path().join("welcome.md");
        std::fs::write(&file_path, "# Welcome")?;
        let _forced = ForcedDialogPaths::new(Some(file_path.clone()), None);
        let _actions = ForcedAppActions::new(&["welcome_open"]);

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, default_input());

        assert_eq!(app.current_file, Some(file_path));
        Ok(())
    }

    #[test]
    fn test_update_impl_welcome_screen_no_open() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements.clear();
        app.current_content.clear();
        app.error_message = None;

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, default_input());

        assert!(app.current_file.is_none());
    }

    #[test]
    fn test_update_impl_welcome_screen_with_error_message() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements.clear();
        app.current_content.clear();
        app.error_message = Some("Load failed".to_string());

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, default_input());

        assert!(app.error_message.is_some());
    }

    #[test]
    fn test_update_impl_navigation_requests() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Nav".to_string(),
        )])];
        let ctx = egui::Context::default();
        let input = default_input();
        let navs = [
            NavigationRequest::Top,
            NavigationRequest::Bottom,
            NavigationRequest::PageUp,
            NavigationRequest::PageDown,
            NavigationRequest::ScrollUp,
            NavigationRequest::ScrollDown,
        ];
        for nav in navs {
            app.nav_request = Some(nav);
            run_app_frame(&mut app, &ctx, input.clone());
        }
    }

    #[test]
    fn test_update_impl_anchor_and_search_highlight() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Header {
            level: 1,
            spans: vec![InlineSpan::Text("Section".to_string())],
            id: "section".to_string(),
        }];
        app.current_content = "# Section".to_string();
        app.show_search = true;
        app.search_query = "Section".to_string();
        app.pending_scroll_to_element = Some(0);
        app.renderer.trigger_link("#section");

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, default_input());

        app.show_search = false;
        app.search_query.clear();
        app.last_query = "Section".to_string();
        run_app_frame(&mut app, &ctx, default_input());
    }

    #[test]
    fn test_update_impl_search_highlight_empty_query_prefers_last() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Find me".to_string(),
        )])];
        app.current_content = "Find me".to_string();
        app.show_search = true;
        app.search_query.clear();
        app.last_query = "Find".to_string();

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, default_input());
    }

    #[test]
    fn test_update_impl_missing_anchor_and_scroll_target() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Header {
            level: 1,
            spans: vec![InlineSpan::Text("Known".to_string())],
            id: "known".to_string(),
        }];
        app.current_content = "# Known".to_string();
        app.renderer.trigger_link("#missing");
        app.pending_scroll_to_element = Some(99);

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, default_input());

        assert!(app.pending_scroll_to_element.is_none());
    }

    #[test]
    fn test_update_impl_raw_cursor_restore_without_state() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line 1\nLine 2".to_string();
        app.current_content = app.raw_buffer.clone();
        app.raw_cursor = Some(3);

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, default_input());
    }

    #[test]
    fn test_update_impl_raw_cursor_without_char_range() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        state.cursor.set_char_range(None);
        state.store(&ctx, editor_id);

        run_app_frame(&mut app, &ctx, default_input());

        assert!(app.raw_cursor.is_none());
    }

    #[test]
    fn test_update_impl_raw_edit_updates_content() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_focus_requested = true;
        app.raw_buffer = "Line".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        let mut input = default_input();
        input.focused = true;
        run_app_frame(&mut app, &ctx, input);

        let mut input = default_input();
        input.focused = true;
        input.events.push(egui::Event::Text("X".to_string()));
        run_app_frame(&mut app, &ctx, input);

        assert!(app.current_content.contains('X'));
    }

    #[test]
    fn test_update_impl_raw_edit_parse_error_sets_message() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_focus_requested = true;
        app.raw_buffer = "Line".to_string();
        app.current_content = app.raw_buffer.clone();
        crate::markdown_renderer::force_parse_error_once();

        let ctx = egui::Context::default();
        let mut input = default_input();
        input.focused = true;
        run_app_frame(&mut app, &ctx, input);

        let mut input = default_input();
        input.focused = true;
        input.events.push(egui::Event::Text("X".to_string()));
        run_app_frame(&mut app, &ctx, input);

        assert!(app.error_message.is_some());
    }

    #[test]
    fn test_update_impl_screenshot_scroll_offset() {
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let config = ScreenshotConfig {
            output_path: temp_dir.path().join("shot.png"),
            viewport_width: 80.0,
            viewport_height: 60.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let mut app = MarkdownViewerApp::new();
        app.screenshot = Some(ScreenshotState::new(config));
        let state = app.screenshot.as_mut().expect("screenshot state");
        state.scroll_offset = Some(10.0);

        let ctx = egui::Context::default();
        run_app_frame(&mut app, &ctx, default_input());
    }

    #[test]
    fn test_save_screenshot_image_creates_parent_dir() -> Result<()> {
        let temp_dir = tempfile::TempDir::new()?;
        let nested = temp_dir.path().join("nested").join("shot.png");
        let config = ScreenshotConfig {
            output_path: nested.clone(),
            viewport_width: 40.0,
            viewport_height: 30.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: None,
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: Instant::now(),
        };
        let image = egui::ColorImage::new([40, 30], Color32::WHITE);
        MarkdownViewerApp::save_screenshot_image(&image, &snapshot)?;
        assert!(nested.exists());
        assert!(nested.with_extension("json").exists());
        Ok(())
    }

    #[test]
    fn test_save_screenshot_image_parent_dir_error() -> Result<()> {
        let temp_dir = tempfile::TempDir::new()?;
        let parent_file = temp_dir.path().join("parent_file");
        std::fs::write(&parent_file, "data")?;
        let output_path = parent_file.join("shot.png");
        let config = ScreenshotConfig {
            output_path,
            viewport_width: 40.0,
            viewport_height: 30.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: None,
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: Instant::now(),
        };
        let image = egui::ColorImage::new([40, 30], Color32::WHITE);
        assert!(MarkdownViewerApp::save_screenshot_image(&image, &snapshot).is_err());
        Ok(())
    }

    #[test]
    fn test_save_screenshot_image_metadata_write_error() -> Result<()> {
        let temp_dir = tempfile::TempDir::new()?;
        let output_path = temp_dir.path().join("shot.png");
        std::fs::create_dir(output_path.with_extension("json"))?;
        let config = ScreenshotConfig {
            output_path: output_path.clone(),
            viewport_width: 40.0,
            viewport_height: 30.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let snapshot = ScreenshotSnapshot {
            config,
            content_rect: None,
            pixels_per_point: 1.0,
            stable_frames: 0,
            timed_out: false,
            pending_renders: false,
            last_scroll_offset: None,
            started: Instant::now(),
        };
        let image = egui::ColorImage::new([40, 30], Color32::WHITE);
        assert!(MarkdownViewerApp::save_screenshot_image(&image, &snapshot).is_err());
        assert!(output_path.exists());
        Ok(())
    }

    #[test]
    fn test_handle_screenshot_events_branches() {
        let mut app = MarkdownViewerApp::new();
        let ctx = egui::Context::default();

        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::A,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_screenshot_events(ctx);
        });

        let image = egui::ColorImage::new([10, 10], Color32::WHITE);
        let mut input = default_input();
        input.events.push(egui::Event::Screenshot {
            viewport_id: egui::ViewportId::ROOT,
            image: std::sync::Arc::new(image),
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_screenshot_events(ctx);
        });

        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let config = ScreenshotConfig {
            output_path: temp_dir.path().to_path_buf(),
            viewport_width: 40.0,
            viewport_height: 30.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        app.screenshot = Some(ScreenshotState::new(config));
        let state = app.screenshot.as_mut().expect("screenshot state");
        state.done = true;
        let image = egui::ColorImage::new([10, 10], Color32::WHITE);
        let mut input = default_input();
        input.events.push(egui::Event::Screenshot {
            viewport_id: egui::ViewportId::ROOT,
            image: std::sync::Arc::new(image),
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_screenshot_events(ctx);
        });
    }

    #[test]
    fn test_screenshot_state_helpers() {
        let config = ScreenshotConfig {
            output_path: PathBuf::from("dummy.png"),
            viewport_width: 120.0,
            viewport_height: 80.0,
            content_only: false,
            scroll_ratio: Some(0.5),
            wait_ms: 500,
            settle_frames: 2,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let mut state = ScreenshotState::new(config);
        assert!(!state.scroll_ready());

        let snapshot = ScrollSnapshot {
            content_size: egui::vec2(200.0, 400.0),
            inner_rect: egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 200.0)),
            offset_y: 0.0,
        };
        assert!(state.record_scroll(snapshot));
        assert!(!state.record_scroll(snapshot));

        state.update_stability(None, None);
        state.last_scroll_offset = Some(0.0);
        state.update_stability(Some(1), Some(10.0));
    }

    #[test]
    fn test_screenshot_is_stable_requires_no_pending() {
        assert!(MarkdownViewerApp::screenshot_is_stable(true, 2, 2, false));
        assert!(!MarkdownViewerApp::screenshot_is_stable(true, 2, 2, true));
        assert!(!MarkdownViewerApp::screenshot_is_stable(false, 2, 2, false));
        assert!(!MarkdownViewerApp::screenshot_is_stable(true, 1, 2, false));
    }

    #[test]
    fn test_record_scroll_without_ratio_returns_false() {
        let config = ScreenshotConfig {
            output_path: PathBuf::from("dummy.png"),
            viewport_width: 120.0,
            viewport_height: 80.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 500,
            settle_frames: 2,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let mut state = ScreenshotState::new(config);
        let snapshot = ScrollSnapshot {
            content_size: egui::vec2(200.0, 400.0),
            inner_rect: egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 200.0)),
            offset_y: 0.0,
        };
        assert!(!state.record_scroll(snapshot));
        assert!(state.scroll_offset.is_none());
    }

    #[test]
    fn test_toggle_write_mode_captures_cursor() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(2));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        app.toggle_write_mode(&ctx);
        assert!(!app.write_enabled);
        assert_eq!(app.raw_cursor, Some(2));
    }

    #[test]
    fn test_toggle_write_mode_without_text_state() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;

        let ctx = egui::Context::default();
        app.toggle_write_mode(&ctx);
        assert!(!app.write_enabled);
        assert!(app.raw_cursor.is_none());
    }

    #[test]
    fn test_toggle_write_mode_without_cursor_range() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        state.cursor.set_char_range(None);
        state.store(&ctx, editor_id);

        app.toggle_write_mode(&ctx);
        assert!(!app.write_enabled);
        assert!(app.raw_cursor.is_none());
    }

    #[test]
    fn test_move_raw_cursor_lines_moves_to_end() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;
        app.raw_buffer = "Line 1\nLine 2".to_string();
        app.current_content = app.raw_buffer.clone();

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(0));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        app.move_raw_cursor_lines(&ctx, 2);
        assert_eq!(app.raw_cursor, Some(app.raw_buffer.len()));
    }

    #[test]
    fn test_find_next_uses_last_query() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Hello".to_string(),
        )])];
        app.search_query.clear();
        app.last_query = "Hello".to_string();

        app.find_next();
        assert_eq!(app.last_match_index, Some(0));
    }

    #[test]
    fn test_find_previous_wraps_from_start() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![
            MarkdownElement::Paragraph(vec![InlineSpan::Text("Match".to_string())]),
            MarkdownElement::Paragraph(vec![InlineSpan::Text("Other".to_string())]),
        ];
        app.search_query.clear();
        app.last_query = "Match".to_string();
        app.last_match_index = Some(0);

        app.find_previous();
        assert_eq!(app.last_match_index, Some(0));
    }

    #[test]
    fn test_set_zoom_scale_on_app() {
        let mut app = MarkdownViewerApp::new();
        let before = app.renderer.font_sizes().body;
        app.set_zoom_scale(1.5);
        assert_ne!(app.renderer.font_sizes().body, before);
    }

    #[test]
    fn test_handle_file_drop_no_valid_files() {
        let mut app = MarkdownViewerApp::new();
        app.handle_file_drop(vec![
            PathBuf::from("missing1.md"),
            PathBuf::from("missing2.md"),
        ]);
        assert!(app
            .error_message
            .as_ref()
            .is_some_and(|msg| msg.contains("No valid files")));
    }

    #[test]
    fn test_compute_window_adjustment_missing_monitor_returns_none() {
        let outer = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 100.0));
        let adjusted = MarkdownViewerApp::compute_window_adjustment(Some(outer), Some(outer), None);
        assert!(adjusted.is_none());
    }

    #[test]
    fn test_compute_window_adjustment_invalid_monitor_returns_none() {
        let outer = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 100.0));
        let adjusted = MarkdownViewerApp::compute_window_adjustment(
            Some(outer),
            Some(outer),
            Some(egui::vec2(0.0, 0.0)),
        );
        assert!(adjusted.is_none());
    }

    #[test]
    fn test_compute_window_adjustment_invalid_monitor_height_returns_none() {
        let outer = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(200.0, 100.0));
        let adjusted = MarkdownViewerApp::compute_window_adjustment(
            Some(outer),
            Some(outer),
            Some(egui::vec2(800.0, 0.0)),
        );
        assert!(adjusted.is_none());
    }

    #[test]
    fn test_compute_window_adjustment_handles_nan_and_clamps() {
        let outer = egui::Rect::from_min_size(
            egui::pos2(f32::NAN, f32::NAN),
            egui::vec2(f32::NAN, f32::NAN),
        );
        let adjusted = MarkdownViewerApp::compute_window_adjustment(
            Some(outer),
            Some(outer),
            Some(egui::vec2(800.0, 600.0)),
        );
        assert!(adjusted.is_some());

        let outer = egui::Rect::from_min_size(egui::pos2(0.0, 0.0), egui::vec2(5000.0, 4000.0));
        let adjusted = MarkdownViewerApp::compute_window_adjustment(
            Some(outer),
            Some(outer),
            Some(egui::vec2(800.0, 600.0)),
        );
        assert!(adjusted.is_some());
    }

    #[test]
    fn test_compute_window_adjustment_invalid_pos_only() {
        let outer = egui::Rect::from_min_size(egui::pos2(f32::NAN, 0.0), egui::vec2(800.0, 600.0));
        let inner = egui::Rect::from_min_size(egui::pos2(10.0, 20.0), egui::vec2(800.0, 600.0));
        let adjusted = MarkdownViewerApp::compute_window_adjustment(
            Some(outer),
            Some(inner),
            Some(egui::vec2(1200.0, 900.0)),
        )
        .expect("adjustment");
        assert!(adjusted.pos.is_some());
        assert!(adjusted.size.is_none());
    }

    #[test]
    fn test_compute_window_adjustment_invalid_pos_y_only() {
        let outer = egui::Rect::from_min_size(egui::pos2(0.0, f32::NAN), egui::vec2(800.0, 600.0));
        let inner = egui::Rect::from_min_size(egui::pos2(10.0, 20.0), egui::vec2(800.0, 600.0));
        let adjusted = MarkdownViewerApp::compute_window_adjustment(
            Some(outer),
            Some(inner),
            Some(egui::vec2(1200.0, 900.0)),
        )
        .expect("adjustment");
        assert!(adjusted.pos.is_some());
        assert!(adjusted.size.is_none());
    }

    #[test]
    fn test_compute_window_adjustment_invalid_size_only() {
        let outer = egui::Rect::from_min_size(egui::pos2(10.0, 20.0), egui::vec2(800.0, f32::NAN));
        let adjusted = MarkdownViewerApp::compute_window_adjustment(
            Some(outer),
            Some(outer),
            Some(egui::vec2(1200.0, 900.0)),
        )
        .expect("adjustment");
        assert!(adjusted.pos.is_none());
        assert!(adjusted.size.is_some());
    }

    #[test]
    fn test_push_history_caps_max() {
        let mut app = MarkdownViewerApp::new();
        app.max_history = 1;
        app.current_content = "First".to_string();
        app.title = "First".to_string();
        app.push_history();

        app.current_content = "Second".to_string();
        app.title = "Second".to_string();
        app.push_history();

        assert_eq!(app.history.len(), 1);
        assert_eq!(app.history[0].content, "Second");
    }

    #[test]
    fn test_navigate_forward_pending_error() {
        let mut app = MarkdownViewerApp::new();
        let _forced = ForcedLoadError::new();
        app.pending_files.push_back(PathBuf::from("missing.md"));

        assert!(app.navigate_forward());
        assert!(app.error_message.is_some());
    }

    #[test]
    fn test_navigate_forward_history() {
        let mut app = MarkdownViewerApp::new();
        app.history = vec![
            HistoryEntry {
                file_path: None,
                title: "One".to_string(),
                content: "One".to_string(),
            },
            HistoryEntry {
                file_path: None,
                title: "Two".to_string(),
                content: "Two".to_string(),
            },
        ];
        app.history_index = 0;

        assert!(app.navigate_forward());
        assert_eq!(app.history_index, 1);
        assert_eq!(app.current_content, "Two");
    }

    #[test]
    fn test_restore_from_history_parse_error() {
        let mut app = MarkdownViewerApp::new();
        app.history = vec![HistoryEntry {
            file_path: None,
            title: "Bad".to_string(),
            content: "Bad".to_string(),
        }];
        app.history_index = 0;
        crate::markdown_renderer::force_parse_error_once();

        app.restore_from_history();
        assert!(app.error_message.is_some());
    }

    #[test]
    fn test_open_file_dialog_load_error() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let temp_dir = tempfile::TempDir::new()?;
        let open_path = temp_dir.path().join("open.md");
        std::fs::write(&open_path, "# Open")?;
        let _forced_path = ForcedDialogPaths::new(Some(open_path), None);
        let _forced_load = ForcedLoadError::new();

        app.open_file_dialog();
        assert!(app
            .error_message
            .as_ref()
            .is_some_and(|msg| msg.contains("Failed to open file")));
        Ok(())
    }

    #[test]
    fn test_toggle_view_mode_captures_cursor() {
        let mut app = MarkdownViewerApp::new();
        app.view_mode = ViewMode::Raw;
        app.write_enabled = true;

        let ctx = egui::Context::default();
        let editor_id = egui::Id::new("raw_editor");
        let mut state = egui::text_edit::TextEditState::default();
        let cr = egui::text::CCursorRange::one(egui::text::CCursor::new(4));
        state.cursor.set_char_range(Some(cr));
        state.store(&ctx, editor_id);

        app.toggle_view_mode(&ctx);
        assert_eq!(app.raw_cursor, Some(4));
        assert_eq!(app.view_mode, ViewMode::Rendered);
    }

    #[test]
    fn test_handle_shortcuts_shift_f3_triggers_previous() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Nav".to_string(),
        )])];
        app.search_query = "Nav".to_string();

        let mut input = default_input();
        input.modifiers = egui::Modifiers::SHIFT;
        input.events.push(egui::Event::Key {
            key: egui::Key::F3,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::SHIFT,
        });

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert_eq!(app.last_match_index, Some(0));
    }

    #[test]
    fn test_handle_shortcuts_f3_triggers_next() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![MarkdownElement::Paragraph(vec![InlineSpan::Text(
            "Nav".to_string(),
        )])];
        app.search_query = "Nav".to_string();

        let mut input = default_input();
        input.events.push(egui::Event::Key {
            key: egui::Key::F3,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::NONE,
        });

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert_eq!(app.last_match_index, Some(0));
    }

    #[test]
    fn test_handle_shortcuts_alt_f3_triggers_previous() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![
            MarkdownElement::Paragraph(vec![InlineSpan::Text("Match".to_string())]),
            MarkdownElement::Paragraph(vec![InlineSpan::Text("Match".to_string())]),
            MarkdownElement::Paragraph(vec![InlineSpan::Text("Match".to_string())]),
        ];
        app.search_query = "Match".to_string();
        app.last_match_index = Some(1);
        app.pending_scroll_to_element = None;

        let mut input = default_input();
        input.modifiers = egui::Modifiers::ALT;
        input.events.push(egui::Event::Key {
            key: egui::Key::F3,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::ALT,
        });

        let ctx = egui::Context::default();
        let _ = ctx.run(input, |ctx| {
            app.handle_shortcuts(ctx);
        });

        assert_eq!(app.last_match_index, Some(0));
        assert_eq!(app.pending_scroll_to_element, Some(0));
    }

    #[test]
    fn test_render_file_menu_save_error_sets_message() {
        let mut app = MarkdownViewerApp::new();
        // Use a path that doesn't exist on any platform
        app.current_file = Some(PathBuf::from("/nonexistent/dir/save.md"));
        app.current_content = "Data".to_string();
        let _actions = ForcedAppActions::new(&["menu_save"]);

        let ctx = egui::Context::default();
        let _ = ctx.run(default_input(), |ctx| {
            egui::CentralPanel::default().show(ctx, |ui| {
                app.render_file_menu_contents(ui, false, Color32::WHITE);
            });
        });

        assert!(app.error_message.is_some());
    }

    #[test]
    fn test_menu_text_with_mnemonic_prefix_and_underline() {
        let job = MarkdownViewerApp::menu_text_with_mnemonic(
            Some("Alt+"),
            "Open",
            'O',
            true,
            Color32::WHITE,
        );
        assert!(!job.sections.is_empty());
    }

    #[test]
    fn test_menu_text_with_mnemonic_no_underline() {
        let job =
            MarkdownViewerApp::menu_text_with_mnemonic(None, "Save", 'S', false, Color32::WHITE);
        assert!(!job.sections.is_empty());
    }

    #[test]
    fn test_menu_text_with_mnemonic_missing_character() {
        let job =
            MarkdownViewerApp::menu_text_with_mnemonic(None, "Save", 'Z', true, Color32::WHITE);
        assert!(!job.sections.is_empty());
    }

    #[test]
    fn test_persist_window_state_no_change_returns_early() {
        let mut app = MarkdownViewerApp::new();
        app.last_window_pos = Some([10.0, 10.0]);
        app.last_window_size = Some([800.0, 600.0]);
        app.last_window_maximized = false;
        app.last_persisted_state = Some(WindowState {
            pos: [10.0, 10.0],
            size: [800.0, 600.0],
            maximized: false,
        });

        app.persist_window_state();
        assert!(app.last_persisted_state.is_some());
    }

    #[test]
    fn test_persist_window_state_without_window_state() {
        let mut app = MarkdownViewerApp::new();
        app.last_window_pos = None;
        app.last_window_size = None;

        app.persist_window_state();
        assert!(app.last_persisted_state.is_none());
    }

    #[test]
    #[cfg(windows)]
    fn test_persist_window_state_save_failure() {
        let _lock = env_lock();
        let temp_file = NamedTempFile::new().expect("temp file");
        let _guard = EnvGuard::set("APPDATA", temp_file.path().to_string_lossy().as_ref());

        let mut app = MarkdownViewerApp::new();
        app.last_window_pos = Some([10.0, 10.0]);
        app.last_window_size = Some([800.0, 600.0]);
        app.last_window_maximized = false;

        app.persist_window_state();
        assert!(app.last_persisted_state.is_none());
    }

    #[test]
    fn test_should_persist_window_state_screenshot_blocks() {
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let config = ScreenshotConfig {
            output_path: temp_dir.path().join("shot.png"),
            viewport_width: 80.0,
            viewport_height: 60.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let mut app = MarkdownViewerApp::new();
        app.screenshot = Some(ScreenshotState::new(config));
        assert!(!app.should_persist_window_state());
    }

    #[test]
    fn test_should_persist_window_state_recently_persisted() {
        let mut app = MarkdownViewerApp::new();
        app.last_window_pos = Some([10.0, 10.0]);
        app.last_window_size = Some([800.0, 600.0]);
        app.last_persist_instant = std::time::Instant::now();
        assert!(!app.should_persist_window_state());
    }

    #[test]
    fn test_should_persist_window_state_without_window_state() {
        let mut app = MarkdownViewerApp::new();
        app.last_persist_instant = std::time::Instant::now() - std::time::Duration::from_secs(2);
        assert!(!app.should_persist_window_state());
    }

    #[test]
    fn test_should_persist_window_state_when_changed() {
        let mut app = MarkdownViewerApp::new();
        app.last_window_pos = Some([10.0, 10.0]);
        app.last_window_size = Some([800.0, 600.0]);
        app.last_persist_instant = std::time::Instant::now() - std::time::Duration::from_secs(2);

        assert!(app.should_persist_window_state());
    }

    #[test]
    fn test_update_impl_persists_window_state_when_due() {
        let _lock = env_lock();
        let temp_dir = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp_dir.path());

        let mut app = MarkdownViewerApp::new();
        app.last_persist_instant = std::time::Instant::now() - std::time::Duration::from_secs(2);

        let ctx = egui::Context::default();
        let mut input = default_input();
        let vp = input
            .viewports
            .get_mut(&egui::ViewportId::ROOT)
            .expect("root viewport");
        vp.monitor_size = Some(egui::vec2(1024.0, 768.0));
        vp.outer_rect = Some(egui::Rect::from_min_size(
            egui::pos2(10.0, 10.0),
            egui::vec2(800.0, 600.0),
        ));
        vp.inner_rect = Some(egui::Rect::from_min_size(
            egui::pos2(20.0, 20.0),
            egui::vec2(780.0, 580.0),
        ));
        vp.fullscreen = Some(false);
        vp.maximized = Some(false);

        run_app_frame(&mut app, &ctx, input);

        let path = config_dir.join("window_state.txt");
        assert!(path.exists());
        assert!(app.last_persisted_state.is_some());
    }

    #[test]
    fn test_update_impl_fullscreen_skips_size_persist() {
        let mut app = MarkdownViewerApp::new();
        app.last_window_size = None;

        let ctx = egui::Context::default();
        let mut input = default_input();
        let vp = input
            .viewports
            .get_mut(&egui::ViewportId::ROOT)
            .expect("root viewport");
        vp.monitor_size = Some(egui::vec2(1024.0, 768.0));
        vp.outer_rect = Some(egui::Rect::from_min_size(
            egui::pos2(10.0, 10.0),
            egui::vec2(800.0, 600.0),
        ));
        vp.inner_rect = Some(egui::Rect::from_min_size(
            egui::pos2(20.0, 20.0),
            egui::vec2(780.0, 580.0),
        ));
        vp.fullscreen = Some(true);
        vp.maximized = Some(false);

        run_app_frame(&mut app, &ctx, input);

        assert!(app.last_window_size.is_none());
    }

    #[test]
    fn test_update_impl_reload_with_file_succeeds() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        let mut temp_file = tempfile::Builder::new().suffix(".md").tempfile()?;
        temp_file.write_all(b"# Reloaded")?;
        temp_file.flush()?;
        app.current_file = Some(temp_file.path().to_path_buf());
        app.reload_requested = true;

        let ctx = egui::Context::default();
        let input = default_input();
        run_app_frame(&mut app, &ctx, input);

        assert!(app.error_message.is_none());
        Ok(())
    }

    #[test]
    fn test_save_current_document_parentless_path() -> Result<()> {
        let mut app = MarkdownViewerApp::new();
        app.current_content = "Saved".to_string();
        let filename = format!("save_parentless_{}.md", std::process::id());
        let _forced = ForcedDialogPaths::new(None, Some(PathBuf::from(&filename)));

        app.save_current_document()?;
        assert_eq!(app.current_file, Some(PathBuf::from(&filename)));
        std::fs::remove_file(&filename)?;
        Ok(())
    }

    #[test]
    fn test_update_search_results_wraps() {
        let mut app = MarkdownViewerApp::new();
        app.parsed_elements = vec![
            MarkdownElement::Paragraph(vec![InlineSpan::Text("Match".to_string())]),
            MarkdownElement::Paragraph(vec![InlineSpan::Text("Other".to_string())]),
        ];
        app.search_query = "Match".to_string();
        app.last_match_index = Some(1);

        app.update_search_results("");
        assert_eq!(app.last_match_index, Some(0));
    }

    #[test]
    fn test_handle_screenshot_events_save_error() {
        let mut app = MarkdownViewerApp::new();
        let ctx = egui::Context::default();
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let config = ScreenshotConfig {
            output_path: temp_dir.path().to_path_buf(),
            viewport_width: 40.0,
            viewport_height: 30.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 0,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        app.screenshot = Some(ScreenshotState::new(config));

        let image = egui::ColorImage::new([10, 10], Color32::WHITE);
        let mut input = default_input();
        input.events.push(egui::Event::Screenshot {
            viewport_id: egui::ViewportId::ROOT,
            image: std::sync::Arc::new(image),
        });
        let _ = ctx.run(input, |ctx| {
            app.handle_screenshot_events(ctx);
        });

        assert!(app.screenshot.as_ref().is_some_and(|state| state.done));
    }

    #[test]
    fn test_update_screenshot_state_requests_repaint() {
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let config = ScreenshotConfig {
            output_path: temp_dir.path().join("shot.png"),
            viewport_width: 200.0,
            viewport_height: 100.0,
            content_only: false,
            scroll_ratio: Some(0.5),
            wait_ms: 1000,
            settle_frames: 5,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let mut app = MarkdownViewerApp::new();
        app.screenshot = Some(ScreenshotState::new(config));

        let ctx = egui::Context::default();
        app.update_screenshot_state(&ctx, None, None);
    }

    #[test]
    fn test_update_screenshot_state_requested_done_skips_repaint() {
        let temp_dir = tempfile::TempDir::new().expect("temp dir");
        let config = ScreenshotConfig {
            output_path: temp_dir.path().join("shot.png"),
            viewport_width: 200.0,
            viewport_height: 100.0,
            content_only: false,
            scroll_ratio: None,
            wait_ms: 1000,
            settle_frames: 0,
            zoom: 1.0,
            theme: ScreenshotTheme::Light,
            font_source: None,
        };
        let mut state = ScreenshotState::new(config);
        state.requested = true;
        state.done = true;

        let mut app = MarkdownViewerApp::new();
        app.screenshot = Some(state);

        let ctx = egui::Context::default();
        app.update_screenshot_state(&ctx, None, None);

        let state = app.screenshot.as_ref().expect("screenshot state");
        assert!(state.requested);
        assert!(state.done);
    }

    #[test]
    fn test_env_guard_removes_unset_key() {
        let _lock = env_lock();
        std::env::remove_var("MDMDVIEW_TMP_ENV");
        {
            let _guard = EnvGuard::set("MDMDVIEW_TMP_ENV", "value");
        }
        assert!(std::env::var("MDMDVIEW_TMP_ENV").is_err());
    }
}
