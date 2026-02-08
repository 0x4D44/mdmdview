#![cfg_attr(coverage_nightly, feature(coverage_attribute))]
//! Standalone markdown viewer for Windows built with Rust and egui.
//!
//! This library provides markdown parsing, rendering, and GUI application logic
//! as a single-binary viewer with syntax highlighting, embedded samples, Mermaid
//! diagram support, and keyboard-driven navigation.

pub mod app;
pub mod emoji_assets;
pub mod emoji_catalog;
pub mod markdown_renderer;
pub mod sample_files;
pub mod table_support;
pub mod window_state;

pub(crate) mod image_decode;
pub(crate) mod mermaid_renderer;

pub use app::{MarkdownViewerApp, APP_TITLE_PREFIX};
pub use markdown_renderer::{MarkdownElement, MarkdownRenderer};
pub use sample_files::{SampleFile, SAMPLE_FILES};
pub use window_state::{
    load_app_settings, load_window_state, sanitize_window_state, save_app_settings,
    save_window_state, AppSettings, WindowState,
};
