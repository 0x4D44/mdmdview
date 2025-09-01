/// A simple markdown viewer built with egui
/// 
/// This library provides markdown parsing and rendering functionality
/// that can be embedded in GUI applications.

pub mod markdown_renderer;
pub mod sample_files;
pub mod app;

pub use app::MarkdownViewerApp;
pub use markdown_renderer::{MarkdownRenderer, MarkdownElement};
pub use sample_files::{SAMPLE_FILES, SampleFile};
