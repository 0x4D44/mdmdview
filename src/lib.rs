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
pub mod theme;
pub mod window_state;

pub(crate) mod image_decode;
pub(crate) mod lru_cache;
pub(crate) mod mermaid_renderer;
#[cfg(feature = "pikchr")]
pub(crate) mod pikchr_renderer;
#[cfg(feature = "d2")]
pub(crate) mod d2_renderer;

// Automatically lower the test binary's process priority on Windows.
// The full test suite (~986 tests, ~8 minutes) saturates all CPU cores,
// making remote desktop sessions unresponsive. Setting BELOW_NORMAL priority
// lets tests use idle CPU without starving interactive applications.
// This runs via the C runtime .CRT$XCU initializer, before main().
#[cfg(all(test, windows))]
mod test_priority {
    extern "system" {
        fn GetCurrentProcess() -> isize;
        fn SetPriorityClass(hProcess: isize, dwPriorityClass: u32) -> i32;
        fn GetPriorityClass(hProcess: isize) -> u32;
    }
    const BELOW_NORMAL_PRIORITY_CLASS: u32 = 0x00004000;

    #[used]
    #[link_section = ".CRT$XCU"]
    static SET_BELOW_NORMAL: unsafe extern "C" fn() = {
        unsafe extern "C" fn init() {
            SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS);
        }
        init
    };

    #[test]
    fn test_process_priority_is_below_normal() {
        let priority = unsafe { GetPriorityClass(GetCurrentProcess()) };
        assert_eq!(
            priority, BELOW_NORMAL_PRIORITY_CLASS,
            "Test process should be running at BELOW_NORMAL priority, got 0x{:08X}",
            priority
        );
    }
}

pub use app::{MarkdownViewerApp, APP_TITLE_PREFIX};
pub use markdown_renderer::{MarkdownElement, MarkdownRenderer};
pub use sample_files::{SampleFile, SAMPLE_FILES};
pub use theme::{apply_dark_mode_visuals, ThemeColors};
pub use window_state::{
    load_app_settings, load_window_state, sanitize_window_state, save_app_settings,
    save_window_state, AppSettings, WindowState,
};
