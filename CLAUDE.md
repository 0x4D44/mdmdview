# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **mdmdview** - a standalone markdown viewer application for Windows built with Rust and egui. It's a single-binary GUI application that renders markdown files with syntax highlighting, embedded samples, and keyboard shortcuts.

## Build Commands

```bash
# Development build
cargo build

# Optimized release build (preferred for distribution)
cargo build --release

# Run in development
cargo run

# Run with a specific file
cargo run -- document.md

# Build with Mermaid offline rendering (requires mermaid.min.js in assets/vendor/)
cargo build --release --features mermaid-quickjs

# Run all tests
cargo test

# Run specific test module
cargo test markdown_renderer

# Run tests with output
cargo test -- --nocapture

# Code formatting and linting
cargo fmt
cargo clippy
```

## Architecture

### Core Components

1. **Main Entry Point** (`src/main.rs`)
   - Application setup and eframe configuration
   - Command-line argument parsing for file opening
   - Window configuration and icon generation
   - egui style configuration with true black background

2. **Application Logic** (`src/app.rs`)
   - Main app state management (`MarkdownViewerApp`)
   - UI rendering with egui (menu bar, status bar, central panel)
   - Keyboard shortcuts and navigation handling
   - **Navigation history**: Back/Forward navigation through files and samples (browser-like)
   - View mode management (Rendered vs Raw, with optional Write mode)
   - Search functionality with Unicode normalization
   - File operations and error handling

3. **Markdown Renderer** (`src/markdown_renderer.rs`)
   - Markdown parsing using pulldown-cmark into `MarkdownElement` enum
   - Conversion to egui widgets for display
   - Syntax highlighting with syntect
   - Image loading and texture management with live refresh
   - Mermaid diagram rendering (Kroki service or QuickJS offline)
   - Table rendering with striped rows
   - Internal anchor navigation for links like `[Section](#section)`
   - Search highlighting with grapheme-aware text matching
   - **Line break preservation**: Single newlines in paragraphs are preserved (good for poetry/lyrics)

4. **Sample Files** (`src/sample_files.rs`)
   - Embedded markdown examples (`SAMPLE_FILES` constant array)
   - Accessible via File → Samples menu
   - Includes welcome, formatting, code, usage, search, and images samples

5. **Window State** (`src/window_state.rs`)
   - Cross-platform window position/size persistence
   - Windows: Registry storage (`HKEY_CURRENT_USER\Software\MarkdownView`)
   - macOS/Linux: File storage in platform-specific config directories
   - Window geometry validation and sanitization

6. **Emoji System** (`src/emoji_catalog.rs`, `src/emoji_assets.rs`)
   - Embedded Twemoji PNG assets in binary
   - Shortcode expansion (e.g., `:rocket:` → 🚀)
   - Texture caching for color emoji rendering

7. **Build Script** (`build.rs`)
   - Windows resource file generation (icon, version info, metadata)
   - Mermaid.js embedding for offline rendering (if `mermaid-quickjs` feature enabled)
   - Version parsing from Cargo.toml into Windows file properties

### Key Architecture Decisions

- **Single Binary**: All dependencies, samples, and emoji assets embedded for easy distribution
- **egui Framework**: Cross-platform immediate-mode GUI with native feel
- **pulldown-cmark**: CommonMark compliant markdown parsing
- **syntect**: Syntax highlighting for code blocks
- **Windows Integration**: Custom icon and metadata via build.rs
- **Optional Features**: Mermaid QuickJS rendering can be compiled in with `--features mermaid-quickjs`

### Mermaid Diagram Rendering

Two rendering modes exist:

1. **Kroki Service (Default)**
   - Network-based rendering via Kroki API
   - Enable with `MDMDVIEW_ENABLE_KROKI=1` environment variable
   - Custom instance: `MDMDVIEW_KROKI_URL=https://your-instance.com`
   - Background rendering with 4 concurrent workers
   - Queue status panel when pool is busy

2. **QuickJS Offline**
   - Build with `--features mermaid-quickjs`
   - Requires `mermaid.min.js` in `assets/vendor/mermaid.min.js`
   - Embedded JavaScript execution via rquickjs
   - No network dependency

Both modes share caching and zoom behavior. Diagram textures are stored with refresh timestamps.

### Image Loading and Texture Management

- Base directory tracking: relative image paths resolved against current file's parent
- Live refresh: textures invalidate when source file timestamp changes
- F5 reload picks up modified images without restarting
- Format support: PNG, JPEG, GIF, BMP, ICO, WEBP (via `image` crate)
- SVG support via usvg/resvg/tiny-skia rasterization

### Search System

- Unicode-aware: case folding and NFKC normalization (finds "resume" and "Istanbul" from "istanbul")
- Grapheme cluster respect: emoji and accent marks highlighted correctly
- Real-time highlighting: search phrase highlighted inline as you type
- Navigation: Enter/Shift+Enter or F3/Shift+F3 for next/previous match
- Search state: tracks current match index, scrolls to element on match

### View Modes

- **Rendered Mode**: Default markdown display with rich formatting
- **Raw Mode** (Ctrl+R): Shows raw markdown source in monospace
- **Write Mode** (Ctrl+E): Enables editing in Raw mode with live preview update
- Cursor position preserved when toggling between modes

### Navigation System

- `NavigationRequest` enum handles scroll types (Top, Bottom, PageUp/Down, Arrow scroll)
- Main scroll area uses persistent state via `egui::Id::new("main_scroll_area")`
- Keyboard shortcuts processed in `handle_shortcuts()`
- Scroll deltas applied via `ui.scroll_with_delta()`
- Page navigation moves 80% of viewport height

### Window State Persistence

- State saved on exit and every 1 second (throttled)
- Tracks position, size, and maximized state
- Geometry validation: clamps offscreen/invalid values
- Dual storage on Windows: registry + config file fallback
- Cross-platform: XDG_CONFIG_HOME on Linux, ~/Library/Application Support on macOS

## Development Patterns

### Error Handling
- Uses `anyhow::Result` for consistent error propagation
- Error messages displayed in UI rather than panicking
- Graceful fallbacks: non-UTF-8 files loaded via `String::from_utf8_lossy`
- Missing images show placeholder text instead of crashing

### State Management
- Centralized app state in `MarkdownViewerApp`
- Parsed content cached as `Vec<MarkdownElement>`
- Element rects tracked via `element_rects` HashMap for scroll-to-element
- Header IDs stored for anchor navigation
- Texture cache managed by MarkdownRenderer

### Testing Strategy
- Unit tests for markdown parsing and rendering
- Integration tests for file loading (use `tempfile` crate)
- Build script tests for version parsing and metadata extraction
- Run tests after any changes to parsing/rendering logic

### File Operations

- **Line ending normalization**: All line endings (`\r\n`, `\n`, `\r`) normalized to Unix style (`\n`) at file load time
- Lossy UTF-8 reading: `read_file_lossy()` tries UTF-8, falls back to lossy conversion
- Save always writes UTF-8
- Base directory tracked for relative image paths
- F5 reload re-reads from disk without losing position

## Windows-Specific Features

- Custom application icon (icon.ico)
- File metadata and version information from Cargo.toml
- Registry-based window state persistence
- Shell association support for .md files
- Native file dialogs using rfd crate
- Build timestamp captured at compile time

## Code Quality Standards

- **Zero Warnings Policy**: All code must compile cleanly with no compiler warnings
- **Documentation Requirements**: Each `.rs` file must begin with a detailed block comment describing:
  - The file's purpose and functionality
  - Key components or structures it defines
  - How it fits into the overall application architecture
- **Code Comments**: All public functions and complex logic must be well-commented
- **Test Coverage**: New features should include unit tests

## Performance Optimizations

Release profile configured for minimal binary size:
- Link-time optimization (LTO)
- Symbol stripping
- Single codegen unit
- Panic abort strategy

When making changes:
- Run both debug and release builds to verify
- Test with large markdown files (1000+ elements)
- Verify texture memory usage with many images
- Check startup time with embedded assets