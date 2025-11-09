# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **mdmdview** - a standalone markdown viewer application for Windows built with Rust and egui. It's a single-binary GUI application that renders markdown files with syntax highlighting, embedded samples, and keyboard shortcuts.

## Build Commands

```bash
# Development build
cargo build

# Optimized release build (preferred)
cargo build --release

# Build with offline Mermaid support
cargo build --release --features mermaid-quickjs

# Run in development
cargo run

# Run with a specific file
cargo run -- document.md

# Run with Kroki enabled
MDMDVIEW_ENABLE_KROKI=1 cargo run

# Run tests
cargo test

# Run specific test module
cargo test markdown_renderer
cargo test app

# Run tests with output
cargo test -- --nocapture

# Code formatting and linting
cargo fmt
cargo clippy
```

## Key Keyboard Shortcuts

Understanding these shortcuts is important for testing and development:
- `Ctrl+O` - Open file dialog
- `Ctrl+W` - Close current file
- `Ctrl+F` - Toggle search panel
- `Ctrl+R` - Toggle between Rendered/Raw view
- `Ctrl+E` - Toggle write mode (in Raw view)
- `F5` - Reload current file
- `F11` - Toggle fullscreen
- `Ctrl++` / `Ctrl+-` / `Ctrl+0` - Zoom in/out/reset
- `Home` / `End` - Scroll to top/bottom
- `Enter` / `Shift+Enter` - Navigate search results forward/backward

## Architecture

### Core Components

1. **Main Entry Point** (`src/main.rs`)
   - Application setup and eframe configuration
   - Command-line argument parsing for file opening
   - Window configuration and icon setup
   - Cross-platform window state restoration

2. **Application Logic** (`src/app.rs`)
   - Main app state management (`MarkdownViewerApp`)
   - UI rendering with egui
   - Keyboard shortcuts and navigation handling
   - File operations and error handling
   - Search functionality with Unicode normalization
   - View modes: Rendered and Raw (with optional editing)
   - Scroll management for navigation and search results

3. **Markdown Renderer** (`src/markdown_renderer.rs`)
   - Markdown parsing using pulldown-cmark
   - Conversion to egui widgets for display
   - Syntax highlighting with syntect
   - Inline element handling (code, links, formatting)
   - Image rendering with live texture refresh
   - Mermaid diagram rendering (Kroki/QuickJS)
   - Emoji support via Twemoji assets

4. **Sample Files** (`src/sample_files.rs`)
   - Embedded markdown examples built into the binary
   - Accessible via File → Samples menu

5. **Window State** (`src/window_state.rs`)
   - Cross-platform window position/size persistence
   - Windows: Registry storage under `HKEY_CURRENT_USER\Software\MarkdownView`
   - Linux/macOS: File storage in `~/.config/mdmdview/window_state.txt`
   - State sanitization to prevent invalid window positions

6. **Emoji Support** (`src/emoji_assets.rs`, `src/emoji_catalog.rs`)
   - Embedded Twemoji color images
   - Shortcode expansion (`:rocket:` → 🚀)
   - Grapheme-aware rendering

### Key Architecture Decisions

- **Single Binary**: All dependencies and samples embedded for easy distribution
- **egui Framework**: Cross-platform immediate-mode GUI with native feel
- **pulldown-cmark**: CommonMark compliant markdown parsing
- **syntect**: Syntax highlighting for code blocks
- **Windows Integration**: Custom icon and metadata via build.rs
- **Cross-platform**: Runs on Windows, Linux, and macOS with platform-specific optimizations

### Navigation System

The app implements keyboard-driven navigation:
- `NavigationRequest` enum handles different scroll types (Top, Bottom, PageUp, PageDown, ScrollUp, ScrollDown)
- Main scroll area uses persistent state via `egui::Id`
- Keyboard shortcuts processed in `handle_keyboard_input()`
- Search results trigger automatic scrolling to matched elements
- Internal anchor links (`[text](#anchor)`) scroll to headers within the document

### Search System

Advanced search with Unicode support:
- **Accent-aware matching**: Uses `unicode-normalization` and `unicode-casefold` crates
- Searches normalized/case-folded versions of text for broader matches
- Highlights respect grapheme clusters (emoji, accents stay intact)
- Element-level highlighting: each `MarkdownElement` tracks matches
- Navigation: Enter/Shift+Enter to move through results
- Search state managed in `MarkdownViewerApp` with query tracking

### View Modes

Two primary viewing modes:
- **Rendered Mode** (default): Displays parsed markdown with formatting, syntax highlighting, images
- **Raw Mode** (Ctrl+R): Shows original markdown source in text editor
  - **Write Mode** (Ctrl+E in Raw view): Enables editing with auto-save on view switch
  - Preserves cursor position when toggling modes
  - Line-based navigation with keyboard shortcuts

### Image Handling

Images are loaded and cached with live refresh:
- Monitors file modification times via `std::time::SystemTime`
- Automatically invalidates cached textures when source files change
- Supports PNG, JPEG, GIF, BMP, ICO, WebP formats
- SVG rendering via `usvg` and `resvg` crates
- Image paths resolved relative to markdown file location

### Mermaid Diagram Rendering

Two rendering modes available:
1. **Kroki Service** (default, network-based):
   - Enable with `MDMDVIEW_ENABLE_KROKI=1` environment variable
   - Custom server: `MDMDVIEW_KROKI_URL=https://your-kroki-server`
   - Background rendering with worker pool (max 4 concurrent)
   - Queue status UI when workers busy

2. **QuickJS** (optional, offline):
   - Enable at compile time with `--features mermaid-quickjs`
   - Requires `assets/vendor/mermaid.min.js` file
   - JavaScript embedded during build via `build.rs`
   - Uses `rquickjs` crate for JS execution

Both modes share caching and zoom behavior.

### Font and Rendering

- Configurable font sizes for different markdown elements (`FontSizes` struct)
- Proper inline element rendering with `InlineSpan` enum
- Table rendering with headers and striped rows
- Professional styling with proper spacing and colors
- Dark mode with true black background for maximum contrast

### Data Structures

Key types for markdown representation:

**`MarkdownElement` enum** - Represents parsed markdown blocks:
- `Paragraph(Vec<InlineSpan>)`
- `Header { level, spans, id }` - id used for anchor links
- `CodeBlock { language, code }` - with syntax highlighting
- `List { items, ordered }` - nested list support
- `Table { headers, rows }` - full table rendering
- `HorizontalRule`, `Blockquote`, `Mermaid`, `Image`, etc.

**`InlineSpan` enum** - Represents inline formatting:
- `Text(String)`, `Code(String)`
- `Strong(String)`, `Emphasis(String)`, `Strikethrough(String)`
- `Link { text, url }` - clickable links
- `Image { src, alt, title }` - embedded images

**`ViewMode` enum**:
- `Rendered` - Display formatted markdown
- `Raw` - Show source text (with optional editing)

**`NavigationRequest` enum**:
- `Top`, `Bottom`, `PageUp`, `PageDown`, `ScrollUp`, `ScrollDown`

## Development Patterns

### Error Handling
- Uses `anyhow::Result` for consistent error propagation
- Error messages displayed in UI rather than panicking
- Graceful fallbacks for missing files or parsing errors
- **Non-UTF-8 files**: Uses `String::from_utf8_lossy` to handle legacy encodings
  - Replacement characters (`U+FFFD`) mark invalid bytes
  - Warning emitted once to stderr
  - Always saves as UTF-8

### State Management
- Centralized app state in `MarkdownViewerApp`
- Parsed content cached as `Vec<MarkdownElement>`
- Window state persistence with throttling (avoids excessive disk writes)
- Deferred state changes to avoid borrowing conflicts during input handling
- Search state tracking with query normalization

### Testing Strategy
- Unit tests for markdown parsing and rendering
- Integration tests for file loading
- Search functionality tests (Unicode normalization, accent handling)
- Window state persistence tests
- Build metadata tests in `build.rs`
- Use `tempfile` crate for file system tests
- Run specific test modules: `cargo test markdown_renderer`, `cargo test app`

## Feature Flags and Environment Variables

### Cargo Features
- `mermaid-quickjs`: Enables offline Mermaid rendering via QuickJS
  - Build with: `cargo build --features mermaid-quickjs`
  - Requires `assets/vendor/mermaid.min.js` file
  - Adds `rquickjs` dependency

### Environment Variables
- `MDMDVIEW_ENABLE_KROKI=1`: Enable Kroki service for Mermaid diagram rendering
- `MDMDVIEW_KROKI_URL`: Custom Kroki server URL (default: public Kroki instance)
- `RUST_LOG`: Control logging level in debug builds (via `env_logger`)

## Platform-Specific Features

### Windows
- Custom application icon (icon.ico) embedded via `build.rs`
- File metadata and version information from `Cargo.toml`
- Windows registry for window state persistence (`HKEY_CURRENT_USER\Software\MarkdownView`)
- Shell association support for .md files
- Build metadata includes timestamp via PowerShell
- Uses `winres` crate for resource compilation

### Linux/macOS
- Window state in `~/.config/mdmdview/window_state.txt` (Linux)
- Window state in `~/Library/Application Support/MarkdownView/window_state.txt` (macOS)
- Native file dialogs using `rfd` crate
- Cross-platform config directory resolution

## Code Quality Standards

- **Zero Warnings Policy**: All code must compile cleanly with no compiler warnings
- **Documentation Requirements**: Each `.rs` file must begin with a detailed block comment describing:
  - The file's purpose and functionality
  - Key components or structures it defines
  - How it fits into the overall application architecture
- **Code Comments**: All public functions and complex logic must be well-commented

## Performance Optimizations

Release profile configured for minimal binary size:
- Link-time optimization (LTO)
- Symbol stripping
- Single codegen unit
- Panic abort strategy
- Once a debug/test build succeeds then proceed to build the release version as well.