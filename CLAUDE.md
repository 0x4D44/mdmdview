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

# Build without embedded Mermaid support
cargo build --release --no-default-features

# Run in development
cargo run

# Run with a specific file
cargo run -- document.md

# Run tests (~1,100+ tests, takes ~8 minutes on Windows)
# See tests.md for the full testing guide
# Test binary auto-lowers to BELOW_NORMAL priority (see lib.rs test_priority)
# Use nice + mdtimeout so compilation also runs at low priority
nice -n 10 mdtimeout 600 cargo test

# Run specific test module (much faster, seconds)
cargo test markdown_renderer
cargo test app

# Run tests with output
cargo test -- --nocapture

# Pre-release validation (mirrors GitHub Actions release pipeline)
.\scripts\prerelease.ps1

# Full test runner (quality + tests + Mermaid visual + coverage → HTML report)
cargo run --release --bin full_test
cargo run --release --bin full_test -- --skip-coverage
cargo run --release --bin full_test -- --quick --skip-mermaid

# Code formatting and linting
cargo fmt
cargo clippy

# Build MSI installer (requires WiX Toolset v3 + cargo-wix)
cargo wix                    # full: build release + create MSI
cargo wix --no-build         # MSI only (if release binary already built)
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
- `Alt+←` / `Alt+→` - Navigate back/forward in history
- `Ctrl++` / `Ctrl+-` / `Ctrl+0` - Zoom in/out/reset
- `Home` / `End` - Scroll to top/bottom
- `Enter` / `Shift+Enter` - Navigate search results forward/backward

## Drag and Drop Support

The application supports dragging files and folders directly from your file explorer:
- **Single File**: Drag a `.md` file to open it immediately
- **Multiple Files**: Drag multiple files to open the first and queue the rest
  - Use `Alt+→` to navigate through queued files
  - Status bar shows queue count: "📋 N files in queue"
- **Folders**: Drag a folder to open all markdown files within (non-recursive)
  - Files are sorted alphabetically
  - Only top-level files are included (subdirectories ignored)
- **Visual Feedback**: Blue overlay appears when dragging over the window
- **Supported Extensions**: `.md`, `.markdown`, `.mdown`, `.mkd`, `.txt`
- **Max Files**: 50 files maximum per drop (protection against accidental large drops)

## Architecture

### Core Components

1. **Main Entry Point** (`src/main.rs`)
   - Application setup and eframe configuration
   - Command-line argument parsing for file opening
   - Window configuration and icon generation
   - egui style configuration with true black background
   - Cross-platform window state restoration

2. **Application Logic** (`src/app.rs`)
   - Main app state management (`MarkdownViewerApp`)
   - UI rendering with egui (menu bar, status bar, central panel)
   - Keyboard shortcuts and navigation handling
   - **Navigation history**: Back/Forward navigation through files and samples (browser-like)
   - View mode management (Rendered vs Raw, with optional Write mode)
   - Search functionality with Unicode normalization
   - Scroll management for navigation and search results
   - File operations and error handling

3. **Markdown Renderer** (`src/markdown_renderer.rs`)
   - Markdown parsing using pulldown-cmark into `MarkdownElement` enum
   - Conversion to egui widgets for display
   - Syntax highlighting with syntect
   - Inline element handling (code, links, formatting)
   - Image loading and texture management with live refresh
   - Mermaid diagram rendering (QuickJS offline, built by default)
   - Table rendering with striped rows
   - Internal anchor navigation for links like `[Section](#section)`
   - Search highlighting with grapheme-aware text matching
   - **Line break preservation**: Single newlines in paragraphs are preserved (good for poetry/lyrics)
   - Emoji support via Twemoji assets

4. **Sample Files** (`src/sample_files.rs`)
   - Embedded markdown examples (`SAMPLE_FILES` constant array)
   - Accessible via File → Samples menu
   - Includes welcome, formatting, code, usage, search, and images samples

5. **Window State** (`src/window_state.rs`)
   - Cross-platform window position/size persistence
   - Windows: Registry storage (`HKEY_CURRENT_USER\Software\MarkdownView`)
   - macOS/Linux: File storage in platform-specific config directories
   - State sanitization to prevent invalid window positions
   - Window geometry validation and sanitization

6. **Emoji System** (`src/emoji_catalog.rs`, `src/emoji_assets.rs`)
   - Embedded Twemoji PNG assets in binary
   - Shortcode expansion (e.g., `:rocket:` → 🚀)
   - Grapheme-aware rendering
   - Texture caching for color emoji rendering

7. **Build Script** (`build.rs`)
   - Windows resource file generation (icon, version info, metadata)
   - Mermaid.js embedding for offline rendering (embedded renderer)
   - Version parsing from Cargo.toml into Windows file properties

8. **Pikchr Renderer** (`src/pikchr_renderer.rs`)
   - Pikchr diagram rendering via embedded C library
   - Synchronous rendering (no worker threads)
   - SVG rasterization via usvg/resvg (shared pipeline with Mermaid)
   - Texture caching with LRU eviction
   - Dark mode support via native `PIKCHR_DARK_MODE` flag

9. **Shell Integration** (`src/shell_integration.rs`)
   - Cross-platform OS file operations for the File menu
   - `clipboard_copy_file`: Places file on clipboard as CF_HDROP (Windows), osascript (macOS), xclip (Linux)
   - `reveal_in_file_manager`: Opens native file manager with file selected

### Key Architecture Decisions

- **Single Binary**: All dependencies, samples, and emoji assets embedded for easy distribution
- **egui Framework**: Cross-platform immediate-mode GUI with native feel
- **pulldown-cmark**: CommonMark compliant markdown parsing
- **syntect**: Syntax highlighting for code blocks
- **Windows Integration**: Custom icon and metadata via build.rs
- **Cross-platform**: Runs on Windows, Linux, and macOS with platform-specific optimizations
- **Optional Features**: Mermaid QuickJS rendering is enabled by default; disable with `--no-default-features`

### Windows Installer (MSI)

- WiX source: `wix/main.wxs` -- **never regenerate** (contains stable `UpgradeCode` GUID for upgrade detection)
- License RTF: `wix/License.rtf` -- auto-generated by `cargo wix init`, used by installer welcome page
- Requires: WiX Toolset v3.14.1 + `cargo-wix` (`cargo install cargo-wix`)
- Output: `target/wix/mdmdview-<version>-x86_64.msi`
- Installs to `C:\Program Files\mdmdview\` with Start Menu shortcut and `.md`/`.markdown` file associations
- Add/Remove Programs entry with icon, version, publisher
- `MajorUpgrade`: new version auto-removes old; user settings in `HKCU\Software\MarkdownView` preserved across uninstall/upgrade

### Navigation System

The app implements keyboard-driven navigation:
- `NavigationRequest` enum handles different scroll types (Top, Bottom, PageUp, PageDown, ScrollUp, ScrollDown)
- Main scroll area uses persistent state via `egui::Id::new("main_scroll_area")`
- Keyboard shortcuts processed in `handle_shortcuts()`
- Search results trigger automatic scrolling to matched elements
- Internal anchor links (`[text](#anchor)`) scroll to headers within the document
- Scroll deltas applied via `ui.scroll_with_delta()`
- Page navigation moves 80% of viewport height
- **Navigation history**: Browser-like back/forward through files and samples (Alt+← / Alt+→)

### Search System

Advanced search with Unicode support:
- **Accent-aware matching**: Uses `unicode-normalization` and `unicode-casefold` crates
- Searches normalized/case-folded versions of text for broader matches (finds "resume" and "Istanbul" from "istanbul")
- Highlights respect grapheme clusters (emoji, accents stay intact)
- Element-level highlighting: each `MarkdownElement` tracks matches
- Real-time highlighting: search phrase highlighted inline as you type
- Navigation: Enter/Shift+Enter or F3/Shift+F3 for next/previous match
- Search state managed in `MarkdownViewerApp` with query tracking

### View Modes

Two primary viewing modes:
- **Rendered Mode** (default): Displays parsed markdown with formatting, syntax highlighting, images
- **Raw Mode** (Ctrl+R): Shows original markdown source in text editor/monospace
  - **Write Mode** (Ctrl+E in Raw view): Enables editing with auto-save on view switch
  - Preserves cursor position when toggling modes
  - Line-based navigation with keyboard shortcuts
  - Live preview update when switching back to rendered mode

### Image Handling

Images are loaded and cached with live refresh:
- Base directory tracking: relative image paths resolved against current file's parent
- Monitors file modification times via `std::time::SystemTime`
- Automatically invalidates cached textures when source files change
- F5 reload picks up modified images without restarting
- Supports PNG, JPEG, GIF, BMP, ICO, WebP formats (via `image` crate)
- SVG rendering via `usvg` and `resvg` / `tiny-skia` crates
- Image paths resolved relative to markdown file location
- Missing images show placeholder text instead of crashing

### Mermaid Diagram Rendering

Embedded QuickJS rendering (offline):
- Built in by default; disable with `--no-default-features`
- Requires `assets/vendor/mermaid.min.js` file
- JavaScript embedded during build via `build.rs`
- Uses `rquickjs` crate for JS execution
- No network dependency
- Theme selectable via View > Mermaid Theme menu (Auto/Dark/Default/Forest/Neutral)
- Auto theme resolves to Dark or Default based on app dark/light mode
- Security level hardcoded to strict

Diagram textures are cached and scale with zoom.

### Font and Rendering

- Configurable font sizes for different markdown elements (`FontSizes` struct)
- Proper inline element rendering with `InlineSpan` enum
- Table rendering with headers and striped rows
- Professional styling with proper spacing and colors
- Dark mode with true black background for maximum contrast
- **Line break preservation**: Single newlines preserved in paragraphs (important for poetry/lyrics)

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

### Window State Persistence

- State saved on exit and every 1 second (throttled)
- Tracks position, size, and maximized state
- Geometry validation: clamps offscreen/invalid values
- Dual storage on Windows: registry + config file fallback
- Cross-platform: `XDG_CONFIG_HOME` on Linux, `~/Library/Application Support` on macOS

## Development Patterns

### Error Handling
- Uses `anyhow::Result` for consistent error propagation
- Error messages displayed in UI rather than panicking
- Graceful fallbacks for missing files or parsing errors
- **Non-UTF-8 files**: Uses `String::from_utf8_lossy` to handle legacy encodings
  - Replacement characters (`U+FFFD`) mark invalid bytes
  - Warning emitted once to stderr
  - Always saves as UTF-8
- Missing images show placeholder text instead of crashing

### State Management
- Centralized app state in `MarkdownViewerApp`
- Parsed content cached as `Vec<MarkdownElement>`
- Window state persistence with throttling (avoids excessive disk writes)
- Deferred state changes to avoid borrowing conflicts during input handling
- Search state tracking with query normalization
- Element rects tracked via `element_rects` HashMap for scroll-to-element
- Header IDs stored for anchor navigation
- Texture cache managed by MarkdownRenderer

### Testing Strategy

See **[tests.md](tests.md)** for the comprehensive testing guide covering all test categories, infrastructure, and CI pipelines.

- ~1,100+ tests across unit, D2 conformance, visual regression, and Mermaid visual categories
- Unit tests for markdown parsing, rendering, app state, window persistence, diagrams, and build metadata
- D2 conformance tests validate four geometric invariants on diagram output
- Visual regression tests (26 cases) pixel-diff mdmdview screenshots against baselines
- Mermaid visual tests (15 cases) compare against official Mermaid CLI output
- Use `tempfile` crate for file system tests; no mocks — thread-local injection instead
- Run specific test modules: `cargo test markdown_renderer`, `cargo test app`
- Run tests after any changes to parsing/rendering logic
- **Timeouts**: Full test suite takes ~8 minutes. Always use `nice -n 10 mdtimeout 600` for full runs. Individual test modules complete in seconds.
- **CPU priority**: Test binaries automatically lower to `BELOW_NORMAL_PRIORITY_CLASS` on Windows via a `.CRT$XCU` initializer in `lib.rs`. This prevents the ~8-minute test suite from starving interactive applications (especially over remote desktop). Use `nice -n 10` for the `cargo test` command itself to also lower compilation priority.

### File Operations

- **Line ending normalization**: All line endings (`\r\n`, `\n`, `\r`) normalized to Unix style (`\n`) at file load time
- Lossy UTF-8 reading: `read_file_lossy()` tries UTF-8, falls back to lossy conversion
- Save always writes UTF-8
- Base directory tracked for relative image paths
- F5 reload re-reads from disk without losing position

## Feature Flags and Environment Variables

### Cargo Features
- `mermaid-embedded`: Enables offline Mermaid rendering via QuickJS (default)
  - Disable with: `cargo build --no-default-features`
  - Requires `assets/vendor/mermaid.min.js` file
  - Adds `rquickjs` dependency
- `mermaid-quickjs`: Alias for `mermaid-embedded` (compatibility)
  - Build with: `cargo build --features mermaid-quickjs`
- `pikchr`: Enables embedded Pikchr diagram rendering (default)
  - Disable with: `cargo build --no-default-features --features mermaid-embedded`
  - Adds `pikchr` crate dependency (C library compiled at build time)

### Environment Variables
- `RUST_LOG`: Control logging level in debug builds (via `env_logger`)

## Platform-Specific Features

### Windows
- Custom application icon (icon.ico) embedded via `build.rs`
- File metadata and version information from `Cargo.toml`
- Windows registry for window state persistence (`HKEY_CURRENT_USER\Software\MarkdownView`)
- Shell association support for .md files
- Build metadata includes timestamp via PowerShell
- Uses `winres` crate for resource compilation
- Native file dialogs using `rfd` crate

### Linux/macOS
- Window state in `~/.config/mdmdview/window_state.txt` (Linux)
- Window state in `~/Library/Application Support/MarkdownView/window_state.txt` (macOS)
- Native file dialogs using `rfd` crate
- Cross-platform config directory resolution (`XDG_CONFIG_HOME`)

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
- Once a debug/test build succeeds then proceed to build the release version as well
- Run both debug and release builds to verify
- Test with large markdown files (1000+ elements)
- Verify texture memory usage with many images
- Check startup time with embedded assets
