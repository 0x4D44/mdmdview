# mdmdview

<div align="center">

**A fast, standalone markdown viewer built with Rust and egui**

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org)
[![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20Linux%20%7C%20macOS-lightgrey.svg)](https://github.com/0x4D44/mdmdview)

</div>

---

## 📖 Overview

**mdmdview** is a professional markdown viewer designed for speed, simplicity, and reliability. Built entirely in Rust with the egui framework, it delivers a native, responsive experience with zero external dependencies. Perfect for viewing documentation, notes, technical specs, or any markdown content with beautiful formatting and syntax-highlighted code blocks.

### Why mdmdview?

- **🚀 Blazing Fast** - Instant startup and rendering with efficient Rust implementation
- **📦 Single Binary** - No installation, no dependencies, just run the executable
- **🎨 Beautiful Rendering** - Professional formatting with syntax highlighting for 100+ languages
- **⌨️ Keyboard-First** - Comprehensive shortcuts for power users
- **🔍 Smart Search** - Unicode-aware search with accent normalization
- **🖼️ Live Images** - Automatic refresh when linked images change
- **📂 Drag & Drop** - Drop files or folders to open instantly
- **🌐 Cross-Platform** - Works on Windows, Linux, and macOS

---

## ✨ Features

### Core Functionality

- **Pure Rust Implementation** - Single executable (~4.8MB) with all assets embedded
- **Fast Markdown Rendering** - CommonMark compliant parsing with pulldown-cmark
- **Syntax Highlighting** - 100+ languages supported via syntect
- **Native File Dialogs** - Seamless OS integration with rfd
- **Embedded Examples** - Built-in sample files demonstrating all features
- **Command Line Support** - Open files directly: `mdmdview document.md`
- **Custom Application Icon** - Distinctive markdown document icon with full Windows metadata

### Advanced Features

- **🔄 Navigation History** - Browser-like back/forward through files and samples (`Alt+←`/`Alt+→`)
- **🔍 Accent-Aware Search** - Finds matches across case and diacritics (e.g., "istanbul" matches "İstanbul")
- **🖼️ Live Image Refresh** - Automatically reloads textures when linked images change on disk
- **📝 Raw Mode with Edit** - View and edit source markdown (`Ctrl+R` to toggle, `Ctrl+E` to edit)
- **🔗 Internal Anchors** - In-document navigation via `[link](#anchor)` syntax
- **📊 Table Support** - Professional grid layout with headers and striped rows
- **🎨 Mermaid Diagrams** - Render flowcharts, sequence diagrams, and more
- **😀 Emoji Support** - Embedded Twemoji assets with shortcode expansion (`:rocket:` → 🚀)
- **🌐 Encoding Fallback** - Opens non-UTF-8 legacy files via lossy decoding
- **💾 Window State Persistence** - Remembers position, size, and zoom level across sessions
- **🔆 Zoom Support** - Adjustable font sizes (`Ctrl++`/`Ctrl+-`/`Ctrl+0`)
- **📋 Multi-File Queue** - Drag multiple files to queue them for sequential viewing

### Drag and Drop Support

Drop files and folders directly from your file explorer:

- **Single File** - Drop a `.md` file to open it immediately
- **Multiple Files** - Drop multiple files to open the first and queue the rest
  - Navigate through queue with `Alt+→` / `Alt+←`
  - Status bar shows: "📋 N files in queue"
- **Folders** - Drop a folder to open all markdown files within (non-recursive, top-level only)
- **Visual Feedback** - Blue overlay appears during drag operations
- **Supported Extensions** - `.md`, `.markdown`, `.mdown`, `.mkd`, `.txt`
- **Protection** - 50 file maximum per drop (prevents accidental large drops)

---

## 🚀 Getting Started

### Prerequisites

- **Rust 1.70+** - Install from [rustup.rs](https://rustup.rs/)
- **Operating System** - Windows 10/11, Linux (Ubuntu 20.04+, Fedora 35+), or macOS 10.15+

### Installation

#### Option 1: Download Pre-built Binary (Recommended)

1. Go to [Releases](https://github.com/0x4D44/mdmdview/releases)
2. Download the latest release for your platform:
   - `mdmdview-vX.Y.Z-windows-x86_64.zip` (Windows)
   - `mdmdview-vX.Y.Z-linux-x86_64.tar.gz` (Linux)
3. Extract and run the executable

#### Option 2: Build from Source

```bash
# Clone the repository
git clone https://github.com/0x4D44/mdmdview.git
cd mdmdview

# Build in release mode (optimized, ~4.8MB binary)
cargo build --release

# Run the application
./target/release/mdmdview
```

#### Mermaid Rendering (Default)

Embedded Mermaid rendering (QuickJS, offline) is enabled by default.
On Windows, QuickJS builds require a `patch` tool in `PATH` (e.g., Git for Windows or MSYS2).

```bash
# Build without embedded Mermaid rendering
cargo build --release --no-default-features
```

---

## 📘 Usage

### Opening Files

**Interactive:**
- Use `File > Open` or press `Ctrl+O`
- Drag and drop `.md` files into the window
- Explore `File > Samples` for built-in examples

**Command Line:**
```bash
# Open with welcome screen
mdmdview

# Open a specific file
mdmdview document.md
mdmdview "C:\path\to\my document.md"

# Works with relative paths
mdmdview ../README.md
```

### Screenshot Mode (PNG)

Render a markdown file directly to an image:

```bash
mdmdview --screenshot document.md --output out.png
```

Optional flags:

- `--width <px>` / `--height <px>`: viewport size (default 1280x720 in screenshot mode)
- `--theme light|dark`: override theme for render
- `--zoom <scale>`: scale factor (e.g., `1.25`)
- `--content-only`: hide window chrome in the capture
- `--scroll <ratio>`: scroll ratio from 0.0 (top) to 1.0 (bottom)
- `--wait-ms <ms>`: delay before capture to allow assets to load
- `--settle-frames <n>`: extra frames to wait for layout stability
- `--test-fonts <dir>`: load fonts from a directory (QA/automation use)

### Windows Shell Integration

Associate mdmdview with `.md` files for double-click opening:

1. Right-click any `.md` file in Windows Explorer
2. Select **"Open with > Choose another app"**
3. Click **"More apps"** → **"Look for another app on this PC"**
4. Browse to `mdmdview.exe` and select it
5. Check **"Always use this app to open .md files"**
6. Click **OK**

Now all `.md` files will open in mdmdview by double-clicking!

### View Modes

- **Rendered Mode** (default) - Formatted markdown with styling and images
- **Raw Mode** (`Ctrl+R`) - Source text in monospace editor
  - **Write Mode** (`Ctrl+E` in Raw mode) - Edit and auto-save changes

### Search Functionality

Press `Ctrl+F` to open the search panel:

- **Unicode-aware matching** - Finds "resume" and "résumé" from single query
- **Case-insensitive** - "istanbul" matches "Istanbul" and "İstanbul"
- **Grapheme-aware highlighting** - Preserves emoji and accent marks
- **Navigation** - Use `Enter`/`Shift+Enter` or `F3`/`Shift+F3` to cycle results
- **Real-time highlighting** - Matches highlighted as you type

---

## ⌨️ Keyboard Shortcuts

### File Operations
| Shortcut | Action |
|----------|--------|
| `Ctrl+O` | Open file dialog |
| `Ctrl+W` | Close current file |
| `F5` | Reload current file from disk |
| `Ctrl+Q` | Quit application |

### Navigation
| Shortcut | Action |
|----------|--------|
| `Alt+←` | Navigate back in history |
| `Alt+→` | Navigate forward in history |
| `Home` | Jump to top of document |
| `End` | Jump to bottom of document |
| `Page Up` | Scroll up one page (~80% viewport) |
| `Page Down` | Scroll down one page (~80% viewport) |
| `↑` / `↓` | Scroll up/down (small increment) |

### View Controls
| Shortcut | Action |
|----------|--------|
| `Ctrl+F` | Toggle search panel |
| `Ctrl+R` | Toggle Rendered/Raw view |
| `Ctrl+E` | Toggle edit mode (in Raw view) |
| `F11` | Toggle fullscreen |
| `Ctrl++` | Zoom in (increase font size) |
| `Ctrl+-` | Zoom out (decrease font size) |
| `Ctrl+0` | Reset zoom to default |

### Search Navigation
| Shortcut | Action |
|----------|--------|
| `Enter` | Next search result |
| `Shift+Enter` | Previous search result |
| `F3` | Next search result |
| `Shift+F3` | Previous search result |
| `Esc` | Close search panel |

---

## 🎨 Markdown Support

mdmdview supports all CommonMark elements with professional formatting:

### Text Formatting
- **Bold** (`**text**` or `__text__`)
- *Italic* (`*text*` or `_text_`)
- ~~Strikethrough~~ (`~~text~~`)
- `Inline code` with proper inline rendering
- [Links](https://example.com) that open in browser
- Internal anchors: `[Section](#section)` for in-document navigation

### Structure
- **Headers** (H1-H6) with proper hierarchy and sizing
- **Lists** - Bulleted and numbered with nesting support
- **Tables** - Professional grid with headers and striped rows
- **Blockquotes** - Styled with orange accent bar
- **Horizontal rules** (`---`) for section separation

### Code Blocks
- Syntax highlighting for 100+ languages
- Rust, Python, JavaScript, C++, Go, TypeScript, SQL, and more
- Proper indentation and color schemes
- Line-based rendering for readability

### Rich Content
- **Images** - PNG, JPEG, GIF, BMP, ICO, WebP, SVG
  - Relative paths resolved from markdown file location
  - Live refresh when source files change
  - Graceful fallback for missing images
- **Emoji** - Color Twemoji assets embedded
  - Shortcode expansion: `:rocket:`, `:tada:`, `:heart:`
  - Native Unicode emoji rendering
- **Mermaid Diagrams** - Flowcharts, sequence diagrams, gantt charts, etc.
  - Embedded renderer is built by default (disable with `--no-default-features`)
  - Select renderer via `MDMDVIEW_MERMAID_RENDERER` (`embedded`, `off`)

### Special Features
- **Line break preservation** - Single newlines respected (perfect for poetry/lyrics)
- **Header IDs** - Auto-generated for anchor navigation
- **Nested structures** - Lists within blockquotes, tables with formatting, etc.

---

## 🔧 Configuration

### Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `MDMDVIEW_MERMAID_RENDERER` | Mermaid renderer (`embedded`, `off`) | `embedded` (or `off` with `--no-default-features`) |
| `MDMDVIEW_MERMAID_SECURITY` | Embedded Mermaid security level (`strict`, `loose`) | `strict` |
| `MDMDVIEW_MERMAID_WORKERS` | QuickJS worker count (1-16) | auto |
| `MDMDVIEW_MERMAID_TIMEOUT_MS` | QuickJS render timeout (ms) | `2000` |
| `MDMDVIEW_MERMAID_BG` | Mermaid background (`theme`, `light`, `dark`, `transparent`) | `theme` |
| `MDMDVIEW_MERMAID_BG_COLOR` | Override Mermaid background color (hex) | empty |
| `MDMDVIEW_MERMAID_THEME` | Mermaid theme name | `base` |
| `RUST_LOG` | Logging level (`error`, `warn`, `info`, `debug`, `trace`) | `warn` |

**Example:**
```bash
# Use embedded Mermaid renderer (default)
MDMDVIEW_MERMAID_RENDERER=embedded mdmdview document.md

# Enable debug logging
RUST_LOG=debug mdmdview document.md
```

### Window State Persistence

Window position, size, and maximized state are automatically saved:

- **Windows**: `%APPDATA%\MarkdownView\window_state.txt` + Registry backup
- **Linux**: `~/.config/mdmdview/window_state.txt` (respects `XDG_CONFIG_HOME`)
- **macOS**: `~/Library/Application Support/MarkdownView/window_state.txt`

State is saved every second (throttled) and on exit.

---

## 🏗️ Architecture

### Project Structure

```
mdmdview/
├── src/
│   ├── main.rs                   # Entry point, eframe setup, CLI args
│   ├── app.rs                    # Main app state, UI logic, shortcuts
│   ├── markdown_renderer.rs      # Parsing, rendering, syntax highlighting
│   ├── sample_files.rs           # Embedded markdown examples
│   ├── window_state.rs           # Cross-platform state persistence
│   ├── emoji_catalog.rs          # Emoji shortcode mappings
│   ├── emoji_assets.rs           # Embedded Twemoji PNGs
│   └── lib.rs                    # Library interface for testing
├── build.rs                      # Windows resources, version metadata
├── assets/
│   ├── icon.ico                  # Application icon
│   └── vendor/
│       └── mermaid.min.js        # For embedded Mermaid renderer
├── Cargo.toml                    # Dependencies and build config
└── README.md                     # This file
```

### Core Components

1. **Main Entry Point** (`src/main.rs`)
   - eframe/egui application setup
   - Command-line argument parsing
   - Window configuration with icon
   - Cross-platform initialization

2. **Application Logic** (`src/app.rs`)
   - `MarkdownViewerApp` struct manages all state
   - UI rendering (menu bar, status bar, content area)
   - Keyboard shortcut handling
   - Navigation history (back/forward stack)
   - View mode switching (Rendered/Raw/Write)
   - Search state and result navigation
   - File operations and error handling

3. **Markdown Renderer** (`src/markdown_renderer.rs`)
   - Parsing with pulldown-cmark into `MarkdownElement` enum
   - Conversion to egui widgets
   - Syntax highlighting with syntect
   - Image loading and texture caching
   - Mermaid diagram rendering (embedded QuickJS)
   - Table layout with proper sizing
   - Search result highlighting
   - Emoji rendering with Twemoji assets

4. **Window State** (`src/window_state.rs`)
   - Platform-specific persistence
   - Geometry validation and sanitization
   - Registry storage on Windows
   - Config file storage on Linux/macOS

5. **Build Script** (`build.rs`)
   - Windows resource file generation
   - Version info from Cargo.toml
   - Icon embedding
   - Mermaid.js embedding for embedded renderer

### Key Design Decisions

- **Single Binary** - All dependencies embedded for zero-config distribution
- **Immediate Mode GUI** - egui provides responsive, native-feeling interface
- **CommonMark Compliance** - pulldown-cmark ensures standard compatibility
- **Efficient Rendering** - Texture caching, incremental updates, scroll optimization
- **Cross-Platform** - Platform-specific code isolated, core logic portable
- **Zero Warnings** - Strict compilation standards for code quality

---

## 🛠️ Development

### Building

```bash
# Development build (faster compilation, larger binary)
cargo build

# Release build (optimized, ~4.8MB)
cargo build --release

# Without embedded Mermaid renderer
cargo build --release --no-default-features

# Run in development mode
cargo run

# Run with specific file
cargo run -- document.md

```

### Testing

```bash
# Run all unit tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test module
cargo test markdown_renderer
cargo test app
cargo test window_state

# Run tests for search functionality
cargo test search
```

### Visual Regression (Optional)

```bash
python -m pip install -r tools/regression/requirements.txt
python -m playwright install chromium
cargo build --release
python tools/regression/runner.py update-reference
python tools/regression/runner.py update-baseline
python tools/regression/runner.py run
```

### Code Quality

```bash
# Format code (required before commits)
cargo fmt --all

# Lint with Clippy (zero warnings policy)
cargo clippy --all-targets -- -D warnings

# Check compilation without building
cargo check

# Run all quality checks
cargo fmt --all && cargo clippy --all-targets -- -D warnings && cargo test
```

### Release Process

**Tagged Releases:**

```bash
# Create and push a version tag
git tag v1.0.0
git push origin v1.0.0
```

This triggers the `Release` workflow which:
1. Builds on Windows and Linux runners
2. Creates release archives:
   - `mdmdview-vX.Y.Z-windows-x86_64.zip`
   - `mdmdview-vX.Y.Z-linux-x86_64.tar.gz`
3. Attaches artifacts to GitHub release

**Manual Release:**
- Go to Actions → Release → Run workflow
- Choose toolchain (`stable`, `beta`, or `nightly`)

### File Metadata (Windows)

Version information is auto-generated from `Cargo.toml`:
- **ProductName/InternalName**: `package.name`
- **FileDescription**: `package.description`
- **FileVersion**: `package.version` (e.g., `0.3.3.0`)
- **CompanyName**: First entry in `package.authors`
- **BuildDateTime**: ISO-8601 timestamp at build time

Update `Cargo.toml` before release, then rebuild to refresh metadata.

---

## 🤝 Contributing

Contributions are welcome! Please follow these guidelines:

### Code Standards

1. **Zero Warnings Policy** - Code must compile cleanly with no warnings
2. **Documentation** - Each `.rs` file needs a descriptive block comment explaining:
   - File's purpose and functionality
   - Key components/structures defined
   - How it fits in the overall architecture
3. **Comments** - Public functions and complex logic must be well-commented
4. **Test Coverage** - New features should include unit tests
5. **Formatting** - Run `cargo fmt` before committing
6. **Linting** - Run `cargo clippy` and address all warnings

### Contribution Workflow

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Make your changes following code standards
4. Run quality checks: `cargo fmt && cargo clippy && cargo test`
5. Test both debug and release builds
6. Commit with clear, descriptive messages
7. Push to your fork and submit a pull request

### Testing Checklist

- [ ] All existing tests pass
- [ ] New tests added for new features
- [ ] Tested with large markdown files (1000+ elements)
- [ ] Tested with images and embedded content
- [ ] Verified keyboard shortcuts work correctly
- [ ] Checked memory usage with many images
- [ ] Tested on target platforms (Windows/Linux/macOS)

---

## 📝 Supported File Types

mdmdview recognizes these file extensions:

- `.md` - Standard markdown
- `.markdown` - Alternative markdown extension
- `.mdown` - Common variation
- `.mkd` - Short markdown extension
- `.txt` - Plain text files

All files are processed as UTF-8 with lossy fallback for legacy encodings.

---

## 🐛 Troubleshooting

### Images Not Displaying

- **Check file path** - Relative paths resolved from markdown file location
- **Verify format** - Supported: PNG, JPEG, GIF, BMP, ICO, WebP, SVG
- **Press F5** - Manually reload if live refresh doesn't trigger
- **Check console** - Error messages printed to stderr

### Non-UTF-8 Files Show Strange Characters

mdmdview uses `String::from_utf8_lossy` for legacy encodings:
- Replacement characters (`U+FFFD`) mark invalid bytes
- Save file as UTF-8 in text editor to fix permanently
- Warning emitted once to console/stderr

### Mermaid Diagrams Not Rendering

**Embedded mode:**
- Ensure you did not build with `--no-default-features`
- Optionally set `MDMDVIEW_MERMAID_RENDERER=embedded`
- Ensure `assets/vendor/mermaid.min.js` exists before building
- Check that diagram syntax is valid

### Window Position Lost

State file may be corrupted:
- **Windows**: Delete `%APPDATA%\MarkdownView\window_state.txt`
- **Linux**: Delete `~/.config/mdmdview/window_state.txt`
- **macOS**: Delete `~/Library/Application Support/MarkdownView/window_state.txt`

App will recreate with default centered window.

### Performance Issues

- **Large files** - Files with 1000+ elements may render slowly
- **Many images** - Texture memory usage grows with image count
- **Syntax highlighting** - Code blocks in very large files may lag

Try breaking large documents into smaller files.

---

## 🔒 Security Considerations

### Safe Markdown Rendering

- **No script execution** - JavaScript in markdown is not executed
- **Safe link handling** - External links open in browser, not in-app
- **Image loading** - Only from local filesystem or explicit URLs
- **No remote code** - All rendering done locally

### Mermaid Rendering

**QuickJS mode** (offline):
- JavaScript sandboxed in QuickJS runtime
- No network access during rendering
- Default security level is strict; set `MDMDVIEW_MERMAID_SECURITY=loose` to relax
- Strict mode still allows inline Mermaid label tags: `<i>`, `<b>`, `<em>`, `<strong>`, `<br>`
- Recommended for sensitive/confidential content

---

## 📜 License

This project is provided as-is for demonstration purposes.

### Third-Party Assets

**Emoji Graphics:**
© 2020 Twitter, Inc and other contributors
Licensed under CC-BY 4.0 (Twemoji)
See https://github.com/twitter/twemoji for details
Images embedded in executable for offline use

**Mermaid:**
Copyright (c) 2014-2024 Mermaid Team
Licensed under MIT
See https://github.com/mermaid-js/mermaid for details
JavaScript bundled in `assets/vendor/mermaid.min.js`

### Dependencies

All Rust crates used are listed in `Cargo.toml` with their respective licenses (mostly MIT/Apache-2.0).

---

## 🙏 Acknowledgments

- **egui** - Excellent immediate-mode GUI framework
- **pulldown-cmark** - Fast, CommonMark-compliant parser
- **syntect** - Comprehensive syntax highlighting
- **Twemoji** - Beautiful open-source emoji set
- **Rust Community** - Amazing ecosystem and tooling

---

## 📬 Contact & Support

- **Issues**: [GitHub Issues](https://github.com/0x4D44/mdmdview/issues)
- **Discussions**: [GitHub Discussions](https://github.com/0x4D44/mdmdview/discussions)
- **Pull Requests**: Contributions welcome!

---

<div align="center">

**Made with ❤️ and Rust**

[⬆ Back to Top](#mdmdview)

</div>
