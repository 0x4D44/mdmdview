# MarkdownView

A simple, standalone markdown viewer for Windows built with Rust and egui.

## Features

- **Pure Rust Implementation** - Single executable, no dependencies
- **Fast Rendering** - Efficient markdown parsing with pulldown-cmark
- **Syntax Highlighting** - Code blocks with proper syntax coloring
- **Native File Dialogs** - Seamless Windows integration
- **Embedded Examples** - Built-in sample files to explore
- **Zoom Support** - Adjustable font sizes for better readability
- **Keyboard Shortcuts** - Quick access to common functions
- **Command Line Support** - Open files directly: `mdmdview.exe file.md`
- **Professional Menus** - Right-aligned keyboard shortcuts
- **Custom Icon** - Distinctive markdown document icon
- **Windows Integration** - Full metadata, icon, and shell association support
- **Perfect Formatting** - Proper inline code, tables, lists, and all markdown elements
- **Encoding Fallback** - Opens non-UTF-8 files via lossy decoding so legacy content still renders.
- **Accent-Aware Search** - Finds matches across case and diacritics with precise inline highlighting.
- **Live Image Refresh** - Automatically reloads textures when linked image files change on disk.

## Requirements

- Rust 1.70 or later
- Windows 10/11 (other platforms may work but are not tested)

## Building

### Install Rust

If you don't have Rust installed, download and install it from [rustup.rs](https://rustup.rs/):

```powershell
# Download and run the Rust installer
# Follow the on-screen instructions
```

### Build the Application

```bash
# Clone or navigate to the project directory
cd mdmdview

# Build in release mode for optimal performance
cargo build --release

# The executable will be created at:
# target/release/mdmdview.exe
```

### Running

```bash
# Run directly with cargo
cargo run

# Or run the built executable
./target/release/mdmdview.exe
```

## Usage

### Opening Files

- Use `File > Open` or `Ctrl+O` to load a markdown file
- Drag and drop `.md` files directly into the application
- Use `File > Samples` to explore built-in examples
- **Command line**: `mdmdview.exe filename.md` to open a specific file

### Windows Shell Integration

You can associate MarkdownView with `.md` files for easy opening:

1. Right-click any `.md` file in Windows Explorer
2. Select "Open with > Choose another app"
3. Browse to `mdmdview.exe`
4. Check "Always use this app to open .md files"

Now double-clicking any `.md` file will open it in MarkdownView!

### Window State Persistence

- The app saves window position and size on exit and restores them on startup.
- On Windows, the state file is stored under `%APPDATA%\MarkdownView\window_state.txt`.
- On Linux/macOS, it is stored under the user config directory (e.g., `~/.config/mdmdview/window_state.txt` or `~/Library/Application Support/MarkdownView/window_state.txt`).

### Searching

- Press `Ctrl+F` to open the search panel.
- Queries use Unicode normalization and case folding, so the same search hits `resume`, `RESUME`, and versions that include accents (for example, typing `istanbul` also finds `Istanbul`).
- Highlights respect grapheme clusters, keeping emoji and accent marks intact.
- Use `Enter`/`Shift+Enter` to move forward or backward through results.

### Handling Non-UTF-8 Files

MarkdownView falls back to `String::from_utf8_lossy` when a document contains invalid UTF-8 bytes (common for Windows-1252 or ISO-8859 encodings). Replacement characters (`U+FFFD`) mark any bytes that could not be converted. A warning is emitted once to the console/stderr, and saving always writes UTF-8 output.

### Refreshing Linked Images

Linked image textures invalidate automatically when the source file timestamp changes. After editing an image externally, return to MarkdownView (or press `F5`) and the viewer will pick up the new pixels without restarting the app.

### File Properties and Icon

Windows file properties are generated from the crate metadata during each build:
- **Product/Internal Name** comes from `package.name` (currently `mdmdview`).
- **Description** mirrors `package.description`.
- **Version** is encoded from `package.version` (for example `0.3.1` renders as `0.3.1.0` in Explorer).
- **Company** resolves to the first entry in `package.authors`.
- **BuildDateTime** records an ISO-8601 timestamp captured at build time.

Update `Cargo.toml` before publishing and rebuild to refresh these values. The bundled `icon.ico` remains embedded for Explorer integration.

### Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+O` | Open file |
| `Ctrl+W` | Close current file |
| `F5` | Reload current file |
| `Ctrl+Q` | Quit application |
| `F11` | Toggle fullscreen |
| `Ctrl++` | Zoom in (increase font size) |
| `Ctrl+-` | Zoom out (decrease font size) |
| `Ctrl+0` | Reset zoom to default size |
| `Home` | Go to top of document |
| `End` | Go to bottom of document |
| `Page Up` | Scroll up one page |
| `Page Down` | Scroll down one page |

### Markdown Rendering

All standard markdown elements are fully supported with professional formatting:

- **Headers** (H1-H6) with proper sizing and hierarchy
- **Text formatting** - Bold, italic, strikethrough  
- **Lists** - Both bulleted and numbered with proper spacing
- **Tables** - Professional grid layout with headers and striped rows
- **Code blocks** - Syntax highlighting for multiple languages
- **Mermaid diagrams** - Optional QuickJS (offline) or Kroki (network) rendering. See **Mermaid Rendering Modes** for configuration details.
- **Inline code** - Properly formatted within text (e.g., use `pip install` command)
- **Links** - Clickable URLs that open in browser
  - Internal anchors supported: `[Section](#section)` scrolls in-document
- **Horizontal rules** - Clean section separators
- **Blockquotes** - Polished dark block with orange bar; supports multi-line and nested quotes
- **Emojis** - Color emoji images embedded (Twemoji); common shortcodes (e.g., `:rocket:`, `:tada:`) expand to Unicode automatically

### Mermaid Rendering Modes

- **Default (Kroki service)**  
  Rendering runs in the background with at most four concurrent workers. Enable network rendering by setting the `MDMDVIEW_ENABLE_KROKI=1` environment variable (use `MDMDVIEW_KROKI_URL` to target a self-hosted instance). When the pool is busy the viewer shows a queue status panel and renders diagrams automatically once a slot frees up.
- **Offline (QuickJS)**  
  Build or run with `--features mermaid-quickjs` to render diagrams locally without network access. Place `mermaid.min.js` in `assets/vendor/mermaid.min.js`; the build script embeds it during compilation. Offline mode shares the same caching and zoom behaviour as the network renderer.

**Key Improvement**: Inline code elements like `filename.ext` now stay properly inline with surrounding text instead of breaking to new lines.

### Supported File Types

- `.md` - Standard markdown files
- `.markdown` - Alternative markdown extension
- `.mdown` - Another common variation
- `.mkd` - Short markdown extension

## Development

### Tagged Releases

- Push a semver tag to trigger a release build and publish assets:
  - Create tag: `git tag v1.0.0 && git push origin v1.0.0`
  - Or use GitHub UI: Releases > Draft a new release > Tag `vX.Y.Z` > Publish
- The `Release` workflow builds on Windows and Linux, then attaches:
  - `mdmdview-vX.Y.Z-windows-x86_64.zip`
  - `mdmdview-vX.Y.Z-linux-x86_64.tar.gz`
- You can also run the workflow manually (Actions > Release > Run workflow) and choose a `toolchain` (default `stable`).

### Running Tests

```bash
# Run all unit tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test module
cargo test markdown_renderer
```

### Code Structure

- `src/main.rs` - Application entry point and eframe setup
- `src/app.rs` - Main application logic and UI
- `src/markdown_renderer.rs` - Markdown parsing and rendering engine
- `src/sample_files.rs` - Embedded sample markdown files
- `src/lib.rs` - Library interface for testing

### Building for Release

For distribution, build an optimized single executable:

```bash
# Build optimized release version
cargo build --release

# The executable will be a single file with no external dependencies
# Located at: target/release/mdmdview.exe
# Size: ~4.8MB with embedded samples and syntax highlighting
```

### Command Line Usage

```bash
# Open MarkdownView with welcome screen
mdmdview.exe

# Open a specific markdown file
mdmdview.exe document.md
mdmdview.exe "C:\path\to\my document.md"

# Works with relative and absolute paths
mdmdview.exe ..\README.md
```

## Architecture

### Components

1. **Main App State** (`app.rs`)
   - Manages application state and UI logic
   - Handles file loading and user interactions
   - Implements eframe::App trait for the main loop

2. **Markdown Renderer** (`markdown_renderer.rs`)
   - Parses markdown using pulldown-cmark
   - Converts to egui widgets for display
   - Provides syntax highlighting with syntect

3. **Sample Files** (`sample_files.rs`)
   - Embedded markdown examples
   - Usage instructions and feature demos
   - Built into the executable for easy access

### Dependencies

- `eframe` / `egui` - Cross-platform GUI framework
- `pulldown-cmark` - CommonMark compliant markdown parser
- `syntect` - Syntax highlighting library
- `rfd` - Native file dialogs
- `anyhow` - Error handling

## Contributing

1. Ensure all tests pass: `cargo test`
2. Format code: `cargo fmt`
3. Check for warnings: `cargo clippy`
4. Build release version: `cargo build --release`

## License

This project is provided as‑is for demonstration purposes.

Emoji graphics are © 2020 Twitter, Inc and other contributors, licensed under CC‑BY 4.0 (Twemoji). See https://github.com/twitter/twemoji for details. The images are embedded into the executable for offline use.
























