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

- Use `File → Open` or `Ctrl+O` to load a markdown file
- Drag and drop `.md` files directly into the application
- Use `File → Samples` to explore built-in examples
- **Command line**: `mdmdview.exe filename.md` to open a specific file

### Windows Shell Integration

You can associate MarkdownView with `.md` files for easy opening:

1. Right-click any `.md` file in Windows Explorer
2. Select "Open with → Choose another app"
3. Browse to `mdmdview.exe`
4. Check "Always use this app to open .md files"

Now double-clicking any `.md` file will open it in MarkdownView!

### File Properties and Icon

The executable includes complete Windows metadata:
- **Product Name**: MarkdownView
- **File Description**: A simple markdown viewer for Windows
- **Version**: 1.0.0
- **Company**: MarkdownView Project
- **Copyright**: Copyright © 2025 MarkdownView Project
- **Custom Icon**: Displays markdown document icon in File Explorer

This ensures professional Windows integration with proper file properties and visual recognition.

### Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+O` | Open file |
| `Ctrl+W` | Close current file |
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
- **Inline code** - Properly formatted within text (e.g., use `pip install` command)
- **Links** - Clickable URLs that open in browser
- **Horizontal rules** - Clean section separators
- **Quotes** - Formatted blockquotes (basic support)

**Key Improvement**: Inline code elements like `filename.ext` now stay properly inline with surrounding text instead of breaking to new lines.

### Supported File Types

- `.md` - Standard markdown files
- `.markdown` - Alternative markdown extension
- `.mdown` - Another common variation
- `.mkd` - Short markdown extension

## Development

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

This project is provided as-is for demonstration purposes.
