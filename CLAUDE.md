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

# Run in development
cargo run

# Run with a specific file
cargo run -- document.md

# Run tests
cargo test

# Run specific test module
cargo test markdown_renderer

# Code formatting and linting
cargo fmt
cargo clippy
```

## Architecture

### Core Components

1. **Main Entry Point** (`src/main.rs`)
   - Application setup and eframe configuration
   - Command-line argument parsing for file opening
   - Window configuration and icon setup

2. **Application Logic** (`src/app.rs`)
   - Main app state management (`MarkdownViewerApp`)
   - UI rendering with egui
   - Keyboard shortcuts and navigation handling
   - File operations and error handling

3. **Markdown Renderer** (`src/markdown_renderer.rs`)
   - Markdown parsing using pulldown-cmark
   - Conversion to egui widgets for display
   - Syntax highlighting with syntect
   - Inline element handling (code, links, formatting)

4. **Sample Files** (`src/sample_files.rs`)
   - Embedded markdown examples built into the binary
   - Accessible via File → Samples menu

### Key Architecture Decisions

- **Single Binary**: All dependencies and samples embedded for easy distribution
- **egui Framework**: Cross-platform immediate-mode GUI with native feel
- **pulldown-cmark**: CommonMark compliant markdown parsing
- **syntect**: Syntax highlighting for code blocks
- **Windows Integration**: Custom icon and metadata via build.rs

### Navigation System

The app implements keyboard-driven navigation:
- `NavigationRequest` enum handles different scroll types
- Main scroll area uses persistent state via `egui::Id`
- Keyboard shortcuts processed in `handle_keyboard_input()`

### Font and Rendering

- Configurable font sizes for different markdown elements
- Proper inline element rendering (fixed in recent updates)
- Table rendering with headers and striped rows
- Professional styling with proper spacing

## Development Patterns

### Error Handling
- Uses `anyhow::Result` for consistent error propagation
- Error messages displayed in UI rather than panicking
- Graceful fallbacks for missing files or parsing errors

### State Management
- Centralized app state in `MarkdownViewerApp`
- Parsed content cached as `Vec<MarkdownElement>`
- Window state persistence via eframe

### Testing Strategy
- Unit tests for markdown parsing and rendering
- Integration tests for file loading
- Use `tempfile` crate for file system tests

## Windows-Specific Features

- Custom application icon (icon.ico)
- File metadata and version information
- Shell association support for .md files
- Native file dialogs using rfd crate

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