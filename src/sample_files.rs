//! Sample markdown files embedded in the application
//!
//! These files demonstrate the markdown viewer capabilities and provide
//! usage instructions. They are built into the executable for easy access.

/// Represents a sample markdown file with metadata
#[derive(Debug, Clone)]
pub struct SampleFile {
    pub name: &'static str,
    pub title: &'static str,
    pub content: &'static str,
}

/// Collection of all embedded sample files
pub const SAMPLE_FILES: &[SampleFile] = &[
    SampleFile {
        name: "welcome.md",
        title: "Welcome to MarkdownView",
        content: WELCOME_CONTENT,
    },
    SampleFile {
        name: "formatting.md",
        title: "Formatting Examples",
        content: FORMATTING_CONTENT,
    },
    SampleFile {
        name: "code.md",
        title: "Code Examples",
        content: CODE_CONTENT,
    },
    SampleFile {
        name: "usage.md",
        title: "Usage Instructions",
        content: USAGE_CONTENT,
    },
    SampleFile {
        name: "search.md",
        title: "Search Tips & Examples",
        content: SEARCH_GUIDE_CONTENT,
    },
    SampleFile {
        name: "images.md",
        title: "Inline Images & Diagrams",
        content: IMAGES_CONTENT,
    },
];

/// Welcome content with basic introduction
const WELCOME_CONTENT: &str = r#"# Welcome to MarkdownView

A simple, fast markdown viewer for Windows built with Rust and egui.

## Features

- **Pure Rust Implementation** - Single executable, no dependencies
- **Fast Rendering** - Efficient markdown parsing with pulldown-cmark
- **Syntax Highlighting** - Code blocks with proper syntax coloring
- **Native File Dialogs** - Seamless Windows integration
- **Embedded Examples** - Built-in sample files to explore

## Quick Start

1. **Open a File**: Use `File → Open` or `Ctrl+O` to load a markdown file
2. **Browse Samples**: Use `File → Samples` to explore built-in examples
3. **Navigate**: Scroll to read through documents

Enjoy reading your markdown files with this lightweight, efficient viewer!

---

*Built with ❤️ using Rust, egui, and pulldown-cmark*
"#;

const SEARCH_GUIDE_CONTENT: &str = r#"# Search Tips & Examples

MarkdownView's search panel is accent-aware and case-insensitive. A single query matches multiple spellings automatically.

## Accent Folding

- Typing `resume` highlights `résumé`, `RESUME`, and `résume` (precomposed vs. combining accent).
- `istanbul` matches both `İstanbul` (Turkish dotted capital I) and `ISTANBUL`.
- `sao` finds `São`, `SÃO`, and `São` regardless of combining marks.
- `nino` finds `Niño`, `NINO`, and `niño`.

Try opening this sample, pressing `Ctrl+F`, and searching for the words below:

| Query | Matches in this document |
|-------|-------------------------|
| `resume` | résumé, RESUME, résume |
| `istanbul` | İstanbul, ISTANBUL |
| `sao` | São, SÃO, São |
| `nino` | Niño, NINO, niño |

> Tip: Use `Enter` / `Shift+Enter` to cycle through matches. The highlight respects grapheme clusters, so emoji and combined characters stay intact.

"#;

/// Images and diagrams examples
const IMAGES_CONTENT: &str = r#"# Inline Images & Diagrams

This sample demonstrates inline images and Mermaid code blocks.

## PNG Image

Below is a PNG image using an embedded emoji asset.

![Smiley PNG](assets/emoji/1f600.png "PNG emoji sample")

## SVG Image

An SVG logo rendered via resvg/tiny-skia.

![SVG Logo](assets/samples/logo.svg "SVG sample logo")

## WEBP Image

A small WEBP sample is bundled for testing.

![WEBP Sample](assets/samples/webp_sample.webp "WEBP sample")

## Mermaid Diagram (feature-gated)

When the `mermaid-quickjs` feature is enabled, Mermaid code blocks are rendered as diagrams.
Otherwise, the source is shown with an informational note.

```mermaid
graph TD;
    A[Start] --> B{Is SVG supported?};
    B -- Yes --> C[Rasterize via resvg];
    B -- No  --> D[Show placeholder];
    C --> E[Display egui texture];
    D --> E;
```

## Notes

- The WEBP sample is at `assets/samples/webp_sample.webp`. See `examples/webp_test.md` for a standalone test file.

"#;

/// Comprehensive formatting examples
const FORMATTING_CONTENT: &str = r#"# Markdown Formatting Guide

This document demonstrates the various formatting options supported by MarkdownView.

## Table of Contents

- [Text Formatting](#text-formatting)
- [Headers](#headers)
- [Lists](#lists)
- [Links and Images](#links-and-images)
- [Emojis](#emojis)
- [Blockquotes](#blockquotes)
- [Horizontal Rules](#horizontal-rules)
- [Tables](#tables)

Quick jump: [Go to Lists](#lists) • [Go to Tables](#tables)

## Text Formatting

**Bold text** and *italic text* are supported, as well as ***bold italic***.

You can also use `inline code` and ~~strikethrough~~ text.

## Headers

# Header 1
## Header 2  
### Header 3
#### Header 4
##### Header 5
###### Header 6

## Lists

### Unordered Lists

- First item
- Second item
  - Nested item
  - Another nested item
- Third item
- Inline code in bullets works: use `--help` or `cargo run`
- Styled + emoji in bullets: **:rocket:** launch, *:tada:* celebration, and ~~:fire:~~ warning
- Tight list check: this item has :rocket: and `code` without blank lines between items

### Ordered Lists

1. First item
2. Second item
   1. Nested numbered item
   2. Another nested item
3. Third item

## Links and Images

[Visit Rust's website](https://rust-lang.org)

Internal anchors let you jump within this document:
- [Jump to Headers](#headers)
- [Jump to Tables](#tables)
- [Back to top](#markdown-formatting-guide)

Links are clickable and will open in your default browser.

## Emojis

Unicode emojis render as normal text if your font supports them.
Examples: Thanks! 🎉  Status: ✅  Ship it 🚀

Shortcodes like `:rocket:` and `:tada:` are expanded to emoji images in normal text, headers, and list items.

## Blockquotes

> This is a blockquote.
> It can span multiple lines.
> 
> > Nested blockquotes are also supported.

## Horizontal Rules

---

## Tables

| Feature | Supported | Notes |
|---------|-----------|-------|
| Headers | ✅ | All levels 1-6 |
| Lists | ✅ | Ordered and unordered |
| Code blocks | ✅ | With syntax highlighting |
| Tables | ✅ | Basic table support |
| Links | ✅ | Opens in browser; internal anchors scroll in‑document |

---

*This covers most of the formatting features available in MarkdownView.*  
[Back to top](#markdown-formatting-guide)
"#;

/// Code examples with syntax highlighting
const CODE_CONTENT: &str = r#"# Code Examples

MarkdownView supports syntax highlighting for many programming languages.

## Rust Code

```rust
fn main() {
    println!("Hello, world!");
    
    let mut vector = vec![1, 2, 3, 4, 5];
    vector.iter()
        .filter(|&x| x % 2 == 0)
        .for_each(|x| println!("Even number: {}", x));
}

// Struct definition with lifetimes
struct Config<'a> {
    name: &'a str,
    version: u32,
}

impl<'a> Config<'a> {
    fn new(name: &'a str) -> Self {
        Config { name, version: 1 }
    }
}
```

## Python Code

```python
def fibonacci(n):
    """Generate fibonacci sequence up to n"""
    if n <= 0:
        return []
    elif n == 1:
        return [0]
    
    sequence = [0, 1]
    while len(sequence) < n:
        sequence.append(sequence[-1] + sequence[-2])
    
    return sequence

# List comprehension example
squares = [x**2 for x in range(10) if x % 2 == 0]
print(f"Even squares: {squares}")
```

## JavaScript Code

```javascript
// Modern JavaScript with async/await
async function fetchUserData(userId) {
    try {
        const response = await fetch(`/api/users/${userId}`);
        const userData = await response.json();
        
        return {
            ...userData,
            lastUpdated: new Date().toISOString()
        };
    } catch (error) {
        console.error('Failed to fetch user data:', error);
        throw error;
    }
}

// Arrow functions and destructuring
const processData = ({ name, age, email }) => ({
    displayName: name.toUpperCase(),
    isAdult: age >= 18,
    emailDomain: email.split('@')[1]
});
```

## SQL Code

```sql
-- Complex query with joins and aggregation
SELECT 
    u.username,
    u.email,
    COUNT(o.id) as total_orders,
    SUM(oi.price * oi.quantity) as total_spent
FROM users u
LEFT JOIN orders o ON u.id = o.user_id
LEFT JOIN order_items oi ON o.id = oi.order_id
WHERE u.created_at >= '2023-01-01'
GROUP BY u.id, u.username, u.email
HAVING COUNT(o.id) > 5
ORDER BY total_spent DESC
LIMIT 20;
```

## Inline Code

You can also use `inline code` within paragraphs, like `let x = 42;` or `print("hello")`.

---

*Syntax highlighting is powered by the syntect library, supporting dozens of languages.*
"#;

/// Usage instructions and tips
const USAGE_CONTENT: &str = r#"# Usage Instructions

## Opening Files

### File Menu
- Use `File → Open` from the menu bar
- Keyboard shortcut: `Ctrl+O`
- Select any `.md` or `.markdown` file from your system

### Drag and Drop
- Drag markdown files directly into the application window
- Multiple files can be opened in sequence

## Viewing Sample Files

### Built-in Examples
- Use `File → Samples` to browse embedded examples
- These files demonstrate various markdown features
- Perfect for testing and learning

### Sample File List
- **welcome.md** - Introduction and overview
- **formatting.md** - Text formatting examples  
- **code.md** - Syntax highlighting examples
- **usage.md** - This usage guide

## Navigation

### Scrolling
- Use mouse wheel or trackpad to scroll
- Keyboard arrows for line-by-line navigation
- Page Up/Page Down for faster scrolling

### Window Controls
- Resize the window as needed
- Content automatically reflows
- Minimum window size is enforced

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+O` | Open file |
| `Ctrl+W` | Close current file |
| `F5` | Reload current file |
| `Ctrl+Q` | Quit application |
| `F11` | Toggle fullscreen |
| `Ctrl++` | Zoom in (increase font size) |
| `Ctrl+-` | Zoom out (decrease font size) |
| `Ctrl` + Mouse Wheel | Zoom in/out |
| `Ctrl+0` | Reset zoom to default size |
| `Home` | Go to top of document |
| `End` | Go to bottom of document |
| `Page Up` | Scroll up one page |
| `Page Down` | Scroll down one page |

## File Associations

### Windows Integration
To make MarkdownView the default for `.md` files:

1. Right-click any `.md` file
2. Select "Open with → Choose another app"
3. Browse to the MarkdownView executable
4. Check "Always use this app"

### Supported Extensions
- `.md` - Standard markdown files
- `.markdown` - Alternative markdown extension
- `.mdown` - Another common variation
- `.mkd` - Short markdown extension

## Performance

### Large Files
- Files up to several MB render efficiently
- Very large files (>10MB) may take a moment to parse
- Consider breaking up extremely large documents

### Memory Usage
- Minimal memory footprint
- Parsed content is cached for fast scrolling
- No memory leaks during normal operation

## Troubleshooting

### Common Issues

**File won't open**
- Check file extension is `.md` or `.markdown`
- Ensure file isn't locked by another application
- Verify file contains valid UTF-8 text

**Display issues**
- Try resizing the window
- Restart the application
- Check if file contains very long lines

**Performance problems**
- Close and reopen large files
- Ensure adequate system memory
- Consider breaking up large documents

### Getting Help

For bugs or feature requests:
1. Check the application works with sample files
2. Try reproducing with a minimal markdown file
3. Note your Windows version and system specs

---

*Thank you for using MarkdownView! We hope it serves your markdown reading needs well.*
"#;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_files_exist() {
        assert_eq!(SAMPLE_FILES.len(), 6);

        let names: Vec<&str> = SAMPLE_FILES.iter().map(|f| f.name).collect();
        assert!(names.contains(&"welcome.md"));
        assert!(names.contains(&"formatting.md"));
        assert!(names.contains(&"code.md"));
        assert!(names.contains(&"usage.md"));
        assert!(names.contains(&"search.md"));
        assert!(names.contains(&"images.md"));
    }

    #[test]
    fn test_sample_files_content_not_empty() {
        for file in SAMPLE_FILES {
            assert!(
                !file.content.is_empty(),
                "File {} has empty content",
                file.name
            );
            assert!(!file.title.is_empty(), "File {} has empty title", file.name);
            assert!(!file.name.is_empty(), "File has empty name");
        }
    }

    #[test]
    fn test_sample_files_are_valid_markdown() {
        use pulldown_cmark::{Options, Parser};

        for file in SAMPLE_FILES {
            let parser = Parser::new_ext(file.content, Options::all());
            let events: Vec<_> = parser.collect();
            assert!(
                !events.is_empty(),
                "File {} produces no markdown events",
                file.name
            );
        }
    }

    #[test]
    fn test_welcome_content_structure() {
        assert!(WELCOME_CONTENT.contains("# Welcome"));
        assert!(WELCOME_CONTENT.contains("## Features"));
        assert!(WELCOME_CONTENT.contains("## Quick Start"));
    }

    #[test]
    fn test_formatting_content_examples() {
        assert!(FORMATTING_CONTENT.contains("**Bold text**"));
        assert!(FORMATTING_CONTENT.contains("*italic text*"));
        assert!(FORMATTING_CONTENT.contains("`inline code`"));
        assert!(FORMATTING_CONTENT.contains("| Feature | Supported |"));
    }

    #[test]
    fn test_code_content_languages() {
        assert!(CODE_CONTENT.contains("```rust"));
        assert!(CODE_CONTENT.contains("```python"));
        assert!(CODE_CONTENT.contains("```javascript"));
        assert!(CODE_CONTENT.contains("```sql"));
    }

    #[test]
    fn test_usage_content_sections() {
        assert!(USAGE_CONTENT.contains("## Opening Files"));
        assert!(USAGE_CONTENT.contains("## Keyboard Shortcuts"));
        assert!(USAGE_CONTENT.contains("## Troubleshooting"));
    }

    #[test]
    fn test_search_guide_examples_present() {
        assert!(SEARCH_GUIDE_CONTENT.contains("résumé"));
        assert!(SEARCH_GUIDE_CONTENT.contains("İstanbul"));
        assert!(SEARCH_GUIDE_CONTENT.contains("São"));
        assert!(SEARCH_GUIDE_CONTENT.contains("Niño"));
        assert!(SEARCH_GUIDE_CONTENT.contains("Ctrl+F"));
    }
}
