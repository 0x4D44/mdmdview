# List Formatting Test

This file tests proper list rendering that was previously broken.

## Features (Should be bulleted)

- **Pure Rust Implementation** - Single executable, no dependencies
- **Fast Rendering** - Efficient markdown parsing with pulldown-cmark  
- **Syntax Highlighting** - Code blocks with proper syntax coloring
- **Native File Dialogs** - Seamless Windows integration
- **Embedded Examples** - Built-in sample files to explore
- **Zoom Support** - Adjustable font sizes for better readability
- **Keyboard Shortcuts** - Quick access to common functions

## Installation Steps (Should be numbered)

1. Download Rust from https://rustup.rs/
2. Clone or download the project
3. Navigate to the project directory
4. Run `cargo build --release`
5. Find the executable in `target/release/`
6. Copy to your desired location
7. Associate with .md files in Windows

## Mixed Content

Some text before a list.

- First bullet point
- Second bullet point with **bold** text
- Third bullet point with `inline code`

And some text after the list.

### Ordered List Example

1. First step
2. Second step  
3. Third step
4. Final step

That should render as proper numbered items, not continuous text!

---

*This file tests list formatting fixes.*
