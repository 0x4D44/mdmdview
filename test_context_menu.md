# Context Menu Test Document

This is a test document to verify the right-click context menu functionality.

## Features to Test

### 1. Regular Text
Right-click on this paragraph to see the context menu. You should be able to copy the selected text.

### 2. Inline Code
Here's some `inline code` that you can right-click to copy.

### 3. Code Block
```python
def hello_world():
    print("Hello, World!")
    return 42
```

### 4. Links
Here's a [link to GitHub](https://github.com) that should have special context menu options.

Also test internal links: [Go to Features](#features-to-test)

### 5. Lists
- First item in the list
- Second item with **bold text**
- Third item with *italic text*
- Fourth item with ~~strikethrough~~

### 6. Table

| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Cell 1   | Cell 2   | Cell 3   |
| Data A   | Data B   | Data C   |

### 7. Blockquote

> This is a blockquote that should also support
> the context menu for copying text.

### 8. Mixed Content

Here's a paragraph with **bold**, *italic*, `code`, and a [link](https://example.com) all mixed together.

---

## Testing Instructions

1. **Right-click on different elements** to see context-specific menus
2. **Right-click on empty space** to see the general context menu
3. **Test copying** with the menu options
4. **Verify clipboard** contents after copying

The context menu should provide:
- Copy options for selected text
- Copy Link URL for links
- Copy Code for code blocks
- Copy All Text for the entire document
- Navigation options (Top/Bottom)