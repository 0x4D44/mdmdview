# Table Layout Algorithms: Quick Reference

## Three Algorithm Patterns

### Pattern 1: Content-Based (Simplest)
```
For each column:
  max_width = 0
  For each cell:
    max_width = max(max_width, cell_width)
  final_width = max_width + padding
```
**Use when:** Small tables, all content should be visible
**Pro:** Simple, predictable
**Con:** Can create very wide tables

### Pattern 2: Proportional Distribution (Balanced)
```
average = remaining_width / num_columns
excess = remaining_width - (average * num_columns)

For column in columns:
  width = average + (if excess > 0 then 1 else 0)
  excess -= 1
```
**Use when:** Need to fit specific width, columns have similar importance
**Pro:** Fair, predictable, good for most tables
**Con:** May squeeze content

### Pattern 3: Comfy-table 7-Step (Most Sophisticated)
```
1. Calculate available space
2-5. Iteratively enforce constraints (converge)
6. Optimize by simulating text wrapping
7. Distribute remaining space proportionally
```
**Use when:** Complex constraints, optimal space utilization needed
**Pro:** Multi-pass, content-aware, handles edge cases
**Con:** More complex to implement

## Text Wrapping Pattern

```rust
// Word-aware wrapping (preserves word boundaries)
fn wrap(text, max_width):
    lines = []
    current = ""
    for word in text.split(' '):
        if fits(current + word, max_width):
            current += " " + word
        else:
            if current not empty: lines.append(current)
            if word_width > max_width:
                for chunk in break_word(word, max_width):
                    lines.append(chunk)
            else:
                current = word
    if current not empty: lines.append(current)
    return lines
```

## Width Calculation Pattern

```rust
// Always grapheme-aware for Unicode support
use unicode_width::UnicodeWidthStr;
use unicode_segmentation::UnicodeSegmentation;

// Display width of string
let width = text.width();  // Handles CJK, ANSI, combining marks

// Safe breaking at width boundary
for grapheme in text.graphemes(true) {
    let g_width = grapheme.width();  // 0, 1, or 2 cells
    if current_width + g_width > max_width {
        // break here
    }
}
```

## mdmdview Table Wrap Overhaul Notes (2025-11-20)
- Header height is now estimated per-table from header text (wrap-aware) instead of a fixed 28 px.
- Row heights grow when rendered content exceeds the prior hint; a repaint is requested to avoid clipping.
- Column stats sample all effective columns (including extra trailing cells) and use `unicode-width` to size CJK/fullwidth text.
- Up to two columns can be marked as remainder when stats show long or rich content; Examples/notes stay remainder-first, IDs/dates remain fixed.
- Table instance identity now includes the element index, so persisted widths do not bleed between identical tables.
- Legacy renderer caps image width estimates to a thumbnail so images no longer blow up width resolution.

## Constraint Hierarchy

1. **Fixed constraints** (must be exact)
   - ContentWidth: exact fit
   - Absolute(Fixed): fixed number of chars
   - Hidden: don't display

2. **Range constraints** (flexible within bounds)
   - LowerBoundary: minimum width
   - UpperBoundary: maximum width
   - Boundaries: both min and max

3. **Percentage constraints** (relative to table)
   - Percentage: X% of available width

4. **Fallback** (no constraints)
   - Content-based: use content width
   - Proportional: distribute equally

## Remainder Distribution

When dividing width unevenly, use this pattern for fairness:

```rust
let per_unit = total / count;
let mut extra = total - (per_unit * count);

for i in 0..count {
    let width = if extra > 0 {
        extra -= 1;
        per_unit + 1
    } else {
        per_unit
    };
    allocate(i, width);
}
```

This distributes remainder left-to-right, creating visual balance.

## Unicode Width Matrix

| Type | Width | Example | Notes |
|------|-------|---------|-------|
| ASCII | 1 | a, 1, ! | Standard monospace |
| CJK | 2 | 中, 日 | Chinese, Japanese, Korean |
| Emoji | 1-2 | 👍, 🎉 | Variable, context-dependent |
| Combining | 0 | ́ (accent) | Attached to previous char |
| ANSI | 0 | \033[31m | Invisible control sequences |

## Minimum Viable Widths

When space is extremely limited:

```
1. Minimum per column: 1 character
2. Content constraint: show longest word if possible
3. Truncation: "..." if needed
4. Fallback: single character placeholder
```

## Decision Tree for Algorithm Choice

```
Is the viewport width known?
  NO  -> Use content-based (Pattern 1)
  YES -> Are there constraints?
           NO  -> Use proportional (Pattern 2)
           YES -> Is performance critical?
                    NO  -> Use comfy-table 7-step (Pattern 3)
                    YES -> Use Pattern 2 with constraints
```

## Common Mistakes to Avoid

1. **Not grapheme-aware**
   - ❌ Counting characters: "ñ".len() = 2 bytes
   - ✓ Using width: "ñ".width() = 1 cell

2. **Not handling ANSI**
   - ❌ Including escape codes in width: "\033[31m" visible
   - ✓ Stripping or ignoring them

3. **Not distributing remainder**
   - ❌ All width to first column
   - ✓ Distribute left-to-right for visual balance

4. **Not simulating wrapping**
   - ❌ Assuming wrapped text needs full space
   - ✓ Simulating to find actual needs (comfy-table Step 6)

5. **Not handling edge cases**
   - ❌ Empty cells, very long words, special chars
   - ✓ Pre-check and handle gracefully

## Performance Benchmarks

| Scenario | Algorithm | Time |
|----------|-----------|------|
| 5 columns, simple content | Pattern 2 | ~5 μs |
| 20 columns, varied content | Pattern 2 | ~20 μs |
| Many constraints | Pattern 3 | ~470 μs |
| Very large table | Pattern 1 | Varies with size |

## For mdmdview Implementation

**Recommended starting point: Pattern 2 (Proportional)**

Provides good balance of:
- Simplicity (straightforward algorithm)
- Correctness (handles most tables well)
- Performance (fast even for large tables)
- Fairness (even distribution)

Later, add Pattern 3 optimizations if needed:
- Multi-pass constraint enforcement
- Text wrapping simulation
- Complex constraint support

## References

See full research in:
- `TABLE_LAYOUT_ALGORITHMS.md` - Detailed analysis
- `RESEARCH_SUMMARY.md` - Executive summary

Source code cloned to `/tmp/`:
- comfy-table: Advanced algorithm reference
- prettytable-rs: Content-based implementation
- tabled: Modular strategy pattern
