# Table Layout Algorithms Research

## Overview

Research on table layout algorithms from:
- comfy-table (advanced dynamic algorithm)
- prettytable-rs (content-based approach)
- tabled (modular strategy pattern)
- CSS table-layout specification

## 1. Content-Based Width (Prettytable-rs)

Scan all cells, take maximum width. Simplest approach.

Key code: prettytable-rs/src/row.rs get_column_width()
- Single-span cells: return width directly
- Multi-span cells: distribute proportionally with ceiling division
- Formula: (w as f64 / c.get_hspan() as f64).ceil() as usize

Strengths: Simple, predictable
Weaknesses: Can create very wide tables, ignores terminal width

## 2. Proportional Distribution

Distribute available width based on column weights.

Algorithm:
1. total_weight = sum of all column weights
2. For each column: width = (weight / total_weight) * available_width
3. Distribute remainder from integer division left-to-right

Code pattern from comfy-table:

```rust
let average_space = remaining_width / remaining_columns;
let mut excess = remaining_width - (average_space * remaining_columns);

for column in columns {
    let width = if excess > 0 {
        excess -= 1;
        average_space + 1
    } else {
        average_space
    };
}
```

## 3. Constraint-Based Systems (Comfy-table)

Constraint types:
- ContentWidth: exact fit
- Absolute(Fixed or Percentage): fixed width
- LowerBoundary/UpperBoundary: min/max constraints
- Boundaries: both min and max
- Hidden: hide column

Resolution process:
1. Hard constraints evaluated first (fixed, hidden)
2. Soft constraints applied (minimum widths)
3. Percentage constraints converted to absolute values

Key: absolute_value_from_width() handles percentages:
- Fixed returns value directly
- Percentage: (table_width - borders) * percent / 100

## 4. Comfy-table's 7-Step Dynamic Algorithm

MOST SOPHISTICATED ALGORITHM. Multi-pass constraint solving.

Step 1: Calculate Available Space
- Subtract borders
- Subtract padding for all columns
- Subtract any fixed-width columns already assigned

Steps 2-5: Iterative Constraint Enforcement
Loop through undecided columns:
- If LowerBoundary > average_remaining: assign it
- If content_width < average_remaining: assign it
- Repeat until no changes (convergence)

Step 6: Optimize After Text Splitting (KEY INNOVATION)
Simulate text wrapping to find actual width needs:
1. At average_space, split all cells in column
2. Find longest line after wrapping
3. If longest_line < average_space: assign that width
4. Repeat with updated average

This is crucial because wrapped content needs less space than original text suggests.

Step 7: Distribute Remaining Equally
- Average space per remaining column
- Distribute remainder left-to-right for fairness

Performance: ~470 microseconds for complex tables with constraints

Why effective:
- Multi-pass: doesn't commit to widths early
- Content-aware: simulates real wrapping
- Fair: remainder distribution predictable
- Robust: handles extreme cases (narrow terminals)

## 5. Text Wrapping Algorithms

### Word-Aware Wrapping

Preferred for readability. Algorithm:
1. Split text by delimiter (typically space)
2. Accumulate words on current line
3. If next word doesn't fit: start new line
4. If single word exceeds max_width: break character-by-character

Preserves word boundaries when possible.

### Grapheme-Aware Width Calculation

Critical for Unicode support. Problem:
- ASCII: 1 cell width
- CJK characters: 2 cells
- Emoji: 1-2 cells variable
- Combining marks: 0 cells (attached)
- ANSI escape sequences: 0 cells (invisible)

Solution: Use unicode_width and unicode_segmentation crates

```rust
use unicode_width::UnicodeWidthStr;
use unicode_segmentation::UnicodeSegmentation;

fn display_width(s: &str) -> usize {
    s.width()  // Handles double-width, ANSI
}

for grapheme in text.graphemes(true) {
    let width = grapheme.width();
    // width is 0, 1, or 2 per grapheme cluster
}
```

## 6. CSS Table Layout Standards

### Auto Layout (Content-Based)
- Scan all table content
- Calculate column widths proportionally
- Browser doesn't render until layout complete
- Good for variable content, slower

### Fixed Layout (Fast)
- Use ONLY first row to determine widths
- No further content scanning
- Constant time performance
- 10-100x faster for large tables
- Trade-off: less flexible

---

## 7. Recommended Strategy for mdmdview

Three-tier approach balancing simplicity with effectiveness:

### Tier 1: Measurement

```rust
struct ColumnMetrics {
    min_width: f32,          // Longest unbreakable word
    natural_width: f32,      // Full content
    expand_factor: f32,      // Expansion preference
}

// Measure column:
// - Min: longest single word or atomic unit
// - Natural: full text without wrapping
// - Add padding to both
```

### Tier 2: Distribution

Given available space:

```
1. If sum(natural_width) <= available:
   Use natural widths (all content visible)

2. Else if sum(min_width) > available:
   Shrink proportionally to available space

3. Else (middle ground):
   Distribute between min and natural
   proportional to content "expand_potential"
```

### Tier 3: Wrapping

Apply word-aware wrapping to cells that exceed allocated width.
Recalculate row heights accordingly.

## 8. Key Implementation Points

1. **Always grapheme-aware**
   - Use unicode_width for width calculations
   - Use unicode_segmentation for text iteration
   - Never assume ASCII

2. **Fair remainder distribution**
   - Left-to-right distribution preferred
   - Creates more visually balanced tables

3. **Edge cases**
   - Empty cells: treat as one space width
   - Very long words: break character-level if needed
   - Multi-line cells: track row height correctly
   - Special Unicode: preserve emoji, combining marks

4. **When to use multi-pass**
   - Simple case: single pass usually sufficient
   - Complex constraints: comfy-table style multiple passes
   - Measure when worth it: test with real data

## 9. Performance Reference

comfy-table benchmarks:
- Complex tables with constraints: ~470 microseconds
- Simple tables: ~30 microseconds

For mdmdview (GUI context):
- More space typically available than terminal
- Can afford more sophisticated algorithm
- User expectations different from CLI

## 10. Code Organization Suggestion

Separate concerns in table_support module:

```
ColumnMetrics trait: Measurement
WidthDistributor: Space allocation
TextWrapper: Content wrapping
TableLayout: Coordinates above
```

Benefits:
- Independent testing of algorithms
- Easy algorithm swapping
- Clear responsibility boundaries
- Reusability

## References

### Source Code
- comfy-table/src/utils/arrangement/dynamic.rs (7-step algorithm)
- comfy-table/src/utils/arrangement/constraint.rs (constraint evaluation)
- comfy-table/src/column.rs (column configuration)
- prettytable-rs/src/row.rs (content-based width)
- prettytable-rs/src/cell.rs (cell measurement)
- tabled/src/settings/width/ (modular strategies)

### Unicode Standards
- Unicode Standard Annex #29: Text Segmentation
- Unicode Standard Annex #14: Line Breaking

### Key Crates
- unicode_width: Display width calculation
- unicode_segmentation: Grapheme cluster iteration
- unicode_normalization: Text normalization

---

## Additional Resources

### Cloned Repositories for Reference

During this research, the following repositories were cloned and examined:
- `/tmp/comfy-table`: Advanced dynamic algorithm reference
- `/tmp/prettytable-rs`: Content-based width calculations
- `/tmp/tabled`: Modular width strategy patterns

Key files examined:
- comfy-table: src/utils/arrangement/dynamic.rs (470 lines, 7-step algorithm)
- comfy-table: src/utils/arrangement/constraint.rs (constraint evaluation)
- prettytable-rs: src/row.rs (width calculation with multi-span support)
- prettytable-rs: src/cell.rs (cell measurement)
- tabled: src/settings/width/ (modular strategies: wrap, truncate, justify)

### Implementation Strategy Summary

For mdmdview table rendering in egui context:

1. **Measure Phase** (per-column analysis):
   - Calculate minimum width (longest unbreakable word)
   - Calculate natural width (full content)
   - Track expand preference

2. **Allocate Phase** (space distribution):
   - Try natural widths first
   - Fall back to proportional if needed
   - Ensure minimum viability

3. **Render Phase** (content adaptation):
   - Apply word-aware wrapping
   - Adjust row heights accordingly
   - Use grapheme-aware width calculations

### Critical Success Factors

1. **Grapheme awareness**: Must handle Unicode properly
   - Use `unicode_width` for display calculations
   - Use `unicode_segmentation` for text iteration
   - Never assume ASCII

2. **Fair distribution**: Remainder handling matters
   - Left-to-right distribution creates visual balance
   - Formula: `excess = total - (average * count)`

3. **Content awareness**: Wrapping changes width needs
   - Comfy-table's Step 6 is key innovation
   - Simulate wrapping to find actual needs
   - Don't over-allocate space

4. **Edge case handling**: Real data is messy
   - Empty cells, very long words, special characters
   - Multi-line cells and row height tracking
   - Terminal width constraints (for sidebar/preview)

### Performance Considerations

- Simple tables: single pass sufficient
- Complex constraints: multi-pass optimization worth it
- comfy-table achieves 470μs for complex cases
- Prioritize correctness in GUI context (humans notice layout errors immediately)

### Next Steps for Implementation

1. Review comfy-table's 7-step algorithm in detail
2. Adapt measurement phase for egui text rendering
3. Implement proportional distribution with remainder handling
4. Add grapheme-aware width calculations
5. Test with real markdown tables (various sizes, content types)
6. Profile and optimize if needed

