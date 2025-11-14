# Table Layout Algorithms: Executive Summary

## Research Completed

Comprehensive analysis of table layout algorithms used in:
- Terminal table rendering libraries (comfy-table, prettytable-rs, tabled)
- CSS table layout standards (auto vs fixed)
- GUI considerations (egui context)

## Key Findings

### Three Main Approaches

1. **Content-Based (Prettytable-rs)**
   - Simplest: scan all cells, take max width
   - Fast for small tables
   - Can create very wide layouts

2. **Constraint-Based (Comfy-table)**
   - Most flexible: supports fixed, min, max, percentage constraints
   - Fair distribution with remainder handling
   - Multi-pass convergence for optimal results

3. **Modular (Tabled)**
   - Strategy pattern: wrap, truncate, justify operations
   - Pluggable components
   - Reusable across contexts

### Best Algorithm for mdmdview

**Comfy-table's 7-step algorithm** offers the best balance:
- Step 1: Calculate available space
- Steps 2-5: Iterative constraint enforcement
- Step 6: Optimize by simulating text wrapping (KEY INNOVATION)
- Step 7: Proportional distribution with fair remainder handling

Performance: ~470 microseconds for complex tables.

## Critical Implementation Details

### 1. Always Use Grapheme-Aware Width

Use `unicode_width` and `unicode_segmentation` crates:
- ASCII: 1 cell
- CJK: 2 cells
- Emoji: 1-2 cells variable
- Combining marks: 0 cells (attached to previous)
- ANSI sequences: 0 cells (invisible)

### 2. Text Wrapping

Implement word-aware wrapping:
1. Split by spaces
2. Accumulate words on line
3. Start new line when needed
4. Break long words character-by-character

### 3. Remainder Distribution

When dividing width unevenly, distribute remainder left-to-right:
```
average = total / count
excess = total - (average * count)
for item in items:
    if excess > 0:
        width = average + 1
        excess -= 1
    else:
        width = average
```

### 4. Constraint Hierarchy

1. Hard constraints (fixed, hidden)
2. Soft constraints (minimum)
3. Percentage constraints
4. Content-based fallback
5. Proportional distribution

## Recommended Architecture for mdmdview

### Tier 1: Measurement
```
For each column:
  - Calculate min_width (longest unbreakable word)
  - Calculate natural_width (full content)
  - Add padding
  - Return metrics
```

### Tier 2: Distribution
```
Given available_width:
  1. Try natural_widths - if fits, use those
  2. Try min_widths - if necessary
  3. Proportional between min and natural
```

### Tier 3: Rendering
```
For each cell:
  - Apply word-aware wrapping
  - Track row height
  - Use grapheme-aware calculations
```

## Key Crates to Use

```toml
unicode_width = "0.1"           # Display width calculation
unicode_segmentation = "1.10"   # Grapheme iteration
```

## Source Files for Reference

All cloned to /tmp during research:
- `/tmp/comfy-table/src/utils/arrangement/dynamic.rs` (470 lines, 7-step algorithm)
- `/tmp/comfy-table/src/utils/arrangement/constraint.rs`
- `/tmp/prettytable-rs/src/row.rs`
- `/tmp/tabled/src/settings/width/`

## Testing Strategy

Essential test cases:
1. Content-based width calculation
2. Proportional distribution (multiple columns)
3. Constraint enforcement (fixed, min, max)
4. Text wrapping effect on width
5. Unicode characters (CJK, emoji, combining marks)
6. Edge cases (empty cells, very long words)
7. Multi-line cells and row height

## Performance Target

- Simple tables: <50 microseconds
- Complex tables with constraints: <500 microseconds
- mdmdview GUI context: correctness matters more than speed

## Next Steps

1. Decide: adapt existing table_support module or rewrite?
2. Choose starting algorithm (recommend comfy-table's 7-step)
3. Implement measurement phase for egui context
4. Add proportional distribution with remainder handling
5. Implement grapheme-aware wrapping
6. Test with real markdown documents
7. Profile and optimize as needed

## Document Location

Full research: `C:\language\mdmdview\TABLE_LAYOUT_ALGORITHMS.md`

## Conclusion

The **comfy-table 7-step algorithm** is the gold standard. Its key innovation (Step 6) of simulating text wrapping to find actual width needs is what makes it superior to simpler approaches. For mdmdview in an egui GUI context with potentially wider viewport than terminal:

1. Start with simple measurement (tier 1)
2. Use proportional distribution (tier 2)
3. Add sophisticated wrapping as needed (tier 3)

This balances simplicity, correctness, and performance.
