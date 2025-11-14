# Table Layout Algorithms Research: Complete Index

## Documents Created

This research produced 3 comprehensive documents in your mdmdview project:

### 1. TABLE_LAYOUT_QUICK_REFERENCE.md (Start Here!)
Quick, actionable reference with:
- Three algorithm patterns (code examples)
- Text wrapping and width calculation patterns
- Constraint hierarchy
- Common mistakes to avoid
- Decision tree for choosing algorithms
- Performance benchmarks

**Best for:** Quick lookup, implementation decisions

### 2. TABLE_LAYOUT_ALGORITHMS.md (Detailed Analysis)
Complete technical reference with:
- 10 sections covering all major algorithms
- Detailed explanations of each approach
- Code patterns from production libraries
- CSS table layout standards
- Recommended 3-tier strategy for mdmdview
- Key implementation points and edge cases

**Best for:** Understanding the concepts deeply, reference while coding

### 3. RESEARCH_SUMMARY.md (Executive Summary)
High-level overview with:
- Research scope and key findings
- Three main approaches
- Best algorithm recommendation (comfy-table 7-step)
- Critical implementation details
- Recommended architecture
- Performance targets

**Best for:** Getting oriented, understanding why each recommendation

## Research Sources

All major Rust table libraries examined:

### comfy-table
- **File:** src/utils/arrangement/dynamic.rs
- **Key:** 7-step iterative constraint solving algorithm
- **Innovation:** Step 6 simulates text wrapping to find actual width needs
- **Performance:** ~470 microseconds for complex tables
- **Status:** Cloned to /tmp/comfy-table for reference

### prettytable-rs
- **File:** src/row.rs, src/cell.rs
- **Key:** Simple content-based width calculation
- **Strength:** Predictable, easy to understand
- **Limitation:** Can create very wide tables
- **Status:** Cloned to /tmp/prettytable-rs for reference

### tabled
- **File:** src/settings/width/ (wrap.rs, truncate.rs, justify.rs)
- **Key:** Modular strategy pattern for width management
- **Strength:** Pluggable, reusable components
- **Use:** Different strategies for different contexts
- **Status:** Cloned to /tmp/tabled for reference

### CSS Table Layout
- **Standard:** W3C CSS specification
- **Methods:** Auto layout (content-based) and Fixed layout (first-row-based)
- **Insight:** Trade-offs between flexibility and performance
- **Relevance:** Conceptual framework applicable to GUI contexts

## Key Algorithms Explained

### Algorithm 1: Content-Based (Simplest)
- Scan all cells, take maximum width
- Formula: `column_width = max_content_width + padding`
- Use for: Small tables, all content visible
- Example: prettytable-rs implementation

### Algorithm 2: Proportional Distribution (Balanced)
- Divide available space fairly among columns
- Formula: `average = width / cols; excess = width % cols`
- Use for: Most common tables, need specific width
- Example: comfy-table Step 7

### Algorithm 3: Constrained Dynamic (Most Sophisticated)
- 7-step algorithm with iterative constraint solving
- Steps:
  1. Calculate available space
  2-5. Enforce constraints iteratively
  6. Simulate text wrapping (KEY INNOVATION)
  7. Distribute remaining proportionally
- Use for: Complex layouts, optimal space utilization
- Example: comfy-table complete algorithm

## Critical Implementation Insights

### Insight 1: Text Wrapping Simulation (Game Changer)
Comfy-table's Step 6 simulates text wrapping BEFORE committing widths.
This discovers that wrapped content needs less space than assumed.
This is the key to optimal layouts.

### Insight 2: Grapheme Awareness (Non-Negotiable)
Must handle Unicode properly:
- ASCII: 1 cell
- CJK: 2 cells
- Emoji: variable
- Combining marks: 0 cells
Use: `unicode_width` and `unicode_segmentation` crates

### Insight 3: Fair Remainder Distribution (Fairness)
When dividing width unevenly, distribute remainder left-to-right.
Creates visually balanced tables. Example:
- Total: 100 chars, 7 columns
- Average: 14 chars/col, remainder: 2
- First 2 columns: 15 chars, rest: 14 chars

### Insight 4: Constraint Hierarchy (Priority)
Hard constraints > Soft constraints > Content-based > Proportional

### Insight 5: Word-Aware Wrapping (UX)
Preserve word boundaries when possible. Only break character-level
for words that exceed max width. Better readability.

## Recommended Implementation Path

### Phase 1: Measurement
```rust
struct ColumnMetrics {
    min_width: f32,     // Longest unbreakable word
    natural_width: f32, // Full content
}

fn measure_column(table, col) -> ColumnMetrics
```

### Phase 2: Distribution
```rust
fn allocate_widths(metrics, available_width) -> Vec<f32>
// Try natural, fall back to proportional
```

### Phase 3: Rendering
```rust
// Apply word-aware wrapping
// Track row heights
// Use grapheme-aware width calculations
```

## Testing Checklist

- [ ] Content-based width calculation
- [ ] Proportional distribution (multiple columns)
- [ ] Constraint enforcement (fixed, min, max)
- [ ] Text wrapping effect on width
- [ ] Unicode characters (CJK, emoji, combining)
- [ ] Edge cases (empty, very long words)
- [ ] Multi-line cells and row height
- [ ] Performance benchmarks

## Performance Targets

- Simple tables: < 50 microseconds
- Complex tables: < 500 microseconds
- mdmdview context: Correctness > Speed

## Quick Decision Matrix

| Scenario | Algorithm | Why |
|----------|-----------|-----|
| Implementing first | Pattern 2 | Good balance, simpler than 3 |
| Wide viewport | Pattern 2 | More space available |
| Complex constraints | Pattern 3 | Need optimal utilization |
| Terminal-like narrow | Pattern 3 | Need sophistication |
| Unknown table size | Pattern 3 | Safe choice |

## References by Category

### Terminal Libraries
- comfy-table: Best algorithm (7-step dynamic)
- prettytable-rs: Simplest approach (content-based)
- tabled: Most modular (strategy pattern)
- term-table-rs: Column max-width support

### Standards & References
- Unicode Text Segmentation (UAX #29)
- Unicode Line Breaking (UAX #14)
- CSS table-layout property (W3C)

### Key Crates
- unicode_width: Display width calculation
- unicode_segmentation: Grapheme iteration
- unicode_normalization: Text normalization

## Next Steps

1. Read TABLE_LAYOUT_QUICK_REFERENCE.md (5-10 minutes)
2. Read RESEARCH_SUMMARY.md (5-10 minutes)
3. Review TABLE_LAYOUT_ALGORITHMS.md as needed (reference)
4. Examine comfy-table source: /tmp/comfy-table/src/utils/arrangement/
5. Choose starting algorithm (recommend Pattern 2)
6. Implement measurement phase
7. Add distribution logic
8. Test with real markdown tables

## Document Locations

All in: `C:\language\mdmdview\`

- TABLE_RESEARCH_INDEX.md (this file)
- TABLE_LAYOUT_QUICK_REFERENCE.md (start here)
- RESEARCH_SUMMARY.md (overview)
- TABLE_LAYOUT_ALGORITHMS.md (detailed reference)

Source code cloned to `/tmp/`:
- /tmp/comfy-table/
- /tmp/prettytable-rs/
- /tmp/tabled/

## Questions Answered

Q: What's the best algorithm for mdmdview?
A: Comfy-table's 7-step (Pattern 3), or Pattern 2 for simpler start

Q: How to handle Unicode properly?
A: Use unicode_width and unicode_segmentation crates, never assume ASCII

Q: Why wrap text before calculating width?
A: Wrapped content needs less space; must simulate to find actual needs

Q: How to distribute width fairly?
A: Average + remainder distributed left-to-right

Q: What's the performance like?
A: ~470μs for complex, ~30μs for simple (comfy-table benchmarks)

---

**Start with:** TABLE_LAYOUT_QUICK_REFERENCE.md
**Go deeper:** TABLE_LAYOUT_ALGORITHMS.md
**Get context:** RESEARCH_SUMMARY.md
