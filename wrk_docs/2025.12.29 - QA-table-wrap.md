# Table Wrap QA Guide

The table renderer is **on by default**. Before testing:

1. Confirm the status bar shows `Tables: Wrap`.
2. Hover the status text to read "Table layout cache: X hits / Y misses" (and the "Rows rendered this frame" line) so you can capture both cache metrics and virtualization stats.

## 1. Regression Sample
1. Open `examples/regressions/table-wrap.md` (or `examples/regressions/table-wrap.csv`).
2. Resize the window to be narrow; confirm each row stays inside its column.
3. Capture a screenshot for the QA log.

## 2. Cache Profiling
1. Scroll through a long document (the regression file works, but a larger table is even better).
2. Hover the status-bar tooltip and record "Table layout cache: X hits / Y misses" plus "Rows rendered this frame: A / B".
3. Scroll again; hits should increase significantly while misses stay nearly flat, and rendered rows should only cover the visible subset. Record both sets of numbers.

## 3. Link & Highlight Checks
1. Add a `[Docs](https://example.org)` link inside the Changes column.
2. Hover to see the pointing cursor; click to open the browser.
3. Use search to highlight "summary"; wrapped cells should retain proper geometry.

Include in the QA report:
- Screenshot of the regression table at narrow width.
- Cache hit/miss numbers before and after scrolling.
- Any anomalies (clipped text, incorrect cursor, cache misses not stabilizing, etc.).

## Table Column QA (2025-11-14)

1. Open `examples/regressions/table-threat-model.md`.
2. Verify Description stays moderate while Examples expands to accommodate the longest entries.
3. Resize the window and test DPI 100/125/150% to ensure column widths adapt smoothly (no clipping or stray lines).
4. Watch the custom vertical dividers/border while resizing; they should stay aligned to column boundaries without drawing through cell text.
5. Drag-resize the Examples column; restart mdmdview and confirm the new width persists.
6. Capture before/after screenshots for the QA report.

## Table Polish QA (2025-11-30)

### Header Separator
1. Open any table file (e.g., `examples/regressions/table-threat-model.md`).
2. Verify a horizontal line appears between the header row and body rows.
3. The separator should use the same stroke style as vertical dividers.

### Table Border Alignment
1. With a table visible, verify the outer border exactly matches the column boundaries.
2. Resize the window - the border should track column resizing without gaps.
3. Scroll a long table - dividers should not leak into the scrollbar area.

### Zoom and Persisted Widths
1. Open a table and manually resize a column (drag the column edge).
2. Close and reopen the file - the custom width should persist.
3. Change zoom level (Ctrl++ or Ctrl+-).
4. Verify the custom column width resets to default (persisted widths are cleared on zoom change).
5. This prevents size mismatches when font sizes change.

Include in the QA report:
- Screenshot showing header separator line
- Confirmation that table borders align with column boundaries
- Confirmation that zoom change resets persisted column widths

