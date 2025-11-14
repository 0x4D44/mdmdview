# Table Wrap QA Guide

The column-aware renderer is **on by default**. Before testing:

1. Confirm the status bar shows `Wrap: Overhaul`. If it says `Legacy`, enable the feature via *View ▸ Table Wrap Overhaul* or relaunch with `--table-wrap`.
2. Hover the status text to read "Table layout cache: X hits / Y misses" (and the new "Rows rendered" line) so you can capture both cache metrics and virtualization stats.

## 1. Regression Sample
1. Open `examples/regressions/table-wrap.md` (or `examples/regressions/table-wrap.csv`).
2. Resize the window to be narrow; confirm each row stays inside its column.
3. Capture a screenshot for the QA log.

## 2. Cache Profiling
1. Scroll through a long document (the regression file works, but a larger table is even better).
2. Hover the status-bar tooltip and record "Table layout cache: X hits / Y misses" plus "Rows rendered this frame: A / B".
3. Scroll again; hits should increase significantly while misses stay nearly flat, and rendered rows should only cover the visible subset. Record both sets of numbers.

## 3. Diagnostic Trace (optional)
1. Launch with `MDMDVIEW_TABLE_WRAP_TRACE=1` (can be combined with `--table-wrap` / `--no-table-wrap`).
2. The console prints `[table-wrap] row ...` lines showing the computed width and text per cell. Capture snippets if debugging.

## 4. Link & Highlight Checks
1. Add a `[Docs](https://example.org)` link inside the Changes column.
2. Hover to see the pointing cursor; click to open the browser.
3. Use search to highlight "summary"; wrapped cells should retain proper geometry.

Include in the QA report:
- Screenshot of the regression table at narrow width.
- Cache hit/miss numbers before and after scrolling.
- Any anomalies (clipped text, incorrect cursor, cache misses not stabilizing, etc.).
## Table Column QA (2025-11-14)

1. Open `examples/regressions/table-threat-model.md` with table wrap enabled.
2. Verify Description stays moderate while Examples expands to accommodate the longest entries.
3. Resize the window and test DPI 100/125/150% to ensure column widths adapt smoothly (no clipping or stray lines).
4. Toggle View > Table Wrap off/on to confirm legacy grid still renders without virtualization while the overhaul restores wrapping.
5. Drag-resize the Examples column; restart mdmdview and confirm the new width persists.
6. Capture before/after screenshots for the QA report.

