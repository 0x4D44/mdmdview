# Phase 2 Code Review: Edge Label Positioning + Container Overlap Fixes

**Commit**: `fd7be24` - "fix: resolve container descendant stranding in D2 layout"
**Files changed**: `edge_routing.rs`, `graph.rs`, `layout.rs`, `layout_sugiyama.rs`
**Reviewer**: Claude Opus 4.6
**Date**: 2026-02-21

---

## Summary

This commit addresses two related problems in the D2 layout engine:

1. **Container descendant stranding**: When Sugiyama's `assign_coordinates`, `arrange_in_line`, and component stacking moved container nodes, their descendants were left at old positions. This caused children of sibling containers to pile on top of each other.

2. **Edge label positioning**: The old `bezier_midpoint` approach placed labels at the Bezier curve's parametric midpoint (t=0.5), which could land inside a shape when the curve bows inward. The new approach uses the geometric midpoint of the clipped endpoints (shape-boundary to shape-boundary), guaranteeing the label sits in the visible gap between shapes. Unlabeled edges now skip label computation entirely (`None` instead of a useless point).

The fix introduces `reposition_node` and `offset_node` on `D2Graph` that propagate position deltas to descendants, and replaces the triple `fit_containers` call with bottom-up fitting in `layout_recursive` plus a final `compute_root_bbox`.

---

## Overall Assessment

**Verdict: Approve with minor suggestions.**

The changes are correct, well-structured, and well-tested. The core insight -- that moving a container must propagate deltas to all descendants -- is the right fix, and the implementation is clean. The edge label change from Bezier midpoint to geometric midpoint is a clear simplification that produces better results. The test coverage is thorough, testing vertical, horizontal, and unlabeled edge cases, plus four container direction variants and nested/overlap scenarios.

No required changes. All 110 D2 tests pass.

---

## Required Changes

None.

---

## Suggestions

### S1. (Nit) Remove `bezier_midpoint` rather than `#[allow(dead_code)]`

`bezier_midpoint` (lines 187-217 of `edge_routing.rs`) is no longer called from production code. It has `#[allow(dead_code)]` and is only exercised by `test_bezier_midpoint`. Dead utility functions that exist solely to have a test are pure maintenance burden -- they'll silently drift out of sync with any future Bezier path format changes.

**Recommendation**: Delete `bezier_midpoint` and `test_bezier_midpoint`. If Bezier-curve-aware label placement is needed later (e.g., for curved multi-segment edges), it can be reintroduced from git history.

### S2. (Nit) Duplicate `offset_subtree` logic

`layout.rs:176-188` has a standalone `offset_subtree` function that does the same thing as `D2Graph::offset_subtree_inner` in `graph.rs:128-144`. The `layout.rs` version is called from `fit_container` (line 140) to push children down for container labels.

This isn't a bug, but it's two implementations of the same recursive tree-offset. Consider refactoring `fit_container` to use `graph.offset_node(child, 0.0, label_height)` for each direct child (which already recurses into descendants). This would eliminate the standalone `offset_subtree` entirely.

### S3. (Nit) `normalize_positions` does not use `reposition_node`

`normalize_positions` (layout.rs:216-253) applies offsets directly to `rect.x`/`rect.y` without using `reposition_node`/`offset_node`. This works correctly because the offset is uniform (same delta to all nodes including the root), so relative positions within containers are preserved. However, for consistency with the new pattern established in this commit, consider using `offset_node` here too. The `0.01` threshold in `reposition_node` would not interfere since normalize offsets are typically many pixels.

### S4. (Nit) Test helper duplication across modules

`layout_ok`, `find_by_label` / `find_node_by_label`, and `find_edge_between` are defined in both `edge_routing.rs::tests` and `layout.rs::tests`. They have slightly different signatures (one returns `&D2Object`, the other returns `NodeIndex`). This is fine for now since they're `#[cfg(test)]` only, but if more test files accumulate, consider a shared `#[cfg(test)] mod test_helpers` in `lib.rs` to avoid further divergence.

### S5. (Nit) Comment on `reposition_node` threshold

`reposition_node` (graph.rs:107) skips subtree traversal when `dx.abs() <= 0.01 && dy.abs() <= 0.01`. The threshold is reasonable for floating-point noise, but there's no doc comment explaining why 0.01 was chosen and what units it's in (pixels). A one-line comment like `// 0.01px: below visual significance, avoids O(n) traversal for fp drift` would help future readers.

### S6. (Optional) Edge label vertical offset for readability

The old code had `mid.y - 10.0` to push the label slightly above the midpoint. The new code places it exactly at the geometric center, which the SVG renderer then centers text on. This is geometrically correct but means the label text sits centered on the edge line itself. Consider whether a small perpendicular offset (2-4px) would improve readability by avoiding overlap with the edge stroke. The background halo in `svg_render.rs:540-548` mitigates this somewhat, so this is purely aesthetic and low priority.

---

## Questions

### Q1. Should `offset_node` propagate unconditionally for non-containers?

`offset_node` (graph.rs:115-124) always recurses into children if `is_container` is true, but doesn't check the `0.01` threshold that `reposition_node` checks. Is this intentional? The use case (component stacking shifts) should always have meaningful deltas, so the threshold isn't needed. But the asymmetry between the two methods could be confusing. Worth a brief comment.

### Q2. Does `compute_root_bbox` need to include container nodes?

`compute_root_bbox` iterates over `graph.objects` which includes both containers and leaves. If containers always enclose their children (guaranteed by `fit_container`), then including containers is redundant but harmless -- the min/max is dominated by the outermost containers. This is fine; just confirming the intent is "belt and suspenders."

---

## Test Quality Assessment

The new tests are well-designed:

| Test | What it validates | Verdict |
|------|-------------------|---------|
| `test_label_between_nodes` | Label y is between a.bottom and b.top (vertical layout) | Good -- tests the actual geometric constraint |
| `test_label_between_nodes_horizontal` | Label x is between a.right and b.left (horizontal layout) | Good -- covers the orthogonal axis |
| `test_unlabeled_edge_no_label_position` | Unlabeled edge gets `None` | Good -- regression guard |
| `test_container_children_inside_parent` | Children enclosed by parent (default direction) | Good |
| `test_container_children_inside_parent_direction_up` | Same for direction=up | Good |
| `test_container_children_inside_parent_direction_left` | Same for direction=left | Good |
| `test_nested_container_no_overlap` | No leaf overlap in 3-level nesting | Good -- the regression case |
| `test_container_delta_propagation` | 3-level nesting: inner inside mid inside outer | Good |
| `test_arrange_in_line_containers` | Edge-less containers with children | Good edge case |

**Missing test coverage** (low priority, not blocking):
- `direction: right` variant for container containment (only down, up, left are tested)
- Parallel edges with labels (to verify offset midpoints stay in the gap)
- Self-loop label position (tested implicitly via existing `test_e2e_self_edge` but no explicit label position check)

---

## Files Reviewed

- `C:\language\mdmdview\crates\d2\src\edge_routing.rs` (label positioning + tests)
- `C:\language\mdmdview\crates\d2\src\graph.rs` (`reposition_node`, `offset_node`, `offset_subtree_inner`)
- `C:\language\mdmdview\crates\d2\src\layout.rs` (`layout_recursive` refactor, `compute_root_bbox`, container tests)
- `C:\language\mdmdview\crates\d2\src\layout_sugiyama.rs` (`arrange_in_line`, `assign_coordinates`, component stacking)
