# Phase 1 Code Review -- D2 Layout Overlap Fixes (A1 + A2)

**Reviewer:** Claude Opus 4.6
**Date:** 2026-02-21
**Files reviewed:**
- `C:\language\mdmdview\crates\d2\src\graph.rs` (new methods: `reposition_node`, `offset_node`, `offset_subtree_inner`)
- `C:\language\mdmdview\crates\d2\src\layout_sugiyama.rs` (3 call sites updated)
- `C:\language\mdmdview\crates\d2\src\layout.rs` (`fit_containers` deleted, `compute_root_bbox` added, 4 new tests)
**Design doc:** `wrk_docs/2026.02.21 - HLD - D2 Layout Overlap Fixes V3.md`

---

### Summary

Phase 1 adds container-aware node repositioning to the D2 layout engine, fixing the critical "descendant stranding" bug where children inside containers would remain at their old absolute positions when a parent container was moved by Sugiyama coordinate assignment. The implementation adds two methods to `D2Graph` (`reposition_node` and `offset_node`) and uses them at all three identified call sites, plus removes the redundant triple-call `fit_containers` pass.

### Overall Assessment

**Approve with minor suggestions.** The implementation is correct, follows the V3 design doc faithfully, and all 13 layout tests pass. The borrow patterns are sound, all four direction arms are handled, and the `fit_containers` deletion is clean. The code is ready to proceed.

---

### Strengths

1. **Borrow pattern in `reposition_node` is clean and correct.** The `if let Some(ref mut rect)` borrow is scoped to the block. After the block, `dx`/`dy` are moved out as owned `f64` values, so the mutable borrow is dropped before `self.graph[node].is_container` and `children.clone()` are accessed. The `offset_subtree_inner` takes `&mut self.graph` (the petgraph field directly), avoiding a second `&mut self` borrow. This is the correct Rust pattern for this situation.

2. **`offset_subtree_inner` as a static method on `D2Graph` is the right design.** By taking `&mut StableDiGraph<...>` directly instead of `&mut D2Graph`, it cleanly avoids the aliasing problem where `reposition_node`/`offset_node` hold `&mut self` while needing to recurse into children. The children `.clone()` at each level breaks the borrow chain.

3. **All four direction arms in `assign_coordinates` are correctly handled.** Each arm reads the necessary dimensions (`width`, `height`, or both via `let r = ...unwrap()`) before calling `reposition_node`, and correctly computes the position:
   - `Down`: `(cross_offset, rank_offsets[rank_idx])` -- standard
   - `Up`: `(cross_offset, -(rank_offsets[rank_idx] + r.height))` -- negated Y with height compensation
   - `Right`: `(rank_offsets[rank_idx], cross_offset)` -- swapped axes
   - `Left`: `(-(rank_offsets[rank_idx] + r.width), cross_offset)` -- negated X with width compensation

4. **`arrange_in_line` correctly groups direction arms.** `Down|Up` share the vertical stacking path and `Right|Left` share horizontal. Since `arrange_in_line` places items linearly starting at offset=0, the Up and Left cases don't need negation here (they'll be repositioned by the caller or normalize_positions later). This is correct.

5. **Component stacking correctly uses `offset_node` instead of `reposition_node`.** The design doc explicitly explains this: component stacking is a relative shift, not an absolute repositioning. Each node in the component is a direct child of the container. If a component node is itself a container, `offset_node` propagates the shift to its descendants. No double-shifting occurs because component members are all siblings (direct children of the same container), never in a parent-child relationship with each other.

6. **`fit_containers` deletion is complete and clean.** No orphan references remain in the codebase (verified via grep). The singular `fit_container` function is correctly preserved for use by `layout_recursive`. The replacement `compute_root_bbox` is a straightforward extraction of the root-only branch from the old `fit_containers`.

7. **Tests cover the four key scenarios from the design doc.** Each test exercises a different aspect of the fix:
   - `test_container_children_inside_parent` -- multi-container with cross-container edges
   - `test_nested_container_no_overlap` -- 3-level nesting (the exact bug scenario)
   - `test_container_delta_propagation` -- pure nesting without edges
   - `test_arrange_in_line_containers` -- disconnected containers (the `arrange_in_line` path)

8. **The `assert_inside` and `assert_no_leaf_overlap` helpers produce excellent diagnostic messages.** Each assertion includes the specific coordinate values and labels, making failures easy to diagnose without re-running under a debugger.

---

### Required Changes

None. The implementation is correct as-is.

---

### Suggestions

**Nit 1: Double `unwrap()` after `is_some()` guard could use `let ... else`.**

In `assign_coordinates` and `arrange_in_line`, the pattern is:
```rust
if graph.graph[node].box_.is_some() {
    match direction {
        Direction::Down => {
            let width = graph.graph[node].box_.unwrap().width;
            graph.reposition_node(node, cross_offset, rank_offsets[rank_idx]);
```

The `is_some()` + `unwrap()` is safe but could be expressed more idiomatically as:
```rust
let Some(rect) = graph.graph[node].box_ else { continue; };
// rect is a Copy value, no borrow held
match direction {
    Direction::Down => {
        graph.reposition_node(node, cross_offset, rank_offsets[rank_idx]);
        cross_offset += rect.width + NODE_SPACING_H;
```

Since `Rect` is `Copy`, this binds an owned copy upfront, eliminating the need for `unwrap()` inside each arm. It's also slightly more efficient (one field access instead of two). However, this is cosmetic -- the current code is correct and the design doc explicitly explains why `unwrap()` is safe.

**Nit 2: Two `impl D2Graph` blocks in `graph.rs`.**

The new methods are in a second `impl D2Graph` block (lines 92-144) right after the first one (lines 64-90). These could be merged into a single block. The compiler doesn't care, but it's slightly unusual to have two `impl` blocks for the same type in the same file without a reason (like conditional compilation or trait impls).

**Optional 3: The `0.01` epsilon in `reposition_node` is undocumented.**

```rust
if self.graph[node].is_container && (dx.abs() > 0.01 || dy.abs() > 0.01) {
```

The epsilon avoids unnecessary subtree traversal for sub-pixel moves, which is a sensible optimization. A named constant or a brief comment explaining the threshold would make the intent clearer:
```rust
// Skip subtree traversal for sub-pixel moves (e.g., floating-point drift)
const REPOSITION_EPSILON: f64 = 0.01;
```

**Optional 4: Duplicate subtree offset logic.**

`offset_subtree` in `layout.rs` (lines 176-188) and `offset_subtree_inner` in `graph.rs` (lines 127-143) do the same thing with different signatures (`&mut D2Graph` vs `&mut StableDiGraph`). The design doc acknowledges this in Section 6 ("Why two `offset_subtree` functions?") and marks it as a future cleanup. Agreed with deferring -- the coupling cost of merging isn't worth it right now.

**Optional 5: `assert_no_leaf_overlap` uses `id` instead of `label` for error messages.**

The helper collects `graph.graph[idx].id.clone()` (the fully-qualified ID like `"platform.frontend.app"`) while the other test helpers use `find_by_label` which matches on the `label` field (the display text like `"React App"`). This is actually better for debugging -- the qualified ID tells you exactly which node in the hierarchy is problematic. No change needed; just noting the intentional choice.

**Optional 6: Test coverage gap -- `direction: left` and `direction: up`.**

The current tests all use the default direction (`Down`) or don't specify a direction. There are no integration tests that exercise `Direction::Up` or `Direction::Left` through the full layout pipeline with containers. The `assign_coordinates` direction arms for Up and Left have more complex position math (negation + dimension offset). Adding tests like:
```rust
#[test]
fn test_container_children_inside_parent_direction_up() {
    let graph = layout_ok("direction: up\na: { x; y }\nb: { p; q }\na.x -> b.p");
    // ... assert_inside checks
}
```
would increase confidence that the Up/Left negation math is correct end-to-end. This is not blocking -- the unit-level coordinate assignment for all four arms is implicitly tested by the existing Sugiyama tests, and the math matches the design doc. But Phase 2 or Phase 3 would be a good time to add directional container tests.

---

### Questions

1. **Is the `offset_node` call in component stacking potentially quadratic?** Each `offset_node` call traverses the entire subtree of the node. If a component has `k` container nodes each with `n` descendants, the stacking loop is O(k*n). For typical D2 diagrams (< 200 nodes), this is negligible. But for very large diagrams with many nested containers in one component, this could add up. Worth noting but not worth optimizing now.

2. **Should `compute_root_bbox` include container nodes in the bounding box calculation?** Currently it iterates `graph.objects` which includes both leaves and containers. Including containers is correct -- the root bbox should encompass everything. But container bboxes already encompass their children (because `fit_container` was called), so they're somewhat redundant with their leaf children. No functional impact either way since `min`/`max` operations are idempotent here.

3. **The `normalize_positions` function (lines 216-253 of layout.rs) does NOT use `reposition_node`/`offset_node`.** It directly mutates `rect.x += offset_x` for each object. This is correct because `normalize_positions` shifts ALL objects by the same delta -- there's no parent-child delta propagation needed since every node (including descendants) gets the same offset. But it's worth noting for future maintainers that this is intentionally different from the Sugiyama repositioning pattern.

---

### Verdict

The implementation is faithful to the V3 design doc, correct in all four direction arms, properly handles borrow patterns, cleanly removes the `fit_containers` triple-call, and has good test coverage of the primary scenarios. All 13 layout tests pass. Ship it.
