# Phase 3 Code Review: Adaptive Container Spacing in Sugiyama Layout

**Commit**: `283be34` - "fix: use geometric midpoint for D2 edge label placement"
**Files changed**: `layout_sugiyama.rs`, `layout.rs`, `edge_routing.rs`
**Reviewer**: Claude Opus 4.6
**Date**: 2026-02-21

---

## Summary

This commit introduces adaptive spacing in the Sugiyama coordinate assignment phase so that container nodes receive wider gaps than leaf nodes. Two new constants are added:

- `CONTAINER_CROSS_SPACING` (60px) -- minimum gap between container nodes within the same rank (cross-axis), vs. `NODE_SPACING_H` (40px) for leaves.
- `CONTAINER_RANK_SPACING` (80px) -- minimum gap between ranks when a rank contains a container, vs. `NODE_SPACING_V` (60px) for all-leaf ranks.

The `total_cross` centering calculation is rewritten from a simple `sum + (n-1) * spacing` formula to a per-node loop that selects spacing based on each node's `is_container` flag. The placement loop that follows uses the same per-node spacing selection, ensuring the centering arithmetic and the actual placement agree. Two new integration tests in `layout.rs` verify that containers get wider spacing and leaves are unchanged.

The same commit also includes the edge label placement change (Bezier midpoint to geometric midpoint), which was reviewed in Phase 2 and is not re-reviewed here.

---

## Overall Assessment

**Verdict: Approve with minor suggestions.**

The container spacing logic is correct and consistent across the four direction arms. The `total_cross` centering calculation matches the placement loop. The two new tests verify the core invariant (containers get >= 60px, leaves stay at ~40px). All 111 D2 tests pass with no regressions.

The change is minimal and well-scoped -- only `assign_coordinates` is touched in `layout_sugiyama.rs`, with no changes to crossing reduction or rank assignment.

---

## Required Changes

None.

---

## Suggestions

### S1. Asymmetric gap semantics -- spacing determined by left node only

The gap between two adjacent nodes is determined solely by the **left** (or upper) node's `is_container` flag:

```rust
let cross_spacing = if graph.graph[node].is_container {
    CONTAINER_CROSS_SPACING
} else {
    NODE_SPACING_H
};
// ...
cross_offset += rect.width + cross_spacing;
```

This means:
- `[container] [leaf]` --> 60px gap (container's spacing governs)
- `[leaf] [container]` --> 40px gap (leaf's spacing governs)

This is probably fine in practice because containers are typically wider than leaves, so the visual breathing room matters more on the container's side. But it is an asymmetry worth documenting in a comment. If symmetry is desired, change to `max(left_spacing, right_spacing)` -- but that would complicate both the `total_cross` calculation and the placement loop, so the current approach is a reasonable trade-off.

The `total_cross` centering block and the placement loop are consistent with each other on this point (both use the current node's flag), so centering is not broken.

### S2. `arrange_in_line` not updated for container spacing

`arrange_in_line` (lines 221-237) handles the case where children have no edges between them. It still uses the hardcoded `NODE_SPACING_V` and `NODE_SPACING_H` constants for all nodes, including containers. If two containers with no edges share a parent, they will get the old 40px/60px spacing rather than the new container-aware spacing.

This is unlikely to matter in practice because `arrange_in_line` is only called for disconnected children, and sibling containers without edges are uncommon. But for consistency, consider applying the same `is_container` check here.

### S3. Rank spacing checks current rank only, not the adjacent rank

The rank-direction spacing uses:

```rust
let has_container = rank_nodes.iter().any(|&n| graph.graph[n].is_container);
let rank_spacing = if has_container { CONTAINER_RANK_SPACING } else { NODE_SPACING_V };
```

This checks whether the **current** rank has a container, but not whether the **next** rank does. If rank 0 has only leaves but rank 1 has a container, the gap between ranks 0 and 1 uses `NODE_SPACING_V` (60px) rather than `CONTAINER_RANK_SPACING` (80px). The practical impact is small since containers are taller and the extra 20px matters more on the container's rank side, but it is worth noting in a comment. An alternative would be `has_container || next_rank_has_container` but that would require a look-ahead or two-pass approach.

### S4. (Nit) The `total_cross` block could be extracted to a helper

The `total_cross` computation (lines 755-774) is a 20-line block inside a `let ... = { }` expression. It is doing a meaningful, self-contained calculation. Extracting it to a small helper like `fn cross_rank_span(graph: &D2Graph, nodes: &[NodeIndex], is_horizontal: bool) -> f64` would improve readability and make the relationship between the centering calculation and the placement loop more explicit.

### S5. (Nit) Comment: doc-comments for the new constants

`CONTAINER_CROSS_SPACING` and `CONTAINER_RANK_SPACING` have brief one-line doc comments. Consider adding a note about the rationale: containers have internal structure (labels, padding, child nodes) so they need more breathing room between them to avoid visual crowding. This helps future readers understand why the values differ from the base spacing constants.

### S6. Missing test for mixed container+leaf rank

Neither `test_container_spacing_wider` nor `test_leaf_spacing_unchanged` tests the case where a container and a leaf coexist in the same rank. This is the case where the asymmetry from S1 manifests. A test like `a: { x }\nb\na.x -> c\nb -> c` (container `a` and leaf `b` in rank 0, both pointing to `c` in rank 1) would verify that nodes don't overlap in the mixed case. The existing `assert_no_leaf_overlap` helper could be reused.

---

## Questions

### Q1. Should the last rank also get `CONTAINER_RANK_SPACING` appended?

The rank offset loop appends spacing after every rank including the last one:

```rust
rank_offset += max_size + rank_spacing;
```

This means the last rank gets trailing spacing that is never used by another rank. This is harmless (it just makes the conceptual layout taller) and is the same behavior as before the change. But it means `CONTAINER_RANK_SPACING` on the last rank inflates the bounding box slightly more than necessary. Not a real problem -- `normalize_positions` and `compute_root_bbox` will recalculate the tight bounding box -- just confirming this is understood.

### Q2. Component gap unaffected by containers?

`COMPONENT_GAP` (60px) is used to stack disconnected components side-by-side (line 132). This is independent of whether the components contain containers. Should the component gap also be wider when components contain containers? This is an open design question, not necessarily a bug.

---

## Test Quality Assessment

| Test | What it validates | Verdict |
|------|-------------------|---------|
| `test_container_spacing_wider` | Gap between two containers >= 60px | Good -- directly tests the new `CONTAINER_CROSS_SPACING` behavior |
| `test_leaf_spacing_unchanged` | Gap between two leaves is ~40px, not 60px | Good -- regression guard ensuring leaf spacing wasn't inflated |

The tests are well-designed with clear invariants and tolerances. The `test_leaf_spacing_unchanged` test uses a two-sided assertion (gap < 60.0 AND gap close to 40.0), which is robust.

**What the tests do well:**
- Both use end-to-end `layout_ok` (parse -> compile -> layout), so they exercise the full pipeline.
- Left/right ordering is handled dynamically (`if a.x < b.x`), so the test doesn't depend on a specific node ordering.
- Tolerances are reasonable (0.01 for the container test, 1.0 for the leaf test).

**Gaps in test coverage (low priority, not blocking):**
- No test for mixed container+leaf in the same rank (see S6).
- No test for `direction: right` / `direction: left` / `direction: up` variants of container spacing (only default `direction: down` is tested).
- No test verifying `CONTAINER_RANK_SPACING` (the rank-direction spacing). Both tests only verify cross-rank (horizontal) gaps.
- No test for `arrange_in_line` with containers (the unmodified code path from S2).

---

## Regression Check

All 111 D2 crate tests pass. No pre-existing tests changed behavior. The only tests added are the two new spacing tests in `layout.rs` and the three edge label tests in `edge_routing.rs` (which belong to the edge label portion of the commit).

---

## Files Reviewed

- `C:\language\mdmdview\crates\d2\src\layout_sugiyama.rs` (constants + `assign_coordinates` changes)
- `C:\language\mdmdview\crates\d2\src\layout.rs` (two new spacing tests)
- `C:\language\mdmdview\crates\d2\src\edge_routing.rs` (edge label changes, reviewed in Phase 2)
