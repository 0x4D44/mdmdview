# Testing Guide

This document describes all testing in the mdmdview repository: what is tested, how tests are organized, how to run them, and the infrastructure that supports them.

## Quick Reference

```bash
# Run the full test suite (~1,100+ tests, ~8 minutes)
nice -n 10 mdtimeout 600 cargo test

# Run a single module (seconds)
cargo test markdown_renderer
cargo test app
cargo test mermaid_renderer
cargo test pikchr_renderer
cargo test window_state

# Run with output
cargo test -- --nocapture

# Pre-release validation (format + lint + build + test)
.\scripts\prerelease.ps1

# Full test suite with optional coverage
.\scripts\full_test.ps1
.\scripts\full_test.ps1 -SkipCoverage

# Full test runner (Rust binary, generates HTML report)
cargo run --release --bin full_test
cargo run --release --bin full_test -- --skip-coverage
cargo run --release --bin full_test -- --quick --skip-mermaid

# Code coverage
cargo llvm-cov --workspace --summary-only
```

---

## Test Categories

The codebase uses four categories of testing:

| Category | Count | Runtime | Description |
|----------|-------|---------|-------------|
| [Unit tests](#unit-tests) | ~1,100+ | ~8 min (full), seconds (per module) | Rust `#[test]` functions across 29 source files |
| [D2 conformance tests](#d2-conformance-tests) | 12 | ~30-60 sec | Geometric invariant checks on D2 diagram output |
| [Visual regression tests](#visual-regression-tests) | 26 cases | ~5-10 min | Pixel-diff screenshot comparison for markdown rendering |
| [Mermaid visual tests](#mermaid-visual-tests) | 15 cases | ~2-5 min | Screenshot comparison against Mermaid CLI reference |

---

## Unit Tests

Unit tests are co-located with the source code using Rust's `#[cfg(test)]` convention. Every major module has its own test suite.

### Test Distribution

#### Core Application (`src/`)

| File | Tests | What It Validates |
|------|------:|-------------------|
| `src/markdown_renderer.rs` | ~403 | Markdown parsing, element conversion, image caching, table layout, link handling, emoji decoding, search highlighting, grapheme-aware text matching, element rect tracking |
| `src/app.rs` | ~224 | App state management, keyboard shortcuts, file loading/saving, navigation history (back/forward), view mode switching, search state, async file loading, error handling, drag-and-drop queue |
| `src/mermaid_renderer.rs` | ~202 | LRU cache management, SVG rasterization, texture caching, Mermaid configuration (theme, security, font, background), diagram type detection, width bucketing, worker pool |
| `src/window_state.rs` | ~45 | Window position/size persistence, file I/O, Windows registry operations, environment variable handling, config directory detection, geometry sanitization |
| `src/pikchr_renderer.rs` | ~43 | Pikchr rendering pipeline, SVG rasterization via usvg/resvg, texture caching, dark mode flag |
| `src/table_support/column_spec.rs` | ~41 | Table column width calculation, layout policy application, responsive column sizing |
| `src/main.rs` | ~19 | Icon generation, build metadata extraction, window initialization |
| `src/image_decode.rs` | ~18 | Image format parsing (PNG, JPEG, GIF, BMP, WebP, SVG), error handling, embedded asset loading |
| `src/d2_renderer.rs` | ~11 | D2 diagram rendering, SVG generation, texture caching |
| `src/table_support/metrics.rs` | ~9 | Table metrics computation |
| `src/lru_cache.rs` | ~7 | Generic LRU cache insertion, eviction, capacity limits |
| `src/sample_files.rs` | ~11 | Sample file content presence and integrity |
| `src/emoji_catalog.rs` | ~6 | Emoji shortcode mapping, asset verification |
| `src/theme.rs` | ~6 | Theme color configuration |
| `src/emoji_assets.rs` | ~3 | Emoji asset integrity |
| `src/lib.rs` | 1 | Process priority setting (Windows `BELOW_NORMAL_PRIORITY_CLASS`) |

#### D2 Diagram Library (`crates/d2/src/`)

| File | Tests | What It Validates |
|------|------:|-------------------|
| `crates/d2/src/edge_routing.rs` | ~39 | Edge routing algorithms, orthogonal path generation, label placement, nudge vectors |
| `crates/d2/src/parser.rs` | ~31 | D2 syntax parsing: statements, containers, properties, escape sequences, edge declarations |
| `crates/d2/src/compiler.rs` | ~22 | D2 graph compilation, style attributes, arrow types, containers, directionality |
| `crates/d2/src/svg_render.rs` | ~13 | SVG generation from compiled graph structure, label dimensions |
| `crates/d2/src/layout.rs` | ~12 | Graph layout algorithms, node positioning |
| `crates/d2/src/lib.rs` | ~12 | End-to-end D2 rendering pipeline |
| `crates/d2/src/graph.rs` | ~8 | Graph data structure operations |
| `crates/d2/src/text.rs` | ~7 | Text measurement and layout |
| `crates/d2/src/shapes.rs` | ~6 | Shape rendering and bounding box calculation |
| `crates/d2/src/layout_sugiyama.rs` | ~5 | Sugiyama hierarchical layout algorithm |
| `crates/d2/src/geo.rs` | ~5 | Geometry utilities: distance, line intersection |

#### Build Script

| File | Tests | What It Validates |
|------|------:|-------------------|
| `build.rs` | 5 | Semantic version parsing, Windows version encoding (4 u16 components), author extraction, ISO 8601 timestamp formatting |

### Test Infrastructure

The unit tests use several patterns to support thorough testing without mocks:

**Thread-local test fixtures** — Instead of mocking, the codebase uses thread-local globals that tests can set to inject specific behaviors. These are automatically cleaned up by RAII guard types.

App test fixtures (`src/app.rs`):
- `FORCED_APP_ACTIONS` — Force specific UI actions to trigger
- `FORCED_OPEN_PATH` / `FORCED_SAVE_PATH` — Simulate file dialog results
- `FORCED_LOAD_ERROR` — Simulate file load failures
- `FORCED_SCAN_ERROR` / `FORCED_SCAN_ENTRY_ERROR` — Simulate directory scan errors
- `FORCED_READ_LOSSY_ERROR` — Simulate lossy read failures
- `FORCE_THREAD_SPAWN_ERROR` — Simulate async thread spawn failures

Renderer test fixtures (`src/markdown_renderer.rs`):
- `FORCED_RENDER_ACTIONS` — Inject render actions
- `FORCED_TABLE_POLICIES` — Override table layout policies
- `FORCED_PARSE_ERROR` — Inject markdown parse errors
- `FORCE_EMOJI_DECODE_ERROR` — Simulate emoji decoding errors

Environment variable guards (`src/window_state.rs`, `src/markdown_renderer.rs`):
- `EnvGuard` / `EnvVarGuard` — RAII types that save and restore env vars
- `env_lock()` — Mutex that serializes env var access across test threads

I/O error simulation:
- `FailingWriter` — Mock `Write` implementation that returns errors on demand
- `force_parse_error_once()` — Thread-local one-shot error injection

### Windows Test Priority

The test binary automatically lowers its CPU priority to `BELOW_NORMAL_PRIORITY_CLASS` on Windows via a `.CRT$XCU` initializer in `src/lib.rs`. This prevents the ~8-minute test suite from starving interactive applications, particularly over remote desktop sessions. The `test_process_priority_is_below_normal` test verifies this behavior.

Use `nice -n 10` when running `cargo test` to also lower the compilation priority.

---

## D2 Conformance Tests

**Location:** `crates/d2/tests/d2_conformance.rs`
**Fixtures:** `tests/d2_conformance/fixtures/` (24 `.d2` files)
**Output:** `tests/d2_conformance/actual/` (SVG artifacts for debugging)

These integration tests validate four geometric invariants that every D2 diagram must satisfy:

| Invariant | Description |
|-----------|-------------|
| `label_not_overlapping_node` | Edge labels must not overlap node rectangles |
| `edge_not_through_node` | Edge paths must not pass through non-endpoint nodes |
| `nodes_not_overlapping` | Node rectangles must not overlap (except container nesting) |
| `label_near_edge` | Edge labels must be within 50px of their edge path |

The conformance suite renders every `.d2` fixture file, parses the resulting SVG, extracts node rects and edge paths, then checks all four invariants. Failures dump the offending SVG for visual inspection.

**Supporting tests** in the conformance file verify the geometry utilities themselves: AABB overlap/containment, Liang-Barsky line-AABB intersection, point-to-segment distance, SVG path tokenization, and attribute extraction.

**Python tooling** in `tools/d2_conformance/` provides additional analysis:
- `d2_svg_parse.py` — Parse SVG structure
- `generate_reference.py` / `generate_actual.py` — Render fixtures
- `compare.py` — Diff reference vs. actual
- `report.py` — Generate summary report

### Running

```bash
# Run D2 conformance tests
cargo test d2_conformance

# Dump all fixture SVGs for inspection
cargo test dump_all_fixture_svgs -- --ignored
```

---

## Visual Regression Tests

**Manifest:** `tests/regression/manifest.toml`
**Cases:** `tests/regression/cases/` (26 markdown files)
**Runner:** `tools/regression/runner.py`
**CI:** `.github/workflows/visual-regression.yml`

The visual regression suite captures screenshots of mdmdview rendering specific markdown documents and compares them pixel-by-pixel against a baseline. This catches rendering regressions that unit tests cannot.

### How It Works

1. **Reference generation** — The runner renders each test case markdown through markdown-it (or another reference renderer) into HTML, then captures a Playwright/Chromium screenshot at a fixed viewport.
2. **Baseline capture** — mdmdview renders the same markdown using its `--screenshot` mode at the same viewport dimensions.
3. **Pixel diff** — The runner compares actual vs. baseline images, computing pixel differences and percentage deviation.
4. **Pass/fail** — A case passes if differences are below the configured thresholds (`max_pixels` and `max_percent`).

### Compare Modes

- **`mdmdview`** (default) — Compares mdmdview's current output against a known-good mdmdview baseline. Used for most cases.
- **`reference`** — Compares mdmdview output against the markdown-it HTML reference. Used when you want to check against a canonical renderer.

### Test Cases (26)

| ID | Title | Notes |
|----|-------|-------|
| 001 | Headings and paragraphs | |
| 002 | Lists and nesting | |
| 003 | Inline formatting | |
| 004 | Code blocks | |
| 005 | Tables and wrapping | |
| 006 | Images and missing assets | |
| 007 | Emoji coverage | |
| 008 | Mermaid diagrams | `compare_mode = "mdmdview"` (no reference renderer equivalent) |
| 009 | Unicode and RTL | |
| 010 | Large document | Two snapshots: `top` (scroll 0.0) and `mid` (scroll 0.5) |
| 011-026 | Table variants | Alignments, escaping, missing cells, wide tables, line breaks, unicode, dark mode, inline elements, lists, blockquotes |

### Configuration

Defaults in `manifest.toml`:
- Viewport: 1280x720, DPR 1.0
- Theme: light
- Zoom: 1.0
- Diff thresholds: max 500 pixels or 10% different
- Wait: 2000ms, settle 3 frames

Per-case overrides are supported for theme, viewport, compare mode, scroll position, and thresholds.

### Running

```bash
# Prerequisites
python -m pip install -r tools/regression/requirements.txt
python -m playwright install chromium
cargo build --release

# Full workflow
python tools/regression/runner.py update-reference   # Generate reference screenshots
python tools/regression/runner.py update-baseline    # Capture mdmdview baselines
python tools/regression/runner.py run                # Compare actual vs. baseline

# Single case
python tools/regression/runner.py run --case 001-headings

# With deterministic fonts
python tools/regression/runner.py run --test-fonts tools/regression/fonts
```

### CI Integration

The `visual-regression.yml` workflow runs on PRs that touch `src/`, `tests/regression/`, or `tools/regression/`. It builds a release binary and runs a smoke test against case `001-headings`, uploading diff artifacts on failure.

### Adding a New Case

1. Create a markdown file in `tests/regression/cases/`
2. Add an entry to `manifest.toml` with an ID, path, and title
3. Keep content deterministic — no network references or timestamps
4. Run `update-baseline` to capture the initial baseline

---

## Mermaid Visual Tests

**Cases:** `tests/mermaid_visual/cases/` (15 markdown files)
**Runner:** `tools/mermaid_visual_check.py`
**Output:** `tests/mermaid_visual/{actual,reference,diff,report}/`

These tests compare mdmdview's embedded Mermaid rendering against the official Mermaid CLI (`mmdc`) output.

### Test Cases (15)

Covers the major Mermaid diagram types: flowchart, flowchart with subgraphs, sequence, sequence with alt/loop, class, complex class, ER, gantt, state, nested state, gitgraph, journey, mindmap, pie, timeline.

### Prerequisites

- Built mdmdview release binary
- Python 3.11+ with Pillow
- Node.js + `npx` or `mmdc` on PATH
- `mdscreensnap` on PATH

### Running

```bash
# All cases
python tools/mermaid_visual_check.py

# Single case
python tools/mermaid_visual_check.py --case flowchart

# Adjust thresholds
python tools/mermaid_visual_check.py --threshold-percent 5.0 --threshold-pixels 1000
```

---

## CI Pipelines

### Release Workflow (`.github/workflows/release.yml`)

Triggered by version tags (`v*`) or manual dispatch. Runs the full quality gate:

1. `cargo fmt --all -- --check` — Formatting
2. `cargo clippy --all-targets -- -D warnings` — Linting (zero warnings)
3. `cargo test --all-targets` — Full test suite
4. `cargo build --release` — Release binary
5. `cargo wix --no-build` — MSI installer

All steps must pass before artifacts are published.

### Visual Regression Workflow (`.github/workflows/visual-regression.yml`)

Triggered on PRs touching rendering code. Runs a smoke test:

1. Setup Python 3.11 + Playwright Chromium
2. Build release binary
3. Run regression on `001-headings`
4. Upload diff/report artifacts

### Pre-Release Script (`scripts/prerelease.ps1`)

Local equivalent of the release CI pipeline — run before tagging:

```powershell
.\scripts\prerelease.ps1
# Runs: fmt check → clippy → release build → test
```

### Full Test Script (`scripts/full_test.ps1`)

Runs the complete test suite with optional coverage:

```powershell
.\scripts\full_test.ps1              # Tests + coverage
.\scripts\full_test.ps1 -SkipCoverage  # Tests only
```

Uses `cargo-llvm-cov` for coverage (install with `cargo install cargo-llvm-cov`).

### Full Test Runner (`full_test.exe`)

Single Rust binary that runs all quality checks, tests, Mermaid visual comparisons, and coverage — then generates a self-contained HTML report.

```bash
# Full run (all phases)
cargo run --release --bin full_test

# Skip coverage (much faster)
cargo run --release --bin full_test -- --skip-coverage

# Quick run, no Mermaid visual tests
cargo run --release --bin full_test -- --quick --skip-mermaid

# All options
cargo run --release --bin full_test -- --help
```

**Phases** (run sequentially, fast-to-slow):

1. **Quality checks** — `cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`
2. **Unit + D2 tests** — `cargo test --release --lib --tests --workspace --no-fail-fast -- --test-threads=1`
   - Uses `--lib --tests` (not `--all-targets`) to avoid relinking the running binary on Windows
3. **Mermaid visual tests** — builds mdmdview, screenshots each of 15 Mermaid test cases, pixel-diffs against reference images
   - Thresholds: <12% diff and <45,000 pixels (per-channel tolerance: 60)
   - Reference images in `tests/mermaid_visual/reference/`
   - Actual output in `tests/mermaid_visual/actual_full_test/`
4. **Coverage** — `cargo llvm-cov --workspace --branch --json --no-cfg-coverage` (gracefully skipped if `cargo-llvm-cov` not installed)

**CLI flags:**

| Flag | Description |
|------|-------------|
| `--skip-quality` | Skip fmt/clippy checks |
| `--skip-coverage` | Skip coverage analysis |
| `--skip-mermaid` | Skip Mermaid visual tests |
| `--quick` | Exclude ignored tests |
| `--help` | Show usage |

**Output:** Self-contained HTML report saved to `tests/results/{YYYY.MM.DD} - full test report.html` with auto-increment on date collision. Features dark mode support, dashboard cards, collapsible test breakdown, and Mermaid visual results table.

**Exit code:** 1 if any phase has hard failures, 0 otherwise.

---

## Testing Philosophy

### No Mocks

Tests validate real logic. Instead of mock objects, the codebase uses thread-local injection points to control specific behaviors (file dialog results, I/O errors, thread spawn failures) while keeping all other code paths real.

### Test Oracles

Each test category uses a different oracle — a definition of "correct" that can be checked mechanically:

| Category | Oracle |
|----------|--------|
| Unit tests | Explicit assertions on state, output, and invariants |
| D2 conformance | Four geometric invariants (no overlaps, labels near edges, paths don't cross nodes) |
| Visual regression | Pixel-diff against known-good baselines within tolerance thresholds |
| Mermaid visual | Pixel-diff against official Mermaid CLI output |

### CPU Priority

The full test suite takes ~8 minutes and is CPU-intensive. Two mechanisms prevent it from starving the system:

1. **Automatic priority lowering** — The test binary sets `BELOW_NORMAL_PRIORITY_CLASS` on Windows via a CRT initializer
2. **Manual nice** — Use `nice -n 10 mdtimeout 600 cargo test` to also lower compilation priority

### What Is Not Tested

- **No property-based testing** — All tests are example-based (no quickcheck/proptest)
- **No fuzz testing** — No fuzzing harnesses for the markdown parser
- **No benchmarks** — Performance validation is manual
- **No end-to-end UI automation** — The egui app loop is not driven by automated UI tests; visual regression covers rendering accuracy instead
