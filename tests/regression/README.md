# Regression Visual Tests

This folder contains the visual regression corpus, manifest, and supporting assets.

## Decisions (Stage 0)
- Reference renderer: markdown-it (default) or cmark-gfm if installed -> HTML + Playwright Chromium screenshot (pinned Python/Playwright versions).
- Canonical CSS: `tools/regression/reference.css` approximating mdmdview defaults.
- Fonts: optional font bundle via `tools/regression/fonts`; pass `--test-fonts` to load in both renderers.
- CI target: Windows runner, fixed viewport, fixed DPI.
- Compare modes: `mdmdview` (baseline default) and `reference` (canonical, opt-in per case).

## Layout
```
/tests/regression/
  cases/         Markdown test cases
  assets/        Local assets for test cases (optional)
  manifest.toml  Case metadata and thresholds
  reference/     Canonical outputs (generated; can be checked in)
  baseline/      mdmdview baselines (generated for compare_mode = mdmdview)
  actual/        mdmdview captures for diffs (generated)
  diff/          Diff artifacts (generated)
  report/        Summary reports (generated)
```

## Adding a Case
1. Add a markdown file under `cases/`.
2. Update `manifest.toml` with case metadata and thresholds.
3. Keep content deterministic and avoid network references.

## Notes
- The default `compare_mode` is `mdmdview` (baselines generated from mdmdview). Set `compare_mode = "reference"` per case to compare against canonical HTML output.
- Some cases may use `compare_mode = "mdmdview"` when a reference renderer cannot faithfully reproduce output (e.g., Mermaid without a pinned renderer).
- Large cases should be kept to the minimum size needed to catch regressions.
- `diff.mask` can point at a PNG mask (black/transparent pixels are ignored).
- `actual/`, `diff/`, and `report/` are generated artifacts and are ignored by git.
- Screenshot mode hides UI chrome; `content_only` only controls cropping.

## Tolerances and Masks
- Use `diff.max_percent` and `diff.max_pixels` to loosen comparisons only when needed; start strict and increase gradually.
- Prefer `max_pixels` for small, localized variance (e.g., hint text) and `max_percent` for wide but light variance (e.g., antialiasing).
- Masks are PNGs sized to the viewport; black or transparent pixels are ignored, white/opaque pixels are compared.
- Store masks under `tests/regression/masks/` and reference them from `manifest.toml`.

## Troubleshooting
- **100% diff:** check viewport size and DPI; ensure mdmdview runs with `--width/--height` and rebuild baselines.
- **Font variance:** run with `--test-fonts tools/regression/fonts` and bundle known fonts.
- **Missing images:** verify `assets/` paths and that `base` href resolves correctly.
- **Playwright errors:** re-run `python -m playwright install chromium`.
- **Flaky layouts:** increase `wait_ms` or `settle_frames` for the affected case.

## Runner
Use the regression runner to generate reference outputs, capture mdmdview screenshots, and diff results:
```
python tools/regression/runner.py update-reference
python tools/regression/runner.py update-baseline
python tools/regression/runner.py run
```

`update-reference` generates canonical outputs for all cases, even when they compare against mdmdview baselines.

Optional: load deterministic fonts for both renderers:
```
python tools/regression/runner.py run --test-fonts tools/regression/fonts
```
