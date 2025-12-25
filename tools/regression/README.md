# Regression Visual Tests

This folder contains the reference renderer adapter and runner for visual regression tests.

## Requirements
- Python 3.11+ (Playwright 1.57 includes cp313 wheels)
- Playwright (see `requirements.txt`)
- A reference markdown renderer (default: `markdown-it-py`) or `cmark-gfm`
- Optional font bundle under `tools/regression/fonts`

Install Python dependencies:
```
pip install -r tools/regression/requirements.txt
python -m playwright install chromium
```

## Commands
- Build mdmdview (recommended):
```
cargo build --release
```

- Update reference renders:
```
python tools/regression/runner.py update-reference
```

- Capture mdmdview baselines (for compare_mode = mdmdview cases):
```
python tools/regression/runner.py update-baseline
```

- Run diffs:
```
python tools/regression/runner.py run
```

## Notes
- Use `--case <id>` to target a single case.
- Use `--reference markdown-it` to force the markdown-it renderer.
- Use `--mdmdview <path>` to point at a custom mdmdview binary.
- Use `--test-fonts <dir>` to load deterministic fonts in mdmdview and the reference renderer.
- `diff.mask` in `tests/regression/manifest.toml` supports PNG masks (black/transparent pixels ignored).
- `update-reference` always produces canonical outputs; `run` compares against the case `compare_mode`.
- The default `compare_mode` is `mdmdview`, so `update-baseline` should be run after rebuilding mdmdview.
