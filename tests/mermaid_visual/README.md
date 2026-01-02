# Mermaid Visual Tests

This suite compares mdmdview's Mermaid renderer output against Mermaid CLI (mmdc)
references. It captures mdmdview with mdscreensnap and diffs images with Pillow.

## Prereqs

- Build mdmdview (`cargo build --release`)
- Python 3.11+ with Pillow (`pip install -r tools/regression/requirements.txt`)
- Node + npx or `mmdc` on PATH (reference renderer)
- mdscreensnap on PATH (`c:\apps`)

## Run

```bash
python tools/mermaid_visual_check.py
```

Run a single case:

```bash
python tools/mermaid_visual_check.py --case flowchart
```

## Outputs

- `tests/mermaid_visual/actual`: mdmdview captures
- `tests/mermaid_visual/reference`: Mermaid CLI renders
- `tests/mermaid_visual/diff`: visual diffs
- `tests/mermaid_visual/report`: summary report (`report.md`, `report.json`)

## Notes

- Adjust thresholds with `--threshold-percent`, `--threshold-pixels`, and
  `--pixel-tolerance` if needed.
