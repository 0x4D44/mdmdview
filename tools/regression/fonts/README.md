# Regression Fonts

This folder is optional. Drop `.ttf` or `.otf` files here to make visual
regression runs deterministic.

Naming rules:
- Filenames containing `mono` or `code` are treated as monospace.
- All other filenames are treated as proportional.

Usage:
```
python tools/regression/runner.py run --test-fonts tools/regression/fonts
```

No fonts are shipped by default; add your own open-source fonts if needed.
