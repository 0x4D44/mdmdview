import argparse
import hashlib
import json
import subprocess
import sys
from pathlib import Path


def read_css(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def sanitize_font_name(name: str) -> str:
    cleaned = []
    for ch in name:
        if ch.isascii() and ch.isalnum():
            cleaned.append(ch)
        else:
            cleaned.append("-")
    value = "".join(cleaned).strip("-")
    return value or "font"


def build_font_css(font_dir: Path) -> tuple[str, dict]:
    font_paths = sorted(
        [
            path
            for path in font_dir.iterdir()
            if path.suffix.lower() in {".ttf", ".otf"}
        ],
        key=lambda p: str(p).lower(),
    )
    if not font_paths:
        raise RuntimeError(f"No font files found in {font_dir}")

    prop_names = []
    mono_names = []
    font_files = []
    face_rules = []

    for path in font_paths:
        stem = sanitize_font_name(path.stem)
        name = f"test-{stem}"
        uri = path.resolve().as_uri()
        face_rules.append(
            f"@font-face {{ font-family: \"{name}\"; src: url(\"{uri}\"); "
            "font-weight: normal; font-style: normal; }}"
        )
        font_files.append(path.name)
        lower = name.lower()
        if "mono" in lower or "code" in lower:
            mono_names.append(name)
        else:
            prop_names.append(name)

    if not prop_names:
        prop_names = mono_names.copy()
    if not mono_names:
        mono_names = prop_names.copy()

    body_stack = [f"\"{name}\"" for name in prop_names] + [
        "\"Segoe UI\"",
        "\"Noto Sans\"",
        "\"Helvetica Neue\"",
        "Arial",
        "sans-serif",
    ]
    code_stack = [f"\"{name}\"" for name in mono_names] + [
        "\"Consolas\"",
        "\"Courier New\"",
        "monospace",
    ]

    css = (
        "\n".join(face_rules)
        + "\n"
        + f"body {{ font-family: {', '.join(body_stack)}; }}\n"
        + f"code, pre {{ font-family: {', '.join(code_stack)}; }}\n"
    )

    metadata = {
        "font_dir": str(font_dir.resolve()),
        "font_files": font_files,
        "font_stack": {"proportional": prop_names, "monospace": mono_names},
    }
    return css, metadata


def render_markdown_cmark(text: str) -> str:
    try:
        proc = subprocess.run(
            ["cmark-gfm", "--unsafe"],
            input=text.encode("utf-8"),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
    except FileNotFoundError as exc:
        raise RuntimeError("cmark-gfm not found in PATH") from exc
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr.decode("utf-8", errors="replace").strip())
    return proc.stdout.decode("utf-8", errors="replace")


def render_markdown_markdown_it(text: str) -> str:
    try:
        from markdown_it import MarkdownIt
    except ImportError as exc:
        raise RuntimeError("markdown-it-py not installed") from exc
    md = MarkdownIt("commonmark").enable("strikethrough").enable("table")
    return md.render(text)


def render_markdown(text: str, renderer: str) -> str:
    if renderer == "cmark-gfm":
        return render_markdown_cmark(text)
    if renderer == "markdown-it":
        return render_markdown_markdown_it(text)
    raise RuntimeError(f"Unsupported renderer: {renderer}")


def renderer_version(renderer: str) -> str:
    if renderer == "cmark-gfm":
        try:
            proc = subprocess.run(
                ["cmark-gfm", "--version"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                check=False,
            )
        except FileNotFoundError:
            return "unknown"
        if proc.returncode != 0:
            return "unknown"
        return proc.stdout.decode("utf-8", errors="replace").strip()
    if renderer == "markdown-it":
        try:
            import markdown_it

            return getattr(markdown_it, "__version__", "markdown-it-py")
        except Exception:
            return "markdown-it-py"
    return "unknown"


def build_html(markdown_html: str, css: str, theme: str, base_href: str) -> str:
    return (
        "<!doctype html>\n"
        "<html>\n"
        "<head>\n"
        "  <meta charset=\"utf-8\">\n"
        f"  <base href=\"{base_href}\">\n"
        "  <style>\n"
        f"{css}\n"
        "  </style>\n"
        "</head>\n"
        f"<body data-theme=\"{theme}\">\n"
        "  <article class=\"markdown-body\">\n"
        f"{markdown_html}\n"
        "  </article>\n"
        "</body>\n"
        "</html>\n"
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True)
    parser.add_argument("--output-dir", required=True)
    parser.add_argument("--width", type=int, required=True)
    parser.add_argument("--height", type=int, required=True)
    parser.add_argument("--dpr", type=float, default=1.0)
    parser.add_argument("--theme", choices=["light", "dark"], default="light")
    parser.add_argument("--css", required=True)
    parser.add_argument("--renderer", default="cmark-gfm")
    parser.add_argument("--fonts", help="Font directory for deterministic rendering")
    args = parser.parse_args()

    md_path = Path(args.input)
    out_dir = Path(args.output_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    css_path = Path(args.css)
    css = read_css(css_path)
    font_metadata = None
    if args.fonts:
        font_css, font_metadata = build_font_css(Path(args.fonts))
        css = f"{css}\n{font_css}"
    css_hash = hashlib.sha256(css.encode("utf-8")).hexdigest()

    md_text = md_path.read_text(encoding="utf-8")
    markdown_html = render_markdown(md_text, args.renderer)

    base_href = md_path.parent.resolve().as_uri() + "/"
    html = build_html(markdown_html, css, args.theme, base_href)

    html_path = out_dir / "reference.html"
    html_path.write_text(html, encoding="utf-8")

    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("Playwright not installed. Run: pip install -r tools/regression/requirements.txt", file=sys.stderr)
        return 2

    png_path = out_dir / "reference.png"
    playwright_version = "unknown"
    try:
        import playwright

        playwright_version = getattr(playwright, "__version__", "unknown")
    except Exception:
        playwright_version = "unknown"

    with sync_playwright() as p:
        browser = p.chromium.launch()
        browser_version = browser.version
        page = browser.new_page(
            viewport={"width": args.width, "height": args.height},
            device_scale_factor=args.dpr,
        )
        page.set_content(html, wait_until="load")
        page.screenshot(path=str(png_path), full_page=False)
        browser.close()

    metadata = {
        "renderer": args.renderer,
        "renderer_version": renderer_version(args.renderer),
        "playwright_version": playwright_version,
        "browser_version": browser_version,
        "css_hash": css_hash,
        "viewport": {"width": args.width, "height": args.height, "dpr": args.dpr},
        "theme": args.theme,
        "input": str(md_path),
    }
    if font_metadata:
        metadata["font_source"] = font_metadata["font_dir"]
        metadata["font_files"] = font_metadata["font_files"]
        metadata["font_stack"] = font_metadata["font_stack"]
    (out_dir / "reference.json").write_text(
        json.dumps(metadata, indent=2), encoding="utf-8"
    )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
