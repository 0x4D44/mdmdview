import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Optional, Tuple

try:
    from PIL import Image, ImageChops
except ImportError as exc:  # pragma: no cover
    raise SystemExit(
        "Pillow is required. Install with: pip install -r tools/regression/requirements.txt"
    ) from exc


MERMAID_CLI_VERSION = "10.9.1"
MERMAID_BG = "#FFF8DB"
MERMAID_CONFIG = {
    "startOnLoad": False,
    "securityLevel": "strict",
    "htmlLabels": False,
    "flowchart": {"htmlLabels": False},
    "deterministicIds": True,
    "deterministicIDSeed": "mdmdview",
    "maxTextSize": 50000,
    "theme": "base",
    "themeVariables": {
        "background": MERMAID_BG,
        "mainBkg": MERMAID_BG,
        "textColor": "#1C2430",
        "titleColor": "#1C2430",
        "primaryColor": "#D7EEFF",
        "primaryBorderColor": "#9BB2C8",
        "primaryTextColor": "#1C2430",
        "secondaryColor": "#DFF5E1",
        "tertiaryColor": "#E9E2FF",
        "lineColor": "#6B7A90",
        "defaultLinkColor": "#6B7A90",
        "clusterBkg": "#FFF1C1",
        "clusterBorder": "#E5C07B",
        "labelBackground": MERMAID_BG,
        "edgeLabelBackground": MERMAID_BG,
    },
}


@dataclass
class DiffResult:
    ok: bool
    message: str
    diff_pixels: int
    percent: float
    max_delta: int
    size_ref: Tuple[int, int]
    size_actual: Tuple[int, int]


def find_mermaid_blocks(text: str) -> list[str]:
    import re

    pattern = re.compile(r"```mermaid\s*(.*?)```", re.DOTALL)
    return [m.group(1).strip() for m in pattern.finditer(text)]


def resolve_mdmdview_bin(root: Path, override: Optional[str]) -> str:
    if override:
        return override
    candidates = [
        root / "target" / "release" / "mdmdview.exe",
        root / "target" / "debug" / "mdmdview.exe",
        root / "target" / "release" / "mdmdview",
        root / "target" / "debug" / "mdmdview",
    ]
    for candidate in candidates:
        if candidate.exists():
            return str(candidate)
    fallback = shutil.which("mdmdview")
    if fallback:
        return fallback
    raise RuntimeError("mdmdview binary not found; build it or pass --mdmdview")


def resolve_mmdc(override: Optional[str]) -> Optional[str]:
    if override:
        return override
    found = shutil.which("mmdc")
    return found


def iter_cases(cases_dir: Path, filters: Iterable[str]) -> list[Path]:
    paths = sorted(cases_dir.glob("*.md"))
    if not filters:
        return paths
    lookup = {item.lower() for item in filters}
    return [path for path in paths if path.stem.lower() in lookup]


def write_config(config_path: Path) -> None:
    config_path.parent.mkdir(parents=True, exist_ok=True)
    config_path.write_text(json.dumps(MERMAID_CONFIG, indent=2), encoding="utf-8")


def run_mdmdview_screenshot(
    mdmdview_bin: str,
    md_path: Path,
    output_png: Path,
    width: int,
    height: int,
    theme: str,
    zoom: float,
    wait_ms: int,
    settle_frames: int,
    content_only: bool,
    test_fonts: Optional[str],
    env: dict,
) -> None:
    cmd = [
        mdmdview_bin,
        "--screenshot",
        str(md_path),
        "--output",
        str(output_png),
        "--width",
        str(width),
        "--height",
        str(height),
        "--theme",
        theme,
        "--zoom",
        f"{zoom:.3f}",
        "--wait-ms",
        str(wait_ms),
        "--settle-frames",
        str(settle_frames),
    ]
    if content_only:
        cmd.append("--content-only")
    if test_fonts:
        cmd.extend(["--test-fonts", test_fonts])
    subprocess.run(cmd, check=True, env=env)


def run_mermaid_cli(
    code: str,
    output_png: Path,
    width: int,
    height: int,
    bg: str,
    config_path: Path,
    mmdc_cmd: Optional[str],
    npx_cmd: str,
) -> None:
    scratch = output_png.parent / "scratch"
    scratch.mkdir(parents=True, exist_ok=True)
    mmd_path = scratch / f"{output_png.stem}.mmd"
    mmd_path.write_text(code, encoding="utf-8")

    if mmdc_cmd:
        cmd = [mmdc_cmd]
    else:
        npx_path = shutil.which(npx_cmd) or npx_cmd
        if not shutil.which(npx_cmd) and npx_path == npx_cmd:
            raise RuntimeError(f"npx not found (tried '{npx_cmd}')")
        if os.name == "nt" and npx_path.lower().endswith(".ps1"):
            cmd = [
                "powershell",
                "-NoProfile",
                "-ExecutionPolicy",
                "Bypass",
                "-File",
                npx_path,
            ]
        else:
            cmd = [npx_path]
        cmd += ["-y", f"@mermaid-js/mermaid-cli@{MERMAID_CLI_VERSION}"]

    cmd += [
        "-i",
        str(mmd_path),
        "-o",
        str(output_png),
        "-w",
        str(width),
        "-H",
        str(height),
        "-b",
        bg,
        "-t",
        "default",
        "-c",
        str(config_path),
        "-q",
    ]

    subprocess.run(cmd, check=True)


def parse_hex_color(value: str) -> Tuple[int, int, int]:
    raw = value.strip().lstrip("#")
    if len(raw) != 6:
        raise ValueError(f"Expected 6-char hex color, got {value}")
    return int(raw[0:2], 16), int(raw[2:4], 16), int(raw[4:6], 16)


def guess_background_color(img: Image.Image) -> Tuple[int, int, int]:
    width, height = img.size
    pixels = img.convert("RGB").load()
    counts: dict[Tuple[int, int, int], int] = {}

    for x in range(width):
        for y in (0, height - 1):
            color = pixels[x, y]
            counts[color] = counts.get(color, 0) + 1

    for y in range(height):
        for x in (0, width - 1):
            color = pixels[x, y]
            counts[color] = counts.get(color, 0) + 1

    if not counts:
        return 255, 255, 255
    return max(counts.items(), key=lambda item: item[1])[0]


def normalize_background(
    img: Image.Image,
    source_rgb: Tuple[int, int, int],
    target_rgb: Tuple[int, int, int],
    tolerance: int,
) -> Image.Image:
    rgba = img.convert("RGBA")
    data = list(rgba.getdata())
    out = []
    for r, g, b, a in data:
        if (
            abs(r - source_rgb[0]) <= tolerance
            and abs(g - source_rgb[1]) <= tolerance
            and abs(b - source_rgb[2]) <= tolerance
        ):
            out.append((target_rgb[0], target_rgb[1], target_rgb[2], a))
        else:
            out.append((r, g, b, a))
    rgba.putdata(out)
    return rgba


def crop_to_content(img: Image.Image, bg_rgb: Tuple[int, int, int], tolerance: int) -> Image.Image:
    width, height = img.size
    min_x = width
    min_y = height
    max_x = 0
    max_y = 0
    found = False

    pixels = img.convert("RGB").load()
    for y in range(height):
        for x in range(width):
            r, g, b = pixels[x, y]
            if not (
                abs(r - bg_rgb[0]) <= tolerance
                and abs(g - bg_rgb[1]) <= tolerance
                and abs(b - bg_rgb[2]) <= tolerance
            ):
                found = True
                min_x = min(min_x, x)
                min_y = min(min_y, y)
                max_x = max(max_x, x)
                max_y = max(max_y, y)

    if not found:
        return img

    pad = 2
    min_x = max(0, min_x - pad)
    min_y = max(0, min_y - pad)
    max_x = min(width - 1, max_x + pad)
    max_y = min(height - 1, max_y + pad)
    return img.crop((min_x, min_y, max_x + 1, max_y + 1))


def pad_to_common_size(
    reference: Image.Image,
    actual: Image.Image,
    bg_rgb: Tuple[int, int, int],
) -> tuple[Image.Image, Image.Image]:
    if reference.size == actual.size:
        return reference, actual
    target_w = max(reference.size[0], actual.size[0])
    target_h = max(reference.size[1], actual.size[1])
    bg_rgba = (bg_rgb[0], bg_rgb[1], bg_rgb[2], 255)

    def pad(img: Image.Image) -> Image.Image:
        canvas = Image.new("RGBA", (target_w, target_h), bg_rgba)
        offset_x = (target_w - img.size[0]) // 2
        offset_y = (target_h - img.size[1]) // 2
        canvas.paste(img.convert("RGBA"), (offset_x, offset_y))
        return canvas

    return pad(reference), pad(actual)


def align_to_content(
    reference: Image.Image,
    actual: Image.Image,
    bg_rgb: Tuple[int, int, int],
    tolerance: int,
) -> Image.Image:
    def bounds(img: Image.Image) -> Optional[Tuple[int, int, int, int]]:
        w, h = img.size
        min_x = w
        min_y = h
        max_x = 0
        max_y = 0
        found = False
        pixels = img.convert("RGB").load()
        for y in range(h):
            for x in range(w):
                r, g, b = pixels[x, y]
                if not (
                    abs(r - bg_rgb[0]) <= tolerance
                    and abs(g - bg_rgb[1]) <= tolerance
                    and abs(b - bg_rgb[2]) <= tolerance
                ):
                    found = True
                    min_x = min(min_x, x)
                    min_y = min(min_y, y)
                    max_x = max(max_x, x)
                    max_y = max(max_y, y)
        if not found:
            return None
        return min_x, min_y, max_x, max_y

    ref_bounds = bounds(reference)
    act_bounds = bounds(actual)
    if not ref_bounds or not act_bounds:
        return actual
    ref_cx = (ref_bounds[0] + ref_bounds[2]) / 2
    ref_cy = (ref_bounds[1] + ref_bounds[3]) / 2
    act_cx = (act_bounds[0] + act_bounds[2]) / 2
    act_cy = (act_bounds[1] + act_bounds[3]) / 2
    dx = int(round(ref_cx - act_cx))
    dy = int(round(ref_cy - act_cy))
    if dx == 0 and dy == 0:
        return actual
    bg_rgba = (bg_rgb[0], bg_rgb[1], bg_rgb[2], 255)
    canvas = Image.new("RGBA", reference.size, bg_rgba)
    canvas.paste(actual.convert("RGBA"), (dx, dy))
    return canvas


def non_bg_ratio(
    img: Image.Image,
    bg_rgb: Tuple[int, int, int],
    tolerance: int,
) -> float:
    rgb = img.convert("RGB")
    width, height = rgb.size
    if width == 0 or height == 0:
        return 0.0
    pixels = rgb.load()
    non = 0
    total = width * height
    for y in range(height):
        for x in range(width):
            r, g, b = pixels[x, y]
            if not (
                abs(r - bg_rgb[0]) <= tolerance
                and abs(g - bg_rgb[1]) <= tolerance
                and abs(b - bg_rgb[2]) <= tolerance
            ):
                non += 1
    return non / total


def diff_images(
    reference_png: Path,
    actual_png: Path,
    diff_png: Path,
    threshold_percent: float,
    threshold_pixels: int,
    pixel_tolerance: int,
) -> DiffResult:
    ref = Image.open(reference_png).convert("RGBA")
    act = Image.open(actual_png).convert("RGBA")
    diff = ImageChops.difference(ref, act)
    diff_data = list(diff.getdata())
    diff_pixels = 0
    max_delta = 0
    for pixel in diff_data:
        delta = max(pixel)
        if delta > pixel_tolerance:
            diff_pixels += 1
            if delta > max_delta:
                max_delta = delta

    total_pixels = ref.size[0] * ref.size[1]
    percent = (diff_pixels / total_pixels) * 100 if total_pixels else 0.0
    diff_png.parent.mkdir(parents=True, exist_ok=True)
    diff.save(diff_png)
    ok = diff_pixels <= threshold_pixels and percent <= threshold_percent
    return DiffResult(
        ok=ok,
        message="ok" if ok else "diff_exceeds_threshold",
        diff_pixels=diff_pixels,
        percent=percent,
        max_delta=max_delta,
        size_ref=ref.size,
        size_actual=act.size,
    )


def is_solid_image(img: Image.Image) -> bool:
    colors = img.getcolors(maxcolors=2)
    return colors is not None and len(colors) == 1


def read_mdmdview_meta(output_png: Path) -> Optional[dict]:
    meta_path = output_png.with_suffix(".json")
    if not meta_path.exists():
        return None
    try:
        return json.loads(meta_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None


def build_env() -> dict:
    env = dict(os.environ)
    env["MDMDVIEW_MERMAID_RENDERER"] = "embedded"
    env["MDMDVIEW_MERMAID_BG_COLOR"] = MERMAID_BG
    env["MDMDVIEW_MERMAID_THEME"] = "base"
    env["MDMDVIEW_MERMAID_MAIN_BKG"] = MERMAID_BG
    env["MDMDVIEW_MERMAID_PRIMARY_COLOR"] = "#D7EEFF"
    env["MDMDVIEW_MERMAID_PRIMARY_BORDER"] = "#9BB2C8"
    env["MDMDVIEW_MERMAID_PRIMARY_TEXT"] = "#1C2430"
    env["MDMDVIEW_MERMAID_SECONDARY_COLOR"] = "#DFF5E1"
    env["MDMDVIEW_MERMAID_TERTIARY_COLOR"] = "#E9E2FF"
    env["MDMDVIEW_MERMAID_LINE_COLOR"] = "#6B7A90"
    env["MDMDVIEW_MERMAID_TEXT_COLOR"] = "#1C2430"
    env["MDMDVIEW_MERMAID_CLUSTER_BKG"] = "#FFF1C1"
    env["MDMDVIEW_MERMAID_CLUSTER_BORDER"] = "#E5C07B"
    env["MDMDVIEW_MERMAID_LINK_COLOR"] = "#6B7A90"
    env["MDMDVIEW_MERMAID_TITLE_COLOR"] = "#1C2430"
    env["MDMDVIEW_MERMAID_LABEL_BG"] = MERMAID_BG
    env["MDMDVIEW_MERMAID_EDGE_LABEL_BG"] = MERMAID_BG
    return env


def make_index(output_dir: Path, results: list[dict]) -> None:
    lines = [
        "<!doctype html>",
        "<html>",
        "<head>",
        "<meta charset=\"utf-8\">",
        "<title>Mermaid Review Pack</title>",
        "<style>",
        "body{font-family:Segoe UI,Arial,sans-serif;margin:20px;background:#f8f8f8;color:#111}",
        ".case{margin:20px 0;padding:16px;background:#fff;border:1px solid #ddd}",
        ".row{display:flex;gap:12px;flex-wrap:wrap}",
        "figure{margin:0}",
        "img{max-width:520px;height:auto;border:1px solid #ccc;background:#fff}",
        "figcaption{font-size:12px;color:#555}",
        "code{background:#f0f0f0;padding:2px 4px}",
        "</style>",
        "</head>",
        "<body>",
        "<h1>Mermaid Review Pack</h1>",
        f"<p>Cases: {len(results)}</p>",
    ]
    for result in results:
        case_id = result["case"]
        case_dir = f"./{case_id}"
        meta = result.get("meta") or {}
        lines.append("<div class=\"case\">")
        lines.append(f"<h2>{case_id}</h2>")
        lines.append("<div class=\"row\">")
        lines.append(
            f"<figure><a href=\"{case_dir}/mdmdview.png\">"
            f"<img src=\"{case_dir}/mdmdview-crop.png\" alt=\"mdmdview\">"
            "</a><figcaption>mdmdview (crop)</figcaption></figure>"
        )
        lines.append(
            f"<figure><a href=\"{case_dir}/reference.png\">"
            f"<img src=\"{case_dir}/reference-crop.png\" alt=\"reference\">"
            "</a><figcaption>reference (crop)</figcaption></figure>"
        )
        lines.append(
            f"<figure><img src=\"{case_dir}/diff.png\" alt=\"diff\">"
            "<figcaption>diff</figcaption></figure>"
        )
        lines.append("</div>")
        lines.append(
            "<p>"
            f"diff: {result.get('percent', 0):.3f}% "
            f"({result.get('diff_pixels', 0)} px), "
            f"md_non_bg={result.get('md_non_bg', 0):.3%}, "
            f"ref_non_bg={result.get('ref_non_bg', 0):.3%}, "
            f"timed_out={meta.get('timed_out')}, "
            f"pending_renders={meta.get('pending_renders')}"
            "</p>"
        )
        lines.append("</div>")
    lines.append("</body></html>")
    (output_dir / "index.html").write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--cases", default="tests/mermaid_visual/cases")
    parser.add_argument("--output", help="Output directory for the review pack")
    parser.add_argument("--limit", type=int, default=50)
    parser.add_argument("--mdmdview", help="Path to mdmdview binary")
    parser.add_argument("--mmdc", help="Path to mermaid-cli (mmdc)")
    parser.add_argument("--npx", default="npx", help="npx command to use")
    parser.add_argument("--theme", default="light")
    parser.add_argument("--zoom", type=float, default=1.0)
    parser.add_argument("--wait-ms", type=int, default=12000)
    parser.add_argument("--settle-frames", type=int, default=3)
    parser.add_argument("--width", type=int, default=1600)
    parser.add_argument("--height", type=int, default=1000)
    parser.add_argument("--threshold-percent", type=float, default=12.0)
    parser.add_argument("--threshold-pixels", type=int, default=45000)
    parser.add_argument("--pixel-tolerance", type=int, default=40)
    parser.add_argument("--test-fonts", help="Font directory for mdmdview")
    parser.add_argument("--case", action="append", default=[])
    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parents[1]
    mdmdview_bin = resolve_mdmdview_bin(repo_root, args.mdmdview)
    mmdc_cmd = resolve_mmdc(args.mmdc)
    cases_dir = Path(args.cases).resolve()

    stamp = time.strftime("%Y.%m.%d")
    output_root = (
        Path(args.output).resolve()
        if args.output
        else repo_root / "tests" / "mermaid_visual" / "review_pack" / stamp
    )
    output_root.mkdir(parents=True, exist_ok=True)
    config_path = output_root / "mermaid-config.json"
    write_config(config_path)
    bg_rgb = parse_hex_color(MERMAID_BG)

    cases = iter_cases(cases_dir, args.case)
    cases = cases[: max(args.limit, 0)]
    results = []

    for md_path in cases:
        case_id = md_path.stem
        case_dir = output_root / case_id
        case_dir.mkdir(parents=True, exist_ok=True)

        text = md_path.read_text(encoding="utf-8").lstrip("\ufeff")
        blocks = find_mermaid_blocks(text)
        if len(blocks) != 1:
            results.append(
                {
                    "case": case_id,
                    "ok": False,
                    "message": f"expected 1 mermaid block, found {len(blocks)}",
                }
            )
            continue

        code = blocks[0]
        render_md_path = case_dir / f"{case_id}_render.md"
        render_md_path.write_text(f"```mermaid\n{code}\n```\n", encoding="utf-8")

        mdmdview_png = case_dir / "mdmdview.png"
        reference_png = case_dir / "reference.png"
        diff_png = case_dir / "diff.png"

        env = build_env()
        run_mdmdview_screenshot(
            mdmdview_bin=mdmdview_bin,
            md_path=render_md_path,
            output_png=mdmdview_png,
            width=args.width,
            height=args.height,
            theme=args.theme,
            zoom=args.zoom,
            wait_ms=args.wait_ms,
            settle_frames=args.settle_frames,
            content_only=True,
            test_fonts=args.test_fonts,
            env=env,
        )
        meta = read_mdmdview_meta(mdmdview_png)

        run_mermaid_cli(
            code=code,
            output_png=reference_png,
            width=args.width,
            height=args.height,
            bg=MERMAID_BG,
            config_path=config_path,
            mmdc_cmd=mmdc_cmd,
            npx_cmd=args.npx,
        )

        mdmdview_img = Image.open(mdmdview_png)
        reference_img = Image.open(reference_png)
        if is_solid_image(mdmdview_img) or is_solid_image(reference_img):
            results.append(
                {
                    "case": case_id,
                    "ok": False,
                    "message": "solid_image",
                    "meta": meta,
                }
            )
            continue

        actual_bg = guess_background_color(mdmdview_img)
        ref_bg = guess_background_color(reference_img)
        md_non_bg = non_bg_ratio(mdmdview_img, actual_bg, tolerance=10)
        ref_non_bg = non_bg_ratio(reference_img, ref_bg, tolerance=10)
        sparse_render = ref_non_bg > 0.02 and md_non_bg < max(0.003, ref_non_bg * 0.2)

        actual_crop = crop_to_content(mdmdview_img, actual_bg, tolerance=10)
        reference_crop = crop_to_content(reference_img, bg_rgb, tolerance=10)
        reference_crop, actual_crop = pad_to_common_size(reference_crop, actual_crop, bg_rgb)
        if actual_bg != bg_rgb:
            actual_crop = normalize_background(actual_crop, actual_bg, bg_rgb, tolerance=12)
        actual_crop = align_to_content(reference_crop, actual_crop, bg_rgb, tolerance=10)
        actual_crop.save(case_dir / "mdmdview-crop.png")
        reference_crop.save(case_dir / "reference-crop.png")

        diff_result = diff_images(
            reference_png=case_dir / "reference-crop.png",
            actual_png=case_dir / "mdmdview-crop.png",
            diff_png=diff_png,
            threshold_percent=args.threshold_percent,
            threshold_pixels=args.threshold_pixels,
            pixel_tolerance=args.pixel_tolerance,
        )

        results.append(
            {
                "case": case_id,
                "ok": diff_result.ok and not sparse_render,
                "message": "sparse_render" if sparse_render else diff_result.message,
                "diff_pixels": diff_result.diff_pixels,
                "percent": diff_result.percent,
                "max_delta": diff_result.max_delta,
                "size_ref": diff_result.size_ref,
                "size_actual": diff_result.size_actual,
                "meta": meta,
                "md_non_bg": md_non_bg,
                "ref_non_bg": ref_non_bg,
            }
        )

    (output_root / "report.json").write_text(
        json.dumps(results, indent=2), encoding="utf-8"
    )
    make_index(output_root, results)

    failures = [r for r in results if not r.get("ok")]
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
