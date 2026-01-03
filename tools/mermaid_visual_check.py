import argparse
import json
import os
import re
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

CASE_SETTINGS = {
    "mindmap": {
        "wait_s": 12.0,
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


def primary_monitor_size() -> Tuple[int, int]:
    import ctypes

    user32 = ctypes.windll.user32
    return int(user32.GetSystemMetrics(0)), int(user32.GetSystemMetrics(1))


def list_windows() -> list[Tuple[int, str]]:
    import ctypes

    user32 = ctypes.windll.user32
    results: list[Tuple[int, str]] = []

    def callback(hwnd, _lparam):
        if not user32.IsWindowVisible(hwnd):
            return True
        length = user32.GetWindowTextLengthW(hwnd)
        if length == 0:
            return True
        buffer = ctypes.create_unicode_buffer(length + 1)
        user32.GetWindowTextW(hwnd, buffer, length + 1)
        title = buffer.value
        if title:
            results.append((hwnd, title))
        return True

    enum_proc = ctypes.WINFUNCTYPE(ctypes.c_bool, ctypes.c_void_p, ctypes.c_void_p)(callback)
    user32.EnumWindows(enum_proc, 0)
    return results


def find_window(title_fragment: str, timeout_s: float) -> Optional[int]:
    deadline = time.time() + timeout_s
    needle = title_fragment.lower()
    while time.time() < deadline:
        for hwnd, title in list_windows():
            if needle in title.lower():
                return hwnd
        time.sleep(0.1)
    return None


def move_resize_window(hwnd: int, width: int, height: int) -> None:
    import ctypes

    user32 = ctypes.windll.user32
    SW_RESTORE = 9
    SWP_NOZORDER = 0x0004
    SWP_SHOWWINDOW = 0x0040
    user32.ShowWindow(hwnd, SW_RESTORE)
    user32.SetWindowPos(hwnd, None, 0, 0, width, height, SWP_NOZORDER | SWP_SHOWWINDOW)
    user32.SetForegroundWindow(hwnd)


def get_window_rect(hwnd: int) -> Optional[Tuple[int, int, int, int]]:
    import ctypes

    user32 = ctypes.windll.user32

    class RECT(ctypes.Structure):
        _fields_ = [
            ("left", ctypes.c_long),
            ("top", ctypes.c_long),
            ("right", ctypes.c_long),
            ("bottom", ctypes.c_long),
        ]

    rect = RECT()
    if user32.GetWindowRect(hwnd, ctypes.byref(rect)) == 0:
        return None
    return rect.left, rect.top, rect.right, rect.bottom


def crop_to_window(snap_path: Path, output_path: Path, rect: Tuple[int, int, int, int]) -> bool:
    left, top, right, bottom = rect
    if right <= left or bottom <= top:
        return False
    img = Image.open(snap_path)
    width, height = img.size
    crop_left = max(0, left)
    crop_top = max(0, top)
    crop_right = min(width, right)
    crop_bottom = min(height, bottom)
    if crop_right <= crop_left or crop_bottom <= crop_top:
        return False
    img.crop((crop_left, crop_top, crop_right, crop_bottom)).save(output_path)
    return True


def run_mdscreensnap(output_dir: Path, name: str, delay: float, monitor: str) -> Path:
    output_dir.mkdir(parents=True, exist_ok=True)
    delay_arg = str(int(round(delay)))
    cmd = [
        "mdscreensnap",
        "--output",
        str(output_dir),
        "--name",
        name,
        "--delay",
        delay_arg,
        "--monitor",
        monitor,
    ]
    subprocess.run(cmd, check=True)
    candidates = sorted(
        output_dir.glob(f"*{name}*.png"), key=lambda p: p.stat().st_mtime, reverse=True
    )
    if not candidates:
        raise RuntimeError(f"mdscreensnap did not produce output for {name}")
    return candidates[0]


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


def read_mdmdview_meta(output_png: Path) -> Optional[dict]:
    meta_path = output_png.with_suffix(".json")
    if not meta_path.exists():
        return None
    try:
        return json.loads(meta_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None


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


def find_content_bounds(
    img: Image.Image, bg_rgb: Tuple[int, int, int], tolerance: int
) -> Optional[Tuple[int, int, int, int]]:
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
        return None

    return min_x, min_y, max_x, max_y


def align_to_content(
    reference: Image.Image,
    actual: Image.Image,
    bg_rgb: Tuple[int, int, int],
    tolerance: int,
) -> Image.Image:
    ref_bounds = find_content_bounds(reference, bg_rgb, tolerance)
    act_bounds = find_content_bounds(actual, bg_rgb, tolerance)
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


def crop_to_background_region(
    img: Image.Image, bg_rgb: Tuple[int, int, int], tolerance: int
) -> Image.Image:
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
            if (
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


def write_report(report_dir: Path, results: list[dict]) -> None:
    report_dir.mkdir(parents=True, exist_ok=True)
    (report_dir / "report.json").write_text(
        json.dumps(results, indent=2), encoding="utf-8"
    )
    lines = ["# Mermaid Visual Diff Report", ""]
    failures = [r for r in results if not r["ok"]]
    lines.append(f"Cases: {len(results)}")
    lines.append(f"Failures: {len(failures)}")
    lines.append("")
    for result in results:
        status = "PASS" if result["ok"] else "FAIL"
        capture = result.get("capture", "unknown")
        lines.append(
            f"- {status} {result['case']} "
            f"({result['percent']:.3f}% diff, {result['diff_pixels']} pixels, "
            f"{result['message']}, capture={capture})"
        )
    (report_dir / "report.md").write_text("\n".join(lines) + "\n", encoding="utf-8")


def run_case(
    md_path: Path,
    mdmdview_bin: str,
    output_root: Path,
    width: int,
    height: int,
    theme: str,
    zoom: float,
    wait_s: float,
    snap_delay: float,
    monitor: str,
    threshold_percent: float,
    threshold_pixels: int,
    pixel_tolerance: int,
    test_fonts: Optional[str],
    mmdc_cmd: Optional[str],
    npx_cmd: str,
    config_path: Path,
    bg_rgb: Tuple[int, int, int],
) -> dict:
    case_id = md_path.stem
    settings = CASE_SETTINGS.get(case_id, {})
    if "wait_s" in settings:
        wait_s = float(settings["wait_s"])
    if "threshold_percent" in settings:
        threshold_percent = float(settings["threshold_percent"])
    if "threshold_pixels" in settings:
        threshold_pixels = int(settings["threshold_pixels"])
    actual_dir = output_root / "actual" / case_id
    reference_dir = output_root / "reference" / case_id
    diff_dir = output_root / "diff" / case_id
    actual_dir.mkdir(parents=True, exist_ok=True)
    reference_dir.mkdir(parents=True, exist_ok=True)
    diff_dir.mkdir(parents=True, exist_ok=True)

    text = md_path.read_text(encoding="utf-8").lstrip("\ufeff")
    blocks = find_mermaid_blocks(text)
    if len(blocks) != 1:
        return {
            "case": case_id,
            "ok": False,
            "message": f"expected 1 mermaid block, found {len(blocks)}",
            "diff_pixels": 0,
            "percent": 100.0,
            "max_delta": 255,
        }

    code = blocks[0]
    render_md_path = actual_dir / f"{case_id}_render.md"
    render_md_path.write_text(f"```mermaid\n{code}\n```\n", encoding="utf-8")

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

    cmd = [
        mdmdview_bin,
        str(render_md_path),
        "--width",
        str(width),
        "--height",
        str(height),
        "--theme",
        theme,
        "--zoom",
        f"{zoom:.3f}",
    ]
    if test_fonts:
        cmd.extend(["--test-fonts", test_fonts])

    proc = subprocess.Popen(cmd, env=env)
    try:
        hwnd = find_window(render_md_path.name, timeout_s=8.0)
        if hwnd:
            move_resize_window(hwnd, width, height)
        time.sleep(wait_s)
        snap_path = run_mdscreensnap(actual_dir, f"{case_id}-mdmdview", snap_delay, monitor)
        actual_raw = actual_dir / "mdmdview.png"
        capture_mode = "mdscreensnap"
        if hwnd:
            rect = get_window_rect(hwnd)
            full_path = actual_dir / "mdmdview-full.png"
            snap_path.replace(full_path)
            if rect and crop_to_window(full_path, actual_raw, rect):
                capture_mode = "mdscreensnap-window"
            else:
                actual_raw = full_path
        else:
            snap_path.replace(actual_raw)
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()

    mdsnap_raw = actual_raw
    mdmdview_raw = actual_dir / "mdmdview-screenshot.png"
    mdmdview_ok = False
    try:
        base_wait_ms = int(wait_s * 1000) if wait_s > 0 else 2000
        run_mdmdview_screenshot(
            mdmdview_bin=mdmdview_bin,
            md_path=render_md_path,
            output_png=mdmdview_raw,
            width=width,
            height=height,
            theme=theme,
            zoom=zoom,
            wait_ms=base_wait_ms,
            settle_frames=3,
            content_only=True,
            test_fonts=test_fonts,
            env=env,
        )
        meta = read_mdmdview_meta(mdmdview_raw)
        if meta and meta.get("timed_out") is True:
            retry_wait_ms = max(base_wait_ms * 2, 8000)
            run_mdmdview_screenshot(
                mdmdview_bin=mdmdview_bin,
                md_path=md_path,
                output_png=mdmdview_raw,
                width=width,
                height=height,
                theme=theme,
                zoom=zoom,
                wait_ms=retry_wait_ms,
                settle_frames=3,
                content_only=True,
                test_fonts=test_fonts,
                env=env,
            )
        if mdmdview_raw.exists():
            mdmdview_ok = not is_solid_image(Image.open(mdmdview_raw))
    except subprocess.CalledProcessError:
        mdmdview_ok = False

    if mdmdview_ok:
        actual_raw = mdmdview_raw
        capture_mode = "mdmdview-screenshot"
    else:
        actual_raw = mdsnap_raw

    reference_raw = reference_dir / "mermaid.png"
    run_mermaid_cli(
        code=code,
        output_png=reference_raw,
        width=width,
        height=height,
        bg=MERMAID_BG,
        config_path=config_path,
        mmdc_cmd=mmdc_cmd,
        npx_cmd=npx_cmd,
    )

    actual_raw_img = Image.open(actual_raw)
    capture_solid = is_solid_image(actual_raw_img)
    if capture_solid:
        ref_img = Image.open(reference_raw)
        return {
            "case": case_id,
            "ok": False,
            "message": "actual_capture_solid",
            "diff_pixels": 0,
            "percent": 100.0,
            "max_delta": 255,
            "size_ref": ref_img.size,
            "size_actual": actual_raw_img.size,
            "capture": capture_mode,
        }

    actual_raw_img = Image.open(actual_raw)
    actual_bg = guess_background_color(actual_raw_img)
    actual_crop = crop_to_background_region(actual_raw_img, actual_bg, tolerance=10)
    if actual_crop.size == actual_raw_img.size:
        actual_crop = crop_to_content(actual_raw_img, actual_bg, tolerance=10)
    reference_crop = crop_to_content(Image.open(reference_raw), bg_rgb, tolerance=10)
    reference_crop, actual_crop = pad_to_common_size(reference_crop, actual_crop, bg_rgb)
    if actual_bg != bg_rgb:
        actual_crop = normalize_background(actual_crop, actual_bg, bg_rgb, tolerance=12)
    actual_crop = align_to_content(reference_crop, actual_crop, bg_rgb, tolerance=10)
    actual_crop_path = actual_dir / "mdmdview-crop.png"
    reference_crop_path = reference_dir / "mermaid-crop.png"
    actual_crop.save(actual_crop_path)
    reference_crop.save(reference_crop_path)

    diff_path = diff_dir / "diff.png"
    diff_result = diff_images(
        reference_png=reference_crop_path,
        actual_png=actual_crop_path,
        diff_png=diff_path,
        threshold_percent=threshold_percent,
        threshold_pixels=threshold_pixels,
        pixel_tolerance=pixel_tolerance,
    )

    return {
        "case": case_id,
        "ok": diff_result.ok,
        "message": diff_result.message,
        "diff_pixels": diff_result.diff_pixels,
        "percent": diff_result.percent,
        "max_delta": diff_result.max_delta,
        "size_ref": diff_result.size_ref,
        "size_actual": diff_result.size_actual,
        "capture": capture_mode,
    }


def iter_cases(cases_dir: Path, filters: Iterable[str]) -> list[Path]:
    paths = sorted(cases_dir.glob("*.md"))
    if not filters:
        return paths
    lookup = {item.lower() for item in filters}
    return [path for path in paths if path.stem.lower() in lookup]


def write_config(config_path: Path) -> None:
    config_path.parent.mkdir(parents=True, exist_ok=True)
    config_path.write_text(json.dumps(MERMAID_CONFIG, indent=2), encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--cases", default="tests/mermaid_visual/cases")
    parser.add_argument("--output", default="tests/mermaid_visual")
    parser.add_argument("--mdmdview", help="Path to mdmdview binary")
    parser.add_argument("--mmdc", help="Path to mermaid-cli (mmdc)")
    parser.add_argument("--npx", default="npx", help="npx command to use")
    parser.add_argument("--theme", default="light")
    parser.add_argument("--zoom", type=float, default=1.0)
    parser.add_argument("--wait", type=float, default=4.0, help="Seconds to wait before capture")
    parser.add_argument("--delay", type=float, default=0.0, help="mdscreensnap delay")
    parser.add_argument("--width", type=int)
    parser.add_argument("--height", type=int)
    parser.add_argument("--monitor", default="primary")
    parser.add_argument("--threshold-percent", type=float, default=12.0)
    parser.add_argument("--threshold-pixels", type=int, default=45000)
    parser.add_argument(
        "--pixel-tolerance",
        type=int,
        default=40,
        help="Ignore per-channel diffs at or below this value",
    )
    parser.add_argument("--test-fonts", help="Font directory for mdmdview")
    parser.add_argument("--case", action="append", default=[])
    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parents[1]
    mdmdview_bin = resolve_mdmdview_bin(repo_root, args.mdmdview)
    mmdc_cmd = resolve_mmdc(args.mmdc)

    width, height = primary_monitor_size()
    if args.width:
        width = args.width
    if args.height:
        height = args.height
    if args.monitor != "primary" and (args.width is None or args.height is None):
        print(
            "Specify --width and --height when using a non-primary monitor.",
            file=sys.stderr,
        )
        return 2

    cases_dir = Path(args.cases).resolve()
    output_root = Path(args.output).resolve()
    config_path = output_root / "reference" / "mermaid-config.json"
    write_config(config_path)
    bg_rgb = parse_hex_color(MERMAID_BG)

    results = []
    for md_path in iter_cases(cases_dir, args.case):
        result = run_case(
            md_path=md_path,
            mdmdview_bin=mdmdview_bin,
            output_root=output_root,
            width=width,
            height=height,
            theme=args.theme,
            zoom=args.zoom,
            wait_s=args.wait,
            snap_delay=args.delay,
            monitor=args.monitor,
            threshold_percent=args.threshold_percent,
            threshold_pixels=args.threshold_pixels,
            pixel_tolerance=args.pixel_tolerance,
            test_fonts=args.test_fonts,
            mmdc_cmd=mmdc_cmd,
            npx_cmd=args.npx,
            config_path=config_path,
            bg_rgb=bg_rgb,
        )
        results.append(result)

    write_report(output_root / "report", results)

    failures = [r for r in results if not r["ok"]]
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
