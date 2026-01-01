import argparse
import json
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional

try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover
    import tomli as tomllib


@dataclass
class SnapshotConfig:
    name: str
    scroll: Optional[float]


@dataclass
class CaseConfig:
    case_id: str
    path: Path
    title: str
    compare_mode: str
    reference: str
    viewport_width: int
    viewport_height: int
    dpr: float
    theme: str
    zoom: float
    content_only: bool
    wait_ms: int
    settle_frames: int
    diff_max_percent: float
    diff_max_pixels: int
    diff_masks: List[Path]
    snapshots: List[SnapshotConfig]


@dataclass
class DiffResult:
    ok: bool
    diff_pixels: int
    percent: float
    max_delta: int
    message: str
    compared_pixels: int
    ignored_pixels: int


def load_manifest(path: Path) -> Dict[str, Any]:
    return tomllib.loads(path.read_text(encoding="utf-8"))


def normalize_mask_list(value: Any) -> List[str]:
    if value is None:
        return []
    if isinstance(value, str):
        return [value]
    if isinstance(value, list):
        masks = []
        for entry in value:
            if isinstance(entry, str):
                masks.append(entry)
            else:
                raise ValueError("Mask entries must be strings")
        return masks
    raise ValueError("Mask must be a string or list of strings")


def resolve_case(defaults: Dict[str, Any], case: Dict[str, Any], manifest_dir: Path) -> CaseConfig:
    viewport = case.get("viewport", {})
    diff_defaults = defaults.get("diff", {})
    diff_case = case.get("diff", {})
    mask_values = normalize_mask_list(diff_case.get("mask", diff_defaults.get("mask")))
    mask_paths: List[Path] = []
    for mask in mask_values:
        mask_path = Path(mask)
        if not mask_path.is_absolute():
            mask_path = (manifest_dir / mask_path).resolve()
        else:
            mask_path = mask_path.resolve()
        mask_paths.append(mask_path)
    snapshots = case.get("snapshots")
    if snapshots is None:
        snapshots = [{"name": "top", "scroll": case.get("scroll", 0.0)}]

    resolved_snapshots = [
        SnapshotConfig(name=s.get("name", "snapshot"), scroll=s.get("scroll"))
        for s in snapshots
    ]

    return CaseConfig(
        case_id=case["id"],
        path=(manifest_dir / case["path"]).resolve(),
        title=case.get("title", case["id"]),
        compare_mode=case.get("compare_mode", defaults.get("compare_mode", "reference")),
        reference=case.get("reference", defaults.get("reference", "cmark-gfm")),
        viewport_width=int(viewport.get("width", defaults.get("viewport_width", 1280))),
        viewport_height=int(viewport.get("height", defaults.get("viewport_height", 720))),
        dpr=float(viewport.get("dpr", defaults.get("dpr", 1.0))),
        theme=case.get("theme", defaults.get("theme", "light")),
        zoom=float(case.get("zoom", defaults.get("zoom", 1.0))),
        content_only=bool(case.get("content_only", defaults.get("content_only", True))),
        wait_ms=int(case.get("wait_ms", defaults.get("wait_ms", 2000))),
        settle_frames=int(case.get("settle_frames", defaults.get("settle_frames", 3))),
        diff_max_percent=float(diff_case.get("max_percent", diff_defaults.get("max_percent", 0.10))),
        diff_max_pixels=int(diff_case.get("max_pixels", diff_defaults.get("max_pixels", 500))),
        diff_masks=mask_paths,
        snapshots=resolved_snapshots,
    )


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


def run_reference_renderer(
    root: Path,
    case: CaseConfig,
    snapshot: SnapshotConfig,
    out_dir: Path,
    css_path: Path,
    renderer_override: Optional[str],
    fonts_dir: Optional[str],
) -> None:
    renderer = renderer_override or case.reference
    ref_script = root / "tools" / "regression" / "reference_renderer.py"
    cmd = [
        sys.executable,
        str(ref_script),
        "--input",
        str(case.path),
        "--output-dir",
        str(out_dir),
        "--width",
        str(case.viewport_width),
        "--height",
        str(case.viewport_height),
        "--dpr",
        str(case.dpr),
        "--theme",
        case.theme,
        "--css",
        str(css_path),
        "--renderer",
        renderer,
    ]
    if fonts_dir:
        cmd.extend(["--fonts", fonts_dir])
    subprocess.run(cmd, check=True, cwd=str(root))


def run_mdmdview_capture(
    case: CaseConfig,
    snapshot: SnapshotConfig,
    out_dir: Path,
    mdmdview_bin: str,
    test_fonts: Optional[str],
) -> None:
    output_png = out_dir / "actual.png"
    cmd = [
        mdmdview_bin,
        "--screenshot",
        str(case.path),
        "--output",
        str(output_png),
        "--width",
        str(case.viewport_width),
        "--height",
        str(case.viewport_height),
        "--theme",
        case.theme,
        "--zoom",
        str(case.zoom),
        "--wait-ms",
        str(case.wait_ms),
        "--settle-frames",
        str(case.settle_frames),
    ]
    if case.content_only:
        cmd.append("--content-only")
    if snapshot.scroll is not None:
        cmd.extend(["--scroll", str(snapshot.scroll)])
    if test_fonts:
        cmd.extend(["--test-fonts", test_fonts])

    env = dict(os.environ)
    env["MDMDVIEW_MERMAID_RENDERER"] = "embedded"
    subprocess.run(cmd, check=True, env=env)


def load_mask_ignores(mask_paths: List[Path], size: tuple[int, int]) -> List[bool]:
    from PIL import Image

    if not mask_paths:
        return []

    total_pixels = size[0] * size[1]
    ignores = [False] * total_pixels
    for path in mask_paths:
        if not path.exists():
            raise RuntimeError(f"Mask not found: {path}")
        mask = Image.open(path).convert("RGBA")
        if mask.size != size:
            raise RuntimeError(f"Mask size mismatch for {path} (expected {size})")
        for idx, pixel in enumerate(mask.getdata()):
            r, g, b, a = pixel
            if a < 128 or (r + g + b) < 30:
                ignores[idx] = True
    return ignores


def diff_images(
    reference_png: Path,
    actual_png: Path,
    diff_png: Path,
    mask_paths: List[Path],
) -> DiffResult:
    try:
        from PIL import Image, ImageChops
    except ImportError as exc:
        raise RuntimeError("Pillow not installed") from exc

    if not reference_png.exists() or not actual_png.exists():
        return DiffResult(False, 0, 100.0, 0, "Missing images for diff", 0, 0)

    ref = Image.open(reference_png).convert("RGBA")
    act = Image.open(actual_png).convert("RGBA")
    if ref.size != act.size:
        return DiffResult(False, 0, 100.0, 0, "Image sizes differ", 0, 0)

    try:
        mask_ignores = load_mask_ignores(mask_paths, ref.size)
    except RuntimeError as exc:
        return DiffResult(False, 0, 100.0, 0, str(exc), 0, 0)

    diff = ImageChops.difference(ref, act)
    diff_data = list(diff.getdata())
    total_pixels = ref.size[0] * ref.size[1]
    diff_pixels = 0
    max_delta = 0
    ignored_pixels = 0
    compared_pixels = 0
    output_pixels = []
    for idx, pixel in enumerate(diff_data):
        if mask_ignores and mask_ignores[idx]:
            ignored_pixels += 1
            output_pixels.append((0, 0, 0, 0))
            continue
        compared_pixels += 1
        delta = max(pixel)
        if delta > 0:
            diff_pixels += 1
            if delta > max_delta:
                max_delta = delta
        output_pixels.append(pixel)

    denom = compared_pixels if compared_pixels else total_pixels
    percent = (diff_pixels / denom) * 100 if denom else 0.0
    diff_png.parent.mkdir(parents=True, exist_ok=True)
    if output_pixels:
        diff_out = Image.new("RGBA", ref.size)
        diff_out.putdata(output_pixels)
        diff_out.save(diff_png)
    else:
        diff.save(diff_png)
    return DiffResult(
        True,
        diff_pixels,
        percent,
        max_delta,
        "ok",
        compared_pixels,
        ignored_pixels,
    )


def write_diff_json(path: Path, result: DiffResult) -> None:
    data = {
        "ok": result.ok,
        "diff_pixels": result.diff_pixels,
        "percent": result.percent,
        "max_delta": result.max_delta,
        "message": result.message,
        "compared_pixels": result.compared_pixels,
        "ignored_pixels": result.ignored_pixels,
    }
    path.write_text(json.dumps(data, indent=2), encoding="utf-8")


def run_cases(
    manifest_path: Path,
    mdmdview_bin: str,
    reference_css: Path,
    renderer_override: Optional[str],
    test_fonts: Optional[str],
    update_reference: bool,
    update_baseline: bool,
    compare: bool,
    case_filter: Optional[str],
) -> int:
    manifest = load_manifest(manifest_path)
    defaults = manifest.get("defaults", {})
    cases = manifest.get("case", [])
    manifest_dir = manifest_path.parent

    results: List[Dict[str, Any]] = []
    failures = 0

    for case in cases:
        resolved = resolve_case(defaults, case, manifest_dir)
        if case_filter and resolved.case_id != case_filter:
            continue

        for snapshot in resolved.snapshots:
            ref_dir = manifest_dir / "reference" / resolved.case_id / snapshot.name
            actual_dir = manifest_dir / "actual" / resolved.case_id / snapshot.name
            diff_dir = manifest_dir / "diff" / resolved.case_id / snapshot.name
            baseline_dir = manifest_dir / "baseline" / resolved.case_id / snapshot.name

            if update_reference or (
                resolved.compare_mode == "reference"
                and not (ref_dir / "reference.png").exists()
            ):
                run_reference_renderer(
                    manifest_path.parents[2],
                    resolved,
                    snapshot,
                    ref_dir,
                    reference_css,
                    renderer_override,
                    test_fonts,
                )

            if update_baseline or compare:
                run_mdmdview_capture(
                    resolved,
                    snapshot,
                    actual_dir,
                    mdmdview_bin,
                    test_fonts,
                )

            if compare:
                if resolved.compare_mode == "reference":
                    reference_png = ref_dir / "reference.png"
                else:
                    reference_png = baseline_dir / "baseline.png"

                actual_png = actual_dir / "actual.png"
                diff_png = diff_dir / "diff.png"
                diff_result = diff_images(reference_png, actual_png, diff_png, resolved.diff_masks)

                ok = diff_result.ok and (
                    diff_result.diff_pixels <= resolved.diff_max_pixels
                    and diff_result.percent <= resolved.diff_max_percent
                )
                if not ok:
                    failures += 1

                diff_dir.mkdir(parents=True, exist_ok=True)
                write_diff_json(diff_dir / "diff.json", diff_result)
                results.append(
                    {
                        "case": resolved.case_id,
                        "snapshot": snapshot.name,
                        "ok": ok,
                        "diff_pixels": diff_result.diff_pixels,
                        "percent": diff_result.percent,
                        "max_delta": diff_result.max_delta,
                        "compare_mode": resolved.compare_mode,
                    }
                )

            if update_baseline and resolved.compare_mode == "mdmdview":
                baseline_dir.mkdir(parents=True, exist_ok=True)
                (actual_dir / "actual.png").replace(baseline_dir / "baseline.png")
                actual_json = actual_dir / "actual.json"
                if actual_json.exists():
                    actual_json.replace(baseline_dir / "baseline.json")

    if compare:
        report_dir = manifest_dir / "report"
        report_dir.mkdir(parents=True, exist_ok=True)
        report_json = report_dir / "report.json"
        report_md = report_dir / "report.md"
        report_json.write_text(json.dumps(results, indent=2), encoding="utf-8")

        lines = ["# Regression Report", "", f"Failures: {failures}", ""]
        for result in results:
            status = "PASS" if result["ok"] else "FAIL"
            lines.append(
                f"- {status} {result['case']}:{result['snapshot']} "
                f"({result['percent']:.3f}% diff, {result['diff_pixels']} pixels)"
            )
        report_md.write_text("\n".join(lines) + "\n", encoding="utf-8")

    return 1 if failures > 0 else 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--manifest",
        default="tests/regression/manifest.toml",
        help="Path to manifest.toml",
    )
    parser.add_argument("--mdmdview", help="Path to mdmdview binary")
    parser.add_argument("--reference", help="Override reference renderer")
    parser.add_argument(
        "--css",
        default="tools/regression/reference.css",
        help="Reference CSS path",
    )
    parser.add_argument("--test-fonts", help="Font directory for mdmdview")
    parser.add_argument("--case", help="Run a single case id")

    subparsers = parser.add_subparsers(dest="command", required=True)
    subparsers.add_parser("run")
    subparsers.add_parser("update-reference")
    subparsers.add_parser("update-baseline")

    args = parser.parse_args()

    manifest_path = Path(args.manifest).resolve()
    root = manifest_path.parents[2]
    mdmdview_bin = resolve_mdmdview_bin(root, args.mdmdview)
    reference_css = Path(args.css).resolve()

    if args.command == "run":
        return run_cases(
            manifest_path,
            mdmdview_bin,
            reference_css,
            args.reference,
            args.test_fonts,
            update_reference=False,
            update_baseline=False,
            compare=True,
            case_filter=args.case,
        )
    if args.command == "update-reference":
        return run_cases(
            manifest_path,
            mdmdview_bin,
            reference_css,
            args.reference,
            args.test_fonts,
            update_reference=True,
            update_baseline=False,
            compare=False,
            case_filter=args.case,
        )
    if args.command == "update-baseline":
        return run_cases(
            manifest_path,
            mdmdview_bin,
            reference_css,
            args.reference,
            args.test_fonts,
            update_reference=False,
            update_baseline=True,
            compare=False,
            case_filter=args.case,
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
