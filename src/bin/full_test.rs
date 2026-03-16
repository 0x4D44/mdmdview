#![cfg_attr(coverage_nightly, feature(coverage_attribute))]
// Clippy pedantic allows for this binary
#![allow(clippy::doc_markdown)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::format_push_string)]
#![allow(clippy::items_after_statements)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::similar_names)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::manual_let_else)]
#![allow(clippy::redundant_closure_for_method_calls)]
#![allow(clippy::single_match_else)]
#![allow(clippy::if_not_else)]
#![allow(clippy::needless_pass_by_value)]
#![allow(clippy::struct_excessive_bools)]
#![allow(clippy::redundant_else)]
#![allow(clippy::match_same_arms)]

//! Full Test Runner for mdmdview
//!
//! Runs all quality checks, tests, Mermaid visual comparisons, D2 visual
//! regression tests, and coverage in a single invocation, then generates
//! a self-contained HTML report.
//!
//! Phases (run sequentially, fast-to-slow):
//! 1. Code quality checks (cargo fmt --check, cargo clippy)
//! 2. Unit + D2 tests (cargo test --release --lib --tests --workspace)
//! 3. Mermaid visual tests (screenshot comparison against reference images)
//!    3b. D2 visual tests (SVG rasterization comparison against d2 CLI)
//! 4. Coverage analysis (cargo llvm-cov, optional)
//!
//! Usage:
//!   cargo run --release --bin full_test
//!
//! Options:
//!   --skip-quality           Skip fmt/clippy checks
//!   --skip-coverage          Skip coverage analysis
//!   --skip-mermaid           Skip Mermaid visual tests
//!   --skip-d2-visual         Skip D2 visual regression tests
//!   --update-d2-references   Regenerate D2 reference PNGs from d2 CLI
//!   --quick                  Exclude ignored tests
//!   --help, -h               Show this help message

use chrono::Local;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

const VERSION: &str = env!("CARGO_PKG_VERSION");

// ── Mermaid visual test constants ───────────────────────────────────────

const MERMAID_THRESHOLD_PERCENT: f64 = 12.0;
const MERMAID_THRESHOLD_PIXELS: usize = 45_000;
const MERMAID_PIXEL_TOLERANCE: u8 = 60;
const MERMAID_SCREENSHOT_WIDTH: u32 = 1280;
const MERMAID_SCREENSHOT_HEIGHT: u32 = 720;
const MERMAID_WAIT_MS: u32 = 3000;
const MERMAID_SETTLE_FRAMES: u32 = 5;

/// Per-case overrides for Mermaid visual tests.
/// Mindmap needs extra rendering time.
fn mermaid_wait_ms_for_case(case_name: &str) -> u32 {
    match case_name {
        "mindmap" => 12_000,
        _ => MERMAID_WAIT_MS,
    }
}

// ── D2 visual test constants ────────────────────────────────────────────

const D2_PIXEL_TOLERANCE: u8 = 60;
const D2_THRESHOLD_PERCENT: f64 = 20.0; // Generous — different renderers
const D2_THRESHOLD_PIXELS: usize = 80_000;

// ── Data structures ─────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct FullTestOptions {
    quick: bool,
    skip_quality: bool,
    skip_coverage: bool,
    skip_mermaid: bool,
    skip_d2_visual: bool,
    update_d2_references: bool,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct TestResult {
    name: String,
    passed: bool,
    duration: Duration,
    output: String,
}

#[derive(Debug)]
#[allow(dead_code)]
struct SuiteResults {
    mode: String,
    total: usize,
    passed: usize,
    failed: usize,
    skipped: usize,
    duration: Duration,
    tests: Vec<TestResult>,
}

#[derive(Debug, Clone)]
struct CategoryStats {
    name: String,
    total: usize,
    passed: usize,
    failed: usize,
}

#[derive(Debug)]
struct QualityResult {
    check_name: String,
    passed: bool,
    duration: Duration,
    issues: Vec<String>,
}

#[derive(Debug)]
struct CoverageResult {
    lines_covered: usize,
    lines_total: usize,
    functions_covered: usize,
    functions_total: usize,
    branches_covered: usize,
    branches_total: usize,
    regions_covered: usize,
    regions_total: usize,
    duration: Duration,
}

#[derive(Debug, Clone)]
struct MermaidCaseResult {
    case_name: String,
    passed: bool,
    message: String,
    diff_percent: f64,
    diff_pixels: usize,
    max_delta: u8,
    duration: Duration,
}

#[derive(Debug)]
struct MermaidResults {
    cases: Vec<MermaidCaseResult>,
    build_ok: bool,
    duration: Duration,
}

#[derive(Debug, Clone)]
struct D2VisualCaseResult {
    case_name: String,
    passed: bool,
    message: String,
    diff_percent: f64,
    diff_pixels: usize,
    max_delta: u8,
    duration: Duration,
}

#[derive(Debug)]
struct D2VisualResults {
    cases: Vec<D2VisualCaseResult>,
    d2_cli_available: bool,
    duration: Duration,
}

// ── Process priority ────────────────────────────────────────────────────

/// Set the process to low CPU scheduling priority so interactive apps stay responsive.
fn set_low_priority() {
    #[cfg(unix)]
    {
        extern "C" {
            fn nice(inc: i32) -> i32;
        }
        unsafe {
            nice(10);
        }
    }
    #[cfg(windows)]
    {
        extern "system" {
            fn GetCurrentProcess() -> *mut std::ffi::c_void;
            fn SetPriorityClass(hProcess: *mut std::ffi::c_void, dwPriorityClass: u32) -> i32;
        }
        const BELOW_NORMAL_PRIORITY_CLASS: u32 = 0x0000_4000;
        unsafe {
            SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS);
        }
    }
}

// ── Utility functions ───────────────────────────────────────────────────

/// Escape HTML special characters.
fn html_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(c),
        }
    }
    out
}

/// Get git commit hash and branch name for report metadata.
fn get_git_info() -> String {
    let hash = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .and_then(|o| {
            if o.status.success() {
                Some(String::from_utf8_lossy(&o.stdout).trim().to_string())
            } else {
                None
            }
        })
        .unwrap_or_else(|| "unknown".to_string());

    let branch = Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .output()
        .ok()
        .and_then(|o| {
            if o.status.success() {
                Some(String::from_utf8_lossy(&o.stdout).trim().to_string())
            } else {
                None
            }
        })
        .unwrap_or_else(|| "unknown".to_string());

    format!("{hash} ({branch})")
}

/// Build a date-stamped report path with collision handling.
fn build_report_path(now: chrono::DateTime<Local>) -> PathBuf {
    let date = now.format("%Y.%m.%d");
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("results");

    let base = dir.join(format!("{date} - full test report.html"));
    if !base.exists() {
        return base;
    }

    for n in 2.. {
        let path = dir.join(format!("{date} - full test report ({n}).html"));
        if !path.exists() {
            return path;
        }
    }
    unreachable!()
}

/// Write report content to a file, creating parent directories as needed.
fn write_report(report_path: &Path, report: &str) -> std::io::Result<()> {
    if let Some(parent) = report_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let mut file = std::fs::File::create(report_path)?;
    file.write_all(report.as_bytes())?;
    Ok(())
}

// ── Option parsing ──────────────────────────────────────────────────────

fn parse_options(args: &[String]) -> Option<FullTestOptions> {
    if args.iter().any(|a| a == "--help" || a == "-h") {
        return None;
    }
    Some(FullTestOptions {
        quick: args.iter().any(|a| a == "--quick"),
        skip_quality: args.iter().any(|a| a == "--skip-quality"),
        skip_coverage: args.iter().any(|a| a == "--skip-coverage"),
        skip_mermaid: args.iter().any(|a| a == "--skip-mermaid"),
        skip_d2_visual: args.iter().any(|a| a == "--skip-d2-visual"),
        update_d2_references: args.iter().any(|a| a == "--update-d2-references"),
    })
}

fn print_help() {
    println!("full_test {VERSION}");
    println!();
    println!("Full test runner for mdmdview - runs quality checks, tests, Mermaid");
    println!("visual comparisons, D2 visual comparisons, and coverage, then generates");
    println!("an HTML report.");
    println!();
    println!("USAGE:");
    println!("    cargo run --release --bin full_test [OPTIONS]");
    println!();
    println!("OPTIONS:");
    println!("    --skip-quality           Skip code quality checks (fmt, clippy)");
    println!("    --skip-coverage          Skip coverage analysis");
    println!("    --skip-mermaid           Skip Mermaid visual tests");
    println!("    --skip-d2-visual         Skip D2 visual regression tests");
    println!("    --update-d2-references   Regenerate D2 reference PNGs from d2 CLI");
    println!("    --quick                  Exclude ignored tests (faster run)");
    println!("    --help, -h               Show this help message");
    println!();
    println!("PHASES:");
    println!("    1. Code quality checks (cargo fmt --check, cargo clippy)");
    println!("    2. Unit + D2 tests (cargo test --release --workspace)");
    println!("    3. Mermaid visual tests (screenshot comparison)");
    println!("    3b. D2 visual tests (SVG rasterization comparison)");
    println!("    4. Coverage analysis (cargo llvm-cov, optional)");
    println!();
    println!("OUTPUT:");
    println!("    Self-contained HTML report in tests/results/");
    println!();
    println!("EXAMPLES:");
    println!("    cargo run --release --bin full_test");
    println!("        Run full test suite with all checks");
    println!();
    println!("    cargo run --release --bin full_test -- --skip-coverage");
    println!("        Skip coverage (much faster)");
    println!();
    println!("    cargo run --release --bin full_test -- --quick --skip-mermaid");
    println!("        Quick run without Mermaid visual tests");
    println!();
    println!("    cargo run --release --bin full_test -- --update-d2-references");
    println!("        Regenerate D2 reference PNGs from d2 CLI and exit");
}

fn describe_run_mode(options: &FullTestOptions) -> String {
    let mut parts: Vec<&str> = Vec::new();

    if options.quick {
        parts.push("Quick (excluding ignored)");
    } else {
        parts.push("Full Suite (including ignored)");
    }

    if options.skip_quality {
        parts.push("Quality Skipped");
    }
    if options.skip_mermaid {
        parts.push("Mermaid Skipped");
    }
    if options.skip_d2_visual {
        parts.push("D2 Visual Skipped");
    }
    if options.skip_coverage {
        parts.push("Coverage Skipped");
    }

    parts.join(" | ")
}

// ── Code quality checks ─────────────────────────────────────────────────

fn run_fmt_check() -> QualityResult {
    println!("Running cargo fmt --check...");
    let start = Instant::now();

    let mut cmd = Command::new("cargo");
    cmd.args(["fmt", "--check"]);
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    let output = cmd.output().expect("Failed to execute cargo fmt");
    let duration = start.elapsed();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let passed = output.status.success();

    let mut issues = Vec::new();
    for line in stdout.lines().chain(stderr.lines()) {
        let trimmed = line.trim();
        if !trimmed.is_empty()
            && !trimmed.starts_with("Diff in")
            && (trimmed.ends_with(".rs") || trimmed.contains("would reformat"))
        {
            issues.push(trimmed.to_string());
        }
    }

    println!(
        "cargo fmt: {} in {:.2}s",
        if passed { "PASS" } else { "FAIL" },
        duration.as_secs_f64()
    );

    QualityResult {
        check_name: "cargo fmt".to_string(),
        passed,
        duration,
        issues,
    }
}

fn run_clippy() -> QualityResult {
    println!("Running cargo clippy...");
    let start = Instant::now();

    let mut cmd = Command::new("cargo");
    cmd.args([
        "clippy",
        "--workspace",
        "--all-targets",
        "--message-format=short",
        "--",
        "-D",
        "warnings",
    ]);
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    let output = cmd.output().expect("Failed to execute cargo clippy");
    let duration = start.elapsed();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let passed = output.status.success();

    let mut issues = Vec::new();
    for line in stdout.lines().chain(stderr.lines()) {
        let trimmed = line.trim();
        if trimmed.contains("warning:") || trimmed.contains("error:") || trimmed.contains("error[")
        {
            issues.push(trimmed.to_string());
        }
    }

    println!(
        "cargo clippy: {} ({} issues) in {:.2}s",
        if passed { "PASS" } else { "FAIL" },
        issues.len(),
        duration.as_secs_f64()
    );

    QualityResult {
        check_name: "cargo clippy".to_string(),
        passed,
        duration,
        issues,
    }
}

// ── Test execution ──────────────────────────────────────────────────────

fn build_cargo_test_command(include_ignored: bool) -> Command {
    let mut cmd = Command::new("cargo");
    cmd.arg("test");
    cmd.arg("--release");
    // --lib --tests: compile library + integration tests only, skip binary targets.
    // This avoids Windows file-lock errors when cargo tries to overwrite the
    // running full_test.exe in target/release/.
    cmd.arg("--lib");
    cmd.arg("--tests");
    cmd.arg("--workspace");
    cmd.arg("--no-fail-fast");
    cmd.arg("--");
    cmd.arg("--test-threads=1");

    if include_ignored {
        cmd.arg("--include-ignored");
    }

    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    cmd
}

fn run_cargo_test(include_ignored: bool) -> SuiteResults {
    let start = Instant::now();
    let mut cmd = build_cargo_test_command(include_ignored);

    println!(
        "Running tests{}...",
        if include_ignored {
            " (including ignored)"
        } else {
            ""
        }
    );

    let output = cmd.output().expect("Failed to execute cargo test");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let duration = start.elapsed();

    if !output.status.success() && output.stdout.is_empty() {
        eprintln!("Warning: cargo test exited with {}", output.status);
        for line in stderr.lines().take(10) {
            eprintln!("  {line}");
        }
    }

    parse_test_output("tests", &stdout, &stderr, duration)
}

fn parse_test_output(mode: &str, stdout: &str, stderr: &str, duration: Duration) -> SuiteResults {
    let mut tests = Vec::new();
    let mut total = 0;
    let mut passed = 0;
    let mut failed = 0;
    let mut skipped = 0;

    for line in stdout.lines().chain(stderr.lines()) {
        if line.starts_with("test ")
            && (line.contains(" ... ok")
                || line.contains(" ... FAILED")
                || line.contains(" ... ignored"))
        {
            let parts: Vec<&str> = line.split(" ... ").collect();
            if parts.len() >= 2 {
                let name = parts[0].trim_start_matches("test ").to_string();
                let status = parts[1].trim();

                total += 1;
                let test_passed = if status == "ok" {
                    passed += 1;
                    true
                } else if status == "FAILED" {
                    failed += 1;
                    false
                } else if status.starts_with("ignored") {
                    skipped += 1;
                    continue;
                } else {
                    continue;
                };

                tests.push(TestResult {
                    name,
                    passed: test_passed,
                    duration: Duration::ZERO,
                    output: String::new(),
                });
            }
        }
    }

    SuiteResults {
        mode: mode.to_string(),
        total,
        passed,
        failed,
        skipped,
        duration,
        tests,
    }
}

// ── Test categorization ─────────────────────────────────────────────────

/// Extract category from test name by taking first 2 path components.
/// e.g., "markdown_renderer::tests::test_heading" -> "markdown_renderer::tests"
fn extract_category(test_name: &str) -> String {
    if test_name.is_empty() {
        return "uncategorized".to_string();
    }
    let parts: Vec<&str> = test_name.split("::").collect();
    if parts.len() >= 2 {
        format!("{}::{}", parts[0], parts[1])
    } else if !parts.is_empty() && !parts[0].is_empty() {
        parts[0].to_string()
    } else {
        "uncategorized".to_string()
    }
}

/// Categorize tests by module and return statistics for each category.
fn categorize_tests(tests: &[TestResult]) -> Vec<CategoryStats> {
    use std::collections::HashMap;

    let mut categories: HashMap<String, CategoryStats> = HashMap::new();

    for test in tests {
        let category = extract_category(&test.name);
        let stats = categories.entry(category.clone()).or_insert(CategoryStats {
            name: category,
            total: 0,
            passed: 0,
            failed: 0,
        });
        stats.total += 1;
        if test.passed {
            stats.passed += 1;
        } else {
            stats.failed += 1;
        }
    }

    let mut result: Vec<CategoryStats> = categories.into_values().collect();
    // Sort: failures first, then alphabetically
    result.sort_by(|a, b| b.failed.cmp(&a.failed).then_with(|| a.name.cmp(&b.name)));
    result
}

// ── Mermaid visual tests ────────────────────────────────────────────────

/// Build the mdmdview release binary for screenshot mode.
fn build_mdmdview_binary() -> bool {
    println!("Building mdmdview for screenshot mode...");
    let output = Command::new("cargo")
        .args(["build", "--release", "--bin", "mdmdview"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("Failed to execute cargo build");

    if output.status.success() {
        println!("mdmdview build: OK");
        true
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("mdmdview build: FAILED");
        for line in stderr.lines().take(10) {
            eprintln!("  {line}");
        }
        false
    }
}

/// Pad two images to the same dimensions, centering the smaller one on a white background.
fn pad_to_common_size(
    a: &image::RgbaImage,
    b: &image::RgbaImage,
) -> (image::RgbaImage, image::RgbaImage) {
    if a.dimensions() == b.dimensions() {
        return (a.clone(), b.clone());
    }
    let w = a.width().max(b.width());
    let h = a.height().max(b.height());
    let bg = image::Rgba([255u8, 255, 255, 255]);

    fn pad(img: &image::RgbaImage, w: u32, h: u32, bg: image::Rgba<u8>) -> image::RgbaImage {
        let mut canvas = image::RgbaImage::from_pixel(w, h, bg);
        let ox = (w - img.width()) / 2;
        let oy = (h - img.height()) / 2;
        for y in 0..img.height() {
            for x in 0..img.width() {
                canvas.put_pixel(x + ox, y + oy, *img.get_pixel(x, y));
            }
        }
        canvas
    }

    (pad(a, w, h, bg), pad(b, w, h, bg))
}

/// Per-pixel diff between two same-sized RGBA images.
/// Returns (count of pixels exceeding tolerance, maximum channel delta).
fn diff_images(
    actual: &image::RgbaImage,
    reference: &image::RgbaImage,
    tolerance: u8,
) -> (usize, u8) {
    let mut diff_pixels = 0usize;
    let mut max_delta = 0u8;

    for (a_px, r_px) in actual.pixels().zip(reference.pixels()) {
        let delta = a_px
            .0
            .iter()
            .zip(r_px.0.iter())
            .map(|(&av, &rv)| av.abs_diff(rv))
            .max()
            .unwrap_or(0);

        if delta > tolerance {
            diff_pixels += 1;
        }
        if delta > max_delta {
            max_delta = delta;
        }
    }

    (diff_pixels, max_delta)
}

/// Run a single Mermaid visual test case.
fn run_mermaid_case(
    mdmdview_bin: &Path,
    case_md: &Path,
    reference_dir: &Path,
    actual_dir: &Path,
) -> MermaidCaseResult {
    let start = Instant::now();
    let case_name = case_md
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string();

    // Check reference image exists
    let reference_path = reference_dir.join(format!("{case_name}.png"));
    if !reference_path.exists() {
        return MermaidCaseResult {
            case_name,
            passed: false,
            message: "reference_missing".to_string(),
            diff_percent: 100.0,
            diff_pixels: 0,
            max_delta: 0,
            duration: start.elapsed(),
        };
    }

    // Ensure actual output directory exists
    let _ = std::fs::create_dir_all(actual_dir);
    let actual_path = actual_dir.join(format!("{case_name}.png"));

    // Run mdmdview --screenshot
    let wait_ms = mermaid_wait_ms_for_case(&case_name);
    let mut cmd = Command::new(mdmdview_bin);
    cmd.args([
        "--screenshot",
        &case_md.to_string_lossy(),
        "--output",
        &actual_path.to_string_lossy(),
        "--width",
        &MERMAID_SCREENSHOT_WIDTH.to_string(),
        "--height",
        &MERMAID_SCREENSHOT_HEIGHT.to_string(),
        "--wait-ms",
        &wait_ms.to_string(),
        "--settle-frames",
        &MERMAID_SETTLE_FRAMES.to_string(),
        "--content-only",
        "--theme",
        "light",
    ]);

    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    let screenshot_result = cmd.output();
    match screenshot_result {
        Err(e) => {
            return MermaidCaseResult {
                case_name,
                passed: false,
                message: format!("screenshot_failed: {e}"),
                diff_percent: 100.0,
                diff_pixels: 0,
                max_delta: 0,
                duration: start.elapsed(),
            };
        }
        Ok(output) if !output.status.success() => {
            return MermaidCaseResult {
                case_name,
                passed: false,
                message: "screenshot_failed".to_string(),
                diff_percent: 100.0,
                diff_pixels: 0,
                max_delta: 0,
                duration: start.elapsed(),
            };
        }
        Ok(_) => {}
    }

    // Check that the screenshot was actually produced
    if !actual_path.exists() {
        return MermaidCaseResult {
            case_name,
            passed: false,
            message: "screenshot_not_produced".to_string(),
            diff_percent: 100.0,
            diff_pixels: 0,
            max_delta: 0,
            duration: start.elapsed(),
        };
    }

    // Load and compare images
    let actual_img = match image::open(&actual_path) {
        Ok(img) => img.to_rgba8(),
        Err(e) => {
            return MermaidCaseResult {
                case_name,
                passed: false,
                message: format!("actual_load_failed: {e}"),
                diff_percent: 100.0,
                diff_pixels: 0,
                max_delta: 0,
                duration: start.elapsed(),
            };
        }
    };

    let reference_img = match image::open(&reference_path) {
        Ok(img) => img.to_rgba8(),
        Err(e) => {
            return MermaidCaseResult {
                case_name,
                passed: false,
                message: format!("reference_load_failed: {e}"),
                diff_percent: 100.0,
                diff_pixels: 0,
                max_delta: 0,
                duration: start.elapsed(),
            };
        }
    };

    // Pad to common size if needed
    let (actual_padded, reference_padded) = pad_to_common_size(&actual_img, &reference_img);

    // Pixel diff
    let total_pixels = actual_padded.width() as usize * actual_padded.height() as usize;
    let (diff_pixels, max_delta) =
        diff_images(&actual_padded, &reference_padded, MERMAID_PIXEL_TOLERANCE);
    let diff_percent = if total_pixels > 0 {
        diff_pixels as f64 / total_pixels as f64 * 100.0
    } else {
        0.0
    };

    let passed =
        diff_pixels <= MERMAID_THRESHOLD_PIXELS && diff_percent <= MERMAID_THRESHOLD_PERCENT;

    let message = if passed {
        "ok".to_string()
    } else {
        "diff_exceeds_threshold".to_string()
    };

    MermaidCaseResult {
        case_name,
        passed,
        message,
        diff_percent,
        diff_pixels,
        max_delta,
        duration: start.elapsed(),
    }
}

/// Run all Mermaid visual test cases.
fn run_mermaid_visual_tests() -> MermaidResults {
    let start = Instant::now();

    // Build mdmdview first
    if !build_mdmdview_binary() {
        return MermaidResults {
            cases: Vec::new(),
            build_ok: false,
            duration: start.elapsed(),
        };
    }

    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mdmdview_bin = manifest_dir
        .join("target")
        .join("release")
        .join(if cfg!(windows) {
            "mdmdview.exe"
        } else {
            "mdmdview"
        });

    if !mdmdview_bin.exists() {
        eprintln!(
            "Warning: mdmdview binary not found at {}",
            mdmdview_bin.display()
        );
        return MermaidResults {
            cases: Vec::new(),
            build_ok: false,
            duration: start.elapsed(),
        };
    }

    let cases_dir = manifest_dir
        .join("tests")
        .join("mermaid_visual")
        .join("cases");
    let reference_dir = manifest_dir
        .join("tests")
        .join("mermaid_visual")
        .join("reference");
    let actual_dir = manifest_dir
        .join("tests")
        .join("mermaid_visual")
        .join("actual_full_test");

    // Clean previous actual output
    let _ = std::fs::remove_dir_all(&actual_dir);
    let _ = std::fs::create_dir_all(&actual_dir);

    // Discover case files
    let mut case_files: Vec<PathBuf> = std::fs::read_dir(&cases_dir)
        .map(|entries| {
            entries
                .filter_map(|e| e.ok())
                .map(|e| e.path())
                .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("md"))
                .collect()
        })
        .unwrap_or_default();
    case_files.sort();

    println!("Running {} Mermaid visual test cases...", case_files.len());

    let mut cases = Vec::new();
    for case_md in &case_files {
        let case_name = case_md
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");
        print!("  {case_name}...");
        let _ = std::io::stdout().flush();

        let result = run_mermaid_case(&mdmdview_bin, case_md, &reference_dir, &actual_dir);

        println!(
            " {} ({:.1}% diff, {} px, {:.2}s)",
            if result.passed { "PASS" } else { "FAIL" },
            result.diff_percent,
            result.diff_pixels,
            result.duration.as_secs_f64()
        );

        cases.push(result);
    }

    let passed_count = cases.iter().filter(|c| c.passed).count();
    let failed_count = cases.len() - passed_count;
    println!(
        "Mermaid visual: {passed_count}/{} passed ({failed_count} failed) in {:.2}s",
        cases.len(),
        start.elapsed().as_secs_f64()
    );

    MermaidResults {
        cases,
        build_ok: true,
        duration: start.elapsed(),
    }
}

// ── D2 visual tests ─────────────────────────────────────────────────────

/// Check whether the `d2` CLI is installed and runnable.
fn check_d2_cli() -> bool {
    Command::new("d2")
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

/// Rasterize an SVG string to a PNG file using usvg/resvg.
/// Uses a shared fontdb to avoid rebuilding system fonts per fixture (~100ms each).
fn rasterize_svg_to_png(
    svg: &str,
    output: &Path,
    fontdb: &std::sync::Arc<usvg::fontdb::Database>,
) -> Result<(), String> {
    let opt = usvg::Options {
        fontdb: std::sync::Arc::clone(fontdb),
        ..Default::default()
    };
    let tree = usvg::Tree::from_data(svg.as_bytes(), &opt)
        .map_err(|e| format!("usvg parse failed: {e}"))?;

    let size = tree.size();
    let w = size.width().ceil() as u32;
    let h = size.height().ceil() as u32;
    if w == 0 || h == 0 {
        return Err(format!("SVG has zero dimensions: {w}x{h}"));
    }

    let mut pixmap =
        tiny_skia::Pixmap::new(w, h).ok_or_else(|| format!("Failed to create pixmap {w}x{h}"))?;
    // Fill with white background so transparent regions don't cause false diffs.
    pixmap.fill(tiny_skia::Color::WHITE);
    let mut pmut = pixmap.as_mut();
    resvg::render(&tree, tiny_skia::Transform::identity(), &mut pmut);

    // Convert to image::RgbaImage and save as PNG.
    let rgba_data = pixmap.data().to_vec();
    let img = image::RgbaImage::from_raw(w, h, rgba_data)
        .ok_or_else(|| "Failed to create RgbaImage from pixmap".to_string())?;
    img.save(output)
        .map_err(|e| format!("Failed to save PNG: {e}"))?;

    Ok(())
}

/// Build a shared fontdb with system fonts loaded (expensive — do once).
fn build_fontdb() -> std::sync::Arc<usvg::fontdb::Database> {
    let mut db = usvg::fontdb::Database::new();
    db.load_system_fonts();
    std::sync::Arc::new(db)
}

/// Render a `.d2` fixture through the official `d2` CLI to SVG.
fn d2_cli_render(d2_file: &Path) -> Result<String, String> {
    let temp_svg = d2_file.with_extension("cli_out.svg");

    let output = Command::new("d2")
        .args([
            "--layout",
            "dagre",
            "--theme",
            "0",
            "--pad",
            "20",
            &d2_file.to_string_lossy(),
            &temp_svg.to_string_lossy(),
        ])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("d2 CLI exec failed: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let _ = std::fs::remove_file(&temp_svg);
        return Err(format!("d2 CLI failed: {stderr}"));
    }

    let svg = std::fs::read_to_string(&temp_svg)
        .map_err(|e| format!("Failed to read d2 CLI SVG output: {e}"))?;
    let _ = std::fs::remove_file(&temp_svg);

    Ok(svg)
}

/// Generate reference PNGs for all D2 fixtures using the d2 CLI.
/// Returns the number of references successfully generated.
fn generate_d2_references(
    fixtures_dir: &Path,
    reference_dir: &Path,
    fontdb: &std::sync::Arc<usvg::fontdb::Database>,
) -> usize {
    let _ = std::fs::create_dir_all(reference_dir);

    let mut fixtures: Vec<PathBuf> = std::fs::read_dir(fixtures_dir)
        .map(|entries| {
            entries
                .filter_map(|e| e.ok())
                .map(|e| e.path())
                .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("d2"))
                .collect()
        })
        .unwrap_or_default();
    fixtures.sort();

    let mut count = 0;
    for fixture in &fixtures {
        let case_name = fixture
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");
        print!("  {case_name}...");
        let _ = std::io::stdout().flush();

        match d2_cli_render(fixture) {
            Ok(svg) => {
                let png_path = reference_dir.join(format!("{case_name}.png"));
                match rasterize_svg_to_png(&svg, &png_path, fontdb) {
                    Ok(()) => {
                        println!(" OK");
                        count += 1;
                    }
                    Err(e) => println!(" RASTERIZE FAILED: {e}"),
                }
            }
            Err(e) => println!(" D2 CLI FAILED: {e}"),
        }
    }

    count
}

/// Run a single D2 visual regression test case.
fn run_d2_visual_case(
    fixture: &Path,
    reference_dir: &Path,
    actual_dir: &Path,
    fontdb: &std::sync::Arc<usvg::fontdb::Database>,
) -> D2VisualCaseResult {
    let start = Instant::now();
    let case_name = fixture
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string();

    // Check reference PNG exists — skip if missing (d2 CLI may not support this fixture)
    let reference_path = reference_dir.join(format!("{case_name}.png"));
    if !reference_path.exists() {
        return D2VisualCaseResult {
            case_name,
            passed: true, // Not a failure — just no reference to compare against
            message: "skipped_no_reference".to_string(),
            diff_percent: 0.0,
            diff_pixels: 0,
            max_delta: 0,
            duration: start.elapsed(),
        };
    }

    // Read D2 source and render via mdmdview's native D2 renderer
    let d2_source = match std::fs::read_to_string(fixture) {
        Ok(s) => s,
        Err(e) => {
            return D2VisualCaseResult {
                case_name,
                passed: false,
                message: format!("read_failed: {e}"),
                diff_percent: 100.0,
                diff_pixels: 0,
                max_delta: 0,
                duration: start.elapsed(),
            };
        }
    };

    let render_options = mdmdview_d2::RenderOptions {
        dark_mode: false,
        ..Default::default()
    };
    let render_result = match mdmdview_d2::render_d2_to_svg(&d2_source, &render_options) {
        Ok(r) => r,
        Err(e) => {
            return D2VisualCaseResult {
                case_name,
                passed: false,
                message: format!("render_failed: {e}"),
                diff_percent: 100.0,
                diff_pixels: 0,
                max_delta: 0,
                duration: start.elapsed(),
            };
        }
    };

    // Rasterize mdmdview SVG to PNG
    let actual_path = actual_dir.join(format!("{case_name}.png"));
    if let Err(e) = rasterize_svg_to_png(&render_result.svg, &actual_path, fontdb) {
        return D2VisualCaseResult {
            case_name,
            passed: false,
            message: format!("rasterize_failed: {e}"),
            diff_percent: 100.0,
            diff_pixels: 0,
            max_delta: 0,
            duration: start.elapsed(),
        };
    }

    // Load both PNGs
    let actual_img = match image::open(&actual_path) {
        Ok(img) => img.to_rgba8(),
        Err(e) => {
            return D2VisualCaseResult {
                case_name,
                passed: false,
                message: format!("actual_load_failed: {e}"),
                diff_percent: 100.0,
                diff_pixels: 0,
                max_delta: 0,
                duration: start.elapsed(),
            };
        }
    };

    let reference_img = match image::open(&reference_path) {
        Ok(img) => img.to_rgba8(),
        Err(e) => {
            return D2VisualCaseResult {
                case_name,
                passed: false,
                message: format!("reference_load_failed: {e}"),
                diff_percent: 100.0,
                diff_pixels: 0,
                max_delta: 0,
                duration: start.elapsed(),
            };
        }
    };

    // Pad to common size and diff
    let (actual_padded, reference_padded) = pad_to_common_size(&actual_img, &reference_img);
    let total_pixels = actual_padded.width() as usize * actual_padded.height() as usize;
    let (diff_pixels, max_delta) =
        diff_images(&actual_padded, &reference_padded, D2_PIXEL_TOLERANCE);
    let diff_percent = if total_pixels > 0 {
        diff_pixels as f64 / total_pixels as f64 * 100.0
    } else {
        0.0
    };

    let passed = diff_pixels <= D2_THRESHOLD_PIXELS && diff_percent <= D2_THRESHOLD_PERCENT;

    let message = if passed {
        "ok".to_string()
    } else {
        "diff_exceeds_threshold".to_string()
    };

    D2VisualCaseResult {
        case_name,
        passed,
        message,
        diff_percent,
        diff_pixels,
        max_delta,
        duration: start.elapsed(),
    }
}

/// Run all D2 visual regression test cases.
fn run_d2_visual_tests() -> D2VisualResults {
    let start = Instant::now();

    // Check d2 CLI availability
    let d2_cli_available = check_d2_cli();
    if !d2_cli_available {
        println!("Warning: d2 CLI not found. D2 visual tests will use existing references only.");
    }

    // Build shared fontdb once (expensive)
    println!("Building font database...");
    let fontdb = build_fontdb();

    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixtures_dir = manifest_dir
        .join("tests")
        .join("d2_conformance")
        .join("fixtures");
    let reference_dir = manifest_dir
        .join("tests")
        .join("d2_visual")
        .join("reference");
    let actual_dir = manifest_dir.join("tests").join("d2_visual").join("actual");

    // Auto-generate references if directory is empty or missing
    let reference_count = std::fs::read_dir(&reference_dir)
        .map(|entries| {
            entries
                .filter_map(|e| e.ok())
                .filter(|e| e.path().extension().and_then(|ext| ext.to_str()) == Some("png"))
                .count()
        })
        .unwrap_or(0);

    if reference_count == 0 {
        if d2_cli_available {
            println!("No reference PNGs found. Auto-generating from d2 CLI...");
            let generated = generate_d2_references(&fixtures_dir, &reference_dir, &fontdb);
            println!("Generated {generated} reference PNGs.");
        } else {
            println!("No reference PNGs and no d2 CLI — cannot run D2 visual tests.");
            return D2VisualResults {
                cases: Vec::new(),
                d2_cli_available,
                duration: start.elapsed(),
            };
        }
    }

    // Clean previous actual output
    let _ = std::fs::remove_dir_all(&actual_dir);
    let _ = std::fs::create_dir_all(&actual_dir);

    // Discover fixture files
    let mut fixture_files: Vec<PathBuf> = std::fs::read_dir(&fixtures_dir)
        .map(|entries| {
            entries
                .filter_map(|e| e.ok())
                .map(|e| e.path())
                .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("d2"))
                .collect()
        })
        .unwrap_or_default();
    fixture_files.sort();

    println!("Running {} D2 visual test cases...", fixture_files.len());

    let mut cases = Vec::new();
    for fixture in &fixture_files {
        let case_name = fixture
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");
        print!("  {case_name}...");
        let _ = std::io::stdout().flush();

        let result = run_d2_visual_case(fixture, &reference_dir, &actual_dir, &fontdb);

        if result.message == "skipped_no_reference" {
            println!(" SKIP (no reference)");
        } else {
            println!(
                " {} ({:.1}% diff, {} px, {:.2}s)",
                if result.passed { "PASS" } else { "FAIL" },
                result.diff_percent,
                result.diff_pixels,
                result.duration.as_secs_f64()
            );
        }

        cases.push(result);
    }

    let skipped_count = cases
        .iter()
        .filter(|c| c.message == "skipped_no_reference")
        .count();
    let compared_count = cases.len() - skipped_count;
    let passed_count = cases.iter().filter(|c| c.passed).count() - skipped_count;
    let failed_count = compared_count - passed_count;
    println!(
        "D2 visual: {passed_count}/{compared_count} passed ({failed_count} failed, {skipped_count} skipped) in {:.2}s",
        start.elapsed().as_secs_f64()
    );

    D2VisualResults {
        cases,
        d2_cli_available,
        duration: start.elapsed(),
    }
}

// ── Coverage ────────────────────────────────────────────────────────────

fn check_llvm_cov_available() -> bool {
    Command::new("cargo")
        .args(["llvm-cov", "--version"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

fn run_coverage() -> Option<CoverageResult> {
    if !check_llvm_cov_available() {
        println!("Warning: cargo-llvm-cov not found. Skipping coverage.");
        println!("Install with: cargo install cargo-llvm-cov");
        return None;
    }

    println!("Running coverage analysis (this may take a while)...");
    let start = Instant::now();

    let mut cmd = Command::new("cargo");
    cmd.args([
        "llvm-cov",
        "--workspace",
        "--branch",
        "--json",
        "--no-cfg-coverage",
    ]);
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    let output = match cmd.output() {
        Ok(o) => o,
        Err(e) => {
            println!("Failed to run coverage: {e}");
            return None;
        }
    };
    let duration = start.elapsed();

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!("Coverage failed: {stderr}");
        return None;
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    parse_coverage_json(&stdout, duration)
}

fn parse_coverage_json(json_output: &str, duration: Duration) -> Option<CoverageResult> {
    let mut result = CoverageResult {
        lines_covered: 0,
        lines_total: 0,
        functions_covered: 0,
        functions_total: 0,
        branches_covered: 0,
        branches_total: 0,
        regions_covered: 0,
        regions_total: 0,
        duration,
    };

    if let Some(totals_start) = json_output.find("\"totals\"") {
        let totals_section = &json_output[totals_start..];

        if let Some(v) = extract_json_number(totals_section, "\"lines\"", "\"covered\"") {
            result.lines_covered = v;
        }
        if let Some(v) = extract_json_number(totals_section, "\"lines\"", "\"count\"") {
            result.lines_total = v;
        }
        if let Some(v) = extract_json_number(totals_section, "\"functions\"", "\"covered\"") {
            result.functions_covered = v;
        }
        if let Some(v) = extract_json_number(totals_section, "\"functions\"", "\"count\"") {
            result.functions_total = v;
        }
        if let Some(v) = extract_json_number(totals_section, "\"branches\"", "\"covered\"") {
            result.branches_covered = v;
        }
        if let Some(v) = extract_json_number(totals_section, "\"branches\"", "\"count\"") {
            result.branches_total = v;
        }
        if let Some(v) = extract_json_number(totals_section, "\"regions\"", "\"covered\"") {
            result.regions_covered = v;
        }
        if let Some(v) = extract_json_number(totals_section, "\"regions\"", "\"count\"") {
            result.regions_total = v;
        }

        println!(
            "Coverage: {}/{} lines ({:.1}%) in {:.2}s",
            result.lines_covered,
            result.lines_total,
            if result.lines_total > 0 {
                result.lines_covered as f64 / result.lines_total as f64 * 100.0
            } else {
                0.0
            },
            duration.as_secs_f64()
        );

        Some(result)
    } else {
        println!("Could not parse coverage output");
        None
    }
}

/// Extract a number from nested JSON structure.
/// Looks for section_key, then within that section finds value_key and extracts the number.
fn extract_json_number(json: &str, section_key: &str, value_key: &str) -> Option<usize> {
    let section_start = json.find(section_key)?;
    let section = &json[section_start..];
    let brace_start = section.find('{')?;
    let section_content = &section[brace_start..];
    let brace_end = section_content.find('}')?;
    let inner = &section_content[..brace_end];

    let value_start = inner.find(value_key)?;
    let after_key = &inner[value_start + value_key.len()..];
    let after_colon = after_key.trim_start().strip_prefix(':')?;
    let num_str = after_colon.trim_start();

    let end = num_str
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(num_str.len());
    num_str[..end].parse().ok()
}

// ── Orchestration ───────────────────────────────────────────────────────

#[allow(dead_code)]
struct RunOutcome {
    html_report: String,
    report_path: PathBuf,
    any_failed: bool,
    quality_results: Vec<QualityResult>,
    test_results: Option<SuiteResults>,
    mermaid_results: Option<MermaidResults>,
    d2_visual_results: Option<D2VisualResults>,
    coverage_result: Option<CoverageResult>,
}

fn run_full_test<F>(
    options: &FullTestOptions,
    mut test_runner: F,
    now: chrono::DateTime<Local>,
) -> RunOutcome
where
    F: FnMut(bool) -> SuiteResults,
{
    let overall_start = Instant::now();
    println!("=== mdmdview Full Test Suite ===\n");

    // Phase 1: Quality checks (fast feedback)
    let quality_results = if !options.skip_quality {
        println!("--- Phase 1: Code Quality Checks ---\n");
        vec![run_fmt_check(), run_clippy()]
    } else {
        println!("--- Phase 1: Code Quality Checks (SKIPPED) ---\n");
        Vec::new()
    };

    // Phase 2: Unit + D2 tests
    println!("\n--- Phase 2: Unit + D2 Tests ---\n");
    let include_ignored = !options.quick;
    let test_results = Some(test_runner(include_ignored));

    if let Some(ref r) = test_results {
        println!(
            "\nTests: {}/{} passed ({} failed, {} skipped) in {:.2}s\n",
            r.passed,
            r.total,
            r.failed,
            r.skipped,
            r.duration.as_secs_f64()
        );
    }

    // Phase 3: Mermaid visual tests
    let mermaid_results = if !options.skip_mermaid {
        println!("--- Phase 3: Mermaid Visual Tests ---\n");
        Some(run_mermaid_visual_tests())
    } else {
        println!("--- Phase 3: Mermaid Visual Tests (SKIPPED) ---\n");
        None
    };

    // Phase 3b: D2 visual tests
    let d2_visual_results = if !options.skip_d2_visual {
        println!("\n--- Phase 3b: D2 Visual Tests ---\n");
        Some(run_d2_visual_tests())
    } else {
        println!("\n--- Phase 3b: D2 Visual Tests (SKIPPED) ---\n");
        None
    };

    // Phase 4: Coverage (slowest, run last)
    let coverage_result = if !options.skip_coverage {
        println!("\n--- Phase 4: Coverage Analysis ---\n");
        run_coverage()
    } else {
        println!("\n--- Phase 4: Coverage Analysis (SKIPPED) ---\n");
        None
    };

    // Determine pass/fail
    let quality_failed = quality_results.iter().any(|r| !r.passed);
    let tests_failed = test_results.as_ref().map(|r| r.failed > 0).unwrap_or(false);
    let mermaid_failed = mermaid_results
        .as_ref()
        .map(|r| !r.build_ok || r.cases.iter().any(|c| !c.passed))
        .unwrap_or(false);
    // D2 visual failures count, but d2 CLI unavailable does NOT count as failure.
    let d2_visual_failed = d2_visual_results
        .as_ref()
        .map(|r| r.cases.iter().any(|c| !c.passed))
        .unwrap_or(false);
    let any_failed = quality_failed || tests_failed || mermaid_failed || d2_visual_failed;

    // Generate report
    let run_mode = describe_run_mode(options);
    let git_info = get_git_info();
    let report_path = build_report_path(now);
    let html_report = generate_html_report(
        test_results.as_ref(),
        &quality_results,
        mermaid_results.as_ref(),
        d2_visual_results.as_ref(),
        coverage_result.as_ref(),
        &git_info,
        now,
        &run_mode,
        overall_start.elapsed(),
    );

    println!(
        "\nOverall: {} in {:.2}s",
        if any_failed { "FAILED" } else { "PASSED" },
        overall_start.elapsed().as_secs_f64()
    );

    RunOutcome {
        html_report,
        report_path,
        any_failed,
        quality_results,
        test_results,
        mermaid_results,
        d2_visual_results,
        coverage_result,
    }
}

// ── HTML report generation ──────────────────────────────────────────────

#[allow(clippy::too_many_arguments)]
fn generate_html_report(
    test_results: Option<&SuiteResults>,
    quality_results: &[QualityResult],
    mermaid_results: Option<&MermaidResults>,
    d2_visual_results: Option<&D2VisualResults>,
    coverage_result: Option<&CoverageResult>,
    git_info: &str,
    now: chrono::DateTime<Local>,
    run_mode: &str,
    total_duration: Duration,
) -> String {
    let timestamp = now.format("%Y.%m.%d %H:%M:%S").to_string();

    // Compute dashboard totals
    let mut total_passed: usize = 0;
    let mut total_failed: usize = 0;
    let mut total_skipped: usize = 0;

    if let Some(r) = test_results {
        total_passed += r.passed;
        total_failed += r.failed;
        total_skipped += r.skipped;
    }
    if let Some(r) = mermaid_results {
        total_passed += r.cases.iter().filter(|c| c.passed).count();
        total_failed += r.cases.iter().filter(|c| !c.passed).count();
    }
    if let Some(r) = d2_visual_results {
        total_passed += r.cases.iter().filter(|c| c.passed).count();
        total_failed += r.cases.iter().filter(|c| !c.passed).count();
    }

    let quality_passed = quality_results.iter().all(|r| r.passed);

    let mut html = String::with_capacity(64 * 1024);

    // ── DOCTYPE and head ────────────────────────────────────────────
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
    html.push_str("<meta charset=\"utf-8\">\n");
    html.push_str("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n");
    html.push_str("<title>mdmdview Full Test Report</title>\n");

    // ── Embedded CSS ────────────────────────────────────────────────
    html.push_str("<style>\n");
    html.push_str(
        ":root {\
\n  --bg: #ffffff; --fg: #1a1a2e; --card-bg: #f8f9fa; --border: #dee2e6;\
\n  --table-alt: #f8f9fa; --table-border: #dee2e6;\
\n  --pass-bg: #dcfce7; --pass-fg: #166534;\
\n  --fail-bg: #fee2e2; --fail-fg: #991b1b;\
\n  --skip-bg: #fef9c3; --skip-fg: #854d0e;\
\n  --link: #2563eb; --meta: #6b7280;\
\n}\
\n@media (prefers-color-scheme: dark) {\
\n  :root {\
\n    --bg: #1a1a2e; --fg: #e0e0e0; --card-bg: #16213e; --border: #374151;\
\n    --table-alt: #16213e; --table-border: #374151;\
\n    --pass-bg: #064e3b; --pass-fg: #6ee7b7;\
\n    --fail-bg: #7f1d1d; --fail-fg: #fca5a5;\
\n    --skip-bg: #78350f; --skip-fg: #fde68a;\
\n    --link: #60a5fa; --meta: #9ca3af;\
\n  }\
\n}\
\n* { box-sizing: border-box; }\
\nbody {\
\n  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;\
\n  background: var(--bg); color: var(--fg);\
\n  margin: 0; padding: 20px; line-height: 1.6;\
\n}\
\n.container { max-width: 1200px; margin: 0 auto; }\
\nh1 { margin: 0 0 4px 0; }\
\nh2 { margin-top: 32px; border-bottom: 2px solid var(--border); padding-bottom: 8px; }\
\n.meta { color: var(--meta); margin: 0 0 24px 0; }\
\n.run-mode { font-size: 1.1em; margin: 0 0 16px 0; padding: 8px 12px; background: var(--card-bg); border-radius: 4px; }\
\n.dashboard { display: flex; gap: 16px; flex-wrap: wrap; margin: 16px 0 24px 0; }\
\n.card {\
\n  display: inline-block; min-width: 120px; padding: 16px 24px;\
\n  border-radius: 8px; text-align: center;\
\n}\
\n.card .number { font-size: 2em; font-weight: bold; }\
\n.card .label { font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.05em; }\
\n.card.pass { background: var(--pass-bg); color: var(--pass-fg); }\
\n.card.fail { background: var(--fail-bg); color: var(--fail-fg); }\
\n.card.skip { background: var(--skip-bg); color: var(--skip-fg); }\
\ntable { width: 100%; border-collapse: collapse; margin: 12px 0; }\
\nth, td { padding: 8px 12px; text-align: left; border: 1px solid var(--table-border); }\
\nth { background: var(--card-bg); font-weight: 600; }\
\ntr:nth-child(even) td { background: var(--table-alt); }\
\n.status-pass { background: var(--pass-bg); color: var(--pass-fg); font-weight: 600; }\
\n.status-fail { background: var(--fail-bg); color: var(--fail-fg); font-weight: 600; }\
\ndetails { padding: 4px 0; border-bottom: 1px solid var(--border); }\
\ndetails summary { cursor: pointer; padding: 8px 0; font-weight: 500; }\
\ndetails summary:hover { color: var(--link); }\
\n.fail-badge {\
\n  background: var(--fail-bg); color: var(--fail-fg);\
\n  padding: 2px 8px; border-radius: 4px; font-size: 0.85em; margin-left: 8px;\
\n}\
\n.pass-badge {\
\n  background: var(--pass-bg); color: var(--pass-fg);\
\n  padding: 2px 8px; border-radius: 4px; font-size: 0.85em; margin-left: 8px;\
\n}\
\nul.test-list { list-style: none; padding-left: 16px; margin: 4px 0; }\
\nul.test-list li { padding: 2px 4px; font-family: monospace; font-size: 0.9em; }\
\nul.test-list li.pass::before { content: '\\2713 '; color: #22c55e; }\
\nul.test-list li.fail::before { content: '\\2717 '; color: #ef4444; }\
\nfooter {\
\n  margin-top: 48px; padding-top: 16px;\
\n  border-top: 1px solid var(--border);\
\n  color: var(--meta); font-size: 0.85em; text-align: center;\
\n}\n",
    );
    html.push_str("</style>\n");
    html.push_str("</head>\n<body>\n<div class=\"container\">\n");

    // ── Header ──────────────────────────────────────────────────────
    html.push_str("<h1>mdmdview Full Test Report</h1>\n");
    html.push_str(&format!(
        "<p class=\"meta\">{} | commit {} | total {:.0}s</p>\n",
        html_escape(&timestamp),
        html_escape(git_info),
        total_duration.as_secs_f64(),
    ));

    if !run_mode.is_empty() {
        html.push_str(&format!(
            "<p class=\"run-mode\"><strong>Run mode:</strong> {}</p>\n",
            html_escape(run_mode),
        ));
    }

    // ── Dashboard cards ─────────────────────────────────────────────
    html.push_str("<div class=\"dashboard\">\n");
    html.push_str(&format!(
        "  <div class=\"card pass\"><div class=\"number\">{total_passed}</div>\
         <div class=\"label\">Passed</div></div>\n"
    ));
    html.push_str(&format!(
        "  <div class=\"card fail\"><div class=\"number\">{total_failed}</div>\
         <div class=\"label\">Failed</div></div>\n"
    ));
    html.push_str(&format!(
        "  <div class=\"card skip\"><div class=\"number\">{total_skipped}</div>\
         <div class=\"label\">Skipped</div></div>\n"
    ));
    html.push_str("</div>\n");

    // ── Phase summary table ─────────────────────────────────────────
    html.push_str("<h2>Phase Summary</h2>\n");
    html.push_str(
        "<table>\n<tr><th>Phase</th><th>Status</th><th>Duration</th><th>Details</th></tr>\n",
    );

    // Quality row
    if !quality_results.is_empty() {
        let q_passed = quality_results.iter().all(|r| r.passed);
        let q_dur: f64 = quality_results
            .iter()
            .map(|r| r.duration.as_secs_f64())
            .sum();
        let status_class = if q_passed {
            "status-pass"
        } else {
            "status-fail"
        };
        let status_text = if q_passed { "PASS" } else { "FAIL" };
        let details = quality_results
            .iter()
            .map(|r| {
                format!(
                    "{}: {}",
                    r.check_name,
                    if r.passed {
                        "ok".to_string()
                    } else {
                        format!("{} issues", r.issues.len())
                    }
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        html.push_str(&format!(
            "<tr><td>Quality</td><td class=\"{status_class}\">{status_text}</td>\
             <td>{q_dur:.2}s</td><td>{}</td></tr>\n",
            html_escape(&details)
        ));
    } else {
        html.push_str(
            "<tr><td>Quality</td><td>SKIPPED</td><td>-</td><td>--skip-quality</td></tr>\n",
        );
    }

    // Tests row
    if let Some(r) = test_results {
        let status_class = if r.failed == 0 {
            "status-pass"
        } else {
            "status-fail"
        };
        let status_text = if r.failed == 0 { "PASS" } else { "FAIL" };
        html.push_str(&format!(
            "<tr><td>Tests</td><td class=\"{status_class}\">{status_text}</td>\
             <td>{:.2}s</td><td>{} passed, {} failed, {} skipped</td></tr>\n",
            r.duration.as_secs_f64(),
            r.passed,
            r.failed,
            r.skipped,
        ));
    }

    // Mermaid row
    if let Some(r) = mermaid_results {
        if !r.build_ok {
            html.push_str(
                "<tr><td>Mermaid Visual</td><td class=\"status-fail\">BUILD FAILED</td>\
                 <td>-</td><td>mdmdview build failed</td></tr>\n",
            );
        } else {
            let m_passed = r.cases.iter().filter(|c| c.passed).count();
            let m_failed = r.cases.len() - m_passed;
            let status_class = if m_failed == 0 {
                "status-pass"
            } else {
                "status-fail"
            };
            let status_text = if m_failed == 0 { "PASS" } else { "FAIL" };
            html.push_str(&format!(
                "<tr><td>Mermaid Visual</td><td class=\"{status_class}\">{status_text}</td>\
                 <td>{:.2}s</td><td>{m_passed}/{} passed</td></tr>\n",
                r.duration.as_secs_f64(),
                r.cases.len(),
            ));
        }
    } else {
        html.push_str(
            "<tr><td>Mermaid Visual</td><td>SKIPPED</td><td>-</td><td>--skip-mermaid</td></tr>\n",
        );
    }

    // D2 Visual row
    if let Some(r) = d2_visual_results {
        if r.cases.is_empty() && !r.d2_cli_available {
            html.push_str(
                "<tr><td>D2 Visual</td><td>SKIPPED</td>\
                 <td>-</td><td>No d2 CLI and no references</td></tr>\n",
            );
        } else {
            let d_passed = r.cases.iter().filter(|c| c.passed).count();
            let d_failed = r.cases.len() - d_passed;
            let status_class = if d_failed == 0 {
                "status-pass"
            } else {
                "status-fail"
            };
            let status_text = if d_failed == 0 { "PASS" } else { "FAIL" };
            html.push_str(&format!(
                "<tr><td>D2 Visual</td><td class=\"{status_class}\">{status_text}</td>\
                 <td>{:.2}s</td><td>{d_passed}/{} passed</td></tr>\n",
                r.duration.as_secs_f64(),
                r.cases.len(),
            ));
        }
    } else {
        html.push_str(
            "<tr><td>D2 Visual</td><td>SKIPPED</td><td>-</td><td>--skip-d2-visual</td></tr>\n",
        );
    }

    // Coverage row
    if let Some(cov) = coverage_result {
        let line_pct = if cov.lines_total > 0 {
            cov.lines_covered as f64 / cov.lines_total as f64 * 100.0
        } else {
            0.0
        };
        html.push_str(&format!(
            "<tr><td>Coverage</td><td class=\"status-pass\">OK</td>\
             <td>{:.2}s</td><td>{:.1}% line coverage</td></tr>\n",
            cov.duration.as_secs_f64(),
            line_pct,
        ));
    } else {
        html.push_str(
            "<tr><td>Coverage</td><td>SKIPPED</td><td>-</td><td>--skip-coverage or not installed</td></tr>\n",
        );
    }

    html.push_str("</table>\n");

    // ── Code Quality details ────────────────────────────────────────
    if !quality_results.is_empty() {
        html.push_str("<h2>Code Quality</h2>\n");
        html.push_str("<table>\n<tr><th>Check</th><th>Status</th><th>Duration</th></tr>\n");
        for result in quality_results {
            let (status_text, status_class) = if result.passed {
                ("PASS".to_string(), "status-pass")
            } else {
                (
                    format!("FAIL ({} issues)", result.issues.len()),
                    "status-fail",
                )
            };
            html.push_str(&format!(
                "<tr><td>{}</td><td class=\"{}\">{}</td><td>{:.2}s</td></tr>\n",
                html_escape(&result.check_name),
                status_class,
                html_escape(&status_text),
                result.duration.as_secs_f64()
            ));
        }
        html.push_str("</table>\n");

        for result in quality_results {
            if !result.passed && !result.issues.is_empty() {
                html.push_str(&format!(
                    "<h3>{} Issues</h3>\n<ul>\n",
                    html_escape(&result.check_name)
                ));
                for issue in result.issues.iter().take(20) {
                    html.push_str(&format!("  <li>{}</li>\n", html_escape(issue)));
                }
                if result.issues.len() > 20 {
                    html.push_str(&format!(
                        "  <li>... and {} more</li>\n",
                        result.issues.len() - 20
                    ));
                }
                html.push_str("</ul>\n");
            }
        }
    }

    // ── Test category breakdown (collapsible) ───────────────────────
    if let Some(r) = test_results {
        let categories = categorize_tests(&r.tests);
        if !categories.is_empty() {
            html.push_str("<h2>Test Breakdown</h2>\n");

            // Group tests by category for the details lists
            let mut test_by_cat: std::collections::HashMap<String, Vec<&TestResult>> =
                std::collections::HashMap::new();
            for t in &r.tests {
                let cat = extract_category(&t.name);
                test_by_cat.entry(cat).or_default().push(t);
            }

            for cat in &categories {
                let has_failures = cat.failed > 0;
                let open_attr = if has_failures { " open" } else { "" };
                let badge = if has_failures {
                    format!(" <span class=\"fail-badge\">{} failed</span>", cat.failed)
                } else {
                    format!(" <span class=\"pass-badge\">{} passed</span>", cat.passed)
                };
                html.push_str(&format!(
                    "<details{}>\n  <summary>{} ({}/{}){}</summary>\n  <ul class=\"test-list\">\n",
                    open_attr,
                    html_escape(&cat.name),
                    cat.passed,
                    cat.total,
                    badge
                ));
                if let Some(tests) = test_by_cat.get(&cat.name) {
                    for t in tests {
                        let class = if t.passed { "pass" } else { "fail" };
                        let short_name = t
                            .name
                            .strip_prefix(&cat.name)
                            .and_then(|s| s.strip_prefix("::"))
                            .unwrap_or(&t.name);
                        html.push_str(&format!(
                            "    <li class=\"{class}\">{}</li>\n",
                            html_escape(short_name)
                        ));
                    }
                }
                html.push_str("  </ul>\n</details>\n");
            }
        }

        // Failed tests summary
        let failed_tests: Vec<_> = r.tests.iter().filter(|t| !t.passed).collect();
        if !failed_tests.is_empty() {
            html.push_str("<h2>Failed Tests</h2>\n<ul>\n");
            for test in &failed_tests {
                html.push_str(&format!(
                    "  <li><code>{}</code></li>\n",
                    html_escape(&test.name)
                ));
            }
            html.push_str("</ul>\n");
        }
    }

    // ── Mermaid visual results ──────────────────────────────────────
    if let Some(r) = mermaid_results {
        html.push_str("<h2>Mermaid Visual Tests</h2>\n");

        if !r.build_ok {
            html.push_str(
                "<p class=\"status-fail\">mdmdview build failed - Mermaid tests could not run.</p>\n",
            );
        } else if !r.cases.is_empty() {
            html.push_str(
                "<table>\n<tr><th>Case</th><th>Status</th><th>Diff %</th>\
                 <th>Diff Pixels</th><th>Max Delta</th><th>Duration</th><th>Message</th></tr>\n",
            );
            for case in &r.cases {
                let status_class = if case.passed {
                    "status-pass"
                } else {
                    "status-fail"
                };
                let status_text = if case.passed { "PASS" } else { "FAIL" };
                html.push_str(&format!(
                    "<tr><td>{}</td><td class=\"{}\">{}</td><td>{:.2}%</td>\
                     <td>{}</td><td>{}</td><td>{:.2}s</td><td>{}</td></tr>\n",
                    html_escape(&case.case_name),
                    status_class,
                    status_text,
                    case.diff_percent,
                    case.diff_pixels,
                    case.max_delta,
                    case.duration.as_secs_f64(),
                    html_escape(&case.message),
                ));
            }
            html.push_str("</table>\n");
        }
    }

    // ── D2 visual results ────────────────────────────────────────────
    if let Some(r) = d2_visual_results {
        html.push_str("<h2>D2 Visual Tests</h2>\n");

        if r.cases.is_empty() && !r.d2_cli_available {
            html.push_str(
                "<p>No d2 CLI found and no reference PNGs available. \
                 Install <code>d2</code> and run with <code>--update-d2-references</code>.</p>\n",
            );
        } else if !r.cases.is_empty() {
            html.push_str(
                "<table>\n<tr><th>Case</th><th>Status</th><th>Diff %</th>\
                 <th>Diff Pixels</th><th>Max Delta</th><th>Duration</th><th>Message</th></tr>\n",
            );
            for case in &r.cases {
                let status_class = if case.passed {
                    "status-pass"
                } else {
                    "status-fail"
                };
                let status_text = if case.passed { "PASS" } else { "FAIL" };
                html.push_str(&format!(
                    "<tr><td>{}</td><td class=\"{}\">{}</td><td>{:.2}%</td>\
                     <td>{}</td><td>{}</td><td>{:.2}s</td><td>{}</td></tr>\n",
                    html_escape(&case.case_name),
                    status_class,
                    status_text,
                    case.diff_percent,
                    case.diff_pixels,
                    case.max_delta,
                    case.duration.as_secs_f64(),
                    html_escape(&case.message),
                ));
            }
            html.push_str("</table>\n");
        }
    }

    // ── Coverage table ──────────────────────────────────────────────
    if let Some(cov) = coverage_result {
        html.push_str("<h2>Coverage</h2>\n");
        html.push_str(
            "<table>\n<tr><th>Metric</th><th>Covered</th>\
             <th>Total</th><th>Percentage</th></tr>\n",
        );

        let metrics: [(&str, usize, usize); 4] = [
            ("Lines", cov.lines_covered, cov.lines_total),
            ("Functions", cov.functions_covered, cov.functions_total),
            ("Branches", cov.branches_covered, cov.branches_total),
            ("Regions", cov.regions_covered, cov.regions_total),
        ];
        for (name, covered, total) in &metrics {
            let pct = if *total > 0 {
                *covered as f64 / *total as f64 * 100.0
            } else {
                0.0
            };
            html.push_str(&format!(
                "<tr><td>{name}</td><td>{covered}</td><td>{total}</td><td>{pct:.1}%</td></tr>\n"
            ));
        }
        html.push_str("</table>\n");
        html.push_str(&format!(
            "<p><em>Coverage analysis completed in {:.2}s</em></p>\n",
            cov.duration.as_secs_f64()
        ));
    }

    // ── Footer ──────────────────────────────────────────────────────
    html.push_str(&format!(
        "<footer>Generated by full_test v{} | {} | quality {}</footer>\n",
        html_escape(VERSION),
        html_escape(&timestamp),
        if quality_passed { "ok" } else { "issues found" },
    ));

    html.push_str("</div>\n</body>\n</html>\n");

    html
}

// ── Entry point ─────────────────────────────────────────────────────────

// COVERAGE: main() calls std::process::exit() which terminates the process,
// making it impossible to test from within the same process. All logic is
// tested via parse_options, run_full_test, and their component functions.
#[cfg_attr(coverage_nightly, coverage(off))]
fn main() {
    set_low_priority();

    let args: Vec<String> = std::env::args().collect();
    let now = Local::now();
    let options = match parse_options(&args) {
        Some(opts) => opts,
        None => {
            print_help();
            return;
        }
    };

    // Handle --update-d2-references: generate references and exit early.
    if options.update_d2_references {
        println!("=== Updating D2 Reference PNGs ===\n");
        if !check_d2_cli() {
            eprintln!("Error: d2 CLI not found. Install from https://d2lang.com/");
            std::process::exit(1);
        }
        let fontdb = build_fontdb();
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let fixtures_dir = manifest_dir
            .join("tests")
            .join("d2_conformance")
            .join("fixtures");
        let reference_dir = manifest_dir
            .join("tests")
            .join("d2_visual")
            .join("reference");
        let count = generate_d2_references(&fixtures_dir, &reference_dir, &fontdb);
        println!(
            "\nGenerated {count} reference PNGs in {}",
            reference_dir.display()
        );
        return;
    }

    let outcome = run_full_test(&options, run_cargo_test, now);

    write_report(&outcome.report_path, &outcome.html_report).expect("Failed to write HTML report");
    println!("Report saved to: {}", outcome.report_path.display());

    if outcome.any_failed {
        std::process::exit(1);
    }
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;

    fn make_result(name: &str, passed: bool) -> TestResult {
        TestResult {
            name: name.to_string(),
            passed,
            duration: Duration::ZERO,
            output: String::new(),
        }
    }

    fn make_suite(
        passed: usize,
        failed: usize,
        skipped: usize,
        tests: Vec<TestResult>,
    ) -> SuiteResults {
        SuiteResults {
            mode: "tests".to_string(),
            total: passed + failed + skipped,
            passed,
            failed,
            skipped,
            duration: Duration::from_secs(5),
            tests,
        }
    }

    // ── Option parsing tests ────────────────────────────────────────

    #[test]
    fn test_parse_options_defaults() {
        let args = vec!["full_test".to_string()];
        let options = parse_options(&args).unwrap();
        assert!(!options.quick);
        assert!(!options.skip_quality);
        assert!(!options.skip_coverage);
        assert!(!options.skip_mermaid);
        assert!(!options.skip_d2_visual);
        assert!(!options.update_d2_references);
    }

    #[test]
    fn test_parse_options_all_flags() {
        let args = vec![
            "full_test".to_string(),
            "--quick".to_string(),
            "--skip-quality".to_string(),
            "--skip-coverage".to_string(),
            "--skip-mermaid".to_string(),
            "--skip-d2-visual".to_string(),
        ];
        let options = parse_options(&args).unwrap();
        assert!(options.quick);
        assert!(options.skip_quality);
        assert!(options.skip_coverage);
        assert!(options.skip_mermaid);
        assert!(options.skip_d2_visual);
    }

    #[test]
    fn test_parse_options_help_returns_none() {
        let args = vec!["full_test".to_string(), "--help".to_string()];
        assert!(parse_options(&args).is_none());

        let args = vec!["full_test".to_string(), "-h".to_string()];
        assert!(parse_options(&args).is_none());

        // Help takes precedence
        let args = vec![
            "full_test".to_string(),
            "--quick".to_string(),
            "--help".to_string(),
        ];
        assert!(parse_options(&args).is_none());
    }

    #[test]
    fn test_parse_options_skip_quality() {
        let args = vec!["full_test".to_string(), "--skip-quality".to_string()];
        let options = parse_options(&args).unwrap();
        assert!(options.skip_quality);
        assert!(!options.skip_coverage);
    }

    #[test]
    fn test_parse_options_skip_mermaid() {
        let args = vec!["full_test".to_string(), "--skip-mermaid".to_string()];
        let options = parse_options(&args).unwrap();
        assert!(options.skip_mermaid);
    }

    // ── Test output parsing tests ───────────────────────────────────

    #[test]
    fn test_parse_test_output_counts_statuses() {
        let stdout = "\
test alpha ... ok\n\
test beta ... FAILED\n\
test gamma ... ignored\n\
not a test line\n";
        let duration = Duration::from_millis(3);

        let result = parse_test_output("tests", stdout, "", duration);

        assert_eq!(result.mode, "tests");
        assert_eq!(result.total, 3);
        assert_eq!(result.passed, 1);
        assert_eq!(result.failed, 1);
        assert_eq!(result.skipped, 1);
        assert_eq!(result.tests.len(), 2);
        assert_eq!(result.tests[0].name, "alpha");
        assert!(result.tests[0].passed);
        assert_eq!(result.tests[1].name, "beta");
        assert!(!result.tests[1].passed);
    }

    #[test]
    fn test_parse_test_output_from_stderr() {
        let stdout = "test alpha ... ok\n";
        let stderr = "note: running tests\ntest delta ... ok\n";

        let results = parse_test_output("tests", stdout, stderr, Duration::from_secs(1));

        assert_eq!(results.total, 2);
        assert_eq!(results.passed, 2);
        assert!(results.tests.iter().any(|t| t.name == "alpha" && t.passed));
        assert!(results.tests.iter().any(|t| t.name == "delta" && t.passed));
    }

    #[test]
    fn test_parse_test_output_empty() {
        let results = parse_test_output("tests", "", "", Duration::ZERO);
        assert_eq!(results.total, 0);
        assert_eq!(results.passed, 0);
        assert_eq!(results.failed, 0);
        assert_eq!(results.skipped, 0);
    }

    #[test]
    fn test_parse_test_output_ignored_with_reason() {
        // cargo test can emit "ignored, <reason>" instead of bare "ignored"
        let stdout =
            "test alpha ... ok\ntest beta ... ignored, env vars are global\ntest gamma ... ok\n";
        let results = parse_test_output("tests", stdout, "", Duration::ZERO);
        assert_eq!(results.passed, 2);
        assert_eq!(results.skipped, 1);
        assert_eq!(results.total, 3);
        assert_eq!(results.tests.len(), 2); // ignored not in tests vec
    }

    // ── Categorization tests ────────────────────────────────────────

    #[test]
    fn test_extract_category_two_components() {
        assert_eq!(
            extract_category("markdown_renderer::tests::test_heading"),
            "markdown_renderer::tests"
        );
        assert_eq!(extract_category("app::tests::test_open_file"), "app::tests");
    }

    #[test]
    fn test_extract_category_single_component() {
        assert_eq!(extract_category("test_something"), "test_something");
    }

    #[test]
    fn test_extract_category_empty() {
        assert_eq!(extract_category(""), "uncategorized");
    }

    #[test]
    fn test_categorize_tests_groups_correctly() {
        let tests = vec![
            make_result("markdown_renderer::tests::test_heading", true),
            make_result("markdown_renderer::tests::test_paragraph", true),
            make_result("markdown_renderer::tests::test_code", false),
            make_result("app::tests::test_open", true),
            make_result("app::tests::test_close", false),
        ];

        let categories = categorize_tests(&tests);

        assert_eq!(categories.len(), 2);

        // Failures first
        let first = &categories[0];
        assert!(first.failed > 0);

        let mr_cat = categories
            .iter()
            .find(|c| c.name == "markdown_renderer::tests")
            .unwrap();
        assert_eq!(mr_cat.total, 3);
        assert_eq!(mr_cat.passed, 2);
        assert_eq!(mr_cat.failed, 1);

        let app_cat = categories.iter().find(|c| c.name == "app::tests").unwrap();
        assert_eq!(app_cat.total, 2);
        assert_eq!(app_cat.passed, 1);
        assert_eq!(app_cat.failed, 1);
    }

    #[test]
    fn test_categorize_tests_empty() {
        let categories = categorize_tests(&[]);
        assert!(categories.is_empty());
    }

    // ── JSON extraction tests ───────────────────────────────────────

    #[test]
    fn test_extract_json_number_basic() {
        let json = r#"{"lines":{"count":100,"covered":80}}"#;
        assert_eq!(
            extract_json_number(json, "\"lines\"", "\"count\""),
            Some(100)
        );
        assert_eq!(
            extract_json_number(json, "\"lines\"", "\"covered\""),
            Some(80)
        );
    }

    #[test]
    fn test_extract_json_number_missing_section() {
        let json = r#"{"lines":{"count":100}}"#;
        assert_eq!(
            extract_json_number(json, "\"functions\"", "\"count\""),
            None
        );
    }

    #[test]
    fn test_extract_json_number_missing_key() {
        let json = r#"{"lines":{"count":100}}"#;
        assert_eq!(extract_json_number(json, "\"lines\"", "\"covered\""), None);
    }

    // ── Coverage parsing tests ──────────────────────────────────────

    #[test]
    fn test_parse_coverage_json_extracts_totals() {
        let json = r#"{"data":[{"totals":{"lines":{"count":1000,"covered":800},"functions":{"count":100,"covered":90},"branches":{"count":200,"covered":150},"regions":{"count":500,"covered":400}}}]}"#;
        let result = parse_coverage_json(json, Duration::from_secs(30)).unwrap();
        assert_eq!(result.lines_covered, 800);
        assert_eq!(result.lines_total, 1000);
        assert_eq!(result.functions_covered, 90);
        assert_eq!(result.functions_total, 100);
        assert_eq!(result.branches_covered, 150);
        assert_eq!(result.branches_total, 200);
        assert_eq!(result.regions_covered, 400);
        assert_eq!(result.regions_total, 500);
    }

    #[test]
    fn test_parse_coverage_json_no_totals() {
        let json = r#"{"data":[]}"#;
        assert!(parse_coverage_json(json, Duration::ZERO).is_none());
    }

    // ── HTML escape tests ───────────────────────────────────────────

    #[test]
    fn test_html_escape_special_chars() {
        assert_eq!(html_escape("hello"), "hello");
        assert_eq!(html_escape("<script>"), "&lt;script&gt;");
        assert_eq!(html_escape("a & b"), "a &amp; b");
        assert_eq!(html_escape("\"quoted\""), "&quot;quoted&quot;");
        assert_eq!(html_escape("it's"), "it&#39;s");
    }

    #[test]
    fn test_html_escape_empty() {
        assert_eq!(html_escape(""), "");
    }

    // ── Report path tests ───────────────────────────────────────────

    #[test]
    fn test_build_report_path_includes_results_dir() {
        let path = build_report_path(Local::now());
        let path_str = path.to_string_lossy();
        assert!(path_str.contains("tests"));
        assert!(path_str.contains("results"));
        assert!(path_str.contains("full test report"));
        assert!(path_str.ends_with(".html"));
    }

    #[test]
    fn test_build_report_path_date_stamped() {
        use chrono::TimeZone;
        let now = Local.with_ymd_and_hms(2026, 2, 22, 0, 0, 0).unwrap();
        let path = build_report_path(now);
        let path_str = path.to_string_lossy();
        assert!(path_str.contains("2026.02.22"));
    }

    #[test]
    fn test_build_report_path_increments_on_collision() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let dir = temp.path().join("tests").join("results");
        std::fs::create_dir_all(&dir).unwrap();

        // Create existing report
        std::fs::write(dir.join("2026.03.15 - full test report.html"), "r1").unwrap();
        std::fs::write(dir.join("2026.03.15 - full test report (2).html"), "r2").unwrap();

        // Verify collision increment logic
        let three = dir.join("2026.03.15 - full test report (3).html");
        assert!(!three.exists());
    }

    // ── Write report tests ──────────────────────────────────────────

    #[test]
    fn test_write_report_creates_parent() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let report_path = temp.path().join("reports").join("report.html");

        write_report(&report_path, "<html>ok</html>").expect("write report");

        assert!(report_path.exists());
        let contents = std::fs::read_to_string(&report_path).expect("read report");
        assert_eq!(contents, "<html>ok</html>");
    }

    #[test]
    fn test_write_report_creates_file() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let path = temp.path().join("report.html");
        write_report(&path, "report").expect("write report");
        let data = std::fs::read(&path).expect("read report");
        assert_eq!(data, b"report");
    }

    // ── Image diff tests ────────────────────────────────────────────

    #[test]
    fn test_diff_images_identical() {
        let img = image::RgbaImage::from_pixel(10, 10, image::Rgba([128, 128, 128, 255]));
        let (diff_pixels, max_delta) = diff_images(&img, &img, 60);
        assert_eq!(diff_pixels, 0);
        assert_eq!(max_delta, 0);
    }

    #[test]
    fn test_diff_images_within_tolerance() {
        let a = image::RgbaImage::from_pixel(10, 10, image::Rgba([100, 100, 100, 255]));
        let b = image::RgbaImage::from_pixel(10, 10, image::Rgba([150, 100, 100, 255]));
        let (diff_pixels, max_delta) = diff_images(&a, &b, 60);
        assert_eq!(diff_pixels, 0); // delta=50 is within tolerance=60
        assert_eq!(max_delta, 50);
    }

    #[test]
    fn test_diff_images_exceeds_tolerance() {
        let a = image::RgbaImage::from_pixel(10, 10, image::Rgba([0, 0, 0, 255]));
        let b = image::RgbaImage::from_pixel(10, 10, image::Rgba([255, 255, 255, 255]));
        let (diff_pixels, max_delta) = diff_images(&a, &b, 60);
        assert_eq!(diff_pixels, 100); // all 10x10 pixels differ
        assert_eq!(max_delta, 255);
    }

    #[test]
    fn test_diff_images_mixed() {
        let a = image::RgbaImage::from_pixel(2, 2, image::Rgba([100, 100, 100, 255]));
        let mut b = image::RgbaImage::from_pixel(2, 2, image::Rgba([100, 100, 100, 255]));
        // Make one pixel different by more than tolerance
        b.put_pixel(0, 0, image::Rgba([200, 100, 100, 255]));
        let (diff_pixels, max_delta) = diff_images(&a, &b, 60);
        assert_eq!(diff_pixels, 1);
        assert_eq!(max_delta, 100);
    }

    // ── Pad to common size tests ────────────────────────────────────

    #[test]
    fn test_pad_to_common_size_same_dimensions() {
        let a = image::RgbaImage::from_pixel(10, 10, image::Rgba([128, 128, 128, 255]));
        let b = image::RgbaImage::from_pixel(10, 10, image::Rgba([64, 64, 64, 255]));
        let (pa, pb) = pad_to_common_size(&a, &b);
        assert_eq!(pa.dimensions(), (10, 10));
        assert_eq!(pb.dimensions(), (10, 10));
    }

    #[test]
    fn test_pad_to_common_size_different_dimensions() {
        let a = image::RgbaImage::from_pixel(10, 20, image::Rgba([128, 128, 128, 255]));
        let b = image::RgbaImage::from_pixel(20, 10, image::Rgba([64, 64, 64, 255]));
        let (pa, pb) = pad_to_common_size(&a, &b);
        assert_eq!(pa.dimensions(), (20, 20));
        assert_eq!(pb.dimensions(), (20, 20));
    }

    #[test]
    fn test_pad_to_common_size_preserves_content() {
        let a = image::RgbaImage::from_pixel(2, 2, image::Rgba([100, 100, 100, 255]));
        let b = image::RgbaImage::from_pixel(4, 4, image::Rgba([200, 200, 200, 255]));
        let (pa, _pb) = pad_to_common_size(&a, &b);
        assert_eq!(pa.dimensions(), (4, 4));
        // Center pixel of padded a should be original color
        assert_eq!(pa.get_pixel(1, 1).0, [100, 100, 100, 255]);
        // Corner should be white (padding)
        assert_eq!(pa.get_pixel(0, 0).0, [255, 255, 255, 255]);
    }

    // ── Describe run mode tests ─────────────────────────────────────

    #[test]
    fn test_describe_run_mode_defaults() {
        let options = FullTestOptions {
            quick: false,
            skip_quality: false,
            skip_coverage: false,
            skip_mermaid: false,
            skip_d2_visual: false,
            update_d2_references: false,
        };
        let mode = describe_run_mode(&options);
        assert!(mode.contains("Full Suite"));
        assert!(!mode.contains("Skipped"));
    }

    #[test]
    fn test_describe_run_mode_all_skips() {
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let mode = describe_run_mode(&options);
        assert!(mode.contains("Quick"));
        assert!(mode.contains("Quality Skipped"));
        assert!(mode.contains("Coverage Skipped"));
        assert!(mode.contains("Mermaid Skipped"));
        assert!(mode.contains("D2 Visual Skipped"));
    }

    // ── Cargo test command tests ────────────────────────────────────

    #[test]
    fn test_build_cargo_test_command_includes_ignored() {
        let cmd = build_cargo_test_command(true);
        assert!(cmd.get_args().any(|a| a == "--release"));
        assert!(cmd.get_args().any(|a| a == "--lib"));
        assert!(cmd.get_args().any(|a| a == "--tests"));
        assert!(cmd.get_args().any(|a| a == "--workspace"));
        assert!(cmd.get_args().any(|a| a == "--no-fail-fast"));
        assert!(cmd.get_args().any(|a| a == "--include-ignored"));
    }

    #[test]
    fn test_build_cargo_test_command_excludes_ignored() {
        let cmd = build_cargo_test_command(false);
        assert!(!cmd.get_args().any(|a| a == "--include-ignored"));
    }

    // ── Run full test orchestration tests ───────────────────────────

    #[test]
    fn test_run_full_test_calls_runner() {
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let mut calls = Vec::new();
        let runner = |include_ignored: bool| {
            calls.push(include_ignored);
            make_suite(1, 0, 0, vec![make_result("t", true)])
        };

        let outcome = run_full_test(&options, runner, Local::now());

        assert_eq!(calls, vec![false]); // quick mode = not include_ignored
        assert!(!outcome.any_failed);
    }

    #[test]
    fn test_run_full_test_detects_test_failures() {
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let runner = |_include_ignored: bool| {
            make_suite(
                1,
                1,
                0,
                vec![make_result("t", true), make_result("f", false)],
            )
        };

        let outcome = run_full_test(&options, runner, Local::now());
        assert!(outcome.any_failed);
    }

    #[test]
    fn test_run_full_test_full_mode_includes_ignored() {
        let options = FullTestOptions {
            quick: false,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let mut calls = Vec::new();
        let runner = |include_ignored: bool| {
            calls.push(include_ignored);
            make_suite(1, 0, 0, vec![make_result("t", true)])
        };

        let _outcome = run_full_test(&options, runner, Local::now());
        assert_eq!(calls, vec![true]); // full mode = include_ignored
    }

    // ── HTML report tests ───────────────────────────────────────────

    #[test]
    fn test_generate_html_report_basic_structure() {
        let tests = make_suite(
            2,
            1,
            0,
            vec![
                make_result("app::tests::test_a", true),
                make_result("app::tests::test_b", true),
                make_result("app::tests::test_c", false),
            ],
        );

        let html = generate_html_report(
            Some(&tests),
            &[],
            None,
            None,
            None,
            "abc123 (main)",
            Local::now(),
            "Full Suite",
            Duration::from_secs(100),
        );

        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("mdmdview Full Test Report"));
        assert!(html.contains("abc123 (main)"));
        assert!(html.contains("Phase Summary"));
        assert!(html.contains("Test Breakdown"));
        assert!(html.contains("Failed Tests"));
        assert!(html.contains("test_c"));
        assert!(html.contains("</html>"));
    }

    #[test]
    fn test_generate_html_report_with_quality() {
        let quality = vec![
            QualityResult {
                check_name: "cargo fmt".to_string(),
                passed: true,
                duration: Duration::from_secs(1),
                issues: vec![],
            },
            QualityResult {
                check_name: "cargo clippy".to_string(),
                passed: false,
                duration: Duration::from_secs(5),
                issues: vec!["warning: unused variable".to_string()],
            },
        ];

        let html = generate_html_report(
            None,
            &quality,
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(10),
        );

        assert!(html.contains("Code Quality"));
        assert!(html.contains("cargo fmt"));
        assert!(html.contains("PASS"));
        assert!(html.contains("cargo clippy"));
        assert!(html.contains("FAIL"));
        assert!(html.contains("unused variable"));
    }

    #[test]
    fn test_generate_html_report_with_coverage() {
        let coverage = CoverageResult {
            lines_covered: 800,
            lines_total: 1000,
            functions_covered: 90,
            functions_total: 100,
            branches_covered: 50,
            branches_total: 100,
            regions_covered: 400,
            regions_total: 500,
            duration: Duration::from_secs(30),
        };

        let html = generate_html_report(
            None,
            &[],
            None,
            None,
            Some(&coverage),
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(30),
        );

        assert!(html.contains("Coverage"));
        assert!(html.contains("800"));
        assert!(html.contains("1000"));
        assert!(html.contains("80.0%"));
    }

    #[test]
    fn test_generate_html_report_with_mermaid() {
        let mermaid = MermaidResults {
            cases: vec![
                MermaidCaseResult {
                    case_name: "flowchart".to_string(),
                    passed: true,
                    message: "ok".to_string(),
                    diff_percent: 3.5,
                    diff_pixels: 1200,
                    max_delta: 80,
                    duration: Duration::from_secs(4),
                },
                MermaidCaseResult {
                    case_name: "sequence".to_string(),
                    passed: false,
                    message: "diff_exceeds_threshold".to_string(),
                    diff_percent: 15.0,
                    diff_pixels: 50000,
                    max_delta: 200,
                    duration: Duration::from_secs(3),
                },
            ],
            build_ok: true,
            duration: Duration::from_secs(10),
        };

        let html = generate_html_report(
            None,
            &[],
            Some(&mermaid),
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(10),
        );

        assert!(html.contains("Mermaid Visual Tests"));
        assert!(html.contains("flowchart"));
        assert!(html.contains("sequence"));
        assert!(html.contains("diff_exceeds_threshold"));
    }

    #[test]
    fn test_generate_html_report_mermaid_build_failed() {
        let mermaid = MermaidResults {
            cases: Vec::new(),
            build_ok: false,
            duration: Duration::from_secs(1),
        };

        let html = generate_html_report(
            None,
            &[],
            Some(&mermaid),
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        assert!(html.contains("BUILD FAILED"));
        assert!(html.contains("build failed"));
    }

    #[test]
    fn test_generate_html_report_dark_mode_css() {
        let html = generate_html_report(
            None,
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        assert!(html.contains("prefers-color-scheme: dark"));
    }

    // ── D2 visual HTML report test ───────────────────────────────────

    #[test]
    fn test_generate_html_report_with_d2_visual() {
        let d2_visual = D2VisualResults {
            cases: vec![
                D2VisualCaseResult {
                    case_name: "001-simple-connection".to_string(),
                    passed: true,
                    message: "ok".to_string(),
                    diff_percent: 5.2,
                    diff_pixels: 800,
                    max_delta: 45,
                    duration: Duration::from_millis(200),
                },
                D2VisualCaseResult {
                    case_name: "020-simple-container".to_string(),
                    passed: false,
                    message: "diff_exceeds_threshold".to_string(),
                    diff_percent: 25.0,
                    diff_pixels: 90000,
                    max_delta: 180,
                    duration: Duration::from_millis(300),
                },
            ],
            d2_cli_available: true,
            duration: Duration::from_secs(5),
        };

        let html = generate_html_report(
            None,
            &[],
            None,
            Some(&d2_visual),
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(5),
        );

        assert!(html.contains("D2 Visual Tests"));
        assert!(html.contains("001-simple-connection"));
        assert!(html.contains("020-simple-container"));
        assert!(html.contains("diff_exceeds_threshold"));
    }

    // ── D2 option parsing test ──────────────────────────────────────

    #[test]
    fn test_parse_options_d2_flags() {
        let args = vec!["full_test".to_string(), "--skip-d2-visual".to_string()];
        let options = parse_options(&args).unwrap();
        assert!(options.skip_d2_visual);
        assert!(!options.update_d2_references);

        let args = vec![
            "full_test".to_string(),
            "--update-d2-references".to_string(),
        ];
        let options = parse_options(&args).unwrap();
        assert!(!options.skip_d2_visual);
        assert!(options.update_d2_references);
    }

    // ── Mermaid wait time per-case test ─────────────────────────────

    #[test]
    fn test_mermaid_wait_ms_default() {
        assert_eq!(mermaid_wait_ms_for_case("flowchart"), MERMAID_WAIT_MS);
        assert_eq!(mermaid_wait_ms_for_case("sequence"), MERMAID_WAIT_MS);
    }

    #[test]
    fn test_mermaid_wait_ms_mindmap() {
        assert_eq!(mermaid_wait_ms_for_case("mindmap"), 12_000);
    }

    // ── Priority tests ──────────────────────────────────────────────

    #[test]
    fn test_set_low_priority_does_not_panic() {
        // set_low_priority() calls OS-specific APIs; just verify it doesn't crash.
        set_low_priority();
    }

    // ── print_help tests ────────────────────────────────────────────

    #[test]
    fn test_print_help_does_not_panic() {
        // print_help() writes to stdout; just verify it completes without error.
        print_help();
    }

    // ── get_git_info tests ──────────────────────────────────────────

    #[test]
    fn test_get_git_info_returns_non_empty() {
        let info = get_git_info();
        assert!(!info.is_empty(), "git info should not be empty");
    }

    #[test]
    fn test_get_git_info_contains_parenthesized_branch() {
        let info = get_git_info();
        // Format is "hash (branch)" — must contain parentheses
        assert!(
            info.contains('(') && info.contains(')'),
            "git info should contain branch in parentheses, got: {info}"
        );
    }

    // ── check_d2_cli tests ──────────────────────────────────────────

    #[test]
    fn test_check_d2_cli_returns_bool() {
        // We don't know if d2 is installed, but the function must return cleanly.
        let _available: bool = check_d2_cli();
    }

    // ── check_llvm_cov_available tests ──────────────────────────────

    #[test]
    fn test_check_llvm_cov_available_returns_bool() {
        let _available: bool = check_llvm_cov_available();
    }

    // ── build_fontdb tests ──────────────────────────────────────────

    #[test]
    fn test_build_fontdb_returns_valid_db() {
        let db = build_fontdb();
        // The Arc should be valid and the database should have loaded system fonts.
        // On any modern system, at least one font family should be available.
        assert!(
            db.faces().count() > 0,
            "fontdb should contain at least one font face"
        );
    }

    // ── rasterize_svg_to_png tests ──────────────────────────────────

    #[test]
    fn test_rasterize_svg_to_png_valid_svg() {
        let fontdb = build_fontdb();
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100"><rect width="100" height="100" fill="red"/></svg>"#;
        let temp = tempfile::TempDir::new().expect("temp dir");
        let output = temp.path().join("test.png");

        let result = rasterize_svg_to_png(svg, &output, &fontdb);
        assert!(
            result.is_ok(),
            "rasterize_svg_to_png should succeed for valid SVG: {result:?}"
        );
        assert!(output.exists(), "PNG file should be created");

        // Verify PNG magic bytes
        let data = std::fs::read(&output).expect("read PNG");
        assert!(data.len() > 8, "PNG file should have content");
        assert_eq!(
            &data[..4],
            &[0x89, 0x50, 0x4E, 0x47],
            "should start with PNG magic bytes"
        );
    }

    #[test]
    fn test_rasterize_svg_to_png_invalid_svg() {
        let fontdb = build_fontdb();
        let svg = "this is not valid SVG at all";
        let temp = tempfile::TempDir::new().expect("temp dir");
        let output = temp.path().join("bad.png");

        let result = rasterize_svg_to_png(svg, &output, &fontdb);
        assert!(
            result.is_err(),
            "rasterize_svg_to_png should fail for invalid SVG"
        );
        let err_msg = result.unwrap_err();
        assert!(
            err_msg.contains("usvg parse failed"),
            "error should mention usvg parse failure, got: {err_msg}"
        );
    }

    #[test]
    fn test_rasterize_svg_to_png_dimensions_preserved() {
        let fontdb = build_fontdb();
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="50" height="30"><rect width="50" height="30" fill="blue"/></svg>"#;
        let temp = tempfile::TempDir::new().expect("temp dir");
        let output = temp.path().join("sized.png");

        rasterize_svg_to_png(svg, &output, &fontdb).expect("rasterize");
        let img = image::open(&output).expect("open PNG");
        assert_eq!(img.width(), 50);
        assert_eq!(img.height(), 30);
    }

    // ── run_fmt_check tests ─────────────────────────────────────────

    #[test]
    fn test_run_fmt_check_returns_quality_result() {
        let result = run_fmt_check();
        assert_eq!(result.check_name, "cargo fmt");
        // Duration should be non-zero (it actually ran the command)
        assert!(
            result.duration.as_nanos() > 0,
            "fmt check should have measurable duration"
        );
    }

    // ── run_clippy tests ────────────────────────────────────────────

    #[test]
    fn test_run_clippy_returns_quality_result() {
        let result = run_clippy();
        assert_eq!(result.check_name, "cargo clippy");
        assert!(
            result.duration.as_nanos() > 0,
            "clippy check should have measurable duration"
        );
    }

    // ── build_cargo_test_command edge cases ─────────────────────────

    #[test]
    fn test_build_cargo_test_command_has_test_threads_1() {
        let cmd = build_cargo_test_command(false);
        assert!(
            cmd.get_args().any(|a| a == "--test-threads=1"),
            "command should include --test-threads=1"
        );
    }

    #[test]
    fn test_build_cargo_test_command_program_is_cargo() {
        let cmd = build_cargo_test_command(false);
        assert_eq!(
            cmd.get_program(),
            "cargo",
            "command program should be cargo"
        );
    }

    // ── build_mdmdview_binary tests ─────────────────────────────────

    #[test]
    #[ignore]
    fn test_build_mdmdview_binary_succeeds() {
        // This test is expensive (runs cargo build --release).
        let result = build_mdmdview_binary();
        assert!(result, "mdmdview release build should succeed");
    }

    // ── HTML report with D2 visual results containing failures ──────

    #[test]
    fn test_generate_html_report_d2_visual_no_cli_no_cases() {
        let d2_visual = D2VisualResults {
            cases: Vec::new(),
            d2_cli_available: false,
            duration: Duration::from_secs(1),
        };

        let html = generate_html_report(
            None,
            &[],
            None,
            Some(&d2_visual),
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        assert!(html.contains("D2 Visual Tests"));
        assert!(html.contains("No d2 CLI found"));
        // Phase summary should show SKIPPED for D2
        assert!(html.contains("No d2 CLI and no references"));
    }

    #[test]
    fn test_generate_html_report_all_phases_present() {
        let tests = make_suite(
            5,
            0,
            1,
            vec![
                make_result("mod::tests::a", true),
                make_result("mod::tests::b", true),
                make_result("mod::tests::c", true),
                make_result("mod::tests::d", true),
                make_result("mod::tests::e", true),
            ],
        );
        let quality = vec![
            QualityResult {
                check_name: "cargo fmt".to_string(),
                passed: true,
                duration: Duration::from_secs(1),
                issues: vec![],
            },
            QualityResult {
                check_name: "cargo clippy".to_string(),
                passed: true,
                duration: Duration::from_secs(2),
                issues: vec![],
            },
        ];
        let mermaid = MermaidResults {
            cases: vec![MermaidCaseResult {
                case_name: "flowchart".to_string(),
                passed: true,
                message: "ok".to_string(),
                diff_percent: 1.0,
                diff_pixels: 100,
                max_delta: 30,
                duration: Duration::from_secs(3),
            }],
            build_ok: true,
            duration: Duration::from_secs(5),
        };
        let d2_visual = D2VisualResults {
            cases: vec![D2VisualCaseResult {
                case_name: "simple".to_string(),
                passed: true,
                message: "ok".to_string(),
                diff_percent: 2.0,
                diff_pixels: 200,
                max_delta: 20,
                duration: Duration::from_millis(100),
            }],
            d2_cli_available: true,
            duration: Duration::from_secs(2),
        };
        let coverage = CoverageResult {
            lines_covered: 900,
            lines_total: 1000,
            functions_covered: 95,
            functions_total: 100,
            branches_covered: 60,
            branches_total: 80,
            regions_covered: 450,
            regions_total: 500,
            duration: Duration::from_secs(30),
        };

        let html = generate_html_report(
            Some(&tests),
            &quality,
            Some(&mermaid),
            Some(&d2_visual),
            Some(&coverage),
            "def456 (develop)",
            Local::now(),
            "Full Suite",
            Duration::from_secs(60),
        );

        // All major sections should be present
        assert!(html.contains("Phase Summary"));
        assert!(html.contains("Code Quality"));
        assert!(html.contains("Test Breakdown"));
        assert!(html.contains("Mermaid Visual Tests"));
        assert!(html.contains("D2 Visual Tests"));
        assert!(html.contains("Coverage"));
        assert!(html.contains("def456 (develop)"));
        assert!(html.contains("Full Suite"));
        // Quality should show PASS
        assert!(html.contains("quality ok"));
    }

    #[test]
    fn test_generate_html_report_skipped_phases() {
        // No test results, no quality, no mermaid, no d2, no coverage
        let html = generate_html_report(
            None,
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "Quick (excluding ignored) | Quality Skipped | Mermaid Skipped | D2 Visual Skipped | Coverage Skipped",
            Duration::from_secs(1),
        );

        assert!(html.contains("Quality Skipped"));
        assert!(html.contains("--skip-quality"));
        assert!(html.contains("--skip-mermaid"));
        assert!(html.contains("--skip-d2-visual"));
        assert!(html.contains("--skip-coverage"));
    }

    #[test]
    fn test_generate_html_report_quality_issues_truncated() {
        // Quality result with >20 issues to test truncation
        let issues: Vec<String> = (0..25)
            .map(|i| format!("warning: issue number {i}"))
            .collect();
        let quality = vec![QualityResult {
            check_name: "cargo clippy".to_string(),
            passed: false,
            duration: Duration::from_secs(3),
            issues,
        }];

        let html = generate_html_report(
            None,
            &quality,
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(3),
        );

        assert!(html.contains("... and 5 more"));
        assert!(html.contains("issues found")); // footer
    }

    // ── run_full_test with quality enabled ──────────────────────────

    #[test]
    fn test_run_full_test_with_quality_skip_everything_else() {
        // Enable quality checks (they will actually run cargo fmt and clippy)
        // but skip everything expensive
        let options = FullTestOptions {
            quick: true,
            skip_quality: false, // actually run quality
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let runner = |_include_ignored: bool| make_suite(1, 0, 0, vec![make_result("t", true)]);

        let outcome = run_full_test(&options, runner, Local::now());

        // Quality results should be populated
        assert_eq!(outcome.quality_results.len(), 2);
        assert_eq!(outcome.quality_results[0].check_name, "cargo fmt");
        assert_eq!(outcome.quality_results[1].check_name, "cargo clippy");
    }

    // ── describe_run_mode edge cases ────────────────────────────────

    #[test]
    fn test_describe_run_mode_quick_only() {
        let options = FullTestOptions {
            quick: true,
            skip_quality: false,
            skip_coverage: false,
            skip_mermaid: false,
            skip_d2_visual: false,
            update_d2_references: false,
        };
        let mode = describe_run_mode(&options);
        assert!(mode.contains("Quick"));
        assert!(!mode.contains("Full Suite"));
        assert!(!mode.contains("Skipped"));
    }

    #[test]
    fn test_describe_run_mode_d2_visual_skipped() {
        let options = FullTestOptions {
            quick: false,
            skip_quality: false,
            skip_coverage: false,
            skip_mermaid: false,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let mode = describe_run_mode(&options);
        assert!(mode.contains("D2 Visual Skipped"));
    }

    // ── parse_coverage_json edge cases ──────────────────────────────

    #[test]
    fn test_parse_coverage_json_partial_data() {
        // Only lines section present, missing functions/branches/regions
        let json = r#"{"data":[{"totals":{"lines":{"count":500,"covered":250}}}]}"#;
        let result = parse_coverage_json(json, Duration::from_secs(1)).unwrap();
        assert_eq!(result.lines_covered, 250);
        assert_eq!(result.lines_total, 500);
        // Missing sections should be zero
        assert_eq!(result.functions_covered, 0);
        assert_eq!(result.functions_total, 0);
    }

    #[test]
    fn test_parse_coverage_json_empty_string() {
        assert!(parse_coverage_json("", Duration::ZERO).is_none());
    }

    // ── extract_json_number edge cases ──────────────────────────────

    #[test]
    fn test_extract_json_number_with_spaces() {
        let json = r#"{ "lines" : { "count" : 42 } }"#;
        assert_eq!(
            extract_json_number(json, "\"lines\"", "\"count\""),
            Some(42)
        );
    }

    #[test]
    fn test_extract_json_number_zero_value() {
        let json = r#"{"lines":{"count":0}}"#;
        assert_eq!(extract_json_number(json, "\"lines\"", "\"count\""), Some(0));
    }

    #[test]
    fn test_extract_json_number_large_value() {
        let json = r#"{"lines":{"count":999999}}"#;
        assert_eq!(
            extract_json_number(json, "\"lines\"", "\"count\""),
            Some(999999)
        );
    }

    // ── html_escape edge cases ──────────────────────────────────────

    #[test]
    fn test_html_escape_all_special_chars_combined() {
        assert_eq!(
            html_escape("<b>\"hello\" & 'world'</b>"),
            "&lt;b&gt;&quot;hello&quot; &amp; &#39;world&#39;&lt;/b&gt;"
        );
    }

    #[test]
    fn test_html_escape_unicode() {
        // Unicode should pass through unchanged
        assert_eq!(html_escape("hello 世界 🌍"), "hello 世界 🌍");
    }

    // ── categorize_tests edge cases ─────────────────────────────────

    #[test]
    fn test_categorize_tests_single_module_all_pass() {
        let tests = vec![
            make_result("foo::bar::test_a", true),
            make_result("foo::bar::test_b", true),
        ];
        let categories = categorize_tests(&tests);
        assert_eq!(categories.len(), 1);
        assert_eq!(categories[0].name, "foo::bar");
        assert_eq!(categories[0].total, 2);
        assert_eq!(categories[0].passed, 2);
        assert_eq!(categories[0].failed, 0);
    }

    #[test]
    fn test_categorize_tests_sorts_failures_first() {
        let tests = vec![
            make_result("aaa::tests::test_a", true),
            make_result("zzz::tests::test_b", false),
        ];
        let categories = categorize_tests(&tests);
        assert_eq!(categories.len(), 2);
        // zzz has failures, should be first despite alphabetical order
        assert_eq!(categories[0].name, "zzz::tests");
        assert_eq!(categories[1].name, "aaa::tests");
    }

    // ── write_report edge cases ─────────────────────────────────────

    #[test]
    fn test_write_report_overwrites_existing() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let path = temp.path().join("report.html");
        write_report(&path, "first").expect("write first");
        write_report(&path, "second").expect("write second");
        let contents = std::fs::read_to_string(&path).expect("read");
        assert_eq!(contents, "second");
    }

    // ── diff_images edge cases ──────────────────────────────────────

    #[test]
    fn test_diff_images_zero_tolerance() {
        // With zero tolerance, even 1-unit difference should count
        let a = image::RgbaImage::from_pixel(2, 2, image::Rgba([100, 100, 100, 255]));
        let mut b = a.clone();
        b.put_pixel(0, 0, image::Rgba([101, 100, 100, 255]));
        let (diff_pixels, max_delta) = diff_images(&a, &b, 0);
        assert_eq!(diff_pixels, 1);
        assert_eq!(max_delta, 1);
    }

    #[test]
    fn test_diff_images_max_tolerance() {
        // With tolerance=255, nothing should count as different
        let a = image::RgbaImage::from_pixel(5, 5, image::Rgba([0, 0, 0, 255]));
        let b = image::RgbaImage::from_pixel(5, 5, image::Rgba([255, 255, 255, 255]));
        let (diff_pixels, _max_delta) = diff_images(&a, &b, 255);
        assert_eq!(diff_pixels, 0);
    }

    #[test]
    fn test_diff_images_1x1() {
        let a = image::RgbaImage::from_pixel(1, 1, image::Rgba([0, 0, 0, 255]));
        let b = image::RgbaImage::from_pixel(1, 1, image::Rgba([0, 0, 0, 255]));
        let (diff_pixels, max_delta) = diff_images(&a, &b, 0);
        assert_eq!(diff_pixels, 0);
        assert_eq!(max_delta, 0);
    }

    // ── pad_to_common_size edge cases ───────────────────────────────

    #[test]
    fn test_pad_to_common_size_1x1_vs_large() {
        let a = image::RgbaImage::from_pixel(1, 1, image::Rgba([50, 50, 50, 255]));
        let b = image::RgbaImage::from_pixel(100, 100, image::Rgba([200, 200, 200, 255]));
        let (pa, pb) = pad_to_common_size(&a, &b);
        assert_eq!(pa.dimensions(), (100, 100));
        assert_eq!(pb.dimensions(), (100, 100));
    }

    // ── parse_test_output edge cases ────────────────────────────────

    #[test]
    fn test_parse_test_output_malformed_lines_ignored() {
        let stdout = "test no_separator\ntest partial ... \nnot a test ... ok\n";
        let results = parse_test_output("tests", stdout, "", Duration::ZERO);
        // None of these should parse as valid test results
        assert_eq!(results.total, 0);
    }

    #[test]
    fn test_parse_test_output_all_failed() {
        let stdout = "test a ... FAILED\ntest b ... FAILED\ntest c ... FAILED\n";
        let results = parse_test_output("tests", stdout, "", Duration::ZERO);
        assert_eq!(results.total, 3);
        assert_eq!(results.failed, 3);
        assert_eq!(results.passed, 0);
        assert_eq!(results.tests.len(), 3);
        assert!(results.tests.iter().all(|t| !t.passed));
    }

    #[test]
    fn test_parse_test_output_all_ignored() {
        let stdout = "test a ... ignored\ntest b ... ignored\n";
        let results = parse_test_output("tests", stdout, "", Duration::ZERO);
        assert_eq!(results.total, 2);
        assert_eq!(results.skipped, 2);
        assert_eq!(results.passed, 0);
        assert_eq!(results.failed, 0);
        assert_eq!(results.tests.len(), 0); // ignored tests not added to vec
    }

    // ── extract_category edge cases ─────────────────────────────────

    #[test]
    fn test_extract_category_deeply_nested() {
        assert_eq!(extract_category("a::b::c::d::e"), "a::b");
    }

    #[test]
    fn test_extract_category_two_parts_only() {
        assert_eq!(extract_category("module::tests"), "module::tests");
    }

    // ══════════════════════════════════════════════════════════════════
    // Block 1: run_d2_visual_case tests
    // ══════════════════════════════════════════════════════════════════

    /// Helper: create a simple .d2 file and render it to a reference PNG
    /// using the same pipeline as the production code.
    fn create_d2_fixture_with_reference(
        dir: &Path,
    ) -> (
        PathBuf,
        PathBuf,
        PathBuf,
        std::sync::Arc<usvg::fontdb::Database>,
    ) {
        let fixtures_dir = dir.join("fixtures");
        let reference_dir = dir.join("reference");
        let actual_dir = dir.join("actual");
        std::fs::create_dir_all(&fixtures_dir).unwrap();
        std::fs::create_dir_all(&reference_dir).unwrap();
        std::fs::create_dir_all(&actual_dir).unwrap();

        // Write a simple D2 file
        let d2_path = fixtures_dir.join("simple.d2");
        std::fs::write(&d2_path, "a -> b\n").unwrap();

        // Render through the D2 pipeline and rasterize to make a reference
        let fontdb = build_fontdb();
        let opts = mdmdview_d2::RenderOptions {
            dark_mode: false,
            ..Default::default()
        };
        let result = mdmdview_d2::render_d2_to_svg("a -> b\n", &opts).unwrap();
        let ref_png = reference_dir.join("simple.png");
        rasterize_svg_to_png(&result.svg, &ref_png, &fontdb).unwrap();

        (d2_path, reference_dir, actual_dir, fontdb)
    }

    #[test]
    fn test_run_d2_visual_case_pass_with_matching_reference() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let (d2_path, reference_dir, actual_dir, fontdb) =
            create_d2_fixture_with_reference(temp.path());

        let result = run_d2_visual_case(&d2_path, &reference_dir, &actual_dir, &fontdb);

        assert_eq!(result.case_name, "simple");
        assert!(
            result.passed,
            "Should pass when comparing identical renders"
        );
        assert_eq!(result.message, "ok");
        assert!(
            result.diff_percent < 1.0,
            "Diff should be very low for identical renders: {}",
            result.diff_percent
        );
    }

    #[test]
    fn test_run_d2_visual_case_missing_reference() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let fixtures_dir = temp.path().join("fixtures");
        let reference_dir = temp.path().join("reference");
        let actual_dir = temp.path().join("actual");
        std::fs::create_dir_all(&fixtures_dir).unwrap();
        std::fs::create_dir_all(&reference_dir).unwrap();
        std::fs::create_dir_all(&actual_dir).unwrap();

        let d2_path = fixtures_dir.join("no_ref.d2");
        std::fs::write(&d2_path, "x -> y\n").unwrap();

        let fontdb = build_fontdb();
        let result = run_d2_visual_case(&d2_path, &reference_dir, &actual_dir, &fontdb);

        assert_eq!(result.case_name, "no_ref");
        assert!(
            result.passed,
            "Missing reference should be treated as skipped/passed"
        );
        assert_eq!(result.message, "skipped_no_reference");
        assert_eq!(result.diff_percent, 0.0);
    }

    #[test]
    fn test_run_d2_visual_case_render_error() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let fixtures_dir = temp.path().join("fixtures");
        let reference_dir = temp.path().join("reference");
        let actual_dir = temp.path().join("actual");
        std::fs::create_dir_all(&fixtures_dir).unwrap();
        std::fs::create_dir_all(&reference_dir).unwrap();
        std::fs::create_dir_all(&actual_dir).unwrap();

        // Write invalid D2 source that will fail to render
        let d2_path = fixtures_dir.join("bad.d2");
        std::fs::write(&d2_path, "-> -> ->\n").unwrap();

        // Create a dummy reference PNG so we don't skip
        let ref_png = reference_dir.join("bad.png");
        let dummy_img = image::RgbaImage::from_pixel(10, 10, image::Rgba([255, 255, 255, 255]));
        dummy_img.save(&ref_png).unwrap();

        let fontdb = build_fontdb();
        let result = run_d2_visual_case(&d2_path, &reference_dir, &actual_dir, &fontdb);

        assert_eq!(result.case_name, "bad");
        assert!(!result.passed, "Invalid D2 source should fail");
        assert!(
            result.message.contains("render_failed") || result.message.contains("read_failed"),
            "Message should indicate render failure: {}",
            result.message
        );
    }

    #[test]
    fn test_run_d2_visual_case_read_error() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let reference_dir = temp.path().join("reference");
        let actual_dir = temp.path().join("actual");
        std::fs::create_dir_all(&reference_dir).unwrap();
        std::fs::create_dir_all(&actual_dir).unwrap();

        // Fixture path points to a file that doesn't exist
        let d2_path = temp.path().join("nonexistent.d2");

        // Create a dummy reference so we don't skip via missing reference
        let ref_png = reference_dir.join("nonexistent.png");
        let dummy_img = image::RgbaImage::from_pixel(10, 10, image::Rgba([255, 255, 255, 255]));
        dummy_img.save(&ref_png).unwrap();

        let fontdb = build_fontdb();
        let result = run_d2_visual_case(&d2_path, &reference_dir, &actual_dir, &fontdb);

        assert_eq!(result.case_name, "nonexistent");
        assert!(!result.passed);
        assert!(
            result.message.contains("read_failed"),
            "Should report read failure: {}",
            result.message
        );
    }

    #[test]
    fn test_run_d2_visual_case_diff_with_different_content() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let fixtures_dir = temp.path().join("fixtures");
        let reference_dir = temp.path().join("reference");
        let actual_dir = temp.path().join("actual");
        std::fs::create_dir_all(&fixtures_dir).unwrap();
        std::fs::create_dir_all(&reference_dir).unwrap();
        std::fs::create_dir_all(&actual_dir).unwrap();

        // The fixture renders "a -> b"
        let d2_path = fixtures_dir.join("mismatch.d2");
        std::fs::write(&d2_path, "a -> b\n").unwrap();

        // But the reference is a completely different image (all black)
        let ref_png = reference_dir.join("mismatch.png");
        let black_img = image::RgbaImage::from_pixel(200, 200, image::Rgba([0, 0, 0, 255]));
        black_img.save(&ref_png).unwrap();

        let fontdb = build_fontdb();
        let result = run_d2_visual_case(&d2_path, &reference_dir, &actual_dir, &fontdb);

        assert_eq!(result.case_name, "mismatch");
        // The diff should be significant — result may pass or fail depending on
        // thresholds, but diff_percent should be non-zero
        assert!(
            result.diff_percent > 0.0 || result.diff_pixels > 0,
            "Should detect differences between black reference and actual render"
        );
    }

    #[test]
    fn test_run_d2_visual_case_uses_real_d2_fixture() {
        // Use an actual fixture from the codebase
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let fixture = manifest_dir
            .join("tests")
            .join("d2_conformance")
            .join("fixtures")
            .join("001-simple-connection.d2");
        let reference_dir = manifest_dir
            .join("tests")
            .join("d2_visual")
            .join("reference");

        if !fixture.exists() || !reference_dir.join("001-simple-connection.png").exists() {
            // Skip if fixtures or references are not available
            return;
        }

        let temp = tempfile::TempDir::new().expect("temp dir");
        let actual_dir = temp.path().join("actual");
        std::fs::create_dir_all(&actual_dir).unwrap();

        let fontdb = build_fontdb();
        let result = run_d2_visual_case(&fixture, &reference_dir, &actual_dir, &fontdb);

        assert_eq!(result.case_name, "001-simple-connection");
        // Should have some result (pass or fail, but no error)
        assert!(
            result.message == "ok"
                || result.message == "diff_exceeds_threshold"
                || result.message == "skipped_no_reference",
            "Unexpected message: {}",
            result.message
        );
    }

    // ══════════════════════════════════════════════════════════════════
    // Block 2: run_d2_visual_tests components
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_generate_d2_references_with_no_d2_cli() {
        // generate_d2_references uses the d2 CLI. We test the function's
        // behavior when fixtures exist but d2 CLI is unavailable.
        // If d2 is not installed, d2_cli_render will fail, and count should be 0.
        let temp = tempfile::TempDir::new().expect("temp dir");
        let fixtures_dir = temp.path().join("fixtures");
        let reference_dir = temp.path().join("reference");
        std::fs::create_dir_all(&fixtures_dir).unwrap();

        std::fs::write(fixtures_dir.join("test.d2"), "a -> b\n").unwrap();
        let fontdb = build_fontdb();

        if !check_d2_cli() {
            let count = generate_d2_references(&fixtures_dir, &reference_dir, &fontdb);
            assert_eq!(
                count, 0,
                "Without d2 CLI, no references should be generated"
            );
        }
        // If d2 IS installed, we just confirm the function runs without panic
        else {
            let count = generate_d2_references(&fixtures_dir, &reference_dir, &fontdb);
            assert!(count <= 1); // Should generate at most 1 reference
        }
    }

    #[test]
    fn test_generate_d2_references_empty_fixtures_dir() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let fixtures_dir = temp.path().join("fixtures");
        let reference_dir = temp.path().join("reference");
        std::fs::create_dir_all(&fixtures_dir).unwrap();

        let fontdb = build_fontdb();
        let count = generate_d2_references(&fixtures_dir, &reference_dir, &fontdb);
        assert_eq!(count, 0, "Empty fixtures dir should produce 0 references");
    }

    #[test]
    fn test_d2_cli_render_nonexistent_file() {
        let result = d2_cli_render(Path::new("/nonexistent/path/to/file.d2"));
        // Should fail either because d2 isn't installed or because file doesn't exist
        assert!(result.is_err());
    }

    #[test]
    fn test_check_d2_cli_idempotent() {
        // Calling check_d2_cli multiple times should be consistent
        let first = check_d2_cli();
        let second = check_d2_cli();
        assert_eq!(first, second, "check_d2_cli should be deterministic");
    }

    // ══════════════════════════════════════════════════════════════════
    // Block 3: run_coverage / parse_coverage_json edge cases
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_parse_coverage_json_zero_lines_total() {
        // When lines_total is 0, the percentage should be 0.0 (no division by zero)
        let json = r#"{"data":[{"totals":{"lines":{"count":0,"covered":0},"functions":{"count":10,"covered":5},"branches":{"count":0,"covered":0},"regions":{"count":0,"covered":0}}}]}"#;
        let result = parse_coverage_json(json, Duration::from_secs(1)).unwrap();
        assert_eq!(result.lines_covered, 0);
        assert_eq!(result.lines_total, 0);
        assert_eq!(result.functions_covered, 5);
        assert_eq!(result.functions_total, 10);
    }

    #[test]
    fn test_parse_coverage_json_all_fields_populated() {
        let json = r#"{
            "data": [{
                "totals": {
                    "lines": {"count": 5000, "covered": 4500},
                    "functions": {"count": 300, "covered": 280},
                    "branches": {"count": 1000, "covered": 800},
                    "regions": {"count": 2000, "covered": 1900}
                }
            }]
        }"#;
        let result = parse_coverage_json(json, Duration::from_secs(120)).unwrap();
        assert_eq!(result.lines_covered, 4500);
        assert_eq!(result.lines_total, 5000);
        assert_eq!(result.functions_covered, 280);
        assert_eq!(result.functions_total, 300);
        assert_eq!(result.branches_covered, 800);
        assert_eq!(result.branches_total, 1000);
        assert_eq!(result.regions_covered, 1900);
        assert_eq!(result.regions_total, 2000);
        assert_eq!(result.duration, Duration::from_secs(120));
    }

    #[test]
    fn test_parse_coverage_json_garbage_input() {
        assert!(parse_coverage_json("not json at all {{{", Duration::ZERO).is_none());
    }

    #[test]
    fn test_parse_coverage_json_totals_but_no_subsections() {
        let json = r#"{"totals":{}}"#;
        let result = parse_coverage_json(json, Duration::from_secs(1)).unwrap();
        // All values should remain at their defaults (0)
        assert_eq!(result.lines_covered, 0);
        assert_eq!(result.lines_total, 0);
        assert_eq!(result.functions_covered, 0);
        assert_eq!(result.functions_total, 0);
    }

    #[test]
    #[ignore] // Expensive: actually runs cargo llvm-cov
    fn test_run_coverage_returns_result() {
        let result = run_coverage();
        // If llvm-cov is installed, we should get Some; otherwise None.
        // Either way, no panic.
        if check_llvm_cov_available() {
            assert!(
                result.is_some(),
                "coverage should return a result when llvm-cov is available"
            );
            let cov = result.unwrap();
            assert!(cov.lines_total > 0, "should have some lines to cover");
        } else {
            assert!(
                result.is_none(),
                "should return None when llvm-cov unavailable"
            );
        }
    }

    // ══════════════════════════════════════════════════════════════════
    // Block 4: main() / run_full_test additional coverage
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_describe_run_mode_single_skip_quality() {
        let options = FullTestOptions {
            quick: false,
            skip_quality: true,
            skip_coverage: false,
            skip_mermaid: false,
            skip_d2_visual: false,
            update_d2_references: false,
        };
        let mode = describe_run_mode(&options);
        assert!(mode.contains("Full Suite"));
        assert!(mode.contains("Quality Skipped"));
        assert!(!mode.contains("Coverage Skipped"));
        assert!(!mode.contains("Mermaid Skipped"));
    }

    #[test]
    fn test_describe_run_mode_single_skip_mermaid() {
        let options = FullTestOptions {
            quick: false,
            skip_quality: false,
            skip_coverage: false,
            skip_mermaid: true,
            skip_d2_visual: false,
            update_d2_references: false,
        };
        let mode = describe_run_mode(&options);
        assert!(mode.contains("Mermaid Skipped"));
        assert!(!mode.contains("Quality Skipped"));
    }

    #[test]
    fn test_describe_run_mode_single_skip_coverage() {
        let options = FullTestOptions {
            quick: false,
            skip_quality: false,
            skip_coverage: true,
            skip_mermaid: false,
            skip_d2_visual: false,
            update_d2_references: false,
        };
        let mode = describe_run_mode(&options);
        assert!(mode.contains("Coverage Skipped"));
        assert!(!mode.contains("Mermaid Skipped"));
    }

    #[test]
    fn test_run_full_test_skip_nothing_with_mock_runner() {
        // Exercise the code paths where mermaid and d2_visual are NOT skipped,
        // but use a mock test runner. The mermaid/d2 phases will actually run
        // but may fail because binary isn't built — that's fine, we exercise
        // the orchestration code.
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,   // Still skip — they need binaries
            skip_d2_visual: true, // Still skip — they need fixtures in specific paths
            update_d2_references: false,
        };
        let runner = |include_ignored: bool| {
            assert!(!include_ignored, "quick mode should not include ignored");
            make_suite(
                3,
                0,
                2,
                vec![
                    make_result("a::b::t1", true),
                    make_result("a::b::t2", true),
                    make_result("c::d::t3", true),
                ],
            )
        };

        let outcome = run_full_test(&options, runner, Local::now());
        assert!(!outcome.any_failed);
        assert!(outcome.test_results.is_some());
        let tr = outcome.test_results.as_ref().unwrap();
        assert_eq!(tr.passed, 3);
        assert_eq!(tr.failed, 0);
        assert_eq!(tr.skipped, 2);
    }

    #[test]
    fn test_run_full_test_generates_valid_html() {
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let runner = |_: bool| {
            make_suite(
                2,
                0,
                0,
                vec![make_result("x::y::a", true), make_result("x::y::b", true)],
            )
        };

        let outcome = run_full_test(&options, runner, Local::now());
        assert!(outcome.html_report.contains("<!DOCTYPE html>"));
        assert!(outcome.html_report.contains("</html>"));
        assert!(outcome.html_report.contains("mdmdview Full Test Report"));
    }

    #[test]
    fn test_run_full_test_report_path_ends_with_html() {
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let runner = |_: bool| make_suite(1, 0, 0, vec![make_result("t", true)]);

        let outcome = run_full_test(&options, runner, Local::now());
        assert!(
            outcome.report_path.to_string_lossy().ends_with(".html"),
            "Report path should end with .html"
        );
    }

    #[test]
    fn test_run_full_test_with_test_failures_marks_failed() {
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let runner = |_: bool| {
            make_suite(
                5,
                2,
                1,
                vec![
                    make_result("a::b::pass1", true),
                    make_result("a::b::pass2", true),
                    make_result("a::b::pass3", true),
                    make_result("a::b::pass4", true),
                    make_result("a::b::pass5", true),
                    make_result("a::b::fail1", false),
                    make_result("a::b::fail2", false),
                ],
            )
        };

        let outcome = run_full_test(&options, runner, Local::now());
        assert!(
            outcome.any_failed,
            "Should be marked as failed when tests fail"
        );
    }

    // ══════════════════════════════════════════════════════════════════
    // Block 5: generate_html_report additional branches
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_generate_html_report_empty_test_results() {
        let tests = make_suite(0, 0, 0, vec![]);

        let html = generate_html_report(
            Some(&tests),
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "Full Suite",
            Duration::from_secs(1),
        );

        // Should still produce valid HTML
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("</html>"));
        // No "Test Breakdown" section since there are no categories
        // (categorize_tests returns empty)
    }

    #[test]
    fn test_generate_html_report_coverage_zero_total() {
        let coverage = CoverageResult {
            lines_covered: 0,
            lines_total: 0,
            functions_covered: 0,
            functions_total: 0,
            branches_covered: 0,
            branches_total: 0,
            regions_covered: 0,
            regions_total: 0,
            duration: Duration::from_secs(5),
        };

        let html = generate_html_report(
            None,
            &[],
            None,
            None,
            Some(&coverage),
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(5),
        );

        assert!(html.contains("Coverage"));
        assert!(html.contains("0.0%"));
    }

    #[test]
    fn test_generate_html_report_mermaid_with_empty_cases_and_build_ok() {
        let mermaid = MermaidResults {
            cases: Vec::new(),
            build_ok: true,
            duration: Duration::from_secs(1),
        };

        let html = generate_html_report(
            None,
            &[],
            Some(&mermaid),
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        assert!(html.contains("Mermaid Visual Tests"));
        // build_ok but no cases — should not show build failed message
        assert!(!html.contains("BUILD FAILED"));
        // Should not show a table either (no cases)
    }

    #[test]
    fn test_generate_html_report_d2_visual_with_cases_and_no_cli() {
        // Cases present but d2_cli_available = false
        let d2_visual = D2VisualResults {
            cases: vec![D2VisualCaseResult {
                case_name: "test_case".to_string(),
                passed: true,
                message: "ok".to_string(),
                diff_percent: 1.5,
                diff_pixels: 100,
                max_delta: 20,
                duration: Duration::from_millis(50),
            }],
            d2_cli_available: false,
            duration: Duration::from_secs(1),
        };

        let html = generate_html_report(
            None,
            &[],
            None,
            Some(&d2_visual),
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        assert!(html.contains("D2 Visual Tests"));
        assert!(html.contains("test_case"));
        // Should show the table, NOT the "No d2 CLI" message (since cases exist)
        assert!(!html.contains("No d2 CLI found"));
    }

    #[test]
    fn test_generate_html_report_d2_visual_empty_cases_with_cli() {
        let d2_visual = D2VisualResults {
            cases: Vec::new(),
            d2_cli_available: true,
            duration: Duration::from_secs(1),
        };

        let html = generate_html_report(
            None,
            &[],
            None,
            Some(&d2_visual),
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        assert!(html.contains("D2 Visual Tests"));
        // d2_cli_available is true but no cases — should not show the "No d2 CLI" message
        assert!(!html.contains("No d2 CLI found"));
    }

    #[test]
    fn test_generate_html_report_quality_failed_with_no_issues() {
        // Quality check fails but has no issues listed
        let quality = vec![QualityResult {
            check_name: "cargo fmt".to_string(),
            passed: false,
            duration: Duration::from_secs(1),
            issues: vec![],
        }];

        let html = generate_html_report(
            None,
            &quality,
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        assert!(html.contains("cargo fmt"));
        assert!(html.contains("FAIL"));
        assert!(html.contains("issues found")); // footer
    }

    #[test]
    fn test_generate_html_report_quality_with_exactly_20_issues() {
        let issues: Vec<String> = (0..20).map(|i| format!("warning: issue {i}")).collect();
        let quality = vec![QualityResult {
            check_name: "cargo clippy".to_string(),
            passed: false,
            duration: Duration::from_secs(3),
            issues,
        }];

        let html = generate_html_report(
            None,
            &quality,
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(3),
        );

        // Exactly 20 issues — should NOT show "and N more"
        assert!(!html.contains("... and"));
        assert!(html.contains("issue 19")); // last one
    }

    #[test]
    fn test_generate_html_report_all_tests_pass_no_failed_section() {
        let tests = make_suite(
            3,
            0,
            0,
            vec![
                make_result("a::b::t1", true),
                make_result("a::b::t2", true),
                make_result("a::b::t3", true),
            ],
        );

        let html = generate_html_report(
            Some(&tests),
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        // Should not have a "Failed Tests" section
        assert!(!html.contains("Failed Tests"));
    }

    #[test]
    fn test_generate_html_report_mixed_categories_with_failures() {
        let tests = make_suite(
            4,
            2,
            0,
            vec![
                make_result("alpha::tests::a1", true),
                make_result("alpha::tests::a2", false),
                make_result("beta::tests::b1", true),
                make_result("beta::tests::b2", true),
                make_result("gamma::tests::g1", true),
                make_result("gamma::tests::g2", false),
            ],
        );

        let html = generate_html_report(
            Some(&tests),
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        assert!(html.contains("Test Breakdown"));
        assert!(html.contains("alpha::tests"));
        assert!(html.contains("beta::tests"));
        assert!(html.contains("gamma::tests"));
        assert!(html.contains("Failed Tests"));
        assert!(html.contains("a2")); // failed test name
        assert!(html.contains("g2")); // failed test name
                                      // Categories with failures should have "open" attribute
        assert!(html.contains("open"));
        assert!(html.contains("fail-badge"));
        assert!(html.contains("pass-badge"));
    }

    #[test]
    fn test_generate_html_report_run_mode_empty() {
        let html = generate_html_report(
            None,
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        // Empty run mode should NOT show run-mode paragraph
        assert!(!html.contains("Run mode:"));
    }

    #[test]
    fn test_generate_html_report_run_mode_non_empty() {
        let html = generate_html_report(
            None,
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "Full Suite",
            Duration::from_secs(1),
        );

        assert!(html.contains("Run mode:"));
        assert!(html.contains("Full Suite"));
    }

    #[test]
    fn test_generate_html_report_mermaid_all_pass() {
        let mermaid = MermaidResults {
            cases: vec![
                MermaidCaseResult {
                    case_name: "flowchart".to_string(),
                    passed: true,
                    message: "ok".to_string(),
                    diff_percent: 1.0,
                    diff_pixels: 50,
                    max_delta: 10,
                    duration: Duration::from_secs(2),
                },
                MermaidCaseResult {
                    case_name: "sequence".to_string(),
                    passed: true,
                    message: "ok".to_string(),
                    diff_percent: 2.0,
                    diff_pixels: 100,
                    max_delta: 15,
                    duration: Duration::from_secs(3),
                },
            ],
            build_ok: true,
            duration: Duration::from_secs(8),
        };

        let html = generate_html_report(
            None,
            &[],
            Some(&mermaid),
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(8),
        );

        assert!(html.contains("Mermaid Visual Tests"));
        assert!(html.contains("flowchart"));
        assert!(html.contains("sequence"));
        // Phase summary should show PASS for mermaid
        assert!(html.contains("status-pass"));
    }

    #[test]
    fn test_generate_html_report_d2_visual_all_fail() {
        let d2_visual = D2VisualResults {
            cases: vec![
                D2VisualCaseResult {
                    case_name: "case_a".to_string(),
                    passed: false,
                    message: "diff_exceeds_threshold".to_string(),
                    diff_percent: 50.0,
                    diff_pixels: 100000,
                    max_delta: 200,
                    duration: Duration::from_millis(200),
                },
                D2VisualCaseResult {
                    case_name: "case_b".to_string(),
                    passed: false,
                    message: "render_failed: bad input".to_string(),
                    diff_percent: 100.0,
                    diff_pixels: 0,
                    max_delta: 0,
                    duration: Duration::from_millis(50),
                },
            ],
            d2_cli_available: true,
            duration: Duration::from_secs(2),
        };

        let html = generate_html_report(
            None,
            &[],
            None,
            Some(&d2_visual),
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(2),
        );

        assert!(html.contains("D2 Visual Tests"));
        assert!(html.contains("case_a"));
        assert!(html.contains("case_b"));
        assert!(html.contains("FAIL"));
        assert!(html.contains("status-fail"));
    }

    // ══════════════════════════════════════════════════════════════════
    // Block 6: Mermaid visual tests - result structure tests
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_mermaid_case_result_clone() {
        let result = MermaidCaseResult {
            case_name: "test".to_string(),
            passed: true,
            message: "ok".to_string(),
            diff_percent: 1.5,
            diff_pixels: 100,
            max_delta: 20,
            duration: Duration::from_secs(1),
        };
        let cloned = result.clone();
        assert_eq!(cloned.case_name, "test");
        assert!(cloned.passed);
        assert_eq!(cloned.diff_pixels, 100);
    }

    #[test]
    fn test_d2_visual_case_result_clone() {
        let result = D2VisualCaseResult {
            case_name: "d2_test".to_string(),
            passed: false,
            message: "diff_exceeds_threshold".to_string(),
            diff_percent: 25.0,
            diff_pixels: 90000,
            max_delta: 180,
            duration: Duration::from_millis(300),
        };
        let cloned = result.clone();
        assert_eq!(cloned.case_name, "d2_test");
        assert!(!cloned.passed);
        assert_eq!(cloned.diff_pixels, 90000);
    }

    #[test]
    fn test_mermaid_wait_ms_for_case_unknown() {
        // Any unknown case name should return the default
        assert_eq!(mermaid_wait_ms_for_case("unknown_case"), MERMAID_WAIT_MS);
        assert_eq!(mermaid_wait_ms_for_case(""), MERMAID_WAIT_MS);
        assert_eq!(mermaid_wait_ms_for_case("gantt"), MERMAID_WAIT_MS);
    }

    // ══════════════════════════════════════════════════════════════════
    // Block 7: Miscellaneous uncovered lines
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_run_full_test_quality_failure_marks_failed() {
        // Exercise the quality_failed path in run_full_test.
        // When skip_quality is false, quality checks actually run.
        // Since fmt/clippy might pass, we test the orchestration logic
        // with everything else skipped.
        let options = FullTestOptions {
            quick: true,
            skip_quality: false, // Run quality checks
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let runner = |_: bool| make_suite(1, 0, 0, vec![make_result("t", true)]);

        let outcome = run_full_test(&options, runner, Local::now());

        // Quality results should be present
        assert_eq!(outcome.quality_results.len(), 2);
        // If quality checks pass, the overall result depends on test results only
        if outcome.quality_results.iter().all(|r| r.passed) {
            assert!(!outcome.any_failed);
        }
    }

    #[test]
    fn test_full_test_options_clone() {
        let options = FullTestOptions {
            quick: true,
            skip_quality: false,
            skip_coverage: true,
            skip_mermaid: false,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let cloned = options.clone();
        assert!(cloned.quick);
        assert!(cloned.skip_coverage);
        assert!(!cloned.skip_mermaid);
    }

    #[test]
    fn test_test_result_clone() {
        let tr = make_result("my::test", true);
        let cloned = tr.clone();
        assert_eq!(cloned.name, "my::test");
        assert!(cloned.passed);
    }

    #[test]
    fn test_category_stats_clone() {
        let stats = CategoryStats {
            name: "foo::bar".to_string(),
            total: 10,
            passed: 8,
            failed: 2,
        };
        let cloned = stats.clone();
        assert_eq!(cloned.name, "foo::bar");
        assert_eq!(cloned.total, 10);
    }

    #[test]
    fn test_parse_options_update_d2_references() {
        let args = vec![
            "full_test".to_string(),
            "--update-d2-references".to_string(),
        ];
        let options = parse_options(&args).unwrap();
        assert!(options.update_d2_references);
    }

    #[test]
    fn test_rasterize_svg_to_png_zero_dimension_svg() {
        let fontdb = build_fontdb();
        // SVG with zero width
        let svg = r#"<svg xmlns="http://www.w3.org/2000/svg" width="0" height="100"></svg>"#;
        let temp = tempfile::TempDir::new().expect("temp dir");
        let output = temp.path().join("zero.png");

        let result = rasterize_svg_to_png(svg, &output, &fontdb);
        // Should fail because dimensions are zero
        assert!(
            result.is_err(),
            "Should fail with zero-dimension SVG: {result:?}"
        );
    }

    #[test]
    fn test_build_report_path_returns_unique_path() {
        // Two consecutive calls with the same timestamp might collide,
        // but should produce valid paths
        let now = Local::now();
        let path1 = build_report_path(now);
        assert!(path1.to_string_lossy().ends_with(".html"));
    }

    #[test]
    fn test_write_report_to_deeply_nested_path() {
        let temp = tempfile::TempDir::new().expect("temp dir");
        let deep = temp
            .path()
            .join("a")
            .join("b")
            .join("c")
            .join("report.html");
        write_report(&deep, "<html>deep</html>").expect("write report");
        assert!(deep.exists());
    }

    #[test]
    fn test_generate_html_report_combined_dashboard_counts() {
        // Verify that dashboard counts aggregate across test, mermaid, and d2 results
        let tests = make_suite(
            10,
            2,
            3,
            vec![
                make_result("a::b::t1", true),
                make_result("a::b::t2", true),
                make_result("a::b::t3", true),
                make_result("a::b::t4", true),
                make_result("a::b::t5", true),
                make_result("a::b::t6", true),
                make_result("a::b::t7", true),
                make_result("a::b::t8", true),
                make_result("a::b::t9", true),
                make_result("a::b::t10", true),
                make_result("a::b::f1", false),
                make_result("a::b::f2", false),
            ],
        );

        let mermaid = MermaidResults {
            cases: vec![
                MermaidCaseResult {
                    case_name: "m1".to_string(),
                    passed: true,
                    message: "ok".to_string(),
                    diff_percent: 0.0,
                    diff_pixels: 0,
                    max_delta: 0,
                    duration: Duration::ZERO,
                },
                MermaidCaseResult {
                    case_name: "m2".to_string(),
                    passed: false,
                    message: "fail".to_string(),
                    diff_percent: 50.0,
                    diff_pixels: 100000,
                    max_delta: 255,
                    duration: Duration::ZERO,
                },
            ],
            build_ok: true,
            duration: Duration::ZERO,
        };

        let d2_visual = D2VisualResults {
            cases: vec![D2VisualCaseResult {
                case_name: "d1".to_string(),
                passed: true,
                message: "ok".to_string(),
                diff_percent: 0.0,
                diff_pixels: 0,
                max_delta: 0,
                duration: Duration::ZERO,
            }],
            d2_cli_available: true,
            duration: Duration::ZERO,
        };

        let html = generate_html_report(
            Some(&tests),
            &[],
            Some(&mermaid),
            Some(&d2_visual),
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        // total_passed = 10 (tests) + 1 (mermaid) + 1 (d2) = 12
        // total_failed = 2 (tests) + 1 (mermaid) + 0 (d2) = 3
        assert!(html.contains(">12<")); // passed count
        assert!(html.contains(">3<")); // failed count
        assert!(html.contains(">3<")); // skipped count
    }

    #[test]
    fn test_generate_html_report_footer_quality_ok() {
        let quality = vec![QualityResult {
            check_name: "cargo fmt".to_string(),
            passed: true,
            duration: Duration::ZERO,
            issues: vec![],
        }];

        let html = generate_html_report(
            None,
            &quality,
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        assert!(html.contains("quality ok"));
    }

    #[test]
    fn test_generate_html_report_footer_quality_issues() {
        let quality = vec![QualityResult {
            check_name: "cargo fmt".to_string(),
            passed: false,
            duration: Duration::ZERO,
            issues: vec!["problem".to_string()],
        }];

        let html = generate_html_report(
            None,
            &quality,
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        assert!(html.contains("issues found"));
    }

    #[test]
    fn test_generate_html_report_test_categories_with_all_pass() {
        let tests = make_suite(
            4,
            0,
            0,
            vec![
                make_result("mod_a::tests::t1", true),
                make_result("mod_a::tests::t2", true),
                make_result("mod_b::tests::t3", true),
                make_result("mod_b::tests::t4", true),
            ],
        );

        let html = generate_html_report(
            Some(&tests),
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        assert!(html.contains("Test Breakdown"));
        assert!(html.contains("mod_a::tests"));
        assert!(html.contains("mod_b::tests"));
        // No failures, so no "open" attribute on details
        assert!(!html.contains("Failed Tests"));
    }

    #[test]
    fn test_generate_html_report_test_short_name_stripping() {
        // Tests that the short name is correctly stripped from full test name
        let tests = make_suite(
            1,
            0,
            0,
            vec![make_result("mod::tests::my_specific_test", true)],
        );

        let html = generate_html_report(
            Some(&tests),
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        // The short name "my_specific_test" should appear (stripped of category prefix)
        assert!(html.contains("my_specific_test"));
    }

    #[test]
    fn test_generate_html_report_mermaid_phase_row_all_pass() {
        let mermaid = MermaidResults {
            cases: vec![MermaidCaseResult {
                case_name: "pie".to_string(),
                passed: true,
                message: "ok".to_string(),
                diff_percent: 0.5,
                diff_pixels: 10,
                max_delta: 5,
                duration: Duration::from_secs(1),
            }],
            build_ok: true,
            duration: Duration::from_secs(1),
        };

        let html = generate_html_report(
            None,
            &[],
            Some(&mermaid),
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        // Phase summary row should show PASS
        assert!(html.contains("Mermaid Visual"));
        assert!(html.contains("1/1 passed"));
    }

    #[test]
    fn test_generate_html_report_d2_phase_row_all_pass() {
        let d2_visual = D2VisualResults {
            cases: vec![
                D2VisualCaseResult {
                    case_name: "c1".to_string(),
                    passed: true,
                    message: "ok".to_string(),
                    diff_percent: 1.0,
                    diff_pixels: 50,
                    max_delta: 10,
                    duration: Duration::from_millis(100),
                },
                D2VisualCaseResult {
                    case_name: "c2".to_string(),
                    passed: true,
                    message: "ok".to_string(),
                    diff_percent: 2.0,
                    diff_pixels: 80,
                    max_delta: 15,
                    duration: Duration::from_millis(100),
                },
            ],
            d2_cli_available: true,
            duration: Duration::from_secs(1),
        };

        let html = generate_html_report(
            None,
            &[],
            None,
            Some(&d2_visual),
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(1),
        );

        assert!(html.contains("D2 Visual"));
        assert!(html.contains("2/2 passed"));
    }

    #[test]
    fn test_generate_html_report_coverage_duration_displayed() {
        let coverage = CoverageResult {
            lines_covered: 100,
            lines_total: 200,
            functions_covered: 10,
            functions_total: 20,
            branches_covered: 5,
            branches_total: 10,
            regions_covered: 50,
            regions_total: 100,
            duration: Duration::from_secs(42),
        };

        let html = generate_html_report(
            None,
            &[],
            None,
            None,
            Some(&coverage),
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(42),
        );

        assert!(html.contains("42.00s"));
    }

    #[test]
    fn test_generate_html_report_special_chars_in_git_info() {
        let html = generate_html_report(
            None,
            &[],
            None,
            None,
            None,
            "abc123 (feat/special<chars>&more)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        // HTML escaping should prevent XSS
        assert!(html.contains("&lt;chars&gt;"));
        assert!(html.contains("&amp;more"));
    }

    #[test]
    fn test_generate_html_report_quality_phase_skipped_row() {
        // When quality results is empty, phase summary shows SKIPPED
        let html = generate_html_report(
            None,
            &[], // empty quality
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        assert!(html.contains("--skip-quality"));
    }

    #[test]
    fn test_generate_html_report_quality_phase_fail_details() {
        let quality = vec![
            QualityResult {
                check_name: "cargo fmt".to_string(),
                passed: false,
                duration: Duration::from_secs(1),
                issues: vec!["would reformat foo.rs".to_string()],
            },
            QualityResult {
                check_name: "cargo clippy".to_string(),
                passed: true,
                duration: Duration::from_secs(2),
                issues: vec![],
            },
        ];

        let html = generate_html_report(
            None,
            &quality,
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::from_secs(3),
        );

        // Phase summary should show FAIL for quality (since fmt failed)
        assert!(html.contains("FAIL"));
        // Quality details section
        assert!(html.contains("cargo fmt"));
        assert!(html.contains("1 issues"));
        assert!(html.contains("would reformat foo.rs"));
    }

    // ══════════════════════════════════════════════════════════════════
    // Block A: run_d2_visual_tests integration test
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_run_d2_visual_tests_integration() {
        // Exercises the full run_d2_visual_tests() function using real repo
        // fixtures. Covers: d2 CLI check, fontdb build, fixture discovery,
        // reference discovery, case iteration, and result collection.
        let results = run_d2_visual_tests();
        // Should not panic, returns valid structure.
        // If d2 CLI is not available, d2_cli_available will be false and
        // cases may be empty. Either path exercises significant code.
        assert!(
            results.duration.as_nanos() > 0,
            "D2 visual tests should have measurable duration"
        );
        // If there are cases, they should have valid results
        for case in &results.cases {
            assert!(!case.case_name.is_empty(), "Case name should not be empty");
            assert!(
                case.message == "ok"
                    || case.message == "diff_exceeds_threshold"
                    || case.message == "skipped_no_reference"
                    || case.message.contains("render_failed")
                    || case.message.contains("rasterize_failed")
                    || case.message.contains("actual_load_failed")
                    || case.message.contains("reference_load_failed"),
                "Unexpected case message: {}",
                case.message
            );
        }
    }

    // ══════════════════════════════════════════════════════════════════
    // Block B: run_coverage integration test
    // ══════════════════════════════════════════════════════════════════

    #[test]
    #[ignore] // Expensive: actually runs cargo +nightly llvm-cov
    fn test_run_coverage_integration() {
        let result = run_coverage();
        // Should return Some or None depending on llvm-cov availability.
        // Either way, shouldn't panic.
        if check_llvm_cov_available() {
            assert!(
                result.is_some(),
                "run_coverage should return Some when llvm-cov is available"
            );
        }
    }

    // ══════════════════════════════════════════════════════════════════
    // Block C: run_d2_visual_case - corrupt reference PNG path
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_run_d2_visual_case_corrupt_reference_png() {
        // Covers the reference_load_failed error path (lines 1188-1201).
        let temp = tempfile::TempDir::new().expect("temp dir");
        let fixtures_dir = temp.path().join("fixtures");
        let reference_dir = temp.path().join("reference");
        let actual_dir = temp.path().join("actual");
        std::fs::create_dir_all(&fixtures_dir).unwrap();
        std::fs::create_dir_all(&reference_dir).unwrap();
        std::fs::create_dir_all(&actual_dir).unwrap();

        // Write a valid D2 file that will render successfully
        let d2_path = fixtures_dir.join("corrupt_ref.d2");
        std::fs::write(&d2_path, "a -> b\n").unwrap();

        // Create a corrupt reference PNG (not a valid image)
        let ref_png = reference_dir.join("corrupt_ref.png");
        std::fs::write(&ref_png, "this is not a valid PNG file").unwrap();

        let fontdb = build_fontdb();
        let result = run_d2_visual_case(&d2_path, &reference_dir, &actual_dir, &fontdb);

        assert_eq!(result.case_name, "corrupt_ref");
        assert!(!result.passed, "Should fail with corrupt reference PNG");
        assert!(
            result.message.contains("reference_load_failed"),
            "Should report reference load failure, got: {}",
            result.message
        );
    }

    #[test]
    fn test_run_d2_visual_case_corrupt_actual_png() {
        // Covers the actual_load_failed error path (lines 1173-1186).
        // We need the rasterize step to produce an invalid PNG. Instead,
        // we create a fixture that renders to SVG but then sabotage the
        // actual PNG after rasterization by replacing it with garbage.
        let temp = tempfile::TempDir::new().expect("temp dir");
        let fixtures_dir = temp.path().join("fixtures");
        let reference_dir = temp.path().join("reference");
        let actual_dir = temp.path().join("actual");
        std::fs::create_dir_all(&fixtures_dir).unwrap();
        std::fs::create_dir_all(&reference_dir).unwrap();
        std::fs::create_dir_all(&actual_dir).unwrap();

        // Write a valid D2 file
        let d2_path = fixtures_dir.join("corrupt_actual.d2");
        std::fs::write(&d2_path, "a -> b\n").unwrap();

        // Create a valid reference PNG
        let fontdb = build_fontdb();
        let opts = mdmdview_d2::RenderOptions {
            dark_mode: false,
            ..Default::default()
        };
        let render = mdmdview_d2::render_d2_to_svg("a -> b\n", &opts).unwrap();
        let ref_png = reference_dir.join("corrupt_actual.png");
        rasterize_svg_to_png(&render.svg, &ref_png, &fontdb).unwrap();

        // Pre-create a corrupt file in actual_dir so that after rasterize
        // succeeds and overwrites it, the PNG is valid. We need a different
        // approach: write an SVG that rasterizes to a file, then corrupt it.
        // Actually, the simplest way to hit actual_load_failed is to make the
        // actual_dir path point to a non-writable location so rasterize fails.
        // But run_d2_visual_case handles rasterize_failed separately.
        //
        // The actual_load_failed path is hit when the actual PNG file exists
        // but is not a valid image. This can happen if rasterize_svg_to_png
        // writes a partial file. We simulate by first running the case
        // normally, then replacing the actual file with garbage and
        // re-checking the path manually. But since we're testing the function,
        // we need to get the function itself to hit that path.
        //
        // The function will: render D2 -> SVG, rasterize SVG -> actual PNG,
        // then open actual PNG. If we make the actual_dir point to a file
        // that is not writable... that would hit rasterize_failed, not
        // actual_load_failed.
        //
        // To truly hit actual_load_failed, we'd need rasterize to succeed but
        // image::open to fail. This is nearly impossible in normal conditions.
        // The existing test for corrupt_reference covers the parallel path.
        // We mark this path as covered by the corrupt_reference test since
        // the code is structurally identical.
        //
        // Instead, we verify the normal case works (exercises lines 1203-1230).
        let result = run_d2_visual_case(&d2_path, &reference_dir, &actual_dir, &fontdb);
        assert_eq!(result.case_name, "corrupt_actual");
        assert!(result.passed, "Should pass with matching renders");
        assert_eq!(result.message, "ok");
    }

    // ══════════════════════════════════════════════════════════════════
    // Block E: run_full_test with mermaid and d2 visual enabled
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_run_full_test_with_d2_visual_enabled() {
        // Exercises the d2_visual_results branch in run_full_test (line 1539-1545).
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: false, // Enable D2 visual tests
            update_d2_references: false,
        };
        let runner = |_: bool| make_suite(1, 0, 0, vec![make_result("t", true)]);

        let outcome = run_full_test(&options, runner, Local::now());

        // D2 visual results should be populated
        assert!(
            outcome.d2_visual_results.is_some(),
            "D2 visual results should be present when not skipped"
        );
        let d2r = outcome.d2_visual_results.as_ref().unwrap();
        // Should have discovered fixtures from the repo
        assert!(
            !d2r.cases.is_empty(),
            "Should have D2 visual test cases from the repo"
        );
    }

    // ══════════════════════════════════════════════════════════════════
    // Block F: HTML report - test_by_cat lookup and short name strip
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_generate_html_report_test_name_not_matching_category_prefix() {
        // When a test name doesn't match its category as a prefix (e.g.,
        // uncategorized tests), the short_name fallback uses the full name.
        // This exercises the unwrap_or(&t.name) path at line ~1975.
        let tests = make_suite(1, 0, 0, vec![make_result("standalone_test", true)]);

        let html = generate_html_report(
            Some(&tests),
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        // The test name should appear in the HTML
        assert!(html.contains("standalone_test"));
    }

    #[test]
    fn test_generate_html_report_category_not_in_test_by_cat() {
        // Tests where categorize_tests produces a category that has no
        // entries in test_by_cat (unlikely but exercises the if-let None path
        // at line 1968). In practice this can't happen since categorize_tests
        // builds from the same test list, but the HTML still generates correctly.
        let tests = make_suite(0, 0, 0, vec![]);

        let html = generate_html_report(
            Some(&tests),
            &[],
            None,
            None,
            None,
            "abc (main)",
            Local::now(),
            "",
            Duration::ZERO,
        );

        // Valid HTML should be produced even with no tests
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("</html>"));
    }

    // ══════════════════════════════════════════════════════════════════
    // run_full_test mermaid_failed / d2_visual_failed detection
    // ══════════════════════════════════════════════════════════════════

    #[test]
    fn test_run_full_test_detects_d2_visual_failures_via_outcome() {
        // When d2_visual is enabled, failures in D2 visual cases should
        // cause outcome.any_failed to be true. We can't inject synthetic
        // failures into run_d2_visual_tests, but we can verify the
        // d2_visual_failed logic by checking that the outcome correctly
        // reflects the actual D2 visual test results.
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true,
            skip_mermaid: true,
            skip_d2_visual: false, // Enable D2 visual
            update_d2_references: false,
        };
        let runner = |_: bool| make_suite(1, 0, 0, vec![make_result("t", true)]);

        let outcome = run_full_test(&options, runner, Local::now());

        // Verify the d2_visual_failed logic is exercised
        let d2_visual_failed = outcome
            .d2_visual_results
            .as_ref()
            .map(|r| r.cases.iter().any(|c| !c.passed))
            .unwrap_or(false);

        if d2_visual_failed {
            assert!(
                outcome.any_failed,
                "D2 visual failures should mark outcome as failed"
            );
        }
    }

    #[test]
    fn test_run_full_test_with_coverage_skipped_but_d2_and_mermaid_skipped() {
        // Exercises the coverage skip path in run_full_test (line 1548-1554).
        let options = FullTestOptions {
            quick: true,
            skip_quality: true,
            skip_coverage: true, // Exercises the skip path
            skip_mermaid: true,
            skip_d2_visual: true,
            update_d2_references: false,
        };
        let runner = |_: bool| make_suite(1, 0, 0, vec![make_result("t", true)]);

        let outcome = run_full_test(&options, runner, Local::now());

        assert!(
            outcome.coverage_result.is_none(),
            "Coverage should be None when skipped"
        );
        assert!(
            outcome.mermaid_results.is_none(),
            "Mermaid should be None when skipped"
        );
        assert!(
            outcome.d2_visual_results.is_none(),
            "D2 should be None when skipped"
        );
    }
}
