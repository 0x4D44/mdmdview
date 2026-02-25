#![cfg_attr(coverage_nightly, feature(coverage_attribute))]
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // Hide console in release mode

// Main entry point for the MarkdownView application.
//
// A simple, standalone markdown viewer for Windows built with Rust and egui.
// This application provides a clean interface for viewing markdown files with
// syntax highlighting, embedded samples, and essential viewing features.

use std::path::PathBuf;

#[cfg(not(test))]
use mdmdview::app::{ScreenshotConfig, ScreenshotTheme};
#[cfg(not(test))]
use mdmdview::{load_window_state, sanitize_window_state, MarkdownViewerApp, APP_TITLE_PREFIX};
#[cfg(not(test))]
use std::path::Path;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ThemeChoice {
    Light,
    Dark,
}

#[derive(Default)]
struct CliOptions {
    initial_file: Option<PathBuf>,
    screenshot: bool,
    screenshot_output: Option<PathBuf>,
    width: Option<f32>,
    height: Option<f32>,
    theme: Option<ThemeChoice>,
    zoom: Option<f32>,
    content_only: bool,
    scroll: Option<f32>,
    wait_ms: Option<u64>,
    settle_frames: Option<u32>,
    test_fonts: Option<PathBuf>,
}

#[cfg(not(test))]
fn parse_cli_args() -> Result<CliOptions, String> {
    parse_cli_from(std::env::args().skip(1))
}

fn parse_cli_from<I>(args: I) -> Result<CliOptions, String>
where
    I: IntoIterator<Item = String>,
{
    let mut opts = CliOptions::default();
    let mut iter = args.into_iter();

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--screenshot" => {
                opts.screenshot = true;
                let value = next_value(&mut iter, "--screenshot")?;
                opts.initial_file = Some(PathBuf::from(value));
            }
            "--output" => {
                let value = next_value(&mut iter, "--output")?;
                opts.screenshot_output = Some(PathBuf::from(value));
            }
            "--width" => {
                let value = next_value(&mut iter, "--width")?;
                opts.width = Some(parse_value("--width", &value)?);
            }
            "--height" => {
                let value = next_value(&mut iter, "--height")?;
                opts.height = Some(parse_value("--height", &value)?);
            }
            "--theme" => {
                let value = next_value(&mut iter, "--theme")?;
                opts.theme = Some(parse_theme(&value)?);
            }
            "--zoom" => {
                let value = next_value(&mut iter, "--zoom")?;
                opts.zoom = Some(parse_value("--zoom", &value)?);
            }
            "--content-only" => opts.content_only = true,
            "--scroll" => {
                let value = next_value(&mut iter, "--scroll")?;
                let ratio = parse_value::<f32>("--scroll", &value)?.clamp(0.0, 1.0);
                opts.scroll = Some(ratio);
            }
            "--wait-ms" => {
                let value = next_value(&mut iter, "--wait-ms")?;
                opts.wait_ms = Some(parse_value("--wait-ms", &value)?);
            }
            "--settle-frames" => {
                let value = next_value(&mut iter, "--settle-frames")?;
                opts.settle_frames = Some(parse_value("--settle-frames", &value)?);
            }
            "--test-fonts" => {
                let value = next_value(&mut iter, "--test-fonts")?;
                opts.test_fonts = Some(PathBuf::from(value));
            }
            _ if opts.initial_file.is_none() => opts.initial_file = Some(PathBuf::from(arg)),
            _ => {}
        }
    }

    if opts.screenshot && opts.screenshot_output.is_none() {
        return Err("Missing --output for screenshot mode".to_string());
    }

    Ok(opts)
}

fn next_value<I>(iter: &mut I, flag: &str) -> Result<String, String>
where
    I: Iterator<Item = String>,
{
    iter.next()
        .ok_or_else(|| format!("{flag} requires a value"))
}

fn parse_theme(value: &str) -> Result<ThemeChoice, String> {
    match value.trim().to_ascii_lowercase().as_str() {
        "light" => Ok(ThemeChoice::Light),
        "dark" => Ok(ThemeChoice::Dark),
        _ => Err(format!("Unsupported theme: {value}")),
    }
}

#[cfg(test)]
fn parse_hex_color32(value: &str) -> Option<egui::Color32> {
    let hex = value.trim().trim_start_matches('#');
    if hex.len() != 6 {
        return None;
    }
    let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
    let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
    let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
    Some(egui::Color32::from_rgb(r, g, b))
}

fn screenshot_background_color() -> egui::Color32 {
    egui::Color32::from_rgb(255, 248, 219)
}

fn parse_value<T: std::str::FromStr>(flag: &str, value: &str) -> Result<T, String> {
    value
        .parse::<T>()
        .map_err(|_| format!("Invalid {flag} value: {value}"))
}

#[cfg(not(test))]
fn load_fonts_from_dir(ctx: &egui::Context, dir: &Path) -> Result<(), String> {
    let mut font_paths: Vec<PathBuf> = std::fs::read_dir(dir)
        .map_err(|e| format!("Failed to read font dir {dir:?}: {e}"))?
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|path| {
            path.extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| matches!(ext.to_ascii_lowercase().as_str(), "ttf" | "otf"))
                .unwrap_or(false)
        })
        .collect();

    if font_paths.is_empty() {
        return Err(format!("No font files found in {dir:?}"));
    }

    font_paths.sort_by(|a, b| a.to_string_lossy().cmp(&b.to_string_lossy()));

    let mut fonts = egui::FontDefinitions::default();
    let mut prop_names = Vec::new();
    let mut mono_names = Vec::new();
    let mut used_names = std::collections::HashSet::new();

    for path in font_paths {
        let bytes =
            std::fs::read(&path).map_err(|e| format!("Failed to read font {path:?}: {e}"))?;
        let name = path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .map(|stem| stem.to_string())
            .unwrap_or_else(|| format!("font-{}", prop_names.len() + mono_names.len()));
        let base = format!("test-{name}");
        let mut name = base.clone();
        let mut counter = 1usize;
        while used_names.contains(&name) {
            name = format!("{base}-{counter}");
            counter += 1;
        }
        used_names.insert(name.clone());

        fonts
            .font_data
            .insert(name.clone(), egui::FontData::from_owned(bytes));

        let lower = name.to_ascii_lowercase();
        if lower.contains("mono") || lower.contains("code") {
            mono_names.push(name);
        } else {
            prop_names.push(name);
        }
    }

    if prop_names.is_empty() {
        prop_names = mono_names.clone();
    }
    if mono_names.is_empty() {
        mono_names = prop_names.clone();
    }

    if let Some(family) = fonts.families.get_mut(&egui::FontFamily::Proportional) {
        for name in prop_names.iter().rev() {
            family.insert(0, name.clone());
        }
    }
    if let Some(family) = fonts.families.get_mut(&egui::FontFamily::Monospace) {
        for name in mono_names.iter().rev() {
            family.insert(0, name.clone());
        }
    }

    ctx.set_fonts(fonts);
    Ok(())
}

/// Application entry point
#[cfg(not(test))]
fn main() -> Result<(), eframe::Error> {
    #[cfg(debug_assertions)]
    env_logger::init();

    let cli = match parse_cli_args() {
        Ok(opts) => opts,
        Err(err) => {
            eprintln!("{err}");
            return Ok(());
        }
    };

    let screenshot_enabled = cli.screenshot;
    let resolved_theme = if screenshot_enabled {
        Some(cli.theme.unwrap_or(ThemeChoice::Light))
    } else {
        cli.theme
    };
    let screenshot_zoom = cli.zoom.unwrap_or(1.0);

    let default_width = if screenshot_enabled { 1280.0 } else { 1000.0 };
    let default_height = if screenshot_enabled { 720.0 } else { 700.0 };
    let window_width = cli.width.unwrap_or(default_width);
    let window_height = cli.height.unwrap_or(default_height);

    let mut viewport = egui::ViewportBuilder::default()
        .with_title(APP_TITLE_PREFIX.to_string())
        .with_inner_size(egui::Vec2::new(window_width, window_height))
        .with_min_inner_size(egui::Vec2::new(
            if screenshot_enabled { 100.0 } else { 600.0 },
            if screenshot_enabled { 100.0 } else { 400.0 },
        ))
        .with_icon(create_app_icon())
        .with_resizable(!screenshot_enabled)
        .with_maximize_button(!screenshot_enabled)
        .with_minimize_button(!screenshot_enabled)
        .with_drag_and_drop(!screenshot_enabled)
        // Start hidden to avoid white flash before dark theme is applied
        .with_visible(screenshot_enabled);

    if !screenshot_enabled {
        if let Some(ws) = load_window_state() {
            if let Some(ws) = sanitize_window_state(ws) {
                viewport = viewport
                    .with_inner_size(egui::Vec2::new(ws.size[0], ws.size[1]))
                    .with_position(egui::pos2(ws.pos[0], ws.pos[1]))
                    .with_maximized(ws.maximized);
            }
        }
    }

    let native_options = eframe::NativeOptions {
        viewport,
        // Disable eframe's built-in window persistence - we handle it ourselves via
        // load_window_state/save_window_state to avoid conflicts
        persist_window: false,
        ..Default::default()
    };

    let initial_file = cli.initial_file.clone();
    let test_fonts = cli.test_fonts.clone();
    let screenshot_config = if screenshot_enabled {
        let theme = match resolved_theme.unwrap_or(ThemeChoice::Light) {
            ThemeChoice::Light => ScreenshotTheme::Light,
            ThemeChoice::Dark => ScreenshotTheme::Dark,
        };
        Some(ScreenshotConfig {
            output_path: cli
                .screenshot_output
                .clone()
                .expect("screenshot output required"),
            viewport_width: window_width,
            viewport_height: window_height,
            content_only: cli.content_only,
            scroll_ratio: cli.scroll,
            wait_ms: cli.wait_ms.unwrap_or(2000),
            settle_frames: cli.settle_frames.unwrap_or(3),
            zoom: screenshot_zoom,
            theme,
            font_source: test_fonts
                .as_ref()
                .map(|path| path.to_string_lossy().to_string()),
        })
    } else {
        None
    };

    eframe::run_native(
        APP_TITLE_PREFIX,
        native_options,
        Box::new(move |cc| {
            if let Some(theme) = resolved_theme {
                let mut visuals = match theme {
                    ThemeChoice::Light => egui::Visuals::light(),
                    ThemeChoice::Dark => egui::Visuals::dark(),
                };
                if screenshot_enabled {
                    let bg = screenshot_background_color();
                    visuals.panel_fill = bg;
                    visuals.window_fill = bg;
                    visuals.extreme_bg_color = bg;
                    visuals.faint_bg_color = bg;
                }
                cc.egui_ctx.set_visuals(visuals);
            }

            configure_egui_style(&cc.egui_ctx);
            if let Some(font_dir) = test_fonts.as_ref() {
                if let Err(err) = load_fonts_from_dir(&cc.egui_ctx, font_dir) {
                    eprintln!("{err}");
                }
            }

            let mut app = MarkdownViewerApp::new();
            // Apply saved theme ONLY when no explicit --theme flag was passed
            if resolved_theme.is_none() {
                app.apply_saved_theme(&cc.egui_ctx);
            }
            if screenshot_zoom != 1.0 {
                app.set_zoom_scale(screenshot_zoom);
            }
            if let Some(config) = screenshot_config {
                app.set_screenshot_mode(config);
            }

            if let Some(file_path) = initial_file {
                if file_path.exists() && file_path.is_file() {
                    if let Err(e) = app.load_file(file_path, true) {
                        eprintln!("Failed to load file: {e}");
                    }
                } else {
                    eprintln!("File not found: {}", file_path.display());
                }
            }

            Box::new(app)
        }),
    )
}

#[cfg(test)]
fn main() {}

/// Create an application icon from embedded data
fn create_app_icon() -> egui::IconData {
    // Create a 32x32 markdown-style document icon
    let size = 32;
    let mut rgba_data = Vec::with_capacity(size * size * 4);

    for y in 0..size {
        for x in 0..size {
            let (r, g, b, a) = if x == 0 || x == size - 1 || y == 0 || y == size - 1 {
                // Border
                (60, 60, 60, 255)
            } else if x == 1 || x == size - 2 || y == 1 || y == size - 2 {
                // Inner border for depth
                (80, 80, 80, 255)
            } else if (4..=8).contains(&y) && (4..=28).contains(&x) {
                // Header area (title bar)
                if (6..=12).contains(&x) {
                    (100, 150, 255, 255) // Blue for # header
                } else if (14..=26).contains(&x) {
                    (200, 200, 200, 255) // Light text
                } else {
                    (250, 250, 250, 255) // Background
                }
            } else if (11..=13).contains(&y) && (4..=28).contains(&x) {
                // Text line 1
                if (6..=8).contains(&x) {
                    (150, 150, 150, 255) // Bullet point
                } else if (10..=24).contains(&x) {
                    (180, 180, 180, 255) // Text
                } else {
                    (250, 250, 250, 255) // Background
                }
            } else if (15..=17).contains(&y) && (4..=28).contains(&x) {
                // Text line 2
                if (6..=8).contains(&x) {
                    (150, 150, 150, 255) // Bullet point
                } else if (10..=22).contains(&x) {
                    (180, 180, 180, 255) // Text
                } else {
                    (250, 250, 250, 255) // Background
                }
            } else if (20..=22).contains(&y) && (6..=26).contains(&x) {
                // Code block area
                if (8..=24).contains(&x) {
                    (100, 255, 100, 255) // Green code text
                } else {
                    (40, 40, 40, 255) // Dark background
                }
            } else if (25..=27).contains(&y) && (4..=28).contains(&x) {
                // Text line 3
                if (6..=20).contains(&x) {
                    (180, 180, 180, 255) // Text
                } else {
                    (250, 250, 250, 255) // Background
                }
            } else {
                // Document background
                (250, 250, 250, 255)
            };

            rgba_data.extend_from_slice(&[r, g, b, a]);
        }
    }

    egui::IconData {
        rgba: rgba_data,
        width: size as u32,
        height: size as u32,
    }
}

/// Configure egui styling for optimal markdown display
fn configure_egui_style(ctx: &egui::Context) {
    let mut style = (*ctx.style()).clone();

    style.spacing.item_spacing = egui::Vec2::new(8.0, 8.0);
    style.spacing.window_margin = egui::Margin::same(8.0);
    style.spacing.menu_margin = egui::Margin::same(6.0);

    style.interaction.resize_grab_radius_side = 8.0;
    style.interaction.resize_grab_radius_corner = 12.0;

    style.visuals.window_rounding = egui::Rounding::same(4.0);
    style.visuals.menu_rounding = egui::Rounding::same(4.0);

    // True black background for maximum contrast in dark mode
    if style.visuals.dark_mode {
        style.visuals.window_fill = egui::Color32::BLACK;
        style.visuals.panel_fill = egui::Color32::BLACK;
        style.visuals.faint_bg_color = egui::Color32::from_gray(20);
        style.visuals.extreme_bg_color = egui::Color32::BLACK;
    }

    ctx.set_style(style);
    configure_font_fallbacks(ctx);
}

/// Configure font fallbacks for better symbol coverage.
/// Adds platform-specific symbol fonts as fallbacks so characters like ✓ ✗ ⚠
/// render instead of showing tofu boxes.
fn configure_font_fallbacks(ctx: &egui::Context) {
    let mut fonts = egui::FontDefinitions::default();

    // Platform-specific symbol font paths
    #[cfg(target_os = "windows")]
    let symbol_font_paths = &[
        "C:\\Windows\\Fonts\\seguisym.ttf", // Segoe UI Symbol
        "C:\\Windows\\Fonts\\segoeui.ttf",  // Segoe UI (backup)
    ];

    #[cfg(target_os = "macos")]
    let symbol_font_paths = &[
        "/System/Library/Fonts/Apple Symbols.ttf",
        "/System/Library/Fonts/Supplemental/Arial Unicode.ttf",
    ];

    #[cfg(target_os = "linux")]
    let symbol_font_paths = &[
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
        "/usr/share/fonts/truetype/freefont/FreeSans.ttf",
    ];

    #[cfg(not(any(target_os = "windows", target_os = "macos", target_os = "linux")))]
    let symbol_font_paths: &[&str] = &[];

    // Load the first available symbol font as fallback
    let font_data = symbol_font_paths.iter().find_map(|p| std::fs::read(p).ok());
    if let Some(font_data) = font_data {
        let font_name = "symbol_fallback".to_string();
        fonts
            .font_data
            .insert(font_name.clone(), egui::FontData::from_owned(font_data));

        if let Some(family) = fonts.families.get_mut(&egui::FontFamily::Proportional) {
            family.push(font_name.clone());
        }
        if let Some(family) = fonts.families.get_mut(&egui::FontFamily::Monospace) {
            family.push(font_name);
        }
    }

    ctx.set_fonts(fonts);
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;
    use std::sync::{Mutex, OnceLock};

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("env lock")
    }

    struct EnvGuard {
        key: &'static str,
        original: Option<String>,
        _lock: std::sync::MutexGuard<'static, ()>,
    }

    impl EnvGuard {
        fn unset(key: &'static str) -> Self {
            let lock = env_lock();
            let original = std::env::var(key).ok();
            std::env::remove_var(key);
            Self {
                key,
                original,
                _lock: lock,
            }
        }
    }

    impl Drop for EnvGuard {
        fn drop(&mut self) {
            if let Some(prev) = &self.original {
                std::env::set_var(self.key, prev);
            } else {
                std::env::remove_var(self.key);
            }
        }
    }

    #[test]
    fn test_app_icon_creation() {
        let icon = create_app_icon();
        assert_eq!(icon.width, 32);
        assert_eq!(icon.height, 32);
        assert_eq!(icon.rgba.len(), 32 * 32 * 4); // RGBA format
    }

    #[test]
    fn test_configure_egui_style_dark_mode() {
        let ctx = egui::Context::default();
        let mut style = (*ctx.style()).clone();
        style.visuals.dark_mode = true;
        ctx.set_style(style);

        configure_egui_style(&ctx);

        let style = ctx.style();
        assert_eq!(style.spacing.item_spacing, egui::Vec2::new(8.0, 8.0));
        assert_eq!(style.spacing.window_margin, egui::Margin::same(8.0));
        assert_eq!(style.spacing.menu_margin, egui::Margin::same(6.0));
        assert_eq!(style.visuals.window_rounding, egui::Rounding::same(4.0));
        assert_eq!(style.visuals.menu_rounding, egui::Rounding::same(4.0));
        assert_eq!(style.visuals.window_fill, egui::Color32::BLACK);
        assert_eq!(style.visuals.panel_fill, egui::Color32::BLACK);
        assert_eq!(style.visuals.faint_bg_color, egui::Color32::from_gray(20));
        assert_eq!(style.visuals.extreme_bg_color, egui::Color32::BLACK);
    }

    #[test]
    fn test_configure_egui_style_light_mode() {
        let ctx = egui::Context::default();
        let mut style = (*ctx.style()).clone();
        style.visuals.dark_mode = false;
        style.visuals.window_fill = egui::Color32::from_rgb(1, 2, 3);
        style.visuals.panel_fill = egui::Color32::from_rgb(4, 5, 6);
        style.visuals.faint_bg_color = egui::Color32::from_rgb(7, 8, 9);
        style.visuals.extreme_bg_color = egui::Color32::from_rgb(10, 11, 12);
        ctx.set_style(style);

        configure_egui_style(&ctx);

        let style = ctx.style();
        assert_eq!(style.spacing.item_spacing, egui::Vec2::new(8.0, 8.0));
        assert_eq!(style.spacing.window_margin, egui::Margin::same(8.0));
        assert_eq!(style.spacing.menu_margin, egui::Margin::same(6.0));
        assert_eq!(style.visuals.window_rounding, egui::Rounding::same(4.0));
        assert_eq!(style.visuals.menu_rounding, egui::Rounding::same(4.0));
        assert_eq!(style.visuals.window_fill, egui::Color32::from_rgb(1, 2, 3));
        assert_eq!(style.visuals.panel_fill, egui::Color32::from_rgb(4, 5, 6));
        assert_eq!(
            style.visuals.faint_bg_color,
            egui::Color32::from_rgb(7, 8, 9)
        );
        assert_eq!(
            style.visuals.extreme_bg_color,
            egui::Color32::from_rgb(10, 11, 12)
        );
    }

    #[test]
    fn test_parse_cli_sets_initial_file_from_arg() {
        let opts = parse_cli_from(vec!["readme.md".to_string()]).expect("parse");
        assert_eq!(opts.initial_file, Some(PathBuf::from("readme.md")));
    }

    #[test]
    fn test_parse_cli_screenshot_args() {
        let args = vec![
            "--screenshot".to_string(),
            "doc.md".to_string(),
            "--output".to_string(),
            "out.png".to_string(),
            "--width".to_string(),
            "800".to_string(),
            "--height".to_string(),
            "600".to_string(),
            "--theme".to_string(),
            "dark".to_string(),
            "--scroll".to_string(),
            "0.5".to_string(),
        ];
        let opts = parse_cli_from(args).expect("parse");
        assert!(opts.screenshot);
        assert_eq!(opts.initial_file, Some(PathBuf::from("doc.md")));
        assert_eq!(opts.screenshot_output, Some(PathBuf::from("out.png")));
        assert_eq!(opts.width, Some(800.0));
        assert_eq!(opts.height, Some(600.0));
        assert_eq!(opts.theme, Some(ThemeChoice::Dark));
        assert_eq!(opts.scroll, Some(0.5));
    }

    #[test]
    fn test_parse_cli_missing_screenshot_input() {
        let args = vec!["--screenshot".to_string()];
        let err = parse_cli_from(args)
            .err()
            .expect("expected missing screenshot input error");
        assert!(err.contains("--screenshot requires a value"));
    }

    #[test]
    fn test_parse_cli_missing_screenshot_output() {
        let args = vec!["--screenshot".to_string(), "doc.md".to_string()];
        let err = parse_cli_from(args)
            .err()
            .expect("expected missing output error");
        assert!(err.contains("Missing --output"));
    }

    #[test]
    fn test_parse_cli_invalid_values() {
        let args = vec!["--width".to_string(), "nope".to_string()];
        let err = parse_cli_from(args)
            .err()
            .expect("expected invalid width error");
        assert!(err.contains("Invalid --width"));

        let args = vec!["--height".to_string(), "nope".to_string()];
        let err = parse_cli_from(args)
            .err()
            .expect("expected invalid height error");
        assert!(err.contains("Invalid --height"));

        let args = vec!["--zoom".to_string(), "nope".to_string()];
        let err = parse_cli_from(args)
            .err()
            .expect("expected invalid zoom error");
        assert!(err.contains("Invalid --zoom"));

        let args = vec!["--scroll".to_string(), "nope".to_string()];
        let err = parse_cli_from(args)
            .err()
            .expect("expected invalid scroll error");
        assert!(err.contains("Invalid --scroll"));

        let args = vec!["--wait-ms".to_string(), "nope".to_string()];
        let err = parse_cli_from(args)
            .err()
            .expect("expected invalid wait-ms error");
        assert!(err.contains("Invalid --wait-ms"));

        let args = vec!["--settle-frames".to_string(), "nope".to_string()];
        let err = parse_cli_from(args)
            .err()
            .expect("expected invalid settle-frames error");
        assert!(err.contains("Invalid --settle-frames"));

        let args = vec!["--theme".to_string(), "blue".to_string()];
        let err = parse_cli_from(args)
            .err()
            .expect("expected invalid theme error");
        assert!(err.contains("Unsupported theme"));
    }

    #[test]
    fn test_parse_cli_missing_flag_values() {
        let flags = [
            "--output",
            "--width",
            "--height",
            "--theme",
            "--zoom",
            "--scroll",
            "--wait-ms",
            "--settle-frames",
            "--test-fonts",
        ];
        for flag in flags {
            let err = parse_cli_from(vec![flag.to_string()])
                .err()
                .expect("expected missing flag value error");
            assert!(err.contains(&format!("{flag} requires a value")));
        }
    }

    #[test]
    fn test_parse_cli_scroll_clamps() {
        let args = vec![
            "--scroll".to_string(),
            "2.5".to_string(),
            "doc.md".to_string(),
        ];
        let opts = parse_cli_from(args).expect("parse");
        assert_eq!(opts.scroll, Some(1.0));
        assert_eq!(opts.initial_file, Some(PathBuf::from("doc.md")));
    }

    #[test]
    fn test_parse_cli_additional_flags() {
        let args = vec![
            "--screenshot".to_string(),
            "doc.md".to_string(),
            "--output".to_string(),
            "out.png".to_string(),
            "--content-only".to_string(),
            "--wait-ms".to_string(),
            "1200".to_string(),
            "--settle-frames".to_string(),
            "3".to_string(),
            "--zoom".to_string(),
            "1.5".to_string(),
            "--theme".to_string(),
            "light".to_string(),
            "--test-fonts".to_string(),
            "fonts".to_string(),
        ];
        let opts = parse_cli_from(args).expect("parse");
        assert!(opts.screenshot);
        assert!(opts.content_only);
        assert_eq!(opts.wait_ms, Some(1200));
        assert_eq!(opts.settle_frames, Some(3));
        assert_eq!(opts.zoom, Some(1.5));
        assert_eq!(opts.theme, Some(ThemeChoice::Light));
        assert_eq!(opts.test_fonts, Some(PathBuf::from("fonts")));
    }

    #[test]
    fn test_parse_cli_ignores_extra_args() {
        let args = vec!["doc.md".to_string(), "extra.md".to_string()];
        let opts = parse_cli_from(args).expect("parse");
        assert_eq!(opts.initial_file, Some(PathBuf::from("doc.md")));
    }

    #[test]
    fn test_parse_hex_color32() {
        let color = parse_hex_color32("#FFF8DB").expect("color");
        assert_eq!(color, egui::Color32::from_rgb(255, 248, 219));
        assert!(parse_hex_color32("#XYZ").is_none());
        assert!(parse_hex_color32("#GG0000").is_none());
        assert!(parse_hex_color32("#00GG00").is_none());
        assert!(parse_hex_color32("#0000GG").is_none());
    }

    #[test]
    fn test_parse_value_helpers() {
        assert_eq!(parse_value::<u32>("--width", "12").expect("u32"), 12);
        assert!(parse_value::<u32>("--width", "nope").is_err());
        assert_eq!(parse_value::<u64>("--timeout", "42").expect("u64"), 42);
        assert!(parse_value::<u64>("--timeout", "-1").is_err());
        assert_eq!(parse_value::<f32>("--zoom", "1.5").expect("f32"), 1.5);
        assert!(parse_value::<f32>("--zoom", "abc").is_err());
    }

    #[test]
    fn test_screenshot_background_color() {
        let color = screenshot_background_color();
        assert_eq!(color, egui::Color32::from_rgb(255, 248, 219));
    }

    #[test]
    fn test_env_guard_restores_previous_value() {
        let key = "MDMDVIEW_TEST_GUARD_RESTORE";
        let _lock = env_lock();
        {
            std::env::set_var(key, "orig");
            let guard = EnvGuard {
                key,
                original: Some("orig".to_string()),
                _lock,
            };
            std::env::set_var(key, "overwritten");
            drop(guard);
        }
        assert_eq!(std::env::var(key).ok().as_deref(), Some("orig"));
        // Clean up
        let _lock = env_lock();
        std::env::remove_var(key);
    }

    #[test]
    fn test_env_guard_drop_removes_unset_key() {
        let key = "MDMDVIEW_TEST_GUARD_UNSET";
        std::env::remove_var(key);
        {
            let _guard = EnvGuard::unset(key);
            std::env::set_var(key, "temp");
        }
        assert!(std::env::var(key).is_err());
    }

    #[test]
    fn test_main_stub_executes() {
        super::main();
    }
}
