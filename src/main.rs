#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // Hide console in release mode

#[cfg(not(test))]
use mdmdview::app::{ScreenshotConfig, ScreenshotTheme};
/// Main entry point for the MarkdownView application
///
/// A simple, standalone markdown viewer for Windows built with Rust and egui.
/// This application provides a clean interface for viewing markdown files with
/// syntax highlighting, embedded samples, and essential viewing features.
#[cfg(not(test))]
use mdmdview::{load_window_state, sanitize_window_state, MarkdownViewerApp, APP_TITLE_PREFIX};
#[cfg(not(test))]
use std::path::Path;
use std::path::PathBuf;

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
                opts.width = Some(parse_f32("--width", &value)?);
            }
            "--height" => {
                let value = next_value(&mut iter, "--height")?;
                opts.height = Some(parse_f32("--height", &value)?);
            }
            "--theme" => {
                let value = next_value(&mut iter, "--theme")?;
                opts.theme = Some(parse_theme(&value)?);
            }
            "--zoom" => {
                let value = next_value(&mut iter, "--zoom")?;
                opts.zoom = Some(parse_f32("--zoom", &value)?);
            }
            "--content-only" => opts.content_only = true,
            "--scroll" => {
                let value = next_value(&mut iter, "--scroll")?;
                let ratio = parse_f32("--scroll", &value)?.clamp(0.0, 1.0);
                opts.scroll = Some(ratio);
            }
            "--wait-ms" => {
                let value = next_value(&mut iter, "--wait-ms")?;
                opts.wait_ms = Some(parse_u64("--wait-ms", &value)?);
            }
            "--settle-frames" => {
                let value = next_value(&mut iter, "--settle-frames")?;
                opts.settle_frames = Some(parse_u32("--settle-frames", &value)?);
            }
            "--test-fonts" => {
                let value = next_value(&mut iter, "--test-fonts")?;
                opts.test_fonts = Some(PathBuf::from(value));
            }
            _ if opts.initial_file.is_none() => opts.initial_file = Some(PathBuf::from(arg)),
            _ => {}
        }
    }

    if opts.screenshot {
        if opts.initial_file.is_none() {
            return Err("Missing screenshot input file after --screenshot".to_string());
        }
        if opts.screenshot_output.is_none() {
            return Err("Missing --output for screenshot mode".to_string());
        }
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

fn parse_f32(flag: &str, value: &str) -> Result<f32, String> {
    value
        .parse::<f32>()
        .map_err(|_| format!("Invalid {flag} value: {value}"))
}

fn parse_u32(flag: &str, value: &str) -> Result<u32, String> {
    value
        .parse::<u32>()
        .map_err(|_| format!("Invalid {flag} value: {value}"))
}

fn parse_u64(flag: &str, value: &str) -> Result<u64, String> {
    value
        .parse::<u64>()
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

    for path in font_paths {
        let bytes =
            std::fs::read(&path).map_err(|e| format!("Failed to read font {path:?}: {e}"))?;
        let name = path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .map(|stem| stem.to_string())
            .unwrap_or_else(|| format!("font-{}", prop_names.len() + mono_names.len()));
        let name = format!("test-{name}");

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
    // Configure logging for debugging (only in debug builds)
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

    // Set up eframe options for the native window
    let mut viewport = egui::ViewportBuilder::default()
        .with_title(format!("{APP_TITLE_PREFIX} - A Simple Markdown Viewer"))
        .with_inner_size(egui::Vec2::new(window_width, window_height))
        .with_min_inner_size(egui::Vec2::new(
            if screenshot_enabled { 100.0 } else { 600.0 },
            if screenshot_enabled { 100.0 } else { 400.0 },
        ))
        .with_icon(create_app_icon())
        .with_resizable(!screenshot_enabled)
        .with_maximize_button(!screenshot_enabled)
        .with_minimize_button(!screenshot_enabled)
        .with_drag_and_drop(!screenshot_enabled);

    // Restore previous window position/size if available
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
        // Keep native persist as well; our explicit file handles cross-platform dirs
        persist_window: !screenshot_enabled,
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

    // Launch the application
    eframe::run_native(
        APP_TITLE_PREFIX,
        native_options,
        Box::new(move |cc| {
            if let Some(theme) = resolved_theme {
                let visuals = match theme {
                    ThemeChoice::Light => egui::Visuals::light(),
                    ThemeChoice::Dark => egui::Visuals::dark(),
                };
                cc.egui_ctx.set_visuals(visuals);
            }

            // Configure egui styling for better markdown display
            configure_egui_style(&cc.egui_ctx);
            if let Some(font_dir) = test_fonts.as_ref() {
                if let Err(err) = load_fonts_from_dir(&cc.egui_ctx, font_dir) {
                    eprintln!("{err}");
                }
            }

            let mut app = MarkdownViewerApp::new();
            if screenshot_zoom != 1.0 {
                app.set_zoom_scale(screenshot_zoom);
            }
            if let Some(config) = screenshot_config {
                app.set_screenshot_mode(config);
            }

            // Load initial file if provided via command line
            if let Some(file_path) = initial_file {
                if file_path.exists() && file_path.is_file() {
                    if let Err(e) = app.load_file(file_path, true) {
                        eprintln!("Failed to load file: {}", e);
                        // Continue with default welcome screen
                    }
                } else {
                    eprintln!("File not found: {}", file_path.display());
                    // Continue with default welcome screen
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

    // Configure spacing for better readability
    style.spacing.item_spacing = egui::Vec2::new(8.0, 8.0);
    style.spacing.window_margin = egui::Margin::same(8.0);
    style.spacing.menu_margin = egui::Margin::same(6.0);

    // Configure interaction settings
    style.interaction.resize_grab_radius_side = 8.0;
    style.interaction.resize_grab_radius_corner = 12.0;

    // Configure visuals for better contrast with black background
    style.visuals.window_rounding = egui::Rounding::same(4.0);
    style.visuals.menu_rounding = egui::Rounding::same(4.0);

    // Set to true black background for maximum contrast
    if style.visuals.dark_mode {
        style.visuals.window_fill = egui::Color32::BLACK;
        style.visuals.panel_fill = egui::Color32::BLACK;
        style.visuals.faint_bg_color = egui::Color32::from_gray(20);
        style.visuals.extreme_bg_color = egui::Color32::BLACK;
        // The text color will be handled by egui's theme system
    }

    ctx.set_style(style);

    // Use default fonts - egui has good built-in font support
    // Custom fonts could be added here if needed
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_app_icon_creation() {
        let icon = create_app_icon();
        assert_eq!(icon.width, 32);
        assert_eq!(icon.height, 32);
        assert_eq!(icon.rgba.len(), 32 * 32 * 4); // RGBA format
    }

    #[test]
    fn test_main_function_setup() {
        // Test that the main function components work
        // This is a basic smoke test
        let icon = create_app_icon();
        assert!(!icon.rgba.is_empty());
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
    fn test_main_stub_executes() {
        super::main();
    }
}
