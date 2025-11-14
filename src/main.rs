#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // Hide console in release mode

/// Main entry point for the MarkdownView application
///
/// A simple, standalone markdown viewer for Windows built with Rust and egui.
/// This application provides a clean interface for viewing markdown files with
/// syntax highlighting, embedded samples, and essential viewing features.
use mdmdview::{load_window_state, sanitize_window_state, MarkdownViewerApp, APP_TITLE_PREFIX};

/// Application entry point
fn main() -> Result<(), eframe::Error> {
    // Configure logging for debugging (only in debug builds)
    #[cfg(debug_assertions)]
    env_logger::init();

    // Parse command line arguments
    let mut initial_file: Option<std::path::PathBuf> = None;
    let mut table_wrap_cli: Option<bool> = None;
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--table-wrap" => table_wrap_cli = Some(true),
            "--no-table-wrap" => table_wrap_cli = Some(false),
            _ if initial_file.is_none() => initial_file = Some(std::path::PathBuf::from(&arg)),
            _ => {}
        }
    }
    let table_wrap_env = std::env::var("MDMDVIEW_TABLE_WRAP_OVERHAUL")
        .ok()
        .and_then(|value| parse_bool_flag(&value));
    let table_wrap_enabled = table_wrap_cli.or(table_wrap_env).unwrap_or(true);

    // Set up eframe options for the native window
    let mut viewport = egui::ViewportBuilder::default()
        .with_title(format!("{APP_TITLE_PREFIX} - A Simple Markdown Viewer"))
        .with_inner_size(egui::Vec2::new(1000.0, 700.0))
        .with_min_inner_size(egui::Vec2::new(600.0, 400.0))
        .with_icon(create_app_icon())
        .with_resizable(true)
        .with_maximize_button(true)
        .with_minimize_button(true)
        .with_drag_and_drop(true);

    // Restore previous window position/size if available
    if let Some(ws) = load_window_state() {
        if let Some(ws) = sanitize_window_state(ws) {
            viewport = viewport
                .with_inner_size(egui::Vec2::new(ws.size[0], ws.size[1]))
                .with_position(egui::pos2(ws.pos[0], ws.pos[1]))
                .with_maximized(ws.maximized);
        }
    }

    let native_options = eframe::NativeOptions {
        viewport,
        // Keep native persist as well; our explicit file handles cross‑platform dirs
        persist_window: true,
        ..Default::default()
    };

    // Launch the application
    eframe::run_native(
        APP_TITLE_PREFIX,
        native_options,
        Box::new(move |cc| {
            // Configure egui styling for better markdown display
            configure_egui_style(&cc.egui_ctx);

            let mut app = MarkdownViewerApp::new();
            app.set_table_wrap_overhaul_enabled(table_wrap_enabled);

            // Load initial file if provided via command line
            if let Some(file_path) = initial_file {
                if file_path.exists() && file_path.is_file() {
                    if let Err(e) = app.load_file(file_path) {
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

fn parse_bool_flag(value: &str) -> Option<bool> {
    match value.trim().to_ascii_lowercase().as_str() {
        "1" | "true" | "yes" | "on" => Some(true),
        "0" | "false" | "no" | "off" => Some(false),
        _ => None,
    }
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
    fn test_parse_bool_flag() {
        assert_eq!(parse_bool_flag("true"), Some(true));
        assert_eq!(parse_bool_flag("FALSE"), Some(false));
        assert_eq!(parse_bool_flag("1"), Some(true));
        assert_eq!(parse_bool_flag("0"), Some(false));
        assert_eq!(parse_bool_flag("unknown"), None);
    }
}
