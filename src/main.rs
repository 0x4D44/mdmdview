#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // Hide console in release mode

/// Main entry point for the MarkdownView application
/// 
/// A simple, standalone markdown viewer for Windows built with Rust and egui.
/// This application provides a clean interface for viewing markdown files with
/// syntax highlighting, embedded samples, and essential viewing features.

use mdmdview::MarkdownViewerApp;

/// Application entry point
fn main() -> Result<(), eframe::Error> {
    // Configure logging for debugging (only in debug builds)
    #[cfg(debug_assertions)]
    env_logger::init();

    // Parse command line arguments
    let args: Vec<String> = std::env::args().collect();
    let initial_file = if args.len() > 1 {
        Some(std::path::PathBuf::from(&args[1]))
    } else {
        None
    };

    // Set up eframe options for the native window
    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_title("MarkdownView - A Simple Markdown Viewer")
            .with_inner_size(egui::Vec2::new(1000.0, 700.0))
            .with_min_inner_size(egui::Vec2::new(600.0, 400.0))
            .with_icon(create_app_icon())
            .with_resizable(true)
            .with_maximize_button(true)
            .with_minimize_button(true),
        // Enable complete window state persistence
        persist_window: true,
        ..Default::default()
    };

    // Launch the application
    eframe::run_native(
        "MarkdownView",
        native_options,
        Box::new(|cc| {
            // Configure egui styling for better markdown display
            configure_egui_style(&cc.egui_ctx);
            
            let mut app = MarkdownViewerApp::new();
            
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
    fn test_command_line_parsing() {
        // This tests the command line logic conceptually
        let args = vec!["program".to_string(), "test.md".to_string()];
        let initial_file = if args.len() > 1 {
            Some(std::path::PathBuf::from(&args[1]))
        } else {
            None
        };
        
        assert!(initial_file.is_some());
        assert_eq!(initial_file.unwrap().to_string_lossy(), "test.md");
        
        // Test no arguments
        let args = vec!["program".to_string()];
        let initial_file = if args.len() > 1 {
            Some(std::path::PathBuf::from(&args[1]))
        } else {
            None
        };
        
        assert!(initial_file.is_none());
    }
}
