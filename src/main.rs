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

    // Set up eframe options for the native window
    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_title("MarkdownView - A Simple Markdown Viewer")
            .with_inner_size(egui::Vec2::new(1000.0, 700.0))
            .with_min_inner_size(egui::Vec2::new(600.0, 400.0))
            .with_icon(create_app_icon()),
        // Enable persistence for window state
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
            
            Box::new(MarkdownViewerApp::new())
        }),
    )
}

/// Create an application icon from embedded data
fn create_app_icon() -> egui::IconData {
    // For simplicity, create a minimal icon
    let size = 32;
    let mut rgba_data = Vec::with_capacity(size * size * 4);
    
    for y in 0..size {
        for x in 0..size {
            // Create a simple document-like icon
            let (r, g, b, a) = if x == 0 || x == size - 1 || y == 0 || y == size - 1 {
                (80, 80, 80, 255) // Border
            } else if y < 8 && x > size / 2 {
                (60, 120, 200, 255) // Header area (blue)
            } else if (y + x) % 4 == 0 && y > 8 {
                (100, 100, 100, 255) // Text lines
            } else {
                (240, 240, 240, 255) // Background
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

    // Configure visuals for better contrast
    style.visuals.window_rounding = egui::Rounding::same(4.0);
    style.visuals.menu_rounding = egui::Rounding::same(4.0);
    
    // Dark theme adjustments for markdown content
    if style.visuals.dark_mode {
        style.visuals.window_fill = egui::Color32::from_rgb(25, 25, 25);
        style.visuals.panel_fill = egui::Color32::from_rgb(30, 30, 30);
        style.visuals.faint_bg_color = egui::Color32::from_rgb(40, 40, 40);
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
}
