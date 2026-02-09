// Theme colors module for light/dark mode support.
//
// Provides a centralized color palette (`ThemeColors`) with const DARK and LIGHT variants,
// replacing hardcoded Color32 values scattered across the renderer files. Also provides
// `apply_dark_mode_visuals()` to switch egui's visual style between dark and light modes
// with true-black overrides for dark mode.

use egui::Color32;

/// Centralized color palette for theme-critical UI elements.
///
/// Two const instances (`DARK` and `LIGHT`) hold every color that needs to change
/// when switching themes. Rendering code calls `ThemeColors::current(dark_mode)` to
/// get the active palette.
pub struct ThemeColors {
    // Links
    pub link: Color32,
    pub link_internal: Color32,
    // Code blocks
    pub code_bg: Color32,
    pub code_border: Color32,
    pub code_label: Color32,
    pub code_fallback_text: Color32,
    // Inline code
    pub inline_code_bg: Color32,
    pub inline_code_fg: Color32,
    // Blockquotes
    pub blockquote_bg: Color32,
    pub blockquote_border: Color32,
    pub blockquote_text: Color32,
    pub blockquote_bar: Color32,
    // Info/error boxes (mermaid)
    pub box_bg: Color32,
    pub box_border: Color32,
    pub box_title: Color32,
    pub box_body: Color32,
    // Status bar accents
    pub status_loading: Color32,
    pub status_queue: Color32,
    pub status_hint: Color32,
}

impl ThemeColors {
    /// Dark palette — matches the current hardcoded values throughout the codebase.
    pub const DARK: Self = Self {
        link: Color32::from_rgb(120, 190, 255),
        link_internal: Color32::from_rgb(135, 206, 250), // Color32::LIGHT_BLUE
        code_bg: Color32::from_rgb(30, 30, 30),
        code_border: Color32::from_rgb(60, 60, 60),
        code_label: Color32::from_rgb(140, 140, 140),
        code_fallback_text: Color32::from_rgb(220, 220, 220),
        inline_code_bg: Color32::from_rgb(30, 30, 30),
        inline_code_fg: Color32::from_rgb(180, 255, 180),
        blockquote_bg: Color32::from_rgb(24, 24, 24),
        blockquote_border: Color32::from_rgb(40, 40, 40),
        blockquote_text: Color32::WHITE,
        blockquote_bar: Color32::from_rgb(255, 103, 25),
        box_bg: Color32::from_rgb(25, 25, 25),
        box_border: Color32::from_rgb(60, 60, 60),
        box_title: Color32::from_rgb(200, 160, 80),
        box_body: Color32::from_rgb(180, 180, 180),
        status_loading: Color32::from_rgb(120, 200, 255),
        status_queue: Color32::from_rgb(100, 150, 255),
        status_hint: Color32::GRAY,
    };

    /// Light palette — readable on white/light backgrounds.
    pub const LIGHT: Self = Self {
        link: Color32::from_rgb(0, 102, 204),
        link_internal: Color32::from_rgb(0, 80, 180),
        code_bg: Color32::from_rgb(245, 245, 245),
        code_border: Color32::from_rgb(210, 210, 210),
        code_label: Color32::from_rgb(120, 120, 120),
        code_fallback_text: Color32::from_rgb(50, 50, 50),
        inline_code_bg: Color32::WHITE,
        inline_code_fg: Color32::from_rgb(60, 80, 150),
        blockquote_bg: Color32::from_rgb(245, 245, 248),
        blockquote_border: Color32::from_rgb(215, 215, 220),
        blockquote_text: Color32::from_rgb(40, 40, 40),
        blockquote_bar: Color32::from_rgb(255, 103, 25),
        box_bg: Color32::from_rgb(245, 245, 245),
        box_border: Color32::from_rgb(200, 200, 200),
        box_title: Color32::from_rgb(160, 100, 20),
        box_body: Color32::from_rgb(80, 80, 80),
        status_loading: Color32::from_rgb(30, 120, 200),
        status_queue: Color32::from_rgb(30, 100, 200),
        status_hint: Color32::from_rgb(130, 130, 130),
    };

    /// Returns the palette for the given mode.
    pub fn current(dark_mode: bool) -> &'static Self {
        if dark_mode {
            &Self::DARK
        } else {
            &Self::LIGHT
        }
    }

    /// Returns the syntect theme name for the given mode.
    pub fn syntect_theme(dark_mode: bool) -> &'static str {
        if dark_mode {
            "base16-ocean.dark"
        } else {
            "InspiredGitHub"
        }
    }
}

/// Apply dark or light visuals with true-black overrides for dark mode.
/// Clones the current style to preserve spacing/rounding set by `configure_egui_style()`.
pub fn apply_dark_mode_visuals(ctx: &egui::Context, dark: bool) {
    let mut style = (*ctx.style()).clone();
    style.visuals = if dark {
        egui::Visuals::dark()
    } else {
        egui::Visuals::light()
    };
    if dark {
        style.visuals.window_fill = Color32::BLACK;
        style.visuals.panel_fill = Color32::BLACK;
        style.visuals.faint_bg_color = Color32::from_gray(20);
        style.visuals.extreme_bg_color = Color32::BLACK;
    }
    ctx.set_style(style);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_current_returns_dark() {
        let tc = ThemeColors::current(true);
        assert_eq!(tc.link, ThemeColors::DARK.link);
        assert_eq!(tc.blockquote_text, ThemeColors::DARK.blockquote_text);
    }

    #[test]
    fn test_current_returns_light() {
        let tc = ThemeColors::current(false);
        assert_eq!(tc.link, ThemeColors::LIGHT.link);
        assert_eq!(tc.blockquote_text, ThemeColors::LIGHT.blockquote_text);
    }

    #[test]
    fn test_dark_palette_matches_hardcoded() {
        // Spot-check key DARK values match what was hardcoded in the codebase
        assert_eq!(ThemeColors::DARK.link, Color32::from_rgb(120, 190, 255));
        assert_eq!(
            ThemeColors::DARK.link_internal,
            Color32::from_rgb(135, 206, 250)
        );
        assert_eq!(
            ThemeColors::DARK.blockquote_bar,
            Color32::from_rgb(255, 103, 25)
        );
        assert_eq!(ThemeColors::DARK.blockquote_text, Color32::WHITE);
        assert_eq!(ThemeColors::DARK.code_bg, Color32::from_rgb(30, 30, 30));
    }

    #[test]
    fn test_light_palette_differs() {
        // Light link color must differ from dark link color
        assert_ne!(ThemeColors::LIGHT.link, ThemeColors::DARK.link);
        assert_ne!(
            ThemeColors::LIGHT.link_internal,
            ThemeColors::DARK.link_internal
        );
        assert_ne!(ThemeColors::LIGHT.code_bg, ThemeColors::DARK.code_bg);
        assert_ne!(
            ThemeColors::LIGHT.blockquote_text,
            ThemeColors::DARK.blockquote_text
        );
    }

    #[test]
    fn test_syntect_theme_dark() {
        assert_eq!(ThemeColors::syntect_theme(true), "base16-ocean.dark");
    }

    #[test]
    fn test_syntect_theme_light() {
        assert_eq!(ThemeColors::syntect_theme(false), "InspiredGitHub");
    }
}
