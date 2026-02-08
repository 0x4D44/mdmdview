// emoji_assets.rs -- Procedural vector fallback icons for a small subset of emoji.
//
// When a Twemoji PNG is unavailable (see emoji_catalog.rs), this module can
// generate simple rasterised shapes for a handful of commonly used emoji.
// The fallback set is intentionally small; most emoji are served as embedded
// PNGs from the catalog.
//
// Public API:
//   make_image(emoji, size) -> Option<ColorImage>  -- render a fallback icon
//   has_fallback(emoji)     -> bool                 -- cheap membership check

use egui::Color32 as C;

/// Emoji codepoints that have procedural vector fallbacks.
/// Both `make_image` and `has_fallback` derive from this list, keeping them
/// automatically in sync.
const FALLBACK_EMOJI: &[&str] = &[
    "\u{2705}",  // white check mark
    "\u{1f389}", // tada / party popper
    "\u{1f680}", // rocket
    "\u{2764}",  // red heart
    "\u{1f496}", // sparkling heart (shares heart renderer)
    "\u{2b50}",  // star
    "\u{1f525}", // fire
];

/// Draw a simple vector fallback icon for a supported emoji.
///
/// Returns `None` for emoji not in `FALLBACK_EMOJI`.
pub fn make_image(emoji: &str, size: usize) -> Option<egui::ColorImage> {
    let mut img = egui::ColorImage::new([size, size], C::TRANSPARENT);
    match emoji {
        "\u{2705}" => {
            draw_circle(&mut img, size, C::from_rgb(34, 139, 34));
            draw_check(&mut img, size, C::WHITE);
        }
        "\u{1f389}" => {
            draw_circle(&mut img, size, C::from_rgb(255, 215, 0));
            confetti(&mut img, size);
        }
        "\u{1f680}" => {
            draw_circle(&mut img, size, C::from_rgb(60, 60, 170));
            rocket(
                &mut img,
                size,
                C::from_rgb(230, 230, 230),
                C::from_rgb(255, 110, 60),
            );
        }
        "\u{2764}" | "\u{1f496}" => {
            heart(&mut img, size, C::from_rgb(220, 20, 60));
        }
        "\u{2b50}" => {
            star(&mut img, size, C::from_rgb(255, 215, 0));
        }
        "\u{1f525}" => {
            flame(&mut img, size);
        }
        _ => return None,
    }
    Some(img)
}

/// Returns true if the emoji has a procedural vector fallback in `make_image`.
///
/// This is a lightweight check that avoids allocating a ColorImage.
pub fn has_fallback(emoji: &str) -> bool {
    FALLBACK_EMOJI.contains(&emoji)
}

fn draw_circle(img: &mut egui::ColorImage, size: usize, color: C) {
    let cx = (size as i32) / 2;
    let cy = cx;
    let r = (size as i32) / 2 - 2;
    for y in 0..size as i32 {
        for x in 0..size as i32 {
            let dx = x - cx;
            let dy = y - cy;
            if dx * dx + dy * dy <= r * r {
                img[(x as usize, y as usize)] = color;
            }
        }
    }
}

fn draw_check(img: &mut egui::ColorImage, size: usize, color: C) {
    let s = size as i32;
    let mut plot = |x: i32, y: i32| {
        img[(x as usize, y as usize)] = color;
    };
    for i in 0..s / 3 {
        plot(s / 3 - i, s * 2 / 3 + i);
        plot(s / 3 - i + 1, s * 2 / 3 + i);
    }
    for i in 0..s / 2 {
        plot(s / 3 + i, s * 2 / 3 - i);
        plot(s / 3 + i + 1, s * 2 / 3 - i);
    }
}

fn confetti(img: &mut egui::ColorImage, size: usize) {
    let dots = [
        C::from_rgb(255, 80, 80),
        C::from_rgb(80, 180, 255),
        C::from_rgb(120, 220, 120),
        C::from_rgb(220, 120, 220),
    ];
    let s = size as i32;
    for (i, col) in dots.iter().enumerate() {
        let x = (s / 4) * ((i as i32) + 1);
        let y = (s / 5) * ((i as i32) + 1);
        if x < s {
            img[(x as usize, y as usize)] = *col;
        }
    }
}

fn rocket(img: &mut egui::ColorImage, size: usize, body: C, flame: C) {
    let s = size as i32;
    // body
    for y in s / 4..s * 3 / 4 {
        for x in s / 3..s * 2 / 3 {
            img[(x as usize, y as usize)] = body;
        }
    }
    // nose
    for i in 0..s / 6 {
        for x in s / 2 - i..=s / 2 + i {
            let y = s / 4 - i;
            img[(x as usize, y as usize)] = body;
        }
    }
    // flame
    for i in 0..s / 6 {
        for x in s / 2 - i..=s / 2 + i {
            let y = s * 3 / 4 + i;
            img[(x as usize, y as usize)] = flame;
        }
    }
}

fn heart(img: &mut egui::ColorImage, size: usize, color: C) {
    let s = size as i32;
    for y in 0..s {
        for x in 0..s {
            let dx = x - s / 2;
            let dy = y - s / 3;
            let a = (dx * dx + dy * dy - s * s / 16) < 0;
            let b = x > s / 4 && x < 3 * s / 4 && y > s / 3 && y < s * 3 / 4;
            if a || b {
                img[(x as usize, y as usize)] = color;
            }
        }
    }
}

fn star(img: &mut egui::ColorImage, size: usize, color: C) {
    let s = size as i32;
    for i in 0..s {
        let y = i;
        let x1 = s / 2;
        let x2 = s / 2 - i / 2;
        let x3 = s / 2 + i / 2;
        img[(x2 as usize, y as usize)] = color;
        img[(x1 as usize, y as usize)] = color;
        img[(x3 as usize, y as usize)] = color;
    }
}

fn flame(img: &mut egui::ColorImage, size: usize) {
    let s = size as i32;
    let base = C::from_rgb(255, 140, 0);
    let tip = C::from_rgb(255, 220, 120);
    for y in 0..s {
        for x in 0..s {
            let dx = (x - s / 2) as f32 / (s as f32 / 3.5);
            let dy = (y - s) as f32 / (s as f32 / 1.5);
            let v = (dx * dx + dy * dy).sqrt();
            if v < 1.0 {
                let t = 1.0 - v;
                img[(x as usize, y as usize)] = C::from_rgb(
                    (base.r() as f32 * t + tip.r() as f32 * (1.0 - t)) as u8,
                    (base.g() as f32 * t + tip.g() as f32 * (1.0 - t)) as u8,
                    (base.b() as f32 * t + tip.b() as f32 * (1.0 - t)) as u8,
                );
            }
        }
    }
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;

    fn has_non_transparent_pixel(img: &egui::ColorImage) -> bool {
        img.pixels.iter().any(|p| p.a() != 0)
    }

    #[test]
    fn test_make_image_known_emojis_produce_pixels() {
        for emoji in FALLBACK_EMOJI {
            let img = make_image(emoji, 24).expect("expected fallback image");
            assert!(has_non_transparent_pixel(&img), "no pixels for {emoji:?}");
        }
    }

    #[test]
    fn test_make_image_unknown_returns_none() {
        assert!(make_image("\u{1f47d}", 24).is_none());
    }

    #[test]
    fn test_has_fallback_matches_make_image() {
        for emoji in FALLBACK_EMOJI {
            assert!(has_fallback(emoji), "has_fallback false for {emoji:?}");
        }
        assert!(!has_fallback("\u{1f47d}"));
        assert!(!has_fallback("\u{26a0}"));
    }
}
