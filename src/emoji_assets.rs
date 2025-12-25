use egui::Color32 as C;

// Draw simple vector fallback icons for a subset of emoji.
pub fn make_image(emoji: &str, size: usize) -> Option<egui::ColorImage> {
    let mut img = egui::ColorImage::new([size, size], C::TRANSPARENT);
    match emoji {
        "\u{2705}" => {
            draw_circle(&mut img, size, C::from_rgb(34, 139, 34));
            draw_check(&mut img, size, C::WHITE);
            Some(img)
        }
        "\u{1f389}" => {
            draw_circle(&mut img, size, C::from_rgb(255, 215, 0));
            confetti(&mut img, size);
            Some(img)
        }
        "\u{1f680}" => {
            draw_circle(&mut img, size, C::from_rgb(60, 60, 170));
            rocket(
                &mut img,
                size,
                C::from_rgb(230, 230, 230),
                C::from_rgb(255, 110, 60),
            );
            Some(img)
        }
        "\u{2764}" | "\u{1f496}" => {
            heart(&mut img, size, C::from_rgb(220, 20, 60));
            Some(img)
        }
        "\u{2b50}" => {
            star(&mut img, size, C::from_rgb(255, 215, 0));
            Some(img)
        }
        "\u{1f525}" => {
            flame(&mut img, size);
            Some(img)
        }
        _ => None,
    }
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
        if x >= 0 && y >= 0 && x < s && y < s {
            img[(x as usize, y as usize)] = color;
        }
    };
    // simple check mark
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
        if x < s && y < s {
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
            if y >= 0 {
                img[(x as usize, y as usize)] = body;
            }
        }
    }
    // flame
    for i in 0..s / 6 {
        for x in s / 2 - i..=s / 2 + i {
            let y = s * 3 / 4 + i;
            if y < s {
                img[(x as usize, y as usize)] = flame;
            }
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
        if x2 >= 0 {
            img[(x2 as usize, y as usize)] = color;
        }
        img[(x1 as usize, y as usize)] = color;
        if x3 < s {
            img[(x3 as usize, y as usize)] = color;
        }
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
mod tests {
    use super::*;

    fn has_non_transparent_pixel(img: &egui::ColorImage) -> bool {
        img.pixels.iter().any(|p| p.a() != 0)
    }

    #[test]
    fn test_make_image_known_emojis_produce_pixels() {
        let emojis = [
            "\u{2705}",
            "\u{1f389}",
            "\u{1f680}",
            "\u{2764}",
            "\u{1f496}",
            "\u{2b50}",
            "\u{1f525}",
        ];

        for emoji in emojis {
            let img = make_image(emoji, 24).expect("expected fallback image");
            assert!(has_non_transparent_pixel(&img));
        }
    }

    #[test]
    fn test_make_image_unknown_returns_none() {
        assert!(make_image("\u{1f47d}", 24).is_none());
    }
}
