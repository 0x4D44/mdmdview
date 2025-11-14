use egui::Color32 as C;

pub fn make_image(emoji: &str, size: usize) -> Option<egui::ColorImage> {
    let mut img = egui::ColorImage::new([size, size], C::TRANSPARENT);
    match emoji {
        "✅" => {
            draw_circle(&mut img, size, C::from_rgb(34, 139, 34));
            draw_check(&mut img, size, C::WHITE);
            Some(img)
        }
        "🎉" => {
            draw_circle(&mut img, size, C::from_rgb(255, 215, 0));
            confetti(&mut img, size);
            Some(img)
        }
        "🚀" => {
            draw_circle(&mut img, size, C::from_rgb(60, 60, 170));
            rocket(
                &mut img,
                size,
                C::from_rgb(230, 230, 230),
                C::from_rgb(255, 110, 60),
            );
            Some(img)
        }
        "❤️" | "❤" => {
            heart(&mut img, size, C::from_rgb(220, 20, 60));
            Some(img)
        }
        "⭐" => {
            star(&mut img, size, C::from_rgb(255, 215, 0));
            Some(img)
        }
        "🔥" => {
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
    let cx1 = s / 2 - s / 6;
    let cx2 = s / 2 + s / 6;
    let cy = s / 3;
    let r = s / 6;
    for y in 0..s {
        for x in 0..s {
            let d1 = (x - cx1) * (x - cx1) + (y - cy) * (y - cy) <= r * r;
            let d2 = (x - cx2) * (x - cx2) + (y - cy) * (y - cy) <= r * r;
            let tri = y >= cy
                && (x >= s / 2 - r)
                && (x <= s / 2 + r)
                && (y - cy <= (r * 2 - (x - s / 2).abs()));
            if d1 || d2 || tri {
                img[(x as usize, y as usize)] = color;
            }
        }
    }
}

fn star(img: &mut egui::ColorImage, size: usize, color: C) {
    let s = size as i32;
    let cx = s / 2;
    let cy = s / 2;
    let r = s / 2 - 2;
    for t in 0..360 {
        let rad = (t as f32).to_radians();
        let rr = if t % 72 < 36 { r } else { r / 2 };
        let x = cx + (rr as f32 * rad.cos()) as i32;
        let y = cy + (rr as f32 * rad.sin()) as i32;
        if x >= 0 && y >= 0 && x < s && y < s {
            img[(x as usize, y as usize)] = color;
        }
    }
}

fn flame(img: &mut egui::ColorImage, size: usize) {
    let s = size as i32;
    for y in 0..s {
        for x in 0..s {
            let t = y as f32 / s as f32;
            let r = 255u8;
            let g = (160.0 * (1.0 - t)) as u8;
            let b = (40.0 * (1.0 - t)) as u8;
            if x.abs_diff(s / 2) as f32 + t * (s as f32 / 2.0) < s as f32 / 2.0 {
                img[(x as usize, y as usize)] = C::from_rgb(r, g, b);
            }
        }
    }
}
