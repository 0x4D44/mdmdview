use egui::ColorImage;

pub(crate) fn svg_bytes_to_color_image(bytes: &[u8]) -> Option<(ColorImage, u32, u32)> {
    svg_bytes_to_color_image_with_bg(bytes, None)
}

pub(crate) fn svg_bytes_to_color_image_with_bg(
    bytes: &[u8],
    bg: Option<[u8; 4]>,
) -> Option<(ColorImage, u32, u32)> {
    let mut opt = usvg::Options::default();
    let mut db = usvg::fontdb::Database::new();
    db.load_system_fonts();
    opt.fontdb = std::sync::Arc::new(db);
    let tree = usvg::Tree::from_data(bytes, &opt).ok()?;
    let sz = tree.size();
    let pix = sz.to_int_size();
    let (mut w, mut h) = (pix.width(), pix.height());
    if w == 0 || h == 0 {
        w = 256;
        h = 256;
    }
    let max_side: u32 = 4096;
    if w > max_side || h > max_side {
        let scale = (max_side as f32 / w as f32).min(max_side as f32 / h as f32);
        w = (w as f32 * scale) as u32;
        h = (h as f32 * scale) as u32;
    }
    let mut pixmap = tiny_skia::Pixmap::new(w, h)?;
    if let Some([r, g, b, a]) = bg {
        let color = tiny_skia::Color::from_rgba8(r, g, b, a);
        pixmap.fill(color);
    }
    let mut pmut = pixmap.as_mut();
    resvg::render(&tree, tiny_skia::Transform::identity(), &mut pmut);
    let data = pixmap.data();
    let img = ColorImage::from_rgba_unmultiplied([w as usize, h as usize], data);
    Some((img, w, h))
}

pub(crate) fn bytes_to_color_image_guess(
    bytes: &[u8],
    bg: Option<[u8; 4]>,
) -> Option<(ColorImage, u32, u32)> {
    if let Ok(img) = image::load_from_memory(bytes) {
        let rgba = img.to_rgba8();
        let (w, h) = rgba.dimensions();
        let mut ci = ColorImage::from_rgba_unmultiplied([w as usize, h as usize], &rgba);
        if let Some([br, bgc, bb, ba]) = bg {
            for p in ci.pixels.iter_mut() {
                let a = p[3] as f32 / 255.0;
                let inv = 1.0 - a;
                p[0] = (a * p[0] as f32 + inv * br as f32) as u8;
                p[1] = (a * p[1] as f32 + inv * bgc as f32) as u8;
                p[2] = (a * p[2] as f32 + inv * bb as f32) as u8;
                p[3] = ba;
            }
        }
        return Some((ci, w, h));
    }
    svg_bytes_to_color_image_with_bg(bytes, bg)
}
