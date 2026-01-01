use egui::ColorImage;
use std::io::Cursor;

const MAX_IMAGE_SIDE: u32 = 4096;
const MAX_IMAGE_PIXELS: u64 = MAX_IMAGE_SIDE as u64 * MAX_IMAGE_SIDE as u64;
const MAX_IMAGE_BYTES: usize = 64 * 1024 * 1024;

pub(crate) fn svg_bytes_to_color_image(bytes: &[u8]) -> Option<(ColorImage, u32, u32)> {
    svg_bytes_to_color_image_with_bg(bytes, None)
}

fn raster_dimensions(bytes: &[u8]) -> Option<(u32, u32)> {
    let reader = image::io::Reader::new(Cursor::new(bytes))
        .with_guessed_format()
        .ok()?;
    reader.into_dimensions().ok()
}

fn raster_exceeds_limits(w: u32, h: u32) -> bool {
    if w == 0 || h == 0 {
        return true;
    }
    if w > MAX_IMAGE_SIDE || h > MAX_IMAGE_SIDE {
        return true;
    }
    let pixels = (w as u64).saturating_mul(h as u64);
    pixels > MAX_IMAGE_PIXELS
}

pub(crate) fn raster_bytes_to_color_image_with_bg(
    bytes: &[u8],
    bg: Option<[u8; 4]>,
) -> Option<(ColorImage, u32, u32)> {
    if bytes.len() > MAX_IMAGE_BYTES {
        return None;
    }
    if let Some((w, h)) = raster_dimensions(bytes) {
        if raster_exceeds_limits(w, h) {
            return None;
        }
    }
    let img = image::load_from_memory(bytes).ok()?;
    let rgba = img.to_rgba8();
    let (w, h) = rgba.dimensions();
    if raster_exceeds_limits(w, h) {
        return None;
    }
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
    Some((ci, w, h))
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
    let max_side: u32 = MAX_IMAGE_SIDE;
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
    if let Some(img) = raster_bytes_to_color_image_with_bg(bytes, bg) {
        return Some(img);
    }
    svg_bytes_to_color_image_with_bg(bytes, bg)
}

#[cfg(test)]
mod tests {
    use super::*;
    use image::{ImageOutputFormat, RgbaImage};
    use std::io::Cursor;

    fn encode_png(rgba: &RgbaImage) -> Vec<u8> {
        let mut bytes = Vec::new();
        let mut cursor = Cursor::new(&mut bytes);
        image::DynamicImage::ImageRgba8(rgba.clone())
            .write_to(&mut cursor, ImageOutputFormat::Png)
            .expect("encode png");
        bytes
    }

    #[test]
    fn test_bytes_to_color_image_guess_png_blends_background() {
        let mut rgba = RgbaImage::new(1, 1);
        rgba.put_pixel(0, 0, image::Rgba([255, 0, 0, 255]));
        let bytes = encode_png(&rgba);

        let bg = [0, 0, 255, 200];
        let (img, w, h) =
            bytes_to_color_image_guess(&bytes, Some(bg)).expect("png decode");
        assert_eq!((w, h), (1, 1));

        let px = img.pixels[0];
        assert_eq!(px[0], 255);
        assert_eq!(px[1], 0);
        assert_eq!(px[2], 0);
        assert_eq!(px[3], bg[3]);
    }

    #[test]
    fn test_svg_bytes_to_color_image_scales_and_fills_bg() {
        let svg = r#"<svg width="5000" height="3000" xmlns="http://www.w3.org/2000/svg">
<rect width="5000" height="3000" fill="red"/>
</svg>"#;
        let bg = Some([1, 2, 3, 255]);
        let (_img, w, h) =
            svg_bytes_to_color_image_with_bg(svg.as_bytes(), bg).expect("svg decode");
        assert!(w > 0);
        assert!(h > 0);
        assert!(w <= 4096);
        assert!(h <= 4096);
    }

    #[test]
    fn test_svg_bytes_to_color_image_invalid_returns_none() {
        assert!(svg_bytes_to_color_image(b"not svg").is_none());
    }

    #[test]
    fn test_raster_bytes_rejects_large_dimensions() {
        let mut rgba = RgbaImage::new(5000, 1);
        rgba.put_pixel(0, 0, image::Rgba([0, 0, 0, 255]));
        let bytes = encode_png(&rgba);
        assert!(raster_bytes_to_color_image_with_bg(&bytes, None).is_none());
    }

    #[test]
    fn test_bytes_to_color_image_guess_svg_fallback() {
        let svg = r#"<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg">
<circle cx="32" cy="32" r="20" fill="blue"/>
</svg>"#;
        let (_img, w, h) =
            bytes_to_color_image_guess(svg.as_bytes(), None).expect("svg fallback");
        assert_eq!((w, h), (64, 64));
    }
}
