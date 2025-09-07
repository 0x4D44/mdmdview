use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

fn main() -> anyhow::Result<()> {
    // Generate a tiny 96x64 checkerboard gradient WEBP under assets/samples/
    let width = 96u32;
    let height = 64u32;
    let mut img = image::RgbaImage::new(width, height);

    for y in 0..height {
        for x in 0..width {
            let cx = (x / 12) % 2;
            let cy = (y / 12) % 2;
            let checker = (cx ^ cy) != 0;
            let r = ((x as f32 / width as f32) * 255.0) as u8;
            let g = ((y as f32 / height as f32) * 255.0) as u8;
            let b = if checker { 220 } else { 60 };
            img.put_pixel(x, y, image::Rgba([r, g, b, 255]));
        }
    }

    let out_path = Path::new("assets/samples/webp_sample.webp");
    if let Some(parent) = out_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let f = File::create(out_path)?;
    let w = BufWriter::new(f);

    // Encode to WEBP using image crate's WebPEncoder
    let encoder = image::codecs::webp::WebPEncoder::new_lossless(w);
    encoder.encode(&img, width, height, image::ColorType::Rgba8)?;

    println!("Generated {}", out_path.display());
    Ok(())
}
