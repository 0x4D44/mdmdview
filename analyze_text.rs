fn main() {
    let text = "A 32-bit address bus would provide 2^32 = 4,294,967,296 bytes (4 GB) of addressable memory. In 1985, when typical PCs had 256 KB to 1 MB of RAM, 4 GB seemed nearly infinite.";

    println!("Analyzing text character by character:");
    println!("Total length: {} bytes", text.len());
    println!();

    for (i, ch) in text.chars().enumerate() {
        let code = ch as u32;
        if code > 127 {
            println!("Position {}: '{}' (U+{:04X}) - NON-ASCII", i, ch, code);
        } else if ch.is_control() {
            println!("Position {}: (U+{:04X}) - CONTROL CHARACTER", i, code);
        }
    }

    println!("\n=== Full character dump ===");
    for (i, ch) in text.chars().enumerate() {
        if i > 0 && i % 10 == 0 {
            println!();
        }
        print!("{}:'{}' ", i, if ch.is_control() { '□' } else { ch });
    }
    println!();
}