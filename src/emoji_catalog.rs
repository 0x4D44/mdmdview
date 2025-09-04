use std::collections::HashMap;

pub fn image_bytes_for(emoji: &str) -> Option<&'static [u8]> {
    match emoji {
        "🎉" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f389.png"
        ))),
        "✅" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2705.png"
        ))),
        "🚀" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f680.png"
        ))),
        "🙂" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f642.png"
        ))),
        "😀" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f600.png"
        ))),
        "😉" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f609.png"
        ))),
        "⭐" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2b50.png"
        ))),
        "🔥" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f525.png"
        ))),
        "👍" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f44d.png"
        ))),
        "👎" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f44e.png"
        ))),
        "💡" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4a1.png"
        ))),
        "❓" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2753.png"
        ))),
        "❗" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2757.png"
        ))),
        "📝" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4dd.png"
        ))),
        "🧠" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f9e0.png"
        ))),
        "🧪" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f9ea.png"
        ))),
        "📦" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4e6.png"
        ))),
        "🔧" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f527.png"
        ))),
        _ => None,
    }
}

pub fn shortcode_map() -> &'static HashMap<&'static str, &'static str> {
    use std::sync::OnceLock;
    static MAP: OnceLock<HashMap<&'static str, &'static str>> = OnceLock::new();
    MAP.get_or_init(|| {
        HashMap::from([
            (":tada:", "🎉"),
            (":white_check_mark:", "✅"),
            (":check_mark:", "✅"),
            (":rocket:", "🚀"),
            (":slightly_smiling_face:", "🙂"),
            (":grinning:", "😀"),
            (":wink:", "😉"),
            (":star:", "⭐"),
            (":fire:", "🔥"),
            (":thumbsup:", "👍"),
            (":thumbsdown:", "👎"),
            (":bulb:", "💡"),
            (":question:", "❓"),
            (":exclamation:", "❗"),
            (":memo:", "📝"),
            (":brain:", "🧠"),
            (":test_tube:", "🧪"),
            (":package:", "📦"),
            (":wrench:", "🔧"),
        ])
    })
}
