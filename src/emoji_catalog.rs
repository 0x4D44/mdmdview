use std::collections::HashMap;

// Return embedded emoji sprites keyed by real Unicode emoji characters.
pub fn image_bytes_for(emoji: &str) -> Option<&'static [u8]> {
    match emoji {
        "\u{1f389}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f389.png"
        ))),
        "\u{2705}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2705.png"
        ))),
        "\u{1f680}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f680.png"
        ))),
        "\u{1f642}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f642.png"
        ))),
        "\u{1f600}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f600.png"
        ))),
        "\u{1f609}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f609.png"
        ))),
        "\u{2b50}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2b50.png"
        ))),
        "\u{1f525}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f525.png"
        ))),
        "\u{1f44d}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f44d.png"
        ))),
        "\u{1f44e}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f44e.png"
        ))),
        "\u{1f4a1}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4a1.png"
        ))),
        "\u{2753}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2753.png"
        ))),
        "\u{2757}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2757.png"
        ))),
        "\u{1f4dd}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4dd.png"
        ))),
        "\u{1f9e0}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f9e0.png"
        ))),
        "\u{1f9ea}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f9ea.png"
        ))),
        "\u{1f4e6}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4e6.png"
        ))),
        "\u{1f527}" => Some(include_bytes!(concat!(
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
            (":tada:", "\u{1f389}"),
            (":white_check_mark:", "\u{2705}"),
            (":check_mark:", "\u{2705}"),
            (":rocket:", "\u{1f680}"),
            (":slightly_smiling_face:", "\u{1f642}"),
            (":grinning:", "\u{1f600}"),
            (":wink:", "\u{1f609}"),
            (":star:", "\u{2b50}"),
            (":fire:", "\u{1f525}"),
            (":thumbsup:", "\u{1f44d}"),
            (":thumbsdown:", "\u{1f44e}"),
            (":bulb:", "\u{1f4a1}"),
            (":question:", "\u{2753}"),
            (":exclamation:", "\u{2757}"),
            (":memo:", "\u{1f4dd}"),
            (":brain:", "\u{1f9e0}"),
            (":test_tube:", "\u{1f9ea}"),
            (":package:", "\u{1f4e6}"),
            (":wrench:", "\u{1f527}"),
        ])
    })
}
