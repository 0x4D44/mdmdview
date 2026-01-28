use std::collections::HashMap;

/// Returns the embedded PNG bytes for a known emoji character.
///
/// This function uses pattern matching and never performs array indexing,
/// so it cannot panic regardless of input. Unknown emoji characters
/// return `None`.
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

/// Returns a reference to the static shortcode-to-emoji mapping.
///
/// Callers should use `.get()` for safe lookups that return `Option`.
/// For convenience, prefer using `emoji_for_shortcode()` which wraps
/// this safely.
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

/// Safely looks up an emoji by its shortcode (e.g., ":rocket:").
///
/// Returns `Some(emoji_str)` if the shortcode is known, `None` otherwise.
/// This function never panics regardless of input.
#[inline]
pub fn emoji_for_shortcode(shortcode: &str) -> Option<&'static str> {
    shortcode_map().get(shortcode).copied()
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;

    #[test]
    fn test_image_bytes_for_known_emojis() {
        let emojis = [
            "\u{1f389}",
            "\u{2705}",
            "\u{1f680}",
            "\u{1f642}",
            "\u{1f600}",
            "\u{1f609}",
            "\u{2b50}",
            "\u{1f525}",
            "\u{1f44d}",
            "\u{1f44e}",
            "\u{1f4a1}",
            "\u{2753}",
            "\u{2757}",
            "\u{1f4dd}",
            "\u{1f9e0}",
            "\u{1f9ea}",
            "\u{1f4e6}",
            "\u{1f527}",
        ];

        for emoji in emojis {
            let bytes = image_bytes_for(emoji).expect("expected embedded emoji bytes");
            assert!(!bytes.is_empty());
        }
    }

    #[test]
    fn test_image_bytes_for_unknown_emoji() {
        assert!(image_bytes_for("\u{1f47d}").is_none());
    }

    #[test]
    fn test_shortcode_map_contains_expected_entries() {
        let map = shortcode_map();
        assert_eq!(map.get(":tada:"), Some(&"\u{1f389}"));
        assert_eq!(map.get(":white_check_mark:"), Some(&"\u{2705}"));
        assert_eq!(map.get(":rocket:"), Some(&"\u{1f680}"));
        assert_eq!(map.get(":fire:"), Some(&"\u{1f525}"));
        assert_eq!(map.get(":wrench:"), Some(&"\u{1f527}"));
        assert!(map.get(":does_not_exist:").is_none());
    }

    #[test]
    fn test_emoji_for_shortcode() {
        // Known shortcodes return Some
        assert_eq!(emoji_for_shortcode(":rocket:"), Some("\u{1f680}"));
        assert_eq!(emoji_for_shortcode(":tada:"), Some("\u{1f389}"));
        assert_eq!(emoji_for_shortcode(":fire:"), Some("\u{1f525}"));

        // Unknown shortcodes return None (no panic)
        assert_eq!(emoji_for_shortcode(":unknown:"), None);
        assert_eq!(emoji_for_shortcode(""), None);
        assert_eq!(emoji_for_shortcode("no_colons"), None);
        assert_eq!(emoji_for_shortcode(":partial"), None);
    }
}
