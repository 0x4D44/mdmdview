use std::collections::HashMap;

/// Returns the embedded PNG bytes for a known emoji character.
///
/// This function uses pattern matching and never performs array indexing,
/// so it cannot panic regardless of input. Unknown emoji characters
/// return `None`.
///
/// The catalog covers 80 emojis across categories: faces, status indicators,
/// objects/tools, arrows, communication, nature/symbols, people, dev/tech,
/// and miscellaneous.
pub fn image_bytes_for(emoji: &str) -> Option<&'static [u8]> {
    match emoji {
        // --- Original 18 emojis ---
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
        // --- Status/Indicators (9) ---
        "\u{26a0}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/26a0.png"
        ))),
        "\u{274c}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/274c.png"
        ))),
        "\u{2728}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2728.png"
        ))),
        "\u{1f4af}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4af.png"
        ))),
        "\u{23f3}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/23f3.png"
        ))),
        "\u{1f504}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f504.png"
        ))),
        "\u{2714}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2714.png"
        ))),
        "\u{274e}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/274e.png"
        ))),
        "\u{1f6ab}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f6ab.png"
        ))),
        // --- Objects/Tools (14) ---
        "\u{1f517}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f517.png"
        ))),
        "\u{1f4cb}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4cb.png"
        ))),
        "\u{1f4c1}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4c1.png"
        ))),
        "\u{1f4c2}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4c2.png"
        ))),
        "\u{1f512}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f512.png"
        ))),
        "\u{1f513}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f513.png"
        ))),
        "\u{2699}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2699.png"
        ))),
        "\u{1f50d}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f50d.png"
        ))),
        "\u{1f4ca}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4ca.png"
        ))),
        "\u{1f4c8}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4c8.png"
        ))),
        "\u{1f4c9}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4c9.png"
        ))),
        "\u{1f4cc}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4cc.png"
        ))),
        "\u{1f528}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f528.png"
        ))),
        "\u{270f}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/270f.png"
        ))),
        // --- Arrows (4) ---
        "\u{27a1}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/27a1.png"
        ))),
        "\u{2b06}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2b06.png"
        ))),
        "\u{2b07}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2b07.png"
        ))),
        "\u{2b05}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2b05.png"
        ))),
        // --- Communication (3) ---
        "\u{1f4ac}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4ac.png"
        ))),
        "\u{1f4e2}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4e2.png"
        ))),
        "\u{1f4e7}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4e7.png"
        ))),
        // --- Nature/Symbols (8) ---
        "\u{2764}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2764.png"
        ))),
        "\u{1f48e}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f48e.png"
        ))),
        "\u{1f31f}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f31f.png"
        ))),
        "\u{1f3af}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f3af.png"
        ))),
        "\u{1f3a8}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f3a8.png"
        ))),
        "\u{1f6e1}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f6e1.png"
        ))),
        "\u{1f30a}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f30a.png"
        ))),
        "\u{26a1}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/26a1.png"
        ))),
        // --- People/Faces (5) ---
        "\u{1f440}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f440.png"
        ))),
        "\u{1f914}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f914.png"
        ))),
        "\u{1f44b}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f44b.png"
        ))),
        "\u{1f64f}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f64f.png"
        ))),
        "\u{1f4aa}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4aa.png"
        ))),
        // --- Dev/Tech (6) ---
        "\u{1f41b}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f41b.png"
        ))),
        "\u{1f6a7}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f6a7.png"
        ))),
        "\u{1f3d7}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f3d7.png"
        ))),
        "\u{1f4bb}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4bb.png"
        ))),
        "\u{1f5a5}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f5a5.png"
        ))),
        "\u{2328}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2328.png"
        ))),
        // --- Misc (13) ---
        "\u{1f3c6}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f3c6.png"
        ))),
        "\u{1f3aa}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f3aa.png"
        ))),
        "\u{1f4d6}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4d6.png"
        ))),
        "\u{1f4da}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f4da.png"
        ))),
        "\u{1f514}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f514.png"
        ))),
        "\u{1f515}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f515.png"
        ))),
        "\u{1f5c2}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f5c2.png"
        ))),
        "\u{2139}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2139.png"
        ))),
        "\u{1f534}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f534.png"
        ))),
        "\u{1f7e2}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f7e2.png"
        ))),
        "\u{1f7e1}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f7e1.png"
        ))),
        "\u{1f535}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/1f535.png"
        ))),
        "\u{2611}" => Some(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/emoji/2611.png"
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
            // --- Original 19 shortcodes ---
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
            // --- Status/Indicators ---
            (":warning:", "\u{26a0}"),
            (":x:", "\u{274c}"),
            (":sparkles:", "\u{2728}"),
            (":100:", "\u{1f4af}"),
            (":hourglass:", "\u{23f3}"),
            (":recycle:", "\u{1f504}"),
            (":heavy_check_mark:", "\u{2714}"),
            (":negative_squared_cross_mark:", "\u{274e}"),
            (":no_entry_sign:", "\u{1f6ab}"),
            // --- Objects/Tools ---
            (":link:", "\u{1f517}"),
            (":clipboard:", "\u{1f4cb}"),
            (":file_folder:", "\u{1f4c1}"),
            (":open_file_folder:", "\u{1f4c2}"),
            (":lock:", "\u{1f512}"),
            (":unlock:", "\u{1f513}"),
            (":gear:", "\u{2699}"),
            (":mag:", "\u{1f50d}"),
            (":bar_chart:", "\u{1f4ca}"),
            (":chart_with_upwards_trend:", "\u{1f4c8}"),
            (":chart_with_downwards_trend:", "\u{1f4c9}"),
            (":pushpin:", "\u{1f4cc}"),
            (":hammer:", "\u{1f528}"),
            (":pencil2:", "\u{270f}"),
            // --- Arrows ---
            (":arrow_right:", "\u{27a1}"),
            (":arrow_up:", "\u{2b06}"),
            (":arrow_down:", "\u{2b07}"),
            (":arrow_left:", "\u{2b05}"),
            // --- Communication ---
            (":speech_balloon:", "\u{1f4ac}"),
            (":loudspeaker:", "\u{1f4e2}"),
            (":email:", "\u{1f4e7}"),
            // --- Nature/Symbols ---
            (":heart:", "\u{2764}"),
            (":gem:", "\u{1f48e}"),
            (":star2:", "\u{1f31f}"),
            (":dart:", "\u{1f3af}"),
            (":target:", "\u{1f3af}"),
            (":art:", "\u{1f3a8}"),
            (":shield:", "\u{1f6e1}"),
            (":ocean:", "\u{1f30a}"),
            (":zap:", "\u{26a1}"),
            // --- People/Faces ---
            (":eyes:", "\u{1f440}"),
            (":thinking:", "\u{1f914}"),
            (":wave:", "\u{1f44b}"),
            (":pray:", "\u{1f64f}"),
            (":muscle:", "\u{1f4aa}"),
            // --- Dev/Tech ---
            (":bug:", "\u{1f41b}"),
            (":construction:", "\u{1f6a7}"),
            (":building_construction:", "\u{1f3d7}"),
            (":computer:", "\u{1f4bb}"),
            (":desktop_computer:", "\u{1f5a5}"),
            (":keyboard:", "\u{2328}"),
            // --- Misc ---
            (":trophy:", "\u{1f3c6}"),
            (":circus_tent:", "\u{1f3aa}"),
            (":book:", "\u{1f4d6}"),
            (":open_book:", "\u{1f4d6}"),
            (":books:", "\u{1f4da}"),
            (":bell:", "\u{1f514}"),
            (":no_bell:", "\u{1f515}"),
            (":card_index_dividers:", "\u{1f5c2}"),
            (":information_source:", "\u{2139}"),
            (":red_circle:", "\u{1f534}"),
            (":green_circle:", "\u{1f7e2}"),
            (":yellow_circle:", "\u{1f7e1}"),
            (":large_blue_circle:", "\u{1f535}"),
            (":ballot_box_with_check:", "\u{2611}"),
            // --- Aliases ---
            (":+1:", "\u{1f44d}"),
            (":-1:", "\u{1f44e}"),
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

    /// All 80 emoji codepoints in the catalog.
    const ALL_EMOJI_CODEPOINTS: [&str; 80] = [
        // Original 18
        "\u{1f389}", "\u{2705}", "\u{1f680}", "\u{1f642}", "\u{1f600}",
        "\u{1f609}", "\u{2b50}", "\u{1f525}", "\u{1f44d}", "\u{1f44e}",
        "\u{1f4a1}", "\u{2753}", "\u{2757}", "\u{1f4dd}", "\u{1f9e0}",
        "\u{1f9ea}", "\u{1f4e6}", "\u{1f527}",
        // Status/Indicators (9)
        "\u{26a0}", "\u{274c}", "\u{2728}", "\u{1f4af}", "\u{23f3}",
        "\u{1f504}", "\u{2714}", "\u{274e}", "\u{1f6ab}",
        // Objects/Tools (14)
        "\u{1f517}", "\u{1f4cb}", "\u{1f4c1}", "\u{1f4c2}", "\u{1f512}",
        "\u{1f513}", "\u{2699}", "\u{1f50d}", "\u{1f4ca}", "\u{1f4c8}",
        "\u{1f4c9}", "\u{1f4cc}", "\u{1f528}", "\u{270f}",
        // Arrows (4)
        "\u{27a1}", "\u{2b06}", "\u{2b07}", "\u{2b05}",
        // Communication (3)
        "\u{1f4ac}", "\u{1f4e2}", "\u{1f4e7}",
        // Nature/Symbols (8)
        "\u{2764}", "\u{1f48e}", "\u{1f31f}", "\u{1f3af}", "\u{1f3a8}",
        "\u{1f6e1}", "\u{1f30a}", "\u{26a1}",
        // People/Faces (5)
        "\u{1f440}", "\u{1f914}", "\u{1f44b}", "\u{1f64f}", "\u{1f4aa}",
        // Dev/Tech (6)
        "\u{1f41b}", "\u{1f6a7}", "\u{1f3d7}", "\u{1f4bb}", "\u{1f5a5}",
        "\u{2328}",
        // Misc (13)
        "\u{1f3c6}", "\u{1f3aa}", "\u{1f4d6}", "\u{1f4da}", "\u{1f514}",
        "\u{1f515}", "\u{1f5c2}", "\u{2139}", "\u{1f534}", "\u{1f7e2}",
        "\u{1f7e1}", "\u{1f535}", "\u{2611}",
    ];

    #[test]
    fn test_image_bytes_for_all_known_emojis() {
        for emoji in ALL_EMOJI_CODEPOINTS {
            let bytes = image_bytes_for(emoji)
                .unwrap_or_else(|| panic!("missing embedded bytes for {emoji:?}"));
            assert!(!bytes.is_empty(), "empty bytes for {emoji:?}");
        }
    }

    #[test]
    fn test_catalog_count() {
        let mut count = 0;
        for emoji in ALL_EMOJI_CODEPOINTS {
            if image_bytes_for(emoji).is_some() {
                count += 1;
            }
        }
        assert_eq!(count, 80, "expected 80 emojis in catalog");
    }

    #[test]
    fn test_image_bytes_for_unknown_emoji() {
        assert!(image_bytes_for("\u{1f47d}").is_none());
    }

    #[test]
    fn test_shortcode_map_contains_expected_entries() {
        let map = shortcode_map();
        // Original entries
        assert_eq!(map.get(":tada:"), Some(&"\u{1f389}"));
        assert_eq!(map.get(":white_check_mark:"), Some(&"\u{2705}"));
        assert_eq!(map.get(":rocket:"), Some(&"\u{1f680}"));
        assert_eq!(map.get(":fire:"), Some(&"\u{1f525}"));
        assert_eq!(map.get(":wrench:"), Some(&"\u{1f527}"));
        // New entries
        assert_eq!(map.get(":warning:"), Some(&"\u{26a0}"));
        assert_eq!(map.get(":bug:"), Some(&"\u{1f41b}"));
        assert_eq!(map.get(":heart:"), Some(&"\u{2764}"));
        assert_eq!(map.get(":eyes:"), Some(&"\u{1f440}"));
        assert_eq!(map.get(":trophy:"), Some(&"\u{1f3c6}"));
        // Aliases
        assert_eq!(map.get(":+1:"), Some(&"\u{1f44d}"));
        assert_eq!(map.get(":-1:"), Some(&"\u{1f44e}"));
        assert_eq!(map.get(":target:"), Some(&"\u{1f3af}"));
        assert_eq!(map.get(":open_book:"), Some(&"\u{1f4d6}"));
        // Unknown
        assert!(map.get(":does_not_exist:").is_none());
    }

    #[test]
    fn test_shortcode_count() {
        let map = shortcode_map();
        // 19 original + 68 new = 87 total (including aliases)
        assert!(
            map.len() >= 85,
            "expected at least 85 shortcodes, got {}",
            map.len()
        );
    }

    #[test]
    fn test_all_shortcodes_map_to_catalogued_emojis() {
        let map = shortcode_map();
        for (shortcode, emoji) in map.iter() {
            assert!(
                image_bytes_for(emoji).is_some(),
                "shortcode {shortcode} maps to {emoji:?} which has no embedded PNG"
            );
        }
    }

    #[test]
    fn test_emoji_for_shortcode() {
        // Known shortcodes return Some
        assert_eq!(emoji_for_shortcode(":rocket:"), Some("\u{1f680}"));
        assert_eq!(emoji_for_shortcode(":tada:"), Some("\u{1f389}"));
        assert_eq!(emoji_for_shortcode(":fire:"), Some("\u{1f525}"));
        assert_eq!(emoji_for_shortcode(":bug:"), Some("\u{1f41b}"));
        assert_eq!(emoji_for_shortcode(":zap:"), Some("\u{26a1}"));

        // Unknown shortcodes return None (no panic)
        assert_eq!(emoji_for_shortcode(":unknown:"), None);
        assert_eq!(emoji_for_shortcode(""), None);
        assert_eq!(emoji_for_shortcode("no_colons"), None);
        assert_eq!(emoji_for_shortcode(":partial"), None);
    }
}
