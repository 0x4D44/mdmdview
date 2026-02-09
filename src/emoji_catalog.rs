// emoji_catalog.rs -- Embedded Twemoji PNG lookup and shortcode-to-emoji mapping.
//
// The catalog embeds 80 Twemoji PNG files at compile time via `include_bytes!`.
// Two lookup paths are provided:
//   image_bytes_for(emoji)       -> Option<&[u8]>   -- codepoint to PNG bytes
//   emoji_for_shortcode(code)    -> Option<&str>     -- shortcode to codepoint
//   shortcode_map()              -> &HashMap         -- full shortcode table
//
// Adding a new emoji requires:
//   1. Place the PNG in assets/emoji/<hex>.png
//   2. Add the codepoint to the `emoji_png!` invocation in `image_bytes_for`
//   3. Add the shortcode mapping in `shortcode_map`
//   4. Add the codepoint to `ALL_EMOJI_CODEPOINTS` in tests

use std::collections::HashMap;

/// Generates a match expression that maps Unicode literals to embedded PNG bytes.
/// Eliminates the repetitive `Some(include_bytes!(concat!(...)))` boilerplate.
macro_rules! emoji_png {
    ( $input:expr; $( $unicode:literal => $file:literal ),+ $(,)? ) => {
        match $input {
            $( $unicode => Some(include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/emoji/", $file))), )+
            _ => None,
        }
    };
}

/// Returns the embedded PNG bytes for a known emoji character.
///
/// Uses pattern matching only -- cannot panic regardless of input.
/// Unknown emoji characters return `None`.
pub fn image_bytes_for(emoji: &str) -> Option<&'static [u8]> {
    emoji_png! { emoji;
        // --- Faces (4) ---
        "\u{1f642}" => "1f642.png",
        "\u{1f600}" => "1f600.png",
        "\u{1f609}" => "1f609.png",
        "\u{1f914}" => "1f914.png",
        // --- Status/Indicators (11) ---
        "\u{2705}" => "2705.png",
        "\u{26a0}" => "26a0.png",
        "\u{274c}" => "274c.png",
        "\u{2728}" => "2728.png",
        "\u{1f4af}" => "1f4af.png",
        "\u{23f3}" => "23f3.png",
        "\u{1f504}" => "1f504.png",
        "\u{2714}" => "2714.png",
        "\u{274e}" => "274e.png",
        "\u{1f6ab}" => "1f6ab.png",
        "\u{2611}" => "2611.png",
        // --- Objects/Tools (16) ---
        "\u{1f4a1}" => "1f4a1.png",
        "\u{1f4dd}" => "1f4dd.png",
        "\u{1f4e6}" => "1f4e6.png",
        "\u{1f527}" => "1f527.png",
        "\u{1f517}" => "1f517.png",
        "\u{1f4cb}" => "1f4cb.png",
        "\u{1f4c1}" => "1f4c1.png",
        "\u{1f4c2}" => "1f4c2.png",
        "\u{1f512}" => "1f512.png",
        "\u{1f513}" => "1f513.png",
        "\u{2699}" => "2699.png",
        "\u{1f50d}" => "1f50d.png",
        "\u{1f4cc}" => "1f4cc.png",
        "\u{1f528}" => "1f528.png",
        "\u{270f}" => "270f.png",
        "\u{1f9ea}" => "1f9ea.png",
        // --- Charts (3) ---
        "\u{1f4ca}" => "1f4ca.png",
        "\u{1f4c8}" => "1f4c8.png",
        "\u{1f4c9}" => "1f4c9.png",
        // --- Arrows (4) ---
        "\u{27a1}" => "27a1.png",
        "\u{2b06}" => "2b06.png",
        "\u{2b07}" => "2b07.png",
        "\u{2b05}" => "2b05.png",
        // --- Communication (3) ---
        "\u{1f4ac}" => "1f4ac.png",
        "\u{1f4e2}" => "1f4e2.png",
        "\u{1f4e7}" => "1f4e7.png",
        // --- Nature/Symbols (10) ---
        "\u{2b50}" => "2b50.png",
        "\u{1f525}" => "1f525.png",
        "\u{2764}" => "2764.png",
        "\u{1f48e}" => "1f48e.png",
        "\u{1f31f}" => "1f31f.png",
        "\u{1f3af}" => "1f3af.png",
        "\u{1f3a8}" => "1f3a8.png",
        "\u{1f6e1}" => "1f6e1.png",
        "\u{1f30a}" => "1f30a.png",
        "\u{26a1}" => "26a1.png",
        // --- People/Gestures (7) ---
        "\u{1f44d}" => "1f44d.png",
        "\u{1f44e}" => "1f44e.png",
        "\u{1f440}" => "1f440.png",
        "\u{1f44b}" => "1f44b.png",
        "\u{1f64f}" => "1f64f.png",
        "\u{1f4aa}" => "1f4aa.png",
        "\u{1f9e0}" => "1f9e0.png",
        // --- Dev/Tech (6) ---
        "\u{1f41b}" => "1f41b.png",
        "\u{1f6a7}" => "1f6a7.png",
        "\u{1f3d7}" => "1f3d7.png",
        "\u{1f4bb}" => "1f4bb.png",
        "\u{1f5a5}" => "1f5a5.png",
        "\u{2328}" => "2328.png",
        // --- Misc (16) ---
        "\u{1f389}" => "1f389.png",
        "\u{1f680}" => "1f680.png",
        "\u{2753}" => "2753.png",
        "\u{2757}" => "2757.png",
        "\u{1f3c6}" => "1f3c6.png",
        "\u{1f3aa}" => "1f3aa.png",
        "\u{1f4d6}" => "1f4d6.png",
        "\u{1f4da}" => "1f4da.png",
        "\u{1f514}" => "1f514.png",
        "\u{1f515}" => "1f515.png",
        "\u{1f5c2}" => "1f5c2.png",
        "\u{2139}" => "2139.png",
        "\u{1f534}" => "1f534.png",
        "\u{1f7e2}" => "1f7e2.png",
        "\u{1f7e1}" => "1f7e1.png",
        "\u{1f535}" => "1f535.png",
    }
}

/// Returns the static shortcode-to-emoji mapping.
///
/// The map is lazily initialized on first access and cached for the
/// lifetime of the process. For single lookups, `emoji_for_shortcode()`
/// is a convenient wrapper.
pub fn shortcode_map() -> &'static HashMap<&'static str, &'static str> {
    use std::sync::OnceLock;
    static MAP: OnceLock<HashMap<&'static str, &'static str>> = OnceLock::new();
    MAP.get_or_init(|| {
        HashMap::from([
            // --- Faces ---
            (":slightly_smiling_face:", "\u{1f642}"),
            (":grinning:", "\u{1f600}"),
            (":wink:", "\u{1f609}"),
            (":thinking:", "\u{1f914}"),
            // --- Status/Indicators ---
            (":white_check_mark:", "\u{2705}"),
            (":check_mark:", "\u{2705}"),
            (":warning:", "\u{26a0}"),
            (":x:", "\u{274c}"),
            (":sparkles:", "\u{2728}"),
            (":100:", "\u{1f4af}"),
            (":hourglass:", "\u{23f3}"),
            (":recycle:", "\u{1f504}"),
            (":heavy_check_mark:", "\u{2714}"),
            (":negative_squared_cross_mark:", "\u{274e}"),
            (":no_entry_sign:", "\u{1f6ab}"),
            (":ballot_box_with_check:", "\u{2611}"),
            // --- Objects/Tools ---
            (":bulb:", "\u{1f4a1}"),
            (":memo:", "\u{1f4dd}"),
            (":package:", "\u{1f4e6}"),
            (":wrench:", "\u{1f527}"),
            (":link:", "\u{1f517}"),
            (":clipboard:", "\u{1f4cb}"),
            (":file_folder:", "\u{1f4c1}"),
            (":open_file_folder:", "\u{1f4c2}"),
            (":lock:", "\u{1f512}"),
            (":unlock:", "\u{1f513}"),
            (":gear:", "\u{2699}"),
            (":mag:", "\u{1f50d}"),
            (":pushpin:", "\u{1f4cc}"),
            (":hammer:", "\u{1f528}"),
            (":pencil2:", "\u{270f}"),
            (":test_tube:", "\u{1f9ea}"),
            // --- Charts ---
            (":bar_chart:", "\u{1f4ca}"),
            (":chart_with_upwards_trend:", "\u{1f4c8}"),
            (":chart_with_downwards_trend:", "\u{1f4c9}"),
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
            (":star:", "\u{2b50}"),
            (":fire:", "\u{1f525}"),
            (":heart:", "\u{2764}"),
            (":gem:", "\u{1f48e}"),
            (":star2:", "\u{1f31f}"),
            (":dart:", "\u{1f3af}"),
            (":target:", "\u{1f3af}"),
            (":art:", "\u{1f3a8}"),
            (":shield:", "\u{1f6e1}"),
            (":ocean:", "\u{1f30a}"),
            (":zap:", "\u{26a1}"),
            // --- People/Gestures ---
            (":thumbsup:", "\u{1f44d}"),
            (":thumbsdown:", "\u{1f44e}"),
            (":eyes:", "\u{1f440}"),
            (":wave:", "\u{1f44b}"),
            (":pray:", "\u{1f64f}"),
            (":muscle:", "\u{1f4aa}"),
            (":brain:", "\u{1f9e0}"),
            // --- Dev/Tech ---
            (":bug:", "\u{1f41b}"),
            (":construction:", "\u{1f6a7}"),
            (":building_construction:", "\u{1f3d7}"),
            (":computer:", "\u{1f4bb}"),
            (":desktop_computer:", "\u{1f5a5}"),
            (":keyboard:", "\u{2328}"),
            // --- Misc ---
            (":tada:", "\u{1f389}"),
            (":rocket:", "\u{1f680}"),
            (":question:", "\u{2753}"),
            (":exclamation:", "\u{2757}"),
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
            // --- Aliases ---
            (":+1:", "\u{1f44d}"),
            (":-1:", "\u{1f44e}"),
        ])
    })
}

/// Looks up an emoji by its shortcode (e.g., ":rocket:").
///
/// Returns `Some(emoji_str)` if the shortcode is known, `None` otherwise.
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
        // Faces (4)
        "\u{1f642}",
        "\u{1f600}",
        "\u{1f609}",
        "\u{1f914}",
        // Status/Indicators (11)
        "\u{2705}",
        "\u{26a0}",
        "\u{274c}",
        "\u{2728}",
        "\u{1f4af}",
        "\u{23f3}",
        "\u{1f504}",
        "\u{2714}",
        "\u{274e}",
        "\u{1f6ab}",
        "\u{2611}",
        // Objects/Tools (16)
        "\u{1f4a1}",
        "\u{1f4dd}",
        "\u{1f4e6}",
        "\u{1f527}",
        "\u{1f517}",
        "\u{1f4cb}",
        "\u{1f4c1}",
        "\u{1f4c2}",
        "\u{1f512}",
        "\u{1f513}",
        "\u{2699}",
        "\u{1f50d}",
        "\u{1f4cc}",
        "\u{1f528}",
        "\u{270f}",
        "\u{1f9ea}",
        // Charts (3)
        "\u{1f4ca}",
        "\u{1f4c8}",
        "\u{1f4c9}",
        // Arrows (4)
        "\u{27a1}",
        "\u{2b06}",
        "\u{2b07}",
        "\u{2b05}",
        // Communication (3)
        "\u{1f4ac}",
        "\u{1f4e2}",
        "\u{1f4e7}",
        // Nature/Symbols (10)
        "\u{2b50}",
        "\u{1f525}",
        "\u{2764}",
        "\u{1f48e}",
        "\u{1f31f}",
        "\u{1f3af}",
        "\u{1f3a8}",
        "\u{1f6e1}",
        "\u{1f30a}",
        "\u{26a1}",
        // People/Gestures (7)
        "\u{1f44d}",
        "\u{1f44e}",
        "\u{1f440}",
        "\u{1f44b}",
        "\u{1f64f}",
        "\u{1f4aa}",
        "\u{1f9e0}",
        // Dev/Tech (6)
        "\u{1f41b}",
        "\u{1f6a7}",
        "\u{1f3d7}",
        "\u{1f4bb}",
        "\u{1f5a5}",
        "\u{2328}",
        // Misc (16)
        "\u{1f389}",
        "\u{1f680}",
        "\u{2753}",
        "\u{2757}",
        "\u{1f3c6}",
        "\u{1f3aa}",
        "\u{1f4d6}",
        "\u{1f4da}",
        "\u{1f514}",
        "\u{1f515}",
        "\u{1f5c2}",
        "\u{2139}",
        "\u{1f534}",
        "\u{1f7e2}",
        "\u{1f7e1}",
        "\u{1f535}",
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
    fn test_image_bytes_for_unknown_emoji() {
        assert!(image_bytes_for("\u{1f47d}").is_none());
    }

    #[test]
    fn test_shortcode_map_contains_expected_entries() {
        let map = shortcode_map();
        // Spot-check representative entries from each category
        assert_eq!(map.get(":tada:"), Some(&"\u{1f389}"));
        assert_eq!(map.get(":white_check_mark:"), Some(&"\u{2705}"));
        assert_eq!(map.get(":rocket:"), Some(&"\u{1f680}"));
        assert_eq!(map.get(":fire:"), Some(&"\u{1f525}"));
        assert_eq!(map.get(":wrench:"), Some(&"\u{1f527}"));
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
        assert_eq!(
            map.len(),
            85,
            "expected exactly 85 shortcodes (80 primary + 5 aliases), got {}",
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

        // Unknown shortcodes return None
        assert_eq!(emoji_for_shortcode(":unknown:"), None);
        assert_eq!(emoji_for_shortcode(""), None);
        assert_eq!(emoji_for_shortcode("no_colons"), None);
        assert_eq!(emoji_for_shortcode(":partial"), None);
    }
}
