//! Reserved keyword definitions for D2 language.
//!
//! Defines which keywords are processed in v1, which are recognized
//! but deferred, and provides lookup functions for the parser and
//! compiler.

/// Keywords that are semantically processed in v1.
pub const PROCESSED_KEYWORDS: &[&str] = &[
    "label",
    "shape",
    "style",
    "width",
    "height",
    "direction",
    "source-arrowhead",
    "target-arrowhead",
];

/// Keywords that are parsed without error but have no layout effect in v1.
pub const LAYOUT_DEFERRED_KEYWORDS: &[&str] = &["near"];

/// Keywords that are recognized (no parse error) but fully deferred to future versions.
pub const DEFERRED_KEYWORDS: &[&str] = &[
    "icon",
    "tooltip",
    "link",
    "class",
    "classes",
    "vars",
    "constraint",
    "grid-rows",
    "grid-columns",
    "grid-gap",
    "vertical-gap",
    "horizontal-gap",
    "top",
    "left",
];

/// Style sub-properties that are valid under `.style.*`.
pub const STYLE_KEYWORDS: &[&str] = &[
    "fill",
    "stroke",
    "stroke-width",
    "stroke-dash",
    "font-size",
    "font-color",
    "bold",
    "italic",
    "underline",
    "border-radius",
    "opacity",
    "shadow",
    "3d",
    "multiple",
    "animated",
    "double-border",
];

/// Check if a keyword is processed in v1.
pub fn is_processed(keyword: &str) -> bool {
    PROCESSED_KEYWORDS.contains(&keyword)
}

/// Check if a keyword is recognized but deferred.
pub fn is_deferred(keyword: &str) -> bool {
    LAYOUT_DEFERRED_KEYWORDS.contains(&keyword) || DEFERRED_KEYWORDS.contains(&keyword)
}

/// Check if a string is any known reserved keyword.
pub fn is_reserved(keyword: &str) -> bool {
    is_processed(keyword) || is_deferred(keyword)
}

/// Check if a string is a valid style sub-property.
pub fn is_style_keyword(keyword: &str) -> bool {
    STYLE_KEYWORDS.contains(&keyword)
}
