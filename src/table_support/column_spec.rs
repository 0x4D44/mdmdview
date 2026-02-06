use std::cmp::max;
use std::hash::{Hash, Hasher};

use egui_extras::Column;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::markdown_renderer::InlineSpan;

const MAX_REMAINDER_COLUMNS: usize = 2;

fn normalize_body_font_px(body: f32) -> f32 {
    if body.is_finite() && body > 4.0 {
        body
    } else {
        14.0
    }
}

fn px(body_px: f32, factor: f32) -> f32 {
    normalize_body_font_px(body_px) * factor
}

#[derive(Debug, Clone, PartialEq)]
pub enum ColumnPolicy {
    Auto,
    Fixed {
        width: f32,
        clip: bool,
    },
    Remainder {
        clip: bool,
    },
    Resizable {
        min: f32,
        preferred: f32,
        clip: bool,
    },
}

impl ColumnPolicy {
    pub fn to_column(&self) -> Column {
        match self {
            ColumnPolicy::Auto => Column::auto(),
            ColumnPolicy::Fixed { width, clip } => {
                let mut col = Column::exact(*width);
                if *clip {
                    col = col.clip(true);
                }
                col
            }
            ColumnPolicy::Remainder { clip } => {
                let mut col = Column::remainder();
                if *clip {
                    col = col.clip(true);
                }
                col
            }
            ColumnPolicy::Resizable {
                min,
                preferred,
                clip,
            } => {
                let mut col = Column::initial(*preferred).at_least(*min).resizable(true);
                if *clip {
                    col = col.clip(true);
                }
                col
            }
        }
    }
}

impl Hash for ColumnPolicy {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            ColumnPolicy::Auto => {}
            ColumnPolicy::Fixed { width, clip } => {
                width.to_bits().hash(state);
                clip.hash(state);
            }
            ColumnPolicy::Remainder { clip } => {
                clip.hash(state);
            }
            ColumnPolicy::Resizable {
                min,
                preferred,
                clip,
            } => {
                min.to_bits().hash(state);
                preferred.to_bits().hash(state);
                clip.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ColumnSpec {
    pub index: usize,
    pub ident: String,
    pub policy: ColumnPolicy,
    pub tooltip: Option<String>,
    pub policy_hash: u64,
}

impl ColumnSpec {
    pub fn new(
        index: usize,
        ident: impl Into<String>,
        policy: ColumnPolicy,
        tooltip: Option<String>,
    ) -> Self {
        let ident = ident.into();
        let policy_hash = calculate_policy_hash(index, &ident, &policy);
        Self {
            index,
            ident,
            policy,
            tooltip,
            policy_hash,
        }
    }

    pub fn as_column(&self) -> Column {
        self.policy.to_column()
    }

    pub fn set_policy(&mut self, policy: ColumnPolicy) {
        self.policy = policy;
        self.policy_hash = calculate_policy_hash(self.index, &self.ident, &self.policy);
    }

    pub fn apply_preferred_width(&mut self, width: f32) {
        if let ColumnPolicy::Resizable {
            min,
            ref mut preferred,
            ..
        } = self.policy
        {
            let clamped = width.max(min);
            *preferred = clamped;
            self.policy_hash = calculate_policy_hash(self.index, &self.ident, &self.policy);
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct RichContentFlags {
    pub has_link: bool,
    pub has_image: bool,
    pub has_emoji_like: bool,
}

#[derive(Debug, Clone, Default)]
pub struct ColumnStat {
    pub max_graphemes: usize,
    pub longest_word: usize,
    pub rich_content: RichContentFlags,
}

pub struct TableColumnContext<'a> {
    pub headers: &'a [Vec<InlineSpan>],
    pub rows: &'a [Vec<Vec<InlineSpan>>],
    pub stats: &'a [ColumnStat],
    pub body_font_px: f32,
    pub table_id: u64,
}

impl<'a> TableColumnContext<'a> {
    pub fn new(
        headers: &'a [Vec<InlineSpan>],
        rows: &'a [Vec<Vec<InlineSpan>>],
        stats: &'a [ColumnStat],
        body_font_px: f32,
        table_id: u64,
    ) -> Self {
        Self {
            headers,
            rows,
            stats,
            body_font_px,
            table_id,
        }
    }

    pub fn column_stat(&self, idx: usize) -> Option<&ColumnStat> {
        self.stats.get(idx)
    }
}

fn calculate_policy_hash(index: usize, ident: &str, policy: &ColumnPolicy) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    index.hash(&mut hasher);
    ident.hash(&mut hasher);
    std::mem::discriminant(policy).hash(&mut hasher);
    match policy {
        ColumnPolicy::Auto => {}
        ColumnPolicy::Fixed { width, clip } => {
            width.to_bits().hash(&mut hasher);
            clip.hash(&mut hasher);
        }
        ColumnPolicy::Remainder { clip } => {
            clip.hash(&mut hasher);
        }
        ColumnPolicy::Resizable { min, clip, .. } => {
            min.to_bits().hash(&mut hasher);
            clip.hash(&mut hasher);
        }
    }
    hasher.finish()
}

pub fn derive_column_specs(ctx: &TableColumnContext) -> Vec<ColumnSpec> {
    let mut remainder_assigned = 0usize;
    let mut fallback_idx: Option<usize> = None;
    let mut fallback_score: usize = 0;
    let column_count = ctx
        .stats
        .len()
        .max(ctx.headers.len())
        .max(ctx.rows.iter().map(|r| r.len()).max().unwrap_or(0));

    let mut scored_indices: Vec<(usize, usize)> = Vec::with_capacity(column_count);

    let mut specs: Vec<ColumnSpec> = (0..column_count)
        .map(|idx| {
            let spans = ctx.headers.get(idx).map(|v| v.as_slice()).unwrap_or(&[]);
            let label = header_text(spans);
            let stat = ctx.column_stat(idx);

            let candidate_score = stat
                .map(|s| {
                    let mut score = s.max_graphemes + s.longest_word * 2;
                    if s.rich_content.has_image {
                        score += 50;
                    }
                    if s.rich_content.has_link {
                        score += 25;
                    }
                    score
                })
                .unwrap_or_else(|| label.len());
            if candidate_score > fallback_score {
                fallback_score = candidate_score;
                fallback_idx = Some(idx);
            }
            scored_indices.push((idx, candidate_score));

            let policy =
                classify_column(&label, idx, &mut remainder_assigned, stat, ctx.body_font_px);
            let tooltip = column_tooltip(&label, &policy);
            ColumnSpec::new(idx, label, policy, tooltip)
        })
        .collect();

    // Ensure at least one remainder if none assigned and available
    if remainder_assigned == 0 {
        let mut candidate = fallback_idx;
        if let Some(idx) = candidate {
            if matches!(specs[idx].policy, ColumnPolicy::Fixed { .. }) {
                candidate = None;
            }
        }
        let has_non_fixed_other = specs
            .iter()
            .enumerate()
            .any(|(idx, spec)| idx != 0 && !matches!(spec.policy, ColumnPolicy::Fixed { .. }));
        if has_non_fixed_other && (candidate.is_none() || candidate == Some(0)) {
            candidate = scored_indices
                .iter()
                .filter(|(idx, _)| *idx != 0)
                .filter(|(idx, _)| !matches!(specs[*idx].policy, ColumnPolicy::Fixed { .. }))
                .max_by_key(|(_, score)| *score)
                .map(|(idx, _)| *idx);
        }
        if candidate.is_none() {
            for (idx, spec) in specs.iter().enumerate() {
                if !matches!(spec.policy, ColumnPolicy::Fixed { .. }) {
                    candidate = Some(idx);
                    break;
                }
            }
        }
        if let Some(idx) = candidate {
            let spec = &mut specs[idx];
            spec.set_policy(ColumnPolicy::Remainder { clip: false });
            remainder_assigned += 1;
        }
    }

    // Promote additional wide columns to remainder up to the cap
    scored_indices.sort_by(|a, b| b.1.cmp(&a.1));
    for (idx, _) in scored_indices {
        if remainder_assigned >= MAX_REMAINDER_COLUMNS {
            break;
        }
        let spec = &mut specs[idx];
        if matches!(spec.policy, ColumnPolicy::Resizable { .. })
            && column_needs_remainder(ctx.column_stat(idx))
        {
            spec.set_policy(ColumnPolicy::Remainder { clip: false });
            remainder_assigned += 1;
        }
    }

    specs
}

fn header_text(spans: &[InlineSpan]) -> String {
    let mut text = String::new();
    for span in spans {
        match span {
            InlineSpan::Text(t)
            | InlineSpan::Strong(t)
            | InlineSpan::Emphasis(t)
            | InlineSpan::Strikethrough(t)
            | InlineSpan::Code(t) => {
                if !text.is_empty() {
                    text.push(' ');
                }
                text.push_str(t.trim());
            }
            InlineSpan::Link { text: t, .. } => {
                if !text.is_empty() {
                    text.push(' ');
                }
                text.push_str(t.trim());
            }
            InlineSpan::Image { alt, .. } => {
                if !text.is_empty() {
                    text.push(' ');
                }
                text.push_str(alt.trim());
            }
        }
    }
    if text.trim().is_empty() {
        "Column".to_string()
    } else {
        text.trim().to_string()
    }
}

fn classify_column(
    label: &str,
    index: usize,
    remainder_assigned: &mut usize,
    stat: Option<&ColumnStat>,
    body_font_px: f32,
) -> ColumnPolicy {
    let lower = label.to_ascii_lowercase();
    if matches_any(&lower, &["version", "rev", "#", "id"]) {
        return ColumnPolicy::Fixed {
            width: px(body_font_px, 5.8).clamp(80.0, 160.0),
            clip: true,
        };
    }
    if matches_any(&lower, &["date", "time", "timestamp"]) {
        return ColumnPolicy::Fixed {
            width: px(body_font_px, 8.5).clamp(110.0, 200.0),
            clip: false,
        };
    }
    if matches_any(&lower, &["author", "owner", "assignee", "user"]) {
        return ColumnPolicy::Resizable {
            min: px(body_font_px, 7.5),
            preferred: px(body_font_px, 10.0),
            clip: false,
        };
    }
    if matches_any(&lower, &["status", "state"]) {
        return ColumnPolicy::Fixed {
            width: px(body_font_px, 7.0).clamp(90.0, 180.0),
            clip: true,
        };
    }
    if matches_any(
        &lower,
        &["notes", "changes", "description", "details", "summary"],
    ) {
        return ColumnPolicy::Resizable {
            min: px(body_font_px, 10.5),
            preferred: px(body_font_px, 14.5),
            clip: false,
        };
    }
    if matches_any(
        &lower,
        &["example", "examples", "sample", "use case", "use cases"],
    ) {
        *remainder_assigned += 1;
        return ColumnPolicy::Remainder { clip: false };
    }
    if index == 0 {
        // First column typically identifiers; keep fixed only for short labels.
        if stat
            .map(|s| s.max_graphemes <= 18 && s.longest_word <= 12)
            .unwrap_or_else(|| label.len() <= 12)
        {
            return ColumnPolicy::Fixed {
                width: px(body_font_px, 7.0).clamp(80.0, 180.0),
                clip: true,
            };
        }
    }
    if *remainder_assigned < MAX_REMAINDER_COLUMNS && column_needs_remainder(stat) {
        *remainder_assigned += 1;
        return ColumnPolicy::Remainder { clip: false };
    }
    ColumnPolicy::Resizable {
        min: px(body_font_px, 7.0),
        preferred: px(body_font_px, 9.5),
        clip: false,
    }
}

fn column_needs_remainder(stat: Option<&ColumnStat>) -> bool {
    if let Some(stat) = stat {
        stat.rich_content.has_image
            || stat.longest_word > 18
            || stat.max_graphemes > 60
            || stat.rich_content.has_link
    } else {
        false
    }
}

fn matches_any(label: &str, needles: &[&str]) -> bool {
    needles.iter().any(|needle| label.contains(needle))
}

fn column_tooltip(label: &str, policy: &ColumnPolicy) -> Option<String> {
    let description = match policy {
        ColumnPolicy::Auto => "Auto-sized column".to_string(),
        ColumnPolicy::Fixed { width, clip } => format!(
            "Fixed width {:.0}px{}",
            width,
            if *clip { " (clipped)" } else { "" }
        ),
        ColumnPolicy::Remainder { clip } => format!(
            "Shared remainder width{}",
            if *clip { " (clipped)" } else { "" }
        ),
        ColumnPolicy::Resizable {
            min,
            preferred,
            clip,
        } => format!(
            "Resizable (min {:.0}px, start {:.0}px{})",
            min,
            preferred,
            if *clip { ", clipped" } else { "" }
        ),
    };
    Some(format!("{label}: {description}"))
}

pub fn compute_column_stats(
    headers: &[Vec<InlineSpan>],
    rows: &[Vec<Vec<InlineSpan>>],
    max_samples: usize,
) -> Vec<ColumnStat> {
    let column_count = max(
        headers.len(),
        rows.iter().map(|row| row.len()).max().unwrap_or(0),
    );
    if column_count == 0 {
        return Vec::new();
    }

    let mut stats = vec![ColumnStat::default(); column_count];

    for (idx, header) in headers.iter().enumerate() {
        accumulate_stats_for_cell(header, &mut stats[idx]);
    }

    let mut sampled_rows = 0;
    for row in rows.iter() {
        for (idx, cell) in row.iter().enumerate().take(column_count) {
            accumulate_stats_for_cell(cell, &mut stats[idx]);
        }
        sampled_rows += 1;
        if sampled_rows >= max_samples {
            break;
        }
    }

    stats
}

/// Check if a character is emoji-like (for column width estimation).
/// Covers common emoji Unicode blocks including:
/// - Miscellaneous Symbols (2600-26FF)
/// - Dingbats (2700-27BF)
/// - Emoticons (1F600-1F64F)
/// - Transport and Map (1F680-1F6FF)
/// - Miscellaneous Symbols and Pictographs (1F300-1F5FF)
/// - Supplemental Symbols and Pictographs (1F900-1F9FF)
/// - Symbols and Pictographs Extended-A (1FA00-1FAFF)
fn is_emoji_like(c: char) -> bool {
    let cp = c as u32;
    matches!(
        cp,
        0x2600..=0x26FF     // Miscellaneous Symbols
        | 0x2700..=0x27BF   // Dingbats
        | 0x1F300..=0x1F5FF // Miscellaneous Symbols and Pictographs
        | 0x1F600..=0x1F64F // Emoticons (smileys)
        | 0x1F680..=0x1F6FF // Transport and Map Symbols
        | 0x1F900..=0x1F9FF // Supplemental Symbols and Pictographs
        | 0x1FA00..=0x1FAFF // Symbols and Pictographs Extended-A
    )
}

fn accumulate_stats_for_cell(spans: &[InlineSpan], stat: &mut ColumnStat) {
    if stat.rich_content.has_image {
        // rich-content flags persist; skip repeated scans for pure images.
    }
    let mut has_link = stat.rich_content.has_link;
    let mut has_image = stat.rich_content.has_image;
    let mut has_emoji_like = stat.rich_content.has_emoji_like;

    for span in spans {
        match span {
            InlineSpan::Link { .. } => has_link = true,
            InlineSpan::Image { .. } => has_image = true,
            _ => {}
        }
    }

    let text = spans_to_text(spans);
    if !text.is_empty() {
        let graphemes = text.graphemes(true).count();
        let display_width = UnicodeWidthStr::width(text.as_str());
        stat.max_graphemes = stat.max_graphemes.max(display_width.max(graphemes));

        for word in text.split_whitespace() {
            let word_len = UnicodeWidthStr::width(word).max(word.graphemes(true).count());
            stat.longest_word = stat.longest_word.max(word_len);
        }

        if !has_emoji_like {
            has_emoji_like = text.chars().any(is_emoji_like);
        }
    }

    stat.rich_content = RichContentFlags {
        has_link,
        has_image,
        has_emoji_like,
    };
}

pub(crate) fn spans_to_text(spans: &[InlineSpan]) -> String {
    let mut text = String::new();
    for span in spans {
        match span {
            InlineSpan::Text(t)
            | InlineSpan::Code(t)
            | InlineSpan::Strong(t)
            | InlineSpan::Emphasis(t)
            | InlineSpan::Strikethrough(t) => {
                if !text.is_empty() {
                    text.push(' ');
                }
                text.push_str(t);
            }
            InlineSpan::Link { text: t, .. } => {
                if !text.is_empty() {
                    text.push(' ');
                }
                text.push_str(t);
            }
            InlineSpan::Image { alt, .. } => {
                if !text.is_empty() {
                    text.push(' ');
                }
                text.push_str(alt);
            }
        }
    }
    text.trim().to_string()
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;

    fn span(text: &str) -> InlineSpan {
        InlineSpan::Text(text.to_string())
    }

    #[test]
    fn classify_version_column() {
        let headers = vec![vec![span("Version")], vec![span("Changes")]];
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(matches!(specs[0].policy, ColumnPolicy::Fixed { .. }));
        assert!(matches!(specs[1].policy, ColumnPolicy::Remainder { .. }));
    }

    #[test]
    fn classify_author_column() {
        let headers = vec![vec![span("Author")], vec![span("Examples")]];
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(matches!(specs[0].policy, ColumnPolicy::Resizable { .. }));
        assert!(matches!(specs[1].policy, ColumnPolicy::Remainder { .. }));
    }

    #[test]
    fn classify_examples_column() {
        let headers = vec![
            vec![span("Element")],
            vec![span("Symbol")],
            vec![span("Description")],
            vec![span("Examples")],
        ];
        assert_eq!(header_text(&headers[3]), "Examples");
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(matches!(specs[3].policy, ColumnPolicy::Remainder { .. }));
        assert!(matches!(specs[2].policy, ColumnPolicy::Resizable { .. }));
    }

    #[test]
    fn compute_stats_counts_text() {
        let headers = vec![vec![span("Head")]];
        let rows = vec![vec![vec![span("Emoji 😀 test")]]];
        let stats = compute_column_stats(&headers, &rows, 32);
        assert_eq!(stats.len(), 1);
        assert!(stats[0].max_graphemes >= 4);
        assert!(stats[0].longest_word >= 5);
        assert!(stats[0].rich_content.has_emoji_like);
    }

    #[test]
    fn fallback_assigns_remainder_based_on_stats() {
        let headers = vec![vec![span("Foo")], vec![span("Bar")]];
        let rows = vec![vec![
            vec![span("Short")],
            vec![span(
                "This column contains a very long sentence that should force remainder selection.",
            )],
        ]];
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(matches!(specs[1].policy, ColumnPolicy::Remainder { .. }));
    }

    #[test]
    fn fallback_avoids_first_column_when_possible() {
        let headers = vec![
            vec![span("Center")],
            vec![span("Left")],
            vec![span("Right")],
        ];
        let rows = vec![vec![
            vec![span("Centered label with enough words to raise score")],
            vec![span("L1")],
            vec![span("R1")],
        ]];
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(!matches!(specs[0].policy, ColumnPolicy::Remainder { .. }));
        assert!(specs[1..]
            .iter()
            .any(|spec| matches!(spec.policy, ColumnPolicy::Remainder { .. })));
    }

    #[test]
    fn derive_column_specs_handles_missing_stats_with_fixed_columns() {
        let headers = vec![vec![span("Alpha")], vec![span("Status")]];
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats: Vec<ColumnStat> = Vec::new();
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert_eq!(specs.len(), 2);
        assert!(matches!(specs[0].policy, ColumnPolicy::Fixed { .. }));
        assert!(matches!(specs[1].policy, ColumnPolicy::Fixed { .. }));
    }

    #[test]
    fn derive_column_specs_empty_header_label_uses_fixed_policy() {
        let headers = vec![Vec::new()];
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats: Vec<ColumnStat> = Vec::new();
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert_eq!(specs.len(), 1);
        assert!(matches!(specs[0].policy, ColumnPolicy::Fixed { .. }));
    }

    #[test]
    fn derive_column_specs_picks_first_non_fixed_when_best_is_fixed() {
        let headers = vec![vec![span("Notes")], vec![span("Status")]];
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats: Vec<ColumnStat> = Vec::new();
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(matches!(specs[0].policy, ColumnPolicy::Remainder { .. }));
    }

    #[test]
    fn fallback_skips_fixed_candidate_for_remainder() {
        let headers = vec![
            vec![span("Version")],
            vec![span("Notes")],
            vec![span("Owner")],
        ];
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats = vec![
            ColumnStat {
                max_graphemes: 50,
                longest_word: 10,
                rich_content: RichContentFlags {
                    has_link: false,
                    has_image: false,
                    has_emoji_like: false,
                },
            },
            ColumnStat {
                max_graphemes: 5,
                longest_word: 5,
                rich_content: RichContentFlags {
                    has_link: false,
                    has_image: false,
                    has_emoji_like: false,
                },
            },
            ColumnStat {
                max_graphemes: 4,
                longest_word: 4,
                rich_content: RichContentFlags {
                    has_link: false,
                    has_image: false,
                    has_emoji_like: false,
                },
            },
        ];
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(matches!(specs[0].policy, ColumnPolicy::Fixed { .. }));
        assert!(specs[1..]
            .iter()
            .any(|spec| matches!(spec.policy, ColumnPolicy::Remainder { .. })));
    }

    #[test]
    fn examples_header_prefers_remainder() {
        let headers = vec![vec![span("Examples")], vec![span("Description")]];
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 16.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(matches!(specs[0].policy, ColumnPolicy::Remainder { .. }));
        assert!(matches!(specs[1].policy, ColumnPolicy::Resizable { .. }));
    }

    #[test]
    fn multiple_remainder_columns_allowed() {
        let headers = vec![
            vec![span("Summary")],
            vec![span("Details")],
            vec![span("Examples")],
        ];
        let rows = vec![vec![
            vec![span("short")],
            vec![span(
                "Long content that should trigger a remainder column due to its width and words.",
            )],
            vec![span(
                "Another large column with links https://example.com and more text.",
            )],
        ]];
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        let remainder_count = specs
            .iter()
            .filter(|s| matches!(s.policy, ColumnPolicy::Remainder { .. }))
            .count();
        assert!(remainder_count >= 2);
    }

    #[test]
    fn cjk_widths_increase_stat_estimates() {
        let headers = vec![vec![span("列")], vec![span("Column")]];
        let rows = vec![vec![vec![span("长内容长内容长内容")], vec![span("short")]]];
        let stats = compute_column_stats(&headers, &rows, 32);
        assert!(stats[0].max_graphemes > stats[1].max_graphemes);
    }

    #[test]
    fn test_header_text_and_spans_to_text_variants() {
        let spans = vec![
            InlineSpan::Text(" Alpha ".to_string()),
            InlineSpan::Strong("Beta".to_string()),
            InlineSpan::Emphasis("Gamma".to_string()),
            InlineSpan::Strikethrough("Delta".to_string()),
            InlineSpan::Code("Epsilon".to_string()),
            InlineSpan::Link {
                text: "Zeta".to_string(),
                url: "https://example.invalid".to_string(),
            },
            InlineSpan::Image {
                src: "img.png".to_string(),
                alt: "Eta".to_string(),
                title: None,
            },
        ];

        let header = header_text(&spans);
        assert!(header.contains("Alpha"));
        assert!(header.contains("Beta"));
        assert!(header.contains("Zeta"));
        assert!(header.contains("Eta"));

        let text = spans_to_text(&spans);
        assert!(text.contains("Alpha"));
        assert!(text.contains("Zeta"));
        assert!(text.contains("Eta"));

        assert_eq!(header_text(&[]), "Column");
        assert_eq!(
            header_text(&[InlineSpan::Text("   ".to_string())]),
            "Column"
        );
    }

    #[test]
    fn test_column_policy_to_column_and_tooltip() {
        let policies = vec![
            ColumnPolicy::Auto,
            ColumnPolicy::Fixed {
                width: 120.0,
                clip: true,
            },
            ColumnPolicy::Remainder { clip: false },
            ColumnPolicy::Resizable {
                min: 50.0,
                preferred: 140.0,
                clip: true,
            },
        ];

        for policy in policies {
            let _col = policy.to_column();
            let tooltip = column_tooltip("Header", &policy).expect("tooltip");
            assert!(tooltip.contains("Header"));
        }
    }

    #[test]
    fn test_apply_preferred_width_clamps_and_hash_stable_for_resizable() {
        let mut spec = ColumnSpec::new(
            0,
            "body",
            ColumnPolicy::Resizable {
                min: 50.0,
                preferred: 120.0,
                clip: false,
            },
            None,
        );
        let original_hash = spec.policy_hash;
        spec.apply_preferred_width(10.0);
        assert!(matches!(spec.policy, ColumnPolicy::Resizable { .. }));
        assert_eq!(spec.policy_hash, original_hash);

        let mut fixed = ColumnSpec::new(
            1,
            "fixed",
            ColumnPolicy::Fixed {
                width: 40.0,
                clip: false,
            },
            None,
        );
        let fixed_hash = fixed.policy_hash;
        fixed.apply_preferred_width(100.0);
        assert_eq!(fixed.policy_hash, fixed_hash);
    }

    #[test]
    fn test_column_spec_hash_includes_index() {
        let policy = ColumnPolicy::Resizable {
            min: 40.0,
            preferred: 120.0,
            clip: false,
        };
        let first = ColumnSpec::new(0, "Header", policy.clone(), None);
        let second = ColumnSpec::new(1, "Header", policy, None);
        assert_ne!(first.policy_hash, second.policy_hash);
    }

    #[test]
    fn test_compute_column_stats_empty() {
        let stats = compute_column_stats(&[], &[], 10);
        assert!(stats.is_empty());
    }

    #[test]
    fn test_classify_date_status_notes_columns() {
        let mut remainder = 0usize;
        let date_policy = classify_column("Date", 0, &mut remainder, None, 14.0);
        let status_policy = classify_column("Status", 1, &mut remainder, None, 14.0);
        let notes_policy = classify_column("Notes", 2, &mut remainder, None, 14.0);

        assert!(matches!(date_policy, ColumnPolicy::Fixed { .. }));
        assert!(matches!(status_policy, ColumnPolicy::Fixed { .. }));
        assert!(matches!(notes_policy, ColumnPolicy::Resizable { .. }));
    }

    #[test]
    fn test_derive_column_specs_remainder_for_rich_content() {
        let headers = vec![vec![span("Body")]];
        let rows = vec![vec![vec![InlineSpan::Link {
            text: "verylonglinkword".to_string(),
            url: "https://example.invalid".to_string(),
        }]]];
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);

        assert!(matches!(specs[0].policy, ColumnPolicy::Remainder { .. }));
    }

    #[test]
    fn test_normalize_body_font_px_defaults_for_invalid_values() {
        assert_eq!(normalize_body_font_px(f32::NAN), 14.0);
        assert_eq!(normalize_body_font_px(-10.0), 14.0);
        assert_eq!(normalize_body_font_px(0.0), 14.0);
        assert_eq!(normalize_body_font_px(12.0), 12.0);
        assert_eq!(px(10.0, 2.0), 20.0);
    }

    #[test]
    fn test_column_policy_tooltip_variants_for_clip_flags() {
        let fixed = ColumnPolicy::Fixed {
            width: 120.0,
            clip: false,
        };
        let remainder = ColumnPolicy::Remainder { clip: true };

        let fixed_tooltip = column_tooltip("Fixed", &fixed).expect("tooltip");
        assert!(!fixed_tooltip.contains("clipped"));

        let remainder_tooltip = column_tooltip("Remainder", &remainder).expect("tooltip");
        assert!(remainder_tooltip.contains("clipped"));
    }

    #[test]
    fn test_set_policy_updates_hash() {
        let mut spec = ColumnSpec::new(0, "col", ColumnPolicy::Auto, None);
        let original = spec.policy_hash;
        spec.set_policy(ColumnPolicy::Fixed {
            width: 100.0,
            clip: true,
        });
        assert_ne!(spec.policy_hash, original);
    }

    #[test]
    fn test_apply_preferred_width_updates_preferred_value() {
        let mut spec = ColumnSpec::new(
            0,
            "body",
            ColumnPolicy::Resizable {
                min: 50.0,
                preferred: 120.0,
                clip: false,
            },
            None,
        );
        spec.apply_preferred_width(180.0);
        assert!(matches!(spec.policy, ColumnPolicy::Resizable { .. }));
    }

    #[test]
    fn test_column_policy_hash_variants() {
        let policies = [
            ColumnPolicy::Auto,
            ColumnPolicy::Fixed {
                width: 120.0,
                clip: true,
            },
            ColumnPolicy::Remainder { clip: true },
            ColumnPolicy::Resizable {
                min: 40.0,
                preferred: 120.0,
                clip: true,
            },
        ];
        for policy in policies {
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            policy.hash(&mut hasher);
            let _ = hasher.finish();
        }
    }

    #[test]
    fn test_column_policy_to_column_clipped_variants() {
        let fixed = ColumnPolicy::Fixed {
            width: 120.0,
            clip: true,
        };
        let fixed_unclipped = ColumnPolicy::Fixed {
            width: 120.0,
            clip: false,
        };
        let remainder = ColumnPolicy::Remainder { clip: true };
        let resizable = ColumnPolicy::Resizable {
            min: 40.0,
            preferred: 120.0,
            clip: true,
        };
        let _ = fixed.to_column();
        let _ = fixed_unclipped.to_column();
        let _ = remainder.to_column();
        let _ = resizable.to_column();
    }

    #[test]
    fn test_column_needs_remainder_none_is_false() {
        assert!(!column_needs_remainder(None));
    }

    #[test]
    fn test_accumulate_stats_for_cell_tracks_images() {
        let spans = vec![InlineSpan::Image {
            src: "img.png".to_string(),
            alt: "Alt".to_string(),
            title: None,
        }];
        let mut stat = ColumnStat::default();
        accumulate_stats_for_cell(&spans, &mut stat);
        assert!(stat.rich_content.has_image);
    }

    #[test]
    fn test_accumulate_stats_detects_emoji_like_text() {
        let emoji = char::from_u32(0x1F600).expect("emoji");
        let spans = vec![InlineSpan::Text(format!("Hello {emoji}"))];
        let mut stat = ColumnStat::default();
        accumulate_stats_for_cell(&spans, &mut stat);
        assert!(stat.rich_content.has_emoji_like);
    }

    #[test]
    fn test_remainder_cap_forces_resizable() {
        let mut remainder = MAX_REMAINDER_COLUMNS;
        let stat = ColumnStat {
            max_graphemes: 80,
            longest_word: 40,
            rich_content: RichContentFlags {
                has_link: true,
                has_image: false,
                has_emoji_like: false,
            },
        };
        let policy = classify_column("Notes", 2, &mut remainder, Some(&stat), 14.0);
        assert!(matches!(policy, ColumnPolicy::Resizable { .. }));
    }

    #[test]
    fn test_compute_column_stats_respects_max_samples() {
        let headers = vec![vec![span("Header")]];
        let rows = vec![
            vec![vec![span("short")]],
            vec![vec![span("this_is_a_much_longer_token")]],
        ];
        let stats = compute_column_stats(&headers, &rows, 1);
        assert_eq!(stats.len(), 1);
        assert!(stats[0].longest_word < 20, "should ignore second row");
    }

    #[test]
    fn test_accumulate_stats_preserves_existing_flags() {
        let mut stat = ColumnStat {
            max_graphemes: 0,
            longest_word: 0,
            rich_content: RichContentFlags {
                has_link: false,
                has_image: true,
                has_emoji_like: false,
            },
        };
        accumulate_stats_for_cell(&[InlineSpan::Text("plain".to_string())], &mut stat);
        assert!(stat.rich_content.has_image);
        assert!(!stat.rich_content.has_link);
    }

    #[test]
    fn test_derive_column_specs_empty_context_returns_empty() {
        let headers: Vec<Vec<InlineSpan>> = Vec::new();
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats: Vec<ColumnStat> = Vec::new();
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(specs.is_empty());
    }

    #[test]
    fn test_header_text_link_and_image_first() {
        let spans = vec![InlineSpan::Link {
            text: "Docs".to_string(),
            url: "https://example.com".to_string(),
        }];
        assert_eq!(header_text(&spans), "Docs");
        let spans = vec![InlineSpan::Image {
            src: "img.png".to_string(),
            alt: "Diagram".to_string(),
            title: None,
        }];
        assert_eq!(header_text(&spans), "Diagram");
    }

    #[test]
    fn test_column_needs_remainder_true_for_long_words() {
        let stat = ColumnStat {
            max_graphemes: 10,
            longest_word: 24,
            rich_content: RichContentFlags {
                has_link: false,
                has_image: false,
                has_emoji_like: false,
            },
        };
        assert!(column_needs_remainder(Some(&stat)));
    }

    #[test]
    fn test_column_needs_remainder_true_for_images() {
        let stat = ColumnStat {
            max_graphemes: 5,
            longest_word: 5,
            rich_content: RichContentFlags {
                has_link: false,
                has_image: true,
                has_emoji_like: false,
            },
        };
        assert!(column_needs_remainder(Some(&stat)));
    }

    #[test]
    fn test_classify_column_skips_remainder_when_limit_reached() {
        let mut remainder_assigned = MAX_REMAINDER_COLUMNS;
        let stat = ColumnStat {
            max_graphemes: 2,
            longest_word: 2,
            rich_content: RichContentFlags {
                has_link: false,
                has_image: true,
                has_emoji_like: false,
            },
        };
        let policy = classify_column("misc", 1, &mut remainder_assigned, Some(&stat), 14.0);
        assert!(matches!(policy, ColumnPolicy::Resizable { .. }));
    }

    #[test]
    fn test_accumulate_stats_empty_text_and_existing_emoji_flag() {
        let mut stat = ColumnStat {
            max_graphemes: 0,
            longest_word: 0,
            rich_content: RichContentFlags {
                has_link: false,
                has_image: false,
                has_emoji_like: true,
            },
        };
        accumulate_stats_for_cell(&[InlineSpan::Text("Hello".to_string())], &mut stat);
        assert!(stat.rich_content.has_emoji_like);

        let mut empty_stat = ColumnStat::default();
        accumulate_stats_for_cell(&[], &mut empty_stat);
        assert_eq!(empty_stat.max_graphemes, 0);
    }

    #[test]
    fn test_emoji_detection_covers_all_blocks() {
        // Basic emoticons (0x1F600-0x1F64F)
        assert!(is_emoji_like('\u{1F600}')); // grinning face
        assert!(is_emoji_like('\u{1F64F}')); // person with folded hands

        // Miscellaneous Symbols (0x2600-0x26FF)
        assert!(is_emoji_like('\u{2600}')); // sun
        assert!(is_emoji_like('\u{26FF}')); // end of block

        // Dingbats (0x2700-0x27BF)
        assert!(is_emoji_like('\u{2700}')); // black scissors
        assert!(is_emoji_like('\u{27BF}')); // end of block

        // Transport and Map (0x1F680-0x1F6FF)
        assert!(is_emoji_like('\u{1F680}')); // rocket
        assert!(is_emoji_like('\u{1F6FF}')); // end of block

        // Supplemental Symbols (0x1F900-0x1F9FF)
        assert!(is_emoji_like('\u{1F900}')); // start of block
        assert!(is_emoji_like('\u{1F9FF}')); // end of block

        // Extended-A (0x1FA00-0x1FAFF)
        assert!(is_emoji_like('\u{1FA00}')); // start of block
        assert!(is_emoji_like('\u{1FAFF}')); // end of block

        // Non-emoji should return false
        assert!(!is_emoji_like('A'));
        assert!(!is_emoji_like('z'));
        assert!(!is_emoji_like('0'));
        assert!(!is_emoji_like(' '));
    }

    #[test]
    fn test_compute_column_stats_jagged_rows() {
        // Headers have 3 columns, but some rows have fewer
        let headers = vec![vec![span("A")], vec![span("B")], vec![span("C")]];
        let rows = vec![
            vec![vec![span("a1")], vec![span("b1")], vec![span("c1")]], // full row
            vec![vec![span("a2")], vec![span("b2")]],                   // missing 3rd column
            vec![vec![span("a3")]],                                     // only 1 column
        ];
        let stats = compute_column_stats(&headers, &rows, 32);
        assert_eq!(stats.len(), 3);
        // All columns should have stats, even if some rows are missing columns
        assert!(stats[0].max_graphemes > 0);
        assert!(stats[1].max_graphemes > 0);
        assert!(stats[2].max_graphemes > 0);
    }

    #[test]
    fn test_derive_column_specs_jagged_rows() {
        // Headers have 3 columns, but rows have varying column counts
        let headers = vec![vec![span("Name")], vec![span("Value")], vec![span("Notes")]];
        let rows = vec![
            vec![vec![span("item1")], vec![span("val1")]], // 2 columns
            vec![vec![span("item2")], vec![span("val2")], vec![span("note2")]], // 3 columns
            vec![vec![span("item3")]],                     // 1 column
        ];
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        // Should produce specs for all 3 columns without panicking
        assert_eq!(specs.len(), 3);
    }
}
