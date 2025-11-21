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
    pub ident: String,
    pub policy: ColumnPolicy,
    pub tooltip: Option<String>,
    pub policy_hash: u64,
}

impl ColumnSpec {
    pub fn new(ident: impl Into<String>, policy: ColumnPolicy, tooltip: Option<String>) -> Self {
        let ident = ident.into();
        let policy_hash = calculate_policy_hash(&ident, &policy);
        Self {
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
        self.policy_hash = calculate_policy_hash(&self.ident, &self.policy);
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
            self.policy_hash = calculate_policy_hash(&self.ident, &self.policy);
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

fn calculate_policy_hash(ident: &str, policy: &ColumnPolicy) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    ident.hash(&mut hasher);
    policy.hash(&mut hasher);
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
            ColumnSpec::new(label, policy, tooltip)
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
        if candidate.is_none() {
            candidate = specs
                .iter()
                .enumerate()
                .find(|(_, spec)| !matches!(spec.policy, ColumnPolicy::Fixed { .. }))
                .map(|(idx, _)| idx);
        }
        if let Some(idx) = candidate {
            if let Some(spec) = specs.get_mut(idx) {
                spec.set_policy(ColumnPolicy::Remainder { clip: false });
                remainder_assigned += 1;
            }
        }
    }

    // Promote additional wide columns to remainder up to the cap
    scored_indices.sort_by(|a, b| b.1.cmp(&a.1));
    for (idx, _) in scored_indices {
        if remainder_assigned >= MAX_REMAINDER_COLUMNS {
            break;
        }
        if let Some(spec) = specs.get_mut(idx) {
            if matches!(spec.policy, ColumnPolicy::Resizable { .. })
                && column_needs_remainder(ctx.column_stat(idx))
            {
                spec.set_policy(ColumnPolicy::Remainder { clip: false });
                remainder_assigned += 1;
            }
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
            has_emoji_like = text.chars().any(|c| matches!(c as u32, 0x1F300..=0x1FAFF));
        }
    }

    stat.rich_content = RichContentFlags {
        has_link,
        has_image,
        has_emoji_like,
    };
}

fn spans_to_text(spans: &[InlineSpan]) -> String {
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
        assert!(
            matches!(specs[0].policy, ColumnPolicy::Fixed { .. }),
            "policy = {:?}",
            specs[0].policy
        );
        assert!(
            matches!(specs[1].policy, ColumnPolicy::Remainder { .. }),
            "policy = {:?}",
            specs[1].policy
        );
    }

    #[test]
    fn classify_author_column() {
        let headers = vec![vec![span("Author")], vec![span("Examples")]];
        let rows: Vec<Vec<Vec<InlineSpan>>> = Vec::new();
        let stats = compute_column_stats(&headers, &rows, 32);
        let ctx = TableColumnContext::new(&headers, &rows, &stats, 14.0, 0);
        let specs = derive_column_specs(&ctx);
        assert!(
            matches!(specs[0].policy, ColumnPolicy::Resizable { .. }),
            "policy = {:?}",
            specs[0].policy
        );
        assert!(
            matches!(specs[1].policy, ColumnPolicy::Remainder { .. }),
            "policy = {:?}",
            specs[1].policy
        );
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
        assert!(
            matches!(specs[3].policy, ColumnPolicy::Remainder { .. }),
            "policy = {:?}",
            specs[3].policy
        );
        assert!(
            matches!(specs[2].policy, ColumnPolicy::Resizable { .. }),
            "policy = {:?}",
            specs[2].policy
        );
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
        assert!(
            remainder_count >= 2,
            "should allow multiple remainder columns, got {remainder_count}"
        );
    }

    #[test]
    fn cjk_widths_increase_stat_estimates() {
        let headers = vec![vec![span("列")], vec![span("Column")]];
        let rows = vec![vec![vec![span("长内容长内容长内容")], vec![span("short")]]];
        let stats = compute_column_stats(&headers, &rows, 32);
        assert!(stats[0].max_graphemes > stats[1].max_graphemes);
    }
}
