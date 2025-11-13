use std::hash::{Hash, Hasher};

use egui_extras::Column;

use crate::markdown_renderer::InlineSpan;

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
}

fn calculate_policy_hash(ident: &str, policy: &ColumnPolicy) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    ident.hash(&mut hasher);
    policy.hash(&mut hasher);
    hasher.finish()
}

pub fn derive_column_specs(headers: &[Vec<InlineSpan>]) -> Vec<ColumnSpec> {
    let mut remainder_assigned = false;
    headers
        .iter()
        .enumerate()
        .map(|(idx, spans)| {
            let label = header_text(spans);
            let policy = classify_column(&label, idx, &mut remainder_assigned);
            let tooltip = column_tooltip(&label, &policy);
            ColumnSpec::new(label, policy, tooltip)
        })
        .collect()
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

fn classify_column(label: &str, index: usize, remainder_assigned: &mut bool) -> ColumnPolicy {
    let lower = label.to_ascii_lowercase();
    if matches_any(&lower, &["version", "rev", "#", "id"]) {
        return ColumnPolicy::Fixed {
            width: 90.0,
            clip: true,
        };
    }
    if matches_any(&lower, &["date", "time", "timestamp"]) {
        return ColumnPolicy::Fixed {
            width: 130.0,
            clip: false,
        };
    }
    if matches_any(&lower, &["author", "owner", "assignee", "user"]) {
        return ColumnPolicy::Resizable {
            min: 110.0,
            preferred: 150.0,
            clip: false,
        };
    }
    if matches_any(&lower, &["status", "state"]) {
        return ColumnPolicy::Fixed {
            width: 120.0,
            clip: true,
        };
    }
    if matches_any(
        &lower,
        &["notes", "changes", "description", "details", "summary"],
    ) {
        *remainder_assigned = true;
        return ColumnPolicy::Remainder { clip: false };
    }
    if matches_any(&lower, &["example", "examples", "sample"]) {
        return ColumnPolicy::Resizable {
            min: 140.0,
            preferred: 200.0,
            clip: false,
        };
    }
    if index == 0 {
        return ColumnPolicy::Fixed {
            width: 120.0,
            clip: true,
        };
    }
    if !*remainder_assigned && (lower.len() > 12 || lower.contains(' ')) {
        *remainder_assigned = true;
        return ColumnPolicy::Remainder { clip: false };
    }
    ColumnPolicy::Resizable {
        min: 110.0,
        preferred: 160.0,
        clip: false,
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

#[cfg(test)]
mod tests {
    use super::*;

    fn span(text: &str) -> InlineSpan {
        InlineSpan::Text(text.to_string())
    }

    #[test]
    fn classify_version_column() {
        let headers = vec![vec![span("Version")], vec![span("Changes")]];
        let specs = derive_column_specs(&headers);
        assert!(matches!(specs[0].policy, ColumnPolicy::Fixed { .. }));
        assert!(matches!(specs[1].policy, ColumnPolicy::Remainder { .. }));
    }

    #[test]
    fn classify_author_column() {
        let headers = vec![vec![span("Author")]];
        let specs = derive_column_specs(&headers);
        assert!(matches!(specs[0].policy, ColumnPolicy::Resizable { .. }));
    }

    #[test]
    fn classify_examples_column() {
        let headers = vec![
            vec![span("Element")],
            vec![span("Symbol")],
            vec![span("Description")],
            vec![span("Examples")],
        ];
        let specs = derive_column_specs(&headers);
        assert!(matches!(specs[2].policy, ColumnPolicy::Remainder { .. }));
        assert!(matches!(specs[3].policy, ColumnPolicy::Resizable { .. }));
    }
}
