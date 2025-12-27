use std::collections::HashMap;

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct RowMetrics {
    pub max_height: f32,
    pub dirty: bool,
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct TableMetricEntry {
    pub rows: Vec<RowMetrics>,
    pub rendered_rows: usize,
    pub total_rows: usize,
    pub header_height: f32,
    pub resolved_widths: Vec<f32>,
    pub last_width_frame: u64,
    pub last_discard_frame: Option<u64>,
    pub persisted_column_widths: HashMap<u64, f32>,
    pub pending_user_resize: Option<PendingResize>,
    /// Font size when widths were last persisted. Used to invalidate
    /// persisted widths when zoom level changes.
    pub persisted_font_size: Option<f32>,
}

#[derive(Debug, Clone, Copy)]
pub struct PendingResize {
    pub column_hash: u64,
    pub width: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WidthChange {
    None,
    Small,
    Large,
}

const WIDTH_EPSILON: f32 = 0.15;
const WIDTH_LARGE_DELTA: f32 = 0.75;

impl TableMetricEntry {
    pub fn ensure_row(&mut self, index: usize) -> &mut RowMetrics {
        if index >= self.rows.len() {
            self.rows.resize_with(index + 1, RowMetrics::default);
        }
        &mut self.rows[index]
    }

    pub fn row(&self, index: usize) -> Option<&RowMetrics> {
        self.rows.get(index)
    }

    pub fn begin_pass(&mut self, total_rows: usize) {
        self.total_rows = total_rows;
        self.rendered_rows = 0;
    }

    pub fn note_row_rendered(&mut self) {
        self.rendered_rows += 1;
    }

    pub fn persisted_width(&self, key: u64) -> Option<f32> {
        self.persisted_column_widths.get(&key).copied()
    }

    pub fn set_persisted_width(&mut self, key: u64, width: f32) {
        self.persisted_column_widths.insert(key, width);
    }

    pub fn remove_persisted_width(&mut self, key: u64) {
        self.persisted_column_widths.remove(&key);
    }

    pub fn update_widths(&mut self, widths: &[f32], frame_id: u64) -> WidthChange {
        if widths.is_empty() {
            self.resolved_widths.clear();
            self.last_width_frame = frame_id;
            return WidthChange::None;
        }

        let first_update = self.resolved_widths.is_empty();
        let mut change = WidthChange::None;
        if !first_update && self.resolved_widths.len() != widths.len() {
            change = WidthChange::Large;
        } else {
            for (old, new) in self.resolved_widths.iter().zip(widths.iter()) {
                let delta = (old - new).abs();
                if delta > WIDTH_LARGE_DELTA {
                    change = WidthChange::Large;
                    break;
                }
                if delta > WIDTH_EPSILON {
                    change = WidthChange::Small;
                }
            }
        }

        self.resolved_widths.clear();
        self.resolved_widths.extend_from_slice(widths);
        self.last_width_frame = frame_id;

        change
    }

    pub fn header_height(&self) -> Option<f32> {
        if self.header_height > 0.0 {
            Some(self.header_height)
        } else {
            None
        }
    }

    pub fn update_header_height(&mut self, height: f32) -> bool {
        let clamped = height.max(0.0);
        if (self.header_height - clamped).abs() > 0.5 {
            self.header_height = clamped;
            true
        } else {
            false
        }
    }

    pub fn current_widths(&self) -> &[f32] {
        &self.resolved_widths
    }

    /// Check if font size has changed since widths were last persisted.
    /// If so, clear all persisted widths and update the stored font size.
    /// Returns true if widths were cleared.
    pub fn check_font_size_change(&mut self, current_font_size: f32) -> bool {
        const FONT_SIZE_EPSILON: f32 = 0.5;
        if let Some(stored_font) = self.persisted_font_size {
            if (stored_font - current_font_size).abs() > FONT_SIZE_EPSILON {
                self.persisted_column_widths.clear();
                self.persisted_font_size = Some(current_font_size);
                return true;
            }
        } else {
            self.persisted_font_size = Some(current_font_size);
        }
        false
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct TableMetrics {
    tables: HashMap<u64, TableMetricEntry>,
}

impl TableMetrics {
    pub fn entry_mut(&mut self, table_id: u64) -> &mut TableMetricEntry {
        self.tables.entry(table_id).or_default()
    }

    pub fn entry(&self, table_id: u64) -> Option<&TableMetricEntry> {
        self.tables.get(&table_id)
    }

    pub fn totals(&self) -> (usize, usize) {
        self.tables
            .values()
            .fold((0, 0), |(rendered, total), entry| {
                (rendered + entry.rendered_rows, total + entry.total_rows)
            })
    }

    pub fn clear(&mut self) {
        self.tables.clear();
    }
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct CacheStats {
    pub hits: u64,
    pub misses: u64,
}

impl CacheStats {
    pub fn record_hit(&mut self) {
        self.hits += 1;
    }

    pub fn record_miss(&mut self) {
        self.misses += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn update_widths_detects_large_change() {
        let mut entry = TableMetricEntry::default();
        assert_eq!(entry.update_widths(&[100.0, 120.0], 1), WidthChange::None);
        assert_eq!(entry.update_widths(&[100.2, 120.6], 2), WidthChange::Small);
        assert_eq!(entry.update_widths(&[140.0, 120.6], 3), WidthChange::Large);
    }

    #[test]
    fn font_size_change_clears_persisted_widths() {
        let mut entry = TableMetricEntry::default();

        // Set some persisted widths at font size 14.0
        entry.set_persisted_width(1, 100.0);
        entry.set_persisted_width(2, 150.0);
        assert!(!entry.check_font_size_change(14.0)); // First call sets baseline
        assert_eq!(entry.persisted_width(1), Some(100.0));
        assert_eq!(entry.persisted_width(2), Some(150.0));

        // Small font size change (within epsilon) should NOT clear
        assert!(!entry.check_font_size_change(14.3));
        assert_eq!(entry.persisted_width(1), Some(100.0));

        // Large font size change should clear all persisted widths
        assert!(entry.check_font_size_change(16.0));
        assert_eq!(entry.persisted_width(1), None);
        assert_eq!(entry.persisted_width(2), None);
        assert_eq!(entry.persisted_font_size, Some(16.0));
    }

    #[test]
    fn row_management_tracks_rows_and_rendered_counts() {
        let mut entry = TableMetricEntry::default();
        entry.begin_pass(3);
        assert_eq!(entry.total_rows, 3);
        assert_eq!(entry.rendered_rows, 0);

        let row = entry.ensure_row(2);
        row.max_height = 12.0;
        row.dirty = true;

        assert_eq!(entry.row(2).map(|r| r.max_height), Some(12.0));
        assert_eq!(entry.row(2).map(|r| r.dirty), Some(true));
        assert!(entry.row(5).is_none());

        entry.note_row_rendered();
        entry.note_row_rendered();
        assert_eq!(entry.rendered_rows, 2);
    }

    #[test]
    fn persisted_widths_can_be_added_and_removed() {
        let mut entry = TableMetricEntry::default();
        assert!(entry.persisted_width(10).is_none());
        entry.set_persisted_width(10, 123.0);
        assert_eq!(entry.persisted_width(10), Some(123.0));
        entry.remove_persisted_width(10);
        assert!(entry.persisted_width(10).is_none());
    }

    #[test]
    fn update_widths_handles_empty_and_length_change() {
        let mut entry = TableMetricEntry::default();
        assert_eq!(entry.update_widths(&[], 1), WidthChange::None);
        assert!(entry.current_widths().is_empty());

        assert_eq!(entry.update_widths(&[50.0, 60.0], 2), WidthChange::None);
        assert_eq!(entry.update_widths(&[50.0], 3), WidthChange::Large);
    }

    #[test]
    fn header_height_updates_when_changed() {
        let mut entry = TableMetricEntry::default();
        assert!(entry.header_height().is_none());
        assert!(entry.update_header_height(24.0));
        assert_eq!(entry.header_height(), Some(24.0));
        assert!(!entry.update_header_height(24.2));
        assert!(entry.update_header_height(30.0));
        assert_eq!(entry.header_height(), Some(30.0));
    }

    #[test]
    fn table_metrics_totals_and_clear() {
        let mut metrics = TableMetrics::default();
        metrics.entry_mut(1).begin_pass(2);
        metrics.entry_mut(1).note_row_rendered();
        metrics.entry_mut(2).begin_pass(3);
        metrics.entry_mut(2).note_row_rendered();
        metrics.entry_mut(2).note_row_rendered();

        assert_eq!(metrics.totals(), (3, 5));
        metrics.clear();
        assert!(metrics.entry(1).is_none());
    }

    #[test]
    fn cache_stats_counts_hits_and_misses() {
        let mut stats = CacheStats::default();
        stats.record_hit();
        stats.record_hit();
        stats.record_miss();
        assert_eq!(stats.hits, 2);
        assert_eq!(stats.misses, 1);
    }
}
