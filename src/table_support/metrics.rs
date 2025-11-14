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
    pub resolved_widths: Vec<f32>,
    pub last_width_frame: u64,
    pub last_discard_frame: Option<u64>,
    pub persisted_column_widths: HashMap<u64, f32>,
    pub pending_user_resize: Option<PendingResize>,
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

    pub fn current_widths(&self) -> &[f32] {
        &self.resolved_widths
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
}
