#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct RowMetrics {
    pub max_height: f32,
    pub dirty: bool,
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct TableMetrics {
    pub rows: Vec<RowMetrics>,
    pub rendered_rows: usize,
    pub total_rows: usize,
}

impl TableMetrics {
    pub fn ensure_row(&mut self, index: usize) -> &mut RowMetrics {
        if index >= self.rows.len() {
            self.rows.resize_with(index + 1, RowMetrics::default);
        }
        &mut self.rows[index]
    }

    pub fn begin_pass(&mut self, total_rows: usize) {
        self.total_rows = total_rows;
        self.rendered_rows = 0;
    }

    pub fn note_row_rendered(&mut self) {
        self.rendered_rows += 1;
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
