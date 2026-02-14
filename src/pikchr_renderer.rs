//! Pikchr diagram renderer.
//!
//! Renders Pikchr markup to SVG using the embedded pikchr C library,
//! then rasterizes SVG to egui textures via usvg/resvg.
//! Synchronous rendering -- no worker threads needed.

use std::sync::Arc;

// TODO: Full implementation in Stage 2
#[allow(dead_code)]
pub(crate) struct PikchrRenderer {
    fontdb: Arc<usvg::fontdb::Database>,
}

#[allow(dead_code)]
impl PikchrRenderer {
    pub(crate) fn new() -> Self {
        let mut db = usvg::fontdb::Database::new();
        db.load_system_fonts();
        Self {
            fontdb: Arc::new(db),
        }
    }

    pub(crate) fn begin_frame(&self) {}

    pub(crate) fn has_pending(&self) -> bool {
        false
    }

    pub(crate) fn release_gpu_textures(&self) {}

    pub(crate) fn render_block(
        &self,
        _ui: &mut egui::Ui,
        _code: &str,
        _ui_scale: f32,
        _code_font_size: f32,
    ) -> bool {
        // TODO: Implement in Stage 2
        false
    }
}
