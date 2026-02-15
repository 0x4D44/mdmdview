//! Geometry primitives for D2 layout and rendering.
//!
//! All coordinates use top-left origin convention:
//! - x increases rightward
//! - y increases downward
//! - Rect.x/y is the top-left corner of the rectangle
//!
//! This matches SVG's coordinate system, so no conversion is needed
//! when generating SVG output.

/// A 2D point.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

impl Point {
    pub fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }

    /// Euclidean distance to another point.
    pub fn distance_to(&self, other: &Point) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx * dx + dy * dy).sqrt()
    }
}

/// An axis-aligned rectangle (top-left origin).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rect {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

impl Rect {
    pub fn new(x: f64, y: f64, width: f64, height: f64) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }

    /// Center point of the rectangle.
    pub fn center(&self) -> Point {
        Point {
            x: self.x + self.width / 2.0,
            y: self.y + self.height / 2.0,
        }
    }

    /// Right edge x-coordinate.
    pub fn right(&self) -> f64 {
        self.x + self.width
    }

    /// Bottom edge y-coordinate.
    pub fn bottom(&self) -> f64 {
        self.y + self.height
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_point_distance() {
        let a = Point::new(0.0, 0.0);
        let b = Point::new(3.0, 4.0);
        assert!((a.distance_to(&b) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn test_rect_center() {
        let r = Rect::new(10.0, 20.0, 100.0, 50.0);
        let c = r.center();
        assert!((c.x - 60.0).abs() < 1e-10);
        assert!((c.y - 45.0).abs() < 1e-10);
    }

    #[test]
    fn test_rect_edges() {
        let r = Rect::new(10.0, 20.0, 100.0, 50.0);
        assert!((r.right() - 110.0).abs() < 1e-10);
        assert!((r.bottom() - 70.0).abs() < 1e-10);
    }
}
