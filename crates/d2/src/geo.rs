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

    /// Test whether two rectangles overlap (strict intersection, not touching).
    pub fn intersects(&self, other: &Rect) -> bool {
        self.x < other.right()
            && self.right() > other.x
            && self.y < other.bottom()
            && self.bottom() > other.y
    }

    /// Minimum axis-aligned separation between two rects.
    /// Returns positive when separated, negative when overlapping.
    pub fn min_separation(&self, other: &Rect) -> f64 {
        let sep_x = if self.right() <= other.x {
            other.x - self.right()
        } else if other.right() <= self.x {
            self.x - other.right()
        } else {
            -(self.right().min(other.right()) - self.x.max(other.x))
        };

        let sep_y = if self.bottom() <= other.y {
            other.y - self.bottom()
        } else if other.bottom() <= self.y {
            self.y - other.bottom()
        } else {
            -(self.bottom().min(other.bottom()) - self.y.max(other.y))
        };

        if sep_x >= 0.0 && sep_y >= 0.0 {
            // Both axes separated: closest gap is the smaller separation
            sep_x.min(sep_y)
        } else {
            // At least one axis overlaps: take the larger (least negative or positive)
            sep_x.max(sep_y)
        }
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

    #[test]
    fn test_rect_intersects() {
        // Overlapping rects
        let a = Rect::new(0.0, 0.0, 10.0, 10.0);
        let b = Rect::new(5.0, 5.0, 10.0, 10.0);
        assert!(a.intersects(&b));
        assert!(b.intersects(&a));

        // Touching (shared edge) — not intersecting (strict)
        let c = Rect::new(10.0, 0.0, 10.0, 10.0);
        assert!(!a.intersects(&c), "touching rects should not intersect");
        assert!(!c.intersects(&a));

        // Fully separated
        let d = Rect::new(20.0, 20.0, 5.0, 5.0);
        assert!(!a.intersects(&d));
        assert!(!d.intersects(&a));

        // Nested (one inside the other)
        let outer = Rect::new(0.0, 0.0, 100.0, 100.0);
        let inner = Rect::new(10.0, 10.0, 20.0, 20.0);
        assert!(outer.intersects(&inner));
        assert!(inner.intersects(&outer));
    }

    #[test]
    fn test_rect_min_separation() {
        // Positive: separated horizontally
        let a = Rect::new(0.0, 0.0, 10.0, 10.0);
        let b = Rect::new(15.0, 0.0, 10.0, 10.0);
        let sep = a.min_separation(&b);
        assert!((sep - 5.0).abs() < 1e-10, "expected 5.0, got {}", sep);

        // Zero: touching
        let c = Rect::new(10.0, 0.0, 10.0, 10.0);
        let sep = a.min_separation(&c);
        assert!(sep.abs() < 1e-10, "expected 0.0, got {}", sep);

        // Negative: overlapping
        let d = Rect::new(5.0, 5.0, 10.0, 10.0);
        let sep = a.min_separation(&d);
        assert!(sep < 0.0, "expected negative, got {}", sep);
        // Overlap on x: min(10,15) - max(0,5) = 10-5 = 5, so sep_x = -5
        // Overlap on y: min(10,15) - max(0,5) = 10-5 = 5, so sep_y = -5
        // Both negative → max(-5, -5) = -5
        assert!((sep - (-5.0)).abs() < 1e-10, "expected -5.0, got {}", sep);

        // Both axes separated: returns the tighter gap (min)
        let e = Rect::new(15.0, 20.0, 10.0, 10.0);
        // e is 5px right of a and 10px below a
        // sep_x = 5.0, sep_y = 10.0 → min_separation = 5.0
        let sep = a.min_separation(&e);
        assert!((sep - 5.0).abs() < 1e-10, "both-axes-separated should return min gap, got {}", sep);
    }
}
