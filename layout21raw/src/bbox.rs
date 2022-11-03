//!
//! # Rectangular Bounding Boxes and Associated Trait
//!

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::{
    geom::{Point, Shape},
    Int, Rect,
};

/// # Axis-Aligned Rectangular Bounding Box
///
/// Points `p0` and `p1` represent opposite corners of a bounding rectangle.
/// `p0` is always closest to negative-infinity, in both x and y,
/// and `p1` is always closest to positive-infinity.
///
#[derive(Debug, Default, Copy, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct BoundBox {
    pub p0: Point,
    pub p1: Point,
}
impl BoundBox {
    /// Create a new [BoundBox] from two [Point]s.
    /// Callers are responsible for ensuring that p0.x <= p1.x, and p0.y <= p1.y.
    fn new(p0: Point, p1: Point) -> Self {
        Self { p0, p1 }
    }
    #[inline]
    pub fn width(&self) -> Int {
        self.p1.x - self.p0.x
    }
    #[inline]
    pub fn height(&self) -> Int {
        self.p1.y - self.p0.y
    }
    /// Create a new [BoundBox] from a single [Point].
    /// The resultant [BoundBox] comprises solely the point, having zero area.
    pub fn from_point(pt: &Point) -> Self {
        Self {
            p0: pt.clone(),
            p1: pt.clone(),
        }
    }
    /// Create a new [BoundBox] from two points
    pub fn from_points(p0: &Point, p1: &Point) -> Self {
        Self {
            p0: Point::new(p0.x.min(p1.x), p0.y.min(p1.y)),
            p1: Point::new(p0.x.max(p1.x), p0.y.max(p1.y)),
        }
    }
    /// Create an empty, otherwise invalid [BoundBox]
    pub fn empty() -> Self {
        Self {
            p0: Point::new(Int::MAX, Int::MAX),
            p1: Point::new(Int::MIN, Int::MIN),
        }
    }
    /// Boolean indication of whether a box is empty
    pub fn is_empty(&self) -> bool {
        self.p0.x > self.p1.x || self.p0.y > self.p1.y
    }
    /// Boolean indication of whether [Point] `pt` lies inside out box.
    pub fn contains(&self, pt: &Point) -> bool {
        self.p0.x <= pt.x && self.p1.x >= pt.x && self.p0.y <= pt.y && self.p1.y >= pt.y
    }
    /// Expand an existing [BoundBox] in all directions by `delta`
    pub fn expand(&mut self, delta: Int) {
        self.p0.x -= delta;
        self.p0.y -= delta;
        self.p1.x += delta;
        self.p1.y += delta;
    }
    /// Get the box's size as an (x,y) tuple
    pub fn size(&self) -> (Int, Int) {
        (self.p1.x - self.p0.x, self.p1.y - self.p0.y)
    }
    /// Get the box's center
    pub fn center(&self) -> Point {
        Point::new((self.p0.x + self.p1.x) / 2, (self.p0.y + self.p1.y) / 2)
    }

    #[inline]
    pub fn into_rect(self) -> Rect {
        Rect::from(self)
    }
}

impl From<Rect> for BoundBox {
    fn from(r: Rect) -> Self {
        debug_assert!(r.p0.x <= r.p1.x);
        debug_assert!(r.p0.y <= r.p1.y);
        Self { p0: r.p0, p1: r.p1 }
    }
}

///
/// # Bounding Box Trait
///
/// Methods for interacting with [BoundBox]s.
/// Implementations for [Point]s, [Shape]s, and [BoundBox]s
/// enable geometric transformations such as union and intersection.  
///
pub trait BoundBoxTrait {
    /// Compute a rectangular bounding box around the implementing type.
    fn bbox(&self) -> BoundBox;
    /// Compute the intersection with rectangular bounding box `bbox`.
    /// Creates and returns a new [BoundBox].
    /// Default implementation is to return the intersection of `self.bbox()` and `bbox`.
    fn intersection(&self, bbox: &BoundBox) -> BoundBox {
        self.bbox().intersection(&bbox)
    }
    /// Compute the union with rectangular bounding box `bbox`.
    /// Creates and returns a new [BoundBox].
    /// Default implementation is to return the union of `self.bbox()` and `bbox`.
    fn union(&self, bbox: &BoundBox) -> BoundBox {
        self.bbox().union(&bbox)
    }
}
impl BoundBoxTrait for BoundBox {
    fn bbox(&self) -> BoundBox {
        // We're great as we are, as a [BoundBox] already.
        // Create a clone to adhere to our "new bbox" return-type.
        self.clone()
    }
    fn intersection(&self, bbox: &BoundBox) -> BoundBox {
        let pmin = Point::new(self.p0.x.max(bbox.p0.x), self.p0.y.max(bbox.p0.y));
        let pmax = Point::new(self.p1.x.min(bbox.p1.x), self.p1.y.min(bbox.p1.y));
        // Check for empty intersection, and return an empty box if so
        if pmin.x > pmax.x || pmin.y > pmax.y {
            return BoundBox::empty();
        }
        // Otherwise return the intersection
        BoundBox::new(pmin, pmax)
    }
    fn union(&self, bbox: &BoundBox) -> BoundBox {
        if bbox.is_empty() {
            return *self;
        }
        if self.is_empty() {
            return *bbox;
        }
        // Take the minimum and maximum of the two bounding boxes
        BoundBox::new(
            Point::new(self.p0.x.min(bbox.p0.x), self.p0.y.min(bbox.p0.y)),
            Point::new(self.p1.x.max(bbox.p1.x), self.p1.y.max(bbox.p1.y)),
        )
    }
}
impl BoundBoxTrait for Point {
    fn bbox(&self) -> BoundBox {
        BoundBox::from_point(self)
    }
    fn intersection(&self, bbox: &BoundBox) -> BoundBox {
        if !bbox.contains(self) {
            return BoundBox::empty();
        }
        bbox.intersection(&BoundBox::from_point(self))
    }
    fn union(&self, bbox: &BoundBox) -> BoundBox {
        BoundBox::new(
            Point::new(self.x.min(bbox.p0.x), self.y.min(bbox.p0.y)),
            Point::new(self.x.max(bbox.p1.x), self.y.max(bbox.p1.y)),
        )
    }
}
impl BoundBoxTrait for Shape {
    fn bbox(&self) -> BoundBox {
        // Dispatch based on shape-type, either two-Point or multi-Point form.
        match self {
            Shape::Rect(ref r) => BoundBox::from_points(&r.p0, &r.p1),
            Shape::Polygon(ref p) => (&p.points).bbox(),
            Shape::Path(ref p) => (&p.points).bbox(),
            Shape::Point(ref p) => BoundBox::from_point(p),
        }
    }
}

impl BoundBoxTrait for Rect {
    fn bbox(&self) -> BoundBox {
        BoundBox::from_points(&self.p0, &self.p1)
    }
}

impl BoundBoxTrait for Vec<Point> {
    fn bbox(&self) -> BoundBox {
        // Take the union of all points in the vector
        let mut bbox = BoundBox::empty();
        for pt in self {
            bbox = bbox.union(&pt.bbox());
        }
        bbox
    }
}
