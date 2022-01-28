//!
//! # Rectangular Bounding Boxes and Associated Trait
//!

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::{
    geom::{Point, Shape},
    Int,
};

/// # Rectangular Bounding Box
///
/// Points `p0` and `p1` represent opposite corners of a bounding rectangle.
/// `p0` is always closest to negative-infinity, in both x and y,
/// and `p1` is always closest to positive-infinity.
///
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
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
    /// Create a new [BoundBox] from a single [Point].
    /// The resultant [BoundBox] comprises solely the point, having zero area.
    pub fn from_point(pt: Point) -> Self {
        Self {
            p0: pt.clone(),
            p1: pt.clone(),
        }
    }
    /// Create a new [BoundBox] from two points
    pub fn from_points(p0: Point, p1: Point) -> Self {
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
}

///
/// # Bounding Box Trait
///
/// Methods for interacting with [BoundBox]s.
/// Implementations for [Point]s, [Shape]s, and [BoundBox]s
/// enable geometric transformations such as union and intersection.  
///
pub trait BoundBoxTrait {
    /// Compute the intersection with rectangular bounding box `bbox`.
    /// Creates and returns a new [BoundBox].
    fn intersection(&self, bbox: &BoundBox) -> BoundBox;
    /// Compute the union with rectangular bounding box `bbox`.
    /// Creates and returns a new [BoundBox].
    fn union(&self, bbox: &BoundBox) -> BoundBox;
    /// Compute a rectangular bounding box around the implementing type.
    fn bbox(&self) -> BoundBox;
}

impl BoundBoxTrait for BoundBox {
    fn intersection(&self, bbox: &BoundBox) -> BoundBox {
        let pmin = Point::new(self.p0.x.max(bbox.p0.x), self.p0.y.max(bbox.p0.y));
        let pmax = Point::new(self.p1.x.min(bbox.p1.x), self.p1.y.min(bbox.p1.y));
        if pmin.x > pmax.x || pmin.y > pmax.y {
            return BoundBox::empty();
        }
        BoundBox::new(pmin, pmax)
    }
    fn union(&self, bbox: &BoundBox) -> BoundBox {
        BoundBox::new(
            Point::new(self.p0.x.min(bbox.p0.x), self.p0.y.min(bbox.p0.y)),
            Point::new(self.p1.x.max(bbox.p1.x), self.p1.y.max(bbox.p1.y)),
        )
    }
    fn bbox(&self) -> BoundBox {
        self.clone()
    }
}

impl BoundBoxTrait for Point {
    fn intersection(&self, bbox: &BoundBox) -> BoundBox {
        if !bbox.contains(self) {
            return BoundBox::empty();
        }
        BoundBox::from_point(self.clone())
    }
    fn union(&self, bbox: &BoundBox) -> BoundBox {
        BoundBox::new(
            Point::new(self.x.min(bbox.p0.x), self.y.min(bbox.p0.y)),
            Point::new(self.x.max(bbox.p1.x), self.y.max(bbox.p1.y)),
        )
    }
    fn bbox(&self) -> BoundBox {
        BoundBox::from_point(self.clone())
    }
}

impl BoundBoxTrait for Shape {
    fn intersection(&self, bbox: &BoundBox) -> BoundBox {
        self.bbox().intersection(&bbox)
    }
    fn union(&self, bbox: &BoundBox) -> BoundBox {
        self.bbox().union(&bbox)
    }
    fn bbox(&self) -> BoundBox {
        match self {
            Shape::Rect { ref p0, ref p1 } => BoundBox::from_points(p0.clone(), p1.clone()),
            Shape::Poly { ref pts } => {
                let mut bbox = BoundBox::empty();
                for pt in pts {
                    bbox = bbox.union(&pt.bbox());
                }
                bbox
            }
            Shape::Path { ref pts, ref width } => {
                let mut bbox = BoundBox::empty();
                for pt in pts {
                    bbox = bbox.union(&pt.bbox());
                }
                bbox.expand(*width as Int);
                bbox
            }
        }
    }
}
