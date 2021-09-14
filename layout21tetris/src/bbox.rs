//!
//! # Rectangular Bounding Boxes
//!

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::coords::{HasUnits, Xy};
use crate::placement::Side;

/// # Bounding Rectangular Box
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct BoundBox<T: HasUnits> {
    pub p0: Xy<T>,
    pub p1: Xy<T>,
}
impl<T: HasUnits> BoundBox<T> {
    /// Create a new [BoundBox] from two [Xy] points.
    pub fn new(p0: Xy<T>, p1: Xy<T>) -> Self {
        Self { p0, p1 }
    }
    /// Retrieve our coordinate at [Side] `side`.
    pub fn side(&self, side: Side) -> T {
        match side {
            Side::Left => self.p0.x,
            Side::Right => self.p1.x,
            Side::Bottom => self.p0.y,
            Side::Top => self.p1.y,
        }
    }
}
impl<T: HasUnits + std::cmp::PartialOrd> BoundBox<T> {
    /// Create a new [BoundBox] from potentially unordered pairs of x and y coordinates.
    pub fn from_xy(xs: (T, T), ys: (T, T)) -> Self {
        let (x0, x1) = if xs.0 < xs.1 {
            (xs.0, xs.1)
        } else {
            (xs.1, xs.0)
        };
        let (y0, y1) = if ys.0 < ys.1 {
            (ys.0, ys.1)
        } else {
            (ys.1, ys.0)
        };
        Self::new(Xy::new(x0, y0), Xy::new(x1, y1))
    }
}

/// Trait for types that can be converted to a [BoundBox].
///
/// Includes a single method `boundbox` which returns a [BoundBox] of the type.
pub trait HasBoundBox {
    type Units: HasUnits;
    type Error;
    /// Get a [BoundBox] of the type.
    fn boundbox(&self) -> Result<BoundBox<Self::Units>, Self::Error>;
}
