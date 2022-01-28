//!
//! # Geometry Module
//!
//! Defines the core geometric types including [Point], [Shape], and [Transform],
//! and their core operations.
//!

// Std-Lib
use std::convert::TryFrom;

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::Int;

/// # Point in two-dimensional layout-space
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct Point {
    pub x: Int,
    pub y: Int,
}
impl Point {
    /// Create a new [Point] from (x,y) coordinates
    pub fn new(x: Int, y: Int) -> Self {
        Self { x, y }
    }
    /// Create a new [Point] which serves as an offset in direction `dir`
    pub fn offset(val: Int, dir: Dir) -> Self {
        match dir {
            Dir::Horiz => Self { x: val, y: 0 },
            Dir::Vert => Self { x: 0, y: val },
        }
    }
    /// Create a new point shifted by `x` in the x-dimension and by `y` in the y-dimension
    pub fn shift(&self, p: &Point) -> Point {
        Point {
            x: p.x + self.x,
            y: p.y + self.y,
        }
    }
    /// Create a new point scaled by `p.x` in the x-dimension and by `p.y` in the y-dimension
    pub fn scale(&self, p: &Point) -> Point {
        Point {
            x: p.x * self.x,
            y: p.y * self.y,
        }
    }
    /// Get the coordinate associated with direction `dir`
    pub fn coord(&self, dir: Dir) -> Int {
        match dir {
            Dir::Horiz => self.x,
            Dir::Vert => self.y,
        }
    }
    /// Create a ne [Point], transformed from our original location by `transform`
    /// Coordinate transforms are applied in floating-point format,
    /// largely for rotations, and then rounded to the nearest integer.
    pub fn transform(&self, trans: &Transform) -> Point {
        let xf = self.x as f64;
        let yf = self.y as f64;
        let x = trans.a[0][0] * xf + trans.a[0][1] * yf + trans.b[0];
        let y = trans.a[1][0] * xf + trans.a[1][1] * yf + trans.b[1];
        Self {
            x: x.round() as Int,
            y: y.round() as Int,
        }
    }
}
/// Direction Enumeration
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum Dir {
    Horiz,
    Vert,
}
impl Dir {
    /// Whichever direction we are, return the other one.
    pub fn other(self) -> Self {
        match self {
            Self::Horiz => Self::Vert,
            Self::Vert => Self::Horiz,
        }
    }
}
impl std::ops::Not for Dir {
    type Output = Self;
    /// Exclamation Operator returns the opposite direction
    fn not(self) -> Self::Output {
        self.other()
    }
}

/// # Shape 
/// 
/// The primary geometric primitive comprising raw layout. 
/// Variants include [Rect], [Polygon], and [Path]. 
/// 
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Shape {
    Rect { p0: Point, p1: Point },
    Poly { pts: Vec<Point> },
    Path { width: usize, pts: Vec<Point> },
}
impl Shape {
    /// Retrieve our "origin", or first [Point]
    pub fn point0(&self) -> &Point {
        match *self {
            Shape::Rect { ref p0, p1: _ } => p0,
            Shape::Poly { ref pts } => &pts[0],
            Shape::Path { ref pts, .. } => &pts[0],
        }
    }
    /// Calculate our center-point
    pub fn center(&self) -> Point {
        match *self {
            Shape::Rect { ref p0, ref p1 } => Point::new((p0.x + p1.x) / 2, (p0.y + p1.y) / 2),
            Shape::Path { ref pts, .. } => {
                // Place on the center of the first segment
                let p0 = &pts[0];
                let p1 = &pts[1];
                Point::new((p0.x + p1.x) / 2, (p0.y + p1.y) / 2)
            }
            Shape::Poly { .. } => {
                unimplemented!("Shape::Poly/Path::center");
            }
        }
    }
    /// Indicate whether this shape is (more or less) horizontal or vertical
    /// Primarily used for orienting label-text
    pub fn orientation(&self) -> Dir {
        match *self {
            Shape::Rect { ref p0, ref p1 } => {
                if (p1.x - p0.x).abs() < (p1.y - p0.y).abs() {
                    return Dir::Vert;
                }
                Dir::Horiz
            }
            // Polygon and Path elements always horizontal, at least for now
            Shape::Poly { .. } | Shape::Path { .. } => Dir::Horiz,
        }
    }
    /// Apply matrix-vector [Tranform] `trans`.
    /// Creates a new shape at a location equal to the transformation of our own.
    pub fn transform(&self, trans: &Transform) -> Shape {
        match *self {
            Shape::Rect { ref p0, ref p1 } => Shape::Rect {
                p0: p0.transform(trans),
                p1: p1.transform(trans),
            },
            Shape::Poly { ref pts } => Shape::Poly {
                pts: pts.iter().map(|p| p.transform(trans)).collect(),
            },
            Shape::Path { ref pts, ref width } => Shape::Path {
                pts: pts.iter().map(|p| p.transform(trans)).collect(),
                width: *width,
            },
        }
    }
    /// Shift coordinates by the (x,y) values specified in `pt`
    pub fn shift(&mut self, pt: &Point) {
        match *self {
            Shape::Rect {
                ref mut p0,
                ref mut p1,
            } => {
                p0.x += pt.x;
                p0.y += pt.y;
                p1.x += pt.x;
                p1.y += pt.y;
            }
            Shape::Poly { ref mut pts } => {
                for p in pts.iter_mut() {
                    p.x += pt.x;
                    p.y += pt.y;
                }
            }
            Shape::Path { ref mut pts, .. } => {
                for p in pts.iter_mut() {
                    p.x += pt.x;
                    p.y += pt.y;
                }
            }
        }
    }
    /// Boolean indication of whether we contain point `pt`
    pub fn contains(&self, pt: &Point) -> bool {
        match self {
            Shape::Rect { ref p0, ref p1 } => {
                p0.x.min(p1.x) <= pt.x
                    && p0.x.max(p1.x) >= pt.x
                    && p0.y.min(p1.y) <= pt.y
                    && p0.y.max(p1.y) >= pt.y
            }
            Shape::Poly { .. } => false, // FIXME! todo!(),
            Shape::Path { ref width, ref pts } => {
                // Break into segments, and check for intersection with each
                // Probably not the most efficient way to do this, but a start.
                // Only "Manhattan paths", i.e. those with segments solely running vertically or horizontally, are supported.
                // FIXME: even with this method, there are some small pieces at corners which we'll miss.
                // Whether these are relevant in real life, tbd.
                let width = Int::try_from(*width).unwrap(); // FIXME: probably store these signed, check them on creation
                for k in 0..pts.len() - 1 {
                    let rect = if pts[k].x == pts[k + 1].x {
                        Shape::Rect {
                            p0: Point::new(pts[k].x - width / 2, pts[k].y),
                            p1: Point::new(pts[k].x + width / 2, pts[k + 1].y),
                        }
                    } else if pts[k].y == pts[k + 1].y {
                        Shape::Rect {
                            p0: Point::new(pts[k].x, pts[k].y - width / 2),
                            p1: Point::new(pts[k + 1].x, pts[k].y + width / 2),
                        }
                    } else {
                        unimplemented!("Unsupported Non-Manhattan Path")
                    };
                    if rect.contains(pt) {
                        return true;
                    }
                }
                false
            }
        }
    }
}

/// # Matrix-Vector Transformation
///
/// 2x2 rotation-matrix and two-entry translation vector,
/// used for relative movement of [Point]s and [Shape]s.
///
#[derive(Debug, Default, Clone)]
pub struct Transform {
    /// Rotation / Transformation Matrix
    /// Represented in row-major order
    pub a: [[f64; 2]; 2],
    /// X-Y Translation
    pub b: [f64; 2],
}
impl Transform {
    /// The identity transform, leaving any transformed object unmodified
    pub fn identity() -> Self {
        Self {
            a: [[1., 0.], [0., 1.]],
            b: [0., 0.],
        }
    }
    /// Translation by (x,y)
    pub fn translate(x: f64, y: f64) -> Self {
        Self {
            a: [[1., 0.], [0., 1.]],
            b: [x, y],
        }
    }
    /// A transform to rotate by `angle` degrees
    pub fn rotate(angle: f64) -> Self {
        let sin = angle.to_radians().sin();
        let cos = angle.to_radians().cos();
        Self {
            a: [[cos, -sin], [sin, cos]],
            b: [0., 0.],
        }
    }
    /// A transform to reflect about the x-axis
    pub fn reflect_vert() -> Self {
        Self {
            a: [[1., 0.], [0., -1.]],
            b: [0., 0.],
        }
    }
    /// Create a transform from instance fields: location, rotation, and reflection
    pub fn from_instance(loc: &Point, reflect_vert: bool, angle: Option<f64>) -> Self {
        let b = [loc.x as f64, loc.y as f64];
        let (mut sin, mut cos) = (0., 1.);
        if let Some(angle) = angle {
            sin = angle.to_radians().sin();
            cos = angle.to_radians().cos();
        }
        let cos_refl = if reflect_vert { -cos } else { cos };
        let a = [[cos, -sin], [sin, cos_refl]];
        Self { a, b }
    }
    /// Create a new [Transform] that is the cascade of `parent` and `child`.
    ///
    /// "Parents" and "children" refer to typical layout-instance hierarchies,
    /// in which each layer of instance has a nested set of transformations relative to its top-level parent.
    ///
    /// Note this operation *is not* commutative.
    /// For example the set of transformations:
    /// * (a) Reflect vertically, then
    /// * (b) Translate by (1,1)
    /// * (c) Place a point at (local coordinate) (1,1)
    /// Lands said point at (2,-2) in top-level space,
    /// whereas reversing the order of (a) and (b) lands it at (2,0).
    ///
    pub fn cascade(parent: &Transform, child: &Transform) -> Transform {
        // The result-transform's origin is the parent's origin,
        // plus the parent-transformed child's origin
        let mut b = matvec(&parent.a, &child.b);
        b[0] += parent.b[0];
        b[1] += parent.b[1];
        // And the cascade-matrix is the product of the parent's and child's
        let a = matmul(&parent.a, &child.a);
        Self { a, b }
    }
}
/// Multiply 2x2 matrices, returning a new 2x2 matrix
fn matmul(a: &[[f64; 2]; 2], b: &[[f64; 2]; 2]) -> [[f64; 2]; 2] {
    [
        [
            a[0][0] * b[0][0] + a[0][1] * b[1][0],
            a[0][0] * b[0][1] + a[0][1] * b[1][1],
        ],
        [
            a[1][0] * b[0][0] + a[1][1] * b[1][0],
            a[1][0] * b[0][1] + a[1][1] * b[1][1],
        ],
    ]
}
/// Multiply a 2x2 matrix by a 2-entry vector, returning a new 2-entry vector
fn matvec(a: &[[f64; 2]; 2], b: &[f64; 2]) -> [f64; 2] {
    [
        a[0][0] * b[0] + a[0][1] * b[1],
        a[1][0] * b[0] + a[1][1] * b[1],
    ]
}

#[cfg(test)]
pub mod tests {
    use super::*;
    #[test]
    fn transform_identity() {
        let shape1 = Shape::Rect {
            p0: Point::new(0, 0),
            p1: Point::new(1, 1),
        };
        let trans = Transform::identity();
        let shape2 = shape1.transform(&trans);
        assert_eq!(shape2, shape1);
    }
    #[test]
    fn transform_rotate() {
        let shape1 = Shape::Rect {
            p0: Point::new(0, 0),
            p1: Point::new(1, 1),
        };
        let trans = Transform::rotate(90.);
        let shape2 = shape1.transform(&trans);
        assert_eq!(
            shape2,
            Shape::Rect {
                p0: Point::new(0, 0),
                p1: Point::new(-1, 1),
            }
        );
        let shape3 = shape2.transform(&trans);
        assert_eq!(
            shape3,
            Shape::Rect {
                p0: Point::new(0, 0),
                p1: Point::new(-1, -1),
            }
        );
        let shape4 = shape3.transform(&trans);
        assert_eq!(
            shape4,
            Shape::Rect {
                p0: Point::new(0, 0),
                p1: Point::new(1, -1),
            }
        );
        let shape0 = shape4.transform(&trans);
        assert_eq!(shape0, shape1);
    }
    #[test]
    fn test_cascade1() {
        let trans1 = Transform::reflect_vert();
        let trans2 = Transform::translate(1., 1.);

        let p = Point::new(1, 1);
        let cascade1 = Transform::cascade(&trans1, &trans2);
        let pc1 = p.transform(&cascade1);
        assert_eq!(pc1, Point::new(2, -2));

        let cascade2 = Transform::cascade(&trans2, &trans1);
        let pc1 = p.transform(&cascade2);
        assert_eq!(pc1, Point::new(2, 0));
    }
}
