// Std-lib imports
use std::fmt::Debug;

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::coords::{Int, PrimPitches};
use crate::raw::{Dir, LayoutError, LayoutResult};

/// Block Outlines are "Tetris Shaped" rectilinear polygons
///
/// These boundaries are closed, consist solely of 90-degree rectangular turns,
/// and are specified by a counter-clockwise set of points.
/// "Holes" such as the shapes "O" and "8" and "divots" such as the shapes "U" and "H" are not supported.
///
/// Two equal-length vectors `x` and `y` describe an Outline's points.
/// Counter-clockwise-ness and divot-free-ness requires that:
/// * (a) `x` values are monotonically non-increasing, and
/// * (b) `y` values are monotonically non-decreasing
///
/// In point-space terms, such an outline has vertices at:
/// `[(0,0), (x[0], 0), (x[0], y[0]), (x[1], y[0]), ... , (0, y[-1]), (0,0)]`
/// With the final point at (0, y[-1]), and its connection back to the origin both implied.
///
/// Example: a rectangular Outline would require a single entry for each of `x` and `y`,
/// at the rectangle's vertex opposite the origin in both axes.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Outline {
    pub x: Vec<PrimPitches>,
    pub y: Vec<PrimPitches>,
}
impl Outline {
    /// Outline constructor, with inline checking for validity of `x` & `y` vectors
    pub fn new(x: &[Int], y: &[Int]) -> LayoutResult<Self> {
        // Check that x and y are of compatible lengths
        if x.len() < 1 || x.len() != y.len() {
            // FIXME: probably worth creating a specific error type
            return Err(LayoutError::Validation);
        }
        // Check for:
        // * x non-increasing-ness,
        // * y for non-decreasing-ness
        // * all non-negative values
        if x[0] < 0 || y[0] < 0 {
            return Err(LayoutError::Validation);
        }
        for k in 1..x.len() {
            if x[k] > x[k - 1] {
                return Err(LayoutError::Validation);
            }
            if y[k] < y[k - 1] {
                return Err(LayoutError::Validation);
            }
            if x[k] < 0 || y[k] < 0 {
                return Err(LayoutError::Validation);
            }
        }
        // Convert into [PrimPitches] united-objects, and return a new Self.
        let x = x
            .into_iter()
            .map(|i| PrimPitches {
                num: *i,
                dir: Dir::Horiz,
            })
            .collect();
        let y = y
            .into_iter()
            .map(|i| PrimPitches {
                num: *i,
                dir: Dir::Vert,
            })
            .collect();
        Ok(Self { x, y })
    }
    /// Create a new rectangular outline of dimenions `x` by `y`
    pub fn rect(x: Int, y: Int) -> LayoutResult<Self> {
        Self::new(&[x], &[y])
    }
    /// Maximum x-coordinate
    /// (Which is also always the *first* x-coordinate)
    pub fn xmax(&self) -> PrimPitches {
        self.x[0]
    }
    /// Maximum y-coordinate
    /// (Which is also always the *last* y-coordinate)
    pub fn ymax(&self) -> PrimPitches {
        self.y[self.y.len() - 1]
    }
    /// Maximum coordinate in [Dir] `dir`
    pub fn max(&self, dir: Dir) -> PrimPitches {
        match dir {
            Dir::Horiz => self.xmax(),
            Dir::Vert => self.ymax(),
        }
    }
}
