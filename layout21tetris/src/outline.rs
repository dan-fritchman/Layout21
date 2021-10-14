//!
//! # Tetris-Cell Outlines
//!

// Std-lib imports
use std::fmt::Debug;

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::coords::{Int, PrimPitches};
use crate::raw::{Dir, LayoutError, LayoutResult};

/// # Block Outline
///
/// All block-outlines are "tetris shaped" rectilinear polygons, and are `layout21::tetris`'s namesake.
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
/// Such an outline has vertices in Cartesian space at:
/// `[(0,0), (x[0], 0), (x[0], y[0]), (x[1], y[0]), ... , (0, y[-1]), (0,0)]`
/// With the first point at the origin, the final point at (0, y[-1]), and its connection back to the origin all implied.
///
/// Example: a rectangular Outline would requires single entry for each of `x` and `y`,
/// at the rectangle's vertex opposite the origin in both axes.
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Outline {
    pub x: Vec<PrimPitches>,
    pub y: Vec<PrimPitches>,
}
impl Outline {
    /// Outline constructor, with inline checking for validity of `x` & `y` vectors
    pub fn new(x: &[Int], y: &[Int]) -> LayoutResult<Self> {
        // Convert into [PrimPitches] united-objects, and return a new Self.
        let x = x.into_iter().map(|i| PrimPitches::x(*i)).collect();
        let y = y.into_iter().map(|i| PrimPitches::y(*i)).collect();
        Self::from_prim_pitches(x, y)
    }
    /// Outline constructor from primitive-pitches
    pub fn from_prim_pitches(x: Vec<PrimPitches>, y: Vec<PrimPitches>) -> LayoutResult<Self> {
        // Check that x and y are of compatible lengths
        if x.len() < 1 || x.len() != y.len() {
            LayoutError::fail("Invalid zero-length Outline dimensions")?;
        }
        // Check for:
        // * Correct directions
        // * all non-negative values
        for k in 0..x.len() {
            if x[k].dir != Dir::Horiz || y[k].dir != Dir::Vert {
                LayoutError::fail("Invalid Outline direction(s)")?;
            }
            if x[k].num < 0 || y[k].num < 0 {
                LayoutError::fail("Invalid Outline with negative coordinate(s)")?;
            }
        }
        // Check for:
        // * x non-increasing-ness,
        // * y for non-decreasing-ness
        for k in 1..x.len() {
            if x[k].num > x[k - 1].num {
                LayoutError::fail("Invalid Outline with non-increasing x-coordinates")?;
            }
            if y[k].num < y[k - 1].num {
                LayoutError::fail("Invalid Outline with non-decreasing y-coordinates")?;
            }
        }
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
