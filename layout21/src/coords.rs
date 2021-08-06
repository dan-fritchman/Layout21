//! # Layout21 Coordinate System(s)

// Std-lib imports
use std::convert::TryFrom;
use std::fmt::Debug;

// Crates.io
use derive_more::{Add, AddAssign, DivAssign, From, MulAssign, Sub, SubAssign, Sum};
use enum_dispatch::enum_dispatch;
use serde::{Deserialize, Serialize};

// Local imports
use crate::raw::Dir;

/// # Location Integer Type-Alias
///
/// Many internal fields are conceptually unsigned integers, but also undergo lots of math.
/// Rather than converting at each call-site, most are converted to [Int] and value-checked at creation time.
///
/// Unsigned integers ([usize]) are generally used for indices, such as where the [Index] trait accepts them.
pub type Int = isize;

/// Much of the confusion in a multi-coordinate system such as this
/// lies in keeping track of which numbers are in which units.
/// There are three generally useful units of measure here:
/// * DB Units generally correspond to physical length quantities, e.g. nanometers
/// * Primitive pitches
/// * Per-layer pitches, parameterized by a metal-layer index
#[enum_dispatch]
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum UnitSpeced {
    DbUnits(DbUnits),
    PrimPitches(PrimPitches),
    LayerPitches(LayerPitches),
}
/// Empty trait, largely for auto-generation of [From] and [Into] implementations.
#[enum_dispatch(UnitSpeced)]
pub trait HasUnits: Clone + Copy {}

/// A Scalar Value in Database Units
#[derive(
    From,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    MulAssign,
    DivAssign,
    Sum,
    Debug,
    Default,
    Clone,
    Copy,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
)]
pub struct DbUnits(pub Int);
impl DbUnits {
    /// Every so often we need the raw number, fine. Use sparingly.
    #[inline(always)]
    pub(crate) fn raw(&self) -> Int {
        self.0
    }
}
impl HasUnits for DbUnits {}
impl std::ops::Div<DbUnits> for DbUnits {
    type Output = Int;
    fn div(self, rhs: DbUnits) -> Self::Output {
        self.raw() / rhs.raw()
    }
}
impl std::ops::Div<Int> for DbUnits {
    type Output = Self;
    fn div(self, rhs: Int) -> Self::Output {
        Self(self.raw() / rhs)
    }
}
impl std::ops::Rem<DbUnits> for DbUnits {
    type Output = Int;
    fn rem(self, rhs: DbUnits) -> Self::Output {
        self.raw().rem(rhs.raw())
    }
}
impl std::ops::Mul<DbUnits> for DbUnits {
    type Output = Self;
    fn mul(self, rhs: DbUnits) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}
impl std::ops::Mul<Int> for DbUnits {
    type Output = Self;
    fn mul(self, rhs: Int) -> Self::Output {
        Self(self.0 * rhs)
    }
}
impl std::ops::Mul<usize> for DbUnits {
    type Output = Self;
    fn mul(self, rhs: usize) -> Self::Output {
        Self(Int::try_from(rhs).unwrap() * self.0)
    }
}

/// A Scalar Value in Primitive-Pitches
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct PrimPitches {
    pub dir: Dir,
    pub num: Int,
}
impl HasUnits for PrimPitches {}
impl std::ops::Add<PrimPitches> for PrimPitches {
    type Output = PrimPitches;
    /// Adding primitive-pitch values.
    /// Panics if the two are not in the same direction.
    fn add(self, rhs: Self) -> Self::Output {
        if self.dir != rhs.dir {
            panic!()
        }
        Self {
            dir: self.dir,
            num: self.num + rhs.num,
        }
    }
}

/// A Scalar Value in Layer-Pitches
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct LayerPitches {
    layer: usize,
    num: Int,
}
impl HasUnits for LayerPitches {}

/// Paired "type" zero-data enum for [UnitSpeced]
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum UnitType {
    DbUnits,
    PrimPitches,
    LayerPitches,
}

/// Common geometric pairing of (x,y) coordinates
/// Represents points, sizes, rectangles, and anything else that pairs `x` and `y` fields.
/// *Only* instantiable with [HasUnits] data.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct Xy<T: HasUnits> {
    pub x: T,
    pub y: T,
}
impl<T: HasUnits> Xy<T> {
    /// Create a new [Xy].
    pub fn new(x: T, y: T) -> Xy<T> {
        Self { x, y }
    }
    /// Create a new [Xy] with transposed coordinates.
    pub fn transpose(&self) -> Xy<T> {
        Self {
            y: self.x,
            x: self.y,
        }
    }
}
impl<T: HasUnits> std::ops::Index<Dir> for Xy<T> {
    type Output = T;
    pub fn index(&self, dir: Dir) -> &Self::Output {
        match dir {
            Dir::Horiz => &self.x,
            Dir::Vert => &self.y,
        }
    }
}
impl From<(Int, Int)> for Xy<DbUnits> {
    fn from(tup: (Int, Int)) -> Self {
        Self {
            x: tup.0.into(),
            y: tup.1.into(),
        }
    }
}
impl From<(Int, Int)> for Xy<PrimPitches> {
    fn from(tup: (Int, Int)) -> Self {
        Self {
            x: PrimPitches {
                dir: Dir::Horiz,
                num: tup.0.into(),
            },
            y: PrimPitches {
                dir: Dir::Vert,
                num: tup.1.into(),
            },
        }
    }
}
