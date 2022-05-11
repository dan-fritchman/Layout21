//!
//! # Tetris Coordinate System(s)
//!

// Std-lib imports
use std::convert::TryFrom;
use std::fmt::Debug;

// Crates.io
use derive_more::{Add, AddAssign, DivAssign, From, MulAssign, Sub, SubAssign, Sum};
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

/// # Unit-Specified Distances Enumeration
///
/// Much of the confusion in a multi-coordinate system such as this
/// lies in keeping track of which numbers are in which units.
///
/// There are three generally useful units of measure here:
/// * DB Units generally correspond to physical length quantities, e.g. nanometers
/// * Primitive pitches
/// * Per-layer pitches, parameterized by a metal-layer index
///
#[derive(From, Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum UnitSpeced {
    /// Database Units, e.g. nanometers
    DbUnits(DbUnits),
    /// Primitive Pitches, parameterized by direction
    PrimPitches(PrimPitches),
    /// Per-Layer Pitches, parameterized by a metal-layer index
    LayerPitches(LayerPitches),
}
/// # HasUnits
///
/// A trait for types that have a unit-speced value.
/// Largely synonymous with being a variant of [UnitSpeced].
///
pub trait HasUnits: Clone + Copy {
    /// Retrieve the raw number being spec'ed. Used sparingly, e.g. for exports.
    fn raw(&self) -> Int;
}
impl HasUnits for UnitSpeced {
    /// Dispatch the `raw` method to each variant
    fn raw(&self) -> Int {
        match self {
            UnitSpeced::DbUnits(x) => x.raw(),
            UnitSpeced::PrimPitches(x) => x.raw(),
            UnitSpeced::LayerPitches(x) => x.raw(),
        }
    }
}

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
impl HasUnits for DbUnits {
    /// Every so often we need the raw number, fine. Use sparingly.
    #[inline(always)]
    fn raw(&self) -> Int {
        self.0
    }
}
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
impl PrimPitches {
    /// Create a new [PrimPitches]
    pub fn new(dir: Dir, num: Int) -> Self {
        Self { dir, num }
    }
    /// Create a [PrimPitches] in the `x` direction
    pub fn x(num: Int) -> Self {
        Self::new(Dir::Horiz, num)
    }
    /// Create a [PrimPitches] in the `y` direction
    pub fn y(num: Int) -> Self {
        Self::new(Dir::Vert, num)
    }
    /// Create a new [PrimPitches] with opposite sign of `self.num`
    pub fn negate(&self) -> Self {
        Self::new(self.dir, -self.num)
    }
}
impl HasUnits for PrimPitches {
    /// Every so often we need the raw number, fine. Use sparingly.
    #[inline(always)]
    fn raw(&self) -> Int {
        self.num
    }
}
/// Numeric operations between primitive-pitch values.
/// Generally panic if operating on two [PrimPitches] with different directions.
impl std::ops::Add<PrimPitches> for PrimPitches {
    type Output = PrimPitches;
    fn add(self, rhs: Self) -> Self::Output {
        if self.dir != rhs.dir {
            panic!(
                "Invalid attempt to add opposite-direction {:?} and {:?}",
                self, rhs
            );
        }
        Self {
            dir: self.dir,
            num: self.num + rhs.num,
        }
    }
}
impl std::ops::AddAssign<PrimPitches> for PrimPitches {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}
impl std::ops::Sub<PrimPitches> for PrimPitches {
    type Output = PrimPitches;
    fn sub(self, rhs: Self) -> Self::Output {
        if self.dir != rhs.dir {
            panic!(
                "Invalid attempt to add opposite-direction {:?} and {:?}",
                self, rhs
            );
        }
        Self {
            dir: self.dir,
            num: self.num - rhs.num,
        }
    }
}
impl std::ops::SubAssign<PrimPitches> for PrimPitches {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}
/// Numeric operations between primitive-pitch values and regular numerics.
impl std::ops::Mul<Int> for PrimPitches {
    type Output = Self;
    fn mul(self, rhs: Int) -> Self::Output {
        Self::new(self.dir, self.num * rhs)
    }
}
impl std::ops::MulAssign<Int> for PrimPitches {
    fn mul_assign(&mut self, rhs: Int) {
        self.num = self.num * rhs;
    }
}
impl std::ops::Mul<usize> for PrimPitches {
    type Output = Self;
    fn mul(self, rhs: usize) -> Self::Output {
        Self::new(self.dir, self.num * Int::try_from(rhs).unwrap())
    }
}
impl std::ops::MulAssign<usize> for PrimPitches {
    fn mul_assign(&mut self, rhs: usize) {
        self.num = self.num * Int::try_from(rhs).unwrap();
    }
}

/// A Scalar Value in Layer-Pitches
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct LayerPitches {
    layer: usize,
    num: Int,
}
impl LayerPitches {
    /// Create a new [LayerPitches] on layer (index) `layer`
    pub fn new(layer: usize, num: Int) -> Self {
        Self { layer, num }
    }
    /// Consume self, returning the underlying [usize] layer-index and [Int] number.
    pub fn into_inner(self) -> (usize, Int) {
        (self.layer, self.num)
    }
}
impl HasUnits for LayerPitches {
    /// Every so often we need the raw number, fine. Use sparingly.
    #[inline(always)]
    fn raw(&self) -> Int {
        self.num
    }
}
/// Numeric operations between pitch-values and regular numerics.
impl std::ops::Mul<Int> for LayerPitches {
    type Output = Self;
    fn mul(self, rhs: Int) -> Self::Output {
        Self::new(self.layer, self.num * rhs)
    }
}
impl std::ops::MulAssign<Int> for LayerPitches {
    fn mul_assign(&mut self, rhs: Int) {
        self.num = self.num * rhs;
    }
}
impl std::ops::Mul<usize> for LayerPitches {
    type Output = Self;
    fn mul(self, rhs: usize) -> Self::Output {
        Self::new(self.layer, self.num * Int::try_from(rhs).unwrap())
    }
}
impl std::ops::MulAssign<usize> for LayerPitches {
    fn mul_assign(&mut self, rhs: usize) {
        self.num = self.num * Int::try_from(rhs).unwrap();
    }
}

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
#[derive(
    Debug,
    Clone,
    Copy,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    From,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    MulAssign,
    DivAssign,
    Sum,
)]
/// X-Y Cartesian Pair
pub struct Xy<T> {
    pub x: T,
    pub y: T,
}
impl<T> Xy<T> {
    /// Create a new [Xy].
    pub fn new(x: T, y: T) -> Xy<T> {
        Self { x, y }
    }
    /// Get the dimension in direction `dir`
    /// Also available via the [Index] trait.
    pub fn dir(&self, dir: Dir) -> &T {
        match dir {
            Dir::Horiz => &self.x,
            Dir::Vert => &self.y,
        }
    }
}
impl<T: Clone> Xy<T> {
    /// Create a new [Xy] with transposed coordinates.
    pub fn transpose(&self) -> Xy<T> {
        Self {
            y: self.x.clone(),
            x: self.y.clone(),
        }
    }
}
impl<T: HasUnits> Xy<T> {
    /// Get a non-unit-spec'ed raw integer [Xy]
    pub fn raw(&self) -> Xy<Int> {
        Xy::new(self.x.raw(), self.y.raw())
    }
}
impl<T: HasUnits> std::ops::Index<Dir> for Xy<T> {
    type Output = T;
    fn index(&self, dir: Dir) -> &Self::Output {
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
        Self::new(
            PrimPitches {
                dir: Dir::Horiz,
                num: tup.0.into(),
            },
            PrimPitches {
                dir: Dir::Vert,
                num: tup.1.into(),
            },
        )
    }
}
