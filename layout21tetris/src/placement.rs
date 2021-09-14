//!
//! # Layout21 Placement Module
//!

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::cell::Instance;
use crate::coords::{HasUnits, Int, UnitSpeced, Xy};
use crate::raw::{LayoutError, LayoutResult};
use crate::stack::{Assign, RelZ, TrackIntersection};
use crate::utils::Ptr;
use crate::{abstrakt, coords, interface, outline, raw};

/// # Placement Enumeration
///
/// Includes absolute and relative placements.
///
/// Absolute placements are in `Self::AbsType` units,
/// which must implement the [HasUnits] trait.
/// Generally this includes the variants of the [UnitSpeced] enum.
///
/// Relative placements use the [RelativePlace] struct,
/// which can be specified relative to any other placement.
///
#[derive(Debug, Clone)]
pub enum Place<AbsType: HasUnits> {
    /// Absolute
    Abs(Xy<AbsType>),
    /// Relative
    Rel(RelativePlace),
}
impl<T: HasUnits> Place<T> {
    /// Assert that self is [Self::Abs], and retrieve a shared reference to the inner [Xy] value.
    pub fn abs(&self) -> LayoutResult<&Xy<T>> {
        match self {
            Place::Abs(ref xy) => Ok(xy),
            Place::Rel(_) => Err(LayoutError::Tbd),
        }
    }
}
impl<T: HasUnits> From<Xy<T>> for Place<T> {
    fn from(xy: Xy<T>) -> Self {
        Self::Abs(xy)
    }
}
impl<T: HasUnits> From<RelativePlace> for Place<T> {
    fn from(rel: RelativePlace) -> Self {
        Self::Rel(rel)
    }
}



#[derive(Debug, Clone)]
pub struct RelativePlace {
    /// Placement is relative `to` this
    pub to: Placeable,

    /// Spacing between the placement and the `to`
    pub side: Side,
    pub align: Side,
    // pub by: RelativeBy,
}

#[derive(Debug, Clone, Copy)]
pub enum Side {
    Top,
    Bottom,
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub struct AbsRelativeBy {
    x: Option<UnitSpeced>,
    y: Option<UnitSpeced>,
    z: Option<usize>,
}
// pub trait Sizable: std::fmt::Debug + std::clone::Clone {} // FIXME!
#[derive(Debug, Clone)]
pub enum RelativeBy {
    // alignment from this instance's pin `x` to another instance's pin `y`
    Abs(AbsRelativeBy),
    // SizeOf(Ptr<dyn Sizable>),
}
#[derive(Debug, Clone)]
pub enum Placeable {
    /// Instance of another cell
    Instance(Ptr<Instance>),
    /// Instance port location
    PortRef(PortRef),
    /// Intersection between two tracks
    TrackIntersection(TrackIntersection),
    /// Uniform array of placeable elements
    Array(Ptr<Array>),
    /// Group of other placeable elements
    Group(Ptr<Group>),
}

#[derive(Debug, Clone)]
pub struct Group {
    name: Option<String>,
    things: Vec<Placeable>,
}

/// Uniform-Spaced Array of Identical [Placeable] Elements
#[derive(Debug, Clone)]
pub struct Array {
    name: Option<String>,
    unit: Placeable,
    loc: UnitSpeced,
    count: usize,
    by: RelativeBy,
}

/// FIXME!
#[derive(Debug, Clone)]
pub struct PortRef;

