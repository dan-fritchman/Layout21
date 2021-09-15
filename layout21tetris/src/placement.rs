//!
//! # Layout21 Placement Module
//!

// Local imports
use crate::cell::{CellBag, Instance};
use crate::coords::{HasUnits, UnitSpeced, Xy};
use crate::raw::{Dir, LayoutError, LayoutResult};
use crate::stack::TrackIntersection;
use crate::utils::Ptr;

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

/// # Relative Placement
#[derive(Debug, Clone)]
pub struct RelativePlace {
    /// Placement is relative `to` this
    pub to: Placeable,
    /// Placed on this `side` of `to`
    pub side: Side,
    /// Aligned to this aspect of `to`
    pub align: Side, // FIXME: move to [Align]
    /// Separation between the placement and the `to`
    pub sep: Separation,
}

#[derive(Debug, Clone, Copy)]
pub enum Side {
    Top,
    Bottom,
    Left,
    Right,
}
impl Side {
    /// Get the side rotated 90 degrees clockwise
    pub fn cw_90(&self) -> Self {
        match self {
            Self::Top => Self::Right,
            Self::Right => Self::Bottom,
            Self::Bottom => Self::Left,
            Self::Left => Self::Top,
        }
    }
    /// Get the side rotated 90 degrees counter-clockwise
    pub fn ccw_90(&self) -> Self {
        match self {
            Self::Top => Self::Left,
            Self::Left => Self::Bottom,
            Self::Bottom => Self::Right,
            Self::Right => Self::Top,
        }
    }
}
#[derive(Debug, Clone)]
pub enum Align {
    /// Side-to-side alignment
    Side(Side),
    /// Center-aligned
    Center,
    /// Port-to-port alignment
    Ports(String, String),
}

/// Enumerated means of specifying relative-placement separation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SepBy {
    /// Separated by [UnitSpeced]-distance in x and y, and by layers in z
    UnitSpeced(UnitSpeced),
    /// Separated by the size of another Cell
    SizeOf(Ptr<CellBag>),
}
/// Three-dimensional separation units
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Separation {
    pub x: Option<SepBy>,
    pub y: Option<SepBy>,
    pub z: Option<isize>,
}
impl Separation {
    pub fn new(x: Option<SepBy>, y: Option<SepBy>, z: Option<isize>) -> Self {
        Self { x, y, z }
    }
    pub fn x(x: SepBy) -> Self {
        Self {
            x: Some(x),
            ..Default::default()
        }
    }
    pub fn y(y: SepBy) -> Self {
        Self {
            y: Some(y),
            ..Default::default()
        }
    }
    /// Get the separation in direction `dir`
    pub fn dir(&self, dir: Dir) -> &Option<SepBy> {
        match dir {
            Dir::Horiz => &self.x,
            Dir::Vert => &self.y,
        }
    }
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

/// Named group of placeable elements
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
    sep: Separation,
}

/// FIXME!
#[derive(Debug, Clone)]
pub struct PortRef;
