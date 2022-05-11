//!
//! # Layout21 Placement Module
//!

// Local imports
use crate::{
    array::ArrayInstance,
    cell::Cell,
    coords::{HasUnits, Int, PrimPitches, UnitSpeced, Xy},
    group::GroupInstance,
    instance::Instance,
    raw::{Dir, LayoutError, LayoutResult},
    utils::Ptr,
};

/// # Placement Enumeration
///
/// Includes absolute and relative placements.
///
/// Absolute placements are in `Self::AbsType` units.
/// Relative placements use the [RelativePlace] struct,
/// which can be specified relative to any other [Placeable] object.
///
#[derive(Debug, Clone)]
pub enum Place<AbsType> {
    /// Absolute
    Abs(AbsType),
    /// Relative
    Rel(RelativePlace),
}
impl<T> Place<T> {
    /// Assert that our place is absolute, and retrieve a shared reference to the inner [Xy] value.
    pub fn abs(&self) -> LayoutResult<&T> {
        match self {
            Place::Abs(ref xy) => Ok(xy),
            Place::Rel(_) => {
                LayoutError::fail("Asserted absolute-placement on a relative-placement")
            }
        }
    }
    /// Assert that our place is absolute, and retrieve a mutable reference to the inner [Xy] value.
    pub fn abs_mut(&mut self) -> LayoutResult<&mut T> {
        match self {
            Place::Abs(ref mut xy) => Ok(xy),
            Place::Rel(_) => {
                LayoutError::fail("Asserted absolute-placement on a relative-placement")
            }
        }
    }
}
impl<T: HasUnits> From<Xy<T>> for Place<Xy<T>> {
    /// Convert [Xy] values into [Place::Abs] absolute places
    fn from(xy: Xy<T>) -> Self {
        Self::Abs(xy)
    }
}
impl<T: HasUnits> From<(T, T)> for Place<Xy<T>> {
    /// Two-tuples of unit-specified numerics are converted to an [Xy] value.
    fn from((x, y): (T, T)) -> Self {
        Self::Abs(Xy::new(x, y))
    }
}
impl From<(Int, Int)> for Place<Xy<PrimPitches>> {
    /// Two-tuples of integers are converted to an [Xy] value.
    fn from(tup: (Int, Int)) -> Self {
        Self::Abs(Xy::from(tup))
    }
}
impl<T> From<RelativePlace> for Place<T> {
    fn from(rel: RelativePlace) -> Self {
        Self::Rel(rel)
    }
}

/// # Relatively-Placed Assignment
/// FIXME: merge back in with absoutely-placed [Assign]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelAssign {
    pub net: String,
    pub loc: RelativePlace,
}
/// # Relative Placement
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelativePlace {
    /// Placement is relative `to` this
    pub to: Placeable,
    /// Placed on this `side` of `to`
    pub side: Side,
    /// Aligned to this aspect of `to`
    pub align: Align,
    /// Separation between the placement and the `to`
    pub sep: Separation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    SizeOf(Ptr<Cell>),
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
    pub fn z(z: isize) -> Self {
        Self {
            z: Some(z),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Placeable {
    /// Instance of another cell
    Instance(Ptr<Instance>),
    /// Uniform array of placeable elements
    Array(Ptr<ArrayInstance>),
    /// Group of other placeable elements
    Group(Ptr<GroupInstance>),
    /// Instance port location
    Port { inst: Ptr<Instance>, port: String },
    /// Assignment
    Assign(Ptr<RelAssign>),
}
impl Placeable {
    /// Get the location of the placeable
    pub fn loc(&self) -> LayoutResult<Place<Xy<PrimPitches>>> {
        let loc = match self {
            Placeable::Instance(ref p) => {
                let p = p.read()?;
                p.loc.clone()
            }
            Placeable::Array(ref p) => {
                let p = p.read()?;
                p.loc.clone()
            }
            Placeable::Group(ref p) => {
                let p = p.read()?;
                p.loc.clone()
            }
            Placeable::Port { .. } => unimplemented!(),
            Placeable::Assign(_) => unimplemented!(),
        };
        Ok(loc)
    }
}
