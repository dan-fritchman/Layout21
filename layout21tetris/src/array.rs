//!
//! # Layout Arrays
//!
//! Uniformly-spaced repetitions of [Arrayable] elements.
//!

// Local imports
use crate::{
    bbox::{BoundBox, HasBoundBox},
    cell::Cell,
    coords::{PrimPitches, Xy},
    group::Group,
    placement::{Place, Separation},
    raw::{LayoutError, LayoutResult},
    utils::Ptr,
};

/// Uniform-Spaced Array of Identical [Placeable] Elements
#[derive(Debug, Clone)]
pub struct Array {
    /// Array Name
    pub name: String,
    /// Unit to be Arrayed
    pub unit: Arrayable,
    /// Number of elements
    pub count: usize,
    /// Separation between elements
    /// FIXME: whether to include the size of the element or not
    pub sep: Separation,
}
impl Array {
    /// Size of the Array's rectangular `boundbox`, i.e. the zero-origin `boundbox` of its `cell`.
    pub fn boundbox_size(&self) -> LayoutResult<Xy<PrimPitches>> {
        let _unit = self.unit.boundbox_size()?;
        todo!() // FIXME: do some math on separation, size
    }
}
/// Enumeration of types that can be Arrayed
#[derive(Debug, Clone)]
pub enum Arrayable {
    /// Instance of a Cell
    Instance(Ptr<Cell>),
    /// Uniform array of placeable elements
    Array(Ptr<Array>),
    /// Group of other placeable elements
    Group(Ptr<Group>),
}
impl Arrayable {
    pub fn boundbox_size(&self) -> LayoutResult<Xy<PrimPitches>> {
        match self {
            Arrayable::Instance(ref p) => p.read()?.boundbox_size(),
            Arrayable::Array(ref p) => p.read()?.boundbox_size(),
            Arrayable::Group(ref p) => p.read()?.boundbox_size(),
        }
    }
}

/// Located Instance of an Array
#[derive(Debug, Clone)]
pub struct ArrayInstance {
    /// Array-Instance Name
    pub name: String,
    /// Array Definition
    pub array: Ptr<Array>,
    /// Location of first element
    pub loc: Place<Xy<PrimPitches>>,
    /// Vertical reflection
    pub reflect_vert: bool,
    /// Horizontal reflection
    pub reflect_horiz: bool,
}

impl HasBoundBox for ArrayInstance {
    type Units = PrimPitches;
    type Error = LayoutError;
    /// Retrieve this Instance's bounding rectangle, specified in [PrimPitches].
    /// Instance location must be resolved to absolute coordinates, or this method will fail.
    fn boundbox(&self) -> LayoutResult<BoundBox<PrimPitches>> {
        // FIXME: share most or all of this with [Instance]

        let loc = self.loc.abs()?;
        let array = self.array.read()?;
        let outline = array.boundbox_size()?;
        let (x0, x1) = match self.reflect_horiz {
            false => (loc.x, loc.x + outline.x),
            true => (loc.x - outline.x, loc.x),
        };
        let (y0, y1) = match self.reflect_vert {
            false => (loc.y, loc.y + outline.y),
            true => (loc.y - outline.y, loc.y),
        };
        Ok(BoundBox::new(Xy::new(x0, y0), Xy::new(x1, y1)))
    }
}
