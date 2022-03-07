//!
//! # Layout Element Groups
//!
//! The primary [Group] type is a set of named, located elements
//! which can be placed and moved together.
//!

// Local imports
use crate::{
    array::Array,
    cell::Cell,
    coords::{PrimPitches, Xy},
    placement::Place,
    raw::LayoutResult,
    utils::Ptr,
};

/// Named group of placeable elements
#[derive(Debug, Clone)]
pub struct Group {
    /// Group Name
    name: String,
    /// Constituent Elements
    elements: Vec<Groupable>,
}
impl Group {
    /// Size of the Instance's rectangular `boundbox`, i.e. the zero-origin `boundbox` of its `cell`.
    pub fn boundbox_size(&self) -> LayoutResult<Xy<PrimPitches>> {
        todo!()
    }
}
/// Enumeration of types that can be Grouped
#[derive(Debug, Clone)]
pub enum Groupable {
    /// Instance of a Cell
    Instance(Ptr<Cell>),
    /// Uniform array of placeable elements
    Array(Ptr<Array>),
    /// Group of other placeable elements
    Group(Ptr<Group>),
}
/// Placed Instance of a [Group]
#[derive(Debug, Clone)]
pub struct GroupInstance {
    /// Group-Instance Name
    pub name: String,
    /// Group Definition
    pub group: Ptr<Group>,
    /// Location
    pub loc: Place<Xy<PrimPitches>>,
}
