// Std-lib imports
use std::fmt::Debug;

// Crates.io
use serde::{Deserialize, Serialize};
use slotmap::{new_key_type, SlotMap};

// Local imports
use crate::raw::LayoutResult;
use crate::{cell, raw, Ptr};

#[derive(Debug, Clone, Default)]
pub struct Cells(Vec<Ptr<cell::CellBag>>);
impl Cells {
    /// Insert a [CellBag], returning a [Ptr] to it
    pub fn insert(&mut self, cell: cell::CellBag) -> Ptr<cell::CellBag> {
        let ptr = Ptr::new(cell);
        let rv = Ptr::clone(&ptr);
        self.0.push(ptr);
        rv
    }
    /// Get an immutable reference to our underlying [CellBag]-vector
    pub fn as_slice(&self) -> &[Ptr<cell::CellBag>] {
        self.0.as_slice()
    }
}
/// # Layout Library
///
/// A combination of cell definitions, sub-libraries, and metadata
///
#[derive(Debug, Clone, Default)]
pub struct Library {
    /// Library Name
    pub name: String,
    /// Cell Definitions
    pub cells: Cells,
}
impl Library {
    /// Create a new and initially empty [Library]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }
}
