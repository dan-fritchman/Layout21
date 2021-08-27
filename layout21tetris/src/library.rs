//!
//! # Layout Library Module 
//! 

// Local imports
use crate::raw::LayoutResult;
use crate::utils::{Ptr, PtrList};
use crate::{cell, raw, rawconv, stack};

/// # Layout Library
///
/// A combination of cell definitions, sub-libraries, and metadata
///
#[derive(Debug, Clone, Default)]
pub struct Library {
    /// Library Name
    pub name: String,
    /// Cell Definitions
    pub cells: PtrList<cell::CellBag>,
    /// [raw::Library] Definitions
    pub rawlibs: PtrList<raw::Library>,
}
impl Library {
    /// Create a new and initially empty [Library]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }
    /// Export to a [raw::Library]
    pub fn to_raw(self, stack: stack::Stack) -> LayoutResult<Ptr<raw::Library>> {
        rawconv::RawExporter::convert(self, stack)
    }
    pub fn add_cell(&mut self, cell: cell::CellBag) -> Ptr<cell::CellBag> {
        self.cells.insert(cell)
    }
    pub fn add_rawlib(&mut self, rawlib: raw::Library) -> Ptr<raw::Library> {
        self.rawlibs.insert(rawlib)
    }
}
