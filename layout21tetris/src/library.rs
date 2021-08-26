// Std-lib imports
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};

// Local imports
use crate::raw::LayoutResult;
use crate::utils::Ptr;
use crate::{cell, raw, rawconv, stack};

/// FIXME: rename
/// # Cell List
///
/// Wraps a <Vec<CellBag>>, providing an interface designed
/// for ease of getting references ([Ptr]) upon insertion.
///
#[derive(Debug, Clone, Default)]
pub struct Cells<T: Clone>(Vec<Ptr<T>>);
impl<T: Clone> Cells<T> {
    /// Insert a [CellBag], returning a [Ptr] to it
    pub fn insert(&mut self, val: T) -> Ptr<T> {
        let ptr = Ptr::new(val);
        let rv = Ptr::clone(&ptr);
        self.0.push(ptr);
        rv
    }
    /// Get an immutable reference to our underlying [CellBag]-vector
    pub fn as_slice(&self) -> &[Ptr<T>] {
        self.0.as_slice()
    }
}
impl<T: Clone> Deref for Cells<T> {
    type Target = Vec<Ptr<T>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T: Clone> DerefMut for Cells<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
    pub cells: Cells<cell::CellBag>,
    /// [raw::Library] Definitions
    pub rawlibs: Cells<raw::Library>,
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
