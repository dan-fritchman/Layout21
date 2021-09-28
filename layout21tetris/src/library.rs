//!
//! # Layout Library Module
//!

// Std-lib
use std::collections::HashSet;

// Local imports
use crate::raw::LayoutResult;
use crate::utils::{Ptr, PtrList};
use crate::{cell, raw, rawconv, stack, validate};

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
    pub fn to_raw(self, stack: validate::ValidStack) -> LayoutResult<Ptr<raw::Library>> {
        rawconv::RawExporter::convert(self, stack)
    }
    /// Add a [CellBag]
    pub fn add_cell(&mut self, cell: cell::CellBag) -> Ptr<cell::CellBag> {
        self.cells.insert(cell)
    }
    /// Add a [raw::Library]
    pub fn add_rawlib(&mut self, rawlib: raw::Library) -> Ptr<raw::Library> {
        self.rawlibs.insert(rawlib)
    }
    /// Create an ordered list in which dependent cells follow their dependencies.
    pub fn dep_order(&self) -> Vec<Ptr<cell::CellBag>> {
        DepOrder::order(self)
    }
}

/// # Dependency-Orderer
///
/// Creates an ordered list in which dependent cells follow their dependencies.
/// Ideally an iterator, but really just a struct that creates an in-order [Vec] at creation time.
///
#[derive(Debug)]
pub struct DepOrder<'lib> {
    lib: &'lib Library,
    stack: Vec<Ptr<cell::CellBag>>,
    seen: HashSet<Ptr<cell::CellBag>>,
}
impl<'lib> DepOrder<'lib> {
    fn order(lib: &'lib Library) -> Vec<Ptr<cell::CellBag>> {
        let mut myself = Self {
            lib,
            stack: Vec::new(),
            seen: HashSet::new(),
        };
        for cell in myself.lib.cells.iter() {
            myself.push(cell);
        }
        myself.stack
    }
    fn push(&mut self, ptr: &Ptr<cell::CellBag>) {
        // If the Cell hasn't already been visited, depth-first search it
        if !self.seen.contains(&ptr) {
            // Read the cell-pointer
            let cell = ptr.read().unwrap();
            // If the cell has an implementation, visit its [Instance]s before inserting it
            if let Some(layout) = &cell.layout {
                for ptr in layout.instances.iter() {
                    let inst = ptr.read().unwrap();
                    self.push(&inst.cell);
                }
            }
            // And insert the cell (pointer) itself
            self.seen.insert(Ptr::clone(ptr));
            self.stack.push(Ptr::clone(ptr));
        }
    }
}
