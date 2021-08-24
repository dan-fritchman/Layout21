// Std-lib imports
use std::fmt::Debug;

// Crates.io
use serde::{Deserialize, Serialize};
use slotmap::SlotMap;

// Local imports
use crate::cell::{CellBag, CellBagKey};

/// # Layout Library
///
/// A combination of cell definitions, sub-libraries, and metadata
///
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Library {
    /// Library Name
    pub name: String,
    /// Cell Definitions
    pub cells: SlotMap<CellBagKey, CellBag>,
}
impl Library {
    /// Create a new and initially empty [Library]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            cells: SlotMap::with_key(),
        }
    }
}
