//!
//! # Cell Definition
//!
//! Defines the [Cell] type, which represents a multi-viewed piece of reusable hardware.
//! [Cell]s can, and generally do, have one or more associated "views",
//! including [Abstract]s, [Layout], interface definitions, and/or "raw" layouts.
//!

// Crates.io
use derive_more;

// Local imports
use crate::coords::{PrimPitches, Xy};
use crate::layout::Layout;
use crate::raw::{LayoutError, LayoutResult};
use crate::utils::Ptr;
use crate::{abs, interface, outline, raw};

/// "Pointer" to a raw (lib, cell) combination.
/// Wraps with basic [Outline] and `metals` information to enable bounded placement.
#[derive(Debug, Clone)]
pub struct RawLayoutPtr {
    /// Outline shape, counted in x and y pitches of `stack`
    pub outline: outline::Outline,
    /// Number of Metal Layers Used
    pub metals: usize,
    /// Pointer to the raw Library
    pub lib: Ptr<raw::Library>,
    /// Pointer to the raw Cell
    pub cell: Ptr<raw::Cell>,
}
/// # Cell View Enumeration
/// All of the ways in which a Cell is represented
#[derive(derive_more::From, Debug, Clone)]
pub enum CellView {
    Interface(interface::Bundle),
    Abstract(abs::Abstract),
    Layout(Layout),
    RawLayoutPtr(RawLayoutPtr),
}

/// Collection of the Views describing a Cell
#[derive(Debug, Default, Clone)]
pub struct Cell {
    /// Cell Name
    pub name: String,
    /// Interface
    pub interface: Option<interface::Bundle>,
    /// Layout Abstract
    pub abs: Option<abs::Abstract>,
    /// Layout Implementation
    pub layout: Option<Layout>,
    /// Raw Layout
    /// FIXME: this should probably move "up" a level,
    /// so that cells are either defined as `raw` or `tetris` implementations,
    /// but not both
    pub raw: Option<RawLayoutPtr>,
}
impl Cell {
    /// Create a new and initially empty [Cell]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }
    /// Add [CellView] `view` to our appropriate type-based field.
    pub fn add_view(&mut self, view: impl Into<CellView>) {
        let view = view.into();
        match view {
            CellView::Interface(x) => {
                self.interface.replace(x);
            }
            CellView::Abstract(x) => {
                self.abs.replace(x);
            }
            CellView::Layout(x) => {
                self.layout.replace(x);
            }
            CellView::RawLayoutPtr(x) => {
                self.raw.replace(x);
            }
        }
    }
    /// Create from a list of [CellView]s and a name.
    pub fn from_views(name: impl Into<String>, views: Vec<CellView>) -> Self {
        let mut myself = Self::default();
        myself.name = name.into();
        for view in views {
            myself.add_view(view);
        }
        myself
    }
    /// Return whichever view highest-prioritorily dictates the outline
    pub fn outline(&self) -> LayoutResult<&outline::Outline> {
        // We take the "most abstract" view for the outline
        // (although if there are more than one, they better be the same...
        // FIXME: this should be a validation step.)
        // Overall this method probably should move to a "validated" cell in which each view is assured consistent.
        if let Some(ref x) = self.abs {
            Ok(&x.outline)
        } else if let Some(ref x) = self.layout {
            Ok(&x.outline)
        } else if let Some(ref x) = self.raw {
            Ok(&x.outline)
        } else {
            LayoutError::fail(format!(
                "Failed to retrieve outline of cell {} with no abstract or implementation",
                self.name,
            ))
        }
    }
    /// Size of the [Cell]'s rectangular `boundbox`.
    pub fn boundbox_size(&self) -> LayoutResult<Xy<PrimPitches>> {
        let outline = self.outline()?;
        Ok(Xy::new(outline.xmax(), outline.ymax()))
    }
    /// Return whichever view highest-prioritorily dictates the top-layer
    pub fn metals(&self) -> LayoutResult<usize> {
        // FIXME: same commentary as `outline` above
        if let Some(ref x) = self.abs {
            Ok(x.metals)
        } else if let Some(ref x) = self.layout {
            Ok(x.metals)
        } else if let Some(ref x) = self.raw {
            Ok(x.metals)
        } else {
            LayoutError::fail(format!(
                "Failed to retrieve metal-layers of cell {} with no abstract or implementation",
                self.name,
            ))
        }
    }
    /// Get the cell's top metal layer (numer).
    /// Returns `None` if no metal layers are used.
    pub fn top_metal(&self) -> LayoutResult<Option<usize>> {
        let metals = self.metals()?;
        if metals == 0 {
            Ok(None)
        } else {
            Ok(Some(metals - 1))
        }
    }
}
impl From<CellView> for Cell {
    fn from(src: CellView) -> Self {
        match src {
            CellView::Interface(x) => x.into(),
            CellView::Abstract(x) => x.into(),
            CellView::Layout(x) => x.into(),
            CellView::RawLayoutPtr(x) => x.into(),
        }
    }
}
impl From<interface::Bundle> for Cell {
    fn from(src: interface::Bundle) -> Self {
        Self {
            name: src.name.clone(),
            interface: Some(src),
            ..Default::default()
        }
    }
}
impl From<abs::Abstract> for Cell {
    fn from(src: abs::Abstract) -> Self {
        Self {
            name: src.name.clone(),
            abs: Some(src),
            ..Default::default()
        }
    }
}
impl From<Layout> for Cell {
    fn from(src: Layout) -> Self {
        Self {
            name: src.name.clone(),
            layout: Some(src),
            ..Default::default()
        }
    }
}
impl From<RawLayoutPtr> for Cell {
    fn from(src: RawLayoutPtr) -> Self {
        let name = {
            let cell = src.cell.read().unwrap();
            cell.name.clone()
        };
        Self {
            name,
            raw: Some(src),
            ..Default::default()
        }
    }
}
