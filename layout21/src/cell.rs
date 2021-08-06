//! # Cell Definition
//!
//! Defines the [Cell] type, which represents a multi-viewed piece of reusable hardware.
//! [Cell]s can, and generally do, have one or more associated "views",
//! including [LayoutAbstract]s, [LayoutImpl], interface definitions, and/or "raw" layouts.
//!

// Crates.io
use enum_dispatch::enum_dispatch;
use serde::{Deserialize, Serialize};
use slotmap::new_key_type;

// Local imports
use crate::{abstrakt, coords, interface, outline, raw};
use crate::{Assign, TrackIntersection};

// Create a slotmap key-type for [CellBag]s
new_key_type! {
    /// Keys for [CellBag] entries
    pub struct CellBagKey;
}

/// # Layout Cell Implementation
///
/// A combination of lower-level cell instances and net-assignments to tracks.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LayoutImpl {
    /// Cell Name
    pub name: String,
    /// Top-layer index
    pub top_layer: usize,
    /// Outline shape, counted in x and y pitches of `stack`
    pub outline: outline::Outline,
    /// Layout Instances
    pub instances: Vec<Instance>,
    /// Net-to-track assignments
    pub assignments: Vec<Assign>,
    /// Track cuts
    pub cuts: Vec<TrackIntersection>,
}
/// # Cell View Enumeration
/// All of the ways in which a Cell is represented
#[enum_dispatch]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CellView {
    Interface(interface::Bundle),
    LayoutAbstract(abstrakt::LayoutAbstract),
    LayoutImpl(LayoutImpl),
    RawLayoutImpl(raw::Cell),
}
/// Empty trait, largely for auto-generation of [From] and [Into] implementations.
#[enum_dispatch(CellView)]
trait CellViewable {}

/// Collection of the Views describing a Cell
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct CellBag {
    // Cell Name
    pub name: String,
    // Interface
    pub interface: Option<interface::Bundle>,
    // Layout Abstract
    pub abstrakt: Option<abstrakt::LayoutAbstract>,
    // Layout Implementation
    pub layout: Option<LayoutImpl>,
    // Raw Layout Implementation
    pub raw: Option<raw::Cell>,
}
impl From<CellView> for CellBag {
    fn from(src: CellView) -> Self {
        match src {
            CellView::Interface(x) => x.into(),
            CellView::LayoutAbstract(x) => x.into(),
            CellView::LayoutImpl(x) => x.into(),
            CellView::RawLayoutImpl(x) => x.into(),
        }
    }
}
impl From<interface::Bundle> for CellBag {
    fn from(src: interface::Bundle) -> Self {
        Self {
            name: src.name.clone(),
            interface: Some(src),
            ..Default::default()
        }
    }
}
impl From<abstrakt::LayoutAbstract> for CellBag {
    fn from(src: abstrakt::LayoutAbstract) -> Self {
        Self {
            name: src.name.clone(),
            abstrakt: Some(src),
            ..Default::default()
        }
    }
}
impl From<LayoutImpl> for CellBag {
    fn from(src: LayoutImpl) -> Self {
        Self {
            name: src.name.clone(),
            layout: Some(src),
            ..Default::default()
        }
    }
}
impl From<raw::Cell> for CellBag {
    fn from(src: raw::Cell) -> Self {
        Self {
            name: src.name.clone(),
            raw: Some(src),
            ..Default::default()
        }
    }
}

/// Instance of another Cell
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instance {
    /// Instance Name
    pub inst_name: String,
    /// Cell Definition Reference
    pub cell: CellBagKey,
    /// Location, in primitive pitches
    pub loc: coords::Xy<coords::PrimPitches>,
    /// Reflection
    pub reflect: bool,
    /// Angle of Rotation (Degrees)
    pub angle: Option<f64>,
}
