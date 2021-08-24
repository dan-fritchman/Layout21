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
use crate::stack::{Assign, RelZ, TrackIntersection};
use crate::{abstrakt, coords, interface, outline, raw};

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
impl LayoutImpl {
    /// Create a new [LayoutImpl]
    pub fn new(name: impl Into<String>, top_layer: usize, outline: outline::Outline) -> LayoutImpl {
        let name = name.into();
        LayoutImpl {
            name,
            top_layer,
            outline,
            instances: Vec::new(),
            assignments: Vec::new(),
            cuts: Vec::new(),
        }
    }
    /// Assign a net at the given coordinates.
    pub fn assign(
        &mut self,
        net: impl Into<String>,
        layer: usize,
        track: usize,
        at: usize,
        relz: RelZ,
    ) {
        let net = net.into();
        self.assignments.push(Assign {
            net,
            at: TrackIntersection {
                layer,
                track,
                at,
                relz,
            },
        })
    }
    /// Add a cut at the specified coordinates.
    pub fn cut(&mut self, layer: usize, track: usize, at: usize, relz: RelZ) {
        self.cuts.push(TrackIntersection {
            layer,
            track,
            at,
            relz,
        })
    }
    /// Get a temporary handle for net assignments
    pub fn net<'h>(&'h mut self, net: impl Into<String>) -> NetHandle<'h> {
        let name = net.into();
        NetHandle { name, parent: self }
    }
}
/// A short-term handle for chaining multiple assignments to a net
/// Typically used as: `mycell.net("name").at(/* args */).at(/* more args */)`
/// Takes an exclusive reference to its parent [LayoutImpl],
/// so generally must be dropped quickly to avoid locking it up.
pub struct NetHandle<'h> {
    name: String,
    parent: &'h mut LayoutImpl,
}
impl<'h> NetHandle<'h> {
    /// Assign our net at the given coordinates.
    /// Consumes and returns `self` to enable chaining.
    pub fn at(self, layer: usize, track: usize, at: usize, relz: RelZ) -> Self {
        self.parent.assign(&self.name, layer, track, at, relz);
        self
    }
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
    // Raw Layout
    pub raw: Option<raw::Cell>,
}
impl CellBag {
    /// Add [CellView] `view` to our appropriate type-based field.
    /// Over-writes are
    fn add_view(&mut self, view: impl Into<CellView>) {
        let view = view.into();
        match view {
            CellView::Interface(x) => {
                self.interface.replace(x);
            }
            CellView::LayoutAbstract(x) => {
                self.abstrakt.replace(x);
            }
            CellView::LayoutImpl(x) => {
                self.layout.replace(x);
            }
            CellView::RawLayoutImpl(x) => {
                self.raw.replace(x);
            }
        }
    }
    /// Create from a list of [CellView]s and a name.
    fn from_views(name: impl Into<String>, views: Vec<CellView>) -> Self {
        // Initialize a default, empty [CellBag]
        let mut me = Self::default();
        me.name = name.into();
        for view in views {
            me.add_view(view);
        }
        me
    }
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
