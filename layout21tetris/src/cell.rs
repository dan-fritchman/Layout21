//!
//! # Cell Definition
//!
//! Defines the [Cell] type, which represents a multi-viewed piece of reusable hardware.
//! [Cell]s can, and generally do, have one or more associated "views",
//! including [LayoutAbstract]s, [LayoutImpl], interface definitions, and/or "raw" layouts.
//!

// Crates.io
use enum_dispatch::enum_dispatch;

// Local imports
use crate::raw::{LayoutError, LayoutResult};
use crate::stack::{Assign, RelZ, TrackIntersection};
use crate::utils::Ptr;
use crate::{abstrakt, coords, interface, outline, raw};

/// # Layout Cell Implementation
///
/// A combination of lower-level cell instances and net-assignments to tracks.
///
#[derive(Debug, Clone)]
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
/// "Pointer" to a raw (lib, cell) combination
#[derive(Debug, Clone)]
pub struct RawLayoutPtr {
    pub lib: Ptr<raw::Library>,
    pub cell: raw::CellKey,
}
/// # Cell View Enumeration
/// All of the ways in which a Cell is represented
#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum CellView {
    Interface(interface::Bundle),
    LayoutAbstract(abstrakt::LayoutAbstract),
    LayoutImpl(LayoutImpl),
    RawLayoutPtr(RawLayoutPtr),
}
/// Empty trait, largely for auto-generation of [From] and [Into] implementations.
#[enum_dispatch(CellView)]
trait CellViewable {}

/// Collection of the Views describing a Cell
#[derive(Debug, Default, Clone)]
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
    pub raw: Option<RawLayoutPtr>,
}
impl CellBag {
    /// Add [CellView] `view` to our appropriate type-based field.
    /// Over-writes are
    pub fn add_view(&mut self, view: impl Into<CellView>) {
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
            CellView::RawLayoutPtr(x) => {
                self.raw.replace(x);
            }
        }
    }
    /// Create from a list of [CellView]s and a name.
    pub fn from_views(name: impl Into<String>, views: Vec<CellView>) -> Self {
        // Initialize a default, empty [CellBag]
        let mut me = Self::default();
        me.name = name.into();
        for view in views {
            me.add_view(view);
        }
        me
    }
    /// Return whichever view highest-prioritorily dictates the outline
    pub fn outline(&self) -> LayoutResult<&outline::Outline> {
        // We take the "most abstract" view for the outline
        // (although if there are more than one, they better be the same...
        // FIXME: this should be a validation step.)
        // Overall this method probably should move to a "validated" cell in which each view is assured consistent.
        if let Some(ref x) = self.abstrakt {
            Ok(&x.outline)
        } else if let Some(ref x) = self.layout {
            Ok(&x.outline)
        } else {
            Err(LayoutError::Tbd)
        }
    }
    /// Return whichever view highest-prioritorily dictates the top-layer
    pub fn top_layer(&self) -> LayoutResult<usize> {
        // FIXME: same commentary as `outline` above
        if let Some(ref x) = self.abstrakt {
            Ok(x.top_layer)
        } else if let Some(ref x) = self.layout {
            Ok(x.top_layer)
        } else {
            Err(LayoutError::Tbd)
        }
    }
}
impl From<CellView> for CellBag {
    fn from(src: CellView) -> Self {
        match src {
            CellView::Interface(x) => x.into(),
            CellView::LayoutAbstract(x) => x.into(),
            CellView::LayoutImpl(x) => x.into(),
            CellView::RawLayoutPtr(x) => x.into(),
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
impl From<RawLayoutPtr> for CellBag {
    fn from(src: RawLayoutPtr) -> Self {
        Self {
            name: "".into(), // FIXME!
            raw: Some(src),
            ..Default::default()
        }
    }
}

/// Instance of another Cell
#[derive(Debug, Clone)]
pub struct Instance {
    /// Instance Name
    pub inst_name: String,
    /// Cell Definition Reference
    pub cell: Ptr<CellBag>,
    /// Location, in primitive pitches
    pub loc: coords::Xy<coords::PrimPitches>,
    /// Reflection
    pub reflect: bool,
    /// Angle of Rotation (Degrees)
    pub angle: Option<f64>,
}
