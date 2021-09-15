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
use crate::bbox::{BoundBox, HasBoundBox};
use crate::coords::{PrimPitches, Xy};
use crate::placement::{Place, Placeable};
use crate::raw::{Dir, LayoutError, LayoutResult};
use crate::stack::{Assign, RelZ, TrackIntersection};
use crate::utils::{Ptr, PtrList};
use crate::{abstrakt, interface, outline, raw};

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
    pub instances: PtrList<Instance>,
    /// Net-to-track assignments
    pub assignments: Vec<Assign>,
    /// Track cuts
    pub cuts: Vec<TrackIntersection>,
    /// Placeable objects
    pub places: PtrList<Placeable>,
}
impl LayoutImpl {
    /// Create a new [LayoutImpl]
    pub fn new(name: impl Into<String>, top_layer: usize, outline: outline::Outline) -> LayoutImpl {
        let name = name.into();
        LayoutImpl {
            name,
            top_layer,
            outline,
            instances: PtrList::new(),
            assignments: Vec::new(),
            cuts: Vec::new(),
            places: PtrList::new(),
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
    pub cell: Ptr<raw::CellBag>,
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
    /// Cell Name
    pub name: String,
    /// Interface
    pub interface: Option<interface::Bundle>,
    /// Layout Abstract
    pub abstrakt: Option<abstrakt::LayoutAbstract>,
    /// Layout Implementation
    pub layout: Option<LayoutImpl>,
    /// Raw Layout
    /// FIXME: this should probably move "up" a level,
    /// so that cells are either defined as `raw` or `tetris` implementations,
    /// but not both
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
        if let Some(ref x) = self.abstrakt {
            Ok(&x.outline)
        } else if let Some(ref x) = self.layout {
            Ok(&x.outline)
        } else {
            Err(LayoutError::Tbd)
        }
    }
    /// Size of the [Cell]'s rectangular `boundbox`.
    pub fn boundbox_size(&self) -> LayoutResult<Xy<PrimPitches>> {
        let outline = self.outline()?;
        Ok(Xy::new(outline.xmax(), outline.ymax()))
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

/// Instance of another Cell
#[derive(Debug, Clone)]
pub struct Instance {
    /// Instance Name
    pub inst_name: String,
    /// Cell Definition Reference
    pub cell: Ptr<CellBag>,
    /// Location of the Instance origin
    /// This origin-position holds regardless of either `reflect` field.
    /// If specified in absolute coordinates, location-units are [PrimPitches].
    pub loc: Place<PrimPitches>,
    /// Horizontal Reflection
    pub reflect_horiz: bool,
    /// Vertical Reflection
    pub reflect_vert: bool,
}
impl Instance {
    /// Boolean indication of whether this Instance is reflected in direction `dir`
    pub fn reflected(&self, dir: Dir) -> bool {
        match dir {
            Dir::Horiz => self.reflect_horiz,
            Dir::Vert => self.reflect_vert,
        }
    }
    /// Size of the Instance's rectangular `boundbox`, i.e. the zero-origin `boundbox` of its `cell`.
    pub fn boundbox_size(&self) -> LayoutResult<Xy<PrimPitches>> {
        let cell = self.cell.read()?;
        cell.boundbox_size()
    }
}
impl HasBoundBox for Instance {
    type Units = PrimPitches;
    type Error = LayoutError;
    /// Retrieve this Instance's bounding rectangle, specified in [PrimPitches].
    /// Instance location must be resolved to absolute coordinates, or this method will fail.
    fn boundbox(&self) -> LayoutResult<BoundBox<PrimPitches>> {
        let loc = self.loc.abs()?;
        let cell = self.cell.read()?;
        let outline = cell.outline()?;
        let (x0, x1) = match self.reflect_horiz {
            false => (loc.x, loc.x + outline.xmax()),
            true => (loc.x - outline.xmax(), loc.x),
        };
        let (y0, y1) = match self.reflect_vert {
            false => (loc.y, loc.y + outline.ymax()),
            true => (loc.y - outline.ymax(), loc.y),
        };
        Ok(BoundBox::new(Xy::new(x0, y0), Xy::new(x1, y1)))
    }
}
