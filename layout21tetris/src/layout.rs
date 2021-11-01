//!
//! # Layout-Cell Definitions
//!
//! Physical implementations of tetris [Cell]s.
//!

// Local imports
use crate::{
    instance::Instance,
    outline,
    placement::Placeable,
    stack::{Assign, RelZ},
    tracks::TrackCross,
    utils::PtrList,
};

/// # Layout Cell Implementation
///
/// A combination of lower-level cell instances and net-assignments to tracks.
///
#[derive(Debug, Clone, Builder)]
#[builder(pattern = "owned", setter(into))]
pub struct Layout {
    /// Cell Name
    pub name: String,
    /// Number of Metal Layers Used
    pub metals: usize,
    /// Outline shape, counted in x and y pitches of `stack`
    pub outline: outline::Outline,

    /// Layout Instances
    #[builder(default)]
    pub instances: PtrList<Instance>,
    /// Net-to-track assignments
    #[builder(default)]
    pub assignments: Vec<Assign>,
    /// Track cuts
    #[builder(default)]
    pub cuts: Vec<TrackCross>,
    /// Placeable objects
    #[builder(default)]
    pub places: Vec<Placeable>,
}
impl Layout {
    /// Create a new [Layout]
    pub fn new(name: impl Into<String>, metals: usize, outline: outline::Outline) -> Self {
        let name = name.into();
        Layout {
            name,
            metals,
            outline,
            instances: PtrList::new(),
            assignments: Vec::new(),
            cuts: Vec::new(),
            places: Vec::new(),
        }
    }
    /// Create a [LayoutBuilder], a struct created by the [Builder] macro.
    pub fn builder() -> LayoutBuilder {
        LayoutBuilder::default()
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
        let at = TrackCross::from_relz(layer, track, at, relz);
        self.assignments.push(Assign { net, at })
    }
    /// Add a cut at the specified coordinates.
    pub fn cut(&mut self, layer: usize, track: usize, at: usize, relz: RelZ) {
        let cut = TrackCross::from_relz(layer, track, at, relz);
        self.cuts.push(cut)
    }
    /// Get a temporary handle for net assignments
    pub fn net<'h>(&'h mut self, net: impl Into<String>) -> NetHandle<'h> {
        let name = net.into();
        NetHandle { name, parent: self }
    }
}
/// # Net Handle
///
/// A short-term handle for chaining multiple assignments to a net
/// Typically used as: `mycell.net("name").at(/* args */).at(/* more args */)`
/// Takes an exclusive reference to its parent [Layout],
/// so generally must be dropped quickly to avoid locking it up.
///
pub struct NetHandle<'h> {
    name: String,
    parent: &'h mut Layout,
}
impl<'h> NetHandle<'h> {
    /// Assign our net at the given coordinates.
    /// Consumes and returns `self` to enable chaining.
    pub fn at(self, layer: usize, track: usize, at: usize, relz: RelZ) -> Self {
        self.parent.assign(&self.name, layer, track, at, relz);
        self
    }
}
