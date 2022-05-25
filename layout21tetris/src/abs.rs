//!
//! # Abstract Layout Module
//!
//! [Abstract] layouts describe a [Cell]'s outline and physical interface, without exposing implementation details.  
//! [Cell]-[Abstract]s primarily comprise their outlines and pins.
//! Outlines follow the same "Tetris-Shapes" as `layout21::tetris` layout cells, including the requirements for a uniform z-axis.  
//! Internal layers are "fully blocked", in that parent layouts may not route through them.
//! In legacy layout systems this would be akin to including blockages of the same shape as [Outline] on each layer.
//!
//! Sadly the english-spelled name "abstract" is reserved as a potential
//! [future Rust keyword](https://doc.rust-lang.org/reference/keywords.html#reserved-keywords),
//! and is hence avoided as an identifier throughout Layout21.  
//!

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::outline;
use crate::stack::RelZ;

/// Abstract-Layout
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Abstract {
    /// Cell Name
    pub name: String,
    /// Outline in "Tetris-Shapes"
    pub outline: outline::Outline,
    /// Number of Metal Layers Used
    pub metals: usize,
    /// Ports
    pub ports: Vec<Port>,
}
impl Abstract {
    /// Create a new abstract layout. No ports are initially defined.
    pub fn new(name: impl Into<String>, metals: usize, outline: outline::Outline) -> Self {
        Self {
            name: name.into(),
            outline,
            metals,
            ports: Vec::new(),
        }
    }
    /// Retrieve a reference to a port by name.
    /// Returns `None` if no port with that name exists.
    pub fn port(&self, name: &str) -> Option<&Port> {
        for port in &self.ports {
            if port.name == name {
                return Some(port);
            }
        }
        None
    }
}
/// Abstract-Layout Port
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Port {
    /// Port/ Signal Name
    pub name: String,
    /// Physical Info
    pub kind: PortKind,
}
/// Abstract-Layout Port Inner Detail
///
/// All location and "geometric" information per Port is stored here,
/// among a few enumerated variants.
///
/// Ports may either connect on x/y edges, or on the top (in the z-axis) layer.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PortKind {
    /// Ports which connect on x/y outline edges
    Edge {
        layer: usize,
        track: usize,
        side: Side,
    },
    /// Ports accessible from bot top *and* top-layer edges
    /// Note their `layer` field is implicitly defined as the cell's `metals`.
    ZTopEdge {
        /// Track Index
        track: usize,
        /// Side
        side: Side,
        /// Location into which the pin extends inward
        into: (usize, RelZ),
    },
    /// Ports which are internal to the cell outline,
    /// but connect from above in the z-stack.
    /// These can be assigned at several locations across their track,
    /// and are presumed to be internally-connected between such locations.
    ZTopInner {
        /// Locations
        locs: Vec<TopLoc>,
    },
}
/// A location (track intersection) on our top z-axis layer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TopLoc {
    /// Track Index
    track: usize,
    /// Intersecting Track Index
    at: usize,
    /// Whether `at` refers to the track-indices above or below
    relz: RelZ,
}
/// # Port Side Enumeration
///
/// Note there are only two such sides: the "origin-side" [BottomOrLeft] and the "width-side" [TopOrRight].
/// Each [Layer]'s orientation ([Dir]) dictates between bottom/left and top/right.
/// Also note the requirements on [Outline] shapes ensure each track has a unique left/right or top/bottom pair of edges.
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Side {
    BottomOrLeft,
    TopOrRight,
}
