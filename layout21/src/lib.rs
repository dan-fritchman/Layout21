//!
//! # Layout21 Structured Layout Generation Package
//! (with some cutesy name to come)
//!

// Std-lib imports
use std::convert::TryFrom;
use std::fmt::Debug;

// Crates.io
use serde::{Deserialize, Serialize};
use slotmap::{new_key_type, SlotMap};

// Re-exports
pub use layout21raw as raw;
pub mod abstrakt;
pub mod cell;
pub mod coords;
pub mod interface;
pub mod outline;
pub mod rawconv;
pub mod validate;

// Local imports
use coords::{DbUnits, Xy};
use raw::{Dir, LayoutError, LayoutResult, Point, Unit};

/// Unit Tests Module
#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TrackEntry {
    pub ttype: TrackType,
    pub width: DbUnits,
}
impl TrackEntry {
    /// Helper method: create of [TrackEntry] of [TrackType] [TrackType::Gap]
    pub fn gap(width: impl Into<DbUnits>) -> Self {
        TrackEntry {
            width: width.into(),
            ttype: TrackType::Gap,
        }
    }
    /// Helper method: create of [TrackEntry] of [TrackType] [TrackType::Signal]
    pub fn sig(width: impl Into<DbUnits>) -> Self {
        TrackEntry {
            width: width.into(),
            ttype: TrackType::Signal,
        }
    }
}
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum TrackType {
    Gap,
    Signal,
    Rail(RailKind),
}
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum RailKind {
    Pwr,
    Gnd,
}
impl RailKind {
    fn to_string(&self) -> String {
        match self {
            Self::Pwr => "VDD".into(),
            Self::Gnd => "VSS".into(),
        }
    }
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TrackSpec {
    Entry(TrackEntry),
    Pat(Pattern),
}
impl TrackSpec {
    pub fn gap(width: impl Into<DbUnits>) -> Self {
        Self::Entry(TrackEntry {
            width: width.into(),
            ttype: TrackType::Gap,
        })
    }
    pub fn sig(width: impl Into<DbUnits>) -> Self {
        Self::Entry(TrackEntry {
            width: width.into(),
            ttype: TrackType::Signal,
        })
    }
    pub fn rail(width: impl Into<DbUnits>, rk: RailKind) -> Self {
        Self::Entry(TrackEntry {
            width: width.into(),
            ttype: TrackType::Rail(rk),
        })
    }
    pub fn pwr(width: impl Into<DbUnits>) -> Self {
        Self::Entry(TrackEntry {
            width: width.into(),
            ttype: TrackType::Rail(RailKind::Pwr),
        })
    }
    pub fn gnd(width: impl Into<DbUnits>) -> Self {
        Self::Entry(TrackEntry {
            width: width.into(),
            ttype: TrackType::Rail(RailKind::Gnd),
        })
    }
    pub fn pat(e: impl Into<Vec<TrackEntry>>, nrep: usize) -> Self {
        Self::Pat(Pattern::new(e, nrep))
    }
}
/// An array of layout `Entries`, repeated `nrep` times
#[derive(Default, Clone, Debug, Deserialize, Serialize, PartialEq)]
pub struct Pattern {
    pub entries: Vec<TrackEntry>,
    pub nrep: usize,
}
impl Pattern {
    pub fn new(e: impl Into<Vec<TrackEntry>>, nrep: usize) -> Self {
        Self {
            entries: e.into(),
            nrep,
        }
    }
}
/// # Stack
///
/// The z-stack, primarily including metal, via, and primitive layers
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Stack {
    /// Measurement units
    pub units: Unit,
    /// Layer used for cell outlines/ boundaries
    pub boundary_layer: Option<raw::Layer>,
    /// Primitive Layer
    pub prim: PrimitiveLayer,
    /// Set of metal layers
    pub layers: Vec<Layer>,
    /// Set of via layers
    pub vias: Vec<ViaLayer>,
}
/// # Layer
///
/// Metal layer in a [Stack]
/// Each layer is effectively infinite-spanning in one dimension, and periodic in the other.
/// Layers with `dir=Dir::Horiz` extend to infinity in x, and repeat in y, and vice-versa.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Layer {
    /// Layer Name
    pub name: String,
    /// Direction Enumeration (Horizontal/ Vertical)
    pub dir: Dir,
    /// Default size of wire-cuts
    pub cutsize: DbUnits,
    /// Track Size & Type Entries
    pub entries: Vec<TrackSpec>,
    /// Offset, in our periodic dimension
    pub offset: DbUnits,
    /// Overlap between periods
    pub overlap: DbUnits,
    /// Layer(s) for streaming exports
    pub raw: Option<raw::Layer>,
    /// Setting for period-by-period flipping
    pub flip: FlipMode,
    /// Primitive-layer relationship
    pub prim: PrimitiveMode,
}
impl Layer {
    /// Convert this [Layer]'s track-info into a [LayerPeriod]
    fn to_layer_period(&self, index: usize, stop: impl Into<DbUnits>) -> LayoutResult<LayerPeriod> {
        let stop = stop.into();
        let mut period = LayerPeriod::default();
        period.index = index;
        let mut cursor = self.offset + (self.pitch() * index);
        let entries = self.entries();
        let iterator: Box<dyn Iterator<Item = _>> =
            if self.flip == FlipMode::EveryOther && index % 2 == 1 {
                Box::new(entries.iter().rev())
            } else {
                Box::new(entries.iter())
            };
        for e in iterator {
            let d = e.width;
            match e.ttype {
                TrackType::Gap => (),
                TrackType::Rail(railkind) => {
                    period.rails.push(
                        Track {
                            ttype: e.ttype,
                            index: period.rails.len(),
                            dir: self.dir,
                            start: cursor,
                            width: d,
                            segments: vec![TrackSegment {
                                net: Some(railkind.to_string()), // FIXME!
                                start: 0.into(),
                                stop,
                            }],
                        }
                        .validate()?,
                    );
                }
                TrackType::Signal => {
                    period.signals.push(
                        Track {
                            ttype: e.ttype,
                            index: period.signals.len(),
                            dir: self.dir,
                            start: cursor,
                            width: d,
                            segments: vec![TrackSegment {
                                net: None,
                                start: 0.into(),
                                stop,
                            }],
                        }
                        .validate()?,
                    );
                }
            };
            cursor += d;
        }
        Ok(period)
    }
    /// Flatten our [Entry]s into a vector
    /// Removes any nested patterns
    fn entries(&self) -> Vec<TrackEntry> {
        let mut v: Vec<TrackEntry> = Vec::new();
        for e in self.entries.iter() {
            match e {
                TrackSpec::Entry(ee) => v.push(ee.clone()),
                // FIXME: why doesn't this recursively call `entries`? Seems it could/should.
                TrackSpec::Pat(p) => {
                    for _i in 0..p.nrep {
                        for ee in p.entries.iter() {
                            v.push(ee.clone());
                        }
                    }
                }
            }
        }
        v
    }
    /// Sum up this [Layer]'s pitch
    fn pitch(&self) -> DbUnits {
        self.entries().iter().map(|e| e.width).sum::<DbUnits>() - self.overlap
    }
}
/// # Via / Insulator Layer Between Metals
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ViaLayer {
    /// Layer name
    pub name: String,
    /// Connected metal-layer indices
    pub between: (usize, usize),
    /// Via size
    pub size: Xy<DbUnits>,
    /// Stream-out layer numbers
    pub raw: Option<raw::Layer>,
}
/// Assignment of a net onto a track-intersection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assign {
    /// Net Name
    pub net: String,
    /// Track Intersection Location
    pub at: TrackIntersection,
}
/// Relative Z-Axis Reference to one Layer `Above` or `Below` another
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RelZ {
    Above,
    Below,
}
impl RelZ {
    pub fn other(&self) -> Self {
        match *self {
            RelZ::Above => RelZ::Below,
            RelZ::Below => RelZ::Above,
        }
    }
}
/// # Layout Library
///
/// A combination of cell definitions, sub-libraries, and metadata
///
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Library {
    /// Library Name
    pub name: String,
    /// Cell Definitions
    pub cells: SlotMap<cell::CellBagKey, cell::CellBag>,
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
#[derive(Debug, Clone)]
pub struct Track {
    /// Track Type (Rail, Signal)
    pub ttype: TrackType,
    /// Track Index
    pub index: usize,
    /// Direction
    pub dir: Dir,
    /// Starting-point in off-dir axis
    pub start: DbUnits,
    /// Track width
    pub width: DbUnits,
    /// Set of wire-segments, in positional order
    pub segments: Vec<TrackSegment>,
}
impl Track {
    /// Verify a (generally just-created) [Track] is valid
    pub fn validate(self) -> LayoutResult<Self> {
        if self.width < DbUnits(0) {
            return Err(LayoutError::from("Negative Track Width"));
        }
        Ok(self)
    }
    /// Retrieve a (mutable) reference to the segment at cross-dimension `dist`
    /// Returns None for `dist` outside the segment, or in-between segments
    pub fn segment_at(&mut self, dist: DbUnits) -> Option<&mut TrackSegment> {
        if self.segments.len() < 1 {
            return None;
        }
        for seg in self.segments.iter_mut() {
            if seg.start > dist {
                return None;
            }
            if seg.start <= dist && seg.stop >= dist {
                return Some(seg);
            }
        }
        None
    }
    /// Cut all of our segments from `start` to `stop`
    pub fn cut(&mut self, start: DbUnits, stop: DbUnits) -> LayoutResult<()> {
        if self.segments.len() == 0 || stop <= start {
            return Err(LayoutError::msg("Error Cutting Track"));
        }
        // Find the segment to be cut
        let mut to_be_removed: Vec<usize> = Vec::new();
        let mut to_be_inserted: Option<(usize, TrackSegment)> = None;
        for (idx, seg) in self.segments.iter_mut().enumerate() {
            if seg.start > stop {
                // Loop done case
                break;
            } else if start > seg.stop {
                // Uninvolved, carry on
                continue;
            } else if start <= seg.start && stop >= seg.stop {
                // Removal case; cut covers entire segment
                to_be_removed.push(idx);
            } else if start > seg.start && start <= seg.stop && stop >= seg.stop {
                // Stop-side trim case
                seg.stop = start;
            } else if start <= seg.start && stop > seg.start && stop < seg.stop {
                // Start-side trim case
                seg.start = stop;
            } else if start > seg.start && stop < seg.stop {
                // Internal cut case
                let mut new_seg = seg.clone();
                new_seg.stop = start;
                seg.start = stop;
                to_be_inserted = Some((idx, new_seg));
            } else {
                return Err(LayoutError::msg("Internal Error: Track::cut"));
            }
        }
        if let Some((idx, seg)) = to_be_inserted {
            self.segments.insert(idx, seg);
        } else {
            // Remove any fully-cut elements, in reverse order so as to not screw up indices
            for idx in to_be_removed.iter().rev() {
                self.segments.remove(*idx);
            }
        }
        Ok(())
    }
    /// Set the stop position for our last [TrackSegment] to `stop`
    pub fn stop(&mut self, stop: DbUnits) -> LayoutResult<()> {
        if self.segments.len() == 0 {
            return Err(LayoutError::msg("Error Stopping Track"));
        }
        let idx = self.segments.len() - 1;
        self.segments[idx].stop = stop;
        Ok(())
    }
}
/// Transformed single period of [Track]s on a [Layer]
/// Splits track-info between signals and rails.
/// Stores each as a [Track] struct, which moves to a (start, width) size-format,
/// and includes a vector of track-segments for cutting and assigning nets.
#[derive(Debug, Clone, Default)]
pub struct LayerPeriod {
    pub index: usize,
    pub signals: Vec<Track>,
    pub rails: Vec<Track>,
}
impl LayerPeriod {
    /// Shift the period by `dist` in its periodic direction
    pub fn offset(&mut self, dist: DbUnits) -> LayoutResult<()> {
        for t in self.rails.iter_mut() {
            t.start += dist;
        }
        for t in self.signals.iter_mut() {
            t.start += dist;
        }
        Ok(())
    }
    /// Set the stop position for all [Track]s to `stop`
    pub fn stop(&mut self, stop: DbUnits) -> LayoutResult<()> {
        for t in self.rails.iter_mut() {
            t.stop(stop)?;
        }
        for t in self.signals.iter_mut() {
            t.stop(stop)?;
        }
        Ok(())
    }
    /// Cut all [Track]s from `start` to `stop`,
    /// cutting, shortening, or deleting `segments` along the way
    pub fn cut(&mut self, start: DbUnits, stop: DbUnits) -> LayoutResult<()> {
        for t in self.rails.iter_mut() {
            t.cut(start, stop)?;
        }
        for t in self.signals.iter_mut() {
            t.cut(start, stop)?;
        }
        Ok(())
    }
}
/// # Segments of un-split, single-net wire on a [Track]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackSegment {
    /// Net Name
    pub net: Option<String>,
    /// Start Location, in [Stack]'s `units`
    pub start: DbUnits,
    /// End/Stop Location, in [Stack]'s `units`
    pub stop: DbUnits,
}
/// Intersection Between Adjacent Layers in [Track]-Space
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackIntersection {
    /// Layer Index
    pub layer: usize,
    /// Track Index
    pub track: usize,
    /// Intersecting Track Index
    pub at: usize,
    /// Whether `at` refers to the track-indices above or below
    pub relz: RelZ,
}
impl TrackIntersection {
    pub(crate) fn transpose(&self) -> Self {
        let layer = match self.relz {
            RelZ::Above => self.layer + 1,
            RelZ::Below => self.layer - 1,
        };
        Self {
            layer,
            track: self.at,
            at: self.track,
            relz: self.relz.other(),
        }
    }
}

/// Indication of whether a layer flips in its periodic axis with every period,
/// as most standard-cell-style logic gates do.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum FlipMode {
    EveryOther,
    None,
}
/// Indication of whether a layer is owned by, partially included in, or external to the primitive blocks
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PrimitiveMode {
    Owned,
    Partial,
    None,
}
/// Description of the primitive-level cells in a [Stack]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PrimitiveLayer {
    pub pitches: Xy<DbUnits>,
}
