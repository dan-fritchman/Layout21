// Std-lib imports
use std::fmt::Debug;

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::cell::Instance;
use crate::coords::{DbUnits, Xy};
use crate::raw::{self, Dir, LayoutError, LayoutResult, Units};
use crate::utils::Ptr;

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
    pub fn to_string(&self) -> String {
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
#[derive(Debug, Clone)]
pub struct Stack {
    /// Measurement units
    pub units: Units,
    /// Primitive Layer
    pub prim: PrimitiveLayer,
    /// Set of metal layers
    pub layers: Vec<Layer>,
    /// Set of via layers
    pub vias: Vec<ViaLayer>,
    /// [raw::Layer] Mappings
    pub rawlayers: Option<Ptr<raw::Layers>>,
    /// Layer used for cell outlines/ boundaries
    pub boundary_layer: Option<raw::LayerKey>,
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
    /// Setting for period-by-period flipping
    pub flip: FlipMode,
    /// Primitive-layer relationship
    pub prim: PrimitiveMode,
    /// [raw::Layer] for exports
    pub raw: Option<raw::LayerKey>,
}
impl Layer {
    /// Convert this [Layer]'s track-info into a [LayerPeriod]
    pub(crate) fn to_layer_period<'me, 'lib>(
        &'me self,
        index: usize,
        stop: impl Into<DbUnits>,
    ) -> LayoutResult<LayerPeriod<'lib>> {
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
                                tp: TrackSegmentType::Rail(railkind),
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
                                tp: TrackSegmentType::Wire { src: None },
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
    pub(crate) fn entries(&self) -> Vec<TrackEntry> {
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
    pub(crate) fn pitch(&self) -> DbUnits {
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
    pub raw: Option<raw::LayerKey>,
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

#[derive(Debug, Clone)]
pub struct Track<'lib> {
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
    pub segments: Vec<TrackSegment<'lib>>,
}
impl<'lib> Track<'lib> {
    /// Verify a (generally just-created) [Track] is valid
    pub fn validate(self) -> LayoutResult<Self> {
        if self.width < DbUnits(0) {
            return Err(LayoutError::from("Negative Track Width"));
        }
        Ok(self)
    }
    /// Set the net of the track-segment at `at` to `net`
    pub fn set_net(&mut self, at: DbUnits, assn: &'lib Assign) -> TrackResult<()> {
        // First find the segment to be modified
        let mut seg = None;
        for s in self.segments.iter_mut() {
            if s.start > at {
                break;
            }
            if s.start <= at && s.stop >= at {
                seg = Some(s);
                break;
            }
        }
        match seg {
            None => Err(TrackError::OutOfBounds(at)),
            Some(seg) => match seg.tp {
                TrackSegmentType::Rail(_) => unreachable!(),
                TrackSegmentType::Blockage { .. } => {
                    // FIXME: sort out the desired behaviour here.
                    // Vias above ZTop instance-pins generally land in this case.
                    // We could check for their locations? Or just let it go.
                    Ok(())
                }
                TrackSegmentType::Cut { src } => Err(TrackError::CutConflict(src.clone())),
                TrackSegmentType::Wire { ref mut src, .. } => {
                    src.replace(assn);
                    Ok(())
                }
            },
        }
    }
    /// Insert a blockage from `start` to `stop`.
    /// Fails if the region is not a contiguous wire segment.
    pub fn cut_or_block(&mut self, blockage: TrackSegment<'lib>) -> TrackResult<()> {
        // Find the segment where the blockage starts
        let segidx = self
            .segments
            .iter_mut()
            .position(|seg| seg.stop >= blockage.start)
            .ok_or(TrackError::OutOfBounds(blockage.start))?
            .clone();
        let seg = &mut self.segments[segidx];
        // Check for conflicts, and get a copy of our segment-type as we will likely insert a similar segment
        let tpcopy = match seg.tp {
            TrackSegmentType::Blockage { src } => Err(TrackError::BlockageConflict(src.clone())),
            TrackSegmentType::Cut { src } => Err(TrackError::CutConflict(src.clone())),
            TrackSegmentType::Wire { .. } => Ok(seg.tp.clone()),
            TrackSegmentType::Rail(_) => Ok(seg.tp.clone()),
        }?;
        // Make sure the cut only effects one segment, or fail
        if seg.stop < blockage.stop {
            // FIXME this should really be the *next* segment, borrow checking fight
            return Err(TrackError::Overlap(seg.stop, blockage.stop));
        }

        // All clear; time to cut it.
        // In the more-common case in which the cut-end and segment-end *do not* coincide,
        // create and insert a new segment.
        // De-structure the dimensional parts of `blockage`
        let start = blockage.start;
        let stop = blockage.stop;
        let mut to_be_inserted: Vec<(usize, TrackSegment)> = Vec::new();
        to_be_inserted.push((segidx + 1, blockage));
        if seg.stop != stop {
            let newseg = TrackSegment {
                tp: tpcopy,
                start: stop,
                stop: seg.stop,
            };
            to_be_inserted.push((segidx + 2, newseg));
        }
        // Update the existing segment (and importantly, drop its mutable borrow)
        seg.stop = start;
        for (idx, seg) in to_be_inserted {
            self.segments.insert(idx, seg);
        }
        Ok(())
    }
    /// Insert a blockage from `start` to `stop`.
    /// Fails if the region is not a contiguous wire segment.
    pub fn block(&mut self, start: DbUnits, stop: DbUnits, src: &'lib Instance) -> TrackResult<()> {
        let seg = TrackSegment {
            start,
            stop,
            tp: TrackSegmentType::Blockage { src },
        };
        self.cut_or_block(seg)
    }
    /// Cut from `start` to `stop`.
    /// Fails if the region is not a contiguous wire segment.
    pub fn cut(
        &mut self,
        start: DbUnits,
        stop: DbUnits,
        src: &'lib TrackIntersection,
    ) -> TrackResult<()> {
        let seg = TrackSegment {
            start,
            stop,
            tp: TrackSegmentType::Cut { src },
        };
        self.cut_or_block(seg)
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
pub struct LayerPeriod<'lib> {
    pub index: usize,
    pub signals: Vec<Track<'lib>>,
    pub rails: Vec<Track<'lib>>,
}
impl<'lib> LayerPeriod<'lib> {
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
    pub fn cut(
        &mut self,
        start: DbUnits,
        stop: DbUnits,
        src: &'lib TrackIntersection,
    ) -> TrackResult<()> {
        for t in self.rails.iter_mut() {
            t.cut(start, stop, src)?;
        }
        for t in self.signals.iter_mut() {
            t.cut(start, stop, src)?;
        }
        Ok(())
    }
    /// Block all [Track]s from `start` to `stop`,
    pub fn block(&mut self, start: DbUnits, stop: DbUnits, src: &'lib Instance) -> TrackResult<()> {
        for t in self.rails.iter_mut() {
            t.block(start, stop, src)?;
        }
        for t in self.signals.iter_mut() {
            t.block(start, stop, src)?;
        }
        Ok(())
    }
}
/// # Segments of un-split, single-net wire on a [Track]
#[derive(Debug, Clone)]
pub struct TrackSegment<'lib> {
    /// Segment-Type
    pub tp: TrackSegmentType<'lib>,
    /// Start Location, in [Stack]'s `units`
    pub start: DbUnits,
    /// End/Stop Location, in [Stack]'s `units`
    pub stop: DbUnits,
}
#[derive(Debug, Clone)]
pub enum TrackSegmentType<'lib> {
    Cut { src: &'lib TrackIntersection },
    Blockage { src: &'lib Instance },
    Wire { src: Option<&'lib Assign> },
    Rail(RailKind),
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

#[derive(Debug)]
pub enum TrackError {
    OutOfBounds(DbUnits),
    Overlap(DbUnits, DbUnits),
    CutConflict(TrackIntersection),
    BlockageConflict(Instance),
}
pub type TrackResult<T> = Result<T, TrackError>;
