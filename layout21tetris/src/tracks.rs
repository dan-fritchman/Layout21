// Std-lib imports
use std::fmt::Debug;

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::coords::DbUnits;
use crate::instance::Instance;
use crate::raw::{Dir, LayoutError, LayoutResult};
use crate::stack::{Assign, RelZ};
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
/// # Track "Specification" Entry
///
/// Either a single entry, or repitition thereof.
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TrackSpec {
    Entry(TrackEntry),
    Repeat(Repeat),
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
    pub fn repeat(e: impl Into<Vec<TrackEntry>>, nrep: usize) -> Self {
        Self::Repeat(Repeat::new(e, nrep))
    }
}
/// An array of layout `Entries`, repeated `nrep` times
#[derive(Default, Clone, Debug, Deserialize, Serialize, PartialEq)]
pub struct Repeat {
    pub entries: Vec<TrackEntry>,
    pub nrep: usize,
}
impl Repeat {
    pub fn new(e: impl Into<Vec<TrackEntry>>, nrep: usize) -> Self {
        Self {
            entries: e.into(),
            nrep,
        }
    }
}
#[derive(Debug, Clone)]
pub struct TrackData {
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
}
/// # Track
///
/// An "instantiated" track, including:
/// * Track-long data in a [TrackData], and
/// * A set of [TrackSegment]s
#[derive(Debug, Clone)]
pub struct Track<'lib> {
    /// Track-long data
    pub data: TrackData,
    /// Set of wire-segments, in positional order
    pub segments: Vec<TrackSegment<'lib>>,
}
impl<'lib> Track<'lib> {
    /// Verify a (generally just-created) [Track] is valid
    pub fn validate(self) -> LayoutResult<Self> {
        if self.data.width < DbUnits(0) {
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
                TrackSegmentType::Cut { .. } => Err(TrackError::Conflict(
                    // Error: trying to assign a net onto a Cut.
                    TrackConflict::Assign(assn.clone()),
                    TrackConflict::from(seg.tp.clone()),
                )),
                TrackSegmentType::Blockage { .. } => {
                    // FIXME: sort out the desired behaviour here.
                    // Vias above ZTop instance-pins generally land in this case.
                    // We could check for their locations? Or just let it go.
                    Ok(())
                }
                TrackSegmentType::Wire { ref mut src, .. } => {
                    // The good case - assignment succeeds.
                    src.replace(assn);
                    Ok(())
                }
            },
        }
    }
    /// Insert a cut or blockage corresponding to `blockage`.
    pub fn cut_or_block(
        &mut self,
        start: DbUnits,
        stop: DbUnits,
        tp: TrackSegmentType<'lib>,
    ) -> TrackResult<()> {
        // First bounds-check against the end of our segments, which are the end of the cell
        if stop > self.segments.last().unwrap().stop {
            return Err(TrackError::OutOfBounds(stop));
        }
        // Find the segment where the blockage starts
        let segidx = self
            .segments
            .iter_mut()
            .position(|seg| seg.stop > start)
            .ok_or(TrackError::OutOfBounds(start))?
            .clone();
        let seg = &mut self.segments[segidx];
        // Check for conflicts, and get a copy of our segment-type as we will likely insert a similar segment
        let tpcopy = match seg.tp {
            TrackSegmentType::Blockage { ref src } => {
                return Err(TrackError::BlockageConflict(
                    TrackConflict::from(tp),
                    src.clone(),
                ));
            }
            TrackSegmentType::Cut { src } => {
                return Err(TrackError::CutConflict(
                    TrackConflict::from(tp),
                    src.clone(),
                ));
            }
            TrackSegmentType::Wire { .. } => seg.tp.clone(),
            TrackSegmentType::Rail(_) => seg.tp.clone(),
        };
        // Make sure the cut only effects one segment, or fail
        if seg.stop < stop {
            // FIXME this should really be the *next* segment, borrow checking fight
            return Err(TrackError::Overlap(seg.stop, stop));
        }

        // All clear; time to cut it.
        // In the more-common case in which the cut-end and segment-end *do not* coincide, create and insert a new segment.
        let mut to_be_inserted: Vec<(usize, TrackSegment)> = Vec::new();
        to_be_inserted.push((segidx + 1, TrackSegment { start, stop, tp }));
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
    pub fn block(&mut self, start: DbUnits, stop: DbUnits, src: &Ptr<Instance>) -> TrackResult<()> {
        self.cut_or_block(start, stop, TrackSegmentType::Blockage { src: src.clone() })
    }
    /// Cut from `start` to `stop`.
    /// Fails if the region is not a contiguous wire segment.
    pub fn cut(&mut self, start: DbUnits, stop: DbUnits, src: &'lib TrackCross) -> TrackResult<()> {
        self.cut_or_block(start, stop, TrackSegmentType::Cut { src })
    }
    /// Set the stop position for our last [TrackSegment] to `stop`
    pub fn stop(&mut self, stop: DbUnits) -> LayoutResult<()> {
        if self.segments.len() == 0 {
            LayoutError::fail("Error Stopping Track")?;
        }
        let idx = self.segments.len() - 1;
        self.segments[idx].stop = stop;
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
    Cut { src: &'lib TrackCross },
    Blockage { src: Ptr<Instance> },
    Wire { src: Option<&'lib Assign> },
    Rail(RailKind),
}
/// # Track Reference
///
/// Integer-pair representing a pointer to a [Layer] and track-index.
///
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct TrackRef {
    /// Layer Index
    pub layer: usize,
    /// Track Index
    pub track: usize,
}
impl TrackRef {
    /// Create a new [TrackRef]
    pub fn new(layer: usize, track: usize) -> Self {
        Self { layer, track }
    }
}
/// # Track Crossing
///
/// Located intersection between opposite-direction [Layer]s in [Track]-Space
///
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct TrackCross {
    /// "Primary" [Track] being referred to
    pub track: TrackRef,
    /// Intersecting "secondary" track
    pub cross: TrackRef,
}
impl TrackCross {
    pub fn new(track: TrackRef, cross: TrackRef) -> Self {
        Self { track, cross }
    }
    /// Create from four [usize], representing the two (layer-index, track-index) pairs.
    pub fn from_parts(layer1: usize, index1: usize, layer2: usize, index2: usize) -> Self {
        Self {
            track: TrackRef::new(layer1, index1),
            cross: TrackRef::new(layer2, index2),
        }
    }
    /// Create from a (layer-index, track-index) pair and a [RelZ]
    pub fn from_relz(layer: usize, track: usize, at: usize, relz: RelZ) -> Self {
        let layer2 = match relz {
            RelZ::Above => layer + 1,
            RelZ::Below => layer - 1,
        };
        let track = TrackRef { layer, track };
        let cross = TrackRef {
            layer: layer2,
            track: at,
        };
        Self::new(track, cross)
    }
}

#[derive(Debug, Clone)]
pub enum TrackConflict {
    Assign(Assign),
    Cut(TrackCross),
    Blockage(Ptr<Instance>),
}
impl std::fmt::Display for TrackConflict {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            // Delegate simpler types to [Debug]
            TrackConflict::Assign(a) => std::fmt::Debug::fmt(a, f),
            TrackConflict::Cut(c) => std::fmt::Debug::fmt(c, f),
            // And for more complicated ones, [Display]
            TrackConflict::Blockage(i) => std::fmt::Debug::fmt(i, f),
        }
    }
}
impl From<TrackSegmentType<'_>> for TrackConflict {
    fn from(tp: TrackSegmentType<'_>) -> Self {
        match tp {
            TrackSegmentType::Cut { src } => TrackConflict::Cut(src.clone()),
            TrackSegmentType::Blockage { src } => TrackConflict::Blockage(src.clone()),
            _ => unreachable!(),
        }
    }
}
pub enum TrackError {
    OutOfBounds(DbUnits),
    Overlap(DbUnits, DbUnits),
    Conflict(TrackConflict, TrackConflict),
    CutConflict(TrackConflict, TrackCross),
    BlockageConflict(TrackConflict, Ptr<Instance>),
}
pub type TrackResult<T> = Result<T, TrackError>;
impl std::fmt::Debug for TrackError {
    /// Display a [TrackError]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TrackError::OutOfBounds(stop) => write!(f, "Track Out of Bounds: {:?}", stop),
            TrackError::Overlap(p0, p1) => {
                write!(f, "Overlapping Track cuts at: {:?}, {:?}", p0, p1)
            }
            TrackError::CutConflict(t0, t1) => {
                write!(f, "Conflicting Track-Cuts at: {:?}, {:?}", t0, t1)
            }
            TrackError::BlockageConflict(t0, t1) => {
                write!(
                    f,
                    "Conflicting Instance Blockages: \n * {}\n * {:?}\n",
                    t0, t1
                )
            }
            TrackError::Conflict(t0, t1) => {
                write!(f, "Conflict Between: \n * {}\n * {:?}\n", t0, t1)
            }
        }
    }
}
impl std::fmt::Display for TrackError {
    /// Display a [TrackError]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}
impl std::error::Error for TrackError {}
impl Into<LayoutError> for TrackError {
    fn into(self) -> LayoutError {
        LayoutError::Boxed(Box::new(self))
    }
}
