// Std-lib imports
use std::convert::TryFrom;
use std::fmt::Debug;

// Crates.io
use derive_more::{Add, AddAssign, DivAssign, From, MulAssign, Sub, SubAssign, Sum};
use enum_dispatch::enum_dispatch;
use num_integer;
use serde::{Deserialize, Serialize};
use slotmap::{new_key_type, SlotMap};

// "Raw" Layout Imports
use layout21raw as raw;
use raw::{
    AbstractKey, CellKey, CellRef, CellViewKey, Dir, LayoutError, LayoutResult, Point, Unit,
};

// Re-exports
pub mod abstrakt;
pub mod interface;
pub mod rawconv;
pub mod validate;

/// Unit Tests Module
#[cfg(test)]
mod tests;

/// # Location Integer Type-Alias
///
/// Many internal fields are conceptually unsigned integers, but also undergo lots of math.
/// Rather than converting at each call-site, most are converted to [Int] and value-checked at creation time.
///
/// Unsigned integers ([usize]) are generally used for indices, such as where the [Index] trait accepts them.
type Int = isize;

/// Much of the confusion in a multi-coordinate system such as this
/// lies in keeping track of which numbers are in which units.
/// There are three generally useful units of measure here:
/// * DB Units generally correspond to physical length quantities, e.g. nanometers
/// * Primitive pitches
/// * Per-layer pitches, parameterized by a metal-layer index
#[enum_dispatch]
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum UnitSpeced {
    DbUnits(DbUnits),
    PrimPitches(PrimPitches),
    LayerPitches(LayerPitches),
}
/// Empty trait, largely for auto-generation of [From] and [Into] implementations.
#[enum_dispatch(UnitSpeced)]
pub trait HasUnits: Clone + Copy {}

/// A Scalar Value in Database Units
#[derive(
    From,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    MulAssign,
    DivAssign,
    Sum,
    Debug,
    Default,
    Clone,
    Copy,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
)]
pub struct DbUnits(Int);
impl DbUnits {
    /// Every so often we need the raw number, fine. Use sparingly.
    #[inline(always)]
    fn raw(&self) -> Int {
        self.0
    }
}
impl HasUnits for DbUnits {}
impl std::ops::Div<DbUnits> for DbUnits {
    type Output = Int;
    fn div(self, rhs: DbUnits) -> Self::Output {
        self.raw() / rhs.raw()
    }
}
impl std::ops::Div<Int> for DbUnits {
    type Output = Self;
    fn div(self, rhs: Int) -> Self::Output {
        Self(self.raw() / rhs)
    }
}
impl std::ops::Rem<DbUnits> for DbUnits {
    type Output = Int;
    fn rem(self, rhs: DbUnits) -> Self::Output {
        self.raw().rem(rhs.raw())
    }
}
impl std::ops::Mul<DbUnits> for DbUnits {
    type Output = Self;
    fn mul(self, rhs: DbUnits) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}
impl std::ops::Mul<Int> for DbUnits {
    type Output = Self;
    fn mul(self, rhs: Int) -> Self::Output {
        Self(self.0 * rhs)
    }
}
impl std::ops::Mul<usize> for DbUnits {
    type Output = Self;
    fn mul(self, rhs: usize) -> Self::Output {
        Self(Int::try_from(rhs).unwrap() * self.0)
    }
}

/// A Scalar Value in Primitive-Pitches
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct PrimPitches {
    dir: Dir,
    num: Int,
}
impl HasUnits for PrimPitches {}
impl std::ops::Add<PrimPitches> for PrimPitches {
    type Output = PrimPitches;
    /// Adding primitive-pitch values.
    /// Panics if the two are not in the same direction.
    fn add(self, rhs: Self) -> Self::Output {
        if self.dir != rhs.dir {
            panic!()
        }
        Self {
            dir: self.dir,
            num: self.num + rhs.num,
        }
    }
}

/// A Scalar Value in Layer-Pitches
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct LayerPitches {
    layer: usize,
    num: Int,
}
impl HasUnits for LayerPitches {}

/// Paired "type" zero-data enum for [UnitSpeced]
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum UnitType {
    DbUnits,
    PrimPitches,
    LayerPitches,
}

/// Common geometric pairing of (x,y) coordinates
/// Represents points, sizes, rectangles, and anything else that pairs `x` and `y` fields.
/// *Only* instantiable with [HasUnits] data.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct Xy<T: HasUnits> {
    pub x: T,
    pub y: T,
}
impl<T: HasUnits> Xy<T> {
    /// Create a new [Xy].
    fn new(x: T, y: T) -> Xy<T> {
        Self { x, y }
    }
    /// Create a new [Xy] with transposed coordinates.
    fn transpose(&self) -> Xy<T> {
        Self {
            y: self.x,
            x: self.y,
        }
    }
}
impl<T: HasUnits> std::ops::Index<Dir> for Xy<T> {
    type Output = T;
    fn index(&self, dir: Dir) -> &Self::Output {
        match dir {
            Dir::Horiz => &self.x,
            Dir::Vert => &self.y,
        }
    }
}
impl From<(Int, Int)> for Xy<DbUnits> {
    fn from(tup: (Int, Int)) -> Self {
        Self {
            x: tup.0.into(),
            y: tup.1.into(),
        }
    }
}
impl From<(Int, Int)> for Xy<PrimPitches> {
    fn from(tup: (Int, Int)) -> Self {
        Self {
            x: PrimPitches {
                dir: Dir::Horiz,
                num: tup.0.into(),
            },
            y: PrimPitches {
                dir: Dir::Vert,
                num: tup.1.into(),
            },
        }
    }
}

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
    /// Cell Names
    pub cell_names: Vec<String>,
    /// Abstracts
    pub abstracts: SlotMap<AbstractKey, abstrakt::Abstract>,
    /// Cell Implementations
    pub cells: SlotMap<CellKey, Cell>,
    /// Sub-Libraries
    pub libs: Vec<Library>,
}
impl Library {
    /// Create a new and initially empty [Library]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            cell_names: Vec::new(),
            abstracts: SlotMap::with_key(),
            cells: SlotMap::with_key(),
            libs: Vec::new(),
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
/// # Layout Cell
///
/// A combination of lower-level cell instances and net-assignments to tracks.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cell {
    /// Cell Name
    pub name: String,
    /// Top-layer index
    pub top_layer: usize,
    /// Outline shape, counted in x and y pitches of `stack`
    pub outline: Outline,
    /// Layout Instances
    pub instances: Vec<Instance>,
    /// Net-to-track assignments
    pub assignments: Vec<Assign>,
    /// Track cuts
    pub cuts: Vec<TrackIntersection>,
}
/// Block Outlines are "Tetris Shaped" rectilinear polygons
///
/// These boundaries are closed, consist solely of 90-degree rectangular turns,
/// and are specified by a counter-clockwise set of points.
/// "Holes" such as the shapes "O" and "8" and "divots" such as the shapes "U" and "H" are not supported.
///
/// Two equal-length vectors `x` and `y` describe an Outline's points.
/// Counter-clockwise-ness and divot-free-ness requires that:
/// * (a) `x` values are monotonically non-increasing, and
/// * (b) `y` values are monotonically non-decreasing
///
/// In point-space terms, such an outline has vertices at:
/// `[(0,0), (x[0], 0), (x[0], y[0]), (x[1], y[0]), ... , (0, y[-1]), (0,0)]`
/// With the final point at (0, y[-1]), and its connection back to the origin both implied.
///
/// Example: a rectangular Outline would require a single entry for each of `x` and `y`,
/// at the rectangle's vertex opposite the origin in both axes.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Outline {
    pub x: Vec<PrimPitches>,
    pub y: Vec<PrimPitches>,
}
impl Outline {
    /// Outline constructor, with inline checking for validity of `x` & `y` vectors
    pub fn new(x: &[Int], y: &[Int]) -> LayoutResult<Self> {
        // Check that x and y are of compatible lengths
        if x.len() < 1 || x.len() != y.len() {
            return Err(LayoutError::Tbd);
        }
        // Check for:
        // * x non-increasing-ness,
        // * y for non-decreasing-ness
        // * all non-negative values
        if x[0] < 0 || y[0] < 0 {
            return Err(LayoutError::Tbd);
        }
        for k in 1..x.len() {
            if x[k] > x[k - 1] {
                return Err(LayoutError::Tbd);
            }
            if y[k] < y[k - 1] {
                return Err(LayoutError::Tbd);
            }
            if x[k] < 0 || y[k] < 0 {
                return Err(LayoutError::Tbd);
            }
        }
        // Convert into [PrimPitches] united-objects, and return a new Self.
        let x = x
            .into_iter()
            .map(|i| PrimPitches {
                num: *i,
                dir: Dir::Horiz,
            })
            .collect();
        let y = y
            .into_iter()
            .map(|i| PrimPitches {
                num: *i,
                dir: Dir::Vert,
            })
            .collect();
        Ok(Self { x, y })
    }
    /// Create a new rectangular outline of dimenions `x` by `y`
    pub fn rect(x: Int, y: Int) -> LayoutResult<Self> {
        Self::new(&[x], &[y])
    }
    /// Maximum x-coordinate
    /// (Which is also always the *first* x-coordinate)
    pub fn xmax(&self) -> PrimPitches {
        self.x[0]
    }
    /// Maximum y-coordinate
    /// (Which is also always the *last* y-coordinate)
    pub fn ymax(&self) -> PrimPitches {
        self.y[self.y.len() - 1]
    }
    /// Maximum coordinate in [Dir] `dir`
    pub fn max(&self, dir: Dir) -> PrimPitches {
        match dir {
            Dir::Horiz => self.xmax(),
            Dir::Vert => self.ymax(),
        }
    }
}

/// # Cell View Enumeration
/// All of the ways in which a Cell is represented
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CellView {
    Interface(interface::Bundle),
    Abstract(abstrakt::Abstract),
    Layout(Cell),
    RawLayout(raw::Cell),
}
/// Collection of the Views describing a Cell
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellViews {
    name: String,
    views: SlotMap<CellViewKey, CellView>,
}

/// Trait for accessing three-dimensional [Outline] data from several views of Layouts
pub trait HasOutline: Debug {
    /// Retrieve a reference to the x-y [Outline]
    fn outline(&self) -> &Outline;
    /// Retrieve the top z-axis layer
    fn top_layer(&self) -> usize;
}
impl HasOutline for Cell {
    fn outline(&self) -> &Outline {
        &self.outline
    }
    fn top_layer(&self) -> usize {
        self.top_layer
    }
}
impl HasOutline for abstrakt::Abstract {
    fn outline(&self) -> &Outline {
        &self.outline
    }
    fn top_layer(&self) -> usize {
        self.top_layer
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

/// Instance of another Cell
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instance {
    /// Instance Name
    pub inst_name: String,
    pub cell_name: String,
    /// Cell Definition Reference
    pub cell: CellRef,
    /// Bottom-Left Corner Point
    pub loc: Xy<PrimPitches>,
    /// Reflection
    pub reflect: bool,
    /// Angle of Rotation (Degrees)
    pub angle: Option<f64>,
}
