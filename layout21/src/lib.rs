use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Add;

use num_integer;
use num_traits::Num;
use serde::{Deserialize, Serialize};
use slotmap::{new_key_type, SlotMap};

/// # "Raw" Layout Module
pub mod raw;

// Create key-types for each internal type stored in [SlotMap]s
new_key_type! {
    /// Keys for [Cell] entries
    pub struct CellKey;
    /// Keys for [abstrakt::Abstract] entries
    pub struct AbstractKey;
    /// Keys for [CellView] entries
    pub struct CellViewKey;
}
/// LayoutError-Specific Result Type
pub type LayoutResult<T> = Result<T, LayoutError>;

/// Distance Units Enumeration
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Unit {
    /// Micrometers, or microns for we olde folke
    Micro,
    /// Nanometers
    Nano,
}
impl Default for Unit {
    /// Default units are nanometers
    fn default() -> Unit {
        Unit::Nano
    }
}

/// Much of the confusion in a multi-coordinate system such as this
/// lies in keeping track of which numbers are in which units.
/// There are three generally useful units of measure here:
/// * DB Units generally correspond to physical length quantities, e.g. nanometers
/// * Primitive pitches
/// * Per-layer pitches, parameterized by a metal-layer index
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum UnitSpeced<T: Num + Copy> {
    DbUnits(DbUnits<T>),
    PrimPitches(PrimPitches<T>),
    LayerPitches(LayerPitches<T>),
}
/// A Scalar Value in Database Units
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct DbUnits<T: Num + Copy>(T);
/// A Scalar Value in Primitive-Pitches
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct PrimPitches<T: Num + Copy>(T);
// Admittedly, this type-signature is about Rust at its worst.
// Adding two references to [PrimPitches] with (potentially) different lifetimes makes this mouthful.
impl<'a, 'b, T: 'b + Num + Copy + Add<&'b PrimPitches<T>, Output = T>> Add for &'a PrimPitches<T> {
    type Output = PrimPitches<T>;

    fn add(self, other: Self) -> Self::Output {
        PrimPitches(self.0 + other.0)
    }
}
/// A Scalar Value in Layer-Pitches
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct LayerPitches<T: Num + Copy> {
    layer: usize,
    val: T,
}

/// Direction Enumeration
/// Primarily for [Layer] orientations
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum Dir {
    /// Horizontal
    Horiz,
    /// Vertical
    Vert,
}
impl Dir {
    /// Whichever direction we are, return the other one.
    fn other(&self) -> Self {
        match self {
            Self::Horiz => Self::Vert,
            Self::Vert => Self::Horiz,
        }
    }
}
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TrackEntry {
    pub width: usize,
    pub ttype: TrackType,
}
impl TrackEntry {
    /// Helper method: create of [TrackEntry] of [TrackType] [TrackType::Gap]
    pub fn gap(width: usize) -> Self {
        TrackEntry {
            width,
            ttype: TrackType::Gap,
        }
    }
    /// Helper method: create of [TrackEntry] of [TrackType] [TrackType::Signal]
    pub fn sig(width: usize) -> Self {
        TrackEntry {
            width,
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
    pub fn gap(width: usize) -> Self {
        Self::Entry(TrackEntry {
            width,
            ttype: TrackType::Gap,
        })
    }
    pub fn sig(width: usize) -> Self {
        Self::Entry(TrackEntry {
            width,
            ttype: TrackType::Signal,
        })
    }
    pub fn rail(width: usize, rk: RailKind) -> Self {
        Self::Entry(TrackEntry {
            width,
            ttype: TrackType::Rail(rk),
        })
    }
    pub fn pwr(width: usize) -> Self {
        Self::Entry(TrackEntry {
            width,
            ttype: TrackType::Rail(RailKind::Pwr),
        })
    }
    pub fn gnd(width: usize) -> Self {
        Self::Entry(TrackEntry {
            width,
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Stack {
    /// Measurement units
    pub units: Unit,
    /// Layer used for cell outlines/ boundaries
    pub boundary_layer: Option<raw::DataTypeMap>,
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
    pub cutsize: usize,
    /// Track Size & Type Entries
    pub entries: Vec<TrackSpec>,
    /// Offset, in our periodic dimension
    pub offset: isize,
    /// Layer(s) for streaming exports
    pub raw: Option<raw::DataTypeMap>,
    /// Setting for period-by-period flipping
    pub flip: FlipMode,
    /// Primitive-layer relationshio
    pub prim: PrimitiveMode,
    /// Overlap between periods
    pub overlap: usize,
}
impl Layer {
    /// Convert this [Layer]'s track-info into a [LayerPeriod]
    fn to_layer_period(&self, index: usize, stop: usize) -> LayerPeriod {
        let mut period = LayerPeriod::default();
        period.index = index;
        let mut cursor = self.offset + (self.pitch() * index) as isize;
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
                    period.rails.push(Track {
                        ttype: e.ttype,
                        index: period.rails.len(),
                        dir: self.dir,
                        start: cursor,
                        width: d,
                        segments: vec![TrackSegment {
                            net: Some(railkind.to_string()), // FIXME!
                            start: 0,
                            stop,
                        }],
                    });
                }
                TrackType::Signal => {
                    period.signals.push(Track {
                        ttype: e.ttype,
                        index: period.signals.len(),
                        dir: self.dir,
                        start: cursor,
                        width: d,
                        segments: vec![TrackSegment {
                            net: None,
                            start: 0,
                            stop,
                        }],
                    });
                }
            };
            cursor += d as isize;
        }
        period
    }
    /// Flatten our [Entry]s into a vector
    /// Removes any nested patterns
    fn entries(&self) -> Vec<TrackEntry> {
        let mut v: Vec<TrackEntry> = Vec::new();
        for e in self.entries.iter() {
            match e {
                TrackSpec::Entry(ee) => v.push(ee.clone()),
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
    fn pitch(&self) -> usize {
        self.entries().iter().map(|e| e.width).sum::<usize>() - self.overlap
    }
}
/// # Via / Insulator Layer Between Metals
///
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ViaLayer {
    /// Layer name
    pub name: String,
    /// Connected metal-layer indices
    pub between: (usize, usize),
    /// Via size
    pub size: Point,
    /// Stream-out layer numbers
    pub raw: Option<raw::DataTypeMap>,
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
/// Instance of another Cell
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instance {
    /// Instance Name
    pub inst_name: String,
    /// Cell Name/ Path
    pub cell_name: String,
    /// Cell Definition Reference
    pub cell: CellRef,
    /// Bottom-Left Corner Point
    pub p0: Point,
    /// Reflection
    pub reflect: bool,
    /// Angle of Rotation (Degrees)
    pub angle: Option<f64>,
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
    pub start: isize,
    /// Track width
    pub width: usize,
    /// Set of wire-segments, in positional order
    pub segments: Vec<TrackSegment>,
}
impl Track {
    /// Retrieve a (mutable) reference to the segment at cross-dimension `dist`
    /// Returns None for `dist` outside the segment, or in-between segments
    pub fn segment_at(&mut self, dist: isize) -> Option<&mut TrackSegment> {
        if self.segments.len() < 1 {
            return None;
        }
        for seg in self.segments.iter_mut() {
            if seg.start as isize > dist {
                return None;
            }
            if seg.start as isize <= dist && seg.stop as isize >= dist {
                return Some(seg);
            }
        }
        None
    }
    /// Cut all of our segments from `start` to `stop`
    pub fn cut(&mut self, start: usize, stop: usize) -> LayoutResult<()> {
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
    pub fn stop(&mut self, stop: usize) -> LayoutResult<()> {
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
    pub fn offset(&mut self, dist: isize) -> LayoutResult<()> {
        for t in self.rails.iter_mut() {
            t.start += dist;
        }
        for t in self.signals.iter_mut() {
            t.start += dist;
        }
        Ok(())
    }
    /// Set the stop position for all [Track]s to `stop`
    pub fn stop(&mut self, stop: usize) -> LayoutResult<()> {
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
    pub fn cut(&mut self, start: usize, stop: usize) -> LayoutResult<()> {
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
    pub start: usize,
    /// End/Stop Location, in [Stack]'s `units`
    pub stop: usize,
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
    pub x: Vec<PrimPitches<usize>>,
    pub y: Vec<PrimPitches<usize>>,
}
impl Outline {
    /// Outline constructor, with inline checking for validity of `x` & `y` vectors
    pub fn new(x: Vec<usize>, y: Vec<usize>) -> LayoutResult<Self> {
        // Check that x and y are of compatible lengths
        if x.len() != y.len() {
            return Err(LayoutError::Tbd);
        }
        if x.len() < 1 {
            return Err(LayoutError::Tbd);
        }
        // Check for x non-increasing-ness
        for k in 1..x.len() {
            if x[k] > x[k - 1] {
                return Err(LayoutError::Tbd);
            }
        }
        // Check for y non-decreasing-ness
        for k in 1..y.len() {
            if y[k] < y[k - 1] {
                return Err(LayoutError::Tbd);
            }
        }
        // Convert into [PrimPitches] united-objects, and return a new Self.
        let x = x.into_iter().map(|i| PrimPitches(i)).collect();
        let y = y.into_iter().map(|i| PrimPitches(i)).collect();
        Ok(Self { x, y })
    }
    /// Create a new rectangular outline of dimenions `x` by `y`
    pub fn rect(x: usize, y: usize) -> LayoutResult<Self> {
        Self::new(vec![x], vec![y])
    }
    /// Maximum x-coordinate
    /// (Which is also always the *first* x-coordinate)
    pub fn xmax(&self) -> PrimPitches<usize> {
        self.x[0]
    }
    /// Maximum y-coordinate
    /// (Which is also always the *last* y-coordinate)
    pub fn ymax(&self) -> PrimPitches<usize> {
        self.y[self.y.len() - 1]
    }
    /// Maximum coordinate in [Dir] `dir`
    pub fn max(&self, dir: Dir) -> PrimPitches<usize> {
        match dir {
            Dir::Horiz => self.xmax(),
            Dir::Vert => self.ymax(),
        }
    }
    /// Convert to a vector of polygon-vertex Points
    pub fn points(&self) -> Vec<Point> {
        let mut pts = vec![Point { x: 0, y: 0 }];
        let mut xp: isize;
        let mut yp: isize = 0;
        for i in 0..self.x.len() {
            xp = self.x[i].0 as isize;
            pts.push(Point::new(xp, yp));
            yp = self.y[i].0 as isize;
            pts.push(Point::new(xp, yp));
        }
        // Add the final implied Point at (x, y[-1])
        pts.push(Point::new(0, yp));
        pts
    }
}
/// # Point in two-dimensional layout-space
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct Point {
    x: isize,
    y: isize,
}
impl Point {
    /// Create a new [Point] from (x,y) coordinates
    pub fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }
    /// Create a new [Point] which serves as an offset in direction `dir`
    pub fn offset(val: isize, dir: Dir) -> Self {
        match dir {
            Dir::Horiz => Self { x: val, y: 0 },
            Dir::Vert => Self { x: 0, y: val },
        }
    }
    /// Create a new point shifted by `x` in the x-dimension and by `y` in the y-dimension
    pub fn shift(&self, p: &Point) -> Point {
        Point {
            x: p.x + self.x,
            y: p.y + self.y,
        }
    }
    /// Create a new point scaled by `p.x` in the x-dimension and by `p.y` in the y-dimension
    pub fn scale(&self, p: &Point) -> Point {
        Point {
            x: p.x * self.x,
            y: p.y * self.y,
        }
    }
    /// Get the coordinate associated with direction `dir`
    fn coord(&self, dir: Dir) -> isize {
        match dir {
            Dir::Horiz => self.x,
            Dir::Vert => self.y,
        }
    }
}

/// Raw-Layout Conversion Module
pub mod conv {
    use super::*;

    /// A short-lived set of references to an [Instance] and its cell-definition
    #[derive(Debug, Clone)]
    struct TempInstance<'a> {
        inst: &'a Instance,
        def: &'a (dyn HasOutline + 'static),
    }
    // Create key-types for each internal type stored in [SlotMap]s
    new_key_type! {
        /// Keys for [ValidAssign] entries
        pub struct AssignKey;
    }
    /// A short-lived Cell, largely organized by layer
    #[derive(Debug, Clone)]
    struct TempCell<'a> {
        /// Reference to the source [Cell]
        cell: &'a Cell,
        /// Reference to the source [Library]
        lib: &'a Library,
        /// Instances and references to their definitions
        instances: Vec<TempInstance<'a>>,
        /// Cuts, arranged by Layer
        cuts: Vec<Vec<validate::ValidTrackLoc>>,
        /// Validated Assignments
        assignments: SlotMap<AssignKey, validate::ValidAssign>,
        /// Assignments, arranged by Layer
        top_assns: Vec<Vec<AssignKey>>,
        /// Assignments, arranged by Layer
        bot_assns: Vec<Vec<AssignKey>>,
    }
    /// Temporary arrangement of data for a [Layer] within a [Cell]
    #[derive(Debug, Clone)]
    struct TempCellLayer<'a> {
        /// Reference to the validated metal layer
        layer: &'a validate::ValidMetalLayer,
        /// Reference to the parent cell
        cell: &'a TempCell<'a>,
        /// Instances which intersect with this layer and period
        instances: Vec<&'a TempInstance<'a>>,
        /// Pitch per layer-period
        pitch: isize,
        /// Number of layer-periods
        nperiods: usize,
        /// Spanning distance in the layer's "infinite" dimension
        span: DbUnits<usize>,
    }

    /// Short-Lived structure of the stuff relevant for converting a single LayerPeriod,
    /// on a single Layer, in a single Cell.
    #[derive(Debug, Clone)]
    struct TempPeriod<'a> {
        periodnum: isize,
        cell: &'a TempCell<'a>,
        layer: &'a TempCellLayer<'a>,
        /// Instance Blockages
        blockages: Vec<(PrimPitches<usize>, PrimPitches<usize>)>,
        cuts: Vec<&'a validate::ValidTrackLoc>,
        top_assns: Vec<AssignKey>,
        bot_assns: Vec<AssignKey>,
    }
    /// # Converter from [Library] and constituent elements to [raw::Library]
    pub struct RawConverter {
        pub(crate) lib: Library,
        pub(crate) stack: validate::ValidStack,
    }
    impl RawConverter {
        /// Convert [Library] `lib` to a [raw::Library]
        /// Consumes `lib` in the process
        pub fn convert(lib: Library, stack: Stack) -> Result<raw::Library, LayoutError> {
            let stack = validate::StackValidator::validate(stack)?;
            Self { lib, stack }.convert_all()
        }
        /// Convert everything in our [Library]
        fn convert_all(self) -> LayoutResult<raw::Library> {
            let mut lib = raw::Library::new(&self.lib.name, self.stack.units);
            // // Collect up unit-cells on each layer
            // for layer in self.stack.metals.iter() {
            //     let unit = self.convert_layer_unit(&layer.spec)?;
            //     lib.cells.push(unit);
            // }
            // Convert each defined [Cell] to a [raw::Cell]
            for (_id, cell) in self.lib.cells.iter() {
                lib.cells.push(self.convert_cell(cell)?);
            }
            // And convert each (un-implemented) Abstract as a boundary
            for (_id, abs) in self.lib.abstracts.iter() {
                // FIXME: temporarily checking whether the same name is already defined
                for (_id, cell) in self.lib.cells.iter() {
                    if abs.name == cell.name {
                        continue;
                    }
                }
                lib.cells.push(self.convert_abstract(abs)?);
            }
            Ok(lib)
        }
        /// Convert to a raw layout cell
        fn convert_cell(&self, cell: &Cell) -> Result<raw::Cell, LayoutError> {
            if cell.outline.x.len() > 1 {
                return Err(LayoutError::Message(
                    "Non-rectangular outline; conversions not supported (yet)".into(),
                ));
            };
            let mut elems: Vec<raw::Element> = Vec::new();
            // Re-organize the cell into the format most helpful here
            let temp_cell = self.temp_cell(cell)?;
            // Convert a layer at a time, starting from bottom
            for layernum in 0..cell.top_layer {
                // Organize the cell/layer combo into temporary conversion format
                let temp_layer = self.temp_cell_layer(&temp_cell, &self.stack.metals[layernum]);
                // Convert each "layer period" one at a time
                for periodnum in 0..temp_layer.nperiods {
                    let periodnum = periodnum as isize;
                    // Again, re-organize into the relevant objects for this "layer period"
                    let temp_period = self.temp_cell_layer_period(&temp_layer, periodnum);
                    // And finally start doing stuff!
                    elems.extend(self.convert_cell_layer_period(&temp_period)?);
                }
            }
            // Convert our [Outline] into a polygon
            elems.push(self.convert_outline(cell)?);
            // Convert our [Instance]s dimensions
            // Note instances are of the same type, but use [Points] of different units.
            let scale = self.stack.pitches();
            let insts = cell
                .instances
                .iter()
                .map(|inst| {
                    // Scale the location of each instance by our pitches
                    let mut i = inst.clone();
                    i.p0 = inst.p0.scale(scale);
                    i
                })
                .collect();
            // Aaaand create & return our new [raw::Cell]
            Ok(raw::Cell {
                name: cell.name.clone(),
                insts,
                arrays: Vec::new(),
                elems,
            })
        }
        /// Create a [TempCell], organizing [Cell] data in more-convenient fashion for conversion
        fn temp_cell<'a>(&'a self, cell: &'a Cell) -> LayoutResult<TempCell<'a>> {
            // Create one of these for each of our instances
            let instances: Vec<TempInstance> = cell
                .instances
                .iter()
                .map(|inst| match inst.cell {
                    CellRef::Cell(c) => {
                        let def = self.lib.cells.get(c).ok_or(LayoutError::Tbd).unwrap();
                        Ok(TempInstance { inst, def })
                    }
                    CellRef::Abstract(c) => {
                        let def = self.lib.abstracts.get(c).ok_or(LayoutError::Tbd).unwrap();
                        Ok(TempInstance { inst, def })
                    }
                    _ => {
                        return Err(LayoutError::Export);
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            // Validate `cuts`, and arrange them by layer
            let mut cuts: Vec<Vec<validate::ValidTrackLoc>> = vec![vec![]; cell.top_layer()];
            for cut in cell.cuts.iter() {
                let c = validate::ValidTrackLoc::validate(cut, &self.stack)?;
                cuts[c.layer].push(c);
                // FIXME: cell validation should also check that this lies within our outline. probably do this earlier
            }
            // Validate all the cell's assignments, and arrange references by layer
            let mut bot_assns = vec![vec![]; cell.top_layer()];
            let mut top_assns = vec![vec![]; cell.top_layer()];
            let mut assignments = SlotMap::with_key();
            for assn in cell.assignments.iter() {
                let v = validate::ValidAssign::validate(assn, &self.stack)?;
                let bot = v.bot.layer;
                let top = v.top.layer;
                let k = assignments.insert(v);
                bot_assns[bot].push(k);
                top_assns[top].push(k);
            }
            // And create our (temporary) cell data!
            Ok(TempCell {
                cell,
                lib: &self.lib,
                instances,
                assignments,
                top_assns,
                bot_assns,
                cuts,
            })
        }
        /// Convert a single row/col (period) on a single layer in a single Cell.
        fn convert_cell_layer_period(
            &self,
            temp_period: &TempPeriod,
        ) -> LayoutResult<Vec<raw::Element>> {
            let mut elems: Vec<raw::Element> = Vec::new();
            let layer = temp_period.layer.layer; // FIXME! Can't love this name.

            // Create the layer-period object we'll manipulate most of the way
            let mut layer_period = temp_period
                .layer
                .layer
                .spec
                .to_layer_period(temp_period.periodnum as usize, temp_period.layer.span.0);
            // Insert blockages on each track
            for (n1, n2) in temp_period.blockages.iter() {
                let prim_pitch = self.stack.prim.pitch.coord(layer.spec.dir);
                let start = n1.0 * prim_pitch as usize;
                let stop = n2.0 * prim_pitch as usize;
                layer_period.cut(start, stop)?;
            }
            // Place all relevant cuts
            let nsig = layer_period.signals.len();
            for cut in temp_period.cuts.iter() {
                // Cut the assigned track
                let track = &mut layer_period.signals[cut.track & nsig];
                track.cut(
                    cut.dist as usize - layer.spec.cutsize / 2,
                    cut.dist as usize + layer.spec.cutsize / 2,
                )?;
            }
            // Handle Net Assignments
            // Start with those for which we're the lower of the two layers.
            // These will also be where we add vias
            let via_layer = &self.stack.vias[layer.index];
            for assn_id in temp_period.bot_assns.iter() {
                let assn = temp_period
                    .cell
                    .assignments
                    .get(*assn_id)
                    .ok_or(LayoutError::Export)?;
                // Grab a (mutable) reference to the assigned track
                let track = &mut layer_period.signals[assn.bot.track & nsig];
                // Find the segment corresponding to the off-axis coordinate
                let mut segment = track
                    .segment_at(assn.bot.dist)
                    .ok_or(LayoutError::msg("COULDNT FIND SEGMENT"))?;
                // Assign both track-segments to the net
                segment.net = Some(assn.net.clone());

                // FIXME: orientations will be wrong here
                let raw_layer = via_layer
                    .raw
                    .as_ref()
                    .ok_or(LayoutError::msg("Raw-Layout Layer Not Defined"))?;
                let e = raw::Element {
                    net: None,
                    layer: raw_layer.clone(), // FIXME: dont really wanna clone here
                    inner: raw::Shape::Rect {
                        p0: Point {
                            x: (assn.bot.dist - via_layer.size.x / 2) as isize,
                            y: (assn.top.dist - via_layer.size.y / 2) as isize,
                        },
                        p1: Point {
                            x: (assn.bot.dist + via_layer.size.x / 2) as isize,
                            y: (assn.top.dist + via_layer.size.y / 2) as isize,
                        },
                    },
                };
                elems.push(e);
            }
            // Assign all the segments for which we're the top layer
            for assn_id in temp_period.top_assns.iter() {
                let assn = temp_period
                    .cell
                    .assignments
                    .get(*assn_id)
                    .ok_or(LayoutError::Export)?;
                // Grab a (mutable) reference to the assigned track
                let track = &mut layer_period.signals[assn.top.track & nsig];
                // Find the segment corresponding to the off-axis coordinate
                let mut segment = track
                    .segment_at(assn.top.dist)
                    .ok_or(LayoutError::msg("COULDNT FIND SEGMENT"))?;
                // Assign both track-segments to the net
                segment.net = Some(assn.net.clone());
            }
            // Convert all TrackSegments to raw Elements
            for t in layer_period.rails.iter() {
                elems.extend(self.convert_track(t, &layer.spec)?);
            }
            for t in layer_period.signals.iter() {
                elems.extend(self.convert_track(t, &layer.spec)?);
            }
            Ok(elems)
        }
        /// Convert to a [raw::Cell], just including an Outline
        /// FIXME: also include the pins!
        pub fn convert_abstract(&self, abs: &abstrakt::Abstract) -> Result<raw::Cell, LayoutError> {
            // Create our [Outline]s boundary
            let outline = self.convert_outline(abs)?;
            // And return a new [raw::Cell]
            Ok(raw::Cell {
                name: abs.name.clone(),
                insts: Vec::new(),
                arrays: Vec::new(),
                elems: vec![outline],
            })
        }
        /// Convert to a [raw::Element] polygon
        pub fn convert_outline(&self, cell: &impl HasOutline) -> LayoutResult<raw::Element> {
            // Doing so requires our [Stack] specify a `boundary_layer`. If not, fail.
            let layer = (self.stack.boundary_layer)
                .as_ref()
                .ok_or(LayoutError::msg(
                    "Cannot Convert Abstract to Raw without Boundary Layer",
                ))?;
            let outline: &Outline = cell.outline();

            // Create an array of Outline-Points
            let pts = outline.points();

            // Scale them to our pitches
            let pts = pts
                .iter()
                .map(|p| p.scale(&self.stack.prim.pitch))
                .collect();
            // Create the [raw::Element]
            Ok(raw::Element {
                net: None,
                layer: layer.clone(), // FIXME: stop cloning
                inner: raw::Shape::Poly { pts },
            })
        }
        /// Convert a [Track]-full of [TrackSegment]s to a vector of [raw::Element] rectangles
        fn convert_track(&self, track: &Track, layer: &Layer) -> LayoutResult<Vec<raw::Element>> {
            let raw_layer = layer
                .raw
                .as_ref()
                .ok_or(LayoutError::msg("Raw-Layout Layer Not Defined"))?;

            let elems = track
                .segments
                .iter()
                .map(|seg| {
                    match track.dir {
                        Dir::Horiz => raw::Element {
                            net: seg.net.clone(),
                            layer: raw_layer.clone(), // FIXME: dont really wanna clone here
                            inner: raw::Shape::Rect {
                                p0: Point {
                                    x: (seg.start) as isize,
                                    y: (track.start as isize),
                                },
                                p1: Point {
                                    x: (seg.stop) as isize,
                                    y: (track.start + track.width as isize) as isize,
                                },
                            },
                        },
                        Dir::Vert => raw::Element {
                            net: seg.net.clone(),
                            layer: raw_layer.clone(), // FIXME: dont really wanna clone here
                            inner: raw::Shape::Rect {
                                p0: Point {
                                    x: (track.start as isize),
                                    y: (seg.start) as isize,
                                },
                                p1: Point {
                                    x: (track.start + track.width as isize) as isize,
                                    y: (seg.stop) as isize,
                                },
                            },
                        },
                    }
                })
                .collect();
            Ok(elems)
        }
        /// Create a [TempCellLayer] for the intersection of `temp_cell` and `layer`
        fn temp_cell_layer<'a>(
            &self,
            temp_cell: &'a TempCell,
            layer: &'a validate::ValidMetalLayer,
        ) -> TempCellLayer<'a> {
            // Sort out which of the cell's [Instance]s come up to this layer
            let instances: Vec<&TempInstance> = temp_cell
                .instances
                .iter()
                .filter(|i| i.def.top_layer() >= layer.index)
                .collect();

            // Sort out which direction we're working across
            let cell = temp_cell.cell;
            // FIXME: needs to be re-unit-ed
            let span: DbUnits<usize> = match layer.spec.dir {
                Dir::Horiz => DbUnits(cell.outline.x[0].0 * self.stack.prim.pitch.x as usize),
                Dir::Vert => DbUnits(cell.outline.y[0].0 * self.stack.prim.pitch.y as usize),
            };
            let outline_dim: DbUnits<usize> = match layer.spec.dir {
                Dir::Horiz => DbUnits(cell.outline.y[0].0 * self.stack.prim.pitch.y as usize),
                Dir::Vert => DbUnits(cell.outline.x[0].0 * self.stack.prim.pitch.x as usize),
            };

            // FIXME: we probably want to detect bad Outline dimensions sooner than this
            if outline_dim.0 % layer.pitch != 0 {
                panic!("HOWD WE GET THIS BAD LAYER?!?!");
            }
            let nperiods = outline_dim.0 / layer.pitch;
            let pitch = layer.pitch as isize;

            TempCellLayer {
                layer,
                cell: temp_cell,
                instances,
                nperiods,
                pitch,
                span,
            }
        }
        /// Create the [TempPeriod] at the intersection of `temp_layer` and `periodnum`
        fn temp_cell_layer_period<'a>(
            &self,
            temp_layer: &'a TempCellLayer,
            periodnum: isize,
        ) -> TempPeriod<'a> {
            let layer = temp_layer.layer;
            let cell = temp_layer.cell;

            // For each row, decide which instances intersect
            // Convert these into blockage-areas for the tracks
            let blockages = temp_layer
                .instances
                .iter()
                .filter(|i| {
                    let prim_pitches = self.stack.primitive_pitches(temp_layer.layer.index);
                    let min = i.inst.p0.coord(layer.spec.dir.other()) / prim_pitches.0 as isize;
                    let span = i.def.outline().max(layer.spec.dir.other()).0 / prim_pitches.0;
                    min <= periodnum && min + span as isize > periodnum
                })
                .map(|i| {
                    let start = i.inst.p0.coord(layer.spec.dir) as usize;
                    let span = i.def.outline().max(layer.spec.dir);
                    (PrimPitches(start), PrimPitches(start + span.0))
                })
                .collect();

            // Grab indices of the relevant tracks for this period
            let nsig = temp_layer.layer.period.signals.len();
            let relevant_track_nums = (periodnum * nsig as isize, (periodnum + 1) * nsig as isize);
            // Filter cuts down to those in this period
            let cuts: Vec<&validate::ValidTrackLoc> = cell.cuts[temp_layer.layer.index]
                .iter()
                .filter(|cut| {
                    cut.track >= relevant_track_nums.0 as usize
                        && cut.track < relevant_track_nums.1 as usize
                })
                .collect();
            // Filter assignments down to those in this period
            let top_assns = cell.top_assns[temp_layer.layer.index]
                .iter()
                .filter(|id| {
                    let assn = cell
                        .assignments
                        .get(**id)
                        .ok_or(LayoutError::Export)
                        .unwrap();
                    assn.top.track >= relevant_track_nums.0 as usize
                        && assn.top.track < relevant_track_nums.1 as usize
                })
                .copied()
                .collect();
            let bot_assns = cell.bot_assns[temp_layer.layer.index]
                .iter()
                .filter(|id| {
                    let assn = cell
                        .assignments
                        .get(**id)
                        .ok_or(LayoutError::Export)
                        .unwrap();
                    assn.bot.track >= relevant_track_nums.0 as usize
                        && assn.bot.track < relevant_track_nums.1 as usize
                })
                .copied()
                .collect();

            TempPeriod {
                periodnum,
                cell,
                layer: temp_layer,
                blockages,
                cuts,
                top_assns,
                bot_assns,
            }
        }
        // /// Create a raw-cell covering a single unit of `layer`
        // pub fn convert_layer_unit(&self, layer: &Layer) -> LayoutResult<raw::Cell> {
        //     let pitch = match layer.spec.dir {
        //         Dir::Horiz => self.stack.xpitch,
        //         Dir::Vert => self.stack.ypitch,
        //     };
        //     let mut elems: Vec<raw::Element> = Vec::new();
        //     // FIXME: probably get away from this Layer::tracks method. Everything else has.
        //     for track in layer.tracks().iter_mut() {
        //         track.segments = vec![TrackSegment {
        //             net: None, // FIXME!
        //             start: 0,
        //             stop: pitch,
        //         }];
        //         // Convert into [raw::Element] rectangles.
        //         // This vector always has just one element, but is easier to iterate over (once).
        //         for e in self.convert_track(&track, layer)?.into_iter() {
        //             elems.push(e);
        //         }
        //     }
        //     Ok(raw::Cell {
        //         name: format!("{}::unit", layer.name.clone()),
        //         insts: Vec::new(),
        //         arrays: Vec::new(),
        //         elems,
        //     })
        // }
    }
}
/// # Abstract Layout Module
///
/// Abstract layouts describe a block's outline and interface,
/// without exposing implementation details.
/// Cells primarily comprise their outlines and pins.
/// Outlines follow the same "Tetris-Shapes" as (OtherNameTbd) layout cells,
/// including the requirements for a uniform z-axis.
/// Internal layers are "fully blocked", in that parent layouts may not route through them.
/// In legacy layout systems this would be akin to including blockages of the same shape as [Outline] on each layer.
///
/// Sadly the english-spelled name "abstract" is reserved as a potential [future Rust keyword](https://doc.rust-lang.org/reference/keywords.html#reserved-keywords).
/// Hence the misspelling.
///
pub mod abstrakt {
    use super::*;
    /// Abstract-Layout
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Abstract {
        /// Cell Name
        pub name: String,
        /// Outline in "Tetris-Shapes"
        pub outline: Outline,
        /// Top Metal Layer
        pub top_layer: usize,
        /// Ports
        pub ports: Vec<Port>,
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
        /// Ports which are internal to the cell outline,
        /// but connect from above in the z-stack.
        /// These can be assigned at several locations across their track,
        /// and are presumed to be internally-connected between such locations.
        Zlocs {
            /// Locations
            locs: Vec<TopLoc>,
        },
        /// Ports which occupy an entire top-level track from edge to edge
        Zfull { track: usize },
        // FIXME:
        // * Sort out cases for "both", i.e. pins on the top-level which also go to X/Y edges
        // * Primitives may need a different kinda `cross`
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
    /// X/Y Side Enumeration
    /// Note the requirements on [Outline] shapes ensure each track has a unique left/right or top/bottom pair of edges.
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Side {
        Left,
        Right,
        Top,
        Bottom,
    }
}
/// Interfaces Module,
/// Describing Cells in terms of their IO Interfaces
pub mod interface {
    use serde::{Deserialize, Serialize};
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Port {
        /// Port Name
        pub name: String,
        /// Port Type & Content
        pub kind: PortKind,
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum PortKind {
        /// Flat Scalar Port, e.g. `clk`
        Scalar,
        /// Array-Based Port, e.g. `data[31:0]`
        Array { width: usize },
        /// Instance of a Hierarchical Bundle
        Bundle { bundle_name: String },
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Bundle {
        pub name: String,
        pub ports: Vec<Port>,
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

///
/// # Layout Error Enumeration
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LayoutError {
    /// Uncategorized Error with Message
    Message(String),
    /// Error Exporting to Foreign Format
    Export,
    /// Validation of input data
    Validation,
    /// Everything to be categorized
    Tbd,
}
impl LayoutError {
    /// Create a [LayoutError::Message] from anything String-convertible
    fn msg(s: impl Into<String>) -> Self {
        Self::Message(s.into())
    }
}
/// # Cell Reference Enumeration
/// Used for enumerating the different types of things an [Instance] may refer to
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CellRef {
    Cell(CellKey),
    Abstract(AbstractKey),
    Name(String),
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
pub mod validate {
    use super::*;
    /// Helper-function for asserting all sorts of boolean conditions, returning [LayoutResult] and enabling the question-mark operator.
    pub(crate) fn assert(b: bool) -> LayoutResult<()> {
        match b {
            true => Ok(()),
            false => Err(LayoutError::Validation),
        }
    }
    /// Derived data for a [Stack], after it has gone through some validation steps.
    #[derive(Debug)]
    pub(crate) struct ValidStack {
        /// Measurement units
        pub units: Unit,
        /// Layer used for cell outlines/ boundaries
        pub boundary_layer: Option<raw::DataTypeMap>,

        /// Primitive layer
        pub prim: PrimitiveLayer,
        /// Set of via layers
        pub vias: Vec<ViaLayer>,
        /// Metal Layers
        pub(crate) metals: Vec<ValidMetalLayer>,
        /// Pitches per metal layer, one each for those in `stack`
        pub(crate) pitches: Vec<usize>,
    }
    impl ValidStack {
        /// Retrieve a [Point] two-tuple of our x/y pitches
        pub(crate) fn pitches(&self) -> &Point {
            &self.prim.pitch
        }
        /// Retrieve the number of primitive-sized pitches in a period of layer number `layer`
        pub(crate) fn primitive_pitches(&self, layer: usize) -> PrimPitches<usize> {
            let layer = &self.metals[layer];
            let prim_pitch = self.prim.pitch.coord(layer.spec.dir.other()) as usize;
            if layer.pitch % prim_pitch != 0 {
                panic!("INVALID LAYER PITCH!!!");
            }
            return PrimPitches(layer.pitch / prim_pitch);
        }
    }
    #[derive(Debug)]
    pub(crate) struct StackValidator;
    impl StackValidator {
        pub(crate) fn validate(stack: Stack) -> LayoutResult<ValidStack> {
            let Stack {
                units,
                boundary_layer,
                vias,
                layers,
                prim,
                ..
            } = stack;
            // Validate the primitive layer
            assert(prim.pitch.x > 0)?;
            assert(prim.pitch.y > 0)?;

            // Validate each metal layer
            let metals = layers
                .into_iter()
                .enumerate()
                .map(|(num, layer)| ValidMetalLayer::validate(layer, num, &prim))
                .collect::<Result<Vec<_>, _>>()?;
            // Calculate pitches as the least-common multiple of same-direction layers below each layer
            let mut pitches = vec![0; metals.len()];
            for (num, metal) in metals.iter().enumerate() {
                let mut pitch = prim.pitch.coord(metal.spec.dir.other()) as usize;
                for nn in 0..num + 1 {
                    if metals[nn].spec.dir == metal.spec.dir {
                        pitch = num_integer::lcm(pitch, metals[nn].pitch);
                    }
                }
                pitches[num] = pitch;
            }
            // FIXME: checks on [ViaLayer]s
            // Stack checks out! Return its derived data
            Ok(ValidStack {
                units,
                boundary_layer,
                vias,
                pitches,
                metals,
                prim,
            })
        }
    }
    #[derive(Debug)]
    pub(crate) struct ValidMetalLayer {
        /// Original Layer Spec
        pub(crate) spec: Layer,

        // Derived data
        /// Index in layers array
        pub(crate) index: usize,
        /// Derived single-period template
        pub(crate) period: LayerPeriod,
        /// Pitch in db-units
        pub(crate) pitch: usize,
    }
    impl ValidMetalLayer {
        /// Perform validation on a [Layer], return a corresponding [ValidMetalLayer]
        pub(crate) fn validate(
            layer: Layer,
            index: usize,
            prim: &PrimitiveLayer,
        ) -> LayoutResult<ValidMetalLayer> {
            // Check for non-zero widths of all entries
            for entry in layer.entries().iter() {
                assert(entry.width > 0)?;
            }
            let pitch = layer.pitch();
            assert(pitch > 0)?;
            // Check for fit on the primitive grid, if the layer is in primitives
            match layer.prim {
                PrimitiveMode::Partial | PrimitiveMode::Owned => {
                    assert(pitch % prim.pitch.coord(layer.dir.other()) as usize == 0)?;
                }
                PrimitiveMode::None => (),
            }
            // Convert to a prototype [LayerPeriod]
            // This is frequently used for calculating track locations
            let period = layer.to_layer_period(0, 0);
            Ok(ValidMetalLayer {
                spec: layer,
                index,
                period,
                pitch,
            })
        }
        /// Get the center-coordinate of signal-track `idx`, in our periodic dimension
        pub fn center(&self, idx: usize) -> isize {
            let track = &self.period.signals[idx % self.period.signals.len()];
            let mut cursor = (self.pitch * (idx / self.period.signals.len())) as isize;
            cursor += track.start + track.width as isize / 2;
            cursor
        }
    }
    /// Location on a [Track], including the db-unit cross-dimension
    #[derive(Debug, Clone)]
    pub(crate) struct ValidTrackLoc {
        pub layer: usize,
        pub track: usize,
        pub dist: isize,
    }
    impl ValidTrackLoc {
        /// Validate a [TrackIntersection], and convert the cross-dimension into db-units
        pub(crate) fn validate(i: &TrackIntersection, stack: &ValidStack) -> LayoutResult<Self> {
            // Check that we won't reach outside the stack, and grab the secondary layer
            assert(i.layer < stack.metals.len())?;
            let other = if i.relz == RelZ::Below {
                assert(i.layer >= 1)?;
                i.layer - 1
            } else {
                assert(i.layer < stack.metals.len() - 1)?;
                i.layer + 1
            };
            // And find the center of the `other` track
            Ok(ValidTrackLoc {
                layer: i.layer,
                track: i.track,
                dist: stack.metals[other].center(i.at),
            })
        }
    }
    /// Intersection between two validated [ValidTrackLoc],
    /// including the invariant that `top` is one layer above `bot`.
    #[derive(Debug, Clone)]
    pub(crate) struct ValidAssign {
        pub net: String,
        pub top: ValidTrackLoc,
        pub bot: ValidTrackLoc,
    }
    impl ValidAssign {
        /// Validate a [TrackIntersection], and convert into top/bottom coordinates
        pub(crate) fn validate(assn: &Assign, stack: &ValidStack) -> LayoutResult<Self> {
            // Name "validation": just empty-string checking, at least for now
            assert(assn.net.len() > 0)?;
            let net = assn.net.clone();
            // Validate the location and its transpose
            let loc1 = ValidTrackLoc::validate(&assn.at, stack)?;
            let loc2 = ValidTrackLoc::validate(&assn.at.transpose(), stack)?;
            // Finally arrange the two by top/bottom
            let (top, bot) = if assn.at.relz == RelZ::Below {
                (loc1, loc2)
            } else {
                (loc2, loc1)
            };
            Ok(Self { net, top, bot })
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
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct PrimitiveLayer {
    pub pitch: Point,
}
/// Unit Tests Module
#[cfg(test)]
mod tests;
