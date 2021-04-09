use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

use id_arena::{Arena, Id};
use serde::{Deserialize, Serialize};

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
/// Direction Enumeration
/// Primarily for [Layer] orientations
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Entry {
    Sig(usize),
    Pwr(usize),
    Gnd(usize),
    Gap(usize),
    Pat(Pattern),
}
/// An array of layout `Entries`, repeated `nrep` times
#[derive(Default, Clone, Debug, Deserialize, Serialize, PartialEq)]
pub struct Pattern {
    pub entries: Vec<Entry>,
    pub nrep: usize,
}
impl Pattern {
    pub fn new(e: impl Into<Vec<Entry>>, nrep: usize) -> Self {
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
    /// Primitive cell horizontal unit-pitch, denominated in `units`
    pub xpitch: usize,
    /// Primitive cell vertical unit-pitch, denominated in `units`
    pub ypitch: usize,
    /// Layer used for cell outlines/ boundaries
    pub boundary_layer: Option<raw::LayerSpec>,
    /// Set of metal layers
    pub layers: Vec<Layer>,
    /// Set of via layers
    pub vias: Vec<ViaLayer>,
}
impl Stack {
    /// Create a [raw::Cell] for the unit area on Layer `layer`
    pub fn get_unit(&mut self, layer: usize) -> Result<raw::Cell, LayoutError> {
        self.layers[layer].raw_unit(self)
    }
}
/// # Layer
///
/// Metal layer in a [Stack]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Layer {
    /// Layer Index
    pub index: usize,
    /// Layer Name
    pub name: String,
    /// Direction Enumeration (Horizontal/ Vertical)
    pub dir: Dir,
    /// Track Size & Type Entries
    pub entries: Vec<Entry>,
    /// X/Y Origin Offset
    pub offset: (isize, isize),
    /// Layer for streaming exports
    pub stream_layer: Option<raw::LayerSpec>,
}
impl Layer {
    /// Convert to a vector of [Track]s
    fn tracks(&self) -> Vec<Track> {
        let mut cursor = match self.dir {
            Dir::Horiz => self.offset.1,
            Dir::Vert => self.offset.0,
        };
        let mut index = 0;
        let mut tracks: Vec<Track> = Vec::new();
        for e in self.flat_entries().iter() {
            match e {
                Entry::Gap(d) => cursor += *d as isize,
                Entry::Pwr(d) | Entry::Gnd(d) | Entry::Sig(d) => {
                    tracks.push(Track {
                        layer: self.index,
                        index,
                        dir: self.dir,
                        start: cursor,
                        width: *d,
                        segments: Vec::new(),
                    });
                    cursor += *d as isize;
                    index += 1;
                }
                Entry::Pat(_) => {
                    panic!("FIXME!");
                    // return Err(LayoutError::Message(
                    //     "Internal Error: failed to flatten Patterns".into(),
                    // ))
                }
            }
        }
        tracks
    }
    /// Flatten our [Entry]s into a vector
    /// Removes any nested patterns
    fn flat_entries(&self) -> Vec<Entry> {
        let mut v = Vec::new();
        Self::flattener_helper(&self.entries, &mut v);
        v
    }
    /// Helper method for depth-first traversing vectors of (potentially) hierarchical entries
    /// Results are appended to vector `accum` in depth-first order
    fn flattener_helper(inp: &Vec<Entry>, accum: &mut Vec<Entry>) {
        for e in inp.iter() {
            match e {
                Entry::Gap(d) => accum.push(Entry::Gap(*d)),
                Entry::Pwr(d) => accum.push(Entry::Pwr(*d)),
                Entry::Gnd(d) => accum.push(Entry::Gnd(*d)),
                Entry::Sig(d) => accum.push(Entry::Sig(*d)),
                Entry::Pat(p) => {
                    for _i in 0..p.nrep {
                        Self::flattener_helper(&p.entries, accum);
                    }
                }
            }
        }
    }
    /// Create a raw-cell covering a single unit of our layer
    pub fn raw_unit(&self, stack: &Stack) -> Result<raw::Cell, LayoutError> {
        let layer_spec = self
            .stream_layer
            .ok_or(LayoutError::Message("No Stream-Layer Defined".into()))?;
        let pitch = match self.dir {
            Dir::Horiz => stack.xpitch,
            Dir::Vert => stack.ypitch,
        };
        let mut elems: Vec<raw::Element> = Vec::new();
        for track in self.tracks().iter_mut() {
            track.segments = vec![TrackSegment {
                net: None, // FIXME!
                start: 0,
                stop: pitch,
            }];
            // Convert into [raw::Element] rectangles.
            // This vector always has just one element, but is easier to iterate over (once).
            for e in track.to_raw_elems(stack, layer_spec)?.into_iter() {
                elems.push(e);
            }
        }
        Ok(raw::Cell {
            name: self.get_unit_name(),
            insts: Vec::new(),
            arrays: Vec::new(),
            elems,
        })
    }
    fn get_unit_name(&self) -> String {
        format!("{}::unit", self.name.clone())
    }
}
/// # Via / Insulator Layer Between Metals
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ViaLayer {
    /// Layer index
    pub index: usize,
    /// Layer name
    pub name: String,
    /// Connected metal-layer indices
    pub between: (usize, usize),
    /// Via size
    pub size: Point,
    /// Stream-out layer numbers
    pub stream_layer: Option<raw::LayerSpec>,
}
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
/// Assignment of a net onto a track-intersection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assign {
    /// Net Name
    pub net: String,
    /// Track Intersection Location
    pub at: TrackIntersection,
}
/// Relative Z-Axis Reference to one Layer `Above` or `Below` another
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RelZ {
    Above,
    Below,
}
/// Instance of another Cell
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct Library {
    /// Library Name
    pub name: String,
    /// Reference to the z-stack
    pub stack: Stack,
    /// Cell Names
    pub cell_names: Vec<String>,
    /// Abstracts
    pub abstracts: Arena<abstrakt::Abstract>,
    /// Cell Implementations
    pub cells: Arena<Cell>,
    /// Sub-Libraries
    pub libs: Vec<Library>,
}
impl Library {
    /// Create a new and initially empty [Library]
    pub fn new(name: impl Into<String>, stack: Stack) -> Self {
        Self {
            name: name.into(),
            stack,
            cell_names: Vec::new(),
            abstracts: Arena::new(),
            cells: Arena::new(),
            libs: Vec::new(),
        }
    }
    /// Convert to a [raw::Library]
    pub fn to_raw_lib(&self) -> Result<raw::Library, LayoutError> {
        let mut lib = raw::Library::new(&self.name, self.stack.units);
        // Collect up unit-cells on each layer
        for layer in self.stack.layers.iter() {
            let unit = layer.raw_unit(&self.stack)?;
            lib.cells.push(unit);
        }
        // Convert each defined [Cell] to a [raw::Cell]
        for (_id, cell) in self.cells.iter() {
            lib.cells.push(cell.to_raw_cell(self)?);
        }
        // lib.cells = defs;
        // And convert each (un-implemented) Abstract as a boundary
        for (_id, abs) in self.abstracts.iter() {
            // FIXME: temporarily checking whether the same name is already defined
            for (_id, cell) in self.cells.iter() {
                if abs.name == cell.name {
                    continue;
                }
            }
            lib.cells.push(abs.to_raw_cell(self)?);
        }
        Ok(lib)
    }
}
#[derive(Debug, Clone)]
pub struct Track {
    /// Layer Index
    pub layer: usize,
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
    /// Convert our [TrackSegment]s to a vector of [raw::Element] rectangles
    fn to_raw_elems(
        &self,
        stack: &Stack,
        layer_spec: raw::LayerSpec,
    ) -> Result<Vec<raw::Element>, LayoutError> {
        let pitch = match self.dir {
            Dir::Horiz => stack.xpitch,
            Dir::Vert => stack.ypitch,
        };
        let elems = self
            .segments
            .iter()
            .map(|seg| match self.dir {
                Dir::Horiz => raw::Element {
                    layer_spec,
                    inner: raw::Shape::Rect {
                        p0: Point {
                            x: (pitch * seg.start) as isize,
                            y: (self.start as isize),
                        },
                        p1: Point {
                            x: (pitch * seg.stop) as isize,
                            y: (self.start + self.width as isize) as isize,
                        },
                    },
                },
                Dir::Vert => raw::Element {
                    layer_spec,
                    inner: raw::Shape::Rect {
                        p0: Point {
                            x: (self.start as isize),
                            y: (pitch * seg.start) as isize,
                        },
                        p1: Point {
                            x: (self.start + self.width as isize) as isize,
                            y: (pitch * seg.stop) as isize,
                        },
                    },
                },
            })
            .collect();
        Ok(elems)
    }
}
/// # Segments of un-split, single-net wire on a [Track]
#[derive(Debug, Clone)]
pub struct TrackSegment {
    /// Net Name
    pub net: Option<String>,
    /// Start Location
    pub start: usize,
    /// End/Stop Location
    pub stop: usize,
}
/// # Layout Cell
///
/// A combination of lower-level cell instances and net-assignments to tracks.
///
#[derive(Debug, Clone)]
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
impl Cell {
    /// Gather a list of the names of cells we instantiate
    fn cell_ref_names(&self) -> Vec<String> {
        self.instances.iter().map(|i| i.cell_name.clone()).collect()
    }
    /// Convert to a raw layout cell
    pub fn to_raw_cell(&self, lib: &Library) -> Result<raw::Cell, LayoutError> {
        println!("TO RAW CELL {:?}", self.name);

        if self.outline.x.len() > 1 {
            return Err(LayoutError::Message(
                "Non-rectangular outline; not supported (yet)".into(),
            ));
        };
        let mut elems: Vec<raw::Element> = Vec::new();

        /// A short-lived set of references to an [Instance] and its cell-definition
        #[derive(Debug, Clone)]
        struct TempInstance<'a> {
            inst: &'a Instance,
            def: &'a (HasOutline + 'static),
        }
        // Create one of these for each of our instances
        let temp_instances: Vec<TempInstance> = self
            .instances
            .iter()
            .map(|inst| {
                match inst.cell {
                    CellRef::Cell(c) => {
                        let def = lib.cells.get(c).ok_or(LayoutError::Tbd).unwrap();
                        TempInstance { inst, def }
                    }
                    CellRef::Abstract(c) => {
                        let def = lib.abstracts.get(c).ok_or(LayoutError::Tbd).unwrap();
                        TempInstance { inst, def }
                    }
                    _ => panic!("FIXME!"),
                    // _ => return Err(LayoutError::Tbd),
                }
            })
            .collect();

        // Iterate over tracks, chopping them at instances and cuts
        for layernum in 0..self.top_layer {
            let layer = &lib.stack.layers[layernum];
            println!("LAYER: {:?}", layer.index);

            // Sort out which of our [Instance]s come up to this layer
            let layer_instances: Vec<&TempInstance> = temp_instances
                .iter()
                .filter(|i| i.def.top_layer() >= layer.index)
                .collect();
            println!("LAYER_INSTS: {:?}", layer_instances);

            // Sort out which direction we're working across
            let (m, n) = match layer.dir {
                Dir::Horiz => (self.outline.y[0], self.outline.x[0]),
                Dir::Vert => (self.outline.x[0], self.outline.y[0]),
            };

            for rown in 0..m {
                let rown = rown as isize;
                println!("ROWN: {:?}", rown);
                // For each row, decide which instances intersect
                let intersecting_instances: Vec<&TempInstance> = layer_instances
                    .iter()
                    .filter(|i| {
                        i.inst.p0.coord(layer.dir.other()) <= rown
                            && i.inst.p0.coord(layer.dir.other())
                                + i.def.outline().max(layer.dir.other()) as isize
                                > rown
                    })
                    .map(|i| i.clone())
                    .collect();
                println!("INTERSECTING_INSTS: {:?}", intersecting_instances);
                // Convert these into blockage-areas for the tracks
                let blockages: Vec<(usize, usize)> = intersecting_instances
                    .iter()
                    .map(|i| {
                        (
                            i.inst.p0.coord(layer.dir) as usize,
                            i.inst.p0.coord(layer.dir) as usize + i.def.outline().max(layer.dir),
                        )
                    })
                    .collect();
                println!("BLOCKAGES: {:?}", blockages);
                // FIXME: these blockages may need some sorting
                // Convert these into "non-blockages", areas where we need Track
                let mut segments: Vec<(usize, usize)> = Vec::new();
                let mut cursor = 0;
                for b in blockages {
                    if b.0 > cursor {
                        segments.push((cursor, b.0));
                    }
                    cursor = b.1;
                }
                if cursor < n {
                    segments.push((cursor, n));
                    cursor = n;
                }
                // FIXME: sorting out where, conceptually, we wanna delineate between signal-tracks and power/ground rails
                // Generally the "API" should use "signal tracks", at some point in conversion to raw::Layout
                // things must move to "raw tracks"

                let layer_spec = layer.stream_layer.ok_or(LayoutError::Tbd)?;

                for t in layer.tracks().iter_mut() {
                    t.segments = segments
                        .iter()
                        .map(|(n1, n2)| TrackSegment {
                            net: None, // FIXME!
                            start: *n1,
                            stop: *n2,
                        })
                        .collect();
                    let shift = Point::offset(rown * lib.stack.xpitch as isize, layer.dir.other());
                    for mut e in t.to_raw_elems(&lib.stack, layer_spec)?.into_iter() {
                        e.inner.shift(&shift);
                        elems.push(e);
                    }
                }
                println!("ELEMS: {:?}", elems);
            }
        }
        // FIXME: handle cuts!
        // for cut in self.cuts.iter() {
        //     unimplemented!("???");
        //     // Split the track into segments
        // }
        // FIXME: handle assignments!
        // for assn in self.assignments.iter() {
        //     unimplemented!("???");
        //     // Find the coordinate of each track-pair
        //     // Insert a corresponding via
        //     // Assign both track-segments to the net
        // }

        // Convert our [Outline] into a polygon
        elems.push(self.outline.to_raw_elem(lib)?);
        // Convert our [Instance]s dimensions
        // Note instances are of the same type, but use [Points] of different units.
        let scale = (lib.stack.xpitch as isize, lib.stack.ypitch as isize);
        let insts = self
            .instances
            .iter()
            .map(|inst| {
                // Scale the location of each instance by our pitches
                let mut i = inst.clone();
                i.p0 = inst.p0.scale(scale.0, scale.1);
                i
            })
            .collect();
        // Aaaand create & return our new [raw::Cell]
        Ok(raw::Cell {
            name: self.name.clone(),
            insts,
            arrays: Vec::new(),
            elems,
        })
    }
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
#[derive(Debug, Clone)]
pub struct Outline {
    pub x: Vec<usize>,
    pub y: Vec<usize>,
}
impl Outline {
    /// Outline constructor, with inline checking for validity of `x` & `y` vectors
    pub fn new(x: Vec<usize>, y: Vec<usize>) -> Result<Self, LayoutError> {
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
        Ok(Self { x, y })
    }
    /// Create a new rectangular outline of dimenions `x` by `y`
    pub fn rect(x: usize, y: usize) -> Result<Self, LayoutError> {
        Self::new(vec![x], vec![y])
    }
    /// Maximum x-coordinate
    /// (Which is also always the *first* x-coordinate)
    pub fn xmax(&self) -> usize {
        self.x[0]
    }
    /// Maximum y-coordinate
    /// (Which is also always the *last* y-coordinate)
    pub fn ymax(&self) -> usize {
        self.y[self.y.len() - 1]
    }
    /// Maximum coordinate in [Dir] `dir`
    pub fn max(&self, dir: Dir) -> usize {
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
            xp = self.x[i] as isize;
            pts.push(Point::new(xp, yp));
            yp = self.y[i] as isize;
            pts.push(Point::new(xp, yp));
        }
        // Add the final implied Point at (x, y[-1])
        pts.push(Point::new(0, yp));
        pts
    }
    /// Convert to a [raw::Element] polygon
    pub fn to_raw_elem(&self, lib: &Library) -> Result<raw::Element, LayoutError> {
        // Doing so requires our [Stack] specify a `boundary_layer`. If not, fail.
        let layer_spec = lib.stack.boundary_layer.ok_or(LayoutError::msg(
            "Cannot Convert Abstract to Raw without Boundary Layer",
        ))?;
        // Create an array of Outline-Points
        let pts = self.points();
        // Scale them to our pitches
        let pitch = (lib.stack.xpitch as isize, lib.stack.ypitch as isize);
        let pts = pts.iter().map(|p| p.scale(pitch.0, pitch.1)).collect();
        // Create the [raw::Element]
        Ok(raw::Element {
            layer_spec,
            inner: raw::Shape::Poly { pts },
        })
    }
}
/// # Point in two-dimensional layout-space
#[derive(Debug, Clone, Serialize, Deserialize)]
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
    /// Create a new point scaled by `x` in the x-dimension and by `y` in the y-dimension
    pub fn scale(&self, x: isize, y: isize) -> Point {
        Point {
            x: x * self.x,
            y: y * self.y,
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

/// # "Raw" Layout Module 
pub mod raw {
    use super::*;
    use gds21;

    // FIXME: need something like raw::Abstract, representing arbitrary-shaped abstract layouts
    #[derive(Debug, Default)]
    pub struct Abstract;

    /// # Raw Layout Library  
    /// A collection of cell-definitions and sub-library definitions
    #[derive(Debug, Default)]
    pub struct Library {
        /// Library Name
        pub name: String,
        /// Distance Units
        pub units: Unit,
        /// Sub-Library Definitions
        pub libs: Vec<Library>,
        /// Cell Definitions
        pub cells: Vec<Cell>,
    }
    impl Library {
        /// Create a new and empty Library
        pub fn new(name: impl Into<String>, units: Unit) -> Self {
            Self {
                name: name.into(),
                units,
                ..Default::default()
            }
        }
        /// Convert to a GDSII [gds21::GdsLibrary]
        pub fn to_gds(self) -> Result<gds21::GdsLibrary, LayoutError> {
            GdsConverter::new(self).convert()
        }
    }
    /// Raw-Layout Cell Definition
    #[derive(Debug)]
    pub struct Cell {
        /// Cell Name
        pub name: String,
        /// Cell Instances
        pub insts: Vec<Instance>,
        /// Instance Arrays
        pub arrays: Vec<InstArray>,
        /// Primitive Elements
        pub elems: Vec<Element>,
    }
    /// # Array of Instances
    ///
    /// Two-dimensional array of identical [Instance]s of the same [Cell].
    #[derive(Debug)]
    pub struct InstArray {
        pub inst_name: String,
        pub cell_name: String,
        pub rows: usize,
        pub cols: usize,
        pub xpitch: usize,
        pub ypitch: usize,
        pub p0: Point,
        pub reflect: bool,
        pub angle: Option<f64>,
    }
    /// # Layer Specification
    /// As in seemingly every layout system, this uses two numbers to identify each layer.
    #[derive(Debug, Clone, Copy, Serialize, Deserialize)]
    pub struct LayerSpec(i16, i16);
    impl LayerSpec {
        pub fn new(n1: i16, n2: i16) -> Self {
            Self(n1, n2)
        }
    }
    /// An instance of a primitive element,
    /// e.g. a geometric shape or piece of text
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Element {
        pub layer_spec: LayerSpec,
        pub inner: Shape,
    }
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Shape {
        Rect { p0: Point, p1: Point },
        Poly { pts: Vec<Point> },
    }
    impl Shape {
        /// Shift coordinates by the (x,y) values specified in `pt`
        pub fn shift(&mut self, pt: &Point) {
            match *self {
                Shape::Rect {
                    ref mut p0,
                    ref mut p1,
                } => {
                    p0.x += pt.x;
                    p0.y += pt.y;
                    p1.x += pt.x;
                    p1.y += pt.y;
                }
                Shape::Poly { ref mut pts } => {
                    for p in pts.iter_mut() {
                        p.x += pt.x;
                        p.y += pt.y;
                    }
                }
            }
        }
    }
    /// # Gds21 Converter
    ///
    /// The sole valid top-level entity for [gds21] conversion is always a [Library].
    ///
    #[derive(Debug)]
    pub struct GdsConverter {
        pub lib: Library,
    }
    impl GdsConverter {
        pub fn new(lib: Library) -> Self {
            Self { lib }
        }
        fn convert(&self) -> Result<gds21::GdsLibrary, LayoutError> {
            if self.lib.libs.len() > 0 {
                return Err(LayoutError::msg("No nested libraries to GDS, (yet)"));
            }
            // Create a new Gds Library
            let mut lib = gds21::GdsLibrary::new(&self.lib.name);
            // Set its distance units
            lib.units = match self.lib.units {
                Unit::Nano => gds21::GdsUnits::new(1e-3, 1e-9),
                Unit::Micro => gds21::GdsUnits::new(1e-3, 1e-6),
            };
            // And convert each of our `cells` into its `structs`
            lib.structs = self
                .lib
                .cells
                .iter()
                .map(|c| self.convert_cell(c))
                .collect();
            Ok(lib)
        }
        /// Convert a [Cell] to a [gds21::GdsStruct] cell-definition
        fn convert_cell(&self, cell: &Cell) -> gds21::GdsStruct {
            let mut elems = Vec::new();
            for inst in cell.insts.iter() {
                elems.push(self.convert_instance(inst).into());
            }
            for arr in cell.arrays.iter() {
                elems.push(self.convert_array(arr).into());
            }
            for elem in cell.elems.iter() {
                elems.push(self.convert_element(elem).into());
            }
            let mut s = gds21::GdsStruct::new(&cell.name);
            s.elems = elems;
            s
        }
        /// Convert an [Instance] to a GDS instance, AKA [gds21::GdsStructRef]
        fn convert_instance(&self, inst: &Instance) -> gds21::GdsStructRef {
            gds21::GdsStructRef {
                name: inst.cell_name.clone(),
                xy: vec![inst.p0.x as i32, inst.p0.y as i32],
                strans: None, //FIXME!
                elflags: None,
                plex: None,
            }
        }
        /// Convert an [Element] to a [gds21::GdsElement]
        pub fn convert_element(&self, elem: &Element) -> gds21::GdsElement {
            self.convert_shape(&elem.inner, &elem.layer_spec).into()
        }
        /// Convert a [Shape] to GDSII Format [gds21::GdsBoundary]
        ///
        /// GDS shapes are flattened vectors of (x,y) coordinates,
        /// and include an explicit repetition of their origin for closure.
        /// So an N-sided polygon is described by a 2*(N+1)-entry vector.
        ///
        pub fn convert_shape(&self, shape: &Shape, layer_spec: &LayerSpec) -> gds21::GdsBoundary {
            let xy = match shape {
                Shape::Rect { p0, p1 } => {
                    let x0 = p0.x as i32;
                    let y0 = p0.y as i32;
                    let x1 = p1.x as i32;
                    let y1 = p1.y as i32;
                    vec![x0, y0, x1, y0, x1, y1, x0, y1, x0, y0]
                }
                Shape::Poly { pts } => {
                    // Flatten our points-vec, converting to 32-bit along the way
                    let mut xy = Vec::new();
                    for p in pts.iter() {
                        xy.push(p.x as i32);
                        xy.push(p.y as i32);
                    }
                    // Add the origin a second time, to "close" the polygon
                    xy.push(pts[0].x as i32);
                    xy.push(pts[0].y as i32);
                    xy
                }
            };
            gds21::GdsBoundary {
                layer: layer_spec.0,
                datatype: layer_spec.1,
                xy,
                elflags: None,
                plex: None,
            }
        }
        /// Convert an [InstArray] to GDS-format [gds21::GdsArrayRef]
        ///
        /// GDS requires three "points" to define an array,
        /// Essentially at its origin and opposite edges
        pub fn convert_array(&self, arr: &InstArray) -> gds21::GdsArrayRef {
            let x0 = arr.p0.x as i32;
            let y0 = arr.p0.y as i32;
            let x1 = x0 + (arr.xpitch * arr.cols + 1) as i32;
            let y1 = y0 + (arr.ypitch * arr.rows + 1) as i32;
            gds21::GdsArrayRef {
                name: arr.cell_name.clone(),
                xy: vec![x0, y0, x1, y0, x0, y1],
                rows: arr.rows as i16,
                cols: arr.cols as i16,
                strans: None, //FIXME!
                elflags: None,
                plex: None,
            }
        }
    }
    impl From<gds21::GdsError> for LayoutError {
        fn from(_e: gds21::GdsError) -> Self {
            LayoutError::Tbd
        }
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
    use super::raw;
    use super::{Assign, LayoutError, Library, Outline, RelZ};

    // FIXME: also need a raw::Abstract, for more-arbitrary-shaped abstract layouts

    /// Abstract-Layout
    #[derive(Debug, Clone)]
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
    impl Abstract {
        /// Convert to a [raw::Cell], just including an Outline
        /// FIXME: also include the pins!
        pub fn to_raw_cell(&self, lib: &Library) -> Result<raw::Cell, LayoutError> {
            // Create our [Outline]s boundary
            let outline = self.outline.to_raw_elem(lib)?;
            // And return a new [raw::Cell]
            Ok(raw::Cell {
                name: self.name.clone(),
                insts: Vec::new(),
                arrays: Vec::new(),
                elems: vec![outline],
            })
        }
    }
    /// Abstract-Layout Port
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
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
    pub struct Port {
        /// Port Name
        pub name: String,
        /// Port Type & Content
        pub kind: PortKind,
    }
    pub enum PortKind {
        /// Flat Scalar Port, e.g. `clk`
        Scalar,
        /// Array-Based Port, e.g. `data[31:0]`
        Array { width: usize },
        /// Instance of a Hierarchical Bundle
        Bundle { bundle_name: String },
    }
    pub struct Bundle {
        pub name: String,
        pub ports: Vec<Port>,
    }
}
/// # Cell View Enumeration
/// All of the ways in which a Cell is represented
pub enum CellView {
    Interface(interface::Bundle),
    Abstract(abstrakt::Abstract),
    Layout(Cell),
    RawLayout(raw::Cell),
}
/// Collection of the Views describing a Cell
pub struct CellViews {
    name: String,
    views: Arena<CellView>,
}

///
/// # Layout Error Enumeration
///
#[derive(Clone, Debug)]
pub enum LayoutError {
    /// Uncategorized Error with Message
    Message(String),
    /// Everything to be categorized
    Tbd,
}
impl LayoutError {
    /// Create a [LayoutError::Message] from anything String-convertible
    fn msg(s: impl Into<String>) -> Self {
        Self::Message(s.into())
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    /// Create a [Stack] used by a number of tests
    fn stack() -> Stack {
        use raw::LayerSpec;
        use Entry::*;

        Stack {
            units: Unit::Nano,
            xpitch: 6600,
            ypitch: 6600,
            boundary_layer: Some(LayerSpec::new(236, 0)),
            layers: vec![
                Layer {
                    index: 1,
                    name: "M1".into(),
                    entries: vec![
                        Gnd(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Pwr(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Gnd(490),
                    ],
                    dir: Dir::Horiz,
                    offset: (0, -245),
                    stream_layer: Some(LayerSpec::new(68, 20)),
                },
                Layer {
                    index: 2,
                    name: "M2".into(),
                    entries: vec![
                        Gnd(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Pwr(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Gnd(490),
                    ],
                    dir: Dir::Vert,
                    offset: (-245, 0),
                    stream_layer: Some(LayerSpec::new(69, 20)),
                },
                Layer {
                    index: 3,
                    name: "M3".into(),
                    entries: vec![
                        Gnd(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Pwr(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Gnd(490),
                    ],
                    dir: Dir::Horiz,
                    offset: (0, -245),
                    stream_layer: Some(LayerSpec::new(70, 20)),
                },
                Layer {
                    index: 4,
                    name: "M4".into(),
                    entries: vec![
                        Gnd(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Pwr(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Gnd(490),
                    ],
                    dir: Dir::Vert,
                    offset: (-245, 0),
                    stream_layer: Some(LayerSpec::new(71, 20)),
                },
            ],
            vias: vec![
                ViaLayer {
                    index: 0,
                    name: "mcon".into(),
                    between: (0, 1),
                    size: Point::new(140, 140),
                    stream_layer: Some(LayerSpec::new(67, 44)),
                },
                ViaLayer {
                    index: 1,
                    name: "via1".into(),
                    between: (1, 2),
                    size: Point::new(140, 140),
                    stream_layer: Some(LayerSpec::new(68, 44)),
                },
                ViaLayer {
                    index: 2,
                    name: "via2".into(),
                    between: (2, 3),
                    size: Point::new(140, 140),
                    stream_layer: Some(LayerSpec::new(69, 44)),
                },
            ],
        }
    }

    /// Create a cell
    #[test]
    fn create_cell() -> Result<(), LayoutError> {
        Cell {
            name: "HereGoes".into(),
            top_layer: 3,
            outline: Outline::rect(5, 5)?,
            instances: Vec::new(),
            assignments: vec![Assign {
                net: "clk".into(),
                at: TrackIntersection {
                    layer: 1,
                    track: 0,
                    at: 1,
                    relz: RelZ::Above,
                },
            }],
            cuts: Vec::new(),
        };
        Ok(())
    }
    /// Create a library
    #[test]
    fn create_lib() -> Result<(), LayoutError> {
        let mut lib = Library::new("HereGoesLib", stack());

        let c = lib.cells.alloc(Cell {
            name: "HereGoes".into(),
            top_layer: 3,
            outline: Outline::rect(5, 5)?,
            instances: Vec::new(),
            assignments: vec![Assign {
                net: "clk".into(),
                at: TrackIntersection {
                    layer: 1,
                    track: 0,
                    at: 1,
                    relz: RelZ::Above,
                },
            }],
            cuts: Vec::new(),
        });
        lib.to_raw_lib()?.to_gds()?.save("test.gds")?;
        Ok(())
    }
    /// Create a cell with instances
    #[test]
    fn create_lib2() -> Result<(), LayoutError> {
        let mut lib = Library::new("InstLib", stack());

        let c2 = lib.cells.alloc(Cell {
            name: "IsInst".into(),
            top_layer: 2,
            outline: Outline::rect(1, 1)?,
            instances: vec![],
            assignments: vec![],
            cuts: Vec::new(),
        });

        let c = lib.cells.alloc(Cell {
            name: "HasInst".into(),
            top_layer: 4,
            outline: Outline::rect(5, 11)?,
            instances: vec![Instance {
                inst_name: "inst1".into(),
                cell_name: "IsInst".into(),
                cell: CellRef::Cell(c2),
                p0: Point::new(1, 2),
                reflect: false,
                angle: None,
            }],
            assignments: vec![Assign {
                net: "clk".into(),
                at: TrackIntersection {
                    layer: 1,
                    track: 0,
                    at: 1,
                    relz: RelZ::Above,
                },
            }],
            cuts: Vec::new(),
        });
        lib.to_raw_lib()?.to_gds()?.save("test_insts.gds")?;
        Ok(())
    }

    /// Create an abstract layout, with its variety of supported port types
    #[test]
    fn create_abstract() -> Result<(), LayoutError> {
        let outline = Outline::rect(11, 11)?;
        let ports = vec![
            abstrakt::Port {
                name: "edge_bot".into(),
                kind: abstrakt::PortKind::Edge {
                    layer: 2,
                    track: 2,
                    side: abstrakt::Side::Bottom,
                },
            },
            abstrakt::Port {
                name: "edge_top".into(),
                kind: abstrakt::PortKind::Edge {
                    layer: 2,
                    track: 4,
                    side: abstrakt::Side::Top,
                },
            },
            abstrakt::Port {
                name: "edge_left".into(),
                kind: abstrakt::PortKind::Edge {
                    layer: 1,
                    track: 1,
                    side: abstrakt::Side::Left,
                },
            },
            abstrakt::Port {
                name: "edge_right".into(),
                kind: abstrakt::PortKind::Edge {
                    layer: 1,
                    track: 5,
                    side: abstrakt::Side::Right,
                },
            },
            abstrakt::Port {
                name: "zfull".into(),
                kind: abstrakt::PortKind::Zfull { track: 3 },
            },
            // abstrakt::Port {
            //     name: "zlocs".into(),
            //     kind: abstrakt::PortKind::Zlocs {
            //         locs: vec![Assign {}],
            //     },
            // },
        ];
        abstrakt::Abstract {
            name: "abstrack".into(),
            outline,
            top_layer: 3,
            ports,
        };
        Ok(())
    }

    /// Create a cell with abstract instances
    #[test]
    fn create_lib3() -> Result<(), LayoutError> {
        let mut lib = Library::new("InstLib", stack());

        let c2 = lib.abstracts.alloc(abstrakt::Abstract {
            name: "IsAbstrakt".into(),
            top_layer: 2,
            outline: Outline::rect(1, 1)?,
            ports: Vec::new(),
        });

        let c = lib.cells.alloc(Cell {
            name: "HasAbstrakts".into(),
            top_layer: 3,
            outline: Outline::rect(5, 5)?,
            instances: vec![
                Instance {
                    inst_name: "inst1".into(),
                    cell_name: "IsAbstrakt".into(),
                    cell: CellRef::Abstract(c2),
                    p0: Point::new(0, 0),
                    reflect: false,
                    angle: None,
                },
                Instance {
                    inst_name: "inst2".into(),
                    cell_name: "IsAbstrakt".into(),
                    cell: CellRef::Abstract(c2),
                    p0: Point::new(2, 2),
                    reflect: false,
                    angle: None,
                },
                Instance {
                    inst_name: "inst4".into(),
                    cell_name: "IsAbstrakt".into(),
                    cell: CellRef::Abstract(c2),
                    p0: Point::new(4, 4),
                    reflect: false,
                    angle: None,
                },
            ],
            assignments: vec![Assign {
                net: "clk".into(),
                at: TrackIntersection {
                    layer: 3,
                    track: 11,
                    at: 11,
                    relz: RelZ::Above,
                },
            }],
            cuts: Vec::new(),
        });
        lib.to_raw_lib()?.to_gds()?.save("test_abstracts.gds")?;
        Ok(())
    }
}

/// # Cell Reference Enumeration 
/// Used for enumerating the different types of things an [Instance] may refer to 
#[derive(Debug, Clone)]
pub enum CellRef {
    Cell(Id<Cell>),
    Abstract(Id<abstrakt::Abstract>),
    Name(String),
}
/// Trait for accessing three-dimensional [Outline] data from several views of Layouts
trait HasOutline: Debug {
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
/// Placeholder for Elements to be implemented
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Tbd;
