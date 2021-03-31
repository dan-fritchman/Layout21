use serde::{Deserialize, Serialize};

/// Distance Units Enumeration
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
pub enum Dir {
    Horiz,
    Vert,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
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
/// The metal and base layer stack
#[derive(Debug, Clone)]
pub struct Stack {
    /// Measurement units
    pub units: Unit,
    /// Primitive cell horizontal unit-pitch, denominated in `units`
    pub xpitch: usize,
    /// Primitive cell vertical unit-pitch, denominated in `units`
    pub ypitch: usize,
    /// Layer used for cell outlines/ boundaries
    pub boundary_layer: Option<(isize, isize)>,
    /// Set of defined layers
    pub layers: Vec<Layer>,
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
#[derive(Debug, Clone)]
pub struct Layer {
    pub index: usize,
    pub name: String,
    pub dir: Dir,
    pub entries: Vec<Entry>,
    pub offset: (isize, isize),
    pub stream_layer: Option<(i16, i16)>,
}
impl Layer {
    /// Flatten our [entries] into a vector
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
        use raw::{Element, Shape};
        let stream_layer = match self.stream_layer {
            Some(s) => s,
            None => return Err(LayoutError::Message("No Stream-Layer Defined".into())),
        };
        let mut elems: Vec<Element> = Vec::new();
        let mut cursor = match self.dir {
            Dir::Horiz => self.offset.1,
            Dir::Vert => self.offset.0,
        };
        let width = match self.dir {
            Dir::Horiz => stack.xpitch,
            Dir::Vert => stack.ypitch,
        };
        for e in self.flat_entries().iter() {
            match e {
                Entry::Gap(d) => cursor += *d as isize,
                Entry::Pwr(d) | Entry::Gnd(d) | Entry::Sig(d) => {
                    let e = match self.dir {
                        Dir::Horiz => Element {
                            layer_spec: stream_layer,
                            inner: Shape::Rect {
                                p0: Point { x: 0, y: cursor },
                                p1: Point {
                                    x: width as isize,
                                    y: cursor + *d as isize,
                                },
                            },
                        },
                        Dir::Vert => Element {
                            layer_spec: stream_layer,
                            inner: Shape::Rect {
                                p0: Point { x: cursor, y: 0 },
                                p1: Point {
                                    x: cursor + *d as isize,
                                    y: width as isize,
                                },
                            },
                        },
                    };
                    elems.push(e);
                    cursor += *d as isize;
                }
                Entry::Pat(_) => {
                    return Err(LayoutError::Message(
                        "Internal Error: failed to flatten Patterns".into(),
                    ))
                }
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
/// Assignment of a net onto a track-intersection
#[derive(Debug, Clone)]
pub struct Assign {
    /// Net Name
    net: String,
    /// Layer Index
    layer: usize,
    /// Track Index
    track: usize,
    /// Intersecting Track Index
    at: usize,
    /// Whether `at` refers to the track-indices above or below
    relz: RelZ,
}
/// Relative Z-Axis Reference to one Layer `Above` or `Below` another
#[derive(Debug, Clone)]
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
pub struct Library<'a> {
    /// Library Name
    pub name: String,
    /// Reference to the z-stack
    pub stack: &'a Stack,
    /// Cell Names
    pub cell_names: Vec<String>,
    /// Abstracts
    pub abstracts: Vec<abstrakt::Cell<'a>>,
    /// Cell Implementations
    pub cells: Vec<Cell<'a>>,
    /// Sub-Libraries
    pub libs: Vec<Library<'a>>,
}
impl<'a> Library<'a> {
    /// Create a new and initially empty [Library]
    pub fn new(name: impl Into<String>, stack: &'a Stack) -> Self {
        Self {
            name: name.into(),
            stack,
            cell_names: Vec::new(),
            abstracts: Vec::new(),
            cells: Vec::new(),
            libs: Vec::new(),
        }
    }
    /// Convert to a [raw::Library]
    pub fn to_raw_lib(&self) -> Result<raw::Library, LayoutError> {
        let mut lib = raw::Library::new(self.name.clone(), self.stack.units);
        // Collect up unit-cells on each layer
        for layer in self.stack.layers.iter() {
            let unit = layer.raw_unit(&self.stack)?;
            lib.cells.push(unit);
        }
        // Convert each defined [Cell] to a [raw::Cell]
        for cell in self.cells.iter() {
            lib.cells.push(cell.to_raw_cell()?);
        }
        // And convert each (un-implemented) Abstract as a boundary
        for abs in self.abstracts.iter() {
            // Check whether the same name is already defined
            for cell in lib.cells.iter() {
                if abs.name == cell.name {
                    continue;
                }
            }
            lib.cells.push(abs.to_raw_cell()?);
        }
        Ok(lib)
    }
}
/// # Layout Cell
///
/// A combination of lower-level cell instances and net-assignments to tracks.
///
#[derive(Debug, Clone)]
pub struct Cell<'a> {
    name: String,             // Cell Name
    stack: &'a Stack,         // Reference to the z-stack
    top_layer: usize,         // Top-layer index
    outline: Outline,         // Outline shape, counted in x and y pitches of `stack`
    instances: Vec<Instance>, // Layout-Abstract Instances
    assignments: Vec<Assign>, // Net-to-track assignments
    cuts: Vec<Tbd>,           // Track cuts
}
impl Cell<'_> {
    /// Convert to a raw layout cell
    pub fn to_raw_cell(&self) -> Result<raw::Cell, LayoutError> {
        if self.outline.x.len() > 1 {
            return Err(LayoutError::Message(
                "Non-rectangular outline; not supported yet".into(),
            ));
        };
        let rows = self.outline.x[0];
        let cols = self.outline.y[0];

        // Collect up arrays of unit-cells on each layer
        let mut arrays: Vec<raw::InstArray> = Vec::new();
        for layer in self.stack.layers.iter() {
            let unit_name = layer.get_unit_name();
            let a = raw::InstArray {
                inst_name: format!("{}.array", unit_name),
                cell_name: unit_name,
                rows,
                cols,
                xpitch: self.stack.xpitch,
                ypitch: self.stack.ypitch,
                p0: Point { x: 0, y: 0 },
                reflect: false,
                angle: None,
            };
            arrays.push(a);
        }
        // Scale the location of each instance by our pitches
        let scale = (self.stack.xpitch as isize, self.stack.ypitch as isize);
        let mut instances = self.instances.clone();
        for inst in instances.iter_mut() {
            inst.p0 = inst.p0.scale(scale.0, scale.1);
        }
        Ok(raw::Cell {
            name: self.name.clone(),
            insts: instances, // Note instances are of the same type, but use Points of different units.
            arrays,
            elems: Vec::new(), // FIXME! cut up metals and such
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
}
#[derive(Debug, Clone)]
pub struct Point {
    x: isize,
    y: isize,
}
impl Point {
    /// Create a new [Point] from (x,y) coordinates
    pub fn new(x: isize, y: isize) -> Self {
        Self {
            x: x.into(),
            y: y.into(),
        }
    }
    /// Create a new point scaled by `x` in the x-dimension and by `y` in the y-dimension
    pub fn scale(&self, x: isize, y: isize) -> Point {
        Point {
            x: x * self.x,
            y: y * self.y,
        }
    }
}
#[derive(Debug, Clone)]
pub struct Tbd {}

/// "Raw" Layout
pub mod raw {
    use super::*;
    use gds21;

    #[derive(Debug, Default)]
    /// # Raw Layout Library  
    /// A collection of cell-definitions and sub-library definitions
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
        /// Convert to a Gds Library
        pub fn to_gds(&self) -> gds21::GdsLibrary {
            if self.libs.len() > 0 {
                panic! {"no nested libraries to GDS, yet "}
            }
            let units: gds21::GdsUnits = match self.units {
                Unit::Nano => gds21::GdsUnits::new(1e-3, 1e-9),
                Unit::Micro => gds21::GdsUnits::new(1e-3, 1e-6),
            };
            // Create a new Gds Library
            let mut lib = gds21::GdsLibrary::new(&self.name, units);
            // And convert each of our `cells` into its `structs`
            lib.structs = self.cells.iter().map(|c| c.to_gds()).collect();
            lib
        }
    }
    /// Raw-Layout Cell Definition
    #[derive(Debug)]
    pub struct Cell {
        pub name: String,           // Cell Name
        pub insts: Vec<Instance>,   // Cell Instances
        pub arrays: Vec<InstArray>, // Instance Arrays
        pub elems: Vec<Element>,    // Primitive Elements
    }
    impl Cell {
        pub fn to_gds(&self) -> gds21::GdsStruct {
            let mut elems = Vec::new();
            for inst in self.insts.iter() {
                elems.push(gds21::GdsElement::GdsStructRef(inst.to_gds()));
            }
            for arr in self.arrays.iter() {
                elems.push(gds21::GdsElement::GdsArrayRef(arr.to_gds()));
            }
            for s in self.elems.iter() {
                elems.push(s.to_gds());
            }
            let mut s = gds21::GdsStruct::new(&self.name);
            s.elems = elems;
            s
        }
    }
    impl Instance {
        /// Convert to a GDS instance, AKA [gds21::GdsStructRef]
        pub fn to_gds(&self) -> gds21::GdsStructRef {
            gds21::GdsStructRef {
                name: self.cell_name.clone(),
                xy: vec![self.p0.x as i32, self.p0.y as i32],
                strans: None, //FIXME!
                elflags: None,
                plex: None,
            }
        }
    }
    /// Array of Instances
    ///
    /// Arrays are two-dimensional, of identical instances of the same Cell.
    ///
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
    impl InstArray {
        /// Convert an Instance Array to GDS Format [gds21::GdsArrayRef]
        ///
        /// GDS requires three "points" to define an array,
        /// Essentially at its origin and opposite edges
        pub fn to_gds(&self) -> gds21::GdsArrayRef {
            let x0 = self.p0.x as i32;
            let y0 = self.p0.y as i32;
            let x1 = x0 + (self.xpitch * self.cols + 1) as i32;
            let y1 = y0 + (self.ypitch * self.rows + 1) as i32;
            gds21::GdsArrayRef {
                name: self.cell_name.clone(),
                xy: vec![x0, y0, x1, y0, x0, y1],
                rows: self.rows as i16,
                cols: self.cols as i16,
                strans: None, //FIXME!
                elflags: None,
                plex: None,
            }
        }
    }
    /// An instance of a primitive element,
    /// e.g. a geometric shape or piece of text
    #[derive(Debug)]
    pub struct Element {
        pub layer_spec: (i16, i16),
        pub inner: Shape,
    }
    impl Element {
        pub fn to_gds(&self) -> gds21::GdsElement {
            self.inner.to_gds(&self.layer_spec).into()
        }
    }
    #[derive(Debug)]
    pub enum Shape {
        Rect { p0: Point, p1: Point },
        Poly { pts: Vec<Point> },
    }
    impl Shape {
        /// Convert a [Shape] to GDSII Format
        ///
        /// GDS shapes are flattened vectors of (x,y) coordinates,
        /// and include an explicit repetition of their origin for closure.
        /// So an N-sided polygon is described by a 2*(N+1)-entry vector.
        ///
        pub fn to_gds(&self, layer_spec: &(i16, i16)) -> gds21::GdsBoundary {
            let xy = match self {
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
    use super::{Assign, LayoutError, Outline, RelZ, Stack};

    /// Abstract-Layout Cell
    #[derive(Debug, Clone)]
    pub struct Cell<'a> {
        /// Cell Name
        pub name: String,
        /// Reference to the Stack
        pub stack: &'a Stack,
        /// Outline in "Tetris-Shapes"
        pub outline: Outline,
        /// Top Metal Layer
        pub top_layer: usize,
        /// Ports
        pub ports: Vec<Port>,
    }
    impl Cell<'_> {
        /// Convert to a [raw::Cell], just including an Outline
        pub fn to_raw_cell(&self) -> Result<raw::Cell, LayoutError> {
            let layer_spec = match self.stack.boundary_layer {
                Some((l1, l2)) => (l1 as i16, l2 as i16),
                None => {
                    return Err(LayoutError::Message(
                        "Cannot Convert Abstract to Raw without Boundary Layer".into(),
                    ))
                }
            };
            // Create an array of Outline-Points
            let pts = self.outline.points();
            // Scale them to our pitches
            let pitch = (self.stack.xpitch as isize, self.stack.ypitch as isize);
            let pts = pts.iter().map(|p| p.scale(pitch.0, pitch.1)).collect();
            // Create the Outline Element
            let outline = raw::Element {
                layer_spec,
                inner: raw::Shape::Poly { pts },
            };
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
pub enum CellView<'a> {
    Interface(interface::Bundle),
    Abstract(abstrakt::Cell<'a>),
    Layout(Cell<'a>),
    RawLayout(raw::Cell),
}
/// Collection of the Views describing a Cell
pub struct CellViews<'a> {
    name: String,
    views: Vec<CellView<'a>>,
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
#[cfg(test)]
mod tests {
    use super::*;

    /// Create a [Stack] used by a number of tests
    fn stack() -> Stack {
        use Entry::*;

        Stack {
            units: Unit::Nano,
            xpitch: 6600,
            ypitch: 6600,
            boundary_layer: Some((236, 0)),
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
                    ],
                    dir: Dir::Horiz,
                    offset: (0, -245),
                    stream_layer: Some((68, 20)),
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
                    ],
                    dir: Dir::Vert,
                    offset: (-245, 0),
                    stream_layer: Some((69, 20)),
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
                    ],
                    dir: Dir::Horiz,
                    offset: (0, -245),
                    stream_layer: Some((70, 20)),
                },
            ],
        }
    }

    /// Create a cell
    #[test]
    fn create_cell() -> Result<(), LayoutError> {
        let s = stack();
        let c = Cell {
            name: "HereGoes".into(),
            stack: &s,
            top_layer: 3,
            outline: Outline::rect(5, 5)?,
            instances: Vec::new(),
            assignments: vec![Assign {
                net: "clk".into(),
                layer: 1,
                track: 0,
                at: 1,
                relz: RelZ::Above,
            }],
            cuts: Vec::new(),
        };
        Ok(())
    }
    /// Create a library
    #[test]
    fn create_lib() -> Result<(), LayoutError> {
        let s = stack();
        let c = Cell {
            name: "HereGoes".into(),
            stack: &s,
            top_layer: 3,
            outline: Outline::rect(5, 5)?,
            instances: Vec::new(),
            assignments: vec![Assign {
                net: "clk".into(),
                layer: 1,
                track: 0,
                at: 1,
                relz: RelZ::Above,
            }],
            cuts: Vec::new(),
        };
        let mut lib = Library::new("HereGoesLib", &s);
        lib.cells.push(c);
        lib.to_raw_lib()?.to_gds().save("test.gds")?;
        Ok(())
    }
    /// Create a cell with instances
    #[test]
    fn create_lib2() -> Result<(), LayoutError> {
        let s = stack();
        let c = Cell {
            name: "HasInst".into(),
            stack: &s,
            top_layer: 3,
            outline: Outline::rect(5, 5)?,
            instances: vec![Instance {
                inst_name: "inst1".into(),
                cell_name: "IsInst".into(),
                p0: Point::new(0, 0),
                reflect: false,
                angle: None,
            }],
            assignments: vec![Assign {
                net: "clk".into(),
                layer: 1,
                track: 0,
                at: 1,
                relz: RelZ::Above,
            }],
            cuts: Vec::new(),
        };
        let c2 = Cell {
            name: "IsInst".into(),
            stack: &s,
            top_layer: 2,
            outline: Outline::rect(1, 1)?,
            instances: vec![],
            assignments: vec![],
            cuts: Vec::new(),
        };
        let mut lib = Library::new("InstLib", &s);
        lib.cells.push(c);
        lib.cells.push(c2);
        lib.to_raw_lib()?.to_gds().save("test_insts.gds")?;
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
        abstrakt::Cell {
            name: "abstrack".into(),
            stack: &stack(),
            outline,
            top_layer: 3,
            ports,
        };
        Ok(())
    }

    /// Create a cell with abstract instances
    #[test]
    fn create_lib3() -> Result<(), LayoutError> {
        let s = stack();
        let c = Cell {
            name: "HasAbstrakts".into(),
            stack: &s,
            top_layer: 3,
            outline: Outline::rect(5, 5)?,
            instances: vec![
                Instance {
                    inst_name: "inst1".into(),
                    cell_name: "IsAbstrakt".into(),
                    p0: Point::new(0, 0),
                    reflect: false,
                    angle: None,
                },
                Instance {
                    inst_name: "inst2".into(),
                    cell_name: "IsAbstrakt".into(),
                    p0: Point::new(2, 2),
                    reflect: false,
                    angle: None,
                },
                Instance {
                    inst_name: "inst4".into(),
                    cell_name: "IsAbstrakt".into(),
                    p0: Point::new(4, 4),
                    reflect: false,
                    angle: None,
                },
            ],
            assignments: vec![],
            cuts: Vec::new(),
        };
        let c2 = abstrakt::Cell {
            name: "IsAbstrakt".into(),
            stack: &s,
            top_layer: 2,
            outline: Outline::rect(1, 1)?,
            ports: Vec::new(),
        };
        let l = Library {
            name: "InstLib".into(),
            stack: &s,
            cell_names: vec![],
            abstracts: vec![c2],
            cells: vec![c],
            libs: vec![],
        };
        l.to_raw_lib()?.to_gds().save("test_abstracts.gds")?;
        Ok(())
    }
}
