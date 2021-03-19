use serde::{Deserialize, Serialize};

/// Distance Units Enumeration
#[derive(Clone, Copy, Debug)]
pub enum Unit {
    Micro, // Micrometers, or microns for we olde folke
    Nano,  // Nanometers
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
pub struct Stack {
    pub units: Unit,        // Measurement units
    pub xpitch: usize,      // Primitive cell horizontal unit-pitch, denominated in `units`
    pub ypitch: usize,      // Primitive cell vertical unit-pitch, denominated in `units`
    pub layers: Vec<Layer>, // Set of defined layers
}
impl Stack {
    pub fn get_unit(&self, layer: usize) -> Result<raw::Cell, LayoutError> {
        self.layers[layer].raw_unit(self)
    }
}

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
            name: format!("{}::unit", self.name.clone()),
            insts: Vec::new(),
            arrays: Vec::new(),
            elems,
        })
    }
}
/// Assignment of a net onto a track
pub struct Assign {
    net: String,  // Net Name
    layer: usize, // Layer Index
    track: usize, // Track Index
    cross: Cross, // Crossing-layer track index, either above or below `layer`
}
/// This should really be private to `Assign`, figure out how to organize it as such
pub enum Cross {
    Above(usize), // Cross at this track index on layer *above* `layer`
    Below(usize), // Cross at this track index on layer *below* `layer`
}
/// # Layout Cell
///
/// A combination of lower-level cell instances and net-assignments to tracks.
///
pub struct Cell<'a> {
    name: String,             // Cell Name
    stack: &'a Stack,         // Reference to the z-stack
    top_layer: usize,         // Top-layer index
    outline: Outline,         // Outline shape, counted in x and y pitches of `stack`
    instances: Vec<Tbd>,      // Layout-Abstract Instances
    assignments: Vec<Assign>, // Net-to-track assignments
    cuts: Vec<Tbd>,           // Track cuts
}
impl Cell<'_> {
    /// Create a new raw::Library with ourselves as the notional "top" cell
    pub fn to_raw_lib(&self) -> Result<raw::Library, LayoutError> {
        let mut lib = raw::Library::new(self.name.clone(), self.stack.units);
        // Collect up unit-cells on each layer
        for layer in 0..self.top_layer {
            let unit = self.stack.get_unit(layer)?;
            lib.cells.push(unit);
        }
        lib.cells.push(self.to_raw_cell()?);
        Ok(lib)
    }
    /// Convert to a raw layout cell
    pub fn to_raw_cell(&self) -> Result<raw::Cell, LayoutError> {
        if self.outline.x.len() > 1 {
            // Non-rectangular outline; not supported yet
            return Err(LayoutError::Tbd);
        };
        let rows = self.outline.x[0];
        let cols = self.outline.y[0];

        // Collect up arrays of unit-cells on each layer
        let mut arrays: Vec<raw::InstArray> = Vec::new();
        for layer in 0..self.top_layer {
            let unit = self.stack.get_unit(layer)?;
            let a = raw::InstArray {
                inst_name: format!("{}.array", unit.name),
                cell_name: unit.name.clone(),
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
        Ok(raw::Cell {
            name: self.name.clone(),
            insts: Vec::new(),
            arrays,
            elems: Vec::new(),
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
pub struct Outline {
    x: Vec<usize>,
    y: Vec<usize>,
}
impl Outline {
    /// Outline constructor, with inline checking for validity of `x` & `y` vectors
    pub fn new(x: Vec<usize>, y: Vec<usize>) -> Result<Self, LayoutError> {
        // Check that x and y are of compatible lengths
        if x.len() != y.len() {
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
#[derive(Debug)]
pub struct Point {
    x: isize,
    y: isize,
}
impl Point {
    pub fn new(x: isize, y: isize) -> Self {
        Self {
            x: x.into(),
            y: y.into(),
        }
    }
}
#[derive(Debug)]
pub struct Tbd {}

/// "Raw" Layout
pub mod raw {
    use super::*;
    use gds21;
    use gds21::{GdsBoundary, GdsElement, GdsLibrary, GdsStruct, GdsStructRef};

    #[derive(Debug)]
    pub struct Library {
        pub name: String,       // Library Name
        pub units: Unit,        // Distance Units
        pub date_info: Tbd,     // Creation & Modification Time Info
        pub libs: Vec<Library>, // Sub-Library Definitions
        pub cells: Vec<Cell>,   // Cell Definitions
    }
    impl Library {
        /// Create a new and empty Library
        pub fn new(name: impl Into<String>, units: Unit) -> Self {
            Self {
                name: name.into(),
                units,
                date_info: Tbd {}, // FIXME!
                libs: Vec::new(),
                cells: Vec::new(),
            }
        }
        /// Convert to a Gds Library
        pub fn to_gds(&self) -> gds21::GdsLibrary {
            if self.libs.len() > 0 {
                panic! {"no nested libraries to GDS, yet "}
            }
            let units: gds21::GdsUnits = match self.units {
                Unit::Nano => gds21::GdsUnits {
                    dbu: 1e-9,
                    uu: 1e-3,
                }, // FIXME!
                Unit::Micro => gds21::GdsUnits {
                    dbu: 1e-9,
                    uu: 1e-3,
                }, // FIXME!
            };
            let mut structs = Vec::new();
            for c in self.cells.iter() {
                structs.push(c.to_gds());
            }
            let lib = gds21::GdsLibrary {
                name: self.name.clone(),
                version: 3,             // FIXME!
                date_info: vec![0; 12], // FIXME: real date-time
                units,
                structs,
                ..Default::default()
            };
            lib
        }
    }
    #[derive(Debug)]
    pub struct Cell {
        pub name: String,           // Cell Name
        pub insts: Vec<Instance>,   // Cell Instances
        pub arrays: Vec<InstArray>, // Instance Arrays
        pub elems: Vec<Element>,    // Primitive Elements
    }
    impl Cell {
        pub fn to_gds(&self) -> GdsStruct {
            let mut elems = Vec::new();
            for inst in self.insts.iter() {
                elems.push(GdsElement::GdsStructRef(inst.to_gds()));
            }
            for arr in self.arrays.iter() {
                elems.push(GdsElement::GdsArrayRef(arr.to_gds()));
            }
            for s in self.elems.iter() {
                elems.push(s.to_gds());
            }
            GdsStruct {
                name: self.name.clone(),
                date_info: vec![0; 12], // FIXME: real date-time
                elems,
            }
        }
    }
    #[derive(Debug)]
    pub struct Instance {
        pub inst_name: String,
        pub cell_name: String,
        pub p0: Point,
        pub reflect: bool,
        pub angle: Option<f64>,
    }
    impl Instance {
        pub fn to_gds(&self) -> GdsStructRef {
            GdsStructRef {
                name: self.cell_name.clone(),
                xy: vec![self.p0.x as i32, self.p0.y as i32],
                strans: None, //FIXME!
                elflags: None,
                plex: None,
            }
        }
    }
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
        pub fn to_gds(&self) -> gds21::GdsArrayRef {
            // GDS requires three "points" to define an array,
            // Essentially at its origin and opposite edges
            let x0 = self.p0.x as i32;
            let y0 = self.p0.y as i32;
            let x1 = x0 + (self.xpitch * self.cols+1) as i32;
            let y1 = y0 + (self.ypitch * self.rows+1) as i32;
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
        pub fn to_gds(&self) -> GdsElement {
            self.inner.to_gds(&self.layer_spec).into()
        }
    }
    #[derive(Debug)]
    pub enum Shape {
        Rect { p0: Point, p1: Point },
        Poly { pts: Vec<Point> },
    }
    impl Shape {
        pub fn to_gds(&self, layer_spec: &(i16, i16)) -> GdsBoundary {
            // Gds shapes are flattened vectors of (x,y) coordinates,
            // and include an explicit repetition of their origin for closure.
            // So an N-sided polygon is described by a 2*(N+1)-entry vector.
            let xy = match self {
                Shape::Rect { p0, p1 } => {
                    let x0 = p0.x as i32;
                    let y0 = p0.y as i32;
                    let x1 = p1.x as i32;
                    let y1 = p1.y as i32;
                    vec![x0, y0, x1, y0, x1, y1, x0, y1, x0, y0]
                }
                Shape::Poly { pts } => unimplemented!("FIXME!"),
            };
            GdsBoundary {
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
///
/// # Layout Error Enumeration
///
#[derive(Clone, Debug)]
pub enum LayoutError {
    Tbd, // Everything to be categorized
    Message(String),
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() -> Result<(), LayoutError> {
        use Entry::*;

        let s = Stack {
            units: Unit::Nano,
            xpitch: 6600,
            ypitch: 6600,
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
            ],
        };

        let c = Cell {
            name: "HereGoes".into(),
            stack: &s,
            top_layer: 2,
            outline: Outline::rect(3, 3)?,
            instances: Vec::new(),
            assignments: vec![Assign {
                net: "clk".into(),
                layer: 1,
                track: 0,
                cross: Cross::Above(1),
            }],
            cuts: Vec::new(),
        };
        c.to_raw_lib()?.to_gds().save("test.gds")?;
        Ok(())
    }
}
