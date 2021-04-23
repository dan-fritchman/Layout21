//! 
//! # Raw Layout 
//! 
//! The most general, most flexible representation layer in layout21. 
//! Consists of geometric primitives and instances of other layout cells, 
//! much akin to nearly any legacy layout system. 
//! 
//! Conversion to GDSII is supported via the [Library::to_gds] method. 
//! Import from GDSII, LEF, and other industry formats remains WIP. 
//! 
use super::*;
use gds21;

/// Raw Abstract-Layout
/// Contains geometric [Element]s generally representing pins and blockages
/// Does not contain instances, arrays, or layout-implementation details
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Abstract {
    /// Cell Name
    pub name: String,
    /// Primitive Elements
    pub elems: Vec<Element>,
}

/// # Raw Layout Library  
/// A collection of cell-definitions and sub-library definitions
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
        GdsConverter::convert(self)
    }
}
/// Raw-Layout Cell Definition
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
/// # Per-Layer Datatype Specification
/// Includes the datatypes used for each category of element on layer `layernum`
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DataTypeMap {
    /// Layer Number
    pub layernum: i16,
    /// Drawing (Geometry) DataType Value
    pub drawing: Option<i16>,
    /// Text DataType Value
    pub text: Option<i16>,
    /// Any Other DataType Values
    pub other: HashMap<String, i16>,
}
/// # Primitive Geometric Element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Element {
    /// Net Name
    pub net: Option<String>,
    /// Layer
    pub layer: DataTypeMap,
    /// Shape
    pub inner: Shape,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Shape {
    Rect { p0: Point, p1: Point },
    Poly { pts: Vec<Point> },
}
impl Shape {
    /// Retrieve our "origin", or first [Point]
    pub fn point0(&self) -> &Point {
        match *self {
            Shape::Rect { ref p0, p1: _ } => p0,
            Shape::Poly { ref pts } => &pts[0],
        }
    }
    /// Calculate our center-point
    pub fn center(&self) -> Point {
        match *self {
            Shape::Rect { ref p0, ref p1 } => Point::new((p0.x + p1.x) / 2, (p0.y + p1.y) / 2),
            Shape::Poly { pts: _ } => {
                unimplemented!("Shape::Poly::center");
            }
        }
    }
    /// Indicate whether this shape is (more or less) horizontal or vertical
    pub fn orientation(&self) -> Dir {
        match *self {
            Shape::Rect { ref p0, ref p1 } => {
                if (p1.x - p0.x).abs() < (p1.y - p0.y).abs() {
                    return Dir::Vert;
                }
                Dir::Horiz
            }
            Shape::Poly { pts: _ } => {
                unimplemented!("Shape::Poly::orientation");
            }
        }
    }
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GdsConverter {
    pub lib: Library,
}
impl GdsConverter {
    pub fn convert(lib: Library) -> LayoutResult<gds21::GdsLibrary> {
        Self { lib }.convert_all()
    }
    fn convert_all(self) -> LayoutResult<gds21::GdsLibrary> {
        if self.lib.libs.len() > 0 {
            return Err(LayoutError::msg("No nested libraries to GDS (yet)"));
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
            .collect::<Result<Vec<_>, _>>()?;
        Ok(lib)
    }
    /// Convert a [Cell] to a [gds21::GdsStruct] cell-definition
    fn convert_cell(&self, cell: &Cell) -> LayoutResult<gds21::GdsStruct> {
        let mut elems = Vec::new();
        for inst in cell.insts.iter() {
            elems.push(self.convert_instance(inst).into());
        }
        for arr in cell.arrays.iter() {
            elems.push(self.convert_array(arr).into());
        }
        for elem in cell.elems.iter() {
            for gdselem in self.convert_element(elem)?.into_iter() {
                elems.push(gdselem);
            }
        }
        let mut s = gds21::GdsStruct::new(&cell.name);
        s.elems = elems;
        Ok(s)
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
    /// Convert an [Element] into one or more [gds21::GdsElement]
    ///
    /// Our [Element]s often correspond to more than one GDSII element,
    /// notably in the case in which a polygon is annotated with a net-name.
    /// Here, the net-name is an attribute of the polygon [Element].
    /// In GDSII, text is "free floating" as a separate element.
    ///
    /// GDS shapes are flattened vectors of (x,y) coordinates,
    /// and include an explicit repetition of their origin for closure.
    /// So an N-sided polygon is described by a 2*(N+1)-entry vector.
    ///
    pub fn convert_element(&self, elem: &Element) -> LayoutResult<Vec<gds21::GdsElement>> {
        let datatype = elem
            .layer
            .drawing
            .ok_or(LayoutError::msg("Drawing Layer Not Defined"))?;
        let xy = match &elem.inner {
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
        // Initialize our vector of elements with the shape
        let mut gds_elems = vec![gds21::GdsBoundary {
            layer: elem.layer.layernum,
            datatype,
            xy,
            ..Default::default()
        }
        .into()];
        // If there's an assigned net, create a corresponding text-element
        if let Some(name) = &elem.net {
            let texttype = elem
                .layer
                .text
                .ok_or(LayoutError::msg("Text Layer Not Defined"))?;

            // Text is placed in the shape's (at least rough) center
            let loc = elem.inner.center();
            // Rotate that text 90 degrees for mostly-vertical shapes
            let strans = match elem.inner.orientation() {
                Dir::Horiz => None,
                Dir::Vert => Some(gds21::GdsStrans {
                    angle: Some(90.0),
                    ..Default::default()
                }),
            };
            gds_elems.push(
                gds21::GdsTextElem {
                    string: name.into(),
                    layer: elem.layer.layernum,
                    texttype,
                    xy: vec![loc.x as i32, loc.y as i32],
                    strans,
                    ..Default::default()
                }
                .into(),
            )
        }
        Ok(gds_elems)
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
