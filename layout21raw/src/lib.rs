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

// Std-Lib
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::hash::Hash;

// Crates.io
use serde::{Deserialize, Serialize};
use slotmap::{new_key_type, SlotMap};

// Local imports
use gds21;
use layout21protos as proto;

// Re-exports
pub mod gds;
pub use gds::{GdsConverter, GdsImporter};

#[cfg(test)]
mod tests;

// Create key-types for each internal type stored in [SlotMap]s
new_key_type! {
    /// Keys for [Element] entries
    pub struct ElementKey;
    /// Keys for [Layer] entries
    pub struct LayerKey;
    /// Keys for [Cell] entries
    pub struct CellKey;
    /// Keys for [abstrakt::Abstract] entries
    pub struct AbstractKey;
    /// Keys for [CellView] entries
    pub struct CellViewKey;
}
/// LayoutError-Specific Result Type
pub type LayoutResult<T> = Result<T, LayoutError>;

///
/// # Layout Error Enumeration
///
#[derive(Debug)]
pub enum LayoutError {
    /// Error Exporting to Foreign Format
    Export(String),
    /// Error Importing from Foreign Format
    Import {
        message: String,
        stack: Vec<ImportContext>,
    },
    /// Validation of input data
    Validation,
    /// Boxed External Errors
    Boxed(Box<dyn std::error::Error>),
    /// Uncategorized Error with Message
    Str(String),
    /// Everything to be categorized
    Tbd,
}
impl LayoutError {
    /// Create a [LayoutError::Message] from anything String-convertible
    pub fn msg(s: impl Into<String>) -> Self {
        Self::Str(s.into())
    }
}
impl From<String> for LayoutError {
    fn from(s: String) -> Self {
        Self::Str(s)
    }
}
impl From<&str> for LayoutError {
    fn from(s: &str) -> Self {
        Self::Str(s.to_string())
    }
}
impl From<std::num::TryFromIntError> for LayoutError {
    fn from(e: std::num::TryFromIntError) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl From<gds21::GdsError> for LayoutError {
    fn from(e: gds21::GdsError) -> Self {
        Self::Boxed(Box::new(e))
    }
}
/// Enumerated conversion contexts
/// Generally used for error reporting
#[derive(Debug, Clone)]
pub enum ImportContext {
    Library(String),
    Cell(String),
    Instance(String),
    Array(String),
    Units,
    Geometry,
    Unknown,
}

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

/// # Point in two-dimensional layout-space
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct Point {
    pub x: isize,
    pub y: isize,
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
    pub fn coord(&self, dir: Dir) -> isize {
        match dir {
            Dir::Horiz => self.x,
            Dir::Vert => self.y,
        }
    }
}
/// Direction Enumeration
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum Dir {
    Horiz,
    Vert,
}
impl Dir {
    /// Whichever direction we are, return the other one.
    pub fn other(&self) -> Self {
        match self {
            Self::Horiz => Self::Vert,
            Self::Vert => Self::Horiz,
        }
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

/// Layer Set & Manager
/// Keep track of active layers, and index them by number (FIXME: and name)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Layers {
    pub slots: SlotMap<LayerKey, Layer>,
    pub nums: HashMap<i16, LayerKey>,
}
impl Layers {
    /// Add a [Layer] to our slot-map and number-map, and
    pub fn add(&mut self, layer: Layer) -> LayerKey {
        let num = layer.layernum;
        let key = self.slots.insert(layer);
        self.nums.insert(num, key.clone());
        key
    }
    /// Get a reference to [LayerKey] number `num`
    fn keynum(&self, num: i16) -> Option<&LayerKey> {
        self.nums.get(&num)
    }
    /// Get a reference to [Layer] number `num`
    pub fn num(&self, num: i16) -> Option<&Layer> {
        let key = self.nums.get(&num)?;
        self.slots.get(*key)
    }
    /// Get a reference to [Layer] from [LayerKey] `key`
    pub fn get(&self, key: LayerKey) -> Option<&Layer> {
        self.slots.get(key)
    }
    /// Get the ([LayerKey], [LayerPurpose]) objects for numbers (`layernum`, `purposenum`) if present.
    /// Inserts a new [Layer] if `layernum` is not present.
    /// Returns `LayerPurpose::Other(purposenum)` if `purposenum` is not present on that layer.
    fn get_or_insert(
        &mut self,
        layernum: i16,
        purposenum: i16,
    ) -> LayoutResult<(LayerKey, LayerPurpose)> {
        // Get the [LayerKey] for `layernum`, creating the [Layer] if it doesn't exist.
        let key = match self.keynum(layernum) {
            Some(key) => key.clone(),
            None => self.add(Layer::new(layernum)),
        };
        // Get that [Layer], so we can get or add a [LayerPurpose]
        let layer = self
            .slots
            .get_mut(key)
            .ok_or(LayoutError::msg("Layer Not Found"))?;
        // Get or create the corresponding [LayerPurpose]
        let purpose = match layer.purpose(purposenum) {
            Some(purpose) => purpose.clone(),
            None => {
                // Create a new anonymous/ numbered layer-purpose
                let purpose = LayerPurpose::Other(purposenum);
                layer.add_purpose(purposenum, purpose.clone())?;
                purpose
            }
        };
        Ok((key, purpose))
    }
}
/// Layer-Purpose Enumeration
/// Includes the common use-cases for each shape,
/// and two "escape hatches", one named and one not.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum LayerPurpose {
    Drawing,
    Pin,
    Label,
    Obstruction,
    Outline,
    /// Named purpose, not first-class supported
    Named(String, i16),
    /// Other purpose, not first-class supported nor named
    Other(i16),
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
pub struct Layer {
    /// Layer Number
    pub layernum: i16,
    /// Number => Purpose Lookup
    purps: HashMap<i16, LayerPurpose>,
    /// Purpose => Number Lookup
    nums: HashMap<LayerPurpose, i16>,
}
impl Layer {
    /// Create a new [Layer] with the given `layernum`.
    pub fn new(num: i16) -> Self {
        Self {
            layernum: num,
            ..Default::default()
        }
    }
    /// Add a new [LayerPurpose]
    pub fn add_purpose(&mut self, num: i16, purp: LayerPurpose) -> LayoutResult<()> {
        // If we get a numbered purpose, make sure its id matches `num`.
        match purp {
            LayerPurpose::Named(_, k) | LayerPurpose::Other(k) => {
                if k != num {
                    return Err(LayoutError::msg("Invalid LayerPurpose"));
                }
            }
            _ => (),
        };
        self.purps.insert(num, purp.clone());
        self.nums.insert(purp, num);
        Ok(())
    }
    /// Retrieve purpose-number `num`
    pub fn purpose(&self, num: i16) -> Option<&LayerPurpose> {
        self.purps.get(&num)
    }
    /// Retrieve the purpose-number for this layer and [Purpose] `purpose`
    pub fn num(&self, purpose: &LayerPurpose) -> Option<&i16> {
        self.nums.get(purpose)
    }
}

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
    /// Layer Definitions
    pub layers: Layers,
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
    pub fn to_gds(self) -> LayoutResult<gds21::GdsLibrary> {
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
    /// Primitive Elements
    pub elems: Vec<Element>,
    /// Text Annotations
    pub annotations: Vec<TextElement>,
}
/// # Text Annotation
///
/// Note [layout21::raw::TextElement]s are "layer-less",
/// i.e. they do not sit on different layers,
/// and do not describe connectivity or generate pins.
/// These are purely annotations in the sense of "design notes".
///
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TextElement {
    /// String Value
    pub string: String,
    /// Location
    pub loc: Point,
}
/// # Primitive Geometric Element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Element {
    /// Net Name
    pub net: Option<String>,
    /// Layer (Reference)
    pub layer: LayerKey,
    /// Purpose
    pub purpose: LayerPurpose,
    /// Shape
    pub inner: Shape,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Shape {
    Rect { p0: Point, p1: Point },
    Poly { pts: Vec<Point> },
    Path { width: usize, pts: Vec<Point> },
}
impl Shape {
    /// Retrieve our "origin", or first [Point]
    pub fn point0(&self) -> &Point {
        match *self {
            Shape::Rect { ref p0, p1: _ } => p0,
            Shape::Poly { ref pts } => &pts[0],
            Shape::Path { ref pts, .. } => &pts[0],
        }
    }
    /// Calculate our center-point
    pub fn center(&self) -> Point {
        match *self {
            Shape::Rect { ref p0, ref p1 } => Point::new((p0.x + p1.x) / 2, (p0.y + p1.y) / 2),
            Shape::Poly { pts: _ } => {
                unimplemented!("Shape::Poly::center");
            }
            Shape::Path { .. } => todo!(),
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
            Shape::Path { .. } => todo!(),
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
            Shape::Path { ref mut pts, .. } => {
                for p in pts.iter_mut() {
                    p.x += pt.x;
                    p.y += pt.y;
                }
            }
        }
    }
    /// Boolean indication of whether we contain point `pt`
    pub fn contains(&self, pt: &Point) -> bool {
        match self {
            Shape::Rect { ref p0, ref p1 } => {
                p0.x.min(p1.x) <= pt.x
                    && p0.x.max(p1.x) >= pt.x
                    && p0.y.min(p1.y) <= pt.y
                    && p0.y.max(p1.y) >= pt.y
            }
            Shape::Poly { .. } => false, // FIXME! todo!(),
            Shape::Path { ref width, ref pts } => {
                // Break into segments, and check for intersection with each
                // Probably not the most efficient way to do this, but a start.
                // Only "Manhattan paths", i.e. those with segments solely running vertically or horizontally, are supported.
                // FIXME: even with this method, there are some small pieces at corners which we'll miss.
                // Whether these are relevant in real life, tbd.
                let width = isize::try_from(*width).unwrap(); // FIXME: probably store these signed, check them on creation
                for k in 0..pts.len() - 1 {
                    let rect = if pts[k].x == pts[k + 1].x {
                        Shape::Rect {
                            p0: Point::new(pts[k].x - width / 2, pts[k].y),
                            p1: Point::new(pts[k].x + width / 2, pts[k + 1].y),
                        }
                    } else if pts[k].y == pts[k + 1].y {
                        Shape::Rect {
                            p0: Point::new(pts[k].x, pts[k].y - width / 2),
                            p1: Point::new(pts[k + 1].x, pts[k].y + width / 2),
                        }
                    } else {
                        unimplemented!("Unsupported Non-Manhattan Path")
                    };
                    if rect.contains(pt) {
                        return true;
                    }
                }
                false
            }
        }
    }
}
/// # ProtoBuf Converter
///
#[derive(Debug)]
pub struct ProtoConverter {
    pub lib: Library,
}
impl ProtoConverter {
    pub fn convert(lib: Library) -> LayoutResult<proto::Library> {
        Self { lib }.convert_all()
    }
    /// Internal implementation method. Convert all, starting from our top-level [Library].
    fn convert_all(&mut self) -> LayoutResult<proto::Library> {
        // Create a new [proto::Library]
        let mut lib = proto::Library::default();
        // FIXME: should these protos have a units field?
        // Set its library name
        lib.name = Some(proto::QualifiedName {
            domain: "".into(),
            name: self.lib.name.clone(),
        });
        // And convert each of our cells
        lib.cells = self
            .lib
            .cells
            .iter()
            .map(|c| self.convert_cell(c))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(lib)
    }
    /// Convert a [Cell] to a [proto::Cell] cell-definition
    fn convert_cell(&self, cell: &Cell) -> LayoutResult<proto::Cell> {
        // Create the empty/default [proto::Cell]
        let mut pcell = proto::Cell::default();
        // Convert our name
        pcell.name = Some(proto::QualifiedName {
            domain: "".into(), // FIXME
            name: cell.name.clone(),
        });
        // Convert each [Instance]
        pcell.instances = cell
            .insts
            .iter()
            .map(|c| self.convert_instance(c))
            .collect::<Result<Vec<_>, _>>()?;
        // Convert each [Instance]
        pcell.annotations = cell
            .annotations
            .iter()
            .map(|x| self.convert_annotation(x))
            .collect::<Result<Vec<_>, _>>()?;
        // Collect up shapes by layer
        // FIXME: should we store them here this way in the first place? Perhaps.
        let mut layers: HashMap<(i16, i16), Vec<&Element>> = HashMap::new();
        for elem in &cell.elems {
            let layer = self.lib.layers.get(elem.layer).ok_or("Invalid Layer")?;
            let number = layer.layernum;
            let purpose = layer
                .num(&elem.purpose)
                .ok_or("Invalid Layer Purpose / DataType")?
                .clone();
            if layers.contains_key(&(number, purpose)) {
                layers.get_mut(&(number, purpose)).unwrap().push(elem);
            } else {
                layers.insert((number, purpose), vec![elem]);
            }
        }
        // Now turn those into [proto::LayerShape]s
        for (layernum, elems) in layers {
            let mut layershape = proto::LayerShapes::default();
            layershape.layer = Some(proto::Layer {
                number: layernum.0 as i64,
                purpose: layernum.1 as i64,
            });
            for elem in elems {
                // Also sort into the proto-schema's by-shape-type vectors
                match self.convert_element(elem)? {
                    ProtoShape::Rect(r) => layershape.rectangles.push(r),
                    ProtoShape::Poly(p) => layershape.polygons.push(p),
                    ProtoShape::Path(p) => layershape.paths.push(p),
                }
            }
            pcell.shapes.push(layershape);
        }
        Ok(pcell)
    }
    /// Convert an [Instance] to a [proto::Instance]
    fn convert_instance(&self, inst: &Instance) -> LayoutResult<proto::Instance> {
        Ok(proto::Instance {
            name: inst.inst_name.clone(),
            cell_name: Some(proto::QualifiedName {
                domain: "".into(), // FIXME
                name: inst.cell_name.clone(),
            }),
            lower_left: Some(proto::Point::new(inst.p0.x as i64, inst.p0.y as i64)),
            rotation_clockwise_degrees: 0,
        })
    }
    /// Convert an [Instance] to a [proto::Instance]
    fn convert_annotation(&self, text: &TextElement) -> LayoutResult<proto::TextElement> {
        Ok(proto::TextElement {
            string: text.string.clone(),
            loc: Some(proto::Point::new(text.loc.x as i64, text.loc.y as i64)),
        })
    }
    fn convert_element(&self, elem: &Element) -> LayoutResult<ProtoShape> {
        // Convert unconnected nets to the empty string
        let net = if let Some(ref net) = elem.net {
            net.clone()
        } else {
            "".into()
        };
        match &elem.inner {
            Shape::Rect { ref p0, ref p1 } => {
                let minx = p0.x.min(p1.x) as i64;
                let miny = p0.y.min(p1.y) as i64;
                let width = p0.x.max(p1.x) as i64 - minx;
                let height = p0.y.max(p1.y) as i64 - miny;
                Ok(ProtoShape::Rect(proto::Rectangle {
                    net,
                    lower_left: Some(proto::Point::new(minx, miny)),
                    width,
                    height,
                }))
            }
            Shape::Poly { ref pts } => {
                let vertices = pts
                    .iter()
                    .map(|p| proto::Point::new(p.x as i64, p.y as i64))
                    .collect::<Vec<_>>();
                Ok(ProtoShape::Poly(proto::Polygon { net, vertices }))
            }
            Shape::Path { ref width, ref pts } => {
                let width = i64::try_from(*width)?;
                let points = pts
                    .iter()
                    .map(|p| proto::Point::new(p.x as i64, p.y as i64))
                    .collect::<Vec<_>>();
                Ok(ProtoShape::Path(proto::Path { net, width, points }))
            }
        }
    }
}
/// Helper enumeration for converting to several proto-primitives
enum ProtoShape {
    Rect(proto::Rectangle),
    Poly(proto::Polygon),
    Path(proto::Path),
}
