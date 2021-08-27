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
use std::ops::Not;

// Crates.io
use serde::{Deserialize, Serialize};
use slotmap::{new_key_type, SlotMap};

// Internal modules & re-exports
pub use layout21utils as utils;
use utils::Ptr;

#[cfg(feature = "gds")]
pub mod gds;
#[cfg(feature = "lef")]
pub mod lef;
#[cfg(feature = "proto")]
pub mod proto;
#[cfg(test)]
mod tests;

// Create key-types for each internal type stored in [SlotMap]s
new_key_type! {
    /// Keys for [Layer] entries
    pub struct LayerKey;
    /// Keys for [Cell] entries
    pub struct CellKey;
    /// Keys for [abstrakt::LayoutAbstract] entries
    pub struct AbstractKey;
}
/// LayoutError-Specific Result Type
pub type LayoutResult<T> = Result<T, LayoutError>;

///
/// # Layout Error Enumeration
///
#[derive(Debug)]
pub enum LayoutError {
    /// Error Exporting to Foreign Format
    Export {
        message: String,
        stack: Vec<ErrorContext>,
    },
    /// Error Importing from Foreign Format
    Import {
        message: String,
        stack: Vec<ErrorContext>,
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
impl From<utils::ser::Error> for LayoutError {
    fn from(e: utils::ser::Error) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl<T> From<std::sync::PoisonError<T>> for LayoutError {
    fn from(_e: std::sync::PoisonError<T>) -> Self {
        Self::Tbd // FIXME!
    }
}
impl<T: std::error::Error + 'static> From<Box<T>> for LayoutError {
    fn from(e: Box<T>) -> Self {
        Self::Boxed(e)
    }
}
/// Helper trait for re-use among our many conversion tree-walkers.
/// Each implementer will generally have some internal state to report upon failure,
/// which it can inject in the implementation-required `err` method.
/// The `fail` method, provided by default, simply returns the `err` value.
pub trait HasErrors {
    /// Create and return a [LayoutError]
    fn err(&self, msg: impl Into<String>) -> LayoutError;
    /// Return failure
    fn fail<T>(&self, msg: impl Into<String>) -> LayoutResult<T> {
        Err(self.err(msg))
    }
    /// Unwrap the [Option] `opt` if it is [Some], and return our error if not.
    fn unwrap<T>(&self, opt: Option<T>, msg: impl Into<String>) -> LayoutResult<T> {
        match opt {
            Some(val) => Ok(val),
            None => self.fail(msg),
        }
    }
    /// Unwrap the [Result] `res`. Return through our failure method if it is [Err].
    fn ok<T, E>(&self, res: Result<T, E>, msg: impl Into<String>) -> LayoutResult<T> {
        match res {
            Ok(val) => Ok(val),
            Err(_) => self.fail(msg),
        }
    }
}
/// Enumerated conversion contexts
/// Generally used for error reporting
#[derive(Debug, Clone)]
pub enum ErrorContext {
    Library(String),
    Cell(String),
    Instance(String),
    Array(String),
    Units,
    Geometry,
    Unknown,
}

/// Distance Units Enumeration
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum Units {
    /// Micrometers, or microns for we olde folke
    Micro,
    /// Nanometers
    Nano,
    /// Angstroms
    Angstrom,
}
impl Default for Units {
    /// Default units are nanometers
    fn default() -> Units {
        Units::Nano
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
    pub fn other(self) -> Self {
        match self {
            Self::Horiz => Self::Vert,
            Self::Vert => Self::Horiz,
        }
    }
}
impl Not for Dir {
    type Output = Self;
    /// Exclamation Operator returns the opposite direction
    fn not(self) -> Self::Output {
        Self::other(self)
    }
}

/// Instance of another Cell
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instance {
    /// Instance Name
    pub inst_name: String,
    /// Cell Definition Reference
    pub cell: CellKey,
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
    slots: SlotMap<LayerKey, Layer>,
    nums: HashMap<i16, LayerKey>,
    names: HashMap<String, LayerKey>,
}
impl Layers {
    /// Add a [Layer] to our slot-map and number-map, and name-map
    pub fn add(&mut self, layer: Layer) -> LayerKey {
        // FIXME: conflicting numbers and/or names, at least some of which tend to happen, over-write each other.
        // Sort out the desired behavior here.
        let num = layer.layernum;
        let name = layer.name.clone();
        let key = self.slots.insert(layer);
        self.nums.insert(num, key.clone());
        if let Some(s) = name {
            self.names.insert(s, key.clone());
        }
        key
    }
    /// Get a reference to the [LayerKey] for layer-number `num`
    pub fn keynum(&self, num: i16) -> Option<&LayerKey> {
        self.nums.get(&num)
    }
    /// Get a reference to the [LayerKey] layer-name `name`
    pub fn keyname(&self, name: impl Into<String>) -> Option<&LayerKey> {
        self.names.get(&name.into())
    }
    /// Get a reference to [Layer] number `num`
    pub fn num(&self, num: i16) -> Option<&Layer> {
        let key = self.nums.get(&num)?;
        self.slots.get(*key)
    }
    /// Get a reference to [Layer] name `name`
    pub fn name(&self, name: &str) -> Option<&Layer> {
        let key = self.names.get(name)?;
        self.slots.get(*key)
    }
    /// Get the name of `layerkey`
    pub fn get_name(&self, layerkey: LayerKey) -> Option<&String> {
        let layer = self.slots.get(layerkey)?;
        layer.name.as_ref()
    }
    /// Get a reference to [Layer] from [LayerKey] `key`
    pub fn get(&self, key: LayerKey) -> Option<&Layer> {
        self.slots.get(key)
    }
    /// Get the ([LayerKey], [LayerPurpose]) objects for numbers (`layernum`, `purposenum`) if present.
    /// Inserts a new [Layer] if `layernum` is not present.
    /// Returns `LayerPurpose::Other(purposenum)` if `purposenum` is not present on that layer.
    pub fn get_or_insert(
        &mut self,
        layernum: i16,
        purposenum: i16,
    ) -> LayoutResult<(LayerKey, LayerPurpose)> {
        // Get the [LayerKey] for `layernum`, creating the [Layer] if it doesn't exist.
        let key = match self.keynum(layernum) {
            Some(key) => key.clone(),
            None => self.add(Layer::from_num(layernum)),
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
    // First-class enumerated purposes
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
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct Layer {
    /// Layer Number
    pub layernum: i16,
    /// Layer Name
    pub name: Option<String>,
    /// Number => Purpose Lookup
    purps: HashMap<i16, LayerPurpose>,
    /// Purpose => Number Lookup
    nums: HashMap<LayerPurpose, i16>,
}
impl Layer {
    /// Create a new [Layer] with the given `layernum` and `name`
    pub fn new(layernum: i16, name: impl Into<String>) -> Self {
        Self {
            layernum,
            name: Some(name.into()),
            ..Default::default()
        }
    }
    /// Create a new [Layer] with the given `layernum`.
    pub fn from_num(layernum: i16) -> Self {
        Self {
            layernum,
            ..Default::default()
        }
    }
    /// Create a new [Layer] purpose-numbers `pairs`.
    pub fn from_pairs(layernum: i16, pairs: &[(i16, LayerPurpose)]) -> LayoutResult<Self> {
        let mut layer = Self::from_num(layernum);
        for (num, purpose) in pairs {
            layer.add_purpose(*num, purpose.clone())?;
        }
        Ok(layer)
    }
    /// Add purpose-numbers `pairs`. Consumes and returns `self` for chainability.
    pub fn add_pairs(mut self, pairs: &[(i16, LayerPurpose)]) -> LayoutResult<Self> {
        for (num, purpose) in pairs {
            self.add_purpose(*num, purpose.clone())?;
        }
        Ok(self)
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
    pub fn num(&self, purpose: &LayerPurpose) -> Option<i16> {
        self.nums.get(purpose).copied()
    }
}

/// Raw Abstract-Layout
/// Contains geometric [Element]s generally representing pins and blockages
/// Does not contain instances, arrays, or layout-implementation details
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Abstract {
    /// Cell Name
    pub name: String,
    /// Ports
    pub ports: Vec<AbstractPort>,
    /// Blockages
    pub blockages: HashMap<LayerKey, Vec<Shape>>,
}
/// # Port Element for [Abstract]s
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AbstractPort {
    /// Net Name
    pub net: String,
    /// Shapes, with paired [Layer] keys
    pub shapes: HashMap<LayerKey, Vec<Shape>>,
}

/// # Raw Layout Library  
/// A collection of cell-definitions and sub-library definitions
#[derive(Debug, Clone, Default)]
pub struct Library {
    /// Library Name
    pub name: String,
    /// Distance Units
    pub units: Units,
    /// Layer Definitions
    pub layers: Ptr<Layers>,
    /// Cell Definitions
    pub cells: SlotMap<CellKey, Cell>,
    /// Abstract-Layout Definitions
    pub abstracts: Vec<Abstract>,
}
impl Library {
    /// Create a new and empty Library
    pub fn new(name: impl Into<String>, units: Units) -> Self {
        Self {
            name: name.into(),
            units,
            cells: SlotMap::with_key(),
            ..Default::default()
        }
    }
    /// Convert to a GDSII Library
    #[cfg(feature = "gds")]
    pub fn to_gds(&self) -> LayoutResult<gds::gds21::GdsLibrary> {
        gds::GdsExporter::export(&self)
    }
    /// Create from GDSII
    #[cfg(feature = "gds")]
    pub fn from_gds(
        gdslib: &gds::gds21::GdsLibrary,
        layers: Option<Ptr<Layers>>,
    ) -> LayoutResult<Library> {
        gds::GdsImporter::import(&gdslib, layers)
    }
    /// Convert to ProtoBuf
    #[cfg(feature = "proto")]
    pub fn to_proto(&self) -> LayoutResult<proto::proto::Library> {
        proto::ProtoExporter::export(&self)
    }
    /// Create from ProtoBuf, or anything convertible into a Proto Library
    #[cfg(feature = "proto")]
    pub fn from_proto<T>(plib: T, layers: Option<Ptr<Layers>>) -> LayoutResult<Library>
    where
        // These trait bounds aren't pretty, but more or less say:
        // * T is convertible into [proto::proto::Library]
        // * Its conversion's error-type is convertible into [LayoutError]
        // The "Into" form of the second condition would be something like:
        // `<T as TryInto<proto::proto::Library>::Error>: Into<LayoutError>`
        // but doesn't quite work, whereas "constraining" [LayoutError] does.
        T: TryInto<proto::proto::Library>,
        LayoutError: From<<T as TryInto<proto::proto::Library>>::Error>,
    {
        let plib = plib.try_into()?;
        proto::ProtoImporter::import(&plib, layers)
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
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct TextElement {
    /// String Value
    pub string: String,
    /// Location
    pub loc: Point,
}
/// # Primitive Geometric Element
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
            Shape::Path { .. } => Point::new(0, 0), // FIXME!!!
        }
    }
    /// Indicate whether this shape is (more or less) horizontal or vertical
    /// Primarily used for orienting label-text
    pub fn orientation(&self) -> Dir {
        match *self {
            Shape::Rect { ref p0, ref p1 } => {
                if (p1.x - p0.x).abs() < (p1.y - p0.y).abs() {
                    return Dir::Vert;
                }
                Dir::Horiz
            }
            // Polygon and Path elements always horizontal, at least for now
            Shape::Poly { .. } => Dir::Horiz,
            Shape::Path { .. } => Dir::Horiz,
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
