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
use std::convert::TryFrom;
use std::hash::Hash;

// Crates.io
use serde::{Deserialize, Serialize};
use slotmap::{new_key_type, SlotMap};

// Local imports
use gds21;
use layout21protos as proto;

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
    Export,
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
            Shape::Path { .. } => todo!(),
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
        let mut elems = Vec::with_capacity(cell.elems.len() + cell.insts.len());
        // Convert each [Instance]
        for inst in cell.insts.iter() {
            elems.push(self.convert_instance(inst).into());
        }
        // Convert each [Element]
        // Note each can produce more than one [GdsElement]
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
            ..Default::default()
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
        let layer = self
            .lib
            .layers
            .get(elem.layer)
            .ok_or(LayoutError::msg("Layer Not Defined"))?;
        let datatype = layer
            .num(&elem.purpose)
            .ok_or(LayoutError::msg(format!(
                "LayerPurpose Not Defined for {}, {:?}",
                layer.layernum, elem.purpose
            )))?
            .clone();

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
            Shape::Path { .. } => todo!(),
        };
        // Initialize our vector of elements with the shape
        let mut gds_elems = vec![gds21::GdsBoundary {
            layer: layer.layernum,
            datatype,
            xy,
            ..Default::default()
        }
        .into()];
        // If there's an assigned net, create a corresponding text-element
        if let Some(name) = &elem.net {
            let texttype = layer
                .num(&LayerPurpose::Label)
                .ok_or(LayoutError::msg("Text Layer Not Defined"))?
                .clone();

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
                    layer: layer.layernum,
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
}
/// # GDSII Importer
///
#[derive(Debug)]
pub struct GdsImporter {
    pub layers: Layers,
}
impl GdsImporter {
    /// Import a [gds21::GdsLibrary] into a [Library]
    /// FIXME: layer definitions
    pub fn import(lib: gds21::GdsLibrary) -> LayoutResult<Library> {
        let mut importer = Self {
            layers: Layers::default(),
        };
        let mut rv = importer.import_lib(&lib)?;
        let Self { layers } = importer;
        rv.layers = layers;
        Ok(rv)
    }
    /// Internal implementation method. Convert all, starting from our top-level [gds21::GdsLibrary].
    fn import_lib(&mut self, gdslib: &gds21::GdsLibrary) -> LayoutResult<Library> {
        // Check our GDS doesn't (somehow) include any unsupported features
        if gdslib.libdirsize.is_some()
            || gdslib.srfname.is_some()
            || gdslib.libsecur.is_some()
            || gdslib.reflibs.is_some()
            || gdslib.fonts.is_some()
            || gdslib.attrtable.is_some()
            || gdslib.generations.is_some()
            || gdslib.format_type.is_some()
        {
            return Err(LayoutError::msg("Unsupported GDSII Feature"));
        }
        // Create a new [Library]
        let mut lib = Library::default();
        // Give it the same name as the GDS
        lib.name = gdslib.name.clone();
        // Set its distance units
        lib.units = self.import_units(&gdslib.units)?;
        // And convert each of its `structs` into our `cells`
        lib.cells = gdslib
            .structs
            .iter()
            .map(|x| self.import_cell(x))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(lib)
    }
    /// Import our [Unit]s
    fn import_units(&mut self, units: &gds21::GdsUnits) -> LayoutResult<Unit> {
        // Peel out the GDS "database unit", the one of its numbers that really matters
        let gdsunit = units.dbunit();
        // FIXME: intermediate/ calculated units
        // Only our enumerated values are thus far supported
        if gdsunit == 1e-9 {
            Ok(Unit::Nano)
        } else if gdsunit == 1e-6 {
            Ok(Unit::Micro)
        } else {
            Err(LayoutError::msg("Unsupported GDSII Unit"))
        }
    }
    /// Import a GDS Cell ([gds21::GdsStruct]) into a [Cell]
    fn import_cell(&mut self, strukt: &gds21::GdsStruct) -> LayoutResult<Cell> {
        let mut cell = Cell::default();
        cell.name = strukt.name.clone();
        // Importing each cell requires at least two passes over its elements.
        // In the first pass we add each [Instance] and geometric element,
        // And keep a list of [gds21::GdsTextElem] on the side.
        let mut texts: Vec<&gds21::GdsTextElem> = Vec::new();
        let mut elems: SlotMap<ElementKey, Element> = SlotMap::with_key();
        // Also keep a hash of by-layer elements, to aid in text-assignment in our second pass
        let mut layers: HashMap<i16, Vec<ElementKey>> = HashMap::new();
        for elem in &strukt.elems {
            use gds21::GdsElement::*;
            match elem {
                GdsBoundary(ref x) => {
                    let e = self.import_boundary(x)?;
                    let ekey = elems.insert(e);
                    if let Some(ref mut bucket) = layers.get_mut(&x.layer) {
                        bucket.push(ekey);
                    } else {
                        layers.insert(x.layer, vec![ekey]);
                    }
                }
                GdsPath(ref x) => {
                    let e = self.import_path(x)?;
                    let ekey = elems.insert(e);
                    if let Some(ref mut bucket) = layers.get_mut(&x.layer) {
                        bucket.push(ekey);
                    } else {
                        layers.insert(x.layer, vec![ekey]);
                    }
                }
                GdsBox(ref x) => {
                    let e = self.import_box(x)?;
                    let ekey = elems.insert(e);
                    if let Some(ref mut bucket) = layers.get_mut(&x.layer) {
                        bucket.push(ekey);
                    } else {
                        layers.insert(x.layer, vec![ekey]);
                    }
                }
                GdsArrayRef(ref x) => cell.insts.extend(self.import_instance_array(x)?),
                GdsStructRef(ref x) => cell.insts.push(self.import_instance(x)?),
                GdsTextElem(ref x) => texts.push(x),
                GdsNode(ref _x) => {
                    // GDSII "Node" elements are fairly rare, and are not supported.
                    // (Maybe some day we'll even learn what they are.)
                    return Err(LayoutError::msg("Unsupported GDSII Element: Node"));
                }
            }
        }
        // Pass two: sort out whether each [gds21::GdsTextElem] is a net-label,
        // And if so, assign it as a net-name on each intersecting [Element].
        // Text elements which do not overlap a geometric element on the same layer
        // are converted to annotations.
        for textelem in &texts {
            let loc = Point::new(textelem.xy[0] as isize, textelem.xy[1] as isize);
            if let Some(layer) = layers.get(&textelem.layer) {
                // Layer exists in geometry; see which elements intersect with this text
                let mut hit = false;
                for ekey in layer.iter() {
                    let elem = elems.get_mut(*ekey).unwrap();
                    if elem.inner.contains(&loc) {
                        // Label lands inside this element.
                        // Check whether we have an existing label.
                        // If so, it better be the same net name!
                        if let Some(pname) = &elem.net {
                            if pname != &textelem.string {
                                return Err(LayoutError::msg(format!(
                                    "GDSII labels shorting nets {} and {} on layer {}",
                                    pname,
                                    textelem.string.clone(),
                                    textelem.layer
                                )));
                            }
                        }
                        elem.net = Some(textelem.string.clone());
                        hit = true;
                    }
                }
                // If we've hit at least one, carry onto the next TextElement
                if hit {
                    continue;
                }
            }
            // No hits (or a no-shape Layer). Create an annotation instead.
            cell.annotations.push(TextElement {
                string: textelem.string.clone(),
                loc,
            });
        }
        // Pull the elements out of the local slot-map, into the vector that [Cell] wants
        cell.elems = elems.drain().map(|(_k, v)| v).collect();
        Ok(cell)
    }
    /// Import a [gds21::GdsBoundary] into an [Element]
    fn import_boundary(&mut self, x: &gds21::GdsBoundary) -> LayoutResult<Element> {
        if x.xy.len() % 2 != 0 {
            return Err(LayoutError::msg(
                "GDS Boundary must have an even number of points",
            ));
        }
        let mut pts = Vec::new();
        for k in 0..x.xy.len() / 2 {
            pts.push(Point::new(x.xy[k * 2] as isize, x.xy[k * 2 + 1] as isize));
        }
        if pts[0] != *pts.last().unwrap() {
            return Err(LayoutError::msg(
                "GDS Boundary must start and end at the same point",
            ));
        }
        // Pop the redundant last entry
        pts.pop();
        // Check for Rectangles; they help
        let inner = if pts.len() == 4
            && ((pts[0].x == pts[1].x // Clockwise
                && pts[1].y == pts[2].y
                && pts[2].x == pts[3].x
                && pts[3].y == pts[0].y)
                || (pts[0].y == pts[1].y // Counter-clockwise
                    && pts[1].x == pts[2].x
                    && pts[2].y == pts[3].y
                    && pts[3].x == pts[0].x))
        {
            // That makes this a Rectangle.
            Shape::Rect {
                p0: pts[0].clone(),
                p1: pts[2].clone(),
            }
        } else {
            // Otherwise, it's a polygon
            Shape::Poly { pts }
        };

        // Grab (or create) its [Layer]
        let (layer, purpose) = self.layers.get_or_insert(x.layer, x.datatype)?;
        // Create the Element, and insert it in our slotmap
        let e = Element {
            net: None,
            layer,
            purpose,
            inner,
        };
        Ok(e)
    }
    /// Import a [gds21::GdsBox] into an [Element]
    fn import_box(&mut self, x: &gds21::GdsBox) -> LayoutResult<Element> {
        if x.xy.len() != 10 {
            return Err(LayoutError::msg("Invalid GDS Box XY Length"));
        }
        // GDS stores *five* coordinates per box (for whatever reason).
        // This does not check fox "box validity", and imports the
        // first and third of those five coordinates,
        // which are by necessity for a valid [GdsBox] located at opposite corners.
        let inner = Shape::Rect {
            p0: Point::new(x.xy[0] as isize, x.xy[1] as isize),
            p1: Point::new(x.xy[3] as isize, x.xy[4] as isize),
        };

        // Grab (or create) its [Layer]
        let (layer, purpose) = self.layers.get_or_insert(x.layer, x.boxtype)?;
        // Create the Element, and insert it in our slotmap
        let e = Element {
            net: None,
            layer,
            purpose,
            inner,
        };
        Ok(e)
    }
    /// Import a [gds21::GdsPath] into an [Element]
    fn import_path(&mut self, x: &gds21::GdsPath) -> LayoutResult<Element> {
        if x.xy.len() % 2 != 0 {
            // FIXME: make this a GDS thing
            return Err(LayoutError::msg(
                "GDS Boundary must have an even number of points",
            ));
        }
        let mut pts = Vec::new();
        for k in 0..x.xy.len() / 2 {
            pts.push(Point::new(x.xy[k * 2] as isize, x.xy[k * 2 + 1] as isize));
        }
        let width = if let Some(w) = x.width {
            w as usize
        } else {
            return Err(LayoutError::msg("Invalid nonspecifed GDS Path width "));
        };
        // Create the shape
        let inner = Shape::Path { width, pts };

        // Grab (or create) its [Layer]
        let (layer, purpose) = self.layers.get_or_insert(x.layer, x.datatype)?;
        // Create the Element, and insert it in our slotmap
        let e = Element {
            net: None,
            layer,
            purpose,
            inner,
        };
        Ok(e)
    }
    /// Import a [gds21::GdsStructRef] cell/struct-instance into an [Instance]
    fn import_instance(&mut self, sref: &gds21::GdsStructRef) -> LayoutResult<Instance> {
        let inst_name = "".into(); // FIXME
        let cell_name = sref.name.clone();
        let cell = CellRef::Name(sref.name.clone()); // FIXME
        let p0 = Point::new(sref.xy[0] as isize, sref.xy[1] as isize);
        let mut inst = Instance {
            inst_name,
            cell_name,
            cell,
            p0,
            reflect: false, // FIXME!
            angle: None,    // FIXME!
        };
        if let Some(strans) = &sref.strans {
            // FIXME: interpretation of the "absolute" settings
            if strans.abs_mag || strans.abs_angle || strans.mag.is_some() {
                return Err(LayoutError::msg("Unsupported GDSII Instance: Absolute"));
            }
            inst.reflect = strans.reflected;
            inst.angle = strans.angle;
        }
        Ok(inst)
    }
    /// Import a (two-dimensional) [gds21::GdsArrayRef] into [Instance]s
    fn import_instance_array(&mut self, aref: &gds21::GdsArrayRef) -> LayoutResult<Vec<Instance>> {
        let inst_name = "".to_string(); // FIXME
        let cell_name = aref.name.clone();
        let cell = CellRef::Name(aref.name.clone()); // FIXME
        if aref.xy.len() != 6 {
            return Err(LayoutError::msg("Invalid Array XY Length"));
        }
        // Check for (thus far) unsupported non-rectangular arrays
        if aref.xy[1] != aref.xy[3] || aref.xy[0] != aref.xy[4] {
            return Err(LayoutError::msg("Invalid Non-Rectangular GDS Array"));
        }
        // Sort out the inter-element spacing
        let p0 = Point::new(aref.xy[0] as isize, aref.xy[1] as isize);
        let width = aref.xy[0] as isize - aref.xy[2] as isize;
        let height = aref.xy[1] as isize - aref.xy[5] as isize;
        let xstep = width / (aref.cols as isize);
        let ystep = height / (aref.rows as isize);
        // Grab the reflection/ rotation settings
        // FIXME: these need *actual* support
        let mut reflect = false;
        let mut angle = None;
        if let Some(strans) = &aref.strans {
            // FIXME: interpretation of the settings
            if strans.abs_mag || strans.abs_angle || strans.mag.is_some() || strans.angle.is_some()
            {
                return Err(LayoutError::msg("Unsupported GDSII Array Attributes"));
            }
            angle = strans.angle;
            reflect = strans.reflected;
        }
        // Create the Instances
        let mut insts = Vec::with_capacity((aref.rows * aref.cols) as usize);
        for ix in 0..(aref.cols as isize) {
            let x = p0.x + ix * xstep;
            for iy in 0..(aref.rows as isize) {
                let y = p0.y + iy * ystep;
                insts.push(Instance {
                    inst_name: inst_name.clone(),
                    cell_name: cell_name.clone(),
                    cell: cell.clone(),
                    p0: Point::new(x, y),
                    reflect, // FIXME!
                    angle,   // FIXME!
                });
            }
        }
        Ok(insts)
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
        // And convert each of our `cells` into its `structs`
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
                let width = i64::try_from(*width)?; //.ok_or(LayoutError::Tbd)?;
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
