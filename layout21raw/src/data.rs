//!
//! # Raw Layout Data Model
//!
//! Defines the primary structures for representation of "raw"  geometry-based IC layout,
//! including [Library], [Cell], [Layout], and related types.
//!

// Std-Lib
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use gds21::GdsLayerSpec;
// Crates.io
use derive_builder::Builder;
use serde::{Deserialize, Serialize};
use slotmap::{new_key_type, SlotMap};

use crate::align::AlignRect;
use crate::translate::Translate;
use crate::Rect;
// Local Imports
use crate::{
    bbox::{BoundBox, BoundBoxTrait},
    error::{LayoutError, LayoutResult},
    geom::{Point, Polygon, Shape, Transform, TransformTrait},
    utils::{Ptr, PtrList},
};

/// # Location Integer Type-Alias
///
/// Used for all layout spatial coordinates.
/// Designed for quickly swapping to other integer types, if we so desire.
///
pub type Int = isize;

// Create key-types for each internal type stored in [SlotMap]s
new_key_type! {
    /// Keys for [Layer] entries
    pub struct LayerKey;
}

/// Distance Units Enumeration
/// FIXME: deprecate in favor of [SiUnits]
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum Units {
    /// Micrometers, or microns for we olde folke
    Micro,
    /// Nanometers
    Nano,
    /// Angstroms
    Angstrom,
    /// Picometers
    Pico,
}
impl Default for Units {
    /// Default units are nanometers
    fn default() -> Units {
        Units::Nano
    }
}
/// Enumerated SI Units
#[allow(dead_code)] // FIXME!
enum SiUnits {
    Yocto, // E-24
    Zepto, // E-21
    Atto,  // E-18
    Femto, // E-15
    Pico,  // E-12
    Nano,  // E-9
    Micro, // E-6
    Milli, // E-3
    Centi, // E-2
    Deci,  // E-1
    Deca,  // E1
    Hecto, // E2
    Kilo,  // E3
    Mega,  // E6
    Giga,  // E9
    Tera,  // E12
    Peta,  // E15
    Exa,   // E18
    Zetta, // E21
    Yotta, // E24
}
impl Default for SiUnits {
    /// Default units are nano-scale
    #[allow(dead_code)] // FIXME!
    fn default() -> SiUnits {
        SiUnits::Nano
    }
}
impl SiUnits {
    /// Get the exponent of the unit
    #[allow(dead_code)] // FIXME!
    fn exp(&self) -> isize {
        use SiUnits::*;
        match self {
            Yocto => -24,
            Zepto => -21,
            Atto => -18,
            Femto => -15,
            Pico => -12,
            Nano => -9,
            Micro => -6,
            Milli => -3,
            Centi => -2,
            Deci => -1,
            Deca => 1,
            Hecto => 2,
            Kilo => 3,
            Mega => 6,
            Giga => 9,
            Tera => 12,
            Peta => 15,
            Exa => 18,
            Zetta => 21,
            Yotta => 24,
        }
    }
}

/// Instance of another Cell
#[derive(Debug, Clone, PartialEq, Builder)]
pub struct Instance {
    /// Instance Name
    #[builder(setter(into))]
    pub inst_name: String,
    /// Cell Definition Reference
    #[builder(setter(into))]
    pub cell: Ptr<Cell>,
    /// Location of `cell` origin
    /// regardless of rotation or reflection
    #[builder(default)]
    pub loc: Point,
    /// Vertical reflection,
    /// applied *before* rotation
    #[builder(default)]
    pub reflect_vert: bool,
    /// Angle of rotation (degrees),
    /// Clockwise and applied *after* reflection
    #[builder(default, setter(strip_option))]
    pub angle: Option<f64>,
}

impl BoundBoxTrait for Instance {
    fn bbox(&self) -> BoundBox {
        let inner = {
            let cell = self.cell.read().unwrap();
            cell.layout.as_ref().unwrap().bbox()
        };

        if inner.is_empty() {
            return inner;
        }

        let r = Rect {
            p0: inner.p0,
            p1: inner.p1,
        };

        let r = r.transform(&Transform::from_instance(
            &self.loc,
            self.reflect_vert,
            self.angle,
        ));

        BoundBox { p0: r.p0, p1: r.p1 }
    }
}

impl Translate for Instance {
    fn translate(&mut self, v: Point) {
        self.loc.translate(v);
    }
}

impl AlignRect for Instance {}

impl Instance {
    pub fn new<N, C>(name: N, cell: C) -> Self
    where
        N: Into<String>,
        C: Into<Ptr<Cell>>,
    {
        Self {
            cell: cell.into(),
            inst_name: name.into(),
            loc: Point::new(0, 0),
            reflect_vert: false,
            angle: None,
        }
    }

    #[inline]
    pub fn builder() -> InstanceBuilder {
        InstanceBuilder::default()
    }

    #[inline]
    pub fn transform(&self) -> Transform {
        self._transform()
    }

    #[inline]
    fn _transform(&self) -> Transform {
        Transform::from_instance(&self.loc, self.reflect_vert, self.angle)
    }

    pub fn port(&self, net: impl Into<String>) -> AbstractPort {
        let net = net.into();
        let cell = self.cell.read().unwrap();
        let port = cell
            .abs
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Cell `{}` does not have an abstract", &cell.name))
            .unwrap()
            .ports
            .iter()
            .find(|p| p.net == net)
            .ok_or_else(|| anyhow::anyhow!("No port named {net}"))
            .unwrap();

        port.transform(&self._transform())
    }

    pub fn ports_starting_with(&self, net: &str) -> Vec<AbstractPort> {
        let cell = self.cell.read().unwrap();
        let xform = self._transform();
        let ports = cell
            .abs
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Cell `{}` does not have an abstract", &cell.name))
            .unwrap()
            .ports
            .iter()
            .filter(|p| p.net.starts_with(net))
            .map(|p| p.transform(&xform))
            .collect();

        ports
    }

    pub fn ports(&self) -> Vec<AbstractPort> {
        let cell = self.cell.read().unwrap();
        let ports = &cell.abs.as_ref().unwrap().ports;
        let xform = self._transform();
        ports.iter().map(|p| p.transform(&xform)).collect()
    }

    pub fn has_abstract(&self) -> bool {
        let cell = self.cell.read().unwrap();
        cell.has_abstract()
    }

    pub fn reflect_vert_anchored(&mut self) -> &mut Self {
        let box0 = self.bbox();
        self.reflect_vert = !self.reflect_vert;
        let box1 = self.bbox();
        self.loc.y += box0.p0.y - box1.p0.y;
        self
    }

    pub fn reflect_horiz_anchored(&mut self) -> &mut Self {
        let box0 = self.bbox();
        self.reflect_vert = !self.reflect_vert;
        self.angle = Some(self.angle.unwrap_or(0f64) + 180f64);

        let box1 = self.bbox();
        self.loc.x += box0.p0.x - box1.p0.x;
        self.loc.y += box0.p0.y - box1.p0.y;

        let final_box = self.bbox();
        assert_eq!(final_box, box0);

        self
    }
}

/// # Layer Set & Manager
///
/// Keep track of active layers, and index them by name and number.
///
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Layers {
    pub slots: SlotMap<LayerKey, Layer>,
    pub nums: HashMap<i16, LayerKey>,
    pub names: HashMap<String, LayerKey>,
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
    /// Get the next-available (lowest) layer number
    pub fn nextnum(&self) -> LayoutResult<i16> {
        for k in 0..i16::MAX {
            if !self.nums.contains_key(&k) {
                return Ok(k);
            }
        }
        LayoutError::fail("No more layer numbers available")
    }
    /// Get a reference to the [LayerKey] layer-name `name`
    pub fn keyname(&self, name: impl Into<String>) -> Option<LayerKey> {
        self.names.get(&name.into()).map(|x| x.clone())
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
    /// Get a shared reference to the internal <[LayerKey], [Layer]> map
    pub fn slots(&self) -> &SlotMap<LayerKey, Layer> {
        &self.slots
    }

    pub fn get_new_purpose(&self, num: i16, purpose: i16, to: LayerPurpose) -> Option<LayerKey> {
        let lay = self.get_layer_from_spec(num, purpose)?;
        let purpose = lay.num(&to)?;
        let (key, _) = self.get_from_spec(LayerSpec(num, purpose))?;
        Some(key)
    }

    pub fn get_layer_from_spec(&self, num: i16, purpose: i16) -> Option<&Layer> {
        let (key, _) = self.get_from_spec(LayerSpec(num, purpose))?;
        self.get(key)
    }

    pub fn get_from_spec(&self, spec: LayerSpec) -> Option<(LayerKey, LayerPurpose)> {
        for (k, layer) in self.slots().iter() {
            if layer.layernum != spec.0 {
                continue;
            }
            if let Some(purpose) = layer.purpose(spec.1) {
                return Some((k, purpose.clone()));
            }
        }
        None
    }

    pub fn get_layer_names(&self) -> Vec<&String> {
        self.names.keys().collect()
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

impl Default for LayerPurpose {
    fn default() -> Self {
        Self::Drawing
    }
}

/// # Layer Specification
/// As in seemingly every layout system, this uses two numbers to identify each layer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LayerSpec(pub i16, pub i16);
impl LayerSpec {
    pub fn new(n1: i16, n2: i16) -> Self {
        Self(n1, n2)
    }
}
impl From<GdsLayerSpec> for LayerSpec {
    fn from(other: GdsLayerSpec) -> Self {
        Self(other.layer, other.xtype)
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

impl PartialOrd for Layer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for Layer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.layernum.cmp(&other.layernum)
    }
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
                    LayoutError::fail("Invalid LayerPurpose")?;
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
    /// Retrieve a list of purpose number-[LayerPurpose] tuples
    pub fn get_purps(&self) -> Vec<(&i16, &LayerPurpose)> {
        self.purps.iter().collect()
    }
}

/// Raw Abstract-Layout
/// Contains geometric [Element]s generally representing pins and blockages
/// Does not contain instances, arrays, or layout-implementation details

#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Abstract {
    /// Cell Name
    pub name: String,
    /// Outline
    pub outline: Option<Polygon>,
    /// Ports
    pub ports: Vec<AbstractPort>,
    /// Blockages
    pub blockages: HashMap<LayerKey, Vec<Shape>>,
}
impl Abstract {
    /// Create a new [Abstract] with the given `name`
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        Self {
            name,
            outline: None,
            ports: Vec::new(),
            blockages: HashMap::new(),
        }
    }

    pub fn add_port(&mut self, port: AbstractPort) -> &mut Self {
        self.ports.push(port);
        self
    }

    pub fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into();
    }
}

/// # Port Element for [Abstract]s
#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AbstractPort {
    /// Net Name
    pub net: String,
    /// Shapes, with paired [Layer] keys
    pub shapes: HashMap<LayerKey, Vec<Shape>>,
}
impl AbstractPort {
    /// Create a new [AbstractPort] with the given `name`
    pub fn new(net: impl Into<String>) -> Self {
        let net = net.into();
        Self {
            net,
            shapes: HashMap::new(),
        }
    }

    /// Renames this port
    pub fn named(mut self, name: impl Into<String>) -> Self {
        self.net = name.into();
        self
    }

    pub fn add_shape(&mut self, layer: LayerKey, shape: Shape) -> &mut Self {
        let v = self.shapes.entry(layer).or_insert(Vec::with_capacity(1));
        v.push(shape);
        self
    }

    pub fn set_net(&mut self, net: impl Into<String>) -> &mut Self {
        self.net = net.into();
        self
    }

    /// Adds the shapes of `other` to this [`AbstractPort`].
    pub fn merge(&mut self, other: Self) {
        for (k, mut v) in other.shapes.into_iter() {
            let shapes = self.shapes.entry(k).or_insert(Vec::new());
            shapes.append(&mut v);
        }
    }

    pub fn bbox(&self, layer: LayerKey) -> Option<BoundBox> {
        if let Some(shapes) = self.shapes.get(&layer) {
            let mut bbox = BoundBox::empty();
            for s in shapes {
                bbox = s.union(&bbox);
            }
            Some(bbox)
        } else {
            None
        }
    }

    pub fn largest_rect(&self, layer: LayerKey) -> Option<Rect> {
        let shapes = self.shapes.get(&layer)?;
        let mut best = None;
        let mut best_area = 0;
        for s in shapes {
            let r = match s {
                Shape::Rect(r) => r,
                _ => continue,
            };
            let area = r.area();
            if area > best_area {
                best_area = area;
                best = Some(*r)
            }
        }
        best
    }
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
    pub cells: PtrList<Cell>,
}

impl Library {
    /// Create a new and empty Library
    pub fn new(name: impl Into<String>, units: Units) -> Self {
        Self {
            name: name.into(),
            units,
            ..Default::default()
        }
    }

    /// Finds the cell with the given name, if one exists.
    ///
    /// Note that this operation takes `O(n)` time in the worst
    /// case, where `n` is the number of cells in the library.
    pub fn cell(&self, name: &str) -> Option<Ptr<Cell>> {
        self.cells
            .iter()
            .find(|c| {
                let c = c.read().unwrap();
                c.name == name
            })
            .cloned()
    }
}

/// # Dependency-Orderer
#[derive(Debug)]
pub struct DepOrder<'lib> {
    // FIXME: move to utils shared version
    lib: &'lib Library,
    stack: Vec<Ptr<Cell>>,
    seen: HashSet<Ptr<Cell>>,
}
impl<'lib> DepOrder<'lib> {
    pub fn order(lib: &'lib Library) -> Vec<Ptr<Cell>> {
        let mut myself = Self {
            lib,
            stack: Vec::new(),
            seen: HashSet::new(),
        };
        for cell in myself.lib.cells.iter() {
            myself.push(cell);
        }
        myself.stack
    }
    pub fn push(&mut self, ptr: &Ptr<Cell>) {
        // If the Cell hasn't already been visited, depth-first search it
        if !self.seen.contains(&ptr) {
            // Read the cell-pointer
            let cell = ptr.read().unwrap();
            // If the cell has an implementation, visit its [Instance]s before inserting it
            if let Some(layout) = &cell.layout {
                for inst in &layout.insts {
                    self.push(&inst.cell);
                }
            }
            // And insert the cell (pointer) itself
            self.seen.insert(Ptr::clone(ptr));
            self.stack.push(Ptr::clone(ptr));
        }
    }
}

/// Collection of the Views describing a Cell
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Cell {
    // Cell Name
    pub name: String,
    // Layout Abstract
    pub abs: Option<Abstract>,
    // Layout Implementation
    pub layout: Option<Layout>,
}
impl Cell {
    /// Create a new and empty Cell named `name`.
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();
        Self {
            name,
            ..Default::default()
        }
    }

    /// Creates a cell with empty abstract and layout views.
    pub fn empty<S>(name: S) -> Self
    where
        S: Clone + Into<String>,
    {
        let abs = Some(Abstract::new(name.clone()));
        let layout = Some(Layout::new(name.clone()));
        let name = name.into();
        Self { name, abs, layout }
    }

    #[inline]
    pub fn has_abstract(&self) -> bool {
        self.abs.is_some()
    }

    /// Gets a mutable reference to this cell's [`Layout`].
    ///
    /// Panics if the cell does not have a layout view.
    #[inline]
    pub fn layout_mut(&mut self) -> &mut Layout {
        self.layout.as_mut().unwrap()
    }

    /// Gets an immutable reference to this cell's [`Layout`].
    ///
    /// Panics if the cell does not have a layout view.
    #[inline]
    pub fn layout(&self) -> &Layout {
        self.layout.as_ref().unwrap()
    }

    /// Gets an immutable reference to this cell's [`Abstract`].
    ///
    /// Panics if the cell does not have an abstract view.
    #[inline]
    pub fn abs(&self) -> &Abstract {
        self.abs.as_ref().unwrap()
    }

    /// Gets a mutable reference to this cell's [`Abstract`].
    ///
    /// Panics if the cell does not have an abstract view.
    #[inline]
    pub fn abs_mut(&mut self) -> &mut Abstract {
        self.abs.as_mut().unwrap()
    }

    /// Adds a new pin to the layout and abstract views of this cell, if they exist.
    pub fn add_pin(&mut self, net: impl Into<String>, layer: LayerKey, rect: Rect) {
        let net = net.into();
        if let Some(ref mut layout) = self.layout {
            layout.add_pin(net.clone(), layer, rect);
        }
        if let Some(ref mut abs) = self.abs {
            let mut port = AbstractPort::new(net);
            port.add_shape(layer, Shape::Rect(rect));
            abs.add_port(port);
        }
    }

    /// Adds a new pin to the layout and abstract views of this cell, if they exist.
    pub fn add_pin_from_port(&mut self, port: AbstractPort, layer: LayerKey) {
        if let Some(ref mut layout) = self.layout {
            let rect = port.largest_rect(layer).unwrap();
            layout.add_pin(port.net.clone(), layer, rect);
        }
        if let Some(ref mut abs) = self.abs {
            abs.add_port(port);
        }
    }
}

impl From<Abstract> for Cell {
    fn from(src: Abstract) -> Self {
        Self {
            name: src.name.clone(),
            abs: Some(src),
            ..Default::default()
        }
    }
}
impl From<Layout> for Cell {
    fn from(src: Layout) -> Self {
        Self {
            name: src.name.clone(),
            layout: Some(src),
            ..Default::default()
        }
    }
}

/// # Raw-Layout Implementation
///
/// The geometric-level layout-definition of a [Cell].
/// Comprised of geometric [Element]s and instances of other [Cell] [Layout]s.
///
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Layout {
    /// Cell Name
    pub name: String,
    /// Instances
    pub insts: Vec<Instance>,
    /// Primitive/ Geometric Elements
    pub elems: Vec<Element>,
    /// Text Annotations
    pub annotations: Vec<TextElement>,
}
impl Layout {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    /// Create a rectangular [BoundBox] surrounding all elements in the [Layout].
    pub fn bbox(&self) -> BoundBox {
        let mut bbox = BoundBox::empty();
        for elem in &self.elems {
            bbox = elem.inner.union(&bbox);
        }
        for inst in &self.insts {
            let b = inst.bbox();
            if !b.is_empty() {
                let s = Shape::Rect(Rect { p0: b.p0, p1: b.p1 });
                bbox = s.union(&bbox);
            }
        }
        bbox
    }

    pub fn add_inst<T>(&mut self, inst: T)
    where
        T: Into<Instance>,
    {
        self.insts.push(inst.into());
    }

    pub fn add<T>(&mut self, elem: T)
    where
        T: Into<Element>,
    {
        self.elems.push(elem.into());
    }

    /// Flatten a [Layout], particularly its hierarchical instances, to a vector of [Element]s
    pub fn flatten(&self) -> LayoutResult<Vec<Element>> {
        // Kick off recursive calls, with the identity-transform applied for the top-level `layout`
        let mut elems = Vec::new();
        flatten_helper(self, &Transform::identity(), &mut elems)?;
        Ok(elems)
    }

    /// Creates a physical layout pin with the given layer, position and name.
    pub fn add_pin(&mut self, net: impl Into<String>, layer: LayerKey, rect: Rect) {
        self.elems.push(Element {
            net: Some(net.into()),
            layer,
            inner: Shape::Rect(rect),
            purpose: LayerPurpose::Pin,
        });
    }

    /// Draws a rectangle on the given layer.
    pub fn draw_rect(&mut self, layer: LayerKey, rect: Rect) {
        self.elems.push(Element {
            net: None,
            layer,
            inner: Shape::Rect(rect),
            purpose: LayerPurpose::Drawing,
        });
    }
}
/// Internal helper and core logic for [Layout::flatten].
fn flatten_helper(
    layout: &Layout,
    trans: &Transform,
    elems: &mut Vec<Element>,
) -> LayoutResult<()> {
    // Translate each geometric element
    for elem in layout.elems.iter() {
        // Clone all other data (layer, net, etc.)
        // FIXME: hierarchy flattening of net labels
        let mut new_elem = elem.clone();
        // And translate the inner shape by `trans`
        new_elem.inner = elem.inner.transform(trans);
        elems.push(new_elem);
    }
    // Note text-valued "annotations" are ignored

    // Visit all of `layout`'s instances, recursively getting their elements
    for inst in &layout.insts {
        // Get the cell's layout-definition, or fail
        let cell = inst.cell.read()?;
        let layout = cell.layout.as_ref().unwrap();

        // Create a new [Transform], cascading the parent's and instance's
        let inst_trans = Transform::from_instance(&inst.loc, inst.reflect_vert, inst.angle);
        let trans = Transform::cascade(&trans, &inst_trans);

        // And recursively add its elements
        flatten_helper(&layout, &trans, elems)?;
    }
    Ok(())
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
///
/// Primary unit of [Layout] definition.
/// Combines a geometric [Shape] with a z-axis [Layer],
/// and optional net connectivity annotation.
///
#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
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

/// Location, orientation, and angular rotation for an [Instance]
/// Note these fields exist "flat" in [Instance] as well,
/// and are grouped here for convenience.

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct InstancePlace {
    /// Location of `cell` origin
    /// regardless of rotation or reflection
    pub loc: Point,
    /// Vertical reflection,
    /// applied *before* rotation
    pub reflect_vert: bool,
    /// Angle of rotation (degrees),
    /// Clockwise and applied *after* reflection
    pub angle: Option<f64>,
}

// pub struct Flatten<'l> {
//     lib: &'l Library,
//     top: &'l Layout,
// }

// impl Iterator for Flatten {
//     type Item = Element;
//     fn next(&mut self) -> Option<Self::Item> {
//         unimplemented!()
//     }
// }
