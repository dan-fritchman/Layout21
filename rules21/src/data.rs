//!
//! # Layout Rules Data Model
//!
//! Defines the core types for connectivity and validity of raw-geometry-style layout.
//!

// Std-library imports
use std::collections::{HashMap, HashSet};

// Crates.io
use slotmap::{new_key_type, SlotMap};

// Local Imports
use layout21raw as raw;
use layout21utils as utils;
use utils::ptr::{Ptr, PtrList};

// Create key-types for each internal type stored in [SlotMap]s
new_key_type! {
    pub struct ElementKey;
}

/// # Rules for Circuit-Extraction from a Layout
///
/// As typically performed as part of a layout-vs-schematic check,
/// or as the early stages of a parasitic extraction.
///
pub struct CircuitExtractionRules {
    pub layers: Vec<Layer>,
    pub devices: PtrList<Device>,
    pub connect_rules: Vec<ConnectRule>,
}

pub struct LayerElements {
    pub layer: Ptr<Layer>,
    pub elements: Vec<raw::Element>,
}
#[derive(Debug, Default)]
struct FlatLayout {
    pub net_names: HashSet<String>,
    pub by_net: HashMap<String, Vec<ElementKey>>,
    pub by_layer: HashMap<(raw::LayerKey, raw::LayerPurpose), Vec<ElementKey>>,
    pub elements: SlotMap<ElementKey, raw::Element>,
}
impl FlatLayout {
    pub fn from_raw(layout: &raw::Layout) -> Self {
        let mut this = Self::default();

        // Flatten any hierarchy in the input layout, creating a flat vector of shape-elements
        let elements = layout.flatten().unwrap();
        for element in elements {
            // Add the element to our slot-mapped set, and get a key for it
            let ekey = this.elements.insert(element);
            let element = &this.elements[ekey];

            // If the element has a labeled net, add it to the by-net set
            if let Some(ref net_name) = element.net {
                this.net_names.insert(net_name.to_string());
                this.by_net
                    .entry(net_name.to_string())
                    .or_insert_with(Vec::new)
                    .push(ekey);
            }
            // And add it to the by-layer set
            this.by_layer
                .entry((element.layer, element.purpose.clone()))
                .or_insert_with(Vec::new)
                .push(ekey);
        }
        this
    }
}

fn extract_instances(
    layout: &raw::Layout,
    rules: &CircuitExtractionRules,
) -> Result<HashMap<Ptr<Device>, Vec<Instance>>, ()> {
    // Flatten `layout`, replacing all layout-instances with raw shapes.
    // This also gives us ownership of the flattened shapes.
    let layout = FlatLayout::from_raw(layout);

    // Associate each shape in `layout` with one of `rules` layers, or fail
    // map_some_layer_thing(layout_layers, rules.layers);

    // For each device-definition in `rules`, search each shape in `layout` for a match
    let mut instances: HashMap<Ptr<Device>, Vec<Instance>> = HashMap::new();
    for device in rules.devices.iter() {
        let device_instances = find_device_instances(&layout, &device.read().unwrap(), rules)?;
        instances.insert(device.clone(), device_instances);
    }
    Ok(instances)
}
/// Find all instances of `device` in `layout`
fn find_device_instances(
    layout: &FlatLayout,
    device: &Device,
    rules: &CircuitExtractionRules,
) -> Result<Vec<Instance>, ()> {
    let mut instances: Vec<Instance> = Vec::new();
    // Many devices have several ports on the same layer, e.g. MOS source and drain.
    // Get a unique hash-set of all relevant layers for intersection.
    let mut port_layers = HashSet::new();
    for port in &device.ports {
        port_layers.insert(port.layer.clone());
    }
    // // Check each polygon in the layout on the device's ID layer
    let id_layer = &device.id_layer;
    for id_layer_elem_key in layout.by_layer[id_layer].iter() {
        // Each ID-layer shape defines, or at least probably intends to define, an instance of this device.
        // Check for intersecting shapes in each of its port-layers.
        let id_layer_elem = &layout.elements[*id_layer_elem_key];
        if let Some(instance) =
            is_this_an_instance(layout, device, &port_layers, id_layer_elem, rules)?
        {
            instances.push(instance);
        }
    }
    Ok(instances)
}
/// Extract whether `id_layer_elem` creates an instance of `device`
fn is_this_an_instance(
    layout: &FlatLayout,
    device: &Device,
    port_layers: &HashSet<(raw::LayerKey, raw::LayerPurpose)>,
    id_layer_elem: &raw::Element,
    rules: &CircuitExtractionRules,
) -> Result<Option<Instance>, ()> {
    let mut intersecting_port_layer_elems: HashMap<
        (raw::LayerKey, raw::LayerPurpose),
        Vec<ElementKey>,
    > = HashMap::new();
    let mut nhits = 0;
    for port_layer in port_layers {
        for port_layer_elem_key in layout.by_layer[port_layer].iter() {
            let port_layer_elem = &layout.elements[*port_layer_elem_key];
            if port_layer_elem.inner.intersects(&id_layer_elem.inner) {
                intersecting_port_layer_elems
                    .entry(port_layer.clone())
                    .or_insert_with(Vec::new)
                    .push(*port_layer_elem_key);
                nhits += 1;
            }
            todo!();
        }
    }
    // Now sort out whether that set of intersections covers all of `device`s ports
    // Easy case: if we found fewer hits than ports, we can't be an instance
    if nhits < device.ports.len() {
        return Ok(None);
    }
    todo!()
}
pub struct PrimaryLayer {
    pub name: String,
    pub desc: String,
    pub id: u64,
    pub spec: LayerMapSpec,
}

pub struct LayerMapSpec {
    pub layernum: u32,
    pub datatype: u32,
}

pub enum LayerData {
    Primary(PrimaryLayer),
    Derived(DerivedLayer),
}
pub struct Layer {
    pub data: LayerData,
    pub connect_rules: Vec<ConnectRule>,
}

/// # Derived Layout Layer
///
/// Produced by a set of geometric expressions applied to other [Layer]s.
///
pub struct DerivedLayer {
    pub name: String,
    pub desc: String,
    pub id: u64,
    pub expr: GeomOp,
}

/// # Enumerated Geometric Operators
/// The primary evaluation-mechanism for derived layers
pub enum GeomOp {
    BinaryOp(BinaryOp),
}

/// # Binary Geometric Operation
///
/// Produces a [DerivedLayer] as a function of two other [Layer]s.
///
pub struct BinaryOp {
    pub op: BinaryOperator,
    pub lhs: Ptr<Layer>,
    pub rhs: Ptr<Layer>,
}

/// # Enumerated Binary Geometric Operations
pub enum BinaryOperator {
    Union,
    Intersection,
    Difference,
    Xor,
    /// Selects all shapes from [Layer] `lhs` that touch or are coincident with shapes in [Layer] `rhs`.
    Interact,
}

pub struct Device {
    pub name: String,
    pub desc: String,
    pub id: u64,
    /// Key-layer identifying the device
    pub id_layer: (raw::LayerKey, raw::LayerPurpose), // FIXME:  Ptr<Layer>,
    /// Port Layers
    pub ports: Vec<Port>,
    /// Additional, optional layers, which generally dictate device flavors or parameters
    pub optional_layers: Vec<Ptr<Layer>>,
    /// Set of parameters and evaluation functions
    pub params: Vec<ParamDeclaration>,
    /// Set of port-symmetries, i.e. ports that can be arbitrarily swapped.
    pub symmetries: Symmetries,
}

/// # Enumerated Types of Layout-Extractable [Device]
pub enum DeviceKind {
    Mos(MosKind),
    Bipolar(BipolarKind),
    Diode,
    Capacitor,
    Resistor,
}

pub enum MosKind {
    Nmos,
    Pmos,
}
pub enum BipolarKind {
    Npn,
    Pnp,
}

/// # Extracted Device Port
///
pub struct Port {
    pub name: String,
    pub layer: (raw::LayerKey, raw::LayerPurpose), // FIXME: Ptr<Layer>,
}

pub struct Symmetries;

pub struct ParamDeclaration {
    pub name: String,
    pub desc: String,
}

/// # Extracted Instance of a [Device]
pub struct Instance {
    pub name: String,
    pub id: u64,
    pub device: Ptr<Device>,
    pub shapes: Vec<Ptr<Shape>>,
    pub ports: HashMap<String, Ptr<Shape>>,
    pub params: HashMap<String, ParamValue>,
}
pub struct Shape;
pub struct ParamValue;

/// # Connection Rule to a [Layer]
///
/// Stored as attributes of each [Layer], indicating which [Layer]s are connected to it.
/// Optionally specifies a third `through` [Layer], typically a via,
/// which if specified is required to coincide with *both* `from` and `to` to form a connection.
pub struct ConnectRule {
    pub to: Ptr<Layer>,
    pub through: Option<Ptr<Layer>>,
}
