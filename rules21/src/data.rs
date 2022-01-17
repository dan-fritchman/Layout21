use std::collections::HashMap;

// Local Imports
use layout21raw as raw;
use layout21utils as utils;
use utils::ptr::Ptr;

/// # Rules for Circuit-Extraction from a Layout
///
/// As typically performed as part of a layout-vs-schematic check,
/// or as the early stages of a parasitic extraction.
///
pub struct CircuitExtractionRules {
    pub layers: Vec<Layer>,
    pub devices: Vec<Device>,
    pub connect_rules: Vec<ConnectRule>,
}

fn extract_instances(
    layout: &raw::Layout,
    rules: &CircuitExtractionRules,
) -> Result<Vec<Instance>, ()> {
    // // Flatten `layout`, replacing all layout-instances with raw shapes
    // let flat = flatten(layout);
    // // Associate each shape in `layout` with one of `rules` layers, or fail
    // map_some_layer_thing(layout_layers, rules.layers);
    // let mut instances = Vec::new();
    // // For each device-definition in `rules`, search each shape in `layout` for a match
    // for device in rules.devices.iter() {
    //     for id_layer_shape in shapes[device.id_layer.read()].iter() {
    //         for port_layer in device.ports.iter() {
    //             for port_layer_shape in shapes[port_layer.read()].iter() {
    //                 if everything_overlaps {
    //                     instances.push(Instance::new(/* stuff */));
    //                 }
    //             }
    //         }
    //     }
    // }

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
    pub id_layer: Ptr<Layer>,
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
    pub layer: Ptr<Layer>,
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
