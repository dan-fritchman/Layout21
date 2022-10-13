//!
//! # Interfaces Module
//!
//! Describing Cells in terms of their IO Interfaces
//!

// Crates.io Imports
use serde::{Deserialize, Serialize};

/// # Port
///
/// Logical port, as in a netlist or HDL description.
/// Includes scalar, vector (bus), and bundle-valued ports.
/// Does not include physical/ geometric information.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Port {
    /// Port Name
    pub name: String,
    /// Port Type & Content
    pub kind: PortKind,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PortKind {
    /// Flat Scalar Port, e.g. `clk`
    Scalar,
    /// Array-Based Port, e.g. `data[31:0]`
    Array { width: usize },
    /// Instance of a Hierarchical Bundle
    Bundle { bundle_name: String },
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bundle {
    pub name: String,
    pub ports: Vec<Port>,
}
