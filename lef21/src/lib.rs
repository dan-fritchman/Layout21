//!
//! # Lef21 Library Exchange Format (LEF) Parser & Writer
//!
//! [Library Exchange Format (LEF)](https://en.wikipedia.org/wiki/Library_Exchange_Format)
//! is an ASCII-based format for integrated circuit (IC) layout and technology.
//!
//! LEF is near-ubiquitously used IC-industry-wide for two related purposes:
//!
//! * LEF *design libraries*, primarily comprised of LEF *macros*, provide the *physical abstract* view of circuit designs.
//!   * Such abstract-views are commonly the target for layout-synthesis programs ("place and route").
//!   * They include a circuit's pin locations and requirements for physical blockages ("obstructions"), among other metadata, typically without including the circuit's internal implementation.
//! * LEF *technology descriptions* ("tech-lef") provide a concise description of design-rules for assembling such cells, as commonly performed by layout-synthesis software.
//!
//! Lef21 includes comprehensive support for parsing and writing LEF *design libraries*, primarily stored as its [`LefLibrary`] and [`LefMacro`] types.
//! A select subset of tech-lef features are also supported, particularly those which blur the lines between technology and library data.
//!
//! ## Usage
//!
//! Creating a [`LefLibrary`] from file solely requires a call to the [`LefLibrary::open`] method:
//!
//! ```skip
//! use lef21::LefLibrary;
//! let lib = LefLibrary::open("mylib.lef")?;
//! ```
//!
//! Each [`LefLibrary`] is a short tree of macro-definitions, which are in turn primarily comprised of pin-definitions and obstructions.
//! This [`LefLibrary`] tree is of the form:
//!
//! * [`LefLibrary`]
//!   * Library Metadata
//!   * Macro Definitions, stored as Vec<[`LefMacro`]>
//!     * Macro Metadata
//!     * Blockages / Obstructions, stored as Vec<[`LefLayerGeometries`]>
//!     * Pin Definitions, stored as Vec<[`LefPin`]>
//!       * Pin Metadata
//!       * Port Definitions, stored as Vec<[`LefPort`]>
//!
//! All fields of all layers in the [`LefLibrary`] tree are publicly accessible and modifiable.
//!
//! Lef21 libraries can be saved to file with their [`LefLibrary::save`] method:
//!
//! ```skip
//! lib.save("yourlib.lef")?;
//! ```
//!
//! Or converted to in-memory LEF-format [String]s via [`LefLibrary::to_string`]:
//!
//! ```skip
//! let s = lib.to_string()?;
//! println!({}, s);
//! ```
//!
//! ## Serialization
//!
//! [`LefLibrary`], all underlying data structures, and all Lef21's other primary data stores are [`serde`](https://crates.io/crates/serde) serializable,
//! and can be straightforwardly converted to and from any serde-compatible format. Examples:
//!
//! ```skip
//! let lib = lef21::LefLibrary::new();
//! let json = serde_json::to_string(&lib);
//! let yaml = serde_yaml::to_string(&lib);
//! let toml = toml::to_string(&lib);
//! ```
//!
//! Lef21 includes built-in support for a subset of serde-formats via its [`SerializationFormat`] enumeration,
//! and support for directly reading and writing files in each format via its accompanying [`SerdeFile`] trait.
//! Example using [`SerializationFormat::Yaml`]:
//!
//! ```skip
//! use lef21::SerializationFormat::Yaml;
//! let lib = lef21::LefLibrary::new();
//!
//! // Write to YAML-format file
//! Yaml.save(&lib, "mylib.lef.yaml")?;
//! // And read back from file
//! let lib2: lef21::LefLibrary = Yaml.open("mylib.lef.yaml")?;  
//! ```
//!
//! ## Background
//!
//! Lef21 is a subset of the larger [Layout21](https://github.com/dan-fritchman/Layout21) library, and is primarily used as an import and export layer.
//! Lef21 correspondingly uses the LEF format's concepts, idioms, and terminology (e.g. "macro" vs. "cell") throughout.
//! Its LEF data structures are nonetheless designed for direct manipulation, for example in programmatically modifying existing LEF content.
//!
//! LEF is frequently paired with the DEF ASCII-based format for specifying circuit's internal physical implementations.
//! More common industry usage pairs LEF with [GDSII](https://crates.io/crates/gds21)'s binary implementation format,
//! which dramatically reduces data-sizes for large circuits.
//! DEF is not supported by Lef21. GDSII is supported by the related [gds21](https://crates.io/crates/gds21) crate.  
//!
//! ## License
//!
//! Lef21 and Layout21 are published under a permissive BSD license.
//!
//! The LEF format was originally designed by Tangent Systems, later acquired by Cadence Design Systems.
//! Lef21 holds no relationship to either entity, nor any authority or ownership over the format.
//! Countless LEF-format design descriptions are [freely available](https://github.com/google/skywater-pdk-libs-sky130_fd_sc_hd/blob/main/cells/mux2/sky130_fd_sc_hd__mux2_1.lef)
//! as open-source software.
//! Their examples serve as the basis for Lef21.
//!

// Crates.io imports, included here for macros
#[macro_use]
extern crate fstrings;

// Layout21 imports & re-exports
pub(crate) use layout21utils as utils;
pub use utils::{SerdeFile, SerializationFormat};

// Local modules & re-exports
mod data;
#[doc(inline)]
pub use data::*;
mod read;
mod write;
pub use write::save;

// Unit tests
#[cfg(test)]
mod tests;
