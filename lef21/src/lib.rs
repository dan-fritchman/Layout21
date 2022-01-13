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

// Std-Lib
use std::convert::TryFrom;
use std::path::Path;

// Crates.io Imports
use derive_more::{Add, AddAssign, Sub, SubAssign};
use once_cell::sync::Lazy;
#[allow(unused_imports)]
use rust_decimal::prelude::*;
use serde::{Deserialize, Serialize};
#[macro_use]
extern crate derive_builder;
#[macro_use]
extern crate fstrings;

// Local modules & re-exports
mod read;
mod write;
use layout21utils as utils;
pub use utils::{SerdeFile, SerializationFormat};

// Unit tests
#[cfg(test)]
mod tests;

/// # LefDecimal
/// Internal type alias for all decimal-valued data.
/// Uses [rust_decimal](https://crates.io/crates/rust_decimal) internally.
pub type LefDecimal = rust_decimal::Decimal;

// Static short-hands for two common [LefDecimal] values, both representing spec-versions
// Note [`once_cell`](https://docs.rs/once_cell/1.8.0/once_cell/#lazy-initialized-global-data)
// demands these be `static`, not `const`, for reasons outside our grasp.
static V5P4: Lazy<LefDecimal> = Lazy::new(|| LefDecimal::from_str("5.4").unwrap());
static V5P8: Lazy<LefDecimal> = Lazy::new(|| LefDecimal::from_str("5.8").unwrap());

/// # Lef Library  
///
/// LEF's primary design-content container, including a set of macro/cell definitions and associated metadata.
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into), private)]
pub struct LefLibrary {
    // Required
    /// Macro Definitions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub macros: Vec<LefMacro>,
    /// Site Definitions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub sites: Vec<LefSite>,

    // Optional
    /// Lef Spec Version
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub version: Option<LefDecimal>,
    /// Case-Sensitive Name Setting  
    /// Valid for LEF versions 5.4 and earlier
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub names_case_sensitive: Option<LefOnOff>,
    /// Wire-Extension Pin Settings  
    /// Valid for LEF versions 5.4 and earlier
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub no_wire_extension_at_pin: Option<LefOnOff>,
    /// Bus-Bit Separator Characters
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub bus_bit_chars: Option<(char, char)>,
    /// Divider Character
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub divider_char: Option<char>,
    /// Dimensional Units
    /// Recommended to be specified in a tech-lef. But turns up in libraries as well.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub units: Option<LefUnits>,

    // Unsupported
    /// Via Definitions (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub vias: Unsupported,
    /// Syntax Extensions (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub extensions: Unsupported,
}
impl LefLibrary {
    /// Create a new and initially empty [LefLibrary].  
    /// Also available via [Default].
    pub fn new() -> LefLibrary {
        LefLibrary::default()
    }
    /// Open a [LefLibrary] from file `fname`
    pub fn open(fname: impl AsRef<Path>) -> LefResult<LefLibrary> {
        read::parse_file(fname)
    }
    /// Write a [LefLibrary] to file `fname`.  
    pub fn save(&self, fname: impl AsRef<Path>) -> LefResult<()> {
        write::save(self, fname)
    }
    /// Write a [LefLibrary] to a LEF-format [String].  
    pub fn to_string(&self) -> LefResult<String> {
        write::to_string(self)
    }
}
/// # Lef Macro
///
/// The primary block-level construct comprising each [LefLibrary].
/// Defines a hardware-block's physical abstract, including:
/// * Pin definitions (`pins`) with locations, directions, and associated metadata
/// * Required blockage-obstructions (`obs`)
/// * A variety of other block-level metadata
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into), private)]
pub struct LefMacro {
    // Required
    /// Macro Name
    pub name: String,
    /// Pin List
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default)]
    pub pins: Vec<LefPin>,
    /// Obstructions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default)]
    pub obs: Vec<LefLayerGeometries>,

    // Optional
    /// Macro Class
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub class: Option<LefMacroClass>,
    /// Foreign (i.e. GDSII, DEF) Cell
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub foreign: Option<LefForeign>,
    /// X-Y Origin
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub origin: Option<LefPoint>,
    /// Outline Size
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub size: Option<(LefDecimal, LefDecimal)>,
    /// Rotational & Translation Symmetries
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub symmetry: Option<Vec<LefSymmetry>>,
    /// Site Name  
    /// Note the optional `SITEPATTERN` is not supported
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub site: Option<String>,
    /// Source  
    /// Valid for LEF versions 5.4 and earlier
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub source: Option<LefDefSource>,

    // Unsupported
    /// Fixed Mask Option (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub fixed_mask: Unsupported,
    /// Electrically-Equivalent Cell (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub eeq: Unsupported,
    /// Density Objects (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub density: Unsupported,
    /// Properties (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub properties: Unsupported,
}
impl LefMacro {
    /// Create a new and initially empty [LefMacro] with name `name`
    pub fn new(name: impl Into<String>) -> LefMacro {
        let name = name.into();
        LefMacro {
            name,
            ..Default::default()
        }
    }
}
/// # [LefMacro] Classes
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefMacroClass {
    Cover { bump: bool },
    Ring,
    Block { tp: Option<LefBlockClassType> },
    Pad { tp: Option<LefPadClassType> },
    Core { tp: Option<LefCoreClassType> },
    EndCap { tp: LefEndCapClassType },
}
/// # Lef Foreign Cell Declaration
///
/// Declares the linkage to another cell, commonly in DEF or GDSII format.
/// Foreign-cell references are stored exacty as in the LEF format: as a string cell-name.
/// The optional `ORIENT` feature is not supported.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefForeign {
    /// Foreign Cell Name
    pub cell_name: String,
    /// Location
    pub pt: Option<LefPoint>,

    // Unsupported Fields
    /// Orientation (Unsupported)
    #[serde(default, skip_serializing)]
    pub orient: Unsupported,
}
/// # Lef Pin Definition
///
/// A named, directed pin, including one or more "weakly connected" physical [LefPort]s.
#[derive(Clone, Default, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into), private)]
pub struct LefPin {
    // Required Fields
    /// Pin Name
    pub name: String,
    /// Port Geometries
    pub ports: Vec<LefPort>,

    // Optional Fields
    /// Direction
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub direction: Option<LefPinDirection>,
    /// Usage / Role
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[serde(rename(serialize = "use", deserialize = "use"))]
    #[builder(default, setter(strip_option))]
    pub use_: Option<LefPinUse>,
    /// Shape
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub shape: Option<LefPinShape>,
    /// Antenna Model
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub antenna_model: Option<LefAntennaModel>,
    /// Antenna Attributes
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub antenna_attrs: Vec<LefPinAntennaAttr>,

    // Unsupported
    /// Taper Rule (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub taper_rule: Unsupported,
    /// Net Expression (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub net_expr: Unsupported,
    /// Supply Sensitivity (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub supply_sensitivity: Unsupported,
    /// Ground Sensitivity (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub ground_sensitivity: Unsupported,
    /// Must-Join (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub must_join: Unsupported,
    /// Properties (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub properties: Unsupported,
}
/// # Lef Pin Direction
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefPinDirection {
    Input,
    Output { tristate: bool },
    Inout,
    FeedThru,
}
impl std::fmt::Display for LefPinDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            Self::Input => "INPUT",
            Self::Inout => "INOUT",
            Self::FeedThru => "FEEDTHRU",
            Self::Output { tristate: false } => "OUTPUT",
            Self::Output { tristate: true } => "OUTPUT TRISTATE",
        };
        write!(f, "{}", s)
    }
}
/// # Lef Antenna Attributes
///
/// Stored as key-value pairs from string-keys named "ANTENNA*" to [LefDecimal] values.
/// Note each pair may have an optional `layer` specifier,
/// and that each key may have multiple attributes, generally specifying different layers.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefPinAntennaAttr {
    key: String,
    val: LefDecimal,
    layer: Option<String>,
}
/// # Lef Port
///
/// Defines the physical locations and optional metadata of a port on a pin.
/// LEF includes the notion of multiple "weakly connected" ports per pin;
/// each [LefPort] is one such weakly-connected point.
#[derive(Clone, Default, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into), private)]
pub struct LefPort {
    /// Port-Class
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub class: Option<LefPortClass>,
    /// Layers & Geometries
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub layers: Vec<LefLayerGeometries>,
}
/// # Lef Single-Layer Geometry Store
///
/// Most LEF spatial data (e.g. ports, blockages) is organized by layer.
/// [LefLayerGeometries] stores the combination of a layer (name)
/// and suite of geometric primitives (e.g. rectangles, polygons) and vias on that layer.  
///
/// [LefLayerGeometries] are the primary building block of [LefPort]s and macro obstructions.
///
#[derive(Clone, Default, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into), private)]
pub struct LefLayerGeometries {
    // Required
    /// Layer Name
    pub layer_name: String,
    /// Geometries
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub geometries: Vec<LefGeometry>,
    /// Vias
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub vias: Vec<LefVia>,

    // Optional
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub except_pg_net: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub spacing: Option<LefLayerSpacing>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub width: Option<LefDecimal>,
}
/// # Lef Via Instance
///
/// A located instance of via-type `via_name`, typically used as part of a [LefLayerGeometries] definition.
/// The via-type is generally interpreted as a string-valued reference into tech-lef data.
/// It is stored in each [LefVia] exactly as in LEF, as a string type-name.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefVia {
    /// Via-Type Name
    pub via_name: String,
    /// Location
    pub pt: LefPoint,
}
/// # Enumerated Layer-Spacing Options
/// Includes absolute spacing and design-rule-width modifiers.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefLayerSpacing {
    Spacing(LefDecimal),
    DesignRuleWidth(LefDecimal),
}
/// # Lef Geometric Object Enumeration
/// Includes [LefShape]s and Iterators thereof
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefGeometry {
    /// Single Shape
    Shape(LefShape),
    /// Repeated Iteration/ Array of Shapes (Unsupported)
    Iterate {
        shape: LefShape,
        pattern: Unsupported,
    },
}
/// # Lef Shape Enumeration
/// Includes each of LEF's individual geometric primitives:
/// rectangles, polygons, and paths.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefShape {
    Rect(LefPoint, LefPoint),
    Polygon(Vec<LefPoint>),
    Path(Vec<LefPoint>),
}
/// # Lef X-Y Spatial Point
///
/// Specified in [LefDecimal]-valued Cartesian coordinates.  
/// Supports common mathematical operations (Add, Sub, increment, etc.).  
#[derive(
    Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq, Add, AddAssign, Sub, SubAssign,
)]
pub struct LefPoint {
    pub x: LefDecimal,
    pub y: LefDecimal,
}
impl LefPoint {
    /// Create a new [LefPoint]
    pub fn new(x: impl Into<LefDecimal>, y: impl Into<LefDecimal>) -> Self {
        Self {
            x: x.into(),
            y: y.into(),
        }
    }
}
impl std::fmt::Display for LefPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {}", self.x, self.y)
    }
}
/// # Lef Distance Units per Micron  
///
/// A constrained numeric type. Allowed values of [LefDbuPerMicron] are:
/// [100, 200, 400, 800, 1000, 2000, 4000, 8000, 10_000, 20_000].
/// Adherence to this set is checked at construction time.
///
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefDbuPerMicron(u32);
impl LefDbuPerMicron {
    /// Create a new [LefDbuPerMicron], checking internally required conditions
    pub fn try_new(x: LefDecimal) -> LefResult<Self> {
        // Check that this is an integer, no fractional part
        if !x.fract().is_zero() {
            return Err("DBU per Micron must be an integer".into());
        }
        // Check the for allowed values
        if ![100, 200, 400, 800, 1000, 2000, 4000, 8000, 10_000, 20_000].contains(&x.mantissa()) {
            return Err("Invalid DBU per Micron value".into());
        }
        // Convert to u32. Note the `unwrap` here is safe,
        // as we have already verified `mantissa` is in the list above,
        // all of which fit in a u32.
        let val = u32::try_from(x.mantissa()).unwrap();
        Ok(Self(val))
    }
    /// Return `self`'s value as an integer.
    pub fn value(&self) -> u32 {
        self.0
    }
}
/// # Lef Physical-Dimension Units
///
/// Conversion factors for a variety of physical quantities.  
/// Only the distance-measurement `database_microns` is supported.
///
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefUnits {
    /// Database Distance Units per Micron
    /// Defaults to 100, i.e. 1 DBU = 10nm
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub database_microns: Option<LefDbuPerMicron>,

    // Unsupported Fields
    #[serde(default, skip_serializing)]
    pub time_ns: Unsupported,
    #[serde(default, skip_serializing)]
    pub capacitance_pf: Unsupported,
    #[serde(default, skip_serializing)]
    pub resistance_ohms: Unsupported,
    #[serde(default, skip_serializing)]
    pub power_mw: Unsupported,
    #[serde(default, skip_serializing)]
    pub current_ma: Unsupported,
    #[serde(default, skip_serializing)]
    pub voltage_volts: Unsupported,
    #[serde(default, skip_serializing)]
    pub frequency_mhz: Unsupported,
}
/// # Lef Site Definition
///
/// Defines a placement-site in designs.
/// Dictates the placement grid for a family of macros.
///
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into), private)]
pub struct LefSite {
    // Required
    /// Site Name
    pub name: String,
    /// Site Class
    pub class: LefSiteClass,
    /// Size
    pub size: (LefDecimal, LefDecimal),

    // Optional
    /// Rotational & Translation Symmetries
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub symmetry: Option<Vec<LefSymmetry>>,

    // Unsupported
    /// Row Patterns, re other previously defined sites (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub row_pattern: Unsupported,
}
/// # Unsupported Feature
///
/// Empty placeholder struct for unsupported LEF features.
/// These fields are largely included for documentation purposes.
/// They are never parsed, never written or serialized, and can only be set to the zero-size [Unsupported] value.
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Unsupported;

/// # Lef String-Enumeration Trait
///
/// Defines two central methods:
/// * `to_str(&self) -> &'static str` converts the enum to its Lef-String values.
/// * `from_str(&str) -> Option<Self>` does the opposite, returning an [Option] indicator of success or failure.
///
trait LefEnum: std::marker::Sized {
    fn to_str(&self) -> &'static str;
    fn from_str(txt: &str) -> Option<Self>;
}
/// Macro for creating `enum`s which:
/// * (a) Have paired string-values, as commonly arrive in enumerated LEF fields such as "ON" / "OFF", "CLASS", etc, and
/// * (b) Automatically implement the [LefEnum] trait for conversions to and from these strings.
/// * (c) Automatically implement [std::fmt::Display] writing the string-values
/// All variants are fieldless, and include derived implementations of common traits notably including `serde::{Serialize,Deserialize}`.
macro_rules! enumstr {
    (   $(#[$meta: meta])*
        $enum_name: ident {
        $( $variant: ident : $strval: literal ),* $(,)?
    }) => {
        $(#[$meta])*
        #[allow(dead_code)]
        #[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
        pub enum $enum_name {
            $( #[doc=$strval]
                $variant ),*
        }
        impl LefEnum for $enum_name {
            /// Convert a [$enum_name] to the (static) string-keyword used in the Lef format
            #[allow(dead_code)]
            fn to_str(&self) -> &'static str {
                match self {
                    $( Self::$variant => $strval),*,
                }
            }
            /// Create a [$enum_name] from one of the string-values specified in the Lef format.
            /// Returns `None` if input `txt` does not match one of [$enum_name]'s variants.
            /// Note `from_str` is case *sensitive*, i.e. uses a native string comparison.
            /// Conversion to case-insensitive matching generall requires re-casing outside `from_str`.
            fn from_str(txt: &str) -> Option<Self> {
                match txt {
                    $( $strval => Some(Self::$variant)),*,
                    _ => None,
                }
            }
        }
        impl ::std::fmt::Display for $enum_name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                let s = match self {
                    $( Self::$variant => $strval),*,
                };
                write!(f, "{}", s)
            }
        }
    }
}
enumstr!(
    /// # Lef Key(Word)s
    ///
    /// Enumerated "key(word)s" used in LEF parsing and generation.
    ///
    /// Unlike typical programming languages, LEF does not really have *keywords*  in the sense of being reserved at all points in the program.
    /// Legality of [LefKey]s is instead context-dependent, more as in a YAML or JSON schema.
    /// For example a [LefMacro] is free to use the name "MACRO" for one of its pins, whereas while during [LefLibrary] definition, "MACRO" is a key with special meaning.
    ///
    /// LEF syntax is case-insensitive. [LefKey]s are always written in the (conventional) upper-case form, but are parsed case-insensitively.
    ///
    LefKey {
        Library: "LIBRARY",
        Version: "VERSION",
        Foreign: "FOREIGN",
        Origin: "ORIGIN",
        Source: "SOURCE",
        NamesCaseSensitive: "NAMESCASESENSITIVE",
        NoWireExtensionAtPin: "NOWIREEXTENSIONATPIN",
        Macro: "MACRO",
        End: "END",
        Pin: "PIN",
        Port: "PORT",
        Obs: "OBS",
        Layer: "LAYER",
        Direction: "DIRECTION",
        Use: "USE",
        Shape: "SHAPE",
        Path: "PATH",
        Polygon: "POLYGON",
        Rect: "RECT",
        Via: "VIA",
        Width: "WIDTH",
        Class: "CLASS",
        Symmetry: "SYMMETRY",
        RowPattern: "ROWPATTERN",
        Site: "SITE",
        Size: "SIZE",
        By: "BY",
        BusBitChars: "BUSBITCHARS",
        DividerChar: "DIVIDERCHAR",
        Units: "UNITS",
        BeginExtension: "BEGINEXT",
        Tristate: "TRISTATE",
        Input: "INPUT",
        Output: "OUTPUT",
        Inout: "INOUT",
        FeedThru: "FEEDTHRU",
        ExceptPgNet: "EXCEPTPGNET",
        DesignRuleWidth: "DESIGNRULEWIDTH",
        Spacing: "SPACING",
        Bump: "BUMP",

        // ANTENNA Fields
        AntennaModel: "ANTENNAMODEL",
        AntennaDiffArea: "ANTENNADIFFAREA",
        AntennaGateArea: "ANTENNAGATEAREA",
        AntennaPartialMetalArea: "ANTENNAPARTIALMETALAREA",
        AntennaPartialMetalSideArea: "ANTENNAPARTIALMETALSIDEAREA",
        AntennaPartialCutArea: "ANTENNAPARTIALCUTAREA",
        AntennaPartialDiffArea: "ANTENNAPARTIALDIFFAREA",
        AntennaMaxAreaCar: "ANTENNAMAXAREACAR",
        AntennaMaxSideAreaCar: "ANTENNAMAXSIDEAREACAR",
        AntennaMaxCutCar: "ANTENNAMAXCUTCAR",

        // Unsupported
        TaperRule: "TAPERRULE",
        NetExpr: "NETEXPR",
        SupplySensitivity: "SUPPLYSENSITIVITY",
        GroundSensitivity: "GROUNDSENSITIVITY",
        MustJoin: "MUSTJOIN",
        Property: "PROPERTY",
    }
);
impl LefKey {
    /// Lef Key parsing, performed case-insensitively by internally converting to upper-case.
    fn parse(txt: &str) -> Option<Self> {
        Self::from_str(&txt.to_ascii_uppercase())
    }
}
enumstr!(
    /// Binary On/Off Settings, Denoted by `ON` and `OFF`
    LefOnOff {
        On: "ON",
        Off: "OFF",
    }
);
enumstr!(
    /// # Lef/ Def `SOURCE`
    ///
    /// Specifies the source of a component
    /// In all versions since at least 5.7 (2009), SOURCE is a DEF-only field on COMPONENT definitions.
    /// Prior versions also include this as a field for LEF MACRO definitions.
    LefDefSource {
        Netlist: "NETLIST",
        Dist: "DIST",
        Timing: "TIMING",
        User: "USER",
    }
);
enumstr!(
    /// Specifies which MACRO orientations are valid for placement
    LefSymmetry {
        X: "X",
        Y: "Y",
        R90: "R90"
    }
);
enumstr!(
    /// # Lef Pin-Usage
    ///
    /// Specifies the usage-intent for a [LefPin].
    /// Note this is the noun form of "use", pronounced with the hard "s" -
    /// not the verb form pronounced like the New Jersey second-person plural "yous".
    LefPinUse {
        Signal: "SIGNAL",
        Analog: "ANALOG",
        Power: "POWER",
        Ground: "GROUND",
        Clock: "CLOCK",
    }
);
enumstr!(
    /// Specifies a pin with special connection requirements because of its shape
    LefPinShape {
        Abutment: "ABUTMENT",
        Ring: "RING",
        FeedThru: "FEEDTHRU",
    }
);
enumstr!(
    /// Identifiers for the enumerated [LefMacroClass]es
    LefMacroClassName {
        Block: "BLOCK",
        Pad: "PAD",
        Core: "CORE",
        EndCap: "ENDCAP",
        Cover: "COVER",
        Ring: "RING"
    }
);
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass::Pad]
    LefPadClassType {
        Input: "INPUT",
        Output: "OUTPUT",
        Inout: "INOUT",
        Power: "POWER",
        Spacer: "SPACER",
        AreaIo: "AREAIO",
    }
);
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass::EndCap]
    LefEndCapClassType {
        Pre: "PRE",
        Post: "POST",
        TopLeft: "TOPLEFT",
        TopRight: "TOPRIGHT",
        BottomLeft: "BOTTOMLEFT",
        BottomRight: "BOTTOMRIGHT",
    }
);
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass::Block]
    LefBlockClassType {
        BlackBox: "BLACKBOX",
        Soft: "SOFT"
    }
);
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass::Core]
    LefCoreClassType {
        FeedThru: "FEEDTHRU",
        TieHigh: "TIEHIGH",
        TieLow: "TIELOW",
        Spacer: "SPACER",
        AntennaCell: "ANTENNACELL",
        WellTap: "WELLTAP",
    }
);
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass::Core]
    LefPortClass {
        None: "NONE",
        Core: "CORE",
        Bump: "BUMP",
    }
);
enumstr!(
    /// [LefSite] Classes
    LefSiteClass {
        Pad: "PAD",
        Core: "CORE",
    }
);
enumstr!(
    /// Antenna Models
    LefAntennaModel {
        Oxide1: "OXIDE1",
        Oxide2: "OXIDE2",
        Oxide3: "OXIDE3",
        Oxide4: "OXIDE4",
    }
);

/// # Lef Error Enumeration
#[derive(Debug)]
pub enum LefError {
    /// Lexer Errors
    Lex {
        next_char: Option<char>,
        line: usize,
        pos: usize,
    },
    /// Parser Errors
    Parse {
        tp: read::LefParseErrorType,
        ctx: Vec<read::LefParseContext>,
        token: String,
        line_content: String,
        line_num: usize,
        pos: usize,
    },
    /// Wrapped errors, generally from other crates
    Boxed(Box<dyn std::error::Error>),
    /// String message-valued errors
    Str(String),
}
impl From<utils::ser::Error> for LefError {
    fn from(e: utils::ser::Error) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl From<std::io::Error> for LefError {
    fn from(e: std::io::Error) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl From<rust_decimal::Error> for LefError {
    fn from(e: rust_decimal::Error) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl From<String> for LefError {
    /// Convert string-based errors by wrapping them
    fn from(e: String) -> Self {
        Self::Str(e)
    }
}
impl From<&str> for LefError {
    /// Convert string-based errors by wrapping them
    fn from(e: &str) -> Self {
        Self::Str(e.into())
    }
}
// One of these days, this way is gonna way, and we'll delete all these specific error-types above.
// impl<E: std::error::Error> From<E> for LefError {
//     /// Wrap External Errors
//     fn from(e: E) -> Self {
//         Self::Boxed(Box::new(e))
//     }
// }

/// Lef21 Library-Wide Result Type
pub type LefResult<T> = Result<T, LefError>;

// Implement the serialization to/from file trait for libraries and macros
impl utils::SerdeFile for LefLibrary {}
impl utils::SerdeFile for LefMacro {}
