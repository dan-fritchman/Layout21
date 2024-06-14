//!
//! # Lef Data Model
//!
//!

// Std-Lib
use std::convert::TryFrom;
use std::path::Path;

// Crates.io Imports
use derive_builder::Builder;
use derive_more::{Add, AddAssign, Sub, SubAssign};
use once_cell::sync::Lazy;
#[allow(unused_imports)]
use rust_decimal::prelude::*;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

// Layout21 Imports
use crate::utils::{enumstr, EnumStr};

///
/// # LefDecimal
///
/// Internal type alias for all decimal-valued data.
/// Uses [rust_decimal](https://crates.io/crates/rust_decimal) internally.
///
pub type LefDecimal = rust_decimal::Decimal;

// Static short-hands for two common [LefDecimal] values, both representing spec-versions
// Note [`once_cell`](https://docs.rs/once_cell/1.8.0/once_cell/#lazy-initialized-global-data)
// demands these be `static`, not `const`, for reasons outside our grasp.
pub(crate) static V5P4: Lazy<LefDecimal> = Lazy::new(|| LefDecimal::from_str("5.4").unwrap());
pub(crate) static V5P8: Lazy<LefDecimal> = Lazy::new(|| LefDecimal::from_str("5.8").unwrap());

/// # Lef Library
///
/// LEF's primary design-content container, including a set of macro/cell definitions and associated metadata.
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into))]
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

    // Unsupported fields recommended for *either* LEF "cell libraries" or "technologies"
    /// Via Definitions (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub vias: Option<Unsupported>,
    /// Syntax Extensions (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub extensions: Option<Unsupported>,
    // Fields recommended for LEF technology descriptions, AKA "tech-lefs"
    /// Manufacturing Grid
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub manufacturing_grid: Option<Unsupported>,
    /// "Use Min Spacing" Option
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub use_min_spacing: Option<LefOnOff>,
    /// Clearance Measure
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub clearance_measure: Option<Unsupported>,
    /// Property Definitions
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub property_definitions: Option<Unsupported>,
    /// Layer Definitions
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub layers: Option<Unsupported>,
    /// Max Via Stack
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub max_via_stack: Option<Unsupported>,
    /// Via Rules
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub via_rules: Option<Unsupported>,
    /// Via Rules Generators
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub via_rule_generators: Option<Unsupported>,
    /// Non Default Rules
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub non_default_rules: Option<Unsupported>,
}
impl LefLibrary {
    /// Create a new and initially empty [LefLibrary].  
    /// Also available via [Default].
    pub fn new() -> LefLibrary {
        LefLibrary::default()
    }
    /// Open a [LefLibrary] from file `fname`
    pub fn open(fname: impl AsRef<Path>) -> LefResult<LefLibrary> {
        super::read::parse_file(fname)
    }
    /// Write a [LefLibrary] to file `fname`.  
    pub fn save(&self, fname: impl AsRef<Path>) -> LefResult<()> {
        super::write::save(self, fname)
    }
    /// Write a [LefLibrary] to a LEF-format [String].  
    pub fn to_string(&self) -> LefResult<String> {
        super::write::to_string(self)
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
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into))]
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
    pub fixed_mask: Option<Unsupported>,
    /// Electrically-Equivalent Cell (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub eeq: Option<Unsupported>,
    /// Density Objects (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub density: Option<Unsupported>,
    /// Properties (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub properties: Option<Unsupported>,
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
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
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
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub struct LefForeign {
    /// Foreign Cell Name
    pub cell_name: String,
    /// Location
    pub pt: Option<LefPoint>,

    // Unsupported Fields
    /// Orientation (Unsupported)
    #[serde(default, skip_serializing)]
    pub orient: Option<Unsupported>,
}
/// # Lef Pin Definition
///
/// A named, directed pin, including one or more "weakly connected" physical [LefPort]s.
#[derive(Clone, Default, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into))]
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
    pub taper_rule: Option<Unsupported>,
    /// Net Expression (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub net_expr: Option<Unsupported>,
    /// Supply Sensitivity (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub supply_sensitivity: Option<Unsupported>,
    /// Ground Sensitivity (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub ground_sensitivity: Option<Unsupported>,
    /// Must-Join (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub must_join: Option<Unsupported>,
    /// Properties (Unsupported)
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub properties: Option<Unsupported>,
}
/// # Lef Pin Direction
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
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
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub struct LefPinAntennaAttr {
    pub key: String,
    pub val: LefDecimal,
    pub layer: Option<String>,
}
/// # Lef Port
///
/// Defines the physical locations and optional metadata of a port on a pin.
/// LEF includes the notion of multiple "weakly connected" ports per pin;
/// each [LefPort] is one such weakly-connected point.
#[derive(Clone, Default, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into))]
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
#[derive(Clone, Default, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into))]
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
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub struct LefVia {
    /// Via-Type Name
    pub via_name: String,
    /// Location
    pub pt: LefPoint,
}
/// # Enumerated Layer-Spacing Options
/// Includes absolute spacing and design-rule-width modifiers.
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub enum LefLayerSpacing {
    Spacing(LefDecimal),
    DesignRuleWidth(LefDecimal),
}
/// # Lef Geometric Object Enumeration
/// Includes [LefShape]s and Iterators thereof
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub enum LefGeometry {
    /// Single Shape
    Shape(LefShape),
    /// Repeated Iteration/ Array of Shapes (Unsupported)
    Iterate {
        shape: LefShape,
        pattern: Option<Unsupported>,
    },
}
/// # Lef Shape Enumeration
/// Includes each of LEF's individual geometric primitives:
/// rectangles, polygons, and paths.
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub enum LefShape {
    Rect(Option<LefDecimal>, LefPoint, LefPoint),
    Polygon(Vec<LefPoint>),
    Path(Vec<LefPoint>),
}
/// # Lef X-Y Spatial Point
///
/// Specified in [LefDecimal]-valued Cartesian coordinates.  
/// Supports common mathematical operations (Add, Sub, increment, etc.).  
#[derive(
    Clone,
    Default,
    Debug,
    Deserialize,
    Serialize,
    JsonSchema,
    PartialEq,
    Eq,
    Add,
    AddAssign,
    Sub,
    SubAssign,
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
#[derive(Clone, Default, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub struct LefDbuPerMicron(pub u32);
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
///
#[derive(Clone, Default, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub struct LefUnits {
    /// Database Distance Units per Micron
    /// Defaults to 100, i.e. 1 DBU = 10nm
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub database_microns: Option<LefDbuPerMicron>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub time_ns: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub capacitance_pf: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub resistance_ohms: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub power_mw: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub current_ma: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub voltage_volts: Option<LefDecimal>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub frequency_mhz: Option<LefDecimal>,
}
/// # Lef Site Definition
///
/// Defines a placement-site in designs.
/// Dictates the placement grid for a family of macros.
///
#[derive(Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
#[builder(pattern = "owned", setter(into))]
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
    pub row_pattern: Option<Unsupported>,
}
/// # Unsupported Feature
///
/// Empty placeholder struct for unsupported LEF features. Primarily included for documentation purposes.
/// Most [`Unsupported`] fields are of type [`Optional<Unsupported>`] for sake of serialization,
/// so that they can take on the `null` value of many data formats.
/// Setting these fields to [`Some(Unsupported)`] instead of [`None`] is largely a distinction without a difference.
///
#[derive(Clone, Default, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub struct Unsupported;

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
        Mask: "MASK",

        // UNITS Fields
        Units: "UNITS",
        Time: "TIME",
        Nanoseconds: "NANOSECONDS",
        Capacitance: "CAPACITANCE",
        Picofarads: "PICOFARADS",
        Resistance: "RESISTANCE",
        Ohms: "OHMS",
        Power: "POWER",
        Milliwatts: "MILLIWATTS",
        Current: "CURRENT",
        Milliamps: "MILLIAMPS",
        Voltage: "VOLTAGE",
        Volts: "VOLTS",
        Database: "DATABASE",
        Microns: "MICRONS",
        Frequency: "FREQUENCY",
        Megahertz: "MEGAHERTZ",

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
        ManufacturingGrid: "MANUFACTURINGGRID",
        UseMinSpacing: "USEMINSPACING",
        ClearanceMeasure: "CLEARANCEMEASURE",
        PropertyDefinitions: "PROPERTYDEFINITIONS",
        MaxViaStack: "MAXVIASTACK",
        ViaRule: "VIARULE",
        Generate: "GENERATE",
        NonDefaultRule: "NONDEFAULTRULE",
    }
);
impl LefKey {
    /// Lef Key parsing, performed case-insensitively by internally converting to upper-case.
    pub fn parse(txt: &str) -> Option<Self> {
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

use super::read::{LefParseErrorType, ParserState};

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
        msg: Option<String>,
        tp: LefParseErrorType,
        state: ParserState,
    },
    /// Wrapped errors, generally from other crates
    Boxed(Box<dyn std::error::Error>),
    /// String message-valued errors
    Str(String),
}
impl From<crate::utils::ser::Error> for LefError {
    fn from(e: crate::utils::ser::Error) -> Self {
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
// One of these days, this way is gonna WORK, and we'll delete all these specific error-types above.
// impl<E: std::error::Error> From<E> for LefError {
//     /// Wrap External Errors
//     fn from(e: E) -> Self {
//         Self::Boxed(Box::new(e))
//     }
// }

impl std::fmt::Display for LefError {
    /// Delegates to the [Debug] implementation
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Debug::fmt(self, f)
    }
}
impl std::error::Error for LefError {}

/// Lef21 Library-Wide Result Type
pub type LefResult<T> = Result<T, LefError>;

// Implement the serialization to/from file trait for libraries and macros
impl crate::utils::SerdeFile for LefLibrary {}
impl crate::utils::SerdeFile for LefMacro {}
