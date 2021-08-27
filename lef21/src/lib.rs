//!
//! # Lef21 Library Exchange Format (LEF) Parser & Writer
//!

// Crates.io Imports
#[allow(unused_imports)]
use rust_decimal::prelude::*;
use serde::{Deserialize, Serialize};
#[macro_use]
extern crate derive_builder;

// Local modules & re-exports
use layout21utils as utils;
mod read;
#[cfg(test)]
mod tests;
mod write;

/// Internal type alias for all decimal-valued data
pub type LefDecimal = rust_decimal::Decimal;

/// Lef Library
/// Primary store of macro/cell definitions
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
pub struct LefLibrary {
    // Required
    /// Macro Definitions
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub macros: Vec<LefMacro>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub sites: Vec<LefSite>,

    // Optional
    /// Lef Spec Version
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub version: Option<LefDecimal>,
    /// Case-Sensitive Name Setting
    /// FIXME: potentially only some LEF versions
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub names_case_sensitive: Option<LefOnOff>,
    /// Wire-Extension Pin Settings
    /// FIXME: potentially only some LEF versions
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
    /// FIXME: recommended for tech-lef, but turns up in libraries as well
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub units: Option<LefUnits>,

    // Not (Yet) Supported
    /// Via Definitions
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub vias: Option<Tbd>,
    /// Syntax Extensions
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub extensions: Option<Tbd>,
}
impl LefLibrary {
    /// Open a [LefLibrary] from file `fname`
    pub fn open(fname: &str) -> LefResult<LefLibrary> {
        read::parse_file(fname)
    }
    /// Write a [LefLibrary] to file `fname`
    /// Fields are written in the LEF-recommended order
    pub fn save(&self, fname: &str) -> LefResult<()> {
        write::save(self, fname)
    }
}
/// Lef Macro Definition
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
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
    /// Foreign (GDSII) Cell
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
    /// Note the optional `sitePattern` is not supported
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub site: Option<String>,
    /// Source
    /// FIXME: supported in earlier versions of LEF only
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub source: Option<LefDefSource>,

    // Not (Yet) Supported
    /// Fixed Mask Option
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub fixed_mask: Option<Tbd>,
    /// Electrically-Equivalent Cell
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub eeq: Option<Tbd>,
    /// Density Objects
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub density: Option<Tbd>,
    /// Properties
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub properties: Option<Tbd>,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefMacroClass {
    Cover { bump: bool },
    Ring,
    Block { tp: Option<LefBlockClassType> },
    Pad { tp: Option<LefPadClassType> },
    Core { tp: Option<LefCoreClassType> },
    EndCap { tp: LefEndCapClassType },
}
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
pub struct LefForeign {
    /// Foreign Cell Name
    pub cell_name: String,
    /// Location
    pub pt: Option<LefPoint>,
    /// Orientation
    pub orient: Option<Tbd>,
}
#[derive(Clone, Default, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[builder(setter(into), private)]
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

    // Not (Yet) Supported
    /// Taper Rule
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub taper_rule: Option<Tbd>,
    /// Net Expression
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub net_expr: Option<Tbd>,
    /// Supply Sensitivity
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub supply_sensitivity: Option<Tbd>,
    /// Ground Sensitivity
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub ground_sensitivity: Option<Tbd>,
    /// Must-Join
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub must_join: Option<Tbd>,
    /// Properties
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub properties: Option<Tbd>,
}
/// Enumerated Pin Directions
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
/// Antenna Attributes
///
/// Stored as key-value pairs from string-keys name "ANTENNA*" to [LefDecimal] values.
/// Note each pair may have an optional `layer` specifier,
/// and that each key may have multiple attributes, generally specifying different layers.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefPinAntennaAttr {
    key: String,
    val: LefDecimal,
    layer: Option<String>,
}
#[derive(Clone, Default, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefPort {
    /// Port-Class
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub class: Option<LefPortClass>,
    /// Layers & Geometries
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub layers: Vec<LefLayerGeometries>,
}
#[derive(Clone, Default, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
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
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefVia {
    /// Via-Type Name
    pub via_name: String,
    /// Location
    pub pt: LefPoint,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefLayerSpacing {
    Spacing(LefDecimal),
    DesignRuleWidth(LefDecimal),
}
/// Lef Geometric Objects -
/// Rectangles, Polygons, Paths, and Iterators thereof
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefGeometry {
    /// Single Shape
    Shape(LefShape),
    /// Repeated Iteration/ Array of Shapes
    Iterate { shape: LefShape, pattern: Tbd },
}
/// Lef Shapes
/// Individual Geometric Primitives
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum LefShape {
    Rect(LefPoint, LefPoint),
    Polygon(Vec<LefPoint>),
    Path(Vec<LefPoint>),
}
/// X-Y Point
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefPoint(LefDecimal, LefDecimal);
impl LefPoint {
    /// Create a new [LefPoint]
    pub fn new(x: impl Into<LefDecimal>, y: impl Into<LefDecimal>) -> Self {
        Self(x.into(), y.into())
    }
}
impl std::fmt::Display for LefPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {}", self.0, self.1)
    }
}
/// # Database-Units per Micron  
///
/// A constrained numeric type, wrapping [LefDecimal].
/// Allowed values of [LefDbuPerMicron] are:
/// [100, 200, 400, 800, 1000, 2000, 4000, 8000, 10_000, 20_000]
/// and adherence to this set is checked at construction time.
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefDbuPerMicron(LefDecimal);
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
        Ok(Self(x))
    }
}
/// Measurement Unit Conversion Factors
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct LefUnits {
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
/// # Lef SITE Definition
///
/// Defines a placement-site in designs.
/// Dictates the placement grid for a family of macros.
///
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq, Eq)]
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

    // Not (Yet) Supported
    /// Row Patterns, re other previously defined sites
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub row_pattern: Option<Tbd>,
}
/// Placeholder Struct for Fields to be completed
#[derive(Clone, Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Tbd;

/// Lef String-Enumeration Trait
/// Defines two central methods:
/// * `to_str(&self) -> &'static str` converts the enum to its Lef-String values.
/// * `from_str(&str) -> Option<Self>` does the opposite, returning an [Option] indicator of success or failure.
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
    (   $(#[$meta:meta])*
        $enum_name:ident {
        $( $variant:ident : $strval:literal ),* $(,)?
    }) => {
        $(#[$meta])*
        #[allow(dead_code)]
        #[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
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
    /// Binary On/Off Settings, Denoted by ON and OFF
    LefOnOff {
        On: "ON",
        Off: "OFF",
    }
);
enumstr!(
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
    /// Specifies the usage-intent for a pin.
    /// Note this is the noun form of "use", pronounced with the hard "s".
    /// Not the verb form pronounced like the New Jersey second-person plural "yous".
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
    /// Sub-Types for Macros of Class [LefMacroClass:EndCap]
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
    /// Sub-Types for Macros of Class [LefMacroClass:Block]
    LefBlockClassType {
        BlackBox: "BLACKBOX",
        Soft: "SOFT"
    }
);
enumstr!(
    /// Sub-Types for Macros of Class [LefMacroClass:Core]
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
    /// Sub-Types for Macros of Class [LefMacroClass:Core]
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

/// Lef Error Enumeration
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
        ctx: read::LefParseContext,
        token: String,
        line_content: String,
        line_num: usize,
        pos: usize,
    },
    /// Errors parsing numeric [LefDecimal] values
    ParseNum(rust_decimal::Error),
    /// File I/O Errors
    Io(std::io::Error),
    /// Other wrapped errors, generally from other crates
    Boxed(Box<dyn std::error::Error>),
    /// Other string-typed errors, generally from other crates
    Str(String),
}
impl From<utils::ser::Error> for LefError {
    /// Convert common IO & file errors by wrapping them
    fn from(e: utils::ser::Error) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl From<std::io::Error> for LefError {
    /// Convert common IO & file errors by wrapping them
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}
impl From<rust_decimal::Error> for LefError {
    /// Convert integer-parsing errors by wrapping them
    fn from(e: rust_decimal::Error) -> Self {
        Self::ParseNum(e)
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
/// Lef21 Library-Wide Result Type
pub type LefResult<T> = Result<T, LefError>;

// Implement the serialization to/from file trait for libraries and macros
impl utils::SerdeFile for LefLibrary {}
impl utils::SerdeFile for LefMacro {}
