//!
//! # Gds21 Data Model
//!

// Std-Lib Imports
use std::error::Error;
use std::io::Write;
use std::path::Path;

// Crates.io
use chrono::{Datelike, NaiveDate, NaiveDateTime, SubsecRound, Timelike, Utc};
use derive_builder::Builder;
use derive_more::{self, Add, AddAssign, Sub, SubAssign};
use num_derive::FromPrimitive;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

// Workspace Imports
use layout21utils::SerdeFile;

// Local Imports
use crate::read::{GdsParser, GdsScanner, GdsStructScan};
use crate::write::GdsWriter;

///
/// # Gds Record Types
///
/// In the numeric-order specified by GDSII, for automatic [FromPrimitive] conversions.
///
#[derive(FromPrimitive, Debug, Clone, Copy, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub enum GdsRecordType {
    Header = 0x00,
    BgnLib,
    LibName,
    Units,
    EndLib,
    BgnStruct,
    StructName, // STRNAME
    EndStruct,
    Boundary,
    Path,
    StructRef,
    ArrayRef,
    Text,
    Layer,
    DataType,
    Width,
    Xy,
    EndElement,
    StructRefName, // SNAME
    ColRow,
    TextNode, // "Not currently used"
    Node,
    TextType,
    Presentation,
    Spacing, // "Discontinued"
    String,
    Strans,
    Mag,
    Angle,
    Uinteger, // "No longer used"
    Ustring,  // "No longer used"
    RefLibs,
    Fonts,
    PathType,
    Generations,
    AttrTable,
    StypTable, // "Unreleased Feature"
    StrType,   // "Unreleased Feature"
    ElemFlags,
    ElemKey,  // "Unreleased Feature"
    LinkType, // "Unreleased Feature"
    LinkKeys, // "Unreleased Feature"
    Nodetype,
    PropAttr,
    PropValue,
    Box,
    BoxType,
    Plex,
    BeginExtn, // "Only occurs in CustomPlus"
    EndExtn,   // "Only occurs in CustomPlus"
    TapeNum,
    TapeCode,
    StrClass, // "Only for Calma internal use"
    Reserved, // "Reserved for future use"
    Format,
    Mask,
    EndMasks,
    LibDirSize,
    SrfName,
    LibSecur,
}
impl GdsRecordType {
    /// Boolean indication of valid record types
    /// Many are either deprecated or provisioned without ever being implemented;
    /// all from this list are deemed invalid.
    pub fn valid(&self) -> bool {
        match self {
            Self::TextNode | // "Not currently used"
            Self::Spacing | // "Discontinued"
            Self::Uinteger | // "No longer used"
            Self::Ustring |  // "No longer used"
            Self::StypTable | // "Unreleased Feature"
            Self::StrType |   // "Unreleased Feature"
            Self::ElemKey |   // "Unreleased Feature"
            Self::LinkType |  // "Unreleased Feature"
            Self::LinkKeys |  // "Unreleased Feature"
            Self::StrClass | // "Only for Calma internal use"
            Self::Reserved   // "Reserved for future use"
              => false,
            _ => true,
        }
    }
}

/// # Gds DataType Enumeration
/// In order as decoded from 16-bit integers in binary data
#[derive(FromPrimitive, Debug, Clone, Copy, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub enum GdsDataType {
    NoData = 0,
    BitArray = 1,
    I16 = 2,
    I32 = 3,
    F32 = 4,
    F64 = 5,
    Str = 6,
}

/// # Gds Record Header
/// Decoded contents of a record's four header bytes,
/// including its record-type, data-type, and length in bytes.
#[derive(Debug, Clone, Copy, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub struct GdsRecordHeader {
    pub rtype: GdsRecordType,
    pub dtype: GdsDataType,
    pub len: u16,
}

///
/// # Gds Record Enumeration
///
/// Keeps each record in relatively "raw" form,
/// other than assuring correct data-types,
/// and converting one-entry arrays into scalars.
/// Invalid record-types are not included.
///
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum GdsRecord {
    Header { version: i16 },
    BgnLib { dates: [i16; 12] },
    LibName(String),
    Units(f64, f64),
    EndLib,
    BgnStruct { dates: [i16; 12] },
    StructName(String),    // STRNAME Record
    StructRefName(String), // SNAME Record
    EndStruct,
    Boundary,
    Path,
    StructRef,
    ArrayRef,
    Text,
    Layer(i16),
    DataType(i16),
    Width(i32),
    Xy(Vec<i32>),
    EndElement,
    ColRow { cols: i16, rows: i16 },
    Node,
    TextType(i16),
    Presentation(u8, u8),
    String(String),
    Strans(u8, u8),
    Mag(f64),
    Angle(f64),
    RefLibs(String),
    Fonts(String),
    PathType(i16),
    Generations(i16),
    AttrTable(String),
    ElemFlags(u8, u8),
    Nodetype(i16),
    PropAttr(i16),
    PropValue(String),
    Box,
    BoxType(i16),
    Plex(i32),
    BeginExtn(i32),
    EndExtn(i32),
    TapeNum(i16),
    TapeCode([i16; 6]),
    Format(i16),
    Mask(String),
    EndMasks,
    LibDirSize(i16),
    SrfName(String),
    LibSecur(i16),
}

/// # Gds Floating Point
/// ## GDSII's Home-Grown Floating-Point Format  
///
/// Incredibly, GDSII is old enough to have its own float-format,
/// like most computers did before IEEE754.
///
/// The [GdsFloat64] struct is not used as a data-store, but largely a namespace
/// for the `encode` and `decode` operations to and from IEEE754 double-precision format.
///
pub struct GdsFloat64;
impl GdsFloat64 {
    /// Decode GDSII's eight-byte representation, stored as a `u64`, to IEEE (and Rust)-compatible `f64`
    pub fn decode(val: u64) -> f64 {
        // Extract the MSB Sign bit
        let neg = (val & 0x8000_0000_0000_0000) != 0;
        // Extract the 7b exponent
        let exp: i32 = ((val & 0x7F00_0000_0000_0000) >> 8 * 7) as i32 - 64;
        // Create the initially integer-valued mantissa from the 7 least-significant bytes
        let mantissa: u64 = val & 0x00FF_FFFF_FFFF_FFFF;
        // And apply its normalization to the range (1/16, 1)
        let mantissa: f64 = mantissa as f64 / 2f64.powi(8 * 7);
        // Combine everything into our overall value
        if neg {
            -1.0 * mantissa * 16f64.powi(exp)
        } else {
            mantissa * 16f64.powi(exp)
        }
    }
    /// Encode `f64` to GDSII's eight bytes, stored as `u64`.
    pub fn encode(mut val: f64) -> u64 {
        if val == 0.0 {
            return 0;
        };
        let mut top: u8 = 0;
        if val < 0.0 {
            top = 0x80;
            val = -val;
        }
        let fexp: f64 = 0.25 * val.log2();
        let mut exponent = fexp.ceil() as i32;
        if fexp == fexp.ceil() {
            exponent += 1;
        }
        let mantissa: u64 = (val * 16_f64.powi(14 - exponent)).round() as u64;
        top += (64 + exponent) as u8;
        let result: u64 = (top as u64).wrapping_shl(56) | (mantissa & 0x00FF_FFFF_FFFF_FFFF);
        result
    }
}

/// # Unsupported (But Spec-Valid) Features
#[derive(Default, Debug, Clone, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub struct Unsupported;


/// # Gds Translation Settings
/// Reflection, rotation, and magnification for text-elements and references.
/// As configured by `STRANS` records.
#[derive(Default, Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
pub struct GdsStrans {
    /// Reflection, about the x-axis.
    /// Applied before rotation.
    #[serde(default, skip_serializing_if = "is_false")]
    pub reflected: bool,
    /// Absolute Magnification Setting
    #[serde(default, skip_serializing_if = "is_false")]
    pub abs_mag: bool,
    /// Absolute Angle Setting
    #[serde(default, skip_serializing_if = "is_false")]
    pub abs_angle: bool,
    /// Magnification Factor. Interpreted as unit-scaling (mag==1.0) if not specified.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mag: Option<f64>,
    /// Angle, in degrees counter-clockwise. Defaults to zero if not specified.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub angle: Option<f64>,
}

/// # Gds Text-Presentation Flags
/// Sets fonts, text justification, and the like.
/// Stored in raw `u8` form.
#[derive(Default, Debug, Clone, Deserialize, Serialize, JsonSchema, PartialEq)]
pub struct GdsPresentation(pub u8, pub u8);

/// # Gds Element Flags
/// As configured by `ELFLAGS` records.
/// Two bytes of bit-fields stored in raw `u8` form.
#[derive(Default, Debug, Clone, Deserialize, Serialize, JsonSchema, PartialEq)]
pub struct GdsElemFlags(pub u8, pub u8);

/// # Gds Plex
/// From the spec:
/// "A unique positive number which is common to all elements of the Plex to which this element belongs."
/// In Gds21's experience, `PLEX` records and settings are highly uncommon.
#[derive(Default, Debug, Clone, Deserialize, Serialize, JsonSchema, PartialEq)]
pub struct GdsPlex(pub i32);

/// # Gds Library Units
///
/// Each GDSII Library has two length-units, referred to as "DB Units" and "User Units" respectively.
/// Essentially all spatial data throughout the Library is denoted in "DB Units".
/// "User units" are a sort of recommendation for GUI programs to use when displaying the Library.  
///
/// From the spec's `UNITS` record-description:  
/// ```text
/// Contains two eight-byte real numbers.
/// The first number is the size of a database-unit, in user-units.
/// The second is the size of a database-unit in meters.
/// To calculate the size of a user-unit in meters, divide the second number by the first.
/// ```
///
/// These two numbers are stored as-is in the [GdsUnits] tuple-struct.
///
#[derive(Debug, Clone, Deserialize, Serialize, JsonSchema, PartialEq)]
pub struct GdsUnits(pub f64, pub f64);
impl GdsUnits {
    /// Create a new [GdsUnits]
    pub fn new(num1: f64, num2: f64) -> Self {
        Self(num1, num2)
    }
    /// Get the database-unit size, in meters. Used for all spatial data.
    pub fn db_unit(&self) -> f64 {
        self.1
    }
    /// Get the user-unit size, in meters. Largely for display/ debug.
    pub fn user_unit(&self) -> f64 {
        self.0 / self.1
    }
}
impl Default for GdsUnits {
    /// Default values for GDS Units:
    /// * DB-Unit = 1nm
    /// * User-Unit = 1µm (1000x the DB-Unit)
    fn default() -> Self {
        Self(1e-3, 1e-9)
    }
}

/// # Gds Spatial Point
/// Coordinate in (x,y) layout-space.
/// Denoted in each [GdsLibrary]'s [GdsUnits].
#[derive(Debug, Clone, Default, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
pub struct GdsPoint {
    pub x: i32,
    pub y: i32,
}
impl GdsPoint {
    /// Create a new [GdsPoint]
    pub fn new(x: i32, y: i32) -> Self {
        GdsPoint { x, y }
    }
    /// Create a vector of [GdsPoint] from an array of tuples
    pub fn vec(pts: &[(i32, i32)]) -> Vec<Self> {
        pts.iter().map(|pt| Self::new(pt.0, pt.1)).collect()
    }
    /// Convert from a two-element vector
    pub(crate) fn parse(from: &Vec<i32>) -> GdsResult<Self> {
        if from.len() != 2 {
            return Err(GdsError::Str(
                "GdsPoint coordinate vector: Invalid number of elements".into(),
            ));
        }
        Ok(GdsPoint {
            x: from[0],
            y: from[1],
        })
    }
    /// Convert an n-element vector if `i32` into an n/2-element vector of [GdsPoint]s.
    pub(crate) fn parse_vec(from: &[i32]) -> GdsResult<Vec<GdsPoint>> {
        if from.len() % 2 != 0 {
            return Err(GdsError::Str(
                "GdsPoint coordinate vector: Invalid number of elements".into(),
            ));
        }
        let mut rv = Vec::with_capacity(from.len() / 2);
        for i in 0..from.len() / 2 {
            rv.push(GdsPoint {
                x: from[i * 2],
                y: from[i * 2 + 1],
            });
        }
        Ok(rv)
    }
    /// Flatten to a two-element vector
    pub(crate) fn flatten(&self) -> Vec<i32> {
        vec![self.x, self.y]
    }
    /// Convert an n-element vector of [GdsPoint]s to a 2n-element i32 vector.
    pub(crate) fn flatten_vec(src: &Vec<GdsPoint>) -> Vec<i32> {
        let mut rv = Vec::with_capacity(src.len() * 2);
        for pt in src.iter() {
            rv.push(pt.x);
            rv.push(pt.y);
        }
        rv
    }
}
/// # Gds Mask-Format Enumeration
/// As set by the FORMAT record
#[derive(Debug, Clone, Deserialize, Serialize, JsonSchema, PartialEq)]
pub enum GdsFormatType {
    /// Default, sole fully-supported case.
    Archive,
    /// Filtered-format includes a list of Mask records. Not supported.
    Filtered(Vec<Unsupported>),
}
/// # Gds Property
/// Spec BNF:
/// ```text
/// PROPATTR PROPVALUE
/// ```
#[derive(Default, Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
pub struct GdsProperty {
    /// Attribute Number
    pub attr: i16,
    /// Attribute Value
    pub value: String,
}

///
/// # Gds Path Element
///
/// Spec BNF:
/// ```text
/// PATH [ELFLAGS] [PLEX] LAYER DATATYPE [PATHTYPE] [WIDTH] XY [BGNEXTN] [ENDEXTN])
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
#[builder(pattern = "owned", setter(into))]
pub struct GdsPath {
    // Required Fields
    /// Layer Number
    pub layer: i16,
    /// DataType ID
    pub datatype: i16,
    /// Vector of x,y coordinates
    pub xy: Vec<GdsPoint>,

    // Optional Fields
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub width: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub path_type: Option<i16>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub begin_extn: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub end_extn: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default, setter(strip_option))]
    pub properties: Vec<GdsProperty>,
}

///
/// # Gds Boundary Element
///
/// The most common type for closed-form shapes in GDSII.
/// Most IC layout is comprised of [GdsBoundary] elements, which represent individual polygons.
/// GDSII dictates that the first two and final two coordinates in each [GdsBoundary]
/// shall be identical, "closing" the polygon.
/// Hence an N-sided polygon is represented by an (N+1)-point `xy` vector.
///
/// Spec BNF:
/// ```text
/// BOUNDARY [ELFLAGS] [PLEX] LAYER DATATYPE XY
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
#[builder(pattern = "owned", setter(into))]
pub struct GdsBoundary {
    // Required Fields
    /// Layer Number
    pub layer: i16,
    /// DataType ID
    pub datatype: i16,
    /// Vector of x,y coordinates
    pub xy: Vec<GdsPoint>,

    // Optional Fields
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default, setter(strip_option))]
    pub properties: Vec<GdsProperty>,
}
///
/// # Gds Struct Reference (Cell Instance)
///
/// Represents an instance of a layout-cell.
/// Coordinate vector `xy` is dictated by spec to have exactly one point (or two numbers),
/// specifying the instance's lower-left coordinate.
/// Options for rotation and reflection are configured in the [GdsStrans] attribute `strans`.
///
/// Spec BNF:
/// ```text
/// SREF [ELFLAGS] [PLEX] SNAME [<strans>] XY
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
#[builder(pattern = "owned", setter(into))]
pub struct GdsStructRef {
    // Required Fields
    /// Struct (Cell) Name
    pub name: String,
    /// Location x,y coordinates
    pub xy: GdsPoint,

    // Optional Fields
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    /// Translation & Reflection Options
    pub strans: Option<GdsStrans>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default, setter(strip_option))]
    pub properties: Vec<GdsProperty>,
}
///
/// # Gds Array Reference
///
/// A two-dimensional array of struct (cell) instances.
///
/// Spec BNF:
/// ```text
/// AREF [ELFLAGS] [PLEX] SNAME [<strans>] COLROW XY
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
#[builder(pattern = "owned", setter(into))]
pub struct GdsArrayRef {
    // Required Fields
    /// Struct (Cell) Name
    pub name: String,
    /// Vector of x,y coordinates
    pub xy: [GdsPoint; 3],
    /// Number of columns
    pub cols: i16,
    /// Number of rows
    pub rows: i16,

    // Optional Fields
    /// Translation & Reflection Options
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default)]
    pub strans: Option<GdsStrans>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default, setter(strip_option))]
    pub properties: Vec<GdsProperty>,
}
///
/// # Gds Text Element
///
/// Spec BNF:
/// ```text
/// TEXT [ELFLAGS] [PLEX] LAYER
/// TEXTTYPE [PRESENTATION] [PATHTYPE] [WIDTH] [<strans>] XY STRING
/// ```
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
#[builder(pattern = "owned", setter(into))]
pub struct GdsTextElem {
    // Required Fields
    /// Text Value
    pub string: String,
    /// Layer Number
    pub layer: i16,
    /// Text-Type ID
    pub texttype: i16,
    /// Vector of x,y coordinates
    pub xy: GdsPoint,

    // Optional Fields
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub presentation: Option<GdsPresentation>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub path_type: Option<i16>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub width: Option<i32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default)]
    /// Translation & Reflection Options
    pub strans: Option<GdsStrans>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default, setter(strip_option))]
    pub properties: Vec<GdsProperty>,
}
///
/// # Gds Node Element
///
/// Spec BNF:
/// ```text
/// NODE [ELFLAGS] [PLEX] LAYER NODETYPE XY
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
#[builder(pattern = "owned", setter(into))]
pub struct GdsNode {
    // Required Fields
    /// Layer Number
    pub layer: i16,
    /// Node-Type ID
    pub nodetype: i16,
    /// Vector of x,y coordinates
    pub xy: Vec<GdsPoint>,

    // Optional Fields
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default, setter(strip_option))]
    pub properties: Vec<GdsProperty>,
}
///
/// # Gds Box Element
///
/// Spec BNF:
/// ```text
/// BOX [ELFLAGS] [PLEX] LAYER BOXTYPE XY
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
#[builder(pattern = "owned", setter(into))]
pub struct GdsBox {
    // Required Fields
    /// Layer Number
    pub layer: i16,
    /// Box-Type ID
    pub boxtype: i16,
    /// Vector of x,y coordinates
    pub xy: [GdsPoint; 5],

    // Optional Fields
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    #[builder(default, setter(strip_option))]
    pub properties: Vec<GdsProperty>,
}
///
/// # Gds Element Enumeration  
///
/// Primary union of geometric elements, instances, and arrays which comprise a GDSII struct (cell).
///
/// Spec BNF:
/// ```text
/// {<boundary> | <path> | <SREF> | <AREF> | <text> | <node> | <box>} {<property>}* ENDEL
/// ```
///
/// Note the `properties` vectors are pushed down to each enum variant.
///
#[derive(derive_more::From, Debug, Clone, Deserialize, Serialize, JsonSchema, PartialEq)]
pub enum GdsElement {
    GdsBoundary(GdsBoundary),
    GdsPath(GdsPath),
    GdsStructRef(GdsStructRef),
    GdsArrayRef(GdsArrayRef),
    GdsTextElem(GdsTextElem),
    GdsNode(GdsNode),
    GdsBox(GdsBox),
}

/// # Gds Summary Stats  
///
/// Summary statistics for a [GdsLibrary] or [GdsStruct].  
/// Total numbers of elements of each type.
#[derive(
    Debug, Default, Deserialize, Serialize, JsonSchema, PartialEq, Add, AddAssign, Sub, SubAssign,
)]
pub struct GdsStats {
    pub libraries: usize,
    pub structs: usize,
    pub boundaries: usize,
    pub paths: usize,
    pub struct_refs: usize,
    pub array_refs: usize,
    pub text_elems: usize,
    pub nodes: usize,
    pub boxes: usize,
}

/// # Gds Date & Time
///
/// From the spec:
/// ```text
/// Two-Byte Signed Integer
/// Contains last modification time of library (two bytes
/// each for year, month, day, hour, minute, and second)
/// as well as time of last access (same format) and
/// marks beginning of library.
/// ```
///
/// In which more specifically:
/// * Years are referenced to **1900**
/// * Days are valued 1-31
/// * Months are valued 1-12
/// * Hours are valued 1-12
///
/// When reading from GDSII file, [`GdsDateTime`] accepts any twelve bytes and stores them as-is;
/// no validation for real dates & times, e.g. month 30 or hour 99, is performed.
/// The default [`GdsDateTime`] when creating a new [`GdsLibrary`] is its creation time,
/// as produced by [`chrono::Utc::now()`]. Such dates & times are always valid.
///
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
pub struct GdsDateTime {
    pub year: i16, // GDSII uses 1900 as the base year
    pub month: i16,
    pub day: i16,
    pub hour: i16,
    pub minute: i16,
    pub second: i16,
}
impl From<NaiveDateTime> for GdsDateTime {
    fn from(dt: NaiveDateTime) -> Self {
        Self {
            year: dt.year() as i16 - 1900, // GDSII uses 1900 as the base year
            month: dt.month() as i16,
            day: dt.day() as i16,
            hour: dt.hour() as i16,
            minute: dt.minute() as i16,
            second: dt.second() as i16,
        }
    }
}
impl TryInto<NaiveDateTime> for GdsDateTime {
    type Error = GdsError;

    /// Try converting a [`GdsDateTime`] to a [`chrono::NaiveDateTime`].
    /// Fails if any of the GDSII values are invalid. e.g. "month 30" or "hour 99".
    fn try_into(self: GdsDateTime) -> GdsResult<NaiveDateTime> {
        // Note GDSII's 1900 offset is applied here
        let ymd = match NaiveDate::from_ymd_opt(
            self.year as i32 + 1900,
            self.month as u32,
            self.day as u32,
        ) {
            Some(y) => y,
            None => return Err(GdsError::Str("Invalid Date".to_string())),
        };
        let dt = match ymd.and_hms_opt(self.hour as u32, self.minute as u32, self.second as u32) {
            Some(t) => t,
            None => return Err(GdsError::Str("Invalid Time".to_string())),
        };
        Ok(dt)
    }
}
impl GdsDateTime {
    /// Get the current time
    ///
    /// Note GDSII's time format is specified in seconds, whereas `NaiveDateTime` has nanosecond precision.
    /// Always round to the nearest second to match data coming in from GDSII files.
    ///
    pub fn now() -> Self {
        // Create a [`chrono::NaiveDateTime`] from the current time, rounded to the nearest second,
        // and convert it via the `From` implementation below.
        let naive_datetime = Utc::now().naive_utc().round_subsecs(0);
        naive_datetime.into()
    }
}
impl Default for GdsDateTime {
    /// Default dates & times: what better time than now!
    fn default() -> Self {
        Self::now()
    }
}
impl From<&[i16; 6]> for GdsDateTime {
    /// Convert from a 6-element array of i16s to a [`GdsDateTime`],
    /// in the order prescribed in the GDSII spec.
    fn from(bytes: &[i16; 6]) -> Self {
        Self {
            year: bytes[0],
            month: bytes[1],
            day: bytes[2],
            hour: bytes[3],
            minute: bytes[4],
            second: bytes[5],
        }
    }
}

/// # Gds Modification & Access Dates & Times
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
pub struct GdsDateTimes {
    /// Last Modification Date & Time
    pub modified: GdsDateTime,
    /// Last Access Date & Time
    pub accessed: GdsDateTime,
}
impl Default for GdsDateTimes {
    /// Default dates & times: what better time than now!
    /// Note this makes a *single* call to `Utc::now()`, so the two dates will be the same.
    fn default() -> Self {
        let now = GdsDateTime::now();
        Self {
            modified: now.clone(),
            accessed: now,
        }
    }
}

///
/// # Gds Struct (Cell) Definition
///
/// GDSII's primary hierarchical layout-definition object is its "struct",
/// which most other layout systems would call a "cell" or "module".
/// (Most GDSII software calls them one of these as well.)  
///
/// [GdsStruct]s are principally composed of an un-ordered, un-indexed vector
/// of [GdsElement]s, which can be polygons ([GdsBoundary]),
/// instances of other layouts ([GdsStructRef]),
/// two-dimensional arrays thereof ([GdsArrayRef]),
/// and a handful of other [GdsElement]s.  
///
/// Spec BNF:
/// ```text
/// BGNSTR STRNAME [STRCLASS] {<element>}* ENDSTR
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
#[builder(pattern = "owned", setter(into))]
pub struct GdsStruct {
    /// Struct Name
    pub name: String,
    /// Modification & Access Dates & Times
    pub dates: GdsDateTimes,
    /// Elements List
    pub elems: Vec<GdsElement>,
}
impl GdsStruct {
    /// Create a new and empty [GdsStruct]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }
    /// Count and return our element statistics
    fn stats(&self) -> GdsStats {
        let mut stats = GdsStats::default();
        stats.structs += 1;
        for elem in &self.elems {
            use GdsElement::*;
            match elem {
                GdsBoundary(_) => stats.boundaries += 1,
                GdsPath(_) => stats.paths += 1,
                GdsStructRef(_) => stats.struct_refs += 1,
                GdsArrayRef(_) => stats.array_refs += 1,
                GdsTextElem(_) => stats.text_elems += 1,
                GdsNode(_) => stats.nodes += 1,
                GdsBox(_) => stats.boxes += 1,
            };
        }
        stats
    }
}

///
/// # Gds Library
///
/// The Library is GDSII's primary idiom for a suite of layout-cells.
/// A Library generally corresponds one-to-one with a `.gds` file.
/// Libraries consist primarily of cell-definitions ([GdsStruct]s),
/// and secondarily include library-level meta-data, including the distance units, GDS-spec version, and modification dates.
///
/// Several more esoteric library-level GDSII features are included as [GdsLibrary] fields,
/// but are not materially supported. The empty [Unsupported] value generally denotes these fields.
///
/// Spec BNF:
/// ```text
/// HEADER BGNLIB [LIBDIRSIZE] [SRFNAME] [LIBSECUR] LIBNAME [REFLIBS] [FONTS] [ATTRTABLE] [GENERATIONS] [<FormatType>]
/// UNITS {<structure>}* ENDLIB
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, JsonSchema, PartialEq)]
#[builder(pattern = "owned", setter(into))]
pub struct GdsLibrary {
    // Required fields
    /// Library Name
    pub name: String,
    /// Gds Spec Version
    pub version: i16,
    /// Modification & Access Dates & Times
    pub dates: GdsDateTimes,
    /// Spatial Units    
    pub units: GdsUnits,
    /// Struct Definitions
    pub structs: Vec<GdsStruct>,

    // Unsupported Fields
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub libdirsize: Unsupported,
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub srfname: Unsupported,
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub libsecur: Unsupported,
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub reflibs: Unsupported,
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub fonts: Unsupported,
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub attrtable: Unsupported,
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub generations: Unsupported,
    #[serde(default, skip_serializing)]
    #[builder(default)]
    pub format_type: Unsupported,
}
impl GdsLibrary {
    /// Create a new and empty [GdsLibrary]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: 3,
            ..Default::default()
        }
    }
    /// Read a GDS loaded from file at path `fname`
    pub fn open(fname: impl AsRef<Path>) -> GdsResult<GdsLibrary> {
        GdsParser::open(fname)?.parse_lib()
    }
    /// Alias for [`GdsLibrary::open`]. To be deprecated.
    pub fn load(fname: impl AsRef<Path>) -> GdsResult<GdsLibrary> {
        GdsLibrary::open(fname)
    }
    /// Read a [GdsLibrary] from byte-vector `bytes`
    pub fn from_bytes(bytes: &[u8]) -> GdsResult<GdsLibrary> {
        GdsParser::from_bytes(bytes)?.parse_lib()
    }
    /// Run a first-pass scan of GDSII data in `fname`.
    /// Returns a vector of [GdsStructScan]s including summary info per struct.
    #[allow(dead_code)] // FIXME!
    fn scan(fname: impl AsRef<Path>) -> GdsResult<Vec<GdsStructScan>> {
        GdsScanner::scan(fname)
    }
    /// Collect and return the library's aggregate statistics
    /// (numbers of structs, elements by type)
    pub fn stats(&self) -> GdsStats {
        let mut stats = GdsStats::default();
        stats.libraries += 1;
        for strukt in self.structs.iter() {
            stats += strukt.stats();
        }
        stats
    }
    /// Save to file `fname`
    pub fn save(&self, fname: impl AsRef<Path>) -> GdsResult<()> {
        let mut wr = GdsWriter::open(fname)?;
        wr.write_lib(self)
    }
    /// Write to file `file`
    pub fn write(&self, file: impl Write) -> GdsResult<()> {
        let mut wr = GdsWriter::new(file);
        wr.write_lib(self)
    }
    /// Set the library and all its structs' modification and access times
    pub fn set_all_dates(&mut self, time: impl Into<GdsDateTime>) {
        // Convert into [`GdsDateTime`]
        let time: GdsDateTime = time.into();
        let forced_gds_date = GdsDateTimes {
            modified: time.clone(),
            accessed: time.clone(),
        };
        self.dates = forced_gds_date.clone();
        for gds_struct in &mut self.structs {
            gds_struct.dates = forced_gds_date.clone();
        }
    }
}
// Enable [GdsLibrary] and [GdsStruct] serialization to file, in each of `utils` supported formats.
impl SerdeFile for GdsLibrary {}
impl SerdeFile for GdsStruct {}

/// # Gds Layer Spec
///
/// Each GDSII element's layer is specified by a set of two numbers,
/// commonly referred to as `layer` and `datatype`.
/// Several element-types refer to their analog of `datatype` by different names,
/// e.g. `texttype` and `nodetype`.  
///
/// `GdsLayerSpecs` generalize across these via the `xtype` field,
/// which holds whichever is appropriate for the given element.
pub struct GdsLayerSpec {
    /// Layer ID Number
    pub layer: i16,
    /// DataType (or TextType, NodeType, etc.) ID Number
    pub xtype: i16,
}
/// # Has-Layer Trait  
/// Sole function `layerspec` returns a [GdsLayerSpec] including the two numbers `layer` and `xtype`.
pub trait HasLayer {
    fn layerspec(&self) -> GdsLayerSpec;
}
impl GdsLayerSpec {
    /// Create a new [GdsLayerSpec] ]
    pub fn new(layer: i16, xtype: i16) -> GdsLayerSpec {
        GdsLayerSpec { layer, xtype }
    }
}
impl HasLayer for GdsBoundary {
    fn layerspec(&self) -> GdsLayerSpec {
        GdsLayerSpec::new(self.layer, self.datatype)
    }
}
impl HasLayer for GdsTextElem {
    fn layerspec(&self) -> GdsLayerSpec {
        GdsLayerSpec::new(self.layer, self.texttype)
    }
}
impl HasLayer for GdsNode {
    fn layerspec(&self) -> GdsLayerSpec {
        GdsLayerSpec::new(self.layer, self.nodetype)
    }
}
impl HasLayer for GdsBox {
    fn layerspec(&self) -> GdsLayerSpec {
        GdsLayerSpec::new(self.layer, self.boxtype)
    }
}
impl HasLayer for GdsPath {
    fn layerspec(&self) -> GdsLayerSpec {
        GdsLayerSpec::new(self.layer, self.datatype)
    }
}

/// # Gds Context
/// Enumeration of each context in which a record can be parsed, primarily for error reporting
#[derive(Debug, Clone)]
pub enum GdsContext {
    Library,
    Struct,
    StructRef,
    ArrayRef,
    Boundary,
    Box,
    Path,
    Text,
    Node,
    Property,
}

/// # GdsResult Type-Alias
pub type GdsResult<T> = Result<T, GdsError>;

/// # Gds Error Enumeration
/// Most errors are tied in some sense to parsing and decoding.
/// Once a valid [GdsLibrary] is created in memory, it can generally be streamed to bytes.
#[derive(Debug)]
pub enum GdsError {
    /// Invalid binary -> record conversion
    RecordDecode(GdsRecordType, GdsDataType, u16),
    /// Invalid record length
    RecordLen(usize),
    /// Invalid data type
    InvalidDataType(u8),
    /// Invalid record type
    InvalidRecordType(u8),
    /// Unsupported feature, in the decoded context
    Unsupported(Option<GdsRecord>, Option<GdsContext>),
    /// Parser Errors
    Parse {
        msg: String,
        record: GdsRecord,
        recordnum: usize,
        bytepos: u64,
        ctx: Vec<GdsContext>,
    },
    /// Boxed (External) Errors
    Boxed(Box<dyn Error>),
    /// Other errors
    Str(String),
}
impl std::fmt::Display for GdsError {
    /// Display a [GdsError].
    /// This functionally delegates to the (derived) [std::fmt::Debug] implementation.
    /// Maybe more info that wanted in some cases. But certainly enough.
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for GdsError {}
impl From<std::io::Error> for GdsError {
    fn from(e: std::io::Error) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl From<std::str::Utf8Error> for GdsError {
    fn from(e: std::str::Utf8Error) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl From<String> for GdsError {
    fn from(e: String) -> Self {
        GdsError::Str(e)
    }
}
impl From<&str> for GdsError {
    fn from(e: &str) -> Self {
        GdsError::Str(e.to_string())
    }
}
impl From<layout21utils::ser::Error> for GdsError {
    fn from(e: layout21utils::ser::Error) -> Self {
        Self::Boxed(Box::new(e))
    }
}

/// Our helper for "do not serialize default `false` boolean values". 
/// This is a function primarily because those are what `#[serde(skip_serializing_if)]` understands. 
/// (Or at least what we understand how to make it understand).
fn is_false(b:& bool) -> bool {
    !b
}

#[cfg(any(test, feature = "selftest"))]
/// Check `lib` matches across a write-read round-trip cycle
pub fn roundtrip(lib: &GdsLibrary) -> GdsResult<()> {
    use std::io::{Read, Seek, SeekFrom};
    use tempfile::tempfile;

    // Write to a temporary file
    let mut file = tempfile()?;
    lib.write(&mut file)?;

    // Rewind to the file-start, and read it back
    file.seek(SeekFrom::Start(0))?;
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes)?;
    let lib2 = GdsLibrary::from_bytes(&bytes)?;

    // And check the two line up
    assert_eq!(*lib, lib2);
    Ok(())
}
