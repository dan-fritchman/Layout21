//!
//! # Gds21 Integrated Circuit Layout Parser & Writer
//!
//! GDSII is the IC industry's de facto standard for storing and sharing layout data.
//! Gds21 is a library for reading and creating GDSII data, similar to and largely inspired by libraries such as [gdstk](https://github.com/heitzmann/gdstk) and its predecessor [gdspy](https://github.com/heitzmann/gdspy).
//! Gds21 differs in being designed primarily as an interface layer to GDSII for the larger [Layout21](https://github.com/dan-fritchman/Layout21) library.
//! Reading and generating GDSII-format data are primary goals;
//! offering ease-of-use functionality for more elaborate manipulations of GDS data is not.
//! (Although these manipulations can be performed on Gds21's data structures).
//! Gds21 accordingly stores layout data on GDSII's terms, using GDSII's idioms and naming conventions.
//!
//! Layout data is represented in three primary forms:
//!
//! * A short tree with three layers:
//!   * The root is a [GdsLibrary], which primarily consists of a set of cells ([GdsStruct]s), and secondarily a set of metadata.
//!     Each [GdsLibrary] is a universe unto itself, in that it has no mechanisms for comprehending layout cells or data defined outside itself.
//!     On-disk each [GdsLibrary] is typically paired one-to-one with a `.gds` file.
//!   * Libraries consist of cell definitions AKA [GdsStruct]s, which define each layout cell (or module, or "struct" in GDSII terms).
//!   * Cells consist of [GdsElement]s, an enumeration which includes individual polygons ([GdsBoundary]),
//!     instances of other layout cells ([GdsStructRef]), text ([GdsTextElem]), and a few other geometric elements.
//! * For storage on disk, the [GdsLibrary] tree is flattened to a series of [GdsRecord]s.
//!   These records indicate the beginning, end, and content of each tree-node.
//!   Detailed descriptions of these records comprise the majority of the GDSII spec.
//! * Records are stored on-disk in binary form as detailed in the GDSII spec.
//!   Each includes a record-type header, datatype, length field, and optional additional content.
//!   These raw-bytes are never stored by Gds21, only generated and consumed on their way into and out of [Read] and [Write] objects (typically [File]s).
//!
//! ## Alternate Serialization
//!
//! Each element in Gds21's [GdsLibrary] tree is [serde]-serializable.
//! Gds21 includes dependencies for serializing and de-serializing to and from [JSON](serde_json), [YAML](serde_yaml), and [TOML](toml) formats.
//! Note these text-based representations will generally be substantially larger than binary GDSII data.
//!
//! ## Usage
//!
//! Loading a [GdsLibrary] from disk:
//!
//! ```skip
//! let lib = GdsLibrary::load("sample.gds")?;
//! ```
//!
//! Creating a new and empty [GdsLibrary], and adding a [GdsStruct] cell-definition:
//!
//! ```
//! use gds21::{GdsLibrary, GdsStruct};
//! let mut lib = GdsLibrary::new("mylib");
//! lib.structs.push(GdsStruct::new("mycell"));
//! ```
//!
//! Saving a [GdsLibrary] to disk:
//!
//! ```skip
//! lib.save("mylib.gds");
//! ```
//!
//! Converting a [GdsLibrary] to JSON, YAML, or TOML:
//!
//! ```
//! let lib = gds21::GdsLibrary::new("mylib");
//! let json = serde_json::to_string(&lib);
//! let yaml = serde_yaml::to_string(&lib);
//! let toml = toml::to_string(&lib);
//! ```
//!

use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fs::File;
#[allow(unused_imports)]
use std::io::prelude::*;
use std::io::{BufWriter, Cursor, Read, Seek, SeekFrom, Write};
use std::{fmt, mem, str};

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use chrono::prelude::*;
use chrono::{Datelike, NaiveDate, NaiveDateTime};
use derive_more::{Add, AddAssign, Sub, SubAssign};
use enum_dispatch::enum_dispatch;
use memmap::Mmap;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use serde::{Deserialize, Serialize};

#[macro_use]
extern crate derive_builder;

///
/// # Gds Record Types
///
/// In the numeric-order specified by GDSII, for automatic [FromPrimitive] conversions.
///
#[derive(FromPrimitive, Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
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
    BgnLib { dates: Vec<i16> },
    LibName(String),
    Units(f64, f64),
    EndLib,
    BgnStruct { dates: Vec<i16> },
    StructName(String),    // STRNAME
    StructRefName(String), // SNAME
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
    TapeCode(Vec<i16>), // Note: always length 6
    Format(i16),
    Mask(String),
    EndMasks,
    LibDirSize(i16),
    SrfName(String),
    LibSecur(i16),
}
impl GdsRecord {
    /// Encode into bytes and write onto `writer`
    pub fn encode(&self, writer: &mut impl Write) -> GdsResult<()> {
        // This is split in two parts - header and data -
        // largely to ease handling the variety of datatypes

        // A quick closure for GDS's "even-lengths-only allowed" strings
        let gds_strlen = |s: &str| -> usize { s.len() + s.len() % 2 };
        // First grab the header info: RecordType, DataType, and length
        use GdsDataType::{BitArray, NoData, Str, F64, I16, I32};
        let (rtype, dtype, len) = match self {
            // Library-Level Records
            GdsRecord::Header { .. } => (GdsRecordType::Header, I16, 2),
            GdsRecord::BgnLib { .. } => (GdsRecordType::BgnLib, I16, 24),
            GdsRecord::LibName(s) => (GdsRecordType::LibName, Str, gds_strlen(s)),
            GdsRecord::Units(_, _) => (GdsRecordType::Units, F64, 16),
            GdsRecord::EndLib => (GdsRecordType::EndLib, NoData, 0),

            // Structure (Cell) Level Records
            GdsRecord::BgnStruct { .. } => (GdsRecordType::BgnStruct, I16, 24),
            GdsRecord::StructName(s) => (GdsRecordType::StructName, Str, gds_strlen(s)),
            GdsRecord::StructRefName(s) => (GdsRecordType::StructRefName, Str, gds_strlen(s)),
            GdsRecord::EndStruct => (GdsRecordType::EndStruct, NoData, 0),

            // Element-Level Records
            GdsRecord::Boundary => (GdsRecordType::Boundary, NoData, 0),
            GdsRecord::Path => (GdsRecordType::Path, NoData, 0),
            GdsRecord::StructRef => (GdsRecordType::StructRef, NoData, 0),
            GdsRecord::ArrayRef => (GdsRecordType::ArrayRef, NoData, 0),
            GdsRecord::Text => (GdsRecordType::Text, NoData, 0),
            GdsRecord::Layer(_) => (GdsRecordType::Layer, I16, 2),
            GdsRecord::DataType(_) => (GdsRecordType::DataType, I16, 2),
            GdsRecord::Width(_) => (GdsRecordType::Width, I32, 4),
            GdsRecord::Xy(d) => (GdsRecordType::Xy, I32, 4 * d.len()),
            GdsRecord::EndElement => (GdsRecordType::EndElement, NoData, 0),

            // More (less well-categorized here) record-types
            GdsRecord::ColRow { .. } => (GdsRecordType::ColRow, I16, 4),
            GdsRecord::Node => (GdsRecordType::Node, NoData, 0),
            GdsRecord::TextType(_) => (GdsRecordType::TextType, I16, 2),
            GdsRecord::Presentation(_, _) => (GdsRecordType::Presentation, BitArray, 2),
            GdsRecord::String(s) => (GdsRecordType::String, Str, gds_strlen(s)),
            GdsRecord::Strans(_, _) => (GdsRecordType::Strans, BitArray, 2),
            GdsRecord::Mag(_) => (GdsRecordType::Mag, F64, 8),
            GdsRecord::Angle(_) => (GdsRecordType::Angle, F64, 8),
            GdsRecord::RefLibs(s) => (GdsRecordType::RefLibs, Str, gds_strlen(s)),
            GdsRecord::Fonts(s) => (GdsRecordType::Fonts, Str, gds_strlen(s)),
            GdsRecord::PathType(_) => (GdsRecordType::PathType, I16, 2),
            GdsRecord::Generations(_) => (GdsRecordType::Generations, I16, 2),
            GdsRecord::AttrTable(s) => (GdsRecordType::AttrTable, Str, gds_strlen(s)),
            GdsRecord::ElemFlags(_, _) => (GdsRecordType::ElemFlags, BitArray, 2),
            GdsRecord::Nodetype(_) => (GdsRecordType::Nodetype, I16, 2),
            GdsRecord::PropAttr(_) => (GdsRecordType::PropAttr, I16, 2),
            GdsRecord::PropValue(s) => (GdsRecordType::PropValue, Str, gds_strlen(s)),
            GdsRecord::Box => (GdsRecordType::Box, NoData, 0),
            GdsRecord::BoxType(_) => (GdsRecordType::BoxType, I16, 2),
            GdsRecord::Plex(_) => (GdsRecordType::Plex, I32, 4),
            GdsRecord::BeginExtn(_) => (GdsRecordType::BeginExtn, I32, 4),
            GdsRecord::EndExtn(_) => (GdsRecordType::EndExtn, I32, 4),
            GdsRecord::TapeNum(_) => (GdsRecordType::TapeNum, I16, 2),
            GdsRecord::TapeCode(_) => (GdsRecordType::TapeCode, I16, 12),
            GdsRecord::Format(_) => (GdsRecordType::Format, I16, 2),
            GdsRecord::Mask(s) => (GdsRecordType::Mask, Str, gds_strlen(s)),
            GdsRecord::EndMasks => (GdsRecordType::EndMasks, NoData, 0),
            GdsRecord::LibDirSize(_) => (GdsRecordType::LibDirSize, I16, 2),
            GdsRecord::SrfName(s) => (GdsRecordType::SrfName, Str, gds_strlen(s)),
            GdsRecord::LibSecur(_) => (GdsRecordType::LibSecur, I16, 2),
        };
        // Send those header-bytes to the writer.
        // Include the four header bytes in total-length.
        match u16::try_from(len + 4) {
            Ok(val) => writer.write_u16::<BigEndian>(val)?,
            Err(_) => return Err(GdsError::RecordLen(len)),
        };
        writer.write_u8(rtype as u8)?;
        writer.write_u8(dtype as u8)?;

        // Now write the data portion
        // This section is generally organized by DataType
        match self {
            // NoData
            GdsRecord::EndLib
            | GdsRecord::EndStruct
            | GdsRecord::Boundary
            | GdsRecord::Path
            | GdsRecord::StructRef
            | GdsRecord::ArrayRef
            | GdsRecord::Text
            | GdsRecord::EndElement
            | GdsRecord::Node
            | GdsRecord::Box
            | GdsRecord::EndMasks => (),

            // BitArrays
            GdsRecord::Presentation(d0, d1)
            | GdsRecord::Strans(d0, d1)
            | GdsRecord::ElemFlags(d0, d1) => {
                writer.write_u8(*d0)?;
                writer.write_u8(*d1)?;
            }
            // Single I16s
            GdsRecord::Header { version: d }
            | GdsRecord::Layer(d)
            | GdsRecord::DataType(d)
            | GdsRecord::TextType(d)
            | GdsRecord::PathType(d)
            | GdsRecord::Generations(d)
            | GdsRecord::Nodetype(d)
            | GdsRecord::PropAttr(d)
            | GdsRecord::BoxType(d)
            | GdsRecord::TapeNum(d)
            | GdsRecord::Format(d)
            | GdsRecord::LibDirSize(d)
            | GdsRecord::LibSecur(d) => writer.write_i16::<BigEndian>(*d)?,

            // Single I32s
            GdsRecord::Width(d)
            | GdsRecord::Plex(d)
            | GdsRecord::BeginExtn(d)
            | GdsRecord::EndExtn(d) => writer.write_i32::<BigEndian>(*d)?,
            // Single F64s
            GdsRecord::Mag(d) | GdsRecord::Angle(d) => {
                writer.write_u64::<BigEndian>(GdsFloat64::encode(*d))?
            }
            // "Structs"
            GdsRecord::Units(d0, d1) => {
                writer.write_u64::<BigEndian>(GdsFloat64::encode(*d0))?;
                writer.write_u64::<BigEndian>(GdsFloat64::encode(*d1))?;
            }
            GdsRecord::ColRow { cols, rows } => {
                writer.write_i16::<BigEndian>(*cols)?;
                writer.write_i16::<BigEndian>(*rows)?;
            }
            // Vectors
            GdsRecord::TapeCode(d)
            | GdsRecord::BgnLib { dates: d }
            | GdsRecord::BgnStruct { dates: d } => {
                for val in d.iter() {
                    writer.write_i16::<BigEndian>(*val)?;
                }
            }
            GdsRecord::Xy(d) => {
                for val in d.iter() {
                    writer.write_i32::<BigEndian>(*val)?;
                }
            }
            // Strings
            GdsRecord::LibName(s)
            | GdsRecord::StructName(s)
            | GdsRecord::StructRefName(s)
            | GdsRecord::String(s)
            | GdsRecord::RefLibs(s)
            | GdsRecord::Fonts(s)
            | GdsRecord::AttrTable(s)
            | GdsRecord::PropValue(s)
            | GdsRecord::Mask(s)
            | GdsRecord::SrfName(s) => {
                for b in s.as_bytes() {
                    writer.write_u8(*b)?;
                }
                if s.len() % 2 != 0 {
                    // Pad odd-length strings with a zero-valued byte
                    writer.write_u8(0x00)?;
                }
            }
        };
        Ok(())
    }
}
/// # Gds DataType Enumeration
///
/// In order as decoded from 16-bit integers in binary data
#[derive(FromPrimitive, Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
pub enum GdsDataType {
    NoData = 0,
    BitArray = 1,
    I16 = 2,
    I32 = 3,
    F32 = 4,
    F64 = 5,
    Str = 6,
}
/// # GDSII's Home-Grown Floating-Point Format  
///
/// Incredibly, GDSII is old enough to have its own float-format,
/// like most computers did before IEEE754.
///
/// The [GdsFloat64] struct is not used as a data-store, but largely a namespace
/// for the `encode` and `decode` operations to and from IEEE754 double-precision format.
///
pub struct GdsFloat64;
impl GdsFloat64 {
    /// Decode eight GDSII-float-encoded bytes to `f64`
    pub fn decode(bytes: &[u8]) -> GdsResult<f64> {
        if bytes.len() != 8 {
            return Err(GdsError::Decode); // Bad length
        }
        let neg = (bytes[0] & 0x80) != 0; // Sign bit
        let exp: i32 = (bytes[0] & 0x7F) as i32 - 64; // Exponent 7b
                                                      // Create the initially integer-valued mantissa
                                                      // `wrapping_shl` is essentially `<<`; unclear why `u64` prefers the former.
        let mantissa: u64 = (bytes[1] as u64).wrapping_shl(8 * 6)
            | (bytes[2] as u64).wrapping_shl(8 * 5)
            | (bytes[3] as u64).wrapping_shl(8 * 4)
            | (bytes[4] as u64).wrapping_shl(8 * 3)
            | (bytes[5] as u64).wrapping_shl(8 * 2)
            | (bytes[6] as u64).wrapping_shl(8)
            | (bytes[7] as u64);
        // And apply its normalization to the range (1/16, 1)
        let mantissa: f64 = mantissa as f64 / 2f64.powi(8 * 7);
        // Combine everything into our overall value
        let val: f64 = if neg {
            -1.0 * mantissa * 16f64.powi(exp)
        } else {
            mantissa * 16f64.powi(exp)
        };
        Ok(val)
    }
    /// Encode `f64` to eight bytes, this time represented as `u64`.
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
        let result: u64 = (top as u64).wrapping_shl(56) | (mantissa & 0x00FFFFFFFFFFFFFF);
        return result;
    }
}

/// Placeholder for Unsupported (But Spec-Valid) Features
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct Unsupported;

/// # Gds Translation Settings
/// Reflection, rotation, and magnification for text-elements and references.
/// As configured by `STRANS` records.
#[derive(Default, Clone, Debug, Deserialize, Serialize, PartialEq)]
pub struct GdsStrans {
    // Required Fields
    /// Reflection
    pub reflected: bool,
    /// Absolute Magnification Setting
    pub abs_mag: bool,
    /// Absolute Angle Setting
    pub abs_angle: bool,

    // Optional Fields
    /// Magnification Factor. Defaults to 1.0 if not specified.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mag: Option<f64>,
    /// Angle, in degrees counter-clockwise. Defaults to zero if not specified.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub angle: Option<f64>,
}
impl GdsStrans {
    /// Decode boolean fields from bytes
    fn decode(d0: u8, d1: u8) -> Self {
        Self {
            reflected: d0 & 0x80 != 0,
            abs_mag: d1 & 0x04 != 0,
            abs_angle: d1 & 0x02 != 0,
            ..Default::default()
        }
    }
    /// Parse from records
    fn parse(it: &mut GdsParser, d0: u8, d1: u8) -> GdsResult<Self> {
        // Decode the first two bytes
        let mut s = Self::decode(d0, d1);
        // And parse optional magnitude & angle
        loop {
            match it.peek() {
                Some(GdsRecord::Mag(d)) => {
                    s.mag = Some(*d);
                    it.next()?; // Advance the iterator
                }
                Some(GdsRecord::Angle(d)) => {
                    s.angle = Some(*d);
                    it.next()?; // Advance the iterator
                }
                _ => break,
            }
        }
        Ok(s)
    }
}
impl ToRecords for GdsStrans {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Strans(
            (self.reflected as u8) << 7,
            (self.abs_mag as u8) << 2 | (self.abs_angle as u8) << 1,
        )];
        if let Some(ref e) = self.mag {
            records.push(GdsRecord::Mag(*e));
        }
        if let Some(ref e) = self.angle {
            records.push(GdsRecord::Angle(*e));
        }
        records
    }
}
/// # Gds Text-Presentation Flags
/// Sets fonts, text justification, and the like.
/// Stored in raw `u8` form.
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsPresentation(u8, u8);

/// # Gds Element Flags
/// As configured by `ELFLAGS` records.
/// ElemFlags two bytes of bit-fields are stored in raw `u8` form.
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsElemFlags(u8, u8);

/// # Gds Plex
/// From the spec:
/// "A unique positive number which is common to all elements of the Plex to which this element belongs."
/// In Gds21's experience, `PLEX` records and settings are highly uncommon.
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsPlex(i32);

/// # Gds Library Units
///
/// Each GDSII Library has two length-units, referred to as "DB Units" and "User Units" respectively.
/// Essentially all other spatial data throughout the Library is denoted in "DB Units".
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
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsUnits(f64, f64);
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
    /// * User-Unit = 1µm (1000x the DB-Unit )
    fn default() -> Self {
        Self(1e-3, 1e-9)
    }
}

/// # Gds Spatial Point
/// Coordinate in (x,y) layout-space.
/// Denoted in each [GdsLibrary]'s [GdsUnits].
#[derive(Debug, Clone, Default, Deserialize, Serialize, PartialEq, Eq)]
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
        pts.iter().map(|pt| GdsPoint::new(pt.0, pt.1)).collect()
    }
    /// Convert from a two-element vector
    fn parse(from: &Vec<i32>) -> GdsResult<Self> {
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
    fn parse_vec(from: &[i32]) -> GdsResult<Vec<GdsPoint>> {
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
    fn flatten(&self) -> Vec<i32> {
        vec![self.x, self.y]
    }
    /// Convert an n-element vector of [GdsPoint]s to a 2n-element i32 vector.
    fn flatten_vec(src: &Vec<GdsPoint>) -> Vec<i32> {
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
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
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
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
pub struct GdsProperty {
    /// Attribute Number
    pub attr: i16,
    /// Attribute Value
    pub value: String,
}
impl GdsProperty {
    /// Parse from record-iterator `it`
    /// Numeric attribute `attr` is collected beforehand,
    /// as its record is the indication to parse an (attr, value) pair.
    fn parse(it: &mut GdsParser, attr: i16) -> GdsResult<Self> {
        // `PropAttr` records must *immediately* be followed by `PropValue`,
        // or parsing/ decoding fails.
        let value = if let Some(GdsRecord::PropValue(v)) = it.next()? {
            v
        } else {
            return Err(GdsError::Decode);
        };
        Ok(Self { attr, value })
    }
}
///
/// # Gds Path Element
///
/// Spec BNF:
/// ```text
/// PATH [ELFLAGS] [PLEX] LAYER DATATYPE [PATHTYPE] [WIDTH] XY [BGNEXTN] [ENDEXTN])
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(pattern = "owned", setter(into), private)]
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
impl GdsPath {
    /// Parse from record-iterator `it`
    fn parse(it: &mut GdsParser) -> GdsResult<GdsPath> {
        let mut b = GdsPathBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        while let Some(r) = it.next()? {
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::DataType(d) => b.datatype(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse_vec(&d)?),
                GdsRecord::Width(d) => b.width(d),
                GdsRecord::PathType(d) => b.path_type(d),
                GdsRecord::BeginExtn(d) => b.begin_extn(d),
                GdsRecord::EndExtn(d) => b.end_extn(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(attr) => {
                    props.push(GdsProperty::parse(it, attr)?);
                    b
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Path)),
            };
        }
        b = b.properties(props);
        Ok(b.build()?)
    }
}
impl ToRecords for GdsPath {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Path];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::DataType(self.datatype));
        if let Some(ref e) = self.path_type {
            records.push(GdsRecord::PathType(*e));
        }
        if let Some(ref e) = self.width {
            records.push(GdsRecord::Width(*e));
        }
        if let Some(ref e) = self.begin_extn {
            records.push(GdsRecord::BeginExtn(*e));
        }
        if let Some(ref e) = self.end_extn {
            records.push(GdsRecord::EndExtn(*e));
        }
        records.push(GdsRecord::Xy(GdsPoint::flatten_vec(&self.xy)));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
///
/// # Gds Boundary Element
///
/// The most common type for closed-form shapes in GDSII.
/// Most IC layout is comprised of [GdsBoundary] elements, which represent individual polygons.
/// GDSII dictates that the first two and final two coordinates in each [GdsBoundary]
/// shall be identical, "closing" the polygon.
/// Hence an N-sided polygon is represented by a 2(N+1)-length `xy` vector.
///
/// Spec BNF:
/// ```text
/// BOUNDARY [ELFLAGS] [PLEX] LAYER DATATYPE XY
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(pattern = "owned", setter(into), private)]
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
impl GdsBoundary {
    /// Parse from record-iterator `it`
    fn parse(it: &mut GdsParser) -> GdsResult<GdsBoundary> {
        let mut b = GdsBoundaryBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        while let Some(r) = it.next()? {
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::DataType(d) => b.datatype(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse_vec(&d)?),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(attr) => {
                    props.push(GdsProperty::parse(it, attr)?);
                    b
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Boundary)),
            };
        }
        b = b.properties(props);
        Ok(b.build()?)
    }
}
impl ToRecords for GdsBoundary {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Boundary];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::DataType(self.datatype));
        records.push(GdsRecord::Xy(GdsPoint::flatten_vec(&self.xy)));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
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
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(pattern = "owned", setter(into), private)]
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
impl GdsStructRef {
    fn parse(it: &mut GdsParser) -> GdsResult<GdsStructRef> {
        let mut b = GdsStructRefBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        while let Some(r) = it.next()? {
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::StructRefName(d) => b.name(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse(&d)?),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::Strans(d0, d1) => b.strans(GdsStrans::parse(it, d0, d1)?),
                GdsRecord::PropAttr(attr) => {
                    props.push(GdsProperty::parse(it, attr)?);
                    b
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::StructRef)),
            };
        }
        b = b.properties(props);
        Ok(b.build()?)
    }
}
impl ToRecords for GdsStructRef {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::StructRef];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::StructRefName(self.name.clone()));
        if let Some(ref e) = self.strans {
            let rs = e.to_records();
            records.extend(rs);
        }
        records.push(GdsRecord::Xy(GdsPoint::flatten(&self.xy)));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
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
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(pattern = "owned", setter(into), private)]
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
impl GdsArrayRef {
    fn parse(it: &mut GdsParser) -> GdsResult<GdsArrayRef> {
        let mut b = GdsArrayRefBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        while let Some(r) = it.next()? {
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::StructRefName(d) => b.name(d),
                GdsRecord::ColRow { rows, cols } => b.rows(rows).cols(cols),
                GdsRecord::Xy(d) => {
                    // XY coordinates must be a three-element array.
                    // First parse a generic [GdsRecord::Xy] to a vector,
                    // and then convert, checking length in the process.
                    let v = GdsPoint::parse_vec(&d)?;
                    let xy: [GdsPoint; 3] = match v.try_into() {
                        Ok(xy) => xy,
                        Err(_) => {
                            return Err(GdsError::Str("Invalid XY for GdsArrayRef".into()));
                        }
                    };
                    b.xy(xy)
                }
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::Strans(d0, d1) => b.strans(GdsStrans::parse(it, d0, d1)?),
                GdsRecord::PropAttr(attr) => {
                    props.push(GdsProperty::parse(it, attr)?);
                    b
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::ArrayRef)),
            };
        }
        b = b.properties(props);
        Ok(b.build()?)
    }
}
impl ToRecords for GdsArrayRef {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::ArrayRef];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::StructRefName(self.name.clone()));
        if let Some(ref e) = self.strans {
            let rs = e.to_records();
            records.extend(rs);
        }
        records.push(GdsRecord::ColRow {
            cols: self.cols,
            rows: self.rows,
        });
        let mut xy = GdsPoint::flatten(&self.xy[0]);
        xy.extend(GdsPoint::flatten(&self.xy[1]));
        xy.extend(GdsPoint::flatten(&self.xy[2]));
        records.push(GdsRecord::Xy(xy));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
///
/// # Gds Text Element
///
/// Spec BNF:
/// ```text
/// TEXT [ELFLAGS] [PLEX] LAYER
/// TEXTTYPE [PRESENTATION] [PATHTYPE] [WIDTH] [<strans>] XY STRING
/// ```
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(pattern = "owned", setter(into), private)]
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
impl GdsTextElem {
    /// Parse a [GdsTextElem] from an iterator of [GdsRecord]s.
    /// Assumes the initial `Text` record has already been parsed.
    fn parse(it: &mut GdsParser) -> GdsResult<GdsTextElem> {
        let mut b = GdsTextElemBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        while let Some(r) = it.next()? {
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::TextType(d) => b.texttype(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse(&d)?),
                GdsRecord::String(d) => b.string(d),
                GdsRecord::Presentation(d0, d1) => b.presentation(GdsPresentation(d0, d1)),
                GdsRecord::PathType(d) => b.path_type(d),
                GdsRecord::Width(d) => b.width(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::Strans(d0, d1) => b.strans(GdsStrans::parse(it, d0, d1)?),
                GdsRecord::PropAttr(attr) => {
                    props.push(GdsProperty::parse(it, attr)?);
                    b
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Text)),
            };
        }
        b = b.properties(props);
        Ok(b.build()?)
    }
}
impl ToRecords for GdsTextElem {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Text];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::TextType(self.texttype));
        if let Some(ref e) = self.presentation {
            records.push(GdsRecord::Presentation(e.0, e.1));
        }
        if let Some(ref e) = self.path_type {
            records.push(GdsRecord::PathType(*e));
        }
        if let Some(ref e) = self.width {
            records.push(GdsRecord::Width(*e));
        }
        if let Some(ref e) = self.strans {
            let rs = e.to_records();
            records.extend(rs);
        }
        records.push(GdsRecord::Xy(GdsPoint::flatten(&self.xy)));
        records.push(GdsRecord::String(self.string.clone()));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
///
/// # Gds Node Element
///
/// Spec BNF:
/// ```text
/// NODE [ELFLAGS] [PLEX] LAYER NODETYPE XY
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(pattern = "owned", setter(into), private)]
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
impl GdsNode {
    fn parse(it: &mut GdsParser) -> GdsResult<GdsNode> {
        let mut b = GdsNodeBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        while let Some(r) = it.next()? {
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::Nodetype(d) => b.nodetype(d),
                GdsRecord::Xy(d) => b.xy(GdsPoint::parse_vec(&d)?),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(attr) => {
                    props.push(GdsProperty::parse(it, attr)?);
                    b
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Node)),
            };
        }
        b = b.properties(props);
        Ok(b.build()?)
    }
}
impl ToRecords for GdsNode {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Node];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::Nodetype(self.nodetype));
        records.push(GdsRecord::Xy(GdsPoint::flatten_vec(&self.xy)));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
///
/// # Gds Box Element
///
/// Spec BNF:
/// ```text
/// BOX [ELFLAGS] [PLEX] LAYER BOXTYPE XY
/// ```
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(pattern = "owned", setter(into), private)]
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
impl GdsBox {
    fn parse(it: &mut GdsParser) -> GdsResult<GdsBox> {
        let mut b = GdsBoxBuilder::default();
        let mut props: Vec<GdsProperty> = Vec::new();

        while let Some(r) = it.next()? {
            b = match r {
                GdsRecord::EndElement => break, // End-of-element
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::BoxType(d) => b.boxtype(d),
                GdsRecord::Xy(d) => {
                    // XY coordinates must be a five-element array.
                    // First parse a generic [GdsRecord::Xy] to a vector,
                    // and then convert, checking length in the process.
                    let v = GdsPoint::parse_vec(&d)?;
                    let xy: [GdsPoint; 5] = match v.try_into() {
                        Ok(xy) => xy,
                        Err(_) => {
                            return Err(GdsError::Str("Invalid XY for GdsBox".into()));
                        }
                    };
                    b.xy(xy)
                }
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(attr) => {
                    props.push(GdsProperty::parse(it, attr)?);
                    b
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Box)),
            };
        }
        b = b.properties(props);
        Ok(b.build()?)
    }
}
impl ToRecords for GdsBox {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![GdsRecord::Box];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::Layer(self.layer));
        records.push(GdsRecord::BoxType(self.boxtype));
        records.push(GdsRecord::Xy(GdsPoint::flatten_vec(&self.xy.to_vec())));
        for prop in self.properties.iter() {
            records.push(GdsRecord::PropAttr(prop.attr));
            records.push(GdsRecord::PropValue(prop.value.clone()));
        }
        records.push(GdsRecord::EndElement);
        records
    }
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
#[enum_dispatch]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub enum GdsElement {
    GdsBoundary(GdsBoundary),
    GdsPath(GdsPath),
    GdsStructRef(GdsStructRef),
    GdsArrayRef(GdsArrayRef),
    GdsTextElem(GdsTextElem),
    GdsNode(GdsNode),
    GdsBox(GdsBox),
}
/// Summary statistics for a [GdsLibrary] or [GdsStruct].
/// Total numbers of elements of each type.
#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Add, AddAssign, Sub, SubAssign)]
pub struct GdsStats {
    libraries: usize,
    structs: usize,
    boundaries: usize,
    paths: usize,
    struct_refs: usize,
    array_refs: usize,
    text_elems: usize,
    nodes: usize,
    boxes: usize,
}
/// Trait for conversion of [GdsElement]s and similar tree-elements to sequences of [GdsRecord]s.  
///
/// Dispatched from the [GdsElement] enum by the `enum_dispatch` macros.
#[enum_dispatch(GdsElement)]
pub trait ToRecords {
    /// Convert to a Vector of [GdsRecord], ordered as dictated by the GDSII spec BNF.
    fn to_records(&self) -> Vec<GdsRecord>;
}
/// # Gds Modification Dates & Times
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
pub struct GdsDateTimes {
    /// Last Modification Date & Time
    pub modified: NaiveDateTime,
    /// Last Access Date & Time
    pub accessed: NaiveDateTime,
}
impl GdsDateTimes {
    /// Parse from GDSII's vector of i16's format
    fn parse(d: Vec<i16>) -> GdsResult<Self> {
        if d.len() != 12 {
            return Err(GdsError::Decode);
        }
        Ok(Self {
            modified: NaiveDate::from_ymd(d[0] as i32, d[1] as u32, d[2] as u32).and_hms(
                d[3] as u32,
                d[4] as u32,
                d[5] as u32,
            ),
            accessed: NaiveDate::from_ymd(d[6] as i32, d[7] as u32, d[8] as u32).and_hms(
                d[9] as u32,
                d[10] as u32,
                d[11] as u32,
            ),
        })
    }
    /// Encode in GDSII's vector of i16's format
    pub fn encode(&self) -> Vec<i16> {
        vec![
            self.modified.date().year() as i16,
            self.modified.date().month() as i16,
            self.modified.date().day() as i16,
            self.modified.time().hour() as i16,
            self.modified.time().minute() as i16,
            self.modified.time().second() as i16,
            self.accessed.date().year() as i16,
            self.accessed.date().month() as i16,
            self.accessed.date().day() as i16,
            self.accessed.time().hour() as i16,
            self.accessed.time().minute() as i16,
            self.accessed.time().second() as i16,
        ]
    }
}
impl Default for GdsDateTimes {
    /// Default dates & times: what better time than now!
    fn default() -> Self {
        let now = Utc::now().naive_utc();
        Self {
            modified: now.clone(),
            accessed: now.clone(),
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
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(pattern = "owned", setter(into), private)]
pub struct GdsStruct {
    /// Struct Name
    pub name: String,
    /// Creation/ Modification-Date Info
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
    /// Parse from record-iterator `it`
    fn parse(it: &mut GdsParser, dates: Vec<i16>) -> GdsResult<GdsStruct> {
        let mut strukt = GdsStructBuilder::default();
        // Parse and store the header information: `dates` and `name`
        strukt = strukt.dates(GdsDateTimes::parse(dates)?);
        strukt = match it.next()? {
            Some(GdsRecord::StructName(d)) => strukt.name(d),
            _ => return Err(GdsError::Decode),
        };
        // Parse [GdsElement] records until hitting a [GdsRecord::EndStruct]
        let mut elems = Vec::<GdsElement>::with_capacity(1024);
        while let Some(r) = it.next()? {
            match r {
                GdsRecord::EndStruct => break, // End-of-struct
                GdsRecord::Boundary => elems.push(GdsBoundary::parse(it)?.into()),
                GdsRecord::Text => elems.push(GdsTextElem::parse(it)?.into()),
                GdsRecord::Path => elems.push(GdsPath::parse(it)?.into()),
                GdsRecord::Box => elems.push(GdsBox::parse(it)?.into()),
                GdsRecord::StructRef => elems.push(GdsStructRef::parse(it)?.into()),
                GdsRecord::ArrayRef => elems.push(GdsArrayRef::parse(it)?.into()),
                GdsRecord::Node => elems.push(GdsNode::parse(it)?.into()),
                // Spec-valid but unsupported records
                GdsRecord::PropAttr(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::Struct)))
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Struct)),
            };
        }
        strukt = strukt.elems(elems);
        Ok(strukt.build()?)
    }
    /// Encode and write to `writer`
    pub fn encode(&self, writer: &mut impl Write) -> GdsResult<()> {
        // Write our modification-date info
        GdsRecord::BgnStruct {
            dates: self.dates.encode(),
        }
        .encode(writer)?;
        // Write our name
        GdsRecord::StructName(self.name.clone()).encode(writer)?;
        // Write each of our elements
        for elem in self.elems.iter() {
            for record in elem.to_records().iter() {
                record.encode(writer)?;
            }
        }
        // And its terminator
        GdsRecord::EndStruct.encode(writer)?;
        Ok(())
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
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(pattern = "owned", setter(into), private)]
pub struct GdsLibrary {
    // Required fields
    /// Library Name
    pub name: String,
    /// Gds Spec Version
    pub version: i16,
    // Modification Date(s)
    pub dates: GdsDateTimes,
    /// Spatial Units    
    pub units: GdsUnits,
    /// Struct Definitions
    pub structs: Vec<GdsStruct>,

    // Optional (and all thus far unsupported) fields
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub libdirsize: Option<Unsupported>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub srfname: Option<Unsupported>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub libsecur: Option<Unsupported>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub reflibs: Option<Unsupported>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub fonts: Option<Unsupported>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub attrtable: Option<Unsupported>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub generations: Option<Unsupported>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[builder(default, setter(strip_option))]
    pub format_type: Option<GdsFormatType>,
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
    /// Read a GDS loaded from file at path `file_name`
    pub fn load(file_name: &str) -> GdsResult<GdsLibrary> {
        // Create an iterator over records
        let mut it = GdsParser::open(file_name)?;
        // And parse it to a library-tree
        GdsLibrary::parse(&mut it)
    }
    pub fn scan_(fname: &str) -> GdsResult<()> {
        // FIXME: the actual name "scan" here conflicts with some std-lib trait, figure out what to call this
        GdsScanner::scan(fname)?;
        Ok(())
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
    /// Parse a GdsLibrary from record-iterator `it`
    fn parse(it: &mut GdsParser) -> GdsResult<GdsLibrary> {
        let mut lib = GdsLibraryBuilder::default();
        let mut structs = Vec::<GdsStruct>::with_capacity(1024);
        // Read the Header and its version data
        lib = match it.next()? {
            Some(GdsRecord::Header { version: v }) => lib.version(v),
            _ => return Err(GdsError::Decode),
        };
        // Read the begin-lib
        lib = match it.next()? {
            Some(GdsRecord::BgnLib { dates: d }) => lib.dates(GdsDateTimes::parse(d)?),
            _ => return Err(GdsError::Decode),
        };
        // Iterate over all others
        while let Some(r) = it.next()? {
            lib = match r {
                GdsRecord::EndLib => break, // End-of-library
                GdsRecord::LibName(d) => lib.name(d),
                GdsRecord::Units(d0, d1) => lib.units(GdsUnits(d0, d1)),
                GdsRecord::BgnStruct { dates } => {
                    let strukt = GdsStruct::parse(it, dates)?;
                    structs.push(strukt);
                    lib
                }
                // Spec-valid but unsupported records
                GdsRecord::LibDirSize(_)
                | GdsRecord::SrfName(_)
                | GdsRecord::LibSecur(_)
                | GdsRecord::RefLibs(_)
                | GdsRecord::Fonts(_)
                | GdsRecord::AttrTable(_)
                | GdsRecord::Generations(_)
                | GdsRecord::Format(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::Library)))
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Library)),
            };
        }
        // Add the Vec of structs, and create the Library from its builder
        lib = lib.structs(structs);
        Ok(lib.build()?)
    }
    /// Save to file `fname`
    pub fn save(&self, fname: &str) -> GdsResult<()> {
        let mut file = BufWriter::new(File::create(fname)?);
        self.encode(&mut file)?;
        file.flush()?;
        Ok(())
    }
    /// Encode and write in binary form to `writer`
    pub fn encode(&self, writer: &mut impl Write) -> GdsResult<()> {
        // Check our in-memory self doesn't include any unsupported features
        if self.libdirsize.is_some()
            || self.srfname.is_some()
            || self.libsecur.is_some()
            || self.reflibs.is_some()
            || self.fonts.is_some()
            || self.attrtable.is_some()
            || self.generations.is_some()
            || self.format_type.is_some()
        {
            return Err(GdsError::Unsupported(None, Some(GdsContext::Library)));
        }
        // Write our header content
        GdsRecord::Header {
            version: self.version,
        }
        .encode(writer)?;
        // Write our modification-date info
        GdsRecord::BgnLib {
            dates: self.dates.encode(),
        }
        .encode(writer)?;
        // Write our library name
        GdsRecord::LibName(self.name.clone()).encode(writer)?;
        // Write our units
        GdsRecord::Units(self.units.0, self.units.1).encode(writer)?;
        // Write all of our Structs/Cells
        for strukt in self.structs.iter() {
            strukt.encode(writer)?;
        }
        // And finally, the library terminator
        GdsRecord::EndLib.encode(writer)?;
        Ok(())
    }
}
/// # Gds Record Header
/// Decoded contents of a record's four header bytes.
#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
struct GdsRecordHeader {
    rtype: GdsRecordType,
    dtype: GdsDataType,
    len: u16,
}
/// # GdsStructScan
/// Typically generated by first-pass file scanning.
/// Stores a struct name and byte-offsets in its source file.
#[derive(Debug, Default)]
struct GdsStructScan {
    /// Struct Name
    name: String,
    /// Starting byte offset, at beginning of [BgnStruct]
    start: u64,
    /// Ending byte offset, at end of [EndStruct]
    end: u64,
}
/// # GdsScanner
///
/// Scans a file for [GdsStruct] definitions,
/// creating a first-pass list of their names and byte-locations.
struct GdsScanner {
    /// Reader-helper
    rdr: GdsReader,
    /// Next record-header, stored for peeking
    nxt: GdsRecordHeader,
}
impl GdsScanner {
    /// Create a new GdsReader iterator
    fn new(mut rdr: GdsReader) -> GdsResult<Self> {
        // Decode the first record to initialize our "peeker"
        let nxt = rdr.read_header()?;
        Ok(Self { rdr, nxt })
    }
    /// Open and scan structs in file `fname`
    fn scan(fname: &str) -> GdsResult<Vec<GdsStructScan>> {
        let rdr = GdsReader::open(fname)?;
        let mut me = Self::new(rdr)?;
        me.scan_lib()
    }
    /// Expect / require the next record to be of type `rtype`
    /// Skips over the remainder of its content, if any.
    fn expect(&mut self, rtype: GdsRecordType) -> GdsResult<()> {
        if self.peek().rtype == rtype {
            // Success. Skip over the record and advance to the next.
            self.skip()
        } else {
            self.err()
        }
    }
    /// Get the next record's header, asserting it must be of type `rtype`.
    /// Consuming the remainder of the record is left to the caller.
    fn get(&mut self, rtype: GdsRecordType) -> GdsResult<&GdsRecordHeader> {
        if self.peek().rtype == rtype {
            Ok(self.peek())
        } else {
            self.err()
        }
    }
    /// Advance our iterator and return the next element
    fn next(&mut self) -> GdsResult<GdsRecordHeader> {
        if self.peek().rtype == GdsRecordType::EndLib {
            // Once we reach [EndLib], keep returning it forever
            Ok(self.peek().clone())
        } else {
            self._next()
        }
    }
    fn _next(&mut self) -> GdsResult<GdsRecordHeader> {
        // Decode a new header and swap it with our `nxt`
        let mut rv = self.rdr.read_header()?;
        mem::swap(&mut rv, &mut self.nxt);
        Ok(rv)
    }
    /// Skip over the current record's content, if any, and load the next.
    fn skip(&mut self) -> GdsResult<()> {
        let len = self.nxt.len.into();
        self.rdr.file.seek(SeekFrom::Current(len))?;
        self._next()?;
        Ok(())
    }
    /// Peek at our next record, without advancing
    #[inline(always)]
    fn peek(&self) -> &GdsRecordHeader {
        &self.nxt
    }
    /// Scan the GDS for cell/ struct definitions
    fn scan_lib(&mut self) -> GdsResult<Vec<GdsStructScan>> {
        // Read off header info
        self.expect(GdsRecordType::Header)?;
        self.expect(GdsRecordType::BgnLib)?;
        // Read the Library name
        let len = self.get(GdsRecordType::LibName)?.len;
        let _lib_name = self.rdr.read_str(len)?;
        self.next()?;
        // More header info
        self.expect(GdsRecordType::Units)?;

        // Scan all of the structs
        let mut strukts = Vec::<GdsStructScan>::with_capacity(1024);
        loop {
            let GdsRecordHeader { rtype, .. } = self.peek();
            match rtype {
                GdsRecordType::EndLib => break,
                GdsRecordType::BgnStruct => strukts.push(self.scan_struct()?),
                _ => return self.err(),
            }
        }
        Ok(strukts)
    }
    /// Scan a single struct definition.
    /// Starts *after* the [BgnStruct] *header* has been read (just above).
    /// Returns a [GdsStructScan] if successful.
    fn scan_struct(&mut self) -> GdsResult<GdsStructScan> {
        // Create our scan-structure, and store the start-position of the struct.
        // Note this requires backing up the four header bytes.
        let mut s = GdsStructScan::default();
        s.start = self.rdr.file.stream_position()? - 4;
        // Skip over the remainder of the [BegnStruct] header
        self.skip()?;

        // Read the Struct name
        let len = self.get(GdsRecordType::StructName)?.len;
        s.name = self.rdr.read_str(len)?;
        self.next()?;

        // Scan the struct content. Skip over everything until [EndStruct]
        loop {
            let GdsRecordHeader { rtype, .. } = self.peek();
            match rtype {
                GdsRecordType::EndStruct => {
                    // Note the `len` of [EndStruct] is also zero, so we need not adjust the `end` position.
                    s.end = self.rdr.file.stream_position()?;
                    self.skip()?;
                    break;
                }
                // While many library-header record-types are invalid here,
                // there's at least one we should check for: end-of-library,
                // Lest we find one and get stuck forever.
                GdsRecordType::EndLib => return self.err(),
                // Everything else: skip and continue
                _ => self.skip()?,
            }
        }
        Ok(s)
    }
    /// Error generation helper
    fn err<T>(&mut self) -> GdsResult<T> {
        Err(GdsError::Str(format!(
            "Scanned Invalid Record {:?} at Byte Position {}",
            self.nxt.rtype,
            self.rdr.file.stream_position()?
        )))
    }
}
/// # GdsParser
///
/// A peekable iterator which loads GdsRecords from file, one at a time
struct GdsParser {
    /// File being read
    rdr: GdsReader,
    /// Next record, stored for peeking
    nxt: Option<GdsRecord>,
    /// Number of records read
    numread: usize,
}
impl GdsParser {
    /// Create a new GdsReader iterator for the file at path `fname`
    fn open(fname: &str) -> GdsResult<GdsParser> {
        let rdr = GdsReader::open(fname)?;
        Self::new(rdr)
    }
    /// Create a new GdsReader iterator
    fn new(mut rdr: GdsReader) -> GdsResult<GdsParser> {
        // Decode the first record to initialize our "peeker"
        let nxt = rdr.decode_record()?;
        Ok(GdsParser {
            rdr,
            nxt: Some(nxt),
            numread: 1,
        })
    }
    /// Parse a [GdsLibrary]. Generally the start-state when reading a GDS file.
    fn parse_lib(&mut self) -> GdsResult<GdsLibrary> {
        GdsLibrary::parse(self)
    }
    /// Advance our iterator and return the next element
    fn next(&mut self) -> GdsResult<Option<GdsRecord>> {
        if self.nxt.is_none() || self.nxt == Some(GdsRecord::EndLib) {
            return Ok(None);
        }
        self._next()
    }
    fn _next(&mut self) -> GdsResult<Option<GdsRecord>> {
        // Decode a new Record and swap it with our `nxt`
        let mut rv = Some(self.rdr.decode_record()?);
        mem::swap(&mut rv, &mut self.nxt);
        self.numread += 1;
        Ok(rv)
    }
    /// Peek at our next record, without advancing
    fn peek(&self) -> &Option<GdsRecord> {
        &self.nxt
    }
    /// JSON-Serialize and write (all) contents of the Iterator to `writer`
    #[cfg(any(test, feature = "selftest"))]
    fn write(&mut self, writer: &mut impl Write) -> GdsResult<()> {
        while let Some(r) = self.next()? {
            let entry: (usize, GdsRecord) = (self.numread, r);
            let s = serde_json::to_string(&entry).unwrap();
            write!(writer, "\t")?;
            writer.write_all(s.as_bytes()).unwrap();
            write!(writer, ",\n")?;
        }
        Ok(())
    }
    /// Open a GDS file `gds` and write all GdsRecords to JSON file `json`
    #[cfg(any(test, feature = "selftest"))]
    fn dump(gds: &str, json: &str) -> GdsResult<()> {
        // This streams one record at a time, rather than loading all into memory.
        // Create a ReaderIter from `gds`
        let mut myself = Self::open(gds)?;
        // Create the JSON file
        let mut w = BufWriter::new(File::create(json)?);
        // Write it as a JSON list/sequence; add the opening bracket
        write!(w, "[\n")?;
        // Write all the records
        myself.write(&mut w)?;
        // And close the list
        write!(w, "]\n")?;
        Ok(())
    }
}
/// Size (in bytes) of the read/decode buffer array
const READER_BUFSIZE: usize = 65537;
/// # GdsReader
/// Helper for parsing and scanning GDS coming from files and similar sources.
struct GdsReader {
    /// Read/conversion buffer
    buf: [u8; READER_BUFSIZE], // FIXME: confirm buffer size
    /// File being read
    file: Cursor<Mmap>,
    // Note: past/alternate versions use an API-compatible [BufReader<File>], like so:
    // file: BufReader<File>,
    // Both implement the [Read] and [Seek] traits required by most of [GdsReader]'s methods.
    // The memory-mapping is *much* faster for initial scanning,
    // as [BufReader] seems to have an extra-slow [Seek] implementation.
    // This comes at the cost of injecting the memory-un-safety of loading the [Mmap].
}
impl GdsReader {
    /// Create a [GdsReader] of file `fname`
    fn open(fname: &str) -> GdsResult<GdsReader> {
        let file = File::open(fname)?;
        let mmap = unsafe { Mmap::map(&file)? };
        let cursor = Cursor::new(mmap);
        Ok(Self::new(cursor))
    }
    /// Create a [GdsReader] of `file`
    fn new(file: Cursor<Mmap>) -> GdsReader {
        GdsReader {
            file,
            buf: [0; READER_BUFSIZE],
        }
    }
    /// Read the next record-header from our file.
    /// Returns a [GdsRecordHeader] if successful.
    fn read_header(&mut self) -> GdsResult<GdsRecordHeader> {
        // (Legacy shorthand)
        let file = &mut self.file;
        // Read the 16-bit record-size. (In bytes, including the four header bytes.)
        let len = match file.read_u16::<BigEndian>() {
            Err(ref e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                return Err(GdsError::Decode); // Unexpected end-of-file without `EndLib`
            }
            Err(_) => return Err(GdsError::Decode), // Some other kinda error; raise it.
            Ok(num) if num < 4 => return Err(GdsError::RecordLen(num as usize)), // Invalid (too short) length; throw Error.
            Ok(num) if num % 2 != 0 => return Err(GdsError::RecordLen(num as usize)), // Invalid (odd) length; throw Error.
            Ok(num) => num, // The normal case
        };
        let len = len - 4; // Strip out the four header-bytes
                           // Read and decode its RecordType
        let record_type = file.read_u8()?;
        let record_type: GdsRecordType =
            FromPrimitive::from_u8(record_type).ok_or(GdsError::InvalidRecordType(record_type))?;
        if !record_type.valid() {
            return Err(GdsError::InvalidRecordType(record_type as u8));
        }
        // Read and decode its DataType
        let data_type = file.read_u8()?;
        let data_type =
            FromPrimitive::from_u8(data_type).ok_or(GdsError::InvalidDataType(data_type))?;
        Ok(GdsRecordHeader {
            rtype: record_type,
            dtype: data_type,
            len,
        })
    }
    /// Decode the next binary-encoded [GdsRecord] from open [Read]-object `file`.
    /// Returns a [GdsError] if `file` cursor is not on a record-boundary,
    /// or if binary decoding otherwise fails.
    pub fn decode_record(&mut self) -> GdsResult<GdsRecord> {
        // Read the record header (types and length)
        let GdsRecordHeader { rtype, dtype, len } = self.read_header()?;

        // Based on that header-data, decode to a `GdsRecord`
        use GdsDataType::{BitArray, NoData, Str, F64, I16, I32};
        let record: GdsRecord = match (rtype, dtype, len) {
            // Library-Level Records
            (GdsRecordType::Header, I16, 2) => GdsRecord::Header {
                version: self.read_i16(len)?[0],
            },
            (GdsRecordType::BgnLib, I16, 24) => GdsRecord::BgnLib {
                dates: self.read_i16(len)?,
            },
            (GdsRecordType::LibName, Str, _) => GdsRecord::LibName(self.read_str(len)?),
            (GdsRecordType::Units, F64, 16) => {
                let v = self.read_f64(len)?;
                GdsRecord::Units(v[0], v[1])
            }
            (GdsRecordType::EndLib, NoData, 0) => GdsRecord::EndLib,

            // Structure (Cell) Level Records
            (GdsRecordType::BgnStruct, I16, 24) => GdsRecord::BgnStruct {
                dates: self.read_i16(len)?,
            },
            (GdsRecordType::StructName, Str, _) => GdsRecord::StructName(self.read_str(len)?),
            (GdsRecordType::StructRefName, Str, _) => GdsRecord::StructRefName(self.read_str(len)?),
            (GdsRecordType::EndStruct, NoData, 0) => GdsRecord::EndStruct,

            // Element-Level Records
            (GdsRecordType::Boundary, NoData, 0) => GdsRecord::Boundary,
            (GdsRecordType::Path, NoData, 0) => GdsRecord::Path,
            (GdsRecordType::StructRef, NoData, 0) => GdsRecord::StructRef,
            (GdsRecordType::ArrayRef, NoData, 0) => GdsRecord::ArrayRef,
            (GdsRecordType::Text, NoData, 0) => GdsRecord::Text,
            (GdsRecordType::Layer, I16, 2) => GdsRecord::Layer(self.read_i16(len)?[0]),
            (GdsRecordType::DataType, I16, 2) => GdsRecord::DataType(self.read_i16(len)?[0]),
            (GdsRecordType::Width, I32, 4) => GdsRecord::Width(self.read_i32(len)?[0]),
            (GdsRecordType::Xy, I32, _) => GdsRecord::Xy(self.read_i32(len)?),
            (GdsRecordType::EndElement, NoData, 0) => GdsRecord::EndElement,

            // More (less well-categorized here) record-types
            (GdsRecordType::ColRow, I16, 4) => {
                let d = self.read_i16(len)?;
                GdsRecord::ColRow {
                    cols: d[0],
                    rows: d[1],
                }
            }
            (GdsRecordType::Node, NoData, 0) => GdsRecord::Node,
            (GdsRecordType::TextType, I16, 2) => GdsRecord::TextType(self.read_i16(len)?[0]),
            (GdsRecordType::Presentation, BitArray, 2) => {
                let bytes = self.read_bytes(len)?;
                GdsRecord::Presentation(bytes[0], bytes[1])
            }
            (GdsRecordType::String, Str, _) => GdsRecord::String(self.read_str(len)?),
            (GdsRecordType::Strans, BitArray, 2) => {
                let bytes = self.read_bytes(len)?;
                GdsRecord::Strans(bytes[0], bytes[1])
            }
            (GdsRecordType::Mag, F64, 8) => GdsRecord::Mag(self.read_f64(len)?[0]),
            (GdsRecordType::Angle, F64, 8) => GdsRecord::Angle(self.read_f64(len)?[0]),
            (GdsRecordType::RefLibs, Str, _) => GdsRecord::RefLibs(self.read_str(len)?),
            (GdsRecordType::Fonts, Str, _) => GdsRecord::Fonts(self.read_str(len)?),
            (GdsRecordType::PathType, I16, 2) => GdsRecord::PathType(self.read_i16(len)?[0]),
            (GdsRecordType::Generations, I16, 2) => GdsRecord::Generations(self.read_i16(len)?[0]),
            (GdsRecordType::AttrTable, Str, _) => GdsRecord::AttrTable(self.read_str(len)?),
            (GdsRecordType::ElemFlags, BitArray, 2) => {
                let bytes = self.read_bytes(len)?;
                GdsRecord::ElemFlags(bytes[0], bytes[1])
            }
            (GdsRecordType::Nodetype, I16, 2) => GdsRecord::Nodetype(self.read_i16(len)?[0]),
            (GdsRecordType::PropAttr, I16, 2) => GdsRecord::PropAttr(self.read_i16(len)?[0]),
            (GdsRecordType::PropValue, Str, _) => GdsRecord::PropValue(self.read_str(len)?),
            (GdsRecordType::Box, NoData, 0) => GdsRecord::Box,
            (GdsRecordType::BoxType, I16, 2) => GdsRecord::BoxType(self.read_i16(len)?[0]),
            (GdsRecordType::Plex, I32, 4) => GdsRecord::Plex(self.read_i32(len)?[0]),
            (GdsRecordType::BeginExtn, I32, 4) => GdsRecord::BeginExtn(self.read_i32(len)?[0]),
            (GdsRecordType::EndExtn, I32, 4) => GdsRecord::EndExtn(self.read_i32(len)?[0]),
            (GdsRecordType::TapeNum, I16, 2) => GdsRecord::TapeNum(self.read_i16(len)?[0]),
            (GdsRecordType::TapeCode, I16, 12) => GdsRecord::TapeCode(self.read_i16(len)?),
            (GdsRecordType::Format, I16, 2) => GdsRecord::Format(self.read_i16(len)?[0]),
            (GdsRecordType::Mask, Str, _) => GdsRecord::Mask(self.read_str(len)?),
            (GdsRecordType::EndMasks, NoData, 0) => GdsRecord::EndMasks,
            (GdsRecordType::LibDirSize, I16, 2) => GdsRecord::LibDirSize(self.read_i16(len)?[0]),
            (GdsRecordType::SrfName, Str, _) => GdsRecord::SrfName(self.read_str(len)?),
            (GdsRecordType::LibSecur, I16, 2) => GdsRecord::LibSecur(self.read_i16(len)?[0]),

            // Failing to meet any of these clauses means this is an invalid record
            _ => return Err(GdsError::RecordDecode(rtype, dtype, len)),
        };
        Ok(record)
    }
    /// Read `len` bytes and convert to `String`
    fn read_str(&mut self, len: u16) -> GdsResult<String> {
        let len: usize = len.into();
        // ASCII Decode. First load bytes into our buffer.
        let mut data = &mut self.buf[0..len];
        self.file.read_exact(data)?;
        // Strip optional end-of-string chars
        let len = data.len();
        if data[len - 1] == 0x00 {
            data = &mut data[0..len - 1];
        }
        // And convert to string
        let s: String = std::str::from_utf8(&data)?.into();
        Ok(s)
    }
    /// Read `len` bytes
    fn read_bytes(&mut self, len: u16) -> Result<Vec<u8>, std::io::Error> {
        let len: usize = len.into();
        let mut rv: Vec<u8> = vec![0; len];
        self.file.read_exact(&mut rv[0..len])?;
        Ok(rv)
    }
    /// Read `len/2` i16s from `len` bytes
    fn read_i16(&mut self, len: u16) -> Result<Vec<i16>, std::io::Error> {
        let len: usize = len.into();
        self.file.read_exact(&mut self.buf[0..len])?;
        let mut rv: Vec<i16> = vec![0; len / 2];
        self.buf[0..len]
            .as_ref()
            .read_i16_into::<BigEndian>(&mut rv)?;
        Ok(rv)
    }
    /// Read `len/4` i32s from `len` bytes
    fn read_i32(&mut self, len: u16) -> Result<Vec<i32>, std::io::Error> {
        let len: usize = len.into();
        self.file.read_exact(&mut self.buf[0..len])?;
        let mut rv: Vec<i32> = vec![0; len / 4];
        self.buf[0..len]
            .as_ref()
            .read_i32_into::<BigEndian>(&mut rv)?;
        Ok(rv)
    }
    /// Read `len/8` f64s from `len` bytes, decoding GDS's float-format along the way
    fn read_f64(&mut self, len: u16) -> GdsResult<Vec<f64>> {
        let len: usize = len.into();
        // This is more fun, as it requires first grabbing "gds floats",
        // which we capture as eight-byte Vec<u8>, and then convert to IEEE-standard floats.
        let mut data = Vec::<f64>::with_capacity(len / 8);
        for _ in 0..(len / 8) {
            let bytes = self.read_bytes(8)?;
            data.push(GdsFloat64::decode(&bytes)?);
        }
        Ok(data)
    }
}
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
/// Trait for "layered" elements
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
    /// Record in an invalid context
    RecordContext(GdsRecord, GdsContext),
    /// Invalid record length
    RecordLen(usize),
    /// Invalid data type
    InvalidDataType(u8),
    /// Invalid record type
    InvalidRecordType(u8),
    /// Unsupported feature, in the decoded context
    Unsupported(Option<GdsRecord>, Option<GdsContext>),
    /// File opening, reading, and writing
    FileIO(String),
    /// Other decoding errors
    Decode,
    /// Other encoding errors
    Encode,
    /// Boxed (External) Errors
    Boxed(Box<dyn Error>),
    /// Other errors
    Str(String),
}
impl std::fmt::Display for GdsError {
    /// Display a [GdsError].
    /// This functionally delegates to the (derived) [std::fmt::Debug] implementation.
    /// Maybe more info that wanted in some cases. But certainly enough.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

#[cfg(any(test, feature = "selftest"))]
/// Check `lib` matches across a write-read round-trip cycle
pub fn roundtrip(lib: &GdsLibrary) -> GdsResult<()> {
    use tempfile::tempfile;
    // Write to a temporary file
    let mut file = tempfile()?;
    lib.encode(&mut file)?;
    // Rewind to the file-start, and read it back
    file.seek(SeekFrom::Start(0))?;
    let mmap = unsafe { Mmap::map(&file)? };
    let rdr = GdsReader::new(Cursor::new(mmap));
    let mut parser = GdsParser::new(rdr)?;
    let lib2 = parser.parse_lib()?;
    // And check the two line up
    assert_eq!(*lib, lib2);
    Ok(())
}

#[cfg(test)]
mod tests;
