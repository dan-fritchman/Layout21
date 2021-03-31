//!
//! # Gds21
//!
//! GDSII Integrated Circuit Layout Format Parser and Writer
//!
//!
//!

use std::fmt;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::io::{Read, Write};
use std::mem;
use std::str;

#[allow(unused_imports)]
use std::io::prelude::*;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use chrono::prelude::*;
use chrono::{Datelike, NaiveDate, NaiveDateTime};
use enum_dispatch::enum_dispatch;
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
#[derive(FromPrimitive, Debug, Clone, Copy)]
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
#[derive(Debug, Clone, PartialEq)]
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
    /// Decode the next binary-encoded `GdsRecord` from open `Read`-object `file`.
    /// Returns a `GdsError` if `file` cursor is not on a record-boundary,
    /// or if binary decoding otherwise fails.
    pub fn decode(file: &mut impl Read) -> Result<GdsRecord, GdsError> {
        // Read the 16-bit record-size. (In bytes, including the four header bytes.)
        let len = match file.read_u16::<BigEndian>() {
            Err(ref e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                return Err(GdsError::Decode); // Unexpected end-of-file without `EndLib`
            }
            Err(_) => return Err(GdsError::Decode), // Some other kinda error; raise it.
            Ok(num) if num < 4 => return Err(GdsError::RecordLen(num)), // Invalid (too short) length; throw Error.
            Ok(num) if num % 2 != 0 => return Err(GdsError::RecordLen(num)), // Invalid (odd) length; throw Error.
            Ok(num) => num,                                                  // The normal case
        };
        let len = len - 4; // Strip out the four header-bytes
                           // Read and decode its RecordType
        let record_type = file.read_u8()?;
        let record_type: GdsRecordType =
            FromPrimitive::from_u8(record_type).ok_or(GdsError::InvalidRecordType(record_type))?;
        if !record_type.valid() {
            return Err(GdsError::UnsupportedRecordType(record_type));
        }
        // Read and decode its DataType
        let data_type = file.read_u8()?;
        let data_type =
            FromPrimitive::from_u8(data_type).ok_or(GdsError::InvalidDataType(data_type))?;

        // Based on that header-data, decode to a `GdsRecord`
        use GdsDataType::{BitArray, NoData, Str, F64, I16, I32};
        let record: GdsRecord = match (record_type, data_type, len) {
            // Library-Level Records
            (GdsRecordType::Header, I16, 2) => GdsRecord::Header {
                version: read_i16(file, len)?[0],
            },
            (GdsRecordType::BgnLib, I16, 24) => GdsRecord::BgnLib {
                dates: read_i16(file, len)?,
            },
            (GdsRecordType::LibName, Str, _) => GdsRecord::LibName(read_str(file, len)?),
            (GdsRecordType::Units, F64, 16) => {
                let v = read_f64(file, len)?;
                GdsRecord::Units(v[0], v[1])
            }
            (GdsRecordType::EndLib, NoData, 0) => GdsRecord::EndLib,

            // Structure (Cell) Level Records
            (GdsRecordType::BgnStruct, I16, 24) => GdsRecord::BgnStruct {
                dates: read_i16(file, len)?,
            },
            (GdsRecordType::StructName, Str, _) => GdsRecord::StructName(read_str(file, len)?),
            (GdsRecordType::StructRefName, Str, _) => {
                GdsRecord::StructRefName(read_str(file, len)?)
            }
            (GdsRecordType::EndStruct, NoData, 0) => GdsRecord::EndStruct,

            // Element-Level Records
            (GdsRecordType::Boundary, NoData, 0) => GdsRecord::Boundary,
            (GdsRecordType::Path, NoData, 0) => GdsRecord::Path,
            (GdsRecordType::StructRef, NoData, 0) => GdsRecord::StructRef,
            (GdsRecordType::ArrayRef, NoData, 0) => GdsRecord::ArrayRef,
            (GdsRecordType::Text, NoData, 0) => GdsRecord::Text,
            (GdsRecordType::Layer, I16, 2) => GdsRecord::Layer(read_i16(file, len)?[0]),
            (GdsRecordType::DataType, I16, 2) => GdsRecord::DataType(read_i16(file, len)?[0]),
            (GdsRecordType::Width, I32, 4) => GdsRecord::Width(read_i32(file, len)?[0]),
            (GdsRecordType::Xy, I32, _) => GdsRecord::Xy(read_i32(file, len)?),
            (GdsRecordType::EndElement, NoData, 0) => GdsRecord::EndElement,

            // More (less well-categorized here) record-types
            (GdsRecordType::ColRow, I16, 4) => {
                let d = read_i16(file, len)?;
                GdsRecord::ColRow {
                    cols: d[0],
                    rows: d[1],
                }
            }
            (GdsRecordType::Node, NoData, 0) => GdsRecord::Node,
            (GdsRecordType::TextType, I16, 2) => GdsRecord::TextType(read_i16(file, len)?[0]),
            (GdsRecordType::Presentation, BitArray, 2) => {
                let bytes = read_bytes(file, len)?;
                GdsRecord::Presentation(bytes[0], bytes[1])
            }
            (GdsRecordType::String, Str, _) => GdsRecord::String(read_str(file, len)?),
            (GdsRecordType::Strans, BitArray, 2) => {
                let bytes = read_bytes(file, len)?;
                GdsRecord::Strans(bytes[0], bytes[1])
            }
            (GdsRecordType::Mag, F64, 8) => GdsRecord::Mag(read_f64(file, len)?[0]),
            (GdsRecordType::Angle, F64, 8) => GdsRecord::Angle(read_f64(file, len)?[0]),
            (GdsRecordType::RefLibs, Str, _) => GdsRecord::RefLibs(read_str(file, len)?),
            (GdsRecordType::Fonts, Str, _) => GdsRecord::Fonts(read_str(file, len)?),
            (GdsRecordType::PathType, I16, 2) => GdsRecord::PathType(read_i16(file, len)?[0]),
            (GdsRecordType::Generations, I16, 2) => GdsRecord::Generations(read_i16(file, len)?[0]),
            (GdsRecordType::AttrTable, Str, _) => GdsRecord::AttrTable(read_str(file, len)?),
            (GdsRecordType::ElemFlags, BitArray, 2) => {
                let bytes = read_bytes(file, len)?;
                GdsRecord::ElemFlags(bytes[0], bytes[1])
            }
            (GdsRecordType::Nodetype, I16, 2) => GdsRecord::Nodetype(read_i16(file, len)?[0]),
            (GdsRecordType::PropAttr, I16, 2) => GdsRecord::PropAttr(read_i16(file, len)?[0]),
            (GdsRecordType::PropValue, Str, _) => GdsRecord::PropValue(read_str(file, len)?),
            (GdsRecordType::Box, NoData, 0) => GdsRecord::Box,
            (GdsRecordType::BoxType, I16, 2) => GdsRecord::BoxType(read_i16(file, len)?[0]),
            (GdsRecordType::Plex, I32, 4) => GdsRecord::Plex(read_i32(file, len)?[0]),
            (GdsRecordType::BeginExtn, I32, 4) => GdsRecord::BeginExtn(read_i32(file, len)?[0]),
            (GdsRecordType::EndExtn, I32, 4) => GdsRecord::EndExtn(read_i32(file, len)?[0]),
            (GdsRecordType::TapeNum, I16, 2) => GdsRecord::TapeNum(read_i16(file, len)?[0]),
            (GdsRecordType::TapeCode, I16, 12) => GdsRecord::TapeCode(read_i16(file, len)?),
            (GdsRecordType::Format, I16, 2) => GdsRecord::Format(read_i16(file, len)?[0]),
            (GdsRecordType::Mask, Str, _) => GdsRecord::Mask(read_str(file, len)?),
            (GdsRecordType::EndMasks, NoData, 0) => GdsRecord::EndMasks,
            (GdsRecordType::LibDirSize, I16, 2) => GdsRecord::LibDirSize(read_i16(file, len)?[0]),
            (GdsRecordType::SrfName, Str, _) => GdsRecord::SrfName(read_str(file, len)?),
            (GdsRecordType::LibSecur, I16, 2) => GdsRecord::LibSecur(read_i16(file, len)?[0]),

            // Failing to meet any of these clauses means this is an invalid record
            _ => return Err(GdsError::RecordDecode(record_type, data_type, len)),
        };
        Ok(record)
    }
    /// Encode into bytes and write onto `writer`
    pub fn encode(&self, writer: &mut impl Write) -> Result<(), GdsError> {
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

        // Send those header-bytes to the writer
        writer.write_u16::<BigEndian>(len as u16 + 4)?; // Include the four header bytes in total-length
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
#[derive(FromPrimitive, Debug, Clone, Copy)]
pub enum GdsDataType {
    NoData = 0,
    BitArray = 1,
    I16 = 2,
    I32 = 3,
    F32 = 4,
    F64 = 5,
    Str = 6,
}
/// Read `len` bytes and convert to `String`
fn read_str(file: &mut impl Read, len: u16) -> Result<String, GdsError> {
    // ASCII Decode. First load into a bytes-vector.
    let mut data = read_bytes(file, len)?;
    // Strip optional end-of-string chars
    if data[data.len() - 1] == 0x00 {
        data.pop();
    }
    // And convert to string
    let s: String = std::str::from_utf8(&data)?.into();
    Ok(s)
}
/// Read `len` bytes
fn read_bytes(file: &mut impl Read, len: u16) -> Result<Vec<u8>, std::io::Error> {
    (0..len)
        .map(|_| file.read_u8())
        .into_iter()
        .collect::<Result<Vec<u8>, _>>()
}
/// Read `len/2` i16s from `len` bytes
fn read_i16(file: &mut impl Read, len: u16) -> Result<Vec<i16>, std::io::Error> {
    (0..len / 2)
        .map(|_| file.read_i16::<BigEndian>())
        .collect::<Result<Vec<i16>, _>>()
}
/// Read `len/4` i32s from `len` bytes
fn read_i32(file: &mut impl Read, len: u16) -> Result<Vec<i32>, std::io::Error> {
    (0..len / 4)
        .map(|_| file.read_i32::<BigEndian>())
        .collect::<Result<Vec<i32>, _>>()
}
/// Read `len/8` f64s from `len` bytes, decoding GDS's float-format along the way
fn read_f64(file: &mut impl Read, len: u16) -> Result<Vec<f64>, GdsError> {
    // This is more fun, as it requires first grabbing "gds floats",
    // which we capture as eight-byte Vec<u8>, and then convert to IEEE-standard floats.
    let mut data = Vec::<f64>::new();
    for _ in 0..(len / 8) {
        let bytes = read_bytes(file, 8)?;
        data.push(GdsFloat64::decode(&bytes)?);
    }
    Ok(data)
}
/// Incredibly, GDSII is old enough to have its own float-format,
/// like most computers did before IEEE754.
pub struct GdsFloat64 {}
impl GdsFloat64 {
    /// Decode eight GDSII-float-encoded bytes to `f64`
    pub fn decode(bytes: &[u8]) -> Result<f64, GdsError> {
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
    /// Encode `f64` to eight bytes, this time stored as `u64`.
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

/// A placeholder for Unsupported elements
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct UnImplemented {}

/// # Gds Translation Settings
/// For text-elements and references.
/// As configured by the STRANS records.
#[derive(Default, Clone, Debug, Deserialize, Serialize, PartialEq)]
pub struct GdsStrans {
    // Required Fields
    pub reflected: bool, // Reflection
    pub abs_mag: bool,   // Absolute Magnification Setting. Not supported
    pub abs_angle: bool, // Absolute Angle Setting. Not supported

    // Optional Fields
    pub mag: Option<f64>,   // Magnification Factor
    pub angle: Option<f64>, // Angle, in degrees counter-clockwise
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
    fn parse(it: &mut GdsReaderIter, d0: u8, d1: u8) -> Result<Self, GdsError> {
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
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsPresentation(u8, u8);

#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsElemFlags(u8, u8);

#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsPlex(i32);

/// # Gds Library Units
///
/// From the spec (not from us):
/// The first number is the size of a database-unit, in user-units.
/// The second is the size of a database-unit in meters.
/// To calculate the size of a user-unit in meters,
/// divide the second number by the first.
///
/// FIXME: the names of these fields probably indicate they aren't terribly intuitive.
/// Probably re-org them.
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsUnits {
    pub dbu: f64,
    pub uu: f64,
}

/// # Gds Mask-Format Enumeration
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub enum GdsFormatType {
    Archive,                      // Default, sole fully-supported case
    Filtered(Vec<UnImplemented>), // Filtered-format includes a list of Mask records. Not supported.
}
/// # Gds Property
/// Spec BNF:
/// PROPATTR PROPVALUE
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsProperty {
    pub attr: i16,
    pub value: String,
}
///
/// # Gds Path Element
///
/// Spec BNF:
/// PATH [ELFLAGS] [PLEX] LAYER DATATYPE [PATHTYPE] [WIDTH] XY [BGNEXTN] [ENDEXTN])
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsPath {
    // Required Fields
    pub layer: i16,    // Layer Number
    pub datatype: i16, // DataType ID
    pub xy: Vec<i32>,  // Vector of x,y coordinates

    // Optional Fields
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub width: Option<i32>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub path_type: Option<i16>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub begin_extn: Option<i32>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub end_extn: Option<i32>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
impl GdsPath {
    fn parse(it: &mut GdsReaderIter) -> Result<GdsPath, GdsError> {
        let mut b = GdsPathBuilder::default();

        while let Some(r) = it.next()? {
            if let GdsRecord::EndElement = r {
                break; // End-of-element
            }
            match r {
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::DataType(d) => b.datatype(d),
                GdsRecord::Xy(d) => b.xy(d),
                GdsRecord::Width(d) => b.width(d),
                GdsRecord::PathType(d) => b.path_type(d),
                GdsRecord::BeginExtn(d) => b.begin_extn(d),
                GdsRecord::EndExtn(d) => b.end_extn(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(_) | GdsRecord::PropValue(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::Path)))
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Path)),
            };
        }
        Ok(b.build()?)
    }
}
impl ToRecords for GdsPath {
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![
            GdsRecord::Path,
            GdsRecord::Layer(self.layer),
            GdsRecord::DataType(self.datatype),
            GdsRecord::Xy(self.xy.clone()),
        ];
        if let Some(ref e) = self.width {
            records.push(GdsRecord::Width(*e));
        }
        if let Some(ref e) = self.path_type {
            records.push(GdsRecord::PathType(*e));
        }
        if let Some(ref e) = self.begin_extn {
            records.push(GdsRecord::BeginExtn(*e));
        }
        if let Some(ref e) = self.end_extn {
            records.push(GdsRecord::EndExtn(*e));
        }
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
///
/// # Gds Boundary Element
///
/// Spec BNF:
/// BOUNDARY [ELFLAGS] [PLEX] LAYER DATATYPE XY
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsBoundary {
    // Required Fields
    pub layer: i16,    // Layer Number
    pub datatype: i16, // DataType ID
    pub xy: Vec<i32>,  // Vector of x,y coordinates

    // Optional Fields
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
impl GdsBoundary {
    /// Parse from record-iterator `it`
    fn parse(it: &mut GdsReaderIter) -> Result<GdsBoundary, GdsError> {
        let mut b = GdsBoundaryBuilder::default();

        while let Some(r) = it.next()? {
            if let GdsRecord::EndElement = r {
                break; // End-of-element
            }
            match r {
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::DataType(d) => b.datatype(d),
                GdsRecord::Xy(d) => b.xy(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(_) | GdsRecord::PropValue(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::Boundary)))
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Boundary)),
            };
        }
        Ok(b.build()?)
    }
}
impl ToRecords for GdsBoundary {
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![
            GdsRecord::Boundary,
            GdsRecord::Layer(self.layer),
            GdsRecord::DataType(self.datatype),
            GdsRecord::Xy(self.xy.clone()),
        ];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
///
/// # Gds Struct Reference
///
/// Spec BNF:
/// SREF [ELFLAGS] [PLEX] SNAME [<strans>] XY
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsStructRef {
    // Required Fields
    pub name: String, // Struct (Cell) Name
    pub xy: Vec<i32>, // Vector of x,y coordinates

    // Optional Fields
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub strans: Option<GdsStrans>, // Translation & Reflection Options
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
impl GdsStructRef {
    fn parse(it: &mut GdsReaderIter) -> Result<GdsStructRef, GdsError> {
        let mut b = GdsStructRefBuilder::default();

        while let Some(r) = it.next()? {
            if let GdsRecord::EndElement = r {
                break; // End-of-element
            }
            match r {
                GdsRecord::StructRefName(d) => b.name(d),
                GdsRecord::Xy(d) => b.xy(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::Strans(d0, d1) => b.strans(GdsStrans::parse(it, d0, d1)?),
                GdsRecord::PropAttr(_) | GdsRecord::PropValue(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::StructRef)))
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::StructRef)),
            };
        }
        let b = b.build()?;
        Ok(b)
    }
}
impl ToRecords for GdsStructRef {
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![
            GdsRecord::StructRef,
            GdsRecord::StructRefName(self.name.clone()),
            GdsRecord::Xy(self.xy.clone()),
        ];
        if let Some(ref e) = self.strans {
            let rs = e.to_records();
            records.extend(rs);
        }
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
///
/// # Gds Array Reference
///
/// Spec BNF:
/// AREF [ELFLAGS] [PLEX] SNAME [<strans>] COLROW XY
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsArrayRef {
    // Required Fields
    pub name: String, // Struct (Cell) Name
    pub xy: Vec<i32>, // Vector of x,y coordinates
    pub cols: i16,    // Number of columns
    pub rows: i16,    // Number of rows
    // Optional Fields
    #[serde(default)]
    #[builder(default)]
    pub strans: Option<GdsStrans>, // Translation & Reflection Options
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
impl GdsArrayRef {
    fn parse(it: &mut GdsReaderIter) -> Result<GdsArrayRef, GdsError> {
        let mut b = GdsArrayRefBuilder::default();

        while let Some(r) = it.next()? {
            if let GdsRecord::EndElement = r {
                break; // End-of-element
            }
            match r {
                GdsRecord::StructRefName(d) => b.name(d),
                GdsRecord::ColRow { rows, cols } => {
                    b.rows(rows);
                    b.cols(cols)
                }
                GdsRecord::Xy(d) => b.xy(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::Strans(d0, d1) => b.strans(GdsStrans::parse(it, d0, d1)?),
                GdsRecord::PropAttr(_) | GdsRecord::PropValue(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::ArrayRef)))
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::ArrayRef)),
            };
        }
        let b = b.build()?;
        Ok(b)
    }
}
impl ToRecords for GdsArrayRef {
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![
            GdsRecord::ArrayRef,
            GdsRecord::StructRefName(self.name.clone()),
            GdsRecord::Xy(self.xy.clone()),
            GdsRecord::ColRow {
                cols: self.cols,
                rows: self.rows,
            },
        ];
        if let Some(ref e) = self.strans {
            let rs = e.to_records();
            records.extend(rs);
        }
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
///
/// # Gds Text Element
///
/// Spec BNF:
/// TEXT [ELFLAGS] [PLEX] LAYER
/// TEXTTYPE [PRESENTATION] [PATHTYPE] [WIDTH] [<strans>] XY STRING
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsTextElem {
    // Required Fields
    pub string: String, // Text Value
    pub layer: i16,     // Layer Number
    pub texttype: i16,  // Text-Type ID
    pub xy: Vec<i32>,   // Vector of x,y coordinates

    // Optional Fields
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub presentation: Option<GdsPresentation>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub path_type: Option<i16>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub width: Option<i32>,
    #[serde(default)]
    #[builder(default)]
    pub strans: Option<GdsStrans>, // Translation & Reflection Options
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
impl GdsTextElem {
    /// Parse a `GdsTextElement` from an iterator of `GdsRecords`
    /// Assumes the initial `Text` record has already been parsed.
    fn parse(it: &mut GdsReaderIter) -> Result<GdsTextElem, GdsError> {
        let mut b = GdsTextElemBuilder::default();

        while let Some(r) = it.next()? {
            if let GdsRecord::EndElement = r {
                break; // End-of-element
            }
            match r {
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::TextType(d) => b.texttype(d),
                GdsRecord::Xy(d) => b.xy(d),
                GdsRecord::String(d) => b.string(d),
                GdsRecord::Presentation(d0, d1) => b.presentation(GdsPresentation(d0, d1)),
                GdsRecord::PathType(d) => b.path_type(d),
                GdsRecord::Width(d) => b.width(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::Strans(d0, d1) => b.strans(GdsStrans::parse(it, d0, d1)?),
                GdsRecord::PropAttr(_) | GdsRecord::PropValue(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::Boundary)))
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Text)),
            };
        }
        Ok(b.build()?)
    }
}
impl ToRecords for GdsTextElem {
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![
            GdsRecord::Text,
            GdsRecord::Layer(self.layer),
            GdsRecord::TextType(self.texttype),
            GdsRecord::String(self.string.clone()),
            GdsRecord::Xy(self.xy.clone()),
        ];
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
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
///
/// # Gds Node Element
///
/// Spec BNF:
/// NODE [ELFLAGS] [PLEX] LAYER NODETYPE XY
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsNode {
    // Required Fields
    pub layer: i16,    // Layer Number
    pub nodetype: i16, // Node-Type ID
    pub xy: Vec<i32>,  // Vector of x,y coordinates

    // Optional Fields
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
///
/// # Gds Box Element
///
/// Spec BNF:
/// BOX [ELFLAGS] [PLEX] LAYER BOXTYPE XY
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsBox {
    // Required Fields
    pub layer: i16,   // Layer Number
    pub boxtype: i16, // Box-Type ID
    pub xy: Vec<i32>, // Vector of x,y coordinates

    // Optional Fields
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
impl GdsBox {
    fn parse(it: &mut GdsReaderIter) -> Result<GdsBox, GdsError> {
        let mut b = GdsBoxBuilder::default();

        while let Some(r) = it.next()? {
            if let GdsRecord::EndElement = r {
                break; // End-of-element
            }
            match r {
                GdsRecord::Layer(d) => b.layer(d),
                GdsRecord::BoxType(d) => b.boxtype(d),
                GdsRecord::Xy(d) => b.xy(d),
                GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
                GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
                GdsRecord::PropAttr(_) | GdsRecord::PropValue(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::Box)))
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Box)),
            };
        }
        Ok(b.build()?)
    }
}
impl ToRecords for GdsBox {
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![
            GdsRecord::Box,
            GdsRecord::Layer(self.layer),
            GdsRecord::BoxType(self.boxtype),
            GdsRecord::Xy(self.xy.clone()),
        ];
        if let Some(ref e) = self.elflags {
            records.push(GdsRecord::ElemFlags(e.0, e.1));
        }
        if let Some(ref e) = self.plex {
            records.push(GdsRecord::Plex(e.0));
        }
        records.push(GdsRecord::EndElement);
        records
    }
}
impl ToRecords for GdsNode {
    fn to_records(&self) -> Vec<GdsRecord> {
        unimplemented!("")
    }
}
///
/// # Gds Element Enumeration  
///
/// Spec BNF:
/// {<boundary> | <path> | <SREF> | <AREF> | <text> | <node> | <box>} {<property>}* ENDEL
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
/// Trait for conversion of `Elements` to sequences of `Records`.
/// Dispatched from the `GdsElement` enum by the `enum_dispatch` macros.
#[enum_dispatch(GdsElement)]
pub trait ToRecords {
    fn to_records(&self) -> Vec<GdsRecord>;
}

/// # Gds Modification Dates & Times
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
pub struct GdsDateTimes {
    /// Last Modification Date & Time
    modified: NaiveDateTime,
    /// Last Access Date & Time
    accessed: NaiveDateTime,
}
impl GdsDateTimes {
    /// Parse from GDSII's vector of i16's format
    fn parse(d: Vec<i16>) -> Result<Self, GdsError> {
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

///
/// # Gds Struct
/// (Usually this means a Cell)
///
/// Spec BNF:
/// BGNSTR STRNAME [STRCLASS] {<element>}* ENDSTR
///
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsStruct {
    /// Struct Name
    pub name: String,
    /// Creation/ Modification-Date Info
    pub dates: GdsDateTimes,
    /// Elements List
    pub elems: Vec<GdsElement>,
}
impl GdsStruct {
    /// Parse from record-iterator `it`
    fn parse(it: &mut GdsReaderIter, dates: Vec<i16>) -> Result<GdsStruct, GdsError> {
        let mut strukt = GdsStructBuilder::default();
        let mut elems = Vec::<GdsElement>::new();

        strukt.dates(GdsDateTimes::parse(dates)?);
        while let Some(r) = it.next()? {
            if let GdsRecord::EndStruct = r {
                break; // End-of-struct
            }
            match r {
                GdsRecord::StructName(d) => {
                    strukt.name(d);
                }
                GdsRecord::Boundary => {
                    let b = GdsBoundary::parse(it)?;
                    elems.push(GdsElement::GdsBoundary(b));
                }
                GdsRecord::Text => {
                    let b = GdsTextElem::parse(it)?;
                    elems.push(GdsElement::GdsTextElem(b));
                }
                GdsRecord::Path => {
                    let b = GdsPath::parse(it)?;
                    elems.push(GdsElement::GdsPath(b));
                }
                GdsRecord::Box => {
                    let b = GdsBox::parse(it)?;
                    elems.push(GdsElement::GdsBox(b));
                }
                GdsRecord::StructRef => {
                    let b = GdsStructRef::parse(it)?;
                    elems.push(GdsElement::GdsStructRef(b));
                }
                GdsRecord::ArrayRef => {
                    let b = GdsArrayRef::parse(it)?;
                    elems.push(GdsElement::GdsArrayRef(b));
                }
                // Spec-valid but unsupported records
                GdsRecord::Node | GdsRecord::PropAttr(_) => {
                    return Err(GdsError::Unsupported(Some(r), Some(GdsContext::Struct)))
                }
                // Invalid
                _ => return Err(GdsError::RecordContext(r, GdsContext::Struct)),
            };
        }
        strukt.elems(elems);
        Ok(strukt.build()?)
    }
    /// Encode and write to `writer` 
    pub fn encode(&self, writer: &mut impl Write) -> Result<(), GdsError> {
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
/// Spec BNF:
/// HEADER BGNLIB [LIBDIRSIZE] [SRFNAME] [LIBSECUR] LIBNAME [REFLIBS] [FONTS] [ATTRTABLE] [GENERATIONS] [<FormatType>]
/// UNITS {<structure>}* ENDLIB
///
#[derive(Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
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
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub libdirsize: Option<UnImplemented>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub srfname: Option<UnImplemented>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub libsecur: Option<UnImplemented>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub reflibs: Option<UnImplemented>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub fonts: Option<UnImplemented>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub attrtable: Option<UnImplemented>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub generations: Option<UnImplemented>,
    #[serde(default)]
    #[builder(default, setter(strip_option))]
    pub format_type: Option<GdsFormatType>,
}
impl GdsLibrary {
    /// Read a GDS loaded from file at path `file_name`
    pub fn load(file_name: &str) -> Result<GdsLibrary, GdsError> {
        // Create an iterator over records
        let mut it = GdsReaderIter::open(file_name)?;
        // And parse it to a library-tree
        let lib = GdsLibrary::parse(&mut it)?;
        Ok(lib)
    }
    /// Parse a GdsLibrary from record-iterator `it`
    fn parse(it: &mut GdsReaderIter) -> Result<GdsLibrary, GdsError> {
        let mut lib = GdsLibraryBuilder::default();
        let mut structs = Vec::<GdsStruct>::new();
        // Read the Header and its version data
        match it.next()? {
            Some(GdsRecord::Header { version: v }) => lib.version(v),
            _ => return Err(GdsError::Decode),
        };
        // Read the begin-lib
        match it.next()? {
            Some(GdsRecord::BgnLib { dates: d }) => lib.dates(GdsDateTimes::parse(d)?),
            _ => return Err(GdsError::Decode),
        };
        // Iterate over all others
        while let Some(r) = it.next()? {
            if let GdsRecord::EndLib = r {
                break; // End-of-library
            }
            match r {
                GdsRecord::LibName(d) => {
                    lib.name(d);
                }
                GdsRecord::Units(d0, d1) => {
                    lib.units(GdsUnits { dbu: d0, uu: d1 });
                }
                GdsRecord::BgnStruct { dates } => {
                    let strukt = GdsStruct::parse(it, dates)?;
                    structs.push(strukt);
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
        lib.structs(structs);
        Ok(lib.build()?)
    }
    /// Save to file `fname`
    pub fn save(&self, fname: &str) -> Result<(), GdsError> {
        let mut file = BufWriter::new(File::create(fname)?);
        self.encode(&mut file)?;
        file.flush()?;
        Ok(())
    }
    /// Encode and write in binary form to `writer`
    pub fn encode(&self, writer: &mut impl Write) -> Result<(), GdsError> {
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
        GdsRecord::Units(self.units.dbu, self.units.uu).encode(writer)?;
        // Write all of our Structs/Cells
        for strukt in self.structs.iter() {
            strukt.encode(writer)?;
        }
        // And finally, the library terminator
        GdsRecord::EndLib.encode(writer)?;
        Ok(())
    }
}
/// # GdsReaderIter
///
/// A peekable iterator which loads GdsRecords from file, one at a time
struct GdsReaderIter {
    /// File being read
    rdr: BufReader<File>,
    /// Next record, stored for peeking
    nxt: Option<GdsRecord>,
}
impl GdsReaderIter {
    /// Create a new GdsReader iterator for the file at path `fname`
    fn open(fname: &str) -> Result<GdsReaderIter, GdsError> {
        Self::new(BufReader::new(File::open(&fname)?))
    }
    /// Create a new GdsReader iterator
    fn new(mut rdr: BufReader<File>) -> Result<GdsReaderIter, GdsError> {
        // Read our first record
        let nxt = Some(GdsRecord::decode(&mut rdr)?);
        Ok(GdsReaderIter { rdr, nxt })
    }
    /// Advance our iterator and return the next element
    fn next(&mut self) -> Result<Option<GdsRecord>, GdsError> {
        if self.nxt.is_none() || self.nxt == Some(GdsRecord::EndLib) {
            return Ok(None);
        }
        // Decode a new Record and swap it with our `nxt`
        let mut rv = Some(GdsRecord::decode(&mut self.rdr)?);
        mem::swap(&mut rv, &mut self.nxt);
        Ok(rv)
    }
    /// Peek at our next record, without advancing
    fn peek(&self) -> &Option<GdsRecord> {
        &self.nxt
    }
}
/// # GdsLayerSpec
/// Each GDSII element's layer is specified by a set of two numbers,
/// commonly referred to as `layer` and `datatype`.
/// Several element-types refer to their analog of `datatype` by different names,
/// e.g. `texttype` and `nodetype`.
/// `GdsLayerSpecs` generalize across these via the `xtype` field,
/// which holds whichever is appropriate for the given element.
pub struct GdsLayerSpec {
    pub layer: i16, // Layer ID Number
    pub xtype: i16, // DataType (or TextType, NodeType etc) ID Number
}

/// Enumeration of each context in which a record can be parsed,
/// generally for error reporting
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
///
/// # Gds Error Enumeration
///
#[derive(Clone, Debug)]
pub enum GdsError {
    RecordDecode(GdsRecordType, GdsDataType, u16), // Invalid binary -> record conversion
    RecordContext(GdsRecord, GdsContext),          // Record in an invalid context
    RecordLen(u16),                                // Invalid record length
    InvalidDataType(u8),                           // Invalid record type
    InvalidRecordType(u8),                         // Invalid record type
    UnsupportedRecordType(GdsRecordType),          // Unsupported (but spec'ed) record type
    Unsupported(Option<GdsRecord>, Option<GdsContext>), // Unsupported feature, in the decoded context
    Decode,                                             // Other decoding errors
    Encode,                                             // Other encoding errors
}
impl std::fmt::Display for GdsError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "GDS_ERROR") // FIXME: this never seems to happen where we expect
    }
}
impl std::error::Error for GdsError {}
impl From<std::io::Error> for GdsError {
    fn from(_e: std::io::Error) -> Self {
        GdsError::Decode
    }
}
impl From<std::str::Utf8Error> for GdsError {
    fn from(_e: std::str::Utf8Error) -> Self {
        GdsError::Decode
    }
}
impl From<String> for GdsError {
    fn from(_e: String) -> Self {
        GdsError::Decode
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Specified creation date for test cases
    fn test_dates() -> GdsDateTimes {
        let test_date = NaiveDate::from_ymd(1970, 1, 1).and_hms(0, 0, 1);
        GdsDateTimes {
            modified: test_date.clone(),
            accessed: test_date.clone(),
        }
    }
    #[test]
    fn it_reads() -> Result<(), GdsError> {
        // Read a sample GDS and compare to golden data
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        let lib = GdsLibrary::load(&fname)?;
        check(&lib, &resource("sample1.json"));
        Ok(())
    }
    #[test]
    fn it_round_trips() -> Result<(), GdsError> {
        // Read a sample
        let lib = GdsLibrary::load(&resource("sample1.gds"))?;
        // And check it round-trips to file
        roundtrip(&lib)?;
        Ok(())
    }
    #[test]
    fn it_instantiates() -> Result<(), GdsError> {
        // Read a sample, add a cell which instantiates it
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        let mut lib = GdsLibrary::load(&fname)?;
        lib.name = "has_inst_lib".into();
        let s = GdsStruct {
            name: "has_inst".into(),
            dates: test_dates(),
            elems: vec![GdsElement::GdsStructRef(GdsStructRef {
                name: "dff1".into(),
                xy: vec![11_000, 11_000],
                strans: None,
                elflags: None,
                plex: None,
            })],
        };
        lib.structs.push(s);
        // Check it against golden data
        check(&lib, &resource("sample1_inst.json"));
        // And check it round-trips to file
        roundtrip(&lib)?;
        Ok(())
    }
    #[test]
    fn it_arrays() -> Result<(), GdsError> {
        // Read a sample, add a cell which arrays it
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        let mut lib = GdsLibrary::load(&fname)?;
        lib.name = "has_array_lib".into();
        let s = GdsStruct {
            name: "has_array".into(),
            dates: test_dates(),
            elems: vec![GdsElement::GdsArrayRef(GdsArrayRef {
                name: "dff1".into(),
                xy: vec![0, 0, 0, 10_000_000, 10_000_000, 0],
                cols: 100,
                rows: 100,
                strans: None,
                elflags: None,
                plex: None,
            })],
        };
        lib.structs.push(s);
        // Check it against golden data
        check(&lib, &resource("sample1_array.json"));
        // And check it round-trips to file
        roundtrip(&lib)?;
        Ok(())
    }
    /// Compare `lib` to "golden" data loaded from JSON at path `golden`.
    fn check(lib: &GdsLibrary, fname: &str) {
        // Uncomment this bit to over-write the golden data
        save_json(lib, fname);

        let golden = load_json(fname);
        assert_eq!(*lib, golden);
    }
    /// Grab the full path of resource-file `fname`
    fn resource(fname: &str) -> String {
        format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname)
    }
    /// Load a library from JSON resource at path `fname`
    fn load_json(fname: &str) -> GdsLibrary {
        let file = File::open(&fname).unwrap();
        let golden: GdsLibrary = serde_json::from_reader(BufReader::new(file)).unwrap();
        golden
    }
    /// Save a `GdsLibrary` as a JSON-format file at path `fname`
    #[allow(dead_code)]
    fn save_json(lib: &GdsLibrary, fname: &str) {
        let mut file = BufWriter::new(File::create(fname).unwrap());
        let s = serde_json::to_string(lib).unwrap();
        file.write_all(s.as_bytes()).unwrap();
        file.flush().unwrap();
    }
}
#[cfg(any(test, feature = "selftest"))]
/// Check `lib` matches across a write-read round-trip cycle
pub fn roundtrip(lib: &GdsLibrary) -> Result<(), GdsError> {
    use std::io::SeekFrom;
    use tempfile::tempfile;
    // Write to a temporary file
    let mut file = tempfile()?;
    lib.encode(&mut file)?;
    // Rewind to the file-start, and read it back
    file.seek(SeekFrom::Start(0))?;
    let mut it = GdsReaderIter::new(BufReader::new(file))?;
    let lib2 = GdsLibrary::parse(&mut it)?;
    // And check the two line up
    assert_eq!(*lib, lib2);
    Ok(())
}
