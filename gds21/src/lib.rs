use std::fmt;
use std::fs::File;
use std::io::BufWriter;
use std::io::Write;
use std::str;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use enum_dispatch::enum_dispatch;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use serde::{Deserialize, Serialize};

#[macro_use]
extern crate derive_builder;

///
/// # Gds Record Types
///
/// In the numeric-order specified by GDSII, for automatic `FromPrimitive` conversions.
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
/// Unsupported record-types are not included.
///
#[derive(Debug, Clone, PartialEq)]
pub enum GdsRecord {
    Header { version: i16 },
    BgnLib { date_info: Vec<i16> },
    LibName(String),
    Units(f64, f64),
    EndLib,
    BgnStruct { date_info: Vec<i16> },
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
    /// Decode the next binary-encoded `GdsRecord` from open `File`-object `file`.
    /// Returns a `GdsError` if `file` cursor is not on a record-boundary,
    /// or if binary decoding otherwise fails.
    pub fn decode(file: &mut File) -> Result<GdsRecord, GdsError> {
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
                date_info: read_i16(file, len)?,
            },
            (GdsRecordType::LibName, Str, _) => GdsRecord::LibName(read_str(file, len)?),
            (GdsRecordType::Units, F64, 16) => {
                let v = read_f64(file, len)?;
                GdsRecord::Units(v[0], v[1])
            }
            (GdsRecordType::EndLib, NoData, 0) => GdsRecord::EndLib,

            // Structure (Cell) Level Records
            (GdsRecordType::BgnStruct, I16, 24) => GdsRecord::BgnStruct {
                date_info: read_i16(file, len)?,
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
    pub fn encode(&self, writer: &mut impl Write) -> Result<(), GdsError> {
        // This is split in two parts - header and data -
        // largely ease of handling the variety of datatypes

        // Quick closure for "even-only allowed" string-lengths
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
                writer.write_u64::<BigEndian>(normal_peoples_float_to_gds_float(*d))?
            }
            // "Structs"
            GdsRecord::Units(d0, d1) => {
                writer.write_u64::<BigEndian>(normal_peoples_float_to_gds_float(*d0))?;
                writer.write_u64::<BigEndian>(normal_peoples_float_to_gds_float(*d1))?;
            }
            GdsRecord::ColRow { cols, rows } => {
                writer.write_i16::<BigEndian>(*cols)?;
                writer.write_i16::<BigEndian>(*rows)?;
            }
            // Vectors
            GdsRecord::TapeCode(d)
            | GdsRecord::BgnLib { date_info: d }
            | GdsRecord::BgnStruct { date_info: d } => {
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

fn read_str(file: &mut File, len: u16) -> Result<String, GdsError> {
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

fn read_bytes(file: &mut File, len: u16) -> Result<Vec<u8>, std::io::Error> {
    (0..len)
        .map(|_| file.read_u8())
        .into_iter()
        .collect::<Result<Vec<u8>, _>>()
}

fn read_i16(file: &mut File, len: u16) -> Result<Vec<i16>, std::io::Error> {
    (0..len / 2)
        .map(|_| file.read_i16::<BigEndian>())
        .collect::<Result<Vec<i16>, _>>()
}

fn read_i32(file: &mut File, len: u16) -> Result<Vec<i32>, std::io::Error> {
    (0..len / 4)
        .map(|_| file.read_i32::<BigEndian>())
        .collect::<Result<Vec<i32>, _>>()
}
fn read_f64(file: &mut File, len: u16) -> Result<Vec<f64>, GdsError> {
    // This is more fun, as it requires first grabbing "gds floats",
    // which we capture as eight-byte Vec<u8>, and then convert to IEEE-standard floats.
    let mut data = Vec::<f64>::new();
    for _ in 0..(len / 8) {
        let bytes = read_bytes(file, 8)?;
        data.push(gds_float_to_normal_peoples_float(&bytes)?);
    }
    Ok(data)
}
/// Read a GDS loaded from file at path `file_name`
pub fn read_gds(file_name: &str) -> Result<GdsLibrary, GdsError> {
    // Open our file, read its header
    let mut file = File::open(&file_name)?;
    let mut records = Vec::<GdsRecord>::new();

    loop {
        let record = GdsRecord::decode(&mut file)?;
        if record == GdsRecord::EndLib {
            // End of library. Any content to follow is ignored
            records.push(record); // Still include the `EndLib` record
            break;
        }
        records.push(record);
    }
    // Create an iterator over records, and parse it to a library-tree
    let mut it = records.into_iter();
    let lib = parse_library(&mut it)?;
    // Check that end-of-library is the end-of-stream
    if it.next().is_some() {
        return Err(GdsError::Decode);
    }
    Ok(lib)
}

/// Incredibly, these things are old enough to have their own float-format,
/// like most computers did before IEEE754
pub fn gds_float_to_normal_peoples_float(bytes: &[u8]) -> Result<f64, GdsError> {
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
pub fn normal_peoples_float_to_gds_float(mut val: f64) -> u64 {
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

/// A placeholder while building up structural elements,
/// while not having everyting underneath
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct Tbd {}

/// # Gds Translation Settings
/// For text-elements and references.
/// As configured by the STRANS records.
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsStrans(u8, u8);

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
/// From the spec (not us):
/// The first number is the size of a database-unit, in user-units.
/// The second is the size of a database-unit in meters.
/// To calculate the size of a user-unit in meters,
/// divide the second number by the first.
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsUnits(f64, f64);

/// # Gds Mask-Format Enumeration
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub enum GdsFormatType {
    Archive,            // Default, sole fully-supported case
    Filtered(Vec<Tbd>), // Filtered-format includes a list of Mask records. Not supported.
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
    pub layer: i16,    // Layer ID
    pub datatype: i16, // DataType ID
    pub xy: Vec<i32>,  // Vector of x,y coordinates

    // Optional Fields
    #[builder(default, setter(strip_option))]
    pub width: Option<i32>,
    #[builder(default, setter(strip_option))]
    pub path_type: Option<i16>,
    #[builder(default, setter(strip_option))]
    pub begin_extn: Option<i32>,
    #[builder(default, setter(strip_option))]
    pub end_extn: Option<i32>,
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
pub fn parse_path(it: &mut impl Iterator<Item = GdsRecord>) -> Result<GdsPath, GdsError> {
    let mut b = GdsPathBuilder::default();

    while let Some(r) = it.next() {
        if let GdsRecord::EndElement = r {
            break; // End-of-element
        }
        let _ = match r {
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
    pub layer: i16,    // Layer ID
    pub datatype: i16, // DataType ID
    pub xy: Vec<i32>,  // Vector of x,y coordinates

    // Optional Fields
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
pub fn parse_boundary(it: &mut impl Iterator<Item = GdsRecord>) -> Result<GdsBoundary, GdsError> {
    let mut b = GdsBoundaryBuilder::default();

    while let Some(r) = it.next() {
        if let GdsRecord::EndElement = r {
            break; // End-of-element
        }
        let _ = match r {
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
    pub name: String, // Instance Name
    pub xy: Vec<i32>, // Vector of x,y coordinates

    // Optional Fields
    #[builder(default, setter(strip_option))]
    pub strans: Option<GdsStrans>,
    #[builder(default, setter(strip_option))]
    pub mag: Option<f64>, // Magnification
    #[builder(default, setter(strip_option))]
    pub angle: Option<f64>, // Angle
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
pub fn parse_struct_ref(
    it: &mut impl Iterator<Item = GdsRecord>,
) -> Result<GdsStructRef, GdsError> {
    let mut b = GdsStructRefBuilder::default();

    while let Some(r) = it.next() {
        if let GdsRecord::EndElement = r {
            break; // End-of-element
        }
        let _ = match r {
            GdsRecord::StructRefName(d) => b.name(d),
            GdsRecord::Xy(d) => b.xy(d),
            GdsRecord::Strans(d0, d1) => b.strans(GdsStrans(d0, d1)),
            GdsRecord::Mag(d) => b.mag(d),
            GdsRecord::Angle(d) => b.angle(d),
            GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
            GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
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
impl ToRecords for GdsStructRef {
    fn to_records(&self) -> Vec<GdsRecord> {
        let mut records = vec![
            GdsRecord::StructRef,
            GdsRecord::StructRefName(self.name.clone()),
            GdsRecord::Xy(self.xy.clone()),
        ];
        if let Some(ref e) = self.strans {
            records.push(GdsRecord::Strans(e.0, e.1));
        }
        if let Some(ref e) = self.mag {
            records.push(GdsRecord::Mag(*e));
        }
        if let Some(ref e) = self.angle {
            records.push(GdsRecord::Angle(*e));
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
    pub name: String, // Struct Name
    pub xy: Vec<i32>, // Vector of x,y coordinates
    pub cols: i16,    // Number of columns
    pub rows: i16,    // Number of rows
    // Optional Fields
    #[builder(default, setter(strip_option))]
    pub strans: Option<GdsStrans>,
    #[builder(default, setter(strip_option))]
    pub mag: Option<f64>, // Magnification
    #[builder(default, setter(strip_option))]
    pub angle: Option<f64>, // Angle
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
pub fn parse_array_ref(it: &mut impl Iterator<Item = GdsRecord>) -> Result<GdsArrayRef, GdsError> {
    let mut b = GdsArrayRefBuilder::default();

    while let Some(r) = it.next() {
        if let GdsRecord::EndElement = r {
            break; // End-of-element
        }
        let _ = match r {
            GdsRecord::StructRefName(d) => b.name(d),
            GdsRecord::ColRow { rows, cols } => {
                b.rows(rows);
                b.cols(cols)
            }
            GdsRecord::Xy(d) => b.xy(d),
            GdsRecord::Strans(d0, d1) => b.strans(GdsStrans(d0, d1)),
            GdsRecord::Mag(d) => b.mag(d),
            GdsRecord::Angle(d) => b.angle(d),
            GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
            GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
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
            records.push(GdsRecord::Strans(e.0, e.1));
        }
        if let Some(ref e) = self.mag {
            records.push(GdsRecord::Mag(*e));
        }
        if let Some(ref e) = self.angle {
            records.push(GdsRecord::Angle(*e));
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
    pub string: String,
    pub layer: i16,
    pub texttype: i16,
    pub xy: Vec<i32>,

    // Optional Fields
    #[builder(default, setter(strip_option))]
    pub presentation: Option<GdsPresentation>,
    #[builder(default, setter(strip_option))]
    pub path_type: Option<i16>,
    #[builder(default, setter(strip_option))]
    pub width: Option<i32>,
    #[builder(default, setter(strip_option))]
    pub strans: Option<GdsStrans>,
    #[builder(default, setter(strip_option))]
    pub mag: Option<f64>, // Magnification
    #[builder(default, setter(strip_option))]
    pub angle: Option<f64>, // Angle
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
/// Parse a `GdsTextElement` from an iterator of `GdsRecords`
/// Assumes the initial `Text` record has already been parsed.
pub fn parse_text_elem(it: &mut impl Iterator<Item = GdsRecord>) -> Result<GdsTextElem, GdsError> {
    let mut b = GdsTextElemBuilder::default();

    while let Some(r) = it.next() {
        if let GdsRecord::EndElement = r {
            break; // End-of-element
        }
        let _ = match r {
            GdsRecord::Layer(d) => b.layer(d),
            GdsRecord::TextType(d) => b.texttype(d),
            GdsRecord::Xy(d) => b.xy(d),
            GdsRecord::String(d) => b.string(d),
            GdsRecord::Presentation(d0, d1) => b.presentation(GdsPresentation(d0, d1)),
            GdsRecord::PathType(d) => b.path_type(d),
            GdsRecord::Width(d) => b.width(d),
            GdsRecord::Strans(d0, d1) => b.strans(GdsStrans(d0, d1)),
            GdsRecord::Mag(d) => b.mag(d),
            GdsRecord::Angle(d) => b.angle(d),
            GdsRecord::Plex(d) => b.plex(GdsPlex(d)),
            GdsRecord::ElemFlags(d0, d1) => b.elflags(GdsElemFlags(d0, d1)),
            GdsRecord::PropAttr(_) | GdsRecord::PropValue(_) => {
                return Err(GdsError::Unsupported(Some(r), Some(GdsContext::Boundary)))
            }
            // Invalid
            _ => return Err(GdsError::RecordContext(r, GdsContext::Text)),
        };
    }
    Ok(b.build()?)
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
            records.push(GdsRecord::Strans(e.0, e.1));
        }
        if let Some(ref e) = self.mag {
            records.push(GdsRecord::Mag(*e));
        }
        if let Some(ref e) = self.angle {
            records.push(GdsRecord::Angle(*e));
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
    pub layer: i16,    // Layer ID
    pub nodetype: i16, // Node-Type ID
    pub xy: Vec<i32>,  // Vector of x,y coordinates

    // Optional Fields
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
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
    pub layer: i16,   // Layer ID
    pub boxtype: i16, // Box-Type ID
    pub xy: Vec<i32>, // Vector of x,y coordinates

    // Optional Fields
    #[builder(default, setter(strip_option))]
    pub elflags: Option<GdsElemFlags>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<GdsPlex>,
}
pub fn parse_box(it: &mut impl Iterator<Item = GdsRecord>) -> Result<GdsBox, GdsError> {
    let mut b = GdsBoxBuilder::default();

    while let Some(r) = it.next() {
        if let GdsRecord::EndElement = r {
            break; // End-of-element
        }
        let _ = match r {
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
///
/// # Gds Struct
/// (Usually this means a Cell)
///
/// Spec BNF:
/// BGNSTR STRNAME [STRCLASS] {<element>}* ENDSTR
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsStruct {
    pub name: String,           // Struct Name
    pub date_info: Vec<i16>,    // Creation/ Modification-Date Info
    pub elems: Vec<GdsElement>, // Elements List
}
pub fn parse_struct(
    it: &mut impl Iterator<Item = GdsRecord>,
    date_info: Vec<i16>,
) -> Result<GdsStruct, GdsError> {
    let mut strukt = GdsStructBuilder::default();
    strukt.date_info(date_info);
    let mut elems = Vec::<GdsElement>::new();

    while let Some(r) = it.next() {
        if let GdsRecord::EndStruct = r {
            break; // End-of-struct
        }
        match r {
            GdsRecord::StructName(d) => {
                strukt.name(d);
            }
            GdsRecord::Boundary => {
                let b = parse_boundary(it)?;
                elems.push(GdsElement::GdsBoundary(b));
            }
            GdsRecord::Text => {
                let b = parse_text_elem(it)?;
                elems.push(GdsElement::GdsTextElem(b));
            }
            GdsRecord::Path => {
                let b = parse_path(it)?;
                elems.push(GdsElement::GdsPath(b));
            }
            GdsRecord::Box => {
                let b = parse_box(it)?;
                elems.push(GdsElement::GdsBox(b));
            }
            GdsRecord::StructRef => {
                let b = parse_struct_ref(it)?;
                elems.push(GdsElement::GdsStructRef(b));
            }
            GdsRecord::ArrayRef => {
                let b = parse_array_ref(it)?;
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
impl GdsStruct {
    pub fn encode(&self, writer: &mut impl Write) -> Result<(), GdsError> {
        GdsRecord::BgnStruct {
            date_info: self.date_info.clone(),
        }
        .encode(writer)?;
        GdsRecord::StructName(self.name.clone()).encode(writer)?;
        // Write each of its elements
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
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsLibrary {
    // Required fields
    pub name: String,            // Library Name
    pub version: i16,            // Gds Spec Version
    pub date_info: Vec<i16>,     // Creation Date, in 16b ints as per spec
    pub units: GdsUnits,         // Spatial Units
    pub structs: Vec<GdsStruct>, // Vector of defined Stucts, generally Cells

    // Optional (and all thus far unsupported) fields
    #[builder(default, setter(strip_option))]
    libdirsize: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    srfname: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    libsecur: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    reflibs: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    fonts: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    attrtable: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    generations: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    format_type: Option<GdsFormatType>,
}
pub fn parse_library(it: &mut impl Iterator<Item = GdsRecord>) -> Result<GdsLibrary, GdsError> {
    let mut lib = GdsLibraryBuilder::default();
    let mut structs = Vec::<GdsStruct>::new();
    // Read the Header and its version data
    match it.next() {
        Some(GdsRecord::Header { version: v }) => lib.version(v),
        _ => return Err(GdsError::Decode),
    };
    // Read the begin-lib
    match it.next() {
        Some(GdsRecord::BgnLib { date_info: d }) => lib.date_info(d),
        _ => return Err(GdsError::Decode),
    };
    // Iterate over all others
    while let Some(r) = it.next() {
        if let GdsRecord::EndLib = r {
            break; // End-of-library
        }
        match r {
            GdsRecord::LibName(d) => {
                lib.name(d);
            }
            GdsRecord::Units(d0, d1) => {
                lib.units(GdsUnits(d0, d1));
            }
            GdsRecord::BgnStruct { date_info } => {
                let strukt = parse_struct(it, date_info)?;
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
impl GdsLibrary {
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
        GdsRecord::BgnLib {
            date_info: self.date_info.clone(),
        }
        .encode(writer)?;
        GdsRecord::LibName(self.name.clone()).encode(writer)?;
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
    use std::io::BufReader;

    #[test]
    fn it_reads() -> Result<(), GdsError> {
        // Read a sample GDS and compare to golden data
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        let lib = read_gds(&fname)?;
        check(&lib, "sample1.json");
        Ok(())
    }
    #[test]
    fn it_round_trips() -> Result<(), GdsError> {
        // Read a sample, write it back, read that, and check the two are equal
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        let lib = read_gds(&fname)?;
        // And check it round-trips to file
        roundtrip(&lib, "sample1_copy.gds")?;
        Ok(())
    }
    #[test]
    fn it_instantiates() -> Result<(), GdsError> {
        // Read a sample, add a cell which instantiates it
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        let mut lib = read_gds(&fname)?;
        lib.name = "has_inst_lib".into();
        let s = GdsStruct {
            name: "has_inst".into(),
            date_info: vec![0; 12], // FIXME: real date-time
            elems: vec![GdsElement::GdsStructRef(GdsStructRef {
                name: "dff1".into(),
                xy: vec![11_000, 11_000],
                strans: None,
                mag: None,
                angle: None,
                elflags: None,
                plex: None,
            })],
        };
        lib.structs.push(s);
        // Check it against golden data
        check(&lib, "sample1_inst.json");
        // And check it round-trips to file
        roundtrip(&lib, "sample1_inst.gds")?;
        Ok(())
    }
    #[test]
    fn it_arrays() -> Result<(), GdsError> {
        // Read a sample, add a cell which arrays it
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        let mut lib = read_gds(&fname)?;
        lib.name = "has_array_lib".into();
        let s = GdsStruct {
            name: "has_array".into(),
            date_info: vec![0; 12], // FIXME: real date-time
            elems: vec![GdsElement::GdsArrayRef(GdsArrayRef {
                name: "dff1".into(),
                xy: vec![0, 0, 0, 10_000_000, 10_000_000, 0],
                cols: 100,
                rows: 100,
                strans: None,
                mag: None,
                angle: None,
                elflags: None,
                plex: None,
            })],
        };
        lib.structs.push(s);
        // Check it against golden data
        check(&lib, "sample1_array.json");
        // And check it round-trips to file
        roundtrip(&lib, "sample1_array.gds")?;
        Ok(())
    }
    fn roundtrip(lib: &GdsLibrary, fname: &str) -> Result<(), GdsError> {
        let fname = format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname);
        lib.save(&fname)?;
        let lib2 = read_gds(&fname)?;
        assert_eq!(*lib, lib2);
        Ok(())
    }
    /// Compare `lib` to "golden" data loaded from JSON at path `golden`.
    fn check(lib: &GdsLibrary, fname: &str) {
        // Uncomment this bit to over-write the golden data
        // save_json(lib, fname);

        let golden = load_json(fname);
        assert_eq!(*lib, golden);
    }
    /// Load a library from JSON resource at path `fname`
    fn load_json(fname: &str) -> GdsLibrary {
        let fname = format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname);
        let file = File::open(&fname).unwrap();
        let golden: GdsLibrary = serde_json::from_reader(BufReader::new(file)).unwrap();
        golden
    }
    /// Save a `GdsLibrary` as a JSON-format file at path `fname`
    #[allow(dead_code)]
    fn save_json(lib: &GdsLibrary, fname: &str) {
        let fname = format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname);
        let mut file = BufWriter::new(File::create(fname).unwrap());
        let s = serde_json::to_string(lib).unwrap();
        file.write_all(s.as_bytes()).unwrap();
        file.flush().unwrap();
    }
}
