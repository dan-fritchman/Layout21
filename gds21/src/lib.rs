use std::error::Error;
use std::fmt;
use std::fs::File;
use std::str;

use byteorder::{BigEndian, ReadBytesExt};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use serde::{Deserialize, Serialize};

#[macro_use]
extern crate derive_builder;

/// Error-Type for Invalid GDSII Decoding
#[derive(Debug, Clone, Copy)]
pub struct GdsDecodeError;

impl fmt::Display for GdsDecodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "GdsDecodeError")
    }
}

impl Error for GdsDecodeError {}

impl From<std::io::Error> for GdsDecodeError {
    fn from(_e: std::io::Error) -> Self {
        Self
    }
}
impl From<String> for GdsDecodeError {
    fn from(_e: String) -> Self {
        Self
    }
}

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
    StructName,
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
    SName, // Apparently the same as `StructName`
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
    Bgnextn, // "Only occurs in CustomPlus"
    Endextn, // "Only occurs in CustomPlus"
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

///
/// # Gds Record Enumeration
///
/// Keeps each record in relatively "raw" form,
/// other than assuring correct data-types,
/// and converting one-entry arrays into scalars.
/// Unsupported record-types are not included.
///
#[derive(Debug)]
pub enum GdsRecord {
    Header { version: i16 },
    BgnLib { date_info: Vec<i16> },
    LibName(String),
    Units { db: f64, user: f64 },
    EndLib,
    BgnStruct { date_info: Vec<i16> },
    StructName(String),
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
    TapeNum(i16),
    TapeCode(Vec<i16>), // Note: always length 6
    Format(i16),
    Mask(String),
    EndMasks,
    LibDirSize(i16),
    SrfName(String),
    LibSecur(i16),
}

impl GdsRecordType {
    /// Boolean indication of valid record types
    /// Many are either deprecated or provisioned without ever being implemented;
    /// all from this list are deemed invalid.
    pub fn valid(&self) -> bool {
        match self {
            Self::SName | // Apparently the same as `StructName`
            Self::TextNode | // "Not currently used"
            Self::Spacing | // "Discontinued"
            Self::Uinteger | // "No longer used"
            Self::Ustring |  // "No longer used"
            Self::StypTable | // "Unreleased Feature"
            Self::StrType |   // "Unreleased Feature"
            Self::ElemKey |   // "Unreleased Feature"
            Self::LinkType |  // "Unreleased Feature"
            Self::LinkKeys |  // "Unreleased Feature"
            Self::Bgnextn |  // "Only occurs in CustomPlus"
            Self::Endextn |  // "Only occurs in CustomPlus"
            Self::StrClass | // "Only for Calma internal use"
            Self::Reserved   // "Reserved for future use"
              => false,
            _ => true,
        }
    }
}

///
/// # Gds DataType Enumeration
///
/// In order as decoded from 16-bit integers in binary data
///
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

fn read_str(file: &mut File, len: u16) -> String {
    // ASCII Decode. First load into a bytes-vector.
    let mut data: Vec<u8> = (0..len).map(|_| file.read_u8().unwrap()).collect();
    // Strip optional end-of-string chars
    if data[data.len() - 1] == 0x00 {
        data.pop();
    }
    // And convert to string
    let s: String = std::str::from_utf8(&data).unwrap().into();
    s
}

fn read_bytes(file: &mut File, len: u16) -> Vec<u8> {
    (0..len).map(|_| file.read_u8().unwrap()).collect()
}

fn read_i16(file: &mut File, len: u16) -> Vec<i16> {
    (0..len / 2)
        .map(|_| file.read_i16::<BigEndian>().unwrap())
        .collect()
}

fn read_i32(file: &mut File, len: u16) -> Vec<i32> {
    (0..len / 4)
        .map(|_| file.read_i32::<BigEndian>().unwrap())
        .collect()
}
fn read_f64(file: &mut File, len: u16) -> Vec<f64> {
    // This is more fun, as it requires first grabbing "gds floats",
    // which we capture as eight-byte Vec<u8>,
    // and then convert to IEEE-standard floats
    let mut data = Vec::<f64>::new();
    for _x in 0..(len / 8) {
        let mut bytes = Vec::<u8>::new();
        for _y in 0..8 {
            bytes.push(file.read_u8().unwrap());
        }
        data.push(gds_float_to_normal_peoples_float(&bytes).unwrap());
    }
    data
}

/// Read a GDS loaded from file at path `file_name`
pub fn read_gds(file_name: &str) -> Result<GdsLibrary, GdsDecodeError> {
    // Open our file, read its header
    let mut file = File::open(&file_name)?;
    let mut records = Vec::<GdsRecord>::new();

    loop {
        // Read the 16-bit record-size. (In bytes, including the four header bytes.)
        let len = match file.read_u16::<BigEndian>() {
            Err(ref e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break, // End-of-file
            Err(_) => return Err(GdsDecodeError), // Some other kinda error; raise it.
            Ok(num) if num < 4 => return Err(GdsDecodeError), // Invalid (too short) length; throw Error.
            Ok(num) if num % 2 != 0 => return Err(GdsDecodeError), // Invalid (odd) length; throw Error.
            Ok(num) => num,                                        // The normal case
        };
        let len = len - 4; // Strip out the four header-bytes
                           // Read and decode its RecordType
        let record_type = file.read_u8()?;
        let record_type: GdsRecordType =
            FromPrimitive::from_u8(record_type).ok_or(GdsDecodeError)?;
        if !record_type.valid() {
            return Err(GdsDecodeError);
        }
        // Read and decode its DataType
        let data_type = file.read_u8()?;
        let data_type = FromPrimitive::from_u8(data_type).ok_or(GdsDecodeError)?;

        // Based on that header-data, decode to a `GdsRecord`
        use GdsDataType::{BitArray, NoData, Str, F64, I16, I32};
        let record: GdsRecord = match (record_type, data_type, len) {
            // Library-Level Records
            (GdsRecordType::Header, I16, 2) => GdsRecord::Header {
                version: read_i16(&mut file, len)[0],
            },
            (GdsRecordType::BgnLib, I16, 24) => GdsRecord::BgnLib {
                date_info: read_i16(&mut file, len),
            },
            (GdsRecordType::LibName, Str, _) => GdsRecord::LibName(read_str(&mut file, len)),
            (GdsRecordType::Units, F64, 16) => {
                let v = read_f64(&mut file, len);
                GdsRecord::Units {
                    db: v[0],
                    user: v[1],
                }
            }
            (GdsRecordType::EndLib, NoData, 0) => GdsRecord::EndLib,

            // Structure (Cell) Level Records
            (GdsRecordType::BgnStruct, I16, 24) => GdsRecord::BgnStruct {
                date_info: read_i16(&mut file, len),
            },
            (GdsRecordType::StructName, Str, _) => GdsRecord::StructName(read_str(&mut file, len)),
            (GdsRecordType::EndStruct, NoData, 0) => GdsRecord::EndStruct,

            // Element-Level Records
            (GdsRecordType::Boundary, NoData, 0) => GdsRecord::Boundary,
            (GdsRecordType::Path, NoData, 0) => GdsRecord::Path,
            (GdsRecordType::StructRef, NoData, 0) => GdsRecord::StructRef,
            (GdsRecordType::ArrayRef, NoData, 0) => GdsRecord::ArrayRef,
            (GdsRecordType::Text, NoData, 0) => GdsRecord::Text,
            (GdsRecordType::Layer, I16, 2) => GdsRecord::Layer(read_i16(&mut file, len)[0]),
            (GdsRecordType::DataType, I16, 2) => GdsRecord::DataType(read_i16(&mut file, len)[0]),
            (GdsRecordType::Width, I32, 4) => GdsRecord::Width(read_i32(&mut file, len)[0]),
            (GdsRecordType::Xy, I32, _) => GdsRecord::Xy(read_i32(&mut file, len)),
            (GdsRecordType::EndElement, NoData, 0) => GdsRecord::EndElement,

            // More (less well-categorized here) record-types
            (GdsRecordType::ColRow, I16, 4) => {
                let d = read_i16(&mut file, len);
                GdsRecord::ColRow {
                    cols: d[0],
                    rows: d[1],
                }
            }
            (GdsRecordType::Node, NoData, 0) => GdsRecord::Node,
            (GdsRecordType::TextType, I16, 2) => GdsRecord::TextType(read_i16(&mut file, len)[0]),
            (GdsRecordType::Presentation, BitArray, 2) => {
                let bytes = read_bytes(&mut file, len);
                GdsRecord::Presentation(bytes[0], bytes[1])
            }
            (GdsRecordType::String, Str, _) => GdsRecord::String(read_str(&mut file, len)),
            (GdsRecordType::Strans, BitArray, 2) => {
                let bytes = read_bytes(&mut file, len);
                GdsRecord::Strans(bytes[0], bytes[1])
            }
            (GdsRecordType::Mag, F64, 8) => GdsRecord::Mag(read_f64(&mut file, len)[0]),
            (GdsRecordType::Angle, F64, 8) => GdsRecord::Angle(read_f64(&mut file, len)[0]),
            (GdsRecordType::RefLibs, Str, _) => GdsRecord::RefLibs(read_str(&mut file, len)),
            (GdsRecordType::Fonts, Str, _) => GdsRecord::Fonts(read_str(&mut file, len)),
            (GdsRecordType::PathType, I16, 2) => GdsRecord::PathType(read_i16(&mut file, len)[0]),
            (GdsRecordType::Generations, I16, 2) => {
                GdsRecord::Generations(read_i16(&mut file, len)[0])
            }
            (GdsRecordType::AttrTable, Str, _) => GdsRecord::AttrTable(read_str(&mut file, len)),
            (GdsRecordType::ElemFlags, BitArray, 2) => {
                let bytes = read_bytes(&mut file, len);
                GdsRecord::ElemFlags(bytes[0], bytes[1])
            }
            (GdsRecordType::Nodetype, I16, 2) => GdsRecord::Nodetype(read_i16(&mut file, len)[0]),
            (GdsRecordType::PropAttr, I16, 2) => GdsRecord::PropAttr(read_i16(&mut file, len)[0]),
            (GdsRecordType::PropValue, Str, _) => GdsRecord::PropValue(read_str(&mut file, len)),
            (GdsRecordType::Box, NoData, 0) => GdsRecord::Box,
            (GdsRecordType::BoxType, I16, 2) => GdsRecord::BoxType(read_i16(&mut file, len)[0]),
            (GdsRecordType::Plex, I32, 4) => GdsRecord::Plex(read_i32(&mut file, len)[0]),
            (GdsRecordType::TapeNum, I16, 2) => GdsRecord::TapeNum(read_i16(&mut file, len)[0]),
            (GdsRecordType::TapeCode, I16, 12) => GdsRecord::TapeCode(read_i16(&mut file, len)),
            (GdsRecordType::Format, I16, 2) => GdsRecord::Format(read_i16(&mut file, len)[0]),
            (GdsRecordType::Mask, Str, _) => GdsRecord::Mask(read_str(&mut file, len)),
            (GdsRecordType::EndMasks, NoData, 0) => GdsRecord::EndMasks,
            (GdsRecordType::LibDirSize, I16, 2) => {
                GdsRecord::LibDirSize(read_i16(&mut file, len)[0])
            }
            (GdsRecordType::SrfName, Str, _) => GdsRecord::SrfName(read_str(&mut file, len)),
            (GdsRecordType::LibSecur, I16, 2) => GdsRecord::LibSecur(read_i16(&mut file, len)[0]),

            // Invalid or unsupported record
            _ => return Err(GdsDecodeError),
        };
        records.push(record);
    }
    // Create an iterator over records, and parse it to a library-tree
    let mut it = records.into_iter();
    let lib = parse_library(&mut it)?;
    // Check that end-of-library is the end-of-stream
    if it.next().is_some() {
        return Err(GdsDecodeError);
    }
    Ok(lib)
}

/// Incredibly, these things are old enough to have their own float-format,
/// like most computers did before IEEE754
pub fn gds_float_to_normal_peoples_float(bytes: &[u8]) -> Result<f64, GdsDecodeError> {
    if bytes.len() != 8 {
        return Err(GdsDecodeError); // Bad length
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

/// Parse a `GdsTextElement` from an iterator of `GdsRecords`
/// Assumes the initial `Text` record has already been parsed.
pub fn parse_text_elem(
    it: &mut impl Iterator<Item = GdsRecord>,
) -> Result<GdsTextElem, GdsDecodeError> {
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
            GdsRecord::Mag(d) => b.mag(d),
            GdsRecord::Angle(d) => b.angle(d),
            GdsRecord::Presentation(d0, d1) => b.presentation(GdsPresentation(d0, d1)),
            GdsRecord::Strans(d0, d1) => b.strans(GdsStrans(d0, d1)),
            // Pending potential support
            GdsRecord::Plex(_) | GdsRecord::ElemFlags(_, _) => return Err(GdsDecodeError),
            // Invalid
            _ => return Err(GdsDecodeError),
        };
    }
    Ok(b.build()?)
}

pub fn parse_boundary(
    it: &mut impl Iterator<Item = GdsRecord>,
) -> Result<GdsBoundary, GdsDecodeError> {
    let mut b = GdsBoundaryBuilder::default();

    while let Some(r) = it.next() {
        if let GdsRecord::EndElement = r {
            break; // End-of-element
        }
        let _ = match r {
            GdsRecord::Layer(d) => b.layer(d),
            GdsRecord::DataType(d) => b.datatype(d),
            GdsRecord::Xy(d) => b.xy(d),
            // Pending potential support
            GdsRecord::Plex(_) | GdsRecord::ElemFlags(_, _) => return Err(GdsDecodeError),
            // Invalid
            _ => return Err(GdsDecodeError),
        };
    }
    Ok(b.build()?)
}
pub fn parse_struct(it: &mut impl Iterator<Item = GdsRecord>) -> Result<GdsStruct, GdsDecodeError> {
    let mut strukt = GdsStructBuilder::default();
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
            // Invalid
            _ => return Err(GdsDecodeError),
        };
    }
    strukt.elems(elems);
    Ok(strukt.build()?)
}

pub fn parse_library(
    it: &mut impl Iterator<Item = GdsRecord>,
) -> Result<GdsLibrary, GdsDecodeError> {
    let mut lib = GdsLibraryBuilder::default();
    let mut structs = Vec::<GdsStruct>::new();
    // Read the Header and its version data
    match it.next() {
        Some(GdsRecord::Header { version: v }) => lib.version(v),
        _ => return Err(GdsDecodeError),
    };
    // Read the begin-lib
    match it.next() {
        Some(GdsRecord::BgnLib { date_info: d }) => lib.date_info(d),
        _ => return Err(GdsDecodeError),
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
            GdsRecord::Units { db, user } => {
                lib.units(GdsUnits { db, user });
            }
            GdsRecord::BgnStruct { date_info } => {
                let strukt = parse_struct(it)?;
                structs.push(strukt);
            }
            // Invalid
            _ => return Err(GdsDecodeError),
        };
    }
    // Add the Vec of structs, and create the Library from its builder
    lib.structs(structs);
    Ok(lib.build()?)
}

/// A placeholder while building up structural elements,
/// while not having everyting underneath
#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct Tbd {}

#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsStrans(u8, u8);

#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsPresentation(u8, u8);

#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsElemFlags(u8, u8);

#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsPlex(i32);

#[derive(Default, Debug, Clone, Deserialize, Serialize, PartialEq)]
pub struct GdsUnits {
    pub db: f64,
    pub user: f64,
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
    // Required
    pub string: String,
    pub layer: i16,
    pub texttype: i16,
    pub xy: Vec<i32>,

    // Optional
    #[builder(default, setter(strip_option))]
    pub presentation: Option<GdsPresentation>,
    #[builder(default, setter(strip_option))]
    pub strans: Option<GdsStrans>,
    #[builder(default, setter(strip_option))]
    pub mag: Option<f64>, // Magnification
    #[builder(default, setter(strip_option))]
    pub angle: Option<f64>, // Angle

    // Not Supported (At least not yet)
    #[builder(default, setter(strip_option))]
    pub properties: Option<Vec<GdsProperty>>,
    #[builder(default, setter(strip_option))]
    pub elflags: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    pub pathtype: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    pub width: Option<Tbd>,
}

/// Gds Boundary Element
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsBoundary {
    pub layer: i16,
    pub datatype: i16,
    pub xy: Vec<i32>,

    // Not Supported (At least not yet)
    #[builder(default, setter(strip_option))]
    pub properties: Option<Vec<GdsProperty>>,
    #[builder(default, setter(strip_option))]
    pub elflags: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<Tbd>,
}

///
/// # Gds Element Enumeration  
///
/// Spec BNF:
/// {<boundary> | <path> | <SREF> | <AREF> | <text> | <node> | <box>} {<property>}* ENDEL
///
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub enum GdsElement {
    GdsBoundary(GdsBoundary),
    GdsPath(Tbd),
    GdsSref(Tbd),
    GdsAref(Tbd),
    GdsTextElem(GdsTextElem),
    GdsNode(Tbd),
    GdsBox(Tbd),
}

///
/// # Gds Property
///
/// Spec BNF:
/// PROPATTR PROPVALUE
///
#[derive(Default, Clone, Builder, Debug, Deserialize, Serialize, PartialEq)]
#[builder(setter(into))]
pub struct GdsProperty {
    pub attr: i16,
    pub value: String,
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
    pub name: String,
    pub elems: Vec<GdsElement>,
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
    pub name: String, // Library Name
    pub version: i16,
    pub date_info: Vec<i16>,
    pub units: GdsUnits,
    pub structs: Vec<GdsStruct>, // Vector of defined Stucts, generally Cells

    // Optional / TBD fields
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
    format_type: Option<Tbd>,
}
#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::BufReader;
    use std::path::Path;
    use super::*;

    #[test]
    fn it_reads() -> Result<(), GdsDecodeError> {
        // Read a sample GDS
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        let lib = read_gds(&fname)?;
        // Read its "golden" (OK, previously parsed) version 
        let fname = format!("{}/resources/sample1.json", env!("CARGO_MANIFEST_DIR"));
        let file = File::open(&fname).unwrap();
        let golden: GdsLibrary = serde_json::from_reader(BufReader::new(file)).unwrap();
        // And check they're the same
        assert_eq!(lib, golden);
        Ok(())
    }
}
