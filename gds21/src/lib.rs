use std::fs::File;
use std::str;

use byteorder::{BigEndian, ReadBytesExt};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[macro_use]
extern crate derive_builder;

/// Error-Type for Invalid GDSII Decoding
#[derive(Debug, Clone)]
pub struct GdsDecodeError;

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

/// Gds Record Types
/// In the numeric-order specified by GDSII,
/// for automatic `FromPrimitive` conversions
#[derive(FromPrimitive, Debug)]
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

#[derive(FromPrimitive, Debug)]
pub enum GdsDataType {
    NoData = 0,
    BitArray = 1,
    TwoByteSignedInteger = 2,
    FourByteSignedInteger = 3,
    FourByteReal = 4,
    EightByteReal = 5,
    AsciiString = 6,
}

///
#[derive(Debug)]
pub enum GdsRecordData {
    NoData,
    BitArray(Vec<u8>),
    I16(Vec<i16>),
    I32(Vec<i32>),
    F32(Vec<f32>), // "At present, this data type is not used" -GDSII Spec. Included here nonetheless.
    F64(Vec<f64>),
    Str(String),
}

/// Gds Stream Record
/// Including record-type and "raw" data
#[derive(Debug)]
pub struct GdsRecord {
    record_type: GdsRecordType,
    data: GdsRecordData,
}

/// Read a GDS loaded from file at path `file_name`
pub fn read_gds(file_name: &str) -> Result<(), GdsDecodeError> {
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
        let record_type = FromPrimitive::from_u8(record_type).ok_or(GdsDecodeError)?;

        // Read and decode its DataType
        let data_type = file.read_u8()?;
        let data_type = FromPrimitive::from_u8(data_type).ok_or(GdsDecodeError)?;

        // And load up the datatype-specific record-data
        let data: GdsRecordData = match data_type {
            GdsDataType::NoData => GdsRecordData::NoData,
            GdsDataType::BitArray => {
                GdsRecordData::BitArray((0..len).map(|_| file.read_u8().unwrap()).collect())
            }
            GdsDataType::TwoByteSignedInteger => GdsRecordData::I16(
                (0..len / 2)
                    .map(|_| file.read_i16::<BigEndian>().unwrap())
                    .collect(),
            ),
            GdsDataType::FourByteSignedInteger => GdsRecordData::I32(
                (0..len / 4)
                    .map(|_| file.read_i32::<BigEndian>().unwrap())
                    .collect(),
            ),
            GdsDataType::EightByteReal => {
                // This is more fun, as it requires first grabbing "gds floats",
                // which we capture as eight-byte Vec<u8>,
                // and then convert to IEEE-standard floats
                let mut data = Vec::<f64>::new();
                for _x in 0..len / 8 {
                    let mut bytes = Vec::<u8>::new();
                    for _y in 0..8 {
                        bytes.push(file.read_u8().unwrap());
                    }
                    data.push(gds_float_to_normal_peoples_float(&bytes)?);
                }
                GdsRecordData::F64(data)
            }
            GdsDataType::AsciiString => {
                // ASCII Decode. First load into a bytes-vector.
                let mut data: Vec<u8> = (0..len).map(|_| file.read_u8().unwrap()).collect();
                // Strip optional end-of-string chars
                if data[data.len() - 1] == 0x00 {
                    data.pop();
                }
                // And convert to string
                let s: String = std::str::from_utf8(&data).unwrap().into();
                GdsRecordData::Str(s)
            }
            // Unsupported type(s) (even in the GDS spec)
            GdsDataType::FourByteReal => return Err(GdsDecodeError),
        };
        let record = GdsRecord { record_type, data };
        records.push(record);
    }
    // Create an iterator over records, and parse it to a library-tree 
    let mut it = records.into_iter();
    let _lib = parse_library(&mut it)?;
    // Check that end-of-library is the end-of-stream
    if it.next().is_some() {
        return Err(GdsDecodeError);
    }
    Ok(())
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
        match (r.record_type, r.data) {
            (GdsRecordType::EndElement, GdsRecordData::NoData) => break,
            (GdsRecordType::Layer, GdsRecordData::I16(d)) if d.len() == 1 => {
                b.layer(d[0]);
            }
            (GdsRecordType::TextType, GdsRecordData::I16(d)) if d.len() == 1 => {
                b.texttype(d[0]);
            }
            (GdsRecordType::Xy, GdsRecordData::I32(d)) => {
                b.xy(d);
            }
            (GdsRecordType::String, GdsRecordData::Str(s)) => {
                b.string(s);
            }
            (GdsRecordType::Presentation, GdsRecordData::BitArray(d)) => {
                b.presentation(GdsPresentation(d[0], d[1]));
            }
            (GdsRecordType::Strans, GdsRecordData::BitArray(d)) => {
                b.strans(GdsStrans(d[0], d[1]));
            }
            (GdsRecordType::Mag, GdsRecordData::F64(d)) if d.len() == 1 => {
                b.mag(d[0]);
            }
            (GdsRecordType::Angle, GdsRecordData::F64(d)) if d.len() == 1 => {
                b.angle(d[0]);
            }
            // Pending potential support
            (GdsRecordType::Plex, _) | (GdsRecordType::ElemFlags, _) | _ => {
                return Err(GdsDecodeError)
            }
        }
    }
    Ok(b.build()?)
}

pub fn parse_boundary(
    it: &mut impl Iterator<Item = GdsRecord>,
) -> Result<GdsBoundary, GdsDecodeError> {
    let mut b = GdsBoundaryBuilder::default();

    while let Some(r) = it.next() {
        match (r.record_type, r.data) {
            (GdsRecordType::EndElement, GdsRecordData::NoData) => break,
            (GdsRecordType::Layer, GdsRecordData::I16(d)) if d.len() == 1 => {
                b.layer(d[0]);
            }
            (GdsRecordType::DataType, GdsRecordData::I16(d)) if d.len() == 1 => {
                b.datatype(d[0]);
            }
            (GdsRecordType::Xy, GdsRecordData::I32(d)) => {
                b.xy(d);
            }
            // Pending potential support
            (GdsRecordType::Plex, _) | (GdsRecordType::ElemFlags, _) | _ => {
                return Err(GdsDecodeError)
            }
        }
    }
    Ok(b.build()?)
}
pub fn parse_struct(it: &mut impl Iterator<Item = GdsRecord>) -> Result<GdsStruct, GdsDecodeError> {
    let mut strukt = GdsStructBuilder::default();
    let mut elems = Vec::<GdsElement>::new();

    while let Some(r) = it.next() {
        println!("{:?}", r);
        match (r.record_type, r.data) {
            (GdsRecordType::EndStruct, GdsRecordData::NoData) => break,
            (GdsRecordType::StructName, GdsRecordData::Str(s)) => {
                strukt.name(s);
            }
            (GdsRecordType::Boundary, GdsRecordData::NoData) => {
                let b = parse_boundary(it)?;
                elems.push(GdsElement::GdsBoundary(b));
            }
            (GdsRecordType::Text, GdsRecordData::NoData) => {
                let b = parse_text_elem(it)?;
                elems.push(GdsElement::GdsTextElem(b));
            }
            _ => (),
        }
    }
    let mut strukt = strukt.build()?;
    strukt.elems = elems;
    Ok(strukt)
}
pub fn parse_library(
    it: &mut impl Iterator<Item = GdsRecord>,
) -> Result<GdsLibrary, GdsDecodeError> {
    let mut lib = GdsLibraryBuilder::default();
    let mut structs = Vec::<GdsStruct>::new();
    // Read the Header
    let hdr = it.next().ok_or(GdsDecodeError)?;
    // Read out the version data. This should be a single int.
    match (hdr.record_type, hdr.data) {
        (GdsRecordType::Header, GdsRecordData::I16(d)) if d.len() == 1 => {
            lib.version(d[0]);
        }
        _ => return Err(GdsDecodeError),
    }
    // Read the begin-lib
    let hdr = it.next().ok_or(GdsDecodeError)?;
    match (hdr.record_type, hdr.data) {
        (GdsRecordType::BgnLib, GdsRecordData::I16(d)) => {
            lib.date_info(d);
        }
        _ => return Err(GdsDecodeError),
    }

    // Iterate over all others
    while let Some(r) = it.next() {
        println!("{:?}", r);
        match (r.record_type, r.data) {
            (GdsRecordType::EndLib, GdsRecordData::NoData) => break,
            (GdsRecordType::BgnStruct, GdsRecordData::I16(d)) => {
                let strukt = parse_struct(it)?;
                structs.push(strukt);
            }
            (GdsRecordType::LibName, GdsRecordData::Str(s)) => {
                lib.name(s);
            }
            (GdsRecordType::Units, GdsRecordData::F64(d)) if d.len() == 2 => {
                lib.units(GdsUnits {
                    db: d[0],
                    user: d[1],
                });
            }
            _ => return Err(GdsDecodeError),
        }
    }
    // Create the Library from its builder, and add the Vec of structs
    let mut lib = lib.build()?;
    lib.structs = structs;
    Ok(lib)
}

/// A placeholder while building up structural elements,
/// while not having everyting underneath
#[derive(Default, Debug, Clone)]
pub struct Tbd {}

#[derive(Default, Debug, Clone)]
pub struct GdsStrans(u8, u8);

#[derive(Default, Debug, Clone)]
pub struct GdsPresentation(u8, u8);

#[derive(Default, Debug, Clone)]
pub struct GdsElemFlags(u8, u8);

#[derive(Default, Debug, Clone)]
pub struct GdsPlex(i32);

#[derive(Default, Debug, Clone)]
pub struct GdsUnits {
    pub db: f64,
    pub user: f64,
}

#[derive(Default, Builder, Debug)]
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

    // (At least not yet) supported
    #[builder(default, setter(strip_option))]
    pub elflags: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    pub pathtype: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    pub width: Option<Tbd>,
}

#[derive(Default, Builder, Debug)]
#[builder(setter(into))]
pub struct GdsBoundary {
    pub layer: i16,
    pub datatype: i16,
    pub xy: Vec<i32>,

    // Optional
    #[builder(default, setter(strip_option))]
    pub elflags: Option<Tbd>,
    #[builder(default, setter(strip_option))]
    pub plex: Option<Tbd>,
}

#[derive(Debug)]
pub enum GdsElement {
    GdsBoundary(GdsBoundary),
    GdsTextElem(GdsTextElem),
    // more to come
}

#[derive(Default, Builder, Debug)]
#[builder(setter(into))]
pub struct GdsStruct {
    pub name: String,
    #[builder(default, setter(skip))]
    pub elems: Vec<GdsElement>,
}

#[derive(Default, Builder, Debug)]
#[builder(setter(into))]
pub struct GdsLibrary {
    pub name: String, // Library Name
    pub version: i16,
    pub date_info: Vec<i16>,
    pub units: GdsUnits,

    #[builder(default, setter(skip))]
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

#[derive(Default, Builder, Debug)]
#[builder(setter(into))]
struct Channel {
    token: i32,
    special_info: i32,
    #[builder(default, setter(strip_option))]
    hasopt: Option<i32>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_reads() -> Result<(), GdsDecodeError> {
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        read_gds(&fname)?;
        Ok(())
    }
    #[test]
    fn test_build() {
        let c = ChannelBuilder::default()
            .token(5)
            .special_info(6)
            .hasopt(7)
            .build()
            .unwrap();
        println!("{:?}", c);
        let c = ChannelBuilder::default()
            .token(5)
            .special_info(6)
            .build()
            .unwrap();
        println!("{:?}", c);
    }
}
