use std::fs::File;
use std::str;

use byteorder::{BigEndian, ReadBytesExt};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

/// Error-Type for Invalid GDSII Decoding
#[derive(Debug, Clone)]
pub struct GdsDecodeError;

impl From<std::io::Error> for GdsDecodeError {
    fn from(_e: std::io::Error) -> Self {
        Self
    }
}

/// Gds Record Types
/// In the numeric-order specified by GDSII,
/// for automatic `FromPrimitive` conversions
#[derive(FromPrimitive, Debug)]
pub enum GdsRecordType {
    Header = 0x00,
    Bgnlib,
    Libname,
    Units,
    Endlib,
    Bgnstr,
    Strname,
    Endstr,
    Boundary,
    Path,
    Sref,
    Aref,
    Text,
    Layer,
    Datatype,
    Width,
    Xy,
    Endel,
    Sname,
    Colrow,
    Textnode,
    Node,
    Texttype,
    Presentation,
    Spacing,
    String,
    Strans,
    Mag,
    Angle,
    Uinteger,
    Ustring,
    Reflibs,
    Fonts,
    Pathtype,
    Generations,
    Attrtable,
    Styptable,
    Strtype,
    Elflags,
    Elkey,
    Linktype,
    Linkkeys,
    Nodetype,
    Propattr,
    Propvalue,
    Box,
    Boxtype,
    Plex,
    Bgnextn,
    Endextn,
    Tapenum,
    Tapecode,
    Strclass,
    Reserved,
    Format,
    Mask,
    Endmasks,
    Libdirsize,
    Srfname,
    Libsecur,
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

#[derive(Debug)]
pub enum GdsRecordData {
    NoData,
    BitArray(Vec<u8>),
    I16(Vec<i16>),
    I32(Vec<i32>),
    F32(Vec<f32>),
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

    loop {
        // Read the 16-bit record-size
        let len = match file.read_u16::<BigEndian>() {
            Ok(num) if num < 4 => return Err(GdsDecodeError),
            Ok(num) => num,
            Err(ref e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
            Err(_) => return Err(GdsDecodeError),
        };
        let len = len - 4; // Strip out the four header-bytes

        // Read and decode its RecordType
        let record_type = file.read_u8()?;
        let record_type: GdsRecordType =
            FromPrimitive::from_u8(record_type).ok_or(GdsDecodeError)?;

        // Read and decode its DataType
        let data_type = file.read_u8()?;
        let data_type: GdsDataType = FromPrimitive::from_u8(data_type).ok_or(GdsDecodeError)?;

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
        println!("{:?}", record);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_reads() -> Result<(), GdsDecodeError> {
        let fname = format!("{}/resources/sample1.gds", env!("CARGO_MANIFEST_DIR"));
        read_gds(&fname)?;
        Ok(())
    }
}
