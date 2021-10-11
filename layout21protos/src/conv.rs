//!
//! # ProtoBuf Conversion Utilities
//!
//! Methods and traits, primarily for converting proto-objects to and from bytes and files thereof.
//!

// Std-Lib Imports
use std::error::Error;
use std::fs::File;
use std::io::{BufWriter, Read, Write};
use std::path::Path;

// Crates.io Imports
use prost::{DecodeError, Message};

/// Encode into a newly-allocated byte-[Vec].
/// Wraps [prost::Message::encode], which is likely more efficent
/// for pre-allocated buffers.
pub fn to_bytes<T: Message + Sized + Default>(data: &T) -> Vec<u8> {
    let mut buf = Vec::<u8>::with_capacity(data.encoded_len());
    data.encode(&mut buf).unwrap();
    buf
}
/// Decode from byte array/vector
///
/// Wraps [prost::Message::decode], adding support for some input types,
/// notably including `&Vec<u8>` as returned from `to_bytes`.
/// Using [prost::Message::decode] directly instead generally
/// works, but requires casting to slice via [Vec::as_slice].
///
pub fn from_bytes<T: Message + Sized + Default>(bytes: &[u8]) -> Result<T, DecodeError> {
    T::decode(bytes)
}
/// Open from file `fname`
pub fn open<Data, P>(fname: P) -> Result<Data, Box<dyn Error>>
where
    Data: Message + Sized + Default,
    P: AsRef<Path>,
{
    let mut file = File::open(&fname)?;
    let mut buf = Vec::<u8>::new();
    file.read_to_end(buf.as_mut())?;
    let res = Data::decode(buf.as_ref())?;
    Ok(res)
}
/// Save to file `fname`
pub fn save<Data, P>(data: &Data, fname: P) -> Result<(), Box<dyn Error>>
where
    Data: Message + Sized + Default,
    P: AsRef<Path>,
{
    let mut file = BufWriter::new(File::create(fname)?);
    file.write_all(&to_bytes(data))?;
    Ok(())
}

/// Trait for reading and writing binary proto-format data to/from files
///
/// Includes default-implemented `open` and `save` methods,
/// allowing empty implementations per type.
///
/// Both methods are also exposed as public module-level functions,
/// allowing usage with types that do not implement `ProtoFile`,
/// but do implement its prerequisites.
pub trait ProtoFile: Message + Sized + Default {
    /// Open from file `fname`
    fn open(fname: impl AsRef<Path>) -> Result<Self, Box<dyn Error>> {
        open(fname)
    }
    /// Save to file `fname`
    fn save(&self, fname: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
        save(self, fname)
    }
}
