//!
//! # Serialization & Deserialization Utilities
//! (and file IO for those serialized objects)
//!

// Standard Lib Imports
#[allow(unused_imports)]
use std::io::prelude::*;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::Path;

// Crates.io Imports
use serde::de::DeserializeOwned;
use serde::Serialize;
use textwrap::dedent;

/// # Enumerated First-Class-Supported Serialization Formats
pub enum SerializationFormat {
    Json,
    Yaml,
    Toml,
}
impl SerializationFormat {
    /// Convert any [serde::Serialize] data to a serialized string
    pub fn to_string(&self, data: &impl Serialize) -> Result<String, Error> {
        match *self {
            Self::Json => Ok(serde_json::to_string(data)?),
            Self::Yaml => Ok(serde_yaml::to_string(data)?),
            Self::Toml => Ok(toml::to_string(data)?),
        }
    }
    /// Parse string `s`
    pub fn from_str<T: DeserializeOwned>(&self, s: &str) -> Result<T, Error> {
        let s = dedent(s);
        match *self {
            Self::Json => Ok(serde_json::from_str(&s)?),
            Self::Yaml => Ok(serde_yaml::from_str(&s)?),
            Self::Toml => Ok(toml::from_str(&s)?),
        }
    }
    /// Save `data` to file `fname`
    pub fn save(&self, data: &impl Serialize, fname: impl AsRef<Path>) -> Result<(), Error> {
        let mut file = BufWriter::new(std::fs::File::create(fname)?);
        let s = self.to_string(data)?;
        file.write_all(s.as_bytes())?;
        file.flush()?;
        Ok(())
    }
    /// Load from file at path `fname`
    pub fn open<T: DeserializeOwned>(&self, fname: impl AsRef<Path>) -> Result<T, Error> {
        let file = std::fs::File::open(&fname)?;
        let mut file = BufReader::new(file);
        let rv: T = match *self {
            Self::Json => serde_json::from_reader(file)?,
            Self::Yaml => serde_yaml::from_reader(file)?,
            Self::Toml => {
                // TOML doesn't have that nice reader method, so we kinda recreate (a probably slower) one
                let mut s = String::new();
                file.read_to_string(&mut s)?;
                toml::from_str(&s)?
            }
        };
        Ok(rv)
    }
}

/// Serialization to & from file trait
///
/// Includes:
/// * `open` for loading from file
/// * `save` for saving to file
///
/// Fully default-implemented, allowing empty implementations
/// for types that implement [serde] serialization and deserialization.
///
pub trait SerdeFile: Serialize + DeserializeOwned {
    /// Save in `fmt`-format to file `fname`
    fn save(&self, fmt: SerializationFormat, fname: impl AsRef<Path>) -> Result<(), Error> {
        fmt.save(self, fname)
    }
    /// Open from `fmt`-format file `fname`
    fn open(fname: impl AsRef<Path>, fmt: SerializationFormat) -> Result<Self, Error> {
        fmt.open(fname)
    }
}

/// Wrapper over other errors
#[derive(Debug)]
pub struct Error(Box<dyn std::error::Error + Send + Sync>);
impl std::fmt::Display for Error {
    /// Delegate [std::fmt::Display] to the (derived) [std::fmt::Debug] implementation.
    /// Maybe more info that wanted in some cases. But certainly enough.
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for Error {}
impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Self(Box::new(e))
    }
}
impl From<serde_yaml::Error> for Error {
    fn from(e: serde_yaml::Error) -> Self {
        Self(Box::new(e))
    }
}
impl From<toml::ser::Error> for Error {
    fn from(e: toml::ser::Error) -> Self {
        Self(Box::new(e))
    }
}
impl From<toml::de::Error> for Error {
    fn from(e: toml::de::Error) -> Self {
        Self(Box::new(e))
    }
}
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self(Box::new(e))
    }
}
