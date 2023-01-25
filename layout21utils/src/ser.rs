//!
//! # Serialization & Deserialization Utilities
//!
//! Primarily defines convenience functions and traits for saving and loading [serde]-serializable data
//! to and from files. Includes:
//!
//! * An enumerated set of first-class supported [`SerializationFormat`]s
//! * Module-level public functions [`save`] and [`open`] for reading/ writing those formats to and from files
//! * A [`SerdeFile`] trait default-implemented on any [`serde`]-able type, adding [`save`] and [`open`] as methods.
//!
//! Typical usage (FIXME: test this!):
//!
//! ```text
//! use serde::{Serialize, Deserialize};
//! use layout21utils::ser::{save, open, SerdeFile, SerializationFormat::{Json, Yaml, Toml}};
//!
//! #[derive(Serialize, Deserialize, SerdeFile)]
//! struct MyData;
//!
//! let m = MyData;
//!
//! // These all save the same thing:
//! save(m, "mydata.yaml", Yaml)?;
//! m.save("mydata.yaml", Yaml)?;
//! Yaml.save(m, "mydata.yaml")?;
//!
//! // And these similarly load the same thing:
//! let m: MyData = open("mydata.yaml", Yaml)?;
//! let m: MyData = Yaml.open("mydata.yaml")?;
//! let m = MyData::open("mydata.yaml", Yaml)?;
//! ```
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
#[derive(Clone, Copy)]
pub enum SerializationFormat {
    Json,
    Yaml,
    Toml,
}
impl SerializationFormat {
    /// Convert any [serde::Serialize] data to a serialized string
    pub fn to_string(&self, data: &impl Serialize) -> Result<String, Error> {
        match *self {
            Self::Json => Ok(serde_json::to_string_pretty(data)?),
            Self::Yaml => Ok(serde_yaml::to_string(data)?),
            Self::Toml => Ok(toml::to_string_pretty(data)?),
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
        // Delegate to the module-level free function of the same name
        save(data, fname, *self)
    }
    /// Load from file at path `fname`
    pub fn open<T: DeserializeOwned>(&self, fname: impl AsRef<Path>) -> Result<T, Error> {
        // Delegate to the module-level free function of the same name
        open(fname, *self)
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
    fn save(&self, fname: impl AsRef<Path>, fmt: SerializationFormat) -> Result<(), Error> {
        // Delegate to the module-level free function of the same name
        save(self, fname, fmt)
    }
    /// Open from `fmt`-format file `fname`
    fn open(fname: impl AsRef<Path>, fmt: SerializationFormat) -> Result<Self, Error> {
        // Delegate to the module-level free function of the same name
        open(fname, fmt)
    }
}

/// Save `data` to file `fname` in format `fmt`
pub fn save(
    data: &impl Serialize,
    fname: impl AsRef<Path>,
    fmt: SerializationFormat,
) -> Result<(), Error> {
    let mut file = BufWriter::new(std::fs::File::create(fname)?);
    let s = fmt.to_string(data)?;
    file.write_all(s.as_bytes())?;
    file.flush()?;
    Ok(())
}

/// Load `fmt`-formatted content from file at path `fname`
pub fn open<T: DeserializeOwned>(
    fname: impl AsRef<Path>,
    fmt: SerializationFormat,
) -> Result<T, Error> {
    let file = std::fs::File::open(&fname)?;
    let mut file = BufReader::new(file);
    let rv: T = match fmt {
        SerializationFormat::Json => serde_json::from_reader(file)?,
        SerializationFormat::Yaml => serde_yaml::from_reader(file)?,
        SerializationFormat::Toml => {
            // TOML doesn't have that nice reader method, so we kinda recreate (a probably slower) one
            let mut s = String::new();
            file.read_to_string(&mut s)?;
            toml::from_str(&s)?
        }
    };
    Ok(rv)
}

/// Wrapper over other errors
#[derive(Debug)]
pub struct Error(Box<dyn std::error::Error>);

impl std::fmt::Display for Error {
    /// Delegate [`Display`] to the wrapped [`Error`]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}
impl std::error::Error for Error {}

// Automatic conversion from the other error types generated above
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
