//!
//! # Markup to GDSII Conversion CLI 
//!
//! Converts any of [`gds21::GdsLibrary`]'s supported markup-serialization formats, including JSON, YAML, and TOML, to GDSII's on-disk binary format.
//!

use clap::Parser;
use std::error::Error;

// Use our own crate, note by name, not `crate::` or `super::`.
use layout21converters::gds_serialization::{from_markup, FromMarkupOptions};

// => The doc-comment on `ProgramOptions` here is displayed by the `clap`-generated help docs =>

/// Markup to GDSII Converter
/// Converts any of [`gds21::GdsLibrary`]'s supported markup-serialization formats, including JSON, YAML, and TOML, to GDSII's on-disk binary format.
#[derive(Parser)]
pub struct ProgramOptions {
    /// GDS Output File
    #[arg(short = 'o', long, default_value = "")]
    pub gds: String,
    /// Input Format. One of ("json", "yaml", "toml")
    #[arg(short = 'f', long, default_value = "")]
    pub fmt: String,
    /// Input Markup File
    #[arg(short = 'i', long, default_value = "")]
    pub inp: String,
    /// Verbose Output Mode
    #[arg(short, long)]
    pub verbose: bool,
}

impl Into<FromMarkupOptions> for ProgramOptions {
    /// Convert into the [`gds_serialization::FromMarkupOptions`] struct.
    fn into(self) -> FromMarkupOptions {
        FromMarkupOptions {
            gds: self.gds,
            fmt: self.fmt,
            inp: self.inp,
            verbose: self.verbose,
        }
    }
}

/// Main entry point.
/// Parses the command-line arguments and calls [`gds_serialization::from_markup`].
pub fn main() -> Result<(), Box<dyn Error>> {
    let options = ProgramOptions::parse();
    from_markup(&options.into())
}
