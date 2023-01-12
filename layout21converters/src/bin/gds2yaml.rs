//!
//! # GDSII to YAML Conversion CLI
//!
//! Converts a GDSII file to [`gds21::GdsLibrary`] YAML. 
//!

use clap::Parser;
use std::error::Error;

// Use our own crate, note by name, not `crate::` or `super::`.
use layout21converters::gds_serialization::{to_markup, ToMarkupOptions};

// => The doc-comment on `ProgramOptions` here is displayed by the `clap`-generated help docs =>

/// # GDSII to YAML Conversion CLI
/// Converts a GDSII file to [`gds21::GdsLibrary`] YAML. 
#[derive(Parser)]
pub struct ProgramOptions {
    /// GDS Input File
    #[arg(short = 'i', long, default_value = "")]
    pub gds: String,
    /// Output File
    #[arg(short = 'o', long, default_value = "")]
    pub out: String,
    /// Verbose Output Mode
    #[arg(short, long)]
    pub verbose: bool,
}

impl Into<ToMarkupOptions> for ProgramOptions {
    /// Convert into the [`gds_serialization::ToMarkupOptions`] struct.
    fn into(self) -> ToMarkupOptions {
        ToMarkupOptions {
            gds: self.gds,
            fmt: "yaml".to_string(), // <= this is kinda the whole program right here!
            out: self.out,
            verbose: self.verbose,
        }
    }
}

/// Main entry point.
/// Parses the command-line arguments and calls [`gds_serialization::to_markup`].
pub fn main() -> Result<(), Box<dyn Error>> {
    let options = ProgramOptions::parse();
    to_markup(&options.into())
}
