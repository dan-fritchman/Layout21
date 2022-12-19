//!
//! # GDSII to Alternate Serialization Conversion CLI
//!
//!  Converts a GDSII file to [`gds21::GdsLibrary`]'s alternate serialization formats,  including JSON, YAML, and TOML.
//!

use clap::Parser;
use std::error::Error;

// Use our own crate, note by name, not `crate::` or `super::`.
use layout21converters::gds_serialization::{convert, ConvOptions};

// => The doc-comment on `ProgramOptions` here is displayed by the `clap`-generated help docs =>

/// GDSII to Markup-Based Serialization Format Converter
/// Converts a GDSII file to [`gds21::GdsLibrary`]'s alternate serialization formats,  including JSON, YAML, and TOML.
#[derive(Parser)]
pub struct ProgramOptions {
    /// GDS Input File
    #[clap(short = 'i', long, default_value = "")]
    pub gds: String,
    /// Output Format. One of ("json", "yaml", "toml")
    #[clap(short = 'f', long, default_value = "")]
    pub fmt: String,
    /// Output File
    #[clap(short = 'o', long, default_value = "")]
    pub out: String,
    /// Verbose Output Mode
    #[clap(short, long)]
    pub verbose: bool,
}

/// Main entry point.
/// Thin wrapper around the testable `_main` function.
/// Parses the command-line arguments and calls `_main`.
pub fn main() -> Result<(), Box<dyn Error>> {
    let options = ProgramOptions::parse();
    _main(&options)
}

pub fn _main(options: &ProgramOptions) -> Result<(), Box<dyn Error>> {
    // FIXME: this conversion from `ProgramOptions` to `ConvOptions` is the one passages that doesn't have test coverage.
    let conv_options = ConvOptions {
        gds: options.gds.clone(),
        fmt: options.fmt.clone(),
        out: options.out.clone(),
        verbose: options.verbose,
    };
    convert(&conv_options)
}
