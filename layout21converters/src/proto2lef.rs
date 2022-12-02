//! # proto2gds
//!
//! VLSIR protobuf schema (Layout) to LEF converter
//!

use clap::Parser;
use std::error::Error;
//use chrono::NaiveDateTime;
use gds21::GdsLibrary;
use layout21protos::conv as proto_converters;
use layout21protos::tech as protos;
use layout21raw as raw;
use raw::utils::Ptr;

// => The doc-comment on `ProgramOptions` here is displayed by the `clap`-generated help docs =>

/// LEF to Lef21 YAML Schema Converter
#[derive(Parser)]
struct ProgramOptions {
    /// VLSIR Layout protobuf (binary) input file
    #[clap(short = 'i', long, default_value = "")]
    proto: String,
    /// LEF output file
    #[clap(short = 'o', long, default_value = "")]
    lef: String,
    /// VLSIR Technology protobuf (binary) input file
    #[clap(short = 't', long, default_value = "")]
    tech: String,
    /// Verbose output mode
    #[clap(short, long)]
    verbose: bool,
}

/// The main entry point.
/// All logic is offloaded to `_main` for sake of testing.
fn main() -> Result<(), Box<dyn Error>> {
    let options = ProgramOptions::parse();
    _main(&options)
}

/// All the real logic, with `ProgramOptions` argument for sake of testing
fn _main(options: &ProgramOptions) -> Result<(), Box<dyn Error>> {
    let proto_library = match proto_converters::open(&options.proto) {
        Err(err) => panic!("Couldn't read layout protobuf: {}", err),
        Ok(lib) => lib,
    };

    if options.verbose {
        println!("read: {:?}", &options.proto);
    }

    let tech_library: protos::Technology = match proto_converters::open(&options.tech) {
        Err(err) => panic!("Couldn't read tech protobuf: {}", err),
        Ok(lib) => lib,
    };

    if options.verbose {
        println!("read: {:?}", &options.tech);
    }

    let tech_layers = raw::data::Layers::from_proto(&tech_library)?;

    if options.verbose {
        println!("assembled technology {:?} layer mapping", tech_library.name);
    }

    let library = match raw::Library::from_proto(proto_library, Some(Ptr::new(tech_layers))) {
        Err(err) => panic!("Couldn't load library from protobuf: {}", err),
        Ok(lib) => lib,
    };

    //// Load LEF from file to a [LefLibrary]
    // arya: returns a LayoutResult<lef21::LefLibrary>
    let lef_library = layout21raw::lef::LefExporter::export(&library)?;

    // arya: in lef21/src/write.rs; this returns LefResult<()>
    lef21::save(&lef_library, &options.lef)?;

    if options.verbose {
        println!("wrote {:?}", &options.lef);
    }

    Ok(())
}

//#[cfg(test)]
//mod tests {
//    use super::*;
//
//    #[test]
//    fn roundtrip_to_golden_file() -> Result<(), Box<dyn Error>> {
//        // The golden file was created by running the program:
//        // $ cargo run -- -i resources/macro.lef -o resources/macro.yaml
//
//        // TODO(aryap): this
//
//        Ok(())
//    }
//
//    /// Grab the full path of resource-file `fname`
//    fn resource(rname: &str) -> String {
//        format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
//    }
//}
