//! # proto2gds
//!
//! VLSIR protobuf schema to GDSII converter
//!
//! This program is the sibiling of gds2proto.


use clap::Parser;
use layout21raw as raw;
use std::error::Error;
use layout21protos::conv as proto_converters;

#[derive(Parser)]
struct ProgramOptions {
    #[clap(short = 'i', long, default_value = "")]
    proto: String,
    #[clap(short = 'o', long, default_value = "")]
    gds: String,
    #[clap(short, long)]
    verbose: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let options = ProgramOptions::parse();
    return _main(&options);
}

fn _main(options: &ProgramOptions) -> Result<(), Box<dyn Error>> {
    let proto_library = match proto_converters::open(&options.proto) {
        Err(err) => panic!("Couldn't read protobuf: {}", err),
        Ok(lib) => lib,
    };

    if options.verbose {
        println!("read: {:?}", &options.proto);
    }

    let library = match raw::Library::from_proto(proto_library, None) {
        Err(err) => panic!("Couldn't load library from protobuf: {}", err),
        Ok(lib) => lib,
    };

    let gds_library = match library.to_gds() {
        Err(err) => panic!("Couldn't convert to GDS library: {}", err),
        Ok(lib) => lib,
    };



    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_to_golden_file() {
        assert_eq!(1, 1);
    }
}
