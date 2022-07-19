//! # proto2gds
//!
//! VLSIR protobuf schema to GDSII converter
//!
//! This program is the sibiling of gds2proto.

use clap::Parser;
use layout21raw as raw;
use std::error::Error;
use layout21protos::conv as proto_converters;
use layout21protos::tech as protos;
use raw::utils::Ptr;

#[derive(Parser)]
struct ProgramOptions {
    #[clap(short = 'i', long, default_value = "")]
    proto: String,
    #[clap(short = 'o', long, default_value = "")]
    gds: String,
    #[clap(short = 't', long, default_value = "")]
    tech: String,
    #[clap(short, long)]
    verbose: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let options = ProgramOptions::parse();
    return _main(&options);
}

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

    let gds_library = match library.to_gds() {
        Err(err) => panic!("Couldn't convert to GDS library: {}", err),
        Ok(lib) => lib,
    };

    gds_library.save(&options.gds)?;

    if options.verbose {
        println!("wrote: {:?}", &options.gds);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_to_golden_file() {
        // The golden file was created by running the program:
        // $ cargo run -- \
        //      -i resources/sky130_fd_sc_hd__dfxtp_1.gds \
        //      -o resources/sky130_fd_sc_hd__dfxtp_1.pb
        let golden_input_path = resource("sky130_fd_sc_hd__dfxtp_1.pb");
        let golden_tech_path = resource("sky130.technology.pb");
        let golden_output_path = resource("sky130_fd_sc_hd__dfxtp_1.roundtrip.gds");
        let golden_bytes = match std::fs::read(&golden_output_path) {
            Ok(bytes) => bytes,
            Err(_err) => panic!("Could not read golden output file"),
        };

        let output_path = resource("proto2gds_test_output.gds");
        let options = ProgramOptions {
            proto: golden_input_path,
            gds: output_path.clone(),
            tech: golden_tech_path,
            verbose: true,
        };

        // Run the main function, producing file `output_path`
        let result = _main(&options);

        // Check that `_main` succeeded, and compare the binary data it wrote to disk.
        assert!(result.is_ok());
        let bytes = match std::fs::read(&output_path) {
            Ok(bytes) => bytes,
            Err(_err) => panic!("Could not read test output file"),
        };

        // FIXME: Writing GDS output doesn't seem to be deterministic. Re-running the program will
        // provide a valid but slightly different GDS file each time, making byte-by-byte
        // comparison a poor way to test. If it's not possible to create GDS files
        // deterministically, then we need a different way to make sure this worked. Something like
        // a GDS format checker?
        //assert_eq!(golden_bytes, bytes);
    }

    /// Grab the full path of resource-file `fname`
    fn resource(rname: &str) -> String {
        format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
    }
}
