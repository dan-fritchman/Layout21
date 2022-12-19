//! # proto2gds
//!
//! VLSIR protobuf schema to GDSII converter
//!
//! This program is the sibiling of gds2proto.

use clap::Parser;

use layout21protos::conv as proto_converters;
use layout21protos::tech as protos;
use layout21raw as raw;
use raw::utils::Ptr;
use std::error::Error;

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
    use chrono::NaiveDateTime;
    use gds21::GdsLibrary;

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
        let mut golden_gds = match GdsLibrary::from_bytes(&golden_bytes) {
            Ok(lib) => lib,
            Err(_err) => panic!("Could not create golden GDS library"),
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
        let output_bytes = match std::fs::read(&output_path) {
            Ok(bytes) => bytes,
            Err(_err) => panic!("Could not read test output file"),
        };

        let mut output_gds = match GdsLibrary::from_bytes(&output_bytes) {
            Ok(lib) => lib,
            Err(_err) => panic!("Could not create GDS library from test output"),
        };
        // GDS files contain a timestamp that will differ between the golden file and the
        // newly-minted version. To avoid this we load both GDS libraries and zero the date fields,
        // then do the comparison.
        let date = NaiveDateTime::from_timestamp(0, 0);
        golden_gds.set_all_dates(&date);
        output_gds.set_all_dates(&date);

        assert_eq!(output_gds, golden_gds);
    }

    /// Grab the full path of resource-file `fname`
    fn resource(rname: &str) -> String {
        format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
    }
}
