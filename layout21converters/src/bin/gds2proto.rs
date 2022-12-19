//! # gds2proto
//!
//! GDSII to VLSIR protobuf schema converter
//!

use clap::Parser;
use layout21raw as raw;
use std::error::Error;

// => The doc-comment on `ProgramOptions` here is displayed by the `clap`-generated help docs =>

/// GDSII to VLSIR Protobuf Schema Converter
#[derive(Parser)]
struct ProgramOptions {
    /// GDS Input File
    #[clap(short = 'i', long, default_value = "")]
    gds: String,
    /// Protobuf Output File
    #[clap(short = 'o', long, default_value = "")]
    proto: String,
    /// Verbose Output Mode
    #[clap(short, long)]
    verbose: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let options = ProgramOptions::parse();
    _main(&options)
}

fn _main(options: &ProgramOptions) -> Result<(), Box<dyn Error>> {
    // Load GDS to [GdsLibrary]
    let gds_library = match gds21::GdsLibrary::load(&options.gds) {
        Err(err) => panic!("Couldn't interpret GDS data: {}", err),
        Ok(lib) => lib,
    };

    if options.verbose {
        let gds_stats = gds_library.stats();
        println!("{:?}", gds_stats);
    }

    // GdsLibrary => raw::Library
    let library = match raw::Library::from_gds(&gds_library, None) {
        Err(err) => panic!("Coulnd't import GDS library: {:?}", err),
        Ok(layout) => layout,
    };

    let proto_library = match library.to_proto() {
        Err(err) => panic!("Couldn't create protobuf library: {}", err),
        Ok(lib) => lib,
    };

    use layout21protos::ProtoFile;
    proto_library.save(&options.proto)?;

    if options.verbose {
        println!("wrote {:?}", &options.proto);
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
        let golden_input_path = resource("sky130_fd_sc_hd__dfxtp_1.gds");
        let golden_output_path = resource("sky130_fd_sc_hd__dfxtp_1.pb");
        let golden_bytes = match std::fs::read(&golden_output_path) {
            Ok(bytes) => bytes,
            Err(_err) => panic!("Could not read golden output file"),
        };
        let output_path = resource("gds2proto_test_output.pb");

        let options = ProgramOptions {
            gds: golden_input_path,
            proto: output_path.clone(),
            verbose: true,
        };

        // Run the main function, producing file `output_path`
        let result = _main(&options);

        // Closure to over-write the golden content. Un-comment to update the golden file.
        // let write_golden = || {
        //     // Read back the newly-written data
        //     use layout21protos::ProtoFile;
        //     let conv_lib = layout21protos::Library::open(&output_path).unwrap();

        //     // Save it to disk in proto-binary format
        //     conv_lib.save(&golden_output_path).unwrap();

        //     // And store a YAML version of it for a bit easier reading & comparison
        //     use layout21utils::SerializationFormat::Yaml;
        //     Yaml.save(&conv_lib, &resource("sky130_fd_sc_hd__dfxtp_1.pb.yaml"))
        //         .unwrap();
        // };
        // write_golden();

        // Check that `_main` succeeded, and compare the binary data it wrote to disk.
        assert!(result.is_ok());
        let bytes = match std::fs::read(&output_path) {
            Ok(bytes) => bytes,
            Err(_err) => panic!("Could not read test output file"),
        };
        assert_eq!(golden_bytes, bytes);
    }

    /// Grab the full path of resource-file `fname`
    fn resource(rname: &str) -> String {
        format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
    }
}
