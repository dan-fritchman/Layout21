//! # lef2yaml
//!
//! LEF to Lef21 YAML Schema Converter
//!

use clap::Parser;
use std::error::Error;

// => The doc-comment on `ProgramOptions` here is displayed by the `clap`-generated help docs =>

/// LEF to Lef21 YAML Schema Converter
#[derive(Parser)]
struct ProgramOptions {
    /// GDS Input File
    #[clap(short = 'i', long, default_value = "")]
    lef: String,
    /// YAML Output File
    #[clap(short = 'o', long, default_value = "")]
    yaml: String,
    /// Verbose Output Mode
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
    
    // Load GDS to [LefLibrary]
    let lef_library = match lef21::LefLibrary::open(&options.lef) {
        Err(err) => panic!("Couldn't interpret GDS data: {:?}", err),
        Ok(lib) => lib,
    };

    // And store a YAML version of it for a bit easier reading & comparison
    use layout21utils::SerializationFormat::Yaml;
    Yaml.save(&lef_library, &options.yaml).unwrap();

    if options.verbose {
        println!("wrote {:?}", &options.yaml);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_to_golden_file() {
        // The golden file was created by running the program:
        // $ cargo run -- -i resources/macro.lef -o resources/macro.yaml
        let golden_input_path = resource("macro.lef");
        let golden_output_path = resource("macro.golden.yaml");
        let output_path = resource("macro.yaml");

        let options = ProgramOptions {
            lef: golden_input_path,
            yaml: output_path.clone(),
            verbose: true,
        };

        // Run the main function, producing file `output_path`
        let result = _main(&options);

        // Closure to over-write the golden content. Un-comment to update the golden file.
        // let write_golden = || {
        //
        //     // FIXME! if you actually wanna overwrite the golden data, re-write this first! 
        //
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

        // FIXME: actually check the content matches the golden content! 
        // let bytes = match std::fs::read(&output_path) {
        //     Ok(bytes) => bytes,
        //     Err(_err) => panic!("Could not read test output file"),
        // };
        // assert_eq!(golden_bytes, bytes);
    }

    /// Grab the full path of resource-file `fname`
    fn resource(rname: &str) -> String {
        format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
    }
}
