//!
//! # GDSII to Alternate Serialization Conversion CLI
//! 
//! Converts a [`gds21::GdsLibrary`] to any of the supported alternate serialization formats.
//!

use clap::Parser;
use layout21utils::SerializationFormat;
use std::error::Error;

// => The doc-comment on `ProgramOptions` here is displayed by the `clap`-generated help docs => 

/// GDSII to Alternate Serializer Converter
#[derive(Parser)]
pub struct ProgramOptions {
    /// GDS Input File
    #[clap(short = 'i', long, default_value = "")]
    gds: String,
    /// Output Format. One of ("json", "yaml", "toml")
    #[clap(short = 'f', long, default_value = "")]
    fmt: String,
    /// Output File
    #[clap(short = 'o', long, default_value = "")]
    out: String,
    /// Verbose Output Mode
    #[clap(short, long)]
    verbose: bool,
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let options = ProgramOptions::parse();
    _main(&options)
}

/// Parse the `fmt` string into a [`SerializationFormat`].
/// FIXME: make this a [`FromStr`] impl for [`SerializationFormat`] instead.
fn parse_format(format: &str) -> Result<SerializationFormat, Box<dyn Error>> {
    match format {
        "json" => Ok(SerializationFormat::Json),
        "yaml" => Ok(SerializationFormat::Yaml),
        "toml" => Err(format!("TOML is not yet supported, see https://github.com/dan-fritchman/Layout21/issues/33").into()),
        _ => Err(format!("Invalid format: {}. Must be one of (json, yaml, toml).", format).into()),
    }
}

pub fn _main(options: &ProgramOptions) -> Result<(), Box<dyn Error>> {
    // Load GDS to [GdsLibrary]
    let gds_library = match gds21::GdsLibrary::load(&options.gds) {
        Err(err) => panic!("Couldn't interpret GDS data: {}", err),
        Ok(lib) => lib,
    };
    
    if options.verbose {
        let gds_stats = gds_library.stats();
        println!("{:?}", gds_stats);
    }
    
    // Save to the target format
    let fmt: SerializationFormat = parse_format(&options.fmt)?;
    match fmt.save(&gds_library, &options.out){
        Err(err) => panic!("Could not save output file: {}", err),
        Ok(_) => {},
    };

    if options.verbose {
        println!("wrote {:?}", &options.out);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // Run the golden-file test for format (string) `fmtstr`
    fn test_fmt(fmtstr: &str) {
        // The golden file was created by running the program:
        // $ cargo run --bin gds2ser -- \
        //      -i resources/sky130_fd_sc_hd__dfxtp_1.gds \
        //      -o resources/sky130_fd_sc_hd__dfxtp_1.golden.json \
        //      -f json
        let golden_input_path = resource("sky130_fd_sc_hd__dfxtp_1.gds");
        let golden_output_path = resource(&format!("sky130_fd_sc_hd__dfxtp_1.golden.{}", fmtstr));
        let golden_bytes = match std::fs::read(&golden_output_path) {
            Ok(bytes) => bytes,
            Err(_err) => panic!("Could not read golden output file"),
        };
        let output_path = resource(&format!("sky130_fd_sc_hd__dfxtp_1.test_output.{}", fmtstr));

        let options = ProgramOptions {
            gds: golden_input_path,
            out: output_path.clone(),
            fmt: fmtstr.to_string(),
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

    #[test]
    fn golden_json(){
        test_fmt("json");
    }
    #[test]
    fn golden_yaml(){
        test_fmt("yaml");
    }
    #[test]
    #[ignore] // https://github.com/dan-fritchman/Layout21/issues/33
    fn golden_toml(){
        test_fmt("toml");
    }

    #[test]
    fn roundtrip_to_golden_file() {
        // The golden file was created by running the program:
        // $ cargo run --bin gds2ser -- \
        //      -i resources/sky130_fd_sc_hd__dfxtp_1.gds \
        //      -o resources/sky130_fd_sc_hd__dfxtp_1.golden.json \
        //      -f json
        let golden_input_path = resource("sky130_fd_sc_hd__dfxtp_1.gds");
        let golden_output_path = resource("sky130_fd_sc_hd__dfxtp_1.golden.json");
        let golden_bytes = match std::fs::read(&golden_output_path) {
            Ok(bytes) => bytes,
            Err(_err) => panic!("Could not read golden output file"),
        };
        let output_path = resource("sky130_fd_sc_hd__dfxtp_1.test_output.json");

        let options = ProgramOptions {
            gds: golden_input_path,
            out: output_path.clone(),
            fmt: "json".to_string(),
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
