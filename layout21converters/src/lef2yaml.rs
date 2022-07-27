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
    /// LEF Input File
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
    // Load LEF from file to a [LefLibrary]
    let lef_library = lef21::LefLibrary::open(&options.lef)?;

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
    fn roundtrip_to_golden_file() -> Result<(), Box<dyn Error>> {
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

        // Check that `_main` succeeded
        assert!(result.is_ok());
        // Read back what it wrote to disk
        use layout21utils::SerializationFormat::Yaml;
        let readback: lef21::LefLibrary = Yaml.open(&output_path)?;

        // NOTE: Uncomment to overwrite the golden data
        // Yaml.save(&readback, &golden_output_path)?;

        // And compare the generated data to the golden version.
        let golden: lef21::LefLibrary = Yaml.open(&golden_output_path)?;
        assert_eq!(readback, golden);

        Ok(())
    }

    /// Grab the full path of resource-file `fname`
    fn resource(rname: &str) -> String {
        format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
    }
}
