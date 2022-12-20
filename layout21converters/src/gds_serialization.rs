//!
//! # GDSII <=> Markup Conversion Implementation
//! The core logic for `gds2json`, `gds2yaml`, `gds2toml`, and `gds2markup`.
//!

use layout21utils::SerializationFormat;
use std::error::Error;

/// # Conversion Options
///
/// *Awfully* similar to each CLI's `ProgramOptions`,
/// and in the case of `gds2markup`, really exactly the same,
/// without the `clap` CLI annotations.
///
pub struct ConvOptions {
    /// GDS Input File
    pub gds: String,
    /// Output Format. One of ("json", "yaml", "toml")
    pub fmt: String,
    /// Output File
    pub out: String,
    /// Verbose Output Mode
    pub verbose: bool,
}

/// Parse the `fmt` string into a [`SerializationFormat`].
/// FIXME: make this a [`FromStr`] impl for [`SerializationFormat`] instead.
fn parse_format(format: &str) -> Result<SerializationFormat, Box<dyn Error>> {
    match format {
        "json" => Ok(SerializationFormat::Json),
        "yaml" => Ok(SerializationFormat::Yaml),
        "toml" => Err(format!(
            "TOML is not yet supported, see https://github.com/dan-fritchman/Layout21/issues/33"
        )
        .into()),
        _ => Err(format!(
            "Invalid format: {}. Must be one of (json, yaml, toml).",
            format
        )
        .into()),
    }
}

pub fn convert(options: &ConvOptions) -> Result<(), Box<dyn Error>> {
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
    match fmt.save(&gds_library, &options.out) {
        Err(err) => panic!("Could not save output file: {}", err),
        Ok(_) => {}
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

        let output_path = scratch(&format!("sky130_fd_sc_hd__dfxtp_1.test_output.{}", fmtstr));
        let golden_output_path =
            resource(&format!("sky130_fd_sc_hd__dfxtp_1.golden.gds.{}", fmtstr));

        let options = ConvOptions {
            gds: resource("sky130_fd_sc_hd__dfxtp_1.gds"),
            out: output_path.clone(),
            fmt: fmtstr.to_string(),
            verbose: true,
        };

        // Run the main function, producing file `output_path`
        let result = convert(&options);

        // Un-comment to update the golden file.
        // std::fs::copy(&output_path, &golden_output_path).unwrap();

        // Check that `_main` succeeded, and compare the binary data it wrote to disk.
        assert!(result.is_ok());
        let bytes = std::fs::read(&output_path).unwrap();
        let golden_bytes = std::fs::read(&golden_output_path).unwrap();
        assert_eq!(golden_bytes, bytes);
    }

    #[test]
    fn golden_json() {
        test_fmt("json");
    }
    #[test]
    fn golden_yaml() {
        test_fmt("yaml");
    }
    #[test]
    #[ignore] // https://github.com/dan-fritchman/Layout21/issues/33
    fn golden_toml() {
        test_fmt("toml");
    }

    /// Grab the full path of resource-file `fname`
    fn resource(rname: &str) -> String {
        format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
    }
    /// Grab the full path of scratch-file `fname`
    fn scratch(rname: &str) -> String {
        format!("{}/scratch/{}", env!("CARGO_MANIFEST_DIR"), rname)
    }
}
