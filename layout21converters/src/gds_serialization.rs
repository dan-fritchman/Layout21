//!
//! # GDSII <=> Markup Conversion Implementation
//! The core logic for `gds2json`, `gds2yaml`, `gds2toml`, and `gds2markup`.
//!

use layout21utils::SerializationFormat;
use std::error::Error;

/// # Conversion to Markup Options
///
/// *Awfully* similar to each CLI's `ProgramOptions`,
/// and in the case of `gds2markup`, really exactly the same,
/// without the `clap` CLI annotations.
///
pub struct ToMarkupOptions {
    /// GDS Input File
    pub gds: String,
    /// Output Format. One of ("json", "yaml", "toml")
    pub fmt: String,
    /// Output File
    pub out: String,
    /// Verbose Output Mode
    pub verbose: bool,
}


/// Core implementation, converting an on-disk GDSII file to an on-disk markup file. 
pub fn to_markup(options: &ToMarkupOptions) -> Result<(), Box<dyn Error>> {
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

/// # Conversion from Markup to GDSII Options
pub struct FromMarkupOptions {
    /// GDS Output File
    pub gds: String,
    /// Input Format. One of ("json", "yaml", "toml")
    pub fmt: String,
    /// Input Markup File
    pub inp: String,
    /// Verbose Output Mode
    pub verbose: bool,
}

/// Core implementation, converting an on-disk markup file to an on-disk GDSII file. 
pub fn from_markup(options: &FromMarkupOptions) -> Result<(), Box<dyn Error>> {
    // Get the target format
    let fmt: SerializationFormat = parse_format(&options.fmt)?;

    // Load a [`GdsLibrary`] in that format from file
    let gds_library: gds21::GdsLibrary = fmt.open(&options.inp)?;

    // Save it to GDSII's binary on-disk format
    gds_library.save(&options.gds)?;

    if options.verbose {
        println!("wrote {:?}", &options.gds);
    }
    Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;

    // Run the golden-file test for format (string) `fmtstr`
    fn test_to_fmt(fmtstr: &str) {
        // The golden file was created by running the program:
        // $ cargo run --bin gds2ser -- \
        //      -i resources/sky130_fd_sc_hd__dfxtp_1.gds \
        //      -o resources/sky130_fd_sc_hd__dfxtp_1.golden.json \
        //      -f json

        let output_path = scratch(&format!("sky130_fd_sc_hd__dfxtp_1.test_output.gds.{}", fmtstr));
        let golden_output_path =
            resource(&format!("sky130_fd_sc_hd__dfxtp_1.golden.gds.{}", fmtstr));

        let options = ToMarkupOptions {
            gds: resource("sky130_fd_sc_hd__dfxtp_1.gds"),
            out: output_path.clone(),
            fmt: fmtstr.to_string(),
            verbose: true,
        };

        // Run the main function, producing file `output_path`
        let result = to_markup(&options);

        // Un-comment to update the golden file.
        // std::fs::copy(&output_path, &golden_output_path).unwrap();

        // Check that `_main` succeeded, and compare the binary data it wrote to disk.
        assert!(result.is_ok());
        let bytes = std::fs::read(&output_path).unwrap();
        let golden_bytes = std::fs::read(&golden_output_path).unwrap();
        assert_eq!(golden_bytes, bytes);
    }

    #[test]
    fn to_golden_json() {
        test_to_fmt("json");
    }
    #[test]
    fn to_golden_yaml() {
        test_to_fmt("yaml");
    }
    #[test]
    #[ignore] // https://github.com/dan-fritchman/Layout21/issues/33
    fn to_golden_toml() {
        test_to_fmt("toml");
    }

    // Run the golden-file test for format (string) `fmtstr`
    fn test_from_fmt(fmtstr: &str) {
        let golden_input_path =resource(&format!("sky130_fd_sc_hd__dfxtp_1.golden.gds.{}", fmtstr));
        let test_output_path = scratch(&format!("sky130_fd_sc_hd__dfxtp_1.test_output.{}.gds", fmtstr));

        let options = FromMarkupOptions {
            inp: golden_input_path.clone(),
            gds: test_output_path.clone(),
            fmt: fmtstr.to_string(),
            verbose: true,
        };

        // Run the main function, producing file `test_output_path`
        let result = from_markup(&options);

        // Un-comment to update the golden file.
        // std::fs::copy(&test_output_path, &golden_input_path).unwrap();

        // Check that `_main` succeeded, and compare the binary data it wrote to disk.
        assert!(result.is_ok());
        let bytes = std::fs::read(&test_output_path).unwrap();
        let golden_output_path = resource("sky130_fd_sc_hd__dfxtp_1.gds");
        let golden_bytes = std::fs::read(&golden_output_path).unwrap();
        assert_eq!(golden_bytes, bytes);
    }

    #[test]
    fn from_golden_json() {
        test_from_fmt("json");
    }
    #[test]
    fn from_golden_yaml() {
        test_from_fmt("yaml");
    }
    #[test]
    #[ignore] // https://github.com/dan-fritchman/Layout21/issues/33
    fn from_golden_toml() {
        test_from_fmt("toml");
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
