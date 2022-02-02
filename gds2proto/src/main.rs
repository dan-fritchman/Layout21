use clap::Parser;
use layout21raw as raw;
use std::error::Error;

#[derive(Parser)]
struct ProgramOptions {
    #[clap(short = 'i', long, default_value = "")]
    gds: String,
    #[clap(short = 'o', long, default_value = "")]
    proto: String,
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
    use tempfile::tempdir;

    #[test]
    fn roundtrip_to_golden_file() {
        let golden_input_path = resource("sky130_fd_sc_hd__dfxtp_1.gds");
        let golden_output_path = resource("sky130_fd_sc_hd__dfxtp_1.pb");
        let golden_bytes = match std::fs::read(&golden_output_path) {
            Ok(bytes) => bytes,
            Err(_err) => panic!("Could not read golden output file"),
        };

        let output_dir = tempdir().expect("Could not create temp dir");
        // jfc
        let output_path = output_dir
            .path()
            .join("gds2proto_test_output.pb")
            .into_os_string()
            .into_string()
            .unwrap();

        let options = ProgramOptions {
            gds: golden_input_path,
            proto: output_path.clone(),
            verbose: true,
        };

        assert_eq!((), _main(&options).unwrap());

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
