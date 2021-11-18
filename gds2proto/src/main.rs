//!
//! # Gds to Proto CLI
//!

use clap::Parser;
use std::error::Error;

use layout21raw as raw;

#[derive(Parser)]
struct ProgramOptions {
    #[clap(short = 'i', long, default_value = "")]
    gds: String,
    #[clap(short = 'o', long, default_value = "")]
    proto: String,
    #[clap(short, long)]
    verbose: bool,
}

///
/// # Gds to Proto CLI
///
fn main() -> Result<(), Box<dyn Error>> {
    let options = ProgramOptions::parse();
    _main(&options)
}

fn _main(options: &ProgramOptions) -> Result<(), Box<dyn Error>> {
    // str (filename) => GdsLibrary => raw::Library => proto::Library => (save) Result<()>

    // Load GDS file to [GdsLibrary]
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
        Err(err) => panic!("Couldn't import GDS library: {:?}", err),
        Ok(layout) => layout,
    };

    // raw::Library => proto::Library
    let proto_library = match library.to_proto() {
        Err(err) => panic!("Couldn't create protobuf library: {}", err),
        Ok(lib) => lib,
    };

    // Save to disk
    use layout21protos::ProtoFile;
    proto_library.save(&options.proto)?;

    if options.verbose {
        println!("wrote: {:?}", &options.proto);
    }

    Ok(())
}

#[test]
fn test1() {
    // let options = ProgramOptions { /* create via literal, rather than via CLI parsing */ };
    // _main(&options)?;
    // assert!(/* stuff about what happened */);
    todo!()
}
