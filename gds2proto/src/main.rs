use clap::Parser;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use prost::Message;

use gds21::GdsLibrary;
use layout21raw::gds::GdsImporter;
use layout21raw::proto::ProtoExporter;
use layout21raw::LayoutResult;
use layout21protos::Library;

#[derive(Parser)]
struct ProgramOptions {
    #[clap(short = 'i', long, default_value = "")]
    gds: String,
    #[clap(short = 'o', long, default_value = "")]
    proto: String,
    #[clap(short, long)]
    verbose: bool,
}

fn main() {
    let options = ProgramOptions::parse();

    let input_gds_path = Path::new(&options.gds);
    let mut gds_file = match File::open(&input_gds_path) {
        Err(err) => panic!("Couldn't open file {}: {}", input_gds_path.display(), err),
        Ok(file) => file,
    };

    let mut gds_bytes = Vec::new();
    gds_file
        .read_to_end(&mut gds_bytes)
        .expect(&format!("Couldn't read file {}", input_gds_path.display()));
    if options.verbose {
        println!("read: {:?}", input_gds_path);
    }

    let gds_library = match gds21::GdsLibrary::from_bytes(gds_bytes) {
        Err(err) => panic!("Couldn't interpret GDS data: {}", err),
        Ok(lib) => lib,
    };

    if options.verbose {
        let gds_stats = gds_library.stats();
        println!("{:?}", gds_stats);
    }

    let library = match GdsImporter::import(&gds_library, None) {
        Err(err) => panic!("Couldn't import GDS library: {:?}", err),
        Ok(layout) => layout,
    };

    let proto_library = match ProtoExporter::export(&library) {
        Err(err) => panic!("Couldn't create protobuf library: {}", err),
        Ok(lib) => lib,
    };

    let mut proto_bytes = Vec::<u8>::new();
    proto_library.encode(&mut proto_bytes).unwrap();

    let output_proto_path = Path::new(&options.proto);
    let mut proto_file = match File::create(&output_proto_path) {
        Err(err) => panic!("Couldn't open file {}: {}", output_proto_path.display(), err),
        Ok(file) => file,
    };

    proto_file
        .write_all(&proto_bytes)
        .expect(&format!("Couldn't write data to {}", output_proto_path.display()));
    if options.verbose {
        println!("wrote: {:?}", output_proto_path.display());
    }
}
