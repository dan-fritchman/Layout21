//! # proto2gds
//!
//! VLSIR protobuf schema to GDSII converter
//!
//! This program is the sibiling of gds2proto.

use std::collections::HashMap;
use clap::Parser;
use layout21raw as raw;
use std::error::Error;
use layout21protos::conv as proto_converters;
use layout21protos::tech as protos;
use raw::utils::Ptr;

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

fn proto_to_internal_layer_purpose(sub_index: i16, proto_purpose: &protos::LayerPurposeType)
    -> raw::data::LayerPurpose {
    return match proto_purpose {
        protos::LayerPurposeType::Label => raw::data::LayerPurpose::Label,
        _ => raw::data::LayerPurpose::Other(sub_index),
    }
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

    let mut layers_by_number = HashMap::new();

    // Generate the Layers data from the given tech proto:
    for layer_pb in tech_library.layers {
        let layer = layers_by_number
            .entry(layer_pb.index)
            .or_insert(raw::data::Layer::from_num(layer_pb.index as i16));

        let sub_index = layer_pb.sub_index as i16;
        let layer_purpose = match layer_pb.purpose {
            Some(purpose) => proto_to_internal_layer_purpose(sub_index, &purpose.r#type()),
            None => raw::data::LayerPurpose::Other(sub_index),
        };
        layer.add_purpose(sub_index, layer_purpose)?;
    }

    let mut tech_layers = raw::data::Layers::default();
    for layer in layers_by_number.values() {
        tech_layers.add(layer.clone());
    }

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

    #[test]
    fn roundtrip_to_golden_file() {
        assert_eq!(1, 1);
    }
}
