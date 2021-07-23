//!
//! # Build Module
//!
//! Primarily expands protobuf definitions, adding a number of annotations.
//!

use prost_build;

fn main() {
    let mut config = prost_build::Config::new();
    // Add serde traits
    config.type_attribute(
        ".",
        "#[derive(serde_derive::Serialize, serde_derive::Deserialize)]",
    );
    // Add our custom annotations
    // config.type_attribute("example", "#[serde(tag = \"type\")]");
    // config.field_attribute("example", "#[serde(flatten)]");
    // And build!
    config
        .compile_protos(&["protos/raw.proto"], &["protos/"])
        .unwrap();
}
