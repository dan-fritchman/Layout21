//!
//! # Multi-View Protobuf Definitions
//!

// These are used by the macro-expanded code
#[allow(unused_imports)]
use prost::Message;
#[allow(unused_imports)]
use serde::{Deserialize, Serialize};

// Local Imports
use crate::conv::ProtoFile;

// Include the prost-expanded proto-file content
include!(concat!(env!("OUT_DIR"), "/vlsir.views.rs"));

// Add the file-exchange trait for Libraries and Cells
impl ProtoFile for Library {}
impl ProtoFile for Cell {}

/// # Unit Tests
///
/// Primarily basic generation of each proto-expanded type,
/// with basic checking of its values.
///
/// These tests also serve as helpful examples of proto-compilation
/// idiosyncrasies, such as where objects are wrapped in Rust's [Option]s.
///
#[cfg(test)]
mod tests {
    use super::*;
    use crate::conv::{from_bytes, to_bytes};
    use crate::raw::*;

    #[test]
    fn abstrakt_port() {
        let r = AbstractPort {
            net: "abs_port_name".into(),
            shapes: vec![LayerShapes::default()],
        };
        assert_eq!(r.net, "abs_port_name");
        assert_eq!(
            r.shapes,
            vec![LayerShapes {
                layer: None,
                rectangles: vec![],
                polygons: vec![],
                paths: vec![],
            }]
        );
        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&r);
        let rt: AbstractPort = from_bytes(&bytes).unwrap();
        assert_eq!(r, rt);
    }
    #[test]
    fn abstrakt() {
        let r = Abstract {
            name: "abs".into(),
            outline: Some(Polygon {
                net: "".into(),
                vertices: vec![],
            }),
            ports: vec![AbstractPort::default()],
            blockages: vec![LayerShapes::default()],
        };

        assert_eq!(r.name, "abs");
        assert_eq!(
            r.outline,
            Some(Polygon {
                net: "".into(),
                vertices: vec![],
            })
        );
        assert_eq!(
            r.ports,
            vec![AbstractPort {
                net: "".into(),
                shapes: vec![],
            }]
        );
        assert_eq!(
            r.blockages,
            vec![LayerShapes {
                layer: None,
                rectangles: vec![],
                polygons: vec![],
                paths: vec![],
            }]
        );
        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&r);
        let rt = Abstract::decode(bytes.as_slice()).unwrap();
        assert_eq!(r, rt);
    }
}
