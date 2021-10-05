//!
//! # Layout Utilities Protobuf Definitions
//!

// These are used by the macro-expanded code
#[allow(unused_imports)]
use prost::Message;
#[allow(unused_imports)]
use serde::{Deserialize, Serialize};

// Include the prost-expanded proto-file content
include!(concat!(env!("OUT_DIR"), "/vlsir.utils.rs"));

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

    #[test]
    fn qualname() {
        let x = QualifiedName {
            domain: "layout.com".into(),
            name: "layout".into(),
        };
        assert_eq!(x.domain, "layout.com");
        assert_eq!(x.name, "layout");

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: QualifiedName = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn reference_external() {
        // A good example of where proto+rust get uglier than we'd like
        // Note the `oneof` fields inside message `Reference` are placed in a `mod` named (lower-case) `reference`
        let r = Reference {
            to: Some(reference::To::External(QualifiedName {
                domain: "cell_domain".into(),
                name: "cell_name".into(),
            })),
        };
        match r.to {
            Some(reference::To::External(ref qn)) => {
                assert_eq!(qn.domain, "cell_domain");
                assert_eq!(qn.name, "cell_name");
            }
            _ => assert!(false),
        }
        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&r);
        let rt: Reference = from_bytes(&bytes).unwrap();
        assert_eq!(r, rt);
    }
    #[test]
    fn reference_local() {
        let r = Reference {
            to: Some(reference::To::Local("here".into())),
        };
        match r.to {
            Some(reference::To::Local(ref name)) => {
                assert_eq!(name, "here");
            }
            _ => assert!(false),
        }
        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&r);
        let rt: Reference = from_bytes(&bytes).unwrap();
        assert_eq!(r, rt);
    }
    #[test]
    fn reference_none() {
        // Test a reference to `None`; essentially the null Instance-pointer
        let r = Reference { to: None };
        match r.to {
            None => (),
            _ => assert!(false),
        }
        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&r);
        let rt: Reference = from_bytes(&bytes).unwrap();
        assert_eq!(r, rt);
    }
}
