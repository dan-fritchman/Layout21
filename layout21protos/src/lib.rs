//!
//! # Protobuf Definitions
//!

// Std-Lib Imports
use std::convert::TryFrom;
use std::error::Error;
use std::fs::File;
use std::io::{BufWriter, Read, Write};

// These are used by the macro-expanded code
#[allow(unused_imports)]
use prost::Message;
#[allow(unused_imports)]
use serde::{Deserialize, Serialize};

// Include the prost-expanded proto-file content
include!(concat!(env!("OUT_DIR"), "/vlsirlol.utils.rs"));
include!(concat!(env!("OUT_DIR"), "/vlsirlol.raw.rs"));
include!(concat!(env!("OUT_DIR"), "/vlsirlol.abstrakt.rs"));

// Define a few additional helper functions
impl Point {
    /// Create a new [Point] at location (x,y)
    pub fn new(x: i64, y: i64) -> Self {
        Self { x, y }
    }
}
impl Layer {
    /// Create a new [Layer] with values `number` and `purpose`
    pub fn new(number: i64, purpose: i64) -> Self {
        Self { number, purpose }
    }
}

/// Encode into a newly-allocated byte-[Vec].
/// Wraps [prost::Message::encode], which is likely more efficent
/// for pre-allocated buffers.
pub fn to_bytes<T: Message + Sized + Default>(data: &T) -> Vec<u8> {
    let mut buf = Vec::<u8>::with_capacity(data.encoded_len());
    data.encode(&mut buf).unwrap();
    buf
}
/// Decode from byte array/vector
///
/// Wraps [prost::Message::decode], adding support for some input types,
/// notably including `&Vec<u8>` as returned from `to_bytes`.
/// Using [prost::Message::decode] directly instead generally
/// works, but requires casting to slice via [Vec::as_slice].
///
pub fn from_bytes<T: Message + Sized + Default>(bytes: &[u8]) -> Result<T, prost::DecodeError> {
    T::decode(bytes)
}
impl TryFrom<&[u8]> for Library {
    type Error = prost::DecodeError;
    /// Decode from byte array/vector
    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        Library::decode(bytes)
    }
}
/// Open from file `fname`
pub fn open<T: Message + Sized + Default>(fname: &str) -> Result<T, Box<dyn Error>> {
    let mut file = File::open(&fname)?;
    let mut buf = Vec::<u8>::new();
    file.read_to_end(buf.as_mut())?;
    let res = T::decode(buf.as_ref())?;
    Ok(res)
}
/// Save to file `fname`
pub fn save<T: Message + Sized + Default>(data: &T, fname: &str) -> Result<(), Box<dyn Error>> {
    let mut file = BufWriter::new(File::create(fname)?);
    file.write_all(&to_bytes(data))?;
    Ok(())
}

/// Trait for reading and writing binary proto-format data to/from files
///
/// Includes default-implemented `open` and `save` methods,
/// allowing empty implementations per type.
///
/// Both methods are also exposed as public module-level functions,
/// allowing usage with types that do not implement `ProtoFile`,
/// but do implement its prerequisites.
pub trait ProtoFile: Message + Sized + Default {
    /// Open from file `fname`
    fn open(fname: &str) -> Result<Self, Box<dyn Error>> {
        open(fname)
    }
    /// Save to file `fname`
    fn save(&self, fname: &str) -> Result<(), Box<dyn Error>> {
        save(self, fname)
    }
}
impl ProtoFile for Library {}

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
    #[test]
    fn point() {
        let x = Point { x: 5, y: 6 };
        assert_eq!(x.x, 5);
        assert_eq!(x.y, 6);

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Point = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn layer() {
        let x = Layer {
            number: 255,
            purpose: 1,
        };
        assert_eq!(x.number, 255);
        assert_eq!(x.purpose, 1);

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Layer = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
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
    fn rectangle() {
        let x = Rectangle {
            net: "rect_net".into(),
            lower_left: Some(Point { x: 0, y: 0 }),
            width: 11,
            height: 12,
        };
        assert_eq!(x.net, "rect_net");
        assert_eq!(x.width, 11);
        assert_eq!(x.height, 12);
        assert_eq!(x.lower_left, Some(Point { x: 0, y: 0 }),);

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Rectangle = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn polygon() {
        let x = Polygon {
            net: "polygon_net".into(),
            // A right triangle
            vertices: vec![Point::new(0, 0), Point::new(1, 0), Point::new(1, 1)],
        };
        assert_eq!(x.net, "polygon_net");
        assert_eq!(
            x.vertices,
            vec![Point::new(0, 0), Point::new(1, 0), Point::new(1, 1)]
        );

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Polygon = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn path() {
        let x = Path {
            net: "path_net".into(),
            width: 50,
            points: vec![Point::new(0, 0), Point::new(1, 0), Point::new(1, 1)],
        };
        assert_eq!(x.net, "path_net");
        assert_eq!(x.width, 50);
        assert_eq!(
            x.points,
            vec![Point::new(0, 0), Point::new(1, 0), Point::new(1, 1)]
        );

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Path = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn text() {
        let x = TextElement {
            string: "text_elem".into(),
            loc: Some(Point::new(100, 100)),
        };
        assert_eq!(x.string, "text_elem");
        assert_eq!(x.loc, Some(Point::new(100, 100)),);

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: TextElement = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn layer_shapes() {
        let x = LayerShapes {
            layer: Some(Layer {
                number: 255,
                purpose: 11,
            }),
            rectangles: Vec::new(),
            polygons: Vec::new(),
            paths: Vec::new(),
        };
        assert_eq!(
            x.layer,
            Some(Layer {
                number: 255,
                purpose: 11
            })
        );
        assert_eq!(x.rectangles, vec![]);
        assert_eq!(x.polygons, vec![]);
        assert_eq!(x.paths, vec![]);

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: LayerShapes = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn layer_shapes2() {
        let x = LayerShapes {
            layer: Some(Layer {
                number: 255,
                purpose: 11,
            }),
            rectangles: vec![Rectangle {
                net: "rect_net".into(),
                lower_left: Some(Point { x: 0, y: 0 }),
                width: 11,
                height: 12,
            }],
            polygons: vec![Polygon {
                net: "polygon_net".into(),
                // A right triangle
                vertices: vec![Point::new(0, 0), Point::new(1, 0), Point::new(1, 1)],
            }],
            paths: vec![Path {
                net: "path_net".into(),
                width: 50,
                points: vec![Point::new(0, 0), Point::new(1, 0), Point::new(1, 1)],
            }],
        };
        assert_eq!(
            x.layer,
            Some(Layer {
                number: 255,
                purpose: 11
            })
        );
        assert_eq!(
            x.rectangles,
            vec![Rectangle {
                net: "rect_net".into(),
                lower_left: Some(Point { x: 0, y: 0 }),
                width: 11,
                height: 12,
            }]
        );
        assert_eq!(
            x.polygons,
            vec![Polygon {
                net: "polygon_net".into(),
                // A right triangle
                vertices: vec![Point::new(0, 0), Point::new(1, 0), Point::new(1, 1)],
            }]
        );
        assert_eq!(
            x.paths,
            vec![Path {
                net: "path_net".into(),
                width: 50,
                points: vec![Point::new(0, 0), Point::new(1, 0), Point::new(1, 1)],
            }]
        );
        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: LayerShapes = from_bytes(&bytes).unwrap();
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
    fn units() {
        // These don't implement [Message], so no round-tripping allowed
        // Just check we can create them
        let _r = Units::Micro;
        let _r = Units::Nano;
        let _r = Units::Angstrom;
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
    #[test]
    fn instance() {
        let x = Instance {
            name: "inst_name".into(),
            cell: Some(Reference {
                to: Some(reference::To::External(QualifiedName {
                    domain: "cell_domain".into(),
                    name: "cell_name".into(),
                })),
            }),
            reflect_vert: true,
            rotation_clockwise_degrees: 0,
            origin_location: Some(Point::new(0, 0)),
        };
        assert_eq!(x.name, "inst_name");
        assert_eq!(
            x.cell,
            Some(Reference {
                to: Some(reference::To::External(QualifiedName {
                    domain: "cell_domain".into(),
                    name: "cell_name".into(),
                })),
            })
        );
        assert_eq!(x.reflect_vert, true);
        assert_eq!(x.rotation_clockwise_degrees, 0);
        assert_eq!(x.origin_location, Some(Point::new(0, 0)));

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Instance = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn cell() {
        let x = Cell {
            name: "cell_name".into(),
            shapes: vec![],
            instances: vec![],
            annotations: vec![],
            author: "author".into(),
            copyright: "copyright".into(),
        };
        assert_eq!(x.name, "cell_name");
        assert_eq!(x.shapes, vec![]);
        assert_eq!(x.instances, vec![]);
        assert_eq!(x.annotations, vec![]);
        assert_eq!(x.author, "author");
        assert_eq!(x.copyright, "copyright");

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Cell = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn library() {
        let r = Library {
            domain: "libdomain".into(),
            units: Units::Angstrom.into(),
            cells: Vec::new(),
            abstracts: Vec::new(),
        };
        assert_eq!(r.domain, "libdomain");
        assert_eq!(r.units, Units::Angstrom.into());
        assert_eq!(r.cells, vec![]);
        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&r);
        let rt: Library = from_bytes(&bytes).unwrap();
        assert_eq!(r, rt);
    }
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
            outline: Some(Rectangle {
                net: "".into(),
                lower_left: Some(Point { x: 0, y: 0 }),
                width: 99,
                height: 11,
            }),
            ports: vec![AbstractPort::default()],
            blockages: vec![LayerShapes::default()],
            author: "author".into(),
            copyright: "copyright".into(),
        };

        assert_eq!(r.name, "abs");
        assert_eq!(
            r.outline,
            Some(Rectangle {
                net: "".into(),
                lower_left: Some(Point { x: 0, y: 0 }),
                width: 99,
                height: 11,
            }),
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
