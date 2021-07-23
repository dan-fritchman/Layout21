//!
//! # Protobuf Definitions
//!

// These are used by the macro-expanded code
#[allow(unused_imports)]
use prost::Message;
#[allow(unused_imports)]
use serde::{Deserialize, Serialize};

// Include the prost-expanded proto-file content
include!(concat!(env!("OUT_DIR"), "/layout21.raw.rs"));

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
    /// Encode into Byte-Vector
    fn to_bytes<T: Message + Sized + Default>(data: &T) -> Vec<u8> {
        let mut buf = Vec::<u8>::with_capacity(data.encoded_len());
        data.encode(&mut buf).unwrap();
        buf
    }
    /// Decode from byte array/vector
    fn from_bytes<T: Message + Sized + Default>(bytes: &[u8]) -> Result<T, prost::DecodeError> {
        T::decode(bytes)
    }
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
        assert_eq!(x.rectangles, []);
        assert_eq!(x.polygons, []);
        assert_eq!(x.paths, []);

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
    fn instance() {
        let x = Instance {
            name: "inst_name".into(),
            cell_name: Some(QualifiedName {
                domain: "cell_domain".into(),
                name: "cell_name".into(),
            }),
            rotation_clockwise_degrees: 0,
            lower_left: Some(Point::new(0, 0)),
        };
        assert_eq!(x.name, "inst_name");
        assert_eq!(
            x.cell_name,
            Some(QualifiedName {
                domain: "cell_domain".into(),
                name: "cell_name".into(),
            })
        );
        assert_eq!(x.rotation_clockwise_degrees, 0);
        assert_eq!(x.lower_left, Some(Point::new(0, 0)));

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Instance = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn cell() {
        let x = Cell {
            name: Some(QualifiedName {
                domain: "cell_domain".into(),
                name: "cell_name".into(),
            }),
            shapes: vec![],
            instances: vec![],
            author: "author".into(),
            copyright: "copyright".into(),
        };
        assert_eq!(
            x.name,
            Some(QualifiedName {
                domain: "cell_domain".into(),
                name: "cell_name".into(),
            })
        );
        assert_eq!(x.shapes, []);
        assert_eq!(x.instances, []);
        assert_eq!(x.author, "author");
        assert_eq!(x.copyright, "copyright");

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Cell = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
    #[test]
    fn cell2() {
        let x = Cell {
            name: Some(QualifiedName {
                domain: "cell_domain".into(),
                name: "cell_name".into(),
            }),
            shapes: vec![],
            instances: vec![],
            author: "author".into(),
            copyright: "copyright".into(),
        };
        assert_eq!(
            x.name,
            Some(QualifiedName {
                domain: "cell_domain".into(),
                name: "cell_name".into(),
            })
        );
        assert_eq!(x.shapes, []);
        assert_eq!(x.instances, []);
        assert_eq!(x.author, "author");
        assert_eq!(x.copyright, "copyright");

        // Protobuf Serialization Round-Trip
        let bytes = to_bytes(&x);
        let rt: Cell = from_bytes(&bytes).unwrap();
        assert_eq!(x, rt);
    }
}
