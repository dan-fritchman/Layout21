//!
//! # layout21::raw unit tests
//!

use super::*;
use crate::utils::Ptr;

#[test]
fn point() {
    let p = Point::new(1, 2);
    assert_eq!(p.x, 1);
    assert_eq!(p.y, 2);
}

/// Create a [Layers] used by a number of tests
pub fn layers() -> LayoutResult<Layers> {
    use LayerPurpose::{Drawing, Label, Outline};
    let mut layers = Layers::default();

    // Add the outline/ boundary layer
    layers.add(Layer::new(236, "boundary").add_pairs(&[(0, Outline)])?);

    // Create metal layers
    let metal_purps = [(20, Drawing), (5, Label)];
    layers.add(Layer::new(68, "met1").add_pairs(&metal_purps)?);
    layers.add(Layer::new(69, "met2").add_pairs(&metal_purps)?);
    layers.add(Layer::new(70, "met3").add_pairs(&metal_purps)?);
    layers.add(Layer::new(71, "met4").add_pairs(&metal_purps)?);

    // Create the via layers
    // Note that while these use the same *GDS* layer number, they are separate [Layer] objects here.
    // (Because, well, they really are.)
    let via_purps = [(44, Drawing)];
    layers.add(Layer::new(67, "mcon").add_pairs(&via_purps)?);
    layers.add(Layer::new(68, "via").add_pairs(&via_purps)?);
    layers.add(Layer::new(69, "via2").add_pairs(&via_purps)?);
    layers.add(Layer::new(70, "via3").add_pairs(&via_purps)?);
    layers.add(Layer::new(71, "via4").add_pairs(&via_purps)?);

    // Add base-layers
    // FIXME: check these datatype numbers!
    layers.add(Layer::new(64, "nwell").add_pairs(&[(20, Drawing), (5, Label)])?);
    layers.add(Layer::new(65, "diff").add_pairs(&[(20, Drawing), (5, Label)])?);
    layers.add(Layer::new(66, "poly").add_pairs(&[(20, Drawing), (5, Label)])?);
    layers.add(Layer::new(67, "li1").add_pairs(&[(20, Drawing), (5, Label)])?);

    Ok(layers)
}
#[test]
fn test_layers() -> LayoutResult<()> {
    // Test we can retrieve from the [Layers] each way
    let layers = layers()?;
    let l = layers.name("met1").unwrap();
    assert_eq!(l.name, Some("met1".into()));
    assert_eq!(l.layernum, 68);
    let l = layers.name("met2").unwrap();
    assert_eq!(l.name, Some("met2".into()));
    assert_eq!(l.layernum, 69);
    let l = layers.name("met3").unwrap();
    assert_eq!(l.name, Some("met3".into()));
    assert_eq!(l.layernum, 70);
    let l = layers.name("met4").unwrap();
    assert_eq!(l.name, Some("met4".into()));
    assert_eq!(l.layernum, 71);

    Ok(())
}
#[test]
fn polygon_builder() -> LayoutResult<()> {
    let mut b = PolygonBuilder::start_at((0, 0));

    // Create an "H" shape
    b.move_by(1, 0);
    b.move_by(0, 1);
    b.move_by(1, 0);
    b.move_by(0, -1);
    b.move_by(1, 0);
    b.move_by(0, 3);
    b.move_by(-1, 0);
    b.move_by(0, -1);
    b.move_by(-1, 0);
    b.move_by(0, 1);
    b.move_by(-1, 0);

    // Convert to a polygon
    let p = b.build();
    assert_eq!(
        p.points,
        vec![
            Point { x: 0, y: 0 },
            Point { x: 1, y: 0 },
            Point { x: 1, y: 1 },
            Point { x: 2, y: 1 },
            Point { x: 2, y: 0 },
            Point { x: 3, y: 0 },
            Point { x: 3, y: 3 },
            Point { x: 2, y: 3 },
            Point { x: 2, y: 2 },
            Point { x: 1, y: 2 },
            Point { x: 1, y: 3 },
            Point { x: 0, y: 3 },
        ]
    );

    Ok(())
}

/// Test [Layout] creation and adding a few elements
#[test]
fn create_layout() -> LayoutResult<()> {
    use LayerPurpose::Drawing;
    let layers = layers()?;
    let met1 = layers.keyname("met1").unwrap();
    let mut lay = Layout::new("test_layout_creation");
    lay.elems.push(Element::new(
        Some("vss"),
        met1,
        Drawing,
        Rect::new(Point::new(0, 0), Point::new(10, 10)),
    ));
    lay.elems.push(Element {
        net: Some("vdd".into()),
        layer: met1,
        purpose: Drawing,
        inner: Shape::Rect(Rect::new(Point::new(0, 10), Point::new(10, 10))),
    });
    lay.elems.push(Element {
        net: Some("something_else".into()),
        layer: met1,
        purpose: Drawing,
        inner: Shape::Polygon(Polygon::new(vec![
            Point::new(0, 10),
            Point::new(10, 10),
            Point::new(10, 0),
            Point::new(0, 0),
        ])),
    });

    // FIXME: more specific content checks

    Ok(())
}

/// Make a tutorial-grade inverter!
#[test]
#[cfg(all(feature = "gds", feature = "proto"))]
fn create_inverter() -> LayoutResult<()> {
    use LayerPurpose::Drawing;
    let layers = layers()?;

    let met1 = layers.keyname("met1").unwrap();
    let diff = layers.keyname("diff").unwrap();
    let poly = layers.keyname("poly").unwrap();
    let nwell = layers.keyname("nwell").unwrap();
    let li1 = layers.keyname("li1").unwrap();

    let mut layout = Layout::new("inv");

    layout.elems.push(Element {
        net: None,
        layer: diff,
        purpose: Drawing,
        inner: Rect::new(Point::new(0, 400), Point::new(500, 500)).into(),
    });
    layout.elems.push(Element {
        net: None,
        layer: diff,
        purpose: Drawing,
        inner: Rect::new(Point::new(0, 0), Point::new(500, 100)).into(),
    });
    layout.elems.push(Element {
        net: Some("VSS".into()),
        layer: met1,
        purpose: Drawing,
        inner: Rect::new(Point::new(-100, -100), Point::new(600, 100)).into(),
    });
    layout.elems.push(Element {
        net: Some("VDD".into()),
        layer: met1,
        purpose: Drawing,
        inner: Rect::new(Point::new(-100, 400), Point::new(600, 600)).into(),
    });
    layout.elems.push(Element {
        net: Some("inp".into()),
        layer: poly,
        purpose: Drawing,
        inner: Rect::new(Point::new(300, 0), Point::new(400, 600)).into(),
    });
    layout.elems.push(Element {
        net: Some("VDD".into()),
        layer: nwell,
        purpose: Drawing,
        inner: Rect::new(Point::new(-100, 300), Point::new(600, 600)).into(),
    });
    layout.elems.push(Element {
        net: Some("VDD".into()),
        layer: li1,
        purpose: Drawing,
        inner: Path::new(vec![Point::new(-100, 300), Point::new(600, 600)], 50).into(),
    });

    // FIXME: add the rest of the content!

    let mut lib = Library::new("invlib", Units::Nano, Some(Ptr::new(layers)));
    lib.cells.insert(layout);

    let gds = lib.to_gds()?;
    gds.save("invlib.gds")?;
    let proto = lib.to_proto()?;
    // FIXME: write that to disk too
    Ok(())
}

/// Take a trip through GDSII -> Layout21::Raw -> ProtoBuf
#[cfg(all(feature = "gds", feature = "proto"))]
#[test]
fn test_gds_to_proto1() -> LayoutResult<()> {
    use crate::{
        gds::gds21::GdsLibrary,
        proto::proto::{Library as ProtoLibrary, ProtoFile},
    };
    // Read a GDS file
    let samp = resource("dff1_lib.golden.gds");
    let gds = GdsLibrary::load(&samp)?;

    // Convert to Layout21::Raw
    let lib = Library::from_gds(&gds, None)?;
    assert_eq!(lib.name, "dff1_lib");
    assert_eq!(lib.cells.len(), 1);

    // Get the first (and only) cell
    let cell = lib.cells.first().unwrap().clone();
    let cell = cell.read()?;
    assert_eq!(cell.name, "dff1");

    // Convert to ProtoBuf
    let plib: ProtoLibrary = lib.to_proto()?;
    assert_eq!(plib.domain, "dff1_lib");

    // And compare against the golden version
    let plib2 = ProtoLibrary::open(&resource("dff1_lib.golden.vlsir.bin")).unwrap();
    assert_eq!(plib, plib2);
    Ok(())
}
/// Grab the full path of resource-file `fname`
fn resource(rname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
}
