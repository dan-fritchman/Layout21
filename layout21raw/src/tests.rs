//!
//! # layout21::raw unit tests
//!

use super::*;

#[test]
fn point() {
    let p = Point::new(1, 2);
    assert_eq!(p.x, 1);
    assert_eq!(p.y, 2);
}

/// Create a [Layers] used by a number of tests
pub fn layers() -> LayoutResult<Layers> {
    let mut layers = Layers::default();

    // Add the outline/ boundary layer
    layers.add(Layer::from_pairs(236, &[(0, LayerPurpose::Outline)])?);

    // Create metal layers
    let metal_purps = [(20, LayerPurpose::Drawing), (5, LayerPurpose::Label)];
    let mut l = Layer::new(68, "met1");
    l.add_pairs(&metal_purps)?;
    layers.add(l);
    let mut l = Layer::new(69, "met2");
    l.add_pairs(&metal_purps)?;
    layers.add(l);
    let mut l = Layer::new(70, "met3");
    l.add_pairs(&metal_purps)?;
    layers.add(l);
    let mut l = Layer::new(71, "met4");
    l.add_pairs(&metal_purps)?;
    layers.add(l);
    // Create the via layers
    // Note that while these use the same *GDS* layer number, they are separate [Layer] objects here.
    // (Because, well, they really are.)
    let via_purps = [(44, LayerPurpose::Drawing)];
    let mut l = Layer::new(67, "mcon");
    l.add_pairs(&via_purps)?;
    layers.add(l);
    let mut l = Layer::new(68, "via");
    l.add_pairs(&via_purps)?;
    layers.add(l);
    let mut l = Layer::new(69, "via2");
    l.add_pairs(&via_purps)?;
    layers.add(l);
    let mut l = Layer::new(70, "via3");
    l.add_pairs(&via_purps)?;
    layers.add(l);
    let mut l = Layer::new(71, "via4");
    l.add_pairs(&via_purps)?;
    layers.add(l);
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
/// Grab the full path of resource-file `fname`
fn resource(fname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname)
}
/// Take a trip through GDSII -> Layout21::Raw -> ProtoBuf
#[cfg(all(feature = "gds", feature = "proto"))]
#[test]
fn test_gds_to_proto1() -> LayoutResult<()> {
    let samp = resource("sample1.gds");
    // let samp = "/Users/dan/dev/ucb/osci/OsciBear/gds/user_analog_project_wrapper.gds";
    let gds = gds::gds21::GdsLibrary::load(&samp)?;
    let lib = gds::GdsImporter::import(&gds, None)?;
    assert_eq!(lib.name, "dff1_lib");
    assert_eq!(lib.cells.len(), 1);
    let cell = &lib.cells.values().next().unwrap();
    assert_eq!(cell.name, "dff1");
    let p = proto::ProtoExporter::export(&lib)?;
    assert_eq!(p.domain, "dff1_lib");
    proto::proto::save(&p, &resource("something.bin")).unwrap();
    let p2 = proto::proto::open(&resource("something.bin")).unwrap();
    assert_eq!(p, p2);
    Ok(())
}
