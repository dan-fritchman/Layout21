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
    layers.add(Layer::new(236, "boundary").add_pairs(&[(0, LayerPurpose::Outline)])?);

    // Create metal layers
    let metal_purps = [(20, LayerPurpose::Drawing), (5, LayerPurpose::Label)];
    layers.add(Layer::new(68, "met1").add_pairs(&metal_purps)?);
    layers.add(Layer::new(69, "met2").add_pairs(&metal_purps)?);
    layers.add(Layer::new(70, "met3").add_pairs(&metal_purps)?);
    layers.add(Layer::new(71, "met4").add_pairs(&metal_purps)?);
    // Create the via layers
    // Note that while these use the same *GDS* layer number, they are separate [Layer] objects here.
    // (Because, well, they really are.)
    let via_purps = [(44, LayerPurpose::Drawing)];
    layers.add(Layer::new(67, "mcon").add_pairs(&via_purps)?);
    layers.add(Layer::new(68, "via").add_pairs(&via_purps)?);
    layers.add(Layer::new(69, "via2").add_pairs(&via_purps)?);
    layers.add(Layer::new(70, "via3").add_pairs(&via_purps)?);
    layers.add(Layer::new(71, "via4").add_pairs(&via_purps)?);

    // Add a base-layer
    layers.add(
        Layer::new(64, "nwell")
            .add_pairs(&[(44, LayerPurpose::Drawing), (5, LayerPurpose::Label)])?,
    );
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
    // Read a GDS file
    let samp = resource("dff1_lib.gds");
    let gds = gds::gds21::GdsLibrary::load(&samp)?;

    // Convert to Layout21::Raw
    let lib = gds::GdsImporter::import(&gds, None)?;
    assert_eq!(lib.name, "dff1_lib");
    assert_eq!(lib.cells.len(), 1);

    // Get the first (and only) cell
    let cell = lib.cells.first().unwrap().clone();
    let cell = cell.read()?;
    assert_eq!(cell.name, "dff1");

    // Convert to ProtoBuf
    let p = proto::ProtoExporter::export(&lib)?;
    assert_eq!(p.domain, "dff1_lib");
    let p2 = proto::ProtoExporter::export(&lib)?;
    assert_eq!(p, p2);

    // And compare against the golden version
    let p2 = proto::proto::open(&resource("dff1_lib.bin")).unwrap();
    assert_eq!(p, p2);
    Ok(())
}
