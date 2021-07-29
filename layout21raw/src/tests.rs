use super::*;

#[test]
fn point() {
    let p = Point::new(1, 2);
    assert_eq!(p.x, 1);
    assert_eq!(p.y, 2);
}
/// Import a GDS Cell with two polygons:
/// One assigned to a net, and the other not.
#[test]
fn gds_import1() -> LayoutResult<()> {
    use gds21::*;
    let gds = GdsLibrary {
        name: "lib1".into(),
        structs: vec![GdsStruct {
            name: "cell1".into(),
            elems: vec![
                GdsElement::GdsBoundary(GdsBoundary {
                    layer: 11,
                    datatype: 22,
                    xy: GdsPoint::vec(&[(0, 0), (2, 0), (2, 2), (0, 2), (0, 0)]),
                    ..Default::default()
                }),
                GdsElement::GdsTextElem(GdsTextElem {
                    string: "net1".into(),
                    layer: 11,    // Same layer as the boundary
                    texttype: 66, // Could be anything, for now
                    xy: GdsPoint::new(1, 1),
                    ..Default::default()
                }),
                GdsElement::GdsBoundary(GdsBoundary {
                    layer: 33,
                    datatype: 44,
                    xy: GdsPoint::vec(&[(10, 10), (12, 10), (12, 12), (10, 12), (10, 10)]),
                    ..Default::default()
                }),
                GdsElement::GdsTextElem(GdsTextElem {
                    string: "net1".into(),
                    layer: 44, // *Not* Same layer as the boundary
                    texttype: 66,
                    xy: GdsPoint::new(11, 11), // Intersects with the boundary
                    ..Default::default()
                }),
            ],
            ..Default::default()
        }],
        ..Default::default()
    };
    let lib = GdsImporter::import(gds)?;
    assert_eq!(lib.name, "lib1");
    let cell = &lib.cells[0];
    assert_eq!(cell.name, "cell1");
    let elem = &cell.elems[0];
    assert_eq!(elem.net, Some("net1".to_string()));
    let elem = &cell.elems[1];
    assert_eq!(elem.net, None);

    Ok(())
}
/// Grab the full path of resource-file `fname`
fn resource(fname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname)
}
/// Take a trip through GDSII -> Layout21::Raw -> ProtoBuf
#[test]
fn gds_sample1() -> LayoutResult<()> {
    let samp = resource("sample1.gds");
    // let samp = "/Users/dan/dev/ucb/osci/OsciBear/gds/user_analog_project_wrapper.gds";
    let gds = gds21::GdsLibrary::load(&samp)?;
    let lib = GdsImporter::import(gds)?;
    assert_eq!(lib.name, "dff1_lib");
    let cell = &lib.cells[0];
    assert_eq!(cell.name, "dff1");
    let p = ProtoConverter::convert(lib)?;
    assert!(p.name.is_some());
    proto::save(&p, &resource("something.bin")).unwrap();
    let p2 = proto::open(&resource("something.bin")).unwrap();
    assert_eq!(p, p2);
    Ok(())
}
