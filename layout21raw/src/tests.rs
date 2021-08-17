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
    let lib = GdsImporter::import(&gds, None)?;
    assert_eq!(lib.name, "lib1");
    assert_eq!(lib.cells.len(), 1);
    let cell = &lib.cells.values().next().unwrap();
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
    let lib = GdsImporter::import(&gds, None)?;
    assert_eq!(lib.name, "dff1_lib");
    assert_eq!(lib.cells.len(), 1);
    let cell = &lib.cells.values().next().unwrap();
    assert_eq!(cell.name, "dff1");
    let p = ProtoExporter::export(&lib)?;
    assert_eq!(p.domain, "dff1_lib");
    layout21protos::save(&p, &resource("something.bin")).unwrap();
    let p2 = layout21protos::open(&resource("something.bin")).unwrap();
    assert_eq!(p, p2);
    Ok(())
}
#[test]
fn proto1() -> LayoutResult<()> {
    // Round-trip through Layout21::Raw -> ProtoBuf -> Layout21::Raw
    let mut lib = Library::new("prt_lib", Units::Nano);
    let (layer, purpose) = lib.layers.get_or_insert(0, 0)?;
    let c1 = lib.cells.insert(Cell {
        name: "prt_cell".into(),
        elems: vec![
            Element {
                net: Some("prt_rect_net".to_string()),
                layer,
                purpose: purpose.clone(),
                inner: Shape::Rect {
                    p0: Point::default(),
                    p1: Point::default(),
                },
            },
            Element {
                net: Some("prt_poly_net".to_string()),
                layer,
                purpose: purpose.clone(),
                inner: Shape::Poly {
                    pts: vec![Point::default(), Point::default(), Point::default()],
                },
            },
            Element {
                net: Some("prt_path_net".to_string()),
                layer,
                purpose: purpose.clone(),
                inner: Shape::Path {
                    width: 5,
                    pts: vec![Point::default(), Point::default(), Point::default()],
                },
            },
        ],
        insts: Vec::new(),
        annotations: vec![TextElement {
            loc: Point::default(),
            string: "prt_text".into(),
        }],
    });
    lib.cells.insert(Cell {
        name: "prt_cell_with_inst".into(),
        elems: Vec::new(),
        insts: vec![Instance {
            inst_name: "prt_inst".into(),
            p0: Point::new(5, 5),
            cell: c1,
            reflect: false,
            angle: None,
        }],
        annotations: vec![TextElement {
            loc: Point::new(11, 11),
            string: "prt_more_text".into(),
        }],
    });
    let p = lib.to_proto()?;
    let lib2 = proto::ProtoImporter::import(&p, None)?;
    assert_eq!(lib.name, lib2.name);
    assert_eq!(lib.units, lib2.units);
    assert_eq!(lib.cells.len(), lib2.cells.len());
    assert_eq!(lib.layers.nums.len(), lib2.layers.nums.len());
    Ok(())
}
