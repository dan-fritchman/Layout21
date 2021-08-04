use super::*;

/// Create a [Stack] used by a number of tests
fn stack() -> LayoutResult<Stack> {
    let metal_purps = [
        (20, raw::LayerPurpose::Drawing),
        (5, raw::LayerPurpose::Label),
    ];
    let via_purps = [(44, raw::LayerPurpose::Drawing)];
    let stack = Stack {
        units: Unit::Nano,
        boundary_layer: Some(raw::Layer::from_pairs(
            236,
            &[(0, raw::LayerPurpose::Outline)],
        )?),
        prim: PrimitiveLayer {
            pitches: (460, 3310).into(),
        },
        layers: vec![
            Layer {
                name: "met1".into(),
                entries: vec![
                    TrackSpec::gnd(490),
                    TrackSpec::pat(vec![TrackEntry::gap(230), TrackEntry::sig(140)], 7),
                    TrackSpec::gap(230),
                    TrackSpec::pwr(490),
                ],
                dir: Dir::Horiz,
                offset: (-245).into(),
                cutsize: (250).into(),
                overlap: (490).into(),
                raw: Some(raw::Layer::from_pairs(68, &metal_purps)?),
                flip: FlipMode::EveryOther,
                prim: PrimitiveMode::Partial,
            },
            Layer {
                name: "met2".into(),
                entries: vec![
                    TrackSpec::gnd(510),
                    TrackSpec::pat(vec![TrackEntry::gap(410), TrackEntry::sig(50)], 8),
                    TrackSpec::gap(410),
                    TrackSpec::pwr(510),
                ],
                dir: Dir::Vert,
                cutsize: (250).into(),
                offset: (-255).into(),
                overlap: (510).into(),
                raw: Some(raw::Layer::from_pairs(69, &metal_purps)?),
                flip: FlipMode::EveryOther,
                prim: PrimitiveMode::None,
            },
            Layer {
                name: "met3".into(),
                entries: vec![
                    TrackSpec::gnd(490),
                    TrackSpec::pat(vec![TrackEntry::gap(230), TrackEntry::sig(140)], 7),
                    TrackSpec::gap(230),
                    TrackSpec::pwr(490),
                ],
                dir: Dir::Horiz,
                offset: (-245).into(),
                cutsize: (250).into(),
                overlap: (490).into(),
                raw: Some(raw::Layer::from_pairs(70, &metal_purps)?),
                flip: FlipMode::EveryOther,
                prim: PrimitiveMode::None,
            },
            Layer {
                name: "met4".into(),
                entries: vec![
                    TrackSpec::gnd(510),
                    TrackSpec::pat(vec![TrackEntry::gap(410), TrackEntry::sig(50)], 8),
                    TrackSpec::gap(410),
                    TrackSpec::pwr(510),
                ],
                dir: Dir::Vert,
                cutsize: (250).into(),
                offset: (-255).into(),
                overlap: (510).into(),
                raw: Some(raw::Layer::from_pairs(71, &metal_purps)?),
                flip: FlipMode::EveryOther,
                prim: PrimitiveMode::None,
            },
        ],
        vias: vec![
            ViaLayer {
                name: "mcon".into(),
                between: (0, 1),
                size: (240, 240).into(),
                raw: Some(raw::Layer::from_pairs(67, &via_purps)?),
            },
            ViaLayer {
                name: "via1".into(),
                between: (1, 2),
                size: (240, 240).into(),
                raw: Some(raw::Layer::from_pairs(68, &via_purps)?),
            },
            ViaLayer {
                name: "via2".into(),
                between: (2, 3),
                size: (240, 240).into(),
                raw: Some(raw::Layer::from_pairs(69, &via_purps)?),
            },
            ViaLayer {
                name: "via3".into(),
                between: (3, 4),
                size: (240, 240).into(),
                raw: Some(raw::Layer::from_pairs(70, &via_purps)?),
            },
        ],
    };
    Ok(stack)
}
/// Run the test-stack through validation
#[test]
fn validate_stack() -> LayoutResult<()> {
    let s = stack()?;
    validate::StackValidator::validate(s)?;
    Ok(())
}
/// Create a cell
#[test]
fn create_cell() -> Result<(), LayoutError> {
    Cell {
        name: "HereGoes".into(),
        top_layer: 3,
        outline: Outline::rect(50, 5)?,
        instances: Vec::new(),
        assignments: vec![Assign {
            net: "clk".into(),
            at: TrackIntersection {
                layer: 1,
                track: 0,
                at: 1,
                relz: RelZ::Above,
            },
        }],
        cuts: Vec::new(),
    };
    Ok(())
}
/// Create a library
#[test]
fn create_lib1() -> Result<(), LayoutError> {
    let mut lib = Library::new("lib1");

    lib.cells.insert(Cell {
        name: "HereGoes".into(),
        top_layer: 2,
        outline: Outline::rect(50, 5)?,
        instances: Vec::new(),
        assignments: vec![Assign {
            net: "clk".into(),
            at: TrackIntersection {
                layer: 1,
                track: 4,
                at: 2,
                relz: RelZ::Below,
            },
        }],
        cuts: vec![
            TrackIntersection {
                layer: 0,
                track: 1,
                at: 1,
                relz: RelZ::Above,
            },
            TrackIntersection {
                layer: 0,
                track: 1,
                at: 3,
                relz: RelZ::Above,
            },
            TrackIntersection {
                layer: 0,
                track: 1,
                at: 5,
                relz: RelZ::Above,
            },
            TrackIntersection {
                layer: 1,
                track: 1,
                at: 1,
                relz: RelZ::Below,
            },
            TrackIntersection {
                layer: 1,
                track: 1,
                at: 3,
                relz: RelZ::Below,
            },
            TrackIntersection {
                layer: 1,
                track: 1,
                at: 5,
                relz: RelZ::Below,
            },
        ],
    });
    exports(lib)
}
/// Create a cell with instances
#[test]
fn create_lib2() -> Result<(), LayoutError> {
    let mut lib = Library::new("lib2");

    let c2 = lib.cells.insert(Cell {
        name: "IsInst".into(),
        top_layer: 2,
        outline: Outline::rect(100, 10)?,

        instances: vec![],
        assignments: vec![],
        cuts: Vec::new(),
    });

    lib.cells.insert(Cell {
        name: "HasInst".into(),
        top_layer: 3,
        outline: Outline::rect(200, 20)?,
        instances: vec![Instance {
            inst_name: "inst1".into(),
            cell_name: "IsInst".into(),
            cell: CellRef::Cell(c2),
            loc: (20, 2).into(),
            reflect: false,
            angle: None,
        }],
        assignments: vec![Assign {
            net: "clk".into(),
            at: TrackIntersection {
                layer: 1,
                track: 1,
                at: 1,
                relz: RelZ::Above,
            },
        }],
        cuts: Vec::new(),
    });
    exports(lib)
}

/// Create an abstract layout, with its variety of supported port types
#[test]
fn create_abstract() -> Result<(), LayoutError> {
    let outline = Outline::rect(11, 11)?;
    let ports = vec![
        abstrakt::Port {
            name: "edge_bot".into(),
            kind: abstrakt::PortKind::Edge {
                layer: 2,
                track: 2,
                side: abstrakt::Side::BottomOrLeft,
            },
        },
        abstrakt::Port {
            name: "edge_top".into(),
            kind: abstrakt::PortKind::Edge {
                layer: 2,
                track: 4,
                side: abstrakt::Side::TopOrRight,
            },
        },
        abstrakt::Port {
            name: "edge_left".into(),
            kind: abstrakt::PortKind::Edge {
                layer: 1,
                track: 1,
                side: abstrakt::Side::BottomOrLeft,
            },
        },
        abstrakt::Port {
            name: "edge_right".into(),
            kind: abstrakt::PortKind::Edge {
                layer: 1,
                track: 5,
                side: abstrakt::Side::TopOrRight,
            },
        },
        abstrakt::Port {
            name: "zfull".into(),
            kind: abstrakt::PortKind::Zfull { track: 3 },
        },
        // abstrakt::Port {
        //     name: "zlocs".into(),
        //     kind: abstrakt::PortKind::Zlocs {
        //         locs: vec![Assign {}],
        //     },
        // },
    ];
    abstrakt::Abstract {
        name: "abstrack".into(),
        outline,
        top_layer: 3,
        ports,
    };
    Ok(())
}

/// Create a cell with abstract instances
#[test]
fn create_lib3() -> Result<(), LayoutError> {
    let mut lib = Library::new("lib3");

    let c2 = lib.abstracts.insert(abstrakt::Abstract {
        name: "IsAbstrakt".into(),
        top_layer: 0,
        outline: Outline::rect(100, 10)?,
        ports: Vec::new(),
    });

    lib.cells.insert(Cell {
        name: "HasAbstrakts".into(),
        top_layer: 3,
        outline: Outline::rect(500, 50)?,
        instances: vec![
            Instance {
                inst_name: "inst1".into(),
                cell_name: "IsAbstrakt".into(),
                cell: CellRef::Abstract(c2),
                loc: (0, 0).into(),
                reflect: false,
                angle: None,
            },
            Instance {
                inst_name: "inst2".into(),
                cell_name: "IsAbstrakt".into(),
                cell: CellRef::Abstract(c2),
                loc: (200, 20).into(),
                reflect: false,
                angle: None,
            },
            Instance {
                inst_name: "inst4".into(),
                cell_name: "IsAbstrakt".into(),
                cell: CellRef::Abstract(c2),
                loc: (400, 40).into(),
                reflect: false,
                angle: None,
            },
        ],
        assignments: vec![
        //     Assign {
        //     net: "clk".into(),
        //     at: TrackIntersection {
        //         layer: 1,
        //         track: 22,
        //         at: 22,
        //         relz: RelZ::Above,
        //     },
        // }
        ],
        cuts: Vec::new(),
    });
    exports(lib)
}

/// Create a cell with abstract instances
#[test]
fn create_lib4() -> Result<(), LayoutError> {
    let mut lib = Library::new("lib4");

    let c2 = lib.abstracts.insert(abstrakt::Abstract {
        name: "UnitCell".into(),
        top_layer: 0,
        outline: Outline::rect(20, 1)?,
        ports: vec![
            abstrakt::Port {
                name: "inp".into(),
                kind: abstrakt::PortKind::Edge {
                    layer: 0,
                    track: 1,
                    side: abstrakt::Side::BottomOrLeft,
                },
            },
            abstrakt::Port {
                name: "out".into(),
                kind: abstrakt::PortKind::Edge {
                    layer: 0,
                    track: 5,
                    side: abstrakt::Side::TopOrRight,
                },
            },
        ],
    });

    // Create an array of instances
    let instances = (0..9_isize)
        .map(|k| Instance {
            inst_name: format!("inst{}", k),
            cell_name: "UnitCell".into(),
            cell: CellRef::Abstract(c2),
            loc: (20 * k, 0).into(),
            reflect: false,
            angle: None,
        })
        .collect();
    let c = Cell {
        name: "HasUnits".into(),
        top_layer: 3,
        outline: Outline::rect(300, 2)?,
        instances,
        assignments: Vec::new(),
        cuts: Vec::new(),
    };
    let c = lib.cells.insert(c);

    // lib.cells.insert(Cell {
    exports(lib)
}
/// Export [Library] `lib` in several formats
fn exports(lib: Library) -> LayoutResult<()> {
    save_yaml(&lib, &resource(&format!("{}.yaml", &lib.name)))?;
    let raw = rawconv::RawConverter::convert(lib, stack()?)?;
    save_yaml(&raw, &resource(&format!("{}.raw.yaml", &raw.name)))?;
    let gds = raw.to_gds()?;
    save_yaml(&gds, &resource(&format!("{}.gds.yaml", &gds.name)))?;
    gds.save(&resource(&format!("{}.gds", &gds.name)))?;
    Ok(())
}
#[allow(unused_imports)]
use std::io::prelude::*;
#[test]
fn stack_to_yaml() -> LayoutResult<()> {
    save_yaml(&stack()?, &resource("stack.yaml"))
}
/// Grab the full path of resource-file `fname`
fn resource(fname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname)
}
/// Save any [Serialize]-able type to yaml-format file `fname`
fn save_yaml(data: &impl Serialize, fname: &str) -> LayoutResult<()> {
    use std::fs::File;
    use std::io::BufWriter;
    let mut file = BufWriter::new(File::create(fname).unwrap());
    let yaml = serde_yaml::to_string(data).unwrap();
    file.write_all(yaml.as_bytes()).unwrap();
    file.flush().unwrap();
    Ok(())
}
