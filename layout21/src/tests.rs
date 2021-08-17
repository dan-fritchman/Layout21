use serde::Serialize;

use super::cell::{Instance, LayoutImpl};
use super::library::Library;
use super::outline::Outline;
use super::raw::{self, Dir, LayoutError, LayoutResult, Units};
use super::stack::*;
use super::{abstrakt, rawconv, validate};

/// Create a [Stack] used by a number of tests
fn stack() -> LayoutResult<Stack> {
    let metal_purps = [
        (20, raw::LayerPurpose::Drawing),
        (5, raw::LayerPurpose::Label),
    ];
    let via_purps = [(44, raw::LayerPurpose::Drawing)];
    let stack = Stack {
        units: Units::Nano,
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
                entries: vec![TrackSpec::sig(140), TrackSpec::gap(320)],
                // entries: vec![
                //     TrackSpec::gnd(510),
                //     TrackSpec::pat(vec![TrackEntry::gap(410), TrackEntry::sig(50)], 8),
                //     TrackSpec::gap(410),
                //     TrackSpec::pwr(510),
                // ],
                // offset: (-255).into(),
                // overlap: (510).into(),
                // flip: FlipMode::EveryOther,
                dir: Dir::Vert,
                cutsize: (250).into(),
                offset: (-70).into(),
                overlap: (0).into(),
                raw: Some(raw::Layer::from_pairs(69, &metal_purps)?),
                flip: FlipMode::None,
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
/// Create an empy cell
#[test]
fn empty_cell() -> Result<(), LayoutError> {
    let c = LayoutImpl {
        name: "EmptyCell".into(),
        top_layer: 4,
        outline: Outline::rect(50, 5)?,
        instances: Vec::new(),
        assignments: Vec::new(),
        cuts: Vec::new(),
    };
    let mut lib = Library::new("EmptyCellLib");
    let c2 = lib.cells.insert(c.into());
    exports(lib)?;
    Ok(())
}
/// Create a layout-implementation
#[test]
fn create_layout() -> Result<(), LayoutError> {
    LayoutImpl {
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

    lib.cells.insert(
        LayoutImpl {
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
        }
        .into(),
    );
    exports(lib)
}
/// Create a cell with instances
#[test]
fn create_lib2() -> Result<(), LayoutError> {
    let mut lib = Library::new("lib2");

    let c2 = lib.cells.insert(
        LayoutImpl {
            name: "IsInst".into(),
            top_layer: 2,
            outline: Outline::rect(100, 10)?,

            instances: vec![],
            assignments: vec![],
            cuts: Vec::new(),
        }
        .into(),
    );

    lib.cells.insert(
        LayoutImpl {
            name: "HasInst".into(),
            top_layer: 3,
            outline: Outline::rect(200, 20)?,
            instances: vec![Instance {
                inst_name: "inst1".into(),
                cell: c2,
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
        }
        .into(),
    );
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
        // abstrakt::Port {
        //     name: "zfull".into(),
        //     kind: abstrakt::PortKind::Z { track: 3 },
        // },
        // abstrakt::Port {
        //     name: "zlocs".into(),
        //     kind: abstrakt::PortKind::ZTopInner {
        //         locs: vec![Assign {}],
        //     },
        // },
    ];
    abstrakt::LayoutAbstract {
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

    let c2 = lib.cells.insert(
        abstrakt::LayoutAbstract {
            name: "IsAbstrakt".into(),
            top_layer: 0,
            outline: Outline::rect(100, 10)?,
            ports: Vec::new(),
        }
        .into(),
    );

    lib.cells.insert(
        LayoutImpl {
            name: "HasAbstrakts".into(),
            top_layer: 3,
            outline: Outline::rect(500, 50)?,
            instances: vec![
                Instance {
                    inst_name: "inst1".into(),
                    cell: c2,
                    loc: (0, 0).into(),
                    reflect: false,
                    angle: None,
                },
                Instance {
                    inst_name: "inst2".into(),
                    cell: c2,
                    loc: (200, 20).into(),
                    reflect: false,
                    angle: None,
                },
                Instance {
                    inst_name: "inst4".into(),
                    cell: c2,
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
        }
        .into(),
    );
    exports(lib)
}

/// Create a cell with abstract instances
#[test]
fn create_lib4() -> Result<(), LayoutError> {
    let mut lib = Library::new("lib4");

    let c2 = lib.cells.insert(
        abstrakt::LayoutAbstract {
            name: "UnitCell".into(),
            top_layer: 0,
            outline: Outline::rect(18, 1)?,
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
        }
        .into(), // Convert to a [CellBag]
    );

    // Create an array of instances
    let mut instances = Vec::new();
    // Create assignments and cuts
    let mut assignments = Vec::new();
    let mut cuts = Vec::new();
    let m2xpitch = 23;
    for x in 0..3 {
        let m2track = (23 * x + 32) as usize;
        let track = m2track;
        for y in 0..3 {
            let botcut = (15 + 21 * y) as usize;
            let topcut = botcut + 4;
            let inst = Instance {
                inst_name: format!("inst{}{}", x, y),
                cell: c2,
                loc: (11 + x * 23, 2 + 3 * y).into(),
                reflect: false,
                angle: None,
            };
            instances.push(inst);
            let a = Assign {
                net: format!("dly{}", x),
                at: TrackIntersection {
                    layer: 1,
                    track,
                    at: botcut,
                    relz: RelZ::Below,
                },
            };
            assignments.push(a);
            let a = Assign {
                net: format!("dly{}", x),
                at: TrackIntersection {
                    layer: 1,
                    track,
                    at: topcut,
                    relz: RelZ::Below,
                },
            };
            assignments.push(a);
            // FIXME: need a third Assign
        }
        let c = TrackIntersection {
            layer: 1,
            track,
            at: 13,
            relz: RelZ::Below,
        };
        cuts.push(c);
        let c = TrackIntersection {
            layer: 1,
            track,
            at: 21 * 3,
            relz: RelZ::Below,
        };
        cuts.push(c);
    }
    let c = LayoutImpl {
        name: "HasUnits".into(),
        top_layer: 3,
        outline: Outline::rect(300, 10)?,
        instances,
        assignments,
        cuts,
    };
    let _c = lib.cells.insert(c.into());
    exports(lib)
}
/// Export [Library] `lib` in several formats
fn exports(lib: Library) -> LayoutResult<()> {
    save_yaml(&lib, &resource(&format!("{}.yaml", &lib.name)))?;
    let raw = rawconv::RawConverter::convert(lib, stack()?)?;
    save_yaml(&raw, &resource(&format!("{}.raw.yaml", &raw.name)))?;

    // If available, also export to GDSII
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
