#[allow(unused_imports)]
use std::io::prelude::*;

use serde::Serialize;

use super::cell::{self, Instance, LayoutImpl};
use super::library::Library;
use super::outline::Outline;
use super::raw::{self, Dir, LayoutError, LayoutResult, Units};
use super::stack::*;
use super::{abstrakt, rawconv, validate};
use crate::utils::Ptr;

/// Create a [Stack] used by a number of tests
fn stack() -> LayoutResult<Stack> {
    let mut rawlayers = raw::Layers::default();
    rawlayers.add(raw::Layer::new(64, "nwell").add_pairs(&[
        (44, raw::LayerPurpose::Drawing),
        (5, raw::LayerPurpose::Label),
    ])?);
    let metal_purps = [
        (20, raw::LayerPurpose::Drawing),
        (5, raw::LayerPurpose::Label),
    ];
    rawlayers.add(raw::Layer::new(67, "li1").add_pairs(&metal_purps)?);
    let via_purps = [
        (44, raw::LayerPurpose::Drawing),
        (5, raw::LayerPurpose::Label),
    ];
    let stack = Stack {
        units: Units::Nano,
        boundary_layer: Some(rawlayers.add(raw::Layer::from_pairs(
            236,
            &[(0, raw::LayerPurpose::Outline)],
        )?)),
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
                raw: Some(rawlayers.add(raw::Layer::from_pairs(68, &metal_purps)?)),
                flip: FlipMode::EveryOther,
                prim: PrimitiveMode::Partial,
            },
            Layer {
                name: "met2".into(),
                entries: vec![TrackSpec::sig(140), TrackSpec::gap(320)],
                dir: Dir::Vert,
                cutsize: (250).into(),
                offset: (-70).into(),
                overlap: (0).into(),
                raw: Some(rawlayers.add(raw::Layer::from_pairs(69, &metal_purps)?)),
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
                raw: Some(rawlayers.add(raw::Layer::from_pairs(70, &metal_purps)?)),
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
                raw: Some(rawlayers.add(raw::Layer::from_pairs(71, &metal_purps)?)),
                flip: FlipMode::EveryOther,
                prim: PrimitiveMode::None,
            },
        ],
        vias: vec![
            ViaLayer {
                name: "mcon".into(),
                between: (0, 1),
                size: (240, 240).into(),
                raw: Some(rawlayers.add(raw::Layer::from_pairs(67, &via_purps)?)),
            },
            ViaLayer {
                name: "via1".into(),
                between: (1, 2),
                size: (240, 240).into(),
                raw: Some(rawlayers.add(raw::Layer::from_pairs(68, &via_purps)?)),
            },
            ViaLayer {
                name: "via2".into(),
                between: (2, 3),
                size: (240, 240).into(),
                raw: Some(rawlayers.add(raw::Layer::from_pairs(69, &via_purps)?)),
            },
            ViaLayer {
                name: "via3".into(),
                between: (3, 4),
                size: (240, 240).into(),
                raw: Some(rawlayers.add(raw::Layer::from_pairs(70, &via_purps)?)),
            },
        ],
        rawlayers: Some(Ptr::new(rawlayers)),
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
    let _c2 = lib.cells.insert(c.into());
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
                    cell: c2.clone(),
                    loc: (0, 0).into(),
                    reflect: false,
                    angle: None,
                },
                Instance {
                    inst_name: "inst2".into(),
                    cell: c2.clone(),
                    loc: (200, 20).into(),
                    reflect: false,
                    angle: None,
                },
                Instance {
                    inst_name: "inst4".into(),
                    cell: c2.clone(),
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

    let unitsize = (18, 1);

    let c2 = lib.cells.insert(
        abstrakt::LayoutAbstract {
            name: "UnitCell".into(),
            top_layer: 0,
            outline: Outline::rect(unitsize.0, unitsize.1)?,
            ports: vec![
                abstrakt::Port {
                    name: "en".into(),
                    kind: abstrakt::PortKind::ZTopEdge {
                        track: 2,
                        side: abstrakt::Side::BottomOrLeft,
                        into: (5, RelZ::Above),
                    },
                },
                abstrakt::Port {
                    name: "inp".into(),
                    kind: abstrakt::PortKind::ZTopEdge {
                        track: 3,
                        side: abstrakt::Side::TopOrRight,
                        into: (11, RelZ::Above),
                    },
                },
                abstrakt::Port {
                    name: "out".into(),
                    kind: abstrakt::PortKind::ZTopEdge {
                        track: 5,
                        side: abstrakt::Side::TopOrRight,
                        into: (11, RelZ::Above),
                    },
                },
            ],
        }
        .into(), // Convert to a [CellBag]
    );

    // Create an initially empty layout
    let mut hasunits = LayoutImpl::new(
        "HasUnits",                                     // name
        3,                                              // top_layer
        Outline::rect(7 * unitsize.0, 7 * unitsize.1)?, // outline
    );
    let m2xpitch = 36;
    let m2botcut = 6;
    let m2topcut = 7 * 6;

    // For each column
    for x in 0..3 {
        let m2track = (m2xpitch * (x + 1) - 4) as usize;
        let m2entrack = (x * m2xpitch) + m2xpitch / 2;

        // For each row
        for y in 0..3 {
            let loc = ((2 * x + 1) * unitsize.0, (2 * y + 1) * unitsize.1).into();
            let inst = Instance {
                inst_name: format!("inst{}{}", x, y),
                cell: c2.clone(),
                loc,
                reflect: false,
                angle: None,
            };
            hasunits.instances.push(inst);

            // Assign the input
            let m1track = (y * 14 + 10) as usize;
            let m3track = m1track + x as usize;
            hasunits
                .net(format!("dly{}", x))
                .at(1, m2track, m1track, RelZ::Below)
                .at(2, m3track, m2track, RelZ::Below);
            if x != 0 {
                // Cut M3 to the *right* of the input
                hasunits.cut(2, m3track, m2track + 1, RelZ::Below);
            } else {
                // Cut M3 to the *left* of the input
                hasunits.cut(2, m3track, m2track - 1, RelZ::Below);
            }
            // Assign the output
            let m3track = m1track + ((x + 1) % 3) as usize;
            let m1track = (y * 14 + 12) as usize;
            hasunits
                .net(format!("dly{}", ((x + 1) % 3)))
                .at(1, m2track + 2, m1track, RelZ::Below)
                .at(2, m3track, m2track + 2, RelZ::Below);
            if x != 2 {
                // Cut M3 to the *left* of the output
                hasunits.cut(2, m3track, m2track + 1, RelZ::Below);
            } else {
                // Cut M3 to the *right* of the output
                hasunits.cut(2, m3track, m2track + 3, RelZ::Below);
            }

            // Assign the enable
            let m1track = (y * 14 + 9) as usize;
            let m2track = (m2entrack + y) as usize;
            hasunits
                .net(format!("en{}{}", x, y))
                .at(1, m2track, m1track, RelZ::Below);
            hasunits.cut(1, m2track, m1track + 1, RelZ::Below); // Cut just above
        }

        // Make top & bottom M2 cuts
        hasunits.cut(1, m2track, m2botcut, RelZ::Below);
        hasunits.cut(1, m2track, m2topcut, RelZ::Below);
        hasunits.cut(1, m2track + 2, m2botcut, RelZ::Below);
        hasunits.cut(1, m2track + 2, m2topcut, RelZ::Below);
    }
    // Add it to our library, and export
    let _ = lib.cells.insert(hasunits.into());
    exports(lib)
}

#[test]
fn wrap_gds() -> LayoutResult<()> {
    // Import a [GdsLibrary] to a [raw::Library]
    let gds_fname = resource("ginv.gds");
    let gds = raw::gds::gds21::GdsLibrary::load(&gds_fname)?;
    // gds.name = "ginv".into();
    // if gds.structs.len() > 1 {
    //     gds.structs.pop();
    // }
    // gds.structs[0].name = "ginv".into();
    // gds.save(&resource("ginv.gds"))?;
    // save_yaml(&gds, &resource("ginv.gds.yaml"))?;

    let stack = stack()?;

    let rawlib = raw::Library::from_gds(&gds, Some(Ptr::clone(&stack.rawlayers.unwrap())))?;
    assert_eq!(rawlib.cells.len(), 1);
    // Get the first (and only) cell key
    let cellkey = rawlib.cells.keys().next().unwrap();

    // Create our [Library]
    let mut lib = Library::new("wrap_gds");
    // Take ownership of the [raw::Library]
    let rawlibptr = lib.add_rawlib(rawlib);
    // Create a [CellBag] from the [raw::Library]'s sole cell
    let wrapped = cell::RawLayoutPtr {
        lib: rawlibptr,
        cell: cellkey,
    };
    let wrapped = lib.cells.insert(wrapped.into());

    // Create a wrapper cell
    let mut wrapper = LayoutImpl::new(
        "Wrapper",              // name
        0,                      // top_layer
        Outline::rect(50, 10)?, // outline
    );
    wrapper.instances.push(Instance {
        inst_name: "wrapped".into(),
        cell: wrapped,
        loc: (0, 0).into(),
        reflect: false,
        angle: None,
    });
    let _wrapper = lib.cells.insert(wrapper.into());
    exports(lib)
}

/// Export [Library] `lib` in several formats
fn exports(lib: Library) -> LayoutResult<()> {
    // FIXME: whether to remove altogether:
    // save_yaml(&lib, &resource(&format!("{}.yaml", &lib.name)))?;
    let raw = rawconv::RawExporter::convert(lib, stack()?)?;
    let raw = raw.read()?;
    // FIXME: whether to remove altogether:
    // save_yaml(&raw, &resource(&format!("{}.raw.yaml", &raw.name)))?;

    // Export to ProtoBuf
    let protolib = raw.to_proto()?;
    save_yaml(
        &protolib,
        &resource(&format!("{}.proto.yaml", &protolib.domain)),
    )?;
    crate::raw::proto::proto::save(
        &protolib,
        &resource(&format!("{}.proto.bin", &protolib.domain)),
    )
    .unwrap();

    // Export to GDSII
    let gds = raw.to_gds()?;
    save_yaml(&gds, &resource(&format!("{}.gds.yaml", &gds.name)))?;
    gds.save(&resource(&format!("{}.gds", &gds.name)))?;
    Ok(())
}
// FIXME: whether to remove altogether:
// #[test]
// fn stack_to_yaml() -> LayoutResult<()> {
//     save_yaml(&stack()?, &resource("stack.yaml"))
// }
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
