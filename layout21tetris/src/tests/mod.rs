//!
//! # Unit Tests
//!

// Local imports
use super::cell::{self, Instance, LayoutImpl};
use super::library::Library;
use super::outline::Outline;
use super::raw::LayoutResult;
use super::stack::*;
use super::tracks::*;
use super::{abstrakt, rawconv, validate::ValidStack};

use crate::utils::PtrList;

// Modules
pub mod ro;
pub mod stacks;
use stacks::SampleStacks;

/// Create an empty cell
#[test]
fn empty_cell() -> LayoutResult<()> {
    let c = LayoutImpl {
        name: "EmptyCell".into(),
        metals: 5,
        outline: Outline::rect(50, 5)?,
        instances: PtrList::new(),
        assignments: Vec::new(),
        cuts: Vec::new(),
        places: Vec::new(),
    };
    let mut lib = Library::new("EmptyCellLib");
    let _c2 = lib.cells.insert(cell::CellBag::from(c));
    exports(lib, SampleStacks::pdka()?)
}
/// Create a layout-implementation
#[test]
fn create_layout() -> LayoutResult<()> {
    LayoutImpl {
        name: "HereGoes".into(),
        metals: 4,
        outline: Outline::rect(50, 5)?,
        instances: PtrList::new(),
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
        places: Vec::new(),
    };
    Ok(())
}
/// Create a library
#[test]
fn create_lib1() -> LayoutResult<()> {
    let mut lib = Library::new("lib1");

    lib.cells.insert(LayoutImpl {
        name: "HereGoes".into(),
        metals: 3,
        outline: Outline::rect(50, 5)?,
        instances: PtrList::new(),
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
        places: Vec::new(),
    });
    exports(lib, SampleStacks::pdka()?)
}
/// Create a cell with instances
#[test]
fn create_lib2() -> LayoutResult<()> {
    let mut lib = Library::new("lib2");
    let c2 = LayoutImpl::new("IsInst", 2, Outline::rect(100, 10)?);
    let c2 = lib.cells.insert(c2);

    lib.cells.insert(LayoutImpl {
        name: "HasInst".into(),
        metals: 4,
        outline: Outline::rect(200, 20)?,
        instances: vec![Instance {
            inst_name: "inst1".into(),
            cell: c2,
            loc: (20, 2).into(),
            reflect_horiz: false,
            reflect_vert: false,
        }]
        .into(),
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
        places: Vec::new(),
    });
    exports(lib, SampleStacks::pdka()?)
}

/// Create an abstract layout, with its variety of supported port types
#[test]
fn create_abstract() -> LayoutResult<()> {
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
    ];
    abstrakt::LayoutAbstract {
        name: "abstrack".into(),
        outline,
        metals: 4,
        ports,
    };
    Ok(())
}

/// Create a cell with abstract instances
#[test]
fn create_lib3() -> LayoutResult<()> {
    let mut lib = Library::new("lib3");

    let c2 = lib.cells.insert(abstrakt::LayoutAbstract {
        name: "IsAbstrakt".into(),
        metals: 1,
        outline: Outline::rect(100, 10)?,
        ports: Vec::new(),
    });

    lib.cells.insert(LayoutImpl {
        name: "HasAbstrakts".into(),
        metals: 4,
        outline: Outline::rect(500, 50)?,
        instances: vec![
            Instance {
                inst_name: "inst1".into(),
                cell: c2.clone(),
                loc: (0, 0).into(),
                reflect_horiz: false,
                reflect_vert: false,
            },
            Instance {
                inst_name: "inst2".into(),
                cell: c2.clone(),
                loc: (200, 20).into(),
                reflect_horiz: false,
                reflect_vert: false,
            },
            Instance {
                inst_name: "inst4".into(),
                cell: c2.clone(),
                loc: (400, 40).into(),
                reflect_horiz: false,
                reflect_vert: false,
            },
        ]
        .into(),
        assignments: Vec::new(),
        cuts: Vec::new(),
        places: Vec::new(),
    });
    exports(lib, SampleStacks::pdka()?)
}
/// Helper function. Export [Library] `lib` in several formats.
pub fn exports(lib: Library, stack: ValidStack) -> LayoutResult<()> {
    // Serializable formats will generally be written as YAML.
    use crate::utils::SerializationFormat::Yaml;

    let rawlib = rawconv::RawExporter::convert(lib, stack)?;
    let rawlib = rawlib.read()?;

    // Export to ProtoBuf, save as YAML and binary
    let protolib = rawlib.to_proto()?;
    Yaml.save(
        &protolib,
        &resource(&format!("{}.proto.yaml", &protolib.domain)),
    )
    .unwrap();
    crate::raw::proto::proto::save(
        &protolib,
        &resource(&format!("{}.proto.bin", &protolib.domain)),
    )
    .unwrap();

    // Export to GDSII
    let gds = rawlib.to_gds()?;
    Yaml.save(&gds, &resource(&format!("{}.gds.yaml", &gds.name)))
        .unwrap();
    gds.save(&resource(&format!("{}.gds", &gds.name)))?;
    Ok(())
}
/// Grab the full path of resource-file `fname`
fn resource(fname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname)
}
