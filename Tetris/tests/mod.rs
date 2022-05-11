//!
//! # Unit Tests
//!

// Local imports
use crate::{
    abs, cell::Cell, conv, instance::Instance, layout::Layout, library::Library, outline::Outline,
    raw::LayoutResult, stack::*, tracks::*, utils::PtrList, validate::ValidStack,
};

// Modules
pub mod demos;
pub mod ro;
pub mod stacks;
use stacks::SampleStacks;

/// Create an empty cell
#[test]
fn empty_cell() -> LayoutResult<()> {
    let c = Layout {
        name: "EmptyCell".into(),
        metals: 5,
        outline: Outline::rect(50, 5)?,
        instances: PtrList::new(),
        assignments: Vec::new(),
        cuts: Vec::new(),
        places: Vec::new(),
    };
    let mut lib = Library::new("EmptyCellLib");
    let _c2 = lib.cells.insert(Cell::from(c));
    exports(lib, SampleStacks::pdka()?)
}
/// Create a layout-implementation
#[test]
fn create_layout() -> LayoutResult<()> {
    Layout {
        name: "HereGoes".into(),
        metals: 4,
        outline: Outline::rect(50, 5)?,
        instances: PtrList::new(),
        assignments: vec![Assign {
            net: "clk".into(),
            at: TrackCross::from_relz(1, 0, 1, RelZ::Above),
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

    lib.cells.insert(Layout {
        name: "HereGoes".into(),
        metals: 3,
        outline: Outline::rect(50, 5)?,
        instances: PtrList::new(),
        assignments: vec![Assign {
            net: "clk".into(),
            at: TrackCross::from_relz(1, 4, 2, RelZ::Below),
        }],
        cuts: vec![
            TrackCross::from_relz(0, 1, 1, RelZ::Above),
            TrackCross::from_relz(0, 1, 3, RelZ::Above),
            TrackCross::from_relz(0, 1, 5, RelZ::Above),
            TrackCross::from_relz(1, 1, 1, RelZ::Below),
            TrackCross::from_relz(1, 1, 3, RelZ::Below),
            TrackCross::from_relz(1, 1, 5, RelZ::Below),
        ],
        places: Vec::new(),
    });
    exports(lib, SampleStacks::pdka()?)
}
/// Create a cell with instances
#[test]
fn create_lib2() -> LayoutResult<()> {
    let mut lib = Library::new("lib2");
    let c2 = Layout::new("IsInst", 2, Outline::rect(100, 10)?);
    let c2 = lib.cells.insert(c2);

    lib.cells.insert(Layout {
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
            at: TrackCross::from_relz(1, 1, 1, RelZ::Above),
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
        abs::Port {
            name: "edge_bot".into(),
            kind: abs::PortKind::Edge {
                layer: 2,
                track: 2,
                side: abs::Side::BottomOrLeft,
            },
        },
        abs::Port {
            name: "edge_top".into(),
            kind: abs::PortKind::Edge {
                layer: 2,
                track: 4,
                side: abs::Side::TopOrRight,
            },
        },
        abs::Port {
            name: "edge_left".into(),
            kind: abs::PortKind::Edge {
                layer: 1,
                track: 1,
                side: abs::Side::BottomOrLeft,
            },
        },
        abs::Port {
            name: "edge_right".into(),
            kind: abs::PortKind::Edge {
                layer: 1,
                track: 5,
                side: abs::Side::TopOrRight,
            },
        },
    ];
    abs::Abstract {
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

    let c2 = lib.cells.insert(abs::Abstract {
        name: "IsAbs".into(),
        metals: 1,
        outline: Outline::rect(100, 10)?,
        ports: Vec::new(),
    });

    lib.cells.insert(Layout {
        name: "HasAbss".into(),
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

    let rawlib = conv::raw::RawExporter::convert(lib, stack)?;
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
fn resource(rname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), rname)
}
