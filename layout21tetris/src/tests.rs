//!
//! # Unit Tests
//!

use super::cell::{self, Instance, LayoutImpl};
use super::library::Library;
use super::outline::Outline;
use super::raw::{self, Dir, LayoutError, LayoutResult, Units};
use super::stack::*;
use super::{abstrakt, rawconv, validate};
use crate::coords::{Int, PrimPitches, Xy};
use crate::placement::Place;
use crate::placer::Placer;
use crate::utils::{Ptr, PtrList};

// FIXME: this would preferably live in `place.rs`,
// but unit tests don't seem to find it there (yet) (?).
impl From<(Int, Int)> for Place<PrimPitches> {
    fn from(tup: (Int, Int)) -> Self {
        Self::Abs(Xy::from(tup))
    }
}

/// # Sample Stacks
/// Namespace for commonly re-used [Stack]s for testing.
pub struct SampleStacks;

impl SampleStacks {
    /// As nearly empty a [Stack] as possible, while being raw-exportable.
    /// Includes:
    /// * A `boundary_layer`
    /// * [raw::Layers] containing solely that boundary layer
    /// * No metals or via layers
    /// Generally useful for placement activities, particularly among [Instace]s.
    pub fn empty() -> LayoutResult<Stack> {
        let mut rawlayers = raw::Layers::default();
        let boundary_layer = Some(rawlayers.add(raw::Layer::from_pairs(
            0,
            &[(0, raw::LayerPurpose::Outline)],
        )?));
        let stack = Stack {
            units: Units::default(),
            boundary_layer,
            prim: PrimitiveLayer::new((100, 100).into()),
            layers: Vec::new(), // No metal layers
            vias: Vec::new(),   // No vias
            rawlayers: Some(Ptr::new(rawlayers)),
        };
        Ok(stack)
    }
    /// Real(istic) PDK [Stack]
    pub fn pdka() -> LayoutResult<Stack> {
        let mut rawlayers = raw::Layers::default();
        // Shorthands for the common purpose-numbers
        let metal_purps = [
            (255, raw::LayerPurpose::Obstruction),
            (20, raw::LayerPurpose::Drawing),
            (5, raw::LayerPurpose::Label),
            (16, raw::LayerPurpose::Pin),
        ];
        let via_purps = [
            (255, raw::LayerPurpose::Obstruction),
            (44, raw::LayerPurpose::Drawing),
            (5, raw::LayerPurpose::Label),
            (16, raw::LayerPurpose::Pin),
        ];
        // Add a few base-layers that we are used in imported/ primitive cells, but not in our stack
        rawlayers.add(raw::Layer::new(64, "nwell").add_pairs(&metal_purps)?);
        rawlayers.add(raw::Layer::new(67, "li1").add_pairs(&metal_purps)?);
        // Create the test stack
        let stack = Stack {
            units: Units::Nano,
            boundary_layer: Some(rawlayers.add(raw::Layer::from_pairs(
                236,
                &[(0, raw::LayerPurpose::Outline)],
            )?)),
            prim: PrimitiveLayer {
                pitches: (460, 2720).into(),
            },
            layers: vec![
                Layer {
                    name: "met1".into(),
                    entries: vec![
                        TrackSpec::gnd(480),
                        TrackSpec::pat(vec![TrackEntry::gap(200), TrackEntry::sig(140)], 6),
                        TrackSpec::gap(200),
                        TrackSpec::pwr(480),
                    ],
                    dir: Dir::Horiz,
                    offset: (-240).into(),
                    cutsize: (250).into(),
                    overlap: (480).into(),
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
                        TrackSpec::gnd(480),
                        TrackSpec::pat(vec![TrackEntry::gap(200), TrackEntry::sig(140)], 6),
                        TrackSpec::gap(200),
                        TrackSpec::pwr(480),
                    ],
                    dir: Dir::Horiz,
                    offset: (-240).into(),
                    cutsize: (250).into(),
                    overlap: (480).into(),
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
}
/// Run the test-stacks through validation
#[test]
fn validate_stack() -> LayoutResult<()> {
    let s = SampleStacks::empty()?;
    validate::StackValidator::validate(s)?;
    let s = SampleStacks::pdka()?;
    validate::StackValidator::validate(s)?;
    Ok(())
}
/// Create an empy cell
#[test]
fn empty_cell() -> LayoutResult<()> {
    let c = LayoutImpl {
        name: "EmptyCell".into(),
        top_layer: 4,
        outline: Outline::rect(50, 5)?,
        instances: PtrList::new(),
        assignments: Vec::new(),
        cuts: Vec::new(),
        places: PtrList::new(),
    };
    let mut lib = Library::new("EmptyCellLib");
    let _c2 = lib.cells.insert(cell::CellBag::from(c));
    exports(lib, SampleStacks::pdka()?)?;
    Ok(())
}
/// Create a layout-implementation
#[test]
fn create_layout() -> LayoutResult<()> {
    LayoutImpl {
        name: "HereGoes".into(),
        top_layer: 3,
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
        places: PtrList::new(),
    };
    Ok(())
}
/// Create a library
#[test]
fn create_lib1() -> LayoutResult<()> {
    let mut lib = Library::new("lib1");

    lib.cells.insert(
        LayoutImpl {
            name: "HereGoes".into(),
            top_layer: 2,
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
            places: PtrList::new(),
        }
        .into(),
    );
    exports(lib, SampleStacks::pdka()?)
}
/// Create a cell with instances
#[test]
fn create_lib2() -> LayoutResult<()> {
    let mut lib = Library::new("lib2");
    let c2 = LayoutImpl::new("IsInst", 2, Outline::rect(100, 10)?).into();
    let c2 = lib.cells.insert(c2);

    lib.cells.insert(
        LayoutImpl {
            name: "HasInst".into(),
            top_layer: 3,
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
            places: PtrList::new(),
        }
        .into(),
    );
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
        top_layer: 3,
        ports,
    };
    Ok(())
}

/// Create a cell with abstract instances
#[test]
fn create_lib3() -> LayoutResult<()> {
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
            places: PtrList::new(),
        }
        .into(),
    );
    exports(lib, SampleStacks::pdka()?)
}

/// Create a cell with abstract instances
#[test]
fn create_lib4() -> LayoutResult<()> {
    let mut lib = Library::new("lib4");
    let unit = abstract_unit()?.into(); // Convert to a [CellBag]
    let unit = lib.cells.insert(unit); // Insert and get a shared-pointer
    let ro = ro(unit)?; // Create the RO level
    lib.cells.insert(ro); // And insert it
    exports(lib, SampleStacks::pdka()?)
}
fn abstract_unit() -> Result<abstrakt::LayoutAbstract, LayoutError> {
    let unitsize = (18, 1);

    let unit = abstrakt::LayoutAbstract {
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
    };
    Ok(unit)
}
/// Create a very-specifically crafted ring from unit-cell `unit`
fn ro(unit: Ptr<cell::CellBag>) -> LayoutResult<cell::CellBag> {
    let unitsize = (18, 1);

    // Create an initially empty layout
    let mut hasunits = LayoutImpl::new(
        "HasUnits",                                     // name
        3,                                              // top_layer
        Outline::rect(7 * unitsize.0, 7 * unitsize.1)?, // outline
    );
    let m2xpitch = 36;
    let m2botcut = 5;
    let m2topcut = 7 * m2botcut + 1;

    // For each column
    for x in 0..3 {
        let m2track = (m2xpitch * (x + 1) - 4) as usize;
        let m2entrack = (x * m2xpitch) + m2xpitch / 2;

        // For each row
        for y in 0..3 {
            let loc = ((2 * x + 1) * unitsize.0, (2 * (y + 1)) * unitsize.1).into();
            let inst = Instance {
                inst_name: format!("inst{}{}", x, y),
                cell: unit.clone(),
                loc,
                reflect_horiz: false,
                reflect_vert: true,
            };
            hasunits.instances.add(inst);

            // Assign the input
            let m1track = (y * 12 + 9) as usize;
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
            let m1track = (y * 12 + 11) as usize;
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
            let m1track = (y * 12 + 8) as usize;
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
    Ok(hasunits.into())
}
/// RO, relative-placement edition
fn ro_rel(unit: Ptr<cell::CellBag>) -> LayoutResult<cell::CellBag> {
    use crate::placement::{Placeable, RelativePlace, SepBy, Separation, Side};
    let unitsize = (18, 1);

    // Create an initially empty layout
    let mut hasunits = LayoutImpl::new(
        "HasUnits",                                     // name
        3,                                              // top_layer
        Outline::rect(7 * unitsize.0, 7 * unitsize.1)?, // outline
    );
    let m2xpitch = 36;
    let m2botcut = 5;
    let m2topcut = 7 * m2botcut + 1;

    // Next-location tracker
    let mut next_loc = (unitsize.0, 2 * unitsize.1).into();
    let mut insts: Vec<Ptr<Instance>> = Vec::new();

    // For each column
    for x in 0..3 {
        let m2track = (m2xpitch * (x + 1) - 4) as usize;
        let m2entrack = (x * m2xpitch) + m2xpitch / 2;

        // For each row
        for y in 0..3 {
            let inst = Instance {
                inst_name: format!("inst{}{}", x, y),
                cell: unit.clone(),
                loc: next_loc,
                reflect_horiz: false,
                reflect_vert: true,
            };
            let inst = hasunits.instances.add(inst);
            insts.push(inst.clone());
            if y == 2 {
                // Top of a row. Place to the right of its bottom instance.
                next_loc = RelativePlace {
                    to: Placeable::Instance(insts[3 * x].clone()),
                    side: Side::Right,
                    align: Side::Bottom, // Top or Bottom both work just as well here
                    sep: Separation::x(SepBy::SizeOf(unit.clone())),
                }
                .into();
            } else {
                // Place above the most-recent instance.
                next_loc = RelativePlace {
                    to: Placeable::Instance(inst.clone()),
                    side: Side::Top,
                    align: Side::Left, // Left or Right both work just as well here
                    sep: Separation::y(SepBy::SizeOf(unit.clone())),
                }
                .into();
            }

            // Assign the input
            let m1track = (y * 12 + 9) as usize;
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
            let m1track = (y * 12 + 11) as usize;
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
            let m1track = (y * 12 + 8) as usize;
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
    Ok(hasunits.into())
}
/// Test importing and wrapping an existing GDSII into a [Library]/[CellBag]
#[test]
fn wrap_gds() -> LayoutResult<()> {
    let mut lib = Library::new("wrap_gds");
    _wrap_gds(&mut lib)?;
    exports(lib, SampleStacks::pdka()?)
}
/// Most internal implementation of the `wrap_gds` test
fn _wrap_gds(lib: &mut Library) -> LayoutResult<Ptr<cell::CellBag>> {
    // Import a [GdsLibrary] to a [raw::Library]
    let gds_fname = resource("ginv.gds");
    let gds = raw::gds::gds21::GdsLibrary::load(&gds_fname)?;

    let stack = SampleStacks::pdka()?;

    let rawlib = raw::Library::from_gds(&gds, Some(Ptr::clone(&stack.rawlayers.unwrap())))?;
    assert_eq!(rawlib.cells.len(), 1);
    // Get a [Ptr] to the first (and only) cell
    let cell = rawlib.cells.first().unwrap().clone();

    // Take ownership of the [raw::Library]
    let rawlibptr = lib.add_rawlib(rawlib);
    // Create a [CellBag] from the [raw::Library]'s sole cell
    let wrapped = cell::RawLayoutPtr {
        lib: rawlibptr,
        cell,
    };
    let wrapped = lib.cells.insert(wrapped.into());

    // Create a wrapper cell
    let unitsize = (18, 1);
    let mut wrapper = LayoutImpl::new(
        "Wrapper",                              // name
        0,                                      // top_layer
        Outline::rect(unitsize.0, unitsize.1)?, // outline
    );
    wrapper.instances.add(Instance {
        inst_name: "wrapped".into(),
        cell: wrapped,
        loc: (0, 0).into(),
        reflect_horiz: false,
        reflect_vert: false,
    });
    let wrapper = lib.cells.insert(wrapper.into());
    Ok(wrapper)
}

#[test]
fn gds_wrapped_ro() -> LayoutResult<()> {
    let mut lib = Library::new("WrappedRo");
    let unit = _wrap_gds(&mut lib)?;
    let ro = ro(unit)?; // Create the RO level
    lib.cells.insert(ro); // And add it to the Library
    exports(lib, SampleStacks::pdka()?)
}

#[test]
fn gds_wrapped_ro_rel() -> LayoutResult<()> {
    let mut lib = Library::new("WrappedRoRelPlaced");
    let unit = _wrap_gds(&mut lib)?;
    let ro = ro_rel(unit)?; // Create the RO level
    let ro = lib.cells.insert(ro); // And add it to the Library
    let (lib, stack) = Placer::place(lib, SampleStacks::pdka()?)?;
    {
        let ro = ro.read()?;
        let lay = ro.layout.as_ref().unwrap();
        for inst in lay.instances.iter() {
            let inst = inst.read()?;
            println!("{:?}", &inst.loc);
        }
    }
    exports(lib, stack)
}

/// Create a library with an "empty" cell
#[test]
fn create_empty_cell_lib() -> LayoutResult<()> {
    let mut lib = Library::new("empty_cell_lib");
    let cell = LayoutImpl::new("empty_cell", 4, Outline::rect(100, 10)?).into();
    lib.cells.insert(cell);
    exports(lib, SampleStacks::pdka()?)
}

/// Export [Library] `lib` in several formats
pub fn exports(lib: Library, stack: Stack) -> LayoutResult<()> {
    use crate::utils::SerializationFormat::Yaml;

    let raw = rawconv::RawExporter::convert(lib, stack)?;
    let raw = raw.read()?;

    // Export to ProtoBuf, save as YAML and binary
    let protolib = raw.to_proto()?;
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
    let gds = raw.to_gds()?;
    Yaml.save(&gds, &resource(&format!("{}.gds.yaml", &gds.name)))
        .unwrap();
    gds.save(&resource(&format!("{}.gds", &gds.name)))?;
    Ok(())
}
/// Grab the full path of resource-file `fname`
fn resource(fname: &str) -> String {
    format!("{}/resources/{}", env!("CARGO_MANIFEST_DIR"), fname)
}
