//!
//! # Tests of one of our very favorite circuits to place in this framework!
//!

// Local imports
use crate::abstrakt;
use crate::cell::{self, Instance, LayoutImpl};
use crate::coords::{PrimPitches, Xy};
use crate::library::Library;
use crate::outline::Outline;
use crate::placement::{Align, Array, ArrayInstance, Arrayable, Place};
use crate::raw::{self, LayoutResult};
use crate::stack::RelZ;
use crate::utils::Ptr;

// Test-locals
use super::{exports, resource, stacks::SampleStacks};

/// Create an abstrakt unit-cell
fn abstract_unit_cell(_lib: &mut Library) -> LayoutResult<Ptr<cell::CellBag>> {
    Ok(Ptr::new(abstract_unit()?.into()))
}
/// Create an abstrakt unit-cell
fn abstract_unit() -> LayoutResult<abstrakt::LayoutAbstract> {
    let unitsize = (18, 1);

    let unit = abstrakt::LayoutAbstract {
        name: "UnitCell".into(),
        metals: 1,
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
/// RO, absolute-placement edition
fn ro_abs(unit: Ptr<cell::CellBag>) -> LayoutResult<cell::CellBag> {
    let unitsize = (18, 1);

    // Create an initially empty layout
    let mut hasunits = LayoutImpl::new(
        "HasUnits",             // name
        4,                      // metals
        Outline::rect(130, 7)?, // outline
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
        "HasUnits",             // name
        4,                      // metals
        Outline::rect(130, 7)?, // outline
    );
    let m2xpitch = 36;
    let m2botcut = 5;
    let m2topcut = 7 * m2botcut + 1;

    // Next-location tracker
    let mut next_loc: Place<Xy<PrimPitches>> = (unitsize.0, 2 * unitsize.1).into();
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
                    align: Align::Side(Side::Bottom), // Top or Bottom both work just as well here
                    sep: Separation::x(SepBy::SizeOf(unit.clone())),
                }
                .into();
            } else {
                // Place above the most-recent instance.
                next_loc = RelativePlace {
                    to: Placeable::Instance(inst.clone()),
                    side: Side::Top,
                    align: Align::Side(Side::Left), // Left or Right both work just as well here
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
    let unitsize = (18, 1);
    let wrapped = cell::RawLayoutPtr {
        outline: Outline::rect(unitsize.0, unitsize.1)?, // outline
        metals: 1,
        lib: rawlibptr,
        cell,
    };
    let wrapped = lib.cells.insert(wrapped);

    // Create a wrapper cell
    let mut wrapper = LayoutImpl::new(
        "Wrapper",                              // name
        1,                                      // metals
        Outline::rect(unitsize.0, unitsize.1)?, // outline
    );
    wrapper.instances.add(Instance {
        inst_name: "wrapped".into(),
        cell: wrapped,
        loc: (0, 0).into(),
        reflect_horiz: false,
        reflect_vert: false,
    });
    let wrapper = lib.cells.insert(wrapper);
    Ok(wrapper)
}
/// RO, array-placement edition
fn ro_array(unit: Ptr<cell::CellBag>) -> LayoutResult<cell::CellBag> {
    use crate::placement::{Placeable, SepBy, Separation};
    let unitsize = (18, 1);

    // Create an initially empty layout
    let mut hasunits = LayoutImpl::new(
        "HasUnits",             // name
        4,                      // metals
        Outline::rect(130, 7)?, // outline
    );
    let m2xpitch = 36;
    let m2botcut = 5;
    let m2topcut = 7 * m2botcut + 1;

    // Create the main array of Instances
    let a = Placeable::Array(Ptr::new(ArrayInstance {
        name: "insts".into(),
        loc: (unitsize.0, 6 * unitsize.1).into(),
        reflect_vert: true,
        reflect_horiz: false,
        array: Ptr::new(Array {
            name: "col".into(),
            count: 3,
            sep: Separation::y(SepBy::UnitSpeced(PrimPitches::y(2).into())),
            unit: Arrayable::Array(Ptr::new(Array {
                name: "row".into(),
                unit: Arrayable::Instance(unit.clone()),
                count: 3,
                sep: Separation::x(SepBy::UnitSpeced(PrimPitches::x(36).into())), // FIXME!
            })),
        }),
    }));
    hasunits.places.push(a);

    // For each column
    for x in 0..3 {
        let m2track = (m2xpitch * (x + 1) - 4) as usize;
        let m2entrack = (x * m2xpitch) + m2xpitch / 2;

        // For each row
        for y in 0..3 {
            /*
            # Remaining TODO here:
            * Assignments, for input, output, and enable of each instance
            * Cuts
            let mut group = Group::new("???");
            let instptr = group.add(Instance {
                inst_name: "inst".into(),
                cell: unit.clone(),
                loc: Place::origin(),
                reflect_horiz: false,
                reflect_vert: true,
            });
            let a1 = group.add(AssignPlace {
                net: format!("dly{}", x),
                at: RelSomething(
                    inst,
                    "out",            // ?
                    Separation::z(1), // One layer up
                    Align::Center,    // Align on pin-center
                ),
            });
            AssignSomething {
                net: format!("dly{}", x),
                at: RelSomething(
                    a1, // Relative to the last assignment
                    Separation::z(2), // Now *two* layers up
                ),
            };
            */
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
/// Runner for each of these RO tests.
/// Accepts function-arguments for the unit-cell and wrapper-cell factories.
fn _ro_test(
    libname: &str,
    unitfn: fn(&mut Library) -> LayoutResult<Ptr<cell::CellBag>>,
    wrapfn: fn(Ptr<cell::CellBag>) -> LayoutResult<cell::CellBag>,
) -> LayoutResult<()> {
    let mut lib = Library::new(libname);
    let unit = unitfn(&mut lib)?; // Create the unit cell
    let ro = wrapfn(unit)?; // Create the RO level
    lib.cells.insert(ro); // And add it to the Library
    exports(lib, SampleStacks::pdka()?) // And export everything to our handful of formats
}
// Execute a bunch of combinations, each as a separate test
#[test]
fn ro_wrap_gds_abs() -> LayoutResult<()> {
    _ro_test("RoWrapGdsAbs", _wrap_gds, ro_abs)
}
#[test]
fn ro_wrap_gds_rel() -> LayoutResult<()> {
    _ro_test("RoWrapGdsRel", _wrap_gds, ro_rel)
}
#[test]
fn ro_wrap_gds_array() -> LayoutResult<()> {
    _ro_test("RoWrapGdsArray", _wrap_gds, ro_array)
}
#[test]
fn ro_abstrakt_abs() -> LayoutResult<()> {
    _ro_test("RoAbstraktAbs", abstract_unit_cell, ro_abs)
}
#[test]
fn ro_abstrakt_rel() -> LayoutResult<()> {
    _ro_test("RoAbstraktRel", abstract_unit_cell, ro_rel)
}
#[test]
fn ro_abstrakt_array() -> LayoutResult<()> {
    _ro_test("RoAbstraktArray", abstract_unit_cell, ro_rel)
}
