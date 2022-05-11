//!
//! # Tests of one of our very favorite circuits to place in this framework!
//!

// Local imports
use crate::abs;
use crate::array::{Array, ArrayInstance, Arrayable};
use crate::cell::{Cell, RawLayoutPtr};
use crate::coords::{PrimPitches, Xy};
use crate::library::Library;
use crate::outline::Outline;
use crate::placement::{Align, Place, RelAssign};
use crate::raw::{self, LayoutResult};
use crate::stack::RelZ;
use crate::utils::Ptr;
use crate::{instance::Instance, layout::Layout};

// Test-locals
use super::{exports, resource, stacks::SampleStacks};

/// Create an abs unit-cell
fn abstract_unit_cell(_lib: &mut Library) -> LayoutResult<Ptr<Cell>> {
    Ok(Ptr::new(abstract_unit()?.into()))
}
/// Create an abs unit-cell
fn abstract_unit() -> LayoutResult<abs::Abstract> {
    let unitsize = (18, 1);

    let unit = abs::Abstract {
        name: "Wrapper".into(),
        metals: 1,
        outline: Outline::rect(unitsize.0, unitsize.1)?,
        ports: vec![
            abs::Port {
                name: "en".into(),
                kind: abs::PortKind::ZTopEdge {
                    track: 2,
                    side: abs::Side::BottomOrLeft,
                    into: (5, RelZ::Above),
                },
            },
            abs::Port {
                name: "inp".into(),
                kind: abs::PortKind::ZTopEdge {
                    track: 3,
                    side: abs::Side::TopOrRight,
                    into: (11, RelZ::Above),
                },
            },
            abs::Port {
                name: "out".into(),
                kind: abs::PortKind::ZTopEdge {
                    track: 5,
                    side: abs::Side::TopOrRight,
                    into: (11, RelZ::Above),
                },
            },
        ],
    };
    Ok(unit)
}
/// RO, absolute-placement edition
fn ro_abs(unit: Ptr<Cell>) -> LayoutResult<Cell> {
    let unitsize = (18, 1);

    // Create an initially empty layout
    let mut ro = Layout::new(
        "RO",                   // name
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
            ro.instances.add(inst);

            // Assign the input
            let m1track = (y * 12 + 9) as usize;
            let m3track = m1track + x as usize;
            ro.net(format!("dly{}", x))
                .at(1, m2track, m1track, RelZ::Below)
                .at(2, m3track, m2track, RelZ::Below);
            if x != 0 {
                // Cut M3 to the *right* of the input
                ro.cut(2, m3track, m2track + 1, RelZ::Below);
            } else {
                // Cut M3 to the *left* of the input
                ro.cut(2, m3track, m2track - 1, RelZ::Below);
            }
            // Assign the output
            let m3track = m1track + ((x + 1) % 3) as usize;
            let m1track = (y * 12 + 11) as usize;
            ro.net(format!("dly{}", ((x + 1) % 3)))
                .at(1, m2track + 2, m1track, RelZ::Below)
                .at(2, m3track, m2track + 2, RelZ::Below);
            if x != 2 {
                // Cut M3 to the *left* of the output
                ro.cut(2, m3track, m2track + 1, RelZ::Below);
            } else {
                // Cut M3 to the *right* of the output
                ro.cut(2, m3track, m2track + 3, RelZ::Below);
            }

            // Assign the enable
            let m1track = (y * 12 + 8) as usize;
            let m2track = (m2entrack + y) as usize;
            ro.net(format!("en{}{}", x, y))
                .at(1, m2track, m1track, RelZ::Below);
            ro.cut(1, m2track, m1track + 1, RelZ::Below); // Cut just above
        }

        // Make top & bottom M2 cuts
        ro.cut(1, m2track, m2botcut, RelZ::Below);
        ro.cut(1, m2track, m2topcut, RelZ::Below);
        ro.cut(1, m2track + 2, m2botcut, RelZ::Below);
        ro.cut(1, m2track + 2, m2topcut, RelZ::Below);
    }
    Ok(ro.into())
}
/// RO, relative-placement edition
fn ro_rel(unit: Ptr<Cell>) -> LayoutResult<Cell> {
    use crate::placement::{Placeable, RelativePlace, SepBy, Separation, Side};
    let unitsize = (18, 1);

    // Create an initially empty layout
    let mut ro = Layout::builder()
        .name("RO")
        .metals(4_usize)
        .outline(Outline::rect(130, 7)?)
        .build()?;
    let m2xpitch = 36;
    let m2botcut = 5;
    let m2topcut = 7 * m2botcut + 1;

    // Next-location tracker
    let mut next_loc: Place<Xy<PrimPitches>> = (unitsize.0, 2 * unitsize.1).into();

    // For each column
    for x in 0..3 {
        let m2track = (m2xpitch * (x + 1) - 4) as usize;
        let m2entrack = (x * m2xpitch) + m2xpitch / 2;
        let mut bottom_inst: Option<Ptr<Instance>> = None;

        // For each row
        for y in 0..3 {
            let inst = Instance {
                inst_name: format!("inst{}{}", x, y),
                cell: unit.clone(),
                loc: next_loc,
                reflect_horiz: false,
                reflect_vert: true,
            };
            let inst = ro.instances.add(inst);
            if y == 0 {
                bottom_inst = Some(inst.clone());
            }

            // Assign an input M2, at the center of its pin
            let assn = Placeable::Assign(Ptr::new(RelAssign {
                net: format!("dly{}", x),
                loc: RelativePlace {
                    to: Placeable::Port {
                        inst: inst.clone(),
                        port: "inp".into(),
                    },
                    side: Side::Right,
                    align: Align::Center,
                    sep: Separation::z(1),
                },
            }));
            ro.places.push(assn);

            // Assign an output M2, at the right-edge of the instance
            let assn = Placeable::Assign(Ptr::new(RelAssign {
                net: format!("dly{}", ((x + 1) % 3)),
                loc: RelativePlace {
                    to: Placeable::Port {
                        inst: inst.clone(),
                        port: "out".into(),
                    },
                    side: Side::Right,
                    align: Align::Side(Side::Right),
                    sep: Separation::z(1),
                },
            }));
            ro.places.push(assn);

            if y == 2 {
                // Top of a row. Place to the right of its bottom instance.
                next_loc = RelativePlace {
                    to: Placeable::Instance(bottom_inst.clone().unwrap()),
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
            ro.net(format!("dly{}", x))
                .at(2, m3track, m2track, RelZ::Below);

            if x != 0 {
                // Cut M3 to the *right* of the input
                ro.cut(2, m3track, m2track + 1, RelZ::Below);
            } else {
                // Cut M3 to the *left* of the input
                ro.cut(2, m3track, m2track - 1, RelZ::Below);
            }
            // Assign the output
            let m3track = m1track + ((x + 1) % 3) as usize;

            ro.net(format!("dly{}", ((x + 1) % 3)))
                .at(2, m3track, m2track + 2, RelZ::Below);

            if x != 2 {
                // Cut M3 to the *left* of the output
                ro.cut(2, m3track, m2track + 1, RelZ::Below);
            } else {
                // Cut M3 to the *right* of the output
                ro.cut(2, m3track, m2track + 3, RelZ::Below);
            }

            // Assign the enable
            let m1track = (y * 12 + 8) as usize;
            let m2track = (m2entrack + y) as usize;
            ro.net(format!("en{}{}", x, y))
                .at(1, m2track, m1track, RelZ::Below);
            ro.cut(1, m2track, m1track + 1, RelZ::Below); // Cut just above
        }

        // Make top & bottom M2 cuts
        ro.cut(1, m2track, m2botcut, RelZ::Below);
        ro.cut(1, m2track, m2topcut, RelZ::Below);
        ro.cut(1, m2track + 2, m2botcut, RelZ::Below);
        ro.cut(1, m2track + 2, m2topcut, RelZ::Below);
    }
    Ok(ro.into())
}
/// Test importing and wrapping an existing GDSII into a [Library]/[Cell]
#[test]
fn wrap_gds() -> LayoutResult<()> {
    let mut lib = Library::new("wrap_gds");
    _wrap_gds(&mut lib)?;
    exports(lib, SampleStacks::pdka()?)
}
/// Most internal implementation of the `wrap_gds` test
fn _wrap_gds(lib: &mut Library) -> LayoutResult<Ptr<Cell>> {
    // Import a [GdsLibrary] to a [raw::Library]
    let gds_fname = resource("ginv.gds");
    let gds = raw::gds::gds21::GdsLibrary::load(&gds_fname)?;

    let stack = SampleStacks::pdka()?;

    let rawlib = raw::Library::from_gds(&gds, Some(Ptr::clone(&stack.rawlayers.unwrap())))?;
    assert_eq!(rawlib.cells.len(), 1);
    // Get a [Ptr] to the first (and only) cell
    let cell = rawlib.cells.first().unwrap().clone();

    // Add tracking of our dependence on the [raw::Library]
    let rawlibptr = lib.add_rawlib(rawlib);
    // Create a [Cell] from the [raw::Library]'s sole cell
    let unitsize = (18, 1);
    let wrapped = RawLayoutPtr {
        outline: Outline::rect(unitsize.0, unitsize.1)?, // outline
        metals: 1,
        lib: rawlibptr,
        cell,
    };
    let wrapped = lib.cells.insert(wrapped);

    // Create a wrapper cell
    let mut wrapper = Layout::new(
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
    // Convert the layout to a [Cell]
    let mut wrapper: Cell = wrapper.into();
    // And add an [Abstract] view
    wrapper.abs = Some(abstract_unit()?);
    // Finally add the wrapper [Cell] to our [Library], and return a pointer to it.
    let wrapper = lib.cells.insert(wrapper);
    Ok(wrapper)
}
/// RO, array-placement edition
fn ro_array(unit: Ptr<Cell>) -> LayoutResult<Cell> {
    use crate::placement::{Placeable, SepBy, Separation};
    let unitsize = (18, 1);

    // Create an initially empty layout
    let mut ro = Layout::new(
        "RO",                   // name
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
    ro.places.push(a);

    // Now do all of the metal-layer stuff: assignments and cuts
    // This part remains in absolute coordinates

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
            ro.net(format!("dly{}", x))
                .at(1, m2track, m1track, RelZ::Below)
                .at(2, m3track, m2track, RelZ::Below);
            if x != 0 {
                // Cut M3 to the *right* of the input
                ro.cut(2, m3track, m2track + 1, RelZ::Below);
            } else {
                // Cut M3 to the *left* of the input
                ro.cut(2, m3track, m2track - 1, RelZ::Below);
            }
            // Assign the output
            let m3track = m1track + ((x + 1) % 3) as usize;
            let m1track = (y * 12 + 11) as usize;
            ro.net(format!("dly{}", ((x + 1) % 3)))
                .at(1, m2track + 2, m1track, RelZ::Below)
                .at(2, m3track, m2track + 2, RelZ::Below);
            if x != 2 {
                // Cut M3 to the *left* of the output
                ro.cut(2, m3track, m2track + 1, RelZ::Below);
            } else {
                // Cut M3 to the *right* of the output
                ro.cut(2, m3track, m2track + 3, RelZ::Below);
            }

            // Assign the enable
            let m1track = (y * 12 + 8) as usize;
            let m2track = (m2entrack + y) as usize;
            ro.net(format!("en{}{}", x, y))
                .at(1, m2track, m1track, RelZ::Below);
            ro.cut(1, m2track, m1track + 1, RelZ::Below); // Cut just above
        }

        // Make top & bottom M2 cuts
        ro.cut(1, m2track, m2botcut, RelZ::Below);
        ro.cut(1, m2track, m2topcut, RelZ::Below);
        ro.cut(1, m2track + 2, m2botcut, RelZ::Below);
        ro.cut(1, m2track + 2, m2topcut, RelZ::Below);
    }
    Ok(ro.into())
}
/// Runner for each of these RO tests.
/// Accepts function-arguments for the unit-cell and wrapper-cell factories.
fn _ro_test(
    libname: &str,
    unitfn: fn(&mut Library) -> LayoutResult<Ptr<Cell>>,
    wrapfn: fn(Ptr<Cell>) -> LayoutResult<Cell>,
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
fn ro_abs_abs() -> LayoutResult<()> {
    _ro_test("RoAbsAbs", abstract_unit_cell, ro_abs)
}
#[test]
fn ro_abs_rel() -> LayoutResult<()> {
    _ro_test("RoAbsRel", abstract_unit_cell, ro_rel)
}
#[test]
fn ro_abs_array() -> LayoutResult<()> {
    _ro_test("RoAbsArray", abstract_unit_cell, ro_rel)
}
