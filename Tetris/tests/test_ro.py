"""
# 
# Tests of one of our very favorite circuits to place in this framework!
# 
"""
import pytest
from typing import Optional, Callable

# Local imports
import tetris as t

# Test-locals
# from . import exports, resource, stacks.SampleStacks

# Create an abs unit-cell
def abstract_unit_cell(_lib: t.Library) -> t.Cell:
    return abstract_unit()


# Create an abs unit-cell
def abstract_unit() -> t.abstract.Abstract:
    unitsize = (18, 1)

    unit = t.abstract.Abstract(
        name="Wrapper",
        metals=1,
        outline=t.Outline.rect(unitsize[0], unitsize[1]),
        ports=[
            t.abstract.Port(
                name="en",
                kind=t.abstract.ZTopEdge(
                    track=2,
                    side=t.abstract.Side.BottomOrLeft,
                    into=(5, t.RelZ.Above),
                ),
            ),
            t.abstract.Port(
                name="inp",
                kind=t.abstract.ZTopEdge(
                    track=3,
                    side=t.abstract.Side.TopOrRight,
                    into=(11, t.RelZ.Above),
                ),
            ),
            t.abstract.Port(
                name="out",
                kind=t.abstract.ZTopEdge(
                    track=5,
                    side=t.abstract.Side.TopOrRight,
                    into=(11, t.RelZ.Above),
                ),
            ),
        ],
    )
    return unit


# RO, absolute-placement edition
def ro_abs(unit: t.Cell) -> t.Layout:
    unitsize = (18, 1)

    # Create an initially empty layout
    ro = t.Layout(
        "RO",  # name
        4,  # metals
        t.Outline.rect(130, 7),  # outline
    )
    m2xpitch = 36
    m2botcut = 5
    m2topcut = 7 * m2botcut + 1

    # For each column
    for x in range(3):
        m2track = m2xpitch * (x + 1) - 4
        m2entrack = (x * m2xpitch) + m2xpitch / 2

        # For each row
        for y in range(3):
            loc = ((2 * x + 1) * unitsize[0], (2 * (y + 1)) * unitsize[1])
            inst = t.Instance(
                name=f"inst{x}{y}",
                of=unit,
                loc=loc,
                reflect=t.Reflect(horiz=False, vert=True),
            )
            ro.add_instance(inst)

            # Assign the input
            m1track = y * 12 + 9
            m3track = m1track + x
            ro.net(f"dly{x}").at(1, m2track, m1track, t.RelZ.Below).at(
                2, m3track, m2track, t.RelZ.Below
            )
            if x != 0:
                # Cut M3 to the *right* of the input
                ro.cut(2, m3track, m2track + 1, t.RelZ.Below)
            else:
                # Cut M3 to the *left* of the input
                ro.cut(2, m3track, m2track - 1, t.RelZ.Below)

            # Assign the output
            m3track = m1track + ((x + 1) % 3)
            m1track = y * 12 + 11
            ro.net(f"dly{((x + 1) % 3)}").at(1, m2track + 2, m1track, t.RelZ.Below).at(
                2, m3track, m2track + 2, t.RelZ.Below
            )
            if x != 2:
                # Cut M3 to the *left* of the output
                ro.cut(2, m3track, m2track + 1, t.RelZ.Below)
            else:
                # Cut M3 to the *right* of the output
                ro.cut(2, m3track, m2track + 3, t.RelZ.Below)

            # Assign the enable
            m1track = y * 12 + 8
            m2track = m2entrack + y
            ro.net(f"en{x}{y}").at(1, m2track, m1track, t.RelZ.Below)
            ro.cut(1, m2track, m1track + 1, t.RelZ.Below)  # Cut just above

        # Make top & bottom M2 cuts
        ro.cut(1, m2track, m2botcut, t.RelZ.Below)
        ro.cut(1, m2track, m2topcut, t.RelZ.Below)
        ro.cut(1, m2track + 2, m2botcut, t.RelZ.Below)
        ro.cut(1, m2track + 2, m2topcut, t.RelZ.Below)

    return ro


def ro_rel(unit: t.Cell) -> t.Layout:
    """# RO, relative-placement edition"""

    unitsize = (18, 1)

    # Create an initially empty layout
    ro = t.Layout(name="RO", metals=4, outline=t.Outline.rect(130, 7))
    m2xpitch = 36
    m2botcut = 5
    m2topcut = 7 * m2botcut + 1

    # Next-location tracker
    next_loc: t.Place = t.AbsPlace(xy=t.Xy.new(unitsize[0], 2 * unitsize[1]))

    # For each column
    for x in range(3):
        m2track = m2xpitch * (x + 1) - 4
        m2entrack = (x * m2xpitch) + m2xpitch / 2
        bottom_inst: Optional[t.Instance] = None

        # For each row
        for y in range(3):
            inst = t.Instance(
                name=f"inst{x}{y}",
                of=unit,
                loc=next_loc,
                reflect=t.Reflect(horiz=False, vert=True),
            )
            inst = ro.add_instance(inst)
            if y == 0:
                bottom_inst = inst

            # FIXME! relative net-assignment locations
            # # Assign an input M2, at the center of its pin
            # assn = Placeable.Assign(Ptr.new(RelAssign
            #     net: format!("dly", x),
            #     loc: RelativePlace
            #         to: Placeable.Port
            #             inst: inst,
            #             port: "inp",
            #         ,
            #         side: Side.Right,
            #         align: Align.Center,
            #         sep: Separation.by_z(1),
            #     ,
            # ))
            # ro.places.push(assn)

            # # Assign an output M2, at the right-edge of the instance
            # assn = Placeable.Assign(Ptr.new(RelAssign
            #     net: format!("dly", ((x + 1) % 3)),
            #     loc: RelativePlace
            #         to: Placeable.Port
            #             inst: inst,
            #             port: "out",
            #         ,
            #         side: Side.Right,
            #         align: Align.Side(Side.Right),
            #         sep: Separation.by_z(1),
            #     ,
            # ))
            # ro.places.push(assn)

            if y == 2:
                # Top of a row. Place to the right of its bottom instance.
                next_loc = t.RelativePlace(
                    to=bottom_inst,
                    side=t.Side.Right,
                    align=t.AlignSide(
                        t.Side.Bottom
                    ),  # Top or Bottom both work just as well here
                    sep=t.Separation.by_x(t.PrimPitches.x(unitsize[0])),
                )

            else:
                # Place above the most-recent instance.
                next_loc = t.RelativePlace(
                    to=inst,
                    side=t.Side.Top,
                    align=t.AlignSide(
                        t.Side.Left
                    ),  # Left or Right both work just as well here
                    sep=t.Separation.by_y(t.PrimPitches.y(unitsize[1])),
                )

            # Assign the input
            m1track = y * 12 + 9
            m3track = m1track + x
            ro.net(f"dly{x}").at(2, m3track, m2track, t.RelZ.Below)

            if x != 0:
                # Cut M3 to the *right* of the input
                ro.cut(2, m3track, m2track + 1, t.RelZ.Below)
            else:
                # Cut M3 to the *left* of the input
                ro.cut(2, m3track, m2track - 1, t.RelZ.Below)

            # Assign the output
            m3track = m1track + ((x + 1) % 3)

            ro.net(f"dly{ (x + 1) % 3}").at(2, m3track, m2track + 2, t.RelZ.Below)

            if x != 2:
                # Cut M3 to the *left* of the output
                ro.cut(2, m3track, m2track + 1, t.RelZ.Below)
            else:
                # Cut M3 to the *right* of the output
                ro.cut(2, m3track, m2track + 3, t.RelZ.Below)

            # Assign the enable
            m1track = y * 12 + 8
            m2track = m2entrack + y
            ro.net(f"en{x}{y}").at(1, m2track, m1track, t.RelZ.Below)
            ro.cut(1, m2track, m1track + 1, t.RelZ.Below)  # Cut just above

        # Make top & bottom M2 cuts
        ro.cut(1, m2track, m2botcut, t.RelZ.Below)
        ro.cut(1, m2track, m2topcut, t.RelZ.Below)
        ro.cut(1, m2track + 2, m2botcut, t.RelZ.Below)
        ro.cut(1, m2track + 2, m2topcut, t.RelZ.Below)

    return ro


def ro_array(unit: t.Cell) -> t.Layout:
    """# RO, array-placement edition"""

    unitsize = (18, 1)

    # Create an initially empty layout
    ro = t.Layout.new(
        "RO",  # name
        4,  # metals
        t.Outline.rect(130, 7),  # outline
    )
    m2xpitch = 36
    m2botcut = 5
    m2topcut = 7 * m2botcut + 1

    # Create the main array of t.Instances
    array_inst = t.Instance(
        name="array_inst",
        unit=t.Array(
            name="col",
            count=3,
            sep=t.Separation.by_y(t.PrimPitches.y(2)),
            unit=t.Array(
                name="row",
                unit=unit,
                count=3,
                sep=t.Separation.by_x(t.PrimPitches.x(36)),  # FIXME!
            ),
            reflect=t.Reflect(vert=True, horiz=False),
        ),
        loc=(unitsize[0], 6 * unitsize[1]),
    )
    ro.add_instance(array_inst)

    # # Now do all of the metal-layer stuff: assignments and cuts
    # # This part remains in absolute coordinates

    # # For each column
    # for x in 0..3
    #     m2track = (m2xpitch * (x + 1) - 4)
    #     m2entrack = (x * m2xpitch) + m2xpitch / 2

    #     # For each row
    #     for y in 0..3
    #         /*
    #         # Remaining TODO here:
    #         * Assignments, for input, output, and enable of each instance
    #         * Cuts
    #         group = Group.new("")
    #         instptr = group.add(t.Instance
    #             name: "inst",
    #             cell: unit,
    #             loc: Place.origin(),
    #             reflect_horiz: False,
    #             reflect_vert: True,
    #         )
    #         a1 = group.add(AssignPlace
    #             net: format!("dly", x),
    #             at: RelSomething(
    #                 inst,
    #                 "out",            #
    #                 Separation.by_z(1), # One layer up
    #                 Align.Center,    # Align on pin-center
    #             ),
    #         )
    #         AssignSomething
    #             net: format!("dly", x),
    #             at: RelSomething(
    #                 a1, # Relative to the last assignment
    #                 Separation.by_z(2), # Now *two* layers up
    #             ),

    #         */
    #         # Assign the input
    #         m1track = (y * 12 + 9)
    #         m3track = m1track + x
    #         ro.net(format!("dly", x))
    #             .at(1, m2track, m1track,t.RelZ.Below)
    #             .at(2, m3track, m2track,t.RelZ.Below)
    #         if x != 0
    #             # Cut M3 to the *right* of the input
    #             ro.cut(2, m3track, m2track + 1,t.RelZ.Below)
    #          else
    #             # Cut M3 to the *left* of the input
    #             ro.cut(2, m3track, m2track - 1,t.RelZ.Below)

    #         # Assign the output
    #         m3track = m1track + ((x + 1) % 3)
    #         m1track = (y * 12 + 11)
    #         ro.net(format!("dly", ((x + 1) % 3)))
    #             .at(1, m2track + 2, m1track,t.RelZ.Below)
    #             .at(2, m3track, m2track + 2,t.RelZ.Below)
    #         if x != 2
    #             # Cut M3 to the *left* of the output
    #             ro.cut(2, m3track, m2track + 1,t.RelZ.Below)
    #          else
    #             # Cut M3 to the *right* of the output
    #             ro.cut(2, m3track, m2track + 3,t.RelZ.Below)

    #         # Assign the enable
    #         m1track = (y * 12 + 8)
    #         m2track = (m2entrack + y)
    #         ro.net(format!("en", x, y))
    #             .at(1, m2track, m1track,t.RelZ.Below)
    #         ro.cut(1, m2track, m1track + 1,t.RelZ.Below) # Cut just above

    #     # Make top & bottom M2 cuts
    #     ro.cut(1, m2track, m2botcut,t.RelZ.Below)
    #     ro.cut(1, m2track, m2topcut,t.RelZ.Below)
    #     ro.cut(1, m2track + 2, m2botcut,t.RelZ.Below)
    #     ro.cut(1, m2track + 2, m2topcut,t.RelZ.Below)

    return ro


# # Test importing and wrapping an existing GDSII into a [Library]/[t.Cell]

# def wrap_gds() -> None:
#     lib =t.Library("wrap_gds")
#     _wrap_gds( lib)
#     exports(lib, SampleStacks.pdka())

# # Most internal implementation of the `wrap_gds` test
# def _wrap_gds(lib: t.Library) ->t.LayoutResult<Ptr<t.Cell>>
#     # Import a [GdsLibrary] to a [raw.Library]
#     gds_fname = resource("ginv.gds")
#     gds = raw.gds.gds21.GdsLibrary.load(&gds_fname)

#     stack = SampleStacks.pdka()

#     rawlib = raw.Library.from_gds(&gds, Some(Ptr.clone(&stack.rawlayers.unwrap())))
#     assert_eq!(rawlib.cells.len(), 1)
#     # Get a [Ptr] to the first (and only) cell
#     cell = rawlib.cells.first().unwrap()

#     # Add tracking of our dependence on the [raw.Library]
#     rawlibptr = lib.add_rawlib(rawlib)
#     # Create a [t.Cell] from the [raw.Library]'s sole cell
#     unitsize = (18, 1)
#     wrapped = RawLayoutPtr
#         outline:t.Outline.rect(unitsize[0], unitsize[1]), # outline
#         metals: 1,
#         lib: rawlibptr,
#         cell,

#     wrapped = lib.cells.insert(wrapped)

#     # Create a wrapper cell
#     wrapper =t.Layout.new(
#         "Wrapper",                              # name
#         1,                                      # metals
#        t.Outline.rect(unitsize[0], unitsize[1]), # outline
#     )
#     wrapper.add_instance(t.Instance
#         name: "wrapped",
#         cell: wrapped,
#         loc: (0, 0),
#         reflect_horiz: False,
#         reflect_vert: False,
#     )
#     # Convert the layout to a [t.Cell]
#     wrapper: t.Cell = wrapper
#     # And add an [Abstract] view
#     wrapper.abs = Some(abstract_unit())
#     # Finally add the wrapper [t.Cell] to our [Library], and return a pointer to it.
#     wrapper = lib.cells.insert(wrapper)
#     return wrapper


def _ro_test(
    libname: str,
    unitfn: Callable,  # def (t.Library) -> t.Cell,
    wrapfn: Callable,  # def (t.Cell) -> t.Layout,
) -> None:
    """# Runner for each of these RO tests.
    Accepts function-arguments for the unit-cell and wrapper-cell factories."""

    lib = t.Library(libname)
    unit = unitfn(lib)  # Create the unit cell
    ro = wrapfn(unit)  # Create the RO layout
    ro_cell = t.Cell(name="RO", layout=ro)  # Wrap it in a Cell
    lib.add_cell(ro_cell)  # And add it to the Library
    exports(lib, SampleStacks.pdka())  # And export everything to our handful of formats


"""
# Execute a bunch of combinations, each as a separate test
"""


@pytest.mark.xfail
def test_ro_wrap_gds_abs() -> None:
    return _ro_test("RoWrapGdsAbs", _wrap_gds, ro_abs)


@pytest.mark.xfail
def test_ro_wrap_gds_rel() -> None:
    return _ro_test("RoWrapGdsRel", _wrap_gds, ro_rel)


@pytest.mark.xfail
def test_ro_wrap_gds_array() -> None:
    return _ro_test("RoWrapGdsArray", _wrap_gds, ro_array)


def test_ro_abs_abs() -> None:
    return _ro_test("RoAbsAbs", abstract_unit_cell, ro_abs)


def test_ro_abs_rel() -> None:
    return _ro_test("RoAbsRel", abstract_unit_cell, ro_rel)


@pytest.mark.xfail
def test_ro_abs_array() -> None:
    return _ro_test("RoAbsArray", abstract_unit_cell, ro_array)
