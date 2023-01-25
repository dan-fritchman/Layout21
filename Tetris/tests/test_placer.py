from dataclasses import dataclass
from typing import Optional

import tetris
from tetris.align import Align, AlignSide
from tetris.abstract import Abstract, Port, PortKind
from tetris.library import Library
from tetris.instance import Instance, Reflect
from tetris.layout import Layout
from tetris.placer import Placer
from tetris.outline import Outline
from tetris.cell import Cell
from tetris.placement import Place, Placeable, RelAssign, RelativePlace, Side, AbsPlace
from tetris.separation import SepBy, Separation

# from .tests import exports, SampleStacks


def exports(lib: Library, stack: Optional["Stack"] = None) -> None:
    ...  # FIXME!


class SampleStacks:
    @classmethod
    def empty(cls) -> None:
        return None

    @classmethod
    def pdka(cls) -> None:
        return None


def test_place1() -> None:
    # Most basic smoke-test
    Placer.place(Library("plib"), SampleStacks.empty())


def test_place2() -> None:
    # Initial test of relative placement

    lib = Library("test_place2")
    # Create a unit cell which we'll instantiate a few times
    unit = Layout("unit", 0, Outline.rect(3, 7))
    unit = Cell(name="unit", layout=unit)
    unit = lib.add_cell(unit)
    # Create an initial instance
    i0 = Instance(
        name="i0",
        of=unit,
        loc=AbsPlace.xy(47, 51),
        reflect=Reflect.default(),
    )
    # Create the parent cell which instantiates it
    parent = Layout("parent", 0, Outline.rect(100, 100))
    i0 = parent.add_instance(i0)
    # Create another Instance, placed relative to `i0`
    i1 = Instance(
        name="i1",
        of=unit,
        loc=RelativePlace(
            to=i0,
            side=Side.Right,
            align=AlignSide(Side.Bottom),
            sep=Separation.zero(),
        ),
        reflect=Reflect.default(),
    )
    i1 = parent.add_instance(i1)
    parent = Cell(name="parent", layout=parent)
    parent = lib.add_cell(parent)

    # The real code-under-test: run placement
    (lib, stack) = Placer.place(lib, SampleStacks.empty())

    # Checks on results
    assert len(lib.cells) == 2
    assert lib.cells["unit"] is unit
    assert lib.cells["parent"] is parent
    # Now check the locations
    parent_layout = parent.layout
    assert parent_layout.instances[0].loc == AbsPlace.xy(47, 51)
    assert parent_layout.instances[1].loc.resolved == AbsPlace.xy(50, 51)
    exports(lib, stack)


# def test_place3() -> None :
#     # Test each relative side and alignment

#     # Get the sample data
#     sample_lib = SampleLib.get()
#     ibig = sample_lib.ibig
#     big = sample_lib.big
#     lil = sample_lib.lil
#     lib = sample_lib.lib
#     parent = sample_lib.parent

#     lib.name = "test_place3"
#     relto = ibig

#     # Relative-placement-adder closure
#     add_inst = |inst_name: str, side, align| {
#         i = Instance(
#             inst_name: inst_name,
#             cell: lil,
#             loc: RelativePlace(RelativePlace(
#                 to: relto,
#                 side,
#                 align: AlignSide(align),
#                 sep: Separation.zero(),
#             ),
#             reflect_horiz: False,
#             reflect_vert: False,
#         )
#         parent.add_instance(i)
#     }
#     # Add a bunch of em
#     i1 = add_inst("i1", Side.Left, Side.Bottom)
#     i2 = add_inst("i2", Side.Right, Side.Bottom)
#     i3 = add_inst("i3", Side.Bottom, Side.Left)
#     i4 = add_inst("i4", Side.Bottom, Side.Right)
#     i5 = add_inst("i5", Side.Left, Side.Top)
#     i6 = add_inst("i6", Side.Right, Side.Top)
#     i7 = add_inst("i7", Side.Top, Side.Left)
#     i8 = add_inst("i8", Side.Top, Side.Right)

#     # Add `parent` to the library
#     _parent = lib.add_cell(parent)

#     # The real code under test: run placement
#     (lib, stack) = Placer.place(lib, SampleStacks.pdka())

#     # And test the placed results
#     bigbox = ibig.read().boundbox()
#     ibox = i1.read().boundbox()
#     assert ibox.side(Side.Right), bigbox.side(Side.Left))
#     assert ibox.side(Side.Bottom), bigbox.side(Side.Bottom))
#     ibox = i2.read().boundbox()
#     assert ibox.side(Side.Left), bigbox.side(Side.Right))
#     assert ibox.side(Side.Bottom), bigbox.side(Side.Bottom))
#     ibox = i3.read().boundbox()
#     assert ibox.side(Side.Top), bigbox.side(Side.Bottom))
#     assert ibox.side(Side.Left), bigbox.side(Side.Left))
#     ibox = i4.read().boundbox()
#     assert ibox.side(Side.Top), bigbox.side(Side.Bottom))
#     assert ibox.side(Side.Right), bigbox.side(Side.Right))
#     ibox = i5.read().boundbox()
#     assert ibox.side(Side.Right), bigbox.side(Side.Left))
#     assert ibox.side(Side.Top), bigbox.side(Side.Top))
#     ibox = i6.read().boundbox()
#     assert ibox.side(Side.Left), bigbox.side(Side.Right))
#     assert ibox.side(Side.Top), bigbox.side(Side.Top))
#     ibox = i7.read().boundbox()
#     assert ibox.side(Side.Bottom), bigbox.side(Side.Top))
#     assert ibox.side(Side.Left), bigbox.side(Side.Left))
#     ibox = i8.read().boundbox()
#     assert ibox.side(Side.Bottom), bigbox.side(Side.Top))
#     assert ibox.side(Side.Right), bigbox.side(Side.Right))
#     exports(lib, stack)


# def test_place4() -> None :
#     # Test size-of separation

#     # Get the sample data
#     sample_lib = SampleLib.get()
#     ibig = sample_lib.ibig
#     big = sample_lib.big
#     lil = sample_lib.lil
#     lib = sample_lib.lib
#     parent = sample_lib.parent

#     lib.name = "test_place4"

#     # Relative-placement-adder closure
#     add_inst = |inst_name: str, side, sep| {
#         i = Instance(
#             inst_name: inst_name,
#             cell: lil,
#             loc: RelativePlace(RelativePlace(
#                 to: ibig,
#                 side,
#                 align: AlignSide(side.cw_90()), # Leave out `align` as an arg, set one-turn CW of `side`
#                 sep,
#             ),
#             reflect_horiz: False,
#             reflect_vert: False,
#         )
#         parent.add_instance(i)
#     }
#     # Add a bunch of em
#     sep_x = Separation.x(SepBy.SizeOf(lil))
#     i1 = add_inst("i1", Side.Left, sep_x)
#     i2 = add_inst("i2", Side.Right, sep_x)
#     sep_y = Separation.y(SepBy.SizeOf(lil))
#     i3 = add_inst("i3", Side.Bottom, sep_y)
#     i4 = add_inst("i4", Side.Top, sep_y)
#     # Add `parent` to the library
#     _parent = lib.add_cell(parent)

#     # The real code under test: run placement
#     (lib, stack) = Placer.place(lib, SampleStacks.pdka())

#     # And test the placed results
#     lilsize = lil.read().boundbox_size()
#     bigbox = ibig.read().boundbox()
#     ibox = i1.read().boundbox()
#     assert ibox.side(Side.Top), bigbox.side(Side.Top))
#     assert ibox.side(Side.Right), bigbox.side(Side.Left) - lilsize.x)
#     ibox = i2.read().boundbox()
#     assert ibox.side(Side.Bottom), bigbox.side(Side.Bottom))
#     assert ibox.side(Side.Left), bigbox.side(Side.Right) + lilsize.x)
#     ibox = i3.read().boundbox()
#     assert ibox.side(Side.Left), bigbox.side(Side.Left))
#     assert ibox.side(Side.Top), bigbox.side(Side.Bottom) - lilsize.y)
#     ibox = i4.read().boundbox()
#     assert ibox.side(Side.Right), bigbox.side(Side.Right))
#     assert ibox.side(Side.Bottom), bigbox.side(Side.Top) + lilsize.y)

#     exports(lib, stack)


# def test_place5() -> None :
#     # Test separation by units

#     # Get the sample data
#     sample_lib = SampleLib.get()
#     ibig = sample_lib.ibig
#     big = sample_lib.big
#     lil = sample_lib.lil
#     lib = sample_lib.lib
#     parent = sample_lib.parent

#     lib.name = "test_place5"

#     # Relative-placement-adder closure
#     add_inst = |inst_name: str, side, sep| {
#         i = Instance {
#             inst_name: inst_name,
#             cell: lil,
#             loc: RelativePlace(RelativePlace {
#                 to: ibig,
#                 side,
#                 align: AlignSide(side.ccw_90()), # Leave out `align` as an arg, set one-turn CCW of `side`
#                 sep,
#             }),
#             reflect_horiz: False,
#             reflect_vert: False,
#         }
#         parent.add_instance(i)
#     }
#     # Add a bunch of em
#     dx = PrimPitches(Dir.Horiz, 1)
#     sep_x = Separation.x(SepBy.UnitSpeced(dx))
#     i1 = add_inst("i1", Side.Left, sep_x)
#     i2 = add_inst("i2", Side.Right, sep_x)
#     dy = PrimPitches(Dir.Vert, 5)
#     sep_y = Separation.y(SepBy.UnitSpeced(dy))
#     i3 = add_inst("i3", Side.Bottom, sep_y)
#     i4 = add_inst("i4", Side.Top, sep_y)
#     # Add `parent` to the library
#     _parent = lib.add_cell(parent)

#     # The real code under test: run placement
#     (lib, stack) = Placer.place(lib, SampleStacks.pdka())

#     # And test the placed results
#     bigbox = ibig.read().boundbox()
#     ibox = i1.read().boundbox()
#     assert ibox.side(Side.Bottom), bigbox.side(Side.Bottom))
#     assert ibox.side(Side.Right), bigbox.side(Side.Left) - dx)
#     ibox = i2.read().boundbox()
#     assert ibox.side(Side.Top), bigbox.side(Side.Top))
#     assert ibox.side(Side.Left), bigbox.side(Side.Right) + dx)
#     ibox = i3.read().boundbox()
#     assert ibox.side(Side.Right), bigbox.side(Side.Right))
#     assert ibox.side(Side.Top), bigbox.side(Side.Bottom) - dy)
#     ibox = i4.read().boundbox()
#     assert ibox.side(Side.Left), bigbox.side(Side.Left))
#     assert ibox.side(Side.Bottom), bigbox.side(Side.Top) + dy)

#     exports(lib, stack)


# def test_place6() -> None :
#     # Test port-relative placement

#     # Get the sample data
#     sample_lib = SampleLib.get()
#     ibig = sample_lib.ibig
#     big = sample_lib.big
#     lil = sample_lib.lil
#     lib = sample_lib.lib
#     parent = sample_lib.parent

#     lib.name = "test_place6"

#     # Relative-placement-adder closure
#     add_inst = |inst_name: str| {
#         i = Instance {
#             inst_name: inst_name,
#             cell: lil,
#             loc: (0, 0),
#             reflect_horiz: False,
#             reflect_vert: False,
#         }
#         parent.add_instance(i)
#     }
#     # Add a `lil`
#     i1 = add_inst("i1")

#     # The "code under test": add a relative-placed `Assign`.
#     parent.places.append(Placeable.Assign(Ptr(RelAssign {
#         net: "NETPPP",
#         loc: RelativePlace {
#             to: Placeable.Port {
#                 inst: i1,
#                 port: "PPP",
#             },
#             align: Align.Center,
#             side: Side.Left, # FIXME: kinda nonsense
#             sep: Separation.z(2),
#         },
#     })))
#     # Add `parent` to the library
#     parent = lib.add_cell(parent)

#     # The real code under test: run placement
#     (lib, stack) = Placer.place(lib, SampleStacks.pdka())

#     {
#         p = parent.read()
#         parent_layout = p.layout.as_ref().unwrap()
#         assert parent_layout.places.len(), 0)
#         assert parent_layout.instances.len(), 2)
#         assert parent_layout.cuts.len(), 0)
#         assert parent_layout.assignments.len(), 1)
#         assn = parent_layout.assignments[0]
#         assert assn.net, "NETPPP")
#         assert assn.at.track.layer, 2)
#         assert assn.at.track.track, 0)
#         assert assn.at.cross.layer, 1)
#         assert assn.at.cross.track, 1)
#     }
#     exports(lib, stack)


@dataclass
class SampleLib:
    lib: Library
    big: Cell
    ibig: Instance
    lil: Cell
    parent: Layout

    @classmethod
    def get(cls) -> "SampleLib":
        """# Get a sample library with test cells `big`, `lil`, and `parent`.
        # Designed for adding instances of `lil` relative to `big` all around `parent`."""

        lib = Library("_rename_me_plz_")
        # Create a big center cell
        big = Layout("big", 1, Outline.rect(11, 12))
        big = lib.add_cell(big)
        # Create the parent cell which instantiates it
        parent = Layout("parent", 3, Outline.rect(40, 35))
        # Create an initial instance
        ibig = Instance(
            name="ibig",
            of=big,
            loc=AbsPlace.xy(16, 15),
            reflect=Reflect.default(),
        )
        ibig = parent.add_instance(ibig)
        # Create a unit cell which we'll instantiate a few times around `ibig`
        lil = Cell("lil")
        lil.layout = Layout("lil", 1, Outline.rect(2, 1))
        lil_abs = Abstract("lil", 1, Outline.rect(2, 1))
        lil_abs.ports.append(
            Port(
                name="PPP",
                kind=PortKind.ZTopEdge(
                    track=0,
                    side=abs.Side.BottomOrLeft,
                    into=(2, stack.RelZ.Above),
                ),
            )
        )
        lil.abs = Some(lil_abs)
        lil = lib.add_cell(lil)
        return SampleLib(
            lib,
            big,
            ibig,
            lil,
            parent,
        )
