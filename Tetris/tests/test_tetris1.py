#
# # Unit Tests
#

from typing import Optional

# Local imports
from tetris.cell import Cell
from tetris.instance import Instance
from tetris.layout import Layout
from tetris.library import Library
from tetris.outline import Outline
from tetris.relz import RelZ
from tetris.stack import *

# from tetris import conv
# from tetris.tracks import *
# from tetris.validate import ValidStack


def exports(lib: Library, stack: Optional["Stack"] = None) -> None:
    ...  # FIXME!


# Create an empty cell
def test_empty_cell() -> None:
    c = Layout(
        name="EmptyCell",
        metals=5,
        outline=Outline.rect(50, 5),
        instances=list(),
        assignments=list(),
        cuts=list(),
    )
    lib = Library("EmptyCellLib")
    lib.cells[c.name] = Cell.from_views(c.name, [c])
    exports(lib, SampleStacks.pdka())


# Create a layout-implementation
def test_create_layout() -> None:
    Layout(
        name="HereGoes",
        metals=4,
        outline=Outline.rect(50, 5),
        instances=list(),
        assignments=[
            Assign(
                net="clk",
                at=TrackCross.from_relz(1, 0, 1, RelZ.Above),
            )
        ],
        cuts=list(),
    )


# Create a library
def test_create_lib1() -> None:
    lib = Library("lib1")
    layout = Layout(
        name="HereGoes",
        metals=3,
        outline=Outline.rect(50, 5),
        instances=list(),
        assignments=[
            Assign(
                net="clk",
                at=TrackCross.from_relz(1, 4, 2, RelZ.Below),
            )
        ],
        cuts=[
            TrackCross.from_relz(0, 1, 1, RelZ.Above),
            TrackCross.from_relz(0, 1, 3, RelZ.Above),
            TrackCross.from_relz(0, 1, 5, RelZ.Above),
            TrackCross.from_relz(1, 1, 1, RelZ.Below),
            TrackCross.from_relz(1, 1, 3, RelZ.Below),
            TrackCross.from_relz(1, 1, 5, RelZ.Below),
        ],
    )
    lib.cells[layout.name] = Cell.from_views(layout.name, [layout])
    exports(lib, SampleStacks.pdka())


# # Create a cell with instances
# def test_create_lib2() -> None :
#     lib = Library("lib2")
#     c2 = Layout("IsInst", 2, Outline.rect(100, 10))
#     c2 = lib.cells.insert(c2)

#     lib.cells.insert(Layout :
#         name: "HasInst",
#         metals: 4,
#         outline: Outline.rect(200, 20),
#         instances: [Instance :
#             inst_name: "inst1",
#             cell: c2,
#             loc: (20, 2),
#             reflect_horiz: false,
#             reflect_vert: false,
#         ]
#         ,
#         assignments: [Assign :
#             net: "clk",
#             at: TrackCross.from_relz(1, 1, 1, RelZ.Above),
#         ],
#         cuts: list(),
#         places: list(),
#     )
#     exports(lib, SampleStacks.pdka())


# # Create an abstract layout, with its variety of supported port types
# def test_create_abstract() -> None :
#     outline = Outline.rect(11, 11)
#     ports = [
#         abs.Port :
#             name: "edge_bot",
#             kind: abs.PortKind.Edge :
#                 layer: 2,
#                 track: 2,
#                 side: abs.Side.BottomOrLeft,
#             ,
#         ,
#         abs.Port :
#             name: "edge_top",
#             kind: abs.PortKind.Edge :
#                 layer: 2,
#                 track: 4,
#                 side: abs.Side.TopOrRight,
#             ,
#         ,
#         abs.Port :
#             name: "edge_left",
#             kind: abs.PortKind.Edge :
#                 layer: 1,
#                 track: 1,
#                 side: abs.Side.BottomOrLeft,
#             ,
#         ,
#         abs.Port :
#             name: "edge_right",
#             kind: abs.PortKind.Edge :
#                 layer: 1,
#                 track: 5,
#                 side: abs.Side.TopOrRight,
#             ,
#         ,
#     ]
#     abs.Abstract :
#         name: "abstrack",
#         outline,
#         metals: 4,
#         ports,


# # Create a cell with abstract instances
# def test_create_lib3() -> None :
#     lib = Library("lib3")

#     c2 = lib.cells.insert(abs.Abstract :
#         name: "IsAbs",
#         metals: 1,
#         outline: Outline.rect(100, 10),
#         ports: list(),
#     )

#     lib.cells.insert(Layout :
#         name: "HasAbss",
#         metals: 4,
#         outline: Outline.rect(500, 50),
#         instances: [
#             Instance :
#                 inst_name: "inst1",
#                 cell: c2.clone(),
#                 loc: (0, 0),
#                 reflect_horiz: false,
#                 reflect_vert: false,
#             ,
#             Instance :
#                 inst_name: "inst2",
#                 cell: c2.clone(),
#                 loc: (200, 20),
#                 reflect_horiz: false,
#                 reflect_vert: false,
#             ,
#             Instance :
#                 inst_name: "inst4",
#                 cell: c2.clone(),
#                 loc: (400, 40),
#                 reflect_horiz: false,
#                 reflect_vert: false,
#             ,
#         ]
#         ,
#         assignments: list(),
#         cuts: list(),
#         places: list(),
#     )
#     exports(lib, SampleStacks.pdka())

# # Helper function. Export [Library] `lib` in several formats.
# def exports(lib: Library, stack: ValidStack) -> None :
#     # Serializable formats will generally be written as YAML.
#     from . import utils.SerializationFormat.Yaml

#     rawlib = conv.raw.RawExporter.convert(lib, stack)
#     rawlib = rawlib.read()

#     # Export to ProtoBuf, save as YAML and binary
#     protolib = rawlib.to_proto()
#     Yaml.save(
#         protolib,
#         resource(format!(":.proto.yaml", protolib.domain)),
#     )

#     raw.proto.proto.save(
#         protolib,
#         resource(format!(":.proto.bin", protolib.domain)),
#     )


#     # Export to GDSII
#     gds = rawlib.to_gds()
#     Yaml.save(gds, resource(format!(":.gds.yaml", gds.name)))

#     gds.save(resource(format!(":.gds", gds.name)))


# # Grab the full path of resource-file `fname`
# def resource(rname: str) -> str :
#     format!(":/resources/:", env!("CARGO_MANIFEST_DIR"), rname)
