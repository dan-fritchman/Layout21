#
# # Instance Structures
#
# Located, oriented instances of other cells or similar reusable layout objects.
#

from pydantic.dataclasses import dataclass

# Local imports
from .coords import PrimPitches, Xy, Dir
from .error import LayoutError

# from .cell import Cell
# from .place import Place

# Instance of another Cell
@dataclass
class Instance:
    # Instance Name
    inst_name: str
    # Cell Definition Reference
    cell: "Cell"
    # Location of the Instance origin
    # This origin-position holds regardless of either `reflect` field.
    # If specified in absolute coordinates location-units are [PrimPitches].
    loc: "Place[Xy[PrimPitches]]"
    # Horizontal Reflection
    reflect_horiz: bool
    # Vertical Reflection
    reflect_vert: bool

    # Boolean indication of whether this Instance is reflected in direction `dir`
    def reflected(self, dir_: Dir) -> bool:
        if dir_ == Dir.Horiz:
            return self.reflect_horiz
        if dir_ == Dir.Vert:
            return self.reflect_vert
        raise ValueError

    # Size of the Instance's rectangular `boundbox`, i.e. the zero-origin `boundbox` of its `cell`.
    def boundbox_size(self) -> "Xy[PrimPitches]":
        return self.cell.boundbox_size()

    def __repr__(self):
        return f"Instance(name=:self.inst_name, cell=:self.cell.name, loc=:self.loc)"

    # Retrieve this Instance's bounding rectangle, specified in [PrimPitches].
    # Instance location must be resolved to absolute coordinates, or this method will fail.
    def boundbox(self) -> "BoundBox[PrimPitches]":
        from .bbox import BoundBox

        loc = self.loc.abs()
        outline = self.cell.outline()

        if self.reflect_horiz:
            (x0, x1) = ((loc.x - outline.xmax(), loc.x),)
        else:
            (x0, x1) = ((loc.x, loc.x + outline.xmax()),)

        if self.reflect_vert:
            (y0, y1) = ((loc.y - outline.ymax(), loc.y),)
        else:
            (y0, y1) = ((loc.y, loc.y + outline.ymax()),)

        return BoundBox(Xy(x0, y0), Xy(x1, y1))

