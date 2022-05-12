#
# # Instance Structures
#
# Located, oriented instances of other cells or similar reusable layout objects.
#

from pydantic.dataclasses import dataclass

# Local imports
from .coords import PrimPitches, Xy, Dir
from .bbox import BoundBox

# from .placement import Place
# from .instantiable import Instantiable


@dataclass
class Reflection:
    """ Reflection-State of an instance """

    # Horizontal Reflection State
    horiz: bool
    # Vertical Reflection State
    vert: bool

    def reflected(self, dir_: Dir) -> bool:
        """ Boolean indication of whether reflected in direction `dir`. """
        if dir_ == Dir.Horiz:
            return self.horiz
        if dir_ == Dir.Vert:
            return self.vert
        raise ValueError

    def __getitem__(self, dir_: Dir) -> bool:
        """ Square bracket access. Boolean indication of whether reflected in direction `dir`. """
        return self.reflected(dir_)


@dataclass
class Instance:
    """ Instance of another Cell, Group, or Array"""

    # Instance Name
    inst_name: str

    # Target `Cell`, `Group`, `Array`, or other `Instantiable`
    of: "Instantiable"

    # Location of the Instance origin
    # This origin-position holds regardless of either `reflect` field.
    # If specified in absolute coordinates location-units are [PrimPitches].
    loc: "Place"

    # Reflection
    reflect: Reflection

    # Boolean indication of whether this Instance is reflected in direction `dir`
    def reflected(self, dir_: Dir) -> bool:
        return self.reflect.reflected(dir_)

    # Size of the Instance's rectangular `boundbox`, i.e. the zero-origin `boundbox` of its `cell`.
    def boundbox_size(self) -> "Xy[PrimPitches]":
        return self.of.boundbox_size()

    def __repr__(self):
        return f"Instance(name=:self.inst_name, cell=:self.of.name, loc=:self.loc)"

    # Retrieve this Instance's bounding rectangle, specified in [PrimPitches].
    # Instance location must be resolved to absolute coordinates, or this method will fail.
    def boundbox(self) -> BoundBox[PrimPitches]:

        loc = self.loc.abs()
        outline = self.of.outline()

        if self.reflect.horiz:
            (x0, x1) = ((loc.x - outline.xmax(), loc.x),)
        else:
            (x0, x1) = ((loc.x, loc.x + outline.xmax()),)

        if self.reflect.vert:
            (y0, y1) = ((loc.y - outline.ymax(), loc.y),)
        else:
            (y0, y1) = ((loc.y, loc.y + outline.ymax()),)

        return BoundBox(Xy(x0, y0), Xy(x1, y1))

