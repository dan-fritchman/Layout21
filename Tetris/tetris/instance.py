#
# # Instance Structures
#
# Located, oriented instances of other cells or similar reusable layout objects.
#

from dataclasses import dataclass, field

# Local imports
from .coords import PrimPitches, Xy, Dir
from .bbox import BoundBox
from .reflect import Reflect

# from .placement import Place
# from .instantiable import Instantiable


@dataclass  # Note this is a std-lib dataclass, not pydantic
class Instance:
    """Instance of another Cell, Group, or Array"""

    # Instance Name
    name: str

    # Target `Cell`, `Group`, `Array`, or other `Instantiable`
    of: "Instantiable"

    # Location of the Instance origin
    # This origin-position holds regardless of either `reflect` field.
    # If specified in absolute coordinates location-units are [PrimPitches].
    loc: "Place" = field()

    # Reflect
    reflect: Reflect = field(default_factory=Reflect.default)

    # Boolean indication of whether this Instance is reflected in direction `dir`
    def reflected(self, dir_: Dir) -> bool:
        return self.reflect.reflected(dir_)

    # Size of the Instance's rectangular `boundbox`, i.e. the zero-origin `boundbox` of its `cell`.
    def boundbox_size(self) -> "Xy[PrimPitches]":
        return self.of.boundbox_size()

    def __repr__(self):
        return f"Instance(name=:self.name, cell=:self.of.name, loc=:self.loc)"

    def boundbox(self) -> BoundBox[PrimPitches]:
        """# Retrieve this Instance's bounding rectangle, specified in [PrimPitches].
        # Instance location must be resolved to absolute coordinates, or this method will fail."""
        from .placement import AbsPlace, RelativePlace

        if isinstance(self.loc, RelativePlace):
            if self.loc.resolved is None:
                msg = f"Instance location must be resolved to absolute coordinates"
                raise RuntimeError(msg)
            abs_loc = self.loc.resolved
        elif isinstance(self.loc, AbsPlace):
            abs_loc = self.loc
        else:
            raise TypeError

        outline = self.of.outline()

        if self.reflect.horiz:
            (x0, x1) = (abs_loc.x - outline.xmax(), abs_loc.x)
        else:
            (x0, x1) = (abs_loc.x, abs_loc.x + outline.xmax())

        if self.reflect.vert:
            (y0, y1) = (abs_loc.y - outline.ymax(), abs_loc.y)
        else:
            (y0, y1) = (abs_loc.y, abs_loc.y + outline.ymax())

        return BoundBox(mins=Xy(x=x0, y=y0), maxs=Xy(x=x1, y=y1))
