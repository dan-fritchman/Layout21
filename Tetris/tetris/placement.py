#
# # Layout21 Placement Module
#

from typing import Union, Any, Optional

from pydantic.dataclasses import dataclass

# Local imports
from .coords import PrimPitches, Xy
from .instance import Instance
from .side import Side
from .align import Align
from .separation import Separation


# # Relatively-Placed Assignment
# FIXME: merge back in with absoutely-placed [Assign]
@dataclass
class RelAssign:
    net: str
    loc: "RelativePlace"


@dataclass
class Port:
    """Reference to the Location of a Port"""

    inst: Instance
    port: str


# Place-able types union
Placeable = Union[Instance, RelAssign, Port]


# Get the location of the placeable
def loc(self: Placeable) -> "Place":
    if isinstance(self, Instance):
        return self.loc
    if isinstance(self, (Port, RelAssign)):
        raise NotImplementedError  # FIXME!
    raise TypeError




@dataclass
class AbsPlace:
    """Absolute-Valued Placement, in Primitive Pitches"""

    xy: Xy ## FIXME: [PrimPitches]

    @property
    def x(self) -> PrimPitches:
        return self.xy.x

    @property
    def y(self) -> PrimPitches:
        return self.xy.y

    @staticmethod
    def xy(x: int, y: int) -> "AbsPlace":
        return AbsPlace(xy=Xy(x=PrimPitches.x(x), y=PrimPitches.y(y)))


# # Relative Placement
@dataclass
class RelativePlace:
    # Placement is relative `to` this
    to: Any  ## FIXME: Placeable
    # Placed on this `side` of `to`
    side: Side
    # Aligned to this aspect of `to`
    align: Align
    # Separation between the placement and the `to`
    sep: Separation

    resolved: Optional[AbsPlace] = None

# # Placement Union
#
# Includes absolute and relative placements.
#
# Absolute placements are in `Self.AbsType` units.
# Relative placements use the [RelativePlace] struct,
# which can be specified relative to any other [Placeable] object.
#
Place = Union[AbsPlace, RelativePlace]

