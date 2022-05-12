#
# # Rectangular Bounding Boxes
#

from typing import TypeVar, Generic, Tuple

from pydantic.generics import GenericModel

# Local imports
from .coords import Xy
from .side import Side


T = TypeVar("T")


# # Bounding Rectangular Box
class BoundBox(GenericModel, Generic[T]):
    p0: Xy[T]
    p1: Xy[T]

    # Retrieve our coordinate at [Side] `side`.
    def side(self, side: Side) -> T:
        if side == Side.Left:
            return (self.p0.x,)
        if side == Side.Right:
            return (self.p1.x,)
        if side == Side.Bottom:
            return (self.p0.y,)
        if side == Side.Top:
            return (self.p1.y,)
        raise ValueError

    # Create a new [BoundBox] from potentially unordered pairs of x and y coordinates.
    def from_xy(xs: Tuple[T, T], ys: Tuple[T, T]) -> "BoundBox":
        (x0, x1) = (xs[0], xs[1]) if xs[0] < xs[1] else (xs[1], xs[0])
        (y0, y1) = (ys[0], ys[1]) if ys[0] < ys[1] else (ys[1], ys[0])
        return BoundBox(Xy(x0, y0), Xy(x1, y1))

