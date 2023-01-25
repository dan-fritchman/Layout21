#
# # Rectangular Bounding Boxes
#

from typing import TypeVar, Generic, Tuple

from pydantic.generics import GenericModel

# Local imports
from .coords import Xy
from .side import Side


T = TypeVar("T")


class BoundBox(GenericModel, Generic[T]):
    """# Rectangular Bounding Box"""

    mins: Xy[T]  # Minimum (x,y) coordinates
    maxs: Xy[T]  # Maximum (x,y) coordinates

    @property
    def top(self) -> T:
        return self.maxs.y

    @property
    def bot(self) -> T:
        return self.mins.y

    @property
    def bottom(self) -> T:
        return self.mins.y

    @property
    def left(self) -> T:
        return self.mins.x

    @property
    def right(self) -> T:
        return self.maxs.x

    def side(self, side: Side) -> T:
        """# Retrieve our coordinate at [Side] `side`."""
        if side == Side.Left:
            return self.mins.x
        if side == Side.Right:
            return self.maxs.x
        if side == Side.Bottom:
            return self.mins.y
        if side == Side.Top:
            return self.maxs.y
        raise ValueError(f"Invalid side: {side}")

    def from_xy(xs: Tuple[T, T], ys: Tuple[T, T]) -> "BoundBox":
        """# Create a new [BoundBox] from potentially unordered pairs of x and y coordinates."""
        (x0, x1) = (xs[0], xs[1]) if xs[0] < xs[1] else (xs[1], xs[0])
        (y0, y1) = (ys[0], ys[1]) if ys[0] < ys[1] else (ys[1], ys[0])
        return BoundBox(Xy(x0, y0), Xy(x1, y1))
