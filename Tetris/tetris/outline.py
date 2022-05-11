#
# # Tetris-Cell Outlines
#

from typing import List

from pydantic.dataclasses import dataclass

from .coords import PrimPitches, Dir
from .error import LayoutError

# # Block Outline
#
# All block-outlines are "tetris shaped" rectilinear polygons, and are `layout21.tetris`'s namesake.
#
# These boundaries are closed, consist solely of 90-degree rectangular turns,
# and are specified by a counter-clockwise set of points.
# "Holes" such as the shapes "O" and "8" and "divots" such as the shapes "U" and "H" are not supported.
#
# Two equal-length vectors `x` and `y` describe an Outline's points.
# Counter-clockwise-ness and divot-free-ness requires that:
# * (a) `x` values are monotonically non-increasing, and
# * (b) `y` values are monotonically non-decreasing
#
# Such an outline has vertices in Cartesian space at:
# `[(0,0), (x[0], 0), (x[0], y[0]), (x[1], y[0]), ... , (0, y[-1]), (0,0)]`
# With the first point at the origin, the final point at (0, y[-1]), and its connection back to the origin all implied.
#
# Example: a rectangular Outline would requires single entry for each of `x` and `y`,
# at the rectangle's vertex opposite the origin in both axes.
#
@dataclass
class Outline:
    x: List[PrimPitches]
    y: List[PrimPitches]

    # Outline constructor from primitive-pitches
    def from_prim_pitches(x: List[PrimPitches], y: List[PrimPitches]) -> "Outline":
        # Check that x and y are of compatible lengths
        if x.len() < 1 or x.len() != y.len():
            raise LayoutError("Invalid zero-length Outline dimensions")

        # Check for:
        # * Correct directions
        # * all non-negative values
        for k in 0.0 .x.len():
            if x[k].dir != Dir.Horiz or y[k].dir != Dir.Vert:
                raise LayoutError("Invalid Outline direction(s)")

            if x[k].num < 0 or y[k].num < 0:
                raise LayoutError("Invalid Outline with negative coordinate(s)")

        # Check for:
        # * x non-increasing-ness,
        # * y for non-decreasing-ness
        for k in 1.0 .x.len():
            if x[k].num > x[k - 1].num:
                raise LayoutError("Invalid Outline with non-increasing x-coordinates")

            if y[k].num < y[k - 1].num:
                raise LayoutError("Invalid Outline with non-decreasing y-coordinates")

        return Outline(x, y)

    @staticmethod
    def rect(x: int, y: int) -> "Outline":
        # Create a new rectangular outline of dimenions `x` by `y`
        return Outline([x], [y])

    # Maximum x-coordinate
    # (Which is also always the *first* x-coordinate)
    def xmax(self) -> PrimPitches:
        self.x[0]

    # Maximum y-coordinate
    # (Which is also always the *last* y-coordinate)
    def ymax(self) -> PrimPitches:
        self.y[self.y.len() - 1]

    # Maximum coordinate in [Dir] `dir`
    def max(self, dir_: Dir) -> PrimPitches:
        if dir_ == Dir.Horiz:
            return self.xmax()
        if dir_ == Dir.Vert:
            return self.ymax()
        raise ValueError
