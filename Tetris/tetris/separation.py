from typing import Union, List, Optional

from pydantic.dataclasses import dataclass

# Local imports
from .coords import UnitSpeced, Dir

# from .instantiable import Instantiable


@dataclass
class SizeOf:
    """# The size of `of`."""

    ...  # FIXME!
    # of: "Instantiable"


# Enumerated means of specifying x-y relative-placement separation
SepBy = Union[UnitSpeced, SizeOf]


@dataclass
class Separation:
    """# Three-dimensional separation"""

    x: Optional[SepBy] = None
    y: Optional[SepBy] = None
    z: Optional[int] = None

    @staticmethod
    def zero() -> "Separation":
        return Separation()

    @staticmethod
    def by_x(x: SepBy) -> "Separation":
        return Separation(x=x)

    @staticmethod
    def by_y(y: SepBy) -> "Separation":
        return Separation(y=y)

    @staticmethod
    def by_z(z: int) -> "Separation":
        return Separation(z=z)

    # Get the separation in direction `dir`
    def dir(self, dir_: Dir) -> Optional[SepBy]:
        if dir_ == Dir.Horiz:
            return self.x
        if dir_ == Dir.Vert:
            return self.y
        raise ValueError
