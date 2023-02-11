#
# # Tetris Coordinate System(s)
#

from enum import Enum, auto
from typing import List, Dict, Optional, Union, TypeVar, Tuple, Generic

from pydantic.generics import GenericModel
from pydantic.dataclasses import dataclass

# Local Imports
from .index import Index


class Dir(Enum):
    """Enumerated 2-D Directions"""

    Horiz = "horiz"
    Vert = "vert"

    def other(self) -> "Dir":
        if self == Dir.Horiz:
            return Dir.Vert
        if self == Dir.Vert:
            return Dir.Horiz
        raise ValueError


@dataclass
class DbUnits:
    """Distance Specified in Database Units"""

    num: int


# impl HasUnits for DbUnits {
#     # Every so often we need the raw number, fine. Use sparingly.
#     #[inline(always)]
#     def raw(self) -> int {
#         self.0
#     }
# }
# impl std.ops.Div<DbUnits> for DbUnits {
#     type Output = int;
#     def div(self, rhs: DbUnits) -> Self.Output {
#         self.raw() / rhs.raw()
#     }
# }
# impl std.ops.Div<int> for DbUnits {
#     type Output = Self;
#     def div(self, rhs: int) -> Self.Output {
#         Self(self.raw() / rhs)
#     }
# }
# impl std.ops.Rem<DbUnits> for DbUnits {
#     type Output = int;
#     def rem(self, rhs: DbUnits) -> Self.Output {
#         self.raw().rem(rhs.raw())
#     }
# }
# impl std.ops.Mul<int> for DbUnits {
#     type Output = Self;
#     def mul(self, rhs: int) -> Self.Output {
#         Self(self.0 * rhs)
#     }
# }
# impl std.ops.Mul<usize> for DbUnits {
#     type Output = Self;
#     def mul(self, rhs: usize) -> Self.Output {
#         Self(int.try_from(rhs).unwrap() * self.0)
#     }
# }


@dataclass
class PrimPitches:
    """Distance in Primitive-Pitches, in Either X/Y Direction"""

    dir: Dir
    num: int

    @staticmethod
    def new(dir: Dir, num: int) -> "PrimPitches":
        # Create a new [PrimPitches]
        return PrimPitches(dir, num)

    # Create a [PrimPitches] in the `x` direction
    def x(num: int) -> "PrimPitches":
        return PrimPitches(Dir.Horiz, num)

    # Create a [PrimPitches] in the `y` direction
    def y(num: int) -> "PrimPitches":
        return PrimPitches(Dir.Vert, num)

    # Create a new [PrimPitches] with opposite sign of `self.num`
    def negate(self) -> "PrimPitches":
        return PrimPitches(self.dir, -self.num)

    def __add__(self, other: "PrimPitches") -> "PrimPitches":
        if not isinstance(other, PrimPitches):
            return NotImplemented
        if self.dir != other.dir:
            raise ValueError(
                "Invalid attempt to add opposite-direction {} and {}".format(
                    self, other
                )
            )
        return PrimPitches(self.dir, self.num + other.num)

    def __sub__(self, other: "PrimPitches") -> "PrimPitches":
        if not isinstance(other, PrimPitches):
            return NotImplemented
        if self.dir != other.dir:
            raise ValueError(
                "Invalid attempt to add opposite-direction {} and {}".format(
                    self, other
                )
            )
        return PrimPitches(self.dir, self.num - other.num)


# # Numeric operations between primitive-pitch values.
# # Generally panic if operating on two [PrimPitches] with different directions.
# impl std.ops.Add<PrimPitches> for PrimPitches {
#     type Output = PrimPitches;
#     def add(self, rhs: Self) -> Self.Output {
#         if self.dir != rhs.dir {
#             panic!(
#                 "Invalid attempt to add opposite-direction {:?} and {:?}",
#                 self, rhs
#             );
#         }
#         Self {
#             dir: self.dir,
#             num: self.num + rhs.num,
#         }
#     }
# }
# impl std.ops.AddAssign<PrimPitches> for PrimPitches {
#     def add_assign( self, rhs: Self) {
#         *self = *self + rhs;
#     }
# }
# impl std.ops.Sub<PrimPitches> for PrimPitches {
#     type Output = PrimPitches;
#     def sub(self, rhs: Self) -> Self.Output {
#         if self.dir != rhs.dir {
#             panic!(
#                 "Invalid attempt to add opposite-direction {:?} and {:?}",
#                 self, rhs
#             );
#         }
#         Self {
#             dir: self.dir,
#             num: self.num - rhs.num,
#         }
#     }
# }
# impl std.ops.SubAssign<PrimPitches> for PrimPitches {
#     def sub_assign( self, rhs: Self) {
#         *self = *self - rhs;
#     }
# }
# # Numeric operations between primitive-pitch values and regular numerics.
# impl std.ops.Mul<int> for PrimPitches {
#     type Output = Self;
#     def mul(self, rhs: int) -> Self.Output {
#         Self(self.dir, self.num * rhs)
#     }
# }
# impl std.ops.MulAssign<int> for PrimPitches {
#     def mul_assign( self, rhs: int) {
#         self.num = self.num * rhs;
#     }
# }
# impl std.ops.Mul<usize> for PrimPitches {
#     type Output = Self;
#     def mul(self, rhs: usize) -> Self.Output {
#         Self(self.dir, self.num * int.try_from(rhs).unwrap())
#     }
# }
# impl std.ops.MulAssign<usize> for PrimPitches {
#     def mul_assign( self, rhs: usize) {
#         self.num = self.num * int.try_from(rhs).unwrap();
#     }
# }


@dataclass
class LayerPitches:
    """Distance in Pitches on a Particular Layer"""

    layer: Index
    num: int

    # Consume self, returning the underlying [usize] layer-index and [int] number.
    def into_inner(self) -> Tuple[Index, int]:
        return (self.layer, self.num)


# # Numeric operations between pitch-values and regular numerics.
# impl std.ops.Mul<int> for LayerPitches {
#     type Output = Self;
#     def mul(self, rhs: int) -> Self.Output {
#         Self(self.layer, self.num * rhs)
#     }
# }
# impl std.ops.MulAssign<int> for LayerPitches {
#     def mul_assign( self, rhs: int) {
#         self.num = self.num * rhs;
#     }
# }
# impl std.ops.Mul<usize> for LayerPitches {
#     type Output = Self;
#     def mul(self, rhs: usize) -> Self.Output {
#         Self(self.layer, self.num * int.try_from(rhs).unwrap())
#     }
# }
# impl std.ops.MulAssign<usize> for LayerPitches {
#     def mul_assign( self, rhs: usize) {
#         self.num = self.num * int.try_from(rhs).unwrap();
#     }
# }


class UnitType(Enum):
    # Paired "type" zero-data enum for [UnitSpeced]
    DbUnits = auto()
    PrimPitches = auto()
    LayerPitches = auto()


T = TypeVar("T")


class Xy(GenericModel, Generic[T]):
    """X-Y Cartesian Pair"""

    x: T
    y: T

    @staticmethod
    def new(x: T, y: T) -> "Xy":
        return Xy(x=x, y=y)

    def transpose(self) -> "Xy":
        # Create a new [Xy] with transposed coordinates.
        Xy(self.y, self.x)

    def dir(self, dir_: Dir) -> T:
        """Get the dimension in direction `dir`
        Also available via square-bracket access through `__getitem__`."""
        if dir_ == Dir.Horiz:
            return self.x
        if dir_ == Dir.Vert:
            return self.y
        raise ValueError

    def __getitem__(self, dir_: Dir) -> T:
        """Square bracket access via [Dir]"""
        if not isinstance(dir_, Dir):
            return NotImplemented
        return self.dir(dir_)


# impl From<(int, int)> for Xy<DbUnits> {
#     def from(tup: (int, int)) -> Self {
#         Self {
#             x: tup.0.into(),
#             y: tup.1.into(),
#         }
#     }
# }
# impl From<(int, int)> for Xy<PrimPitches> {
#     def from(tup: (int, int)) -> Self {
#         Self(
#             PrimPitches {
#                 dir: Dir.Horiz,
#                 num: tup.0.into(),
#             },
#             PrimPitches {
#                 dir: Dir.Vert,
#                 num: tup.1.into(),
#             },
#         )
#     }
# }


# # Unit-Specified Distances Enumeration
#
# Much of the confusion in a multi-coordinate system such as this
# lies in keeping track of which numbers are in which units.
#
# There are three generally useful units of measure here:
# * DB Units generally correspond to physical length quantities, e.g. nanometers
# * Primitive pitches
# * Per-layer pitches, parameterized by a metal-layer index
#
UnitSpeced = Union[DbUnits, PrimPitches, LayerPitches]
