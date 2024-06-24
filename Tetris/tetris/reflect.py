from pydantic.dataclasses import dataclass

# Local imports
from .coords import PrimPitches, Xy, Dir
from .bbox import BoundBox


@dataclass
class Reflect:
    """# Reflection-State"""

    horiz: bool  # Reflected horizontally
    vert: bool  # Reflected vertically

    @staticmethod
    def default() -> "Reflect":
        """The default, non-reflected state."""
        return Reflect(horiz=False, vert=False)

    def reflected(self, dir_: Dir) -> bool:
        """Boolean indication of whether reflected in direction `dir`."""
        if dir_ == Dir.Horiz:
            return self.horiz
        if dir_ == Dir.Vert:
            return self.vert
        raise ValueError(f"Invalid direction: {dir_}")

    def __getitem__(self, dir_: Dir) -> bool:
        """Square bracket access. Boolean indication of whether reflected in direction `dir`."""
        return self.reflected(dir_)
