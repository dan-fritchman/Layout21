#
# # Cell Definition
#
# Defines the [Cell] type, which represents a multi-viewed piece of reusable hardware.
# [Cell]s can, and generally do, have one or more associated "views",
# including [Abstract]s, [Layout], interface definitions, and/or "raw" layouts.
#


from typing import List, Set, Dict, Type, Union, Optional

from pydantic.dataclasses import dataclass

# Local imports
from .index import Index
from .coords import PrimPitches, Xy
from .layout import Layout
from .outline import Outline
from .error import LayoutError
from .bundle import Bundle
from .abstract import Abstract

# "Pointer" to a raw (lib, cell) combination.
# Wraps with basic [Outline] and `metals` information to enable bounded placement.
@dataclass
class RawLayoutPtr:
    ...  # FIXME! handle `raw` stuff

    # # Outline shape, counted in x and y pitches of `stack`
    # outline: Outline
    # # Number of Metal Layers Used
    # metals: int
    # # Pointer to the raw Library
    # lib: Ptr<raw.Library>
    # # Pointer to the raw Cell
    # cell: Ptr<raw.Cell>


# # Cell View Enumeration
# All of the ways in which a Cell is represented
CellView = Union[
    Bundle, Abstract, Layout, RawLayoutPtr,
]

# Collection of the Views describing a Cell
@dataclass
class Cell:
    # Cell Name
    name: str
    # Interface
    interface: Optional[Bundle] = None
    # Layout Abstract
    abs: Optional[Abstract] = None
    # Layout Implementation
    layout: Optional[Layout] = None

    # # Raw Layout
    # # FIXME: this should probably move "up" a level
    # # so that cells are either defined as `raw` or `tetris` implementations
    # # but not both
    # raw: Option<RawLayoutPtr>

    # Add [CellView] `view` to our appropriate type-based field.
    def add_view(self, view: CellView):
        if isinstance(view, Layout):
            self.layout = view
        elif isinstance(view, Abstract):
            self.abs = view
        elif isinstance(view, Bundle):
            self.interface = view
        raise TypeError

    # Create from a list of [CellView]s and a name.
    def from_views(name: str, views: List[CellView]) -> "Cell":
        myself = Cell(name=name)
        for view in views:
            myself.add_view(view)
        return myself

    # Return whichever view highest-prioritorily dictates the outline
    def outline(self) -> Outline:
        # We take the "most abstract" view for the outline
        # (although if there are more than one, they better be the same...
        # FIXME: this should be a validation step.)
        # Overall this method probably should move to a "validated" cell in which each view is assured consistent.
        if self.abs is not None:
            return self.abs.outline
        elif self.layout is not None:
            return self.layout.outline
        raise LayoutError(
            "Failed to retrieve outline of cell : with no abstract or implementation",
        )

    # Size of the [Cell]'s rectangular `boundbox`.
    def boundbox_size(self) -> Xy[PrimPitches]:
        outline = self.outline()
        return Xy.new(outline.xmax(), outline.ymax())

    # Return whichever view highest-prioritorily dictates the top-layer
    def metals(self) -> int:
        # FIXME: same commentary as `outline` above
        if self.abs is not None:
            return self.abs.metals
        elif self.layout is not None:
            return self.layout.metals
        raise LayoutError(
            "Failed to retrieve metal-layers of cell : with no abstract or implementation",
        )

    # Get the cell's top metal layer (numer).
    # Returns `None` if no metal layers are used.
    def top_metal(self) -> Optional[Index]:
        metals = self.metals()
        if metals == 0:
            return None
        else:
            return Index(metals - 1)

