#
# # Abstract Layout Module
#
# [Abstract] layouts describe a [Cell]'s outline and physical interface, without exposing implementation details.
# [Cell]-[Abstract]s primarily comprise their outlines and pins.
# Outlines follow the same "Tetris-Shapes" as `layout21.tetris` layout cells, including the requirements for a uniform z-axis.
# Internal layers are "fully blocked", in that parent layouts may not route through them.
# In legacy layout systems this would be akin to including blockages of the same shape as [Outline] on each layer.
#
# Sadly the english-spelled name "abstract" is reserved as a potential
# [future Rust keyword](https:#doc.rust-lang.org/reference/keywords.html#reserved-keywords),
# and is hence avoided as an identifier throughout Layout21.
#

from enum import Enum, auto
from typing import List, Union, Optional, Tuple

from pydantic.dataclasses import dataclass

# Local imports
from .outline import Outline
from .relz import RelZ

# A location (track intersection) on our top z-axis layer
@dataclass
class TopLoc:
    # Track Index
    track: int
    # Intersecting Track Index
    at: int
    # Whether `at` refers to the track-indices above or below
    relz: RelZ


# # Port Side Enumeration
#
# Note there are only two such sides: the "origin-side" [BottomOrLeft] and the "width-side" [TopOrRight].
# Each [Layer]'s orientation ([Dir]) dictates between bottom/left and top/right.
# Also note the requirements on [Outline] shapes ensure each track has a unique left/right or top/bottom pair of edges.
#
@dataclass
class Side(Enum):
    BottomOrLeft = auto()
    TopOrRight = auto()


# Ports which connect on x/y outline edges
@dataclass
class Edge:
    layer: int
    track: int
    side: Side


# Ports accessible from bot top *and* top-layer edges
# Note their `layer` field is implicitly defined as the cell's `metals`.
@dataclass
class ZTopEdge:
    # Track Index
    track: int
    # Side
    side: Side
    # Location into which the pin extends inward
    into: Tuple[int, RelZ]


# Ports which are internal to the cell outline,
# but connect from above in the z-stack.
# These can be assigned at several locations across their track,
# and are presumed to be internally-connected between such locations.
@dataclass
class ZTopInner:
    # Locations
    locs: List[TopLoc]


# Abstract-Layout Port Inner Detail
#
# All location and "geometric" information per Port is stored here,
# among a few enumerated variants.
#
# Ports may either connect on x/y edges, or on the top (in the z-axis) layer.
PortKind = Union[Edge, ZTopInner, ZTopEdge]


# Abstract-Layout Port
@dataclass
class Port:
    # Port/ Signal Name
    name: str
    # Physical Info
    kind: PortKind


# Abstract-Layout
@dataclass
class Abstract:
    # Cell Name
    name: str
    # Outline in "Tetris-Shapes"
    outline: Outline
    # Number of Metal Layers Used
    metals: int
    # Ports
    ports: List[Port]

    # Retrieve a reference to a port by name.
    # Returns `None` if no port with that name exists.
    def port(self, name: str) -> Optional[Port]:
        for port in self.ports:
            if port.name == name:
                return port
        return None
