#
# # Layout-Cell Definitions
#
# Physical implementations of tetris [Cell]s.
#
from typing import List
from dataclasses import field

from pydantic.dataclasses import dataclass

# Local imports
from .outline import Outline
from .stack import Assign
from .relz import RelZ
from .track_spec import TrackCross
from .instance import Instance


@dataclass
class Layout:
    """
    # Layout Cell Implementation
    A combination of hierarchical instances and net-assignments to tracks.
    """

    # Cell Name
    name: str
    # Number of Metal Layers Used
    metals: int
    # Outline shape counted in x and y pitches of `stack`
    outline: Outline

    # Instances of other layout objects (cells, arrays, etc.)
    instances: List[Instance] = field(default_factory=list)
    # Net-to-track assignments
    assignments: List[Assign] = field(default_factory=list)
    # Track cuts
    cuts: List[TrackCross] = field(default_factory=list)

    # Assign a net at the given coordinates.
    def assign(
        self, net: str, layer: int, track: int, at: int, relz: RelZ,
    ) -> None:
        at = TrackCross.from_relz(layer, track, at, relz)
        self.assignments.append(Assign(net, at))

    # Add a cut at the specified coordinates.
    def cut(self, layer: int, track: int, at: int, relz: RelZ):
        cut = TrackCross.from_relz(layer, track, at, relz)
        self.cuts.append(cut)

    # Get a temporary handle for net assignments
    def net(self, net: str) -> "NetHandle":
        from .net_handle import NetHandle

        return NetHandle(name=net, parent=self)
