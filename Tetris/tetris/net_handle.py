from pydantic.dataclasses import dataclass

# Local imports
from .layout import Layout
from .relz import RelZ

# # Net Handle
#
# A short-term handle for chaining multiple assignments to a net
# Typically used as: `mycell.net("name").at(/* args */).at(/* more args */)`
# Takes an exclusive reference to its parent [Layout],
# so generally must be dropped quickly to avoid locking it up.
#
@dataclass
class NetHandle:
    name: str
    parent: Layout

    # Assign our net at the given coordinates.
    # Consumes and returns `self` to enable chaining.
    def at(self, layer: int, track: int, at: int, relz: RelZ) -> "NetHandle":
        self.parent.assign(self.name, layer, track, at, relz)
        return self
