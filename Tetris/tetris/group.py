#
# # Layout Element Groups
#
# The primary [Group] type is a set of named, located elements
# which can be placed and moved together.
#

from typing import List

from pydantic.dataclasses import dataclass

# Local imports
from .coords import PrimPitches, Xy
from .instance import Instance


# Named group of placeable elements
@dataclass
class Group:
    # Group Name
    name: str
    # Constituent Elements
    elements: List[Instance]

    # Size of the Instance's rectangular `boundbox`, i.e. the zero-origin `boundbox` of its `cell`.
    def boundbox_size(self) -> Xy[PrimPitches]:
        raise NotImplementedError

