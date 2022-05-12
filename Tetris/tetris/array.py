#
# # Layout Arrays
#
# Uniformly-spaced repetitions of [Arrayable] elements.
#

from pydantic.dataclasses import dataclass

# Local imports
from .coords import PrimPitches, Xy
from .separation import Separation

# from .instantiable import Instantiable


@dataclass
class Array:
    """ # Array 
    A Uniform-Spaced Array of Identical [`Instantiable`] Elements """

    # Array Name
    name: str
    # Unit to be Arrayed
    unit: "Instantiable"
    # Number of elements
    count: int
    # Separation between elements
    # FIXME: whether to include the size of the element or not
    sep: Separation

    # Size of the Array's rectangular `boundbox` i.e. the zero-origin `boundbox` of its `cell`.
    def boundbox_size(self) -> Xy[PrimPitches]:
        _unit = self.unit.boundbox_size()
        raise NotImplementedError  # FIXME!

