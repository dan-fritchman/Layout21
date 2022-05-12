from enum import Enum, auto
from typing import List, Optional
from dataclasses import dataclass

from pydantic.dataclasses import dataclass

# Local imports
from .index import Index
from .units import Units
from .coords import DbUnits, Xy, Dir
from .track_spec import TrackCross, TrackSpec, TrackEntry, Repeat


# # Via Targets
#
# Enumerates the things vias can "go between".
# Generally either a numbered metal layer, or the primitive base-layers.
#
# Values stored in the `Metal` variant are treated as indicies into `Stack.metals`,
# i.e. `Metal(0)` is the first metal layer defined in the stack.
ViaTarget = Optional[Index]

# # Via / Insulator Layer Between Metals
@dataclass
class ViaLayer:
    # Layer name
    name: str
    # Top of the two layers connected by this layer
    top: ViaTarget
    # Bottom of the two layers connected by this layer
    bot: ViaTarget
    # Via size
    size: Xy[DbUnits]

    # FIXME: `raw` stuff
    # # Stream-out layer numbers
    # raw: Option<raw::LayerKey>


# Assignment of a net onto a track-intersection
@dataclass
class Assign:
    # Net Name
    net: str
    # Track Intersection Location
    at: TrackCross


# Indication of whether a layer flips in its periodic axis with every period,
# as most standard-cell-style logic gates do.
class FlipMode(Enum):
    EveryOther = auto()
    NoFlip = auto()


# Indication of whether a layer is owned by, partially included in, or external to the primitive blocks
class PrimitiveMode(Enum):
    # Owned by Primitives
    Prim = auto()
    # Partially split between Primitives and Stack
    Split = auto()
    # Owned by the Stack
    Stack = auto()


# Description of the primitive-level cells in a [Stack]
@dataclass
class PrimitiveLayer:
    pitches: Xy[DbUnits]


# # MetalLayer
#
# Metal layer in a [Stack]
# Each layer is effectively infinite-spanning in one dimension, and periodic in the other.
# Layers with `dir=Dir::Horiz` extend to infinity in x, and repeat in y, and vice-versa.
#
@dataclass
class MetalLayer:
    # Layer Name
    name: str
    # Direction Enumeration (Horizontal/ Vertical)
    dir: Dir
    # Default size of wire-cuts
    cutsize: DbUnits
    # Track Size  Type Entries
    entries: List[TrackSpec]
    # Offset in our periodic dimension
    offset: DbUnits
    # Overlap between periods
    overlap: DbUnits
    # Setting for period-by-period flipping
    flip: FlipMode
    # Primitive-layer relationship
    prim: PrimitiveMode

    # FIXME: handling `raw` stuff
    # # [raw::Layer] for exports
    # raw: Option<raw::LayerKey>

    # Sum up this [Layer]'s pitch
    def pitch(self) -> DbUnits:
        return sum(self.flatten()) - self.overlap

    # Flatten our [Entry]s into a vector
    # Removes any nested patterns
    def flatten(self) -> List[TrackEntry]:
        v: List[TrackEntry] = list()
        for e in self.entries:
            if isinstance(e, Repeat):
                for _ in range(e.nrep):
                    for ee in e.entries:
                        v.push(ee)
            elif isinstance(e, TrackEntry):
                v.push(e)
            else:
                raise TypeError
        return v


# # Stack
#
# The z-stack, primarily including metal, via, and primitive layers
@dataclass
class Stack:
    # Measurement units
    units: Units
    # Primitive Layer
    prim: PrimitiveLayer
    # Set of metal layers
    metals: List[MetalLayer]
    # Set of via layers
    vias: List[ViaLayer]

    # FIXME: how to handle `raw` thing
    # # [raw::Layer] Mappings
    # rawlayers: Option<Ptr<raw::Layers>>
    # # Layer used for cell outlines/ boundaries
    # boundary_layer: Optional<raw::LayerKey>

    # Run validation, creating a [ValidStack]
    def validate(self) -> "ValidStack":
        from .validate import validate_stack

        return validate_stack(self)
