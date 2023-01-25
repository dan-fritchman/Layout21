from enum import Enum, auto
from typing import List, Union, Optional

from pydantic.dataclasses import dataclass

# Local imports
from .coords import DbUnits, Dir
from .error import LayoutError
from .relz import RelZ


# # Track Reference
#
# Integer-pair representing a pointer to a [Layer] and track-index.
#
@dataclass
class TrackRef:
    layer: int  # Layer Index
    track: int  # Track Index


# # Track Crossing
#
# Located intersection between opposite-direction [Layer]s in [Track]-Space
#
@dataclass
class TrackCross:
    # "Primary" [Track] being referred to
    track: TrackRef
    # Intersecting "secondary" track
    cross: TrackRef

    # Create from four [int], representing the two (layer-index, track-index) pairs.
    def from_parts(layer1: int, index1: int, layer2: int, index2: int) -> "TrackCross":
        return TrackCross(
            track=TrackRef(layer1, index1),
            cross=TrackRef(layer2, index2),
        )

    # Create from a (layer-index, track-index) pair and a [RelZ]
    def from_relz(layer: int, track: int, at: int, relz: RelZ) -> "TrackCross":
        layer2 = layer + 1 if relz == RelZ.Above else layer - 1
        track = TrackRef(layer, track)
        cross = TrackRef(
            layer=layer2,
            track=at,
        )

        return TrackCross(track, cross)


class TrackType(Enum):
    Gap = auto()
    Signal = auto()
    Pwr = auto()
    Gnd = auto()

    def to_string(self) -> str:
        if self == TrackEntry.Pwr:
            return "VDD"
        if self == TrackEntry.Gnd:
            return "VSS"
        raise ValueError


@dataclass
class TrackEntry:
    ttype: TrackType
    width: DbUnits

    # # Helper method: create of [TrackEntry] of [TrackType] [TrackType.Gap]
    # def gap(width: impl Into<DbUnits>) -> Self {
    #     TrackEntry {
    #         width: width.into(),
    #         ttype: TrackType.Gap,
    #     }
    # }
    # # Helper method: create of [TrackEntry] of [TrackType] [TrackType.Signal]
    # def sig(width: impl Into<DbUnits>) -> Self {
    #     TrackEntry {
    #         width: width.into(),
    #         ttype: TrackType.Signal,


# An array of layout `Entries`, repeated `nrep` times
@dataclass
class Repeat:
    entries: List[TrackEntry]
    nrep: int


# # Track "Specification" Entry
#
# Either a single entry, or repitition thereof.
#
TrackSpec = Union[TrackEntry, Repeat]

# def gap(width: impl Into<DbUnits>) -> Self {
#     Self.Entry(TrackEntry {
#         width: width.into(),
#         ttype: TrackType.Gap,
#     })
# }
# def sig(width: impl Into<DbUnits>) -> Self {
#     Self.Entry(TrackEntry {
#         width: width.into(),
#         ttype: TrackType.Signal,
#     })
# }
# def rail(width: impl Into<DbUnits>, rk: RailKind) -> Self {
#     Self.Entry(TrackEntry {
#         width: width.into(),
#         ttype: TrackType.Rail(rk),
#     })
# }
# def pwr(width: impl Into<DbUnits>) -> Self {
#     Self.Entry(TrackEntry {
#         width: width.into(),
#         ttype: TrackType.Rail(RailKind.Pwr),
#     })
# }
# def gnd(width: impl Into<DbUnits>) -> Self {
#     Self.Entry(TrackEntry {
#         width: width.into(),
#         ttype: TrackType.Rail(RailKind.Gnd),
#     })
# }
# def repeat(e: impl Into<Vec<TrackEntry>>, nrep: int) -> Self {
#     Self.Repeat(Repeat(e, nrep))
