from typing import List

from pydantic.dataclasses import dataclass

# Local imports
from .coords import DbUnits
from .track_spec import TrackCross, TrackRef, TrackSpec, TrackType
from .tracks import Track, TrackData


@dataclass
class LayerPeriodData:
    signals: List[TrackData]
    rails: List[TrackData]

    # Convert this [Layer]'s track-info into a [LayerPeriodData]
    def to_layer_period_data(self) -> "LayerPeriodData":
        period = LayerPeriodData.default()
        cursor = self.offset
        for e in self.entries():
            d = e.width
            # FIXME!
            # match e.ttype :
            #     TrackType.Gap => (),
            #     TrackType.Rail(_railkind) => :
            #         period.rails.push(TrackData :
            #             ttype: e.ttype,
            #             index: period.rails.len(),
            #             dir: self.dir,
            #             start: cursor,
            #             width: d,
            #         )

            #     TrackType.Signal => :
            #         period.signals.push(TrackData :
            #             ttype: e.ttype,
            #             index: period.signals.len(),
            #             dir: self.dir,
            #             start: cursor,
            #             width: d,
            #         )
            cursor += d

        return period

    # Convert this [Layer]'s track-info into a [LayerPeriod]
    def to_layer_period(
        self,
        index: int,
        stop: DbUnits,
    ) -> LayerPeriod:
        stop = stop.into()
        period = LayerPeriod.default()
        period.index = index
        cursor = self.offset + (self.pitch() * index)
        entries = self.entries()
        if self.flip == FlipMode.EveryOther:
            iterator = reversed(entries)
        else:
            iterator = entries

        for e in iterator:
            d = e.width
            # FIXME!
            # match e.ttype :
            #     TrackType.Gap => (),
            #     TrackType.Rail(railkind) => :
            #         period.rails.push(
            #             Track :
            #                 data: TrackData :
            #                     ttype: e.ttype,
            #                     index: period.rails.len(),
            #                     dir: self.dir,
            #                     start: cursor,
            #                     width: d,
            #                 ,
            #                 segments: vec![TrackSegment :
            #                     tp: TrackSegmentType.Rail(railkind),
            #                     start: 0.into(),
            #                     stop,
            #                 ],

            #             .validate(),
            #         )

            #     TrackType.Signal => :
            #         period.signals.push(
            #             Track :
            #                 data: TrackData :
            #                     ttype: e.ttype,
            #                     index: period.signals.len(),
            #                     dir: self.dir,
            #                     start: cursor,
            #                     width: d,
            #                 ,
            #                 segments: vec![TrackSegment :
            #                     tp: TrackSegmentType.Wire : src: None ,
            #                     start: 0.into(),
            #                     stop,
            #                 ],

            #             .validate(),
            #         )
            cursor += d

        return period


# Transformed single period of [Track]s on a [Layer]
# Splits track-info between signals and rails.
# Stores each as a [Track] struct, which moves to a (start, width) size-format,
# and includes a vector of track-segments for cutting and assigning nets.
@dataclass
class LayerPeriod:
    index: int
    signals: List[Track]
    rails: List[Track]

    # Shift the period by `dist` in its periodic direction
    def offset(self, dist: DbUnits) -> None:
        for t in self.rails:
            t.data.start += dist

        for t in self.signals:
            t.data.start += dist

    # Set the stop position for all [Track]s to `stop`
    def stop(self, stop: DbUnits) -> None:
        for t in self.rails:
            t.stop(stop)

        for t in self.signals:
            t.stop(stop)

    # Cut all [Track]s from `start` to `stop`,
    def cut(
        self,
        start: DbUnits,
        stop: DbUnits,
        src: TrackCross,
    ) -> None:
        for t in self.rails:
            t.cut(start, stop, src)

        for t in self.signals:
            t.cut(start, stop, src)

    # Block all [Track]s from `start` to `stop`,
    def block(self, start: DbUnits, stop: DbUnits, src: "Instance") -> None:
        for t in self.rails:
            t.block(start, stop, src)

        for t in self.signals:
            t.block(start, stop, src)
