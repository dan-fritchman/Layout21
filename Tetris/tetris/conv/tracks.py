
from enum import Enum, auto 
from typing import List, Union, Optional

from pydantic.dataclasses import dataclass

# Local imports
from .coords import DbUnits, Dir 
from .error import LayoutError
from .stack import Assign
from .relz import RelZ
# from .instance import Instance


@dataclass
class TrackSegmentType:
    ... # FIXME!
# enum TrackSegmentType<'lib> :
#     Cut : src: 'lib TrackCross ,
#     Blockage : src: Ptr<Instance> ,
#     Wire : src: Optional<'lib Assign> ,
#     Rail(RailKind),

# # Segments of un-split, single-net wire on a [Track]
@dataclass
class TrackSegment:
    # Segment-Type
    tp: TrackSegmentType
    # Start Location, in [Stack]'s `units`
    start: DbUnits
    # End/Stop Location, in [Stack]'s `units`
    stop: DbUnits


TrackConflict = Union[Assign, TrackCross, Instance]


        
    

enum TrackError :
    OutOfBounds(DbUnits),
    Overlap(DbUnits, DbUnits),
    Conflict(TrackConflict, TrackConflict),
    CutConflict(TrackConflict, TrackCross),
    BlockageConflict(TrackConflict, Ptr<Instance>),

type TrackResult<T> = Result<T, TrackError>

    # Display a [TrackError]
    def fmt(self, f: std.fmt.Formatter) -> std.fmt.Result :
        match self :
            TrackError.OutOfBounds(stop) => write!(f, "Track Out of Bounds: ::", stop),
            TrackError.Overlap(p0, p1) => :
                write!(f, "Overlapping Track cuts at: ::, ::", p0, p1)
            
            TrackError.CutConflict(t0, t1) => :
                write!(f, "Conflicting Track-Cuts at: ::, ::", t0, t1)
            
            TrackError.BlockageConflict(t0, t1) => :
                write!(
                    f,
                    "Conflicting Instance Blockages: \n * :\n * ::\n",
                    t0, t1
                )
            
            TrackError.Conflict(t0, t1) => :
                write!(f, "Conflict Between: \n * :\n * ::\n", t0, t1)
            
        
    


    # Display a [TrackError]
    def fmt(self, f: std.fmt.Formatter) -> std.fmt.Result :
        std.fmt.Debug.fmt(self, f)
    



    def into(self) -> LayoutError :
        LayoutError.Boxed(Box(self))
    



@dataclass
class TrackData :
    # Track Type (Rail, Signal)
    ttype: TrackType
    # Track Index
    index: int
    # Direction
    dir: Dir
    # Starting-point in off-dir axis
    start: DbUnits
    # Track width
    width: DbUnits

# # Track
#
# An "instantiated" track, including:
# * Track-long data in a [TrackData], and
# * A set of [TrackSegment]s
@dataclass
class Track:
    # Track-long data
    data: TrackData
    # Set of wire-segments in positional order
    segments: List[TrackSegment]

    # Verify a (generally just-created) [Track] is valid
    def validate(self) -> "Track" :
        if self.data.width < DbUnits(0) :
            raise LayoutError("Negative Track Width")
        return self

    # Set the net of the track-segment at `at` to `net`
    def set_net(self, at: DbUnits, assn: Assign) -> None :
        # First find the segment to be modified
        seg = None
        for s in self.segments.iter_mut() :
            if s.start > at :
                break
            
            if s.start <= at  s.stop >= at :
                seg = Some(s)
                break
            
        
        match seg :
            None => Err(TrackError.OutOfBounds(at)),
            Some(seg) => match seg.tp :
                TrackSegmentType.Rail(_) => unreachable!(),
                TrackSegmentType.Cut : ..  => Err(TrackError.Conflict(
                    # Error: trying to assign a net onto a Cut.
                    TrackConflict.Assign(assn.clone()),
                    TrackConflict.from(seg.tp.clone()),
                )),
                TrackSegmentType.Blockage : ..  => :
                    # FIXME: sort out the desired behaviour here.
                    # Vias above ZTop instance-pins generally land in this case.
                    # We could check for their locations Or just it go.
                    Ok(())
                
                TrackSegmentType.Wire : ref src, ..  => :
                    # The good case - assignment succeeds.
                    src.replace(assn)
                    Ok(())

    # Insert a cut or blockage corresponding to `blockage`.
    def cut_or_block(
        self,
        start: DbUnits,
        stop: DbUnits,
        tp: TrackSegmentType,
    ) -> None:
        # First bounds-check against the end of our segments, which are the end of the cell
        if stop > self.segments.last().unwrap().stop :
            return Err(TrackError.OutOfBounds(stop))

        # Find the segment where the blockage starts
        segidx = self
            .segments
            .iter_mut()
            .position(|seg| seg.stop > start)
            .ok_or(TrackError.OutOfBounds(start))
            .clone()
        seg = self.segments[segidx]
        # Check for conflicts, and get a copy of our segment-type as we will likely insert a similar segment
        tpcopy = match seg.tp :
            TrackSegmentType.Blockage : ref src  => :
                return Err(TrackError.BlockageConflict(
                    TrackConflict.from(tp),
                    src.clone(),
                ))
            
            TrackSegmentType.Cut : src  => :
                return Err(TrackError.CutConflict(
                    TrackConflict.from(tp),
                    src.clone(),
                ))
            
            TrackSegmentType.Wire : ..  => seg.tp.clone(),
            TrackSegmentType.Rail(_) => seg.tp.clone(),
        
        # Make sure the cut only effects one segment, or fail
        if seg.stop < stop :
            # FIXME this should really be the *next* segment, borrow checking fight
            return Err(TrackError.Overlap(seg.stop, stop))
        

        # All clear time to cut it.
        # In the more-common case in which the cut-end and segment-end *do not* coincide, create and insert a new segment.
        to_be_inserted: List<(int, TrackSegment)> = List()
        to_be_inserted.push((segidx + 1, TrackSegment : start, stop, tp ))
        if seg.stop != stop :
            newseg = TrackSegment :
                tp: tpcopy,
                start: stop,
                stop: seg.stop,
            
            to_be_inserted.push((segidx + 2, newseg))
        
        # Update the existing segment (and importantly, drop its mutable borrow)
        seg.stop = start
        for (idx, seg) in to_be_inserted :
            self.segments.insert(idx, seg)
        
        Ok(())
    
    # Insert a blockage from `start` to `stop`.
    # Fails if the region is not a contiguous wire segment.
    # def block(self, start: DbUnits, stop: DbUnits, src: Instance) -> None:
    #     return self.cut_or_block(start, stop, TrackSegmentType.Blockage (src))
    # 
    # Cut from `start` to `stop`.
    # Fails if the region is not a contiguous wire segment.
    def cut(
        self,
        start: DbUnits,
        stop: DbUnits,
        src: TrackCross,
    ) -> None :
        self.cut_or_block(start, stop, TrackSegmentType.Cut  (src))
    
    # Set the stop position for our last [TrackSegment] to `stop`
    def stop(self, stop: DbUnits) -> None:
        if self.segments.len() == 0 :
            raise LayoutError("Error Stopping Track")
        
        idx = self.segments.len() - 1
        self.segments[idx].stop = stop
