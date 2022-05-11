// Std-lib imports
use std::fmt::Debug;

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::coords::{DbUnits, Xy};
use crate::instance::Instance;
use crate::raw::{self, Dir, LayoutResult, Units};
use crate::utils::Ptr;
use crate::{tracks::*, validate};



/// Transformed single period of [Track]s on a [Layer]
/// Splits track-info between signals and rails.
/// Stores each as a [Track] struct, which moves to a (start, width) size-format,
/// and includes a vector of track-segments for cutting and assigning nets.
#[derive(Debug, Clone, Default)]
pub struct LayerPeriod<'lib> {
    pub index: usize,
    pub signals: Vec<Track<'lib>>,
    pub rails: Vec<Track<'lib>>,
}
impl<'lib> LayerPeriod<'lib> {
    /// Shift the period by `dist` in its periodic direction
    pub fn offset(&mut self, dist: DbUnits) -> LayoutResult<()> {
        for t in self.rails.iter_mut() {
            t.data.start += dist;
        }
        for t in self.signals.iter_mut() {
            t.data.start += dist;
        }
        Ok(())
    }
    /// Set the stop position for all [Track]s to `stop`
    pub fn stop(&mut self, stop: DbUnits) -> LayoutResult<()> {
        for t in self.rails.iter_mut() {
            t.stop(stop)?;
        }
        for t in self.signals.iter_mut() {
            t.stop(stop)?;
        }
        Ok(())
    }
    /// Cut all [Track]s from `start` to `stop`,
    pub fn cut(
        &mut self,
        start: DbUnits,
        stop: DbUnits,
        src: &'lib TrackCross,
    ) -> TrackResult<()> {
        for t in self.rails.iter_mut() {
            t.cut(start, stop, src)?;
        }
        for t in self.signals.iter_mut() {
            t.cut(start, stop, src)?;
        }
        Ok(())
    }
    /// Block all [Track]s from `start` to `stop`,
    pub fn block(&mut self, start: DbUnits, stop: DbUnits, src: &Ptr<Instance>) -> TrackResult<()> {
        for t in self.rails.iter_mut() {
            t.block(start, stop, src)?;
        }
        for t in self.signals.iter_mut() {
            t.block(start, stop, src)?;
        }
        Ok(())
    }
}


#[derive(Debug, Clone, Default)]
pub struct LayerPeriodData {
    pub signals: Vec<TrackData>,
    pub rails: Vec<TrackData>,
}
impl MetalLayer {
    /// Convert this [Layer]'s track-info into a [LayerPeriodData]
    pub(crate) fn to_layer_period_data(&self) -> LayoutResult<LayerPeriodData> {
        let mut period = LayerPeriodData::default();
        let mut cursor = self.offset;
        for e in &self.entries() {
            let d = e.width;
            match e.ttype {
                TrackType::Gap => (),
                TrackType::Rail(_railkind) => {
                    period.rails.push(TrackData {
                        ttype: e.ttype,
                        index: period.rails.len(),
                        dir: self.dir,
                        start: cursor,
                        width: d,
                    });
                }
                TrackType::Signal => {
                    period.signals.push(TrackData {
                        ttype: e.ttype,
                        index: period.signals.len(),
                        dir: self.dir,
                        start: cursor,
                        width: d,
                    });
                }
            };
            cursor += d;
        }
        Ok(period)
    }
    /// Convert this [Layer]'s track-info into a [LayerPeriod]
    pub(crate) fn to_layer_period<'me, 'lib>(
        &'me self,
        index: usize,
        stop: impl Into<DbUnits>,
    ) -> LayoutResult<LayerPeriod<'lib>> {
        let stop = stop.into();
        let mut period = LayerPeriod::default();
        period.index = index;
        let mut cursor = self.offset + (self.pitch() * index);
        let entries = self.entries();
        let iterator: Box<dyn Iterator<Item = _>> =
            if self.flip == FlipMode::EveryOther && index % 2 == 1 {
                Box::new(entries.iter().rev())
            } else {
                Box::new(entries.iter())
            };
        for e in iterator {
            let d = e.width;
            match e.ttype {
                TrackType::Gap => (),
                TrackType::Rail(railkind) => {
                    period.rails.push(
                        Track {
                            data: TrackData {
                                ttype: e.ttype,
                                index: period.rails.len(),
                                dir: self.dir,
                                start: cursor,
                                width: d,
                            },
                            segments: vec![TrackSegment {
                                tp: TrackSegmentType::Rail(railkind),
                                start: 0.into(),
                                stop,
                            }],
                        }
                        .validate()?,
                    );
                }
                TrackType::Signal => {
                    period.signals.push(
                        Track {
                            data: TrackData {
                                ttype: e.ttype,
                                index: period.signals.len(),
                                dir: self.dir,
                                start: cursor,
                                width: d,
                            },
                            segments: vec![TrackSegment {
                                tp: TrackSegmentType::Wire { src: None },
                                start: 0.into(),
                                stop,
                            }],
                        }
                        .validate()?,
                    );
                }
            };
            cursor += d;
        }
        Ok(period)
    }