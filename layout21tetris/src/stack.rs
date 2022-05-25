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

/// # Stack
///
/// The z-stack, primarily including metal, via, and primitive layers
#[derive(Debug, Clone)]
pub struct Stack {
    /// Measurement units
    pub units: Units,
    /// Primitive Layer
    pub prim: PrimitiveLayer,
    /// Set of metal layers
    pub metals: Vec<MetalLayer>,
    /// Set of via layers
    pub vias: Vec<ViaLayer>,
    /// [raw::Layer] Mappings
    pub rawlayers: Option<Ptr<raw::Layers>>,
    /// Layer used for cell outlines/ boundaries
    pub boundary_layer: Option<raw::LayerKey>,
}
impl Stack {
    /// Run validation, consuming `self` and creating a [validate::ValidStack]
    pub fn validate(self) -> LayoutResult<validate::ValidStack> {
        validate::validate_stack(self)
    }
}
/// # MetalLayer
///
/// Metal layer in a [Stack]
/// Each layer is effectively infinite-spanning in one dimension, and periodic in the other.
/// Layers with `dir=Dir::Horiz` extend to infinity in x, and repeat in y, and vice-versa.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetalLayer {
    /// Layer Name
    pub name: String,
    /// Direction Enumeration (Horizontal/ Vertical)
    pub dir: Dir,
    /// Default size of wire-cuts
    pub cutsize: DbUnits,
    /// Track Size & Type Entries
    pub entries: Vec<TrackSpec>,
    /// Offset, in our periodic dimension
    pub offset: DbUnits,
    /// Overlap between periods
    pub overlap: DbUnits,
    /// Setting for period-by-period flipping
    pub flip: FlipMode,
    /// Primitive-layer relationship
    pub prim: PrimitiveMode,
    /// [raw::Layer] for exports
    pub raw: Option<raw::LayerKey>,
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
    /// Flatten our [Entry]s into a vector
    /// Removes any nested patterns
    pub(crate) fn entries(&self) -> Vec<TrackEntry> {
        let mut v: Vec<TrackEntry> = Vec::new();
        for e in self.entries.iter() {
            match e {
                TrackSpec::Entry(ee) => v.push(ee.clone()),
                // FIXME: why doesn't this recursively call `entries`? Seems it could/should.
                TrackSpec::Repeat(p) => {
                    for _i in 0..p.nrep {
                        for ee in p.entries.iter() {
                            v.push(ee.clone());
                        }
                    }
                }
            }
        }
        v
    }
    /// Sum up this [Layer]'s pitch
    pub(crate) fn pitch(&self) -> DbUnits {
        self.entries().iter().map(|e| e.width).sum::<DbUnits>() - self.overlap
    }
}

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
    pub fn cut(&mut self, start: DbUnits, stop: DbUnits, src: &'lib TrackCross) -> TrackResult<()> {
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
/// # Via / Insulator Layer Between Metals
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ViaLayer {
    /// Layer name
    pub name: String,
    /// Top of the two layers connected by this layer
    pub top: ViaTarget,
    /// Bottom of the two layers connected by this layer
    pub bot: ViaTarget,
    /// Via size
    pub size: Xy<DbUnits>,
    /// Stream-out layer numbers
    pub raw: Option<raw::LayerKey>,
}
/// # Via Targets
///
/// Enumerates the things vias can "go between".
/// Generally either a numbered metal layer, or the primitive base-layers.
///
/// Values stored in the `Metal` variant are treated as indicies into `Stack.metals`,
/// i.e. `Metal(0)` is the first metal layer defined in the stack.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ViaTarget {
    /// Connect to the Primitive layer
    Primitive,
    /// Connect to an indexed metal layer
    Metal(usize),
}
impl From<usize> for ViaTarget {
    fn from(i: usize) -> Self {
        Self::Metal(i)
    }
}
impl From<Option<usize>> for ViaTarget {
    fn from(i: Option<usize>) -> Self {
        match i {
            None => Self::Primitive,
            Some(i) => Self::Metal(i),
        }
    }
}
/// Assignment of a net onto a track-intersection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assign {
    /// Net Name
    pub net: String,
    /// Track Intersection Location
    pub at: TrackCross,
}
impl Assign {
    /// Create a new [Assign]
    pub fn new(net: impl Into<String>, at: impl Into<TrackCross>) -> Self {
        Self {
            net: net.into(),
            at: at.into(),
        }
    }
}
/// Relative Z-Axis Reference to one Layer `Above` or `Below` another
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RelZ {
    Above,
    Below,
}
impl RelZ {
    pub fn other(&self) -> Self {
        match *self {
            RelZ::Above => RelZ::Below,
            RelZ::Below => RelZ::Above,
        }
    }
}

/// Indication of whether a layer flips in its periodic axis with every period,
/// as most standard-cell-style logic gates do.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum FlipMode {
    EveryOther,
    None,
}
/// Indication of whether a layer is owned by, partially included in, or external to the primitive blocks
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PrimitiveMode {
    /// Owned by Primitives
    Prim,
    /// Partially split between Primitives and Stack
    Split,
    /// Owned by the Stack
    Stack,
}
/// Description of the primitive-level cells in a [Stack]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PrimitiveLayer {
    pub pitches: Xy<DbUnits>,
}
impl PrimitiveLayer {
    /// Create a new [PrimitiveLayer] with the given pitches
    pub fn new(pitches: Xy<DbUnits>) -> Self {
        Self { pitches }
    }
}
