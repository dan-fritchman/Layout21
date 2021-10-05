//! # Validators
//! Integrity checks for [Stack]s, [Layer]s, and the like.
//!

// Std-Lib Imports
use std::convert::TryFrom;

// Local imports
use crate::coords::{DbUnits, Xy};
use crate::raw::{self, Dir, LayoutError, LayoutResult, Units};
use crate::stack::{Assign, Layer, LayerPeriodData, PrimitiveLayer, RelZ, Stack};
use crate::stack::{PrimitiveMode, ViaLayer, ViaTarget};
use crate::tracks::TrackIntersection;
use crate::utils::Ptr;

/// Helper-function for asserting all sorts of boolean conditions, returning [LayoutResult] and enabling the question-mark operator.
pub fn assert(b: bool) -> LayoutResult<()> {
    match b {
        true => Ok(()),
        false => Err(LayoutError::Validation),
    }
}

#[derive(Debug)]
pub struct StackValidator;
impl StackValidator {
    pub fn validate(stack: Stack) -> LayoutResult<ValidStack> {
        let Stack {
            units,
            boundary_layer,
            vias,
            layers,
            prim,
            rawlayers,
            ..
        } = stack;
        // Validate the primitive layer
        assert(prim.pitches.x.raw() > 0)?;
        assert(prim.pitches.y.raw() > 0)?;

        // Validate each metal layer
        let mut metals = Vec::new();
        for (num, layer) in layers.into_iter().enumerate() {
            metals.push(ValidMetalLayer::validate(layer, num, &prim)?);
        }
        // Calculate pitches as the *least-common multiple* of same-direction layers below each layer
        let mut pitches = vec![DbUnits(0); metals.len()];
        for (num, metal) in metals.iter().enumerate() {
            let mut pitch = prim.pitches[!metal.spec.dir];
            for nn in 0..num + 1 {
                if metals[nn].spec.dir == metal.spec.dir {
                    pitch = num_integer::lcm(pitch.raw(), metals[nn].pitch.raw()).into();
                }
            }
            pitches[num] = pitch;
        }
        // FIXME: add checks on [ViaLayer]s
        // Stack checks out! Return its derived data
        Ok(ValidStack {
            units,
            vias,
            pitches,
            metals,
            prim,
            rawlayers,
            boundary_layer,
        })
    }
}
/// Derived data for a [Stack], after it has gone through some validation steps.
#[derive(Debug)]
pub struct ValidStack {
    /// Measurement units
    pub units: Units,
    /// Primitive layer
    pub prim: PrimitiveLayer,
    /// Set of via layers
    pub vias: Vec<ViaLayer>,
    /// Metal Layers
    metals: Vec<ValidMetalLayer>,
    /// Pitches per metal layer, one each for those in `stack`
    pub pitches: Vec<DbUnits>,

    /// [raw::Layer] Mappings
    pub rawlayers: Option<Ptr<raw::Layers>>,
    /// Layer used for cell outlines/ boundaries
    pub boundary_layer: Option<raw::LayerKey>,
}
impl ValidStack {
    /// Get Metal-Layer number `idx`. Returns `None` if `idx` is out of bounds.
    pub fn metal(&self, idx: usize) -> LayoutResult<&ValidMetalLayer> {
        if idx >= self.metals.len() {
            Err(LayoutError::Validation)
        } else {
            Ok(&self.metals[idx])
        }
    }
    /// Get the via-layer whose bottom "target" is metal-layer `idx`.
    pub fn via_from(&self, idx: usize) -> LayoutResult<&ViaLayer> {
        for via_layer in self.vias.iter() {
            if let ViaTarget::Metal(k) = via_layer.bot {
                if k == idx {
                    return Ok(via_layer);
                }
            }
        }
        Err(LayoutError::Validation)
    }
    /// Get Via-Layer number `idx`. Returns an error if `idx` is out of bounds.
    pub fn via(&self, idx: usize) -> LayoutResult<&ViaLayer> {
        if idx >= self.vias.len() {
            Err(LayoutError::Validation)
        } else {
            Ok(&self.vias[idx])
        }
    }
}
#[derive(Debug)]
pub struct ValidMetalLayer {
    /// Original Layer Spec
    pub spec: Layer,

    // Derived data
    /// Index in layers array
    pub index: usize,
    /// Derived single-period template
    pub period_data: LayerPeriodData,
    /// Pitch in db-units
    pub pitch: DbUnits,
    /// Raw layer-key
    pub raw: Option<raw::LayerKey>,
}
impl ValidMetalLayer {
    /// Perform validation on a [Layer], return a corresponding [ValidMetalLayer]
    pub fn validate<'prim>(
        layer: Layer,
        index: usize,
        prim: &'prim PrimitiveLayer,
    ) -> LayoutResult<ValidMetalLayer> {
        // Check for non-zero widths of all entries
        for entry in layer.entries().iter() {
            assert(entry.width.raw() > 0)?;
        }
        let pitch = layer.pitch();
        assert(pitch.raw() > 0)?;
        // Check for fit on the primitive grid, if the layer is in primitives
        match layer.prim {
            PrimitiveMode::Split | PrimitiveMode::Prim => {
                let prim_pitch = prim.pitches[!layer.dir];
                assert(pitch % prim_pitch == 0)?;
            }
            PrimitiveMode::Stack => (),
        }
        // Convert to a prototype [LayerPeriod]
        // This is frequently used for calculating track locations
        let period_data = layer.to_layer_period_data()?;
        Ok(ValidMetalLayer {
            raw: layer.raw.clone(),
            spec: layer,
            index,
            period_data,
            pitch,
        })
    }
    /// Get the track-index at [DbUnits] `dist`
    pub fn track_index(&self, dist: DbUnits) -> LayoutResult<usize> {
        // FIXME: this, particularly the `position` call, grabs the first track that ends *after* `dist`.
        // It could end up more helpful to do "closest" if `dist` is in-between two,
        // or have some alignment options.
        let npitches = dist / self.pitch;
        let remainder = DbUnits(dist % self.pitch);
        let mut index = usize::try_from(npitches)? * self.period_data.signals.len();

        index += self
            .period_data
            .signals
            .iter()
            .position(|sig| sig.start + sig.width > remainder)
            .unwrap();
        Ok(index)
    }
    /// Get the center-coordinate of signal-track `idx`, in our periodic dimension
    pub fn center(&self, idx: usize) -> LayoutResult<DbUnits> {
        // FIXME: incorrect for asymmetric tracks via `FlipMode` turned on!
        let len = self.period_data.signals.len();
        let track = &self.period_data.signals[idx % len];
        let mut cursor = self.pitch * (idx / len);
        cursor += track.start + track.width / 2;
        Ok(cursor)
    }
    /// Get the spanning-coordinates of signal-track `idx`, in our periodic dimension
    pub fn span(&self, idx: usize) -> LayoutResult<(DbUnits, DbUnits)> {
        let len = self.period_data.signals.len();
        let track = &self.period_data.signals[idx % len];
        let cursor = self.pitch * (idx / len) + track.start;
        Ok((cursor, cursor + track.width))
    }
}

#[derive(Debug, Clone)]
pub struct ValidLayerAndTrack {
    pub layer: usize,
    pub track: usize,
}
impl ValidLayerAndTrack {
    pub fn validate(i: &TrackIntersection, stack: &ValidStack) -> LayoutResult<ValidLayerAndTrack> {
        let track = i.track;
        let layer = i.layer;
        // Check that we won't reach outside the stack, and grab the secondary layer
        assert(i.layer < stack.metals.len())?;
        Ok(ValidLayerAndTrack { track, layer })
    }
}
/// Location on a [Track], including the db-unit cross-dimension
#[derive(Debug, Clone)]
pub struct ValidCut<'lib> {
    pub layer: usize,
    pub track: usize,
    pub xy: Xy<DbUnits>,
    pub src: &'lib TrackIntersection,
}
impl<'lib> ValidCut<'lib> {
    /// Validate a [TrackIntersection], and convert the cross-dimension into db-units
    pub fn validate(i: &'lib TrackIntersection, stack: &'lib ValidStack) -> LayoutResult<Self> {
        let ValidLayerAndTrack { layer, track } = ValidLayerAndTrack::validate(i, stack)?;
        let other = if i.relz == RelZ::Below {
            assert(i.layer >= 1)?;
            i.layer - 1
        } else {
            assert(i.layer < stack.metals.len() - 1)?;
            i.layer + 1
        };

        // Find the center of our track, initially assuming it runs vertically
        let x = stack.metals[i.layer].center(i.track)?;
        // And find the center of the `other` track
        let y = stack.metals[other].center(i.at)?;
        let mut xy = Xy::new(x, y);
        if stack.metals[i.layer].spec.dir == Dir::Horiz {
            xy = xy.transpose();
        }
        Ok(ValidCut {
            layer,
            track,
            xy,
            src: i,
        })
    }
}

/// Location on a [Track], including the db-unit cross-dimension
#[derive(Debug, Clone)]
pub struct ValidAssignLoc<'lib> {
    pub top: ValidLayerAndTrack,
    pub bot: ValidLayerAndTrack,
    pub xy: Xy<DbUnits>,
    pub src: &'lib TrackIntersection,
}
impl<'lib> ValidAssignLoc<'lib> {
    /// Validate a [TrackIntersection], and convert the cross-dimension into db-units
    pub fn validate(i: &'lib TrackIntersection, stack: &'lib ValidStack) -> LayoutResult<Self> {
        // Validate the location and its transpose
        let loc1 = ValidLayerAndTrack::validate(i, stack)?;
        let loc2 = ValidLayerAndTrack::validate(&i.transpose(), stack)?;
        // Finally arrange the two by top/bottom
        let (top, bot) = if i.relz == RelZ::Below {
            (loc1, loc2)
        } else {
            (loc2, loc1)
        };
        let other = if i.relz == RelZ::Below {
            assert(i.layer >= 1)?;
            i.layer - 1
        } else {
            assert(i.layer < stack.metals.len() - 1)?;
            i.layer + 1
        };

        // Find the center of our track, initially assuming it runs vertically
        let x = stack.metals[i.layer].center(i.track)?;
        // And find the center of the `other` track
        let y = stack.metals[other].center(i.at)?;
        let mut xy = Xy::new(x, y);
        if stack.metals[i.layer].spec.dir == Dir::Horiz {
            xy = xy.transpose();
        }
        Ok(ValidAssignLoc {
            top,
            bot,
            xy,
            src: i,
        })
    }
}

/// Intersection between two validated [ValidAssignLoc],
/// including the invariant that `top` is one layer above `bot`.
#[derive(Debug, Clone)]
pub struct ValidAssign<'lib> {
    pub net: String,
    pub loc: ValidAssignLoc<'lib>,
    pub src: &'lib Assign,
}
impl<'lib> ValidAssign<'lib> {
    /// Validate a [TrackIntersection], and convert into top/bottom coordinates
    pub fn validate(assn: &'lib Assign, stack: &'lib ValidStack) -> LayoutResult<Self> {
        // Name "validation": just empty-string checking, at least for now
        assert(assn.net.len() > 0)?;
        let net = assn.net.clone();
        let loc = ValidAssignLoc::validate(&assn.at, stack)?;
        Ok(Self {
            net,
            loc,
            src: assn,
        })
    }
}
