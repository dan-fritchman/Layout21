//! # Validators
//! Integrity checks for [Stack]s, [Layer]s, and the like.
//!
use super::*;

/// Helper-function for asserting all sorts of boolean conditions, returning [LayoutResult] and enabling the question-mark operator.
pub(crate) fn assert(b: bool) -> LayoutResult<()> {
    match b {
        true => Ok(()),
        false => Err(LayoutError::Validation),
    }
}
/// Derived data for a [Stack], after it has gone through some validation steps.
#[derive(Debug)]
pub(crate) struct ValidStack {
    /// Measurement units
    pub units: Unit,
    /// Layer used for cell outlines/ boundaries
    pub boundary_layer: Option<raw::Layer>,

    /// Primitive layer
    pub prim: PrimitiveLayer,
    /// Set of via layers
    pub vias: Vec<ViaLayer>,
    /// Metal Layers
    pub(crate) metals: Vec<ValidMetalLayer>,
    /// Pitches per metal layer, one each for those in `stack`
    pub(crate) pitches: Vec<DbUnits>,
}
impl ValidStack {
    /// Retrieve the number of primitive-sized pitches in a period of layer number `layer`
    pub(crate) fn primitive_pitches(&self, layer: usize) -> PrimPitches {
        let layer = &self.metals[layer];
        let dir = layer.spec.dir.other();
        let prim_pitch = self.prim.pitches[dir];
        if layer.pitch % prim_pitch != 0 {
            panic!("INVALID LAYER PITCH!!!");
        }
        return PrimPitches {
            dir,
            num: layer.pitch / prim_pitch,
        };
    }
}
#[derive(Debug)]
pub(crate) struct StackValidator;
impl StackValidator {
    pub(crate) fn validate(stack: Stack) -> LayoutResult<ValidStack> {
        let Stack {
            units,
            boundary_layer,
            vias,
            layers,
            prim,
            ..
        } = stack;
        // Validate the primitive layer
        assert(prim.pitches.x.raw() > 0)?;
        assert(prim.pitches.y.raw() > 0)?;

        // Validate each metal layer
        let metals = layers
            .into_iter()
            .enumerate()
            .map(|(num, layer)| ValidMetalLayer::validate(layer, num, &prim))
            .collect::<Result<Vec<_>, _>>()?;
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
        // FIXME: checks on [ViaLayer]s
        // Stack checks out! Return its derived data
        Ok(ValidStack {
            units,
            boundary_layer,
            vias,
            pitches,
            metals,
            prim,
        })
    }
}
#[derive(Debug)]
pub(crate) struct ValidMetalLayer {
    /// Original Layer Spec
    pub(crate) spec: Layer,

    // Derived data
    /// Index in layers array
    pub(crate) index: usize,
    /// Derived single-period template
    pub(crate) period: LayerPeriod,
    /// Pitch in db-units
    pub(crate) pitch: DbUnits,
}
impl ValidMetalLayer {
    /// Perform validation on a [Layer], return a corresponding [ValidMetalLayer]
    pub(crate) fn validate(
        layer: Layer,
        index: usize,
        prim: &PrimitiveLayer,
    ) -> LayoutResult<ValidMetalLayer> {
        // Check for non-zero widths of all entries
        for entry in layer.entries().iter() {
            assert(entry.width.raw() > 0)?;
        }
        let pitch = layer.pitch();
        assert(pitch.raw() > 0)?;
        // Check for fit on the primitive grid, if the layer is in primitives
        match layer.prim {
            PrimitiveMode::Partial | PrimitiveMode::Owned => {
                let prim_pitch = prim.pitches[!layer.dir];
                assert(pitch % prim_pitch == 0)?;
            }
            PrimitiveMode::None => (),
        }
        // Convert to a prototype [LayerPeriod]
        // This is frequently used for calculating track locations
        let period = layer.to_layer_period(0, 0)?;
        Ok(ValidMetalLayer {
            spec: layer,
            index,
            period,
            pitch,
        })
    }
    /// Get the center-coordinate of signal-track `idx`, in our periodic dimension
    pub fn center(&self, idx: usize) -> LayoutResult<DbUnits> {
        let len = self.period.signals.len();
        let track = &self.period.signals[idx % len];
        let mut cursor = self.pitch * (idx / len);
        cursor += track.start + track.width / 2;
        Ok(cursor)
    }
}
/// Location on a [Track], including the db-unit cross-dimension
#[derive(Debug, Clone)]
pub(crate) struct ValidTrackLoc {
    pub layer: usize,
    pub track: usize,
    pub dist: DbUnits,
}
impl ValidTrackLoc {
    /// Validate a [TrackIntersection], and convert the cross-dimension into db-units
    pub(crate) fn validate(i: &TrackIntersection, stack: &ValidStack) -> LayoutResult<Self> {
        // Check that we won't reach outside the stack, and grab the secondary layer
        assert(i.layer < stack.metals.len())?;
        let other = if i.relz == RelZ::Below {
            assert(i.layer >= 1)?;
            i.layer - 1
        } else {
            assert(i.layer < stack.metals.len() - 1)?;
            i.layer + 1
        };
        // And find the center of the `other` track
        Ok(ValidTrackLoc {
            layer: i.layer,
            track: i.track,
            dist: stack.metals[other].center(i.at)?,
        })
    }
}
/// Intersection between two validated [ValidTrackLoc],
/// including the invariant that `top` is one layer above `bot`.
#[derive(Debug, Clone)]
pub(crate) struct ValidAssign {
    pub net: String,
    pub top: ValidTrackLoc,
    pub bot: ValidTrackLoc,
}
impl ValidAssign {
    /// Validate a [TrackIntersection], and convert into top/bottom coordinates
    pub(crate) fn validate(assn: &Assign, stack: &ValidStack) -> LayoutResult<Self> {
        // Name "validation": just empty-string checking, at least for now
        assert(assn.net.len() > 0)?;
        let net = assn.net.clone();
        // Validate the location and its transpose
        let loc1 = ValidTrackLoc::validate(&assn.at, stack)?;
        let loc2 = ValidTrackLoc::validate(&assn.at.transpose(), stack)?;
        // Finally arrange the two by top/bottom
        let (top, bot) = if assn.at.relz == RelZ::Below {
            (loc1, loc2)
        } else {
            (loc2, loc1)
        };
        Ok(Self { net, top, bot })
    }
}
