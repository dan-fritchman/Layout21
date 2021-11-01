//!
//! # Tetris Validators
//!
//! Integrity checks for [Stack]s, [Library]s, and the like.
//!

// Std-Lib Imports
use std::convert::TryFrom;

// Local imports
use crate::{
    abs::Abstract,
    cell::Cell,
    coords::{DbUnits, HasUnits},
    instance::Instance,
    layout::Layout,
    library::Library,
    raw::{self, LayoutError, LayoutResult, Units},
    stack::{Assign, LayerPeriodData, MetalLayer, PrimitiveLayer, Stack},
    stack::{PrimitiveMode, ViaLayer, ViaTarget},
    tracks::{TrackCross, TrackRef},
    utils::{ErrorHelper, Ptr},
};

/// Helper-function for asserting all sorts of boolean conditions, returning [LayoutResult] and enabling the question-mark operator.
pub fn assert(b: bool) -> LayoutResult<()> {
    match b {
        true => Ok(()),
        false => LayoutError::fail("Assertion Failed"),
    }
}
#[derive(Debug)]
pub struct StackValidator;
impl ErrorHelper for StackValidator {
    type Error = LayoutError;
    /// Errors are string-valued [LayoutError::String]s.
    fn err(&self, msg: impl Into<String>) -> Self::Error {
        LayoutError::msg(msg)
    }
}
/// Validate a [Stack], returning a [ValidStack] in its place.
pub fn validate_stack(stack: Stack) -> LayoutResult<ValidStack> {
    // Create a [StackValidator] instance, and use its internal instance method.
    StackValidator.validate_stack(stack)
}
impl StackValidator {
    /// Internal implementation of [validate_stack].
    fn validate_stack(&mut self, stack: Stack) -> LayoutResult<ValidStack> {
        let Stack {
            units,
            boundary_layer,
            vias,
            metals,
            prim,
            rawlayers,
            ..
        } = stack;
        // Validate the primitive layer
        self.assert(
            prim.pitches.x.raw() > 0,
            "Invalid zero or negative Primitive pitch",
        )?;
        self.assert(
            prim.pitches.y.raw() > 0,
            "Invalid zero or negative Primitive pitch",
        )?;

        // Validate each metal layer
        let mut valid_metals = Vec::new();
        for (num, layer) in metals.into_iter().enumerate() {
            valid_metals.push(self.validate_metal(layer, num, &prim)?);
        }
        // Calculate pitches as the *least-common multiple* of same-direction layers below each layer
        let mut pitches = vec![DbUnits(0); valid_metals.len()];
        for (num, metal) in valid_metals.iter().enumerate() {
            let mut pitch = prim.pitches[!metal.spec.dir];
            for nn in 0..num + 1 {
                if valid_metals[nn].spec.dir == metal.spec.dir {
                    pitch = num_integer::lcm(pitch.raw(), valid_metals[nn].pitch.raw()).into();
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
            metals: valid_metals,
            prim,
            rawlayers,
            boundary_layer,
        })
    }
    /// Perform validation on a [Layer], return a corresponding [ValidMetalLayer]
    pub fn validate_metal<'prim>(
        &mut self,
        layer: MetalLayer,
        index: usize,
        prim: &'prim PrimitiveLayer,
    ) -> LayoutResult<ValidMetalLayer> {
        // Check for non-zero widths of all entries
        for entry in layer.entries().iter() {
            self.assert(
                entry.width.raw() > 0,
                format!(
                    "Invalid non-positive entry on {:?}: {:?}",
                    layer, entry.width
                ),
            )?;
        }
        let pitch = layer.pitch();
        self.assert(
            pitch.raw() > 0,
            format!(
                "Invalid layer with non-positive pitch={}: {:?}",
                pitch.raw(),
                layer
            ),
        )?;
        // Check for fit on the primitive grid, if the layer is in primitives
        match layer.prim {
            PrimitiveMode::Split | PrimitiveMode::Prim => {
                let prim_pitch = prim.pitches[!layer.dir];
                self.assert(pitch % prim_pitch == 0, format!("Invalid layer {:?} shared with Primitives is not an integer multiple of the primitive pitch in the {:?} direction", layer, !layer.dir))?;
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
            LayoutError::fail(format!("Invalid metal index {}", idx))
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
        LayoutError::fail(format!("Requiring undefined via from metal layer {}", idx))
    }
    /// Get Via-Layer number `idx`. Returns an error if `idx` is out of bounds.
    pub fn via(&self, idx: usize) -> LayoutResult<&ViaLayer> {
        if idx >= self.vias.len() {
            LayoutError::fail(format!("Invalid via index {}", idx))
        } else {
            Ok(&self.vias[idx])
        }
    }
}
#[derive(Debug)]
pub struct ValidMetalLayer {
    /// Original Layer Spec
    pub spec: MetalLayer,

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
/// Validate [Library] `lib`. Requires a valid `stack`.
pub fn validate_lib(lib: &Library, stack: &ValidStack) -> LayoutResult<()> {
    LibValidator::new(stack).validate_lib(lib)
}
/// # Library Validator
pub struct LibValidator<'stk> {
    pub stack: &'stk ValidStack,
}
impl<'stk> LibValidator<'stk> {
    pub(crate) fn new(stack: &'stk ValidStack) -> Self {
        Self { stack }
    }
    pub(crate) fn validate_lib(&mut self, lib: &Library) -> LayoutResult<()> {
        self.assert(lib.name.len() > 0, "Library name is empty")?;
        for cellptr in lib.cells.iter() {
            let mut cell = cellptr.write()?;
            self.validate_cell(&mut *cell)?;
        }
        // FIXME: validate raw-content
        Ok(())
    }
    pub(crate) fn validate_cell(&mut self, cell: &mut Cell) -> LayoutResult<()> {
        // FIXME: add checks on `metals`, `outline`
        self.assert(cell.name.len() > 0, "Cell name is empty")?;
        if let Some(ref mut abs) = cell.abs {
            self.assert(
                abs.name == cell.name,
                format!(
                    "Cell name mismatch between Abstract {} and Cell {}",
                    abs.name, cell.name
                ),
            )?;

            self.validate_abstract(abs)?;
        }
        if let Some(ref mut layout) = cell.layout {
            self.assert(
                layout.name == cell.name,
                format!(
                    "Cell name mismatch between Layout {} and Cell {}",
                    layout.name, cell.name
                ),
            )?;
            self.validate_layout(layout)?;
        }
        // FIXME: validate any raw and circuit content
        Ok(())
    }
    pub(crate) fn validate_abstract(&mut self, _abs: &Abstract) -> LayoutResult<()> {
        Ok(()) // FIXME!
    }
    pub(crate) fn validate_layout(&mut self, layout: &Layout) -> LayoutResult<()> {
        for instptr in layout.instances.iter() {
            let inst = instptr.read()?;
            self.validate_instance(&*inst)?;
        }
        for cut in layout.cuts.iter() {
            self.validate_track_cross(cut)?;
        }
        for assn in layout.assignments.iter() {
            self.validate_assign(assn)?;
        }
        self.assert(
            layout.places.len() == 0,
            "Internal Error: Layout being validated without first being Placed ",
        )?;
        Ok(())
    }
    pub(crate) fn validate_instance(&mut self, _inst: &Instance) -> LayoutResult<()> {
        Ok(()) // FIXME!
    }
    pub(crate) fn validate_assign(&mut self, assn: &Assign) -> LayoutResult<ValidAssign> {
        // Net "validation": just empty-string checking, at least for now
        self.assert(
            assn.net.len() > 0,
            format!("Invalid zero-length net assigned at {:?}", assn.at),
        )?;
        // Validate the track-cross location
        let i = &assn.at;
        self.validate_track_cross(i)?;
        // Arrange the two by top/bottom
        let (top, bot) = if i.track.layer == i.cross.layer + 1 {
            (i.track, i.cross)
        } else if i.track.layer == i.cross.layer - 1 {
            (i.cross, i.track)
        } else {
            return self.fail(format!("Invalid Assign on non-adjacent layers: {:?}", assn));
        };
        Ok(ValidAssign {
            top,
            bot,
            src: assn.clone(),
        })
    }
    pub(crate) fn validate_track_cross(&mut self, i: &TrackCross) -> LayoutResult<()> {
        // Validate both [TrackRef]s
        self.validate_track_ref(&i.track)?;
        self.validate_track_ref(&i.cross)?;
        // Verify that the two are in opposite directions
        if self.stack.metal(i.track.layer)?.spec.dir == self.stack.metal(i.cross.layer)?.spec.dir {
            self.fail(format!(
                "TrackCross {:?} and {:?} are in the same direction",
                i.track, i.cross
            ))?;
        }
        Ok(())
    }
    pub(crate) fn validate_track_ref(&mut self, i: &TrackRef) -> LayoutResult<()> {
        // Check that we won't reach outside the stack
        self.assert(
            i.layer < self.stack.metals.len(),
            format!("Invalid TrackRef outside Stack: {:?}", i),
        )?;
        Ok(())
    }
}
impl ErrorHelper for LibValidator<'_> {
    type Error = LayoutError;
    /// Errors are string-valued [LayoutError::String]s.
    fn err(&self, msg: impl Into<String>) -> Self::Error {
        LayoutError::msg(msg)
    }
}

/// # Validated Assignment
///
/// Track-intersection  including the invariant that `top` is one layer above `bot`,
/// such that the a via can be drawn between the two.
///
#[derive(Debug, Clone)]
pub struct ValidAssign {
    pub src: Assign,
    pub top: TrackRef,
    pub bot: TrackRef,
}
