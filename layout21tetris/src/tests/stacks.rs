//!
//! # Test Sample [Stack]s

// Local imports
use crate::raw::{self, Dir, LayoutResult, Units};
use crate::stack::*;
use crate::utils::Ptr;

/// # Sample Stacks
/// Namespace for commonly re-used [Stack]s for testing.
pub struct SampleStacks;

impl SampleStacks {
    /// As nearly empty a [Stack] as possible, while being raw-exportable.
    /// Includes:
    /// * A `boundary_layer`
    /// * [raw::Layers] containing solely that boundary layer
    /// * No metals or via layers
    /// Generally useful for placement activities, particularly among [Instace]s.
    pub fn empty() -> LayoutResult<Stack> {
        let mut rawlayers = raw::Layers::default();
        let boundary_layer = Some(rawlayers.add(raw::Layer::from_pairs(
            0,
            &[(0, raw::LayerPurpose::Outline)],
        )?));
        let stack = Stack {
            units: Units::default(),
            boundary_layer,
            prim: PrimitiveLayer::new((100, 100).into()),
            layers: Vec::new(), // No metal layers
            vias: Vec::new(),   // No vias
            rawlayers: Some(Ptr::new(rawlayers)),
        };
        Ok(stack)
    }

    /// Real(istic) PDK [Stack]
    pub fn pdka() -> LayoutResult<Stack> {
        let mut rawlayers = raw::Layers::default();
        // Shorthands for the common purpose-numbers
        let metal_purps = [
            (255, raw::LayerPurpose::Obstruction),
            (20, raw::LayerPurpose::Drawing),
            (5, raw::LayerPurpose::Label),
            (16, raw::LayerPurpose::Pin),
        ];
        let via_purps = [
            (255, raw::LayerPurpose::Obstruction),
            (44, raw::LayerPurpose::Drawing),
            (5, raw::LayerPurpose::Label),
            (16, raw::LayerPurpose::Pin),
        ];
        // Add a few base-layers that we are used in imported/ primitive cells, but not in our stack
        rawlayers.add(raw::Layer::new(64, "nwell").add_pairs(&metal_purps)?);
        rawlayers.add(raw::Layer::new(67, "li1").add_pairs(&metal_purps)?);
        // Create the test stack
        let stack = Stack {
            units: Units::Nano,
            boundary_layer: Some(rawlayers.add(raw::Layer::from_pairs(
                236,
                &[(0, raw::LayerPurpose::Outline)],
            )?)),
            prim: PrimitiveLayer {
                pitches: (460, 2720).into(),
            },
            layers: vec![
                Layer {
                    name: "met1".into(),
                    entries: vec![
                        TrackSpec::gnd(480),
                        TrackSpec::pat(vec![TrackEntry::gap(200), TrackEntry::sig(140)], 6),
                        TrackSpec::gap(200),
                        TrackSpec::pwr(480),
                    ],
                    dir: Dir::Horiz,
                    offset: (-240).into(),
                    cutsize: (250).into(),
                    overlap: (480).into(),
                    raw: Some(rawlayers.add(raw::Layer::from_pairs(68, &metal_purps)?)),
                    flip: FlipMode::EveryOther,
                    prim: PrimitiveMode::Partial,
                },
                Layer {
                    name: "met2".into(),
                    entries: vec![TrackSpec::sig(140), TrackSpec::gap(320)],
                    dir: Dir::Vert,
                    cutsize: (250).into(),
                    offset: (-70).into(),
                    overlap: (0).into(),
                    raw: Some(rawlayers.add(raw::Layer::from_pairs(69, &metal_purps)?)),
                    flip: FlipMode::None,
                    prim: PrimitiveMode::None,
                },
                Layer {
                    name: "met3".into(),
                    entries: vec![
                        TrackSpec::gnd(480),
                        TrackSpec::pat(vec![TrackEntry::gap(200), TrackEntry::sig(140)], 6),
                        TrackSpec::gap(200),
                        TrackSpec::pwr(480),
                    ],
                    dir: Dir::Horiz,
                    offset: (-240).into(),
                    cutsize: (250).into(),
                    overlap: (480).into(),
                    raw: Some(rawlayers.add(raw::Layer::from_pairs(70, &metal_purps)?)),
                    flip: FlipMode::EveryOther,
                    prim: PrimitiveMode::None,
                },
                Layer {
                    name: "met4".into(),
                    entries: vec![
                        TrackSpec::gnd(510),
                        TrackSpec::pat(vec![TrackEntry::gap(410), TrackEntry::sig(50)], 8),
                        TrackSpec::gap(410),
                        TrackSpec::pwr(510),
                    ],
                    dir: Dir::Vert,
                    cutsize: (250).into(),
                    offset: (-255).into(),
                    overlap: (510).into(),
                    raw: Some(rawlayers.add(raw::Layer::from_pairs(71, &metal_purps)?)),
                    flip: FlipMode::EveryOther,
                    prim: PrimitiveMode::None,
                },
            ],
            vias: vec![
                ViaLayer {
                    name: "mcon".into(),
                    between: (0, 1),
                    size: (240, 240).into(),
                    raw: Some(rawlayers.add(raw::Layer::from_pairs(67, &via_purps)?)),
                },
                ViaLayer {
                    name: "via1".into(),
                    between: (1, 2),
                    size: (240, 240).into(),
                    raw: Some(rawlayers.add(raw::Layer::from_pairs(68, &via_purps)?)),
                },
                ViaLayer {
                    name: "via2".into(),
                    between: (2, 3),
                    size: (240, 240).into(),
                    raw: Some(rawlayers.add(raw::Layer::from_pairs(69, &via_purps)?)),
                },
                ViaLayer {
                    name: "via3".into(),
                    between: (3, 4),
                    size: (240, 240).into(),
                    raw: Some(rawlayers.add(raw::Layer::from_pairs(70, &via_purps)?)),
                },
            ],
            rawlayers: Some(Ptr::new(rawlayers)),
        };
        Ok(stack)
    }
}
/// Run the test-stacks through validation
#[test]
fn validate_stack() -> LayoutResult<()> {
    use crate::validate;

    let s = SampleStacks::empty()?;
    validate::StackValidator::validate(s)?;
    let s = SampleStacks::pdka()?;
    validate::StackValidator::validate(s)?;
    Ok(())
}
