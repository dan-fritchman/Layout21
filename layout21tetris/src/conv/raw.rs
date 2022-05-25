//!
//! # Raw-Layout Conversion Module
//!

// Std-lib
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::Debug;

// Crates.io
use slotmap::{new_key_type, SlotMap};

// Local imports
use crate::{
    abs, cell,
    coords::{DbUnits, HasUnits, PrimPitches, UnitSpeced, Xy},
    instance::Instance,
    layout::Layout,
    library::Library,
    outline::Outline,
    raw::{self, Dir, LayoutError, LayoutResult, Point},
    stack::{LayerPeriod, RelZ},
    tracks::{Track, TrackCross, TrackSegmentType},
    utils::{ErrorContext, ErrorHelper, Ptr, PtrList},
    validate,
};

// Create key-types for each internal type stored in [SlotMap]s
new_key_type! {
    /// Keys for [ValidAssign] entries
    pub struct AssignKey;
}
/// A short-lived Cell, largely organized by layer
#[derive(Debug, Clone)]
struct TempCell<'lib> {
    /// Reference to the source [Cell]
    cell: &'lib Layout,
    /// Reference to the source [Library]
    lib: &'lib Library,
    /// Instances and references to their definitions
    instances: PtrList<Instance>,
    /// Cuts, arranged by Layer
    cuts: Vec<Vec<&'lib TrackCross>>,
    /// Validated Assignments
    assignments: SlotMap<AssignKey, validate::ValidAssign>,
    /// Assignments, arranged by Layer
    top_assns: Vec<Vec<AssignKey>>,
    /// Assignments, arranged by Layer
    bot_assns: Vec<Vec<AssignKey>>,
}
/// Temporary arrangement of data for a [Layer] within a [Cell]
#[derive(Debug, Clone)]
struct TempCellLayer<'lib> {
    /// Reference to the validated metal layer
    layer: &'lib validate::ValidMetalLayer,
    /// Reference to the parent cell
    cell: &'lib TempCell<'lib>,
    /// Instances which intersect with this layer and period
    instances: PtrList<Instance>,
    /// Pitch per layer-period
    pitch: DbUnits,
    /// Number of layer-periods
    nperiods: usize,
    /// Spanning distance in the layer's "infinite" dimension
    span: DbUnits,
}

/// Short-Lived structure of the stuff relevant for converting a single LayerPeriod,
/// on a single Layer, in a single Cell.
#[derive(Debug, Clone)]
struct TempPeriod<'lib> {
    periodnum: usize,
    cell: &'lib TempCell<'lib>,
    layer: &'lib TempCellLayer<'lib>,
    /// Instance Blockages
    blockages: Vec<(PrimPitches, PrimPitches, Ptr<Instance>)>,
    cuts: Vec<&'lib TrackCross>,
    top_assns: Vec<AssignKey>,
    bot_assns: Vec<AssignKey>,
}
/// # Converter from [Library] and constituent elements to [raw::Library]
#[derive(Debug)]
pub struct RawExporter {
    /// Source [Library]
    lib: Library,
    /// Source (validated) [Stack]
    stack: validate::ValidStack,
    /// HashMap from source [Cell] to exported [raw::Cell],
    /// largely for lookup during conversion of [Instance]s
    rawcells: HashMap<Ptr<cell::Cell>, Ptr<raw::Cell>>,
    /// Context stack, largely for error reporting
    ctx: Vec<ErrorContext>,
}
impl<'lib> RawExporter {
    /// Convert the combination of a [Library] `lib` and [Stack] `stack` to a [raw::Library].
    /// Both `lib` and `stack` are consumed in the process.
    pub fn convert(lib: Library, stack: validate::ValidStack) -> LayoutResult<Ptr<raw::Library>> {
        // Put the combination through absolute-placement
        use crate::placer::Placer;
        let (lib, stack) = Placer::place(lib, stack)?;

        // Run the [Library] through validation
        validate::LibValidator::new(&stack).validate_lib(&lib)?;

        let mut myself = Self {
            lib,
            stack,
            rawcells: HashMap::new(),
            ctx: Vec::new(),
        };
        myself.export_stack()?;
        myself.export_lib()
    }
    /// "Convert" our [Stack]. Really just checks a few properties are valid.
    fn export_stack(&mut self) -> LayoutResult<()> {
        // Require our [Stack] specify both:
        // (a) set of [raw::Layers], and
        // (b) a boundary layer
        // Both these fields are frequently `unwrap`ed hereafter.
        if !self.stack.rawlayers.is_some() {
            return self.fail("Raw export failed: no [raw::Layers] specified");
        }
        if !self.stack.boundary_layer.is_some() {
            return self.fail("Raw export failed: no `boundary_layer` specified");
        }
        Ok(())
    }
    /// Convert everything in our [Library]
    fn export_lib(&mut self) -> LayoutResult<Ptr<raw::Library>> {
        self.ctx.push(ErrorContext::Library(self.lib.name.clone()));
        // Get our starter raw-lib, either anew or from any we've imported
        let rawlibptr = if self.lib.rawlibs.len() == 0 {
            // Create a new [raw::Library]
            let mut rawlib = raw::Library::new(&self.lib.name, self.stack.units);
            rawlib.layers = Ptr::clone(self.stack.rawlayers.as_ref().unwrap());
            Ok(Ptr::new(rawlib))
        } else if self.lib.rawlibs.len() == 1 {
            // Pop the sole raw-library, and use it as a starting point
            let rawlibptr = self.lib.rawlibs.pop().unwrap();
            let mut rawlib = rawlibptr.write()?;
            rawlib.name = self.lib.name.to_string();
            if rawlib.units != self.stack.units {
                // Check that the units match, or fail
                return self.fail(format!(
                    "NotImplemented: varying units between raw and tetris libraries: {:?} vs {:?}",
                    rawlib.units, self.stack.units,
                ));
            }
            drop(rawlib);
            Ok(rawlibptr)
        } else {
            // Multiple raw-libraries will require some merging
            // Probably not difficult, but not done yet either.
            self.fail("NotImplemented: with multiple [raw::Library")
        }?;
        {
            // Get write-access to the raw-lib
            let mut rawlib = rawlibptr.write()?;
            // Convert each defined [Cell] to a [raw::Cell]
            for srcptr in self.lib.dep_order() {
                let rawptr = self.export_cell(&*srcptr.read()?, &mut rawlib.cells)?;
                self.rawcells.insert(srcptr.clone(), rawptr);
            }
        } // Ends `rawlib` write-access scope
        self.ctx.pop();
        Ok(rawlibptr)
    }
    /// Convert a [Cell] to a [raw::Cell] and add to `rawcells`.
    /// FIXME: In reality only one of the cell-views is converted,
    /// generally the "most specific" available view.
    fn export_cell(
        &mut self,
        cell: &cell::Cell,
        rawcells: &mut PtrList<raw::Cell>,
    ) -> LayoutResult<Ptr<raw::Cell>> {
        if let Some(ref x) = cell.raw {
            // Raw definitions store the cell-pointer
            // Just return a copy of it and *don't* add it to `rawcells`
            // First check for validity, i.e. lack of alternate definitions
            if cell.abs.is_some() || cell.layout.is_some() {
                // FIXME: move this to validation stages
                return self.fail(format!(
                    "Cell {} has an invalid combination of raw-definition and tetris-definition",
                    cell.name,
                ));
            }
            return Ok(x.cell.clone());
        }
        if cell.abs.is_none() && cell.layout.is_none() {
            // FIXME: move this to validation stages
            return self.fail(format!(
                "Cell {} has no abstract nor implementation",
                cell.name,
            ));
        }

        // Create the raw-cell
        let mut rawcell = raw::Cell::new(&cell.name.to_string());
        // And create each defined view
        if let Some(ref x) = cell.layout {
            rawcell.layout = Some(self.export_layout_impl(x)?);
        }
        if let Some(ref x) = cell.abs {
            rawcell.abs = Some(self.export_abstract(x)?);
        }
        // Add it to `rawcells`, and return the pointer that comes back
        Ok(rawcells.add(rawcell))
    }
    /// Convert to a raw layout cell
    fn export_layout_impl(&self, layout: &Layout) -> LayoutResult<raw::Layout> {
        if layout.outline.x.len() > 1 {
            return Err(LayoutError::Str(
                "Non-rectangular outline; conversions not supported (yet)".into(),
            ));
        };
        let mut elems: Vec<raw::Element> = Vec::new();
        // Re-organize the cell into the format most helpful here
        let temp_cell = self.temp_cell(layout)?;
        // Convert a layer at a time, starting from bottom
        for layernum in 0..layout.metals {
            // Organize the cell/layer combo into temporary conversion format
            let temp_layer = self.temp_cell_layer(&temp_cell, self.stack.metal(layernum)?)?;
            // Convert each "layer period" one at a time
            for periodnum in 0..temp_layer.nperiods {
                // Again, re-organize into the relevant objects for this "layer period"
                let temp_period = self.temp_cell_layer_period(&temp_layer, periodnum)?;
                // And finally start doing stuff!
                elems.extend(self.export_cell_layer_period(&temp_period)?);
            }
        }

        // Convert our [Instance]s
        let insts = layout
            .instances
            .iter()
            .map(|ptr| {
                let inst = ptr.read()?;
                self.export_instance(&*inst)
            })
            .collect::<Result<Vec<_>, _>>()?;
        // Aaaand create our new [raw::Cell]
        Ok(raw::Layout {
            name: layout.name.clone(),
            insts,
            elems,
            ..Default::default()
        })
    }
    /// Convert an [Instance] to a [raw::Instance]
    fn export_instance(&self, inst: &Instance) -> LayoutResult<raw::Instance> {
        // Get the raw-cell pointer from our mapping.
        // Note this requires dependent cells be converted first, depth-wise.
        let rawkey = self.unwrap(
            self.rawcells.get(&inst.cell),
            format!("Internal Error Exporting Instance {}", inst.inst_name),
        )?;
        // Convert its orientation
        let (reflect_vert, angle) = match (inst.reflect_vert, inst.reflect_horiz) {
            (false, false) => (false, None),     // Default orientation
            (true, false) => (true, None),       // Flip vertically
            (false, true) => (true, Some(180.)), // Flip horiz via vert-flip and rotation
            (true, true) => (false, Some(180.)), // Flip both by rotation
        };
        // Primarily scale the location of each instance by our pitches
        Ok(raw::Instance {
            inst_name: inst.inst_name.clone(),
            cell: rawkey.clone(),
            loc: self.export_xy(inst.loc.abs()?).into(),
            reflect_vert,
            angle,
        })
    }
    /// Create a [TempCell], organizing [Cell] data in more-convenient fashion for conversion
    fn temp_cell<'a>(&'a self, layout: &'a Layout) -> LayoutResult<TempCell<'a>> {
        // Collect references to its instances
        let instances = layout.instances.clone();
        // Validate `cuts`, and arrange them by layer
        let mut cuts: Vec<Vec<&TrackCross>> = vec![vec![]; layout.metals];
        for cut in layout.cuts.iter() {
            validate::LibValidator::new(&self.stack).validate_track_cross(cut)?;
            cuts[cut.track.layer].push(&cut);
            // FIXME: cell validation should also check that this lies within our outline. probably do this earlier
        }

        // Validate all the cell's assignments, and arrange references by layer
        let mut bot_assns = vec![vec![]; layout.metals];
        let mut top_assns = vec![vec![]; layout.metals];
        let mut assignments = SlotMap::with_key();
        for assn in layout.assignments.iter() {
            // Validate the assignment
            let v = validate::LibValidator::new(&self.stack).validate_assign(assn)?;
            let bot = v.bot.layer;
            let top = v.top.layer;

            // Check both layers exist in our stack
            // (This also returns the layer, which we ignore.)
            self.stack.metal(bot)?;
            self.stack.metal(top)?;

            let k = assignments.insert(v);
            bot_assns[bot].push(k);
            top_assns[top].push(k);
        }
        // And create our (temporary) cell data!
        Ok(TempCell {
            cell: layout,
            lib: &self.lib,
            instances,
            assignments,
            top_assns,
            bot_assns,
            cuts,
        })
    }
    /// Convert a single row/col (period) on a single layer in a single Cell.
    fn export_cell_layer_period(
        &self,
        temp_period: &TempPeriod,
    ) -> LayoutResult<Vec<raw::Element>> {
        let mut elems: Vec<raw::Element> = Vec::new();
        let layer = temp_period.layer.layer; // FIXME! Can't love this name.

        // Create the layer-period object we'll manipulate most of the way
        let mut layer_period = temp_period
            .layer
            .layer
            .spec
            .to_layer_period(temp_period.periodnum, temp_period.layer.span.0)?;
        // Insert blockages on each track
        for (n1, n2, inst_ptr) in temp_period.blockages.iter() {
            // Convert primitive-pitch-based blockages to db units
            let start = self.db_units(*n1);
            let stop = self.db_units(*n2);
            let res = layer_period.block(start, stop, &inst_ptr);
            self.ok(
                res,
                format!(
                    "Could not insert blockage on Layer {:?}, period {} from {:?} to {:?}",
                    layer, temp_period.periodnum, start, stop
                ),
            )?;
        }
        // Place all relevant cuts
        let nsig = layer_period.signals.len();
        for cut in temp_period.cuts.iter() {
            // Cut the assigned track
            let track = &mut layer_period.signals[cut.track.track % nsig];
            let cut_loc = self.track_cross_xy(cut)?;
            let dist = cut_loc[layer.spec.dir];
            let res = track.cut(
                dist - layer.spec.cutsize / 2, // start
                dist + layer.spec.cutsize / 2, // stop
                cut,                           // src
            );
            self.ok(
                res,
                format!("Could not make track-cut {:?} in {:?}", cut, temp_period),
            )?;
        }
        // Handle Net Assignments
        // Start with those for which we're the lower of the two layers.
        // These will also be where we add vias.
        let mut via_opt = None;
        for assn_id in temp_period.bot_assns.iter() {
            // Note that while `via_layer` is identical over every iteration of this loop, it may not exist if we never enter the loop.
            // So, retrieve it from the `stack` on our first iteration.
            if via_opt.is_none() {
                via_opt = Some(self.stack.via_from(layer.index)?);
            }
            let via_layer = via_opt.as_ref().unwrap();

            let assn = self.unwrap(
                temp_period.cell.assignments.get(*assn_id),
                "Internal error: invalid assignment",
            )?;
            self.assign_track(layer, &mut layer_period, assn, false)?;
            let assn_loc = self.track_cross_xy(&assn.src.at)?;
            // Create the via element
            let e = raw::Element {
                net: Some(assn.src.net.clone()),
                layer: via_layer.raw.unwrap(),
                purpose: raw::LayerPurpose::Drawing,
                inner: raw::Shape::Rect(raw::Rect {
                    p0: self.export_point(
                        assn_loc.x - via_layer.size.x / 2,
                        assn_loc.y - via_layer.size.y / 2,
                    ),
                    p1: self.export_point(
                        assn_loc.x + via_layer.size.x / 2,
                        assn_loc.y + via_layer.size.y / 2,
                    ),
                }),
            };
            elems.push(e);
        }

        // Assign all the segments for which we're the top layer
        for assn_id in temp_period.top_assns.iter() {
            let assn = self.unwrap(
                temp_period.cell.assignments.get(*assn_id),
                "Internal error: invalid assignment",
            )?;
            self.assign_track(layer, &mut layer_period, assn, true)?;
        }

        // Convert all TrackSegments to raw Elements
        for t in layer_period.rails.iter() {
            elems.extend(self.export_track(t, &layer)?);
        }
        for t in layer_period.signals.iter() {
            elems.extend(self.export_track(t, &layer)?);
        }
        Ok(elems)
    }
    /// Set the net corresponding to `assn` on layer `layer`.
    ///
    /// The type signature, particularly lifetimes, aren't pretty.
    /// <'f> is the short "function lifetime" of the argument references
    pub fn assign_track<'f>(
        &self,
        layer: &'f validate::ValidMetalLayer,
        layer_period: &'f mut LayerPeriod<'lib>,
        assn: &'lib validate::ValidAssign,
        top: bool, // Boolean indication of whether to assign `top` or `bot`. FIXME: not our favorite.
    ) -> LayoutResult<()> {
        // Grab a (mutable) reference to the assigned track
        let nsig = layer_period.signals.len();
        let track = if top { assn.top.track } else { assn.bot.track };
        let track = &mut layer_period.signals[track % nsig];
        // And set the net at the assignment's location
        let assn_loc = self.track_cross_xy(&assn.src.at)?;
        let res = track.set_net(assn_loc[layer.spec.dir], &assn.src);
        self.ok(res, "Error Assigning Track")?;
        Ok(())
    }
    /// Convert a [Abstract] into raw form.
    pub fn export_abstract(&mut self, abs: &abs::Abstract) -> LayoutResult<raw::Abstract> {
        self.ctx.push(ErrorContext::Abstract);

        // Create the outline-element, and grab a copy of its inner shape
        let outline = self.export_outline(&abs.outline)?;
        // Create the raw abstract
        let mut rawabs = raw::Abstract::new(&abs.name);
        rawabs.outline = Some(outline);

        // Draw a blockage on each layer, equal to the shape of the outline
        for layerindex in 0..abs.metals {
            let layerkey = self.stack.metal(layerindex)?.raw.unwrap();
            let blk = vec![raw::Shape::Polygon(outline.clone())];
            rawabs.blockages.insert(layerkey, blk);
        }

        // Create shapes for each port
        for port in abs.ports.iter() {
            let rawport = self.export_abstract_port(abs, port)?;
            rawabs.ports.push(rawport);
        }
        // And return the [raw::Abstract]
        self.ctx.pop();
        Ok(rawabs)
    }
    /// Convert an [abs::Port] into raw form.
    pub fn export_abstract_port(
        &mut self,
        abs: &abs::Abstract,
        port: &abs::Port,
    ) -> LayoutResult<raw::AbstractPort> {
        use abs::PortKind::{Edge, ZTopEdge, ZTopInner};

        let (layerkey, shape): (raw::LayerKey, raw::Shape) = match &port.kind {
            Edge {
                layer: layer_index,
                track,
                side,
            } => {
                let layer = &self.stack.metal(*layer_index)?.spec;
                // First get the "infinite dimension" coordinate from the edge
                let infdims: (DbUnits, DbUnits) = match side {
                    abs::Side::BottomOrLeft => (DbUnits(0), DbUnits(100)),
                    abs::Side::TopOrRight => {
                        // FIXME: this assumes rectangular outlines; will take some more work for polygons.
                        let outside = self.db_units(abs.outline.max(layer.dir));
                        (outside - DbUnits(100), outside)
                    }
                };
                // Now get the "periodic dimension" from our layer-center
                let perdims: (DbUnits, DbUnits) = self.track_span(*layer_index, *track)?;
                // Presuming we're horizontal, points are here:
                let mut pts = [Xy::new(infdims.0, perdims.0), Xy::new(infdims.1, perdims.1)];
                // And if vertical, just transpose them
                if layer.dir == Dir::Vert {
                    pts[0] = pts[0].transpose();
                    pts[1] = pts[1].transpose();
                }
                (
                    self.stack.metal(*layer_index)?.raw.unwrap(),
                    raw::Shape::Rect(raw::Rect {
                        p0: self.export_xy(&pts[0]),
                        p1: self.export_xy(&pts[1]),
                    }),
                )
            }
            ZTopEdge { track, side, into } => {
                let top_metal = if abs.metals == 0 {
                    self.fail("Abs Port with no metal layers")
                } else {
                    Ok(abs.metals - 1)
                }?;
                let layer = &self.stack.metal(top_metal)?.spec;
                let other_layer_index = match into.1 {
                    RelZ::Above => top_metal + 1,
                    RelZ::Below => top_metal - 1,
                };
                let other_layer = self.stack.metal(other_layer_index)?;
                let other_layer_center = other_layer.center(into.0)?;
                // First get the "infinite dimension" coordinate from the edge
                let infdims: (DbUnits, DbUnits) = match side {
                    abs::Side::BottomOrLeft => (DbUnits(0), other_layer_center),
                    abs::Side::TopOrRight => {
                        // FIXME: this assumes rectangular outlines; will take some more work for polygons.
                        let outside = self.db_units(abs.outline.max(layer.dir));
                        (other_layer_center, outside)
                    }
                };
                // Now get the "periodic dimension" from our layer-center
                let perdims: (DbUnits, DbUnits) = self.track_span(top_metal, *track)?;
                // Presuming we're horizontal, points are here:
                let mut pts = [Xy::new(infdims.0, perdims.0), Xy::new(infdims.1, perdims.1)];
                // And if vertical, just transpose them
                if layer.dir == Dir::Vert {
                    pts[0] = pts[0].transpose();
                    pts[1] = pts[1].transpose();
                }
                (
                    self.stack.metal(top_metal)?.raw.unwrap(),
                    raw::Shape::Rect(raw::Rect {
                        p0: self.export_xy(&pts[0]),
                        p1: self.export_xy(&pts[1]),
                    }),
                )
            }
            ZTopInner { .. } => todo!(),
        };
        let mut shapes = HashMap::new();
        shapes.insert(layerkey, vec![shape]);
        let rawport = raw::AbstractPort {
            net: port.name.clone(),
            shapes,
        };
        Ok(rawport)
    }
    /// Get the positions spanning track number `track` on layer number `layer`
    fn track_span(
        &self,
        layer_index: usize,
        track_index: usize,
    ) -> LayoutResult<(DbUnits, DbUnits)> {
        let layer = self.stack.metal(layer_index)?;
        layer.span(track_index)
    }
    /// Convert an [Outline] to a [raw::Shape]
    fn outline_shape(&self, outline: &Outline) -> LayoutResult<raw::Polygon> {
        // FIXME: always uses `Poly`, because some proto-schemas insist on it as the most general.
        // Probably move that conversion down-stack, keep either `Poly` or `Rect` on `layout21::raw::Abstract`.

        // if outline.x.len() == 1 {
        //     // Rectangular
        //     let p0 = Point::new(0, 0);
        //     let xp = self.db_units(outline.x[0]).raw();
        //     let yp = self.db_units(outline.y[0]).raw();
        //     let p1 = Point::new(xp, yp);
        //     return Ok(raw::Shape::Rect { p0, p1 });
        // }
        // Polygon
        // Create an array of Outline-Points
        let mut pts = vec![Point { x: 0, y: 0 }];
        let mut xp: isize;
        let mut yp: isize = 0;
        for i in 0..outline.x.len() {
            xp = self.db_units(outline.x[i]).raw();
            pts.push(Point::new(xp, yp));
            yp = self.db_units(outline.y[i]).raw();
            pts.push(Point::new(xp, yp));
        }
        // Add the final implied Point at (x, y[-1])
        pts.push(Point::new(0, yp));
        Ok(raw::Polygon { points: pts })
    }
    /// Convert an [Outline] to a [raw::Element] polygon
    pub fn export_outline(&self, outline: &Outline) -> LayoutResult<raw::Polygon> {
        // Create the outline shape
        let shape = self.outline_shape(outline)?;
        // And create the [raw::Element]
        Ok(shape)
    }
    /// Convert a [Track]-full of [TrackSegment]s to a vector of [raw::Element] rectangles
    fn export_track(
        &self,
        track: &Track,
        layer: &validate::ValidMetalLayer,
    ) -> LayoutResult<Vec<raw::Element>> {
        let mut elems = Vec::new();
        for seg in &track.segments {
            // Convert wires and rails, skip blockages and cuts
            use TrackSegmentType::*;
            let net: Option<String> = match seg.tp {
                Wire { src } => src.map(|src| src.net.clone()),
                Rail(rk) => Some(rk.to_string()),
                Cut { .. } | Blockage { .. } => continue,
            };
            // Convert the inner shape
            let inner = match track.data.dir {
                Dir::Horiz => raw::Shape::Rect(raw::Rect {
                    p0: self.export_point(seg.start, track.data.start),
                    p1: self.export_point(seg.stop, track.data.start + track.data.width),
                }),
                Dir::Vert => raw::Shape::Rect(raw::Rect {
                    p0: self.export_point(track.data.start, seg.start),
                    p1: self.export_point(track.data.start + track.data.width, seg.stop),
                }),
            };
            // And pack it up as a [raw::Element]
            let e = raw::Element {
                net,
                layer: self.stack.metal(layer.index)?.raw.unwrap(),
                purpose: raw::LayerPurpose::Drawing,
                inner,
            };
            elems.push(e);
        }
        Ok(elems)
    }
    /// Create a [TempCellLayer] for the intersection of `temp_cell` and `layer`
    fn temp_cell_layer<'a>(
        &self,
        temp_cell: &'a TempCell,
        layer: &'a validate::ValidMetalLayer,
    ) -> LayoutResult<TempCellLayer<'a>> {
        // Sort out which of the cell's [Instance]s come up to this layer
        let mut instances = Vec::with_capacity(temp_cell.instances.len());
        for ptr in temp_cell.instances.iter() {
            let inst = ptr.read()?;
            let cell = inst.cell.read()?;
            if cell.metals()? > layer.index {
                instances.push(ptr.clone());
            }
        }
        // And convert it to a [PtrList]
        let instances = PtrList::from_ptrs(instances);

        // Sort out which direction we're working across
        let cell = temp_cell.cell;
        // Convert to database units
        let x = self.db_units(cell.outline.x[0]); // FIXME: rectangles implied here
        let y = self.db_units(cell.outline.y[0]);
        let (span, breadth) = match layer.spec.dir {
            Dir::Horiz => (x, y),
            Dir::Vert => (y, x),
        };

        // FIXME: move to `validate` stage
        if (breadth % layer.pitch) != 0 {
            return self.fail(format!(
                "{} has invalid dimension on {}: {:?}, must be multiple of {:?}",
                cell.name, layer.spec.name, breadth, layer.pitch,
            ));
        }
        let nperiods = usize::try_from(breadth / layer.pitch).unwrap(); // FIXME: errors
        Ok(TempCellLayer {
            layer,
            cell: temp_cell,
            instances,
            nperiods,
            pitch: layer.pitch,
            span,
        })
    }
    /// Create the [TempPeriod] at the intersection of `temp_layer` and `periodnum`
    fn temp_cell_layer_period<'a>(
        &self,
        temp_layer: &'a TempCellLayer,
        periodnum: usize,
    ) -> LayoutResult<TempPeriod<'a>> {
        let cell = temp_layer.cell;
        let layer = temp_layer.layer;
        let dir = layer.spec.dir;

        // For each row, decide which instances intersect
        // Convert these into blockage-areas for the tracks
        let mut blockages = Vec::with_capacity(temp_layer.instances.len());
        for ptr in temp_layer.instances.iter() {
            let inst = &*ptr.read()?;
            if self.instance_intersects(inst, layer, periodnum)? {
                // Create the blockage
                let cell = inst.cell.read()?;
                let start = inst.loc.abs()?[dir];
                let stop = start + cell.outline()?.max(dir);
                blockages.push((start, stop, ptr.clone()));
            }
        }

        // Grab indices of the relevant tracks for this period
        let nsig = temp_layer.layer.period_data.signals.len();
        let relevant_track_nums = (periodnum * nsig, (periodnum + 1) * nsig);
        // Filter cuts down to those in this period
        let cuts: Vec<&TrackCross> = cell.cuts[temp_layer.layer.index]
            .iter()
            .filter(|cut| {
                cut.track.track >= relevant_track_nums.0 && cut.track.track < relevant_track_nums.1
            })
            .map(|r| *r)
            .collect();
        // Filter assignments down to those in this period
        let top_assns = cell.top_assns[temp_layer.layer.index]
            .iter()
            .filter(|id| {
                let assn = cell
                    .assignments
                    .get(**id)
                    .ok_or(LayoutError::from("Internal error: invalid assignment"))
                    .unwrap();
                assn.top.track >= relevant_track_nums.0 && assn.top.track < relevant_track_nums.1
            })
            .copied()
            .collect();
        let bot_assns = cell.bot_assns[temp_layer.layer.index]
            .iter()
            .filter(|id| {
                let assn = cell
                    .assignments
                    .get(**id)
                    .ok_or(LayoutError::from("Internal error: invalid assignment"))
                    .unwrap();
                assn.bot.track >= relevant_track_nums.0 && assn.bot.track < relevant_track_nums.1
            })
            .copied()
            .collect();

        Ok(TempPeriod {
            periodnum,
            cell,
            layer: temp_layer,
            blockages,
            cuts,
            top_assns,
            bot_assns,
        })
    }
    /// Boolean indication of whether `inst` intersects `layer` at `periodnum`
    /// FIXME: rectangular only for now
    fn instance_intersects(
        &self,
        inst: &Instance,
        layer: &validate::ValidMetalLayer,
        periodnum: usize,
    ) -> LayoutResult<bool> {
        // Grab the layer's *periodic* direction
        let dir = !layer.spec.dir;
        // Get its starting location in that dimension
        let inst_start = self.db_units(inst.loc.abs()?[dir]);
        // Sort out whether it's been reflected in this direction
        let reflected = match dir {
            Dir::Horiz => inst.reflect_horiz,
            Dir::Vert => inst.reflect_vert,
        };
        // Grab the span of the cell-outline
        let span = {
            let cell = inst.cell.read()?;
            self.db_units(cell.outline()?.max(dir))
        };
        // And sort out the span of the [Instance], from its cell-outline and reflection
        let (inst_min, inst_max) = if !reflected {
            (inst_start, inst_start + span)
        } else {
            (inst_start - span, inst_start)
        };
        // And return the boolean intersection. "Touching" edge-to-edge is *not* considered an intersection.
        Ok(inst_max > layer.pitch * periodnum && inst_min < layer.pitch * (periodnum + 1))
    }
    /// Convert any [UnitSpeced]-convertible distances into [DbUnits]
    fn db_units(&self, pt: impl Into<UnitSpeced>) -> DbUnits {
        let pt: UnitSpeced = pt.into();
        match pt {
            UnitSpeced::DbUnits(u) => u, // Return as-is
            UnitSpeced::PrimPitches(p) => {
                // Multiply by the primitive pitch in `pt`s direction
                let pitch = self.stack.prim.pitches[p.dir];
                (p.num * pitch.raw()).into()
            }
            UnitSpeced::LayerPitches(_p) => {
                // LayerPitches are always in the layer's "periodic" dimension
                todo!()
            }
        }
    }
    /// Convert an [Xy] into a [raw::Point]
    fn export_xy<T: HasUnits + Into<UnitSpeced>>(&self, xy: &Xy<T>) -> raw::Point {
        let x = self.db_units(xy.x);
        let y = self.db_units(xy.y);
        self.export_point(x, y)
    }
    /// Convert a two-tuple of [DbUnits] into a [raw::Point]
    fn export_point(&self, x: DbUnits, y: DbUnits) -> raw::Point {
        raw::Point::new(x.0, y.0)
    }
    /// Convert a [TrackCross] into an (x,y) ([Xy]) coordinate in [DbUnits]
    fn track_cross_xy(&self, i: &TrackCross) -> LayoutResult<Xy<DbUnits>> {
        // Find the (x,y) center of our track, initially assuming it runs vertically
        let x = self.stack.metal(i.track.layer)?.center(i.track.track)?;
        let y = self.stack.metal(i.cross.layer)?.center(i.cross.track)?;

        // And transpose if it's actually horizontal
        let mut xy = Xy::new(x, y);
        if self.stack.metal(i.track.layer)?.spec.dir == Dir::Horiz {
            xy = xy.transpose();
        }
        Ok(xy)
    }
}
impl ErrorHelper for RawExporter {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
    fn ok<T, E: std::error::Error + Send + Sync + 'static>(
        &self,
        res: Result<T, E>,
        msg: impl Into<String>,
    ) -> Result<T, Self::Error> {
        match res {
            Ok(t) => Ok(t),
            Err(e) => Err(LayoutError::Conversion {
                message: msg.into(),
                err: Box::new(e),
                stack: self.ctx.clone(),
            }),
        }
    }
}
