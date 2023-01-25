//!
//! # ProtoBuf Import & Export
//!

// Std-Lib
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

// Local imports
use crate::{
    // FIXME: some of these should come from `validate`
    abs::{Abstract, Port},
    cell::Cell,
    coords::{HasUnits, PrimPitches, Xy},
    instance::Instance,
    layout::Layout,
    library::Library,
    outline::Outline,
    placement::Place,
    raw::{Dir, LayoutError, LayoutResult},
    stack::{Assign, RelZ},
    tracks::{TrackCross, TrackRef},
    utils::{DepOrder, DepOrderer, ErrorContext, ErrorHelper, Ptr},
};
// Proto-crate imports and aliases
use layout21protos as proto;
use proto::raw as rawproto;
use proto::tetris as tproto;

/// # ProtoBuf Exporter
#[derive(Debug)]
pub struct ProtoExporter<'lib> {
    lib: &'lib Library,     // Source [Library]
    ctx: Vec<ErrorContext>, // Error Stack
}
impl<'lib> ProtoExporter<'lib> {
    pub fn export(lib: &'lib Library) -> LayoutResult<tproto::Library> {
        Self {
            lib,
            ctx: Vec::new(),
        }
        .export_lib()
    }
    /// Internal implementation method. Convert all, starting from our top-level [Library].
    fn export_lib(&mut self) -> LayoutResult<tproto::Library> {
        self.ctx.push(ErrorContext::Library(self.lib.name.clone()));
        // Create a new [tproto::Library]
        let mut plib = tproto::Library::default();

        // Set its library name
        plib.domain = self.lib.name.clone();
        // And convert each of our cells
        for cell in CellOrder::order(&self.lib.cells)?.iter() {
            let pcell = self.export_cell(&*cell.read()?)?;
            plib.cells.push(pcell);
        }
        self.ctx.pop();
        Ok(plib)
    }
    /// Convert a [Cell] to a [tproto::Cell] cell-definition
    fn export_cell(&mut self, cell: &Cell) -> LayoutResult<tproto::Cell> {
        self.ctx.push(ErrorContext::Cell(cell.name.clone()));
        let mut pcell = tproto::Cell::default();
        pcell.name = cell.name.clone();

        if let Some(ref lay) = cell.layout {
            pcell.layout = Some(self.export_layout(lay)?);
        }
        if let Some(ref a) = cell.abs {
            pcell.r#abstract = Some(self.export_abstract(a)?);
        }
        self.ctx.pop();
        Ok(pcell)
    }
    /// Convert a [Abstract]
    fn export_abstract(&mut self, abs: &Abstract) -> LayoutResult<tproto::Abstract> {
        self.ctx.push(ErrorContext::Abstract);
        // Create the new [tproto::Abstract]
        let mut pabs = tproto::Abstract::default();
        // Convert our name
        pabs.name = abs.name.clone();
        // Convert its ports
        for port in abs.ports.iter() {
            let pport = self.export_abstract_port(&port, abs.metals)?;
            pabs.ports.push(pport);
        }
        // Convert its outline
        pabs.outline = Some(self.export_outline(&abs.outline, abs.metals)?);
        // And we're done - pop the context and return
        self.ctx.pop();
        Ok(pabs)
    }
    /// Export an [Outline]
    fn export_outline(
        &mut self,
        outline: &Outline,
        metals: usize,
    ) -> LayoutResult<tproto::Outline> {
        let x = self.export_dimensions(outline.x.as_slice())?;
        let y = self.export_dimensions(outline.y.as_slice())?;
        let metals = i64::try_from(metals)?;
        Ok(tproto::Outline { x, y, metals })
    }
    /// Export an abstract's [Port]
    fn export_abstract_port(
        &mut self,
        port: &Port,
        metals: usize,
    ) -> LayoutResult<tproto::AbstractPort> {
        use crate::abs::{PortKind, Side};
        let mut pport = tproto::AbstractPort::default();
        pport.net = port.name.clone();

        use tproto::abstract_port::{EdgePort, Kind, PortSide, ZTopEdgePort};
        let kind = match &port.kind {
            PortKind::Edge { layer, track, side } => {
                let track = Some(tproto::TrackRef {
                    layer: i64::try_from(*layer)?,
                    track: i64::try_from(*track)?,
                });
                let side = match side {
                    Side::BottomOrLeft => PortSide::BottomOrLeft,
                    Side::TopOrRight => PortSide::TopOrRight,
                };
                let side = i32::from(side);
                Kind::Edge(EdgePort { track, side })
            }
            PortKind::ZTopEdge { track, side, into } => {
                let track = i64::try_from(*track)?;
                let into = {
                    let layer = match into.1 {
                        RelZ::Above => metals + 1,
                        RelZ::Below => metals - 1,
                    };
                    Some(tproto::TrackRef {
                        layer: i64::try_from(layer)?,
                        track: i64::try_from(into.0)?,
                    })
                };
                let side = match side {
                    Side::BottomOrLeft => PortSide::BottomOrLeft,
                    Side::TopOrRight => PortSide::TopOrRight,
                };
                let side = i32::from(side);

                Kind::ZtopEdge(ZTopEdgePort { track, side, into })
            }
            PortKind::ZTopInner { locs: _ } => todo!(),
        };
        pport.kind = Some(kind);
        Ok(pport)
    }
    /// Export a [Layout] to a [tproto::Layout] cell-implementation
    fn export_layout(&mut self, layout: &Layout) -> LayoutResult<tproto::Layout> {
        self.ctx.push(ErrorContext::Impl);
        // Create the empty/default [tproto::Layout]
        let mut playout = tproto::Layout::default();
        // Convert our name
        playout.name = layout.name.clone();
        playout.outline = Some(self.export_outline(&layout.outline, layout.metals)?);
        // Convert each [Instance]
        for ptr in layout.instances.iter() {
            let inst = ptr.read()?;
            playout.instances.push(self.export_instance(&*inst)?);
        }
        // Convert each [Assign]
        for assn in &layout.assignments {
            playout.assignments.push(self.export_assignment(assn)?);
        }
        // Convert each [Cut]
        for cut in &layout.cuts {
            playout.cuts.push(self.export_track_cross(cut)?);
        }
        self.ctx.pop();
        Ok(playout)
    }
    /// Convert an [Instance] to a [tproto::Instance]
    fn export_instance(&mut self, inst: &Instance) -> LayoutResult<tproto::Instance> {
        let cell = inst.cell.read()?;
        let abs_loc = inst.loc.abs()?; // FIXME: asserts the instance has an absolute location, for now
        let loc = self.export_point(&abs_loc)?;

        // This is where the auto-generated proto-`oneof`s get fun.
        // proto::tetris::Place <= this is a struct
        // proto::tetris::place <= this is a `mod`, lower-case named after that struct
        // proto::tetris::place::Place <= this is an enum, upper-case named after that `mod`
        // Got it?
        let loc = Some(proto::tetris::Place {
            place: Some(proto::tetris::place::Place::Abs(loc)),
        });

        Ok(tproto::Instance {
            name: inst.inst_name.clone(),
            cell: Some(proto::utils::Reference {
                to: Some(proto::utils::reference::To::Local(cell.name.clone())),
            }),
            reflect_vert: inst.reflect_vert,
            reflect_horiz: inst.reflect_horiz,
            loc,
        })
    }
    /// Export an [Assign] to a [tproto::Assign]
    fn export_assignment(&mut self, assn: &Assign) -> LayoutResult<tproto::Assign> {
        let mut passn = tproto::Assign::default();
        passn.net = assn.net.clone();
        passn.at = Some(self.export_track_cross(&assn.at)?);
        Ok(passn)
    }
    /// Export a [TrackCross]
    fn export_track_cross(&mut self, cross: &TrackCross) -> LayoutResult<tproto::TrackCross> {
        let track = Some(self.export_track_ref(&cross.track)?);
        let cross = Some(self.export_track_ref(&cross.cross)?);
        let pcross = tproto::TrackCross { track, cross };
        Ok(pcross)
    }
    /// Export a [TrackRef]
    fn export_track_ref(&mut self, track: &TrackRef) -> LayoutResult<tproto::TrackRef> {
        let layer = i64::try_from(track.layer)?;
        let track = i64::try_from(track.track)?;
        Ok(tproto::TrackRef { layer, track })
    }
    /// Export a list of [HasUnit] dimensioned distance-values to
    fn export_dimensions<T: HasUnits>(&mut self, p: &[T]) -> LayoutResult<Vec<i64>> {
        let mut rv = Vec::with_capacity(p.len());
        for val in p {
            rv.push(self.export_dimension(val)?);
        }
        Ok(rv)
    }
    /// Export a [HasUnit] dimensioned distance-value to `i64`
    fn export_dimension<T: HasUnits>(&mut self, p: &T) -> LayoutResult<i64> {
        // Convert to no-unit integers, and then to i64
        Ok(i64::try_from(p.raw())?)
    }
    /// Export a [Point]
    fn export_point<T: HasUnits>(&mut self, p: &Xy<T>) -> LayoutResult<rawproto::Point> {
        let p = p.raw(); // Convert to no-unit integers
        let x = i64::try_from(p.x)?; // Convert into i64
        let y = i64::try_from(p.y)?;
        Ok(rawproto::Point::new(x, y)) // And send back a proto-Point
    }
} // impl ProtoExporter
impl ErrorHelper for ProtoExporter<'_> {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}

/// Empty struct for implementing the [DepOrder] trait for library [Cell]s
struct CellOrder;
impl DepOrder for CellOrder {
    type Item = Ptr<Cell>;
    type Error = LayoutError;

    /// Process [Cell]-pointer `item`
    /// Follow its `instances` list, pushing their `cell` to the stack
    fn process(item: &Ptr<Cell>, orderer: &mut DepOrderer<Self>) -> LayoutResult<()> {
        let cell = item.read()?;
        if let Some(layout) = &cell.layout {
            // Push all instances first
            for ptr in layout.instances.iter() {
                let inst = ptr.read()?;
                orderer.push(&inst.cell)?;
            }
        }
        Ok(())
    }
    fn fail() -> Result<(), Self::Error> {
        LayoutError::fail("Cell ordering error")
    }
}

/// # ProtoBuf Library Importer
#[derive(Debug, Default)]
pub struct ProtoLibImporter {
    ctx: Vec<ErrorContext>,               // Error Stack
    cell_map: HashMap<String, Ptr<Cell>>, // Proto cell-name => [Cell]
}
impl ProtoLibImporter {
    pub fn import(plib: &tproto::Library) -> LayoutResult<Library> {
        // Run the main import-implementation method
        Self::default().import_lib(&plib)
    }
    /// Internal implementation method. Convert the top-level library.
    fn import_lib(&mut self, plib: &tproto::Library) -> LayoutResult<Library> {
        let name = plib.domain.clone();
        self.ctx.push(ErrorContext::Library(name.clone()));
        // Give our library its name
        let mut lib = Library::new(name);
        // And convert each of its `cells`
        for cell in &plib.cells {
            let name = cell.name.clone();
            let cell = self.import_cell(cell)?;
            let cellkey = lib.cells.insert(cell);
            self.cell_map.insert(name, cellkey);
        }
        Ok(lib)
    }
    /// Import a [Cell]
    fn import_cell(&mut self, pcell: &tproto::Cell) -> LayoutResult<Cell> {
        self.ctx.push(ErrorContext::Cell(pcell.name.clone()));
        let mut cell = Cell::new(&pcell.name);
        if let Some(ref lay) = pcell.layout {
            cell.layout = Some(self.import_layout(lay)?);
        }
        if let Some(ref a) = pcell.r#abstract {
            cell.abs = Some(self.import_abstract(a)?);
        }
        self.ctx.pop();
        Ok(cell)
    }
    /// Import an [Abstract]
    fn import_abstract(&mut self, pabs: &tproto::Abstract) -> LayoutResult<Abstract> {
        self.ctx.push(ErrorContext::Abstract);

        // Convert the abstract's [Outline]
        let poutline = self.unwrap(pabs.outline.as_ref(), "Invalid Abstract with no Outline")?;
        let (outline, metals) = self.import_outline(poutline)?;
        // Create our in-memory [Abstract]
        let mut abs = Abstract::new(&pabs.name, metals, outline);
        // And convert each of its [Port]s
        for pport in &pabs.ports {
            abs.ports.push(self.import_abstract_port(pport)?);
        }

        self.ctx.pop();
        Ok(abs)
    }
    /// Import an Abstract [Port]
    fn import_abstract_port(&mut self, _pport: &tproto::AbstractPort) -> LayoutResult<Port> {
        todo!() // FIXME!
    }
    /// Import an [Outline]
    fn import_outline(&mut self, poutline: &tproto::Outline) -> LayoutResult<(Outline, usize)> {
        let x = self.import_prim_pitches_list(poutline.x.as_slice(), Dir::Horiz)?;
        let y = self.import_prim_pitches_list(poutline.y.as_slice(), Dir::Vert)?;
        let metals = usize::try_from(poutline.metals)?;
        Ok((Outline::from_prim_pitches(x, y)?, metals))
    }
    /// Import an [Assign]
    fn import_assignment(&mut self, passn: &tproto::Assign) -> LayoutResult<Assign> {
        let at = self.unwrap(passn.at.as_ref(), "Invalid proto Assign with no location ")?;
        let at = self.import_track_cross(at)?;
        let assn = Assign::new(passn.net.clone(), at);
        Ok(assn)
    }
    /// Import a [TrackCross]
    fn import_track_cross(&mut self, pcross: &tproto::TrackCross) -> LayoutResult<TrackCross> {
        // Create a [TrackCross], always using `pcross.top` as the primary track, and `pcross.bot` as its intersection.
        // First unwrap the not-really-optional `track` and `cross` fields
        let track = self.unwrap(pcross.track.as_ref(), "Invalid TrackCross missing `top`")?;
        let cross = self.unwrap(pcross.cross.as_ref(), "Invalid TrackCross missing `top`")?;
        let track = self.import_track_ref(track)?;
        let cross = self.import_track_ref(cross)?;
        // And create the intersection
        let cross = TrackCross::new(track, cross);
        Ok(cross)
    }
    /// Import a [TrackRef]
    fn import_track_ref(&mut self, pref: &tproto::TrackRef) -> LayoutResult<TrackRef> {
        let layer = usize::try_from(pref.layer)?;
        let track = usize::try_from(pref.track)?;
        Ok(TrackRef { layer, track })
    }
    /// Import a [Layout]
    fn import_layout(&mut self, playout: &tproto::Layout) -> LayoutResult<Layout> {
        self.ctx.push(ErrorContext::Impl);
        let name = playout.name.clone();

        let poutline = self.unwrap(
            playout.outline.as_ref(),
            format!("Invalid tproto::Instance with no Outline: {}", playout.name),
        )?;

        let (outline, metals) = self.import_outline(poutline)?;
        let mut layout = Layout::new(name, metals, outline);

        for inst in &playout.instances {
            layout.instances.push(self.import_instance(inst)?);
        }
        for s in &playout.assignments {
            layout.assignments.push(self.import_assignment(s)?);
        }
        for txt in &playout.cuts {
            layout.cuts.push(self.import_track_cross(txt)?);
        }
        self.ctx.pop();
        Ok(layout)
    }
    /// Import an [Instance]
    fn import_instance(&mut self, pinst: &tproto::Instance) -> LayoutResult<Ptr<Instance>> {
        let inst_name = pinst.name.clone();
        self.ctx.push(ErrorContext::Instance(inst_name.clone()));

        // Look up the cell-pointer, which must be imported by now, or we fail
        let cell = self.import_reference(&pinst)?;

        // Mostly wind through protobuf-generated structures' layers of [Option]s
        let loc = self.unwrap(
            pinst.loc.as_ref(),
            format!("Invalid tproto::Instance with no Location: {}", pinst.name),
        )?;
        let loc = self.unwrap(
            loc.place.as_ref(),
            format!("Invalid tproto::Instance with no Location: {}", pinst.name),
        )?;
        use tproto::place::Place::{Abs, Rel};
        let loc = match loc {
            Abs(ref p) => self.import_xy_prim_pitches(p)?,
            Rel(_) => self.fail("Proto-imports of relative placements are not (yet) supported")?,
        };
        let loc = Place::Abs(loc);

        let inst = Instance {
            inst_name,
            cell,
            loc,
            reflect_horiz: pinst.reflect_horiz,
            reflect_vert: pinst.reflect_vert,
        };
        let inst = Ptr::new(inst);
        self.ctx.pop();
        Ok(inst)
    }
    /// Import a proto-defined pointer, AKA [tproto::Reference]
    fn import_reference(&mut self, pinst: &tproto::Instance) -> LayoutResult<Ptr<Cell>> {
        // Mostly wind through protobuf-generated structures' layers of [Option]s
        let pref = self.unwrap(
            pinst.cell.as_ref(),
            format!("Invalid tproto::Instance with null Cell: {}", pinst.name),
        )?;
        let pref_to = self.unwrap(
            pref.to.as_ref(),
            format!("Invalid tproto::Instance with null Cell: {}", pinst.name),
        )?;
        use proto::utils::reference::To::{External, Local};
        let cellname: &str = match pref_to {
            Local(ref name) => Ok(name),
            External(_) => self.fail("Import of external proto-references not supported"),
        }?;
        // Now look that up in our hashmap
        let cellptr = self.unwrap(
            self.cell_map.get(cellname),
            format!("Instance tproto::Instance of undefined cell {}", cellname),
        )?;
        Ok(cellptr.clone())
    }
    /// Import a [tproto::Point] designed to be interpreted as [PrimPitches]
    fn import_xy_prim_pitches(&mut self, pt: &rawproto::Point) -> LayoutResult<Xy<PrimPitches>> {
        let x = PrimPitches::x(pt.x.try_into()?);
        let y = PrimPitches::y(pt.y.try_into()?);
        Ok(Xy::new(x, y))
    }
    /// Import a list of primitive-pitch dimensions
    fn import_prim_pitches_list(
        &mut self,
        pts: &[i64],
        dir: Dir,
    ) -> LayoutResult<Vec<PrimPitches>> {
        let mut rv = Vec::with_capacity(pts.len());
        for pt in pts {
            rv.push(self.import_prim_pitches(*pt, dir)?);
        }
        Ok(rv)
    }
    /// Import a [tproto::Point] designed to be interpreted as [PrimPitches]
    fn import_prim_pitches(&mut self, pt: i64, dir: Dir) -> LayoutResult<PrimPitches> {
        Ok(PrimPitches::new(dir, pt.try_into()?))
    }
} // impl ProtoLibImporter

impl ErrorHelper for ProtoLibImporter {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Import {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::SerializationFormat::Yaml;

    #[test]
    fn proto_roundtrip1() -> LayoutResult<()> {
        // Proto-export
        let lib = Library::new("proto_rt1");
        let plib = ProtoExporter::export(&lib)?;
        assert_eq!(plib.domain, "proto_rt1");
        assert_eq!(plib.cells, Vec::new());
        assert_eq!(plib.author, None);

        // Import it back
        let lib2 = ProtoLibImporter::import(&plib)?;
        assert_eq!(lib2.name, "proto_rt1");
        assert_eq!(lib2.cells.len(), 0);
        assert_eq!(lib2.rawlibs.len(), 0);

        Ok(())
    }

    #[test]
    fn proto_roundtrip2() -> LayoutResult<()> {
        // Proto round-trip, round 2, with some content
        let mut lib = Library::new("proto_rt2");
        let mut cell = Cell::new("proto_rt2_cell");
        cell.layout = Some(Layout::new("proto_rt2_cell", 0, Outline::rect(1, 1)?));
        cell.abs = Some(Abstract::new("proto_rt2_cell", 0, Outline::rect(1, 1)?));
        lib.cells.add(cell);

        let plib = ProtoExporter::export(&lib)?;
        assert_eq!(plib.domain, "proto_rt2");
        assert_eq!(plib.author, None);
        assert_eq!(plib.cells.len(), 1);
        let pcell = &plib.cells[0];
        let playout = pcell.layout.as_ref().unwrap();
        assert_eq!(playout.name, "proto_rt2_cell");
        let playout_outline = playout.outline.as_ref().unwrap();
        assert_eq!(playout_outline.x, vec![1]);
        assert_eq!(playout_outline.y, vec![1]);
        let pabs = pcell.r#abstract.as_ref().unwrap();
        assert_eq!(pabs.name, "proto_rt2_cell");
        assert_eq!(pabs.ports.len(), 0);
        let pabs_outline = pabs.outline.as_ref().unwrap();
        assert_eq!(pabs_outline.x, vec![1]);
        assert_eq!(pabs_outline.y, vec![1]);
        assert_eq!(pabs_outline, playout_outline);

        // Import it back
        let lib2 = ProtoLibImporter::import(&plib)?;
        assert_eq!(lib2.name, "proto_rt2");
        assert_eq!(lib2.rawlibs.len(), 0);
        assert_eq!(lib2.cells.len(), 1);
        let cell2 = &lib2.cells[0].read()?;
        let layout2 = cell2.layout.as_ref().unwrap();
        assert_eq!(layout2.name, "proto_rt2_cell");
        assert_eq!(layout2.outline, Outline::rect(1, 1)?);
        assert_eq!(layout2.metals, 0);
        assert_eq!(layout2.instances.len(), 0);
        assert_eq!(layout2.assignments.len(), 0);
        assert_eq!(layout2.cuts.len(), 0);
        let abs2 = cell2.abs.as_ref().unwrap();
        assert_eq!(abs2.name, "proto_rt2_cell");
        assert_eq!(abs2.ports.len(), 0);

        // Yaml.save(&plib, "proto_rt2.yaml")?;
        Ok(())
    }

    #[test]
    fn proto_yaml1() -> LayoutResult<()> {
        // Proto export, then YAML export
        let lib = Library::new("proto_yaml1");
        let plib = ProtoExporter::export(&lib)?;
        assert_eq!(plib.domain, "proto_yaml1");
        assert_eq!(plib.cells, Vec::new());
        assert_eq!(plib.author, None);

        // Yaml.save(&plib, "proto_yaml1.yaml")?;
        Ok(())
    }
    #[test]
    fn proto_yaml2() -> LayoutResult<()> {
        // Import from YAML
        let yaml = r#"
---
domain: proto_rt2
cells:
  - name: proto_rt2_cell
    abstract:
      name: proto_rt2_cell
      outline:
        x: [ 1 ]
        y: [ 1 ]
        metals: 0
      ports: []
    layout:
      name: proto_rt2_cell
      outline:
        x: [ 1 ]
        y: [ 1 ]
        metals: 0
      instances: []
      assignments: []
      cuts: []
"#;
        let plib: tproto::Library = Yaml.from_str(yaml)?;
        let _lib = ProtoLibImporter::import(&plib)?;

        // let lib = Library::new("proto_yaml1");
        // let plib = ProtoExporter::export(&lib)?;
        // assert_eq!(plib.domain, "proto_yaml1");
        // assert_eq!(plib.cells, Vec::new());
        // assert_eq!(plib.author, None);

        // Yaml.save(&plib, "proto_yaml2.yaml")?;
        Ok(())
    }
}
