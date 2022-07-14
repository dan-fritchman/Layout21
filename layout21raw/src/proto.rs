//! # ProtoBuf Import & Export
//!
//! Converts between [layout21raw] and [layout21proto] types.
//! The two are highly similar, but differ in some details, largely related to serializability.
//!
//! * [Instances] differ between in-memory "pointers" (SlotMap keys) and strings
//! * [Layer]s have more behavior, including enumerated "smart" [LayerPurpose]s,
//!   where the proto-schema uses the two-integer scheme of most layout systems
//! Most other types ([Units, [Point], etc.) are direct translations.
//!

// Std-Lib
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

// Local imports
use crate::{
    utils::{ErrorContext, ErrorHelper, Ptr},
    Abstract, AbstractPort, Cell, DepOrder, Element, Instance, Int, LayerKey, LayerPurpose, Layers,
    Layout, LayoutError, LayoutResult, Library, Path, Point, Polygon, Rect, Shape, TextElement,
    Units,
};
pub use layout21protos as proto;

/// Additional [Library] methods for converting to/from proto-format
impl Library {
    /// Convert to ProtoBuf
    pub fn to_proto(&self) -> LayoutResult<proto::Library> {
        ProtoExporter::export(&self)
    }
    /// Create from ProtoBuf
    pub fn from_proto(plib: proto::Library, layers: Option<Ptr<Layers>>) -> LayoutResult<Library> {
        ProtoImporter::import(&plib, layers)
    }
}
/// # ProtoBuf Exporter
#[derive(Debug)]
pub struct ProtoExporter<'lib> {
    lib: &'lib Library,
    ctx: Vec<ErrorContext>,
}
impl<'lib> ProtoExporter<'lib> {
    pub fn export(lib: &'lib Library) -> LayoutResult<proto::Library> {
        Self {
            lib,
            ctx: Vec::new(),
        }
        .export_lib()
    }
    /// Internal implementation method. Convert all, starting from our top-level [Library].
    fn export_lib(&mut self) -> LayoutResult<proto::Library> {
        self.ctx.push(ErrorContext::Library(self.lib.name.clone()));
        // Create a new [proto::Library]
        let mut plib = proto::Library::default();
        plib.units = self.export_units(&self.lib.units)?.into();
        // Set its library name
        plib.domain = self.lib.name.clone();
        // And convert each of our cells
        for cell in DepOrder::order(self.lib).iter() {
            let cell = cell.read()?;
            let pcell = self.export_cell(&*cell)?;
            plib.cells.push(pcell);
        }
        self.ctx.pop();
        Ok(plib)
    }
    /// Export the [Units] distance definition
    fn export_units(&mut self, units: &Units) -> LayoutResult<proto::Units> {
        Ok(match units {
            Units::Micro => proto::Units::Micro,
            Units::Nano => proto::Units::Nano,
            Units::Angstrom => proto::Units::Angstrom,
            Units::Pico => unimplemented!(),
        })
    }
    /// Convert a [Cell] to a [proto::Cell] cell-definition
    fn export_cell(&mut self, cell: &Cell) -> LayoutResult<proto::Cell> {
        self.ctx.push(ErrorContext::Cell(cell.name.clone()));

        let mut pcell = proto::Cell::default();
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
    fn export_abstract(&mut self, abs: &Abstract) -> LayoutResult<proto::Abstract> {
        self.ctx.push(ErrorContext::Abstract);
        // Create the new [proto::Abstract]
        let mut pabs = proto::Abstract::default();
        // Convert our name
        pabs.name = abs.name.clone();
        // Convert its ports
        for port in abs.ports.iter() {
            pabs.ports.push(self.export_abstract_port(&port)?);
        }
        // Convert its blockages
        for (layerkey, shapes) in abs.blockages.iter() {
            pabs.blockages
                .push(self.export_abstract_blockages(layerkey, shapes)?);
        }
        // Convert its outline
        pabs.outline = Some(self.export_polygon(&abs.outline)?);
        // And we're done - pop the context and return
        self.ctx.pop();
        Ok(pabs)
    }
    /// Export an abstract's blockages for layer `layer`
    fn export_abstract_blockages(
        &mut self,
        layerkey: &LayerKey,
        shapes: &[Shape],
    ) -> LayoutResult<proto::LayerShapes> {
        let mut pshapes = proto::LayerShapes::default();
        pshapes.layer = Some(self.export_layerspec(&layerkey, &LayerPurpose::Obstruction)?);
        for shape in shapes.iter() {
            self.export_and_add_shape(shape, &mut pshapes)?;
        }
        Ok(pshapes)
    }
    /// Export an [AbstractPort]
    fn export_abstract_port(&mut self, port: &AbstractPort) -> LayoutResult<proto::AbstractPort> {
        let mut pport = proto::AbstractPort::default();
        pport.net = port.net.clone();
        for (layerkey, shapes) in &port.shapes {
            // Export the port's shapes. Each `layer` field uses our `Pin` [LayerPurpose].
            let mut pshapes = proto::LayerShapes::default();
            pshapes.layer = Some(self.export_layerspec(&layerkey, &LayerPurpose::Pin)?);
            for shape in shapes.iter() {
                self.export_and_add_shape(shape, &mut pshapes)?;
            }
        }
        Ok(pport)
    }
    /// Convert a [Layout] to a [proto::Cell] cell-definition
    fn export_layout(&mut self, cell: &Layout) -> LayoutResult<proto::Layout> {
        self.ctx.push(ErrorContext::Impl);
        // Create the empty/default [proto::Layout]
        let mut pcell = proto::Layout::default();
        // Convert our name
        pcell.name = cell.name.clone();
        // Convert each [Instance]
        pcell.instances = cell
            .insts
            .iter()
            .map(|c| self.export_instance(c))
            .collect::<Result<Vec<_>, _>>()?;
        // Convert each [Instance]
        pcell.annotations = cell
            .annotations
            .iter()
            .map(|x| self.export_annotation(x))
            .collect::<Result<Vec<_>, _>>()?;
        // Collect up shapes by layer
        // FIXME: should we store them here this way in the first place? Perhaps.
        let mut layers: HashMap<(i16, i16), Vec<&Element>> = HashMap::new();
        let mut layerorder: Vec<(i16, i16)> = Vec::new();
        for elem in &cell.elems {
            let selflayers = self.lib.layers.read()?;
            let layer = selflayers.get(elem.layer).ok_or("Invalid Layer")?;
            let number = layer.layernum;
            let purpose = layer
                .num(&elem.purpose)
                .ok_or("Invalid Layer Purpose / DataType")?
                .clone();
            if layers.contains_key(&(number, purpose)) {
                layers.get_mut(&(number, purpose)).unwrap().push(elem);
            } else {
                layers.insert((number, purpose), vec![elem]);
                layerorder.push((number, purpose));
            }
        }
        // Now turn those into [proto::LayerShape]s
        for layernums in layerorder {
            let elems = layers.get(&layernums).unwrap();
            let mut layershape = proto::LayerShapes::default();
            layershape.layer = Some(proto::Layer {
                number: layernums.0 as i64,
                purpose: layernums.1 as i64,
            });
            for elem in elems {
                // Also sort into the proto-schema's by-shape-type vectors
                match self.export_element(elem)? {
                    ProtoShape::Rect(r) => layershape.rectangles.push(r),
                    ProtoShape::Poly(p) => layershape.polygons.push(p),
                    ProtoShape::Path(p) => layershape.paths.push(p),
                }
            }
            pcell.shapes.push(layershape);
        }
        self.ctx.pop();
        Ok(pcell)
    }
    /// Convert an [Instance] to a [proto::Instance]
    fn export_instance(&mut self, inst: &Instance) -> LayoutResult<proto::Instance> {
        let cell = inst.cell.read()?;
        Ok(proto::Instance {
            name: inst.inst_name.clone(),
            cell: Some(proto::Reference {
                to: Some(proto::reference::To::Local(cell.name.clone())),
            }),
            reflect_vert: inst.reflect_vert,
            origin_location: Some(self.export_point(&inst.loc)?),
            rotation_clockwise_degrees: 0,
        })
    }
    /// Export an [Element]
    fn export_element(&mut self, elem: &Element) -> LayoutResult<ProtoShape> {
        // Create its [proto::Shape]
        let mut pshape = self.export_shape(&elem.inner)?;
        // And assign its net. Convert unconnected nets to the empty string.
        // Because the `net` field is stored on the inner variants in the proto-schema,
        // this requires destructuring the variants.
        if let Some(ref net) = elem.net {
            match pshape {
                ProtoShape::Rect(ref mut r) => r.net = net.to_string(),
                ProtoShape::Poly(ref mut r) => r.net = net.to_string(),
                ProtoShape::Path(ref mut r) => r.net = net.to_string(),
            }
        }
        Ok(pshape)
    }
    /// Export a [Shape]
    fn export_shape(&mut self, shape: &Shape) -> LayoutResult<ProtoShape> {
        match shape {
            Shape::Rect(ref r) => Ok(ProtoShape::Rect(self.export_rect(r)?)),
            Shape::Polygon(ref p) => Ok(ProtoShape::Poly(self.export_polygon(p)?)),
            Shape::Path(ref p) => Ok(ProtoShape::Path(self.export_path(p)?)),
        }
    }
    /// Export a [Rect]
    fn export_rect(&mut self, rect: &Rect) -> LayoutResult<proto::Rectangle> {
        let (p0, p1) = (&rect.p0, &rect.p1);
        let minx = p0.x.min(p1.x) as i64;
        let miny = p0.y.min(p1.y) as i64;
        let width = p0.x.max(p1.x) as i64 - minx;
        let height = p0.y.max(p1.y) as i64 - miny;
        Ok(proto::Rectangle {
            net: "".into(),
            lower_left: Some(proto::Point::new(minx, miny)),
            width,
            height,
        })
    }
    /// Export a [Polygon]
    fn export_polygon(&mut self, poly: &Polygon) -> LayoutResult<proto::Polygon> {
        let vertices = poly
            .points
            .iter()
            .map(|p| self.export_point(p))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(proto::Polygon {
            net: "".into(),
            vertices,
        })
    }
    /// Export a [Path]
    fn export_path(&mut self, path: &Path) -> LayoutResult<proto::Path> {
        let width = i64::try_from(path.width)?;
        let points = path
            .points
            .iter()
            .map(|p| self.export_point(p))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(proto::Path {
            net: "".into(),
            width,
            points,
        })
    }
    /// Convert `shape` and add it to `pshapes`
    fn export_and_add_shape(
        &mut self,
        shape: &Shape,
        pshapes: &mut proto::LayerShapes,
    ) -> LayoutResult<()> {
        // Convert the shape, and add it to the appropriate list
        match shape {
            Shape::Rect(rect) => pshapes.rectangles.push(self.export_rect(rect)?),
            Shape::Polygon(poly) => pshapes.polygons.push(self.export_polygon(poly)?),
            Shape::Path(path) => pshapes.paths.push(self.export_path(path)?),
        };
        Ok(())
    }
    /// Export a [TextElement]
    fn export_annotation(&mut self, text: &TextElement) -> LayoutResult<proto::TextElement> {
        Ok(proto::TextElement {
            string: text.string.clone(),
            loc: Some(self.export_point(&text.loc)?),
        })
    }
    /// Convert a (LayerKey, LayerPurpose) combination to a [proto::Layer]
    fn export_layerspec(
        &mut self,
        layer: &LayerKey,
        purpose: &LayerPurpose,
    ) -> LayoutResult<proto::Layer> {
        let layers = self.lib.layers.read()?;
        let layer = self.unwrap(
            layers.get(*layer),
            format!("Layer {:?} Not Defined in Library {}", layer, self.lib.name),
        )?;
        let purpose = self
            .unwrap(
                layer.num(purpose),
                format!("LayerPurpose Not Defined for {:?}, {:?}", layer, purpose),
            )?
            .clone();
        // Do a few numeric type conversions
        let purpose = purpose.into();
        let number = layer.layernum.into();
        // And return the [proto::Layer]
        Ok(proto::Layer { number, purpose })
    }
    /// Export a [Point]
    fn export_point(&mut self, p: &Point) -> LayoutResult<proto::Point> {
        let x = i64::try_from(p.x)?;
        let y = i64::try_from(p.y)?;
        Ok(proto::Point::new(x, y))
    }
}
impl ErrorHelper for ProtoExporter<'_> {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}
/// Helper enumeration for converting to several proto-primitives
#[derive(Debug, Clone, PartialEq)]
enum ProtoShape {
    Rect(proto::Rectangle),
    Poly(proto::Polygon),
    Path(proto::Path),
}

/// # ProtoBuf Importer
#[derive(Debug, Default)]
pub struct ProtoImporter {
    pub layers: Ptr<Layers>,
    ctx: Vec<ErrorContext>,
    cell_map: HashMap<String, Ptr<Cell>>,
    lib: Library,
}
impl ProtoImporter {
    pub fn import(plib: &proto::Library, layers: Option<Ptr<Layers>>) -> LayoutResult<Library> {
        // Create a default [Layers] if none were provided
        let layers = match layers {
            Some(l) => l,
            None => Ptr::new(Layers::default()),
        };
        // Create the importer
        let mut importer = Self {
            layers,
            ..Default::default()
        };
        // Run the main import method
        importer.import_lib(&plib)?;
        // And destructure the result from our importer
        let Self {
            mut lib, layers, ..
        } = importer;
        lib.layers = layers;
        Ok(lib)
    }
    /// Internal implementation method. Convert the top-level library.
    fn import_lib(&mut self, plib: &proto::Library) -> LayoutResult<()> {
        let name = plib.domain.clone();
        self.ctx.push(ErrorContext::Library(name.clone()));
        // Give our library its name
        self.lib.name = name;
        // Set its distance units
        self.lib.units = self.import_units(plib.units)?;
        // And convert each of its `cells`
        for cell in &plib.cells {
            let name = cell.name.clone();
            let cell = self.import_cell(cell)?;
            let cellkey = self.lib.cells.insert(cell);
            self.cell_map.insert(name, cellkey);
        }
        Ok(())
    }
    /// Import our [Units]. (Yes, proto-code keeps them as an integer. At least it has some helpers.)
    fn import_units(&mut self, punits: i32) -> LayoutResult<Units> {
        self.ctx.push(ErrorContext::Units);
        let punits = match proto::Units::from_i32(punits) {
            Some(p) => Ok(p),
            None => self.fail(format!("Invalid proto::Units value {}", punits)),
        }?;
        let ours = match punits {
            proto::Units::Micro => Units::Micro,
            proto::Units::Nano => Units::Nano,
            proto::Units::Angstrom => Units::Angstrom,
        };
        self.ctx.pop();
        Ok(ours)
    }
    /// Import a [Cell]
    fn import_cell(&mut self, pcell: &proto::Cell) -> LayoutResult<Cell> {
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
    /// Import a [Abstract]
    fn import_abstract(&mut self, pabs: &proto::Abstract) -> LayoutResult<Abstract> {
        self.ctx.push(ErrorContext::Abstract);

        let mut abs = Abstract::default();

        abs.name = pabs.name.clone();

        for port in pabs.ports.iter() {
            abs.ports.push(self.import_abstract_port(port)?);
        }

        for layershapes in pabs.blockages.iter() {
            let proto::Layer { number, purpose } = layershapes.layer.as_ref().unwrap();
            let (layerkey, _) = self
                .layers
                .write()
                .unwrap()
                .get_or_insert(*number as i16, *purpose as i16)
                .unwrap();
            let shapes = self.import_abstract_layer_shapes(layershapes)?;
            abs.blockages.insert(layerkey, shapes);
        }

        abs.outline = match self.import_polygon(&pabs.outline.as_ref().unwrap())? {
            Shape::Polygon(p) => p,
            _ => unreachable!(
                "import_polygon only returns the Shape::Polygon(Polygon) enum variant."
            ),
        };

        Ok(abs)
    }
    /// Import an [AbstractPort]
    fn import_abstract_port(&mut self, pport: &proto::AbstractPort) -> LayoutResult<AbstractPort> {
        let mut port = AbstractPort::default();
        port.net = pport.net.clone();

        for layershapes in pport.shapes.iter() {
            let proto::Layer { number, purpose } = layershapes.layer.as_ref().unwrap();
            let (layerkey, _) = self
                .layers
                .write()
                .unwrap()
                .get_or_insert(*number as i16, *purpose as i16)
                .unwrap();
            let shapes = self.import_abstract_layer_shapes(layershapes)?;
            port.shapes.insert(layerkey, shapes);
        }

        Ok(port)
    }
    /// Import a [Layout]
    fn import_layout(&mut self, playout: &proto::Layout) -> LayoutResult<Layout> {
        let mut cell = Layout::default();
        let name = playout.name.clone();
        cell.name = name.clone();
        self.ctx.push(ErrorContext::Impl);

        for inst in &playout.instances {
            cell.insts.push(self.import_instance(inst)?);
        }
        for s in &playout.shapes {
            cell.elems.extend(self.import_layer_shapes(s)?);
        }
        for txt in &playout.annotations {
            cell.annotations.push(self.import_annotation(txt)?);
        }
        self.ctx.pop();
        Ok(cell)
    }
    /// Import a layer's-worth of shapes
    fn import_layer_shapes(&mut self, player: &proto::LayerShapes) -> LayoutResult<Vec<Element>> {
        // Import the layer
        let (layer, purpose) = match player.layer {
            Some(ref l) => self.import_layer(l),
            None => self.fail("Invalid proto::LayerShapes with no Layer"),
        }?;
        // Import all the shapes
        self.ctx.push(ErrorContext::Geometry);
        let mut elems = Vec::new();
        for shape in &player.rectangles {
            let e = self.import_rect(shape)?;
            let e = self.convert_shape(e, layer, purpose.clone(), &shape.net)?;
            elems.push(e);
        }
        for shape in &player.polygons {
            let e = self.import_polygon(shape)?;
            let e = self.convert_shape(e, layer, purpose.clone(), &shape.net)?;
            elems.push(e);
        }
        for shape in &player.paths {
            let e = self.import_path(shape)?;
            let e = self.convert_shape(e, layer, purpose.clone(), &shape.net)?;
            elems.push(e);
        }
        self.ctx.pop();
        Ok(elems)
    }
    /// Import a layer's-worth of shapes
    fn import_abstract_layer_shapes(
        &mut self,
        player: &proto::LayerShapes,
    ) -> LayoutResult<Vec<Shape>> {
        // Import the layer
        let (layer, purpose) = match player.layer {
            Some(ref l) => self.import_layer(l),
            None => self.fail("Invalid proto::LayerShapes with no Layer"),
        }?;
        // Import all the shapes
        self.ctx.push(ErrorContext::Geometry);
        let mut shapes = Vec::new();
        for shape in &player.rectangles {
            let s = self.import_rect(shape)?;
            shapes.push(s);
        }
        for shape in &player.polygons {
            let s = self.import_polygon(shape)?;
            shapes.push(s);
        }
        for shape in &player.paths {
            let s = self.import_path(shape)?;
            shapes.push(s);
        }
        self.ctx.pop();
        Ok(shapes)
    }
    /// Import a text annotation
    fn import_annotation(&mut self, x: &proto::TextElement) -> LayoutResult<TextElement> {
        let mut e = TextElement::default();
        e.string = x.string.clone();
        e.loc = match x.loc {
            Some(ref loc) => self.import_point(loc)?,
            None => {
                return self.fail(format!(
                    "Invalid positionless proto::TextElement {}",
                    x.string
                ))
            }
        };
        Ok(e)
    }
    /// Import a [Shape::Poly]
    fn import_polygon(&mut self, ppoly: &proto::Polygon) -> LayoutResult<Shape> {
        let points: Vec<Point> = self.import_point_vec(&ppoly.vertices)?;
        Ok(Shape::Polygon(Polygon { points }))
    }
    /// Import a [Shape::Rect]
    fn import_rect(&mut self, prect: &proto::Rectangle) -> LayoutResult<Shape> {
        let p0 = match prect.lower_left {
            Some(ref p) => self.import_point(p),
            None => self.fail("Invalid proto::Rectangle with no location"),
        }?;
        let width = Int::try_from(prect.width)?;
        let height = Int::try_from(prect.height)?;
        let p1 = Point::new(p0.x + width, p0.y + height);
        Ok(Shape::Rect(Rect { p0, p1 }))
    }
    /// Import a [Shape::Path]
    fn import_path(&mut self, x: &proto::Path) -> LayoutResult<Shape> {
        let points = self.import_point_vec(&x.points)?;
        let width = usize::try_from(x.width)?;
        Ok(Shape::Path(Path { width, points }))
    }
    /// Add the finishing touches to convert a [Shape] to an [Element]
    fn convert_shape(
        &mut self,
        inner: Shape,
        layer: LayerKey,
        purpose: LayerPurpose,
        net: &str,
    ) -> LayoutResult<Element> {
        let net = if net.is_empty() {
            None
        } else {
            Some(net.to_string())
        };
        // Create the Element. Note the layer fields are thus far left default.
        Ok(Element {
            net,
            inner,
            layer,
            purpose,
        })
    }
    /// Import a proto-defined pointer, AKA [proto::Reference]
    fn import_reference(&mut self, pinst: &proto::Instance) -> LayoutResult<Ptr<Cell>> {
        // Mostly wind through protobuf-generated structures' layers of [Option]s
        let pref = self.unwrap(
            pinst.cell.as_ref(),
            format!("Invalid proto::Instance with null Cell: {}", pinst.name),
        )?;
        let pref_to = self.unwrap(
            pref.to.as_ref(),
            format!("Invalid proto::Instance with null Cell: {}", pinst.name),
        )?;
        use proto::reference::To::{External, Local};
        let cellname: &str = match pref_to {
            Local(ref name) => Ok(name),
            External(_) => self.fail("Import of external proto-references not supported"),
        }?;
        // Now look that up in our hashmap
        let cellkey = self.unwrap(
            self.cell_map.get(cellname),
            format!("Instance proto::Instance of undefined cell {}", cellname),
        )?;
        Ok(cellkey.clone())
    }
    /// Import an [Instance]
    fn import_instance(&mut self, pinst: &proto::Instance) -> LayoutResult<Instance> {
        let inst_name = pinst.name.clone();
        self.ctx.push(ErrorContext::Instance(inst_name.clone()));
        // Look up the cell-pointer, which must be imported by now, or we fail
        let cell = self.import_reference(&pinst)?;
        // Unwrap the [Option] over (not really optional) location `origin_location`
        let origin_location = self.unwrap(
            pinst.origin_location.as_ref(),
            format!("Invalid proto::Instance with no Location: {}", pinst.name),
        )?;
        // And convert it
        let loc = self.import_point(origin_location)?;
        // Convert the rotation, mapping the proto-default zero to [None]
        let angle = if pinst.rotation_clockwise_degrees == 0 {
            None
        } else {
            Some(f64::from(pinst.rotation_clockwise_degrees))
        };
        let inst = Instance {
            inst_name,
            cell,
            loc,
            reflect_vert: pinst.reflect_vert,
            angle,
        };
        self.ctx.pop();
        Ok(inst)
    }
    /// Import a [Point]
    fn import_point(&mut self, pt: &proto::Point) -> LayoutResult<Point> {
        let x = pt.x.try_into()?;
        let y = pt.y.try_into()?;
        Ok(Point::new(x, y))
    }
    /// Import a vector of [Point]s
    fn import_point_vec(&mut self, points: &Vec<proto::Point>) -> LayoutResult<Vec<Point>> {
        points
            .iter()
            .map(|p| self.import_point(p))
            .collect::<Result<Vec<_>, _>>()
    }
    /// Get the ([LayerKey], [LayerPurpose]) pair for `player`.
    /// Layers are created if they do not already exist,
    /// although this may eventually be a per-importer setting.
    fn import_layer(&mut self, player: &proto::Layer) -> LayoutResult<(LayerKey, LayerPurpose)> {
        // FIXME: maybe a bigger-size type for these layer numbers.
        let num = i16::try_from(player.number)?;
        let purpose = i16::try_from(player.purpose)?;
        let mut layers = self.layers.write()?;
        layers.get_or_insert(num, purpose)
    }
}
impl ErrorHelper for ProtoImporter {
    type Error = LayoutError;
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Import {
            message: msg.into(),
            stack: self.ctx.clone(),
        }
    }
}

#[cfg(all(test, feature = "proto"))]
#[test]
fn proto1() -> LayoutResult<()> {
    // Round-trip through Layout21::Raw -> ProtoBuf -> Layout21::Raw
    let mut lib = Library::new("prt_lib", Units::Nano);
    let (layer, purpose) = {
        let mut layers = lib.layers.write()?;
        layers.get_or_insert(0, 0)?
    };
    let c1 = lib.cells.insert(Layout {
        name: "prt_cell".into(),
        elems: vec![
            Element {
                net: Some("prt_rect_net".to_string()),
                layer,
                purpose: purpose.clone(),
                inner: Shape::Rect(Rect {
                    p0: Point::default(),
                    p1: Point::default(),
                }),
            },
            Element {
                net: Some("prt_poly_net".to_string()),
                layer,
                purpose: purpose.clone(),
                inner: Shape::Polygon(Polygon {
                    points: vec![Point::default(), Point::default(), Point::default()],
                }),
            },
            Element {
                net: Some("prt_path_net".to_string()),
                layer,
                purpose: purpose.clone(),
                inner: Shape::Path(Path {
                    width: 5,
                    points: vec![Point::default(), Point::default(), Point::default()],
                }),
            },
        ],
        insts: Vec::new(),
        annotations: vec![TextElement {
            loc: Point::default(),
            string: "prt_text".into(),
        }],
    });
    lib.cells.insert(Layout {
        name: "prt_cell_with_inst".into(),
        elems: Vec::new(),
        insts: vec![Instance {
            inst_name: "prt_inst".into(),
            loc: Point::new(5, 5),
            cell: c1,
            reflect_vert: false,
            angle: None,
        }],
        annotations: vec![TextElement {
            loc: Point::new(11, 11),
            string: "prt_more_text".into(),
        }],
    });
    let p = lib.to_proto()?;
    let lib2 = ProtoImporter::import(&p, None)?;
    assert_eq!(lib.name, lib2.name);
    assert_eq!(lib.units, lib2.units);
    assert_eq!(lib.cells.len(), lib2.cells.len());
    let layers = lib.layers.read()?;
    assert_eq!(layers.nums.len(), layers.nums.len());
    Ok(())
}
