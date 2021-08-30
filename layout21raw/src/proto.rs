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
use crate::utils::Ptr;
use crate::{
    Cell, Element, ErrorContext, HasErrors, Instance, LayerKey, LayerPurpose, Layers, LayoutError,
    LayoutResult, Library, Point, Shape, TextElement, Units,
};
pub use layout21protos as proto;

/// # ProtoBuf Exporter
#[derive(Debug)]
pub struct ProtoExporter<'lib> {
    pub lib: &'lib Library,
}
impl<'lib> ProtoExporter<'lib> {
    pub fn export(lib: &'lib Library) -> LayoutResult<proto::Library> {
        Self { lib }.export_lib()
    }
    /// Internal implementation method. Convert all, starting from our top-level [Library].
    fn export_lib(&mut self) -> LayoutResult<proto::Library> {
        // Create a new [proto::Library]
        let mut lib = proto::Library::default();
        lib.units = self.export_units(&self.lib.units)?.into();
        // Set its library name
        lib.domain = self.lib.name.clone();
        // And convert each of our cells
        // FIXME: need to write these in dependency-aware order
        lib.cells = self
            .lib
            .cells
            .iter()
            .map(|c| self.export_cell(&*c.read()?))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(lib)
    }
    /// Export the [Units] distance definition
    fn export_units(&self, units: &Units) -> LayoutResult<proto::Units> {
        Ok(match units {
            Units::Micro => proto::Units::Micro,
            Units::Nano => proto::Units::Nano,
            Units::Angstrom => proto::Units::Angstrom,
        })
    }
    /// Convert a [Cell] to a [proto::Cell] cell-definition
    fn export_cell(&self, cell: &Cell) -> LayoutResult<proto::Cell> {
        // Create the empty/default [proto::Cell]
        let mut pcell = proto::Cell::default();
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
            }
        }
        // Now turn those into [proto::LayerShape]s
        for (layernum, elems) in layers {
            let mut layershape = proto::LayerShapes::default();
            layershape.layer = Some(proto::Layer {
                number: layernum.0 as i64,
                purpose: layernum.1 as i64,
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
        Ok(pcell)
    }
    /// Convert an [Instance] to a [proto::Instance]
    fn export_instance(&self, inst: &Instance) -> LayoutResult<proto::Instance> {
        let cell = inst.cell.read()?;
        Ok(proto::Instance {
            name: inst.inst_name.clone(),
            cell: Some(proto::Reference {
                to: Some(proto::reference::To::Local(cell.name.clone())),
            }),
            lower_left: Some(proto::Point::new(inst.p0.x as i64, inst.p0.y as i64)), // FIXME: convert-point or similar method
            rotation_clockwise_degrees: 0,
        })
    }
    /// Convert an [Instance] to a [proto::Instance]
    fn export_annotation(&self, text: &TextElement) -> LayoutResult<proto::TextElement> {
        Ok(proto::TextElement {
            string: text.string.clone(),
            loc: Some(proto::Point::new(text.loc.x as i64, text.loc.y as i64)),
        })
    }
    fn export_element(&self, elem: &Element) -> LayoutResult<ProtoShape> {
        // Convert unconnected nets to the empty string
        let net = if let Some(ref net) = elem.net {
            net.clone()
        } else {
            "".into()
        };
        match &elem.inner {
            Shape::Rect { ref p0, ref p1 } => {
                let minx = p0.x.min(p1.x) as i64;
                let miny = p0.y.min(p1.y) as i64;
                let width = p0.x.max(p1.x) as i64 - minx;
                let height = p0.y.max(p1.y) as i64 - miny;
                Ok(ProtoShape::Rect(proto::Rectangle {
                    net,
                    lower_left: Some(proto::Point::new(minx, miny)),
                    width,
                    height,
                }))
            }
            Shape::Poly { ref pts } => {
                let vertices = pts
                    .iter()
                    .map(|p| proto::Point::new(p.x as i64, p.y as i64))
                    .collect::<Vec<_>>();
                Ok(ProtoShape::Poly(proto::Polygon { net, vertices }))
            }
            Shape::Path { ref width, ref pts } => {
                let width = i64::try_from(*width)?;
                let points = pts
                    .iter()
                    .map(|p| proto::Point::new(p.x as i64, p.y as i64))
                    .collect::<Vec<_>>();
                Ok(ProtoShape::Path(proto::Path { net, width, points }))
            }
        }
    }
}
impl HasErrors for ProtoExporter<'_> {
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Export {
            message: msg.into(),
            stack: Vec::new(), // FIXME! get a stack already! self.ctx_stack.clone(),
        }
    }
}
/// Helper enumeration for converting to several proto-primitives
enum ProtoShape {
    Rect(proto::Rectangle),
    Poly(proto::Polygon),
    Path(proto::Path),
}

/// # ProtoBuf Importer
#[derive(Debug, Default)]
pub struct ProtoImporter {
    pub layers: Ptr<Layers>,
    ctx_stack: Vec<ErrorContext>,
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
        self.ctx_stack.push(ErrorContext::Library(name.clone()));
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
        self.ctx_stack.push(ErrorContext::Units);
        let punits = match proto::Units::from_i32(punits) {
            Some(p) => Ok(p),
            None => self.fail(format!("Invalid proto::Units value {}", punits)),
        }?;
        let ours = match punits {
            proto::Units::Micro => Units::Micro,
            proto::Units::Nano => Units::Nano,
            proto::Units::Angstrom => Units::Angstrom,
        };
        self.ctx_stack.pop();
        Ok(ours)
    }
    /// Import a [Cell]
    fn import_cell(&mut self, pcell: &proto::Cell) -> LayoutResult<Cell> {
        let mut cell = Cell::default();
        let name = pcell.name.clone();
        cell.name = name.clone();
        self.ctx_stack.push(ErrorContext::Cell(name));

        for inst in &pcell.instances {
            cell.insts.push(self.import_instance(inst)?);
        }
        for s in &pcell.shapes {
            cell.elems.extend(self.import_layer_shapes(s)?);
        }
        for txt in &pcell.annotations {
            cell.annotations.push(self.import_annotation(txt)?);
        }
        self.ctx_stack.pop();
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
        self.ctx_stack.push(ErrorContext::Geometry);
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
        self.ctx_stack.pop();
        Ok(elems)
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
        let pts: Vec<Point> = self.import_point_vec(&ppoly.vertices)?;
        Ok(Shape::Poly { pts })
    }
    /// Import a [Shape::Rect]
    fn import_rect(&mut self, prect: &proto::Rectangle) -> LayoutResult<Shape> {
        let p0 = match prect.lower_left {
            Some(ref p) => self.import_point(p),
            None => self.fail("Invalid proto::Rectangle with no location"),
        }?;
        let width = isize::try_from(prect.width)?;
        let height = isize::try_from(prect.height)?;
        let p1 = Point::new(p0.x + width, p0.y + height);
        Ok(Shape::Rect { p0, p1 })
    }
    /// Import a [Shape::Path]
    fn import_path(&mut self, x: &proto::Path) -> LayoutResult<Shape> {
        let pts = self.import_point_vec(&x.points)?;
        let width = usize::try_from(x.width)?;
        Ok(Shape::Path { width, pts })
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
        // Mostly wind through protobuf-generated structures layers of [Option]s
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
        let cname = pinst.name.clone();
        self.ctx_stack.push(ErrorContext::Instance(cname.clone()));
        // Look up the cell-key, which must be imported by now, or we fail
        let cellkey = self.import_reference(&pinst)?;

        let p0 = match &pinst.lower_left {
            Some(p) => self.import_point(p),
            None => self.fail(format!(
                "Invalid proto::Instance with no Location: {}",
                pinst.name
            )),
        }?;
        let angle = if pinst.rotation_clockwise_degrees == 0 {
            None
        } else {
            Some(pinst.rotation_clockwise_degrees as f64)
        };
        let inst = Instance {
            inst_name: pinst.name.clone(),
            cell: cellkey,
            p0,
            reflect: false, // FIXME!
            angle,
        };
        self.ctx_stack.pop();
        Ok(inst)
    }
    /// Import a [Point]
    fn import_point(&mut self, pt: &proto::Point) -> LayoutResult<Point> {
        let x = pt.x.try_into()?;
        let y = pt.y.try_into()?;
        Ok(Point::new(x, y))
    }
    /// Import a vector of [Point]s
    fn import_point_vec(&mut self, pts: &Vec<proto::Point>) -> LayoutResult<Vec<Point>> {
        pts.iter()
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
impl HasErrors for ProtoImporter {
    fn err(&self, msg: impl Into<String>) -> LayoutError {
        LayoutError::Import {
            message: msg.into(),
            stack: self.ctx_stack.clone(),
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
    let c1 = lib.cells.insert(Cell {
        name: "prt_cell".into(),
        elems: vec![
            Element {
                net: Some("prt_rect_net".to_string()),
                layer,
                purpose: purpose.clone(),
                inner: Shape::Rect {
                    p0: Point::default(),
                    p1: Point::default(),
                },
            },
            Element {
                net: Some("prt_poly_net".to_string()),
                layer,
                purpose: purpose.clone(),
                inner: Shape::Poly {
                    pts: vec![Point::default(), Point::default(), Point::default()],
                },
            },
            Element {
                net: Some("prt_path_net".to_string()),
                layer,
                purpose: purpose.clone(),
                inner: Shape::Path {
                    width: 5,
                    pts: vec![Point::default(), Point::default(), Point::default()],
                },
            },
        ],
        insts: Vec::new(),
        annotations: vec![TextElement {
            loc: Point::default(),
            string: "prt_text".into(),
        }],
    });
    lib.cells.insert(Cell {
        name: "prt_cell_with_inst".into(),
        elems: Vec::new(),
        insts: vec![Instance {
            inst_name: "prt_inst".into(),
            p0: Point::new(5, 5),
            cell: c1,
            reflect: false,
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
