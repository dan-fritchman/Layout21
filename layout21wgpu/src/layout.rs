use std::collections::HashMap;

use lyon::{
    geom::Box2D,
    math::{point, Transform},
    path::polygon::Polygon,
    tessellation::{
        geometry_builder::{BuffersBuilder, VertexBuffers},
        FillOptions, FillTessellator, FillVertex, FillVertexConstructor,
    },
};

use layout21protos::{self, conv as proto_converters};
use layout21raw as raw;
use layout21utils::Ptr;

// Local imports
use crate::{Color, ColorWheel, Vertex};

///
/// # Tessellate `layout_display` to triangles ready to render on the GPU.
///
pub fn tessellate(layout_display: &LayoutDisplay, size: &Size<u32>) -> VertexBuffers<Vertex, u16> {
    let cell1 = layout_display.cell.read().unwrap();
    let layout = cell1.layout.as_ref().unwrap();
    let bbox = &layout_display.bbox;
    let layer_colors = &layout_display.layer_colors;

    let screen_transform = fit(&bbox, size);
    let transform = &screen_transform.transform;

    let mut tessellator = FillTessellator::new();
    let mut buffers: VertexBuffers<Vertex, u16> = VertexBuffers::new();

    // Closures to convert between lyon and layout21 shapes, applying the transform
    let get_lyon_point = |p: &raw::Point| transform.transform_point(point(p.x as f32, p.y as f32));
    let get_lyon_rect = |r: &raw::Rect| Box2D::new(get_lyon_point(&r.p0), get_lyon_point(&r.p1));

    for elem in &layout.elems {
        let color = layer_colors.get(&elem.layer).unwrap().clone();

        let shape = &elem.inner;
        match shape {
            raw::Shape::Rect(r) => {
                tessellator
                    .tessellate_rectangle(
                        &get_lyon_rect(r),
                        &FillOptions::DEFAULT,
                        &mut BuffersBuilder::new(&mut buffers, WithColor::new(color)),
                    )
                    .unwrap();
            }
            raw::Shape::Path(p) => {
                // Operate on manhattan paths only, for now
                // If the path is non-manhattan, we'll just skip it
                let path_rects = p.rects();
                if path_rects.is_none() {
                    continue;
                }
                let path_rects = path_rects.unwrap();

                // Tessellate each rectangle in the path
                for r in path_rects.iter() {
                    let lyon_rect = get_lyon_rect(r);
                    tessellator
                        .tessellate_rectangle(
                            &lyon_rect,
                            &FillOptions::DEFAULT,
                            &mut BuffersBuilder::new(&mut buffers, WithColor::new(color)),
                        )
                        .unwrap();
                }
            }
            raw::Shape::Polygon(p) => {
                let points: Vec<_> = p.points.iter().map(|p| get_lyon_point(p)).collect();
                let lyon_polygon: Polygon<_> = Polygon {
                    points: &points,
                    closed: true,
                };
                tessellator
                    .tessellate_polygon(
                        lyon_polygon,
                        &FillOptions::DEFAULT,
                        &mut BuffersBuilder::new(&mut buffers, WithColor::new(color)),
                    )
                    .unwrap();
            }
        }
    }

    buffers
}

/// Screen/ window size
#[derive(Debug)]
pub struct Size<T> {
    pub width: T,
    pub height: T,
}
/// Get the transform to fit the bounding box in the screen
fn fit(bbox: &raw::BoundBox, size: &Size<u32>) -> ScreenTransformState {
    let xspan = (bbox.p1.x - bbox.p0.x) as f64;
    let yspan = (bbox.p1.y - bbox.p0.y) as f64;

    // Sort out which dimension to scale to
    let zoom = if yspan * size.width as f64 > xspan * size.height as f64 {
        2.0 / yspan // scale to height
    } else {
        2.0 / xspan // scale to width
    };
    let zoom = (0.9 * zoom) as f32; // leave a bit of padding

    // Get the center of the bounding box
    let xmid = (bbox.p1.x + bbox.p0.x) as f32 / 2.0;
    let ymid = (bbox.p1.y + bbox.p0.y) as f32 / 2.0;

    // Provide a panning coordinate which scales/ zooms it into GPU coordinates
    let pan = (-zoom * xmid, -zoom * ymid);

    ScreenTransformState::new(zoom, pan)
}

/// The state of zooming and panning the screen
#[derive(Debug)]
pub struct ScreenTransformState {
    pub zoom: f32,
    pub pan: (f32, f32),
    pub transform: Transform,
}
impl ScreenTransformState {
    pub fn new(zoom: f32, pan: (f32, f32)) -> Self {
        Self {
            zoom,
            pan,
            transform: Transform::identity()
                .pre_translate((pan.0, pan.1).into())
                .pre_scale(zoom, zoom),
        }
    }
    pub fn identity() -> Self {
        Self {
            zoom: 1.0,
            pan: (0.0, 0.0),
            transform: Transform::identity(),
        }
    }
    pub fn update(&mut self, zoom: f32, pan: (f32, f32)) {
        self.zoom = zoom;
        self.pan = pan;
        self.transform = Transform::identity()
            .pre_translate((pan.0, pan.1).into())
            .pre_scale(zoom, zoom);
    }
}

#[derive(Debug)]
pub struct LayoutDisplay {
    // Source layout data
    pub lib: raw::Library,
    pub cell: Ptr<raw::Cell>,

    // Derived at load time
    pub bbox: raw::BoundBox,
    pub layer_colors: HashMap<raw::LayerKey, Color>,
}
impl LayoutDisplay {
    pub fn from_proto() -> Self {
        let proto_lib: layout21protos::Library =
            proto_converters::open(&resource("sky130_fd_sc_hd__dfxtp_1.pb")).unwrap();
        let rawlib = raw::Library::from_proto(proto_lib, None).unwrap();
        let cell = rawlib.cells[0].clone();
        Self::build(rawlib, cell)
    }
    pub fn build(rawlib: raw::Library, cell: Ptr<raw::Cell>) -> Self {
        let cell1 = cell.read().unwrap();
        let layout = cell1.layout.as_ref().unwrap();

        let bbox: raw::BoundBox = layout.bbox();

        let mut layer_colors: HashMap<raw::LayerKey, Color> = HashMap::new();
        let mut color_wheel = ColorWheel::new();
        for elem in &layout.elems {
            layer_colors
                .entry(elem.layer)
                .or_insert_with(|| color_wheel.next());
        }

        Self {
            lib: rawlib,
            cell: cell.clone(),
            bbox,
            layer_colors,
        }
    }
}

pub struct WithColor {
    pub color: Color,
}
impl WithColor {
    fn new(color: Color) -> Self {
        Self { color }
    }
}
impl FillVertexConstructor<Vertex> for WithColor {
    fn new_vertex(&mut self, vertex: FillVertex) -> Vertex {
        Vertex {
            position: vertex.position().to_array(),
            color: self.color.clone(),
        }
    }
}

/// Grab the full path of resource-file `fname`
fn resource(rname: &str) -> String {
    format!(
        "{}/../layout21converters/resources/{}",
        env!("CARGO_MANIFEST_DIR"),
        rname
    )
}
