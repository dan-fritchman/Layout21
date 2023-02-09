use lyon::{
    geom::Box2D,
    math::point,
    path::polygon::Polygon,
    tessellation::{
        geometry_builder::{BuffersBuilder, VertexBuffers},
        FillOptions, FillTessellator, FillVertex, FillVertexConstructor,
    },
};
use std::collections::HashMap;

use layout21protos::{self, conv as proto_converters};
use layout21raw as raw;
use layout21utils::Ptr;
use log::error;
// Local imports
use crate::{Color, ColorWheel, Vertex};

#[derive(Debug)]
pub struct LayoutDisplay {
    // Data elements
    pub lib: raw::Library,
    pub cell: Ptr<raw::Cell>,

    // Rendering elements
    pub bbox: raw::BoundBox,
    pub layer_colors: HashMap<raw::LayerKey, Color>,
    pub geometry: VertexBuffers<Vertex, u16>,
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

        let mut layer_colors: HashMap<raw::LayerKey, Color> = HashMap::new();

        // let mut rects: Vec<Rect> = Vec::with_capacity(layout.elems.len());
        let bbox: raw::BoundBox = layout.bbox();
        error!("bbox: {:?}", bbox);

        let mut color_wheel = ColorWheel::new();
        let mut tessellator = FillTessellator::new();
        let mut geometry: VertexBuffers<Vertex, u16> = VertexBuffers::new();

        for elem in &layout.elems {
            let color = layer_colors
                .entry(elem.layer)
                .or_insert_with(|| color_wheel.next())
                .clone();

            let shape = &elem.inner;
            match shape {
                raw::Shape::Rect(r) => {
                    let lyon_rect = Box2D::new(
                        point(
                            r.p0.x as f32 / bbox.p1.x as f32,
                            r.p0.y as f32 / bbox.p1.y as f32,
                        ),
                        point(
                            r.p1.x as f32 / bbox.p1.x as f32,
                            r.p1.y as f32 / bbox.p1.y as f32,
                        ),
                    );
                    tessellator
                        .tessellate_rectangle(
                            &lyon_rect,
                            &FillOptions::DEFAULT,
                            &mut BuffersBuilder::new(&mut geometry, WithColor::new(color)),
                        )
                        .unwrap();
                }
                raw::Shape::Path(p) => {
                    let path_rects = p.rects();
                    if path_rects.is_none() {
                        continue;
                    }
                    let path_rects = path_rects.unwrap();
                    for r in path_rects.iter() {
                        let lyon_rect = Box2D::new(
                            point(
                                r.p0.x as f32 / bbox.p1.x as f32,
                                r.p0.y as f32 / bbox.p1.y as f32,
                            ),
                            point(
                                r.p1.x as f32 / bbox.p1.x as f32,
                                r.p1.y as f32 / bbox.p1.y as f32,
                            ),
                        );
                        tessellator
                            .tessellate_rectangle(
                                &lyon_rect,
                                &FillOptions::DEFAULT,
                                &mut BuffersBuilder::new(&mut geometry, WithColor::new(color)),
                            )
                            .unwrap();
                    }
                }
                raw::Shape::Polygon(p) => {
                    let points: Vec<_> = p
                        .points
                        .iter()
                        .map(|p| {
                            lyon::math::Point::new(
                                p.x as f32 / bbox.p1.x as f32,
                                p.y as f32 / bbox.p1.y as f32,
                            )
                        })
                        .collect();
                    let lyon_polygon: Polygon<_> = Polygon {
                        points: &points,
                        closed: true,
                    };
                    tessellator
                        .tessellate_polygon(
                            lyon_polygon,
                            &FillOptions::DEFAULT,
                            &mut BuffersBuilder::new(&mut geometry, WithColor::new(color)),
                        )
                        .unwrap();
                }
            }
        }

        Self {
            lib: rawlib,
            cell: cell.clone(),
            bbox,

            layer_colors,
            geometry,
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
