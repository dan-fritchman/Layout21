use std::{iter, mem};

use bytemuck::{Pod, Zeroable};

use layout21protos::{self, conv as proto_converters};
use log::{debug, error, info, log_enabled, Level};
use rand::Rng;
use wgpu::util::DeviceExt;
use winit::{
    event::*,
    event_loop::{ControlFlow, EventLoop},
    window::{Window, WindowBuilder},
};

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
struct Vertex {
    position: [f32; 2],
    color: Color,
}
impl Vertex {
    const ATTRIBUTES: [wgpu::VertexAttribute; 2] =
        wgpu::vertex_attr_array![0 => Float32x2, 1 => Float32x3];
    fn desc<'a>() -> wgpu::VertexBufferLayout<'a> {
        wgpu::VertexBufferLayout {
            array_stride: mem::size_of::<Vertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &Self::ATTRIBUTES,
        }
    }
}

struct Rect {
    p0: [f32; 2],
    p1: [f32; 2],
    color: Color,
}
impl Rect {
    /// Triangulated vertices
    fn vertices(&self) -> [Vertex; 6] {
        let Rect { p0, p1, color } = self;
        let [x0, y0] = *p0;
        let [x1, y1] = *p1;
        [
            Vertex {
                position: [x0, y0],
                color: *color,
            },
            Vertex {
                position: [x1, y0],
                color: *color,
            },
            Vertex {
                position: [x0, y1],
                color: *color,
            },
            Vertex {
                position: [x0, y1],
                color: *color,
            },
            Vertex {
                position: [x1, y0],
                color: *color,
            },
            Vertex {
                position: [x1, y1],
                color: *color,
            },
        ]
    }
}

const NUM_RECTS: usize = 10_000;
const NUM_VERTICES: usize = NUM_RECTS * 6;
struct RectHolder {
    rects: Vec<Rect>,
    vertices: Vec<Vertex>,
}
impl RectHolder {
    /// Generate a random set of rectangles
    fn random() -> RectHolder {
        let mut rects = Vec::with_capacity(NUM_RECTS);
        let mut rng = rand::thread_rng();
        let mut random = || rng.gen_range(-1.0..1.0);
        for _ in 0..NUM_RECTS {
            let r = Rect {
                p0: [random(), random()],
                p1: [random(), random()],
                color: Color([random(), random(), random()]),
            };
            rects.push(r);
        }
        let mut vertices = Vec::with_capacity(6 * NUM_RECTS);
        for rect in rects.iter() {
            vertices.extend_from_slice(&rect.vertices());
        }
        Self { rects, vertices }
    }
}

// const NUM_TRIANGLES: usize = 100_000;
// const NUM_VERTICES: usize = NUM_TRIANGLES * 3;

// fn generate_vertices() -> Vec<Vertex> {
//     let mut rng = rand::thread_rng();
//     let mut random = || rng.gen_range(-1.0..1.0);
//     let mut vertices = Vec::new();
//     for _ in 0..NUM_VERTICES {
//         let x = random();
//         let y = random();
//         let color = Color([random(), random(), random()]);
//         vertices.push(Vertex {
//             position: [x, y],
//             color,
//         });
//     }
//     vertices
// }

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
struct Color(pub [f32; 3]);

// const RED: Color = Color([1.0, 0.0, 0.0]);
// const GREEN: Color = Color([0.0, 1.0, 0.0]);
// const BLUE: Color = Color([0.0, 0.0, 1.0]);
// const BLACK: Color = Color([0.0, 0.0, 0.0]);
// const WHITE: Color = Color([1.0, 1.0, 1.0]);

// const VERTICES: &[Vertex] = &[
//     Vertex {
//         // vertex a
//         position: [-0.5, -0.5],
//         color: RED,
//     },
//     Vertex {
//         // vertex b
//         position: [0.5, -0.5],
//         color: RED,
//     },
//     Vertex {
//         // vertex d
//         position: [-0.5, 0.5],
//         color: RED,
//     },
//     Vertex {
//         // vertex d
//         position: [-0.5, 0.5],
//         color: Color([1.0, 1.0, 0.0]),
//     },
//     Vertex {
//         // vertex b
//         position: [0.5, -0.5],
//         color: GREEN,
//     },
//     Vertex {
//         // vertex c
//         position: [0.5, 0.5],
//         color: BLUE,
//     },
//     Vertex {
//         position: [-0.75, 0.25],
//         color: Color([1.0, 1.0, 0.0]),
//     },
//     Vertex {
//         position: [0.75, -0.25],
//         color: GREEN,
//     },
//     Vertex {
//         position: [0.15, 0.95],
//         color: BLUE,
//     },
// ];

struct State {
    surface: wgpu::Surface,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
    size: winit::dpi::PhysicalSize<u32>,
    pipeline: wgpu::RenderPipeline,
    vertex_buffer_stuff: VertexBufferStuff,
    // vertex_buffer: wgpu::Buffer,
    // vertices: Vec<Vertex>,
}
struct OneOfThese {
    vertex_buffer: wgpu::Buffer,
    vertices: Vec<Vertex>,
}
impl OneOfThese {
    fn new(device: &wgpu::Device) -> Self {
        // let vertices = generate_vertices();
        let vertices = RectHolder::random().vertices;
        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: wgpu::BufferUsages::VERTEX,
        });
        Self {
            vertex_buffer,
            vertices,
        }
    }
}
struct VertexBufferStuff {
    one: OneOfThese,
    two: OneOfThese,
    idx: usize,
}
impl VertexBufferStuff {
    fn new(device: &wgpu::Device) -> Self {
        let one = OneOfThese::new(device);
        let two = OneOfThese::new(device);
        Self { one, two, idx: 0 }
    }
    fn current(&self) -> &OneOfThese {
        match self.idx {
            0 => &self.one,
            1 => &self.two,
            _ => unreachable!(),
        }
    }
    fn swap(&mut self) {
        self.idx = (self.idx + 1) % 2;
    }
}

impl State {
    async fn new(window: &Window) -> Self {
        let size = window.inner_size();
        let backends = wgpu::util::backend_bits_from_env().unwrap_or_else(wgpu::Backends::all);
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends,
            dx12_shader_compiler: wgpu::Dx12Compiler::default(),
        });
        let surface = unsafe { instance.create_surface(window) }.unwrap();
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                compatible_surface: Some(&surface),
                force_fallback_adapter: false,
            })
            .await
            .unwrap();

        let (device, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    features: wgpu::Features::empty(),
                    limits: wgpu::Limits::default(),
                },
                None, // Trace path
            )
            .await
            .unwrap();
        let swapchain_capabilities = surface.get_capabilities(&adapter);
        let swapchain_format = swapchain_capabilities.formats[0];

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: swapchain_format,
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: Vec::new(),
        };
        surface.configure(&device, &config);

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("shader.wgsl").into()),
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Render Pipeline Layout"),
            bind_group_layouts: &[],
            push_constant_ranges: &[],
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Render Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[Vertex::desc()],
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(wgpu::ColorTargetState {
                    format: config.format,
                    blend: Some(wgpu::BlendState {
                        color: wgpu::BlendComponent::OVER,
                        alpha: wgpu::BlendComponent::OVER,
                    }),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                strip_index_format: None,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
        });

        // let vertices = generate_vertices();
        // let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        //     label: Some("Vertex Buffer"),
        //     contents: bytemuck::cast_slice(&vertices),
        //     usage: wgpu::BufferUsages::VERTEX,
        // });
        let vertex_buffer_stuff = VertexBufferStuff::new(&device);

        Self {
            surface,
            device,
            queue,
            config,
            size,
            pipeline,
            vertex_buffer_stuff,
            // vertex_buffer,
            // vertices
        }
    }

    pub fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.size = new_size;
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
        }
    }

    #[allow(unused_variables)]
    fn input(&mut self, event: &WindowEvent) -> bool {
        false
    }

    fn update(&mut self) {}

    fn render(&mut self) -> Result<(), wgpu::SurfaceError> {
        //let output = self.surface.get_current_frame()?.output;
        // self.vertex_buffer_stuff = VertexBufferStuff::new(&self.device);
        // self.vertices = generate_vertices();
        // self.vertex_buffer = self.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        //     label: Some("Vertex Buffer"),
        //     contents: bytemuck::cast_slice(&self.vertices),
        //     usage: wgpu::BufferUsages::VERTEX,
        // });

        self.vertex_buffer_stuff.swap();

        let output = self.surface.get_current_texture()?;
        let view = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Render Encoder"),
            });

        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Render Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.2,
                            g: 0.247,
                            b: 0.314,
                            a: 1.0,
                        }),
                        store: true,
                    },
                })],
                depth_stencil_attachment: None,
            });

            render_pass.set_pipeline(&self.pipeline);
            render_pass.set_vertex_buffer(0, self.vertex_buffer().slice(..));
            render_pass.draw(0..NUM_VERTICES as u32, 0..1);
        }

        self.queue.submit(iter::once(encoder.finish()));
        output.present();

        Ok(())
    }
    fn vertex_buffer(&self) -> &wgpu::Buffer {
        &self.vertex_buffer_stuff.current().vertex_buffer
    }
}

pub fn run() {
    env_logger::init();
    let something: layout21protos::Library =
        proto_converters::open(&resource("sky130_fd_sc_hd__dfxtp_1.pb")).unwrap();
    let event_loop = EventLoop::new();
    let window = WindowBuilder::new().build(&event_loop).unwrap();
    window.set_title(&*format!("{}", "Layout21 Viewer"));
    let mut state = pollster::block_on(State::new(&window));

    error!("START!!!");

    event_loop.run(move |event, _, control_flow| match event {
        Event::WindowEvent {
            ref event,
            window_id,
        } if window_id == window.id() => {
            if !state.input(event) {
                match event {
                    WindowEvent::CloseRequested
                    | WindowEvent::KeyboardInput {
                        input:
                            KeyboardInput {
                                state: ElementState::Pressed,
                                virtual_keycode: Some(VirtualKeyCode::Escape),
                                ..
                            },
                        ..
                    } => *control_flow = ControlFlow::Exit,
                    WindowEvent::Resized(physical_size) => {
                        state.resize(*physical_size);
                        window.request_redraw();
                    }
                    WindowEvent::ScaleFactorChanged { new_inner_size, .. } => {
                        state.resize(**new_inner_size);
                    }
                    _ => {}
                }
            }
        }
        Event::RedrawRequested(_) => {
            error!("REDRAW!!!");
            state.update();
            match state.render() {
                Ok(_) => {}
                Err(wgpu::SurfaceError::Lost) => state.resize(state.size),
                Err(wgpu::SurfaceError::OutOfMemory) => *control_flow = ControlFlow::Exit,
                Err(e) => eprintln!("{:?}", e),
            }
        }
        Event::MainEventsCleared => {
            error!("REQUESTING!!!");
            window.request_redraw();
        }
        _ => {}
    });
}

/// Grab the full path of resource-file `fname`
fn resource(rname: &str) -> String {
    format!(
        "{}/../layout21converters/resources/{}",
        env!("CARGO_MANIFEST_DIR"),
        rname
    )
}
