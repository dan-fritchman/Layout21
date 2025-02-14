use log::error;
use winit::{
    event::*,
    event_loop::{ControlFlow, EventLoop},
    window::{Window, WindowBuilder},
};

// Local imports
use crate::{tessellate, GpuStuff, LayoutDisplay, Size, Buffers};

/// # Application State
pub struct State {
    gpu: GpuStuff,
    layout: LayoutDisplay,
    buffers: Buffers,
}
impl State {
    async fn new(window: &Window) -> Self {
        let layout = LayoutDisplay::from_proto();
        let size = window.inner_size();
        let size: Size<u32> = Size {
            width: size.width,
            height: size.height,
        };
        let buffers = tessellate(&layout, &size);
        let gpu = GpuStuff::new(window, &buffers).await;
        Self {
            gpu,
            layout,
            buffers,
        }
    }
}

#[allow(unused_variables)]
fn handle_input(event: &WindowEvent) -> bool {
    false
}

pub fn run() {
    env_logger::init();

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
            if !handle_input(event) {
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
                        state.gpu.resize(*physical_size);
                        window.request_redraw();
                    }
                    WindowEvent::ScaleFactorChanged { new_inner_size, .. } => {
                        state.gpu.resize(**new_inner_size);
                    }
                    _ => {}
                }
            }
        }
        Event::RedrawRequested(_) => {
            error!("REDRAW!!!");
            match state.gpu.render(&state.buffers) {
                Ok(_) => {}
                Err(wgpu::SurfaceError::Lost) => state.gpu.resize(state.gpu.size),
                Err(wgpu::SurfaceError::OutOfMemory) => *control_flow = ControlFlow::Exit,
                Err(e) => eprintln!("{:?}", e),
            }
        }
        Event::MainEventsCleared => {
            // error!("REQUESTING!!!");
            // window.request_redraw();
        }
        _ => {}
    });
}
