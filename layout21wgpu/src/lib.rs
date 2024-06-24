//!
//! # Layout21 WGPU
//!

// Internal modules
mod color;
use crate::color::{Color, ColorWheel};

mod gpu;
use crate::gpu::GpuStuff;

mod vertex;
use crate::vertex::Vertex;

mod buffers;
use crate::buffers::Buffers;

mod layout;
use crate::layout::{tessellate, LayoutDisplay, Size};

// Primary public export: the run function
mod run;
pub use crate::run::run;
