use bytemuck::{Pod, Zeroable};

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct Color(pub [f32; 3]);

pub const COLORS: [Color; 7] = [
    Color([1.0, 0.0, 0.0]), // red
    Color([0.0, 1.0, 0.0]), // green
    Color([0.0, 0.0, 1.0]), // blue
    Color([1.0, 1.0, 0.0]), //
    Color([1.0, 0.0, 1.0]), //
    Color([0.0, 1.0, 1.0]), //
    Color([1.0, 1.0, 1.0]), // white
];
#[derive(Debug)]
pub(crate) struct ColorWheel {
    index: usize,
}
impl ColorWheel {
    pub fn new() -> Self {
        Self { index: 0 }
    }
    pub fn next(&mut self) -> Color {
        let color = COLORS[self.index];
        self.index = (self.index + 1) % COLORS.len();
        color
    }
}
