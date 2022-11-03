use crate::{Point, Rect};

pub trait Translate {
    fn translate(&mut self, v: Point);
}

impl Translate for Point {
    fn translate(&mut self, v: Point) {
        self.x += v.x;
        self.y += v.y;
    }
}

impl Translate for Rect {
    fn translate(&mut self, v: Point) {
        self.p0.translate(v);
        self.p1.translate(v);
    }
}
