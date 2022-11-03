use crate::{snap_to_grid, translate::Translate, BoundBox, BoundBoxTrait, Int, Point};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AlignMode {
    Left,
    Right,
    Bottom,
    Top,
    CenterHorizontal,
    CenterVertical,
    ToTheRight,
    ToTheLeft,
    Beneath,
    Above,
}

pub trait AlignRect: Translate + BoundBoxTrait {
    fn align(&mut self, mode: AlignMode, obox: BoundBox, space: Int) -> &mut Self {
        let sbox = self.bbox();

        match mode {
            AlignMode::Left => {
                self.translate(Point::new(obox.p0.x - sbox.p0.x + space, 0));
            }
            AlignMode::Right => {
                self.translate(Point::new(obox.p1.x - sbox.p1.x + space, 0));
            }
            AlignMode::Bottom => {
                self.translate(Point::new(0, obox.p0.y - sbox.p0.y + space));
            }
            AlignMode::Top => {
                self.translate(Point::new(0, obox.p1.y - sbox.p1.y + space));
            }
            AlignMode::ToTheRight => {
                self.translate(Point::new(obox.p1.x - sbox.p0.x + space, 0));
            }
            AlignMode::ToTheLeft => {
                self.translate(Point::new(obox.p0.x - sbox.p1.x - space, 0));
            }
            AlignMode::CenterHorizontal => {
                self.translate(Point::new(
                    ((obox.p0.x + obox.p1.x) - (sbox.p0.x + sbox.p1.x)) / 2 + space,
                    0,
                ));
            }
            AlignMode::CenterVertical => {
                self.translate(Point::new(
                    0,
                    ((obox.p0.y + obox.p1.y) - (sbox.p0.y + sbox.p1.y)) / 2 + space,
                ));
            }
            AlignMode::Beneath => {
                self.translate(Point::new(0, obox.p0.y - sbox.p1.y - space));
            }
            AlignMode::Above => {
                self.translate(Point::new(0, obox.p1.y - sbox.p0.y + space));
            }
        }

        self
    }

    fn align_left(&mut self, other: BoundBox) {
        self.align(AlignMode::Left, other, 0);
    }

    fn align_right(&mut self, other: BoundBox) {
        self.align(AlignMode::Right, other, 0);
    }

    fn align_bottom(&mut self, other: BoundBox) {
        self.align(AlignMode::Bottom, other, 0);
    }

    fn align_top(&mut self, other: BoundBox) {
        self.align(AlignMode::Top, other, 0);
    }

    fn align_to_the_right_of(&mut self, other: BoundBox, space: Int) {
        self.align(AlignMode::ToTheRight, other, space);
    }

    fn align_to_the_left_of(&mut self, other: BoundBox, space: Int) {
        self.align(AlignMode::ToTheLeft, other, space);
    }

    fn align_centers_horizontally(&mut self, other: BoundBox) {
        self.align(AlignMode::CenterHorizontal, other, 0);
    }

    fn align_centers_vertically(&mut self, other: BoundBox) {
        self.align(AlignMode::CenterVertical, other, 0);
    }

    fn align_centers(&mut self, other: BoundBox) {
        self.align_centers_vertically(other);
        self.align_centers_vertically(other);
    }

    fn align_beneath(&mut self, other: BoundBox, space: Int) {
        self.align(AlignMode::Beneath, other, space);
    }

    fn align_above(&mut self, other: BoundBox, space: Int) {
        self.align(AlignMode::Above, other, space);
    }

    fn align_centers_horizontally_gridded(&mut self, other: BoundBox, grid: Int) {
        // Align the center
        self.align(AlignMode::CenterHorizontal, other, 0);

        // Then snap to the nearest grid location
        let bbox = self.bbox();
        assert_eq!(bbox.width() % grid, 0);
        let offset = snap_to_grid(bbox.p0.x, grid) - bbox.p0.x;
        self.translate(Point::new(offset, 0));
    }

    fn align_centers_vertically_gridded(&mut self, other: BoundBox, grid: Int) {
        // Align the center
        self.align(AlignMode::CenterVertical, other, 0);

        // Then snap to the nearest grid location
        let bbox = self.bbox();
        assert_eq!(bbox.height() % grid, 0);
        let offset = snap_to_grid(bbox.p0.y, grid) - bbox.p0.y;
        self.translate(Point::new(0, offset));
    }

    fn align_centers_gridded(&mut self, other: BoundBox, grid: Int) {
        self.align_centers_horizontally_gridded(other, grid);
        self.align_centers_vertically_gridded(other, grid);
    }
}
