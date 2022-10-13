//!
//! # Instance Structures
//!
//! Located, oriented instances of other cells or similar reusable layout objects.
//!

// Local imports
use crate::bbox::{BoundBox, HasBoundBox};
use crate::cell::Cell;
use crate::coords::{PrimPitches, Xy};
use crate::placement::Place;
use crate::raw::{Dir, LayoutError, LayoutResult};
use crate::utils::Ptr;

/// Instance of another Cell
#[derive(Debug, Clone)]
pub struct Instance {
    /// Instance Name
    pub inst_name: String,
    /// Cell Definition Reference
    pub cell: Ptr<Cell>,
    /// Location of the Instance origin
    /// This origin-position holds regardless of either `reflect` field.
    /// If specified in absolute coordinates, location-units are [PrimPitches].
    pub loc: Place<Xy<PrimPitches>>,
    /// Horizontal Reflection
    pub reflect_horiz: bool,
    /// Vertical Reflection
    pub reflect_vert: bool,
}
impl Instance {
    /// Boolean indication of whether this Instance is reflected in direction `dir`
    pub fn reflected(&self, dir: Dir) -> bool {
        match dir {
            Dir::Horiz => self.reflect_horiz,
            Dir::Vert => self.reflect_vert,
        }
    }
    /// Size of the Instance's rectangular `boundbox`, i.e. the zero-origin `boundbox` of its `cell`.
    pub fn boundbox_size(&self) -> LayoutResult<Xy<PrimPitches>> {
        let cell = self.cell.read()?;
        cell.boundbox_size()
    }
}
impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cell_name = {
            let cell = self.cell.read().unwrap();
            cell.name.clone()
        };
        write!(
            f,
            "Instance(name={}, cell={}, loc={:?})",
            self.inst_name, cell_name, self.loc
        )
    }
}
impl HasBoundBox for Instance {
    type Units = PrimPitches;
    type Error = LayoutError;
    /// Retrieve this Instance's bounding rectangle, specified in [PrimPitches].
    /// Instance location must be resolved to absolute coordinates, or this method will fail.
    fn boundbox(&self) -> LayoutResult<BoundBox<PrimPitches>> {
        let loc = self.loc.abs()?;
        let cell = self.cell.read()?;
        let outline = cell.outline()?;
        let (x0, x1) = match self.reflect_horiz {
            false => (loc.x, loc.x + outline.xmax()),
            true => (loc.x - outline.xmax(), loc.x),
        };
        let (y0, y1) = match self.reflect_vert {
            false => (loc.y, loc.y + outline.ymax()),
            true => (loc.y - outline.ymax(), loc.y),
        };
        Ok(BoundBox::new(Xy::new(x0, y0), Xy::new(x1, y1)))
    }
}
