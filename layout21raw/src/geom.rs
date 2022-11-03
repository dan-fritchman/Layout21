//!
//! # Geometry Module
//!
//! Defines the core geometric types including [Point], [Shape], and [Transform],
//! and their core operations.
//!

// Std-Lib
use std::{collections::HashMap, convert::TryFrom, fmt::Display};

// Crates.io
use serde::{Deserialize, Serialize};

// Local imports
use crate::{align::AlignRect, bbox::BoundBoxTrait, AbstractPort, BoundBox, Int};

/// # Point in two-dimensional layout-space
#[derive(Debug, Copy, Clone, Default, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Point {
    pub x: Int,
    pub y: Int,
}
impl Point {
    /// Create a new [Point] from (x,y) coordinates
    pub fn new(x: Int, y: Int) -> Self {
        Self { x, y }
    }
    /// The origin, (0, 0).
    pub fn zero() -> Self {
        Self { x: 0, y: 0 }
    }
    /// Create a new [Point] which serves as an offset in direction `dir`
    pub fn offset(val: Int, dir: Dir) -> Self {
        match dir {
            Dir::Horiz => Self { x: val, y: 0 },
            Dir::Vert => Self { x: 0, y: val },
        }
    }
    /// Create a new point shifted by `x` in the x-dimension and by `y` in the y-dimension
    pub fn shift(&self, p: &Point) -> Point {
        Point {
            x: p.x + self.x,
            y: p.y + self.y,
        }
    }
    /// Create a new point scaled by `p.x` in the x-dimension and by `p.y` in the y-dimension
    pub fn scale(&self, p: &Point) -> Point {
        Point {
            x: p.x * self.x,
            y: p.y * self.y,
        }
    }
    /// Get the coordinate associated with direction `dir`
    pub fn coord(&self, dir: Dir) -> Int {
        match dir {
            Dir::Horiz => self.x,
            Dir::Vert => self.y,
        }
    }
    /// Create a ne [Point], transformed from our original location by `transform`
    /// Coordinate transforms are applied in floating-point format,
    /// largely for rotations, and then rounded to the nearest integer.
    pub fn transform(&self, trans: &Transform) -> Point {
        let xf = self.x as f64;
        let yf = self.y as f64;
        let x = trans.a[0][0] * xf + trans.a[0][1] * yf + trans.b[0];
        let y = trans.a[1][0] * xf + trans.a[1][1] * yf + trans.b[1];
        Self {
            x: x.round() as Int,
            y: y.round() as Int,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, Serialize, Deserialize, PartialEq, Eq)]
pub struct Span {
    start: Int,
    stop: Int,
}

/// Snaps `pos` to the nearest multiple of `grid`.
pub fn snap_to_grid(pos: Int, grid: Int) -> Int {
    assert!(grid > 0);

    let rem = pos.rem_euclid(grid);
    assert!(rem >= 0);
    assert!(rem < grid);
    if rem <= grid / 2 {
        pos - rem
    } else {
        pos + grid - rem
    }
}

impl Span {
    pub fn new(start: Int, stop: Int) -> Self {
        use std::cmp::{max, min};
        let lower = min(start, stop);
        let upper = max(start, stop);
        Self {
            start: lower,
            stop: upper,
        }
    }

    pub fn expand(&mut self, pos: bool, amount: Int) -> &mut Self {
        if pos {
            self.stop += amount;
        } else {
            self.start -= amount;
        }
        self
    }

    pub fn edge(&self, pos: bool) -> Int {
        if pos {
            self.stop
        } else {
            self.start
        }
    }

    pub fn from_center_span(center: Int, span: Int) -> Self {
        assert!(span >= 0);
        assert_eq!(span % 2, 0);

        Self::new(center - (span / 2), center + (span / 2))
    }

    pub fn from_center_span_gridded(center: Int, span: Int, grid: Int) -> Self {
        assert!(span >= 0);
        assert_eq!(span % 2, 0);
        assert_eq!(span % grid, 0);

        let start = snap_to_grid(center - (span / 2), grid);

        Self::new(start, start + span)
    }

    #[inline]
    pub fn center(&self) -> Int {
        (self.start + self.stop) / 2
    }

    #[inline]
    pub fn intersects(&self, other: &Self) -> bool {
        !(other.stop < self.start || self.stop < other.start)
    }

    #[inline]
    pub fn length(&self) -> Int {
        self.stop - self.start
    }

    #[inline]
    pub fn start(&self) -> Int {
        self.start
    }

    #[inline]
    pub fn stop(&self) -> Int {
        self.stop
    }

    pub fn merge(spans: impl IntoIterator<Item = Self>) -> Self {
        use std::cmp::{max, min};
        let mut spans = spans.into_iter();
        let (mut start, mut stop) = spans
            .next()
            .expect("Span::merge requires at least one span")
            .into();

        for span in spans {
            start = min(start, span.start);
            stop = max(stop, span.stop);
        }

        debug_assert!(start <= stop);

        Span { start, stop }
    }
}

impl From<(Int, Int)> for Span {
    #[inline]
    fn from(tup: (Int, Int)) -> Self {
        Self::new(tup.0, tup.1)
    }
}

impl From<Span> for (Int, Int) {
    #[inline]
    fn from(s: Span) -> Self {
        (s.start(), s.stop())
    }
}

/// Direction Enumeration
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum Dir {
    /// Horizontal.
    Horiz,
    /// Vertical.
    Vert,
}

impl Dir {
    /// Whichever direction we are, return the other one.
    pub fn other(self) -> Self {
        match self {
            Self::Horiz => Self::Vert,
            Self::Vert => Self::Horiz,
        }
    }
    pub fn short_form(&self) -> &'static str {
        match *self {
            Self::Horiz => "h",
            Self::Vert => "v",
        }
    }
}

impl Default for Dir {
    #[inline]
    fn default() -> Self {
        Self::Horiz
    }
}

impl Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Horiz => write!(f, "horizontal"),
            Self::Vert => write!(f, "vertical"),
        }
    }
}

impl std::ops::Not for Dir {
    type Output = Self;
    /// Exclamation Operator returns the opposite direction
    fn not(self) -> Self::Output {
        self.other()
    }
}

/// # Path
///
/// Open-ended geometric path with non-zero width.
/// Primarily consists of a series of ordered [Point]s.
///
#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Path {
    pub points: Vec<Point>,
    pub width: usize,
}
/// # Polygon
///
/// Closed n-sided polygon with arbitrary number of vertices.
/// Primarily consists of a series of ordered [Point]s.
///
/// Closure from the last point back to the first is implied;
/// the initial point need not be repeated at the end.
///
#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Polygon {
    pub points: Vec<Point>,
}
/// # Rectangle
///
/// Axis-aligned rectangle, specified by two opposite corners.
///
#[derive(Debug, Default, Copy, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Rect {
    pub p0: Point,
    pub p1: Point,
}
impl Rect {
    /// Calculate our center-point
    pub fn center(&self) -> Point {
        Point::new((self.p0.x + self.p1.x) / 2, (self.p0.y + self.p1.y) / 2)
    }

    pub fn new(p0: Point, p1: Point) -> Self {
        Self { p0, p1 }
    }

    pub fn from_spans(h: Span, v: Span) -> Self {
        Self {
            p0: Point::new(h.start(), v.start()),
            p1: Point::new(h.stop(), v.stop()),
        }
    }

    #[inline]
    pub fn bottom(&self) -> Int {
        self.p0.y
    }
    #[inline]
    pub fn top(&self) -> Int {
        self.p1.y
    }
    #[inline]
    pub fn left(&self) -> Int {
        self.p0.x
    }
    #[inline]
    pub fn right(&self) -> Int {
        self.p1.x
    }

    pub fn hspan(&self) -> Span {
        Span::new(self.p0.x, self.p1.x)
    }

    pub fn vspan(&self) -> Span {
        Span::new(self.p0.y, self.p1.y)
    }

    #[inline]
    pub fn width(&self) -> Int {
        self.hspan().length()
    }

    #[inline]
    pub fn area(&self) -> Int {
        self.width() * self.height()
    }

    pub fn lower_edge(&self, dir: Dir) -> Int {
        self.span(dir).start()
    }

    pub fn upper_edge(&self, dir: Dir) -> Int {
        self.span(dir).stop()
    }

    pub fn span(&self, dir: Dir) -> Span {
        match dir {
            Dir::Horiz => self.hspan(),
            Dir::Vert => self.vspan(),
        }
    }

    fn sorted_edges(&self, other: &Self, dir: Dir) -> [Int; 4] {
        let mut edges = [
            self.lower_edge(dir),
            self.upper_edge(dir),
            other.lower_edge(dir),
            other.upper_edge(dir),
        ];
        edges.sort();
        edges
    }

    #[inline]
    pub fn inner_span(&self, other: &Self, dir: Dir) -> Span {
        let edges = self.sorted_edges(other, dir);
        Span::new(edges[1], edges[2])
    }

    #[inline]
    pub fn outer_span(&self, other: &Self, dir: Dir) -> Span {
        let edges = self.sorted_edges(other, dir);
        Span::new(edges[0], edges[3])
    }

    pub fn edge_closer_to(&self, x: Int, dir: Dir) -> Int {
        let (x0, x1) = self.span(dir).into();
        if (x - x0).abs() <= (x - x1).abs() {
            x0
        } else {
            x1
        }
    }

    pub fn edge_farther_from(&self, x: Int, dir: Dir) -> Int {
        let (x0, x1) = self.span(dir).into();
        if (x - x0).abs() <= (x - x1).abs() {
            x1
        } else {
            x0
        }
    }

    #[inline]
    pub fn span_builder() -> RectSpanBuilder {
        RectSpanBuilder::new()
    }

    #[inline]
    pub fn height(&self) -> Int {
        self.vspan().length()
    }

    #[inline]
    pub fn longer_dir(&self) -> Dir {
        if self.width() > self.height() {
            Dir::Horiz
        } else {
            Dir::Vert
        }
    }

    #[inline]
    pub fn shorter_dir(&self) -> Dir {
        !self.longer_dir()
    }

    #[inline]
    pub fn expand(&self, amount: Int) -> Self {
        Self::new(
            Point::new(self.p0.x - amount, self.p0.y - amount),
            Point::new(self.p1.x + amount, self.p1.y + amount),
        )
    }

    #[inline]
    pub fn expand_dir(&self, dir: Dir, amount: Int) -> Self {
        match dir {
            Dir::Horiz => Self::new(
                Point::new(self.p0.x - amount, self.p0.y),
                Point::new(self.p1.x + amount, self.p1.y),
            ),
            Dir::Vert => Self::new(
                Point::new(self.p0.x, self.p0.y - amount),
                Point::new(self.p1.x, self.p1.y + amount),
            ),
        }
    }
}

impl AlignRect for Rect {}

/// A helper struct for building [`Rect`]s from [`Span`]s.
#[derive(Clone, Default, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct RectSpanBuilder {
    hspan: Option<Span>,
    vspan: Option<Span>,
}

impl RectSpanBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with(&mut self, dir: Dir, span: Span) -> &mut Self {
        match dir {
            Dir::Horiz => self.hspan = Some(span),
            Dir::Vert => self.vspan = Some(span),
        }
        self
    }

    /// Builds a Rect from the specified spans. Panics if one or more directions
    /// were left unspecified.
    pub fn build(&self) -> Rect {
        Rect::from_spans(self.hspan.unwrap(), self.vspan.unwrap())
    }
}

impl From<BoundBox> for Rect {
    fn from(r: BoundBox) -> Self {
        debug_assert!(!r.is_empty());
        debug_assert!(r.p0.x <= r.p1.x);
        debug_assert!(r.p0.y <= r.p1.y);
        Self { p0: r.p0, p1: r.p1 }
    }
}

/// # Shape
///
/// The primary geometric primitive comprising raw layout.
/// Variants include [Rect], [Polygon], and [Path].
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[enum_dispatch(ShapeTrait)]
pub enum Shape {
    Rect(Rect),
    Polygon(Polygon),
    Path(Path),
    Point(Point),
}

impl Default for Shape {
    fn default() -> Self {
        Self::Rect(Rect::default())
    }
}

impl Shape {
    /// Boolean indication of whether we intersect with [Shape] `other`.
    pub fn intersects(&self, _other: &Shape) -> bool {
        todo!() // FIXME!
    }
}

/// # ShapeTrait
///
/// Common shape operations, dispatched from the [Shape] enum to its variants by [enum_dispatch].
///
#[enum_dispatch]
pub trait ShapeTrait {
    /// Retrieve our "origin", or first [Point]
    fn point0(&self) -> &Point;
    /// Indicate whether this shape is (more or less) horizontal or vertical.
    /// Primarily used for orienting label-text.
    fn orientation(&self) -> Dir;
    /// Shift coordinates by the (x,y) values specified in `pt`
    fn shift(&mut self, pt: &Point);
    /// Boolean indication of whether the [Shape] contains [Point] `pt`.
    /// Containment is *inclusive* for all [Shape] types.
    /// [Point]s on their boundary, which generally include all points specifying the shape itself, are regarded throughout as "inside" the shape.
    fn contains(&self, pt: &Point) -> bool;
    /// Convert to a [Polygon], our most general of shapes
    fn to_poly(&self) -> Polygon;
}

impl ShapeTrait for Rect {
    /// Retrieve our "origin", or first [Point]
    fn point0(&self) -> &Point {
        &self.p0
    }

    /// Indicate whether this shape is (more or less) horizontal or vertical.
    /// Primarily used for orienting label-text.
    fn orientation(&self) -> Dir {
        let (p0, p1) = (&self.p0, &self.p1);
        if (p1.x - p0.x).abs() < (p1.y - p0.y).abs() {
            return Dir::Vert;
        }
        Dir::Horiz
    }
    /// Shift coordinates by the (x,y) values specified in `pt`
    fn shift(&mut self, pt: &Point) {
        self.p0.x += pt.x;
        self.p0.y += pt.y;
        self.p1.x += pt.x;
        self.p1.y += pt.y;
    }
    /// Boolean indication of whether the [Shape] contains [Point] `pt`.
    /// Containment is *inclusive* for all [Shape] types.
    /// [Point]s on their boundary, which generally include all points specifying the shape itself, are regarded throughout as "inside" the shape.
    fn contains(&self, pt: &Point) -> bool {
        let (p0, p1) = (&self.p0, &self.p1);
        p0.x.min(p1.x) <= pt.x
            && p0.x.max(p1.x) >= pt.x
            && p0.y.min(p1.y) <= pt.y
            && p0.y.max(p1.y) >= pt.y
    }
    fn to_poly(&self) -> Polygon {
        // Create a four-sided polygon, cloning our corners
        Polygon {
            points: vec![
                self.p0.clone(),
                Point::new(self.p1.x, self.p0.y),
                self.p1.clone(),
                Point::new(self.p0.x, self.p1.y),
            ],
        }
    }
}
impl ShapeTrait for Polygon {
    /// Retrieve our "origin", or first [Point]
    fn point0(&self) -> &Point {
        &self.points[0]
    }
    /// Indicate whether this shape is (more or less) horizontal or vertical.
    /// Primarily used for orienting label-text.
    fn orientation(&self) -> Dir {
        // FIXME: always horizontal, at least for now
        Dir::Horiz
    }
    /// Shift coordinates by the (x,y) values specified in `pt`
    fn shift(&mut self, pt: &Point) {
        for p in self.points.iter_mut() {
            p.x += pt.x;
            p.y += pt.y;
        }
    }
    /// Boolean indication of whether the [Shape] contains [Point] `pt`.
    /// Containment is *inclusive* for all [Shape] types.
    /// [Point]s on their boundary, which generally include all points specifying the shape itself, are regarded throughout as "inside" the shape.
    fn contains(&self, pt: &Point) -> bool {
        // First check for the fast way out: if the point is outside the bounding box, it can't be in the polygon.
        if !self.points.bbox().contains(pt) {
            return false;
        }

        // Not quite so lucky this time. Now do some real work. Using the "winding number" algorithm, which works for all (realistically useful) layout-polygons.
        let mut winding_num: isize = 0;
        for idx in 0..self.points.len() {
            // Grab the segment's start and end points.
            // Note these accesses go one past `points.len`, closing the polygon back at its first point.
            let (past, next) = (
                &self.points[idx],
                &self.points[(idx + 1) % self.points.len()],
            );

            // First check whether the point is anywhere in the y-range of this segment
            if past.y.min(next.y) <= pt.y && past.y.max(next.y) >= pt.y {
                // May have a hit here. Sort out whether the semi-infinite horizontal line at `y=pt.y` intersects the edge.
                if next.y == past.y {
                    // This is a horizontal segment, and we're on the same y-level as the point.
                    // If its x-coordinate also lies within range, no need for further checks, we've got a hit.
                    if past.x.min(next.x) <= pt.x && past.x.max(next.x) >= pt.x {
                        return true;
                    }
                    // Otherwise "hits" against these horizontal segments are not counted in `winding_num`.
                    // (FIXME: double-check this.)
                } else {
                    // This is a non-horizontal segment. Check for intersection.
                    let xsolve = (next.x - past.x) * (pt.y - past.y) / (next.y - past.y) + past.x;

                    if xsolve == pt.x {
                        // This segment runs straight through the point. No need to check further.
                        return true;
                    } else if xsolve > pt.x {
                        // We've got a hit on the semi-infinite horizontal line through `pt`.
                        // Either increment or decrement the winding number.
                        if next.y > past.y {
                            winding_num += 1;
                        } else {
                            winding_num -= 1;
                        }
                    }
                }
            }
        }
        // Trick is: if the winding number is non-zero, we're inside the polygon. And if it's zero, we're outside.
        winding_num != 0
    }
    fn to_poly(&self) -> Polygon {
        self.clone()
    }
}
impl ShapeTrait for Path {
    /// Retrieve our "origin", or first [Point]
    fn point0(&self) -> &Point {
        &self.points[0]
    }
    /// Indicate whether this shape is (more or less) horizontal or vertical.
    /// Primarily used for orienting label-text.
    fn orientation(&self) -> Dir {
        // FIXME: always horizontal, at least for now
        Dir::Horiz
    }
    /// Shift coordinates by the (x,y) values specified in `pt`
    fn shift(&mut self, pt: &Point) {
        for p in self.points.iter_mut() {
            p.x += pt.x;
            p.y += pt.y;
        }
    }
    /// Boolean indication of whether the [Shape] contains [Point] `pt`.
    /// Containment is *inclusive* for all [Shape] types.
    /// [Point]s on their boundary, which generally include all points specifying the shape itself, are regarded throughout as "inside" the shape.
    fn contains(&self, pt: &Point) -> bool {
        // Break into segments, and check for intersection with each
        // Probably not the most efficient way to do this, but a start.
        // Only "Manhattan paths", i.e. those with segments solely running vertically or horizontally, are supported.
        // FIXME: even with this method, there are some small pieces at corners which we'll miss.
        // Whether these are relevant in real life, tbd.
        let (points, width) = (&self.points, self.width);
        let width = Int::try_from(width).unwrap(); // FIXME: probably store these signed, check them on creation
        for k in 0..points.len() - 1 {
            let rect = if points[k].x == points[k + 1].x {
                Rect {
                    p0: Point::new(points[k].x - width / 2, points[k].y),
                    p1: Point::new(points[k].x + width / 2, points[k + 1].y),
                }
            } else if points[k].y == points[k + 1].y {
                Rect {
                    p0: Point::new(points[k].x, points[k].y - width / 2),
                    p1: Point::new(points[k + 1].x, points[k].y + width / 2),
                }
            } else {
                unimplemented!("Unsupported Non-Manhattan Path")
            };
            if rect.contains(pt) {
                return true;
            }
        }
        false
    }
    fn to_poly(&self) -> Polygon {
        unimplemented!("Path::to_poly")
    }
}
impl ShapeTrait for Point {
    /// Retrieve our "origin", or first [Point]
    fn point0(&self) -> &Point {
        &self
    }
    /// Indicate whether this shape is (more or less) horizontal or vertical.
    /// Primarily used for orienting label-text.
    fn orientation(&self) -> Dir {
        // FIXME: always horizontal, at least for now
        Dir::Horiz
    }
    /// Shift coordinates by the (x,y) values specified in `pt`
    fn shift(&mut self, pt: &Point) {
        self.x += pt.x;
        self.y += pt.y;
    }
    /// Boolean indication of whether the [Shape] contains [Point] `pt`.
    /// Containment is *inclusive* for all [Shape] types.
    /// [Point]s on their boundary, which generally include all points specifying the shape itself, are regarded throughout as "inside" the shape.
    fn contains(&self, pt: &Point) -> bool {
        *pt == *self
    }
    fn to_poly(&self) -> Polygon {
        panic!("Cannot convert a Point to a Polygon")
    }
}

/// # Matrix-Vector Transformation
///
/// 2x2 rotation-matrix and two-entry translation vector,
/// used for relative movement of [Point]s and [Shape]s.
///
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Transform {
    /// Rotation / Transformation Matrix
    /// Represented in row-major order
    pub a: [[f64; 2]; 2],
    /// X-Y Translation
    pub b: [f64; 2],
}
impl Transform {
    /// The identity transform, leaving any transformed object unmodified
    pub fn identity() -> Self {
        Self {
            a: [[1., 0.], [0., 1.]],
            b: [0., 0.],
        }
    }
    /// Translation by (x,y)
    pub fn translate(x: f64, y: f64) -> Self {
        Self {
            a: [[1., 0.], [0., 1.]],
            b: [x, y],
        }
    }
    /// A transform to rotate by `angle` degrees
    pub fn rotate(angle: f64) -> Self {
        let sin = angle.to_radians().sin();
        let cos = angle.to_radians().cos();
        Self {
            a: [[cos, -sin], [sin, cos]],
            b: [0., 0.],
        }
    }
    /// A transform to reflect about the x-axis
    pub fn reflect_vert() -> Self {
        Self {
            a: [[1., 0.], [0., -1.]],
            b: [0., 0.],
        }
    }
    /// Create a transform from instance fields: location, rotation, and reflection
    pub fn from_instance(loc: &Point, reflect_vert: bool, angle: Option<f64>) -> Self {
        let b = [loc.x as f64, loc.y as f64];
        let (mut sin, mut cos) = (0., 1.);
        if let Some(angle) = angle {
            sin = angle.to_radians().sin();
            cos = angle.to_radians().cos();
        }
        let cos_refl = if reflect_vert { -cos } else { cos };
        let a = [[cos, -sin], [sin, cos_refl]];
        Self { a, b }
    }
    /// Create a new [Transform] that is the cascade of `parent` and `child`.
    ///
    /// "Parents" and "children" refer to typical layout-instance hierarchies,
    /// in which each layer of instance has a nested set of transformations relative to its top-level parent.
    ///
    /// Note this operation *is not* commutative.
    /// For example the set of transformations:
    /// * (a) Reflect vertically, then
    /// * (b) Translate by (1,1)
    /// * (c) Place a point at (local coordinate) (1,1)
    /// Lands said point at (2,-2) in top-level space,
    /// whereas reversing the order of (a) and (b) lands it at (2,0).
    ///
    pub fn cascade(parent: &Transform, child: &Transform) -> Transform {
        // The result-transform's origin is the parent's origin,
        // plus the parent-transformed child's origin
        let mut b = matvec(&parent.a, &child.b);
        b[0] += parent.b[0];
        b[1] += parent.b[1];
        // And the cascade-matrix is the product of the parent's and child's
        let a = matmul(&parent.a, &child.a);
        Self { a, b }
    }
}
/// Multiply 2x2 matrices, returning a new 2x2 matrix
fn matmul(a: &[[f64; 2]; 2], b: &[[f64; 2]; 2]) -> [[f64; 2]; 2] {
    [
        [
            a[0][0] * b[0][0] + a[0][1] * b[1][0],
            a[0][0] * b[0][1] + a[0][1] * b[1][1],
        ],
        [
            a[1][0] * b[0][0] + a[1][1] * b[1][0],
            a[1][0] * b[0][1] + a[1][1] * b[1][1],
        ],
    ]
}
/// Multiply a 2x2 matrix by a 2-entry vector, returning a new 2-entry vector
fn matvec(a: &[[f64; 2]; 2], b: &[f64; 2]) -> [f64; 2] {
    [
        a[0][0] * b[0] + a[0][1] * b[1],
        a[1][0] * b[0] + a[1][1] * b[1],
    ]
}
pub trait TransformTrait {
    /// Apply matrix-vector [Tranform] `trans`.
    /// Creates a new shape at a location equal to the transformation of our own.
    fn transform(&self, trans: &Transform) -> Self;
}
impl TransformTrait for Shape {
    /// Apply matrix-vector [Tranform] `trans`.
    /// Creates a new shape at a location equal to the transformation of our own.
    fn transform(&self, trans: &Transform) -> Self {
        match self {
            Shape::Rect(r) => Shape::Rect(r.transform(trans)),
            Shape::Polygon(p) => Shape::Polygon(p.transform(trans)),
            Shape::Path(p) => Shape::Path(p.transform(trans)),
            Shape::Point(p) => Shape::Point(p.transform(trans)),
        }
    }
}
impl TransformTrait for Rect {
    /// Apply matrix-vector [Tranform] `trans`.
    /// Creates a new shape at a location equal to the transformation of our own.
    fn transform(&self, trans: &Transform) -> Self {
        let (p0, p1) = (&self.p0, &self.p1);
        let p0p = p0.transform(trans);
        let p1p = p1.transform(trans);

        let p0 = Point::new(std::cmp::min(p0p.x, p1p.x), std::cmp::min(p0p.y, p1p.y));
        let p1 = Point::new(std::cmp::max(p0p.x, p1p.x), std::cmp::max(p0p.y, p1p.y));

        Rect { p0, p1 }
    }
}
impl TransformTrait for Polygon {
    /// Apply matrix-vector [Tranform] `trans`.
    /// Creates a new shape at a location equal to the transformation of our own.
    fn transform(&self, trans: &Transform) -> Self {
        Polygon {
            points: self.points.iter().map(|p| p.transform(trans)).collect(),
        }
    }
}
impl TransformTrait for Path {
    /// Apply matrix-vector [Tranform] `trans`.
    /// Creates a new shape at a location equal to the transformation of our own.
    fn transform(&self, trans: &Transform) -> Self {
        Path {
            points: self.points.iter().map(|p| p.transform(trans)).collect(),
            width: self.width,
        }
    }
}
impl TransformTrait for AbstractPort {
    /// Apply matrix-vector [Tranform] `trans`.
    /// Creates a new shape at a location equal to the transformation of our own.
    fn transform(&self, trans: &Transform) -> Self {
        let shapes = self
            .shapes
            .iter()
            .map(|(k, v)| {
                let v = v.iter().map(|s| s.transform(trans)).collect::<Vec<_>>();
                (k.clone(), v)
            })
            .collect::<HashMap<_, _>>();

        Self {
            net: self.net.clone(),
            shapes,
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    #[test]
    fn transform_identity() {
        let shape1 = Shape::Rect(Rect {
            p0: Point::new(0, 0),
            p1: Point::new(1, 1),
        });
        let trans = Transform::identity();
        let shape2 = shape1.transform(&trans);
        assert_eq!(shape2, shape1);
    }
    #[test]
    fn transform_rotate() {
        let shape1 = Shape::Rect(Rect {
            p0: Point::new(0, 0),
            p1: Point::new(1, 1),
        });
        let trans = Transform::rotate(90.);
        let shape2 = shape1.transform(&trans);
        assert_eq!(
            shape2,
            Shape::Rect(Rect {
                p0: Point::new(0, 0),
                p1: Point::new(-1, 1),
            })
        );
        let shape3 = shape2.transform(&trans);
        assert_eq!(
            shape3,
            Shape::Rect(Rect {
                p0: Point::new(0, 0),
                p1: Point::new(-1, -1),
            })
        );
        let shape4 = shape3.transform(&trans);
        assert_eq!(
            shape4,
            Shape::Rect(Rect {
                p0: Point::new(0, 0),
                p1: Point::new(1, -1),
            })
        );
        let shape0 = shape4.transform(&trans);
        assert_eq!(shape0, shape1);
    }
    #[test]
    fn test_cascade1() {
        let trans1 = Transform::reflect_vert();
        let trans2 = Transform::translate(1., 1.);

        let p = Point::new(1, 1);
        let cascade1 = Transform::cascade(&trans1, &trans2);
        let pc1 = p.transform(&cascade1);
        assert_eq!(pc1, Point::new(2, -2));

        let cascade2 = Transform::cascade(&trans2, &trans1);
        let pc1 = p.transform(&cascade2);
        assert_eq!(pc1, Point::new(2, 0));
    }
    #[test]
    fn test_polygon_contains() {
        // Test polygon-point containment of several flavors

        // Create a right triangle at the origin
        let triangle = Polygon {
            points: vec![Point::new(0, 0), Point::new(2, 0), Point::new(0, 2)],
        };
        assert!(triangle.contains(&Point::new(0, 0)));
        assert!(triangle.contains(&Point::new(1, 0)));
        assert!(triangle.contains(&Point::new(2, 0)));
        assert!(triangle.contains(&Point::new(0, 1)));
        assert!(triangle.contains(&Point::new(1, 1)));
        assert!(!triangle.contains(&Point::new(2, 2)));

        // Create a 2:1 tall-ish diamond-shape
        let diamond = Polygon {
            points: vec![
                Point::new(1, 0),
                Point::new(2, 2),
                Point::new(1, 4),
                Point::new(0, 2),
            ],
        };
        assert!(!diamond.contains(&Point::new(0, 0)));
        assert!(!diamond.contains(&Point::new(100, 100)));
        // Check a few points through its vertical center
        assert!(diamond.contains(&Point::new(1, 0)));
        assert!(diamond.contains(&Point::new(1, 1)));
        assert!(diamond.contains(&Point::new(1, 2)));
        assert!(diamond.contains(&Point::new(1, 3)));
        assert!(diamond.contains(&Point::new(1, 4)));
        // And its horizontal center
        assert!(diamond.contains(&Point::new(0, 2)));
        assert!(diamond.contains(&Point::new(1, 2)));
        assert!(diamond.contains(&Point::new(2, 2)));

        // More fun: create a U-shaped polygon, inside a 10x10 square
        let u = Polygon {
            points: vec![
                Point::new(0, 0),
                Point::new(0, 10),
                Point::new(2, 10),
                Point::new(2, 2),
                Point::new(8, 2),
                Point::new(8, 10),
                Point::new(10, 10),
                Point::new(10, 0),
            ],
        };
        for pt in &u.points {
            assert!(u.contains(pt));
        }
        assert!(u.contains(&Point::new(1, 1)));
        assert!(u.contains(&Point::new(1, 9)));
        assert!(u.contains(&Point::new(9, 9)));
        assert!(u.contains(&Point::new(9, 1)));
        // Points "inside" the u-part, i.e. "outside" the polygon
        assert!(!u.contains(&Point::new(3, 3)));
        assert!(!u.contains(&Point::new(3, 9)));
        assert!(!u.contains(&Point::new(7, 3)));
        assert!(!u.contains(&Point::new(7, 9)));
    }
}
