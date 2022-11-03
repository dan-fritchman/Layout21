//!
//! # Raw Layout
//!
//! The most general and lowest-level representation layer in layout21.
//! Consists of geometric primitives and instances of other layout cells,
//! much akin to nearly any legacy layout system.
//!

// Crates.io dependencies, at crate-level for their macros
#[macro_use]
extern crate enum_dispatch;

// Internal modules
pub mod align;
pub mod bbox;
pub mod data;
pub mod error;
pub mod geom;
pub mod translate;

// Re-exports
#[doc(inline)]
pub use bbox::*;
#[doc(inline)]
pub use data::*;
#[doc(inline)]
pub use error::*;
#[doc(inline)]
pub use geom::*;
pub use layout21utils as utils;

// Optional-feature modules
#[cfg(feature = "gds")]
pub mod gds;
#[cfg(feature = "lef")]
pub mod lef;
#[cfg(feature = "proto")]
pub mod proto;

// Unit tests
#[cfg(test)]
mod tests;
