//!
//! # Layout21 "Tetris" Semi-Custom Layout System
//!

// External macro usages
#[macro_use]
extern crate derive_builder;

// Modules
pub mod abs;
pub mod bbox;
pub mod cell;
pub mod conv;
pub mod coords;
pub mod instance;
pub mod interface;
pub mod layout;
pub mod library;
pub mod outline;
pub mod placement;
pub mod placer;
pub mod stack;
pub mod tracks;
pub mod validate;

// Re-exports
pub use layout21raw as raw;
pub use layout21utils as utils;

/// Unit Tests Module
#[cfg(test)]
mod tests;
