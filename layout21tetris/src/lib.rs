//!
//! # Layout21 "Tetris" Semi-Custom Layout System
//!

// Modules
pub mod abstrakt;
pub mod bbox;
pub mod cell;
pub mod coords;
pub mod interface;
pub mod library;
pub mod outline;
pub mod placement;
pub mod placer;
pub mod rawconv;
pub mod stack;
pub mod validate;

// Re-exports
pub use layout21raw as raw;
pub use layout21utils as utils;

/// Unit Tests Module
#[cfg(test)]
mod tests;
