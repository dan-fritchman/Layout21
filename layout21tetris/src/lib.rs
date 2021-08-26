//!
//! # Layout21 "Tetris" Semi-Custom Layout System
//!

use by_address::ByAddress;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, LockResult, RwLock, RwLockReadGuard};

/// Internal type-alias for cell and library pointers.
#[derive(Clone, Debug, Default)]
pub struct Ptr<T>(ByAddress<Arc<RwLock<T>>>);
// All are thread-safe and reference-counted "smart pointers".
// While none of the code *using* them is threaded (yet, maybe ever),
// we'll see how much heartache this induces in both design and performance.
//
// [Ptr] also uses the [ByAddress] struct to allow for comparisons
// and hashes *by address* (i.e. pointer value).
impl<T> Ptr<T> {
    /// Pointer Constructor
    pub fn new(i: T) -> Self {
        Self(ByAddress(Arc::new(RwLock::new(i))))
    }
    /// Read the underlying data
    /// Typical usage includes a follow-up call to [std::borrow::Borrow::borrow],
    /// to get a reference to the underlying `T`. Example:
    /// ```ignore
    /// use std::borrow::Borrow;      // Trait must be in-scope for the `borrow` method to work
    /// { // Guard & borrow scope
    ///     let guard = ptr.read()?;  // `?` unwraps the [LockResult]
    ///     let t = guard.borrow();   // Borrow the underlying [T] data
    ///     some_function(t);         // Call `some_function` with the borrowed data
    /// } // Guard & borrow dropped here
    /// ```
    /// Note `guard` in the example above generally requires a dedecated `let` binding,
    /// lest it be dropped mid-line in something like so:
    /// ```ignore
    /// let t = ptr.read()?.borrow();  // Fails, guard dropped immediately
    /// ```
    ///
    pub fn read(&self) -> LockResult<RwLockReadGuard<T>> {
        self.0.read()
    }
}
impl<T> PartialEq for Ptr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T> Eq for Ptr<T> {}
impl<T> Hash for Ptr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

// Modules
pub mod abstrakt;
pub mod cell;
pub mod coords;
pub mod interface;
pub mod library;
pub mod outline;
pub mod rawconv;
pub mod stack;
pub mod validate;

// Re-exports
pub use layout21raw as raw;
pub use layout21utils as utils;

/// Unit Tests Module
#[cfg(test)]
mod tests;
