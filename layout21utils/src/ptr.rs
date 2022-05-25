//!
//! # Shared-Pointer Types
//!

// Std-lib
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock};

// Crates.io
use by_address::ByAddress;

///
/// # Ptr  
///
/// Internal type-alias for cell and library pointers.
/// All are thread-safe and reference-counted "smart pointers".  
///
/// Attribute access is largely forwarded through [Deref] calls,
/// allowing for fairly natural syntax after grabbing `read()` or `write()` access.
/// For example:
///
/// ```text
/// let data = ptr.read()?;
/// data.some_function();
/// let x = data.some_attribute;
/// ```
///
/// Every so often cases come up which aren't so clean, and require one of a few more-verbose work-arounds.
/// Calling methods with the borrowed data is a common example, which often does not work like so:
/// ```text
/// some_function(ptr.read()?);
/// ```
///
/// Two common work-arounds:
/// (a) Ref-and-Deref with `&*`, i.e.:
/// ```text
/// some_function(&*ptr.read()?); // Yes that generally works
/// ```
///
/// (b) include a follow-up call to [std::borrow::Borrow::borrow]
/// to get a reference to the underlying `T`. Example:
/// ```text
/// use std::borrow::Borrow;      // Trait must be in-scope for the `borrow` method to work
/// { // Guard & borrow scope
///     let guard = ptr.read()?;  // `?` unwraps the [LockResult]
///     let t = guard.borrow();   // Borrow the underlying [T] data
///     some_function(t);         // Call `some_function` with the borrowed data
/// } // Guard & borrow dropped here
/// ```
/// Note `guard` in the example above generally requires a dedecated `let` binding,
/// lest it be dropped mid-line in something like so:
/// ```text
/// let t = ptr.read()?.borrow();  // Fails, guard dropped immediately
/// ```
///
/// [Ptr] also uses the [ByAddress] struct to allow for comparisons
/// and hashes *by address* (i.e. pointer value).
/// Cell-pointers in particular are commonly used as hash-keys
/// in operations such as converting hierarchical trees,
/// in which many of the nodes are shared.
///
#[derive(Debug, Default)]
pub struct Ptr<T: ?Sized>(ByAddress<Arc<RwLock<T>>>);

impl<T> Ptr<T> {
    /// Pointer Constructor
    pub fn new(i: T) -> Self {
        Self(ByAddress(Arc::new(RwLock::new(i))))
    }
}
impl<T> From<T> for Ptr<T> {
    fn from(t: T) -> Self {
        Self::new(t)
    }
}
impl<T> Deref for Ptr<T> {
    type Target = ByAddress<Arc<RwLock<T>>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> DerefMut for Ptr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
// Having a [Deref] implementation seems to screw with the auto-`derive`d implementations
// of a few key traits. Conveniently, they're all quite short.
impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Self(ByAddress::clone(&self.0))
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
///
/// # Pointer List
///
/// Newtype wrapping a [Vec<Ptr>], adding an interface designed for ease of getting referable [Ptr]s upon insertion.
/// Other methods are passed into the underlying [Vec] via [Deref] and [DerefMut].
///
#[derive(Debug, Clone)]
pub struct PtrList<T: ?Sized>(Vec<Ptr<T>>);

impl<T> PtrList<T> {
    /// Create a new and empty [PtrList]. Also available via [Default].
    pub fn new() -> Self {
        Self(Vec::new())
    }
    /// Create a [PtrList] from a [Vec] of [Ptr]s.
    /// Also available via the [From]/[Into] traits.
    pub fn from_ptrs(ptrs: Vec<Ptr<T>>) -> Self {
        Self(ptrs)
    }
    /// Create a [PtrList] from owned `T`s.
    /// Also available via the [From]/[Into] traits.
    pub fn from_owned(vals: Vec<T>) -> Self {
        let ptrs = vals.into_iter().map(|v| Ptr::new(v)).collect();
        Self(ptrs)
    }
    /// Add a `T`-convertible element.
    /// Returns a cloned [Ptr] to it, which can be used to access it.
    pub fn add(&mut self, t: impl Into<T>) -> Ptr<T> {
        let t = Ptr::new(t.into()); // Convert if necessary
        self.0.push(t.clone()); // Add a clone to the list
        t // And return the "original" pointer
    }
    /// Alias for `add`
    pub fn insert(&mut self, t: impl Into<T>) -> Ptr<T> {
        self.add(t)
    }
}
impl<T> Default for PtrList<T> {
    fn default() -> Self {
        Self::new()
    }
}
// All dereferences, mostly method calls, are forwarded to the underlying [Vec]
impl<T> Deref for PtrList<T> {
    type Target = Vec<Ptr<T>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> DerefMut for PtrList<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
/// Create from a vector of `Ptr<T>` elements.
impl<T> From<Vec<Ptr<T>>> for PtrList<T> {
    fn from(v: Vec<Ptr<T>>) -> Self {
        Self::from_ptrs(v)
    }
}
/// Create from a vector of `T` elements.
impl<T> From<Vec<T>> for PtrList<T> {
    fn from(v: Vec<T>) -> Self {
        Self::from_owned(v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ptr() {
        let p1 = Ptr::new(43);
        let p2 = Ptr::new(43);
        assert_ne!(p1, p2);

        let p3 = p1.clone();
        assert_ne!(p3, p2);
        assert_eq!(p3, p1);
    }
    #[test]
    fn test_ptr_list() {
        let mut list = PtrList::<bool>::new();

        let p = list.add(true);
        assert_eq!(list.len(), 1);
        assert_eq!(*p.read().unwrap(), true);

        let p = list.insert(false);
        assert_eq!(list.len(), 2);
        assert_eq!(*p.read().unwrap(), false);

        list.push(Ptr::new(true));
        assert_eq!(list.len(), 3);
        assert_eq!(*list[2].read().unwrap(), true);
    }
}
