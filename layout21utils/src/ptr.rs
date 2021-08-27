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
/// Internal type-alias for cell and library pointers.
///
/// All are thread-safe and reference-counted "smart pointers".
/// While none of the code *using* them is threaded (yet, maybe ever),
/// we'll see how much heartache this induces in both design and performance.
///
/// Attribute access is largely forwarded through [Deref] calls,
/// allowing for fairly natural syntax after grabbing `read()` or `write()` access.
/// For examaple:
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
#[derive(Clone, Debug, Default)]
pub struct Ptr<T>(ByAddress<Arc<RwLock<T>>>);

impl<T> Ptr<T> {
    /// Pointer Constructor
    pub fn new(i: T) -> Self {
        Self(ByAddress(Arc::new(RwLock::new(i))))
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
// The `derive`d implementations for `PartialEq`, `Eq`, and `Hash`
// presumably look very much like this, but don't compile
// for reasons that remain mysterious here.
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
/// Newtype wrapping a <Vec<Ptr>>, adding an interface designed
/// for ease of getting referable [Ptr]s upon insertion.
/// Other methods are passed into the underlying [Vec] via [Deref] and [DerefMut].
///
#[derive(Debug, Clone, Default)]
pub struct PtrList<T: Clone>(Vec<Ptr<T>>);
impl<T: Clone> PtrList<T> {
    /// Add an owned [T], returning a [Ptr] to it
    pub fn add(&mut self, val: T) -> Ptr<T> {
        let rv = Ptr::new(val);
        self.0.push(Ptr::clone(&rv));
        rv
    }
    /// Alias for `add`
    pub fn insert(&mut self, val: T) -> Ptr<T> {
        self.add(val)
    }
}
impl<T: Clone> Deref for PtrList<T> {
    type Target = Vec<Ptr<T>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T: Clone> DerefMut for PtrList<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

