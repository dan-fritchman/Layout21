//!
//! # Dependency-Ordering Trait and Helpers
//!

// Std-lib
use std::collections::HashSet;
use std::marker::PhantomData;

///
/// # Dependency-Ordering Trait
///
/// Many layout-types include a graph-like structure of dependencies between items.
/// Libraries in which cells instantiate other cells serve as prime examples.
/// Graph nodes are commonly stored unordered, but must occassionally be dependency-ordered
/// to perform processing tasks.
///
/// The [DepOrder] trait aids these orderings.
/// It requires a single user-defined method `process`, which processes a single `Item`.
/// The associated `Item` type is commonly a pointer to a graph node.
/// The implementation of `process` is responsible for iterating over `item`'s (direct) dependencies,
/// and passing each as an argument to `orderer.push`.
/// The `push` method, implemented on local helper-type [DepOrderer], recursively traverses
/// dependencies, calling `process` on each.
/// `push` also monitors for graph-cycles and returns the associated `Error` type if one is detected.
///
/// Typical usage:
///
/// ```text
/// struct MyGraphOrder;
/// impl DepOrder for MyGraphOrder {
///     type Item = Ptr<Node>;
///     type Error = MyError;
///
///     /// Process a single `item`
///     fn process(item: &Self::Item, orderer: &mut DepOrderer<Self>) -> Result<(), Self::Error> {
///         // Push each dependency
///         for dep in item.dependencies() {
///             orderer.push(dep);
///         }
///         // And return
///         Ok(())
///     }
///     fn fail() -> Result<(), Self::Error> {
///         Err(MyError::new())
///     }
/// }
/// ```
///
/// The default-implemented [DepOrder::order] creates and returns a dependency-ordered vector of `Item`s.
/// This method serves as the primary entrypoint for typical usage:
///
/// ```text
/// for item in MyGraphOrder::order(MyGraph::random()) {
///    // Do something with item
/// }
/// ```
///
pub trait DepOrder: Sized {
    // Associated types
    /// Item Type. Typically pointers or keys to the nodes in the dependency graph.
    type Item: Clone + Eq + std::hash::Hash;
    /// Error Type
    type Error;

    // Default Methods
    /// Dependency-order all entries in slice `items`
    fn order(items: &[Self::Item]) -> Result<Vec<Self::Item>, Self::Error> {
        DepOrderer::<Self>::order(items)
    }

    // Required Methods
    /// Process a single `item`, typically depth-first
    fn process(item: &Self::Item, orderer: &mut DepOrderer<Self>) -> Result<(), Self::Error>;
    /// Failure-handler. Return our `Error` type.
    fn fail() -> Result<(), Self::Error>;
}
/// # Dependency Order Helper
/// Should not be used directly.  
/// Public solely for use in the call-signature of [DepOrder::process].  
pub struct DepOrderer<P: DepOrder> {
    /// Ordered, completed items
    stack: Vec<P::Item>,
    /// Hash-set of completed items, for quick membership tests
    seen: HashSet<P::Item>,
    /// Hash-set of pending items, for cycle detection
    pending: HashSet<P::Item>,
    // Item-processor phantom reference
    p: PhantomData<P>,
}
impl<P: DepOrder> DepOrderer<P> {
    /// Dependency-order all entries in slice `items`
    pub fn order(items: &[P::Item]) -> Result<Vec<P::Item>, P::Error> {
        // Create an Orderer
        let len = items.len();
        let mut this = Self {
            stack: Vec::with_capacity(len),
            seen: HashSet::with_capacity(len),
            pending: HashSet::new(),
            p: PhantomData,
        };
        // Push it each item in `items`
        for item in items.iter() {
            this.push(item)?;
        }
        // And return its ordered stack
        Ok(this.stack)
    }
    /// Push `item`'s dependencies, and then itself, onto the stack
    pub fn push(&mut self, item: &P::Item) -> Result<(), P::Error> {
        // Depth-first search dependent Instance placements
        if !self.seen.contains(item) {
            // Check for cycles, indicated if `item` is in the pending-set, i.e. an open recursive stack-frame.
            if self.pending.contains(item) {
                return P::fail();
            }
            self.pending.insert(item.clone());
            // Process the Item, dependencies first
            P::process(item, self)?;
            // Check that `item` hasn't (somehow) been removed from the pending-set
            if !self.pending.remove(item) {
                return P::fail();
            }
            // And insert the Item itself
            self.seen.insert(item.clone());
            self.stack.push(item.clone());
        }
        Ok(())
    }
}
