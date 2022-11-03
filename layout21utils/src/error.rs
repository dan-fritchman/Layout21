//!
//! # Layout21 Error-Helper Utilities
//!

/// Helper trait for re-use among our many conversion tree-walkers.
/// Each implementer will generally have some internal state to report upon failure,
/// which it can inject in the implementation-required `err` method.
/// The `fail` method, provided by default, simply returns the `err` value.
pub trait ErrorHelper {
    type Error;

    /// Create and return a [Self::Error] value.
    fn err(&self, msg: impl Into<String>) -> Self::Error;
    /// Return failure
    fn fail<T>(&self, msg: impl Into<String>) -> Result<T, Self::Error> {
        Err(self.err(msg))
    }
    /// Unwrap the [Option] `opt` if it is [Some], and return our error if not.
    fn unwrap<T>(&self, opt: Option<T>, msg: impl Into<String>) -> Result<T, Self::Error> {
        match opt {
            Some(val) => Ok(val),
            None => self.fail(msg),
        }
    }
    /// Assert boolean condition `b`. Returns through `self.fail` if not.
    fn assert(&self, b: bool, msg: impl Into<String>) -> Result<(), Self::Error> {
        match b {
            true => Ok(()),
            false => self.fail(msg),
        }
    }
    /// Unwrap the [Result] `res`. Return through our failure method if it is [Err].
    /// Optional method, but must be implemented to be (usefully) called.
    /// The default implementation simply returns an error via `self.fail`.
    fn ok<T, E>(&self, _res: Result<T, E>, msg: impl Into<String>) -> Result<T, Self::Error>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        self.fail(msg) // Default version always fails.
    }
}
/// Enumerated conversion contexts
/// Generally used for error reporting
#[derive(Debug, Clone)]
pub enum ErrorContext {
    Library(String),
    Cell(String),
    Abstract,
    Impl,
    Instance(String),
    Array(String),
    Units,
    Geometry,
    Unknown,
}
