//!
//! # Layout21 Error-Helper Utilities
//!
//! ```rust
//! use layout21utils::error::{ErrorHelper, Unwrapper};
//!
//! /// Example implementer of [`ErrorHelper`].
//! /// Typical implementers will have some internal state to report upon failure.
//! struct HasFunErrors;
//! impl ErrorHelper for HasFunErrors {
//!     type Error = String;
//!
//!     /// Add our extra-fun state upon failure.
//!     fn err(&self, msg: impl Into<String>) -> Self::Error {
//!         format!("Extra Fun Error: {}", msg.into())
//!     }
//! }
//! impl HasFunErrors {
//!     /// Demo of using the [`Unwrapper`] trait on [`Option`]s and [`Result`]s.
//!     fn fun(&self) -> Result<(), String> {
//!         // Unwrap an [`Option`]
//!         Some(5).unwrapper(self, "Option failed!")?;
//!
//!         // Unwrap a [`Result`]
//!         let r: Result<(), String> = Ok(());
//!         r.unwrapper(self, "Result failed!")
//!     }
//! }
//! ```
//!

///
/// # ErrorHelper
///
/// Helper trait for re-use among many conversion tree-walkers.
/// Each implementer will generally have some internal state to report upon failure,
/// which it can inject in the implementation-required `err` method.
/// The `fail` method, provided by default, simply returns the `err` value.
///
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
    /// Assert a boolean condition. Returns through `self.fail` if it is not satisfied.
    fn assert(&self, b: bool, msg: impl Into<String>) -> Result<(), Self::Error> {
        match b {
            true => Ok(()),
            false => self.fail(msg),
        }
    }
}

///
/// # Unwrapper
///
/// Trait for post-fix application of [`ErrorHelper`] handling,
/// during the particularly common cases of unwrapping [`Option`]s and [`Result`]s.
///
/// Sole method `unwrapper` takes an [`ErrorHelper`] and string-convertible error-message as arguments,
/// and returns a [`Result`] of the [`ErrorHelper`]'s associated error type.  
///
/// Example:
///
/// ```rust
/// use layout21utils::error::{ErrorHelper, Unwrapper};
///
/// fn example(h: &impl ErrorHelper<Error=String>) -> Result<(), String> {
///     // Unwrap an [`Option`]
///     Some(5).unwrapper(h, "Option failed!")?;
///
///     // Unwrap a [`Result`]
///     let r: Result<(), String> = Ok(());
///     r.unwrapper(h, "Result failed!")
/// }
/// ```
///
/// The typical usage of [`Unwrapper`] is not to implement it for new types,
/// but to just import the trait and use it on the standard library [`Option`] and [`Result`] types.
/// And while not required, said usages are generally expected to be
/// in the context of a type that implements [`ErrorHelper`].
///
pub trait Unwrapper {
    type Ok;
    fn unwrapper<H>(self, helper: &H, msg: impl Into<String>) -> Result<Self::Ok, H::Error>
    where
        H: ErrorHelper;
}

/// # Unwrapper for [`Option`]
///
/// Performs an action similar to [`Option.unwrap`], but routing failures to the paired [`ErrorHelper`] failure-handler rather than panicking.
///
impl<T> Unwrapper for Option<T> {
    type Ok = T;
    fn unwrapper<H>(self, helper: &H, msg: impl Into<String>) -> Result<Self::Ok, H::Error>
    where
        H: ErrorHelper,
    {
        match self {
            Some(t) => Ok(t),
            None => helper.fail(msg),
        }
    }
}

/// # Unwrapper for [`Result`]
///
/// Performs an action similar to [`Result.unwrap`], but routing failures to the paired [`ErrorHelper`] failure-handler rather than panicking.
///
impl<T, E> Unwrapper for Result<T, E> {
    type Ok = T;
    fn unwrapper<H>(
        self,
        helper: &H,
        msg: impl Into<String>,
    ) -> Result<<Self as Unwrapper>::Ok, H::Error>
    where
        H: ErrorHelper,
    {
        match self {
            Ok(t) => Ok(t),
            Err(_) => helper.fail(msg),
        }
    }
}
