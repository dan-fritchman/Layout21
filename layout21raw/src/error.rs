//!
//! # Layout Result and Error Types
//!

// Local Imports
pub use crate::utils::{self, ErrorContext};

/// # [LayoutError] Result Type
pub type LayoutResult<T> = Result<T, LayoutError>;

///
/// # Layout Error Enumeration
///
pub enum LayoutError {
    /// Error Exporting to Foreign Format
    Export {
        message: String,
        stack: Vec<ErrorContext>,
    },
    /// Error Importing from Foreign Format
    Import {
        message: String,
        stack: Vec<ErrorContext>,
    },
    /// Conversion Errors, with Boxed External Error
    Conversion {
        message: String,
        err: Box<dyn std::error::Error + Send + Sync>,
        stack: Vec<ErrorContext>,
    },
    /// Boxed External Errors
    Boxed(Box<dyn std::error::Error + Send + Sync>),
    /// Uncategorized Error, with String Message
    Str(String),
    /// # [Ptr] Locking
    /// Caused by trouble with a [Ptr]: either deadlock, or panic while holding a lock.
    /// Generally caused by a [std::sync::PoisonError], which is not forwardable due to lifetime constraints.
    PtrLock,
}
impl LayoutError {
    /// Create a [LayoutError::Message] from anything String-convertible
    pub fn msg(s: impl Into<String>) -> Self {
        Self::Str(s.into())
    }
    /// Create an error-variant [Result] of our [LayoutError::Message] variant from anything String-convertible
    pub fn fail<T>(s: impl Into<String>) -> Result<T, Self> {
        Err(Self::msg(s))
    }
}
impl std::fmt::Debug for LayoutError {
    /// Display a [LayoutError]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LayoutError::Export { message, stack } => {
                write!(f, "Export Error: \n - {} \n - {:?}", message, stack)
            }
            LayoutError::Import { message, stack } => {
                write!(f, "Import Error: \n - {} \n - {:?}", message, stack)
            }
            LayoutError::Conversion {
                message,
                err,
                stack,
            } => write!(
                f,
                "Conversion Error: \n - {} \n - {} \n - {:?}",
                message, err, stack
            ),
            LayoutError::Boxed(err) => err.fmt(f),
            LayoutError::Str(err) => err.fmt(f),
            LayoutError::PtrLock => write!(f, "[std::sync::PoisonError]"),
        }
    }
}
impl std::fmt::Display for LayoutError {
    /// Display a [LayoutError]
    /// Delegates to the [Debug] implementation
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}
impl std::error::Error for LayoutError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Boxed(e) => Some(&**e),
            _ => None,
        }
    }
}

impl From<String> for LayoutError {
    fn from(s: String) -> Self {
        Self::Str(s)
    }
}
impl From<&str> for LayoutError {
    fn from(s: &str) -> Self {
        Self::Str(s.to_string())
    }
}
impl From<std::num::TryFromIntError> for LayoutError {
    fn from(e: std::num::TryFromIntError) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl From<utils::ser::Error> for LayoutError {
    fn from(e: utils::ser::Error) -> Self {
        Self::Boxed(Box::new(e))
    }
}
impl<T> From<std::sync::PoisonError<T>> for LayoutError {
    fn from(_e: std::sync::PoisonError<T>) -> Self {
        Self::PtrLock
    }
}
impl<T: std::error::Error + Send + Sync + 'static> From<Box<T>> for LayoutError {
    fn from(e: Box<T>) -> Self {
        Self::Boxed(e)
    }
}
