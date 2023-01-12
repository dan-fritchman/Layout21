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
