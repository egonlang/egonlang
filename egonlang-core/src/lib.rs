pub mod ast;
pub mod diagnostics;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod span;

pub mod prelude {
    pub use crate::ast;
    pub use crate::diagnostics;
    pub use crate::errors::{EgonError, EgonErrorS, EgonSyntaxError, EgonTypeError};
    pub use crate::parser::parse;
    pub use crate::span::Span;
    pub use crate::span::Spanned;
}
