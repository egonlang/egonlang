pub mod ast;
mod diagnostics;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod span;

pub mod prelude {
    pub use crate::ast;
    pub use crate::errors::{
        Error as EgonError, ErrorS as EgonErrorS, SyntaxError as EgonSyntaxError,
        TypeError as EgonTypeError,
    };
    pub use crate::parser::parse;
    pub use crate::span::Span;
    pub use crate::span::Spanned;
}
