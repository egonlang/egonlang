pub mod ast;
pub mod diagnostics;
pub mod lexer;
pub mod parser;

pub mod prelude {
    pub use crate::ast;
    pub use crate::diagnostics;
    pub use crate::parser::parse;
}
