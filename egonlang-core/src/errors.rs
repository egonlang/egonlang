use thiserror::Error;

use crate::span::Spanned;

pub type ErrorS = Spanned<Error>;

#[derive(Debug, Error, PartialEq)]
pub enum Error {
    #[error("SyntaxError: {0}")]
    SyntaxError(SyntaxError),
    #[error("TypeError: {0}")]
    TypeError(TypeError),
}

#[derive(Debug, Error, Eq, PartialEq)]
pub enum SyntaxError {
    #[error("extraneous input: {token:?}")]
    ExtraToken { token: String },
    #[error("invalid input")]
    InvalidToken,
    #[error("unexpected input")]
    UnexpectedInput { token: String },
    #[error("unexpected end of file; expected: {expected:?}")]
    UnrecognizedEOF { expected: Vec<String> },
    #[error("unexpected {token:?}; expected: {expected:?}")]
    UnrecognizedToken {
        token: String,
        expected: Vec<String>,
    },
    #[error("unterminated string")]
    UnterminatedString,
    #[error("const `{name}` missing value. consts must be declared with a value")]
    UninitializedConst { name: String },
    #[error("`{name}` is a const value and can't be reassigned")]
    ReassigningConst { name: String },
    #[error("let `{name}` missing value and type. lets require at least a type or declared with a value")]
    UninitializedUntypedLet { name: String },
}

#[derive(Debug, Error, Eq, PartialEq)]
pub enum TypeError {
    #[error("mismatched types: expected type `{expected}` but received `{actual}`")]
    MismatchType { expected: String, actual: String },

    #[error("list declared with an unknown type e.g. let a = []; // list<unknown>")]
    UknownListType,

    #[error("unknown type e.g. let a: unknown; or let a;")]
    UnknownType,
}

macro_rules! impl_from_error {
    ($($error:tt),+) => {$(
        impl From<$error> for Error {
            fn from(e: $error) -> Self {
                Error::$error(e)
            }
        }
    )+};
}

impl_from_error!(SyntaxError);
impl_from_error!(TypeError);
