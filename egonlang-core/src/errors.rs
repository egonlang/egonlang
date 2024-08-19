use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::span::Spanned;

pub type EgonErrorS = Spanned<EgonError>;

#[derive(Debug, Clone, Error, PartialEq, Serialize, Deserialize)]
pub enum EgonError {
    #[error("SyntaxError: {0}")]
    SyntaxError(EgonSyntaxError),
    #[error("TypeError: {0}")]
    TypeError(EgonTypeError),
}

#[derive(Debug, Clone, Error, Eq, PartialEq, Serialize, Deserialize)]
pub enum EgonSyntaxError {
    #[error("extraneous input: {token:?}")]
    ExtraToken { token: String },
    #[error("invalid input")]
    InvalidToken,
    #[error("unexpected input: {token:?}")]
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
    #[error("empty range e.g. `..`")]
    EmptyRange,
    #[error("type aliases are required to be PascalCase: `{name}`")]
    InvalidTypeAlias { name: String },
    #[error("can not divide by zero")]
    DivideByZero,
}

#[derive(Debug, Clone, Error, Eq, PartialEq, Serialize, Deserialize)]
pub enum EgonTypeError {
    #[error("mismatched types: expected type `{expected}` but received `{actual}`")]
    MismatchType { expected: String, actual: String },

    #[error("list declared with an unknown type e.g. let a = []; // list<unknown>")]
    UknownListType,

    #[error("unknown type e.g. let a: unknown; or let a;")]
    UnknownType,
    #[error("`{0}` is not defined")]
    Undefined(String),
}

macro_rules! impl_from_error {
    ($($error:ident),+) => {$(
        ::paste::paste! {
        impl From<[<Egon $error>]> for EgonError {
                fn from(e: [<Egon $error>]) -> Self {
                    EgonError::$error(e)
                }
            }
        }
    )+};
}

impl_from_error!(SyntaxError, TypeError);
