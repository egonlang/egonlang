use thiserror::Error;

use crate::span::Spanned;

pub type ErrorS = Spanned<Error>;

#[derive(Debug, Error, PartialEq)]
pub enum Error {
    #[error("SyntaxError: {0}")]
    SyntaxError(SyntaxError),
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
