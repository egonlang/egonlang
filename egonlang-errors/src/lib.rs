use egonlang_diagnostics::Diagnosable;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use span::{Span, Spanned};

pub type EgonErrorS = Spanned<EgonError>;

/// A [`Result`] using a single [`EgonError`] wrapped in a [`Span`].
pub type EgonResultSingleSpannedErr<T> = Result<T, EgonErrorS>;

/// A [`Result`] using multiple [`EgonError`] wrapped in [`Span`]s.
pub type EgonResultMultiSpannedErr<T> = Result<T, Vec<EgonErrorS>>;

#[derive(Debug, Clone, Error, PartialEq, Serialize, Deserialize)]
pub enum EgonError {
    #[error("SyntaxError: {0}")]
    SyntaxError(EgonSyntaxError),
    #[error("TypeError: {0}")]
    TypeError(EgonTypeError),
}

trait ErrorCode {
    fn error_code(&self) -> String;
}

impl ErrorCode for EgonError {
    fn error_code(&self) -> String {
        match self {
            EgonError::SyntaxError(e) => e.error_code(),
            EgonError::TypeError(e) => e.error_code(),
        }
    }
}

impl Diagnosable for EgonError {
    fn to_diagnosis(&self, source: &str, span: span::Span) -> egonlang_diagnostics::EgonDiagnosis {
        match self {
            EgonError::SyntaxError(e) => e.to_diagnosis(source, span),
            EgonError::TypeError(e) => e.to_diagnosis(source, span),
        }
    }
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
    #[error("can not return outside of a block")]
    ReturnedUsedOutsideBlock,
    /// Indicates the reported code will never be executed.
    ///
    /// A common cause would be any code after a return statement.
    ///
    /// ```egon
    /// {
    ///   return true;
    ///   123; // SyntaxError: unreachable code
    ///   456  // SyntaxError: unreachable code
    /// };
    /// ```
    #[error("unreachable code")]
    UnreachableCode,
}

impl ErrorCode for EgonSyntaxError {
    fn error_code(&self) -> String {
        let subtype = match self {
            EgonSyntaxError::ExtraToken { token: _ } => "ExtraToken",
            EgonSyntaxError::InvalidToken => "InvalidToken",
            EgonSyntaxError::UnexpectedInput { token: _ } => "UnexpectedInput",
            EgonSyntaxError::UnrecognizedEOF { expected: _ } => "UnrecognizedEOF",
            EgonSyntaxError::UnrecognizedToken {
                token: _,
                expected: _,
            } => "UnrecognizedToken",
            EgonSyntaxError::UnterminatedString => "UnterminatedString",
            EgonSyntaxError::UninitializedConst { name: _ } => "UninitializedConst",
            EgonSyntaxError::ReassigningConst { name: _ } => "ReassigningConst",
            EgonSyntaxError::UninitializedUntypedLet { name: _ } => "UninitializedUntypedLet",
            EgonSyntaxError::EmptyRange => "EmptyRange",
            EgonSyntaxError::InvalidTypeAlias { name: _ } => "InvalidTypeAlias",
            EgonSyntaxError::DivideByZero => "DivideByZero",
            EgonSyntaxError::ReturnedUsedOutsideBlock => "ReturnedUsedOutsideBlock",
            EgonSyntaxError::UnreachableCode => "UnreachableCode",
        }
        .to_string();

        format!("SyntaxError{subtype}")
    }
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

impl ErrorCode for EgonTypeError {
    fn error_code(&self) -> String {
        let subtype = match self {
            EgonTypeError::MismatchType {
                expected: _,
                actual: _,
            } => "MismatchType",
            EgonTypeError::UknownListType => "UnknownListType",
            EgonTypeError::UnknownType => "UnknownType",
            EgonTypeError::Undefined(_) => "Undefined",
        }
        .to_string();

        format!("TypeError{subtype}")
    }
}

macro_rules! impl_from_error {
    ($($error:ident),+) => {$(
        ::paste::paste! {
        impl From<[<Egon $error>]> for $crate::EgonError {
                fn from(e: [<Egon $error>]) -> Self {
                    EgonError::$error(e)
                }
            }
        }
    )+};
}

impl_from_error!(SyntaxError, TypeError);

macro_rules! impl_diagnosible_for_error {
    ($error:ident) => {
        ::paste::paste! {
        impl ::egonlang_diagnostics::Diagnosable for $crate::[<Egon $error>] {
                fn to_diagnosis(&self, source: &str, span: Span) -> ::egonlang_diagnostics::EgonDiagnosis {
                    let start = {
                        let index = span.start;
                        let (line, character) = ::str_idxpos::index_to_position(source, index);

                        ::egonlang_diagnostics::EgonDiagnosisPosition {
                            line,
                            character,
                            index,
                        }
                    };

                    let end = {
                        let index = span.end;
                        let (line, character) = ::str_idxpos::index_to_position(source, index);

                        ::egonlang_diagnostics::EgonDiagnosisPosition {
                            line,
                            character,
                            index,
                        }
                    };

                    let range = ::egonlang_diagnostics::EgonDiagnosisRange { start, end };

                    let severity = match &self {
                        _ => Some(::egonlang_diagnostics::EgonDiagnosisSeverity::ERROR),
                    };

                    let message = self.to_string();

                    let code = self.error_code();

                    ::egonlang_diagnostics::EgonDiagnosis {
                        range,
                        severity,
                        message,
                        code
                    }
                }
        }
        }
    };
}

impl_diagnosible_for_error!(TypeError);
impl_diagnosible_for_error!(SyntaxError);

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use egonlang_diagnostics::{
        Diagnosable, EgonDiagnosis, EgonDiagnosisPosition, EgonDiagnosisRange,
        EgonDiagnosisSeverity,
    };

    use crate::{EgonError, EgonTypeError};

    #[test]
    fn it_maps_error_to_diagnosis_in_multiline_source() {
        let source = r#"let b: number = 123;

    a = b;"#;
        let err: EgonError = EgonTypeError::Undefined("a".to_string()).into();
        let span = 22..27;

        let diagnostics = err.to_diagnosis(source, span);

        assert_eq!(
            EgonDiagnosis {
                range: EgonDiagnosisRange {
                    start: EgonDiagnosisPosition {
                        line: 2,
                        character: 0,
                        index: 22
                    },
                    end: EgonDiagnosisPosition {
                        line: 2,
                        character: 5,
                        index: 27
                    }
                },
                severity: Some(EgonDiagnosisSeverity::ERROR),
                message: "`a` is not defined".to_string(),
                code: "TypeErrorUndefined".to_string()
            },
            diagnostics
        );
    }

    #[test]
    fn it_maps_error_to_diagnosis_in_multiline_source_b() {
        let source = r#"let b: number = 123;

    a = b;

    // out: TypeError: `a` is not defined"#;
        let err: EgonError = EgonTypeError::Undefined("a".to_string()).into();
        let span = 22..27;

        let diagnostics = err.to_diagnosis(source, span);

        assert_eq!(
            EgonDiagnosis {
                range: EgonDiagnosisRange {
                    start: EgonDiagnosisPosition {
                        line: 2,
                        character: 0,
                        index: 22
                    },
                    end: EgonDiagnosisPosition {
                        line: 2,
                        character: 5,
                        index: 27
                    }
                },
                severity: Some(EgonDiagnosisSeverity::ERROR),
                message: "`a` is not defined".to_string(),
                code: "TypeErrorUndefined".to_string()
            },
            diagnostics
        );
    }

    #[test]
    fn it_maps_error_to_diagnosis_in_single_line_source() {
        let source = r#"a = 123;"#;
        let err: EgonError = EgonTypeError::Undefined("a".to_string()).into();
        let span = 0..7;

        let diagnostics = err.to_diagnosis(source, span);

        assert_eq!(
            EgonDiagnosis {
                range: EgonDiagnosisRange {
                    start: EgonDiagnosisPosition {
                        line: 0,
                        character: 0,
                        index: 0
                    },
                    end: EgonDiagnosisPosition {
                        line: 0,
                        character: 7,
                        index: 7
                    }
                },
                severity: Some(EgonDiagnosisSeverity::ERROR),
                message: "`a` is not defined".to_string(),
                code: "TypeErrorUndefined".to_string()
            },
            diagnostics
        );
    }
}
