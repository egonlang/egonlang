use crate::errors::{EgonError, EgonSyntaxError, EgonTypeError};
use crate::span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use serde::{Deserialize, Serialize};

#[derive(Debug, Default)]
pub struct Diagnoser {}

impl Diagnoser {
    #[allow(dead_code)]
    pub fn get_diagnostics(source: &str) -> Vec<Diagnosis> {
        match crate::parser::parse(source, 0) {
            Ok(_) => vec![],
            Err(errs) => {
                return errs
                    .iter()
                    .map(|(err, span)| Diagnosis {
                        range: Diagnoser::get_range(source, span),
                        severity: Some(DiagnosisSeverity::ERROR),
                        message: err.to_string(),
                    })
                    .collect();
            }
        }
    }

    #[allow(dead_code)]
    fn get_range(source: &str, span: &Span) -> DiagnosisRange {
        DiagnosisRange {
            start: Diagnoser::get_position(source, span.start),
            end: Diagnoser::get_position(source, span.end),
        }
    }

    #[allow(dead_code)]
    fn get_position(source: &str, idx: usize) -> DiagnosisPosition {
        let before = &source[..idx];
        let line = before.lines().count().checked_sub(1).unwrap_or_default();
        let character = before.lines().last().unwrap_or_default().len();
        DiagnosisPosition {
            line: line as _,
            character: character as _,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Diagnosis {
    pub range: DiagnosisRange,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub severity: Option<DiagnosisSeverity>,

    pub message: String,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Deserialize, Serialize)]
#[serde(transparent)]
pub struct DiagnosisSeverity(i32);
impl DiagnosisSeverity {
    pub const ERROR: DiagnosisSeverity = DiagnosisSeverity(1);
    #[allow(dead_code)]
    pub const WARNING: DiagnosisSeverity = DiagnosisSeverity(2);
    #[allow(dead_code)]
    pub const INFORMATION: DiagnosisSeverity = DiagnosisSeverity(3);
    #[allow(dead_code)]
    pub const HINT: DiagnosisSeverity = DiagnosisSeverity(4);
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Default, Deserialize, Serialize)]
pub struct DiagnosisPosition {
    pub line: u32,
    pub character: u32,
}

impl DiagnosisPosition {
    #[allow(dead_code)]
    pub fn new(line: u32, character: u32) -> DiagnosisPosition {
        DiagnosisPosition { line, character }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Default, Deserialize, Serialize)]
pub struct DiagnosisRange {
    /// The range's start position (inclusive)
    pub start: DiagnosisPosition,
    /// The range's end position (exclusive)
    pub end: DiagnosisPosition,
}

impl DiagnosisRange {
    #[allow(dead_code)]
    pub fn new(start: DiagnosisPosition, end: DiagnosisPosition) -> DiagnosisRange {
        DiagnosisRange { start, end }
    }
}

#[allow(dead_code)]
trait AsDiagnostic {
    fn as_diagnostic(&self, span: &Span) -> Diagnostic<()>;
}

macro_rules! impl_as_dianostic {
    ($($error:tt),+) => {$(
        impl AsDiagnostic for $error {
            fn as_diagnostic(&self, span: &Span) -> Diagnostic<()> {
                Diagnostic::error()
                    .with_code(stringify!($error))
                    .with_message(self.to_string())
                    .with_labels(vec![Label::primary((), span.clone())])
            }
        }
    )+};
}

impl_as_dianostic!(EgonSyntaxError, EgonTypeError);

impl AsDiagnostic for EgonError {
    fn as_diagnostic(&self, span: &Span) -> Diagnostic<()> {
        match self {
            EgonError::SyntaxError(e) => e.as_diagnostic(span),
            EgonError::TypeError(e) => e.as_diagnostic(span),
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::diagnostics::{
        Diagnoser, Diagnosis, DiagnosisPosition, DiagnosisRange, DiagnosisSeverity,
    };

    macro_rules! diagnostics_test {
        ($test_name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let source = String::from($input);

                assert_eq!($expected, Diagnoser::get_diagnostics(&source));
            }
        };
    }

    diagnostics_test!(
        diagnosis_unexpected_eof,
        "123",
        vec![Diagnosis {
            range: DiagnosisRange {
                start: DiagnosisPosition {
                    line: 0,
                    character: 3,
                },
                end: DiagnosisPosition {
                    line: 0,
                    character: 3,
                },
            },
            severity: Some(DiagnosisSeverity::ERROR),
            message: String::from("SyntaxError: unexpected end of file; expected: [\"\\\"!=\\\"\", \"\\\"%\\\"\", \"\\\")\\\"\", \"\\\"*\\\"\", \"\\\"+\\\"\", \"\\\",\\\"\", \"\\\"-\\\"\", \"\\\"..\\\"\", \"\\\"/\\\"\", \"\\\";\\\"\", \"\\\"<\\\"\", \"\\\"<=\\\"\", \"\\\"==\\\"\", \"\\\">\\\"\", \"\\\">=\\\"\", \"\\\"]\\\"\", \"\\\"and\\\"\", \"\\\"or\\\"\", \"\\\"}\\\"\"]")
        }]
    );

    diagnostics_test!(
        diagnosis_unterminated_string,
        r#""foo"#,
        vec![Diagnosis {
            range: DiagnosisRange {
                start: DiagnosisPosition {
                    line: 0,
                    character: 0,
                },
                end: DiagnosisPosition {
                    line: 0,
                    character: 4,
                },
            },
            severity: Some(DiagnosisSeverity::ERROR),
            message: String::from("SyntaxError: unterminated string")
        }]
    );

    diagnostics_test!(
        diagnosis_unexpected_input,
        r#"@foo"#,
        vec![Diagnosis {
            range: DiagnosisRange {
                start: DiagnosisPosition {
                    line: 0,
                    character: 0,
                },
                end: DiagnosisPosition {
                    line: 0,
                    character: 4,
                },
            },
            severity: Some(DiagnosisSeverity::ERROR),
            message: String::from("SyntaxError: unexpected input")
        }]
    );

    diagnostics_test!(
        diagnosis_error_on_newline,
        "\n@foo",
        vec![Diagnosis {
            range: DiagnosisRange {
                start: DiagnosisPosition {
                    line: 0, // TODO: This should be 1?
                    character: 0,
                },
                end: DiagnosisPosition {
                    line: 1,
                    character: 4,
                },
            },
            severity: Some(DiagnosisSeverity::ERROR),
            message: String::from("SyntaxError: unexpected input")
        }]
    );
}
