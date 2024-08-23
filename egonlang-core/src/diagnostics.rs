use crate::prelude::*;
use line_col::LineColLookup;
use serde::{Deserialize, Serialize};

#[derive(Debug, Default)]
pub struct Diagnoser;

impl Diagnoser {
    pub fn get_diagnostics(errs: &[(EgonError, Span)], source: &str) -> Vec<Diagnosis> {
        return errs
            .iter()
            .map(|(err, span)| Diagnosis {
                range: Diagnoser::get_range(source, span),
                severity: Some(DiagnosisSeverity::ERROR),
                message: err.to_string(),
            })
            .collect();
    }

    /// Map position index to (line, column)
    ///
    /// Line and column are zero based
    pub fn index_to_position(source: &str, index: usize) -> (usize, usize) {
        let lookup = LineColLookup::new(source);

        let (line, char) = lookup.get(index);

        (line - 1, char - 1)
    }

    /// Map position (line, column) to index
    ///
    /// Line and column are zero based
    pub fn position_to_index(source: &str, position: (usize, usize)) -> usize {
        let (line, character) = position;
        let lines = source.split('\n');
        let lines_before = lines.take(line);
        let line_chars_before = lines_before.fold(0usize, |acc, e| acc + e.len() + 1);
        let chars = character;

        line_chars_before + chars
    }

    fn get_range(source: &str, span: &Span) -> DiagnosisRange {
        DiagnosisRange {
            start: Diagnoser::get_position(source, span.start),
            end: Diagnoser::get_position(source, span.end),
        }
    }

    fn get_position(source: &str, idx: usize) -> DiagnosisPosition {
        let (line, character) = Diagnoser::index_to_position(source, idx);

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
    pub const WARNING: DiagnosisSeverity = DiagnosisSeverity(2);
    pub const INFORMATION: DiagnosisSeverity = DiagnosisSeverity(3);
    pub const HINT: DiagnosisSeverity = DiagnosisSeverity(4);
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Default, Deserialize, Serialize)]
pub struct DiagnosisPosition {
    pub line: u32,
    pub character: u32,
}

impl DiagnosisPosition {
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
    pub fn new(start: DiagnosisPosition, end: DiagnosisPosition) -> DiagnosisRange {
        DiagnosisRange { start, end }
    }
}

#[cfg(test)]
mod tests {
    use crate::diagnostics::{Diagnosis, DiagnosisPosition, DiagnosisRange, DiagnosisSeverity};

    use super::{Diagnoser, EgonErrorS, EgonTypeError};

    use pretty_assertions::assert_eq;

    #[test]
    fn it_maps_error_to_diagnosis_in_multiline_source() {
        let source = r#"let b: number = 123;

a = b;"#;
        let err: EgonErrorS = (EgonTypeError::Undefined("a".to_string()).into(), 22..27);

        let diagnostics = Diagnoser::get_diagnostics(&[err], source);

        assert_eq!(
            vec![Diagnosis {
                range: DiagnosisRange {
                    start: DiagnosisPosition {
                        line: 2,
                        character: 0
                    },
                    end: DiagnosisPosition {
                        line: 2,
                        character: 5
                    }
                },
                severity: Some(DiagnosisSeverity::ERROR),
                message: "TypeError: `a` is not defined".to_string()
            }],
            diagnostics
        );
    }

    #[test]
    fn it_maps_error_to_diagnosis_in_multiline_source_b() {
        let source = r#"let b: number = 123;

a = b;

// out: TypeError: `a` is not defined"#;
        let err: EgonErrorS = (EgonTypeError::Undefined("a".to_string()).into(), 22..27);

        let diagnostics = Diagnoser::get_diagnostics(&[err], source);

        assert_eq!(
            vec![Diagnosis {
                range: DiagnosisRange {
                    start: DiagnosisPosition {
                        line: 2,
                        character: 0
                    },
                    end: DiagnosisPosition {
                        line: 2,
                        character: 5
                    }
                },
                severity: Some(DiagnosisSeverity::ERROR),
                message: "TypeError: `a` is not defined".to_string()
            }],
            diagnostics
        );
    }

    #[test]
    fn it_maps_error_to_diagnosis_in_single_line_source() {
        let source = r#"a = 123;"#;
        let err: EgonErrorS = (EgonTypeError::Undefined("a".to_string()).into(), 0..7);

        let diagnostics = Diagnoser::get_diagnostics(&[err], source);

        assert_eq!(
            vec![Diagnosis {
                range: DiagnosisRange {
                    start: DiagnosisPosition {
                        line: 0,
                        character: 0
                    },
                    end: DiagnosisPosition {
                        line: 0,
                        character: 7
                    }
                },
                severity: Some(DiagnosisSeverity::ERROR),
                message: "TypeError: `a` is not defined".to_string()
            }],
            diagnostics
        );
    }

    #[test]
    fn it_should_convert_index_to_position() {
        let source = "let a = 123;\nlet b = 456;";

        let index = 17usize;
        let expected_position = (1, 4);

        let actual_position = Diagnoser::index_to_position(source, index);

        assert_eq!(expected_position, actual_position);
    }

    #[test]
    fn it_should_convert_position_to_index() {
        let source = "let a = 123;\nlet b = 456;";
        let position = (1, 4);
        let expected_index = 17usize;
        let actual_index = Diagnoser::position_to_index(source, position);

        assert_eq!(expected_index, actual_index);
    }

    #[test]
    fn it_should_convert_position_to_index_and_back() {
        let source = "let a = 123;\nlet b = 456;";
        let position = (1, 4);
        let actual_index = Diagnoser::position_to_index(source, position);

        assert_eq!(position, Diagnoser::index_to_position(source, actual_index));
    }

    #[test]
    fn it_should_convert_position_to_index_and_back_b() {
        let source = "let a = 123;\n{\n    let b = 456;\n}";
        let position = (2, 12);
        let actual_index = Diagnoser::position_to_index(source, position);

        assert_eq!(position, Diagnoser::index_to_position(source, actual_index));
    }

    #[test]
    fn it_should_convert_position_to_index_b() {
        let source = "let a = 123;\n{\n    let b = 456;\n}";
        let position = (2, 12);
        let actual_index = Diagnoser::position_to_index(source, position);

        assert_eq!(27, actual_index);
    }

    #[test]
    fn it_should_convert_position_to_index_c() {
        let source = "let a = 123;\nlet b = 456;\nlet c = 789;";
        let position = (2, 8);
        let actual_index = Diagnoser::position_to_index(source, position);

        assert_eq!(34, actual_index);
    }

    #[test]
    fn it_should_convert_position_to_index_d() {
        let source = "let a = 123;\nlet b = 456;\nlet c = 789;\nlet d = 000;";
        let position = (3, 8);
        let actual_index = Diagnoser::position_to_index(source, position);

        assert_eq!(47, actual_index);
    }

    #[test]
    fn it_should_convert_position_to_index_e() {
        let source = "let a = 123;\nlet b = 456;\nlet c = 789;\nlet d = 000;\nlet e = 999;";
        let position = (4, 8);
        let actual_index = Diagnoser::position_to_index(source, position);

        assert_eq!(60, actual_index);
    }

    #[test]
    fn it_should_convert_position_to_index_f() {
        let source = "let a = 123;\nlet b = 456;\nlet c = 789;\nlet d = 000;\nlet e = 999;\n";
        let position = (4, 8);
        let actual_index = Diagnoser::position_to_index(source, position);

        assert_eq!(60, actual_index);
    }
}
