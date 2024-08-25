use serde::{Deserialize, Serialize};

#[derive(Debug, Eq, PartialEq, Clone, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EgonDiagnosis {
    pub range: EgonDiagnosisRange,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub severity: Option<EgonDiagnosisSeverity>,

    pub message: String,

    /// Diagnostic code
    pub code: String,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Deserialize, Serialize)]
#[serde(transparent)]
pub struct EgonDiagnosisSeverity(i32);
impl EgonDiagnosisSeverity {
    pub const ERROR: EgonDiagnosisSeverity = EgonDiagnosisSeverity(1);
    pub const WARNING: EgonDiagnosisSeverity = EgonDiagnosisSeverity(2);
    pub const INFORMATION: EgonDiagnosisSeverity = EgonDiagnosisSeverity(3);
    pub const HINT: EgonDiagnosisSeverity = EgonDiagnosisSeverity(4);
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Default, Deserialize, Serialize)]
pub struct EgonDiagnosisPosition {
    pub index: usize,
    pub line: usize,
    pub character: usize,
}

impl EgonDiagnosisPosition {
    pub fn new(index: usize, line: usize, character: usize) -> EgonDiagnosisPosition {
        EgonDiagnosisPosition {
            index,
            line,
            character,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Default, Deserialize, Serialize)]
pub struct EgonDiagnosisRange {
    /// The range's start position (inclusive)
    pub start: EgonDiagnosisPosition,
    /// The range's end position (exclusive)
    pub end: EgonDiagnosisPosition,
}

impl EgonDiagnosisRange {
    pub fn new(start: EgonDiagnosisPosition, end: EgonDiagnosisPosition) -> EgonDiagnosisRange {
        EgonDiagnosisRange { start, end }
    }
}

pub trait Diagnosable {
    fn to_diagnosis(&self, source: &str, index: span::Span) -> EgonDiagnosis;
}
