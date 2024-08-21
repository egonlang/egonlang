use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, Position, Range, ServerCapabilities, ServerInfo,
    TextDocumentSyncKind,
};
use tower_lsp::{jsonrpc, Client, LanguageServer, LspService, Server};

use egonlang_core::prelude::*;
use egonlang_verifier::prelude::*;

#[derive(Debug)]
struct Backend {
    client: Client,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncKind::FULL.into()),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: env!("CARGO_PKG_NAME").to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            offset_encoding: None,
        })
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let source = &params.text_document.text;
        let uri = params.text_document.uri;
        let version = Some(params.text_document.version);
        let diagnostics = Diagnoser::get_diagnostics(source);
        self.client
            .publish_diagnostics(
                uri,
                diagnostics.iter().map(|x| (*x).clone().into()).collect(),
                version,
            )
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let source = &params.content_changes.first().unwrap().text;
        let uri = params.text_document.uri;
        let version = Some(params.text_document.version);
        let diagnostics = Diagnoser::get_diagnostics(source);
        self.client
            .publish_diagnostics(
                uri,
                diagnostics.iter().map(|x| (*x).clone().into()).collect(),
                version,
            )
            .await;
    }
}

impl From<Diagnosis> for Diagnostic {
    fn from(value: Diagnosis) -> Self {
        Diagnostic {
            range: value.range.into(),
            severity: value.severity.map(|x| x.into()),
            message: value.message,
            ..Default::default()
        }
    }
}

impl From<DiagnosisPosition> for Position {
    fn from(value: DiagnosisPosition) -> Self {
        Position {
            line: value.line,
            character: value.character,
        }
    }
}

impl From<DiagnosisRange> for Range {
    fn from(value: DiagnosisRange) -> Self {
        Range {
            start: value.start.into(),
            end: value.end.into(),
        }
    }
}

impl From<DiagnosisSeverity> for DiagnosticSeverity {
    fn from(value: DiagnosisSeverity) -> Self {
        match value {
            DiagnosisSeverity::ERROR => DiagnosticSeverity::ERROR,
            DiagnosisSeverity::HINT => DiagnosticSeverity::HINT,
            DiagnosisSeverity::INFORMATION => DiagnosticSeverity::INFORMATION,
            DiagnosisSeverity::WARNING => DiagnosticSeverity::WARNING,
            _ => panic!("Invalid diagnosis severity {:?}", value),
        }
    }
}

#[derive(Debug, Default)]
pub struct Diagnoser;

impl Diagnoser {
    pub fn get_diagnostics(source: &str) -> Vec<Diagnosis> {
        match parse(source, 0) {
            Ok(mut module) => {
                if let Err(errs) = verify_module(&mut module) {
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
            Err(errs) => {
                return errs
                    .iter()
                    .map(|(err, span)| Diagnosis {
                        range: Diagnoser::get_range(source, span),
                        severity: Some(DiagnosisSeverity::ERROR),
                        message: err.to_string(),
                    })
                    .collect()
            }
        }

        vec![]
    }

    pub fn get_range(source: &str, span: &Span) -> DiagnosisRange {
        DiagnosisRange {
            start: Diagnoser::get_position(source, span.start),
            end: Diagnoser::get_position(source, span.end),
        }
    }

    pub fn get_position(source: &str, idx: usize) -> DiagnosisPosition {
        let before = &source[..idx];
        let line = before.lines().count() - 1;
        let character = before.lines().last().unwrap().len();
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

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(Backend::new).finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
