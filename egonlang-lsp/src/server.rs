use std::collections::HashMap;
use std::sync::Arc;

use diagnostics::{Diagnoser, Diagnosis, DiagnosisPosition, DiagnosisRange, DiagnosisSeverity};
use egonlang_core::prelude::*;
use egonlang_verifier::prelude::*;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{HoverProviderCapability, Url};
use tower_lsp::{
    jsonrpc,
    lsp_types::{
        Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
        Hover, HoverContents, HoverParams, InitializeParams, InitializeResult, MarkupContent,
        Position, Range, ServerCapabilities, ServerInfo, TextDocumentSyncKind,
    },
    Client, LanguageServer,
};

#[derive(Debug)]
pub struct EgonLanguageServerBackend {
    client: Client,
    documents: Arc<Mutex<HashMap<Url, String>>>,
}

impl EgonLanguageServerBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(Mutex::new(HashMap::new())),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for EgonLanguageServerBackend {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncKind::FULL.into()),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
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

        let mut documents = self.documents.lock().await;
        documents.insert(uri.clone(), source.to_string());

        let version = Some(params.text_document.version);
        let errs = parse(source, 0)
            .and_then(|mut module| verify_module(&mut module))
            .err()
            .unwrap_or_default();
        let diagnostics = Diagnoser::get_diagnostics(&errs, source);
        self.client
            .publish_diagnostics(
                uri,
                diagnostics
                    .iter()
                    .map(|x| LspDiagnosis((*x).clone()).into())
                    .collect(),
                version,
            )
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let source = &params.content_changes.first().unwrap().text;
        let uri = params.text_document.uri;

        let mut documents = self.documents.lock().await;
        documents.insert(uri.clone(), source.to_string());

        let version = Some(params.text_document.version);
        let errs = parse(source, 0)
            .and_then(|mut module| verify_module(&mut module))
            .err()
            .unwrap_or_default();
        let diagnostics = Diagnoser::get_diagnostics(&errs, source);
        self.client
            .publish_diagnostics(
                uri,
                diagnostics
                    .iter()
                    .map(|x| LspDiagnosis((*x).clone()).into())
                    .collect(),
                version,
            )
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let source_uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.lock().await;

        let doc = docs.get(&source_uri).unwrap();
        let index = Diagnoser::position_to_index(
            doc,
            (position.line as usize, position.character as usize),
        );

        if let Ok(module) = parse(doc, 0) {
            let mut nodes = module.get_by_index(index);

            nodes.reverse();

            Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::PlainText,
                    value: format!("{nodes:#?}"),
                }),
                range: None,
            }))
        } else {
            Ok(None)
        }
    }
}

struct LspDiagnosis(Diagnosis);

impl From<LspDiagnosis> for Diagnostic {
    fn from(value: LspDiagnosis) -> Self {
        Diagnostic {
            range: LspDiagnosisRange(value.0.range).into(),
            severity: value.0.severity.map(|x| LspDiagnosisSeverity(x).into()),
            message: value.0.message,
            ..Default::default()
        }
    }
}

struct LspDiagnosisPosition(DiagnosisPosition);

impl From<LspDiagnosisPosition> for Position {
    fn from(value: LspDiagnosisPosition) -> Self {
        Position {
            line: value.0.line,
            character: value.0.character,
        }
    }
}

struct LspDiagnosisRange(DiagnosisRange);

impl From<LspDiagnosisRange> for Range {
    fn from(value: LspDiagnosisRange) -> Self {
        Range {
            start: LspDiagnosisPosition(value.0.start).into(),
            end: LspDiagnosisPosition(value.0.end).into(),
        }
    }
}

struct LspDiagnosisSeverity(DiagnosisSeverity);

impl From<LspDiagnosisSeverity> for DiagnosticSeverity {
    fn from(value: LspDiagnosisSeverity) -> Self {
        match value.0 {
            DiagnosisSeverity::ERROR => DiagnosticSeverity::ERROR,
            DiagnosisSeverity::HINT => DiagnosticSeverity::HINT,
            DiagnosisSeverity::INFORMATION => DiagnosticSeverity::INFORMATION,
            DiagnosisSeverity::WARNING => DiagnosticSeverity::WARNING,
            _ => panic!("Invalid diagnosis severity {:?}", value.0),
        }
    }
}
