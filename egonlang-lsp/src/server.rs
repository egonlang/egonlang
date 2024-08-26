use std::collections::HashMap;
use std::sync::Arc;

use egonlang_core::parser::parse;
// use egonlang_core::prelude::*;
use egonlang_diagnostics::{
    Diagnosable, EgonDiagnosis, EgonDiagnosisPosition, EgonDiagnosisRange, EgonDiagnosisSeverity,
};
use egonlang_verifier::prelude::*;
use span::Span;
use str_idxpos::position_to_index;
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

        // Parse and verify egon code
        let errs = parse(source, 0)
            .and_then(|mut module| verify_module(&mut module))
            .err()
            .unwrap_or_default();

        // Map errors to
        let diagnostics: Vec<LspDiagnosis> = errs
            .iter()
            .map(|(e, e_span)| {
                let diagnosable: &(dyn Diagnosable + Sync) = e as &(dyn Diagnosable + Sync);
                LspDiagnosis(diagnosable.to_diagnosis(source, e_span.clone()))
            })
            .collect();

        self.client
            .publish_diagnostics(
                uri,
                diagnostics.iter().map(|x| x.clone().into()).collect(),
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
        let diagnostics: Vec<(&(dyn Diagnosable + Sync), &Span)> = errs
            .iter()
            .map(|e| {
                let e2: &(dyn Diagnosable + Sync) = &e.0 as &(dyn Diagnosable + Sync);
                (e2, &e.1)
            })
            .collect();
        self.client
            .publish_diagnostics(
                uri,
                diagnostics
                    .iter()
                    .map(|x| {
                        let d = x.0.to_diagnosis(source, x.1.clone());
                        LspDiagnosis(d).into()
                    })
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
        let index = position_to_index(doc, (position.line as usize, position.character as usize));

        if let Ok(module) = parse(doc, 0) {
            let mut nodes = module.get_by_index(index);

            let first_node = nodes.pop().unwrap();

            // Primary AST node
            let first_node_type = first_node.node_type();
            let first_node = format!("{:#?}", first_node);

            // Surrounding/related AST nodes
            // Reversing the nodes allows the more relevant nodes to appear first
            nodes.reverse();

            let nodes: Vec<String> = nodes
                .iter()
                .map(|x| format!("---\n### {}\n```\n{x:#?}\n```", x.node_type()))
                .collect();
            let nodes = nodes.join("\n");

            Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: format!(
                        "# AST\nHovered At: index={index}; {position:?}\n## {first_node_type}\n```\n{first_node}\n```\n## See Also\n{nodes}"
                    ),
                }),
                range: None,
            }))
        } else {
            Ok(None)
        }
    }
}

/// Struct wrapping [`EgonDiagnosis`] so it can be mapped to LSP Diagnostic
///
/// We don't own [`EgonDiagnosis`] so we have to wrap it.
#[derive(Clone)]
struct LspDiagnosis(EgonDiagnosis);

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

struct LspDiagnosisPosition(EgonDiagnosisPosition);

impl From<LspDiagnosisPosition> for Position {
    fn from(value: LspDiagnosisPosition) -> Self {
        Position {
            line: value.0.line as u32,
            character: value.0.character as u32,
        }
    }
}

struct LspDiagnosisRange(EgonDiagnosisRange);

impl From<LspDiagnosisRange> for Range {
    fn from(value: LspDiagnosisRange) -> Self {
        Range {
            start: LspDiagnosisPosition(value.0.start).into(),
            end: LspDiagnosisPosition(value.0.end).into(),
        }
    }
}

struct LspDiagnosisSeverity(EgonDiagnosisSeverity);

impl From<LspDiagnosisSeverity> for DiagnosticSeverity {
    fn from(value: LspDiagnosisSeverity) -> Self {
        match value.0 {
            EgonDiagnosisSeverity::ERROR => DiagnosticSeverity::ERROR,
            EgonDiagnosisSeverity::HINT => DiagnosticSeverity::HINT,
            EgonDiagnosisSeverity::INFORMATION => DiagnosticSeverity::INFORMATION,
            EgonDiagnosisSeverity::WARNING => DiagnosticSeverity::WARNING,
            _ => panic!("Invalid diagnosis severity {:?}", value.0),
        }
    }
}
