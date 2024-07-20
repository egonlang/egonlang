pub mod rules;
pub mod type_env;
pub mod verifier;
mod verify_trace;
pub mod visitor;

use egonlang_core::{ast::Module, parser::parse};
pub use type_env::TypeEnv;
use verifier::{VerificationResult, Verifier};

/// Parse source string and verify the module AST
pub fn verify_source(source: &str) -> VerificationResult {
    parse(source, 0).and_then(|module| verify_module(&module))
}

/// Verify the module AST
pub fn verify_module(module: &Module) -> VerificationResult {
    Verifier::new().verify(module)
}

pub mod prelude {
    pub use crate::expr_rule;
    pub use crate::rules::rule::Rule;
    pub use crate::stmt_rule;
    pub use crate::type_env::TypeEnv;
    pub use crate::verifier::VerificationResult;
    pub use crate::verify_source;
    pub use crate::verify_trace;
}
