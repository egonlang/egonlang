pub mod rules;
mod type_env;
mod verifier;
mod verify_trace;

pub use type_env::{TypeEnv, TypeEnvValue};
pub use verifier::{VerificationResult, Verifier};

/// Verify a [`egonlang_core::ast::Module`] using the core [`rules::Rule`] set
///
/// See: [`rules::core`]
pub fn verify_module(module: &egonlang_core::ast::Module) -> VerificationResult {
    Verifier::default().verify(module)
}

pub mod prelude {
    pub use crate::expr_rule;
    pub use crate::rules;
    pub use crate::stmt_rule;
    pub use crate::type_env::{TypeEnv, TypeEnvValue};
    pub use crate::verifier::VerificationResult;
    pub use crate::verifier::Verifier;
    pub use crate::verify_module;
    pub use crate::verify_trace;
}
