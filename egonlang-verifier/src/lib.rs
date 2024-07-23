pub mod rules;
pub mod type_env;
pub mod verifier;
mod verify_trace;
pub mod visitor;

use egonlang_core::ast::Module;
pub use type_env::TypeEnv;
use verifier::{VerificationResult, Verifier};

/// Verify the module AST
pub fn verify_module(module: &Module) -> VerificationResult {
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
