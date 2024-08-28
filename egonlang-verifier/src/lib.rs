pub mod rules;
mod verifier;

use egonlang_errors::EgonResultMultiSpannedErr;
pub use verifier::Verifier;

/// Verify a [`egonlang_core::ast::Module`] using the core [`rules::Rule`] set
///
/// See: [`rules::core`]
pub fn verify_module(module: &mut egonlang_core::ast::Module) -> EgonResultMultiSpannedErr<()> {
    Verifier::default().verify(module)
}

pub mod prelude {
    pub use crate::expr_rule;
    pub use crate::rules;
    pub use crate::stmt_rule;
    pub use crate::verifier::Verifier;
    pub use crate::verify_module;
}
