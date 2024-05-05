use egonlang_core::{
    ast::{Expr, Module, Stmt},
    errors::ErrorS,
    span::Span,
};

use crate::{type_env::TypeEnv, visitor::Visitor};

pub type VerificationResult = Result<(), Vec<ErrorS>>;

#[derive(Default)]
pub struct Verifier<'a> {
    types: TypeEnv<'a>,
}

impl Verifier<'_> {
    fn new() -> Self {
        Verifier::default()
    }

    pub fn verify(&mut self, module: &Module) -> VerificationResult {
        Ok(())
    }
}

impl Visitor<'_> for Verifier<'_> {
    fn visit_stmt(
        &self,
        stmt: &Stmt,
        span: &Span,
        types: &mut TypeEnv<'_>,
    ) -> Result<(), Vec<ErrorS>> {
        todo!()
    }

    fn visit_expr(
        &self,
        stmt: &Expr,
        span: &Span,
        types: &mut TypeEnv<'_>,
    ) -> Result<(), Vec<ErrorS>> {
        todo!()
    }
}

#[cfg(test)]
mod verifier_tests {
    use egonlang_core::ast::Module;
    use pretty_assertions::assert_eq;

    use super::Verifier;

    #[test]
    fn works_with_empty_module() {
        let mut verifier = Verifier::new();
        let module = Module::default();
        let results = verifier.verify(&module);

        assert_eq!(Ok(()), results);
    }
}
