use egonlang_core::{
    ast::{Expr, Stmt, TypeRef},
    errors::TypeError,
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;
use crate::verify_trace;

pub struct TypeMismatchIfCondExprRule;
impl<'a> Rule<'a> for TypeMismatchIfCondExprRule {
    fn visit_stmt(&self, _stmt: &Stmt, _span: &Span, _types: &mut TypeEnv) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr, _span: &Span, types: &mut TypeEnv) -> VerificationResult {
        if let Expr::If(if_expr) = expr {
            verify_trace!("Verifying if expression cond: {expr}");

            let (cond_expr, cond_span) = &if_expr.cond;
            let cond_typeref = types.resolve_expr_type(cond_expr, cond_span)?;

            if cond_typeref != TypeRef::bool() {
                return Err(vec![(
                    TypeError::MismatchType {
                        expected: TypeRef::bool().to_string(),
                        actual: cond_typeref.to_string(),
                    }
                    .into(),
                    cond_span.clone(),
                )]);
            }
        };

        Ok(())
    }
}
