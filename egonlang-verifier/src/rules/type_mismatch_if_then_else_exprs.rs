use egonlang_core::{
    ast::{Expr, Stmt},
    errors::TypeError,
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;
use crate::verify_trace;

pub struct TypeMismatchIfthenElseExprRule;
impl<'a> Rule<'a> for TypeMismatchIfthenElseExprRule {
    fn visit_stmt(&self, _stmt: &Stmt, _span: &Span, _types: &mut TypeEnv) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr, _span: &Span, types: &mut TypeEnv) -> VerificationResult {
        match expr {
            Expr::If(if_expr) => {
                verify_trace!("Verifying if expression then/else types: {expr}");

                let (then_expr, then_span) = &if_expr.then;
                let then_typeref = types.resolve_expr_type(then_expr, then_span)?;

                if let Some((else_expr, else_span)) = &if_expr.else_ {
                    let else_typeref = types.resolve_expr_type(else_expr, else_span)?;

                    if then_typeref != else_typeref {
                        verify_trace!("Error: then and else branches types don't match {then_typeref:?} vs {else_typeref:?} {expr}");

                        return Err(vec![(
                            TypeError::MismatchType {
                                expected: then_typeref.to_string(),
                                actual: else_typeref.to_string(),
                            }
                            .into(),
                            else_span.clone(),
                        )]);
                    }
                }
            }
            _ => {}
        };

        Ok(())
    }
}
