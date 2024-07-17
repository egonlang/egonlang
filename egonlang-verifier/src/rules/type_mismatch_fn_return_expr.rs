use egonlang_core::{
    ast::{Expr, Stmt},
    errors::TypeError,
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;
use crate::verify_trace;

pub struct TypeMismatchFnReturnExprRule;
impl<'a> Rule<'a> for TypeMismatchFnReturnExprRule {
    fn visit_stmt(&self, _stmt: &Stmt, _span: &Span, _types: &mut TypeEnv) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr, _span: &Span, types: &mut TypeEnv) -> VerificationResult {
        if let Expr::Fn(fn_expr) = expr {
            verify_trace!(
                "Verifying fn return type and body expression: {}",
                expr.to_string().cyan()
            );

            let (fn_return_type_typeref, _) = &fn_expr.return_type;
            let (body_expr, body_span) = &fn_expr.body;
            let body_typeref = types.resolve_expr_type(body_expr, body_span)?;

            if body_typeref != *fn_return_type_typeref {
                verify_trace!(error:
                    "fn body type {} doesn't match fn return type {}",
                    body_typeref.to_string().yellow().italic(),
                    fn_return_type_typeref.to_string().yellow().italic()
                );

                return Err(vec![(
                    TypeError::MismatchType {
                        expected: fn_return_type_typeref.to_string(),
                        actual: body_typeref.to_string(),
                    }
                    .into(),
                    body_span.clone(),
                )]);
            }
        }

        Ok(())
    }
}
