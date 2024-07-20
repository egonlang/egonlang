use egonlang_core::{
    ast::{Expr, Stmt, TypeRef},
    errors::{ErrorS, TypeError},
    span::Span,
};

use crate::{rule, type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;
use crate::verify_trace;

rule!(
    TypeMismatchIfCondExprRule,
    fn visit_expr(expr: &Expr, _span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Expr::If(if_expr) = expr {
            verify_trace!(
                "Verifying if expression condition: {}",
                expr.to_string().cyan()
            );

            let (cond_expr, cond_span) = &if_expr.cond;
            let cond_typeref = types.resolve_expr_type(cond_expr, cond_span).unwrap();

            if cond_typeref != TypeRef::bool() {
                verify_trace!(error:
                    "condition expr expected to be a {} but was a {}",
                    "bool".to_string().yellow().italic(),
                    cond_typeref.to_string().yellow().italic()
                );

                errs.push((
                    TypeError::MismatchType {
                        expected: TypeRef::bool().to_string(),
                        actual: cond_typeref.to_string(),
                    }
                    .into(),
                    cond_span.clone(),
                ));
            }
        };

        errs
    }
);
