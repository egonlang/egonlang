use egonlang_core::{ast::Expr, errors::TypeError, span::Span};

use crate::{expr_rule, type_env::TypeEnv, verifier::VerificationResult};

use crate::verify_trace;

expr_rule!(
    TypeMismatchIfthenElseExprRule,
    fn (expr: &Expr, _span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Expr::If(if_expr) = expr {
            verify_trace!("Verifying if expression then/else types: {expr}");

            let (then_expr, then_span) = &if_expr.then;
            let then_typeref = types.resolve_expr_type(then_expr, then_span).unwrap();

            if let Some((else_expr, else_span)) = &if_expr.else_ {
                let else_typeref = types.resolve_expr_type(else_expr, else_span).unwrap();

                if then_typeref != else_typeref {
                    verify_trace!(error: "then and else branches types don't match {then_typeref:?} vs {else_typeref:?} {expr}");

                    errs.push((
                        TypeError::MismatchType {
                            expected: then_typeref.to_string(),
                            actual: else_typeref.to_string(),
                        }
                        .into(),
                        else_span.clone(),
                    ));
                }
            }
        };

        errs
    }
);
