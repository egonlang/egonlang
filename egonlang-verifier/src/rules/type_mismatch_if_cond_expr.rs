use crate::prelude::*;
use egonlang_core::{errors::TypeError, prelude::*};

expr_rule!(
    /// Checks the condition expression of a if statement is a boolean
    ///
    /// ```egon
    /// if (123) {}; // TypeError
    /// if ("example") {} else {}; // TypeError
    /// ```
    TypeMismatchIfCondExpr,
    fn (expr: &Expr, _span: &Span, types: &mut TypeEnv) {
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
