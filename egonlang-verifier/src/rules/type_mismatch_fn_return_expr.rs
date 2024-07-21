use egonlang_core::{errors::TypeError, prelude::*};

use crate::prelude::*;

expr_rule!(
    /// Checks the return type of a function matches function body's
    /// returning expression, if one is present.
    ///
    /// ```egon
    /// fn example1(): string => {
    ///     123 // TypeError
    /// }
    ///
    /// fn example2(): number => {
    ///     123
    /// }
    /// ```
    TypeMismatchFnReturnExpr,
    fn (expr: &Expr, _span: &Span, types: &mut TypeEnv) {
        if let Expr::Fn(fn_expr) = expr {
            verify_trace!(
                "Verifying fn return type and body expression: {}",
                expr.to_string().cyan()
            );

            let (fn_return_type_typeref, _) = &fn_expr.return_type;
            let (body_expr, body_span) = &fn_expr.body;

            return match types.resolve_expr_type(body_expr, body_span) {
                Ok(body_typeref) => {
                    let mut errs = vec![];

                    if body_typeref != *fn_return_type_typeref {
                        verify_trace!(error:
                            "fn body type {} doesn't match fn return type {}",
                            body_typeref.to_string().yellow().italic(),
                            fn_return_type_typeref.to_string().yellow().italic()
                        );

                        errs.push((
                            TypeError::MismatchType {
                                expected: fn_return_type_typeref.to_string(),
                                actual: body_typeref.to_string(),
                            }
                            .into(),
                            body_span.clone(),
                        ));
                    }

                    errs
                }
                Err(errs) => errs,
            };
        } else {
            vec![]
        }
    }
);
