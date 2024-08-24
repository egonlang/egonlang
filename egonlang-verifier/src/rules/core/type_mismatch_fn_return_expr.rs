use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;

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
    |expr, _span, _resolve_ident, resolve_expr| {
        if let ast::Expr::Fn(fn_expr) = expr {
            verify_trace!(
                "Verifying fn return type and body expression: {}",
                expr.to_string().cyan()
            );

            let (fn_return_type_typeref, _) = &fn_expr.return_type;
            let (body_expr, body_span) = &fn_expr.body;

            return match resolve_expr(body_expr, body_span) {
                Some(body_typeref) => {
                    let mut errs = vec![];

                    if body_typeref.typeref != *fn_return_type_typeref {
                        verify_trace!(error:
                            "fn body type {} doesn't match fn return type {}",
                            body_typeref.typeref.to_string().yellow().italic(),
                            fn_return_type_typeref.to_string().yellow().italic()
                        );

                        errs.push((
                            EgonTypeError::MismatchType {
                                expected: fn_return_type_typeref.to_string(),
                                actual: body_typeref.typeref.to_string(),
                            }
                            .into(),
                            body_span.clone(),
                        ));
                    }

                    errs
                }
                None => vec![],
            };
        } else {
            vec![]
        }
    }
);

#[cfg(test)]
mod tests {
    use super::TypeMismatchFnReturnExprRule;
    use crate::verifier_rule_test;
    use egonlang_errors::EgonTypeError;

    verifier_rule_test!(
        TypeMismatchFnReturnExprRule,
        returns_ok_fn_body_type_matches_fn_return_type,
        r#"(): string => { "foo" };"#
    );

    verifier_rule_test!(
        TypeMismatchFnReturnExprRule,
        returns_err_fn_body_type_does_not_match_fn_return_type,
        r#"(): string => { 123 };"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "string".to_string(),
                actual: "number".to_string()
            }
            .into(),
            14..21
        )])
    );

    verifier_rule_test!(
        TypeMismatchFnReturnExprRule,
        returns_err_fn_body_type_does_not_match_fn_return_type_2,
        r#"(): string => { { 123 } };"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "string".to_string(),
                actual: "number".to_string()
            }
            .into(),
            14..25
        )])
    );

    verifier_rule_test!(
        TypeMismatchFnReturnExprRule,
        returns_err_fn_body_type_does_not_match_fn_return_type_3,
        r#"(): () => { { 123 } };"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }
            .into(),
            10..21
        )])
    );

    verifier_rule_test!(
        TypeMismatchFnReturnExprRule,
        returns_err_fn_body_type_does_not_match_fn_return_type_4,
        r#"(): string => { { 123; } };"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "string".to_string(),
                actual: "()".to_string()
            }
            .into(),
            14..26
        )])
    );
}
