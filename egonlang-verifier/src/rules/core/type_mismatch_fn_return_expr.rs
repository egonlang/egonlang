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
        if let ast::Expr::Fn(fn_expr) = &*expr {
            let (fn_return_type_typeref, _) = &fn_expr.return_type;
            let (body_expr, body_span) = &fn_expr.body;

            if let Some(body_typeref) = resolve_expr.get(&(body_expr.clone(), body_span.clone())) {
                let mut errs = vec![];

                if !body_typeref.is_unknown() && body_typeref != fn_return_type_typeref {
                    errs.push((
                        EgonTypeError::MismatchType {
                            expected: fn_return_type_typeref.to_string(),
                            actual: body_typeref.to_string(),
                        }
                        .into(),
                        body_span.clone(),
                    ));
                }

                errs
            } else {
                vec![]
            }
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
