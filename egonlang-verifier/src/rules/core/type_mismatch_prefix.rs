use crate::prelude::*;
use egonlang_core::prelude::*;

expr_rule!(
    /// Checks value types for all prefix operation expressions
    /// e.g. `!, -`
    ///
    /// ```egon
    /// !0 // TypeError
    /// -false // TypeError
    /// -10
    /// !true
    /// ```
    TypeMismatchPrefix,
    fn (expr: &ast::Expr, _span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let ast::Expr::Prefix(prefix_expr) = expr {
            verify_trace!("Verifying prefix expression: {expr}");

            match prefix_expr.op {
                ast::OpPrefix::Negate => {
                    let (value_expr, value_span) = &prefix_expr.rt;
                    let value_typeref = types.resolve_expr_type(value_expr, value_span).unwrap();

                    if value_typeref != ast::TypeRef::number() {
                        verify_trace!(error: "negate prefix on a none number value: {expr}");
                        errs.push((
                            EgonTypeError::MismatchType {
                                expected: ast::TypeRef::number().to_string(),
                                actual: value_typeref.to_string(),
                            }
                            .into(),
                            value_span.clone(),
                        ));
                    }
                }
                ast::OpPrefix::Not => {
                    let (value_expr, value_span) = &prefix_expr.rt;
                    let value_typeref = types.resolve_expr_type(value_expr, value_span).unwrap();

                    if value_typeref != ast::TypeRef::bool() {
                        verify_trace!(error: "not prefix on a none bool value: {expr}");
                        errs.push((
                            EgonTypeError::MismatchType {
                                expected: ast::TypeRef::bool().to_string(),
                                actual: value_typeref.to_string(),
                            }
                            .into(),
                            value_span.clone(),
                        ));
                    }
                }
            }
        };

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::TypeMismatchPrefixRule;
    use crate::verifier_rule_test;
    use egonlang_core::prelude::*;

    verifier_rule_test! {
        TypeMismatchPrefixRule,
        returns_ok_if_negate_prefix_on_number_literal_expr,
        "-100;"
    }

    verifier_rule_test! {
        TypeMismatchPrefixRule,
        returns_err_if_negate_prefix_on_bool_literal_expr,
        "-true;",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: ast::TypeRef::number().to_string(),
                actual: ast::TypeRef::bool().to_string()
            }
            .into(),
            1..5
        )])
    }

    verifier_rule_test! {
        TypeMismatchPrefixRule,
        returns_ok_if_not_prefix_on_number_literal_expr,
        "!false;"
    }

    verifier_rule_test! {
        TypeMismatchPrefixRule,
        returns_err_if_not_prefix_on_number_literal_expr,
        "!123;",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: ast::TypeRef::bool().to_string(),
                actual: ast::TypeRef::number().to_string()
            }
            .into(),
            1..4
        )])
    }

    verifier_rule_test! {
        TypeMismatchPrefixRule,
        returns_err_if_not_prefix_on_string_literal_expr,
        r#"!"test";"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: ast::TypeRef::bool().to_string(),
                actual: ast::TypeRef::string().to_string()
            }
            .into(),
            1..7
        )])
    }
}
