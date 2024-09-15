use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;
use egonlang_types::Type;

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
    |expr_span, _resolve_ident, resolve_expr| {
        let (expr, _) = expr_span;

        let mut errs = vec![];

        if let ast::Expr::Prefix(prefix_expr) = &*expr {
            match prefix_expr.op {
                ast::OpPrefix::Negate => {
                    let (value_expr, value_span) = &prefix_expr.rt;
                    let value_typeref = resolve_expr.get(&(value_expr.clone(), value_span.clone())).unwrap();

                    if *value_typeref != Type::number() {
                        errs.push((
                            EgonTypeError::MismatchType {
                                expected: Type::number().to_string(),
                                actual: value_typeref.to_string(),
                            }
                            .into(),
                            value_span.clone(),
                        ));
                    }
                }
                ast::OpPrefix::Not => {
                    let (value_expr, value_span) = &prefix_expr.rt;
                    let value_typeref = resolve_expr.get(&(value_expr.clone(), value_span.clone())).unwrap();

                    if *value_typeref != Type::bool() {
                        errs.push((
                            EgonTypeError::MismatchType {
                                expected: Type::bool().to_string(),
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
    use egonlang_errors::EgonTypeError;
    use egonlang_types::Type;

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
                expected: Type::number().to_string(),
                actual: Type::bool().to_string()
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
                expected: Type::bool().to_string(),
                actual: Type::number().to_string()
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
                expected: Type::bool().to_string(),
                actual: Type::string().to_string()
            }
            .into(),
            1..7
        )])
    }
}
