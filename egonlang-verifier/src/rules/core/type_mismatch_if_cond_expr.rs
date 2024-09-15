use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;
use egonlang_types::Type;

expr_rule!(
    /// Checks the condition expression of a if statement is a boolean
    ///
    /// ```egon
    /// if (123) {}; // TypeError
    /// if ("example") {} else {}; // TypeError
    /// ```
    TypeMismatchIfCondExpr,
    |expr_span, _resolve_ident, resolve_expr| {
        let (expr, _) = expr_span;

        let mut errs = vec![];

        if let ast::Expr::If(if_expr) = &*expr {
            let (cond_expr, cond_span) = &if_expr.cond;
            if let Some(cond_typeref) = resolve_expr.get(&(cond_expr.clone(), cond_span.clone())) {
                if *cond_typeref != Type::bool() {
                    errs.push((
                        EgonTypeError::MismatchType {
                            expected: Type::bool().to_string(),
                            actual: cond_typeref.to_string(),
                        }
                        .into(),
                        cond_span.clone(),
                    ));
                }
            }
        };

        errs
    }
);

#[cfg(test)]
mod tests {
    use egonlang_errors::EgonTypeError;

    use super::TypeMismatchIfCondExprRule;
    use crate::verifier_rule_test;

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_elif_condition_expr_is_number,
        r#"if (true) {} else if (123) {} else {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            21..26
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_empty_string,
        r#"if ("") {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            3..7
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_empty_string,
        r#"if ("") {} else {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            3..7
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_string,
        r#"if ("foo") {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            3..10
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_string,
        r#"if ("foo") {} else {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            3..10
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_string_of_false,
        r#"if ("false") {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            3..12
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_string_of_false,
        r#"if ("false") {} else {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            3..12
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_unit,
        r#"if (()) {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "()".to_string()
            }
            .into(),
            3..7
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_unit,
        r#"if (()) {} else {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "()".to_string()
            }
            .into(),
            3..7
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_empty_list,
        r#"if ([]) {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "list<unknown>".to_string()
            }
            .into(),
            3..7
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_empty_list,
        r#"if ([]) {} else {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "list<unknown>".to_string()
            }
            .into(),
            3..7
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_list_of_bools,
        r#"if ([true, false]) {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "list<bool>".to_string()
            }
            .into(),
            3..18
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_list_of_bools,
        r#"if ([true, false]) {} else {};"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "list<bool>".to_string()
            }
            .into(),
            3..18
        )])
    }
}
