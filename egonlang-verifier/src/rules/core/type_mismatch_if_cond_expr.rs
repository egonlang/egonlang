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

#[cfg(test)]
mod tests {
    use super::TypeMismatchIfCondExprRule;
    use crate::verifier_rule_test;
    use egonlang_core::errors::TypeError;

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_elif_condition_expr_is_number,
        r#"if (true) {} else if (123) {} else {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            22..25
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_empty_string,
        r#"if ("") {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            4..6
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_empty_string,
        r#"if ("") {} else {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            4..6
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_string,
        r#"if ("foo") {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            4..9
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_string,
        r#"if ("foo") {} else {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            4..9
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_string_of_false,
        r#"if ("false") {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            4..11
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_string_of_false,
        r#"if ("false") {} else {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            4..11
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_unit,
        r#"if (()) {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "()".to_string()
            }
            .into(),
            4..6
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_unit,
        r#"if (()) {} else {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "()".to_string()
            }
            .into(),
            4..6
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_empty_list,
        r#"if ([]) {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "list<unknown>".to_string()
            }
            .into(),
            4..6
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_empty_list,
        r#"if ([]) {} else {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "list<unknown>".to_string()
            }
            .into(),
            4..6
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_list_of_bools,
        r#"if ([true, false]) {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "list<bool>".to_string()
            }
            .into(),
            4..17
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_list_of_bools,
        r#"if ([true, false]) {} else {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "list<bool>".to_string()
            }
            .into(),
            4..17
        )])
    }
}
