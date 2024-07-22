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
mod testss {
    use super::TypeMismatchIfCondExprRule;
    use crate::verifier_rule_test;
    use egonlang_core::errors::TypeError;

    verifier_rule_test!(
        TypeMismatchIfCondExprRule,
        returns_ok_if_condition_expr_is_bool_true,
        r#"if (true) {};"#
    );

    verifier_rule_test!(
        TypeMismatchIfCondExprRule,
        returns_ok_ifelse_condition_expr_is_bool_true,
        r#"if (true) {} else {};"#
    );

    verifier_rule_test!(
        TypeMismatchIfCondExprRule,
        returns_ok_if_condition_expr_is_bool_false,
        r#"if (false) {};"#
    );

    verifier_rule_test!(
        TypeMismatchIfCondExprRule,
        returns_ok_ifelse_condition_expr_is_bool_false,
        r#"if (false) {} else {};"#
    );

    verifier_rule_test!(
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_number,
        r#"if (123) {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            4..7
        )])
    );

    verifier_rule_test!(
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_number,
        r#"if (123) {} else {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            4..7
        )])
    );

    verifier_rule_test!(
        TypeMismatchIfCondExprRule,
        returns_err_if_condition_expr_is_number_is_zero,
        r#"if (0) {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            4..5
        )])
    );

    verifier_rule_test!(
        TypeMismatchIfCondExprRule,
        returns_err_ifelse_condition_expr_is_number_is_zero,
        r#"if (0) {} else {};"#,
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            4..5
        )])
    );
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::prelude::*;
    use egonlang_core::{errors::TypeError, prelude::*};

    use super::TypeMismatchIfCondExprRule;

    #[test]
    #[ignore = "todo"]
    fn returns_err_elif_condition_expr_is_number() {
        let expr: Expr = r#"if (true) {} else if (123) {} else {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "number".to_string()
                }
                .into(),
                22..25
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_condition_expr_is_empty_string() {
        let expr: Expr = r#"if ("") {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                4..6
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_ifelse_condition_expr_is_empty_string() {
        let expr: Expr = r#"if ("") {} else {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                4..6
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_condition_expr_is_string() {
        let expr: Expr = r#"if ("foo") {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                4..9
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_ifelse_condition_expr_is_string() {
        let expr: Expr = r#"if ("foo") {} else {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                4..9
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_condition_expr_is_string_of_false() {
        let expr: Expr = r#"if ("false") {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                4..11
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_ifelse_condition_expr_is_string_of_false() {
        let expr: Expr = r#"if ("false") {} else {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                4..11
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_condition_expr_is_unit() {
        let expr: Expr = r#"if (()) {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "()".to_string()
                }
                .into(),
                4..6
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_ifelse_condition_expr_is_unit() {
        let expr: Expr = r#"if (()) {} else {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "()".to_string()
                }
                .into(),
                4..6
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_condition_expr_is_empty_list() {
        let expr: Expr = r#"if ([]) {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "list<unknown>".to_string()
                }
                .into(),
                4..6
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_ifelse_condition_expr_is_empty_list() {
        let expr: Expr = r#"if ([]) {} else {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "list<unknown>".to_string()
                }
                .into(),
                4..6
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_condition_expr_is_list_of_bools() {
        let expr: Expr = r#"if ([true, false]) {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "list<bool>".to_string()
                }
                .into(),
                4..17
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_ifelse_condition_expr_is_list_of_bools() {
        let expr: Expr = r#"if ([true, false]) {} else {};"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "list<bool>".to_string()
                }
                .into(),
                4..17
            )]),
            TypeMismatchIfCondExprRule.visit_expr(&expr, &span, &mut types)
        );
    }
}
