use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;

expr_rule!(
    /// Checks that both branches of an if/else expression/statement return the same type
    ///
    /// ```egon
    /// if (true) { 123 } else { "foo" }; // TypeError
    /// if (true) { 123 } else { }; // TypeError
    /// if (true) { 123 } else { 456 };
    /// ```
    TypeMismatchIfthenElseExpr,
    |expr, _span, _resolve_ident, resolve_expr| {
        let mut errs = vec![];

        if let ast::Expr::If(if_expr) = expr {
            let (then_expr, then_span) = &if_expr.then;
            let then_typeref = resolve_expr(then_expr, then_span).unwrap().typeref;

            if let Some((else_expr, else_span)) = &if_expr.else_ {
                let else_typeref = resolve_expr(else_expr, else_span).unwrap().typeref;

                if then_typeref != else_typeref {
                    if then_typeref.is_list() && else_typeref.is_list() {
                        if then_typeref.is_known_list() && else_typeref.is_known_list() {
                            errs.push((
                                EgonTypeError::MismatchType {
                                    expected: then_typeref.to_string(),
                                    actual: else_typeref.to_string(),
                                }
                                .into(),
                                else_span.clone(),
                            ));
                        }
                    } else {
                        errs.push((
                            EgonTypeError::MismatchType {
                                expected: then_typeref.to_string(),
                                actual: else_typeref.to_string(),
                            }
                            .into(),
                            else_span.clone(),
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
    use super::TypeMismatchIfthenElseExprRule;
    use crate::verifier_rule_test;
    use egonlang_errors::EgonTypeError;

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_both_branches_return_numbers,
        r#"if (true) { 123 } else { 456 };"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_both_branches_return_bools,
        r#"if (true) { true } else { false };"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_both_branches_return_units,
        r#"if (true) { () } else { () };"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_both_branches_return_implicit_units,
        r#"if (true) {} else {};"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_both_branches_return_lists_of_same_type,
        r#"if (true) { [1, 2, 3] } else { [4, 5, 6] };"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_all_branches_of_elseif_return_lists_of_same_type,
        r#"if (true) { [1, 2, 3] } else if (true) { [4, 5, 6] } else { [7, 8, 9] };"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_all_branches_of_elseif_elseif_return_lists_of_same_type,
        r#"if (true) { [1, 2, 3] } else if (true) { [4, 5, 6] } else if (true) { [7, 8, 9] } else { [10, 11, 12] };"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_both_branches_return_lists_of_same_type_using_typed_list_and_empty_list,
        r#"if (true) { [1, 2, 3] } else { [] };"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_both_branches_return_lists_of_same_type_using_empty_list_and_typed_list,
        r#"if (true) { [] } else { [1, 2, 3] };"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_ok_if_both_branches_return_lists_of_same_type_using_empty_lists,
        r#"if (true) { [] } else { [] };"#
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_err_if_the_else_branch_returns_different_type,
        r#"if (true) { [1, 2, 3] } else { "foo" };"#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "list<number>".to_string(),
                actual: "string".to_string()
            }
            .into(),
            29..38
        )])
    }

    verifier_rule_test! {
        TypeMismatchIfthenElseExprRule,
        returns_err_if_the_elseif_branches_return_different_types,
        r#"if (true) { [1, 2, 3] } else if (true) { 123 } else { "foo" };"#,
        Err(vec![
            (
                EgonTypeError::MismatchType {
                    expected: "list<number>".to_string(),
                    actual: "number".to_string()
                }
                .into(),
                29..61
            ),
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                52..61
            )
        ])
    }
}
