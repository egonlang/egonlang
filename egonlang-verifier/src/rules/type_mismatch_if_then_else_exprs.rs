use egonlang_core::{errors::TypeError, prelude::*};

use crate::prelude::*;

expr_rule!(
    /// Checks that both branches of an if/else expression/statement return the same type
    ///
    /// ```egon
    /// if (true) { 123 } else { "foo" }; // TypeError
    /// if (true) { 123 } else { }; // TypeError
    /// if (true) { 123 } else { 456 };
    /// ```
    TypeMismatchIfthenElseExpr,
    fn (expr: &Expr, _span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Expr::If(if_expr) = expr {
            verify_trace!("Verifying if expression then/else types: {expr}");

            let (then_expr, then_span) = &if_expr.then;
            let then_typeref = types.resolve_expr_type(then_expr, then_span).unwrap();

            if let Some((else_expr, else_span)) = &if_expr.else_ {
                let else_typeref = types.resolve_expr_type(else_expr, else_span).unwrap();

                if then_typeref != else_typeref {
                    verify_trace!(error: "then and else branches types don't match {then_typeref:?} vs {else_typeref:?} {expr}");

                    errs.push((
                        TypeError::MismatchType {
                            expected: then_typeref.to_string(),
                            actual: else_typeref.to_string(),
                        }
                        .into(),
                        else_span.clone(),
                    ));
                }
            }
        };

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::TypeMismatchIfthenElseExprRule;
    use crate::{prelude::*, verifier_rule_test};
    use egonlang_core::{errors::TypeError, prelude::*};
    use pretty_assertions::assert_eq;

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

    #[test]
    #[ignore = "todo"]
    fn returns_ok_if_both_branches_return_lists_of_same_type_using_typed_list_and_empty_list() {
        let expr: Expr = r#"if (true) { [1, 2, 3] } else { [] };"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Ok(()),
            TypeMismatchIfthenElseExprRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    #[ignore = "todo"]
    fn returns_ok_if_both_branches_return_lists_of_same_type_using_empty_list_and_typed_list() {
        let expr: Expr = r#"if (true) { [] } else { [1, 2, 3] };"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Ok(()),
            TypeMismatchIfthenElseExprRule.visit_expr(&expr, &span, &mut types)
        );
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
            TypeError::MismatchType {
                expected: "list<number>".to_string(),
                actual: "string".to_string()
            }
            .into(),
            29..38
        )])
    }

    #[test]
    #[ignore = "todo"]
    fn returns_err_if_the_elseif_branches_return_different_types() {
        let expr: Expr = r#"if (true) { [1, 2, 3] } else if (true) { 123 } else { "foo" };"#
            .try_into()
            .unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![
                (
                    TypeError::MismatchType {
                        expected: "list<number>".to_string(),
                        actual: "number".to_string()
                    }
                    .into(),
                    29..61
                ),
                (
                    TypeError::MismatchType {
                        expected: "number".to_string(),
                        actual: "string".to_string()
                    }
                    .into(),
                    54..59
                )
            ]),
            TypeMismatchIfthenElseExprRule.visit_expr(&expr, &span, &mut types)
        );
    }
}
