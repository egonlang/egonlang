use egonlang_core::{ast::ExprS, errors::TypeError, prelude::*};

use crate::prelude::*;

expr_rule!(
    /// Checks that all items of a list are of the same type
    ///
    /// ```egon
    /// [1, 2, "a"]; // TypeError
    /// [true, false, 0]; // TypeError
    /// ["a", "b", "c"];
    /// ```
    TypeMisMatchListItems,
    fn (expr: &Expr, _span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Expr::List(expr_list) = expr {
            verify_trace!(
                "Verifying list expression for matching item types: {}",
                expr.to_string().cyan()
            );

            let items = &expr_list.items;

            if !items.is_empty() {
                let (first_item_expr, first_item_span) = items.first().unwrap();
                let first_item_typeref = types
                    .resolve_expr_type(first_item_expr, first_item_span)
                    .unwrap();

                let remaining_items: Vec<ExprS> = items.clone().into_iter().skip(1).collect();

                for (item, item_span) in &remaining_items {
                    match types.resolve_expr_type(item, item_span) {
                        Ok(item_typeref) => {
                            if item_typeref != first_item_typeref {
                                verify_trace!(error:
                                    "Found {} in a list of {}",
                                    item_typeref.to_string().yellow(),
                                    first_item_typeref.to_string().yellow()
                                );

                                errs.push((
                                    TypeError::MismatchType {
                                        expected: first_item_typeref.to_string(),
                                        actual: item_typeref.to_string(),
                                    }
                                    .into(),
                                    item_span.clone(),
                                ));
                            }
                        }
                        Err(e) => errs.extend(e),
                    }
                }
            }
        };

        errs
    }
);

#[cfg(test)]
mod type_mismatch_negate_prefix_tests {
    use egonlang_core::{errors::TypeError, prelude::*};
    use pretty_assertions::assert_eq;

    use crate::prelude::*;

    use super::TypeMisMatchListItemsRule;

    #[test]
    fn returns_ok_if_list_is_empty() {
        let expr: Expr = "[];".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Ok(()),
            TypeMisMatchListItemsRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_ok_if_list_has_one_item() {
        let expr: Expr = "[10];".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Ok(()),
            TypeMisMatchListItemsRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_ok_if_list_has_multiple_items_of_same_type() {
        let expr: Expr = "[10, 100];".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Ok(()),
            TypeMisMatchListItemsRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_list_has_multiple_items_of_different_types() {
        let expr: Expr = r#"[10, false, "foo"];"#.try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![
                (
                    TypeError::MismatchType {
                        expected: TypeRef::number().to_string(),
                        actual: TypeRef::bool().to_string()
                    }
                    .into(),
                    5..10
                ),
                (
                    TypeError::MismatchType {
                        expected: TypeRef::number().to_string(),
                        actual: TypeRef::string().to_string()
                    }
                    .into(),
                    12..17
                )
            ]),
            TypeMisMatchListItemsRule.visit_expr(&expr, &span, &mut types)
        );
    }
}
