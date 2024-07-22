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
mod tests {
    use super::TypeMisMatchListItemsRule;
    use crate::verifier_rule_test;
    use egonlang_core::{errors::TypeError, prelude::*};

    verifier_rule_test!(
        TypeMisMatchListItemsRule,
        returns_ok_if_list_is_empty,
        "[];"
    );

    verifier_rule_test!(
        TypeMisMatchListItemsRule,
        returns_ok_if_list_has_one_item,
        "[10];"
    );

    verifier_rule_test!(
        TypeMisMatchListItemsRule,
        returns_ok_if_list_has_multiple_items_of_same_type,
        "[10, 100];"
    );

    verifier_rule_test!(
        TypeMisMatchListItemsRule,
        returns_err_if_list_has_multiple_items_of_different_types,
        r#"[10, false, "foo"];"#,
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
        ])
    );
}
