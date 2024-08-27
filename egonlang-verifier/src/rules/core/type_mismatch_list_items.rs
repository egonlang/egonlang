use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;

expr_rule!(
    /// Checks that all items of a list are of the same type
    ///
    /// ```egon
    /// [1, 2, "a"]; // TypeError
    /// [true, false, 0]; // TypeError
    /// ["a", "b", "c"];
    /// ```
    TypeMisMatchListItems,
    |expr, _span, _resolve_ident, resolve_expr| {
        let mut errs = vec![];

        if let ast::Expr::List(expr_list) = expr {
            verify_trace!(
                "Verifying list expression for matching item types: {}",
                expr.to_string().cyan()
            );

            let items = &expr_list.items;

            if !items.is_empty() {
                let (first_item_expr, first_item_span) = items.first().unwrap();
                let first_item_typeref = resolve_expr(first_item_expr, first_item_span)
                    .unwrap();

                let remaining_items: Vec<span::Spanned<ast::Expr>> = items.clone().into_iter().skip(1).collect();

                for (item, item_span) in &remaining_items {
                    if let Some(item_typeref) = resolve_expr(item, item_span) {
                        if item_typeref.typeref != first_item_typeref.typeref {
                            verify_trace!(error:
                                "Found {} in a list of {}",
                                item_typeref.typeref.to_string().yellow(),
                                first_item_typeref.typeref.to_string().yellow()
                            );

                            errs.push((
                                EgonTypeError::MismatchType {
                                    expected: first_item_typeref.typeref.to_string(),
                                    actual: item_typeref.typeref.to_string(),
                                }
                                .into(),
                                item_span.clone(),
                            ));
                        }
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
    use egonlang_errors::EgonTypeError;
    use egonlang_types::Type;

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
                EgonTypeError::MismatchType {
                    expected: Type::number().to_string(),
                    actual: Type::bool().to_string()
                }
                .into(),
                5..10
            ),
            (
                EgonTypeError::MismatchType {
                    expected: Type::number().to_string(),
                    actual: Type::string().to_string()
                }
                .into(),
                12..17
            )
        ])
    );
}
