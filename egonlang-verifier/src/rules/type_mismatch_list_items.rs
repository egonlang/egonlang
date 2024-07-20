use egonlang_core::{ast::ExprS, errors::TypeError, prelude::*};

use crate::prelude::*;

expr_rule!(
    TypeMisMatchListItemsRule,
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
    use egonlang_core::{
        ast::{ExprList, ExprLiteral},
        errors::TypeError,
        prelude::*,
    };
    use pretty_assertions::assert_eq;

    use crate::prelude::*;

    use super::TypeMisMatchListItemsRule;

    #[test]
    fn returns_ok_if_list_is_empty() {
        let rule = TypeMisMatchListItemsRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprList { items: vec![] }.into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_expr(&expr, &span, &mut types));
    }

    #[test]
    fn returns_ok_if_list_has_one_item() {
        let rule = TypeMisMatchListItemsRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprList {
            items: vec![(ExprLiteral::Number(10f64).into(), 0..0)],
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_expr(&expr, &span, &mut types));
    }

    #[test]
    fn returns_ok_if_list_has_multiple_items_of_same_type() {
        let rule = TypeMisMatchListItemsRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprList {
            items: vec![
                (ExprLiteral::Number(10f64).into(), 0..0),
                (ExprLiteral::Number(100f64).into(), 0..0),
            ],
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_expr(&expr, &span, &mut types));
    }

    #[test]
    fn returns_ok_if_list_has_multiple_items_of_different_types() {
        let rule = TypeMisMatchListItemsRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprList {
            items: vec![
                (ExprLiteral::Number(10f64).into(), 0..1),
                (ExprLiteral::Bool(false).into(), 2..3),
                (ExprLiteral::String("foo".to_string()).into(), 4..7),
            ],
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![
                (
                    TypeError::MismatchType {
                        expected: TypeRef::number().to_string(),
                        actual: TypeRef::bool().to_string()
                    }
                    .into(),
                    2..3
                ),
                (
                    TypeError::MismatchType {
                        expected: TypeRef::number().to_string(),
                        actual: TypeRef::string().to_string()
                    }
                    .into(),
                    4..7
                )
            ]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }
}
