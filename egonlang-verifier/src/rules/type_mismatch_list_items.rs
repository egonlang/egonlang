use egonlang_core::{
    ast::{Expr, ExprS, Stmt},
    errors::TypeError,
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;

pub struct TypeMisMatchListItemsRule;
impl<'a> Rule<'a> for TypeMisMatchListItemsRule {
    fn visit_stmt(
        &self,
        _stmt: &Stmt,
        _span: &Span,
        _types: &mut TypeEnv<'a>,
    ) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr, _span: &Span, types: &mut TypeEnv<'a>) -> VerificationResult {
        let mut errs = vec![];

        match expr {
            Expr::List(expr_list) => {
                let items = &expr_list.items;

                if !items.is_empty() {
                    let (first_item_expr, first_item_span) = items.first().unwrap();
                    let first_item_typeref =
                        types.resolve_expr_type(first_item_expr, first_item_span)?;

                    let remaining_items: Vec<ExprS> = items.clone().into_iter().skip(1).collect();

                    for (item, item_span) in &remaining_items {
                        let item_typeref = types.resolve_expr_type(item, item_span)?;

                        if item_typeref != first_item_typeref {
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
                }
            }
            _ => {}
        };

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(())
    }
}

#[cfg(test)]
mod type_mismatch_negate_prefix_tests {
    use egonlang_core::{
        ast::{Expr, ExprList, ExprLiteral, TypeRef},
        errors::TypeError,
    };
    use pretty_assertions::assert_eq;

    use crate::{rules::rule::Rule, type_env::TypeEnv};

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
