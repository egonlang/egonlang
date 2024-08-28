use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::{EgonErrorS, EgonTypeError};
use egonlang_types::Type;
use rules::rule::ResolveExpr;

expr_rule!(
    /// Checks value types for all infix operation expressions
    /// e.g. `+, -, /, *, <, <=, >, >=, ==, !=`
    ///
    /// ```egon
    /// 1 + "foo"; // TypeError
    /// 1 + 2;
    /// ```
    TypeMismatchInfix,
    |expr, _span, _resolve_ident, resolve_expr| {
        let mut errs: Vec<EgonErrorS> = vec![];

        if let ast::Expr::Infix(infix) = expr {
            match infix.op {
                ast::OpInfix::Greater => {
                    let infix_errs = validate_infix_types(infix, Type::number(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::GreaterEqual => {
                    let infix_errs = validate_infix_types(infix, Type::number(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::Less => {
                    let infix_errs = validate_infix_types(infix, Type::number(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::LessEqual => {
                    let infix_errs = validate_infix_types(infix, Type::number(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::Add => {
                    let infix_errs = validate_infix_types(infix, Type::number(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::Subtract => {
                    let infix_errs = validate_infix_types(infix, Type::number(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::Multiply => {
                    let infix_errs = validate_infix_types(infix, Type::number(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::Divide => {
                    let infix_errs = validate_infix_types(infix, Type::number(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::LogicAnd => {
                    let infix_errs = validate_infix_types(infix, Type::bool(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::LogicOr => {
                    let infix_errs = validate_infix_types(infix, Type::bool(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                ast::OpInfix::Modulus => {
                    let infix_errs = validate_infix_types(infix, Type::number(), resolve_expr)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                _ => {}
            };
        }

        errs
    }
);

fn validate_infix_types(
    infix: &ast::ExprInfix,
    expected_type: Type,
    resolve_expr: &dyn ResolveExpr,
) -> Result<(), Vec<EgonErrorS>> {
    let mut errs = vec![];

    let (lt_expr, lt_span) = &infix.lt;
    let lt_type = resolve_expr(lt_expr, lt_span).unwrap().of_type;

    let (rt_expr, rt_span) = &infix.rt;
    let rt_type = resolve_expr(rt_expr, rt_span).unwrap().of_type;

    if lt_type != expected_type {
        errs.push((
            EgonTypeError::MismatchType {
                expected: expected_type.to_string(),
                actual: lt_type.to_string(),
            }
            .into(),
            lt_span.clone(),
        ));
    }
    if rt_type != expected_type {
        errs.push((
            EgonTypeError::MismatchType {
                expected: expected_type.to_string(),
                actual: rt_type.to_string(),
            }
            .into(),
            rt_span.clone(),
        ));
    }

    if !errs.is_empty() {
        return Err(errs);
    }

    Ok(())
}
#[cfg(test)]
mod type_mismatch_infix_tests {
    use super::TypeMismatchInfixRule;
    use crate::verifier_rule_test;
    use egonlang_errors::EgonTypeError;
    use egonlang_types::Type;

    verifier_rule_test!(
        TypeMismatchInfixRule,
        returns_ok_if_infix_gt_values_are_numbers,
        "10 > 100;"
    );

    verifier_rule_test!(
        TypeMismatchInfixRule,
        returns_err_if_infix_gt_values_are_not_numbers_1,
        "true > 100;",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: Type::number().to_string(),
                actual: Type::bool().to_string()
            }
            .into(),
            0..4
        )])
    );

    verifier_rule_test!(
        TypeMismatchInfixRule,
        returns_err_if_infix_gt_values_are_not_numbers_2,
        "10 > false;",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: Type::number().to_string(),
                actual: Type::bool().to_string()
            }
            .into(),
            5..10
        )])
    );

    verifier_rule_test!(
        TypeMismatchInfixRule,
        returns_err_if_infix_gt_values_are_not_numbers_3,
        "false > false;",
        Err(vec![
            (
                EgonTypeError::MismatchType {
                    expected: Type::number().to_string(),
                    actual: Type::bool().to_string()
                }
                .into(),
                0..5
            ),
            (
                EgonTypeError::MismatchType {
                    expected: Type::number().to_string(),
                    actual: Type::bool().to_string()
                }
                .into(),
                8..13
            )
        ])
    );
}
