use egonlang_core::{
    ast::{ExprInfix, OpInfix},
    errors::{ErrorS, TypeError},
    prelude::*,
};

use crate::prelude::*;

expr_rule!(
    /// Checks value types for all infix operation expressions
    /// e.g. `+, -, /, *, <, <=, >, >=, ==, !=`
    ///
    /// ```egon
    /// 1 + "foo"; // TypeError
    /// 1 + 2;
    /// ```
    TypeMismatchInfix,
    fn (expr: &Expr, _span: &Span, types: &mut TypeEnv) {
        let mut errs: Vec<ErrorS> = vec![];

        if let Expr::Infix(infix) = expr {
            verify_trace!("Verifying infix expression: {}", expr.to_string().cyan());

            match infix.op {
                OpInfix::Greater => {
                    let infix_errs = validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::GreaterEqual => {
                    let infix_errs = validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Less => {
                    let infix_errs = validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::LessEqual => {
                    let infix_errs = validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Add => {
                    let infix_errs = validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Subtract => {
                    let infix_errs = validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Multiply => {
                    let infix_errs = validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Divide => {
                    let infix_errs = validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::LogicAnd => {
                    let infix_errs = validate_infix_types(infix, TypeRef::bool(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::LogicOr => {
                    let infix_errs = validate_infix_types(infix, TypeRef::bool(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Modulus => {
                    let infix_errs = validate_infix_types(infix, TypeRef::number(), types)
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
    infix: &ExprInfix,
    expected_type: TypeRef,
    types: &mut TypeEnv,
) -> Result<(), Vec<ErrorS>> {
    let mut errs = vec![];

    let (lt_expr, lt_span) = &infix.lt;
    let lt_type = types.resolve_expr_type(lt_expr, lt_span)?;

    let (rt_expr, rt_span) = &infix.rt;
    let rt_type = types.resolve_expr_type(rt_expr, rt_span)?;

    if lt_type != expected_type {
        verify_trace!(error: "infix operation received non number: {lt_expr}");

        errs.push((
            TypeError::MismatchType {
                expected: expected_type.to_string(),
                actual: lt_type.to_string(),
            }
            .into(),
            lt_span.clone(),
        ));
    }
    if rt_type != expected_type {
        verify_trace!(error: "infix operation received non number: {rt_expr}");

        errs.push((
            TypeError::MismatchType {
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
    use egonlang_core::{errors::TypeError, prelude::*};
    use pretty_assertions::assert_eq;

    use crate::prelude::*;

    use super::TypeMismatchInfixRule;

    #[test]
    fn returns_ok_if_infix_gt_values_are_numbers() {
        let expr: Expr = "10 > 100;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Ok(()),
            TypeMismatchInfixRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_infix_gt_values_are_not_numbers_1() {
        let expr: Expr = "true > 100;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::bool().to_string()
                }
                .into(),
                0..4
            )]),
            TypeMismatchInfixRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_infix_gt_values_are_not_numbers_2() {
        let expr: Expr = "10 > false;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::bool().to_string()
                }
                .into(),
                5..10
            )]),
            TypeMismatchInfixRule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_infix_gt_values_are_not_numbers_3() {
        let expr: Expr = "false > false;".try_into().unwrap();
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
                    0..5
                ),
                (
                    TypeError::MismatchType {
                        expected: TypeRef::number().to_string(),
                        actual: TypeRef::bool().to_string()
                    }
                    .into(),
                    8..13
                )
            ]),
            TypeMismatchInfixRule.visit_expr(&expr, &span, &mut types)
        );
    }
}
