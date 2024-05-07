use egonlang_core::ast::{Expr, ExprInfix, OpInfix, TypeRef};
use egonlang_core::{ast::Stmt, errors::TypeError, span::Span};

use crate::verify_trace;
use crate::{type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;

pub struct TypeMismatchInfixRule;

impl TypeMismatchInfixRule {
    fn validate_infix_types(
        &self,
        infix: &ExprInfix,
        expected_type: TypeRef,
        types: &mut TypeEnv,
    ) -> VerificationResult {
        let mut errs = vec![];

        let (lt_expr, lt_span) = &infix.lt;
        let lt_type = types.resolve_expr_type(lt_expr, lt_span)?;

        let (rt_expr, rt_span) = &infix.rt;
        let rt_type = types.resolve_expr_type(rt_expr, rt_span)?;

        if lt_type != expected_type {
            verify_trace!("Error: infix operation received non number: {lt_expr}");

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
            verify_trace!("Error: infix operation received non number: {rt_expr}");

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
}

impl<'a> Rule<'a> for TypeMismatchInfixRule {
    fn visit_stmt(&self, _stmt: &Stmt, _span: &Span, _types: &mut TypeEnv) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr, _span: &Span, types: &mut TypeEnv) -> VerificationResult {
        if let Expr::Infix(infix) = expr {
            let mut errs = vec![];

            verify_trace!("Verifying infix expression: {expr}");

            match infix.op {
                OpInfix::Greater => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::GreaterEqual => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Less => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::LessEqual => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Add => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Subtract => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Multiply => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::Divide => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::number(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::LogicAnd => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::bool(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                OpInfix::LogicOr => {
                    let infix_errs = self
                        .validate_infix_types(infix, TypeRef::bool(), types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(infix_errs);
                }
                _ => {}
            };

            if !errs.is_empty() {
                return Err(errs);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod type_mismatch_infix_tests {
    use egonlang_core::{
        ast::{Expr, ExprInfix, ExprLiteral, OpInfix, TypeRef},
        errors::TypeError,
    };
    use pretty_assertions::assert_eq;

    use crate::{rules::rule::Rule, type_env::TypeEnv};

    use super::TypeMismatchInfixRule;

    #[test]
    fn returns_ok_if_infix_gt_values_are_numbers() {
        let rule = TypeMismatchInfixRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprInfix {
            lt: (ExprLiteral::Number(10f64).into(), 0..0),
            op: OpInfix::Greater,
            rt: (ExprLiteral::Number(100f64).into(), 0..0),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_expr(&expr, &span, &mut types));
    }

    #[test]
    fn returns_err_if_infix_gt_values_are_not_numbers_1() {
        let rule = TypeMismatchInfixRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprInfix {
            lt: (ExprLiteral::Bool(true).into(), 0..1),
            op: OpInfix::Greater,
            rt: (ExprLiteral::Number(100f64).into(), 1..2),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::bool().to_string()
                }
                .into(),
                0..1
            )]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_infix_gt_values_are_not_numbers_2() {
        let rule = TypeMismatchInfixRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprInfix {
            lt: (ExprLiteral::Number(10f64).into(), 0..1),
            op: OpInfix::Greater,
            rt: (ExprLiteral::Bool(false).into(), 1..2),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::bool().to_string()
                }
                .into(),
                1..2
            )]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_infix_gt_values_are_not_numbers_3() {
        let rule = TypeMismatchInfixRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprInfix {
            lt: (ExprLiteral::Bool(false).into(), 0..1),
            op: OpInfix::Greater,
            rt: (ExprLiteral::Bool(false).into(), 1..2),
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
                    0..1
                ),
                (
                    TypeError::MismatchType {
                        expected: TypeRef::number().to_string(),
                        actual: TypeRef::bool().to_string()
                    }
                    .into(),
                    1..2
                )
            ]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }
}
