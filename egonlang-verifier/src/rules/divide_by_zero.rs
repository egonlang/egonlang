use egonlang_core::{
    ast::{Expr, OpInfix, Stmt},
    errors::SyntaxError,
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;

pub struct DivideByZeroRule;
impl<'a> Rule<'a> for DivideByZeroRule {
    fn visit_stmt(
        &self,
        _stmt: &Stmt,
        _span: &Span,
        _types: &mut TypeEnv<'a>,
    ) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(
        &self,
        expr: &Expr,
        _span: &Span,
        _types: &mut TypeEnv<'a>,
    ) -> VerificationResult {
        let mut errs = vec![];

        if let Expr::Infix(infix_expr) = &expr {
            if let OpInfix::Divide = infix_expr.op {
                let (lt_expr, lt_span) = &infix_expr.lt;

                let (rt_expr, rt_span) = &infix_expr.rt;

                let lt_value: f64 = lt_expr.clone().try_into().unwrap();
                let rt_value: f64 = rt_expr.clone().try_into().unwrap();

                if lt_value == 0f64 {
                    errs.push((SyntaxError::DivideByZero.into(), lt_span.clone()));
                }

                if rt_value == 0f64 {
                    errs.push((SyntaxError::DivideByZero.into(), rt_span.clone()));
                }
            }
        }

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(())
    }
}

#[cfg(test)]
mod divide_by_zero_tests {
    use egonlang_core::{
        ast::{Expr, ExprInfix, ExprLiteral, OpInfix},
        errors::SyntaxError,
    };
    use pretty_assertions::assert_eq;

    use crate::{rules::rule::Rule, type_env::TypeEnv};

    use super::DivideByZeroRule;

    #[test]
    fn returns_ok_if_dividing_non_zero_numbers() {
        let rule = DivideByZeroRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..1),
            op: OpInfix::Divide,
            rt: (ExprLiteral::Number(50f64).into(), 1..2),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_expr(&expr, &span, &mut types));
    }

    #[test]
    fn returns_ok_if_dividing_zero_numbers_1() {
        let rule = DivideByZeroRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprInfix {
            lt: (ExprLiteral::Number(0f64).into(), 0..1),
            op: OpInfix::Divide,
            rt: (ExprLiteral::Number(50f64).into(), 1..2),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(SyntaxError::DivideByZero.into(), 0..1)]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_ok_if_dividing_zero_numbers_2() {
        let rule = DivideByZeroRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprInfix {
            lt: (ExprLiteral::Number(50f64).into(), 0..1),
            op: OpInfix::Divide,
            rt: (ExprLiteral::Number(0f64).into(), 1..2),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(SyntaxError::DivideByZero.into(), 1..2)]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_ok_if_dividing_zero_numbers_3() {
        let rule = DivideByZeroRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprInfix {
            lt: (ExprLiteral::Number(0f64).into(), 0..1),
            op: OpInfix::Divide,
            rt: (ExprLiteral::Number(0f64).into(), 1..2),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![
                (SyntaxError::DivideByZero.into(), 0..1),
                (SyntaxError::DivideByZero.into(), 1..2)
            ]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }
}
