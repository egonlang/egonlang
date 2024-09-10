use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonSyntaxError;
use rules::rule::RuleTarget;

expr_rule!(
    /// Checks infix expressions for dividing by zero
    ///
    /// ```egon
    /// let a = 10 / 0; // SyntaxError
    /// ```
    DivideByZero,
    |context| {
        let mut errs = vec![];

        if let RuleTarget::Expr(expr) = context.target() {
            if let ast::Expr::Infix(infix_expr) = &expr {
                if let ast::OpInfix::Divide = infix_expr.op {

                    let (lt_expr, lt_span) = &infix_expr.lt;

                    let (rt_expr, rt_span) = &infix_expr.rt;

                    let lt_value: f64 = lt_expr.clone().try_into().unwrap();
                    let rt_value: f64 = rt_expr.clone().try_into().unwrap();

                    if lt_value == 0f64 {
                        errs.push((EgonSyntaxError::DivideByZero.into(), lt_span.clone()));
                    }

                    if rt_value == 0f64 {
                        errs.push((EgonSyntaxError::DivideByZero.into(), rt_span.clone()));
                    }
                }
            }
        }

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::DivideByZeroRule;
    use crate::verifier_rule_test;
    use egonlang_errors::EgonSyntaxError;

    verifier_rule_test!(
        DivideByZeroRule,
        returns_ok_if_dividing_non_zero_numbers,
        "100 / 50;"
    );

    verifier_rule_test!(
        DivideByZeroRule,
        returns_err_if_dividing_zero_numbers,
        "50 / 0;",
        Err(vec![(EgonSyntaxError::DivideByZero.into(), 5..6)])
    );

    verifier_rule_test!(
        DivideByZeroRule,
        returns_ok_if_dividing_zero_numbers_2,
        "0 / 0;",
        Err(vec![
            (EgonSyntaxError::DivideByZero.into(), 0..1),
            (EgonSyntaxError::DivideByZero.into(), 4..5)
        ])
    );
}
