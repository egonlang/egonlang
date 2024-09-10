use crate::{expr_rule, rules::rule::RuleTarget};
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;

expr_rule!(
    /// Checks that expressions being called are callable
    ///
    /// ```egon
    /// fn example1(): string => {
    ///     123 // TypeError
    /// }
    ///
    /// example1();
    ///
    /// 123(); // TypeError: number is not callable
    /// ```
    NoNonCallableCalled,
    |context| {
        if let RuleTarget::Expr(expr) = context.target() {
            if let ast::Expr::Call(expr_call) = &expr {
                if let Some(callee_type) = context.resolve_expr(&expr_call.callee.0, &expr_call.callee.1) {
                    if !callee_type.is_function() {
                        return vec![(EgonTypeError::NotCallable(callee_type.to_string()).into(), expr_call.callee.1.clone())];
                    }
                }
            }
        }

        vec![]
    }
);
