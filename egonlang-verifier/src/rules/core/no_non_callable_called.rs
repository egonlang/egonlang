use crate::expr_rule;
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
    |expr_span, _resolve_ident, resolve_expr| {
        let (expr, _) = expr_span;

        if let ast::Expr::Call(expr_call) = &*expr {
            if let Some(callee_type) = resolve_expr.get(&(expr_call.callee.0.clone(), expr_call.callee.1.clone())) {
                if !callee_type.is_function() {
                    return vec![(EgonTypeError::NotCallable(callee_type.to_string()).into(), expr_call.callee.1.clone())];
                }
            }
        }

        vec![]
    }
);
