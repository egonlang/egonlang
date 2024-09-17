use crate::{expr_rule, rules};
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
        let mut errs = vec![];

        if let rules::rule::RuleTarget::Expr(expr) = context.target() {
            if let ast::Expr::Call(expr_call) = &*expr {
                let (callee_expr, callee_span) = &expr_call.callee;

                if let Some(callee_type) = context.resolve_expr(callee_expr.clone(), callee_span) {
                    if !callee_type.is_function() {
                        errs.push((
                            EgonTypeError::NotCallable(callee_type.to_string()).into(),
                            expr_call.callee.1.clone())
                        );
                    }
                }
            }
        }

        errs
    }
);
