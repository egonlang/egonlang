use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;
use egonlang_types::Type;
use rules::rule::RuleTarget;
use span::Span;

expr_rule!(
    /// Checks the return type of a function matches function body's
    /// returning expression, if one is present.
    ///
    /// ```egon
    /// fn example1(): string => {
    ///     123 // TypeError
    /// }
    ///
    /// fn example2(): number => {
    ///     123
    /// }
    /// ```
    TypeMismatchArgsInCallExpr,
    |context| {
        let mut errs = vec![];

        if let RuleTarget::Expr(expr) = context.target() {
            if let ast::Expr::Call(call_expr) = &expr {

                let fn_param_types: Vec<(Type, Span)> = if let Some(callee_type) = context.resolve_expr(&call_expr.callee.0, &call_expr.callee.1) {
                    if !callee_type.is_function() {
                        vec![]
                    } else {
                        callee_type.get_function_params().iter().map(|x| (x.clone(), context.span().clone())).collect()
                    }
                } else {
                    vec![]
                };

                let call_arg_types: Vec<(Type, Span)> = call_expr.clone().args.into_iter().map(|x| {
                    let result = context.resolve_expr(&x.0, &x.1).expect("WHOOPS");

                    (result, x.1.clone())
                }).collect();

                for (i, (t, s)) in call_arg_types.into_iter().enumerate() {
                    if let Some((xt, _)) = fn_param_types.get(i) {
                        if t != *xt {
                            errs.push((
                                EgonTypeError::MismatchType {
                                    expected: xt.to_string(),
                                    actual: t.to_string()
                                }.into(),
                                s.clone()
                            ));
                        }
                    }
                };
            }
        }

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::TypeMismatchArgsInCallExprRule;
    use crate::verifier_rule_test;
    use egonlang_errors::EgonTypeError;

    verifier_rule_test!(
        TypeMismatchArgsInCallExprRule,
        returns_ok_call_args_match_fn_param_tpes,
        r#"
        let sum = (a: number, b: number): string => { a + b };
        sum(10, 10);"#
    );

    verifier_rule_test!(
        TypeMismatchArgsInCallExprRule,
        returns_err_if_call_args_mismatch_fn_param_tpes,
        r#"
        let sum = (a: number, b: number): string => { a + b };
        sum(false, "foo");"#,
        Err(vec![
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "bool".to_string()
                }
                .into(),
                76..81
            ),
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                83..88
            )
        ])
    );
}
