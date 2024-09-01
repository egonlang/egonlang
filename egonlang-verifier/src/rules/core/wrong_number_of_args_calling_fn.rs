use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;
use egonlang_types::Type;
use span::Span;

expr_rule!(
    WrongNumberOfArgsCallingFn,
    |expr, span, _resolve_ident, resolve_expr| {
        if let ast::Expr::Call(call_expr) = &expr {
            let mut errs = vec![];

            if let Ok(callee_type) = resolve_expr(&call_expr.callee.0, &call_expr.callee.1) {
                if callee_type.of_type.is_function() {
                    let fn_param_types: Vec<(Type, Span)> = callee_type
                        .of_type
                        .get_function_params()
                        .iter()
                        .map(|x| (x.clone(), span.clone()))
                        .collect();

                    let call_arg_types: Vec<(Type, Span)> = call_expr
                        .clone()
                        .args
                        .into_iter()
                        .map(|x| {
                            let result = resolve_expr(&x.0, &x.1).expect("WHOOPS");

                            (result.of_type, x.1.clone())
                        })
                        .collect();

                    if fn_param_types.len() != call_arg_types.len() {
                        errs.push((
                            EgonTypeError::WrongNumberOfArgs {
                                expected: fn_param_types.len(),
                                actual: call_arg_types.len(),
                            }
                            .into(),
                            span.clone(),
                        ));
                    }
                }
            }

            errs
        } else {
            vec![]
        }
    }
);

#[cfg(test)]
mod tests {
    use super::WrongNumberOfArgsCallingFnRule;
    use crate::verifier_rule_test;
    use egonlang_errors::EgonTypeError;

    verifier_rule_test!(
        WrongNumberOfArgsCallingFnRule,
        returns_ok_call_args_match_fn_param_count,
        r#"
        let sum = (a: number, b: number): string => { a + b };
        sum(10, 10);"#
    );

    verifier_rule_test!(
        WrongNumberOfArgsCallingFnRule,
        returns_err_if_call_has_too_many_args,
        r#"
        let sum = (a: number, b: number): string => { a + b };
        sum(10, 15, 20);"#,
        Err(vec![(
            EgonTypeError::WrongNumberOfArgs {
                expected: 2,
                actual: 3
            }
            .into(),
            72..87
        )])
    );

    verifier_rule_test!(
        WrongNumberOfArgsCallingFnRule,
        returns_err_if_call_has_too_few_args,
        r#"
        let sum = (a: number, b: number): string => { a + b };
        sum(10);"#,
        Err(vec![(
            EgonTypeError::WrongNumberOfArgs {
                expected: 2,
                actual: 1
            }
            .into(),
            72..79
        )])
    );
}
