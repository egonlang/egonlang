use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonSyntaxError;

expr_rule!(
    /// Check that consts can not be reassigned
    ///
    /// ```egon
    /// const a = 123;
    /// a = 456; // SyntaxError
    /// ```
    ReassigningConstValue,
    |expr, span, resolve_ident, _resolve_expr| {
        let mut errs = vec![];

        if let ast::Expr::Assign(expr_assign) = expr {
            let identifier = &expr_assign.identifier.0.name;
            let type_env_value = resolve_ident(identifier);
            let is_const = type_env_value.map(|x| x.is_const).unwrap_or(false);

            if is_const {
                errs.push((
                    EgonSyntaxError::ReassigningConst {
                        name: identifier.clone(),
                    }
                    .into(),
                    span.clone(),
                ));
            }
        };

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::ReassigningConstValueRule;
    use crate::verifier_rule_test;
    use egonlang_errors::EgonSyntaxError;

    verifier_rule_test!(
        ReassigningConstValueRule,
        returns_ok_if_identifier_not_const,
        "let a: number; a = 100;"
    );

    verifier_rule_test!(
        ReassigningConstValueRule,
        returns_err_if_identifier_is_const,
        "const a = 5; a = 100;",
        Err(vec![(
            EgonSyntaxError::ReassigningConst {
                name: "a".to_string()
            }
            .into(),
            13..20
        )])
    );
}
