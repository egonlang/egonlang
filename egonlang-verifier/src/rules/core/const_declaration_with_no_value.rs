use crate::prelude::*;
use egonlang_core::prelude::*;

stmt_rule!(
    /// Checks assignment statements initialize consts with a value
    DeclareConstWithoutValue,
    | stmt, span, _resolve_ident, _resolve_expr | {
        let mut errs = vec![];

        if let ast::Stmt::Assign(stmt_assign) = stmt {
            if stmt_assign.is_const && stmt_assign.value.is_none() {
                errs.push((
                    EgonSyntaxError::UninitializedConst {
                        name: stmt_assign.identifier.0.name.clone(),
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
    use super::DeclareConstWithoutValueRule;
    use crate::verifier_rule_test;
    use egonlang_core::prelude::*;

    verifier_rule_test!(
        DeclareConstWithoutValueRule,
        returns_ok_const_declared_with_value,
        "const a = 123;"
    );

    verifier_rule_test!(
        DeclareConstWithoutValueRule,
        returns_err_const_declared_without_type_or_value,
        "const a;",
        Err(vec![(
            EgonSyntaxError::UninitializedConst {
                name: "a".to_string()
            }
            .into(),
            0..8
        )])
    );

    verifier_rule_test!(
        DeclareConstWithoutValueRule,
        returns_err_const_declared_without_value,
        "const a: number;",
        Err(vec![(
            EgonSyntaxError::UninitializedConst {
                name: "a".to_string()
            }
            .into(),
            0..16
        )])
    );
}
