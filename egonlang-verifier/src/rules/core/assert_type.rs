use crate::prelude::*;
use egonlang_core::ast::Stmt;
use egonlang_errors::{EgonErrorS, EgonTypeError};

stmt_rule!(AssertType, |stmt, _span, _resolve_ident, resolve_expr| {
    let mut errs: Vec<EgonErrorS> = vec![];

    if let Stmt::AssertType(stmt_assert_type) = stmt {
        if let Some(value_type) = resolve_expr.get(&(
            stmt_assert_type.value.0.clone(),
            stmt_assert_type.value.1.clone(),
        )) {
            if let Some(expected_type_type) = resolve_expr.get(&(
                stmt_assert_type.expected_type.0.clone(),
                stmt_assert_type.expected_type.1.clone(),
            )) {
                if value_type != expected_type_type {
                    errs.push((
                        EgonTypeError::MismatchType {
                            expected: expected_type_type.to_string(),
                            actual: value_type.to_string(),
                        }
                        .into(),
                        stmt_assert_type.value.1.clone(),
                    ));
                }
            }
        }
    }

    errs
});

#[cfg(test)]
mod tests {
    use egonlang_errors::EgonError;

    use crate::{rules::core::AssertTypeRule, verifier_rule_test};

    verifier_rule_test!(
        AssertTypeRule,
        should_work,
        r#"
    let a = 123;

    {
        let a = false;

        assert_type a, number;
        assert_type a, bool;
    };

    assert_type a, bool;
    assert_type a, number;
    "#,
        Err(vec![
            (
                EgonError::TypeError(egonlang_errors::EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "bool".to_string()
                }),
                69..70
            ),
            (
                EgonError::TypeError(egonlang_errors::EgonTypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "number".to_string()
                }),
                133..134
            )
        ])
    );
}
