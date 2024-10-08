use crate::prelude::*;
use egonlang_core::ast::Stmt;
use egonlang_errors::{EgonErrorS, EgonTypeError};
use rules::rule::RuleTarget;

stmt_rule!(AssertType, |context| {
    let mut errs: Vec<EgonErrorS> = vec![];

    if let RuleTarget::Stmt(Stmt::AssertType(stmt_assert_type)) = context.target() {
        if let Some(value_type) =
            context.resolve_expr(stmt_assert_type.value.0.clone(), &stmt_assert_type.value.1)
        {
            if let Some(expected_type_type) = context.resolve_expr(
                stmt_assert_type.expected_type.0.clone(),
                &stmt_assert_type.expected_type.1,
            ) {
                // Check if the value type is the expected type
                //
                // Ignore this if the expected type is "unknown"
                // This prevents generating confusing type errors
                //
                // ```egon
                // type 123, DoesntExist;
                // // out: TypeError: mismatched types: expected type `number` but received `string`
                // ```
                //
                // TODO: Should this produce TypeError:UnknownType instead?
                if value_type != expected_type_type && !expected_type_type.is_unknown() {
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
        assert_type_mismatch_across_scopes,
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

    verifier_rule_test!(
        AssertTypeRule,
        assert_type_mismatch_in_block,
        r#"
    {
        assert_type 123, string;
    };
    "#,
        Err(vec![(
            EgonError::TypeError(egonlang_errors::EgonTypeError::MismatchType {
                expected: "string".to_string(),
                actual: "number".to_string()
            }),
            27..30
        )])
    );

    verifier_rule_test!(
        AssertTypeRule,
        assert_type_mismatch_in_function_body,
        r#"
    (): () => {
        assert_type 123, string;
    };
    "#,
        Err(vec![(
            EgonError::TypeError(egonlang_errors::EgonTypeError::MismatchType {
                expected: "string".to_string(),
                actual: "number".to_string()
            }),
            37..40
        )])
    );

    verifier_rule_test!(
        AssertTypeRule,
        assert_type_mismatch_with_identifier,
        r#"
    let a = false;
    assert_type a, number;
    "#,
        Err(vec![(
            EgonError::TypeError(egonlang_errors::EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "bool".to_string()
            }),
            36..37
        )])
    );

    verifier_rule_test!(
        AssertTypeRule,
        assert_type_mismatch_with_type_alias,
        r#"
    type String = string;

    assert_type 123, String;
    "#,
        Err(vec![(
            EgonError::TypeError(egonlang_errors::EgonTypeError::MismatchType {
                expected: "string".to_string(),
                actual: "number".to_string()
            }),
            44..47
        )])
    );

    verifier_rule_test!(
        AssertTypeRule,
        assert_type_mismatch,
        r#"assert_type 123, string;"#,
        Err(vec![(
            EgonError::TypeError(egonlang_errors::EgonTypeError::MismatchType {
                expected: "string".to_string(),
                actual: "number".to_string()
            }),
            12..15
        )])
    );

    verifier_rule_test!(
        AssertTypeRule,
        assert_type_undefined_identifier,
        r#"
    assert_type b, number;
    assert_type 123, Number;
    "#,
        Err(vec![
            (
                EgonError::TypeError(egonlang_errors::EgonTypeError::Undefined("b".to_string())),
                17..18
            ),
            (
                EgonError::TypeError(egonlang_errors::EgonTypeError::Undefined(
                    "Number".to_string()
                )),
                49..55
            )
        ])
    );
}
