use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;
use rules::rule::RuleTarget;

expr_rule!(
    /// Checks for references to undefined variables
    ///
    /// ```egon
    /// let a = 123;
    /// b; // TypeError
    /// a;
    /// ```
    ReferencingUndefinedIdentifier,
    |context| {
        let mut errs = vec![];

        if let RuleTarget::Expr(expr) = context.target() {
            if let ast::Expr::Identifier(boxed) = expr {
                let identifier = &boxed.identifier;
                let name = &identifier.name;

                if context.resolve_identifier(name).is_none() {
                    errs.push((EgonTypeError::Undefined(name.to_string()).into(), context.span().clone()));
                }
            }
        }

        errs
    }
);

#[cfg(test)]
mod tests {
    use egonlang_errors::EgonTypeError;

    use super::ReferencingUndefinedIdentifierRule;
    use crate::verifier_rule_test;

    verifier_rule_test! {
        ReferencingUndefinedIdentifierRule,
        returns_ok_if_identifier_is_defined,
        "let a = 123; a;"
    }

    verifier_rule_test! {
        ReferencingUndefinedIdentifierRule,
        returns_error_if_identifier_is_undefined,
        "a;",
        Err(vec![(
            EgonTypeError::Undefined("a".to_string()).into(),
            0..1
        )])
    }

    verifier_rule_test!(
        ReferencingUndefinedIdentifierRule,
        returns_error_if_assert_type_value_is_undefined_identifier,
        "assert_type a, number;",
        Err(vec![(
            EgonTypeError::Undefined("a".to_string()).into(),
            12..13
        )])
    );
}
