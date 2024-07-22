use crate::prelude::*;
use egonlang_core::{ast::ExprIdentifier, errors::TypeError, prelude::*};

expr_rule!(
    /// Checks for references to undefined variables
    ///
    /// ```egon
    /// let a = 123;
    /// b; // TypeError
    /// a;
    /// ```
    ReferencingUndefinedIdentifier,
    fn (expr: &Expr, span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Expr::Identifier(ExprIdentifier { identifier }) = expr {
            verify_trace!(
                "Checking if idenitifer {} has been defined",
                expr.to_string().cyan()
            );

            let name = &identifier.name;

            if types.get(name).is_none() {
                verify_trace!(error: "identifier not defined: {expr}");

                errs.push((TypeError::Undefined(name.to_string()).into(), span.clone()));
            }
        }

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::ReferencingUndefinedIdentifierRule;
    use crate::verifier_rule_test;
    use egonlang_core::errors::TypeError;

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
            TypeError::Undefined("a".to_string()).into(),
            0..1
        )])
    }
}
