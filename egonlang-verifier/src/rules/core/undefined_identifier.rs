use crate::prelude::*;
use egonlang_core::prelude::*;

expr_rule!(
    /// Checks for references to undefined variables
    ///
    /// ```egon
    /// let a = 123;
    /// b; // TypeError
    /// a;
    /// ```
    ReferencingUndefinedIdentifier,
    fn (expr: &ast::Expr, span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let ast::Expr::Identifier(ast::ExprIdentifier { identifier }) = expr {
            verify_trace!(
                "Checking if idenitifer {} has been defined",
                expr.to_string().cyan()
            );

            let name = &identifier.name;

            if types.get(name).is_none() {
                verify_trace!(error: "identifier not defined: {expr}");

                errs.push((EgonTypeError::Undefined(name.to_string()).into(), span.clone()));
            }
        }

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::ReferencingUndefinedIdentifierRule;
    use crate::verifier_rule_test;
    use egonlang_core::prelude::*;

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
}
