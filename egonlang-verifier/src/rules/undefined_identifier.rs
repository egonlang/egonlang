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
mod undefined_identifier_test {
    use egonlang_core::{ast::Identifier, errors::TypeError, prelude::*};
    use pretty_assertions::assert_eq;

    use crate::prelude::*;

    use super::ReferencingUndefinedIdentifierRule;

    #[test]
    fn returns_error_if_identifier_is_undefined() {
        let rule = ReferencingUndefinedIdentifierRule;

        let mut types = TypeEnv::new();

        let expr: Expr = Identifier {
            name: "a".to_string(),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(
                TypeError::Undefined("a".to_string()).into(),
                span.clone()
            )]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_ok_if_identifier_is_defined() {
        let rule = ReferencingUndefinedIdentifierRule;

        let mut types = TypeEnv::new();

        types.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::unit(),
                is_const: true,
            },
        );

        let expr: Expr = Identifier {
            name: "a".to_string(),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_expr(&expr, &span, &mut types));
    }
}
